(** Monorepo export: create a standalone buildable directory from unpac workspace.

    Combines all projects and their vendored dependencies into a single directory
    structure suitable for building with dune. No git history is included. *)

let src = Logs.Src.create "unpac.monorepo" ~doc:"Monorepo export"
module Log = (val Logs.src_log src : Logs.LOG)

type export_config = {
  output_dir : string;
  projects : string list option;  (** None = all projects *)
  include_opam : bool;
  include_git : bool;
}

type export_result = {
  projects_exported : string list;
  opam_packages : string list;
  git_repos : string list;
  output_path : string;
}

let default_config ~output_dir = {
  output_dir;
  projects = None;
  include_opam = true;
  include_git = true;
}

(* Copy a directory tree recursively, excluding .git and _build *)
let rec copy_tree ~src ~dst =
  if Sys.is_directory src then begin
    if not (Sys.file_exists dst) then
      Unix.mkdir dst 0o755;
    let entries = Sys.readdir src in
    Array.iter (fun name ->
      if name <> ".git" && name <> "_build" then begin
        let src_path = Filename.concat src name in
        let dst_path = Filename.concat dst name in
        copy_tree ~src:src_path ~dst:dst_path
      end
    ) entries
  end else begin
    (* Copy file *)
    let content = In_channel.with_open_bin src In_channel.input_all in
    Out_channel.with_open_bin dst (fun oc ->
      Out_channel.output_string oc content)
  end

(* Remove a directory tree recursively *)
let rec remove_tree path =
  if Sys.file_exists path then begin
    if Sys.is_directory path then begin
      Array.iter (fun name ->
        remove_tree (Filename.concat path name)
      ) (Sys.readdir path);
      Unix.rmdir path
    end else
      Sys.remove path
  end

(* Export files from a git branch to a directory (without git history) *)
let export_branch_to_dir ~proc_mgr ~git_dir ~branch ~output_dir =
  Log.info (fun m -> m "Exporting branch %s to %s" branch output_dir);
  let temp_dir = Filename.temp_dir "unpac-export" "" in
  begin
    try
      (* Check if branch exists *)
      let branch_exists =
        match Git.rev_parse ~proc_mgr ~cwd:git_dir branch with
        | Some _ -> true
        | None -> false
      in
      if not branch_exists then begin
        Log.warn (fun m -> m "Branch %s does not exist, skipping" branch);
        false
      end else begin
        (* Create temporary worktree *)
        Git.run_exn ~proc_mgr ~cwd:git_dir
          ["worktree"; "add"; "--detach"; temp_dir; branch]
        |> ignore;
        (* Copy files to output *)
        if not (Sys.file_exists output_dir) then
          Unix.mkdir output_dir 0o755;
        copy_tree ~src:temp_dir ~dst:output_dir;
        (* Remove worktree *)
        Git.run_exn ~proc_mgr ~cwd:git_dir
          ["worktree"; "remove"; "--force"; temp_dir]
        |> ignore;
        true
      end
    with exn ->
      (* Clean up on error *)
      (try
        Git.run_exn ~proc_mgr ~cwd:git_dir
          ["worktree"; "remove"; "--force"; temp_dir]
        |> ignore
      with _ -> ());
      (try remove_tree temp_dir with _ -> ());
      raise exn
  end

(* Export a project, stripping its vendor/ directory *)
let export_project ~proc_mgr ~git_dir ~project ~output_dir =
  let branch = "project/" ^ project in
  let project_dir = Filename.concat output_dir project in
  Log.info (fun m -> m "Exporting project %s" project);

  if export_branch_to_dir ~proc_mgr ~git_dir ~branch ~output_dir:project_dir then begin
    (* Remove the vendor/ directory from exported project - deps go in root vendor/ *)
    let vendor_dir = Filename.concat project_dir "vendor" in
    if Sys.file_exists vendor_dir then begin
      Log.info (fun m -> m "Removing vendor/ from project %s (will use root vendor/)" project);
      remove_tree vendor_dir
    end;
    true
  end else
    false

(* Export an opam package from patches branch *)
let export_opam_package ~proc_mgr ~git_dir ~package ~vendor_dir =
  let branch = "opam/patches/" ^ package in
  let package_dir = Filename.concat (Filename.concat vendor_dir "opam") package in
  Log.info (fun m -> m "Exporting opam package %s" package);
  export_branch_to_dir ~proc_mgr ~git_dir ~branch ~output_dir:package_dir

(* Export a git repo from patches branch *)
let export_git_repo ~proc_mgr ~git_dir ~repo ~vendor_dir =
  let branch = "git-repos/patches/" ^ repo in
  let repo_dir = Filename.concat (Filename.concat vendor_dir "git") repo in
  Log.info (fun m -> m "Exporting git repo %s" repo);
  export_branch_to_dir ~proc_mgr ~git_dir ~branch ~output_dir:repo_dir

(* Generate root dune-project file *)
let generate_dune_project ~output_dir ~projects =
  let content = Printf.sprintf
{|(lang dune 3.0)
(name unpac-monorepo)

; Combined monorepo from unpac workspace
; Projects: %s

(generate_opam_files false)
|}
    (String.concat ", " projects)
  in
  let path = Filename.concat output_dir "dune-project" in
  Out_channel.with_open_bin path (fun oc ->
    Out_channel.output_string oc content)

(* Generate root dune file with vendored_dirs and includes *)
let generate_root_dune ~output_dir ~projects ~has_opam ~has_git =
  let vendor_stanzas =
    if has_opam || has_git then
      "(vendored_dirs vendor)\n"
    else ""
  in
  (* Simple root dune - projects are subdirectories *)
  let content = Printf.sprintf
{|; Root dune file for unpac monorepo
; Auto-generated - do not edit
; Projects: %s

%s|}
    (String.concat ", " projects)
    vendor_stanzas
  in
  let path = Filename.concat output_dir "dune" in
  Out_channel.with_open_bin path (fun oc ->
    Out_channel.output_string oc content)

(* Generate vendor/dune file *)
let generate_vendor_dune ~vendor_dir ~has_opam ~has_git =
  let subdirs =
    (if has_opam then ["opam"] else []) @
    (if has_git then ["git"] else [])
  in
  if subdirs <> [] then begin
    let content = Printf.sprintf
{|; Vendor dune file for unpac monorepo
(vendored_dirs %s)
|}
      (String.concat " " subdirs)
    in
    let path = Filename.concat vendor_dir "dune" in
    Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc content)
  end

(* Update project dune files to reference parent vendor/ *)
let update_project_dune ~project_dir =
  let dune_path = Filename.concat project_dir "dune" in
  if Sys.file_exists dune_path then begin
    let content = In_channel.with_open_bin dune_path In_channel.input_all in
    (* Remove local vendored_dirs since we use root-level vendor/ *)
    if String.length content > 0 then begin
      let lines = String.split_on_char '\n' content in
      let filtered = List.filter (fun line ->
        let trimmed = String.trim line in
        not (String.length trimmed >= 14 &&
             String.sub trimmed 0 14 = "(vendored_dirs")
      ) lines in
      let updated = String.concat "\n" filtered in
      Out_channel.with_open_bin dune_path (fun oc ->
        Out_channel.output_string oc updated)
    end
  end

(* Main export function *)
let export ~proc_mgr ~root ~config =
  let git_dir = Worktree.git_dir root in

  (* Create output directory *)
  if not (Sys.file_exists config.output_dir) then
    Unix.mkdir config.output_dir 0o755;

  (* Get list of projects to export *)
  let all_projects = Worktree.list_projects ~proc_mgr root in
  let projects = match config.projects with
    | Some ps -> List.filter (fun p -> List.mem p all_projects) ps
    | None -> all_projects
  in

  if projects = [] then begin
    Log.warn (fun m -> m "No projects to export");
    { projects_exported = []; opam_packages = []; git_repos = [];
      output_path = config.output_dir }
  end else begin
    Log.info (fun m -> m "Exporting %d projects: %s"
      (List.length projects) (String.concat ", " projects));

    (* Export each project *)
    let exported_projects = List.filter_map (fun project ->
      if export_project ~proc_mgr ~git_dir ~project ~output_dir:config.output_dir then
        Some project
      else
        None
    ) projects in

    (* Create vendor directory *)
    let vendor_dir = Filename.concat config.output_dir "vendor" in
    if not (Sys.file_exists vendor_dir) then
      Unix.mkdir vendor_dir 0o755;

    (* Export opam packages *)
    let opam_packages =
      if config.include_opam then begin
        let all_opam = Worktree.list_opam_packages ~proc_mgr root in
        Log.info (fun m -> m "Exporting %d opam packages" (List.length all_opam));
        (* Create opam subdirectory *)
        let opam_dir = Filename.concat vendor_dir "opam" in
        if not (Sys.file_exists opam_dir) && all_opam <> [] then
          Unix.mkdir opam_dir 0o755;
        List.filter_map (fun pkg ->
          if export_opam_package ~proc_mgr ~git_dir ~package:pkg ~vendor_dir then
            Some pkg
          else
            None
        ) all_opam
      end else []
    in

    (* Export git repos *)
    let git_repos =
      if config.include_git then begin
        let all_git = Git_backend.list_repos ~proc_mgr ~root in
        Log.info (fun m -> m "Exporting %d git repos" (List.length all_git));
        (* Create git subdirectory *)
        let git_subdir = Filename.concat vendor_dir "git" in
        if not (Sys.file_exists git_subdir) && all_git <> [] then
          Unix.mkdir git_subdir 0o755;
        List.filter_map (fun repo ->
          if export_git_repo ~proc_mgr ~git_dir ~repo ~vendor_dir then
            Some repo
          else
            None
        ) all_git
      end else []
    in

    (* Generate dune files *)
    let has_opam = opam_packages <> [] in
    let has_git = git_repos <> [] in

    generate_dune_project ~output_dir:config.output_dir ~projects:exported_projects;
    generate_root_dune ~output_dir:config.output_dir ~projects:exported_projects
      ~has_opam ~has_git;

    if has_opam || has_git then
      generate_vendor_dune ~vendor_dir ~has_opam ~has_git;

    (* Update project dune files *)
    List.iter (fun project ->
      let project_dir = Filename.concat config.output_dir project in
      update_project_dune ~project_dir
    ) exported_projects;

    Log.info (fun m -> m "Monorepo export complete: %s" config.output_dir);

    { projects_exported = exported_projects;
      opam_packages;
      git_repos;
      output_path = config.output_dir }
  end
