(** Project promotion to vendor library.

    Promotes a locally-developed project to a vendored library by:
    1. Filtering out the vendor/ directory from the project history
    2. Creating vendor branches (upstream/vendor/patches) for the specified backend
    3. Recording the promotion in the audit log

    This allows the project to be merged into other projects as a dependency. *)

let src = Logs.Src.create "unpac.promote" ~doc:"Project promotion"
module Log = (val Logs.src_log src : Logs.LOG)

(** Backend types for promotion *)
type backend =
  | Opam
  | Git

let backend_of_string = function
  | "opam" -> Some Opam
  | "git" -> Some Git
  | _ -> None

let backend_to_string = function
  | Opam -> "opam"
  | Git -> "git"

(** Branch names for a backend *)
let upstream_branch backend name = match backend with
  | Opam -> "opam/upstream/" ^ name
  | Git -> "git/upstream/" ^ name

let vendor_branch backend name = match backend with
  | Opam -> "opam/vendor/" ^ name
  | Git -> "git/vendor/" ^ name

let patches_branch backend name = match backend with
  | Opam -> "opam/patches/" ^ name
  | Git -> "git/patches/" ^ name

let vendor_path backend name = match backend with
  | Opam -> "vendor/opam/" ^ name
  | Git -> "vendor/git/" ^ name

(** Result of promotion *)
type promote_result =
  | Promoted of {
      name : string;
      backend : backend;
      original_commits : int;
      filtered_commits : int;
    }
  | Already_promoted of string
  | Project_not_found of string
  | Failed of { name : string; error : string }

(** Filter a branch to exclude vendor/ directory.
    Uses git-filter-repo to rewrite history. *)
let filter_vendor_directory ~proc_mgr ~cwd ~branch =
  Log.info (fun m -> m "Filtering vendor/ directory from branch %s..." branch);

  (* Use git-filter-repo with path filtering to exclude vendor/ *)
  let fs = fst cwd in
  let git_path = snd cwd in
  let parent_path = Filename.dirname git_path in

  (* Create a unique temporary worktree *)
  let safe_branch = String.map (fun c -> if c = '/' then '-' else c) branch in
  let temp_wt_name = ".filter-vendor-" ^ safe_branch in
  let temp_wt_relpath = "../" ^ temp_wt_name in
  let temp_wt_path = Filename.concat parent_path temp_wt_name in
  let temp_wt : Git.path = (fs, temp_wt_path) in

  (* Remove any existing temp worktree *)
  ignore (Git.run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);

  (* Create worktree for the branch *)
  Git.run_exn ~proc_mgr ~cwd ["worktree"; "add"; temp_wt_relpath; branch] |> ignore;

  (* Count commits before filtering *)
  let commits_before =
    int_of_string (String.trim (Git.run_exn ~proc_mgr ~cwd:temp_wt ["rev-list"; "--count"; "HEAD"]))
  in

  (* Run git-filter-repo to exclude vendor/ *)
  let result = Git.run ~proc_mgr ~cwd:temp_wt [
    "filter-repo";
    "--invert-paths";
    "--path"; "vendor/";
    "--force";
    "--refs"; "HEAD"
  ] in

  match result with
  | Ok _ ->
      (* Count commits after filtering *)
      let commits_after =
        int_of_string (String.trim (Git.run_exn ~proc_mgr ~cwd:temp_wt ["rev-list"; "--count"; "HEAD"]))
      in
      (* Get the new HEAD SHA *)
      let new_sha = Git.run_exn ~proc_mgr ~cwd:temp_wt ["rev-parse"; "HEAD"] |> String.trim in
      (* Cleanup temporary worktree *)
      ignore (Git.run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
      (* Update the branch in the bare repo *)
      Git.run_exn ~proc_mgr ~cwd ["branch"; "-f"; branch; new_sha] |> ignore;
      Ok (commits_before, commits_after)
  | Error e ->
      (* Cleanup and return error *)
      ignore (Git.run ~proc_mgr ~cwd ["worktree"; "remove"; "-f"; temp_wt_relpath]);
      Error (Fmt.str "%a" Git.pp_error e)

(** Promote a project to a vendored library *)
let promote ~proc_mgr ~root ~project ~backend ~vendor_name =
  let git = Worktree.git_dir root in
  let name = Option.value ~default:project vendor_name in

  (* Check if project exists *)
  if not (Worktree.branch_exists ~proc_mgr root (Worktree.Project project)) then
    Project_not_found project
  else begin
    (* Check if already promoted for this backend *)
    let patches_br = patches_branch backend name in
    if Git.branch_exists ~proc_mgr ~cwd:git patches_br then
      Already_promoted name
    else begin
      try
        Log.info (fun m -> m "Promoting project %s as %s vendor %s..." project (backend_to_string backend) name);

        let project_branch = Worktree.branch (Worktree.Project project) in

        (* Step 1: Create a temporary branch from the project for filtering *)
        let temp_branch = "promote-temp-" ^ name in
        Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-f"; temp_branch; project_branch] |> ignore;

        (* Step 2: Filter out vendor/ directory from the temp branch *)
        let (commits_before, commits_after) =
          match filter_vendor_directory ~proc_mgr ~cwd:git ~branch:temp_branch with
          | Ok counts -> counts
          | Error msg ->
              (* Cleanup temp branch *)
              ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; temp_branch]);
              failwith msg
        in

        Log.info (fun m -> m "Filtered %d -> %d commits" commits_before commits_after);

        (* Step 3: Create upstream branch (filtered, files at root) *)
        (* For local projects, upstream is the same as filtered temp - no external upstream *)
        Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-f"; upstream_branch backend name; temp_branch] |> ignore;

        (* Step 4: Create vendor branch from upstream and rewrite to vendor path *)
        Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-f"; vendor_branch backend name; upstream_branch backend name] |> ignore;

        (* Rewrite vendor branch to move files into vendor/<backend>/<name>/ *)
        Git.filter_repo_to_subdirectory ~proc_mgr ~cwd:git
          ~branch:(vendor_branch backend name)
          ~subdirectory:(vendor_path backend name);

        (* Step 5: Create patches branch from vendor *)
        Git.run_exn ~proc_mgr ~cwd:git ["branch"; patches_branch backend name; vendor_branch backend name] |> ignore;

        (* Step 6: Cleanup temp branch *)
        ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; temp_branch]);

        Promoted {
          name;
          backend;
          original_commits = commits_before;
          filtered_commits = commits_after
        }
      with exn ->
        (* Cleanup on failure *)
        let temp_branch = "promote-temp-" ^ name in
        ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; temp_branch]);
        ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; upstream_branch backend name]);
        ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; vendor_branch backend name]);
        Failed { name = project; error = Printexc.to_string exn }
    end
  end

(** {1 Remote Management} *)

(** Remote name for a project *)
let project_remote_name project = "origin-" ^ project

(** Result of set-remote operation *)
type set_remote_result =
  | Remote_set of { project : string; url : string; created : bool }
  | Project_not_found of string
  | Set_remote_failed of { project : string; error : string }

(** Set the remote URL for a project *)
let set_remote ~proc_mgr ~root ~project ~url =
  let git = Worktree.git_dir root in

  (* Check if project exists *)
  if not (Worktree.branch_exists ~proc_mgr root (Worktree.Project project)) then
    Project_not_found project
  else begin
    try
      let remote_name = project_remote_name project in
      Log.info (fun m -> m "Setting remote %s -> %s for project %s" remote_name url project);

      let created = match Git.ensure_remote ~proc_mgr ~cwd:git ~name:remote_name ~url with
        | `Created -> true
        | `Updated | `Existed -> false
      in

      Remote_set { project; url; created }
    with exn ->
      Set_remote_failed { project; error = Printexc.to_string exn }
  end

(** Get the remote URL for a project *)
let get_remote ~proc_mgr ~root ~project =
  let git = Worktree.git_dir root in
  let remote_name = project_remote_name project in
  Git.remote_url ~proc_mgr ~cwd:git remote_name

(** Result of push operation *)
type push_result =
  | Pushed of { project : string; branch : string; remote : string }
  | No_remote of string
  | Project_not_found of string
  | Push_failed of { project : string; error : string }

(** Push a project to its configured remote *)
let push ~proc_mgr ~root ~project =
  let git = Worktree.git_dir root in

  (* Check if project exists *)
  if not (Worktree.branch_exists ~proc_mgr root (Worktree.Project project)) then
    Project_not_found project
  else begin
    let remote_name = project_remote_name project in
    match Git.remote_url ~proc_mgr ~cwd:git remote_name with
    | None -> No_remote project
    | Some _url ->
        try
          let branch = Worktree.branch (Worktree.Project project) in
          Log.info (fun m -> m "Pushing %s to %s..." branch remote_name);
          Git.run_exn ~proc_mgr ~cwd:git ["push"; "-u"; remote_name; branch] |> ignore;
          Pushed { project; branch; remote = remote_name }
        with exn ->
          Push_failed { project; error = Printexc.to_string exn }
  end

(** {1 Project Info} *)

type project_info = {
  name : string;
  origin : [`Local | `Vendored];
  remote : string option;
  promoted_as : (backend * string) option;  (* backend, vendor_name *)
}

(** Get information about a project *)
let get_info ~proc_mgr ~root ~project =
  let git = Worktree.git_dir root in

  if not (Worktree.branch_exists ~proc_mgr root (Worktree.Project project)) then
    None
  else begin
    (* Check for remote *)
    let remote = get_remote ~proc_mgr ~root ~project in

    (* Check if promoted - look for opam/patches/<project> or git/patches/<project> *)
    let promoted_as =
      if Git.branch_exists ~proc_mgr ~cwd:git (patches_branch Opam project) then
        Some (Opam, project)
      else if Git.branch_exists ~proc_mgr ~cwd:git (patches_branch Git project) then
        Some (Git, project)
      else
        None
    in

    Some {
      name = project;
      origin = `Local;  (* All projects created via `unpac project new` are local *)
      remote;
      promoted_as;
    }
  end

(** {1 Export (Unvendor)} *)

(** Export branch name - where unvendored code goes *)
let export_branch backend name = match backend with
  | Opam -> "opam/export/" ^ name
  | Git -> "git/export/" ^ name

(** Result of export operation *)
type export_result =
  | Exported of {
      name : string;
      backend : backend;
      source_branch : string;
      export_branch : string;
      commits : int;
    }
  | Not_vendored of string
  | Already_exported of string
  | Export_failed of { name : string; error : string }

(** Export a vendored package back to root-level files.
    This is the inverse of vendoring - takes a vendor branch and creates
    an export branch with files moved from vendor/<backend>/<name>/ to root.

    Can export from either vendor/* or patches/* branch. *)
let export ~proc_mgr ~root ~name ~backend ~from_patches =
  let git = Worktree.git_dir root in

  (* Determine source branch *)
  let source_br = if from_patches then patches_branch backend name
                  else vendor_branch backend name in
  let export_br = export_branch backend name in
  let subdir = vendor_path backend name in

  (* Check if source branch exists *)
  if not (Git.branch_exists ~proc_mgr ~cwd:git source_br) then
    Not_vendored name
  else if Git.branch_exists ~proc_mgr ~cwd:git export_br then
    Already_exported name
  else begin
    try
      Log.info (fun m -> m "Exporting %s from %s to %s..." name source_br export_br);

      (* Step 1: Create export branch from source *)
      Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-f"; export_br; source_br] |> ignore;

      (* Step 2: Count commits before transformation *)
      let commits =
        int_of_string (String.trim (
          Git.run_exn ~proc_mgr ~cwd:git ["rev-list"; "--count"; export_br]))
      in

      (* Step 3: Rewrite export branch to move files from subdirectory to root *)
      Git.filter_repo_from_subdirectory ~proc_mgr ~cwd:git
        ~branch:export_br
        ~subdirectory:subdir;

      Exported {
        name;
        backend;
        source_branch = source_br;
        export_branch = export_br;
        commits;
      }
    with exn ->
      (* Cleanup on failure *)
      ignore (Git.run ~proc_mgr ~cwd:git ["branch"; "-D"; export_br]);
      Export_failed { name; error = Printexc.to_string exn }
  end

(** Remote name for export (where we push to) *)
let export_remote_name name = "export-" ^ name

(** Remote name for upstream (where we fetch from) *)
let upstream_remote_name name = "upstream-" ^ name

(** Result of export push operation *)
type export_push_result =
  | Export_pushed of {
      name : string;
      backend : backend;
      remote : string;
      branch : string;
      commits : int;
    }
  | Export_not_found of string
  | No_export_remote of string
  | Export_push_failed of { name : string; error : string }

(** Set the remote URL for exporting a package *)
let set_export_remote ~proc_mgr ~root ~name ~url =
  let git = Worktree.git_dir root in
  let remote_name = export_remote_name name in
  Log.info (fun m -> m "Setting export remote %s -> %s" remote_name url);
  Git.ensure_remote ~proc_mgr ~cwd:git ~name:remote_name ~url

(** Get the export remote URL for a package *)
let get_export_remote ~proc_mgr ~root ~name =
  let git = Worktree.git_dir root in
  let remote_name = export_remote_name name in
  Git.remote_url ~proc_mgr ~cwd:git remote_name

(** Set the remote URL for fetching upstream updates.
    This is used for promoted local packages that don't have an opam source URL. *)
let set_upstream_remote ~proc_mgr ~root ~name ~url =
  let git = Worktree.git_dir root in
  let remote_name = upstream_remote_name name in
  Log.info (fun m -> m "Setting upstream remote %s -> %s" remote_name url);
  Git.ensure_remote ~proc_mgr ~cwd:git ~name:remote_name ~url

(** Get the upstream remote URL for a package *)
let get_upstream_remote ~proc_mgr ~root ~name =
  let git = Worktree.git_dir root in
  let remote_name = upstream_remote_name name in
  Git.remote_url ~proc_mgr ~cwd:git remote_name

(** Push an exported branch to its remote *)
let push_export ~proc_mgr ~root ~name ~backend =
  let git = Worktree.git_dir root in
  let export_br = export_branch backend name in
  let remote_name = export_remote_name name in

  (* Check if export branch exists *)
  if not (Git.branch_exists ~proc_mgr ~cwd:git export_br) then
    Export_not_found name
  else begin
    match Git.remote_url ~proc_mgr ~cwd:git remote_name with
    | None -> No_export_remote name
    | Some _url ->
        try
          (* Count commits *)
          let commits =
            int_of_string (String.trim (
              Git.run_exn ~proc_mgr ~cwd:git ["rev-list"; "--count"; export_br]))
          in

          Log.info (fun m -> m "Pushing %s to %s..." export_br remote_name);
          (* Push the export branch - push to main/master on the remote *)
          Git.run_exn ~proc_mgr ~cwd:git [
            "push"; "-u"; remote_name;
            export_br ^ ":main"  (* Push export branch as 'main' on remote *)
          ] |> ignore;

          Export_pushed {
            name;
            backend;
            remote = remote_name;
            branch = export_br;
            commits;
          }
        with exn ->
          Export_push_failed { name; error = Printexc.to_string exn }
  end

(** List all exported packages *)
let list_exports ~proc_mgr ~root =
  let git = Worktree.git_dir root in
  let branches = Git.run_lines ~proc_mgr ~cwd:git ["branch"; "--list"; "*/export/*"] in
  List.filter_map (fun line ->
    let branch = String.trim line in
    let branch = if String.length branch > 0 && branch.[0] = '*' then
      String.trim (String.sub branch 1 (String.length branch - 1))
    else branch in
    (* Parse backend/export/name *)
    match String.split_on_char '/' branch with
    | [backend_str; "export"; name] ->
        (match backend_of_string backend_str with
         | Some backend -> Some (backend, name)
         | None -> None)
    | _ -> None
  ) branches
