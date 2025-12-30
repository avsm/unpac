open Cmdliner

let src = Logs.Src.create "unpac.main" ~doc:"Main CLI operations"
module Log = (val Logs.src_log src : Logs.LOG)

(* Logging setup *)
let setup_logging ?(verbose=false) () =
  Fmt_tty.setup_std_outputs ();
  let level = if verbose then Logs.Debug else Logs.Info in
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_term =
  Term.(const (setup_logging ~verbose:false) $ const ())

(* Helper to find project root *)
let with_root f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let cwd = Sys.getcwd () in
  match Unpac.Init.find_root ~fs ~cwd with
  | None ->
      Format.eprintf "Error: Not in an unpac project.@.";
      exit 1
  | Some root ->
      f ~env ~fs ~proc_mgr ~root

(* Helper to wrap operations with audit logging *)
let with_audit ~proc_mgr ~root ~operation_type ~args f =
  let main_wt = Unpac.Worktree.path root Unpac.Worktree.Main in
  let mgr = Unpac.Audit.create_manager ~proc_mgr ~main_wt in
  let ctx = Unpac.Audit.begin_operation mgr ~operation_type ~args in
  try
    let result = f ctx in
    ignore (Unpac.Audit.end_success mgr);
    result
  with exn ->
    ignore (Unpac.Audit.end_failed mgr ~error:(Printexc.to_string exn));
    raise exn

(* Helper to get config path *)
let config_path root =
  let main_path = Unpac.Worktree.path root Unpac.Worktree.Main in
  Eio.Path.(main_path / "unpac.toml") |> snd

(* Helper to load config *)
let load_config root =
  let path = config_path root in
  match Unpac.Config.load path with
  | Ok cfg -> cfg
  | Error _ -> Unpac.Config.empty

(* Helper to save config and commit *)
let save_config ~proc_mgr root config msg =
  let path = config_path root in
  Unpac.Config.save_exn path config;
  let main_wt = Unpac.Worktree.path root Unpac.Worktree.Main in
  Unpac.Git.run_exn ~proc_mgr ~cwd:main_wt ["add"; "unpac.toml"] |> ignore;
  Unpac.Git.run_exn ~proc_mgr ~cwd:main_wt ["commit"; "-m"; msg] |> ignore

(* Check if string looks like a URL or path (vs a package name) *)
let is_url_or_path s =
  String.starts_with ~prefix:"http://" s ||
  String.starts_with ~prefix:"https://" s ||
  String.starts_with ~prefix:"git@" s ||
  String.starts_with ~prefix:"git://" s ||
  String.starts_with ~prefix:"ssh://" s ||
  String.starts_with ~prefix:"file://" s ||
  String.starts_with ~prefix:"/" s ||       (* Absolute path *)
  String.starts_with ~prefix:"./" s ||      (* Relative path *)
  String.starts_with ~prefix:"../" s ||     (* Relative path *)
  String.contains s ':'                      (* URL with scheme *)

(* Normalize a dev-repo URL for grouping comparison *)
let normalize_dev_repo url =
  let s = url in
  (* Strip git+ prefix *)
  let s = if String.starts_with ~prefix:"git+" s then
    String.sub s 4 (String.length s - 4) else s in
  (* Strip trailing .git *)
  let s = if String.ends_with ~suffix:".git" s then
    String.sub s 0 (String.length s - 4) else s in
  (* Strip trailing slash *)
  let s = if String.ends_with ~suffix:"/" s then
    String.sub s 0 (String.length s - 1) else s in
  (* Normalize github URLs: git@github.com:x/y -> https://github.com/x/y *)
  let s = if String.starts_with ~prefix:"git@github.com:" s then
    "https://github.com/" ^ String.sub s 15 (String.length s - 15) else s in
  String.lowercase_ascii s

(* Group solved packages by their dev-repo *)
type package_group = {
  canonical_name : string;  (* First package name, used as vendor name *)
  dev_repo : string;        (* Original dev-repo URL *)
  packages : string list;   (* All package names in this group *)
}

let group_packages_by_dev_repo ~config (pkgs : OpamPackage.t list) : package_group list =
  let repos = config.Unpac.Config.opam.repositories in
  (* Build a map from normalized dev-repo to package info *)
  let groups = Hashtbl.create 16 in
  List.iter (fun pkg ->
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
    match Unpac_opam.Repo.find_package ~repos ~name ~version () with
    | None -> ()  (* Skip packages not found *)
    | Some result ->
        match result.metadata.dev_repo with
        | None -> ()  (* Skip packages without dev-repo *)
        | Some dev_repo ->
            let key = normalize_dev_repo dev_repo in
            match Hashtbl.find_opt groups key with
            | None ->
                Hashtbl.add groups key (dev_repo, [name])
            | Some (orig_url, names) ->
                Hashtbl.replace groups key (orig_url, name :: names)
  ) pkgs;
  (* Convert to list of groups *)
  Hashtbl.fold (fun _key (dev_repo, names) acc ->
    let names = List.rev names in  (* Preserve order *)
    let canonical_name = List.hd names in
    { canonical_name; dev_repo; packages = names } :: acc
  ) groups []
  |> List.sort (fun a b -> String.compare a.canonical_name b.canonical_name)

(* Helper to resolve vendor cache *)
let resolve_cache ~proc_mgr ~fs ~config ~cli_cache =
  match Unpac.Config.resolve_vendor_cache ?cli_override:cli_cache config with
  | None -> None
  | Some path ->
      Some (Unpac.Vendor_cache.init ~proc_mgr ~fs ~path ())

(* Init command *)
let init_cmd =
  let doc = "Initialize a new unpac workspace." in
  let man = [
    `S Manpage.s_description;
    `P "Creates a new unpac workspace with the standard directory structure:";
    `Pre "  <path>/
    git/           # Bare git repository (all branches stored here)
    main/          # Main worktree (unpac.toml config lives here)
    vendor/        # Vendor worktrees (created as needed)
      opam/        # Opam package worktrees
      git/         # Git repository worktrees
    project/       # Project worktrees";
    `P "The workspace uses git worktrees to maintain isolated views of \
        vendored dependencies. Each vendored item has three branches:";
    `I ("upstream/*", "Tracks original repository state");
    `I ("vendor/*", "Clean snapshot for merging");
    `I ("patches/*", "Local modifications");
    `S Manpage.s_examples;
    `P "Create a new workspace:";
    `Pre "  unpac init my-project
  cd my-project";
    `S "SEE ALSO";
    `P "unpac-project(1), unpac-opam(1), unpac-git(1)";
  ] in
  let path_arg =
    let doc = "Path for the new workspace. Will create the directory if it doesn't exist." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)
  in
  let run () path =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let _root = Unpac.Init.init ~proc_mgr ~fs path in
    Format.printf "Initialized unpac workspace at %s@." path;
    Format.printf "@.Next steps:@.";
    Format.printf "  cd %s@." path;
    Format.printf "  unpac opam repo add <name> <path>   # configure opam repository@.";
    Format.printf "  unpac project new <name>            # create a project@."
  in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ path_arg)

(* Project new command *)
let project_new_cmd =
  let doc = "Create a new project branch." in
  let name_arg =
    let doc = "Name of the project." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Project_new ~args:[name] @@ fun _ctx ->
    let _path = Unpac.Init.create_project ~proc_mgr root name in
    Format.printf "Created project %s@." name;
    Format.printf "@.Next steps:@.";
    Format.printf "  unpac opam add <package>            # vendor a package@.";
    Format.printf "  unpac opam merge <package> %s   # merge package into project@." name
  in
  let info = Cmd.info "new" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Project list command *)
let project_list_cmd =
  let doc = "List projects." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let projects = Unpac.Worktree.list_projects ~proc_mgr root in
    List.iter (Format.printf "%s@.") projects
  in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Project promote command *)
let project_promote_cmd =
  let doc = "Promote a local project to a vendored library." in
  let man = [
    `S Manpage.s_description;
    `P "Converts a locally-developed project into the vendor branch structure \
        used by unpac for dependency management. This allows the project to be:";
    `I ("•", "Merged into other projects as a dependency");
    `I ("•", "Pushed to an independent git repository");
    `I ("•", "Updated independently of the workspace");
    `S "FILTERING";
    `P "The promotion process filters the project history to remove \
        vendored dependencies (the vendor/ directory), producing a clean \
        library that can be independently distributed.";
    `P "Specifically, it:";
    `I ("1.", "Extracts project/<name> branch history");
    `I ("2.", "Filters out vendor/ directory (all backends' vendored code)");
    `I ("3.", "Creates clean <backend>/upstream/<name> with filtered history");
    `I ("4.", "Creates <backend>/vendor/<name> with path prefix applied");
    `I ("5.", "Creates <backend>/patches/<name> for local modifications");
    `P "The original project/<name> branch is preserved unchanged.";
    `S "BACKENDS";
    `P "The --backend flag determines how the library is structured:";
    `I ("opam", "Use for OCaml libraries built with dune. \
                 Creates vendor/opam/<name>/ structure. \
                 Merge with: unpac opam merge <name> <project>");
    `I ("git", "Use for reference code, C libraries, or non-OCaml sources. \
                Creates vendor/git/<name>/ structure. \
                Merge with: unpac git merge <name> <project>");
    `S Manpage.s_examples;
    `P "Promote a completed OCaml library:";
    `Pre "  unpac project promote brotli --backend opam";
    `P "Promote with a different vendor name:";
    `Pre "  unpac project promote mybrotli --backend opam --name brotli";
    `P "Promote a reference implementation:";
    `Pre "  unpac project promote zstd-reference --backend git";
    `P "Full workflow from development to distribution:";
    `Pre "  unpac project new mybrotli
  # ... develop the library ...
  unpac project promote mybrotli --backend opam
  unpac project set-remote mybrotli git@github.com:me/mybrotli.git
  unpac opam merge mybrotli other-project";
    `S "SEE ALSO";
    `P "unpac-project-new(1), unpac-opam-merge(1), unpac-git-merge(1)";
  ] in
  let name_arg =
    let doc = "Name of the project to promote." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let backend_arg =
    let doc = "Vendor backend type: opam or git. \
               Determines branch structure and merge semantics." in
    Arg.(required & opt (some string) None & info ["backend"; "b"] ~docv:"BACKEND" ~doc)
  in
  let vendor_name_arg =
    let doc = "Override the vendor library name (defaults to project name)." in
    Arg.(value & opt (some string) None & info ["name"; "n"] ~docv:"NAME" ~doc)
  in
  let run () project backend_str vendor_name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    (* Parse backend *)
    let backend = match Unpac.Promote.backend_of_string backend_str with
      | Some b -> b
      | None ->
          Format.eprintf "Error: Unknown backend '%s'. Use 'opam' or 'git'.@." backend_str;
          exit 1
    in
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Project_promote
      ~args:(
        [project; "--backend"; backend_str] @
        (match vendor_name with Some n -> ["--name"; n] | None -> [])
      ) @@ fun _ctx ->
    match Unpac.Promote.promote ~proc_mgr ~root ~project ~backend ~vendor_name with
    | Unpac.Promote.Promoted { name; backend; original_commits; filtered_commits } ->
        Format.printf "Promoted %s as %s vendor@." project (Unpac.Promote.backend_to_string backend);
        Format.printf "@.Filtered history: %d → %d commits (removed vendor/ directory)@."
          original_commits filtered_commits;
        Format.printf "@.Created branches:@.";
        Format.printf "  %s@." (Unpac.Promote.upstream_branch backend name);
        Format.printf "  %s@." (Unpac.Promote.vendor_branch backend name);
        Format.printf "  %s@." (Unpac.Promote.patches_branch backend name);
        Format.printf "@.%s can now be merged into other projects:@." name;
        (match backend with
         | Unpac.Promote.Opam ->
             Format.printf "  unpac opam merge %s <project>@." name
         | Unpac.Promote.Git ->
             Format.printf "  unpac git merge %s <project>@." name)
    | Unpac.Promote.Already_promoted name ->
        Format.eprintf "Error: %s is already promoted.@." name;
        exit 1
    | Unpac.Promote.Project_not_found name ->
        Format.eprintf "Error: Project '%s' not found.@." name;
        exit 1
    | Unpac.Promote.Failed { name; error } ->
        Format.eprintf "Error promoting %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "promote" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ backend_arg $ vendor_name_arg)

(* Project set-remote command *)
let project_set_remote_cmd =
  let doc = "Set the remote URL for a project." in
  let man = [
    `S Manpage.s_description;
    `P "Configures a git remote for pushing a project to an independent repository. \
        This allows projects developed in the workspace to be published separately.";
    `P "The remote is named 'origin-<project>' and is stored in the bare git \
        repository. Use 'unpac project push' to push to this remote.";
    `S Manpage.s_examples;
    `P "Set remote for a project:";
    `Pre "  unpac project set-remote brotli git@github.com:user/ocaml-brotli.git";
    `P "Full workflow:";
    `Pre "  unpac project new mylib
  # ... develop the library ...
  unpac project promote mylib --backend opam
  unpac project set-remote mylib git@github.com:me/mylib.git
  unpac project push mylib";
    `S "SEE ALSO";
    `P "unpac-project-push(1), unpac-project-promote(1)";
  ] in
  let name_arg =
    let doc = "Name of the project." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let url_arg =
    let doc = "Remote URL (git SSH or HTTPS URL)." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let run () project url =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Project_set_remote
      ~args:[project; url] @@ fun _ctx ->
    match Unpac.Promote.set_remote ~proc_mgr ~root ~project ~url with
    | Unpac.Promote.Remote_set { project; url; created } ->
        if created then
          Format.printf "Created remote for %s: %s@." project url
        else
          Format.printf "Updated remote for %s: %s@." project url;
        Format.printf "@.Push with: unpac project push %s@." project
    | Unpac.Promote.Project_not_found name ->
        Format.eprintf "Error: Project '%s' not found.@." name;
        exit 1
    | Unpac.Promote.Set_remote_failed { project; error } ->
        Format.eprintf "Error setting remote for %s: %s@." project error;
        exit 1
  in
  let info = Cmd.info "set-remote" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ url_arg)

(* Project push command *)
let project_push_cmd =
  let doc = "Push a project to its configured remote." in
  let man = [
    `S Manpage.s_description;
    `P "Pushes a project branch to the remote configured via 'set-remote'. \
        This allows publishing projects developed in the workspace to \
        independent repositories.";
    `S Manpage.s_examples;
    `P "Push a project:";
    `Pre "  unpac project push brotli";
    `S "SEE ALSO";
    `P "unpac-project-set-remote(1)";
  ] in
  let name_arg =
    let doc = "Name of the project to push." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let run () project =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    match Unpac.Promote.push ~proc_mgr ~root ~project with
    | Unpac.Promote.Pushed { project; branch; remote } ->
        Format.printf "Pushed %s (%s) to %s@." project branch remote
    | Unpac.Promote.No_remote project ->
        Format.eprintf "Error: No remote configured for %s.@." project;
        Format.eprintf "Set one with: unpac project set-remote %s <url>@." project;
        exit 1
    | Unpac.Promote.Project_not_found name ->
        Format.eprintf "Error: Project '%s' not found.@." name;
        exit 1
    | Unpac.Promote.Push_failed { project; error } ->
        Format.eprintf "Error pushing %s: %s@." project error;
        exit 1
  in
  let info = Cmd.info "push" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Project info command *)
let project_info_cmd =
  let doc = "Show detailed information about a project." in
  let man = [
    `S Manpage.s_description;
    `P "Displays information about a project including:";
    `I ("Origin", "Whether the project was created locally or vendored");
    `I ("Remote", "Configured push URL (if any)");
    `I ("Promoted", "Whether promoted to vendor library and which backend");
    `S Manpage.s_examples;
    `Pre "  unpac project info brotli";
  ] in
  let name_arg =
    let doc = "Name of the project." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let run () project =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    match Unpac.Promote.get_info ~proc_mgr ~root ~project with
    | None ->
        Format.eprintf "Error: Project '%s' not found.@." project;
        exit 1
    | Some info ->
        Format.printf "Project: %s@." info.name;
        Format.printf "Origin: %s@."
          (match info.origin with `Local -> "local" | `Vendored -> "vendored");
        Format.printf "Remote: %s@."
          (match info.remote with Some url -> url | None -> "(none)");
        Format.printf "Promoted: %s@."
          (match info.promoted_as with
           | Some (backend, name) ->
               Printf.sprintf "%s vendor (%s)" (Unpac.Promote.backend_to_string backend) name
           | None -> "no")
  in
  let info = Cmd.info "info" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Export command - unvendor a package for upstream push *)
let export_cmd =
  let doc = "Export a vendored package for pushing to upstream." in
  let man = [
    `S Manpage.s_description;
    `P "Creates an export branch from a vendored package with files moved \
        from vendor/<backend>/<name>/ back to the repository root. This is \
        the inverse of vendoring, producing a branch suitable for pushing \
        to an upstream git repository.";
    `P "Use --from-patches to include local modifications in the export. \
        Without this flag, exports from the vendor/* branch (pristine upstream).";
    `S "WORKFLOW";
    `P "The typical export workflow is:";
    `Pre "  # Export with local patches
  unpac export brotli --backend opam --from-patches

  # Set upstream remote
  unpac export-set-remote brotli git@github.com:me/brotli.git

  # Push to upstream
  unpac export-push brotli --backend opam";
    `S Manpage.s_examples;
    `P "Export an opam package (pristine upstream):";
    `Pre "  unpac export brotli --backend opam";
    `P "Export with local patches included:";
    `Pre "  unpac export brotli --backend opam --from-patches";
    `P "Export a git-vendored package:";
    `Pre "  unpac export zstd --backend git";
  ] in
  let name_arg =
    let doc = "Name of the vendored package to export." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let backend_arg =
    let doc = "Vendor backend type: opam or git." in
    Arg.(required & opt (some string) None & info ["backend"; "b"] ~docv:"BACKEND" ~doc)
  in
  let from_patches_arg =
    let doc = "Export from patches/* branch (includes local modifications) \
               instead of vendor/* branch (pristine upstream)." in
    Arg.(value & flag & info ["from-patches"; "p"] ~doc)
  in
  let run () name backend_str from_patches =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let backend = match Unpac.Promote.backend_of_string backend_str with
      | Some b -> b
      | None ->
          Format.eprintf "Error: Unknown backend '%s'. Use 'opam' or 'git'.@." backend_str;
          exit 1
    in
    match Unpac.Promote.export ~proc_mgr ~root ~name ~backend ~from_patches with
    | Unpac.Promote.Exported { name; backend; source_branch; export_branch; commits } ->
        Format.printf "Exported %s (%s backend)@." name (Unpac.Promote.backend_to_string backend);
        Format.printf "  Source: %s@." source_branch;
        Format.printf "  Export: %s (%d commits)@." export_branch commits;
        Format.printf "@.Files moved from vendor/%s/%s/ to repository root.@."
          (Unpac.Promote.backend_to_string backend) name;
        Format.printf "@.Next steps:@.";
        Format.printf "  unpac export-set-remote %s <url>@." name;
        Format.printf "  unpac export-push %s --backend %s@." name backend_str
    | Unpac.Promote.Not_vendored name ->
        Format.eprintf "Error: No vendor branch found for '%s'.@." name;
        Format.eprintf "Check available packages with: unpac opam list / unpac git list@.";
        exit 1
    | Unpac.Promote.Already_exported name ->
        Format.eprintf "Error: Export branch already exists for '%s'.@." name;
        Format.eprintf "Delete it first with: git branch -D %s/export/%s@."
          backend_str name;
        exit 1
    | Unpac.Promote.Export_failed { name; error } ->
        Format.eprintf "Error exporting %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "export" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ backend_arg $ from_patches_arg)

(* Export set-remote command *)
let export_set_remote_cmd =
  let doc = "Set the remote URL for pushing exports." in
  let man = [
    `S Manpage.s_description;
    `P "Configures a git remote for pushing exported packages to an upstream \
        repository. The remote is named 'export-<name>'.";
    `S Manpage.s_examples;
    `Pre "  unpac export-set-remote brotli git@github.com:me/brotli.git";
  ] in
  let name_arg =
    let doc = "Name of the package." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let url_arg =
    let doc = "Remote URL (git SSH or HTTPS URL)." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let run () name url =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    match Unpac.Promote.set_export_remote ~proc_mgr ~root ~name ~url with
    | `Created ->
        Format.printf "Created export remote for %s: %s@." name url;
        Format.printf "@.Push with: unpac export-push %s --backend <backend>@." name
    | `Updated ->
        Format.printf "Updated export remote for %s: %s@." name url
    | `Existed ->
        Format.printf "Export remote already set for %s: %s@." name url
  in
  let info = Cmd.info "export-set-remote" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ url_arg)

(* Export push command *)
let export_push_cmd =
  let doc = "Push an export branch to its configured remote." in
  let man = [
    `S Manpage.s_description;
    `P "Pushes an export branch to the remote configured via 'export-set-remote'. \
        The export branch is pushed as 'main' on the remote repository.";
    `S Manpage.s_examples;
    `Pre "  unpac export-push brotli --backend opam";
  ] in
  let name_arg =
    let doc = "Name of the package to push." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let backend_arg =
    let doc = "Vendor backend type: opam or git." in
    Arg.(required & opt (some string) None & info ["backend"; "b"] ~docv:"BACKEND" ~doc)
  in
  let run () name backend_str =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let backend = match Unpac.Promote.backend_of_string backend_str with
      | Some b -> b
      | None ->
          Format.eprintf "Error: Unknown backend '%s'. Use 'opam' or 'git'.@." backend_str;
          exit 1
    in
    match Unpac.Promote.push_export ~proc_mgr ~root ~name ~backend with
    | Unpac.Promote.Export_pushed { name = _; backend; remote; branch; commits } ->
        Format.printf "Pushed %s (%d commits) to %s@." branch commits remote;
        Format.printf "Backend: %s@." (Unpac.Promote.backend_to_string backend);
        Format.printf "@.Export pushed as 'main' on remote.@."
    | Unpac.Promote.Export_not_found name ->
        Format.eprintf "Error: No export branch found for '%s'.@." name;
        Format.eprintf "Export first with: unpac export %s --backend %s@." name backend_str;
        exit 1
    | Unpac.Promote.No_export_remote name ->
        Format.eprintf "Error: No export remote configured for '%s'.@." name;
        Format.eprintf "Set one with: unpac export-set-remote %s <url>@." name;
        exit 1
    | Unpac.Promote.Export_push_failed { name; error } ->
        Format.eprintf "Error pushing export %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "export-push" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ backend_arg)

(* Export list command *)
let export_list_cmd =
  let doc = "List all exported packages." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let exports = Unpac.Promote.list_exports ~proc_mgr ~root in
    if exports = [] then
      Format.printf "No exported packages.@."
    else begin
      Format.printf "Exported packages:@.";
      List.iter (fun (backend, name) ->
        let remote = Unpac.Promote.get_export_remote ~proc_mgr ~root ~name in
        Format.printf "  %s (%s)%s@." name
          (Unpac.Promote.backend_to_string backend)
          (match remote with Some url -> " → " ^ url | None -> "")
      ) exports
    end
  in
  let info = Cmd.info "export-list" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Project command group *)
let project_cmd =
  let doc = "Project management commands." in
  let man = [
    `S Manpage.s_description;
    `P "Projects are isolated branches where you merge vendored dependencies \
        and build your application. Each project is a git worktree at \
        project/<name>/ with its own branch project/<name>.";
    `P "Workflow:";
    `Pre "  1. Create a project:     unpac project new myapp
  2. Vendor dependencies:  unpac opam add foo
  3. Merge into project:   unpac opam merge foo myapp
  4. Build in project:     cd project/myapp && dune build";
    `P "Multiple projects can share the same vendored dependencies - \
        each project merges the patches branch independently.";
    `S "PROMOTING PROJECTS";
    `P "Once a project is complete, you can promote it to a vendored library:";
    `Pre "  unpac project promote mylib --backend opam";
    `P "This creates clean vendor branches (filtering out vendored deps) so \
        the library can be merged into other projects.";
    `S "PUBLISHING PROJECTS";
    `P "Projects can be pushed to independent repositories:";
    `Pre "  unpac project set-remote mylib git@github.com:me/mylib.git
  unpac project push mylib";
    `S "EXPORTING AS STANDALONE LIBRARY";
    `P "To publish a promoted project as a standalone opam library:";
    `Pre "  # 1. Promote project to opam vendor
  unpac project promote mylib --backend opam

  # 2. Export with your patches (files moved to root)
  unpac export mylib --backend opam --from-patches

  # 3. Configure remotes and push
  unpac export-set-remote mylib git@github.com:me/mylib.git
  unpac export-push mylib --backend opam

  # 4. Configure upstream for pulling updates
  unpac opam set-upstream mylib git@github.com:me/mylib.git";
    `P "The export branch has files at repository root (not in vendor/), \
        suitable for a standalone git repository. The upstream remote \
        allows 'unpac opam update' to fetch changes back.";
  ] in
  let info = Cmd.info "project" ~doc ~man in
  Cmd.group info [
    project_new_cmd;
    project_list_cmd;
    project_info_cmd;
    project_promote_cmd;
    project_set_remote_cmd;
    project_push_cmd;
  ]

(* Opam repo add command *)
let opam_repo_add_cmd =
  let doc = "Add an opam repository for package lookups." in
  let name_arg =
    let doc = "Name for the repository." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let path_arg =
    let doc = "Path to the repository (local directory)." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH" ~doc)
  in
  let run () name path =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let config = load_config root in
    (* Check if already exists *)
    if Unpac.Config.find_repo config name <> None then begin
      Format.eprintf "Repository '%s' already exists@." name;
      exit 1
    end;
    (* Resolve to absolute path *)
    let abs_path =
      if Filename.is_relative path then
        Filename.concat (Sys.getcwd ()) path
      else path
    in
    (* Check path exists *)
    if not (Sys.file_exists abs_path && Sys.is_directory abs_path) then begin
      Format.eprintf "Error: '%s' is not a valid directory@." abs_path;
      exit 1
    end;
    let repo : Unpac.Config.repo_config = {
      repo_name = name;
      source = Local abs_path;
    } in
    let config' = Unpac.Config.add_repo config repo in
    save_config ~proc_mgr root config' (Printf.sprintf "Add repository %s" name);
    Format.printf "Added repository %s at %s@." name abs_path;
    Format.printf "@.Next: unpac opam add <package>   # vendor a package by name@."
  in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ path_arg)

(* Opam repo list command *)
let opam_repo_list_cmd =
  let doc = "List configured opam repositories." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr:_ ~root ->
    let config = load_config root in
    if config.opam.repositories = [] then begin
      Format.printf "No repositories configured@.";
      Format.printf "@.Hint: unpac opam repo add <name> <path>@."
    end else
      List.iter (fun (r : Unpac.Config.repo_config) ->
        let path = match r.source with
          | Local p -> p
          | Remote u -> u
        in
        Format.printf "%s: %s@." r.repo_name path
      ) config.opam.repositories
  in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Opam repo remove command *)
let opam_repo_remove_cmd =
  let doc = "Remove an opam repository." in
  let name_arg =
    let doc = "Name of the repository to remove." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let config = load_config root in
    if Unpac.Config.find_repo config name = None then begin
      Format.eprintf "Repository '%s' not found@." name;
      exit 1
    end;
    let config' = Unpac.Config.remove_repo config name in
    save_config ~proc_mgr root config' (Printf.sprintf "Remove repository %s" name);
    Format.printf "Removed repository %s@." name
  in
  let info = Cmd.info "remove" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Opam repo command group *)
let opam_repo_cmd =
  let doc = "Manage opam repositories." in
  let info = Cmd.info "repo" ~doc in
  Cmd.group info [opam_repo_add_cmd; opam_repo_list_cmd; opam_repo_remove_cmd]

(* Opam config compiler command *)
let opam_config_compiler_cmd =
  let doc = "Set or show the OCaml compiler version for dependency solving." in
  let version_arg =
    let doc = "OCaml version to use (e.g., 5.2.0)." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc)
  in
  let run () version_opt =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let config = load_config root in
    match version_opt with
    | None ->
        (* Show current compiler *)
        (match Unpac.Config.get_compiler config with
         | Some v -> Format.printf "Compiler: %s@." v
         | None -> Format.printf "No compiler configured@.@.Hint: unpac opam config compiler 5.2.0@.")
    | Some version ->
        (* Set compiler *)
        let config' = Unpac.Config.set_compiler config version in
        save_config ~proc_mgr root config' (Printf.sprintf "Set compiler to %s" version);
        Format.printf "Compiler set to %s@." version
  in
  let info = Cmd.info "compiler" ~doc in
  Cmd.v info Term.(const run $ logging_term $ version_arg)

(* Opam config command group *)
let opam_config_cmd =
  let doc = "Configure opam settings." in
  let info = Cmd.info "config" ~doc in
  Cmd.group info [opam_config_compiler_cmd]

(* Opam add command - enhanced to support package names and dependency solving *)
let opam_add_cmd =
  let doc = "Vendor an opam package (by name or git URL)." in
  let pkg_arg =
    let doc = "Package name or git URL." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let name_arg =
    let doc = "Override package name." in
    Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"NAME" ~doc)
  in
  let version_arg =
    let doc = "Package version (when adding by name)." in
    Arg.(value & opt (some string) None & info ["V"; "pkg-version"] ~docv:"VERSION" ~doc)
  in
  let branch_arg =
    let doc = "Git branch to vendor (defaults to remote default)." in
    Arg.(value & opt (some string) None & info ["b"; "branch"] ~docv:"BRANCH" ~doc)
  in
  let solve_arg =
    let doc = "Solve dependencies and vendor all required packages." in
    Arg.(value & flag & info ["solve"] ~doc)
  in
  let cache_arg =
    let doc = "Path to vendor cache (overrides config and UNPAC_VENDOR_CACHE env var)." in
    Arg.(value & opt (some string) None & info ["cache"] ~docv:"PATH" ~doc)
  in
  let run () pkg name_opt version_opt branch_opt solve cli_cache =
    with_root @@ fun ~env:_ ~fs ~proc_mgr ~root ->
    let config = load_config root in
    let cache = resolve_cache ~proc_mgr ~fs ~config ~cli_cache in

    (* Wrap entire operation with audit logging *)
    let args = [pkg] @ (match name_opt with Some n -> ["--name"; n] | None -> [])
               @ (if solve then ["--solve"] else []) in
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Opam_add ~args @@ fun _ctx ->

    if solve then begin
      (* Solve dependencies and add all packages *)
      let repos = config.opam.repositories in
      if repos = [] then begin
        Format.eprintf "No repositories configured. Add one with: unpac opam repo add <name> <path>@.";
        exit 1
      end;
      let ocaml_version = match Unpac.Config.get_compiler config with
        | Some v -> v
        | None ->
            Format.eprintf "No compiler version configured.@.";
            Format.eprintf "Set one with: unpac opam config compiler 5.2.0@.";
            exit 1
      in
      (* Get repo paths *)
      let repo_paths = List.map (fun (r : Unpac.Config.repo_config) ->
        match r.source with
        | Unpac.Config.Local p -> p
        | Unpac.Config.Remote u -> u  (* TODO: handle remote repos *)
      ) repos in
      Format.printf "Solving dependencies for %s...@." pkg;
      match Unpac_opam.Solver.solve ~repos:repo_paths ~ocaml_version ~packages:[pkg] with
      | Error msg ->
          Format.eprintf "Dependency solving failed:@.%s@." msg;
          exit 1
      | Ok result ->
          let pkgs = result.packages in
          Format.printf "Solution found: %d packages@." (List.length pkgs);
          List.iter (fun p ->
            Format.printf "  %s.%s@."
              (OpamPackage.Name.to_string (OpamPackage.name p))
              (OpamPackage.Version.to_string (OpamPackage.version p))
          ) pkgs;

          (* Group packages by dev-repo to avoid duplicating sources *)
          let groups = group_packages_by_dev_repo ~config pkgs in
          Format.printf "@.Grouped into %d unique repositories:@." (List.length groups);
          List.iter (fun (g : package_group) ->
            if List.length g.packages > 1 then
              Format.printf "  %s (%d packages: %s)@."
                g.canonical_name
                (List.length g.packages)
                (String.concat ", " g.packages)
            else
              Format.printf "  %s@." g.canonical_name
          ) groups;

          Format.printf "@.Vendoring repositories...@.";
          let added = ref 0 in
          let failed = ref 0 in
          let config = ref config in
          List.iter (fun (g : package_group) ->
            (* Use canonical name as vendor name, dev-repo as URL *)
            let url = if String.starts_with ~prefix:"git+" g.dev_repo then
              String.sub g.dev_repo 4 (String.length g.dev_repo - 4)
            else g.dev_repo in
            let info : Unpac.Backend.package_info = {
              name = g.canonical_name;
              url;
              branch = None;
            } in
            match Unpac_opam.Opam.add_package ~proc_mgr ~root ?cache info with
            | Unpac.Backend.Added { name = pkg_name; sha } ->
                (* Record in config for remote recreation *)
                let vendored : Unpac.Config.vendored_package = {
                  pkg_name; pkg_url = url; pkg_branch = None
                } in
                config := Unpac.Config.add_vendored_package !config vendored;
                Format.printf "Added %s (%s)@." pkg_name (String.sub sha 0 7);
                if List.length g.packages > 1 then
                  Format.printf "  Contains: %s@." (String.concat ", " g.packages);
                incr added
            | Unpac.Backend.Already_exists pkg_name ->
                Format.printf "Package %s already vendored@." pkg_name
            | Unpac.Backend.Failed { name = pkg_name; error } ->
                Format.eprintf "Error adding %s: %s@." pkg_name error;
                incr failed
          ) groups;
          (* Save config with all vendored packages *)
          if !added > 0 then
            save_config ~proc_mgr root !config "Record vendored packages in config";
          Format.printf "@.Done: %d repositories added, %d failed@." !added !failed;
          if !failed > 0 then exit 1
    end else begin
      (* Single package mode *)
      let url, name =
        if is_url_or_path pkg then begin
          (* It's a URL *)
          let n = match name_opt with
            | Some n -> n
            | None ->
                let base = Filename.basename pkg in
                if String.ends_with ~suffix:".git" base then
                  String.sub base 0 (String.length base - 4)
                else base
          in
          (pkg, n)
        end else begin
          (* It's a package name - look up in repositories *)
          let repos = config.opam.repositories in
          if repos = [] then begin
            Format.eprintf "No repositories configured. Add one with: unpac opam repo add <name> <path>@.";
            exit 1
          end;
          match Unpac_opam.Repo.find_package ~repos ~name:pkg ?version:version_opt () with
          | None ->
              Format.eprintf "Package '%s' not found in configured repositories@." pkg;
              exit 1
          | Some result ->
              match result.metadata.dev_repo with
              | None ->
                  Format.eprintf "Package '%s' has no dev-repo field@." pkg;
                  exit 1
              | Some dev_repo ->
                  (* Strip git+ prefix if present (opam dev-repo format) *)
                  let url = if String.starts_with ~prefix:"git+" dev_repo then
                    String.sub dev_repo 4 (String.length dev_repo - 4)
                  else dev_repo in
                  let n = match name_opt with Some n -> n | None -> pkg in
                  (url, n)
        end
      in

      let info : Unpac.Backend.package_info = {
        name;
        url;
        branch = branch_opt;
      } in
      match Unpac_opam.Opam.add_package ~proc_mgr ~root ?cache info with
      | Unpac.Backend.Added { name = pkg_name; sha } ->
          (* Record in config for remote recreation on fresh clones *)
          let vendored : Unpac.Config.vendored_package = {
            pkg_name; pkg_url = url; pkg_branch = branch_opt
          } in
          let config = Unpac.Config.add_vendored_package config vendored in
          save_config ~proc_mgr root config "Record vendored package in config";
          Format.printf "Added %s (%s)@." pkg_name (String.sub sha 0 7);
          Format.printf "@.Next steps:@.";
          Format.printf "  unpac opam edit %s              # make local changes@." pkg_name;
          Format.printf "  unpac opam merge %s <project>   # merge into a project@." pkg_name
      | Unpac.Backend.Already_exists name ->
          Format.printf "Package %s already vendored@." name
      | Unpac.Backend.Failed { name; error } ->
          Format.eprintf "Error adding %s: %s@." name error;
          exit 1
    end
  in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg $ name_arg $ version_arg $ branch_arg $ solve_arg $ cache_arg)

(* Opam list command *)
let opam_list_cmd =
  let doc = "List vendored opam packages." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if packages = [] then begin
      Format.printf "No packages vendored@.";
      Format.printf "@.Hint: unpac opam add <package>@."
    end else
      List.iter (Format.printf "%s@.") packages
  in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Opam edit command *)
let opam_edit_cmd =
  let doc = "Open a package's patches worktree for editing. \
             Also creates a vendor worktree for reference." in
  let pkg_arg =
    let doc = "Package name to edit." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () pkg =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    (* Check package exists *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if not (List.mem pkg packages) then begin
      Format.eprintf "Package '%s' is not vendored@." pkg;
      exit 1
    end;
    (* Ensure both patches and vendor worktrees exist *)
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Opam_patches pkg);
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Opam_vendor pkg);
    let patches_path = snd (Unpac.Worktree.path root (Unpac.Worktree.Opam_patches pkg)) in
    let vendor_path = snd (Unpac.Worktree.path root (Unpac.Worktree.Opam_vendor pkg)) in
    Format.printf "Editing %s@." pkg;
    Format.printf "@.";
    Format.printf "Worktrees created:@.";
    Format.printf "  patches: %s  (make changes here)@." patches_path;
    Format.printf "  vendor:  %s  (original for reference)@." vendor_path;
    Format.printf "@.";
    Format.printf "Make your changes in the patches worktree, then:@.";
    Format.printf "  cd %s@." patches_path;
    Format.printf "  git add -A && git commit -m 'your message'@.";
    Format.printf "@.";
    Format.printf "When done: unpac opam done %s@." pkg
  in
  let info = Cmd.info "edit" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg)

(* Opam done command *)
let opam_done_cmd =
  let doc = "Close a package's patches and vendor worktrees after editing." in
  let pkg_arg =
    let doc = "Package name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () pkg =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let patches_kind = Unpac.Worktree.Opam_patches pkg in
    let vendor_kind = Unpac.Worktree.Opam_vendor pkg in
    if not (Unpac.Worktree.exists root patches_kind) then begin
      Format.eprintf "No editing session for '%s'@." pkg;
      exit 1
    end;
    (* Check for uncommitted changes in patches worktree *)
    let wt_path = Unpac.Worktree.path root patches_kind in
    let status = Unpac.Git.run_exn ~proc_mgr ~cwd:wt_path ["status"; "--porcelain"] in
    if String.trim status <> "" then begin
      Format.eprintf "Warning: uncommitted changes in %s@." pkg;
      Format.eprintf "Commit or discard them before closing.@.";
      exit 1
    end;
    (* Remove both worktrees *)
    Unpac.Worktree.remove ~proc_mgr root patches_kind;
    if Unpac.Worktree.exists root vendor_kind then
      Unpac.Worktree.remove ~proc_mgr root vendor_kind;
    Format.printf "Closed editing session for %s@." pkg;
    Format.printf "@.Next steps:@.";
    Format.printf "  unpac opam diff %s               # view your changes@." pkg;
    Format.printf "  unpac opam merge %s <project>    # merge into a project@." pkg
  in
  let info = Cmd.info "done" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg)

(* Opam set-upstream command *)
let opam_set_upstream_cmd =
  let doc = "Set the upstream URL for a vendored opam package." in
  let man = [
    `S Manpage.s_description;
    `P "Configures the upstream git URL for a vendored opam package. \
        This is used by 'unpac opam update' to fetch new changes from upstream.";
    `P "For packages added via 'unpac opam add', the upstream is automatically \
        configured from the opam source URL. This command is mainly useful for \
        promoted local projects that don't have an opam source.";
    `S Manpage.s_examples;
    `Pre "  unpac opam set-upstream ocaml-zstd git@github.com:user/ocaml-zstd.git";
    `S "SEE ALSO";
    `P "unpac-opam-update(1), unpac-export-set-remote(1)";
  ] in
  let name_arg =
    let doc = "Name of the package." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let url_arg =
    let doc = "Upstream URL (git SSH or HTTPS URL)." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let run () name url =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    match Unpac.Promote.set_upstream_remote ~proc_mgr ~root ~name ~url with
    | `Created ->
        Format.printf "Set upstream for %s: %s@." name url;
        Format.printf "@.You can now run: unpac opam update %s@." name
    | `Updated ->
        Format.printf "Updated upstream for %s: %s@." name url
    | `Existed ->
        Format.printf "Upstream already set for %s: %s@." name url
  in
  let info = Cmd.info "set-upstream" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ url_arg)

(* Opam update command *)
let opam_update_cmd =
  let doc = "Update a vendored opam package from upstream." in
  let name_arg =
    let doc = "Package name to update." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Opam_update ~args:[name] @@ fun _ctx ->
    match Unpac_opam.Opam.update_package ~proc_mgr ~root name with
    | Unpac.Backend.Updated { name = pkg_name; old_sha; new_sha } ->
        Format.printf "Updated %s: %s -> %s@." pkg_name
          (String.sub old_sha 0 7) (String.sub new_sha 0 7);
        Format.printf "@.Next steps:@.";
        Format.printf "  unpac opam diff %s               # view changes@." pkg_name;
        Format.printf "  unpac opam merge %s <project>    # merge into a project@." pkg_name
    | Unpac.Backend.No_changes name ->
        Format.printf "%s is up to date@." name
    | Unpac.Backend.Update_failed { name; error } ->
        Format.eprintf "Error updating %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "update" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Opam merge command *)
let opam_merge_cmd =
  let doc = "Merge vendored opam packages into a project. \
             Use --solve to merge a package and its dependencies, \
             or --all to merge all vendored packages." in
  let args =
    let doc = "PACKAGE PROJECT (or just PROJECT with --all)." in
    Arg.(value & pos_all string [] & info [] ~docv:"ARGS" ~doc)
  in
  let all_flag =
    let doc = "Merge all vendored packages into the project." in
    Arg.(value & flag & info ["all"] ~doc)
  in
  let solve_flag =
    let doc = "Solve dependencies for PACKAGE and merge all solved packages into the project." in
    Arg.(value & flag & info ["solve"] ~doc)
  in
  let run () args all solve =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let config = load_config root in
    let audit_args = args @ (if all then ["--all"] else []) @ (if solve then ["--solve"] else []) in
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Opam_merge ~args:audit_args @@ fun _ctx ->

    let merge_one ~project pkg =
      let patches_branch = Unpac_opam.Opam.patches_branch pkg in
      match Unpac.Backend.merge_to_project ~proc_mgr ~root ~project ~patches_branch with
      | Ok () ->
          Format.printf "Merged %s@." pkg;
          true
      | Error (`Conflict files) ->
          Format.eprintf "Merge conflict in %s:@." pkg;
          List.iter (Format.eprintf "  %s@.") files;
          false
    in

    let merge_packages packages project =
      Format.printf "Merging %d packages into project %s...@." (List.length packages) project;
      let (successes, failures) = List.fold_left (fun (s, f) pkg ->
        if merge_one ~project pkg then (s + 1, f) else (s, f + 1)
      ) (0, 0) packages in
      Format.printf "@.Done: %d merged" successes;
      if failures > 0 then Format.printf ", %d had conflicts" failures;
      Format.printf "@.";
      if failures > 0 then begin
        Format.eprintf "Resolve conflicts in project/%s and commit.@." project;
        exit 1
      end else
        Format.printf "Next: Build your project in project/%s@." project
    in

    if solve then begin
      (* Solve dependencies and merge all solved packages that are vendored *)
      let pkg, project = match args with
        | [pkg; project] -> pkg, project
        | _ ->
            Format.eprintf "Usage: unpac opam merge --solve PACKAGE PROJECT@.";
            exit 1
      in
      let repos = config.opam.repositories in
      if repos = [] then begin
        Format.eprintf "No repositories configured. Add one with: unpac opam repo add <name> <path>@.";
        exit 1
      end;
      let ocaml_version = match Unpac.Config.get_compiler config with
        | Some v -> v
        | None ->
            Format.eprintf "No compiler version configured.@.";
            Format.eprintf "Set one with: unpac opam config compiler 5.2.0@.";
            exit 1
      in
      let repo_paths = List.map (fun (r : Unpac.Config.repo_config) ->
        match r.source with
        | Unpac.Config.Local p -> p
        | Unpac.Config.Remote u -> u
      ) repos in
      Format.printf "Solving dependencies for %s...@." pkg;
      match Unpac_opam.Solver.solve ~repos:repo_paths ~ocaml_version ~packages:[pkg] with
      | Error msg ->
          Format.eprintf "Dependency solving failed:@.%s@." msg;
          exit 1
      | Ok result ->
          (* Group by dev-repo to get canonical names *)
          let groups = group_packages_by_dev_repo ~config result.packages in
          let canonical_names = List.map (fun (g : package_group) -> g.canonical_name) groups in
          (* Filter to only vendored packages *)
          let vendored = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
          let to_merge = List.filter (fun name -> List.mem name vendored) canonical_names in
          if to_merge = [] then begin
            Format.eprintf "No vendored packages match the solved dependencies.@.";
            Format.eprintf "Run 'unpac opam add %s --solve' first to vendor them.@." pkg;
            exit 1
          end;
          Format.printf "Found %d vendored packages to merge.@.@." (List.length to_merge);
          merge_packages to_merge project
    end else if all then begin
      (* Merge all vendored packages *)
      let project = match args with
        | [project] -> project
        | _ ->
            Format.eprintf "Usage: unpac opam merge --all PROJECT@.";
            exit 1
      in
      let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
      if packages = [] then begin
        Format.eprintf "No vendored packages to merge.@.";
        exit 1
      end;
      merge_packages packages project
    end else begin
      (* Single package mode *)
      let pkg, project = match args with
        | [pkg; project] -> pkg, project
        | _ ->
            Format.eprintf "Usage: unpac opam merge PACKAGE PROJECT@.";
            exit 1
      in
      if merge_one ~project pkg then
        Format.printf "@.Next: Build your project in project/%s@." project
      else begin
        Format.eprintf "Resolve conflicts in project/%s and commit.@." project;
        exit 1
      end
    end
  in
  let info = Cmd.info "merge" ~doc in
  Cmd.v info Term.(const run $ logging_term $ args $ all_flag $ solve_flag)

(* Opam info command *)
let opam_info_cmd =
  let doc = "Show information about a vendored package." in
  let pkg_arg =
    let doc = "Package name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () pkg =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in
    (* Check package exists *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if not (List.mem pkg packages) then begin
      Format.eprintf "Package '%s' is not vendored@." pkg;
      exit 1
    end;
    (* Get remote URL *)
    let remote = "origin-" ^ pkg in
    let url = Unpac.Git.remote_url ~proc_mgr ~cwd:git remote in
    Format.printf "Package: %s@." pkg;
    (match url with
     | Some u -> Format.printf "URL: %s@." u
     | None -> ());
    (* Get branch SHAs *)
    let upstream = Unpac_opam.Opam.upstream_branch pkg in
    let vendor = Unpac_opam.Opam.vendor_branch pkg in
    let patches = Unpac_opam.Opam.patches_branch pkg in
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git upstream with
     | Some sha -> Format.printf "Upstream: %s@." (String.sub sha 0 7)
     | None -> ());
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git vendor with
     | Some sha -> Format.printf "Vendor: %s@." (String.sub sha 0 7)
     | None -> ());
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git patches with
     | Some sha -> Format.printf "Patches: %s@." (String.sub sha 0 7)
     | None -> ());
    (* Count commits ahead *)
    let log_output = Unpac.Git.run_exn ~proc_mgr ~cwd:git
      ["log"; "--oneline"; vendor ^ ".." ^ patches] in
    let commits = List.length (String.split_on_char '\n' log_output |>
                               List.filter (fun s -> String.trim s <> "")) in
    Format.printf "Local commits: %d@." commits;
    Format.printf "@.Commands:@.";
    Format.printf "  unpac opam diff %s     # view local changes@." pkg;
    Format.printf "  unpac opam edit %s     # edit package@." pkg;
    Format.printf "  unpac opam update %s   # fetch upstream@." pkg
  in
  let info = Cmd.info "info" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg)

(* Opam diff command *)
let opam_diff_cmd =
  let doc = "Show diff between vendor and patches branches." in
  let pkg_arg =
    let doc = "Package name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () pkg =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in
    (* Check package exists *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if not (List.mem pkg packages) then begin
      Format.eprintf "Package '%s' is not vendored@." pkg;
      exit 1
    end;
    let vendor = Unpac_opam.Opam.vendor_branch pkg in
    let patches = Unpac_opam.Opam.patches_branch pkg in
    let diff = Unpac.Git.run_exn ~proc_mgr ~cwd:git
      ["diff"; vendor; patches] in
    if String.trim diff = "" then begin
      Format.printf "No local changes@.";
      Format.printf "@.Hint: unpac opam edit %s   # to make changes@." pkg
    end else begin
      print_string diff;
      Format.printf "@.Next: unpac opam merge %s <project>@." pkg
    end
  in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg)

(* Opam remove command *)
let opam_remove_cmd =
  let doc = "Remove a vendored package." in
  let pkg_arg =
    let doc = "Package name to remove." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () pkg =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in
    (* Check package exists *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if not (List.mem pkg packages) then begin
      Format.eprintf "Package '%s' is not vendored@." pkg;
      exit 1
    end;
    (* Remove worktrees if exist *)
    (try Unpac.Worktree.remove_force ~proc_mgr root (Unpac.Worktree.Opam_upstream pkg) with _ -> ());
    (try Unpac.Worktree.remove_force ~proc_mgr root (Unpac.Worktree.Opam_vendor pkg) with _ -> ());
    (try Unpac.Worktree.remove_force ~proc_mgr root (Unpac.Worktree.Opam_patches pkg) with _ -> ());
    (* Delete branches *)
    let upstream = Unpac_opam.Opam.upstream_branch pkg in
    let vendor = Unpac_opam.Opam.vendor_branch pkg in
    let patches = Unpac_opam.Opam.patches_branch pkg in
    (try Unpac.Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; upstream] |> ignore with _ -> ());
    (try Unpac.Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; vendor] |> ignore with _ -> ());
    (try Unpac.Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; patches] |> ignore with _ -> ());
    (* Remove remote *)
    let remote = "origin-" ^ pkg in
    (try Unpac.Git.run_exn ~proc_mgr ~cwd:git ["remote"; "remove"; remote] |> ignore with _ -> ());
    Format.printf "Removed %s@." pkg;
    Format.printf "@.Hint: unpac opam add <package>   # to add another package@."
  in
  let info = Cmd.info "remove" ~doc in
  Cmd.v info Term.(const run $ logging_term $ pkg_arg)

(* Opam init command - create a new local opam package *)
let opam_init_cmd =
  let doc = "Create a new local opam package (no upstream repository)." in
  let man = [
    `S Manpage.s_description;
    `P "Creates a new opam package that originates locally rather than from \
        an external repository. This is useful for:";
    `I ("New libraries", "Starting a new OCaml library from scratch");
    `I ("Internal packages", "Creating packages that will never be published");
    `I ("Agent-created packages", "AI agents can create new dependencies on-demand");
    `P "The package is created with a minimal scaffold including dune-project \
        and a .opam file. It uses the standard three-tier branch model but \
        with no upstream branch (url='local' in config).";
    `S "PACKAGE STRUCTURE";
    `P "The created package will have:";
    `Pre "  vendor/opam/<name>/
    dune-project        # Dune project file
    <name>.opam         # Opam package file
    lib/
      dune              # Library build rules
      <name>.ml         # Main module (empty)
      <name>.mli        # Interface file (empty)";
    `S Manpage.s_examples;
    `P "Create a new local library:";
    `Pre "  unpac opam init mylib
  unpac opam merge mylib myproject";
    `P "Create with description:";
    `Pre "  unpac opam init mylib --synopsis 'My utility library'";
    `S "LIFECYCLE";
    `P "Local packages can later be published by:";
    `Pre "  1. Push the opam/patches/<name> branch to a git repository
  2. Update config with: unpac opam set-upstream <name> <url>
  3. Submit to opam-repository if desired";
    `S "SEE ALSO";
    `P "unpac-opam-promote(1) for graduating projects to dependencies.";
  ] in
  let name_arg =
    let doc = "Name for the new package. Should be a valid opam package name \
               (lowercase, alphanumeric, hyphens allowed)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let synopsis_arg =
    let doc = "One-line synopsis for the package." in
    Arg.(value & opt string "A local opam package" & info ["synopsis"; "s"] ~docv:"TEXT" ~doc)
  in
  let run () name synopsis =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Opam_init ~args:[name] @@ fun _ctx ->
    let git = Unpac.Worktree.git_dir root in
    let config = load_config root in

    (* Validate package name *)
    if String.length name = 0 then begin
      Format.eprintf "Error: Package name cannot be empty@.";
      exit 1
    end;
    let valid_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' || c = '_' in
    if not (String.for_all valid_char name) then begin
      Format.eprintf "Error: Package name must be lowercase alphanumeric (hyphens/underscores allowed)@.";
      exit 1
    end;

    (* Check if already exists *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if List.mem name packages then begin
      Format.eprintf "Package '%s' already exists@." name;
      exit 1
    end;

    (* Create an orphan branch for vendor *)
    let vendor_branch = Unpac_opam.Opam.vendor_branch name in
    let patches_branch = Unpac_opam.Opam.patches_branch name in
    let vendor_path = "vendor/opam/" ^ name in

    (* Create orphan branch with initial content *)
    Unpac.Git.checkout_orphan ~proc_mgr ~cwd:git vendor_branch;

    (* Remove any existing index content *)
    Unpac.Git.rm_cached_rf ~proc_mgr ~cwd:git;

    (* Create scaffold files in a temporary worktree *)
    let wt_path = Unpac.Worktree.path root (Unpac.Worktree.Opam_vendor name) in
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Opam_vendor name);

    (* Create directory structure *)
    let pkg_dir = Eio.Path.(wt_path / vendor_path) in
    let lib_dir = Eio.Path.(pkg_dir / "lib") in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 lib_dir;

    (* Create dune-project *)
    let dune_project = Printf.sprintf {|(lang dune 3.0)
(name %s)
(generate_opam_files true)
(source (uri "local"))
(authors "Local")
(maintainers "Local")
(package
 (name %s)
 (synopsis "%s")
 (depends
  (ocaml (>= 4.14))))
|} name name synopsis in
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(pkg_dir / "dune-project") dune_project;

    (* Create lib/dune *)
    let lib_dune = Printf.sprintf {|(library
 (name %s)
 (public_name %s))
|} (String.map (fun c -> if c = '-' then '_' else c) name) name in
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(lib_dir / "dune") lib_dune;

    (* Create lib/<name>.ml *)
    let ml_file = Printf.sprintf {|(* %s - A local opam package *)

(** This module was created by [unpac opam init].
    Add your implementation here. *)
|} name in
    let ml_name = String.map (fun c -> if c = '-' then '_' else c) name in
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(lib_dir / (ml_name ^ ".ml")) ml_file;

    (* Create lib/<name>.mli *)
    let mli_file = Printf.sprintf {|(* %s - A local opam package *)

(** This module was created by [unpac opam init].
    Define your interface here. *)
|} name in
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(lib_dir / (ml_name ^ ".mli")) mli_file;

    (* Commit the scaffold *)
    Unpac.Git.add_all ~proc_mgr ~cwd:wt_path;
    Unpac.Git.commit ~proc_mgr ~cwd:wt_path
      ~message:(Printf.sprintf "Initialize local package %s" name);

    (* Get the commit SHA *)
    let sha = Unpac.Git.current_head ~proc_mgr ~cwd:wt_path in

    (* Create patches branch from vendor *)
    Unpac.Git.branch_create ~proc_mgr ~cwd:git
      ~name:patches_branch ~start_point:vendor_branch;

    (* Cleanup worktree *)
    Unpac.Worktree.remove ~proc_mgr root (Unpac.Worktree.Opam_vendor name);

    (* Switch back to main *)
    Unpac.Git.checkout ~proc_mgr ~cwd:git "main";

    (* Record in config with url = "local" *)
    let vendored : Unpac.Config.vendored_package = {
      pkg_name = name; pkg_url = "local"; pkg_branch = None
    } in
    let config = Unpac.Config.add_vendored_package config vendored in
    save_config ~proc_mgr root config (Printf.sprintf "Add local package %s" name);

    Format.printf "Created local package %s (%s)@." name (String.sub sha 0 7);
    Format.printf "@.Package structure:@.";
    Format.printf "  %s/@." vendor_path;
    Format.printf "    dune-project@.";
    Format.printf "    lib/dune@.";
    Format.printf "    lib/%s.ml@." ml_name;
    Format.printf "    lib/%s.mli@." ml_name;
    Format.printf "@.Next steps:@.";
    Format.printf "  unpac opam edit %s              # add code to the package@." name;
    Format.printf "  unpac opam merge %s <project>   # use in a project@." name
  in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ synopsis_arg)

(* Opam promote command - graduate a project to a vendored dependency *)
let opam_promote_cmd =
  let doc = "Promote a project to a vendored opam dependency." in
  let man = [
    `S Manpage.s_description;
    `P "Graduates a project branch to become a vendored opam dependency that \
        other projects can use. This is the lifecycle path for code that:";
    `I ("Started as a project", "Code developed in project/<name> that should \
                                  become a shared library");
    `I ("Needs reuse", "A project that other projects want to depend on");
    `I ("Agent refactoring", "AI agents can extract common code into libraries");
    `P "The project's content is copied to create opam/vendor/<name> and \
        opam/patches/<name> branches. The original project remains unchanged \
        and can be deleted if no longer needed.";
    `S "REQUIREMENTS";
    `P "The project directory should contain a valid dune-project file with \
        the package definition. If not present, a basic one will be created.";
    `S Manpage.s_examples;
    `P "Promote a project to a dependency:";
    `Pre "  unpac opam promote my-utils
  unpac opam merge my-utils other-project";
    `P "Promote with a different name:";
    `Pre "  unpac opam promote my-app --as my-lib";
    `S "LIFECYCLE";
    `P "After promotion:";
    `Pre "  1. The new package appears in 'unpac opam list'
  2. Other projects can merge it with 'unpac opam merge'
  3. Edit with 'unpac opam edit' (changes go to patches branch)
  4. Original project can be deleted if desired";
    `S "SEE ALSO";
    `P "unpac-opam-init(1) for creating new packages from scratch.";
  ] in
  let project_arg =
    let doc = "Name of the project to promote." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let as_arg =
    let doc = "Name for the opam package (defaults to project name)." in
    Arg.(value & opt (some string) None & info ["as"] ~docv:"NAME" ~doc)
  in
  let run () project pkg_name_opt =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let pkg_name = match pkg_name_opt with Some n -> n | None -> project in
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Opam_promote ~args:[project; pkg_name] @@ fun _ctx ->
    let git = Unpac.Worktree.git_dir root in
    let config = load_config root in

    (* Check project exists *)
    let projects = Unpac.Worktree.list_projects ~proc_mgr root in
    if not (List.mem project projects) then begin
      Format.eprintf "Project '%s' does not exist@." project;
      Format.eprintf "@.Available projects:@.";
      List.iter (Format.eprintf "  %s@.") projects;
      exit 1
    end;

    (* Check package doesn't already exist *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if List.mem pkg_name packages then begin
      Format.eprintf "Package '%s' already exists@." pkg_name;
      exit 1
    end;

    let vendor_branch = Unpac_opam.Opam.vendor_branch pkg_name in
    let patches_branch = Unpac_opam.Opam.patches_branch pkg_name in
    let vendor_path = "vendor/opam/" ^ pkg_name in

    (* Create orphan branch for vendor *)
    Unpac.Git.checkout_orphan ~proc_mgr ~cwd:git vendor_branch;
    Unpac.Git.rm_cached_rf ~proc_mgr ~cwd:git;

    (* Create vendor worktree *)
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Opam_vendor pkg_name);
    let vendor_wt = Unpac.Worktree.path root (Unpac.Worktree.Opam_vendor pkg_name) in

    (* Get project worktree or create temporary one *)
    let project_wt = Unpac.Worktree.path root (Unpac.Worktree.Project project) in
    let created_project_wt = not (Sys.file_exists (snd project_wt)) in
    if created_project_wt then
      Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Project project);

    (* Create target directory *)
    let pkg_dir = Eio.Path.(vendor_wt / vendor_path) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 pkg_dir;

    (* Copy project content to vendor path *)
    let rec copy_dir src dst =
      Eio.Path.read_dir src |> List.iter (fun name ->
        if name <> ".git" then begin
          let src_path = Eio.Path.(src / name) in
          let dst_path = Eio.Path.(dst / name) in
          if Eio.Path.is_directory src_path then begin
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst_path;
            copy_dir src_path dst_path
          end else begin
            let content = Eio.Path.load src_path in
            Eio.Path.save ~create:(`Or_truncate 0o644) dst_path content
          end
        end
      )
    in
    copy_dir project_wt pkg_dir;

    (* Commit *)
    Unpac.Git.add_all ~proc_mgr ~cwd:vendor_wt;
    Unpac.Git.commit ~proc_mgr ~cwd:vendor_wt
      ~message:(Printf.sprintf "Promote project %s to package %s" project pkg_name);

    (* Get SHA *)
    let sha = Unpac.Git.current_head ~proc_mgr ~cwd:vendor_wt in

    (* Create patches branch from vendor *)
    Unpac.Git.branch_create ~proc_mgr ~cwd:git
      ~name:patches_branch ~start_point:vendor_branch;

    (* Cleanup *)
    Unpac.Worktree.remove ~proc_mgr root (Unpac.Worktree.Opam_vendor pkg_name);
    if created_project_wt then
      Unpac.Worktree.remove ~proc_mgr root (Unpac.Worktree.Project project);

    (* Switch back to main *)
    Unpac.Git.checkout ~proc_mgr ~cwd:git "main";

    (* Record in config *)
    let vendored : Unpac.Config.vendored_package = {
      pkg_name; pkg_url = "local"; pkg_branch = None
    } in
    let config = Unpac.Config.add_vendored_package config vendored in
    save_config ~proc_mgr root config (Printf.sprintf "Promote project %s to package %s" project pkg_name);

    Format.printf "Promoted project %s to package %s (%s)@." project pkg_name (String.sub sha 0 7);
    Format.printf "@.The package is now available as a vendored dependency.@.";
    Format.printf "@.Next steps:@.";
    Format.printf "  unpac opam merge %s <other-project>   # use in another project@." pkg_name;
    Format.printf "  unpac opam edit %s                    # make changes@." pkg_name;
    if project <> pkg_name then
      Format.printf "  unpac project remove %s               # remove original project (optional)@." project
  in
  let info = Cmd.info "promote" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ project_arg $ as_arg)

(* Opam command group *)
let opam_cmd =
  let doc = "Opam package vendoring commands." in
  let man = [
    `S Manpage.s_description;
    `P "Vendor OCaml packages from opam repositories or create new local packages. \
        Uses a three-tier branch model for conflict-free vendoring:";
    `I ("opam/upstream/<pkg>", "Tracks the original repository state (empty for local packages)");
    `I ("opam/vendor/<pkg>", "Clean snapshot used as merge base");
    `I ("opam/patches/<pkg>", "Local modifications on top of vendor");
    `S "PACKAGE SOURCES";
    `P "Packages can come from three sources:";
    `I ("External (unpac opam add)", "Vendor from opam repository or git URL. \
                                       Has upstream tracking for updates.");
    `I ("Local (unpac opam init)", "Create a new package from scratch. \
                                    No upstream, recorded as url='local' in config.");
    `I ("Promoted (unpac opam promote)", "Graduate a project to a dependency. \
                                          Allows code reuse between projects.");
    `S "TYPICAL WORKFLOW - External Packages";
    `P "1. Configure an opam repository:";
    `Pre "  unpac opam repo add default /path/to/opam-repository";
    `P "2. Set the OCaml compiler version for dependency solving:";
    `Pre "  unpac opam config compiler 5.2.0";
    `P "3. Vendor a package with dependency solving:";
    `Pre "  unpac opam add mypackage --solve";
    `P "4. Create a project and merge dependencies:";
    `Pre "  unpac project new myapp
  unpac opam merge mypackage myapp --solve";
    `P "5. Build in the project directory:";
    `Pre "  cd project/myapp && dune build";
    `S "TYPICAL WORKFLOW - Local Packages";
    `P "1. Create a new local package:";
    `Pre "  unpac opam init mylib --synopsis 'My utility library'";
    `P "2. Add code to the package:";
    `Pre "  unpac opam edit mylib
  # edit files in vendor/opam/mylib-patches/
  git add -A && git commit -m 'implement mylib'
  unpac opam done mylib";
    `P "3. Use in a project:";
    `Pre "  unpac opam merge mylib myproject";
    `S "TYPICAL WORKFLOW - Promoting Projects";
    `P "When a project should become a shared library:";
    `Pre "  unpac opam promote myproject --as mylib
  unpac opam merge mylib other-project";
    `S "MAKING LOCAL CHANGES";
    `P "1. Open package for editing (creates worktrees):";
    `Pre "  unpac opam edit mypackage";
    `P "2. Make changes in the patches worktree:";
    `Pre "  cd vendor/opam/mypackage-patches
  # edit files...
  git add -A && git commit -m 'my changes'";
    `P "3. Close the editing session:";
    `Pre "  unpac opam done mypackage";
    `P "4. View your changes:";
    `Pre "  unpac opam diff mypackage";
    `S "UPDATING FROM UPSTREAM";
    `P "For packages with external upstreams (added via 'opam add'):";
    `Pre "  unpac opam update mypackage
  unpac opam merge mypackage myapp";
    `P "For promoted local packages, first configure the upstream URL:";
    `Pre "  unpac opam set-upstream mylib git@github.com:me/mylib.git
  unpac opam update mylib";
    `S "FOR AI AGENTS";
    `P "When an agent needs to create a new dependency:";
    `Pre "  # Option 1: Create from scratch
  unpac opam init new-lib --synopsis 'Agent-created library'
  unpac opam edit new-lib
  # ... add implementation ...
  unpac opam done new-lib
  unpac opam merge new-lib target-project";
    `Pre "  # Option 2: Extract from existing project
  unpac opam promote existing-project --as new-lib
  unpac opam merge new-lib other-project";
    `P "Local packages have url='local' in unpac.toml and can be identified with:";
    `Pre "  unpac opam info <package>  # shows URL: local";
    `S "COMMANDS";
  ] in
  let info = Cmd.info "opam" ~doc ~man in
  Cmd.group info [
    opam_repo_cmd;
    opam_config_cmd;
    opam_add_cmd;
    opam_init_cmd;
    opam_promote_cmd;
    opam_list_cmd;
    opam_edit_cmd;
    opam_done_cmd;
    opam_set_upstream_cmd;
    opam_update_cmd;
    opam_merge_cmd;
    opam_info_cmd;
    opam_diff_cmd;
    opam_remove_cmd;
  ]

(* Git add command *)
let git_add_cmd =
  let doc = "Vendor a git repository." in
  let url_arg =
    let doc = "Git URL to clone from." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let name_arg =
    let doc = "Override repository name (default: derived from URL)." in
    Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"NAME" ~doc)
  in
  let branch_arg =
    let doc = "Git branch or tag to vendor (default: remote default)." in
    Arg.(value & opt (some string) None & info ["b"; "branch"] ~docv:"REF" ~doc)
  in
  let subdir_arg =
    let doc = "Extract only this subdirectory from the repository." in
    Arg.(value & opt (some string) None & info ["subdir"] ~docv:"PATH" ~doc)
  in
  let cache_arg =
    let doc = "Path to vendor cache." in
    Arg.(value & opt (some string) None & info ["cache"] ~docv:"PATH" ~doc)
  in
  let run () url name_opt branch_opt subdir_opt cli_cache =
    with_root @@ fun ~env:_ ~fs ~proc_mgr ~root ->
    let config = load_config root in
    let cache = resolve_cache ~proc_mgr ~fs ~config ~cli_cache in
    let audit_args = [url] @ (match name_opt with Some n -> ["--name"; n] | None -> []) in
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Git_add ~args:audit_args @@ fun _ctx ->

    let name = match name_opt with
      | Some n -> n
      | None ->
          let base = Filename.basename url in
          if String.ends_with ~suffix:".git" base then
            String.sub base 0 (String.length base - 4)
          else base
    in

    let info : Unpac.Git_backend.repo_info = {
      name; url; branch = branch_opt; subdir = subdir_opt;
    } in

    match Unpac.Git_backend.add_repo ~proc_mgr ~root ?cache info with
    | Unpac.Backend.Added { name = repo_name; sha } ->
        Format.printf "Added %s (%s)@." repo_name (String.sub sha 0 7);
        let repo_config : Unpac.Config.git_repo_config = {
          git_name = name; git_url = url;
          git_branch = branch_opt; git_subdir = subdir_opt;
        } in
        let config' = Unpac.Config.add_git_repo config repo_config in
        save_config ~proc_mgr root config' (Printf.sprintf "Add git repo %s" name);
        Format.printf "@.Next steps:@.";
        Format.printf "  unpac git edit %s              # make local changes@." repo_name;
        Format.printf "  unpac git merge %s <project>   # merge into a project@." repo_name
    | Unpac.Backend.Already_exists name ->
        Format.printf "Repository %s already vendored@." name
    | Unpac.Backend.Failed { name; error } ->
        Format.eprintf "Error adding %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_term $ url_arg $ name_arg $ branch_arg $ subdir_arg $ cache_arg)

(* Git list command *)
let git_list_cmd =
  let doc = "List vendored git repositories." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if repos = [] then begin
      Format.printf "No git repositories vendored@.";
      Format.printf "@.Hint: unpac git add <url>@."
    end else
      List.iter (Format.printf "%s@.") repos
  in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Git update command *)
let git_update_cmd =
  let doc = "Update a vendored git repository from upstream." in
  let name_arg =
    let doc = "Repository name to update." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Git_update ~args:[name] @@ fun _ctx ->
    match Unpac.Git_backend.update_repo ~proc_mgr ~root name with
    | Unpac.Backend.Updated { name = repo_name; old_sha; new_sha } ->
        Format.printf "Updated %s: %s -> %s@." repo_name
          (String.sub old_sha 0 7) (String.sub new_sha 0 7)
    | Unpac.Backend.No_changes name ->
        Format.printf "%s is up to date@." name
    | Unpac.Backend.Update_failed { name; error } ->
        Format.eprintf "Error updating %s: %s@." name error;
        exit 1
  in
  let info = Cmd.info "update" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git merge command *)
let git_merge_cmd =
  let doc = "Merge a vendored git repository into a project." in
  let name_arg =
    let doc = "Repository name to merge." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let project_arg =
    let doc = "Project to merge into." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PROJECT" ~doc)
  in
  let run () name project =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    with_audit ~proc_mgr ~root ~operation_type:Unpac.Audit.Git_merge ~args:[name; project] @@ fun _ctx ->
    let patches_branch = Unpac.Git_backend.patches_branch name in
    match Unpac.Backend.merge_to_project ~proc_mgr ~root ~project ~patches_branch with
    | Ok () ->
        Format.printf "Merged %s into %s@." name project;
        Format.printf "@.Next: Build your project in project/%s@." project
    | Error (`Conflict files) ->
        Format.eprintf "Merge conflict in %s:@." name;
        List.iter (Format.eprintf "  %s@.") files;
        Format.eprintf "Resolve conflicts in project/%s and commit.@." project;
        exit 1
  in
  let info = Cmd.info "merge" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg $ project_arg)

(* Git info command *)
let git_info_cmd =
  let doc = "Show information about a vendored git repository." in
  let name_arg =
    let doc = "Repository name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then begin
      Format.eprintf "Repository '%s' is not vendored@." name;
      exit 1
    end;
    let remote = "origin-" ^ name in
    let url = Unpac.Git.remote_url ~proc_mgr ~cwd:git remote in
    Format.printf "Repository: %s@." name;
    (match url with Some u -> Format.printf "URL: %s@." u | None -> ());
    let upstream = Unpac.Git_backend.upstream_branch name in
    let vendor = Unpac.Git_backend.vendor_branch name in
    let patches = Unpac.Git_backend.patches_branch name in
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git upstream with
     | Some sha -> Format.printf "Upstream: %s@." (String.sub sha 0 7) | None -> ());
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git vendor with
     | Some sha -> Format.printf "Vendor: %s@." (String.sub sha 0 7) | None -> ());
    (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git patches with
     | Some sha -> Format.printf "Patches: %s@." (String.sub sha 0 7) | None -> ());
    let log_output = Unpac.Git.run_exn ~proc_mgr ~cwd:git
      ["log"; "--oneline"; vendor ^ ".." ^ patches] in
    let commits = List.length (String.split_on_char '\n' log_output |>
                               List.filter (fun s -> String.trim s <> "")) in
    Format.printf "Local commits: %d@." commits
  in
  let info = Cmd.info "info" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git diff command *)
let git_diff_cmd =
  let doc = "Show diff between vendor and patches branches." in
  let name_arg =
    let doc = "Repository name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then begin
      Format.eprintf "Repository '%s' is not vendored@." name;
      exit 1
    end;
    let vendor = Unpac.Git_backend.vendor_branch name in
    let patches = Unpac.Git_backend.patches_branch name in
    let diff = Unpac.Git.run_exn ~proc_mgr ~cwd:git ["diff"; vendor; patches] in
    if String.trim diff = "" then
      Format.printf "No local changes@."
    else
      print_string diff
  in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git edit command *)
let git_edit_cmd =
  let doc = "Open a repository's patches worktree for editing." in
  let name_arg =
    let doc = "Repository name to edit." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then begin
      Format.eprintf "Repository '%s' is not vendored@." name;
      exit 1
    end;
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Git_patches name);
    Unpac.Worktree.ensure ~proc_mgr root (Unpac.Worktree.Git_vendor name);
    let patches_path = snd (Unpac.Worktree.path root (Unpac.Worktree.Git_patches name)) in
    let vendor_path = snd (Unpac.Worktree.path root (Unpac.Worktree.Git_vendor name)) in
    Format.printf "Editing %s@.@." name;
    Format.printf "Worktrees created:@.";
    Format.printf "  patches: %s  (make changes here)@." patches_path;
    Format.printf "  vendor:  %s  (original for reference)@." vendor_path;
    Format.printf "@.When done: unpac git done %s@." name
  in
  let info = Cmd.info "edit" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git done command *)
let git_done_cmd =
  let doc = "Close a repository's patches and vendor worktrees." in
  let name_arg =
    let doc = "Repository name." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let patches_kind = Unpac.Worktree.Git_patches name in
    let vendor_kind = Unpac.Worktree.Git_vendor name in
    if not (Unpac.Worktree.exists root patches_kind) then begin
      Format.eprintf "No editing session for '%s'@." name;
      exit 1
    end;
    let wt_path = Unpac.Worktree.path root patches_kind in
    let status = Unpac.Git.run_exn ~proc_mgr ~cwd:wt_path ["status"; "--porcelain"] in
    if String.trim status <> "" then begin
      Format.eprintf "Warning: uncommitted changes in %s@." name;
      Format.eprintf "Commit or discard them before closing.@.";
      exit 1
    end;
    Unpac.Worktree.remove ~proc_mgr root patches_kind;
    if Unpac.Worktree.exists root vendor_kind then
      Unpac.Worktree.remove ~proc_mgr root vendor_kind;
    Format.printf "Closed editing session for %s@." name
  in
  let info = Cmd.info "done" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git remove command *)
let git_remove_cmd =
  let doc = "Remove a vendored git repository." in
  let name_arg =
    let doc = "Repository name to remove." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let run () name =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then begin
      Format.eprintf "Repository '%s' is not vendored@." name;
      exit 1
    end;
    Unpac.Git_backend.remove_repo ~proc_mgr ~root name;
    let config = load_config root in
    let config' = Unpac.Config.remove_git_repo config name in
    save_config ~proc_mgr root config' (Printf.sprintf "Remove git repo %s" name);
    Format.printf "Removed %s@." name
  in
  let info = Cmd.info "remove" ~doc in
  Cmd.v info Term.(const run $ logging_term $ name_arg)

(* Git command group *)
let git_cmd =
  let doc = "Git repository vendoring commands." in
  let man = [
    `S Manpage.s_description;
    `P "Vendor arbitrary git repositories with full history preservation. \
        Uses the three-tier branch model:";
    `I ("git/upstream/<name>", "Tracks the original repository state");
    `I ("git/vendor/<name>", "Clean snapshot used as merge base");
    `I ("git/patches/<name>", "Local modifications on top of vendor");
    `S "REQUIREMENTS";
    `P "git-filter-repo must be installed and in PATH. Install with:";
    `Pre "  curl -o ~/.local/bin/git-filter-repo \\
    https://raw.githubusercontent.com/newren/git-filter-repo/refs/heads/main/git-filter-repo
  chmod +x ~/.local/bin/git-filter-repo";
    `S "TYPICAL WORKFLOW";
    `P "1. Vendor a git repository:";
    `Pre "  unpac git add https://github.com/owner/repo.git";
    `P "2. Optionally extract only a subdirectory:";
    `Pre "  unpac git add https://github.com/owner/monorepo.git --subdir lib/component";
    `P "3. Create a project and merge:";
    `Pre "  unpac project new myapp
  unpac git merge repo myapp";
    `S "MAKING LOCAL CHANGES";
    `P "1. Open repository for editing:";
    `Pre "  unpac git edit repo";
    `P "2. Make changes in vendor/git/repo-patches/";
    `P "3. Close the editing session:";
    `Pre "  unpac git done repo";
    `S "COMMANDS";
  ] in
  let info = Cmd.info "git" ~doc ~man in
  Cmd.group info [
    git_add_cmd; git_list_cmd; git_update_cmd; git_merge_cmd;
    git_info_cmd; git_diff_cmd; git_edit_cmd; git_done_cmd; git_remove_cmd;
  ]

(* Log command *)
let log_cmd =
  let doc = "Show audit log of unpac operations." in
  let man = [
    `S Manpage.s_description;
    `P "Display the audit log of all unpac operations. The log contains \
        hierarchical records including nested git commands.";
    `S Manpage.s_examples;
    `P "View recent operations:";
    `Pre "  unpac log -n 5";
    `P "Export as JSON:";
    `Pre "  unpac log --json > ops.json";
    `P "Generate HTML report:";
    `Pre "  unpac log --html -o report.html";
  ] in
  let json_flag =
    let doc = "Output raw JSON instead of text." in
    Arg.(value & flag & info ["json"] ~doc)
  in
  let html_flag =
    let doc = "Generate HTML report." in
    Arg.(value & flag & info ["html"] ~doc)
  in
  let output_file =
    let doc = "Output file (defaults to stdout)." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)
  in
  let last_n =
    let doc = "Show only the last N operations." in
    Arg.(value & opt (some int) None & info ["n"; "last"] ~docv:"N" ~doc)
  in
  let run () json html output last_n_opt =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr:_ ~root ->
    let log_path = Filename.concat (snd (Unpac.Worktree.path root Unpac.Worktree.Main))
                     Unpac.Audit.default_log_file in
    match Unpac.Audit.load log_path with
    | Error msg ->
        Format.eprintf "Failed to load audit log: %s@." msg;
        exit 1
    | Ok log ->
        let log = match last_n_opt with
          | None -> log
          | Some n ->
              let entries = List.filteri (fun i _ -> i < n) log.entries in
              { log with entries }
        in
        if html then begin
          let html_content = Unpac.Audit.to_html log in
          match output with
          | Some path ->
              let oc = open_out path in
              output_string oc html_content;
              close_out oc;
              Format.printf "HTML report written to %s@." path
          | None -> print_string html_content
        end else if json then begin
          match Jsont_bytesrw.encode_string ~format:Jsont.Indent Unpac.Audit.log_jsont log with
          | Ok s -> print_string s; print_newline ()
          | Error e -> Format.eprintf "Failed to encode JSON: %s@." e; exit 1
        end else begin
          if log.entries = [] then
            Format.printf "No operations recorded.@."
          else
            Format.printf "%a" Unpac.Audit.pp_log log
        end
  in
  let info = Cmd.info "log" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ json_flag $ html_flag $ output_file $ last_n)

(* Push command - push all unpac branches to a remote *)
let push_cmd =
  let doc = "Push all unpac branches to a remote." in
  let remote_arg =
    let doc = "Remote name (e.g., origin)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"REMOTE" ~doc)
  in
  let force_arg =
    let doc = "Force push (use with caution)." in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  in
  let dry_run_arg =
    let doc = "Show what would be pushed without actually pushing." in
    Arg.(value & flag & info ["n"; "dry-run"] ~doc)
  in
  let run () remote force dry_run =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in

    (* Check if remote exists *)
    (match Unpac.Git.remote_url ~proc_mgr ~cwd:git remote with
     | None ->
         Format.eprintf "Remote '%s' not configured.@." remote;
         Format.eprintf "Add it with: git -C %s remote add %s <url>@." (snd git) remote;
         exit 1
     | Some _ -> ());

    (* Get all branches *)
    let all_branches = Unpac.Git.run_lines ~proc_mgr ~cwd:git ["branch"; "--format=%(refname:short)"] in

    (* Filter to only unpac-managed branches *)
    let unpac_branches = List.filter (fun b ->
      b = "main" ||
      String.starts_with ~prefix:"opam/" b ||
      String.starts_with ~prefix:"project/" b
    ) all_branches in

    if unpac_branches = [] then begin
      Format.printf "No branches to push@.";
      exit 0
    end;

    Format.printf "Branches to push to %s:@." remote;
    List.iter (fun b -> Format.printf "  %s@." b) unpac_branches;
    Format.printf "@.";

    if dry_run then begin
      Format.printf "(dry run - no changes made)@."
    end else begin
      (* Build push command *)
      let force_flag = if force then ["--force"] else [] in
      let push_args = ["push"] @ force_flag @ [remote; "--"] @ unpac_branches in

      Format.printf "Pushing %d branches...@." (List.length unpac_branches);
      try
        Unpac.Git.run_exn ~proc_mgr ~cwd:git push_args |> ignore;
        Format.printf "Done.@."
      with e ->
        Format.eprintf "Push failed: %s@." (Printexc.to_string e);
        exit 1
    end
  in
  let info = Cmd.info "push" ~doc in
  Cmd.v info Term.(const run $ logging_term $ remote_arg $ force_arg $ dry_run_arg)

(* Vendor status command *)
let vendor_status_cmd =
  let doc = "Show status of all vendored packages." in
  let run () =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let git = Unpac.Worktree.git_dir root in

    (* Get all vendored packages *)
    let packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    if packages = [] then begin
      Format.printf "No vendored packages.@.";
      exit 0
    end;

    (* Get all project branches *)
    let all_branches = Unpac.Git.run_lines ~proc_mgr ~cwd:git
      ["branch"; "--format=%(refname:short)"] in
    let project_branches = List.filter (fun b ->
      String.starts_with ~prefix:"project/" b
    ) all_branches in
    let project_names = List.map (fun b ->
      String.sub b 8 (String.length b - 8)  (* Remove "project/" prefix *)
    ) project_branches in

    (* Print header *)
    Format.printf "%-25s %8s   %s@." "Package" "Patches" "Merged into";
    Format.printf "%s@." (String.make 70 '-');

    (* For each package, get patch count and merge status *)
    List.iter (fun pkg ->
      let vendor_branch = Unpac_opam.Opam.vendor_branch pkg in
      let patches_branch = Unpac_opam.Opam.patches_branch pkg in

      (* Count commits on patches that aren't on vendor *)
      let patch_count =
        let output = Unpac.Git.run_exn ~proc_mgr ~cwd:git
          ["rev-list"; "--count"; vendor_branch ^ ".." ^ patches_branch] in
        int_of_string (String.trim output)
      in

      (* Check which projects contain this package's patches *)
      let merged_into = List.filter (fun proj_name ->
        let proj_branch = "project/" ^ proj_name in
        (* Check if patches branch is an ancestor of project branch *)
        match Unpac.Git.run ~proc_mgr ~cwd:git
          ["merge-base"; "--is-ancestor"; patches_branch; proj_branch] with
        | Ok _ -> true
        | Error _ -> false
      ) project_names in

      let merged_str = if merged_into = [] then "-"
        else String.concat ", " merged_into in

      Format.printf "%-25s %8d   %s@." pkg patch_count merged_str
    ) packages;

    Format.printf "@.Total: %d packages@." (List.length packages)
  in
  let info = Cmd.info "status" ~doc in
  Cmd.v info Term.(const run $ logging_term)

(* Vendor command group *)
let vendor_cmd =
  let doc = "Vendor status and management commands." in
  let info = Cmd.info "vendor" ~doc in
  Cmd.group info [vendor_status_cmd]

(* Status command - comprehensive workspace status *)
let status_cmd =
  let doc = "Show comprehensive workspace status." in
  let man = [
    `S Manpage.s_description;
    `P "Shows the overall state of the unpac workspace including:";
    `I ("Projects", "All project branches and their merge status");
    `I ("Opam packages", "Vendored packages, patch counts, and merge status");
    `I ("Git repos", "Vendored git repositories and their status");
    `I ("Worktrees", "Any active worktrees with uncommitted changes");
    `P "Also updates README.md in the main branch with status in markdown format.";
    `S Manpage.s_examples;
    `Pre "  unpac status          # Full status
  unpac status --short   # Compact summary";
  ] in
  let short_flag =
    let doc = "Show compact summary only." in
    Arg.(value & flag & info ["s"; "short"] ~doc)
  in
  let no_readme_flag =
    let doc = "Don't update README.md." in
    Arg.(value & flag & info ["no-readme"] ~doc)
  in
  let verbose_flag =
    let doc = "Enable verbose/debug logging to help diagnose issues." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let run () short no_readme verbose =
    setup_logging ~verbose ();
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    Log.debug (fun m -> m "Starting status command...");
    let git = Unpac.Worktree.git_dir root in
    let main_wt = Unpac.Worktree.path root Unpac.Worktree.Main in
    Log.debug (fun m -> m "Git dir: %s" (snd git));
    Log.debug (fun m -> m "Main worktree: %s" (snd main_wt));

    (* Get all branches *)
    Log.debug (fun m -> m "Listing all branches...");
    let all_branches = Unpac.Git.run_lines ~proc_mgr ~cwd:git
      ["branch"; "--format=%(refname:short)"] in
    Log.debug (fun m -> m "Found %d branches" (List.length all_branches));

    (* Categorize branches *)
    Log.debug (fun m -> m "Categorizing branches...");
    let project_branches = List.filter (fun b ->
      String.starts_with ~prefix:"project/" b
    ) all_branches in
    Log.debug (fun m -> m "Found %d project branches" (List.length project_branches));

    Log.debug (fun m -> m "Listing opam packages...");
    let opam_packages = Unpac_opam.Opam.list_packages ~proc_mgr ~root in
    Log.debug (fun m -> m "Found %d opam packages" (List.length opam_packages));

    Log.debug (fun m -> m "Listing git repos...");
    let git_repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    Log.debug (fun m -> m "Found %d git repos" (List.length git_repos));

    (* Parallel map helper using Eio fibers *)
    let parallel_map f items =
      Log.debug (fun m -> m "Running %d operations in parallel..." (List.length items));
      Eio.Switch.run @@ fun sw ->
      let fibers = List.map (fun item ->
        Eio.Fiber.fork_promise ~sw (fun () -> f item)
      ) items in
      List.map Eio.Promise.await_exn fibers
    in

    (* Parallel commit count for all packages *)
    let commit_count_calls = ref 0 in
    let parallel_commit_counts pkgs vendor_fn patches_fn =
      Log.debug (fun m -> m "Counting commits for %d items in parallel..." (List.length pkgs));
      parallel_map (fun pkg ->
        let from_ref = vendor_fn pkg in
        let to_ref = patches_fn pkg in
        incr commit_count_calls;
        try
          let output = Unpac.Git.run_exn ~proc_mgr ~cwd:git
            ["rev-list"; "--count"; from_ref ^ ".." ^ to_ref] in
          (pkg, int_of_string (String.trim output))
        with _ -> (pkg, 0)
      ) pkgs
    in

    (* Helper to check if branch A is ancestor of B - single call *)
    let is_ancestor_calls = ref 0 in
    let is_ancestor a b =
      incr is_ancestor_calls;
      Log.debug (fun m -> m "is_ancestor #%d: %s in %s" !is_ancestor_calls a b);
      match Unpac.Git.run ~proc_mgr ~cwd:git
        ["merge-base"; "--is-ancestor"; a; b] with
      | Ok _ -> true
      | Error _ -> false
    in

    (* Parallel is_ancestor check for a list of (source, target) pairs *)
    let parallel_is_ancestor pairs =
      Log.debug (fun m -> m "Checking %d ancestry relations in parallel..." (List.length pairs));
      parallel_map (fun (a, b) ->
        incr is_ancestor_calls;
        match Unpac.Git.run ~proc_mgr ~cwd:git
          ["merge-base"; "--is-ancestor"; a; b] with
        | Ok _ -> (a, b, true)
        | Error _ -> (a, b, false)
      ) pairs
    in

    (* Check for uncommitted changes in a worktree *)
    let has_changes_calls = ref 0 in
    let has_changes wt_path =
      incr has_changes_calls;
      Log.debug (fun m -> m "has_changes #%d: %s" !has_changes_calls (snd wt_path));
      if Sys.file_exists (snd wt_path) then
        let status = Unpac.Git.run_exn ~proc_mgr ~cwd:wt_path ["status"; "--porcelain"] in
        String.trim status <> ""
      else false
    in

    (* Project names *)
    let project_names = List.map (fun b ->
      String.sub b 8 (String.length b - 8)
    ) project_branches in
    Log.debug (fun m -> m "Project names: %a" Fmt.(list ~sep:comma string) project_names);

    if short then begin
      (* Short summary *)
      Log.debug (fun m -> m "Generating short summary...");
      Format.printf "Workspace: %s@." (snd (Unpac.Worktree.git_dir root) |> Filename.dirname);
      Format.printf "Projects: %d | Opam: %d | Git: %d@."
        (List.length project_branches)
        (List.length opam_packages)
        (List.length git_repos);

      (* Count total patches - parallel *)
      Log.debug (fun m -> m "Counting opam patches (%d packages) in parallel..." (List.length opam_packages));
      let opam_patch_counts = parallel_commit_counts opam_packages
        Unpac_opam.Opam.vendor_branch Unpac_opam.Opam.patches_branch in
      let opam_patches = List.fold_left (fun acc (_, n) -> acc + n) 0 opam_patch_counts in
      Log.debug (fun m -> m "Counting git patches (%d repos) in parallel..." (List.length git_repos));
      let git_patch_counts = parallel_commit_counts git_repos
        Unpac.Git_backend.vendor_branch Unpac.Git_backend.patches_branch in
      let git_patches = List.fold_left (fun acc (_, n) -> acc + n) 0 git_patch_counts in
      if opam_patches + git_patches > 0 then
        Format.printf "Local patches: %d commits@." (opam_patches + git_patches);

      (* Check main for uncommitted *)
      Log.debug (fun m -> m "Checking main worktree for changes...");
      if has_changes main_wt then
        Format.printf "Warning: Uncommitted changes in main@.";
      Log.debug (fun m -> m "Short summary complete.")
    end else begin
      (* Full status *)
      Log.debug (fun m -> m "Generating full status...");
      Format.printf "=== Unpac Workspace Status ===@.@.";

      (* Main worktree status *)
      Log.debug (fun m -> m "Checking main worktree status...");
      Format.printf "Main worktree: %s@." (snd main_wt);
      if has_changes main_wt then
        Format.printf "  @{<yellow>Warning: Uncommitted changes@}@."
      else
        Format.printf "  Clean@.";
      Format.printf "@.";

      (* Projects - precompute all ancestry relationships in parallel *)
      Log.debug (fun m -> m "Processing %d projects..." (List.length project_names));
      Format.printf "=== Projects (%d) ===@." (List.length project_names);
      if project_names = [] then
        Format.printf "  (none)@."
      else begin
        (* Build all ancestry pairs to check: (patches_branch, project_branch) *)
        let opam_pairs = List.concat_map (fun proj ->
          let proj_branch = "project/" ^ proj in
          List.map (fun pkg ->
            (Unpac_opam.Opam.patches_branch pkg, proj_branch)
          ) opam_packages
        ) project_names in
        let git_pairs = List.concat_map (fun proj ->
          let proj_branch = "project/" ^ proj in
          List.map (fun repo ->
            (Unpac.Git_backend.patches_branch repo, proj_branch)
          ) git_repos
        ) project_names in

        (* Check all ancestry in parallel *)
        Log.debug (fun m -> m "Checking %d ancestry relations in parallel..."
          (List.length opam_pairs + List.length git_pairs));
        let all_pairs = opam_pairs @ git_pairs in
        let ancestry_results = parallel_is_ancestor all_pairs in

        (* Build lookup table: (patches_branch, project_branch) -> is_ancestor *)
        let ancestry_table = Hashtbl.create (List.length ancestry_results) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add ancestry_table (a, b) result
        ) ancestry_results;

        let is_ancestor_cached a b =
          try Hashtbl.find ancestry_table (a, b)
          with Not_found -> is_ancestor a b (* fallback *)
        in

        List.iteri (fun i proj ->
          Log.debug (fun m -> m "  Project %d/%d: %s" (i+1) (List.length project_names) proj);
          let proj_branch = "project/" ^ proj in
          let proj_wt = Unpac.Worktree.path root (Unpac.Worktree.Project proj) in
          let wt_exists = Sys.file_exists (snd proj_wt) in
          let dirty = wt_exists && has_changes proj_wt in

          (* Count merged packages - use cached results *)
          let merged_opam = List.filter (fun pkg ->
            is_ancestor_cached (Unpac_opam.Opam.patches_branch pkg) proj_branch
          ) opam_packages in
          let merged_git = List.filter (fun repo ->
            is_ancestor_cached (Unpac.Git_backend.patches_branch repo) proj_branch
          ) git_repos in

          Format.printf "  %s" proj;
          if wt_exists then Format.printf " [worktree]";
          if dirty then Format.printf " @{<yellow>*dirty*@}";
          Format.printf "@.";
          Format.printf "    Merged: %d opam, %d git@."
            (List.length merged_opam) (List.length merged_git)
        ) project_names
      end;
      Format.printf "@.";

      (* Opam packages - parallel commit counts and cached ancestry *)
      Log.debug (fun m -> m "Processing %d opam packages..." (List.length opam_packages));
      Format.printf "=== Opam Packages (%d) ===@." (List.length opam_packages);
      if opam_packages = [] then
        Format.printf "  (none)@."
      else begin
        (* Parallel commit counts *)
        let opam_counts = parallel_commit_counts opam_packages
          Unpac_opam.Opam.vendor_branch Unpac_opam.Opam.patches_branch in
        let opam_count_table = Hashtbl.create (List.length opam_counts) in
        List.iter (fun (pkg, count) -> Hashtbl.add opam_count_table pkg count) opam_counts;

        (* Build ancestry pairs for opam -> projects (reuse if possible) *)
        let opam_to_proj_pairs = List.concat_map (fun pkg ->
          let patches_branch = Unpac_opam.Opam.patches_branch pkg in
          List.map (fun proj -> (patches_branch, "project/" ^ proj)) project_names
        ) opam_packages in
        let opam_ancestry_results = parallel_is_ancestor opam_to_proj_pairs in
        let opam_ancestry_table = Hashtbl.create (List.length opam_ancestry_results) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add opam_ancestry_table (a, b) result
        ) opam_ancestry_results;

        Format.printf "  %-25s %8s   %s@." "Package" "Patches" "Merged into";
        Format.printf "  %s@." (String.make 60 '-');
        List.iteri (fun i pkg ->
          Log.debug (fun m -> m "  Opam package %d/%d: %s" (i+1) (List.length opam_packages) pkg);
          let patches_branch = Unpac_opam.Opam.patches_branch pkg in
          let patch_count = try Hashtbl.find opam_count_table pkg with Not_found -> 0 in

          (* Check active worktrees *)
          let patches_wt = Unpac.Worktree.path root (Unpac.Worktree.Opam_patches pkg) in
          let has_wt = Sys.file_exists (snd patches_wt) in
          let dirty = has_wt && has_changes patches_wt in

          (* Check merged into which projects - use cached results *)
          let merged_into = List.filter (fun proj ->
            try Hashtbl.find opam_ancestry_table (patches_branch, "project/" ^ proj)
            with Not_found -> false
          ) project_names in

          let merged_str = if merged_into = [] then "-"
            else String.concat ", " merged_into in

          Format.printf "  %-25s" pkg;
          if has_wt then Format.printf "*" else Format.printf " ";
          Format.printf "%7d   %s" patch_count merged_str;
          if dirty then Format.printf " @{<yellow>(uncommitted)@}";
          Format.printf "@."
        ) opam_packages
      end;
      Format.printf "@.";

      (* Git repos - parallel commit counts and cached ancestry *)
      Log.debug (fun m -> m "Processing %d git repos..." (List.length git_repos));
      Format.printf "=== Git Repositories (%d) ===@." (List.length git_repos);
      if git_repos = [] then
        Format.printf "  (none)@."
      else begin
        (* Parallel commit counts *)
        let git_counts = parallel_commit_counts git_repos
          Unpac.Git_backend.vendor_branch Unpac.Git_backend.patches_branch in
        let git_count_table = Hashtbl.create (List.length git_counts) in
        List.iter (fun (repo, count) -> Hashtbl.add git_count_table repo count) git_counts;

        (* Build ancestry pairs for git -> projects *)
        let git_to_proj_pairs = List.concat_map (fun repo ->
          let patches_branch = Unpac.Git_backend.patches_branch repo in
          List.map (fun proj -> (patches_branch, "project/" ^ proj)) project_names
        ) git_repos in
        let git_ancestry_results = parallel_is_ancestor git_to_proj_pairs in
        let git_ancestry_table = Hashtbl.create (List.length git_ancestry_results) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add git_ancestry_table (a, b) result
        ) git_ancestry_results;

        Format.printf "  %-25s %8s   %s@." "Repository" "Patches" "Merged into";
        Format.printf "  %s@." (String.make 60 '-');
        List.iteri (fun i repo ->
          Log.debug (fun m -> m "  Git repo %d/%d: %s" (i+1) (List.length git_repos) repo);
          let patches_branch = Unpac.Git_backend.patches_branch repo in
          let patch_count = try Hashtbl.find git_count_table repo with Not_found -> 0 in

          let patches_wt = Unpac.Worktree.path root (Unpac.Worktree.Git_patches repo) in
          let has_wt = Sys.file_exists (snd patches_wt) in
          let dirty = has_wt && has_changes patches_wt in

          let merged_into = List.filter (fun proj ->
            try Hashtbl.find git_ancestry_table (patches_branch, "project/" ^ proj)
            with Not_found -> false
          ) project_names in

          let merged_str = if merged_into = [] then "-"
            else String.concat ", " merged_into in

          Format.printf "  %-25s" repo;
          if has_wt then Format.printf "*" else Format.printf " ";
          Format.printf "%7d   %s" patch_count merged_str;
          if dirty then Format.printf " @{<yellow>(uncommitted)@}";
          Format.printf "@."
        ) git_repos
      end;
      Format.printf "@.";

      (* Active worktrees summary *)
      let active_worktrees = ref [] in
      List.iter (fun pkg ->
        let wt = Unpac.Worktree.path root (Unpac.Worktree.Opam_patches pkg) in
        if Sys.file_exists (snd wt) then
          active_worktrees := ("opam/" ^ pkg ^ "-patches", has_changes wt) :: !active_worktrees
      ) opam_packages;
      List.iter (fun repo ->
        let wt = Unpac.Worktree.path root (Unpac.Worktree.Git_patches repo) in
        if Sys.file_exists (snd wt) then
          active_worktrees := ("git/" ^ repo ^ "-patches", has_changes wt) :: !active_worktrees
      ) git_repos;

      if !active_worktrees <> [] then begin
        Format.printf "=== Active Worktrees ===@.";
        List.iter (fun (name, dirty) ->
          Format.printf "  %s" name;
          if dirty then Format.printf " @{<yellow>*uncommitted*@}";
          Format.printf "@."
        ) (List.rev !active_worktrees);
        Format.printf "@."
      end;

      (* Legend *)
      Format.printf "Legend: * = worktree active@.";
      Log.debug (fun m -> m "Full status output complete.")
    end;

    (* Generate README.md unless --no-readme *)
    if not no_readme then begin
      Log.debug (fun m -> m "Generating README.md...");
      let buf = Buffer.create 4096 in
      let add = Buffer.add_string buf in
      let addf fmt = Printf.ksprintf add fmt in
      let timestamp =
        let tm = Unix.localtime (Unix.time ()) in
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
          (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      in

      (* Get tangled.org base URL from origin remote *)
      Log.debug (fun m -> m "Getting origin remote URL...");
      let tangled_base =
        match Unpac.Git.remote_url ~proc_mgr ~cwd:git "origin" with
        | None ->
            Log.debug (fun m -> m "No origin remote found");
            None
        | Some url ->
            (* Parse git@git.recoil.org:user/repo or similar *)
            let url = String.trim url in
            (* Handle git@host:user/repo format *)
            if String.starts_with ~prefix:"git@" url then
              match String.index_opt url ':' with
              | None -> None
              | Some colon_pos ->
                  let path = String.sub url (colon_pos + 1) (String.length url - colon_pos - 1) in
                  (* Strip .git suffix if present *)
                  let path = if String.ends_with ~suffix:".git" path then
                    String.sub path 0 (String.length path - 4) else path in
                  Some (Printf.sprintf "https://tangled.org/%s" path)
            else None
      in

      (* URL encode a branch name for tree URLs *)
      let url_encode s =
        let buf = Buffer.create (String.length s * 2) in
        String.iter (fun c ->
          match c with
          | '/' -> Buffer.add_string buf "%2F"
          | ' ' -> Buffer.add_string buf "%20"
          | c -> Buffer.add_char buf c
        ) s;
        Buffer.contents buf
      in

      (* Create a markdown link to a branch tree, or just the name if no base URL *)
      let branch_link name branch =
        match tangled_base with
        | None -> name
        | Some base -> Printf.sprintf "[%s](%s/tree/%s)" name base (url_encode branch)
      in

      add "# Unpac Workspace Status\n\n";
      addf "_Last updated: %s_\n\n" timestamp;

      (* Summary *)
      add "## Summary\n\n";
      addf "| Category | Count |\n";
      addf "|----------|-------|\n";
      addf "| Projects | %d |\n" (List.length project_names);
      addf "| Opam Packages | %d |\n" (List.length opam_packages);
      addf "| Git Repositories | %d |\n\n" (List.length git_repos);

      (* Projects section - parallel ancestry checks *)
      Log.debug (fun m -> m "README: Processing %d projects..." (List.length project_names));
      add "## Projects\n\n";
      if project_names = [] then
        add "_No projects created yet._\n\n"
      else begin
        (* Precompute all ancestry in parallel for README *)
        let readme_opam_pairs = List.concat_map (fun proj ->
          let proj_branch = "project/" ^ proj in
          List.map (fun pkg ->
            (Unpac_opam.Opam.patches_branch pkg, proj_branch)
          ) opam_packages
        ) project_names in
        let readme_git_pairs = List.concat_map (fun proj ->
          let proj_branch = "project/" ^ proj in
          List.map (fun repo ->
            (Unpac.Git_backend.patches_branch repo, proj_branch)
          ) git_repos
        ) project_names in
        Log.debug (fun m -> m "README: Checking %d ancestry relations in parallel..."
          (List.length readme_opam_pairs + List.length readme_git_pairs));
        let readme_ancestry_results = parallel_is_ancestor (readme_opam_pairs @ readme_git_pairs) in
        let readme_ancestry_table = Hashtbl.create (List.length readme_ancestry_results) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add readme_ancestry_table (a, b) result
        ) readme_ancestry_results;
        let readme_is_ancestor a b =
          try Hashtbl.find readme_ancestry_table (a, b)
          with Not_found -> false
        in

        add "| Project | Opam Merged | Git Merged | Status |\n";
        add "|---------|-------------|------------|--------|\n";
        List.iteri (fun i proj ->
          Log.debug (fun m -> m "README: Project %d/%d: %s" (i+1) (List.length project_names) proj);
          let proj_branch = "project/" ^ proj in
          let proj_wt = Unpac.Worktree.path root (Unpac.Worktree.Project proj) in
          let wt_exists = Sys.file_exists (snd proj_wt) in
          let dirty = wt_exists && has_changes proj_wt in

          let merged_opam = List.filter (fun pkg ->
            readme_is_ancestor (Unpac_opam.Opam.patches_branch pkg) proj_branch
          ) opam_packages in
          let merged_git = List.filter (fun repo ->
            readme_is_ancestor (Unpac.Git_backend.patches_branch repo) proj_branch
          ) git_repos in

          let status =
            if dirty then "⚠️ uncommitted"
            else if wt_exists then "📂 worktree active"
            else "✓" in
          addf "| %s | %d | %d | %s |\n"
            (branch_link proj proj_branch) (List.length merged_opam) (List.length merged_git) status
        ) project_names;
        add "\n"
      end;

      (* Opam packages section - parallel *)
      Log.debug (fun m -> m "README: Processing %d opam packages..." (List.length opam_packages));
      add "## Opam Packages\n\n";
      if opam_packages = [] then
        add "_No opam packages vendored yet._\n\n"
      else begin
        (* Parallel commit counts *)
        let readme_opam_counts = parallel_commit_counts opam_packages
          Unpac_opam.Opam.vendor_branch Unpac_opam.Opam.patches_branch in
        let readme_opam_count_table = Hashtbl.create (List.length readme_opam_counts) in
        List.iter (fun (pkg, count) -> Hashtbl.add readme_opam_count_table pkg count) readme_opam_counts;

        (* Parallel ancestry for opam -> projects *)
        let readme_opam_to_proj = List.concat_map (fun pkg ->
          let patches_branch = Unpac_opam.Opam.patches_branch pkg in
          List.map (fun proj -> (patches_branch, "project/" ^ proj)) project_names
        ) opam_packages in
        let readme_opam_ancestry = parallel_is_ancestor readme_opam_to_proj in
        let readme_opam_anc_table = Hashtbl.create (List.length readme_opam_ancestry) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add readme_opam_anc_table (a, b) result
        ) readme_opam_ancestry;

        add "| Package | Patches | Merged Into | Status |\n";
        add "|---------|---------|-------------|--------|\n";
        List.iteri (fun i pkg ->
          Log.debug (fun m -> m "README: Opam package %d/%d: %s" (i+1) (List.length opam_packages) pkg);
          let patches_branch = Unpac_opam.Opam.patches_branch pkg in
          let patch_count = try Hashtbl.find readme_opam_count_table pkg with Not_found -> 0 in

          let patches_wt = Unpac.Worktree.path root (Unpac.Worktree.Opam_patches pkg) in
          let has_wt = Sys.file_exists (snd patches_wt) in
          let dirty = has_wt && has_changes patches_wt in

          let merged_into = List.filter (fun proj ->
            try Hashtbl.find readme_opam_anc_table (patches_branch, "project/" ^ proj)
            with Not_found -> false
          ) project_names in

          let merged_str = if merged_into = [] then "-"
            else String.concat ", " (List.map (fun p -> branch_link p ("project/" ^ p)) merged_into) in

          let status =
            if dirty then "⚠️ uncommitted"
            else if has_wt then "📂 editing"
            else "✓" in

          addf "| %s | %d | %s | %s |\n" (branch_link pkg patches_branch) patch_count merged_str status
        ) opam_packages;
        add "\n"
      end;

      (* Git repositories section - parallel *)
      Log.debug (fun m -> m "README: Processing %d git repos..." (List.length git_repos));
      add "## Git Repositories\n\n";
      if git_repos = [] then
        add "_No git repositories vendored yet._\n\n"
      else begin
        (* Parallel commit counts *)
        let readme_git_counts = parallel_commit_counts git_repos
          Unpac.Git_backend.vendor_branch Unpac.Git_backend.patches_branch in
        let readme_git_count_table = Hashtbl.create (List.length readme_git_counts) in
        List.iter (fun (repo, count) -> Hashtbl.add readme_git_count_table repo count) readme_git_counts;

        (* Parallel ancestry for git -> projects *)
        let readme_git_to_proj = List.concat_map (fun repo ->
          let patches_branch = Unpac.Git_backend.patches_branch repo in
          List.map (fun proj -> (patches_branch, "project/" ^ proj)) project_names
        ) git_repos in
        let readme_git_ancestry = parallel_is_ancestor readme_git_to_proj in
        let readme_git_anc_table = Hashtbl.create (List.length readme_git_ancestry) in
        List.iter (fun (a, b, result) ->
          Hashtbl.add readme_git_anc_table (a, b) result
        ) readme_git_ancestry;

        add "| Repository | Patches | Merged Into | Status |\n";
        add "|------------|---------|-------------|--------|\n";
        List.iteri (fun i repo ->
          Log.debug (fun m -> m "README: Git repo %d/%d: %s" (i+1) (List.length git_repos) repo);
          let patches_branch = Unpac.Git_backend.patches_branch repo in
          let patch_count = try Hashtbl.find readme_git_count_table repo with Not_found -> 0 in

          let patches_wt = Unpac.Worktree.path root (Unpac.Worktree.Git_patches repo) in
          let has_wt = Sys.file_exists (snd patches_wt) in
          let dirty = has_wt && has_changes patches_wt in

          let merged_into = List.filter (fun proj ->
            try Hashtbl.find readme_git_anc_table (patches_branch, "project/" ^ proj)
            with Not_found -> false
          ) project_names in

          let merged_str = if merged_into = [] then "-"
            else String.concat ", " (List.map (fun p -> branch_link p ("project/" ^ p)) merged_into) in

          let status =
            if dirty then "⚠️ uncommitted"
            else if has_wt then "📂 editing"
            else "✓" in

          addf "| %s | %d | %s | %s |\n" (branch_link repo patches_branch) patch_count merged_str status
        ) git_repos;
        add "\n"
      end;

      (* Active worktrees *)
      let active_wts = ref [] in
      List.iter (fun pkg ->
        let wt = Unpac.Worktree.path root (Unpac.Worktree.Opam_patches pkg) in
        if Sys.file_exists (snd wt) then
          active_wts := (Printf.sprintf "vendor/opam/%s-patches" pkg, has_changes wt) :: !active_wts
      ) opam_packages;
      List.iter (fun repo ->
        let wt = Unpac.Worktree.path root (Unpac.Worktree.Git_patches repo) in
        if Sys.file_exists (snd wt) then
          active_wts := (Printf.sprintf "vendor/git/%s-patches" repo, has_changes wt) :: !active_wts
      ) git_repos;

      if !active_wts <> [] then begin
        add "## Active Worktrees\n\n";
        add "| Path | Status |\n";
        add "|------|--------|\n";
        List.iter (fun (name, dirty) ->
          let status = if dirty then "⚠️ uncommitted changes" else "✓ clean" in
          addf "| `%s` | %s |\n" name status
        ) (List.rev !active_wts);
        add "\n"
      end;

      (* Changes section from audit log *)
      Log.debug (fun m -> m "README: Generating Changes section from audit log...");
      let audit_path = Eio.Path.(main_wt / ".unpac-audit.json") |> snd in
      (match Unpac.Audit.load audit_path with
       | Error e ->
           Log.debug (fun m -> m "README: Could not load audit log: %s" e)
       | Ok audit_log ->
           (* Filter to significant events and take most recent *)
           let significant_ops = List.filter (fun (op : Unpac.Audit.operation) ->
             match op.operation_type with
             | Unpac.Audit.Project_new
             | Unpac.Audit.Project_promote
             | Unpac.Audit.Opam_add
             | Unpac.Audit.Git_add
             | Unpac.Audit.Init -> true
             | _ -> false
           ) audit_log.entries in
           (* Take most recent 20 events, reverse to show oldest first *)
           let recent_ops =
             significant_ops
             |> (fun l -> if List.length l > 20 then
                   List.filteri (fun i _ -> i < 20) l
                 else l)
             |> List.rev
           in
           if recent_ops <> [] then begin
             add "## Changes\n\n";
             add "| Date | Event | Details |\n";
             add "|------|-------|--------|\n";
             List.iter (fun (op : Unpac.Audit.operation) ->
               let tm = Unix.localtime op.timestamp in
               let date = Printf.sprintf "%04d-%02d-%02d"
                 (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday in
               let (event, details) = match op.operation_type, op.args with
                 | Unpac.Audit.Init, _ ->
                     ("Workspace initialized", "")
                 | Unpac.Audit.Project_new, name :: _ ->
                     ("Project created", Printf.sprintf "`%s`" name)
                 | Unpac.Audit.Project_promote, name :: _ ->
                     let backend = List.find_map (fun arg ->
                       if String.starts_with ~prefix:"--backend" arg then None
                       else match List.nth_opt op.args (1 + (List.length (List.filter ((=) arg) (List.filteri (fun i _ -> i = 0) op.args)))) with
                       | _ -> None
                     ) op.args in
                     let backend_str = match backend with Some b -> b | None ->
                       (* Try to find backend in args *)
                       let rec find_backend = function
                         | "--backend" :: b :: _ -> b
                         | "-b" :: b :: _ -> b
                         | _ :: rest -> find_backend rest
                         | [] -> "opam"
                       in find_backend op.args
                     in
                     ("Project promoted", Printf.sprintf "`%s` → %s vendor" name backend_str)
                 | Unpac.Audit.Opam_add, pkgs ->
                     let pkg_list = String.concat ", " (List.map (fun p -> Printf.sprintf "`%s`" p) pkgs) in
                     ("Opam packages added", pkg_list)
                 | Unpac.Audit.Git_add, name :: _ ->
                     ("Git repo added", Printf.sprintf "`%s`" name)
                 | _, args ->
                     (Unpac.Audit.operation_type_to_string op.operation_type,
                      String.concat " " args)
               in
               addf "| %s | %s | %s |\n" date event details
             ) recent_ops;
             add "\n"
           end);

      (* Footer *)
      add "---\n\n";
      add "_Generated by `unpac status`_\n";

      (* Write README.md *)
      Log.debug (fun m -> m "README: Checking if README.md needs update...");
      let readme_path = Filename.concat (snd main_wt) "README.md" in
      let content = Buffer.contents buf in

      (* Check if content changed *)
      let old_content =
        if Sys.file_exists readme_path then begin
          Log.debug (fun m -> m "README: Reading existing README.md...");
          let ic = open_in readme_path in
          let len = in_channel_length ic in
          let s = really_input_string ic len in
          close_in ic;
          Some s
        end else begin
          Log.debug (fun m -> m "README: No existing README.md");
          None
        end
      in

      (* Only write and commit if changed (ignoring timestamp line) *)
      let content_without_timestamp s =
        (* Remove the timestamp line for comparison *)
        Str.global_replace (Str.regexp "_Last updated:.*_") "" s
      in
      let changed = match old_content with
        | None -> true
        | Some old -> content_without_timestamp old <> content_without_timestamp content
      in

      if changed then begin
        Log.debug (fun m -> m "README: Writing updated README.md...");
        let oc = open_out readme_path in
        output_string oc content;
        close_out oc;
        Format.printf "@.README.md updated.@.";
        (* Git add and commit *)
        Log.debug (fun m -> m "README: Staging README.md...");
        Unpac.Git.run_exn ~proc_mgr ~cwd:main_wt ["add"; "README.md"] |> ignore;
        (try
          Log.debug (fun m -> m "README: Committing README.md...");
          Unpac.Git.run_exn ~proc_mgr ~cwd:main_wt
            ["commit"; "-m"; "Update workspace status in README.md"] |> ignore;
          Format.printf "Committed README.md changes.@."
        with _ ->
          (* Commit might fail if nothing staged (e.g., only timestamp changed) *)
          Log.debug (fun m -> m "README: Commit failed (likely nothing to commit)"))
      end else
        Log.debug (fun m -> m "README: No changes, skipping write");
      Log.debug (fun m -> m "Status command complete.")
    end
  in
  let info = Cmd.info "status" ~doc ~man in
  Cmd.v info Term.(const run $ const () $ short_flag $ no_readme_flag $ verbose_flag)

(* Monorepo export command *)
let monorepo_cmd =
  let doc = "Export a standalone buildable monorepo." in
  let man = [
    `S Manpage.s_description;
    `P "Creates a standalone directory containing all projects and their \
        vendored dependencies, suitable for building with dune. No git history \
        is included - only the current state of each branch.";
    `S "OUTPUT STRUCTURE";
    `Pre "  output/
  ├── dune-project
  ├── dune
  ├── project1/
  │   ├── src/
  │   └── dune
  ├── project2/
  │   └── ...
  └── vendor/
      ├── opam/
      │   ├── pkg1/
      │   └── pkg2/
      └── git/
          └── repo1/";
    `S Manpage.s_examples;
    `P "Export all projects:";
    `Pre "  unpac monorepo /path/to/output";
    `P "Export specific projects:";
    `Pre "  unpac monorepo -p myapp -p mylib /path/to/output";
    `P "Export without opam packages:";
    `Pre "  unpac monorepo --no-opam /path/to/output";
  ] in
  let output_arg =
    let doc = "Output directory for the monorepo." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)
  in
  let projects_arg =
    let doc = "Specific projects to include (can be repeated). Default: all projects." in
    Arg.(value & opt_all string [] & info ["p"; "project"] ~docv:"NAME" ~doc)
  in
  let no_opam_arg =
    let doc = "Exclude vendored opam packages." in
    Arg.(value & flag & info ["no-opam"] ~doc)
  in
  let no_git_arg =
    let doc = "Exclude vendored git repositories." in
    Arg.(value & flag & info ["no-git"] ~doc)
  in
  let run () output_dir projects no_opam no_git =
    with_root @@ fun ~env:_ ~fs:_ ~proc_mgr ~root ->
    let config : Unpac.Monorepo.export_config = {
      output_dir;
      projects = if projects = [] then None else Some projects;
      include_opam = not no_opam;
      include_git = not no_git;
    } in
    let result = Unpac.Monorepo.export ~proc_mgr ~root ~config in
    Format.printf "@.Monorepo exported to %s@." result.output_path;
    Format.printf "@.Contents:@.";
    Format.printf "  Projects: %s@." (String.concat ", " result.projects_exported);
    if result.opam_packages <> [] then
      Format.printf "  Opam packages: %d@." (List.length result.opam_packages);
    if result.git_repos <> [] then
      Format.printf "  Git repos: %d@." (List.length result.git_repos);
    Format.printf "@.Build with:@.";
    Format.printf "  cd %s && dune build@." output_dir;
    Format.printf "  cd %s && dune build @doc@." output_dir
  in
  let info = Cmd.info "monorepo" ~doc ~man in
  Cmd.v info Term.(const run $ logging_term $ output_arg $ projects_arg $ no_opam_arg $ no_git_arg)

(* Main command *)
let main_cmd =
  let doc = "Multi-backend vendoring tool using git worktrees." in
  let man = [
    `S Manpage.s_description;
    `P "Unpac is a vendoring tool that maintains third-party dependencies \
        as git branches with full history. It uses git worktrees to provide \
        isolated views for editing, and a three-tier branch model \
        (upstream/vendor/patches) for conflict-free updates.";
    `S "VENDORING MODES";
    `I ("unpac opam", "Vendor OCaml packages from opam repositories with \
                       dependency solving.");
    `I ("unpac git", "Vendor arbitrary git repositories directly by URL.");
    `S "THREE-TIER BRANCH MODEL";
    `P "Each vendored item has three branches:";
    `I ("upstream/*", "Tracks the original repository");
    `I ("vendor/*", "Clean snapshot used as merge base");
    `I ("patches/*", "Your local modifications");
    `S "QUICK START";
    `Pre "  unpac init myproject && cd myproject
  unpac opam repo add default /path/to/opam-repository
  unpac opam config compiler 5.2.0
  unpac project new main
  unpac opam add mylib --solve
  unpac opam merge --all main";
    `S "COMMANDS";
  ] in
  let info = Cmd.info "unpac" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [init_cmd; status_cmd; project_cmd; opam_cmd; git_cmd; vendor_cmd; push_cmd; log_cmd;
                  export_cmd; export_set_remote_cmd; export_push_cmd; export_list_cmd; monorepo_cmd]

let () = exit (Cmd.eval main_cmd)
