(** Git backend for direct repository vendoring.

    Implements vendoring of arbitrary git repositories using the three-tier branch model:
    - git/upstream/<name> - pristine upstream code
    - git/vendor/<name> - upstream history rewritten with vendor/git/<name>/ prefix
    - git/patches/<name> - local modifications *)

(** {1 Branch Naming} *)

let upstream_branch name = "git/upstream/" ^ name
let vendor_branch name = "git/vendor/" ^ name
let patches_branch name = "git/patches/" ^ name
let vendor_path name = "vendor/git/" ^ name

(** {1 Worktree Kinds} *)

let upstream_kind name = Worktree.Git_upstream name
let vendor_kind name = Worktree.Git_vendor name
let patches_kind name = Worktree.Git_patches name

(** {1 Repository Info} *)

type repo_info = {
  name : string;
  url : string;
  branch : string option;
  subdir : string option;
}

(** {1 Repository Operations} *)

let add_repo ~proc_mgr ~root ?cache info =
  let repo_name = info.name in
  let git = Worktree.git_dir root in

  try
    (* Check if already exists *)
    if Worktree.branch_exists ~proc_mgr root (patches_kind repo_name) then
      Backend.Already_exists repo_name
    else begin
      (* Rewrite URL for known mirrors *)
      let url = Git_repo_lookup.rewrite_url info.url in

      (* Determine the ref to use: explicit > override > default *)
      let branch = match info.branch with
        | Some b -> b
        | None ->
            match Git_repo_lookup.branch_override ~name:repo_name ~url with
            | Some b -> b
            | None -> Git.ls_remote_default_branch ~proc_mgr ~cwd:git ~url
      in

      (* Fetch - either via cache or directly *)
      let ref_point = match cache with
        | Some cache_path ->
            (* Fetch through vendor cache *)
            Vendor_cache.fetch_to_project ~proc_mgr
              ~cache:cache_path ~project_git:git ~url ~branch
        | None ->
            (* Direct fetch (with tags to support version tags) *)
            let remote = "origin-" ^ repo_name in
            ignore (Git.ensure_remote ~proc_mgr ~cwd:git ~name:remote ~url);
            Git.fetch_with_tags ~proc_mgr ~cwd:git ~remote;
            Git.resolve_branch_or_tag ~proc_mgr ~cwd:git ~remote ~ref_name:branch
      in

      (* Step 1: Create upstream branch from fetched ref *)
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(upstream_branch repo_name) ~point:ref_point;

      (* Step 2: Create vendor branch from upstream and rewrite history *)
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(vendor_branch repo_name) ~point:(upstream_branch repo_name);

      (* If subdir is specified, we first filter to that subdirectory,
         then move to vendor path. Otherwise, just move to vendor path. *)
      (match info.subdir with
       | Some subdir ->
           (* First filter to extract only the subdirectory *)
           Git.filter_repo_to_subdirectory ~proc_mgr ~cwd:git
             ~branch:(vendor_branch repo_name)
             ~subdirectory:subdir;
           (* Now the subdir is at root, rewrite to vendor path *)
           Git.filter_repo_to_subdirectory ~proc_mgr ~cwd:git
             ~branch:(vendor_branch repo_name)
             ~subdirectory:(vendor_path repo_name)
       | None ->
           (* Rewrite vendor branch history to move all files into vendor/git/<name>/ *)
           Git.filter_repo_to_subdirectory ~proc_mgr ~cwd:git
             ~branch:(vendor_branch repo_name)
             ~subdirectory:(vendor_path repo_name));

      (* Get the vendor SHA after rewriting *)
      let vendor_sha = match Git.rev_parse ~proc_mgr ~cwd:git (vendor_branch repo_name) with
        | Some sha -> sha
        | None -> failwith "Vendor branch not found after filter-repo"
      in

      (* Step 3: Create patches branch from vendor *)
      Git.branch_create ~proc_mgr ~cwd:git
        ~name:(patches_branch repo_name)
        ~start_point:(vendor_branch repo_name);

      Backend.Added { name = repo_name; sha = vendor_sha }
    end
  with exn ->
    (* Cleanup on failure *)
    (try Worktree.remove_force ~proc_mgr root (upstream_kind repo_name) with _ -> ());
    (try Worktree.remove_force ~proc_mgr root (vendor_kind repo_name) with _ -> ());
    Backend.Failed { name = repo_name; error = Printexc.to_string exn }

let copy_with_prefix ~src_dir ~dst_dir ~prefix =
  (* Recursively copy files from src_dir to dst_dir/prefix/ *)
  let prefix_dir = Eio.Path.(dst_dir / prefix) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 prefix_dir;

  let rec copy_dir src dst =
    Eio.Path.read_dir src |> List.iter (fun name ->
      let src_path = Eio.Path.(src / name) in
      let dst_path = Eio.Path.(dst / name) in
      if Eio.Path.is_directory src_path then begin
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst_path;
        copy_dir src_path dst_path
      end else begin
        let content = Eio.Path.load src_path in
        Eio.Path.save ~create:(`Or_truncate 0o644) dst_path content
      end
    )
  in

  (* Copy everything except .git *)
  Eio.Path.read_dir src_dir |> List.iter (fun name ->
    if name <> ".git" then begin
      let src_path = Eio.Path.(src_dir / name) in
      let dst_path = Eio.Path.(prefix_dir / name) in
      if Eio.Path.is_directory src_path then begin
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst_path;
        copy_dir src_path dst_path
      end else begin
        let content = Eio.Path.load src_path in
        Eio.Path.save ~create:(`Or_truncate 0o644) dst_path content
      end
    end
  )

let update_repo ~proc_mgr ~root ?cache repo_name =
  let git = Worktree.git_dir root in

  try
    (* Check if repo exists *)
    if not (Worktree.branch_exists ~proc_mgr root (patches_kind repo_name)) then
      Backend.Update_failed { name = repo_name; error = "Repository not vendored" }
    else begin
      (* Get remote URL *)
      let remote = "origin-" ^ repo_name in
      let url = match Git.remote_url ~proc_mgr ~cwd:git remote with
        | Some u -> u
        | None -> failwith ("Remote not found: " ^ remote)
      in

      (* Fetch latest - either via cache or directly (with tags for completeness) *)
      (match cache with
       | Some cache_path ->
           let branch = Git.ls_remote_default_branch ~proc_mgr ~cwd:git ~url in
           ignore (Vendor_cache.fetch_to_project ~proc_mgr
             ~cache:cache_path ~project_git:git ~url ~branch)
       | None ->
           Git.fetch_with_tags ~proc_mgr ~cwd:git ~remote);

      (* Get old SHA *)
      let old_sha = match Git.rev_parse ~proc_mgr ~cwd:git (upstream_branch repo_name) with
        | Some sha -> sha
        | None -> failwith "Upstream branch not found"
      in

      (* Determine default branch and update upstream *)
      let default_branch = Git.ls_remote_default_branch ~proc_mgr ~cwd:git ~url in
      let ref_point = remote ^ "/" ^ default_branch in
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(upstream_branch repo_name) ~point:ref_point;

      (* Get new SHA *)
      let new_sha = match Git.rev_parse ~proc_mgr ~cwd:git (upstream_branch repo_name) with
        | Some sha -> sha
        | None -> failwith "Upstream branch not found"
      in

      if old_sha = new_sha then
        Backend.No_changes repo_name
      else begin
        (* Create worktrees *)
        Worktree.ensure ~proc_mgr root (upstream_kind repo_name);
        Worktree.ensure ~proc_mgr root (vendor_kind repo_name);

        let upstream_wt = Worktree.path root (upstream_kind repo_name) in
        let vendor_wt = Worktree.path root (vendor_kind repo_name) in

        (* Clear vendor content and copy new *)
        let vendor_pkg_path = Eio.Path.(vendor_wt / "vendor" / "git" / repo_name) in
        (try Eio.Path.rmtree vendor_pkg_path with _ -> ());

        copy_with_prefix
          ~src_dir:upstream_wt
          ~dst_dir:vendor_wt
          ~prefix:(vendor_path repo_name);

        (* Commit *)
        Git.add_all ~proc_mgr ~cwd:vendor_wt;
        Git.commit ~proc_mgr ~cwd:vendor_wt
          ~message:(Printf.sprintf "Update %s to %s" repo_name (String.sub new_sha 0 7));

        (* Cleanup *)
        Worktree.remove ~proc_mgr root (upstream_kind repo_name);
        Worktree.remove ~proc_mgr root (vendor_kind repo_name);

        Backend.Updated { name = repo_name; old_sha; new_sha }
      end
    end
  with exn ->
    (try Worktree.remove_force ~proc_mgr root (upstream_kind repo_name) with _ -> ());
    (try Worktree.remove_force ~proc_mgr root (vendor_kind repo_name) with _ -> ());
    Backend.Update_failed { name = repo_name; error = Printexc.to_string exn }

let list_repos ~proc_mgr ~root =
  Worktree.list_git_repos ~proc_mgr root

let remove_repo ~proc_mgr ~root repo_name =
  let git = Worktree.git_dir root in

  (* Remove worktrees if exist *)
  (try Worktree.remove_force ~proc_mgr root (upstream_kind repo_name) with _ -> ());
  (try Worktree.remove_force ~proc_mgr root (vendor_kind repo_name) with _ -> ());
  (try Worktree.remove_force ~proc_mgr root (patches_kind repo_name) with _ -> ());

  (* Delete branches *)
  (try Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; upstream_branch repo_name] |> ignore with _ -> ());
  (try Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; vendor_branch repo_name] |> ignore with _ -> ());
  (try Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; patches_branch repo_name] |> ignore with _ -> ());

  (* Remove remote *)
  let remote = "origin-" ^ repo_name in
  (try Git.run_exn ~proc_mgr ~cwd:git ["remote"; "remove"; remote] |> ignore with _ -> ())
