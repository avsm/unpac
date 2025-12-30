(** Opam backend for unpac.

    Implements vendoring of opam packages using the three-tier branch model:
    - opam/upstream/<pkg> - pristine upstream code
    - opam/vendor/<pkg> - upstream history rewritten with vendor/opam/<pkg>/ prefix
    - opam/patches/<pkg> - local modifications

    The vendor branch preserves full git history from upstream, with all paths
    rewritten to be under vendor/opam/<pkg>/. This allows git blame/log to work
    correctly on vendored files. *)

module Worktree = Unpac.Worktree
module Git = Unpac.Git
module Git_repo_lookup = Unpac.Git_repo_lookup
module Vendor_cache = Unpac.Vendor_cache
module Backend = Unpac.Backend

let name = "opam"

(** {1 Branch Naming} *)

let upstream_branch pkg = "opam/upstream/" ^ pkg
let vendor_branch pkg = "opam/vendor/" ^ pkg
let patches_branch pkg = "opam/patches/" ^ pkg
let vendor_path pkg = "vendor/opam/" ^ pkg

(** {1 Worktree Kinds} *)

let upstream_kind pkg = Worktree.Opam_upstream pkg
let vendor_kind pkg = Worktree.Opam_vendor pkg
let patches_kind pkg = Worktree.Opam_patches pkg

(** {1 Package Operations} *)

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

let add_package ~proc_mgr ~root ?cache (info : Backend.package_info) =
  let pkg = info.name in
  let git = Worktree.git_dir root in

  try
    (* Check if already exists *)
    if Worktree.branch_exists ~proc_mgr root (patches_kind pkg) then
      Backend.Already_exists pkg
    else begin
      (* Rewrite URL for known mirrors *)
      let url = Git_repo_lookup.rewrite_url info.url in

      (* Determine the ref to use: explicit > override > default *)
      let branch = match info.branch with
        | Some b -> b
        | None ->
            match Git_repo_lookup.branch_override ~name:pkg ~url with
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
            (* Direct fetch (with tags to support version tags like 3.20.2) *)
            let remote = "origin-" ^ pkg in
            ignore (Git.ensure_remote ~proc_mgr ~cwd:git ~name:remote ~url);
            Git.fetch_with_tags ~proc_mgr ~cwd:git ~remote;
            Git.resolve_branch_or_tag ~proc_mgr ~cwd:git ~remote ~ref_name:branch
      in

      (* Step 1: Create upstream branch from fetched ref *)
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(upstream_branch pkg) ~point:ref_point;

      (* Step 2: Create vendor branch from upstream and rewrite history *)
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(vendor_branch pkg) ~point:(upstream_branch pkg);

      (* Rewrite vendor branch history to move all files into vendor/opam/<pkg>/ *)
      Git.filter_repo_to_subdirectory ~proc_mgr ~cwd:git
        ~branch:(vendor_branch pkg)
        ~subdirectory:(vendor_path pkg);

      (* Get the vendor SHA after rewriting *)
      let vendor_sha = match Git.rev_parse ~proc_mgr ~cwd:git (vendor_branch pkg) with
        | Some sha -> sha
        | None -> failwith "Vendor branch not found after filter-repo"
      in

      (* Step 3: Create patches branch from vendor *)
      Git.branch_create ~proc_mgr ~cwd:git
        ~name:(patches_branch pkg)
        ~start_point:(vendor_branch pkg);

      Backend.Added { name = pkg; sha = vendor_sha }
    end
  with exn ->
    (* Cleanup on failure *)
    (try Worktree.remove_force ~proc_mgr root (upstream_kind pkg) with _ -> ());
    (try Worktree.remove_force ~proc_mgr root (vendor_kind pkg) with _ -> ());
    Backend.Failed { name = pkg; error = Printexc.to_string exn }

let update_package ~proc_mgr ~root ?cache pkg =
  let git = Worktree.git_dir root in

  try
    (* Check if package exists *)
    if not (Worktree.branch_exists ~proc_mgr root (patches_kind pkg)) then
      Backend.Update_failed { name = pkg; error = "Package not vendored" }
    else begin
      (* Get remote URL - check origin-<pkg> first, then upstream-<pkg> for promoted packages *)
      let origin_remote = "origin-" ^ pkg in
      let upstream_remote = "upstream-" ^ pkg in
      let (remote, url) = match Git.remote_url ~proc_mgr ~cwd:git origin_remote with
        | Some u -> (origin_remote, u)
        | None ->
            (* Try upstream remote for promoted/local packages *)
            match Git.remote_url ~proc_mgr ~cwd:git upstream_remote with
            | Some u -> (upstream_remote, u)
            | None -> failwith (Printf.sprintf "No remote found. Set one with: unpac opam set-upstream %s <url>" pkg)
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
      let old_sha = match Git.rev_parse ~proc_mgr ~cwd:git (upstream_branch pkg) with
        | Some sha -> sha
        | None -> failwith "Upstream branch not found"
      in

      (* Determine default branch and update upstream *)
      let default_branch = Git.ls_remote_default_branch ~proc_mgr ~cwd:git ~url in
      let ref_point = remote ^ "/" ^ default_branch in
      Git.branch_force ~proc_mgr ~cwd:git
        ~name:(upstream_branch pkg) ~point:ref_point;

      (* Get new SHA *)
      let new_sha = match Git.rev_parse ~proc_mgr ~cwd:git (upstream_branch pkg) with
        | Some sha -> sha
        | None -> failwith "Upstream branch not found"
      in

      if old_sha = new_sha then
        Backend.No_changes pkg
      else begin
        (* Create worktrees *)
        Worktree.ensure ~proc_mgr root (upstream_kind pkg);
        Worktree.ensure ~proc_mgr root (vendor_kind pkg);

        let upstream_wt = Worktree.path root (upstream_kind pkg) in
        let vendor_wt = Worktree.path root (vendor_kind pkg) in

        (* Clear vendor content and copy new *)
        let vendor_pkg_path = Eio.Path.(vendor_wt / "vendor" / "opam" / pkg) in
        (try Eio.Path.rmtree vendor_pkg_path with _ -> ());

        copy_with_prefix
          ~src_dir:upstream_wt
          ~dst_dir:vendor_wt
          ~prefix:(vendor_path pkg);

        (* Commit *)
        Git.add_all ~proc_mgr ~cwd:vendor_wt;
        Git.commit ~proc_mgr ~cwd:vendor_wt
          ~message:(Printf.sprintf "Update %s to %s" pkg (String.sub new_sha 0 7));

        (* Cleanup *)
        Worktree.remove ~proc_mgr root (upstream_kind pkg);
        Worktree.remove ~proc_mgr root (vendor_kind pkg);

        Backend.Updated { name = pkg; old_sha; new_sha }
      end
    end
  with exn ->
    (try Worktree.remove_force ~proc_mgr root (upstream_kind pkg) with _ -> ());
    (try Worktree.remove_force ~proc_mgr root (vendor_kind pkg) with _ -> ());
    Backend.Update_failed { name = pkg; error = Printexc.to_string exn }

let list_packages ~proc_mgr ~root =
  Worktree.list_opam_packages ~proc_mgr root
