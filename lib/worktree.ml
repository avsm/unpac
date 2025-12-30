(** Git worktree lifecycle management for unpac.

    Manages creation, cleanup, and paths of worktrees within the unpac
    directory structure. All branch operations happen in isolated worktrees. *)

(** {1 Types} *)

type root = Eio.Fs.dir_ty Eio.Path.t
(** The unpac project root directory (contains git/, main/, etc.) *)

type kind =
  | Main
  | Project of string
  | Opam_upstream of string
  | Opam_vendor of string
  | Opam_patches of string
  | Git_upstream of string
  | Git_vendor of string
  | Git_patches of string
(** Worktree kinds with their associated names.
    Opam_* variants are for opam package vendoring.
    Git_* variants are for direct git repository vendoring. *)

(** {1 Path and Branch Helpers} *)

let git_dir root = Eio.Path.(root / "git")
(** Path to the bare git repository. *)

let path root = function
  | Main -> Eio.Path.(root / "main")
  | Project name -> Eio.Path.(root / "project" / name)
  | Opam_upstream name -> Eio.Path.(root / "opam" / "upstream" / name)
  | Opam_vendor name -> Eio.Path.(root / "opam" / "vendor" / name)
  | Opam_patches name -> Eio.Path.(root / "opam" / "patches" / name)
  | Git_upstream name -> Eio.Path.(root / "git-repos" / "upstream" / name)
  | Git_vendor name -> Eio.Path.(root / "git-repos" / "vendor" / name)
  | Git_patches name -> Eio.Path.(root / "git-repos" / "patches" / name)

let branch = function
  | Main -> "main"
  | Project name -> "project/" ^ name
  | Opam_upstream name -> "opam/upstream/" ^ name
  | Opam_vendor name -> "opam/vendor/" ^ name
  | Opam_patches name -> "opam/patches/" ^ name
  | Git_upstream name -> "git/upstream/" ^ name
  | Git_vendor name -> "git/vendor/" ^ name
  | Git_patches name -> "git/patches/" ^ name

let relative_path = function
  | Main -> "main"
  | Project name -> "project/" ^ name
  | Opam_upstream name -> "opam/upstream/" ^ name
  | Opam_vendor name -> "opam/vendor/" ^ name
  | Opam_patches name -> "opam/patches/" ^ name
  | Git_upstream name -> "git-repos/upstream/" ^ name
  | Git_vendor name -> "git-repos/vendor/" ^ name
  | Git_patches name -> "git-repos/patches/" ^ name

(** {1 Queries} *)

let exists root kind =
  let p = path root kind in
  Eio.Path.is_directory p

let branch_exists ~proc_mgr root kind =
  let git = git_dir root in
  Git.branch_exists ~proc_mgr ~cwd:git (branch kind)

(** {1 Operations} *)

let ensure ~proc_mgr root kind =
  if exists root kind then ()
  else begin
    let git = git_dir root in
    let wt_path = path root kind in
    let rel_path = "../" ^ relative_path kind in  (* Relative to git/ dir *)
    let br = branch kind in

    (* Ensure parent directories exist *)
    let parent = Eio.Path.split wt_path |> Option.map fst in
    Option.iter (fun p -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p) parent;

    (* Create worktree *)
    Git.run_exn ~proc_mgr ~cwd:git
      ["worktree"; "add"; rel_path; br] |> ignore
  end

let ensure_orphan ~proc_mgr root kind =
  if exists root kind then ()
  else begin
    let git = git_dir root in
    let wt_path = path root kind in
    let rel_path = "../" ^ relative_path kind in  (* Relative to git/ dir *)
    let br = branch kind in

    (* Ensure parent directories exist *)
    let parent = Eio.Path.split wt_path |> Option.map fst in
    Option.iter (fun p -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p) parent;

    (* Create a detached worktree from main branch, then make it an orphan *)
    let start_commit = Git.run_exn ~proc_mgr ~cwd:git ["rev-parse"; "main"] |> String.trim in
    Git.run_exn ~proc_mgr ~cwd:git
      ["worktree"; "add"; "--detach"; rel_path; start_commit] |> ignore;

    (* Now in the worktree, create an orphan branch and clear files *)
    Git.run_exn ~proc_mgr ~cwd:wt_path ["checkout"; "--orphan"; br] |> ignore;
    (* Remove all tracked files from index *)
    Git.run_exn ~proc_mgr ~cwd:wt_path ["rm"; "-rf"; "--cached"; "."] |> ignore;
    (* Clean the working directory *)
    Git.run_exn ~proc_mgr ~cwd:wt_path ["clean"; "-fd"] |> ignore
  end

let ensure_detached ~proc_mgr root kind ~commit =
  if exists root kind then ()
  else begin
    let git = git_dir root in
    let wt_path = path root kind in
    let rel_path = "../" ^ relative_path kind in  (* Relative to git/ dir *)

    (* Ensure parent directories exist *)
    let parent = Eio.Path.split wt_path |> Option.map fst in
    Option.iter (fun p -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p) parent;

    (* Create detached worktree at commit *)
    Git.run_exn ~proc_mgr ~cwd:git
      ["worktree"; "add"; "--detach"; rel_path; commit] |> ignore
  end

let remove ~proc_mgr root kind =
  if not (exists root kind) then ()
  else begin
    let git = git_dir root in
    let rel_path = "../" ^ relative_path kind in  (* Relative to git/ dir *)
    Git.run_exn ~proc_mgr ~cwd:git
      ["worktree"; "remove"; rel_path] |> ignore
  end

let remove_force ~proc_mgr root kind =
  if not (exists root kind) then ()
  else begin
    let git = git_dir root in
    let rel_path = "../" ^ relative_path kind in  (* Relative to git/ dir *)
    Git.run_exn ~proc_mgr ~cwd:git
      ["worktree"; "remove"; "--force"; rel_path] |> ignore
  end

let with_temp ~proc_mgr root kind f =
  ensure ~proc_mgr root kind;
  Fun.protect
    ~finally:(fun () -> remove ~proc_mgr root kind)
    (fun () -> f (path root kind))

let with_temp_orphan ~proc_mgr root kind f =
  ensure_orphan ~proc_mgr root kind;
  Fun.protect
    ~finally:(fun () -> remove ~proc_mgr root kind)
    (fun () -> f (path root kind))

(** {1 Listing} *)

let list_worktrees ~proc_mgr root =
  let git = git_dir root in
  Git.run_lines ~proc_mgr ~cwd:git ["worktree"; "list"; "--porcelain"]
  |> List.filter_map (fun line ->
      if String.starts_with ~prefix:"worktree " line then
        Some (String.sub line 9 (String.length line - 9))
      else None)

let list_projects ~proc_mgr root =
  let git = git_dir root in
  Git.run_lines ~proc_mgr ~cwd:git ["branch"; "--list"; "project/*"]
  |> List.filter_map (fun line ->
      let line = String.trim line in
      (* Strip "* " (current) or "+ " (linked worktree) prefix *)
      let line =
        if String.starts_with ~prefix:"* " line || String.starts_with ~prefix:"+ " line
        then String.sub line 2 (String.length line - 2)
        else line
      in
      if String.starts_with ~prefix:"project/" line then
        Some (String.sub line 8 (String.length line - 8))
      else None)

let list_opam_packages ~proc_mgr root =
  let git = git_dir root in
  Git.run_lines ~proc_mgr ~cwd:git ["branch"; "--list"; "opam/patches/*"]
  |> List.filter_map (fun line ->
      let line = String.trim line in
      (* Strip "* " (current) or "+ " (linked worktree) prefix *)
      let line =
        if String.starts_with ~prefix:"* " line || String.starts_with ~prefix:"+ " line
        then String.sub line 2 (String.length line - 2)
        else line
      in
      if String.starts_with ~prefix:"opam/patches/" line then
        Some (String.sub line 13 (String.length line - 13))
      else None)

let list_git_repos ~proc_mgr root =
  let git = git_dir root in
  Git.run_lines ~proc_mgr ~cwd:git ["branch"; "--list"; "git/patches/*"]
  |> List.filter_map (fun line ->
      let line = String.trim line in
      (* Strip "* " (current) or "+ " (linked worktree) prefix *)
      let line =
        if String.starts_with ~prefix:"* " line || String.starts_with ~prefix:"+ " line
        then String.sub line 2 (String.length line - 2)
        else line
      in
      if String.starts_with ~prefix:"git/patches/" line then
        Some (String.sub line 12 (String.length line - 12))
      else None)
