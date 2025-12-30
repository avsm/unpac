(** Project initialization for unpac.

    Creates the bare repository structure and initial main worktree. *)

val init :
  proc_mgr:Git.proc_mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  string ->
  Worktree.root
(** [init ~proc_mgr ~fs path] creates a new unpac project at [path].

    Creates:
    - [path/git/] - bare git repository
    - [path/main/] - worktree for main branch with unpac.toml *)

val is_unpac_root : Eio.Fs.dir_ty Eio.Path.t -> bool
(** [is_unpac_root path] checks if [path] is an unpac project root. *)

val find_root :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:string ->
  Worktree.root option
(** [find_root ~fs ~cwd] walks up from [cwd] to find the unpac root. *)

val create_project :
  proc_mgr:Git.proc_mgr ->
  Worktree.root ->
  string ->
  Eio.Fs.dir_ty Eio.Path.t
(** [create_project ~proc_mgr root name] creates a new project branch.

    Creates orphan branch [project/<name>] with template:
    - dune-project (lang dune 3.20)
    - dune with (vendored_dirs vendor)
    - vendor/opam/ directory

    Updates main/unpac.toml to register the project. *)

val remove_project :
  proc_mgr:Git.proc_mgr ->
  Worktree.root ->
  string ->
  unit
(** [remove_project ~proc_mgr root name] removes a project branch and worktree. *)
