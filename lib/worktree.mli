(** Git worktree lifecycle management for unpac.

    Manages creation, cleanup, and paths of worktrees within the unpac
    directory structure. All branch operations happen in isolated worktrees.

    {2 Directory Structure}

    An unpac project has this layout:
    {v
    my-project/
    ├── git/                    # Bare repository
    ├── main/                   # Worktree → main branch
    ├── project/
    │   └── myapp/              # Worktree → project/myapp
    ├── opam/
    │   ├── upstream/
    │   │   └── pkg/            # Worktree → opam/upstream/pkg
    │   ├── vendor/
    │   │   └── pkg/            # Worktree → opam/vendor/pkg
    │   └── patches/
    │       └── pkg/            # Worktree → opam/patches/pkg
    └── git-repos/
        ├── upstream/
        │   └── repo/           # Worktree → git/upstream/repo
        ├── vendor/
        │   └── repo/           # Worktree → git/vendor/repo
        └── patches/
            └── repo/           # Worktree → git/patches/repo
    v} *)

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

val git_dir : root -> Eio.Fs.dir_ty Eio.Path.t
(** [git_dir root] returns the path to the bare git repository. *)

val path : root -> kind -> Eio.Fs.dir_ty Eio.Path.t
(** [path root kind] returns the filesystem path for the worktree. *)

val branch : kind -> string
(** [branch kind] returns the git branch name for the worktree kind. *)

(** {1 Queries} *)

val exists : root -> kind -> bool
(** [exists root kind] checks if the worktree directory exists. *)

val branch_exists : proc_mgr:Git.proc_mgr -> root -> kind -> bool
(** [branch_exists ~proc_mgr root kind] checks if the branch exists in git. *)

(** {1 Operations} *)

val ensure : proc_mgr:Git.proc_mgr -> root -> kind -> unit
(** [ensure ~proc_mgr root kind] creates the worktree if it doesn't exist.
    The branch must already exist. *)

val ensure_orphan : proc_mgr:Git.proc_mgr -> root -> kind -> unit
(** [ensure_orphan ~proc_mgr root kind] creates an orphan worktree.
    Creates a new orphan branch. *)

val ensure_detached : proc_mgr:Git.proc_mgr -> root -> kind -> commit:string -> unit
(** [ensure_detached ~proc_mgr root kind ~commit] creates a detached worktree
    at the given commit. Does not create a branch. *)

val remove : proc_mgr:Git.proc_mgr -> root -> kind -> unit
(** [remove ~proc_mgr root kind] removes the worktree (keeps the branch). *)

val remove_force : proc_mgr:Git.proc_mgr -> root -> kind -> unit
(** [remove_force ~proc_mgr root kind] forcibly removes the worktree. *)

val with_temp : proc_mgr:Git.proc_mgr -> root -> kind -> (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_temp ~proc_mgr root kind f] creates the worktree, runs [f] with
    the worktree path, then removes the worktree. *)

val with_temp_orphan : proc_mgr:Git.proc_mgr -> root -> kind -> (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_temp_orphan ~proc_mgr root kind f] creates an orphan worktree,
    runs [f], then removes the worktree. *)

(** {1 Listing} *)

val list_worktrees : proc_mgr:Git.proc_mgr -> root -> string list
(** [list_worktrees ~proc_mgr root] returns paths of all worktrees. *)

val list_projects : proc_mgr:Git.proc_mgr -> root -> string list
(** [list_projects ~proc_mgr root] returns names of all project branches. *)

val list_opam_packages : proc_mgr:Git.proc_mgr -> root -> string list
(** [list_opam_packages ~proc_mgr root] returns names of all vendored opam packages
    (packages with opam/patches/* branches). *)

val list_git_repos : proc_mgr:Git.proc_mgr -> root -> string list
(** [list_git_repos ~proc_mgr root] returns names of all vendored git repositories
    (repos with git/patches/* branches). *)
