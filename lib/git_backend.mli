(** Git backend for direct repository vendoring.

    Implements vendoring of arbitrary git repositories using the three-tier branch model:
    - git/upstream/<name> - pristine upstream code
    - git/vendor/<name> - upstream history rewritten with vendor/git/<name>/ prefix
    - git/patches/<name> - local modifications

    Unlike the opam backend which discovers packages via opam repositories,
    this backend allows cloning any git repository directly. *)

(** {1 Branch Naming} *)

val upstream_branch : string -> string
(** [upstream_branch name] returns the upstream branch name "git/upstream/<name>". *)

val vendor_branch : string -> string
(** [vendor_branch name] returns the vendor branch name "git/vendor/<name>". *)

val patches_branch : string -> string
(** [patches_branch name] returns the patches branch name "git/patches/<name>". *)

val vendor_path : string -> string
(** [vendor_path name] returns the vendor directory path "vendor/git/<name>". *)

(** {1 Repository Info} *)

type repo_info = {
  name : string;           (** User-specified name *)
  url : string;            (** Git URL to clone from *)
  branch : string option;  (** Optional branch/tag to track *)
  subdir : string option;  (** Optional subdirectory to extract *)
}

(** {1 Repository Operations} *)

val add_repo :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  ?cache:Vendor_cache.t ->
  repo_info ->
  Backend.add_result
(** [add_repo ~proc_mgr ~root ?cache info] vendors a git repository.

    Creates the three-tier branch structure:
    1. Fetches from url into git/upstream/<name>
    2. Rewrites history into git/vendor/<name> with vendor/git/<name>/ prefix
    3. Creates git/patches/<name> for local modifications

    If [subdir] is specified, only that subdirectory is extracted from the repo. *)

val update_repo :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  ?cache:Vendor_cache.t ->
  string ->
  Backend.update_result
(** [update_repo ~proc_mgr ~root ?cache name] updates a vendored repository from upstream. *)

val list_repos :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  string list
(** [list_repos ~proc_mgr ~root] returns names of all vendored git repositories. *)

val remove_repo :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  string ->
  unit
(** [remove_repo ~proc_mgr ~root name] removes a vendored repository. *)
