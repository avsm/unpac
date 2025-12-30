(** Git operations wrapped with Eio and robust error handling.

    All git commands are executed via [Eio.Process] with proper logging
    and error context. Errors are wrapped in [Eio.Exn.Io] with context
    chains for debugging. *)

(** {1 Error Types} *)

type error =
  | Command_failed of {
      cmd : string list;
      exit_code : int;
      stdout : string;
      stderr : string;
    }
  | Not_a_repository
  | Remote_exists of string
  | Remote_not_found of string
  | Branch_exists of string
  | Branch_not_found of string
  | Merge_conflict of { branch : string; conflicting_files : string list }
  | Rebase_conflict of { onto : string; hint : string }
  | Uncommitted_changes
  | Not_on_branch
  | Detached_head

val pp_error : Format.formatter -> error -> unit

type Eio.Exn.err += E of error

val err : error -> exn
(** [err e] creates an [Eio.Exn.Io] exception with the given error. *)

(** {1 Types} *)

type proc_mgr = [ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t
type path = Eio.Fs.dir_ty Eio.Path.t

(** {1 Low-level execution} *)

val run :
  proc_mgr:proc_mgr ->
  ?cwd:path ->
  ?audit:Audit.context ->
  string list ->
  (string, error) result
(** [run ~proc_mgr args] executes [git args] and returns stdout on success.
    If [audit] is provided, records the operation to the audit context. *)

val run_exn :
  proc_mgr:proc_mgr ->
  ?cwd:path ->
  ?audit:Audit.context ->
  string list ->
  string
(** [run_exn ~proc_mgr args] executes [git args] and returns stdout.
    Raises on failure with context. If [audit] is provided, records the operation. *)

val run_lines :
  proc_mgr:proc_mgr ->
  ?cwd:path ->
  ?audit:Audit.context ->
  string list ->
  string list
(** [run_lines ~proc_mgr args] executes and splits output by newlines.
    If [audit] is provided, records the operation. *)

(** {1 Queries - Safe read-only operations} *)

val is_repository : path -> bool
(** [is_repository path] checks if [path] contains a [.git] directory. *)

val current_branch :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string option
(** [current_branch] returns [Some branch] if on a branch, [None] if detached. *)

val current_branch_exn :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string
(** [current_branch_exn] returns current branch or raises [Not_on_branch]. *)

val current_head :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string
(** [current_head] returns the current HEAD SHA. *)

val has_uncommitted_changes :
  proc_mgr:proc_mgr ->
  cwd:path ->
  bool
(** [has_uncommitted_changes] returns true if there are staged or unstaged changes. *)

val remote_exists :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  bool
(** [remote_exists ~proc_mgr ~cwd name] checks if remote [name] exists. *)

val branch_exists :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  bool
(** [branch_exists ~proc_mgr ~cwd name] checks if branch [name] exists. *)

val rev_parse :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string option
(** [rev_parse ~proc_mgr ~cwd ref] returns the SHA for [ref], or [None]. *)

val rev_parse_exn :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string
(** [rev_parse_exn] returns SHA or raises. *)

val rev_parse_short :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string
(** [rev_parse_short] returns abbreviated SHA. *)

val ls_remote_default_branch :
  proc_mgr:proc_mgr ->
  cwd:path ->
  url:string ->
  string
(** [ls_remote_default_branch ~proc_mgr ~cwd ~url] detects the default branch of remote. *)

val list_remotes :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string list
(** [list_remotes] returns all remote names. *)

val remote_url :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string option
(** [remote_url ~proc_mgr ~cwd name] returns the URL for remote [name]. *)

val log_oneline :
  proc_mgr:proc_mgr ->
  cwd:path ->
  ?max_count:int ->
  string ->
  string ->
  string list
(** [log_oneline ~proc_mgr ~cwd from_ref to_ref] returns commit summaries. *)

val diff_stat :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string ->
  string
(** [diff_stat ~proc_mgr ~cwd from_ref to_ref] returns diff statistics. *)

val ls_tree :
  proc_mgr:proc_mgr ->
  cwd:path ->
  tree:string ->
  path:string ->
  bool
(** [ls_tree ~proc_mgr ~cwd ~tree ~path] checks if [path] exists in [tree]. *)

val rev_list_count :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  string ->
  int
(** [rev_list_count ~proc_mgr ~cwd from_ref to_ref] counts commits between refs. *)

(** {1 Idempotent mutations - Safe to re-run} *)

val ensure_remote :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  url:string ->
  [ `Created | `Existed | `Updated ]
(** [ensure_remote] adds remote if missing, updates URL if different. *)

val ensure_branch :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  start_point:string ->
  [ `Created | `Existed ]
(** [ensure_branch] creates branch if it doesn't exist. *)

val ensure_vendored_remotes :
  proc_mgr:proc_mgr ->
  cwd:path ->
  Config.vendored_package list ->
  int
(** [ensure_vendored_remotes ~proc_mgr ~cwd packages] ensures remotes exist for
    all vendored packages. Returns the number of remotes created.
    Use this to recreate remotes after cloning a workspace. *)

(** {1 State-changing operations} *)

val init :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [init] initializes a new git repository. *)

val fetch :
  proc_mgr:proc_mgr ->
  cwd:path ->
  remote:string ->
  unit
(** [fetch] fetches from a remote. *)

val fetch_with_tags :
  proc_mgr:proc_mgr ->
  cwd:path ->
  remote:string ->
  unit
(** [fetch_with_tags] fetches from a remote including all tags. *)

val resolve_branch_or_tag :
  proc_mgr:proc_mgr ->
  cwd:path ->
  remote:string ->
  ref_name:string ->
  string
(** [resolve_branch_or_tag] tries to resolve a ref first as a remote tracking
    branch (remote/ref_name), then as a tag (refs/tags/ref_name). Returns the
    resolved ref or raises an exception if neither exists. *)

val checkout :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  unit
(** [checkout] switches to a branch or commit. *)

val checkout_orphan :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  unit
(** [checkout_orphan] creates and switches to a new orphan branch. *)

val read_tree_prefix :
  proc_mgr:proc_mgr ->
  cwd:path ->
  prefix:string ->
  tree:string ->
  unit
(** [read_tree_prefix] reads a tree into the index with a path prefix. *)

val checkout_index :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [checkout_index] checks out files from the index to working directory. *)

val rm_rf :
  proc_mgr:proc_mgr ->
  cwd:path ->
  target:string ->
  unit
(** [rm_rf] removes files/directories from git tracking. *)

val rm_cached_rf :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [rm_cached_rf] removes all files from index (for orphan branch setup). *)

val add_all :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [add_all] stages all changes. *)

val commit :
  proc_mgr:proc_mgr ->
  cwd:path ->
  message:string ->
  unit
(** [commit] creates a commit with the given message. *)

val commit_allow_empty :
  proc_mgr:proc_mgr ->
  cwd:path ->
  message:string ->
  unit
(** [commit_allow_empty] creates a commit even if there are no changes. *)

val branch_create :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  start_point:string ->
  unit
(** [branch_create] creates a new branch at [start_point]. *)

val branch_force :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  point:string ->
  unit
(** [branch_force] moves branch to point (creates if needed). *)

val remote_add :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  url:string ->
  unit
(** [remote_add] adds a new remote. *)

val remote_set_url :
  proc_mgr:proc_mgr ->
  cwd:path ->
  name:string ->
  url:string ->
  unit
(** [remote_set_url] updates the URL of an existing remote. *)

val merge_allow_unrelated :
  proc_mgr:proc_mgr ->
  cwd:path ->
  branch:string ->
  message:string ->
  (unit, [ `Conflict of string list ]) result
(** [merge_allow_unrelated] merges with [--allow-unrelated-histories].
    Returns [Error (`Conflict files)] if there are conflicts. *)

val rebase :
  proc_mgr:proc_mgr ->
  cwd:path ->
  onto:string ->
  (unit, [ `Conflict of string ]) result
(** [rebase] rebases current branch onto [onto].
    Returns [Error (`Conflict hint)] if there are conflicts. *)

val rebase_abort :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [rebase_abort] aborts an in-progress rebase. *)

val merge_abort :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [merge_abort] aborts an in-progress merge. *)

val reset_hard :
  proc_mgr:proc_mgr ->
  cwd:path ->
  string ->
  unit
(** [reset_hard] does a hard reset to the given ref. *)

val clean_fd :
  proc_mgr:proc_mgr ->
  cwd:path ->
  unit
(** [clean_fd] removes untracked files and directories. *)

val filter_repo_to_subdirectory :
  proc_mgr:proc_mgr ->
  cwd:path ->
  branch:string ->
  subdirectory:string ->
  unit
(** [filter_repo_to_subdirectory ~proc_mgr ~cwd ~branch ~subdirectory]
    rewrites the history of [branch] so all files are moved into [subdirectory].
    Uses git-filter-repo for fast history rewriting. Preserves full commit history. *)

val filter_repo_from_subdirectory :
  proc_mgr:proc_mgr ->
  cwd:path ->
  branch:string ->
  subdirectory:string ->
  unit
(** [filter_repo_from_subdirectory ~proc_mgr ~cwd ~branch ~subdirectory]
    rewrites the history of [branch] extracting only files from [subdirectory]
    and placing them at the repository root. This is the inverse of
    [filter_repo_to_subdirectory]. Uses git-filter-repo --subdirectory-filter.
    Preserves full commit history for files that were in the subdirectory. *)
