(** Vendor cache - a persistent bare git repository for caching upstream fetches.

    The cache stores fetched repositories as remotes/branches, allowing multiple
    unpac projects to share fetched content without re-downloading. *)

(** {1 Types} *)

type t = Eio.Fs.dir_ty Eio.Path.t
(** Path to the cache bare repository *)

(** {1 Cache Location} *)

val default_path : unit -> string
(** Returns the default cache path (XDG_CACHE_HOME/unpac/vendor-cache) *)

(** {1 Initialization} *)

val init : proc_mgr:Git.proc_mgr -> fs:Eio.Fs.dir_ty Eio.Path.t -> ?path:string -> unit -> t
(** [init ~proc_mgr ~fs ?path ()] initializes and returns the cache.
    Creates the bare repository if it doesn't exist.
    @param path Optional custom cache path. Uses default if not provided. *)

(** {1 Remote Naming} *)

val url_to_remote_name : string -> string
(** [url_to_remote_name url] converts a git URL to a remote name.
    e.g., "https://github.com/dbuenzli/astring.git" -> "github.com/dbuenzli/astring" *)

val branch_name : remote:string -> branch:string -> string
(** [branch_name ~remote ~branch] returns the full branch name in cache. *)

(** {1 Cache Operations} *)

val has_remote : proc_mgr:Git.proc_mgr -> t -> string -> bool
(** [has_remote ~proc_mgr cache name] checks if a remote exists in cache. *)

val ensure_remote : proc_mgr:Git.proc_mgr -> t -> url:string -> string
(** [ensure_remote ~proc_mgr cache ~url] adds remote if needed, returns remote name. *)

val fetch : proc_mgr:Git.proc_mgr -> t -> url:string -> string
(** [fetch ~proc_mgr cache ~url] fetches from URL into cache, returns remote name. *)

val get_ref : proc_mgr:Git.proc_mgr -> t -> url:string -> branch:string -> string option
(** [get_ref ~proc_mgr cache ~url ~branch] returns the SHA for a cached ref. *)

val fetch_to_project :
  proc_mgr:Git.proc_mgr ->
  cache:t ->
  project_git:Eio.Fs.dir_ty Eio.Path.t ->
  url:string ->
  branch:string ->
  string
(** [fetch_to_project ~proc_mgr ~cache ~project_git ~url ~branch]
    fetches from upstream to cache, then from cache to project's bare repo.
    Returns the cache ref name. *)

(** {1 Listing} *)

val list_remotes : proc_mgr:Git.proc_mgr -> t -> string list
(** List all remotes in the cache. *)

val list_branches : proc_mgr:Git.proc_mgr -> t -> string list
(** List all branches in the cache. *)
