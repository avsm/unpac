(** Project promotion to vendor library.

    Promotes a locally-developed project to a vendored library by:
    1. Filtering out the vendor/ directory from the project history
    2. Creating vendor branches (upstream/vendor/patches) for the specified backend
    3. Recording the promotion in the audit log

    This allows the project to be merged into other projects as a dependency. *)

(** {1 Backend Types} *)

(** Vendor backend for the promoted library *)
type backend =
  | Opam  (** OCaml package - creates opam/* branches, vendor/opam/<name>/ path *)
  | Git   (** Git repository - creates git/* branches, vendor/git/<name>/ path *)

val backend_of_string : string -> backend option
(** Parse backend from string: "opam" or "git" *)

val backend_to_string : backend -> string
(** Convert backend to string *)

(** {1 Branch Names} *)

val upstream_branch : backend -> string -> string
(** [upstream_branch backend name] returns the upstream branch name,
    e.g., "opam/upstream/brotli" or "git/upstream/brotli" *)

val vendor_branch : backend -> string -> string
(** [vendor_branch backend name] returns the vendor branch name *)

val patches_branch : backend -> string -> string
(** [patches_branch backend name] returns the patches branch name *)

val vendor_path : backend -> string -> string
(** [vendor_path backend name] returns the vendor directory path,
    e.g., "vendor/opam/brotli" or "vendor/git/brotli" *)

(** {1 Promotion} *)

(** Result of a promote operation *)
type promote_result =
  | Promoted of {
      name : string;           (** Vendor library name *)
      backend : backend;       (** Backend used *)
      original_commits : int;  (** Commits in project before filtering *)
      filtered_commits : int;  (** Commits after removing vendor/ *)
    }
  | Already_promoted of string
      (** Library already exists with this name *)
  | Project_not_found of string
      (** Source project does not exist *)
  | Failed of { name : string; error : string }
      (** Promotion failed *)

val promote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  project:string ->
  backend:backend ->
  vendor_name:string option ->
  promote_result
(** [promote ~proc_mgr ~root ~project ~backend ~vendor_name] promotes
    a local project to a vendored library.

    The operation:
    1. Checks that the project exists and hasn't been promoted yet
    2. Creates a filtered copy of project history (excluding vendor/)
    3. Creates upstream/vendor/patches branches for the backend
    4. The original project branch is preserved unchanged

    @param project Name of the project to promote (e.g., "brotli")
    @param backend Backend type (Opam or Git)
    @param vendor_name Optional override for the vendor library name

    After promotion, the library can be merged into other projects using:
    - [unpac opam merge <name> <project>] for Opam backend
    - [unpac git merge <name> <project>] for Git backend *)

(** {1 Remote Management} *)

val project_remote_name : string -> string
(** [project_remote_name project] returns the git remote name for a project,
    e.g., "origin-brotli" *)

(** Result of set-remote operation *)
type set_remote_result =
  | Remote_set of { project : string; url : string; created : bool }
  | Project_not_found of string
  | Set_remote_failed of { project : string; error : string }

val set_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  project:string ->
  url:string ->
  set_remote_result
(** [set_remote ~proc_mgr ~root ~project ~url] sets the remote URL for a project.

    Creates or updates a git remote named "origin-<project>" pointing to the URL.
    This allows the project to be pushed independently using [push]. *)

val get_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  project:string ->
  string option
(** [get_remote ~proc_mgr ~root ~project] returns the remote URL for a project,
    or None if no remote is configured. *)

(** Result of push operation *)
type push_result =
  | Pushed of { project : string; branch : string; remote : string }
  | No_remote of string
  | Project_not_found of string
  | Push_failed of { project : string; error : string }

val push :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  project:string ->
  push_result
(** [push ~proc_mgr ~root ~project] pushes a project to its configured remote.

    Pushes the project/<name> branch to the remote configured via [set_remote].
    Returns [No_remote] if no remote has been configured. *)

(** {1 Project Info} *)

type project_info = {
  name : string;
  origin : [`Local | `Vendored];
  remote : string option;
  promoted_as : (backend * string) option;  (** backend, vendor_name *)
}

val get_info :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  project:string ->
  project_info option
(** [get_info ~proc_mgr ~root ~project] returns information about a project,
    or None if the project doesn't exist. *)

(** {1 Export (Unvendor)}

    Export reverses the vendoring process, creating a branch with files
    at the repository root suitable for pushing to an external git repo.

    This is the inverse of vendoring:
    - Vendoring: files at root → files in vendor/<backend>/<name>/
    - Exporting: files in vendor/<backend>/<name>/ → files at root *)

val export_branch : backend -> string -> string
(** [export_branch backend name] returns the export branch name,
    e.g., "opam/export/brotli" or "git/export/brotli" *)

(** Result of export operation *)
type export_result =
  | Exported of {
      name : string;            (** Package name *)
      backend : backend;        (** Backend used *)
      source_branch : string;   (** Branch exported from (vendor or patches) *)
      export_branch : string;   (** Created export branch *)
      commits : int;            (** Number of commits in export *)
    }
  | Not_vendored of string
      (** No vendor branch exists for this package *)
  | Already_exported of string
      (** Export branch already exists *)
  | Export_failed of { name : string; error : string }
      (** Export operation failed *)

val export :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  backend:backend ->
  from_patches:bool ->
  export_result
(** [export ~proc_mgr ~root ~name ~backend ~from_patches] exports a vendored
    package back to root-level files.

    Creates an export branch where files are moved from [vendor/<backend>/<name>/]
    to the repository root. This branch can then be pushed to an upstream repo.

    @param name The vendored package name
    @param backend The backend (Opam or Git)
    @param from_patches If true, exports from patches/* branch (includes local mods);
                        if false, exports from vendor/* branch (pristine upstream)

    The export branch is named [<backend>/export/<name>], e.g., "git/export/brotli".

    Example workflow:
    {[
      (* Export with local patches *)
      export ~from_patches:true ...

      (* Set remote and push *)
      set_export_remote ~url:"git@github.com:me/brotli.git" ...
      push_export ...
    ]} *)

val export_remote_name : string -> string
(** [export_remote_name name] returns the git remote name for exports,
    e.g., "export-brotli" *)

val set_export_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  url:string ->
  [ `Created | `Existed | `Updated ]
(** [set_export_remote ~proc_mgr ~root ~name ~url] sets the remote URL
    for pushing exports of a package. Creates remote "export-<name>". *)

val get_export_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  string option
(** [get_export_remote ~proc_mgr ~root ~name] returns the export remote URL,
    or None if no export remote is configured. *)

(** {2 Upstream Remote}

    The upstream remote is where we fetch updates from. For packages added
    via [opam add], the upstream is automatically configured. For promoted
    local projects, use [set_upstream_remote] to configure where updates
    should be fetched from. *)

val upstream_remote_name : string -> string
(** [upstream_remote_name name] returns the git remote name for upstream,
    e.g., "upstream-brotli" *)

val set_upstream_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  url:string ->
  [ `Created | `Existed | `Updated ]
(** [set_upstream_remote ~proc_mgr ~root ~name ~url] sets the remote URL
    for fetching upstream updates. Creates remote "upstream-<name>".

    This is used by [opam update] to fetch new changes. For promoted local
    projects, this typically points to the same repo as the export remote. *)

val get_upstream_remote :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  string option
(** [get_upstream_remote ~proc_mgr ~root ~name] returns the upstream remote URL,
    or None if no upstream remote is configured. *)

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
      (** No export branch exists for this package *)
  | No_export_remote of string
      (** No export remote configured *)
  | Export_push_failed of { name : string; error : string }
      (** Push operation failed *)

val push_export :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  name:string ->
  backend:backend ->
  export_push_result
(** [push_export ~proc_mgr ~root ~name ~backend] pushes an export branch
    to its configured remote.

    Pushes the [<backend>/export/<name>] branch to the remote configured
    via [set_export_remote], targeting the 'main' branch on the remote.

    Returns [Export_not_found] if the package hasn't been exported yet.
    Returns [No_export_remote] if no remote has been configured. *)

val list_exports :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  (backend * string) list
(** [list_exports ~proc_mgr ~root] returns all exported packages as
    (backend, name) pairs. *)
