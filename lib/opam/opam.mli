(** Opam backend for unpac.

    Implements vendoring of opam packages using the three-tier branch model:
    - opam/upstream/<pkg> - pristine upstream code
    - opam/vendor/<pkg> - orphan branch with vendor/opam/<pkg>/ prefix
    - opam/patches/<pkg> - local modifications *)

val name : string
(** Backend name: "opam" *)

(** {1 Branch Naming} *)

val upstream_branch : string -> string
(** [upstream_branch pkg] returns "opam/upstream/<pkg>". *)

val vendor_branch : string -> string
(** [vendor_branch pkg] returns "opam/vendor/<pkg>". *)

val patches_branch : string -> string
(** [patches_branch pkg] returns "opam/patches/<pkg>". *)

val vendor_path : string -> string
(** [vendor_path pkg] returns "vendor/opam/<pkg>". *)

(** {1 Worktree Kinds} *)

val upstream_kind : string -> Unpac.Worktree.kind
val vendor_kind : string -> Unpac.Worktree.kind
val patches_kind : string -> Unpac.Worktree.kind

(** {1 Package Operations} *)

val add_package :
  proc_mgr:Unpac.Git.proc_mgr ->
  root:Unpac.Worktree.root ->
  ?cache:Unpac.Vendor_cache.t ->
  Unpac.Backend.package_info ->
  Unpac.Backend.add_result
(** [add_package ~proc_mgr ~root ?cache info] vendors a single package.

    1. Fetches upstream into opam/upstream/<pkg> (via cache if provided)
    2. Creates opam/vendor/<pkg> with vendor/opam/<pkg>/ prefix (preserving history)
    3. Creates opam/patches/<pkg> from vendor

    Uses git-filter-repo for fast history rewriting.
    @param cache Optional vendor cache for shared fetches across projects. *)

val update_package :
  proc_mgr:Unpac.Git.proc_mgr ->
  root:Unpac.Worktree.root ->
  ?cache:Unpac.Vendor_cache.t ->
  string ->
  Unpac.Backend.update_result
(** [update_package ~proc_mgr ~root ?cache name] updates a package from upstream.

    1. Fetches latest into opam/upstream/<pkg> (via cache if provided)
    2. Updates opam/vendor/<pkg> with new content

    Does NOT rebase patches - call [Backend.rebase_patches] separately.

    @param cache Optional vendor cache for shared fetches across projects. *)

val list_packages :
  proc_mgr:Unpac.Git.proc_mgr ->
  root:Unpac.Worktree.root ->
  string list
(** [list_packages ~proc_mgr root] returns all vendored opam package names. *)
