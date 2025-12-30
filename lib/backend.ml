(** Backend module signature for package managers.

    Each backend (opam, cargo, etc.) implements this interface to provide
    vendoring capabilities. *)

(** {1 Types} *)

type package_info = {
  name : string;
  url : string;
  branch : string option;  (** Branch/tag/ref to use *)
}
(** Information about a package to vendor. *)

type add_result =
  | Added of { name : string; sha : string }
  | Already_exists of string
  | Failed of { name : string; error : string }

type update_result =
  | Updated of { name : string; old_sha : string; new_sha : string }
  | No_changes of string
  | Update_failed of { name : string; error : string }

(** {1 Backend Signature} *)

module type S = sig
  val name : string
  (** Backend name, e.g. "opam", "cargo". *)

  (** {2 Branch Naming} *)

  val upstream_branch : string -> string
  (** [upstream_branch pkg] returns branch name, e.g. "opam/upstream/astring". *)

  val vendor_branch : string -> string
  (** [vendor_branch pkg] returns branch name, e.g. "opam/vendor/astring". *)

  val patches_branch : string -> string
  (** [patches_branch pkg] returns branch name, e.g. "opam/patches/astring". *)

  val vendor_path : string -> string
  (** [vendor_path pkg] returns path prefix, e.g. "vendor/opam/astring". *)

  (** {2 Worktree Kinds} *)

  val upstream_kind : string -> Worktree.kind
  val vendor_kind : string -> Worktree.kind
  val patches_kind : string -> Worktree.kind

  (** {2 Package Operations} *)

  val add_package :
    proc_mgr:Git.proc_mgr ->
    root:Worktree.root ->
    package_info ->
    add_result
  (** [add_package ~proc_mgr ~root info] vendors a single package.

      1. Creates/updates opam/upstream/<pkg> from URL
      2. Creates opam/vendor/<pkg> orphan with vendor/ prefix
      3. Creates opam/patches/<pkg> from vendor *)

  val update_package :
    proc_mgr:Git.proc_mgr ->
    root:Worktree.root ->
    string ->
    update_result
  (** [update_package ~proc_mgr ~root name] updates a package from upstream.

      1. Fetches latest into opam/upstream/<pkg>
      2. Updates opam/vendor/<pkg> with new content
      Does NOT rebase patches - that's a separate operation. *)

  val list_packages :
    proc_mgr:Git.proc_mgr ->
    root:Worktree.root ->
    string list
  (** [list_packages ~proc_mgr root] returns all vendored package names. *)
end

(** {1 Merge Operations} *)

(** These operations are backend-agnostic and work on any patches branch. *)

let merge_to_project ~proc_mgr ~root ~project ~patches_branch =
  let project_wt = Worktree.path root (Worktree.Project project) in
  Git.merge_allow_unrelated ~proc_mgr ~cwd:project_wt
    ~branch:patches_branch
    ~message:(Printf.sprintf "Merge %s" patches_branch)

let rebase_patches ~proc_mgr ~root ~patches_kind ~onto =
  Worktree.ensure ~proc_mgr root patches_kind;
  let patches_wt = Worktree.path root patches_kind in
  let result = Git.rebase ~proc_mgr ~cwd:patches_wt ~onto in
  Worktree.remove ~proc_mgr root patches_kind;
  result
