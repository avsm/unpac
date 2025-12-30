(** Monorepo export: create a standalone buildable directory from unpac workspace.

    Combines all projects and their vendored dependencies into a single directory
    structure suitable for building with dune. No git history is included.

    {1 Output Structure}

    The exported monorepo has this structure:
    {v
    output/
    ├── dune-project          # Combined project metadata
    ├── dune                   # Root dune with vendored_dirs
    ├── project1/              # First project
    │   ├── src/
    │   ├── dune
    │   └── dune-project
    ├── project2/              # Second project
    │   └── ...
    └── vendor/                # All vendored dependencies
        ├── dune               # (vendored_dirs opam git)
        ├── opam/
        │   ├── astring/
        │   ├── eio/
        │   └── ...
        └── git/
            ├── mylib/
            └── ...
    v}

    {1 Usage}

    {v
    unpac monorepo /path/to/output
    unpac monorepo -p myproject /path/to/output   # single project
    unpac monorepo --no-opam /path/to/output      # skip opam packages
    v}

    The output can be built directly with [dune build] or [dune build @doc].
*)

(** {1 Configuration} *)

type export_config = {
  output_dir : string;           (** Target directory for export *)
  projects : string list option; (** Projects to include (None = all) *)
  include_opam : bool;           (** Include vendored opam packages *)
  include_git : bool;            (** Include vendored git repositories *)
}

val default_config : output_dir:string -> export_config
(** Create default config exporting all projects and dependencies. *)

(** {1 Export Result} *)

type export_result = {
  projects_exported : string list;  (** Projects that were exported *)
  opam_packages : string list;      (** Opam packages in vendor/ *)
  git_repos : string list;          (** Git repos in vendor/ *)
  output_path : string;             (** Path to output directory *)
}

(** {1 Export Function} *)

val export :
  proc_mgr:Git.proc_mgr ->
  root:Worktree.root ->
  config:export_config ->
  export_result
(** [export ~proc_mgr ~root ~config] creates a standalone monorepo.

    The function:
    1. Exports each project from its [project/<name>] branch
    2. Strips the [vendor/] directory from each project
    3. Exports all vendored opam packages from [opam/patches/*] branches
    4. Exports all vendored git repos from [git-repos/patches/*] branches
    5. Places dependencies in a shared [vendor/] directory
    6. Generates appropriate dune files for building

    No git history is preserved - only the current state of each branch. *)
