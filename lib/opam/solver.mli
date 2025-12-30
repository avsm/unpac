(** Dependency solver using 0install algorithm.

    Solves package dependencies across multiple configured opam repositories,
    filtering out compiler packages and respecting availability constraints. *)

type solve_result = {
  packages : OpamPackage.t list;
  (** List of packages that need to be installed, including transitive deps. *)
}

type solve_error = string
(** Human-readable error message explaining why solving failed. *)

val solve :
  repos:string list ->
  ocaml_version:string ->
  packages:string list ->
  (solve_result, solve_error) result
(** [solve ~repos ~ocaml_version ~packages] solves dependencies for [packages].

    @param repos List of opam repository root directories (containing packages/)
    @param ocaml_version The OCaml compiler version to solve for (e.g. "5.2.0")
    @param packages List of package names to solve for

    Returns the full list of packages (including transitive dependencies) that
    need to be installed, or an error message if solving failed.

    Compiler packages (ocaml-base-compiler, base-*, etc.) are automatically
    filtered out since they are assumed to be pre-installed. *)

val is_compiler_package : OpamPackage.Name.t -> bool
(** [is_compiler_package name] returns true if [name] is a known compiler
    or base package that should be filtered out. *)
