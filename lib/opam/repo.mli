(** Opam repository operations. *)

type repo = {
  name : string;
  path : string;
}

type search_result = {
  repo : repo;
  metadata : Opam_file.metadata;
}

val find_package :
  repos:Unpac.Config.repo_config list ->
  name:string ->
  ?version:string ->
  unit ->
  search_result option
(** [find_package ~repos ~name ?version ()] searches for a package in repositories.
    Returns the first match found. *)

val list_versions :
  repos:Unpac.Config.repo_config list ->
  name:string ->
  (repo * string list) list
(** [list_versions ~repos ~name] lists all versions across repositories. *)

val search_packages :
  repos:Unpac.Config.repo_config list ->
  pattern:string ->
  (repo * string) list
(** [search_packages ~repos ~pattern] searches for packages matching a pattern. *)
