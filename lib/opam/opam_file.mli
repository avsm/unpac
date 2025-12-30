(** Opam file parsing for extracting package metadata. *)

type metadata = {
  name : string;
  version : string;
  dev_repo : string option;
  synopsis : string option;
}

val parse : name:string -> version:string -> string -> metadata
(** [parse ~name ~version content] parses opam file content. *)

val parse_file : name:string -> version:string -> string -> metadata
(** [parse_file ~name ~version path] parses an opam file from disk. *)

val find_in_repo : repo_path:string -> name:string -> ?version:string -> unit -> (string * string) option
(** [find_in_repo ~repo_path ~name ?version ()] finds a package in an opam repository.
    Returns [Some (opam_file_path, version)] if found. *)

val get_metadata : repo_path:string -> name:string -> ?version:string -> unit -> metadata option
(** [get_metadata ~repo_path ~name ?version ()] gets package metadata from a repository. *)

val list_versions : repo_path:string -> name:string -> string list
(** [list_versions ~repo_path ~name] lists all available versions of a package. *)
