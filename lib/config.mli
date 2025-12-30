(** Configuration file handling for unpac.

    Loads and parses main/unpac.toml configuration files. *)

(** {1 Types} *)

type repo_source =
  | Local of string
  | Remote of string

type repo_config = {
  repo_name : string;
  source : repo_source;
}

type vendored_package = {
  pkg_name : string;          (** Package name (used as vendor name) *)
  pkg_url : string;           (** Original remote URL *)
  pkg_branch : string option; (** Original branch if specified *)
}

type opam_config = {
  repositories : repo_config list;
  compiler : string option;
  vendored : vendored_package list;  (** Tracked vendored packages *)
}

(** Git repository configuration for direct git vendoring *)
type git_repo_config = {
  git_name : string;          (** User-specified name for the repo *)
  git_url : string;           (** Git URL to clone from *)
  git_branch : string option; (** Optional branch/tag to track *)
  git_subdir : string option; (** Optional subdirectory to extract *)
}

type git_config = {
  repos : git_repo_config list;
}

type project_config = {
  project_name : string;
}

type t = {
  opam : opam_config;
  git : git_config;
  vendor_cache : string option;
  projects : project_config list;
}

(** {1 Loading} *)

val load : string -> (t, string) result
(** [load path] loads configuration from the TOML file at [path]. *)

val load_exn : string -> t
(** [load_exn path] is like {!load} but raises on error. *)

(** {1 Saving} *)

val save : string -> t -> (unit, string) result
(** [save path config] saves configuration to the TOML file at [path]. *)

val save_exn : string -> t -> unit
(** [save_exn path config] is like {!save} but raises on error. *)

(** {1 Helpers} *)

val empty : t
(** Empty configuration. *)

val find_project : t -> string -> project_config option
(** [find_project config name] finds a project by name. *)

(** {2 Opam Repository Helpers} *)

val add_repo : t -> repo_config -> t
(** [add_repo config repo] adds an opam repository to the config. *)

val remove_repo : t -> string -> t
(** [remove_repo config name] removes an opam repository by name. *)

val find_repo : t -> string -> repo_config option
(** [find_repo config name] finds an opam repository by name. *)

val set_compiler : t -> string -> t
(** [set_compiler config version] sets the OCaml compiler version. *)

val get_compiler : t -> string option
(** [get_compiler config] gets the configured OCaml compiler version. *)

(** {2 Git Repository Helpers} *)

val add_git_repo : t -> git_repo_config -> t
(** [add_git_repo config repo] adds a git repository to the config. *)

val remove_git_repo : t -> string -> t
(** [remove_git_repo config name] removes a git repository by name. *)

val find_git_repo : t -> string -> git_repo_config option
(** [find_git_repo config name] finds a git repository by name. *)

val list_git_repos : t -> git_repo_config list
(** [list_git_repos config] returns all configured git repositories. *)

(** {2 Vendor Cache Helpers} *)

val set_vendor_cache : t -> string -> t
(** [set_vendor_cache config path] sets the vendor cache path. *)

val get_vendor_cache : t -> string option
(** [get_vendor_cache config] gets the configured vendor cache path. *)

val resolve_vendor_cache : ?cli_override:string -> t -> string option
(** [resolve_vendor_cache ?cli_override config] resolves vendor cache path.
    Priority: CLI flag > UNPAC_VENDOR_CACHE env var > config file.
    Returns None if not configured anywhere. *)

(** {2 Vendored Package Helpers} *)

val add_vendored_package : t -> vendored_package -> t
(** [add_vendored_package config pkg] adds or replaces a vendored package entry. *)

val remove_vendored_package : t -> string -> t
(** [remove_vendored_package config name] removes a vendored package by name. *)

val find_vendored_package : t -> string -> vendored_package option
(** [find_vendored_package config name] finds a vendored package by name. *)

val list_vendored_packages : t -> vendored_package list
(** [list_vendored_packages config] returns all vendored packages. *)

(** {1 Codecs} *)

val codec : t Tomlt.t
(** TOML codec for the configuration type. *)
