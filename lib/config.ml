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

(** {1 TOML Codecs} *)

let repo_config_codec : repo_config Tomlt.t =
  let open Tomlt in
  let open Table in
  let make repo_name path url : repo_config =
    let source =
      match (path, url) with
      | Some p, None -> Local p
      | None, Some u -> Remote u
      | Some _, Some _ ->
          failwith "Repository cannot have both 'path' and 'url'"
      | None, None -> failwith "Repository must have either 'path' or 'url'"
    in
    { repo_name; source }
  in
  let enc_path (r : repo_config) =
    match r.source with Local p -> Some p | Remote _ -> None
  in
  let enc_url (r : repo_config) =
    match r.source with Remote u -> Some u | Local _ -> None
  in
  obj make
  |> mem "name" string ~enc:(fun (r : repo_config) -> r.repo_name)
  |> opt_mem "path" string ~enc:enc_path
  |> opt_mem "url" string ~enc:enc_url
  |> finish

let vendored_package_codec : vendored_package Tomlt.t =
  let open Tomlt in
  let open Table in
  obj (fun pkg_name pkg_url pkg_branch : vendored_package ->
    { pkg_name; pkg_url; pkg_branch })
  |> mem "name" string ~enc:(fun (p : vendored_package) -> p.pkg_name)
  |> mem "url" string ~enc:(fun (p : vendored_package) -> p.pkg_url)
  |> opt_mem "branch" string ~enc:(fun (p : vendored_package) -> p.pkg_branch)
  |> finish

let opam_config_codec : opam_config Tomlt.t =
  let open Tomlt in
  let open Table in
  obj (fun repositories compiler vendored : opam_config ->
    { repositories; compiler; vendored = Option.value ~default:[] vendored })
  |> mem "repositories" (list repo_config_codec)
       ~enc:(fun (c : opam_config) -> c.repositories)
  |> opt_mem "compiler" string ~enc:(fun (c : opam_config) -> c.compiler)
  |> opt_mem "vendored" (list vendored_package_codec)
       ~enc:(fun (c : opam_config) -> if c.vendored = [] then None else Some c.vendored)
  |> finish

let git_repo_config_codec : git_repo_config Tomlt.t =
  let open Tomlt in
  let open Table in
  obj (fun git_name git_url git_branch git_subdir : git_repo_config ->
    { git_name; git_url; git_branch; git_subdir })
  |> mem "name" string ~enc:(fun (r : git_repo_config) -> r.git_name)
  |> mem "url" string ~enc:(fun (r : git_repo_config) -> r.git_url)
  |> opt_mem "branch" string ~enc:(fun (r : git_repo_config) -> r.git_branch)
  |> opt_mem "subdir" string ~enc:(fun (r : git_repo_config) -> r.git_subdir)
  |> finish

let git_config_codec : git_config Tomlt.t =
  let open Tomlt in
  let open Table in
  obj (fun repos : git_config -> { repos })
  |> mem "repos" (list git_repo_config_codec)
       ~enc:(fun (c : git_config) -> c.repos)
  |> finish

let empty_git = { repos = [] }

(* For now, projects is not parsed from TOML - derived from git branches *)
type config = t

let codec : config Tomlt.t =
  let open Tomlt in
  let open Table in
  obj (fun opam git vendor_cache : config ->
    { opam; git = Option.value ~default:empty_git git; vendor_cache; projects = [] })
  |> mem "opam" opam_config_codec ~enc:(fun (c : config) -> c.opam)
  |> opt_mem "git" git_config_codec ~enc:(fun (c : config) ->
       if c.git.repos = [] then None else Some c.git)
  |> opt_mem "vendor_cache" string ~enc:(fun (c : config) -> c.vendor_cache)
  |> finish

(** {1 Loading} *)

let load path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    Tomlt_bytesrw.decode_string codec content
    |> Result.map_error Tomlt.Toml.Error.to_string
  with
  | Sys_error msg -> Error msg
  | Failure msg -> Error msg

let load_exn path =
  match load path with Ok c -> c | Error msg -> failwith msg

(** {1 Saving} *)

let save path config =
  try
    let content = Tomlt_bytesrw.encode_string codec config in
    Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content);
    Ok ()
  with
  | Sys_error msg -> Error msg
  | Failure msg -> Error msg

let save_exn path config =
  match save path config with
  | Ok () -> ()
  | Error msg -> failwith msg

(** {1 Helpers} *)

let empty_opam = { repositories = []; compiler = None; vendored = [] }
let empty = { opam = empty_opam; git = empty_git; vendor_cache = None; projects = [] }

let find_project config name =
  List.find_opt (fun p -> p.project_name = name) config.projects

(* Opam repo helpers *)
let add_repo config repo =
  let repos = config.opam.repositories @ [repo] in
  { config with opam = { config.opam with repositories = repos } }

let remove_repo config name =
  let repos = List.filter (fun r -> r.repo_name <> name) config.opam.repositories in
  { config with opam = { config.opam with repositories = repos } }

let find_repo config name =
  List.find_opt (fun r -> r.repo_name = name) config.opam.repositories

let set_compiler config version =
  { config with opam = { config.opam with compiler = Some version } }

let get_compiler config =
  config.opam.compiler

(* Git repo helpers *)
let add_git_repo config (repo : git_repo_config) =
  let repos = config.git.repos @ [repo] in
  { config with git = { repos } }

let remove_git_repo config name =
  let repos = List.filter (fun (r : git_repo_config) -> r.git_name <> name) config.git.repos in
  { config with git = { repos } }

let find_git_repo config name =
  List.find_opt (fun (r : git_repo_config) -> r.git_name = name) config.git.repos

let list_git_repos config =
  config.git.repos

(* Vendor cache helpers *)
let set_vendor_cache config path =
  { config with vendor_cache = Some path }

let get_vendor_cache config =
  config.vendor_cache

let resolve_vendor_cache ?cli_override config =
  (* Priority: CLI flag > env var > config file > default *)
  match cli_override with
  | Some path -> Some path
  | None ->
      match Sys.getenv_opt "UNPAC_VENDOR_CACHE" with
      | Some path -> Some path
      | None -> config.vendor_cache

(* Vendored package helpers *)
let add_vendored_package config (pkg : vendored_package) =
  (* Replace if exists, otherwise append *)
  let vendored = List.filter (fun p -> p.pkg_name <> pkg.pkg_name) config.opam.vendored in
  let vendored = vendored @ [pkg] in
  { config with opam = { config.opam with vendored } }

let remove_vendored_package config name =
  let vendored = List.filter (fun p -> p.pkg_name <> name) config.opam.vendored in
  { config with opam = { config.opam with vendored } }

let find_vendored_package config name =
  List.find_opt (fun p -> p.pkg_name = name) config.opam.vendored

let list_vendored_packages config =
  config.opam.vendored
