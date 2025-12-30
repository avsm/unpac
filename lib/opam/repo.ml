(** Opam repository operations. *)

type repo = {
  name : string;
  path : string;
}

type search_result = {
  repo : repo;
  metadata : Opam_file.metadata;
}

(** Resolve repository path from config. *)
let resolve_repo (cfg : Unpac.Config.repo_config) : repo option =
  match cfg.source with
  | Unpac.Config.Local path ->
      if Sys.file_exists path && Sys.is_directory path then
        Some { name = cfg.repo_name; path }
      else None
  | Unpac.Config.Remote _url ->
      (* Remote repos not yet supported *)
      None

(** Search for a package in configured repositories. *)
let find_package ~repos ~name ?version () : search_result option =
  let rec search = function
    | [] -> None
    | cfg :: rest ->
        match resolve_repo cfg with
        | None -> search rest
        | Some repo ->
            match Opam_file.get_metadata ~repo_path:repo.path ~name ?version () with
            | None -> search rest
            | Some metadata -> Some { repo; metadata }
  in
  search repos

(** List all versions of a package across repositories. *)
let list_versions ~repos ~name : (repo * string list) list =
  List.filter_map (fun cfg ->
    match resolve_repo cfg with
    | None -> None
    | Some repo ->
        let versions = Opam_file.list_versions ~repo_path:repo.path ~name in
        if versions = [] then None
        else Some (repo, versions)
  ) repos

(** Search for packages matching a pattern. *)
let search_packages ~repos ~pattern : (repo * string) list =
  List.concat_map (fun cfg ->
    match resolve_repo cfg with
    | None -> []
    | Some repo ->
        let packages_dir = Filename.concat repo.path "packages" in
        if not (Sys.file_exists packages_dir) then []
        else
          Sys.readdir packages_dir
          |> Array.to_list
          |> List.filter (fun name ->
              (* Simple substring match *)
              let pattern_lower = String.lowercase_ascii pattern in
              let name_lower = String.lowercase_ascii name in
              String.length pattern_lower <= String.length name_lower &&
              (let rec check i =
                if i > String.length name_lower - String.length pattern_lower then false
                else if String.sub name_lower i (String.length pattern_lower) = pattern_lower then true
                else check (i + 1)
              in check 0))
          |> List.map (fun name -> (repo, name))
  ) repos
