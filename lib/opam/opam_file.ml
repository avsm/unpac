(** Opam file parsing for extracting package metadata. *)

type metadata = {
  name : string;
  version : string;
  dev_repo : string option;
  synopsis : string option;
}

let empty_metadata = {
  name = "";
  version = "";
  dev_repo = None;
  synopsis = None;
}

(** Parse an opam file and extract metadata. *)
let parse ~name ~version content =
  try
    let opam = OpamParser.FullPos.string content "<opam>" in
    let items = opam.file_contents in

    let dev_repo = ref None in
    let synopsis = ref None in

    List.iter (fun item ->
      match item.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.Variable (name_pos, value_pos) ->
          let var_name = name_pos.OpamParserTypes.FullPos.pelem in
          (match var_name, value_pos.OpamParserTypes.FullPos.pelem with
           | "dev-repo", OpamParserTypes.FullPos.String s ->
               dev_repo := Some s
           | "synopsis", OpamParserTypes.FullPos.String s ->
               synopsis := Some s
           | _ -> ())
      | _ -> ()
    ) items;

    { name; version; dev_repo = !dev_repo; synopsis = !synopsis }
  with _ ->
    { empty_metadata with name; version }

(** Parse an opam file from a path. *)
let parse_file ~name ~version path =
  let content = In_channel.with_open_text path In_channel.input_all in
  parse ~name ~version content

(** Find a package in an opam repository directory.
    Returns the path to the opam file if found. *)
let find_in_repo ~repo_path ~name ?version () =
  let packages_dir = Filename.concat repo_path "packages" in
  let pkg_dir = Filename.concat packages_dir name in

  if not (Sys.file_exists pkg_dir && Sys.is_directory pkg_dir) then
    None
  else
    (* List version directories *)
    let entries = Sys.readdir pkg_dir |> Array.to_list in
    let version_dirs = List.filter (fun entry ->
      let full = Filename.concat pkg_dir entry in
      Sys.is_directory full && String.starts_with ~prefix:(name ^ ".") entry
    ) entries in

    match version with
    | Some v ->
        (* Look for specific version *)
        let target = name ^ "." ^ v in
        if List.mem target version_dirs then
          let opam_path = Filename.concat (Filename.concat pkg_dir target) "opam" in
          if Sys.file_exists opam_path then Some (opam_path, v)
          else None
        else None
    | None ->
        (* Find latest version (simple string sort, works for semver) *)
        let sorted = List.sort (fun a b -> String.compare b a) version_dirs in
        match sorted with
        | [] -> None
        | latest :: _ ->
            let v = String.sub latest (String.length name + 1)
                      (String.length latest - String.length name - 1) in
            let opam_path = Filename.concat (Filename.concat pkg_dir latest) "opam" in
            if Sys.file_exists opam_path then Some (opam_path, v)
            else None

(** Get metadata for a package from an opam repository. *)
let get_metadata ~repo_path ~name ?version () =
  match find_in_repo ~repo_path ~name ?version () with
  | None -> None
  | Some (opam_path, v) ->
      Some (parse_file ~name ~version:v opam_path)

(** List all versions of a package in a repository. *)
let list_versions ~repo_path ~name =
  let packages_dir = Filename.concat repo_path "packages" in
  let pkg_dir = Filename.concat packages_dir name in

  if not (Sys.file_exists pkg_dir && Sys.is_directory pkg_dir) then
    []
  else
    Sys.readdir pkg_dir
    |> Array.to_list
    |> List.filter_map (fun entry ->
        if String.starts_with ~prefix:(name ^ ".") entry then
          Some (String.sub entry (String.length name + 1)
                  (String.length entry - String.length name - 1))
        else None)
    |> List.sort String.compare
