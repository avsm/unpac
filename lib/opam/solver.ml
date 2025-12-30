(** Dependency solver using 0install algorithm. *)

let ( / ) = Filename.concat

(** List directory entries, returns empty list if directory doesn't exist. *)
let list_dir path =
  try Sys.readdir path |> Array.to_list
  with Sys_error _ -> []

(** Known compiler packages to filter out. *)
let is_compiler_package name =
  let s = OpamPackage.Name.to_string name in
  String.starts_with ~prefix:"ocaml-base-compiler" s ||
  String.starts_with ~prefix:"ocaml-variants" s ||
  String.starts_with ~prefix:"ocaml-system" s ||
  String.starts_with ~prefix:"ocaml-config" s ||
  s = "ocaml" ||
  s = "base-unix" ||
  s = "base-threads" ||
  s = "base-bigarray" ||
  s = "base-domains" ||
  s = "base-nnp"

(** Check if a package has the compiler flag. *)
let has_compiler_flag opam =
  let flags = OpamFile.OPAM.flags opam in
  List.mem OpamTypes.Pkgflag_Compiler flags

(** Multi-repo context that searches multiple opam repository directories. *)
module Multi_context : sig
  include Opam_0install.S.CONTEXT

  val create :
    ?constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    repos:string list ->
    ocaml_version:string ->
    unit -> t
end = struct
  type rejection =
    | UserConstraint of OpamFormula.atom
    | Unavailable
    | CompilerPackage

  let pp_rejection f = function
    | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
    | Unavailable -> Fmt.pf f "Availability condition not satisfied"
    | CompilerPackage -> Fmt.pf f "Compiler package (filtered out)"

  type t = {
    repos : string list;  (* List of packages/ directories *)
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
    ocaml_version : string;
  }

  let env t _pkg v =
    match OpamVariable.Full.to_string v with
    | "arch" -> Some (OpamTypes.S "x86_64")
    | "os" -> Some (OpamTypes.S "linux")
    | "os-distribution" -> Some (OpamTypes.S "debian")
    | "os-version" -> Some (OpamTypes.S "12")
    | "os-family" -> Some (OpamTypes.S "debian")
    | "opam-version" -> Some (OpamTypes.S "2.2.0")
    | "sys-ocaml-version" -> Some (OpamTypes.S t.ocaml_version)
    | "ocaml:native" -> Some (OpamTypes.B true)
    | _ -> None

  let filter_deps t pkg f =
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev:false ~dev_setup:false ~default:false

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  (** Load opam file from path. *)
  let load_opam path =
    try Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
    with _ -> None

  (** Create a minimal virtual opam file for base packages. *)
  let virtual_opam () =
    OpamFile.OPAM.empty

  (** Find all versions of a package across all repos. *)
  let find_versions t name =
    let name_str = OpamPackage.Name.to_string name in
    (* Collect versions from all repos, first repo wins for duplicates *)
    let seen = Hashtbl.create 16 in
    List.iter (fun packages_dir ->
      let pkg_dir = packages_dir / name_str in
      list_dir pkg_dir |> List.iter (fun entry ->
        match OpamPackage.of_string_opt entry with
        | Some pkg when OpamPackage.name pkg = name ->
            let v = OpamPackage.version pkg in
            if not (Hashtbl.mem seen v) then begin
              let opam_path = pkg_dir / entry / "opam" in
              Hashtbl.add seen v opam_path
            end
        | _ -> ()
      )
    ) t.repos;
    Hashtbl.fold (fun v path acc -> (v, path) :: acc) seen []

  let candidates t name =
    let name_str = OpamPackage.Name.to_string name in
    (* Provide virtual packages for compiler/base packages at the configured version *)
    if name_str = "ocaml" then
      let v = OpamPackage.Version.of_string t.ocaml_version in
      [v, Ok (virtual_opam ())]
    else if name_str = "base-unix" || name_str = "base-threads" ||
            name_str = "base-bigarray" || name_str = "base-domains" ||
            name_str = "base-nnp" then
      let v = OpamPackage.Version.of_string "base" in
      [v, Ok (virtual_opam ())]
    else if is_compiler_package name then
      (* Other compiler packages - not available *)
      []
    else
      let user_constraints = user_restrictions t name in
      find_versions t name
      |> List.sort (fun (v1, _) (v2, _) -> OpamPackage.Version.compare v2 v1)  (* Prefer newest *)
      |> List.map (fun (v, opam_path) ->
          match user_constraints with
          | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
              v, Error (UserConstraint (name, Some test))
          | _ ->
              match load_opam opam_path with
              | None -> v, Error Unavailable
              | Some opam ->
                  (* Check flags:compiler *)
                  if has_compiler_flag opam then
                    v, Error CompilerPackage
                  else
                    (* Check available filter *)
                    let pkg = OpamPackage.create name v in
                    let available = OpamFile.OPAM.available opam in
                    match OpamFilter.eval ~default:(OpamTypes.B false) (env t pkg) available with
                    | B true -> v, Ok opam
                    | _ -> v, Error Unavailable
        )

  let create ?(constraints=OpamPackage.Name.Map.empty) ~repos ~ocaml_version () =
    (* Convert repo roots to packages/ directories *)
    let packages_dirs = List.map (fun r -> r / "packages") repos in
    { repos = packages_dirs; constraints; ocaml_version }
end

module Solver = Opam_0install.Solver.Make(Multi_context)

type solve_result = {
  packages : OpamPackage.t list;
}

type solve_error = string

(** Solve dependencies for a list of package names. *)
let solve ~repos ~ocaml_version ~packages : (solve_result, solve_error) result =
  let context = Multi_context.create ~repos ~ocaml_version () in
  let names = List.map OpamPackage.Name.of_string packages in
  match Solver.solve context names with
  | Ok selections ->
      let pkgs = Solver.packages_of_result selections in
      (* Filter out compiler packages from result *)
      let pkgs = List.filter (fun pkg ->
        not (is_compiler_package (OpamPackage.name pkg))
      ) pkgs in
      Ok { packages = pkgs }
  | Error diagnostics ->
      Error (Solver.diagnostics diagnostics)
