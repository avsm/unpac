(** Project initialization for unpac.

    Creates the bare repository structure and initial main worktree. *)

let default_unpac_toml = {|[opam]
repositories = []
# compiler = "5.4.0"

# Vendor cache location (default: XDG cache directory)
# vendor_cache = "/path/to/cache"

[projects]
# Projects will be added here
|}

let project_dune_project name = Printf.sprintf {|(lang dune 3.20)
(name %s)
|} name

let project_dune = {|(vendored_dirs vendor)
|}

let project_gitignore = {|_build/
*.install
|}

let vendor_dune = {|(vendored_dirs opam)
|}

(** Initialize a new unpac project at the given path. *)
let init ~proc_mgr ~fs path =
  (* Convert relative paths to absolute *)
  let abs_path =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else path
  in
  let root = Eio.Path.(fs / abs_path) in

  (* Create root directory *)
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o755 root;

  (* Initialize bare repository *)
  let git_path = Eio.Path.(root / "git") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o755 git_path;
  Git.run_exn ~proc_mgr ~cwd:git_path ["init"; "--bare"] |> ignore;

  (* Create initial main branch with unpac.toml *)
  (* First create a temporary worktree to make the initial commit *)
  let main_path = Eio.Path.(root / "main") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o755 main_path;

  (* Initialize as a regular repo temporarily to create first commit *)
  Git.run_exn ~proc_mgr ~cwd:main_path ["init"] |> ignore;

  (* Write unpac.toml *)
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(main_path / "unpac.toml")
    default_unpac_toml;

  (* Create initial commit *)
  Git.run_exn ~proc_mgr ~cwd:main_path ["add"; "unpac.toml"] |> ignore;
  Git.run_exn ~proc_mgr ~cwd:main_path
    ["commit"; "-m"; "Initial commit"] |> ignore;

  (* Rename branch to main if needed *)
  Git.run_exn ~proc_mgr ~cwd:main_path ["branch"; "-M"; "main"] |> ignore;

  (* Push to bare repo and convert to worktree *)
  Git.run_exn ~proc_mgr ~cwd:main_path
    ["remote"; "add"; "origin"; "../git"] |> ignore;
  Git.run_exn ~proc_mgr ~cwd:main_path
    ["push"; "-u"; "origin"; "main"] |> ignore;

  (* Remove the temporary clone and add main as a worktree of the bare repo *)
  Eio.Path.rmtree main_path;

  (* Add main as a worktree of the bare repo *)
  Git.run_exn ~proc_mgr ~cwd:git_path
    ["worktree"; "add"; "../main"; "main"] |> ignore;

  root

(** Check if a path is an unpac project root. *)
let is_unpac_root path =
  Eio.Path.is_directory Eio.Path.(path / "git") &&
  Eio.Path.is_directory Eio.Path.(path / "main") &&
  Eio.Path.is_file Eio.Path.(path / "main" / "unpac.toml")

(** Find the unpac root by walking up from current directory. *)
let find_root ~fs ~cwd =
  let rec go path =
    if is_unpac_root path then Some path
    else match Eio.Path.split path with
      | Some (parent, _) -> go parent
      | None -> None
  in
  go Eio.Path.(fs / cwd)

(** Create a new project branch with template. *)
let create_project ~proc_mgr root name =
  let project_path = Worktree.path root (Project name) in

  (* Ensure project directory parent exists *)
  let project_dir = Eio.Path.(root / "project") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 project_dir;

  (* Create orphan branch *)
  Worktree.ensure_orphan ~proc_mgr root (Project name);

  (* Write template files *)
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(project_path / "dune-project")
    (project_dune_project name);

  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(project_path / "dune")
    project_dune;

  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(project_path / ".gitignore")
    project_gitignore;

  (* Create vendor directory structure with dune file *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(project_path / "vendor" / "opam");

  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(project_path / "vendor" / "dune")
    vendor_dune;

  (* Commit template *)
  Git.run_exn ~proc_mgr ~cwd:project_path ["add"; "-A"] |> ignore;
  Git.run_exn ~proc_mgr ~cwd:project_path
    ["commit"; "-m"; "Initialize project " ^ name] |> ignore;

  (* Update main/unpac.toml to register project *)
  let main_path = Worktree.path root Main in
  let toml_path = Eio.Path.(main_path / "unpac.toml") in
  let content = Eio.Path.load toml_path in

  (* Simple append to [projects] section - a proper implementation would parse TOML *)
  let updated =
    if content = "" || not (String.ends_with ~suffix:"\n" content)
    then content ^ "\n" ^ name ^ " = {}\n"
    else content ^ name ^ " = {}\n"
  in
  Eio.Path.save ~create:(`Or_truncate 0o644) toml_path updated;

  Git.run_exn ~proc_mgr ~cwd:main_path ["add"; "unpac.toml"] |> ignore;
  Git.run_exn ~proc_mgr ~cwd:main_path
    ["commit"; "-m"; "Add project " ^ name] |> ignore;

  project_path

(** Remove a project branch and worktree. *)
let remove_project ~proc_mgr root name =
  (* Remove worktree if exists *)
  Worktree.remove_force ~proc_mgr root (Project name);

  (* Delete the branch *)
  let git = Worktree.git_dir root in
  let branch = Worktree.branch (Project name) in
  Git.run_exn ~proc_mgr ~cwd:git ["branch"; "-D"; branch] |> ignore
