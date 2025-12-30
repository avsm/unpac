(** Tool definitions for Claude to interact with unpac and analyze code.

    Uses the Claude SDK's MCP-based custom tool architecture. Tools are
    defined as Claude.Tool.t values and bundled into an Mcp_server that
    gets registered with the Claude client. *)

let src = Logs.Src.create "unpac.claude.tools" ~doc:"Claude tools"
module Log = (val Logs.src_log src : Logs.LOG)

(* Helper to truncate long output *)
let truncate_output ?(max_len=50000) s =
  if String.length s > max_len then
    String.sub s 0 max_len ^ "\n\n[... truncated ...]"
  else s

(* Tool result helpers - convert to Claude.Tool format *)
let ok s = Ok (Claude.Tool.text_result s)
let err s = Error s

(* === TOOL IMPLEMENTATIONS === *)

(* Git list tool *)
let git_list ~proc_mgr ~root () =
  try
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if repos = [] then
      ok "No git repositories vendored.\n\nTo add one: use git_add tool with url parameter."
    else
      let buf = Buffer.create 256 in
      Buffer.add_string buf "Vendored git repositories:\n";
      List.iter (fun r -> Buffer.add_string buf (Printf.sprintf "- %s\n" r)) repos;
      ok (Buffer.contents buf)
  with exn ->
    err (Printf.sprintf "Failed to list git repos: %s" (Printexc.to_string exn))

(* Git add tool *)
let git_add ~proc_mgr ~fs ~root ~url ?name ?branch ?subdir () =
  try
    let repo_name = match name with
      | Some n -> n
      | None ->
          let base = Filename.basename url in
          if String.ends_with ~suffix:".git" base then
            String.sub base 0 (String.length base - 4)
          else base
    in

    let info : Unpac.Git_backend.repo_info = {
      name = repo_name;
      url;
      branch;
      subdir;
    } in

    let config_path = Filename.concat (snd (Unpac.Worktree.path root Unpac.Worktree.Main))
                        "unpac.toml" in
    let cache = if Sys.file_exists config_path then begin
      match Unpac.Config.load config_path with
      | Ok config -> Unpac.Config.resolve_vendor_cache config
      | Error _ -> None
    end else None in

    let cache = match cache with
      | Some path -> Some (Eio.Path.(fs / path))
      | None -> None
    in

    match Unpac.Git_backend.add_repo ~proc_mgr ~root ?cache info with
    | Unpac.Backend.Added { name = added_name; sha } ->
        ok (Printf.sprintf
          "Successfully added repository '%s' (commit %s).\n\n\
           Next steps:\n\
           - Use git_info %s to see repository details\n\
           - Use git_diff %s to see any local changes\n\
           - Merge into a project when ready" added_name (String.sub sha 0 7)
          added_name added_name)
    | Unpac.Backend.Already_exists name ->
        ok (Printf.sprintf "Repository '%s' is already vendored." name)
    | Unpac.Backend.Failed { name; error } ->
        err (Printf.sprintf "Failed to add '%s': %s" name error)
  with exn ->
    err (Printf.sprintf "Failed to add repository: %s" (Printexc.to_string exn))

(* Git info tool *)
let git_info ~proc_mgr ~root ~name () =
  try
    let git = Unpac.Worktree.git_dir root in
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then
      err (Printf.sprintf "Repository '%s' is not vendored" name)
    else begin
      let buf = Buffer.create 512 in
      let add s = Buffer.add_string buf s in

      add (Printf.sprintf "Repository: %s\n" name);

      let remote = "origin-" ^ name in
      (match Unpac.Git.remote_url ~proc_mgr ~cwd:git remote with
       | Some u -> add (Printf.sprintf "URL: %s\n" u)
       | None -> ());

      let upstream = Unpac.Git_backend.upstream_branch name in
      let vendor = Unpac.Git_backend.vendor_branch name in
      let patches = Unpac.Git_backend.patches_branch name in

      (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git upstream with
       | Some sha -> add (Printf.sprintf "Upstream: %s\n" (String.sub sha 0 7))
       | None -> ());
      (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git vendor with
       | Some sha -> add (Printf.sprintf "Vendor: %s\n" (String.sub sha 0 7))
       | None -> ());
      (match Unpac.Git.rev_parse ~proc_mgr ~cwd:git patches with
       | Some sha -> add (Printf.sprintf "Patches: %s\n" (String.sub sha 0 7))
       | None -> ());

      let log_output = Unpac.Git.run_exn ~proc_mgr ~cwd:git
        ["log"; "--oneline"; vendor ^ ".." ^ patches] in
      let commits = List.length (String.split_on_char '\n' log_output |>
                                 List.filter (fun s -> String.trim s <> "")) in
      add (Printf.sprintf "Local commits: %d\n" commits);

      ok (Buffer.contents buf)
    end
  with exn ->
    err (Printf.sprintf "Failed to get info for '%s': %s" name (Printexc.to_string exn))

(* Git diff tool *)
let git_diff ~proc_mgr ~root ~name () =
  try
    let git = Unpac.Worktree.git_dir root in
    let repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    if not (List.mem name repos) then
      err (Printf.sprintf "Repository '%s' is not vendored" name)
    else begin
      let vendor = Unpac.Git_backend.vendor_branch name in
      let patches = Unpac.Git_backend.patches_branch name in
      let diff = Unpac.Git.run_exn ~proc_mgr ~cwd:git ["diff"; vendor; patches] in
      if String.trim diff = "" then
        ok (Printf.sprintf "No local changes in '%s'." name)
      else
        ok (truncate_output (Printf.sprintf "Diff for '%s':\n\n%s" name diff))
    end
  with exn ->
    err (Printf.sprintf "Failed to get diff for '%s': %s" name (Printexc.to_string exn))

(* Opam list tool *)
let opam_list ~proc_mgr ~root () =
  try
    let pkgs = Unpac.Worktree.list_opam_packages ~proc_mgr root in
    if pkgs = [] then
      ok "No opam packages vendored."
    else begin
      let buf = Buffer.create 256 in
      Buffer.add_string buf "Vendored opam packages:\n";
      List.iter (fun p -> Buffer.add_string buf (Printf.sprintf "- %s\n" p)) pkgs;
      ok (Buffer.contents buf)
    end
  with exn ->
    err (Printf.sprintf "Failed to list opam packages: %s" (Printexc.to_string exn))

(* Project list tool *)
let project_list ~proc_mgr ~root () =
  try
    let projects = Unpac.Worktree.list_projects ~proc_mgr root in
    if projects = [] then
      ok "No projects configured."
    else begin
      let buf = Buffer.create 256 in
      Buffer.add_string buf "Projects:\n";
      List.iter (fun p -> Buffer.add_string buf (Printf.sprintf "- %s\n" p)) projects;
      ok (Buffer.contents buf)
    end
  with exn ->
    err (Printf.sprintf "Failed to list projects: %s" (Printexc.to_string exn))

(* Status tool - overview of the workspace *)
let status ~proc_mgr ~root () =
  try
    let buf = Buffer.create 1024 in
    let add s = Buffer.add_string buf s in

    add "=== Unpac Workspace Status ===\n\n";

    let projects = Unpac.Worktree.list_projects ~proc_mgr root in
    add (Printf.sprintf "Projects (%d):\n" (List.length projects));
    List.iter (fun p -> add (Printf.sprintf "  - %s\n" p)) projects;
    if projects = [] then add "  (none)\n";
    add "\n";

    let git_repos = Unpac.Git_backend.list_repos ~proc_mgr ~root in
    add (Printf.sprintf "Git Repositories (%d):\n" (List.length git_repos));
    List.iter (fun r -> add (Printf.sprintf "  - %s\n" r)) git_repos;
    if git_repos = [] then add "  (none)\n";
    add "\n";

    let opam_pkgs = Unpac.Worktree.list_opam_packages ~proc_mgr root in
    add (Printf.sprintf "Opam Packages (%d):\n" (List.length opam_pkgs));
    List.iter (fun p -> add (Printf.sprintf "  - %s\n" p)) opam_pkgs;
    if opam_pkgs = [] then add "  (none)\n";

    ok (Buffer.contents buf)
  with exn ->
    err (Printf.sprintf "Failed to get status: %s" (Printexc.to_string exn))

(* Read file tool *)
let read_file ~fs ~path () =
  try
    let full_path = Eio.Path.(fs / path) in
    if not (Eio.Path.is_file full_path) then
      err (Printf.sprintf "File not found: %s" path)
    else begin
      let content = Eio.Path.load full_path in
      ok (truncate_output content)
    end
  with exn ->
    err (Printf.sprintf "Failed to read '%s': %s" path (Printexc.to_string exn))

(* Write file tool *)
let write_file ~fs ~path ~content () =
  try
    let full_path = Eio.Path.(fs / path) in
    let parent = Filename.dirname path in
    if parent <> "." && parent <> "/" then begin
      let parent_path = Eio.Path.(fs / parent) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent_path
    end;
    Eio.Path.save ~create:(`Or_truncate 0o644) full_path content;
    ok (Printf.sprintf "Successfully wrote %d bytes to %s" (String.length content) path)
  with exn ->
    err (Printf.sprintf "Failed to write '%s': %s" path (Printexc.to_string exn))

(* List directory tool *)
let list_dir ~fs ~path () =
  try
    let full_path = Eio.Path.(fs / path) in
    if not (Eio.Path.is_directory full_path) then
      err (Printf.sprintf "Not a directory: %s" path)
    else begin
      let entries = Eio.Path.read_dir full_path in
      let entries = List.sort String.compare entries in
      let buf = Buffer.create 256 in
      Buffer.add_string buf (Printf.sprintf "Contents of %s:\n" path);
      List.iter (fun e ->
        let entry_path = Eio.Path.(full_path / e) in
        let suffix = if Eio.Path.is_directory entry_path then "/" else "" in
        Buffer.add_string buf (Printf.sprintf "  %s%s\n" e suffix)
      ) entries;
      ok (Buffer.contents buf)
    end
  with exn ->
    err (Printf.sprintf "Failed to list '%s': %s" path (Printexc.to_string exn))

(* Glob files tool *)
let glob_files ~fs ~pattern ~base_path () =
  try
    let full_base = Eio.Path.(fs / base_path) in
    if not (Eio.Path.is_directory full_base) then
      err (Printf.sprintf "Base path not a directory: %s" base_path)
    else begin
      let results = ref [] in
      let rec walk dir rel_path =
        let entries = try Eio.Path.read_dir dir with _ -> [] in
        List.iter (fun name ->
          let entry_path = Eio.Path.(dir / name) in
          let rel = if rel_path = "" then name else rel_path ^ "/" ^ name in
          if Eio.Path.is_directory entry_path then
            walk entry_path rel
          else begin
            let matches =
              if String.starts_with ~prefix:"**/" pattern then
                let ext = String.sub pattern 3 (String.length pattern - 3) in
                String.ends_with ~suffix:ext name
              else if String.starts_with ~prefix:"*" pattern then
                let ext = String.sub pattern 1 (String.length pattern - 1) in
                String.ends_with ~suffix:ext name
              else
                name = pattern
            in
            if matches then results := rel :: !results
          end
        ) entries
      in
      walk full_base "";
      let files = List.sort String.compare !results in
      if files = [] then
        ok (Printf.sprintf "No files matching '%s' in %s" pattern base_path)
      else begin
        let buf = Buffer.create 256 in
        Buffer.add_string buf (Printf.sprintf "Files matching '%s' in %s:\n" pattern base_path);
        List.iter (fun f -> Buffer.add_string buf (Printf.sprintf "  %s\n" f)) files;
        ok (Buffer.contents buf)
      end
    end
  with exn ->
    err (Printf.sprintf "Failed to glob '%s': %s" pattern (Printexc.to_string exn))

(* Shell execution tool *)
let run_shell ~proc_mgr ~fs ~cwd ~command () =
  try
    let result = Eio.Switch.run @@ fun sw ->
      let stdout_buf = Buffer.create 4096 in
      let stderr_buf = Buffer.create 4096 in
      let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
      let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in

      let cwd_path = Eio.Path.(fs / cwd) in

      let child = Eio.Process.spawn proc_mgr ~sw
        ~cwd:(cwd_path :> Eio.Fs.dir_ty Eio.Path.t)
        ~stdout:stdout_w ~stderr:stderr_w
        ["sh"; "-c"; command]
      in
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      Eio.Fiber.both
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stdout_r chunk with
             | n ->
                 Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n));
                 loop ()
             | exception End_of_file -> ()
           in loop ())
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stderr_r chunk with
             | n ->
                 Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n));
                 loop ()
             | exception End_of_file -> ()
           in loop ());

      let status = Eio.Process.await child in
      let stdout = Buffer.contents stdout_buf in
      let stderr = Buffer.contents stderr_buf in
      (status, stdout, stderr)
    in
    let (status, stdout, stderr) = result in
    let exit_code = match status with
      | `Exited c -> c
      | `Signaled s -> 128 + s
    in
    let buf = Buffer.create 256 in
    Buffer.add_string buf (Printf.sprintf "Exit code: %d\n" exit_code);
    if stdout <> "" then begin
      Buffer.add_string buf "\n=== STDOUT ===\n";
      Buffer.add_string buf stdout
    end;
    if stderr <> "" then begin
      Buffer.add_string buf "\n=== STDERR ===\n";
      Buffer.add_string buf stderr
    end;
    if exit_code = 0 then
      ok (truncate_output (Buffer.contents buf))
    else
      err (truncate_output (Buffer.contents buf))
  with exn ->
    err (Printf.sprintf "Failed to run command: %s" (Printexc.to_string exn))

(* Unpac status sync *)
let unpac_status_sync ~proc_mgr ~root () =
  try
    let main_wt = Unpac.Worktree.path root Unpac.Worktree.Main in
    let result = Eio.Switch.run @@ fun sw ->
      let stdout_buf = Buffer.create 4096 in
      let stderr_buf = Buffer.create 4096 in
      let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
      let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in

      let child = Eio.Process.spawn proc_mgr ~sw
        ~cwd:(main_wt :> Eio.Fs.dir_ty Eio.Path.t)
        ~stdout:stdout_w ~stderr:stderr_w
        ["unpac"; "status"]
      in
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      Eio.Fiber.both
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stdout_r chunk with
             | n -> Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ())
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stderr_r chunk with
             | n -> Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ());

      ignore (Eio.Process.await child);
      Buffer.contents stdout_buf
    in
    ok (Printf.sprintf "Ran unpac status:\n%s" (truncate_output result))
  with exn ->
    err (Printf.sprintf "Failed to run unpac status: %s" (Printexc.to_string exn))

(* Unpac push *)
let unpac_push ~proc_mgr ~root ~remote () =
  try
    let main_wt = Unpac.Worktree.path root Unpac.Worktree.Main in
    let result = Eio.Switch.run @@ fun sw ->
      let stdout_buf = Buffer.create 4096 in
      let stderr_buf = Buffer.create 4096 in
      let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
      let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in

      let child = Eio.Process.spawn proc_mgr ~sw
        ~cwd:(main_wt :> Eio.Fs.dir_ty Eio.Path.t)
        ~stdout:stdout_w ~stderr:stderr_w
        ["unpac"; "push"; remote]
      in
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      Eio.Fiber.both
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stdout_r chunk with
             | n -> Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ())
        (fun () ->
           let chunk = Cstruct.create 4096 in
           let rec loop () =
             match Eio.Flow.single_read stderr_r chunk with
             | n -> Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ());

      let status = Eio.Process.await child in
      let stdout = Buffer.contents stdout_buf in
      let stderr = Buffer.contents stderr_buf in
      (status, stdout, stderr)
    in
    let (status, stdout, stderr) = result in
    let exit_code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    if exit_code = 0 then
      ok (Printf.sprintf "Pushed to %s:\n%s" remote (truncate_output stdout))
    else
      err (Printf.sprintf "Push failed (exit %d):\n%s\n%s" exit_code stdout stderr)
  with exn ->
    err (Printf.sprintf "Failed to push: %s" (Printexc.to_string exn))

(* Git commit tool *)
let git_commit ~proc_mgr ~cwd ~message () =
  try
    let result = Eio.Switch.run @@ fun sw ->
      let add_child = Eio.Process.spawn proc_mgr ~sw
        ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
        ["git"; "add"; "-A"]
      in
      let add_status = Eio.Process.await add_child in
      (match add_status with
       | `Exited 0 -> ()
       | _ -> failwith "git add failed");

      let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
      let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in
      let stdout_buf = Buffer.create 256 in
      let stderr_buf = Buffer.create 256 in

      let commit_child = Eio.Process.spawn proc_mgr ~sw
        ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
        ~stdout:stdout_w ~stderr:stderr_w
        ["git"; "commit"; "-m"; message]
      in
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;

      Eio.Fiber.both
        (fun () ->
           let chunk = Cstruct.create 1024 in
           let rec loop () =
             match Eio.Flow.single_read stdout_r chunk with
             | n -> Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ())
        (fun () ->
           let chunk = Cstruct.create 1024 in
           let rec loop () =
             match Eio.Flow.single_read stderr_r chunk with
             | n -> Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n)); loop ()
             | exception End_of_file -> ()
           in loop ());

      let status = Eio.Process.await commit_child in
      (status, Buffer.contents stdout_buf, Buffer.contents stderr_buf)
    in
    let (status, stdout, stderr) = result in
    match status with
    | `Exited 0 -> ok (Printf.sprintf "Committed:\n%s" stdout)
    | `Exited 1 when String.length stdout > 0 ->
        ok "Nothing to commit (working tree clean)"
    | _ -> err (Printf.sprintf "Commit failed:\n%s\n%s" stdout stderr)
  with exn ->
    err (Printf.sprintf "Failed to commit: %s" (Printexc.to_string exn))

(* === MCP SERVER CREATION === *)

(** Create an MCP server with all unpac tools.

    The server name will be "unpac" so tools are accessible as mcp__unpac__<tool_name>.
    Call this with the Eio environment to create handlers with captured context. *)
let create_mcp_server ~proc_mgr ~fs ~root =
  let open Claude.Tool in

  let tools = [
    (* Workspace status tools *)
    create
      ~name:"unpac_status"
      ~description:"Get an overview of the unpac workspace, including all projects, \
                    vendored git repositories, and opam packages."
      ~input_schema:(schema_object [] ~required:[])
      ~handler:(fun _args -> status ~proc_mgr ~root ());

    create
      ~name:"unpac_status_sync"
      ~description:"Run 'unpac status' to update README.md and sync workspace state. \
                    Call this periodically to keep the workspace documentation current."
      ~input_schema:(schema_object [] ~required:[])
      ~handler:(fun _args -> unpac_status_sync ~proc_mgr ~root ());

    create
      ~name:"unpac_push"
      ~description:"Push all branches to the remote repository. Call this after making \
                    changes to sync with the remote."
      ~input_schema:(schema_object [("remote", schema_string)] ~required:["remote"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "remote" with
        | None -> err "Missing required parameter: remote"
        | Some remote -> unpac_push ~proc_mgr ~root ~remote ());

    (* Git vendoring tools *)
    create
      ~name:"unpac_git_list"
      ~description:"List all vendored git repositories in the workspace."
      ~input_schema:(schema_object [] ~required:[])
      ~handler:(fun _args -> git_list ~proc_mgr ~root ());

    create
      ~name:"unpac_git_add"
      ~description:"Vendor a new git repository. Clones the repo and creates the three-tier \
                    branch structure for conflict-free vendoring with full history preservation."
      ~input_schema:(schema_object [
        ("url", schema_string);
        ("name", schema_string);
        ("branch", schema_string);
        ("subdir", schema_string);
      ] ~required:["url"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "url" with
        | None -> err "Missing required parameter: url"
        | Some url ->
            let name = Claude.Tool_input.get_string args "name" in
            let branch = Claude.Tool_input.get_string args "branch" in
            let subdir = Claude.Tool_input.get_string args "subdir" in
            git_add ~proc_mgr ~fs ~root ~url ?name ?branch ?subdir ());

    create
      ~name:"unpac_git_info"
      ~description:"Show detailed information about a vendored git repository, including \
                    branch SHAs and number of local commits."
      ~input_schema:(schema_object [("name", schema_string)] ~required:["name"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "name" with
        | None -> err "Missing required parameter: name"
        | Some name -> git_info ~proc_mgr ~root ~name ());

    create
      ~name:"unpac_git_diff"
      ~description:"Show the diff between vendor and patches branches for a git repository. \
                    This shows what local modifications have been made."
      ~input_schema:(schema_object [("name", schema_string)] ~required:["name"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "name" with
        | None -> err "Missing required parameter: name"
        | Some name -> git_diff ~proc_mgr ~root ~name ());

    (* Opam tools *)
    create
      ~name:"unpac_opam_list"
      ~description:"List all vendored opam packages in the workspace."
      ~input_schema:(schema_object [] ~required:[])
      ~handler:(fun _args -> opam_list ~proc_mgr ~root ());

    create
      ~name:"unpac_project_list"
      ~description:"List all projects in the workspace."
      ~input_schema:(schema_object [] ~required:[])
      ~handler:(fun _args -> project_list ~proc_mgr ~root ());

    (* File operation tools *)
    create
      ~name:"read_file"
      ~description:"Read the contents of a file. Use this to analyze source code, \
                    STATUS.md files, test files, etc."
      ~input_schema:(schema_object [("path", schema_string)] ~required:["path"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "path" with
        | None -> err "Missing required parameter: path"
        | Some path -> read_file ~fs ~path ());

    create
      ~name:"write_file"
      ~description:"Write content to a file. Use this to update STATUS.md, fix code, \
                    add tests, etc. Parent directories are created if needed."
      ~input_schema:(schema_object [
        ("path", schema_string);
        ("content", schema_string);
      ] ~required:["path"; "content"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "path", Claude.Tool_input.get_string args "content" with
        | None, _ -> err "Missing required parameter: path"
        | _, None -> err "Missing required parameter: content"
        | Some path, Some content -> write_file ~fs ~path ~content ());

    create
      ~name:"list_directory"
      ~description:"List the contents of a directory."
      ~input_schema:(schema_object [("path", schema_string)] ~required:["path"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "path" with
        | None -> err "Missing required parameter: path"
        | Some path -> list_dir ~fs ~path ());

    create
      ~name:"glob_files"
      ~description:"Find files matching a glob pattern. Supports *.ml, **/*.ml patterns."
      ~input_schema:(schema_object [
        ("pattern", schema_string);
        ("base_path", schema_string);
      ] ~required:["pattern"; "base_path"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "pattern", Claude.Tool_input.get_string args "base_path" with
        | None, _ -> err "Missing required parameter: pattern"
        | _, None -> err "Missing required parameter: base_path"
        | Some pattern, Some base_path -> glob_files ~fs ~pattern ~base_path ());

    (* Shell execution *)
    create
      ~name:"run_shell"
      ~description:"Execute a shell command. Use for building (dune build), testing \
                    (dune test), or other operations. Be careful with destructive commands."
      ~input_schema:(schema_object [
        ("command", schema_string);
        ("cwd", schema_string);
      ] ~required:["command"; "cwd"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "command", Claude.Tool_input.get_string args "cwd" with
        | None, _ -> err "Missing required parameter: command"
        | _, None -> err "Missing required parameter: cwd"
        | Some command, Some cwd -> run_shell ~proc_mgr ~fs ~cwd ~command ());

    (* Git commit *)
    create
      ~name:"git_commit"
      ~description:"Stage all changes and create a git commit with the given message."
      ~input_schema:(schema_object [
        ("cwd", schema_string);
        ("message", schema_string);
      ] ~required:["cwd"; "message"])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "cwd", Claude.Tool_input.get_string args "message" with
        | None, _ -> err "Missing required parameter: cwd"
        | _, None -> err "Missing required parameter: message"
        | Some cwd, Some message ->
            let cwd_path = Eio.Path.(fs / cwd) in
            git_commit ~proc_mgr ~cwd:cwd_path ~message ());
  ] in

  Claude.Mcp_server.create ~name:"unpac" ~version:"1.0.0" ~tools ()
