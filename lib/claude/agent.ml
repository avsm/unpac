(** Ralph-loop style Claude agent for unpac workspace analysis.

    Implements the ralph-loop pattern: same prompt fed each iteration,
    with state persisting in files. Runs up to max_iterations per project,
    exiting early on completion promise.

    Uses the Claude SDK's MCP-based custom tool architecture. Custom unpac
    tools are registered via an in-process MCP server, while Claude's built-in
    tools (Read, Write, Bash, etc.) are handled by Claude CLI directly. *)

let src = Logs.Src.create "unpac.claude.agent" ~doc:"Claude agent"
module Log = (val Logs.src_log src : Logs.LOG)

(* ANSI color codes *)
module Color = struct
  let reset = "\x1b[0m"
  let bold = "\x1b[1m"
  let dim = "\x1b[2m"
  let red = "\x1b[31m"
  let green = "\x1b[32m"
  let yellow = "\x1b[33m"
  let blue = "\x1b[34m"
  let magenta = "\x1b[35m"
  let cyan = "\x1b[36m"
end

type config = {
  verbose : bool;
  web_port : int option;
  max_iterations : int;
  project : string option;
}

let default_config = {
  verbose = false;
  web_port = None;
  max_iterations = 20;
  project = None;
}

let completion_promise = "AGENTIC-HUMPS-COUNT-2"

(* Format tool call for logging - full paths, no truncation *)
let format_tool_call name (input : Claude.Tool_input.t) =
  let get_string key = Claude.Tool_input.get_string input key in
  match name with
  | "Read" ->
      let path = get_string "file_path" |> Option.value ~default:"?" in
      Printf.sprintf "Read %s" path
  | "Write" ->
      let path = get_string "file_path" |> Option.value ~default:"?" in
      Printf.sprintf "Write %s" path
  | "Edit" ->
      let path = get_string "file_path" |> Option.value ~default:"?" in
      Printf.sprintf "Edit %s" path
  | "Bash" ->
      let cmd = get_string "command" |> Option.value ~default:"?" in
      Printf.sprintf "$ %s" cmd
  | "Glob" ->
      let pattern = get_string "pattern" |> Option.value ~default:"*" in
      let path = get_string "path" |> Option.value ~default:"" in
      if path = "" then Printf.sprintf "Glob %s" pattern
      else Printf.sprintf "Glob %s in %s" pattern path
  | "Grep" ->
      let pattern = get_string "pattern" |> Option.value ~default:"?" in
      let path = get_string "path" |> Option.value ~default:"" in
      if path = "" then Printf.sprintf "Grep %s" pattern
      else Printf.sprintf "Grep %s in %s" pattern path
  (* MCP tools are prefixed with mcp__unpac__ *)
  | s when String.length s > 12 && String.sub s 0 12 = "mcp__unpac__" ->
      let tool_name = String.sub s 12 (String.length s - 12) in
      (match tool_name with
       | "read_file" ->
           let path = get_string "path" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:read %s" path
       | "write_file" ->
           let path = get_string "path" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:write %s" path
       | "list_directory" ->
           let path = get_string "path" |> Option.value ~default:"." in
           Printf.sprintf "unpac:ls %s" path
       | "glob_files" ->
           let pattern = get_string "pattern" |> Option.value ~default:"*" in
           Printf.sprintf "unpac:glob %s" pattern
       | "run_shell" ->
           let cmd = get_string "command" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:$ %s" cmd
       | "git_commit" ->
           let msg = get_string "message" |> Option.value ~default:"" in
           Printf.sprintf "unpac:commit %s" msg
       | "unpac_status" -> "unpac:status"
       | "unpac_status_sync" -> "unpac:status --sync"
       | "unpac_push" ->
           let remote = get_string "remote" |> Option.value ~default:"origin" in
           Printf.sprintf "unpac:push %s" remote
       | "unpac_project_list" -> "unpac:projects"
       | "unpac_opam_list" -> "unpac:opam list"
       | "unpac_git_list" -> "unpac:git list"
       | "unpac_git_add" ->
           let url = get_string "url" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:git add %s" url
       | "unpac_git_info" ->
           let n = get_string "name" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:git info %s" n
       | "unpac_git_diff" ->
           let n = get_string "name" |> Option.value ~default:"?" in
           Printf.sprintf "unpac:git diff %s" n
       | _ -> Printf.sprintf "unpac:%s" tool_name)
  | _ -> name

(* Find unpac root from a given directory *)
let find_root_from fs dir =
  let rec search path depth =
    if depth > 10 then None
    else begin
      let git_path = Eio.Path.(path / "git") in
      let main_path = Eio.Path.(path / "main") in
      if Eio.Path.is_directory git_path && Eio.Path.is_directory main_path then
        Some path
      else
        match Eio.Path.split path with
        | Some (parent, _) -> search parent (depth + 1)
        | None -> None
    end
  in
  search (Eio.Path.(fs / dir)) 0

let string_contains ~sub s =
  let len_sub = String.length sub in
  let len_s = String.length s in
  if len_sub > len_s then false
  else begin
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0
  end

(* Shuffle a list randomly *)
let shuffle list =
  let arr = Array.of_list list in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

(* Ensure working directory exists *)
let ensure_work_dir fs root project =
  let root_dir = snd root in
  let work_base = Eio.Path.(fs / root_dir / ".unpac-claude") in
  let work_dir = Eio.Path.(work_base / project) in
  let claude_dir = Eio.Path.(work_dir / ".claude") in
  (try Eio.Path.mkdir ~perm:0o755 work_base with _ -> ());
  (try Eio.Path.mkdir ~perm:0o755 work_dir with _ -> ());
  (try Eio.Path.mkdir ~perm:0o755 claude_dir with _ -> ());
  work_dir

(* Run ralph-loop for a single project *)
let run_project_ralph_loop ~env ~config ~root ~project ~event_bus =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in

  let prefix = Printf.sprintf "[%s] " project in
  let log msg = Log.info (fun m -> m "%s%s" prefix msg) in
  let emit event = Event.emit event_bus event in

  log "Starting ralph-loop agent";
  Format.printf "@.%s%s═══ Project: %s ═══%s@." Color.bold Color.blue project Color.reset;
  emit (Event.Text (Printf.sprintf "\n=== Starting ralph-loop for %s ===\n" project));

  (* Ensure working directory *)
  let _work_dir = ensure_work_dir fs root project in

  (* Get project path *)
  let project_path = Unpac.Worktree.path root (Unpac.Worktree.Project project) in
  let project_dir = snd project_path in

  (* Generate the prompt (same prompt used every iteration - ralph-loop style) *)
  let prompt = Prompt.generate_for_project ~proc_mgr ~root ~project in

  (* Create MCP server with custom unpac tools *)
  let mcp_server = Tools.create_mcp_server ~proc_mgr ~fs ~root in

  (* Build Claude options - always Opus 4.5 *)
  (* Register MCP server so custom tools are available via mcp__unpac__<tool> *)
  let options =
    Claude.Options.default
    |> Claude.Options.with_model (`Custom "claude-opus-4-5")
    |> Claude.Options.with_system_prompt prompt
    |> Claude.Options.with_permission_mode Claude.Permissions.Mode.Bypass_permissions
    |> Claude.Options.with_mcp_server ~name:"unpac" mcp_server
  in

  (* Ralph-loop: same prompt each iteration *)
  let iteration_prompt = Printf.sprintf
    "You are working on the '%s' project at %s.\n\n\
     Analyze the project, make improvements, update STATUS.md, and commit changes.\n\n\
     You have access to:\n\
     - Claude's built-in tools: Read, Write, Edit, Bash, Glob, Grep\n\
     - Custom unpac tools (mcp__unpac__*): unpac_status, unpac_git_list, etc.\n\n\
     When ALL significant work is complete, output exactly: %s\n\n\
     Begin."
    project project_dir completion_promise
  in

  (* Run the ralph-loop *)
  let rec ralph_loop iteration =
    if iteration > config.max_iterations then begin
      log (Printf.sprintf "Max iterations (%d) reached" config.max_iterations);
      Format.printf "@.%s%s⚠ Max iterations (%d) reached%s@."
        Color.bold Color.yellow config.max_iterations Color.reset;
      emit (Event.Text (Printf.sprintf "\n[%s] Max iterations reached.\n" project))
    end else begin
      log (Printf.sprintf "Iteration %d/%d" iteration config.max_iterations);
      Format.printf "@.%s%s── Iteration %d/%d ──%s@."
        Color.bold Color.yellow iteration config.max_iterations Color.reset;
      emit (Event.Text (Printf.sprintf "\n[%s] --- Iteration %d/%d ---\n"
        project iteration config.max_iterations));

      let accumulated_response = ref "" in
      let completion_detected = ref false in
      let last_was_tool = ref false in

      begin
        try
          Eio.Switch.run @@ fun inner_sw ->
          let client = Claude.Client.create ~sw:inner_sw ~process_mgr:proc_mgr
            ~clock:(Eio.Stdenv.clock env) ~options () in

          emit Event.Thinking;

          let handler = object
            inherit Claude.Handler.default

            method! on_text text =
              let content = Claude.Response.Text.content text in
              accumulated_response := !accumulated_response ^ content;
              if !last_was_tool then begin
                Format.printf "@.";
                last_was_tool := false
              end;
              Format.printf "%s@?" content;
              emit (Event.Text (Printf.sprintf "[%s] %s" project content));
              if string_contains ~sub:completion_promise !accumulated_response then
                completion_detected := true

            method! on_thinking thinking =
              let content = Claude.Response.Thinking.content thinking in
              Format.printf "%s%s  💭 %s%s@." Color.dim Color.magenta content Color.reset

            method! on_tool_use tool =
              (* Just log tool usage - execution is handled by Claude CLI for built-in
                 tools and by MCP server for custom tools *)
              let name = Claude.Response.Tool_use.name tool in
              let id = Claude.Response.Tool_use.id tool in
              let input = Claude.Response.Tool_use.input tool in

              let call_summary = format_tool_call name input in

              if config.verbose then
                log (Printf.sprintf "Tool %s (id: %s)" name id);

              emit (Event.Tool_call { id; name; input = call_summary });

              (* Print tool call with color *)
              Format.printf "  %s→%s %s@." Color.cyan Color.reset call_summary;
              last_was_tool := true

            method! on_tool_result result =
              (* Log tool results for observability *)
              let tool_use_id = Claude.Content_block.Tool_result.tool_use_id result in
              let is_error = Claude.Content_block.Tool_result.is_error result
                             |> Option.value ~default:false in
              let result_color = if is_error then Color.red else Color.green in
              let status = if is_error then "ERROR" else "ok" in
              Format.printf "    %s←%s %s@." result_color Color.reset status;
              emit (Event.Tool_result { id = tool_use_id; name = ""; output = status; is_error })

            method! on_complete result =
              let cost = Claude.Response.Complete.total_cost_usd result in
              emit (Event.Turn_complete { turn = iteration; cost_usd = cost })

            method! on_error err =
              let msg = Claude.Response.Error.message err in
              log (Printf.sprintf "Error: %s" msg);
              Format.printf "%s%sError: %s%s@." Color.bold Color.red msg Color.reset;
              emit (Event.Error (Printf.sprintf "[%s] %s" project msg))
          end in

          (* Same prompt every iteration - ralph-loop style *)
          Claude.Client.query client iteration_prompt;
          Claude.Client.run client ~handler
        with exn ->
          let msg = Printexc.to_string exn in
          log (Printf.sprintf "Exception: %s" msg);
          emit (Event.Error (Printf.sprintf "[%s] %s" project msg))
      end;

      (* Check if we should stop *)
      if !completion_detected then begin
        log "Completion promise detected!";
        Format.printf "@.%s%s✓ Completion promise detected%s@." Color.bold Color.green Color.reset;
        emit (Event.Text (Printf.sprintf "\n[%s] ✓ Completion promise detected.\n" project))
      end else
        ralph_loop (iteration + 1)
    end
  in

  ralph_loop 1;
  log "Ralph-loop complete";
  Format.printf "@.%s%s─── Project complete: %s ───%s@." Color.dim Color.blue project Color.reset;
  emit (Event.Text (Printf.sprintf "\n=== Ralph-loop complete for %s ===\n" project))

(* Main entry point *)
let run ~env ~config ~workspace_path () =
  Random.self_init ();

  let fs = Eio.Stdenv.fs env in
  let net = Eio.Stdenv.net env in

  (* Find unpac root *)
  let root = match find_root_from fs workspace_path with
    | Some r -> r
    | None ->
        Format.eprintf "Error: '%s' is not an unpac workspace.@." workspace_path;
        exit 1
  in

  Log.info (fun m -> m "Starting ralph-loop agent in workspace: %s" (snd root));

  (* Get projects to process *)
  let all_projects = Unpac.Worktree.list_projects ~proc_mgr:(Eio.Stdenv.process_mgr env) root in

  if all_projects = [] then begin
    Format.eprintf "No projects found in workspace.@.";
    exit 1
  end;

  let projects = match config.project with
    | Some p ->
        if List.mem p all_projects then [p]
        else begin
          Format.eprintf "Project '%s' not found. Available: %s@."
            p (String.concat ", " all_projects);
          exit 1
        end
    | None ->
        shuffle all_projects
  in

  Log.info (fun m -> m "Projects to process: %s" (String.concat ", " projects));

  (* Create shared event bus *)
  let event_bus = Event.create_bus () in

  Eio.Switch.run @@ fun sw ->

  (* Start web server if enabled *)
  (match config.web_port with
   | Some port ->
       let _web = Web.start ~sw ~net ~port event_bus in
       Log.info (fun m -> m "Web UI available at http://localhost:%d" port);
       Format.printf "Web UI: http://localhost:%d@." port
   | None -> ());

  Event.emit event_bus Event.Agent_start;

  Format.printf "%s%sRalph-loop agent starting...%s@." Color.bold Color.cyan Color.reset;
  Format.printf "  %sModel:%s Opus 4.5@." Color.dim Color.reset;
  Format.printf "  %sMax iterations:%s %d@." Color.dim Color.reset config.max_iterations;
  Format.printf "  %sCompletion promise:%s %s@." Color.dim Color.reset completion_promise;
  Format.printf "  %sCustom tools:%s mcp__unpac__* (via MCP server)@." Color.dim Color.reset;
  Format.printf "  %sProjects (%d):%s %s@."
    Color.dim (List.length projects) Color.reset (String.concat ", " projects);

  (* Process projects sequentially *)
  List.iter (fun project ->
    run_project_ralph_loop ~env ~config ~root ~project ~event_bus
  ) projects;

  Event.emit event_bus Event.Agent_stop;

  Format.printf "@.%s%s✓ All projects complete.%s@." Color.bold Color.green Color.reset
