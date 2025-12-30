(** Structured audit logging for unpac operations. *)

let src = Logs.Src.create "unpac.audit" ~doc:"Audit logging"
module Log = (val Logs.src_log src : Logs.LOG)

(* Git operation types *)

type git_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

type git_operation = {
  git_id : string;
  git_timestamp : float;
  git_cmd : string list;
  git_cwd : string;
  git_duration_ms : int;
  git_result : git_result;
}

(* Unpac operation types *)

type status =
  | Success
  | Failed of string
  | Conflict of string list

type operation_type =
  | Init
  | Project_new
  | Project_promote
  | Project_set_remote
  | Opam_add
  | Opam_init
  | Opam_promote
  | Opam_update
  | Opam_merge
  | Opam_edit
  | Opam_done
  | Opam_remove
  | Git_add
  | Git_update
  | Git_merge
  | Git_remove
  | Push
  | Unknown of string

type operation = {
  id : string;
  timestamp : float;
  operation_type : operation_type;
  args : string list;
  cwd : string;
  duration_ms : int;
  status : status;
  git_operations : git_operation list;
}

type log = {
  version : string;
  entries : operation list;
}

let current_version = "1.0"

(* UUID generation - simple random hex *)
let () = Random.self_init ()

let generate_id () =
  let buf = Buffer.create 32 in
  for _ = 1 to 8 do
    Buffer.add_string buf (Printf.sprintf "%04x" (Random.int 0x10000))
  done;
  let s = Buffer.contents buf in
  (* Format as UUID: 8-4-4-4-12 *)
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub s 0 8)
    (String.sub s 8 4)
    (String.sub s 12 4)
    (String.sub s 16 4)
    (String.sub s 20 12)

(* JSON codecs *)

let git_result_jsont =
  Jsont.Object.map
    ~kind:"git_result"
    (fun exit_code stdout stderr -> { exit_code; stdout; stderr })
  |> Jsont.Object.mem "exit_code" Jsont.int ~enc:(fun r -> r.exit_code)
  |> Jsont.Object.mem "stdout" Jsont.string ~enc:(fun r -> r.stdout)
  |> Jsont.Object.mem "stderr" Jsont.string ~enc:(fun r -> r.stderr)
  |> Jsont.Object.finish

let git_operation_jsont =
  Jsont.Object.map
    ~kind:"git_operation"
    (fun git_id git_timestamp git_cmd git_cwd git_duration_ms git_result ->
       { git_id; git_timestamp; git_cmd; git_cwd; git_duration_ms; git_result })
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun g -> g.git_id)
  |> Jsont.Object.mem "timestamp" Jsont.number ~enc:(fun g -> g.git_timestamp)
  |> Jsont.Object.mem "cmd" (Jsont.list Jsont.string) ~enc:(fun g -> g.git_cmd)
  |> Jsont.Object.mem "cwd" Jsont.string ~enc:(fun g -> g.git_cwd)
  |> Jsont.Object.mem "duration_ms" Jsont.int ~enc:(fun g -> g.git_duration_ms)
  |> Jsont.Object.mem "result" git_result_jsont ~enc:(fun g -> g.git_result)
  |> Jsont.Object.finish

let status_jsont =
  (* Encode status as a simple object with status field and optional data *)
  Jsont.Object.map ~kind:"status"
    (fun status data_opt ->
       match status, data_opt with
       | "success", _ -> Success
       | "failed", Some msg -> Failed msg
       | "conflict", Some files_str ->
           Conflict (String.split_on_char ',' files_str)
       | s, _ -> Failed (Printf.sprintf "Unknown status: %s" s))
  |> Jsont.Object.mem "status" Jsont.string
       ~enc:(function
         | Success -> "success"
         | Failed _ -> "failed"
         | Conflict _ -> "conflict")
  |> Jsont.Object.opt_mem "data" Jsont.string
       ~enc:(function
         | Success -> None
         | Failed msg -> Some msg
         | Conflict files -> Some (String.concat "," files))
  |> Jsont.Object.finish

let operation_type_to_string = function
  | Init -> "init"
  | Project_new -> "project.new"
  | Project_promote -> "project.promote"
  | Project_set_remote -> "project.set-remote"
  | Opam_add -> "opam.add"
  | Opam_init -> "opam.init"
  | Opam_promote -> "opam.promote"
  | Opam_update -> "opam.update"
  | Opam_merge -> "opam.merge"
  | Opam_edit -> "opam.edit"
  | Opam_done -> "opam.done"
  | Opam_remove -> "opam.remove"
  | Git_add -> "git.add"
  | Git_update -> "git.update"
  | Git_merge -> "git.merge"
  | Git_remove -> "git.remove"
  | Push -> "push"
  | Unknown s -> s

let operation_type_of_string = function
  | "init" -> Init
  | "project.new" -> Project_new
  | "project.promote" -> Project_promote
  | "project.set-remote" -> Project_set_remote
  | "opam.add" -> Opam_add
  | "opam.init" -> Opam_init
  | "opam.promote" -> Opam_promote
  | "opam.update" -> Opam_update
  | "opam.merge" -> Opam_merge
  | "opam.edit" -> Opam_edit
  | "opam.done" -> Opam_done
  | "opam.remove" -> Opam_remove
  | "git.add" -> Git_add
  | "git.update" -> Git_update
  | "git.merge" -> Git_merge
  | "git.remove" -> Git_remove
  | "push" -> Push
  | s -> Unknown s

let operation_type_jsont =
  Jsont.string
  |> Jsont.map ~dec:operation_type_of_string ~enc:operation_type_to_string

let operation_jsont =
  Jsont.Object.map
    ~kind:"operation"
    (fun id timestamp operation_type args cwd duration_ms status git_operations ->
       { id; timestamp; operation_type; args; cwd; duration_ms; status; git_operations })
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun o -> o.id)
  |> Jsont.Object.mem "timestamp" Jsont.number ~enc:(fun o -> o.timestamp)
  |> Jsont.Object.mem "operation" operation_type_jsont ~enc:(fun o -> o.operation_type)
  |> Jsont.Object.mem "args" (Jsont.list Jsont.string) ~enc:(fun o -> o.args)
  |> Jsont.Object.mem "cwd" Jsont.string ~enc:(fun o -> o.cwd)
  |> Jsont.Object.mem "duration_ms" Jsont.int ~enc:(fun o -> o.duration_ms)
  |> Jsont.Object.mem "status" status_jsont ~enc:(fun o -> o.status)
  |> Jsont.Object.mem "git_operations" (Jsont.list git_operation_jsont)
       ~enc:(fun o -> o.git_operations)
  |> Jsont.Object.finish

let log_jsont =
  Jsont.Object.map
    ~kind:"audit_log"
    (fun version entries -> { version; entries })
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun l -> l.version)
  |> Jsont.Object.mem "entries" (Jsont.list operation_jsont) ~enc:(fun l -> l.entries)
  |> Jsont.Object.finish

(* Context for accumulating git operations *)

type context = {
  ctx_id : string;
  ctx_operation_type : operation_type;
  ctx_args : string list;
  ctx_cwd : string;
  ctx_start : float;
  mutable ctx_git_ops : git_operation list;
}

let start_operation ~operation_type ~args ~cwd =
  let ctx = {
    ctx_id = generate_id ();
    ctx_operation_type = operation_type;
    ctx_args = args;
    ctx_cwd = cwd;
    ctx_start = Unix.gettimeofday ();
    ctx_git_ops = [];
  } in
  Log.debug (fun m -> m "Starting operation %s: %s %a"
    ctx.ctx_id (operation_type_to_string operation_type)
    Fmt.(list ~sep:sp string) args);
  ctx

let record_git ctx ~cmd ~cwd ~started ~result =
  let now = Unix.gettimeofday () in
  let duration_ms = int_of_float ((now -. started) *. 1000.0) in
  let op = {
    git_id = generate_id ();
    git_timestamp = started;
    git_cmd = cmd;
    git_cwd = cwd;
    git_duration_ms = duration_ms;
    git_result = result;
  } in
  ctx.ctx_git_ops <- op :: ctx.ctx_git_ops;
  Log.debug (fun m -> m "Recorded git: %a (exit %d, %dms)"
    Fmt.(list ~sep:sp string) cmd result.exit_code duration_ms)

let finalize_operation ctx status =
  let now = Unix.gettimeofday () in
  let duration_ms = int_of_float ((now -. ctx.ctx_start) *. 1000.0) in
  let op = {
    id = ctx.ctx_id;
    timestamp = ctx.ctx_start;
    operation_type = ctx.ctx_operation_type;
    args = ctx.ctx_args;
    cwd = ctx.ctx_cwd;
    duration_ms;
    status;
    git_operations = List.rev ctx.ctx_git_ops;
  } in
  Log.info (fun m -> m "Completed operation %s in %dms" ctx.ctx_id duration_ms);
  op

let complete_success ctx = finalize_operation ctx Success

let complete_failed ctx ~error =
  Log.warn (fun m -> m "Operation %s failed: %s" ctx.ctx_id error);
  finalize_operation ctx (Failed error)

let complete_conflict ctx ~files =
  Log.warn (fun m -> m "Operation %s had conflicts in %d files" ctx.ctx_id (List.length files));
  finalize_operation ctx (Conflict files)

(* Log file management *)

let default_log_file = ".unpac-audit.json"

let load path =
  if not (Sys.file_exists path) then
    Ok { version = current_version; entries = [] }
  else
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      match Jsont_bytesrw.decode_string' log_jsont content with
      | Ok log -> Ok log
      | Error e -> Error (Printf.sprintf "Parse error: %s" (Jsont.Error.to_string e))
    with
    | Sys_error msg -> Error msg

let save path log =
  try
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent log_jsont log with
    | Ok content ->
        let oc = open_out path in
        output_string oc content;
        close_out oc;
        Ok ()
    | Error e -> Error (Printf.sprintf "Encode error: %s" e)
  with
  | Sys_error msg -> Error msg

let append path op =
  match load path with
  | Error e -> Error e
  | Ok log ->
      let log' = { log with entries = op :: log.entries } in
      save path log'

(* Pretty printing *)

let pp_status fmt = function
  | Success -> Format.fprintf fmt "@{<green>SUCCESS@}"
  | Failed msg -> Format.fprintf fmt "@{<red>FAILED@}: %s" msg
  | Conflict files ->
      Format.fprintf fmt "@{<yellow>CONFLICT@}: %a"
        Fmt.(list ~sep:comma string) files

let pp_git_operation fmt op =
  let status_color = if op.git_result.exit_code = 0 then "green" else "red" in
  Format.fprintf fmt "  @{<%s>[%d]@} git %a (%dms)@."
    status_color op.git_result.exit_code
    Fmt.(list ~sep:sp string) op.git_cmd
    op.git_duration_ms

let pp_operation fmt op =
  let tm = Unix.localtime op.timestamp in
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "[%04d-%02d-%02d %02d:%02d:%02d] %s %a@."
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (operation_type_to_string op.operation_type)
    Fmt.(list ~sep:sp string) op.args;
  Format.fprintf fmt "  ID: %s | Duration: %dms@." op.id op.duration_ms;
  Format.fprintf fmt "  Status: %a@." pp_status op.status;
  if op.git_operations <> [] then begin
    Format.fprintf fmt "  Git operations (%d):@." (List.length op.git_operations);
    List.iter (pp_git_operation fmt) op.git_operations
  end;
  Format.fprintf fmt "@]"

let pp_log fmt log =
  Format.fprintf fmt "@[<v>Unpac Audit Log (version %s)@." log.version;
  Format.fprintf fmt "Total operations: %d@.@." (List.length log.entries);
  List.iter (fun op ->
    pp_operation fmt op;
    Format.fprintf fmt "@."
  ) log.entries;
  Format.fprintf fmt "@]"

(* HTML generation *)

let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* Commit audit log to git *)

let commit_log ~proc_mgr ~main_wt ~log_path =
  (* Stage the audit log *)
  let rel_path = Filename.basename log_path in
  let started = Unix.gettimeofday () in
  let result =
    try
      (* Add the file *)
      Eio.Switch.run @@ fun sw ->
      let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
      let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in
      let child = Eio.Process.spawn proc_mgr ~sw
          ~cwd:(main_wt :> Eio.Fs.dir_ty Eio.Path.t)
          ~stdout:stdout_w ~stderr:stderr_w
          ["git"; "add"; rel_path]
      in
      Eio.Flow.close stdout_w;
      Eio.Flow.close stderr_w;
      (* Drain outputs *)
      let stdout_buf = Buffer.create 64 in
      let stderr_buf = Buffer.create 64 in
      Eio.Fiber.both
        (fun () ->
           try
             while true do
               let chunk = Cstruct.create 1024 in
               let n = Eio.Flow.single_read stdout_r chunk in
               Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n))
             done
           with End_of_file -> ())
        (fun () ->
           try
             while true do
               let chunk = Cstruct.create 1024 in
               let n = Eio.Flow.single_read stderr_r chunk in
               Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n))
             done
           with End_of_file -> ());
      let status = Eio.Process.await child in
      match status with
      | `Exited 0 -> Ok ()
      | `Exited code -> Error (Printf.sprintf "git add failed (exit %d): %s" code (Buffer.contents stderr_buf))
      | `Signaled sig_ -> Error (Printf.sprintf "git add killed by signal %d" sig_)
    with exn -> Error (Printf.sprintf "Exception: %s" (Printexc.to_string exn))
  in
  match result with
  | Error e ->
      Log.warn (fun m -> m "Failed to stage audit log: %s" e);
      Error e
  | Ok () ->
      (* Commit the file *)
      let result =
        try
          Eio.Switch.run @@ fun sw ->
          let stdout_r, stdout_w = Eio.Process.pipe proc_mgr ~sw in
          let stderr_r, stderr_w = Eio.Process.pipe proc_mgr ~sw in
          let child = Eio.Process.spawn proc_mgr ~sw
              ~cwd:(main_wt :> Eio.Fs.dir_ty Eio.Path.t)
              ~stdout:stdout_w ~stderr:stderr_w
              ["git"; "commit"; "-m"; "Update audit log"; "--no-verify"]
          in
          Eio.Flow.close stdout_w;
          Eio.Flow.close stderr_w;
          (* Drain outputs *)
          let stdout_buf = Buffer.create 64 in
          let stderr_buf = Buffer.create 64 in
          Eio.Fiber.both
            (fun () ->
               try
                 while true do
                   let chunk = Cstruct.create 1024 in
                   let n = Eio.Flow.single_read stdout_r chunk in
                   Buffer.add_string stdout_buf (Cstruct.to_string (Cstruct.sub chunk 0 n))
                 done
               with End_of_file -> ())
            (fun () ->
               try
                 while true do
                   let chunk = Cstruct.create 1024 in
                   let n = Eio.Flow.single_read stderr_r chunk in
                   Buffer.add_string stderr_buf (Cstruct.to_string (Cstruct.sub chunk 0 n))
                 done
               with End_of_file -> ());
          let status = Eio.Process.await child in
          match status with
          | `Exited 0 -> Ok ()
          | `Exited 1 when String.length (Buffer.contents stdout_buf) > 0 &&
                           (String.exists (fun c -> c = 'n') (Buffer.contents stdout_buf)) ->
              (* "nothing to commit" - this is fine *)
              Ok ()
          | `Exited code -> Error (Printf.sprintf "git commit failed (exit %d): %s" code (Buffer.contents stderr_buf))
          | `Signaled sig_ -> Error (Printf.sprintf "git commit killed by signal %d" sig_)
        with exn -> Error (Printf.sprintf "Exception: %s" (Printexc.to_string exn))
      in
      let duration = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
      (match result with
       | Ok () -> Log.debug (fun m -> m "Committed audit log (%dms)" duration)
       | Error e -> Log.warn (fun m -> m "Failed to commit audit log: %s" e));
      result

(** Full audit manager that wraps operations *)
type manager = {
  proc_mgr : [ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t;
  main_wt : Eio.Fs.dir_ty Eio.Path.t;
  log_path : string;
  mutable current_ctx : context option;
}

let create_manager ~proc_mgr ~main_wt =
  let log_path = Eio.Path.(main_wt / default_log_file) |> snd in
  { proc_mgr; main_wt; log_path; current_ctx = None }

let begin_operation mgr ~operation_type ~args =
  let cwd = snd mgr.main_wt in
  let ctx = start_operation ~operation_type ~args ~cwd in
  mgr.current_ctx <- Some ctx;
  ctx

let end_operation mgr status =
  match mgr.current_ctx with
  | None ->
      Log.warn (fun m -> m "end_operation called without active context");
      Error "No active operation"
  | Some ctx ->
      mgr.current_ctx <- None;
      let op = finalize_operation ctx status in
      (* Append to log file *)
      (match append mgr.log_path op with
       | Error e ->
           Log.err (fun m -> m "Failed to append to audit log: %s" e);
           Error e
       | Ok () ->
           (* Commit the log *)
           match commit_log ~proc_mgr:mgr.proc_mgr ~main_wt:mgr.main_wt ~log_path:mgr.log_path with
           | Error e ->
               Log.warn (fun m -> m "Failed to commit audit log (will retry next operation): %s" e);
               Ok op  (* Still return success - the log is saved, just not committed *)
           | Ok () ->
               Ok op)

let end_success mgr = end_operation mgr Success
let end_failed mgr ~error = end_operation mgr (Failed error)
let end_conflict mgr ~files = end_operation mgr (Conflict files)

let get_context mgr = mgr.current_ctx

let to_html log =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  add {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Unpac Audit Log</title>
  <style>
    :root {
      --bg: #1a1a2e;
      --card: #16213e;
      --text: #e4e4e4;
      --accent: #0f3460;
      --success: #4ecca3;
      --error: #e94560;
      --warning: #f39c12;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: var(--bg);
      color: var(--text);
      margin: 0;
      padding: 20px;
      line-height: 1.6;
    }
    h1 { color: var(--success); margin-bottom: 10px; }
    .meta { color: #888; margin-bottom: 30px; }
    .operation {
      background: var(--card);
      border-radius: 8px;
      padding: 20px;
      margin-bottom: 20px;
      border-left: 4px solid var(--accent);
    }
    .operation.success { border-left-color: var(--success); }
    .operation.failed { border-left-color: var(--error); }
    .operation.conflict { border-left-color: var(--warning); }
    .op-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 10px;
    }
    .op-type {
      font-weight: bold;
      font-size: 1.1em;
      color: var(--success);
    }
    .op-time { color: #888; font-size: 0.9em; }
    .op-args { font-family: monospace; color: #888; margin: 5px 0; }
    .status {
      display: inline-block;
      padding: 2px 8px;
      border-radius: 4px;
      font-size: 0.85em;
      font-weight: bold;
    }
    .status.success { background: var(--success); color: #000; }
    .status.failed { background: var(--error); color: #fff; }
    .status.conflict { background: var(--warning); color: #000; }
    .git-ops {
      margin-top: 15px;
      padding-top: 15px;
      border-top: 1px solid var(--accent);
    }
    .git-ops summary {
      cursor: pointer;
      color: #888;
    }
    .git-op {
      font-family: monospace;
      font-size: 0.9em;
      padding: 5px 10px;
      margin: 5px 0;
      background: var(--accent);
      border-radius: 4px;
    }
    .git-op.error { border-left: 3px solid var(--error); }
    .git-cmd { color: var(--success); }
    .git-exit { color: #888; }
    .git-duration { color: #888; float: right; }
  </style>
</head>
<body>
  <h1>Unpac Audit Log</h1>
  <div class="meta">Version |};
  add (html_escape log.version);
  add {| | |};
  add (string_of_int (List.length log.entries));
  add {| operations</div>
|};
  List.iter (fun op ->
    let status_class = match op.status with
      | Success -> "success"
      | Failed _ -> "failed"
      | Conflict _ -> "conflict"
    in
    let tm = Unix.localtime op.timestamp in
    add (Printf.sprintf {|  <div class="operation %s">
    <div class="op-header">
      <span class="op-type">%s</span>
      <span class="op-time">%04d-%02d-%02d %02d:%02d:%02d (%dms)</span>
    </div>
    <div class="op-args">%s</div>
    <span class="status %s">%s</span>
|}
      status_class
      (html_escape (operation_type_to_string op.operation_type))
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      op.duration_ms
      (html_escape (String.concat " " op.args))
      status_class
      (match op.status with
       | Success -> "SUCCESS"
       | Failed msg -> "FAILED: " ^ html_escape msg
       | Conflict files -> "CONFLICT: " ^ html_escape (String.concat ", " files)));
    if op.git_operations <> [] then begin
      add {|    <div class="git-ops">
      <details>
        <summary>|};
      add (string_of_int (List.length op.git_operations));
      add {| git operations</summary>
|};
      List.iter (fun git_op ->
        let error_class = if git_op.git_result.exit_code <> 0 then " error" else "" in
        add (Printf.sprintf {|        <div class="git-op%s">
          <span class="git-cmd">git %s</span>
          <span class="git-duration">%dms</span>
          <span class="git-exit">[exit %d]</span>
        </div>
|}
          error_class
          (html_escape (String.concat " " git_op.git_cmd))
          git_op.git_duration_ms
          git_op.git_result.exit_code)
      ) op.git_operations;
      add {|      </details>
    </div>
|}
    end;
    add {|  </div>
|}
  ) log.entries;
  add {|</body>
</html>
|};
  Buffer.contents buf
