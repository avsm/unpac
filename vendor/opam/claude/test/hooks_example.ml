open Eio.Std

let src = Logs.Src.create "hooks_example" ~doc:"Hooks example"

module Log = (val Logs.src_log src : Logs.LOG)

(* Example 1: Block dangerous bash commands *)
let block_dangerous_bash ~input ~tool_use_id:_ ~context:_ =
  let hook = Claude.Hooks.PreToolUse.of_json input in
  let tool_name = Claude.Hooks.PreToolUse.tool_name hook in

  if tool_name = "Bash" then
    let tool_input = Claude.Hooks.PreToolUse.tool_input hook in
    match Test_json_utils.get_string tool_input "command" with
    | Some command ->
        if String.length command >= 6 && String.sub command 0 6 = "rm -rf" then begin
          Log.app (fun m -> m "🚫 Blocked dangerous command: %s" command);
          let output =
            Claude.Hooks.PreToolUse.deny
              ~reason:"Command contains dangerous 'rm -rf' pattern" ()
          in
          Claude.Hooks.continue
            ~system_message:"Blocked dangerous rm -rf command"
            ~hook_specific_output:
              (Claude.Hooks.PreToolUse.output_to_json output)
            ()
        end
        else Claude.Hooks.continue ()
    | _ -> Claude.Hooks.continue ()
  else Claude.Hooks.continue ()

(* Example 2: Log all tool usage *)
let log_tool_usage ~input ~tool_use_id ~context:_ =
  let hook = Claude.Hooks.PreToolUse.of_json input in
  let tool_name = Claude.Hooks.PreToolUse.tool_name hook in
  let tool_use_id_str = Option.value tool_use_id ~default:"<none>" in
  Log.app (fun m -> m "📝 Tool %s called (ID: %s)" tool_name tool_use_id_str);
  Claude.Hooks.continue ()

let run_example ~sw ~env =
  Log.app (fun m -> m "🔧 Hooks System Example");
  Log.app (fun m -> m "====================\n");

  (* Configure hooks *)
  let hooks =
    Claude.Hooks.empty
    |> Claude.Hooks.add Claude.Hooks.Pre_tool_use
         [
           (* Log all tool usage *)
           Claude.Hooks.matcher [ log_tool_usage ];
           (* Block dangerous bash commands *)
           Claude.Hooks.matcher ~pattern:"Bash" [ block_dangerous_bash ];
         ]
  in

  let options =
    Claude.Options.create ~model:(Claude.Model.of_string "sonnet") ~hooks ()
  in

  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr ()
  in

  (* Test 1: Safe command (should work) *)
  Log.app (fun m -> m "Test 1: Safe bash command");
  Claude.Client.query client "Run the bash command: echo 'Hello from hooks!'";

  let messages = Claude.Client.receive_all client in
  List.iter
    (fun msg ->
      match msg with
      | Claude.Message.Assistant msg ->
          List.iter
            (function
              | Claude.Content_block.Text t ->
                  let text = Claude.Content_block.Text.text t in
                  if String.length text > 0 then
                    Log.app (fun m -> m "Claude: %s" text)
              | _ -> ())
            (Claude.Message.Assistant.content msg)
      | Claude.Message.Result msg ->
          if Claude.Message.Result.is_error msg then
            Log.err (fun m -> m "❌ Error!")
          else Log.app (fun m -> m "✅ Test 1 complete\n")
      | _ -> ())
    messages;

  (* Test 2: Dangerous command (should be blocked) *)
  Log.app (fun m -> m "Test 2: Dangerous bash command (should be blocked)");
  Claude.Client.query client "Run the bash command: rm -rf /tmp/test";

  let messages = Claude.Client.receive_all client in
  List.iter
    (fun msg ->
      match msg with
      | Claude.Message.Assistant msg ->
          List.iter
            (function
              | Claude.Content_block.Text t ->
                  let text = Claude.Content_block.Text.text t in
                  if String.length text > 0 then
                    Log.app (fun m -> m "Claude: %s" text)
              | _ -> ())
            (Claude.Message.Assistant.content msg)
      | Claude.Message.Result msg ->
          if Claude.Message.Result.is_error msg then
            Log.err (fun m -> m "❌ Error!")
          else Log.app (fun m -> m "✅ Test 2 complete")
      | _ -> ())
    messages;

  Log.app (fun m -> m "\n====================");
  Log.app (fun m -> m "✨ Example complete!")

let main ~env = Switch.run @@ fun sw -> run_example ~sw ~env

(* Command-line interface *)
open Cmdliner

let main_term env =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    if level = None then Logs.set_level (Some Logs.App);
    match level with
    | Some Logs.Info | Some Logs.Debug ->
        Logs.Src.set_level Claude.Client.src (Some Logs.Info)
    | _ -> ()
  in
  let run style level =
    setup_log style level;
    main ~env
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd env =
  let doc = "Demonstrate Claude's hooks system" in
  let info = Cmd.info "hooks_example" ~version:"1.0" ~doc in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
