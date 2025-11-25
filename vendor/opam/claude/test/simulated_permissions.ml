let src = Logs.Src.create "simulated_permissions" ~doc:"Simulated permission demonstration"
module Log = (val Logs.src_log src : Logs.LOG)

(* Track granted permissions *)
module PermissionState = struct
  module StringSet = Set.Make(String)
  let granted = ref StringSet.empty
  let denied = ref StringSet.empty
  
  let grant tool =
    granted := StringSet.add tool !granted;
    denied := StringSet.remove tool !denied
    
  let deny tool =
    denied := StringSet.add tool !denied;
    granted := StringSet.remove tool !granted
    
  let is_granted tool = StringSet.mem tool !granted
  let is_denied tool = StringSet.mem tool !denied
  
  let _reset () =
    granted := StringSet.empty;
    denied := StringSet.empty
    
  let show () =
    Log.app (fun m -> m "\n📊 Permission Status:");
    if StringSet.is_empty !granted && StringSet.is_empty !denied then
      Log.app (fun m -> m "  No permissions configured")
    else begin
      if not (StringSet.is_empty !granted) then
        Log.app (fun m -> m "  ✅ Granted: %s" 
          (StringSet.elements !granted |> String.concat ", "));
      if not (StringSet.is_empty !denied) then  
        Log.app (fun m -> m "  ❌ Denied: %s" 
          (StringSet.elements !denied |> String.concat ", "))
    end
end

(* Example permission callback *)
let example_permission_callback ~tool_name ~input:_ ~context:_ =
  Log.app (fun m -> m "\n🔐 Permission Request for: %s" tool_name);
  
  (* Check current state *)
  if PermissionState.is_granted tool_name then begin
    Log.app (fun m -> m "  → Auto-approved (previously granted)");
    Claude.Permissions.Result.allow ()
  end else if PermissionState.is_denied tool_name then begin
    Log.app (fun m -> m "  → Auto-denied (previously denied)");
    Claude.Permissions.Result.deny
      ~message:(Printf.sprintf "Tool %s is blocked by policy" tool_name)
      ~interrupt:false ()
  end else begin
    (* Ask user *)
    Printf.printf "  Allow %s? [y/n/always/never]: %!" tool_name;
    match read_line () |> String.lowercase_ascii with
    | "y" | "yes" ->
        Log.app (fun m -> m "  → Allowed (one time)");
        Claude.Permissions.Result.allow ()
    | "n" | "no" ->
        Log.app (fun m -> m "  → Denied (one time)");
        Claude.Permissions.Result.deny
          ~message:(Printf.sprintf "User denied %s" tool_name)
          ~interrupt:false ()
    | "a" | "always" ->
        PermissionState.grant tool_name;
        Log.app (fun m -> m "  → Allowed (always)");
        Claude.Permissions.Result.allow ()
    | "never" ->
        PermissionState.deny tool_name;
        Log.app (fun m -> m "  → Denied (always)");
        Claude.Permissions.Result.deny
          ~message:(Printf.sprintf "Tool %s permanently blocked" tool_name)
          ~interrupt:false ()
    | _ ->
        Log.app (fun m -> m "  → Denied (invalid response)");
        Claude.Permissions.Result.deny
          ~message:"Invalid permission response"
          ~interrupt:false ()
  end

(* Demonstrate the permission system *)
let demo_permissions () =
  Log.app (fun m -> m "🎭 Permission System Demonstration");
  Log.app (fun m -> m "==================================\n");
  
  (* Simulate permission requests *)
  let tools = ["Read"; "Write"; "Bash"; "Edit"] in
  let context = Claude.Permissions.Context.create () in
  
  Log.app (fun m -> m "This demo simulates permission requests.");
  Log.app (fun m -> m "You can respond with: y/n/always/never\n");
  
  (* Test each tool *)
  List.iter (fun tool ->
    let input =
      let open Jsont in
      Object ([
        (("file_path", Meta.none), String ("/example/path.txt", Meta.none))
      ], Meta.none)
    in
    let result = example_permission_callback
      ~tool_name:tool ~input ~context in
    
    (* Show result *)
    (match result with
    | Claude.Permissions.Result.Allow _ ->
        Log.info (fun m -> m "Result: Permission granted for %s" tool)
    | Claude.Permissions.Result.Deny { message; _ } ->
        Log.info (fun m -> m "Result: Permission denied for %s - %s" tool message))
  ) tools;
  
  (* Show final state *)
  PermissionState.show ()

(* Also demonstrate discovery callback *)
let demo_discovery () =
  Log.app (fun m -> m "\n\n🔍 Discovery Callback Demonstration");
  Log.app (fun m -> m "====================================\n");
  
  let discovered = ref [] in
  let callback = Claude.Permissions.discovery_callback discovered in
  
  (* Simulate some tool requests *)
  let requests =
    let open Jsont in
    [
      ("Read", Object ([
        (("file_path", Meta.none), String ("test.ml", Meta.none))
      ], Meta.none));
      ("Bash", Object ([
        (("command", Meta.none), String ("ls -la", Meta.none))
      ], Meta.none));
      ("Write", Object ([
        (("file_path", Meta.none), String ("output.txt", Meta.none))
      ], Meta.none));
    ]
  in
  
  Log.app (fun m -> m "Simulating tool requests with discovery callback...\n");
  
  List.iter (fun (tool, input) ->
    Log.app (fun m -> m "  Request: %s" tool);
    let context = Claude.Permissions.Context.create () in
    let _ = callback ~tool_name:tool ~input ~context in
    ()
  ) requests;
  
  Log.app (fun m -> m "\n📋 Discovered permissions:");
  if !discovered = [] then
    Log.app (fun m -> m "  None")
  else
    List.iter (fun rule ->
      Log.app (fun m -> m "  - %s%s" 
        (Claude.Permissions.Rule.tool_name rule)
        (match Claude.Permissions.Rule.rule_content rule with
         | Some content -> Printf.sprintf " (content: %s)" content  
         | None -> ""))
    ) !discovered

let main () =
  demo_permissions ();
  demo_discovery ()

(* Command-line interface *)
open Cmdliner

let main_term =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    if level = None then Logs.set_level (Some Logs.App)
  in
  let run style level =
    setup_log style level;
    main ()
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Demonstrate permission callbacks and discovery" in
  let man = [
    `S Manpage.s_description;
    `P "This program demonstrates how permission callbacks work in the Claude OCaml library.";
    `P "It simulates permission requests and shows how to implement custom callbacks.";
  ] in
  let info = Cmd.info "simulated_permissions" ~version:"1.0" ~doc ~man in
  Cmd.v info main_term

let () = exit (Cmd.eval cmd)