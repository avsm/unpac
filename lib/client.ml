let src = Logs.Src.create "claude.client" ~doc:"Claude client"
module Log = (val Logs.src_log src : Logs.LOG)

(** Control response builders using Sdk_control codecs *)
module Control_response = struct
  let success ~request_id ~response =
    let resp = Sdk_control.Response.success ~request_id ?response () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    match Jsont.Json.encode Sdk_control.jsont ctrl with
    | Ok json -> json
    | Error msg -> failwith ("Control_response.success: " ^ msg)

  let error ~request_id ~message =
    let resp = Sdk_control.Response.error ~request_id ~error:message () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    match Jsont.Json.encode Sdk_control.jsont ctrl with
    | Ok json -> json
    | Error msg -> failwith ("Control_response.error: " ^ msg)
end

(* Helper functions for JSON manipulation using jsont *)
let json_to_string json =
  match Jsont_bytesrw.encode_string' Jsont.json json with
  | Ok s -> s
  | Error err -> failwith (Jsont.Error.to_string err)

(** Wire-level codec for permission responses to CLI.
    Uses camelCase field names as expected by the CLI protocol. *)
module Permission_wire = struct
  type allow = { allow_behavior : string; allow_updated_input : Jsont.json }
  type deny = { deny_behavior : string; deny_message : string }

  let allow_jsont : allow Jsont.t =
    let make allow_behavior allow_updated_input = { allow_behavior; allow_updated_input } in
    Jsont.Object.map ~kind:"AllowWire" make
    |> Jsont.Object.mem "behavior" Jsont.string ~enc:(fun r -> r.allow_behavior)
    |> Jsont.Object.mem "updatedInput" Jsont.json ~enc:(fun r -> r.allow_updated_input)
    |> Jsont.Object.finish

  let deny_jsont : deny Jsont.t =
    let make deny_behavior deny_message = { deny_behavior; deny_message } in
    Jsont.Object.map ~kind:"DenyWire" make
    |> Jsont.Object.mem "behavior" Jsont.string ~enc:(fun r -> r.deny_behavior)
    |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.deny_message)
    |> Jsont.Object.finish

  let encode_allow ~updated_input =
    match Jsont.Json.encode allow_jsont { allow_behavior = "allow"; allow_updated_input = updated_input } with
    | Ok json -> json
    | Error msg -> failwith ("Permission_wire.encode_allow: " ^ msg)

  let encode_deny ~message =
    match Jsont.Json.encode deny_jsont { deny_behavior = "deny"; deny_message = message } with
    | Ok json -> json
    | Error msg -> failwith ("Permission_wire.encode_deny: " ^ msg)
end

(** Wire-level codec for hook matcher configuration sent to CLI. *)
module Hook_matcher_wire = struct
  type t = {
    matcher : string option;
    hook_callback_ids : string list;
  }

  let jsont : t Jsont.t =
    let make matcher hook_callback_ids = { matcher; hook_callback_ids } in
    Jsont.Object.map ~kind:"HookMatcherWire" make
    |> Jsont.Object.opt_mem "matcher" Jsont.string ~enc:(fun r -> r.matcher)
    |> Jsont.Object.mem "hookCallbackIds" (Jsont.list Jsont.string) ~enc:(fun r -> r.hook_callback_ids)
    |> Jsont.Object.finish

  let encode matchers =
    Jsont.Json.list (List.map (fun m ->
      match Jsont.Json.encode jsont m with
      | Ok json -> json
      | Error msg -> failwith ("Hook_matcher_wire.encode: " ^ msg)
    ) matchers)
end

type t = {
  transport : Transport.t;
  permission_callback : Permissions.callback option;
  permission_log : Permissions.Rule.t list ref option;
  hook_callbacks : (string, Hooks.callback) Hashtbl.t;
  mutable session_id : string option;
  control_responses : (string, Jsont.json) Hashtbl.t;
  control_mutex : Eio.Mutex.t;
  control_condition : Eio.Condition.t;
}

let session_id t = t.session_id

let handle_control_request t (ctrl_req : Incoming.Control_request.t) =
  let request_id = Incoming.Control_request.request_id ctrl_req in
  Log.info (fun m -> m "Handling control request: %s" (Incoming.Control_request.subtype ctrl_req));

  match Incoming.Control_request.request ctrl_req with
  | Incoming.Control_request.Can_use_tool req ->
      let tool_name = Incoming.Control_request.Can_use_tool.tool_name req in
      let input = Incoming.Control_request.Can_use_tool.input req in
      Log.info (fun m -> m "Permission request for tool '%s' with input: %s"
        tool_name (json_to_string input));
      (* TODO: Parse permission_suggestions properly *)
      let context = Permissions.Context.create ~suggestions:[] () in

      Log.info (fun m -> m "Invoking permission callback for tool: %s" tool_name);
      let result = match t.permission_callback with
        | Some callback ->
            Log.info (fun m -> m "Using custom permission callback");
            callback ~tool_name ~input ~context
        | None ->
            Log.info (fun m -> m "Using default allow callback");
            Permissions.default_allow_callback ~tool_name ~input ~context
      in
      Log.info (fun m -> m "Permission callback returned: %s"
        (match result with
         | Permissions.Result.Allow _ -> "ALLOW"
         | Permissions.Result.Deny _ -> "DENY"));

      (* Convert permission result to CLI format using wire codec *)
      let response_data = match result with
        | Permissions.Result.Allow { updated_input; updated_permissions = _; unknown = _ } ->
            let updated_input = Option.value updated_input ~default:input in
            Permission_wire.encode_allow ~updated_input
        | Permissions.Result.Deny { message; interrupt = _; unknown = _ } ->
            Permission_wire.encode_deny ~message
      in
      let response = Control_response.success ~request_id ~response:(Some response_data) in
      Log.info (fun m -> m "Sending control response: %s" (json_to_string response));
      Transport.send t.transport response

  | Incoming.Control_request.Hook_callback req ->
      let callback_id = Incoming.Control_request.Hook_callback.callback_id req in
      let input = Incoming.Control_request.Hook_callback.input req in
      let tool_use_id = Incoming.Control_request.Hook_callback.tool_use_id req in
      Log.info (fun m -> m "Hook callback request for callback_id: %s" callback_id);

      (try
        let callback = Hashtbl.find t.hook_callbacks callback_id in
        let context = Hooks.Context.create () in
        let result = callback ~input ~tool_use_id ~context in

        let result_json = match Jsont.Json.encode Hooks.result_jsont result with
          | Ok j -> j
          | Error msg -> failwith ("Failed to encode hook result: " ^ msg)
        in
        let response = Control_response.success ~request_id ~response:(Some result_json) in
        Log.info (fun m -> m "Hook callback succeeded, sending response");
        Transport.send t.transport response
      with
      | Not_found ->
          let error_msg = Printf.sprintf "Hook callback not found: %s" callback_id in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport (Control_response.error ~request_id ~message:error_msg)
      | exn ->
          let error_msg = Printf.sprintf "Hook callback error: %s" (Printexc.to_string exn) in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport (Control_response.error ~request_id ~message:error_msg))

  | Incoming.Control_request.Unknown (subtype, _) ->
      let error_msg = Printf.sprintf "Unsupported control request: %s" subtype in
      Transport.send t.transport (Control_response.error ~request_id ~message:error_msg)

let handle_control_response t control_resp =
  let request_id = match control_resp.Sdk_control.response with
    | Sdk_control.Response.Success s -> s.request_id
    | Sdk_control.Response.Error e -> e.request_id
  in
  Log.debug (fun m -> m "Received control response for request_id: %s" request_id);

  (* Store the response as JSON and signal waiting threads *)
  let json = match Jsont.Json.encode Sdk_control.control_response_jsont control_resp with
    | Ok j -> j
    | Error err -> failwith ("Failed to encode control response: " ^ err)
  in
  Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
    Hashtbl.replace t.control_responses request_id json;
    Eio.Condition.broadcast t.control_condition
  )

let handle_messages t =
  let rec loop () =
    match Transport.receive_line t.transport with
    | None ->
        (* EOF *)
        Log.debug (fun m -> m "Handle messages: EOF received");
        Seq.Nil
    | Some line ->
        (* Use unified Incoming codec for all message types *)
        match Jsont_bytesrw.decode_string' Incoming.jsont line with
        | Ok (Incoming.Message msg) ->
            Log.info (fun m -> m "← %a" Message.pp msg);

            (* Extract session ID from system messages *)
            (match msg with
            | Message.System sys ->
                (match Message.System.session_id sys with
                | Some session_id ->
                  t.session_id <- Some session_id;
                  Log.debug (fun m -> m "Stored session ID: %s" session_id)
                | None -> ())
            | _ -> ());

            Seq.Cons (msg, loop)

        | Ok (Incoming.Control_response resp) ->
            handle_control_response t resp;
            loop ()

        | Ok (Incoming.Control_request ctrl_req) ->
            Log.info (fun m -> m "Received control request: %s (request_id: %s)"
              (Incoming.Control_request.subtype ctrl_req)
              (Incoming.Control_request.request_id ctrl_req));
            handle_control_request t ctrl_req;
            loop ()

        | Error err ->
            Log.err (fun m -> m "Failed to decode incoming message: %s\nLine: %s"
              (Jsont.Error.to_string err) line);
            loop ()
  in
  Log.debug (fun m -> m "Starting message handler");
  loop

let create ?(options = Options.default) ~sw ~process_mgr () =
  (* Automatically enable permission prompt tool when callback is configured
     (matching Python SDK behavior in client.py:104-121) *)
  let options =
    match Options.permission_callback options with
    | Some _ when Options.permission_prompt_tool_name options = None ->
        (* Set permission_prompt_tool_name to "stdio" to enable control protocol *)
        Options.with_permission_prompt_tool_name "stdio" options
    | _ -> options
  in
  let transport = Transport.create ~sw ~process_mgr ~options () in

  (* Setup hook callbacks *)
  let hook_callbacks = Hashtbl.create 16 in
  let next_callback_id = ref 0 in

  let t = {
    transport;
    permission_callback = Options.permission_callback options;
    permission_log = None;
    hook_callbacks;
    session_id = None;
    control_responses = Hashtbl.create 16;
    control_mutex = Eio.Mutex.create ();
    control_condition = Eio.Condition.create ();
  } in

  (* Register hooks and send initialize if hooks are configured *)
  (match Options.hooks options with
  | Some hooks_config ->
      Log.info (fun m -> m "Registering hooks...");

      (* Build hooks configuration with callback IDs as (string * Jsont.json) list *)
      let hooks_list = List.map (fun (event, matchers) ->
        let event_name = Hooks.event_to_string event in
        let matcher_wires = List.map (fun matcher ->
          let callback_ids = List.map (fun callback ->
            let callback_id = Printf.sprintf "hook_%d" !next_callback_id in
            incr next_callback_id;
            Hashtbl.add hook_callbacks callback_id callback;
            Log.debug (fun m -> m "Registered callback: %s for event: %s" callback_id event_name);
            callback_id
          ) matcher.Hooks.callbacks in
          Hook_matcher_wire.{ matcher = matcher.Hooks.matcher; hook_callback_ids = callback_ids }
        ) matchers in
        (event_name, Hook_matcher_wire.encode matcher_wires)
      ) hooks_config in

      (* Create initialize request using Sdk_control codec *)
      let request = Sdk_control.Request.initialize ~hooks:hooks_list () in
      let ctrl_req = Sdk_control.create_request ~request_id:"init_hooks" ~request () in
      let initialize_msg = match Jsont.Json.encode Sdk_control.jsont ctrl_req with
        | Ok json -> json
        | Error msg -> failwith ("Failed to encode initialize request: " ^ msg)
      in
      Log.info (fun m -> m "Sending hooks initialize request");
      Transport.send t.transport initialize_msg
  | None -> ());

  t

let query t prompt =
  let msg = Message.user_string prompt in
  Log.info (fun m -> m "→ %a" Message.pp msg);
  let json = Message.to_json msg in
  Transport.send t.transport json

let send_message t msg =
  Log.info (fun m -> m "→ %a" Message.pp msg);
  let json = Message.to_json msg in
  Transport.send t.transport json

let send_user_message t user_msg =
  let msg = Message.User user_msg in
  Log.info (fun m -> m "→ %a" Message.pp msg);
  let json = Message.User.to_json user_msg in
  Transport.send t.transport json

let receive t =
  handle_messages t

let receive_all t =
  let rec collect acc seq =
    match seq () with
    | Seq.Nil -> 
        Log.debug (fun m -> m "End of message sequence (%d messages)" (List.length acc));
        List.rev acc
    | Seq.Cons (Message.Result _ as msg, _) ->
        Log.debug (fun m -> m "Received final Result message");
        List.rev (msg :: acc)
    | Seq.Cons (msg, rest) ->
        collect (msg :: acc) rest
  in
  collect [] (handle_messages t)

let interrupt t =
  Transport.interrupt t.transport

let discover_permissions t =
  let log = ref [] in
  let callback = Permissions.discovery_callback log in
  { t with 
    permission_callback = Some callback;
    permission_log = Some log
  }

let get_discovered_permissions t =
  match t.permission_log with
  | Some log -> !log
  | None -> []

let with_permission_callback t callback =
  { t with permission_callback = Some callback }

(* Helper to send a control request and wait for response *)
let send_control_request t ~request_id request =
  (* Send the control request *)
  let control_msg = Sdk_control.create_request ~request_id ~request () in
  let json = match Jsont.Json.encode Sdk_control.jsont control_msg with
    | Ok j -> j
    | Error msg -> failwith ("Failed to encode control request: " ^ msg)
  in
  Log.info (fun m -> m "Sending control request: %s" (json_to_string json));
  Transport.send t.transport json;

  (* Wait for the response with timeout *)
  let max_wait = 10.0 in (* 10 seconds timeout *)
  let start_time = Unix.gettimeofday () in

  let rec wait_for_response () =
    Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
      match Hashtbl.find_opt t.control_responses request_id with
      | Some response_json ->
          (* Remove it from the table *)
          Hashtbl.remove t.control_responses request_id;
          response_json
      | None ->
          let elapsed = Unix.gettimeofday () -. start_time in
          if elapsed > max_wait then
            raise (Failure (Printf.sprintf "Timeout waiting for control response: %s" request_id))
          else (
            (* Release mutex and wait for signal *)
            Eio.Condition.await_no_mutex t.control_condition;
            wait_for_response ()
          )
    )
  in

  let response_json = wait_for_response () in
  Log.debug (fun m -> m "Received control response: %s" (json_to_string response_json));

  (* Parse the response - extract the "response" field using jsont codec *)
  let response_field_codec = Jsont.Object.map ~kind:"ResponseField" Fun.id
    |> Jsont.Object.mem "response" Jsont.json ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let response_data = match Jsont.Json.decode response_field_codec response_json with
    | Ok r -> r
    | Error msg -> raise (Invalid_argument ("Failed to extract response field: " ^ msg))
  in
  let response = match Jsont.Json.decode Sdk_control.Response.jsont response_data with
    | Ok r -> r
    | Error msg -> raise (Invalid_argument ("Failed to decode response: " ^ msg))
  in
  match response with
  | Sdk_control.Response.Success s -> s.response
  | Sdk_control.Response.Error e ->
      raise (Failure (Printf.sprintf "Control request failed: %s" e.error))

let set_permission_mode t mode =
  let request_id = Printf.sprintf "set_perm_mode_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.set_permission_mode ~mode () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m -> m "Permission mode set to: %a" Permissions.Mode.pp mode)

let set_model t model =
  let model_str = Model.to_string model in
  let request_id = Printf.sprintf "set_model_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.set_model ~model:model_str () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m -> m "Model set to: %a" Model.pp model)

let set_model_string t model_str =
  set_model t (Model.of_string model_str)

let get_server_info t =
  let request_id = Printf.sprintf "get_server_info_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.get_server_info () in
  match send_control_request t ~request_id request with
  | Some response_data ->
      let server_info = match Jsont.Json.decode Sdk_control.Server_info.jsont response_data with
        | Ok si -> si
        | Error msg -> raise (Invalid_argument ("Failed to decode server info: " ^ msg))
      in
      Log.info (fun m -> m "Retrieved server info: %a" Sdk_control.Server_info.pp server_info);
      server_info
  | None ->
      raise (Failure "No response data from get_server_info request")
