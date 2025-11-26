let src = Logs.Src.create "claude.client" ~doc:"Claude client"

module Log = (val Logs.src_log src : Logs.LOG)

(** Control response builders using Sdk_control codecs *)
module Control_response = struct
  let success ~request_id ~response =
    let resp = Sdk_control.Response.success ~request_id ?response () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    Jsont.Json.encode Sdk_control.jsont ctrl
    |> Err.get_ok ~msg:"Control_response.success: "

  let error ~request_id ~message =
    let resp = Sdk_control.Response.error ~request_id ~error:message () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    Jsont.Json.encode Sdk_control.jsont ctrl
    |> Err.get_ok ~msg:"Control_response.error: "
end

(* Helper functions for JSON manipulation using jsont *)
let json_to_string json =
  Jsont_bytesrw.encode_string' Jsont.json json
  |> Result.map_error Jsont.Error.to_string
  |> Err.get_ok ~msg:""

(** Wire-level codec for permission responses to CLI. Uses camelCase field names
    as expected by the CLI protocol. *)
module Permission_wire = struct
  type allow = { allow_behavior : string; allow_updated_input : Jsont.json }
  type deny = { deny_behavior : string; deny_message : string }

  let allow_jsont : allow Jsont.t =
    let make allow_behavior allow_updated_input =
      { allow_behavior; allow_updated_input }
    in
    Jsont.Object.map ~kind:"AllowWire" make
    |> Jsont.Object.mem "behavior" Jsont.string ~enc:(fun r -> r.allow_behavior)
    |> Jsont.Object.mem "updatedInput" Jsont.json ~enc:(fun r ->
        r.allow_updated_input)
    |> Jsont.Object.finish

  let deny_jsont : deny Jsont.t =
    let make deny_behavior deny_message = { deny_behavior; deny_message } in
    Jsont.Object.map ~kind:"DenyWire" make
    |> Jsont.Object.mem "behavior" Jsont.string ~enc:(fun r -> r.deny_behavior)
    |> Jsont.Object.mem "message" Jsont.string ~enc:(fun r -> r.deny_message)
    |> Jsont.Object.finish

  let encode_allow ~updated_input =
    Jsont.Json.encode allow_jsont
      { allow_behavior = "allow"; allow_updated_input = updated_input }
    |> Err.get_ok ~msg:"Permission_wire.encode_allow: "

  let encode_deny ~message =
    Jsont.Json.encode deny_jsont
      { deny_behavior = "deny"; deny_message = message }
    |> Err.get_ok ~msg:"Permission_wire.encode_deny: "
end

(** Wire-level codec for hook matcher configuration sent to CLI. *)
module Hook_matcher_wire = struct
  type t = { matcher : string option; hook_callback_ids : string list }

  let jsont : t Jsont.t =
    let make matcher hook_callback_ids = { matcher; hook_callback_ids } in
    Jsont.Object.map ~kind:"HookMatcherWire" make
    |> Jsont.Object.opt_mem "matcher" Jsont.string ~enc:(fun r -> r.matcher)
    |> Jsont.Object.mem "hookCallbackIds" (Jsont.list Jsont.string)
         ~enc:(fun r -> r.hook_callback_ids)
    |> Jsont.Object.finish

  let encode matchers =
    List.map
      (fun m ->
        Jsont.Json.encode jsont m
        |> Err.get_ok ~msg:"Hook_matcher_wire.encode: ")
      matchers
    |> Jsont.Json.list
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

let handle_control_request t (ctrl_req : Sdk_control.control_request) =
  let request_id = ctrl_req.request_id in
  Log.info (fun m -> m "Handling control request: %s" request_id);

  match ctrl_req.request with
  | Sdk_control.Request.Permission req ->
      let tool_name = req.tool_name in
      let input = req.input in
      Log.info (fun m ->
          m "Permission request for tool '%s' with input: %s" tool_name
            (json_to_string input));
      (* Convert permission_suggestions to Context *)
      let suggestions = Option.value req.permission_suggestions ~default:[] in
      let context = Permissions.Context.create ~suggestions () in

      Log.info (fun m ->
          m "Invoking permission callback for tool: %s" tool_name);
      let callback =
        Option.value t.permission_callback
          ~default:Permissions.default_allow_callback
      in
      let result = callback ~tool_name ~input ~context in
      Log.info (fun m ->
          m "Permission callback returned: %s"
            (match result with
            | Permissions.Result.Allow _ -> "ALLOW"
            | Permissions.Result.Deny _ -> "DENY"));

      (* Convert permission result to CLI format using wire codec *)
      let response_data =
        match result with
        | Permissions.Result.Allow
            { updated_input; updated_permissions = _; unknown = _ } ->
            let updated_input = Option.value updated_input ~default:input in
            Permission_wire.encode_allow ~updated_input
        | Permissions.Result.Deny { message; interrupt = _; unknown = _ } ->
            Permission_wire.encode_deny ~message
      in
      let response =
        Control_response.success ~request_id ~response:(Some response_data)
      in
      Log.info (fun m ->
          m "Sending control response: %s" (json_to_string response));
      Transport.send t.transport response
  | Sdk_control.Request.Hook_callback req -> (
      let callback_id = req.callback_id in
      let input = req.input in
      let tool_use_id = req.tool_use_id in
      Log.info (fun m ->
          m "Hook callback request for callback_id: %s" callback_id);

      try
        let callback = Hashtbl.find t.hook_callbacks callback_id in
        let context = Hooks.Context.create () in
        let result = callback ~input ~tool_use_id ~context in

        let result_json =
          Jsont.Json.encode Hooks.result_jsont result
          |> Err.get_ok ~msg:"Failed to encode hook result: "
        in
        Log.debug (fun m ->
            m "Hook result JSON: %s" (json_to_string result_json));
        let response =
          Control_response.success ~request_id ~response:(Some result_json)
        in
        Log.info (fun m -> m "Hook callback succeeded, sending response");
        Transport.send t.transport response
      with
      | Not_found ->
          let error_msg =
            Printf.sprintf "Hook callback not found: %s" callback_id
          in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport
            (Control_response.error ~request_id ~message:error_msg)
      | exn ->
          let error_msg =
            Printf.sprintf "Hook callback error: %s" (Printexc.to_string exn)
          in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport
            (Control_response.error ~request_id ~message:error_msg))
  | _ ->
      (* Other request types not handled here *)
      let error_msg = "Unsupported control request type" in
      Transport.send t.transport
        (Control_response.error ~request_id ~message:error_msg)

let handle_control_response t control_resp =
  let request_id =
    match control_resp.Sdk_control.response with
    | Sdk_control.Response.Success s -> s.request_id
    | Sdk_control.Response.Error e -> e.request_id
  in
  Log.debug (fun m ->
      m "Received control response for request_id: %s" request_id);

  (* Store the response as JSON and signal waiting threads *)
  let json =
    Jsont.Json.encode Sdk_control.control_response_jsont control_resp
    |> Err.get_ok ~msg:"Failed to encode control response: "
  in
  Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
      Hashtbl.replace t.control_responses request_id json;
      Eio.Condition.broadcast t.control_condition)

let handle_messages t =
  let rec loop () =
    match Transport.receive_line t.transport with
    | None ->
        (* EOF *)
        Log.debug (fun m -> m "Handle messages: EOF received");
        Seq.Nil
    | Some line -> (
        (* Use unified Incoming codec for all message types *)
        match Jsont_bytesrw.decode_string' Incoming.jsont line with
        | Ok (Incoming.Message msg) ->
            Log.info (fun m -> m "← %a" Message.pp msg);

            (* Extract session ID from system messages *)
            (match msg with
            | Message.System sys ->
                Message.System.session_id sys
                |> Option.iter (fun session_id ->
                    t.session_id <- Some session_id;
                    Log.debug (fun m -> m "Stored session ID: %s" session_id))
            | _ -> ());

            Seq.Cons (msg, loop)
        | Ok (Incoming.Control_response resp) ->
            handle_control_response t resp;
            loop ()
        | Ok (Incoming.Control_request ctrl_req) ->
            Log.info (fun m ->
                m "Received control request (request_id: %s)"
                  ctrl_req.request_id);
            handle_control_request t ctrl_req;
            loop ()
        | Error err ->
            Log.err (fun m ->
                m "Failed to decode incoming message: %s\nLine: %s"
                  (Jsont.Error.to_string err)
                  line);
            loop ())
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

  let t =
    {
      transport;
      permission_callback = Options.permission_callback options;
      permission_log = None;
      hook_callbacks;
      session_id = None;
      control_responses = Hashtbl.create 16;
      control_mutex = Eio.Mutex.create ();
      control_condition = Eio.Condition.create ();
    }
  in

  (* Register hooks and send initialize if hooks are configured *)
  Options.hooks options
  |> Option.iter (fun hooks_config ->
      Log.info (fun m -> m "Registering hooks...");

      (* Build hooks configuration with callback IDs as (string * Jsont.json) list *)
      let hooks_list =
        List.map
          (fun (event, matchers) ->
            let event_name = Hooks.event_to_string event in
            let matcher_wires =
              List.map
                (fun matcher ->
                  let callback_ids =
                    List.map
                      (fun callback ->
                        let callback_id =
                          Printf.sprintf "hook_%d" !next_callback_id
                        in
                        incr next_callback_id;
                        Hashtbl.add hook_callbacks callback_id callback;
                        Log.debug (fun m ->
                            m "Registered callback: %s for event: %s"
                              callback_id event_name);
                        callback_id)
                      matcher.Hooks.callbacks
                  in
                  Hook_matcher_wire.
                    {
                      matcher = matcher.Hooks.matcher;
                      hook_callback_ids = callback_ids;
                    })
                matchers
            in
            (event_name, Hook_matcher_wire.encode matcher_wires))
          hooks_config
      in

      (* Create initialize request using Sdk_control codec *)
      let request = Sdk_control.Request.initialize ~hooks:hooks_list () in
      let ctrl_req =
        Sdk_control.create_request ~request_id:"init_hooks" ~request ()
      in
      let initialize_msg =
        Jsont.Json.encode Sdk_control.jsont ctrl_req
        |> Err.get_ok ~msg:"Failed to encode initialize request: "
      in
      Log.info (fun m -> m "Sending hooks initialize request");
      Transport.send t.transport initialize_msg);

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

let receive t = handle_messages t

let receive_all t =
  let rec collect acc seq =
    match seq () with
    | Seq.Nil ->
        Log.debug (fun m ->
            m "End of message sequence (%d messages)" (List.length acc));
        List.rev acc
    | Seq.Cons ((Message.Result _ as msg), _) ->
        Log.debug (fun m -> m "Received final Result message");
        List.rev (msg :: acc)
    | Seq.Cons (msg, rest) -> collect (msg :: acc) rest
  in
  collect [] (handle_messages t)

let interrupt t = Transport.interrupt t.transport

let discover_permissions t =
  let log = ref [] in
  let callback = Permissions.discovery_callback log in
  { t with permission_callback = Some callback; permission_log = Some log }

let get_discovered_permissions t =
  t.permission_log |> Option.map ( ! ) |> Option.value ~default:[]

let with_permission_callback t callback =
  { t with permission_callback = Some callback }

(* Helper to send a control request and wait for response *)
let send_control_request t ~request_id request =
  (* Send the control request *)
  let control_msg = Sdk_control.create_request ~request_id ~request () in
  let json =
    Jsont.Json.encode Sdk_control.jsont control_msg
    |> Err.get_ok ~msg:"Failed to encode control request: "
  in
  Log.info (fun m -> m "Sending control request: %s" (json_to_string json));
  Transport.send t.transport json;

  (* Wait for the response with timeout *)
  let max_wait = 10.0 in
  (* 10 seconds timeout *)
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
              raise
                (Failure
                   (Printf.sprintf "Timeout waiting for control response: %s"
                      request_id))
            else (
              (* Release mutex and wait for signal *)
              Eio.Condition.await_no_mutex t.control_condition;
              wait_for_response ()))
  in

  let response_json = wait_for_response () in
  Log.debug (fun m ->
      m "Received control response: %s" (json_to_string response_json));

  (* Parse the response - extract the "response" field using jsont codec *)
  let response_field_codec =
    Jsont.Object.map ~kind:"ResponseField" Fun.id
    |> Jsont.Object.mem "response" Jsont.json ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let response_data =
    Jsont.Json.decode response_field_codec response_json
    |> Err.get_ok' ~msg:"Failed to extract response field: "
  in
  let response =
    Jsont.Json.decode Sdk_control.Response.jsont response_data
    |> Err.get_ok' ~msg:"Failed to decode response: "
  in
  match response with
  | Sdk_control.Response.Success s -> s.response
  | Sdk_control.Response.Error e ->
      raise (Failure (Printf.sprintf "Control request failed: %s" e.error))

let set_permission_mode t mode =
  let request_id = Printf.sprintf "set_perm_mode_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.set_permission_mode ~mode () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m ->
      m "Permission mode set to: %s" (Permissions.Mode.to_string mode))

let set_model t model =
  let model_str = Model.to_string model in
  let request_id = Printf.sprintf "set_model_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.set_model ~model:model_str () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m -> m "Model set to: %s" model_str)

let get_server_info t =
  let request_id = Printf.sprintf "get_server_info_%f" (Unix.gettimeofday ()) in
  let request = Sdk_control.Request.get_server_info () in
  let response_data =
    send_control_request t ~request_id request
    |> Option.to_result ~none:"No response data from get_server_info request"
    |> Err.get_ok ~msg:""
  in
  let server_info =
    Jsont.Json.decode Sdk_control.Server_info.jsont response_data
    |> Err.get_ok' ~msg:"Failed to decode server info: "
  in
  Log.info (fun m ->
      m "Retrieved server info: %a"
        (Jsont.pp_value Sdk_control.Server_info.jsont ())
        server_info);
  server_info
