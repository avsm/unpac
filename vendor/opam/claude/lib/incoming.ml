let src =
  Logs.Src.create "claude.incoming" ~doc:"Incoming messages from Claude CLI"

module Log = (val Logs.src_log src : Logs.LOG)

(** Control request types for incoming control_request messages *)
module Control_request = struct
  (** Can use tool permission request *)
  module Can_use_tool = struct
    type t = {
      tool_name : string;
      input : Jsont.json;
      permission_suggestions : Jsont.json list;
    }

    let tool_name t = t.tool_name
    let input t = t.input
    let permission_suggestions t = t.permission_suggestions

    let jsont : t Jsont.t =
      let make tool_name input permission_suggestions =
        {
          tool_name;
          input;
          permission_suggestions =
            Option.value permission_suggestions ~default:[];
        }
      in
      Jsont.Object.map ~kind:"CanUseTool" make
      |> Jsont.Object.mem "tool_name" Jsont.string ~enc:tool_name
      |> Jsont.Object.mem "input" Jsont.json ~enc:input
      |> Jsont.Object.opt_mem "permission_suggestions" (Jsont.list Jsont.json)
           ~enc:(fun t ->
             if t.permission_suggestions = [] then None
             else Some t.permission_suggestions)
      |> Jsont.Object.finish
  end

  (** Hook callback request *)
  module Hook_callback = struct
    type t = {
      callback_id : string;
      input : Jsont.json;
      tool_use_id : string option;
    }

    let callback_id t = t.callback_id
    let input t = t.input
    let tool_use_id t = t.tool_use_id

    let jsont : t Jsont.t =
      let make callback_id input tool_use_id =
        { callback_id; input; tool_use_id }
      in
      Jsont.Object.map ~kind:"HookCallback" make
      |> Jsont.Object.mem "callback_id" Jsont.string ~enc:callback_id
      |> Jsont.Object.mem "input" Jsont.json ~enc:input
      |> Jsont.Object.opt_mem "tool_use_id" Jsont.string ~enc:tool_use_id
      |> Jsont.Object.finish
  end

  (** Request payload - discriminated by subtype *)
  type request =
    | Can_use_tool of Can_use_tool.t
    | Hook_callback of Hook_callback.t
    | Unknown of string * Jsont.json

  let request_of_json json =
    let subtype_codec =
      Jsont.Object.map ~kind:"Subtype" Fun.id
      |> Jsont.Object.mem "subtype" Jsont.string ~enc:Fun.id
      |> Jsont.Object.finish
    in
    match Jsont.Json.decode subtype_codec json with
    | Error _ -> Unknown ("unknown", json)
    | Ok subtype -> (
        match subtype with
        | "can_use_tool" -> (
            match Jsont.Json.decode Can_use_tool.jsont json with
            | Ok r -> Can_use_tool r
            | Error _ -> Unknown (subtype, json))
        | "hook_callback" -> (
            match Jsont.Json.decode Hook_callback.jsont json with
            | Ok r -> Hook_callback r
            | Error _ -> Unknown (subtype, json))
        | _ -> Unknown (subtype, json))

  type t = { request_id : string; request : request }
  (** Full control request message *)

  let request_id t = t.request_id
  let request t = t.request

  let subtype t =
    match t.request with
    | Can_use_tool _ -> "can_use_tool"
    | Hook_callback _ -> "hook_callback"
    | Unknown (s, _) -> s

  let jsont : t Jsont.t =
    let dec json =
      let envelope_codec =
        Jsont.Object.map ~kind:"ControlRequestEnvelope"
          (fun request_id request_json -> (request_id, request_json))
        |> Jsont.Object.mem "request_id" Jsont.string ~enc:fst
        |> Jsont.Object.mem "request" Jsont.json ~enc:snd
        |> Jsont.Object.finish
      in
      match Jsont.Json.decode envelope_codec json with
      | Error err ->
          failwith ("Failed to decode control_request envelope: " ^ err)
      | Ok (request_id, request_json) ->
          { request_id; request = request_of_json request_json }
    in
    let enc t =
      let request_json =
        match t.request with
        | Can_use_tool r -> (
            match Jsont.Json.encode Can_use_tool.jsont r with
            | Ok j -> j
            | Error err -> failwith ("Failed to encode Can_use_tool: " ^ err))
        | Hook_callback r -> (
            match Jsont.Json.encode Hook_callback.jsont r with
            | Ok j -> j
            | Error err -> failwith ("Failed to encode Hook_callback: " ^ err))
        | Unknown (_, j) -> j
      in
      Jsont.Json.object'
        [
          Jsont.Json.mem (Jsont.Json.name "type")
            (Jsont.Json.string "control_request");
          Jsont.Json.mem
            (Jsont.Json.name "request_id")
            (Jsont.Json.string t.request_id);
          Jsont.Json.mem (Jsont.Json.name "request") request_json;
        ]
    in
    Jsont.map ~kind:"ControlRequest" ~dec ~enc Jsont.json
end

type t =
  | Message of Message.t
  | Control_response of Sdk_control.control_response
  | Control_request of Control_request.t

let jsont : t Jsont.t =
  (* Custom decoder that checks the type field and dispatches to the appropriate codec.

     The challenge is that Message can have multiple type values ("user", "assistant",
     "system", "result"), while control_response and control_request have single type values.
     Jsont's case_mem discriminator doesn't support multiple tags per case, so we implement
     a custom decoder/encoder. *)
  let type_field_codec =
    Jsont.Object.map ~kind:"type_field" Fun.id
    |> Jsont.Object.opt_mem "type" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in

  let dec json =
    match Jsont.Json.decode type_field_codec json with
    | Error _ | Ok None -> (
        (* No type field, try as message *)
        match Jsont.Json.decode Message.jsont json with
        | Ok msg -> Message msg
        | Error err -> failwith ("Failed to decode message: " ^ err))
    | Ok (Some typ) -> (
        match typ with
        | "control_response" -> (
            match Jsont.Json.decode Sdk_control.control_response_jsont json with
            | Ok resp -> Control_response resp
            | Error err -> failwith ("Failed to decode control_response: " ^ err)
            )
        | "control_request" -> (
            match Jsont.Json.decode Control_request.jsont json with
            | Ok req -> Control_request req
            | Error err -> failwith ("Failed to decode control_request: " ^ err)
            )
        | "user" | "assistant" | "system" | "result" | _ -> (
            (* Message types *)
            match Jsont.Json.decode Message.jsont json with
            | Ok msg -> Message msg
            | Error err -> failwith ("Failed to decode message: " ^ err)))
  in

  let enc = function
    | Message msg -> (
        match Jsont.Json.encode Message.jsont msg with
        | Ok json -> json
        | Error err -> failwith ("Failed to encode message: " ^ err))
    | Control_response resp -> (
        match Jsont.Json.encode Sdk_control.control_response_jsont resp with
        | Ok json -> json
        | Error err -> failwith ("Failed to encode control response: " ^ err))
    | Control_request req -> (
        match Jsont.Json.encode Control_request.jsont req with
        | Ok json -> json
        | Error err -> failwith ("Failed to encode control request: " ^ err))
  in

  Jsont.map ~kind:"Incoming" ~dec ~enc Jsont.json
