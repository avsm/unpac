let src = Logs.Src.create "claude.hooks" ~doc:"Claude hooks system"

module Log = (val Logs.src_log src : Logs.LOG)

(** Hook events that can be intercepted *)
type event =
  | Pre_tool_use
  | Post_tool_use
  | User_prompt_submit
  | Stop
  | Subagent_stop
  | Pre_compact

let event_to_string = function
  | Pre_tool_use -> "PreToolUse"
  | Post_tool_use -> "PostToolUse"
  | User_prompt_submit -> "UserPromptSubmit"
  | Stop -> "Stop"
  | Subagent_stop -> "SubagentStop"
  | Pre_compact -> "PreCompact"

let event_of_string = function
  | "PreToolUse" -> Pre_tool_use
  | "PostToolUse" -> Post_tool_use
  | "UserPromptSubmit" -> User_prompt_submit
  | "Stop" -> Stop
  | "SubagentStop" -> Subagent_stop
  | "PreCompact" -> Pre_compact
  | s -> raise (Invalid_argument (Printf.sprintf "Unknown hook event: %s" s))

let event_jsont : event Jsont.t =
  Jsont.enum
    [
      ("PreToolUse", Pre_tool_use);
      ("PostToolUse", Post_tool_use);
      ("UserPromptSubmit", User_prompt_submit);
      ("Stop", Stop);
      ("SubagentStop", Subagent_stop);
      ("PreCompact", Pre_compact);
    ]

(** Context provided to hook callbacks *)
module Context = struct
  type t = {
    signal : unit option; (* Future: abort signal support *)
    unknown : Unknown.t;
  }

  let create ?(signal = None) ?(unknown = Unknown.empty) () =
    { signal; unknown }

  let signal t = t.signal
  let unknown t = t.unknown

  let jsont : t Jsont.t =
    let make unknown = { signal = None; unknown } in
    Jsont.Object.map ~kind:"Context" make
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish
end

(** Hook decision control *)
type decision = Continue | Block

let decision_jsont : decision Jsont.t =
  Jsont.enum [ ("continue", Continue); ("block", Block) ]

type result = {
  decision : decision option;
  system_message : string option;
  hook_specific_output : Jsont.json option;
  unknown : Unknown.t;
}
(** Generic hook result *)

let result_jsont : result Jsont.t =
  let make decision system_message hook_specific_output unknown =
    { decision; system_message; hook_specific_output; unknown }
  in
  Jsont.Object.map ~kind:"Result" make
  |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun r -> r.decision)
  |> Jsont.Object.opt_mem "systemMessage" Jsont.string ~enc:(fun r ->
      r.system_message)
  |> Jsont.Object.opt_mem "hookSpecificOutput" Jsont.json ~enc:(fun r ->
      r.hook_specific_output)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun r -> r.unknown)
  |> Jsont.Object.finish

(** {1 PreToolUse Hook} *)
module PreToolUse = struct
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Jsont.json;
    unknown : Unknown.t;
  }

  type t = input

  let session_id t = t.session_id
  let transcript_path t = t.transcript_path
  let tool_name t = t.tool_name
  let tool_input t = t.tool_input
  let unknown t = t.unknown

  let input_jsont : input Jsont.t =
    let make session_id transcript_path tool_name tool_input unknown =
      { session_id; transcript_path; tool_name; tool_input; unknown }
    in
    Jsont.Object.map ~kind:"PreToolUseInput" make
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
    |> Jsont.Object.mem "tool_name" Jsont.string ~enc:tool_name
    |> Jsont.Object.mem "tool_input" Jsont.json ~enc:tool_input
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode input_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("PreToolUse: " ^ msg))

  type permission_decision = [ `Allow | `Deny | `Ask ]

  let permission_decision_jsont : permission_decision Jsont.t =
    Jsont.enum [ ("allow", `Allow); ("deny", `Deny); ("ask", `Ask) ]

  type output = {
    permission_decision : permission_decision option;
    permission_decision_reason : string option;
    updated_input : Jsont.json option;
    unknown : Unknown.t;
  }

  let output_jsont : output Jsont.t =
    let make permission_decision permission_decision_reason updated_input
        unknown =
      {
        permission_decision;
        permission_decision_reason;
        updated_input;
        unknown;
      }
    in
    Jsont.Object.map ~kind:"PreToolUseOutput" make
    |> Jsont.Object.opt_mem "permissionDecision" permission_decision_jsont
         ~enc:(fun o -> o.permission_decision)
    |> Jsont.Object.opt_mem "permissionDecisionReason" Jsont.string
         ~enc:(fun o -> o.permission_decision_reason)
    |> Jsont.Object.opt_mem "updatedInput" Jsont.json ~enc:(fun o ->
        o.updated_input)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun o -> o.unknown)
    |> Jsont.Object.finish

  let output_to_json output =
    match Jsont.Json.encode output_jsont output with
    | Ok json -> json
    | Error msg -> failwith ("PreToolUse.output_to_json: " ^ msg)

  let allow ?reason ?updated_input ?(unknown = Unknown.empty) () =
    {
      permission_decision = Some `Allow;
      permission_decision_reason = reason;
      updated_input;
      unknown;
    }

  let deny ?reason ?(unknown = Unknown.empty) () =
    {
      permission_decision = Some `Deny;
      permission_decision_reason = reason;
      updated_input = None;
      unknown;
    }

  let ask ?reason ?(unknown = Unknown.empty) () =
    {
      permission_decision = Some `Ask;
      permission_decision_reason = reason;
      updated_input = None;
      unknown;
    }

  let continue ?(unknown = Unknown.empty) () =
    {
      permission_decision = None;
      permission_decision_reason = None;
      updated_input = None;
      unknown;
    }
end

(** {1 PostToolUse Hook} *)
module PostToolUse = struct
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Jsont.json;
    tool_response : Jsont.json;
    unknown : Unknown.t;
  }

  type t = input

  let session_id t = t.session_id
  let transcript_path t = t.transcript_path
  let tool_name t = t.tool_name
  let tool_input t = t.tool_input
  let tool_response t = t.tool_response
  let unknown t = t.unknown

  let input_jsont : input Jsont.t =
    let make session_id transcript_path tool_name tool_input tool_response
        unknown =
      {
        session_id;
        transcript_path;
        tool_name;
        tool_input;
        tool_response;
        unknown;
      }
    in
    Jsont.Object.map ~kind:"PostToolUseInput" make
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
    |> Jsont.Object.mem "tool_name" Jsont.string ~enc:tool_name
    |> Jsont.Object.mem "tool_input" Jsont.json ~enc:tool_input
    |> Jsont.Object.mem "tool_response" Jsont.json ~enc:tool_response
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode input_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("PostToolUse: " ^ msg))

  type output = {
    decision : decision option;
    reason : string option;
    additional_context : string option;
    unknown : Unknown.t;
  }

  let output_jsont : output Jsont.t =
    let make decision reason additional_context unknown =
      { decision; reason; additional_context; unknown }
    in
    Jsont.Object.map ~kind:"PostToolUseOutput" make
    |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o -> o.decision)
    |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
    |> Jsont.Object.opt_mem "additionalContext" Jsont.string ~enc:(fun o ->
        o.additional_context)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun o -> o.unknown)
    |> Jsont.Object.finish

  let output_to_json output =
    match Jsont.Json.encode output_jsont output with
    | Ok json -> json
    | Error msg -> failwith ("PostToolUse.output_to_json: " ^ msg)

  let continue ?additional_context ?(unknown = Unknown.empty) () =
    { decision = None; reason = None; additional_context; unknown }

  let block ?reason ?additional_context ?(unknown = Unknown.empty) () =
    { decision = Some Block; reason; additional_context; unknown }
end

(** {1 UserPromptSubmit Hook} *)
module UserPromptSubmit = struct
  type input = {
    session_id : string;
    transcript_path : string;
    prompt : string;
    unknown : Unknown.t;
  }

  type t = input

  let session_id t = t.session_id
  let transcript_path t = t.transcript_path
  let prompt t = t.prompt
  let unknown t = t.unknown

  let input_jsont : input Jsont.t =
    let make session_id transcript_path prompt unknown =
      { session_id; transcript_path; prompt; unknown }
    in
    Jsont.Object.map ~kind:"UserPromptSubmitInput" make
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
    |> Jsont.Object.mem "prompt" Jsont.string ~enc:prompt
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode input_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("UserPromptSubmit: " ^ msg))

  type output = {
    decision : decision option;
    reason : string option;
    additional_context : string option;
    unknown : Unknown.t;
  }

  let output_jsont : output Jsont.t =
    let make decision reason additional_context unknown =
      { decision; reason; additional_context; unknown }
    in
    Jsont.Object.map ~kind:"UserPromptSubmitOutput" make
    |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o -> o.decision)
    |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
    |> Jsont.Object.opt_mem "additionalContext" Jsont.string ~enc:(fun o ->
        o.additional_context)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun o -> o.unknown)
    |> Jsont.Object.finish

  let output_to_json output =
    match Jsont.Json.encode output_jsont output with
    | Ok json -> json
    | Error msg -> failwith ("UserPromptSubmit.output_to_json: " ^ msg)

  let continue ?additional_context ?(unknown = Unknown.empty) () =
    { decision = None; reason = None; additional_context; unknown }

  let block ?reason ?(unknown = Unknown.empty) () =
    { decision = Some Block; reason; additional_context = None; unknown }
end

(** {1 Stop Hook} *)
module Stop = struct
  type input = {
    session_id : string;
    transcript_path : string;
    stop_hook_active : bool;
    unknown : Unknown.t;
  }

  type t = input

  let session_id t = t.session_id
  let transcript_path t = t.transcript_path
  let stop_hook_active t = t.stop_hook_active
  let unknown t = t.unknown

  let input_jsont : input Jsont.t =
    let make session_id transcript_path stop_hook_active unknown =
      { session_id; transcript_path; stop_hook_active; unknown }
    in
    Jsont.Object.map ~kind:"StopInput" make
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
    |> Jsont.Object.mem "stop_hook_active" Jsont.bool ~enc:stop_hook_active
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode input_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("Stop: " ^ msg))

  type output = {
    decision : decision option;
    reason : string option;
    unknown : Unknown.t;
  }

  let output_jsont : output Jsont.t =
    let make decision reason unknown = { decision; reason; unknown } in
    Jsont.Object.map ~kind:"StopOutput" make
    |> Jsont.Object.opt_mem "decision" decision_jsont ~enc:(fun o -> o.decision)
    |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(fun o -> o.reason)
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun o -> o.unknown)
    |> Jsont.Object.finish

  let output_to_json output =
    match Jsont.Json.encode output_jsont output with
    | Ok json -> json
    | Error msg -> failwith ("Stop.output_to_json: " ^ msg)

  let continue ?(unknown = Unknown.empty) () =
    { decision = None; reason = None; unknown }

  let block ?reason ?(unknown = Unknown.empty) () =
    { decision = Some Block; reason; unknown }
end

(** {1 SubagentStop Hook} - Same structure as Stop *)
module SubagentStop = struct
  include Stop
end

(** {1 PreCompact Hook} *)
module PreCompact = struct
  type input = {
    session_id : string;
    transcript_path : string;
    unknown : Unknown.t;
  }

  type t = input

  let session_id t = t.session_id
  let transcript_path t = t.transcript_path
  let unknown t = t.unknown

  let input_jsont : input Jsont.t =
    let make session_id transcript_path unknown =
      { session_id; transcript_path; unknown }
    in
    Jsont.Object.map ~kind:"PreCompactInput" make
    |> Jsont.Object.mem "session_id" Jsont.string ~enc:session_id
    |> Jsont.Object.mem "transcript_path" Jsont.string ~enc:transcript_path
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
    |> Jsont.Object.finish

  let of_json json =
    match Jsont.Json.decode input_jsont json with
    | Ok v -> v
    | Error msg -> raise (Invalid_argument ("PreCompact: " ^ msg))

  type output = unit (* No specific output for PreCompact *)

  let output_to_json () = Jsont.Object ([], Jsont.Meta.none)
  let continue () = ()
end

type callback =
  input:Jsont.json -> tool_use_id:string option -> context:Context.t -> result
(** {1 Generic Callback Type} *)

type matcher = { matcher : string option; callbacks : callback list }
(** {1 Matcher Configuration} *)

type config = (event * matcher list) list

(** {1 Result Builders} *)
let continue ?system_message ?hook_specific_output ?(unknown = Unknown.empty) ()
    =
  { decision = None; system_message; hook_specific_output; unknown }

let block ?system_message ?hook_specific_output ?(unknown = Unknown.empty) () =
  { decision = Some Block; system_message; hook_specific_output; unknown }

(** {1 Matcher Builders} *)
let matcher ?pattern callbacks = { matcher = pattern; callbacks }

(** {1 Config Builders} *)
let empty = []

let add event matchers config = (event, matchers) :: config

(** {1 JSON Conversion} *)
let result_to_json result =
  match Jsont.Json.encode result_jsont result with
  | Ok json -> json
  | Error msg -> failwith ("result_to_json: " ^ msg)

(** Wire codec for hook matcher in protocol format *)
module Protocol_matcher_wire = struct
  type t = { matcher : string option; callbacks : Jsont.json list }

  let jsont : t Jsont.t =
    let make matcher callbacks = { matcher; callbacks } in
    Jsont.Object.map ~kind:"ProtocolMatcher" make
    |> Jsont.Object.opt_mem "matcher" Jsont.string ~enc:(fun r -> r.matcher)
    |> Jsont.Object.mem "callbacks" (Jsont.list Jsont.json) ~enc:(fun r ->
        r.callbacks)
    |> Jsont.Object.finish

  let encode m =
    match Jsont.Json.encode jsont m with
    | Ok json -> json
    | Error msg -> failwith ("Protocol_matcher_wire.encode: " ^ msg)
end

let config_to_protocol_format config =
  let hooks_dict =
    List.map
      (fun (event, matchers) ->
        let event_name = event_to_string event in
        let matchers_json =
          List.map
            (fun m ->
              (* matcher and hookCallbackIds will be filled in by client *)
              Protocol_matcher_wire.encode
                { matcher = m.matcher; callbacks = [] })
            matchers
        in
        Jsont.Json.mem
          (Jsont.Json.name event_name)
          (Jsont.Json.list matchers_json))
      config
  in
  Jsont.Json.object' hooks_dict
