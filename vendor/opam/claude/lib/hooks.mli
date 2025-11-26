(** Claude Code Hooks System

    Hooks allow you to intercept and control events in Claude Code sessions,
    such as tool usage, prompt submission, and session stops.

    {1 Overview}

    Hooks are organized by event type, with each event having:
    - A typed input structure (accessible via submodules)
    - A typed output structure for responses
    - Helper functions for common responses

    {1 Example Usage}

    {[
      open Eio.Std

      (* Block dangerous bash commands *)
      let get_string json key =
        match json with
        | Jsont.Object (members, _) ->
            List.find_map (fun ((name, _), value) ->
              if name = key then
                match value with
                | Jsont.String (s, _) -> Some s
                | _ -> None
              else None
            ) members
        | _ -> None
      in
      let block_rm_rf ~input ~tool_use_id:_ ~context:_ =
        let hook = Hooks.PreToolUse.of_json input in
        if Hooks.PreToolUse.tool_name hook = "Bash" then
          let tool_input = Hooks.PreToolUse.tool_input hook in
          match get_string tool_input "command" with
          | Some cmd when String.contains cmd "rm -rf" ->
              let output = Hooks.PreToolUse.deny ~reason:"Dangerous command" () in
              Hooks.continue
                ~hook_specific_output:(Hooks.PreToolUse.output_to_json output)
                ()
          | _ -> Hooks.continue ()
        else Hooks.continue ()

      let hooks =
        Hooks.empty
        |> Hooks.add Hooks.Pre_tool_use [
            Hooks.matcher ~pattern:"Bash" [block_rm_rf]
          ]

      let options = Claude.Options.create ~hooks:(Some hooks) () in
      let client = Claude.Client.create ~options ~sw ~process_mgr () in
    ]} *)

val src : Logs.Src.t
(** The log source for hooks *)

(** {1 Hook Events} *)

(** Hook event types *)
type event =
  | Pre_tool_use  (** Fires before a tool is executed *)
  | Post_tool_use  (** Fires after a tool completes *)
  | User_prompt_submit  (** Fires when user submits a prompt *)
  | Stop  (** Fires when conversation stops *)
  | Subagent_stop  (** Fires when a subagent stops *)
  | Pre_compact  (** Fires before message compaction *)

val event_to_string : event -> string
val event_of_string : string -> event
val event_jsont : event Jsont.t

(** {1 Context} *)

module Context : sig
  type t = { signal : unit option; unknown : Unknown.t }

  val create : ?signal:unit option -> ?unknown:Unknown.t -> unit -> t
  val signal : t -> unit option
  val unknown : t -> Unknown.t
  val jsont : t Jsont.t
end

(** {1 Decisions} *)

type decision =
  | Continue  (** Allow the action to proceed *)
  | Block  (** Block the action *)

val decision_jsont : decision Jsont.t

(** {1 Generic Hook Result} *)

type result = {
  decision : decision option;
  system_message : string option;
  hook_specific_output : Jsont.json option;
  unknown : Unknown.t;
}
(** Generic result structure for hooks *)

val result_jsont : result Jsont.t

(** {1 Typed Hook Modules} *)

(** PreToolUse hook - fires before tool execution *)
module PreToolUse : sig
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Jsont.json;
    unknown : Unknown.t;
  }
  (** Typed input for PreToolUse hooks *)

  type t = input

  val of_json : Jsont.json -> t
  (** Parse hook input from JSON *)

  val session_id : t -> string
  (** {2 Accessors} *)

  val transcript_path : t -> string
  val tool_name : t -> string
  val tool_input : t -> Jsont.json
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t

  type permission_decision = [ `Allow | `Deny | `Ask ]
  (** Permission decision for tool usage *)

  val permission_decision_jsont : permission_decision Jsont.t

  type output = {
    permission_decision : permission_decision option;
    permission_decision_reason : string option;
    updated_input : Jsont.json option;
    unknown : Unknown.t;
  }
  (** Typed output for PreToolUse hooks *)

  val output_jsont : output Jsont.t

  val allow :
    ?reason:string ->
    ?updated_input:Jsont.json ->
    ?unknown:Unknown.t ->
    unit ->
    output
  (** {2 Response Builders} *)

  val deny : ?reason:string -> ?unknown:Unknown.t -> unit -> output
  val ask : ?reason:string -> ?unknown:Unknown.t -> unit -> output
  val continue : ?unknown:Unknown.t -> unit -> output

  val output_to_json : output -> Jsont.json
  (** Convert output to JSON for hook_specific_output *)
end

(** PostToolUse hook - fires after tool execution *)
module PostToolUse : sig
  type input = {
    session_id : string;
    transcript_path : string;
    tool_name : string;
    tool_input : Jsont.json;
    tool_response : Jsont.json;
    unknown : Unknown.t;
  }

  type t = input

  val of_json : Jsont.json -> t
  val session_id : t -> string
  val transcript_path : t -> string
  val tool_name : t -> string
  val tool_input : t -> Jsont.json
  val tool_response : t -> Jsont.json
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t

  type output = {
    decision : decision option;
    reason : string option;
    additional_context : string option;
    unknown : Unknown.t;
  }

  val output_jsont : output Jsont.t

  val continue :
    ?additional_context:string -> ?unknown:Unknown.t -> unit -> output

  val block :
    ?reason:string ->
    ?additional_context:string ->
    ?unknown:Unknown.t ->
    unit ->
    output

  val output_to_json : output -> Jsont.json
end

(** UserPromptSubmit hook - fires when user submits a prompt *)
module UserPromptSubmit : sig
  type input = {
    session_id : string;
    transcript_path : string;
    prompt : string;
    unknown : Unknown.t;
  }

  type t = input

  val of_json : Jsont.json -> t
  val session_id : t -> string
  val transcript_path : t -> string
  val prompt : t -> string
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t

  type output = {
    decision : decision option;
    reason : string option;
    additional_context : string option;
    unknown : Unknown.t;
  }

  val output_jsont : output Jsont.t

  val continue :
    ?additional_context:string -> ?unknown:Unknown.t -> unit -> output

  val block : ?reason:string -> ?unknown:Unknown.t -> unit -> output
  val output_to_json : output -> Jsont.json
end

(** Stop hook - fires when conversation stops *)
module Stop : sig
  type input = {
    session_id : string;
    transcript_path : string;
    stop_hook_active : bool;
    unknown : Unknown.t;
  }

  type t = input

  val of_json : Jsont.json -> t
  val session_id : t -> string
  val transcript_path : t -> string
  val stop_hook_active : t -> bool
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t

  type output = {
    decision : decision option;
    reason : string option;
    unknown : Unknown.t;
  }

  val output_jsont : output Jsont.t
  val continue : ?unknown:Unknown.t -> unit -> output
  val block : ?reason:string -> ?unknown:Unknown.t -> unit -> output
  val output_to_json : output -> Jsont.json
end

(** SubagentStop hook - fires when a subagent stops *)
module SubagentStop : sig
  type input = Stop.input
  type t = input
  type output = Stop.output

  val of_json : Jsont.json -> t
  val session_id : t -> string
  val transcript_path : t -> string
  val stop_hook_active : t -> bool
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t
  val output_jsont : output Jsont.t
  val continue : ?unknown:Unknown.t -> unit -> output
  val block : ?reason:string -> ?unknown:Unknown.t -> unit -> output
  val output_to_json : output -> Jsont.json
end

(** PreCompact hook - fires before message compaction *)
module PreCompact : sig
  type input = {
    session_id : string;
    transcript_path : string;
    unknown : Unknown.t;
  }

  type t = input
  type output = unit

  val of_json : Jsont.json -> t
  val session_id : t -> string
  val transcript_path : t -> string
  val unknown : t -> Unknown.t
  val input_jsont : input Jsont.t
  val continue : unit -> output
  val output_to_json : output -> Jsont.json
end

(** {1 Callbacks} *)

type callback =
  input:Jsont.json -> tool_use_id:string option -> context:Context.t -> result
(** Generic callback function type.

    Callbacks receive:
    - [input]: Raw JSON input (parse with [PreToolUse.of_json], etc.)
    - [tool_use_id]: Optional tool use ID
    - [context]: Hook context

    And return a generic [result] with optional hook-specific output. *)

(** {1 Matchers} *)

type matcher = {
  matcher : string option;
      (** Pattern to match (e.g., "Bash" or "Write|Edit") *)
  callbacks : callback list;  (** Callbacks to invoke on match *)
}
(** A matcher configuration *)

type config = (event * matcher list) list
(** Hook configuration: map from events to matchers *)

(** {1 Generic Result Builders} *)

val continue :
  ?system_message:string ->
  ?hook_specific_output:Jsont.json ->
  ?unknown:Unknown.t ->
  unit ->
  result
(** [continue ?system_message ?hook_specific_output ?unknown ()] creates a
    continue result *)

val block :
  ?system_message:string ->
  ?hook_specific_output:Jsont.json ->
  ?unknown:Unknown.t ->
  unit ->
  result
(** [block ?system_message ?hook_specific_output ?unknown ()] creates a block
    result *)

(** {1 Configuration Builders} *)

val matcher : ?pattern:string -> callback list -> matcher
(** [matcher ?pattern callbacks] creates a matcher *)

val empty : config
(** Empty hooks configuration *)

val add : event -> matcher list -> config -> config
(** [add event matchers config] adds matchers for an event *)

(** {1 JSON Serialization} *)

val result_to_json : result -> Jsont.json
val config_to_protocol_format : config -> Jsont.json
