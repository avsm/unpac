(** Incoming messages from the Claude CLI.

    This module defines a discriminated union of all possible message types
    that can be received from the Claude CLI, with a single jsont codec.

    The codec uses the "type" field to discriminate between message types:
    - "user", "assistant", "system", "result" -> Message variant
    - "control_response" -> Control_response variant
    - "control_request" -> Control_request variant

    This provides a clean, type-safe way to decode incoming messages in a single
    operation, avoiding the parse-then-switch-then-parse pattern. *)

(** Control request types for incoming control_request messages *)
module Control_request : sig
  (** Can use tool permission request *)
  module Can_use_tool : sig
    type t

    val tool_name : t -> string
    val input : t -> Jsont.json
    val permission_suggestions : t -> Jsont.json list
    val jsont : t Jsont.t
  end

  (** Hook callback request *)
  module Hook_callback : sig
    type t

    val callback_id : t -> string
    val input : t -> Jsont.json
    val tool_use_id : t -> string option
    val jsont : t Jsont.t
  end

  (** Request payload - discriminated by subtype *)
  type request =
    | Can_use_tool of Can_use_tool.t
    | Hook_callback of Hook_callback.t
    | Unknown of string * Jsont.json

  (** Full control request message *)
  type t

  val request_id : t -> string
  val request : t -> request
  val subtype : t -> string
  val jsont : t Jsont.t
end

type t =
  | Message of Message.t
  | Control_response of Sdk_control.control_response
  | Control_request of Control_request.t

val jsont : t Jsont.t
(** Codec for incoming messages. Uses the "type" field to discriminate. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints the incoming message. *)
