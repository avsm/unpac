(** Error handling for the Claude protocol.

    This module provides a protocol-specific exception and Result combinators
    for handling JSON encoding/decoding errors in the Claude SDK. *)

exception Protocol_error of string
(** Raised when there is an error in the Claude protocol, such as JSON
    encoding/decoding failures or malformed messages. *)

(** [protocol_error msg] raises [Protocol_error msg]. *)
let protocol_error msg = raise (Protocol_error msg)

(** [get_ok ~msg r] returns [x] if [r] is [Ok x], or raises
    [Protocol_error (msg ^ e)] if [r] is [Error e]. *)
let get_ok ~msg = function
  | Ok x -> x
  | Error e -> raise (Protocol_error (msg ^ e))

(** [get_ok' ~msg r] returns [x] if [r] is [Ok x], or raises
    [Invalid_argument (msg ^ e)] if [r] is [Error e]. Use this for user-facing
    parse errors where Invalid_argument is expected. *)
let get_ok' ~msg = function
  | Ok x -> x
  | Error e -> raise (Invalid_argument (msg ^ e))
