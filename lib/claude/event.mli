(** Event types for live agent UI updates. *)

(** Tool call event data. *)
type tool_call = {
  id : string;
  name : string;
  input : string;
}

(** Tool result event data. *)
type tool_result = {
  id : string;
  name : string;
  output : string;
  is_error : bool;
}

(** Agent events. *)
type t =
  | Thinking
  | Text of string
  | Tool_call of tool_call
  | Tool_result of tool_result
  | Error of string
  | Sync of string
  | Turn_complete of { turn : int; cost_usd : float option }
  | Agent_start
  | Agent_stop

val to_json : t -> string
(** Convert event to JSON string. *)

(** Event bus for broadcasting to listeners. *)

type listener = t -> unit
type bus

val create_bus : unit -> bus
val subscribe : bus -> listener -> unit
val unsubscribe : bus -> listener -> unit
val emit : bus -> t -> unit
