(** The log source for transport operations *)
val src : Logs.Src.t

exception CLI_not_found of string
exception Process_error of string
exception Connection_error of string

type t

val create :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  options:Options.t ->
  unit -> t

val send : t -> Jsont.json -> unit
val receive_line : t -> string option
val interrupt : t -> unit
val close : t -> unit
