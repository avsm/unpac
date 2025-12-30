(** Minimal WebSocket server for live agent UI.

    Serves a single-page web UI that displays agent events in real-time. *)

type t
(** Web server state. *)

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  port:int ->
  Event.bus ->
  t
(** [start ~sw ~net ~port event_bus] starts the web server.

    Listens on [port] and serves:
    - GET / - The HTML UI
    - WS /ws - WebSocket for event streaming

    Subscribes to [event_bus] and broadcasts all events to connected clients. *)

val broadcast : t -> Event.t -> unit
(** [broadcast t event] sends an event to all connected WebSocket clients. *)
