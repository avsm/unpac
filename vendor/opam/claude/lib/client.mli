(** Client interface for interacting with Claude.

    This module provides the high-level client API for sending messages to
    Claude and receiving responses. It handles the bidirectional streaming
    protocol, permission callbacks, and hooks.

    {2 Basic Usage}

    {[
      Eio.Switch.run @@ fun sw ->
      let client = Client.create ~sw ~process_mgr () in
      Client.query client "What is 2+2?";

      let messages = Client.receive_all client in
      List.iter
        (function
          | Message.Assistant msg ->
              Printf.printf "Claude: %s\n" (Message.Assistant.text msg)
          | _ -> ())
        messages
    ]}

    {2 Features}

    - {b Message Streaming}: Messages are streamed lazily via {!Seq.t}
    - {b Permission Control}: Custom permission callbacks for tool usage
    - {b Hooks}: Intercept and modify tool execution
    - {b Dynamic Control}: Change settings mid-conversation
    - {b Resource Management}: Automatic cleanup via Eio switches

    {2 Message Flow}

    1. Create a client with {!create} 2. Send messages with {!query} or
    {!send_message} 3. Receive responses with {!receive} or {!receive_all} 4.
    Continue multi-turn conversations by sending more messages 5. Client
    automatically cleans up when the switch exits

    {2 Advanced Features}

    - Permission discovery mode for understanding required permissions
    - Mid-conversation model switching and permission mode changes
    - Server capability introspection *)

val src : Logs.Src.t
(** The log source for client operations *)

type t
(** The type of Claude clients. *)

val session_id : t -> string option
(** [session_id t] returns the session ID if one has been received from Claude.
    The session ID is provided in system init messages and uniquely identifies
    the current conversation session. *)

val create :
  ?options:Options.t ->
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  unit ->
  t
(** [create ?options ~sw ~process_mgr ()] creates a new Claude client.

    @param options Configuration options (defaults to {!Options.default})
    @param sw Eio switch for resource management
    @param process_mgr Eio process manager for spawning the Claude CLI *)

val query : t -> string -> unit
(** [query t prompt] sends a text message to Claude.

    This is a convenience function for simple string messages. For more complex
    messages with tool results or multiple content blocks, use {!send_message}
    instead. *)

val send_message : t -> Message.t -> unit
(** [send_message t msg] sends a message to Claude.

    Supports all message types including user messages with tool results. *)

val send_user_message : t -> Message.User.t -> unit
(** [send_user_message t msg] sends a user message to Claude. *)

val receive : t -> Message.t Seq.t
(** [receive t] returns a lazy sequence of messages from Claude.

    The sequence yields messages as they arrive from Claude, including:
    - {!constructor:Message.Assistant} - Claude's responses
    - {!constructor:Message.System} - System notifications
    - {!constructor:Message.Result} - Final result with usage statistics

    Control messages (permission requests, hook callbacks) are handled
    internally and not yielded to the sequence. *)

val receive_all : t -> Message.t list
(** [receive_all t] collects all messages into a list.

    This is a convenience function that consumes the {!receive} sequence. Use
    this when you want to process all messages at once rather than streaming
    them. *)

val interrupt : t -> unit
(** [interrupt t] sends an interrupt signal to stop Claude's execution. *)

val discover_permissions : t -> t
(** [discover_permissions t] enables permission discovery mode.

    In discovery mode, all tool usage is logged but allowed. Use
    {!get_discovered_permissions} to retrieve the list of permissions that were
    requested during execution.

    This is useful for understanding what permissions your prompt requires. *)

val get_discovered_permissions : t -> Permissions.Rule.t list
(** [get_discovered_permissions t] returns permissions discovered during
    execution.

    Only useful after enabling {!discover_permissions}. *)

val with_permission_callback : t -> Permissions.callback -> t
(** [with_permission_callback t callback] updates the permission callback.

    Allows dynamically changing the permission callback without recreating the
    client. *)

(** {1 Dynamic Control Methods}

    These methods allow you to change Claude's behavior mid-conversation without
    recreating the client. This is useful for:

    - Adjusting permission strictness based on user feedback
    - Switching to faster/cheaper models for simple tasks
    - Adapting to changing requirements during long conversations
    - Introspecting server capabilities

    {2 Example: Adaptive Permission Control}

    {[
      (* Start with strict permissions *)
      let client = Client.create ~sw ~process_mgr
        ~options:(Options.default
                  |> Options.with_permission_mode Permissions.Mode.Default) ()
      in

      Client.query client "Analyze this code";
      let _ = Client.receive_all client in

      (* User approves, switch to auto-accept edits *)
      Client.set_permission_mode client Permissions.Mode.Accept_edits;

      Client.query client "Now refactor it";
      let _ = Client.receive_all client in
    ]}

    {2 Example: Model Switching for Efficiency}

    {[
      (* Use powerful model for complex analysis *)
      let client = Client.create ~sw ~process_mgr
        ~options:(Options.default |> Options.with_model "claude-sonnet-4-5") ()
      in

      Client.query client "Design a new architecture for this system";
      let _ = Client.receive_all client in

      (* Switch to faster model for simple tasks *)
      Client.set_model client "claude-haiku-4";

      Client.query client "Now write a README";
      let _ = Client.receive_all client in
    ]}

    {2 Example: Server Introspection}

    {[
      let info = Client.get_server_info client in
      Printf.printf "Claude CLI version: %s\n"
        (Sdk_control.Server_info.version info);
      Printf.printf "Capabilities: %s\n"
        (String.concat ", " (Sdk_control.Server_info.capabilities info))
    ]} *)

val set_permission_mode : t -> Permissions.Mode.t -> unit
(** [set_permission_mode t mode] changes the permission mode mid-conversation.

    This allows switching between permission modes without recreating the
    client:
    - {!Permissions.Mode.Default} - Prompt for all permissions
    - {!Permissions.Mode.Accept_edits} - Auto-accept file edits
    - {!Permissions.Mode.Plan} - Planning mode with restricted execution
    - {!Permissions.Mode.Bypass_permissions} - Skip all permission checks

    @raise Failure if the server returns an error *)

val set_model : t -> Model.t -> unit
(** [set_model t model] switches to a different AI model mid-conversation.

    Common models:
    - [`Sonnet_4_5] - Most capable, balanced performance
    - [`Opus_4] - Maximum capability for complex tasks
    - [`Haiku_4] - Fast and cost-effective

    @raise Failure if the model is invalid or unavailable *)

val get_server_info : t -> Sdk_control.Server_info.t
(** [get_server_info t] retrieves server capabilities and metadata.

    Returns information about:
    - Server version string
    - Available capabilities
    - Supported commands
    - Available output styles

    Useful for feature detection and debugging.

    @raise Failure if the server returns an error *)
