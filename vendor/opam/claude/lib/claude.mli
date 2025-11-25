(** OCaml Eio library for Claude Code CLI.

    This library provides an interface to the Claude Code command-line interface
    using OCaml's Eio concurrency library. It wraps Claude CLI invocations with
    JSON streaming for asynchronous communication.

    {1 Overview}

    The Claude library enables you to:
    - Send messages to Claude and receive streaming responses
    - Control tool permissions and execution
    - Configure system prompts and model parameters
    - Handle content blocks including text, tool use, and thinking blocks
    - Manage sessions with proper resource cleanup

    {1 Architecture}

    The library is structured into several focused modules:

    - {!Content_block}: Defines content blocks (text, tool use, tool results,
      thinking)
    - {!Message}: Messages exchanged with Claude (user, assistant, system,
      result)
    - {!Control}: Control flow messages for session management
    - {!Permissions}: Fine-grained permission system for tool usage
    - {!Options}: Configuration options for Claude sessions
    - {!Transport}: Low-level transport layer for CLI communication
    - {!Client}: High-level client interface for interacting with Claude

    {1 Basic Usage}

    {[
      open Claude

      (* Create a simple query *)
      let query_claude ~sw env prompt =
        let options = Options.default in
        Client.query ~sw env ~options prompt

      (* Process streaming responses *)
      let process_response messages =
        Seq.iter
          (function
            | Message.Assistant msg ->
                List.iter
                  (function
                    | Content_block.Text t ->
                        print_endline (Content_block.Text.text t)
                    | _ -> ())
                  (Message.Assistant.content msg)
            | _ -> ())
          messages
    ]}

    {1 Advanced Features}

    {2 Tool Permissions}

    Control which tools Claude can use and how:

    {[
      let options =
        Options.default
        |> Options.with_allowed_tools [ "Read"; "Write"; "Bash" ]
        |> Options.with_permission_mode Permissions.Mode.Accept_edits
    ]}

    {2 Custom Permission Callbacks}

    Implement custom logic for tool approval:

    {[
      let my_callback ~tool_name ~input ~context =
        if tool_name = "Bash" then
          Permissions.Result.deny ~message:"Bash not allowed" ~interrupt:false
        else Permissions.Result.allow ()

      let options =
        Options.default |> Options.with_permission_callback my_callback
    ]}

    {2 System Prompts}

    Customize Claude's behavior with system prompts:

    {[
      let options =
        Options.default
        |> Options.with_system_prompt
             "You are a helpful OCaml programming assistant."
        |> Options.with_append_system_prompt "Always use Jane Street style."
    ]}

    {1 Logging}

    The library uses the Logs library for structured logging. Each module has
    its own log source (e.g., "claude.message", "claude.transport") allowing
    fine-grained control over logging verbosity:

    {[
      (* Enable debug logging for message handling *)
      Logs.Src.set_level Message.src (Some Logs.Debug);

      (* Enable info logging for transport layer *)
      Logs.Src.set_level Transport.src (Some Logs.Info)
    ]}

    {1 Error Handling}

    The library uses exceptions for error handling. Common exceptions include:
    - [Invalid_argument]: For malformed JSON or invalid parameters
    - [Transport.Transport_error]: For CLI communication failures
    - [Message.Message_parse_error]: For message parsing failures

    {1 Example: Complete Session}

    {[
      let run_claude_session ~sw env =
        let options =
          Options.create ~allowed_tools:[ "Read"; "Write" ]
            ~permission_mode:Permissions.Mode.Accept_edits
            ~system_prompt:"You are an OCaml expert." ~max_thinking_tokens:10000
            ()
        in

        let prompt = "Write a function to calculate fibonacci numbers" in
        let messages = Client.query ~sw env ~options prompt in

        Seq.iter
          (fun msg ->
            Message.log_received msg;
            match msg with
            | Message.Assistant assistant ->
                Printf.printf "Claude: %s\n" (Message.Assistant.model assistant);
                List.iter
                  (function
                    | Content_block.Text t ->
                        print_endline (Content_block.Text.text t)
                    | Content_block.Tool_use t ->
                        Printf.printf "Using tool: %s\n"
                          (Content_block.Tool_use.name t)
                    | _ -> ())
                  (Message.Assistant.content assistant)
            | Message.Result result ->
                Printf.printf "Session complete. Duration: %dms\n"
                  (Message.Result.duration_ms result)
            | _ -> ())
          messages
    ]} *)

(** {1 Modules} *)

module Client = Client
(** High-level client interface for Claude interactions. *)

module Options = Options
(** Configuration options for Claude sessions. *)

module Model = Model
(** Claude AI model identifiers with type-safe variants. *)

module Content_block = Content_block
(** Content blocks for messages (text, tool use, tool results, thinking). *)

module Message = Message
(** Messages exchanged with Claude (user, assistant, system, result). *)

module Control = Control
(** Control messages for session management. *)

module Permissions = Permissions
(** Permission system for tool invocations. *)

module Hooks = Hooks
(** Hooks system for event interception. *)

module Sdk_control = Sdk_control
(** SDK control protocol for dynamic configuration. *)

module Incoming = Incoming
(** Discriminated union of all incoming message types from Claude CLI. *)

module Structured_output = Structured_output
(** Structured output support using JSON Schema. *)

module Transport = Transport
(** Low-level transport layer for CLI communication. *)
