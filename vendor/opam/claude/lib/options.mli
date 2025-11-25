(** Configuration options for Claude sessions.

    This module provides comprehensive configuration options for controlling
    Claude's behavior, including tool permissions, system prompts, models,
    execution environment, cost controls, and structured outputs.

    {2 Overview}

    Options control all aspects of Claude's behavior:
    - {b Permissions}: Which tools Claude can use and how permission is granted
    - {b Models}: Which AI model to use and fallback options
    - {b Environment}: Working directory, environment variables, settings
    - {b Cost Control}: Budget limits to prevent runaway spending
    - {b Hooks}: Intercept and modify tool execution
    - {b Structured Output}: JSON schema validation for responses
    - {b Session Management}: Continue or resume conversations

    {2 Builder Pattern}

    Options use a functional builder pattern - each [with_*] function returns
    a new options value with the specified field updated:

    {[
      let options = Options.default
        |> Options.with_model "claude-sonnet-4-5"
        |> Options.with_max_budget_usd 1.0
        |> Options.with_permission_mode Permissions.Mode.Accept_edits
    ]}

    {2 Common Configuration Scenarios}

    {3 CI/CD: Isolated, Reproducible Builds}

    {[
      let ci_config = Options.default
        |> Options.with_no_settings           (* Ignore user config *)
        |> Options.with_max_budget_usd 0.50   (* 50 cent limit *)
        |> Options.with_permission_mode
             Permissions.Mode.Bypass_permissions
        |> Options.with_model "claude-haiku-4"
    ]}

    {3 Production: Cost Control with Fallback}

    {[
      let prod_config = Options.default
        |> Options.with_model "claude-sonnet-4-5"
        |> Options.with_fallback_model "claude-haiku-4"
        |> Options.with_max_budget_usd 10.0   (* $10 daily limit *)
        |> Options.with_max_buffer_size 5_000_000
    ]}

    {3 Development: User Settings with Overrides}

    {[
      let dev_config = Options.default
        |> Options.with_setting_sources [User; Project]
        |> Options.with_max_budget_usd 1.0
        |> Options.with_permission_mode Permissions.Mode.Default
    ]}

    {3 Structured Output: Type-Safe Responses}

    {[
      let schema = Jsont.json_of_json (`O [
        ("type", `String "object");
        ("properties", `O [
          ("count", `O [("type", `String "integer")]);
          ("has_tests", `O [("type", `String "boolean")]);
        ]);
      ])
      let format = Structured_output.of_json_schema schema

      let analysis_config = Options.default
        |> Options.with_output_format format
        |> Options.with_allowed_tools ["Read"; "Glob"; "Grep"]
    ]}

    {2 Advanced Options}

    {3 Budget Control}

    Use {!with_max_budget_usd} to set hard spending limits. Claude will
    terminate the session if the budget is exceeded, preventing runaway costs.

    {3 Settings Isolation}

    Use {!with_setting_sources} or {!with_no_settings} to control which
    configuration files are loaded:
    - [User] - ~/.claude/config
    - [Project] - .claude/ in project root
    - [Local] - Current directory settings
    - [Some \[\]] (via {!with_no_settings}) - No settings, fully isolated

    This is critical for reproducible builds in CI/CD environments.

    {3 Model Fallback}

    Use {!with_fallback_model} to specify an alternative model when the
    primary model is unavailable or overloaded. This improves reliability. *)

(** The log source for options operations *)
val src : Logs.Src.t

(** {1 Types} *)

type setting_source = User | Project | Local
(** Setting source determines which configuration files to load.
    - [User]: Load user-level settings from ~/.claude/config
    - [Project]: Load project-level settings from .claude/ in project root
    - [Local]: Load local settings from current directory *)

type t
(** The type of configuration options. *)

val default : t
(** [default] returns the default configuration with sensible defaults:
    - No tool restrictions
    - 8000 max thinking tokens
    - Default allow permission callback
    - No custom prompts or model override *)

val create :
  ?allowed_tools:string list ->
  ?disallowed_tools:string list ->
  ?max_thinking_tokens:int ->
  ?system_prompt:string ->
  ?append_system_prompt:string ->
  ?permission_mode:Permissions.Mode.t ->
  ?permission_callback:Permissions.callback ->
  ?model:Model.t ->
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  ?env:(string * string) list ->
  ?continue_conversation:bool ->
  ?resume:string ->
  ?max_turns:int ->
  ?permission_prompt_tool_name:string ->
  ?settings:string ->
  ?add_dirs:string list ->
  ?extra_args:(string * string option) list ->
  ?debug_stderr:Eio.Flow.sink_ty Eio.Flow.sink ->
  ?hooks:Hooks.config ->
  ?max_budget_usd:float ->
  ?fallback_model:Model.t ->
  ?setting_sources:setting_source list ->
  ?max_buffer_size:int ->
  ?user:string ->
  ?output_format:Structured_output.t ->
  ?unknown:Jsont.json ->
  unit -> t
(** [create ?allowed_tools ?disallowed_tools ?max_thinking_tokens ?system_prompt
    ?append_system_prompt ?permission_mode ?permission_callback ?model ?cwd ?env
    ?continue_conversation ?resume ?max_turns ?permission_prompt_tool_name ?settings
    ?add_dirs ?extra_args ?debug_stderr ?hooks ?max_budget_usd ?fallback_model
    ?setting_sources ?max_buffer_size ?user ()]
    creates a new configuration.
    @param allowed_tools List of explicitly allowed tool names
    @param disallowed_tools List of explicitly disallowed tool names
    @param max_thinking_tokens Maximum tokens for thinking blocks (default: 8000)
    @param system_prompt Replace the default system prompt
    @param append_system_prompt Append to the default system prompt
    @param permission_mode Permission mode to use
    @param permission_callback Custom permission callback
    @param model Override the default model
    @param cwd Working directory for file operations
    @param env Environment variables to set
    @param continue_conversation Continue an existing conversation
    @param resume Resume from a specific session ID
    @param max_turns Maximum number of conversation turns
    @param permission_prompt_tool_name Tool name for permission prompts
    @param settings Path to settings file
    @param add_dirs Additional directories to allow access to
    @param extra_args Additional CLI flags to pass through
    @param debug_stderr Sink for debug output when debug-to-stderr is set
    @param hooks Hooks configuration for event interception
    @param max_budget_usd Hard spending limit in USD (terminates on exceed)
    @param fallback_model Automatic fallback on primary model unavailability
    @param setting_sources Control which settings load (user/project/local)
    @param max_buffer_size Control for stdout buffer size in bytes
    @param user Unix user for subprocess execution
    @param output_format Optional structured output format specification *)

(** {1 Accessors} *)

val allowed_tools : t -> string list
(** [allowed_tools t] returns the list of allowed tools. *)

val disallowed_tools : t -> string list
(** [disallowed_tools t] returns the list of disallowed tools. *)

val max_thinking_tokens : t -> int
(** [max_thinking_tokens t] returns the maximum thinking tokens. *)

val system_prompt : t -> string option
(** [system_prompt t] returns the optional system prompt override. *)

val append_system_prompt : t -> string option
(** [append_system_prompt t] returns the optional system prompt append. *)

val permission_mode : t -> Permissions.Mode.t option
(** [permission_mode t] returns the optional permission mode. *)

val permission_callback : t -> Permissions.callback option
(** [permission_callback t] returns the optional permission callback. *)

val model : t -> Model.t option
(** [model t] returns the optional model override. *)

val cwd : t -> Eio.Fs.dir_ty Eio.Path.t option
(** [cwd t] returns the optional working directory. *)

val env : t -> (string * string) list
(** [env t] returns the environment variables. *)

val continue_conversation : t -> bool
(** [continue_conversation t] returns whether to continue an existing conversation. *)

val resume : t -> string option
(** [resume t] returns the optional session ID to resume. *)

val max_turns : t -> int option
(** [max_turns t] returns the optional maximum number of turns. *)

val permission_prompt_tool_name : t -> string option
(** [permission_prompt_tool_name t] returns the optional tool name for permission prompts. *)

val settings : t -> string option
(** [settings t] returns the optional path to settings file. *)

val add_dirs : t -> string list
(** [add_dirs t] returns the list of additional allowed directories. *)

val extra_args : t -> (string * string option) list
(** [extra_args t] returns the additional CLI flags. *)

val debug_stderr : t -> Eio.Flow.sink_ty Eio.Flow.sink option
(** [debug_stderr t] returns the optional debug output sink. *)

val hooks : t -> Hooks.config option
(** [hooks t] returns the optional hooks configuration. *)

val max_budget_usd : t -> float option
(** [max_budget_usd t] returns the optional spending limit in USD. *)

val fallback_model : t -> Model.t option
(** [fallback_model t] returns the optional fallback model. *)

val setting_sources : t -> setting_source list option
(** [setting_sources t] returns the optional list of setting sources to load. *)

val max_buffer_size : t -> int option
(** [max_buffer_size t] returns the optional stdout buffer size in bytes. *)

val user : t -> string option
(** [user t] returns the optional Unix user for subprocess execution. *)

val output_format : t -> Structured_output.t option
(** [output_format t] returns the optional structured output format. *)

val unknown : t -> Jsont.json
(** [unknown t] returns any unknown JSON fields that were preserved during decoding. *)

(** {1 Builders} *)

val with_allowed_tools : string list -> t -> t
(** [with_allowed_tools tools t] sets the allowed tools. *)

val with_disallowed_tools : string list -> t -> t
(** [with_disallowed_tools tools t] sets the disallowed tools. *)

val with_max_thinking_tokens : int -> t -> t
(** [with_max_thinking_tokens tokens t] sets the maximum thinking tokens. *)

val with_system_prompt : string -> t -> t
(** [with_system_prompt prompt t] sets the system prompt override. *)

val with_append_system_prompt : string -> t -> t
(** [with_append_system_prompt prompt t] sets the system prompt append. *)

val with_permission_mode : Permissions.Mode.t -> t -> t
(** [with_permission_mode mode t] sets the permission mode. *)

val with_permission_callback : Permissions.callback -> t -> t
(** [with_permission_callback callback t] sets the permission callback. *)

val with_model : Model.t -> t -> t
(** [with_model model t] sets the model override using a typed Model.t. *)

val with_model_string : string -> t -> t
(** [with_model_string model t] sets the model override from a string.
    The string is parsed using {!Model.of_string}. *)

val with_cwd : Eio.Fs.dir_ty Eio.Path.t -> t -> t
(** [with_cwd cwd t] sets the working directory. *)

val with_env : (string * string) list -> t -> t
(** [with_env env t] sets the environment variables. *)

val with_continue_conversation : bool -> t -> t
(** [with_continue_conversation continue t] sets whether to continue conversation. *)

val with_resume : string -> t -> t
(** [with_resume session_id t] sets the session ID to resume. *)

val with_max_turns : int -> t -> t
(** [with_max_turns turns t] sets the maximum number of turns. *)

val with_permission_prompt_tool_name : string -> t -> t
(** [with_permission_prompt_tool_name tool t] sets the permission prompt tool name. *)

val with_settings : string -> t -> t
(** [with_settings path t] sets the path to settings file. *)

val with_add_dirs : string list -> t -> t
(** [with_add_dirs dirs t] sets the additional allowed directories. *)

val with_extra_args : (string * string option) list -> t -> t
(** [with_extra_args args t] sets the additional CLI flags. *)

val with_debug_stderr : Eio.Flow.sink_ty Eio.Flow.sink -> t -> t
(** [with_debug_stderr sink t] sets the debug output sink. *)

val with_hooks : Hooks.config -> t -> t
(** [with_hooks hooks t] sets the hooks configuration. *)

val with_max_budget_usd : float -> t -> t
(** [with_max_budget_usd budget t] sets the maximum spending limit in USD.
    The session will terminate if this limit is exceeded. *)

val with_fallback_model : Model.t -> t -> t
(** [with_fallback_model model t] sets the fallback model using a typed Model.t. *)

val with_fallback_model_string : string -> t -> t
(** [with_fallback_model_string model t] sets the fallback model from a string.
    The string is parsed using {!Model.of_string}. *)

val with_setting_sources : setting_source list -> t -> t
(** [with_setting_sources sources t] sets which configuration sources to load.
    Use empty list for isolated environments (e.g., CI/CD). *)

val with_no_settings : t -> t
(** [with_no_settings t] disables all settings loading (user, project, local).
    Useful for CI/CD environments where you want isolated, reproducible behavior. *)

val with_max_buffer_size : int -> t -> t
(** [with_max_buffer_size size t] sets the maximum stdout buffer size in bytes. *)

val with_user : string -> t -> t
(** [with_user user t] sets the Unix user for subprocess execution. *)

val with_output_format : Structured_output.t -> t -> t
(** [with_output_format format t] sets the structured output format. *)

(** {1 Serialization} *)

val jsont : t Jsont.t
(** [jsont] is the Jsont codec for Options.t *)

val to_json : t -> Jsont.json
(** [to_json t] converts options to JSON representation. *)

val of_json : Jsont.json -> t
(** [of_json json] parses options from JSON.
    @raise Invalid_argument if the JSON is not valid options. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints the options. *)

(** {1 Logging} *)

val log_options : t -> unit
(** [log_options t] logs the current options configuration. *)