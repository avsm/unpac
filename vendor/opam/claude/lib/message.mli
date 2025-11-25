(** Messages exchanged with Claude.

    This module defines the various types of messages that can be sent to and
    received from Claude, including user input, assistant responses, system
    messages, and result metadata. *)

(** The log source for message operations *)
val src : Logs.Src.t

(** {1 User Messages} *)

module User : sig
  (** Messages sent by the user. *)

  type content =
    | String of string  (** Simple text message *)
    | Blocks of Content_block.t list  (** Complex message with multiple content blocks *)
  (** The content of a user message. *)

  type t
  (** The type of user messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for user messages. *)

  val create_string : string -> t
  (** [create_string s] creates a user message with simple text content. *)

  val create_blocks : Content_block.t list -> t
  (** [create_blocks blocks] creates a user message with content blocks. *)

  val create_with_tool_result :
    tool_use_id:string ->
    content:string ->
    ?is_error:bool ->
    unit -> t
  (** [create_with_tool_result ~tool_use_id ~content ?is_error ()] creates a user
      message containing a tool result. *)

  val create_mixed : text:string option -> tool_results:(string * string * bool option) list -> t
  (** [create_mixed ?text ~tool_results] creates a user message with optional text
      and tool results. Each tool result is (tool_use_id, content, is_error). *)

  val content : t -> content
  (** [content t] returns the content of the user message. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)

  val as_text : t -> string option
  (** [as_text t] returns the text content if the message is a simple string, None otherwise. *)

  val get_blocks : t -> Content_block.t list
  (** [get_blocks t] returns the content blocks, or a single text block if it's a string message. *)

  val to_json : t -> Jsont.json
  (** [to_json t] converts the user message to its JSON representation. *)

  val of_json : Jsont.json -> t
  (** [of_json json] parses a user message from JSON.
      @raise Invalid_argument if the JSON is not a valid user message. *)
end

(** {1 Assistant Messages} *)

module Assistant : sig
  (** Messages from Claude assistant. *)

  type error = [
    | `Authentication_failed  (** Authentication with Claude API failed *)
    | `Billing_error         (** Billing or account issue *)
    | `Rate_limit            (** Rate limit exceeded *)
    | `Invalid_request       (** Request was invalid *)
    | `Server_error          (** Internal server error *)
    | `Unknown               (** Unknown error type *)
  ]
  (** The type of assistant message errors based on Python SDK error types. *)

  val error_to_string : error -> string
  (** [error_to_string err] converts an error to its string representation. *)

  val error_of_string : string -> error
  (** [error_of_string s] parses an error string. Unknown strings become [`Unknown]. *)

  type t
  (** The type of assistant messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for assistant messages. *)

  val create : content:Content_block.t list -> model:string -> ?error:error -> unit -> t
  (** [create ~content ~model ?error ()] creates an assistant message.
      @param content List of content blocks in the response
      @param model The model identifier used for the response
      @param error Optional error that occurred during message generation *)

  val content : t -> Content_block.t list
  (** [content t] returns the content blocks of the assistant message. *)

  val model : t -> string
  (** [model t] returns the model identifier. *)

  val error : t -> error option
  (** [error t] returns the optional error that occurred during message generation. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)

  val get_text_blocks : t -> string list
  (** [get_text_blocks t] extracts all text content from the message. *)

  val get_tool_uses : t -> Content_block.Tool_use.t list
  (** [get_tool_uses t] extracts all tool use blocks from the message. *)

  val get_thinking : t -> Content_block.Thinking.t list
  (** [get_thinking t] extracts all thinking blocks from the message. *)

  val has_tool_use : t -> bool
  (** [has_tool_use t] returns true if the message contains any tool use blocks. *)

  val combined_text : t -> string
  (** [combined_text t] concatenates all text blocks into a single string. *)

  val to_json : t -> Jsont.json
  (** [to_json t] converts the assistant message to its JSON representation. *)

  val of_json : Jsont.json -> t
  (** [of_json json] parses an assistant message from JSON.
      @raise Invalid_argument if the JSON is not a valid assistant message. *)
end

(** {1 System Messages} *)

module System : sig
  (** System control and status messages.

      System messages use a discriminated union on the "subtype" field:
      - "init": Session initialization with session_id, model, cwd
      - "error": Error messages with error string
      - Other subtypes are preserved as [Other] *)

  type init = {
    session_id : string option;
    model : string option;
    cwd : string option;
    unknown : Unknown.t;
  }
  (** Init message fields. *)

  type error = {
    error : string;
    unknown : Unknown.t;
  }
  (** Error message fields. *)

  type other = {
    subtype : string;
    unknown : Unknown.t;
  }
  (** Unknown subtype fields. *)

  type t =
    | Init of init
    | Error of error
    | Other of other
  (** The type of system messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for system messages. *)

  (** {2 Constructors} *)

  val init : ?session_id:string -> ?model:string -> ?cwd:string -> unit -> t
  (** [init ?session_id ?model ?cwd ()] creates an init message. *)

  val error : error:string -> t
  (** [error ~error] creates an error message. *)

  val other : subtype:string -> t
  (** [other ~subtype] creates a message with unknown subtype. *)

  (** {2 Accessors} *)

  val session_id : t -> string option
  (** [session_id t] returns session_id from Init, None otherwise. *)

  val model : t -> string option
  (** [model t] returns model from Init, None otherwise. *)

  val cwd : t -> string option
  (** [cwd t] returns cwd from Init, None otherwise. *)

  val error_msg : t -> string option
  (** [error_msg t] returns error from Error, None otherwise. *)

  val subtype : t -> string
  (** [subtype t] returns the subtype string. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields. *)

  (** {2 Conversion} *)

  val to_json : t -> Jsont.json
  (** [to_json t] converts to JSON representation. *)

  val of_json : Jsont.json -> t
  (** [of_json json] parses from JSON.
      @raise Invalid_argument if invalid. *)
end

(** {1 Result Messages} *)

module Result : sig
  (** Final result messages with metadata about the conversation. *)

  module Usage : sig
    (** Usage statistics for API calls. *)

    type t
    (** Type for usage statistics. *)

    val jsont : t Jsont.t
    (** [jsont] is the Jsont codec for usage statistics. *)

    val create :
      ?input_tokens:int ->
      ?output_tokens:int ->
      ?total_tokens:int ->
      ?cache_creation_input_tokens:int ->
      ?cache_read_input_tokens:int ->
      unit -> t
    (** [create ?input_tokens ?output_tokens ?total_tokens ?cache_creation_input_tokens
        ?cache_read_input_tokens ()] creates usage statistics. *)

    val input_tokens : t -> int option
    (** [input_tokens t] returns the number of input tokens used. *)

    val output_tokens : t -> int option
    (** [output_tokens t] returns the number of output tokens generated. *)

    val total_tokens : t -> int option
    (** [total_tokens t] returns the total number of tokens. *)

    val cache_creation_input_tokens : t -> int option
    (** [cache_creation_input_tokens t] returns cache creation input tokens. *)

    val cache_read_input_tokens : t -> int option
    (** [cache_read_input_tokens t] returns cache read input tokens. *)

    val unknown : t -> Unknown.t
    (** [unknown t] returns the unknown fields preserved from JSON. *)

    val effective_input_tokens : t -> int
    (** [effective_input_tokens t] returns input tokens minus cached tokens, or 0 if not available. *)

    val total_cost_estimate : t -> input_price:float -> output_price:float -> float option
    (** [total_cost_estimate t ~input_price ~output_price] estimates the cost based on token
        prices per million tokens. Returns None if token counts are not available. *)
  end

  type t
  (** The type of result messages. *)

  val jsont : t Jsont.t
  (** [jsont] is the Jsont codec for result messages. *)

  val create :
    subtype:string ->
    duration_ms:int ->
    duration_api_ms:int ->
    is_error:bool ->
    num_turns:int ->
    session_id:string ->
    ?total_cost_usd:float ->
    ?usage:Usage.t ->
    ?result:string ->
    ?structured_output:Jsont.json ->
    unit -> t
  (** [create ~subtype ~duration_ms ~duration_api_ms ~is_error ~num_turns
      ~session_id ?total_cost_usd ?usage ?result ()] creates a result message.
      @param subtype The subtype of the result
      @param duration_ms Total duration in milliseconds
      @param duration_api_ms API duration in milliseconds
      @param is_error Whether the result represents an error
      @param num_turns Number of conversation turns
      @param session_id Unique session identifier
      @param total_cost_usd Optional total cost in USD
      @param usage Optional usage statistics as JSON
      @param result Optional result string
      @param structured_output Optional structured JSON output from Claude *)

  val subtype : t -> string
  (** [subtype t] returns the subtype of the result. *)

  val duration_ms : t -> int
  (** [duration_ms t] returns the total duration in milliseconds. *)

  val duration_api_ms : t -> int
  (** [duration_api_ms t] returns the API duration in milliseconds. *)

  val is_error : t -> bool
  (** [is_error t] returns whether this result represents an error. *)

  val num_turns : t -> int
  (** [num_turns t] returns the number of conversation turns. *)

  val session_id : t -> string
  (** [session_id t] returns the session identifier. *)

  val total_cost_usd : t -> float option
  (** [total_cost_usd t] returns the optional total cost in USD. *)

  val usage : t -> Usage.t option
  (** [usage t] returns the optional usage statistics. *)

  val result : t -> string option
  (** [result t] returns the optional result string. *)

  val structured_output : t -> Jsont.json option
  (** [structured_output t] returns the optional structured JSON output. *)

  val unknown : t -> Unknown.t
  (** [unknown t] returns the unknown fields preserved from JSON. *)

  val to_json : t -> Jsont.json
  (** [to_json t] converts the result message to its JSON representation. *)

  val of_json : Jsont.json -> t
  (** [of_json json] parses a result message from JSON.
      @raise Invalid_argument if the JSON is not a valid result message. *)
end

(** {1 Message Union Type} *)

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of System.t
  | Result of Result.t
(** The type of messages, which can be user, assistant, system, or result. *)

val jsont : t Jsont.t
(** [jsont] is the Jsont codec for messages. *)

val user_string : string -> t
(** [user_string s] creates a user message with text content. *)

val user_blocks : Content_block.t list -> t
(** [user_blocks blocks] creates a user message with content blocks. *)

val user_with_tool_result : tool_use_id:string -> content:string -> ?is_error:bool -> unit -> t
(** [user_with_tool_result ~tool_use_id ~content ?is_error ()] creates a user message
    containing a tool result. *)

val assistant : content:Content_block.t list -> model:string -> ?error:Assistant.error -> unit -> t
(** [assistant ~content ~model ?error ()] creates an assistant message. *)

val assistant_text : text:string -> model:string -> ?error:Assistant.error -> unit -> t
(** [assistant_text ~text ~model ?error ()] creates an assistant message with only text content. *)

val system_init : session_id:string -> t
(** [system_init ~session_id] creates a system init message. *)

val system_error : error:string -> t
(** [system_error ~error] creates a system error message. *)

val result :
  subtype:string ->
  duration_ms:int ->
  duration_api_ms:int ->
  is_error:bool ->
  num_turns:int ->
  session_id:string ->
  ?total_cost_usd:float ->
  ?usage:Result.Usage.t ->
  ?result:string ->
  ?structured_output:Jsont.json ->
  unit -> t
(** [result ~subtype ~duration_ms ~duration_api_ms ~is_error ~num_turns
    ~session_id ?total_cost_usd ?usage ?result ()] creates a result message. *)

val to_json : t -> Jsont.json
(** [to_json t] converts any message to its JSON representation. *)

val of_json : Jsont.json -> t
(** [of_json json] parses a message from JSON.
    @raise Invalid_argument if the JSON is not a valid message. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints any message. *)

(** {1 Message Analysis} *)

val is_user : t -> bool
(** [is_user t] returns true if the message is from a user. *)

val is_assistant : t -> bool
(** [is_assistant t] returns true if the message is from the assistant. *)

val is_system : t -> bool
(** [is_system t] returns true if the message is a system message. *)

val is_result : t -> bool
(** [is_result t] returns true if the message is a result message. *)

val is_error : t -> bool
(** [is_error t] returns true if the message represents an error. *)

val extract_text : t -> string option
(** [extract_text t] attempts to extract text content from any message type. *)

val extract_tool_uses : t -> Content_block.Tool_use.t list
(** [extract_tool_uses t] extracts tool use blocks from assistant messages. *)

val get_session_id : t -> string option
(** [get_session_id t] extracts the session ID from system or result messages. *)

(** {1 Logging} *)

val log_received : t -> unit
(** [log_received t] logs that a message was received. *)

val log_sending : t -> unit
(** [log_sending t] logs that a message is being sent. *)

val log_error : string -> t -> unit
(** [log_error msg t] logs an error with the given message and context. *)

