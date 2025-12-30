(** Structured audit logging for unpac operations.

    This module provides hierarchical logging with JSON serialization,
    enabling both human-readable and machine-processable audit trails.

    All unpac operations are logged with their constituent git operations,
    timestamps, durations, and outcomes. *)

(** {1 Git Operation Logging} *)

(** Result of a git command *)
type git_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

(** A single git command execution *)
type git_operation = {
  git_id : string;           (** Unique ID for this operation *)
  git_timestamp : float;     (** Unix timestamp when started *)
  git_cmd : string list;     (** Git command args (without 'git') *)
  git_cwd : string;          (** Working directory *)
  git_duration_ms : int;     (** Duration in milliseconds *)
  git_result : git_result;   (** Command result *)
}

val git_operation_jsont : git_operation Jsont.t
(** JSON codec for git operations *)

(** {1 Unpac Operation Logging} *)

(** Status of an unpac operation *)
type status =
  | Success
  | Failed of string
  | Conflict of string list

val status_jsont : status Jsont.t

(** Type of unpac operation *)
type operation_type =
  | Init
  | Project_new
  | Project_promote
  | Project_set_remote
  | Opam_add
  | Opam_init
  | Opam_promote
  | Opam_update
  | Opam_merge
  | Opam_edit
  | Opam_done
  | Opam_remove
  | Git_add
  | Git_update
  | Git_merge
  | Git_remove
  | Push
  | Unknown of string

val operation_type_jsont : operation_type Jsont.t

val operation_type_to_string : operation_type -> string
(** Convert operation type to string representation *)

(** An unpac operation with its git operations *)
type operation = {
  id : string;               (** Unique operation ID (UUID) *)
  timestamp : float;         (** Unix timestamp when started *)
  operation_type : operation_type;
  args : string list;        (** Command arguments *)
  cwd : string;              (** Working directory *)
  duration_ms : int;         (** Total duration in milliseconds *)
  status : status;           (** Final status *)
  git_operations : git_operation list;  (** Constituent git operations *)
}

val operation_jsont : operation Jsont.t
(** JSON codec for unpac operations *)

(** {1 Audit Log} *)

(** Complete audit log *)
type log = {
  version : string;          (** Log format version *)
  entries : operation list;  (** Log entries, newest first *)
}

val log_jsont : log Jsont.t
(** JSON codec for the complete audit log *)

(** Current log format version *)
val current_version : string

(** {1 Logging API} *)

(** Active operation context for accumulating git operations *)
type context

(** Start a new unpac operation.
    Returns a context for recording git operations. *)
val start_operation :
  operation_type:operation_type ->
  args:string list ->
  cwd:string ->
  context

(** Record a git operation within the current context.
    Call this after each git command completes. *)
val record_git :
  context ->
  cmd:string list ->
  cwd:string ->
  started:float ->
  result:git_result ->
  unit

(** Complete an operation successfully *)
val complete_success : context -> operation

(** Complete an operation with failure *)
val complete_failed : context -> error:string -> operation

(** Complete an operation with conflict *)
val complete_conflict : context -> files:string list -> operation

(** {1 Log File Management} *)

(** Default log file path relative to project root *)
val default_log_file : string

(** Load audit log from file. Returns empty log if file doesn't exist. *)
val load : string -> (log, string) result

(** Save audit log to file *)
val save : string -> log -> (unit, string) result

(** Append an operation to the log file *)
val append : string -> operation -> (unit, string) result

(** {1 Formatting} *)

(** Pretty-print an operation for terminal output *)
val pp_operation : Format.formatter -> operation -> unit

(** Pretty-print the log for terminal output *)
val pp_log : Format.formatter -> log -> unit

(** Generate HTML report from log *)
val to_html : log -> string

(** {1 Audit Manager} *)

(** Manager that handles full operation lifecycle with auto-commit *)
type manager

(** Create an audit manager for the given workspace *)
val create_manager :
  proc_mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  main_wt:Eio.Fs.dir_ty Eio.Path.t ->
  manager

(** Begin a new audited operation. Returns the context for recording git ops. *)
val begin_operation :
  manager ->
  operation_type:operation_type ->
  args:string list ->
  context

(** End an operation successfully. Appends to log and commits. *)
val end_success : manager -> (operation, string) result

(** End an operation with failure. Appends to log and commits. *)
val end_failed : manager -> error:string -> (operation, string) result

(** End an operation with merge conflict. Appends to log and commits. *)
val end_conflict : manager -> files:string list -> (operation, string) result

(** Get the current context if one is active *)
val get_context : manager -> context option

(** Commit the audit log to git *)
val commit_log :
  proc_mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  main_wt:Eio.Fs.dir_ty Eio.Path.t ->
  log_path:string ->
  (unit, string) result
