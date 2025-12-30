(** Ralph-loop style Claude agent for unpac workspace analysis.

    Runs a single autonomous agent per project using ralph-loop iteration:
    - Same prompt fed each iteration (state persists in files)
    - Up to 20 iterations per project
    - Early exit on completion promise
    - Projects processed sequentially in random order *)

(** {1 Agent Configuration} *)

type config = {
  verbose : bool;
  web_port : int option;  (** Port for web UI, None = disabled *)
  max_iterations : int;   (** Max ralph-loop iterations per project *)
  project : string option; (** Specific project, or None for all *)
}

val default_config : config

(** {1 Completion Promise} *)

val completion_promise : string
(** The phrase that signals work is complete: "AGENTIC-HUMPS-COUNT-2" *)

(** {1 Running Agents} *)

val run :
  env:Eio_unix.Stdenv.base ->
  config:config ->
  workspace_path:string ->
  unit ->
  unit
(** [run ~env ~config ~workspace_path ()] runs ralph-loop agents for projects
    in the workspace.

    If [config.project] is specified, runs only that project.
    Otherwise, runs all projects sequentially in random order.

    Each project agent:
    - Uses Opus 4.5 model
    - Runs up to [max_iterations] iterations
    - Exits early if response contains [completion_promise]
    - Works from [workspace/.unpac-claude/project/] directory

    The function blocks until all projects complete. *)
