(** Dynamic system prompt generation for Claude agent.

    Builds a comprehensive system prompt by:
    1. Running 'unpac --help' to get current CLI documentation
    2. Reading ARCHITECTURE.md if present
    3. Querying current workspace state *)

val generate :
  proc_mgr:Unpac.Git.proc_mgr ->
  root:Unpac.Worktree.root ->
  autonomous:bool ->
  string
(** Generate a system prompt with full unpac knowledge.
    If [autonomous] is true, includes detailed instructions for autonomous
    code maintenance and improvement. *)

val generate_for_project :
  proc_mgr:Unpac.Git.proc_mgr ->
  root:Unpac.Worktree.root ->
  project:string ->
  string
(** Generate a system prompt for a specific project agent.
    The agent will focus exclusively on the given project. *)

val autonomous_base_prompt : string
(** Base system prompt for autonomous mode. *)

val interactive_base_prompt : string
(** Base system prompt for interactive mode. *)
