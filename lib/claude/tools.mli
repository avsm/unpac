(** Tool definitions for Claude to interact with unpac.

    Uses the Claude SDK's MCP-based custom tool architecture. All tools are
    bundled into an in-process MCP server that gets registered with the
    Claude client, making them available as mcp__unpac__<tool_name>.

    {1 Available Tools}

    Workspace status:
    - [unpac_status] - Overview of workspace (projects, git repos, opam packages)
    - [unpac_status_sync] - Run 'unpac status' to update README.md
    - [unpac_push] - Push all branches to remote

    Git vendoring:
    - [unpac_git_list] - List vendored git repositories
    - [unpac_git_add] - Vendor a new git repository
    - [unpac_git_info] - Show details about a vendored repository
    - [unpac_git_diff] - Show local changes in a vendored repository

    Opam:
    - [unpac_opam_list] - List vendored opam packages
    - [unpac_project_list] - List projects

    File operations:
    - [read_file] - Read file contents
    - [write_file] - Write content to a file
    - [list_directory] - List directory contents
    - [glob_files] - Find files matching a pattern

    Shell:
    - [run_shell] - Execute a shell command
    - [git_commit] - Stage and commit changes *)

val create_mcp_server :
  proc_mgr:Unpac.Git.proc_mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  root:Unpac.Worktree.root ->
  Claude.Mcp_server.t
(** Create an MCP server with all unpac tools.

    The server is named "unpac" so tools are accessible as [mcp__unpac__<tool_name>].
    Register it with [Claude.Options.with_mcp_server ~name:"unpac" server]. *)
