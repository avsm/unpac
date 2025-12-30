(** Unpac Claude agent - ralph-loop style autonomous coding for workspace projects. *)

open Cmdliner

let setup_logging verbose =
  Fmt_tty.setup_std_outputs ();
  (* Normal mode: Warning level (suppress Claude lib's JSON INFO logs)
     Verbose mode: Debug level (show everything) *)
  let level = if verbose then Logs.Debug else Logs.Warning in
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ())

let run_agent verbose web_port project workspace_path =
  setup_logging verbose;
  Eio_main.run @@ fun env ->
  let config : Unpac_claude.Agent.config = {
    verbose;
    web_port;
    max_iterations = 20;
    project;
  } in
  Unpac_claude.Agent.run ~env ~config ~workspace_path ()

(* CLI *)
let verbose_arg =
  let doc = "Enable verbose logging." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let web_port_arg =
  let doc = "Enable web UI on this port. Shows live streaming events." in
  Arg.(value & opt (some int) None & info ["web"] ~docv:"PORT" ~doc)

let project_arg =
  let doc = "Specific project to work on. If not specified, runs all \
             projects sequentially in random order." in
  Arg.(value & opt (some string) None & info ["p"; "project"] ~docv:"NAME" ~doc)

let workspace_arg =
  let doc = "Path to the unpac workspace. Required." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKSPACE" ~doc)

let cmd =
  let doc = "Ralph-loop style Claude agent for unpac workspace projects" in
  let man = [
    `S Manpage.s_description;
    `P "Runs an autonomous Claude agent using the ralph-loop pattern: \
        the same prompt is fed each iteration, with state persisting in \
        files. The agent works on projects until either:";
    `I ("Iterations", "20 iterations have completed");
    `I ("Completion", "The agent outputs the completion promise");
    `S "RALPH-LOOP PATTERN";
    `P "Unlike traditional agentic loops that vary prompts based on \
        previous responses, ralph-loop feeds the SAME prompt every \
        iteration. Claude's progress persists in files (STATUS.md, \
        source code, git commits) which it reads on each iteration.";
    `P "This creates a self-referential improvement loop where Claude \
        sees its own previous work and continues from there.";
    `S "COMPLETION PROMISE";
    `P (Printf.sprintf "When all significant work is complete, Claude \
        outputs exactly: %s" Unpac_claude.Agent.completion_promise);
    `P "This signals the loop to stop early before 20 iterations.";
    `S "MODEL";
    `P "Always uses Claude Opus 4.5 for maximum capability.";
    `S "WEB UI";
    `P "Use --web PORT to enable a live web dashboard showing:";
    `I ("Events", "Real-time streaming from the agent");
    `I ("Tool calls", "Each tool invocation with input/output");
    `I ("Iterations", "Current iteration progress");
    `S Manpage.s_examples;
    `P "Run agent on all projects (random order):";
    `Pre "  unpac-claude /path/to/workspace";
    `P "Run agent on a specific project:";
    `Pre "  unpac-claude -p mylib /path/to/workspace";
    `P "With web UI on port 8080:";
    `Pre "  unpac-claude --web 8080 /path/to/workspace";
    `P "Verbose logging:";
    `Pre "  unpac-claude -v /path/to/workspace";
    `S "WORKING DIRECTORY";
    `P "State is maintained in <workspace>/.unpac-claude/<project>/ \
        with a .claude subdirectory for ralph-loop state.";
    `S "EXIT STATUS";
    `P "Exits with 0 when all projects complete (either by iteration \
        limit or completion promise). Can be interrupted with Ctrl+C.";
  ] in
  let info = Cmd.info "unpac-claude" ~version:"0.5.0" ~doc ~man in
  Cmd.v info Term.(const run_agent $ verbose_arg $ web_port_arg $
                   project_arg $ workspace_arg)

let () = exit (Cmd.eval cmd)
