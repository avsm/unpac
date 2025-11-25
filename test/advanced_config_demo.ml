(* Advanced Configuration Demo

   This example demonstrates the advanced configuration options available
   in the OCaml Claude SDK, including:
   - Budget limits for cost control
   - Fallback models for reliability
   - Settings isolation for CI/CD environments
   - Custom buffer sizes for large outputs
*)

open Eio.Std
open Claude

let log_setup () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

(* Example 1: CI/CD Configuration

   In CI/CD environments, you want isolated, reproducible behavior
   without any user/project/local settings interfering.
*)
let ci_cd_config () =
  Options.default |> Options.with_no_settings (* Disable all settings loading *)
  |> Options.with_max_budget_usd 0.50 (* 50 cent limit per run *)
  |> Options.with_fallback_model_string "claude-haiku-4" (* Fast fallback *)
  |> Options.with_model_string "claude-sonnet-4-5"
  |> Options.with_permission_mode Permissions.Mode.Bypass_permissions

(* Example 2: Production Configuration with Fallback

   Production usage with cost controls and automatic fallback
   to ensure availability.
*)
let production_config () =
  Options.default
  |> Options.with_model_string "claude-sonnet-4-5"
  |> Options.with_fallback_model_string "claude-sonnet-3-5"
  |> Options.with_max_budget_usd 10.0 (* $10 limit *)
  |> Options.with_max_buffer_size 5_000_000 (* 5MB buffer for large outputs *)

(* Example 3: Development Configuration

   Development with user settings enabled but with cost controls.
*)
let dev_config () =
  Options.default
  |> Options.with_setting_sources [ Options.User; Options.Project ]
  |> Options.with_max_budget_usd 1.0 (* $1 limit for dev testing *)
  |> Options.with_fallback_model_string "claude-haiku-4"

(* Example 4: Isolated Test Configuration

   For automated testing with no external settings and strict limits.
*)
let test_config () =
  Options.default |> Options.with_no_settings
  |> Options.with_max_budget_usd 0.10 (* 10 cent limit per test *)
  |> Options.with_model_string "claude-haiku-4" (* Fast, cheap model *)
  |> Options.with_permission_mode Permissions.Mode.Bypass_permissions
  |> Options.with_max_buffer_size 1_000_000 (* 1MB buffer *)

(* Example 5: Custom Buffer Size Demo

   For applications that need to handle very large outputs.
*)
let _large_output_config () =
  Options.default
  |> Options.with_max_buffer_size 10_000_000 (* 10MB buffer *)
  |> Options.with_model_string "claude-sonnet-4-5"

(* Helper to run a query with a specific configuration *)
let run_query ~sw process_mgr config prompt =
  print_endline "\n=== Configuration ===";
  (match Options.max_budget_usd config with
  | Some budget -> Printf.printf "Budget limit: $%.2f\n" budget
  | None -> print_endline "Budget limit: None");
  (match Options.fallback_model config with
  | Some model ->
      Printf.printf "Fallback model: %s\n" (Claude.Model.to_string model)
  | None -> print_endline "Fallback model: None");
  (match Options.setting_sources config with
  | Some [] -> print_endline "Settings: Isolated (no settings loaded)"
  | Some sources ->
      let source_str =
        String.concat ", "
          (List.map
             (function
               | Options.User -> "user"
               | Options.Project -> "project"
               | Options.Local -> "local")
             sources)
      in
      Printf.printf "Settings: %s\n" source_str
  | None -> print_endline "Settings: Default");
  (match Options.max_buffer_size config with
  | Some size -> Printf.printf "Buffer size: %d bytes\n" size
  | None -> print_endline "Buffer size: Default (1MB)");

  print_endline "\n=== Running Query ===";
  let client = Client.create ~options:config ~sw ~process_mgr () in
  Client.query client prompt;
  let messages = Client.receive client in

  Seq.iter
    (function
      | Message.Assistant msg ->
          List.iter
            (function
              | Content_block.Text t ->
                  Printf.printf "Response: %s\n" (Content_block.Text.text t)
              | _ -> ())
            (Message.Assistant.content msg)
      | Message.Result result ->
          Printf.printf "\n=== Session Complete ===\n";
          Printf.printf "Duration: %dms\n" (Message.Result.duration_ms result);
          (match Message.Result.total_cost_usd result with
          | Some cost -> Printf.printf "Cost: $%.4f\n" cost
          | None -> ());
          Printf.printf "Turns: %d\n" (Message.Result.num_turns result)
      | _ -> ())
    messages

let main () =
  log_setup ();

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let process_mgr = Eio.Stdenv.process_mgr env in

  print_endline "==============================================";
  print_endline "Claude SDK - Advanced Configuration Examples";
  print_endline "==============================================";

  (* Example: CI/CD isolated environment *)
  print_endline "\n\n### Example 1: CI/CD Configuration ###";
  print_endline "Purpose: Isolated, reproducible environment for CI/CD";
  let config = ci_cd_config () in
  run_query ~sw process_mgr config "What is 2+2? Answer in one sentence.";

  (* Example: Production with fallback *)
  print_endline "\n\n### Example 2: Production Configuration ###";
  print_endline "Purpose: Production with cost controls and fallback";
  let config = production_config () in
  run_query ~sw process_mgr config "Explain OCaml in one sentence.";

  (* Example: Development with settings *)
  print_endline "\n\n### Example 3: Development Configuration ###";
  print_endline "Purpose: Development with user/project settings";
  let config = dev_config () in
  run_query ~sw process_mgr config
    "What is functional programming? One sentence.";

  (* Example: Test configuration *)
  print_endline "\n\n### Example 4: Test Configuration ###";
  print_endline "Purpose: Automated testing with strict limits";
  let config = test_config () in
  run_query ~sw process_mgr config "Say 'test passed' in one word.";

  print_endline "\n\n==============================================";
  print_endline "All examples completed successfully!";
  print_endline "=============================================="

let () =
  try main ()
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
