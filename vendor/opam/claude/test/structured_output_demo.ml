(* Example demonstrating structured output with JSON Schema *)

module C = Claude

let () =
  (* Configure logging to see what's happening *)
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Logs.Src.set_level C.Message.src (Some Logs.Debug)

let run_codebase_analysis env =
  Printf.printf "\n=== Codebase Analysis with Structured Output ===\n\n";

  (* Define the JSON Schema for our expected output structure *)
  let analysis_schema =
    let open Jsont in
    Object
      ( [
          (("type", Meta.none), String ("object", Meta.none));
          ( ("properties", Meta.none),
            Object
              ( [
                  ( ("file_count", Meta.none),
                    Object
                      ( [
                          (("type", Meta.none), String ("integer", Meta.none));
                          ( ("description", Meta.none),
                            String ("Total number of files analyzed", Meta.none)
                          );
                        ],
                        Meta.none ) );
                  ( ("has_tests", Meta.none),
                    Object
                      ( [
                          (("type", Meta.none), String ("boolean", Meta.none));
                          ( ("description", Meta.none),
                            String
                              ("Whether the codebase has test files", Meta.none)
                          );
                        ],
                        Meta.none ) );
                  ( ("primary_language", Meta.none),
                    Object
                      ( [
                          (("type", Meta.none), String ("string", Meta.none));
                          ( ("description", Meta.none),
                            String
                              ( "The primary programming language used",
                                Meta.none ) );
                        ],
                        Meta.none ) );
                  ( ("complexity_rating", Meta.none),
                    Object
                      ( [
                          (("type", Meta.none), String ("string", Meta.none));
                          ( ("enum", Meta.none),
                            Array
                              ( [
                                  String ("low", Meta.none);
                                  String ("medium", Meta.none);
                                  String ("high", Meta.none);
                                ],
                                Meta.none ) );
                          ( ("description", Meta.none),
                            String ("Overall complexity rating", Meta.none) );
                        ],
                        Meta.none ) );
                  ( ("key_findings", Meta.none),
                    Object
                      ( [
                          (("type", Meta.none), String ("array", Meta.none));
                          ( ("items", Meta.none),
                            Object
                              ( [
                                  ( ("type", Meta.none),
                                    String ("string", Meta.none) );
                                ],
                                Meta.none ) );
                          ( ("description", Meta.none),
                            String
                              ( "List of key findings from the analysis",
                                Meta.none ) );
                        ],
                        Meta.none ) );
                ],
                Meta.none ) );
          ( ("required", Meta.none),
            Array
              ( [
                  String ("file_count", Meta.none);
                  String ("has_tests", Meta.none);
                  String ("primary_language", Meta.none);
                  String ("complexity_rating", Meta.none);
                  String ("key_findings", Meta.none);
                ],
                Meta.none ) );
          (("additionalProperties", Meta.none), Bool (false, Meta.none));
        ],
        Meta.none )
  in

  (* Create structured output format from the schema *)
  let output_format = C.Structured_output.of_json_schema analysis_schema in

  (* Configure Claude with structured output *)
  let options =
    C.Options.default
    |> C.Options.with_output_format output_format
    |> C.Options.with_allowed_tools [ "Read"; "Glob"; "Grep" ]
    |> C.Options.with_system_prompt
         "You are a code analysis assistant. Analyze codebases and provide \
          structured output matching the given JSON Schema."
  in

  Printf.printf "Structured output format configured\n";
  Printf.printf "Schema: %s\n\n"
    (Test_json_utils.to_string ~minify:false analysis_schema);

  (* Create Claude client and query *)
  Eio.Switch.run @@ fun sw ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let client = C.Client.create ~sw ~process_mgr ~options () in

  let prompt =
    "Please analyze the current codebase structure. Look at the files, \
     identify the primary language, count files, check for tests, assess \
     complexity, and provide key findings. Return your analysis in the \
     structured JSON format I specified."
  in

  Printf.printf "Sending query: %s\n\n" prompt;
  C.Client.query client prompt;

  (* Process responses *)
  let messages = C.Client.receive client in
  Seq.iter
    (function
      | C.Message.Assistant msg ->
          Printf.printf "\nAssistant response:\n";
          List.iter
            (function
              | C.Content_block.Text text ->
                  Printf.printf "  Text: %s\n" (C.Content_block.Text.text text)
              | C.Content_block.Tool_use tool ->
                  Printf.printf "  Using tool: %s\n"
                    (C.Content_block.Tool_use.name tool)
              | _ -> ())
            (C.Message.Assistant.content msg)
      | C.Message.Result result -> (
          Printf.printf "\n=== Result ===\n";
          Printf.printf "Duration: %dms\n" (C.Message.Result.duration_ms result);
          Printf.printf "Cost: $%.4f\n"
            (Option.value (C.Message.Result.total_cost_usd result) ~default:0.0);

          (* Extract and display structured output *)
          match C.Message.Result.structured_output result with
          | Some output ->
              Printf.printf "\n=== Structured Output ===\n";
              Printf.printf "%s\n\n"
                (Test_json_utils.to_string ~minify:false output);

              (* Parse the structured output *)
              let file_count =
                Test_json_utils.get_int output "file_count"
                |> Option.value ~default:0
              in
              let has_tests =
                Test_json_utils.get_bool output "has_tests"
                |> Option.value ~default:false
              in
              let language =
                Test_json_utils.get_string output "primary_language"
                |> Option.value ~default:"unknown"
              in
              let complexity =
                Test_json_utils.get_string output "complexity_rating"
                |> Option.value ~default:"unknown"
              in
              let findings =
                match Test_json_utils.get_array output "key_findings" with
                | Some items ->
                    List.filter_map
                      (fun json -> Test_json_utils.as_string json)
                      items
                | None -> []
              in

              Printf.printf "=== Parsed Analysis ===\n";
              Printf.printf "File Count: %d\n" file_count;
              Printf.printf "Has Tests: %b\n" has_tests;
              Printf.printf "Primary Language: %s\n" language;
              Printf.printf "Complexity: %s\n" complexity;
              Printf.printf "Key Findings:\n";
              List.iter
                (fun finding -> Printf.printf "  - %s\n" finding)
                findings
          | None -> (
              Printf.printf "No structured output received\n";
              match C.Message.Result.result result with
              | Some text -> Printf.printf "Text result: %s\n" text
              | None -> ()))
      | C.Message.System (C.Message.System.Init _) ->
          Printf.printf "Session initialized\n"
      | C.Message.System (C.Message.System.Error _) -> ()
      | _ -> ())
    messages;

  Printf.printf "\nDone!\n"

let () =
  Eio_main.run @@ fun env ->
  try run_codebase_analysis env with
  | C.Transport.CLI_not_found msg ->
      Printf.eprintf "Error: Claude CLI not found\n%s\n" msg;
      Printf.eprintf "Make sure 'claude' is installed and in your PATH\n";
      exit 1
  | C.Transport.Connection_error msg ->
      Printf.eprintf "Connection error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1
