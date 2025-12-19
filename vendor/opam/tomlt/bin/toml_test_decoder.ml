(* TOML test decoder - reads TOML from stdin, outputs tagged JSON to stdout *)

let () =
  let input = In_channel.input_all In_channel.stdin in
  match Tomlt.decode_string input with
  | Ok toml ->
      let json = Tomlt.toml_to_tagged_json toml in
      print_string json;
      print_newline ()
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
