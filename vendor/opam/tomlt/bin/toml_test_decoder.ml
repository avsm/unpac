(* TOML test decoder - reads TOML from stdin, outputs tagged JSON to stdout *)

let () =
  let input = In_channel.input_all In_channel.stdin in
  match Tomlt.of_string input with
  | Ok toml ->
      let json = Tomlt.Internal.to_tagged_json toml in
      print_string json;
      print_newline ()
  | Error e ->
      Printf.eprintf "Error: %s\n" (Tomlt.Error.to_string e);
      exit 1
