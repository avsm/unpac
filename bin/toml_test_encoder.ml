(* TOML test encoder - reads tagged JSON from stdin, outputs TOML to stdout *)

let () =
  let input = In_channel.input_all In_channel.stdin in
  match Tomlt.Internal.encode_from_tagged_json input with
  | Ok toml ->
      print_string toml
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
