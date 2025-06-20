(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

rule analyze = parse
(* RNTM section or shebang directly to the runtime *)
  | "#!" ([^ ' ' '\n']+ as runtime) '\n'
  | ([^ '\000']+ as runtime) '\000' eof
      { Some runtime }

(* Shell script launcher (if it matches, this always matches more than the above
   regexp) *)
  | "#!" [^ ' ' '\n']+ "/sh\nexec '"
      { analyze_sh_launcher (Buffer.create 1024) lexbuf }

  | _ | eof
      { None }

and analyze_sh_launcher b = parse
(* An embedded single quote in the filename passed to exec *)
  | "'\\''"
      { analyze_sh_launcher (Buffer.add_char b '\''; b) lexbuf }

  | [^ '\'']+ as s
      { analyze_sh_launcher (Buffer.add_string b s; b) lexbuf }

(* End of the filename parsed; return the entire string *)
  | "' \"$0\" \"$@\"\n"
      { Some (Buffer.contents b) }

  | _ | eof
      { None }

{
let read_runtime t ic =
  seek_in ic 0;
  let lexbuf =
    try
      if really_input_string ic 2 = "#!" then
        let () = seek_in ic 0 in
        Some (Lexing.from_channel ic)
      else
        let rntm = Bytesections.(read_section_string t ic Name.RNTM) in
        Some (Lexing.from_string rntm)
    with End_of_file | Not_found -> None
  in
  Option.bind lexbuf analyze
}
