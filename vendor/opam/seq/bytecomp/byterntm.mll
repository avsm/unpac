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

{
type search_method =
| Disable of string
| Fallback of string
| Enable

(* First word of the current line being analysed - [exec ...], [r=...], or
   [c=...] *)
type state = Exec | R | C of string
}

rule analyze = parse
(* RNTM section for -runtime-search absolute or shebang directly to the
   runtime *)
  | "#!" ([^ ' ' '\n']* as dir) ('/' as sep) ([^ '/' ' ' '\n']+ as runtime) '\n'
  | ([^ '\000']* as dir) (['/' '\\' '\000'] as sep) (* Directory portion *)
    ([^ '\\' '/' '\000']+ as runtime) eof           (* Runtime portion *)
      { if sep = '\000' then
          if dir = "" then
            Some (runtime, Enable)
          else
            Some (runtime, Fallback (Filename.concat dir ""))
        else
          Some (runtime, Disable (dir ^ String.make 1 sep)) }

(* Legacy RNTM (remove after bootstrap) *)
  | (([^ '\000']* ['/' '\\']) as dir)
    ([^ '\\' '/' '\000']+ as runtime) '\000' eof
      { if dir = "" then
          Some (runtime, Enable)
        else
          Some (runtime, Disable dir) }

(* Shell script launcher (if it matches, this always matches more than the above
   regexp) *)
  | "#!" [^ ' ' '\n']+ "/sh\n" (("exec '" | "r='") as next)
      { let state = if next.[0] = 'r' then R else Exec in
        analyze_sh_launcher state (Buffer.create 1024) lexbuf }

  | _ | eof
      { None }

and analyze_sh_launcher state b = parse
(* An embedded single quote *)
  | "'\\''"
      { analyze_sh_launcher state (Buffer.add_char b '\''; b) lexbuf }

  | [^ '\'' ]+ as s
      { analyze_sh_launcher state (Buffer.add_string b s; b) lexbuf }

(* exec line for -runtime-search disable *)
  | "' \"$0\" \"$@\"\n"
      { if state = Exec then
          let name = Buffer.contents b in
          let runtime = Filename.basename name in
          let dir =
            String.sub name 0 (String.length name - String.length runtime)
          in
          Some (runtime, Disable dir)
        else
          None }

(* r= line for -runtime-search {fallback,enable} *)
  | "'\n" ("c='" as c)?
      { if state = R then
          let runtime = Buffer.contents b in
          if c = None then
            Some (runtime, Enable)
          else
            analyze_sh_launcher (C runtime) (Buffer.clear b; b) lexbuf
        else
          None }

(* c= line for -runtime-search fallback *)
  | "'\"$r\"\n"
      { match state with
        | C runtime ->
            Some (runtime, Fallback (Buffer.contents b))
        | _ ->
            None }

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
