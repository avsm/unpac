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

(** Parser for RNTM in bytecode executables. Parses both the RNTM section and
    the shebang launcher produced by {!Bytelink}. *)

val read_runtime : Bytesections.section_table -> in_channel -> string option
(** Returns the runtime used by this tendered/standalone image. If the runtime
    used cannot be parsed, or the image was linked using -without-runtime, then
    [None] is returned. *)
