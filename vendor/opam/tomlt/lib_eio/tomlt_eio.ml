(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio integration for TOML errors.

    This module registers TOML errors with Eio's exception system,
    allowing them to be used with [Eio.Io] and providing context tracking. *)

module Error = Tomlt.Error

(** Extend Eio.Exn.err with TOML errors *)
type Eio.Exn.err += E of Error.t

(** Create an Eio.Io exception from a TOML error *)
let err e = Eio.Exn.create (E e)

(** Register pretty-printer with Eio *)
let () =
  Eio.Exn.register_pp (fun f -> function
    | E e ->
      Format.fprintf f "Toml %a" Error.pp e;
      true
    | _ -> false
  )

(** Convert a Error.Error exception to Eio.Io *)
let wrap_error f =
  try f ()
  with Error.Error e ->
    raise (err e)

(** Parse TOML with Eio error handling *)
let parse_toml ?file input =
  try Tomlt.parse_toml input
  with Error.Error e ->
    let bt = Printexc.get_raw_backtrace () in
    let eio_exn = err e in
    let eio_exn = match file with
      | Some f -> Eio.Exn.add_context eio_exn "parsing %s" f
      | None -> eio_exn
    in
    Printexc.raise_with_backtrace eio_exn bt

(** Read and parse TOML from an Eio flow *)
let of_flow ?file flow =
  let input = Eio.Flow.read_all flow in
  parse_toml ?file input

(** Read and parse TOML from an Eio path *)
let of_path ~fs path =
  let file = Eio.Path.(/) fs path |> Eio.Path.native_exn in
  Eio.Path.load (Eio.Path.(/) fs path)
  |> parse_toml ~file

(** Write TOML to an Eio flow *)
let to_flow flow value =
  let output = Tomlt.encode_toml value in
  Eio.Flow.copy_string output flow
