(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Error = Tomlt.Error

type Eio.Exn.err += E of Error.t

let err e = Eio.Exn.create (E e)

let () =
  Eio.Exn.register_pp (fun f -> function
    | E e ->
      Format.fprintf f "Toml %a" Error.pp e;
      true
    | _ -> false
  )

let wrap_error f =
  try f ()
  with Error.Error e ->
    raise (err e)

let parse ?file input =
  try Tomlt.parse input
  with Error.Error e ->
    let bt = Printexc.get_raw_backtrace () in
    let eio_exn = err e in
    let eio_exn = match file with
      | Some f -> Eio.Exn.add_context eio_exn "parsing %s" f
      | None -> eio_exn
    in
    Printexc.raise_with_backtrace eio_exn bt

let of_flow ?file flow =
  let input = Eio.Flow.read_all flow in
  parse ?file input

let of_path ~fs path =
  let file = Eio.Path.(/) fs path |> Eio.Path.native_exn in
  Eio.Path.load (Eio.Path.(/) fs path)
  |> parse ~file

let to_flow flow value =
  let output = Tomlt.to_toml_string value in
  Eio.Flow.copy_string output flow
