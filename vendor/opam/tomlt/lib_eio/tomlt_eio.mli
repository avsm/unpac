(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio integration for TOML.

    This module provides Eio-native functions for parsing and encoding TOML,
    with proper integration into Eio's exception system.

    {2 Example}
    {[
      let config = Eio.Path.with_open_in path (fun flow ->
        Tomlt_eio.of_flow ~file:(Eio.Path.native_exn path) flow
      )
    ]}
*)

(** {1 Eio Exception Integration} *)

type Eio.Exn.err += E of Tomlt.Toml.Error.t
(** TOML errors as Eio errors. *)

val err : Tomlt.Toml.Error.t -> exn
(** [err e] creates an [Eio.Io] exception from TOML error [e]. *)

val wrap_error : (unit -> 'a) -> 'a
(** [wrap_error f] runs [f] and converts [Tomlt.Toml.Error.Error] to [Eio.Io]. *)

(** {1 Parsing with Eio} *)

val parse : ?file:string -> string -> Tomlt.Toml.t
(** [parse s] parses TOML string [s] with Eio error handling.
    @param file optional filename for error context.
    @raise Eio.Io on parse errors. *)

val of_flow : ?file:string -> _ Eio.Flow.source -> Tomlt.Toml.t
(** [of_flow flow] reads and parses TOML from an Eio flow.
    @param file optional filename for error context.
    @raise Eio.Io on read or parse errors. *)

val of_path : fs:_ Eio.Path.t -> string -> Tomlt.Toml.t
(** [of_path ~fs path] reads and parses TOML from a file path.
    @raise Eio.Io on file or parse errors. *)

(** {1 Encoding with Eio} *)

val to_flow : _ Eio.Flow.sink -> Tomlt.Toml.t -> unit
(** [to_flow flow t] writes TOML value [t] to an Eio flow.
    @raise Invalid_argument if [t] is not a table. *)
