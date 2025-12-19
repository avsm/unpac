(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio integration for TOML errors.

    This module registers TOML errors with Eio's exception system,
    allowing them to be used with {!Eio.Io} and providing context tracking.

    {2 Example}
    {[
      let config = Eio.Path.with_open_in path (fun flow ->
        Tomlt_eio.of_flow ~file:(Eio.Path.native_exn path) flow
      )
    ]}
*)

(** {1 Eio Exception Integration} *)

(** TOML errors as Eio errors *)
type Eio.Exn.err += E of Tomlt.Error.t

(** Create an [Eio.Io] exception from a TOML error *)
val err : Tomlt.Error.t -> exn

(** Wrap a function, converting [Tomlt_error.Error] to [Eio.Io] *)
val wrap_error : (unit -> 'a) -> 'a

(** {1 Parsing with Eio} *)

(** Parse TOML string with Eio error handling.
    @param file optional filename for error context *)
val parse_toml : ?file:string -> string -> Tomlt.toml_value

(** Read and parse TOML from an Eio flow.
    @param file optional filename for error context *)
val of_flow : ?file:string -> _ Eio.Flow.source -> Tomlt.toml_value

(** Read and parse TOML from an Eio path *)
val of_path : fs:_ Eio.Path.t -> string -> Tomlt.toml_value

(** {1 Encoding with Eio} *)

(** Write TOML to an Eio flow *)
val to_flow : _ Eio.Flow.sink -> Tomlt.toml_value -> unit
