(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TOML 1.1 codec.

    This module provides TOML 1.1 parsing and encoding with Bytesrw streaming
    support.

    {b Example:}
    {[
      let contents = Bytesrw.Bytes.Reader.of_string toml_input in
      match Tomlt.decode contents with
      | Ok toml -> (* use toml *)
      | Error msg -> (* handle error *)
    ]} *)

open Bytesrw

(** {1:types TOML Value Types} *)

type toml_value =
  | Toml_string of string
  | Toml_int of int64
  | Toml_float of float
  | Toml_bool of bool
  | Toml_datetime of string  (** Offset datetime (RFC 3339 with timezone) *)
  | Toml_datetime_local of string  (** Local datetime (no timezone) *)
  | Toml_date_local of string  (** Local date only *)
  | Toml_time_local of string  (** Local time only *)
  | Toml_array of toml_value list
  | Toml_table of (string * toml_value) list
(** The type for TOML values. *)

(** {1:decode Decode} *)

val decode : ?file:string -> Bytes.Reader.t -> (toml_value, string) result
(** [decode r] decodes a TOML document from reader [r].
    - [file] is the file path for error messages. Defaults to ["-"]. *)

val decode_string : string -> (toml_value, string) result
(** [decode_string s] decodes a TOML document from string [s]. *)

val decode_to_tagged_json : ?file:string -> Bytes.Reader.t -> (string, string) result
(** [decode_to_tagged_json r] decodes TOML and outputs tagged JSON
    in the format used by toml-test. *)

(** {1:encode Encode} *)

val encode_toml : toml_value -> string
(** [encode_toml v] encodes TOML value [v] to a TOML string. *)

val encode_toml_to_buffer : Buffer.t -> toml_value -> unit
(** [encode_toml_to_buffer buf v] encodes TOML value [v] directly to buffer [buf].
    This avoids allocating an intermediate string. *)

val encode_to_writer : Bytes.Writer.t -> toml_value -> unit
(** [encode_to_writer w v] encodes TOML value [v] directly to writer [w].
    Useful for streaming output to files or network without building the
    full string in memory first. *)

val encode_from_tagged_json : string -> (string, string) result
(** [encode_from_tagged_json json] converts tagged JSON to TOML. *)

(** {1:helpers Helpers} *)

val toml_to_tagged_json : toml_value -> string
(** [toml_to_tagged_json v] converts a TOML value to tagged JSON format
    used by toml-test. *)

val decode_tagged_json_string : string -> toml_value
(** [decode_tagged_json_string s] parses tagged JSON into TOML values. *)

val parse_toml : string -> toml_value
(** [parse_toml s] parses a TOML string. Raises [Error.Error] on failure. *)

(** {1:errors Error Handling} *)

module Error = Tomlt_error
(** Error types for TOML parsing and encoding. *)
