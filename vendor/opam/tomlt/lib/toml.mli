(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TOML 1.1 codec.

    Tomlt provides TOML 1.1 parsing and encoding with efficient streaming
    support via {{:https://erratique.ch/software/bytesrw}Bytesrw}.

    {2 Quick Start}

    Parse a TOML string:
    {[
      let config = Tomlt.of_string {|
        [server]
        host = "localhost"
        port = 8080
      |} in
      match config with
      | Ok t ->
          let host = Tomlt.(t.%{"server"; "host"} |> to_string) in
          let port = Tomlt.(t.%{"server"; "port"} |> to_int) in
          Printf.printf "Server: %s:%Ld\n" host port
      | Error e -> prerr_endline (Tomlt.Error.to_string e)
    ]}

    Create and encode TOML:
    {[
      let config = Tomlt.(table [
        "title", string "My App";
        "database", table [
          "host", string "localhost";
          "ports", array [int 5432L; int 5433L]
        ]
      ]) in
      print_endline (Tomlt.to_string config)
    ]}

    {2 Module Overview}

    - {!section:types} - TOML value representation
    - {!section:construct} - Value constructors
    - {!section:access} - Value accessors and type conversion
    - {!section:navigate} - Table navigation
    - {!section:decode} - Parsing from strings and readers
    - {!section:encode} - Encoding to strings and writers
    - {!module:Error} - Structured error types *)

open Bytesrw

(** {1:types TOML Value Types} *)

(** The type of TOML values.

    TOML supports the following value types:
    - Strings (UTF-8 encoded)
    - Integers (64-bit signed)
    - Floats (IEEE 754 double precision)
    - Booleans
    - Offset date-times (RFC 3339 with timezone)
    - Local date-times (no timezone)
    - Local dates
    - Local times
    - Arrays (heterogeneous in TOML 1.1)
    - Tables (string-keyed maps) *)
type t =
  | String of string
  | Int of int64
  | Float of float
  | Bool of bool
  | Datetime of string        (** Offset datetime, e.g. [1979-05-27T07:32:00Z] *)
  | Datetime_local of string  (** Local datetime, e.g. [1979-05-27T07:32:00] *)
  | Date_local of string      (** Local date, e.g. [1979-05-27] *)
  | Time_local of string      (** Local time, e.g. [07:32:00] *)
  | Array of t list
  | Table of (string * t) list
(** A TOML value. Tables preserve key insertion order. *)

(** {1:construct Value Constructors}

    These functions create TOML values. Use them to build TOML documents
    programmatically. *)

val string : string -> t
(** [string s] creates a string value. *)

val int : int64 -> t
(** [int i] creates an integer value. *)

val int_of_int : int -> t
(** [int_of_int i] creates an integer value from an [int]. *)

val float : float -> t
(** [float f] creates a float value. *)

val bool : bool -> t
(** [bool b] creates a boolean value. *)

val array : t list -> t
(** [array vs] creates an array value from a list of values.
    TOML 1.1 allows heterogeneous arrays. *)

val table : (string * t) list -> t
(** [table pairs] creates a table value from key-value pairs.
    Keys should be unique; later bindings shadow earlier ones during lookup. *)

val datetime : string -> t
(** [datetime s] creates an offset datetime value.
    The string should be in RFC 3339 format with timezone,
    e.g. ["1979-05-27T07:32:00Z"] or ["1979-05-27T07:32:00-07:00"]. *)

val datetime_local : string -> t
(** [datetime_local s] creates a local datetime value (no timezone).
    E.g. ["1979-05-27T07:32:00"]. *)

val date_local : string -> t
(** [date_local s] creates a local date value.
    E.g. ["1979-05-27"]. *)

val time_local : string -> t
(** [time_local s] creates a local time value.
    E.g. ["07:32:00"] or ["07:32:00.999"]. *)

(** {1:access Value Accessors}

    These functions extract OCaml values from TOML values.
    They raise [Invalid_argument] if the value is not of the expected type. *)

val to_string : t -> string
(** [to_string t] returns the string if [t] is a [String].
    @raise Invalid_argument if [t] is not a string. *)

val to_string_opt : t -> string option
(** [to_string_opt t] returns [Some s] if [t] is [String s], [None] otherwise. *)

val to_int : t -> int64
(** [to_int t] returns the integer if [t] is an [Int].
    @raise Invalid_argument if [t] is not an integer. *)

val to_int_opt : t -> int64 option
(** [to_int_opt t] returns [Some i] if [t] is [Int i], [None] otherwise. *)

val to_float : t -> float
(** [to_float t] returns the float if [t] is a [Float].
    @raise Invalid_argument if [t] is not a float. *)

val to_float_opt : t -> float option
(** [to_float_opt t] returns [Some f] if [t] is [Float f], [None] otherwise. *)

val to_bool : t -> bool
(** [to_bool t] returns the boolean if [t] is a [Bool].
    @raise Invalid_argument if [t] is not a boolean. *)

val to_bool_opt : t -> bool option
(** [to_bool_opt t] returns [Some b] if [t] is [Bool b], [None] otherwise. *)

val to_array : t -> t list
(** [to_array t] returns the list if [t] is an [Array].
    @raise Invalid_argument if [t] is not an array. *)

val to_array_opt : t -> t list option
(** [to_array_opt t] returns [Some vs] if [t] is [Array vs], [None] otherwise. *)

val to_table : t -> (string * t) list
(** [to_table t] returns the association list if [t] is a [Table].
    @raise Invalid_argument if [t] is not a table. *)

val to_table_opt : t -> (string * t) list option
(** [to_table_opt t] returns [Some pairs] if [t] is [Table pairs], [None] otherwise. *)

val to_datetime : t -> string
(** [to_datetime t] returns the datetime string for any datetime type.
    @raise Invalid_argument if [t] is not a datetime variant. *)

val to_datetime_opt : t -> string option
(** [to_datetime_opt t] returns [Some s] if [t] is any datetime variant. *)

(** {2 Type Predicates} *)

val is_string : t -> bool
(** [is_string t] is [true] iff [t] is a [String]. *)

val is_int : t -> bool
(** [is_int t] is [true] iff [t] is an [Int]. *)

val is_float : t -> bool
(** [is_float t] is [true] iff [t] is a [Float]. *)

val is_bool : t -> bool
(** [is_bool t] is [true] iff [t] is a [Bool]. *)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an [Array]. *)

val is_table : t -> bool
(** [is_table t] is [true] iff [t] is a [Table]. *)

val is_datetime : t -> bool
(** [is_datetime t] is [true] iff [t] is any datetime variant. *)

(** {1:navigate Table Navigation}

    Functions for navigating and querying TOML tables. *)

val find : string -> t -> t
(** [find key t] returns the value associated with [key] in table [t].
    @raise Invalid_argument if [t] is not a table.
    @raise Not_found if [key] is not in the table. *)

val find_opt : string -> t -> t option
(** [find_opt key t] returns [Some v] if [key] maps to [v] in table [t],
    or [None] if [key] is not bound or [t] is not a table. *)

val mem : string -> t -> bool
(** [mem key t] is [true] if [key] is bound in table [t], [false] otherwise.
    Returns [false] if [t] is not a table. *)

val keys : t -> string list
(** [keys t] returns all keys in table [t].
    @raise Invalid_argument if [t] is not a table. *)

val get : string list -> t -> t
(** [get path t] navigates through nested tables following [path].
    For example, [get ["server"; "port"] t] returns [t.server.port].
    @raise Invalid_argument if any intermediate value is not a table.
    @raise Not_found if any key in [path] is not found. *)

val get_opt : string list -> t -> t option
(** [get_opt path t] is like [get] but returns [None] on any error. *)

val ( .%{} ) : t -> string list -> t
(** [t.%{path}] is [get path t].

    Example: [config.%{["database"; "port"]}]

    @raise Invalid_argument if any intermediate value is not a table.
    @raise Not_found if any key in the path is not found. *)

val ( .%{}<- ) : t -> string list -> t -> t
(** [t.%{path} <- v] returns a new table with value [v] at [path].
    Creates intermediate tables as needed.

    Example: [config.%{["server"; "host"]} <- string "localhost"]

    @raise Invalid_argument if [t] is not a table or if an intermediate
    value exists but is not a table. *)

(** {1:decode Decoding (Parsing)}

    Parse TOML from various sources. *)

val of_string : string -> (t, Toml_error.t) result
(** [of_string s] parses [s] as a TOML document. *)

val of_reader : ?file:string -> Bytes.Reader.t -> (t, Toml_error.t) result
(** [of_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages. *)

val parse : string -> t
(** [parse s] parses [s] as a TOML document.
    @raise Error.Error on parse errors. *)

val parse_reader : ?file:string -> Bytes.Reader.t -> t
(** [parse_reader r] parses a TOML document from reader [r].
    @param file Optional filename for error messages.
    @raise Error.Error on parse errors. *)

(** {1:encode Encoding}

    Encode TOML values to various outputs. *)

val to_toml_string : t -> string
(** [to_toml_string t] encodes [t] as a TOML document string.
    @raise Invalid_argument if [t] is not a [Table]. *)

val to_buffer : Buffer.t -> t -> unit
(** [to_buffer buf t] writes [t] as TOML to buffer [buf].
    @raise Invalid_argument if [t] is not a [Table]. *)

val to_writer : Bytes.Writer.t -> t -> unit
(** [to_writer w t] writes [t] as TOML to writer [w].
    Useful for streaming output without building the full string in memory.
    @raise Invalid_argument if [t] is not a [Table]. *)

(** {1:pp Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] in TOML format. *)

val pp_value : Format.formatter -> t -> unit
(** [pp_value fmt t] pretty-prints a single TOML value (not a full document).
    Useful for debugging. Tables are printed as inline tables. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on TOML values.
    NaN floats are considered equal to each other. *)

val compare : t -> t -> int
(** [compare a b] is a total ordering on TOML values. *)

(** {1:errors Error Handling} *)

module Error = Toml_error
(** Structured error types for TOML parsing and encoding.

    See {!Toml_error} for detailed documentation. *)

(** {1:tagged_json Tagged JSON}

    Functions for interoperating with the
    {{:https://github.com/toml-lang/toml-test}toml-test} suite's tagged JSON
    format. These functions are primarily for testing and validation. *)

module Tagged_json : sig
  val encode : t -> string
  (** [encode t] converts TOML value [t] to tagged JSON format.

      The tagged JSON format wraps each value with type information:
      - Strings: [{"type": "string", "value": "..."}]
      - Integers: [{"type": "integer", "value": "..."}]
      - Floats: [{"type": "float", "value": "..."}]
      - Booleans: [{"type": "bool", "value": "true"|"false"}]
      - Datetimes: [{"type": "datetime", "value": "..."}]
      - Arrays: [[...]]
      - Tables: [{...}] *)

  val decode : string -> t
  (** [decode s] parses tagged JSON string [s] into a TOML value.
      @raise Failure if the JSON is malformed or has invalid types. *)

  val decode_and_encode_toml : string -> (string, string) result
  (** [decode_and_encode_toml json] decodes tagged JSON and encodes as TOML.
      Used by the toml-test encoder harness. *)
end
