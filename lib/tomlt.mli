(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative TOML 1.1 codecs.

    Tomlt provides a bidirectional codec system for TOML files, inspired by
    {{:https://erratique.ch/software/jsont}Jsont}'s approach to JSON codecs.

    {2 Quick Start}

    Define a codec for your OCaml types:
    {v
    type config = { host : string; port : int; debug : bool }

    let config_codec =
      Tomlt.(Table.(
        obj (fun host port debug -> { host; port; debug })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> mem "debug" bool ~enc:(fun c -> c.debug) ~dec_absent:false
        |> finish
      ))

    let () =
      match Tomlt.decode_string config_codec {|
        host = "localhost"
        port = 8080
      |} with
      | Ok config -> Printf.printf "Host: %s\n" config.host
      | Error e -> prerr_endline (Tomlt.Toml.Error.to_string e)
    v}

    {2 Codec Pattern}

    Each codec ['a t] defines:
    - A decoder: [Toml.t -> ('a, error) result]
    - An encoder: ['a -> Toml.t]

    Codecs compose through combinators to build complex types from
    simple primitives.

    {2 Module Overview}

    - {!section:datetime} - Structured datetime types
    - {!section:codec} - Core codec type and combinators
    - {!section:base} - Primitive type codecs
    - {!section:combinators} - Codec transformers
    - {!section:arrays} - Array codec builders
    - {!section:tables} - Table/object codec builders
    - {!section:codec_ops} - Encoding and decoding operations *)

(** {1:datetime Structured Datetime Types}

    TOML 1.1 supports four datetime formats. These modules provide
    structured representations for parsing and formatting. *)

(** Timezone offsets for TOML offset datetimes.

    Per RFC 3339, timezones are expressed as [Z] (UTC) or as
    [+HH:MM] / [-HH:MM] offsets from UTC. *)
module Tz : sig
  (** Timezone offset representation. *)
  type t =
    | UTC                                    (** UTC timezone, written as [Z] *)
    | Offset of { hours : int; minutes : int } (** Fixed offset from UTC *)

  val utc : t
  (** [utc] is the UTC timezone. *)

  val offset : hours:int -> minutes:int -> t
  (** [offset ~hours ~minutes] creates a fixed UTC offset.
      Hours may be negative for western timezones. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total ordering. *)

  val to_string : t -> string
  (** [to_string tz] formats as ["Z"] or ["+HH:MM"]/["-HH:MM"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt tz] pretty-prints the timezone. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses ["Z"], ["+HH:MM"], or ["-HH:MM"]. *)
end

(** Local dates (no timezone information).

    Represents a calendar date like [1979-05-27]. *)
module Date : sig
  type t = { year : int; month : int; day : int }
  (** A calendar date with year (4 digits), month (1-12), and day (1-31). *)

  val make : year:int -> month:int -> day:int -> t
  (** [make ~year ~month ~day] creates a date value. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  (** [to_string d] formats as ["YYYY-MM-DD"]. *)

  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
  (** [of_string s] parses ["YYYY-MM-DD"] format. *)
end

(** Local times (no date or timezone).

    Represents a time of day like [07:32:00] or [07:32:00.999999]. *)
module Time : sig
  type t = {
    hour : int;     (** Hour (0-23) *)
    minute : int;   (** Minute (0-59) *)
    second : int;   (** Second (0-59, 60 for leap seconds) *)
    frac : float;   (** Fractional seconds [0.0, 1.0) *)
  }

  val make : hour:int -> minute:int -> second:int -> ?frac:float -> unit -> t
  (** [make ~hour ~minute ~second ?frac ()] creates a time value.
      [frac] defaults to [0.0]. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  (** [to_string t] formats as ["HH:MM:SS"] or ["HH:MM:SS.fff"]. *)

  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** Offset datetimes (date + time + timezone).

    The complete datetime format per RFC 3339, like
    [1979-05-27T07:32:00Z] or [1979-05-27T07:32:00-07:00]. *)
module Datetime : sig
  type t = { date : Date.t; time : Time.t; tz : Tz.t }

  val make : date:Date.t -> time:Time.t -> tz:Tz.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** Local datetimes (date + time, no timezone).

    Like [1979-05-27T07:32:00] - a datetime with no timezone
    information, representing "wall clock" time. *)
module Datetime_local : sig
  type t = { date : Date.t; time : Time.t }

  val make : date:Date.t -> time:Time.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** {1:codec Codec Types} *)

(** Errors that can occur during codec operations. *)
type codec_error =
  | Type_mismatch of { expected : string; got : string }
      (** TOML value was not the expected type *)
  | Missing_member of string
      (** Required table member was not present *)
  | Unknown_member of string
      (** Unknown member found (when using [error_unknown]) *)
  | Value_error of string
      (** Value failed validation or parsing *)
  | Int_overflow of int64
      (** Integer value exceeds OCaml [int] range *)
  | Parse_error of string
      (** Parsing failed *)

val codec_error_to_string : codec_error -> string
(** [codec_error_to_string e] returns a human-readable error message. *)

(** The type of TOML codecs.

    A value of type ['a t] can decode TOML values to type ['a]
    and encode values of type ['a] to TOML. *)
type 'a t

val kind : 'a t -> string
(** [kind c] returns the kind description of codec [c]. *)

val doc : 'a t -> string
(** [doc c] returns the documentation string of codec [c]. *)

val with_doc : ?kind:string -> ?doc:string -> 'a t -> 'a t
(** [with_doc ?kind ?doc c] returns a codec with updated metadata. *)

(** {1:base Base Type Codecs}

    Primitive codecs for TOML's basic value types. *)

val bool : bool t
(** Codec for TOML booleans. *)

val int : int t
(** Codec for TOML integers to OCaml [int].
    @raise Int_overflow if the value exceeds platform [int] range. *)

val int32 : int32 t
(** Codec for TOML integers to [int32]. *)

val int64 : int64 t
(** Codec for TOML integers to [int64]. *)

val float : float t
(** Codec for TOML floats. Handles [inf], [-inf], and [nan]. *)

val number : float t
(** Codec that accepts both TOML integers and floats as [float].
    Integers are converted to floats during decoding. *)

val string : string t
(** Codec for TOML strings (UTF-8 encoded). *)

val datetime : Datetime.t t
(** Codec for offset datetimes like [1979-05-27T07:32:00Z]. *)

val datetime_local : Datetime_local.t t
(** Codec for local datetimes like [1979-05-27T07:32:00]. *)

val date_local : Date.t t
(** Codec for local dates like [1979-05-27]. *)

val time_local : Time.t t
(** Codec for local times like [07:32:00]. *)

val datetime_string : string t
(** Codec for any datetime type as a raw string.
    Decodes any datetime variant; encodes as offset datetime. *)

(** {2 Ptime Codecs}

    Codecs that work with {{:https://erratique.ch/software/ptime}Ptime}
    timestamps directly. These provide convenient interop between TOML
    datetime values and ptime's precise timestamp representation. *)

val ptime : Ptime.t t
(** Codec for offset datetimes as [Ptime.t].
    Decodes TOML offset datetime (e.g., [1979-05-27T07:32:00Z]) to a ptime
    timestamp. Encodes as UTC (with [Z] suffix).

    Supports TOML 1.1 optional seconds (e.g., [1979-05-27T07:32Z]).

    @raise Value_error if the datetime cannot be parsed or is not an
    offset datetime. Local datetimes without timezone cannot be converted. *)

val ptime_tz : ?tz_offset_s:int -> ?frac_s:int -> unit -> (Ptime.t * Ptime.tz_offset_s option) t
(** Codec for offset datetimes with timezone information.
    Decodes to [(ptime, tz_offset)] where [tz_offset] is:
    - [Some 0] for [Z] (UTC)
    - [Some offset_s] for explicit offsets like [+05:30] (19800 seconds)
    - [None] for the unknown local offset convention ([-00:00])

    @param tz_offset_s Timezone offset for encoding (default: 0 for UTC).
    @param frac_s Fractional second digits for encoding (default: 0). *)

val ptime_date : Ptime.date t
(** Codec for local dates as [Ptime.date] (i.e., [(year, month, day)]).
    Decodes TOML local date (e.g., [1979-05-27]) to a ptime date tuple.

    Uses ptime for validation, ensuring the date is valid in the
    proleptic Gregorian calendar. *)

(** {1:combinators Codec Combinators} *)

val map :
  ?kind:string -> ?doc:string ->
  ?dec:('a -> 'b) -> ?enc:('b -> 'a) ->
  'a t -> 'b t
(** [map ?dec ?enc c] transforms codec [c] through functions.
    [dec] transforms decoded values; [enc] transforms values before encoding. *)

val const : ?kind:string -> ?doc:string -> 'a -> 'a t
(** [const v] is a codec that always decodes to [v] and encodes as empty. *)

val enum : ?cmp:('a -> 'a -> int) -> ?kind:string -> ?doc:string ->
  (string * 'a) list -> 'a t
(** [enum assoc] creates a codec for string enumerations.
    @param cmp Comparison function for finding values during encoding.
    @param assoc List of [(string, value)] pairs. *)

val option : ?kind:string -> ?doc:string -> 'a t -> 'a option t
(** [option c] wraps codec [c] to decode [Some v] or encode [None] as omitted. *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] tries [ok] first, then [error]. *)

val rec' : 'a t Lazy.t -> 'a t
(** [rec' lazy_c] creates a recursive codec.
    Use for self-referential types:
    {v
    let rec tree = lazy Tomlt.(
      Table.(obj (fun v children -> Node (v, children))
        |> mem "value" int ~enc:(function Node (v, _) -> v)
        |> mem "children" (list (rec' tree)) ~enc:(function Node (_, cs) -> cs)
        |> finish))
    v} *)

(** {1:arrays Array Codecs}

    Build codecs for TOML arrays. *)

module Array : sig
  type 'a codec = 'a t

  (** Encoder specification for arrays. *)
  type ('array, 'elt) enc = {
    fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc
  }

  (** Array codec builder. *)
  type ('array, 'elt, 'builder) map

  val map :
    ?kind:string -> ?doc:string ->
    ?dec_empty:(unit -> 'builder) ->
    ?dec_add:('elt -> 'builder -> 'builder) ->
    ?dec_finish:('builder -> 'array) ->
    ?enc:('array, 'elt) enc ->
    'elt codec -> ('array, 'elt, 'builder) map
  (** [map elt] creates an array codec builder for elements of type ['elt]. *)

  val list : ?kind:string -> ?doc:string -> 'a codec -> ('a list, 'a, 'a list) map
  (** [list c] builds lists from arrays of elements decoded by [c]. *)

  val array : ?kind:string -> ?doc:string -> 'a codec -> ('a array, 'a, 'a list) map
  (** [array c] builds arrays from arrays of elements decoded by [c]. *)

  val finish : ('array, 'elt, 'builder) map -> 'array codec
  (** [finish m] completes the array codec. *)
end

val list : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [list c] is a codec for TOML arrays as OCaml lists. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array c] is a codec for TOML arrays as OCaml arrays. *)

(** {1:tables Table Codecs}

    Build codecs for TOML tables (objects). The applicative-style
    builder pattern allows defining bidirectional codecs declaratively.

    {2 Basic Usage}

    {v
    type person = { name : string; age : int }

    let person_codec = Tomlt.Table.(
      obj (fun name age -> { name; age })
      |> mem "name" Tomlt.string ~enc:(fun p -> p.name)
      |> mem "age" Tomlt.int ~enc:(fun p -> p.age)
      |> finish
    )
    v} *)

module Table : sig
  type 'a codec = 'a t

  (** {2 Member Specifications} *)

  module Mem : sig
    type 'a codec = 'a t
    type ('o, 'a) t
    (** A member specification for type ['a] within object type ['o]. *)

    val v :
      ?doc:string ->
      ?dec_absent:'a ->
      ?enc:('o -> 'a) ->
      ?enc_omit:('a -> bool) ->
      string -> 'a codec -> ('o, 'a) t
    (** [v name codec] creates a member specification.
        @param doc Documentation for this member.
        @param dec_absent Default value if member is absent (makes it optional).
        @param enc Encoder function from object to member value.
        @param enc_omit Predicate to omit member during encoding. *)

    val opt :
      ?doc:string ->
      ?enc:('o -> 'a option) ->
      string -> 'a codec -> ('o, 'a option) t
    (** [opt name codec] creates an optional member that decodes to [None]
        when absent and is omitted when encoding [None]. *)
  end

  (** {2 Table Builder} *)

  type ('o, 'dec) map
  (** Builder state for a table codec producing ['o], currently decoding ['dec]. *)

  val obj : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [obj f] starts building a table codec with decoder function [f].

      The function [f] receives each member's decoded value as arguments
      and returns the final decoded object. Build incrementally with [mem]:
      {v
      obj (fun a b c -> { a; b; c })
      |> mem "a" codec_a ~enc:...
      |> mem "b" codec_b ~enc:...
      |> mem "c" codec_c ~enc:...
      |> finish
      v} *)

  val obj' : ?kind:string -> ?doc:string -> (unit -> 'dec) -> ('o, 'dec) map
  (** [obj' f] is like [obj] but [f] is a thunk for side-effecting decoders. *)

  val mem :
    ?doc:string ->
    ?dec_absent:'a ->
    ?enc:('o -> 'a) ->
    ?enc_omit:('a -> bool) ->
    string -> 'a codec -> ('o, 'a -> 'dec) map -> ('o, 'dec) map
  (** [mem name codec m] adds a member to the table builder.

      @param name The TOML key name.
      @param codec The codec for the member's value.
      @param doc Documentation string.
      @param dec_absent Default value if absent (makes member optional).
      @param enc Extractor function for encoding.
      @param enc_omit Predicate; if [true], omit member during encoding. *)

  val opt_mem :
    ?doc:string ->
    ?enc:('o -> 'a option) ->
    string -> 'a codec -> ('o, 'a option -> 'dec) map -> ('o, 'dec) map
  (** [opt_mem name codec m] adds an optional member.
      Absent members decode as [None]; [None] values are omitted on encode. *)

  (** {2 Unknown Member Handling} *)

  val skip_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [skip_unknown m] ignores unknown members (the default). *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [error_unknown m] raises an error on unknown members. *)

  (** Collection of unknown members. *)
  module Mems : sig
    type 'a codec = 'a t

    type ('mems, 'a) enc = {
      fold : 'acc. ('acc -> string -> 'a -> 'acc) -> 'acc -> 'mems -> 'acc
    }

    type ('mems, 'a, 'builder) map

    val map :
      ?kind:string -> ?doc:string ->
      ?dec_empty:(unit -> 'builder) ->
      ?dec_add:(string -> 'a -> 'builder -> 'builder) ->
      ?dec_finish:('builder -> 'mems) ->
      ?enc:('mems, 'a) enc ->
      'a codec -> ('mems, 'a, 'builder) map

    val string_map : ?kind:string -> ?doc:string ->
      'a codec -> ('a Map.Make(String).t, 'a, (string * 'a) list) map
    (** [string_map codec] collects unknown members into a [StringMap]. *)

    val assoc : ?kind:string -> ?doc:string ->
      'a codec -> ((string * 'a) list, 'a, (string * 'a) list) map
    (** [assoc codec] collects unknown members into an association list. *)
  end

  val keep_unknown :
    ?enc:('o -> 'mems) ->
    ('mems, 'a, 'builder) Mems.map ->
    ('o, 'mems -> 'dec) map -> ('o, 'dec) map
  (** [keep_unknown mems m] collects unknown members.

      Unknown members are decoded using [mems] and passed to the decoder.
      If [enc] is provided, those members are included during encoding. *)

  val finish : ('o, 'o) map -> 'o codec
  (** [finish m] completes the table codec.
      @raise Invalid_argument if member names are duplicated. *)

  val inline : ('o, 'o) map -> 'o codec
  (** [inline m] is like [finish] but marks the table for inline encoding. *)
end

val array_of_tables : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [array_of_tables c] decodes a TOML array of tables.
    This corresponds to TOML's [[[ ]]] syntax. *)

(** {1 Generic Value Codecs} *)

val value : Toml.t t
(** [value] passes TOML values through unchanged. *)

val value_mems : (string * Toml.t) list t
(** [value_mems] decodes a table as raw key-value pairs. *)

val any :
  ?kind:string -> ?doc:string ->
  ?dec_string:'a t -> ?dec_int:'a t -> ?dec_float:'a t -> ?dec_bool:'a t ->
  ?dec_datetime:'a t -> ?dec_array:'a t -> ?dec_table:'a t ->
  ?enc:('a -> 'a t) ->
  unit -> 'a t
(** [any ()] creates a codec that handles any TOML type.
    Provide decoders for each type you want to support.
    The [enc] function should return the appropriate codec for encoding. *)

(** {1:codec_ops Encoding and Decoding} *)

val decode : 'a t -> Toml.t -> ('a, Toml.Error.t) result
(** [decode c v] decodes TOML value [v] using codec [c]. *)

val decode_exn : 'a t -> Toml.t -> 'a
(** [decode_exn c v] is like [decode] but raises on error.
    @raise Toml.Error.Error on decode failure. *)

val encode : 'a t -> 'a -> Toml.t
(** [encode c v] encodes OCaml value [v] to TOML using codec [c]. *)

val decode_string : 'a t -> string -> ('a, Toml.Error.t) result
(** [decode_string c s] parses TOML string [s] and decodes with [c]. *)

val decode_string_exn : 'a t -> string -> 'a
(** [decode_string_exn c s] is like [decode_string] but raises on error. *)

val encode_string : 'a t -> 'a -> string
(** [encode_string c v] encodes [v] to a TOML-formatted string. *)

val decode_reader : ?file:string -> 'a t -> Bytesrw.Bytes.Reader.t ->
  ('a, Toml.Error.t) result
(** [decode_reader c r] parses TOML from reader [r] and decodes with [c].
    @param file Optional filename for error messages. *)

val encode_writer : 'a t -> 'a -> Bytesrw.Bytes.Writer.t -> unit
(** [encode_writer c v w] encodes [v] and writes TOML to writer [w]. *)

(** {1 Re-exported Modules} *)

module Toml = Toml
(** The raw TOML value module. Use for low-level TOML manipulation. *)

module Error = Toml.Error
(** Error types from the TOML parser. *)
