(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative {{:https://toml.io/en/v1.1.0}TOML 1.1} codecs.

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

    {2 Datetime Handling}

    Tomlt uses {{:https://erratique.ch/software/ptime}Ptime} for all datetime
    operations, providing a unified approach to TOML's four datetime formats:

    {v
    (* Accept any TOML datetime format, normalize to Ptime.t *)
    type event = { name : string; when_ : Ptime.t }

    let event_codec = Tomlt.(Table.(
      obj (fun name when_ -> { name; when_ })
      |> mem "name" string ~enc:(fun e -> e.name)
      |> mem "when" (ptime ()) ~enc:(fun e -> e.when_)
      |> finish
    ))

    (* All of these work: *)
    (* when = 2024-01-15T10:30:00Z      -> offset datetime *)
    (* when = 2024-01-15T10:30:00       -> local datetime (uses system tz) *)
    (* when = 2024-01-15                -> date only (assumes midnight) *)
    (* when = 10:30:00                  -> time only (uses today's date) *)
    v}

    See {!section:ptime_codecs} for the complete datetime codec API.

    {2 Module Overview}

    - {!section:datetime} - Structured datetime types (for advanced use)
    - {!section:codec} - Core codec type and combinators
    - {!section:base} - Primitive type codecs
    - {!section:ptime_codecs} - Ptime-based datetime codecs
    - {!section:combinators} - Codec transformers
    - {!section:arrays} - Array codec builders
    - {!section:tables} - Table/object codec builders
    - {!section:codec_ops} - Encoding and decoding operations *)

(** {1:preliminaries Preliminaries} *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

(** Sorts of TOML values.

    TOML values are classified into sorts (types). This module provides
    utilities for working with these sorts programmatically. *)
module Sort : sig
  type t =
    | String  (** Strings *)
    | Int     (** Integers *)
    | Float   (** Floating-point numbers *)
    | Bool    (** Booleans *)
    | Datetime (** Offset datetimes *)
    | Datetime_local (** Local datetimes *)
    | Date    (** Local dates *)
    | Time    (** Local times *)
    | Array   (** Arrays *)
    | Table   (** Tables (objects) *)
  (** The type for sorts of TOML values. *)

  val to_string : t -> string
  (** [to_string sort] is a human-readable string for [sort]. *)

  val pp : t fmt
  (** [pp] formats sorts. *)

  val of_toml : Toml.t -> t
  (** [of_toml v] returns the sort of TOML value [v]. *)

  val or_kind : kind:string -> t -> string
  (** [or_kind ~kind sort] is [to_string sort] if [kind] is [""] and
      [kind] otherwise. *)

  val kinded : kind:string -> t -> string
  (** [kinded ~kind sort] is [to_string sort] if [kind] is [""]
      and [String.concat " " \[kind; to_string sort\]] otherwise. *)
end

(** {1:datetime Structured Datetime Types}

    TOML 1.1 supports four datetime formats:

    - {b {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime}}:
      [1979-05-27T07:32:00Z] or [1979-05-27T07:32:00-07:00]
    - {b {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime}}:
      [1979-05-27T07:32:00] (no timezone)
    - {b {{:https://toml.io/en/v1.1.0#local-date}Local date}}:
      [1979-05-27]
    - {b {{:https://toml.io/en/v1.1.0#local-time}Local time}}:
      [07:32:00] or [07:32:00.999999]

    These modules provide structured representations for parsing and
    formatting. For most use cases, prefer the {!section:ptime_codecs}
    which provide a unified Ptime-based interface. *)

(** Timezone offsets for {{:https://toml.io/en/v1.1.0#offset-date-time}TOML
    offset datetimes}.

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

(** {{:https://toml.io/en/v1.1.0#local-date}Local dates} (no timezone information).

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

(** {{:https://toml.io/en/v1.1.0#local-time}Local times} (no date or timezone).

    Represents a time of day like [07:32:00] or [07:32:00.999999]. *)
module Time : sig
  type t = {
    hour : int;     (** Hour (0-23) *)
    minute : int;   (** Minute (0-59) *)
    second : int;   (** Second (0-59, 60 for leap seconds) *)
    frac : float;   (** Fractional seconds in range \[0.0, 1.0) *)
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

(** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetimes}
    (date + time + timezone).

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

(** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetimes}
    (date + time, no timezone).

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
(** [with_doc ?kind ?doc c] is [c] with its {!kind} or {!doc} updated
    to the corresponding values if specified. Unlike {!map}, this does
    not change the codec's decoding or encoding behavior.

    {4 Example}
    {[
      let person_id = with_doc ~kind:"person ID" int
    ]} *)

(** {1:base Base Type Codecs}

    Primitive codecs for {{:https://toml.io/en/v1.1.0}TOML 1.1}'s basic
    value types. *)

val bool : bool t
(** Codec for {{:https://toml.io/en/v1.1.0#boolean}TOML booleans}. *)

val int : int t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to OCaml [int].
    Supports decimal, hex ([0x]), octal ([0o]), and binary ([0b]) formats.
    @raise Int_overflow if the value exceeds platform [int] range. *)

val int32 : int32 t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to [int32]. *)

val int64 : int64 t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to [int64]. *)

val float : float t
(** Codec for {{:https://toml.io/en/v1.1.0#float}TOML floats}.
    Handles [inf], [-inf], and [nan] per the spec. *)

val number : float t
(** Codec that accepts both {{:https://toml.io/en/v1.1.0#integer}integers}
    and {{:https://toml.io/en/v1.1.0#float}floats} as [float].
    Integers are converted to floats during decoding. *)

val string : string t
(** Codec for {{:https://toml.io/en/v1.1.0#string}TOML strings} (UTF-8 encoded).
    Supports basic strings, literal strings, and their multiline variants. *)

val int_as_string : int t
(** Codec for integers stored as TOML strings.

    On decode, uses [int_of_string_opt] which accepts decimal, hex ([0x]),
    octal ([0o]), and binary ([0b]) formats.
    On encode, uses [Int.to_string] (decimal).

    Useful when integers must be stored as strings for compatibility,
    or when you need to preserve leading zeros or specific formats. *)

val int64_as_string : int64 t
(** Codec for 64-bit integers stored as TOML strings.

    Like {!int_as_string} but for [int64] values. Uses [Int64.of_string_opt]
    for decoding and [Int64.to_string] for encoding. *)

(** {1:ptime_codecs Ptime Datetime Codecs}

    Tomlt provides a unified datetime handling system built on
    {{:https://erratique.ch/software/ptime}Ptime}. All
    {{:https://toml.io/en/v1.1.0#offset-date-time}TOML datetime formats}
    can be decoded to [Ptime.t] timestamps with sensible defaults for
    incomplete information.

    {2 TOML Datetime Formats}

    {{:https://toml.io/en/v1.1.0}TOML 1.1} supports four datetime formats
    with varying levels of precision:

    {v
    # Offset datetime - full timestamp with timezone (unambiguous)
    # See: https://toml.io/en/v1.1.0#offset-date-time
    published = 2024-01-15T10:30:00Z
    published = 2024-01-15T10:30:00-05:00

    # Local datetime - no timezone (wall clock time)
    # See: https://toml.io/en/v1.1.0#local-date-time
    meeting = 2024-01-15T10:30:00

    # Local date - date only
    # See: https://toml.io/en/v1.1.0#local-date
    birthday = 1979-05-27

    # Local time - time only
    # See: https://toml.io/en/v1.1.0#local-time
    alarm = 07:30:00
    v}

    {2 Choosing a Codec}

    - {!val:ptime} - {b Recommended for most cases.} Accepts any datetime format
      and normalizes to [Ptime.t] by filling in sensible defaults.

    - {!val:ptime_opt} - {b For strict validation.} Only accepts offset datetimes
      with explicit timezone. Rejects ambiguous local formats.

    - {!val:ptime_date} - For fields that should only contain dates.

    - {!val:ptime_span} - For fields that should only contain times (as duration).

    - {!val:ptime_full} - {b For roundtripping.} Preserves the exact datetime
      variant from the source, allowing faithful re-encoding.

    {2 Timezone Handling}

    For local datetimes without explicit timezone, Tomlt uses
    [Ptime_clock.current_tz_offset_s ()] to get the system timezone.
    You can override this by passing [~tz_offset_s]:

    {v
    (* Force UTC interpretation for local datetimes *)
    let codec = ptime ~tz_offset_s:0 ()

    (* Force Eastern Time (-05:00 = -18000 seconds) *)
    let codec = ptime ~tz_offset_s:(-18000) ()
    v}

    {2 Examples}

    {3 Basic Event Tracking}
    {v
    type event = { name : string; timestamp : Ptime.t }

    let event_codec = Tomlt.(Table.(
      obj (fun name timestamp -> { name; timestamp })
      |> mem "name" string ~enc:(fun e -> e.name)
      |> mem "when" (ptime ()) ~enc:(fun e -> e.timestamp)
      |> finish
    ))

    (* All of these decode successfully: *)
    (* when = 2024-01-15T10:30:00Z       *)
    (* when = 2024-01-15T10:30:00        *)
    (* when = 2024-01-15                 *)
    (* when = 10:30:00                   *)
    v}

    {3 Strict Timestamp Validation}
    {v
    type log_entry = { message : string; timestamp : Ptime.t }

    let log_codec = Tomlt.(Table.(
      obj (fun message timestamp -> { message; timestamp })
      |> mem "message" string ~enc:(fun e -> e.message)
      |> mem "timestamp" (ptime_opt ()) ~enc:(fun e -> e.timestamp)
      |> finish
    ))

    (* Only accepts: timestamp = 2024-01-15T10:30:00Z *)
    (* Rejects:      timestamp = 2024-01-15T10:30:00  *)
    v}

    {3 Birthday (Date Only)}
    {v
    type person = { name : string; birthday : Ptime.date }

    let person_codec = Tomlt.(Table.(
      obj (fun name birthday -> { name; birthday })
      |> mem "name" string ~enc:(fun p -> p.name)
      |> mem "birthday" ptime_date ~enc:(fun p -> p.birthday)
      |> finish
    ))

    (* birthday = 1979-05-27 -> (1979, 5, 27) *)
    v}

    {3 Daily Alarm (Time Only)}
    {v
    type alarm = { label : string; time : Ptime.Span.t }

    let alarm_codec = Tomlt.(Table.(
      obj (fun label time -> { label; time })
      |> mem "label" string ~enc:(fun a -> a.label)
      |> mem "time" ptime_span ~enc:(fun a -> a.time)
      |> finish
    ))

    (* time = 07:30:00 -> 27000 seconds (7.5 hours from midnight) *)
    v}

    {3 Preserving Datetime Format}
    {v
    type flexible_event = {
      name : string;
      when_ : Toml.ptime_datetime;
    }

    let flexible_codec = Tomlt.(Table.(
      obj (fun name when_ -> { name; when_ })
      |> mem "name" string ~enc:(fun e -> e.name)
      |> mem "when" (ptime_full ()) ~enc:(fun e -> e.when_)
      |> finish
    ))

    (* Decoding preserves the variant:
       when = 2024-01-15T10:30:00Z -> `Datetime (ptime, Some 0)
       when = 2024-01-15T10:30:00  -> `Datetime_local ptime
       when = 2024-01-15           -> `Date (2024, 1, 15)
       when = 10:30:00             -> `Time (10, 30, 0, 0)

       Encoding reproduces the original format. *)
    v} *)

val ptime :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  ?now:(unit -> Ptime.t) ->
  ?frac_s:int ->
  unit -> Ptime.t t
(** Datetime codec that converts any TOML datetime to {!Ptime.t}.

    This is the recommended codec for most datetime use cases. It handles
    all TOML datetime variants by filling in sensible defaults:

    - {b Offset datetime} ([2024-01-15T10:30:00Z]): Parsed directly to [Ptime.t]
    - {b Local datetime} ([2024-01-15T10:30:00]): Converted using the timezone
    - {b Local date} ([2024-01-15]): Assumed to be midnight (00:00:00) in the
      given timezone
    - {b Local time} ([10:30:00]): Combined with today's date using [now]

    Encoding always produces an RFC 3339 offset datetime string.

    {4 Parameters}

    @param tz_offset_s Explicit timezone offset in seconds, used for:
      - Converting local datetimes to [Ptime.t]
      - Converting local dates to [Ptime.t] (at midnight)
      - Converting local times to [Ptime.t] (on today's date)
      - Formatting the timezone when encoding

      Common values:
      - [0] = UTC
      - [3600] = +01:00 (Central European Time)
      - [-18000] = -05:00 (Eastern Standard Time)
      - [-28800] = -08:00 (Pacific Standard Time)

      If not provided, [get_tz] is called. If neither is provided, defaults
      to UTC (0).

    @param get_tz Function to get the current timezone offset. Called when
      [tz_offset_s] is not provided. Pass [Tomlt_unix.current_tz_offset_s]
      for OS-specific timezone support:
      {[let codec = ptime ~get_tz:Tomlt_unix.current_tz_offset_s ()]}

    @param now Function to get the current time. Used when decoding local
      times (e.g., [10:30:00]) to combine with today's date. Pass
      [Tomlt_unix.now] for OS-specific time support. If not provided,
      defaults to [Ptime.epoch] (1970-01-01).

    @param frac_s Number of fractional second digits to include when encoding.
      Range: 0-12. Default: 0 (whole seconds only). For example, [~frac_s:3]
      produces [2024-01-15T10:30:00.123Z].

    {4 Example}
    {[
      type event = { name : string; timestamp : Ptime.t }

      let event_codec = Tomlt.(Table.(
        obj (fun name timestamp -> { name; timestamp })
        |> mem "name" string ~enc:(fun e -> e.name)
        |> mem "when" (ptime ()) ~enc:(fun e -> e.timestamp)
        |> finish
      ))

      (* All of these decode to a Ptime.t: *)
      let e1 = decode_string_exn event_codec {|name="a" when=2024-01-15T10:30:00Z|}
      let e2 = decode_string_exn event_codec {|name="b" when=2024-01-15T10:30:00|}
      let e3 = decode_string_exn event_codec {|name="c" when=2024-01-15|}
      let e4 = decode_string_exn event_codec {|name="d" when=10:30:00|}
    ]} *)

val ptime_opt : ?tz_offset_s:int -> ?frac_s:int -> unit -> Ptime.t t
(** Strict datetime codec that only accepts offset datetimes.

    Unlike {!ptime} which accepts any datetime format, this codec requires
    an explicit timezone and rejects local datetime variants. Use this when
    you need unambiguous timestamps and want to reject values that would
    require timezone assumptions.

    {4 Accepted}
    - [2024-01-15T10:30:00Z] (UTC)
    - [2024-01-15T10:30:00+05:30] (explicit offset)
    - [2024-01-15T10:30:00-08:00] (explicit offset)

    {4 Rejected}

    These raise [Value_error]:

    - [2024-01-15T10:30:00] (local datetime - no timezone)
    - [2024-01-15] (local date)
    - [10:30:00] (local time)

    @param tz_offset_s Timezone offset for encoding. Default: 0 (UTC).
    @param frac_s Fractional second digits for encoding. Default: 0.

    {4 Example}
    {[
      type audit_log = { action : string; timestamp : Ptime.t }

      let audit_codec = Tomlt.(Table.(
        obj (fun action timestamp -> { action; timestamp })
        |> mem "action" string ~enc:(fun a -> a.action)
        |> mem "timestamp" (ptime_opt ()) ~enc:(fun a -> a.timestamp)
        |> finish
      ))

      (* Valid: timestamp = 2024-01-15T10:30:00Z *)
      (* Error: timestamp = 2024-01-15T10:30:00 (no timezone) *)
    ]} *)

val ptime_span : Ptime.Span.t t
(** Codec for TOML local times as [Ptime.Span.t] (duration from midnight).

    Decodes a local time like [07:32:00] or [14:30:45.123] to a [Ptime.Span.t]
    representing the time elapsed since midnight (00:00:00).

    When encoding, the span is formatted as a local time string. Values are
    clamped to the range [00:00:00] to [23:59:59.999999999].

    {4 Decoding}
    - [07:32:00] -> 27120 seconds (7 hours, 32 minutes)
    - [14:30:45.5] -> 52245.5 seconds
    - [00:00:00] -> 0 seconds

    {4 Encoding}
    - 27120 seconds -> [07:32:00]
    - 52245.5 seconds -> [14:30:45.5]

    {4 Example}
    {[
      type daily_schedule = { name : string; start_time : Ptime.Span.t }

      let schedule_codec = Tomlt.(Table.(
        obj (fun name start_time -> { name; start_time })
        |> mem "name" string ~enc:(fun s -> s.name)
        |> mem "start_time" ptime_span ~enc:(fun s -> s.start_time)
        |> finish
      ))

      (* start_time = 09:00:00 -> 32400 seconds *)
    ]} *)

val ptime_date : Ptime.date t
(** Codec for TOML local dates as [Ptime.date] (a [(year, month, day)] tuple).

    Decodes a local date like [1979-05-27] to an [(int * int * int)] tuple.
    Only accepts [Date_local] TOML values; rejects datetimes and times.

    {4 Example}
    {[
      type person = { name : string; birthday : Ptime.date }

      let person_codec = Tomlt.(Table.(
        obj (fun name birthday -> { name; birthday })
        |> mem "name" string ~enc:(fun p -> p.name)
        |> mem "birthday" ptime_date ~enc:(fun p -> p.birthday)
        |> finish
      ))

      (* birthday = 1979-05-27 -> (1979, 5, 27) *)
    ]}

    To work with dates as [Ptime.t] (at midnight), use {!ptime} instead. *)

val ptime_full :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  unit -> Toml.ptime_datetime t
(** Codec that preserves full datetime variant information.

    Unlike {!ptime} which normalizes all datetime formats to [Ptime.t],
    this codec returns a polymorphic variant that indicates exactly what
    was present in the TOML source. This is essential for:

    - Distinguishing between datetime formats during decoding
    - Roundtripping TOML files while preserving the original format
    - Applications that treat different datetime formats differently

    {4 Decoded Variants}

    The [Toml.ptime_datetime] type is:
    {[
      type ptime_datetime = [
        | `Datetime of Ptime.t * Ptime.tz_offset_s option
        | `Datetime_local of Ptime.t
        | `Date of Ptime.date
        | `Time of int * int * int * int  (* hour, minute, second, nanoseconds *)
      ]
    ]}

    {4 Mapping from TOML}

    - [2024-01-15T10:30:00Z] -> [`Datetime (ptime, Some 0)]
    - [2024-01-15T10:30:00-05:00] -> [`Datetime (ptime, Some (-18000))]
    - [2024-01-15T10:30:00] -> [`Datetime_local ptime]
    - [2024-01-15] -> [`Date (2024, 1, 15)]
    - [10:30:45.123] -> [`Time (10, 30, 45, 123_000_000)]

    {4 Encoding}

    When encoding, the variant determines the output format:
    - [`Datetime] -> offset datetime with timezone
    - [`Datetime_local] -> local datetime (no timezone)
    - [`Date] -> local date
    - [`Time] -> local time

    @param tz_offset_s Explicit timezone offset for converting
      [`Datetime_local] to [Ptime.t].

    @param get_tz Function to get the current timezone offset. Called when
      [tz_offset_s] is not provided. Pass [Tomlt_unix.current_tz_offset_s]
      for OS-specific timezone support. If neither is provided, defaults to
      UTC (0).

    {4 Example}
    {[
      type schedule_item = {
        description : string;
        when_ : Toml.ptime_datetime;
      }

      let item_codec = Tomlt.(Table.(
        obj (fun description when_ -> { description; when_ })
        |> mem "description" string ~enc:(fun i -> i.description)
        |> mem "when" (ptime_full ()) ~enc:(fun i -> i.when_)
        |> finish
      ))

      (* Can distinguish between:
         - when = 2024-01-15T10:00:00Z  (specific instant)
         - when = 2024-01-15T10:00:00   (wall clock time)
         - when = 2024-01-15            (all day)
         - when = 10:00:00              (daily recurring)
      *)
    ]} *)

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

val iter :
  ?kind:string -> ?doc:string ->
  ?dec:('a -> unit) -> ?enc:('a -> unit) ->
  'a t -> 'a t
(** [iter ?dec ?enc c] applies [dec] on decoding and [enc] on encoding
    but otherwise behaves like [c]. Useful for:
    - Asserting additional constraints on decoded values
    - Tracing/debugging codec behavior
    - Side effects during encoding/decoding

    {4 Example}
    {[
      (* Trace all decoded integers *)
      let traced_int = iter int
        ~dec:(fun i -> Printf.printf "Decoded: %d\n" i)

      (* Validate port range *)
      let port = iter int
        ~dec:(fun p ->
          if p < 0 || p > 65535 then
            failwith "port out of range")
    ]} *)

val recode : dec:'a t -> ('a -> 'b) -> enc:'b t -> 'b t
(** [recode ~dec f ~enc] decodes like [dec] followed by [f], and
    encodes using [enc]. This allows changing the TOML representation
    while maintaining bidirectionality.

    {4 Example}
    {[
      (* Store URI as string, decode to Uri.t *)
      let uri_codec =
        recode ~dec:string Uri.of_string ~enc:string_of_uri

      (* Convert between string list and comma-separated string *)
      let tags_codec =
        recode
          ~dec:string
          (String.split_on_char ',')
          ~enc:(list string)
    ]} *)

(** {2:query Query Combinators}

    Extract single values from arrays or tables without processing
    the entire structure. *)

val nth : ?absent:'a -> int -> 'a t -> 'a t
(** [nth n t] decodes the [n]th element of a TOML array with [t].
    Other elements are skipped.

    @param absent Value to use if the index is out of bounds.
      If not provided, an error is raised for out-of-bounds access.
    @raise Value_error if [n] is out of bounds and [absent] is not provided.

    {4 Example}
    {[
      (* Get first element of array *)
      let first = nth 0 string

      (* Get second element with default *)
      let second = nth ~absent:"default" 1 string
    ]} *)

val mem : ?absent:'a -> string -> 'a t -> 'a t
(** [mem name t] decodes the member named [name] from a TOML table with [t].
    Other members are skipped. This is simpler than {!Table} when you only
    need a single value.

    @param absent Value to use if the member doesn't exist.
      If not provided, an error is raised for missing members.

    {4 Example}
    {[
      (* Extract just the "version" field *)
      let version = mem "version" string

      (* With default value *)
      let debug = mem ~absent:false "debug" bool
    ]} *)

(** {2:folding Folding Combinators}

    Process all elements of an array or table. These are decode-only;
    encoding produces empty containers. *)

val fold_array : 'a t -> (int -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_array t f init] folds [f] over all elements of a TOML array,
    decoding each element with [t]. The fold function receives the index,
    decoded value, and accumulator.

    Encodes to an empty array (folding is decode-only).

    {4 Example}
    {[
      (* Sum all integers in array *)
      let sum = fold_array int (fun _i x acc -> x + acc) 0

      (* Collect values into a Set *)
      let string_set = fold_array string
        (fun _i s acc -> StringSet.add s acc) StringSet.empty
    ]} *)

val fold_table : 'a t -> (string -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_table t f init] folds [f] over all members of a TOML table,
    decoding each value with [t]. The fold function receives the key,
    decoded value, and accumulator.

    Encodes to an empty table (folding is decode-only).

    {4 Example}
    {[
      (* Build a map from table *)
      let string_map = fold_table string
        (fun k v acc -> StringMap.add k v acc) StringMap.empty

      (* Count members *)
      let count = fold_table any (fun _k _v n -> n + 1) 0
    ]} *)

(** {2:ignoring Ignoring and Placeholders} *)

val ignore : unit t
(** [ignore] maps any TOML value to [()] on decoding and errors on encoding.
    Use for values you want to skip during decoding. *)

val zero : unit t
(** [zero] maps any TOML value to [()] on decoding and encodes as an
    empty table. Useful for placeholder values. *)

val todo : ?kind:string -> ?doc:string -> ?dec_stub:'a -> unit -> 'a t
(** [todo ?dec_stub ()] is a placeholder codec for work in progress.
    - On decode: returns [dec_stub] if provided, errors otherwise
    - On encode: always errors

    Useful during development to mark incomplete parts of a codec.

    {4 Example}
    {[
      type config = { name : string; advanced : unit (* TODO *) }

      let config_codec = Tomlt.(Table.(
        obj (fun name advanced -> { name; advanced })
        |> mem "name" string ~enc:(fun c -> c.name)
        |> mem "advanced" (todo ~dec_stub:() ()) ~enc:(fun _ -> ())
        |> finish
      ))
    ]} *)

(** {1:arrays Array Codecs}

    Build codecs for {{:https://toml.io/en/v1.1.0#array}TOML arrays}. *)

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
(** [list c] is a codec for {{:https://toml.io/en/v1.1.0#array}TOML arrays}
    as OCaml lists. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array c] is a codec for {{:https://toml.io/en/v1.1.0#array}TOML arrays}
    as OCaml arrays. *)

(** {1:tables Table Codecs}

    Build codecs for {{:https://toml.io/en/v1.1.0#table}TOML tables}
    (key-value mappings). The applicative-style builder pattern allows
    defining bidirectional codecs declaratively.

    Tables can be defined using standard headers or as
    {{:https://toml.io/en/v1.1.0#inline-table}inline tables}.
    {{:https://toml.io/en/v1.1.0#keys}Keys} can be bare, quoted, or dotted.

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
(** [array_of_tables c] decodes a
    {{:https://toml.io/en/v1.1.0#array-of-tables}TOML array of tables}.
    This corresponds to TOML's [[\[\[name\]\]]] syntax for defining
    arrays of table elements. *)

(** {1 Generic Value Codecs} *)

val value : Toml.t t
(** [value] passes TOML values through unchanged.
    Useful for preserving parts of a document without interpretation. *)

val value_mems : (string * Toml.t) list t
(** [value_mems] decodes a {{:https://toml.io/en/v1.1.0#table}table}
    as raw key-value pairs. *)

val any :
  ?kind:string -> ?doc:string ->
  ?dec_string:'a t -> ?dec_int:'a t -> ?dec_float:'a t -> ?dec_bool:'a t ->
  ?dec_datetime:'a t -> ?dec_array:'a t -> ?dec_table:'a t ->
  ?enc:('a -> 'a t) ->
  unit -> 'a t
(** [any ()] creates a codec that handles any TOML type.
    Provide decoders for each type you want to support.
    The [enc] function should return the appropriate codec for encoding. *)

(** {1:codec_ops Encoding and Decoding}

    Functions for converting between OCaml values and
    {{:https://toml.io/en/v1.1.0}TOML 1.1} documents. *)

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
