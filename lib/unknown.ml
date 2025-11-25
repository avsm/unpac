(** Unknown fields for capturing extra JSON object members.

    This module provides a type and utilities for preserving unknown/extra
    fields when parsing JSON objects with jsont. Use with
    [Jsont.Object.keep_unknown] to capture fields not explicitly defined
    in your codec. *)

type t = Jsont.json
(** The type of unknown fields - stored as raw JSON. *)

let empty = Jsont.Object ([], Jsont.Meta.none)
(** An empty unknown fields value (empty JSON object). *)

let is_empty = function
  | Jsont.Object ([], _) -> true
  | _ -> false
(** [is_empty t] returns [true] if there are no unknown fields. *)

let jsont = Jsont.json
(** Codec for unknown fields. *)
