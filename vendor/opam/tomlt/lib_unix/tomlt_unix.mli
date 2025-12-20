(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OS-based time utilities for Tomlt.

    This module provides timezone and time functions using
    {{:https://erratique.ch/software/ptime}ptime.clock.os}, suitable for
    non-Eio applications that need system timezone support.

    {2 Timezone}

    Get the current system timezone offset. *)

val current_tz_offset_s : unit -> int option
(** [current_tz_offset_s ()] returns the current system timezone offset in
    seconds from UTC. Returns [Some offset] where positive values are east
    of UTC (e.g., 3600 for +01:00) and negative values are west. *)

(** {2 Current Time} *)

val now : unit -> Ptime.t
(** [now ()] returns the current time as a [Ptime.t]. *)

val today_date : ?tz_offset_s:int -> unit -> Ptime.date
(** [today_date ?tz_offset_s ()] returns today's date as [(year, month, day)].
    If [tz_offset_s] is not provided, uses [current_tz_offset_s ()]. *)
