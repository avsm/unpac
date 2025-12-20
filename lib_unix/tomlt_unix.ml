(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let current_tz_offset_s = Ptime_clock.current_tz_offset_s

let now = Ptime_clock.now

let today_date ?tz_offset_s () =
  let tz_offset_s = match tz_offset_s with
    | Some tz -> tz
    | None ->
        match current_tz_offset_s () with
        | Some tz -> tz
        | None -> 0
  in
  Ptime.to_date ~tz_offset_s (now ())
