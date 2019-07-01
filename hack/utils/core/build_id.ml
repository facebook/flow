(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

external get_build_revision : unit -> string = "hh_get_build_revision"
external get_build_commit_time : unit -> int = "hh_get_build_commit_time"
external get_build_commit_time_string : unit -> string =
  "hh_get_build_commit_time_string"
external get_build_major : unit -> int = "hh_get_build_major"
external get_build_minor : unit -> int = "hh_get_build_minor"
external get_build_mode : unit -> string = "hh_get_build_mode"

let build_revision = get_build_revision ()

let build_commit_time = get_build_commit_time ()

let build_commit_time_string = get_build_commit_time_string ()

let build_major_version = get_build_major ()
let build_minor_version = get_build_minor ()

let build_mode = get_build_mode ()

let is_build_optimized =
  String_utils.string_starts_with build_mode "dbgo" ||
  String_utils.string_starts_with build_mode "opt" ||
  build_mode = "" (* fail open if we don't know build mode *)

(* Monotonically increasing identifier that can be used when we introduce
 * backward incompatible changes in hh_client commands, and to signal
 * new capabilities to clients.
 * v1 (hvvm 3.15, 11 May 2016) - persistent connection introduced
 * v4 (hvvm 3.18, 7 Nov 2016) - persistent connection stable
 * v5 (hvvm 3.23, 17 Nov 2017) - 'hh_client lsp' stable
 *)
let build_api_version = 5
