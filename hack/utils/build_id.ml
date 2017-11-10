(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external get_build_revision : unit -> string = "hh_get_build_revision"
external get_build_commit_time : unit -> int = "hh_get_build_commit_time"
external get_build_commit_time_string : unit -> string =
  "hh_get_build_commit_time_string"

let build_revision = get_build_revision ()

let build_id_ohai = build_revision ^ " " ^ get_build_commit_time_string ()

let build_commit_time = get_build_commit_time ()

(* Monotonically increasing identifier that can be used when we introduce
 * backward incompatible changes in hh_client commands, and to signal
 * new capabilities to clients.
 * v1 (hvvm 3.15, 11 May 2016) - persistent connection introduced
 * v4 (hvvm 3.18, 7 Nov 2016) - persistent connection stable
 * v5 (hvvm 3.23, 17 Nov 2017) - 'hh_client lsp' stable
 *)
let build_api_version = 5
