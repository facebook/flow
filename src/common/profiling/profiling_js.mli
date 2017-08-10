(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type running
type finished

val with_profiling: (running -> running * 'a) -> finished * 'a

val start_timer: timer:string -> running -> running
val stop_timer: timer:string -> running -> running
val get_finished_timer: timer:string -> running -> (float * float * float * float) option

val sample_memory: metric:string -> value:float -> running -> running

val get_timing_json_string: finished -> string
val get_memory_json_string: finished -> string

val to_json_properties: finished -> (string * Hh_json.json) list
