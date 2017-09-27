(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type running
type finished

val with_profiling: (running -> 'a) -> finished * 'a

val start_timer: timer:string -> running -> unit
val stop_timer: timer:string -> running -> unit
val get_finished_timer: timer:string -> running -> (float * float * float * float) option

val sample_memory: metric:string -> value:float -> running -> unit

val get_timing_json_string: finished -> string
val get_memory_json_string: finished -> string

val to_json_properties: finished -> (string * Hh_json.json) list
