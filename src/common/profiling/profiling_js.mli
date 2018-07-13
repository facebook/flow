(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type running
type finished

val with_profiling_lwt: should_print_summary:bool -> (running -> 'a Lwt.t) -> (finished * 'a) Lwt.t
val get_profiling_duration: finished -> float

val with_timer_prefix_lwt: prefix:string -> f:(unit -> 'a Lwt.t) -> running -> 'a Lwt.t

val start_timer: timer:string -> running -> unit
val stop_timer: timer:string -> running -> unit
val with_timer_lwt: timer:string -> f:(unit -> 'a Lwt.t) -> running -> 'a Lwt.t
val get_finished_timer: timer:string -> running -> (float * float * float * float) option

val sample_memory: metric:string -> value:float -> running -> unit

val get_timing_json_string: finished -> string
val get_abridged_timing_json_string: finished -> string
val get_memory_json_string: finished -> string

val to_json_properties: finished -> (string * Hh_json.json) list

val print_summary: finished -> unit
