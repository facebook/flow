(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type running

type finished

val with_profiling_lwt :
  label:string -> should_print_summary:bool -> (running -> 'a Lwt.t) -> (finished * 'a) Lwt.t

val get_profiling_duration : finished -> float

val merge : from:finished -> into:running -> unit

val with_timer_lwt :
  ?should_print:bool -> timer:string -> f:(unit -> 'a Lwt.t) -> running -> 'a Lwt.t

val legacy_sample_memory : metric:string -> value:float -> running -> unit

val sample_memory : ?group:string -> metric:string -> value:float -> running -> unit

val add_memory :
  ?group:string -> metric:string -> start:float -> delta:float -> hwm_delta:float -> running -> unit

val get_timing_json_string : finished -> string

val get_abridged_timing_json_string : finished -> string

val get_abridged_legacy_timing_json_string : finished -> string

val get_memory_json_string : finished -> string

val get_abridged_memory_json_string : finished -> string

val to_json_properties : finished -> (string * Hh_json.json) list

val to_legacy_json_properties : finished -> (string * Hh_json.json) list

val print_summary : finished -> unit
