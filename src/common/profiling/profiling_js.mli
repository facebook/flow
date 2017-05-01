(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 type t

 val empty: t

 val start_timer: timer:string -> t -> t
 val stop_timer: timer:string -> t -> t
 val get_finished_timer: timer:string -> t -> (float * float) option

 val sample_memory: metric:string -> value:float -> t -> t

 val get_timing_json_string: t -> string
 val get_memory_json_string: t -> string

 val to_json: t -> Hh_json.json
 val to_json_properties: t -> (string * Hh_json.json) list
