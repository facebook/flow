(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type estimates = {
  estimated_time_to_recheck: float;
  estimated_time_to_restart: float;
  estimated_time_to_init: float;
  estimated_time_to_merge_a_file: float;
  estimated_files_to_merge: int;
  estimated_files_to_init: int;
}

val init: options:Options.t -> init_time:float -> parsed_count:int -> unit Lwt.t

val record_merge_time: options:Options.t -> total_time:float -> merged_files:int -> unit

val get_init_time: unit -> float
val get_per_file_time: unit -> float
