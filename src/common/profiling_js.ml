(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 module Timing : sig
   type t
   val empty: t
   val start_timer: timer:string -> t -> t
   val stop_timer: timer:string -> t -> t
   val get_finished_timer: timer:string -> t -> (float * float) option
   val to_json: t -> Hh_json.json
 end = struct
  type result = {
    start_wall_age: float;
    wall_duration: float;
  }

  type t = {
    running_timers: float SMap.t;
    results: result SMap.t;
  }

  let empty = { running_timers = SMap.empty; results = SMap.empty }

  let start_timer ~timer { running_timers; results; } =
    {
      running_timers = SMap.add timer (Unix.gettimeofday ()) running_timers;
      results;
    }

  let flow_start_time = Unix.gettimeofday ()

  let stop_timer ~timer { running_timers; results; } =
    match SMap.get timer running_timers with
    | None -> { running_timers; results; }
    | Some timer_start_time ->
        let result = {
          start_wall_age = timer_start_time -. flow_start_time;
          wall_duration = Unix.gettimeofday () -. timer_start_time;
        } in
        {
          running_timers = SMap.remove timer running_timers;
          results = SMap.add timer result results;
        }

  let get_finished_timer ~timer { results; _; } =
    match SMap.get timer results with
    | None -> None
    | Some { start_wall_age; wall_duration; } ->
        Some (start_wall_age, wall_duration)

  let json_of_result { start_wall_age; wall_duration; } =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "start_wall_age", JSON_Number (string_of_float_trunc start_wall_age);
      "wall_duration", JSON_Number (string_of_float_trunc wall_duration);
    ]

  let to_json { results; _; } =
    let open Hh_json in
    let results = results
    |> SMap.map json_of_result
    |> SMap.elements in
    JSON_Object [
      "results", JSON_Object results;
    ]
 end

module Memory: sig
  type t
  val empty: t
  val sample_memory: metric:string -> value:float -> t -> t
  val to_json: t -> Hh_json.json
end = struct
  type t = float SMap.t

  let empty = SMap.empty

  let sample_memory ~metric ~value memory =
    SMap.add metric value memory

  let to_json results =
    let open Hh_json in
    let results = results
    |> SMap.map (fun v -> JSON_Number (Utils_js.string_of_float_trunc v))
    |> SMap.elements in
    JSON_Object results
end

type t = {
  timing: Timing.t;
  memory: Memory.t;
}

let empty = {
  timing = Timing.empty;
  memory = Memory.empty;
}

let start_timer ~timer profile =
  { profile with timing = Timing.start_timer ~timer profile.timing }

let stop_timer ~timer profile =
  { profile with timing = Timing.stop_timer ~timer profile.timing }

let get_finished_timer ~timer profile =
  Timing.get_finished_timer ~timer profile.timing

let sample_memory ~metric ~value profile =
  { profile with memory = Memory.sample_memory ~metric ~value profile.memory }

let to_json_properties profile =
  [
    "timing", Timing.to_json profile.timing;
    "memory", Memory.to_json profile.memory;
  ]

let to_json profile = Hh_json.JSON_Object (to_json_properties profile)

let get_timing_json_string profile =
  Timing.to_json profile.timing |> Hh_json.json_to_string

let get_memory_json_string profile =
  Memory.to_json profile.memory |> Hh_json.json_to_string
