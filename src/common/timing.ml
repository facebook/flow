(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = {
  start_wall_age: float;
  wall_duration: float;
}

type t = {
  running_timers: float SMap.t;
  results: result SMap.t;
}

let create () = { running_timers = SMap.empty; results = SMap.empty }

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

let to_string sample = to_json sample |> Hh_json.json_to_string
