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
  type time_measurement = {
    start_age: float;
    duration: float;
  }

  type result = {
    user: time_measurement;
    system: time_measurement;
    worker_user: time_measurement;
    worker_system: time_measurement;
    wall: time_measurement;
  }

  type running_timer = {
    user_start: float;
    system_start: float;
    worker_user_start: float;
    worker_system_start: float;
    wall_start: float;
  }

  type t = {
    running_timers: running_timer SMap.t;
    results: result SMap.t;
  }

  let empty = { running_timers = SMap.empty; results = SMap.empty }

  (* Returns the user cpu and system cpu times *)
  let times () = Unix.(
    let tm = times () in
    (* Warning - cutime and cstime (children times) don't work on Windows *)
    (tm.tms_utime +. tm.tms_cutime, tm.tms_stime +. tm.tms_cstime)
  )

  let worker_times () =
    let worker_user_time = match Measure.get_sum "worker_user_time" with
    | None -> 0.0
    | Some time -> time in
    let worker_system_time = match Measure.get_sum "worker_system_time" with
    | None -> 0.0
    | Some time -> time in
    (worker_user_time, worker_system_time)

  let start_timer ~timer { running_timers; results; } =
    let wall_start = Unix.gettimeofday () in
    let (user_start, system_start) = times () in
    let (worker_user_start, worker_system_start) = worker_times () in
    let running_timer = {
      user_start;
      system_start;
      worker_user_start;
      worker_system_start;
      wall_start;
    } in
    {
      running_timers = SMap.add timer running_timer running_timers;
      results;
    }

  let flow_start_time = Unix.gettimeofday ()

  let stop_timer ~timer { running_timers; results; } =
    match SMap.get timer running_timers with
    | None -> { running_timers; results; }
    | Some running_timer ->
        let wall_end = Unix.gettimeofday () in
        let (user_end, system_end) = times () in
        let (worker_user_end, worker_system_end) = worker_times () in
        let result = {
          user = {
            start_age= running_timer.user_start;
            duration= user_end -. running_timer.user_start;
          };
          system = {
            start_age= running_timer.system_start;
            duration= system_end -. running_timer.system_start;
          };
          worker_user = {
            start_age= running_timer.worker_user_start;
            duration= worker_user_end -. running_timer.worker_user_start;
          };
          worker_system = {
            start_age= running_timer.worker_system_start;
            duration= worker_system_end -. running_timer.worker_system_start;
          };
          wall = {
            start_age = running_timer.wall_start -. flow_start_time;
            duration = wall_end -. running_timer.wall_start;
          }
        } in
        {
          running_timers = SMap.remove timer running_timers;
          results = SMap.add timer result results;
        }

  let get_finished_timer ~timer { results; _; } =
    match SMap.get timer results with
    | None -> None
    | Some { wall = { start_age; duration; }; _; } ->
        Some (start_age, duration)

  let json_of_time_measurement { start_age; duration; } =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "start_age", JSON_Number (string_of_float_trunc start_age);
      "duration", JSON_Number (string_of_float_trunc duration);
    ]

  let json_of_result { wall; user; system; worker_user; worker_system; } =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "wall", json_of_time_measurement wall;
      "user", json_of_time_measurement user;
      "system", json_of_time_measurement system;
      "worker_user", json_of_time_measurement worker_user;
      "worker_system", json_of_time_measurement worker_system;
      (* legacy fields *)
      "start_wall_age", JSON_Number (string_of_float_trunc wall.start_age);
      "wall_duration", JSON_Number (string_of_float_trunc wall.duration);
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
