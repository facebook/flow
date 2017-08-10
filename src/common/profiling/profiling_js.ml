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
   val get_finished_timer: timer:string -> t -> (float * float * float * float) option
   val to_json: t -> Hh_json.json
 end = struct
  type time_measurement = {
    start_age: float;
    duration: float;
  }

  type processor_info = {
    cpu_user: float;
    cpu_nice_user: float;
    cpu_system: float;
    cpu_idle: float;
    cpu_usage: float;
  }

  type result = {
    user: time_measurement;
    system: time_measurement;
    worker_user: time_measurement;
    worker_system: time_measurement;
    wall: time_measurement;
    processor_totals: processor_info;
    flow_cpu_usage: float;
  }

  type running_timer = {
    user_start: float;
    system_start: float;
    worker_user_start: float;
    worker_system_start: float;
    wall_start: float;
    processor_info_start: Sys_utils.processor_info;
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
    let processor_info_start = Sys_utils.processor_info () in
    let running_timer = {
      user_start;
      system_start;
      worker_user_start;
      worker_system_start;
      wall_start;
      processor_info_start;
    } in
    {
      running_timers = SMap.add timer running_timer running_timers;
      results;
    }

  let flow_start_time = Unix.gettimeofday ()

  let make_processor_info start end_ =
    let cpu_user = end_.Sys_utils.cpu_user -. start.Sys_utils.cpu_user in
    let cpu_nice_user = end_.Sys_utils.cpu_nice_user -. start.Sys_utils.cpu_nice_user in
    let cpu_system = end_.Sys_utils.cpu_system -. start.Sys_utils.cpu_system in
    let cpu_idle = end_.Sys_utils.cpu_idle -. start.Sys_utils.cpu_idle in
    let cpu_busy = cpu_user +. cpu_nice_user +. cpu_system in
    let cpu_usage = if cpu_busy = 0.
      then 0.
      else cpu_busy /. (cpu_busy +. cpu_idle) in
    { cpu_user; cpu_nice_user; cpu_system; cpu_idle; cpu_usage }

  let stop_timer ~timer { running_timers; results; } =
    match SMap.get timer running_timers with
    | None -> { running_timers; results; }
    | Some running_timer ->
        let wall_end = Unix.gettimeofday () in
        let (user_end, system_end) = times () in
        let (worker_user_end, worker_system_end) = worker_times () in
        let processor_info_end = Sys_utils.processor_info () in

        let user = {
          start_age = running_timer.user_start;
          duration = user_end -. running_timer.user_start;
        } in
        let system = {
          start_age = running_timer.system_start;
          duration = system_end -. running_timer.system_start;
        } in
        let worker_user = {
          start_age = running_timer.worker_user_start;
          duration = worker_user_end -. running_timer.worker_user_start;
        } in
        let worker_system = {
          start_age = running_timer.worker_system_start;
          duration = worker_system_end -. running_timer.worker_system_start;
        } in
        let wall = {
          start_age = running_timer.wall_start -. flow_start_time;
          duration = wall_end -. running_timer.wall_start;
        } in
        let processor_totals = make_processor_info
          running_timer.processor_info_start.Sys_utils.proc_totals
          processor_info_end.Sys_utils.proc_totals in

        let flow_cpu_time =
          user.duration +. system.duration +. worker_user.duration +. worker_system.duration in
        let total_cpu_time =
          processor_totals.cpu_user +. processor_totals.cpu_nice_user +.
          processor_totals.cpu_system +. processor_totals.cpu_idle in

        (* flow_cpu_time and total_cpu_time are calculated using slightly different systems.
         * flow_cpu_time should always be less than total_cpu_time, so we could in theory just
         * check the numerator. However, checking the denominator is a slightly safer way to avoid
         * a division by zero *)
        let flow_cpu_usage = if total_cpu_time = 0. then 0. else flow_cpu_time /. total_cpu_time in

        let result = {
          user; system; worker_user; worker_system; wall; processor_totals; flow_cpu_usage;
        } in
        {
          running_timers = SMap.remove timer running_timers;
          results = SMap.add timer result results;
        }

  let get_finished_timer ~timer { results; _; } =
    match SMap.get timer results with
    | None -> None
    | Some {
      wall = { start_age; duration; };
      processor_totals = { cpu_usage; _; };
      flow_cpu_usage;
      _;
    } ->
        Some (start_age, duration, cpu_usage, flow_cpu_usage)

  let json_of_time_measurement { start_age; duration; } =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "start_age", JSON_Number (string_of_float_trunc start_age);
      "duration", JSON_Number (string_of_float_trunc duration);
    ]

  let json_of_processor_info info =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "user", JSON_Number (string_of_float_trunc info.cpu_user);
      "nice", JSON_Number (string_of_float_trunc info.cpu_nice_user);
      "system", JSON_Number (string_of_float_trunc info.cpu_system);
      "idle", JSON_Number (string_of_float_trunc info.cpu_idle);
      "usage", JSON_Number (string_of_float_trunc info.cpu_usage);
    ]

  let json_of_result
    { wall; user; system; worker_user; worker_system; processor_totals; flow_cpu_usage; } =
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
      "processor_totals", json_of_processor_info processor_totals;
      "flow_cpu_usage", JSON_Number (string_of_float_trunc flow_cpu_usage)
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

type finished = {
  timing: Timing.t;
  memory: Memory.t;
}
type running = finished ref

let empty = {
  timing = Timing.empty;
  memory = Memory.empty;
}

let start_timer ~timer profile =
  profile := { !profile with timing = Timing.start_timer ~timer !profile.timing }

let stop_timer ~timer profile =
  profile := { !profile with timing = Timing.stop_timer ~timer !profile.timing }

let profiling_timer_name = "Profiling"
let reserved_timer_names = SSet.of_list [ profiling_timer_name ]

let with_profiling f =
  let profiling = ref empty in
  start_timer ~timer:profiling_timer_name profiling;
  let ret = f profiling in
  stop_timer ~timer:profiling_timer_name profiling;
  (!profiling, ret)

let check_for_reserved_timer_name f ~timer profile =
  if SSet.mem timer reserved_timer_names
  then failwith (Printf.sprintf "%s is a reserved timer name" timer);
  f ~timer profile

let start_timer = check_for_reserved_timer_name start_timer
let stop_timer = check_for_reserved_timer_name stop_timer

let get_finished_timer ~timer profile =
  Timing.get_finished_timer ~timer !profile.timing

let sample_memory ~metric ~value profile =
  profile := {!profile with memory = Memory.sample_memory ~metric ~value !profile.memory }

let to_json_properties profile =
  [
    "timing", Timing.to_json profile.timing;
    "memory", Memory.to_json profile.memory;
  ]

let get_timing_json_string profile =
  Timing.to_json profile.timing |> Hh_json.json_to_string

let get_memory_json_string profile =
  Memory.to_json profile.memory |> Hh_json.json_to_string
