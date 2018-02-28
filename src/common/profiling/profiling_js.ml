(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module Timing : sig
  type t
  val empty: t
  val start_timer: timer:string -> t -> t
  val stop_timer: timer:string -> t -> t
  val push_timer_prefix: prefix:string -> t -> t
  val pop_timer_prefix: t -> t
  val get_results: t -> result SMap.t
  val to_json: abridged:bool -> t -> Hh_json.json
end = struct

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
    timer_prefix: string list;
  }

  let empty = { running_timers = SMap.empty; results = SMap.empty; timer_prefix = [] }

  let with_prefix prefix timer =
    match prefix with
    | [] -> timer
    | prefix::_ -> prefix ^ timer

  let push_timer_prefix ~prefix timing =
    { timing with timer_prefix = prefix::timing.timer_prefix }

  let pop_timer_prefix timing =
    match timing.timer_prefix with
    | [] -> timing
    | _::rest -> { timing with timer_prefix = rest }

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

  let start_timer ~timer { running_timers; results; timer_prefix } =
    let timer = with_prefix timer_prefix timer in
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
      timer_prefix;
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

  let stop_timer ~timer { running_timers; results; timer_prefix } =
    let timer = with_prefix timer_prefix timer in
    match SMap.get timer running_timers with
    | None -> { running_timers; results; timer_prefix }
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
          timer_prefix
        }

  let get_results { results; _; } = results

  let combine_time_measurements = List.fold_left
    (fun acc t ->
      { start_age = acc.start_age +. t.start_age; duration = acc.duration +. t.duration }
    )
    { start_age = 0.0; duration = 0.0 }

  let json_of_time_measurement { start_age; duration; } =
    let open Hh_json in
    let open Utils_js in
    JSON_Object [
      "start_age", JSON_Number (string_of_float_trunc start_age);
      "duration", JSON_Number (string_of_float_trunc duration);
    ]

  let json_of_processor_info ~abridged info =
    let open Hh_json in
    let open Utils_js in
    if abridged
    then
      let total =  info.cpu_user +. info.cpu_nice_user +. info.cpu_system +. info.cpu_idle in
      (* We can infer enough from these two numbers
       * busy = total * usage
       * idle = total - busy *)
      JSON_Object [
        "total", JSON_Number (string_of_float_trunc total);
        "usage", JSON_Number (string_of_float_trunc info.cpu_usage);
      ]
    else
      JSON_Object [
        "user", JSON_Number (string_of_float_trunc info.cpu_user);
        "nice", JSON_Number (string_of_float_trunc info.cpu_nice_user);
        "system", JSON_Number (string_of_float_trunc info.cpu_system);
        "idle", JSON_Number (string_of_float_trunc info.cpu_idle);
        "usage", JSON_Number (string_of_float_trunc info.cpu_usage);
      ]

  let json_of_result ~abridged
    { wall; user; system; worker_user; worker_system; processor_totals; flow_cpu_usage; } =
    let open Hh_json in
    let open Utils_js in
    let cpu = [user; system; worker_user; worker_system] in
    let common_fields = [
      "wall", json_of_time_measurement wall;
      "cpu", json_of_time_measurement (combine_time_measurements cpu);
      "flow_cpu_usage", JSON_Number (string_of_float_trunc flow_cpu_usage);
      "processor_totals", json_of_processor_info ~abridged processor_totals;
    ] in
    let fields =
      if abridged
      then common_fields
      else common_fields @ [
        "wall", json_of_time_measurement wall;
        "user", json_of_time_measurement user;
        "system", json_of_time_measurement system;
        "worker_user", json_of_time_measurement worker_user;
        "worker_system", json_of_time_measurement worker_system;
        (* legacy fields *)
        "start_wall_age", JSON_Number (string_of_float_trunc wall.start_age);
        "wall_duration", JSON_Number (string_of_float_trunc wall.duration);
      ]
    in JSON_Object fields

  let to_json ~abridged { results; _; } =
    let open Hh_json in
    let results = results
    |> SMap.map (json_of_result ~abridged)
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

let with_timer_prefix_lwt ~prefix ~f profile =
  profile := { !profile with timing = Timing.push_timer_prefix ~prefix !profile.timing };
  Lwt.finalize
    f
    (fun () ->
      profile := { !profile with timing = Timing.pop_timer_prefix !profile.timing };
      Lwt.return_unit
    )

let start_timer ~timer profile =
  profile := { !profile with timing = Timing.start_timer ~timer !profile.timing }

let stop_timer ~timer profile =
  profile := { !profile with timing = Timing.stop_timer ~timer !profile.timing }

let profiling_timer_name = "Profiling"
let reserved_timer_names = SSet.of_list [ profiling_timer_name ]

(* Prints out a nice table of all the timers for a profiling run. It might look like this:
 *
 *  WALL TIME                CPU TIME            SECTION
 * --------------------------------------------------------
 * 0.620 (100.0%)          2.454 (100.0%)        <Total>
 * 0.036 (  5.9%)          0.154 (  6.3%)        Parsing
 * 0.022 (  3.6%)          0.003 (  0.1%)        <Unknown>
 * 0.023 (  3.7%)          0.001 (  0.0%)        CommitModules
 * 0.023 (  3.7%)          0.012 (  0.5%)        ResolveRequires
 * 0.028 (  4.5%)          0.047 (  1.9%)        DependentFiles
 * 0.019 (  3.0%)          0.013 (  0.5%)        <Unknown>
 * 0.012 (  1.9%)          0.003 (  0.1%)        RecalcDepGraph
 * 0.017 (  2.7%)          0.038 (  1.5%)        CalcDepsTypecheck
 * 0.000 (  0.0%)          0.001 (  0.0%)        Infer
 * 0.001 (  0.2%)          0.001 (  0.0%)        MakeMergeInput
 * 0.017 (  2.7%)          0.009 (  0.4%)        CalcDeps
 * 0.407 ( 65.7%)          2.157 ( 87.9%)        Merge
 * 0.056 (  9.0%)          0.031 (  1.3%)        <Unknown total>
 *
 * For each profiled section, it prints out the wall time and cpu time, including the percentage of
 * the total profiled time. The sections are printed in the order that they were run.
 *
 * The <Unknown> sections appear when some unprofiled code takes up more than 1% of wall time. The
 * <Unknown total> is the sum of all unprofiled time.
 *)
let print_summary =
  (* Total cpu duration *)
  let sum_cpu result =
    result.user.duration +.
    result.system.duration +.
    result.worker_user.duration +.
    result.worker_system.duration
  in

  (* Total cpu start age *)
  let sum_cpu_start_age result =
    result.user.start_age +.
    result.system.start_age +.
    result.worker_user.start_age +.
    result.worker_system.start_age
  in

  (* Prints a single row of the table. All but the last column have a fixed width. *)
  let print_summary_single_raw key (result_wall, result_cpu) total =
    Printf.eprintf "%7.3f (%5.1f%%)\t%7.3f (%5.1f%%)\t%s\n%!"
      result_wall
      (100.0 *. result_wall /. total.wall.duration)
      result_cpu
      (100.0 *. result_cpu /. (sum_cpu total))
      key
  in

  let print_summary_single key result total =
    print_summary_single_raw key (result.wall.duration, sum_cpu result) total
  in

  (* If there's more than 1% of wall time since the last end and the next start_age, then print an
   * <Unknown> row *)
  let print_unknown last_end (wall_start_age, cpu_start_age) total =
    match last_end with
    | None -> ()
    | Some (wall_end, cpu_end) ->
      let unknown_wall = wall_start_age -. wall_end in
      if unknown_wall /. total.wall.duration > 0.01
      then
        let unknown_cpu = cpu_start_age -. cpu_end in
        print_summary_single_raw "<Unknown>" (unknown_wall, unknown_cpu) total
  in

  fun profile ->
    let results = Timing.get_results profile.timing in
    let total, parts = SMap.fold (fun key result (total, parts) ->
      if key = profiling_timer_name
      then (Some result), parts
      else total, ((key, result)::parts)
    ) results (None, []) in
    let total = match total with
    | None -> failwith (Printf.sprintf "Couldn't find timing results for '%s'" profiling_timer_name)
    | Some total -> total in
    let parts =
      List.sort (fun (_, r1) (_, r2) -> compare r1.wall.start_age r2.wall.start_age) parts in

    (* Print the header *)
    Printf.eprintf "   WALL TIME\t\t    CPU TIME\t\tSECTION\n%!";
    Printf.eprintf "--------------------------------------------------------\n%!";

    (* Print the total time *)
    print_summary_single "<Total>" total total;

    (* Print the various sections and the unknown durations *)
    let last_end, remaining = List.fold_left
      (fun (last_end, (wall_remaining, cpu_remaining)) (key, result) ->
        (* Print an <Unknown> row if needed *)
        print_unknown last_end (result.wall.start_age, sum_cpu_start_age result) total;

        (* Print this row *)
        print_summary_single key result total;

        let remaining = wall_remaining -. result.wall.duration, cpu_remaining -. (sum_cpu result) in
        let last_end =
          result.wall.start_age +. result.wall.duration,
          (sum_cpu_start_age result) +. (sum_cpu result) in
        Some last_end, remaining
      )
      (None, (total.wall.duration, sum_cpu total))
      parts in

    (* Print an <Unknown> row if there's too much time between the last section and the end of the
     * profiling *)
    print_unknown
      last_end
      (total.wall.start_age +. total.wall.duration, sum_cpu_start_age total +. sum_cpu total)
      total;

    (* Print the unknown totals *)
    print_summary_single_raw "<Unknown total>" remaining total

let with_profiling_lwt ~should_print_summary f =
  let profiling = ref empty in
  start_timer ~timer:profiling_timer_name profiling;
  let%lwt ret = (f profiling) [%lwt.finally
    stop_timer ~timer:profiling_timer_name profiling;
    if should_print_summary then print_summary !profiling;
    Lwt.return_unit
  ] in
  Lwt.return (!profiling, ret)

let check_for_reserved_timer_name f ~timer profile =
  if SSet.mem timer reserved_timer_names
  then failwith (Printf.sprintf "%s is a reserved timer name" timer);
  f ~timer profile

let start_timer = check_for_reserved_timer_name start_timer
let stop_timer = check_for_reserved_timer_name stop_timer
let with_timer_lwt ~timer ~f profiling =
  start_timer ~timer profiling;
  Lwt.finalize f (fun () -> stop_timer ~timer profiling; Lwt.return_unit)

let get_finished_timer ~timer profile =
  let results = Timing.get_results !profile.timing in
  match SMap.get timer results with
  | None -> None
  | Some r ->
    Some (r.wall.start_age, r.wall.duration, r.processor_totals.cpu_usage, r.flow_cpu_usage)

let sample_memory ~metric ~value profile =
  profile := {!profile with memory = Memory.sample_memory ~metric ~value !profile.memory }

let to_json_properties profile =
  [
    "timing", Timing.to_json ~abridged:false profile.timing;
    "memory", Memory.to_json profile.memory;
  ]

let get_timing_json_string profile =
  Timing.to_json ~abridged:false profile.timing |> Hh_json.json_to_string

let get_abridged_timing_json_string profile =
  Timing.to_json ~abridged:true profile.timing |> Hh_json.json_to_string

let get_memory_json_string profile =
  Memory.to_json profile.memory |> Hh_json.json_to_string
