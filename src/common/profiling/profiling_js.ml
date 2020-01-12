(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This module is used to time stuff. And not stuff that runs a million times. Stuff that runs
 * maybe once or twice.
 *
 * These timers are hierarchical. So you can start a timer while another timer is still running.
 *
 *  Ok:
 *          A----------------------------
 *            B1------------    B2------
 *                C----
 *
 * Not ok (and impossible by construction)
 *         A-----------------------------
 *                    B--------------------------
 *
 *)
module Timing : sig
  type running

  type finished

  val with_timing_lwt : label:string -> f:(running -> 'a Lwt.t) -> (finished * 'a) Lwt.t

  val with_timer_lwt :
    ?should_print:bool -> timer:string -> f:(unit -> 'a Lwt.t) -> running -> 'a Lwt.t

  val get_total_wall_duration : finished -> float

  val to_json : abridged:bool -> finished -> Hh_json.json

  val to_json_legacy : abridged:bool -> finished -> Hh_json.json

  val print_summary_timing_table : finished -> unit

  val merge : from:finished -> into:running -> unit
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

  type worker_wall_times = {
    worker_idle: time_measurement;
    worker_read_request: time_measurement;
    worker_run: time_measurement;
    worker_send_response: time_measurement;
    worker_done: time_measurement;
    worker_gc_minor: time_measurement;
    worker_gc_major: time_measurement;
  }

  type result = {
    timer_name: string;
    user: time_measurement;
    system: time_measurement;
    worker_user: time_measurement;
    worker_system: time_measurement;
    worker_wall_times: worker_wall_times;
    wall: time_measurement;
    processor_totals: processor_info;
    flow_cpu_usage: float;
    sub_results: result list;
    sample_count: int; (* If we merge a sample with 2 duplicates, this will be 3 *)
  }

  type worker_wall_start_times = {
    worker_idle_start: float;
    worker_read_request_start: float;
    worker_run_start: float;
    worker_send_response_start: float;
    worker_done_start: float;
    worker_gc_minor_start: float;
    worker_gc_major_start: float;
  }

  type running_timer = {
    timer: string;
    user_start: float;
    system_start: float;
    worker_user_start: float;
    worker_system_start: float;
    worker_wall_start_times: worker_wall_start_times;
    wall_start: float;
    processor_info_start: Sys_utils.processor_info;
    mutable sub_results_rev: result list;
  }

  type running = running_timer ref (* The current running parent *)

  type finished = result

  (* Returns the user cpu and system cpu times *)
  let times () =
    Unix.(
      let tm = times () in
      (* Warning - cutime and cstime (children times) don't work on Windows *)
      (tm.tms_utime +. tm.tms_cutime, tm.tms_stime +. tm.tms_cstime))

  let worker_times () =
    let worker_user_time =
      match Measure.get_sum "worker_user_time" with
      | None -> 0.0
      | Some time -> time
    in
    let worker_system_time =
      match Measure.get_sum "worker_system_time" with
      | None -> 0.0
      | Some time -> time
    in
    (worker_user_time, worker_system_time)

  let time_measurement start end_ = { start_age = start; duration = end_ -. start }

  let (worker_wall_start_times, worker_wall_times) =
    let get_run () = Option.value ~default:0.0 (Measure.get_sum "worker_wall_time") in
    let get_read () = Option.value ~default:0.0 (Measure.get_sum "worker_read_request") in
    let get_send () = Option.value ~default:0.0 (Measure.get_sum "worker_send_response") in
    let get_idle () = Option.value ~default:0.0 (Measure.get_sum "worker_idle") in
    let get_done () = Option.value ~default:0.0 (Measure.get_sum "worker_done") in
    let get_gc_minor () = Option.value ~default:0.0 (Measure.get_sum "worker_gc_minor_wall_time") in
    let get_gc_major () = Option.value ~default:0.0 (Measure.get_sum "worker_gc_major_wall_time") in
    let worker_wall_start_times () =
      {
        worker_idle_start = get_idle ();
        worker_read_request_start = get_read ();
        worker_run_start = get_run ();
        worker_send_response_start = get_send ();
        worker_done_start = get_done ();
        worker_gc_minor_start = get_gc_minor ();
        worker_gc_major_start = get_gc_major ();
      }
    in
    let worker_wall_times start =
      {
        worker_idle = time_measurement start.worker_idle_start (get_idle ());
        worker_read_request = time_measurement start.worker_read_request_start (get_read ());
        worker_run = time_measurement start.worker_run_start (get_run ());
        worker_send_response = time_measurement start.worker_send_response_start (get_send ());
        worker_done = time_measurement start.worker_done_start (get_done ());
        worker_gc_minor = time_measurement start.worker_gc_minor_start (get_gc_minor ());
        worker_gc_major = time_measurement start.worker_gc_major_start (get_gc_major ());
      }
    in
    (worker_wall_start_times, worker_wall_times)

  let legacy_top_timer_name = "Profiling"

  let start_timer ~timer =
    let wall_start = Unix.gettimeofday () in
    let (user_start, system_start) = times () in
    let (worker_user_start, worker_system_start) = worker_times () in
    let worker_wall_start_times = worker_wall_start_times () in
    let processor_info_start = Sys_utils.processor_info () in
    {
      timer;
      user_start;
      system_start;
      worker_user_start;
      worker_system_start;
      worker_wall_start_times;
      wall_start;
      processor_info_start;
      sub_results_rev = [];
    }

  let flow_start_time = Unix.gettimeofday ()

  let make_processor_info start end_ =
    let cpu_user = end_.Sys_utils.cpu_user -. start.Sys_utils.cpu_user in
    let cpu_nice_user = end_.Sys_utils.cpu_nice_user -. start.Sys_utils.cpu_nice_user in
    let cpu_system = end_.Sys_utils.cpu_system -. start.Sys_utils.cpu_system in
    let cpu_idle = end_.Sys_utils.cpu_idle -. start.Sys_utils.cpu_idle in
    let cpu_busy = cpu_user +. cpu_nice_user +. cpu_system in
    let cpu_usage =
      if cpu_busy = 0. then
        0.
      else
        cpu_busy /. (cpu_busy +. cpu_idle)
    in
    { cpu_user; cpu_nice_user; cpu_system; cpu_idle; cpu_usage }

  let stop_timer running_timer =
    let wall_end = Unix.gettimeofday () in
    let (user_end, system_end) = times () in
    let (worker_user_end, worker_system_end) = worker_times () in
    let processor_info_end = Sys_utils.processor_info () in
    let user =
      { start_age = running_timer.user_start; duration = user_end -. running_timer.user_start }
    in
    let system =
      {
        start_age = running_timer.system_start;
        duration = system_end -. running_timer.system_start;
      }
    in
    let worker_user =
      {
        start_age = running_timer.worker_user_start;
        duration = worker_user_end -. running_timer.worker_user_start;
      }
    in
    let worker_system =
      {
        start_age = running_timer.worker_system_start;
        duration = worker_system_end -. running_timer.worker_system_start;
      }
    in
    let worker_wall_times = worker_wall_times running_timer.worker_wall_start_times in
    let wall =
      {
        start_age = running_timer.wall_start -. flow_start_time;
        duration = wall_end -. running_timer.wall_start;
      }
    in
    let processor_totals =
      make_processor_info
        running_timer.processor_info_start.Sys_utils.proc_totals
        processor_info_end.Sys_utils.proc_totals
    in
    let flow_cpu_time =
      user.duration +. system.duration +. worker_user.duration +. worker_system.duration
    in
    let total_cpu_time =
      processor_totals.cpu_user
      +. processor_totals.cpu_nice_user
      +. processor_totals.cpu_system
      +. processor_totals.cpu_idle
    in
    (* flow_cpu_time and total_cpu_time are calculated using slightly different systems.
     * flow_cpu_time should always be less than total_cpu_time, so we could in theory just
     * check the numerator. However, checking the denominator is a slightly safer way to avoid
     * a division by zero *)
    let flow_cpu_usage =
      if total_cpu_time = 0. then
        0.
      else
        flow_cpu_time /. total_cpu_time
    in
    {
      timer_name = running_timer.timer;
      user;
      system;
      worker_user;
      worker_system;
      worker_wall_times;
      wall;
      processor_totals;
      flow_cpu_usage;
      sub_results = List.rev running_timer.sub_results_rev;
      sample_count = 1;
    }

  let with_timing_lwt ~label ~f =
    let total_timer = start_timer ~timer:label in
    let running = ref total_timer in
    (* Why don't we wrap this in a finalize? Well, if f throws, then no one will ever read our
     * finished timer, so we don't really need to stop it *)
    let%lwt ret = f running in
    let finished_timer = stop_timer total_timer in
    Lwt.return (finished_timer, ret)

  let with_timer_lwt ?(should_print = false) ~timer ~f running =
    let parent_timer = !running in
    let running_timer = start_timer ~timer in
    running := running_timer;
    Lwt.finalize f (fun () ->
        let finished_timer = stop_timer running_timer in
        parent_timer.sub_results_rev <- finished_timer :: parent_timer.sub_results_rev;

        running := parent_timer;

        ( if should_print then
          let stats =
            Printf.sprintf
              "start_wall_age: %f; wall_duration: %f; cpu_usage: %f; flow_cpu_usage: %f"
              finished_timer.wall.start_age
              finished_timer.wall.duration
              finished_timer.processor_totals.cpu_usage
              finished_timer.flow_cpu_usage
          in
          Hh_logger.info "TimingEvent `%s`: %s" timer stats );

        Lwt.return_unit)

  let get_total_wall_duration finished_timer = finished_timer.wall.duration

  let combine_time_measurements =
    List.fold_left
      (fun acc t ->
        { start_age = acc.start_age +. t.start_age; duration = acc.duration +. t.duration })
      { start_age = 0.0; duration = 0.0 }

  let json_of_time_measurement { start_age; duration } =
    Hh_json.(
      JSON_Object
        [
          ("start_age", JSON_Number (Dtoa.ecma_string_of_float start_age));
          ("duration", JSON_Number (Dtoa.ecma_string_of_float duration));
        ])

  let total_cpu_time info = info.cpu_user +. info.cpu_nice_user +. info.cpu_system +. info.cpu_idle

  let json_of_processor_info ~abridged info =
    Hh_json.(
      if abridged then
        let total = total_cpu_time info in
        (* We can infer enough from these two numbers
         * busy = total * usage
         * idle = total - busy *)
        JSON_Object
          [
            ("total", JSON_Number (Dtoa.ecma_string_of_float total));
            ("usage", JSON_Number (Dtoa.ecma_string_of_float info.cpu_usage));
          ]
      else
        JSON_Object
          [
            ("user", JSON_Number (Dtoa.ecma_string_of_float info.cpu_user));
            ("nice", JSON_Number (Dtoa.ecma_string_of_float info.cpu_nice_user));
            ("system", JSON_Number (Dtoa.ecma_string_of_float info.cpu_system));
            ("idle", JSON_Number (Dtoa.ecma_string_of_float info.cpu_idle));
            ("usage", JSON_Number (Dtoa.ecma_string_of_float info.cpu_usage));
          ])

  (* This function solves the problem of having multiple sibling timers (timers with the same
   * parent) with the same name. Our JSON representation is an object keyed by the name of the
   * timer, so we need to merge any two timers with the same name *)
  let merge_dupes =
    let merge_time_measurement a b =
      { start_age = a.start_age; duration = a.duration +. b.duration }
    in
    let merge_worker_wall_times a b =
      {
        worker_idle = merge_time_measurement a.worker_idle b.worker_idle;
        worker_read_request = merge_time_measurement a.worker_read_request b.worker_read_request;
        worker_run = merge_time_measurement a.worker_run b.worker_run;
        worker_send_response = merge_time_measurement a.worker_send_response b.worker_send_response;
        worker_done = merge_time_measurement a.worker_done b.worker_done;
        worker_gc_minor = merge_time_measurement a.worker_gc_minor b.worker_gc_minor;
        worker_gc_major = merge_time_measurement a.worker_gc_major b.worker_gc_major;
      }
    in
    let weighted_average values =
      let (weight_sum, acc) =
        List.fold_left
          (fun (weight_sum, acc) (weight, value) ->
            assert (weight >= 0.);
            (weight_sum +. weight, acc +. (weight *. value)))
          (0., 0.)
          values
      in
      if weight_sum > 0. then
        acc /. weight_sum
      else
        0.
    in
    let merge_processor_totals a b =
      {
        cpu_user = a.cpu_user +. b.cpu_user;
        cpu_nice_user = a.cpu_nice_user +. b.cpu_nice_user;
        cpu_system = a.cpu_system +. b.cpu_system;
        cpu_idle = a.cpu_idle +. b.cpu_idle;
        cpu_usage =
          weighted_average [(total_cpu_time a, a.cpu_usage); (total_cpu_time b, b.cpu_usage)];
      }
    in
    fun ~dupes result ->
      List.fold_left
        (fun result dupe ->
          {
            timer_name = result.timer_name;
            wall = merge_time_measurement result.wall dupe.wall;
            user = merge_time_measurement result.user dupe.user;
            system = merge_time_measurement result.system dupe.system;
            worker_user = merge_time_measurement result.worker_user dupe.worker_user;
            worker_system = merge_time_measurement result.worker_system dupe.worker_system;
            worker_wall_times =
              merge_worker_wall_times result.worker_wall_times dupe.worker_wall_times;
            processor_totals = merge_processor_totals result.processor_totals dupe.processor_totals;
            flow_cpu_usage =
              weighted_average
                [
                  (total_cpu_time result.processor_totals, result.flow_cpu_usage);
                  (total_cpu_time dupe.processor_totals, dupe.flow_cpu_usage);
                ];
            sub_results = result.sub_results @ dupe.sub_results;
            sample_count = result.sample_count + 1;
          })
        result
        dupes

  let rec json_of_result ~abridged ~max_depth ~dupes result =
    let {
      timer_name;
      wall;
      user;
      system;
      worker_user;
      worker_system;
      worker_wall_times;
      processor_totals;
      flow_cpu_usage;
      sub_results;
      sample_count;
    } =
      merge_dupes ~dupes result
    in
    Hh_json.(
      let cpu = [user; system; worker_user; worker_system] in
      let common_fields =
        [
          ("wall", json_of_time_measurement wall);
          ("cpu", json_of_time_measurement (combine_time_measurements cpu));
          ("flow_cpu_usage", JSON_Number (Dtoa.ecma_string_of_float flow_cpu_usage));
          ("processor_totals", json_of_processor_info ~abridged processor_totals);
        ]
      in
      let fields =
        if abridged then
          if sample_count > 1 then
            ("samples", JSON_Number (string_of_int sample_count)) :: common_fields
          else
            common_fields
        else
          let sub_results =
            if max_depth > 0 then
              json_of_results ~abridged ~max_depth:(max_depth - 1) sub_results
            else
              JSON_Object []
          in
          common_fields
          @ [
              ("wall", json_of_time_measurement wall);
              ("user", json_of_time_measurement user);
              ("system", json_of_time_measurement system);
              ("worker_user", json_of_time_measurement worker_user);
              ("worker_system", json_of_time_measurement worker_system);
              ( "worker_wall_times",
                JSON_Object
                  [
                    ("run", json_of_time_measurement worker_wall_times.worker_run);
                    ("read", json_of_time_measurement worker_wall_times.worker_read_request);
                    ("send", json_of_time_measurement worker_wall_times.worker_send_response);
                    ("idle", json_of_time_measurement worker_wall_times.worker_idle);
                    ("done", json_of_time_measurement worker_wall_times.worker_done);
                    ("gc_minor", json_of_time_measurement worker_wall_times.worker_gc_minor);
                    ("gc_major", json_of_time_measurement worker_wall_times.worker_gc_major);
                  ] );
              ("sub_results", sub_results);
              ("samples", JSON_Number (string_of_int sample_count));
            ]
      in
      (timer_name, JSON_Object fields))

  (* This will return a JSON object which is a map from the timer name to the timer's results. This
   * makes it easy for tools like Scuba to query for timing.results.Parsing.wall.duration or
   * something like that.
   *
   * But what happens if we ran 2 or more timers with the name "Parsing"? This is actually a very
   * reasonable thing to do. Imagine some code which needs to call typecheck_contents on 2 or more
   * files (get_def will do this when following a chain). We will get a "Parsing" timer for each
   * typecheck_contents.
   *
   * I see a few relatively reasonable options here:
   * 1) Only report the first one. Since duplicates are an edge case, this is pretty reasonable
   * 2) Merge them together somehow. A little less obvious but avoids throwing away the data.
   *
   * I'm going with #2. And I'm basically going to merge together the durations, keep the first
   * start time, and report how many timers there were.
   *)
  and json_of_results ~abridged ~max_depth results =
    let (results_rev, dupes) =
      List.fold_left
        (fun (results, dupes) result ->
          match SMap.find_opt result.timer_name dupes with
          | None -> (result :: results, SMap.add result.timer_name [] dupes)
          | Some prev_dupes -> (results, SMap.add result.timer_name (result :: prev_dupes) dupes))
        ([], SMap.empty)
        results
    in
    let json_results =
      List.fold_left
        (fun acc result ->
          let json_result =
            json_of_result ~abridged ~max_depth ~dupes:(SMap.find result.timer_name dupes) result
          in
          json_result :: acc)
        []
        results_rev
    in
    Hh_json.JSON_Object json_results

  (* There are two concerns here:
   *
   * 1. When abridged is set, we don't want to log too much data
   * 2. We don't want to make breaking changes without first updating our consumers (which I think
   *    are currently just Scuba and ServiceLab)
   *
   * So here's the plan:
   *
   * A) When abridged is set, only output the first 3 levels of the hierarchy. That should give us
   *    totals, each timer, and each sub timer.
   * B) The legacy graphs and profiling assumes two main things:
   *    1) A flat object with all the timers. So we need to flatten out the results
   *    2) The "totals" to be in a timer named "Profiling".
   *)
  let to_json ~abridged result =
    let max_depth =
      if abridged then
        1
      else
        100
    in
    json_of_results ~abridged ~max_depth [result]

  let to_json_legacy ~abridged result =
    (* If we have the hierarchy
     * <Total>
     *   Foo
     *   Bar
     *     BazOne
     *     BazTwo
     *   Qux
     *
     * We flatten it to
     *
     * Profiling, Foo, Bar, Bar:BazOne, Bar:BazTwo, Qux
     *)
    let results_rev =
      List.fold_left
        (fun acc sub_result ->
          let prefix = sub_result.timer_name ^ ":" in
          List.fold_left
            (fun acc sub_sub_result ->
              { sub_sub_result with timer_name = prefix ^ sub_sub_result.timer_name } :: acc)
            (sub_result :: acc)
            sub_result.sub_results)
        [{ result with timer_name = legacy_top_timer_name }]
        result.sub_results
    in
    let results = json_of_results ~abridged ~max_depth:0 (List.rev results_rev) in
    Hh_json.JSON_Object [("results", results)]

  (* Prints out a nice table of all the timers for a profiling run. It might look like this:
   *
   *   ======================Init Timings=====================
   *      WALL TIME            CPU TIME        SECTION
   *   -------------------------------------------------------
   *    31.652 (100.0%)    378.263 (100.0%)    <Total>
   *     4.225 ( 13.3%)     31.738 (  8.4%)      Parsing
   *     0.001 (  0.0%)      0.001 (  0.0%)      PackageHeap
   *     1.013 (  3.2%)      1.012 (  0.3%)      InitLibs
   *     0.824 (  2.6%)      3.949 (  1.0%)      CommitModules
   *     0.342 (  1.1%)      3.383 (  0.9%)      ResolveRequires
   *     0.223 (  0.7%)      0.703 (  0.2%)      CalcDepsTypecheck
   *     0.223 (  0.7%)      0.223 (  0.1%)      FilesToInfer
   *     0.191 (  0.6%)      0.191 (  0.1%)      PruneDeps
   *     0.128 (  0.4%)      0.126 (  0.0%)      CalcDeps
   *    24.189 ( 76.4%)    336.643 ( 89.0%)      Merge
   *     0.034 (  0.1%)      0.034 (  0.0%)      PrintGCStats
   *     0.260 (  0.8%)      0.259 (  0.1%)    <Unknown total>
   *
   *
   * For each profiled section, it prints out the wall time and cpu time, including the percentage
   * of the total profiled time. The sections are printed in the order that they were run. Sub
   * timers are indented under their parent.
   *
   * The <Unknown> sections appear when some unprofiled code takes up more than 1% of wall time. The
   * <Unknown total> is the sum of all unprofiled time.
   *)
  let print_summary_timing_table =
    (* Total cpu duration *)
    let sum_cpu result =
      result.user.duration
      +. result.system.duration
      +. result.worker_user.duration
      +. result.worker_system.duration
    in
    (* Total cpu start age *)
    let sum_cpu_start_age result =
      result.user.start_age
      +. result.system.start_age
      +. result.worker_user.start_age
      +. result.worker_system.start_age
    in
    (* Prints a single row of the table. All but the last column have a fixed width. *)
    let print_summary_single_raw
        key (result_wall, result_cpu, (run, read, send, idle, done_, gc_minor, gc_major)) total =
      let run = run -. gc_minor -. gc_major in
      (* run time includes gc time *)
      let worker_total = idle +. done_ +. read +. run +. send +. gc_minor +. gc_major in
      let worker_total =
        if worker_total = 0.0 then
          1.0
        else
          worker_total
      in
      let worker_idle_pct = idle /. worker_total *. 100. in
      let worker_read_pct = read /. worker_total *. 100. in
      let worker_run_pct = run /. worker_total *. 100. in
      let worker_send_pct = send /. worker_total *. 100. in
      let worker_done_pct = done_ /. worker_total *. 100. in
      let worker_gc_minor_pct = gc_minor /. worker_total *. 100. in
      let worker_gc_major_pct = gc_major /. worker_total *. 100. in
      Printf.eprintf
        "%7.3f (%5.1f%%)   %9.3f (%5.1f%%)   %3d%% %3d%% %3d%% %3d%% %3d%% %3d%% %3d%%    %s\n%!"
        result_wall
        (100.0 *. result_wall /. total.wall.duration)
        result_cpu
        (100.0 *. result_cpu /. sum_cpu total)
        (worker_run_pct |> int_of_float)
        (worker_read_pct |> int_of_float)
        (worker_send_pct |> int_of_float)
        (worker_idle_pct |> int_of_float)
        (worker_done_pct |> int_of_float)
        (worker_gc_minor_pct |> int_of_float)
        (worker_gc_major_pct |> int_of_float)
        key
    in
    let print_summary_single key result total =
      let worker_wall_times =
        ( result.worker_wall_times.worker_run.duration,
          result.worker_wall_times.worker_read_request.duration,
          result.worker_wall_times.worker_send_response.duration,
          result.worker_wall_times.worker_idle.duration,
          result.worker_wall_times.worker_done.duration,
          result.worker_wall_times.worker_gc_minor.duration,
          result.worker_wall_times.worker_gc_major.duration )
      in
      print_summary_single_raw key (result.wall.duration, sum_cpu result, worker_wall_times) total
    in
    (* If there's more than 1% of wall time since the last end and the next start_age, then print an
     * <Unknown> row *)
    let print_unknown ~indent last_end (wall_start_age, cpu_start_age, worker_wall_start) total =
      let (run_start, read_start, send_start, idle_start, done_start, gc_minor_start, gc_major_start)
          =
        worker_wall_start
      in
      let ( wall_end,
            cpu_end,
            (run_end, read_end, send_end, idle_end, done_end, gc_minor_end, gc_major_end) ) =
        last_end
      in
      let unknown_wall = wall_start_age -. wall_end in
      if unknown_wall /. total.wall.duration > 0.01 then
        let unknown_cpu = cpu_start_age -. cpu_end in
        let unknown_worker =
          ( run_start -. run_end,
            read_start -. read_end,
            send_start -. send_end,
            idle_start -. idle_end,
            done_start -. done_end,
            gc_minor_start -. gc_minor_end,
            gc_major_start -. gc_major_end )
        in
        print_summary_single_raw
          (indent ^ "<Unknown>")
          (unknown_wall, unknown_cpu, unknown_worker)
          total
    in
    let worker_wall_times_to_tuples worker_wall_times =
      let {
        worker_run = { start_age = run_start; duration = run_duration };
        worker_read_request = { start_age = read_start; duration = read_duration };
        worker_send_response = { start_age = send_start; duration = send_duration };
        worker_idle = { start_age = idle_start; duration = idle_duration };
        worker_done = { start_age = done_start; duration = done_duration };
        worker_gc_minor = { start_age = gc_minor_start; duration = gc_minor_duration };
        worker_gc_major = { start_age = gc_major_start; duration = gc_major_duration };
      } =
        worker_wall_times
      in
      let worker_last =
        (run_start, read_start, send_start, idle_start, done_start, gc_minor_start, gc_major_start)
      in
      let worker_remaining =
        ( run_duration,
          read_duration,
          send_duration,
          idle_duration,
          done_duration,
          gc_minor_duration,
          gc_major_duration )
      in
      let worker_end =
        ( run_start +. run_duration,
          read_start +. read_duration,
          send_start +. send_duration,
          idle_start +. idle_duration,
          done_start +. done_duration,
          gc_minor_start +. gc_minor_duration,
          gc_major_start +. gc_major_duration )
      in
      (worker_last, worker_remaining, worker_end)
    in
    let rec print_result_rows
        ~indent ~total (last_end, (wall_remaining, cpu_remaining, worker_remaining)) result =
      let (result_worker_starts, result_worker_durations, result_worker_end) =
        worker_wall_times_to_tuples result.worker_wall_times
      in
      (* Print an <Unknown> row if needed *)
      print_unknown
        ~indent
        last_end
        (result.wall.start_age, sum_cpu_start_age result, result_worker_starts)
        total;

      (* Print this row *)
      print_summary_single (indent ^ result.timer_name) result total;

      if result.sub_results <> [] then (
        let new_indent = indent ^ "  " in
        let (last_end, remaining) =
          List.fold_left
            (print_result_rows ~indent:new_indent ~total)
            ( (result.wall.start_age, sum_cpu_start_age result, result_worker_starts),
              (result.wall.duration, sum_cpu result, result_worker_durations) )
            result.sub_results
        in
        (* Print an <Unknown> row if there's too much time between the last section and the end of
         * the profiling *)
        print_unknown
          ~indent:new_indent
          last_end
          ( result.wall.start_age +. result.wall.duration,
            sum_cpu_start_age result +. sum_cpu result,
            result_worker_end )
          total;

        (* Print the unknown totals *)
        print_summary_single_raw (new_indent ^ "<Unknown total>") remaining total
      );

      let last_end =
        ( result.wall.start_age +. result.wall.duration,
          sum_cpu_start_age result +. sum_cpu result,
          result_worker_end )
      in
      let remaining =
        let wall_remaining = wall_remaining -. result.wall.duration in
        let cpu_remaining = cpu_remaining -. sum_cpu result in
        let worker_remaining =
          let (run, read, send, idle, done_, gc_minor, gc_major) = worker_remaining in
          ( run -. result.worker_wall_times.worker_run.duration,
            read -. result.worker_wall_times.worker_read_request.duration,
            send -. result.worker_wall_times.worker_send_response.duration,
            idle -. result.worker_wall_times.worker_idle.duration,
            done_ -. result.worker_wall_times.worker_done.duration,
            gc_minor -. result.worker_wall_times.worker_gc_minor.duration,
            gc_major -. result.worker_wall_times.worker_gc_major.duration )
        in
        (wall_remaining, cpu_remaining, worker_remaining)
      in
      (last_end, remaining)
    in
    fun total ->
      (* Print the header *)
      let label = Printf.sprintf "%s Timings" total.timer_name in
      let header =
        "   WALL TIME            CPU TIME         RUN/READ/SEND/IDLE/DONE/GC m/GC M      SECTION"
      in
      let header_len = String.length header + 8 in
      let whitespace_len = header_len - String.length label in
      Printf.eprintf
        "%s%s%s\n%!"
        (String.make ((whitespace_len + 1) / 2) '=')
        label
        (String.make (whitespace_len / 2) '=');
      Printf.eprintf "%s\n%!" header;
      Printf.eprintf "%s\n%!" (String.make header_len '-');

      (* Print the total time *)
      print_summary_single "<Total>" total total;

      let indent = "  " in
      let (worker_last, worker_remaining, worker_end) =
        worker_wall_times_to_tuples total.worker_wall_times
      in
      let last_end = (total.wall.start_age, sum_cpu_start_age total, worker_last) in
      let remaining = (total.wall.duration, sum_cpu total, worker_remaining) in
      (* Print the various sections and the unknown durations *)
      let (last_end, remaining) =
        List.fold_left (print_result_rows ~indent ~total) (last_end, remaining) total.sub_results
      in
      (* Print an <Unknown> row if there's too much time between the last section and the end of the
       * profiling *)
      let () =
        let start =
          ( total.wall.start_age +. total.wall.duration,
            sum_cpu_start_age total +. sum_cpu total,
            worker_end )
        in
        print_unknown ~indent last_end start total
      in
      (* Print the unknown totals *)
      print_summary_single_raw "<Unknown total>" remaining total

  let merge ~from ~into = !into.sub_results_rev <- from :: !into.sub_results_rev
end

module Memory : sig
  type running

  type finished

  val with_memory_lwt : label:string -> f:(running -> 'a Lwt.t) -> (finished * 'a) Lwt.t

  val legacy_sample_memory : metric:string -> value:float -> running -> unit

  val sample_memory : group:string -> metric:string -> value:float -> running -> unit

  val add_memory :
    group:string ->
    metric:string ->
    start:float ->
    delta:float ->
    hwm_delta:float ->
    running ->
    unit

  val to_json : abridged:bool -> finished -> Hh_json.json

  val print_summary_memory_table : finished -> unit

  val merge : from:finished -> into:running -> unit
end = struct
  type memory_result = {
    start: float;
    delta: float;
    high_water_mark_delta: float;
    is_legacy: bool;
  }

  and running' = {
    running_groups_rev: string list;
    running_results: memory_result SMap.t SMap.t;
    running_sub_results_rev: finished list;
  }

  and running = running' ref

  and finished = {
    finished_label: string;
    finished_groups: string list;
    finished_results: memory_result SMap.t SMap.t;
    finished_sub_results: finished list;
  }

  let legacy_group = "LEGACY"

  let with_memory_lwt ~label ~f =
    let running_memory =
      ref { running_groups_rev = []; running_results = SMap.empty; running_sub_results_rev = [] }
    in
    let%lwt ret = f running_memory in
    let finished_memory =
      {
        finished_label = label;
        finished_groups = List.rev !running_memory.running_groups_rev;
        finished_results = !running_memory.running_results;
        finished_sub_results = List.rev !running_memory.running_sub_results_rev;
      }
    in
    Lwt.return (finished_memory, ret)

  let get_group_map ~group running_memory =
    match SMap.find_opt group !running_memory.running_results with
    | None ->
      running_memory :=
        {
          !running_memory with
          running_groups_rev = group :: !running_memory.running_groups_rev;
          running_results = SMap.add group SMap.empty !running_memory.running_results;
        };
      SMap.empty
    | Some group -> group

  let get_metric ~group ~metric running_memory =
    get_group_map ~group running_memory |> SMap.find_opt metric

  let set_metric ~group ~metric entry running_memory =
    let group_map = get_group_map ~group running_memory |> SMap.add metric entry in
    running_memory :=
      {
        !running_memory with
        running_results = SMap.add group group_map !running_memory.running_results;
      }

  let legacy_sample_memory ~metric ~value running_memory =
    let legacy_metric =
      { start = 0.0; delta = value; high_water_mark_delta = value; is_legacy = true }
    in
    set_metric ~group:legacy_group ~metric legacy_metric running_memory

  let start_sampling ~group ~metric ~value running_memory =
    let new_metric =
      { start = value; delta = 0.0; high_water_mark_delta = 0.0; is_legacy = false }
    in
    set_metric ~group ~metric new_metric running_memory

  let sample_memory ~group ~metric ~value running_memory =
    match get_metric ~group ~metric running_memory with
    | None -> start_sampling ~group ~metric ~value running_memory
    | Some old_metric ->
      let new_metric =
        {
          old_metric with
          delta = value -. old_metric.start;
          high_water_mark_delta = max (value -. old_metric.start) old_metric.high_water_mark_delta;
        }
      in
      set_metric ~group ~metric new_metric running_memory

  let add_memory ~group ~metric ~start ~delta ~hwm_delta running_memory =
    let new_metric = { start; delta; high_water_mark_delta = hwm_delta; is_legacy = false } in
    set_metric ~group ~metric new_metric running_memory

  let rec to_json ~abridged finished_memory =
    Hh_json.(
      let object_props =
        SMap.fold
          (fun group_name group props ->
            if group_name = legacy_group then
              SMap.fold
                (fun k v props -> (k, JSON_Number (Dtoa.ecma_string_of_float v.delta)) :: props)
                group
                props
            else
              let group_json =
                SMap.fold
                  (fun k v props ->
                    ( k,
                      JSON_Object
                        [
                          ("start", JSON_Number (Dtoa.ecma_string_of_float v.start));
                          ("delta", JSON_Number (Dtoa.ecma_string_of_float v.delta));
                          ( "hwm_delta",
                            JSON_Number (Dtoa.ecma_string_of_float v.high_water_mark_delta) );
                        ] )
                    :: props)
                  group
                  []
              in
              (group_name, JSON_Object group_json) :: props)
          finished_memory.finished_results
          []
      in
      let object_props =
        if abridged then
          object_props
        else
          let sub_results =
            JSON_Object
              (List.map
                 (fun result -> (result.finished_label, to_json ~abridged:false result))
                 finished_memory.finished_sub_results)
          in
          ("sub_results", sub_results) :: object_props
      in
      JSON_Object object_props)

  let print_summary_memory_table =
    let pretty_num f =
      let abs_f = abs_float f in
      if abs_f > 1000000000.0 then
        Printf.sprintf "%+7.2fG" (f /. 1000000000.0)
      else if abs_f > 1000000.0 then
        Printf.sprintf "%+7.2fM" (f /. 1000000.0)
      else if abs_f > 1000.0 then
        Printf.sprintf "%+7.2fK" (f /. 1000.0)
      else
        Printf.sprintf "%+7.2f " f
    in
    let pretty_pct num denom =
      if denom = 0.0 then
        "(--N/A--)"
      else
        let fraction = num /. denom in
        if fraction >= 10.0 (* e.g "( +20.4x)" fits the space whereas (+2040.0%) doesn't *) then
          Printf.sprintf "(%+6.1fx)" fraction
        else
          Printf.sprintf "(%+6.1f%%)" (fraction *. 100.0)
    in
    (* Prints a single row of the table. All but the last column have a fixed width. *)
    let print_summary_single ~indent key result =
      let indent = String.make indent ' ' in
      Printf.eprintf
        "%s        %s %s    %s %s    %s%s\n%!"
        (pretty_num result.start)
        (pretty_num result.delta)
        (pretty_pct result.delta result.start)
        (pretty_num result.high_water_mark_delta)
        (pretty_pct result.high_water_mark_delta result.start)
        indent
        key
    in
    let header_without_section = "  START                DELTA               HWM DELTA          " in
    let pre_section_whitespace = String.make (String.length header_without_section) ' ' in
    let print_group ~indent finished_results group_name =
      Option.iter (SMap.find_opt group_name finished_results) ~f:(fun group ->
          let indent_str = String.make (String.length header_without_section + indent - 2) ' ' in
          Printf.eprintf "%s== %s ==\n%!" indent_str group_name;
          SMap.iter (print_summary_single ~indent:(indent + 2)) group)
    in
    let print_header label =
      let label = Printf.sprintf "%s Memory Stats" label in
      let header = header_without_section ^ "SECTION" in
      let header_len = String.length header + 8 in
      let whitespace_len = header_len - String.length label in
      Printf.eprintf
        "%s%s%s\n%!"
        (String.make ((whitespace_len + 1) / 2) '=')
        label
        (String.make (whitespace_len / 2) '=');
      Printf.eprintf "%s\n%!" header;
      Printf.eprintf "%s\n%!" (String.make header_len '-')
    in
    let rec print_finished ~indent results =
      if (not (SMap.is_empty results.finished_results)) || results.finished_sub_results <> [] then (
        let header_indent = String.make indent '=' in
        Printf.eprintf
          "%s%s %s %s\n%!"
          pre_section_whitespace
          header_indent
          results.finished_label
          header_indent;
        let indent = indent + 2 in
        List.iter (print_group ~indent results.finished_results) results.finished_groups;
        List.iter (fun sub_result -> print_finished ~indent sub_result) results.finished_sub_results
      )
    in
    fun memory ->
      if SMap.cardinal memory.finished_results > 0 || memory.finished_sub_results <> [] then (
        print_header memory.finished_label;
        print_finished ~indent:2 memory
      )

  let merge ~from ~into =
    into := { !into with running_sub_results_rev = from :: !into.running_sub_results_rev }
end

type running = {
  running_timing: Timing.running;
  running_memory: Memory.running;
}

type finished = {
  finished_timing: Timing.finished;
  finished_memory: Memory.finished;
}

let print_summary profile =
  Printf.eprintf "\n%!";
  Memory.print_summary_memory_table profile.finished_memory;
  Printf.eprintf "\n%!";
  Timing.print_summary_timing_table profile.finished_timing;
  Printf.eprintf "\n%!"

let with_profiling_lwt ~label ~should_print_summary f =
  let%lwt (finished_timing, (finished_memory, ret)) =
    Timing.with_timing_lwt ~label ~f:(fun running_timing ->
        Memory.with_memory_lwt ~label ~f:(fun running_memory ->
            let profile = { running_timing; running_memory } in
            (* We don't really need to wrap this in a finalize, because if this throws no one will ever
             * read the profiling info, so there's really nothing we need to do in the exceptional case
             *)
            f profile))
  in
  let finished_profile = { finished_timing; finished_memory } in
  if should_print_summary then print_summary finished_profile;
  Lwt.return (finished_profile, ret)

let get_profiling_duration profile = Timing.get_total_wall_duration profile.finished_timing

let with_timer_lwt ?should_print ~timer ~f profile =
  Timing.with_timer_lwt ?should_print ~timer ~f profile.running_timing

let legacy_sample_memory ~metric ~value profile =
  Memory.legacy_sample_memory ~metric ~value profile.running_memory

let total_memory_group = "TOTAL"

let sample_memory ?group ~metric ~value profile =
  Memory.sample_memory ~group:total_memory_group ~metric ~value profile.running_memory;
  Option.iter group ~f:(fun group ->
      Memory.sample_memory ~group ~metric ~value profile.running_memory)

let add_memory ?group ~metric ~start ~delta ~hwm_delta profile =
  Memory.add_memory
    ~group:total_memory_group
    ~metric
    ~start
    ~delta
    ~hwm_delta
    profile.running_memory;
  Option.iter group ~f:(fun group ->
      Memory.add_memory ~group ~metric ~start ~delta ~hwm_delta profile.running_memory)

let to_json_properties profile =
  [
    ("timing", Timing.to_json ~abridged:false profile.finished_timing);
    ("memory", Memory.to_json ~abridged:false profile.finished_memory);
  ]

let to_legacy_json_properties profile =
  [
    ("timing", Timing.to_json_legacy ~abridged:false profile.finished_timing);
    ("memory", Memory.to_json ~abridged:false profile.finished_memory);
  ]

let get_timing_json_string profile =
  Timing.to_json ~abridged:false profile.finished_timing |> Hh_json.json_to_string

let get_abridged_timing_json_string profile =
  Timing.to_json ~abridged:true profile.finished_timing |> Hh_json.json_to_string

let get_abridged_legacy_timing_json_string profile =
  Timing.to_json_legacy ~abridged:true profile.finished_timing |> Hh_json.json_to_string

let get_memory_json_string profile =
  Memory.to_json ~abridged:false profile.finished_memory |> Hh_json.json_to_string

let get_abridged_memory_json_string profile =
  Memory.to_json ~abridged:true profile.finished_memory |> Hh_json.json_to_string

let merge ~from ~into =
  Timing.merge ~from:from.finished_timing ~into:into.running_timing;
  Memory.merge ~from:from.finished_memory ~into:into.running_memory
