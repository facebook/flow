(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let sample_memory profiling =
  let heap = SharedMem.heap_size () in
  let { SharedMem.nonempty_slots; used_slots; slots } = SharedMem.hash_stats () in
  Profiling_js.sample_memory profiling ~metric:"heap" ~value:(float_of_int heap);
  Profiling_js.sample_memory
    profiling
    ~metric:"hash_nonempty_slots"
    ~value:(float_of_int nonempty_slots);
  Profiling_js.sample_memory profiling ~metric:"hash_slots" ~value:(float_of_int slots);
  Profiling_js.sample_memory profiling ~metric:"hash_used_slots" ~value:(float_of_int used_slots)

let with_memory_profiling_lwt ~profiling f =
  sample_memory profiling;

  let%lwt ret = f () in

  sample_memory profiling;

  Lwt.return ret

let with_memory_info callback =
  let%lwt cgroup_stats = CGroup.get_stats () in
  let hash_stats = SharedMem.hash_stats () in
  let heap_size = SharedMem.heap_size () in
  callback ~cgroup_stats ~hash_stats ~heap_size;
  Lwt.return_unit

module MemorySamplingLoop = LwtLoop.Make (struct
  type acc =
    cgroup_stats:(CGroup.stats, string) result ->
    hash_stats:SharedMem.table_stats ->
    heap_size:int ->
    unit

  let main callback =
    let%lwt () = with_memory_info callback in
    let%lwt () = Lwt_unix.sleep 1.0 in
    Lwt.return callback

  let catch _ exn =
    Hh_logger.error "Exception in MemorySamplingLoop: %s" (Exception.to_string exn);
    Lwt.return_unit
end)

let with_memory_timer_lwt =
  let module P = Profiling_js in
  let clear_worker_memory () =
    ["worker_rss_start"; "worker_rss_delta"; "worker_rss_hwm_delta"] |> List.iter Measure.delete
  in
  let profile_add_memory profiling getter group metric =
    getter "worker_rss_start"
    |> Base.Option.iter ~f:(fun start ->
           getter "worker_rss_delta"
           |> Base.Option.iter ~f:(fun delta ->
                  getter "worker_rss_hwm_delta"
                  |> Base.Option.iter ~f:(fun hwm_delta ->
                         P.add_memory ~group ~metric ~start ~delta ~hwm_delta profiling)))
  in
  let sample_memory timer profiling ~cgroup_stats ~hash_stats ~heap_size =
    P.sample_memory profiling ~group:timer ~metric:"heap" ~value:(float heap_size);

    let { SharedMem.nonempty_slots; used_slots; slots } = hash_stats in
    P.sample_memory
      profiling
      ~group:timer
      ~metric:"hash_nonempty_slots"
      ~value:(float nonempty_slots);

    P.sample_memory profiling ~group:timer ~metric:"hash_used_slots" ~value:(float used_slots);

    P.sample_memory profiling ~group:timer ~metric:"hash_slots" ~value:(float slots);

    match cgroup_stats with
    | Error _ -> ()
    | Ok { CGroup.total; total_swap; anon; file; shmem } ->
      P.sample_memory profiling ~group:timer ~metric:"cgroup_total" ~value:(float total);

      P.sample_memory profiling ~group:timer ~metric:"cgroup_swap" ~value:(float total_swap);

      P.sample_memory profiling ~group:timer ~metric:"cgroup_anon" ~value:(float anon);

      P.sample_memory profiling ~group:timer ~metric:"cgroup_shmem" ~value:(float shmem);

      P.sample_memory profiling ~group:timer ~metric:"cgroup_file" ~value:(float file)
  in
  fun ?options timer profiling f ->
    let should_print = Base.Option.value_map options ~default:false ~f:Options.should_profile in
    let sample_memory = sample_memory timer profiling in
    clear_worker_memory ();

    (* Record the cgroup info at the start *)
    let%lwt () = with_memory_info sample_memory in
    (* Asynchronously run a thread that periodically grabs the cgroup stats *)
    let sampling_loop = MemorySamplingLoop.run sample_memory in
    let%lwt ret =
      try%lwt
        let%lwt ret = P.with_timer_lwt ~should_print ~timer ~f profiling in
        Lwt.cancel sampling_loop;
        Lwt.return ret
      with
      | exn ->
        let exn = Exception.wrap exn in
        Lwt.cancel sampling_loop;
        Exception.reraise exn
    in
    (* Record the cgroup info at the end *)
    let%lwt () = with_memory_info sample_memory in
    profile_add_memory profiling Measure.get_mean timer "worker_rss_avg";
    profile_add_memory profiling Measure.get_max timer "worker_rss_max";
    clear_worker_memory ();
    Lwt.return ret
