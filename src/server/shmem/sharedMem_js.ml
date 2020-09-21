(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include SharedMem
module Prefix = Prefix
module Ident = Ident

module Collect : sig
  val collect : [ `gentle | `aggressive | `always_TEST ] -> unit

  val with_memory_profiling_lwt :
    profiling:Profiling_js.running -> collect_at_end:bool -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val with_memory_timer_lwt :
    ?options:Options.t -> string -> Profiling_js.running -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end = struct
  let profile_before_collect_callback = ref (fun () -> ())

  let collect effort =
    if SharedMem.should_collect effort then (
      !profile_before_collect_callback ();
      MonitorRPC.status_update ~event:ServerStatus.GC_start;
      SharedMem.collect effort
    )

  let sample_memory profiling =
    let heap = heap_size () in
    let { nonempty_slots; used_slots; slots } = hash_stats () in
    Profiling_js.sample_memory profiling ~metric:"heap" ~value:(float_of_int heap);
    Profiling_js.sample_memory
      profiling
      ~metric:"hash_nonempty_slots"
      ~value:(float_of_int nonempty_slots);
    Profiling_js.sample_memory profiling ~metric:"hash_slots" ~value:(float_of_int slots);
    Profiling_js.sample_memory profiling ~metric:"hash_used_slots" ~value:(float_of_int used_slots)

  let with_memory_profiling_lwt ~profiling ~collect_at_end f =
    sample_memory profiling;
    (profile_before_collect_callback := (fun () -> sample_memory profiling));

    let%lwt ret = f () in
    if collect_at_end then collect `aggressive;

    sample_memory profiling;
    (profile_before_collect_callback := (fun () -> ()));

    Lwt.return ret

  let with_memory_info callback =
    let%lwt cgroup_stats = CGroup.get_stats () in
    (* Reading hash_stats while workers are writing can cause assertion errors *)
    let hash_stats = (try Some (hash_stats ()) with _ -> None) in
    let heap_size = heap_size () in
    callback ~cgroup_stats ~hash_stats ~heap_size;
    Lwt.return_unit

  module MemorySamplingLoop = LwtLoop.Make (struct
    type acc =
      cgroup_stats:(CGroup.stats, string) result ->
      hash_stats:table_stats option ->
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

      Base.Option.iter hash_stats ~f:(fun { nonempty_slots; used_slots; slots } ->
          P.sample_memory
            profiling
            ~group:timer
            ~metric:"hash_nonempty_slots"
            ~value:(float nonempty_slots);

          P.sample_memory profiling ~group:timer ~metric:"hash_used_slots" ~value:(float used_slots);

          P.sample_memory profiling ~group:timer ~metric:"hash_slots" ~value:(float slots));

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
        with exn ->
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
end

include Collect
