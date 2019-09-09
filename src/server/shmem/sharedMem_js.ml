(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include SharedMem

module Prefix = Prefix

module Ident = Ident

module Collect : sig
  val collect: [ `gentle | `aggressive | `always_TEST ] -> unit
  val with_memory_profiling_lwt:
    profiling:Profiling_js.running ->
    collect_at_end:bool ->
    (unit -> 'a Lwt.t) ->
    'a Lwt.t
end = struct
  let profile_before_collect_callback = ref (fun () -> ())

  let collect effort =
    if SharedMem.should_collect effort
    then begin
      (!profile_before_collect_callback) ();
      MonitorRPC.status_update ~event:ServerStatus.GC_start;
      SharedMem.collect effort
    end

  let sample_memory profiling =
    let heap = heap_size () in
    let { nonempty_slots; used_slots; slots } = hash_stats () in
    Profiling_js.sample_memory profiling
      ~metric:"heap"
      ~value:(float_of_int heap);
    Profiling_js.sample_memory profiling
      ~metric:"hash_nonempty_slots"
      ~value:(float_of_int nonempty_slots);
    Profiling_js.sample_memory profiling
      ~metric:"hash_slots"
      ~value:(float_of_int slots);
    Profiling_js.sample_memory profiling
      ~metric:"hash_used_slots"
      ~value:(float_of_int used_slots)

  let with_memory_profiling_lwt ~profiling ~collect_at_end f =
    sample_memory profiling;
    profile_before_collect_callback := (fun () -> sample_memory profiling);

    let%lwt ret = f () in

    if collect_at_end then collect `aggressive;

    sample_memory profiling;
    profile_before_collect_callback := (fun () -> ());

    Lwt.return ret
end

include Collect
