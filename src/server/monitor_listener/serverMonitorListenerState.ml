(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameSet = Utils_js.FilenameSet

type env_update = ServerEnv.env -> ServerEnv.env

(* Workloads are client requests which we processes FIFO *)
let workload_stream = WorkloadStream.create ()

let push_new_workload workload = WorkloadStream.push workload workload_stream

let push_new_parallelizable_workload workload =
  WorkloadStream.push_parallelizable workload workload_stream

let deferred_parallelizable_workloads_rev = ref []

let defer_parallelizable_workload workload =
  deferred_parallelizable_workloads_rev := workload :: !deferred_parallelizable_workloads_rev

let requeue_deferred_parallelizable_workloads () =
  let workloads = !deferred_parallelizable_workloads_rev in
  deferred_parallelizable_workloads_rev := [];
  Core_list.iter workloads ~f:(fun workload ->
      WorkloadStream.requeue_parallelizable workload workload_stream)

(* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
 * but are quick to handle *)
let (env_update_stream, push_new_env_update) = Lwt_stream.create ()

let push_new_env_update env_update = push_new_env_update (Some env_update)

(* Outstanding cancellation requests are lodged here as soon as they arrive
 * from the monitor (NOT FIFO) as well as being lodged in the normal FIFO
 * queue. (1) if there was a workload sent prior to the cancellation request
 * but we're only just FIFO getting to it now, it can peek to see whether
 * that a cancellation request came after it, and not bother starting.
 * (2) by the time the FIFO queue gets around to seeing the cancellation
 * request in the normal FIFO queue, then we know it can no longer cancel
 * any future requests, so we'll remove it from the set.
 * Observe that our cancellation handling is best-effort only... we won't
 * cancel something already underway, and we might start something even while
 * the cancellation request is queued up somewhere between monitor and server. *)
let cancellation_requests = ref Lsp.IdSet.empty

type recheck_msg = {
  callback: (Profiling_js.finished option -> unit) option;
  file_watcher_metadata: MonitorProt.file_watcher_metadata option;
  files: recheck_files;
  recheck_reason: Persistent_connection_prot.recheck_reason;
}

and recheck_files =
  | ChangedFiles of SSet.t
  | FilesToForceFocusedAndRecheck of SSet.t
  | CheckedSetToForce of CheckedSet.t

(* Files which have changed *)
let (recheck_stream, push_recheck_msg) = Lwt_stream.create ()

let push_recheck_msg ?metadata ?callback ~reason:recheck_reason files =
  push_recheck_msg (Some { files; callback; file_watcher_metadata = metadata; recheck_reason })

let push_files_to_recheck ?metadata ?callback ~reason changed_files =
  push_recheck_msg ?metadata ?callback ~reason (ChangedFiles changed_files)

let push_files_to_force_focused_and_recheck ?callback ~reason forced_focused_files =
  push_recheck_msg ?callback ~reason (FilesToForceFocusedAndRecheck forced_focused_files)

let push_checked_set_to_force ?callback ~reason checked_set =
  push_recheck_msg ?callback ~reason (CheckedSetToForce checked_set)

let pop_next_workload () = WorkloadStream.pop workload_stream

let rec wait_and_pop_parallelizable_workload () =
  let%lwt () = WorkloadStream.wait_for_parallelizable_workload workload_stream in
  match WorkloadStream.pop_parallelizable workload_stream with
  | Some workload -> Lwt.return workload
  | None -> wait_and_pop_parallelizable_workload ()

let update_env env =
  Lwt_stream.get_available env_update_stream |> List.fold_left (fun env f -> f env) env

type recheck_workload = {
  files_to_recheck: FilenameSet.t;
  files_to_force: CheckedSet.t;
  profiling_callbacks: (Profiling_js.finished option -> unit) list;
  metadata: MonitorProt.file_watcher_metadata;
  recheck_reasons_rev: Persistent_connection_prot.recheck_reason list;
}

let empty_recheck_workload =
  {
    files_to_recheck = FilenameSet.empty;
    files_to_force = CheckedSet.empty;
    profiling_callbacks = [];
    metadata = MonitorProt.empty_file_watcher_metadata;
    recheck_reasons_rev = [];
  }

let recheck_workload_is_empty workload =
  let {
    files_to_recheck;
    files_to_force;
    profiling_callbacks = _;
    metadata = _;
    recheck_reasons_rev = _;
  } =
    workload
  in
  FilenameSet.is_empty files_to_recheck && CheckedSet.is_empty files_to_force

let recheck_acc = ref empty_recheck_workload

(* Process the messages which are currently in the recheck stream and return the resulting workload
 *
 * The recheck stream gives us files as a set of strings. `process_updates` takes that set of
 * strings and returns a `FilenameSet.t`. It filters out stuff we don't care about and causes us to
 * exit on incompatible changes.
 *
 * `get_forced` is a function which gives us the `CheckedSet.t` of currently forced files. So if
 * the recheck stream is asking us to focus `foo.js` but it's already focused, then we can ignore
 * it.
 *)
let recheck_fetch ~process_updates ~get_forced =
  recheck_acc :=
    Lwt_stream.get_available recheck_stream
    (* Get all the files which have changed *)
    |> Core_list.fold_left
         ~init:!recheck_acc
         ~f:(fun workload { files; callback; file_watcher_metadata; recheck_reason } ->
           let (is_empty_msg, workload) =
             match files with
             | ChangedFiles changed_files ->
               let updates = process_updates changed_files in
               ( FilenameSet.is_empty updates,
                 {
                   workload with
                   files_to_recheck = FilenameSet.union updates workload.files_to_recheck;
                 } )
             | FilesToForceFocusedAndRecheck forced_focused_files ->
               let updates = process_updates forced_focused_files in
               let focused = FilenameSet.diff updates (get_forced () |> CheckedSet.focused) in
               ( FilenameSet.is_empty updates,
                 {
                   workload with
                   files_to_force = CheckedSet.add ~focused workload.files_to_force;
                   files_to_recheck = FilenameSet.union updates workload.files_to_recheck;
                 } )
             | CheckedSetToForce checked_set ->
               let checked_set = CheckedSet.diff checked_set (get_forced ()) in
               ( CheckedSet.is_empty checked_set,
                 {
                   workload with
                   files_to_force = CheckedSet.union checked_set workload.files_to_force;
                 } )
           in
           let workload =
             match callback with
             | None -> workload
             | Some callback ->
               if is_empty_msg then (
                 (* Call the callback immediately if there's nothing to recheck *)
                 callback None;
                 workload
               ) else
                 { workload with profiling_callbacks = callback :: workload.profiling_callbacks }
           in
           let workload =
             { workload with recheck_reasons_rev = recheck_reason :: workload.recheck_reasons_rev }
           in
           MonitorProt.(
             match file_watcher_metadata with
             | None -> workload
             | Some { total_update_distance; changed_mergebase } ->
               let total_update_distance =
                 total_update_distance + workload.metadata.total_update_distance
               in
               let changed_mergebase = changed_mergebase || workload.metadata.changed_mergebase in
               { workload with metadata = { total_update_distance; changed_mergebase } }))

let get_and_clear_recheck_workload ~process_updates ~get_forced =
  recheck_fetch ~process_updates ~get_forced;
  let recheck_workload = !recheck_acc in
  recheck_acc := empty_recheck_workload;
  recheck_workload

let rec wait_for_updates_for_recheck ~process_updates ~get_forced =
  let%lwt _ = Lwt_stream.is_empty recheck_stream in
  recheck_fetch ~process_updates ~get_forced;
  if recheck_workload_is_empty !recheck_acc then
    wait_for_updates_for_recheck ~process_updates ~get_forced
  else
    Lwt.return_unit

(* Block until any stream receives something *)
let wait_for_anything ~process_updates ~get_forced =
  let%lwt () =
    Lwt.pick
      [ WorkloadStream.wait_for_workload workload_stream;
        (let%lwt _ = Lwt_stream.is_empty env_update_stream in
         Lwt.return_unit);
        (let%lwt _ = Lwt_stream.is_empty recheck_stream in
         Lwt.return_unit);
        wait_for_updates_for_recheck ~process_updates ~get_forced ]
  in
  Lwt.return_unit
