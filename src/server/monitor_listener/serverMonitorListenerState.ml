(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameSet = Utils_js.FilenameSet

type env_update = ServerEnv.env -> ServerEnv.env

(* Workloads are client requests which we processes FIFO *)
let workload_stream = WorkloadStream.create ()

let push_new_workload ~name workload = WorkloadStream.push ~name workload workload_stream

let push_new_parallelizable_workload ~name workload =
  WorkloadStream.push_parallelizable ~name workload workload_stream

let deferred_parallelizable_workloads_rev = ref []

let defer_parallelizable_workload ~name workload =
  deferred_parallelizable_workloads_rev :=
    (name, workload) :: !deferred_parallelizable_workloads_rev

let requeue_deferred_parallelizable_workloads () =
  let workloads = !deferred_parallelizable_workloads_rev in
  deferred_parallelizable_workloads_rev := [];
  Base.List.iter workloads ~f:(fun (name, workload) ->
      WorkloadStream.requeue_parallelizable ~name workload workload_stream
  )

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
  recheck_reason: LspProt.recheck_reason;
}

and recheck_files =
  | ChangedFiles of SSet.t * bool
  | FilesToForceFocusedAndRecheck of SSet.t
  | DependenciesToPrioritize of FilenameSet.t
  | FilesToResync of SSet.t
      (** When the file watcher restarts, it can miss changes so we need to recheck
          all of these files. This differs subtly from [ChangedFiles] because they
          didn't necessarily change, and we want to run the recheck even if they
          didn't, because we also need to recheck ServerEnv's checked files. *)

(* Files which have changed *)
let (recheck_stream, push_recheck_msg) = Lwt_stream.create ()

let push_recheck_msg ?metadata ?callback ~reason:recheck_reason files =
  push_recheck_msg (Some { files; callback; file_watcher_metadata = metadata; recheck_reason })

let push_files_to_recheck ?metadata ?callback ~reason changed_files =
  push_recheck_msg ?metadata ?callback ~reason (ChangedFiles (changed_files, false))

let push_files_to_prioritize ~reason changed_files =
  push_recheck_msg ~reason (ChangedFiles (changed_files, true))

let push_files_to_force_focused_and_recheck ?callback ~reason forced_focused_files =
  push_recheck_msg ?callback ~reason (FilesToForceFocusedAndRecheck forced_focused_files)

let push_dependencies_to_prioritize ?callback ~reason dependencies =
  push_recheck_msg ?callback ~reason (DependenciesToPrioritize dependencies)

let push_files_to_resync_after_file_watcher_restart
    ?metadata ?callback ~reason changed_since_mergebase =
  push_recheck_msg ?metadata ?callback ~reason (FilesToResync changed_since_mergebase)

let pop_next_workload () = WorkloadStream.pop workload_stream

let rec wait_and_pop_parallelizable_workload () =
  let%lwt () = WorkloadStream.wait_for_parallelizable_workload workload_stream in
  match WorkloadStream.pop_parallelizable workload_stream with
  | Some workload -> Lwt.return workload
  | None -> wait_and_pop_parallelizable_workload ()

let update_env env =
  Lwt_stream.get_available env_update_stream |> List.fold_left (fun env f -> f env) env

type recheck_workload = {
  files_to_prioritize: FilenameSet.t;
  files_to_recheck: FilenameSet.t;
  files_to_force: CheckedSet.t;
  profiling_callbacks: (Profiling_js.finished option -> unit) list;
  metadata: MonitorProt.file_watcher_metadata;
  recheck_reasons_rev: LspProt.recheck_reason list;
}

let empty_recheck_workload =
  {
    files_to_prioritize = FilenameSet.empty;
    files_to_recheck = FilenameSet.empty;
    files_to_force = CheckedSet.empty;
    profiling_callbacks = [];
    metadata = MonitorProt.empty_file_watcher_metadata;
    recheck_reasons_rev = [];
  }

let recheck_acc = ref empty_recheck_workload

(** Updates [workload] while maintaining physical equality of each key if there's nothing to do *)
let update ?files_to_prioritize ?files_to_recheck ?files_to_force workload =
  let workload =
    match files_to_prioritize with
    | Some new_files_to_prioritize ->
      let files_to_prioritize =
        workload.files_to_prioritize
        |> FilenameSet.diff new_files_to_prioritize
        |> FilenameSet.union workload.files_to_prioritize
      in
      { workload with files_to_prioritize }
    | None -> workload
  in
  let workload =
    match files_to_recheck with
    | Some new_files_to_recheck ->
      let files_to_recheck =
        workload.files_to_recheck
        |> FilenameSet.diff new_files_to_recheck
        |> FilenameSet.union workload.files_to_recheck
      in
      { workload with files_to_recheck }
    | None -> workload
  in
  let workload =
    match files_to_force with
    | Some new_files_to_force ->
      let files_to_force =
        workload.files_to_force
        |> CheckedSet.diff new_files_to_force
        |> CheckedSet.union workload.files_to_force
      in
      { workload with files_to_force }
    | None -> workload
  in
  workload

let workload_changed a b =
  let {
    files_to_prioritize = files_to_prioritize_a;
    files_to_recheck = files_to_recheck_a;
    files_to_force = files_to_force_a;
    profiling_callbacks = _;
    metadata = _;
    recheck_reasons_rev = _;
  } =
    a
  in
  let {
    files_to_prioritize = files_to_prioritize_b;
    files_to_recheck = files_to_recheck_b;
    files_to_force = files_to_force_b;
    profiling_callbacks = _;
    metadata = _;
    recheck_reasons_rev = _;
  } =
    b
  in
  files_to_prioritize_a != files_to_prioritize_b
  || files_to_recheck_a != files_to_recheck_b
  || files_to_force_a != files_to_force_b

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
    |> Base.List.fold_left
         ~init:!recheck_acc
         ~f:(fun workload { files; callback; file_watcher_metadata; recheck_reason } ->
           let skip_incompatible =
             match recheck_reason with
             | LspProt.Lazy_init_typecheck -> Some true
             | _ -> None
           in
           let (is_empty_msg, workload) =
             match files with
             | ChangedFiles (changed_files, urgent) ->
               let updates = process_updates ?skip_incompatible changed_files in
               let workload =
                 if urgent then
                   update ~files_to_prioritize:updates workload
                 else
                   update ~files_to_recheck:updates workload
               in
               (FilenameSet.is_empty updates, workload)
             | FilesToForceFocusedAndRecheck forced_focused_files ->
               let updates = process_updates ?skip_incompatible forced_focused_files in
               let focused = FilenameSet.diff updates (get_forced () |> CheckedSet.focused) in
               let files_to_force = CheckedSet.add ~focused CheckedSet.empty in
               let workload = update ~files_to_prioritize:updates ~files_to_force workload in
               (FilenameSet.is_empty updates, workload)
             | DependenciesToPrioritize dependencies ->
               let checked_set = CheckedSet.add ~dependencies CheckedSet.empty in
               let files_to_force = CheckedSet.diff checked_set (get_forced ()) in
               let workload = update ~files_to_force workload in
               (CheckedSet.is_empty files_to_force, workload)
             | FilesToResync changed_since_mergebase ->
               let updates = process_updates ?skip_incompatible changed_since_mergebase in
               let workload = update ~files_to_recheck:updates workload in
               (* we don't know yet whether there's anything to recheck. even if [updates]
                   is empty, the server also needs to check the existing checked files, so
                   we always return [false] here. *)
               (false, workload)
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
           match file_watcher_metadata with
           | None -> workload
           | Some metadata ->
             let metadata = MonitorProt.merge_file_watcher_metadata metadata workload.metadata in
             { workload with metadata }
       )

let requeue_workload workload =
  Hh_logger.info
    "Re-queueing force-check of %d files and recheck of %d files"
    (CheckedSet.cardinal workload.files_to_force)
    (FilenameSet.cardinal workload.files_to_recheck);
  let prev = !recheck_acc in
  let next =
    {
      files_to_prioritize = FilenameSet.union workload.files_to_prioritize prev.files_to_prioritize;
      files_to_recheck = FilenameSet.union workload.files_to_recheck prev.files_to_recheck;
      files_to_force = CheckedSet.union workload.files_to_force prev.files_to_force;
      profiling_callbacks = prev.profiling_callbacks @ workload.profiling_callbacks;
      metadata = MonitorProt.merge_file_watcher_metadata prev.metadata workload.metadata;
      recheck_reasons_rev = prev.recheck_reasons_rev @ workload.recheck_reasons_rev;
    }
  in
  recheck_acc := next

let get_and_clear_recheck_workload ~prioritize_dependency_checks ~process_updates ~get_forced =
  recheck_fetch ~process_updates ~get_forced;
  let recheck_workload = !recheck_acc in
  let { files_to_force; files_to_prioritize; _ } = recheck_workload in
  (* when prioritize_dependency_checks is enabled, if there are any dependencies to force, then we
     will return them first and leave everything else in the queue for the next recheck.

     if there are any files_to_prioritize, those are included in the next recheck but do not
     themselves cause a prioritized recheck. the use-case for this is unexpected changes that
     need to be reparsed. they could be discovered by either a regular recheck or a prioritized
     recheck, and so need to be included in whichever kind happens next.

     for example, imagine we're doing a priority recheck and discover that [foo.js] needs to
     be rechecked, so we push it onto the workload stream and cancel the priority recheck. if
     [foo.js] was not included in the next priority workload, then it would fail and be retried
     but again discover it needs [foo.js]. *)
  let (dependencies_to_force, files_to_force) =
    CheckedSet.partition ~f:CheckedSet.is_dependency files_to_force
  in
  if (not prioritize_dependency_checks) || CheckedSet.is_empty dependencies_to_force then (
    recheck_acc := empty_recheck_workload;
    recheck_workload
  ) else
    let priority_workload =
      { empty_recheck_workload with files_to_force = dependencies_to_force; files_to_prioritize }
    in
    let recheck_workload =
      { recheck_workload with files_to_force; files_to_prioritize = FilenameSet.empty }
    in
    recheck_acc := recheck_workload;
    priority_workload

(** [wait_for stream] blocks until a message arrives on [stream] *)
let wait_for stream =
  (* [is_empty] blocks until there is something to read or an explicit [None] (end of stream) *)
  let%lwt _ = Lwt_stream.is_empty stream in
  Lwt.return_unit

let rec wait_for_updates_for_recheck ~process_updates ~get_forced =
  let%lwt () = wait_for recheck_stream in
  let workload_before = !recheck_acc in
  recheck_fetch ~process_updates ~get_forced;
  let workload_after = !recheck_acc in
  if workload_changed workload_before workload_after then
    Lwt.return_unit
  else
    wait_for_updates_for_recheck ~process_updates ~get_forced

(* Block until any stream receives something *)
let wait_for_anything ~process_updates ~get_forced =
  let%lwt () =
    Lwt.pick
      [
        WorkloadStream.wait_for_workload workload_stream;
        wait_for env_update_stream;
        wait_for recheck_stream;
        wait_for_updates_for_recheck ~process_updates ~get_forced;
      ]
  in
  Lwt.return_unit
