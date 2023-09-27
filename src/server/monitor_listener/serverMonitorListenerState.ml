(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  file_watcher_metadata: MonitorProt.file_watcher_metadata option;
  files: recheck_files;
}

and recheck_files =
  | ChangedFiles of SSet.t * bool
  | FilesToForceFocusedAndRecheck of {
      files: SSet.t;
      skip_incompatible: bool;
          (** Normally, a recheck will abort if certain files change in an
              incompatible way that can't be handled incrementally. But when
              starting a lazy server, we just read the flowconfig, libs, etc.
              as part of the init so we shouldn't fail if they are included
              in the files changed since mergebase. *)
    }
  | GlobalFindRef of {
      request: FindRefsTypes.request;
      client: Persistent_connection.single_client;
      references_to_lsp_response:
        (FindRefsTypes.single_ref list, string) result -> LspProt.response * LspProt.metadata;
      def_locs: Loc.t list;
    }
  | DependenciesToPrioritize of FilenameSet.t
  | FilesToReinit of {
      files_to_prioritize: FilenameSet.t;
      files_to_recheck: FilenameSet.t;
      files_to_force: CheckedSet.t;
    }

(* Files which have changed *)
let (recheck_stream, push_recheck_msg) = Lwt_stream.create ()

let push_recheck_msg ?metadata files =
  push_recheck_msg (Some { files; file_watcher_metadata = metadata })

let push_files_to_recheck ?metadata changed_files =
  push_recheck_msg ?metadata (ChangedFiles (changed_files, false))

let push_files_to_prioritize changed_files = push_recheck_msg (ChangedFiles (changed_files, true))

let push_files_to_force_focused_and_recheck files =
  push_recheck_msg (FilesToForceFocusedAndRecheck { files; skip_incompatible = false })

let push_global_find_ref_request ~request ~client ~references_to_lsp_response def_locs =
  push_recheck_msg (GlobalFindRef { request; client; references_to_lsp_response; def_locs })

(** [push_lazy_init files] triggers a recheck of [files]. It should be called
    immediately after a lazy init, and [files] should be the files changed
    since mergebase. *)
let push_lazy_init ?metadata files =
  push_recheck_msg ?metadata (FilesToForceFocusedAndRecheck { files; skip_incompatible = true })

let push_dependencies_to_prioritize dependencies =
  push_recheck_msg (DependenciesToPrioritize dependencies)

let push_after_reinit
    ?(files_to_prioritize = FilenameSet.empty)
    ?(files_to_recheck = FilenameSet.empty)
    ?(files_to_force = CheckedSet.empty)
    () =
  push_recheck_msg (FilesToReinit { files_to_prioritize; files_to_recheck; files_to_force })

let pop_next_workload () = WorkloadStream.pop workload_stream

let pop_next_parallelizable_workload () = WorkloadStream.pop_parallelizable workload_stream

let update_env env =
  Lwt_stream.get_available env_update_stream |> List.fold_left (fun env f -> f env) env

type recheck_workload = {
  files_to_prioritize: FilenameSet.t;
  files_to_recheck: FilenameSet.t;
  files_to_force: CheckedSet.t;
  find_ref_command:
    ( FindRefsTypes.request
    * Persistent_connection.single_client
    * ((FindRefsTypes.single_ref list, string) result -> LspProt.response * LspProt.metadata)
    )
    option;
  metadata: MonitorProt.file_watcher_metadata;
}

type priority =
  | Priority
  | Normal

let empty_recheck_workload =
  {
    files_to_prioritize = FilenameSet.empty;
    files_to_recheck = FilenameSet.empty;
    files_to_force = CheckedSet.empty;
    find_ref_command = None;
    metadata = MonitorProt.empty_file_watcher_metadata;
  }

let recheck_acc = ref empty_recheck_workload

(** Updates [workload] while maintaining physical equality if there's nothing to do *)
let update
    ?files_to_prioritize ?files_to_recheck ?files_to_force ?find_ref_command ?metadata workload =
  let orig_workload = workload in
  let workload =
    match files_to_prioritize with
    | Some new_files_to_prioritize ->
      let files_to_prioritize =
        workload.files_to_prioritize
        |> FilenameSet.diff new_files_to_prioritize
        |> FilenameSet.union workload.files_to_prioritize
      in
      if workload.files_to_prioritize == files_to_prioritize then
        workload
      else
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
      if workload.files_to_recheck == files_to_recheck then
        workload
      else
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
      if workload.files_to_force == files_to_force then
        workload
      else
        { workload with files_to_force }
    | None -> workload
  in
  let workload =
    match find_ref_command with
    | Some _ ->
      if workload.find_ref_command = find_ref_command then
        workload
      else
        { workload with find_ref_command }
    | None -> workload
  in
  let workload =
    match metadata with
    | Some new_metadata ->
      let metadata = MonitorProt.merge_file_watcher_metadata new_metadata workload.metadata in
      (* merge_file_watcher_metadata doesn't maintain physical equality so use =, not == *)
      if workload.metadata = metadata then
        workload
      else
        { workload with metadata }
    | None -> workload
  in
  (workload, orig_workload != workload)

type workload_changes = {
  num_files_to_prioritize: int;
  num_files_to_recheck: int;
  num_files_to_force: int;
}

let summarize_changes a b =
  let {
    files_to_prioritize = files_to_prioritize_a;
    files_to_recheck = files_to_recheck_a;
    files_to_force = files_to_force_a;
    _;
  } =
    a
  in
  let {
    files_to_prioritize = files_to_prioritize_b;
    files_to_recheck = files_to_recheck_b;
    files_to_force = files_to_force_b;
    _;
  } =
    b
  in
  let num_files_to_prioritize =
    FilenameSet.cardinal files_to_prioritize_b - FilenameSet.cardinal files_to_prioritize_a
  in
  let num_files_to_recheck =
    FilenameSet.cardinal files_to_recheck_b - FilenameSet.cardinal files_to_recheck_a
  in
  let num_files_to_force =
    CheckedSet.cardinal files_to_force_b - CheckedSet.cardinal files_to_force_a
  in
  { num_files_to_prioritize; num_files_to_recheck; num_files_to_force }

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
let recheck_fetch ~process_updates ~get_forced ~priority =
  let (acc, changed) =
    Lwt_stream.get_available recheck_stream
    (* Get all the files which have changed *)
    |> Base.List.fold
         ~init:(!recheck_acc, false)
         ~f:(fun (workload, changed_acc) { files; file_watcher_metadata } ->
           let (workload, changed) =
             match files with
             | ChangedFiles (changed_files, urgent) ->
               let updates = process_updates ~skip_incompatible:false changed_files in
               if urgent then
                 update ~files_to_prioritize:updates workload
               else
                 update ~files_to_recheck:updates workload
             | FilesToForceFocusedAndRecheck { files; skip_incompatible } ->
               let updates = process_updates ~skip_incompatible files in
               let focused = FilenameSet.diff updates (get_forced () |> CheckedSet.focused) in
               let files_to_force = CheckedSet.add ~focused CheckedSet.empty in
               update ~files_to_recheck:updates ~files_to_force workload
             | GlobalFindRef { request; client; references_to_lsp_response; def_locs } ->
               let files_to_recheck = FilenameSet.of_list (List.filter_map Loc.source def_locs) in
               update
                 ~files_to_recheck
                 ~find_ref_command:(request, client, references_to_lsp_response)
                 workload
             | DependenciesToPrioritize dependencies ->
               let to_prioritize = CheckedSet.add ~dependencies CheckedSet.empty in
               let files_to_force =
                 (* if we're doing a normal recheck, don't filter out dependencies that are
                    already being checked, because we want to cancel this recheck and do a
                    faster priority check. but if we're already doing a priority check, we
                    don't want to cancel it just to start another with the same files. *)
                 match priority with
                 | Normal -> to_prioritize
                 | Priority -> CheckedSet.diff to_prioritize (get_forced ())
               in
               update ~files_to_force workload
             | FilesToReinit { files_to_prioritize; files_to_recheck; files_to_force } ->
               let (workload, _changed) =
                 update ~files_to_prioritize ~files_to_recheck ~files_to_force workload
               in
               (* pushing files to reinit should not trigger a "change", because
                  these files are not caused by a distinct event, like a file
                  watcher event; they're a continuation of the existing reinit
                  event. *)
               (workload, false)
           in
           let (workload, metadata_changed) =
             match file_watcher_metadata with
             | None -> (workload, false)
             | Some metadata -> update ~metadata workload
           in
           (workload, changed_acc || changed || metadata_changed)
       )
  in
  recheck_acc := acc;
  changed

let requeue_workload workload =
  Hh_logger.info
    "Re-queueing force-check of %d files and recheck of %d files (%d dependencies)"
    (CheckedSet.cardinal workload.files_to_force)
    (FilenameSet.cardinal (FilenameSet.union workload.files_to_recheck workload.files_to_prioritize))
    (FilenameSet.cardinal workload.files_to_prioritize);
  let prev = !recheck_acc in
  let next =
    {
      files_to_prioritize = FilenameSet.union workload.files_to_prioritize prev.files_to_prioritize;
      files_to_recheck = FilenameSet.union workload.files_to_recheck prev.files_to_recheck;
      files_to_force = CheckedSet.union workload.files_to_force prev.files_to_force;
      find_ref_command =
        (match (workload.find_ref_command, prev.find_ref_command) with
        | (None, r)
        | (r, None)
        | (r, _) ->
          r);
      metadata = MonitorProt.merge_file_watcher_metadata prev.metadata workload.metadata;
    }
  in
  recheck_acc := next

let get_and_clear_recheck_workload ~process_updates ~get_forced =
  let _changed = recheck_fetch ~process_updates ~get_forced ~priority:Normal in
  let recheck_workload = !recheck_acc in
  let { files_to_force; files_to_prioritize; files_to_recheck; _ } = recheck_workload in
  (* if there are any dependencies to force, then we will return them first and leave everything
     else in the queue for the next recheck.

     if there are any files_to_prioritize, those are included in the next recheck but do not
     themselves cause a prioritized recheck. the use-case for this is unexpected changes that
     need to be reparsed. they could be discovered by either a normal recheck or a prioritized
     recheck, and so need to be included in whichever kind happens next.

     for example, imagine we're doing a priority recheck and discover that [foo.js] needs to
     be rechecked, so we push it onto the workload stream and cancel the priority recheck. if
     [foo.js] was not included in the next priority workload, then it would fail and be retried
     but again discover it needs [foo.js]. *)
  let (dependencies_to_force, files_to_force) =
    CheckedSet.partition ~f:(fun _file -> CheckedSet.is_dependency) files_to_force
  in
  if CheckedSet.is_empty dependencies_to_force then (
    recheck_acc := empty_recheck_workload;
    (Normal, recheck_workload)
  ) else
    (* include all files_to_recheck in files_to_prioritize, so that we update the dependency
       graph and merge all known changes. we have to do this because an added or deleted
       file will modify the dependency graph (e.g. phantom dependents), and if we don't
       include everything, we end up discovering a subset of the changes bit by bit as "unexpected
       changes" via [ensure_parsed], which is much slower. Also, we'll never find out about added
       files that way, so the results can end up being inaccurate -- until the file watcher catches
       up.

       so why do we track files_to_prioritize and files_to_recheck separately? files_to_prioritize
       become "dependency" updates. so here, priority_workload treats everything as a
       dependency -- updating the dependency graph and merging, but not checking -- but the
       normal recheck_workload ALSO passes the same files_to_recheck, which upgrades them to normal
       "focused" updates that get checked. *)
    let files_to_prioritize = FilenameSet.union files_to_prioritize files_to_recheck in
    let priority_workload =
      { empty_recheck_workload with files_to_force = dependencies_to_force; files_to_prioritize }
    in
    let recheck_workload =
      { recheck_workload with files_to_force; files_to_prioritize = FilenameSet.empty }
    in
    recheck_acc := recheck_workload;
    (Priority, priority_workload)

(** [wait_for stream] blocks until a message arrives on [stream] *)
let wait_for stream =
  (* [is_empty] blocks until there is something to read or an explicit [None] (end of stream) *)
  let%lwt _ = Lwt_stream.is_empty stream in
  Lwt.return_unit

let wait_for_parallelizable_workload () =
  WorkloadStream.wait_for_parallelizable_workload workload_stream

let rec wait_for_updates_for_recheck ~process_updates ~get_forced ~priority =
  let%lwt () = wait_for recheck_stream in
  let workload_before = !recheck_acc in
  if recheck_fetch ~process_updates ~get_forced ~priority then
    let workload_after = !recheck_acc in
    Lwt.return (summarize_changes workload_before workload_after)
  else
    wait_for_updates_for_recheck ~process_updates ~get_forced ~priority

(* Block until any stream receives something *)
let wait_for_anything ~process_updates ~get_forced =
  let%lwt () =
    Lwt.pick
      [
        WorkloadStream.wait_for_workload workload_stream;
        wait_for env_update_stream;
        wait_for recheck_stream;
        (let%lwt _ = wait_for_updates_for_recheck ~process_updates ~get_forced ~priority:Normal in
         Lwt.return_unit
        );
      ]
  in
  Lwt.return_unit
