(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

module Parallelizable_workload_loop = LwtLoop.Make (struct
  type acc = unit Lwt.t * ServerEnv.genv * ServerEnv.env

  let should_pause = ref true

  let main (wait_for_cancel, genv, env) =
    (* The Lwt.pick will arbitrarily choose one or the other thread if they are both ready. So let's
     * explicitly check if the wait_for_cancel thread has resolved to give it priority *)
    let () =
      match Lwt.state wait_for_cancel with
      | Lwt.Return () -> raise Lwt.Canceled
      | Lwt.Fail exn ->
        let exn = Exception.wrap exn in
        Exception.reraise exn
      | Lwt.Sleep -> ()
    in
    (* Lwt.pick waits until one of these two promises is resolved (returns or fails). Then it
     * cancels the other one and returns/fails.
     *
     * Normally, the wait_for_parallelizable_workload thread will finish first. Then Lwt.pick
     * will cancel the `let%lwt () = ... in raise Lwt.Canceled` thread. The `wait_for_cancel` thread
     * is NOT cancelable so that will stay unresolved.
     *
     * Eventually, wait_for_cancel will resolve. Then we'll cancel the
     * wait_for_parallelizable_workload thread throw Lwt.Canceled *)
    let%lwt () =
      Lwt.pick
        [
          ServerMonitorListenerState.wait_for_parallelizable_workload ();
          (let%lwt () = wait_for_cancel in
           raise Lwt.Canceled
          );
        ]
    in
    let%lwt () =
      match ServerMonitorListenerState.pop_next_parallelizable_workload () with
      | Some
          {
            WorkloadStream.parallelizable_workload_should_be_cancelled;
            parallelizable_workload_handler;
          } ->
        (* We have a workload! Let's run it! *)
        Hh_logger.info "Running a parallel workload";
        should_pause :=
          if parallelizable_workload_should_be_cancelled () then
            false
          else
            !should_pause;
        parallelizable_workload_handler env
      | None ->
        (* this should never happen... we just waited for one! *)
        Lwt.return_unit
    in
    Lwt.return (wait_for_cancel, genv, env)

  let catch _ exn = Exception.reraise exn
end)

let start_parallelizable_workloads genv env =
  (* The wait_for_cancel thread itself is NOT cancelable *)
  let (wait_for_cancel, wakener) = Lwt.wait () in
  let loop_thread = Parallelizable_workload_loop.run (wait_for_cancel, genv, env) in
  (* Allow this stop function to be called multiple times for the same loop *)
  let already_woken = ref false in
  fun () ->
    (* Tell the loop to cancel at its earliest convenience *)
    if not !already_woken then Lwt.wakeup wakener ();
    already_woken := true;

    (* Wait for the loop to finish *)
    loop_thread

let get_lazy_stats ~options env =
  let checked_files = CheckedSet.cardinal env.checked_files in
  let total_files = FilenameSet.cardinal env.files in
  { ServerProt.Response.lazy_mode = Options.lazy_mode options; checked_files; total_files }

(* Filter a set of updates coming from the file watcher or force-recheck or wherever and return a
 * FilenameSet. Updates may be coming in from the root, or an include path.
 *
 * If any update can't be processed incrementally, the Flow server will exit *)
let process_updates ~skip_incompatible ~options env updates =
  match
    Recheck_updates.process_updates ~skip_incompatible ~options ~libs:env.ServerEnv.libs updates
  with
  | Ok updates -> updates
  | Error { Recheck_updates.msg; exit_status } -> Exit.exit ~msg exit_status

(** Notify clients that a recheck is starting. This is used to know that
    error updates are incremental. [send_end_recheck] must be called when
    the recheck is done, to exit this mode. *)
let send_start_recheck env =
  MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
  Persistent_connection.send_start_recheck env.connections

(** Notify clients that the recheck is done and send finalized errors *)
let send_end_recheck ~options env =
  (* We must send "end_recheck" prior to sending errors+warnings so the client
     knows that this set of errors+warnings are final ones, not incremental. *)
  let lazy_stats = get_lazy_stats ~options env in
  Persistent_connection.send_end_recheck ~lazy_stats env.connections;

  let calc_errors_and_warnings () =
    let (errors, warnings, _) = ErrorCollator.get_with_separate_warnings env in
    (errors, warnings)
  in
  Persistent_connection.update_clients
    ~clients:env.connections
    ~errors_reason:LspProt.End_of_recheck
    ~calc_errors_and_warnings;

  MonitorRPC.status_update ~event:ServerStatus.Finishing_up

let recheck
    genv
    env
    ?(files_to_force = CheckedSet.empty)
    ~find_ref_command
    ~changed_mergebase
    ~missed_changes
    ~will_be_checked_files
    updates =
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  let (find_ref_request, find_ref_transformer_with_client) =
    match find_ref_command with
    | Some (r, client, f) -> (r, Some (f, client))
    | None -> (FindRefsTypes.empty_request, None)
  in
  let%lwt (profiling, (log_recheck_event, recheck_stats, env)) =
    let should_print_summary = Options.should_profile options in
    Profiling_js.with_profiling_lwt ~label:"Recheck" ~should_print_summary (fun profiling ->
        send_start_recheck env;
        let%lwt (log_recheck_event, recheck_stats, find_ref_results, env) =
          Types_js.recheck
            ~profiling
            ~options
            ~workers
            ~updates
            ~find_ref_request
            ~files_to_force
            ~changed_mergebase
            ~missed_changes
            ~will_be_checked_files
            env
        in
        Base.Option.iter find_ref_transformer_with_client ~f:(fun (transformer, client) ->
            let server_logging_context = Some (FlowEventLogger.get_context ()) in
            let (resp, metadata) = transformer find_ref_results in
            let metadata = { metadata with LspProt.server_logging_context } in
            Persistent_connection.send_response (resp, metadata) client
        );
        send_end_recheck ~options env;
        Lwt.return (log_recheck_event, recheck_stats, env)
    )
  in
  let%lwt () = log_recheck_event ~profiling in

  let event =
    let duration = Profiling_js.get_profiling_duration profiling in
    LspProt.Recheck_summary { duration; stats = recheck_stats }
  in
  MonitorRPC.send_telemetry event;

  Lwt.return (profiling, env)

(* Runs a function which should be canceled if we are notified about any file changes. After the
 * thread is canceled, post_cancel is called and its result returned *)
let run_but_cancel_on_file_changes ~options env ~get_forced ~priority ~f ~pre_cancel ~post_cancel =
  let process_updates ~skip_incompatible = process_updates ~skip_incompatible ~options env in
  (* We don't want to start running f until we're in the try block *)
  let (waiter, wakener) = Lwt.task () in
  let run_thread =
    let%lwt () = waiter in
    f ()
  in
  let cancel_thread =
    let%lwt {
          ServerMonitorListenerState.num_files_to_prioritize;
          num_files_to_force;
          num_files_to_recheck;
        } =
      ServerMonitorListenerState.wait_for_updates_for_recheck ~process_updates ~get_forced ~priority
    in
    Hh_logger.info
      "Canceling recheck to prioritize %d, force %d, and recheck %d additional files"
      num_files_to_prioritize
      num_files_to_force
      num_files_to_recheck;
    FlowEventLogger.recheck_canceled
      ~priority:
        (match priority with
        | ServerMonitorListenerState.Priority -> true
        | ServerMonitorListenerState.Normal -> false)
      ~num_files_to_prioritize
      ~num_files_to_force
      ~num_files_to_recheck;
    let%lwt () = pre_cancel () in
    Lwt.cancel run_thread;
    Lwt.return_unit
  in
  try%lwt
    Lwt.wakeup wakener ();
    let%lwt ret = run_thread in
    Lwt.cancel cancel_thread;
    Lwt.return ret
  with
  | Lwt.Canceled ->
    Lwt.cancel cancel_thread;
    post_cancel ()

type recheck_outcome =
  | Nothing_to_do of ServerEnv.env
  | Completed_recheck of {
      profiling: Profiling_js.finished;
      env: ServerEnv.env;
      recheck_count: int;
    }

(* Perform a single recheck. This will incorporate any pending changes from the file watcher.
 * If any file watcher notifications come in during the recheck, it will be canceled and restarted
 * to include the new changes
 *
 * [recheck_count] is the number of rechecks in this series, including past canceled rechecks and
 * the current one.
 *)
let rec recheck_single ~recheck_count genv env =
  let env = ServerMonitorListenerState.update_env env in
  let options = genv.ServerEnv.options in
  let process_updates ~skip_incompatible = process_updates ~skip_incompatible ~options env in
  (* This ref is an estimate of the files which will be checked by the time the recheck is done.
   * As the recheck progresses, the estimate will get better. We use this estimate to prevent
   * canceling the recheck to force a file which we were already going to check
   *
   * This early estimate is not a very good estimate, since it's missing new dependents and
   * dependencies. However it should be good enough to prevent rechecks continuously restarting as
   * the server gets spammed with autocomplete requests *)
  let will_be_checked_files = ref env.ServerEnv.checked_files in
  let get_forced () = !will_be_checked_files in
  let (priority, workload) =
    ServerMonitorListenerState.get_and_clear_recheck_workload ~process_updates ~get_forced
  in
  (* When watchman restarts, we miss any filesystem changes that might happen while it's
     down. Likewise, if so many files change that Watchman's underlying file watchers
     can't keep up, it acts like it restarted.

     This is signaled by [missed_changes = true].

     To re-synchronize, we need to recheck all of the files that could have changed
     while it wasn't watching:

       1) a file that was previously unchanged is now changed
       2) a changed file changed again
       3) a previously changed file was reverted
       4) the mergebase changed, changing some committed files

     The changes since mergebase are included in files_to_recheck, which tells us about
     (1) and (2). To handle (3), we recheck all focused (i.e. previously changed) files.
     But we can't incrementally handle (4): watchman can't tell us all the files that
     changed between the two mergebase commits (`hg` can, but it's not worth implementing
     this). To handle (4) we reinitialize when we missed a mergebase change. *)
  let {
    ServerMonitorListenerState.metadata = { MonitorProt.changed_mergebase; missed_changes };
    files_to_recheck;
    files_to_prioritize;
    files_to_force;
    find_ref_command;
  } =
    workload
  in
  let did_change_mergebase = Base.Option.value ~default:false changed_mergebase in
  let files_to_recheck =
    CheckedSet.add ~focused:files_to_recheck ~dependencies:files_to_prioritize CheckedSet.empty
  in
  let files_to_recheck =
    if missed_changes && not did_change_mergebase then
      (* If the file watcher missed some changes, it's possible that previously-modified
         files have been reverted when it wasn't watching. Since previously-modified
         files are focused, we recheck all focused files. However, if the mergebase
         changed, then we're going to re-init instead. *)
      CheckedSet.add ~focused:(CheckedSet.focused env.checked_files) files_to_recheck
    else
      files_to_recheck
  in
  if
    (not did_change_mergebase)
    && CheckedSet.is_empty files_to_recheck
    && CheckedSet.is_empty files_to_force
  then
    Lwt.return (Nothing_to_do env)
  else
    (* Start the parallelizable workloads loop and return a function which will stop the loop *)
    let stop_parallelizable_workloads = start_parallelizable_workloads genv env in
    let post_cancel () =
      Hh_logger.info
        "Recheck successfully canceled. Restarting the recheck to include new file changes";
      (* The canceled recheck, or a preceding sequence of canceled rechecks where none completed,
       * may have introduced garbage into shared memory. Since we immediately start another
       * recheck, we should first check whether we need to compact. Otherwise, sharedmem could
       * potentially grow unbounded.
       *
       * The constant budget provided here should be sufficient to fully scan a 5G heap within 5
       * iterations. We want to avoid the scenario where repeatedly cancelled rechecks cause the
       * heap to grow faster than we can scan. An algorithmic approach to determine the amount of
       * work based on the allocation rate would be better. *)
      let _done : bool = SharedMem.collect_slice 256000 in
      ServerMonitorListenerState.requeue_workload workload;
      recheck_single ~recheck_count:(recheck_count + 1) genv env
    in
    let f () =
      let%lwt (profiling, env) =
        try%lwt
          recheck
            genv
            env
            ~files_to_force
            ~changed_mergebase
            ~missed_changes
            ~will_be_checked_files
            ~find_ref_command
            files_to_recheck
        with
        | exn ->
          let exn = Exception.wrap exn in
          let%lwt () = stop_parallelizable_workloads () in
          Exception.reraise exn
      in
      let%lwt () = stop_parallelizable_workloads () in

      (* Now that the recheck is done, it's safe to retry deferred parallelizable workloads *)
      ServerMonitorListenerState.requeue_deferred_parallelizable_workloads ();
      Lwt.return (Completed_recheck { profiling; env; recheck_count })
    in

    (* adding files_to_force to will_be_checked_files makes sure that future requests for
        the same files doesn't cause us to cancel a check that was already working on
        checking those files (see [get_forced]). note: will_be_checked_files is also passed
        into [recheck] and mutated further when we determine what to recheck, but forced
        files are definitely checked, so we can add them now. *)
    will_be_checked_files := CheckedSet.union files_to_force !will_be_checked_files;

    run_but_cancel_on_file_changes
      ~options
      env
      ~get_forced
      ~priority
      ~f
      ~pre_cancel:stop_parallelizable_workloads
      ~post_cancel

let recheck_loop =
  (* It's not obvious to Mr Gabe how we should merge together the profiling info from multiple
   * rechecks. But something is better than nothing... *)
  let rec loop ~profiling genv env =
    let should_print_summary = Options.should_profile genv.options in
    let%lwt (recheck_series_profiling, recheck_result) =
      (* We are intentionally logging just the details around a single recursive `recheck_single`
       * call so as to include only a series of canceled rechecks and the eventual completed one. *)
      Profiling_js.with_profiling_lwt
        ~label:"RecheckSeries"
        ~should_print_summary
        (fun _profiling -> recheck_single ~recheck_count:1 genv env
      )
    in
    match recheck_result with
    | Nothing_to_do env -> Lwt.return (List.rev profiling, env)
    | Completed_recheck { profiling = recheck_profiling; env; recheck_count } ->
      FlowEventLogger.recheck_series ~profiling:recheck_series_profiling ~recheck_count;
      (* We just finished a recheck. Let's see if there's any more stuff to recheck *)
      loop ~profiling:(recheck_profiling :: profiling) genv env
  in
  (fun genv env -> loop ~profiling:[] genv env)
