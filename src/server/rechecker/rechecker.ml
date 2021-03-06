(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

module Parallelizable_workload_loop = LwtLoop.Make (struct
  type acc = unit Lwt.t * ServerEnv.env

  let main (wait_for_cancel, env) =
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
     * Normally, the wait_and_pop_parallelizable_workload thread will finish first. Then Lwt.pick
     * will cancel the `let%lwt () = ... in raise Lwt.Canceled` thread. The `wait_for_cancel` thread
     * is NOT cancelable so that will stay unresolved.
     *
     * Eventually, wait_for_cancel will resolve. Then we'll cancel the
     * wait_and_pop_parallelizable_workload thread throw Lwt.Canceled *)
    let%lwt workload =
      Lwt.pick
        [
          ServerMonitorListenerState.wait_and_pop_parallelizable_workload ();
          (let%lwt () = wait_for_cancel in
           raise Lwt.Canceled);
        ]
    in
    (* We have a workload! Let's run it! *)
    Hh_logger.info "Running a parallel workload";
    let%lwt () = workload env in
    Lwt.return (wait_for_cancel, env)

  let catch _ exn = Exception.reraise exn
end)

let start_parallelizable_workloads env =
  (* The wait_for_cancel thread itself is NOT cancelable *)
  let (wait_for_cancel, wakener) = Lwt.wait () in
  let loop_thread = Parallelizable_workload_loop.run (wait_for_cancel, env) in
  (* Allow this stop function to be called multiple times for the same loop *)
  let already_woken = ref false in
  fun () ->
    (* Tell the loop to cancel at its earliest convinience *)
    if not !already_woken then Lwt.wakeup wakener ();
    already_woken := true;

    (* Wait for the loop to finish *)
    loop_thread

let get_lazy_stats ~options env =
  {
    ServerProt.Response.lazy_mode = Options.lazy_mode options;
    checked_files = CheckedSet.all env.checked_files |> FilenameSet.cardinal;
    total_files = FilenameSet.cardinal env.files;
  }

(* Filter a set of updates coming from the file watcher or force-recheck or wherever and return a
 * FilenameSet. Updates may be coming in from the root, or an include path.
 *
 * If any update can't be processed incrementally, the Flow server will exit *)
let process_updates ?skip_incompatible ~options env updates =
  match
    Recheck_updates.process_updates ?skip_incompatible ~options ~libs:env.ServerEnv.libs updates
  with
  | Base.Result.Ok updates -> updates
  | Base.Result.Error { Recheck_updates.msg; exit_status } ->
    Hh_logger.fatal "Status: Error";
    Hh_logger.fatal "%s" msg;
    FlowExitStatus.exit ~msg exit_status

(* on notification, execute client commands or recheck files *)
let recheck
    genv
    env
    ?(files_to_force = CheckedSet.empty)
    ~file_watcher_metadata
    ~recheck_reasons
    ~will_be_checked_files
    updates =
  (* Caller should have already checked this *)
  assert (not (FilenameSet.is_empty updates && CheckedSet.is_empty files_to_force));

  MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
  Persistent_connection.send_start_recheck env.connections;
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  let%lwt (profiling, (log_recheck_event, summary_info, env)) =
    let should_print_summary = Options.should_profile options in
    Profiling_js.with_profiling_lwt ~label:"Recheck" ~should_print_summary (fun profiling ->
        let%lwt (log_recheck_event, summary_info, env) =
          Types_js.recheck
            ~profiling
            ~options
            ~workers
            ~updates
            env
            ~files_to_force
            ~file_watcher_metadata
            ~recheck_reasons
            ~will_be_checked_files
        in
        let lazy_stats = get_lazy_stats ~options env in
        Persistent_connection.send_end_recheck ~lazy_stats env.connections;

        (* We must send "end_recheck" prior to sending errors+warnings so the client *)
        (* knows that this set of errors+warnings are final ones, not incremental.   *)
        let calc_errors_and_warnings () =
          let reader = State_reader.create () in
          let (errors, warnings, _) =
            ErrorCollator.get_with_separate_warnings ~profiling ~reader ~options env
          in
          (errors, warnings)
        in
        let errors_reason = LspProt.End_of_recheck { recheck_reasons } in
        Persistent_connection.update_clients
          ~clients:env.connections
          ~errors_reason
          ~calc_errors_and_warnings;
        Lwt.return (log_recheck_event, summary_info, env))
  in
  log_recheck_event ~profiling;

  let duration = Profiling_js.get_profiling_duration profiling in
  let summary = ServerStatus.{ duration; info = summary_info } in

  MonitorRPC.status_update ~event:(ServerStatus.Finishing_up summary);
  Lwt.return (profiling, env)

(* Runs a function which should be canceled if we are notified about any file changes. After the
 * thread is canceled, post_cancel is called and its result returned *)
let run_but_cancel_on_file_changes ~options env ~get_forced ~f ~pre_cancel ~post_cancel =
  let process_updates ?skip_incompatible = process_updates ?skip_incompatible ~options env in
  (* We don't want to start running f until we're in the try block *)
  let (waiter, wakener) = Lwt.task () in
  let run_thread =
    let%lwt () = waiter in
    f ()
  in
  let cancel_thread =
    let%lwt () =
      ServerMonitorListenerState.wait_for_updates_for_recheck ~process_updates ~get_forced
    in
    Hh_logger.info "Canceling since a recheck is needed";
    let%lwt () = pre_cancel () in
    Lwt.cancel run_thread;
    Lwt.return_unit
  in
  try%lwt
    Lwt.wakeup wakener ();
    let%lwt ret = run_thread in
    Lwt.cancel cancel_thread;
    Lwt.return ret
  with Lwt.Canceled ->
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
 * to include the new changes *)
let rec recheck_single
    ?(files_to_recheck = FilenameSet.empty)
    ~files_to_force
    ?(file_watcher_metadata = MonitorProt.empty_file_watcher_metadata)
    ?(recheck_reasons_list_rev = [])
    ?((* The number of rechecks in this series, including past canceled rechecks and the current
       * one. *)
    recheck_count = 1)
    genv
    env =
  ServerMonitorListenerState.(
    let env = update_env env in
    let options = genv.ServerEnv.options in
    let process_updates ?skip_incompatible = process_updates ?skip_incompatible ~options env in
    (* This ref is an estimate of the files which will be checked by the time the recheck is done.
     * As the recheck progresses, the estimate will get better. We use this estimate to prevent
     * canceling the recheck to force a file which we were already going to check
     *
     * This early estimate is not a very good estimate, since it's missing new dependents and
     * dependencies. However it should be good enough to prevent rechecks continuously restarting as
     * the server gets spammed with autocomplete requests *)
    let will_be_checked_files = ref (CheckedSet.union env.ServerEnv.checked_files files_to_force) in
    let get_forced () = !will_be_checked_files in
    let workload = get_and_clear_recheck_workload ~process_updates ~get_forced in
    let files_to_recheck = FilenameSet.union files_to_recheck workload.files_to_recheck in
    let files_to_force = CheckedSet.union files_to_force workload.files_to_force in
    let file_watcher_metadata =
      MonitorProt.merge_file_watcher_metadata file_watcher_metadata workload.metadata
    in
    let recheck_reasons_list_rev = workload.recheck_reasons_rev :: recheck_reasons_list_rev in
    if FilenameSet.is_empty files_to_recheck && CheckedSet.is_empty files_to_force then (
      List.iter (fun callback -> callback None) workload.profiling_callbacks;
      Lwt.return (Nothing_to_do env)
    ) else
      (* Start the parallelizable workloads loop and return a function which will stop the loop *)
      let stop_parallelizable_workloads = start_parallelizable_workloads env in
      let post_cancel () =
        Hh_logger.info
          "Recheck successfully canceled. Restarting the recheck to include new file changes";
        (* The canceled recheck, or a preceding sequence of canceled rechecks where none completed,
         * may have introduced garbage into shared memory. Since we immediately start another
         * recheck, we should first check whether we need to compact. Otherwise, sharedmem could
         * potentially grow unbounded.
         *
         * The constant budget provided here should be sufficient to fully scan a 25G heap within 5
         * iterations. We want to avoid the scenario where repeatedly cancelled rechecks cause the
         * heap to grow faster than we can scan. An algorithmic approach to determine the amount of
         * work based on the allocation rate would be better. *)
        let _done : bool = SharedMem.collect_slice 20000000 in
        recheck_single
          ~files_to_recheck
          ~files_to_force
          ~file_watcher_metadata
          ~recheck_reasons_list_rev
          ~recheck_count:(recheck_count + 1)
          genv
          env
      in
      let f () =
        (* Take something like [[10, 9], [8], [7], [6,5,4,3], [2,1]] and output [1,2,3,4,5,6,7,8,9,10]
         *)
        let recheck_reasons =
          List.fold_left
            (fun recheck_reasons recheck_reasons_rev ->
              List.rev_append recheck_reasons_rev recheck_reasons)
            []
            recheck_reasons_list_rev
        in
        let%lwt (profiling, env) =
          try%lwt
            recheck
              genv
              env
              ~files_to_force
              ~file_watcher_metadata
              ~recheck_reasons
              ~will_be_checked_files
              files_to_recheck
          with exn ->
            let exn = Exception.wrap exn in
            let%lwt () = stop_parallelizable_workloads () in
            Exception.reraise exn
        in
        let%lwt () = stop_parallelizable_workloads () in
        List.iter (fun callback -> callback (Some profiling)) workload.profiling_callbacks;

        (* Now that the recheck is done, it's safe to retry deferred parallelizable workloads *)
        ServerMonitorListenerState.requeue_deferred_parallelizable_workloads ();
        Lwt.return (Completed_recheck { profiling; env; recheck_count })
      in
      run_but_cancel_on_file_changes
        ~options
        env
        ~get_forced
        ~f
        ~pre_cancel:stop_parallelizable_workloads
        ~post_cancel)

let recheck_loop =
  (* It's not obvious to Mr Gabe how we should merge together the profiling info from multiple
   * rechecks. But something is better than nothing... *)
  let rec loop ~profiling genv env =
    let files_to_recheck = FilenameSet.empty in
    let files_to_force = CheckedSet.empty in
    let should_print_summary = Options.should_profile genv.options in
    let%lwt (recheck_series_profiling, recheck_result) =
      (* We are intentionally logging just the details around a single recursive `recheck_single`
       * call so as to include only a series of canceled rechecks and the eventual completed one. *)
      Profiling_js.with_profiling_lwt
        ~label:"RecheckSeries"
        ~should_print_summary
        (fun _profiling -> recheck_single ~files_to_recheck ~files_to_force genv env)
    in
    match recheck_result with
    | Nothing_to_do env -> Lwt.return (List.rev profiling, env)
    | Completed_recheck { profiling = recheck_profiling; env; recheck_count } ->
      FlowEventLogger.recheck_series ~profiling:recheck_series_profiling ~recheck_count;
      (* We just finished a recheck. Let's see if there's any more stuff to recheck *)
      loop ~profiling:(recheck_profiling :: profiling) genv env
  in
  (fun genv env -> loop ~profiling:[] genv env)

let recheck_single ~files_to_force genv env = recheck_single ~files_to_force genv env
