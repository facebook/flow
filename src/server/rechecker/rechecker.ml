(**
 * Copyright (c) 2013-present, Facebook, Inc.
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
    let () = match Lwt.state wait_for_cancel with
    | Lwt.Return () -> raise Lwt.Canceled
    | Lwt.Fail exn -> let exn = Exception.wrap exn in Exception.reraise exn
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
    let%lwt workload = Lwt.pick [
      ServerMonitorListenerState.wait_and_pop_parallelizable_workload ();
      (let%lwt () = wait_for_cancel in raise Lwt.Canceled)
    ] in

    (* We have a workload! Let's run it! *)
    Hh_logger.info "Running a parallel workload";
    let%lwt () = workload env in

    Lwt.return (wait_for_cancel, env)

  let catch _ exn =
    let exn = Exception.wrap exn in
    Exception.reraise exn
end)

let with_parallelizable_workloads env f =
  (* The wait_for_cancel thread itself is NOT cancelable *)
  let wait_for_cancel, wakener = Lwt.wait () in
  let loop_thread = Parallelizable_workload_loop.run (wait_for_cancel, env) in
  let%lwt ret = f () in
  (* Tell the loop to cancel at its earliest convinience *)
  Lwt.wakeup wakener ();
  (* Wait for the loop to finish *)
  let%lwt () = loop_thread in
  Lwt.return ret

let get_lazy_stats genv env =
  { ServerProt.Response.
    lazy_mode = Options.lazy_mode genv.options;
    checked_files = CheckedSet.all env.checked_files |> FilenameSet.cardinal;
    total_files = FilenameSet.cardinal env.files;
  }

(* Filter a set of updates coming from the file watcher or force-recheck or wherever and return a
 * FilenameSet. Updates may be coming in from the root, or an include path.
 *
 * If any update can't be processed incrementally, the Flow server will exit *)
let process_updates genv env updates =
  let open Recheck_updates in
  match process_updates ~options:genv.ServerEnv.options ~libs:env.ServerEnv.libs updates with
  | Core_result.Ok updates -> updates
  | Core_result.Error { msg; exit_status } -> begin
    Hh_logger.fatal "Status: Error";
    Hh_logger.fatal "%s" msg;
    FlowExitStatus.exit ~msg exit_status
  end

let files_currently_being_forced_by_a_recheck = ref CheckedSet.empty
let forced_files_after_recheck env () =
  CheckedSet.union env.checked_files (!files_currently_being_forced_by_a_recheck)

(* on notification, execute client commands or recheck files *)
let recheck genv env ?(files_to_force=CheckedSet.empty) ~file_watcher_metadata updates =
  (* Caller should have already checked this *)
  assert (not (FilenameSet.is_empty updates && CheckedSet.is_empty files_to_force));

  files_currently_being_forced_by_a_recheck := files_to_force;

  MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
  Persistent_connection.send_start_recheck env.connections;
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in

  let%lwt profiling, summary, env =
    Types_js.recheck ~options ~workers ~updates env ~files_to_force ~file_watcher_metadata
  in

  let lazy_stats = get_lazy_stats genv env in
  Persistent_connection.send_end_recheck ~lazy_stats env.connections;
  (* We must send "end_recheck" prior to sending errors+warnings so the client *)
  (* knows that this set of errors+warnings are final ones, not incremental.   *)
  let calc_errors_and_warnings () =
    let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
    errors, warnings
  in
  Persistent_connection.update_clients ~clients:env.connections ~calc_errors_and_warnings;

  MonitorRPC.status_update ~event:(ServerStatus.Finishing_up summary);
  Lwt.return (profiling, env)

(* Runs a function which should be canceled if we are notified about any file changes. After the
 * thread is canceled, on_cancel is called and its result returned *)
let run_but_cancel_on_file_changes genv env ~f ~on_cancel =
  let process_updates = process_updates genv env in
  (* We don't want to start running f until we're in the try block *)
  let waiter, wakener = Lwt.task () in
  let run_thread = let%lwt () = waiter in f () in
  let get_forced = forced_files_after_recheck env in
  let cancel_thread =
    let%lwt () =
      ServerMonitorListenerState.wait_for_updates_for_recheck ~process_updates ~get_forced
    in
    Hh_logger.info "Canceling since a recheck is needed";
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
    on_cancel ()

(* Perform a single recheck. This will incorporate any pending changes from the file watcher.
 * If any file watcher notifications come in during the recheck, it will be canceled and restarted
 * to include the new changes *)
let rec recheck_single
    ?(files_to_recheck=FilenameSet.empty)
    ?(files_to_force=CheckedSet.empty)
    ?(file_watcher_metadata=MonitorProt.empty_file_watcher_metadata)
    genv env =
  let open ServerMonitorListenerState in
  let env = update_env env in
  let process_updates = process_updates genv env in
  let get_forced = forced_files_after_recheck env in
  let workload = get_and_clear_recheck_workload ~process_updates ~get_forced in
  let files_to_recheck = FilenameSet.union files_to_recheck workload.files_to_recheck in
  let files_to_force = CheckedSet.union files_to_force workload.files_to_force in
  let file_watcher_metadata =
    MonitorProt.merge_file_watcher_metadata file_watcher_metadata workload.metadata
  in
  if FilenameSet.is_empty files_to_recheck && CheckedSet.is_empty files_to_force
  then begin
    List.iter (fun callback -> callback None) workload.profiling_callbacks;
    Lwt.return (Error env) (* Nothing to do *)
  end else
    let on_cancel () =
      Hh_logger.info
        "Recheck successfully canceled. Restarting the recheck to include new file changes";
      recheck_single ~files_to_recheck ~files_to_force ~file_watcher_metadata genv env
    in
    let f () =
      let%lwt profiling, env = with_parallelizable_workloads env @@ fun () ->
        recheck genv env ~files_to_force ~file_watcher_metadata files_to_recheck
      in
      List.iter (fun callback -> callback (Some profiling)) workload.profiling_callbacks;
      (* Now that the recheck is done, it's safe to retry deferred parallelizable workloads *)
      ServerMonitorListenerState.requeue_deferred_parallelizable_workloads ();
      Lwt.return (Ok (profiling, env))
    in

    run_but_cancel_on_file_changes genv env ~f ~on_cancel

let recheck_loop =
  (* It's not obvious to Mr Gabe how we should merge together the profiling info from multiple
   * rechecks. But something is better than nothing... *)
  let rec loop
      ?(files_to_recheck=FilenameSet.empty)
      ?(files_to_force=CheckedSet.empty)
      ?(profiling=[])
      genv env =
    match%lwt recheck_single ~files_to_recheck ~files_to_force genv env with
    | Error env ->
      (* No more work to do for now *)
      Lwt.return (List.rev profiling, env)
    | Ok (recheck_profiling, env) ->
      (* We just finished a recheck. Let's see if there's any more stuff to recheck *)
      loop ~profiling:(recheck_profiling::profiling) genv env
  in

  fun genv env ->
    loop genv env
