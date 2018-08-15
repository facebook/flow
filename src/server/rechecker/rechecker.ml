(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

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
    Persistent_connection.send_exit env.connections exit_status;
    Hh_logger.fatal "Status: Error";
    Hh_logger.fatal "%s" msg;
    FlowExitStatus.exit ~msg exit_status
  end

(* on notification, execute client commands or recheck files *)
let recheck genv env ?(files_to_focus=FilenameSet.empty) updates =
  (* Caller should have already checked this *)
  assert (not (FilenameSet.is_empty updates));

  MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
  Persistent_connection.send_start_recheck env.connections;
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in

  let%lwt profiling, summary, env =
    Types_js.recheck ~options ~workers ~updates env ~files_to_focus in

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
  let run_thread = f () in
  let cancel_thread =
    let%lwt () =
      if Options.enable_cancelable_rechecks genv.ServerEnv.options
      then begin
        let%lwt () = ServerMonitorListenerState.wait_for_updates_for_recheck ~process_updates in
        Hh_logger.info "Canceling due to new file changes";
        Lwt.cancel run_thread;
        Lwt.return_unit
      end else
        Lwt.return_unit
    in
    Lwt.return_unit
  in
  try%lwt
    let%lwt ret = run_thread in
    Lwt.cancel cancel_thread;
    Lwt.return ret
  with Lwt.Canceled ->
    on_cancel ()

(* Perform a single recheck. This will incorporate any pending changes from the file watcher.
 * If any file watcher notifications come in during the recheck, it will be canceled and restarted
 * to include the new changes *)
let rec recheck_single
    ?(files_to_recheck=FilenameSet.empty)
    ?(files_to_focus=FilenameSet.empty)
    genv env =
  let open ServerMonitorListenerState in
  let env = update_env env in
  let process_updates = process_updates genv env in
  let workload = get_and_clear_recheck_workload ~process_updates in
  let files_to_recheck = FilenameSet.union files_to_recheck workload.files_to_recheck in
  let files_to_focus = FilenameSet.union files_to_focus workload.files_to_focus in
  let all_files = FilenameSet.union files_to_recheck files_to_focus in
  if FilenameSet.is_empty all_files
  then begin
    List.iter (fun callback -> callback None) workload.profiling_callbacks;
    Lwt.return (Error env) (* Nothing to do *)
  end else
    let on_cancel () =
      Hh_logger.info
        "Recheck successfully canceled. Restarting the recheck to include new file changes";
      recheck_single ~files_to_recheck:all_files ~files_to_focus genv env
    in
    let f () =
      let%lwt profiling, env = recheck genv env ~files_to_focus all_files in
      List.iter (fun callback -> callback (Some profiling)) workload.profiling_callbacks;
      Lwt.return (Ok (profiling, env))
    in

    run_but_cancel_on_file_changes genv env ~f ~on_cancel

let rec recheck_loop
    ?(files_to_recheck=FilenameSet.empty)
    ?(files_to_focus=FilenameSet.empty)
    genv env =
  match%lwt recheck_single ~files_to_recheck ~files_to_focus genv env with
  | Error env ->
    (* No more work to do for now *)
    Lwt.return env
  | Ok (_profiling, env) ->
    (* We just finished a recheck. Let's see if there's any more stuff to recheck *)
    recheck_loop genv env
