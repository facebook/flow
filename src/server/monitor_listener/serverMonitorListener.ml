(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv

let kill_workers () =
  (* TODO - find a way to gracefully kill the workers. At the moment, if the workers are in the
     * middle of a job this will lead to some log spew. We probably should send SIGTERM to each
     * worker and set up a signal handler to kill the fork and exit gracefully. Might also want
     * to use the SharedMem_js.cancel thingy *)
  Hh_logger.info "Killing the worker processes";
  WorkerController.killall ()

let handle_message genv = function
  | MonitorProt.Request (request_id, command) ->
    CommandHandler.enqueue_or_handle_ephemeral genv (request_id, command)
  | MonitorProt.PersistentConnectionRequest (client_id, request) ->
    CommandHandler.enqueue_persistent genv client_id request
  | MonitorProt.NewPersistentConnection (client_id, lsp_init_params) ->
    (* Immediately register the new client *)
    Persistent_connection.add_client client_id lsp_init_params;
    ServerMonitorListenerState.push_new_env_update (fun env ->
        {
          env with
          connections = Persistent_connection.add_client_to_clients env.connections client_id;
        }
    )
  | MonitorProt.DeadPersistentConnection client_id ->
    (* Immediately remove the dead client *)
    Persistent_connection.remove_client client_id;
    ServerMonitorListenerState.push_new_env_update (fun env ->
        {
          env with
          connections = Persistent_connection.remove_client_from_clients env.connections client_id;
        }
    )
  | MonitorProt.FileWatcherNotification { files = changed_files; metadata; initial } ->
    let open LspProt in
    let file_count = SSet.cardinal changed_files in
    if initial then
      let reason = Lazy_init_typecheck in
      ServerMonitorListenerState.push_files_to_force_focused_and_recheck ~reason changed_files
    else
      let reason =
        match metadata with
        | Some { MonitorProt.changed_mergebase = Some true; _ } -> Rebased { file_count }
        | Some { MonitorProt.missed_changes = true; _ } -> File_watcher_missed_changes
        | _ when file_count = 1 ->
          Single_file_changed { filename = SSet.elements changed_files |> List.hd }
        | _ -> Many_files_changed { file_count }
      in
      ServerMonitorListenerState.push_files_to_recheck ?metadata ~reason changed_files
  | MonitorProt.PleaseDie please_die_reason ->
    kill_workers ();
    let msg =
      match please_die_reason with
      | MonitorProt.MonitorExiting (monitor_exit_status, monitor_msg) ->
        Utils_js.spf
          "Monitor is exiting with status %s (%s)"
          (Exit.to_string monitor_exit_status)
          monitor_msg
    in
    Exit.(exit ~msg Killed_by_monitor)

(** This is a thread that keeps reading messages from the monitor process and
    adding them to a stream. It runs in parallel with the [Server.serve] loop, which
    consumes the stream. *)
let rec listen_for_messages genv =
  (* read a message from the monitor *)
  let%lwt message =
    try%lwt MonitorRPC.read () with
    | End_of_file ->
      let () = kill_workers () in
      let msg = "Connection to monitor closed unexpectedly" in
      Exit.(exit ~msg Killed_by_monitor)
  in
  let () = handle_message genv message in
  listen_for_messages genv
