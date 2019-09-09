(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv

(* This is a thread that just keeps looping in the background. It reads messages from the
 * monitor process and adds them to a stream *)
module ListenLoop = LwtLoop.Make (struct
  type acc = genv

  let handle_message genv = function
    | MonitorProt.Request (request_id, command) ->
      CommandHandler.enqueue_or_handle_ephemeral genv (request_id, command)
    | MonitorProt.PersistentConnectionRequest (client_id, request) ->
      CommandHandler.enqueue_persistent genv client_id request
    | MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) ->
      (* Immediately register the new client *)
      Persistent_connection.add_client client_id logging_context lsp;
      ServerMonitorListenerState.push_new_env_update (fun env ->
          {
            env with
            connections = Persistent_connection.add_client_to_clients env.connections client_id;
          });
      Lwt.return_unit
    | MonitorProt.DeadPersistentConnection client_id ->
      (* Immediately remove the dead client *)
      Persistent_connection.remove_client client_id;
      ServerMonitorListenerState.push_new_env_update (fun env ->
          {
            env with
            connections =
              Persistent_connection.remove_client_from_clients env.connections client_id;
          });
      Lwt.return_unit
    | MonitorProt.FileWatcherNotification (changed_files, metadata) ->
      let file_count = SSet.cardinal changed_files in
      let reason =
        Persistent_connection_prot.(
          match metadata with
          | Some { MonitorProt.changed_mergebase = true; total_update_distance } ->
            Rebased { distance = total_update_distance; file_count }
          | _ when file_count = 1 ->
            Single_file_changed { filename = SSet.elements changed_files |> List.hd }
          | _ -> Many_files_changed { file_count })
      in
      ServerMonitorListenerState.push_files_to_recheck ?metadata ~reason changed_files;
      Lwt.return_unit
    | MonitorProt.PleaseDie please_die_reason ->
      (* TODO - find a way to gracefully kill the workers. At the moment, if the workers are in the
       * middle of a job this will lead to some log spew. We probably should send SIGTERM to each
       * worker and set up a signal handler to kill the fork and exit gracefully. Might also want
       * to use the SharedMem_js.cancel thingy *)
      Hh_logger.info "Killing the worker processes";
      WorkerController.killall ();
      let msg =
        match please_die_reason with
        | MonitorProt.MonitorExiting (monitor_exit_status, monitor_msg) ->
          Utils.spf
            "Monitor is exiting with status %s (%s)"
            (FlowExitStatus.to_string monitor_exit_status)
            monitor_msg
      in
      FlowExitStatus.(exit ~msg Killed_by_monitor)

  let main genv =
    (* read a message from the monitor *)
    let%lwt message = MonitorRPC.read () in
    let%lwt () = handle_message genv message in
    Lwt.return genv

  external reraise : exn -> 'a = "%reraise"

  let catch _ exn = reraise exn
end)

let listen_for_messages genv = ListenLoop.run genv
