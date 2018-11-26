(**
 * Copyright (c) 2013-present, Facebook, Inc.
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
    CommandHandler.enqueue_persistent genv client_id request;
    Lwt.return_unit
  | MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) ->
    ServerMonitorListenerState.push_new_env_update (fun env -> { env with
      connections = Persistent_connection.add_client env.connections client_id logging_context lsp
    });
    Lwt.return_unit
  | MonitorProt.DeadPersistentConnection client_id ->
    ServerMonitorListenerState.push_new_env_update (fun env -> { env with
      connections = Persistent_connection.remove_client env.connections client_id
    });
    Lwt.return_unit
  | MonitorProt.FileWatcherNotification (changed_files, metadata) ->
    ServerMonitorListenerState.push_files_to_recheck ?metadata changed_files;
    Lwt.return_unit
  | MonitorProt.PleaseDie please_die_reason ->
    (* TODO - find a way to gracefully kill the workers. At the moment, if the workers are in the
     * middle of a job this will lead to some log spew. We probably should send SIGTERM to each
     * worker and set up a signal handler to kill the fork and exit gracefully. Might also want
     * to use the SharedMem.cancel thingy *)
    Hh_logger.info "Killing the worker processes";
    WorkerController.killall ();
    let msg = match please_die_reason with
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
