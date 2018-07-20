(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv

type workload = env -> env Lwt.t

(* Workloads are client requests which we processes FIFO *)
let workload_stream, push_new_workload = Lwt_stream.create ()
(* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
 * but are quick to handle *)
let env_update_stream, push_new_env_update = Lwt_stream.create ()
(* Files which have changed *)
let recheck_stream, push_files_to_recheck = Lwt_stream.create ()

(* This is a thread that just keeps looping in the background. It reads messages from the
 * monitor process and adds them to a stream *)
module ListenLoop = LwtLoop.Make (struct
  type acc = genv

  let handle_message genv = function
  | MonitorProt.Request (request_id, command) ->
    push_new_workload (Some (fun env -> CommandHandler.handle_ephemeral genv env (request_id, command)))
  | MonitorProt.PersistentConnectionRequest (client_id, request) ->
    push_new_workload (Some (fun env -> CommandHandler.handle_persistent genv env client_id request))
  | MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) ->
    push_new_env_update (Some (fun env -> { env with
      connections = Persistent_connection.add_client env.connections client_id logging_context lsp
    }))
  | MonitorProt.DeadPersistentConnection client_id ->
    push_new_env_update (Some (fun env -> { env with
      connections = Persistent_connection.remove_client env.connections client_id
    }))
  | MonitorProt.FileWatcherNotification changed_files ->
    push_files_to_recheck (Some changed_files)
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
    let%lwt message = MonitorRPC.read () in
    handle_message genv message;
    Lwt.return genv

  external reraise : exn -> 'a = "%reraise"

  let catch _ exn = reraise exn
end)

let listen_for_messages genv = ListenLoop.run genv

let get_next_workload () =
  match Lwt_stream.get_available_up_to 1 workload_stream with
  | [ workload ] -> Some workload
  | [] -> None
  | _ -> failwith "Unreachable"

let update_env env =
  Lwt_stream.get_available env_update_stream
  |> List.fold_left (fun env f -> f env) env

let recheck_acc = ref Utils_js.FilenameSet.empty
let recheck_fetch genv env =
  recheck_acc :=
    Lwt_stream.get_available recheck_stream (* Get all the files which have changed *)
    |> List.fold_left SSet.union SSet.empty (* Flatten the set *)
    |> Rechecker.process_updates genv env (* Process the changes *)
    |> Utils_js.FilenameSet.union (!recheck_acc) (* Union them with the acc *)
let get_updates_for_recheck genv env =
  recheck_fetch genv env;
  let files = !recheck_acc in
  recheck_acc := Utils_js.FilenameSet.empty;
  files
let rec wait_for_updates_for_recheck genv env =
  let%lwt _ = Lwt_stream.is_empty recheck_stream in
  recheck_fetch genv env;
  if Utils_js.FilenameSet.is_empty !recheck_acc
  then wait_for_updates_for_recheck genv env
  else Lwt.return_unit

(* Block until any stream receives something *)
let wait_for_anything genv env =
  let%lwt () = Lwt.pick [
    (let%lwt _ = Lwt_stream.is_empty workload_stream in Lwt.return_unit);
    (let%lwt _ = Lwt_stream.is_empty env_update_stream in Lwt.return_unit);
    (let%lwt _ = Lwt_stream.is_empty recheck_stream in Lwt.return_unit);
    wait_for_updates_for_recheck genv env;
  ] in
  Lwt.return_unit
