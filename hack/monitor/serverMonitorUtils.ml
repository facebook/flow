(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type monitor_config =
  {
    (** The socket file on which the monitor is listening for connections. *)
    socket_file: string;
    (** This lock is held when a monitor is alive. *)
    lock_file: string;
    (** The path to the server log file *)
    server_log_file: string;
    (** The path to the monitor log file *)
    monitor_log_file: string;
    (** The path to the load script log file *)
    load_script_log_file: string;
  }

module type Server_config = sig

  type server_start_options

  (** Start the server. Optionally takes in the exit code of the previously
   * running server that exited. *)
  val start_server :
    informant_managed:bool ->
    prior_exit_status:(int option) ->
    server_start_options ->
    ServerProcess.process_data

  (** Callback to run when server exits *)
  val on_server_exit : monitor_config -> unit
end

type build_mismatch_info =
  {
    existing_version: string;
    existing_build_commit_time: string;
    existing_argv: string list;
    existing_launch_time: float;
  }

let current_build_info =
  {
    existing_version = Build_id.build_revision;
    existing_build_commit_time = Build_id.get_build_commit_time_string ();
    existing_argv = Array.to_list Sys.argv;
    existing_launch_time = Unix.gettimeofday ();
  }

type connection_error =
  (**
   * This should be rare. The monitor rapidly accepts connections and does
   * the version ID check very quickly. Only under very heavy load will that
   * sequence time out. *)
  | Monitor_establish_connection_timeout
  | Server_missing
  (** There is a brief period of time after the Monitor has grabbed its
   * liveness lock and before it starts listening in on the socket
   * (which can only happen after the socket file is created). During that
   * period, either the socket file doesn't exist yet, or socket connections
   * are refused. *)
  | Monitor_socket_not_ready
  | Server_died
  (** Server dormant and can't join the (now full) queue of connections
   * waiting for the next server. *)
  | Server_dormant
  | Build_id_mismatched of build_mismatch_info option
  | Monitor_connection_failure

type connection_state =
  | Connection_ok
  | Build_id_mismatch
  | Build_id_mismatch_ex of build_mismatch_info

(** Result of a shutdown monitor RPC. *)
type shutdown_result =
  (** Request sent and channel hung up, indicating the process has exited. *)
  | SHUTDOWN_VERIFIED
  (** Request sent, but channel hasn't hung up. *)
  | SHUTDOWN_UNVERIFIED

(* The clients that connect to IDE process can either establish persistent
 * connection and talk the JSON protocol, or exchange a single request-response
 * by sending a ServerCommand *)
type ide_client_type =
  | Request
  | Persistent

let send_ide_client_type oc (t : ide_client_type)=
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc) t

(* Message we send to the --waiting-client *)
let ready = "ready"

let exit_if_parent_dead () =
(** Cross-platform compatible way; parent PID becomes 1 when parent dies. *)
  if Unix.getppid() = 1 then
    (Hh_logger.log "Server's parent has died; exiting.\n";
     Exit_status.exit Exit_status.Lost_parent_monitor);
