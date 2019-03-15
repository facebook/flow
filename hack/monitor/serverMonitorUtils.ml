(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
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
  }

(** In an Informant-directed restart, Watchman provided a new
 * mergebase, a new clock, and a list of files changed w.r.t.
 * that mergebase.
 *
 * A new server instance can "resume" from that new mergebase
 * given that it handles the list of files changed w.r.t. that
 * new mergebase, and just starts a watchman subscription
 * beginning with that clock.
 *)
type watchman_mergebase = {
  (** Watchman says current repo mergebase is this. *)
  mergebase_global_rev : int;
  (** ... plus these files changed to represent its current state *)
  files_changed : SSet.t;
  (** ...as of this clock *)
  watchman_clock : string;
}

(** Informant-induced restart may specify the saved state
 * we should load from. *)
type target_saved_state = {
  saved_state_everstore_handle : string;
  (** The global revision to which the above handle corresponds to. *)
  target_global_rev : int;
  watchman_mergebase : watchman_mergebase option;
}

let watchman_mergebase_to_string { mergebase_global_rev; files_changed; watchman_clock; } =
  Printf.sprintf
    "watchman_mergebase (mergebase_global_rev: %d; files_changed count: %d; watchman_clock: %s)"
    mergebase_global_rev
    (SSet.cardinal files_changed)
    watchman_clock

module type Server_config = sig

  type server_start_options

  (** Start the server. Optionally takes in the exit code of the previously
   * running server that exited. *)
  val start_server :
    ?target_saved_state:target_saved_state ->
    informant_managed:bool ->
    prior_exit_status:(int option) ->
    server_start_options ->
    ServerProcess.process_data

  val kill_server : ServerProcess.process_data -> unit

  val wait_for_server_exit : ServerProcess.process_data ->
    float (** Kill signal time *) ->
    unit

  val wait_pid : ServerProcess.process_data -> int * Unix.process_status

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
  | Server_dormant_out_of_retries
  | Build_id_mismatched of build_mismatch_info option
  | Monitor_connection_failure

type connection_state =
  | Connection_ok
  (* Build_is_mismatch is never used, but it can't be removed, because *)
  (* the sequence of constructors here is part of the binary protocol  *)
  (* we want to support between mismatched versions of client_server.  *)
  | Build_id_mismatch
  (* Build_id_mismatch_ex *is* used. *)
  | Build_id_mismatch_ex of build_mismatch_info

(** Result of a shutdown monitor RPC. *)
type shutdown_result =
  (** Request sent and channel hung up, indicating the process has exited. *)
  | SHUTDOWN_VERIFIED
  (** Request sent, but channel hasn't hung up. *)
  | SHUTDOWN_UNVERIFIED

(* Message we send to the --waiting-client *)
let ready = "ready"

let exit_if_parent_dead () =
(** Cross-platform compatible way; parent PID becomes 1 when parent dies. *)
  if Unix.getppid() = 1 then
    (Hh_logger.log "Server's parent has died; exiting.\n";
     Exit_status.exit Exit_status.Lost_parent_monitor);
