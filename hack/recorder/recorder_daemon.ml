(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let marshal_events events =
  List.iter (fun e -> Marshal.to_channel stdout e []) events

let clean_exit recorder =
  let events = Recorder.get_events recorder in
  marshal_events events;
  exit 0

let rec read_and_record recorder d_in =
  let event = try Debug_port.read d_in with
    | Debug_port.Port_closed ->
      Hh_logger.log "Port closed abruptly. Flushing recording and exiting";
      clean_exit recorder
  in
  let recorder = Recorder.add_event event recorder in
  if Recorder.is_finished recorder
  then begin
    Hh_logger.log "Recording finished. Flushing recording and exiting";
    clean_exit recorder
  end else
    read_and_record recorder d_in

let restore_relative_path_prefixes init_env =
  let open Relative_path in
  let open Recorder_types in
  set_path_prefix Root init_env.root_path;
  set_path_prefix Hhi init_env.hhi_path

(** Type annotations are required here because we neve resolve them.
 * Consider actually using these phantom types.*)
let daemon_main init_env (ic, (_oc: unit Daemon.out_channel)) =
  Printexc.record_backtrace true;
  restore_relative_path_prefixes init_env;
  Hh_logger.log "Started recording";
  let d_port = Debug_port.in_port_of_in_channel ic in
  read_and_record (Recorder.start init_env) d_port

let entry =
  Daemon.register_entry_point "Recorder_daemon.daemon_main" daemon_main

let maybe_rename_old_log_link log_link =
  try Sys.rename log_link (log_link ^ ".old") with _ -> ()

(** Retire the old sym link, create a new timestamped file, point the
 * link to that new file, and return a file descriptor to this file. *)
let new_file_from_link link =
  maybe_rename_old_log_link link;
  let file = Sys_utils.make_link_of_timestamped link in
  Daemon.fd_of_path file

let start_daemon output_fn log_link =
  let log_fd = new_file_from_link log_link in
  let out_fd = new_file_from_link output_fn in
  let init_env = {
    Recorder_types.root_path = Path.make Relative_path.(path_of_prefix Root);
    hhi_path = Path.make Relative_path.(path_of_prefix Hhi);
  } in
  Hh_logger.log
    "About to spawn recorder daemon. Output will go to %s. Logs to %s.\n"
    output_fn log_link;
  Daemon.spawn
    (** This doesn't work in `socket mode. The recorder daemon doesn't
     * see EOF when the serve exits, and just ends up waiting forever. No
     * idea why. *)
    ~channel_mode:`pipe
    (out_fd, log_fd)
    entry
    init_env
