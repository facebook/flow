(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(**
 * Hack for HipHop: type checker's server monitor code.
 *
 * This runs hh server in 1 of 3 ways:
   1) Runs hh server in-process (check mode)
   2) Runs a ServerMonitor in-process which will pass connections to a
      daemonized hh server (non-detached mode).
   3) Daemonizes a ServerMonitor which will pass connections to a daemonized
      hh_server (detached mode).
*)

open HhServerMonitorConfig
module SP = ServerProcess

let start_server options name log_link daemon_entry =
  let log_file, log_mode =
    if ServerArgs.should_detach options then begin
      (try Sys.rename log_link (log_link ^ ".old") with _ -> ());
      let log_file = Sys_utils.make_link_of_timestamped log_link in
      Hh_logger.log "About to spawn %s daemon. Logs will go to %s\n%!"
        name (if Sys.win32 then log_file else log_link);
      log_file, Daemon.Log_file
    end else begin
      Hh_logger.log "About to spawn %s daemon. Logs will go here." name;
      "", Daemon.Parent_streams
    end
  in
  let start_t = Unix.time () in
  let {Daemon.pid; Daemon.channels = (ic, oc)} =
    Daemon.spawn ~channel_mode:`socket ~log_file ~log_mode daemon_entry
      options in
  Hh_logger.log "Just started %s server with pid: %d." name pid;
  let server =
    SP.({
      pid = pid;
      name = name;
      in_fd = Daemon.descr_of_in_channel ic;
      out_fd = Daemon.descr_of_out_channel oc;
      log_file = log_file;
      start_t = start_t;
      last_request_handoff = ref (Unix.time());
    }) in
  server

let start_hh_server options =
  let log_link = ServerFiles.log_link (ServerArgs.root options) in
  start_server options Program.hh_server log_link ServerMain.entry

let start_ide_server options =
  let log_link = ServerFiles.ide_log_link (ServerArgs.root options) in
  start_server options Program.ide_server log_link IdeMain.entry

(** Main method of the server monitor daemon. The daemon is responsible for
 * listening to socket requests from hh_client, checking Build ID, and relaying
 * requests to the typechecker process. *)
let monitor_daemon_main (options: ServerArgs.options) =
  if Sys_utils.is_test_mode ()
  then EventLogger.init (Daemon.devnull ()) 0.0
  else HackEventLogger.init_monitor (ServerArgs.root options)
      (Unix.gettimeofday ());
  Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
  let www_root = (ServerArgs.root options) in
  ignore @@ Sys_utils.setsid ();
  (** Make sure to lock the lockfile before doing *anything*, especially
   * opening the socket. *)
  let lock_file = ServerFiles.lock_file www_root in
  if ServerArgs.check_mode options then
    ServerMain.run_once options
  else
    if not (Lock.grab lock_file) then
      (Hh_logger.log "Monitor daemon already running. Killing";
       Exit_status.exit Exit_status.Ok);
    let hh_server_monitor_starter = begin fun () ->
      let typechecker = start_hh_server options in
      let ide = start_ide_server options in
      [typechecker; ide]
    end in
    ServerMonitor.start_monitoring ServerMonitorUtils.({
      socket_file = ServerFiles.socket_file www_root;
      lock_file = ServerFiles.lock_file www_root;
    }) hh_server_monitor_starter

let daemon_entry =
  Daemon.register_entry_point
    "monitor_daemon_main"
    (fun (options: ServerArgs.options) (_ic, _oc) ->
       monitor_daemon_main options)

(* Starts a monitor daemon if one doesn't already exist. Otherwise,
 * immediately exits with non-zero exit code. This is because the monitor
 * should never actually be attempted to be started if one is already running
 * (i.e. hh_client should play nice and only start a server monitor if one
 * isn't running by first checking the liveness lock file.) *)
let daemon_starter options =
  let root = ServerArgs.root options in
  let log_link = ServerFiles.monitor_log_link root in
  (try Sys.rename log_link (log_link ^ ".old") with _ -> ());
  let log_file_path = Sys_utils.make_link_of_timestamped log_link in
  let {Daemon.pid; _} =
    Daemon.spawn ~log_file:log_file_path daemon_entry options in
  Printf.eprintf "Spawned %s (child pid=%d)\n" Program.hh_server pid;
  Printf.eprintf "Logs will go to %s\n%!"
    (if Sys.win32 then log_file_path else log_link);
  Exit_status.Ok

(** Either starts a monitor daemon (which will spawn a typechecker daemon),
 * or just runs the typechecker if detachment not enabled. *)
let start () =
  Daemon.check_entry_point (); (* this call might not return *)
  let options = ServerArgs.parse_options () in
  if ServerArgs.should_detach options
  then Exit_status.exit (daemon_starter options)
  else monitor_daemon_main options
