(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module MC = MonitorConnection

let shutdown_client (_ic, oc) =
  let cli = Unix.descr_of_out_channel oc in
  try
    Unix.shutdown cli Unix.SHUTDOWN_ALL;
    close_out oc
  with _ -> ()

type file_input =
  | FileName of string
  | FileContent of string

let hh_monitor_config root = ServerMonitorUtils.({
  lock_file = ServerFiles.lock_file root;
  socket_file = ServerFiles.socket_file root;
})

let shut_down_server root =
  MC.connect_and_shut_down (hh_monitor_config root)

let connect_to_monitor root =
  MC.connect_once (hh_monitor_config root)

let die_nicely () =
  HackEventLogger.killed ();
  (** Monitor will exit on its next check loop when it sees that
   * the typechecker process has exited. *)
  Hh_logger.log "Sent KILL command by client. Dying.";
  (* XXX when we exit, the dfind process will attempt to read from the broken
   * pipe and then exit with SIGPIPE, so it is unnecessary to kill it
   * explicitly *)
  exit 0
