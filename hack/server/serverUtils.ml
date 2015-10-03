(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let shutdown_client (_ic, oc) =
  let cli = Unix.descr_of_out_channel oc in
  try
    Unix.shutdown cli Unix.SHUTDOWN_ALL;
    close_out oc
  with _ -> ()

type connection_state =
  | Connection_ok
  | Build_id_mismatch

let msg_to_channel oc msg =
  Marshal.to_channel oc msg [];
  flush oc

type file_input =
  | FileName of string
  | FileContent of string

let die_nicely () =
  HackEventLogger.killed ();
  Printf.printf "Status: Error\n";
  Printf.printf "Sent KILL command by client. Dying.\n";
  (* XXX when we exit, the dfind process will attempt to read from the broken
   * pipe and then exit with SIGPIPE, so it is unnecessary to kill it
   * explicitly *)
  exit 0
