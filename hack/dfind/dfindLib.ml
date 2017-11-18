(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = (DfindServer.msg, unit) Daemon.handle

let init log_fds (scuba_table, roots) =
  let name = Printf.sprintf "file watching process for server %d" (Unix.getpid ()) in
  Daemon.spawn ~name log_fds DfindServer.entry_point (scuba_table, roots)

let pid handle = handle.Daemon.pid

let wait_until_ready {Daemon.channels = (ic, _oc); pid = _} =
  assert (Daemon.from_channel ic = DfindServer.Ready)

let request_changes ?timeout {Daemon.channels = (ic, oc); pid = _} =
  Daemon.to_channel oc ();
  Daemon.from_channel ?timeout ic

let get_changes ?timeout daemon =
  let rec loop acc =
    let diff =
      match request_changes ?timeout daemon with
      | DfindServer.Updates s -> s
      | DfindServer.Ready -> assert false in
    if SSet.is_empty diff
    then acc
    else begin
      let acc = SSet.union diff acc in
      loop acc
    end
  in loop SSet.empty
