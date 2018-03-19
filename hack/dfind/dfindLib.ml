(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {infd: Unix.file_descr; outfd: Unix.file_descr; pid: int }

let init log_fds (scuba_table, roots) =
  let name = Printf.sprintf "file watching process for server %d" (Unix.getpid ()) in
  let {Daemon.channels = (ic, oc); pid} =
    Daemon.spawn ~name log_fds DfindServer.entry_point (scuba_table, roots)
  in
  {
    infd = Daemon.descr_of_in_channel ic;
    outfd = Daemon.descr_of_out_channel oc;
    pid;
  }

let pid handle = handle.pid

let wait_until_ready handle =
  let msg = Marshal_tools.from_fd_with_preamble handle.infd in
  assert (msg = DfindServer.Ready)

let request_changes ?timeout handle =
  Marshal_tools.to_fd_with_preamble handle.outfd () |> ignore;
  Marshal_tools.from_fd_with_preamble ?timeout handle.infd

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
