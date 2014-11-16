(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
(*****************************************************************************)
(* Library code *)
(*****************************************************************************)
open DfindEnv

let start root = 
  let msg_out, result_in, pid = DfindServer.fork_in_pipe root in
  (Unix.out_channel_of_descr msg_out,
  Unix.in_channel_of_descr result_in),
  pid

let get_changes (msg_out, result_in) =
  Marshal.to_channel msg_out "Go" [];
  flush msg_out;
  let (result: SSet.t) = Marshal.from_channel result_in in
  result
