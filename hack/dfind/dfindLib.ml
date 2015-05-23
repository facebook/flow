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

let start roots =
  let {Daemon.channels; pid} =
    Daemon.fork (DfindServer.run_daemon roots) in
  channels, pid

let get_changes (result_in, msg_out) =
  Daemon.to_channel msg_out ();
  Daemon.from_channel result_in
