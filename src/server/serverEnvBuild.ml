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
(* Building the environment *)
(*****************************************************************************)
open ServerEnv

let make_genv ~multicore options watch_paths handle =
  let check_mode   = Options.is_check_mode options in
  let workers =
    if multicore then
      Some (ServerWorker.make options handle)
    else
      None
  in
  let dfind =
    if check_mode then None
    else Some (DfindLib.init ("flow_server_events", watch_paths)) in
  { options;
    workers;
    dfind;
  }

let make_env options =
  { files_info     = ServerEnv.PathMap.empty;
    errorl         = [];
  }
