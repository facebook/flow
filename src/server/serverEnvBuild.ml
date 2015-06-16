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

let make_genv ~multicore options watch_paths =
  let check_mode   = ServerArgs.check_mode options in
  let gc_control   = GlobalConfig.gc_control in
  let nbr_procs    = GlobalConfig.nbr_procs in
  let workers =
    if multicore then Some (Worker.make nbr_procs gc_control) else None
  in
  let dfind = if check_mode then None else Some (DfindLib.init watch_paths) in
  { options;
    workers;
    dfind;
  }

let make_env options =
  { files_info     = Relative_path.Map.empty;
    errorl         = [];
  }
