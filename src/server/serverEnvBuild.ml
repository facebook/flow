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

let make_genv ~multicore options config watch_paths =
  let check_mode   = ServerArgs.check_mode options in
  let gc_control   = ServerConfig.gc_control config in
  Typing_deps.trace :=
    not check_mode || ServerArgs.convert options <> None ||
    ServerArgs.save_filename options <> None;
  let nbr_procs    = GlobalConfig.nbr_procs in
  let workers =
    if multicore then Some (Worker.make nbr_procs gc_control) else None
  in
  let dfind = if check_mode then None else Some (DfindLib.init watch_paths) in
  { options;
    config;
    workers;
    dfind;
  }

let make_env options config =
  { files_info     = Relative_path.Map.empty;
    errorl         = [];
  }
