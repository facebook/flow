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

let make_genv options config watch_paths =
  let check_mode   = ServerArgs.check_mode options in
  let ai_mode      = ServerArgs.ai_mode options in
  let gc_control   = ServerConfig.gc_control config in
  Typing_deps.trace :=
    not check_mode || not ai_mode || ServerArgs.convert options <> None ||
    ServerArgs.save_filename options <> None;
  let nbr_procs = GlobalConfig.nbr_procs in
  let workers = Some (Worker.make nbr_procs gc_control) in
  let dfind =
    if check_mode || ai_mode then None else Some (DfindLib.init watch_paths) in
  { options;
    config;
    workers;
    dfind;
  }

let make_env options config =
  let nenv = Naming.empty (ServerConfig.typechecker_options config) in
  { nenv;
    files_info     = Relative_path.Map.empty;
    errorl         = [];
    failed_parsing = Relative_path.Set.empty;
    failed_decl    = Relative_path.Set.empty;
    failed_check   = Relative_path.Set.empty;
  }
