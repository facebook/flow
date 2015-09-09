(**
 * Copyright (c) 2015, Facebook, Inc.
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
  let root = ServerArgs.root options in
  let check_mode   = ServerArgs.check_mode options in
  let gc_control   = ServerConfig.gc_control config in
  Typing_deps.trace :=
    not check_mode || ServerArgs.convert options <> None ||
    ServerArgs.save_filename options <> None;
  let nbr_procs = GlobalConfig.nbr_procs in
  let workers =
    if Sys.win32 then
      None (* No parallelism on Windows yet, Work-in-progress *)
    else
      Some (Worker.make nbr_procs gc_control) in
  let dfind =
    if Sys.win32 || check_mode then
      None
    else
      let log_link = ServerFiles.dfind_log root in
      let log_file = ServerFiles.make_link_of_timestamped log_link in
      Some (DfindLib.init ~log_file watch_paths)
  in
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
