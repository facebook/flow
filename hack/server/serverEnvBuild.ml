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

let make_genv ~multicore options config =
  let root         = ServerArgs.root options in
  let check_mode   = ServerArgs.check_mode options in
  let gc_control   = ServerConfig.gc_control config in
  Typing_deps.trace :=
    not check_mode || ServerArgs.convert options <> None ||
    ServerArgs.save_filename options <> None;
  let nbr_procs    = GlobalConfig.nbr_procs in
  let workers =
    if multicore then Some (Worker.make nbr_procs gc_control) else None
  in
  if not check_mode
  then begin
    if not check_mode && not (Lock.check root "lock")
    then begin
      Printf.fprintf stderr "Error: another server is already running?\n";
      exit 1
    end;
    ()
  end;
  { options      = options;
    config       = config;
    workers      = workers;
  }

let make_env options config =
  let nenv = { Naming.empty with
    Naming.itcopt = ServerConfig.typechecker_options config;
  } in
  { nenv;
    files_info     = Relative_path.Map.empty;
    errorl         = [];
    failed_parsing = Relative_path.Set.empty;
    failed_decl    = Relative_path.Set.empty;
    failed_check   = Relative_path.Set.empty;
  }
