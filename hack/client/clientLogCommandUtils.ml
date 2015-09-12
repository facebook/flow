(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module C = ClientCommand
module LC = ClientLogCommand

let log_command_of_command = function
  | C.CCheck env ->
      let mode = ClientEnv.mode_to_string env.ClientEnv.mode in
      LC.LCCheck (env.ClientEnv.root, env.ClientEnv.from, mode)
  | C.CStart env -> LC.LCStart env.ClientStart.root
  | C.CStop env -> LC.LCStop env.ClientStop.root
  | C.CRestart env -> LC.LCRestart env.ClientStart.root
  | C.CBuild env ->
     LC.LCBuild (env.ClientBuild.root,
                 ClientBuild.build_kind_of env.ClientBuild.build_opts)
  | C.CProlog env -> LC.LCProlog env.ClientProlog.root
