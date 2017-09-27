(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* server command *)
(***********************************************************************)

open CommandUtils

module Main = ServerFunctors.ServerMain (Server.FlowProgram)

let spec = { CommandSpec.
  name = "server";
  doc = "Runs a Flow server in the foreground";
  args = CommandSpec.ArgSpec.(
      empty
      |> lazy_flags
      |> options_flags
      |> shm_flags
      |> ignore_version_flag
      |> from_flag
      |> anon "root" (optional string) ~doc:"Root directory"
    );
  usage = Printf.sprintf
    "Usage: %s server [OPTION]... [ROOT]\n\n\
      Runs a Flow server in the foreground.\n\n\
      Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be the current directory if unspecified.\n"
      exe_name;
}

let main lazy_mode options_flags shm_flags ignore_version from path_opt () =
  let root = CommandUtils.guess_root path_opt in
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let options = make_options ~flowconfig ~lazy_mode ~root options_flags in

  (* initialize loggers before doing too much, especially anything that might exit *)
  LoggingUtils.init_loggers ~from ~options ();

  if not ignore_version then assert_version flowconfig;

  let shared_mem_config = shm_config shm_flags flowconfig in

  Main.run ~shared_mem_config options

let command = CommandSpec.command spec main
