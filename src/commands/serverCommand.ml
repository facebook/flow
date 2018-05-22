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
      |> log_file_flags
      |> no_restart_flag
      |> file_watcher_flag
      |> anon "root" (optional string)
    );
  usage = Printf.sprintf
    "Usage: %s server [OPTION]... [ROOT]\n\n\
      Runs a Flow server in the foreground.\n\n\
      Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be the current directory if unspecified.\n"
      exe_name;
}

let main lazy_mode options_flags shm_flags ignore_version from
  server_log_file monitor_log_file no_restart file_watcher path_opt () =

  let root = CommandUtils.guess_root path_opt in
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let options = make_options ~flowconfig ~lazy_mode ~root options_flags in

  (* initialize loggers before doing too much, especially anything that might exit *)
  LoggingUtils.init_loggers ~from ~options ();

  if not ignore_version then assert_version flowconfig;

  let shared_mem_config = shm_config shm_flags flowconfig in

  let server_log_file = match server_log_file with
  | Some s -> s
  | None ->
    CommandUtils.server_log_file ~tmp_dir:(Options.temp_dir options) root flowconfig
    |> Path.to_string
  in

  let monitor_log_file = match monitor_log_file with
  | Some s -> s
  | None ->
    CommandUtils.monitor_log_file ~tmp_dir:(Options.temp_dir options) root
    |> Path.to_string
  in

  let monitor_options = FlowServerMonitorOptions.({
    log_file = monitor_log_file;
    autostop = false;
    no_restart;
    server_log_file;
    server_options = options;
    shared_mem_config;
    argv = Sys.argv;
    file_watcher;
  }) in

  FlowServerMonitor.start monitor_options

let command = CommandSpec.command spec main
