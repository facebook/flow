(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* server command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "server";
    doc = "Runs a Flow server in the foreground";
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> lazy_flags
        |> options_flags
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> log_file_flags
        |> no_restart_flag
        |> file_watcher_flag
        |> no_cgroup_flag
        |> anon "root" (optional string));
    usage =
      Printf.sprintf
        "Usage: %s server [OPTION]... [ROOT]\n\nRuns a Flow server in the foreground.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n"
        exe_name;
  }

let main
    base_flags
    lazy_mode
    options_flags
    shm_flags
    ignore_version
    server_log_file
    monitor_log_file
    no_restart
    file_watcher
    file_watcher_debug
    file_watcher_timeout
    path_opt
    () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = CommandUtils.guess_root flowconfig_name path_opt in
  let flowconfig =
    let flowconfig_path = Server_files_js.config_file flowconfig_name root in
    read_config_or_exit ~enforce_warnings:(not ignore_version) flowconfig_path
  in
  let options = make_options ~flowconfig_name ~flowconfig ~lazy_mode ~root options_flags in
  (* initialize loggers before doing too much, especially anything that might exit *)
  LoggingUtils.init_loggers ~options ();

  if not ignore_version then assert_version flowconfig;

  let shared_mem_config = shm_config shm_flags flowconfig in
  let server_log_file =
    match server_log_file with
    | Some s -> s
    | None ->
      CommandUtils.server_log_file
        ~flowconfig_name
        ~tmp_dir:(Options.temp_dir options)
        root
        flowconfig
      |> Path.to_string
  in
  let monitor_log_file =
    match monitor_log_file with
    | Some s -> s
    | None ->
      CommandUtils.monitor_log_file ~flowconfig_name ~tmp_dir:(Options.temp_dir options) root
      |> Path.to_string
  in
  let file_watcher = choose_file_watcher ~options ~file_watcher ~flowconfig in
  let file_watcher_timeout = choose_file_watcher_timeout ~flowconfig file_watcher_timeout in
  let monitor_options =
    {
      FlowServerMonitorOptions.log_file = monitor_log_file;
      autostop = false;
      no_restart;
      server_log_file;
      server_options = options;
      shared_mem_config;
      argv = Sys.argv;
      file_watcher;
      file_watcher_debug;
      file_watcher_timeout;
    }
  in
  FlowServerMonitor.start monitor_options

let command = CommandSpec.command spec main
