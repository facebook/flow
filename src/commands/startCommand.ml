(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* start command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "start";
    doc = "Starts a Flow server";
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> options_and_json_flags
        |> log_file_flags
        |> flag "--wait" no_arg ~doc:"Wait for the server to finish initializing"
        |> lazy_flags
        |> autostop_flag
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> no_restart_flag
        |> file_watcher_flag
        |> no_cgroup_flag
        |> anon "root" (optional string));
    usage =
      Printf.sprintf
        "Usage: %s start [OPTION]... [ROOT]\n\nStarts a Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n"
        exe_name;
  }

let main
    base_flags
    options_flags
    json
    pretty
    server_log_file
    monitor_log_file
    wait
    lazy_mode
    autostop
    shm_flags
    ignore_version
    no_restart
    file_watcher
    file_watcher_debug
    file_watcher_timeout
    file_watcher_sync_timeout
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
  let on_spawn pid =
    if pretty || json then
      Hh_json.(
        print_json_endline
          ~pretty
          (JSON_Object
             [
               ("pid", JSON_String (string_of_int pid));
               ("log_file", JSON_String server_log_file);
               ("monitor_log_file", JSON_String monitor_log_file);
             ]))
    else if not (Options.is_quiet options) then (
      Printf.eprintf "Spawned flow server (pid=%d)\n" pid;
      Printf.eprintf "Logs will go to %s\n%!" server_log_file;
      Printf.eprintf "Monitor logs will go to %s\n%!" monitor_log_file
    )
  in
  (* A quiet `flow start` doesn't imply a quiet `flow server` *)
  let server_options = { options with Options.opt_quiet = false } in
  let file_watcher = choose_file_watcher ~options ~file_watcher ~flowconfig in
  let file_watcher_timeout = choose_file_watcher_timeout ~flowconfig file_watcher_timeout in
  let file_watcher_sync_timeout =
    choose_file_watcher_sync_timeout ~flowconfig file_watcher file_watcher_sync_timeout
  in
  let monitor_options =
    {
      FlowServerMonitorOptions.log_file = monitor_log_file;
      autostop;
      no_restart;
      server_log_file;
      server_options;
      shared_mem_config;
      argv = Sys.argv;
      file_watcher;
      file_watcher_debug;
      file_watcher_timeout;
      file_watcher_sync_timeout;
    }
  in
  FlowServerMonitor.daemonize ~wait ~on_spawn monitor_options

let command = CommandSpec.command spec main
