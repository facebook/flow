(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module Server_files = Server_files_js

type args = {
  shared_mem_config: SharedMem_js.config;
  options: Options.t;
  logging_context: FlowEventLogger.logging_context;
  argv: string array;
  parent_pid: int;
  parent_logger_pid: int option;
  file_watcher_pid: int option;
}

type entry_point = (
  args,
  MonitorProt.monitor_to_server_message,
  MonitorProt.server_to_monitor_message
) Daemon.entry

let open_log_file file =
  (* When opening a new foo.log file, if foo.log already exists, we move it to
   * foo.log.old. On Linux/OSX this is easy, we just call rename. On Windows,
   * the rename can fail if foo.log is open or if foo.log.old already exists.
   * Not a huge problem, we just need to be more intentional *)
  if Sys.file_exists file
  then begin
    let old_file = file ^ ".old" in

    (try
      if Sys.file_exists old_file
      then Sys.remove old_file;
      Sys.rename file old_file
    with e ->
      Utils.prerr_endlinef
        "Log rotate: failed to move '%s' to '%s'\n%s"
        file
        old_file
        (Printexc.to_string e)
    )
  end;
  Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o666

let new_entry_point =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    Printf.sprintf "main_%d" !cpt

let register_entry_point
  (main:
    monitor_channels:MonitorRPC.channels ->
    shared_mem_config:SharedMem_js.config ->
    Options.t ->
    unit)
: entry_point =
  Daemon.register_entry_point
    (new_entry_point ())
    (fun args monitor_channels ->
      let {
        shared_mem_config;
        options;
        logging_context;
        argv;
        parent_pid;
        parent_logger_pid;
        file_watcher_pid;
      } = args in
      LoggingUtils.set_hh_logger_min_level options;
      Hh_logger.info "argv=%s" (argv |> Array.to_list |> String.concat " ");

      (* This server might have been started by a monitor process which is already pretty old, so
      * the start_time might be way out of date. *)
      FlowEventLogger.(restore_context {logging_context with start_time = Unix.gettimeofday (); });
      (* It makes the logs easier if all server logs have the "command" column set to "server",
       * regardless of whether they were started with `flow start` or `flow server` *)
      FlowEventLogger.set_command (Some "server");
      FlowEventLogger.init_flow_command ~version:Flow_version.version;

      let root = Options.root options in
      let tmp_dir = Options.temp_dir options in

      (* Create the pid log and record all the processes that already exist *)
      let flowconfig_name = Options.flowconfig_name options in
      PidLog.init (Server_files_js.pids_file ~flowconfig_name ~tmp_dir root);
      PidLog.log ~reason:"monitor" parent_pid;
      Option.iter parent_logger_pid ~f:(PidLog.log ~reason:"monitor_logger");
      Option.iter file_watcher_pid ~f:(PidLog.log ~reason:"file_watcher");
      PidLog.log ~reason:"main" (Unix.getpid ());
      Option.iter (EventLogger.logger_pid ()) ~f:(PidLog.log ~reason:"main_logger");

      main ~monitor_channels ~shared_mem_config options)

let daemonize ~log_file ~shared_mem_config ~argv ~options ~file_watcher_pid
  main_entry =
  (* Let's make sure this isn't all for naught before we fork *)
  let root = Options.root options in
  let tmp_dir = Options.temp_dir options in
  let flowconfig_name = Options.flowconfig_name options in
  let lock = Server_files.lock_file ~flowconfig_name ~tmp_dir root in
  if not (Lock.check lock)
  then begin
    let msg = spf
      "Error: There is already a server running for %s"
      (Path.to_string root) in
    FlowExitStatus.(exit ~msg Lock_stolen)
  end;

  let null_fd = Daemon.null_fd () in
  let log_fd = open_log_file log_file in
  (* Daemon.spawn is creating a new process with log_fd as both the stdout
   * and stderr. We are NOT leaking stdout and stderr. But the Windows
   * implementation of OCaml does leak stdout and stderr. This means any process
   * that waits for `flow start`'s stdout and stderr to close might wait
   * forever.
   *
   * On Windows 10 (and 8 I think), you can just call `set_close_on_exec` on
   * stdout and stderr and that seems to solve things. However, that call
   * fails on Windows 7. After poking around for a few hours, I can't think
   * of a solution other than manually implementing Unix.create_process
   * correctly.
   *
   * So for now let's make Windows 7 not crash. It seems like `flow start` on
   * Windows 7 doesn't actually leak stdio, so a no op is acceptable
   *)
  if Sys.win32
  then Unix.(try
    set_close_on_exec stdout;
    set_close_on_exec stderr
  with Unix_error (EINVAL, _, _) -> ());
  Daemon.spawn (null_fd, log_fd, log_fd) (main_entry) {
    shared_mem_config;
    options;
    logging_context = FlowEventLogger.get_context ();
    argv;
    parent_pid = Unix.getpid ();
    parent_logger_pid = EventLogger.logger_pid ();
    file_watcher_pid;
  }
