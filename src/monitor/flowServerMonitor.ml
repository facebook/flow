(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

module Logger = FlowServerMonitorLogger

(* We want to send a "Starting" message immediately and a "Ready" message when the Flow server is
 * ready for the first time. This function sends the "Starting" message immediately and sets up a
 * callback for when the server is ready *)
let handle_waiting_start_command waiting_fd =
  (* Close the fd, but don't worry if it's already closed *)
  let close () =
    try%lwt Lwt_unix.close waiting_fd with
    (* If the waiting process decided not to wait and the fd is closed, then that's fine *)
    | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
    | exn ->
      Logger.error ~exn "Unexpected exception when closing connection to waiting start command";
      Lwt.return_unit
  in
  (* Send a message to the fd, but don't worry if it's already closed *)
  let send_message msg =
    try%lwt
      let%lwt _ = Marshal_tools_lwt.to_fd_with_preamble waiting_fd msg in
      Lwt.return_unit
    with
    (* If the waiting process decided not to wait and closed the pipe, then that's fine *)
    | Unix.Unix_error (Unix.EPIPE, _, _) -> close ()
    | Sys_error msg when msg = "Broken pipe" || msg = "Invalid argument" -> close ()
    | exn ->
      Logger.error ~exn "Unexpected exception when talking to waiting start command";
      close ()
  in
  let%lwt () = send_message FlowServerMonitorDaemon.Starting in
  StatusStream.call_on_free ~f:(fun () ->
      let%lwt () = send_message FlowServerMonitorDaemon.Ready in
      close ()
  )

(* The EventLogger needs to be periodically flushed. The server flushes it during its main serve
 * loop, but the monitor has no main loop. So instead we flush every 5 seconds. That should be
 * good enough, plus the logs are flushed automatically at exit too *)
module LogFlusher = LwtLoop.Make (struct
  type acc = unit

  let main () =
    let%lwt () = Lwt_unix.sleep 5.0 in
    EventLoggerLwt.flush ()

  let catch () exn =
    Logger.fatal ~exn:(Exception.to_exn exn) "LogFlusher somehow hit an exception";
    Exception.reraise exn
end)

(* This is the common entry point for both daemonize and start. *)
let internal_start ~is_daemon ?waiting_fd monitor_options =
  let { FlowServerMonitorOptions.server_options; argv; _ } = monitor_options in
  let root = Options.root server_options in
  let () =
    let file_watcher =
      let open FlowServerMonitorOptions in
      string_of_file_watcher monitor_options.file_watcher
    in
    let vcs =
      match Vcs.find root with
      | None -> "none"
      | Some Vcs.Hg -> "hg"
      | Some Vcs.Git -> "git"
    in
    FlowEventLogger.set_monitor_options ~file_watcher ~vcs;
    LoggingUtils.set_server_options ~server_options
  in
  let tmp_dir = Options.temp_dir server_options in
  (* We need to grab the lock before initializing the pid files and before allocating the shared
   * heap. Luckily for us, the server will do both of these later *)
  let flowconfig_name = Options.flowconfig_name server_options in
  ( if not (Lock.grab (Server_files_js.lock_file ~flowconfig_name ~tmp_dir root)) then
    let msg = "Error: another server is already running?\n" in
    Exit.(exit ~msg Lock_stolen)
  );

  (* We can't open the log until we have the lock.
   *
   * The daemon wants to redirect all stderr to the log. So we can dup2
   * `flow server` wants to output to both stderr and the log, so we initialize Logger with this fd
   *)
  let log_fd =
    let log_file = monitor_options.FlowServerMonitorOptions.log_file in
    let fd = Server_daemon.open_log_file log_file in
    if is_daemon then (
      Unix.dup2 fd Unix.stderr;
      None
    ) else (
      Hh_logger.set_log log_file (Unix.out_channel_of_descr fd);
      Some fd
    )
  in
  (* Open up the socket immediately. When a client tries to connect to an
   * open socket, it will block. When a client tries to connect to a not-yet-open
   * socket, it will fail immediately. The blocking behavior is a little nicer
   *)
  let monitor_socket_fd =
    Socket.init_unix_socket (Server_files_js.socket_file ~flowconfig_name ~tmp_dir root)
  in
  let legacy2_socket_fd =
    Socket.init_unix_socket (Server_files_js.legacy2_socket_file ~flowconfig_name ~tmp_dir root)
  in
  let legacy1_socket_fd =
    Socket.init_unix_socket (Server_files_js.legacy1_socket_file ~flowconfig_name ~tmp_dir root)
  in
  (************************* HERE BEGINS THE MAGICAL WORLD OF LWT *********************************)
  let initial_lwt_thread () =
    Lwt.async (LogFlusher.run ~cancel_condition:ExitSignal.signal);

    (* If `prom`  in `Lwt.async (fun () -> prom)` resolves to an exception, this function will be
     * called *)
    (Lwt.async_exception_hook :=
       fun exn ->
         let exn = Exception.wrap exn in
         let msg =
           Printf.sprintf
             "Uncaught async exception: %s\n%s"
             (Exception.get_ctor_string exn)
             (Exception.get_full_backtrace_string max_int exn)
         in
         Logger.fatal_s ~exn "Uncaught async exception. Exiting";
         Exit.(exit ~msg Unknown_error)
    );

    Logger.init_logger log_fd;
    Logger.info "argv=%s" (argv |> Array.to_list |> String.concat " ");
    LoggingUtils.dump_server_options
      ~server_options:monitor_options.FlowServerMonitorOptions.server_options
      ~log:(Logger.info "%s");

    (* If there is a waiting fd, start up a thread that will message it *)
    let handle_waiting_start_command =
      match waiting_fd with
      | None -> Lwt.return_unit
      | Some fd ->
        let fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true fd in
        handle_waiting_start_command fd
    in
    (* Don't start the server until we've set up the threads to handle the waiting channel *)
    Lwt.async (fun () ->
        let%lwt () = handle_waiting_start_command in
        FlowServerMonitorServer.start monitor_options
    );

    (* We can start up the socket acceptor even before the server starts *)
    Lwt.async (fun () ->
        SocketAcceptor.run
          (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true monitor_socket_fd)
          monitor_options.FlowServerMonitorOptions.autostop
    );
    Lwt.async (fun () ->
        SocketAcceptor.run_legacy
          (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true legacy2_socket_fd)
    );
    Lwt.async (fun () ->
        SocketAcceptor.run_legacy
          (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true legacy1_socket_fd)
    );

    (* Wait forever! Mwhahahahahaha *)
    Lwt.wait () |> fst
  in
  LwtInit.run_lwt initial_lwt_thread

let daemon_entry_point =
  FlowServerMonitorDaemon.register_entry_point (internal_start ~is_daemon:true)

(* The entry point for creating a daemonized flow server monitor (like from `flow start`) *)
let daemonize ~init_id ~wait ~on_spawn monitor_options =
  let server_options = monitor_options.FlowServerMonitorOptions.server_options in
  (* Let's make sure this isn't all for naught before we fork *)
  let root = Options.root server_options in
  let tmp_dir = Options.temp_dir server_options in
  let flowconfig_name = Options.flowconfig_name server_options in
  let lock = Server_files_js.lock_file ~flowconfig_name ~tmp_dir root in
  ( if not (Lock.check lock) then
    let msg = spf "Error: There is already a server running for %s" (Path.to_string root) in
    Exit.(exit ~msg Lock_stolen)
  );

  FlowServerMonitorDaemon.daemonize ~init_id ~wait ~on_spawn ~monitor_options daemon_entry_point

(* The entry point for creating a non-daemonized flow server monitor (like from `flow server`) *)
let start monitor_options =
  (* So this is a tricky situation. Technically this code is running in the `flow server` process.
   * However, we kind of want the actual Flow server to log using the "server" command, and we don't
   * want the monitor's logs to interfere. So instead, we'll pretend like the monitor was created
   * with some imaginary `flow monitor` command *)
  FlowEventLogger.set_command (Some "monitor");

  internal_start ~is_daemon:false ?waiting_fd:None monitor_options
