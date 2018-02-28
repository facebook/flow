(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

module Logger = FlowServerMonitorLogger

(* So there's a bug in Unix.select on Windows. Basically, select is supposed to be
 * "level triggered". That is, as long as you select a ready fd, select should always return it.
 * This is opposed to an "edge triggered" API  which would only return the fd when it becomes
 * available.
 *
 * However, Unix.select doesn't seem to behave this way on Windows. If you call Unix.select with
 * only sockets, then it uses the select system call  and everything is fine. But if you call
 * Unix.select with a mix of fd types (like stdin and a socket, or a pipe and a socket), it
 * falls back to some other code.
 *
 * The long and the short of it is that sometimes a ready fd isn't returned by Unix.select on
 * Windows when you pass in a mix of sockets and non-sockets. The work around is to split the
 * single Unix.select call into multiple Unix.select calls.
 *
 * OCaml bug report: https://caml.inria.fr/mantis/view.php?id=7665
 * lwt issue: https://github.com/ocsigen/lwt/issues/496
 *)
class windows_select = object
  inherit Lwt_engine.select_based

  method private select fds_r fds_w timeout =
    (* Figure out which fds are already ready to be read *)
    let ready_r = List.fold_left (fun ready_r fd_r ->
      match Unix.select [fd_r] [] [] 0.0 with
      | [], _, _ -> ready_r
      | _ -> fd_r::ready_r
    ) [] fds_r in

    (* Figure out which fds are already ready to be written *)
    let ready_w = List.fold_left (fun ready_w fd_w ->
      match Unix.select [] [fd_w] [] 0.0 with
      | _, [], _ -> ready_w
      | _ -> fd_w::ready_w
    ) [] fds_w in

    (* If nothing is ready, then do a multi-fd select with the timeout *)
    if ready_r = [] && ready_w = []
    then
      let fds_r, fds_w, _ = Unix.select fds_r fds_w [] timeout in
      (fds_r, fds_w)
    else (ready_r, ready_w)
end

(*
 * So there's a bug in Unix.select on unix (Linux and OSX). Basically, select is supposed to raise
 * EBADF on bad fds, but it raises EINVAL on fds that are < 0 or >= FD_SETSIZE. Lwt can handle EBADF
 * and will filter out bad fds. However, it doesn't handle EINVAL.
 *
 * So until this is fixed, let's just translate EINVAL to EBADF. This is only dangerous if EINVAL
 * is ever thrown for something other than a bad fd. From the man page, EINVAL is only thrown for
 *
 * 1) invalid timeouts (impossible from the OCaml API)
 * 2) nfds is < 0 or > RLIMIT_NOFILE, which also should be impossible due the c code in select.c
 *
 * So the only case we really need to worry about is fds that are larger than FD_SETSIZE but still
 * valid. From select's manpage:
 *
 * "select() can monitor only file descriptors numbers that are less than FD_SETSIZE;
 *  poll(2) does not have this limitation"
 *
 * So we're kind of screwed if we have large fds we need to monitor anyway.
 *
 * OCaml bug report: https://caml.inria.fr/mantis/view.php?id=7700
 * lwt issue: https://github.com/ocsigen/lwt/issues/529
 *)
class unix_select = object
  inherit Lwt_engine.select_based

  method private select fds_r fds_w timeout =
    let fds_r, fds_w, _ =
      try Unix.select fds_r fds_w [] timeout
      with
      | Unix.Unix_error (Unix.EINVAL, fn, params) -> begin
        (* Ok, so either one of the fds is an invalid fd, or maybe it's a valid fd but too large
        * for select *)
        begin try
          let explode_if_bad fd = Unix.fstat fd |> ignore in
          List.iter explode_if_bad fds_r;
          List.iter explode_if_bad fds_w
        with Unix.Unix_error (_, _, _) ->
          raise (Unix.Unix_error (Unix.EBADF, fn, params))
        end;
        (* Oh boy. So it looks like all the fds are valid. This likely means that one fd is larger
         * than FD_SETSIZE (which is probably 1024). select() stops working for large fds like this
         *)
        let string_of_fd fd = string_of_int ((Obj.magic fd): int) in
        let string_of_fds fds = String.concat ";" (List.map string_of_fd fds) in
        let params = spf "[%s] [%s] []" (string_of_fds fds_r) (string_of_fds fds_w) in
        raise (Unix.Unix_error (Unix.EINVAL, "select", params))
      end
    in
    (fds_r, fds_w)
end

(* We're using lwt's logger instead of Hh_logger, so let's map Hh_logger levels to lwt levels *)
let lwt_level_of_hh_logger_level = function
| Hh_logger.Level.Off -> Lwt_log_core.Fatal
| Hh_logger.Level.Fatal -> Lwt_log_core.Fatal
| Hh_logger.Level.Error -> Lwt_log_core.Error
| Hh_logger.Level.Warn -> Lwt_log_core.Warning
| Hh_logger.Level.Info -> Lwt_log_core.Info
| Hh_logger.Level.Debug -> Lwt_log_core.Debug

(* We want to send a "Starting" message immediately and a "Ready" message when the Flow server is
 * ready for the first time. This function sends the "Starting" message immediately and sets up a
 * callback for when the server is ready *)
let handle_waiting_start_command waiting_fd =
  (* Close the fd, but don't worry if it's already closed *)
  let close () =
    try%lwt
      Lwt_unix.close waiting_fd
    with
    (* If the waiting process decided not to wait and the fd is closed, then that's fine *)
    | Unix.Unix_error(Unix.EBADF, _, _) -> Lwt.return_unit
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
    | Unix.Unix_error(Unix.EPIPE, _, _) -> close ()
    | Sys_error msg when msg = "Broken pipe"  || msg = "Invalid argument" -> close ()
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
    EventLogger.flush ();
    Lwt.return_unit

  let catch () exn =
    Logger.fatal ~exn "LogFlusher somehow hit an exception";
    raise exn
end)

(* This is the common entry point for both daemonize and start. *)
let internal_start ~is_daemon ?waiting_fd monitor_options =
  let { FlowServerMonitorOptions.server_options; argv; _; } = monitor_options in
  let root = Options.root server_options in
  let tmp_dir = Options.temp_dir server_options in

  (* We need to grab the lock before initializing the pid files and before allocating the shared
   * heap. Luckily for us, the server will do both of these later *)
  if not (Lock.grab (Server_files_js.lock_file ~tmp_dir root))
  then begin
   let msg = "Error: another server is already running?\n" in
   FlowExitStatus.(exit ~msg Lock_stolen)
  end;

  (* We can't open the log until we have the lock.
   *
   * The daemon wants to redirect all stderr to the log. So we can dup2
   * `flow server` wants to output to both stderr and the log, so we initialize Logger with this fd
   *)
  let log_fd =
    let log_file = monitor_options.FlowServerMonitorOptions.log_file in
    let fd = Server_daemon.open_log_file log_file in
    if is_daemon
    then begin
      Unix.dup2 fd Unix.stderr;
      None
    end else begin
      Hh_logger.set_log log_file (Unix.out_channel_of_descr fd);
      Some fd
    end
  in

  (* Open up the socket immediately. When a client tries to connect to an
   * open socket, it will block. When a client tries to connect to a not-yet-open
   * socket, it will fail immediately. The blocking behavior is a little nicer
   *)
  let monitor_socket_fd =
    Socket.init_unix_socket (Server_files_js.socket_file ~tmp_dir root) in
  let legacy_socket_fd =
    Socket.init_unix_socket (Server_files_js.legacy_socket_file ~tmp_dir root) in

  (************************* HERE BEGINS THE MAGICAL WORLD OF LWT *********************************)

  (* In theory, we could allow Flow built on machines with libev to use libev instead of select.
   * However, it seems like lwt_config.h on my OSX opam and my CentOS opam both comment out
   * HAVE_LIBEV. And I suppose if we can't rely on libev everywhere then we should rely on it
   * nowhere *)
  if Sys.win32
  then Lwt_engine.set (new windows_select) (* See comment on windows_select *)
  else Lwt_engine.set (new unix_select); (* See comment on unix_select *)

  Lwt.async (LogFlusher.run ~cancel_condition:ExitSignal.signal);

  (* If `prom`  in `Lwt.async (fun () -> prom)` resolves to an exception, this function will be
   * called *)
  Lwt.async_exception_hook := (fun exn ->
    let bt = Printexc.get_backtrace () in
    let msg = Utils.spf "Uncaught async exception: %s%s"
      (Printexc.to_string exn)
      (if bt = "" then bt else "\n"^bt)
    in
    Logger.fatal ~exn "Uncaught async exception. Exiting";
    FlowExitStatus.(exit ~msg Unknown_error)
  );

  Logger.init_logger
    ?log_fd (Hh_logger.Level.min_level () |> lwt_level_of_hh_logger_level);
  Logger.info "argv=%s" (argv |> Array.to_list |> String.concat " ");

  (* If there is a waiting fd, start up a thread that will message it *)
  let handle_waiting_start_command = match waiting_fd with
  | None -> Lwt.return_unit
  | Some fd ->
    let fd = Lwt_unix.of_unix_file_descr ~blocking:true fd in
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
      (Lwt_unix.of_unix_file_descr ~blocking:true monitor_socket_fd)
      monitor_options.FlowServerMonitorOptions.autostop
  );
  Lwt.async (fun () ->
    SocketAcceptor.run_legacy (Lwt_unix.of_unix_file_descr ~blocking:true legacy_socket_fd)
  );

  (* Wait forever! Mwhahahahahaha *)
  Lwt.wait () |> fst
  |> Lwt_main.run

let daemon_entry_point =
  FlowServerMonitorDaemon.register_entry_point (internal_start ~is_daemon:true)

(* The entry point for creating a daemonized flow server monitor (like from `flow start`) *)
let daemonize ~wait ~on_spawn monitor_options =
  let server_options = monitor_options.FlowServerMonitorOptions.server_options in

  (* Let's make sure this isn't all for naught before we fork *)
  let root = Options.root server_options in
  let tmp_dir = Options.temp_dir server_options in
  let lock = Server_files_js.lock_file ~tmp_dir root in
  if not (Lock.check lock)
  then begin
    let msg = spf
      "Error: There is already a server running for %s"
      (Path.to_string root) in
    FlowExitStatus.(exit ~msg Lock_stolen)
  end;

  FlowServerMonitorDaemon.daemonize ~wait ~on_spawn ~monitor_options daemon_entry_point

(* The entry point for creating a non-daemonized flow server monitor (like from `flow server`) *)
let start monitor_options =
  (* So this is a tricky situation. Technically this code is running in the `flow server` process.
   * However, we kind of want the actual Flow server to log using the "server" command, and we don't
   * want the monitor's logs to interfere. So instead, we'll pretend like the monitor was created
   * with some imaginary `flow monitor` command *)
  FlowEventLogger.set_command (Some "monitor");

  internal_start ~is_daemon:false ?waiting_fd:None monitor_options
