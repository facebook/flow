(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Flow server monitor will start one or more Flow servers over its lifetime. This module is how
 * the monitor interacts with the server. The basic idea is that there is a long-lived stream of
 * requests, which outlives servers, and a ServerInstance.t that wraps a connection to the server.
 *
 * When a server is alive, the long lived stream of requests gets written to the server and stored
 * in a RequestMap. When a response is received, we look up the client in the RequestMap and forward
 * the response.
 *
 * When a server dies, the monitor decides whether or not to die with the server. If it doesn't die,
 * it creates a new server. Any request that was written to the old server but never received a
 * response will be written again to the new server
 *)

let spf = Printf.sprintf

module Logger = FlowServerMonitorLogger
module PersistentProt = Persistent_connection_prot

type command =
  | Write_ephemeral_request of {
      request: ServerProt.Request.command_with_context;
      client: EphemeralConnection.t;
    }
  | Write_persistent_request of {
      client_id: PersistentProt.client_id;
      request: PersistentProt.request;
    }
  | Notify_new_persistent_connection of {
      client_id: PersistentProt.client_id;
      logging_context: FlowEventLogger.logging_context;
      lsp: Lsp.Initialize.params option;
    }
  | Notify_dead_persistent_connection of { client_id: PersistentProt.client_id }
  | Notify_file_changes

(* A wrapper for Pervasives.exit which gives other threads a second to handle their business
 * before the monitor exits *)
let exiting = ref false

let exit ~msg exit_status =
  if !exiting then
    (* We're already exiting, so there's nothing to do. But no one expects `exit` to return, so
     * let's just wait forever *)
    let (waiter, _) = Lwt.wait () in
    waiter
  else (
    exiting := true;
    Logger.info "Monitor is exiting (%s)" msg;
    Logger.info "Broadcasting to threads and waiting 1 second for them to exit";
    Lwt_condition.broadcast ExitSignal.signal (exit_status, msg);

    (* Protect this thread from getting canceled *)
    Lwt.protected
      (let%lwt () = Lwt_unix.sleep 1.0 in
       FlowEventLogger.exit (Some msg) (FlowExitStatus.to_string exit_status);
       Pervasives.exit (FlowExitStatus.error_code exit_status))
  )

(* Exit after 7 days of no requests *)
module Doomsday : sig
  val start_clock : unit -> unit Lwt.t

  val postpone : unit -> unit
end = struct
  let seven_days_in_secs = 3600. *. 24. *. 7.

  let time_in_seven_days () = Unix.time () +. seven_days_in_secs

  let doomsday_time = ref (time_in_seven_days ())

  let postpone () = doomsday_time := time_in_seven_days ()

  let rec start_clock () =
    let time_til_doomsday = !doomsday_time -. Unix.time () in
    if time_til_doomsday <= 0. then
      exit ~msg:"Exiting server. Last used >7 days ago" FlowExitStatus.Unused_server
    else
      let%lwt () = Lwt_unix.sleep time_til_doomsday in
      start_clock ()
end

(* The long-lived stream of requests in the monitor that have arrived from client *)
(* This is unbounded, because otherwise lspCommand might deadlock. *)
let (command_stream, push_to_command_stream) = Lwt_stream.create ()

(* ServerInstance.t is an individual Flow server instance. The code inside this module handles
 * interacting with a Flow server instance *)
module ServerInstance : sig
  type t

  val start : FlowServerMonitorOptions.t -> ServerStatus.restart_reason option -> t Lwt.t

  val cleanup : t -> unit Lwt.t

  val pid_of : t -> int
end = struct
  type t = {
    pid: int;
    watcher: FileWatcher.watcher;
    connection: ServerConnection.t;
    command_loop: unit Lwt.t;
    file_watcher_loop: unit Lwt.t;
    on_exit_thread: unit Lwt.t;
    file_watcher_exit_thread: unit Lwt.t;
  }

  let handle_response ~msg ~connection:_ =
    match msg with
    | MonitorProt.Response (request_id, response) ->
      Logger.debug "Read a response to request '%s' from the server!" request_id;
      let%lwt request = RequestMap.remove ~request_id in
      (match request with
      | None ->
        Logger.error "Failed to look up request '%s'" request_id;
        Lwt.return_unit
      | Some (_, client) ->
        let msg = MonitorProt.Data response in
        begin
          try EphemeralConnection.write_and_close ~msg client
          with Lwt_stream.Closed ->
            Logger.debug "Client for request '%s' is dead. Throwing away response" request_id
        end;
        Lwt.return_unit)
    | MonitorProt.RequestFailed (request_id, exn_str) ->
      Logger.error "Server threw exception when processing '%s': %s" request_id exn_str;
      let%lwt request = RequestMap.remove ~request_id in
      (match request with
      | None ->
        Logger.error "Failed to look up request '%s'" request_id;
        Lwt.return_unit
      | Some (_, client) ->
        let msg = MonitorProt.ServerException exn_str in
        begin
          try EphemeralConnection.write_and_close ~msg client
          with Lwt_stream.Closed ->
            Logger.debug "Client for request '%s' is dead. Throwing away response" request_id
        end;
        Lwt.return_unit)
    | MonitorProt.StatusUpdate status ->
      StatusStream.update ~status;
      Lwt.return_unit
    | MonitorProt.PersistentConnectionResponse (client_id, response) ->
      (match PersistentConnectionMap.get client_id with
      | None -> Logger.error "Failed to look up persistent client #%d" client_id
      | Some connection -> PersistentConnection.write ~msg:response connection);
      Lwt.return_unit

  module CommandLoop = LwtLoop.Make (struct
    type acc = FileWatcher.watcher * ServerConnection.t

    (* Writes a message to the out-stream of the monitor, to be eventually *)
    (* picked up by the server. *)
    let send_request ~msg conn = ServerConnection.write ~msg conn

    (* In order to try and avoid races between the file system and a command (like `flow status`),
     * we check for file system notification before sending a request to the server *)
    let send_file_watcher_notification watcher conn =
      let%lwt (files, metadata) = watcher#get_and_clear_changed_files in
      if not (SSet.is_empty files) then (
        let count = SSet.cardinal files in
        Logger.info
          "File watcher reported %d file%s changed"
          count
          ( if count = 1 then
            ""
          else
            "s" );
        send_request ~msg:(MonitorProt.FileWatcherNotification (files, metadata)) conn
      );
      Lwt.return_unit

    let main (watcher, conn) =
      let%lwt command = Lwt_stream.next command_stream in
      let%lwt () =
        match command with
        | Write_ephemeral_request { request; client } ->
          Doomsday.postpone ();
          if not (EphemeralConnection.is_closed client) then (
            let%lwt () = send_file_watcher_notification watcher conn in
            let%lwt request_id = RequestMap.add ~request ~client in
            Logger.debug "Writing '%s' to the server connection" request_id;
            send_request ~msg:(MonitorProt.Request (request_id, request)) conn;
            Lwt.return_unit
          ) else (
            Logger.debug "Skipping request from a dead ephemeral connection";
            Lwt.return_unit
          )
        | Write_persistent_request { client_id; request } ->
          Doomsday.postpone ();
          let%lwt () = send_file_watcher_notification watcher conn in
          let msg = MonitorProt.PersistentConnectionRequest (client_id, request) in
          send_request ~msg conn;
          Lwt.return_unit
        | Notify_new_persistent_connection { client_id; logging_context; lsp } ->
          let msg = MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) in
          send_request ~msg conn;
          Lwt.return_unit
        | Notify_dead_persistent_connection { client_id } ->
          let () = PersistentConnectionMap.remove ~client_id in
          let msg = MonitorProt.DeadPersistentConnection client_id in
          send_request ~msg conn;
          Lwt.return_unit
        | Notify_file_changes -> send_file_watcher_notification watcher conn
      in
      Lwt.return (watcher, conn)

    let catch _ exn =
      Logger.fatal ~exn "Uncaught exception in Server command loop";
      raise exn
  end)

  module FileWatcherLoop = LwtLoop.Make (struct
    type acc = FileWatcher.watcher

    (* Poll for file changes every second *)
    let main watcher =
      let%lwt () = watcher#wait_for_changed_files in
      push_to_command_stream (Some Notify_file_changes);
      Lwt.return watcher

    let catch watcher exn =
      match exn with
      | FileWatcher.FileWatcherDied exn ->
        let msg = spf "File watcher (%s) died" watcher#name in
        Logger.fatal ~exn "%s" msg;
        exit ~msg FlowExitStatus.Dfind_died
      | _ ->
        Logger.fatal ~exn "Uncaught exception in Server file watcher loop";
        raise exn
  end)

  (* The monitor is exiting. Let's try and shut down the server gracefully *)
  let cleanup_on_exit ~exit_status ~exit_msg ~connection ~pid =
    let () =
      try
        let msg = MonitorProt.(PleaseDie (MonitorExiting (exit_status, exit_msg))) in
        ServerConnection.write ~msg connection
      with Lwt_stream.Closed ->
        (* Connection to the server has already closed. The server is likely already dead *)
        ()
    in
    (* The monitor waits 1 second before exiting. So let's give the server .75 seconds to shutdown
     * gracefully. *)
    try%lwt
      let%lwt server_status =
        Lwt.pick
          [
            (let%lwt (_, status) = LwtSysUtils.blocking_waitpid pid in
             Lwt.return (Some status));
            (let%lwt () = Lwt_unix.sleep 0.75 in
             Lwt.return None);
          ]
      in
      let%lwt () = ServerConnection.close_immediately connection in
      let still_alive =
        match server_status with
        | Some (Unix.WEXITED exit_status) ->
          let exit_type =
            (try Some (FlowExitStatus.error_type exit_status) with Not_found -> None)
          in
          begin
            if exit_type = Some FlowExitStatus.Killed_by_monitor then
              Logger.info "Successfully killed the server process"
            else
              let exit_status_string =
                Option.value_map ~default:"Invalid_exit_code" ~f:FlowExitStatus.to_string exit_type
              in
              Logger.error
                "Tried to kill the server process (%d), which exited with the wrong exit code: %s"
                pid
                exit_status_string
          end;
          false
        | Some (Unix.WSIGNALED signal) ->
          Logger.error
            "Tried to kill the server process (%d), but for some reason it was killed with %s signal"
            pid
            (PrintSignal.string_of_signal signal);
          false
        | Some (Unix.WSTOPPED signal) ->
          Logger.error
            "Tried to kill the server process (%d), but for some reason it was stopped with %s signal"
            pid
            (PrintSignal.string_of_signal signal);
          true
        | None ->
          Logger.error "Tried to kill the server process (%d), but it didn't die" pid;
          true
      in
      if still_alive then Unix.kill pid Sys.sigkill;

      Lwt.return_unit
    with Unix.Unix_error (Unix.ECHILD, _, _) ->
      Logger.info "Server process has already exited. No need to kill it";
      Lwt.return_unit

  let cleanup t =
    Lwt.cancel t.command_loop;
    Lwt.cancel t.file_watcher_loop;
    Lwt.cancel t.file_watcher_exit_thread;
    Lwt.cancel t.on_exit_thread;

    (* Lwt.join will run these threads in parallel and only return when EVERY thread has returned
     * or failed *)
    Lwt.join [(t.watcher)#stop; ServerConnection.close_immediately t.connection]

  let handle_file_watcher_exit watcher =
    (* TODO (glevi) - We probably don't need to make the monitor exit when the file watcher dies.
     * We could probably just restart it. For dfind, we'd also need to start a new server, but for
     * watchman we probably could just start a new watchman daemon and use the clockspec *)
    exit ~msg:(spf "File watcher (%s) died" watcher#name) FlowExitStatus.Dfind_died

  let server_num = ref 0

  (* Spawn a brand new Flow server *)
  let start monitor_options restart_reason =
    Logger.info "Creating a new Flow server";
    let {
      FlowServerMonitorOptions.shared_mem_config;
      server_options;
      server_log_file = log_file;
      argv;
      file_watcher;
      _;
    } =
      monitor_options
    in
    let%lwt () = StatusStream.reset file_watcher restart_reason in
    let watcher =
      match file_watcher with
      | Options.NoFileWatcher -> new FileWatcher.dummy
      | Options.DFind -> new FileWatcher.dfind monitor_options
      | Options.Watchman -> new FileWatcher.watchman monitor_options
    in
    Logger.debug "Initializing file watcher (%s)" watcher#name;
    watcher#start_init;
    let file_watcher_pid = watcher#getpid in
    let handle =
      Server.daemonize ~log_file ~shared_mem_config ~argv ~file_watcher_pid server_options
    in
    let (ic, oc) = handle.Daemon.channels in
    let in_fd =
      ic
      |> Daemon.descr_of_in_channel
      |> Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true
    in
    let out_fd =
      oc
      |> Daemon.descr_of_out_channel
      |> Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true
    in
    let close_if_open fd =
      try Lwt_unix.close fd (* If it's already closed, we'll get EBADF *)
      with Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
    in
    (* So it's actually important that we close the Lwt_unix.file_descr and not just the
     * underlying Unix.file_descr. Why?
     *
     * 1. Unix.file_descr is just an int
     * 2. File descriptors can be reused after they are closed
     * 3. You might get a reaaaally weird bug where your seemly closed Lwt_unix.file_descr
     *    suddenly starts getting data again. This totally happened to Gabe on halloween and it
     *    totally freaked him out.
     *
     * Lwt_unix.file_descr, on the otherhand, carries around some state, like whether it is open
     * or closed. So a closed Lwt_unix.file_descr won't resurrect.
     *)
    let close () =
      (* Lwt.join will run these threads in parallel and only finish when EVERY thread has finished
       * or failed *)
      Lwt.join [close_if_open in_fd; close_if_open out_fd]
    in
    incr server_num;
    let name = spf "server #%d" !server_num in
    let%lwt (start, connection) =
      ServerConnection.create ~name ~in_fd ~out_fd ~close ~on_read:handle_response
    in
    start ();

    let pid = handle.Daemon.pid in
    (* Close the connection to the server when we're about to exit *)
    let on_exit_thread =
      try%lwt
        let%lwt (exit_status, exit_msg) = Lwt_condition.wait ExitSignal.signal in
        cleanup_on_exit ~exit_status ~exit_msg ~connection ~pid
      with
      | Lwt.Canceled -> Lwt.return_unit
      | exn ->
        Logger.fatal ~exn "Uncaught exception in on_exit_thread";
        raise exn
    in
    (* This may block for quite awhile. No messages will be sent to the server process until the
     * file watcher is up and running *)
    let%lwt () = watcher#wait_for_init in
    Logger.debug "File watcher (%s) ready!" watcher#name;
    let file_watcher_exit_thread =
      let%lwt () =
        try%lwt watcher#waitpid with
        | Lwt.Canceled as exn ->
          let exn = Exception.wrap exn in
          Exception.reraise exn
        | exn ->
          Logger.error ~exn "Uncaught exception in watcher#waitpid";
          Lwt.return_unit
      in
      handle_file_watcher_exit watcher
    in
    StatusStream.file_watcher_ready ();

    let command_loop = CommandLoop.run ~cancel_condition:ExitSignal.signal (watcher, connection) in
    let file_watcher_loop =
      if file_watcher = Options.NoFileWatcher then
        Lwt.return_unit
      (* Don't even bother *)
      else
        FileWatcherLoop.run ~cancel_condition:ExitSignal.signal watcher
    in
    Lwt.return
      {
        pid;
        watcher;
        connection;
        command_loop;
        file_watcher_loop;
        on_exit_thread;
        file_watcher_exit_thread;
      }

  let pid_of t = t.pid
end

(* A loop who's job is to start a server and then wait for it to die *)
module KeepAliveLoop = LwtLoop.Make (struct
  type acc = FlowServerMonitorOptions.t * ServerStatus.restart_reason option

  (* Given that a Flow server has just exited with this exit status, should the monitor exit too?
   *
   * Returns the tuple (should_monitor_exit_with_server, restart_reason)
   *)
  let process_server_exit monitor_options exit_status =
    if monitor_options.FlowServerMonitorOptions.no_restart then
      (true, None)
    else
      FlowExitStatus.(
        match exit_status with
        (**** Things the server might exit with that implies that the monitor should exit too ****)
        | No_error
        (* Server exited cleanly *)
        
        | Windows_killed_by_task_manager
        (* Windows task manager killed the server *)
        
        | Invalid_flowconfig
        (* Parse/version/etc error. Server will never start correctly. *)
        
        | Path_is_not_a_file
        (* Required a file but privided path was not a file *)
        
        | Server_client_directory_mismatch
        (* This is a weird one *)
        
        | Flowconfig_changed
        (* We could survive some config changes, but it's too hard to tell *)
        
        | Invalid_saved_state
        (* The saved state file won't automatically recover by restarting *)
        
        | Unused_server
        (* The server appears unused for long enough that it decided to just die *)
        
        | Unknown_error
        (* Uncaught exn. We probably could survive this, but it's a little risky *)
        
        | Watchman_error
        (* We ran into an issue with Watchman *)
        (**** Things that the server shouldn't use, but would imply that the monitor should exit ****)
        
        | Interrupted
        | Build_id_mismatch
        (* Client build differs from server build - only monitor uses this *)
        
        | Lock_stolen
        (* Lock lost - only monitor should use this *)
        
        | Socket_error
        (* Failed to set up socket - only monitor should use this *)
        
        | Dfind_died
        (* Any file watcher died (it's misnamed) - only monitor should use this *)
        
        | Dfind_unresponsive (* Not used anymore *) ->
          (true, None)
        (**** Things the server might exit with which the monitor can survive ****)
        | Server_out_of_date (* Server needs to restart, but monitor can survive *) ->
          (false, Some ServerStatus.Server_out_of_date)
        | Out_of_shared_memory (* The monitor doesn't used sharedmem so we can survive *) ->
          (false, Some ServerStatus.Out_of_shared_memory)
        | Killed_by_monitor (* The server died because we asked it to die *) -> (false, None)
        | Restart (* The server asked to be restarted *) -> (false, Some ServerStatus.Restart)
        (**** Unrelated exit codes. If we see them then something is wrong ****)
        | Type_error
        | Out_of_time
        | Kill_error
        | No_server_running
        | Out_of_retries
        | Input_error
        | Could_not_find_flowconfig
        | Commandline_usage_error
        | No_input
        | Missing_flowlib
        | Server_start_failed _
        | Autostop (* is used by monitor to exit, not server *) ->
          (true, None))

  let should_monitor_exit_with_signaled_server signal =
    (* While there are many scary things which can cause segfaults, in practice we've mostly seen
     * them when the Flow server hits some infinite or very deep recursion (like Core_list.map ~f:on a
     * very large list). Often, this is triggered by some ephemeral command, which is rerun when
     * the server starts back up, leading to a cycle of segfaulting servers.
     *
     * The easiest solution is for the monitor to exit as well when the server segfaults. This
     * will cause the bad command to consume retries and eventually exit. This doesn't prevent
     * future bad commands, but is better than the alternative.
     *)
    signal = Sys.sigsegv

  let wait_for_server_to_die monitor_options server =
    let pid = ServerInstance.pid_of server in
    let%lwt (_, status) = LwtSysUtils.blocking_waitpid pid in
    let%lwt () = ServerInstance.cleanup server in
    if Sys.unix && (try Sys_utils.check_dmesg_for_oom pid "flow" with _ -> false) then
      FlowEventLogger.murdered_by_oom_killer ();

    match status with
    | Unix.WEXITED exit_status ->
      let exit_type =
        (try Some (FlowExitStatus.error_type exit_status) with Not_found -> None)
      in
      let exit_status_string =
        Option.value_map ~default:"Invalid_exit_code" ~f:FlowExitStatus.to_string exit_type
      in
      Logger.error
        "Flow server (pid %d) exited with code %s (%d)"
        pid
        exit_status_string
        exit_status;
      begin
        match exit_type with
        | None ->
          exit
            ~msg:(spf "Flow server exited with invalid exit code (%d)" exit_status)
            FlowExitStatus.Unknown_error
        | Some exit_type ->
          (* There are a few whitelisted reasons where the persistent client wants *)
          (* to know why the flow server is about to fatally close the persistent  *)
          (* connection. This WEXITED case covers them. (It doesn't matter that    *)
          (* it also sends the reason in a few additional cases as well.)          *)
          let send_close conn =
            try PersistentConnection.write ~msg:(PersistentProt.ServerExit exit_type) conn
            with _ -> ()
          in
          PersistentConnectionMap.get_all_clients () |> List.iter send_close;

          let (should_monitor_exit_with_server, restart_reason) =
            process_server_exit monitor_options exit_type
          in
          if should_monitor_exit_with_server then
            exit ~msg:"Dying along with server" exit_type
          else
            Lwt.return restart_reason
      end
    | Unix.WSIGNALED signal ->
      Logger.error
        "Flow server (pid %d) was killed with %s signal"
        pid
        (PrintSignal.string_of_signal signal);
      FlowEventLogger.report_from_monitor_server_exit_due_to_signal signal;
      if should_monitor_exit_with_signaled_server signal then
        exit ~msg:"Dying along with signaled server" FlowExitStatus.Interrupted
      else
        Lwt.return_none
    | Unix.WSTOPPED signal ->
      (* If a Flow server has been stopped but hasn't exited then what should we do? I suppose we
        * could try to signal it to resume. Or we could wait for it to start up again. But killing
        * it and starting a new server seems easier *)
      Logger.error
        "Flow server (pid %d) was stopped with %s signal. Sending sigkill"
        pid
        (PrintSignal.string_of_signal signal);

      (* kill is not a blocking system call, which is likely why it is missing from Lwt_unix *)
      Unix.kill pid Sys.sigkill;
      Lwt.return_none

  (* The RequestMap will contain all the requests which have been sent to the server but never
   * received a response. If we're starting up a new server, we can resend all these requests to
   * the new server *)
  let requeue_stalled_requests () =
    let%lwt requests = RequestMap.remove_all () in
    Lwt_list.iter_p
      (fun (request, client) ->
        Lwt.return (push_to_command_stream (Some (Write_ephemeral_request { request; client }))))
      requests

  (* Ephemeral commands are stateless, so they can survive a server restart. However a persistent
   * connection might have state, so it's wrong to allow it to survive. Maybe in the future we can
   * tell the persistent connection that the server has died and let it adjust its state, but for
   * now lets close all persistent connections *)
  let killall_persistent_connections () =
    PersistentConnectionMap.get_all_clients ()
    |> Lwt_list.iter_p PersistentConnection.close_immediately

  let main (monitor_options, restart_reason) =
    let%lwt () = requeue_stalled_requests () in
    let%lwt server = ServerInstance.start monitor_options restart_reason in
    let%lwt restart_reason = wait_for_server_to_die monitor_options server in
    let%lwt () = killall_persistent_connections () in
    Lwt.return (monitor_options, restart_reason)

  let catch _ exn =
    let e = Exception.wrap exn in
    match exn with
    | Watchman_lwt.Timeout ->
      let msg = Printf.sprintf "Watchman timed out.\n%s" (Exception.to_string e) in
      FlowExitStatus.(exit ~msg Watchman_error)
    | _ ->
      Logger.error ~exn "Exception in KeepAliveLoop";
      Exception.reraise e
end)

let setup_signal_handlers =
  let signals =
    [
      Sys.sigint;
      (* Interrupt - ctrl-c *)
        Sys.sigterm;
      (* Termination - like a nicer sigkill giving you a chance to cleanup *)
        Sys.sighup;
      (* Hang up - the terminal went away *)
        Sys.sigquit;
        (* Dump core - Kind of a meaner sigterm *)
      
    ]
  in
  let handle_signal signal =
    Lwt.async (fun () ->
        exit
          ~msg:(spf "Received %s signal" (PrintSignal.string_of_signal signal))
          FlowExitStatus.Interrupted)
  in
  let set_signal s =
    try Sys_utils.set_signal s (Sys.Signal_handle handle_signal)
    with exn ->
      Logger.error ~exn "Failed to install signal handler for %s" (PrintSignal.string_of_signal s)
  in
  (fun () -> List.iter set_signal signals)

let start monitor_options =
  Lwt.async Doomsday.start_clock;
  setup_signal_handlers ();
  KeepAliveLoop.run ~cancel_condition:ExitSignal.signal (monitor_options, None)

let send_request ~client ~request =
  Logger.debug
    "Adding request (%s) to the command stream"
    (ServerProt.Request.to_string request.ServerProt.Request.command);
  push_to_command_stream (Some (Write_ephemeral_request { request; client }))

let send_persistent_request ~client_id ~request =
  Logger.debug
    "Adding request (%s) to the command stream"
    (PersistentProt.string_of_request request);
  push_to_command_stream (Some (Write_persistent_request { client_id; request }))

let notify_new_persistent_connection ~client_id ~logging_context ~lsp =
  Logger.debug "Adding notification that there's a new persistent client #%d" client_id;
  push_to_command_stream
    (Some (Notify_new_persistent_connection { client_id; logging_context; lsp }))

let notify_dead_persistent_connection ~client_id =
  Logger.debug "Adding notification that persistent client #%d died" client_id;
  push_to_command_stream (Some (Notify_dead_persistent_connection { client_id }))
