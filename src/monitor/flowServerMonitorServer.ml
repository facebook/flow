(**
 * Copyright (c) 2017-present, Facebook, Inc.
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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
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
| Notify_dead_persistent_connection of {
    client_id: PersistentProt.client_id;
  }

(* The long-lived stream of requests *)
let command_stream, push_to_command_stream = Lwt_stream.create ()

(* ServerInstance.t is an individual Flow server instance. The code inside this module handles
 * interacting with a Flow server instance *)
module ServerInstance : sig
  type t
  val start: FlowServerMonitorOptions.t -> t Lwt.t
  val cleanup: t -> unit Lwt.t
  val pid_of: t -> int
end = struct
  type t = {
    pid: int;
    connection: ServerConnection.t;
    command_loop: unit Lwt.t;
    on_exit_thread: unit Lwt.t;
  }

  let handle_response ~msg ~connection:_ =
    match msg with
    | MonitorProt.Response (request_id, response) ->
      Logger.debug "Read a response to request '%s' from the server!" request_id;
      RequestMap.remove ~request_id
      >|= (function
      | None -> Logger.error "Failed to look up request '%s'" request_id
      | Some (_, client) ->
        let msg = MonitorProt.Data response in
        try EphemeralConnection.write_and_close ~msg client
        with Lwt_stream.Closed ->
          Logger.debug "Client for request '%s' is dead. Throwing away response" request_id
      )
    | MonitorProt.RequestFailed (request_id, exn_str) ->
      Logger.error "Server threw exception when processing '%s': %s" request_id exn_str;
      RequestMap.remove ~request_id
      >|= (function
      | None -> Logger.error "Failed to look up request '%s'" request_id
      | Some (_, client) ->
        let msg = MonitorProt.ServerException exn_str in
        try EphemeralConnection.write_and_close ~msg client
        with Lwt_stream.Closed ->
          Logger.debug "Client for request '%s' is dead. Throwing away response" request_id
      )
    | MonitorProt.StatusUpdate status ->
      StatusStream.update ~status;
      Lwt.return_unit
    | MonitorProt.PersistentConnectionResponse (client_id, response) ->
      (match PersistentConnectionMap.get client_id with
      | None -> Logger.error "Failed to look up persistent client #%d" client_id
      | Some connection -> PersistentConnection.write ~msg:response connection);
      Lwt.return_unit

  module CommandLoop = LwtLoop.Make (struct
    type acc = ServerConnection.t

    let send_request ~msg conn =
      ServerConnection.write ~msg conn;
      conn

    let main conn =
      Lwt_stream.next command_stream
      >>= (function
        | Write_ephemeral_request { request; client; } ->
          if not (EphemeralConnection.is_closed client)
          then begin
            RequestMap.add ~request ~client
            >|= (fun request_id ->
              Logger.debug "Writing '%s' to the server connection" request_id;
              send_request ~msg:(MonitorProt.Request (request_id, request)) conn
            )
          end else begin
            Logger.debug "Skipping request from a dead ephemeral connection";
            Lwt.return conn
          end
        | Write_persistent_request { client_id; request; } ->
          let msg = MonitorProt.PersistentConnectionRequest (client_id, request) in
          Lwt.return (send_request ~msg conn)
        | Notify_new_persistent_connection { client_id; logging_context; lsp; } ->
          let msg = MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) in
          Lwt.return (send_request ~msg conn)
        | Notify_dead_persistent_connection { client_id; } ->
          let () = PersistentConnectionMap.remove ~client_id in
          let msg = MonitorProt.DeadPersistentConnection client_id in
          Lwt.return (send_request ~msg conn)
      )

    let catch _ exn =
      Logger.fatal ~exn "Uncaught exception in Server command loop";
      raise exn
  end)

  let cleanup_on_exit ~connection =
    ServerConnection.close_immediately connection

  let cleanup t =
    Lwt.cancel t.command_loop;
    Lwt.cancel t.on_exit_thread;
    ServerConnection.close_immediately t.connection

  let server_num = ref 0

  (* Spawn a brand new Flow server *)
  let start monitor_options =
    Logger.info "Creating a new Flow server";
    let { FlowServerMonitorOptions.
      shared_mem_config;
      server_options;
      server_log_file=log_file;
      argv;
      _;
    } = monitor_options in
    let handle = Server.daemonize
      ~log_file ~shared_mem_config ~argv server_options in
    let (ic, oc) = handle.Daemon.channels in
    let in_fd =
      ic
      |> Daemon.descr_of_in_channel
      |> Lwt_unix.of_unix_file_descr ~blocking:true in
    let out_fd =
      oc
      |> Daemon.descr_of_out_channel
      |> Lwt_unix.of_unix_file_descr ~blocking:true in

    let close_if_open fd =
      try Lwt_unix.close fd
      (* If it's already closed, we'll get EBADF *)
      with Unix.Unix_error(Unix.EBADF, _, _) -> Lwt.return_unit
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
      Lwt.join [
        close_if_open in_fd;
        close_if_open out_fd;
      ]
    in

    incr server_num;
    let name = spf "server #%d" !server_num in
    ServerConnection.create ~name ~in_fd ~out_fd ~close ~on_read:handle_response
    >|= (fun (start, connection) ->
      start ();

      let command_loop = CommandLoop.run ~cancel_condition:ExitSignal.signal connection in

      (* Close the connection to the server when we're about to exit *)
      let on_exit_thread =
        Lwt.catch
          (fun () ->
            Lwt_condition.wait ExitSignal.signal
            >>= (fun () -> cleanup_on_exit ~connection)
          )
          (fun exn ->
            match exn with
            | Lwt.Canceled -> Lwt.return_unit
            | exn ->
              Logger.fatal ~exn "Uncaught exception in on_exit_thread";
              raise exn
          )
      in

      { pid = handle.Daemon.pid; connection; command_loop; on_exit_thread }
    )

  let pid_of t = t.pid
end

(* A wrapper for Pervasives.exit which gives other threads a second to handle their business
 * before the monitor exits *)
let exit ~msg exit_status =
  Logger.info "Monitor is exiting (%s)" msg;
  Logger.info "Broadcasting to threads and waiting 1 second for them to exit";
  Lwt_condition.broadcast ExitSignal.signal ();

  (* Protect this thread from getting canceled *)
  Lwt.protected (
    Lwt_unix.sleep 1.0
    >>= (fun () ->
      FlowEventLogger.exit (Some msg) (FlowExitStatus.to_string exit_status);
      Pervasives.exit (FlowExitStatus.error_code exit_status)
    )
  )

(* A loop who's job is to start a server and then wait for it to die *)
module KeepAliveLoop = LwtLoop.Make (struct
  type acc = FlowServerMonitorOptions.t

  (* Given that a Flow server has just exited with this exit status, should the monitor exit too? *)
  let should_monitor_exit_with_server monitor_options exit_status =
    if monitor_options.FlowServerMonitorOptions.no_restart
    then true
    else begin
      let open FlowExitStatus in
      match exit_status with
      (**** Things the server might exit with that implies that the monitor should exit too ****)

      | No_error  (* Server exited cleanly *)
      | Windows_killed_by_task_manager (* Windows task manager killed the server *)
      | Invalid_flowconfig (* Parse/version/etc error. Server will never start correctly. *)
      | Path_is_not_a_file (* Required a file but privided path was not a file *)
      | Server_client_directory_mismatch (* This is a weird one *)
      | Flowconfig_changed (* We could survive some config changes, but it's too hard to tell *)
      | Unused_server (* The server appears unused for long enough that it decided to just die *)
      | Unknown_error (* Uncaught exn. We probably could survive this, but it's a little risky *)

      (**** Things that the server shouldn't use, but would imply that the monitor should exit ****)

      | Build_id_mismatch (* Client build differs from server build - only monitor uses this *)
      | Lock_stolen (* Lock lost - only monitor should use this *)
      | Socket_error (* Failed to set up socket - only monitor should use this *)
        -> true

      (**** Things the server might exit with which the monitor can survive ****)

      | Server_out_of_date (* Server needs to restart, but monitor can survive *)
      | Out_of_shared_memory (* The monitor doesn't used sharedmem so we can survive *)
      | Dfind_died
      | Dfind_unresponsive
        -> false

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
      | Autostop (* is used by monitor to exit, not server *)
        -> true
    end

  (* Basically a `waitpid [ WUNTRACED ] pid` (WUNTRACED means also return on stopped processes) *)
  let blocking_waitpid =
    let reasonable_impl pid = Lwt_unix.waitpid [Unix.WUNTRACED] pid
    in

    (* Lwt_unix.waitpid without WNOHANG doesn't work on Windows. As a workaround, we can call the
     * WNOHANG version every .5 seconds. https://github.com/ocsigen/lwt/issues/494 *)
    let rec damn_it_windows_impl pid =
      Lwt_unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid
      >>= (function
      (* Still hasn't exited. Let's wait .5s and try again *)
      | 0, _ ->
        Lwt_unix.sleep 0.5 >>= (fun () -> damn_it_windows_impl pid)
      (* Ok, process has exited or died or something. *)
      | result -> Lwt.return result
      )
    in

    if Sys.win32 then damn_it_windows_impl else reasonable_impl

  let wait_for_server_to_die monitor_options server =
    let pid = ServerInstance.pid_of server in
    blocking_waitpid pid
    >>= (fun (_, status) ->
      ServerInstance.cleanup server
      >>= (fun () ->
        if Sys.unix && try Sys_utils.check_dmesg_for_oom pid "flow" with _ -> false
        then FlowEventLogger.murdered_by_oom_killer ();

        match status with
        | Unix.WEXITED exit_status ->
          let exit_type =
            try Some (FlowExitStatus.error_type exit_status)
            with Not_found -> None in
          let exit_status_string =
            Option.value_map ~default:"Invalid_exit_code" ~f:FlowExitStatus.to_string exit_type in
          Logger.error "Flow server (pid %d) exited with code %s (%d)"
            pid
            exit_status_string
            exit_status;
          begin match exit_type with
          | None ->
             exit
              ~msg:(spf "Flow server exited with invalid exit code (%d)" exit_status)
              FlowExitStatus.Unknown_error
          | Some exit_type ->
            if should_monitor_exit_with_server monitor_options exit_type
            then exit ~msg:"Dying along with server" exit_type
            else Lwt.return_unit
          end
        | Unix.WSIGNALED signal ->
          Logger.error "Flow server (pid %d) was killed with %s signal"
            pid
            (PrintSignal.string_of_signal signal);
          FlowEventLogger.report_from_monitor_server_exit_due_to_signal signal;
          Lwt.return_unit
        | Unix.WSTOPPED signal ->
          (* If a Flow server has been stopped but hasn't exited then what should we do? I suppose we
           * could try to signal it to resume. Or we could wait for it to start up again. But killing
           * it and starting a new server seems easier *)
          Logger.error "Flow server (pid %d) was stopped with %s signal. Sending sigkill"
            pid
            (PrintSignal.string_of_signal signal);
          (* kill is not a blocking system call, which is likely why it is missing from Lwt_unix *)
          Unix.kill pid Sys.sigkill;
          Lwt.return_unit
      )
    )

  (* The RequestMap will contain all the requests which have been sent to the server but never
   * received a response. If we're starting up a new server, we can resend all these requests to
   * the new server *)
  let requeue_stalled_requests () =
    RequestMap.remove_all ()
    >>= Lwt_list.iter_p (fun (request, client) ->
      Lwt.return (push_to_command_stream (Some (Write_ephemeral_request {request; client;})))
    )

  (* Ephemeral commands are stateless, so they can survive a server restart. However a persistent
   * connection might have state, so it's wrong to allow it to survive. Maybe in the future we can
   * tell the persistent connection that the server has died and let it adjust its state, but for
   * now lets close all persistent connections *)
  let killall_persistent_connections () =
    PersistentConnectionMap.get_all_clients ()
    |> Lwt_list.iter_p PersistentConnection.close_immediately

  let main monitor_options =
    requeue_stalled_requests ()
    >>= (fun () -> ServerInstance.start monitor_options)
    >>= (wait_for_server_to_die monitor_options)
    >>= killall_persistent_connections
    >|= (fun () -> monitor_options)

  let catch _ exn =
    Logger.error ~exn "Exception in KeepAliveLoop";
    raise exn
end)

let start monitor_options = KeepAliveLoop.run ~cancel_condition:ExitSignal.signal monitor_options

let send_request ~client ~request =
  Logger.debug
    "Adding request (%s) to the command stream"
    (ServerProt.Request.to_string request.ServerProt.Request.command);
  push_to_command_stream
    (Some (Write_ephemeral_request {request; client;}))

let send_persistent_request ~client_id ~request =
  Logger.debug
    "Adding request (%s) to the command stream"
    (PersistentProt.string_of_request request);
  push_to_command_stream
    (Some (Write_persistent_request {client_id; request;}))

let notify_new_persistent_connection ~client_id ~logging_context ~lsp =
  Logger.debug "Adding notification that there's a new persistent client #%d" client_id;
  push_to_command_stream
    (Some (Notify_new_persistent_connection {client_id; logging_context; lsp;}))

let notify_dead_persistent_connection ~client_id =
  Logger.debug "Adding notification that persistent client #%d died" client_id;
  push_to_command_stream
    (Some (Notify_dead_persistent_connection {client_id;}))
