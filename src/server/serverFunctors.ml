(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js
module List = Core_list
module Server_files = Server_files_js

exception State_not_found

module type SERVER_PROGRAM = sig
  val init : focus_targets:FilenameSet.t option -> genv -> (Profiling_js.finished * env)
  val check_once : genv -> env ->
    Errors.ErrorSet.t * (* errors *)
    Errors.ErrorSet.t * (* warnings *)
    (Errors.error * Loc.LocSet.t) list (* suppressed errors *)
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> FilenameSet.t
  val recheck: genv -> env ->
    ?force_focus:bool -> FilenameSet.t -> env
  val get_watch_paths: Options.t -> Path.t list
  val name: string
  val handle_command :
    genv -> env -> MonitorProt.request_id * ServerProt.Request.command_with_context -> env
  val handle_persistent_client : genv -> env ->
    Persistent_connection_prot.client_id -> Persistent_connection_prot.request -> env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)
module ServerMain (Program : SERVER_PROGRAM) : sig
  val check_once :
    shared_mem_config:SharedMem_js.config ->
    client_include_warnings:bool ->
    ?focus_targets:FilenameSet.t ->
    Options.t ->
    Profiling_js.finished *
      Errors.ErrorSet.t * (* errors *)
      Errors.ErrorSet.t * (* warnings *)
      (Errors.error * Loc.LocSet.t) list (* suppressed errors *)
  val daemonize :
    log_file:string ->
    shared_mem_config:SharedMem_js.config ->
    argv:string array ->
    Options.t ->
    (MonitorProt.server_to_monitor_message, MonitorProt.monitor_to_server_message) Daemon.handle
end = struct
  let sleep_and_check () =
    MonitorRPC.read ~timeout:1.0

  let handle_command ~genv env command =
    try Program.handle_command genv env command
    with
    | Unix.Unix_error (e, _, _) ->
        Printf.fprintf stderr "Unix error: %s\n" (Unix.error_message e);
        Printexc.print_backtrace stderr;
        flush stderr;
        env
    | e ->
        Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        flush stderr;
        env

  let exit_due_to_dfind_dying ~genv e =
    let root = Options.root genv.options in
    let tmp_dir = Options.temp_dir genv.options in
    let dfind_logs = Sys_utils.cat_no_fail (Server_files.dfind_log_file ~tmp_dir root) in
    let logs_len = String.length dfind_logs in
    (* Let's limit how much of the log we stick in the exit message *)
    let max_len = 2000 in
    let dfind_logs = if logs_len > max_len
      then String.sub dfind_logs (logs_len - max_len) max_len
      else dfind_logs in
    let msg = spf
      "dfind died (got exception: %s)\ndfind logs:\n%s"
      (Printexc.to_string e)
      dfind_logs in
    FlowExitStatus.(exit Dfind_died ~msg)

  (* When a rebase occurs, dfind takes a while to give us the full list of
   * updates, and it often comes in batches. To get an accurate measurement
   * of rebase time, we use the heuristic that any changes that come in
   * right after one rechecking round finishes to be part of the same
   * rebase, and we don't log the recheck_end event until the update list
   * is no longer getting populated. *)
  let rec recheck_loop ~dfind genv env =
    let raw_updates =
      try DfindLib.get_changes dfind
      with
      | Sys_error msg as e when msg = "Broken pipe" -> exit_due_to_dfind_dying ~genv e
      | End_of_file as e -> exit_due_to_dfind_dying ~genv e
    in
    if SSet.is_empty raw_updates then env else begin
      let updates = Program.process_updates genv env raw_updates in
      let env = Program.recheck genv env updates in
      recheck_loop ~dfind genv env
    end


  let process_message genv env request =
    ServerPeriodical.stamp_connection ();
    match request with
    | MonitorProt.Request (request_id, command) ->
      handle_command genv env (request_id, command)
    | MonitorProt.NewPersistentConnection (client_id, logging_context) ->
      { env with
        connections = Persistent_connection.add_client env.connections client_id logging_context
      }
    | MonitorProt.PersistentConnectionRequest (client_id, request) ->
      Program.handle_persistent_client genv env client_id request
    | MonitorProt.DeadPersistentConnection client_id ->
      { env with connections = Persistent_connection.remove_client env.connections client_id }

  let rec serve ~dfind ~genv ~env =
    MonitorRPC.status_update ~event:ServerStatus.Ready;

    ServerPeriodical.call_before_sleeping ();
    let message = sleep_and_check () in

    let env = recheck_loop ~dfind genv env in

    let env = Option.value_map ~default:env ~f:(process_message genv env) message in

    EventLogger.flush ();
    serve ~dfind ~genv ~env

  (* This code is only executed when the options --check is NOT present *)
  let with_init_lock init_fun =
    let t = Unix.gettimeofday () in
    Hh_logger.info "Initializing Server (This might take some time)";
    let _profiling, env = init_fun () in
    Hh_logger.info "Server is READY";
    let t' = Unix.gettimeofday () in
    Hh_logger.info "Took %f seconds to initialize." (t' -. t);
    env

  let init_dfind options =
    let tmp_dir = Options.temp_dir options in
    let root = Options.root options in
    let in_fd = Daemon.null_fd () in
    let log_file = Server_files_js.dfind_log_file ~tmp_dir root in
    let log_fd = Daemon.fd_of_path log_file in
    let fds = (in_fd, log_fd, log_fd) in
    let watch_paths = Program.get_watch_paths options in
    DfindLib.init fds ("flow_server_events", watch_paths)


  (* The main entry point of the daemon
  * the only trick to understand here, is that env.modified is the set
  * of files that changed, it is only set back to SSet.empty when the
  * type-checker succeeded. So to know if there is some work to be done,
  * we look if env.modified changed.
  *)
  let create_program_init ~shared_mem_config ~focus_targets options =
    let handle = SharedMem_js.init shared_mem_config in
    let genv = ServerEnvBuild.make_genv options handle in
    let program_init = fun () ->
      let profiling, env = Program.init ~focus_targets genv in
      FlowEventLogger.init_done ~profiling;
      profiling, env
    in
    genv, program_init

  let run ~monitor_channels ~shared_mem_config options =
    MonitorRPC.init ~channels:monitor_channels;
    let genv, program_init =
      create_program_init ~shared_mem_config ~focus_targets:None options in

    let dfind = init_dfind options in

    let env = with_init_lock (fun () ->
      ServerPeriodical.init ();
      let env = program_init () in
      DfindLib.wait_until_ready dfind;
      env
    ) in

    serve ~dfind ~genv ~env

  let run_from_daemonize ~monitor_channels ~shared_mem_config options =
    try run ~monitor_channels ~shared_mem_config options
    with
    | SharedMem_js.Out_of_shared_memory ->
        FlowExitStatus.(exit Out_of_shared_memory)
    | e ->
        let bt = Printexc.get_backtrace () in
        let msg = Utils.spf "Unhandled exception: %s%s"
          (Printexc.to_string e)
          (if bt = "" then bt else "\n"^bt)
        in
        FlowExitStatus.(exit ~msg Unknown_error)

  let check_once ~shared_mem_config ~client_include_warnings ?focus_targets options =
    PidLog.disable ();
    MonitorRPC.disable ();
    let genv, program_init =
      create_program_init ~shared_mem_config ~focus_targets options in
    let profiling, env = program_init () in
    let errors, warnings, suppressed_errors = Program.check_once genv env in
    let warnings = if client_include_warnings || Options.should_include_warnings options
      then warnings
      else Errors.ErrorSet.empty
    in
    profiling, errors, warnings, suppressed_errors

  let daemonize =
    let entry = Server_daemon.register_entry_point run_from_daemonize in
    fun ~log_file ~shared_mem_config ~argv options ->
      Server_daemon.daemonize ~log_file ~shared_mem_config ~argv ~options entry
end
