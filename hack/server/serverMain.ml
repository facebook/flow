(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Sys_utils
open ServerEnv
open ServerUtils
open Utils

exception State_not_found
exception Load_state_disabled

type recheck_loop_stats = {
  rechecked_batches : int;
  rechecked_count : int;
  (* includes dependencies *)
  total_rechecked_count : int;
}

let empty_recheck_loop_stats = {
  rechecked_batches = 0;
  rechecked_count = 0;
  total_rechecked_count = 0;
}

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

module MainInit : sig
  val go:
    ServerArgs.options ->
    out_channel option ->
    (unit -> env) ->    (* init function to run while we have init lock *)
    env
end = struct
  let wakeup_client oc msg =
    Option.iter oc begin fun oc ->
      try
        output_string oc (msg ^ "\n");
        flush oc
      with
      (* The client went away *)
      | Sys_error ("Broken pipe") -> ()
      | e ->
          Printf.eprintf "wakeup_client: %s\n%!" (Printexc.to_string e)
    end

  let close_waiting_channel oc =
    Option.iter oc begin fun oc ->
      try close_out oc
      with
      (* The client went away *)
      | Sys_error ("Broken pipe") -> ()
      | e ->
          Printf.eprintf "close_waiting_channel: %s\n%!" (Printexc.to_string e)
    end

  (* This code is only executed when the options --check is NOT present *)
  let go options (waiting_channel: out_channel option) init_fun =
    let root = ServerArgs.root options in
    let t = Unix.gettimeofday () in
    Hh_logger.log "Initializing Server (This might take some time)";
    wakeup_client waiting_channel "starting";
    (* note: we only run periodical tasks on the root, not extras *)
    ServerIdle.init root;
    let init_id = Random_id.short_string () in
    Hh_logger.log "Init id: %s" init_id;
    let env = HackEventLogger.with_id ~stage:`Init init_id init_fun in
    Hh_logger.log "Server is READY";
    (** TODO: Send "ready" signal to the monitor. *)
    wakeup_client waiting_channel "ready";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    close_waiting_channel waiting_channel;
    env
end

module Program =
  struct
    let preinit () =
      HackSearchService.attach_hooks ();
      (* Force hhi files to be extracted and their location saved before workers
       * fork, so everyone can know about the same hhi path. *)
      ignore (Hhi.get_hhi_root());
      Sys_utils.set_signal Sys.sigusr1
        (Sys.Signal_handle Typing.debug_print_last_pos);
      Sys_utils.set_signal Sys.sigusr2
        (Sys.Signal_handle (fun _ -> (
             Hh_logger.log "Got sigusr2 signal. Going to shut down.";
             Exit_status.exit Exit_status.Server_shutting_down
           )))

    let run_once_and_exit genv env =
      ServerError.print_errorl
        (ServerArgs.json_mode genv.options)
        (List.map env.errorl Errors.to_absolute) stdout;
      match ServerArgs.convert genv.options with
      | None ->
         exit (if env.errorl = [] then 0 else 1)
      | Some dirname ->
         ServerConvert.go genv env dirname;
         exit 0

    (* filter and relativize updated file paths *)
    let process_updates genv _env updates =
      let root = Path.to_string @@ ServerArgs.root genv.options in
      (* Because of symlinks, we can have updates from files that aren't in
       * the .hhconfig directory *)
      let updates = SSet.filter (fun p -> str_starts_with p root) updates in
      Relative_path.(relativize_set Root updates)

    let recheck genv old_env typecheck_updates =
      if Relative_path.Set.is_empty typecheck_updates then
        old_env, 0
      else begin
        let failed_parsing =
          Relative_path.Set.union typecheck_updates old_env.failed_parsing in
        let check_env = { old_env with failed_parsing = failed_parsing } in
        let new_env, total_rechecked = ServerTypeCheck.check genv check_env in
        ServerStamp.touch_stamp_errors old_env.errorl new_env.errorl;
        new_env, total_rechecked
      end

    (* This is a hack for us to save / restore the global state that is not
     * already captured by ServerEnv *)
    let marshal chan =
      Typing_deps.marshal chan;
      HackSearchService.SS.MasterApi.marshal chan

    let unmarshal chan =
      Typing_deps.unmarshal chan;
      HackSearchService.SS.MasterApi.unmarshal chan

  end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)

let sleep_and_check in_fd =
  let ready_fd_l, _, _ = Unix.select [in_fd] [] [] (1.0) in
  ready_fd_l <> []

let handle_connection_ genv env ic oc =
  try
    output_string oc "Hello\n";
    flush oc;
    ServerCommand.handle genv env (ic, oc)
  with
  | Sys_error("Broken pipe") | ServerCommand.Read_command_timeout ->
    shutdown_client (ic, oc)
  | e ->
    let msg = Printexc.to_string e in
    EventLogger.master_exception msg;
    Printf.fprintf stderr "Error: %s\n%!" msg;
    Printexc.print_backtrace stderr;
    shutdown_client (ic, oc)

let handle_connection genv env ic oc =
  ServerIdle.stamp_connection ();
  try handle_connection_ genv env ic oc
  with
  | Unix.Unix_error (e, _, _) ->
     Printf.fprintf stderr "Unix error: %s\n" (Unix.error_message e);
     Printexc.print_backtrace stderr;
     flush stderr
  | e ->
     Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
     Printexc.print_backtrace stderr;
     flush stderr

let recheck genv old_env updates =
  let to_recheck =
    Relative_path.Set.filter begin fun update ->
      ServerEnv.file_filter (Relative_path.suffix update)
    end updates in
  let config_in_updates =
    Relative_path.Set.mem ServerConfig.filename updates in
  if config_in_updates then begin
    let new_config = ServerConfig.(load filename genv.options) in
    if not (ServerConfig.is_compatible genv.config new_config) then begin
      Hh_logger.log
        "%s changed in an incompatible way; please restart %s.\n"
        (Relative_path.suffix ServerConfig.filename)
        GlobalConfig.program_name;
       (** TODO: Notify the server monitor directly about this. *)
       Exit_status.(exit Hhconfig_changed)
    end;
  end;
  let env, total_rechecked = Program.recheck genv old_env to_recheck in
  BuildMain.incremental_update genv old_env env updates;
  env, to_recheck, total_rechecked

(* When a rebase occurs, dfind takes a while to give us the full list of
 * updates, and it often comes in batches. To get an accurate measurement
 * of rebase time, we use the heuristic that any changes that come in
 * right after one rechecking round finishes to be part of the same
 * rebase, and we don't log the recheck_end event until the update list
 * is no longer getting populated. *)
let rec recheck_loop acc genv env =
  let t = Unix.gettimeofday () in
  let raw_updates = genv.notifier () in
  if SSet.is_empty raw_updates then
    acc, env
  else begin
    HackEventLogger.notifier_returned t (SSet.cardinal raw_updates);
    let updates = Program.process_updates genv env raw_updates in
    let env, rechecked, total_rechecked = recheck genv env updates in
    let acc = {
      rechecked_batches = acc.rechecked_batches + 1;
      rechecked_count =
        acc.rechecked_count + Relative_path.Set.cardinal rechecked;
      total_rechecked_count = acc.total_rechecked_count + total_rechecked;
    } in
    recheck_loop acc genv env
  end

let recheck_loop = recheck_loop {
  rechecked_batches = 0;
  rechecked_count = 0;
  total_rechecked_count = 0;
}

(** Retrieve channels to client from monitor process. *)
let get_client_channels parent_in_fd =
  let socket = Libancillary.ancil_recv_fd parent_in_fd in
  (Timeout.in_channel_of_descr socket), (Unix.out_channel_of_descr socket)

let serve genv env in_fd _ =
  let env = ref env in
  let last_stats = ref empty_recheck_loop_stats in
  let recheck_id = ref (Random_id.short_string ()) in
  while true do
    ServerMonitorUtils.exit_if_parent_dead ();
    let has_client = sleep_and_check in_fd in
    let has_parsing_hook = !ServerTypeCheck.hook_after_parsing <> None in
    if not has_client && not has_parsing_hook
    then begin
      (* Ugly hack: We want GC_SHAREDMEM_RAN to record the last rechecked
       * count so that we can figure out if the largest reclamations
       * correspond to massive rebases. However, the logging call is done in
       * the SharedMem module, which doesn't know anything about Server stuff.
       * So we wrap the call here. *)
      HackEventLogger.with_rechecked_stats
        !last_stats.rechecked_batches
        !last_stats.rechecked_count
        !last_stats.total_rechecked_count
        ServerIdle.go;
      recheck_id := Random_id.short_string ();
    end;
    let start_t = Unix.gettimeofday () in
    HackEventLogger.with_id ~stage:`Recheck !recheck_id @@ fun () ->
    let stats, new_env = recheck_loop genv !env in
    env := new_env;
    if stats.rechecked_count > 0 then begin
      HackEventLogger.recheck_end start_t has_parsing_hook
        stats.rechecked_batches
        stats.rechecked_count
        stats.total_rechecked_count;
      Hh_logger.log "Recheck id: %s" !recheck_id
    end;
    last_stats := stats;
    if has_client then
      (try
        let ic, oc = get_client_channels in_fd in
        HackEventLogger.got_client_channels start_t;
        (try
          handle_connection genv !env ic oc;
          HackEventLogger.handled_connection start_t;
        with
        | e ->
          HackEventLogger.handle_connection_exception e;
          Hh_logger.log "Handling client failed. Ignoring.")
      with
      | e ->
        HackEventLogger.get_client_channels_exception e;
        Hh_logger.log
          "Getting Client FDs failed. Ignoring.");
       ;
  done

let load genv filename to_recheck =
  let t = Unix.gettimeofday () in
  let chan = open_in filename in
  let env = Marshal.from_channel chan in
  Program.unmarshal chan;
  close_in chan;
  SharedMem.load (filename^".sharedmem");
  HackEventLogger.load_read_end t filename;
  let t = Hh_logger.log_duration "Loading saved state" t in

  let to_recheck =
    List.rev_append (BuildMain.get_all_targets ()) to_recheck in
  let paths_to_recheck =
    List.map to_recheck (Relative_path.concat Relative_path.Root) in
  let updates = Relative_path.set_of_list paths_to_recheck in
  let env, rechecked, total_rechecked = recheck genv env updates in
  let rechecked_count = Relative_path.Set.cardinal rechecked in
  HackEventLogger.load_recheck_end t rechecked_count total_rechecked;
  let _t = Hh_logger.log_duration "Post-load rechecking" t in
  env

let run_load_script genv cmd =
  try
    let t = Unix.gettimeofday () in
    let cmd = Path.to_string cmd in
    let root_arg =
      Path.to_string (ServerArgs.root genv.options) in
    let build_id_arg = Build_id.build_id_ohai in
    Hh_logger.log
      "Running load script: %s %s %s\n%!"
      (Filename.quote cmd)
      (Filename.quote root_arg)
      (Filename.quote build_id_arg);
    let state_fn, to_recheck =
      let reader timeout ic _oc =
        let state_fn =
          try Timeout.input_line ~timeout ic
          with End_of_file -> raise State_not_found
        in
        if state_fn = "DISABLED" then raise Load_state_disabled;
        let to_recheck = ref [] in
        begin
          try
            while true do
              to_recheck := Timeout.input_line ~timeout ic :: !to_recheck
            done
          with End_of_file -> ()
        end;
        let rc = Timeout.close_process_in ic in
        assert (rc = Unix.WEXITED 0);
        state_fn, !to_recheck
      in
      Timeout.read_process
        ~timeout:(ServerConfig.load_script_timeout genv.config)
        ~on_timeout:(fun _ -> failwith "Load script timed out")
        ~reader
        cmd [| cmd; root_arg; build_id_arg; |] in
    Hh_logger.log
      "Load state found at %s. %d files to recheck\n%!"
      state_fn (List.length to_recheck);
    HackEventLogger.load_script_done t;
    let init_type = "load" in
    let env = HackEventLogger.with_init_type init_type begin fun () ->
      load genv state_fn to_recheck
    end in
    env, init_type
  with e -> begin
    let init_type = match e with
      | State_not_found ->
        Hh_logger.log "Load state not found!";
        Hh_logger.log "Starting from a fresh state instead...";
        "load_state_not_found"
      | Load_state_disabled ->
        Hh_logger.log "Load state disabled!";
        "load_state_disabled"
      | e ->
        let msg = Printexc.to_string e in
        Hh_logger.log "Load error: %s" msg;
        Printexc.print_backtrace stderr;
        Hh_logger.log "Starting from a fresh state instead...";
        HackEventLogger.load_failed msg;
        "load_error"
    in
    let env, _ = HackEventLogger.with_init_type init_type begin fun () ->
      ServerInit.init genv
    end in
    env, init_type
  end

let program_init genv =
  let env, init_type =
    (* If we are saving, always start from a fresh state -- just in case
     * incremental mode introduces any errors. *)
    if genv.local_config.ServerLocalConfig.use_mini_state &&
      ServerArgs.save_filename genv.options = None then
      match ServerConfig.load_mini_script genv.config with
      | None ->
          let env, _ = ServerInit.init genv in
          env, "fresh"
      | Some load_mini_script ->
          let env, did_load = ServerInit.init ~load_mini_script genv in
          env, if did_load then "mini_load" else "mini_load_fail"
    else
      match ServerConfig.load_script genv.config with
      | Some load_script
        when ServerArgs.save_filename genv.options = None &&
        not (ServerArgs.no_load genv.options) ->
          run_load_script genv load_script
      | _ ->
          let env, _ = ServerInit.init genv in
          env, "fresh"
  in
  HackEventLogger.init_end init_type;
  Hh_logger.log "Waiting for daemon(s) to be ready...";
  genv.wait_until_ready ();
  ServerStamp.touch_stamp ();
  HackEventLogger.init_really_end init_type;
  env

let save_complete env fn =
  let chan = open_out_no_fail fn in
  Marshal.to_channel chan env [];
  Program.marshal chan;
  close_out_no_fail fn chan;
  (* We cannot save the shared memory to `chan` because the OCaml runtime
   * does not expose the underlying file descriptor to C code; so we use
   * a separate ".sharedmem" file. *)
  SharedMem.save (fn^".sharedmem")

let save _genv env (kind, fn) =
  match kind with
  | ServerArgs.Complete -> save_complete env fn
  | ServerArgs.Mini -> ServerInit.save_state env fn

let setup_server options =
  let root = ServerArgs.root options in
  (* The OCaml default is 500, but we care about minimizing the memory
   * overhead *)
  let gc_control = Gc.get () in
  Gc.set {gc_control with Gc.max_overhead = 200};
  Relative_path.set_path_prefix Relative_path.Root root;
  let config = ServerConfig.(load filename options) in
  let {ServerLocalConfig.cpu_priority; io_priority; _} as local_config =
    ServerLocalConfig.load () in
  if Sys_utils.is_test_mode ()
  then EventLogger.init (Daemon.devnull ()) 0.0
  else HackEventLogger.init root (Unix.gettimeofday ());
  Option.iter
    (ServerArgs.waiting_client options)
    ~f:(fun handle ->
        let fd = Handle.wrap_handle handle in
        Unix.set_close_on_exec fd);
  Program.preinit ();
  Sys_utils.set_priorities ~cpu_priority ~io_priority;
  SharedMem.init (ServerConfig.sharedmem_config config);
  (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
   * someone C-c the client.
   *)
  Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
  PidLog.init (ServerFiles.pids_file root);
  PidLog.log ~reason:"main" (Unix.getpid());
  ServerEnvBuild.make_genv options config local_config

let run_once options =
  let genv = setup_server options in
  if not (ServerArgs.check_mode genv.options) then
    (Hh_logger.log "ServerMain run_once only supported in check mode.";
    Exit_status.(exit Input_error));
  let env = program_init genv in
  Option.iter (ServerArgs.save_filename genv.options) (save genv env);
  Hh_logger.log "Running in check mode";
  Program.run_once_and_exit genv env

(*
 * The server monitor will pass client connections to this process
 * via ic.
 *)
let daemon_main options (ic, oc) =
  let genv = setup_server options in
  if ServerArgs.check_mode genv.options then
    (Hh_logger.log "Invalid program args - can't run daemon in check mode.";
    Exit_status.(exit Input_error));
  let in_fd = Daemon.descr_of_in_channel ic in
  let out_fd = Daemon.descr_of_out_channel oc in
  (** If the client started the server, it opened an FD before forking,
   * so it can be notified when the server is ready. The FD number was
   * passed in program args. *)
  let waiting_channel =
    Option.map
      (ServerArgs.waiting_client options)
      ~f:Handle.to_out_channel in
  let env = MainInit.go options waiting_channel
    (fun () -> program_init genv) in
  serve genv env in_fd out_fd

let entry =
  Daemon.register_entry_point "ServerMain.daemon_main" daemon_main
