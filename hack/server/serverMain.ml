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

let sprintf = Printf.sprintf

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

module MainInit : sig
  val go:
    ServerArgs.options ->
    (unit -> env) ->    (* init function to run while we have init lock *)
    env
end = struct
  let grab_init_lock root =
    ignore(Lock.grab (ServerFiles.init_file root))

  let release_init_lock root =
    ignore(Lock.release (ServerFiles.init_file root))

  let wakeup_client ?(close = false) options msg =
    match ServerArgs.waiting_client options with
    | None -> ()
    | Some oc ->
        output_string oc (msg ^ "\n");
        if close then close_out oc else flush oc

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun =
    let root = ServerArgs.root options in
    let t = Unix.gettimeofday () in
    Hh_logger.log "Initializing Server (This might take some time)";
    grab_init_lock root;
    wakeup_client options "starting";
    (* note: we only run periodical tasks on the root, not extras *)
    ServerPeriodical.init root;
    let env = init_fun () in
    release_init_lock root;
    Hh_logger.log "Server is READY";
    wakeup_client ~close:true options "ready";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    env
end

module type SERVER_PROGRAM = sig
  val preinit : unit -> unit
  val init : genv -> env -> env
  val run_once_and_exit : genv -> env -> unit
  val should_recheck : Relative_path.t -> bool
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> Relative_path.Set.t
  val recheck: genv -> env -> Relative_path.Set.t -> env
  val post_recheck_hook: genv -> env -> env -> Relative_path.Set.t -> unit
  val parse_options: unit -> ServerArgs.options
  val get_watch_paths: ServerArgs.options -> Path.t list
  val name: string
  val config_filename : unit -> Relative_path.t
  val load_config : unit -> ServerConfig.t
  val validate_config : genv -> bool
  val handle_client : genv -> env -> client -> unit
  (* This is a hack for us to save / restore the global state that is not
   * already captured by ServerEnv *)
  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit
end

module Program : SERVER_PROGRAM =
  struct
    let name = "hh_server"

    let config_filename_ =
      Relative_path.concat Relative_path.Root ".hhconfig"

    let config_filename () = config_filename_

    let load_config () = ServerConfig.load config_filename_

    let validate_config genv =
      let new_config = load_config () in
      (* This comparison can eventually be made more complex; we may not always
       * need to restart hh_server, e.g. changing the path to the load script
       * is immaterial*)
      genv.config = new_config

    let handle_client (genv:ServerEnv.genv) (env:ServerEnv.env) client =
      ServerCommand.handle genv env client

    let preinit () =
      HackSearchService.attach_hooks ();
      (* Force hhi files to be extracted and their location saved before workers
       * fork, so everyone can know about the same hhi path. *)
      ignore (Hhi.get_hhi_root());
      if not Sys.win32 then
        ignore @@
        Sys.signal Sys.sigusr1 (Sys.Signal_handle Typing.debug_print_last_pos)

    let make_next_files dir =
      let php_next_files = Find.make_next_files FindUtils.is_php dir in
      let js_next_files = Find.make_next_files FindUtils.is_js dir in
      fun () -> php_next_files () @ js_next_files ()

    let stamp_file = Filename.concat GlobalConfig.tmp_dir "stamp"
    let touch_stamp () =
      Sys_utils.mkdir_no_fail (Filename.dirname stamp_file);
      Sys_utils.with_umask
        0o111
        (fun () ->
         (* Open and close the file to set its mtime. Don't use the Unix.utimes
          * function since that will fail if the stamp file doesn't exist. *)
         close_out (open_out stamp_file)
        )
    let touch_stamp_errors l1 l2 =
      (* We don't want to needlessly touch the stamp file if the error list is
       * the same and nothing has changed, but we also don't want to spend a ton
       * of time comparing huge lists of errors over and over (i.e., grind to a
       * halt in the cases when there are thousands of errors). So we cut off
       * the comparison at an arbitrary point. *)
      let rec length_greater_than n = function
        | [] -> false
        | _ when n = 0 -> true
        | _::l -> length_greater_than (n-1) l in
      if length_greater_than 5 l1 || length_greater_than 5 l2 || l1 <> l2
      then touch_stamp ()

    let init genv env =
      let module RP = Relative_path in
      let root = ServerArgs.root genv.options in
      let hhi_root = Hhi.get_hhi_root () in
      let next_files_hhi =
        compose (List.map ~f:(RP.create RP.Hhi)) (make_next_files hhi_root) in
      let next_files_root =
        compose (List.map ~f:(RP.create RP.Root)) (make_next_files root)
      in
      let next_files = fun () ->
        match next_files_hhi () with
        | [] -> next_files_root ()
        | x -> x in
      let env = ServerInit.init genv env next_files in
      touch_stamp ();
      env

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

    let process_updates _genv _env updates =
      Relative_path.relativize_set Relative_path.Root updates

    let should_recheck update =
      FindUtils.is_php (Relative_path.suffix update)

    let recheck genv old_env typecheck_updates =
      if Relative_path.Set.is_empty typecheck_updates then
        old_env
      else
        let failed_parsing =
          Relative_path.Set.union typecheck_updates old_env.failed_parsing in
        let check_env = { old_env with failed_parsing = failed_parsing } in
        let new_env = ServerTypeCheck.check genv check_env in
        begin
          touch_stamp_errors old_env.errorl new_env.errorl;
          new_env
        end

    let post_recheck_hook = BuildMain.incremental_update

    let parse_options = ServerArgs.parse_options

    let get_watch_paths _options = []

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

let sleep_and_check socket =
  let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
  ready_socket_l <> []

let handle_connection_ genv env socket =
  let cli, _ = Unix.accept socket in
  let ic = Unix.in_channel_of_descr cli in
  let oc = Unix.out_channel_of_descr cli in
  let close () =
    begin
      Unix.shutdown cli Unix.SHUTDOWN_ALL;
      Unix.close cli
    end
  in
  try
    let client_build_id = input_line ic in
    if client_build_id <> Build_id.build_id_ohai then
      (msg_to_channel oc Build_id_mismatch;
       HackEventLogger.out_of_date ();
       Printf.eprintf "Status: Error\n";
       Printf.eprintf "%s is out of date. Exiting.\n" Program.name;
       Exit_status.exit Exit_status.Build_id_mismatch)
    else
      msg_to_channel oc Connection_ok;
    let client = { ic; oc; close } in
    Program.handle_client genv env client
  with
  | Sys_error("Broken pipe") ->
    close ()
  | e ->
    let msg = Printexc.to_string e in
    EventLogger.master_exception msg;
    Printf.fprintf stderr "Error: %s\n%!" msg;
    Printexc.print_backtrace stderr;
    close ()

let handle_connection genv env socket =
  ServerPeriodical.stamp_connection ();
  try handle_connection_ genv env socket
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
    Relative_path.Set.filter Program.should_recheck updates in
  let config = Program.config_filename () in
  let config_in_updates = Relative_path.Set.mem config updates in
  if config_in_updates && not (Program.validate_config genv) then
    (Hh_logger.log
      "%s changed in an incompatible way; please restart %s.\n"
      (Relative_path.suffix config)
      Program.name;
     exit 4);
  let env = Program.recheck genv old_env to_recheck in
  Program.post_recheck_hook genv old_env env updates;
  env, to_recheck

(* When a rebase occurs, dfind takes a while to give us the full list of
 * updates, and it often comes in batches. To get an accurate measurement
 * of rebase time, we use the heuristic that any changes that come in
 * right after one rechecking round finishes to be part of the same
 * rebase, and we don't log the recheck_end event until the update list
 * is no longer getting populated. *)
let rec recheck_loop i rechecked_count genv env =
  let raw_updates =
    match genv.dfind with
    | None -> SSet.empty
    | Some dfind -> DfindLib.get_changes dfind in
  if SSet.is_empty raw_updates then
    i, rechecked_count, env
  else
    let updates = Program.process_updates genv env raw_updates in
    let env, rechecked = recheck genv env updates in
    let rechecked_count =
      rechecked_count + (Relative_path.Set.cardinal rechecked)
    in
    recheck_loop (i + 1) rechecked_count genv env

let recheck_loop = recheck_loop 0 0

let serve genv env socket =
  let root = ServerArgs.root genv.options in
  let env = ref env in
  while true do
    let lock_file = ServerFiles.lock_file root in
    if not (Lock.grab lock_file) then
      (Hh_logger.log "Lost lock; terminating.\n%!";
       HackEventLogger.lock_stolen lock_file;
       Exit_status.(exit Lock_stolen));
    ServerPeriodical.call_before_sleeping();
    let has_client = sleep_and_check socket in
    let start_t = Unix.time () in
    let loop_count, rechecked_count, new_env = recheck_loop genv !env in
    env := new_env;
    if rechecked_count > 0 then
      HackEventLogger.recheck_end start_t loop_count rechecked_count;
    if has_client then handle_connection genv !env socket;
    ServerEnv.invoke_async_queue ();
    EventLogger.flush ();
  done

let load genv filename to_recheck =
  let chan = open_in filename in
  let env = Marshal.from_channel chan in
  Program.unmarshal chan;
  close_in chan;
  SharedMem.load (filename^".sharedmem");
  HackEventLogger.load_read_end filename;
  let to_recheck =
    List.rev_append (BuildMain.get_all_targets ()) to_recheck in
  let paths_to_recheck =
    List.map to_recheck (Relative_path.concat Relative_path.Root) in
  let updates =
    List.fold_left paths_to_recheck
      ~f:(fun acc update -> Relative_path.Set.add update acc)
      ~init:Relative_path.Set.empty in
  let start_t = Unix.time () in
  let env, rechecked = recheck genv env updates in
  let rechecked_count = Relative_path.Set.cardinal rechecked in
  HackEventLogger.load_recheck_end start_t rechecked_count;
  env

let run_load_script genv env cmd =
  try
    let cmd =
      sprintf
        "%s %s %s"
        (Filename.quote (Path.to_string cmd))
        (Filename.quote (Path.to_string (ServerArgs.root genv.options)))
        (Filename.quote Build_id.build_id_ohai) in
    Hh_logger.log "Running load script: %s\n%!" cmd;
    let state_fn, to_recheck =
      let do_fn () =
        let ic = Unix.open_process_in cmd in
        let state_fn =
          try input_line ic
          with End_of_file -> raise State_not_found
        in
        let to_recheck = ref [] in
        begin
          try
            while true do
              to_recheck := input_line ic :: !to_recheck
            done
          with End_of_file -> ()
        end;
        assert (Unix.close_process_in ic = Unix.WEXITED 0);
        state_fn, !to_recheck
      in
      with_timeout
        (ServerConfig.load_script_timeout genv.config)
        ~on_timeout:(fun _ -> failwith "Load script timed out")
        ~do_:do_fn
    in
    Hh_logger.log
      "Load state found at %s. %d files to recheck\n%!"
      state_fn (List.length to_recheck);
    HackEventLogger.load_script_done ();
    let env = load genv state_fn to_recheck in
    HackEventLogger.init_done "load";
    env
  with
  | State_not_found ->
     Hh_logger.log "Load state not found!";
     Hh_logger.log "Starting from a fresh state instead...";
     let env = Program.init genv env in
     HackEventLogger.init_done "load_state_not_found";
     env
  | e ->
     let msg = Printexc.to_string e in
     Hh_logger.log "Load error: %s" msg;
     Printexc.print_backtrace stderr;
     Hh_logger.log "Starting from a fresh state instead...";
     HackEventLogger.load_failed msg;
     let env = Program.init genv env in
     HackEventLogger.init_done "load_error";
     env

let create_program_init genv env = fun () ->
  match ServerConfig.load_script genv.config with
  | None ->
     let env = Program.init genv env in
     HackEventLogger.init_done "fresh";
     env
  | Some load_script -> run_load_script genv env load_script

let save _genv env fn =
  let chan = open_out_no_fail fn in
  Marshal.to_channel chan env [];
  Program.marshal chan;
  close_out_no_fail fn chan;
  (* We cannot save the shared memory to `chan` because the OCaml runtime
   * does not expose the underlying file descriptor to C code; so we use
   * a separate ".sharedmem" file. *)
  SharedMem.save (fn^".sharedmem");
  HackEventLogger.init_done "save"

(* The main entry point of the daemon
 * the only trick to understand here, is that env.modified is the set
 * of files that changed, it is only set back to SSet.empty when the
 * type-checker succeeded. So to know if there is some work to be done,
 * we look if env.modified changed.
 *)
let main options config =
  let root = ServerArgs.root options in
  HackEventLogger.init root (Unix.time ());
  Program.preinit ();
  SharedMem.init (ServerConfig.sharedmem_config config);
  (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
   * someone C-c the client.
   *)
  if not Sys.win32 then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  PidLog.init (ServerFiles.pids_file root);
  PidLog.log ~reason:"main" (Unix.getpid());
  let watch_paths = root :: Program.get_watch_paths options in
  let genv = ServerEnvBuild.make_genv options config watch_paths in
  let env = ServerEnvBuild.make_env options config in
  let program_init = create_program_init genv env in
  let is_check_mode = ServerArgs.check_mode genv.options in
  if is_check_mode then
    let env = program_init () in
    Option.iter (ServerArgs.save_filename genv.options) (save genv env);
    Program.run_once_and_exit genv env
  else
    (* Make sure to lock the lockfile before doing *anything*, especially
     * opening the socket. *)
    if not (Lock.grab (ServerFiles.lock_file root)) then begin
      Hh_logger.log "Error: another server is already running?\n";
      Exit_status.(exit Server_already_exists);
    end;
    (* Open up a server on the socket before we go into MainInit -- the client
     * will try to connect to the socket as soon as we lock the init lock. We
     * need to have the socket open now (even if we won't actually accept
     * connections until init is done) so that the client can try to use the
     * socket and get blocked on it -- otherwise, trying to open a socket with
     * no server on the other end is an immediate error. *)
    let socket = Socket.init_unix_socket (ServerFiles.socket_file root) in
    let env = MainInit.go options program_init in
    serve genv env socket

let monitor_daemon options f =
  let log_link = ServerFiles.log_link (ServerArgs.root options) in
  (try Sys.rename log_link (log_link ^ ".old") with _ -> ());
  let log_file = ServerFiles.make_link_of_timestamped log_link in
  let {Daemon.pid; _} = Daemon.fork begin fun (_ic, _oc) ->
    ignore @@ Unix.setsid ();
    let {Daemon.pid; _} = Daemon.fork ~log_file f in
    let _pid, proc_stat = Unix.waitpid [] pid in
    (match proc_stat with
    | Unix.WEXITED 0 -> ()
    | _ -> HackEventLogger.bad_exit proc_stat)
  end in
  Printf.eprintf "Spawned %s (child pid=%d)\n" Program.name pid;
  Printf.eprintf "Logs will go to %s\n%!" log_link;
  ()

let start () =
  let options = Program.parse_options () in
  Relative_path.set_path_prefix Relative_path.Root (ServerArgs.root options);
  let config = Program.load_config () in
  if ServerArgs.should_detach options
  then monitor_daemon options (fun (_ic, _oc) -> main options config)
  else main options config
