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

type recheck_loop_acc = {
  rechecked_batches : int;
  rechecked_count : int;
  (* includes dependencies *)
  total_rechecked_count : int;
}

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

  let wakeup_client oc msg =
    Option.iter oc
      ~f:(fun oc ->
          try
            output_string oc (msg ^ "\n");
            flush oc
          with _ ->
            (* In case the client don't care... *)
            ())

  let close_waiting_channel oc =
    Option.iter oc
      ~f:(fun oc ->
          try close_out oc
          with exn -> Printf.eprintf "Close: %S\n%!" (Printexc.to_string exn))

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun =
    let waiting_channel =
      Option.map
        (ServerArgs.waiting_client options)
        ~f:Handle.to_out_channel in
    let root = ServerArgs.root options in
    let t = Unix.gettimeofday () in
    Hh_logger.log "Initializing Server (This might take some time)";
    grab_init_lock root;
    wakeup_client waiting_channel "starting";
    (* note: we only run periodical tasks on the root, not extras *)
    ServerIdle.init root;
    let env = init_fun () in
    release_init_lock root;
    Hh_logger.log "Server is READY";
    wakeup_client waiting_channel "ready";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    close_waiting_channel waiting_channel;
    env
end

module type SERVER_PROGRAM = sig
  val preinit : unit -> unit
  val init : genv -> env -> env
  val run_once_and_exit : genv -> env -> unit
  val should_recheck : Relative_path.t -> bool
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> Relative_path.Set.t
  val recheck: genv -> env -> Relative_path.Set.t -> env * int
  val post_recheck_hook: genv -> env -> env -> Relative_path.Set.t -> unit
  val parse_options: unit -> ServerArgs.options
  val get_watch_paths: ServerArgs.options -> Path.t list
  val name: string
  val config_filename : unit -> Relative_path.t
  val load_config : unit -> ServerConfig.t
  val validate_config : genv -> bool
  val handle_client : genv -> env -> (in_channel * out_channel) -> unit
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

    let process_updates genv _env updates =
      let root = Path.to_string @@ ServerArgs.root genv.options in
      (* Because of symlinks, we can have updates from files that aren't in
       * the .hhconfig directory *)
      let updates = SSet.filter (fun p -> str_starts_with p root) updates in
      Relative_path.(relativize_set Root updates)

    let should_recheck update =
      FindUtils.is_php (Relative_path.suffix update)

    let recheck genv old_env typecheck_updates =
      if Relative_path.Set.is_empty typecheck_updates then
        old_env, 0
      else begin
        let failed_parsing =
          Relative_path.Set.union typecheck_updates old_env.failed_parsing in
        let check_env = { old_env with failed_parsing = failed_parsing } in
        let new_env, total_rechecked = ServerTypeCheck.check genv check_env in
        touch_stamp_errors old_env.errorl new_env.errorl;
        new_env, total_rechecked
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
    Program.handle_client genv env (ic, oc)
  with
  | Sys_error("Broken pipe") ->
    shutdown_client (ic, oc)
  | e ->
    let msg = Printexc.to_string e in
    EventLogger.master_exception msg;
    Printf.fprintf stderr "Error: %s\n%!" msg;
    Printexc.print_backtrace stderr;
    shutdown_client (ic, oc)

let handle_connection genv env socket =
  ServerIdle.stamp_connection ();
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
  let env, total_rechecked = Program.recheck genv old_env to_recheck in
  Program.post_recheck_hook genv old_env env updates;
  env, to_recheck, total_rechecked

(* When a rebase occurs, dfind takes a while to give us the full list of
 * updates, and it often comes in batches. To get an accurate measurement
 * of rebase time, we use the heuristic that any changes that come in
 * right after one rechecking round finishes to be part of the same
 * rebase, and we don't log the recheck_end event until the update list
 * is no longer getting populated. *)
let rec recheck_loop acc genv env =
  let t = Unix.time () in
  let raw_updates =
    match genv.dfind with
    | None -> SSet.empty
    | Some dfind ->
        (try
          with_timeout 120
            ~on_timeout:(fun _ -> Exit_status.(exit Dfind_unresponsive))
            ~do_:(fun () -> DfindLib.get_changes dfind)
        with _ -> Exit_status.(exit Dfind_died))
  in
  if SSet.is_empty raw_updates then
    acc, env
  else begin
    HackEventLogger.dfind_returned t (SSet.cardinal raw_updates);
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

let serve genv env socket =
  let root = ServerArgs.root genv.options in
  let env = ref env in
  while true do
    let lock_file = ServerFiles.lock_file root in
    if not (Lock.grab lock_file) then
      (Hh_logger.log "Lost lock; terminating.\n%!";
       HackEventLogger.lock_stolen lock_file;
       Exit_status.(exit Lock_stolen));
    let has_client = sleep_and_check socket in
    let has_parsing_hook = !ServerTypeCheck.hook_after_parsing <> None in
    if not has_client && not has_parsing_hook
    then ServerIdle.go ();
    let start_t = Unix.time () in
    let acc, new_env = recheck_loop genv !env in
    env := new_env;
    if acc.rechecked_batches > 0 then
      HackEventLogger.recheck_end start_t
        has_parsing_hook
        acc.rechecked_batches acc.rechecked_count acc.total_rechecked_count;
    if has_client then handle_connection genv !env socket;
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
  let env, rechecked, total_rechecked = recheck genv env updates in
  let rechecked_count = Relative_path.Set.cardinal rechecked in
  HackEventLogger.load_recheck_end start_t rechecked_count total_rechecked;
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
let daemon_main options =
  (* The OCaml default is 500, but we care about minimizing the memory
   * overhead *)
  let gc_control = Gc.get () in
  Gc.set {gc_control with Gc.max_overhead = 200};
  Relative_path.set_path_prefix Relative_path.Root (ServerArgs.root options);
  let config = Program.load_config () in
  let root = ServerArgs.root options in
  if Sys_utils.is_test_mode ()
  then EventLogger.init (Daemon.devnull ()) 0.0
  else HackEventLogger.init root (Unix.time ());
  Option.iter
    (ServerArgs.waiting_client options)
    ~f:(fun handle ->
        let fd = Handle.wrap_handle handle in
        Unix.set_close_on_exec fd);
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

let main_entry =
  Daemon.register_entry_point
    "main"
    (fun options (_ic, _oc) -> daemon_main options)

let monitor_entry =
  Daemon.register_entry_point
    "monitor"
    (ServerMonitor.go main_entry)

let monitor_daemon options =
  let log_link = ServerFiles.log_link (ServerArgs.root options) in
  (try Sys.rename log_link (log_link ^ ".old") with _ -> ());
  let log_file = ServerFiles.make_link_of_timestamped log_link in
  let {Daemon.pid; _} = Daemon.spawn monitor_entry (options, log_file) in
  Printf.eprintf "Spawned %s (child pid=%d)\n" Program.name pid;
  (* We are not using symlink on Windows (see Sys_utils.symlink),
     so we announce the `log_file` to the user. The `log_link`
     is only read by the client. *)
  Printf.eprintf "Logs will go to %s\n%!"
    (if Sys.win32 then log_file else log_link);
  ()

let start () =
  Daemon.check_entry_point (); (* this call might not return *)
  let options = Program.parse_options () in
  if ServerArgs.should_detach options
  then monitor_daemon options
  else daemon_main options
