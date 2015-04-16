(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Sys_utils
open ServerEnv

exception State_not_found

module type SERVER_PROGRAM = sig
  module EventLogger : sig
    val init: Path.path -> float -> unit
    val init_done: string -> unit
    val load_script_done: unit -> unit
    val load_read_end: string -> unit
    val load_recheck_end: unit -> unit
    val load_failed: string -> unit
    val lock_lost: Path.path -> string -> unit
    val lock_stolen: Path.path -> string -> unit
  end

  val preinit : unit -> unit
  val init : genv -> env -> env
  val run_once_and_exit : genv -> env -> unit
  (* filter a single updated file path *)
  val filter_update : genv -> env -> Relative_path.t -> bool
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> Relative_path.Set.t
  val recheck: genv -> env -> Relative_path.Set.t -> env
  val infer: env -> (ServerMsg.file_input * int * int) -> out_channel -> unit
  val suggest: string list -> out_channel -> unit
  val parse_options: unit -> ServerArgs.options
  val get_watch_paths: ServerArgs.options -> Path.path list
  val name: string
  val config_filename : unit -> Relative_path.t
  val load_config : unit -> ServerConfig.t
  val validate_config : genv -> bool
  val get_errors: ServerEnv.env -> Errors.t
  val handle_connection : genv -> env -> Unix.file_descr -> unit
  (* This is a hack for us to save / restore the global state that is not
   * already captured by ServerEnv *)
  val marshal : out_channel -> unit
  val unmarshal : in_channel -> unit
end

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

module MainInit : sig
  val go:
    Path.path ->        (* server root - one server at a time per root *)
    Path.path list ->   (* other watched paths *)
    (unit -> env) ->    (* init function to run while we have init lock *)
    env
end = struct

  let other_server_running() =
    Hh_logger.log "Error: another server is already running?\n";
    exit 1

  let grab_lock root =
    if not (Lock.grab root "lock")
    then other_server_running()

  let grab_init_lock root =
    ignore(Lock.grab root "init")

  let release_init_lock root =
    ignore(Lock.release root "init")

  (* This code is only executed when the options --check is NOT present *)
  let go root watch_paths init_fun =
    let t = Unix.gettimeofday () in
    grab_lock root;
    Hh_logger.log "Initializing Server (This might take some time)";
    grab_init_lock root;
    (* note: we only run periodical tasks on the root, not extras *)
    ServerPeriodical.init root;
    (* watch root and extra paths *)
    ServerDfind.dfind_init (root :: watch_paths);
    let env = init_fun () in
    release_init_lock root;
    Hh_logger.log "Server is READY";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)

module ServerMain (Program : SERVER_PROGRAM) : sig
  val start : unit -> unit
end = struct
  let sleep_and_check socket =
    let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
    ready_socket_l <> []

  let serve genv env socket =
    let root = ServerArgs.root genv.options in
    let env = ref env in
    while true do
      if not (Lock.check root "lock") then begin
        Hh_logger.log "Lost %s lock; reacquiring.\n" Program.name;
        Program.EventLogger.lock_lost root "lock";
        if not (Lock.grab root "lock")
        then
          Hh_logger.log "Failed to reacquire lock; terminating.\n";
          Program.EventLogger.lock_stolen root "lock";
          die()
      end;
      ServerHealth.check();
      ServerPeriodical.call_before_sleeping();
      let has_client = sleep_and_check socket in
      let updates = ServerDfind.get_updates () in
      let updates = Program.process_updates genv !env updates in
      let config = Program.config_filename () in
      if Relative_path.Set.mem config updates &&
        not (Program.validate_config genv) then begin
        Hh_logger.log
          "%s changed in an incompatible way; please restart %s.\n"
          (Relative_path.suffix config)
          Program.name;
        exit 4;
      end;
      env := Program.recheck genv !env updates;
      if has_client then Program.handle_connection genv !env socket;
    done

  let load genv filename to_recheck =
      let chan = open_in filename in
      let env = Marshal.from_channel chan in
      Program.unmarshal chan;
      close_in chan;
      SharedMem.load (filename^".sharedmem");
      Program.EventLogger.load_read_end filename;
      let to_recheck =
        List.rev_append (BuildMain.get_all_targets ()) to_recheck in
      let paths_to_recheck =
        rev_rev_map (Relative_path.concat Relative_path.Root) to_recheck
      in
      let updates = List.fold_left
        (fun acc update -> Relative_path.Set.add update acc)
        Relative_path.Set.empty
        paths_to_recheck in
      let updates =
        Relative_path.Set.filter (Program.filter_update genv env) updates in
      let env = Program.recheck genv env updates in
      Program.EventLogger.load_recheck_end ();
      env

  let run_load_script genv env cmd =
    try
      let cmd = Printf.sprintf "%s %s %s" cmd
        (Filename.quote (Path.string_of_path (ServerArgs.root genv.options)))
        (Filename.quote Build_id.build_id_ohai) in
      Hh_logger.log "Running load script: %s\n%!" cmd;
      let state_fn, to_recheck =
        with_timeout (ServerConfig.load_script_timeout genv.config)
        ~on_timeout:(fun _ -> failwith "Load script timed out")
        ~do_:begin fun () ->
          let ic = Unix.open_process_in cmd in
          let state_fn = begin
            try input_line ic
            with End_of_file -> raise State_not_found
          end in
          let to_recheck = ref [] in
          begin
            try while true do to_recheck := input_line ic :: !to_recheck done
            with End_of_file -> ()
          end;
          assert (Unix.close_process_in ic = Unix.WEXITED 0);
          state_fn, !to_recheck
        end in
      Hh_logger.log
        "Load state found at %s. %d files to recheck\n%!"
        state_fn (List.length to_recheck);
      Program.EventLogger.load_script_done ();
      let env = load genv state_fn to_recheck in
      Program.EventLogger.init_done "load";
      env
    with
    | State_not_found ->
        Hh_logger.log "Load state not found!";
        Hh_logger.log "Starting from a fresh state instead...";
        let env = Program.init genv env in
        Program.EventLogger.init_done "load_state_not_found";
        env
    | e ->
        let msg = Printexc.to_string e in
        Hh_logger.log "Load error: %s" msg;
        Printexc.print_backtrace stderr;
        Hh_logger.log "Starting from a fresh state instead...";
        Program.EventLogger.load_failed msg;
        let env = Program.init genv env in
        Program.EventLogger.init_done "load_error";
        env

  let create_program_init genv env = fun () ->
    match ServerConfig.load_script genv.config with
    | None ->
        let env = Program.init genv env in
        Program.EventLogger.init_done "fresh";
        env
    | Some load_script ->
        run_load_script genv env load_script

  let save _genv env fn =
    let chan = open_out_no_fail fn in
    Marshal.to_channel chan env [];
    Program.marshal chan;
    close_out_no_fail fn chan;
    (* We cannot save the shared memory to `chan` because the OCaml runtime
     * does not expose the underlying file descriptor to C code; so we use
     * a separate ".sharedmem" file. *)
    SharedMem.save (fn^".sharedmem");
    Program.EventLogger.init_done "save"

  (* The main entry point of the daemon
  * the only trick to understand here, is that env.modified is the set
  * of files that changed, it is only set back to SSet.empty when the
  * type-checker succeeded. So to know if there is some work to be done,
  * we look if env.modified changed.
  *)
  let main options config =
    let root = ServerArgs.root options in
    Program.EventLogger.init root (Unix.time ());
    Program.preinit ();
    SharedMem.init();
    (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
    * someone C-c the client.
    *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    PidLog.init root;
    PidLog.log ~reason:"main" (Unix.getpid());
    let genv = ServerEnvBuild.make_genv ~multicore:true options config in
    let env = ServerEnvBuild.make_env options config in
    let program_init = create_program_init genv env in
    let is_check_mode = ServerArgs.check_mode genv.options in
    if is_check_mode then
      let env = program_init () in
      Option.iter (ServerArgs.save_filename genv.options) (save genv env);
      Program.run_once_and_exit genv env
    else
      let watch_paths = Program.get_watch_paths options in
      let env = MainInit.go root watch_paths program_init in
      let socket = Socket.init_unix_socket root in
      serve genv env socket

  let get_log_file root =
    let tmp_dir = Tmp.get_dir() in
    let root_part = Path.slash_escaped_string_of_path root in
    Printf.sprintf "%s/%s.log" tmp_dir root_part

  let daemonize options =
    (* detach ourselves from the parent process *)
    let pid = Unix.fork() in
    if pid == 0
    then begin
      ignore(Unix.setsid());
      with_umask 0o111 begin fun () ->
        (* close stdin/stdout/stderr *)
        let fd = Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CREAT] 0o777 in
        Unix.dup2 fd Unix.stdin;
        Unix.close fd;
        let file = get_log_file (ServerArgs.root options) in
        (try Sys.rename file (file ^ ".old") with _ -> ());
        let fd = Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o666 in
        Unix.dup2 fd Unix.stdout;
        Unix.dup2 fd Unix.stderr;
        Unix.close fd
      end
      (* child process is ready *)
    end else begin
      (* let original parent exit *)
      Printf.eprintf "Spawned %s (child pid=%d)\n" (Program.name) pid;
      Printf.eprintf
        "Logs will go to %s\n%!" (get_log_file (ServerArgs.root options));
      raise Exit
    end

  let start () =
    let options = Program.parse_options () in
    let root = Path.string_of_path (ServerArgs.root options) in
    Relative_path.set_path_prefix Relative_path.Root root;
    let config = Program.load_config () in
    try
      if ServerArgs.should_detach options
      then daemonize options;
      main options config
    with Exit ->
      ()
end
