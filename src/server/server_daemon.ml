(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module Server_files = Server_files_js

type daemon_msg =
  | Starting
  | Ready

type waiting_channel = daemon_msg Daemon.out_channel

type entry_point = (
  SharedMem_js.config * Options.t * FlowEventLogger.logging_context * string array,
  in_channel,
  daemon_msg
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

(* The server can communicate with the process that forked it over a pipe.
 * The current scheme has it write a message when it starts up and has the
 * lock and then write another message when it has finished initializing.
 * It's up to the forking process whether it cares to wait for the
 * initialization to complete *)
let rec wait_loop ~should_wait child_pid ic =
  let msg = try
    Daemon.from_channel ic
    with End_of_file ->
    (* The pipe broke before we got the alls-clear from the server. What kind
     * of things could go wrong. Well we check the lock before forking the
     * server, but maybe by the time the server started someone else had
     * grabbed the lock, so it exited. I'm sure there's a million other
     * things that could have gone wrong *)
    let pid, status =
      match Unix.(waitpid [ WNOHANG; WUNTRACED; ] child_pid) with
      | 0, _ ->
          (* Sometimes the End_of_file races the child process actually
           * exiting. In case that's happening here, let's give the child 1
           * second more to die *)
          Unix.sleep 1;
          Unix.(waitpid [ WNOHANG; WUNTRACED; ] child_pid)
      | pid, status -> pid, status in
    let exit_code =  FlowExitStatus.Server_start_failed status in
    let msg, exit_code = if pid = 0
    (* The server is still alive...not sure what happened *)
    then
      "Error: Failed to start server for some unknown reason.", exit_code
    (* The server is dead. Shucks. *)
    else
      let reason, exit_code = match status with
      | Unix.WEXITED code ->
          if code = FlowExitStatus.(error_code Lock_stolen)
          then
            (* Sometimes when we actually go to start the server we find a
             * server already running (race condition). If so, we can just
             * forward that error code *)
            "There is already a server running.",
            FlowExitStatus.Lock_stolen
          else if code = FlowExitStatus.(error_code Out_of_shared_memory)
          then
            "The server is failed to allocate shared memory.",
            FlowExitStatus.Out_of_shared_memory
          else
            spf "exited prematurely with code %d." code, exit_code
      | Unix.WSIGNALED signal ->
          let signal_name = Sys_utils.name_of_signal signal in
          spf "The server was killed prematurely with signal %s." signal_name,
          exit_code
      | Unix.WSTOPPED signal ->
          spf "The server was stopped prematurely with signal %d." signal,
          exit_code
      in spf "Error: Failed to start server. %s" reason, exit_code
    in FlowExitStatus.(exit ~msg exit_code)
  in
  if should_wait && msg <> Ready
  then wait_loop ~should_wait child_pid ic

let new_entry_point =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    Printf.sprintf "main_%d" !cpt

let register_entry_point
  (main:
    ?waiting_channel:waiting_channel ->
    shared_mem_config:SharedMem_js.config ->
    Options.t ->
    unit)
: entry_point =
  Daemon.register_entry_point
    (new_entry_point ())
    (fun (shared_mem_config, options, logging_context, argv) (ic, waiting_channel) ->
      ignore(Sys_utils.setsid());
      Daemon.close_in ic;
      LoggingUtils.set_hh_logger_min_level options;
      Hh_logger.info "argv=%s" (argv |> Array.to_list |> String.concat " ");
      FlowEventLogger.restore_context logging_context;
      FlowEventLogger.init_flow_command ~version:Flow_version.version;
      main ?waiting_channel:(Some waiting_channel) ~shared_mem_config options)

let daemonize ~wait ~log_file ~shared_mem_config ~options ?on_spawn main_entry =
  (* Let's make sure this isn't all for naught before we fork *)
  let root = Options.root options in
  let tmp_dir = Options.temp_dir options in
  let lock = Server_files.lock_file ~tmp_dir root in
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
  let {Daemon.pid; channels = (waiting_channel_ic, waiting_channel_oc)} =
    Daemon.spawn (null_fd, log_fd, log_fd) (main_entry) (
      shared_mem_config,
      options,
      FlowEventLogger.get_context (),
      Sys.argv
    )
  in
  (* detach ourselves from the parent process *)
  Daemon.close_out waiting_channel_oc;
  (* let original parent exit *)
  let pretty_pid = Sys_utils.pid_of_handle pid in
  Option.call pretty_pid ~f:on_spawn;
  wait_loop ~should_wait:wait pid waiting_channel_ic

(* Sends a message to the parent that forked us *)
let wakeup_client oc msg =
  Option.iter oc begin fun oc ->
    try
      Daemon.to_channel oc msg
    with
    (* The client went away *)
    | Sys_error msg
      when msg = "Broken pipe"  || msg = "Invalid argument" -> ()
    | e ->
      prerr_endlinef "wakeup_client: %s" (Printexc.to_string e)
  end

(* Closes the connection to the parent that forked us *)
let close_waiting_channel oc =
  Option.iter oc begin fun oc ->
    try
      Daemon.close_out oc
    with
    (* The client went away *)
    | Sys_error msg
      when msg = "Broken pipe"  || msg = "Invalid argument" -> ()
    | e ->
      prerr_endlinef "close_waiting_channel: %s" (Printexc.to_string e)
  end
