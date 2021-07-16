(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

type start_function = ?waiting_fd:Unix.file_descr -> FlowServerMonitorOptions.t -> unit

(* When `flow start --wait` daemonizes the Flow server monitor, it listens over a pipe and waits
 * for the Flow server to finish initializing. These are the messages we send over the pipe *)
type wait_msg =
  | Starting  (** Monitor is up. All `flow start` commands wait for this *)
  | Ready  (** Server is done initializing. `flow start --wait` commands wait for this *)

type state = {
  monitor_options: FlowServerMonitorOptions.t;
  init_id: string;
  logging_context: FlowEventLogger.logging_context;
}

type entry_point = (state, unit, wait_msg) Daemon.entry

(* When the daemonized monitor process starts up, this is the first code it runs *)
let register_entry_point (start : start_function) : entry_point =
  Daemon.register_entry_point
    "monitor"
    (fun { monitor_options; init_id; logging_context } (ic, oc) ->
      (* Disassociate this process with the process that spawned it *)
      ignore (Sys_utils.setsid ());

      (* We never read from this channel, so close it *)
      Daemon.close_in ic;

      (* Set up various logging related things *)
      LoggingUtils.set_hh_logger_min_level monitor_options.FlowServerMonitorOptions.server_options;
      FlowEventLogger.restore_context logging_context;
      FlowEventLogger.set_command (Some "monitor");
      FlowEventLogger.init_flow_command ~init_id;

      let out_fd = Daemon.descr_of_out_channel oc in
      start ~waiting_fd:out_fd monitor_options)

(* The monitor can communicate with the process that spawned it over a pipe.
 * The current scheme has it write a message when it starts up and has the
 * lock and then write another message when it has finished initializing.
 * It's up to the forking process whether it cares to wait for the
 * initialization to complete *)
let rec wait_loop ~should_wait child_pid ic =
  let msg : wait_msg =
    try Marshal_tools.from_fd_with_preamble (Daemon.descr_of_in_channel ic) with
    | End_of_file ->
      (* The pipe broke before we got the all-clear from the monitor. What kind
       * of things could go wrong? Well we check the lock before forking the
       * monitor, but maybe by the time the monitor started someone else had
       * grabbed the lock, so it exited. I'm sure there's a million other
       * things that could have gone wrong *)
      let (pid, status) =
        match Unix.(waitpid [WNOHANG; WUNTRACED] child_pid) with
        | (0, _) ->
          (* Sometimes the End_of_file races the child process actually
           * exiting. In case that's happening here, let's give the child 1
           * second more to die *)
          Unix.sleep 1;
          Unix.(waitpid [WNOHANG; WUNTRACED] child_pid)
        | (pid, status) -> (pid, status)
      in
      let exit_code = Exit.Server_start_failed status in
      let (msg, exit_code) =
        if pid = 0 (* The monitor is still alive...not sure what happened *) then
          ("Error: Failed to start server for some unknown reason.", exit_code)
        (* The monitor is dead. Shucks. *)
        else
          let (reason, exit_code) =
            match status with
            | Unix.WEXITED code ->
              if code = Exit.(error_code Lock_stolen) then
                (* Sometimes when we actually go to start the monitor we find a
                 * monitor already running (race condition). If so, we can just
                 * forward that error code *)
                ("There is already a server running.", Exit.Lock_stolen)
              else if code = Exit.(error_code Out_of_shared_memory) then
                ("The server is failed to allocate shared memory.", Exit.Out_of_shared_memory)
              else
                (spf "exited prematurely with code %d." code, exit_code)
            | Unix.WSIGNALED signal ->
              let signal_name = Sys_utils.name_of_signal signal in
              (spf "The server was killed prematurely with signal %s." signal_name, exit_code)
            | Unix.WSTOPPED signal ->
              (spf "The server was stopped prematurely with signal %d." signal, exit_code)
          in
          (spf "Error: Failed to start server. %s" reason, exit_code)
      in
      Exit.(exit ~msg exit_code)
  in
  if should_wait && msg <> Ready then
    wait_loop ~should_wait child_pid ic
  else
    Daemon.close_in ic

let daemonize ~wait ~on_spawn ~init_id ~monitor_options (entry_point : entry_point) =
  let null_fd = Daemon.null_fd () in
  (* Daemon.spawn is creating a new process with /dev/null as both the stdout
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
  (if Sys.win32 then
    Unix.(
      try
        set_close_on_exec stdout;
        set_close_on_exec stderr
      with
      | Unix_error (EINVAL, _, _) -> ()));

  let root_str =
    monitor_options.FlowServerMonitorOptions.server_options |> Options.root |> Path.to_string
  in
  let { Daemon.pid; channels = (ic, oc) } =
    Daemon.spawn
      ~name:(spf "monitor for %s" root_str)
      (null_fd, null_fd, null_fd)
      entry_point
      { monitor_options; init_id; logging_context = FlowEventLogger.get_context () }
  in
  (* We never write to the child process so we can close this channel *)
  Daemon.close_out oc;

  let pretty_pid = Sys_utils.pid_of_handle pid in
  on_spawn pretty_pid;

  (* If wait is true, wait for the "Ready" message.
   * Otherwise, only wait for the "Starting message" *)
  wait_loop ~should_wait:wait pid ic
