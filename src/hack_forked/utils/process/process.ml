(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Ocaml_overrides
open Stack_utils

module Entry = struct
  type 'param t = ('param, unit, unit) Daemon.entry

  let register name entry =
    let daemon_entry = Daemon.register_entry_point name (fun params _channels -> entry params) in
    daemon_entry
end

(** In the blocking read_and_wait_pid call, we alternate between
 * non-blocking consuming of output and a nonblocking waitpid.
 * To avoid pegging the CPU at 100%, sleep for a short time between
 * those. *)
let sleep_seconds_per_retry = 0.04

let chunk_size = 65536

(** Reuse the buffer for reading. Just an allocation optimization. *)
let buffer = Bytes.create chunk_size

let env_to_array (env : Process_types.environment) : string array option =
  match env with
  | Process_types.Default -> None
  | Process_types.Empty -> Some [||]
  | Process_types.Augment augments_to_env ->
    (* deduping the env is not necessary. glibc putenv/getenv will grab the first
     * one *)
    let fullenv = Array.append (Array.of_list augments_to_env) (Unix.environment ()) in
    Some fullenv
  | Process_types.Replace fullenv -> Some (Array.of_list fullenv)

let status_to_string (status : Unix.process_status) : string =
  match status with
  | Unix.WEXITED i -> Printf.sprintf "Unix.WEXITED %d" i
  | Unix.WSIGNALED i -> Printf.sprintf "Unix.WSIGNALED %d" i
  | Unix.WSTOPPED i -> Printf.sprintf "Unix.WSTOPPED %d" i

(* make_result returns either (stdout,stderr) or a failure. *)
let make_result (status : Unix.process_status) (stdout : string) (stderr : string) :
    Process_types.process_result =
  Process_types.(
    match status with
    | Unix.WEXITED 0 -> Ok { stdout; stderr }
    | Unix.WEXITED _
    | Unix.WSIGNALED _
    | Unix.WSTOPPED _ ->
      Error (Abnormal_exit { status; stdout; stderr }))

(** Read from the FD if there is something to be read. FD is a reference
 * so when EOF is read from it, it is set to None. *)
let rec maybe_consume
    ?(max_time : float = 0.0)
    (fd_ref : Unix.file_descr option ref)
    (acc : string Stack_utils.Stack.t) : unit =
  if Float.(max_time < 0.0) then
    ()
  else
    let start_t = Unix.time () in
    Base.Option.iter !fd_ref ~f:(fun fd ->
        match Unix.select [fd] [] [] max_time with
        | ([], _, _) -> ()
        | _ ->
          let bytes_read = Unix.read fd buffer 0 chunk_size in
          if bytes_read = 0 then (
            (* EOF reached. *)
            Unix.close fd;
            fd_ref := None
          ) else
            let chunk = String.sub (Bytes.to_string buffer) 0 bytes_read in
            Stack.push chunk acc;
            let consumed_t = Unix.time () -. start_t in
            let max_time = max_time -. consumed_t in
            maybe_consume ~max_time fd_ref acc)

(**
 * Read data from stdout and stderr until EOF is reached. Waits for
 * process to terminate returns the stderr and stdout
 * and stderr.
 *
 * Idempotent.
 *
 * If process exits with something other than (Unix.WEXITED 0), will return a
 * Error
 *)
let read_and_wait_pid_nonblocking (process : Process_types.t) : unit =
  Process_types.(
    let { stdin_fd = _stdin_fd; stdout_fd; stderr_fd; lifecycle; acc; acc_err; _ } = process in
    match !lifecycle with
    | Lifecycle_killed_due_to_overflow_stdin
    | Lifecycle_exited _ ->
      ()
    | Lifecycle_running { pid } ->
      maybe_consume stdout_fd acc;
      maybe_consume stderr_fd acc_err;
      (match Unix.waitpid [Unix.WNOHANG] pid with
      | (0, _) -> ()
      | (_, status) ->
        let () = lifecycle := Lifecycle_exited status in
        (* Process has exited. Non-blockingly consume residual output. *)
        let () = maybe_consume stdout_fd acc in
        let () = maybe_consume stderr_fd acc_err in
        ()))

(** Returns true if read_and_close_pid would be nonblocking. *)
let is_ready (process : Process_types.t) : bool =
  read_and_wait_pid_nonblocking process;
  Process_types.(
    match !(process.lifecycle) with
    | Lifecycle_running _ -> false
    | Lifecycle_killed_due_to_overflow_stdin
    | Lifecycle_exited _ ->
      true)

let kill_and_cleanup_fds (pid : int) (fds : Unix.file_descr option ref list) : unit =
  Unix.kill pid Sys.sigkill;
  let maybe_close fd_ref =
    Base.Option.iter !fd_ref ~f:(fun fd ->
        Unix.close fd;
        fd_ref := None)
  in
  List.iter fds ~f:maybe_close

(**
 * Consumes from stdout and stderr pipes and waitpids on the process.
 * Returns immediately if process has already been waited on (so this
 * function is idempotent).
 *
 * The implementation is a little complicated because:
 *   (1) The pipe can get filled up and the child process will pause
 *       until it's emptied out.
 *   (2) If the child process itself forks a grandchild, the
 *       granchild will unknowingly inherit the pipe's file descriptors;
 *       in this case, the pipe will not provide an EOF as you'd expect.
 *
 * Due to (1), we can't just blockingly waitpid followed by reading the
 * data from the pipe.
 *
 * Due to (2), we can't just read data from the pipes until an EOF is
 * reached and then do a waitpid.
 *
 * We must do some weird alternating between them.
 *)
let rec read_and_wait_pid ~(retries : int) (process : Process_types.t) :
    Process_types.process_result =
  Process_types.(
    let { stdin_fd = _stdin_fd; stdout_fd; stderr_fd; lifecycle; acc; acc_err; _ } = process in
    read_and_wait_pid_nonblocking process;
    match !lifecycle with
    | Lifecycle_exited status ->
      make_result status (Stack.merge_bytes acc) (Stack.merge_bytes acc_err)
    | Lifecycle_killed_due_to_overflow_stdin -> Error Overflow_stdin
    | Lifecycle_running { pid } ->
      let fds = List.rev_filter_map ~f:( ! ) [stdout_fd; stderr_fd] in
      if List.is_empty fds then
        (* EOF reached for all FDs. Blocking wait. *)
        let (_, status) = Unix.waitpid [] pid in
        let () = lifecycle := Lifecycle_exited status in
        make_result status (Stack.merge_bytes acc) (Stack.merge_bytes acc_err)
      else
        (* Consume output to clear the buffers which might
         * be blocking the process from continuing. *)
        let () = maybe_consume ~max_time:(sleep_seconds_per_retry /. 2.0) stdout_fd acc in
        let () = maybe_consume ~max_time:(sleep_seconds_per_retry /. 2.0) stderr_fd acc_err in
        (* EOF hasn't been reached for all FDs. Here's where we switch from
         * reading the pipes to attempting a non-blocking waitpid. *)
        (match Unix.waitpid [Unix.WNOHANG] pid with
        | (0, _) ->
          if retries <= 0 then
            let () = kill_and_cleanup_fds pid [stdout_fd; stderr_fd] in
            let stdout = Stack.merge_bytes acc in
            let stderr = Stack.merge_bytes acc_err in
            Error (Timed_out { stdout; stderr })
          else
            (* And here we switch from waitpid back to reading. *)
            read_and_wait_pid ~retries:(retries - 1) process
        | (_, status) ->
          (* Process has exited. Non-blockingly consume residual output. *)
          let () = maybe_consume stdout_fd acc in
          let () = maybe_consume stderr_fd acc_err in
          let () = lifecycle := Lifecycle_exited status in
          make_result status (Stack.merge_bytes acc) (Stack.merge_bytes acc_err)))

let read_and_wait_pid ~(timeout : int) (process : Process_types.t) : Process_types.process_result =
  let retries = Float.of_int timeout /. sleep_seconds_per_retry |> Int.of_float in
  read_and_wait_pid ~retries process

let failure_msg (failure : Process_types.failure) : string =
  Process_types.(
    match failure with
    | Timed_out { stdout; stderr } ->
      Printf.sprintf "Process timed out. stdout:\n%s\nstderr:\n%s\n" stdout stderr
    | Abnormal_exit { stdout; stderr; _ } ->
      Printf.sprintf "Process exited abnormally. stdout:\n%s\nstderr:\n%s\n" stdout stderr
    | Overflow_stdin -> Printf.sprintf "Process_aborted_input_too_large")

let send_input_and_form_result
    ?(input : string option)
    ~(info : Process_types.invocation_info)
    ~(pid : int)
    ~(stdin_parent : Unix.file_descr)
    ~(stdout_parent : Unix.file_descr)
    ~(stderr_parent : Unix.file_descr) : Process_types.t =
  Process_types.(
    let input_succeeded =
      match input with
      | None -> true
      | Some input ->
        let written = Unix.write stdin_parent input 0 (Bytes.length input) in
        written = Bytes.length input
    in
    let lifecycle =
      if input_succeeded then
        Lifecycle_running { pid }
      else
        let () = Unix.kill pid Sys.sigkill in
        Lifecycle_killed_due_to_overflow_stdin
    in
    Unix.close stdin_parent;
    {
      info;
      stdin_fd = ref @@ None;
      stdout_fd = ref @@ Some stdout_parent;
      stderr_fd = ref @@ Some stderr_parent;
      acc = Stack.create ();
      acc_err = Stack.create ();
      lifecycle = ref @@ lifecycle;
    })

(**
 * Launches a process, optionally modifying the environment variables with ~env
 *)
let exec_no_chdir
    ~(prog : string)
    ?(input : string option)
    ~(env : Process_types.environment option)
    (args : string list) : Process_types.t =
  let info =
    {
      Process_types.name = prog;
      args;
      stack =
        Utils.Callstack (Caml.Printexc.get_callstack 100 |> Caml.Printexc.raw_backtrace_to_string);
    }
  in
  let args = Array.of_list (prog :: args) in
  let (stdin_child, stdin_parent) = Unix.pipe () in
  let (stdout_parent, stdout_child) = Unix.pipe () in
  let (stderr_parent, stderr_child) = Unix.pipe () in
  Unix.set_close_on_exec stdin_parent;
  Unix.set_close_on_exec stdout_parent;
  Unix.set_close_on_exec stderr_parent;

  let env = Base.Option.value env ~default:Process_types.Default in
  let pid =
    match env_to_array env with
    | None -> Unix.create_process prog args stdin_child stdout_child stderr_child
    | Some env -> Unix.create_process_env prog args env stdin_child stdout_child stderr_child
  in
  Unix.close stdin_child;
  Unix.close stdout_child;
  Unix.close stderr_child;
  send_input_and_form_result ?input ~info ~pid ~stdin_parent ~stdout_parent ~stderr_parent

let register_entry_point = Entry.register

type chdir_params = {
  cwd: string;
  prog: string;
  env: Process_types.environment option;
  args: string list;
}

(** Wraps a entry point inside a Process, so we get Process's
 * goodness for free (read_and_wait_pid and is_ready). The entry will be
 * spawned into a separate process. *)
let run_entry ?(input : string option) (entry : 'a Entry.t) (params : 'a) : Process_types.t =
  let (stdin_child, stdin_parent) = Unix.pipe () in
  let (stdout_parent, stdout_child) = Unix.pipe () in
  let (stderr_parent, stderr_child) = Unix.pipe () in
  let info =
    {
      Process_types.name = Daemon.name_of_entry entry;
      args = [];
      stack =
        Utils.Callstack (Caml.Printexc.get_callstack 100 |> Caml.Printexc.raw_backtrace_to_string);
    }
  in
  let ({ Daemon.pid; _ } as daemon) =
    Daemon.spawn (stdin_child, stdout_child, stderr_child) entry params
  in
  Daemon.close daemon;
  send_input_and_form_result ?input ~info ~pid ~stdin_parent ~stdout_parent ~stderr_parent

let chdir_main (p : chdir_params) : 'a =
  Unix.chdir p.cwd;

  let args = Array.of_list (p.prog :: p.args) in
  (* NOTE: to preserve original behavior of this code, empty environment is the default here.
     This is different from exec_no_chdir where the default is Default (current environment). *)
  let env = Base.Option.value p.env ~default:Process_types.Empty in
  let env = env_to_array env in
  match env with
  | None -> Unix.execvp p.prog args
  | Some env -> Unix.execvpe p.prog args env

let chdir_entry : (chdir_params, 'a, 'b) Daemon.entry = Entry.register "chdir_main" chdir_main

let exec
    (prog : string)
    ?(input : string option)
    ?(env : Process_types.environment option)
    (args : string list) : Process_types.t =
  exec_no_chdir ~prog ?input ~env args

let exec_with_working_directory
    ~(dir : string)
    (prog : string)
    ?(input : string option)
    ?(env : Process_types.environment option)
    (args : string list) : Process_types.t =
  run_entry ?input chdir_entry { cwd = dir; prog; env; args }
