(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Stack_utils

let chunk_size = 65536

(** Reuse the buffer for reading. Just an allocation optimization. *)
let buffer = String.create chunk_size

(** Read from the FD if there is something to be read. FD is a reference
 * so when EOF is read from it, it is set to None. *)
let rec maybe_consume fd_ref acc =
  Option.iter !fd_ref ~f:begin fun fd ->
    match Unix.select [fd] [] [] 0.0 with
    | [], _, _ -> ()
    | _ ->
      let bytes_read = Unix.read fd buffer 0 chunk_size in
      if bytes_read = 0 then
        (** EOF reached. *)
        fd_ref := None
      else
        let chunk = String.sub buffer 0 bytes_read in
        Stack.push chunk acc;
        maybe_consume fd_ref acc
  end

let filter_none refs =
  List.fold_left refs ~init:[] ~f:(fun acc ref ->
    match !ref with
    | None -> acc
    | Some x -> x :: acc
  )

(** Non-blockingly consumes from pipes and non-blockingly waitpids.
 * Accumulators and process_status references are mutated accordingly. *)
let read_and_wait_pid_nonblocking process =
  let open Process_types in
  let {
  stdin_fd = _stdin_fd;
  stdout_fd;
  stderr_fd;
  process_status;
  acc;
  acc_err; } = process in
  match !process_status with
  | Process_exited _ ->
    ()
  | Process_running pid ->
    maybe_consume stdout_fd acc;
    maybe_consume stderr_fd acc_err;
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
      ()
    | _, status ->
      let () = process_status := Process_exited status in
      (** Process has exited. Non-blockingly consume residual output. *)
      let () = maybe_consume stdout_fd acc in
      let () = maybe_consume stderr_fd acc_err in
      ()

let is_ready process =
  read_and_wait_pid_nonblocking process;
  let open Process_types in
  match !(process.process_status) with
  | Process_running _ -> false
  | Process_exited _ -> true

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
let rec read_and_wait_pid process =
  let open Process_types in
  let {
  stdin_fd = _stdin_fd;
  stdout_fd;
  stderr_fd;
  process_status;
  acc;
  acc_err; } = process in
  read_and_wait_pid_nonblocking process;
  match !process_status with
  | Process_exited status ->
    status, (Stack.merge_bytes acc), (Stack.merge_bytes acc_err)
  | Process_running pid ->
  let fds = filter_none [stdout_fd; stderr_fd;] in
  if fds = []
  then
    (** EOF reached for all FDs. Blocking wait. *)
    let _, status = Unix.waitpid [] pid in
    let () = process_status := Process_exited status in
    status, (Stack.merge_bytes acc), (Stack.merge_bytes acc_err)
  else
    (** EOF hasn't been reached for all FDs. Here's where we switch from
     * reading the pipes to attempting a non-blocking waitpid. *)
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
      (** Process hasn't exited. We want to avoid a spin-loop
       * alternating between non-blocking read from pipes and
       * non-blocking waitpid, so we insert a select here. *)
      let _, _, _ = Unix.select fds [] [] 0.1 in
      (** And here we switch from waitpid back to reading. *)
      read_and_wait_pid process
    | _, status ->
      (** Process has exited. Non-blockingly consume residual output. *)
      let () = maybe_consume stdout_fd acc in
      let () = maybe_consume stderr_fd acc_err in
      let () = process_status := Process_exited status in
      status, (Stack.merge_bytes acc), (Stack.merge_bytes acc_err)

let exec prog ?env args =
  let args = Array.of_list (prog :: args) in
  let stdin_child, stdin_parent = Unix.pipe () in
  let stdout_parent, stdout_child = Unix.pipe () in
  let stderr_parent, stderr_child = Unix.pipe () in
  let pid = match env with
  | None ->
    Unix.create_process prog args stdin_child stdout_child stderr_child
  | Some env ->
    Unix.create_process_env prog args
      (Array.of_list env) stdin_child stdout_child stderr_child
  in
  Unix.close stdin_child;
  Unix.close stdout_child;
  Unix.close stderr_child;
  {
    Process_types.stdin_fd = ref @@ Some stdin_parent;
    stdout_fd = ref @@ Some stdout_parent;
    stderr_fd = ref @@ Some stderr_parent;
    acc = Stack.create ();
    acc_err = Stack.create ();
    process_status = ref @@ Process_types.Process_running pid;
  }
