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

let chunk_size = 65536

(** Reuse the buffer for reading. Just an allocation optimization. *)
let buffer = String.create chunk_size

(** Read from the FD if there is something to be read. This should only be
 * done after the child process has exited. *)
let rec maybe_consume fd acc =
  match Unix.select [fd] [] [] 0.0 with
  | [], _, _ -> acc
  | _ ->
    let bytes_read = Unix.read fd buffer 0 chunk_size in
    if bytes_read = 0 then
      acc
    else
      let chunk = String.sub buffer 0 bytes_read in
      maybe_consume fd (chunk :: acc)

(** Recursively read from fds. If EOF is reached, remove that
 * fd from fds; termination when fds is empty.
 *
 * fd and err_fd helps us track which accumulator to put the read
 * data from.
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
let rec read_and_close_pid fd err_fd fds pid acc acc_err =
  if fds = [] then
    (** EOF has been reached for all FDs. Waitpid and return result. *)
    let () = Unix.close fd in
    let () = Unix.close err_fd in
    match Unix.waitpid [] pid with
    | _, status ->
      let result = String.concat "" (List.rev acc) in
      let err = String.concat "" (List.rev acc_err) in
      status, result, err
  else
    let ready_fds, _, _ = Unix.select fds [] [] 0.1 in
    if ready_fds = [] then begin
      (** Here's where we switch from attempting to read from pipe to
       * attempting a non-blocking waitpid. *)
      match Unix.waitpid [Unix.WNOHANG] pid with
      | i, status when i = pid ->
        let acc = maybe_consume fd acc in
        let out = String.concat "" (List.rev acc) in
        let acc_err = maybe_consume err_fd acc_err in
        let err = String.concat "" (List.rev acc_err) in
        status, out, err
      | _ ->
        (** Process has not exited. Keep going. *)
        read_and_close_pid fd err_fd fds pid acc acc_err
    end
    else

    let fds, acc, acc_err = List.fold_left ready_fds ~init:(fds, acc, acc_err)
    ~f:begin fun (fds, acc, acc_err) fd ->
      let bytes_read = Unix.read fd buffer 0 chunk_size in
      if bytes_read = 0 then
        (((List.filter fds ~f:(fun x -> x <> fd))), acc, acc_err)
      else
        let chunk = String.sub buffer 0 bytes_read in
        if fd = err_fd then
          (fds, acc, (chunk :: acc_err))
        else
          (fds, (chunk :: acc), acc_err)
    end
    in
    read_and_close_pid fd err_fd fds pid acc acc_err

 let read_and_close_pid {
   Process_types.stdin_fd = _stdin_fd;
   stdout_fd;
   stderr_fd;
   pid; } =
     read_and_close_pid stdout_fd stderr_fd [stdout_fd; stderr_fd] pid [] []

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
    Process_types.stdin_fd = stdin_parent;
    stdout_fd = stdout_parent;
    stderr_fd = stderr_parent;
    pid;
  }
