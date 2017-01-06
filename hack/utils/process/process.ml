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

(** Recursively read from fds. If EOF is reached, remove that
 * fd from fds; terminatin when fds is empty.
 *
 * fd and err_fd helps us track which accumulator to put the read
 * data from. *)
let rec read_and_close_pid fd err_fd fds pid acc acc_err =
  if fds = [] then
    let () = Unix.close fd in
    let () = Unix.close err_fd in
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
      let result = String.concat "" (List.rev acc) in
      result
    | _, status ->
      let err = String.concat "" (List.rev acc_err) in
      raise (Process_types.Process_exited_with_error (status, err))
  else
    let ready_fds, _, _ = Unix.select fds [] [] 9999999.9 in
    if ready_fds = [] then
       raise Process_types.Select_timed_out
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
   try read_and_close_pid
     stdout_fd stderr_fd [stdout_fd; stderr_fd] pid [] [] with
     | Process_types.Process_exited_with_error (_, stderr) as e ->
       let user = Option.value (Sys_utils.getenv_user ()) ~default:"" in
       let home = Option.value (Sys_utils.getenv_home ()) ~default:"" in
       let () = Printf.eprintf
         "Process failed. See also env USER=%s. HOME=%s\n" user home in
       let () = Printf.eprintf "Stderr:%s\n" stderr in
       raise e

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
