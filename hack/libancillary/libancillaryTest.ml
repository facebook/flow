(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let int_to_fd (x: int) : Unix.file_descr = Obj.magic x
let fd_to_int (x: Unix.file_descr) : int = Obj.magic x
let payload_message = "Hello world"

(** Tail call; exits *)
let send_fd_and_wait child_1_pid socket_fd fd_to_be_sent =
  let result = Libancillary.ancil_send_fd
    socket_fd fd_to_be_sent in
  if (result = -1) then
    (Printf.eprintf "Parent: Failed to send fd. Exiting\n";
    let _ = Unix.wait () in
    exit 1)
  else
    (let _, status = Unix.waitpid [] child_1_pid in
    match status with
    | Unix.WEXITED 0 ->
        print_endline "Success!";
      exit 0
    | Unix.WEXITED i -> Printf.eprintf "Error: Child 1 exited with code %i" i;
      exit 1
    | _ -> Printf.eprintf "Error. Unexpected status";
      exit 1)

(** Tail call; exits *)
let child_1_process socket_fd =
  Unix.sleep 2;
  (** Receive the fd from parent process which will be used to get messages
   * from child 2. *)
  let upward_fd_2 =
    try Libancillary.ancil_recv_fd socket_fd
    with
    | Libancillary.Receiving_Fd_Exception ->
      (Printf.eprintf "Child 1: Failed to received fd. Exiting.\n";
      exit 1)
    in
  let ic = Unix.in_channel_of_descr upward_fd_2 in
  let msg: string = Marshal.from_channel ic in
  if (msg = payload_message) then
    exit 0
  else
    (Printf.eprintf "Child 1: Got message: %s\n" msg;
    exit 1)

(** Tail call; exits *)
let child_2_process socket_fd =
  let oc = Unix.out_channel_of_descr socket_fd in
  Marshal.to_channel oc payload_message [];
  exit 0

(** After forking once, continue as the parent to fork the second child.
 * downward_1_fd is the socket to send data to child 1. *)
let continue_parent child_1_pid downward_fd_1 =
  let downward_fd_2, upward_fd_2 =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let child_2_pid = Unix.fork() in
  if (child_2_pid = -1) then
    (Printf.eprintf "Parent: Error forking child 2. Exiting with code 1\n";
    exit 1)
  else if (child_2_pid = 0) then
    (** Child process doesn't use this. *)
    (Unix.close downward_fd_2;
    child_2_process upward_fd_2)
  else
    (Unix.close upward_fd_2;
    send_fd_and_wait child_1_pid downward_fd_1 downward_fd_2)

let () =
  (** This socket lets parent send data to child 1. We will be using it to
   * send a file descriptor to child 1. *)
  let upward_fd_1, downward_fd_1 = Unix.socketpair
    Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (*let upward_fd_1, downward_fd_1 = Unix.pipe () in*)
  let child_1_pid = Unix.fork() in
  (if (child_1_pid = -1) then
    (Printf.eprintf "Owner: Error forking child 1. Exiting with code 1\n";
    exit 1)
  else if (child_1_pid = 0) then
    ((** Child process doesn't use this. *)
    Unix.close downward_fd_1;
    child_1_process upward_fd_1)
  else
    ((** Parent process doesn't use this. *)
    Unix.close upward_fd_1;
    continue_parent child_1_pid downward_fd_1)
  )
