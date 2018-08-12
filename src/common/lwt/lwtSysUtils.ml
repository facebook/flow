(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Basically a `waitpid [ WUNTRACED ] pid` (WUNTRACED means also return on stopped processes) *)
let blocking_waitpid =
  let reasonable_impl pid = Lwt_unix.waitpid [Unix.WUNTRACED] pid
  in

  (* Lwt_unix.waitpid without WNOHANG doesn't work on Windows. As a workaround, we can call the
    * WNOHANG version every .5 seconds. https://github.com/ocsigen/lwt/issues/494 *)
  let rec damn_it_windows_impl pid_to_wait_for =
    let%lwt (pid_ret, status) = Lwt_unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid_to_wait_for in
    if pid_ret = 0
    then
      (* Still hasn't exited. Let's wait .5s and try again *)
      let%lwt () = Lwt_unix.sleep 0.5 in
      damn_it_windows_impl pid_to_wait_for
    else
      (* Ok, process has exited or died or something. *)
      Lwt.return (pid_ret, status)
  in

  if Sys.win32 then damn_it_windows_impl else reasonable_impl

(* An lwt version of Sys_utils.exec_read. Basically just runs a command and returns the first line
 * of stdout *)
let exec_read cmd args =
  let process = Lwt_process.open_process_in (cmd, Array.of_list (cmd::args)) in
  let%lwt result = Lwt_io.read_line process#stdout in
  let%lwt status = process#close in
  assert (status = Unix.WEXITED 0);
  Lwt.return result
