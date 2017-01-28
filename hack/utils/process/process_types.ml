(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


exception Select_timed_out

type process_status =
  (** The process is still running. *)
  | Process_running of int
  (** Waitpid finished on the process, and the remainder of output
   * in the pipes have been consumed, whether or not an EOF has been
   * reached. *)
  | Process_exited of Unix.process_status

type t = {
  stdin_fd : Unix.file_descr option ref;
  stdout_fd : Unix.file_descr option ref;
  stderr_fd : Unix.file_descr option ref;
  acc : string Stack.t;
  acc_err : string Stack.t;
  process_status : process_status ref;
}
