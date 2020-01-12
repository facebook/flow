(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A Process.t represents a unix process which we execute
 * It is a mutable structure which accumulates stdout/stderr.
 * The mutation happens during calls to read_and_wait_pid.
 * As for stdin, that's sent all in one chunk at the beginning.
 *)

(* lifecycle is an internal book-keeping thing. The lifecycle is:
 * 1.  We launch the process, and try to send all the stdin in one go.
 * 1a. If all goes well, we start in the state "Process_running"
 * 1b. If stdin exceeds the OS buffer then we'll kill it immediately and
 *     start in the state "Process_killed_due_to_overflow_stdin"
 * 2.  The caller calls read_and_wait_pid. If it was in Process_running,
 *     go on to 2a/2b.
 * 2a. If eventually the process terminates before the timeout then we
 *     transition to the state "Process_exited".
 * 2b. If the timeout happens first, then we actually bypass updating
 *     current_state_of_process, and we just construct a process_result directly.
 *)
type lifecycle =
  | Lifecycle_running of { pid: int } (* the process is still running *)
  | Lifecycle_exited of Unix.process_status (* the process exited *)
  | Lifecycle_killed_due_to_overflow_stdin

(* Invocation info is internal book-keeping to record information about
 * the process's original invocation. *)
type invocation_info = {
  name: string;
  args: string list;
  stack: Utils.callstack;
}

type environment =
  | Default
  | Empty
  | Augment of string list
  | Replace of string list

(* type 't' represents a process, be it completed or still underway.
 * From the information in 't' we can figure out if it has completed,
 * and if so then we can synthesize the process_result. *)
type t = {
  info: invocation_info;
  stdin_fd: Unix.file_descr option ref;
  stdout_fd: Unix.file_descr option ref;
  stderr_fd: Unix.file_descr option ref;
  acc: string Stack.t;
  acc_err: string Stack.t;
  lifecycle: lifecycle ref;
}

(* type 'process_results' represents the end-state of waiting for a process.
   It's obtained by invoking read_and_wait_pid, which runs until either
   the lifecycle isn't Process_running, or until the timeout parameter expires.
*)
type process_result = (success, failure) result

and success = {
  stdout: string;
  stderr: string;
}

and failure =
  (* process terminated with an error code *)
  | Abnormal_exit of {
      status: Unix.process_status;
      stdout: string;
      stderr: string;
    }
  (* process didn't terminate within specified timeout *)
  | Timed_out of {
      stdout: string;
      stderr: string;
    }
  (* we initially tried to send a bigger stdin than the current implementation allows for *)
  | Overflow_stdin

let dummy =
  {
    info = { name = "dummy"; args = []; stack = Utils.Callstack "" };
    stdin_fd = ref None;
    stdout_fd = ref None;
    stderr_fd = ref None;
    acc = Stack.create ();
    acc_err = Stack.create ();
    lifecycle = ref @@ Lifecycle_exited (Unix.WEXITED 0);
  }
