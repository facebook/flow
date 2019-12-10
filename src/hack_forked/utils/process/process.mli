(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Utilities to deal with subprocesses. *)

open Process_types

module Entry : sig
  type 'param t

  val register : string -> ('param -> unit) -> 'param t
end

val exec :
  string -> ?input:string -> ?env:Process_types.environment -> string list -> Process_types.t
(**
 * Shells out the program with the given args.
 * Sends input to stdin of spawned process if given.
 *)

val exec_with_working_directory :
  dir:string ->
  string ->
  ?input:string ->
  ?env:Process_types.environment ->
  string list ->
  Process_types.t
(**
 * Shells out the program with the given args.
 * Sets the working directory to the one specified before executing.
 * NOTE: make sure to call Daemon.check_entry_point in your main entry point if passing
 * this argument! We actually spawn our own process with chdir_main as the entry point, which
 * changes the current working directory to the desired directory, executes the program,
 * and redirect the output back to the original process. Therefore,
 * if you don't check entry point, the process will use the regular main entry
 * point instead, and the results will be unpredictable and difficult to understand.
 * Sends input to stdin of spawned process if given.
 * NOTE: the default environment for the execution is the current program's environment.
 * Specify the desired environment if you want a different behavior.
 * Sends input to stdin of spawned process if given.
 *)

val register_entry_point : string -> ('param -> unit) -> 'param Entry.t

val run_entry : ?input:string -> 'a Entry.t -> 'a -> Process_types.t
(** Wraps a entry point inside a Process, so we get Process's
 * goodness for free (read_and_wait_pid and is_ready). The entry will be
 * spawned into a separate process. *)

val read_and_wait_pid : timeout:int -> Process_types.t -> process_result
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

val failure_msg : failure -> string

val status_to_string : Unix.process_status -> string

val is_ready : Process_types.t -> bool
(** Returns true if read_and_close_pid would be nonblocking. *)
