(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(** Uttilities to deal with subprocesses. *)

(** exec program ?env args
 *
 * Shells out the program with the given args. *)
val exec : string -> ?env:string list -> string list -> Process_types.t

(**
 * Read data from stdout and stderr until EOF is reached. Waits for
 * process to terminate, and returns the process status, the stdout
 * and stderr.
 *
 * Idempotent.
 *)
val read_and_wait_pid :
  Process_types.t -> Unix.process_status * string * string

(** Returns true if read_and_close_pid would be nonblocking. *)
val is_ready : Process_types.t -> bool
