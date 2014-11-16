(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Module building workers.
 * A worker is a subprocess executing an arbitrary function.
 * You should first create a fixed amount of workers and then use those 
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml).
 *)
(*****************************************************************************)

(* The type of a worker visible to the outside world *)
type t

(*****************************************************************************)
(* The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 * Note that the scheduler has to use a handle for that. But the handle
 * is just a trick to get type-checking on workers, a handle is a
 * phantom type, it doesn't really have a value.
 *)
(*****************************************************************************)
type 'a handle

(* Creates a worker *)
val make: int -> t list

(* Call in a sub-process (CAREFULL, GLOBALS ARE COPIED) *)
val call: t -> ('a -> 'b) -> 'a -> 'b handle

(* Retrieves the result (once the worker is done) hangs otherwise *)
val get_result: t -> 'a handle -> 'a

(* Selects among multiple process those which are ready *)
val select: t list -> t list

(* Gives back the Unix process ID of a process *)
val get_pid: t -> int

(* Returns the file descriptor used by the master process to receive results
 * from the child. (cf hh_server.ml section on pipes)
 *)
val get_file_descr: t -> Unix.file_descr

(* Kills the worker with kill -9 *)
val kill: t -> unit
