(**
 * Copyright (c) 2015, Facebook, Inc.
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
 *)
(*****************************************************************************)
type 'a handle

(* This function creates an alternate entry point. Its usage should
   follow the same rules than `Daemon.register_entry_point`. It
   returns an abstract 'builder' that should be used when creating
   worker. The 'save' function will be called on master when creating
   the worker, it results will be sent to the worker. The worker will
   pass this value to `restore` in order to (re)initialize itself.
   See [ServerWorker]. *)
type builder
val register_entry_point:
  save:(unit -> 'a) -> restore:('a -> unit) -> builder

(* Creates a pool of workers. *)
val make: builder -> int -> Gc.control -> SharedMem.handle -> t list

(* Call in a sub-process (CAREFUL, GLOBALS ARE COPIED) *)
val call: t -> ('a -> 'b) -> 'a -> 'b handle

(* Retrieves the result (once the worker is done) hangs otherwise *)
val get_result: 'a handle -> 'a

(* Selects among multiple handles those which are ready. *)
type 'a selected = {
  readys: 'a handle list;
  waiters: 'a handle list;
}
val select: 'a handle list -> 'a selected

(* Returns the worker which produces this handle *)
val get_worker: 'a handle -> t

(* Killall the workers *)
val killall: unit -> unit
