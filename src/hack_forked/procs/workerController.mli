(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* Module building workers.
 * A worker is a subprocess executing an arbitrary function.
 * You should first create a fixed amount of workers and then use those
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml).
 *)
(*****************************************************************************)

type process_id = int

type worker_id = int

type worker_failure =
  | Worker_oomed  (** Worker killed by Out Of Memory. *)
  | Worker_quit of Unix.process_status option

exception Worker_failed of (process_id * worker_failure)

(* Raise this exception when sending work to a worker that is already busy.
 * We should never be doing that, and this is an assertion error. *)
exception Worker_busy

val failure_to_string : worker_failure -> string

type send_job_failure =
  | Worker_already_exited of Unix.process_status option
  | Other_send_job_failure of Exception.t

exception Worker_failed_to_send_job of send_job_failure

(* The type of a worker visible to the outside world *)
type worker

type call_wrapper = { wrap: 'x 'b. ('x -> 'b) -> 'x -> 'b }

type 'a entry

val register_entry_point : restore:('a -> worker_id:int -> unit) -> 'a entry

(* Creates a pool of workers. *)
val make :
  call_wrapper:(* See docs in WorkerController.worker for call_wrapper. *)
    call_wrapper option ->
  saved_state:'a ->
  entry:'a entry ->
  nbr_procs:int ->
  gc_control:Caml.Gc.control ->
  heap_handle:SharedMem.handle ->
  worker list

(* Call in a sub-process (CAREFUL, GLOBALS ARE COPIED) *)
val call : worker -> ('a -> 'b) -> 'a -> 'b Lwt.t

(* Killall the workers *)
val killall : unit -> unit
