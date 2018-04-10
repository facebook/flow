(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_core

(* Hide the worker type from our users *)
type worker = WorkerController.worker

type 'a interrupt_config = 'a MultiThreadedCall.interrupt_config

let single_threaded_call job merge neutral next =
  let x = ref (next()) in
  let acc = ref neutral in
  (* This is a just a sanity check that the job is serializable and so
   * that the same code will work both in single threaded and parallel
   * mode.
   *)
  let _ = Marshal.to_string job [Marshal.Closures] in
  while !x <> Bucket.Done do
    match !x with
    | Bucket.Wait ->
        (* this state should never be reached in single threaded mode, since
           there is no hope for ever getting out of this state *)
        failwith "stuck!"
    | Bucket.Job l ->
      let res = job neutral l in
      acc := merge res !acc;
      x := next()
    | Bucket.Done -> ()
  done;
  !acc

module type CALLER = sig
  type 'a result

  val return: 'a -> 'a result

  val multi_threaded_call:
    WorkerController.worker list ->
    ('c -> 'a -> 'b) ->
    ('b -> 'c -> 'c) ->
    'c ->
    'a Bucket.next ->
    'c result
end

module CallFunctor(Caller: CALLER): sig
  val call:
    WorkerController.worker list option ->
    job:('c -> 'a -> 'b) ->
    merge:('b -> 'c -> 'c) -> neutral:'c ->
    next:'a Bucket.next ->
    'c Caller.result
end = struct
  let call workers ~job ~merge ~neutral ~next =
    match workers with
      | None ->
        Caller.return (single_threaded_call job merge neutral next)
      | Some workers -> Caller.multi_threaded_call workers job merge neutral next
end

module Call = CallFunctor(struct
  type 'a result = 'a
  let return x = x
  let multi_threaded_call = MultiThreadedCall.call
end)

let call = Call.call

(* If we ever want this in MultiWorkerLwt then move this into CallFunctor *)
let call_with_interrupt workers ~job ~merge ~neutral ~next ~interrupt =
  match workers with
  | None -> single_threaded_call job merge neutral next, interrupt.MultiThreadedCall.env, []
  | Some workers ->
    MultiThreadedCall.call_with_interrupt workers job merge neutral next
      interrupt

let next ?progress_fn ?max_size workers =
  Bucket.make
    ~num_workers: (match workers with Some w -> List.length w | None -> 1)
    ?progress_fn
    ?max_size

let make = WorkerController.make
