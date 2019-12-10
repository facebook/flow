(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A WorkloadStream.t is a datastructure which keeps track of the workloads (aka commands) that the
 * server has queued up to run. The basic operations are pushing new workloads and popping the
 * oldest workloads.
 *
 * We keep parallelizable workloads and nonparallelizable workloads in separate queues. This allows
 * the caller to ask for the next parallelizable workload or any workload.
 *
 * Parallelizable workloads can also be requeued. Requeueing basically sticks the workload at the
 * front of the queue. We do this when parallelizable workloads are canceled due to a recheck.
 *)

type workload = ServerEnv.env -> ServerEnv.env Lwt.t

type parallelizable_workload = ServerEnv.env -> unit Lwt.t

type t = {
  mutable parallelizable: (float * parallelizable_workload) ImmQueue.t;
  mutable requeued_parallelizable: parallelizable_workload list;
  mutable nonparallelizable: (float * workload) ImmQueue.t;
  signal: unit Lwt_condition.t;
}

let create () =
  {
    parallelizable = ImmQueue.empty;
    requeued_parallelizable = [];
    nonparallelizable = ImmQueue.empty;
    signal = Lwt_condition.create ();
  }

(* Add a non-parallelizable workload to the stream and wake up anyone waiting *)
let push workload stream =
  let now = Unix.gettimeofday () in
  stream.nonparallelizable <- ImmQueue.push stream.nonparallelizable (now, workload);
  Lwt_condition.broadcast stream.signal ()

(* Add a parallelizable workload to the stream and wake up anyone waiting *)
let push_parallelizable workload stream =
  let now = Unix.gettimeofday () in
  stream.parallelizable <- ImmQueue.push stream.parallelizable (now, workload);
  Lwt_condition.broadcast stream.signal ()

(* Add a parallelizable workload to the front of the stream and wake up anyone waiting *)
let requeue_parallelizable workload stream =
  stream.requeued_parallelizable <- workload :: stream.requeued_parallelizable;
  Lwt_condition.broadcast stream.signal ()

(* Cast a parallelizable workload to a nonparallelizable workload. *)
let workload_of_parallelizable_workload parallelizable_workload env =
  let%lwt () = parallelizable_workload env in
  Lwt.return env

(* Pop the oldest workload *)
let pop stream =
  match stream.requeued_parallelizable with
  | workload :: rest ->
    (* Always prefer requeued parallelizable jobs *)
    stream.requeued_parallelizable <- rest;
    Some (workload_of_parallelizable_workload workload)
  | [] ->
    let (entry_p, parallelizable) = ImmQueue.peek stream.parallelizable in
    let (entry_n, nonparallelizable) = ImmQueue.peek stream.nonparallelizable in
    (* Pop from the parallelizable queue unless the nonparallelizable queue has an older entry *)
    let use_parallelizable =
      match (entry_p, entry_n) with
      | (None, None)
      | (Some _, None) ->
        true
      | (Some (timestamp_p, _), Some (timestamp_n, _)) -> timestamp_p <= timestamp_n
      | (None, Some _) -> false
    in
    let (workload_opt, parallelizable, nonparallelizable) =
      if use_parallelizable then
        let (_, parallelizable) = ImmQueue.pop parallelizable in
        let workload =
          Option.map entry_p ~f:(fun (_, workload) -> workload_of_parallelizable_workload workload)
        in
        (workload, parallelizable, nonparallelizable)
      else
        let (_, nonparallelizable) = ImmQueue.pop nonparallelizable in
        (Option.map entry_n ~f:snd, parallelizable, nonparallelizable)
    in
    stream.parallelizable <- parallelizable;
    stream.nonparallelizable <- nonparallelizable;
    workload_opt

(* Pop the oldest parallelizable workload *)
let pop_parallelizable stream =
  match stream.requeued_parallelizable with
  | workload :: rest ->
    (* Always prefer requeued parallelizable jobs *)
    stream.requeued_parallelizable <- rest;
    Some workload
  | [] ->
    let (entry_opt, parallelizable) = ImmQueue.pop stream.parallelizable in
    stream.parallelizable <- parallelizable;
    Option.map entry_opt ~f:snd

(* Wait until there's a workload in the stream *)
let rec wait_for_workload stream =
  if
    stream.requeued_parallelizable = []
    && ImmQueue.is_empty stream.parallelizable
    && ImmQueue.is_empty stream.nonparallelizable
  then
    let%lwt () = Lwt_condition.wait stream.signal in
    wait_for_workload stream
  else
    Lwt.return_unit

(* Wait until there's a parallelizable workload in the stream *)
let rec wait_for_parallelizable_workload stream =
  if stream.requeued_parallelizable = [] && ImmQueue.is_empty stream.parallelizable then
    let%lwt () = Lwt_condition.wait stream.signal in
    wait_for_parallelizable_workload stream
  else
    Lwt.return_unit
