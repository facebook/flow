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

type 'a nextlist = 'a list Bucket.next

type 'a bucket = 'a Bucket.bucket =
  | Job of 'a
  | Wait
  | Done

let single_threaded_call job merge neutral next =
  let x = ref (next()) in
  let acc = ref neutral in
  (* This is a just a sanity check that the job is serializable and so
   * that the same code will work both in single threaded and parallel
   * mode.
   *)
  let _ = Marshal.to_string job [Marshal.Closures] in
  while !x <> Done do
    match !x with
    | Wait ->
        (* this state should never be reached in single threaded mode, since
           there is no hope for ever getting out of this state *)
        failwith "stuck!"
    | Job l ->
        let res = job neutral l in
        acc := merge res !acc;
        x := next()
    | Done -> ()
  done;
  !acc

let multi_threaded_call
  (type a) (type b) (type c)
  workers (job: c -> a -> b)
  (merge: b -> c -> c)
  (neutral: c)
  (next: a Bucket.next) =
  let rec dispatch workers handles acc =
    (* 'worker' represents available workers. *)
    (* 'handles' represents pendings jobs. *)
    (* 'acc' are the accumulated results. *)
    match workers with
    | [] when handles = [] -> acc
    | [] ->
        (* No worker available: wait for some workers to finish. *)
        collect [] handles acc
    | worker :: workers ->
        (* At least one worker is available... *)
        match next () with
        | Wait -> collect (worker :: workers) handles acc
        | Done ->
            (* ... but no more job to be distributed, let's collect results. *)
            dispatch [] handles acc
        | Job bucket ->
            (* ... send a job to the worker.*)
            let handle =
              Worker.call worker
                (fun xl -> job neutral xl)
                bucket in
            dispatch workers (handle :: handles) acc
  and collect workers handles acc =
    let { Worker.readys; waiters } = Worker.select handles in
    let workers = List.map ~f:Worker.get_worker readys @ workers in
    (* Collect the results. *)
    let acc =
      List.fold_left
        ~f:(fun acc h -> merge (Worker.get_result h) acc)
        ~init:acc
        readys in
    (* And continue.. *)
    dispatch workers waiters acc in
  dispatch workers [] neutral

let call workers ~job ~merge ~neutral ~next =
  match workers with
  | None -> single_threaded_call job merge neutral next
  | Some workers -> multi_threaded_call workers job merge neutral next

let next ?max_size workers =
  Bucket.make
    ~num_workers: (match workers with Some w -> List.length w | None -> 1)
    ?max_size
