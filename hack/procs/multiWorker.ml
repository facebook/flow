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

type interrupt_handler = Unix.file_descr list -> bool

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
  (next: a Bucket.next)
  (interrupt_fds: Unix.file_descr list)
  (interrupt_handler: interrupt_handler) =

  let check_cancel workers handles ready_fds =
    if ready_fds <> [] && interrupt_handler ready_fds then begin
      Worker.cancel handles;
      true
    end else false in

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
    let { Worker.readys; waiters; ready_fds } =
      Worker.select handles interrupt_fds in
    let workers = List.map ~f:Worker.get_worker readys @ workers in
    (* Collect the results. *)
    let acc =
      List.fold_left
        ~f:(fun acc h -> merge (Worker.get_result h) acc)
        ~init:acc
        readys in
    if check_cancel workers waiters ready_fds then acc else
    (* And continue.. *)
    dispatch workers waiters acc in
  dispatch workers [] neutral

let call workers ~job ~merge ~neutral ~next ~interrupt_fds ~interrupt_handler =
  match workers with
  | None -> single_threaded_call job merge neutral next
  | Some workers ->
    multi_threaded_call workers job merge neutral next
      interrupt_fds interrupt_handler

let call_with_interrupt workers ~job ~merge ~neutral ~next ~interrupt_fds
    ~interrupt_handler =
  call workers ~job ~merge ~neutral ~next ~interrupt_fds ~interrupt_handler

let call workers ~job ~merge ~neutral ~next =
  let interrupt_fds = [] in
  let interrupt_handler = fun _ -> assert false in
  call workers ~job ~merge ~neutral ~next ~interrupt_fds ~interrupt_handler

let next ?progress_fn ?max_size workers =
  Bucket.make
    ~num_workers: (match workers with Some w -> List.length w | None -> 1)
    ?progress_fn
    ?max_size
