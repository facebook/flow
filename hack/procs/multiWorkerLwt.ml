(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include MultiWorker.CallFunctor (struct
  type 'a result = 'a Lwt.t

  let return = Lwt.return

  let multi_threaded_call
    (type a) (type b) (type c)
    workers
    (job: c -> a -> b)
    (merge: b -> c -> c)
    (neutral: c)
    (next: a Bucket.next) =

    let acc = ref neutral in

    let merge_with_acc =
      (* Why do we need a lock? Well, we don't really know what is inside the merge function, and if
       * something makes Lwt yield then we could end up with a race condition. At the moment, the
       * merge function doesn't use Lwt, but it might in the future. Locking and unlocking is cheap,
       * so I'm pre-emptively adding this lock *)
      let merge_mutex = Lwt_mutex.create () in
      fun result ->
        Lwt_mutex.with_lock merge_mutex (fun () ->
          acc := merge result !acc;
          Lwt.return_unit
        )
    in

    (* Our next() function may give us a job, say there are no more jobs left, or tell us to
     * try again later. This signal is to wake up any workers who were told "try again later"
     *)
    let wait_signal = Lwt_condition.create () in

    (* Returns None if there will never be any more jobs *)
    let rec get_job () =
      match next () with
      | Bucket.Job bucket -> Lwt.return (Some bucket)
      | Bucket.Done -> Lwt.return None
      | Bucket.Wait ->
        let%lwt () = Lwt_condition.wait wait_signal in
        get_job ()
    in

    let rec run_worker worker =
      let%lwt bucket = get_job () in
      match bucket with
      | None -> Lwt.return_unit
      | Some bucket ->
        let%lwt result = WorkerControllerLwt.call worker (fun xl -> job neutral xl) bucket in
        let%lwt () = merge_with_acc result in
        (* Wait means "ask again after a worker has finished and has merged its result". So now that
         * we've merged our response, let's wake any other workers which are waiting for work *)
        Lwt_condition.broadcast wait_signal ();
        run_worker worker
    in

    let%lwt () = Lwt.join (List.map run_worker workers) in

    Lwt.return (!acc)
end)

(* A separate abstract type from MultiWorker.worker forces users to always use MultiWorkerLwt *)
type worker = WorkerController.worker

let next ?progress_fn ?max_size workers =
  Bucket.make
    ~num_workers: (match workers with Some w -> List.length w | None -> 1)
    ?progress_fn
    ?max_size

(* Wrap WorkerController.make to abstract out the worker type *)
let make = WorkerController.make
