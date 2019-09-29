(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_bucket = Bucket

let report_canceled_callback = ref (fun ~total:_ ~finished:_ -> ())

let set_report_canceled_callback callback = report_canceled_callback := callback

let report_canceled ~total ~finished = !report_canceled_callback ~total ~finished

include MultiWorker.CallFunctor (struct
  type 'a result = 'a Lwt.t

  let return = Lwt.return

  let multi_threaded_call
      (type a b c)
      workers
      (job : WorkerController.worker_id * c -> a -> b)
      (merge : WorkerController.worker_id * b -> c -> c)
      (neutral : c)
      (next : a Hh_bucket.next) =
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
            Lwt.return_unit)
    in
    (* Our next() function may give us a job, say there are no more jobs left, or tell us to
     * try again later. This signal is to wake up any workers who were told "try again later"
     *)
    let wait_signal = Lwt_condition.create () in
    (* Returns None if there will never be any more jobs *)
    let rec get_job () =
      match next () with
      | Hh_bucket.Job bucket -> Lwt.return (Some bucket)
      | Hh_bucket.Done -> Lwt.return None
      | Hh_bucket.Wait ->
        let%lwt () = Lwt_condition.wait wait_signal in
        get_job ()
    in
    let rec run_worker worker =
      let idle_start_wall_time = Unix.gettimeofday () in
      let%lwt bucket = get_job () in
      match bucket with
      | None -> Lwt.return idle_start_wall_time
      | Some bucket ->
        Measure.sample "worker_idle" (Unix.gettimeofday () -. idle_start_wall_time);
        let worker_id = WorkerController.worker_id worker in
        let%lwt result =
          WorkerControllerLwt.call worker (fun xl -> job (worker_id, neutral) xl) bucket
        in
        let%lwt () = merge_with_acc (WorkerController.worker_id worker, result) in
        (* Wait means "ask again after a worker has finished and has merged its result". So now that
         * we've merged our response, let's wake any other workers which are waiting for work *)
        Lwt_condition.broadcast wait_signal ();
        run_worker worker
    in
    let%lwt () =
      let worker_threads = List.map run_worker workers in
      try%lwt
        let%lwt idle_start_times = LwtUtils.all worker_threads in
        let idle_end_wall_time = Unix.gettimeofday () in
        List.iter
          (fun idle_start_wall_time ->
            Measure.sample "worker_done" (idle_end_wall_time -. idle_start_wall_time))
          idle_start_times;
        Lwt.return_unit
      with Lwt.Canceled ->
        let total = List.length worker_threads in
        let finished = ref 0 in
        let worker_threads =
          List.map
            (fun thread ->
              (let%lwt _ = thread in
               Lwt.return_unit)
                [%lwt.finally
                  incr finished;
                  report_canceled ~total ~finished:!finished;
                  Lwt.return_unit])
            worker_threads
        in
        (* For most exceptions, we want to propagate the exception as soon as one worker throws.
         * However, for Canceled we want to wait for all the workers to process the Canceled.
         * Lwt.join will wait for every thread to finish or fail *)
        (Lwt.join worker_threads)
          [%lwt.finally
            WorkerCancel.resume_workers ();
            Lwt.return_unit]
    in
    Lwt.return !acc
end)

exception MultiWorkersBusy

(* Currently, MultiWorker calls may not be interleaved, which can happen with
 * Lwt. Keep track of whether we have a call in flight and raise an exception if
 * we do when another comes in. *)
let is_busy = ref false

let call_with_worker_id workers ~job ~merge ~neutral ~next =
  if !is_busy then
    raise MultiWorkersBusy
  else (
    is_busy := true;
    (call workers ~job ~merge ~neutral ~next)
      [%lwt.finally
        is_busy := false;
        Lwt.return_unit]
  )

let call workers ~job ~merge ~neutral ~next =
  let job (_worker_id, a) b = job a b in
  let merge (_worker_id, a) b = merge a b in
  call_with_worker_id workers ~job ~merge ~neutral ~next

(* A separate abstract type from MultiWorker.worker forces users to always use MultiWorkerLwt *)
type worker = WorkerController.worker

let next ?progress_fn ?max_size workers =
  Hh_bucket.make
    ~num_workers:
      (match workers with
      | Some w -> List.length w
      | None -> 1)
    ?progress_fn
    ?max_size

(* Wrap WorkerController.make to abstract out the worker type *)
let make = WorkerController.make
