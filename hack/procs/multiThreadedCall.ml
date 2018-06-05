(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core

exception Coalesced_failures of (WorkerController.worker_failure list)

let coalesced_failures_to_string failures =
  let failure_strings =
    List.map failures ~f:WorkerController.failure_to_string in
  Printf.sprintf "Coalesced_failures[%s]" (String.concat ", " failure_strings)

let () = Printexc.register_printer @@ function
  | Coalesced_failures failures -> Some (coalesced_failures_to_string failures)
  | _ -> None

type interrupt_result = Cancel | Continue

type 'env interrupt_handler = 'env -> 'env * interrupt_result

type 'env interrupt_config = {
  env : 'env;
  handlers : 'env -> (Unix.file_descr * 'env interrupt_handler) list;
}

let no_interrupt env = {
  handlers = (fun _ -> []);
  env;
}

(* Integer that increases with every invocation of multi_threaded_call, used to
 * distinguish worker handles that belong to current job vs those that are still
 * processing some other job (in cases when multi_threaded_call is called during
 * an already ongoing multi_threaded_call job. *)
let call_id = ref 0

(* Exceptions from parallel jobs are in general not recoverable - the workers
 * are dead and we don't respawn them. The only reason someone should catch
 * them is to log and exit. There are unfortunately callers that ignore them and
 * keep going. If such caller happens to be an inner job, we want to re-raise
 * the exception for outer job too instead of making it fail due to random
 * undefined state issue *)
let nested_exception: exn option ref = ref None

let multi_threaded_call
  (type a) (type b) (type c) (type d)
  workers
  (job: c -> a -> b)
  (merge: b -> c -> c)
  (neutral: c)
  (next: a Bucket.next)
  (interrupt: d interrupt_config) =

  incr call_id;
  let call_id = !call_id in

  (* Split workers into those that are free, and those that are still doing
   * previous jobs. *)
  let workers, handles = List.fold workers
    ~init:([], [])
    ~f:begin fun (workers, handles) worker ->
      (* Note than now some handles have mismatched types. We need to remember
       * to check their get_call_id against this multi_threaded_call call_id
       * before trusting the types. *)
      match WorkerController.get_handle_UNSAFE worker with
      | None -> worker::workers, handles
      | Some handle -> workers, handle::handles
    end
  in

  let is_current h = (call_id = (WorkerController.get_call_id h)) in

  (* merge accumulator, leaving environment and interrupt handlers untouched *)
  let merge x (y1, y2, y3) = merge x y1, y2, y3 in

  (* interrupt handlers are irrelevant after job is done *)
  let unpack_result (acc, env, _handlers) = acc, env in

  let handler_fds (_, _, handlers) = List.map handlers ~f:fst in

  let rec add_pending acc = match next () with
    | Bucket.Done -> acc
    | Bucket.Job a -> add_pending (a::acc)
    | Bucket.Wait ->
      (* Wait is only used by Flow at the moment, and they don't plan to use
      * this cancelling mechanism for now. *)
      failwith "cancelling jobs with Wait not supported" in

  (* When a job is cancelled, return all the jobs that were not started OR were
   * cancelled in the middle (so you better hope they are idempotent).*)
  let check_cancel handles ready_fds (acc, env, handlers) =
    let env, decision, handlers = List.fold handlers
      ~init:(env, Continue, handlers)
      ~f:begin fun (env, decision, handlers) (fd, handler) ->
        if decision = Cancel || not @@ List.mem ready_fds fd
          then env, decision, handlers else
        let env, decision = handler env in
        (* Re-raise the exception even if handler have caught and ignored it *)
        Option.iter !nested_exception ~f:(fun x -> raise x);
        (* running a handler could have changed the handlers,
         * so need to regenerate them based on new environment *)
        let handlers = interrupt.handlers env in
        env, decision, handlers
      end
    in
    let res = acc, env, handlers in
    if decision = Cancel then begin
      WorkerController.cancel handles;
      let unfinished = List.map handles ~f:WorkerController.get_job in
      let unfinished = add_pending unfinished in
      res, Some unfinished
    end else res, None in

  let rec dispatch workers handles acc =
    (* 'worker' represents available workers. *)
    (* 'handles' represents pendings jobs. *)
    (* 'acc' are the accumulated results. *)
    match workers with
    | None when (not @@ List.exists handles ~f:is_current) ->
      (* No more handles at this recursion level *)
        unpack_result acc, []
    | None (* No more jobs to start *)
    | Some [] ->
        (* No worker available: wait for some workers to finish. *)
        collect [] handles acc
    | Some (worker :: workers) ->
        (* At least one worker is available... *)
        match next () with
        | Bucket.Wait -> collect (worker :: workers) handles acc
        | Bucket.Done ->
            (* ... but no more job to be distributed, let's collect results. *)
            dispatch None handles acc
        | Bucket.Job bucket ->
            (* ... send a job to the worker.*)
            let handle =
              WorkerController.call ~call_id worker
                (fun xl -> job neutral xl)
                bucket in
            dispatch (Some workers) (handle :: handles) acc
  and collect workers handles acc =
    let { WorkerController.readys; waiters; ready_fds } =
      WorkerController.select handles (handler_fds acc) in
    let workers = List.map ~f:WorkerController.get_worker readys @ workers in
    (* Collect the results. *)
    let acc, failures =
      (** Fold the results of all the finished workers. Also, coalesce the exit
       * statuses for all the failed workers. *)
      List.fold_left
        ~f:begin fun (acc, failures) h ->
           try
             let res = WorkerController.get_result h in
             (* Results for handles from other calls are cached by get_result
              * and will be retrieved later, so we ignore them here *)
             let acc = if is_current h then merge res acc else acc in
             acc, failures
           with
           | WorkerController.Worker_failed (_, failure) ->
             acc, (failure :: failures)
        end
        ~init:(acc, [])
        readys in
    if (failures <> []) then
      (** If any single worker failed, we stop fanning out more jobs. *)
      raise (Coalesced_failures failures)
    else
    match check_cancel waiters ready_fds acc with
    | acc, Some unfinished -> unpack_result acc, unfinished
    | acc, None ->
      (* And continue.. *)
      dispatch (Some workers) waiters acc in
  try
    let () = nested_exception := None in
    dispatch (Some workers) handles (neutral, interrupt.env, interrupt.handlers interrupt.env)
  with e ->
    nested_exception := Some e;
    raise e

let call workers job merge neutral next =
  let (res, ()), unfinished =
    multi_threaded_call workers job merge neutral next (no_interrupt ()) in
  assert (unfinished = []);
  res

let call_with_interrupt workers job merge neutral next interrupt =
  SharedMem.allow_removes false;
  (* Interrupting of nested jobs is not implemented *)
  assert (List.for_all workers
    ~f:(fun x -> Option.is_none @@ WorkerController.get_handle_UNSAFE x)
  );
  let (res, interrupt_env), unfinished =
    multi_threaded_call workers job merge neutral next interrupt in
  SharedMem.allow_removes true;
  res, interrupt_env, unfinished
