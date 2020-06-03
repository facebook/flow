(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Worker

(*****************************************************************************
 * Module building workers
 *
 * A worker is a subprocess executing an arbitrary function
 *
 * You should first create a fixed amount of workers and then use those
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml)
 *
 * On Unix, we "spawn" workers when initializing Hack. Then, this
 * worker, "fork" a clone process for each incoming request.
 * The forked "clone" will die after processing a single request.
 *
 * On Windows, we do not "prespawn" when initializing Hack, but we just
 * allocate all the required information into a record. Then, we
 * spawn a worker for each incoming request. It will also die after
 * one request.
 *
 * A worker never handle more than one request at a time.
 *
 *****************************************************************************)

type process_id = int

type worker_id = int

type worker_failure =
  (* Worker killed by Out Of Memory. *)
  | Worker_oomed
  | Worker_quit of Unix.process_status

exception Worker_failed of (process_id * worker_failure)

exception Worker_busy

type send_job_failure =
  | Worker_already_exited of Unix.process_status
  | Other_send_job_failure of exn

exception Worker_failed_to_send_job of send_job_failure

let status_string = function
  | Unix.WEXITED i -> Printf.sprintf "WEXITED %d" i
  | Unix.WSIGNALED i -> Printf.sprintf "WSIGNALED %d" i
  | Unix.WSTOPPED i -> Printf.sprintf "WSTOPPED %d" i

let failure_to_string f =
  match f with
  | Worker_oomed -> "Worker_oomed"
  | Worker_quit s -> Printf.sprintf "(Worker_quit %s)" (status_string s)

let () =
  Caml.Printexc.register_printer @@ function
  | Worker_failed_to_send_job (Other_send_job_failure exn) ->
    Some (Printf.sprintf "Other_send_job_failure: %s" (Exn.to_string exn))
  | Worker_failed_to_send_job (Worker_already_exited status) ->
    Some (Printf.sprintf "Worker_already_exited: %s" (status_string status))
  | Worker_failed (id, failure) ->
    Some (Printf.sprintf "Worker_failed (process_id = %d): %s" id (failure_to_string failure))
  | _ -> None

(* Should we 'prespawn' the worker ? *)
let use_prespawned = not Sys.win32

(* The maximum amount of workers *)
let max_workers = 1000

type void (* an empty type *)

type call_wrapper = { wrap: 'x 'b. ('x -> 'b) -> 'x -> 'b }

(*****************************************************************************
 * Everything we need to know about a worker.
 *
 *****************************************************************************)

type worker = {
  (* Simple id for the worker. This is not the worker pid: on Windows, we spawn
   * a new worker for each job.
   *
   * This is also an offset into the shared heap segment, used to access
   * worker-local data. As such, the numbering is important. The IDs must be
   * dense and start at 1. (0 is the controller process offset.) *)
  id: int;
  (* The call wrapper will wrap any workload sent to the worker (via "call"
   * below) before invoking the workload.
   *
   * That is, when calling the worker with workload `f x`, it will be wrapped
   * as `wrap (f x)`.
   *
   * This allows universal handling of workload at the time we create the actual
   * workers. For example, this can be useful to handle exceptions uniformly
   * across workers regardless what workload is called on them. *)
  call_wrapper: call_wrapper option;
  (* On Unix, Worker sends status messages over this fd to this Controller. On
   * Windows, it doesn't send anything, so don't try to read from it (it should
   * be set to None). *)
  controller_fd: Unix.file_descr option;
  (* Sanity check: is the worker still available ? *)
  mutable killed: bool;
  (* Sanity check: is the worker currently busy ? *)
  mutable busy: bool;
  (* If the worker is currently busy, handle of the job it's execuing *)
  mutable handle: 'a 'b. ('a, 'b) handle option;
  (* On Unix, a reference to the 'prespawned' worker. *)
  prespawned: (void, request) Daemon.handle option;
  (* On Windows, a function to spawn a worker. *)
  spawn: unit -> (void, request) Daemon.handle;
}

(*****************************************************************************
 * The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 *
 *****************************************************************************)
and ('a, 'b) handle = ('a, 'b) delayed ref

(* Integer represents job the handle belongs to.
 * See MultiThreadedCall.call_id. *)
and ('a, 'b) delayed = ('a * int) * 'b worker_handle

and 'b worker_handle =
  | Processing of 'b job
  | Cached of 'b * worker
  | Canceled
  | Failed of exn

(* The Controller's job has a Worker. The Worker is itself a single process
 * on Windows. On Unix, the Worker is itself forks Clones *)
and 'a job = {
  worker: worker;
  (* The associated worker *)

  (* The file descriptor we might pass to select in order to
     wait for the worker to finish its job. *)
  infd: Unix.file_descr;
  (* A blocking function that returns the job result. *)
  result: unit -> 'a;
  (* A blocking function that waits for job cancellation (see Worker.cancel)
   * to finish *)
  wait_for_cancel: unit -> unit;
}

let worker_id w = w.id

(* Has the worker been killed *)
let is_killed w = w.killed

(* Mark the worker as busy. Throw if it is already busy *)
let mark_busy w =
  if w.busy then raise Worker_busy;
  w.busy <- true

let get_handle_UNSAFE w = w.handle

(* Mark the worker as free *)
let mark_free w =
  w.busy <- false;
  w.handle <- None

(* If the worker isn't prespawned, spawn the worker *)
let spawn w =
  match w.prespawned with
  | None -> w.spawn ()
  | Some handle -> handle

(* If the worker isn't prespawned, close the worker *)
let close w h = if Option.is_none w.prespawned then Daemon.close h

(* If there is a call_wrapper, apply it and create the Request *)
let wrap_request w f x =
  match w.call_wrapper with
  | Some { wrap } -> Request (fun { send } -> send (wrap f x))
  | None -> Request (fun { send } -> send (f x))

type 'a entry_state = 'a * Caml.Gc.control * SharedMem.handle * int

type 'a entry = ('a entry_state * Unix.file_descr option, request, void) Daemon.entry

let entry_counter = ref 0

let register_entry_point ~restore =
  Int.incr entry_counter;
  let restore (st, gc_control, heap_handle, worker_id) =
    restore st ~worker_id;
    SharedMem.connect heap_handle ~worker_id;
    Caml.Gc.set gc_control
  in
  let name = Printf.sprintf "worker_%d" !entry_counter in
  Daemon.register_entry_point
    name
    ( if Sys.win32 then
      win32_worker_main restore
    else
      unix_worker_main restore )

(**************************************************************************
 * Creates a pool of workers.
 *
 **************************************************************************)

let workers = ref []

(* Build one worker. *)
let make_one ?call_wrapper controller_fd spawn id =
  if id >= max_workers then failwith "Too many workers";

  let prespawned =
    if not use_prespawned then
      None
    else
      Some (spawn ())
  in
  let worker =
    {
      call_wrapper;
      controller_fd;
      id;
      busy = false;
      handle = None;
      killed = false;
      prespawned;
      spawn;
    }
  in
  workers := worker :: !workers;
  worker

(* Make a few workers. When workload is given to a worker (via "call" below),
 * the workload is wrapped in the calL_wrapper. *)
let make ?call_wrapper ~saved_state ~entry ~nbr_procs ~gc_control ~heap_handle =
  let setup_controller_fd () =
    if use_prespawned then
      let (parent_fd, child_fd) = Unix.pipe () in
      (* parent_fd is only used in this process. Don't leak it to children.
       * This will auto-close parent_fd in children created with Daemon.spawn
       * since Daemon.spawn uses exec. *)
      let () = Unix.set_close_on_exec parent_fd in
      (Some parent_fd, Some child_fd)
    else
      (* We don't use the side channel on Windows. *)
      (None, None)
  in
  let spawn worker_id name child_fd () =
    Unix.clear_close_on_exec heap_handle;

    (* Daemon.spawn runs exec after forking. We explicitly *do* want to "leak"
     * child_fd to this one spawned process because it will be using that FD to
     * send messages back up to us. Close_on_exec is probably already false, but
     * we force it again to be false here just in case. *)
    Base.Option.iter child_fd ~f:Unix.clear_close_on_exec;
    let state = (saved_state, gc_control, heap_handle, worker_id) in
    let handle =
      Daemon.spawn ~name (Daemon.null_fd (), Unix.stdout, Unix.stderr) entry (state, child_fd)
    in
    Unix.set_close_on_exec heap_handle;

    (* This process no longer needs child_fd after its spawned the child.
     * Messages are read using controller_fd. *)
    Base.Option.iter child_fd ~f:Unix.close;
    handle
  in
  let made_workers = ref [] in
  let pid = Unix.getpid () in
  for n = 1 to nbr_procs do
    let (controller_fd, child_fd) = setup_controller_fd () in
    let name = Printf.sprintf "worker process %d/%d for server %d" n nbr_procs pid in
    made_workers := make_one ?call_wrapper controller_fd (spawn n name child_fd) n :: !made_workers
  done;
  !made_workers

(**************************************************************************
 * Send a job to a worker
 *
 **************************************************************************)

let call ?(call_id = 0) w (type a b) (f : a -> b) (x : a) : (a, b) handle =
  if is_killed w then Printf.ksprintf failwith "killed worker (%d)" (worker_id w);
  mark_busy w;

  (* Spawn the worker process, if not prespawned. *)
  let ({ Daemon.pid = worker_pid; channels = (inc, outc) } as h) = spawn w in
  let infd = Daemon.descr_of_in_channel inc in
  let outfd = Daemon.descr_of_out_channel outc in
  let worker_failed pid_stat controller_fd =
    (* If we have a controller fd, we read the true pid status
     * over that channel instead of using the one returned from the
     * Worker. *)
    let pid_stat =
      match controller_fd with
      | None -> snd pid_stat
      | Some fd ->
        Timeout.with_timeout
          ~timeout:3
          ~on_timeout:(fun _ -> snd pid_stat)
          ~do_:(fun _ ->
            try
              let (Job_terminated status) = Marshal_tools.from_fd_with_preamble fd in
              status
            with End_of_file -> snd pid_stat)
    in
    match pid_stat with
    | Unix.WEXITED i when i = Exit_status.(exit_code Out_of_shared_memory) ->
      raise SharedMem.Out_of_shared_memory
    | Unix.WEXITED i ->
      Caml.Printf.eprintf "Subprocess(%d): fail %d" worker_pid i;
      raise (Worker_failed (worker_pid, Worker_quit (Unix.WEXITED i)))
    | Unix.WSTOPPED i -> raise (Worker_failed (worker_pid, Worker_quit (Unix.WSTOPPED i)))
    | Unix.WSIGNALED i -> raise (Worker_failed (worker_pid, Worker_quit (Unix.WSIGNALED i)))
  in
  (* Checks if the worker process has exited. *)
  let with_exit_status_check ?(block_on_waitpid = false) worker_pid f =
    let wait_flags =
      if block_on_waitpid then
        []
      else
        [Unix.WNOHANG]
    in
    let pid_stat = Unix.waitpid wait_flags worker_pid in
    match pid_stat with
    | (0, _) -> f ()
    | (_, Unix.WEXITED 0) ->
      (* This will never actually happen. Worker process only exits if this
       * Controller process has exited. *)
      failwith "Worker process exited 0 unexpectedly"
    | _ -> worker_failed pid_stat w.controller_fd
  in
  (* Prepare ourself to read answer from the worker process. *)
  let get_result_with_status_check ?(block_on_waitpid = false) () : b =
    with_exit_status_check ~block_on_waitpid worker_pid (fun () ->
        let data : b = Marshal_tools.from_fd_with_preamble infd in
        let stats : Measure.record_data = Marshal_tools.from_fd_with_preamble infd in
        close w h;
        Measure.merge (Measure.deserialize stats);
        data)
  in
  let result () : b =
    (*
     * We run the "with_exit_status_check" twice (first time non-blockingly).
     * This is because of a race condition.
     *
     * Immediately after the worker forks the clone (see worker.ml), it does
     * a blocking, non-interruptible waitpid on the clone. This means that if the clone
     * fails, then the worker will see the failure and also fail accordingly, which we
     * will catch in here with "with_exit_status_check". This is designed around a sort-of
     * invariant - if the clone fails, then the worker will fail, so the WorkerController
     * here will see the failure and not attempt to read the result with
     *"Marshal_tools.from_fd_with_preamble"
     *
     * But there is a brief moment after the clone is forked and before the
     * non-interruptible waitpid where the clone can actually fail quickly
     * and this WorkerController has already checked the worker process's status. So the
     * invariant above will be broken, the WorkerController will try to read the result
     * with Marshal_tools, get an End_of_file, and crash.
     *
     * To get around this, we give the worker process time to "catch up" and reach the
     * non-interruptible waitpid that we expect it to be at. Eventually, it will also
     * fail accordingly, since its clone has failed.
     *)
    try get_result_with_status_check ()
    with End_of_file -> get_result_with_status_check ~block_on_waitpid:true ()
  in
  let wait_for_cancel () : unit =
    with_exit_status_check worker_pid (fun () ->
        (* Depending on whether we manage to kill the clone before it starts writing
         * results back, this will return either actual results, or "anything"
         * (written by interrupt signal that exited). The types don't match, but we
         * ignore both of them anyway. *)
        let (_ : 'c) = Marshal_tools.from_fd_with_preamble infd in
        let (_ : 'c) = Marshal_tools.from_fd_with_preamble infd in
        ())
  in
  let job = { result; infd; worker = w; wait_for_cancel } in
  let request = wrap_request w f x in
  (* Send the job to the worker. *)
  let () =
    try Marshal_tools.to_fd_with_preamble ~flags:[Caml.Marshal.Closures] outfd request |> ignore
    with e ->
      begin
        match Unix.waitpid [Unix.WNOHANG] worker_pid with
        | (0, _) -> raise (Worker_failed_to_send_job (Other_send_job_failure e))
        | (_, status) -> raise (Worker_failed_to_send_job (Worker_already_exited status))
      end
  in
  (* And returned the 'handle'. *)
  let handle = ref ((x, call_id), Processing job) in
  w.handle <- Caml.Obj.magic (Some handle);
  handle

(**************************************************************************
 * Read results from a handle.
 * This might block if the worker hasn't finished yet.
 *
 **************************************************************************)

let with_worker_exn (handle : ('a, 'b) handle) job f =
  try f () with
  | Worker_failed (pid, status) as exn ->
    mark_free job.worker;
    handle := (fst !handle, Failed exn);
    begin
      match status with
      | Worker_quit (Unix.WSIGNALED -7) -> raise (Worker_failed (pid, Worker_oomed))
      | _ -> raise exn
    end
  | exn ->
    mark_free job.worker;
    handle := (fst !handle, Failed exn);
    raise exn

let get_result d =
  match snd !d with
  | Cached (x, _) -> x
  | Failed exn -> raise exn
  | Canceled -> raise End_of_file
  | Processing s ->
    with_worker_exn d s (fun () ->
        let res = s.result () in
        mark_free s.worker;
        d := (fst !d, Cached (res, s.worker));
        res)

(*****************************************************************************
 * Our polling primitive on workers
 * Given a list of handle, returns the ones that are ready.
 *
 *****************************************************************************)

type ('a, 'b) selected = {
  readys: ('a, 'b) handle list;
  waiters: ('a, 'b) handle list;
  ready_fds: Unix.file_descr list;
}

let get_processing ds =
  List.rev_filter_map ds ~f:(fun d ->
      match snd !d with
      | Processing p -> Some p
      | _ -> None)

let select ds additional_fds =
  let processing = get_processing ds in
  let fds = List.map ~f:(fun { infd; _ } -> infd) processing in
  let (ready_fds, _, _) =
    if List.is_empty fds || List.length processing <> List.length ds then
      ([], [], [])
    else
      Sys_utils.select_non_intr (fds @ additional_fds) [] [] (-1.)
  in
  let additional_ready_fds = List.filter ~f:(List.mem ~equal:Poly.( = ) ready_fds) additional_fds in
  List.fold_right
    ~f:(fun d acc ->
      match snd !d with
      | Cached _
      | Canceled
      | Failed _ ->
        { acc with readys = d :: acc.readys }
      | Processing s when List.mem ~equal:Poly.( = ) ready_fds s.infd ->
        { acc with readys = d :: acc.readys }
      | Processing _ -> { acc with waiters = d :: acc.waiters })
    ~init:{ readys = []; waiters = []; ready_fds = additional_ready_fds }
    ds

let get_worker h =
  match snd !h with
  | Processing { worker; _ } -> worker
  | Cached (_, worker) -> worker
  | Canceled
  | Failed _ ->
    invalid_arg "Worker.get_worker"

let get_job h = fst (fst !h)

let get_call_id h = snd (fst !h)

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not (is_killed w) then (
    w.killed <- true;
    Base.Option.iter ~f:Daemon.kill w.prespawned
  )

let killall () = List.iter ~f:kill !workers

let wait_for_cancel d =
  match snd !d with
  | Processing s ->
    with_worker_exn d s (fun () ->
        s.wait_for_cancel ();
        mark_free s.worker;
        d := (fst !d, Canceled))
  | _ -> ()

let cancel handles =
  WorkerCancel.stop_workers ();
  List.iter handles ~f:(fun x -> wait_for_cancel x);
  WorkerCancel.resume_workers ();
  ()
