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
 * worker, "fork" a slave for each incoming request. The forked "slave"
 * will die after processing a single request.
 *
 * On Windows, we do not "prespawn" when initializing Hack, but we just
 * allocate all the required information into a record. Then, we
 * spawn a slave for each incoming request. It will also die after
 * one request.
 *
 * A worker never handle more than one request at a time.
 *
 *****************************************************************************)

type process_id = int
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
  id: int; (* Simple id for the worker. This is not the worker pid: on
              Windows, we spawn a new worker for each job. *)


  (** The call wrapper will wrap any workload sent to the worker (via "call"
   * below) before invoking the workload.
   *
   * That is, when calling the worker with workload `f x`, it will be wrapped
   * as `wrap (f x)`.
   *
   * This allows universal handling of workload at the time we create the actual
   * workers. For example, this can be useful to handle exceptions uniformly
   * across workers regardless what workload is called on them. *)
  call_wrapper: call_wrapper option;

  (* Sanity check: is the worker still available ? *)
  mutable killed: bool;

  (* Sanity check: is the worker currently busy ? *)
  mutable busy: bool;

  (* On Unix, a reference to the 'prespawned' worker. *)
  prespawned: (void, request) Daemon.handle option;

  (* On Windows, a function to spawn a slave. *)
  spawn: unit -> (void, request) Daemon.handle;
}

let worker_id w = w.id

(* Has the worker been killed *)
let is_killed w = w.killed

(* Mark the worker as busy. Throw if it is already busy *)
let mark_busy w =
  if w.busy then raise Worker_busy;
  w.busy <- true

(* Mark the worker as free *)
let mark_free w = w.busy <- false

(* If the worker isn't prespawned, spawn the worker *)
let spawn w = match w.prespawned with
| None -> w.spawn ()
| Some handle -> handle

(* If the worker isn't prespawned, close the worker *)
let close w h = if w.prespawned = None then Daemon.close h

(* If there is a call_wrapper, apply it and create the Request *)
let wrap_request w f x = match w.call_wrapper with
  | Some { wrap } -> Request (fun { send } -> send (wrap f x))
  | None -> Request (fun { send } -> send (f x))

(*****************************************************************************
 * The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 *
 *****************************************************************************)

type ('a, 'b) handle = ('a, 'b) delayed ref

and ('a, 'b) delayed = 'a * 'b worker_handle

and 'b worker_handle =
  | Processing of 'b slave
  | Cached of 'b
  | Failed of exn

and 'a slave = {

  worker: worker;      (* The associated worker *)
  slave_pid: int; (* The actual slave pid *)

  (* The file descriptor we might pass to select in order to
     wait for the slave to finish its job. *)
  infd: Unix.file_descr;

  (* A blocking function that returns the job result. *)
  result: unit -> 'a;

  (* A blocking function that waits for job cancellation (see Worker.cancel)
   * to finish *)
  wait_for_cancel: unit -> unit;

}

type 'a entry_state = 'a * Gc.control * SharedMem.handle
type 'a entry = ('a entry_state, request, void) Daemon.entry

let entry_counter = ref 0
let register_entry_point ~restore =
  incr entry_counter;
  let restore (st, gc_control, heap_handle) =
    restore st;
    SharedMem.connect heap_handle ~is_master:false;
    Gc.set gc_control in
  let name = Printf.sprintf "slave_%d" !entry_counter in
  Daemon.register_entry_point
    name
    (if Sys.win32
      then win32_worker_main restore
      else unix_worker_main restore)

(**************************************************************************
 * Creates a pool of workers.
 *
 **************************************************************************)

let workers = ref []

(* Build one worker. *)
let make_one ?call_wrapper spawn id =
  if id >= max_workers then failwith "Too many workers";

  let prespawned = if not use_prespawned then None else Some (spawn ()) in
  let worker = { call_wrapper; id; busy = false; killed = false; prespawned; spawn } in
  workers := worker :: !workers;
  worker

(** Make a few workers. When workload is given to a worker (via "call" below),
 * the workload is wrapped in the calL_wrapper. *)
let make ?call_wrapper ~saved_state ~entry ~nbr_procs ~gc_control ~heap_handle =
  let spawn name () =
    Unix.clear_close_on_exec heap_handle.SharedMem.h_fd;
    let handle =
      Daemon.spawn
        ~name
        (Daemon.null_fd (), Unix.stdout, Unix.stderr)
        entry
        (saved_state, gc_control, heap_handle) in
    Unix.set_close_on_exec heap_handle.SharedMem.h_fd;
    handle
  in
  let made_workers = ref [] in
  let pid = Unix.getpid () in
  for n = 1 to nbr_procs do
    let name = Printf.sprintf "worker process %d/%d for server %d" n nbr_procs pid in
    made_workers := make_one ?call_wrapper (spawn name) n :: !made_workers
  done;
  !made_workers

(**************************************************************************
 * Send a job to a worker
 *
 **************************************************************************)

let call w (type a) (type b) (f : a -> b) (x : a) : (a, b) handle =
  if is_killed w then Printf.ksprintf failwith "killed worker (%d)" (worker_id w);
  mark_busy w;

  (* Spawn the slave, if not prespawned. *)
  let { Daemon.pid = slave_pid; channels = (inc, outc) } as h = spawn w in

  let infd = Daemon.descr_of_in_channel inc in
  let outfd = Daemon.descr_of_out_channel outc in

  let with_exit_status_check slave_pid f =
    match Unix.waitpid [Unix.WNOHANG] slave_pid with
    | 0, _ | _, Unix.WEXITED 0 ->
        f ()
    | _, Unix.WEXITED i when i = Exit_status.(exit_code Out_of_shared_memory) ->
        raise SharedMem.Out_of_shared_memory
    | _, Unix.WEXITED i ->
        Printf.eprintf "Subprocess(%d): fail %d" slave_pid i;
        raise (Worker_failed (slave_pid, Worker_quit (Unix.WEXITED i)))
    | _, Unix.WSTOPPED i ->
        raise (Worker_failed (slave_pid, Worker_quit (Unix.WSTOPPED i)))
    | _, Unix.WSIGNALED i ->
        raise (Worker_failed (slave_pid, Worker_quit (Unix.WSIGNALED i)))
  in
  (* Prepare ourself to read answer from the slave. *)
  let result () : b =
    with_exit_status_check slave_pid begin fun () ->
      let res : b * Measure.record_data = Marshal_tools.from_fd_with_preamble infd in
      close w h;
      Measure.merge (Measure.deserialize (snd res));
      fst res
    end in
  let wait_for_cancel () : unit =
    with_exit_status_check slave_pid begin fun () ->
      (* Depending on whether we manage to kill the slave before it starts writing
       * results back, this will return either actual results, or "anything"
       * (written by interrupt signal that exited). The types don't match, but we
       * ignore both of them anyway. *)
      let _ : 'c = Marshal_tools.from_fd_with_preamble infd in
      ()
    end in

  (* Mark the worker as busy. *)
  let slave = { result; slave_pid; infd; worker = w; wait_for_cancel } in
  let request = wrap_request w f x in

  (* Send the job to the slave. *)
  let () =
    try Marshal_tools.to_fd_with_preamble ~flags:[Marshal.Closures] outfd request |> ignore
    with
    | e -> begin
      match Unix.waitpid [Unix.WNOHANG] slave_pid with
      | 0, _ ->
        raise (Worker_failed_to_send_job (Other_send_job_failure e))
      | _, status ->
        raise (Worker_failed_to_send_job (Worker_already_exited status))
    end
  in
  (* And returned the 'handle'. *)
  ref (x, Processing slave)


(**************************************************************************
 * Read results from a handle.
 * This might block if the worker hasn't finished yet.
 *
 **************************************************************************)

let with_worker_exn (handle : ('a, 'b) handle) slave f =
  try f () with
  | Worker_failed (pid, status) as exn ->
    slave.worker.busy <- false;
    handle := fst !handle, Failed exn;
    begin match status with
    | Worker_quit (Unix.WSIGNALED -7) ->
      raise (Worker_failed (pid, Worker_oomed))
    | _ ->
      raise exn
    end
  | exn ->
    slave.worker.busy <- false;
    handle := fst !handle, Failed exn;
    raise exn

let get_result d =
  match snd !d with
  | Cached x -> x
  | Failed exn -> raise exn
  | Processing s ->
    with_worker_exn d s begin fun () ->
      let res = s.result () in
      s.worker.busy <- false;
      d := fst !d, Cached res;
      res
    end

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
  List.rev_filter_map
    ds
    ~f:(fun d -> match snd !d with Processing p -> Some p | _ -> None)

let select ds additional_fds =
  let processing = get_processing ds in
  let fds = List.map ~f:(fun {infd; _} -> infd) processing in
  let ready_fds, _, _ =
    if fds = [] || List.length processing <> List.length ds then
      [], [], []
    else
      Sys_utils.select_non_intr (fds @ additional_fds) [] [] ~-.1. in
  let additional_ready_fds =
    List.filter ~f:(List.mem ready_fds) additional_fds in
  List.fold_right
    ~f:(fun d acc ->
      match snd !d with
      | Cached _ | Failed _ ->
          { acc with readys = d :: acc.readys }
      | Processing s when List.mem ready_fds s.infd ->
          { acc with readys = d :: acc.readys }
      | Processing _ ->
          { acc with waiters = d :: acc.waiters})
    ~init:{ readys = [] ; waiters = []; ready_fds = additional_ready_fds }
    ds

let get_worker h =
  match snd !h with
  | Processing {worker; _} -> worker
  | Cached _
  | Failed _ -> invalid_arg "Worker.get_worker"

let get_job h = fst !h

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not (is_killed w) then begin
    w.killed <- true;
    Option.iter ~f:Daemon.kill w.prespawned
  end

let killall () =
  List.iter ~f:kill !workers

let wait_for_cancel d =
  match snd !d with
  | Processing s ->
    with_worker_exn d s begin fun () ->
      s.wait_for_cancel ();
      s.worker.busy <- false
    end
  | _ -> ()

let cancel handles =
  SharedMem.stop_workers ();
  List.iter handles ~f:(fun x -> wait_for_cancel x);
  SharedMem.resume_workers ();
  ()
