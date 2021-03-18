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
  | Worker_oomed  (** Worker killed by Out Of Memory. *)
  | Worker_quit of Unix.process_status option

exception Worker_failed of (process_id * worker_failure)

exception Worker_busy

type send_job_failure =
  | Worker_already_exited of Unix.process_status option
  | Other_send_job_failure of Exception.t

exception Worker_failed_to_send_job of send_job_failure

let status_string = function
  | Some (Unix.WEXITED i) -> Printf.sprintf "WEXITED %d" i
  | Some (Unix.WSIGNALED i) -> Printf.sprintf "WSIGNALED %d" i
  | Some (Unix.WSTOPPED i) -> Printf.sprintf "WSTOPPED %d" i
  | None -> "GONE"

let failure_to_string f =
  match f with
  | Worker_oomed -> "Worker_oomed"
  | Worker_quit s -> Printf.sprintf "(Worker_quit %s)" (status_string s)

let () =
  Caml.Printexc.register_printer @@ function
  | Worker_failed_to_send_job (Other_send_job_failure exn) ->
    Some (Printf.sprintf "Other_send_job_failure: %s" (Exception.to_string exn))
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
  (* On Unix, a reference to the 'prespawned' worker. *)
  prespawned: (void, request) Daemon.handle option;
  (* On Windows, a function to spawn a worker. *)
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

(** [Gc.set] has a bug in all ocaml releases since 4.08.0 and before 2021-01-07,
    which causes the `custom_*` fields to be doubled, plus 1. To work around it, we
    halve those values. still off by one, so you'll still see them change if you run
    OCAMLRUNPARAM='v=0x20', but best we can do.

    Fixed in https://github.com/ocaml/ocaml/pull/10125 *)
let gc_set : Caml.Gc.control -> unit =
  let fixed_set control =
    let open Caml.Gc in
    let { custom_major_ratio; custom_minor_ratio; custom_minor_max_size; _ } = control in
    let control =
      {
        control with
        custom_major_ratio = custom_major_ratio lsr 1;
        custom_minor_ratio = custom_minor_ratio lsr 1;
        custom_minor_max_size = custom_minor_max_size lsr 1;
      }
    in
    Caml.Gc.set control
  in
  let v =
    let v = Sys.ocaml_version in
    match String.index v '+' with
    | Some i -> String.sub v 0 i
    | None -> v
  in
  (* the fix has been backported to all of these branches, so any
     version after these will be fixed. *)
  match v with
  | "4.11.1"
  | "4.11.0"
  | "4.10.2"
  | "4.10.1"
  | "4.09.1"
  | "4.09.0"
  | "4.08.1"
  | "4.08.0" ->
    fixed_set
  | _ -> Caml.Gc.set

let register_entry_point ~restore =
  Int.incr entry_counter;
  let restore (st, gc_control, heap_handle, worker_id) =
    restore st ~worker_id;
    SharedMem.connect heap_handle ~worker_id;
    gc_set gc_control
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
    { call_wrapper; controller_fd; id; busy = false; killed = false; prespawned; spawn }
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

(** Sends a request to call `f x` on `worker` *)
let send worker worker_pid outfd (f : 'a -> 'b) (x : 'a) : unit Lwt.t =
  let outfd_lwt = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true outfd in
  let request = wrap_request worker f x in
  try%lwt
    (* Wait in an lwt-friendly manner for the worker to be writable (should be instant) *)
    let%lwt () = Lwt_unix.wait_write outfd_lwt in

    (* Write in a lwt-unfriendly, blocking manner to the worker.

       I, glevi, found a perf regression when I used Marshal_tools_lwt to send the job
       to the worker. Here's my hypothesis:

       1. On a machine with many CPUs (like 56) we create 56 threads to send a job to each worker.
       2. Lwt attempts to write the jobs to the workers in parallel.
       3. Each worker spends more time between getting the first byte and last byte
       4. Something something this leads to more context switches for the worker
       5. The worker spends more time on a job

       This is reinforced by the observation that the regression only happens as the number of
       workers grows.

       By switching from Marshal_tools_lwt.to_fd_with_preamble to Marshal_tools.to_fd_with_preamble,
       the issue seems to have disappeared. *)
    let _ = Marshal_tools.to_fd_with_preamble ~flags:[Caml.Marshal.Closures] outfd request in
    Lwt.return_unit
  with exn ->
    let exn = Exception.wrap exn in
    Hh_logger.error ~exn "Failed to read response from work #%d" (worker_id worker);

    (* Failed to send the job to the worker. Is it because the worker is dead or is it
     * something else? *)
    (match%lwt Lwt_unix.waitpid [Unix.WNOHANG] worker_pid with
    | (0, _) -> raise (Worker_failed_to_send_job (Other_send_job_failure exn))
    | (_, status) -> raise (Worker_failed_to_send_job (Worker_already_exited (Some status)))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      raise (Worker_failed_to_send_job (Worker_already_exited None)))

let read (type result) worker_pid infd : (result * Measure.record_data) Lwt.t =
  let infd_lwt = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true infd in
  try%lwt
    (* Wait in an lwt-friendly manner for the worker to finish the job *)
    let%lwt () = Lwt_unix.wait_read infd_lwt in

    (* Read in a lwt-unfriendly, blocking manner from the worker.

       Unlike writing (see `send`), reading from the worker didn't seem to trigger a perf issue
       in our testing, but there's really nothing more urgent than reading a response from a
       finished worker, so reading in a blocking manner is fine. *)
    (* Due to https://github.com/ocsigen/lwt/issues/564, annotation cannot go on let%let node *)
    let data : result = Marshal_tools.from_fd_with_preamble infd in
    let stats : Measure.record_data = Marshal_tools.from_fd_with_preamble infd in
    Lwt.return (data, stats)
  with
  | Lwt.Canceled as exn ->
    (* Worker is handling a job but we're cancelling *)
    let exn = Exception.wrap exn in

    (* Each worker might call this but that's ok *)
    WorkerCancel.stop_workers ();

    (* Wait for the worker to finish cancelling *)
    let%lwt () = Lwt_unix.wait_read infd_lwt in
    (* Read the junk from the pipe *)
    let _ = Marshal_tools.from_fd_with_preamble infd in
    let _ = Marshal_tools.from_fd_with_preamble infd in
    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    (match%lwt Lwt_unix.waitpid [Unix.WNOHANG] worker_pid with
    | (0, _)
    | (_, Unix.WEXITED 0) ->
      (* The worker is still running or exited normally. It's odd that we failed to read
       * the response, so just raise that exception *)
      Exception.reraise exn
    | (_, Unix.WEXITED i) ->
      (match Exit.error_type_opt i with
      | Some Exit.Out_of_shared_memory -> raise SharedMem.Out_of_shared_memory
      | Some Exit.Hash_table_full -> raise SharedMem.Hash_table_full
      | Some Exit.Heap_full -> raise SharedMem.Heap_full
      | _ ->
        let () = Caml.Printf.eprintf "Subprocess(%d): fail %d" worker_pid i in
        raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WEXITED i)))))
    | (_, Unix.WSTOPPED i) ->
      let () = Caml.Printf.eprintf "Subprocess(%d): stopped %d" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSTOPPED i))))
    | (_, Unix.WSIGNALED i) ->
      let () = Caml.Printf.eprintf "Subprocess(%d): signaled %d" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSIGNALED i))))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      let () = Caml.Printf.eprintf "Subprocess(%d): gone" worker_pid in
      raise (Worker_failed (worker_pid, Worker_quit None)))

(** Send a job to a worker

    This is basically an lwt thread that writes a job to the worker, waits for the response, and
    then returns the result. *)
let call w (f : 'a -> 'b) (x : 'a) : 'b Lwt.t =
  if is_killed w then Printf.ksprintf failwith "killed worker (%d)" (worker_id w);
  mark_busy w;

  (* Spawn the worker, if not prespawned. *)
  let ({ Daemon.pid = worker_pid; channels = (inc, outc) } as h) = spawn w in
  let infd = Daemon.descr_of_in_channel inc in
  let outfd = Daemon.descr_of_out_channel outc in
  Lwt.finalize
    (fun () ->
      let%lwt () = send w worker_pid outfd f x in
      let%lwt (res, measure_data) = read worker_pid infd in
      close w h;
      Measure.merge (Measure.deserialize measure_data);
      Lwt.return res)
    (fun () ->
      (* No matter what, always mark worker as free when we're done *)
      mark_free w;
      Lwt.return_unit)

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not (is_killed w) then (
    w.killed <- true;
    Base.Option.iter ~f:Daemon.kill w.prespawned
  )

let killall () = List.iter ~f:kill !workers
