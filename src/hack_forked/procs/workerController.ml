(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
 * On Unix, we "spawn" workers when initializing Flow. Then, this
 * worker, "fork" a clone process for each incoming request.
 * The forked "clone" will die after processing a single request.
 *
 * On Windows, we also "prespawn" when initializing Flow, but we just
 * just handle all requests in the spawned process without forking for each job.
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
  Stdlib.Printexc.register_printer @@ function
  | Worker_failed_to_send_job (Other_send_job_failure exn) ->
    Some (Printf.sprintf "Other_send_job_failure: %s" (Exception.to_string exn))
  | Worker_failed_to_send_job (Worker_already_exited status) ->
    Some (Printf.sprintf "Worker_already_exited: %s" (status_string status))
  | Worker_failed (id, failure) ->
    Some (Printf.sprintf "Worker_failed (process_id = %d): %s" id (failure_to_string failure))
  | _ -> None

(* The maximum amount of workers *)
let max_workers = 1000

type void (* an empty type *)

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
  (* Sanity check: is the worker still available ? *)
  mutable killed: bool;
  (* Sanity check: is the worker currently busy ? *)
  mutable busy: bool;
  (* A reference to the prespawned worker. *)
  handle: (void, request) Daemon.handle;
}
[@@warning "-69"]

let worker_id w = w.id

(* Has the worker been killed *)
let is_killed w = w.killed

(* Mark the worker as busy. Throw if it is already busy *)
let mark_busy w =
  if w.busy then raise Worker_busy;
  w.busy <- true

(* Mark the worker as free *)
let mark_free w = w.busy <- false

type 'a entry_state = 'a * Stdlib.Gc.control * SharedMem.handle * int * Worker.worker_mode

type 'a entry = ('a entry_state, request, void) Daemon.entry

let entry_counter = ref 0

let register_entry_point ~restore =
  Int.incr entry_counter;
  let restore (st, gc_control, heap_handle, worker_id, worker_mode) =
    restore st ~worker_id;
    SharedMem.connect heap_handle ~worker_id;
    Stdlib.Gc.set gc_control;
    worker_mode
  in
  let name = Printf.sprintf "worker_%d" !entry_counter in
  Daemon.register_entry_point name (worker_main restore)

(**************************************************************************
 * Creates a pool of workers.
 *
 **************************************************************************)

let workers = ref []

(* Build one worker. *)
let make_one spawn id =
  if id >= max_workers then failwith "Too many workers";
  let handle = spawn () in
  let worker = { id; busy = false; killed = false; handle } in
  workers := worker :: !workers;
  worker

(* Make a few workers. When workload is given to a worker (via "call" below),
 * the workload is wrapped in the calL_wrapper. *)
let make ~worker_mode ~channel_mode ~saved_state ~entry ~nbr_procs ~gc_control ~heap_handle =
  let spawn worker_id name () =
    Unix.clear_close_on_exec heap_handle;

    let state = (saved_state, gc_control, heap_handle, worker_id, worker_mode) in
    let handle =
      Daemon.spawn ~channel_mode ~name (Daemon.null_fd (), Unix.stdout, Unix.stderr) entry state
    in
    Unix.set_close_on_exec heap_handle;
    handle
  in
  let made_workers = ref [] in
  let pretty_pid = Sys_utils.get_pretty_pid () in
  for n = 1 to nbr_procs do
    let name = Printf.sprintf "worker process %d/%d for server %d" n nbr_procs pretty_pid in
    made_workers := make_one (spawn n name) n :: !made_workers
  done;
  !made_workers

(** Sends a request to call `f x` on `worker` *)
let send_blocking worker worker_pid outfd_lwt (f : 'a -> 'b) (x : 'a) : unit Lwt.t =
  let outfd = Lwt_unix.unix_file_descr outfd_lwt in
  let request = Request (fun { send } -> send (f x)) in
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
    let _ = Marshal_tools.to_fd_with_preamble ~flags:[Stdlib.Marshal.Closures] outfd request in
    Lwt.return_unit
  with
  | exn ->
    let exn = Exception.wrap exn in
    Hh_logger.error ~exn "Failed to send request to worker #%d" (worker_id worker);

    (* Failed to send the job to the worker. Is it because the worker is dead or is it
     * something else? *)
    (match%lwt Lwt_unix.waitpid [Unix.WNOHANG] worker_pid with
    | (0, _) -> raise (Worker_failed_to_send_job (Other_send_job_failure exn))
    | (_, status) -> raise (Worker_failed_to_send_job (Worker_already_exited (Some status)))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      raise (Worker_failed_to_send_job (Worker_already_exited None)))

(** Sends a request to call `f x` on `worker`
 *
 * If we start sending a job, we must complete that work. Workers will treat a partial job as a
 * fatal error and exit.
 *
 * First we call `wait_write` to cancel quickly in the common case where we have not yet sent a
 * single byte.
 *
 * Note that Lwt.protected does not wait for the inner promise to resolve when it is canceled. We
 * use the `sent_request` ref to synchronize with the inner promise from the Canceled exception
 * handler.
 *)
let send_non_blocking worker worker_pid infd outfd (f : 'a -> 'b) (x : 'a) : unit Lwt.t =
  let request = Request (fun { send } -> send (f x)) in
  let sent_request = ref false in
  try%lwt
    let%lwt () = Lwt_unix.wait_write outfd in
    Lwt.protected
      ( (* This write must happen first, to synchronize with the Canceled exception handler. *)
        sent_request := true;
        let%lwt _ =
          Marshal_tools_lwt.to_fd_with_preamble ~flags:[Stdlib.Marshal.Closures] outfd request
        in
        Lwt.return_unit
      )
  with
  | Lwt.Canceled as exn ->
    (* Cancel request while sending a job to the worker. *)
    let exn = Exception.wrap exn in

    (* Each worker might call this but that's ok *)
    WorkerCancel.stop_workers ();

    (* If we sent the request, we need to wait for the response and drain the pipe. Note that
       workers may send a full response, so handle the `Some _` case as well. *)
    let%lwt () =
      if !sent_request then
        (* We should not be canceled again at this point, but just in case prevent this operation
           from being canceled. We will re-raise the Canceled exception anyway. *)
        Lwt.no_cancel
          (match%lwt Marshal_tools_lwt.from_fd_with_preamble infd with
          | None -> Lwt.return_unit
          | Some _ ->
            let%lwt _ = Marshal_tools_lwt.from_fd_with_preamble infd in
            Lwt.return_unit)
      else
        Lwt.return_unit
    in

    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    Hh_logger.error ~exn "Failed to send request to worker #%d" (worker_id worker);

    (* Failed to send the job to the worker. Is it because the worker is dead or is it
     * something else? *)
    (match%lwt Lwt_unix.waitpid [Unix.WNOHANG] worker_pid with
    | (0, _) -> raise (Worker_failed_to_send_job (Other_send_job_failure exn))
    | (_, status) -> raise (Worker_failed_to_send_job (Worker_already_exited (Some status)))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      raise (Worker_failed_to_send_job (Worker_already_exited None)))

let read_blocking (type result) worker_pid infd_lwt : (result * Measure.record_data) option Lwt.t =
  let infd = Lwt_unix.unix_file_descr infd_lwt in
  try%lwt
    (* Wait in an lwt-friendly manner for the worker to finish the job *)
    let%lwt () = Lwt_unix.wait_read infd_lwt in

    (* Read in a lwt-unfriendly, blocking manner from the worker.

       Unlike writing (see `send`), reading from the worker didn't seem to trigger a perf issue
       in our testing, but there's really nothing more urgent than reading a response from a
       finished worker, so reading in a blocking manner is fine. *)
    (* Due to https://github.com/ocsigen/lwt/issues/564, annotation cannot go on let%let node *)
    let data : result option = Marshal_tools.from_fd_with_preamble infd in
    match data with
    | None -> Lwt.return_none
    | Some data ->
      let stats : Measure.record_data = Marshal_tools.from_fd_with_preamble infd in
      Lwt.return (Some (data, stats))
  with
  | Lwt.Canceled as exn ->
    (* Worker is handling a job but we're cancelling *)
    let exn = Exception.wrap exn in

    (* Each worker might call this but that's ok *)
    WorkerCancel.stop_workers ();

    (* Wait for the worker to finish cancelling *)
    let%lwt () = Lwt_unix.wait_read infd_lwt in
    (* Read the junk from the pipe. If the worker was almost finished, it will
       send real data, so handle both cases. *)
    begin
      match Marshal_tools.from_fd_with_preamble infd with
      | None -> ()
      | Some _ -> ignore (Marshal_tools.from_fd_with_preamble infd)
    end;
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
        let () = Stdlib.Printf.eprintf "Subprocess(%d): fail %d\n%!" worker_pid i in
        raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WEXITED i)))))
    | (_, Unix.WSTOPPED i) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): stopped %d\n%!" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSTOPPED i))))
    | (_, Unix.WSIGNALED i) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): signaled %d\n%!" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSIGNALED i))))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): gone\n%!" worker_pid in
      raise (Worker_failed (worker_pid, Worker_quit None)))

(** Reads a response from the worker
 *
 * If we start reading a response, we must complete that work. Otherwise, we will leave bytes on the
 * pipe, causing the next read to be corrupted.
 *
 * First we call `wait_read` to cancel quickly in the common case where we have not yet read a
 * single byte.
 *
 * Note that Lwt.protected does not wait for the inner promise to resolve when it is canceled. We
 * use the `read_response` ref to synchronize with the inner promise from the Canceled exception
 * handler.
 *)
let read_non_blocking (type result) worker_pid infd : (result * Measure.record_data) option Lwt.t =
  let read_response = ref false in
  let (wait_for_read_to_finish, signal_finished_read) = Lwt.wait () in
  try%lwt
    (* Ensure we finish reading a response if we get one. If we cancel while reading, then the pipe
       will still contain the unread junk which will cause the next read to fail. The call to
       `wait_read` is outside the protected block so we can quickly cancelin the common case where
       we have not yet read a single byte. *)
    let%lwt () = Lwt_unix.wait_read infd in
    Lwt.protected
      ( (* This write must happen first, to synchronize with the Canceled exception handler. *)
        read_response := true;
        let%lwt (data : result option) = Marshal_tools_lwt.from_fd_with_preamble infd in
        match data with
        | None ->
          Lwt.wakeup signal_finished_read ();
          Lwt.return_none
        | Some data ->
          let%lwt (stats : Measure.record_data) = Marshal_tools_lwt.from_fd_with_preamble infd in
          Lwt.wakeup signal_finished_read ();
          Lwt.return (Some (data, stats))
      )
  with
  | Lwt.Canceled as exn ->
    (* Worker is handling a job but we're cancelling *)
    let exn = Exception.wrap exn in

    (* Each worker might call this but that's ok *)
    WorkerCancel.stop_workers ();

    (* We need to wait for the response and drain the pipe. Note that workers may send a full
       response, so handle the `Some _` case as well. *)
    let%lwt () =
      if !read_response then
        let%lwt () = wait_for_read_to_finish in
        Lwt.return_unit
      else
        (* We should not be canceled again at this point, but just in case prevent this operation
           from being canceled. We will re-raise the Canceled exception anyway. *)
        Lwt.no_cancel
          (match%lwt Marshal_tools_lwt.from_fd_with_preamble infd with
          | None -> Lwt.return_unit
          | Some _ ->
            let%lwt _ = Marshal_tools_lwt.from_fd_with_preamble infd in
            Lwt.return_unit)
    in
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
        let () = Stdlib.Printf.eprintf "Subprocess(%d): fail %d\n%!" worker_pid i in
        raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WEXITED i)))))
    | (_, Unix.WSTOPPED i) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): stopped %d\n%!" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSTOPPED i))))
    | (_, Unix.WSIGNALED i) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): signaled %d\n%!" worker_pid i in
      raise (Worker_failed (worker_pid, Worker_quit (Some (Unix.WSIGNALED i))))
    | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
      let () = Stdlib.Printf.eprintf "Subprocess(%d): gone\n%!" worker_pid in
      raise (Worker_failed (worker_pid, Worker_quit None)))

(** Send a job to a worker

    This is basically an lwt thread that writes a job to the worker, waits for the response, and
    then returns the result. *)
let call ~blocking w (f : 'a -> 'b) (x : 'a) : 'b option Lwt.t =
  if is_killed w then Printf.ksprintf failwith "killed worker (%d)" (worker_id w);
  mark_busy w;
  let { Daemon.pid = worker_pid; channels = (inc, outc) } = w.handle in
  let infd = Lwt_unix.of_unix_file_descr (Daemon.descr_of_in_channel inc) in
  let outfd = Lwt_unix.of_unix_file_descr (Daemon.descr_of_out_channel outc) in
  (let%lwt () =
     if blocking then
       send_blocking w worker_pid outfd f x
     else
       send_non_blocking w worker_pid infd outfd f x
   in
   let%lwt result =
     if blocking then
       read_blocking worker_pid infd
     else
       read_non_blocking worker_pid infd
   in
   match result with
   | None -> Lwt.return_none
   | Some (data, stats) ->
     Measure.merge (Measure.deserialize stats);
     Lwt.return (Some data)
  )
    [%lwt.finally
      mark_free w;
      Lwt.return_unit]

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not (is_killed w) then (
    w.killed <- true;
    Daemon.kill w.handle
  )

let killall () = List.iter ~f:kill !workers
