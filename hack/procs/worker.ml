(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

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

(* Should we 'prespawn' the worker ? *)
let use_prespawned = not Sys.win32

(* The maximum amount of workers *)
let max_workers = 1000



(*****************************************************************************
 * The job executed by the worker.
 *
 * The 'serializer' is the job continuation: it is a function that must
 * be called at the end of the request ir order to send back the result
 * to the master (this is "internal business", this is not visible outside
 * this module). The slave will provide the expected function.
 * cf 'send_result' in 'slave_main'.
 *
 *****************************************************************************)

type request = Request of (serializer -> unit)
and serializer = { send: 'a. 'a -> unit }
and void (* an empty type *)



(*****************************************************************************
 * Everything we need to know about a worker.
 *
 *****************************************************************************)

type t = {

  id: int; (* Simple id for the worker. This is not the worker pid: on
              Windows, we spawn a new worker for each job. *)

  (* Sanity check: is the worker still available ? *)
  mutable killed: bool;

  (* Sanity check: is the worker currently busy ? *)
  mutable busy: bool;

  (* On Unix, a reference to the 'prespawned' worker. *)
  prespawned: (void, request) Daemon.handle option;

  (* On Windows, a function to spawn a slave. *)
  spawn: unit -> (void, request) Daemon.handle;

}



(*****************************************************************************
 * The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 *
 *****************************************************************************)

type 'a handle = 'a delayed ref

and 'a delayed =
  | Processing of 'a slave
  | Cached of 'a
  | Failed of exn

and 'a slave = {

  worker: t;      (* The associated worker *)
  slave_pid: int; (* The actual slave pid *)

  (* The file descriptor we might pass to select in order to
     wait for the slave to finish its job. *)
  infd: Unix.file_descr;

  (* A blocking function that returns the job result. *)
  result: unit -> 'a;

}



(*****************************************************************************
 * Entry point for spawned worker.
 *
 *****************************************************************************)

let slave_main ic oc =
  let send_result data =
    let s = Marshal.to_string data [Marshal.Closures] in
    let len = String.length s in
    if len > 10 * 1024 * 1024 (* 10 MB *) then begin
      Hh_logger.log "WARNING: you are sending quite a lot of data (%d bytes), \
        which may have an adverse performance impact. If you are sending \
        closures, double-check to ensure that they have not captured large
        values in their environment." len;
      Printf.eprintf "%s" (Printexc.raw_backtrace_to_string
        (Printexc.get_callstack 100));
    end;
    Daemon.output_string oc s;
    Daemon.flush oc in
  try
    let Request do_process = Daemon.from_channel ic in
    do_process { send = send_result };
    exit 0
  with
  | End_of_file ->
      exit 1
  | SharedMem.Out_of_shared_memory ->
      Exit_status.(exit Out_of_shared_memory)
  | e ->
      let e_str = Printexc.to_string e in
      Printf.printf "Exception: %s\n" e_str;
      EventLogger.worker_exception e_str;
      print_endline "Potential backtrace:";
      Printexc.print_backtrace stdout;
      exit 2

let win32_worker_main restore state (ic, oc) =
  restore state;
  slave_main ic oc

let unix_worker_main restore state (ic, oc) =
  restore state;
  let in_fd = Daemon.descr_of_in_channel ic in
  if !Utils.profile then Utils.log := prerr_endline;
  try
    while true do
      (* Wait for an incoming job : is there something to read?
         But we don't read it yet. It will be read by the forked slave. *)
      let readyl, _, _ = Unix.select [in_fd] [] [] (-1.0) in
      if readyl = [] then exit 0;
      (* We fork a slave for every incoming request.
         And let it die after one request. This is the quickest GC. *)
      match Fork.fork() with
      | 0 -> slave_main ic oc
      | pid ->
          (* Wait for the slave termination... *)
          match snd (Unix.waitpid [] pid) with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED 1 ->
              raise End_of_file
          | Unix.WEXITED code ->
              Printf.printf "Worker exited (code: %d)\n" code;
              flush stdout;

              (* Propagate out of memory exit codes *)
              if code = Exit_status.(exit_code Out_of_shared_memory)
              then Exit_status.(exit Out_of_shared_memory)
              else raise End_of_file
          | Unix.WSIGNALED x ->
              let sig_str = PrintSignal.string_of_signal x in
              Printf.printf "Worker interrupted with signal: %s\n" sig_str;
              exit 2
          | Unix.WSTOPPED x ->
              Printf.printf "Worker stopped with signal: %d\n" x;
              exit 3
    done;
    assert false
  with End_of_file -> exit 0

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
let make_one spawn id =
  if id >= max_workers then failwith "Too many workers";

  let prespawned = if not use_prespawned then None else Some (spawn ()) in
  let worker = { id; busy = false; killed = false; prespawned; spawn } in
  workers := worker :: !workers;
  worker

let make ~saved_state ~entry ~nbr_procs ~gc_control ~heap_handle =
  let spawn log_fd =
    Unix.clear_close_on_exec heap_handle.SharedMem.h_fd;
    let handle =
      Daemon.spawn
        Unix.(stdout, stderr)
        entry
        (saved_state, gc_control, heap_handle) in
    Unix.set_close_on_exec heap_handle.SharedMem.h_fd;
    handle
  in
  let made_workers = ref [] in
  for n = 1 to nbr_procs do
    made_workers := make_one spawn n :: !made_workers
  done;
  !made_workers

(**************************************************************************
 * Send a job to a worker
 *
 **************************************************************************)

let call w (type a) (type b) (f : a -> b) (x : a) : b handle =
  if w.killed then Printf.ksprintf failwith "killed worker (%d)" w.id;
  if w.busy then Printf.ksprintf failwith "busy worker (%d)" w.id;
  (* Spawn the slave, if not prespawned. *)
  let { Daemon.pid = slave_pid; channels = (inc, outc) } as h =
    match w.prespawned with
    | None -> w.spawn ()
    | Some handle -> handle in
  (* Prepare ourself to read answer from the slave. *)
  let result () : b =
    match Unix.waitpid [Unix.WNOHANG] slave_pid with
    | 0, _ | _, Unix.WEXITED 0 ->
        let res : b = Daemon.input_value inc in
        if w.prespawned = None then Daemon.close h;
        res
    | _, Unix.WEXITED i when i = Exit_status.(exit_code Out_of_shared_memory) ->
        raise SharedMem.Out_of_shared_memory
    | _, Unix.WEXITED i ->
        Printf.ksprintf failwith "Subprocess(%d): fail %d" slave_pid i
    | _, Unix.WSTOPPED i ->
        Printf.ksprintf failwith "Subprocess(%d): stopped %d" slave_pid i
    | _, Unix.WSIGNALED i ->
        Printf.ksprintf failwith "Subprocess(%d): signaled %d" slave_pid i in
  (* Mark the worker as busy. *)
  let infd = Daemon.descr_of_in_channel inc in
  let slave = { result; slave_pid; infd; worker = w; } in
  w.busy <- true;
  (* Send the job to the slave. *)
  Daemon.to_channel outc
    ~flush:true ~flags:[Marshal.Closures]
    (Request (fun { send } -> send (f x)));
  (* And returned the 'handle'. *)
  ref (Processing slave)


(**************************************************************************
 * Read results from a handle.
 * This might block if the worker hasn't finished yet.
 *
 **************************************************************************)

let get_result d =
  match !d with
  | Cached x -> x
  | Failed exn -> raise exn
  | Processing s ->
      try
        let res = s.result () in
        s.worker.busy <- false;
        d := Cached res;
        res
      with exn ->
        s.worker.busy <- false;
        d := Failed exn;
        raise exn


(*****************************************************************************
 * Our polling primitive on workers
 * Given a list of handle, returns the ones that are ready.
 *
 *****************************************************************************)

type 'a selected = {
  readys: 'a handle list;
  waiters: 'a handle list;
}

let get_processing ds =
  List.rev_filter_map
    ds
    ~f:(fun d -> match !d with Processing p -> Some p | _ -> None)

let select ds =
  let processing = get_processing ds in
  let fds = List.map ~f:(fun {infd; _} -> infd) processing in
  let ready_fds, _, _ =
    if fds = [] || List.length processing <> List.length ds then
      [], [], []
    else
      Unix.select fds [] [] ~-.1. in
  List.fold_right
    ~f:(fun d { readys ; waiters } ->
      match !d with
      | Cached _ | Failed _ ->
          { readys = d :: readys ; waiters }
      | Processing s when List.mem ready_fds s.infd ->
          { readys = d :: readys ; waiters }
      | Processing _ ->
          { readys ; waiters = d :: waiters})
    ~init:{ readys = [] ; waiters = [] }
    ds

let get_worker h =
  match !h with
  | Processing {worker; _} -> worker
  | Cached _
  | Failed _ -> invalid_arg "Worker.get_worker"

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not w.killed then begin
    w.killed <- true;
    match w.prespawned with
    | None -> ()
    | Some handle -> Daemon.kill handle
  end

let killall () =
  List.iter ~f:kill !workers
