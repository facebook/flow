(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external hh_worker_init: unit -> unit = "hh_worker_init"

(*****************************************************************************)
(* Module building workers
 * A worker is a subprocess executing an arbitrary function
 * You should first create a fixed amount of workers and then use those
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml)
 *)
(*****************************************************************************)

(* The maximum amount of workers *)
let max_workers = 1000

(*****************************************************************************)
(* The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
 * Note that the scheduler has to use a handle for that. But the handle
 * is just a trick to get type-checking on workers, a handle is a
 * phantom type, it doesn't really have a value.
 *)
(*****************************************************************************)
type 'a handle

(*****************************************************************************)
(* Pipes, we need to keep some extra descriptors around to make select work.
 *)
(*****************************************************************************)

type ('a, 'b) pipe = {

    (* Inputs *)
    pipe_descr_in  : Unix.file_descr ;
    pipe_fin       : (unit -> 'a)    ;

    (* Outputs *)
    pipe_descr_out : Unix.file_descr ;
    pipe_fout      : ('b -> unit)    ;
  }

let (make_pipe: unit -> ('a, 'b) pipe) = fun () ->
  let descr_ic, descr_oc = Unix.pipe() in
  (* close descriptors on exec so they are not leaked *)
  Unix.set_close_on_exec descr_ic;
  Unix.set_close_on_exec descr_oc;

  let ic = Unix.in_channel_of_descr descr_ic in
  let oc = Unix.out_channel_of_descr descr_oc in
  let input() = Marshal.from_channel ic in
  let output data = Marshal.to_channel oc data [Marshal.Closures]; flush oc in
  { pipe_descr_in = descr_ic;
    pipe_fin = input;
    pipe_descr_out = descr_oc;
    pipe_fout = output;
  }

(*****************************************************************************)
(* The job executed by the worker. The worker will execute (msg.job msg.arg)
 *)
(*****************************************************************************)

type ('a, 'b) msg = {
    job: ('a -> 'b) ;
    arg: 'a         ;
  }

(*****************************************************************************)
(* Everything we need to know about a worker.
 * It is called real_t and not t because the type-system is not flexible
 * enough in this case. A worker is something very polymorphic, we want
 * to be able to use the same worker with any type.
 * To do so, we force the type-checker to consider a worker as a black-box.
 * Internally (within this file), a worker is a "real_t" (to preserve as much
 * typing as we can), externally it is a "t" (an abstract type).
 * We force the conversion using Obj.magic.
 *)
(*****************************************************************************)

type ('a, 'b) real_t = {

    (* Unix process ID *)
    pid         : int                    ;

    (* Used by the worker to output the result of the job *)
    send_task   : (('a, 'b) msg -> unit) ;

    (* Used by the scheduler to retrieve the result of a job *)
    recv_result : (unit -> 'b)           ;

    (* We need to keep this file descriptors around to use Unix.select *)
    descr_recv  : Unix.file_descr        ;

    (* parent's write end that should be closed by other worker childs *)
    descr_send  : Unix.file_descr        ;
  }

(* The type of a worker visible to the outside world *)
type t

(*****************************************************************************)
(* Our polling primitive on workers
 * Given a list workers, returns the ones that a ready for more work.
 *)
(*****************************************************************************)

type ('a, 'b) worker_list = ('a, 'b) real_t list

let select: ('a, 'b) worker_list -> ('a, 'b) worker_list =
fun tl ->
  let fdl = List.map (fun x -> x.descr_recv) tl in
  let readyl, _, _ = Unix.select fdl [] [] (-1.0) in
  let res = List.filter (fun x -> List.mem x.descr_recv readyl) tl in
  res

(*****************************************************************************)
(* Creates a pool of workers. It's important to create them all at once,
 * because we would duplicate some file descriptors during the fork otherwise.
 *)
(*****************************************************************************)

module MakeWorker = struct

  (* The type of the accumulator *)
  type ('a, 'b) acc = ('a, 'b) worker_list

  (* The current amount of "live" workers *)
  let current_workers = ref 0

  let rec make: ('a, 'b) acc -> int -> Gc.control -> ('a, 'b) acc =
  fun acc n gc_control ->
    incr current_workers;
    if !current_workers > max_workers
    then failwith "Too many workers"
    else if n <= 0
    then acc
    else make_ acc n gc_control

  and make_: ('a, 'b) acc -> int -> Gc.control -> ('a, 'b) acc =
  fun acc n gc_control ->
    (* Initializing a bidirectional pipe *)
    let pipe_parent_reads_child_sends = make_pipe() in
    let pipe_parent_sends_child_reads = make_pipe() in
    let {
      (* Parent reads *)
      pipe_descr_in = descr_parent_reads;
      pipe_fin = parent_reads_result;

      (* Child sends *)
      pipe_descr_out = descr_child_sends;
      pipe_fout = child_sends_result;
    } = pipe_parent_reads_child_sends in
    let {
      (* Child reads *)
      pipe_descr_in = descr_child_reads;
      pipe_fin = child_reads_task;

      (* Parent sends *)
      pipe_descr_out = descr_parent_sends;
      pipe_fout = parent_sends_task;
    } = pipe_parent_sends_child_reads in
    match Fork.fork_and_log ~reason:"worker" () with
    | -1 ->
        failwith "Could not create process"
    | 0 ->
        (* CHILD *)
        (* Unix duplicates file descriptors during a fork, we make sure
         * we close all the ones we don't need anymore.
         *)
        hh_worker_init();
        Gc.set gc_control;
        close_parent descr_parent_reads descr_parent_sends acc;
        if !Utils.profile
        then begin
          let f = open_out (string_of_int (Unix.getpid ())^".log") in
          Utils.log := (fun s -> Printf.fprintf f "%s\n" s)
        end;
        (* And now start the daemon worker *)
        start_worker descr_child_reads child_reads_task child_sends_result
    | pid ->
        (* PARENT *)
        close_child descr_child_sends descr_child_reads;
        let worker = {
          pid = pid;
          send_task = parent_sends_task;
          recv_result = parent_reads_result;
          descr_recv = descr_parent_reads;
          descr_send = descr_parent_sends;
        } in
        let acc = worker :: acc in
        make acc (n-1) gc_control

  and close_parent: Unix.file_descr -> Unix.file_descr -> ('a, 'b) acc
    -> unit =
  fun descr_parent_reads descr_parent_sends acc ->
    close_in stdin;
    Unix.close descr_parent_reads;
    Unix.close descr_parent_sends;
    (* Disconnect from the previously created workers
     * This is a bit subtle. When we fork, the parent process has
     * a file descriptor open on the pipe for the worker.
     * When we fork for the next worker, all the previous pipes
     * are duplicated. To avoid this, we need to close the pipes
     * of the previously created workers.
     *)
    List.iter (fun w -> Unix.close w.descr_recv; Unix.close w.descr_send) acc;
    ()

  and start_worker: Unix.file_descr -> (unit -> 'a) -> ('b -> unit) -> 'c =
  fun descr_in child_reads_task child_sends_result ->
    (* Daemon *)
    try
      while true do
        (* This is a trick to use less memory and to be faster.
         * If we fork now, the heap is very small, because no job
         * was sent in yet.
         *)
        let readyl, _, _ = Unix.select [descr_in] [] [] (-1.0) in
        if readyl = [] then exit 0;
        match Fork.fork() with
        | 0 ->
            (try
              let { job = job; arg = arg } = child_reads_task() in
              let result = job arg in
              child_sends_result result;
              (* This is the interesting part. Since we die here,
               * all the memory allocated during the job is reclaimed
               * by the system. This makes memory consumption much much
               * lower.
               *)
              exit 0
            with
            | End_of_file ->
                exit 1
            | e ->
                let e_str = Printexc.to_string e in
                Printf.printf "Exception: %s\n" e_str;
                EventLogger.worker_exception e_str;
                print_endline "Potential backtrace:";
                Printexc.print_backtrace stdout;
                exit 2
            )
        | pid ->
            (match snd (Unix.waitpid [] pid) with
            | Unix.WEXITED 0 -> ()
            | Unix.WEXITED 1 ->
                raise End_of_file
            | Unix.WEXITED x ->
                Printf.printf "Worker exited (code: %d)\n" x;
                flush stdout;
                raise End_of_file
            | Unix.WSIGNALED x ->
                let sig_str = PrintSignal.string_of_signal x in
                Printf.printf "Worker interrupted with signal: %s\n" sig_str;
                exit 2
            | Unix.WSTOPPED x ->
                Printf.printf "Worker stopped with signal: %d\n" x;
                exit 3
            )
      done;
      assert false
    with End_of_file ->
      exit 0

  and close_child: Unix.file_descr -> Unix.file_descr -> unit =
  fun descr_child_sends descr_child_reads ->
    Unix.close descr_child_sends;
    Unix.close descr_child_reads;
    ()

end

(*****************************************************************************)
(* As explained in the header, we wrap every function with Obj.magic, because
 * the type-checker does not allow us to have a polymorphic worker.
 *)
(*****************************************************************************)
let get_pid proc = (Obj.magic proc).pid
let call proc f x = proc.send_task ({ job = f; arg = x })
let get_result proc _ = proc.recv_result()
let make heap gc_control = Obj.magic (MakeWorker.make [] heap gc_control)
let call proc = Obj.magic (call (Obj.magic proc))
let select procl = Obj.magic (select (Obj.magic procl))
let get_result proc = get_result (Obj.magic proc)
let get_file_descr proc = (Obj.magic proc).descr_recv
let kill proc = try Unix.kill (Obj.magic proc).pid 9 with _ -> ()
