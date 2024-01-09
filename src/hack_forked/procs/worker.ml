(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************
 * The job executed by the worker.
 *
 * The 'serializer' is the job continuation: it is a function that must
 * be called at the end of the request ir order to send back the result
 * to the worker process (this is "internal business", this is not visible
 * outside this module). The clone process will provide the expected
 * function. cf 'send_result' in 'worker_job_main'.
 *
 *****************************************************************************)

type request = Request of (serializer -> unit)

and serializer = { send: 'a. 'a -> unit }

type worker_mode =
  | Prespawned_long_lived
  | Prespawned_should_fork

type job_status = Job_terminated of Unix.process_status

exception Connection_closed

(* On Unix each job runs in a forked clone process. The first thing these clones
 * do is deserialize a marshaled closure which is the job.
 *
 * The marshaled representation of a closure includes a MD5 digest of the code
 * segment and an offset. The digest is lazily computed, but if it has not been
 * computed before the fork, then each forked process will need to compute it.
 *
 * To avoid this, we deserialize a dummy closure before forking, so that we only
 * need to calculate the digest once per worker instead of once per job. *)
let dummy_closure () = ()

(*****************************************************************************
 * Entry point for spawned worker.
 *
 *****************************************************************************)

let worker_job_main infd outfd =
  let start_user_time = ref 0. in
  let start_system_time = ref 0. in
  let start_minor_words = ref 0. in
  let start_promoted_words = ref 0. in
  let start_major_words = ref 0. in
  let start_minor_collections = ref 0 in
  let start_major_collections = ref 0 in
  let start_compactions = ref 0 in
  let start_wall_time = ref 0. in
  let result_sent = ref false in
  let send_result data =
    (* If we got this far, the job is complete and we should send the results
     * back to the server, even if we try to cancel at this point. If a cancel
     * request is handled during this block, then we will get a cancel exception
     * when we return. We set the `result_sent` flag to avoid sending the dummy
     * result in the exception handler.
     *
     * (In practice, none of the code below should observe a cancel request) *)
    WorkerCancel.with_no_cancellations @@ fun () ->
    let tm = Unix.times () in
    let end_user_time = tm.Unix.tms_utime +. tm.Unix.tms_cutime in
    let end_system_time = tm.Unix.tms_stime +. tm.Unix.tms_cstime in
    let {
      Gc.minor_words = end_minor_words;
      promoted_words = end_promoted_words;
      major_words = end_major_words;
      minor_collections = end_minor_collections;
      major_collections = end_major_collections;
      compactions = end_compactions;
      _;
    } =
      Gc.quick_stat ()
    in
    let (major_time, minor_time) = Sys_utils.get_gc_time () in
    Measure.sample "worker_gc_major_wall_time" major_time;
    Measure.sample "worker_gc_minor_wall_time" minor_time;

    Measure.sample "worker_user_time" (end_user_time -. !start_user_time);
    Measure.sample "worker_system_time" (end_system_time -. !start_system_time);
    Measure.sample "worker_wall_time" (Unix.gettimeofday () -. !start_wall_time);

    Measure.track_distribution "minor_words" ~bucket_size:(float (100 * 1024 * 1024));
    Measure.sample "minor_words" (end_minor_words -. !start_minor_words);

    Measure.track_distribution "promoted_words" ~bucket_size:(float (25 * 1024 * 1024));
    Measure.sample "promoted_words" (end_promoted_words -. !start_promoted_words);

    Measure.track_distribution "major_words" ~bucket_size:(float (50 * 1024 * 1024));
    Measure.sample "major_words" (end_major_words -. !start_major_words);

    Measure.sample "minor_collections" (float (end_minor_collections - !start_minor_collections));
    Measure.sample "major_collections" (float (end_major_collections - !start_major_collections));
    Measure.sample "compactions" (float (end_compactions - !start_compactions));

    let len =
      Measure.time "worker_send_response" (fun () ->
          try Marshal_tools.to_fd_with_preamble ~flags:[Marshal.Closures] outfd (Some data) with
          | Unix.Unix_error (Unix.EPIPE, _, _) -> raise Connection_closed
      )
    in
    if len > 30 * 1024 * 1024 (* 30 MB *) then (
      Hh_logger.log
        "WARNING: you are sending quite a lot of data (%d bytes), which may have an adverse performance impact. If you are sending closures, double-check to ensure that they have not captured large
        values in their environment."
        len;
      Printf.eprintf "%s" (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100))
    );

    Measure.sample "worker_response_len" (float len);

    let stats = Measure.serialize (Measure.pop_global ()) in
    let _ =
      try Marshal_tools.to_fd_with_preamble outfd stats with
      | Unix.Unix_error (Unix.EPIPE, _, _) -> raise Connection_closed
    in
    result_sent := true
  in
  try
    Measure.push_global ();
    let (Request do_process) =
      Measure.time "worker_read_request" (fun () ->
          try Marshal_tools.from_fd_with_preamble infd with
          | End_of_file -> raise Connection_closed
      )
    in
    let tm = Unix.times () in
    let gc = Gc.quick_stat () in
    Sys_utils.start_gc_profiling ();

    start_user_time := tm.Unix.tms_utime +. tm.Unix.tms_cutime;
    start_system_time := tm.Unix.tms_stime +. tm.Unix.tms_cstime;
    start_minor_words := gc.Gc.minor_words;
    start_promoted_words := gc.Gc.promoted_words;
    start_major_words := gc.Gc.major_words;
    start_minor_collections := gc.Gc.minor_collections;
    start_major_collections := gc.Gc.major_collections;
    start_compactions := gc.Gc.compactions;
    start_wall_time := Unix.gettimeofday ();
    try do_process { send = send_result } with
    | WorkerCancel.Worker_should_cancel ->
      (* Send `None` to reflect canceled status. *)
      if not !result_sent then (
        try ignore (Marshal_tools.to_fd_with_preamble outfd None) with
        | Unix.Unix_error (Unix.EPIPE, _, _) -> raise Connection_closed
      )
  with
  | Connection_closed -> exit 1
  | SharedMem.Out_of_shared_memory -> Exit.(exit Out_of_shared_memory)
  | SharedMem.Hash_table_full -> Exit.(exit Hash_table_full)
  | SharedMem.Heap_full -> Exit.(exit Heap_full)
  | e ->
    let exn = Exception.wrap e in
    let e_str =
      Printf.sprintf
        "%s\nBacktrace: %s"
        (Exception.get_ctor_string exn)
        (Exception.get_full_backtrace_string max_int exn)
    in
    let pid = Sys_utils.get_pretty_pid () in
    if FlowEventLogger.should_log () then FlowEventLogger.worker_exception e_str;
    Printf.printf "Worker %d exception: %s\n%!" pid e_str;
    exit 2

let worker_loop handler infd outfd =
  try
    while true do
      (* Wait for an incoming job : is there something to read?
         But we don't read it yet. It will be read by the handler. *)
      let (readyl, _, _) = Sys_utils.select_non_intr [infd] [] [] (-1.0) in
      if readyl = [] then raise End_of_file;
      handler infd outfd
    done
  with
  | End_of_file -> ()

(* The fork handler creates forks for each job and reaps them with waitpid.
 * The forked process runs the actual job and sends the results over outfd.
 *
 * If the forked process exits with a non-zero code, the worker also exits with
 * the same code. Thus, the owning process of this Worker can just waitpid
 * directly on this process and see correct exit codes.
 *
 * Except `WSIGNALED i` and `WSTOPPED i` are all compressed to `exit 2` and
 * `exit 3` respectively. Thus some resolution is lost. So if the underling
 * forked process is for example SIGKILL'd by the OOM killer, then the owning
 * process won't be aware of it.
 *)
let fork_handler infd outfd =
  match Fork.fork () with
  | 0 ->
    worker_job_main infd outfd;
    exit 0
  | pid ->
    (* Wait for the clone to terminate... *)
    let status = snd (Sys_utils.waitpid_non_intr [] pid) in
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED 1 -> raise End_of_file
    | Unix.WEXITED code ->
      Printf.printf "Worker exited (code: %d)\n" code;
      flush stdout;
      Stdlib.exit code
    | Unix.WSIGNALED x ->
      let sig_str = PrintSignal.string_of_signal x in
      Printf.printf "Worker interrupted with signal: %s\n" sig_str;
      exit 2
    | Unix.WSTOPPED x ->
      Printf.printf "Worker stopped with signal: %d\n" x;
      exit 3)

let worker_main restore state (ic, oc) =
  let infd = Daemon.descr_of_in_channel ic in
  let outfd = Daemon.descr_of_out_channel oc in
  (match restore state with
  | Prespawned_should_fork ->
    (* see dummy_closure above *)
    ignore Marshal.(from_bytes (to_bytes dummy_closure [Closures]) 0);
    worker_loop fork_handler infd outfd
  | Prespawned_long_lived -> worker_loop worker_job_main infd outfd);
  exit 0
