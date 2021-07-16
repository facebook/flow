(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
 * function. cf 'send_result' in 'worker_main'.
 *
 *****************************************************************************)

type request = Request of (serializer -> unit)

and serializer = { send: 'a. 'a -> unit }

type job_status = Job_terminated of Unix.process_status

exception Connection_closed

let on_job_cancelled parent_outfd =
  (* The cancelling controller will ignore result of cancelled job anyway (see
   * wait_for_cancel function), so we can send back anything. Write twice, since
   * the normal response writes twice too *)
  Marshal_tools.to_fd_with_preamble parent_outfd "anything" |> ignore;
  Marshal_tools.to_fd_with_preamble parent_outfd "anything" |> ignore

(*****************************************************************************
 * Entry point for spawned worker.
 *
 *****************************************************************************)

let worker_main ic oc =
  let start_user_time = ref 0. in
  let start_system_time = ref 0. in
  let start_minor_words = ref 0. in
  let start_promoted_words = ref 0. in
  let start_major_words = ref 0. in
  let start_minor_collections = ref 0 in
  let start_major_collections = ref 0 in
  let start_compactions = ref 0 in
  let start_wall_time = ref 0. in
  let start_proc_fs_status = ref None in
  let infd = Daemon.descr_of_in_channel ic in
  let outfd = Daemon.descr_of_out_channel oc in
  let send_result data =
    Mem_profile.stop ();
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

    begin
      match (!start_proc_fs_status, ProcFS.status_for_pid (Unix.getpid ())) with
      | (Some { ProcFS.rss_total = start; _ }, Ok { ProcFS.rss_total = total; rss_hwm = hwm; _ }) ->
        Measure.sample "worker_rss_start" (float start);
        Measure.sample "worker_rss_delta" (float (total - start));
        Measure.sample "worker_rss_hwm_delta" (float (hwm - start))
      | _ -> ()
    end;

    (* If we got so far, just let it finish "naturally" *)
    WorkerCancel.set_on_worker_cancelled (fun () -> ());
    let len =
      Measure.time "worker_send_response" (fun () ->
          try Marshal_tools.to_fd_with_preamble ~flags:[Marshal.Closures] outfd data with
          | Unix.Unix_error (Unix.EPIPE, _, _) -> raise Connection_closed)
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
    ()
  in
  try
    try
      Measure.push_global ();
      let (Request do_process) =
        Measure.time "worker_read_request" (fun () ->
            try Marshal_tools.from_fd_with_preamble infd with
            | End_of_file -> raise Connection_closed)
      in
      WorkerCancel.set_on_worker_cancelled (fun () -> on_job_cancelled outfd);
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
      start_proc_fs_status := ProcFS.status_for_pid (Unix.getpid ()) |> Base.Result.ok;
      Mem_profile.start ();
      do_process { send = send_result };
      exit 0
    with
    | Connection_closed -> exit 1
    | SharedMem.Out_of_shared_memory -> Exit.(exit Out_of_shared_memory)
    | SharedMem.Hash_table_full -> Exit.(exit Hash_table_full)
    | SharedMem.Heap_full -> Exit.(exit Heap_full)
  with
  | e ->
    let exn = Exception.wrap e in
    let e_str =
      Printf.sprintf
        "%s\nBacktrace: %s"
        (Exception.get_ctor_string exn)
        (Exception.get_full_backtrace_string max_int exn)
    in
    let pid = Unix.getpid () in
    if EventLogger.should_log () then EventLogger.worker_exception e_str;
    Printf.printf "Worker %d exception: %s\n%!" pid e_str;
    exit 2

let win32_worker_main restore (state, _controller_fd) (ic, oc) =
  restore state;
  worker_main ic oc

let maybe_send_status_to_controller fd status =
  match fd with
  | None -> ()
  | Some fd ->
    let to_controller fd msg = ignore (Marshal_tools.to_fd_with_preamble fd msg : int) in
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED 1 ->
      (* 1 is an expected exit code. On unix systems, when the controller
       * process exits, the pipe becomes readable. We fork a new clone, which
       * reads 0 bytes and exits with code 1. In this case, the controller is
       * dead so trying to write a message to the controller willcause an
       * exception *)
      ()
    | _ ->
      Timeout.with_timeout
        ~timeout:10
        ~on_timeout:(fun _ -> Hh_logger.log "Timed out sending status to controller")
        ~do_:(fun _ -> to_controller fd (Job_terminated status)))

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

(**
 * On Windows, the Worker is a process and runs the job directly. See above.
 *
 * On Unix, the Worker forks Clone processes and reaps them with waitpid.
 * The Clone runs the actual job and sends the results over the oc.
 * If the Clone exits normally (exit code 0), the Worker keeps living and
 * waits for the next incoming job before forking a new Clone.
 *
 * If the Clone exits with a non-zero code, the Worker also exits with the
 * same code. Thus, the owning process of this Worker can just waitpid
 * directly on this process and see correct exit codes.
 *
 * Except `WSIGNALED i` and `WSTOPPED i` are all compressed to `exit 2`
 * and `exit 3` respectively. Thus some resolution is lost. So if
 * the underling Clone is for example SIGKILL'd by the OOM killer,
 * then the owning process won't be aware of it.
 *
 * To regain this lost resolution, controller_fd can be optionally set. The
 * real exit statuses (includinng WSIGNALED and WSTOPPED) will be sent over
 * this file descriptor to the Controller when the Clone exits
 * abnormally (non-zero exit code).
 *)
let unix_worker_main restore (state, controller_fd) (ic, oc) =
  restore state;

  (* see dummy_closure above *)
  ignore Marshal.(from_bytes (to_bytes dummy_closure [Closures]) 0);

  let in_fd = Daemon.descr_of_in_channel ic in
  try
    while true do
      (* Wait for an incoming job : is there something to read?
         But we don't read it yet. It will be read by the forked clone. *)
      let (readyl, _, _) = Unix.select [in_fd] [] [] (-1.0) in
      if readyl = [] then exit 0;

      (* We fork a clone for every incoming request.
         And let it die after one request. This is the quickest GC. *)
      match Fork.fork () with
      | 0 -> worker_main ic oc
      | pid ->
        (* Wait for the clone to terminate... *)
        let status = snd (Sys_utils.waitpid_non_intr [] pid) in
        let () = maybe_send_status_to_controller controller_fd status in
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
    done;
    assert false
  with
  | End_of_file -> exit 0
