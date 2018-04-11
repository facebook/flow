(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

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

(* If we cancel before sending results, master will be left blocking waiting
* for results. We need to send something back *)
let on_slave_cancelled parent_outfd =
  (* The cancelling master will ignore result of cancelled job anyway (see
   * wait_for_cancel function), so we can send back anything. *)
  Marshal_tools.to_fd_with_preamble parent_outfd "anything"
  |> ignore

(*****************************************************************************
 * Entry point for spawned worker.
 *
 *****************************************************************************)

let slave_main ic oc =
  let start_user_time = ref 0. in
  let start_system_time = ref 0. in
  let start_minor_words = ref 0. in
  let start_promoted_words = ref 0. in
  let start_major_words = ref 0. in
  let start_minor_collections = ref 0 in
  let start_major_collections = ref 0 in

  let infd = Daemon.descr_of_in_channel ic in
  let outfd = Daemon.descr_of_out_channel oc in

  let send_result data =
    let tm = Unix.times () in
    let end_user_time = tm.Unix.tms_utime +. tm.Unix.tms_cutime in
    let end_system_time = tm.Unix.tms_stime +. tm.Unix.tms_cstime in
    let { Gc.
      minor_words = end_minor_words;
      promoted_words = end_promoted_words;
      major_words = end_major_words;
      minor_collections = end_minor_collections;
      major_collections = end_major_collections;
      _;
    } = Gc.quick_stat () in
    Measure.sample "worker_user_time" (end_user_time -. !start_user_time);
    Measure.sample "worker_system_time" (end_system_time -. !start_system_time);
    Measure.sample "minor_words" (end_minor_words -. !start_minor_words);
    Measure.sample "promoted_words" (end_promoted_words -. !start_promoted_words);
    Measure.sample "major_words" (end_major_words -. !start_major_words);
    Measure.sample "minor_collections" (float (end_minor_collections - !start_minor_collections));
    Measure.sample "major_collections" (float (end_major_collections - !start_major_collections));
    let stats = Measure.serialize (Measure.pop_global ()) in
    (* If we got so far, just let it finish "naturally" *)
    SharedMem.set_on_worker_cancelled (fun () -> ());
    let len = Marshal_tools.to_fd_with_preamble ~flags:[Marshal.Closures] outfd (data,stats) in
    if len > 30 * 1024 * 1024 (* 30 MB *) then begin
      Hh_logger.log "WARNING: you are sending quite a lot of data (%d bytes), \
        which may have an adverse performance impact. If you are sending \
        closures, double-check to ensure that they have not captured large
        values in their environment." len;
      Printf.eprintf "%s" (Printexc.raw_backtrace_to_string
        (Printexc.get_callstack 100));
    end
  in

  try
    Measure.push_global ();
    let Request do_process = Marshal_tools.from_fd_with_preamble infd in
    SharedMem.set_on_worker_cancelled (fun () -> on_slave_cancelled outfd);
    let tm = Unix.times () in
    let gc = Gc.quick_stat () in
    start_user_time := tm.Unix.tms_utime +. tm.Unix.tms_cutime;
    start_system_time := tm.Unix.tms_stime +. tm.Unix.tms_cstime;
    start_minor_words := gc.Gc.minor_words;
    start_promoted_words := gc.Gc.promoted_words;
    start_major_words := gc.Gc.major_words;
    start_minor_collections := gc.Gc.minor_collections;
    start_major_collections := gc.Gc.major_collections;
    do_process { send = send_result };
    exit 0
  with
  | End_of_file ->
      exit 1
  | SharedMem.Out_of_shared_memory ->
      Exit_status.(exit Out_of_shared_memory)
  | SharedMem.Hash_table_full ->
      Exit_status.(exit Hash_table_full)
  | SharedMem.Heap_full ->
      Exit_status.(exit Heap_full)
  | SharedMem.Sql_assertion_failure err_num ->
      let exit_code = match err_num with
        | 11 -> Exit_status.Sql_corrupt
        | 14 -> Exit_status.Sql_cantopen
        | 21 -> Exit_status.Sql_misuse
        | _ -> Exit_status.Sql_assertion_failure
      in
      Exit_status.exit exit_code
  | e ->
      let e_str = Printexc.to_string e in
      Printf.printf "Exception: %s\n" e_str;
      EventLogger.log_if_initialized (fun () ->
        EventLogger.worker_exception e_str
      );
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
          match snd (Sys_utils.waitpid_non_intr [] pid) with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED 1 ->
              raise End_of_file
          | Unix.WEXITED code ->
              Printf.printf "Worker exited (code: %d)\n" code;
              flush stdout;
              Pervasives.exit code
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
