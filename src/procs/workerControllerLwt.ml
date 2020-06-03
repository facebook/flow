(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open WorkerController

(* This is basically an lwt thread that writes a job to the worker, waits for the response, and
 * then returns the result.
 *
 * The main complication is that I, glevi, found a perf regression when I used Marshal_tools_lwt
 * to send the job to the worker. Here's my hypothesis:
 *
 * 1. On a machine with many CPUs (like 56) we create 56 threads to send a job to each worker.
 * 2. Lwt attempts to write the jobs to the workers in parallel.
 * 3. Each worker spends more time between getting the first byte and last byte
 * 4. Something something this leads to more context switches for the worker
 * 5. The worker spends more time on a job
 *
 * This is reinforced by the observation that the regression only happens as the number of workers
 * grows.
 *
 * By switching from Marshal_tools_lwt.to_fd_with_preamble to Marshal_tools.to_fd_with_preamble,
 * the issue seems to have disappeared. Reading from the worker didn't seem to trigger a perf issue
 * in my testing, but there's really nothing more urgent than reading a response from a finished
 * worker, so reading in a blocking manner is fine.
 *)
let call w (type a b) (f : a -> b) (x : a) : b Lwt.t =
  if is_killed w then Printf.ksprintf failwith "killed worker (%d)" (worker_id w);
  mark_busy w;

  (* Spawn the worker, if not prespawned. *)
  let ({ Daemon.pid = worker_pid; channels = (inc, outc) } as h) = spawn w in
  let infd = Daemon.descr_of_in_channel inc in
  let outfd = Daemon.descr_of_out_channel outc in
  let infd_lwt = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true infd in
  let outfd_lwt = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true outfd in
  let request = wrap_request w f x in
  (* Send the job *)
  (let%lwt () =
     try%lwt
       (* Wait in an lwt-friendly manner for the worker to be writable (should be instant) *)
       let%lwt () = Lwt_unix.wait_write outfd_lwt in
       (* Write in a lwt-unfriendly, blocking manner to the worker *)
       let _ = Marshal_tools.to_fd_with_preamble ~flags:[Marshal.Closures] outfd request in
       Lwt.return_unit
     with exn ->
       let stack = Printexc.get_backtrace () in
       Hh_logger.error "Failed to read response from work #%d\n%s" (worker_id w) stack;

       (* Failed to send the job to the worker. Is it because the worker is dead or is it
        * something else? *)
       let%lwt (pid, status) = Lwt_unix.waitpid [Unix.WNOHANG] worker_pid in
       (match pid with
       | 0 -> raise (Worker_failed_to_send_job (Other_send_job_failure exn))
       | _ -> raise (Worker_failed_to_send_job (Worker_already_exited status)))
   in
   (* Get the job's result *)
   let%lwt res =
     try%lwt
       (* Wait in an lwt-friendly manner for the worker to finish the job *)
       let%lwt () = Lwt_unix.wait_read infd_lwt in
       (* Read in a lwt-unfriendly, blocking manner from the worker *)
       (* Due to https://github.com/ocsigen/lwt/issues/564, annotation cannot go on let%let node *)
       let data : b = Marshal_tools.from_fd_with_preamble infd in
       let stats : Measure.record_data = Marshal_tools.from_fd_with_preamble infd in
       Lwt.return (data, stats)
     with
     | Lwt.Canceled as exn ->
       (* Worker is handling a job but we're cancelling *)

       (* Each worker might call this but that's ok *)
       WorkerCancel.stop_workers ();

       (* Wait for the worker to finish cancelling *)
       let%lwt () = Lwt_unix.wait_read infd_lwt in
       (* Read the junk from the pipe *)
       let _ = Marshal_tools.from_fd_with_preamble infd in
       let _ = Marshal_tools.from_fd_with_preamble infd in
       raise exn
     | exn ->
       let%lwt (pid, status) = Lwt_unix.waitpid [Unix.WNOHANG] worker_pid in
       begin
         match (pid, status) with
         | (0, _)
         | (_, Unix.WEXITED 0) ->
           (* The worker is still running or exited normally. It's odd that we failed to read
            * the response, so just raise that exception *)
           raise exn
         | (_, Unix.WEXITED i) when i = Exit_status.(exit_code Out_of_shared_memory) ->
           raise SharedMem.Out_of_shared_memory
         | (_, Unix.WEXITED i) ->
           let () = Printf.eprintf "Subprocess(%d): fail %d" worker_pid i in
           raise (Worker_failed (worker_pid, Worker_quit (Unix.WEXITED i)))
         | (_, Unix.WSTOPPED i) ->
           let () = Printf.eprintf "Subprocess(%d): stopped %d" worker_pid i in
           raise (Worker_failed (worker_pid, Worker_quit (Unix.WSTOPPED i)))
         | (_, Unix.WSIGNALED i) ->
           let () = Printf.eprintf "Subprocess(%d): signaled %d" worker_pid i in
           raise (Worker_failed (worker_pid, Worker_quit (Unix.WSIGNALED i)))
       end
   in
   close w h;
   Measure.merge (Measure.deserialize (snd res));
   Lwt.return (fst res))
    [%lwt.finally
      (* No matter what, always mark worker as free when we're done *)
      mark_free w;
      Lwt.return_unit]
