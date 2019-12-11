(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Procs_test_utils

(* create workers and synchronize this global reference from master *)
let pipe_path = ref ""

let entry =
  WorkerController.register_entry_point ~restore:(fun s ~(worker_id : int) ->
      pipe_path := s;
      Hh_logger.set_id (Printf.sprintf "test_cancel_env %d" worker_id))

let make_workers n =
  let handle =
    let default_sharedmem_config =
      let gig = 1024 * 1024 * 1024 in
      (* Where to write temp files *)
      let tmp_dir =
        try Sys.getenv "HH_TMPDIR"
        with _ -> Path.to_string @@ Path.concat (Path.make Sys_utils.temp_dir_name) "hh_server"
      in

      let shm_dir = (try Sys.getenv "HH_SHMDIR" with _ -> "/dev/shm") in
      {
        SharedMem.global_size = gig;
        heap_size = 20 * gig;
        dep_table_pow = 17;
        (* 1 << 17 *)
        hash_table_pow = 18;
        (* 1 << 18 *)
        shm_dirs = [shm_dir; tmp_dir];
        shm_min_avail = gig / 2;
        (* Half a gig by default *)
        log_level = 0;
        sample_rate = 0.0;
      }
    in
    SharedMem.init ~num_workers:n default_sharedmem_config
  in
  let workers = MultiWorker.make !pipe_path entry n (Core_kernel.Gc.get ()) handle in
  workers

module UnitVal = struct
  type t = unit

  let prefix = Prefix.make ()

  let description = "UnitVal"
end

module TestHeap = SharedMem.NoCache (SharedMem.Immediate) (StringKey) (UnitVal)

(* The tasks will be numbers 1...num_workers_and_jobs,
 * and the job will be to sum them. Each worker will get one number at a time.
 *)
let num_workers_and_jobs = 10

let max_job_size = 1

let sum acc x = acc + x

let loop_until_cancel () =
  while true do
    (* interruptions only happen during shared-memory IO *)
    let (_ : bool) = TestHeap.mem "test" in
    Unix.sleep 1
  done

let do_work (acc : int) (jobs : int list) : int =
  let job =
    match jobs with
    | [job] -> job
    | _ -> assert false
    (* impossible due to max_job_size = 1 *)
  in
  (* Worker number 2 will go to sleep for long
   * enough that it will be definitely cancelled. *)
  if job = 2 then loop_until_cancel ();

  (* Workers number 3 and 7 will sleep long enough to let all the other ones
     (besides 2) to finish, and then ping the interrupt handler. Interrupt
     handler will will count the number of interruptions and cancel entire job
     after second ping (so all the workers beside 2 and 7 should finish) *)
  if job = 3 || job = 7 then (
    Unix.sleep
      ( if job = 3 then
        2
      else
        4 );
    let fd = Unix.openfile !pipe_path [Unix.O_WRONLY] 0o640 in
    let written = Unix.write fd "!" 0 1 in
    assert (written = 1);
    if job = 7 then loop_until_cancel ()
  );
  sum acc job

let rec make_work acc = function
  | 0 -> acc
  | x -> make_work (x :: acc) (x - 1)

let make_work () = make_work [] num_workers_and_jobs

(* read the pings, count them in env, and cancel after second one *)
let interrupt_handler fd env =
  let exclamation_mark = Bytes.create 1 in
  let read = Unix.read fd exclamation_mark 0 1 in
  assert (read = 1 && exclamation_mark = "!");
  let env = env + 1 in
  let result =
    MultiThreadedCall.(
      if env = 2 then
        Cancel
      else
        Continue)
  in
  (env, result)

let test_cancel_env () =
  Tempfile.with_tempdir @@ fun tempdir ->
  (* must set this before spawning workers *)
  (pipe_path := Path.(concat tempdir "mypipe" |> to_string));
  let workers = make_workers num_workers_and_jobs in
  let work = make_work () in
  Unix.mkfifo !pipe_path 0o777;

  (* Opening as read-only either blocks, or spuriously runs interrupt handler
   * when workers open it for the first time. *)
  let interrupt_fd = Unix.openfile !pipe_path [Unix.O_RDWR] 0o640 in
  let next = Bucket.make ~num_workers:num_workers_and_jobs ~max_size:max_job_size work in
  let (res, interrupt_env, cancelled) =
    MultiWorker.call_with_interrupt
      (Some workers)
      ~job:do_work
      ~merge:sum
      ~neutral:0
      ~next
      ~interrupt:
        {
          MultiThreadedCall.handlers = (fun _ -> [(interrupt_fd, interrupt_handler interrupt_fd)]);
          env = 0 (* counting number of times interrupt handler ran *);
        }
  in
  let total = num_workers_and_jobs * (num_workers_and_jobs + 1) / 2 in
  assert (interrupt_env = 2);
  assert (res = total - 7 - 2);
  assert (cancelled = [[7]; [2]] || cancelled = [[2]; [7]]);
  true

let () =
  Daemon.check_entry_point ();
  try_finalize Unit_test.run_all [("cancel_env", test_cancel_env)] cleanup ()
