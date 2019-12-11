(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Procs_test_utils

exception MultiWorkerException

(* A "regular" multi worker job where workers concatenate "a"-s together.
 * If ~fail_inner is true, the workers will die with an error. *)
let multi_worker_inner workers ~fail_inner =
  let counter = ref 0 in
  let next () =
    incr counter;
    if !counter < 20 then
      Bucket.Job "a"
    else
      Bucket.Done
  in
  let concat acc x =
    if fail_inner then exit 3;
    acc ^ x
  in
  let result = MultiWorker.call (Some workers) ~job:concat ~merge:( ^ ) ~neutral:"" ~next in
  Printf.printf "Got %s\n" result;
  assert (result = "aaaaaaaaaaaaaaaaaaa")

(* An interruptible job that will also run multi_worker_inner in the middle of
 * call_with_interrupt *)
let multi_worker_nested workers ?(fail_inner = false) () =
  (* Custom next() function - keeps producing jobs until number of produced
   * buckets (counter) is bigger than finish_at *)
  let counter = ref 0 in
  let finish_at = ref None in
  let is_finished () =
    match !finish_at with
    | Some x when x > !counter -> true
    | _ -> false
  in
  let next () =
    if is_finished () then
      Bucket.Done
    else
      Bucket.Job (incr counter)
  in
  (* interrupting process - wakes up every second until killed *)
  let (interrupt_fd1, interrupter_pid1) = run_interrupter None in
  let kill_interrupter () = Unix.kill interrupter_pid1 Sys.sigkill in
  (* After processing 100 buckets, perform a nested job, and then process 100
   * more buckets to see if the workers are healthy afterwards *)
  let interrupt_handler fd () =
    let () = read_exclamation_mark fd in
    if !counter > 100 then (
      (try multi_worker_inner workers ~fail_inner
       with _ ->
         (* Try to ignore the exception that will be thrown when fail_inner is
          * true - it should be re-raised for the main job anyway *)
         ());
      finish_at := Some (!counter + 100)
    );
    ((), MultiThreadedCall.Continue)
  in
  (* The work is just to count amount of buckets processed *)
  let do_work acc () = acc + 1 in
  let (result, (), _) =
    try
      MultiWorker.call_with_interrupt
        (Some workers)
        ~job:do_work
        ~merge:( + )
        ~neutral:0
        ~next
        ~interrupt:
          {
            MultiThreadedCall.handlers =
              (fun () -> [(interrupt_fd1, interrupt_handler interrupt_fd1)]);
            env = ();
          }
    with
    (* The mechanism to create Coalesced_failures is too flaky to count on it
     * in tests *)
    (* MultiThreadedCall.Coalesced_failures *)
    | _ ->
      kill_interrupter ();
      raise MultiWorkerException
  in
  kill_interrupter ();
  result = !counter

let multi_worker_nested_exn workers () =
  let did_throw = ref false in
  (try assert (multi_worker_nested workers ~fail_inner:true ())
   with MultiWorkerException -> did_throw := true);
  !did_throw

let multi_worker_nested = multi_worker_nested ~fail_inner:false

let tests =
  [
    ("multi_worker_nested", multi_worker_nested);
    ("multi_worker_nested_exn", multi_worker_nested_exn);
  ]

let () =
  Daemon.check_entry_point ();
  let workers = make_workers 10 in
  try_finalize Unit_test.run_all (List.map (fun (n, t) -> (n, t workers)) tests) cleanup ()
