(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Procs_test_utils

let sum acc elements = List.fold_left (fun acc elem -> acc + elem) acc elements

let multi_worker_list workers () =
  let work = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13] in
  let expected = List.length work * (List.length work + 1) / 2 in
  let result =
    MultiWorker.call
      (Some workers)
      ~job:sum
      ~merge:( + )
      ~neutral:0
      ~next:(Bucket.make ~num_workers:20 work)
  in
  Printf.printf "Got %d\n" result;
  result = expected

let multi_worker_bucket workers () =
  let buckets = 20 in
  let next =
    let counter = ref 1 in
    fun () ->
      let current = !counter in
      counter := !counter + 1;
      if current <= buckets then
        Bucket.Job current
      else
        Bucket.Done
  in
  let expected = buckets * (buckets + 1) / 2 in
  let result = MultiWorker.call (Some workers) ~job:( + ) ~merge:( + ) ~neutral:0 ~next in
  Printf.printf "Got %d\n" result;
  result = expected

let multi_worker_of_n_buckets workers () =
  let buckets = 20 in
  let split ~bucket = bucket + 1 in
  let expected = buckets * (buckets + 1) / 2 in
  Bucket.(
    let do_work _ bucket =
      assert (bucket.work = bucket.bucket + 1);
      bucket.work
    in
    let result =
      MultiWorker.call
        (Some workers)
        ~job:do_work
        ~merge:( + )
        ~neutral:0
        ~next:(make_n_buckets ~buckets ~split)
    in
    Printf.printf "Got %d\n" result;
    result = expected)

let multi_worker_one_worker_throws _workers () =
  (* When a worker fails, the rest in the same MultiWorker call can't
   * be reused right now. So let's use fresh workers for this unit test
   * instead of corrupted the shared workers. *)
  let workers = make_workers 10 in
  let split ~bucket = bucket + 1 in
  Bucket.(
    let do_work _ bucket =
      if bucket.work = 5 then
        (* Bucket number 5 exits abnormally. *)
        exit 3
      else
        bucket.work
    in
    MultiThreadedCall.(
      try
        let _result =
          MultiWorker.call
            (Some workers)
            ~job:do_work
            ~merge:( + )
            ~neutral:0
            ~next:(make_n_buckets ~buckets:20 ~split)
        in
        false
      with Coalesced_failures [WorkerController.Worker_quit (Unix.WEXITED 3)] -> true))

let multi_worker_with_failure_handler _workers () =
  (* Spawn new workers since some will be failing and the rest non-reusable. *)
  let workers = make_workers 10 in
  let split ~bucket =
    (*
     * We want the first 5 buckets to all have exited when the MultiWorker result
     * is merged. Yes, they fail quickly, but it's still racy. We don't want to
     * see "just 3" failures or something when merging.
     *
     * To ensure all 5 have failed, we delay merging.
     *
     * To do that, we delay handing out the last 5 buckets.
     *
     * To do that, we add a sleep when creating the 6th bucket.
     *
     * This works because the result isn't merged until all buckets are handed
     * out.
     *)
    if bucket = 5 then
      let () = Unix.sleep 10 in
      bucket + 1
    else
      bucket + 1
  in
  Bucket.(
    let do_work _ { Bucket.work; _ } =
      (* First 5 buckets exit abnormally. The rest sleep. *)
      if work <= 5 then
        exit 3
      else
        let () = Unix.sleep 60 in
        work
    in
    MultiThreadedCall.(
      try
        let _ =
          MultiWorker.call
            (Some workers)
            ~job:do_work
            ~merge:( + )
            ~neutral:0
            ~next:(make_n_buckets ~buckets:10 ~split)
        in
        Printf.eprintf "Expected MultiWorker.call to throw, but it didn't!\n";
        false
      with Coalesced_failures failures ->
        (* The first 5 buckets all failed; Last 5 buckets are still sleeping. *)
        Printf.eprintf "Got %d failed jobs. Expecting 5." (List.length failures);
        assert (List.length failures = 5);
        let sum =
          List.fold_left
            (fun acc e ->
              match e with
              | WorkerController.Worker_quit (Unix.WEXITED 3) -> acc + 3
              | _ -> failwith "Unexpected worker exit")
            0
            failures
        in
        (* Sum of all exit codes (each exit 3) is 15. *)
        sum = 15))

let tests =
  [
    ("multi_worker_list", multi_worker_list);
    ("multi_worker_bucket", multi_worker_bucket);
    ("multi_worker_of_n_buckets", multi_worker_of_n_buckets);
    ("multi_worker_one_worker_throws", multi_worker_one_worker_throws);
    ("multi_worker_with_failure_handler", multi_worker_with_failure_handler);
  ]

let () =
  Daemon.check_entry_point ();

  (* this call might not return *)
  let workers = make_workers 10 in
  try_finalize Unit_test.run_all (List.map (fun (n, t) -> (n, t workers)) tests) cleanup ()
