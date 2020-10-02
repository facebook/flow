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
    Lwt_main.run
      (MultiWorkerLwt.call
         (Some workers)
         ~job:sum
         ~merge:( + )
         ~neutral:0
         ~next:(Bucket.make ~num_workers:20 work))
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
  let result =
    Lwt_main.run (MultiWorkerLwt.call (Some workers) ~job:( + ) ~merge:( + ) ~neutral:0 ~next)
  in
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
      Lwt_main.run
        (MultiWorkerLwt.call
           (Some workers)
           ~job:do_work
           ~merge:( + )
           ~neutral:0
           ~next:(make_n_buckets ~buckets ~split))
    in
    Printf.printf "Got %d\n" result;
    result = expected)

let tests =
  [
    ("multi_worker_list", multi_worker_list);
    ("multi_worker_bucket", multi_worker_bucket);
    ("multi_worker_of_n_buckets", multi_worker_of_n_buckets);
  ]

let () =
  Daemon.check_entry_point ();

  (* this call might not return *)
  let workers = make_workers 10 in
  try_finalize Unit_test.run_all (List.map (fun (n, t) -> (n, t workers)) tests) cleanup ()
