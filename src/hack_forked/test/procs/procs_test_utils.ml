(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel

let entry =
  WorkerController.register_entry_point ~restore:(fun _ ~(worker_id : int) ->
      Hh_logger.set_id (Printf.sprintf "procs_test_utils %d" worker_id))

let try_finalize f x finally y =
  let res =
    try f x
    with exn ->
      finally y;
      raise exn
  in
  finally y;
  res

let make_workers n =
  let default_sharedmem_config =
    let gig = 1024 * 1024 * 1024 in
    { SharedMem.heap_size = 20 * gig; hash_table_pow = 18; log_level = 0 }
  in
  let handle = SharedMem.init ~num_workers:n default_sharedmem_config in
  let workers = MultiWorkerLwt.make handle entry n (Gc.get ()) handle in
  workers

let cleanup () = WorkerController.killall ()

let run_interrupter limit =
  let (fd_in, fd_out) = Unix.pipe () in
  let interrupter_pid =
    match Unix.fork () with
    | 0 ->
      Unix.close fd_in;
      let rec aux x =
        match x with
        | Some 0 -> exit 0
        | _ ->
          let written = Unix.write fd_out "!" 0 1 in
          assert (written = 1);
          aux (Option.map x (fun x -> x - 1))
      in
      aux limit
    | pid -> pid
  in
  Unix.close fd_out;
  (fd_in, interrupter_pid)

let read_exclamation_mark fd =
  let exclamation_mark = Bytes.create 1 in
  let read = Unix.read fd exclamation_mark 0 1 in
  assert (read = 1 && exclamation_mark = "!")
