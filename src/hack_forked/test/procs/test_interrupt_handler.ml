(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Procs_test_utils

let do_work () (_ : unit list) : unit = Unix.sleep 3

let handler1 fd (x, y) =
  let () = read_exclamation_mark fd in
  ((x + 1, y), MultiThreadedCall.Continue)

let handler2 fd (x, y) =
  let () = read_exclamation_mark fd in
  ((x, y + 1), MultiThreadedCall.Continue)

let configure_handlers fd1 fd2 (x, y) =
  let handlers = [] in
  let handlers =
    if x < 3 then
      (fd1, handler1 fd1) :: handlers
    else
      handlers
  in
  let handlers =
    if y < 4 then
      (fd2, handler2 fd2) :: handlers
    else
      handlers
  in
  handlers

let test_interrupt_handler () =
  let next = Bucket.make ~num_workers:1 ~max_size:1 [()] in
  let workers = Some (make_workers 1) in
  let merge () _ = () in
  let (interrupt_fd1, interrupter_pid1) = run_interrupter (Some 10) in
  let (interrupt_fd2, interrupter_pid2) = run_interrupter (Some 10) in
  let ((), (x, y), cancelled) =
    MultiWorker.call_with_interrupt
      workers
      ~job:do_work
      ~merge
      ~neutral:()
      ~next
      ~interrupt:
        {
          MultiThreadedCall.handlers = configure_handlers interrupt_fd1 interrupt_fd2;
          env = (0, 0) (* counting number of times interrupt handlers ran *);
        }
  in
  let (_ : int * Unix.process_status) = Unix.waitpid [] interrupter_pid1 in
  let (_ : int * Unix.process_status) = Unix.waitpid [] interrupter_pid2 in
  assert (x = 3);
  assert (y = 4);
  assert (cancelled = []);
  true

let () =
  Daemon.check_entry_point ();
  try_finalize Unit_test.run_all [("test_interrupt_handler", test_interrupt_handler)] cleanup ()
