(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Process

(* check that B is not shadowed when we set A.
 *
 * set B=1 outside and then run:
 *
 * case "$B" in
 *   1) exit 0;;
 *   * ) exit 1;;
 * esac
 *
 * except with * and ) next to each other
 *
 * *)
let test_proc_env () =
  let () = Unix.putenv "B" "1" in
  let proc_t =
    exec
      "bash"
      ~env:(Process_types.Augment ["A=1"])
      ["-c"; "case \"$B\" in 1) exit 0;; *) exit 1;; esac"]
  in
  let proc_stat_ref = proc_t.Process_types.lifecycle in
  let proc_stat = !proc_stat_ref in
  match proc_stat with
  | Process_types.Lifecycle_killed_due_to_overflow_stdin -> failwith "process aborted"
  | Process_types.Lifecycle_running { pid } ->
    (match Unix.waitpid [] pid with
    | (caught_pid, Unix.WEXITED status) ->
      (match (caught_pid = pid, status) with
      | (false, _) -> failwith "impossible: caught wrong child process"
      | (true, 0) -> true (* success! *)
      | (true, _) -> failwith "child exited abnormally")
    | _ -> failwith "child was killed by signal or something")
  | _ -> failwith "process failed for a different reason"

let tests = [("test_proc_env", test_proc_env)]

let () = Unit_test.run_all tests
