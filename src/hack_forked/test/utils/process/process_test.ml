(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Asserter

let test_echo () =
  let process = Process.exec "echo" ["hello world"] in
  match Process.read_and_wait_pid ~timeout:2 process with
  | Ok { Process_types.stdout; _ } ->
    let () = String_asserter.assert_equals "hello world\n" stdout "" in
    true
  | _ -> false

let test_echo_in_a_loop () =
  let rec loop acc = function
    | 0 -> acc
    | n ->
      let acc = acc && test_echo () in
      loop acc (n - 1)
  in
  (* There was a bug leaking 2 file descriptors per Process execution, and
   * running it over 500 times would run out of FDs *)
  loop true 600

let test_process_read_idempotent () =
  let process = Process.exec "echo" ["hello world"] in
  let result = Process.read_and_wait_pid ~timeout:2 process in
  let () =
    match result with
    | Ok { Process_types.stdout; _ } -> String_asserter.assert_equals "hello world\n" stdout ""
    | _ -> ()
  in
  let result = Process.read_and_wait_pid ~timeout:2 process in
  match result with
  | Ok { Process_types.stdout; _ } ->
    String_asserter.assert_equals "hello world\n" stdout "";
    true
  | _ -> false

let test_env_variable () =
  let process = Process.exec "printenv" ~env:(Process_types.Augment ["NAME=world"]) [] in
  match Process.read_and_wait_pid ~timeout:2 process with
  | Ok { Process_types.stdout; _ } ->
    let env = String_utils.split_into_lines stdout in
    let name_env = List.filter (fun s -> String_utils.string_starts_with s "NAME=") env in
    (match name_env with
    | [] -> false
    | n :: _ ->
      let () = String_asserter.assert_equals "NAME=world" n "" in
      true)
  | _ -> false

let test_process_timeout () =
  let process = Process.exec "sleep" ["2"] in
  match Process.read_and_wait_pid ~timeout:1 process with
  | Error (Process_types.Timed_out _) -> true
  | _ -> false

let test_process_finishes_within_timeout () =
  let process = Process.exec "sleep" ["1"] in
  match Process.read_and_wait_pid ~timeout:2 process with
  | Ok _ -> true
  | _ -> false

(** Send "hello" to stdin and use sed to replace hello to world. *)
let test_stdin_input () =
  let process = Process.exec "sed" ~input:"hello" ["s/hello/world/g"] in
  match Process.read_and_wait_pid ~timeout:3 process with
  | Ok { Process_types.stdout; _ } ->
    String_asserter.assert_equals "world" stdout "sed should replace hello with world";
    true
  | Error failure ->
    Printf.eprintf "Error %s" (Process.failure_msg failure);
    false

let print_string_main str = Printf.printf "%s\n" str

let print_string_entry = Process.register_entry_point "print_string_main" print_string_main

let test_entry_point () =
  let process = Process.run_entry print_string_entry "hello" in
  let result = Process.read_and_wait_pid ~timeout:10 process in
  match result with
  | Ok { Process_types.stdout; _ } ->
    let () = String_asserter.assert_equals "hello\n" stdout "" in
    true
  | _ -> false

let test_chdir () =
  let process = Process.exec_with_working_directory ~dir:"/tmp" "pwd" [] in
  let result = Process.read_and_wait_pid ~timeout:10 process in
  match result with
  | Ok { Process_types.stdout; _ } ->
    let () = String_asserter.assert_equals "/tmp\n" stdout "" in
    true
  | Error (Process_types.Timed_out _) ->
    Printf.eprintf "Process timed out\n";
    false
  | Error Process_types.Overflow_stdin ->
    Printf.eprintf "Unexpected error process input too large\n";
    false
  | Error (Process_types.Abnormal_exit { stdout; stderr; _ }) ->
    Printf.eprintf "Process exited abnormally\n";
    Printf.eprintf "See stdout: %s\n" stdout;
    Printf.eprintf "See stderr: %s\n" stderr;
    false

let open_an_fd () = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o440

let int_of_fd (x : Unix.file_descr) : int = Obj.magic x

let close_fds fds = List.iter Unix.close fds

(** Asserts the next opened file descriptor is exactly 1 greater
 * than the "last_fd". Repeats this "repeats" times. Accumulates all opened
 * FDs into all_fds and closes all of them at the end. *)
let rec assert_next_fd_incremental ~repeats all_fds last_fd =
  if repeats <= 0 then
    close_fds all_fds
  else
    let fd = open_an_fd () in
    let () =
      Int_asserter.assert_equals
        (int_of_fd last_fd + 1)
        (int_of_fd fd)
        "Test unexpectedly leaked a File Descriptor."
    in
    assert_next_fd_incremental ~repeats:(repeats - 1) (fd :: all_fds) fd

(** Opens one FD before running a test, and then opens 1000 FDs after the test
 * and asserts that all the FD numbers are sequential (i.e. no holes).
 *
 * This catches leaks during the test because if the test opened n FDs
 * and closed none of them, then the first opened file descriptor after the
 * test finishes will be number n+1 instead of one greater than the first
 * one (the one opened before running the test). If instead the test failed
 * to close the k'th descriptor, then this will discover after k
 * iterations that the next FD opened is non-sequential.
 *)
let assert_no_fd_leaked test () =
  let first_fd = open_an_fd () in
  let result = test () in
  assert_next_fd_incremental ~repeats:1000 [first_fd] first_fd;
  result

let tests =
  [
    ("test_echo", assert_no_fd_leaked test_echo);
    ("test_echo_in_a_loop", assert_no_fd_leaked test_echo_in_a_loop);
    ("test_process_read_idempotent", assert_no_fd_leaked test_process_read_idempotent);
    ("test_env_variable", assert_no_fd_leaked test_env_variable);
    ("test_process_timeout", assert_no_fd_leaked test_process_timeout);
    ( "test_process_finishes_within_timeout",
      assert_no_fd_leaked test_process_finishes_within_timeout );
    ("test_stdin_input", assert_no_fd_leaked test_stdin_input);
    ("test_entry_point", assert_no_fd_leaked test_entry_point);
    ("test_chdir", assert_no_fd_leaked test_chdir);
  ]

let () =
  Daemon.check_entry_point ();
  Unit_test.run_all tests
