(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Buffered_line_reader
open Asserter

let test_mixed_read () =
  String_asserter.(
    (* Test line-based reading, LF-terminated *)
    let msg1 = "hello line" in
    (* Test line-based reading, CRLF-terminated *)
    let msg2 = "world line" in
    (* Test contains some newlines in message, will get split. *)
    let msg2a = "this\ncontains\nnewlines" in
    (* Test an empty CRLF-terminated line, like at the end of http headers *)
    let msg3 = "" in
    (* Test a length-based string that's sent in two separate writes *)
    let msg4a = "bytes" in
    let msg4b = " without newlines" in
    (* Test a length-based string that includes LF *)
    let msg5 = "bytes text with \n newlines" in
    (* Test a few bytes of length-based, followed by line-based *)
    let msg6 = "bytes" in
    let msg7 = "line" in
    (* Must do this with threads because of blocking get_next_bytes of msg4a+b *)
    let (fd_in, fd_out) = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      Unix.close fd_in;
      let _ = Unix.write_substring fd_out (msg1 ^ "\n") 0 (String.length msg1 + 1) in
      let _ = Unix.write_substring fd_out (msg2 ^ "\r\n") 0 (String.length msg2 + 2) in
      let _ = Unix.write_substring fd_out (msg2a ^ "\n") 0 (String.length msg2a + 1) in
      let _ = Unix.write_substring fd_out (msg3 ^ "\r\n") 0 (String.length msg3 + 2) in
      let _ = Unix.write_substring fd_out msg4a 0 (String.length msg4a) in
      let _ = Unix.sleepf 0.1 in
      let _ = Unix.write_substring fd_out msg4b 0 (String.length msg4b) in
      let _ = Unix.write_substring fd_out msg5 0 (String.length msg5) in
      let _ = Unix.write_substring fd_out msg6 0 (String.length msg6) in
      let _ = Unix.write_substring fd_out (msg7 ^ "\n") 0 (String.length msg7 + 1) in
      Unix.close fd_out;
      exit 0
    | _pid ->
      Unix.close fd_out;
      let reader = Buffered_line_reader.create fd_in in
      assert_equals msg1 (get_next_line reader) "msg1";
      assert_equals msg2 (get_next_line reader) "msg2";
      assert_equals "this" (get_next_line reader) "msg2a part 1";
      assert_equals "contains" (get_next_line reader) "msg2a part 2";
      assert_equals "newlines" (get_next_line reader) "msg2a part 3";
      assert_equals msg3 (get_next_line reader) "msg3";
      let msg4 = msg4a ^ msg4b in
      assert_equals msg4 (get_next_bytes reader (String.length msg4)) "msg4";
      assert_equals msg5 (get_next_bytes reader (String.length msg5)) "msg5";
      assert_equals msg6 (get_next_bytes reader (String.length msg6)) "msg6";
      assert_equals msg7 (get_next_line reader) "msg7";
      Unix.close fd_in;
      true
  )

let str_split str len =
  let hd = String.sub str 0 len in
  let tl = String.sub str len (String.length str - len) in
  (hd, tl)

let rec write_string str chunk_size fd =
  if String.length str <= chunk_size then
    let written = Unix.single_write_substring fd str 0 (String.length str) in
    assert (written = String.length str)
  else
    let (hd, tl) = str_split str chunk_size in
    let written = Unix.write_substring fd hd 0 (String.length hd) in
    let () = assert (written = String.length hd) in
    write_string tl chunk_size fd

(** Write a string containing newlines at varying spaces apart, a few
 * bytes at a time. *)
let test_few_bytes_at_a_time chunk_size () =
  String_asserter.(
    let (fd_in, fd_out) = Unix.pipe () in
    let () = write_string "a\nbc\ndef\nghij\nklmno\n" chunk_size fd_out in
    let reader = Buffered_line_reader.create fd_in in
    assert_equals "a" (get_next_line reader) "first word";
    assert_equals "bc" (get_next_line reader) "second word";
    assert_equals "def" (get_next_line reader) "third word";
    assert_equals "ghij" (get_next_line reader) "fourth word";
    assert_equals "klmno" (get_next_line reader) "fifth word";
    true
  )

(** Write some after some has been read into the buffer. *)
let test_write_after_read () =
  String_asserter.(
    let (fd_in, fd_out) = Unix.pipe () in
    (* the ending "g" is not newline delimited. Write all of this at once. *)
    let () = write_string "a\nbc\ndef\ngh" 11 fd_out in
    let reader = Buffered_line_reader.create fd_in in
    assert_equals "a" (get_next_line reader) "first word";
    assert_equals "bc" (get_next_line reader) "second word";
    assert_equals "def" (get_next_line reader) "third word";

    (* Write the rest of it. *)
    let () = write_string "ij\nklmno\n" 9 fd_out in
    (* gh written before gets joined with the ij here. *)
    assert_equals "ghij" (get_next_line reader) "fourth word";
    assert_equals "klmno" (get_next_line reader) "fifth word";
    true
  )

let tests =
  [
    ("test_mixed_read", test_mixed_read);
    ("test_one_byte_at_a_time", test_few_bytes_at_a_time 1);
    ("test_two_bytes_at_a_time", test_few_bytes_at_a_time 2);
    ("test_three_bytes_at_a_time", test_few_bytes_at_a_time 3);
    ("test_four_bytes_at_a_time", test_few_bytes_at_a_time 4);
    ("test_write_after_read", test_write_after_read);
  ]

let () = Unit_test.run_all tests
