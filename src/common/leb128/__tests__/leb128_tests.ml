(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let mk_read_byte buf =
  let bytes = Buffer.to_bytes buf in
  let pos = ref 0 in
  fun () ->
    let byte = Bytes.get bytes !pos in
    incr pos;
    Char.code byte

let test_roundtrip case ctxt =
  let buf = Buffer.create 16 in
  Leb128.Unsigned.write (Buffer.add_int8 buf) case;
  let out = Leb128.Unsigned.read (mk_read_byte buf) in
  assert_equal ~ctxt ~cmp:Int.equal ~printer:Int.to_string out case

let mk_test case =
  let test_name = Printf.sprintf "roundtrip_%d" case in
  test_name >:: test_roundtrip case

let mk_tests = List.rev_map mk_test

let () = run_test_tt_main ("leb128" >::: mk_tests [0; 127; 128; 255; 256; 1024; max_int])
