(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let mk_loc (start_line, start_column, end_line, end_column) =
  {
    Loc.source = None;
    start = { Loc.line = start_line; column = start_column };
    _end = { Loc.line = end_line; column = end_column };
  }

let hexdump = String.iter (fun c -> Printf.printf "%02x " (Char.code c))

let locdump loc =
  let open Loc in
  Printf.printf "%d:%d-%d:%d" loc.start.line loc.start.column loc._end.line loc._end.column

let dump locs =
  let len = Array.length locs in
  let iter f = Array.iter (fun loc -> f (mk_loc loc)) locs in
  let packed = Packed_locs.pack len iter in
  let locs = Packed_locs.unpack None Array.init packed in
  hexdump packed;
  Array.iter
    (fun loc ->
      print_newline ();
      locdump loc)
    locs

let%expect_test "single_same_line_near_column" =
  dump [| (0, 63, 0, 64) |];
  [%expect {|
    01 3f 01
    0:63-0:64
  |}]

let%expect_test "single_same_line_far_column" =
  dump [| (0, 64, 0, 65) |];
  [%expect {|
    01 40 00 01
    0:64-0:65
  |}]

let%expect_test "single_near_line" =
  dump [| (62, 1, 62, 3) |];
  [%expect {|
    01 7e 01 02
    62:1-62:3
  |}]

let%expect_test "single_far_line" =
  dump [| (63, 1, 63, 3) |];
  [%expect {|
    01 7f 00 01 02
    63:1-63:3
  |}]

let%expect_test "multi_same_line_near_column" =
  dump [| (0, 63, 1, 2) |];
  [%expect {|
    01 bf 01 02
    0:63-1:2
  |}]

let%expect_test "multi_same_line_far_column" =
  dump [| (0, 64, 1, 2) |];
  [%expect {|
    01 c0 00 01 02
    0:64-1:2
  |}]

let%expect_test "multi_near_line" =
  dump [| (62, 1, 64, 3) |];
  [%expect {|
    01 fe 01 02 03
    62:1-64:3
  |}]

let%expect_test "multi_far_line" =
  dump [| (63, 1, 64, 3) |];
  [%expect {|
    01 ff 00 01 01 03
    63:1-64:3
  |}]
