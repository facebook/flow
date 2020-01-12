(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let test_truncate_short () =
  let s = "hello world" in
  let len = String.length s in
  let truncated = String_utils.truncate len s in
  Asserter.String_asserter.assert_equals
    "hello world"
    truncated
    "truncated to exact length should return the same string";
  let len = len + 10 in
  let truncated = String_utils.truncate len s in
  Asserter.String_asserter.assert_equals
    "hello world"
    truncated
    "truncated with room for more should return the same string";
  true

(** Input string is too long and gets cut short *)
let test_truncate_long () =
  let s = "hello world" in
  let len = 5 in
  let truncated = String_utils.truncate len s in
  Asserter.String_asserter.assert_equals "hello" truncated "truncate cuts the string short";
  true

let tests =
  [("test_truncate_short", test_truncate_short); ("test_truncate_long", test_truncate_long)]

let () = Unit_test.run_all tests
