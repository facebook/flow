(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Stack_utils

let merge_bytes_test () =
  let stack = Stack.create () in
  Stack.push "abc" stack;
  Stack.push "def" stack;
  let result = Stack.merge_bytes stack in
  Asserter.String_asserter.assert_equals "abcdef" result "";
  true

let tests = [("merge_bytes_test", merge_bytes_test)]

let () = Unit_test.run_all tests
