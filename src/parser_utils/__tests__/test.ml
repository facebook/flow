(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open OUnit2

let tests = "parser_utils" >::: [
  Scope_builder_test.tests;
  Comment_attacher_test.tests;
  Ssa_builder_test.tests;
  Require_test.tests;
]

let () = run_test_tt_main tests
