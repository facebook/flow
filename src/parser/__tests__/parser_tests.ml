(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "parser_utils" >::: [Jsdoc_test.tests; Loc_test.tests; Offset_utils_test.tests]

let () = run_test_tt_main tests
