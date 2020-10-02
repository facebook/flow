(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "utils" >::: [LwtUtils_test.tests; LwtTimeout_test.tests]

let () = run_test_tt_main tests
