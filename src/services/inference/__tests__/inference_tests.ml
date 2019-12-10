(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "inference" >::: [Types_js_test.tests; Pure_dep_graph_operations_test.tests]

let () = run_test_tt_main tests
