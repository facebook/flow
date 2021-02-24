(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "export" >::: [Export_service_tests.suite]

let () = run_test_tt_main tests
