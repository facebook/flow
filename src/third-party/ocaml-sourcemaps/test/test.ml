(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "sourcemaps" >::: [
  Compose_test.tests;
  Json_test.tests;
  Original_loc_test.tests;
  Original_loc_frozen_test.tests;
]

let () = run_test_tt_main tests
