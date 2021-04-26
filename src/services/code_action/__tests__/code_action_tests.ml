(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let _handle =
  let one_gig = 1024 * 1024 * 1024 in
  SharedMem.init
    ~num_workers:0
    { SharedMem.heap_size = 5 * one_gig; hash_table_pow = 19; log_level = 0 }

let tests =
  "code_action"
  >::: [
         Autofix_imports_tests.tests;
         Code_action_service_tests.tests;
         Insert_type_utils_tests.tests;
         Validation_tests.tests;
       ]

let () = run_test_tt_main tests
