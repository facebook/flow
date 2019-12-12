(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  "typing"
  >::: [Typed_ast_test.tests; Signature_verifier_test.tests; Signature_generator_test.tests]

let _handle =
  let one_gig = 1024 * 1024 * 1024 in
  SharedMem_js.(
    init
      ~num_workers:0
      {
        global_size = 0;
        heap_size = 5 * one_gig;
        hash_table_pow = 19;
        shm_dirs = [];
        shm_min_avail = one_gig / 2;
        log_level = 0;
      })

let () = run_test_tt_main tests
