(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let to_string_tests =
  "to_string"
  >::: [
         ( "is_16_chars" >:: fun ctxt ->
           let state = Xx.init 0L in
           let hash = Xx.digest state in
           let str = Xx.to_string hash in
           assert_equal ~ctxt ~printer:(Printf.sprintf "%S") "ef46db3751d8e999" str );
       ]

let tests = "xx" >::: [to_string_tests]

let () = run_test_tt_main tests
