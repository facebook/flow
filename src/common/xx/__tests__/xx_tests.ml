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

let equal_tests =
  "equal"
  >::: [
         ( "is_equal" >:: fun ctxt ->
           let a =
             let state = Xx.init 0L in
             let () = Xx.update state "foo" in
             Xx.digest state
           in
           let b =
             let state = Xx.init 0L in
             let () = Xx.update state "foo" in
             Xx.digest state
           in
           assert_equal ~ctxt ~cmp:Xx.equal ~printer:Xx.to_string a b );
         ( "not_equal" >:: fun ctxt ->
           let a =
             let state = Xx.init 0L in
             let () = Xx.update state "foo" in
             Xx.digest state
           in
           let b =
             let state = Xx.init 0L in
             let () = Xx.update state "bar" in
             Xx.digest state
           in
           assert_equal ~ctxt ~cmp:(fun a b -> not (Xx.equal a b)) ~printer:Xx.to_string a b );
       ]

let tests = "xx" >::: [to_string_tests; equal_tests]

let () = run_test_tt_main tests
