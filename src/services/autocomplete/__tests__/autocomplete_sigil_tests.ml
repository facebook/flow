(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  [
    "add"
    >::: [
           ( "empty_line" >:: fun ctxt ->
             let contents = "// @flow\n\nfoo\n\nbar" in
             let expected = "// @flow\n\nfoo\nAUTO332\nbar" in
             let (actual, broader_context) = Autocomplete_sigil.add contents 4 0 in
             assert_equal ~ctxt ~printer:(fun x -> x) expected actual;
             let expected = "foo\nAUTO332\nbar" in
             assert_equal ~ctxt ~printer:(fun x -> x) expected broader_context
           );
           ( "last_line" >:: fun ctxt ->
             let contents = "// @flow\n" in
             let expected = "// @flow\nAUTO332" in
             let (actual, broader_context) = Autocomplete_sigil.add contents 2 0 in
             assert_equal ~ctxt ~printer:(fun x -> x) expected actual;
             let expected = "// @flow\nAUTO332" in
             assert_equal ~ctxt ~printer:(fun x -> x) expected broader_context
           );
         ];
  ]
