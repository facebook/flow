(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open AutocompleteService_js

let tests =
  [
    "add_autocomplete_token"
    >::: [
           ( "empty_line" >:: fun ctxt ->
             let contents = "// @flow\n\nfoo\n\nbar" in
             let expected = "// @flow\n\nfoo\nAUTO332\nbar" in
             let (actual, broader_context) = add_autocomplete_token contents 4 0 in
             assert_equal ~ctxt ~printer:(fun x -> x) expected actual;
             let expected = "foo\nAUTO332\nbar" in
             assert_equal ~ctxt ~printer:(fun x -> x) expected broader_context
           );
         ];
  ]
