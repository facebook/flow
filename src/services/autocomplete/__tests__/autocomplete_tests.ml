(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "autocomplete" >::: ["autocompleteService_js" >::: AutocompleteService_js_tests.tests]

let () = run_test_tt_main tests
