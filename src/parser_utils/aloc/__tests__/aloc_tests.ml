(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let compare_tests =
  [
    ( "keyed_vs_concrete_whole_file" >:: fun _ctxt ->
      let source = File_key.SourceFile "test.js" in
      (* a loc at 0:0 points at the whole file. it should be sorted before all keyed locs *)
      let concrete = ALoc.of_loc (Loc.mk_loc ~source (0, 0) (0, 0)) in
      let keyed = ALoc.ALocRepresentationDoNotUse.make_keyed (Some source) 1 in
      assert_bool "concrete should come before keyed" (ALoc.compare concrete keyed < 0);
      assert_bool "keyed should come after concrete" (ALoc.compare keyed concrete > 0)
    );
    ( "keyed_vs_concrete" >:: fun _ctxt ->
      let source = File_key.SourceFile "test.js" in
      let concrete = ALoc.of_loc (Loc.mk_loc ~source (1, 1) (1, 2)) in
      let keyed = ALoc.ALocRepresentationDoNotUse.make_keyed (Some source) 1 in
      assert_raises
        (Invalid_argument
           "Unable to compare a keyed location with a concrete one. loc1: \"test.js\": (1, 1) to (1, 2), loc2: \"test.js\": 1"
        )
        (fun () -> ALoc.compare concrete keyed
      )
    );
  ]

let tests = "aloc" >::: ["compare" >::: compare_tests]

let () = run_test_tt_main tests
