(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let mk ?source start_line start_column end_line end_column =
  let start = { Loc.line = start_line; column = start_column } in
  let _end = { Loc.line = end_line; column = end_column } in
  { Loc.source; start; _end }

let intersects_tests =
  [
    ( "locs_equal" >:: fun ctxt ->
      let loc1 = mk 1 0 1 5 in
      let loc2 = mk 1 0 1 5 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc1_before_loc2_same_line" >:: fun ctxt ->
      let loc1 = mk 1 0 1 2 in
      let loc2 = mk 1 3 1 5 in
      assert_equal ~ctxt false (Loc.intersects loc1 loc2)
    );
    ( "loc1_before_loc2_diff_line" >:: fun ctxt ->
      let loc1 = mk 1 0 1 2 in
      let loc2 = mk 2 0 2 2 in
      assert_equal ~ctxt false (Loc.intersects loc1 loc2)
    );
    ( "loc1_after_loc2_same_line" >:: fun ctxt ->
      let loc1 = mk 1 3 1 5 in
      let loc2 = mk 1 0 1 2 in
      assert_equal ~ctxt false (Loc.intersects loc1 loc2)
    );
    ( "loc1_after_loc2_diff_line" >:: fun ctxt ->
      let loc1 = mk 2 0 2 2 in
      let loc2 = mk 1 0 1 2 in
      assert_equal ~ctxt false (Loc.intersects loc1 loc2)
    );
    ( "loc1_in_loc2" >:: fun ctxt ->
      let loc1 = mk 1 1 1 3 in
      let loc2 = mk 1 0 1 5 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_in_loc1" >:: fun ctxt ->
      let loc1 = mk 1 0 1 5 in
      let loc2 = mk 1 1 1 3 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_intersects_end_of_loc1" >:: fun ctxt ->
      let loc1 = mk 1 0 1 5 in
      let loc2 = mk 1 3 1 7 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_intersects_start_of_loc1" >:: fun ctxt ->
      let loc1 = mk 1 3 1 7 in
      let loc2 = mk 1 0 1 5 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_intersects_end_of_loc1_multiline" >:: fun ctxt ->
      let loc1 = mk 1 0 3 0 in
      let loc2 = mk 2 0 4 0 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_intersects_start_of_loc1_multiline" >:: fun ctxt ->
      let loc1 = mk 2 0 4 0 in
      let loc2 = mk 1 0 3 0 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc1_extends_past_loc2" >:: fun ctxt ->
      let loc1 = mk 1 0 2 0 in
      let loc2 = mk 1 0 1 3 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_extends_past_loc1" >:: fun ctxt ->
      let loc1 = mk 1 0 1 3 in
      let loc2 = mk 1 0 2 0 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc1_starts_before_loc2" >:: fun ctxt ->
      let loc1 = mk 1 0 2 0 in
      let loc2 = mk 1 3 2 0 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
    ( "loc2_starts_before_loc1" >:: fun ctxt ->
      let loc1 = mk 1 3 2 0 in
      let loc2 = mk 1 0 2 0 in
      assert_equal ~ctxt true (Loc.intersects loc1 loc2)
    );
  ]

let tests = "loc" >::: ["intersects" >::: intersects_tests]
