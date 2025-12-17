(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let mk_source () = Source.create ()

let mk_loc (start_line, start_col) (end_line, end_col) =
  {
    Loc.none with
    Loc.start = { Loc.line = start_line; column = start_col };
    _end = { Loc.line = end_line; column = end_col };
  }

let assert_contents_equal =
  let printer x = x in
  fun ~ctxt (expected : string) (source : Source.t) ->
    assert_equal ~ctxt ~printer expected (Source.contents source)

let tests =
  "source"
  >::: [
         ( "simple_string" >:: fun ctxt ->
           let s = mk_source () |> Source.add_string "foo;" in
           assert_contents_equal ~ctxt "foo;" s
         );
         ( "two_strings" >:: fun ctxt ->
           let s = mk_source () |> Source.add_string "foo;" |> Source.add_string "bar;" in
           assert_contents_equal ~ctxt "foo;bar;" s
         );
       ]
