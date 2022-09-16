(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Lsp
open Lsp_fmt
open Hh_json

let show_position { Lsp.line; character } =
  Printf.sprintf "{ line = %d; character = %d }" line character

let assert_json ~ctxt ?msg (exp : string) (actual : json) =
  assert_equal ~ctxt ?msg ~printer:json_to_string (json_of_string exp) actual

let position_tests =
  [
    ( "roundtrip" >:: fun ctxt ->
      let p = { line = 1; character = 3 } in
      let json = print_position p in
      assert_json ~ctxt {|{ "line": 1, "character": 3 }|} json;
      let p' = parse_position (Some json) in
      assert_equal ~ctxt ~printer:show_position p p'
    );
  ]

(* TODO: write a load more printing+parsing tests *)

let tests = ["position" >::: position_tests]

let () = run_test_tt_main ("lsp_fmt" >::: tests)
