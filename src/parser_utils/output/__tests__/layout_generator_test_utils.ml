(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast_builder

let space_regex = Str.regexp_string " "

let newline_regex = Str.regexp_string "\n"

let assert_output ~ctxt ?msg ?(pretty = false) expected_str layout =
  let print =
    if pretty then
      Pretty_printer.print ~source_maps:None ~skip_endline:false
    else
      Compact_printer.print ~source_maps:None
  in
  let out = print layout |> Source.contents in
  let out = String.sub out 0 (String.length out - 1) in
  (* remove trailing \n *)
  let printer x =
    x
    |> Str.global_replace space_regex "\xE2\x90\xA3" (* open box *)
    |> Str.global_replace newline_regex "\xC2\xAC\n"
    (* not sign, what Atom uses *)
  in
  assert_equal ~ctxt ?msg ~printer expected_str out

let assert_expression
    ~ctxt ?msg ?pretty ?(expr_ctxt = Js_layout_generator.normal_context) expected_str ast =
  let layout = Js_layout_generator.expression ~ctxt:expr_ctxt ast in
  assert_output ~ctxt ?msg ?pretty expected_str layout

let assert_expression_string ~ctxt ?msg ?pretty ?expr_ctxt str =
  let ast = expression_of_string str in
  assert_expression ~ctxt ?msg ?pretty ?expr_ctxt str ast

let assert_statement ~ctxt ?msg ?pretty expected_str ast =
  let layout = Js_layout_generator.statement ast in
  assert_output ~ctxt ?msg ?pretty expected_str layout

let assert_statement_string ~ctxt ?msg ?pretty str =
  let ast = statement_of_string str in
  let layout = Js_layout_generator.statement ast in
  assert_output ~ctxt ?msg ?pretty str layout
