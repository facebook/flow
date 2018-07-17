(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_test_utils

module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module L = Layout_builder

let tests = [
  (* `{ foo: x }` *)
  "flat_spaces_inside_braces" >:: begin fun ctxt ->
    let prop = E.object_property
      (E.object_property_key "foo")
      (E.identifier "x")
    in
    let ast = E.object_ [prop] in
    let prop_layout = Js_layout_generator.object_property prop in
    assert_layout_of_expression ~ctxt
      L.(loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
        fused [
          fused [
            atom "{";
            flat_pretty_space;
          ];
          sequence ~break:Layout.Break_if_needed [
            fused [
              prop_layout;
              Layout.IfBreak ((Layout.IfPretty ((atom ","), empty)), empty);
            ];
          ];
          fused [
            flat_pretty_space;
            atom "}";
          ];
        ];
      ]))
      ast;
  end;
]
