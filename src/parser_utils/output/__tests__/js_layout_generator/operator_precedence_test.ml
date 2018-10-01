(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast_builder
open Layout_test_utils
open Layout_generator_test_utils

module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module L = Layout_builder

let x, y, z = E.identifier "x", E.identifier "y", E.identifier "z"
let x40 = E.identifier (String.make 40 'x')
let str = E.literal (Literals.string "a")
let (&&) a b = E.logical_and a b
let (||) a b = E.logical_or a b
let (+) a b = E.binary ~op:Flow_ast.Expression.Binary.Plus a b
let (-) a b = E.binary ~op:Flow_ast.Expression.Binary.Minus a b

let tests = [
  "and_with_and_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x && y) && z) in
    assert_layout ~ctxt
      L.(loc (group [
        expression (x && y); pretty_space; atom "&&";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "x&&y&&z" layout;
    assert_output ~ctxt ~pretty:true "x && y && z" layout;

    let layout = Js_layout_generator.expression ((x40 && x40) && x40) in
    assert_output ~ctxt
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      layout;
    assert_output ~ctxt ~pretty:true
      ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      layout;
  end;

  "and_with_and_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x && (y && z)) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "&&";
        indent (fused (
          line::(wrap_in_parens_raw (expression (y && z)))
        ));
      ]))
      layout;
    assert_output ~ctxt "x&&(y&&z)" layout;
    assert_output ~ctxt ~pretty:true "x && (y && z)" layout;

    let layout = Js_layout_generator.expression (x40 && (x40 && x40)) in
    assert_output ~ctxt
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)"
      layout;
    assert_output ~ctxt ~pretty:true
      ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "  (\n"^
       "    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"^
       "  )")
      layout;
  end;

  "or_with_and_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x && y) || z) in
    assert_layout ~ctxt
      L.(loc (group [
        expression (x && y); pretty_space; atom "||";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "x&&y||z" layout;
    assert_output ~ctxt ~pretty:true "x && y || z" layout;

    let layout = Js_layout_generator.expression ((x40 && x40) || x40) in
    assert_output ~ctxt
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      layout;
    assert_output ~ctxt ~pretty:true
      ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      layout;
  end;

  "and_with_or_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x && (y || z)) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "&&";
        indent (fused (
          line::(wrap_in_parens_raw (expression (y || z)))
        ));
      ]))
      layout;
    assert_output ~ctxt "x&&(y||z)" layout;
    assert_output ~ctxt ~pretty:true "x && (y || z)" layout;

    let layout = Js_layout_generator.expression (x40 && (x40 || x40)) in
    assert_output ~ctxt
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)"
      layout;
    assert_output ~ctxt ~pretty:true
      ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"^
       "  (\n"^
       "    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"^
       "      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"^
       "  )")
      layout;
  end;

  "or_with_or_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x || y) || z) in
    assert_layout ~ctxt
      L.(loc (group [
        expression (x || y); pretty_space; atom "||";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "x||y||z" layout;
    assert_output ~ctxt ~pretty:true "x || y || z" layout;

    let layout = Js_layout_generator.expression ((x40 || x40) || x40) in
    assert_output ~ctxt
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      layout;
    assert_output ~ctxt ~pretty:true
      ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"^
       "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      layout;
  end;

  "or_with_or_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x || (y || z)) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "||";
        indent (fused (
          line::(wrap_in_parens_raw (expression (y || z)))
        ));
      ]))
      layout;
    assert_output ~ctxt "x||(y||z)" layout;
    assert_output ~ctxt ~pretty:true "x || (y || z)" layout;
  end;

  "and_with_or_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x || y) && z) in
    assert_layout ~ctxt
      L.(loc (group [
        wrap_in_parens (expression (x || y));
        pretty_space; atom "&&";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "(x||y)&&z" layout;
    assert_output ~ctxt ~pretty:true "(x || y) && z" layout;
  end;

  "or_with_and_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x || (y && z)) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "||";
        indent (fused [line; expression (y && z)]);
      ]))
      layout;
    assert_output ~ctxt "x||y&&z" layout;
    assert_output ~ctxt ~pretty:true "x || y && z" layout;
  end;

  "plus_with_plus_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x + y) + z) in
    assert_layout ~ctxt
      L.(loc (fused [expression (x + y); pretty_space; atom "+"; pretty_space; expression z]))
      layout;
    assert_output ~ctxt "x+y+z" layout;
    assert_output ~ctxt ~pretty:true "x + y + z" layout;
  end;

  "plus_with_plus_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x + (y + z)) in
    assert_layout ~ctxt
      L.(loc (fused (
        [expression x; pretty_space; atom "+"; pretty_space] @
        wrap_in_parens_raw (expression (y + z))
      )))
      layout;
    assert_output ~ctxt "x+(y+z)" layout;
    assert_output ~ctxt ~pretty:true "x + (y + z)" layout;
  end;

  "minus_with_plus_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x + y) - z) in
    assert_layout ~ctxt
      L.(loc (fused [expression (x + y); pretty_space; atom "-"; pretty_space; expression z]))
      layout;
    assert_output ~ctxt "x+y-z" layout;
    assert_output ~ctxt ~pretty:true "x + y - z" layout;
  end;

  "plus_with_minus_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x + (y - z)) in
    assert_layout ~ctxt
      L.(loc (fused (
        [expression x; pretty_space; atom "+"; pretty_space] @
        wrap_in_parens_raw (expression (y - z))
      )))
      layout;
    assert_output ~ctxt "x+(y-z)" layout;
    assert_output ~ctxt ~pretty:true "x + (y - z)" layout;
  end;

  "and_with_plus_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x + y) && z) in
    assert_layout ~ctxt
      L.(loc (group [
        expression (x + y); pretty_space; atom "&&";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "x+y&&z" layout;
    assert_output ~ctxt ~pretty:true "x + y && z" layout;
  end;

  "plus_with_and_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x + (y && z)) in
    assert_layout ~ctxt
      L.(loc (fused (
        [expression x; pretty_space; atom "+"; pretty_space] @
        wrap_in_parens_raw (expression (y && z))
      )))
      layout;
    assert_output ~ctxt "x+(y&&z)" layout;
    assert_output ~ctxt ~pretty:true "x + (y && z)" layout;
  end;

  "plus_with_and_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression ((x && y) + z) in
    assert_layout ~ctxt
      L.(loc (fused (
        wrap_in_parens_raw (expression (x && y)) @
        [pretty_space; atom "+"; pretty_space; expression z]
      )))
      layout;
    assert_output ~ctxt "(x&&y)+z" layout;
    assert_output ~ctxt ~pretty:true "(x && y) + z" layout;
  end;

  "and_with_plus_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x && (y + z)) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "&&";
        indent (fused [line; expression (y + z)]);
      ]))
      layout;
    assert_output ~ctxt "x&&y+z" layout;
    assert_output ~ctxt ~pretty:true "x && y + z" layout;
  end;

  "and_literal_lhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (str && x) in
    assert_layout ~ctxt
      L.(loc (group [
        expression str; pretty_space; atom "&&";
        indent (fused [line; expression x]);
      ]))
      layout;
    assert_output ~ctxt "\"a\"&&x" layout;
    assert_output ~ctxt ~pretty:true "\"a\" && x" layout;
  end;

  "and_literal_rhs" >:: begin fun ctxt ->
    let layout = Js_layout_generator.expression (x && str) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "&&";
        indent (fused [line; expression str]);
      ]))
      layout;
    assert_output ~ctxt "x&&\"a\"" layout;
    assert_output ~ctxt ~pretty:true "x && \"a\"" layout;
  end;

  "function" >:: begin fun ctxt ->
    let fn = (Loc.none, Flow_ast.Expression.Function (
      Functions.make ~id:None ~expression:true ~params:[] ())
    ) in
    let layout = Js_layout_generator.expression (fn && x) in
    assert_layout ~ctxt
      L.(loc (group [
        expression fn; pretty_space; atom "&&";
        indent (fused [line; expression x]);
      ]))
      layout;
    assert_output ~ctxt "function(){}&&x" layout;
    assert_output ~ctxt ~pretty:true "function() {} && x" layout;

    let layout = Js_layout_generator.expression (x && fn) in
    assert_layout ~ctxt
      L.(loc (group [
        expression x; pretty_space; atom "&&";
        indent (fused [line; expression fn]);
      ]))
      layout;
    assert_output ~ctxt "x&&function(){}" layout;
    assert_output ~ctxt ~pretty:true "x && function() {}" layout;
  end;

  "sequence" >:: begin fun ctxt ->
    let seq = E.sequence [x; y] in
    let layout = Js_layout_generator.expression (seq && z) in
    assert_layout ~ctxt
      L.(loc (group [
        wrap_in_parens (expression seq);
        pretty_space; atom "&&";
        indent (fused [line; expression z]);
      ]))
      layout;
    assert_output ~ctxt "(x,y)&&z" layout;
    assert_output ~ctxt ~pretty:true "(x, y) && z" layout;

    let layout = Js_layout_generator.expression (z && seq) in
    assert_layout ~ctxt
      L.(loc (group [
        expression z; pretty_space; atom "&&";
        indent (fused (
          line::(wrap_in_parens_raw (expression seq))
        ));
      ]))
      layout;
    assert_output ~ctxt "z&&(x,y)" layout;
    assert_output ~ctxt ~pretty:true "z && (x, y)" layout;

    let layout = Js_layout_generator.expression (E.sequence [z; seq]) in
    assert_layout ~ctxt
      L.(loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
        sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
          fused [
            expression z;
            Layout.IfBreak ((atom ","), (fused [atom ","; pretty_space]));
          ];
          wrap_in_parens (expression seq);
        ];
      ]))
      layout;
    assert_output ~ctxt "z,(x,y)" layout;
    assert_output ~ctxt ~pretty:true "z, (x, y)" layout;

    let layout = Js_layout_generator.expression (E.sequence [seq; z]) in
    assert_layout ~ctxt
      L.(loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
        sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
          fused (
            wrap_in_parens_raw (expression seq) @
            [Layout.IfBreak ((atom ","), (fused [atom ","; pretty_space]))]
          );
          expression z;
        ];
      ]))
      layout;
    assert_output ~ctxt "(x,y),z" layout;
    assert_output ~ctxt ~pretty:true "(x, y), z" layout;
  end;
]
