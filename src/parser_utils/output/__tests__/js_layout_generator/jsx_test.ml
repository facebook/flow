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

module E = Ast_builder.Expressions
module J = Ast_builder.JSXs
module L = Layout_builder

let make_loc start_line end_line = Loc.{
    source = None;
    start = { line = start_line; column = 0; offset = 0; };
    _end = { line = end_line; column = 0; offset = 0; };
  }

let tests = [
  "simple_self_closing" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A/>";
  end;

  "simple" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A></A>";
  end;

  "namespaced" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A:a/>";
  end;

  "member" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A.b/>";
  end;

  "nested_member" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A.b.c/>";
  end;

   "simple_with_child" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A><B/></A>";
  end;

  "simple_with_attribute_and_child" >:: begin fun ctxt ->
    assert_expression_string ~ctxt ~pretty:true (
      "<A a=\"a\"><B /></A>"
    );
  end;

  "long_attribute_with_children" >:: begin fun ctxt ->
    let a_loc = make_loc 1 4 in
    let b_loc = make_loc 2 2 in
    let c_loc = make_loc 3 3 in
    let ast = E.jsx_element ~loc:a_loc (
      J.element
        (J.identifier "A")
        ~attrs:[
          J.attr
            (J.attr_identifier "a")
            (Some (J.attr_literal (Literals.string (String.make 80 'a'))))
        ]
        ~children:[
          J.child_element ~loc:b_loc (J.identifier "B") ~selfclosing:true;
          J.child_element ~loc:c_loc (J.identifier "C") ~selfclosing:true;
        ]
    ) in
    let layout = L.(loc ~loc:a_loc (fused [
      loc (fused [
        atom "<"; id "A";
        sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
          fused [
            Layout.IfBreak (empty, (atom " "));
            sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
              loc (fused [
                id "a";
                atom "=";
                loc (fused [
                  atom "\"";
                  atom "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
                  atom "\"";
                ]);
              ]);
            ];
          ];
        ];
        atom ">";
      ]);
      sequence ~break:Layout.Break_if_pretty [
        fused [
          loc ~loc:b_loc (loc (fused [atom "<"; id "B"; pretty_space; atom "/>"]));
          hardline;
          loc ~loc:c_loc (loc (fused [atom "<"; id "C"; pretty_space; atom "/>"]));
        ]
      ];
      loc (fused [atom "</"; id "A"; atom ">"]);
    ])) in
    assert_layout_of_expression ~ctxt layout ast;
    assert_expression ~ctxt ~pretty:true (
      "<A\n  a=\"" ^ String.make 80 'a' ^ "\">\n  <B />\n  <C />\n</A>"
    ) ast;
    assert_expression ~ctxt (
      "<A a=\"" ^ String.make 80 'a' ^ "\"><B/>\n<C/></A>"
    ) ast;
  end;

  "long_child_text" >:: begin fun ctxt ->
    assert_expression_string ~ctxt ~pretty:true (
      "<A a=\"a\">\n  " ^ String.make 80 'b' ^ "\n</A>"
    );
  end;

  "literal_whitespace" >:: begin fun ctxt ->
    assert_expression_string ~ctxt ~pretty:true (
      "<A a=\"a\">\n  a{\" \"}\n  b\n</A>"
    );
  end;

  "children_fit_on_one_line" >:: begin fun ctxt ->
    assert_expression_string ~ctxt ~pretty:true (
      "<A><B /><C /></A>"
    );
  end;

  "long_child_element" >:: begin fun ctxt ->
    assert_expression_string ~ctxt ~pretty:true (
      "<A>\n  <" ^ String.make 80 'B' ^ " />\n  <C />\n</A>"
    );
  end;

  "user_supplied_newlines" >:: begin fun ctxt ->
    (* TODO: Utils_jsx.trim_jsx_text is overly aggressive for pretty
     *       printing, user supplied newlines between words should be
     *       maintained. The following test should pass:
     *
     *  assert_expression_string ~ctxt ~pretty:true (
     *    "<A>\n  " ^ String.make 80 'a' ^ "\n  " ^ String.make 80 'b' ^ "\n</A>"
     *  );
     *)
    ignore ctxt;
  end;

  "valueless_attribute" >:: begin fun ctxt ->
    (* TODO: valueless attributes shouldnt print trailing spaces when last *)
    assert_expression_string ~ctxt "<A a />";
  end;

  "namespaced_valueluess_attribute" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a:a />";
  end;

  "attribute_braces" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a={1}/>";
  end;

  "attribute_string" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a=\"\"/>";
  end;

  "attribute_spread" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A {...a}/>";
  end;

  "attribute_spread_between_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a {...a}b />";
  end;

  "multiple_shorthand_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a b />";
  end;

  "shorthand_and_longhand_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a b=\"\"/>";
  end;

  "multiple_longhand_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A a=\"a\"b={b}/>";
    assert_expression_string ~ctxt ~pretty:true (
      "<A a=\"a\" b=\"b\" />"
    );
    assert_expression_string ~ctxt ~pretty:true (
      "<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b=\"b\"\n/>"
    );
  end;

  "string_longhand_and_shorthand_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A b=\"\"a />";
    assert_expression_string ~ctxt ~pretty:true (
      "<A a=\"a\" b />"
    );
    assert_expression_string ~ctxt ~pretty:true (
      "<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b\n/>"
    );
  end;

  "expression_longhand_and_shorthand_attributes" >:: begin fun ctxt ->
    assert_expression_string ~ctxt "<A b={1}a />";
  end;
]
