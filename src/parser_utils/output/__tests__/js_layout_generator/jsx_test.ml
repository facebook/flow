(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
module S = Ast_builder.Statements
module L = Layout_builder

let opts = Js_layout_generator.default_opts

let make_loc start_line end_line =
  Loc.
    {
      source = None;
      start = { line = start_line; column = 0 };
      _end = { line = end_line; column = 0 };
    }

let tests =
  [
    ("simple_self_closing" >:: fun ctxt -> assert_expression_string ~ctxt "<A/>");
    ("simple" >:: fun ctxt -> assert_expression_string ~ctxt "<A></A>");
    ("namespaced" >:: fun ctxt -> assert_expression_string ~ctxt "<A:a/>");
    ("member" >:: fun ctxt -> assert_expression_string ~ctxt "<A.b/>");
    ("nested_member" >:: fun ctxt -> assert_expression_string ~ctxt "<A.b.c/>");
    ("simple_with_child" >:: fun ctxt -> assert_expression_string ~ctxt "<A><B/></A>");
    ( "simple_with_attribute_and_child" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "<A a=\"a\"><B /></A>" );
    ( "long_attribute_with_children" >:: fun ctxt ->
      let a_loc = make_loc 1 4 in
      let b_loc = make_loc 2 2 in
      let c_loc = make_loc 3 3 in
      let ast =
        E.jsx_element
          ~loc:a_loc
          (J.element
             (J.identifier "A")
             ~attrs:
               [
                 J.attr
                   (J.attr_identifier "a")
                   (Some (J.attr_literal (Literals.string (String.make 80 'a'))));
               ]
             ~children:
               [
                 J.child_element ~loc:b_loc (J.identifier "B") ~selfclosing:true;
                 J.child_element ~loc:c_loc (J.identifier "C") ~selfclosing:true;
               ])
      in
      let layout =
        L.(
          loc
            ~loc:a_loc
            (fused
               [
                 loc
                   (group
                      [
                        atom "<";
                        id "A";
                        indent
                          (fused
                             [
                               line;
                               loc
                                 (fused
                                    [
                                      id "a";
                                      atom "=";
                                      loc
                                        (fused
                                           [
                                             atom "\"";
                                             atom
                                               "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
                                             atom "\"";
                                           ]);
                                    ]);
                             ]);
                        atom ">";
                      ]);
                 indent
                   (fused
                      [
                        pretty_hardline;
                        loc ~loc:b_loc (loc (group [atom "<"; id "B"; pretty_space; atom "/>"]));
                        hardline;
                        loc ~loc:c_loc (loc (group [atom "<"; id "C"; pretty_space; atom "/>"]));
                      ]);
                 pretty_hardline;
                 loc (fused [atom "</"; id "A"; atom ">"]);
               ]))
      in
      assert_layout_of_expression ~ctxt layout ast;
      assert_expression
        ~ctxt
        ~pretty:true
        ("<A\n  a=\"" ^ String.make 80 'a' ^ "\">\n  <B />\n  <C />\n</A>")
        ast;
      assert_expression ~ctxt ("<A a=\"" ^ String.make 80 'a' ^ "\"><B/>\n<C/></A>") ast );
    ( "borderline_length_with_children" >:: fun ctxt ->
      (* opening tag is 80 columns. if it's indented, make sure it breaks.

      <aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc" ddddd="eeeeeeeeeeee">
        <f />
      </aaaaaaaaaaaaa>
    *)
      let a_loc = make_loc 1 4 in
      let f_loc = make_loc 2 2 in
      let ast =
        E.jsx_element
          ~loc:a_loc
          (J.element
             (J.identifier "aaaaaaaaaaaaa")
             ~attrs:
               [
                 J.attr
                   (J.attr_identifier "bbbb")
                   (Some (J.attr_literal (Literals.string "cccccccccccccccccccccccccccccccccccc")));
                 J.attr
                   (J.attr_identifier "ddddd")
                   (Some (J.attr_literal (Literals.string "eeeeeeeeeeee")));
               ]
             ~children:[J.child_element ~loc:f_loc (J.identifier "f") ~selfclosing:true])
      in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout
        ~ctxt
        L.(
          loc
            ~loc:a_loc
            (fused
               [
                 loc
                   (group
                      [
                        atom "<";
                        id "aaaaaaaaaaaaa";
                        indent
                          (fused
                             [
                               line;
                               loc
                                 (fused
                                    [
                                      id "bbbb";
                                      atom "=";
                                      loc
                                        (fused
                                           [
                                             atom "\"";
                                             atom "cccccccccccccccccccccccccccccccccccc";
                                             atom "\"";
                                           ]);
                                    ]);
                               pretty_line;
                               loc
                                 (fused
                                    [
                                      id "ddddd";
                                      atom "=";
                                      loc (fused [atom "\""; atom "eeeeeeeeeeee"; atom "\""]);
                                    ]);
                             ]);
                        atom ">";
                      ]);
                 indent
                   (fused
                      [
                        pretty_hardline;
                        loc ~loc:f_loc (loc (group [atom "<"; id "f"; pretty_space; atom "/>"]));
                      ]);
                 pretty_hardline;
                 loc (fused [atom "</"; id "aaaaaaaaaaaaa"; atom ">"]);
               ]))
        layout;
      assert_output
        ~ctxt
        ( {|<aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc"ddddd="eeeeeeeeeeee">|}
        ^ {|<f/>|}
        ^ {|</aaaaaaaaaaaaa>|} )
        layout;
      assert_output
        ~ctxt
        ~pretty:true
        ( {|<aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc" ddddd="eeeeeeeeeeee">|}
        ^ "\n"
        ^ {|  <f />|}
        ^ "\n"
        ^ {|</aaaaaaaaaaaaa>|} )
        layout;

      let block_layout = Js_layout_generator.statement ~opts (S.block [S.expression ast]) in
      assert_output
        ~ctxt
        ( "{"
        ^ "<aaaaaaaaaaaaa bbbb=\"cccccccccccccccccccccccccccccccccccc\"ddddd=\"eeeeeeeeeeee\">"
        ^ "<f/>"
        ^ "</aaaaaaaaaaaaa>"
        ^ "}" )
        block_layout;
      assert_output
        ~ctxt
        ~pretty:true
        ( "{\n"
        ^ "  <aaaaaaaaaaaaa\n"
        ^ "    bbbb=\"cccccccccccccccccccccccccccccccccccc\"\n"
        ^ "    ddddd=\"eeeeeeeeeeee\">\n"
        ^ "    <f />\n"
        ^ "  </aaaaaaaaaaaaa>;\n"
        ^ "}" )
        block_layout );
    ( "long_child_text" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true ("<A a=\"a\">\n  " ^ String.make 80 'b' ^ "\n</A>")
    );
    ( "literal_whitespace" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "<A a=\"a\">\n  a{\" \"}\n  b\n</A>" );
    ( "children_fit_on_one_line" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "<A><B /><C /></A>" );
    ( "long_child_element" >:: fun ctxt ->
      assert_expression_string
        ~ctxt
        ~pretty:true
        ("<A>\n  <" ^ String.make 80 'B' ^ " />\n  <C />\n</A>") );
    ( "user_supplied_newlines" >:: fun ctxt ->
      (* TODO: Utils_jsx.trim_jsx_text is overly aggressive for pretty
       *       printing, user supplied newlines between words should be
       *       maintained. The following test should pass:
       *
       *  assert_expression_string ~ctxt ~pretty:true (
       *    "<A>\n  " ^ String.make 80 'a' ^ "\n  " ^ String.make 80 'b' ^ "\n</A>"
       *  );
       *)
      ignore ctxt );
    ( "valueless_attribute" >:: fun ctxt ->
      (* TODO: valueless attributes shouldnt print trailing spaces when last *)
      assert_expression_string ~ctxt "<A a />" );
    ("namespaced_valueluess_attribute" >:: fun ctxt -> assert_expression_string ~ctxt "<A a:a />");
    ("attribute_braces" >:: fun ctxt -> assert_expression_string ~ctxt "<A a={1}/>");
    ("attribute_string" >:: fun ctxt -> assert_expression_string ~ctxt "<A a=\"\"/>");
    ("attribute_spread" >:: fun ctxt -> assert_expression_string ~ctxt "<A {...a}/>");
    ( "attribute_spread_between_attributes" >:: fun ctxt ->
      assert_expression_string ~ctxt "<A a {...a}b />" );
    ("multiple_shorthand_attributes" >:: fun ctxt -> assert_expression_string ~ctxt "<A a b />");
    ( "shorthand_and_longhand_attributes" >:: fun ctxt ->
      assert_expression_string ~ctxt "<A a b=\"\"/>" );
    ( "multiple_longhand_attributes" >:: fun ctxt ->
      assert_expression_string ~ctxt "<A a=\"a\"b={b}/>";
      assert_expression_string ~ctxt ~pretty:true "<A a=\"a\" b=\"b\" />";
      assert_expression_string
        ~ctxt
        ~pretty:true
        ("<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b=\"b\"\n/>") );
    ( "string_longhand_and_shorthand_attributes" >:: fun ctxt ->
      assert_expression_string ~ctxt "<A b=\"\"a />";
      assert_expression_string ~ctxt ~pretty:true "<A a=\"a\" b />";
      assert_expression_string ~ctxt ~pretty:true ("<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b\n/>")
    );
    ( "expression_longhand_and_shorthand_attributes" >:: fun ctxt ->
      assert_expression_string ~ctxt "<A b={1}a />" );
    ( "preserve_blank_lines_between_children" >:: fun ctxt ->
      (* Single blank line is preserved *)
      assert_expression_string ~ctxt ~pretty:true "<A>\n  <B />\n  \n  <C />\n</A>";
      (* Multiple blank lines are condensed to a single blank line *)
      assert_expression
        ~ctxt
        ~pretty:true
        "<A>\n  <B />\n  \n  <C />\n</A>"
        (expression_of_string "<A>\n  <B />\n  \n  \n  <C />\n</A>") );
  ]
