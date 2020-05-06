(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_test_utils
open Layout_generator_test_utils
module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module L = Layout_builder

let opts = Js_layout_generator.default_opts

let expected_object2_layout prop1 prop2 =
  let prop1_layout = Js_layout_generator.object_property ~opts prop1 in
  let prop2_layout = Js_layout_generator.object_property ~opts prop2 in
  L.(
    loc
      (group
         [
           atom "{";
           indent
             (fused
                [
                  pretty_line;
                  prop1_layout;
                  atom ",";
                  pretty_line;
                  prop2_layout;
                  Layout.IfBreak (atom ",", empty);
                ]);
           pretty_line;
           atom "}";
         ]))

let tests =
  [
    ( "empty_object" >:: fun ctxt ->
      let ast = E.object_ [] in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout ~ctxt L.(loc (group [atom "{"; atom "}"])) layout;
      assert_output ~ctxt "{}" layout;
      assert_output ~ctxt ~pretty:true "{}" layout );
    (* `{ foo: x, bar: y }` rather than `{foo: x, bar: y}` *)
    ( "flat_spaces_inside_braces" >:: fun ctxt ->
      let prop1 = E.object_property (E.object_property_key "foo") (E.identifier "x") in
      let prop2 = E.object_property (E.object_property_key "bar") (E.identifier "y") in
      let ast = E.object_ [prop1; prop2] in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout ~ctxt (expected_object2_layout prop1 prop2) layout;
      assert_output ~ctxt "{foo:x,bar:y}" layout;
      assert_output ~ctxt ~pretty:true "{ foo: x, bar: y }" layout );
    (* if it wraps, there's a trailing comma *)
    ( "newlines_and_trailing_comma" >:: fun ctxt ->
      let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" in
      let prop1 = E.object_property (E.object_property_key "foo") (E.identifier x40) in
      let prop2 = E.object_property (E.object_property_key "bar") (E.identifier x40) in
      let ast = E.object_ [prop1; prop2] in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout ~ctxt (expected_object2_layout prop1 prop2) layout;
      assert_output ~ctxt ("{foo:" ^ x40 ^ ",bar:" ^ x40 ^ "}") layout;
      assert_output ~ctxt ~pretty:true ("{\n  foo: " ^ x40 ^ ",\n  bar: " ^ x40 ^ ",\n}") layout );
    ( "object_property_is_function" >:: fun ctxt ->
      let prop1 = E.object_property (E.object_property_key "foo") (E.identifier "x") in
      let prop2 = E.object_property (E.object_property_key "bar") (E.function_ ()) in
      let prop3 = E.object_property (E.object_property_key "baz") (E.identifier "y") in
      let ast = E.object_ [prop1; prop2; prop3] in
      let layout = Js_layout_generator.expression ~opts ast in
      let prop1_layout = Js_layout_generator.object_property ~opts prop1 in
      let prop2_layout = Js_layout_generator.object_property ~opts prop2 in
      let prop3_layout = Js_layout_generator.object_property ~opts prop3 in
      assert_layout
        ~ctxt
        L.(
          loc
            (group
               [
                 atom "{";
                 indent
                   (fused
                      [
                        pretty_line;
                        prop1_layout;
                        atom ",";
                        pretty_line;
                        prop2_layout;
                        atom ",";
                        pretty_line;
                        prop3_layout;
                        Layout.IfBreak (atom ",", empty);
                      ]);
                 pretty_line;
                 atom "}";
               ]))
        layout;
      assert_output ~ctxt "{foo:x,bar:function(){},baz:y}" layout;
      assert_output ~ctxt ~pretty:true "{ foo: x, bar: function() {}, baz: y }" layout );
    ( "object_property_is_method" >:: fun ctxt ->
      let layout =
        Js_layout_generator.expression
          ~opts
          (E.object_ [E.object_method (E.object_property_key "foo")])
      in
      assert_output ~ctxt "{foo(){}}" layout;
      assert_output ~ctxt ~pretty:true "{ foo() {} }" layout );
    ( "object_property_is_generator_method" >:: fun ctxt ->
      let layout =
        Js_layout_generator.expression
          ~opts
          (E.object_ [E.object_method ~generator:true (E.object_property_key "foo")])
      in
      assert_output ~ctxt "{*foo(){}}" layout;
      assert_output ~ctxt ~pretty:true "{ *foo() {} }" layout );
    ( "object_property_is_sequence" >:: fun ctxt ->
      let layout =
        Js_layout_generator.expression
          ~opts
          (E.object_
             [
               E.object_property
                 (E.object_property_key "foo")
                 (E.sequence [E.identifier "x"; E.identifier "y"]);
             ])
      in
      assert_output ~ctxt "{foo:(x,y)}" layout;
      assert_output ~ctxt ~pretty:true "{ foo: (x, y) }" layout );
    ( "object_property_key_is_literal" >:: fun ctxt ->
      let layout =
        Js_layout_generator.expression
          ~opts
          (E.object_
             [
               E.object_property_with_literal
                 (Ast_builder.Literals.string "foo")
                 (E.literal (Ast_builder.Literals.string "bar"));
             ])
      in
      assert_output ~ctxt ~msg:"string literal keys should be quoted" "{\"foo\":\"bar\"}" layout;
      assert_output
        ~ctxt
        ~msg:"string literal keys should be quoted"
        ~pretty:true
        "{ \"foo\": \"bar\" }"
        layout );
    ( "object_property_key_is_computed" >:: fun ctxt ->
      let b80 = String.make 80 'b' in
      let ast =
        E.object_
          [
            E.object_property
              (E.object_property_computed_key (E.identifier b80))
              (E.literal (Ast_builder.Literals.number 123. "123"));
          ]
      in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout
        ~ctxt
        L.(
          loc
            (group
               [
                 atom "{";
                 indent
                   (fused
                      [
                        pretty_line;
                        loc
                          (group
                             [
                               atom "[";
                               indent (fused [pretty_line; loc (id b80)]);
                               pretty_line;
                               atom "]";
                               atom ":";
                               pretty_space;
                               loc (atom "123");
                             ]);
                        Layout.IfBreak (atom ",", empty);
                      ]);
                 pretty_line;
                 atom "}";
               ]))
        layout;
      assert_output ~ctxt ("{[" ^ b80 ^ "]:123}") layout;
      assert_output ~ctxt ~pretty:true ("{\n  [\n    " ^ b80 ^ "\n  ]: 123,\n}") layout;

      let b40 = String.make 40 'b' in
      let ast =
        E.object_
          [
            E.object_property
              (E.object_property_computed_key
                 (E.binary
                    ~op:Flow_ast.Expression.Binary.Plus
                    (E.identifier b40)
                    (E.identifier b40)))
              (E.literal (Ast_builder.Literals.number 123. "123"));
          ]
      in
      let layout = Js_layout_generator.expression ~opts ast in
      assert_layout
        ~ctxt
        L.(
          loc
            (group
               [
                 atom "{";
                 indent
                   (fused
                      [
                        pretty_line;
                        loc
                          (group
                             [
                               atom "[";
                               indent
                                 (fused
                                    [
                                      pretty_line;
                                      loc
                                        (fused
                                           [
                                             loc (id b40);
                                             pretty_space;
                                             atom "+";
                                             pretty_space;
                                             loc (id b40);
                                           ]);
                                    ]);
                               pretty_line;
                               atom "]";
                               atom ":";
                               pretty_space;
                               loc (atom "123");
                             ]);
                        Layout.IfBreak (atom ",", empty);
                      ]);
                 pretty_line;
                 atom "}";
               ]))
        layout;
      assert_output ~ctxt ("{[" ^ b40 ^ "+" ^ b40 ^ "]:123}") layout;

      (* TODO: the second b40 should wrap *)
      assert_output
        ~ctxt
        ~pretty:true
        ("{\n  [\n    " ^ b40 ^ " + " ^ b40 ^ "\n  ]: 123,\n}")
        layout );
    ( "preserve_blank_lines_between_properties" >:: fun ctxt ->
      (* Single blank line is preserved *)
      assert_expression_string ~ctxt ~pretty:true "{\n  a: 1,\n  \n  b: 2,\n}";
      (* Multiple blank lines are condensed to a single blank line *)
      assert_expression
        ~ctxt
        ~pretty:true
        "{\n  a: 1,\n  \n  b: 2,\n}"
        (Ast_builder.expression_of_string "{\n  a: 1,\n  \n  \n b: 2,\n}");
      (* Comments are not treated as blank lines *)
      assert_expression_string ~ctxt ~pretty:true "{\n  a: 1,\n  //L\n  b: 2,\n}" );
    ( "preserve_wrapping" >:: fun ctxt ->
      (* Object that fits on single line with no wrapping is printed on single line *)
      assert_expression_string ~ctxt ~pretty:true "{ a: 1 }";
      (* Object that fits on single line but wraps is printed as wrapping *)
      assert_expression_string ~ctxt ~pretty:true "{\n  a: 1,\n}" );
    ( "bracket_spacing" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "{ a: 1 }";
      assert_expression_string
        ~ctxt
        ~pretty:true
        ~opts:Js_layout_generator.{ default_opts with bracket_spacing = false }
        "{a: 1}" );
  ]
