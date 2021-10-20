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
module P = Ast_builder.Patterns
module L = Layout_builder

let opts = Js_layout_generator.default_opts

let tests =
  [
    ( "let_simple_assign" >:: fun ctxt ->
      let mk_layout a =
        Js_layout_generator.statement
          ~opts
          (S.let_declaration [S.variable_declarator a ~init:(E.identifier "a")])
      in
      let layout = mk_layout "a" in
      assert_layout
        ~ctxt
        L.(
          loc
            (loc
               (fused
                  [
                    atom "let";
                    space;
                    loc (fused [loc (id "a"); pretty_space; atom "="; pretty_space; loc (id "a")]);
                    atom ";";
                  ]
               )
            )
        )
        layout;
      assert_output ~ctxt "let a=a;" layout;
      assert_output ~ctxt ~pretty:true "let a = a;" layout;

      let a80 = String.make 80 'a' in
      let layout = mk_layout a80 in
      assert_output ~ctxt ("let " ^ a80 ^ "=a;") layout;
      assert_output ~ctxt ~pretty:true ("let " ^ a80 ^ " = a;") layout
    );
    ( "let_simple_object_assign" >:: fun ctxt ->
      let mk_layout a =
        Js_layout_generator.statement
          ~opts
          (S.let_declaration [S.variable_declarator_generic (P.object_ a) (Some (E.identifier "a"))])
      in
      let layout = mk_layout "a" in
      assert_layout
        ~ctxt
        L.(
          loc
            (loc
               (fused
                  [
                    atom "let";
                    pretty_space;
                    loc
                      (fused
                         [
                           loc
                             (group
                                [
                                  atom "{";
                                  indent (fused [softline; loc (id "a")]);
                                  softline;
                                  atom "}";
                                ]
                             );
                           pretty_space;
                           atom "=";
                           pretty_space;
                           loc (id "a");
                         ]
                      );
                    atom ";";
                  ]
               )
            )
        )
        layout;
      assert_output ~ctxt "let{a}=a;" layout;
      assert_output ~ctxt ~pretty:true "let {a} = a;" layout;

      let a80 = String.make 80 'a' in
      let layout = mk_layout a80 in
      assert_output ~ctxt ("let{" ^ a80 ^ "}=a;") layout;
      assert_output ~ctxt ~pretty:true ("let {\n  " ^ a80 ^ "\n} = a;") layout
    );
    ("let_optional_assign" >:: fun ctxt -> assert_statement_string ~ctxt "let a?=a;");
    ("let_assign_annotation" >:: fun ctxt -> assert_statement_string ~ctxt "let a:b=a;");
    ("let_optional_assign_annotation" >:: fun ctxt -> assert_statement_string ~ctxt "let a?:b=a;");
    ("let_empty_object" >:: fun ctxt -> assert_statement_string ~ctxt "let{}=a;");
    ("let_empty_object_annotation" >:: fun ctxt -> assert_statement_string ~ctxt "let{}:b=a;");
    ("let_object_single_var" >:: fun ctxt -> assert_statement_string ~ctxt "let{a}=a;");
    ( "let_object_aliased_var" >:: fun ctxt ->
      assert_statement_string ~ctxt "let{a:b}=a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b} = a;"
    );
    ("let_object_multiple_vars" >:: fun ctxt -> assert_statement_string ~ctxt "let{a,b}=a;");
    ( "let_object_multiple_vars_aliased" >:: fun ctxt ->
      assert_statement_string ~ctxt "let{a:b,c}=a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b, c} = a;"
    );
    ("let_object_nested" >:: fun ctxt -> assert_statement_string ~ctxt "let{a,b:{c}}=a;");
    ("let_object_default" >:: fun ctxt -> assert_statement_string ~ctxt "let{a=b}=a;");
    ("let_object_aliased_default" >:: fun ctxt -> assert_statement_string ~ctxt "let{a:b=c}=a;");
    ( "let_object_alias_and_default" >:: fun ctxt ->
      assert_statement_string ~ctxt "let{a:b,c=d}=a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b, c = d} = a;"
    );
    ( "let_object_alias_and_default_long" >:: fun ctxt ->
      let d80 = String.make 80 'd' in
      assert_statement_string ~ctxt ("let{a:b,c=" ^ d80 ^ "}=a;");
      assert_statement_string ~ctxt ~pretty:true ("let {\n  a: b,\n  c = " ^ d80 ^ "\n} = a;")
    );
    ("let_object_default_expression" >:: fun ctxt -> assert_statement_string ~ctxt "let{a=++b}=a;");
    ( "let_object_rest" >:: fun ctxt ->
      assert_statement_string ~ctxt "let{...a}=a;";
      assert_statement_string ~ctxt ~pretty:true "let {...a} = a;"
    );
    ("let_object_var_rest" >:: fun ctxt -> assert_statement_string ~ctxt "let{a,...b}=a;");
    ( "let_object_var_rest_long" >:: fun ctxt ->
      let c80 = String.make 80 'c' in
      assert_statement_string ~ctxt ("let{a,..." ^ c80 ^ "}=a;");
      assert_statement_string ~ctxt ~pretty:true ("let {\n  a,\n  ..." ^ c80 ^ "\n} = a;")
    );
    ("let_array_empty" >:: fun ctxt -> assert_statement_string ~ctxt "let[]=a;");
    ("let_array_annotated" >:: fun ctxt -> assert_statement_string ~ctxt "let[]:a=a;");
    ( "let_array_single_item" >:: fun ctxt ->
      let mk_layout a =
        Js_layout_generator.statement
          ~opts
          (S.let_declaration
             [
               S.variable_declarator_generic
                 (P.array [Some (P.identifier a)])
                 (Some (E.identifier "a"));
             ]
          )
      in
      let layout = mk_layout "a" in
      assert_layout
        ~ctxt
        L.(
          loc
            (loc
               (fused
                  [
                    atom "let";
                    pretty_space;
                    loc
                      (fused
                         [
                           loc
                             (group
                                [
                                  atom "[";
                                  indent (fused [softline; loc (loc (id "a"))]);
                                  softline;
                                  atom "]";
                                ]
                             );
                           pretty_space;
                           atom "=";
                           pretty_space;
                           loc (id "a");
                         ]
                      );
                    atom ";";
                  ]
               )
            )
        )
        layout;
      assert_output ~ctxt "let[a]=a;" layout;
      assert_output ~ctxt ~pretty:true "let [a] = a;" layout;

      let a80 = String.make 80 'a' in
      let layout = mk_layout a80 in
      assert_output ~ctxt ("let[" ^ a80 ^ "]=a;") layout;
      assert_output ~ctxt ~pretty:true ("let [\n" ^ "  " ^ a80 ^ "\n" ^ "] = a;") layout
    );
    ("let_array_optional_item" >:: fun ctxt -> assert_statement_string ~ctxt "let[a?]=a;");
    ("let_array_annotated_item" >:: fun ctxt -> assert_statement_string ~ctxt "let[a:b]=a;");
    ( "let_array_optional_annotated_item" >:: fun ctxt ->
      assert_statement_string ~ctxt "let[a?:b]=a;"
    );
    ( "let_array_multiple_items" >:: fun ctxt ->
      assert_statement_string ~ctxt "let[a,b]=a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, b] = a;"
    );
    ( "let_array_holes" >:: fun ctxt ->
      assert_statement_string ~ctxt "let[,,a]=a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, , b] = a;";
      assert_statement_string
        ~ctxt
        ~pretty:true
        ("let [\n  a,\n  ,\n  " ^ String.make 80 'b' ^ "\n] = a;")
    );
    ("let_nested_array" >:: fun ctxt -> assert_statement_string ~ctxt "let[[]]=a;");
    ("let_array_holes_and_nested" >:: fun ctxt -> assert_statement_string ~ctxt "let[,,[a]]=a;");
    ("let_array_spread" >:: fun ctxt -> assert_statement_string ~ctxt "let[...a]=a;");
    ( "let_array_item_and_spread" >:: fun ctxt ->
      assert_statement_string ~ctxt "let[a,...b]=a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, ...b] = a;";
      assert_statement_string
        ~ctxt
        ~pretty:true
        ("let [\n  a,\n  ...b" ^ String.make 80 'c' ^ "\n] = a;")
    );
  ]
