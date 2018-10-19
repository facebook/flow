(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

let tests = [
  "let_simple_assign" >:: begin fun ctxt ->
    let mk_layout a =
      Js_layout_generator.statement (
        S.let_declaration [
          S.variable_declarator a ~init:(E.identifier "a");
        ]
      )
    in

    let layout = mk_layout "a" in
    assert_layout ~ctxt
      L.(loc (fused [
        loc (fused [
          atom "let";
          space;
          loc (fused [
            loc (id "a");
            pretty_space;
            atom "=";
            pretty_space;
            loc (id "a");
          ]);
        ]);
        atom ";";
      ]))
      layout;
    assert_output ~ctxt "let a=a;" layout;
    assert_output ~ctxt ~pretty:true "let a = a;" layout;

    let a80 = String.make 80 'a' in
    let layout = mk_layout a80 in
    assert_output ~ctxt ("let "^a80^"=a;") layout;
    assert_output ~ctxt ~pretty:true
      ("let "^a80^" = a;")
      layout;
  end;

  "let_simple_object_assign" >:: begin fun ctxt ->
    let mk_layout a =
      Js_layout_generator.statement (
        S.let_declaration [
          S.variable_declarator_generic
            (P.object_ a)
            (Some (E.identifier "a"));
        ]
      )
    in

    let layout = mk_layout "a" in
    assert_layout ~ctxt
      L.(loc (fused [
        loc (fused [
          atom "let";
          pretty_space;
          loc (fused [
            loc (group [
              atom "{";
              indent ((fused [
                softline;
                loc (id "a");
              ]));
              softline;
              atom "}";
            ]);
            pretty_space;
            atom "=";
            pretty_space;
            loc (id "a");
          ]);
        ]);
        atom ";";
      ]))
      layout;
    assert_output ~ctxt "let{a}=a;" layout;
    assert_output ~ctxt ~pretty:true "let {a} = a;" layout;

    let a80 = String.make 80 'a' in
    let layout = mk_layout a80 in
    assert_output ~ctxt ("let{"^a80^"}=a;") layout;
    assert_output ~ctxt ~pretty:true
      ("let {\n  "^a80^"\n} = a;")
      layout;
  end;

  "let_optional_assign" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let a?=a;";
  end;

  "let_assign_annotation" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let a:b=a;";
  end;

  "let_optional_assign_annotation" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let a?:b=a;";
  end;

  "let_empty_object" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{}=a;";
  end;

  "let_empty_object_annotation" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{}:b=a;";
  end;

  "let_object_single_var" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a}=a;";
  end;

  "let_object_aliased_var" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a:b}=a;";
    assert_statement_string ~ctxt ~pretty:true "let {a: b} = a;";
  end;

  "let_object_multiple_vars" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a,b}=a;";
  end;

  "let_object_multiple_vars_aliased" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a:b,c}=a;";
    assert_statement_string ~ctxt ~pretty:true "let {a: b, c} = a;";
  end;

  "let_object_nested" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a,b:{c}}=a;";
  end;

  "let_object_default" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a=b}=a;";
  end;

  "let_object_aliased_default" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a:b=c}=a;";
  end;

  "let_object_alias_and_default" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a:b,c=d}=a;";
    assert_statement_string ~ctxt ~pretty:true "let {a: b, c = d} = a;";
  end;

  "let_object_alias_and_default_long" >:: begin fun ctxt ->
    let d80 = String.make 80 'd' in
    assert_statement_string ~ctxt ("let{a:b,c="^d80^"}=a;");
    assert_statement_string ~ctxt ~pretty:true (
      "let {\n  a: b,\n  c = " ^ d80 ^ "\n} = a;"
    );
  end;

  "let_object_default_expression" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a=++b}=a;";
  end;

  "let_object_rest" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{...a}=a;";
    assert_statement_string ~ctxt ~pretty:true "let {...a} = a;";
  end;

  "let_object_var_rest" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let{a,...b}=a;";
  end;

  "let_object_var_rest_long" >:: begin fun ctxt ->
    let c80 = String.make 80 'c' in
    assert_statement_string ~ctxt ("let{a,..." ^ c80 ^ "}=a;");
    assert_statement_string ~ctxt ~pretty:true (
      "let {\n  a,\n  ..." ^ c80 ^ "\n} = a;"
    );
  end;

  "let_array_empty" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[]=a;";
  end;

  "let_array_annotated" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[]:a=a;";
  end;

  "let_array_single_item">:: begin fun ctxt ->
    let mk_layout a =
      Js_layout_generator.statement (
        S.let_declaration [
          S.variable_declarator_generic
            (P.array [
              Some (P.identifier a);
            ])
            (Some (E.identifier "a"));
        ]
      )
    in
    let layout = mk_layout "a" in
    assert_layout ~ctxt
      L.(loc (fused [
        loc (fused [
          atom "let";
          pretty_space;
          loc (fused [
            loc (group [
              atom "[";
              indent ((fused [
                softline;
                loc (id "a");
              ]));
              softline;
              atom "]";
            ]);
            pretty_space;
            atom "=";
            pretty_space;
            loc (id "a");
          ]);
        ]);
        atom ";";
      ]))
      layout;
    assert_output ~ctxt "let[a]=a;" layout;
    assert_output ~ctxt ~pretty:true "let [a] = a;" layout;

    let a80 = String.make 80 'a' in
    let layout = mk_layout a80 in
    assert_output ~ctxt ("let["^a80^"]=a;") layout;
    assert_output ~ctxt ~pretty:true
      ("let [\n"^
       "  "^a80^"\n"^
       "] = a;")
       layout;
  end;

  "let_array_optional_item" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[a?]=a;";
  end;

  "let_array_annotated_item" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[a:b]=a;";
  end;

  "let_array_optional_annotated_item" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[a?:b]=a;";
  end;

  "let_array_multiple_items" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[a,b]=a;";
    assert_statement_string ~ctxt ~pretty:true "let [a, b] = a;";
  end;

  "let_array_holes" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[,,a]=a;";
    assert_statement_string ~ctxt ~pretty:true "let [a, , b] = a;";
    assert_statement_string ~ctxt ~pretty:true (
      "let [\n  a,\n  ,\n  " ^ String.make 80 'b' ^ "\n] = a;"
    );
  end;

  "let_nested_array" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[[]]=a;";
  end;

  "let_array_holes_and_nested" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[,,[a]]=a;";
  end;

  "let_array_spread" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[...a]=a;";
  end;

  "let_array_item_and_spread" >:: begin fun ctxt ->
    assert_statement_string ~ctxt "let[a,...b]=a;";
    assert_statement_string ~ctxt ~pretty:true "let [a, ...b] = a;";
    assert_statement_string ~ctxt ~pretty:true (
      "let [\n  a,\n  ...b" ^ String.make 80 'c' ^ "\n] = a;"
    );
  end;
]
