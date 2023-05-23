(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_test_utils
open Layout_generator_test_utils
module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module T = Ast_builder.Types
module L = Layout_builder

let opts = Js_layout_generator.default_opts

let variable_declaration =
  S.const_declaration
    [S.variable_declarator ~init:(E.literal (Ast_builder.Literals.string "x")) "x"]

let void_annotation = Flow_ast.Type.Available (T.annotation (T.void ()))

let type_params = T.type_params [T.type_param "T"]

let comments =
  {
    Flow_ast.Syntax.leading = [Ast_builder.Comments.block "leading"];
    trailing = [Ast_builder.Comments.line "trailing"];
    internal = ();
  }

let tests =
  [
    ( "simple_component" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let ast = S.component_declaration "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(){}" layout;
      assert_output ~ctxt ~pretty:true "component Comp() {}" layout
    );
    ( "component_with_body" >:: fun ctxt ->
      let body =
        (Loc.none, { Flow_ast.Statement.Block.body = [variable_declaration]; comments = None })
      in
      let ast = S.component_declaration "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(){const x=\"x\"}" layout;
      assert_output ~ctxt ~pretty:true "component Comp() {\n  const x = \"x\";\n}" layout
    );
    ( "component_with_return" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let ast = S.component_declaration ~tparams:type_params "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp<T>(){}" layout;
      assert_output ~ctxt ~pretty:true "component Comp<T>() {}" layout
    );
    ( "component_with_comments" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let ast = S.component_declaration ~comments "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "/*leading*/component Comp(){}//trailing\n" layout;
      assert_output ~ctxt ~pretty:true "/*leading*/ component Comp() {} //trailing\n" layout
    );
    ( "component_with_simple_params" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let params =
        S.component_params
          [
            S.component_id_param "p1";
            S.component_string_param "p-2" (Ast_builder.Patterns.identifier "p2");
          ]
      in
      let ast = S.component_declaration ~params "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(p1,\"p-2\" as p2){}" layout;
      assert_output ~ctxt ~pretty:true "component Comp(p1, \"p-2\" as p2) {}" layout
    );
    ( "component_params_with_defaults" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let params =
        S.component_params
          [
            S.component_id_param ~default:(E.Literals.string "default_p1") "p1";
            S.component_string_param
              ~default:(E.Literals.string "default_p2")
              "p-2"
              (Ast_builder.Patterns.identifier "p2");
          ]
      in
      let ast = S.component_declaration ~params "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(p1=\"default_p1\",\"p-2\" as p2=\"default_p2\"){}" layout;
      assert_output
        ~ctxt
        ~pretty:true
        "component Comp(p1 = \"default_p1\", \"p-2\" as p2 = \"default_p2\") {}"
        layout
    );
    ( "component_params_id_with_local" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let params =
        S.component_params
          [S.component_id_param ~local:(Ast_builder.Patterns.identifier "local_p1") "p1"]
      in
      let ast = S.component_declaration ~params "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(p1 as local_p1){}" layout;
      assert_output ~ctxt ~pretty:true "component Comp(p1 as local_p1) {}" layout
    );
    ( "component_params_id_with_rest" >:: fun ctxt ->
      let body = (Loc.none, { Flow_ast.Statement.Block.body = []; comments = None }) in
      let params =
        S.component_params
          ~rest:
            ( Loc.none,
              {
                Flow_ast.Statement.ComponentDeclaration.RestParam.argument =
                  Ast_builder.Patterns.identifier "rest";
                comments = None;
              }
            )
          [S.component_id_param "p1"]
      in
      let ast = S.component_declaration ~params "Comp" body in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "component Comp(p1,...rest){}" layout;
      assert_output ~ctxt ~pretty:true "component Comp(p1, ...rest) {}" layout
    );
    ( "declare_component" >:: fun ctxt ->
      let ast = S.declare_component "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp();" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp();" layout
    );
    ( "declare_component_with_return_annot" >:: fun ctxt ->
      let return = Some (Flow_ast.Type.Available (T.annotation @@ T.mixed ())) in
      let ast = S.declare_component ?return "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp():mixed;" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp(): mixed;" layout
    );
    ( "declare_component_with_comments" >:: fun ctxt ->
      let ast = S.declare_component ~comments "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "/*leading*/declare component Comp();//trailing\n" layout;
      assert_output ~ctxt ~pretty:true "/*leading*/ declare component Comp(); //trailing\n" layout
    );
    ( "declare_component_with_simple_params" >:: fun ctxt ->
      let open Flow_ast.Statement.ComponentDeclaration.Param in
      let params =
        S.component_type_params
          [
            S.component_type_param
              (Identifier (Ast_builder.Identifiers.identifier "p1"))
              (T.annotation @@ T.number ());
            S.component_type_param
              (StringLiteral (Loc.none, Ast_builder.string_literal "p-2"))
              (T.annotation @@ T.void ());
          ]
      in
      let ast = S.declare_component ~params "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp(p1:number,\"p-2\":void);" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp(p1: number, \"p-2\": void);" layout
    );
    ( "declare_component_with_rest" >:: fun ctxt ->
      let params =
        S.component_type_params
          ~rest:
            ( Loc.none,
              {
                Flow_ast.Type.Component.RestParam.argument =
                  Some (Ast_builder.Identifiers.identifier "rest");
                optional = false;
                annot = T.mixed ();
                comments = None;
              }
            )
          []
      in
      let ast = S.declare_component ~params "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp(...rest:mixed);" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp(...rest: mixed);" layout
    );
    ( "declare_component_with_optional_rest" >:: fun ctxt ->
      let params =
        S.component_type_params
          ~rest:
            ( Loc.none,
              {
                Flow_ast.Type.Component.RestParam.argument =
                  Some (Ast_builder.Identifiers.identifier "rest");
                optional = true;
                annot = T.mixed ();
                comments = None;
              }
            )
          []
      in
      let ast = S.declare_component ~params "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp(...rest?:mixed);" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp(...rest?: mixed);" layout
    );
    ( "declare_component_with_param_and_rest" >:: fun ctxt ->
      let open Flow_ast.Statement.ComponentDeclaration.Param in
      let params =
        S.component_type_params
          ~rest:
            ( Loc.none,
              {
                Flow_ast.Type.Component.RestParam.argument = None;
                optional = false;
                annot = T.mixed ();
                comments = None;
              }
            )
          [
            S.component_type_param
              (Identifier (Ast_builder.Identifiers.identifier "p1"))
              (T.annotation @@ T.number ());
          ]
      in
      let ast = S.declare_component ~params "Comp" in
      let layout = Js_layout_generator.statement ~opts ast in
      assert_output ~ctxt "declare component Comp(p1:number,...mixed);" layout;
      assert_output ~ctxt ~pretty:true "declare component Comp(p1: number, ...mixed);" layout
    );
  ]
