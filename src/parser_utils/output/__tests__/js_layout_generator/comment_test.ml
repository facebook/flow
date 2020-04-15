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
module L = Layout_builder

let tests =
  [
    ( "block" >:: fun ctxt ->
      let comment = Ast_builder.Comments.block "test" in
      let layout = Js_layout_generator.comment comment in
      assert_layout ~ctxt L.(loc (fused [atom "/*"; atom "test"; atom "*/"])) layout;
      assert_output ~ctxt "/*test*/" layout;
      assert_output ~ctxt ~pretty:true "/*test*/" layout );
    ( "line" >:: fun ctxt ->
      let comment = Ast_builder.Comments.line "test" in
      let layout = Js_layout_generator.comment comment in
      assert_layout ~ctxt L.(loc (fused [atom "//"; atom "test"; Layout.Newline])) layout;
      assert_output ~ctxt "//test\n" layout;
      assert_output ~ctxt ~pretty:true "//test\n" layout );
    ( "leading" >:: fun ctxt ->
      (* Line with single newline *)
      let ast = expression_of_string "//L\nA" in
      assert_expression ~ctxt "//L\nA" ast;
      assert_expression ~ctxt ~pretty:true "//L\nA" ast;
      (* Line with two newlines *)
      let ast = expression_of_string "//L\n\nA" in
      assert_expression ~ctxt "//L\nA" ast;
      assert_expression ~ctxt ~pretty:true "//L\n\nA" ast;
      (* Line with more than two newlines *)
      let ast = expression_of_string "//L\n\n\nA" in
      assert_expression ~ctxt "//L\nA" ast;
      assert_expression ~ctxt ~pretty:true "//L\n\nA" ast;
      (* Block with no newline *)
      let ast = expression_of_string "/*L*/A" in
      assert_expression ~ctxt "/*L*/A" ast;
      assert_expression ~ctxt ~pretty:true "/*L*/ A" ast;
      (* Block with single newline *)
      let ast = expression_of_string "/*L*/\nA" in
      assert_expression ~ctxt "/*L*/A" ast;
      assert_expression ~ctxt ~pretty:true "/*L*/\nA" ast;
      (* Block with two newlines *)
      let ast = expression_of_string "/*L*/\n\nA" in
      assert_expression ~ctxt "/*L*/A" ast;
      assert_expression ~ctxt ~pretty:true "/*L*/\n\nA" ast;
      (* Block with more than two newlines *)
      let ast = expression_of_string "/*L*/\n\n\nA" in
      assert_expression ~ctxt "/*L*/A" ast;
      assert_expression ~ctxt ~pretty:true "/*L*/\n\nA" ast;
      (* Multiple leading comments *)
      let ast = expression_of_string "//L1\n//L2\nA" in
      assert_expression ~ctxt "//L1\n//L2\nA" ast;
      assert_expression ~ctxt ~pretty:true "//L1\n//L2\nA" ast );
    ( "trailing" >:: fun ctxt ->
      (* After node with no newline *)
      let ast = expression_of_string "A//T\n" in
      assert_expression ~ctxt "A//T\n" ast;
      assert_expression ~ctxt ~pretty:true "A //T\n" ast;
      (* After node with single newline *)
      let ast = expression_of_string "A\n//T\n" in
      assert_expression ~ctxt "A//T\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n//T\n" ast;
      (* After node with two newlines *)
      let ast = expression_of_string "A\n\n//T\n" in
      assert_expression ~ctxt "A//T\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n\n//T\n" ast;
      (* After node with more than two newlines *)
      let ast = expression_of_string "A\n\n\n//T\n" in
      assert_expression ~ctxt "A//T\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n\n//T\n" ast;
      (* After line with single newline *)
      let ast = expression_of_string "A\n//T1\n//T2\n" in
      assert_expression ~ctxt "A//T1\n//T2\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n//T1\n//T2\n" ast;
      (* After line with two newlines *)
      let ast = expression_of_string "A\n//T1\n\n//T2\n" in
      assert_expression ~ctxt "A//T1\n//T2\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n//T1\n\n//T2\n" ast;
      (* After line with more than two newlines *)
      let ast = expression_of_string "A\n//T1\n\n\n//T2\n" in
      assert_expression ~ctxt "A//T1\n//T2\n" ast;
      assert_expression ~ctxt ~pretty:true "A\n//T1\n\n//T2\n" ast;
      (* After block with no newline *)
      let ast = expression_of_string "A\n/*T1*//*T2*/" in
      assert_expression ~ctxt "A/*T1*//*T2*/" ast;
      assert_expression ~ctxt ~pretty:true "A\n/*T1*/ /*T2*/" ast;
      (* After block with single newline *)
      let ast = expression_of_string "A\n/*T1*/\n/*T2*/" in
      assert_expression ~ctxt "A/*T1*//*T2*/" ast;
      assert_expression ~ctxt ~pretty:true "A\n/*T1*/\n/*T2*/" ast;
      (* After block with two newlines *)
      let ast = expression_of_string "A\n/*T1*/\n\n/*T2*/" in
      assert_expression ~ctxt "A/*T1*//*T2*/" ast;
      assert_expression ~ctxt ~pretty:true "A\n/*T1*/\n\n/*T2*/" ast;
      (* After block with more than two newlines *)
      let ast = expression_of_string "A\n/*T1*/\n\n\n/*T2*/" in
      assert_expression ~ctxt "A/*T1*//*T2*/" ast;
      assert_expression ~ctxt ~pretty:true "A\n/*T1*/\n\n/*T2*/" ast );
    ( "arrow_function_params" >:: fun ctxt ->
      assert_expression_string ~ctxt "/*L*/()/*T*/=>{}";
      assert_expression ~ctxt "/*L*/A/*T*/=>{}" (expression_of_string "/*L*/(A)/*T*/=>{}") );
    ("break" >:: fun ctxt -> assert_statement_string ~ctxt "break;/*T*/");
    ("class_private_field" >:: fun ctxt -> assert_expression_string ~ctxt "class C{/*L*/#A/*T*/;}");
    ("continue" >:: fun ctxt -> assert_statement_string ~ctxt "continue;/*T*/");
    ("debugger" >:: fun ctxt -> assert_statement_string ~ctxt "debugger;/*T*/");
    ("do_while" >:: fun ctxt -> assert_statement_string ~ctxt "do{}while(A);/*T*/");
    ( "enum" >:: fun ctxt ->
      assert_statement_string ~ctxt "enum E of boolean{A=/*L*/true/*T*/,}";
      assert_statement_string ~ctxt "enum E of number{A=/*L*/1/*T*/,}";
      assert_statement_string ~ctxt {|enum E of string{A=/*L*/"A"/*T*/,}|} );
    ( "function_params" >:: fun ctxt ->
      let ast = expression_of_string "function foo/*L*/()/*T*/\n{}" in
      assert_expression ~ctxt "function foo/*L*/()/*T*/{}" ast );
    ("literal" >:: fun ctxt -> assert_expression_string ~ctxt "//L\n1//T\n");
    ("tagged_template" >:: fun ctxt -> assert_expression_string ~ctxt "/*L1*/A/*L2*/`B`/*T*/");
    ( "member_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt "A./*L*/B/*T*/";
      assert_expression_string ~ctxt "A./*L*/#B/*T*/" );
    ("return" >:: fun ctxt -> assert_statement_string ~ctxt "return;/*T*/");
    ("throw" >:: fun ctxt -> assert_statement_string ~ctxt "throw A;/*T*/");
    ("variable_declaration" >:: fun ctxt -> assert_statement_string ~ctxt "let A=B;/*T*/");
  ]
