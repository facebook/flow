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
    ( "statements_separated_by_comments" >:: fun ctxt ->
      assert_program_string ~ctxt ~pretty:true "A;\n//L\nB;";
      assert_program_string ~ctxt ~pretty:true "A;\n/*L1*/\n/*L2*/\nB;";
      assert_program_string ~ctxt ~pretty:true "A; //L\nB;";
      assert_program_string ~ctxt ~pretty:true "A; /*T1\nT2*/\nB;" );
    ( "assignment_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "A = //L\nB";
      assert_expression_string ~ctxt ~pretty:true "A =\n//L\nB" );
    ( "array" >:: fun ctxt ->
      assert_expression_string ~ctxt "[/*I*/]";
      assert_expression_string ~ctxt ~pretty:true "[\n  a,\n  /*I*/\n]";
      assert_expression_string ~ctxt ~pretty:true "[\n  a,\n  \n  /*I*/\n]";
      assert_expression_string ~ctxt ~pretty:true "[\n  a //T\n  ,\n  \n  //L\n  b,\n]" );
    ( "array_pattern" >:: fun ctxt ->
      assert_statement_string ~ctxt "var[/*I*/];";
      assert_statement_string ~ctxt ~pretty:true "var [\n  a\n  /*I*/\n];";
      assert_statement_string ~ctxt ~pretty:true "var [\n  a\n  \n  /*I*/\n];" );
    ( "arrow_function_body" >:: fun ctxt ->
      (* Body without leading comment separated by space *)
      assert_expression_string ~ctxt ~pretty:true "() => <A />";
      (* Body with leading comment separated by newline *)
      assert_expression_string ~ctxt ~pretty:true "() =>\n//L\n<A />" );
    ( "arrow_function_params" >:: fun ctxt ->
      assert_expression_string ~ctxt "/*L*/()/*T*/=>{}";
      assert_expression ~ctxt "/*L*/A/*T*/=>{}" (expression_of_string "/*L*/(A)/*T*/=>{}");
      assert_expression_string ~ctxt "(/*L*/A/*T*/)=>{}";
      assert_expression_string ~ctxt "//L\nA=>{}" );
    ("block" >:: fun ctxt -> assert_statement_string ~ctxt "{/*I*/}");
    ("break" >:: fun ctxt -> assert_statement_string ~ctxt "break;/*T*/");
    ( "binary_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "a + //L\nb";
      assert_expression_string ~ctxt ~pretty:true "a + //L\n+b";
      assert_expression_string ~ctxt ~pretty:true "a + \n//L\nb";
      assert_expression_string ~ctxt ~pretty:true "a + \n//L\n+b" );
    ( "call" >:: fun ctxt ->
      let a80 = String.make 80 'a' in
      assert_expression_string ~ctxt "foo(/*I*/)";
      assert_expression_string ~ctxt ~pretty:true ("foo(\n  " ^ a80 ^ ",\n  /*I*/\n)");
      assert_expression_string ~ctxt ~pretty:true "foo(\n  a,\n  \n  /*I*/\n)" );
    ( "call_type_args" >:: fun ctxt ->
      let a80 = String.make 80 'a' in
      assert_expression_string ~ctxt "foo</*I*/>()";
      assert_expression_string ~ctxt ~pretty:true ("foo<\n  " ^ a80 ^ ",\n  /*I*/\n>()");
      assert_expression_string ~ctxt ~pretty:true "foo<\n  a,\n  \n  /*I*/\n>()" );
    ("class_private_field" >:: fun ctxt -> assert_expression_string ~ctxt "class C{/*L*/#A/*T*/;}");
    ("continue" >:: fun ctxt -> assert_statement_string ~ctxt "continue;/*T*/");
    ("debugger" >:: fun ctxt -> assert_statement_string ~ctxt "debugger;/*T*/");
    ("declare_module" >:: fun ctxt -> assert_statement_string ~ctxt "declare module A{/*I*/}");
    ("do_while" >:: fun ctxt -> assert_statement_string ~ctxt "do{}while(A);/*T*/");
    ( "enum" >:: fun ctxt ->
      assert_statement_string ~ctxt "enum E of boolean{A=/*L*/true/*T*/,}";
      assert_statement_string ~ctxt "enum E of number{A=/*L*/1/*T*/,}";
      assert_statement_string ~ctxt {|enum E of string{A=/*L*/"A"/*T*/,}|} );
    ("function_body" >:: fun ctxt -> assert_statement_string ~ctxt "function foo(){/*I*/}");
    ( "function_params" >:: fun ctxt ->
      let ast = expression_of_string "function foo/*L*/()/*T*/\n{}" in
      assert_expression ~ctxt "function foo/*L*/()/*T*/{}" ast;
      assert_expression_string ~ctxt "(/*I*/)=>{}";
      assert_expression_string ~ctxt ~pretty:true "(\n  a,\n  /*I*/\n) => {}";
      assert_expression_string ~ctxt ~pretty:true "(\n  a,\n  \n  /*I*/\n) => {}" );
    ( "function_type_params" >:: fun ctxt ->
      assert_statement_string ~ctxt "type T=(/*I*/)=>a;";
      assert_statement_string ~ctxt ~pretty:true "type T = (\n  a\n  /*I*/\n) => b;";
      assert_statement_string ~ctxt ~pretty:true "type T = (\n  a\n  \n  /*I*/\n) => b;" );
    ( "if_statement" >:: fun ctxt ->
      assert_statement_string ~ctxt ~pretty:true "if (true) {} //L\n else {}";
      assert_statement_string ~ctxt ~pretty:true "if (true) {}\n//L\nelse {}" );
    ("jsx_expression_container" >:: fun ctxt -> assert_expression_string ~ctxt "<A>{/*I*/}</A>");
    ("literal" >:: fun ctxt -> assert_expression_string ~ctxt "//L\n1//T\n");
    ( "logical_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "a && //L\n  b";
      assert_expression_string ~ctxt ~pretty:true "a &&\n  //L\n  b";
      assert_expression_string ~ctxt ~pretty:true "(\n  //L\n  a &&\n    b\n) ??\n  c" );
    ("tagged_template" >:: fun ctxt -> assert_expression_string ~ctxt "/*L1*/A/*L2*/`B`/*T*/");
    ( "member_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt "A./*L*/B/*T*/";
      assert_expression_string ~ctxt "A./*L*/#B/*T*/";
      assert_expression_string ~ctxt ~pretty:true "foo //C\n.bar";
      assert_expression_string ~ctxt ~pretty:true "foo /*C*/\n.bar";
      assert_expression_string ~ctxt ~pretty:true "foo /*C*/.bar";
      assert_expression_string ~ctxt ~pretty:true "foo\n//C\n.bar";
      assert_expression_string ~ctxt ~pretty:true "foo\n/*C*/\n.bar";
      assert_expression_string ~ctxt ~pretty:true "foo\n/*C*/.bar" );
    ( "new" >:: fun ctxt ->
      let a80 = String.make 80 'a' in
      assert_expression_string ~ctxt "new Foo(/*I*/)";
      assert_expression_string ~ctxt ~pretty:true ("new Foo(\n  " ^ a80 ^ ",\n  /*I*/\n)");
      assert_expression_string ~ctxt ~pretty:true "new Foo(\n  a,\n  \n  /*I*/\n)";
      assert_expression_string ~ctxt ~pretty:true "new (\n  //L\n  A ||\n    B\n)" );
    ( "object" >:: fun ctxt ->
      assert_expression_string ~ctxt "{/*I*/}";
      assert_expression_string ~ctxt ~pretty:true "{\n  a,\n  /*I*/\n}";
      assert_expression_string ~ctxt ~pretty:true "{\n  a,\n  \n  /*I*/\n}";
      assert_expression_string ~ctxt ~pretty:true "{\n  a: //L\n  b,\n}";
      assert_expression_string ~ctxt ~pretty:true "{\n  a:\n  //L\n  b,\n}" );
    ( "object_pattern" >:: fun ctxt ->
      assert_statement_string ~ctxt "var{/*I*/};";
      assert_statement_string ~ctxt "var{a,/*I*/};" );
    ( "object_type" >:: fun ctxt ->
      assert_statement_string ~ctxt "type T={/*I*/};";
      assert_statement_string ~ctxt "type T={a:any,/*I*/};";
      assert_statement_string
        ~ctxt
        ~pretty:true
        "type T = {\n  a: any,\n  /*I1*/\n  /*I2*/\n  ...,\n};";
      assert_statement
        ~ctxt
        ~pretty:true
        "type T = {\n  a: any,\n  \n  /*I1*/\n  /*I2*/\n  ...,\n};"
        (statement_of_string "type T = {\n  a: any,\n  ...,\n  /*I1*/\n  /*I2*/\n};");
      assert_statement_string
        ~ctxt
        ~pretty:true
        "type T = {\n  a: any,\n  /*I1*/\n  \n  /*I2*/\n  ...,\n};";
      (* Leading comments on variance nodes are included in comment bounds of property *)
      assert_statement_string ~ctxt ~pretty:true "type T = {\n  +a: any,\n  //L\n  +b: any,\n};" );
    ( "parenthesized_expression" >:: fun ctxt ->
      assert_expression_string ~ctxt ~pretty:true "(\n  //L\n  a + b\n) * c" );
    ("return" >:: fun ctxt -> assert_statement_string ~ctxt "return;/*T*/");
    ( "switch_case" >:: fun ctxt ->
      assert_statement_string ~ctxt ~pretty:true "switch (x) {\n  case 1: /*T*/\n    break;\n}" );
    ("throw" >:: fun ctxt -> assert_statement_string ~ctxt "throw A;/*T*/");
    ( "type_alias" >:: fun ctxt ->
      assert_statement_string ~ctxt ~pretty:true "type A = //L\nB;";
      assert_statement_string ~ctxt ~pretty:true "type A =\n//L\nB;" );
    ( "type_args" >:: fun ctxt ->
      let a80 = String.make 80 'a' in
      assert_statement_string ~ctxt "type Foo=Bar</*I*/>;";
      assert_statement_string ~ctxt ~pretty:true ("type Foo = Bar<\n  " ^ a80 ^ ",\n  /*I*/\n>;");
      assert_statement_string ~ctxt ~pretty:true "type Foo = Bar<\n  a,\n  \n  /*I*/\n>;" );
    ( "union_type" >:: fun ctxt ->
      let b80 = String.make 80 'b' in
      assert_statement_string ~ctxt ~pretty:true ("type Foo =\n//L\n| a\n  | " ^ b80 ^ ";");
      assert_statement_string ~ctxt ~pretty:true ("type Foo = \n  | a\n  | //L\n  " ^ b80 ^ ";");
      assert_statement_string ~ctxt ~pretty:true ("type Foo = \n  | a\n  |\n  //L\n  " ^ b80 ^ ";")
    );
    ( "variable_declaration" >:: fun ctxt ->
      assert_statement_string ~ctxt "let A=B;/*T*/";
      assert_statement_string ~ctxt ~pretty:true "let A = //L\nB;";
      assert_statement_string ~ctxt ~pretty:true "let A =\n//L\nB;" );
  ]
