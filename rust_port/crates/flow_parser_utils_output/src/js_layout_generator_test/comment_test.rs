/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn block() {
    let comment = ast_builder::comments::block(None, None, "test");
    let layout = js_layout_generator::comment(false, &comment);
    assert_layout(
        L::loc(
            None,
            L::fused(vec![L::atom("/*"), L::atom("test"), L::atom("*/")]),
        ),
        layout.clone(),
    );
    assert_output(false, "/*test*/", &layout);
    assert_output(true, "/*test*/", &layout);
}

#[test]
fn line() {
    let comment = ast_builder::comments::line(None, None, "test");
    let layout = js_layout_generator::comment(false, &comment);
    assert_layout(
        L::loc(
            None,
            L::fused(vec![L::atom("//"), L::atom("test"), LayoutNode::newline()]),
        ),
        layout.clone(),
    );
    assert_output(false, "//test\n", &layout);
    assert_output(true, "//test\n", &layout);
}

#[test]
fn leading() {
    // Line with single newline
    let ast = ast_builder::test_expression_of_string("//L\nA");
    assert_expression(false, None, None, "//L\nA", &ast);
    assert_expression(true, None, None, "//L\nA", &ast);
    // Line with two newlines
    let ast = ast_builder::test_expression_of_string("//L\n\nA");
    assert_expression(false, None, None, "//L\nA", &ast);
    assert_expression(true, None, None, "//L\n\nA", &ast);
    // Line with more than two newlines
    let ast = ast_builder::test_expression_of_string("//L\n\n\nA");
    assert_expression(false, None, None, "//L\nA", &ast);
    assert_expression(true, None, None, "//L\n\nA", &ast);
    // Block with no newline
    let ast = ast_builder::test_expression_of_string("/*L*/A");
    assert_expression(false, None, None, "/*L*/A", &ast);
    assert_expression(true, None, None, "/*L*/ A", &ast);
    // Block with single newline
    let ast = ast_builder::test_expression_of_string("/*L*/\nA");
    assert_expression(false, None, None, "/*L*/A", &ast);
    assert_expression(true, None, None, "/*L*/\nA", &ast);
    // Block with two newlines
    let ast = ast_builder::test_expression_of_string("/*L*/\n\nA");
    assert_expression(false, None, None, "/*L*/A", &ast);
    assert_expression(true, None, None, "/*L*/\n\nA", &ast);
    // Block with more than two newlines
    let ast = ast_builder::test_expression_of_string("/*L*/\n\n\nA");
    assert_expression(false, None, None, "/*L*/A", &ast);
    assert_expression(true, None, None, "/*L*/\n\nA", &ast);
    // Multiple leading comments
    let ast = ast_builder::test_expression_of_string("//L1\n//L2\nA");
    assert_expression(false, None, None, "//L1\n//L2\nA", &ast);
    assert_expression(true, None, None, "//L1\n//L2\nA", &ast);
}

#[test]
fn trailing() {
    // After node with no newline
    let ast = ast_builder::test_expression_of_string("A//T\n");
    assert_expression(false, None, None, "A//T\n", &ast);
    assert_expression(true, None, None, "A //T\n", &ast);
    // After node with single newline
    let ast = ast_builder::test_expression_of_string("A\n//T\n");
    assert_expression(false, None, None, "A//T\n", &ast);
    assert_expression(true, None, None, "A\n//T\n", &ast);
    // After node with two newlines
    let ast = ast_builder::test_expression_of_string("A\n\n//T\n");
    assert_expression(false, None, None, "A//T\n", &ast);
    assert_expression(true, None, None, "A\n\n//T\n", &ast);
    // After node with more than two newlines
    let ast = ast_builder::test_expression_of_string("A\n\n\n//T\n");
    assert_expression(false, None, None, "A//T\n", &ast);
    assert_expression(true, None, None, "A\n\n//T\n", &ast);
    // After line with single newline
    let ast = ast_builder::test_expression_of_string("A\n//T1\n//T2\n");
    assert_expression(false, None, None, "A//T1\n//T2\n", &ast);
    assert_expression(true, None, None, "A\n//T1\n//T2\n", &ast);
    // After line with two newlines
    let ast = ast_builder::test_expression_of_string("A\n//T1\n\n//T2\n");
    assert_expression(false, None, None, "A//T1\n//T2\n", &ast);
    assert_expression(true, None, None, "A\n//T1\n\n//T2\n", &ast);
    // After line with more than two newlines
    let ast = ast_builder::test_expression_of_string("A\n//T1\n\n\n//T2\n");
    assert_expression(false, None, None, "A//T1\n//T2\n", &ast);
    assert_expression(true, None, None, "A\n//T1\n\n//T2\n", &ast);
    // After block with no newline
    let ast = ast_builder::test_expression_of_string("A\n/*T1*//*T2*/");
    assert_expression(false, None, None, "A/*T1*//*T2*/", &ast);
    assert_expression(true, None, None, "A\n/*T1*/ /*T2*/", &ast);
    // After block with single newline
    let ast = ast_builder::test_expression_of_string("A\n/*T1*/\n/*T2*/");
    assert_expression(false, None, None, "A/*T1*//*T2*/", &ast);
    assert_expression(true, None, None, "A\n/*T1*/\n/*T2*/", &ast);
    // After block with two newlines
    let ast = ast_builder::test_expression_of_string("A\n/*T1*/\n\n/*T2*/");
    assert_expression(false, None, None, "A/*T1*//*T2*/", &ast);
    assert_expression(true, None, None, "A\n/*T1*/\n\n/*T2*/", &ast);
    // After block with more than two newlines
    let ast = ast_builder::test_expression_of_string("A\n/*T1*/\n\n\n/*T2*/");
    assert_expression(false, None, None, "A/*T1*//*T2*/", &ast);
    assert_expression(true, None, None, "A\n/*T1*/\n\n/*T2*/", &ast);
}

#[test]
fn statements_separated_by_comments() {
    assert_program_string(true, "A;\n//L\nB;");
    assert_program_string(true, "A;\n/*L1*/\n/*L2*/\nB;");
    assert_program_string(true, "A; //L\nB;");
    assert_program_string(true, "A; /*T1\nT2*/\nB;");
}

#[test]
fn assignment_expression() {
    assert_expression_string(true, None, None, "A = //L\nB");
    assert_expression_string(true, None, None, "A =\n//L\nB");
}

#[test]
fn array() {
    assert_expression_string(false, None, None, "[/*I*/]");
    assert_expression_string(true, None, None, "[\n  a,\n  /*I*/\n]");
    assert_expression_string(true, None, None, "[\n  a,\n  \n  /*I*/\n]");
    assert_expression_string(true, None, None, "[\n  a //T\n  ,\n  \n  //L\n  b,\n]");
}

#[test]
fn array_pattern() {
    assert_statement_string(false, None, "var[/*I*/];");
    assert_statement_string(true, None, "var [\n  a\n  /*I*/\n];");
    assert_statement_string(true, None, "var [\n  a\n  \n  /*I*/\n];");
}

#[test]
fn arrow_function_body() {
    // Body without leading comment separated by space
    assert_expression_string(true, None, None, "() => <A />");
    // Body with leading comment separated by newline
    assert_expression_string(true, None, None, "() =>\n//L\n<A />");
}

#[test]
fn arrow_function_params() {
    assert_expression_string(false, None, None, "/*L*/()/*T*/=>{}");
    assert_expression(
        false,
        None,
        None,
        "/*L*/A/*T*/=>{}",
        &ast_builder::test_expression_of_string("/*L*/(A)/*T*/=>{}"),
    );
    assert_expression_string(false, None, None, "(/*L*/A/*T*/)=>{}");
    assert_expression_string(false, None, None, "//L\nA=>{}");
}

#[test]
fn block_stmt() {
    assert_statement_string(false, None, "{/*I*/}");
}

#[test]
fn break_() {
    assert_statement_string(false, None, "break;/*T*/");
}

#[test]
fn binary_expression() {
    assert_expression_string(true, None, None, "a + //L\nb");
    assert_expression_string(true, None, None, "a + //L\n+b");
    assert_expression_string(true, None, None, "a + \n//L\nb");
    assert_expression_string(true, None, None, "a + \n//L\n+b");
}

#[test]
fn call() {
    let a80 = "a".repeat(80);
    assert_expression_string(false, None, None, "foo(/*I*/)");
    assert_expression_string(true, None, None, &format!("foo(\n  {},\n  /*I*/\n)", a80));
    assert_expression_string(true, None, None, "foo(\n  a,\n  \n  /*I*/\n)");
}

#[test]
fn call_type_args() {
    let a80 = "a".repeat(80);
    assert_expression_string(false, None, None, "foo</*I*/>()");
    assert_expression_string(true, None, None, &format!("foo<\n  {},\n  /*I*/\n>()", a80));
    assert_expression_string(true, None, None, "foo<\n  a,\n  \n  /*I*/\n>()");
}

#[test]
fn class_private_field() {
    assert_expression_string(false, None, None, "class C{/*L*/#A/*T*/;}");
}

#[test]
fn continue_() {
    assert_statement_string(false, None, "continue;/*T*/");
}

#[test]
fn debugger() {
    assert_statement_string(false, None, "debugger;/*T*/");
}

#[test]
fn declare_module() {
    assert_statement_string(false, None, "declare module A{/*I*/}");
}

#[test]
fn do_while() {
    assert_statement_string(false, None, "do{}while(A);/*T*/");
}

#[test]
fn enum_() {
    assert_statement_string(false, None, "enum E of boolean{A=/*L*/true/*T*/,}");
    assert_statement_string(false, None, "enum E of number{A=/*L*/1/*T*/,}");
    assert_statement_string(false, None, r#"enum E of string{A=/*L*/"A"/*T*/,}"#);
}

#[test]
fn function_body() {
    assert_statement_string(false, None, "function foo(){/*I*/}");
}

#[test]
fn function_params() {
    let ast = ast_builder::test_expression_of_string("function foo/*L*/()/*T*/\n{}");
    assert_expression(false, None, None, "function foo/*L*/()/*T*/{}", &ast);
    assert_expression_string(false, None, None, "(/*I*/)=>{}");
    assert_expression_string(true, None, None, "(\n  a,\n  /*I*/\n) => {}");
    assert_expression_string(true, None, None, "(\n  a,\n  \n  /*I*/\n) => {}");
}

#[test]
fn function_type_params() {
    assert_statement_string(false, None, "type T=(/*I*/)=>a;");
    assert_statement_string(true, None, "type T = (\n  a\n  /*I*/\n) => b;");
    assert_statement_string(true, None, "type T = (\n  a\n  \n  /*I*/\n) => b;");
}

#[test]
fn if_statement() {
    assert_statement_string(true, None, "if (true) {} //L\n else {}");
    assert_statement_string(true, None, "if (true) {}\n//L\nelse {}");
}

#[test]
fn jsx_expression_container() {
    assert_expression_string(false, None, None, "<A>{/*I*/}</A>");
}

#[test]
fn literal() {
    assert_expression_string(false, None, None, "//L\n1//T\n");
}

#[test]
fn logical_expression() {
    assert_expression_string(true, None, None, "a && //L\n  b");
    assert_expression_string(true, None, None, "a &&\n  //L\n  b");
    assert_expression_string(true, None, None, "(\n  //L\n  a &&\n    b\n) ??\n  c");
}

#[test]
fn tagged_template() {
    assert_expression_string(false, None, None, "/*L1*/A/*L2*/`B`/*T*/");
}

#[test]
fn member_expression() {
    assert_expression_string(false, None, None, "A./*L*/B/*T*/");
    assert_expression_string(false, None, None, "A./*L*/#B/*T*/");
    assert_expression_string(true, None, None, "foo //C\n.bar");
    assert_expression_string(true, None, None, "foo /*C*/\n.bar");
    assert_expression_string(true, None, None, "foo /*C*/.bar");
    assert_expression_string(true, None, None, "foo\n//C\n.bar");
    assert_expression_string(true, None, None, "foo\n/*C*/\n.bar");
    assert_expression_string(true, None, None, "foo\n/*C*/.bar");
    assert_expression_string(true, None, None, "foo[\n  //L\n  a\n]");
}

#[test]
fn new() {
    let a80 = "a".repeat(80);
    assert_expression_string(false, None, None, "new Foo(/*I*/)");
    assert_expression_string(
        true,
        None,
        None,
        &format!("new Foo(\n  {},\n  /*I*/\n)", a80),
    );
    assert_expression_string(true, None, None, "new Foo(\n  a,\n  \n  /*I*/\n)");
    assert_expression_string(true, None, None, "new (\n  //L\n  A ||\n    B\n)");
}

#[test]
fn object() {
    assert_expression_string(false, None, None, "{/*I*/}");
    assert_expression_string(true, None, None, "{\n  a,\n  /*I*/\n}");
    assert_expression_string(true, None, None, "{\n  a,\n  \n  /*I*/\n}");
    assert_expression_string(true, None, None, "{\n  a: //L\n  b,\n}");
    assert_expression_string(true, None, None, "{\n  a:\n  //L\n  b,\n}");
}

#[test]
fn object_pattern() {
    let b80 = "b".repeat(80);
    assert_statement_string(false, None, "var{/*I*/};");
    assert_statement_string(
        true,
        None,
        &format!("var {{\n  a,\n  {}\n  /*I*/\n}};", b80),
    );
    assert_statement_string(
        true,
        None,
        &format!("var {{\n  a,\n  {}\n  \n  /*I*/\n}};", b80),
    );
}

#[test]
fn object_type() {
    assert_statement_string(false, None, "type T={/*I*/};");
    assert_statement_string(false, None, "type T={a:any,/*I*/};");
    assert_statement_string(
        true,
        None,
        "type T = {\n  a: any,\n  /*I1*/\n  /*I2*/\n  ...\n};",
    );
    assert_statement(
        true,
        None,
        "type T = {\n  a: any,\n  \n  /*I1*/\n  /*I2*/\n  ...\n};",
        &ast_builder::test_statement_of_string(
            "type T = {\n  a: any,\n  ...\n  /*I1*/\n  /*I2*/\n};",
        ),
    );
    assert_statement_string(
        true,
        None,
        "type T = {\n  a: any,\n  /*I1*/\n  \n  /*I2*/\n  ...\n};",
    );
    // Leading comments on variance nodes are included in comment bounds of property
    assert_statement_string(true, None, "type T = {\n  +a: any,\n  //L\n  +b: any,\n};");
}

#[test]
fn parenthesized_expression() {
    assert_expression_string(true, None, None, "(\n  //L\n  a + b\n) * c");
}

#[test]
fn return_() {
    assert_statement_string(false, None, "return;/*T*/");
    assert_statement_string(true, None, "return (\n  //L\n  x\n);");
    assert_statement_string(true, None, "return /*L*/ x;");
}

#[test]
fn switch_case() {
    assert_statement_string(true, None, "switch (x) {\n  case 1: /*T*/\n    break;\n}");
}

#[test]
fn throw() {
    assert_statement_string(false, None, "throw A;/*T*/");
    assert_statement_string(true, None, "throw (\n  //L\n  x\n);");
    assert_statement_string(true, None, "throw /*L*/ x;");
}

#[test]
fn type_alias() {
    assert_statement_string(true, None, "type A = //L\nB;");
    assert_statement_string(true, None, "type A =\n//L\nB;");
}

#[test]
fn type_args() {
    let a80 = "a".repeat(80);
    assert_statement_string(false, None, "type Foo=Bar</*I*/>;");
    assert_statement_string(
        true,
        None,
        &format!("type Foo = Bar<\n  {},\n  /*I*/\n>;", a80),
    );
    assert_statement_string(true, None, "type Foo = Bar<\n  a,\n  \n  /*I*/\n>;");
}

#[test]
fn type_params() {
    let a80 = "a".repeat(80);
    assert_expression_string(
        true,
        None,
        None,
        &format!("<\n  {},\n  /*I*/\n>() => {{}}", a80),
    );
    assert_expression_string(
        true,
        None,
        None,
        &format!("<\n  {},\n  \n  /*I*/\n>() => {{}}", a80),
    );
}

#[test]
fn union_type() {
    let b80 = "b".repeat(80);
    assert_statement_string(true, None, &format!("type Foo =\n//L\n| a\n  | {};", b80));
    assert_statement_string(
        true,
        None,
        &format!("type Foo = \n  | a\n  | //L\n  {};", b80),
    );
    assert_statement_string(
        true,
        None,
        &format!("type Foo = \n  | a\n  |\n  //L\n  {};", b80),
    );
}

#[test]
fn variable_declaration() {
    assert_statement_string(false, None, "let A=B;/*T*/");
    assert_statement_string(true, None, "let A = //L\nB;");
    assert_statement_string(true, None, "let A =\n//L\nB;");
}
