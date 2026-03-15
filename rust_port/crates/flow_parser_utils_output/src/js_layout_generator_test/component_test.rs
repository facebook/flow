/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_parser::ast;
use flow_parser::ast::statement;
use flow_parser::ast::types as at;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::statements as S;
use flow_parser_utils::ast_builder::types as T;

use crate::layout_generator_test_utils::*;

fn variable_declaration() -> statement::Statement<Loc, Loc> {
    S::const_declaration(
        None,
        None,
        vec![S::variable_declarator(
            None,
            Some(E::literals::string(None, None, "x")),
            None,
            "x",
        )],
    )
}

#[allow(dead_code)]
fn void_annotation() -> at::AnnotationOrHint<Loc, Loc> {
    at::AnnotationOrHint::Available(T::annotation(T::void(None, None)))
}

fn type_params() -> at::TypeParams<Loc, Loc> {
    T::type_params(
        None,
        None,
        vec![T::type_param(None, None, None, None, None, None, "T")],
    )
}

fn comments() -> ast::Syntax<Loc, ()> {
    ast::Syntax {
        leading: Arc::from(vec![ast_builder::comments::block(None, None, "leading")]),
        trailing: Arc::from(vec![ast_builder::comments::line(None, None, "trailing")]),
        internal: (),
    }
}

#[test]
fn simple_component() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let ast = S::component_declaration(None, None, None, None, None, None, "Comp", Some(body));
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp(){}", &layout);
    assert_output(true, "component Comp() {}", &layout);
}

#[test]
fn simple_component_type() {
    let ast = T::component_type(
        None,
        None,
        None,
        at::component_params::Params {
            loc: Loc::none(),
            params: vec![].into(),
            rest: None,
            comments: None,
        },
    );
    let layout = crate::js_layout_generator::type_(&opts(), &ast);
    assert_output(false, "component()", &layout);
    assert_output(true, "component()", &layout);
}

#[test]
fn component_with_body() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![variable_declaration()].into(),
            comments: None,
        },
    );
    let ast = S::component_declaration(None, None, None, None, None, None, "Comp", Some(body));
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp(){const x=\"x\"}", &layout);
    assert_output(true, "component Comp() {\n  const x = \"x\";\n}", &layout);
}

#[test]
fn component_with_renders() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let renders = Some(T::component_renders_annotation(
        None,
        at::RendersVariant::Normal,
        T::mixed(None, None),
    ));
    let ast = S::component_declaration(
        None,
        None,
        Some(type_params()),
        None,
        renders,
        None,
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp<T>() renders mixed{}", &layout);
    assert_output(true, "component Comp<T>() renders mixed {}", &layout);
}

#[test]
fn component_with_comments() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let ast = S::component_declaration(
        None,
        None,
        None,
        None,
        None,
        Some(comments()),
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "/*leading*/component Comp(){}//trailing\n", &layout);
    assert_output(
        true,
        "/*leading*/ component Comp() {} //trailing\n",
        &layout,
    );
}

#[test]
fn component_with_simple_params() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let params = S::component_params(
        None,
        None,
        None,
        vec![
            S::component_id_param(None, None, None, "p1"),
            S::component_string_param(
                None,
                None,
                "p-2",
                ast_builder::patterns::identifier(None, None, "p2"),
            ),
        ],
    );
    let ast = S::component_declaration(
        None,
        None,
        None,
        Some(params),
        None,
        None,
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp(p1,\"p-2\" as p2){}", &layout);
    assert_output(true, "component Comp(p1, \"p-2\" as p2) {}", &layout);
}

#[test]
fn component_params_with_defaults() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let params = S::component_params(
        None,
        None,
        None,
        vec![
            S::component_id_param(
                None,
                Some(E::literals::string(None, None, "default_p1")),
                None,
                "p1",
            ),
            S::component_string_param(
                None,
                Some(E::literals::string(None, None, "default_p2")),
                "p-2",
                ast_builder::patterns::identifier(None, None, "p2"),
            ),
        ],
    );
    let ast = S::component_declaration(
        None,
        None,
        None,
        Some(params),
        None,
        None,
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        false,
        "component Comp(p1=\"default_p1\",\"p-2\" as p2=\"default_p2\"){}",
        &layout,
    );
    assert_output(
        true,
        "component Comp(p1 = \"default_p1\", \"p-2\" as p2 = \"default_p2\") {}",
        &layout,
    );
}

#[test]
fn component_params_id_with_local() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let params = S::component_params(
        None,
        None,
        None,
        vec![S::component_id_param(
            None,
            None,
            Some(ast_builder::patterns::identifier(None, None, "local_p1")),
            "p1",
        )],
    );
    let ast = S::component_declaration(
        None,
        None,
        None,
        Some(params),
        None,
        None,
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp(p1 as local_p1){}", &layout);
    assert_output(true, "component Comp(p1 as local_p1) {}", &layout);
}

#[test]
fn component_params_id_with_rest() {
    let body = (
        Loc::none(),
        statement::Block {
            body: vec![].into(),
            comments: None,
        },
    );
    let params = S::component_params(
        None,
        Some(statement::component_params::RestParam {
            loc: Loc::none(),
            argument: ast_builder::patterns::identifier(None, None, "rest"),
            comments: None,
        }),
        None,
        vec![S::component_id_param(None, None, None, "p1")],
    );
    let ast = S::component_declaration(
        None,
        None,
        None,
        Some(params),
        None,
        None,
        "Comp",
        Some(body),
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "component Comp(p1,...rest){}", &layout);
    assert_output(true, "component Comp(p1, ...rest) {}", &layout);
}

#[test]
fn declare_component() {
    let ast = S::declare_component(None, None, None, None, None, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "declare component Comp();", &layout);
    assert_output(true, "declare component Comp();", &layout);
}

#[test]
fn declare_component_with_renders_annot() {
    let renders = Some(T::component_renders_annotation(
        None,
        at::RendersVariant::Normal,
        T::mixed(None, None),
    ));
    let ast = S::declare_component(None, None, None, None, renders, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "declare component Comp() renders mixed;", &layout);
    assert_output(true, "declare component Comp() renders mixed;", &layout);
}

#[test]
fn declare_component_with_comments() {
    let ast = S::declare_component(None, None, None, Some(comments()), None, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        false,
        "/*leading*/declare component Comp();//trailing\n",
        &layout,
    );
    assert_output(
        true,
        "/*leading*/ declare component Comp(); //trailing\n",
        &layout,
    );
}

#[test]
fn declare_component_with_simple_params() {
    use statement::component_params::Param;
    use statement::component_params::ParamName;
    let params = S::component_params(
        None,
        None,
        None,
        vec![
            Param {
                loc: Loc::none(),
                name: ParamName::Identifier(ast_builder::identifiers::identifier(None, "p1")),
                local: ast_builder::patterns::identifier(
                    None,
                    Some(at::AnnotationOrHint::Available(T::annotation(T::number(
                        None, None,
                    )))),
                    "p1",
                ),
                default: None,
                shorthand: true,
            },
            Param {
                loc: Loc::none(),
                name: ParamName::StringLiteral((
                    Loc::none(),
                    ast_builder::string_literal(None, "p-2"),
                )),
                local: ast_builder::patterns::identifier(
                    None,
                    Some(at::AnnotationOrHint::Available(T::annotation(T::void(
                        None, None,
                    )))),
                    "p2",
                ),
                default: None,
                shorthand: false,
            },
        ],
    );
    let ast = S::declare_component(None, None, Some(params), None, None, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        false,
        "declare component Comp(p1:number,\"p-2\" as p2:void);",
        &layout,
    );
    assert_output(
        true,
        "declare component Comp(p1: number, \"p-2\" as p2: void);",
        &layout,
    );
}

#[test]
fn declare_component_with_rest() {
    let params = S::component_params(
        None,
        Some(statement::component_params::RestParam {
            loc: Loc::none(),
            argument: ast_builder::patterns::identifier(
                None,
                Some(at::AnnotationOrHint::Available(T::annotation(T::mixed(
                    None, None,
                )))),
                "rest",
            ),
            comments: None,
        }),
        None,
        vec![],
    );
    let ast = S::declare_component(None, None, Some(params), None, None, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(false, "declare component Comp(...rest:mixed);", &layout);
    assert_output(true, "declare component Comp(...rest: mixed);", &layout);
}

#[test]
fn declare_component_with_param_and_rest() {
    use statement::component_params::Param;
    use statement::component_params::ParamName;
    let params = S::component_params(
        None,
        Some(statement::component_params::RestParam {
            loc: Loc::none(),
            argument: ast_builder::patterns::identifier(
                None,
                Some(at::AnnotationOrHint::Available(T::annotation(T::mixed(
                    None, None,
                )))),
                "rest",
            ),
            comments: None,
        }),
        None,
        vec![Param {
            loc: Loc::none(),
            name: ParamName::Identifier(ast_builder::identifiers::identifier(None, "p1")),
            local: ast_builder::patterns::identifier(
                None,
                Some(at::AnnotationOrHint::Available(T::annotation(T::number(
                    None, None,
                )))),
                "p1",
            ),
            default: None,
            shorthand: true,
        }],
    );
    let ast = S::declare_component(None, None, Some(params), None, None, "Comp");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        false,
        "declare component Comp(p1:number,...rest:mixed);",
        &layout,
    );
    assert_output(
        true,
        "declare component Comp(p1: number, ...rest: mixed);",
        &layout,
    );
}

// Tests which include parsing
#[test]
fn parse_simple_component() {
    assert_statement_string(true, None, "component Comp() {}");
}

#[test]
fn parse_component_renders() {
    assert_statement_string(true, None, "component Comp() renders TReturn {}");
}

#[test]
fn parse_component_comments() {
    assert_statement_string(
        true,
        None,
        "/* leading */ component Comp() {} // trailing\n",
    );
}

#[test]
fn parse_component_comments2() {
    assert_statement_string(true, None, "component /* c1 */ Comp() {} /* c2 */");
}

#[test]
fn parse_component_comments3() {
    assert_statement_string(true, None, "component Comp(/* c1 */) /* c2 */ {}");
}

#[test]
fn parse_component_comments4() {
    assert_statement_string(
        true,
        None,
        "component Comp() renders TReturn /* c1*/ {// c2\n}",
    );
}

#[test]
fn parse_component_comments5() {
    assert_statement_string(true, None, "component Comp() /* c1 */ renders TReturn {}");
}

#[test]
fn parse_component_params() {
    assert_statement_string(
        true,
        None,
        "component Comp(p1: T1, p2: T2, ...rest: TRest) {}",
    );
}

#[test]
fn parse_component_params2() {
    assert_statement_string(true, None, "component Comp(p1 as p11: T1 = \"default\") {}");
}

#[test]
fn parse_component_params_comments() {
    assert_statement_string(true, None, "component Comp(p1: T1 /* c1 */) {}");
}

#[test]
fn parse_component_params_comments2() {
    assert_statement_string(
        true,
        None,
        "component Comp(p1: T1 /* c1 */, p2 /* some */: T2) {}",
    );
}

#[test]
fn parse_component_params_comments3() {
    // TODO: This comment printing is not ideal. However, this needs to also be fixed for functions
    let ast = ast_builder::test_statement_of_string("component Comp(\n  p1: T1, // c1\n) {}");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(true, "component Comp(\n  p1: T1 // c1\n  ,\n) {}", &layout);
}

#[test]
fn parse_component_params_comments4() {
    assert_statement_string(
        true,
        None,
        "component Comp(p1 as p11 /* c1 */: T1 /* c1 */ = \"default\") {}",
    );
}

#[test]
fn parse_component_params_comments5() {
    assert_statement_string(
        true,
        None,
        "component Comp(...rest /* c1 */: TRest /* c2 */) {}",
    );
}

#[test]
fn parse_declare_component() {
    assert_statement_string(true, None, "declare component Comp();");
}

#[test]
fn parse_declare_component_renders() {
    assert_statement_string(true, None, "declare component Comp() renders TReturn;");
}

#[test]
fn parse_declare_component_comments() {
    assert_statement_string(true, None, "/* c1 */ declare component Comp(); // c2\n");
}

#[test]
fn parse_declare_component_comments2() {
    // TODO: This comment printing is not ideal. However, this needs to also be fixed for functions
    let ast = ast_builder::test_statement_of_string(
        "declare /* c1 */ component Comp(/* c2 */); /* c3 */",
    );
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        true,
        "/* c1 */ declare component Comp(/* c2 */); /* c3 */",
        &layout,
    );
}

#[test]
fn parse_declare_component_comments3() {
    assert_statement_string(true, None, "declare component /* c1 */ Comp() /* c2 */;");
}

#[test]
fn parse_declare_component_comments4() {
    assert_statement_string(
        true,
        None,
        "declare component Comp() /* c1 */ renders TReturn; // c2\n",
    );
}

#[test]
fn parse_declare_component_params() {
    assert_statement_string(
        true,
        None,
        "declare component Comp(p1: T1, p2: T2, ...TRest);",
    );
}

#[test]
fn parse_declare_component_params_comments() {
    assert_statement_string(true, None, "declare component Comp(p1: T1 /* c1 */);");
}

#[test]
fn parse_declare_component_params_comments2() {
    assert_statement_string(
        true,
        None,
        "declare component Comp(p1 /* c1 */: T1, /* c2 */ p2: /* c3 */ T2);",
    );
}

#[test]
fn parse_declare_component_params_comments3() {
    // TODO: This comment printing is not ideal. However, this needs to also be fixed for functions
    let ast = ast_builder::test_statement_of_string("declare component Comp(\n  p1: T1, // c1\n);");
    let layout = crate::js_layout_generator::statement(&opts(), false, &ast);
    assert_output(
        true,
        "declare component Comp(\n  p1: T1 // c1\n  ,\n);",
        &layout,
    );
}

#[test]
fn parse_declare_component_params_comments4() {
    assert_statement_string(
        true,
        None,
        "declare component Comp(p1 as p11 /* c1 */: T1 /* c1 */ = \"default\");",
    );
}

#[test]
fn parse_declare_component_params_comments5() {
    assert_statement_string(
        true,
        None,
        "declare component Comp(...rest /* c1 */: TRest /* c2 */);",
    );
}
