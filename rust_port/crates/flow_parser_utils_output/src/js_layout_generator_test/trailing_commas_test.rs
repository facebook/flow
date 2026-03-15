/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::functions as F;
use flow_parser_utils::ast_builder::identifiers as I;
use flow_parser_utils::ast_builder::patterns as P;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::js_layout_generator::TrailingCommas as C;
use crate::layout_generator_test_utils::*;

mod array {
    use super::*;

    fn array_layout(trailing_commas: C) -> crate::layout::LayoutNode {
        let a80 = "a".repeat(80);
        js_layout_generator::expression(
            &js_layout_generator::Opts {
                trailing_commas,
                ..js_layout_generator::default_opts()
            },
            None,
            &E::array(
                None,
                None,
                vec![
                    E::array_expression(E::identifier(None, None, &a80)),
                    E::array_expression(E::identifier(None, None, &a80)),
                ],
            ),
        )
    }

    #[test]
    fn test_all() {
        let a80 = "a".repeat(80);
        let layout = array_layout(C::All);
        assert_output(false, &format!("[{},{}]", a80, a80), &layout);
        assert_output(true, &format!("[\n  {},\n  {},\n]", a80, a80), &layout);
    }

    #[test]
    fn test_es5() {
        let a80 = "a".repeat(80);
        let layout = array_layout(C::ES5);
        assert_output(false, &format!("[{},{}]", a80, a80), &layout);
        assert_output(true, &format!("[\n  {},\n  {},\n]", a80, a80), &layout);
    }

    #[test]
    fn test_off() {
        let a80 = "a".repeat(80);
        let layout = array_layout(C::Off);
        assert_output(false, &format!("[{},{}]", a80, a80), &layout);
        assert_output(true, &format!("[\n  {},\n  {}\n]", a80, a80), &layout);
    }
}

mod object {
    use super::*;

    fn obj_layout(trailing_commas: C) -> crate::layout::LayoutNode {
        let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
        let prop1 = E::object_property(
            None,
            None,
            E::object_property_key(None, "foo"),
            E::identifier(None, None, x40),
        );
        let prop2 = E::object_property(
            None,
            None,
            E::object_property_key(None, "bar"),
            E::identifier(None, None, x40),
        );
        js_layout_generator::expression(
            &js_layout_generator::Opts {
                trailing_commas,
                ..js_layout_generator::default_opts()
            },
            None,
            &E::object_(None, None, vec![prop1, prop2]),
        )
    }

    #[test]
    fn test_all() {
        let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
        let layout = obj_layout(C::All);
        assert_output(false, &format!("{{foo:{},bar:{}}}", x40, x40), &layout);
        assert_output(
            true,
            &format!("{{\n  foo: {},\n  bar: {},\n}}", x40, x40),
            &layout,
        );
    }

    #[test]
    fn test_es5() {
        let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
        let layout = obj_layout(C::ES5);
        assert_output(false, &format!("{{foo:{},bar:{}}}", x40, x40), &layout);
        assert_output(
            true,
            &format!("{{\n  foo: {},\n  bar: {},\n}}", x40, x40),
            &layout,
        );
    }

    #[test]
    fn test_off() {
        let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
        let layout = obj_layout(C::Off);
        assert_output(false, &format!("{{foo:{},bar:{}}}", x40, x40), &layout);
        assert_output(
            true,
            &format!("{{\n  foo: {},\n  bar: {}\n}}", x40, x40),
            &layout,
        );
    }
}

mod call {
    use flow_parser::ast::expression::ExpressionOrSpread;

    use super::*;

    fn call_layout(trailing_commas: C) -> crate::layout::LayoutNode {
        let a80 = "a".repeat(80);
        js_layout_generator::expression(
            &js_layout_generator::Opts {
                trailing_commas,
                ..js_layout_generator::default_opts()
            },
            None,
            &E::call(
                None,
                Some(E::arg_list(
                    None,
                    None,
                    vec![ExpressionOrSpread::Expression(E::identifier(
                        None, None, &a80,
                    ))],
                )),
                E::identifier(None, None, "x"),
            ),
        )
    }

    #[test]
    fn test_all() {
        let a80 = "a".repeat(80);
        let layout = call_layout(C::All);
        assert_output(false, &format!("x({})", a80), &layout);
        assert_output(true, &format!("x(\n  {},\n)", a80), &layout);
    }

    #[test]
    fn test_es5() {
        let a80 = "a".repeat(80);
        let layout = call_layout(C::ES5);
        assert_output(false, &format!("x({})", a80), &layout);
        assert_output(true, &format!("x(\n  {},\n)", a80), &layout);
    }

    #[test]
    fn test_off() {
        let a80 = "a".repeat(80);
        let layout = call_layout(C::Off);
        assert_output(false, &format!("x({})", a80), &layout);
        assert_output(true, &format!("x(\n  {}\n)", a80), &layout);
    }
}

mod function_params {
    use super::*;

    fn fn_layout(trailing_commas: C) -> crate::layout::LayoutNode {
        let a80 = "a".repeat(80);
        let params = F::params(
            None,
            None,
            None,
            None,
            vec![F::param(None, None, P::identifier(None, None, &a80))],
        );
        js_layout_generator::statement(
            &js_layout_generator::Opts {
                trailing_commas,
                ..js_layout_generator::default_opts()
            },
            false,
            &S::function_declaration(
                None,
                None,
                None,
                Some(params),
                None,
                I::identifier(None, "x"),
            ),
        )
    }

    #[test]
    fn test_all() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::All);
        assert_output(false, &format!("function x({}){{}}", a80), &layout);
        assert_output(true, &format!("function x(\n  {},\n) {{}}", a80), &layout);
    }

    #[test]
    fn test_es5() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::ES5);
        assert_output(false, &format!("function x({}){{}}", a80), &layout);
        assert_output(true, &format!("function x(\n  {}\n) {{}}", a80), &layout);
    }

    #[test]
    fn test_off() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::Off);
        assert_output(false, &format!("function x({}){{}}", a80), &layout);
        assert_output(true, &format!("function x(\n  {}\n) {{}}", a80), &layout);
    }
}

mod function_params_with_rest {
    use super::*;

    fn fn_layout(trailing_commas: C) -> crate::layout::LayoutNode {
        let a80 = "a".repeat(80);
        let rest = F::rest_param(None, None, P::identifier(None, None, "rest"));
        let params = F::params(
            None,
            Some(rest),
            None,
            None,
            vec![F::param(None, None, P::identifier(None, None, &a80))],
        );
        js_layout_generator::statement(
            &js_layout_generator::Opts {
                trailing_commas,
                ..js_layout_generator::default_opts()
            },
            false,
            &S::function_declaration(
                None,
                None,
                None,
                Some(params),
                None,
                I::identifier(None, "x"),
            ),
        )
    }

    #[test]
    fn test_all() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::All);
        assert_output(false, &format!("function x({},...rest){{}}", a80), &layout);
        assert_output(
            true,
            &format!("function x(\n  {},\n  ...rest\n) {{}}", a80),
            &layout,
        );
    }

    #[test]
    fn test_es5() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::ES5);
        assert_output(false, &format!("function x({},...rest){{}}", a80), &layout);
        assert_output(
            true,
            &format!("function x(\n  {},\n  ...rest\n) {{}}", a80),
            &layout,
        );
    }

    #[test]
    fn test_off() {
        let a80 = "a".repeat(80);
        let layout = fn_layout(C::Off);
        assert_output(false, &format!("function x({},...rest){{}}", a80), &layout);
        assert_output(
            true,
            &format!("function x(\n  {},\n  ...rest\n) {{}}", a80),
            &layout,
        );
    }
}
