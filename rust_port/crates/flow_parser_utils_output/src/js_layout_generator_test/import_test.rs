/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::js_layout_generator;
use crate::layout_generator_test_utils::*;

fn no_bracket_spacing(opts: &js_layout_generator::Opts) -> js_layout_generator::Opts {
    js_layout_generator::Opts {
        bracket_spacing: false,
        ..opts.clone()
    }
}

#[test]
fn test_basic() {
    assert_statement_string(false, None, r#"import"a";"#);
    assert_statement_string(false, None, r#"import a from"a";"#);
    assert_statement_string(false, None, r#"import type a from"a";"#);
    assert_statement_string(false, None, r#"import typeof a from"a";"#);
    assert_statement_string(false, None, r#"import a,*as b from"a";"#);
    assert_statement_string(false, None, r#"import a,{b}from"a";"#);
    assert_statement_string(false, None, r#"import{a,type b}from"a";"#);
    assert_statement_string(false, None, r#"import{a,typeof b}from"a";"#);
    assert_statement_string(false, None, r#"import{a,type b as c}from"a";"#);
    assert_statement_string(false, None, r#"import{a as b}from"a";"#);
    assert_statement_string(false, None, r#"import type{a}from"a";"#);
    assert_statement_string(false, None, r#"import{a,b}from"a";"#);
    assert_statement_string(false, None, r#"import type{}from"a";"#);
    assert_statement_string(false, None, r#"import typeof{}from"a";"#);
    assert_statement_string(true, None, r#"import { a, b } from "a";"#);
    let opts = opts();
    assert_statement_string(
        true,
        Some(&no_bracket_spacing(&opts)),
        r#"import {a, b} from "a";"#,
    );
    assert_statement_string(true, None, r#"import type { a, b } from "a";"#);
    assert_statement_string(
        true,
        Some(&no_bracket_spacing(&opts)),
        r#"import type {a, b} from "a";"#,
    );
    assert_statement_string(
        true,
        None,
        &format!("import {{\n  a,\n  {},\n}} from \"a\";", "b".repeat(80)),
    );
    assert_statement_string(true, None, r#"import a, * as b from "a";"#);
    assert_statement_string(
        true,
        None,
        &format!("import a, * as {} from \"a\";", "b".repeat(80)),
    );
    assert_statement_string(true, None, r#"import a, { b } from "a";"#);
    assert_statement_string(
        true,
        Some(&no_bracket_spacing(&opts)),
        r#"import a, {b} from "a";"#,
    );
}

#[test]
fn test_wrap_specifiers() {
    assert_statement_string(
        true,
        None,
        &format!("import a, {{\n  {},\n}} from \"a\";", "b".repeat(80)),
    );
    assert_statement_string(
        true,
        None,
        &format!(
            "import {{\n  {},\n  {},\n}} from \"a\";",
            "a".repeat(80),
            "b".repeat(80)
        ),
    );
    assert_statement_string(
        true,
        None,
        &format!(
            "import {{\n  {},\n  {},\n}} from \"{}\";",
            "a".repeat(20),
            "b".repeat(20),
            "c".repeat(40)
        ),
    );
    // don't wrap a single specifier even if it's too long
    assert_statement_string(
        true,
        None,
        &format!(
            "import {{ {} }} from \"{}\";",
            "a".repeat(40),
            "b".repeat(40)
        ),
    );
    let opts = opts();
    assert_statement_string(
        true,
        Some(&no_bracket_spacing(&opts)),
        &format!("import {{{}}} from \"{}\";", "a".repeat(40), "b".repeat(40)),
    );
    // do wrap a single specifier if there's a default
    assert_statement_string(
        true,
        None,
        &format!(
            "import x, {{\n  {},\n}} from \"{}\";",
            "a".repeat(40),
            "b".repeat(40)
        ),
    );
}
