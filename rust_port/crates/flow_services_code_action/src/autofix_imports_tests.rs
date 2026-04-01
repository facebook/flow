/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/__tests__/autofix_imports_tests.ml`

fn indent_len(s: &str) -> usize {
    let mut i = 0;
    let bytes = s.as_bytes();
    while i < bytes.len() && bytes[i] == b' ' {
        i += 1;
    }
    i
}

fn dedent_trim(s: &str) -> String {
    let lines: Vec<&str> = s.split('\n').collect();
    let non_empty_lines: Vec<&str> = lines.iter().filter(|l| l.trim() != "").copied().collect();
    let min_indent = non_empty_lines
        .iter()
        .fold(usize::MAX, |acc, line| acc.min(indent_len(line)));
    let lines: Vec<&str> = lines
        .iter()
        .map(|line| {
            let len = line.len();
            if len < min_indent {
                *line
            } else {
                &line[min_indent..]
            }
        })
        .collect();
    let joined = lines.join("\n");
    format!("{}\n", joined.trim())
}

fn parse(
    contents: &str,
) -> flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    use flow_parser::ParseOptions;
    use flow_parser::parse_program_without_file;
    let parse_options = Some(ParseOptions {
        enums: true,
        ..ParseOptions::default()
    });
    let (ast, _errors) = parse_program_without_file(false, None, parse_options, Ok(contents));
    ast
}

fn named_binding(remote_name: &str) -> crate::autofix_imports::NamedBinding {
    crate::autofix_imports::NamedBinding {
        remote_name: remote_name.to_string(),
        local_name: None,
    }
}

fn named_binding_with_local(
    remote_name: &str,
    local_name: &str,
) -> crate::autofix_imports::NamedBinding {
    crate::autofix_imports::NamedBinding {
        remote_name: remote_name.to_string(),
        local_name: Some(local_name.to_string()),
    }
}

fn offset_of_position(s: &str, line: i32, column: i32) -> usize {
    let mut init: usize = 0;
    let mut remaining_line = line;
    while remaining_line > 1 {
        let eol = s[init..].find('\n').expect("newline not found") + init;
        init = eol + 1;
        remaining_line -= 1;
    }
    init + column as usize
}

fn apply_patch(contents: &str, patch: &[(flow_parser::loc::Loc, String)]) -> String {
    let mut reversed: Vec<_> = patch.iter().collect();
    reversed.reverse();
    let mut result = contents.to_string();
    for (loc, replacement) in reversed {
        let start_offset = offset_of_position(&result, loc.start.line, loc.start.column);
        let end_offset = offset_of_position(&result, loc.end.line, loc.end.column);
        result = format!(
            "{}{}{}",
            &result[..start_offset],
            replacement,
            &result[end_offset..]
        );
    }
    result
}

fn assert_patch<F>(
    bracket_spacing: Option<bool>,
    single_quotes: Option<bool>,
    f: F,
    expected: &str,
    contents: &str,
) where
    F: FnOnce(
        &flow_parser_utils_output::js_layout_generator::Opts,
        &flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    ) -> Vec<(flow_parser::loc::Loc, String)>,
{
    let expected = dedent_trim(expected);
    let contents = dedent_trim(contents);
    let mut options = flow_parser_utils_output::js_layout_generator::default_opts();
    if let Some(bs) = bracket_spacing {
        options.bracket_spacing = bs;
    }
    if let Some(sq) = single_quotes {
        options.single_quotes = sq;
    }
    let ast = parse(&contents);
    let patch = f(&options, &ast);
    let patched = apply_patch(&contents, &patch);
    assert_eq!(expected, patched);
}

fn assert_import(
    bracket_spacing: Option<bool>,
    single_quotes: Option<bool>,
    expected: &str,
    bindings: crate::autofix_imports::Bindings,
    from: &str,
    contents: &str,
) {
    let from = from.to_string();
    assert_patch(
        bracket_spacing,
        single_quotes,
        move |options, ast| crate::autofix_imports::add_import(options, bindings, from, ast),
        expected,
        contents,
    );
}

fn assert_imports(
    bracket_spacing: Option<bool>,
    single_quotes: Option<bool>,
    expected: &str,
    added_imports: Vec<(String, crate::autofix_imports::Bindings)>,
    contents: &str,
) {
    assert_patch(
        bracket_spacing,
        single_quotes,
        move |options, ast| crate::autofix_imports::add_imports(options, &added_imports, ast),
        expected,
        contents,
    );
}

fn assert_organized(
    bracket_spacing: Option<bool>,
    single_quotes: Option<bool>,
    expected: &str,
    contents: &str,
) {
    assert_patch(
        bracket_spacing,
        single_quotes,
        crate::autofix_imports::organize_imports,
        expected,
        contents,
    );
}

#[cfg(test)]
mod add_import_tests {
    use super::*;
    use crate::autofix_imports::Bindings;

    #[test]
    fn import_named_no_existing() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        let contents = "
          foo
        ";
        let expected = r#"
          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);

        let expected = expected.replace('"', "'");
        let binding = Bindings::Named(vec![named_binding("foo")]);
        assert_import(None, Some(true), &expected, binding, from, contents);
    }

    #[test]
    fn import_named_above_existing() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        let contents = r#"
          import { zzz } from "./zzz";

          foo
        "#;
        let expected = r#"
          import { foo } from "./foo";
          import { zzz } from "./zzz";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_below_existing() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        let contents = r#"
          import { bar } from "./bar";

          foo
        "#;
        let expected = r#"
          import { bar } from "./bar";
          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_below_existing_between_sections() {
        let binding = Bindings::Named(vec![named_binding("xyz")]);
        let from = "./xyz";
        //
        //
        let contents = r#"
          import { foo } from "./relative";

          import { bar } from "module";

          foo
        "#;
        //
        //
        let expected = r#"
          import { foo } from "./relative";
          import { xyz } from "./xyz";

          import { bar } from "module";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_sorted_existing() {
        let binding = Bindings::Named(vec![named_binding("baz")]);
        let from = "./baz";
        //
        let contents = r#"
          import { bar } from "./bar";
          import { foo } from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import { bar } from "./bar";
          import { baz } from "./baz";
          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_unsorted_existing() {
        let binding = Bindings::Named(vec![named_binding("baz")]);
        let from = "./baz";
        //
        let contents = r#"
          import { foo } from "./foo";
          import { bar } from "./bar";

          foo
        "#;
        //
        let expected = r#"
          import { foo } from "./foo";
          import { bar } from "./bar";
          import { baz } from "./baz";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_in_existing() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        //
        let contents = r#"
          import { bar } from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import { bar, foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);

        let expected = expected.replace('"', "'");
        let binding = Bindings::Named(vec![named_binding("foo")]);
        assert_import(None, Some(true), &expected, binding, from, contents);
    }

    #[test]
    fn import_multiple_named_in_existing() {
        let binding = Bindings::Named(vec![
            named_binding("foo"),
            named_binding_with_local("baz", "bazz"),
        ]);
        let from = "./foo";
        //
        let contents = r#"
          import { bar } from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import { bar, baz as bazz, foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_below_existing_default() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        //
        let contents = r#"
          import Foo from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import Foo from "./foo";
          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_named_below_existing_type() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        //
        let contents = r#"
          import type { IFoo } from "./foo";

          foo
        "#;
        //
        //
        let expected = r#"
          import type { IFoo } from "./foo";

          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_default_no_existing() {
        let binding = Bindings::Default("foo".to_string());
        let from = "./foo";
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_default_duplicate() {
        let binding = Bindings::Default("Foo".to_string());
        let from = "./foo";
        //
        let contents = r#"
          import Bar from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import Bar from "./foo";
          import Foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_default_type_no_existing() {
        let binding = Bindings::DefaultType("foo".to_string());
        let from = "./foo";
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import type foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_default_type_duplicate() {
        let binding = Bindings::DefaultType("Foo".to_string());
        let from = "./foo";
        //
        let contents = r#"
          import type Bar from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import type Bar from "./foo";
          import type Foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_no_existing() {
        let binding = Bindings::NamedType(vec![named_binding("IFoo")]);
        let from = "./foo";
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import type { IFoo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_in_existing_type() {
        let binding = Bindings::NamedType(vec![named_binding("IBar")]);
        let from = "./foo";
        let contents = r#"
          import type { IFoo } from "./foo";
        "#;
        let expected = r#"
          import type { IBar, IFoo } from "./foo";
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_in_existing_type_unsorted() {
        let binding = Bindings::NamedType(vec![named_binding("IBar")]);
        let from = "./foo";
        let contents = r#"
          import type { IFoo, IBaz } from "./foo";
        "#;
        let expected = r#"
          import type { IFoo, IBaz, IBar } from "./foo";
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_above_existing_named() {
        let binding = Bindings::NamedType(vec![named_binding("IFoo")]);
        let from = "./foo";
        //
        let contents = r#"
          import { foo } from "./foo";

          foo
        "#;
        //
        //
        let expected = r#"
          import type { IFoo } from "./foo";

          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_above_existing_default() {
        let binding = Bindings::NamedType(vec![named_binding("IFoo")]);
        let from = "./foo";
        //
        let contents = r#"
          import foo from "./foo";

          foo
        "#;
        //
        //
        let expected = r#"
          import type { IFoo } from "./foo";

          import foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_above_existing_default_type() {
        let binding = Bindings::NamedType(vec![named_binding("IFoo")]);
        let from = "./foo";
        //
        let contents = r#"
          import type Foo from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import type Foo from "./foo";
          import type { IFoo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_type_unsorted_existing() {
        let binding = Bindings::NamedType(vec![named_binding("IBaz")]);
        let from = "./baz";
        //
        let contents = r#"
          import { foo } from "./foo";
          import { bar } from "./bar";

          foo
        "#;
        //
        let expected = r#"
          import { foo } from "./foo";
          import { bar } from "./bar";
          import type { IBaz } from "./baz";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_namespace_no_existing() {
        let binding = Bindings::Namespace("React".to_string());
        let from = "react";
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import * as React from "react";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_namespace_above_existing_named_from_same_module() {
        let binding = Bindings::Namespace("React".to_string());
        let from = "react";
        //
        let contents = r#"
          import { foo } from "react";

          foo
        "#;
        //
        let expected = r#"
          import * as React from "react";
          import { foo } from "react";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_namespace_above_existing_named() {
        let binding = Bindings::Namespace("Bar".to_string());
        let from = "./bar";
        //
        //
        let contents = r#"
          import type { IFoo } from "./foo";

          import { foo } from "./foo";

          foo
        "#;
        //
        //
        let expected = r#"
          import type { IFoo } from "./foo";

          import * as Bar from "./bar";
          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_namespace_duplicate() {
        let binding = Bindings::Namespace("Foo".to_string());
        let from = "./foo";
        //
        let contents = r#"
          import * as Bar from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import * as Bar from "./foo";
          import * as Foo from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn import_multiple_named_after_namespace() {
        let binding = Bindings::Named(vec![named_binding("useContext")]);
        let from = "react";
        //
        let contents = r#"
          import * as React from "react";
          import { useState } from "react";

          foo
        "#;
        //
        let expected = r#"
          import * as React from "react";
          import { useContext, useState } from "react";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn insert_after_flow_comment() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        //   // @flow
        //
        let contents = "
          // @flow

          foo
        ";
        //   // @flow
        //
        //
        let expected = r#"
          // @flow

          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn insert_after_directives() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        //   // @flow
        //
        //
        let contents = r#"
          // @flow

          "use strict";

          foo
        "#;
        //   // @flow
        //
        //
        //
        let expected = r#"
          // @flow

          "use strict";

          import { foo } from "./foo";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }

    #[test]
    fn bracket_spacing() {
        let binding = Bindings::Named(vec![named_binding("foo")]);
        let from = "./foo";
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import { foo } from "./foo";

          foo
        "#;
        assert_import(Some(true), None, expected, binding, from, contents);

        let expected = expected.replace("{ foo }", "{foo}");
        let binding = Bindings::Named(vec![named_binding("foo")]);
        assert_import(Some(false), None, &expected, binding, from, contents);
    }

    #[test]
    fn case_sensitive() {
        let binding = Bindings::Named(vec![named_binding("bar")]);
        let from = "bar";
        //
        let contents = r#"
          import { foo } from "Foo";

          foo
        "#;
        //
        //
        let expected = r#"
          import { foo } from "Foo";

          import { bar } from "bar";

          foo
        "#;
        assert_import(None, None, expected, binding, from, contents);
    }
}

#[cfg(test)]
mod add_imports_tests {
    use super::*;
    use crate::autofix_imports::Bindings;

    #[test]
    fn import_named_no_existing() {
        let added_imports = vec![
            (
                "./foo".to_string(),
                Bindings::Named(vec![named_binding("foo")]),
            ),
            (
                "./bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
        ];
        let contents = "
          foo
        ";
        //
        let expected = r#"
          import { bar } from "./bar";
          import { foo } from "./foo";

          foo
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn import_named_and_type_no_existing() {
        let added_imports = vec![
            (
                "./foo".to_string(),
                Bindings::NamedType(vec![named_binding("foo")]),
            ),
            (
                "./bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
            (
                "./baz".to_string(),
                Bindings::NamedType(vec![named_binding("baz")]),
            ),
        ];
        let contents = "
          foo
        ";
        //
        //
        let expected = r#"
          import type { baz } from "./baz";
          import type { foo } from "./foo";

          import { bar } from "./bar";

          foo
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn import_named_and_type_around_existing() {
        let added_imports = vec![
            (
                "./bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
            (
                "./baz".to_string(),
                Bindings::NamedType(vec![named_binding("baz")]),
            ),
        ];
        //
        let contents = r#"
          import type { foo } from "./foo";

          (1: foo)
        "#;
        //
        //
        let expected = r#"
          import type { baz } from "./baz";
          import type { foo } from "./foo";

          import { bar } from "./bar";

          (1: foo)
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn import_kinds_from_same_module() {
        let added_imports = vec![
            (
                "./foo".to_string(),
                Bindings::Named(vec![named_binding("foo")]),
            ),
            ("./foo".to_string(), Bindings::Default("Foo".to_string())),
            (
                "./foo".to_string(),
                Bindings::DefaultType("FooType".to_string()),
            ),
            (
                "./foo".to_string(),
                Bindings::Namespace("FooNS".to_string()),
            ),
        ];
        let contents = "
          foo
        ";
        //
        //
        let expected = r#"
          import type FooType from "./foo";

          import Foo from "./foo";
          import * as FooNS from "./foo";
          import { foo } from "./foo";

          foo
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn import_named_and_type_to_existing_sections() {
        let added_imports = vec![
            (
                "./bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
            (
                "./baz".to_string(),
                Bindings::NamedType(vec![named_binding("baz")]),
            ),
        ];
        //
        //
        let contents = r#"
          import type { foo } from "./foo";

          import { fizz } from "./fizz";

          (1: foo)
        "#;
        //
        //
        let expected = r#"
          import type { baz } from "./baz";
          import type { foo } from "./foo";

          import { bar } from "./bar";
          import { fizz } from "./fizz";

          (1: foo)
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn add_and_update() {
        let added_imports = vec![
            (
                "./foo".to_string(),
                Bindings::Named(vec![named_binding("other")]),
            ),
            (
                "./bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
        ];
        //
        let contents = r#"
          import { foo } from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import { bar } from "./bar";
          import { foo, other } from "./foo";

          foo
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }

    #[test]
    fn case_sensitive() {
        let added_imports = vec![
            (
                "bar".to_string(),
                Bindings::Named(vec![named_binding("bar")]),
            ),
            (
                "Foo".to_string(),
                Bindings::Named(vec![named_binding("foo")]),
            ),
        ];
        let contents = "
          foo
        ";
        //
        //
        let expected = r#"
          import { foo } from "Foo";

          import { bar } from "bar";

          foo
        "#;
        assert_imports(None, None, expected, added_imports, contents);
    }
}

#[cfg(test)]
mod organize_imports_tests {
    use super::*;

    #[test]
    fn combine_specifiers() {
        //
        let contents = r#"
          import { foo } from "./foo";
          import { bar } from "./foo";

          foo
        "#;
        //
        let expected = r#"
          import { bar, foo } from "./foo";

          foo
        "#;
        assert_organized(None, None, expected, contents);
    }

    #[test]
    fn maintain_comments() {
        //   // leading on ./foo
        //   // trailing on ./foo
        //
        let contents = r#"
          import { foo } from "./foo";

          foo
        "#;
        assert_organized(None, None, contents, contents);
    }

    #[test]
    fn move_comments() {
        //   // leading on ./foo
        //
        //   // leading on ./bar
        //
        let contents = r#"
          import { foo } from "./foo";

          import { bar } from "./bar";

          foo
        "#;
        //   // leading on ./bar
        //   // leading on ./foo
        //
        let expected = r#"
          import { bar } from "./bar";
          import { foo } from "./foo";

          foo
        "#;
        assert_organized(None, None, expected, contents);
    }

    #[test]
    fn combine_comments() {
        //   // comment on foo
        //   // comment on bar
        //
        let contents = r#"
          import { foo } from "./foo";
          import { bar } from "./foo";

          foo
        "#;
        let expected = r#"
          import { bar, foo } from "./foo";

          foo
        "#;
        assert_organized(None, None, expected, contents);
    }
}
