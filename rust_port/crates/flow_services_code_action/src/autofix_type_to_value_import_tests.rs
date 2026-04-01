/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/__tests__/autofix_type_to_value_import_tests.ml`

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
    use flow_parser::PERMISSIVE_PARSE_OPTIONS;
    use flow_parser::parse_program_without_file;
    let parse_options = Some(PERMISSIVE_PARSE_OPTIONS);
    let (ast, _errors) = parse_program_without_file(false, None, parse_options, Ok(contents));
    ast
}

fn assert_fixed(expected: &str, loc: flow_parser::loc::Loc, contents: &str) {
    let expected = dedent_trim(expected);
    let contents = dedent_trim(contents);
    let ast = parse(&contents);
    let new_ast = crate::autofix_type_to_value_import::convert_type_to_value_import(&ast, loc)
        .expect("convert_type_to_value_import returned None");
    let diff = crate::insert_type::mk_diff(&ast, &new_ast);
    let opts = flow_parser_utils_output::js_layout_generator::default_opts();
    let patch =
        flow_parser_utils_output::replacement_printer::mk_patch_ast_differ(&opts, &diff, &contents);
    let patched = flow_parser_utils_output::replacement_printer::print(&patch, &contents);
    assert_eq!(expected, patched);
}

fn mk_loc(
    (start_line, start_column): (i32, i32),
    (end_line, end_column): (i32, i32),
) -> flow_parser::loc::Loc {
    flow_parser::loc::Loc {
        source: None,
        start: flow_parser::loc::Position {
            line: start_line,
            column: start_column,
        },
        end: flow_parser::loc::Position {
            line: end_line,
            column: end_column,
        },
    }
}

#[cfg(test)]
mod simple_type_imports_tests {
    use super::*;

    #[test]
    fn named_specifiers_1() {
        let contents = "
        import type { Foo } from \"./a\";
        Foo;
      ";
        let expected = "
        import { Foo } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn named_specifiers_2() {
        let contents = "
        import type { Foo, Bar } from \"./a\";
        Foo;
      ";
        let expected = "
        import type { Bar } from \"./a\";
        import { Foo } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn named_specifiers_3() {
        let contents = "
        import type Bar, { Foo } from \"./a\";
        Foo;
      ";
        let expected = "
        import type Bar from \"./a\";
        import { Foo } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn named_specifiers_4() {
        let contents = "
        import type Bar, { Foo, Baz } from \"./a\";
        Foo;
      ";
        let expected = "
        import type Bar, { Baz } from \"./a\";
        import { Foo } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn default_1() {
        let contents = "
        import type Foo from \"./a\";
        Foo;
      ";
        let expected = "
        import Foo from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn default_2() {
        let contents = "
        import type Foo, { Bar } from \"./a\";
        Foo;
      ";
        let expected = "
        import type { Bar } from \"./a\";
        import Foo from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }
}

#[cfg(test)]
mod type_imports_in_value_imports_tests {
    use super::*;

    #[test]
    fn named_specifiers_1() {
        let contents = "
        import { type Foo } from \"./a\";
        Foo;
      ";
        let expected = "
        import { Foo } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }

    #[test]
    fn named_specifiers_2() {
        let contents = "
        import { type Foo, Bar } from \"./a\";
        Foo;
      ";
        let expected = "
        import { Foo, Bar } from \"./a\";
        Foo;
      ";
        assert_fixed(expected, mk_loc((2, 0), (2, 3)), contents);
    }
}
