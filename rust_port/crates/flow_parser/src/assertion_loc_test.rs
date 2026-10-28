/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::ast::expression::ExpressionInner;
use crate::ast::expression::OptionalCallKind;
use crate::ast::expression::OptionalMemberKind;
use crate::ast::statement::StatementInner;
use crate::estree_translator;
use crate::offset_utils::OffsetTable;

#[test]
fn chain_assertion_spans_are_recorded_before_continuations() {
    // Each continuation owns the span ending at its assertion, not its call/member end.
    for (source, asserted, pointer) in [
        (
            "value?.prop!.next;",
            "value?.prop!",
            "/body/0/expression/expression/object",
        ),
        (
            "value?.method!();",
            "value?.method!",
            "/body/0/expression/expression/callee",
        ),
        (
            "value?.prop![key];",
            "value?.prop!",
            "/body/0/expression/expression/object",
        ),
        (
            "value?.method!<number>();",
            "value?.method!",
            "/body/0/expression/expression/callee",
        ),
        (
            "value?.prop /* ! */ !.next;",
            "value?.prop /* ! */ !",
            "/body/0/expression/expression/object",
        ),
        (
            "value?.method!!();",
            "value?.method!!",
            "/body/0/expression/expression/callee",
        ),
        (
            "value?.[\"😀\"] /* ! */ !.next;",
            "value?.[\"😀\"] /* ! */ !",
            "/body/0/expression/expression/object",
        ),
    ] {
        let (program, errors) = crate::parse_program_without_file(false, None, None, Ok(source));
        assert!(errors.is_empty(), "invalid input {source}: {errors:?}");
        let StatementInner::Expression { inner, .. } = &*program.statements[0] else {
            panic!("expected expression statement");
        };
        let assertion_loc = match &*inner.expression {
            ExpressionInner::OptionalMember { inner, .. } => {
                let OptionalMemberKind::AssertNonnull { assertion_loc } = &inner.optional else {
                    panic!("expected member assertion");
                };
                assertion_loc
            }
            ExpressionInner::OptionalCall { inner, .. } => {
                let OptionalCallKind::AssertNonnull { assertion_loc } = &inner.optional else {
                    panic!("expected call assertion");
                };
                assertion_loc
            }
            _ => panic!("expected assertion continuation"),
        };
        assert_eq!(assertion_loc.start.line, 1);
        assert_eq!(assertion_loc.start.column, 0);
        assert_eq!(assertion_loc.end.line, 1);
        assert_eq!(assertion_loc.end.column as usize, asserted.len());

        // Native translation receives no source text and still emits exact UTF-16 spans.
        let value = estree_translator::program(
            &OffsetTable::make(source),
            &estree_translator::Config {
                include_locs: true,
                include_filename: false,
                offset_style: estree_translator::OffsetStyle::JsIndices,
            },
            &program,
        );
        let nonnull = value.pointer(pointer).expect("assertion expression");
        assert_eq!(nonnull["type"], "NonNullExpression");
        assert_eq!(
            nonnull["range"],
            serde_json::json!([0, asserted.encode_utf16().count()])
        );
        assert_eq!(
            nonnull["loc"]["end"]["column"],
            asserted.encode_utf16().count()
        );
    }
}

#[test]
fn comparison_fallback_retains_the_recorded_assertion_span() {
    // Both ambiguity modes retain the bang when < is a comparison, excluding trailing trivia.
    for ambiguous_types in [false, true] {
        let source = "value?.prop /* before */ ! /* after */ < other;";
        let value = crate::parse_to_json(
            Some(crate::ParseOptions {
                ambiguous_types,
                ..Default::default()
            }),
            false,
            Ok(source),
        );
        assert!(value.get("errors").is_none(), "parse errors: {value}");
        let comparison = &value["body"][0]["expression"];
        assert_eq!(comparison["type"], "BinaryExpression");
        let assertion = &comparison["left"]["expression"];
        assert_eq!(assertion["type"], "NonNullExpression");
        assert_eq!(
            assertion["range"],
            serde_json::json!([0, "value?.prop /* before */ !".len()])
        );
    }
}
