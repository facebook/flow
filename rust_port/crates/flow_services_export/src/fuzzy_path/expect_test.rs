/*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * Based on the TypeScript implementation from VSCode:
 * https://github.com/microsoft/vscode/blob/e79a401ba5f6bc8eff07bfffaf4544e96f394837/src/vs/base/test/common/filters.test.ts#L281
 * That implementation is...
 *
 *   Copyright (c) Microsoft Corporation. All rights reserved.
 */

use super::fuzzy_score;

fn ocaml_string_literal(s: &str) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\x08' => out.push_str("\\b"),
            c if (c as u32) < 0x20 || (c as u32) == 0x7f => {
                out.push_str(&format!("\\{:03}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[test]
fn fuzzy_score_test() {
    let mut buf = String::new();
    let mut test = |pattern: &str, word: &str| {
        let score = match fuzzy_score(true, false, pattern, word) {
            Some(score) => format!("{}", score),
            None => String::from("none"),
        };
        buf.push_str(&format!(
            "{} ~> {}: {}\n",
            ocaml_string_literal(pattern),
            ocaml_string_literal(word),
            score
        ));
    };
    test("a", "a");
    test("A", "A");
    test("A", "a");
    test("a", "A");
    test("a", "ab");
    test("A", "AB");
    test("a", "Ab");
    test("A", "Ab");
    test("ab", "ab");
    test("AB", "AB");
    test("ab", "abc");
    test("AB", "ABC");
    test("ab", "abA");
    test("CONST", "CONST");
    test("CONST", "CxOxNxSxTx");
    test("ABcD", "ABcD");
    test("ccm", "cacmelCase");
    test("bti", "the_black_knight");
    test("ccm", "camelCase");
    test("cmcm", "camelCase");
    test("cmcm", "camelCase");
    test("BK", "the_black_knight");
    test("KeyboardLayout=", "KeyboardLayout");
    test("LLL", "SVisualLoggerLogsList");
    test("LLLL", "SVilLoLosLi");
    test("LLLL", "SVisualLoggerLogsList");
    test("TEdit", "TextEdit");
    test("TEdit", "TextEditor");
    test("TEdit", "Textedit");
    test("TEdit", "text_edit");
    test("TEditDit", "TextEditorDecorationType");
    test("TEdit", "TextEditorDecorationType");
    test("Tedit", "TextEdit");
    test("ba", "?AB?");
    test("bkn", "the_black_knight");
    test("bt", "the_black_knight");
    test("ccm", "camelCasecm");
    test("fdm", "findModel");
    test("fob", "foobar");
    test("fobz", "foobar");
    test("foobar", "foobar");
    test("form", "editor.formatOnSave");
    test("g p", "Git: Pull");
    test("g p", "Git: Pull");
    test("gip", "Git: Pull");
    test("gip", "Git: Pull");
    test("gp", "Git: Pull");
    test("gp", "Git_Git_Pull");
    test("is", "ImportStatement");
    test("is", "isValid");
    test("lowrd", "lowWord");
    test("myvable", "myvariable");
    test("no", "");
    test("no", "match");
    test("ob", "foobar");
    test("sl", "SVisualLoggerLogsList");
    test("sllll", "SVisualLoggerLogsList");
    test("Three", "HTMLHRElement");
    test("Three", "Three");
    test("fo", "barfoo");
    test("fo", "bar_foo");
    test("fo", "bar_Foo");
    test("fo", "bar foo");
    test("fo", "bar.foo");
    test("fo", "bar/foo");
    test("fo", "bar\\foo");
    test("f", "Foo");
    test("tiat", "this_is_a_test");
    test("tiat", "ThisIsATest");
    test(
        "ab",
        "axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxbxax",
    );
    test(
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    );
    let expected = "\"a\" ~> \"a\": 4\n\
                    \"A\" ~> \"A\": 4\n\
                    \"A\" ~> \"a\": 2\n\
                    \"a\" ~> \"A\": 2\n\
                    \"a\" ~> \"ab\": 2\n\
                    \"A\" ~> \"AB\": 2\n\
                    \"a\" ~> \"Ab\": 0\n\
                    \"A\" ~> \"Ab\": 2\n\
                    \"ab\" ~> \"ab\": 12\n\
                    \"AB\" ~> \"AB\": 12\n\
                    \"ab\" ~> \"abc\": 10\n\
                    \"AB\" ~> \"ABC\": 10\n\
                    \"ab\" ~> \"abA\": 10\n\
                    \"CONST\" ~> \"CONST\": 36\n\
                    \"CONST\" ~> \"CxOxNxSxTx\": 14\n\
                    \"ABcD\" ~> \"ABcD\": 28\n\
                    \"ccm\" ~> \"cacmelCase\": -1\n\
                    \"bti\" ~> \"the_black_knight\": none\n\
                    \"ccm\" ~> \"camelCase\": none\n\
                    \"cmcm\" ~> \"camelCase\": none\n\
                    \"cmcm\" ~> \"camelCase\": none\n\
                    \"BK\" ~> \"the_black_knight\": -10\n\
                    \"KeyboardLayout=\" ~> \"KeyboardLayout\": none\n\
                    \"LLL\" ~> \"SVisualLoggerLogsList\": -8\n\
                    \"LLLL\" ~> \"SVilLoLosLi\": none\n\
                    \"LLLL\" ~> \"SVisualLoggerLogsList\": none\n\
                    \"TEdit\" ~> \"TextEdit\": 9\n\
                    \"TEdit\" ~> \"TextEditor\": 9\n\
                    \"TEdit\" ~> \"Textedit\": 5\n\
                    \"TEdit\" ~> \"text_edit\": 4\n\
                    \"TEditDit\" ~> \"TextEditorDecorationType\": 1\n\
                    \"TEdit\" ~> \"TextEditorDecorationType\": 9\n\
                    \"Tedit\" ~> \"TextEdit\": 7\n\
                    \"ba\" ~> \"?AB?\": none\n\
                    \"bkn\" ~> \"the_black_knight\": -8\n\
                    \"bt\" ~> \"the_black_knight\": -21\n\
                    \"ccm\" ~> \"camelCasecm\": -8\n\
                    \"fdm\" ~> \"findModel\": 1\n\
                    \"fob\" ~> \"foobar\": 5\n\
                    \"fobz\" ~> \"foobar\": none\n\
                    \"foobar\" ~> \"foobar\": 44\n\
                    \"form\" ~> \"editor.formatOnSave\": -4\n\
                    \"g p\" ~> \"Git: Pull\": 4\n\
                    \"g p\" ~> \"Git: Pull\": 4\n\
                    \"gip\" ~> \"Git: Pull\": 7\n\
                    \"gip\" ~> \"Git: Pull\": 7\n\
                    \"gp\" ~> \"Git: Pull\": -2\n\
                    \"gp\" ~> \"Git_Git_Pull\": -5\n\
                    \"is\" ~> \"ImportStatement\": -3\n\
                    \"is\" ~> \"isValid\": 10\n\
                    \"lowrd\" ~> \"lowWord\": 14\n\
                    \"myvable\" ~> \"myvariable\": 23\n\
                    \"no\" ~> \"\": none\n\
                    \"no\" ~> \"match\": none\n\
                    \"ob\" ~> \"foobar\": none\n\
                    \"sl\" ~> \"SVisualLoggerLogsList\": -4\n\
                    \"sllll\" ~> \"SVisualLoggerLogsList\": -8\n\
                    \"Three\" ~> \"HTMLHRElement\": none\n\
                    \"Three\" ~> \"Three\": 36\n\
                    \"fo\" ~> \"barfoo\": none\n\
                    \"fo\" ~> \"bar_foo\": -5\n\
                    \"fo\" ~> \"bar_Foo\": -5\n\
                    \"fo\" ~> \"bar foo\": -5\n\
                    \"fo\" ~> \"bar.foo\": -5\n\
                    \"fo\" ~> \"bar/foo\": -5\n\
                    \"fo\" ~> \"bar\\\\foo\": -5\n\
                    \"f\" ~> \"Foo\": 0\n\
                    \"tiat\" ~> \"this_is_a_test\": 1\n\
                    \"tiat\" ~> \"ThisIsATest\": 1\n\
                    \"ab\" ~> \"axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxbxax\": -125\n\
                    \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\" ~> \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\": 1020\n";
    assert_eq!(expected, buf);
}

/* Test that first_match_can_be_weak=true allows matches where the first
matched character is in a weak position (mid-word). With the default
first_match_can_be_weak=false, "ob" ~> "foobar" returns None because
'o' is not at a word boundary. With first_match_can_be_weak=true, it
should return Some score.

NOTE: If the bug in fuzzy_path_stubs.c line 196 is present
(Bool_val(first_match_can_be_weak) instead of Bool_val(first_match_can_be_weak_v)),
both columns will show "none" because the parameter is silently ignored. */
#[test]
fn first_match_can_be_weak_test() {
    let mut buf = String::new();
    let mut test_weak = |pattern: &str, word: &str| {
        let score_default = match fuzzy_score(true, false, pattern, word) {
            Some(score) => format!("{}", score),
            None => String::from("none"),
        };
        let score_weak = match fuzzy_score(true, true, pattern, word) {
            Some(score) => format!("{}", score),
            None => String::from("none"),
        };
        buf.push_str(&format!(
            "{} ~> {}: default={} weak={}\n",
            ocaml_string_literal(pattern),
            ocaml_string_literal(word),
            score_default,
            score_weak
        ));
    };
    test_weak("ob", "foobar");
    test_weak("fo", "barfoo");
    test_weak("ar", "foobar");
    test_weak("oo", "foobar");
    test_weak("fo", "foobar");
    let expected = "\"ob\" ~> \"foobar\": default=none weak=-9\n\
                    \"fo\" ~> \"barfoo\": default=none weak=-10\n\
                    \"ar\" ~> \"foobar\": default=none weak=-11\n\
                    \"oo\" ~> \"foobar\": default=none weak=-8\n\
                    \"fo\" ~> \"foobar\": default=10 weak=10\n";
    assert_eq!(expected, buf);
}
