// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils::ast_builder;
use serde_json::json;

use crate::indented_printer;
use crate::source_map::SourceMapError;
use crate::source_map::SourceMapTracker;

fn source_file() -> FileKey {
    FileKey::new(FileKeyInner::SourceFile("input.js".to_owned()))
}

fn location(line: i32, column: i32) -> ALoc {
    ALoc::of_loc(Loc {
        source: None,
        start: Position { line, column },
        end: Position { line, column },
    })
}

#[test]
fn synthetic_segments_end_previous_mappings() {
    let source = "first;\n'😀'; second;\nend;";
    let mut tracker = SourceMapTracker::new(&source_file(), source);
    tracker.record(Some(&location(2, 8)), None).unwrap();
    tracker.advance("outer(");
    tracker
        .record(Some(&location(3, 0)), Some(&"end".into()))
        .unwrap();
    tracker.advance("end");
    tracker.record(Some(&location(2, 8)), None).unwrap();
    tracker.advance(")");
    tracker
        .record(Some(&ALoc::of_loc(Loc::default())), None)
        .unwrap();
    tracker.advance("helper");
    tracker.record(Some(&location(2, 8)), None).unwrap();
    assert_eq!(
        serde_json::to_value(tracker.finish()).unwrap(),
        json!({
            "version": 3, "file": "input.js", "sources": ["input.js"],
            "sourcesContent": [source], "names": ["end"],
            "mappings": "AACM,MACNA,GADM,C,MAAA"
        })
    );
}

#[test]
fn tracks_surrogate_pairs_and_all_javascript_line_terminators() {
    let source = "😀a;\r\nb;\u{2028}c;\u{2029}a;";
    let mut tracker = SourceMapTracker::new(&source_file(), source);
    tracker.record(None, None).unwrap();
    tracker.advance("😀");
    tracker
        .record(Some(&location(1, 4)), Some(&"a".into()))
        .unwrap();
    tracker.advance("a\r");
    tracker.advance("\n");
    tracker
        .record(Some(&location(2, 0)), Some(&"b".into()))
        .unwrap();
    tracker.advance("b\u{2028}");
    tracker
        .record(Some(&location(3, 0)), Some(&"c".into()))
        .unwrap();
    tracker.advance("c\u{2029}");
    tracker
        .record(Some(&location(4, 0)), Some(&"a".into()))
        .unwrap();
    let map = serde_json::to_value(tracker.finish()).unwrap();
    assert_eq!(map["names"], json!(["a", "b", "c"]));
    assert_eq!(map["mappings"], "A,EAAEA;AACFC;AACAC;AACAF");
}

#[test]
fn generated_columns_include_indentation() {
    let mut tracker = SourceMapTracker::new(&source_file(), "a\nb");
    tracker
        .record(Some(&location(1, 0)), Some(&"a".into()))
        .unwrap();
    tracker.advance("a\n");
    tracker.advance_indent(2);
    tracker
        .record(Some(&location(2, 0)), Some(&"b".into()))
        .unwrap();
    assert_eq!(
        serde_json::to_value(tracker.finish()).unwrap()["mappings"],
        "AAAAA;EACAC"
    );
}

#[test]
fn invalid_or_unresolved_locations_return_errors() {
    let file = source_file();
    for position in [
        Position { line: 0, column: 1 },
        Position {
            line: 1,
            column: -1,
        },
        Position { line: 1, column: 1 },
        Position {
            line: 1,
            column: 99,
        },
        Position { line: 2, column: 0 },
    ] {
        let mut tracker = SourceMapTracker::new(&file, "😀");
        assert!(
            matches!(tracker.record(Some(&location(position.line, position.column)), Some(&"x".into())), Err(SourceMapError::InvalidPosition(actual)) if actual == position)
        );
    }
    let foreign = FileKey::new(FileKeyInner::SourceFile("other.js".to_owned()));
    let loc = location(1, 0).update_source(|_| Some(foreign.dupe()));
    let mut tracker = SourceMapTracker::new(&file, "x");
    assert!(
        matches!(tracker.record(Some(&loc), None), Err(SourceMapError::ForeignSource { expected, actual }) if expected == file && actual == foreign)
    );
    let keyed: ALoc = serde_json::from_value(
        json!([null, {"line": 1, "column": i32::MIN}, {"line": 0, "column": 0}]),
    )
    .unwrap();
    assert!(matches!(
        tracker.record(Some(&keyed), None),
        Err(SourceMapError::KeyedLocation)
    ));
}

#[test]
fn empty_programs_do_not_create_mappings() {
    let program = ast_builder::test_program_of_string("");
    let (output, map) =
        indented_printer::print_with_source_map(&program, &source_file(), "").unwrap();
    assert_eq!(output.contents(), "\n");
    let map = serde_json::to_value(map).unwrap();
    assert_eq!(map["mappings"], "");
    assert_eq!(map["names"], json!([]));
    assert_eq!(map["sourcesContent"], json!([""]));
}

#[test]
fn tracking_preserves_direct_ast_printer_output() {
    for source in [
        "function f(x) { return `😀\r\n${x}\u{2028}last`; }",
        "function f(x) { if (x) throw new Error('bad'); return x + 1; }",
    ] {
        let program = ast_builder::test_program_of_string(source);
        let expected = indented_printer::print(&program).unwrap().contents();
        let (output, map) =
            indented_printer::print_with_source_map(&program, &source_file(), source).unwrap();
        assert_eq!(output.contents(), expected);
        let map = serde_json::to_value(map).unwrap();
        assert_eq!(map["sourcesContent"], json!([source]));
        assert_ne!(map["mappings"], "");
    }
}

#[test]
fn direct_ast_tokens_restore_parent_anchors_after_identifiers() {
    let source = "a + b;";
    let program = ast_builder::test_program_of_string(source);
    let (output, map) =
        indented_printer::print_with_source_map(&program, &source_file(), source).unwrap();
    assert_eq!(output.contents(), "((a + b));\n");
    // Columns 0/4/7 map to the binary start; named columns 2/6 map to a/b.
    assert_eq!(
        serde_json::to_value(map).unwrap(),
        json!({
            "version": 3, "file": "input.js", "sources": ["input.js"],
            "sourcesContent": [source], "names": ["a", "b"],
            "mappings": "AAAA,EAAAA,EAAA,EAAIC,CAAJ"
        })
    );
}
