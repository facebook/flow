/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::parse_program_without_file;
use pretty_assertions::assert_eq;

use crate::file_sig::*;

fn visit(source: &str) -> FileSig {
    visit_with_opts(source, &FileSigOptions::default())
}

fn visit_with_opts(source: &str, opts: &FileSigOptions) -> FileSig {
    let (ast, _errors) = parse_program_without_file(true, None, None, Ok(source));
    let file_key = FileKey::new(FileKeyInner::SourceFile("test.js".to_string()));
    FileSig::from_program(&file_key, &ast, opts)
}

fn substring_loc<'a>(source: &'a str, loc: &Loc) -> &'a str {
    // Convert (line, column) positions to byte offsets
    let start_offset = position_to_offset(source, &loc.start);
    let end_offset = position_to_offset(source, &loc.end);
    &source[start_offset..end_offset]
}

fn position_to_offset(source: &str, pos: &Position) -> usize {
    let mut current_line = 1;
    let mut offset = 0;

    for (idx, ch) in source.char_indices() {
        if current_line == pos.line {
            // Found the target line, now add the column offset
            return idx + pos.column as usize;
        }
        if ch == '\n' {
            current_line += 1;
        }
        offset = idx + ch.len_utf8();
    }

    // If we're at or past the last line
    if current_line == pos.line {
        offset + pos.column as usize
    } else {
        source.len()
    }
}

#[test]
fn test_cjs_require() {
    let source = "const Foo = require('foo')";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            bindings: Some(RequireBindings::BindIdent(Identifier(ident_loc, ident_name))),
            prefix,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(ident_name.as_str(), "Foo");
            assert_eq!(prefix, &None);
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, require_loc), "require('foo')");
            assert_eq!(substring_loc(source, ident_loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_deep_requires() {
    let source = "let foo = {x: require('bar')}; func(foo, require('baz'));";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 2);

    match (&requires[0], &requires[1]) {
        (
            Require::Require {
                source: Source(bar_loc, bar_name),
                require_loc: req_bar_loc,
                bindings: None,
                ..
            },
            Require::Require {
                source: Source(baz_loc, baz_name),
                require_loc: req_baz_loc,
                bindings: None,
                ..
            },
        ) => {
            assert_eq!(bar_name.as_str(), "bar");
            assert_eq!(baz_name.as_str(), "baz");
            assert_eq!(substring_loc(source, bar_loc), "'bar'");
            assert_eq!(substring_loc(source, req_bar_loc), "require('bar')");
            assert_eq!(substring_loc(source, baz_loc), "'baz'");
            assert_eq!(substring_loc(source, req_baz_loc), "require('baz')");
        }
        _ => panic!("Unexpected requires"),
    }
}

#[test]
fn test_cjs_deep_requires_plus_bindings() {
    let source = "const Foo = require('foo'); func(Foo, require('bar'));";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 2);

    // Find the 'bar' require (without bindings)
    let bar_req = requires.iter().find(|r| {
        matches!(r,
            Require::Require {
                source: Source(_, name),
                bindings: None,
                ..
            } if name.as_str() == "bar"
        )
    });
    assert!(bar_req.is_some());
    if let Some(Require::Require {
        source: Source(bar_loc, _),
        require_loc: req_bar_loc,
        ..
    }) = bar_req
    {
        assert_eq!(substring_loc(source, bar_loc), "'bar'");
        assert_eq!(substring_loc(source, req_bar_loc), "require('bar')");
    }

    // Find the 'foo' require (with Foo binding)
    let foo_req = requires.iter().find(|r| {
        matches!(r,
            Require::Require {
                source: Source(_, name),
                bindings: Some(_),
                ..
            } if name.as_str() == "foo"
        )
    });
    assert!(foo_req.is_some());
    if let Some(Require::Require {
        source: Source(foo_loc, _),
        require_loc: req_foo_loc,
        bindings: Some(RequireBindings::BindIdent(Identifier(foo_id_loc, foo_id))),
        ..
    }) = foo_req
    {
        assert_eq!(substring_loc(source, foo_loc), "'foo'");
        assert_eq!(substring_loc(source, req_foo_loc), "require('foo')");
        assert_eq!(substring_loc(source, foo_id_loc), "Foo");
        assert_eq!(foo_id.as_str(), "Foo");
    }
}

#[test]
fn test_cjs_require_template_literal() {
    let source = "const Foo = require(`foo`)";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            bindings: Some(RequireBindings::BindIdent(Identifier(ident_loc, ident_name))),
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(ident_name.as_str(), "Foo");
            assert_eq!(substring_loc(source, source_loc), "`foo`");
            assert_eq!(substring_loc(source, require_loc), "require(`foo`)");
            assert_eq!(substring_loc(source, ident_loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_require_named() {
    let source = "const {foo, bar: baz} = require('foo');";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            bindings: Some(RequireBindings::BindNamed(map)),
            prefix: _,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, require_loc), "require('foo')");
            assert_eq!(map.len(), 2);

            // Find the foo binding
            let foo_binding = map
                .iter()
                .find(|(Identifier(_, name), _)| name.as_str() == "foo");
            assert!(foo_binding.is_some());
            if let Some((
                Identifier(foo_loc, _),
                RequireBindings::BindIdent(Identifier(foo_id_loc, foo_id)),
            )) = foo_binding
            {
                assert_eq!(substring_loc(source, foo_loc), "foo");
                assert_eq!(substring_loc(source, foo_id_loc), "foo");
                assert_eq!(foo_id.as_str(), "foo");
            }

            // Find the bar binding
            let bar_binding = map
                .iter()
                .find(|(Identifier(_, name), _)| name.as_str() == "bar");
            assert!(bar_binding.is_some());
            if let Some((
                Identifier(bar_loc, _),
                RequireBindings::BindIdent(Identifier(baz_loc, baz_name)),
            )) = bar_binding
            {
                assert_eq!(substring_loc(source, bar_loc), "bar");
                assert_eq!(substring_loc(source, baz_loc), "baz");
                assert_eq!(baz_name.as_str(), "baz");
            }
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_require_duplicate_remote() {
    let source = "const {foo: bar, foo: baz} = require('foo');";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            bindings: Some(RequireBindings::BindNamed(map)),
            prefix: _,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, require_loc), "require('foo')");
            assert_eq!(map.len(), 2);

            // Find foo bindings (should have 2 entries with same remote name "foo")
            let foo_bindings: Vec<_> = map
                .iter()
                .filter(|(Identifier(_, name), _)| name.as_str() == "foo")
                .collect();
            assert_eq!(foo_bindings.len(), 2);

            // Verify we have both bar and baz as local names
            let has_bar = foo_bindings.iter().any(|(_, binding)| {
                matches!(binding, RequireBindings::BindIdent(Identifier(_, name))
                    if name.as_str() == "bar")
            });
            let has_baz = foo_bindings.iter().any(|(_, binding)| {
                matches!(binding, RequireBindings::BindIdent(Identifier(_, name))
                    if name.as_str() == "baz")
            });
            assert!(has_bar);
            assert!(has_baz);
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_require_duplicate_local() {
    let source = "const {foo: bar, baz: bar} = require('foo');";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            bindings: Some(RequireBindings::BindNamed(map)),
            prefix: _,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, require_loc), "require('foo')");
            assert_eq!(map.len(), 2);

            // Find the baz binding
            let baz_binding = map
                .iter()
                .find(|(Identifier(_, name), _)| name.as_str() == "baz");
            assert!(baz_binding.is_some());
            if let Some((
                Identifier(baz_loc, _),
                RequireBindings::BindIdent(Identifier(bar_loc, bar_name)),
            )) = baz_binding
            {
                assert_eq!(substring_loc(source, bar_loc), "bar");
                assert_eq!(substring_loc(source, baz_loc), "baz");
                assert_eq!(bar_name.as_str(), "bar");
            }
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_require_in_export() {
    // An initial version of the change to ban non-toplevel exports failed to descend into the RHS
    // of export statements
    let source = "module.exports.foo = require('foo');";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, _),
            require_loc,
            bindings: None,
            ..
        } => {
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, require_loc), "require('foo')");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_cjs_require_typeapp() {
    let source = "const Foo = require<X>('foo')";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    // Should be empty - type application on require should not be recognized
    assert_eq!(requires.len(), 0);
}

#[test]
fn test_cjs_module_ref() {
    let source = "moduleRefConsumer('m#foo')";
    // Note: The Rust parser doesn't support module_ref_prefix parsing yet,
    // so this won't be recognized as a require
    let file_sig = visit(source);
    let requires = file_sig.requires();
    // TODO: When module_ref_prefix support is added to the Rust parser,
    // this should expect 1 require with prefix = Some("m#")
    assert_eq!(requires.len(), 0);
}

#[test]
fn test_relay_integration() {
    let source = "graphql`query foo {}`";
    let opts = FileSigOptions {
        enable_relay_integration: true,
        ..Default::default()
    };
    let file_sig = visit_with_opts(source, &opts);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo.graphql");
            assert_eq!(substring_loc(source, source_loc), "graphql`query foo {}`");
            assert_eq!(substring_loc(source, require_loc), "graphql`query foo {}`");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_relay_integration_module_prefix() {
    let source = "graphql`query foo {}`";
    let opts = FileSigOptions {
        enable_relay_integration: true,
        relay_integration_module_prefix: Some(FlowSmolStr::new("./__generated__/")),
        ..Default::default()
    };
    let file_sig = visit_with_opts(source, &opts);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Require {
            source: Source(source_loc, module_name),
            require_loc,
            ..
        } => {
            assert_eq!(module_name.as_str(), "./__generated__/foo.graphql");
            assert_eq!(substring_loc(source, source_loc), "graphql`query foo {}`");
            assert_eq!(substring_loc(source, require_loc), "graphql`query foo {}`");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_dynamic_import() {
    let source = "import('foo')";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ImportDynamic {
            source: Source(source_loc, module_name),
            import_loc,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
            assert_eq!(substring_loc(source, import_loc), "import('foo')");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_dynamic_import_template_literal() {
    let source = "import(`foo`)";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ImportDynamic {
            source: Source(source_loc, module_name),
            import_loc,
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "`foo`");
            assert_eq!(substring_loc(source, import_loc), "import(`foo`)");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import() {
    let source = "import 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import0 {
            source: Source(loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_default() {
    let source = "import Foo from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            named,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(named.len(), 1);

            let default_map = named.get("default").expect("default export");
            assert_eq!(default_map.len(), 1);

            let foo_locs = default_map.get("Foo").expect("Foo import");
            assert_eq!(foo_locs.len(), 1);

            let locs = &foo_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "Foo");
            assert_eq!(substring_loc(source, &locs.local_loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_named() {
    let source = "import {A} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            named,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(named.len(), 1);

            let a_map = named.get("A").expect("A export");
            assert_eq!(a_map.len(), 1);

            let a_locs = a_map.get("A").expect("A import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_renamed() {
    let source = "import {A as B} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            named,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(named.len(), 1);

            let a_map = named.get("A").expect("A export");
            assert_eq!(a_map.len(), 1);

            let b_locs = a_map.get("B").expect("B import");
            assert_eq!(b_locs.len(), 1);

            let locs = &b_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "B");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_named_type() {
    let source = "import {type A} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            types,
            named,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert!(named.is_empty());
            assert_eq!(types.len(), 1);

            let a_map = types.get("A").expect("A type export");
            assert_eq!(a_map.len(), 1);

            let a_locs = a_map.get("A").expect("A type import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_named_typeof() {
    let source = "import {typeof A} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            typesof,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(typesof.len(), 1);

            let a_map = typesof.get("A").expect("A typesof export");
            assert_eq!(a_map.len(), 1);

            let a_locs = a_map.get("A").expect("A typesof import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_ns() {
    let source = "import * as Foo from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            ns: Some(Identifier(loc, name)),
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(name.as_str(), "Foo");
            assert_eq!(substring_loc(source, loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_type() {
    let source = "import type A from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            types,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(types.len(), 1);

            let default_map = types.get("default").expect("default type export");
            assert_eq!(default_map.len(), 1);

            let a_locs = default_map.get("A").expect("A type import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_type_named() {
    let source = "import type {A} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            types,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(types.len(), 1);

            let a_map = types.get("A").expect("A type export");
            assert_eq!(a_map.len(), 1);

            let a_locs = a_map.get("A").expect("A type import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_type_renamed() {
    let source = "import type {A as B} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            types,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(types.len(), 1);

            let a_map = types.get("A").expect("A type export");
            assert_eq!(a_map.len(), 1);

            let b_locs = a_map.get("B").expect("B type import");
            assert_eq!(b_locs.len(), 1);

            let locs = &b_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "B");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_typeof() {
    let source = "import typeof A from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            typesof,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(typesof.len(), 1);

            let default_map = typesof.get("default").expect("default typesof export");
            assert_eq!(default_map.len(), 1);

            let a_locs = default_map.get("A").expect("A typesof import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_typeof_named() {
    let source = "import typeof {A} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            typesof,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(typesof.len(), 1);

            let a_map = typesof.get("A").expect("A typesof export");
            assert_eq!(a_map.len(), 1);

            let a_locs = a_map.get("A").expect("A typesof import");
            assert_eq!(a_locs.len(), 1);

            let locs = &a_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "A");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_typeof_renamed() {
    let source = "import typeof {A as B} from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            typesof,
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(typesof.len(), 1);

            let a_map = typesof.get("A").expect("A typesof export");
            assert_eq!(a_map.len(), 1);

            let b_locs = a_map.get("B").expect("B typesof import");
            assert_eq!(b_locs.len(), 1);

            let locs = &b_locs[0];
            assert_eq!(substring_loc(source, &locs.remote_loc), "A");
            assert_eq!(substring_loc(source, &locs.local_loc), "B");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_typesof_ns() {
    let source = "import typeof * as Foo from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            typesof_ns: Some(Identifier(loc, name)),
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(name.as_str(), "Foo");
            assert_eq!(substring_loc(source, loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_es_import_type_ns() {
    let source = "import type * as Foo from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::Import {
            source: Source(_, module_name),
            type_ns: Some(Identifier(loc, name)),
            ..
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(name.as_str(), "Foo");
            assert_eq!(substring_loc(source, loc), "Foo");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_export_star() {
    let source = "export * from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ExportFrom {
            source: Source(source_loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_export_type_star() {
    let source = "export type * from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ExportFrom {
            source: Source(source_loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_export_ns() {
    let source = "export * as ns from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ExportFrom {
            source: Source(source_loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_declare_export_star() {
    let source = "declare export * from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ExportFrom {
            source: Source(source_loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}

#[test]
fn test_declare_export_ns() {
    let source = "declare export * as ns from 'foo'";
    let file_sig = visit(source);
    let requires = file_sig.requires();
    assert_eq!(requires.len(), 1);

    match &requires[0] {
        Require::ExportFrom {
            source: Source(source_loc, module_name),
        } => {
            assert_eq!(module_name.as_str(), "foo");
            assert_eq!(substring_loc(source, source_loc), "'foo'");
        }
        _ => panic!("Unexpected require structure"),
    }
}
