/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::flow_import_specifier::Userland;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;

use crate::export_index;
use crate::export_index::Export;
use crate::export_index::Kind;
use crate::export_index::Source;

fn file_source(name: &str) -> Source {
    Source::FileKey(FileKey::new(FileKeyInner::SourceFile(name.to_string())))
}

fn file_lib(name: &str) -> Source {
    Source::FileKey(FileKey::new(FileKeyInner::LibFile(name.to_string())))
}

fn global() -> Source {
    Source::Global
}

fn declare_module(name: Userland) -> Source {
    Source::Builtin(name)
}

fn assert_exports(expected: &[(Source, Kind)], actual: &export_index::ExportMap<i32>) {
    let actual_keys: Vec<(Source, Kind)> = actual
        .keys()
        .map(|Export(source, kind)| (source.clone(), kind.clone()))
        .collect();
    assert_eq!(expected, actual_keys.as_slice());
}

#[test]
fn sorted_by_filename_ignoring_extension() {
    let file_a = file_source("path/to/a.js");
    let file_a_foo = file_source("path/to/a.foo.js");
    let file_b = file_source("path/to/b.js");
    let lib = file_lib("path/to/a.bar.js");

    // - libs are mixed together with source files
    // - a.js comes before a.bar.js which comes before a.foo.js, even
    //   though .j is lexographically after .b.
    let expected = [
        (file_a, Kind::Default),
        (lib, Kind::Default),
        (file_a_foo, Kind::Default),
        (file_b, Kind::Default),
    ];

    let mut index = export_index::empty();
    export_index::add(
        "foo",
        file_source("path/to/a.foo.js"),
        Kind::Default,
        &mut index,
    );
    export_index::add(
        "foo",
        file_source("path/to/b.js"),
        Kind::Default,
        &mut index,
    );
    export_index::add(
        "foo",
        file_source("path/to/a.js"),
        Kind::Default,
        &mut index,
    );
    export_index::add(
        "foo",
        file_lib("path/to/a.bar.js"),
        Kind::Default,
        &mut index,
    );

    let actual = export_index::find("foo", &index);

    assert_exports(&expected, &actual);
}

#[test]
fn compare_export() {
    let file_a = file_source("path/to/a.js");
    let file_b = file_source("path/to/b.js");
    let file_foo = file_source("path/to/foo.js");
    let builtin_z = declare_module(Userland::from_smol_str(FlowSmolStr::new("z")));

    // defaults before named before namespace, then
    // globals before builtins before source files
    let expected = [
        (builtin_z, Kind::Default),
        (file_foo.clone(), Kind::Default),
        (global(), Kind::Named),
        (file_a, Kind::Named),
        (file_b, Kind::Named),
        (file_foo, Kind::Namespace),
    ];

    let mut index = export_index::empty();
    export_index::add(
        "foo",
        declare_module(Userland::from_smol_str(FlowSmolStr::new("z"))),
        Kind::Default,
        &mut index,
    );
    export_index::add("foo", file_source("path/to/a.js"), Kind::Named, &mut index);
    export_index::add("foo", file_source("path/to/b.js"), Kind::Named, &mut index);
    export_index::add(
        "foo",
        file_source("path/to/foo.js"),
        Kind::Default,
        &mut index,
    );
    export_index::add(
        "foo",
        file_source("path/to/foo.js"),
        Kind::Namespace,
        &mut index,
    );
    export_index::add("foo", Source::Global, Kind::Named, &mut index);

    let actual = export_index::find("foo", &index);

    assert_exports(&expected, &actual);
}
