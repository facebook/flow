/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_common::flow_import_specifier::Userland;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum Kind {
    DefaultType,
    Default,
    Named,
    NamedType,
    Namespace,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum Source {
    Global,
    // [Builtin "foo"] refers to a `declare module "foo"` lib
    Builtin(Userland),
    FileKey(FileKey),
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Source::Global => write!(f, "Global"),
            Source::Builtin(s) => write!(f, "Builtin {}", s.display()),
            Source::FileKey(fk) => write!(f, "{}", fk.as_str()),
        }
    }
}

// Custom ordering where the "kind" (LibFile vs SourceFile vs JsonFile, etc) does
// not matter, and we compare filenames without extensions, so that something like
// [Foo.example.js] sorts _after_ [Foo.js] even though [e] comes before [j]; the
// extension is less important than the rest of the basename and we should suggest
// [import ... from 'Foo'] before [import ... from 'Foo.example'].
fn compare_file_key(a: &FileKey, b: &FileKey) -> std::cmp::Ordering {
    let a = a.as_str();
    let b = b.as_str();
    fn chop_ext(s: &str) -> &str {
        match s.rfind('.') {
            Some(idx) => &s[..idx],
            None => s,
        }
    }
    let k = chop_ext(a).cmp(chop_ext(b));
    if k != std::cmp::Ordering::Equal {
        k
    } else {
        a.cmp(b)
    }
}

// Custom ordering where globals come first, followed by [declare module],
// followed by source files.
//
// Exports are sorted by both source and kind. After sorting by kind (defaults
// before named before namespace), we then want globals first, followed by
// declared modules (builtins). In this way, if there is a module named `Map`
// with a default export, it'll be suggested before the builtin `Map`, but
// some other module with a named `Map` export will be suggested after. This
// is so that named exports from random modules don't shadow common globals,
// but you can still shadow the builtins with an entire module (e.g. a Promise
// polyfill defined by the `Promise` module).
//
// TODO: this is a very coarse ranking. We could do much better. For example, we
// could track how commonly used each export is. For example, the `Promise` global
// is probably far more common than any source file exporting the same name.
fn compare_source(a: &Source, b: &Source) -> std::cmp::Ordering {
    match (a, b) {
        // globals first
        (Source::Global, Source::Global) => std::cmp::Ordering::Equal,
        (Source::Global, _) => std::cmp::Ordering::Less,
        (_, Source::Global) => std::cmp::Ordering::Greater,
        // builtins second
        (Source::Builtin(a), Source::Builtin(b)) => a.cmp(b),
        (Source::Builtin(_), _) => std::cmp::Ordering::Less,
        (_, Source::Builtin(_)) => std::cmp::Ordering::Greater,
        // user modules last
        (Source::FileKey(a), Source::FileKey(b)) => compare_file_key(a, b),
    }
}

impl PartialOrd for Source {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Source {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_source(self, other)
    }
}

// Order by kind and then by source.
//
// 1. Default exports from declared modules (builtins)
// 2. Default exports from user modules
// 3. Named exports from globals (e.g. `declare class Image`)
// 4. Named exports from declared modules
// 5. Named exports from user modules
// 6. Same for types
// 7. Namespaces of declared modules
// 8. Namespaces of user modules
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export(pub Source, pub Kind);

impl PartialOrd for Export {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Export {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let Export(a_source, a_kind) = self;
        let Export(b_source, b_kind) = other;
        let k = a_kind.cmp(b_kind);
        if k == std::cmp::Ordering::Equal {
            compare_source(a_source, b_source)
        } else {
            k
        }
    }
}

pub type ExportMap<V> = BTreeMap<Export, V>;

pub type ExportIndex = BTreeMap<FlowSmolStr, ExportMap<i32>>;

pub fn empty() -> ExportIndex {
    BTreeMap::new()
}

pub fn add(name: &str, source: Source, kind: Kind, t: &mut ExportIndex) {
    let exports = t.entry(FlowSmolStr::new(name)).or_default();
    let key = Export(source, kind);
    *exports.entry(key).or_insert(0) += 1;
}

pub fn merge(x: &ExportIndex, y: &ExportIndex) -> ExportIndex {
    let mut result = x.clone();
    for (name, y_exports) in y {
        let entry = result.entry(name.dupe()).or_default();
        for (export_key, y_count) in y_exports {
            *entry.entry(export_key.clone()).or_insert(0) += y_count;
        }
    }
    result
}

pub fn merge_export_import(add_index: &ExportIndex, t: &ExportIndex) -> ExportIndex {
    let mut acc = t.clone();
    for (name, add_exports) in add_index {
        let entry = acc.entry(name.dupe()).or_default();
        for (export_key, add_count) in add_exports {
            let Export(source, _) = export_key;
            if let Some(existing_count) = entry.get_mut(export_key) {
                *existing_count += add_count;
            } else {
                match source {
                    Source::FileKey(_) => {
                        entry.insert(export_key.clone(), *add_count);
                    }
                    Source::Global | Source::Builtin(_) => {}
                }
            }
        }
    }
    acc
}

pub fn fold_names<Acc>(
    f: &mut impl FnMut(Acc, &str, &ExportMap<i32>) -> Acc,
    init: Acc,
    t: &ExportIndex,
) -> Acc {
    let mut acc = init;
    for (name, exports) in t {
        acc = f(acc, name, exports);
    }
    acc
}

pub fn fold<Acc>(f: &mut impl FnMut(Acc, &str, &Export) -> Acc, init: Acc, t: &ExportIndex) -> Acc {
    fold_names(
        &mut |acc, name, exports: &ExportMap<i32>| {
            let mut acc = acc;
            for export in exports.keys() {
                acc = f(acc, name, export);
            }
            acc
        },
        init,
        t,
    )
}

pub fn map(f: impl Fn(&i32) -> i32, t: &ExportIndex) -> ExportIndex {
    t.iter()
        .map(|(name, exports)| {
            let new_exports = exports.iter().map(|(k, v)| (k.clone(), f(v))).collect();
            (name.dupe(), new_exports)
        })
        .collect()
}

// Returns tuple of two disjoint index: (addition_index, removal_index),
// to be passed to [merge] and [subtract].
pub fn diff(old_index: &ExportIndex, new_index: &ExportIndex) -> (ExportIndex, ExportIndex) {
    let exist_in_index = |export_name: &str, export_key: &Export, map: &ExportIndex| -> bool {
        match map.get(export_name) {
            None => false,
            Some(map) => map.contains_key(export_key),
        }
    };
    let diff_index = |l: &ExportIndex, r: &ExportIndex| -> ExportIndex {
        let mut acc: ExportIndex = BTreeMap::new();
        for (export_name, map) in l {
            for export_key in map.keys() {
                if exist_in_index(export_name, export_key, r) {
                } else {
                    let Export(s, k) = export_key;
                    add(export_name, s.clone(), k.clone(), &mut acc);
                }
            }
        }
        acc
    };
    (
        diff_index(new_index, old_index),
        diff_index(old_index, new_index),
    )
}

// [subtract to_remove t] removes all of the exports in [to_remove] from [t], and
// also returns a list of keys that no longer are exported by any file.
pub fn subtract(old_t: &ExportIndex, t: &ExportIndex) -> (ExportIndex, Vec<FlowSmolStr>) {
    let mut result = t.clone();
    let mut dead_names: Vec<FlowSmolStr> = Vec::new();
    for (name, files_to_remove) in old_t {
        match result.get(name) {
            Some(files) => {
                let updated: ExportMap<i32> = files
                    .iter()
                    .filter(|(key, _value)| !files_to_remove.contains_key(*key))
                    .map(|(k, v)| (k.clone(), *v))
                    .collect();
                if updated.is_empty() {
                    result.remove(name.as_str());
                    dead_names.push(name.dupe());
                } else {
                    result.insert(name.dupe(), updated);
                }
            }
            None => {}
        }
    }
    (result, dead_names)
}

pub fn subtract_count(rem: &ExportIndex, t: &ExportIndex) -> ExportIndex {
    let mut acc = t.clone();
    for (name, rem_exports) in rem {
        if let Some(existing) = acc.get_mut(name) {
            for (export_key, rem_count) in rem_exports {
                if let Some(n) = existing.get_mut(export_key) {
                    *n -= rem_count;
                }
            }
        }
    }
    acc
}

// [find name t] returns all of the [(file_key, kind)] tuples that export [name]
pub fn find(name: &str, t: &ExportIndex) -> ExportMap<i32> {
    match t.get(name) {
        Some(exports) => exports.clone(),
        None => BTreeMap::new(),
    }
}

pub fn find_seq(name: &str, t: &ExportIndex) -> Vec<(Export, i32)> {
    match t.get(name) {
        Some(t) => t.iter().map(|(k, v)| (k.clone(), *v)).collect(),
        None => Vec::new(),
    }
}

// [keys t] returns all of the exported names from every file in [t]
pub fn keys(t: &ExportIndex) -> Vec<FlowSmolStr> {
    t.keys().duped().collect()
}

pub fn kind_is_value(kind: &Kind) -> bool {
    match kind {
        Kind::Default | Kind::Named | Kind::Namespace => true,
        Kind::DefaultType | Kind::NamedType => false,
    }
}

pub fn kind_is_type(kind: &Kind) -> bool {
    match kind {
        Kind::Default | Kind::Named | Kind::Namespace => false,
        Kind::DefaultType | Kind::NamedType => true,
    }
}
