/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common_modulename::Modulename;
use flow_common_utils::utils_js;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::ResolvedRequires;
use flow_heap::parse::TypedParse;
use flow_heap::parsing_heaps::SharedMem;
use flow_imports_exports::exports::Export;
use flow_imports_exports::exports::Exports;
use flow_imports_exports::imports;
use flow_monitor_rpc::monitor_rpc;
use flow_monitor_rpc::server_status;
use flow_parser::file_key::FileKey;

use crate::export_index;
use crate::export_index::ExportIndex;
use crate::export_index::Kind;
use crate::export_index::Source;
use crate::export_search;
use crate::export_search::ExportSearch;

fn inferred_name_of_modulename(module_name: &str) -> FlowSmolStr {
    // for filenames: /foo/bar/baz.bliffl.js -> baz.bliff.js
    // for strings: @example/foo -> foo
    let str = std::path::Path::new(module_name)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(module_name);
    // remove suffixes, e.g. baz.bliffl.js -> baz
    let stripped = match str.find('.') {
        Some(index) => &str[..index],
        None => str,
    };
    // convert hyphens to camel case
    FlowSmolStr::new(utils_js::camelize(stripped))
}

// In this module, we will generate a stream of resolved module information, for indexing star
// re-exports.
// The stream starts from the module we want to export. Given a mref with in the module, it will
// resolve information to reach the imported module's export, and so on.
// The stream is completely lazy. For modules without star re-exports, it will have zero cost.
mod module_resolution_lazy_stream {
    use super::*;

    pub(super) struct StreamResult<'a> {
        pub(super) typed_parse: TypedParse,
        pub(super) file_key: FileKey,
        pub(super) next: Box<dyn Fn(&str) -> Option<StreamResult<'a>> + 'a>,
        pub(super) shared_mem: &'a SharedMem,
    }

    pub(super) fn get_dep_and_next_resolver<'a>(
        shared_mem: &'a SharedMem,
        file_key: FileKey,
        typed_parse: TypedParse,
    ) -> Box<dyn Fn(&str) -> Option<StreamResult<'a>> + 'a> {
        Box::new(move |mref: &str| next(shared_mem, &file_key, &typed_parse, mref))
    }

    fn next<'a>(
        shared_mem: &'a SharedMem,
        _file_key: &FileKey,
        typed_parse: &TypedParse,
        mref: &str,
    ) -> Option<StreamResult<'a>> {
        let specifier = FlowImportSpecifier::userland(FlowSmolStr::new(mref));
        let resolved_requires: ResolvedRequires = typed_parse.resolved_requires_unsafe();
        let resolved_modules = resolved_requires.get_resolved_modules();
        let requires = typed_parse.requires();
        let idx = requires.iter().position(|r| *r == specifier);
        let dependency = idx
            .and_then(|i| resolved_modules.get(i))
            .and_then(|rm| rm.to_result().ok());
        let dependency = dependency?;
        let dep_file_addr = shared_mem.get_provider(&dependency)?;
        let dep_parse = shared_mem.get_typed_parse(&dep_file_addr)?;
        let dep_file_key = dep_file_addr;
        let next_fn: Box<dyn Fn(&str) -> Option<StreamResult<'a>> + 'a> = {
            let dep_file_key = dep_file_key.dupe();
            let dep_parse = dep_parse.dupe();
            Box::new(move |mref: &str| next(shared_mem, &dep_file_key, &dep_parse, mref))
        };
        Some(StreamResult {
            typed_parse: dep_parse,
            file_key: dep_file_key,
            next: next_fn,
            shared_mem,
        })
    }
}

fn entries_of_exports_helper<'a>(
    module_name: &str,
    get_dep_and_next_resolver: &dyn Fn(
        &str,
    )
        -> Option<module_resolution_lazy_stream::StreamResult<'a>>,
    visited_deps: &mut BTreeSet<FileKey>,
    include_values: bool,
    exports: &Exports,
    acc: &mut Vec<(FlowSmolStr, Kind)>,
) {
    let mut has_named = false;
    for export in exports.iter() {
        match export {
            Export::DefaultType(name_opt) => {
                let name_from_modulename = inferred_name_of_modulename(module_name);
                acc.push((name_from_modulename.dupe(), Kind::DefaultType));
                match name_opt {
                    Some(name) if name.as_str() != name_from_modulename.as_str() => {
                        acc.push((name.dupe(), Kind::DefaultType));
                    }
                    _ => {}
                }
            }
            Export::Default(name_opt) => {
                if include_values {
                    let name_from_modulename = inferred_name_of_modulename(module_name);
                    acc.push((name_from_modulename.dupe(), Kind::Default));
                    match name_opt {
                        Some(name) if name.as_str() != name_from_modulename.as_str() => {
                            acc.push((name.dupe(), Kind::Default));
                        }
                        _ => {}
                    }
                }
            }
            Export::Named(name) => {
                if include_values {
                    has_named = true;
                    acc.push((name.dupe(), Kind::Named));
                }
            }
            Export::NamedType(name) => {
                acc.push((name.dupe(), Kind::NamedType));
            }
            Export::Module(module_specifier, sub_exports) => {
                let sub_module_name = match module_specifier {
                    FlowImportSpecifier::Userland(u) => u.display(),
                    FlowImportSpecifier::HasteImportWithSpecifiedNamespace { name, .. } => {
                        name.as_str()
                    }
                };
                let sub_exports = Exports::new(sub_exports.clone());
                entries_of_exports_helper(
                    sub_module_name,
                    get_dep_and_next_resolver,
                    visited_deps,
                    include_values,
                    &sub_exports,
                    acc,
                );
            }
            Export::ReExportModule(mref) => {
                with_reexports(
                    mref,
                    module_name,
                    get_dep_and_next_resolver,
                    visited_deps,
                    include_values,
                    acc,
                );
            }
            Export::ReExportModuleTypes(mref) => {
                with_reexports(
                    mref,
                    module_name,
                    get_dep_and_next_resolver,
                    visited_deps,
                    false,
                    acc,
                );
            }
        }
    }
    if has_named {
        acc.push((inferred_name_of_modulename(module_name), Kind::Namespace));
    }
}

fn with_reexports<'a>(
    mref: &Userland,
    module_name: &str,
    get_dep_and_next_resolver: &dyn Fn(
        &str,
    )
        -> Option<module_resolution_lazy_stream::StreamResult<'a>>,
    visited_deps: &mut BTreeSet<FileKey>,
    include_values: bool,
    acc: &mut Vec<(FlowSmolStr, Kind)>,
) {
    match get_dep_and_next_resolver(mref.display()) {
        None => {}
        Some(module_resolution_lazy_stream::StreamResult {
            typed_parse: _parse,
            file_key,
            next,
            shared_mem,
        }) => {
            if visited_deps.contains(&file_key) {
                return;
            }
            visited_deps.insert(file_key.dupe());
            let dep_exports = shared_mem.get_exports_unsafe(&file_key);
            entries_of_exports_helper(
                module_name,
                &*next,
                visited_deps,
                include_values,
                &dep_exports,
                acc,
            );
        }
    }
}

fn entries_of_exports<'a>(
    module_name: &str,
    get_dep_and_next_resolver: &dyn Fn(
        &str,
    )
        -> Option<module_resolution_lazy_stream::StreamResult<'a>>,
    exports: &Exports,
) -> Vec<(FlowSmolStr, Kind)> {
    let mut visited_deps = BTreeSet::new();
    let mut acc = Vec::new();
    entries_of_exports_helper(
        module_name,
        get_dep_and_next_resolver,
        &mut visited_deps,
        true,
        exports,
        &mut acc,
    );
    acc
}

// [add_exports ~source ~module_name exports index] adds [exports] to [index].
// For default and namespace exports, [module_name] is used as the exported name
// (converted to a valid identifier firist).
fn add_exports<'a>(
    source: &Source,
    module_name: &str,
    get_dep_and_next_resolver: &dyn Fn(
        &str,
    )
        -> Option<module_resolution_lazy_stream::StreamResult<'a>>,
    exports: &Exports,
    index: &mut ExportIndex,
) {
    let names = entries_of_exports(module_name, get_dep_and_next_resolver, exports);
    for (name, kind) in names {
        export_index::add(&name, source.clone(), kind, index);
    }
}

fn add_imports(
    imports_list: &imports::Imports,
    resolved_requires: &ResolvedRequires,
    requires: &[FlowImportSpecifier],
    shared_mem: &SharedMem,
    index: &mut ExportIndex,
) {
    for import in imports_list.iter() {
        match &import.source {
            imports::Source::Global => {
                let name = &import.export;
                export_index::add(name, Source::Global, Kind::NamedType, index);
                export_index::add(name, Source::Global, Kind::Named, index);
            }
            imports::Source::UnresolvedSource(mref) => {
                let idx = requires.iter().position(|r| r == mref);
                let result = idx.and_then(|i| resolved_requires.get_resolved_modules().get(i));

                let kind_and_name = |module_name: &Userland| -> (Kind, FlowSmolStr) {
                    match &import.kind {
                        imports::Kind::Default => (Kind::Default, import.export.dupe()),
                        imports::Kind::Named => (Kind::Named, import.export.dupe()),
                        imports::Kind::Namespace => {
                            (Kind::Namespace, FlowSmolStr::new(module_name.display()))
                        }
                        imports::Kind::NamedType => (Kind::NamedType, import.export.dupe()),
                        imports::Kind::Unknown => panic!("Unknown Kind"),
                    }
                };

                match result {
                    Some(rm) => match rm.to_result() {
                        Ok(dependency) => {
                            let module_name = dependency.to_modulename();
                            let inferred = inferred_name_of_modulename(module_name.as_str());
                            let userland = Userland::from_smol_str(inferred);
                            let (kind, name) = kind_and_name(&userland);
                            match &module_name {
                                Modulename::Filename(fn_key) => {
                                    let file_key = Source::FileKey(fn_key.dupe());
                                    export_index::add(&name, file_key, kind, index);
                                }
                                Modulename::Haste(_) => {
                                    if let Some(file) = shared_mem.get_provider(&dependency) {
                                        let file_key = Source::FileKey(file);
                                        export_index::add(&name, file_key, kind, index);
                                    }
                                }
                            }
                        }
                        Err(Some(FlowImportSpecifier::Userland(u))) => {
                            let (kind, name) = kind_and_name(&u);
                            export_index::add(&name, Source::Builtin(u), kind, index);
                        }
                        Err(Some(FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                            ..
                        })) => {}
                        Err(None) => {
                            if let FlowImportSpecifier::Userland(u) = mref {
                                let (kind, name) = kind_and_name(u);
                                export_index::add(&name, Source::Builtin(u.dupe()), kind, index);
                            }
                        }
                    },
                    None => {}
                }
            }
        }
    }
}

// [add_exports_of_checked_file file_key parse haste_info index] extracts the
// exports of [file_key] from its [parse] entry in shared memory. [haste_info]
// is used to fetch the module name from [file_key].
fn add_exports_of_checked_file(
    shared_mem: &SharedMem,
    file_key: &FileKey,
    typed_parse: &TypedParse,
    haste_info: Option<&flow_common_modulename::HasteModuleInfo>,
    index: &mut ExportIndex,
) {
    let source = Source::FileKey(file_key.dupe());
    let exports = shared_mem.get_exports_unsafe(file_key);
    let module_name = match haste_info {
        Some(info) => Modulename::Haste(info.clone()),
        None => Modulename::Filename(files::chop_flow_ext(file_key)),
    };
    let module_name_str = module_name.as_str();
    let get_dep_and_next_resolver = module_resolution_lazy_stream::get_dep_and_next_resolver(
        shared_mem,
        file_key.dupe(),
        typed_parse.dupe(),
    );
    add_exports(
        &source,
        module_name_str,
        &*get_dep_and_next_resolver,
        &exports,
        index,
    );
}

// Adds builtins to [index]. See [Exports.of_builtins] for how libdefs
// are converted as if they "export" things.
fn add_exports_of_builtins_inner(lib_exports: &Exports, index: &mut ExportIndex) {
    for export in lib_exports.iter() {
        match export {
            Export::Module(module_specifier, sub_exports) => match module_specifier {
                FlowImportSpecifier::Userland(module_name) => {
                    let source = Source::Builtin(module_name.dupe());
                    let sub_exports = Exports::new(sub_exports.clone());
                    add_exports(
                        &source,
                        module_name.display(),
                        &|_: &str| None,
                        &sub_exports,
                        index,
                    );
                }
                _ => {}
            },
            Export::Named(name) => {
                export_index::add(name, Source::Global, Kind::Named, index);
            }
            Export::NamedType(name) => {
                export_index::add(name, Source::Global, Kind::NamedType, index);
            }
            Export::DefaultType(_) => {}
            Export::Default(_) => {}
            Export::ReExportModule(_) => {}
            Export::ReExportModuleTypes(_) => {}
        }
    }
}

pub fn add_exports_of_builtins(
    lib_exports: &Exports,
    scoped_lib_exports: &[(impl AsRef<str>, Exports)],
    index: &mut ExportIndex,
) {
    add_exports_of_builtins_inner(lib_exports, index);
    for (_scoped_dir, lib_exports) in scoped_lib_exports {
        add_exports_of_builtins_inner(lib_exports, index);
    }
}

// [index_file ~reader (exports_to_add, exports_to_remove) file] reads the exports of [file] from
// shared memory and adds all of the current exports to [exports_to_add], and all of the
// previous exports to [exports_to_remove].
fn index_file(
    shared_mem: &SharedMem,
    file_key: &FileKey,
    new_available_exports: &mut ExportIndex,
    old_available_exports: &mut ExportIndex,
    imports_to_add: &mut ExportIndex,
    imports_to_remove: &mut ExportIndex,
) {
    if let flow_parser::file_key::FileKeyInner::ResourceFile(_) = file_key.inner() {
        return;
    }

    if let Some(old_parse) = shared_mem.get_typed_parse_committed(file_key) {
        let old_imports = shared_mem.get_imports_unsafe(file_key);
        let old_haste_info = shared_mem.get_haste_info_committed(file_key);
        let old_resolved_requires = old_parse.resolved_requires_unsafe();
        let old_requires = old_parse.requires();
        add_exports_of_checked_file(
            shared_mem,
            file_key,
            &old_parse,
            old_haste_info.as_ref(),
            old_available_exports,
        );
        add_imports(
            &old_imports,
            &old_resolved_requires,
            &old_requires,
            shared_mem,
            imports_to_remove,
        );
    }

    if let Some(new_parse) = shared_mem.get_typed_parse(file_key) {
        let new_imports = shared_mem.get_imports_unsafe(file_key);
        let new_haste_info = shared_mem.get_haste_info(file_key);
        let new_resolved_requires = new_parse.resolved_requires_unsafe();
        let new_requires = new_parse.requires();
        add_exports_of_checked_file(
            shared_mem,
            file_key,
            &new_parse,
            new_haste_info.as_ref(),
            new_available_exports,
        );
        add_imports(
            &new_imports,
            &new_resolved_requires,
            &new_requires,
            shared_mem,
            imports_to_add,
        );
    }
}

// Indexes all of the files in [parsed] and returns two [Export_index.t]'s: the first is
// all of the exports to add to the final index, and the second are to be removed.
// The latter is important because exports are indexed by the export name, not the
// filename; it would be expensive to walk the entire export index to check each exported
// name to see if a changed file used to export it. Instead, we re-index the previous
// version of the file to know what to remove.
pub fn index(
    shared_mem: &SharedMem,
    parsed: &BTreeSet<FileKey>,
) -> (ExportIndex, ExportIndex, ExportIndex, ExportIndex) {
    let total_count = parsed.len() as i32;
    let mut new_available_exports = export_index::empty();
    let mut old_available_exports = export_index::empty();
    let mut imports_to_add = export_index::empty();
    let mut imports_to_remove = export_index::empty();

    monitor_rpc::status_update(server_status::Event::IndexingProgress(
        server_status::Progress {
            finished: 0,
            total: Some(total_count),
        },
    ));

    for (finished, file_key) in parsed.iter().enumerate() {
        index_file(
            shared_mem,
            file_key,
            &mut new_available_exports,
            &mut old_available_exports,
            &mut imports_to_add,
            &mut imports_to_remove,
        );
        let finished = (finished + 1) as i32;
        monitor_rpc::status_update(server_status::Event::IndexingProgress(
            server_status::Progress {
                finished,
                total: Some(total_count),
            },
        ));
    }

    monitor_rpc::status_update(server_status::Event::IndexingPostProcess);

    (
        new_available_exports,
        old_available_exports,
        imports_to_add,
        imports_to_remove,
    )
}

// Initializes an [Export_search.t] with the exports of all of the [parsed] files
// as well as the builtin libdefs.
pub fn init(
    shared_mem: &SharedMem,
    libs: &(Exports, Vec<(String, Exports)>),
    parsed: &BTreeSet<FileKey>,
) -> ExportSearch {
    let (new_available_exports, _old_available_exports, imports_to_add, _imports_to_remove) =
        index(shared_mem, parsed);
    let mut exports_to_add = new_available_exports;
    let (lib_exports, scoped_lib_exports) = libs;
    add_exports_of_builtins(lib_exports, scoped_lib_exports, &mut exports_to_add);
    let final_export_index = export_index::merge_export_import(&imports_to_add, &exports_to_add);
    let search = export_search::init(final_export_index);
    monitor_rpc::status_update(server_status::Event::IndexingEnd);
    search
}

// [update ~changed previous] updates the exports for all of the [changed] files
// in the [previous] [Export_search.t].
pub fn update(
    shared_mem: &SharedMem,
    dirty_files: &BTreeSet<FileKey>,
    previous: &ExportSearch,
) -> ExportSearch {
    let (new_available_exports, old_available_exports, imports_to_add, imports_to_remove) =
        index(shared_mem, dirty_files);
    let result = export_search::merge_available_exports(
        &old_available_exports,
        &new_available_exports,
        previous,
    );
    let result = export_search::subtract_count(&imports_to_remove, &result);
    let result = export_search::merge_export_import(&imports_to_add, &result);
    monitor_rpc::status_update(server_status::Event::IndexingEnd);
    result
}

pub mod for_test {
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;

    pub fn inferred_name_of_modulename(module_name: &str) -> FlowSmolStr {
        super::inferred_name_of_modulename(module_name)
    }
}
