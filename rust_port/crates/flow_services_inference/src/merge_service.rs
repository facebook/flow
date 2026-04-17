/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::Cell;
use std::cell::LazyCell;
use std::cell::OnceCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocTable;
use flow_common::docblock::Docblock;
use flow_common::docblock::FlowMode;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common::options::Options;
use flow_common_cycle_hash as cycle_hash;
use flow_common_utils::graph::Graph;
use flow_common_xx as xx;
use flow_common_xx::content_hash_of;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::ResolvedModule;
use flow_heap::parse::MergeHashes;
use flow_heap::parse::TypedParse;
use flow_heap::parsing_heaps::SharedMem;
use flow_heap::parsing_heaps::merge_context_mutator;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_server_files::server_files_js;
use flow_services_coverage::FileCoverage;
use flow_services_coverage::file_coverage;
use flow_type_sig::compact_table::Index;
use flow_type_sig::compact_table::Table;
use flow_type_sig::type_sig_hash;
use flow_type_sig::type_sig_hash::CheckedDep;
use flow_type_sig::type_sig_hash::Dependency;
use flow_type_sig::type_sig_hash::ReadHash;
use flow_type_sig::type_sig_pack::ModuleKind;
use flow_typing::merge::get_lint_severities;
use flow_typing::type_inference::scan_for_suppressions;
use flow_typing_context::Context;
use flow_typing_context::MasterContext;
use flow_typing_context::Metadata;
use flow_typing_context::OverridableMetadata;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_type::type_::Type;
use flow_utils_concurrency::thread_pool::ThreadPool;
use once_cell::unsync::Lazy;
use vec1::Vec1;

use crate::check_cache::CheckCache;
use crate::check_service;
use crate::inference_utils;
use crate::merge_stream::Component;
use crate::merge_stream::MergeResult;
use crate::merge_stream::MergeStream;

pub type UnitResult<A> = Result<A, (ALoc, InternalError)>;

pub struct SigOptsData {
    pub skipped_count: usize,
    pub sig_new_or_changed: FlowOrdSet<FileKey>,
}

pub type MergeResults<A> = (Vec<A>, SigOptsData);

fn append_to_server_log(options: &Options, message: &str) {
    let log_file = std::env::var("FLOW_LOG_FILE").unwrap_or_else(|_| {
        server_files_js::log_file(
            &options.flowconfig_name,
            options.temp_dir.as_str(),
            options.root.as_path(),
        )
    });
    if let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_file)
    {
        let _ = writeln!(file, "{}", message);
    }
}

pub fn sig_hash(
    check_dirty_set: bool,
    _root: &Path,
    shared_mem: &SharedMem,
    component: &Vec1<FileKey>,
) -> u64 {
    type ComponentRec = Rc<OnceCell<Vec<Rc<CheckedDep<Rc<cycle_hash::Node>>>>>>;

    fn hash_file_key(file_key: &FileKey) -> u64 {
        xx::hash(file_key.as_str().as_bytes(), 0)
    }

    let resource_dep = |f: &FileKey| -> Dependency {
        let ext = std::path::Path::new(f.as_str())
            .extension()
            .map(|e| format!(".{}", e.to_string_lossy()))
            .expect("resource file without extension");
        let hash = xx::hash(ext.as_bytes(), 0);
        Dependency::Resource(Box::new(move || hash))
    };

    let acyclic_dep = |dep_key: &FileKey, dep_parse: &TypedParse| -> CheckedDep<ReadHash> {
        let filename_hash = hash_file_key(dep_key);
        let filename: ReadHash = Box::new(move || filename_hash);

        match dep_parse.get_merge_hashes() {
            Some(MergeHashes::CJS {
                type_export_hashes,
                exports_hash,
                ns_hash,
            }) => {
                let type_exports = type_export_hashes
                    .into_iter()
                    .map(|(k, h)| {
                        let rh: ReadHash = Box::new(move || h);
                        (k, rh)
                    })
                    .collect();
                let exports = exports_hash.map(|h| {
                    let rh: ReadHash = Box::new(move || h);
                    rh
                });
                let ns_h = ns_hash;
                let ns: ReadHash = Box::new(move || ns_h);
                CheckedDep::CJS {
                    filename,
                    type_exports,
                    exports,
                    ns,
                }
            }
            Some(MergeHashes::ES {
                type_export_hashes,
                export_hashes,
                ns_hash,
            }) => {
                let type_exports = type_export_hashes
                    .into_iter()
                    .map(|(k, h)| {
                        let rh: ReadHash = Box::new(move || h);
                        (k, rh)
                    })
                    .collect();
                let exports = export_hashes
                    .into_iter()
                    .map(|(k, h)| {
                        let rh: ReadHash = Box::new(move || h);
                        (k, rh)
                    })
                    .collect();
                let ns_h = ns_hash;
                let ns: ReadHash = Box::new(move || ns_h);
                CheckedDep::ES {
                    filename,
                    type_exports,
                    exports,
                    ns,
                }
            }
            None => {
                fn structural_hash<T: std::hash::Hash>(item: &T) -> ReadHash {
                    let hash = content_hash_of(item);
                    Box::new(move || hash)
                }
                let module = dep_parse.type_sig_unsafe(dep_key);
                match &module.module_kind {
                    ModuleKind::CJSModule {
                        type_exports,
                        exports,
                        info,
                    } => {
                        let te = info
                            .type_export_keys
                            .iter()
                            .enumerate()
                            .map(|(i, key)| (key.dupe(), structural_hash(&type_exports[i])))
                            .collect();
                        let exp = exports.as_ref().map(|exp| structural_hash(exp));
                        let ns = structural_hash(info);
                        CheckedDep::CJS {
                            filename,
                            type_exports: te,
                            exports: exp,
                            ns,
                        }
                    }
                    ModuleKind::ESModule {
                        type_exports,
                        exports,
                        ts_pending: _,
                        info,
                    } => {
                        let te = info
                            .type_export_keys
                            .iter()
                            .enumerate()
                            .map(|(i, key)| (key.dupe(), structural_hash(&type_exports[i])))
                            .collect();
                        let exp = info
                            .export_keys
                            .iter()
                            .enumerate()
                            .map(|(i, key)| (key.dupe(), structural_hash(&exports[i])))
                            .collect();
                        let ns = structural_hash(info);
                        CheckedDep::ES {
                            filename,
                            type_exports: te,
                            exports: exp,
                            ns,
                        }
                    }
                }
            }
        }
    };

    let cyclic_dep = |file_key: &FileKey,
                      parse: &TypedParse,
                      file: &Rc<type_sig_hash::File>|
     -> CheckedDep<Rc<cycle_hash::Node>> {
        let filename_hash = hash_file_key(file_key);
        let filename: ReadHash = Box::new(move || filename_hash);
        let module = parse.type_sig_unsafe(file_key);

        match &module.module_kind {
            ModuleKind::CJSModule {
                type_exports,
                exports,
                info,
            } => {
                let type_export_nodes: Vec<Rc<cycle_hash::Node>> = type_exports
                    .iter()
                    .map(|texport| {
                        let init_hash = content_hash_of(texport);
                        let hash = Rc::new(Cell::new(init_hash));
                        let hash_r = hash.dupe();
                        let read_hash: ReadHash = Box::new(move || hash_r.get());
                        let hash_w = hash.dupe();
                        let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                        let texport = texport.clone();
                        let file = file.dupe();
                        let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                            Box::new(move |edge, _dep_edge| {
                                type_sig_hash::visit_type_export(edge, &file, &texport);
                            });
                        Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
                    })
                    .collect();

                let exports_node: Option<Rc<cycle_hash::Node>> = exports.as_ref().map(|packed| {
                    let init_hash = content_hash_of(packed);
                    let hash = Rc::new(Cell::new(init_hash));
                    let hash_r = hash.dupe();
                    let read_hash: ReadHash = Box::new(move || hash_r.get());
                    let hash_w = hash.dupe();
                    let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                    let packed = packed.clone();
                    let file = file.dupe();
                    let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                        Box::new(move |edge, dep_edge| {
                            type_sig_hash::visit_packed(edge, dep_edge, &file, &packed);
                        });
                    Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
                });

                let ns_init_hash = content_hash_of(info);
                let ns_hash = Rc::new(Cell::new(ns_init_hash));
                let ns_hash_r = ns_hash.dupe();
                let ns_read: ReadHash = Box::new(move || ns_hash_r.get());
                let ns_hash_w = ns_hash.dupe();
                let ns_write: cycle_hash::WriteHash = Box::new(move |h| ns_hash_w.set(h));
                let te_nodes_for_ns = type_export_nodes.clone();
                let exp_node_for_ns = exports_node.clone();
                let type_stars = info.type_stars.clone();
                let file_for_ns = file.dupe();
                let ns_visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                    Box::new(move |edge, dep_edge| {
                        for te in &te_nodes_for_ns {
                            edge(te.dupe());
                        }
                        if let Some(exp) = &exp_node_for_ns {
                            edge(exp.dupe());
                        }
                        for (_, index) in &type_stars {
                            type_sig_hash::edge_import_ns(edge, dep_edge, &file_for_ns, *index);
                        }
                    });
                let ns = Rc::new(cycle_hash::create_node(ns_visit, ns_read, ns_write));

                let type_export_map: BTreeMap<FlowSmolStr, Rc<cycle_hash::Node>> = info
                    .type_export_keys
                    .iter()
                    .zip(type_export_nodes)
                    .map(|(key, node)| (key.dupe(), node))
                    .collect();

                CheckedDep::CJS {
                    filename,
                    type_exports: type_export_map,
                    exports: exports_node,
                    ns,
                }
            }

            ModuleKind::ESModule {
                type_exports,
                exports,
                ts_pending,
                info,
            } => {
                let type_export_nodes: Vec<Rc<cycle_hash::Node>> = type_exports
                    .iter()
                    .map(|texport| {
                        let init_hash = content_hash_of(texport);
                        let hash = Rc::new(Cell::new(init_hash));
                        let hash_r = hash.dupe();
                        let read_hash: ReadHash = Box::new(move || hash_r.get());
                        let hash_w = hash.dupe();
                        let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                        let texport = texport.clone();
                        let file = file.dupe();
                        let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                            Box::new(move |edge, _dep_edge| {
                                type_sig_hash::visit_type_export(edge, &file, &texport);
                            });
                        Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
                    })
                    .collect();

                let export_nodes: Vec<Rc<cycle_hash::Node>> = exports
                    .iter()
                    .map(|export| {
                        let init_hash = content_hash_of(export);
                        let hash = Rc::new(Cell::new(init_hash));
                        let hash_r = hash.dupe();
                        let read_hash: ReadHash = Box::new(move || hash_r.get());
                        let hash_w = hash.dupe();
                        let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                        let export = export.clone();
                        let file = file.dupe();
                        let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                            Box::new(move |edge, dep_edge| {
                                type_sig_hash::visit_export(edge, dep_edge, &file, &export);
                            });
                        Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
                    })
                    .collect();

                let ts_pending_nodes: Vec<Rc<cycle_hash::Node>> = ts_pending
                    .iter()
                    .map(|pending| {
                        let init_hash = content_hash_of(pending);
                        let hash = Rc::new(Cell::new(init_hash));
                        let hash_r = hash.dupe();
                        let read_hash: ReadHash = Box::new(move || hash_r.get());
                        let hash_w = hash.dupe();
                        let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                        let pending = pending.clone();
                        let file = file.dupe();
                        let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                            Box::new(move |edge, dep_edge| {
                                type_sig_hash::visit_ts_pending_export(
                                    edge, dep_edge, &file, &pending,
                                );
                            });
                        Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
                    })
                    .collect();

                let ns_init_hash = content_hash_of(info);
                let ns_hash = Rc::new(Cell::new(ns_init_hash));
                let ns_hash_r = ns_hash.dupe();
                let ns_read: ReadHash = Box::new(move || ns_hash_r.get());
                let ns_hash_w = ns_hash.dupe();
                let ns_write: cycle_hash::WriteHash = Box::new(move |h| ns_hash_w.set(h));
                let te_nodes_for_ns = type_export_nodes.clone();
                let exp_nodes_for_ns = export_nodes.clone();
                let ts_pending_nodes_for_ns = ts_pending_nodes.clone();
                let type_stars = info.type_stars.clone();
                let stars = info.stars.clone();
                let file_for_ns = file.dupe();
                let ns_visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                    Box::new(move |edge, dep_edge| {
                        for te in &te_nodes_for_ns {
                            edge(te.dupe());
                        }
                        for exp in &exp_nodes_for_ns {
                            edge(exp.dupe());
                        }
                        for tp in &ts_pending_nodes_for_ns {
                            edge(tp.dupe());
                        }
                        for (_, index) in &type_stars {
                            type_sig_hash::edge_import_ns(edge, dep_edge, &file_for_ns, *index);
                        }
                        for (_, index) in &stars {
                            type_sig_hash::edge_import_ns(edge, dep_edge, &file_for_ns, *index);
                        }
                    });
                let ns = Rc::new(cycle_hash::create_node(ns_visit, ns_read, ns_write));

                let type_export_map: BTreeMap<FlowSmolStr, Rc<cycle_hash::Node>> = info
                    .type_export_keys
                    .iter()
                    .zip(type_export_nodes)
                    .map(|(key, node)| (key.dupe(), node))
                    .collect();

                let export_map: BTreeMap<FlowSmolStr, Rc<cycle_hash::Node>> = info
                    .export_keys
                    .iter()
                    .zip(export_nodes)
                    .map(|(key, node)| (key.dupe(), node))
                    .collect();

                CheckedDep::ES {
                    filename,
                    type_exports: type_export_map,
                    exports: export_map,
                    ns,
                }
            }
        }
    };

    let file_dependency = |component_rec: &ComponentRec,
                           component_map: &BTreeMap<FileKey, usize>,
                           resolved_module: &ResolvedModule|
     -> Dependency {
        match resolved_module.to_result() {
            Err(_) => Dependency::Unchecked,
            Ok(dep_ref) => match shared_mem.get_provider(&dep_ref) {
                None => Dependency::Unchecked,
                Some(dep_file) => {
                    if matches!(dep_file.inner(), FileKeyInner::ResourceFile(_)) {
                        resource_dep(&dep_file)
                    } else {
                        match shared_mem.get_typed_parse(&dep_file) {
                            None => Dependency::Unchecked,
                            Some(dep_parse) => {
                                if let Some(&i) = component_map.get(&dep_file) {
                                    let component_rec = component_rec.dupe();
                                    Dependency::Cyclic(Lazy::new(Box::new(move || {
                                        component_rec.get().unwrap()[i].dupe()
                                    })))
                                } else {
                                    let dep_key = dep_file.dupe();
                                    Dependency::Acyclic(Lazy::new(Box::new(move || {
                                        acyclic_dep(&dep_key, &dep_parse)
                                    })))
                                }
                            }
                        }
                    }
                }
            },
        }
    };

    let component_file = |component_rec: &ComponentRec,
                          component_map: &BTreeMap<FileKey, usize>,
                          file_key: &FileKey,
                          parse: &TypedParse|
     -> Rc<CheckedDep<Rc<cycle_hash::Node>>> {
        let module = parse.type_sig_unsafe(file_key);

        let resolved_modules_map: BTreeMap<Userland, ResolvedModule> = {
            let requires = parse.requires();
            let resolved_requires = parse.resolved_requires_unsafe();
            let resolved = resolved_requires.get_resolved_modules();
            requires
                .iter()
                .zip(resolved.iter())
                .filter_map(|(spec, resolved)| match spec {
                    FlowImportSpecifier::Userland(u) => Some((u.dupe(), resolved.clone())),
                    _ => None,
                })
                .collect()
        };

        let dependencies: Table<Dependency> = Table::init(module.module_refs.len(), |i| {
            let mref = module.module_refs.get(Index::<Userland>::new(i));
            match resolved_modules_map.get(mref) {
                Some(resolved) => file_dependency(component_rec, component_map, resolved),
                None => Dependency::Unchecked,
            }
        });

        let file_cell: Rc<OnceCell<Rc<type_sig_hash::File>>> = Rc::new(OnceCell::new());

        let local_defs = {
            let dirty_indices = &module.dirty_local_defs;
            Table::init(module.local_defs.len(), |i| {
                let def = module.local_defs.get(Index::<()>::new(i));
                let mut hash_val = content_hash_of(def);
                if check_dirty_set && dirty_indices.contains(&i) {
                    hash_val = !hash_val;
                }
                let hash = Rc::new(Cell::new(hash_val));
                let hash_r = hash.dupe();
                let read_hash: ReadHash = Box::new(move || hash_r.get());
                let hash_w = hash.dupe();
                let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                let def = def.clone();
                let file_cell = file_cell.dupe();
                let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                    Box::new(move |edge, dep_edge| {
                        let file = file_cell.get().unwrap();
                        type_sig_hash::visit_def(edge, dep_edge, file, &def);
                    });
                Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
            })
        };

        let remote_refs = Table::init(module.remote_refs.len(), |i| {
            let rref = module.remote_refs.get(Index::<()>::new(i));
            let hash_val = content_hash_of(rref);
            let hash = Rc::new(Cell::new(hash_val));
            let hash_r = hash.dupe();
            let read_hash: ReadHash = Box::new(move || hash_r.get());
            let hash_w = hash.dupe();
            let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
            let rref = rref.clone();
            let file_cell = file_cell.dupe();
            let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                Box::new(move |edge, dep_edge| {
                    let file = file_cell.get().unwrap();
                    type_sig_hash::visit_remote_ref(edge, dep_edge, file, &rref);
                });
            Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
        });

        let pattern_defs = {
            let dirty_indices = &module.dirty_pattern_defs;
            Table::init(module.pattern_defs.len(), |i| {
                let pdef = module.pattern_defs.get(Index::<()>::new(i));
                let mut hash_val = content_hash_of(pdef);
                if check_dirty_set && dirty_indices.contains(&i) {
                    hash_val = !hash_val;
                }
                let hash = Rc::new(Cell::new(hash_val));
                let hash_r = hash.dupe();
                let read_hash: ReadHash = Box::new(move || hash_r.get());
                let hash_w = hash.dupe();
                let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
                let pdef = pdef.clone();
                let file_cell = file_cell.dupe();
                let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                    Box::new(move |edge, dep_edge| {
                        let file = file_cell.get().unwrap();
                        type_sig_hash::visit_packed(edge, dep_edge, file, &pdef);
                    });
                Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
            })
        };

        let patterns = Table::init(module.patterns.len(), |i| {
            let pattern = module.patterns.get(Index::<()>::new(i));
            let hash_val = content_hash_of(pattern);
            let hash = Rc::new(Cell::new(hash_val));
            let hash_r = hash.dupe();
            let read_hash: ReadHash = Box::new(move || hash_r.get());
            let hash_w = hash.dupe();
            let write_hash: cycle_hash::WriteHash = Box::new(move |h| hash_w.set(h));
            let pattern = pattern.clone();
            let file_cell = file_cell.dupe();
            let visit: Box<dyn Fn(&dyn Fn(Rc<cycle_hash::Node>), &dyn Fn(&ReadHash))> =
                Box::new(move |edge, _dep_edge| {
                    let file = file_cell.get().unwrap();
                    type_sig_hash::visit_pattern(edge, file, &pattern);
                });
            Rc::new(cycle_hash::create_node(visit, read_hash, write_hash))
        });

        let file = Rc::new(type_sig_hash::File {
            dependencies,
            local_defs,
            remote_refs,
            pattern_defs,
            patterns,
        });
        assert!(file_cell.set(file.dupe()).is_ok(), "Should be initialized");

        Rc::new(cyclic_dep(file_key, parse, &file))
    };

    let component_vec: Vec<(FileKey, TypedParse)> = component
        .iter()
        .filter_map(|f| shared_mem.get_typed_parse(f).map(|p| (f.dupe(), p)))
        .collect();

    if component_vec.is_empty() {
        return 0u64;
    }

    let component_map: BTreeMap<FileKey, usize> = component_vec
        .iter()
        .enumerate()
        .map(|(i, (f, _))| (f.dupe(), i))
        .collect();

    let component_rec: ComponentRec = Rc::new(OnceCell::new());
    let files: Vec<Rc<CheckedDep<Rc<cycle_hash::Node>>>> = component_vec
        .iter()
        .map(|(file_key, parse)| component_file(&component_rec, &component_map, file_key, parse))
        .collect();
    assert!(component_rec.set(files).is_ok(), "Should be initialized");

    let cx = cycle_hash::create_cx();
    let mut component_hash = 0u64;

    for checked_dep in component_rec.get().unwrap() {
        match checked_dep.as_ref() {
            CheckedDep::CJS { ns, .. } | CheckedDep::ES { ns, .. } => {
                cycle_hash::root(&cx, ns);
                let file_hash = cycle_hash::read_hash(ns);
                component_hash ^= file_hash;
            }
        }
    }

    for (i, checked_dep) in component_rec.get().unwrap().iter().enumerate() {
        let (file_key, parse) = &component_vec[i];
        let merge_hashes = match checked_dep.as_ref() {
            CheckedDep::CJS {
                type_exports,
                exports,
                ns,
                ..
            } => MergeHashes::CJS {
                type_export_hashes: type_exports
                    .iter()
                    .map(|(k, node)| (k.dupe(), cycle_hash::read_hash(node)))
                    .collect(),
                exports_hash: exports.as_ref().map(|node| cycle_hash::read_hash(node)),
                ns_hash: cycle_hash::read_hash(ns),
            },
            CheckedDep::ES {
                type_exports,
                exports,
                ns,
                ..
            } => MergeHashes::ES {
                type_export_hashes: type_exports
                    .iter()
                    .map(|(k, node)| (k.dupe(), cycle_hash::read_hash(node)))
                    .collect(),
                export_hashes: exports
                    .iter()
                    .map(|(k, node)| (k.dupe(), cycle_hash::read_hash(node)))
                    .collect(),
                ns_hash: cycle_hash::read_hash(ns),
            },
        };
        parse.set_merge_hashes(merge_hashes);
        let _ = file_key;
    }

    component_hash
}

fn merge_component(
    shared_mem: &SharedMem,
    options: &Options,
    for_find_all_refs: bool,
    component: Vec1<FileKey>,
) -> (bool, Option<(ErrorSuppressions, f64)>) {
    let start_time = Instant::now();

    let typed_component: Vec<_> = component
        .iter()
        .filter_map(|f| shared_mem.get_typed_parse(f).map(|p| (f.dupe(), p)))
        .collect();

    if typed_component.is_empty() {
        return (false, None);
    }

    let hash = sig_hash(for_find_all_refs, &options.root, shared_mem, &component);

    let mut suppressions = ErrorSuppressions::empty();
    for (file, typed_parse) in &typed_component {
        let docblock = typed_parse.docblock_unsafe(file);
        let metadata = Metadata {
            overridable: OverridableMetadata {
                strict: docblock.flow == Some(FlowMode::OptInStrict),
                strict_local: docblock.flow == Some(FlowMode::OptInStrictLocal),
                ..Default::default()
            },
            ..Default::default()
        };
        let lint_severities = get_lint_severities(
            &metadata,
            &options.strict_mode,
            options.lint_severities.clone(), // clone needed: LintSettings doesn't implement Dupe
        );
        let ast = typed_parse.ast_unsafe(file);
        let (_, new_suppressions, _) = scan_for_suppressions(
            false,
            &lint_severities,
            vec![(file.dupe(), &ast.all_comments)],
        );
        suppressions.union(new_suppressions);
    }

    let diff = merge_context_mutator::add_merge_on_diff(for_find_all_refs, &typed_component, hash);
    let duration = start_time.elapsed().as_secs_f64();

    (diff, Some((suppressions, duration)))
}

pub type CheckFileResult = (
    (
        Context<'static>,
        Arc<flow_type_sig::packed_type_sig::Module<Loc>>,
        Arc<FileSig>,
        ast::Program<ALoc, (ALoc, Type)>,
    ),
    (ErrorSet, ErrorSet, ErrorSuppressions, FileCoverage, f64),
);

fn mk_check_file(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: &MasterContext,
) -> (
    Box<dyn FnMut(FileKey) -> Option<CheckFileResult>>,
    Rc<std::cell::RefCell<CheckCache<'static>>>,
) {
    let cache = Rc::new(std::cell::RefCell::new(CheckCache::create(10_000_000)));
    let cache_ref = cache.dupe();
    let check_service::CheckFileAndCompEnv {
        mut make_cx,
        mut check_file,
        compute_env: _,
    } = check_service::mk_check_file(shared_mem.dupe(), options, master_cx, cache);

    let check_file_fn = Box::new(move |file: FileKey| {
        let start_time = Instant::now();
        let parse = shared_mem.get_typed_parse(&file)?;
        let ast = parse.ast_unsafe(&file);
        let type_sig = parse.type_sig_unsafe(&file);
        let (file_sig, tolerable_errors) = parse.tolerable_file_sig_unsafe(&file);
        let docblock = parse.docblock_unsafe(&file);
        let aloc_table: flow_aloc::LazyALocTable = {
            let file_for_aloc = file.dupe();
            let parse_for_aloc = parse.dupe();
            Rc::new(LazyCell::new(Box::new(move || {
                Rc::new(ALocTable::unpack(
                    file_for_aloc.dupe(),
                    &parse_for_aloc.aloc_table_unsafe(&file_for_aloc),
                ))
            })
                as Box<dyn FnOnce() -> Rc<ALocTable>>))
        };
        let resolved_modules: BTreeMap<FlowImportSpecifier, ResolvedModule> = {
            let requires = parse.requires();
            let resolved_requires = parse.resolved_requires_unsafe();
            let resolved = resolved_requires.get_resolved_modules();
            requires
                .iter()
                .zip(resolved.iter())
                .map(|(specifier, module)| (specifier.dupe(), module.clone()))
                .collect()
        };
        let cx = make_cx(
            file.dupe(),
            resolved_modules,
            ast.dupe(),
            docblock,
            aloc_table,
        );
        let ast_ref = ast;
        let ast::Program {
            all_comments: comments,
            ..
        } = ast_ref.as_ref();
        let mut mapper = flow_aloc::LocToALocMapper;
        let Ok(aloc_ast) = flow_parser::polymorphic_ast_mapper::program(&mut mapper, &ast_ref);
        let metadata = cx.metadata().clone();
        let typed_ast = check_file(&cx, &file, file_sig.dupe(), &metadata, comments, &aloc_ast);
        drop(aloc_ast);
        drop(ast_ref);
        let coverage = file_coverage(&cx, &typed_ast);
        let errors = cx.errors();
        let tolerable_error_set =
            inference_utils::set_of_file_sig_tolerable_errors(file.dupe(), &tolerable_errors);
        let errors = errors.union(&tolerable_error_set);
        let mut suppressions = cx.error_suppressions().clone();
        let severity_cover = cx.severity_cover().dupe();
        let include_suppressions = cx.include_suppressions();
        let aloc_tables: HashMap<FileKey, flow_aloc::LazyALocTable> = cx.aloc_tables().clone();
        let (errors, warnings) =
            suppressions.filter_lints(errors, &aloc_tables, include_suppressions, &severity_cover);
        cx.post_inference_cleanup();
        #[cfg(debug_assertions)]
        {
            let count = cx.strong_count();
            if count > 2 {
                eprintln!(
                    "[LEAK-DEBUG] Context for {} has strong_count={} after cleanup (expected <=2)",
                    file.as_str(),
                    count
                );
            }
        }
        let duration = start_time.elapsed().as_secs_f64();
        if duration > 5.0 {
            eprintln!("[SLOW-CHECK] {:>8.3}s  {}", duration, file.as_str());
        }
        Some((
            (cx, type_sig, file_sig, typed_ast),
            (errors, warnings, suppressions, coverage, duration),
        ))
    });
    (check_file_fn, cache_ref)
}

thread_local! {
    static CHECK_CONTENTS_CACHE: Rc<std::cell::RefCell<CheckCache<'static>>> =
        Rc::new(std::cell::RefCell::new(CheckCache::create(10_000)));
}

pub fn check_contents_cache() -> Rc<std::cell::RefCell<CheckCache<'static>>> {
    CHECK_CONTENTS_CACHE.with(|cache| cache.dupe())
}

pub fn check_contents_context(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: Arc<MasterContext>,
    file: FileKey,
    ast: Arc<ast::Program<Loc, Loc>>,
    docblock: Arc<Docblock>,
    file_sig: Arc<FileSig>,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> (Context<'static>, ast::Program<ALoc, (ALoc, Type)>) {
    let aloc_table: flow_aloc::LazyALocTable = {
        let file_for_aloc = file.dupe();
        let shared_mem_for_aloc = shared_mem.dupe();
        Rc::new(LazyCell::new(Box::new(move || {
            match shared_mem_for_aloc.get_aloc_table(&file_for_aloc) {
                Some(packed) => Rc::new(ALocTable::unpack(file_for_aloc.dupe(), &packed)),
                None => Rc::new(ALocTable::empty(file_for_aloc.dupe())),
            }
        })
            as Box<dyn FnOnce() -> Rc<ALocTable>>))
    };
    let resolved_modules: BTreeMap<FlowImportSpecifier, ResolvedModule> = file_sig
        .require_loc_map()
        .into_keys()
        .map(|mref| {
            let result = flow_services_module::imported_module(
                &options,
                &shared_mem,
                node_modules_containers,
                &file,
                None,
                &mref,
            );
            (mref, ResolvedModule::from_result(result))
        })
        .collect();
    let check_service::CheckFileAndCompEnv {
        mut make_cx,
        mut check_file,
        compute_env: _,
    } = CHECK_CONTENTS_CACHE.with(|cache| {
        check_service::mk_check_file(
            shared_mem.dupe(),
            options.dupe(),
            master_cx.as_ref(),
            cache.dupe(),
        )
    });
    let cx = make_cx(
        file.dupe(),
        resolved_modules,
        ast.dupe(),
        docblock,
        aloc_table,
    );
    let ast_ref = ast;
    let ast::Program {
        all_comments: comments,
        ..
    } = ast_ref.as_ref();
    let mut mapper = flow_aloc::LocToALocMapper;
    let Ok(aloc_ast) = flow_parser::polymorphic_ast_mapper::program(&mut mapper, &ast_ref);
    let metadata = cx.metadata().clone();
    let typed_ast = check_file(&cx, &file, file_sig, &metadata, comments, &aloc_ast);
    (cx, typed_ast)
}

pub fn compute_env_of_contents(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: Arc<MasterContext>,
    file: FileKey,
    ast: Arc<ast::Program<Loc, Loc>>,
    docblock: Arc<Docblock>,
    file_sig: Arc<FileSig>,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> (Context<'static>, ast::Program<ALoc, ALoc>) {
    let aloc_table: flow_aloc::LazyALocTable = {
        let file_for_aloc = file.dupe();
        let shared_mem_for_aloc = shared_mem.dupe();
        Rc::new(LazyCell::new(Box::new(move || {
            match shared_mem_for_aloc.get_aloc_table(&file_for_aloc) {
                Some(packed) => Rc::new(ALocTable::unpack(file_for_aloc.dupe(), &packed)),
                None => Rc::new(ALocTable::empty(file_for_aloc.dupe())),
            }
        })
            as Box<dyn FnOnce() -> Rc<ALocTable>>))
    };
    let resolved_modules: BTreeMap<FlowImportSpecifier, ResolvedModule> = file_sig
        .require_loc_map()
        .into_keys()
        .map(|mref| {
            let result = flow_services_module::imported_module(
                &options,
                &shared_mem,
                node_modules_containers,
                &file,
                None,
                &mref,
            );
            (mref, ResolvedModule::from_result(result))
        })
        .collect();
    let check_service::CheckFileAndCompEnv {
        mut make_cx,
        check_file: _,
        mut compute_env,
    } = CHECK_CONTENTS_CACHE.with(|cache| {
        check_service::mk_check_file(
            shared_mem.dupe(),
            options.dupe(),
            master_cx.as_ref(),
            cache.dupe(),
        )
    });
    let cx = make_cx(
        file.dupe(),
        resolved_modules,
        ast.dupe(),
        docblock,
        aloc_table,
    );
    let mut mapper = flow_aloc::LocToALocMapper;
    let Ok(aloc_ast) = flow_parser::polymorphic_ast_mapper::program(&mut mapper, &ast);
    compute_env(&cx, &aloc_ast);
    (cx, aloc_ast)
}

fn merge_job<A, F>(
    shared_mem: &SharedMem,
    options: &Options,
    for_find_all_refs: bool,
    job: &F,
    batch: Vec<Component>,
) -> MergeResult<A>
where
    F: Fn(&SharedMem, &Options, bool, Vec1<FileKey>) -> (bool, A),
{
    let results = batch
        .into_iter()
        .map(|Component(component)| {
            let leader = component.first().dupe();
            let component_cloned = (*component).clone();
            let (diff, result) = job(shared_mem, options, for_find_all_refs, component_cloned);
            (leader, diff, result)
        })
        .collect();
    MergeResult(results)
}

pub fn merge_runner<A, F>(
    pool: &ThreadPool,
    shared_mem: &SharedMem,
    options: &Options,
    for_find_all_refs: bool,
    sig_dependency_graph: &Graph<FileKey>,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
    job: F,
) -> MergeResults<A>
where
    A: Send + Sync + Default + std::fmt::Debug + 'static,
    F: Fn(&SharedMem, &Options, bool, Vec1<FileKey>) -> (bool, A) + Send + Sync + 'static,
{
    let num_workers = pool.num_workers();
    let stream = Arc::new(MergeStream::<A>::new(
        num_workers,
        sig_dependency_graph,
        components,
        recheck_set,
    ));
    stream.update_server_status();

    let stream_ref = stream.dupe();
    let stream_merge = stream.dupe();

    let shared_mem_clone = shared_mem.dupe();

    let results: Vec<A> = flow_utils_concurrency::map_reduce::call(
        pool,
        move || stream_ref.next(),
        move |acc: &mut Vec<A>, batch: Vec<Component>| {
            let merged = merge_job(shared_mem_clone, options, for_find_all_refs, &job, batch);
            let new_acc = stream_merge.merge(merged, std::mem::take(acc));
            *acc = new_acc;
        },
        |a, mut b| {
            a.append(&mut b);
        },
    );

    let total_files = stream.total_files();
    let skipped_count = stream.skipped_count();
    let sig_new_or_changed = stream.sig_new_or_changed();
    let message = format!("Merge skipped {} of {} modules", skipped_count, total_files);

    eprintln!("{}", message);
    log::info!("{}", message);
    append_to_server_log(options, &message);

    (
        results,
        SigOptsData {
            skipped_count,
            sig_new_or_changed,
        },
    )
}

pub fn merge(
    pool: &ThreadPool,
    shared_mem: &SharedMem,
    options: &Options,
    for_find_all_refs: bool,
    sig_dependency_graph: &Graph<FileKey>,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
) -> MergeResults<Option<(ErrorSuppressions, f64)>> {
    merge_runner(
        pool,
        shared_mem,
        options,
        for_find_all_refs,
        sig_dependency_graph,
        components,
        recheck_set,
        |shared_mem, options, for_find_all_refs, component| {
            merge_component(shared_mem, options, for_find_all_refs, component)
        },
    )
}

pub fn mk_check(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: &MasterContext,
) -> (
    Box<dyn FnMut(FileKey) -> UnitResult<Option<CheckFileResult>>>,
    Rc<std::cell::RefCell<CheckCache<'static>>>,
) {
    let (mut check_file, cache) = mk_check_file(shared_mem.dupe(), options.dupe(), master_cx);
    let check_fn = Box::new(move |file: FileKey| {
        let result: Result<Option<CheckFileResult>, _> =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| check_file(file.dupe())));
        match result {
            Ok(ok) => Ok(ok),
            Err(panic_payload) => {
                let is_speculative_error = panic_payload
                    .downcast_ref::<flow_typing_flow_common::flow_js_utils::SpeculativeError>()
                    .is_some();

                let is_critical = panic_payload
                    .downcast_ref::<String>()
                    .map(|s| s.as_str())
                    .or_else(|| panic_payload.downcast_ref::<&str>().copied())
                    .map(|s| {
                        s.contains("Worker_should_cancel")
                            || s.contains("out of shared memory")
                            || s.contains("heap full")
                            || s.contains("hash table full")
                    })
                    .unwrap_or(false);
                if is_critical {
                    std::panic::resume_unwind(panic_payload);
                }

                let exn_str: String = if is_speculative_error {
                    format!("{}: <SpeculativeError>", file.as_str())
                } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                    format!("{}: {}", file.as_str(), s)
                } else if let Some(s) = panic_payload.downcast_ref::<&str>() {
                    format!("{}: {}", file.as_str(), s)
                } else {
                    format!("{}: <unknown panic>", file.as_str())
                };

                eprintln!("({}) check_job THROWS: {}", std::process::id(), exn_str);

                let file_loc = ALoc::of_loc(Loc {
                    source: Some(file.dupe()),
                    ..LOC_NONE
                });
                if let Some(s) = panic_payload.downcast_ref::<String>() {
                    if s.starts_with("ECheckTimeout:") {
                        let duration_str = s.trim_start_matches("ECheckTimeout:").trim();
                        return Err((
                            file_loc,
                            InternalError::CheckTimeout(duration_str.to_owned()),
                        ));
                    }
                    if s.starts_with("EDebugThrow") {
                        return Err((file_loc, InternalError::DebugThrow));
                    }
                }
                Err((file_loc, InternalError::CheckJobException(exn_str.into())))
            }
        }
    });
    (check_fn, cache)
}
