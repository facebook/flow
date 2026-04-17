/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocTable;
use flow_aloc::LazyALocTable;
use flow_aloc::aloc_representation_do_not_use;
use flow_common::docblock::Docblock;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common::options::Options;
use flow_common::reason;
use flow_common::reason::VirtualReasonDesc::*;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::Dependency;
use flow_heap::entity::ResolvedModule;
use flow_heap::parse::TypedParse;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_type_sig::compact_table::Index;
use flow_type_sig::type_sig_pack as Pack;
use flow_typing::merge::copy_into;
use flow_typing::merge::mk_builtins;
use flow_typing::type_inference;
use flow_typing_builtins::builtins::Builtins;
use flow_typing_context::ComponentT;
use flow_typing_context::Context;
use flow_typing_context::MasterContext;
use flow_typing_context::Metadata;
use flow_typing_context::ResolvedRequire;
use flow_typing_context::docblock_overrides;
use flow_typing_context::make_ccx;
use flow_typing_context::metadata_of_options;
use flow_typing_type::type_;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::Type;
use flow_typing_utils::annotation_inference;
use flow_typing_utils::type_sig_merge;
use flow_typing_utils::type_sig_merge::Exports;
use once_cell::unsync::Lazy;

use crate::check_cache::CheckCache;

pub struct CheckFileAndCompEnv {
    pub make_cx: Box<
        dyn FnMut(
                FileKey,
                BTreeMap<FlowImportSpecifier, ResolvedModule>,
                Arc<ast::Program<Loc, Loc>>,
                Arc<Docblock>,
                LazyALocTable,
            ) -> Context<'static>
            + 'static,
    >,
    pub check_file: Box<
        dyn FnMut(
                &Context<'static>,
                &FileKey,
                Arc<FileSig>,
                &Metadata,
                &[flow_parser::ast::Comment<Loc>],
                &ast::Program<ALoc, ALoc>,
            ) -> ast::Program<ALoc, (ALoc, Type)>
            + 'static,
    >,
    pub compute_env: Box<dyn FnMut(&Context<'static>, &ast::Program<ALoc, ALoc>) + 'static>,
}

fn typed_builtin_module_opt<'cx>(
    cx: &Context<'cx>,
    builtin_module_name: &Userland,
) -> Option<Rc<dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type> + 'cx>> {
    match cx.builtin_module_opt(builtin_module_name) {
        Some((reason, lazy_module)) => {
            let s = type_::constraint::forcing_state::ModuleTypeForcingState::of_lazy_module(
                reason,
                lazy_module,
            );
            let thunk = annotation_inference::force_module_type_thunk(cx.dupe(), s);
            Some(thunk)
        }
        None => None,
    }
}

fn unknown_module_t<'cx>(
    cx: &Context<'cx>,
    module_name: &FlowImportSpecifier,
) -> ResolvedRequire<'cx> {
    match module_name {
        FlowImportSpecifier::Userland(user_land_module_name) => {
            match typed_builtin_module_opt(cx, user_land_module_name) {
                Some(typed) => ResolvedRequire::TypedModule(typed),
                None => ResolvedRequire::MissingModule,
            }
        }
        FlowImportSpecifier::HasteImportWithSpecifiedNamespace { .. } => {
            // We should not lookup builtins modules for synthetic imports.
            ResolvedRequire::MissingModule
        }
    }
}

fn unchecked_module_t<'cx>(
    cx: &Context<'cx>,
    file_key: &FileKey,
    mref: &FlowImportSpecifier,
) -> ResolvedRequire<'cx> {
    let loc = ALoc::of_loc(Loc {
        source: Some(file_key.dupe()),
        ..LOC_NONE
    });
    match mref {
        FlowImportSpecifier::Userland(user_land_module_name) => {
            match typed_builtin_module_opt(cx, user_land_module_name) {
                Some(typed) => ResolvedRequire::TypedModule(typed),
                None => ResolvedRequire::UncheckedModule(loc),
            }
        }
        FlowImportSpecifier::HasteImportWithSpecifiedNamespace { .. } => {
            // We should not lookup builtins modules for synthetic imports.
            ResolvedRequire::UncheckedModule(loc)
        }
    }
}

fn get_lint_severities(
    metadata: &Metadata,
    options: &Options,
) -> flow_lint_settings::lint_settings::LintSettings<flow_lint_settings::severity::Severity> {
    let lint_severities = options.lint_severities.clone();
    let strict_mode = &options.strict_mode;
    flow_typing::merge::get_lint_severities(metadata, strict_mode, lint_severities)
}

/// This function is designed to be applied up to the unit argument and returns a
/// function which can be called repeatedly. The returned function closes over an
/// environment which defines caches that can be re-used when checking multiple
/// files.
pub fn mk_check_file(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: &MasterContext,
    cache: Rc<RefCell<CheckCache<'static>>>,
) -> CheckFileAndCompEnv {
    let base_metadata = metadata_of_options(&options);

    let mk_builtins_fn = mk_builtins(&base_metadata, master_cx);

    /// Create a type representing the exports of a dependency. For checked
    /// dependencies, we will create a "sig tvar" with a lazy thunk that evaluates
    /// to a ModuleT type.
    fn dep_module_t(
        cx: &Context<'static>,
        mref: &FlowImportSpecifier,
        resolved_module: Result<Dependency, Option<FlowImportSpecifier>>,
        shared_mem: &Arc<SharedMem>,
        base_metadata: &Metadata,
        mk_builtins_fn: &Rc<dyn Fn(&Context<'static>) -> Builtins<'static, Context<'static>>>,
        cache: &Rc<RefCell<CheckCache<'static>>>,
    ) -> ResolvedRequire<'static> {
        match resolved_module {
            Err(mapped_name) => {
                let m = mapped_name.as_ref().unwrap_or(mref);
                unknown_module_t(cx, m)
            }
            Ok(m) => match shared_mem.get_provider(&m) {
                None => {
                    let modulename = m.to_modulename();
                    unknown_module_t(
                        cx,
                        &FlowImportSpecifier::userland(FlowSmolStr::from(modulename.as_str())),
                    )
                }
                Some(dep_file_key) => match dep_file_key.inner() {
                    FileKeyInner::ResourceFile(_) => {
                        let (reason, lazy_module) = type_sig_merge::merge_resource_module_t(
                            cx,
                            dep_file_key.dupe(),
                            &dep_file_key.to_absolute(),
                        );
                        let thunk = annotation_inference::force_module_type_thunk(
                            cx.dupe(),
                            type_::constraint::forcing_state::ModuleTypeForcingState::of_lazy_module(
                                reason,
                                lazy_module,
                            ),
                        );
                        ResolvedRequire::TypedModule(thunk)
                    }
                    _ => match shared_mem.get_typed_parse(&dep_file_key) {
                        Some(parse) => ResolvedRequire::TypedModule(sig_module_t(
                            &dep_file_key,
                            parse,
                            shared_mem,
                            base_metadata,
                            mk_builtins_fn,
                            cache,
                        )),
                        None => unchecked_module_t(cx, &dep_file_key, mref),
                    },
                },
            },
        }
    }

    fn sig_module_t(
        file_key: &FileKey,
        parse: TypedParse,
        shared_mem: &Arc<SharedMem>,
        base_metadata: &Metadata,
        mk_builtins_fn: &Rc<dyn Fn(&Context<'static>) -> Builtins<'static, Context<'static>>>,
        cache: &Rc<RefCell<CheckCache<'static>>>,
    ) -> Rc<dyn Fn(&Context<'static>, &Context<'static>) -> Result<ModuleType, Type> + 'static>
    {
        let file_key_for_create = file_key.dupe();
        let parse_for_create = parse.dupe();
        let shared_mem_for_create = shared_mem.dupe();
        let base_metadata_for_create = base_metadata.clone();
        let mk_builtins_for_create = mk_builtins_fn.dupe();
        let cache_for_create = cache.dupe();
        let cache_for_find = cache.dupe();
        let file_key_for_closure = file_key.dupe();
        let parse_for_leader = parse.dupe();
        let leader: Lazy<FileKey, Box<dyn FnOnce() -> FileKey>> =
            Lazy::new(Box::new(move || parse_for_leader.leader_unsafe()));
        Rc::new(move |cx: &Context<'static>, _dst_cx: &Context<'static>| {
            let create_file = |ccx: Rc<ComponentT<'static>>| {
                dep_file(
                    &file_key_for_create,
                    parse_for_create.dupe(),
                    ccx,
                    &shared_mem_for_create,
                    &base_metadata_for_create,
                    &mk_builtins_for_create,
                    &cache_for_create,
                )
            };
            cx.add_reachable_dep(file_key_for_closure.dupe());
            let (file, dep_cx) = cache_for_find.borrow_mut().find_or_create(
                || Lazy::force(&leader).dupe(),
                create_file,
                file_key_for_closure.dupe(),
            );
            copy_into(&dep_cx, cx, file.exports.as_ref())
        })
    }

    /// Create a Type_sig_merge.file record for a dependency, which we use to
    /// convert signatures into types. This function reads the signature for a file
    /// from shared memory and creates thunks (either lazy tvars or lazy types)
    /// that resolve to types.
    fn dep_file(
        file_key: &FileKey,
        parse: TypedParse,
        ccx: Rc<ComponentT<'static>>,
        shared_mem: &Arc<SharedMem>,
        base_metadata: &Metadata,
        mk_builtins_fn: &Rc<dyn Fn(&Context<'static>) -> Builtins<'static, Context<'static>>>,
        cache: &Rc<RefCell<CheckCache<'static>>>,
    ) -> (type_sig_merge::File<'static>, Context<'static>) {
        let source = Some(file_key.dupe());

        let aloc_table = {
            let file_key = file_key.dupe();
            let packed_aloc_table = parse.aloc_table_unsafe(&file_key);
            Rc::new(LazyCell::new(Box::new(move || {
                Rc::new(ALocTable::unpack(file_key.dupe(), &packed_aloc_table))
            })
                as Box<dyn FnOnce() -> Rc<ALocTable>>))
        };

        let aloc = {
            let source = source.dupe();
            move |i: &Index<Loc>| -> ALoc {
                aloc_representation_do_not_use::make_keyed(source.dupe(), i.as_usize() as u32)
            }
        };
        let aloc = Rc::new(aloc);

        let type_sig = parse.type_sig_unsafe(file_key);

        let resolved_requires_data = parse.resolved_requires_unsafe();
        let requires = parse.requires();

        let resolved_modules: BTreeMap<FlowImportSpecifier, ResolvedModule> = {
            let resolved = resolved_requires_data.get_resolved_modules();
            requires
                .iter()
                .zip(resolved.iter())
                .map(|(specifier, module)| (specifier.dupe(), module.clone()))
                .collect()
        };

        let resolved_requires: Rc<
            RefCell<
                BTreeMap<
                    FlowImportSpecifier,
                    Rc<
                        flow_lazy::Lazy<
                            Context<'static>,
                            ResolvedRequire<'static>,
                            Box<dyn FnOnce(&Context<'static>) -> ResolvedRequire<'static>>,
                        >,
                    >,
                >,
            >,
        > = Rc::new(RefCell::new(BTreeMap::new()));

        let cx = {
            let docblock = parse.docblock_unsafe(file_key);
            let metadata = docblock_overrides(&docblock, file_key, base_metadata.clone());
            let resolved_requires_for_resolve = resolved_requires.dupe();
            let resolve_require: Rc<
                dyn Fn(&Context<'static>, &FlowImportSpecifier) -> ResolvedRequire<'static>,
            > = Rc::new(move |cx: &Context, mref: &FlowImportSpecifier| {
                let rr = resolved_requires_for_resolve.borrow();
                let lazy_val = rr.get(mref).unwrap_or_else(|| {
                    panic!(
                        "dep_file resolve_require: module reference not found: {:?}",
                        mref
                    )
                });
                lazy_val.get_forced(cx).dupe()
            });
            Context::make(
                ccx,
                metadata,
                file_key.dupe(),
                aloc_table,
                resolve_require,
                mk_builtins_fn.dupe(),
            )
        };

        {
            let mut rr = resolved_requires.borrow_mut();
            for (mref, m) in &resolved_modules {
                // fun mref m -> lazy (dep_module_t cx mref m)
                let key = mref.dupe();
                let mref = mref.dupe();
                let m = m.to_result();
                let shared_mem = shared_mem.dupe();
                let base_metadata = base_metadata.clone();
                let mk_builtins_fn = mk_builtins_fn.dupe();
                let cache = cache.dupe();
                rr.insert(
                    key,
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context| {
                        dep_module_t(
                            cx,
                            &mref,
                            m,
                            &shared_mem,
                            &base_metadata,
                            &mk_builtins_fn,
                            &cache,
                        )
                    }))),
                );
            }
        }

        let dependencies = {
            let resolved_requires = resolved_requires.dupe();
            type_sig.module_refs.map(|mref| {
                (
                    mref.dupe(),
                    resolved_requires
                        .borrow()
                        .get(&FlowImportSpecifier::Userland(mref.dupe()))
                        .unwrap()
                        .dupe(),
                )
            })
        };

        use std::cell::OnceCell;
        // Use Weak<FileInner> instead of File to break the self-referential Rc cycle:
        // File → Lazy closures (local_defs, remote_refs, etc.) → file_cell → File
        // With Weak, file_cell does not prevent File from being freed when all
        // strong references are dropped.
        let file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>> =
            Rc::new(OnceCell::new());

        let file_loc = ALoc::of_loc(Loc {
            source: Some(file_key.dupe()),
            ..LOC_NONE
        });
        let reason = reason::mk_reason(RExports, file_loc);
        let reason_for_forcing = reason.dupe();
        let file_cell_for_exports = file_cell.dupe();
        let aloc_for_exports = aloc.dupe();
        let module_kind = type_sig.module_kind.clone();
        let lazy_module: Rc<
            flow_lazy::Lazy<
                Context<'static>,
                ModuleType,
                Box<dyn FnOnce(&Context<'static>) -> ModuleType + 'static>,
            >,
        > = Rc::new(flow_lazy::Lazy::new(Box::new(move |src_cx| {
            let type_export =
                |te: &Pack::TypeExport<Index<Loc>>| -> type_sig_merge::LazyExport<'static> {
                    let te = te.clone();
                    let file_cell = file_cell_for_exports.dupe();
                    let reason = reason.dupe();
                    let aloc = aloc_for_exports.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(
                        move |cx: &Context<'static>| {
                            let te = te.map(&|i| (*aloc)(i));
                            let file = type_sig_merge::File::from_weak(
                                file_cell.get().expect("file_rec not initialized"),
                            );
                            type_sig_merge::merge_type_export(cx, &file, reason, &te)
                        },
                    )))
                };
            let cjs_exports_fn =
                |packed: &Pack::Packed<Index<Loc>>| -> type_sig_merge::LazyCjsExport<'static> {
                    let packed = packed.clone();
                    let file_cell = file_cell_for_exports.dupe();
                    let aloc = aloc_for_exports.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(
                        move |cx: &Context<'static>| {
                            let packed = packed.map(&|i| (*aloc)(i));
                            let file = type_sig_merge::File::from_weak(
                                file_cell.get().expect("file_rec not initialized"),
                            );
                            type_sig_merge::merge_cjs_export_t(cx, &file, &packed)
                        },
                    )))
                };
            let es_export =
                |export: &Pack::Export<Index<Loc>>| -> type_sig_merge::LazyExport<'static> {
                    let export = export.clone();
                    let file_cell = file_cell_for_exports.dupe();
                    let aloc = aloc_for_exports.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(
                        move |cx: &Context<'static>| {
                            let export = export.map(&|i| (*aloc)(i));
                            let file = type_sig_merge::File::from_weak(
                                file_cell.get().expect("file_rec not initialized"),
                            );
                            type_sig_merge::merge_export(cx, &file, &export)
                        },
                    )))
                };
            let cjs_module = |type_exports_arr: &[Pack::TypeExport<Index<Loc>>],
                              exports_opt: &Option<Pack::Packed<Index<Loc>>>,
                              info: &Pack::CJSModuleInfo<Index<Loc>>|
             -> Exports<'static> {
                let Pack::CJSModuleInfo {
                    type_export_keys,
                    type_stars,
                    strict,
                    platform_availability_set,
                } = &info.map(&|i| (*aloc_for_exports)(i));

                assert_eq!(
                    type_export_keys.len(),
                    type_exports_arr.len(),
                    "type_export_keys and type_exports length mismatch"
                );
                let type_exports: BTreeMap<FlowSmolStr, type_sig_merge::LazyExport<'static>> =
                    type_export_keys
                        .iter()
                        .zip(type_exports_arr.iter())
                        .map(|(name, te)| (name.dupe(), type_export(te)))
                        .collect();

                let exports: Option<type_sig_merge::LazyCjsExport<'static>> =
                    exports_opt.as_ref().map(cjs_exports_fn);

                Exports::CJSExports {
                    type_exports,
                    exports,
                    type_stars: type_stars.clone(),
                    strict: *strict,
                    platform_availability_set: platform_availability_set.clone(),
                }
            };
            let es_module = |type_exports_arr: &[Pack::TypeExport<Index<Loc>>],
                             exports_arr: &[Pack::Export<Index<Loc>>],
                             ts_pending_arr: &[Pack::TsPendingExport<Index<Loc>>],
                             info: &Pack::ESModuleInfo<Index<Loc>>|
             -> Exports<'static> {
                let Pack::ESModuleInfo {
                    type_export_keys,
                    type_stars,
                    export_keys,
                    stars,
                    ts_pending_keys,
                    strict,
                    platform_availability_set,
                } = &info.map(&|i| (*aloc_for_exports)(i));

                assert_eq!(
                    type_export_keys.len(),
                    type_exports_arr.len(),
                    "type_export_keys and type_exports length mismatch"
                );
                let type_exports: BTreeMap<FlowSmolStr, type_sig_merge::LazyExport<'static>> =
                    type_export_keys
                        .iter()
                        .zip(type_exports_arr.iter())
                        .map(|(name, te)| (name.dupe(), type_export(te)))
                        .collect();

                assert_eq!(
                    export_keys.len(),
                    exports_arr.len(),
                    "export_keys and exports length mismatch"
                );
                let exports: BTreeMap<FlowSmolStr, type_sig_merge::LazyExport<'static>> =
                    export_keys
                        .iter()
                        .zip(exports_arr.iter())
                        .map(|(name, export)| (name.dupe(), es_export(export)))
                        .collect();

                assert_eq!(
                    ts_pending_keys.len(),
                    ts_pending_arr.len(),
                    "ts_pending_keys and ts_pending_arr length mismatch"
                );
                let ts_pending: Vec<(
                    FlowSmolStr,
                    type_sig_merge::LazyTsPendingClassified<'static>,
                )> = ts_pending_keys
                    .iter()
                    .zip(ts_pending_arr.iter())
                    .map(|(name, pending)| {
                        let pending = pending.clone();
                        let file_cell = file_cell_for_exports.dupe();
                        let aloc = aloc_for_exports.dupe();
                        let lazy_fn: Box<
                            dyn FnOnce(&Context<'static>) -> type_sig_merge::TsPendingClassified
                                + 'static,
                        > = Box::new(move |cx: &Context<'static>| {
                            let pending = pending.map(&|i| (*aloc)(i));
                            let file = type_sig_merge::File::from_weak(
                                file_cell.get().expect("file_rec not initialized"),
                            );
                            type_sig_merge::classify_ts_pending_export(cx, &file, &pending)
                        });
                        (name.dupe(), Rc::new(flow_lazy::Lazy::new(lazy_fn)))
                    })
                    .collect();

                Exports::ESExports {
                    type_exports,
                    exports,
                    ts_pending,
                    type_stars: type_stars.clone(),
                    stars: stars.clone(),
                    strict: *strict,
                    platform_availability_set: platform_availability_set.clone(),
                }
            };
            let exports_info = match &module_kind {
                Pack::ModuleKind::CJSModule {
                    type_exports,
                    exports,
                    info,
                } => cjs_module(type_exports, exports, info),
                Pack::ModuleKind::ESModule {
                    type_exports,
                    exports,
                    ts_pending,
                    info,
                } => es_module(type_exports, exports, ts_pending, info),
            };
            let file = type_sig_merge::File::from_weak(
                file_cell_for_exports
                    .get()
                    .expect("file_rec not initialized"),
            );
            let module_type_lazy =
                type_sig_merge::merge_exports(src_cx, &file, reason, exports_info);
            module_type_lazy.get_forced(src_cx).dupe()
        })));
        let s = type_::constraint::forcing_state::ModuleTypeForcingState::of_lazy_module(
            reason_for_forcing,
            lazy_module,
        );
        let thunk = annotation_inference::force_module_type_thunk(cx.dupe(), s);
        let exports: Rc<
            dyn Fn(&Context<'static>, &Context<'static>) -> Result<type_::ModuleType, Type>
                + 'static,
        > = thunk;

        let local_def = |file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>>,
                         def: &Pack::PackedDef<Index<Loc>>| {
            let aloc = aloc.dupe();
            let file_cell = file_cell;
            let def = def.clone();
            Rc::new(flow_lazy::Lazy::new(
                Box::new(move |_cx: &Context<'static>| {
                    let def = Rc::new(def.map(
                        &mut (),
                        |_, loc: &Index<Loc>| (*aloc)(loc),
                        |_, t: &Pack::Packed<Index<Loc>>| t.map(&|i| (*aloc)(i)),
                    ));
                    let loc = def.id_loc();
                    let name = def.name().dupe();
                    let reason = type_sig_merge::def_reason(&def);
                    let type_ =
                        |const_decl: bool,
                         file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>>,
                         cx: &Context<'static>,
                         reason: reason::Reason,
                         def: Rc<Pack::PackedDef<ALoc>>|
                         -> Type {
                            let reason_for_tvar = reason.dupe();
                            let file_cell2 = file_cell.dupe();
                            let reason2 = reason.dupe();
                            let def2 = def.dupe();
                            let resolved: Rc<
                                flow_lazy::Lazy<
                                    Context<'static>,
                                    Type,
                                    Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                                >,
                            > = Rc::new(flow_lazy::Lazy::new(Box::new(
                                move |cx: &Context<'static>| {
                                    let file = type_sig_merge::File::from_weak(
                                        file_cell2.get().expect("file_rec not initialized"),
                                    );
                                    type_sig_merge::merge_def(cx, &file, reason2, &def2, const_decl)
                                },
                            )));
                            annotation_inference::mk_sig_tvar(cx, reason_for_tvar, resolved)
                        };
                    let file_cell2 = file_cell.dupe();
                    let reason2 = reason.dupe();
                    let def2 = def.dupe();
                    (
                        loc,
                        name,
                        Rc::new(flow_lazy::Lazy::new(
                            Box::new(move |cx: &Context<'static>| {
                                type_(false, file_cell, cx, reason, def)
                            })
                                as Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                        )),
                        Rc::new(flow_lazy::Lazy::new(
                            Box::new(move |cx: &Context<'static>| {
                                type_(true, file_cell2, cx, reason2, def2)
                            })
                                as Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                        )),
                    )
                })
                    as Box<
                        dyn FnOnce(
                                &Context<'static>,
                            ) -> (
                                ALoc,
                                FlowSmolStr,
                                Rc<
                                    flow_lazy::Lazy<
                                        Context<'static>,
                                        Type,
                                        Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                                    >,
                                >,
                                Rc<
                                    flow_lazy::Lazy<
                                        Context<'static>,
                                        Type,
                                        Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                                    >,
                                >,
                            ) + 'static,
                    >,
            ))
        };

        let remote_ref = |file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>>,
                          rref: &Pack::RemoteRef<Index<Loc>>| {
            let aloc = aloc.dupe();
            let file_cell = file_cell;
            let rref = rref.clone();
            Rc::new(flow_lazy::Lazy::new(
                Box::new(move |cx: &Context<'static>| {
                    let remote_ref = rref.map(&|i| (*aloc)(i));
                    let loc = remote_ref.loc().dupe();
                    let name = remote_ref.name().dupe();
                    let reason = type_sig_merge::remote_ref_reason(&remote_ref);
                    let file_cell2 = file_cell.dupe();
                    let reason2 = reason.dupe();
                    let remote_ref2 = remote_ref.clone();
                    let resolved: Rc<
                        flow_lazy::Lazy<
                            Context<'static>,
                            Type,
                            Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
                        >,
                    > = Rc::new(flow_lazy::Lazy::new(Box::new(
                        move |cx: &Context<'static>| {
                            let file = type_sig_merge::File::from_weak(
                                file_cell2.get().expect("file_rec not initialized"),
                            );
                            type_sig_merge::merge_remote_ref(cx, &file, reason2, &remote_ref2)
                        },
                    )));
                    let t = annotation_inference::mk_sig_tvar(cx, reason.dupe(), resolved);
                    (loc, name, t)
                })
                    as Box<dyn FnOnce(&Context<'static>) -> (ALoc, FlowSmolStr, Type) + 'static>,
            ))
        };

        let pattern_def = |file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>>,
                           def: &Pack::Packed<Index<Loc>>| {
            let aloc = aloc.dupe();
            let file_cell = file_cell;
            let def = def.clone();
            Rc::new(flow_lazy::Lazy::new(
                Box::new(move |cx: &Context<'static>| {
                    let def = def.map(&|i| (*aloc)(i));
                    let file = type_sig_merge::File::from_weak(
                        file_cell.get().expect("file_rec not initialized"),
                    );
                    type_sig_merge::merge(FlowOrdMap::new(), cx, &file, &def)
                }) as Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
            ))
        };

        let pattern = |file_cell: Rc<OnceCell<Weak<type_sig_merge::FileInner<'static>>>>,
                       p: &Pack::Pattern<Index<Loc>>| {
            let aloc = aloc.dupe();
            let file_cell = file_cell;
            let p = p.clone();
            Rc::new(flow_lazy::Lazy::new(
                Box::new(move |cx: &Context<'static>| {
                    let p = p.map(&|i| (*aloc)(i));
                    let file = type_sig_merge::File::from_weak(
                        file_cell.get().expect("file_rec not initialized"),
                    );
                    type_sig_merge::merge_pattern(cx, &file, &p)
                }) as Box<dyn FnOnce(&Context<'static>) -> Type + 'static>,
            ))
        };

        let local_defs = {
            let file_cell = file_cell.dupe();
            type_sig
                .local_defs
                .map(|def| local_def(file_cell.dupe(), def))
        };

        let remote_refs = {
            let file_cell = file_cell.dupe();
            type_sig
                .remote_refs
                .map(|rref| remote_ref(file_cell.dupe(), rref))
        };

        let pattern_defs = {
            let file_cell = file_cell.dupe();
            type_sig
                .pattern_defs
                .map(|def| pattern_def(file_cell.dupe(), def))
        };

        let patterns = {
            let file_cell = file_cell.dupe();
            type_sig.patterns.map(|p| pattern(file_cell.dupe(), p))
        };

        let file = type_sig_merge::File::new(
            dependencies,
            exports,
            local_defs,
            remote_refs,
            pattern_defs,
            patterns,
        );
        file_cell
            .set(file.downgrade())
            .unwrap_or_else(|_| panic!("file_rec should only be set once"));
        (file, cx)
    }

    let options = options.dupe();

    let make_cx = {
        let shared_mem = shared_mem.dupe();
        let base_metadata = base_metadata.clone();
        let mk_builtins_fn = mk_builtins_fn.dupe();
        let cache = cache.dupe();
        Box::new(
            move |file_key: FileKey,
                  resolved_modules: BTreeMap<FlowImportSpecifier, ResolvedModule>,
                  _ast: Arc<ast::Program<Loc, Loc>>,
                  docblock: Arc<Docblock>,
                  aloc_table: LazyALocTable| {
                let ccx = Rc::new(make_ccx());
                let metadata = docblock_overrides(&docblock, &file_key, base_metadata.clone());
                let resolved_requires: Rc<
                    RefCell<HashMap<FlowImportSpecifier, ResolvedRequire<'static>>>,
                > = Rc::new(RefCell::new(HashMap::new()));
                let resolved_requires_for_resolve = resolved_requires.dupe();
                let resolve_require: Rc<
                    dyn Fn(&Context<'static>, &FlowImportSpecifier) -> ResolvedRequire<'static>,
                > = Rc::new(move |_cx: &Context<'static>, mref: &FlowImportSpecifier| {
                    resolved_requires_for_resolve
                        .borrow()
                        .get(mref)
                        .unwrap_or_else(|| {
                            panic!("resolve_require: module reference not found: {:?}", mref)
                        })
                        .dupe()
                });
                let cx: Context<'static> = Context::make(
                    ccx,
                    metadata.clone(),
                    file_key.dupe(),
                    aloc_table,
                    resolve_require,
                    mk_builtins_fn.dupe(),
                );
                {
                    let mut rr = resolved_requires.borrow_mut();
                    for (mref, m) in &resolved_modules {
                        let r = dep_module_t(
                            &cx,
                            mref,
                            m.to_result(),
                            &shared_mem,
                            &base_metadata,
                            &mk_builtins_fn,
                            &cache,
                        );
                        rr.insert(mref.dupe(), r);
                    }
                }
                cx
            },
        )
    };

    let check_file = {
        let options = options.dupe();
        Box::new(
            move |cx: &Context<'static>,
                  file_key: &FileKey,
                  file_sig: Arc<FileSig>,
                  metadata: &Metadata,
                  comments: &[flow_parser::ast::Comment<Loc>],
                  aloc_ast: &ast::Program<ALoc, ALoc>| {
                // Set merge_dst_cx to self to establish the chain for copy_into:
                // nested copy_into calls read this to find the ultimate error
                // destination (the file being checked).
                cx.set_merge_dst_cx(cx);
                let lint_severities = get_lint_severities(metadata, &options);
                type_inference::infer_file(
                    &lint_severities,
                    cx,
                    file_key,
                    file_sig,
                    metadata,
                    comments,
                    aloc_ast,
                )
            },
        )
    };

    let compute_env = Box::new(
        move |cx: &Context<'static>, aloc_ast: &ast::Program<ALoc, ALoc>| {
            cx.set_merge_dst_cx(cx);
            type_inference::initialize_env(cx, None, aloc_ast);
        },
    );

    CheckFileAndCompEnv {
        make_cx,
        check_file,
        compute_env,
    }
}
