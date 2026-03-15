/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Merge service - drives the type merging process

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
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
use flow_common::options::Options;
use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::entity::ResolvedModule;
use flow_heap::parsing_heaps::SharedMem;
use flow_heap::parsing_heaps::merge_context_mutator;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_services_coverage::FileCoverage;
use flow_services_coverage::file_coverage;
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

pub fn sig_hash(
    _check_dirty_set: bool,
    _root: &Path,
    _shared_mem: &SharedMem,
    _component: &Vec1<FileKey>,
) -> u64 {
    0u64
}

// Entry point for merging a component
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

    let hash = 0u64;

    let mut suppressions = ErrorSuppressions::empty();
    for (file, typed_parse) in &typed_component {
        let docblock = typed_parse.docblock.as_ref().unwrap();
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
        let ast = typed_parse.ast.as_ref().unwrap();
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
    // (cx, type_sig, file_sig, typed_ast)
    (
        Context,
        Arc<flow_type_sig::packed_type_sig::Module<Loc>>,
        Arc<FileSig>,
        ast::Program<ALoc, (ALoc, Type)>,
    ),
    // (errors, warnings, suppressions, coverage, duration)
    (ErrorSet, ErrorSet, ErrorSuppressions, FileCoverage, f64),
);
// WARNING: The Context in CheckFileResult has already had post_inference_cleanup()
// called on it. It must NOT be used for type inference or find_require() calls.
// The caller (job_helper) immediately discards it: Ok(Some((_, acc))).

fn mk_check_file(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: Arc<MasterContext>,
) -> (
    Box<dyn FnMut(FileKey) -> Option<CheckFileResult>>,
    Rc<std::cell::RefCell<CheckCache>>,
) {
    let cache = Rc::new(std::cell::RefCell::new(CheckCache::create(10_000_000)));
    let cache_ref = cache.dupe();
    let check_service::CheckFileAndCompEnv {
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
        let (cx, typed_ast) = check_file(
            file.dupe(),
            resolved_modules,
            ast,
            file_sig.dupe(),
            docblock,
            aloc_table,
        );
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
        // Break Rc reference cycles in Context to prevent memory leaks.
        // cx is returned but immediately dropped by job_helper (line 41: Ok(Some((_, acc)))).
        cx.post_inference_cleanup();
        // Clear the thread-local DST_CX_REF to release the retained Context.
        flow_typing_utils::annotation_inference::clear_dst_cx();
        #[cfg(debug_assertions)]
        {
            // After cleanup, the only strong references to this Context should be:
            // 1. The local `cx` binding in this closure
            // 2. The `cx` in the return tuple (about to be dropped by job_helper)
            // If strong_count > 2, some cycle or retention path was missed.
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
        if duration > 0.5 {
            eprintln!("[SLOW-CHECK] {:>8.3}s  {}", duration, file.as_str());
        }
        Some((
            (cx, type_sig, file_sig, typed_ast),
            (errors, warnings, suppressions, coverage, duration),
        ))
    });
    (check_file_fn, cache_ref)
}

// This cache is used in check_contents_context below. When we check the
// contents of a file, we create types from the signatures of dependencies.
//
// Note that this cache needs to be invaliated when files change. We can use the
// set of changed files (determined by the merge stream's signature hashing) to
// invalidate file-by-file when a recheck transaction commits.
//
// This cache also needs to be cleared when we compact the shared heap. The
// values in this cache can contain lazy thunks which close over shared heap
// addresses. In the event of a compaction, these addresses can become invalid.
//
// Any state derived from the values in this cache also needs to be reset in the
// event of a compaction, which can be done in the SharedMem.on_compact
// callback.
thread_local! {
    static CHECK_CONTENTS_CACHE: Rc<std::cell::RefCell<CheckCache>> =
        Rc::new(std::cell::RefCell::new(CheckCache::create(10_000)));
}

pub fn check_contents_cache() -> Rc<std::cell::RefCell<CheckCache>> {
    CHECK_CONTENTS_CACHE.with(|cache| cache.dupe())
}

// Variation of merge_context where requires may not have already been
// resolved. This is used by commands that make up a context on the fly.
//
// IMPORTANT: The returned Context contains Rc reference cycles. The caller
// must call `cx.post_inference_cleanup()` when done using the Context to
// prevent memory leaks.
pub fn check_contents_context(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: Arc<MasterContext>,
    file: FileKey,
    ast: Arc<ast::Program<Loc, Loc>>,
    docblock: Arc<Docblock>,
    file_sig: Arc<FileSig>,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> (Context, ast::Program<ALoc, (ALoc, Type)>) {
    // Loading an aloc_table is unusual for check contents! During check, we use
    // this aloc table for two purposes: (1) to compare concrete and keyed alocs
    // which might be equivalent and (2) to create ALoc.id values which always
    // have the same representation for equivalent locations.
    //
    // If this file is in a cycle, an aloc table will exist and we will
    // successfully fetch it for use in cases (1) and (2). However, in the common
    // case of no cycles, an aloc table may not exist yet, which will cause an
    // exception in the (2) case. The (1) case, where a concrete and keyed
    // location are equivalent, will not occur.
    //
    // Catching the exception provides reasonable behavior, but is not the true
    // fix. Instead, if check-contents needs to deal with cycles, the cyclic
    // dependency on `file` should come from the freshly parsed type sig data, not
    // whatever data is in the heap, and the aloc table should also come from the
    // fresh parse.
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
        mut check_file,
        compute_env: _,
    } = CHECK_CONTENTS_CACHE.with(|cache| {
        check_service::mk_check_file(shared_mem.dupe(), options.dupe(), master_cx, cache.dupe())
    });
    let (cx, typed_ast) = check_file(file, resolved_modules, ast, file_sig, docblock, aloc_table);
    (cx, typed_ast)
}

// IMPORTANT: The returned Context contains Rc reference cycles. The caller
// must call `cx.post_inference_cleanup()` when done using the Context to
// prevent memory leaks.
pub fn compute_env_of_contents(
    shared_mem: Arc<SharedMem>,
    options: Arc<Options>,
    master_cx: Arc<MasterContext>,
    file: FileKey,
    ast: Arc<ast::Program<Loc, Loc>>,
    docblock: Arc<Docblock>,
    file_sig: Arc<FileSig>,
    node_modules_containers: &BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> (Context, ast::Program<ALoc, ALoc>) {
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
        check_file: _,
        mut compute_env,
    } = CHECK_CONTENTS_CACHE.with(|cache| {
        check_service::mk_check_file(shared_mem.dupe(), options.dupe(), master_cx, cache.dupe())
    });
    compute_env(file, resolved_modules, ast, docblock, aloc_table)
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

    eprintln!("Merge skipped {} of {} modules", skipped_count, total_files);

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
    master_cx: Arc<MasterContext>,
) -> (
    Box<dyn FnMut(FileKey) -> UnitResult<Option<CheckFileResult>>>,
    Rc<std::cell::RefCell<CheckCache>>,
) {
    let (mut check_file, cache) = mk_check_file(shared_mem.dupe(), options.dupe(), master_cx);
    let check_fn = Box::new(move |file: FileKey| {
        let result: Result<Option<CheckFileResult>, _> =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| check_file(file.dupe())));
        match result {
            Ok(ok) => Ok(ok),
            Err(panic_payload) => {
                // Break Rc cycles in any leaked Context from the panicked check.
                // The DST_CX_REF thread-local holds a strong reference to the
                // Context being checked. Without cleanup, Rc cycles in the
                // Context prevent it from being freed.
                flow_typing_utils::annotation_inference::cleanup_and_clear_dst_cx();

                // SpeculativeError panics are expected: they simulate OCaml's exception
                // unwinding from add_output during speculation. These should be caught
                // and treated as non-critical errors, not re-panicked.
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

                // A catch all suppression is probably a bad idea...
                let exn_str: String = if is_speculative_error {
                    format!("{}: <SpeculativeError>", file.as_str())
                } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                    format!("{}: {}", file.as_str(), s)
                } else if let Some(s) = panic_payload.downcast_ref::<&str>() {
                    format!("{}: {}", file.as_str(), s)
                } else {
                    format!("{}: <unknown panic>", file.as_str())
                };

                // In the OCaml codebase, various exceptions (Env_invariant,
                // SpeculativeError, etc.) are used for control flow and caught by the
                // per-file handler. In Rust, these become panics. We log them and
                // continue rather than re-panicking.
                // TODO(rust_port): Convert panics to proper Result types.
                eprintln!("({}) check_job THROWS: {}", std::process::id(), exn_str);

                let file_loc = ALoc::of_loc(Loc {
                    source: Some(file.dupe()),
                    ..LOC_NONE
                });
                // We can't pattern match on the exception type once it's marshalled
                // back to the master process, so we pattern match on it here to create
                // an error result.
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
