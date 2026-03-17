/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Type checking service - drives the type checker
//!
//! This module is ported from OCaml's services/inference/types_js.ml

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Instant;

use crossbeam::channel;
use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_common::files;
use flow_common::files::FileOptions;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_tarjan::topsort;
use flow_common_utils::checked_set::CheckedSet;
use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parsing::parsing_service;
use flow_server_env::collated_errors::CollatedErrors;
use flow_server_env::dependency_info::DependencyInfo;
use flow_server_env::error_collator;
use flow_server_env::server_env::Env;
use flow_server_env::server_env::Errors;
use flow_typing_context::MasterContext;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error;
use flow_typing_errors::flow_error::ErrorSet;
use flow_utils_concurrency::thread_pool::ThreadPool;
use vec1::Vec1;

use crate::dep_service;
use crate::inference_utils;
use crate::init;
use crate::job_utils;
use crate::merge_service;
use crate::pure_dep_graph_operations;

pub fn make_next_files(
    root: &Path,
    file_options: Arc<FileOptions>,
    include_libdef: bool,
    all_unordered_libs: Arc<BTreeSet<String>>,
    node_modules_containers: &RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>,
    mut send_chunked: impl FnMut(Vec<FileKey>),
) {
    let file_opts_for_convert = file_options.dupe();
    let libs_for_convert = all_unordered_libs.clone();
    files::make_next_files(
        root,
        None,
        false,
        false,
        file_options,
        include_libdef,
        all_unordered_libs,
        node_modules_containers,
        |chunk: Vec<PathBuf>| {
            let files: Vec<FileKey> = chunk
                .into_iter()
                .map(|p| {
                    let path_str = p.to_string_lossy().into_owned();
                    files::filename_from_string(
                        &file_opts_for_convert,
                        include_libdef,
                        &libs_for_convert,
                        &path_str,
                    )
                })
                .collect();
            send_chunked(files);
        },
    );
}

pub struct MergeResult {
    pub suppressions: ErrorSuppressions,
    pub skipped_count: usize,
    pub sig_new_or_changed: FlowOrdSet<FileKey>,
    pub top_cycle: Option<(FileKey, usize)>,
    pub calc_deps_time: std::time::Duration,
    pub time_to_merge: std::time::Duration,
}

/// calc_deps filters components to only those that contain at least one file in to_merge
pub fn calc_deps(
    components: Vec<Vec1<FileKey>>,
    to_merge: &FlowOrdSet<FileKey>,
) -> Vec<Vec1<FileKey>> {
    components
        .into_iter()
        .filter(|component: &Vec1<FileKey>| component.iter().any(|f| to_merge.contains(f)))
        .collect()
}

/// Exception raised when files unexpectedly change during ensure_parsed
#[derive(Debug)]
struct UnexpectedFileChanges(Vec<FileKey>);

/// ensure_parsed takes a set of files, finds the files which haven't been parsed, and parses them.
/// The set of files that we expected to parse, but were skipped, either because they had
/// changed since the last recheck or no longer exist on disk. This is in contrast to files
/// that were skipped intentionally because they are not @flow, or because they are resource files.
fn ensure_parsed(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    files: FlowOrdSet<FileKey>,
) -> Result<(), UnexpectedFileChanges> {
    let parse_unexpected_skips = parsing_service::ensure_parsed(pool, shared_mem, options, files);

    if parse_unexpected_skips.is_empty() {
        Ok(())
    } else {
        let skips: Vec<FileKey> = parse_unexpected_skips.into_iter().collect();
        Err(UnexpectedFileChanges(skips))
    }
}

fn handle_unexpected_file_changes(changed_files: Vec<FileKey>) {
    let file_count = changed_files.len();
    eprintln!(
        "Canceling recheck due to {} unexpected file changes",
        file_count
    );
    // TODO: In OCaml, this pushes files to prioritize and raises Lwt.Canceled
}

pub fn ensure_parsed_or_trigger_recheck(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    files: FlowOrdSet<FileKey>,
) {
    match ensure_parsed(pool, shared_mem, options, files) {
        Ok(()) => {}
        Err(UnexpectedFileChanges(changed_files)) => {
            handle_unexpected_file_changes(changed_files);
        }
    }
}

fn run_merge_service(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    sig_dependency_graph: &Graph<FileKey>,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
    suppressions: ErrorSuppressions,
) -> (ErrorSuppressions, usize, FlowOrdSet<FileKey>) {
    let (results, sig_opts_data) = merge_service::merge(
        pool,
        shared_mem,
        options,
        false,
        sig_dependency_graph,
        components,
        recheck_set,
    );

    let suppressions = results.into_iter().fold(suppressions, |mut acc, result| {
        if let Some((supp, _duration)) = result {
            acc.update_suppressions(supp);
        }
        acc
    });

    (
        suppressions,
        sig_opts_data.skipped_count,
        sig_opts_data.sig_new_or_changed,
    )
}

fn merge(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    to_merge: &CheckedSet,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
    sig_dependency_graph: &Graph<FileKey>,
    suppressions: ErrorSuppressions,
) -> MergeResult {
    let files_to_merge = to_merge.dupe().all();

    // Filter components to those in files_to_merge (timed)
    let calc_deps_start = Instant::now();
    let components = calc_deps(components, &files_to_merge);
    let calc_deps_time = calc_deps_start.elapsed();

    let merge_start = Instant::now();

    // Compute largest cycle for logging
    let top_cycle = components
        .iter()
        .filter(|c: &&Vec1<FileKey>| c.len() > 1)
        .max_by_key(|c: &&Vec1<FileKey>| c.len())
        .map(|c: &Vec1<FileKey>| (c.first().dupe(), c.len()));

    let (suppressions, skipped_count, sig_new_or_changed) = run_merge_service(
        pool,
        shared_mem,
        options,
        sig_dependency_graph,
        components,
        recheck_set,
        suppressions,
    );

    let time_to_merge = merge_start.elapsed();

    MergeResult {
        suppressions,
        skipped_count,
        sig_new_or_changed,
        top_cycle,
        calc_deps_time,
        time_to_merge,
    }
}

fn filter_out_node_modules(options: &Options, files: &FlowOrdSet<FileKey>) -> FlowOrdSet<FileKey> {
    let root = &options.root;
    let file_options = &options.file_options;

    let mut result = files.dupe();
    let is_in_node_modules = files::generate_is_within_node_modules_fn(root, file_options);
    for file in files {
        if is_in_node_modules(file.as_str()) {
            result.remove(file);
        }
    }
    result
}

fn update_errset(
    mut map: BTreeMap<FileKey, ErrorSet>,
    file: FileKey,
    errset: ErrorSet,
) -> BTreeMap<FileKey, ErrorSet> {
    if errset.is_empty() {
        map
    } else {
        let errset = match map.get(&file) {
            Some(prev_errset) => prev_errset.union(&errset),
            None => errset,
        };
        map.insert(file, errset);
        map
    }
}

fn merge_error_maps(
    mut left: BTreeMap<FileKey, ErrorSet>,
    right: BTreeMap<FileKey, ErrorSet>,
) -> BTreeMap<FileKey, ErrorSet> {
    for (file, errset) in right {
        let errset = match left.get(&file) {
            Some(prev_errset) => prev_errset.union(&errset),
            None => errset,
        };
        left.insert(file, errset);
    }
    left
}

fn collate_parse_results(parse_results: parsing_service::ParseResults) -> CollatedParseResults {
    let parsing_service::ParseResults {
        parsed,
        unparsed,
        changed,
        failed: (failed, errors),
        unchanged,
        not_found,
        package_json,
        dirty_modules,
    } = parse_results;
    // No one who is calling collate_parse_results is skipping files with hash mismatches
    assert!(changed.is_empty());
    let local_errors =
        failed
            .iter()
            .zip(errors.iter())
            .fold(BTreeMap::new(), |acc, (file, error)| {
                let errset = match error {
                    parsing_service::ParseFailure::UncaughtException(exn) => {
                        inference_utils::set_of_parse_exception(
                            file.dupe(),
                            FlowSmolStr::from(exn.as_str()),
                        )
                    }
                    parsing_service::ParseFailure::ParseError(parse_error) => {
                        inference_utils::set_of_parse_error(file.dupe(), parse_error.clone())
                    }
                    parsing_service::ParseFailure::DocblockErrors(errs) => {
                        inference_utils::set_of_docblock_errors(file.dupe(), errs)
                    }
                };
                update_errset(acc, file.dupe(), errset)
            });
    let failed_set: FlowOrdSet<FileKey> = failed.into_iter().collect();
    let unparsed = unparsed.union(failed_set);
    CollatedParseResults {
        parsed,
        unparsed,
        unchanged,
        not_found,
        dirty_modules,
        local_errors,
        package_json,
    }
}

struct CollatedParseResults {
    parsed: FlowOrdSet<FileKey>,
    unparsed: FlowOrdSet<FileKey>,
    unchanged: FlowOrdSet<FileKey>,
    not_found: FlowOrdSet<FileKey>,
    dirty_modules: BTreeSet<flow_common_modulename::Modulename>,
    local_errors: BTreeMap<FileKey, ErrorSet>,
    package_json: (
        Vec<FileKey>,
        Vec<Option<(Loc, flow_parser::parse_error::ParseError)>>,
    ),
}

fn error_set_of_internal_error(
    file: FileKey,
    (loc, internal_error): (ALoc, InternalError),
) -> ErrorSet {
    let msg = ErrorMessage::EInternal(loc, internal_error);
    let err = flow_error::error_of_msg(file, msg);
    ErrorSet::singleton(err)
}

fn update_first_internal_error(
    first_internal_error: Option<String>,
    (loc, internal_error): &(ALoc, InternalError),
) -> Option<String> {
    match first_internal_error {
        Some(_) => first_internal_error,
        None => Some(format!(
            "{}\n{}",
            loc.debug_to_string(true),
            flow_typing_errors::error_message::string_of_internal_error(internal_error),
        )),
    }
}

fn add_internal_error(
    errors: BTreeMap<FileKey, ErrorSet>,
    file: FileKey,
    err: (ALoc, InternalError),
) -> BTreeMap<FileKey, ErrorSet> {
    let new_errors = error_set_of_internal_error(file.dupe(), err);
    update_errset(errors, file, new_errors)
}

fn update_slow_files(
    acc: (i32, f64, Option<FileKey>),
    file: FileKey,
    check_time: f64,
) -> (i32, f64, Option<FileKey>) {
    if check_time > 1.0 {
        let (num_slow_files, slowest_time, slowest_file) = acc;
        let (slowest_time, slowest_file) = if check_time > slowest_time {
            (check_time, Some(file))
        } else {
            (slowest_time, slowest_file)
        };
        (num_slow_files + 1, slowest_time, slowest_file)
    } else {
        acc
    }
}

fn update_check_results(
    acc: (
        (
            BTreeMap<FileKey, ErrorSet>,
            BTreeMap<FileKey, ErrorSet>,
            ErrorSuppressions,
            BTreeMap<FileKey, flow_services_coverage::FileCoverage>,
            Result<Vec<()>, String>,
            Option<String>,
        ),
        (i32, f64, Option<FileKey>),
    ),
    (file, result): (
        FileKey,
        Result<
            Option<(
                ErrorSet,
                ErrorSet,
                ErrorSuppressions,
                flow_services_coverage::FileCoverage,
                f64,
            )>,
            (ALoc, InternalError),
        >,
    ),
) -> (
    (
        BTreeMap<FileKey, ErrorSet>,
        BTreeMap<FileKey, ErrorSet>,
        ErrorSuppressions,
        BTreeMap<FileKey, flow_services_coverage::FileCoverage>,
        Result<Vec<()>, String>,
        Option<String>,
    ),
    (i32, f64, Option<FileKey>),
) {
    let (
        (
            mut errors,
            mut warnings,
            mut suppressions,
            mut coverage,
            find_ref_results,
            first_internal_error,
        ),
        slow_files,
    ) = acc;
    match result {
        Ok(None) => (
            (
                errors,
                warnings,
                suppressions,
                coverage,
                find_ref_results,
                first_internal_error,
            ),
            slow_files,
        ),
        Ok(Some((new_errors, new_warnings, new_suppressions, new_coverage, check_time))) => {
            errors.remove(&file);
            warnings.remove(&file);
            suppressions.remove(&file);
            coverage.remove(&file);
            let errors = update_errset(errors, file.dupe(), new_errors);
            let warnings = update_errset(warnings, file.dupe(), new_warnings);
            suppressions.update_suppressions(new_suppressions);
            coverage.insert(file.dupe(), new_coverage);
            let slow_files = update_slow_files(slow_files, file, check_time);
            (
                (
                    errors,
                    warnings,
                    suppressions,
                    coverage,
                    find_ref_results,
                    first_internal_error,
                ),
                slow_files,
            )
        }
        Err(e) => {
            errors.remove(&file);
            warnings.remove(&file);
            suppressions.remove(&file);
            coverage.remove(&file);
            let first_internal_error = update_first_internal_error(first_internal_error, &e);
            let errors = add_internal_error(errors, file, e);
            (
                (
                    errors,
                    warnings,
                    suppressions,
                    coverage,
                    find_ref_results,
                    first_internal_error,
                ),
                slow_files,
            )
        }
    }
}

mod check_files {
    use super::*;

    pub fn check_files(
        options: Arc<Options>,
        pool: &ThreadPool,
        shared_mem: &Arc<SharedMem>,
        errors: Errors,
        updated_suppressions: ErrorSuppressions,
        coverage: BTreeMap<FileKey, flow_services_coverage::FileCoverage>,
        to_check: CheckedSet,
        dirty_direct_dependents: FlowOrdSet<FileKey>,
        sig_new_or_changed: FlowOrdSet<FileKey>,
        dependency_info: &DependencyInfo,
        master_cx: Arc<MasterContext>,
    ) -> (
        Errors,
        BTreeMap<FileKey, flow_services_coverage::FileCoverage>,
        Result<Vec<()>, String>,
        f64,
        usize,
        Option<String>,
        i32,
        Option<String>,
    ) {
        eprintln!("Check prep");
        eprintln!("new or changed signatures: {}", sig_new_or_changed.len());
        let focused_to_check = to_check.focused();
        let merged_dependents = to_check.dependents();
        let mut skipped_count = 0;
        let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
        // skip dependents whenever none of their dependencies have new or changed signatures
        let dependents_to_check = merged_dependents
            .iter()
            .filter_map(|file| {
                let keep = dirty_direct_dependents.contains(file)
                    || implementation_dependency_graph
                        .find_opt(file)
                        .is_some_and(|deps| deps.iter().any(|f| sig_new_or_changed.contains(f)));
                if keep {
                    Some(file.dupe())
                } else {
                    skipped_count += 1;
                    None
                }
            })
            .collect::<Vec<_>>();
        eprintln!(
            "Check will skip {} of {} files",
            skipped_count,
            // We can just add these counts without worrying about files which are in both sets. We
            // got these both from a CheckedSet. CheckedSet's representation ensures that a single
            // file cannot have more than one kind.
            focused_to_check.len() + merged_dependents.len()
        );
        let mut files = focused_to_check.dupe();
        for file in dependents_to_check {
            files.insert(file);
        }
        let intermediate_result_callback: Arc<dyn Fn(&[_]) + Send + Sync> = Arc::new(|_| {});
        eprintln!("Checking files");

        let check_start_time = Instant::now();
        let max_size = options.max_files_checked_per_worker as usize;
        let num_workers = pool.num_workers();
        let (next, mk_next_merge) = job_utils::mk_next(
            intermediate_result_callback,
            max_size,
            num_workers,
            files.iter().map(|f| f.dupe()).collect(),
        );
        // We must cache the partially evaluated mk_check result, since it contains the partially
        // evaluated builtin types are sent to the worker in full. This is quite critical for perf.
        // Recomputing these builtins from scratch per batch will be extremely expensive.
        type CheckFn = Box<
            dyn FnMut(FileKey) -> merge_service::UnitResult<Option<merge_service::CheckFileResult>>,
        >;
        type WorkerState = (
            CheckFn,
            Rc<std::cell::RefCell<crate::check_cache::CheckCache>>,
        );
        thread_local! {
            static WORKER_CHECK: RefCell<Option<WorkerState>> = const { RefCell::new(None) };
        }
        let mk_check: Arc<dyn Fn() -> WorkerState + Send + Sync> = {
            let shared_mem = shared_mem.dupe();
            let options = options.dupe();
            Arc::new(move || {
                merge_service::mk_check(shared_mem.dupe(), options.dupe(), master_cx.dupe())
            })
        };
        // Create per-worker deques for intra-batch work stealing.
        // When a worker hits a slow file, idle workers can steal remaining
        // files from its deque instead of waiting for the next batch.
        let num_workers = pool.num_workers();
        let (deque_slots, stealers) = {
            let deques: Vec<crossbeam::deque::Worker<FileKey>> = (0..num_workers)
                .map(|_| crossbeam::deque::Worker::new_fifo())
                .collect();
            let stealers: Vec<crossbeam::deque::Stealer<FileKey>> =
                deques.iter().map(|d| d.stealer()).collect();
            let slots: Vec<Option<crossbeam::deque::Worker<FileKey>>> =
                deques.into_iter().map(Some).collect();
            (
                Arc::new(flow_utils_concurrency::lock::Mutex::new(slots)),
                Arc::new(stealers),
            )
        };
        thread_local! {
            static WORKER_DEQUE: RefCell<Option<crossbeam::deque::Worker<FileKey>>> =
                const { RefCell::new(None) };
        }

        let deque_slots_for_job = deque_slots.dupe();
        let mk_check_for_steal = mk_check.dupe();
        let stealers_for_steal = stealers.dupe();

        let ret = flow_utils_concurrency::map_reduce::call_with_stealing(
            pool,
            next,
            // job: push batch files onto the worker's stealable deque, process via mk_job_stealing
            move |acc: &mut Vec<_>, batch| {
                WORKER_CHECK.with(|cell: &RefCell<Option<WorkerState>>| {
                    let mut opt = cell.borrow_mut();
                    if opt.is_none() {
                        *opt = Some(mk_check());
                    }
                    // It is important to have a fresh check cache per batch. Otherwise the cache
                    // will be extremely large.
                    let (check, cache) = opt.as_mut().unwrap();
                    // Clear cached files, breaking Rc cycles and freeing entries.
                    // Reuses existing HashMap/LinkedHashMap allocations instead of
                    // creating a brand new cache each batch.
                    cache.borrow_mut().clear();

                    WORKER_DEQUE.with(|deque_cell| {
                        let mut deque_opt = deque_cell.borrow_mut();
                        if deque_opt.is_none() {
                            let mut slots = deque_slots_for_job.lock();
                            *deque_opt = slots.iter_mut().find_map(|s| s.take());
                        }
                        let deque = deque_opt.as_ref().unwrap();

                        // Push batch files onto the stealable deque
                        for file in batch {
                            deque.push(file);
                        }

                        // Process files from deque — other workers can steal the rest
                        let (results, unfinished) =
                            job_utils::mk_job_stealing(&mut **check, &options, deque);
                        mk_next_merge(acc, (results, unfinished));
                    });

                    // Break Rc cycles AND free cached dependency files after each batch.
                    // Without this, the last batch's cached files (each containing a
                    // Context with full type graph, property maps, export maps, etc.)
                    // persist in the WORKER_CHECK thread-local indefinitely.
                    // clear() calls cleanup_all_files() then drops all entries.
                    cache.borrow_mut().clear();
                });
            },
            // merge: same as before
            |a: &mut Vec<_>, b: Vec<_>| {
                a.extend(b);
            },
            // steal: called when this worker is idle (between batches)
            move |acc: &mut Vec<_>| -> bool {
                for stealer in stealers_for_steal.iter() {
                    if let crossbeam::deque::Steal::Success(file) = stealer.steal() {
                        WORKER_CHECK.with(|cell: &RefCell<Option<WorkerState>>| {
                            let mut opt = cell.borrow_mut();
                            if opt.is_none() {
                                *opt = Some(mk_check_for_steal());
                            }
                            let (check, _cache) = opt.as_mut().unwrap();
                            let result = check(file.dupe());
                            let mapped = match result {
                                Ok(Some((_, r))) => Ok(Some(r)),
                                Ok(None) => Ok(None),
                                Err(e) => Err(e),
                            };
                            acc.push((file, mapped));
                        });
                        return true;
                    }
                }
                false
            },
        );
        let ret = ret;
        // Drop WorkerState on all worker threads to reclaim memory.
        // Each worker accumulated Context objects in its thread-local
        // across all batches. Dropping the WorkerState allows the
        // next recheck cycle to start fresh.
        pool.broadcast(|_| {
            WORKER_CHECK.with(|cell| {
                *cell.borrow_mut() = None;
            });
            WORKER_DEQUE.with(|cell| {
                *cell.borrow_mut() = None;
            });
            flow_typing_utils::annotation_inference::clear_dst_cx();
        });
        let Errors {
            merge_errors,
            warnings,
            local_errors,
            duplicate_providers,
            suppressions: _,
        } = errors;
        let (
            (
                merge_errors,
                warnings,
                suppressions,
                coverage,
                find_ref_results,
                first_internal_error,
            ),
            slow_files,
        ) = ret.into_iter().fold(
            (
                (
                    merge_errors,
                    warnings,
                    updated_suppressions,
                    coverage,
                    Ok(Vec::new()),
                    None,
                ),
                (0, 0.0, None),
            ),
            update_check_results,
        );
        let (num_slow_files, _slowest_time, slowest_file) = slow_files;
        let time_to_check_merged = check_start_time.elapsed().as_secs_f64();
        eprintln!("Checking Done");
        let errors = Errors {
            local_errors,
            duplicate_providers,
            merge_errors,
            warnings,
            suppressions,
        };
        (
            errors,
            coverage,
            find_ref_results,
            time_to_check_merged,
            skipped_count,
            slowest_file.map(|file| file.as_str().to_owned()),
            num_slow_files,
            first_internal_error.map(|msg| format!("First check internal error:\n{}", msg)),
        )
    }
}

// Filesystem lazy mode focuses on any file which changes. Non-lazy mode focuses on every file in
// the repo. In both cases, we never want node_modules to appear in the focused sets.
//
// There are no expected invariants for the input sets. The returned set has the following invariants
// 1. Node modules will only appear in the dependency set.
// 2. Dependent files are empty.
pub(crate) fn unfocused_files_to_infer(
    options: &Options,
    input_focused: &FlowOrdSet<FileKey>,
    input_dependencies: FlowOrdSet<FileKey>,
) -> CheckedSet {
    let focused = filter_out_node_modules(options, input_focused);
    let mut checked_set = CheckedSet::empty();
    checked_set.add(Some(focused), None, Some(input_dependencies));
    checked_set
}

/// Given a set of focused files and a dependency graph, calculates the recursive dependents and
/// returns a CheckedSet containing both the focused and dependent files.
pub(crate) fn focused_files_to_infer(
    implementation_dependency_graph: &Graph<FileKey>,
    sig_dependency_graph: &Graph<FileKey>,
    focused: FlowOrdSet<FileKey>,
) -> CheckedSet {
    let roots = pure_dep_graph_operations::calc_all_dependents(
        sig_dependency_graph,
        implementation_dependency_graph,
        &focused,
    );
    // [roots] is the set of all focused files and all dependent files.
    // CheckedSet will ignore the focused files in [dependents] since they are
    // also passed via [focused] and duplicates take the highest priority.
    let dependents = roots;
    let mut checked_set = CheckedSet::empty();
    checked_set.add(Some(focused), Some(dependents), None);
    checked_set
}

/// Called on initialization in non-lazy mode, with optional focus targets.
///
/// When focus targets are not provided, the result is a checked set focusing on parsed files minus
/// node modules, plus no dependents (because effectively any dependent is already focused), plus all
/// their dependencies (minus those that are already focused). The set of dependencies might contain
/// node modules.
///
/// When focus targets are provided, we remove any unparsed (e.g. syntax error) targets, and then
/// the result is a checked set focusing on those files, plus their dependents, plus all their
/// combined dependencies. All these sets might contain node modules.
///
/// In either case, we can consider the result to be "closed" in terms of expected invariants.
pub(crate) fn files_to_infer(
    options: &Options,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    parsed: FlowOrdSet<FileKey>,
    dependency_info: &DependencyInfo,
) -> CheckedSet {
    match focus_targets {
        None => unfocused_files_to_infer(options, &parsed, FlowOrdSet::new()),
        Some(input_focused) => {
            let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
            let sig_dependency_graph = dependency_info.sig_dependency_graph();
            // Only focus files that parsed successfully. Parsed files are also
            // necessarily checked because we only parse checked files, so this also
            // serves to filter the input to checked files.
            let input_focused =
                FlowOrdSet::from(input_focused.into_inner().intersection(parsed.into_inner()));
            focused_files_to_infer(
                implementation_dependency_graph,
                sig_dependency_graph,
                input_focused,
            )
        }
    }
}

// The input passed in basically tells us what the caller wants to typecheck.
// However, due to laziness, it's possible that certain dependents or dependencies have not been
// checked yet. So we need to calculate all the transitive dependents and transitive dependencies
// and add them to input, unless they're already checked and in unchanged_checked
//
// Note that we do not want to add all_dependent_files to input directly! We only want to
// pass the dependencies, and later add dependent files as needed. This is important for recheck
// optimizations. We create the recheck map which indicates whether a given file needs to be
// rechecked. Dependent files only need to be rechecked if their dependencies change.
pub(crate) fn include_dependencies_and_dependents(
    _options: &Options,
    input: CheckedSet,
    unchanged_checked: CheckedSet,
    all_dependent_files: FlowOrdSet<FileKey>,
    implementation_dependency_graph: &Graph<FileKey>,
    sig_dependency_graph: &Graph<FileKey>,
) -> (
    CheckedSet,
    CheckedSet,
    Vec<Vec1<FileKey>>,
    FlowOrdSet<FileKey>,
) {
    // We need to run the check phase on the entire input set as well as all_dependent_files.
    // We'll calculate the set of files we need to merge based on this.
    let mut to_check = input.dupe();
    to_check.add(None, Some(all_dependent_files.dupe()), None);

    let to_check_all = to_check.dupe().all();

    // We need to make sure that signatures are available for the dependencies of the files we
    // are going to check. To accomplish this, we start by finding the direct *implementation*
    // dependencies of all the files we will check. Just the signature dependencies won't do,
    // since we need signatures available for all the files imported by the bodies of the files
    // we are going to check.
    let preliminary_dependencies = pure_dep_graph_operations::calc_direct_dependencies(
        implementation_dependency_graph,
        &to_check_all,
    );

    // So we want to prune our dependencies to only the dependencies which changed. However, two
    // dependencies A and B might be in a cycle. If A changed and B did not, we still need to
    // merge B. Likewise, a dependent A and a dependency B might be in a cycle. So we need to
    // calculate components before we can prune.

    let components =
        // Grab the subgraph containing all our dependencies and sort it into the strongly connected
        // cycles
        topsort(preliminary_dependencies.iter().map(|f| f.dupe()), sig_dependency_graph);

    let mut dependencies = FlowOrdSet::new();
    for component in &components {
        let all_in_unchanged_checked = component
            .iter()
            .all(|filename| unchanged_checked.mem(filename));

        if !all_in_unchanged_checked {
            // If some member of the component is not unchanged, then keep the component
            for file in component {
                dependencies.insert(file.dupe());
            }
        }
        // If every element is unchanged, drop the component
    }

    // Definitely recheck input and dependencies. As merging proceeds, dependents may or may not be
    // rechecked.
    let mut definitely_to_merge = input.dupe();
    definitely_to_merge.add(None, None, Some(dependencies.dupe()));

    let mut to_merge = definitely_to_merge.dupe();
    to_merge.add(None, Some(all_dependent_files), None);

    // NOTE: An important invariant here is that if we recompute Sort_js.topsort with
    // to_merge on sig_dependency_graph, we would get exactly the same components. Later, we
    // will filter sig_dependency_graph to just to_merge, and correspondingly filter
    // components as well. This will work out because every component is either entirely
    // inside to_merge or entirely outside.
    let recheck_set = definitely_to_merge.all();

    (to_merge, to_check, components, recheck_set)
}

pub fn init_from_scratch(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    root: &Path,
) -> (Env, bool /* libs_ok */) {
    let (ordered_libs, all_unordered_libs) =
        files::ordered_and_unordered_lib_paths(&options.file_options);
    let all_unordered_libs = Arc::new(all_unordered_libs);
    let all_unordered_libs_set: BTreeSet<FlowSmolStr> = all_unordered_libs
        .iter()
        .map(|name| FlowSmolStr::from(name.as_str()))
        .collect();

    let file_opts = options.file_options.dupe();
    let root_buf = root.to_path_buf();

    let parse_start = Instant::now();

    let node_modules_containers: Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>> =
        Arc::new(RwLock::new(BTreeMap::new()));

    let (sender, receiver) = channel::unbounded::<Vec<FileKey>>();
    let receiver = Arc::new(receiver);

    let node_modules_containers_for_thread = node_modules_containers.clone();
    let all_libs_for_thread = all_unordered_libs.clone();
    let handle = std::thread::spawn(move || {
        make_next_files(
            &root_buf,
            file_opts,
            true,
            all_libs_for_thread,
            &node_modules_containers_for_thread,
            |files| {
                sender.send(files).unwrap();
            },
        );
        drop(sender);
    });
    let receiver_for_next = receiver.dupe();
    let next: parsing_service::Next = Box::new(move || receiver_for_next.recv().ok());

    // Parsing stage
    let results = parsing_service::parse_with_defaults(pool, shared_mem, options, &[], next);
    handle.join().unwrap();
    let parse_time = parse_start.elapsed();

    eprintln!("Parsed {} files successfully", results.parsed.len());
    if !results.unparsed.is_empty() {
        eprintln!("Skipped {} files", results.unparsed.len());
    }
    if !results.failed.0.is_empty() {
        eprintln!("Failed to parse {} files:", results.failed.0.len());
        for (file, error) in results.failed.0.iter().zip(results.failed.1.iter()) {
            eprintln!("  {}: {:?}", file.as_str(), error);
        }
    }
    if !results.not_found.is_empty() {
        eprintln!("Not found: {} files", results.not_found.len());
    }

    let CollatedParseResults {
        parsed: parsed_set,
        unparsed: unparsed_set,
        unchanged,
        not_found: _not_found,
        dirty_modules,
        local_errors,
        package_json: (package_json_files_list, package_json_errors),
    } = collate_parse_results(results);
    assert!(unchanged.is_empty());

    // Filter parsed_set to only files that have typed parses. Some files may parse
    // successfully (get an AST) but fail type sig extraction. Without this filter,
    // dep_service::calc_dependency_info panics calling get_type_sig_unsafe on those files.
    let parsed_set: FlowOrdSet<FileKey> = parsed_set
        .iter()
        .filter(|f| shared_mem.get_typed_parse(f).is_some())
        .map(|f| f.dupe())
        .collect();

    let dirty_modules_ordered: flow_common_modulename::ModulenameSet =
        dirty_modules.into_iter().collect();

    // Parsing won't raise warnings
    let warnings = BTreeMap::new();
    let package_errors = package_json_files_list
        .iter()
        .zip(package_json_errors.iter())
        .fold(
            BTreeMap::new(),
            |acc, (source_file, parse_error)| match parse_error {
                None => acc,
                Some((loc, err)) => {
                    let error_set = inference_utils::set_of_parse_error(
                        source_file.dupe(),
                        (loc.dupe(), err.clone()),
                    );
                    update_errset(acc, source_file.dupe(), error_set)
                }
            },
        );
    let package_json_files = package_json_files_list
        .iter()
        .duped()
        .collect::<FlowOrdSet<_>>();
    let local_errors = merge_error_maps(package_errors, local_errors);

    let init::InitResult {
        ok: libs_ok,
        errors: lib_errors,
        warnings: lib_warnings,
        suppressions: lib_suppressions,
        exports: lib_exports,
        master_cx,
    } = init::init(options, shared_mem, ordered_libs.clone());
    let _ = lib_exports;
    let local_errors = merge_error_maps(lib_errors, local_errors);
    let warnings = merge_error_maps(lib_warnings, warnings);
    let mut suppressions = ErrorSuppressions::empty();
    suppressions.update_suppressions(lib_suppressions);

    // commit_modules
    let commit_start = Instant::now();
    let (changed_modules, duplicate_providers) =
        flow_services_module::commit_modules(pool, options, shared_mem, dirty_modules_ordered);
    let commit_time = commit_start.elapsed();

    eprintln!("Changed modules: {}", changed_modules.len());
    if !duplicate_providers.is_empty() {
        eprintln!("Duplicate providers: {}", duplicate_providers.len());
    }

    // Resolve requires stage
    let resolve_start = Instant::now();

    // Track results across batches
    let resolved_count = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let failed_count = Arc::new(std::sync::atomic::AtomicUsize::new(0));

    // Create batched next function for smart work distribution
    let parsed_files: Vec<FileKey> = parsed_set.iter().map(|f| f.dupe()).collect();
    let next = flow_utils_concurrency::map_reduce::make_next(
        pool.num_workers(),
        None, // Use default max_size (500)
        parsed_files,
    );

    // Use iter to process batches in parallel (like OCaml's MultiWorkerLwt.iter)
    let resolved_count_clone = resolved_count.dupe();
    let failed_count_clone = failed_count.dupe();
    let options_clone = options.dupe();
    let shared_mem_clone = shared_mem.dupe();
    flow_utils_concurrency::map_reduce::iter(pool, next, move |batch| {
        let mut batch_resolved = 0;
        let mut batch_failed = 0;

        for file in batch {
            match flow_services_module::add_parsed_resolved_requires(
                &options_clone,
                &shared_mem_clone,
                &node_modules_containers,
                &file,
            ) {
                Ok(()) => batch_resolved += 1,
                Err(e) => {
                    eprintln!("Failed to resolve requires for {}: {}", file.as_str(), e);
                    batch_failed += 1;
                }
            }
        }

        // Update stats once per batch
        resolved_count_clone.fetch_add(batch_resolved, std::sync::atomic::Ordering::Relaxed);
        failed_count_clone.fetch_add(batch_failed, std::sync::atomic::Ordering::Relaxed);
    });
    let resolve_time = resolve_start.elapsed();

    let resolved = resolved_count.load(std::sync::atomic::Ordering::Relaxed);
    let failed = failed_count.load(std::sync::atomic::Ordering::Relaxed);

    eprintln!("Resolved requires for {} files", resolved);
    if failed > 0 {
        eprintln!("Failed to resolve {} files", failed);
    }

    let dep_start = Instant::now();
    let dependency_info = dep_service::calc_dependency_info(pool, shared_mem, &parsed_set);
    let dep_time = dep_start.elapsed();

    eprintln!(
        "Calculated dependency info: {} files in sig graph, {} files in impl graph",
        dependency_info.sig_dependency_graph().node_count(),
        dependency_info
            .implementation_dependency_graph()
            .node_count()
    );

    let mut collated_errors = CollatedErrors::empty();
    {
        let loc_of_aloc = |loc: &ALoc| -> Loc { shared_mem.loc_of_aloc(loc) };
        let shared_mem_for_ast = shared_mem.dupe();
        let get_ast = move |file: &FileKey| -> Option<Arc<Program<Loc, Loc>>> {
            shared_mem_for_ast.get_ast(file)
        };
        let unsuppressable_error_codes: BTreeSet<FlowSmolStr> =
            options.unsuppressable_error_codes.iter().cloned().collect();
        error_collator::update_local_collated_errors(
            &loc_of_aloc,
            &get_ast,
            &options.root,
            &options.file_options,
            &unsuppressable_error_codes,
            &suppressions,
            &local_errors,
            &mut collated_errors,
        );
        error_collator::update_error_state_timestamps(&mut collated_errors);
    }

    // OCaml: mk_env
    let errors = Errors {
        local_errors,
        duplicate_providers,
        merge_errors: BTreeMap::new(),
        warnings,
        suppressions,
    };
    let env = Env {
        files: parsed_set.dupe(),
        dependency_info,
        checked_files: CheckedSet::empty(),
        package_json_files,
        ordered_libs: ordered_libs
            .into_iter()
            .map(|(opt, s)| {
                (
                    opt.map(|o| FlowSmolStr::from(o.as_str())),
                    FlowSmolStr::from(s.as_str()),
                )
            })
            .collect(),
        all_unordered_libs: all_unordered_libs_set,
        unparsed: unparsed_set.dupe(),
        errors,
        coverage: BTreeMap::new(),
        collated_errors,
        master_cx,
    };

    eprintln!("Parsing:            {:6.2}s", parse_time.as_secs_f64());
    eprintln!("Commit modules:     {:6.2}s", commit_time.as_secs_f64());
    eprintln!("Resolve requires:   {:6.2}s", resolve_time.as_secs_f64());
    eprintln!("Calc dependencies:  {:6.2}s", dep_time.as_secs_f64());

    (env, libs_ok)
}

pub fn check_files_for_init(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    parsed: FlowOrdSet<FileKey>,
    env: Env,
) -> (Env, Option<String> /* check_internal_error */) {
    let Env {
        dependency_info,
        errors: env_errors,
        coverage: env_coverage,
        collated_errors: env_collated_errors,
        master_cx,
        files: env_files,
        checked_files: env_checked_files,
        package_json_files,
        ordered_libs,
        all_unordered_libs,
        unparsed,
    } = env;

    // Files to infer stage
    let infer_start = Instant::now();
    let to_infer = files_to_infer(options, focus_targets, parsed, &dependency_info);
    let infer_time = infer_start.elapsed();

    eprintln!(
        "Files to infer: {} focused, {} dependents, {} dependencies",
        to_infer.focused_cardinal(),
        to_infer.dependents_cardinal(),
        to_infer.dependencies_cardinal()
    );

    // Include dependencies and dependents stage
    let deps_start = Instant::now();
    let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
    let sig_dependency_graph = dependency_info.sig_dependency_graph();
    let (to_merge, to_check, components, recheck_set) = include_dependencies_and_dependents(
        options,
        to_infer,
        CheckedSet::empty(),
        FlowOrdSet::new(),
        implementation_dependency_graph,
        sig_dependency_graph,
    );
    let deps_time = deps_start.elapsed();

    eprintln!(
        "Prune deps: to_merge={}, to_check={}, recheck_set={}",
        to_merge.cardinal(),
        to_check.cardinal(),
        recheck_set.len()
    );

    // Ensure parsed stage
    let ensure_start = Instant::now();
    eprintln!("Ensuring all files to merge are parsed...");
    ensure_parsed_or_trigger_recheck(pool, shared_mem, options, to_merge.dupe().all());
    let ensure_time = ensure_start.elapsed();

    // Merge stage
    eprintln!("Merging {} components...", components.len());
    let merge_result = merge(
        pool,
        shared_mem,
        options,
        &to_merge,
        components,
        &recheck_set,
        sig_dependency_graph,
        ErrorSuppressions::empty(),
    );

    eprintln!(
        "Merged: skipped={}, sig_changed={}",
        merge_result.skipped_count,
        merge_result.sig_new_or_changed.len()
    );
    if let Some((leader, count)) = merge_result.top_cycle {
        eprintln!(
            "Largest cycle: {} files (leader: {})",
            count,
            leader.as_str()
        );
    }

    let (_dependencies, to_check) = to_check.partition_dependencies();
    let check_start = Instant::now();
    let (
        errors,
        coverage,
        _find_ref_results,
        _time_to_check_merged,
        _skipped,
        _slowest_file,
        _num_slow_files,
        check_internal_error,
    ) = check_files::check_files(
        options.dupe(),
        pool,
        shared_mem,
        env_errors,
        merge_result.suppressions.clone(),
        env_coverage,
        to_check.dupe(),
        FlowOrdSet::new(),
        merge_result.sig_new_or_changed.dupe(),
        &dependency_info,
        master_cx.dupe(),
    );
    let check_time = check_start.elapsed();
    if let Some(msg) = &check_internal_error {
        eprintln!("{}", msg);
    }

    // Update collated errors
    let mut collated_errors = env_collated_errors;
    {
        let loc_of_aloc = |loc: &ALoc| -> Loc { shared_mem.loc_of_aloc(loc) };
        let shared_mem_for_ast = shared_mem.dupe();
        let get_ast = move |file: &FileKey| -> Option<Arc<Program<Loc, Loc>>> {
            shared_mem_for_ast.get_ast(file)
        };
        error_collator::update_collated_errors(
            &loc_of_aloc,
            &get_ast,
            options,
            &to_merge,
            &merge_result.suppressions,
            &errors,
            &mut collated_errors,
        );
        error_collator::update_error_state_timestamps(&mut collated_errors);
    }

    eprintln!("Files to infer:     {:6.2}s", infer_time.as_secs_f64());
    eprintln!("Prune deps:         {:6.2}s", deps_time.as_secs_f64());
    eprintln!("Ensure parsed:      {:6.2}s", ensure_time.as_secs_f64());
    eprintln!(
        "CalcDeps:           {:6.2}s",
        merge_result.calc_deps_time.as_secs_f64()
    );
    eprintln!(
        "Merge:              {:6.2}s",
        merge_result.time_to_merge.as_secs_f64()
    );
    eprintln!("Checking:           {:6.2}s", check_time.as_secs_f64());

    let env = Env {
        files: env_files,
        dependency_info,
        checked_files: {
            let mut checked_files = to_merge;
            checked_files.union(env_checked_files);
            checked_files
        },
        package_json_files,
        ordered_libs,
        all_unordered_libs,
        unparsed,
        errors,
        coverage,
        collated_errors,
        master_cx,
    };

    (env, check_internal_error)
}

pub fn full_check_for_init(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    env: Env,
) -> (Env, Option<String>) {
    let parsed = env.files.dupe();
    check_files_for_init(options, pool, shared_mem, focus_targets, parsed, env)
}

/// Performs a single full check from scratch: initializes the environment,
/// runs type checking (optionally focused on specific files), and returns
/// errors and warnings.
pub fn check_once(
    options: Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    root: &Path,
    focus_targets: Option<FlowOrdSet<FileKey>>,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let total_start = Instant::now();

    let (env, libs_ok) = init_from_scratch(&options, pool, shared_mem, root);
    let env = if libs_ok {
        let (env, _first_internal_error) =
            full_check_for_init(&options, pool, shared_mem, focus_targets, env);
        env
    } else {
        env
    };

    let total_time = total_start.elapsed();
    eprintln!("Total:              {:6.2}s", total_time.as_secs_f64());

    let mut errors = ConcreteLocPrintableErrorSet::empty();
    for (err, _, _) in &env.collated_errors.collated_duplicate_providers_errors {
        errors.add(err.clone());
    }
    for errs in env.collated_errors.collated_local_errors.values() {
        errors.union(errs);
    }
    for errs in env.collated_errors.collated_merge_errors.values() {
        errors.union(errs);
    }
    let mut warnings = ConcreteLocPrintableErrorSet::empty();
    for errs in env.collated_errors.collated_warning_map.values() {
        warnings.union(errs);
    }
    (errors, warnings)
}
