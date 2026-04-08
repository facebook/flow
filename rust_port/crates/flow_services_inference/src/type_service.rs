/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
use flow_common_modulename::Modulename;
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
use flow_server_env::persistent_connection;
use flow_server_env::server_env::Env;
use flow_server_env::server_env::Errors;
use flow_server_env::server_monitor_listener_state;
use flow_services_export::export_search::ExportSearch;
use flow_services_get_def::get_def_types::DefInfo;
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
use crate::recheck_stats;
use crate::transaction;

#[derive(Debug)]
pub enum RecheckError {
    TooSlow,
    Canceled,
}

fn with_memory_timer<T>(options: &Options, timer: &str, f: impl FnOnce() -> T) -> T {
    let should_print = options.profile;
    let start = Instant::now();
    let result = f();
    if should_print {
        eprintln!("[{}] {:.3}s", timer, start.elapsed().as_secs_f64());
    }
    result
}

pub(crate) fn clear_errors(files: &FlowOrdSet<FileKey>, mut errors: Errors) -> Errors {
    let Errors {
        ref mut local_errors,
        duplicate_providers: _,
        ref mut merge_errors,
        ref mut warnings,
        ref mut suppressions,
    } = errors;
    for file in files {
        local_errors.remove(file);
        merge_errors.remove(file);
        warnings.remove(file);
        suppressions.remove(file);
    }
    errors
}

pub(crate) fn filter_errors(files: &FlowOrdSet<FileKey>, errors: &Errors) -> Errors {
    let local_errors: BTreeMap<FileKey, ErrorSet> = errors
        .local_errors
        .iter()
        .filter(|(file, _)| files.contains(file))
        .map(|(k, v)| (k.dupe(), v.dupe()))
        .collect();
    let merge_errors: BTreeMap<FileKey, ErrorSet> = errors
        .merge_errors
        .iter()
        .filter(|(file, _)| files.contains(file))
        .map(|(k, v)| (k.dupe(), v.dupe()))
        .collect();
    let warnings: BTreeMap<FileKey, ErrorSet> = errors
        .warnings
        .iter()
        .filter(|(file, _)| files.contains(file))
        .map(|(k, v)| (k.dupe(), v.dupe()))
        .collect();
    let mut suppressions = errors.suppressions.clone();
    suppressions.filter_by_file(files);
    Errors {
        local_errors,
        duplicate_providers: errors.duplicate_providers.clone(),
        merge_errors,
        warnings,
        suppressions,
    }
}

fn update_errset(map: &mut BTreeMap<FileKey, ErrorSet>, file: FileKey, errset: ErrorSet) {
    if !errset.is_empty() {
        let errset = match map.get(&file) {
            Some(prev_errset) => prev_errset.union(&errset),
            None => errset,
        };
        map.insert(file, errset);
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
    assert!(changed.is_empty());
    let local_errors =
        failed
            .iter()
            .zip(errors.iter())
            .fold(BTreeMap::new(), |mut acc, (file, error)| {
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
                update_errset(&mut acc, file.dupe(), errset);
                acc
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
    dirty_modules: BTreeSet<Modulename>,
    local_errors: BTreeMap<FileKey, ErrorSet>,
    package_json: (
        Vec<FileKey>,
        Vec<Option<(Loc, flow_parser::parse_error::ParseError)>>,
    ),
}

fn parse(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    parse_next: parsing_service::Next,
) -> CollatedParseResults {
    with_memory_timer(options, "Parsing", || {
        let results =
            parsing_service::parse_with_defaults(pool, shared_mem, options, &[], parse_next);
        collate_parse_results(results)
    })
}

fn reparse(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    def_info: &DefInfo,
    modified: parsing_service::Next,
) -> CollatedParseResults {
    with_memory_timer(options, "Parsing", || {
        let locs_to_dirtify = flow_services_get_def::get_def_utils::all_locs_of_def_info(def_info);
        let results = parsing_service::reparse_with_defaults(
            pool,
            shared_mem,
            options,
            &locs_to_dirtify,
            modified,
        );
        collate_parse_results(results)
    })
}

fn commit_modules(
    pool: &ThreadPool,
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    mut duplicate_providers: BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    dirty_modules: flow_common_modulename::ModulenameSet,
) -> (
    flow_common_modulename::ModulenameSet,
    BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
) {
    with_memory_timer(options, "CommitModules", || {
        if !duplicate_providers.is_empty() {
            for m in dirty_modules.iter() {
                match m {
                    Modulename::Haste(m) => {
                        duplicate_providers.remove(m.module_name());
                    }
                    Modulename::Filename(_) => {}
                }
            }
        }
        let (changed_modules, new_duplicate_providers) =
            flow_services_module::commit_modules(pool, options, shared_mem, dirty_modules);
        for (key, value) in new_duplicate_providers {
            duplicate_providers.entry(key).or_insert(value);
        }
        (changed_modules, duplicate_providers)
    })
}

fn resolve_requires(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    parsed: &FlowOrdSet<FileKey>,
) {
    with_memory_timer(options, "ResolveRequires", || {
        let parsed_files: Vec<FileKey> = parsed.iter().map(|f| f.dupe()).collect();
        let next =
            flow_utils_concurrency::map_reduce::make_next(pool.num_workers(), None, parsed_files);
        let options_clone = options.dupe();
        let shared_mem_clone = shared_mem.dupe();
        let node_modules_containers = node_modules_containers.dupe();
        flow_utils_concurrency::map_reduce::iter(pool, next, move |batch| {
            for file in batch {
                let Ok(()) = flow_services_module::add_parsed_resolved_requires(
                    &options_clone,
                    &shared_mem_clone,
                    &node_modules_containers,
                    &file,
                ) else {
                    continue;
                };
            }
        });
    })
}

fn error_set_of_internal_error(
    file: FileKey,
    (loc, internal_error): (ALoc, InternalError),
) -> ErrorSet {
    let msg = ErrorMessage::EInternal(Box::new((loc, internal_error)));
    let err = flow_error::error_of_msg(file, msg);
    ErrorSet::singleton(err)
}

pub struct MergeResult {
    pub suppressions: ErrorSuppressions,
    pub skipped_count: usize,
    pub sig_new_or_changed: FlowOrdSet<FileKey>,
    pub top_cycle: Option<(FileKey, usize)>,
    pub calc_deps_time: std::time::Duration,
    pub time_to_merge: std::time::Duration,
}

pub fn calc_deps(
    options: &Options,
    components: Vec<Vec1<FileKey>>,
    to_merge: &FlowOrdSet<FileKey>,
) -> Vec<Vec1<FileKey>> {
    with_memory_timer(options, "CalcDeps", || {
        components
            .into_iter()
            .filter(|component: &Vec1<FileKey>| component.iter().any(|f| to_merge.contains(f)))
            .collect()
    })
}

pub fn include_dependencies_and_dependents(
    options: &Options,
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
    with_memory_timer(options, "PruneDeps", || {
        let mut to_check = input.dupe();
        to_check.add(None, Some(all_dependent_files.dupe()), None);

        let to_check_all = to_check.dupe().all();

        let preliminary_dependencies = pure_dep_graph_operations::calc_direct_dependencies(
            implementation_dependency_graph,
            &to_check_all,
        );

        let components = topsort(
            preliminary_dependencies.iter().map(|f| f.dupe()),
            sig_dependency_graph,
        );

        let mut dependencies = FlowOrdSet::new();
        for component in &components {
            let all_in_unchanged_checked = component
                .iter()
                .all(|filename| unchanged_checked.mem(filename));

            if !all_in_unchanged_checked {
                for file in component {
                    dependencies.insert(file.dupe());
                }
            }
        }

        let mut definitely_to_merge = input.dupe();
        definitely_to_merge.add(None, None, Some(dependencies.dupe()));

        let mut to_merge = definitely_to_merge.dupe();
        to_merge.add(None, Some(all_dependent_files), None);

        let recheck_set = definitely_to_merge.all();

        (to_merge, to_check, components, recheck_set)
    })
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
    errors: &mut BTreeMap<FileKey, ErrorSet>,
    file: FileKey,
    err: (ALoc, InternalError),
) {
    let new_errors = error_set_of_internal_error(file.dupe(), err);
    update_errset(errors, file, new_errors);
}

fn update_merge_results(
    mut acc: ErrorSuppressions,
    result: Option<(ErrorSuppressions, f64)>,
) -> ErrorSuppressions {
    match result {
        None => acc,
        Some((suppressions, _duration)) => {
            acc.update_suppressions(suppressions);
            acc
        }
    }
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

type CheckAcc = (
    (
        BTreeMap<FileKey, ErrorSet>,
        BTreeMap<FileKey, ErrorSet>,
        ErrorSuppressions,
        BTreeMap<FileKey, flow_services_coverage::FileCoverage>,
        Result<Vec<()>, String>,
        Option<String>,
    ),
    (i32, f64, Option<FileKey>),
);

fn update_check_results(
    mut acc: CheckAcc,
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
) -> CheckAcc {
    let (
        (
            ref mut errors,
            ref mut warnings,
            ref mut suppressions,
            ref mut coverage,
            ref mut find_ref_results,
            ref mut first_internal_error,
        ),
        ref mut slow_files,
    ) = acc;
    match result {
        Ok(None) => {}
        Ok(Some((new_errors, new_warnings, new_suppressions, new_coverage, check_time))) => {
            errors.remove(&file);
            warnings.remove(&file);
            suppressions.remove(&file);
            coverage.remove(&file);
            update_errset(errors, file.dupe(), new_errors);
            update_errset(warnings, file.dupe(), new_warnings);
            suppressions.update_suppressions(new_suppressions);
            coverage.insert(file.dupe(), new_coverage);
            *slow_files = update_slow_files(std::mem::take(slow_files), file, check_time);
        }
        Err(e) => {
            errors.remove(&file);
            warnings.remove(&file);
            suppressions.remove(&file);
            coverage.remove(&file);
            *first_internal_error = update_first_internal_error(first_internal_error.take(), &e);
            add_internal_error(errors, file, e);
            *find_ref_results = Err(String::new());
        }
    }
    acc
}

fn run_merge_service(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    for_find_all_refs: bool,
    sig_dependency_graph: &Graph<FileKey>,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
    suppressions: ErrorSuppressions,
) -> (ErrorSuppressions, usize, FlowOrdSet<FileKey>) {
    with_memory_timer(options, "Merge", || {
        let (results, sig_opts_data) = merge_service::merge(
            pool,
            shared_mem,
            options,
            for_find_all_refs,
            sig_dependency_graph,
            components,
            recheck_set,
        );

        let suppressions = results.into_iter().fold(suppressions, update_merge_results);

        (
            suppressions,
            sig_opts_data.skipped_count,
            sig_opts_data.sig_new_or_changed,
        )
    })
}

fn merge(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    for_find_all_refs: bool,
    to_merge: &CheckedSet,
    components: Vec<Vec1<FileKey>>,
    recheck_set: &FlowOrdSet<FileKey>,
    sig_dependency_graph: &Graph<FileKey>,
    suppressions: ErrorSuppressions,
) -> MergeResult {
    eprintln!("Calculating dependencies");
    let files_to_merge = to_merge.dupe().all();
    let calc_deps_start = Instant::now();
    let components = calc_deps(options, components, &files_to_merge);
    let calc_deps_time = calc_deps_start.elapsed();

    eprintln!("Merging");
    let merge_start = Instant::now();

    let top_cycle = components
        .iter()
        .filter(|c: &&Vec1<FileKey>| c.len() > 1)
        .max_by_key(|c: &&Vec1<FileKey>| c.len())
        .map(|c: &Vec1<FileKey>| (c.first().dupe(), c.len()));

    let (suppressions, skipped_count, sig_new_or_changed) = run_merge_service(
        pool,
        shared_mem,
        options,
        for_find_all_refs,
        sig_dependency_graph,
        components,
        recheck_set,
        suppressions,
    );

    eprintln!("Merging Done");
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
        let options_ref = options.dupe();
        with_memory_timer(&options_ref, "Check", || {
            eprintln!("Check prep");
            eprintln!("new or changed signatures: {}", sig_new_or_changed.len());
            let focused_to_check = to_check.focused();
            let merged_dependents = to_check.dependents();
            let mut skipped_count = 0;
            let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
            let dependents_to_check = merged_dependents
                .iter()
                .filter_map(|file| {
                    let keep = dirty_direct_dependents.contains(file)
                        || implementation_dependency_graph
                            .find_opt(file)
                            .is_some_and(|deps| {
                                deps.iter().any(|f| sig_new_or_changed.contains(f))
                            });
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
            let (next, mk_next_merge, files_completed) = job_utils::mk_next(
                intermediate_result_callback,
                max_size,
                num_workers,
                files.iter().map(|f| f.dupe()).collect(),
            );
            type CheckFn = Box<
                dyn FnMut(
                    FileKey,
                )
                    -> merge_service::UnitResult<Option<merge_service::CheckFileResult>>,
            >;
            type WorkerState = (
                CheckFn,
                Rc<std::cell::RefCell<crate::check_cache::CheckCache<'static>>>,
            );
            thread_local! {
                static WORKER_CHECK: RefCell<Option<WorkerState>> = const { RefCell::new(None) };
            }
            let mk_check: Arc<dyn Fn() -> WorkerState + Send + Sync> = {
                let shared_mem = shared_mem.dupe();
                let options = options.dupe();
                Arc::new(move || {
                    merge_service::mk_check(shared_mem.dupe(), options.dupe(), master_cx.as_ref())
                })
            };
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
                move |acc: &mut Vec<_>, batch| {
                    WORKER_CHECK.with(|cell: &RefCell<Option<WorkerState>>| {
                        let mut opt = cell.borrow_mut();
                        if opt.is_none() {
                            *opt = Some(mk_check());
                        }
                        let (check, cache) = opt.as_mut().unwrap();
                        cache.borrow_mut().clear();

                        WORKER_DEQUE.with(|deque_cell| {
                            let mut deque_opt = deque_cell.borrow_mut();
                            if deque_opt.is_none() {
                                let mut slots = deque_slots_for_job.lock();
                                *deque_opt = slots.iter_mut().find_map(|s| s.take());
                            }
                            let deque = deque_opt.as_ref().unwrap();

                            for file in batch {
                                deque.push(file);
                            }

                            let results = job_utils::mk_job_stealing(&mut **check, &options, deque);
                            mk_next_merge(acc, results);
                        });

                        cache.borrow_mut().clear();
                    });
                },
                |a: &mut Vec<_>, b: Vec<_>| {
                    a.extend(b);
                },
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
                            files_completed.fetch_add(1, std::sync::atomic::Ordering::Release);
                            return true;
                        }
                    }
                    false
                },
            );
            pool.broadcast(move |_| {
                WORKER_CHECK.with(|cell| {
                    *cell.borrow_mut() = None;
                });
                WORKER_DEQUE.with(|cell| {
                    *cell.borrow_mut() = None;
                });
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
        })
    }
}

#[derive(Debug)]
pub(crate) struct UnexpectedFileChanges(Vec<FileKey>);

fn handle_unexpected_file_changes(changed_files: Vec<FileKey>) -> RecheckError {
    let filename_set: BTreeSet<String> = changed_files
        .iter()
        .map(|f| f.as_str().to_owned())
        .collect();
    let file_count = filename_set.len();
    eprintln!(
        "Canceling recheck due to {} unexpected file changes",
        file_count
    );
    server_monitor_listener_state::push_files_to_prioritize(filename_set);
    RecheckError::Canceled
}

fn ensure_parsed(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    files: FlowOrdSet<FileKey>,
) -> Result<(), UnexpectedFileChanges> {
    with_memory_timer(options, "EnsureParsed", || {
        let parse_unexpected_skips =
            parsing_service::ensure_parsed(pool, shared_mem, options, files);
        if parse_unexpected_skips.is_empty() {
            Ok(())
        } else {
            let skips: Vec<FileKey> = parse_unexpected_skips.into_iter().collect();
            Err(UnexpectedFileChanges(skips))
        }
    })
}

pub fn ensure_parsed_or_trigger_recheck(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    files: FlowOrdSet<FileKey>,
) -> Result<(), RecheckError> {
    match ensure_parsed(pool, shared_mem, options, files) {
        Ok(()) => Ok(()),
        Err(UnexpectedFileChanges(changed_files)) => {
            Err(handle_unexpected_file_changes(changed_files))
        }
    }
}

fn init_libs(
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    ordered_libs: Vec<(Option<String>, String)>,
    local_errors: BTreeMap<FileKey, ErrorSet>,
    warnings: BTreeMap<FileKey, ErrorSet>,
    suppressions: ErrorSuppressions,
) -> (
    bool,                        // ok
    BTreeMap<FileKey, ErrorSet>, // local_errors
    BTreeMap<FileKey, ErrorSet>, // warnings
    ErrorSuppressions,           // suppressions
    (),
    Arc<MasterContext>, // master_cx
) {
    with_memory_timer(options, "InitLibs", || {
        let init::InitResult {
            ok: libs_ok,
            errors: lib_errors,
            warnings: lib_warnings,
            suppressions: lib_suppressions,
            exports: _lib_exports,
            master_cx,
        } = init::init(options, shared_mem, ordered_libs);
        let local_errors = merge_error_maps(lib_errors, local_errors);
        let warnings = merge_error_maps(lib_warnings, warnings);
        let mut suppressions = suppressions;
        suppressions.update_suppressions(lib_suppressions);
        (libs_ok, local_errors, warnings, suppressions, (), master_cx)
    })
}

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
    let dependents = roots;
    let mut checked_set = CheckedSet::empty();
    checked_set.add(Some(focused), Some(dependents), None);
    checked_set
}

fn filter_out_node_modules(options: &Options, files: &FlowOrdSet<FileKey>) -> FlowOrdSet<FileKey> {
    if options.node_modules_errors {
        return files.dupe();
    }
    let root = &options.root;
    let file_options = &options.file_options;

    let mut result = files.dupe();
    let is_in_node_modules = files::generate_is_within_node_modules_fn(root, file_options);
    for file in files {
        if is_in_node_modules(&file.to_absolute()) {
            result.remove(file);
        }
    }
    result
}

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

pub(crate) fn files_to_infer(
    options: &Options,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    parsed: FlowOrdSet<FileKey>,
    dependency_info: &DependencyInfo,
) -> CheckedSet {
    with_memory_timer(options, "FilesToInfer", || match focus_targets {
        None => unfocused_files_to_infer(options, &parsed, FlowOrdSet::new()),
        Some(input_focused) => {
            let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
            let sig_dependency_graph = dependency_info.sig_dependency_graph();
            let input_focused =
                FlowOrdSet::from(input_focused.into_inner().intersection(parsed.into_inner()));
            focused_files_to_infer(
                implementation_dependency_graph,
                sig_dependency_graph,
                input_focused,
            )
        }
    })
}

pub(crate) fn restart_if_faster_than_recheck(
    options: &Options,
    env: &Env,
    to_merge: &CheckedSet,
) -> Result<(), RecheckError> {
    let files_already_checked = env.checked_files.cardinal();
    let files_about_to_recheck = to_merge.cardinal();
    eprintln!(
        "We've already checked {} files. We're about to recheck {} files",
        files_already_checked, files_about_to_recheck
    );
    let init_time = recheck_stats::get_init_time();
    let per_file_time = recheck_stats::get_per_file_time();
    let time_to_restart = init_time + (per_file_time * files_already_checked as f64);
    let time_to_recheck = per_file_time * files_about_to_recheck as f64;
    let estimates = recheck_stats::Estimates {
        estimated_time_to_recheck: time_to_recheck,
        estimated_time_to_restart: time_to_restart,
        estimated_time_to_init: init_time,
        estimated_time_per_file: per_file_time,
        estimated_files_to_recheck: files_about_to_recheck as i64,
        estimated_files_to_init: files_already_checked as i64,
    };
    eprintln!(
        "Estimated restart time: {}s to init + ({}s * {} files) = {}s",
        init_time, per_file_time, files_already_checked, time_to_restart
    );
    eprintln!(
        "Estimated recheck time: {}s * {} files = {}s",
        per_file_time, files_about_to_recheck, time_to_recheck
    );
    eprintln!(
        "Estimating a recheck would take {:.2}s and a restart would take {:.2}s",
        time_to_recheck, time_to_restart
    );
    if time_to_restart < time_to_recheck {
        recheck_stats::record_last_estimates(options, &estimates);
        Err(RecheckError::TooSlow)
    } else {
        Ok(())
    }
}

pub struct DetermineWhatToRecheckResult {
    pub to_merge: CheckedSet,
    pub to_check: CheckedSet,
    pub components: Vec<Vec1<FileKey>>,
    pub recheck_set: FlowOrdSet<FileKey>,
    pub dependent_file_count: usize,
}

pub(crate) mod recheck {
    use super::*;

    pub struct RecheckResult {
        pub modified_count: usize,
        pub deleted_count: usize,
        pub dependent_file_count: usize,
        #[allow(dead_code)]
        pub to_merge: CheckedSet,
        pub to_check: CheckedSet,
        pub top_cycle: Option<(FileKey, usize)>,
        pub merge_skip_count: usize,
        pub check_skip_count: usize,
        pub slowest_file: Option<String>,
        pub num_slow_files: usize,
    }

    pub(crate) struct IntermediateValues {
        pub modified_count: usize,
        pub deleted_count: usize,
        pub dirty_direct_dependents: FlowOrdSet<FileKey>,
        pub errors: Errors,
        pub collated_errors: CollatedErrors,
        pub freshparsed: CheckedSet,
        pub unchanged_checked: CheckedSet,
        pub unchanged_files_to_force: CheckedSet,
        pub unchanged_files_to_upgrade: CheckedSet,
    }

    pub(crate) fn recheck_parse_and_update_dependency_info(
        pool: &ThreadPool,
        shared_mem: &Arc<SharedMem>,
        options: &Arc<Options>,
        updates: &CheckedSet,
        def_info: &DefInfo,
        files_to_force: CheckedSet,
        node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
        mut env: Env,
    ) -> Result<(Env, IntermediateValues), UnexpectedFileChanges> {
        let loc_of_aloc = |loc: &ALoc| -> Loc { shared_mem.loc_of_aloc(loc) };
        let shared_mem_for_ast = shared_mem.dupe();
        let get_ast = move |file: &FileKey| -> Option<Arc<Program<Loc, Loc>>> {
            shared_mem_for_ast.get_ast(file)
        };
        let errors = std::mem::replace(
            &mut env.errors,
            Errors {
                local_errors: BTreeMap::new(),
                duplicate_providers: BTreeMap::new(),
                merge_errors: BTreeMap::new(),
                warnings: BTreeMap::new(),
                suppressions: ErrorSuppressions::empty(),
            },
        );
        let collated_errors = std::mem::replace(&mut env.collated_errors, CollatedErrors::empty());
        let mut files_to_force = files_to_force;
        files_to_force.diff(&env.checked_files);

        eprintln!("Parsing");

        let modified_set = updates.dupe().all();
        let modified_files: Vec<FileKey> = modified_set.iter().map(|f| f.dupe()).collect();
        let modified_next: parsing_service::Next = {
            let mut iter_state = Some(modified_files);
            Box::new(move || iter_state.take())
        };
        let CollatedParseResults {
            parsed: parsed_set,
            unparsed: unparsed_set,
            unchanged: unchanged_parse,
            not_found: deleted,
            dirty_modules,
            local_errors: new_local_errors,
            package_json: _,
        } = reparse(pool, shared_mem, options, def_info, modified_next);

        let new_or_changed = parsed_set.dupe().union(unparsed_set.dupe());
        let new_or_changed_or_deleted = new_or_changed.dupe().union(deleted.dupe());

        let freshparsed = updates.filter(|file, _kind| parsed_set.contains(file));

        let Errors {
            local_errors,
            duplicate_providers,
            merge_errors,
            warnings,
            suppressions,
        } = clear_errors(&new_or_changed_or_deleted, errors);

        let mut collated_errors = collated_errors;
        {
            collated_errors.clear_all(&new_or_changed_or_deleted);
            let unsuppressable_error_codes: BTreeSet<FlowSmolStr> =
                options.unsuppressable_error_codes.iter().duped().collect();
            error_collator::update_local_collated_errors(
                &loc_of_aloc,
                &get_ast,
                &options.root,
                &options.file_options,
                options.node_modules_errors,
                &unsuppressable_error_codes,
                &suppressions,
                &new_local_errors,
                &mut collated_errors,
            );
        }

        let mut coverage = env.coverage;
        for file in &new_or_changed_or_deleted {
            coverage.remove(file);
        }

        let local_errors = merge_error_maps(new_local_errors, local_errors);

        let old_parsed = env.files;
        let old_parsed_count = old_parsed.len();
        let unchanged = FlowOrdSet::from(
            old_parsed
                .into_inner()
                .difference(new_or_changed_or_deleted.into_inner()),
        );

        let deleted_count = deleted.len();
        let modified_count = new_or_changed.len();
        if deleted_count + modified_count > 0 {
            eprintln!(
                "recheck {} modified, {} deleted files",
                modified_count, deleted_count
            );
            let log_files = |files: &FlowOrdSet<FileKey>, msg: &str, n: usize| {
                eprintln!("{} files:", msg);
                for (i, f) in files.iter().enumerate() {
                    let cap = 500;
                    if i < cap {
                        eprintln!("  {}/{}: {}", i + 1, n, f.as_str());
                    } else if i == cap {
                        eprintln!("  ...");
                        break;
                    }
                }
            };
            if modified_count > 0 {
                log_files(&new_or_changed, "modified", modified_count);
            }
            if deleted_count > 0 {
                log_files(&deleted, "deleted", deleted_count);
            }
        }

        eprintln!(
            "recheck: old = {}, del = {}, fresh = {}, unmod = {}",
            old_parsed_count,
            deleted.len(),
            freshparsed.cardinal(),
            unchanged.len(),
        );

        let unchanged_files_to_upgrade = updates.filter(|file, kind| {
            !CheckedSet::is_dependency(kind)
                && unchanged_parse.contains(file)
                && env.checked_files.mem_dependency(file)
        });

        // TODO: Is OLD_M the same as NEW_M?

        let new_or_changed_or_deleted_for_remove = parsed_set
            .dupe()
            .union(unparsed_set.dupe())
            .union(deleted.dupe());
        let mut unchanged_checked = env.checked_files.dupe();
        unchanged_checked.remove(&new_or_changed_or_deleted_for_remove);

        let unchanged_files_to_force =
            files_to_force.filter(|file, _kind| unchanged.contains(file));

        let dirty_modules_ordered: flow_common_modulename::ModulenameSet =
            dirty_modules.into_iter().collect();
        let (changed_modules, duplicate_providers) = commit_modules(
            pool,
            options,
            shared_mem,
            duplicate_providers,
            dirty_modules_ordered,
        );

        let unparsed_or_deleted: BTreeSet<FileKey> = {
            let mut set: BTreeSet<FileKey> = unparsed_set.iter().duped().collect();
            for f in &deleted {
                set.insert(f.dupe());
            }
            set
        };

        let dirty_direct_dependents: FlowOrdSet<FileKey> =
            with_memory_timer(options, "DirectDependentFiles", || {
                let dirty_direct_dependents_btree =
                    dep_service::calc_unchanged_dependents(shared_mem, None, changed_modules);
                dirty_direct_dependents_btree.into_iter().collect()
            });

        eprintln!("Re-resolving parsed and directly dependent files");
        let dirty_direct_dependents_set: FlowOrdSet<FileKey> = dirty_direct_dependents.dupe();
        ensure_parsed(pool, shared_mem, options, dirty_direct_dependents_set)?;
        let parsed_set_for_resolve = parsed_set.dupe().union(dirty_direct_dependents.dupe());
        resolve_requires(
            pool,
            shared_mem,
            options,
            node_modules_containers,
            &parsed_set_for_resolve,
        );

        eprintln!("Recalculating dependency graph");
        let parsed = parsed_set.dupe().union(unchanged.dupe());
        let dependency_info = with_memory_timer(options, "CalcDepsTypecheck", || {
            let files_to_update_dependency_info = parsed_set.union(dirty_direct_dependents.dupe());
            let partial_dependency_graph = dep_service::calc_partial_dependency_graph(
                pool,
                shared_mem,
                &files_to_update_dependency_info,
                &parsed,
            );
            DependencyInfo::update(
                env.dependency_info,
                partial_dependency_graph,
                unparsed_or_deleted,
            )
        });

        let to_remove = parsed.dupe().union(deleted);
        let unparsed =
            FlowOrdSet::from(env.unparsed.into_inner().difference(to_remove.into_inner()))
                .union(unparsed_set);

        let env = Env {
            files: parsed,
            unparsed,
            dependency_info,
            coverage,
            checked_files: env.checked_files,
            package_json_files: env.package_json_files,
            ordered_libs: env.ordered_libs,
            all_unordered_libs: env.all_unordered_libs,
            errors: env.errors,
            collated_errors: env.collated_errors,
            connections: env.connections,
            exports: env.exports,
            master_cx: env.master_cx,
        };

        let errors = Errors {
            local_errors,
            duplicate_providers,
            merge_errors,
            warnings,
            suppressions,
        };

        let intermediate_values = IntermediateValues {
            modified_count,
            deleted_count,
            dirty_direct_dependents,
            errors,
            collated_errors,
            freshparsed,
            unchanged_checked,
            unchanged_files_to_force,
            unchanged_files_to_upgrade,
        };
        Ok((env, intermediate_values))
    }

    pub(crate) fn determine_what_to_recheck(
        options: &Options,
        sig_dependency_graph: &Graph<FileKey>,
        implementation_dependency_graph: &Graph<FileKey>,
        freshparsed: &CheckedSet,
        unchanged_checked: CheckedSet,
        unchanged_files_to_force: &CheckedSet,
        dirty_direct_dependents: &FlowOrdSet<FileKey>,
    ) -> DetermineWhatToRecheckResult {
        let input_focused = freshparsed
            .focused()
            .dupe()
            .union(unchanged_files_to_force.focused().dupe());
        let input_dependencies = freshparsed
            .dependencies()
            .dupe()
            .union(unchanged_files_to_force.dependencies().dupe());
        let all_dependent_files = with_memory_timer(options, "AllDependentFiles", || {
            let roots = input_focused.dupe().union(dirty_direct_dependents.dupe());
            let all_dependent_files = pure_dep_graph_operations::calc_all_dependents(
                sig_dependency_graph,
                implementation_dependency_graph,
                &roots,
            );
            filter_out_node_modules(options, &all_dependent_files)
        });
        let input = unfocused_files_to_infer(options, &input_focused, input_dependencies);
        let (to_merge, to_check, components, recheck_set) = include_dependencies_and_dependents(
            options,
            input,
            unchanged_checked,
            all_dependent_files.dupe(),
            implementation_dependency_graph,
            sig_dependency_graph,
        );
        let dependent_file_count = all_dependent_files.len();
        DetermineWhatToRecheckResult {
            to_merge,
            to_check,
            components,
            recheck_set,
            dependent_file_count,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn recheck_merge(
        pool: &ThreadPool,
        shared_mem: &Arc<SharedMem>,
        options: &Arc<Options>,
        for_find_all_refs: bool,
        will_be_checked_files: &mut CheckedSet,
        changed_mergebase: Option<bool>,
        intermediate_values: IntermediateValues,
        env: Env,
    ) -> Result<
        (
            Env,
            RecheckResult,
            Box<dyn FnOnce()>,
            Result<Vec<()>, String>,
            Option<String>,
        ),
        RecheckError,
    > {
        let IntermediateValues {
            modified_count,
            deleted_count,
            dirty_direct_dependents,
            errors,
            collated_errors,
            freshparsed,
            unchanged_checked,
            unchanged_files_to_force,
            unchanged_files_to_upgrade,
        } = intermediate_values;
        let dependency_info = &env.dependency_info;
        let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
        let sig_dependency_graph = dependency_info.sig_dependency_graph();
        eprintln!("Determining what to recheck...");
        let mut unchanged_files_to_force = unchanged_files_to_force;
        unchanged_files_to_force.union(unchanged_files_to_upgrade.dupe());
        let DetermineWhatToRecheckResult {
            to_merge,
            to_check,
            components,
            recheck_set,
            dependent_file_count,
        } = determine_what_to_recheck(
            options,
            sig_dependency_graph,
            implementation_dependency_graph,
            &freshparsed,
            unchanged_checked.dupe(),
            &unchanged_files_to_force,
            &dirty_direct_dependents,
        );
        will_be_checked_files.union(to_merge.dupe());

        match changed_mergebase {
            Some(true) if options.lazy_mode && options.estimate_recheck_time => {
                restart_if_faster_than_recheck(options, &env, &to_merge)?;
            }
            _ => {}
        }
        ensure_parsed_or_trigger_recheck(pool, shared_mem, options, to_merge.dupe().all())?;
        if dependent_file_count > 0 {
            eprintln!("recheck {} dependent files:", dependent_file_count);
        }
        let MergeResult {
            suppressions: updated_suppressions,
            skipped_count: merge_skip_count,
            sig_new_or_changed,
            top_cycle,
            calc_deps_time: _,
            time_to_merge,
        } = merge(
            pool,
            shared_mem,
            options,
            for_find_all_refs,
            &to_merge,
            components,
            &recheck_set,
            sig_dependency_graph,
            errors.suppressions.clone(),
        );

        let exports = match &env.exports {
            None => None,
            Some(exports) => {
                eprintln!("Updating index");
                let updated_exports = with_memory_timer(options, "Indexing", || {
                    let dirty_files: BTreeSet<FileKey> =
                        sig_new_or_changed.iter().duped().collect();
                    flow_services_export::export_service::update(shared_mem, &dirty_files, exports)
                });
                eprintln!("Done updating index");
                Some(updated_exports)
            }
        };

        let sig_new_or_changed = sig_new_or_changed.union(unchanged_files_to_upgrade.dupe().all());
        let (
            errors,
            coverage,
            find_ref_results,
            time_to_check_merged,
            check_skip_count,
            slowest_file,
            num_slow_files,
            check_internal_error,
        ) = check_files::check_files(
            options.dupe(),
            pool,
            shared_mem,
            errors,
            updated_suppressions,
            env.coverage.clone(),
            to_check.dupe(),
            dirty_direct_dependents,
            sig_new_or_changed,
            &env.dependency_info,
            env.master_cx.dupe(),
        );
        if let Some(ref err) = check_internal_error {
            eprintln!("Error: {}", err);
        }

        let options_for_record = options.dupe();
        let to_merge_cardinal = to_merge.cardinal() as i64;
        let total_time = time_to_merge.as_secs_f64() + time_to_check_merged;
        let record_recheck_time: Box<dyn FnOnce()> = Box::new(move || {
            recheck_stats::record_recheck_time(&options_for_record, total_time, to_merge_cardinal);
        });
        let mut checked_files = unchanged_checked;
        checked_files.union(to_merge.dupe());
        eprintln!("Checked set: {}", checked_files.debug_counts_to_string());

        Ok((
            Env {
                checked_files,
                errors,
                collated_errors,
                coverage,
                exports,
                ..env
            },
            RecheckResult {
                modified_count,
                deleted_count,
                dependent_file_count,
                to_merge,
                to_check,
                top_cycle,
                merge_skip_count,
                check_skip_count,
                slowest_file,
                num_slow_files: num_slow_files as usize,
            },
            record_recheck_time,
            find_ref_results,
            check_internal_error,
        ))
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn full(
        pool: &ThreadPool,
        shared_mem: &Arc<SharedMem>,
        options: &Arc<Options>,
        node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
        updates: &CheckedSet,
        def_info: &DefInfo,
        files_to_force: CheckedSet,
        changed_mergebase: Option<bool>,
        will_be_checked_files: &mut CheckedSet,
        env: Env,
    ) -> Result<
        (
            Env,
            RecheckResult,
            Box<dyn FnOnce()>,
            Result<Vec<()>, String>,
            Option<String>,
        ),
        RecheckError,
    > {
        let (env, intermediate_values) = match recheck_parse_and_update_dependency_info(
            pool,
            shared_mem,
            options,
            updates,
            def_info,
            files_to_force,
            node_modules_containers,
            env,
        ) {
            Ok(result) => result,
            Err(UnexpectedFileChanges(changed_files)) => {
                return Err(handle_unexpected_file_changes(changed_files));
            }
        };
        let for_find_all_refs = match &def_info {
            DefInfo::VariableDefinition(_, _) | DefInfo::PropertyDefinition(_) => true,
            DefInfo::NoDefinition(_) => false,
        };
        recheck_merge(
            pool,
            shared_mem,
            options,
            for_find_all_refs,
            will_be_checked_files,
            changed_mergebase,
            intermediate_values,
            env,
        )
    }

    #[allow(dead_code)] // Called from handle_updates_since_saved_state which is not yet ported
    pub(crate) fn parse_and_update_dependency_info(
        pool: &ThreadPool,
        shared_mem: &Arc<SharedMem>,
        options: &Arc<Options>,
        updates: &CheckedSet,
        def_info: &DefInfo,
        files_to_force: CheckedSet,
        node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
        env: Env,
    ) -> Result<Env, UnexpectedFileChanges> {
        let (env, intermediate_values) = recheck_parse_and_update_dependency_info(
            pool,
            shared_mem,
            options,
            updates,
            def_info,
            files_to_force,
            node_modules_containers,
            env,
        )?;
        let IntermediateValues {
            errors,
            collated_errors,
            ..
        } = intermediate_values;
        Ok(Env {
            errors,
            collated_errors,
            ..env
        })
    }
}

fn clear_caches() {
    persistent_connection::clear_type_parse_artifacts_caches();
    merge_service::check_contents_cache().borrow_mut().clear();
}

fn with_transaction<T>(name: &str, f: impl FnOnce(&mut transaction::Transaction) -> T) -> T {
    transaction::with_transaction_sync(name, |transaction| {
        transaction.add(clear_caches, || {});
        f(transaction)
    })
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn recheck_impl(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    updates: &CheckedSet,
    def_info: &DefInfo,
    files_to_force: CheckedSet,
    changed_mergebase: Option<bool>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    will_be_checked_files: &mut CheckedSet,
    env: Env,
) -> Result<
    (
        Box<dyn FnOnce()>,
        RecheckStats,
        Result<Vec<()>, String>,
        Env,
    ),
    RecheckError,
> {
    let (env, stats, record_recheck_time, find_ref_results, _first_internal_error) =
        with_transaction("recheck", |_transaction| {
            recheck::full(
                pool,
                shared_mem,
                options,
                node_modules_containers,
                updates,
                def_info,
                files_to_force,
                changed_mergebase,
                will_be_checked_files,
                env,
            )
        })?;

    shared_mem.commit_entities();

    let recheck::RecheckResult {
        modified_count,
        deleted_count,
        dependent_file_count,
        to_merge: _,
        to_check,
        top_cycle,
        merge_skip_count: _merge_skip_count,
        check_skip_count: _check_skip_count,
        slowest_file: _slowest_file,
        num_slow_files: _num_slow_files,
    } = stats;

    let (collated_errors, _error_resolution_stat) =
        with_memory_timer(options, "CollateErrors", || {
            let focused_to_check = to_check.focused().dupe();
            let dependents_to_check = to_check.dependents().dupe();
            let files = FlowOrdSet::from(
                focused_to_check
                    .into_inner()
                    .union(dependents_to_check.into_inner()),
            );
            let all_suppressions = env.errors.suppressions.clone();
            let new_errors = filter_errors(&files, &env.errors);

            let mut collated_errors = env.collated_errors;
            collated_errors.clear_merge(&files);

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
                    &env.checked_files,
                    &all_suppressions,
                    &new_errors,
                    &mut collated_errors,
                );
            }

            let error_resolution_stat =
                error_collator::update_error_state_timestamps(&mut collated_errors);

            (collated_errors, error_resolution_stat)
        });

    let env = Env {
        collated_errors,
        errors: env.errors,
        files: env.files,
        unparsed: env.unparsed,
        dependency_info: env.dependency_info,
        checked_files: env.checked_files,
        package_json_files: env.package_json_files,
        ordered_libs: env.ordered_libs,
        all_unordered_libs: env.all_unordered_libs,
        coverage: env.coverage,
        connections: env.connections,
        exports: env.exports,
        master_cx: env.master_cx,
    };

    let log_recheck_event: Box<dyn FnOnce()> = Box::new(move || {
        record_recheck_time();
    });

    let changed_file_count = modified_count + deleted_count;
    let recheck_stats = RecheckStats {
        dependent_file_count,
        changed_file_count,
        top_cycle,
    };

    Ok((log_recheck_event, recheck_stats, find_ref_results, env))
}

pub struct RecheckStats {
    pub dependent_file_count: usize,
    pub changed_file_count: usize,
    pub top_cycle: Option<(FileKey, usize)>,
}

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

fn mk_env(
    files: FlowOrdSet<FileKey>,
    unparsed: FlowOrdSet<FileKey>,
    package_json_files: FlowOrdSet<FileKey>,
    dependency_info: DependencyInfo,
    ordered_libs: Vec<(Option<FlowSmolStr>, FlowSmolStr)>,
    all_unordered_libs: BTreeSet<FlowSmolStr>,
    errors: Errors,
    collated_errors: CollatedErrors,
    exports: Option<ExportSearch>,
    master_cx: Arc<MasterContext>,
) -> Env {
    Env {
        files,
        unparsed,
        dependency_info,
        checked_files: CheckedSet::empty(),
        package_json_files,
        ordered_libs,
        all_unordered_libs,
        errors,
        coverage: BTreeMap::new(),
        collated_errors,
        connections: flow_server_env::persistent_connection::empty(),
        exports,
        master_cx,
    }
}

#[allow(dead_code)]
fn verify_hash(shared_mem: &Arc<SharedMem>, file_key: &FileKey) -> bool {
    let filename_string = file_key.as_str();
    let content = match std::fs::read_to_string(filename_string) {
        Ok(c) => c,
        Err(_) => return false,
    };
    parsing_service::does_content_match_file_hash(shared_mem, file_key, &content)
}

#[allow(dead_code)]
fn assert_valid_hashes(updates: &CheckedSet, invalid_hashes: Vec<FileKey>) {
    let invalid_hashes: Vec<FileKey> = invalid_hashes
        .into_iter()
        .filter(|file| !updates.mem(file))
        .collect();
    if invalid_hashes.is_empty() {
        log::info!("Saved state verification succeeded");
    } else {
        let files_str: String = invalid_hashes
            .iter()
            .map(|f| f.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        eprintln!(
            "The following files do not match their hashes in saved state:\n{}",
            files_str
        );
        log::error!("Saved state verification failed");
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InvalidSavedState);
    }
}

#[allow(dead_code)]
fn init_with_initial_state() {
    panic!("saved state not yet ported")
}

#[allow(dead_code)]
fn init_from_legacy_saved_state() {
    panic!("saved state not yet ported")
}

#[allow(dead_code)]
fn init_from_direct_saved_state() {
    panic!("saved state not yet ported")
}

#[allow(dead_code)]
fn init_from_saved_state() {
    panic!("saved state not yet ported")
}

#[allow(dead_code)]
fn handle_updates_since_saved_state() {
    panic!("saved state not yet ported")
}

pub fn init_from_scratch(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    root: &Path,
) -> (
    Env,
    bool,                                                      /* libs_ok */
    Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>, /* node_modules_containers */
) {
    with_transaction("init", |_transaction| {
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

        eprintln!("Parsing");
        let CollatedParseResults {
            parsed: parsed_set,
            unparsed: unparsed_set,
            unchanged,
            not_found: _not_found,
            dirty_modules,
            local_errors,
            package_json: (package_json_files_list, package_json_errors),
        } = parse(pool, shared_mem, options, next);
        handle.join().unwrap();
        let parse_time = parse_start.elapsed();

        assert!(unchanged.is_empty());

        let parsed_set: FlowOrdSet<FileKey> = parsed_set
            .iter()
            .filter(|f| shared_mem.get_typed_parse(f).is_some())
            .map(|f| f.dupe())
            .collect();

        let dirty_modules_ordered: flow_common_modulename::ModulenameSet =
            dirty_modules.into_iter().collect();

        let warnings = BTreeMap::new();
        let package_errors = package_json_files_list
            .iter()
            .zip(package_json_errors.iter())
            .fold(
                BTreeMap::new(),
                |mut acc, (source_file, parse_error)| match parse_error {
                    None => acc,
                    Some((loc, err)) => {
                        let error_set = inference_utils::set_of_parse_error(
                            source_file.dupe(),
                            (loc.dupe(), err.clone()),
                        );
                        update_errset(&mut acc, source_file.dupe(), error_set);
                        acc
                    }
                },
            );
        let package_json_files = package_json_files_list
            .iter()
            .duped()
            .collect::<FlowOrdSet<_>>();
        let local_errors = merge_error_maps(package_errors, local_errors);

        eprintln!("Loading libraries");
        let (libs_ok, local_errors, warnings, suppressions, _exports, master_cx) = init_libs(
            options,
            shared_mem,
            ordered_libs.clone(),
            local_errors,
            warnings,
            ErrorSuppressions::empty(),
        );

        eprintln!("Resolving dependencies");
        let commit_start = Instant::now();
        let (changed_modules, duplicate_providers) = commit_modules(
            pool,
            options,
            shared_mem,
            BTreeMap::new(),
            dirty_modules_ordered,
        );
        let commit_time = commit_start.elapsed();

        eprintln!("Changed modules: {}", changed_modules.len());
        if !duplicate_providers.is_empty() {
            eprintln!("Duplicate providers: {}", duplicate_providers.len());
        }

        let resolve_start = Instant::now();
        resolve_requires(
            pool,
            shared_mem,
            options,
            &node_modules_containers,
            &parsed_set,
        );
        let resolve_time = resolve_start.elapsed();

        let dep_start = Instant::now();
        let dependency_info = with_memory_timer(options, "CalcDepsTypecheck", || {
            dep_service::calc_dependency_info(pool, shared_mem, &parsed_set)
        });
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
                options.unsuppressable_error_codes.iter().duped().collect();
            error_collator::update_local_collated_errors(
                &loc_of_aloc,
                &get_ast,
                &options.root,
                &options.file_options,
                options.node_modules_errors,
                &unsuppressable_error_codes,
                &suppressions,
                &local_errors,
                &mut collated_errors,
            );
            error_collator::update_error_state_timestamps(&mut collated_errors);
        }

        let errors = Errors {
            local_errors,
            duplicate_providers,
            merge_errors: BTreeMap::new(),
            warnings,
            suppressions,
        };
        let env = mk_env(
            parsed_set.dupe(),
            unparsed_set.dupe(),
            package_json_files,
            dependency_info,
            ordered_libs
                .into_iter()
                .map(|(opt, s)| {
                    (
                        opt.map(|o| FlowSmolStr::from(o.as_str())),
                        FlowSmolStr::from(s.as_str()),
                    )
                })
                .collect(),
            all_unordered_libs_set,
            errors,
            collated_errors,
            None, // exports
            master_cx,
        );

        eprintln!("Parsing:            {:6.2}s", parse_time.as_secs_f64());
        eprintln!("Commit modules:     {:6.2}s", commit_time.as_secs_f64());
        eprintln!("Resolve requires:   {:6.2}s", resolve_time.as_secs_f64());
        eprintln!("Calc dependencies:  {:6.2}s", dep_time.as_secs_f64());

        shared_mem.commit_entities();

        (env, libs_ok, node_modules_containers)
    })
}

#[allow(dead_code)]
pub fn exit_if_no_fallback(msg: Option<&str>, options: &Options) {
    if options.saved_state_no_fallback {
        if let Some(msg) = msg {
            panic!("Invalid_saved_state: {}", msg);
        } else {
            panic!("Invalid_saved_state");
        }
    }
}

#[allow(dead_code)]
pub fn load_saved_state(_options: &Arc<Options>) -> Result<(), String> {
    panic!("saved state not yet ported")
}

#[allow(clippy::too_many_arguments)]
#[allow(dead_code)]
pub fn reinit(
    _pool: &ThreadPool,
    _shared_mem: &Arc<SharedMem>,
    _options: &Arc<Options>,
    _updates: &CheckedSet,
    _files_to_force: CheckedSet,
    _will_be_checked_files: &mut CheckedSet,
    _env: Env,
) -> (
    Box<dyn FnOnce()>,
    RecheckStats,
    Result<Vec<()>, String>,
    Env,
) {
    panic!("reinit: saved state not yet ported")
}

#[allow(clippy::too_many_arguments)]
#[allow(dead_code)]
pub fn reinit_full_check(
    _pool: &ThreadPool,
    _shared_mem: &Arc<SharedMem>,
    _options: &Arc<Options>,
    updates: &CheckedSet,
    files_to_force: CheckedSet,
    will_be_checked_files: &mut CheckedSet,
    env: Env,
) -> (
    Box<dyn FnOnce()>,
    RecheckStats,
    Result<Vec<()>, String>,
    Env,
) {
    eprintln!("Reiniting with a full check.");
    let env = env;

    let mut all_checked_set = CheckedSet::empty();
    all_checked_set.add(Some(env.files.dupe()), None, None);
    all_checked_set.union(files_to_force);
    all_checked_set.union(updates.dupe());
    *will_be_checked_files = all_checked_set.dupe();

    server_monitor_listener_state::push_after_reinit(
        Some(all_checked_set.focused().dupe()),
        Some(all_checked_set.focused().dupe()),
        Some(all_checked_set),
    );

    let log_recheck_event: Box<dyn FnOnce()> = Box::new(|| {});
    let recheck_stats = RecheckStats {
        dependent_file_count: 0,
        changed_file_count: 0,
        top_cycle: None,
    };
    (log_recheck_event, recheck_stats, Ok(vec![]), env)
}

#[allow(clippy::too_many_arguments)]
pub fn recheck(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    options: &Arc<Options>,
    updates: &CheckedSet,
    def_info: &DefInfo,
    files_to_force: CheckedSet,
    require_full_check_reinit: bool,
    changed_mergebase: Option<bool>,
    missed_changes: bool,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    will_be_checked_files: &mut CheckedSet,
    env: Env,
) -> Result<
    (
        Box<dyn FnOnce()>,
        RecheckStats,
        Result<Vec<()>, String>,
        Env,
    ),
    RecheckError,
> {
    let did_change_mergebase = changed_mergebase.unwrap_or(false);
    if require_full_check_reinit {
        Ok(reinit_full_check(
            pool,
            shared_mem,
            options,
            updates,
            files_to_force,
            will_be_checked_files,
            env,
        ))
    } else if missed_changes && did_change_mergebase {
        Ok(reinit(
            pool,
            shared_mem,
            options,
            updates,
            files_to_force,
            will_be_checked_files,
            env,
        ))
    } else {
        match recheck_impl(
            pool,
            shared_mem,
            options,
            updates,
            def_info,
            files_to_force.dupe(),
            changed_mergebase,
            node_modules_containers,
            will_be_checked_files,
            env,
        ) {
            Ok(result) => Ok(result),
            Err(RecheckError::TooSlow) => {
                panic!("reinit after Recheck_too_slow: saved state not yet ported")
            }
            Err(RecheckError::Canceled) => Err(RecheckError::Canceled),
        }
    }
}

pub fn check_files_for_init(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    parsed: FlowOrdSet<FileKey>,
    message: &str,
    env: Env,
) -> Result<(Env, Option<String> /* check_internal_error */), RecheckError> {
    with_transaction(message, |_transaction| -> Result<_, RecheckError> {
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
            connections,
            exports,
        } = env;

        let infer_start = Instant::now();
        let to_infer = files_to_infer(options, focus_targets, parsed, &dependency_info);
        let infer_time = infer_start.elapsed();

        eprintln!(
            "Files to infer: {} focused, {} dependents, {} dependencies",
            to_infer.focused_cardinal(),
            to_infer.dependents_cardinal(),
            to_infer.dependencies_cardinal()
        );

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

        let ensure_start = Instant::now();
        ensure_parsed_or_trigger_recheck(pool, shared_mem, options, to_merge.dupe().all())?;
        let ensure_time = ensure_start.elapsed();

        eprintln!("Merging {} components...", components.len());
        let merge_result = merge(
            pool,
            shared_mem,
            options,
            false, // for_find_all_refs: init path doesn't use find-all-refs
            &to_merge,
            components,
            &recheck_set,
            sig_dependency_graph,
            env_errors.suppressions.clone(),
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
            connections,
            exports,
            master_cx,
        };

        shared_mem.commit_entities();

        Ok((env, check_internal_error))
    })
}

pub fn libdef_check_for_lazy_init(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    env: Env,
) -> Result<(Env, Option<String>), RecheckError> {
    let parsed: FlowOrdSet<FileKey> = env
        .all_unordered_libs
        .iter()
        .map(|n| FileKey::lib_file_of_absolute(n))
        .collect();
    check_files_for_init(
        options,
        pool,
        shared_mem,
        None,
        parsed,
        "lazy init check",
        env,
    )
}

pub fn full_check_for_init(
    options: &Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    env: Env,
) -> Result<(Env, Option<String>), RecheckError> {
    let parsed = env.files.dupe();
    check_files_for_init(
        options,
        pool,
        shared_mem,
        focus_targets,
        parsed,
        "full check",
        env,
    )
}

pub fn check_once(
    options: Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    root: &Path,
    focus_targets: Option<FlowOrdSet<FileKey>>,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let total_start = Instant::now();

    let (env, libs_ok, _node_modules_containers) =
        init_from_scratch(&options, pool, shared_mem, root);
    let env = if libs_ok {
        let (env, _first_internal_error) =
            full_check_for_init(&options, pool, shared_mem, focus_targets, env)
                .expect("Unexpected file changes during full check");
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
