/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Mutex;

use flow_common::files::LibDir;
use flow_common::options::Options;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_server_env::server_env::Env as ServerEnv;
use flow_server_env::server_env::Genv;
use flow_typing_context::Metadata;
use flow_utils_concurrency::thread_pool::ThreadPool;

use super::codemod_context;
use super::codemod_printer;
use super::codemod_report;

fn files_default_lib_dir(options: &Options) -> Option<LibDir> {
    options.file_options.default_lib_dir.clone()
}

fn include_dependencies_and_dependents(
    options: &Options,
    input: flow_common_utils::checked_set::CheckedSet,
    unchanged_checked: flow_common_utils::checked_set::CheckedSet,
    all_dependent_files: flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey>,
    implementation_dependency_graph: &flow_common_utils::graph::Graph<FileKey>,
    sig_dependency_graph: &flow_common_utils::graph::Graph<FileKey>,
) -> (
    flow_common_utils::checked_set::CheckedSet,
    flow_common_utils::checked_set::CheckedSet,
    Vec<vec1::Vec1<FileKey>>,
    flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey>,
) {
    flow_services_inference::type_service::include_dependencies_and_dependents(
        options,
        input,
        unchanged_checked,
        all_dependent_files,
        implementation_dependency_graph,
        sig_dependency_graph,
    )
}

fn checked_set_all(
    to_merge: flow_common_utils::checked_set::CheckedSet,
) -> flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey> {
    to_merge.all()
}

fn checked_set_of_focused_list(files: Vec<FileKey>) -> flow_common_utils::checked_set::CheckedSet {
    flow_common_utils::checked_set::CheckedSet::of_focused_list(files)
}

fn types_js_calc_deps(
    options: &Options,
    components: Vec<vec1::Vec1<FileKey>>,
    roots: &flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey>,
) -> Vec<vec1::Vec1<FileKey>> {
    flow_services_inference::type_service::calc_deps(options, components, roots)
}

#[allow(dead_code)]
fn options_root(options: &Options) -> &std::path::Path {
    &options.root
}

fn diff_heaps_remove_batch(roots: &BTreeSet<FileKey>) {
    super::diff_heaps::remove_batch(roots);
}

fn profiling_start(label: &str, _should_print_summary: bool) {
    tracing::info!("Starting profiling: {}", label);
}

#[allow(dead_code)]
fn state_reader_create() {}

fn next_of_filename_set(
    workers: &Option<ThreadPool>,
    filename_set: &BTreeSet<FileKey>,
) -> flow_parsing::parsing_service::Next {
    let files: Vec<FileKey> = filename_set.iter().cloned().collect();
    let num_workers = workers.as_ref().map_or(1, |w| w.num_workers());
    let batch_size = std::cmp::max(1, files.len() / std::cmp::max(1, num_workers));
    let state = Mutex::new((files, 0usize));
    Box::new(move || {
        let mut guard = state.lock().unwrap();
        let (ref files, ref mut index) = *guard;
        if *index >= files.len() {
            return None;
        }
        let end = std::cmp::min(*index + batch_size, files.len());
        let batch = files[*index..end].to_vec();
        *index = end;
        Some(batch)
    })
}

fn log_input_files(fileset: &BTreeSet<FileKey>) {
    let files_str: Vec<String> = fileset.iter().map(|f| f.to_absolute()).collect();
    tracing::debug!(
        "Running codemod on {} files:\n{}",
        fileset.len(),
        files_str.join("\n")
    );
}

pub fn get_target_filename_set(
    options: &std::sync::Arc<flow_common::files::FileOptions>,
    all_unordered_libs: &BTreeSet<String>,
    all: bool,
    filename_set: BTreeSet<FileKey>,
) -> BTreeSet<FileKey> {
    filename_set
        .into_iter()
        .filter(|f| {
            let s = f.to_absolute();
            let is_valid_path = flow_common::files::is_valid_path(options, &s);
            let (is_ignored, _) = flow_common::files::is_ignored(options, &s);
            is_valid_path && (all || !is_ignored) && !all_unordered_libs.contains(&s)
        })
        .collect()
}

pub fn extract_flowlibs_or_exit(options: &Options) {
    let default_lib_dir = files_default_lib_dir(options);
    match default_lib_dir {
        Some(libdir) => {
            let flowlib_libdir = match libdir {
                LibDir::Prelude(path) => flow_flowlib::LibDir::Prelude(path),
                LibDir::Flowlib(path) => flow_flowlib::LibDir::Flowlib(path),
            };
            let extract_result =
                std::panic::catch_unwind(|| flow_flowlib::extract(&flowlib_libdir));
            match extract_result {
                Ok(()) => {}
                Err(_err) => {
                    let libdir_str = flow_flowlib::path_of_libdir(&flowlib_libdir)
                        .display()
                        .to_string();
                    let msg = format!(
                        "Could not extract flowlib files into {}: extract failed",
                        libdir_str
                    );
                    eprintln!("{}", msg);
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::CouldNotExtractFlowlibs,
                    )
                }
            }
        }
        None => {}
    }
}

pub type UnitResult<A> = Result<
    A,
    (
        flow_aloc::ALoc,
        flow_typing_errors::error_message::InternalError,
    ),
>;

pub type ResultList<A> = Vec<(FileKey, UnitResult<Option<A>>)>;

pub type AbstractVisitor<A, Ctx> =
    Box<dyn Fn(&Options, &ast::Program<Loc, Loc>, Ctx) -> A + Send + Sync>;

pub trait SimpleTypedRunnerConfig {
    type Accumulator: Send + Sync + 'static;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey>;
    fn check_options(options: Options) -> Options;
    fn visit(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: codemod_context::typed::TypedCodemodContext<'_>,
    ) -> Self::Accumulator;
}

pub trait UntypedRunnerConfig {
    type Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn visit(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: codemod_context::untyped::UntypedCodemodContext,
    ) -> Self::Accumulator;
}

// Runner init function which is called after Types_js.init but before any of the jobs.
// This is useful to set up or populate shared memory for the jobs.
pub trait UntypedFlowInitRunnerConfig {
    type Accumulator;

    fn init(reader: &Arc<flow_heap::parsing_heaps::SharedMem>);
    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn visit(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: codemod_context::untyped_flow_init::UntypedFlowInitCodemodContext,
    ) -> Self::Accumulator;
}

pub trait StepRunner {
    type Env;
    type Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn init_run(
        genv: &Genv,
        roots: BTreeSet<FileKey>,
    ) -> impl std::future::Future<Output = ((), (Self::Env, ResultList<Self::Accumulator>))>;
    fn recheck_run(
        genv: &Genv,
        env: Self::Env,
        iteration: i32,
        roots: BTreeSet<FileKey>,
    ) -> impl std::future::Future<Output = ((), (Self::Env, ResultList<Self::Accumulator>))>;
    fn digest(results: ResultList<Self::Accumulator>) -> (Vec<FileKey>, Self::Accumulator);
}

pub trait Runnable {
    fn run(
        genv: &Genv,
        write: bool,
        repeat: bool,
        roots: BTreeSet<FileKey>,
    ) -> impl std::future::Future<Output = ()>;
}

async fn merge_targets<'a>(
    env: &'a ServerEnv,
    options: &Options,
    _profiling: &(),
    _get_dependent_files: impl Fn(
        &flow_common_utils::graph::Graph<FileKey>,
        &flow_common_utils::graph::Graph<FileKey>,
        &BTreeSet<FileKey>,
    ) -> std::pin::Pin<
        Box<dyn std::future::Future<Output = BTreeSet<FileKey>>>,
    >,
    roots: &BTreeSet<FileKey>,
) -> (
    &'a flow_common_utils::graph::Graph<FileKey>,
    Vec<vec1::Vec1<FileKey>>,
    flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey>,
    flow_common_utils::checked_set::CheckedSet,
) {
    let dependency_info = &env.dependency_info;
    let sig_dependency_graph = dependency_info.sig_dependency_graph();
    let implementation_dependency_graph = dependency_info.implementation_dependency_graph();
    tracing::info!("Calculating dependencies");
    let all_dependent_files: BTreeSet<FileKey> =
        _get_dependent_files(sig_dependency_graph, implementation_dependency_graph, roots).await;
    let (to_merge, to_check, _components, _recheck_set) = include_dependencies_and_dependents(
        options,
        checked_set_of_focused_list(roots.iter().cloned().collect()),
        flow_common_utils::checked_set::CheckedSet::empty(),
        flow_data_structure_wrapper::ord_set::FlowOrdSet::from_iter(all_dependent_files),
        implementation_dependency_graph,
        sig_dependency_graph,
    );
    let roots = checked_set_all(to_merge);
    let components = types_js_calc_deps(options, _components, &roots);
    (sig_dependency_graph, components, roots, to_check)
}

fn merge_job(
    _mutator: &(),
    options: &Options,
    for_find_all_refs: bool,
    reader: &flow_heap::parsing_heaps::SharedMem,
    component: vec1::Vec1<FileKey>,
) -> (bool, ()) {
    let leader = component.first();
    let rest = &component[1..];
    let diff = match reader.typed_component(leader, rest) {
        None => false,
        Some(typed_component) => {
            let _root = &options.root;
            let hash = flow_services_inference::merge_service::sig_hash(
                for_find_all_refs,
                &options.root,
                reader,
                &component,
            );
            flow_heap::parsing_heaps::merge_context_mutator::add_merge_on_diff(
                for_find_all_refs,
                &typed_component,
                hash,
            )
        }
    };
    (diff, ())
}

#[allow(unreachable_code)]
fn post_check<A>(
    _visit: &dyn Fn(
        &Options,
        &ast::Program<Loc, Loc>,
        codemod_context::typed::TypedCodemodContext<'_>,
    ) -> A,
    _iteration: i32,
    _reader: &Arc<flow_heap::parsing_heaps::SharedMem>,
    _options: &Options,
    _metadata: &Metadata,
    _file: &FileKey,
    _result: flow_services_inference::merge_service::UnitResult<
        Option<flow_services_inference::merge_service::CheckFileResult>,
    >,
) -> UnitResult<Option<((), A)>> {
    match _result {
        Ok(None) => Ok(None),
        Err(e) => Err(e),
        Ok(Some(((cx, type_sig, file_sig, typed_ast), _accs))) => {
            let ast = _reader.get_ast_unsafe(_file);
            let docblock = _reader.get_docblock_unsafe(_file);
            let ccx = codemod_context::typed::TypedCodemodContext {
                file: _file.clone(),
                type_sig,
                file_sig,
                metadata: _metadata.clone(),
                options: _options.clone(),
                cx,
                typed_ast,
                docblock: (*docblock).clone(),
                iteration: _iteration,
                reader: _reader.clone(),
            };
            let result = _visit(_options, &ast, ccx);
            Ok(Some(((), result)))
        }
    }
}

#[allow(unreachable_code)]
fn mk_check<'a, A>(
    _visit: &'a dyn Fn(
        &Options,
        &ast::Program<Loc, Loc>,
        codemod_context::typed::TypedCodemodContext<'_>,
    ) -> A,
    _iteration: i32,
    reader: &'a Arc<flow_heap::parsing_heaps::SharedMem>,
    options: &'a Options,
    _metadata: &'a Metadata,
    master_cx: &'a flow_typing_context::MasterContext,
) -> impl FnMut(FileKey) -> UnitResult<Option<((), A)>> + 'a {
    let (mut check, _cache) = flow_services_inference::merge_service::mk_check(
        reader.clone(),
        Arc::new(options.clone()),
        master_cx,
    );
    move |file: FileKey| -> UnitResult<Option<((), A)>> {
        let result = check(file.clone());
        post_check(
            _visit, _iteration, reader, options, _metadata, &file, result,
        )
    }
}

fn mk_next_for_check<R: 'static>(
    options: &Options,
    workers: &Option<ThreadPool>,
    roots: &BTreeSet<FileKey>,
) -> (
    impl flow_utils_concurrency::map_reduce::Next<FileKey>,
    impl Fn(&mut Vec<R>, Vec<R>) + Send + Sync,
    std::sync::Arc<std::sync::atomic::AtomicUsize>,
) {
    let intermediate_result_callback: std::sync::Arc<dyn Fn(&[R]) + Send + Sync> =
        std::sync::Arc::new(|_| ());
    let max_size = options.max_files_checked_per_worker as usize;
    let num_workers = workers.as_ref().map_or(1, |w| w.num_workers());
    let files: Vec<FileKey> = roots.iter().cloned().collect();
    flow_services_inference::job_utils::mk_next(
        intermediate_result_callback,
        false,
        max_size,
        num_workers,
        files,
    )
}

pub trait TypedRunnerWithPrepassConfig {
    type Accumulator;
    type PrepassState;
    type PrepassResult;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey>;
    fn prepass_init() -> Self::PrepassState;
    fn mod_prepass_options(options: Options) -> Options;
    fn check_options(options: Options) -> Options;
    fn include_dependents_in_prepass() -> bool;
    fn prepass_run(
        cx: flow_typing_context::Context<'_>,
        state: &Self::PrepassState,
        file: FileKey,
        file_options: &Arc<flow_common::files::FileOptions>,
        reader: &Arc<flow_heap::parsing_heaps::SharedMem>,
        file_sig: &Arc<flow_parser_utils::file_sig::FileSig>,
        typed_ast: &ast::Program<flow_aloc::ALoc, (flow_aloc::ALoc, flow_typing_type::type_::Type)>,
    ) -> Self::PrepassResult;
    fn store_precheck_result(results: BTreeMap<FileKey, UnitResult<Self::PrepassResult>>);
    fn visit(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: codemod_context::typed::TypedCodemodContext<'_>,
    ) -> Self::Accumulator;
}

pub trait TypedRunnerConfig {
    type Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator>;
    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey>;
    fn merge_and_check(
        env: &ServerEnv,
        workers: &Option<ThreadPool>,
        options: &Options,
        profiling: &(),
        roots: BTreeSet<FileKey>,
        iteration: i32,
        shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    ) -> impl std::future::Future<Output = ResultList<Self::Accumulator>>;
}

// Checks the codebase and applies C, providing it with the inference context.
pub struct SimpleTypedRunner<C: SimpleTypedRunnerConfig> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: SimpleTypedRunnerConfig> TypedRunnerConfig for SimpleTypedRunner<C> {
    type Accumulator = C::Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        C::reporter()
    }

    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey> {
        C::expand_roots(_env, roots)
    }

    #[allow(unreachable_code)]
    async fn merge_and_check(
        _env: &ServerEnv,
        _workers: &Option<ThreadPool>,
        _options: &Options,
        _profiling: &(),
        _roots: BTreeSet<FileKey>,
        _iteration: i32,
        _shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    ) -> ResultList<Self::Accumulator> {
        let _should_print = _options.profile;
        let reader = _shared_mem.clone();
        let get_dependent_files = |_: &flow_common_utils::graph::Graph<FileKey>,
                                   _: &flow_common_utils::graph::Graph<FileKey>,
                                   _: &BTreeSet<FileKey>|
         -> std::pin::Pin<
            Box<dyn std::future::Future<Output = BTreeSet<FileKey>>>,
        > { Box::pin(async { BTreeSet::new() }) };
        let (_sig_dependency_graph, _components, files_to_merge, _) =
            merge_targets(_env, _options, &(), get_dependent_files, &_roots).await;
        if let Some(pool) = _workers {
            flow_services_inference::type_service::ensure_parsed_or_trigger_recheck(
                pool,
                &reader,
                &Arc::new(_options.clone()),
                files_to_merge.clone().into_iter().collect(),
            )
            .expect("ensure_parsed_or_trigger_recheck failed");
        }
        let mutator = ();
        tracing::info!("Merging {} files", files_to_merge.len());
        if let Some(pool) = _workers {
            let _merge_results = flow_services_inference::merge_service::merge_runner(
                pool,
                &reader,
                _options,
                false, // for_find_all_refs
                _sig_dependency_graph,
                _components,
                &files_to_merge,
                move |_shared_mem: &flow_heap::parsing_heaps::SharedMem,
                      _opts: &Options,
                      _for_find_all_refs: bool,
                      _component: vec1::Vec1<FileKey>| {
                    merge_job(&mutator, _opts, _for_find_all_refs, _shared_mem, _component)
                },
            );
        }
        tracing::info!("Merging done.");
        tracing::info!("Checking {} files", _roots.len());
        let options = C::check_options(_options.clone());
        let metadata = flow_typing_context::metadata_of_options(&options);
        let visit_fn: &dyn Fn(
            &Options,
            &ast::Program<Loc, Loc>,
            codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator = &C::visit;
        let mut check_fn = mk_check(
            visit_fn,
            _iteration,
            &reader,
            &options,
            &metadata,
            &_env.master_cx,
        );
        let (_next, _merge, _count) = mk_next_for_check::<(
            FileKey,
            UnitResult<Option<((), Self::Accumulator)>>,
        )>(&options, _workers, &_roots);
        let files: Vec<FileKey> = _roots.iter().cloned().collect();
        let (job_results, _remaining) =
            flow_services_inference::job_utils::mk_job(&mut check_fn, &options, files);
        let result: ResultList<Self::Accumulator> = job_results;
        tracing::info!("Done");
        result
    }
}

pub struct SimpleTypedTwoPassRunner<C: SimpleTypedRunnerConfig> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: SimpleTypedRunnerConfig> TypedRunnerConfig for SimpleTypedTwoPassRunner<C> {
    type Accumulator = (C::Accumulator, BTreeSet<FileKey>);

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        let c_reporter = C::reporter();
        let report = match c_reporter.report {
            codemod_report::Reporter::StringReporter(f) => {
                codemod_report::Reporter::StringReporter(Box::new(
                    move |opts: &codemod_report::ReporterOptions,
                          (acc, _): &(C::Accumulator, BTreeSet<FileKey>)| {
                        f(opts, acc)
                    },
                ))
            }
            codemod_report::Reporter::UnitReporter(f) => {
                codemod_report::Reporter::UnitReporter(Box::new(
                    move |opts: &codemod_report::ReporterOptions,
                          (acc, _): &(C::Accumulator, BTreeSet<FileKey>)| {
                        f(opts, acc)
                    },
                ))
            }
        };
        let c_combine = c_reporter.combine;
        let combine: Box<
            dyn Fn(Self::Accumulator, Self::Accumulator) -> Self::Accumulator + Send + Sync,
        > = Box::new(move |(acc1, deps1), (acc2, deps2)| {
            ((c_combine)(acc1, acc2), &deps1 | &deps2)
        });
        let empty = (c_reporter.empty, BTreeSet::new());
        codemod_report::CodemodReport {
            report,
            combine,
            empty,
        }
    }

    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey> {
        C::expand_roots(_env, roots)
    }

    #[allow(unreachable_code)]
    async fn merge_and_check(
        _env: &ServerEnv,
        _workers: &Option<ThreadPool>,
        _options: &Options,
        _profiling: &(),
        _roots: BTreeSet<FileKey>,
        _iteration: i32,
        _shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    ) -> ResultList<Self::Accumulator> {
        let _should_print = _options.profile;
        let reader = _shared_mem.clone();
        let get_dependent_files = |_: &flow_common_utils::graph::Graph<FileKey>,
                                   _: &flow_common_utils::graph::Graph<FileKey>,
                                   _: &BTreeSet<FileKey>|
         -> std::pin::Pin<
            Box<dyn std::future::Future<Output = BTreeSet<FileKey>>>,
        > { Box::pin(async { BTreeSet::new() }) };
        let (_sig_dependency_graph, _components, files_to_merge, _) =
            merge_targets(_env, _options, &(), get_dependent_files, &_roots).await;
        if let Some(pool) = _workers {
            flow_services_inference::type_service::ensure_parsed_or_trigger_recheck(
                pool,
                &reader,
                &Arc::new(_options.clone()),
                files_to_merge.clone().into_iter().collect(),
            )
            .expect("ensure_parsed_or_trigger_recheck failed");
        }
        let mutator = ();
        tracing::info!("Merging {} files", files_to_merge.len());
        if let Some(pool) = _workers {
            let _merge_results = flow_services_inference::merge_service::merge_runner(
                pool,
                &reader,
                _options,
                false,
                _sig_dependency_graph,
                _components,
                &files_to_merge,
                move |_sm: &flow_heap::parsing_heaps::SharedMem,
                      _o: &Options,
                      _f: bool,
                      _c: vec1::Vec1<FileKey>| {
                    merge_job(&mutator, _o, _f, _sm, _c)
                },
            );
        }
        tracing::info!("Merging done.");
        tracing::info!("Checking {} files", _roots.len());
        let options = C::check_options(_options.clone());
        let metadata = flow_typing_context::metadata_of_options(&options);
        let visit = |options: &Options,
                     ast: &ast::Program<Loc, Loc>,
                     ctx: codemod_context::typed::TypedCodemodContext<'_>| {
            let cx = ctx.cx.clone();
            let acc = C::visit(options, ast, ctx);
            let files = cx.reachable_deps().clone();
            (acc, files)
        };
        let mut check_fn = mk_check(
            &visit,
            _iteration,
            &reader,
            &options,
            &metadata,
            &_env.master_cx,
        );
        let files: Vec<FileKey> = _roots.iter().cloned().collect();
        let (job_results, _remaining) =
            flow_services_inference::job_utils::mk_job(&mut check_fn, &options, files);
        let initial_run_result: ResultList<Self::Accumulator> = job_results;
        tracing::info!("Initial run done");
        let second_run_roots: BTreeSet<FileKey> = initial_run_result.iter().fold(
            BTreeSet::<FileKey>::new(),
            |mut acc: BTreeSet<FileKey>, item| {
                let (_fk, r) = item;
                match r {
                    Ok(Some((_acc_inner, files))) => {
                        for f in files.difference(&_roots) {
                            acc.insert(f.clone());
                        }
                    }
                    _ => {}
                }
                acc
            },
        );
        let get_dependent_files2 = |_: &flow_common_utils::graph::Graph<FileKey>,
                                    _: &flow_common_utils::graph::Graph<FileKey>,
                                    _: &BTreeSet<FileKey>|
         -> std::pin::Pin<
            Box<dyn std::future::Future<Output = BTreeSet<FileKey>>>,
        > { Box::pin(async { BTreeSet::new() }) };
        let (_sig_dependency_graph2, _components2, files_to_merge2, _) =
            merge_targets(_env, _options, &(), get_dependent_files2, &second_run_roots).await;
        if let Some(pool) = _workers {
            flow_services_inference::type_service::ensure_parsed_or_trigger_recheck(
                pool,
                &reader,
                &Arc::new(_options.clone()),
                files_to_merge2.clone().into_iter().collect(),
            )
            .expect("ensure_parsed_or_trigger_recheck failed");
        }
        let mutator2 = ();
        tracing::info!("Merging {} files", files_to_merge2.len());
        if let Some(pool) = _workers {
            let _merge_results = flow_services_inference::merge_service::merge_runner(
                pool,
                &reader,
                _options,
                false,
                _sig_dependency_graph2,
                _components2,
                &files_to_merge2,
                move |_sm: &flow_heap::parsing_heaps::SharedMem,
                      _o: &Options,
                      _f: bool,
                      _c: vec1::Vec1<FileKey>| {
                    merge_job(&mutator2, _o, _f, _sm, _c)
                },
            );
        }
        tracing::info!("Merging done.");
        let mut check_fn2 = mk_check(
            &visit,
            _iteration,
            &reader,
            &options,
            &metadata,
            &_env.master_cx,
        );
        let files2: Vec<FileKey> = second_run_roots.iter().cloned().collect();
        let (job_results2, _remaining2) =
            flow_services_inference::job_utils::mk_job(&mut check_fn2, &options, files2);
        let mut result: ResultList<Self::Accumulator> = initial_run_result;
        result.extend(job_results2);
        tracing::info!("Pruned-deps run done");
        result
    }
}

pub struct TypedRunnerWithPrepass<C: TypedRunnerWithPrepassConfig> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: TypedRunnerWithPrepassConfig> TypedRunnerConfig for TypedRunnerWithPrepass<C> {
    type Accumulator = C::Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        C::reporter()
    }

    fn expand_roots(_env: &ServerEnv, roots: BTreeSet<FileKey>) -> BTreeSet<FileKey> {
        C::expand_roots(_env, roots)
    }

    #[allow(unreachable_code)]
    async fn merge_and_check(
        _env: &ServerEnv,
        _workers: &Option<ThreadPool>,
        _options: &Options,
        _profiling: &(),
        _roots: BTreeSet<FileKey>,
        _iteration: i32,
        _shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    ) -> ResultList<Self::Accumulator> {
        let _should_print = _options.profile;
        let reader = _shared_mem.clone();
        let get_dependent_files = |sig_dep: &flow_common_utils::graph::Graph<FileKey>,
                                   impl_dep: &flow_common_utils::graph::Graph<FileKey>,
                                   roots: &BTreeSet<FileKey>|
         -> std::pin::Pin<
            Box<dyn std::future::Future<Output = BTreeSet<FileKey>>>,
        > {
            if C::include_dependents_in_prepass() {
                let roots_set: flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey> =
                    roots.iter().cloned().collect();
                let result =
                    flow_services_inference::pure_dep_graph_operations::calc_all_dependents(
                        sig_dep, impl_dep, &roots_set,
                    );
                let btree_result: BTreeSet<FileKey> = result.into_iter().collect();
                Box::pin(async move { btree_result })
            } else {
                Box::pin(async { BTreeSet::new() })
            }
        };
        let (_sig_dependency_graph, _components, files_to_merge, _files_to_check) =
            merge_targets(_env, _options, &(), get_dependent_files, &_roots).await;
        if let Some(pool) = _workers {
            flow_services_inference::type_service::ensure_parsed_or_trigger_recheck(
                pool,
                &reader,
                &Arc::new(_options.clone()),
                files_to_merge.clone().into_iter().collect(),
            )
            .expect("ensure_parsed_or_trigger_recheck failed");
        }
        let mutator = ();
        tracing::info!("Merging {} files", files_to_merge.len());
        if let Some(pool) = _workers {
            let _merge_results = flow_services_inference::merge_service::merge_runner(
                pool,
                &reader,
                _options,
                false,
                _sig_dependency_graph,
                _components,
                &files_to_merge,
                move |_sm: &flow_heap::parsing_heaps::SharedMem,
                      _o: &Options,
                      _f: bool,
                      _c: vec1::Vec1<FileKey>| {
                    merge_job(&mutator, _o, _f, _sm, _c)
                },
            );
        }
        tracing::info!("Merging done.");
        let files_to_check = checked_set_all(_files_to_check);
        tracing::info!("Pre-Checking {} files", files_to_check.len());
        let precheck_result: BTreeMap<FileKey, UnitResult<C::PrepassResult>> = {
            let state = C::prepass_init();
            let options = C::mod_prepass_options(_options.clone());
            let master_cx = &_env.master_cx;
            let (mut check, _cache) = flow_services_inference::merge_service::mk_check(
                reader.clone(),
                Arc::new(options.clone()),
                master_cx,
            );
            let mut acc: BTreeMap<FileKey, UnitResult<C::PrepassResult>> = BTreeMap::new();
            let files_to_check_list: Vec<FileKey> = files_to_check.iter().cloned().collect();
            for file in files_to_check_list {
                match check(file.clone()) {
                    Ok(None) => {}
                    Ok(Some(((cx, _type_sig, file_sig, typed_ast), _))) => {
                        let result = C::prepass_run(
                            cx,
                            &state,
                            file.clone(),
                            &options.file_options,
                            &reader,
                            &file_sig,
                            &typed_ast,
                        );
                        acc.insert(file, Ok(result));
                    }
                    Err(e) => {
                        acc.insert(file, Err(e));
                    }
                }
            }
            acc
        };
        tracing::info!("Pre-checking Done");
        tracing::info!("Storing pre-checking results");
        C::store_precheck_result(precheck_result);
        tracing::info!("Storing pre-checking results Done");
        tracing::info!("Checking+Codemodding {} files", _roots.len());
        let options = C::check_options(_options.clone());
        let metadata = flow_typing_context::metadata_of_options(&options);
        let visit_fn: &dyn Fn(
            &Options,
            &ast::Program<Loc, Loc>,
            codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator = &C::visit;
        let mut check_fn = mk_check(
            visit_fn,
            _iteration,
            &reader,
            &options,
            &metadata,
            &_env.master_cx,
        );
        let files: Vec<FileKey> = _roots.iter().cloned().collect();
        let (job_results, _remaining) =
            flow_services_inference::job_utils::mk_job(&mut check_fn, &options, files);
        let result: ResultList<Self::Accumulator> = job_results;
        tracing::info!("Checking+Codemodding Done");
        result
    }
}

pub struct TypedRunner<TRC: TypedRunnerConfig> {
    _phantom: std::marker::PhantomData<TRC>,
}

impl<TRC: TypedRunnerConfig> StepRunner for TypedRunner<TRC>
where
    TRC::Accumulator: Clone,
{
    type Env = ServerEnv;
    type Accumulator = TRC::Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        TRC::reporter()
    }

    #[allow(unreachable_code)]
    async fn init_run(
        _genv: &Genv,
        _roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let options = &*_genv.options;
        let workers = &_genv.workers;
        let should_print_summary = options.profile;
        let profiling = profiling_start("Codemod", should_print_summary);
        extract_flowlibs_or_exit(options);
        let shared_mem = &_genv.shared_mem;
        let options_arc = Arc::new(options.clone());
        let pool = workers.as_ref().expect("workers required for init");
        let root = &options.root;
        let (env, _libs_ok, _node_modules_containers) =
            flow_services_inference::type_service::init_from_scratch(
                &options_arc,
                pool,
                shared_mem,
                root,
            );
        let file_options = &options.file_options;
        let all_unordered_libs: BTreeSet<String> = env
            .all_unordered_libs
            .iter()
            .map(|s| s.to_string())
            .collect();
        let all = options.all;
        let roots = get_target_filename_set(file_options, &all_unordered_libs, all, _roots);
        let roots = TRC::expand_roots(&env, roots);
        let env_files: BTreeSet<FileKey> = env.files.iter().cloned().collect();
        let roots: BTreeSet<FileKey> = roots.intersection(&env_files).cloned().collect();
        log_input_files(&roots);
        let results =
            TRC::merge_and_check(&env, workers, options, &profiling, roots, 0, shared_mem).await;
        ((), (env, results))
    }

    #[allow(unreachable_code)]
    async fn recheck_run(
        _genv: &Genv,
        _env: Self::Env,
        _iteration: i32,
        _roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let options = &*_genv.options;
        let workers = &_genv.workers;
        let should_print_summary = options.profile;
        let profiling = profiling_start("Codemod", should_print_summary);
        diff_heaps_remove_batch(&_roots);
        let shared_mem = &_genv.shared_mem;
        let options_arc = Arc::new(options.clone());
        let pool = workers.as_ref().expect("workers required for recheck");
        let mut updates = flow_common_utils::checked_set::CheckedSet::empty();
        let focused_set: flow_data_structure_wrapper::ord_set::FlowOrdSet<FileKey> =
            _roots.iter().cloned().collect();
        updates.add(Some(focused_set), None, None);
        let def_info = flow_services_get_def::get_def_types::DefInfo::NoDefinition(None);
        let files_to_force = flow_common_utils::checked_set::CheckedSet::empty();
        let node_modules_containers = Arc::new(std::sync::RwLock::new(BTreeMap::new()));
        let mut will_be_checked_files = flow_common_utils::checked_set::CheckedSet::empty();
        let recheck_result = flow_services_inference::type_service::recheck(
            pool,
            shared_mem,
            &options_arc,
            &updates,
            &def_info,
            files_to_force,
            false,
            None,  // changed_mergebase
            false, // missed_changes
            &node_modules_containers,
            &mut will_be_checked_files,
            _env,
        );
        let (_, _, _, env) = recheck_result.expect("recheck failed");
        log_input_files(&_roots);
        let results = TRC::merge_and_check(
            &env, workers, options, &profiling, _roots, _iteration, shared_mem,
        )
        .await;
        ((), (env, results))
    }

    #[allow(unreachable_code)]
    fn digest(results: ResultList<Self::Accumulator>) -> (Vec<FileKey>, Self::Accumulator) {
        let reporter = TRC::reporter();
        let empty = reporter.empty.clone();
        results.into_iter().fold(
            (Vec::new(), empty),
            |(mut acc_files, acc_result), (file_key, result)| match result {
                Ok(Some(ok)) => {
                    acc_files.push(file_key);
                    let combined = (reporter.combine)(acc_result, ok);
                    (acc_files, combined)
                }
                Ok(None) => (acc_files, acc_result),
                Err((aloc, err)) => {
                    eprintln!(
                        "{}: {}",
                        flow_common::reason::string_of_aloc(None, &aloc),
                        flow_typing_errors::error_message::string_of_internal_error(&err),
                    );
                    (acc_files, acc_result)
                }
            },
        )
    }
}

pub fn untyped_runner_job<A, Ctx>(
    mk_ccx: impl Fn(&FileKey) -> Ctx,
    visit: impl Fn(&ast::Program<Loc, Loc>, Ctx) -> A,
    abstract_reader: &flow_heap::parsing_heaps::SharedMem,
    file_list: Vec<FileKey>,
) -> ResultList<A> {
    file_list.into_iter().fold(Vec::new(), |mut acc, file| {
        let ast = abstract_reader.get_ast_unsafe(&file);
        let result = visit(&ast, mk_ccx(&file));
        acc.push((file, Ok(Some(result))));
        acc
    })
}

pub fn untyped_digest<A>(
    reporter: &codemod_report::CodemodReport<A>,
    results: ResultList<A>,
) -> (Vec<FileKey>, A)
where
    A: Clone,
{
    let empty = reporter.empty.clone();
    results.into_iter().fold(
        (Vec::new(), empty),
        |(mut acc_files, acc_result), (file_key, result)| match result {
            Err(_) | Ok(None) => (acc_files, acc_result),
            Ok(Some(result)) => {
                acc_files.push(file_key);
                let combined = (reporter.combine)(result, acc_result);
                (acc_files, combined)
            }
        },
    )
}

pub struct UntypedRunner<C: UntypedRunnerConfig> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: UntypedRunnerConfig> UntypedRunner<C>
where
    C::Accumulator: Clone,
{
    #[allow(unreachable_code)]
    async fn run(_genv: &Genv, _roots: BTreeSet<FileKey>) -> ((), ResultList<C::Accumulator>) {
        let options = &*_genv.options;
        let workers = &_genv.workers;
        let should_print_summary = options.profile;
        let _profiling = profiling_start("Codemod", should_print_summary);
        let file_options = &options.file_options;
        let all = options.all;
        let (_ordered_libs, all_unordered_libs) =
            flow_common::files::ordered_and_unordered_lib_paths(file_options);
        let filename_set = get_target_filename_set(file_options, &all_unordered_libs, all, _roots);
        let next = next_of_filename_set(workers, &filename_set);
        let reader = _genv.shared_mem.clone();
        let parse_results = flow_parsing::parsing_service::parse_with_defaults(
            workers.as_ref().unwrap(),
            &reader,
            &Arc::new(options.clone()),
            &[],
            next,
        );
        let roots: BTreeSet<FileKey> = parse_results.parsed.into_iter().collect();
        log_input_files(&roots);
        let _next2 = next_of_filename_set(workers, &roots);
        let mk_ccx =
            |file: &FileKey| codemod_context::untyped::UntypedCodemodContext { file: file.clone() };
        let visit = |ast: &ast::Program<Loc, Loc>,
                     ccx: codemod_context::untyped::UntypedCodemodContext| {
            C::visit(options, ast, ccx)
        };
        let files: Vec<FileKey> = roots.into_iter().collect();
        let result: ResultList<C::Accumulator> = untyped_runner_job(mk_ccx, visit, &reader, files);
        ((), result)
    }
}

impl<C: UntypedRunnerConfig> StepRunner for UntypedRunner<C>
where
    C::Accumulator: Clone,
{
    type Env = ();
    type Accumulator = C::Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        C::reporter()
    }

    async fn init_run(
        genv: &Genv,
        roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let ((), results) = Self::run(genv, roots).await;
        ((), ((), results))
    }

    async fn recheck_run(
        genv: &Genv,
        _env: Self::Env,
        _iteration: i32,
        roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let ((), results) = Self::run(genv, roots).await;
        ((), ((), results))
    }

    fn digest(results: ResultList<Self::Accumulator>) -> (Vec<FileKey>, Self::Accumulator) {
        let reporter = C::reporter();
        untyped_digest(&reporter, results)
    }
}

pub struct UntypedFlowInitRunner<C: UntypedFlowInitRunnerConfig> {
    _phantom: std::marker::PhantomData<C>,
}

impl<C: UntypedFlowInitRunnerConfig> UntypedFlowInitRunner<C>
where
    C::Accumulator: Clone,
{
    #[allow(unreachable_code)]
    async fn run(_genv: &Genv, _roots: BTreeSet<FileKey>) -> ((), ResultList<C::Accumulator>) {
        let options = &*_genv.options;
        let workers = &_genv.workers;
        let should_print_summary = options.profile;
        let _profiling = profiling_start("Codemod", should_print_summary);
        let mut options = options.clone();
        options.all = true;
        extract_flowlibs_or_exit(&options);
        let shared_mem = &_genv.shared_mem;
        let options_arc = Arc::new(options.clone());
        let pool = workers.as_ref().expect("workers required for init");
        let root = &options.root;
        let (env, _libs_ok, _node_modules_containers) =
            flow_services_inference::type_service::init_from_scratch(
                &options_arc,
                pool,
                shared_mem,
                root,
            );
        let file_options = &options.file_options;
        let all = options.all;
        let all_unordered_libs: BTreeSet<String> = env
            .all_unordered_libs
            .iter()
            .map(|s| s.to_string())
            .collect();
        let filename_set = get_target_filename_set(file_options, &all_unordered_libs, all, _roots);
        let env_files: BTreeSet<FileKey> = env.files.iter().cloned().collect();
        let filename_set: BTreeSet<FileKey> =
            filename_set.intersection(&env_files).cloned().collect();
        let _next = next_of_filename_set(workers, &filename_set);
        let reader = shared_mem.clone();
        C::init(&reader);
        let reader_for_ccx = reader.clone();
        let mk_ccx =
            |file: &FileKey| codemod_context::untyped_flow_init::UntypedFlowInitCodemodContext {
                file: file.clone(),
                reader: reader_for_ccx.clone(),
            };
        let visit =
            |ast: &ast::Program<Loc, Loc>,
             ccx: codemod_context::untyped_flow_init::UntypedFlowInitCodemodContext| {
                C::visit(&options, ast, ccx)
            };
        let abstract_reader = reader.clone();
        log_input_files(&filename_set);
        let files: Vec<FileKey> = filename_set.into_iter().collect();
        let result: ResultList<C::Accumulator> =
            untyped_runner_job(mk_ccx, visit, &abstract_reader, files);
        ((), result)
    }
}

impl<C: UntypedFlowInitRunnerConfig> StepRunner for UntypedFlowInitRunner<C>
where
    C::Accumulator: Clone,
{
    type Env = ();
    type Accumulator = C::Accumulator;

    fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
        C::reporter()
    }

    async fn init_run(
        genv: &Genv,
        roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let ((), results) = Self::run(genv, roots).await;
        ((), ((), results))
    }

    async fn recheck_run(
        genv: &Genv,
        _env: Self::Env,
        _iteration: i32,
        roots: BTreeSet<FileKey>,
    ) -> ((), (Self::Env, ResultList<Self::Accumulator>)) {
        let ((), results) = Self::run(genv, roots).await;
        ((), ((), results))
    }

    fn digest(results: ResultList<Self::Accumulator>) -> (Vec<FileKey>, Self::Accumulator) {
        let reporter = C::reporter();
        untyped_digest(&reporter, results)
    }
}

const MAX_NUMBER_OF_ITERATIONS: i32 = 10;

fn header(iteration: i32, filenames: &BTreeSet<FileKey>) {
    // Use natural counting starting from 1.
    println!(">>> Iteration: {}", iteration + 1);
    println!(">>> Running codemod on {} files...", filenames.len());
}

pub struct RepeatRunner<SR: StepRunner> {
    _phantom: std::marker::PhantomData<SR>,
}

impl<SR: StepRunner> RepeatRunner<SR>
where
    SR::Accumulator: Clone,
{
    async fn post_run(
        _options: &Options,
        write: bool,
        results: ResultList<SR::Accumulator>,
    ) -> Option<BTreeSet<FileKey>> {
        let strip_root: Option<std::path::PathBuf> = if _options.strip_root {
            Some((*_options.root).clone())
        } else {
            None
        };

        let reporter_options = codemod_report::ReporterOptions {
            strip_root: strip_root.clone(),
            exact_by_default: _options.exact_by_default,
        };

        let (files, report) = SR::digest(results);

        let changed_files = codemod_printer::print_asts(&strip_root, write, files).await;

        let sr_reporter = SR::reporter();
        codemod_printer::print_results(&reporter_options, &sr_reporter.report, &report);

        changed_files.map(|files| files.into_iter().collect())
    }

    async fn loop_run(
        genv: &Genv,
        mut env: SR::Env,
        mut iteration: i32,
        options: &Options,
        write: bool,
        mut changed_files: Option<BTreeSet<FileKey>>,
    ) {
        loop {
            if iteration > MAX_NUMBER_OF_ITERATIONS {
                eprintln!(">>> Reached maximum number of iterations (10). Exiting...");
                return;
            }
            match changed_files {
                None => return,
                Some(ref set) if set.is_empty() => return,
                Some(roots) => {
                    header(iteration, &roots);
                    let (_, (new_env, results)) =
                        SR::recheck_run(genv, env, iteration, roots).await;
                    env = new_env;
                    changed_files = Self::post_run(options, write, results).await;
                    iteration += 1;
                }
            }
        }
    }
}

impl<SR: StepRunner> Runnable for RepeatRunner<SR>
where
    SR::Accumulator: Clone,
{
    async fn run(genv: &Genv, write: bool, repeat: bool, roots: BTreeSet<FileKey>) {
        if repeat {
            header(0, &roots);
        }
        let (_prof, (env, results)) = SR::init_run(genv, roots).await;
        let options = &*genv.options;
        let changed_files = Self::post_run(options, write, results).await;
        if repeat {
            Self::loop_run(genv, env, 1, options, write, changed_files).await;
        }
    }
}

pub type MakeSimpleTypedRunner<C> = RepeatRunner<TypedRunner<SimpleTypedRunner<C>>>;

pub type MakeSimpleTypedTwoPassRunner<C> = RepeatRunner<TypedRunner<SimpleTypedTwoPassRunner<C>>>;

pub type MakeTypedRunnerWithPrepass<C> = RepeatRunner<TypedRunner<TypedRunnerWithPrepass<C>>>;

pub type MakeUntypedFlowInitRunner<C> = RepeatRunner<UntypedFlowInitRunner<C>>;

pub type MakeUntypedRunner<C> = RepeatRunner<UntypedRunner<C>>;
