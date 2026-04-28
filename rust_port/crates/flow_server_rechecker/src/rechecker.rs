/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::thread;
use std::time::Instant;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_server_env::error_collator;
use flow_server_env::file_watcher_status;
use flow_server_env::lsp_prot;
use flow_server_env::monitor_rpc;
use flow_server_env::persistent_connection;
use flow_server_env::server_env;
use flow_server_env::server_monitor_listener_state;
use flow_server_env::server_monitor_listener_state::Priority;
use flow_server_env::server_monitor_listener_state::Updates;
use flow_server_env::server_prot;
use flow_server_env::server_status;
use flow_services_inference::type_service;
use flow_services_references::find_refs_types;
use flow_utils_concurrency::worker_cancel;

use crate::recheck_updates;

pub struct ProfilingFinished {
    pub duration: f64,
}

impl ProfilingFinished {
    pub fn get_profiling_duration(&self) -> f64 {
        self.duration
    }
}
mod parallelizable_workload_loop {
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;

    use flow_server_env::server_env;
    use flow_server_env::server_monitor_listener_state;

    pub(super) fn run(wait_for_cancel: &AtomicBool, env: &server_env::Env) {
        loop {
            if wait_for_cancel.load(Ordering::Acquire) {
                return;
            }
            server_monitor_listener_state::wait_for_parallelizable_workload_or_stop(
                wait_for_cancel,
            );
            if wait_for_cancel.load(Ordering::Acquire) {
                return;
            }
            match server_monitor_listener_state::pop_next_parallelizable_workload() {
                Some(workload) => {
                    log::info!("Running a parallel workload");
                    (workload.parallelizable_workload_handler)(env);
                }
                None => {}
            }
        }
    }
}

fn start_parallelizable_workloads(
    _genv: &server_env::Genv,
    env: &server_env::Env,
) -> Box<dyn FnOnce()> {
    let wait_for_cancel = Arc::new(AtomicBool::new(false));
    let env_for_loop = env.clone();
    let wait_for_cancel_for_loop = wait_for_cancel.dupe();
    let loop_thread = thread::Builder::new()
        .name("parallelizable_workload_loop".to_string())
        .spawn(move || {
            parallelizable_workload_loop::run(&wait_for_cancel_for_loop, &env_for_loop);
        })
        .expect("failed to spawn parallelizable_workload_loop thread");
    let mut already_woken = false;
    let mut loop_thread = Some(loop_thread);
    Box::new(move || {
        if !already_woken {
            wait_for_cancel.store(true, Ordering::Release);
            server_monitor_listener_state::wake_workload_waiters();
        }
        #[allow(unused_assignments)]
        {
            already_woken = true;
        }

        if let Some(handle) = loop_thread.take() {
            if let Err(panic_payload) = handle.join() {
                std::panic::resume_unwind(panic_payload);
            }
        }
    })
}

pub fn get_lazy_stats(
    options: &Options,
    env: &server_env::Env,
) -> server_prot::response::LazyStats {
    // Report only focused + dependents as "checked" files. Dependencies are only
    // merged (signatures computed) but not type-checked, so they shouldn't count
    // toward the user-visible "checking N files" number.
    let (focused_count, checked_libdef_files) =
        env.checked_files
            .focused()
            .iter()
            .fold((0i32, 0i32), |(total, libs), f| {
                if f.is_lib_file() {
                    (total + 1, libs + 1)
                } else {
                    (total + 1, libs)
                }
            });
    let checked_files = focused_count + env.checked_files.dependents_cardinal() as i32;
    let (total_files, total_libdef_files) =
        env.files.iter().fold((0i32, 0i32), |(total, libs), f| {
            if f.is_lib_file() {
                (total + 1, libs + 1)
            } else {
                (total + 1, libs)
            }
        });
    server_prot::response::LazyStats {
        lazy_mode: options.lazy_mode,
        checked_files,
        checked_libdef_files,
        total_files,
        total_libdef_files,
    }
}

pub fn process_updates(
    skip_incompatible: bool,
    options: &Options,
    env: &server_env::Env,
    shared_mem: &SharedMem,
    updates: &BTreeSet<String>,
) -> Updates {
    match recheck_updates::process_updates(
        skip_incompatible,
        options,
        &env.all_unordered_libs,
        shared_mem,
        updates,
    ) {
        Ok(updates) => Updates::NormalUpdates(updates),
        Err(recheck_updates::Error::RecoverableShouldReinitNonLazily { msg, updates }) => {
            eprintln!("{}", msg);
            Updates::RequiredFullCheckReinit(updates)
        }
        Err(recheck_updates::Error::Unrecoverable { msg, exit_status }) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(exit_status);
        }
    }
}

fn send_start_recheck(env: &server_env::Env) {
    monitor_rpc::status_update(server_status::Event::RecheckStart);
    persistent_connection::send_start_recheck(&env.connections);
    persistent_connection::send_status(
        server_status::Status::Typechecking(
            server_status::TypecheckMode::Rechecking,
            server_status::TypecheckStatus::StartingTypecheck,
        ),
        (
            file_watcher_status::FileWatcher::NoFileWatcher,
            file_watcher_status::StatusKind::Ready,
        ),
        &env.connections,
    );
}

// We must send "end_recheck" prior to sending errors+warnings so the client
// knows that this set of errors+warnings are final ones, not incremental.
fn send_end_recheck(options: &Options, env: &server_env::Env) {
    let lazy_stats = get_lazy_stats(options, env);
    persistent_connection::send_end_recheck(lazy_stats, &env.connections);
    persistent_connection::send_status(
        server_status::Status::Free,
        (
            file_watcher_status::FileWatcher::NoFileWatcher,
            file_watcher_status::StatusKind::Ready,
        ),
        &env.connections,
    );

    persistent_connection::update_clients(
        &env.connections,
        lsp_prot::ErrorsReason::EndOfRecheck,
        || {
            let (errors, warnings) = error_collator::get_with_separate_warnings(env);
            (errors, warnings.clone())
        },
    );

    monitor_rpc::status_update(server_status::Event::FinishingUp);
}

fn persistent_server_logging_context() -> lsp_prot::LoggingContext {
    lsp_prot::LoggingContext {
        from: None,
        agent_id: None,
    }
}

fn recheck(
    genv: &server_env::Genv,
    env: server_env::Env,
    files_to_force: CheckedSet,
    find_ref_command: &mut Option<server_monitor_listener_state::FindRefCommand>,
    incompatible_lib_change: bool,
    changed_mergebase: Option<bool>,
    missed_changes: bool,
    will_be_checked_files: &mut CheckedSet,
    updates: CheckedSet,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> Result<(ProfilingFinished, server_env::Env), type_service::RecheckError> {
    let options = &genv.options;
    let workers = genv.workers.as_ref().unwrap();

    let find_ref_request = match find_ref_command.as_ref() {
        Some(server_monitor_listener_state::FindRefCommand { request, .. }) => request.clone(),
        None => find_refs_types::empty_request(),
    };

    let _should_print_summary = options.profile;
    let recheck_start = Instant::now();

    send_start_recheck(&env);

    if incompatible_lib_change {
        flow_flowlib::extract_if_missing_or_exit(options.file_options.default_lib_dir.as_ref());
    }

    let recheck_result = type_service::recheck(
        workers,
        &genv.shared_mem,
        options,
        &updates,
        &find_ref_request.def_info,
        files_to_force,
        incompatible_lib_change,
        changed_mergebase,
        missed_changes,
        node_modules_containers,
        will_be_checked_files,
        env,
    );
    let (log_recheck_event, recheck_stats, find_ref_results, env) = match recheck_result {
        Ok((log_event, stats, results, env)) => (log_event, stats, results, env),
        Err(type_service::RecheckError::Canceled(changed_files)) => {
            return Err(type_service::RecheckError::Canceled(changed_files));
        }
        Err(type_service::RecheckError::TooSlow) => {
            unreachable!("TooSlow is handled inside type_service::recheck");
        }
    };

    if let Some(server_monitor_listener_state::FindRefCommand {
        client_id,
        references_to_lsp_response,
        ..
    }) = find_ref_command.take()
    {
        let (response, metadata) = references_to_lsp_response(find_ref_results);
        let metadata = lsp_prot::Metadata {
            server_logging_context: Some(persistent_server_logging_context()),
            ..metadata
        };
        if let Some(client) = persistent_connection::get_client(client_id) {
            persistent_connection::send_response((response, metadata), &client);
        }
    }

    send_end_recheck(options, &env);

    let duration = recheck_start.elapsed().as_secs_f64();
    let profiling = ProfilingFinished { duration };

    log_recheck_event();

    let lsp_stats = lsp_prot::RecheckStats {
        dependent_file_count: recheck_stats.dependent_file_count as i32,
        changed_file_count: recheck_stats.changed_file_count as i32,
        top_cycle: recheck_stats.top_cycle.map(|(f, s)| (f, s as i32)),
    };
    monitor_rpc::send_telemetry(lsp_prot::TelemetryFromServer::RecheckSummary {
        duration: profiling.get_profiling_duration(),
        stats: lsp_stats,
    });

    Ok((profiling, env))
}

// Runs a function which should be canceled if we are notified about any file changes. After the
// thread is canceled, post_cancel is called and its result returned
fn run_but_cancel_on_file_changes<T>(
    _options: &Options,
    _shared_mem: &SharedMem,
    _get_forced: &dyn Fn() -> CheckedSet,
    _priority: Priority,
    f: impl FnOnce() -> T,
    _pre_cancel: impl FnOnce(),
    _post_cancel: impl FnOnce() -> T,
) -> T {
    let cancel_monitor = spawn_recheck_cancel_monitor();
    let ret = f();
    cancel_monitor.stop();
    ret
}

/// Stop handle for the recheck cancel monitor thread spawned by
/// `spawn_recheck_cancel_monitor`. Calling `stop()` deregisters the monitor
/// and joins its thread; safe to call after the monitor has already fired and
/// exited.
struct RecheckCancelMonitor {
    stop_tx: crossbeam::channel::Sender<()>,
    thread: Option<std::thread::JoinHandle<()>>,
}

impl RecheckCancelMonitor {
    fn stop(mut self) {
        // Either the monitor is still waiting on its select! (Ok(()) wakes it
        // via the stop branch) or it already fired on a recheck-stream push
        // and exited (Full/Disconnected — nothing to wake). All three cases
        // mean the monitor will exit, so we can join unconditionally.
        match self.stop_tx.try_send(()) {
            Ok(()) => {}
            Err(crossbeam::channel::TrySendError::Full(())) => {}
            Err(crossbeam::channel::TrySendError::Disconnected(())) => {}
        }
        if let Some(handle) = self.thread.take() {
            if let Err(panic_payload) = handle.join() {
                std::panic::resume_unwind(panic_payload);
            }
        }
    }
}

/// Spawn the recheck cancel monitor: subscribes to the recheck push channel
/// and waits for either:
///   1. A new force-recheck or file-watcher update (push to recheck stream)
///      → call `worker_cancel::stop_workers()` so the in-progress recheck
///      observes cancel and unwinds back to `recheck_single`.
///   2. The recheck completing on its own (`RecheckCancelMonitor::stop`)
///      → exit without signaling cancel.
///
/// This is the Rust port equivalent of OCaml's `cancel_thread` half of
/// `Lwt.pick` inside `run_but_cancel_on_file_changes` (rechecker.ml:228).
fn spawn_recheck_cancel_monitor() -> RecheckCancelMonitor {
    let push_rx = server_monitor_listener_state::subscribe_recheck_pushes();
    let (stop_tx, stop_rx) = crossbeam::channel::bounded::<()>(1);
    let thread = std::thread::Builder::new()
        .name("recheck_cancel_monitor".to_string())
        .spawn(move || {
            crossbeam::channel::select! {
                recv(push_rx) -> _ => {
                    eprintln!(
                        "Canceling recheck because a new force-recheck or file-watcher update arrived"
                    );
                    worker_cancel::stop_workers();
                }
                recv(stop_rx) -> _ => {}
            }
        })
        .expect("failed to spawn recheck_cancel_monitor thread");
    RecheckCancelMonitor {
        stop_tx,
        thread: Some(thread),
    }
}

pub(crate) enum RecheckOutcome {
    NothingToDo(server_env::Env),
    CompletedRecheck {
        profiling: ProfilingFinished,
        env: server_env::Env,
        recheck_count: usize,
    },
}

// This ref is an estimate of the files which will be checked by the time the recheck is done.
// As the recheck progresses, the estimate will get better. We use this estimate to prevent
// canceling the recheck to force a file which we were already going to check.
// This early estimate is not a very good estimate, since it's missing new dependents and
// dependencies. However it should be good enough to prevent rechecks continuously restarting as
// the server gets spammed with autocomplete requests.
pub(crate) fn recheck_single(
    recheck_count: usize,
    genv: &server_env::Genv,
    env: server_env::Env,
    shared_mem: &SharedMem,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> RecheckOutcome {
    let env = server_monitor_listener_state::update_env(env);
    let options = &genv.options;
    let process_updates = |skip_incompatible: bool, updates: &BTreeSet<String>| -> Updates {
        process_updates(skip_incompatible, options, &env, shared_mem, updates)
    };

    let mut will_be_checked_files = env.checked_files.clone();

    let (priority, mut workload) =
        server_monitor_listener_state::get_and_clear_recheck_workload(&process_updates, &|| {
            will_be_checked_files.clone()
        });

    let did_change_mergebase = workload.metadata.changed_mergebase.unwrap_or(false);
    let missed_changes = workload.metadata.missed_changes;
    let incompatible_lib_change = workload.incompatible_lib_change;

    let mut files_to_recheck_set = CheckedSet::empty();
    files_to_recheck_set.add(
        Some(workload.files_to_recheck.dupe()),
        None,
        Some(workload.files_to_prioritize.dupe()),
    );

    if missed_changes && !did_change_mergebase {
        files_to_recheck_set.add(Some(env.checked_files.focused().clone()), None, None);
    }

    if !incompatible_lib_change
        && !did_change_mergebase
        && files_to_recheck_set.is_empty()
        && workload.files_to_force.is_empty()
    {
        return RecheckOutcome::NothingToDo(env);
    }

    let stop_parallelizable_workloads = start_parallelizable_workloads(genv, &env);
    let env_for_cancel = env.clone();

    // The canceled recheck, or a preceding sequence of canceled rechecks where none completed,
    // may have introduced garbage into shared memory. Since we immediately start another
    // recheck, we should first check whether we need to compact. Otherwise, sharedmem could
    // potentially grow unbounded.
    // The constant budget provided here should be sufficient to fully scan a 5G heap within 5
    // iterations. We want to avoid the scenario where repeatedly cancelled rechecks cause the
    // heap to grow faster than we can scan. An algorithmic approach to determine the amount of
    // work based on the allocation rate would be better.
    // Adding files_to_force to will_be_checked_files makes sure that future requests for
    // the same files doesn't cause us to cancel a check that was already working on
    // files are definitely checked, so we can add them now.
    worker_cancel::resume_workers();
    will_be_checked_files.union(workload.files_to_force.dupe());

    let f = move || match recheck(
        genv,
        env,
        workload.files_to_force.dupe(),
        &mut workload.find_ref_command,
        incompatible_lib_change,
        workload.metadata.changed_mergebase,
        missed_changes,
        &mut will_be_checked_files,
        files_to_recheck_set,
        node_modules_containers,
    ) {
        Ok((profiling, env)) => {
            stop_parallelizable_workloads();
            server_monitor_listener_state::requeue_deferred_parallelizable_workloads();

            RecheckOutcome::CompletedRecheck {
                profiling,
                env,
                recheck_count,
            }
        }
        Err(type_service::RecheckError::Canceled(_changed_files)) => {
            stop_parallelizable_workloads();
            log::info!(
                "Recheck successfully canceled. Restarting the recheck to include new file changes"
            );
            let _done: bool = shared_mem.collect_slice(256000);
            server_monitor_listener_state::requeue_workload(workload);
            recheck_single(
                recheck_count + 1,
                genv,
                env_for_cancel,
                shared_mem,
                node_modules_containers,
            )
        }
        Err(type_service::RecheckError::TooSlow) => {
            unreachable!("TooSlow is handled inside type_service::recheck");
        }
    };
    run_but_cancel_on_file_changes(
        options,
        shared_mem,
        &|| CheckedSet::empty(),
        priority,
        f,
        || {},
        || unreachable!("post_cancel is unreachable without async cancellation support"),
    )
}

// It's not obvious to Mr Gabe how we should merge together the profiling info from multiple
// rechecks. But something is better than nothing...
pub fn recheck_loop(
    genv: &server_env::Genv,
    env: server_env::Env,
    shared_mem: &SharedMem,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> (Vec<ProfilingFinished>, server_env::Env) {
    let mut profiling_list: Vec<ProfilingFinished> = Vec::new();
    let mut env = env;
    loop {
        let _should_print_summary = genv.options.profile;
        let recheck_result = recheck_single(1, genv, env, shared_mem, node_modules_containers);
        match recheck_result {
            RecheckOutcome::NothingToDo(e) => {
                return (profiling_list, e);
            }
            RecheckOutcome::CompletedRecheck {
                profiling: recheck_profiling,
                env: e,
                recheck_count,
            } => {
                let _ = recheck_count;
                profiling_list.push(recheck_profiling);
                env = e;
            }
        }
    }
}
