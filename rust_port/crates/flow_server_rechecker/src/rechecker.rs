/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
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

static TOKIO_RUNTIME: LazyLock<tokio::runtime::Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("failed to build flow_rechecker tokio runtime")
});

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
                    flow_hh_logger::info!("Running a parallel workload");
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
) -> ParallelizableWorkloadsStopper {
    let wait_for_cancel = Arc::new(AtomicBool::new(false));
    let env_for_loop = env.clone();
    let wait_for_cancel_for_loop = wait_for_cancel.dupe();
    let loop_task = TOKIO_RUNTIME.spawn_blocking(move || {
        parallelizable_workload_loop::run(&wait_for_cancel_for_loop, &env_for_loop);
    });
    ParallelizableWorkloadsStopper {
        wait_for_cancel,
        loop_task: Arc::new(Mutex::new(Some(loop_task))),
    }
}

#[derive(Clone)]
struct ParallelizableWorkloadsStopper {
    wait_for_cancel: Arc<AtomicBool>,
    loop_task: Arc<Mutex<Option<tokio::task::JoinHandle<()>>>>,
}

impl ParallelizableWorkloadsStopper {
    fn stop(&self) {
        if !self.wait_for_cancel.swap(true, Ordering::AcqRel) {
            server_monitor_listener_state::wake_workload_waiters();
        }

        let handle = self
            .loop_task
            .lock()
            .expect("parallelizable workload loop task lock should not be poisoned")
            .take();
        if let Some(handle) = handle {
            match TOKIO_RUNTIME.block_on(handle) {
                Ok(()) => {}
                Err(err) if err.is_panic() => std::panic::resume_unwind(err.into_panic()),
                Err(err) => panic!("parallelizable workload loop task failed: {}", err),
            }
        }
    }
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
            flow_hh_logger::info!("Libdef change detected: {}", msg);
            flow_hh_logger::info!("Will require full check reinit ({} updates)", updates.len());
            Updates::RequiredFullCheckReinit(updates)
        }
        Err(recheck_updates::Error::Unrecoverable { msg, exit_status }) => {
            flow_common_exit_status::exit_with_msg(exit_status, &msg);
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
        Some(server_monitor_listener_state::FindRefCommand { request, .. }) => request
            .downcast_ref::<find_refs_types::Request>()
            .expect("find ref request type should match rechecker")
            .clone(),
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
        &find_ref_request,
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
        let (response, metadata) = references_to_lsp_response(Box::new(find_ref_results));
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
// work is canceled, post_cancel is called and its result returned
fn run_but_cancel_on_file_changes<T, ProcessUpdates, GetForced, F, PreCancel, PostCancel>(
    _options: &Options,
    process_updates: ProcessUpdates,
    get_forced: GetForced,
    priority: Priority,
    f: F,
    pre_cancel: PreCancel,
    post_cancel: PostCancel,
) -> T
where
    ProcessUpdates: Fn(bool, &BTreeSet<String>) -> Updates + Send + 'static,
    GetForced: Fn() -> CheckedSet + Send + 'static,
    F: FnOnce() -> Result<T, type_service::RecheckError>,
    PreCancel: FnOnce() + Send + 'static,
    PostCancel: FnOnce() -> T,
{
    let (stop_tx, stop_rx) = crossbeam::channel::bounded::<()>(1);
    let cancel_task = TOKIO_RUNTIME.spawn_blocking(move || {
        match server_monitor_listener_state::wait_for_updates_for_recheck(
            &process_updates,
            &get_forced,
            priority,
            Some(&stop_rx),
        ) {
            Some(server_monitor_listener_state::WorkloadChanges {
                num_files_to_prioritize,
                num_files_to_force,
                num_files_to_recheck,
            }) => {
                flow_hh_logger::info!(
                    "Canceling recheck to prioritize {}, force {}, and recheck {} additional files",
                    num_files_to_prioritize,
                    num_files_to_force,
                    num_files_to_recheck,
                );
                flow_event_logger::recheck_canceled(
                    match priority {
                        Priority::Priority => "true",
                        Priority::Normal => "",
                    },
                    num_files_to_prioritize as i32,
                    num_files_to_recheck as i32,
                    num_files_to_force as i32,
                );
                pre_cancel();
                worker_cancel::stop_workers();
                true
            }
            None => false,
        }
    });

    let ret = f();
    match stop_tx.try_send(()) {
        Ok(()) => {}
        Err(crossbeam::channel::TrySendError::Full(())) => {}
        Err(crossbeam::channel::TrySendError::Disconnected(())) => {}
    }
    let cancel_fired = match TOKIO_RUNTIME.block_on(cancel_task) {
        Ok(cancel_fired) => cancel_fired,
        Err(err) if err.is_panic() => std::panic::resume_unwind(err.into_panic()),
        Err(err) => panic!("recheck cancel task failed: {}", err),
    };
    if cancel_fired && ret.is_ok() {
        worker_cancel::resume_workers();
    }
    match ret {
        Ok(ret) => ret,
        Err(type_service::RecheckError::Canceled(_changed_files)) => post_cancel(),
        Err(type_service::RecheckError::TooSlow) => {
            unreachable!("TooSlow is handled inside type_service::recheck");
        }
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
    let env_for_process_updates = env.clone();
    let process_updates = |skip_incompatible: bool, updates: &BTreeSet<String>| -> Updates {
        process_updates(
            skip_incompatible,
            options,
            &env_for_process_updates,
            shared_mem,
            updates,
        )
    };

    let will_be_checked_files = Arc::new(RwLock::new(env.checked_files.clone()));
    let get_forced = || {
        will_be_checked_files
            .read()
            .expect("will_be_checked_files lock should not be poisoned")
            .clone()
    };

    let (priority, workload) = server_monitor_listener_state::get_and_clear_recheck_workload(
        &process_updates,
        &get_forced,
    );
    let options_for_cancel = options.dupe();
    let shared_mem_for_cancel = genv.shared_mem.dupe();
    let env_for_cancel_updates = env_for_process_updates.clone();
    let process_updates_for_cancel =
        move |skip_incompatible: bool, updates: &BTreeSet<String>| -> Updates {
            crate::rechecker::process_updates(
                skip_incompatible,
                &options_for_cancel,
                &env_for_cancel_updates,
                &shared_mem_for_cancel,
                updates,
            )
        };
    let will_be_checked_files_for_cancel = will_be_checked_files.clone();
    let get_forced_for_cancel = move || {
        will_be_checked_files_for_cancel
            .read()
            .expect("will_be_checked_files lock should not be poisoned")
            .clone()
    };

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
    let stop_parallelizable_workloads_for_cancel = stop_parallelizable_workloads.clone();
    let stop_parallelizable_workloads_for_recheck = stop_parallelizable_workloads.clone();
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
    will_be_checked_files
        .write()
        .expect("will_be_checked_files lock should not be poisoned")
        .union(workload.files_to_force.dupe());

    let workload_cell = std::rc::Rc::new(std::cell::RefCell::new(Some(workload)));
    let workload_cell_for_recheck = workload_cell.clone();
    let will_be_checked_files_for_recheck = will_be_checked_files.clone();
    let f = move || {
        let mut workload = workload_cell_for_recheck
            .borrow_mut()
            .take()
            .expect("recheck workload should be available");
        let mut will_be_checked_files_for_recheck = will_be_checked_files_for_recheck
            .read()
            .expect("will_be_checked_files lock should not be poisoned")
            .clone();
        match recheck(
            genv,
            env,
            workload.files_to_force.dupe(),
            &mut workload.find_ref_command,
            incompatible_lib_change,
            workload.metadata.changed_mergebase,
            missed_changes,
            &mut will_be_checked_files_for_recheck,
            files_to_recheck_set,
            node_modules_containers,
        ) {
            Ok((profiling, env)) => {
                stop_parallelizable_workloads_for_recheck.stop();
                server_monitor_listener_state::requeue_deferred_parallelizable_workloads();

                Ok(RecheckOutcome::CompletedRecheck {
                    profiling,
                    env,
                    recheck_count,
                })
            }
            Err(type_service::RecheckError::Canceled(_changed_files)) => {
                stop_parallelizable_workloads_for_recheck.stop();
                *workload_cell_for_recheck.borrow_mut() = Some(workload);
                Err(type_service::RecheckError::Canceled(_changed_files))
            }
            Err(type_service::RecheckError::TooSlow) => Err(type_service::RecheckError::TooSlow),
        }
    };
    let workload_cell_for_cancel = workload_cell.clone();
    run_but_cancel_on_file_changes(
        options,
        process_updates_for_cancel,
        get_forced_for_cancel,
        priority,
        f,
        move || stop_parallelizable_workloads_for_cancel.stop(),
        move || {
            let workload = workload_cell_for_cancel
                .borrow_mut()
                .take()
                .expect("canceled recheck should leave workload available");
            flow_hh_logger::info!(
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
        },
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
        let should_print_summary = genv.options.profile;
        let recheck_series_start = Instant::now();
        let recheck_result = recheck_single(1, genv, env, shared_mem, node_modules_containers);
        let recheck_series_profiling = ProfilingFinished {
            duration: recheck_series_start.elapsed().as_secs_f64(),
        };
        if should_print_summary {
            flow_hh_logger::info!(
                "RecheckSeries: wall duration {:.3}s",
                recheck_series_profiling.get_profiling_duration()
            );
        }
        match recheck_result {
            RecheckOutcome::NothingToDo(e) => {
                return (profiling_list, e);
            }
            RecheckOutcome::CompletedRecheck {
                profiling: recheck_profiling,
                env: e,
                recheck_count,
            } => {
                flow_event_logger::recheck_series(
                    recheck_count as i32,
                    &serde_json::json!({
                        "duration": recheck_series_profiling.get_profiling_duration(),
                    }),
                );
                profiling_list.push(recheck_profiling);
                env = e;
            }
        }
    }
}
