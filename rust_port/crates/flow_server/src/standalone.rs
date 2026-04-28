/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::net::TcpListener;
use std::panic::AssertUnwindSafe;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::cli_output;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_server_command_handler::command_handler;
use flow_server_env::error_collator;
use flow_server_env::file_watcher_status;
use flow_server_env::monitor_prot::FileWatcherMetadata;
use flow_server_env::persistent_connection;
use flow_server_env::server_monitor_listener_state;
pub use flow_server_env::server_prot::response::LazyStats;
use flow_server_env::server_socket_rpc;
use flow_server_env::server_socket_rpc::SaveStateOut;
use flow_server_env::server_socket_rpc::ServerRequest;
use flow_server_env::server_socket_rpc::ServerResponse;
use flow_server_env::server_status;
use flow_server_files::server_files_js;
use flow_server_rechecker::rechecker;
use flow_services_get_def::get_def_types::DefInfo;
use flow_services_inference::type_service;
use flow_services_inference_types::CheckedDependenciesCanceled;
use flow_services_inference_types::TypeContentsError;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;
use flow_utils_concurrency::worker_cancel;

const SERVER_THREAD_STACK_SIZE: usize = 32 * 1024 * 1024;
const CONNECTION_THREAD_STACK_SIZE: usize = 2 * 1024 * 1024;
const MAX_CONNECTION_THREADS: usize = 128;
const INITIAL_CONNECTION_READ_TIMEOUT_SECS: u64 = 5;

pub fn start(
    options: Arc<Options>,
    flowconfig_name: String,
    shm_heap_size: Option<u64>,
    shm_hash_table_pow: Option<u32>,
) {
    crate::server::check_supported_operating_system(&options);
    flow_server_env::monitor_rpc::disable();
    let shared_mem = Arc::new(SharedMem::new_with_config(
        shm_heap_size,
        shm_hash_table_pow,
    ));
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));
    let tmp_dir = options.temp_dir.to_string();
    let server = FlowServer::new(options, shared_mem, pool, flowconfig_name, tmp_dir);
    server.run();
}

struct ServerState {
    env: Option<flow_server_env::server_env::Env>,
    env_generation: u64,
    init_done: bool,
    pending_recheck: bool,
    recheck_in_progress: bool,
    should_shutdown: bool,
}

fn current_persistent_status(
    server_state: &ServerState,
) -> (server_status::Status, file_watcher_status::Status) {
    let status = if !server_state.init_done {
        server_status::INITIAL_STATUS
    } else if server_state.recheck_in_progress || server_state.pending_recheck {
        server_status::Status::Typechecking(
            server_status::TypecheckMode::Rechecking,
            server_status::TypecheckStatus::StartingTypecheck,
        )
    } else {
        server_status::Status::Free
    };
    (
        status,
        (
            file_watcher_status::FileWatcher::NoFileWatcher,
            file_watcher_status::StatusKind::Ready,
        ),
    )
}

struct ConnectionSlots {
    active: AtomicUsize,
}

impl ConnectionSlots {
    fn new() -> Self {
        Self {
            active: AtomicUsize::new(0),
        }
    }

    fn try_acquire(self: &Arc<Self>) -> Option<ConnectionSlotGuard> {
        loop {
            let active = self.active.load(Ordering::SeqCst);
            if active >= MAX_CONNECTION_THREADS {
                return None;
            }
            if self
                .active
                .compare_exchange(active, active + 1, Ordering::SeqCst, Ordering::SeqCst)
                .is_ok()
            {
                return Some(ConnectionSlotGuard {
                    slots: self.clone(),
                });
            }
        }
    }
}

struct ConnectionSlotGuard {
    slots: Arc<ConnectionSlots>,
}

impl Drop for ConnectionSlotGuard {
    fn drop(&mut self) {
        self.slots.active.fetch_sub(1, Ordering::SeqCst);
    }
}

pub(crate) struct FlowServer {
    options: Arc<Options>,
    shared_mem: Arc<SharedMem>,
    pool: ThreadPool,
    flowconfig_name: String,
    tmp_dir: String,
}

impl FlowServer {
    pub(crate) fn new(
        options: Arc<Options>,
        shared_mem: Arc<SharedMem>,
        pool: ThreadPool,
        flowconfig_name: String,
        tmp_dir: String,
    ) -> Self {
        Self {
            options,
            shared_mem,
            pool,
            flowconfig_name,
            tmp_dir,
        }
    }

    pub(crate) fn run(&self) {
        let root = &*self.options.root;
        let lock_path = server_files_js::lock_file(&self.flowconfig_name, &self.tmp_dir, root);
        let socket_path = server_files_js::socket_file(&self.flowconfig_name, &self.tmp_dir, root);
        let pids_path = server_files_js::pids_file(&self.flowconfig_name, &self.tmp_dir, root);

        let _lock_file = match acquire_lock(&lock_path) {
            Ok(f) => f,
            Err(_) => {
                eprintln!("Error: another server is already running?");
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::LockStolen);
            }
        };

        let _lock_guard = LockGuard {
            lock_path: lock_path.clone(),
            socket_path: socket_path.clone(),
            pids_path: pids_path.clone(),
        };

        if let Some(parent) = Path::new(&pids_path).parent() {
            let _mkdir_result = std::fs::create_dir_all(parent);
        }
        std::fs::write(&pids_path, format!("{}\tmain\n", std::process::id())).unwrap_or_else(|e| {
            eprintln!("Error: failed to write pids file {}: {}", pids_path, e);
            cleanup_and_exit_with_code(&pids_path, &lock_path, &socket_path, 1);
        });

        let _remove_result = std::fs::remove_file(&socket_path);

        if let Some(parent) = Path::new(&socket_path).parent() {
            let _mkdir_result = std::fs::create_dir_all(parent);
        }

        let listener = match TcpListener::bind("127.0.0.1:0") {
            Ok(l) => l,
            Err(e) => {
                eprintln!("Error: failed to bind TCP listener: {}", e);
                cleanup_and_exit_with_code(&pids_path, &lock_path, &socket_path, 1);
            }
        };
        let port = listener.local_addr().unwrap().port();

        if let Some(parent) = Path::new(&socket_path).parent() {
            let _mkdir_result = std::fs::create_dir_all(parent);
        }
        std::fs::write(&socket_path, port.to_string()).unwrap_or_else(|e| {
            eprintln!("Error: failed to write socket file {}: {}", socket_path, e);
            cleanup_and_exit_with_code(&pids_path, &lock_path, &socket_path, 1);
        });

        let state = Arc::new((
            Mutex::new(ServerState {
                env: None,
                env_generation: 0,
                init_done: false,
                pending_recheck: false,
                recheck_in_progress: false,
                should_shutdown: false,
            }),
            Condvar::new(),
        ));

        let canceled_state = state.clone();
        crate::multi_worker::set_report_canceled_callback(move |total, finished| {
            log::info!("Canceling progress {}/{}", finished, total);
            let progress = server_status::Progress {
                total: Some(total),
                finished,
            };
            flow_server_env::monitor_rpc::status_update(server_status::Event::CancelingProgress(
                progress.clone(),
            ));
            let connections = {
                let (lock, _) = &*canceled_state;
                let server_state = lock.lock().unwrap();
                server_state.env.as_ref().map(|env| env.connections.clone())
            };
            if let Some(connections) = connections {
                persistent_connection::send_status(
                    server_status::Status::Typechecking(
                        server_status::TypecheckMode::Rechecking,
                        server_status::TypecheckStatus::Canceling(progress),
                    ),
                    (
                        file_watcher_status::FileWatcher::NoFileWatcher,
                        file_watcher_status::StatusKind::Ready,
                    ),
                    &connections,
                );
            }
        });

        let init_state = state.clone();
        let init_options = self.options.dupe();
        let init_shared_mem = self.shared_mem.dupe();
        let init_pool_workers = self.pool.num_workers();
        let init_lock_path = lock_path.clone();
        let init_socket_path = socket_path.clone();

        let node_modules_containers: Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>> =
            Arc::new(RwLock::new(BTreeMap::new()));
        let nmc = node_modules_containers.clone();
        let init_nmc_ref = nmc.clone();
        let init_pids_path = pids_path.clone();

        std::thread::Builder::new()
            .stack_size(SERVER_THREAD_STACK_SIZE)
            .spawn(move || {
                let init_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                    flow_server_env::monitor_rpc::status_update(server_status::Event::InitStart);
                    eprintln!("Initializing server...");
                    let init_start = std::time::Instant::now();
                    let pool = ThreadPool::with_thread_count(
                        flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
                            std::num::NonZeroUsize::new(init_pool_workers)
                                .expect("pool_workers should be positive"),
                        ),
                    );
                    let (mut env, init_nmc, _first_internal_error) =
                        type_service::init(&init_options, &pool, &init_shared_mem);
                    env = server_monitor_listener_state::update_env(env);
                    let init_duration = init_start.elapsed().as_secs_f64();
                    let finishing_up_status = server_status::Status::Typechecking(
                        server_status::TypecheckMode::Initializing,
                        server_status::TypecheckStatus::FinishingTypecheck,
                    );
                    env.connections = persistent_connection::all_clients();
                    persistent_connection::send_status(
                        finishing_up_status,
                        (
                            file_watcher_status::FileWatcher::NoFileWatcher,
                            file_watcher_status::StatusKind::Ready,
                        ),
                        &env.connections,
                    );
                    persistent_connection::send_telemetry(
                        flow_server_env::lsp_prot::TelemetryFromServer::InitSummary {
                            duration: init_duration,
                        },
                        &env.connections,
                    );
                    flow_server_env::monitor_rpc::send_telemetry(
                        flow_server_env::lsp_prot::TelemetryFromServer::InitSummary {
                            duration: init_duration,
                        },
                    );
                    flow_server_env::monitor_rpc::status_update(server_status::Event::FinishingUp);

                    {
                        let mut nmc_guard = nmc.write().unwrap();
                        *nmc_guard = init_nmc.read().unwrap().clone();
                    }

                    {
                        let (lock, cvar) = &*init_state;
                        let mut server_state = lock.lock().unwrap();
                        env.connections = persistent_connection::all_clients();
                        server_state.env = Some(env);
                        server_state.env_generation += 1;
                        server_state.init_done = true;
                        let (status, watcher_status) = current_persistent_status(&server_state);
                        cvar.notify_all();
                        persistent_connection::send_status(
                            status,
                            watcher_status,
                            &persistent_connection::all_clients(),
                        );
                    }
                    flow_server_env::monitor_rpc::status_update(server_status::Event::Ready);

                    eprintln!("Server is ready (port {})", port);

                    process_pending_rechecks(
                        &init_state,
                        &init_options,
                        &init_shared_mem,
                        init_pool_workers,
                        &init_nmc_ref,
                        &init_pids_path,
                        &init_lock_path,
                        &init_socket_path,
                    );
                }));
                if let Err(payload) = init_result {
                    let panic_message = payload
                        .downcast_ref::<&str>()
                        .copied()
                        .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
                        .unwrap_or("unknown panic payload");
                    eprintln!("Error: server initialization panicked: {}", panic_message);
                    cleanup_and_exit_with_code(
                        &init_pids_path,
                        &init_lock_path,
                        &init_socket_path,
                        1,
                    );
                }
            })
            .expect("failed to spawn init thread");

        let workload_state = state.clone();
        let workload_options = self.options.dupe();
        let workload_shared_mem = self.shared_mem.dupe();
        let workload_nmc = node_modules_containers.clone();
        std::thread::Builder::new()
            .stack_size(SERVER_THREAD_STACK_SIZE)
            .spawn(move || {
                process_persistent_workloads(
                    &workload_state,
                    &workload_options,
                    &workload_shared_mem,
                    &workload_nmc,
                );
            })
            .expect("failed to spawn persistent workload thread");

        let connection_slots = Arc::new(ConnectionSlots::new());

        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let Some(slot_guard) = connection_slots.try_acquire() else {
                        eprintln!("Refusing connection: too many concurrent clients");
                        drop(stream);
                        continue;
                    };
                    let state = state.clone();
                    let options = self.options.dupe();
                    let shared_mem = self.shared_mem.dupe();
                    let pool_workers = self.pool.num_workers();
                    let pids_path = pids_path.clone();
                    let lock_path = lock_path.clone();
                    let socket_path = socket_path.clone();
                    let nmc = node_modules_containers.clone();
                    std::thread::Builder::new()
                        .stack_size(CONNECTION_THREAD_STACK_SIZE)
                        .spawn(move || {
                            let _slot_guard = slot_guard;
                            handle_connection(
                                &state,
                                &options,
                                &shared_mem,
                                pool_workers,
                                stream,
                                &pids_path,
                                &lock_path,
                                &socket_path,
                                &nmc,
                            );
                        })
                        .expect("failed to spawn connection thread");
                }
                Err(e) => {
                    eprintln!("Error accepting connection: {}", e);
                }
            }
        }
    }
}

enum RecheckOutcome {
    Ok,
}

fn process_persistent_workloads(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    _options: &Arc<Options>,
    _shared_mem: &Arc<SharedMem>,
    _node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> ! {
    loop {
        server_monitor_listener_state::wait_for_workload();
        let Some(workload) = server_monitor_listener_state::pop_next_workload() else {
            continue;
        };

        let env = {
            let (lock, cvar) = &**state;
            let server_state = lock.lock().unwrap();
            let mut server_state = cvar
                .wait_while(server_state, |s| {
                    !s.init_done
                        || s.env.is_none()
                        || s.pending_recheck
                        || s.recheck_in_progress
                        || s.should_shutdown
                })
                .unwrap();
            if server_state.should_shutdown {
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::KilledByMonitor,
                );
            }
            server_state.env.take().unwrap()
        };

        let env = workload(env);
        let mut env = server_monitor_listener_state::update_env(env);
        let (lock, cvar) = &**state;
        let mut server_state = lock.lock().unwrap();
        env.connections = persistent_connection::all_clients();
        server_state.env = Some(env);
        server_state.env_generation += 1;
        cvar.notify_all();
    }
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

/// Spawn the recheck cancel monitor. Subscribes to the recheck push channel
/// and waits for either:
///   1. A new force-recheck or file-watcher update (push to recheck stream)
///      → call `worker_cancel::stop_workers()` so the in-progress recheck
///      observes cancel and unwinds back to `do_rechecks`.
///   2. The recheck completing on its own (do_rechecks calls
///      `RecheckCancelMonitor::stop`) → exit without signaling cancel.
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

fn do_rechecks(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> RecheckOutcome {
    let pool = ThreadPool::with_thread_count(
        flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
            std::num::NonZeroUsize::new(pool_workers).expect("pool_workers should be positive"),
        ),
    );
    let mut env = {
        let (lock, _) = &**state;
        let server_state = lock.lock().unwrap();
        server_state.env.clone().unwrap()
    };

    loop {
        env = server_monitor_listener_state::update_env(env);
        let process_updates = |skip_incompatible: bool,
                               updates: &BTreeSet<String>|
         -> server_monitor_listener_state::Updates {
            rechecker::process_updates(skip_incompatible, options, &env, shared_mem, updates)
        };
        let mut will_be_checked_files = env.checked_files.dupe();
        let (_priority, workload) = server_monitor_listener_state::get_and_clear_recheck_workload(
            &process_updates,
            &|| will_be_checked_files.clone(),
        );
        let server_monitor_listener_state::RecheckWorkload {
            metadata:
                FileWatcherMetadata {
                    changed_mergebase,
                    missed_changes,
                },
            files_to_recheck,
            files_to_prioritize,
            files_to_force,
            find_ref_command,
            incompatible_lib_change,
        } = workload;
        let did_change_mergebase = changed_mergebase.unwrap_or(false);
        let workload = server_monitor_listener_state::RecheckWorkload {
            files_to_prioritize: files_to_prioritize.dupe(),
            files_to_recheck: files_to_recheck.dupe(),
            files_to_force: files_to_force.dupe(),
            find_ref_command,
            metadata: FileWatcherMetadata {
                changed_mergebase,
                missed_changes,
            },
            incompatible_lib_change,
        };
        let mut updates = CheckedSet::empty();
        updates.add(Some(files_to_recheck), None, Some(files_to_prioritize));

        if missed_changes && !did_change_mergebase {
            updates.add(Some(env.checked_files.focused().dupe()), None, None);
        }

        if !incompatible_lib_change
            && !did_change_mergebase
            && updates.is_empty()
            && files_to_force.is_empty()
        {
            env = server_monitor_listener_state::update_env(env);
            let (lock, cvar) = &**state;
            let mut server_state = lock.lock().unwrap();
            env.connections = persistent_connection::all_clients();
            server_state.env = Some(env);
            server_state.env_generation += 1;
            cvar.notify_all();
            return RecheckOutcome::Ok;
        }

        let def_info = DefInfo::NoDefinition(None);

        worker_cancel::resume_workers();
        will_be_checked_files.union(files_to_force.dupe());

        let old_env = env.clone();
        let recheck_start = std::time::Instant::now();
        flow_server_env::monitor_rpc::status_update(server_status::Event::RecheckStart);
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

        let cancel_monitor = spawn_recheck_cancel_monitor();

        let recheck_result = type_service::recheck(
            &pool,
            shared_mem,
            options,
            &updates,
            &def_info,
            files_to_force,
            incompatible_lib_change,
            changed_mergebase,
            missed_changes,
            node_modules_containers,
            &mut will_be_checked_files,
            env,
        );

        // Stop the monitor regardless of the recheck outcome; it is a no-op
        // if the monitor already fired and exited on its own.
        cancel_monitor.stop();

        match recheck_result {
            Ok((log_recheck_event, recheck_stats, _find_ref_results, new_env)) => {
                env = new_env;
                let lazy_stats = rechecker::get_lazy_stats(options, &env);
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
                    flow_server_env::lsp_prot::ErrorsReason::EndOfRecheck,
                    || {
                        let (errors, warnings) = error_collator::get_with_separate_warnings(&env);
                        (errors, warnings.clone())
                    },
                );
                flow_server_env::monitor_rpc::status_update(server_status::Event::FinishingUp);
                log_recheck_event();
                let lsp_stats = flow_server_env::lsp_prot::RecheckStats {
                    dependent_file_count: recheck_stats.dependent_file_count as i32,
                    changed_file_count: recheck_stats.changed_file_count as i32,
                    top_cycle: recheck_stats.top_cycle.map(|(f, s)| (f, s as i32)),
                };
                flow_server_env::monitor_rpc::send_telemetry(
                    flow_server_env::lsp_prot::TelemetryFromServer::RecheckSummary {
                        duration: recheck_start.elapsed().as_secs_f64(),
                        stats: lsp_stats,
                    },
                );
                env = server_monitor_listener_state::update_env(env);
                let (lock, cvar) = &**state;
                let mut server_state = lock.lock().unwrap();
                env.connections = persistent_connection::all_clients();
                server_state.env = Some(env.clone());
                server_state.env_generation += 1;
                cvar.notify_all();
                drop(server_state);
                flow_server_env::monitor_rpc::status_update(server_status::Event::Ready);
            }
            Err(type_service::RecheckError::TooSlow) => {
                unreachable!("TooSlow is handled inside type_service::recheck");
            }
            Err(type_service::RecheckError::Canceled(changed_files)) => {
                eprintln!(
                    "Recheck successfully canceled. Restarting the recheck to include new file changes"
                );
                let _done: bool = shared_mem.collect_slice(256000);
                server_monitor_listener_state::requeue_workload(workload);
                if !changed_files.is_empty()
                    && changed_files
                        .iter()
                        .all(|file| Path::new(&file.to_absolute()).exists())
                {
                    let updates = {
                        let mut updates = CheckedSet::empty();
                        updates.add(None, None, Some(changed_files.into_iter().collect()));
                        updates
                    };
                    env = type_service::parse_and_update_dependency_info(
                        &pool,
                        shared_mem,
                        options,
                        &updates,
                        &def_info,
                        CheckedSet::empty(),
                        node_modules_containers,
                        old_env.clone(),
                    )
                    .unwrap_or(old_env);
                    env = server_monitor_listener_state::update_env(env);
                    let (lock, cvar) = &**state;
                    let mut server_state = lock.lock().unwrap();
                    env.connections = persistent_connection::all_clients();
                    server_state.env = Some(env.clone());
                    server_state.env_generation += 1;
                    cvar.notify_all();
                } else {
                    env = old_env;
                }
            }
        }
    }
}

fn process_pending_rechecks(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    pids_path: &str,
    lock_path: &str,
    socket_path: &str,
) {
    loop {
        {
            let (lock, cvar) = &**state;
            let mut server_state = lock.lock().unwrap();

            if server_state.should_shutdown {
                cleanup_and_exit(pids_path, lock_path, socket_path);
            }

            if !server_state.pending_recheck {
                server_state.recheck_in_progress = false;
                cvar.notify_all();
                server_state = cvar
                    .wait_while(server_state, |s| !s.pending_recheck && !s.should_shutdown)
                    .unwrap();

                if server_state.should_shutdown {
                    cleanup_and_exit(pids_path, lock_path, socket_path);
                }
            }

            server_state.pending_recheck = false;
            server_state.recheck_in_progress = true;
        }

        let outcome = {
            let (lock, _) = &**state;
            let server_state = lock.lock().unwrap();
            if server_state.env.is_some() {
                drop(server_state);
                do_rechecks(
                    state,
                    options,
                    shared_mem,
                    pool_workers,
                    node_modules_containers,
                )
            } else {
                RecheckOutcome::Ok
            }
        };

        match outcome {
            RecheckOutcome::Ok => {}
        }
    }
}

fn acquire_lock(lock_path: &str) -> std::io::Result<std::fs::File> {
    if let Some(parent) = Path::new(lock_path).parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_path)?;
    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(std::fs::TryLockError::WouldBlock) => Err(std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            "lock file is already held",
        )),
        Err(err) => Err(err.into()),
    }
}

struct LockGuard {
    lock_path: String,
    socket_path: String,
    pids_path: String,
}

impl Drop for LockGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.pids_path);
        let _ = std::fs::remove_file(&self.socket_path);
        let _ = std::fs::remove_file(&self.lock_path);
    }
}

fn cleanup_and_exit(pids_path: &str, lock_path: &str, socket_path: &str) -> ! {
    cleanup_and_exit_with_code(pids_path, lock_path, socket_path, 0)
}

fn cleanup_and_exit_with_code(
    pids_path: &str,
    lock_path: &str,
    socket_path: &str,
    exit_code: i32,
) -> ! {
    let _ = std::fs::remove_file(pids_path);
    let _ = std::fs::remove_file(socket_path);
    let _ = std::fs::remove_file(lock_path);
    std::process::exit(exit_code);
}

fn request_dependency_recheck(state: &Arc<(Mutex<ServerState>, Condvar)>) {
    let (lock, cvar) = &**state;
    let mut server_state = lock.lock().unwrap();
    if server_state.recheck_in_progress {
        worker_cancel::stop_workers();
    }
    server_state.pending_recheck = true;
    cvar.notify_all();
}

fn remove_persistent_client(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    client_id: flow_server_env::lsp_prot::ClientId,
) {
    persistent_connection::remove_client(client_id);
    server_monitor_listener_state::push_new_env_update(Box::new(move |mut env| {
        env.connections =
            persistent_connection::remove_client_from_clients(env.connections.clone(), client_id);
        env
    }));
    let (lock, cvar) = &**state;
    let mut server_state = lock.lock().unwrap();
    if let Some(env) = server_state.env.as_mut() {
        env.connections =
            persistent_connection::remove_client_from_clients(env.connections.clone(), client_id);
        server_state.env_generation += 1;
        cvar.notify_all();
    }
}

fn handle_persistent_request(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    expected_client_id: flow_server_env::lsp_prot::ClientId,
    request: ServerRequest,
) -> (ServerResponse, bool) {
    match request {
        ServerRequest::PersistentRequest { client_id, request } => {
            if client_id != expected_client_id {
                return (
                    ServerResponse::Error {
                        message: format!(
                            "Unexpected persistent client id {} (expected {})",
                            client_id, expected_client_id
                        ),
                    },
                    false,
                );
            }
            if !persistent_connection::has_client(client_id) {
                return (
                    ServerResponse::Error {
                        message: format!("Unknown persistent client {}", client_id),
                    },
                    true,
                );
            }
            let genv = Arc::new(flow_server_env::server_env::Genv {
                options: Arc::new(options.clone()),
                workers: None,
                shared_mem: shared_mem.dupe(),
                node_modules_containers: Arc::new(node_modules_containers.read().unwrap().clone()),
            });
            command_handler::enqueue_persistent(&genv, client_id, request);
            (ServerResponse::PersistentAck, false)
        }
        ServerRequest::PersistentPoll { client_id } => {
            if client_id != expected_client_id {
                return (
                    ServerResponse::Error {
                        message: format!(
                            "Unexpected persistent client id {} (expected {})",
                            client_id, expected_client_id
                        ),
                    },
                    false,
                );
            }
            match persistent_connection::pop_message(client_id) {
                Ok(message) => (ServerResponse::PersistentPoll { message }, false),
                Err(message) => (ServerResponse::Error { message }, false),
            }
        }
        ServerRequest::PersistentDisconnect { client_id } => {
            if client_id != expected_client_id {
                return (
                    ServerResponse::Error {
                        message: format!(
                            "Unexpected persistent client id {} (expected {})",
                            client_id, expected_client_id
                        ),
                    },
                    false,
                );
            }
            remove_persistent_client(state, client_id);
            (ServerResponse::PersistentAck, true)
        }
        _ => (
            ServerResponse::Error {
                message: "Unexpected request on persistent connection".to_string(),
            },
            false,
        ),
    }
}

fn handle_connection(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    _pool_workers: usize,
    mut stream: std::net::TcpStream,
    pids_path: &str,
    lock_path: &str,
    socket_path: &str,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) {
    let _ = stream.set_read_timeout(Some(std::time::Duration::from_secs(
        INITIAL_CONNECTION_READ_TIMEOUT_SECS,
    )));
    let request: ServerRequest = match server_socket_rpc::receive_message(&mut stream) {
        Ok(req) => req,
        Err(e) => {
            eprintln!("Error reading request: {}", e);
            return;
        }
    };
    let _ = stream.set_read_timeout(None);

    if let ServerRequest::PersistentConnect {
        client_id,
        lsp_initialize_params,
    } = &request
    {
        let client_id = *client_id;
        let (lock, cvar) = &**state;
        let mut server_state = lock.lock().unwrap();
        if !server_state.init_done {
            let (server_status, watcher_status) = current_persistent_status(&server_state);
            drop(server_state);
            let _ = server_socket_rpc::send_message(
                &mut stream,
                &ServerResponse::PersistentBusy {
                    server_status,
                    watcher_status,
                },
            );
            return;
        }
        if !persistent_connection::add_client(client_id, lsp_initialize_params.clone()) {
            let _ = server_socket_rpc::send_message(
                &mut stream,
                &ServerResponse::Error {
                    message: format!("Persistent client {} is already connected", client_id),
                },
            );
            return;
        }
        server_monitor_listener_state::push_new_env_update(Box::new(move |mut env| {
            env.connections =
                persistent_connection::add_client_to_clients(env.connections.clone(), client_id);
            env
        }));
        if let Some(env) = server_state.env.as_mut() {
            env.connections =
                persistent_connection::add_client_to_clients(env.connections.clone(), client_id);
            server_state.env_generation += 1;
            cvar.notify_all();
        }
        let (status, watcher_status) = current_persistent_status(&server_state);
        drop(server_state);

        if let Err(e) =
            server_socket_rpc::send_message(&mut stream, &ServerResponse::PersistentConnected)
        {
            eprintln!("Error sending response: {}", e);
            remove_persistent_client(state, client_id);
            return;
        }
        persistent_connection::send_status(
            status,
            watcher_status,
            &persistent_connection::PersistentConnection(vec![client_id]),
        );

        loop {
            let request = match server_socket_rpc::receive_message(&mut stream) {
                Ok(req) => req,
                Err(e) => {
                    if !matches!(
                        e.kind(),
                        std::io::ErrorKind::UnexpectedEof
                            | std::io::ErrorKind::ConnectionReset
                            | std::io::ErrorKind::BrokenPipe
                            | std::io::ErrorKind::ConnectionAborted
                    ) {
                        eprintln!("Error reading request: {}", e);
                    }
                    remove_persistent_client(state, client_id);
                    return;
                }
            };
            let (response, should_close) = handle_persistent_request(
                state,
                options,
                shared_mem,
                node_modules_containers,
                client_id,
                request,
            );
            if let Err(e) = server_socket_rpc::send_message(&mut stream, &response) {
                eprintln!("Error sending response: {}", e);
                remove_persistent_client(state, client_id);
                return;
            }
            if should_close {
                return;
            }
        }
    }

    let response = match request {
        ServerRequest::CliEphemeralCommand { command } => {
            let genv = flow_server_env::server_env::Genv {
                options: Arc::new(options.clone()),
                workers: None,
                shared_mem: shared_mem.dupe(),
                node_modules_containers: Arc::new(node_modules_containers.read().unwrap().clone()),
            };
            let command = command.into_server_command();
            loop {
                let (lock, cvar) = &**state;
                let server_state = lock.lock().unwrap();
                let wait_for_recheck = matches!(
                    command_handler::classify_ephemeral_command(&genv, &command),
                    command_handler::CommandHandler::HandleNonparallelizable
                );
                let server_state = cvar
                    .wait_while(server_state, |s| {
                        !s.init_done
                            || s.env.is_none()
                            || (wait_for_recheck && (s.recheck_in_progress || s.pending_recheck))
                    })
                    .unwrap();

                if let Some(ref env) = server_state.env {
                    let result = command_handler::handle_ephemeral_command_for_standalone_wrapped(
                        &genv,
                        env,
                        command.clone(),
                    );
                    let response = match result {
                        Err(command_handler::WorkloadCanceled) => {
                            let current_generation = server_state.env_generation;
                            drop(server_state);
                            request_dependency_recheck(state);
                            let server_state = lock.lock().unwrap();
                            let _server_state = cvar
                                .wait_while(server_state, |s| {
                                    !s.init_done
                                        || s.env.is_none()
                                        || s.env_generation == current_generation
                                })
                                .unwrap();
                            continue;
                        }
                        Ok((response, _json_data)) => response,
                    };
                    if command_handler::standalone_response_needs_checked_dependencies_retry(
                        &command, &response,
                    ) {
                        let current_generation = server_state.env_generation;
                        drop(server_state);
                        request_dependency_recheck(state);
                        let server_state = lock.lock().unwrap();
                        let _server_state = cvar
                            .wait_while(server_state, |s| {
                                !s.init_done
                                    || s.env.is_none()
                                    || s.env_generation == current_generation
                            })
                            .unwrap();
                        continue;
                    }
                    break match server_socket_rpc::CliResponse::try_from_server_response(response) {
                        Ok(response) => ServerResponse::CliEphemeralResponse { response },
                        Err(message) => ServerResponse::Error { message },
                    };
                } else {
                    break ServerResponse::Error {
                        message: "Server not initialized".to_string(),
                    };
                }
            }
        }
        ServerRequest::PersistentConnect { .. }
        | ServerRequest::PersistentRequest { .. }
        | ServerRequest::PersistentPoll { .. }
        | ServerRequest::PersistentDisconnect { .. } => ServerResponse::Error {
            message: "Persistent requests must use a persistent connection".to_string(),
        },
        ServerRequest::Status {
            error_flags,
            from,
            strip_root,
            json,
            pretty,
            json_version,
            offset_kind,
        } => {
            let (lock, cvar) = &**state;
            let server_state = lock.lock().unwrap();
            let server_state = cvar
                .wait_while(server_state, |s| {
                    !s.init_done || s.recheck_in_progress || s.pending_recheck
                })
                .unwrap();

            if let Some(ref env) = server_state.env {
                handle_status(
                    env,
                    options,
                    shared_mem,
                    error_flags.clone().into(),
                    from,
                    strip_root,
                    json,
                    pretty,
                    json_version,
                    offset_kind,
                    options.lazy_mode,
                )
            } else {
                ServerResponse::Error {
                    message: "Server not initialized".to_string(),
                }
            }
        }
        ServerRequest::ForceRecheck {
            focus,
            files,
            missed_changes,
            changed_mergebase,
        } => {
            {
                let (lock, cvar) = &**state;
                let mut server_state = lock.lock().unwrap();
                let fileset: BTreeSet<String> = files
                    .into_iter()
                    .map(|file| {
                        if Path::new(&file).is_absolute() {
                            file
                        } else {
                            options.root.join(file).to_string_lossy().into_owned()
                        }
                    })
                    .collect();

                if server_state.recheck_in_progress {
                    worker_cancel::stop_workers();
                }

                if focus {
                    server_monitor_listener_state::push_files_to_force_focused_and_recheck(fileset);
                } else {
                    server_monitor_listener_state::push_files_to_recheck_with_metadata(
                        Some(FileWatcherMetadata {
                            missed_changes,
                            changed_mergebase: Some(changed_mergebase),
                        }),
                        fileset,
                    );
                }
                server_state.pending_recheck = true;
                cvar.notify_all();
            }

            if let Err(e) =
                server_socket_rpc::send_message(&mut stream, &ServerResponse::ForceRecheck)
            {
                eprintln!("Error sending response: {}", e);
            }
            return;
        }
        ServerRequest::SaveState { out, from: _from } => {
            let (lock, cvar) = &**state;
            let server_state = lock.lock().unwrap();
            let server_state = cvar
                .wait_while(server_state, |s| {
                    !s.init_done || s.recheck_in_progress || s.pending_recheck
                })
                .unwrap();

            if let Some(ref env) = server_state.env {
                handle_save_state(env, options, shared_mem, node_modules_containers, out)
            } else {
                ServerResponse::Error {
                    message: "Server not initialized".to_string(),
                }
            }
        }
        ServerRequest::CheckContents {
            input,
            verbose,
            force,
            error_flags,
            wait_for_recheck,
            strip_root,
            json,
            pretty,
            json_version,
            offset_kind,
        } => loop {
            let (lock, cvar) = &**state;
            let server_state = lock.lock().unwrap();
            let server_state = cvar
                .wait_while(server_state, |s| {
                    !s.init_done
                        || s.env.is_none()
                        || (wait_for_recheck.unwrap_or(options.wait_for_recheck)
                            && (s.recheck_in_progress || s.pending_recheck))
                })
                .unwrap();

            if let Some(ref env) = server_state.env {
                match handle_check_contents(
                    env,
                    options,
                    shared_mem,
                    node_modules_containers,
                    input.clone(),
                    verbose.clone().map(flow_common::verbose::Verbose::from),
                    force,
                    error_flags.clone().into(),
                    strip_root,
                    json,
                    pretty,
                    json_version,
                    offset_kind,
                ) {
                    Ok(response) => break response,
                    Err(CheckedDependenciesCanceled) => {
                        let current_generation = server_state.env_generation;
                        drop(server_state);
                        request_dependency_recheck(state);
                        let server_state = lock.lock().unwrap();
                        let _server_state = cvar
                            .wait_while(server_state, |s| {
                                !s.init_done
                                    || s.env.is_none()
                                    || s.env_generation == current_generation
                            })
                            .unwrap();
                        continue;
                    }
                }
            } else {
                break ServerResponse::Error {
                    message: "Server not initialized".to_string(),
                };
            }
        },
        ServerRequest::Shutdown => {
            let response = ServerResponse::ShutdownAck;
            let _ = server_socket_rpc::send_message(&mut stream, &response);
            eprintln!("Shutdown requested, exiting...");

            {
                let (lock, cvar) = &**state;
                let mut server_state = lock.lock().unwrap();
                server_state.should_shutdown = true;
                worker_cancel::stop_workers();
                if let Some(env) = server_state.env.as_ref() {
                    persistent_connection::send_server_exit(
                        flow_common_exit_status::FlowExitStatus::KilledByMonitor,
                        &env.connections,
                    );
                }
                cvar.notify_all();
            }

            cleanup_and_exit(pids_path, lock_path, socket_path);
        }
    };

    if let Err(e) = server_socket_rpc::send_message(&mut stream, &response) {
        eprintln!("Error sending response: {}", e);
    }
}

fn handle_status(
    env: &flow_server_env::server_env::Env,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    error_flags: cli_output::ErrorFlags,
    from: Option<String>,
    client_strip_root: bool,
    json: bool,
    pretty: bool,
    json_version: Option<flow_common_errors::error_utils::json_output::JsonVersion>,
    offset_kind: flow_parser::offset_utils::OffsetKind,
    lazy_mode: bool,
) -> ServerResponse {
    let (errors, warnings, suppressed_errors) = if options.include_suppressions {
        error_collator::get(env)
    } else {
        let (errors, warnings) = error_collator::get_without_suppressed(env);
        (errors, warnings, vec![])
    };

    let mut buf = Vec::new();
    let strip_root = if client_strip_root || options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };
    let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
    let get_ast = |file: &flow_parser::file_key::FileKey| shared_mem.get_ast(file);
    let suppressed_errors: Vec<_> = suppressed_errors
        .into_iter()
        .map(|(e, loc_set)| {
            (
                flow_typing_errors::intermediate_error::to_printable_error(
                    &loc_of_aloc,
                    get_ast,
                    strip_root,
                    e,
                ),
                loc_set,
            )
        })
        .collect();
    let error_count = errors.cardinal() + suppressed_errors.len();
    let include_warnings = error_flags.include_warnings || options.include_warnings;
    let warning_count = if include_warnings {
        warnings.cardinal()
    } else {
        0
    };
    let has_errors = error_count > 0;
    let actual_warnings = if include_warnings {
        &warnings
    } else {
        &ConcreteLocPrintableErrorSet::empty()
    };
    let strip_root_str = strip_root.map(|root| root.to_string_lossy().into_owned());
    if json {
        flow_common_errors::error_utils::json_output::print_errors_with_offset_kind(
            &mut buf,
            strip_root_str.as_deref(),
            &suppressed_errors,
            pretty,
            json_version
                .unwrap_or(flow_common_errors::error_utils::json_output::JsonVersion::JsonV1),
            &None,
            offset_kind,
            &errors,
            actual_warnings,
        )
        .expect("failed to write errors to buffer");
    } else {
        let strip_root_str = strip_root.map(|root| root.to_string_lossy().into_owned());
        if from.as_deref() == Some("vim") || from.as_deref() == Some("emacs") {
            flow_common_errors::error_utils::vim_emacs_output::print_errors(
                strip_root_str.as_deref(),
                &mut buf,
                &errors,
                actual_warnings,
            )
            .expect("failed to write errors to buffer");
        } else {
            let mut errors_with_suppressed = ConcreteLocPrintableErrorSet::empty();
            errors_with_suppressed.union(&errors);
            for (error, _) in &suppressed_errors {
                errors_with_suppressed.add(error.clone());
            }
            cli_output::print_errors(
                &mut buf,
                &error_flags,
                &None,
                strip_root,
                &errors_with_suppressed,
                actual_warnings,
                None,
            )
            .expect("failed to write errors to buffer");
        }
    }

    let error_output = String::from_utf8_lossy(&buf).into_owned();

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

    ServerResponse::Status {
        has_errors,
        error_count,
        warning_count,
        error_output,
        lazy_stats: LazyStats {
            lazy_mode,
            checked_files,
            checked_libdef_files,
            total_files,
            total_libdef_files,
        },
    }
}

fn handle_save_state(
    env: &flow_server_env::server_env::Env,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    out: SaveStateOut,
) -> ServerResponse {
    let path = match out {
        SaveStateOut::File(path) => PathBuf::from(flow_common::files::imaginary_realpath(&path)),
        SaveStateOut::Scm => match flow_saved_state::output_filename(options) {
            Ok(path) => path,
            Err(err) => {
                return ServerResponse::SaveState {
                    result: Err(format!(
                        "Failed to determine saved-state output file: {}",
                        err
                    )),
                };
            }
        },
    };
    if let Some(parent) = path.parent() {
        if let Err(err) = std::fs::create_dir_all(parent) {
            return ServerResponse::SaveState {
                result: Err(format!(
                    "Failed to create saved-state file `{}`:\n{}",
                    path.display(),
                    err
                )),
            };
        }
    }
    let node_modules_containers = node_modules_containers.read().unwrap();
    let result =
        match flow_saved_state::save(&path, shared_mem, env, options, &node_modules_containers) {
            Ok(()) => Ok(format!("Created saved-state file `{}`", path.display())),
            Err(reason) => Err(format!(
                "Failed to create saved-state file `{}`:\n{}",
                path.display(),
                reason
            )),
        };
    ServerResponse::SaveState { result }
}

fn handle_check_contents(
    env: &flow_server_env::server_env::Env,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    input: server_socket_rpc::FileInput,
    verbose: Option<flow_common::verbose::Verbose>,
    force: bool,
    error_flags: cli_output::ErrorFlags,
    strip_root: bool,
    json: bool,
    pretty: bool,
    json_version: Option<flow_common_errors::error_utils::json_output::JsonVersion>,
    offset_kind: flow_parser::offset_utils::OffsetKind,
) -> Result<ServerResponse, CheckedDependenciesCanceled> {
    let mut options = options.clone();
    options.all = options.all || force;
    options.verbose = verbose.map(Arc::new);
    let wire_input = input.clone();
    let input = input.into_server_file_input();
    let content = input
        .content_of_file_input()
        .expect("check-contents input should be readable");
    let file_key = match &wire_input {
        server_socket_rpc::FileInput::FileName(path)
        | server_socket_rpc::FileInput::FileContent(Some(path), _) => {
            FileKey::source_file_of_absolute(path)
        }
        server_socket_rpc::FileInput::FileContent(None, _) => FileKey::source_file_of_absolute("-"),
    };
    let intermediate_result =
        flow_services_inference::type_contents::parse_contents(&options, &content, &file_key);
    if intermediate_result.0.is_none() && intermediate_result.1.is_empty() {
        let error_output = if json {
            let mut buf = Vec::new();
            flow_common_errors::error_utils::json_output::print_errors_with_offset_kind(
                &mut buf,
                None,
                &[],
                pretty,
                json_version
                    .unwrap_or(flow_common_errors::error_utils::json_output::JsonVersion::JsonV1),
                &match &wire_input {
                    server_socket_rpc::FileInput::FileContent(None, content) => {
                        Some((PathBuf::from("-"), content.clone()))
                    }
                    server_socket_rpc::FileInput::FileContent(Some(path), content) => {
                        Some((PathBuf::from(path), content.clone()))
                    }
                    server_socket_rpc::FileInput::FileName(_) => None,
                },
                offset_kind,
                &ConcreteLocPrintableErrorSet::empty(),
                &ConcreteLocPrintableErrorSet::empty(),
            )
            .expect("failed to write errors to buffer");
            String::from_utf8_lossy(&buf).into_owned()
        } else {
            String::new()
        };
        return Ok(ServerResponse::CheckContents {
            has_errors: false,
            warning_count: 0,
            error_output,
            not_covered: true,
        });
    }
    let result = if !intermediate_result.1.is_empty() {
        Err(TypeContentsError::Errors(intermediate_result.1))
    } else {
        let node_modules_containers = node_modules_containers.read().unwrap();
        flow_services_inference::type_contents::type_parse_artifacts(
            &options,
            shared_mem.dupe(),
            env.master_cx.clone(),
            file_key.dupe(),
            intermediate_result,
            &node_modules_containers,
        )
    };
    if matches!(result, Err(TypeContentsError::CheckedDependenciesCanceled)) {
        return Err(CheckedDependenciesCanceled);
    }
    let (errors, warnings) =
        flow_services_inference::type_contents::printable_errors_of_file_artifacts_result(
            &options,
            env,
            shared_mem,
            &file_key,
            result.as_ref(),
        );
    let include_warnings = error_flags.include_warnings || options.include_warnings;
    let warning_count = if include_warnings {
        warnings.cardinal()
    } else {
        0
    };
    let mut buf = Vec::new();
    let strip_root = if strip_root || options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };
    let actual_warnings = if include_warnings {
        &warnings
    } else {
        &ConcreteLocPrintableErrorSet::empty()
    };
    let stdin_file = match &wire_input {
        server_socket_rpc::FileInput::FileContent(None, content) => {
            Some((PathBuf::from("-"), content.clone()))
        }
        server_socket_rpc::FileInput::FileContent(Some(path), content) => {
            Some((PathBuf::from(path), content.clone()))
        }
        server_socket_rpc::FileInput::FileName(_) => None,
    };
    let strip_root_str = strip_root.map(|root| root.to_string_lossy().into_owned());
    if json {
        flow_common_errors::error_utils::json_output::print_errors_with_offset_kind(
            &mut buf,
            strip_root_str.as_deref(),
            &[],
            pretty,
            json_version
                .unwrap_or(flow_common_errors::error_utils::json_output::JsonVersion::JsonV1),
            &stdin_file,
            offset_kind,
            &errors,
            actual_warnings,
        )
        .expect("failed to write errors to buffer");
    } else {
        cli_output::print_errors(
            &mut buf,
            &error_flags,
            &stdin_file,
            strip_root,
            &errors,
            actual_warnings,
            None,
        )
        .expect("failed to write errors to buffer");
    }
    Ok(ServerResponse::CheckContents {
        has_errors: !errors.is_empty(),
        warning_count,
        error_output: String::from_utf8_lossy(&buf).into_owned(),
        not_covered: false,
    })
}
