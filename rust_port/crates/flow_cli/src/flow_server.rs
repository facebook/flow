/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::net::TcpListener;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::RwLock;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::cli_output;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_server_env::monitor_prot::FileWatcherMetadata;
use flow_server_env::server_monitor_listener_state;
use flow_server_files::server_files_js;
use flow_server_rechecker::rechecker;
use flow_services_get_def::get_def_types::DefInfo;
use flow_services_inference::type_service;
use flow_services_inference_types::CheckedDependenciesCanceled;
use flow_services_inference_types::TypeContentsError;
use flow_utils_concurrency::thread_pool::ThreadPool;
use flow_utils_concurrency::worker_cancel;

use crate::command_connect;
use crate::command_connect::SaveStateOut;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;

/// Shared state for the server, protected by a Mutex.
/// The Condvar is used to wake the recheck thread when new work arrives.
struct ServerState {
    /// The current environment. None during init.
    env: Option<flow_server_env::server_env::Env>,
    /// Whether init has completed.
    init_done: bool,
    pending_recheck: bool,
    /// Whether a recheck is currently in progress.
    recheck_in_progress: bool,
    /// Whether the server should shut down.
    should_shutdown: bool,
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

    #[expect(dead_code)]
    pub(crate) fn run(&self) {
        self.run_with_signal_ready(false);
    }

    pub(crate) fn run_with_signal_ready(&self, signal_ready: bool) {
        let ready_fd: i32 = if signal_ready { 1 } else { -1 };
        let root = &*self.options.root;
        let lock_path = server_files_js::lock_file(&self.flowconfig_name, &self.tmp_dir, root);
        let socket_path = server_files_js::socket_file(&self.flowconfig_name, &self.tmp_dir, root);

        let _lock_file = match acquire_lock(&lock_path) {
            Ok(f) => f,
            Err(_) => {
                eprintln!("Error: another server is already running?");
                std::process::exit(1);
            }
        };

        let _lock_guard = LockGuard {
            lock_path: lock_path.clone(),
            socket_path: socket_path.clone(),
        };

        let _remove_result = std::fs::remove_file(&socket_path);

        if let Some(parent) = Path::new(&socket_path).parent() {
            let _mkdir_result = std::fs::create_dir_all(parent);
        }

        // Bind TCP listener BEFORE init so clients can connect during init.
        let listener = match TcpListener::bind("127.0.0.1:0") {
            Ok(l) => l,
            Err(e) => {
                eprintln!("Error: failed to bind TCP listener: {}", e);
                std::process::exit(1);
            }
        };
        let port = listener.local_addr().unwrap().port();

        // Write the port to the socket file so clients can connect
        if let Some(parent) = Path::new(&socket_path).parent() {
            let _mkdir_result = std::fs::create_dir_all(parent);
        }
        std::fs::write(&socket_path, port.to_string()).unwrap_or_else(|e| {
            eprintln!("Error: failed to write socket file {}: {}", socket_path, e);
            std::process::exit(1);
        });

        // Create shared server state
        let state = Arc::new((
            Mutex::new(ServerState {
                env: None,
                init_done: false,
                pending_recheck: false,
                recheck_in_progress: false,
                should_shutdown: false,
            }),
            Condvar::new(),
        ));

        // Spawn initialization in a background thread
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

        let ready_path = server_files_js::ready_file(&self.flowconfig_name, &self.tmp_dir, root);

        std::thread::spawn(move || {
            eprintln!("Initializing server...");
            let pool = ThreadPool::with_thread_count(
                flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
                    std::num::NonZeroUsize::new(init_pool_workers)
                        .expect("pool_workers should be positive"),
                ),
            );
            let (env, init_nmc, _first_internal_error) =
                type_service::init(&init_options, &pool, &init_shared_mem);

            // Update node_modules_containers
            {
                let mut nmc_guard = nmc.write().unwrap();
                *nmc_guard = init_nmc.read().unwrap().clone();
            }

            // Store the env and mark init as done
            {
                let (lock, cvar) = &*init_state;
                let mut server_state = lock.lock().unwrap();
                server_state.env = Some(env);
                server_state.init_done = true;
                cvar.notify_all();
                // Drop the MutexGuard before calling process_pending_rechecks
                // to avoid deadlock (process_pending_rechecks acquires the same lock).
            }

            eprintln!("Server is ready (port {})", port);

            // Signal readiness by writing to the ready file
            if ready_fd >= 0 {
                let _ = std::fs::write(&ready_path, "ready");
            }

            // Process any pending rechecks that arrived during init
            process_pending_rechecks(
                &init_state,
                &init_options,
                &init_shared_mem,
                init_pool_workers,
                &init_nmc_ref,
                &init_lock_path,
                &init_socket_path,
            );
        });

        // Accept connections in the main thread
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let state = state.clone();
                    let options = self.options.dupe();
                    let shared_mem = self.shared_mem.dupe();
                    let pool_workers = self.pool.num_workers();
                    let lock_path = lock_path.clone();
                    let socket_path = socket_path.clone();
                    let nmc = node_modules_containers.clone();
                    std::thread::spawn(move || {
                        handle_connection(
                            &state,
                            &options,
                            &shared_mem,
                            pool_workers,
                            stream,
                            &lock_path,
                            &socket_path,
                            &nmc,
                        );
                    });
                }
                Err(e) => {
                    eprintln!("Error accepting connection: {}", e);
                }
            }
        }
    }
}

/// Result of a force-recheck operation.
enum RecheckOutcome {
    /// Recheck completed successfully.
    Ok,
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
        let mut server_state = lock.lock().unwrap();
        server_state.env.take().unwrap()
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
            let (lock, _) = &**state;
            let mut server_state = lock.lock().unwrap();
            server_state.env = Some(env);
            return RecheckOutcome::Ok;
        }

        let def_info = DefInfo::NoDefinition(None);

        worker_cancel::resume_workers();
        will_be_checked_files.union(files_to_force.dupe());

        let old_env = env.clone();
        match type_service::recheck(
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
        ) {
            Ok((_log_recheck_event, _recheck_stats, _find_ref_results, new_env)) => {
                env = new_env;
            }
            Err(type_service::RecheckError::TooSlow) => {
                unreachable!("TooSlow is handled inside type_service::recheck");
            }
            Err(type_service::RecheckError::Canceled(changed_files)) => {
                eprintln!(
                    "Recheck successfully canceled. Restarting the recheck to include new file changes"
                );
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
                } else {
                    env = old_env;
                }
            }
        }
    }
}

/// Process any pending rechecks. This is called by the background init/recheck
/// thread after init completes and whenever new recheck requests arrive.
fn process_pending_rechecks(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    lock_path: &str,
    socket_path: &str,
) {
    loop {
        {
            let (lock, cvar) = &**state;
            let mut server_state = lock.lock().unwrap();

            // Check for shutdown
            if server_state.should_shutdown {
                let _ = std::fs::remove_file(socket_path);
                let _ = std::fs::remove_file(lock_path);
                std::process::exit(0);
            }

            if !server_state.pending_recheck {
                // No pending work -- mark recheck as not in progress.
                // Notify waiters (e.g., status handlers waiting for
                // recheck_in_progress == false).
                server_state.recheck_in_progress = false;
                cvar.notify_all();
                server_state = cvar
                    .wait_while(server_state, |s| !s.pending_recheck && !s.should_shutdown)
                    .unwrap();

                if server_state.should_shutdown {
                    let _ = std::fs::remove_file(socket_path);
                    let _ = std::fs::remove_file(lock_path);
                    std::process::exit(0);
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
                // Init not done yet, skip
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
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(lock_path)
}

struct LockGuard {
    lock_path: String,
    socket_path: String,
}

impl Drop for LockGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.socket_path);
        let _ = std::fs::remove_file(&self.lock_path);
    }
}

fn handle_connection(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    _pool_workers: usize,
    mut stream: std::net::TcpStream,
    lock_path: &str,
    socket_path: &str,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) {
    let request: ServerRequest = match command_connect::receive_message(&mut stream) {
        Ok(req) => req,
        Err(e) => {
            eprintln!("Error reading request: {}", e);
            return;
        }
    };

    let response = match request {
        ServerRequest::Status {
            include_warnings,
            strip_root,
        } => {
            // Wait for init to complete and no recheck in progress or pending
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
                    include_warnings,
                    strip_root,
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
            // Queue the recheck request and respond immediately
            let response = ServerResponse::ForceRecheck;
            if let Err(e) = command_connect::send_message(&mut stream, &response) {
                eprintln!("Error sending response: {}", e);
            }

            // Cancel any in-progress recheck
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
                    // Signal cancellation of the current recheck
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

            return;
        }
        ServerRequest::SaveState { out } => {
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
            filename,
            content,
            force,
            include_warnings,
            strip_root,
        } => loop {
            let (lock, cvar) = &**state;
            let server_state = lock.lock().unwrap();
            let server_state = cvar
                .wait_while(server_state, |s| !s.init_done || s.env.is_none())
                .unwrap();

            if let Some(ref env) = server_state.env {
                match handle_check_contents(
                    env,
                    options,
                    shared_mem,
                    node_modules_containers,
                    filename.clone(),
                    content.clone(),
                    force,
                    include_warnings,
                    strip_root,
                ) {
                    Ok(response) => break response,
                    Err(CheckedDependenciesCanceled) => {
                        drop(server_state);
                        let server_state = lock.lock().unwrap();
                        let _server_state = cvar
                            .wait_while(server_state, |s| {
                                !s.init_done || s.recheck_in_progress || s.pending_recheck
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
            let response = ServerResponse::ForceRecheck;
            let _ = command_connect::send_message(&mut stream, &response);
            eprintln!("Shutdown requested, exiting...");

            {
                let (lock, cvar) = &**state;
                let mut server_state = lock.lock().unwrap();
                server_state.should_shutdown = true;
                worker_cancel::stop_workers();
                cvar.notify_all();
            }

            let _ = std::fs::remove_file(socket_path);
            let _ = std::fs::remove_file(lock_path);
            std::process::exit(0);
        }
    };

    if let Err(e) = command_connect::send_message(&mut stream, &response) {
        eprintln!("Error sending response: {}", e);
    }
}

fn handle_status(
    env: &flow_server_env::server_env::Env,
    options: &Options,
    include_warnings: bool,
    client_strip_root: bool,
    lazy_mode: bool,
) -> ServerResponse {
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

    let error_count = errors.cardinal();
    let warning_count = if include_warnings || options.include_warnings {
        warnings.cardinal()
    } else {
        0
    };
    let has_errors = !errors.is_empty();

    let error_flags = cli_output::ErrorFlags {
        rendering_mode: cli_output::RenderingMode::CliColorNever,
        include_warnings: include_warnings || options.include_warnings,
        max_warnings: None,
        one_line: false,
        list_files: false,
        show_all_errors: true,
        show_all_branches: false,
        unicode: false,
        message_width: 120,
    };

    let mut buf = Vec::new();
    let strip_root = if client_strip_root || options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };
    let actual_warnings = if error_flags.include_warnings {
        &warnings
    } else {
        &ConcreteLocPrintableErrorSet::empty()
    };
    cli_output::print_errors(
        &mut buf,
        &error_flags,
        &None,
        strip_root,
        &errors,
        actual_warnings,
        None,
    )
    .expect("failed to write errors to buffer");

    let error_output = String::from_utf8_lossy(&buf).into_owned();

    let checked_files = env.checked_files.cardinal() as i32;
    let total_files = env.files.len() as i32;

    ServerResponse::Status {
        has_errors,
        error_count,
        warning_count,
        error_output,
        lazy_stats: command_connect::LazyStats {
            lazy_mode,
            checked_files,
            total_files,
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
        SaveStateOut::File(path) => PathBuf::from(path),
        SaveStateOut::Scm => match flow_saved_state::output_filename(options) {
            Ok(path) => path,
            Err(err) => {
                return ServerResponse::SaveState { result: Err(err) };
            }
        },
    };
    if let Some(parent) = path.parent() {
        if let Err(err) = std::fs::create_dir_all(parent) {
            return ServerResponse::SaveState {
                result: Err(err.to_string()),
            };
        }
    }
    let node_modules_containers = node_modules_containers.read().unwrap();
    let result = flow_saved_state::save(&path, shared_mem, env, options, &node_modules_containers)
        .map(|_| path.display().to_string())
        .map_err(|reason| reason.to_string());
    ServerResponse::SaveState { result }
}

fn handle_check_contents(
    env: &flow_server_env::server_env::Env,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    filename: Option<String>,
    content: String,
    force: bool,
    include_warnings: bool,
    strip_root: bool,
) -> Result<ServerResponse, CheckedDependenciesCanceled> {
    let mut options = options.clone();
    options.all = options.all || force;
    let filename = filename.unwrap_or_else(|| "-".to_string());
    let abs_path = if Path::new(&filename).is_absolute() {
        PathBuf::from(&filename)
    } else {
        options.root.join(&filename)
    };
    let file_key = FileKey::source_file_of_absolute(&abs_path.to_string_lossy());
    let intermediate_result =
        flow_services_inference::type_contents::parse_contents(&options, &content, &file_key);
    if intermediate_result.0.is_none() && intermediate_result.1.is_empty() {
        return Ok(ServerResponse::CheckContents {
            has_errors: false,
            warning_count: 0,
            error_output: String::new(),
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
    let warning_count = if include_warnings || options.include_warnings {
        warnings.cardinal()
    } else {
        0
    };
    let error_flags = cli_output::ErrorFlags {
        rendering_mode: cli_output::RenderingMode::CliColorNever,
        include_warnings: include_warnings || options.include_warnings,
        max_warnings: None,
        one_line: false,
        list_files: false,
        show_all_errors: true,
        show_all_branches: false,
        unicode: false,
        message_width: 120,
    };
    let mut buf = Vec::new();
    let strip_root = if strip_root || options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };
    let actual_warnings = if error_flags.include_warnings {
        &warnings
    } else {
        &ConcreteLocPrintableErrorSet::empty()
    };
    cli_output::print_errors(
        &mut buf,
        &error_flags,
        &None,
        strip_root,
        &errors,
        actual_warnings,
        None,
    )
    .expect("failed to write errors to buffer");
    Ok(ServerResponse::CheckContents {
        has_errors: !errors.is_empty(),
        warning_count,
        error_output: String::from_utf8_lossy(&buf).into_owned(),
        not_covered: false,
    })
}
