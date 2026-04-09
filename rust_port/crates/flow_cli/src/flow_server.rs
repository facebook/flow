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
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::RwLock;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::cli_output;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_server_files::server_files_js;
use flow_services_get_def::get_def_types::DefInfo;
use flow_services_inference::type_service;
use flow_utils_concurrency::thread_pool::ThreadPool;
use flow_utils_concurrency::worker_cancel;

use crate::command_connect;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;

/// Represents a pending force-recheck request.
struct RecheckRequest {
    files: Vec<String>,
    focus: bool,
    missed_changes: bool,
}

/// Shared state for the server, protected by a Mutex.
/// The Condvar is used to wake the recheck thread when new work arrives.
struct ServerState {
    /// The current environment. None during init.
    env: Option<flow_server_env::server_env::Env>,
    /// Whether init has completed.
    init_done: bool,
    /// Pending recheck requests (accumulated while a recheck is in progress).
    pending_rechecks: Vec<RecheckRequest>,
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
                pending_rechecks: Vec::new(),
                recheck_in_progress: false,
                should_shutdown: false,
            }),
            Condvar::new(),
        ));

        let focus_targets: Arc<Mutex<FlowOrdSet<FileKey>>> =
            Arc::new(Mutex::new(FlowOrdSet::new()));

        // Spawn initialization in a background thread
        let init_state = state.clone();
        let init_options = self.options.dupe();
        let init_shared_mem = self.shared_mem.dupe();
        let init_pool_workers = self.pool.num_workers();
        let init_focus_targets = focus_targets.clone();
        let init_lock_path = lock_path.clone();
        let init_socket_path = socket_path.clone();

        let node_modules_containers: Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>> =
            Arc::new(RwLock::new(BTreeMap::new()));
        let nmc = node_modules_containers.clone();
        let init_nmc_ref = nmc.clone();

        let ready_path = format!("{}.ready", socket_path);

        std::thread::spawn(move || {
            eprintln!("Initializing server...");
            let (env, libs_ok, init_nmc) = type_service::init_from_scratch(
                &init_options,
                &ThreadPool::with_thread_count(
                    flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
                        std::num::NonZeroUsize::new(init_pool_workers)
                            .expect("pool_workers should be positive"),
                    ),
                ),
                &init_shared_mem,
                &init_options.root,
            );
            let env = if libs_ok {
                if init_options.lazy_mode {
                    let pool = ThreadPool::with_thread_count(
                        flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
                            std::num::NonZeroUsize::new(init_pool_workers)
                                .expect("pool_workers should be positive"),
                        ),
                    );
                    let (env, _first_internal_error) = type_service::libdef_check_for_lazy_init(
                        &init_options,
                        &pool,
                        &init_shared_mem,
                        env,
                    )
                    .expect("Unexpected file changes during lazy init check");
                    env
                } else {
                    let pool = ThreadPool::with_thread_count(
                        flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
                            std::num::NonZeroUsize::new(init_pool_workers)
                                .expect("pool_workers should be positive"),
                        ),
                    );
                    let (env, _first_internal_error) = type_service::full_check_for_init(
                        &init_options,
                        &pool,
                        &init_shared_mem,
                        None,
                        env,
                    )
                    .expect("Unexpected file changes during full check");
                    env
                }
            } else {
                env
            };

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
                &init_focus_targets,
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
                    let focus_targets = focus_targets.clone();
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
                            &focus_targets,
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
    /// Server should exit (e.g., package.json changed).
    Exit,
    /// Recheck was cancelled. The original env was restored.
    /// The files from the cancelled recheck should be re-queued.
    Cancelled { files: Vec<String> },
}

/// Process any pending rechecks. This is called by the background init/recheck
/// thread after init completes and whenever new recheck requests arrive.
fn process_pending_rechecks(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Arc<Options>,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    accumulated_focus: &Arc<Mutex<FlowOrdSet<FileKey>>>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
    lock_path: &str,
    socket_path: &str,
) {
    loop {
        let pending: Vec<RecheckRequest>;
        {
            let (lock, cvar) = &**state;
            let mut server_state = lock.lock().unwrap();

            // Check for shutdown
            if server_state.should_shutdown {
                let _ = std::fs::remove_file(socket_path);
                let _ = std::fs::remove_file(lock_path);
                std::process::exit(0);
            }

            if server_state.pending_rechecks.is_empty() {
                // No pending work -- mark recheck as not in progress.
                // Notify waiters (e.g., status handlers waiting for
                // recheck_in_progress == false).
                server_state.recheck_in_progress = false;
                cvar.notify_all();
                server_state = cvar
                    .wait_while(server_state, |s| {
                        s.pending_rechecks.is_empty() && !s.should_shutdown
                    })
                    .unwrap();

                if server_state.should_shutdown {
                    let _ = std::fs::remove_file(socket_path);
                    let _ = std::fs::remove_file(lock_path);
                    std::process::exit(0);
                }
            }

            // Take all pending rechecks
            pending = std::mem::take(&mut server_state.pending_rechecks);
            server_state.recheck_in_progress = true;
        }

        if pending.is_empty() {
            continue;
        }
        // Merge all pending recheck requests into one
        let mut all_files: Vec<String> = Vec::new();
        let mut any_focus = false;
        let mut any_missed_changes = false;
        for req in pending {
            all_files.extend(req.files);
            any_focus = any_focus || req.focus;
            any_missed_changes = any_missed_changes || req.missed_changes;
        }

        // Clear any cancellation from previous rechecks
        worker_cancel::resume_workers();

        let outcome = {
            let (lock, _) = &**state;
            let server_state = lock.lock().unwrap();
            if server_state.env.is_some() {
                drop(server_state);
                do_force_recheck(
                    state,
                    options,
                    shared_mem,
                    pool_workers,
                    &all_files,
                    any_focus,
                    any_missed_changes,
                    accumulated_focus,
                    node_modules_containers,
                )
            } else {
                // Init not done yet, skip
                RecheckOutcome::Ok
            }
        };

        match outcome {
            RecheckOutcome::Ok => {}
            RecheckOutcome::Exit => {
                let _ = std::fs::remove_file(socket_path);
                let _ = std::fs::remove_file(lock_path);
                std::process::exit(0);
            }
            RecheckOutcome::Cancelled { files } => {
                // Re-queue the cancelled files so they get processed in the
                // next iteration along with any new pending rechecks.
                let (lock, _) = &**state;
                let mut server_state = lock.lock().unwrap();
                // Prepend cancelled files so they are processed first
                server_state.pending_rechecks.insert(
                    0,
                    RecheckRequest {
                        files,
                        focus: any_focus,
                        missed_changes: any_missed_changes,
                    },
                );
            }
        }
    }
}

/// Perform the actual force-recheck operation.
fn do_force_recheck(
    state: &Arc<(Mutex<ServerState>, Condvar)>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    files: &[String],
    focus: bool,
    missed_changes: bool,
    accumulated_focus: &Arc<Mutex<FlowOrdSet<FileKey>>>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> RecheckOutcome {
    // Check for incompatible package.json changes.
    let has_package_json_change = files.iter().any(|f| {
        let basename = std::path::Path::new(f)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");
        basename == "package.json"
    });
    if has_package_json_change {
        eprintln!("Package.json changed, server needs restart");
        return RecheckOutcome::Exit;
    }

    eprintln!(
        "Force recheck: incremental recheck of {} files...",
        files.len()
    );
    let pool = ThreadPool::with_thread_count(
        flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(
            std::num::NonZeroUsize::new(pool_workers).expect("pool_workers should be positive"),
        ),
    );
    let options_arc = Arc::new(options.clone());
    // Take env out of shared state for the duration of recheck
    let current_env = {
        let (lock, _) = &**state;
        let mut server_state = lock.lock().unwrap();
        server_state.env.take().unwrap()
    };

    let all_unordered_libs_set: BTreeSet<String> = current_env
        .all_unordered_libs
        .iter()
        .map(|s| String::from(s.as_str()))
        .collect();

    let sroot = format!(
        "{}{}",
        options.root.to_string_lossy(),
        std::path::MAIN_SEPARATOR
    );
    let file_options = &options.file_options;
    let all_file_keys: FlowOrdSet<FileKey> = files
        .iter()
        .filter_map(|f| {
            let abs_path = flow_common::files::cached_canonicalize(std::path::Path::new(f))
                .unwrap_or_else(|_| std::path::PathBuf::from(f));
            let path_str = abs_path.to_string_lossy();
            if !flow_common::files::is_flow_file(file_options, &path_str) {
                return None;
            }
            if !(flow_common::files::is_included(file_options, &path_str)
                || file_options.implicitly_include_root && path_str.starts_with(&sroot))
            {
                return None;
            }
            let fk = flow_common::files::filename_from_string(
                file_options,
                true,
                &all_unordered_libs_set,
                &path_str,
            );
            if !abs_path.exists() && shared_mem.get_parse(&fk).is_none() {
                return None;
            }
            Some(fk)
        })
        .collect();

    let files_to_force = if options.lazy_mode && focus {
        let mut acc = accumulated_focus.lock().unwrap();
        for fk in &all_file_keys {
            acc.insert(fk.dupe());
        }
        for lib in &current_env.all_unordered_libs {
            acc.insert(FileKey::new(FileKeyInner::LibFile(String::from(
                lib.as_str(),
            ))));
        }
        let mut ft = CheckedSet::empty();
        ft.add(Some(acc.clone()), None, None);
        ft
    } else {
        CheckedSet::empty()
    };

    // Fix up haste module provider lists
    for fk in &all_file_keys {
        let abs_path = std::path::Path::new(fk.as_str());
        if let Some(haste_info) = shared_mem.get_haste_info(fk) {
            if let Some(haste_module) = shared_mem.get_haste_module(&haste_info) {
                if !abs_path.exists() {
                    haste_module.remove_provider(fk);
                } else {
                    haste_module.add_provider(fk.dupe());
                }
            }
        }
    }

    // Separate lib files from source files
    let mut lib_content_changed = false;
    let non_lib_file_keys: FlowOrdSet<FileKey> = all_file_keys
        .iter()
        .filter(|fk| {
            if fk.is_lib_file() {
                let abs_path = fk.to_absolute();
                if let Ok(content) = std::fs::read_to_string(&abs_path) {
                    if !flow_parsing::parsing_service::does_content_match_file_hash(
                        shared_mem, fk, &content,
                    ) {
                        lib_content_changed = true;
                    }
                } else {
                    lib_content_changed = true;
                }
                false
            } else {
                true
            }
        })
        .map(|fk| fk.dupe())
        .collect();

    let mut updates = CheckedSet::empty();
    updates.add(Some(non_lib_file_keys), None, None);

    eprintln!(
        "Force recheck: {} files, focus={}",
        updates.cardinal(),
        focus,
    );

    if lib_content_changed {
        eprintln!("Lib file content changed, reinitializing from scratch...");
        shared_mem.clear_reader_cache();
        for file in current_env.files.iter() {
            shared_mem.clear_file(file.dupe(), shared_mem.get_haste_info(file));
        }
        for file in current_env.unparsed.iter() {
            shared_mem.clear_file(file.dupe(), shared_mem.get_haste_info(file));
        }
        for lib in &current_env.all_unordered_libs {
            let lib_key = FileKey::new(FileKeyInner::LibFile(String::from(lib.as_str())));
            shared_mem.clear_file(lib_key, None);
        }
        let root = &*options.root;
        let (new_env, libs_ok, new_nmc) =
            type_service::init_from_scratch(&options_arc, &pool, shared_mem, root);
        let new_env = if libs_ok {
            if options.lazy_mode {
                let (env, _first_internal_error) = type_service::libdef_check_for_lazy_init(
                    &options_arc,
                    &pool,
                    shared_mem,
                    new_env,
                )
                .expect("Unexpected file changes during lazy init check");
                env
            } else {
                let (env, _first_internal_error) = type_service::full_check_for_init(
                    &options_arc,
                    &pool,
                    shared_mem,
                    None,
                    new_env,
                )
                .expect("Unexpected file changes during full check");
                env
            }
        } else {
            new_env
        };
        // Update node_modules_containers
        let mut nmc_guard = node_modules_containers.write().unwrap();
        *nmc_guard = new_nmc.read().unwrap().clone();
        eprintln!("Force recheck: complete (reinit)");
        {
            let (lock, _) = &**state;
            let mut server_state = lock.lock().unwrap();
            server_state.env = Some(new_env);
        }
        return RecheckOutcome::Ok;
    }

    {
        // Use the current_env directly (we took it out of the state)
        let def_info = DefInfo::NoDefinition(None);
        let mut will_be_checked_files = current_env.checked_files.dupe();

        // Check if we were cancelled before we even start the recheck
        if worker_cancel::should_cancel() {
            eprintln!("Force recheck: cancelled before start, restoring env");
            let (lock, _) = &**state;
            let mut server_state = lock.lock().unwrap();
            server_state.env = Some(current_env);
            return RecheckOutcome::Cancelled {
                files: files.to_vec(),
            };
        }

        match type_service::recheck(
            &pool,
            shared_mem,
            &options_arc,
            &updates,
            &def_info,
            files_to_force,
            false,
            None,
            missed_changes,
            node_modules_containers,
            &mut will_be_checked_files,
            current_env,
        ) {
            Ok((_log_recheck_event, _recheck_stats, _find_ref_results, new_env)) => {
                // Check if we were cancelled during the recheck.
                // If so, discard the result and restore the original env so
                // the cancelled files can be re-processed in the next recheck.
                if worker_cancel::should_cancel() {
                    eprintln!("Force recheck: cancelled during recheck, discarding result");
                    // Restore pre-recheck env by re-initializing from scratch.
                    // The recheck may have partially modified shared memory,
                    // so we need a clean reinit. Clear all file entries so
                    // init_from_scratch re-discovers and re-parses everything.
                    shared_mem.clear_reader_cache();
                    for file in new_env.files.iter() {
                        shared_mem.clear_file(file.dupe(), shared_mem.get_haste_info(file));
                    }
                    for file in new_env.unparsed.iter() {
                        shared_mem.clear_file(file.dupe(), shared_mem.get_haste_info(file));
                    }
                    for lib in &new_env.all_unordered_libs {
                        let lib_key =
                            FileKey::new(FileKeyInner::LibFile(String::from(lib.as_str())));
                        shared_mem.clear_file(lib_key, None);
                    }
                    let root = &*options.root;
                    let (reinit_env, libs_ok, new_nmc) =
                        type_service::init_from_scratch(&options_arc, &pool, shared_mem, root);
                    let reinit_env = if libs_ok {
                        if options.lazy_mode {
                            let (env, _) = type_service::libdef_check_for_lazy_init(
                                &options_arc,
                                &pool,
                                shared_mem,
                                reinit_env,
                            )
                            .expect("Unexpected file changes during lazy init check");
                            env
                        } else {
                            let (env, _) = type_service::full_check_for_init(
                                &options_arc,
                                &pool,
                                shared_mem,
                                None,
                                reinit_env,
                            )
                            .expect("Unexpected file changes during full check");
                            env
                        }
                    } else {
                        reinit_env
                    };
                    {
                        let mut nmc_guard = node_modules_containers.write().unwrap();
                        *nmc_guard = new_nmc.read().unwrap().clone();
                    }
                    {
                        let (lock, _) = &**state;
                        let mut server_state = lock.lock().unwrap();
                        server_state.env = Some(reinit_env);
                    }
                    return RecheckOutcome::Cancelled {
                        files: files.to_vec(),
                    };
                }
                eprintln!("Force recheck: complete");
                // Store the new env
                {
                    let (lock, _) = &**state;
                    let mut server_state = lock.lock().unwrap();
                    server_state.env = Some(new_env);
                }
                RecheckOutcome::Ok
            }
            Err(e) => {
                eprintln!("Force recheck failed: {:?}", e);
                // The recheck was cancelled (e.g., a dependent file was
                // deleted from disk). Re-init from scratch to get a clean
                // env that matches the current state of the filesystem.
                eprintln!("Re-initializing from scratch after failed recheck...");
                shared_mem.clear_reader_cache();
                let root = &*options.root;
                let (new_env, libs_ok, new_nmc) =
                    type_service::init_from_scratch(&options_arc, &pool, shared_mem, root);
                let new_env = if libs_ok {
                    if options.lazy_mode {
                        let (env, _) = type_service::libdef_check_for_lazy_init(
                            &options_arc,
                            &pool,
                            shared_mem,
                            new_env,
                        )
                        .expect("Unexpected file changes during lazy init check");
                        env
                    } else {
                        let (env, _) = type_service::full_check_for_init(
                            &options_arc,
                            &pool,
                            shared_mem,
                            None,
                            new_env,
                        )
                        .expect("Unexpected file changes during full check");
                        env
                    }
                } else {
                    new_env
                };
                {
                    let mut nmc_guard = node_modules_containers.write().unwrap();
                    *nmc_guard = new_nmc.read().unwrap().clone();
                }
                eprintln!("Force recheck: complete (reinit after failure)");
                {
                    let (lock, _) = &**state;
                    let mut server_state = lock.lock().unwrap();
                    server_state.env = Some(new_env);
                }
                RecheckOutcome::Ok
            }
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
    _shared_mem: &Arc<SharedMem>,
    _pool_workers: usize,
    mut stream: std::net::TcpStream,
    lock_path: &str,
    socket_path: &str,
    _accumulated_focus: &Arc<Mutex<FlowOrdSet<FileKey>>>,
    _node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
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
                    !s.init_done || s.recheck_in_progress || !s.pending_rechecks.is_empty()
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
            ..
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

                if server_state.recheck_in_progress {
                    // Signal cancellation of the current recheck
                    worker_cancel::stop_workers();
                }

                server_state.pending_rechecks.push(RecheckRequest {
                    files,
                    focus,
                    missed_changes,
                });
                cvar.notify_all();
            }

            return;
        }
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
