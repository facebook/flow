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

use crate::command_connect;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;

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

        let _ = std::fs::remove_file(&socket_path);

        if let Some(parent) = Path::new(&socket_path).parent() {
            let _ = std::fs::create_dir_all(parent);
        }

        eprintln!("Initializing server...");
        let (env, libs_ok, node_modules_containers) =
            type_service::init_from_scratch(&self.options, &self.pool, &self.shared_mem, root);
        let env = if libs_ok {
            if self.options.lazy_mode {
                let (env, _first_internal_error) = type_service::libdef_check_for_lazy_init(
                    &self.options,
                    &self.pool,
                    &self.shared_mem,
                    env,
                )
                .expect("Unexpected file changes during lazy init check");
                env
            } else {
                let (env, _first_internal_error) = type_service::full_check_for_init(
                    &self.options,
                    &self.pool,
                    &self.shared_mem,
                    None,
                    env,
                )
                .expect("Unexpected file changes during full check");
                env
            }
        } else {
            env
        };

        let env = Arc::new(Mutex::new(env));
        let focus_targets: Arc<Mutex<FlowOrdSet<FileKey>>> =
            Arc::new(Mutex::new(FlowOrdSet::new()));
        let node_modules_containers = node_modules_containers;

        // Bind to a random available TCP port on localhost
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
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&socket_path, port.to_string()).unwrap_or_else(|e| {
            eprintln!("Error: failed to write socket file {}: {}", socket_path, e);
            std::process::exit(1);
        });

        eprintln!("Server is ready (port {})", port);

        // Signal readiness by writing to the ready file
        if ready_fd >= 0 {
            let ready_path = format!("{}.ready", socket_path);
            let _ = std::fs::write(&ready_path, "ready");
        }

        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let env = env.clone();
                    let options = self.options.dupe();
                    let shared_mem = self.shared_mem.dupe();
                    let nmc = node_modules_containers.dupe();
                    let pool_workers = self.pool.num_workers();
                    let lock_path = lock_path.clone();
                    let socket_path = socket_path.clone();
                    let focus_targets = focus_targets.clone();
                    std::thread::spawn(move || {
                        handle_connection(
                            &env,
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
    env: &Arc<Mutex<flow_server_env::server_env::Env>>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    mut stream: std::net::TcpStream,
    lock_path: &str,
    socket_path: &str,
    accumulated_focus: &Arc<Mutex<FlowOrdSet<FileKey>>>,
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
            let env_guard = env.lock().unwrap();
            handle_status(
                &env_guard,
                options,
                include_warnings,
                strip_root,
                options.lazy_mode,
            )
        }
        ServerRequest::ForceRecheck {
            focus,
            files,
            missed_changes,
            ..
        } => {
            let response = ServerResponse::ForceRecheck;
            if let Err(e) = command_connect::send_message(&mut stream, &response) {
                eprintln!("Error sending response: {}", e);
            }

            handle_force_recheck(
                env,
                options,
                shared_mem,
                pool_workers,
                &files,
                focus,
                missed_changes,
                accumulated_focus,
                node_modules_containers,
            );

            return;
        }
        ServerRequest::Shutdown => {
            let response = ServerResponse::ForceRecheck;
            let _ = command_connect::send_message(&mut stream, &response);
            eprintln!("Shutdown requested, exiting...");
            let _ = std::fs::remove_file(socket_path);
            let _ = std::fs::remove_file(lock_path);
            std::process::exit(0);
        }
    };

    if let Err(e) = command_connect::send_message(&mut stream, &response) {
        eprintln!("Error sending response: {}", e);
    }
}

fn handle_force_recheck(
    env: &Arc<Mutex<flow_server_env::server_env::Env>>,
    options: &Options,
    shared_mem: &Arc<SharedMem>,
    pool_workers: usize,
    files: &[String],
    focus: bool,
    missed_changes: bool,
    accumulated_focus: &Arc<Mutex<FlowOrdSet<FileKey>>>,
    node_modules_containers: &Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>,
) -> ServerResponse {
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

    // Hold the env lock for the entire recheck to prevent concurrent status
    // requests from seeing an empty env. The OCaml server is single-threaded
    // (Lwt cooperative), so recheck naturally blocks other requests; we
    // replicate that behavior here by holding the mutex.
    let mut env_guard = env.lock().unwrap();

    let all_unordered_libs_set: BTreeSet<String> = env_guard
        .all_unordered_libs
        .iter()
        .map(|s| String::from(s.as_str()))
        .collect();

    // Build file keys, filtering out phantom files (files that don't exist on
    // disk AND aren't tracked in SharedMem). Including such files would cause
    // add_unparsed to create spurious entries and pollute haste module provider
    // lists with files that never existed.
    let all_file_keys: FlowOrdSet<FileKey> = files
        .iter()
        .filter_map(|f| {
            let abs_path = flow_common::files::cached_canonicalize(std::path::Path::new(f))
                .unwrap_or_else(|_| std::path::PathBuf::from(f));
            let path_str = abs_path.to_string_lossy();
            let fk = flow_common::files::filename_from_string(
                &options.file_options,
                false,
                &all_unordered_libs_set,
                &path_str,
            );
            // Skip files that don't exist on disk AND aren't in SharedMem.
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
        for lib in &env_guard.all_unordered_libs {
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

    // Fix up haste module provider lists for files that have been deleted or
    // restored. The incremental recheck's calc_dirty_modules only calls
    // add_provider when haste_info has_changed(), but for deleted/restored files
    // the haste info stays the same -- only the file's parse state changes.
    // Without this fixup, commit_modules/choose_provider would see stale
    // provider lists and fail to detect module provider changes.
    for fk in &all_file_keys {
        let abs_path = std::path::Path::new(fk.as_str());
        if let Some(haste_info) = shared_mem.get_haste_info(fk) {
            if let Some(haste_module) = shared_mem.get_haste_module(&haste_info) {
                if !abs_path.exists() {
                    // File was deleted: remove from all_providers so choose_provider
                    // returns None, making the module "changed" and triggering
                    // dependent rechecks with proper "cannot resolve" errors.
                    haste_module.remove_provider(fk);
                } else {
                    // File exists (possibly restored): ensure it's in all_providers
                    // so choose_provider can pick it. calc_dirty_modules won't call
                    // add_provider when haste info is unchanged (old == new).
                    haste_module.add_provider(fk.dupe());
                }
            }
        }
    }

    let mut updates = CheckedSet::empty();
    updates.add(Some(all_file_keys), None, None);

    eprintln!(
        "Force recheck: {} files, focus={}",
        updates.cardinal(),
        focus,
    );

    let current_env = std::mem::replace(
        &mut *env_guard,
        flow_server_env::server_env::Env {
            files: FlowOrdSet::new(),
            dependency_info: flow_server_env::dependency_info::DependencyInfo::empty(),
            checked_files: CheckedSet::empty(),
            package_json_files: FlowOrdSet::new(),
            ordered_libs: Vec::new(),
            all_unordered_libs: BTreeSet::new(),
            unparsed: FlowOrdSet::new(),
            errors: flow_server_env::server_env::Errors {
                local_errors: BTreeMap::new(),
                duplicate_providers: BTreeMap::new(),
                merge_errors: BTreeMap::new(),
                warnings: BTreeMap::new(),
                suppressions: flow_typing_errors::error_suppressions::ErrorSuppressions::empty(),
            },
            coverage: BTreeMap::new(),
            collated_errors: flow_server_env::collated_errors::CollatedErrors::empty(),
            connections: flow_server_env::persistent_connection::PersistentConnection::empty(),
            exports: None,
            master_cx: Arc::new(flow_typing_context::MasterContext::EmptyMasterContext),
        },
    );

    let def_info = DefInfo::NoDefinition(None);
    let mut will_be_checked_files = current_env.checked_files.dupe();

    match type_service::recheck(
        &pool,
        shared_mem,
        &options_arc,
        &updates,
        &def_info,
        files_to_force,
        false, // require_full_check_reinit
        None,  // changed_mergebase
        missed_changes,
        node_modules_containers,
        &mut will_be_checked_files,
        current_env,
    ) {
        Ok((_log_recheck_event, _recheck_stats, _find_ref_results, new_env)) => {
            *env_guard = new_env;
            eprintln!("Force recheck: complete");
        }
        Err(e) => {
            eprintln!("Force recheck failed: {:?}", e);
        }
    }

    ServerResponse::ForceRecheck
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
    let warning_count = warnings.cardinal();
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
