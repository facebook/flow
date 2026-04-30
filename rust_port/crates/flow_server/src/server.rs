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

use flow_common::files::LibDir;
use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::options::SupportedOs;
use flow_common_exit_status::FlowExitStatus;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_flowlib;
use flow_parser::file_key;
use flow_server_env::error_collator;
use flow_server_env::monitor_rpc;
use flow_server_env::persistent_connection;
use flow_server_env::server_env::Env;
use flow_server_env::server_env::Genv;
use flow_server_env::server_monitor_listener_state;
use flow_server_env::server_status;
use flow_typing_errors::intermediate_error::to_printable_error;

use crate::server_daemon;
use crate::server_env_build;

type NodeModulesContainers = Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>;

struct ProfilingRunning {
    start: std::time::Instant,
}

struct ProfilingFinished {
    duration: f64,
}

impl ProfilingRunning {
    fn new() -> Self {
        ProfilingRunning {
            start: std::time::Instant::now(),
        }
    }

    fn legacy_sample_memory(&self, _metric: &str, _value: f64) {}

    fn sample_memory(&self, _metric: &str, _value: f64) {}

    fn finish(self) -> ProfilingFinished {
        ProfilingFinished {
            duration: self.start.elapsed().as_secs_f64(),
        }
    }
}

impl ProfilingFinished {
    fn get_profiling_duration(&self) -> f64 {
        self.duration
    }
}

fn with_profiling<F, R>(label: &str, should_print_summary: bool, f: F) -> (ProfilingFinished, R)
where
    F: FnOnce(&ProfilingRunning) -> R,
{
    let running = ProfilingRunning::new();
    let ret = f(&running);
    let finished = running.finish();
    if should_print_summary {
        flow_hh_logger::info!(
            "{}: wall duration {:.3}s",
            label,
            finished.get_profiling_duration()
        );
    }
    (finished, ret)
}

fn sample_init_memory(
    profiling: &ProfilingRunning,
    shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
) {
    let hash_stats = shared_mem.hash_stats();
    let heap_size = shared_mem.heap_size();
    let memory_metrics = [
        ("heap.size", heap_size),
        ("hash_table.nonempty_slots", hash_stats.nonempty_slots),
        ("hash_table.used_slots", hash_stats.used_slots),
        ("hash_table.slots", hash_stats.slots),
    ];
    for (metric, value) in memory_metrics {
        profiling.legacy_sample_memory(&format!("init_done.{}", metric), value as f64);
    }
}

fn extract_flowlibs_or_exit(options: &Options) {
    match &options.file_options.default_lib_dir {
        Some(libdir) => {
            let flowlib_libdir = match libdir {
                LibDir::Prelude(path) => flow_flowlib::LibDir::Prelude(path.clone()),
                LibDir::Flowlib(path) => flow_flowlib::LibDir::Flowlib(path.clone()),
            };
            let extract_result =
                std::panic::catch_unwind(|| flow_flowlib::extract(&flowlib_libdir));
            match extract_result {
                Ok(()) => {}
                Err(_) => {
                    let libdir_str = flow_flowlib::path_of_libdir(&flowlib_libdir)
                        .display()
                        .to_string();
                    let msg = format!(
                        "Could not extract flowlib files into {}: extract failed",
                        libdir_str
                    );
                    flow_common_exit_status::exit_with_msg(
                        FlowExitStatus::CouldNotExtractFlowlibs,
                        &msg,
                    );
                }
            }
        }
        None => {}
    }
}

fn string_of_saved_state_fetcher(options: &Options) -> &'static str {
    match options.saved_state_fetcher {
        SavedStateFetcher::DummyFetcher => "none",
        SavedStateFetcher::LocalFetcher => "local",
        SavedStateFetcher::ScmFetcher => "scm",
        SavedStateFetcher::FbFetcher => "fb",
    }
}

fn idle_logging_loop(
    _options: &Options,
    _start_time: f64,
    stop_flag: &std::sync::Arc<std::sync::atomic::AtomicBool>,
) {
    let idle_period_in_seconds = 300.0_f64;
    let sample = |profiling: &ProfilingRunning| {
        let cgroup_stats = crate::cgroup::get_stats();
        match cgroup_stats {
            Err(_) => {}
            Ok(crate::cgroup::Stats {
                total,
                total_swap,
                anon,
                file,
                shmem,
            }) => {
                profiling.sample_memory("cgroup_total", total as f64);
                profiling.sample_memory("cgroup_swap", total_swap as f64);
                profiling.sample_memory("cgroup_anon", anon as f64);
                profiling.sample_memory("cgroup_shmem", shmem as f64);
                profiling.sample_memory("cgroup_file", file as f64);
            }
        }
    };

    let should_print_summary = _options.profile;
    while !stop_flag.load(std::sync::atomic::Ordering::Relaxed) {
        let (_profiling, completed_idle_period) =
            with_profiling("Idle", should_print_summary, |profiling| {
                let iterations = idle_period_in_seconds as u64;
                for _ in 0..iterations {
                    if stop_flag.load(std::sync::atomic::Ordering::Relaxed) {
                        return false;
                    }
                    sample(profiling);
                    std::thread::park_timeout(std::time::Duration::from_secs(1));
                }
                true
            });

        if !completed_idle_period {
            break;
        }

        let idle_time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64()
            - _start_time;
        flow_hh_logger::info!("Idle heartbeat after {:.3}s", idle_time);
        flow_event_logger::idle_heartbeat(idle_time, &serde_json::Value::Null);
    }
}

fn gc_loop(
    shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    stop_flag: &std::sync::Arc<std::sync::atomic::AtomicBool>,
) {
    while !stop_flag.load(std::sync::atomic::Ordering::Relaxed) {
        std::thread::sleep(std::time::Duration::from_millis(10));
        let done = shared_mem.collect_slice(10000);
        if done {
            break;
        }
    }
}

fn serve(
    _genv: &Genv,
    mut _env: Env,
    shared_mem: &Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &NodeModulesContainers,
) {
    loop {
        monitor_rpc::status_update(server_status::Event::Ready);

        let _options = &_genv.options;

        let _start_time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64();
        let stop_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let idle_handle = std::thread::spawn({
            let options = _options.clone();
            let start_time = _start_time;
            let stop = stop_flag.clone();
            move || {
                if !stop.load(std::sync::atomic::Ordering::Relaxed) {
                    idle_logging_loop(&options, start_time, &stop);
                }
            }
        });
        let gc_handle = std::thread::spawn({
            let shared_mem = shared_mem.clone();
            let stop = stop_flag.clone();
            move || gc_loop(&shared_mem, &stop)
        });
        server_monitor_listener_state::wait_for_anything(
            &|skip_incompatible, updates| {
                flow_server_rechecker::rechecker::process_updates(
                    skip_incompatible,
                    _options,
                    &_env,
                    shared_mem,
                    updates,
                )
            },
            &|| _env.checked_files.clone(),
        );

        stop_flag.store(true, std::sync::atomic::Ordering::Relaxed);
        idle_handle.thread().unpark();
        let _ = idle_handle.join();
        let _ = gc_handle.join();

        let (_profiling, new_env) = flow_server_rechecker::rechecker::recheck_loop(
            _genv,
            _env,
            shared_mem,
            node_modules_containers,
        );
        _env = new_env;

        let next_workload = server_monitor_listener_state::pop_next_workload();
        if let Some(workload) = next_workload {
            flow_hh_logger::info!("Running a serial workload");
            _env = workload(_env);
        }
        // Flush the logs asynchronously
        log::logger().flush();
    }
}

#[allow(dead_code)]
pub(crate) fn on_compact(shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>) -> impl FnOnce() {
    monitor_rpc::status_update(server_status::Event::GCStart);
    let old_size = shared_mem.heap_size();
    let start_t = std::time::Instant::now();
    flow_services_inference::merge_service::check_contents_cache()
        .borrow_mut()
        .clear();
    persistent_connection::clear_type_parse_artifacts_caches();
    move || {
        let new_size = shared_mem.heap_size();
        let time_taken = start_t.elapsed().as_secs_f64();
        if old_size != new_size {
            flow_hh_logger::info!(
                "Sharedmem GC: {} bytes before; {} bytes after; in {} seconds",
                old_size,
                new_size,
                time_taken
            );
            flow_event_logger::sharedmem_gc_ran(
                "aggressive",
                old_size as f64,
                new_size as f64,
                time_taken,
            );
        }
    }
}

pub fn create_program_init(init_id: &str, options: Arc<Options>) -> Genv {
    file_key::set_project_root(&options.root.display().to_string());
    match &options.file_options.default_lib_dir {
        Some(LibDir::Flowlib(path) | LibDir::Prelude(path)) => {
            file_key::set_flowlib_root(&path.display().to_string());
        }
        None => {}
    }

    let shared_mem = Arc::new(flow_heap::parsing_heaps::SharedMem::new());
    let shared_mem_for_on_compact = shared_mem.clone();
    shared_mem.set_on_compact(Arc::new(move || {
        Box::new(on_compact(shared_mem_for_on_compact.clone()))
    }));
    server_env_build::make_genv(options, init_id, shared_mem)
}

fn detect_linux_distro() -> Option<String> {
    let contents = match std::fs::read_to_string("/etc/os-release") {
        Ok(c) => c,
        Err(_) => return None,
    };
    let lines: Vec<&str> = contents.lines().collect();

    let id_line = lines.iter().find(|line| line.starts_with("ID="));
    match id_line {
        Some(line) => {
            let id = &line[3..];
            let id = id.trim();
            let id = if id.len() >= 2 && id.starts_with('"') && id.ends_with('"') {
                &id[1..id.len() - 1]
            } else {
                id
            };
            Some(id.to_string())
        }
        None => None,
    }
}

pub fn check_supported_operating_system(options: &Options) {
    let supported_os_list = &options.supported_operating_systems;
    let current_os_unsupported = match supported_os_list.as_slice() {
        [] => false,
        _ => !supported_os_list.iter().any(|os| match os {
            SupportedOs::CentOS => {
                matches!(detect_linux_distro().as_deref(), Some("centos"))
            }
        }),
    };
    if current_os_unsupported {
        let current_os = match detect_linux_distro() {
            Some(distro) => distro,
            None => "Unknown".to_string(),
        };
        let msg = format!(
            "This operating system ({}) is not supported by this Flow configuration.",
            current_os
        );
        flow_hh_logger::info!("{}", msg);
        flow_common_exit_status::exit(FlowExitStatus::InvalidFlowconfig);
    }
}

fn run(_init_id: &str, _options: Arc<Options>, monitor_channels: Option<monitor_rpc::Channels>) {
    // Check if the current operating system is supported
    check_supported_operating_system(&_options);

    match monitor_channels {
        Some(channels) => monitor_rpc::init(channels),
        None => monitor_rpc::disable(),
    }

    let genv_arc = Arc::new(create_program_init(_init_id, _options.clone()));
    let listener_running = matches!(monitor_rpc::state(), monitor_rpc::StateKind::Initialized);
    if listener_running {
        let genv_for_listener = genv_arc.clone();
        std::thread::spawn(move || {
            let callbacks = flow_server_env::server_monitor_listener::CommandHandlerCallbacks {
                enqueue_or_handle_ephemeral:
                    flow_server_command_handler::command_handler::enqueue_or_handle_ephemeral,
                enqueue_persistent:
                    flow_server_command_handler::command_handler::enqueue_persistent,
            };
            flow_server_env::server_monitor_listener::listen_for_messages(
                &genv_for_listener,
                &callbacks,
            );
        });
    }

    let t = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    flow_hh_logger::info!("Initializing Server (This might take some time)");

    flow_hh_logger::info!(
        "executable={}",
        std::env::current_exe()
            .map(|p| p.display().to_string())
            .unwrap_or_default()
    );
    flow_hh_logger::info!("version={}", flow_common::flow_version::VERSION);

    crate::multi_worker::set_report_canceled_callback(|total, finished| {
        flow_hh_logger::info!("Canceling progress {}/{}", finished, total);
        monitor_rpc::status_update(server_status::Event::CancelingProgress(
            server_status::Progress {
                total: Some(total),
                finished,
            },
        ));
    });

    monitor_rpc::status_update(server_status::Event::InitStart);

    extract_flowlibs_or_exit(&genv_arc.options);

    let should_print_summary = _options.profile;
    let pool = genv_arc
        .workers
        .as_ref()
        .expect("workers must be initialized");
    let (profiling, (env, node_modules_containers, first_internal_error)) =
        with_profiling("Init", should_print_summary, |profiling| {
            let (env, node_modules_containers, first_internal_error) =
                flow_services_inference::type_service::init(
                    &genv_arc.options,
                    pool,
                    &genv_arc.shared_mem,
                );
            sample_init_memory(profiling, &genv_arc.shared_mem);

            flow_event_logger::sharedmem_init_done(genv_arc.shared_mem.heap_size() as u64);

            (env, node_modules_containers, first_internal_error)
        });
    let init_duration = profiling.get_profiling_duration();

    monitor_rpc::send_telemetry(
        flow_server_env::lsp_prot::TelemetryFromServer::InitSummary {
            duration: init_duration,
        },
    );
    monitor_rpc::status_update(server_status::Event::FinishingUp);

    let saved_state_fetcher = string_of_saved_state_fetcher(&_options);

    flow_event_logger::init_done(
        first_internal_error.as_deref(),
        saved_state_fetcher,
        &serde_json::json!({ "duration": init_duration }),
    );

    flow_hh_logger::info!("Server is READY");

    let t_prime = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    flow_hh_logger::info!("Took {} seconds to initialize.", t_prime - t);

    serve(
        &genv_arc,
        env,
        &genv_arc.shared_mem,
        &node_modules_containers,
    );
}

fn exit_msg_of_exception(error: &dyn std::fmt::Display, msg: &str) -> String {
    let bt = format!("{}", error);
    if bt.is_empty() {
        msg.to_string()
    } else {
        format!("{}:\n{}", msg, bt)
    }
}

pub fn run_from_daemonize(
    _init_id: &str,
    _options: Arc<Options>,
    monitor_channels: Option<monitor_rpc::Channels>,
) {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run(_init_id, _options, monitor_channels);
    }));
    match result {
        Ok(()) => {}
        Err(e) => {
            let err_msg = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic".to_string()
            };
            let err_display: &dyn std::fmt::Display = &err_msg;
            if err_msg.contains("Out_of_shared_memory") || err_msg.contains("Out of shared memory")
            {
                let msg = exit_msg_of_exception(err_display, "Out of shared memory");
                flow_hh_logger::info!("{}", msg);
                flow_common_exit_status::exit(FlowExitStatus::OutOfSharedMemory);
            } else if err_msg.contains("Hash_table_full") || err_msg.contains("Hash table is full")
            {
                let msg = exit_msg_of_exception(err_display, "Hash table is full");
                flow_hh_logger::info!("{}", msg);
                flow_common_exit_status::exit(FlowExitStatus::HashTableFull);
            } else if err_msg.contains("Heap_full") || err_msg.contains("Heap is full") {
                let msg = exit_msg_of_exception(err_display, "Heap is full");
                flow_hh_logger::info!("{}", msg);
                flow_common_exit_status::exit(FlowExitStatus::HeapFull);
            } else if err_msg.contains("Monitor_died") || err_msg.contains("Monitor died") {
                flow_hh_logger::info!("Monitor died unexpectedly");
                flow_common_exit_status::exit(FlowExitStatus::KilledByMonitor);
            } else {
                let msg = format!("Unhandled exception: {}", err_msg);
                flow_hh_logger::info!("{}", msg);
                flow_common_exit_status::exit(FlowExitStatus::UnknownError);
            }
        }
    }
}

pub fn check_once(_init_id: &str, _options: Arc<Options>) {
    monitor_rpc::disable();

    let _genv = create_program_init(_init_id, _options.clone());

    flow_hh_logger::info!(
        "executable={}",
        std::env::current_exe()
            .map(|p| p.display().to_string())
            .unwrap_or_default()
    );
    flow_hh_logger::info!("version={}", flow_common::flow_version::VERSION);

    crate::multi_worker::set_report_canceled_callback(|total, finished| {
        flow_hh_logger::info!("Canceling progress {}/{}", finished, total);
        monitor_rpc::status_update(server_status::Event::CancelingProgress(
            server_status::Progress {
                total: Some(total),
                finished,
            },
        ));
    });

    monitor_rpc::status_update(server_status::Event::InitStart);

    extract_flowlibs_or_exit(&_genv.options);

    let should_print_summary = _options.profile;
    let pool = _genv.workers.as_ref().expect("workers must be initialized");

    let (profiling, (_errors, _warnings, _first_internal_error)) =
        with_profiling("Init", should_print_summary, |profiling| {
            let (env, _node_modules_containers, first_internal_error) =
                flow_services_inference::type_service::init(
                    &_genv.options,
                    pool,
                    &_genv.shared_mem,
                );
            sample_init_memory(profiling, &_genv.shared_mem);

            flow_event_logger::sharedmem_init_done(_genv.shared_mem.heap_size() as u64);

            let (errors, warnings, suppressed_errors) = error_collator::get(&env);

            let shared_mem = &_genv.shared_mem;
            let loc_of_aloc =
                |aloc: &flow_aloc::ALoc| -> flow_parser::loc::Loc { shared_mem.loc_of_aloc(aloc) };
            let get_ast = |file: &flow_parser::file_key::FileKey| -> Option<
                Arc<flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>>,
            > { shared_mem.get_ast(file) };
            let strip_root = if _options.strip_root {
                Some(_options.root.as_path())
            } else {
                None
            };
            let _suppressed_errors: Vec<_> = if _options.include_suppressions {
                suppressed_errors
                    .into_iter()
                    .map(|(e, loc_set)| {
                        (
                            to_printable_error(&loc_of_aloc, get_ast, strip_root, e),
                            loc_set,
                        )
                    })
                    .collect()
            } else {
                vec![]
            };
            (errors, warnings, first_internal_error)
        });

    monitor_rpc::send_telemetry(
        flow_server_env::lsp_prot::TelemetryFromServer::InitSummary {
            duration: profiling.get_profiling_duration(),
        },
    );
    monitor_rpc::status_update(server_status::Event::FinishingUp);

    let saved_state_fetcher = string_of_saved_state_fetcher(&_options);

    flow_event_logger::init_done(
        _first_internal_error.as_deref(),
        saved_state_fetcher,
        &serde_json::json!({ "duration": profiling.get_profiling_duration() }),
    );
}

pub fn daemonize(
    init_id: &str,
    log_file: &str,
    argv: &[String],
    lazy_mode: Option<String>,
    no_flowlib: bool,
    ignore_version: bool,
    file_watcher_pid: Option<u32>,
    options: Arc<Options>,
) -> Result<flow_daemon::Handle<(), ()>, String> {
    server_daemon::daemonize(
        init_id,
        log_file,
        argv,
        lazy_mode,
        no_flowlib,
        ignore_version,
        options,
        file_watcher_pid,
    )
}
