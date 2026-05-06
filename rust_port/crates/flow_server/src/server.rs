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
use std::time::Duration;

use flow_cgroup as cgroup;
use flow_common::files::LibDir;
use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::options::SupportedOs;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_common_exit_status::FlowExitStatus;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_flowlib;
use flow_parser::ast::Program;
use flow_parser::file_key;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_server_env::error_collator;
use flow_server_env::monitor_rpc;
use flow_server_env::persistent_connection;
use flow_server_env::server_env::Env;
use flow_server_env::server_env::Genv;
use flow_server_env::server_monitor_listener_state;
use flow_server_env::server_status;
use flow_services_inference::type_service::RecheckError;
use flow_typing_errors::intermediate_error::to_printable_error;

use crate::server_daemon;
use crate::server_env_build;

type NodeModulesContainers = Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>>;
pub type CheckOnceSuppressedErrors = Vec<(PrintableError<Loc>, BTreeSet<Loc>)>;
pub type CheckOnceCollatedErrors<'a> = (
    &'a ConcreteLocPrintableErrorSet,
    &'a ConcreteLocPrintableErrorSet,
    CheckOnceSuppressedErrors,
);
pub type CheckOncePrintErrors<'a> = Box<dyn FnOnce(&ProfilingFinished) + 'a>;

struct ProfilingRunning {
    start: std::time::Instant,
}

pub struct ProfilingFinished {
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

    fn with_timer<F, R>(&self, _timer: &str, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        f()
    }

    fn finish(self) -> ProfilingFinished {
        ProfilingFinished {
            duration: self.start.elapsed().as_secs_f64(),
        }
    }
}

impl ProfilingFinished {
    pub fn get_profiling_duration(&self) -> f64 {
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

fn exit_on_recheck_error(error: RecheckError) -> ! {
    let msg = match error {
        RecheckError::TooSlow => "Unhandled exception: recheck was too slow".to_string(),
        RecheckError::Canceled(files) => {
            format!(
                "Unhandled exception: recheck canceled for files: {:?}",
                files
            )
        }
    };
    flow_common_exit_status::exit_with_msg(FlowExitStatus::UnknownError, &msg);
}

fn init(
    profiling: &ProfilingRunning,
    focus_targets: Option<FlowOrdSet<FileKey>>,
    genv: &Genv,
) -> Result<(Env, NodeModulesContainers, Option<String>), RecheckError> {
    // write binary path and version to server log
    flow_hh_logger::info!(
        "executable={}",
        std::env::current_exe()
            .map(|p| p.display().to_string())
            .unwrap_or_default()
    );
    flow_hh_logger::info!("version={}", flow_common::flow_version::VERSION);

    let Some(workers) = genv.workers.as_ref() else {
        flow_common_exit_status::exit_with_msg(
            FlowExitStatus::UnknownError,
            "Unhandled exception: workers are not initialized",
        );
    };
    let options = &genv.options;
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

    extract_flowlibs_or_exit(options);

    let (env, node_modules_containers, first_internal_error) =
        flow_services_inference::type_service::init(
            options,
            workers,
            &genv.shared_mem,
            focus_targets,
        )?;

    sample_init_memory(profiling, &genv.shared_mem);
    flow_event_logger::sharedmem_init_done(genv.shared_mem.heap_size() as u64);
    Ok((env, node_modules_containers, first_internal_error))
}

async fn idle_logging_loop(_options: Arc<Options>, _start_time: f64) {
    let idle_period_in_seconds = 300.0_f64;
    async fn sample(profiling: &ProfilingRunning) {
        let cgroup_stats = cgroup::get_stats();
        match cgroup_stats {
            Err(_) => {}
            Ok(cgroup::Stats {
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
    }

    let should_print_summary = _options.profile;
    loop {
        let profiling = ProfilingRunning::new();
        tokio::select! {
            _ = async {
                loop {
                    tokio::join!(sample(&profiling), tokio::time::sleep(Duration::from_secs(1)));
                }
            } => {}
            _ = tokio::time::sleep(Duration::from_secs_f64(idle_period_in_seconds)) => {}
        }

        let profiling = profiling.finish();
        if should_print_summary {
            flow_hh_logger::info!(
                "Idle: wall duration {:.3}s",
                profiling.get_profiling_duration()
            );
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

async fn gc_loop(shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>) {
    loop {
        tokio::task::yield_now().await;
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
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to create tokio runtime");
    loop {
        monitor_rpc::status_update(server_status::Event::Ready);

        let _options = &_genv.options;

        let _start_time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64();
        runtime.block_on(async {
            let idle_thread = async {
                tokio::join!(
                    idle_logging_loop(Arc::clone(_options), _start_time),
                    gc_loop(Arc::clone(shared_mem)),
                );
            };
            let process_updates = |skip_incompatible: bool, updates: &BTreeSet<String>| {
                flow_server_rechecker::rechecker::process_updates(
                    skip_incompatible,
                    _options,
                    &_env,
                    shared_mem,
                    updates,
                )
            };
            let get_forced = || _env.checked_files.clone();
            let wait_thread = server_monitor_listener_state::wait_for_anything_async(
                &process_updates,
                &get_forced,
            );
            tokio::select! {
                biased;
                _ = wait_thread => {}
                _ = idle_thread => {}
            }
        });

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

    let should_print_summary = _options.profile;
    let (profiling, init_result) = with_profiling("Init", should_print_summary, |profiling| {
        init(profiling, None, &genv_arc)
    });
    let (env, node_modules_containers, first_internal_error) = match init_result {
        Ok(result) => result,
        Err(error) => exit_on_recheck_error(error),
    };
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
    init_id: &str,
    options: Arc<Options>,
    monitor_channels: Option<monitor_rpc::Channels>,
) {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run(init_id, options, monitor_channels);
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

pub fn check_once<'a, FormatErrors>(
    init_id: &str,
    options: Arc<Options>,
    format_errors: FormatErrors,
    focus_targets: Option<FlowOrdSet<FileKey>>,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet)
where
    FormatErrors: for<'b> FnOnce(CheckOnceCollatedErrors<'b>) -> CheckOncePrintErrors<'a>,
{
    flow_daemon::pid_log::disable();
    monitor_rpc::disable();

    flow_event_logger::set_eden(Some(flow_common_vcs::eden::is_eden(&options.root)));
    flow_logging_utils::set_server_options(&options);

    let genv = create_program_init(init_id, Arc::clone(&options));
    let should_print_summary = options.profile;

    let (profiling, init_result) = with_profiling("Init", should_print_summary, |profiling| {
        let (env, _node_modules_containers, first_internal_error) =
            init(profiling, focus_targets, &genv)?;
        let (errors, warnings, suppressed_errors) = error_collator::get(&env);

        let print_errors = profiling.with_timer("FormatErrors", || {
            let shared_mem = &genv.shared_mem;
            let loc_of_aloc = |aloc: &flow_aloc::ALoc| -> Loc { shared_mem.loc_of_aloc(aloc) };
            let get_ast =
                |file: &FileKey| -> Option<Arc<Program<Loc, Loc>>> { shared_mem.get_ast(file) };
            let strip_root = if options.strip_root {
                Some(options.root.as_path())
            } else {
                None
            };

            let suppressed_errors = if options.include_suppressions {
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
            let collated_errors = (&errors, &warnings, suppressed_errors);
            format_errors(collated_errors)
        });
        Ok((print_errors, errors, warnings, first_internal_error))
    });
    let (print_errors, errors, warnings, first_internal_error) = match init_result {
        Ok(result) => result,
        Err(error) => exit_on_recheck_error(error),
    };

    print_errors(&profiling);

    monitor_rpc::send_telemetry(
        flow_server_env::lsp_prot::TelemetryFromServer::InitSummary {
            duration: profiling.get_profiling_duration(),
        },
    );
    monitor_rpc::status_update(server_status::Event::FinishingUp);

    let saved_state_fetcher = string_of_saved_state_fetcher(&options);

    flow_event_logger::init_done(
        first_internal_error.as_deref(),
        saved_state_fetcher,
        &serde_json::json!({ "duration": profiling.get_profiling_duration() }),
    );

    (errors, warnings)
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
    cli_overrides: &flow_common::cli_overrides::CliOverrides,
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
        cli_overrides,
    )
}
