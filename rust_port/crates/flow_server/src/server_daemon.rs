/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use flow_common::options::Options;
use flow_common_exit_status::FlowExitStatus;
use flow_server_files::server_files_js;

mod pid_log {
    use std::fs;
    use std::io::Write;
    use std::path::Path;
    use std::sync::Mutex;

    static LOG_OC: Mutex<Option<fs::File>> = Mutex::new(None);
    static ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

    pub fn init(pids_file: &str) {
        let mut log_oc = LOG_OC.lock().unwrap();
        assert!(log_oc.is_none());
        if let Some(parent) = Path::new(pids_file).parent() {
            let _mkdir_result = fs::create_dir_all(parent);
        }
        let file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(pids_file)
            .unwrap_or_else(|e| panic!("Failed to open pids file '{}': {}", pids_file, e));
        *log_oc = Some(file);
    }

    pub fn log(reason: &str, no_fail: bool, pid: u32) {
        if !ENABLED.load(std::sync::atomic::Ordering::SeqCst) {
            return;
        }
        let mut log_oc = LOG_OC.lock().unwrap();
        match log_oc.as_mut() {
            None if no_fail => {}
            None => panic!("Can't write pid to uninitialized pids log"),
            Some(oc) => {
                write!(oc, "{}\t{}\n", pid, reason).expect("failed to write pid log");
                oc.flush().expect("failed to flush pid log");
            }
        }
    }
}

fn hh_logger_level_of_env(env: &str) -> Option<log::LevelFilter> {
    match std::env::var(env).ok().as_deref() {
        Some("off") => Some(log::LevelFilter::Off),
        Some("fatal") => Some(log::LevelFilter::Error),
        Some("error") => Some(log::LevelFilter::Error),
        Some("warn") => Some(log::LevelFilter::Warn),
        Some("info") => Some(log::LevelFilter::Info),
        Some("debug") => Some(log::LevelFilter::Debug),
        Some(_) | None => None,
    }
}

fn set_hh_logger_min_level(options: &Options) {
    let level = if options.quiet {
        log::LevelFilter::Off
    } else if options.verbose.is_some() || options.debug {
        log::LevelFilter::Debug
    } else {
        match hh_logger_level_of_env("FLOW_LOG_LEVEL") {
            Some(level) => level,
            None => log::LevelFilter::Info,
        }
    };
    log::set_max_level(level);
}

fn dump_server_options(options: &Options) {
    let lazy_mode = if options.lazy_mode { "on" } else { "off" };
    log::info!("lazy_mode={}", lazy_mode);
    log::info!("max_workers={}", options.max_workers);
    log::info!("long_lived_workers={}", options.long_lived_workers);
    log::info!("debug={}", options.debug);
    for (method_name, log_saving) in options.log_saving.iter() {
        let limit_str = match log_saving.limit {
            None => "null".to_string(),
            Some(limit) => format!("{}", limit),
        };
        log::info!(
            "{} threshold_time_ms={} limit={} rate={}",
            method_name,
            log_saving.threshold_time_ms,
            limit_str,
            log_saving.rate
        );
    }
    for (r, g) in options.enabled_rollouts.iter() {
        log::info!("Rollout {:?} set to {:?}", r, g);
    }
}

fn acquire_lock(lock_file: &str) -> std::io::Result<fs::File> {
    if let Some(parent) = Path::new(lock_file).parent() {
        fs::create_dir_all(parent)?;
    }
    let file = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_file)?;
    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(fs::TryLockError::WouldBlock) => Err(std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            "lock file is already held",
        )),
        Err(err) => Err(err.into()),
    }
}

pub struct Args {
    pub options: Arc<Options>,
    pub init_id: String,
    pub argv: Vec<String>,
    pub parent_pid: u32,
    pub parent_logger_pid: Option<u32>,
    pub file_watcher_pid: Option<u32>,
}

pub type EntryPoint = Box<dyn Fn(Args) + Send + Sync>;

pub fn try_open_log_file(file: &str) -> Result<fs::File, String> {
    // Not a huge problem, we just need to be more intentional
    if Path::new(file).exists() {
        let old_file = format!("{}.old", file);
        if let Err(e) = (|| -> std::io::Result<()> {
            if Path::new(&old_file).exists() {
                fs::remove_file(&old_file)?;
            }
            fs::rename(file, &old_file)?;
            Ok(())
        })() {
            eprintln!(
                "Log rotate: failed to move '{}' to '{}'\n{}",
                file, old_file, e
            );
        }
    }
    fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(file)
        .map_err(|e| format!("Failed to open log file '{}': {}", file, e))
}

pub fn open_log_file(file: &str) -> fs::File {
    try_open_log_file(file).unwrap_or_else(|e| panic!("{}", e))
}

fn new_entry_point() -> String {
    static CPT: AtomicUsize = AtomicUsize::new(0);
    let n = CPT.fetch_add(1, Ordering::SeqCst) + 1;
    format!("main_{}", n)
}

pub fn register_entry_point(
    main: impl Fn(&str, Arc<Options>) + Send + Sync + 'static,
) -> EntryPoint {
    let _name = new_entry_point();
    Box::new(move |args: Args| {
        let Args {
            options,
            init_id,
            argv,
            parent_pid,
            parent_logger_pid,
            file_watcher_pid,
        } = args;

        set_hh_logger_min_level(&options);
        log::info!("argv={}", argv.join(" "));
        dump_server_options(&options);

        let root = &options.root;
        let tmp_dir = &options.temp_dir;
        let flowconfig_name = &options.flowconfig_name;
        let pids_file = server_files_js::pids_file(flowconfig_name, tmp_dir, root);
        pid_log::init(&pids_file);
        pid_log::log("monitor", false, parent_pid);
        if let Some(pid) = parent_logger_pid {
            pid_log::log("monitor_logger", false, pid);
        }
        if let Some(pid) = file_watcher_pid {
            pid_log::log("file_watcher", false, pid);
        }
        pid_log::log("main", false, std::process::id());

        main(&init_id, options);
    })
}

pub fn daemonize(
    _init_id: &str,
    log_file: &str,
    _argv: &[String],
    options: Arc<Options>,
    _file_watcher_pid: Option<u32>,
    _main_entry: &EntryPoint,
) {
    // Let's make sure this isn't all for naught before we fork
    let root = &options.root;
    let tmp_dir = &options.temp_dir;
    let flowconfig_name = &options.flowconfig_name;
    let lock = server_files_js::lock_file(flowconfig_name, tmp_dir, root);
    let _lock_file = match acquire_lock(&lock) {
        Ok(file) => file,
        Err(_) => {
            let msg = format!(
                "Error: There is already a server running for {}",
                root.display()
            );
            eprintln!("{}", msg);
            flow_common_exit_status::exit(FlowExitStatus::LockStolen);
        }
    };

    let _log_fd = open_log_file(log_file);

    // implementation of OCaml does leak stdout and stderr. This means any process
    // that waits for `flow start`'s stdout and stderr to close might wait forever.
    // stdout and stderr and that seems to solve things. However, that call fails
    // on Windows 7. After poking around for a few hours, I can't think of a
    // So for now let's make Windows 7 not crash. It seems like `flow start` on
    // Windows 7 doesn't actually leak stdio, so a no op is acceptable.

    let _name = format!("server master process watching {}", root.display());
    let args = Args {
        options,
        init_id: _init_id.to_string(),
        argv: _argv.to_vec(),
        parent_pid: std::process::id(),
        parent_logger_pid: None,
        file_watcher_pid: _file_watcher_pid,
    };
    _main_entry(args);
}
