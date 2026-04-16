/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
pub use flow_server::standalone::LazyStats;
use flow_server_files::server_files_js;

pub struct DaemonizeArgs {
    pub flowconfig_name: String,
    pub no_flowlib: bool,
    pub ignore_version: bool,
    pub include_suppressions: bool,
    pub all: bool,
    pub wait: bool,
    pub no_restart: bool,
    pub lazy_mode: Option<String>,
    pub long_lived_workers: Option<bool>,
    pub max_workers: Option<i32>,
    pub wait_for_recheck: Option<bool>,
    pub file_watcher: Option<String>,
    pub file_watcher_debug: bool,
    pub file_watcher_timeout: Option<u32>,
    pub file_watcher_mergebase_with: Option<String>,
    pub file_watcher_sync_timeout: Option<u32>,
    pub shm_heap_size: Option<u64>,
    pub shm_hash_table_pow: Option<u32>,
    pub profile: bool,
    pub verbose: bool,
    pub server_log_file: String,
    pub monitor_log_file: String,
    pub from: Option<String>,
    pub saved_state_fetcher: Option<SavedStateFetcher>,
    pub saved_state_force_recheck: bool,
    pub saved_state_no_fallback: bool,
    pub saved_state_skip_version_check: bool,
    pub saved_state_verify: bool,
    pub no_cgroup: bool,
    pub root: PathBuf,
    pub temp_dir: String,
}

pub struct StartArgs {
    pub flowconfig_name: String,
    pub signal_ready: bool,
    pub server_log_file: String,
    pub monitor_log_file: String,
    pub wait_for_recheck: Option<bool>,
    pub file_watcher: Option<String>,
    pub file_watcher_debug: bool,
    pub file_watcher_timeout: Option<u32>,
    pub file_watcher_mergebase_with: Option<String>,
    pub file_watcher_sync_timeout: Option<u32>,
    pub shm_heap_size: Option<u64>,
    pub shm_hash_table_pow: Option<u32>,
    pub from: Option<String>,
}

fn append_log_line(
    log_file: &str,
    component: &str,
    root: &Path,
    from: Option<&str>,
    message: &str,
) {
    let timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    let mut line = format!(
        "[{timestamp:.3}] {component}: root={} {message}",
        root.display()
    );
    if let Some(from) = from {
        line.push_str(&format!(" from={from}"));
    }
    line.push('\n');
    if let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_file)
    {
        let _ = file.write_all(line.as_bytes());
    }
}

fn prepare_log_file(log_file: &str) -> Result<(), String> {
    flow_server::server_daemon::try_open_log_file(log_file).map(|_| ())
}

fn cleanup_startup_artifacts_if_owned(
    lock_path: &str,
    socket_path: &str,
    ready_path: &str,
    pids_path: &str,
    child_pid: u32,
) {
    let owns_startup_files = std::fs::read_to_string(pids_path)
        .ok()
        .and_then(|contents| contents.lines().next().map(str::to_owned))
        .and_then(|row| row.split_once('\t').map(|(pid, _reason)| pid.to_owned()))
        .and_then(|pid| pid.parse::<u32>().ok())
        == Some(child_pid);
    if owns_startup_files {
        let _ = std::fs::remove_file(pids_path);
        let _ = std::fs::remove_file(lock_path);
        let _ = std::fs::remove_file(socket_path);
        let _ = std::fs::remove_file(ready_path);
    }
}

fn exit_status_message(status: ExitStatus) -> String {
    if let Some(code) = status.code() {
        return format!("server exited with code {}", code);
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;

        if let Some(signal) = status.signal() {
            return format!("server was terminated by signal {}", signal);
        }
    }
    "server exited unsuccessfully".to_string()
}

fn wait_for_child_exit(child: &mut std::process::Child) -> Result<Option<ExitStatus>, String> {
    child
        .try_wait()
        .map_err(|e| format!("failed to wait for server process: {}", e))
}

pub fn daemonize(args: DaemonizeArgs) -> Result<u32, String> {
    let DaemonizeArgs {
        flowconfig_name,
        no_flowlib,
        ignore_version,
        include_suppressions,
        all,
        wait,
        no_restart,
        lazy_mode,
        long_lived_workers,
        max_workers,
        wait_for_recheck,
        file_watcher,
        file_watcher_debug,
        file_watcher_timeout,
        file_watcher_mergebase_with,
        file_watcher_sync_timeout,
        shm_heap_size,
        shm_hash_table_pow,
        profile,
        verbose,
        server_log_file,
        monitor_log_file,
        from,
        saved_state_fetcher,
        saved_state_force_recheck,
        saved_state_no_fallback,
        saved_state_skip_version_check,
        saved_state_verify,
        no_cgroup,
        root,
        temp_dir,
    } = args;
    // Keep the monitor cleanup paths in the same namespace the child server uses.
    let root = root.canonicalize().unwrap_or(root);
    let lock_path = server_files_js::lock_file(&flowconfig_name, &temp_dir, &root);
    if Path::new(&lock_path).exists() {
        return Err(format!(
            "There is already a server running for {}",
            root.display()
        ));
    }

    prepare_log_file(&monitor_log_file)?;

    let pids_path = server_files_js::pids_file(&flowconfig_name, &temp_dir, &root);
    let socket_path = server_files_js::socket_file(&flowconfig_name, &temp_dir, &root);
    let ready_path = server_files_js::ready_file(&flowconfig_name, &temp_dir, &root);
    let _ = std::fs::remove_file(&socket_path);
    let _ = std::fs::remove_file(&ready_path);

    let log_file = flow_server::server_daemon::try_open_log_file(&server_log_file)?;
    let log_file_err = log_file
        .try_clone()
        .map_err(|e| format!("failed to clone log file {}: {}", server_log_file, e))?;
    let exe = std::env::args_os()
        .next()
        .ok_or_else(|| "failed to get argv[0]".to_string())?;

    let mut cmd = std::process::Command::new(&exe);
    cmd.arg("server");
    if no_flowlib {
        cmd.arg("--no-flowlib");
    }
    if ignore_version {
        cmd.arg("--ignore-version");
    }
    if include_suppressions {
        cmd.arg("--include-suppressed");
    }
    if all {
        cmd.arg("--all");
    }
    if no_restart {
        cmd.arg("--no-auto-restart");
    }
    if let Some(mode) = lazy_mode {
        cmd.arg("--lazy-mode").arg(mode);
    }
    if let Some(long_lived_workers) = long_lived_workers {
        cmd.arg("--long-lived-workers")
            .arg(long_lived_workers.to_string());
    }
    if let Some(max_workers) = max_workers {
        cmd.arg("--max-workers").arg(max_workers.to_string());
    }
    if let Some(wait_for_recheck) = wait_for_recheck {
        cmd.arg("--wait-for-recheck")
            .arg(wait_for_recheck.to_string());
    }
    if let Some(file_watcher) = file_watcher {
        cmd.arg("--file-watcher").arg(file_watcher);
    }
    if file_watcher_debug {
        cmd.arg("--file-watcher-debug");
    }
    if let Some(file_watcher_timeout) = file_watcher_timeout {
        cmd.arg("--file-watcher-timeout")
            .arg(file_watcher_timeout.to_string());
    }
    if let Some(file_watcher_mergebase_with) = file_watcher_mergebase_with {
        cmd.arg("--file-watcher-mergebase-with")
            .arg(file_watcher_mergebase_with);
    }
    if let Some(file_watcher_sync_timeout) = file_watcher_sync_timeout {
        cmd.arg("--file-watcher-sync-timeout")
            .arg(file_watcher_sync_timeout.to_string());
    }
    if let Some(shm_heap_size) = shm_heap_size {
        cmd.arg("--sharedmemory-heap-size")
            .arg(shm_heap_size.to_string());
    }
    if let Some(shm_hash_table_pow) = shm_hash_table_pow {
        cmd.arg("--sharedmemory-hash-table-pow")
            .arg(shm_hash_table_pow.to_string());
    }
    if profile {
        cmd.arg("--profile");
    }
    if verbose {
        cmd.arg("--verbose");
    }
    if let Some(from) = from.as_deref() {
        cmd.arg("--from").arg(from);
    }
    if let Some(saved_state_fetcher) = saved_state_fetcher {
        let saved_state_fetcher = match saved_state_fetcher {
            SavedStateFetcher::DummyFetcher => "none",
            SavedStateFetcher::LocalFetcher => "local",
            SavedStateFetcher::ScmFetcher => "scm",
            SavedStateFetcher::FbFetcher => "fb",
        };
        cmd.arg("--saved-state-fetcher").arg(saved_state_fetcher);
    }
    if saved_state_force_recheck {
        cmd.arg("--saved-state-force-recheck");
    }
    if saved_state_no_fallback {
        cmd.arg("--saved-state-no-fallback");
    }
    if saved_state_skip_version_check {
        cmd.arg("--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED");
    }
    if saved_state_verify {
        cmd.arg("--saved-state-verify");
    }
    if no_cgroup {
        cmd.arg("--no-cgroup");
    }
    cmd.arg("--flowconfig-name").arg(&flowconfig_name);
    cmd.arg("--log-file").arg(&server_log_file);
    cmd.arg("--monitor-log-file").arg(&monitor_log_file);
    cmd.arg("--temp-dir").arg(&temp_dir);
    if wait {
        cmd.env("FLOW_SERVER_SIGNAL_READY", "1");
    }
    cmd.arg(root.to_string_lossy().as_ref());
    cmd.stdin(std::process::Stdio::null());
    cmd.stdout(std::process::Stdio::from(log_file));
    cmd.stderr(std::process::Stdio::from(log_file_err));

    let mut child = cmd
        .spawn()
        .map_err(|e| format!("failed to spawn server: {}", e))?;
    let pid = child.id();

    if !wait {
        loop {
            if Path::new(&lock_path).exists() {
                if let Some(status) = wait_for_child_exit(&mut child)? {
                    let message = exit_status_message(status);
                    cleanup_startup_artifacts_if_owned(
                        &lock_path,
                        &socket_path,
                        &ready_path,
                        &pids_path,
                        pid,
                    );
                    append_log_line(
                        &monitor_log_file,
                        "monitor",
                        &root,
                        from.as_deref(),
                        &format!("server exited after acquiring lock: {message}"),
                    );
                    return Err(format!("server exited after acquiring lock: {message}"));
                }
                std::thread::sleep(Duration::from_millis(50));
                if let Some(status) = wait_for_child_exit(&mut child)? {
                    let message = exit_status_message(status);
                    cleanup_startup_artifacts_if_owned(
                        &lock_path,
                        &socket_path,
                        &ready_path,
                        &pids_path,
                        pid,
                    );
                    append_log_line(
                        &monitor_log_file,
                        "monitor",
                        &root,
                        from.as_deref(),
                        &format!("server exited immediately after acquiring lock: {message}"),
                    );
                    return Err(format!(
                        "server exited immediately after acquiring lock: {message}"
                    ));
                }
                append_log_line(
                    &monitor_log_file,
                    "monitor",
                    &root,
                    from.as_deref(),
                    "server child acquired lock; returning without waiting for readiness",
                );
                return Ok(pid);
            }
            if let Some(status) = wait_for_child_exit(&mut child)? {
                let message = exit_status_message(status);
                cleanup_startup_artifacts_if_owned(
                    &lock_path,
                    &socket_path,
                    &ready_path,
                    &pids_path,
                    pid,
                );
                append_log_line(
                    &monitor_log_file,
                    "monitor",
                    &root,
                    from.as_deref(),
                    &format!("server exited before acquiring lock: {message}"),
                );
                return Err(format!("server exited before acquiring lock: {message}"));
            }
            std::thread::sleep(Duration::from_millis(50));
        }
    }

    if wait {
        loop {
            if Path::new(&ready_path).exists() {
                let _ = std::fs::remove_file(&ready_path);
                if let Some(status) = wait_for_child_exit(&mut child)? {
                    let message = exit_status_message(status);
                    cleanup_startup_artifacts_if_owned(
                        &lock_path,
                        &socket_path,
                        &ready_path,
                        &pids_path,
                        pid,
                    );
                    append_log_line(
                        &monitor_log_file,
                        "monitor",
                        &root,
                        from.as_deref(),
                        &format!("server exited after signaling readiness: {message}"),
                    );
                    return Err(format!(
                        "server exited after signaling readiness: {message}"
                    ));
                }
                std::thread::sleep(Duration::from_millis(50));
                if let Some(status) = wait_for_child_exit(&mut child)? {
                    let message = exit_status_message(status);
                    cleanup_startup_artifacts_if_owned(
                        &lock_path,
                        &socket_path,
                        &ready_path,
                        &pids_path,
                        pid,
                    );
                    append_log_line(
                        &monitor_log_file,
                        "monitor",
                        &root,
                        from.as_deref(),
                        &format!("server exited immediately after signaling readiness: {message}"),
                    );
                    return Err(format!(
                        "server exited immediately after signaling readiness: {message}"
                    ));
                }
                append_log_line(
                    &monitor_log_file,
                    "monitor",
                    &root,
                    from.as_deref(),
                    "server child signaled readiness",
                );
                break;
            }
            if let Some(status) = wait_for_child_exit(&mut child)? {
                let message = exit_status_message(status);
                cleanup_startup_artifacts_if_owned(
                    &lock_path,
                    &socket_path,
                    &ready_path,
                    &pids_path,
                    pid,
                );
                append_log_line(
                    &monitor_log_file,
                    "monitor",
                    &root,
                    from.as_deref(),
                    &format!("server exited before signaling readiness: {message}"),
                );
                return Err(format!(
                    "server exited before signaling readiness: {message}"
                ));
            }
            std::thread::sleep(Duration::from_millis(50));
        }
    }

    Ok(pid)
}

pub fn start(options: Arc<Options>, args: StartArgs) -> Result<(), String> {
    let lock_path = server_files_js::lock_file(
        &args.flowconfig_name,
        options.temp_dir.as_str(),
        options.root.as_path(),
    );
    if Path::new(&lock_path).exists() {
        return Err(format!(
            "There is already a server running for {}",
            options.root.display()
        ));
    }
    prepare_log_file(&args.monitor_log_file)?;
    prepare_log_file(&args.server_log_file)?;
    flow_server::standalone::start(
        options,
        args.flowconfig_name,
        args.signal_ready,
        args.shm_heap_size,
        args.shm_hash_table_pow,
    );
    Ok(())
}
