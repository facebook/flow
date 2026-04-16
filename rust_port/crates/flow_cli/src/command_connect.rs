/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::time::Duration;
use std::time::Instant;

use flow_common::flow_version;
use flow_server_env::server_socket_rpc::ServerRequest;
use flow_server_env::server_socket_rpc::ServerResponse;

use crate::command_connect_simple as CCS;
use crate::command_connect_simple::BusyReason;
use crate::command_connect_simple::CCSError;
use crate::command_connect_simple::MismatchBehavior;

pub(crate) struct Env<'a> {
    pub(crate) root: &'a Path,
    pub(crate) autostart: bool,
    pub(crate) retries: i32,
    pub(crate) expiry: Option<Instant>,
    pub(crate) lazy_mode: Option<&'a str>,
    pub(crate) autostop: bool,
    pub(crate) tmp_dir: &'a str,
    pub(crate) shm_hash_table_pow: Option<u32>,
    pub(crate) ignore_version: bool,
    #[allow(dead_code)]
    pub(crate) emoji: bool,
    pub(crate) quiet: bool,
    pub(crate) flowconfig_name: &'a str,
    pub(crate) rerun_on_mismatch: bool,
}

fn arg(name: &str, value: Option<&str>, arr: &mut Vec<String>) {
    if let Some(value) = value {
        arr.push(name.to_string());
        arr.push(value.to_string());
    }
}

fn arg_map<T, F: Fn(&T) -> String>(name: &str, f: F, value: Option<&T>, arr: &mut Vec<String>) {
    let value = value.map(f);
    arg(name, value.as_deref(), arr);
}

fn flag(name: &str, value: bool, arr: &mut Vec<String>) {
    if value {
        arr.push(name.to_string());
    }
}

// Starts up a flow server by literally calling flow start
fn start_flow_server(env: &Env) -> Result<(), (String, flow_common_exit_status::FlowExitStatus)> {
    let Env {
        tmp_dir,
        lazy_mode,
        autostop,
        shm_hash_table_pow,
        ignore_version,
        root,
        quiet,
        flowconfig_name,
        ..
    } = env;

    let exe = std::env::current_exe()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| {
            std::env::args()
                .next()
                .unwrap_or_else(|| "flow".to_string())
        });

    let root_str = root.to_string_lossy().to_string();
    let mut args = vec!["start".to_string(), root_str];
    arg_map(
        "--sharedmemory-hash-table-pow",
        |v: &u32| v.to_string(),
        shm_hash_table_pow.as_ref(),
        &mut args,
    );
    arg("--lazy-mode", *lazy_mode, &mut args);
    arg("--temp-dir", Some(tmp_dir), &mut args);
    let from = crate::flow_event_logger::get_from_i_am_a_clown();
    arg("--from", from.as_deref(), &mut args);
    flag("--ignore-version", *ignore_version, &mut args);
    flag("--quiet", *quiet, &mut args);
    flag("--autostop", *autostop, &mut args);
    arg("--flowconfig-name", Some(flowconfig_name), &mut args);

    use std::process::Command;
    match Command::new(&exe)
        .args(&args)
        .stdin(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .status()
    {
        Ok(status) => {
            if status.success() {
                Ok(())
            } else if let Some(code) = status.code() {
                if code
                    == flow_common_exit_status::error_code(
                        flow_common_exit_status::FlowExitStatus::LockStolen,
                    )
                {
                    Err((
                        "Lock stolen".to_string(),
                        flow_common_exit_status::FlowExitStatus::LockStolen,
                    ))
                } else {
                    Err((
                        "Could not start Flow server!".to_string(),
                        flow_common_exit_status::FlowExitStatus::ServerStartFailed,
                    ))
                }
            } else {
                Err((
                    "Could not start Flow server!".to_string(),
                    flow_common_exit_status::FlowExitStatus::ServerStartFailed,
                ))
            }
        }
        Err(exn) => Err((
            format!("Could not start Flow server! Unexpected exception: {}", exn),
            flow_common_exit_status::FlowExitStatus::UnknownError,
        )),
    }
}

struct RetryInfo {
    retries_remaining: i32,
    original_retries: i32,
    last_connect_time: Instant,
}

fn reset_retries_if_necessary(retries: &mut RetryInfo, conn: &Result<ServerResponse, CCSError>) {
    match conn {
        Err(CCSError::ServerBusy(BusyReason::FailOnInit(..))) => {
            retries.retries_remaining = 0;
        }
        Err(CCSError::ServerMissing) | Err(CCSError::ServerBusy(_)) => {}
        Ok(_) | Err(CCSError::ServerSocketMissing) | Err(CCSError::BuildIdMismatch(_)) => {
            retries.retries_remaining = retries.original_retries;
        }
    }
}

fn rate_limit(retries: &RetryInfo) {
    // Make sure there is at least 1 second between retries
    let elapsed = retries.last_connect_time.elapsed();
    if elapsed < Duration::from_secs(1) {
        std::thread::sleep(Duration::from_secs(1) - elapsed);
    }
}

fn consume_retry(retries: &mut RetryInfo) {
    retries.retries_remaining -= 1;
    if retries.retries_remaining >= 0 {
        rate_limit(retries);
    }
}

// A featureful wrapper around CommandConnectSimple.connect_once. This
// function handles retries, timeouts, displaying messages during
// initialization, etc
fn connect_rec(env: &Env, request: &ServerRequest, retries: &mut RetryInfo) -> ServerResponse {
    if retries.retries_remaining < 0 {
        eprintln!("\nOut of retries, exiting!");
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::OutOfRetries);
    }

    let has_timed_out = match env.expiry {
        None => false,
        Some(t) => Instant::now() >= t,
    };
    if has_timed_out {
        eprintln!("\nTimeout exceeded, exiting");
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::OutOfTime);
    }

    retries.last_connect_time = Instant::now();

    let conn = CCS::connect_once(env.flowconfig_name, env.tmp_dir, env.root, request, Some(1));

    reset_retries_if_necessary(retries, &conn);

    match conn {
        Ok(response) => response,

        Err(CCSError::ServerMissing) => handle_missing_server(env, request, retries),

        Err(CCSError::ServerBusy(busy_reason)) => {
            let busy_reason_str = match &busy_reason {
                BusyReason::TooManyClients => "has too many clients and rejected our connection",
                BusyReason::NotResponding => "is not responding",
                BusyReason::FailOnInit(..) => {
                    "is still initializing and the client used --retry-if-init false"
                }
            };
            if !env.quiet {
                eprint!(
                    "The flow server {} [{}] ({} {} remaining): ...",
                    busy_reason_str,
                    CCS::busy_reason_to_string(&busy_reason),
                    retries.retries_remaining,
                    if retries.retries_remaining == 1 {
                        "retry"
                    } else {
                        "retries"
                    },
                );
            }
            consume_retry(retries);
            connect_rec(env, request, retries)
        }

        Err(CCSError::BuildIdMismatch(MismatchBehavior::ServerExited)) => {
            let msg = "The flow server's version didn't match the client's, so it exited.";
            if env.autostart {
                if !env.quiet {
                    eprintln!("{}\nGoing to launch a new one.\n", msg);
                }
                // Don't decrement retries -- the server is definitely not running,
                // so the next time round will hit Server_missing above, *but*
                // before that will actually start the server -- we need to make
                // sure that happens.
                connect_rec(env, request, retries)
            } else {
                let msg = format!("\n{}", msg);
                eprintln!("{}", msg);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
                );
            }
        }

        Err(CCSError::BuildIdMismatch(MismatchBehavior::ClientShouldError {
            server_bin,
            server_version,
        })) => {
            if env.rerun_on_mismatch {
                if !env.quiet {
                    eprintln!(
                        "Version mismatch! Server binary is Flow v{} but we are using v{}",
                        server_version,
                        flow_version::VERSION,
                    );
                    eprintln!("Restarting command using the same binary as the server");
                }
                let args: Vec<String> = std::env::args().skip(1).collect();
                match std::process::Command::new(&server_bin).args(&args).status() {
                    Ok(status) => {
                        std::process::exit(status.code().unwrap_or(1));
                    }
                    Err(e) => {
                        eprintln!("Failed to exec {}: {}", server_bin, e);
                        flow_common_exit_status::exit(
                            flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
                        );
                    }
                }
            } else {
                let msg = format!(
                    "\nThe Flow server's version (v{}) didn't match the client's (v{}). Exiting",
                    server_version,
                    flow_version::VERSION,
                );
                eprintln!("{}", msg);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
                );
            }
        }

        Err(CCSError::ServerSocketMissing) => {
            if !env.quiet {
                eprintln!("Attempting to kill server for `{}`", env.root.display());
            }
            match crate::command_mean_kill::mean_kill(env.flowconfig_name, env.tmp_dir, env.root) {
                Ok(()) => {
                    if !env.quiet {
                        eprintln!("Successfully killed server for `{}`", env.root.display());
                    }
                    handle_missing_server(env, request, retries)
                }
                Err(crate::command_mean_kill::FailedToKill::Message(err)) => {
                    if !env.quiet {
                        match err {
                            Some(err) => eprintln!("{}", err),
                            None => {}
                        }
                    }
                    let msg = format!("Failed to kill server for `{}`", env.root.display());
                    eprintln!("{}", msg);
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::KillError,
                    );
                }
            }
        }
    }
}

fn handle_missing_server(
    env: &Env,
    request: &ServerRequest,
    retries: &mut RetryInfo,
) -> ServerResponse {
    if env.autostart {
        if !env.quiet {
            eprintln!("Launching Flow server for {}", env.root.display());
        }
        match start_flow_server(env) {
            Ok(()) => {
                if !env.quiet {
                    eprint!("Started a new flow server: ...");
                }
            }
            Err((_, flow_common_exit_status::FlowExitStatus::LockStolen)) => {
                if !env.quiet {
                    eprint!(
                        "Failed to start a new flow server ({} {} remaining): ...",
                        retries.retries_remaining,
                        if retries.retries_remaining == 1 {
                            "retry"
                        } else {
                            "retries"
                        },
                    );
                }
                consume_retry(retries);
            }
            Err((msg, code)) => {
                eprintln!("{}", msg);
                flow_common_exit_status::exit(code);
            }
        }
        connect_rec(env, request, retries)
    } else {
        let msg = format!(
            "\nError: There is no Flow server running in '{}'.",
            env.root.display()
        );
        eprintln!("{}", msg);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoServerRunning);
    }
}

pub(crate) fn connect(env: &Env, request: &ServerRequest) -> ServerResponse {
    let mut retries = RetryInfo {
        retries_remaining: env.retries,
        original_retries: env.retries,
        last_connect_time: Instant::now(),
    };
    connect_rec(env, request, &mut retries)
}
