/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::Path;
use std::sync::Mutex;
use std::sync::OnceLock;

use flow_common::sys_utils;

fn log_oc() -> &'static Mutex<Option<File>> {
    static LOG_OC: OnceLock<Mutex<Option<File>>> = OnceLock::new();
    LOG_OC.get_or_init(|| Mutex::new(None))
}

fn enabled() -> &'static Mutex<bool> {
    static ENABLED: OnceLock<Mutex<bool>> = OnceLock::new();
    ENABLED.get_or_init(|| Mutex::new(true))
}

pub fn disable() {
    *enabled().lock().expect("pid_log enabled mutex poisoned") = false;
}

pub fn init(pids_file: &Path) -> std::io::Result<()> {
    let mut guard = log_oc().lock().expect("pid_log log_oc mutex poisoned");
    assert!(guard.is_none(), "PidLog::init called twice");
    sys_utils::with_umask(0o111, || -> std::io::Result<()> {
        if let Some(parent) = pids_file.parent() {
            sys_utils::mkdir_no_fail(parent)?;
        }
        let oc = fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(pids_file)?;
        *guard = Some(oc);
        Ok(())
    })
}

pub fn log(reason: Option<&str>, no_fail: bool, pid: u32) {
    if !*enabled().lock().expect("pid_log enabled mutex poisoned") {
        return;
    }
    let pid = sys_utils::pid_of_handle(pid);
    let reason = reason.unwrap_or("unknown");
    let mut guard = log_oc().lock().expect("pid_log log_oc mutex poisoned");
    match guard.as_mut() {
        None if no_fail => {}
        None => panic!("Can't write pid to uninitialized pids log"),
        Some(oc) => {
            writeln!(oc, "{}\t{}", pid, reason).expect("PidLog::log: failed to write");
            oc.flush().expect("PidLog::log: failed to flush");
        }
    }
}

#[derive(Debug)]
pub struct FailedToGetPids;

impl std::fmt::Display for FailedToGetPids {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FailedToGetPids")
    }
}

impl std::error::Error for FailedToGetPids {}

pub fn get_pids(pids_file: &Path) -> Result<Vec<(u32, String)>, FailedToGetPids> {
    let ic = File::open(pids_file).map_err(|_| FailedToGetPids)?;
    let reader = BufReader::new(ic);
    let mut results: Vec<(u32, String)> = Vec::new();
    for line in reader.lines() {
        let row = match line {
            Ok(row) => row,
            Err(_) => break,
        };
        let mut parts = row.splitn(2, '\t');
        let Some(pid_str) = parts.next() else {
            continue;
        };
        let Some(reason) = parts.next() else {
            continue;
        };
        let Ok(pid) = pid_str.parse::<u32>() else {
            continue;
        };
        results.push((pid, reason.to_string()));
    }
    Ok(results)
}

pub fn close() -> std::io::Result<()> {
    let mut guard = log_oc().lock().expect("pid_log log_oc mutex poisoned");
    if let Some(mut oc) = guard.take() {
        oc.flush()?;
        drop(oc);
    }
    Ok(())
}
