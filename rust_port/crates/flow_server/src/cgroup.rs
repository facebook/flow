/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

pub struct Stats {
    pub total: i64,
    pub total_swap: i64,
    pub anon: i64,
    pub shmem: i64,
    pub file: i64,
}

fn read_proc_file(filename: &str, pid: u32) -> Result<String, String> {
    let file = format!("/proc/{}/{}", pid, filename);
    std::fs::read_to_string(&file).map_err(|e| format!("{}", e))
}

fn parse_cgroup(raw_cgroup_contents: &str) -> Result<String, String> {
    let lines: Vec<&str> = raw_cgroup_contents.split('\n').collect();
    match lines.first() {
        None => Err("Expected at least one cgroup in /proc/<PID>/cgroup file".to_string()),
        Some(first_line) => {
            let parts: Vec<&str> = first_line.split(':').collect();
            if parts.len() == 3 {
                Ok(parts[2].to_string())
            } else {
                Err(
                    "First line of  /proc/<PID>/cgroup file was not correctly formatted"
                        .to_string(),
                )
            }
        }
    }
}

fn assert_procfs_supported() -> Result<(), String> {
    static RESULT: OnceLock<Result<(), String>> = OnceLock::new();
    RESULT
        .get_or_init(|| {
            if cfg!(unix) && std::path::Path::new("/proc").exists() {
                Ok(())
            } else {
                Err("Proc filesystem not supported".to_string())
            }
        })
        .clone()
}

fn first_cgroup_for_pid(pid: u32) -> Result<String, String> {
    assert_procfs_supported()?;
    let contents = read_proc_file("cgroup", pid)?;
    parse_cgroup(&contents)
}

const CGROUP_DIR: &str = "/sys/fs/cgroup";

fn assert_is_using_cgroup_v2() -> Result<(), String> {
    static RESULT: OnceLock<Result<(), String>> = OnceLock::new();
    RESULT
        .get_or_init(|| {
            if std::path::Path::new(CGROUP_DIR).exists() {
                let memory_path = format!("{}/memory", CGROUP_DIR);
                if std::path::Path::new(&memory_path).exists() {
                    Err(format!(
                        "cgroup v1 is mounted at {}. We need v2",
                        CGROUP_DIR
                    ))
                } else {
                    Ok(())
                }
            } else {
                Err(format!("{} doesn't exist", CGROUP_DIR))
            }
        })
        .clone()
}

fn get_cgroup_name() -> Result<String, String> {
    static CACHE: Mutex<Option<(f64, Result<String, String>)>> = Mutex::new(None);
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    let mut cache = CACHE.lock().unwrap();
    if let Some((good_until, ref result)) = *cache {
        if now < good_until {
            return result.clone();
        }
    }
    let result = first_cgroup_for_pid(std::process::id());
    *cache = Some((now + 5.0, result.clone()));
    result
}

fn cat(file: &str) -> Result<String, String> {
    std::fs::read_to_string(file).map_err(|e| {
        if e.kind() == std::io::ErrorKind::NotFound {
            "File doesn't exist".to_string()
        } else {
            format!("{}", e)
        }
    })
}

fn read_single_number_file(path: &str) -> Result<i64, String> {
    let contents = cat(path)?;
    contents
        .trim()
        .parse::<i64>()
        .map_err(|_| "Failed to parse memory.current".to_string())
}

fn parse_stat(stat_contents: &str) -> Result<(i64, i64, i64), String> {
    let stats: BTreeMap<&str, i64> =
        stat_contents
            .split('\n')
            .fold(BTreeMap::new(), |mut stats, line| {
                let parts: Vec<&str> = line.split(' ').collect();
                if parts.len() == 2 {
                    if let Ok(stat) = parts[1].parse::<i64>() {
                        stats.insert(parts[0], stat);
                    }
                }
                stats
            });
    let get = |key: &str| -> Result<i64, String> {
        stats
            .get(key)
            .copied()
            .ok_or_else(|| format!("Failed to find {:?} in memory.stat", key))
    };
    let anon = get("anon")?;
    let file = get("file")?;
    let shmem = get("shmem")?;
    Ok((anon, file - shmem, shmem))
}

// cgroup_name starts with a /, like /my_cgroup
fn get_stats_for_cgroup(cgroup_name: &str) -> Result<Stats, String> {
    let dir = format!("{}{}", CGROUP_DIR, cgroup_name);
    let total = read_single_number_file(&format!("{}/memory.current", dir))?;
    let total_swap = read_single_number_file(&format!("{}/memory.swap.current", dir))?;
    let stat_contents = cat(&format!("{}/memory.stat", dir))?;
    let (anon, file, shmem) = parse_stat(&stat_contents)?;
    Ok(Stats {
        total,
        total_swap,
        anon,
        file,
        shmem,
    })
}

pub fn get_stats() -> Result<Stats, String> {
    assert_is_using_cgroup_v2()?;
    let cgroup_name = get_cgroup_name()?;
    get_stats_for_cgroup(&cgroup_name)
}
