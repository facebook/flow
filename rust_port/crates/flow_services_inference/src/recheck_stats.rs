/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::PathBuf;
use std::sync::Mutex;

use flow_common::options::Options;
use flow_server_files::server_files_js;

const PER_FILE_TIME_GUESS: f64 = 0.003;

const PER_MERGE_FILE_TIME_GUESS: f64 = 0.001;

const PER_CHECK_FILE_TIME_GUESS: f64 = 0.005;

const PER_FILE_TIME_KEY: &str = "per_file_time";

const PER_MERGE_FILE_TIME_KEY: &str = "per_merge_file_time";

const PER_CHECK_FILE_TIME_KEY: &str = "per_check_file_time";

const ESTIMATES_KEY: &str = "estimates";

const ESTIMATED_TIME_TO_RECHECK_KEY: &str = "estimated_time_to_recheck";

const ESTIMATED_TIME_TO_RESTART_KEY: &str = "estimated_time_to_restart";

const ESTIMATED_TIME_TO_INIT_KEY: &str = "estimated_time_to_init";

const ESTIMATED_TIME_PER_FILE_KEY: &str = "estimated_time_per_file";

const ESTIMATED_FILES_TO_RECHECK_KEY: &str = "estimated_files_to_recheck";

const ESTIMATED_FILES_TO_INIT_KEY: &str = "estimated_files_to_init";

pub struct Estimates {
    pub estimated_time_to_recheck: f64,
    pub estimated_time_to_restart: f64,
    pub estimated_time_to_init: f64,
    pub estimated_time_per_file: f64,
    pub estimated_files_to_recheck: i64,
    pub estimated_files_to_init: i64,
}

struct Averages {
    init_time: f64,
    per_file_time: f64,
    // per_merge_file_time: float;
    per_merge_file_time: f64,
    // per_check_file_time: float;
    per_check_file_time: f64,
    parsed_count: i64,
}

static AVERAGES: Mutex<Option<Averages>> = Mutex::new(None);

// window should be a positive integer
fn moving_average(window: i64, avg: f64, sample: f64, sample_count: i64) -> f64 {
    let window = window as f64;
    let sample_count = sample_count as f64;
    if sample_count >= window {
        sample
    } else {
        ((avg * (window - sample_count)) + (sample * sample_count)) / window
    }
}

fn get_file(options: &Options) -> PathBuf {
    let root = &options.root;
    let tmp_dir = &options.temp_dir;
    let flowconfig_name = &options.flowconfig_name;
    PathBuf::from(server_files_js::recheck_stats_file(
        flowconfig_name,
        tmp_dir,
        root,
    ))
}

fn load_stats(options: &Options) -> (f64, f64, f64) {
    let file = get_file(options);
    let result: Result<(f64, f64, f64), String> = (|| {
        let contents = fs::read_to_string(&file).map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                "File doesn't exist".to_string()
            } else {
                format!("Failed to open file: {}", e)
            }
        })?;
        let json: serde_json::Value = serde_json::from_str(&contents).map_err(|e| {
            format!(
                "Failed to parse as JSON contents. {:?}: {:?}",
                e.to_string(),
                contents
            )
        })?;
        let per_file_time = json
            .get(PER_FILE_TIME_KEY)
            .and_then(|v| v.as_f64())
            .unwrap_or(PER_FILE_TIME_GUESS);
        let per_merge_file_time = json
            .get(PER_MERGE_FILE_TIME_KEY)
            .and_then(|v| v.as_f64())
            .unwrap_or(PER_MERGE_FILE_TIME_GUESS);
        let per_check_file_time = json
            .get(PER_CHECK_FILE_TIME_KEY)
            .and_then(|v| v.as_f64())
            .unwrap_or(PER_CHECK_FILE_TIME_GUESS);
        Ok((per_file_time, per_merge_file_time, per_check_file_time))
    })();
    match result {
        Ok((per_file_time, per_merge_file_time, per_check_file_time)) => {
            (per_file_time, per_merge_file_time, per_check_file_time)
        }
        Err(reason) => {
            flow_hh_logger::info!(
                "Failed to load recheck stats from {:?}. Reason: {:?}",
                file.display(),
                reason
            );
            (
                PER_FILE_TIME_GUESS,
                PER_MERGE_FILE_TIME_GUESS,
                PER_CHECK_FILE_TIME_GUESS,
            )
        }
    }
}

fn save_averages(options: &Options, estimates: Option<&Estimates>, new_averages: Averages) {
    let per_file_time = new_averages.per_file_time;
    let per_merge_file_time = new_averages.per_merge_file_time;
    let per_check_file_time = new_averages.per_check_file_time;

    {
        let mut guard = AVERAGES.lock().unwrap();
        *guard = Some(new_averages);
    }

    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        PER_FILE_TIME_KEY.to_string(),
        serde_json::Value::from(per_file_time),
    );
    json_obj.insert(
        PER_MERGE_FILE_TIME_KEY.to_string(),
        serde_json::Value::from(per_merge_file_time),
    );
    json_obj.insert(
        PER_CHECK_FILE_TIME_KEY.to_string(),
        serde_json::Value::from(per_check_file_time),
    );

    if let Some(Estimates {
        estimated_time_to_recheck,
        estimated_time_to_restart,
        estimated_time_to_init,
        estimated_time_per_file,
        estimated_files_to_recheck,
        estimated_files_to_init,
    }) = estimates
    {
        let mut estimates_obj = serde_json::Map::new();
        estimates_obj.insert(
            ESTIMATED_TIME_TO_RECHECK_KEY.to_string(),
            serde_json::Value::from(*estimated_time_to_recheck),
        );
        estimates_obj.insert(
            ESTIMATED_TIME_TO_RESTART_KEY.to_string(),
            serde_json::Value::from(*estimated_time_to_restart),
        );
        estimates_obj.insert(
            ESTIMATED_TIME_TO_INIT_KEY.to_string(),
            serde_json::Value::from(*estimated_time_to_init),
        );
        estimates_obj.insert(
            ESTIMATED_TIME_PER_FILE_KEY.to_string(),
            serde_json::Value::from(*estimated_time_per_file),
        );
        estimates_obj.insert(
            ESTIMATED_FILES_TO_RECHECK_KEY.to_string(),
            serde_json::Value::from(*estimated_files_to_recheck),
        );
        estimates_obj.insert(
            ESTIMATED_FILES_TO_INIT_KEY.to_string(),
            serde_json::Value::from(*estimated_files_to_init),
        );
        json_obj.insert(
            ESTIMATES_KEY.to_string(),
            serde_json::Value::Object(estimates_obj),
        );
    }

    let json_str = serde_json::to_string(&serde_json::Value::Object(json_obj))
        .expect("Failed to serialize JSON");

    let file = get_file(options);

    let result: Result<(), String> = fs::write(&file, &json_str).map_err(|e| {
        if e.kind() == std::io::ErrorKind::NotFound {
            "File doesn't exist".to_string()
        } else {
            format!("Failed to open file\n{}", e)
        }
    });

    match result {
        Ok(()) => {}
        Err(msg) => {
            flow_hh_logger::error!(
                "Failed to save per_file_time to {:?}. {}",
                file.display(),
                msg
            );
        }
    }
}

pub fn init(options: &Options, init_time: f64, parsed_count: i64) {
    let (per_file_time, per_merge_file_time, per_check_file_time) = load_stats(options);
    let mut guard = AVERAGES.lock().unwrap();
    *guard = Some(Averages {
        init_time,
        per_file_time,
        per_merge_file_time,
        per_check_file_time,
        parsed_count,
    });
}

fn with_averages<T>(f: impl FnOnce(&Averages) -> T) -> T {
    let guard = AVERAGES.lock().unwrap();
    match &*guard {
        None => panic!("Recheck_stats needs to be initialized before it can be used"),
        Some(averages) => f(averages),
    }
}

pub fn record_recheck_time(
    options: &Options,
    merge_time: f64,
    check_time: f64,
    merged_files: i64,
    checked_files: i64,
) {
    // with_averages
    let (init_time, per_file_time, per_merge_file_time, per_check_file_time, parsed_count) =
        with_averages(|averages| {
            let Averages {
                init_time,
                per_file_time,
                per_merge_file_time,
                per_check_file_time,
                parsed_count,
            } = *averages;
            (
                init_time,
                per_file_time,
                per_merge_file_time,
                per_check_file_time,
                parsed_count,
            )
        });
    // What should we do for tiny repositories? Let's make the window at least 15 samples big
    let window = parsed_count.max(15);
    let total_files = merged_files + checked_files;
    let per_file_time = if total_files > 0 {
        moving_average(
            window,
            per_file_time,
            (merge_time + check_time) / total_files as f64,
            total_files,
        )
    } else {
        per_file_time
    };
    let per_merge_file_time = if merged_files > 0 {
        moving_average(
            window,
            per_merge_file_time,
            merge_time / merged_files as f64,
            merged_files,
        )
    } else {
        per_merge_file_time
    };
    let per_check_file_time = if checked_files > 0 {
        moving_average(
            window,
            per_check_file_time,
            check_time / checked_files as f64,
            checked_files,
        )
    } else {
        per_check_file_time
    };
    save_averages(
        options,
        None,
        Averages {
            init_time,
            per_file_time,
            per_merge_file_time,
            per_check_file_time,
            parsed_count,
        },
    );
}

pub fn record_last_estimates(options: &Options, estimates: &Estimates) {
    let (init_time, per_file_time, per_merge_file_time, per_check_file_time, parsed_count) =
        with_averages(|averages| {
            let Averages {
                init_time,
                per_file_time,
                per_merge_file_time,
                per_check_file_time,
                parsed_count,
            } = *averages;
            (
                init_time,
                per_file_time,
                per_merge_file_time,
                per_check_file_time,
                parsed_count,
            )
        });
    save_averages(
        options,
        Some(estimates),
        Averages {
            init_time,
            per_file_time,
            per_merge_file_time,
            per_check_file_time,
            parsed_count,
        },
    );
}

pub fn get_init_time() -> f64 {
    with_averages(|Averages { init_time, .. }| *init_time)
}

pub fn get_per_merge_file_time() -> f64 {
    with_averages(
        |Averages {
             per_merge_file_time,
             ..
         }| *per_merge_file_time,
    )
}

pub fn get_per_check_file_time() -> f64 {
    with_averages(
        |Averages {
             per_check_file_time,
             ..
         }| *per_check_file_time,
    )
}
