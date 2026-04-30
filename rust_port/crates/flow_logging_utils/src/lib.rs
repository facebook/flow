/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::options::Options;

pub fn hh_logger_level_of_env(env: &str) -> Option<flow_hh_logger::Level> {
    match std::env::var(env).ok().as_deref() {
        Some("off") => Some(flow_hh_logger::Level::Off),
        Some("fatal") => Some(flow_hh_logger::Level::Fatal),
        Some("error") => Some(flow_hh_logger::Level::Error),
        Some("warn") => Some(flow_hh_logger::Level::Warn),
        Some("info") => Some(flow_hh_logger::Level::Info),
        Some("debug") => Some(flow_hh_logger::Level::Debug),
        // ignore invalid values
        Some(_) | None => None,
    }
}

// TODO: min_level should probably default to warn, but was historically info
pub fn set_hh_logger_min_level(min_level: Option<flow_hh_logger::Level>, options: &Options) {
    let min_level = min_level.unwrap_or(flow_hh_logger::Level::Info);
    let level = if options.quiet {
        flow_hh_logger::Level::Off
    } else if options.verbose.is_some() || options.debug {
        flow_hh_logger::Level::Debug
    } else {
        match hh_logger_level_of_env("FLOW_LOG_LEVEL") {
            Some(level) => level,
            None => min_level,
        }
    };
    flow_hh_logger::level::set_min_level(level);
}

pub fn init_loggers(options: &Options, min_level: Option<flow_hh_logger::Level>) {
    set_hh_logger_min_level(min_level, options)
}

struct Formatted<'a> {
    lazy_mode: &'static str,
    max_workers: i32,
    long_lived_workers: bool,
    enabled_rollouts: &'a std::collections::BTreeMap<String, String>,
    debug: bool,
    log_saving: &'a std::collections::BTreeMap<String, flow_common::options::LogSaving>,
    log_file: String,
}

fn format(server_options: &Options) -> Formatted<'_> {
    let lazy_mode = if server_options.lazy_mode {
        "on"
    } else {
        "off"
    };
    let max_workers = server_options.max_workers;
    let long_lived_workers = server_options.long_lived_workers;
    let enabled_rollouts = server_options.enabled_rollouts.as_ref();
    let debug = server_options.debug;
    let log_saving = server_options.log_saving.as_ref();
    let log_file = server_options.log_file.display().to_string();
    Formatted {
        lazy_mode,
        max_workers,
        long_lived_workers,
        enabled_rollouts,
        debug,
        log_saving,
        log_file,
    }
}

pub fn set_server_options(server_options: &Options) {
    let f = format(server_options);
    flow_event_logger::set_server_options(
        f.lazy_mode,
        f.max_workers,
        f.long_lived_workers,
        f.enabled_rollouts,
        f.debug,
        f.log_saving,
        &f.log_file,
    );
}

pub fn dump_server_options(server_options: &Options, log: &mut dyn FnMut(&str)) {
    let f = format(server_options);
    log(&format!("lazy_mode={}", f.lazy_mode));
    log(&format!("max_workers={}", f.max_workers));
    log(&format!("long_lived_workers={}", f.long_lived_workers));
    log(&format!("debug={}", f.debug));
    for (method_name, log_saving) in f.log_saving.iter() {
        let limit_str = match log_saving.limit {
            None => "null".to_string(),
            Some(limit) => format!("{}", limit),
        };
        log(&format!(
            "{} threshold_time_ms={} limit={} rate={}",
            method_name, log_saving.threshold_time_ms, limit_str, log_saving.rate
        ));
    }
    for (r, g) in f.enabled_rollouts.iter() {
        log(&format!("Rollout {:?} set to {:?}", r, g));
    }
}

pub fn disable_logging() {
    flow_event_logger_common::event_logger::disable_logging();
    flow_event_logger::disable_logging();
    flow_interaction_logger::disable_logging();
}
