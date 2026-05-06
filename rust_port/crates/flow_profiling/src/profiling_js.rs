/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::LazyLock;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use flow_common_utils::measure;
#[cfg(unix)]
use nix::sys::resource;
#[cfg(unix)]
use nix::sys::time::TimeValLike;
use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

thread_local! {
    static CURRENT: RefCell<Option<Running>> = const { RefCell::new(None) };
}

#[derive(Clone)]
pub struct Running {
    running_timing: Rc<RefCell<TimingRunning>>,
    running_memory: Rc<RefCell<MemoryRunning>>,
}

#[derive(Clone)]
pub struct Finished {
    finished_timing: TimingResult,
    finished_memory: MemoryFinished,
}

#[derive(Clone)]
struct TimeMeasurement {
    start_age: f64,
    duration: f64,
}

#[derive(Clone)]
struct WorkerWallTimes {
    worker_idle: TimeMeasurement,
    worker_read_request: TimeMeasurement,
    worker_run: TimeMeasurement,
    worker_send_response: TimeMeasurement,
    worker_done: TimeMeasurement,
    worker_gc_minor: TimeMeasurement,
    worker_gc_major: TimeMeasurement,
}

#[derive(Clone)]
struct TimingResult {
    timer_name: String,
    user: TimeMeasurement,
    system: TimeMeasurement,
    worker_user: TimeMeasurement,
    worker_system: TimeMeasurement,
    worker_wall_times: WorkerWallTimes,
    wall: TimeMeasurement,
    sub_results: Vec<TimingResult>,
    sample_count: usize,
}

struct WorkerWallStartTimes {
    worker_idle_start: f64,
    worker_read_request_start: f64,
    worker_run_start: f64,
    worker_send_response_start: f64,
    worker_done_start: f64,
    worker_gc_minor_start: f64,
    worker_gc_major_start: f64,
}

struct RunningTimer {
    timer: String,
    user_start: f64,
    system_start: f64,
    worker_user_start: f64,
    worker_system_start: f64,
    worker_wall_start_times: WorkerWallStartTimes,
    wall_start: f64,
    sub_results_rev: Vec<TimingResult>,
}

struct TimingRunning {
    stack: Vec<RunningTimer>,
}

#[derive(Clone)]
struct MemoryResult {
    start: f64,
    delta: f64,
    high_water_mark_delta: f64,
}

struct MemoryRunning {
    running_groups_rev: Vec<String>,
    running_results: BTreeMap<String, BTreeMap<String, MemoryResult>>,
    running_sub_results_rev: Vec<MemoryFinished>,
}

#[derive(Clone)]
struct MemoryFinished {
    finished_label: String,
    finished_groups: Vec<String>,
    finished_results: BTreeMap<String, BTreeMap<String, MemoryResult>>,
    finished_sub_results: Vec<MemoryFinished>,
}

const LEGACY_TOP_TIMER_NAME: &str = "Profiling";
const LEGACY_MEMORY_GROUP: &str = "LEGACY";
const TOTAL_MEMORY_GROUP: &str = "TOTAL";

static FLOW_START_TIME: LazyLock<f64> = LazyLock::new(now_seconds);

fn now_seconds() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64()
}

#[cfg(unix)]
fn times() -> (f64, f64) {
    let tm = match resource::getrusage(resource::UsageWho::RUSAGE_SELF) {
        Ok(tm) => tm,
        Err(_) => return (0.0, 0.0),
    };
    let children_tm = match resource::getrusage(resource::UsageWho::RUSAGE_CHILDREN) {
        Ok(children_tm) => children_tm,
        Err(_) => return (0.0, 0.0),
    };
    (
        (tm.user_time().num_microseconds() + children_tm.user_time().num_microseconds()) as f64
            / 1_000_000.0,
        (tm.system_time().num_microseconds() + children_tm.system_time().num_microseconds()) as f64
            / 1_000_000.0,
    )
}

#[cfg(not(unix))]
fn times() -> (f64, f64) {
    (0.0, 0.0)
}

fn worker_times() -> (f64, f64) {
    let worker_user_time = measure::get_sum(None, "worker_user_time").unwrap_or(0.0);
    let worker_system_time = measure::get_sum(None, "worker_system_time").unwrap_or(0.0);
    (worker_user_time, worker_system_time)
}

fn time_measurement(start: f64, end: f64) -> TimeMeasurement {
    TimeMeasurement {
        start_age: start,
        duration: end - start,
    }
}

fn worker_wall_start_times() -> WorkerWallStartTimes {
    let get_run = || measure::get_sum(None, "worker_wall_time").unwrap_or(0.0);
    let get_read = || measure::get_sum(None, "worker_read_request").unwrap_or(0.0);
    let get_send = || measure::get_sum(None, "worker_send_response").unwrap_or(0.0);
    let get_idle = || measure::get_sum(None, "worker_idle").unwrap_or(0.0);
    let get_done = || measure::get_sum(None, "worker_done").unwrap_or(0.0);
    let get_gc_minor = || measure::get_sum(None, "worker_gc_minor_wall_time").unwrap_or(0.0);
    let get_gc_major = || measure::get_sum(None, "worker_gc_major_wall_time").unwrap_or(0.0);
    WorkerWallStartTimes {
        worker_idle_start: get_idle(),
        worker_read_request_start: get_read(),
        worker_run_start: get_run(),
        worker_send_response_start: get_send(),
        worker_done_start: get_done(),
        worker_gc_minor_start: get_gc_minor(),
        worker_gc_major_start: get_gc_major(),
    }
}

fn worker_wall_times(start: WorkerWallStartTimes) -> WorkerWallTimes {
    let get_run = || measure::get_sum(None, "worker_wall_time").unwrap_or(0.0);
    let get_read = || measure::get_sum(None, "worker_read_request").unwrap_or(0.0);
    let get_send = || measure::get_sum(None, "worker_send_response").unwrap_or(0.0);
    let get_idle = || measure::get_sum(None, "worker_idle").unwrap_or(0.0);
    let get_done = || measure::get_sum(None, "worker_done").unwrap_or(0.0);
    let get_gc_minor = || measure::get_sum(None, "worker_gc_minor_wall_time").unwrap_or(0.0);
    let get_gc_major = || measure::get_sum(None, "worker_gc_major_wall_time").unwrap_or(0.0);
    WorkerWallTimes {
        worker_idle: time_measurement(start.worker_idle_start, get_idle()),
        worker_read_request: time_measurement(start.worker_read_request_start, get_read()),
        worker_run: time_measurement(start.worker_run_start, get_run()),
        worker_send_response: time_measurement(start.worker_send_response_start, get_send()),
        worker_done: time_measurement(start.worker_done_start, get_done()),
        worker_gc_minor: time_measurement(start.worker_gc_minor_start, get_gc_minor()),
        worker_gc_major: time_measurement(start.worker_gc_major_start, get_gc_major()),
    }
}

fn start_timer(timer: &str) -> RunningTimer {
    let wall_start = now_seconds();
    let (user_start, system_start) = times();
    let (worker_user_start, worker_system_start) = worker_times();
    let worker_wall_start_times = worker_wall_start_times();
    RunningTimer {
        timer: timer.to_string(),
        user_start,
        system_start,
        worker_user_start,
        worker_system_start,
        worker_wall_start_times,
        wall_start,
        sub_results_rev: Vec::new(),
    }
}

fn stop_timer(running_timer: RunningTimer) -> TimingResult {
    let wall_end = now_seconds();
    let (user_end, system_end) = times();
    let (worker_user_end, worker_system_end) = worker_times();
    TimingResult {
        timer_name: running_timer.timer,
        user: TimeMeasurement {
            start_age: running_timer.user_start,
            duration: user_end - running_timer.user_start,
        },
        system: TimeMeasurement {
            start_age: running_timer.system_start,
            duration: system_end - running_timer.system_start,
        },
        worker_user: TimeMeasurement {
            start_age: running_timer.worker_user_start,
            duration: worker_user_end - running_timer.worker_user_start,
        },
        worker_system: TimeMeasurement {
            start_age: running_timer.worker_system_start,
            duration: worker_system_end - running_timer.worker_system_start,
        },
        worker_wall_times: worker_wall_times(running_timer.worker_wall_start_times),
        wall: TimeMeasurement {
            start_age: running_timer.wall_start - *FLOW_START_TIME,
            duration: wall_end - running_timer.wall_start,
        },
        sub_results: running_timer.sub_results_rev,
        sample_count: 1,
    }
}

impl TimingRunning {
    fn new(label: &str) -> Self {
        Self {
            stack: vec![start_timer(label)],
        }
    }

    fn with_timer<T>(&mut self, should_print: bool, timer: &str, f: impl FnOnce() -> T) -> T {
        self.stack.push(start_timer(timer));
        let ret = f();
        let running_timer = self
            .stack
            .pop()
            .expect("profiling timer stack should contain the running timer");
        let finished_timer = stop_timer(running_timer);
        if let Some(parent) = self.stack.last_mut() {
            parent.sub_results_rev.push(finished_timer.clone());
        }
        if should_print {
            flow_hh_logger::info!(
                "TimingEvent `{}`: start_wall_age: {}; wall_duration: {}",
                timer,
                finished_timer.wall.start_age,
                finished_timer.wall.duration
            );
        }
        ret
    }

    fn finish(&mut self) -> TimingResult {
        let total_timer = self
            .stack
            .pop()
            .expect("profiling timer stack should contain the total timer");
        stop_timer(total_timer)
    }

    fn merge(&mut self, from: TimingResult) {
        let into = self
            .stack
            .last_mut()
            .expect("profiling timer stack should contain the merge parent");
        into.sub_results_rev.push(from);
    }
}

impl MemoryRunning {
    fn new() -> Self {
        Self {
            running_groups_rev: Vec::new(),
            running_results: BTreeMap::new(),
            running_sub_results_rev: Vec::new(),
        }
    }

    fn finish(&self, label: &str) -> MemoryFinished {
        MemoryFinished {
            finished_label: label.to_string(),
            finished_groups: self.running_groups_rev.clone(),
            finished_results: self.running_results.clone(),
            finished_sub_results: self.running_sub_results_rev.clone(),
        }
    }

    fn get_group_map(&mut self, group: &str) -> BTreeMap<String, MemoryResult> {
        match self.running_results.get(group) {
            Some(group_map) => group_map.clone(),
            None => {
                self.running_groups_rev.push(group.to_string());
                self.running_results
                    .insert(group.to_string(), BTreeMap::new());
                BTreeMap::new()
            }
        }
    }

    fn get_metric(&mut self, group: &str, metric: &str) -> Option<MemoryResult> {
        self.get_group_map(group).get(metric).cloned()
    }

    fn set_metric(&mut self, group: &str, metric: &str, entry: MemoryResult) {
        let mut group_map = self.get_group_map(group);
        group_map.insert(metric.to_string(), entry);
        self.running_results.insert(group.to_string(), group_map);
    }

    fn legacy_sample_memory(&mut self, metric: &str, value: f64) {
        let legacy_metric = MemoryResult {
            start: 0.0,
            delta: value,
            high_water_mark_delta: value,
        };
        self.set_metric(LEGACY_MEMORY_GROUP, metric, legacy_metric);
    }

    fn start_sampling(&mut self, group: &str, metric: &str, value: f64) {
        let new_metric = MemoryResult {
            start: value,
            delta: 0.0,
            high_water_mark_delta: 0.0,
        };
        self.set_metric(group, metric, new_metric);
    }

    fn sample_memory(&mut self, group: &str, metric: &str, value: f64) {
        match self.get_metric(group, metric) {
            None => self.start_sampling(group, metric, value),
            Some(old_metric) => {
                let delta = value - old_metric.start;
                let new_metric = MemoryResult {
                    start: old_metric.start,
                    delta,
                    high_water_mark_delta: delta.max(old_metric.high_water_mark_delta),
                };
                self.set_metric(group, metric, new_metric);
            }
        }
    }

    fn add_memory(&mut self, group: &str, metric: &str, start: f64, delta: f64, hwm_delta: f64) {
        let new_metric = MemoryResult {
            start,
            delta,
            high_water_mark_delta: hwm_delta,
        };
        self.set_metric(group, metric, new_metric);
    }

    fn merge(&mut self, from: MemoryFinished) {
        self.running_sub_results_rev.push(from);
    }
}

impl Running {
    pub fn new(label: &str) -> Self {
        Self {
            running_timing: Rc::new(RefCell::new(TimingRunning::new(label))),
            running_memory: Rc::new(RefCell::new(MemoryRunning::new())),
        }
    }

    pub fn finish(&self) -> Finished {
        let finished_timing = self.running_timing.borrow_mut().finish();
        let finished_memory = self
            .running_memory
            .borrow()
            .finish(&finished_timing.timer_name);
        Finished {
            finished_timing,
            finished_memory,
        }
    }

    pub fn with_timer<T>(&self, should_print: bool, timer: &str, f: impl FnOnce() -> T) -> T {
        self.running_timing
            .borrow_mut()
            .with_timer(should_print, timer, f)
    }

    pub fn legacy_sample_memory(&self, metric: &str, value: f64) {
        self.running_memory
            .borrow_mut()
            .legacy_sample_memory(metric, value);
    }

    pub fn sample_memory(&self, group: Option<&str>, metric: &str, value: f64) {
        let mut memory = self.running_memory.borrow_mut();
        memory.sample_memory(TOTAL_MEMORY_GROUP, metric, value);
        if let Some(group) = group {
            memory.sample_memory(group, metric, value);
        }
    }

    pub fn add_memory(
        &self,
        group: Option<&str>,
        metric: &str,
        start: f64,
        delta: f64,
        hwm_delta: f64,
    ) {
        let mut memory = self.running_memory.borrow_mut();
        memory.add_memory(TOTAL_MEMORY_GROUP, metric, start, delta, hwm_delta);
        if let Some(group) = group {
            memory.add_memory(group, metric, start, delta, hwm_delta);
        }
    }

    pub fn merge(&self, from: &Finished) {
        self.running_timing
            .borrow_mut()
            .merge(from.finished_timing.clone());
        self.running_memory
            .borrow_mut()
            .merge(from.finished_memory.clone());
    }
}

impl Finished {
    pub fn get_profiling_duration(&self) -> f64 {
        self.finished_timing.wall.duration
    }

    pub fn to_json_properties(&self) -> Value {
        let mut props = Map::new();
        props.insert(
            "timing".to_string(),
            timing_to_json(false, &self.finished_timing),
        );
        props.insert(
            "memory".to_string(),
            memory_to_json(false, &self.finished_memory),
        );
        Value::Object(props)
    }

    pub fn get_abridged_timing_json_string(&self) -> String {
        flow_hh_json::json_string_of_value(&timing_to_json(true, &self.finished_timing))
    }

    pub fn get_abridged_memory_json_string(&self) -> String {
        flow_hh_json::json_string_of_value(&memory_to_json(true, &self.finished_memory))
    }
}

pub fn with_current<T>(profiling: &Running, f: impl FnOnce() -> T) -> T {
    let previous = CURRENT.with(|current| current.replace(Some(profiling.clone())));
    let ret = f();
    CURRENT.with(|current| {
        current.replace(previous);
    });
    ret
}

pub fn current() -> Option<Running> {
    CURRENT.with(|current| current.borrow().clone())
}

pub fn with_profiling_sync<T>(
    label: &str,
    should_print_summary: bool,
    f: impl FnOnce(&Running) -> T,
) -> (Finished, T) {
    let profiling = Running::new(label);
    let ret = with_current(&profiling, || f(&profiling));
    let finished_profile = profiling.finish();
    if should_print_summary {
        print_summary(&finished_profile);
    }
    (finished_profile, ret)
}

pub fn get_profiling_duration(profile: &Finished) -> f64 {
    profile.get_profiling_duration()
}

pub fn with_timer<T>(
    profile: &Running,
    should_print: bool,
    timer: &str,
    f: impl FnOnce() -> T,
) -> T {
    profile.with_timer(should_print, timer, f)
}

pub fn legacy_sample_memory(profile: &Running, metric: &str, value: f64) {
    profile.legacy_sample_memory(metric, value);
}

pub fn sample_memory(profile: &Running, group: Option<&str>, metric: &str, value: f64) {
    profile.sample_memory(group, metric, value);
}

pub fn add_memory(
    profile: &Running,
    group: Option<&str>,
    metric: &str,
    start: f64,
    delta: f64,
    hwm_delta: f64,
) {
    profile.add_memory(group, metric, start, delta, hwm_delta);
}

pub fn merge(from: &Finished, into: &Running) {
    into.merge(from);
}

pub fn to_json_properties(profile: &Finished) -> Value {
    profile.to_json_properties()
}

pub fn get_abridged_timing_json_string(profile: &Finished) -> String {
    profile.get_abridged_timing_json_string()
}

pub fn get_abridged_memory_json_string(profile: &Finished) -> String {
    profile.get_abridged_memory_json_string()
}

pub fn print_summary(profile: &Finished) {
    eprintln!();
    print_summary_memory_table(&profile.finished_memory);
    eprintln!();
    print_summary_timing_table(&profile.finished_timing);
    eprintln!();
}

fn json_number(value: f64) -> Value {
    Value::Number(Number::from_f64(value).unwrap_or_else(|| Number::from(0)))
}

fn json_of_time_measurement(time: &TimeMeasurement) -> Value {
    let mut props = Map::new();
    props.insert("start_age".to_string(), json_number(time.start_age));
    props.insert("duration".to_string(), json_number(time.duration));
    Value::Object(props)
}

fn combine_time_measurements(times: &[&TimeMeasurement]) -> TimeMeasurement {
    times.iter().fold(
        TimeMeasurement {
            start_age: 0.0,
            duration: 0.0,
        },
        |acc, time| TimeMeasurement {
            start_age: acc.start_age + time.start_age,
            duration: acc.duration + time.duration,
        },
    )
}

fn merge_time_measurement(a: &TimeMeasurement, b: &TimeMeasurement) -> TimeMeasurement {
    TimeMeasurement {
        start_age: a.start_age,
        duration: a.duration + b.duration,
    }
}

fn merge_worker_wall_times(a: &WorkerWallTimes, b: &WorkerWallTimes) -> WorkerWallTimes {
    WorkerWallTimes {
        worker_idle: merge_time_measurement(&a.worker_idle, &b.worker_idle),
        worker_read_request: merge_time_measurement(&a.worker_read_request, &b.worker_read_request),
        worker_run: merge_time_measurement(&a.worker_run, &b.worker_run),
        worker_send_response: merge_time_measurement(
            &a.worker_send_response,
            &b.worker_send_response,
        ),
        worker_done: merge_time_measurement(&a.worker_done, &b.worker_done),
        worker_gc_minor: merge_time_measurement(&a.worker_gc_minor, &b.worker_gc_minor),
        worker_gc_major: merge_time_measurement(&a.worker_gc_major, &b.worker_gc_major),
    }
}

fn merge_dupes(result: &TimingResult, dupes: &[TimingResult]) -> TimingResult {
    dupes
        .iter()
        .fold(result.clone(), |result, dupe| TimingResult {
            timer_name: result.timer_name,
            wall: merge_time_measurement(&result.wall, &dupe.wall),
            user: merge_time_measurement(&result.user, &dupe.user),
            system: merge_time_measurement(&result.system, &dupe.system),
            worker_user: merge_time_measurement(&result.worker_user, &dupe.worker_user),
            worker_system: merge_time_measurement(&result.worker_system, &dupe.worker_system),
            worker_wall_times: merge_worker_wall_times(
                &result.worker_wall_times,
                &dupe.worker_wall_times,
            ),
            sub_results: result
                .sub_results
                .into_iter()
                .chain(dupe.sub_results.iter().cloned())
                .collect(),
            sample_count: result.sample_count + 1,
        })
}

fn json_of_result(
    abridged: bool,
    max_depth: usize,
    dupes: &[TimingResult],
    result: &TimingResult,
) -> (String, Value) {
    let result = merge_dupes(result, dupes);
    let cpu = combine_time_measurements(&[
        &result.user,
        &result.system,
        &result.worker_user,
        &result.worker_system,
    ]);
    let mut fields = Map::new();
    if abridged && result.sample_count > 1 {
        fields.insert(
            "samples".to_string(),
            Value::Number(Number::from(result.sample_count)),
        );
    }
    fields.insert("wall".to_string(), json_of_time_measurement(&result.wall));
    fields.insert("cpu".to_string(), json_of_time_measurement(&cpu));
    if !abridged {
        fields.insert("user".to_string(), json_of_time_measurement(&result.user));
        fields.insert(
            "system".to_string(),
            json_of_time_measurement(&result.system),
        );
        fields.insert(
            "worker_user".to_string(),
            json_of_time_measurement(&result.worker_user),
        );
        fields.insert(
            "worker_system".to_string(),
            json_of_time_measurement(&result.worker_system),
        );
        let mut worker_wall_times = Map::new();
        worker_wall_times.insert(
            "run".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_run),
        );
        worker_wall_times.insert(
            "read".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_read_request),
        );
        worker_wall_times.insert(
            "send".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_send_response),
        );
        worker_wall_times.insert(
            "idle".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_idle),
        );
        worker_wall_times.insert(
            "done".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_done),
        );
        worker_wall_times.insert(
            "gc_minor".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_gc_minor),
        );
        worker_wall_times.insert(
            "gc_major".to_string(),
            json_of_time_measurement(&result.worker_wall_times.worker_gc_major),
        );
        fields.insert(
            "worker_wall_times".to_string(),
            Value::Object(worker_wall_times),
        );
        let sub_results = if max_depth > 0 {
            json_of_results(abridged, max_depth - 1, &result.sub_results)
        } else {
            Value::Object(Map::new())
        };
        fields.insert("sub_results".to_string(), sub_results);
        fields.insert(
            "samples".to_string(),
            Value::Number(Number::from(result.sample_count)),
        );
    }
    (result.timer_name, Value::Object(fields))
}

fn json_of_results(abridged: bool, max_depth: usize, results: &[TimingResult]) -> Value {
    let mut results_rev: Vec<TimingResult> = Vec::new();
    let mut dupes: BTreeMap<String, Vec<TimingResult>> = BTreeMap::new();
    for result in results {
        match dupes.get_mut(&result.timer_name) {
            None => {
                results_rev.push(result.clone());
                dupes.insert(result.timer_name.clone(), Vec::new());
            }
            Some(prev_dupes) => prev_dupes.push(result.clone()),
        }
    }

    let mut json_results = Map::new();
    for result in results_rev.into_iter().rev() {
        let dupes = dupes
            .get(&result.timer_name)
            .expect("dupes map should contain every result timer");
        let (name, value) = json_of_result(abridged, max_depth, dupes, &result);
        json_results.insert(name, value);
    }
    Value::Object(json_results)
}

fn timing_to_json(abridged: bool, result: &TimingResult) -> Value {
    let mut results_rev = vec![TimingResult {
        timer_name: LEGACY_TOP_TIMER_NAME.to_string(),
        ..result.clone()
    }];
    for sub_result in &result.sub_results {
        results_rev.push(sub_result.clone());
        let prefix = format!("{}:", sub_result.timer_name);
        for sub_sub_result in &sub_result.sub_results {
            results_rev.push(TimingResult {
                timer_name: format!("{}{}", prefix, sub_sub_result.timer_name),
                ..sub_sub_result.clone()
            });
        }
    }
    let mut props = Map::new();
    props.insert(
        "results".to_string(),
        json_of_results(abridged, 0, &results_rev),
    );
    Value::Object(props)
}

fn memory_to_json(abridged: bool, finished_memory: &MemoryFinished) -> Value {
    let mut object_props = Map::new();
    for (group_name, group) in &finished_memory.finished_results {
        if group_name == LEGACY_MEMORY_GROUP {
            for (k, v) in group {
                object_props.insert(k.clone(), json_number(v.delta));
            }
        } else {
            let mut group_json = Map::new();
            for (k, v) in group {
                let mut metric = Map::new();
                metric.insert("start".to_string(), json_number(v.start));
                metric.insert("delta".to_string(), json_number(v.delta));
                metric.insert(
                    "hwm_delta".to_string(),
                    json_number(v.high_water_mark_delta),
                );
                group_json.insert(k.clone(), Value::Object(metric));
            }
            object_props.insert(group_name.clone(), Value::Object(group_json));
        }
    }
    if !abridged {
        let mut sub_results = Map::new();
        for result in &finished_memory.finished_sub_results {
            sub_results.insert(result.finished_label.clone(), memory_to_json(false, result));
        }
        object_props.insert("sub_results".to_string(), Value::Object(sub_results));
    }
    Value::Object(object_props)
}

fn sum_cpu(result: &TimingResult) -> f64 {
    result.user.duration
        + result.system.duration
        + result.worker_user.duration
        + result.worker_system.duration
}

fn sum_cpu_start_age(result: &TimingResult) -> f64 {
    result.user.start_age
        + result.system.start_age
        + result.worker_user.start_age
        + result.worker_system.start_age
}

type WorkerTuple = (f64, f64, f64, f64, f64, f64, f64);
type LastEnd = (f64, f64, WorkerTuple);
type Remaining = (f64, f64, WorkerTuple);

fn pct(num: f64, denom: f64) -> f64 {
    if denom == 0.0 {
        0.0
    } else {
        100.0 * num / denom
    }
}

fn print_summary_single_raw(key: &str, result: (f64, f64, WorkerTuple), total: &TimingResult) {
    let (result_wall, result_cpu, (run, read, send, idle, done_, gc_minor, gc_major)) = result;
    let run = run - gc_minor - gc_major;
    let worker_total = idle + done_ + read + run + send + gc_minor + gc_major;
    let worker_total = if worker_total == 0.0 {
        1.0
    } else {
        worker_total
    };
    let worker_idle_pct = idle / worker_total * 100.0;
    let worker_read_pct = read / worker_total * 100.0;
    let worker_run_pct = run / worker_total * 100.0;
    let worker_send_pct = send / worker_total * 100.0;
    let worker_done_pct = done_ / worker_total * 100.0;
    let worker_gc_minor_pct = gc_minor / worker_total * 100.0;
    let worker_gc_major_pct = gc_major / worker_total * 100.0;
    eprintln!(
        "{:7.3} ({:5.1}%)   {:9.3} ({:5.1}%)   {:3}% {:3}% {:3}% {:3}% {:3}% {:3}% {:3}%    {}",
        result_wall,
        pct(result_wall, total.wall.duration),
        result_cpu,
        pct(result_cpu, sum_cpu(total)),
        worker_run_pct as i32,
        worker_read_pct as i32,
        worker_send_pct as i32,
        worker_idle_pct as i32,
        worker_done_pct as i32,
        worker_gc_minor_pct as i32,
        worker_gc_major_pct as i32,
        key
    );
}

fn print_summary_single(key: &str, result: &TimingResult, total: &TimingResult) {
    let worker_wall_times = (
        result.worker_wall_times.worker_run.duration,
        result.worker_wall_times.worker_read_request.duration,
        result.worker_wall_times.worker_send_response.duration,
        result.worker_wall_times.worker_idle.duration,
        result.worker_wall_times.worker_done.duration,
        result.worker_wall_times.worker_gc_minor.duration,
        result.worker_wall_times.worker_gc_major.duration,
    );
    print_summary_single_raw(
        key,
        (result.wall.duration, sum_cpu(result), worker_wall_times),
        total,
    );
}

fn print_unknown(indent: &str, last_end: LastEnd, start: LastEnd, total: &TimingResult) {
    let (
        wall_start_age,
        cpu_start_age,
        (run_start, read_start, send_start, idle_start, done_start, gc_minor_start, gc_major_start),
    ) = start;
    let (
        wall_end,
        cpu_end,
        (run_end, read_end, send_end, idle_end, done_end, gc_minor_end, gc_major_end),
    ) = last_end;
    let unknown_wall = wall_start_age - wall_end;
    if total.wall.duration != 0.0 && unknown_wall / total.wall.duration > 0.01 {
        let unknown_cpu = cpu_start_age - cpu_end;
        let unknown_worker = (
            run_start - run_end,
            read_start - read_end,
            send_start - send_end,
            idle_start - idle_end,
            done_start - done_end,
            gc_minor_start - gc_minor_end,
            gc_major_start - gc_major_end,
        );
        print_summary_single_raw(
            &format!("{}<Unknown>", indent),
            (unknown_wall, unknown_cpu, unknown_worker),
            total,
        );
    }
}

fn worker_wall_times_to_tuples(
    worker_wall_times: &WorkerWallTimes,
) -> (WorkerTuple, WorkerTuple, WorkerTuple) {
    let worker_last = (
        worker_wall_times.worker_run.start_age,
        worker_wall_times.worker_read_request.start_age,
        worker_wall_times.worker_send_response.start_age,
        worker_wall_times.worker_idle.start_age,
        worker_wall_times.worker_done.start_age,
        worker_wall_times.worker_gc_minor.start_age,
        worker_wall_times.worker_gc_major.start_age,
    );
    let worker_remaining = (
        worker_wall_times.worker_run.duration,
        worker_wall_times.worker_read_request.duration,
        worker_wall_times.worker_send_response.duration,
        worker_wall_times.worker_idle.duration,
        worker_wall_times.worker_done.duration,
        worker_wall_times.worker_gc_minor.duration,
        worker_wall_times.worker_gc_major.duration,
    );
    let worker_end = (
        worker_wall_times.worker_run.start_age + worker_wall_times.worker_run.duration,
        worker_wall_times.worker_read_request.start_age
            + worker_wall_times.worker_read_request.duration,
        worker_wall_times.worker_send_response.start_age
            + worker_wall_times.worker_send_response.duration,
        worker_wall_times.worker_idle.start_age + worker_wall_times.worker_idle.duration,
        worker_wall_times.worker_done.start_age + worker_wall_times.worker_done.duration,
        worker_wall_times.worker_gc_minor.start_age + worker_wall_times.worker_gc_minor.duration,
        worker_wall_times.worker_gc_major.start_age + worker_wall_times.worker_gc_major.duration,
    );
    (worker_last, worker_remaining, worker_end)
}

fn print_result_rows(
    indent: &str,
    total: &TimingResult,
    state: (LastEnd, Remaining),
    result: &TimingResult,
) -> (LastEnd, Remaining) {
    let (last_end, (wall_remaining, cpu_remaining, worker_remaining)) = state;
    let (result_worker_starts, result_worker_durations, result_worker_end) =
        worker_wall_times_to_tuples(&result.worker_wall_times);

    print_unknown(
        indent,
        last_end,
        (
            result.wall.start_age,
            sum_cpu_start_age(result),
            result_worker_starts,
        ),
        total,
    );
    print_summary_single(&format!("{}{}", indent, result.timer_name), result, total);

    if !result.sub_results.is_empty() {
        let new_indent = format!("{}  ", indent);
        let child_last_end = (
            result.wall.start_age,
            sum_cpu_start_age(result),
            result_worker_starts,
        );
        let child_remaining = (
            result.wall.duration,
            sum_cpu(result),
            result_worker_durations,
        );
        let (child_last_end, child_remaining) = result
            .sub_results
            .iter()
            .fold((child_last_end, child_remaining), |state, sub_result| {
                print_result_rows(&new_indent, total, state, sub_result)
            });
        print_unknown(
            &new_indent,
            child_last_end,
            (
                result.wall.start_age + result.wall.duration,
                sum_cpu_start_age(result) + sum_cpu(result),
                result_worker_end,
            ),
            total,
        );
        print_summary_single_raw(
            &format!("{}<Unknown total>", new_indent),
            child_remaining,
            total,
        );
    }

    let last_end = (
        result.wall.start_age + result.wall.duration,
        sum_cpu_start_age(result) + sum_cpu(result),
        result_worker_end,
    );
    let (
        worker_run,
        worker_read,
        worker_send,
        worker_idle,
        worker_done,
        worker_gc_minor,
        worker_gc_major,
    ) = worker_remaining;
    let remaining = (
        wall_remaining - result.wall.duration,
        cpu_remaining - sum_cpu(result),
        (
            worker_run - result.worker_wall_times.worker_run.duration,
            worker_read - result.worker_wall_times.worker_read_request.duration,
            worker_send - result.worker_wall_times.worker_send_response.duration,
            worker_idle - result.worker_wall_times.worker_idle.duration,
            worker_done - result.worker_wall_times.worker_done.duration,
            worker_gc_minor - result.worker_wall_times.worker_gc_minor.duration,
            worker_gc_major - result.worker_wall_times.worker_gc_major.duration,
        ),
    );
    (last_end, remaining)
}

fn print_summary_timing_table(total: &TimingResult) {
    let label = format!("{} Timings", total.timer_name);
    let header =
        "   WALL TIME            CPU TIME         RUN/READ/SEND/IDLE/DONE/GC m/GC M      SECTION";
    let header_len = header.len() + 8;
    let whitespace_len = header_len.saturating_sub(label.len());
    eprintln!(
        "{}{}{}",
        "=".repeat(whitespace_len.div_ceil(2)),
        label,
        "=".repeat(whitespace_len / 2)
    );
    eprintln!("{}", header);
    eprintln!("{}", "-".repeat(header_len));
    print_summary_single("<Total>", total, total);

    let indent = "  ";
    let (worker_last, worker_remaining, worker_end) =
        worker_wall_times_to_tuples(&total.worker_wall_times);
    let last_end = (total.wall.start_age, sum_cpu_start_age(total), worker_last);
    let remaining = (total.wall.duration, sum_cpu(total), worker_remaining);
    let (last_end, remaining) = total
        .sub_results
        .iter()
        .fold((last_end, remaining), |state, result| {
            print_result_rows(indent, total, state, result)
        });
    print_unknown(
        indent,
        last_end,
        (
            total.wall.start_age + total.wall.duration,
            sum_cpu_start_age(total) + sum_cpu(total),
            worker_end,
        ),
        total,
    );
    print_summary_single_raw("<Unknown total>", remaining, total);
}

fn pretty_num(f: f64) -> String {
    let abs_f = f.abs();
    if abs_f > 1_000_000_000.0 {
        format!("{:+7.2}G", f / 1_000_000_000.0)
    } else if abs_f > 1_000_000.0 {
        format!("{:+7.2}M", f / 1_000_000.0)
    } else if abs_f > 1_000.0 {
        format!("{:+7.2}K", f / 1_000.0)
    } else {
        format!("{:+7.2} ", f)
    }
}

fn pretty_pct(num: f64, denom: f64) -> String {
    if denom == 0.0 {
        "(--N/A--)".to_string()
    } else {
        let fraction = num / denom;
        if fraction >= 10.0 {
            format!("({:+6.1}x)", fraction)
        } else {
            format!("({:+6.1}%)", fraction * 100.0)
        }
    }
}

fn print_summary_memory_single(indent: usize, key: &str, result: &MemoryResult) {
    let indent = " ".repeat(indent);
    eprintln!(
        "{}        {} {}    {} {}    {}{}",
        pretty_num(result.start),
        pretty_num(result.delta),
        pretty_pct(result.delta, result.start),
        pretty_num(result.high_water_mark_delta),
        pretty_pct(result.high_water_mark_delta, result.start),
        indent,
        key
    );
}

fn print_memory_group(
    header_without_section: &str,
    indent: usize,
    finished_results: &BTreeMap<String, BTreeMap<String, MemoryResult>>,
    group_name: &str,
) {
    if let Some(group) = finished_results.get(group_name) {
        let indent_str = " ".repeat(header_without_section.len() + indent - 2);
        eprintln!("{}== {} ==", indent_str, group_name);
        for (key, result) in group {
            print_summary_memory_single(indent + 2, key, result);
        }
    }
}

fn print_summary_memory_header(label: &str) {
    let label = format!("{} Memory Stats", label);
    let header_without_section = "  START                DELTA               HWM DELTA          ";
    let header = format!("{}SECTION", header_without_section);
    let header_len = header.len() + 8;
    let whitespace_len = header_len.saturating_sub(label.len());
    eprintln!(
        "{}{}{}",
        "=".repeat(whitespace_len.div_ceil(2)),
        label,
        "=".repeat(whitespace_len / 2)
    );
    eprintln!("{}", header);
    eprintln!("{}", "-".repeat(header_len));
}

fn print_finished_memory(indent: usize, results: &MemoryFinished) {
    if !results.finished_results.is_empty() || !results.finished_sub_results.is_empty() {
        let header_without_section =
            "  START                DELTA               HWM DELTA          ";
        let pre_section_whitespace = " ".repeat(header_without_section.len());
        let header_indent = "=".repeat(indent);
        eprintln!(
            "{}{} {} {}",
            pre_section_whitespace, header_indent, results.finished_label, header_indent
        );
        let indent = indent + 2;
        for group in &results.finished_groups {
            print_memory_group(
                header_without_section,
                indent,
                &results.finished_results,
                group,
            );
        }
        for sub_result in &results.finished_sub_results {
            print_finished_memory(indent, sub_result);
        }
    }
}

fn print_summary_memory_table(memory: &MemoryFinished) {
    if !memory.finished_results.is_empty() || !memory.finished_sub_results.is_empty() {
        print_summary_memory_header(&memory.finished_label);
        print_finished_memory(2, memory);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_nested_timers() {
        let (profile, ()) = with_profiling_sync("Init", false, |profile| {
            profile.with_timer(false, "Parsing", || ());
        });
        let timing = profile.to_json_properties();
        let results = timing
            .get("timing")
            .and_then(|v| v.get("results"))
            .and_then(|v| v.as_object())
            .expect("profiling JSON should contain timing results");
        assert!(
            results.contains_key("Parsing"),
            "top-level timer should be present in timing results"
        );
    }
}
