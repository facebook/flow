/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::sync::Arc;
use std::time::Instant;

use flow_cgroup as cgroup;
use flow_common_utils::measure;
use flow_heap::parsing_heaps::SharedMem;

use crate::profiling_js;

thread_local! {
    static CURRENT_SHARED_MEM: RefCell<Option<Arc<SharedMem>>> = const { RefCell::new(None) };
}

pub fn with_shared_mem<T>(shared_mem: Arc<SharedMem>, f: impl FnOnce() -> T) -> T {
    let previous = CURRENT_SHARED_MEM.with(|current| current.replace(Some(shared_mem)));
    let ret = f();
    CURRENT_SHARED_MEM.with(|current| {
        current.replace(previous);
    });
    ret
}

fn current_shared_mem() -> Option<Arc<SharedMem>> {
    CURRENT_SHARED_MEM.with(|current| current.borrow().clone())
}

pub fn sample_memory(profiling: &profiling_js::Running) {
    let Some(shared_mem) = current_shared_mem() else {
        return;
    };
    let heap = shared_mem.heap_size();
    let hash_stats = shared_mem.hash_stats();
    profiling.sample_memory(None, "heap", f64::from(heap));
    profiling.sample_memory(
        None,
        "hash_nonempty_slots",
        f64::from(hash_stats.nonempty_slots),
    );
    profiling.sample_memory(None, "hash_slots", f64::from(hash_stats.slots));
    profiling.sample_memory(None, "hash_used_slots", f64::from(hash_stats.used_slots));
}

pub fn sample_init_memory(profiling: &profiling_js::Running) {
    let Some(shared_mem) = current_shared_mem() else {
        return;
    };
    let hash_stats = shared_mem.hash_stats();
    let heap_size = shared_mem.heap_size();
    let memory_metrics = [
        ("heap.size", heap_size),
        ("hash_table.nonempty_slots", hash_stats.nonempty_slots),
        ("hash_table.used_slots", hash_stats.used_slots),
        ("hash_table.slots", hash_stats.slots),
    ];
    for (metric, value) in memory_metrics {
        profiling.legacy_sample_memory(&format!("init_done.{}", metric), f64::from(value));
    }
}

pub fn with_memory_profiling<T>(profiling: &profiling_js::Running, f: impl FnOnce() -> T) -> T {
    sample_memory(profiling);
    let ret = f();
    sample_memory(profiling);
    ret
}

fn with_memory_info(
    callback: impl FnOnce(Result<cgroup::Stats, String>, flow_heap::parsing_heaps::HashStats, i32),
) {
    let cgroup_stats = cgroup::get_stats();
    let Some(shared_mem) = current_shared_mem() else {
        return;
    };
    let hash_stats = shared_mem.hash_stats();
    let heap_size = shared_mem.heap_size();
    callback(cgroup_stats, hash_stats, heap_size);
}

fn sample_timer_memory(
    timer: &str,
    profiling: &profiling_js::Running,
    cgroup_stats: Result<cgroup::Stats, String>,
    hash_stats: flow_heap::parsing_heaps::HashStats,
    heap_size: i32,
) {
    profiling.sample_memory(Some(timer), "heap", f64::from(heap_size));
    profiling.sample_memory(
        Some(timer),
        "hash_nonempty_slots",
        f64::from(hash_stats.nonempty_slots),
    );
    profiling.sample_memory(
        Some(timer),
        "hash_used_slots",
        f64::from(hash_stats.used_slots),
    );
    profiling.sample_memory(Some(timer), "hash_slots", f64::from(hash_stats.slots));

    match cgroup_stats {
        Err(_) => {}
        Ok(cgroup::Stats {
            total,
            total_swap,
            anon,
            file,
            shmem,
        }) => {
            profiling.sample_memory(Some(timer), "cgroup_total", total as f64);
            profiling.sample_memory(Some(timer), "cgroup_swap", total_swap as f64);
            profiling.sample_memory(Some(timer), "cgroup_anon", anon as f64);
            profiling.sample_memory(Some(timer), "cgroup_shmem", shmem as f64);
            profiling.sample_memory(Some(timer), "cgroup_file", file as f64);
        }
    }
}

fn clear_worker_memory() {
    for metric in [
        "worker_rss_start",
        "worker_rss_delta",
        "worker_rss_hwm_delta",
    ] {
        measure::delete(None, metric);
    }
}

fn profile_add_memory(
    profiling: &profiling_js::Running,
    getter: fn(Option<&measure::Record>, &str) -> Option<f64>,
    group: &str,
    metric: &str,
) {
    if let Some(start) = getter(None, "worker_rss_start") {
        if let Some(delta) = getter(None, "worker_rss_delta") {
            if let Some(hwm_delta) = getter(None, "worker_rss_hwm_delta") {
                profiling.add_memory(Some(group), metric, start, delta, hwm_delta);
            }
        }
    }
}

pub fn with_memory_timer<T>(should_print: bool, timer: &str, f: impl FnOnce() -> T) -> T {
    match profiling_js::current() {
        Some(profiling) => {
            let sample = |cgroup_stats, hash_stats, heap_size| {
                sample_timer_memory(timer, &profiling, cgroup_stats, hash_stats, heap_size);
            };
            clear_worker_memory();
            with_memory_info(sample);
            let previous_measure_enabled = measure::is_enabled();
            measure::set_enabled(previous_measure_enabled || should_print);
            let ret = profiling.with_timer(should_print, timer, f);
            measure::set_enabled(previous_measure_enabled);
            let sample = |cgroup_stats, hash_stats, heap_size| {
                sample_timer_memory(timer, &profiling, cgroup_stats, hash_stats, heap_size);
            };
            with_memory_info(sample);
            profile_add_memory(&profiling, measure::get_mean, timer, "worker_rss_avg");
            profile_add_memory(&profiling, measure::get_max, timer, "worker_rss_max");
            clear_worker_memory();
            ret
        }
        None => {
            let start = Instant::now();
            let ret = f();
            if should_print {
                eprintln!("[{}] {:.3}s", timer, start.elapsed().as_secs_f64());
            }
            ret
        }
    }
}
