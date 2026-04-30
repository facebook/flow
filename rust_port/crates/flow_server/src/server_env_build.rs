/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_common::options::Options;
use flow_server_env::server_env::Genv;

use crate::server_worker;

#[derive(Debug, Clone)]
pub struct StdlibGcControl {
    pub minor_heap_size: u32,
    pub major_heap_increment: u32,
    pub space_overhead: u32,
    pub window_size: u32,
    pub custom_major_ratio: u32,
    pub custom_minor_ratio: u32,
    pub custom_minor_max_size: u32,
}

impl Default for StdlibGcControl {
    fn default() -> Self {
        StdlibGcControl {
            minor_heap_size: 262144,
            major_heap_increment: 15,
            space_overhead: 120,
            window_size: 1,
            custom_major_ratio: 44,
            custom_minor_ratio: 100,
            custom_minor_max_size: 8192,
        }
    }
}

// Saves the default GC settings, which are restored by the workers.
fn default_gc_control() -> StdlibGcControl {
    StdlibGcControl::default()
}

fn worker_gc_control(options: &Options) -> StdlibGcControl {
    let gc = &options.gc_worker;
    let mut gc_control = default_gc_control();
    if let Some(v) = gc.minor_heap_size {
        gc_control.minor_heap_size = v;
    }
    if let Some(v) = gc.major_heap_increment {
        gc_control.major_heap_increment = v;
    }
    if let Some(v) = gc.space_overhead {
        gc_control.space_overhead = v;
    }
    if let Some(v) = gc.window_size {
        gc_control.window_size = v;
    }
    if let Some(v) = gc.custom_major_ratio {
        gc_control.custom_major_ratio = v;
    }
    if let Some(v) = gc.custom_minor_ratio {
        gc_control.custom_minor_ratio = v;
    }
    if let Some(v) = gc.custom_minor_max_size {
        gc_control.custom_minor_max_size = v;
    }
    if options.profile {
        flow_hh_logger::info!(
            "Worker GC params: minor_heap_size = {}; major_heap_increment = {}; space_overhead = {}; window_size = {}; custom_major_ratio = {}%; custom_minor_ratio = {}%; custom_minor_max_size = {}",
            gc_control.minor_heap_size,
            gc_control.major_heap_increment,
            gc_control.space_overhead,
            gc_control.window_size,
            gc_control.custom_major_ratio,
            gc_control.custom_minor_ratio,
            gc_control.custom_minor_max_size,
        );
    }
    gc_control
}

pub fn make_genv(
    options: Arc<Options>,
    init_id: &str,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
) -> Genv {
    let workers = {
        let num_workers = options.max_workers;
        if num_workers > 0 {
            let gc_control = worker_gc_control(&options);
            Some(server_worker::make(
                num_workers,
                (),
                (),
                gc_control,
                init_id,
                (),
            ))
        } else {
            None
        }
    };
    Genv {
        options,
        workers,
        shared_mem,
        node_modules_containers: Arc::new(std::collections::BTreeMap::new()),
    }
}
