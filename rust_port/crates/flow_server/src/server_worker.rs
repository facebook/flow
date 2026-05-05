/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

pub mod server_worker_state {
    use flow_parser::file_key;

    pub struct State {
        pub init_id: String,
        pub log_filename: Option<String>,
        pub project_root: String,
        pub flowlib_root: String,
    }

    pub fn save(init_id: &str) -> State {
        State {
            init_id: init_id.to_string(),
            log_filename: None,
            project_root: file_key::get_project_root(),
            flowlib_root: file_key::get_flowlib_root(),
        }
    }

    pub fn restore(
        State {
            init_id,
            log_filename,
            project_root,
            flowlib_root,
            ..
        }: &State,
        _worker_id: i32,
    ) {
        // Restore File_key root paths in worker processes
        file_key::set_project_root(project_root);
        file_key::set_flowlib_root(flowlib_root);

        // let init_id = init_id ^ "." ^ Random_id.short_string () in
        let init_id = format!(
            "{}.{}",
            init_id,
            flow_common_utils::random_id::short_string()
        );
        // FlowEventLogger.init_worker ~init_id (Unix.gettimeofday ());
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64();
        flow_event_logger::init_worker(&init_id, &serde_json::json!(now));

        match log_filename {
            None => {}
            Some(_file) => {}
        }
    }
}

pub fn make(
    _n: i32,
    _worker_mode: (),
    _channel_mode: (), // channel_mode: OCaml IPC, not applicable to Rust threads
    _gc_control: crate::server_env_build::StdlibGcControl,
    _init_id: &str,
    _heap_handle: (),
) -> ThreadPool {
    ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(_n as usize).expect("worker count must be > 0"),
    ))
}
