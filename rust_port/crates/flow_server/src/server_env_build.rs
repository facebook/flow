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

pub fn make_genv(
    options: Arc<Options>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
) -> Genv {
    let workers = {
        let num_workers = options.max_workers;
        if num_workers > 0 {
            Some(server_worker::make(num_workers))
        } else {
            None
        }
    };
    Genv {
        options,
        workers,
        shared_mem,
    }
}
