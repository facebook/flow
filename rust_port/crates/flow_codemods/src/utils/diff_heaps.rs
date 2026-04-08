/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::OnceLock;

use dupe::Dupe;
use flow_parser::file_key::FileKey;
use flow_parser_utils_output::replacement_printer::Patch;

fn diff_patch_heap() -> &'static Mutex<HashMap<FileKey, Patch>> {
    static HEAP: OnceLock<Mutex<HashMap<FileKey, Patch>>> = OnceLock::new();
    HEAP.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn set_diff(file_key: &FileKey, patch: &Patch) {
    diff_patch_heap()
        .lock()
        .unwrap()
        .insert(file_key.dupe(), patch.clone());
}

pub fn get_diff(file_key: &FileKey) -> Option<Patch> {
    diff_patch_heap().lock().unwrap().get(file_key).cloned()
}

pub fn remove_batch(keys: &std::collections::BTreeSet<FileKey>) {
    let mut heap = diff_patch_heap().lock().unwrap();
    for key in keys {
        heap.remove(key);
    }
}
