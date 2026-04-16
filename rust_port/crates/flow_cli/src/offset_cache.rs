/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::offset_utils::OffsetTable;

pub(crate) struct Info {
    pub(crate) offsets: OffsetTable,
    pub(crate) ends_in_newline: bool,
}

static INFO_CACHE: Mutex<Option<HashMap<String, Arc<Info>>>> = Mutex::new(None);

pub(crate) fn info_of_file_key(file_key: &FileKey) -> Option<Arc<Info>> {
    match file_key.inner() {
        FileKeyInner::ResourceFile(_) => None,
        _ => {
            let file = file_key.to_absolute();
            let mut cache = INFO_CACHE.lock().unwrap();
            let cache = cache.get_or_insert_with(HashMap::new);
            match cache.get(&file) {
                Some(info) => Some(info.clone()),
                None => {
                    let contents = std::fs::read_to_string(&file).unwrap();
                    let offsets = OffsetTable::make(&contents);
                    let ends_in_newline = contents.ends_with('\n');
                    let info = Info {
                        offsets,
                        ends_in_newline,
                    };
                    let info = Arc::new(info);
                    cache.insert(file, info.clone());
                    Some(info)
                }
            }
        }
    }
}

pub(crate) fn offset_table_of_file_key(file_key: &FileKey) -> Option<Arc<Info>> {
    info_of_file_key(file_key)
}

pub(crate) fn ends_in_newline_of_file_key(file_key: &FileKey) -> Option<bool> {
    info_of_file_key(file_key).map(|info| info.ends_in_newline)
}
