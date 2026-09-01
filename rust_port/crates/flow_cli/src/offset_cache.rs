/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use dupe::Dupe;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::offset_utils::OffsetTable;

pub(crate) struct Info {
    pub(crate) offsets: Arc<OffsetTable>,
    pub(crate) ends_in_newline: bool,
}

#[derive(Default)]
pub(crate) struct OffsetCache {
    info: RefCell<HashMap<FileKey, Arc<Info>>>,
}

impl OffsetCache {
    pub(crate) fn info_of_file_key(&self, file_key: &FileKey) -> Option<Arc<Info>> {
        match file_key.inner() {
            FileKeyInner::ResourceFile(_) => None,
            _ => {
                if let Some(info) = self.info.borrow().get(file_key) {
                    return Some(info.dupe());
                }

                let contents = std::fs::read_to_string(file_key.to_absolute())
                    .expect("source file should be readable while generating Glean offsets");
                let info = Arc::new(Info {
                    offsets: Arc::new(OffsetTable::make(&contents)),
                    ends_in_newline: contents.ends_with('\n'),
                });
                self.info.borrow_mut().insert(file_key.dupe(), info.dupe());
                Some(info)
            }
        }
    }

    pub(crate) fn offset_table_of_file_key(&self, file_key: &FileKey) -> Option<Arc<OffsetTable>> {
        self.info_of_file_key(file_key)
            .map(|info| info.offsets.dupe())
    }
}
