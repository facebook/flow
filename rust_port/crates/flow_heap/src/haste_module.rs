/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_common::bitset::Bitset;
use flow_common_modulename::HasteModuleInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_utils_concurrency::locked_set::LockedSet;
use parking_lot::RwLock;

use crate::entity::Entity;

#[derive(Debug, Clone, Dupe)]
pub struct HasteModule {
    module_info: Arc<HasteModuleInfo>,
    provider: Arc<Entity<FileKey>>,
    dependents: Arc<LockedSet<FileKey>>,
    all_providers: Arc<RwLock<BTreeSet<FileKey>>>,
}

impl HasteModule {
    pub fn new(module_info: HasteModuleInfo) -> Self {
        Self {
            module_info: Arc::new(module_info),
            provider: Arc::new(Entity::empty()),
            dependents: Arc::new(LockedSet::new()),
            all_providers: Arc::new(RwLock::new(BTreeSet::new())),
        }
    }

    pub fn module_name(&self) -> &FlowSmolStr {
        self.module_info.module_name()
    }

    pub fn namespace_bitset(&self) -> &Bitset {
        self.module_info.namespace_bitset()
    }

    pub fn module_info(&self) -> &HasteModuleInfo {
        &self.module_info
    }

    pub fn get_provider(&self) -> Option<FileKey> {
        self.provider.read_latest()
    }

    pub fn get_provider_committed(&self) -> Option<FileKey> {
        self.provider.read_committed()
    }

    pub fn set_provider(&self, provider: Option<FileKey>) {
        self.provider.advance(provider);
    }

    pub fn add_provider(&self, file: FileKey) {
        self.all_providers.write().insert(file);
    }

    pub fn remove_provider(&self, file: &FileKey) {
        self.all_providers.write().remove(file);
    }

    pub fn get_all_providers(&self) -> Vec<FileKey> {
        self.all_providers.read().iter().duped().collect()
    }

    pub fn add_dependent(&self, file: FileKey) {
        self.dependents.insert(file);
    }

    pub fn remove_dependent(&self, file: &FileKey) {
        self.dependents.remove(file);
    }

    pub fn get_dependents(&self) -> Vec<FileKey> {
        self.dependents.iter().collect()
    }

    pub fn has_dependents(&self) -> bool {
        !self.dependents.is_empty()
    }
}
