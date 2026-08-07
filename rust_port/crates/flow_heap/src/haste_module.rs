/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_common_modulename::HasteModuleInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;

use crate::resolved_requires::Dependency;
use crate::resolved_requires::DependencyTarget;

#[derive(Debug, Clone, Dupe)]
pub struct HasteModule {
    module_info: Arc<HasteModuleInfo>,
    dependency: Dependency,
    provider: Option<FileKey>,
}

impl HasteModule {
    pub(crate) fn new(module_info: HasteModuleInfo) -> Self {
        let dependency = Dependency::new(DependencyTarget::HasteModule(module_info.dupe()));
        Self {
            dependency,
            module_info: Arc::new(module_info),
            provider: None,
        }
    }

    pub(crate) fn new_committed(module_info: HasteModuleInfo, provider: Option<FileKey>) -> Self {
        let dependency = Dependency::new(DependencyTarget::HasteModule(module_info.dupe()));
        Self {
            dependency,
            module_info: Arc::new(module_info),
            provider,
        }
    }

    pub fn module_name(&self) -> &FlowSmolStr {
        self.module_info.module_name()
    }

    pub fn module_info(&self) -> &HasteModuleInfo {
        &self.module_info
    }

    pub(crate) fn dependency(&self) -> Dependency {
        self.dependency.dupe()
    }

    pub fn get_provider(&self) -> Option<FileKey> {
        self.provider.dupe()
    }

    pub fn with_provider(&self, provider: Option<FileKey>) -> Self {
        Self {
            module_info: self.module_info.dupe(),
            dependency: self.dependency.dupe(),
            provider,
        }
    }
}
