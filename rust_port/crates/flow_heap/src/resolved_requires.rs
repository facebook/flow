/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common_modulename::Modulename;
use flow_parser::file_key::FileKey;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Dependency {
    HasteModule(Modulename),
    File(FileKey),
}

impl Dependency {
    pub fn from_modulename(mname: Modulename) -> Self {
        match mname {
            Modulename::Haste(_) => Dependency::HasteModule(mname),
            Modulename::Filename(file_key) => Dependency::File(file_key),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Dependency::HasteModule(modulename) => modulename.as_str(),
            Dependency::File(file_key) => file_key.as_str(),
        }
    }

    pub fn to_modulename(&self) -> Modulename {
        match self {
            Dependency::HasteModule(modulename) => modulename.clone(),
            Dependency::File(file_key) => Modulename::Filename(file_key.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedModule {
    HasteModule(Modulename),
    File(FileKey),
    String(FlowImportSpecifier),
    Null,
}

impl ResolvedModule {
    pub fn from_result(result: Result<Dependency, Option<FlowImportSpecifier>>) -> Self {
        match result {
            Ok(Dependency::HasteModule(m)) => ResolvedModule::HasteModule(m),
            Ok(Dependency::File(f)) => ResolvedModule::File(f),
            Err(Some(s)) => ResolvedModule::String(s),
            Err(None) => ResolvedModule::Null,
        }
    }

    pub fn to_result(&self) -> Result<Dependency, Option<FlowImportSpecifier>> {
        match self {
            ResolvedModule::HasteModule(m) => Ok(Dependency::HasteModule(m.clone())),
            ResolvedModule::File(f) => Ok(Dependency::File(f.dupe())),
            ResolvedModule::String(s) => Err(Some(s.dupe())),
            ResolvedModule::Null => Err(None),
        }
    }
}

#[derive(Debug)]
struct ResolvedRequiresInner {
    resolved_modules: Vec<ResolvedModule>,
    phantom_dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone, Dupe)]
pub struct ResolvedRequires {
    inner: Arc<ResolvedRequiresInner>,
}

impl ResolvedRequires {
    pub fn new(
        resolved_modules: Vec<ResolvedModule>,
        phantom_dependencies: Vec<Dependency>,
    ) -> Self {
        Self {
            inner: Arc::new(ResolvedRequiresInner {
                resolved_modules,
                phantom_dependencies,
            }),
        }
    }

    pub fn get_resolved_modules(&self) -> &[ResolvedModule] {
        &self.inner.resolved_modules
    }

    pub fn get_phantom_dependencies(&self) -> &[Dependency] {
        &self.inner.phantom_dependencies
    }
}
