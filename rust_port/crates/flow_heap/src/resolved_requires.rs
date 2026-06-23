/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_parser::file_key::FileKey;

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DependencyTarget {
    HasteModule(HasteModuleInfo),
    File(FileKey),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolvedModuleTarget {
    Dependency(DependencyTarget),
    String(Box<FlowImportSpecifier>),
}

#[derive(Debug, Clone, Dupe)]
pub struct Dependency {
    target: Arc<ResolvedModuleTarget>,
}

impl PartialEq for Dependency {
    fn eq(&self, other: &Self) -> bool {
        self.target() == other.target()
    }
}

impl Eq for Dependency {}

impl Hash for Dependency {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.target().hash(state);
    }
}

impl PartialOrd for Dependency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Dependency {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.target().cmp(other.target())
    }
}

impl serde::Serialize for Dependency {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.target().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Dependency {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(DependencyTarget::deserialize(deserializer)?))
    }
}

impl Dependency {
    pub fn new(target: DependencyTarget) -> Self {
        Self {
            target: Arc::new(ResolvedModuleTarget::Dependency(target)),
        }
    }

    pub fn from_modulename(mname: Modulename) -> Self {
        match mname {
            Modulename::Haste(info) => Dependency::new(DependencyTarget::HasteModule(info)),
            Modulename::Filename(file_key) => Dependency::new(DependencyTarget::File(file_key)),
        }
    }

    pub fn target(&self) -> &DependencyTarget {
        match self.target.as_ref() {
            ResolvedModuleTarget::Dependency(target) => target,
            ResolvedModuleTarget::String(_) => {
                unreachable!("Dependency should always point to a dependency target")
            }
        }
    }

    fn target_addr(&self) -> Arc<ResolvedModuleTarget> {
        self.target.dupe()
    }

    pub fn target_dupe(&self) -> DependencyTarget {
        self.target().dupe()
    }

    pub fn haste_module(info: HasteModuleInfo) -> Self {
        Dependency::new(DependencyTarget::HasteModule(info))
    }

    pub fn file(file_key: FileKey) -> Self {
        Dependency::new(DependencyTarget::File(file_key))
    }

    pub fn as_haste_module_info(&self) -> Option<&HasteModuleInfo> {
        match self.target() {
            DependencyTarget::HasteModule(info) => Some(info),
            DependencyTarget::File(_) => None,
        }
    }

    pub fn as_file(&self) -> Option<&FileKey> {
        match self.target() {
            DependencyTarget::HasteModule(_) => None,
            DependencyTarget::File(file) => Some(file),
        }
    }

    pub fn name(&self) -> &str {
        match self.target() {
            DependencyTarget::HasteModule(info) => info.module_name().as_str(),
            DependencyTarget::File(file_key) => file_key.as_str(),
        }
    }

    pub fn to_modulename(&self) -> Modulename {
        match self.target() {
            DependencyTarget::HasteModule(info) => Modulename::Haste(info.dupe()),
            DependencyTarget::File(file_key) => Modulename::Filename(file_key.dupe()),
        }
    }
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub struct ResolvedModule {
    target: Option<Arc<ResolvedModuleTarget>>,
}

impl ResolvedModule {
    pub fn dependency(dependency: Dependency) -> Self {
        Self {
            target: Some(dependency.target_addr()),
        }
    }

    pub fn string(specifier: FlowImportSpecifier) -> Self {
        Self {
            target: Some(Arc::new(ResolvedModuleTarget::String(Box::new(specifier)))),
        }
    }

    pub fn null() -> Self {
        Self { target: None }
    }

    pub fn from_result(result: Result<Dependency, Option<FlowImportSpecifier>>) -> Self {
        match result {
            Ok(dependency) => Self::dependency(dependency),
            Err(Some(s)) => Self::string(s),
            Err(None) => Self::null(),
        }
    }

    pub fn to_result(&self) -> Result<Dependency, Option<FlowImportSpecifier>> {
        match self.target.as_ref().map(|target| target.as_ref()) {
            Some(ResolvedModuleTarget::Dependency(_)) => Ok(Dependency {
                target: self.target.as_ref().unwrap().dupe(),
            }),
            Some(ResolvedModuleTarget::String(specifier)) => Err(Some(specifier.as_ref().dupe())),
            None => Err(None),
        }
    }

    pub fn as_dependency(&self) -> Option<Dependency> {
        match self.target.as_ref().map(|target| target.as_ref()) {
            Some(ResolvedModuleTarget::Dependency(_)) => Some(Dependency {
                target: self.target.as_ref().unwrap().dupe(),
            }),
            Some(ResolvedModuleTarget::String(_)) | None => None,
        }
    }

    pub fn as_string(&self) -> Option<&FlowImportSpecifier> {
        match self.target.as_ref().map(|target| target.as_ref()) {
            Some(ResolvedModuleTarget::String(specifier)) => Some(specifier.as_ref()),
            Some(ResolvedModuleTarget::Dependency(_)) | None => None,
        }
    }

    pub fn is_null(&self) -> bool {
        self.target.is_none()
    }
}

impl serde::Serialize for ResolvedModule {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.target.as_ref().map(|target| target.as_ref()) {
            Some(ResolvedModuleTarget::Dependency(target)) => {
                ResolvedModuleSerde::Dependency(target.dupe()).serialize(serializer)
            }
            Some(ResolvedModuleTarget::String(specifier)) => {
                ResolvedModuleSerde::String(specifier.as_ref().dupe()).serialize(serializer)
            }
            None => ResolvedModuleSerde::Null.serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for ResolvedModule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match ResolvedModuleSerde::deserialize(deserializer)? {
            ResolvedModuleSerde::Dependency(target) => {
                Ok(Self::dependency(Dependency::new(target)))
            }
            ResolvedModuleSerde::String(specifier) => Ok(Self::string(specifier)),
            ResolvedModuleSerde::Null => Ok(Self::null()),
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
enum ResolvedModuleSerde {
    Dependency(DependencyTarget),
    String(FlowImportSpecifier),
    Null,
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

    /// Combine successfully resolved modules and phantom modules into a sorted list.
    pub fn all_dependencies(&self) -> Vec<Dependency> {
        let mut deps = Vec::with_capacity(
            self.inner.resolved_modules.len() + self.inner.phantom_dependencies.len(),
        );
        for module in &self.inner.resolved_modules {
            if let Some(dependency) = module.as_dependency() {
                deps.push(dependency);
            }
        }
        for dep in &self.inner.phantom_dependencies {
            deps.push(dep.dupe());
        }
        deps.sort_unstable();
        deps.dedup();
        deps
    }
}
