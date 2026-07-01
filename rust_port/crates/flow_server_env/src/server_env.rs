/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::overlay_map::EnvCell;
use flow_data_structure_wrapper::overlay_map::EnvCellMapBase;
pub use flow_data_structure_wrapper::overlay_map::EnvCellReadGuard;
use flow_data_structure_wrapper::overlay_map::OverlayMap;
use flow_data_structure_wrapper::overlay_map::apply_delta_to_base_map;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_services_coverage::FileCoverage;
use flow_services_export::export_search::ExportSearch;
use flow_typing_context::MasterContext;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_utils_concurrency::thread_pool::ThreadPool;
use vec1::Vec1;

use crate::collated_errors::CollatedErrors;
use crate::collated_errors::OverlayCollatedErrors;
use crate::dependency_info::DependencyInfo;
use crate::persistent_connection::PersistentConnection;

// The "static" environment, initialized first and then unchanged.

pub struct Genv {
    pub options: Arc<Options>,
    pub workers: Option<ThreadPool>,
    pub shared_mem: Arc<SharedMem>,
}

// The environment constantly maintained by the server.

// Do not change these to contain `Loc`s. Because these errors are stored between rechecks, it is
// critical that they contain `ALoc`s, so that we can update the concrete locations when we
// render the errors, without having to retypecheck the files that generated those errors.
#[derive(Clone)]
pub struct Errors {
    /// errors are stored in a map from file path to error set, so that the errors
    /// from checking particular files can be cleared during recheck.
    pub local_errors: BTreeMap<FileKey, ErrorSet>,
    /// duplicate providers found during commit_modules are stored separately so
    /// they can be cleared easily
    pub duplicate_providers: BTreeMap<FlowSmolStr, (FileKey, Vec1<FileKey>)>,
    /// errors encountered during merge have to be stored separately so
    /// dependencies can be cleared during merge.
    pub merge_errors: BTreeMap<FileKey, ErrorSet>,
    /// warnings are stored in a map from file path to error set, so that the warnings
    /// from checking particular files can be cleared during recheck.
    pub warnings: BTreeMap<FileKey, ErrorSet>,
    /// error suppressions in the code
    pub suppressions: ErrorSuppressions,
}

impl Errors {
    pub fn empty() -> Self {
        Self {
            local_errors: BTreeMap::new(),
            duplicate_providers: BTreeMap::new(),
            merge_errors: BTreeMap::new(),
            warnings: BTreeMap::new(),
            suppressions: ErrorSuppressions::empty(),
        }
    }
}

impl Default for Errors {
    fn default() -> Self {
        Self::empty()
    }
}

pub type OverlayErrorMap = OverlayMap<FileKey, ErrorSet, EnvCellMapBase<Errors, FileKey, ErrorSet>>;
pub type OverlayDuplicateProviders = OverlayMap<
    FlowSmolStr,
    (FileKey, Vec1<FileKey>),
    EnvCellMapBase<Errors, FlowSmolStr, (FileKey, Vec1<FileKey>)>,
>;

#[derive(Clone)]
pub struct OverlayErrors {
    pub base_cell: Arc<EnvCell<Errors>>,
    pub local_errors: OverlayErrorMap,
    pub duplicate_providers: OverlayDuplicateProviders,
    pub merge_errors: OverlayErrorMap,
    pub warnings: OverlayErrorMap,
    pub suppressions: ErrorSuppressions,
}

impl OverlayErrors {
    pub fn new(base_cell: Arc<EnvCell<Errors>>) -> Self {
        let suppressions = base_cell.read().suppressions.clone();
        Self {
            base_cell: base_cell.dupe(),
            local_errors: OverlayMap::with_base(EnvCellMapBase::new(base_cell.dupe(), |errors| {
                &errors.local_errors
            })),
            duplicate_providers: OverlayMap::with_base(EnvCellMapBase::new(
                base_cell.dupe(),
                |errors| &errors.duplicate_providers,
            )),
            merge_errors: OverlayMap::with_base(EnvCellMapBase::new(base_cell.dupe(), |errors| {
                &errors.merge_errors
            })),
            warnings: OverlayMap::with_base(EnvCellMapBase::new(base_cell, |errors| {
                &errors.warnings
            })),
            suppressions,
        }
    }

    pub fn from_errors(errors: Errors) -> Self {
        Self::new(env_cell(errors))
    }

    pub fn commit(self) {
        let OverlayErrors {
            base_cell,
            local_errors,
            duplicate_providers,
            merge_errors,
            warnings,
            suppressions,
        } = self;
        let local_errors = local_errors.into_delta();
        let duplicate_providers = duplicate_providers.into_delta();
        let merge_errors = merge_errors.into_delta();
        let warnings = warnings.into_delta();

        let mut errors = base_cell.write();
        apply_delta_to_base_map(&mut errors.local_errors, local_errors);
        apply_delta_to_base_map(&mut errors.duplicate_providers, duplicate_providers);
        apply_delta_to_base_map(&mut errors.merge_errors, merge_errors);
        apply_delta_to_base_map(&mut errors.warnings, warnings);
        errors.suppressions = suppressions;
    }
}

pub type Coverage = BTreeMap<FileKey, FileCoverage>;
pub type OverlayCoverage =
    OverlayMap<FileKey, FileCoverage, EnvCellMapBase<Coverage, FileKey, FileCoverage>>;

pub struct Env {
    /// All the files that we at least parse (includes libs).
    pub files: FlowOrdSet<FileKey>,
    pub dependency_info: DependencyInfo,
    /// All the current files we typecheck.
    pub checked_files: CheckedSet,
    /// package.json files
    pub package_json_files: FlowOrdSet<FileKey>,
    /// The lib files, in their merge order
    pub ordered_libs: Vec<(Option<FlowSmolStr>, FlowSmolStr)>,
    /// The lib files as a set
    pub all_unordered_libs: BTreeSet<FlowSmolStr>,
    /// The files which didn't parse (skipped or errored)
    pub unparsed: FlowOrdSet<FileKey>,
    pub errors: Arc<EnvCell<Errors>>,
    pub coverage: Arc<EnvCell<Coverage>>,
    pub collated_errors: Arc<EnvCell<CollatedErrors>>,
    pub connections: PersistentConnection,
    /// None means auto-imports are not enabled
    pub exports: Option<ExportSearch>,
    pub master_cx: Arc<MasterContext>,
}

pub type EnvRef = Arc<Env>;

impl Env {
    pub fn errors(&self) -> EnvCellReadGuard<Errors> {
        self.errors.read_arc()
    }

    pub fn coverage(&self) -> EnvCellReadGuard<Coverage> {
        self.coverage.read_arc()
    }

    pub fn collated_errors(&self) -> EnvCellReadGuard<CollatedErrors> {
        self.collated_errors.read_arc()
    }
}

pub struct EnvTransaction {
    env: EnvRef,
    files: Option<FlowOrdSet<FileKey>>,
    dependency_info: Option<DependencyInfo>,
    checked_files: Option<CheckedSet>,
    package_json_files: Option<FlowOrdSet<FileKey>>,
    ordered_libs: Option<Vec<(Option<FlowSmolStr>, FlowSmolStr)>>,
    all_unordered_libs: Option<BTreeSet<FlowSmolStr>>,
    unparsed: Option<FlowOrdSet<FileKey>>,
    errors: Option<OverlayErrors>,
    coverage: Option<OverlayCoverage>,
    collated_errors: Option<OverlayCollatedErrors>,
    connections: Option<PersistentConnection>,
    exports: Option<Option<ExportSearch>>,
    master_cx: Option<Arc<MasterContext>>,
}

impl EnvTransaction {
    pub fn new(env: EnvRef) -> Self {
        Self {
            env,
            files: None,
            dependency_info: None,
            checked_files: None,
            package_json_files: None,
            ordered_libs: None,
            all_unordered_libs: None,
            unparsed: None,
            errors: None,
            coverage: None,
            collated_errors: None,
            connections: None,
            exports: None,
            master_cx: None,
        }
    }

    pub fn files(&self) -> &FlowOrdSet<FileKey> {
        self.files.as_ref().unwrap_or(&self.env.files)
    }

    pub fn dependency_info(&self) -> &DependencyInfo {
        self.dependency_info
            .as_ref()
            .unwrap_or(&self.env.dependency_info)
    }

    pub fn checked_files(&self) -> &CheckedSet {
        self.checked_files
            .as_ref()
            .unwrap_or(&self.env.checked_files)
    }

    pub fn package_json_files(&self) -> &FlowOrdSet<FileKey> {
        self.package_json_files
            .as_ref()
            .unwrap_or(&self.env.package_json_files)
    }

    pub fn ordered_libs(&self) -> &[(Option<FlowSmolStr>, FlowSmolStr)] {
        self.ordered_libs.as_ref().unwrap_or(&self.env.ordered_libs)
    }

    pub fn all_unordered_libs(&self) -> &BTreeSet<FlowSmolStr> {
        self.all_unordered_libs
            .as_ref()
            .unwrap_or(&self.env.all_unordered_libs)
    }

    pub fn unparsed(&self) -> &FlowOrdSet<FileKey> {
        self.unparsed.as_ref().unwrap_or(&self.env.unparsed)
    }

    pub fn errors(&mut self) -> &OverlayErrors {
        if self.errors.is_none() {
            self.errors = Some(OverlayErrors::new(self.env.errors.dupe()));
        }
        self.errors.as_ref().expect("errors initialized")
    }

    pub fn coverage(&mut self) -> &OverlayCoverage {
        if self.coverage.is_none() {
            self.coverage = Some(overlay_coverage(self.env.coverage.dupe()));
        }
        self.coverage.as_ref().expect("coverage initialized")
    }

    pub fn collated_errors(&mut self) -> &OverlayCollatedErrors {
        if self.collated_errors.is_none() {
            self.collated_errors =
                Some(OverlayCollatedErrors::new(self.env.collated_errors.dupe()));
        }
        self.collated_errors
            .as_ref()
            .expect("collated errors initialized")
    }

    pub fn connections(&self) -> &PersistentConnection {
        self.connections.as_ref().unwrap_or(&self.env.connections)
    }

    pub fn exports(&self) -> Option<&ExportSearch> {
        self.exports.as_ref().unwrap_or(&self.env.exports).as_ref()
    }

    pub fn master_cx(&self) -> &Arc<MasterContext> {
        self.master_cx.as_ref().unwrap_or(&self.env.master_cx)
    }

    pub fn take_errors(&mut self) -> OverlayErrors {
        self.errors
            .take()
            .unwrap_or_else(|| OverlayErrors::new(self.env.errors.dupe()))
    }

    pub fn take_dependency_info(&mut self) -> DependencyInfo {
        self.dependency_info
            .take()
            .unwrap_or_else(|| self.env.dependency_info.clone())
    }

    pub fn take_coverage(&mut self) -> OverlayCoverage {
        self.coverage
            .take()
            .unwrap_or_else(|| overlay_coverage(self.env.coverage.dupe()))
    }

    pub fn take_collated_errors(&mut self) -> OverlayCollatedErrors {
        self.collated_errors
            .take()
            .unwrap_or_else(|| OverlayCollatedErrors::new(self.env.collated_errors.dupe()))
    }

    pub fn set_files(&mut self, files: FlowOrdSet<FileKey>) {
        self.files = Some(files);
    }

    pub fn set_dependency_info(&mut self, dependency_info: DependencyInfo) {
        self.dependency_info = Some(dependency_info);
    }

    pub fn set_checked_files(&mut self, checked_files: CheckedSet) {
        self.checked_files = Some(checked_files);
    }

    pub fn set_unparsed(&mut self, unparsed: FlowOrdSet<FileKey>) {
        self.unparsed = Some(unparsed);
    }

    pub fn set_errors(&mut self, errors: OverlayErrors) {
        self.errors = Some(errors);
    }

    pub fn set_coverage(&mut self, coverage: OverlayCoverage) {
        self.coverage = Some(coverage);
    }

    pub fn set_collated_errors(&mut self, collated_errors: OverlayCollatedErrors) {
        self.collated_errors = Some(collated_errors);
    }

    pub fn set_connections(&mut self, connections: PersistentConnection) {
        self.connections = Some(connections);
    }

    pub fn set_exports(&mut self, exports: Option<ExportSearch>) {
        self.exports = Some(exports);
    }

    pub fn commit(self) -> EnvRef {
        let EnvTransaction {
            env,
            files,
            dependency_info,
            checked_files,
            package_json_files,
            ordered_libs,
            all_unordered_libs,
            unparsed,
            errors,
            coverage,
            collated_errors,
            connections,
            exports,
            master_cx,
        } = self;
        let needs_new_env = files.is_some()
            || dependency_info.is_some()
            || checked_files.is_some()
            || package_json_files.is_some()
            || ordered_libs.is_some()
            || all_unordered_libs.is_some()
            || unparsed.is_some()
            || connections.is_some()
            || exports.is_some()
            || master_cx.is_some();

        if let Some(errors) = errors {
            errors.commit();
        }
        if let Some(coverage) = coverage {
            let coverage = coverage.into_delta();
            let mut base = env.coverage.write();
            apply_delta_to_base_map(&mut base, coverage);
        }
        if let Some(collated_errors) = collated_errors {
            collated_errors.commit();
        }

        if needs_new_env {
            Arc::new(Env {
                files: files.unwrap_or_else(|| env.files.dupe()),
                dependency_info: dependency_info.unwrap_or_else(|| env.dependency_info.clone()),
                checked_files: checked_files.unwrap_or_else(|| env.checked_files.dupe()),
                package_json_files: package_json_files
                    .unwrap_or_else(|| env.package_json_files.dupe()),
                ordered_libs: ordered_libs.unwrap_or_else(|| env.ordered_libs.clone()),
                all_unordered_libs: all_unordered_libs
                    .unwrap_or_else(|| env.all_unordered_libs.clone()),
                unparsed: unparsed.unwrap_or_else(|| env.unparsed.dupe()),
                errors: env.errors.dupe(),
                coverage: env.coverage.dupe(),
                collated_errors: env.collated_errors.dupe(),
                connections: connections.unwrap_or_else(|| env.connections.clone()),
                exports: exports.unwrap_or_else(|| env.exports.clone()),
                master_cx: master_cx.unwrap_or_else(|| env.master_cx.dupe()),
            })
        } else {
            env
        }
    }

    pub fn into_env(self) -> Env {
        let EnvTransaction {
            env,
            files,
            dependency_info,
            checked_files,
            package_json_files,
            ordered_libs,
            all_unordered_libs,
            unparsed,
            errors,
            coverage,
            collated_errors,
            connections,
            exports,
            master_cx,
        } = self;

        if let Some(errors) = errors {
            errors.commit();
        }
        if let Some(coverage) = coverage {
            let coverage = coverage.into_delta();
            let mut base = env.coverage.write();
            apply_delta_to_base_map(&mut base, coverage);
        }
        if let Some(collated_errors) = collated_errors {
            collated_errors.commit();
        }

        Env {
            files: files.unwrap_or_else(|| env.files.dupe()),
            dependency_info: dependency_info.unwrap_or_else(|| env.dependency_info.clone()),
            checked_files: checked_files.unwrap_or_else(|| env.checked_files.dupe()),
            package_json_files: package_json_files.unwrap_or_else(|| env.package_json_files.dupe()),
            ordered_libs: ordered_libs.unwrap_or_else(|| env.ordered_libs.clone()),
            all_unordered_libs: all_unordered_libs
                .unwrap_or_else(|| env.all_unordered_libs.clone()),
            unparsed: unparsed.unwrap_or_else(|| env.unparsed.dupe()),
            errors: env.errors.dupe(),
            coverage: env.coverage.dupe(),
            collated_errors: env.collated_errors.dupe(),
            connections: connections.unwrap_or_else(|| env.connections.clone()),
            exports: exports.unwrap_or_else(|| env.exports.clone()),
            master_cx: master_cx.unwrap_or_else(|| env.master_cx.dupe()),
        }
    }
}

pub fn env_cell<T>(value: T) -> Arc<EnvCell<T>> {
    Arc::new(EnvCell::new(value))
}

pub fn overlay_coverage(cell: Arc<EnvCell<Coverage>>) -> OverlayCoverage {
    OverlayMap::with_base(EnvCellMapBase::identity(cell))
}

pub fn with_connections(env: EnvRef, connections: PersistentConnection) -> EnvRef {
    let mut transaction = EnvTransaction::new(env);
    transaction.set_connections(connections);
    transaction.commit()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_errors() -> Errors {
        Errors {
            local_errors: BTreeMap::new(),
            duplicate_providers: BTreeMap::new(),
            merge_errors: BTreeMap::new(),
            warnings: BTreeMap::new(),
            suppressions: ErrorSuppressions::empty(),
        }
    }

    fn env_ref() -> EnvRef {
        Arc::new(Env {
            files: FlowOrdSet::new(),
            dependency_info: DependencyInfo::empty(),
            checked_files: CheckedSet::empty(),
            package_json_files: FlowOrdSet::new(),
            ordered_libs: Vec::new(),
            all_unordered_libs: BTreeSet::new(),
            unparsed: FlowOrdSet::new(),
            errors: env_cell(empty_errors()),
            coverage: env_cell(BTreeMap::new()),
            collated_errors: env_cell(CollatedErrors::empty()),
            connections: PersistentConnection(Vec::new()),
            exports: None,
            master_cx: Arc::new(MasterContext::EmptyMasterContext),
        })
    }

    #[test]
    fn unchanged_transaction_preserves_base_env() {
        let env = env_ref();
        let transaction = EnvTransaction::new(env.dupe());

        let committed = transaction.into_env();

        assert_eq!(env.files.len(), committed.files.len());
        assert_eq!(env.connections.0, committed.connections.0);
    }

    #[test]
    fn updated_checked_files_do_not_mutate_base_env() {
        let env = env_ref();
        let file = FileKey::source_file_of_absolute("/tmp/a.js");
        let mut checked_files = CheckedSet::empty();
        checked_files.add(Some([file].into_iter().collect()), None, None);

        let mut transaction = EnvTransaction::new(env.dupe());
        transaction.set_checked_files(checked_files);
        let committed = transaction.into_env();

        assert_eq!(env.checked_files.focused_cardinal(), 0);
        assert_eq!(committed.checked_files.focused_cardinal(), 1);
    }

    #[test]
    fn connection_update_preserves_other_fields() {
        let env = env_ref();
        let file = FileKey::source_file_of_absolute("/tmp/a.js");
        let mut files = FlowOrdSet::new();
        files.insert(file);
        let mut transaction = EnvTransaction::new(env);
        transaction.set_files(files);
        let env = Arc::new(transaction.into_env());

        let committed = with_connections(env.dupe(), PersistentConnection(vec![42]));

        assert_eq!(env.connections.0, Vec::<i32>::new());
        assert_eq!(committed.connections.0, vec![42]);
        assert_eq!(committed.files.len(), 1);
    }
}
