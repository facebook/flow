/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use flow_common::options::Options;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
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
use crate::dependency_info::DependencyInfo;
use crate::persistent_connection::PersistentConnection;

// The "static" environment, initialized first and then unchanged.

pub struct Genv {
    pub options: Arc<Options>,
    pub workers: Option<ThreadPool>,
    pub shared_mem: Arc<SharedMem>,
    pub node_modules_containers: Arc<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>,
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

#[derive(Clone)]
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
    pub errors: Errors,
    pub coverage: BTreeMap<FileKey, FileCoverage>,
    pub collated_errors: CollatedErrors,
    pub connections: PersistentConnection,
    /// None means auto-imports are not enabled
    pub exports: Option<ExportSearch>,
    pub master_cx: Arc<MasterContext>,
}
