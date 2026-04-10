/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use flow_aloc::ALoc;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_typing_errors::intermediate_error_types::IntermediateError;

#[derive(Clone, Copy)]
pub struct ErrorStateTimestamps {
    pub timestamp_start_of_non_zero_type_errors: Option<f64>,
    pub timestamp_start_of_non_zero_type_errors_all_in_one_file: Option<f64>,
    pub timestamp_start_of_non_zero_subtyping_errors: Option<f64>,
    pub timestamp_start_of_non_zero_subtyping_errors_all_in_one_file: Option<f64>,
}

impl ErrorStateTimestamps {
    pub fn empty() -> Self {
        ErrorStateTimestamps {
            timestamp_start_of_non_zero_type_errors: None,
            timestamp_start_of_non_zero_type_errors_all_in_one_file: None,
            timestamp_start_of_non_zero_subtyping_errors: None,
            timestamp_start_of_non_zero_subtyping_errors_all_in_one_file: None,
        }
    }
}

#[derive(Clone)]
pub struct CollatedErrors {
    pub collated_duplicate_providers_errors: Vec<(PrintableError<Loc>, FileKey, FileKey)>,
    pub collated_local_errors: BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
    pub collated_merge_errors: BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
    pub collated_warning_map: BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
    pub collated_suppressed_errors:
        BTreeMap<FileKey, Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>>,
    pub error_state_timestamps: ErrorStateTimestamps,
}

impl CollatedErrors {
    pub fn empty() -> Self {
        CollatedErrors {
            collated_duplicate_providers_errors: Vec::new(),
            collated_local_errors: BTreeMap::new(),
            collated_merge_errors: BTreeMap::new(),
            collated_warning_map: BTreeMap::new(),
            collated_suppressed_errors: BTreeMap::new(),
            error_state_timestamps: ErrorStateTimestamps::empty(),
        }
    }

    pub fn clear_all(&mut self, files: &FlowOrdSet<FileKey>) {
        for file in files {
            self.collated_duplicate_providers_errors
                .retain(|(_, f1, f2)| f1 != file && f2 != file);
            self.collated_local_errors.remove(file);
            self.collated_merge_errors.remove(file);
            self.collated_warning_map.remove(file);
            self.collated_suppressed_errors.remove(file);
        }
    }

    pub fn clear_merge(&mut self, files: &FlowOrdSet<FileKey>) {
        for file in files {
            self.collated_merge_errors.remove(file);
            self.collated_warning_map.remove(file);
            self.collated_suppressed_errors.remove(file);
        }
    }
}
