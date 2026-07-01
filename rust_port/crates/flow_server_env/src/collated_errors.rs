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
use flow_aloc::ALoc;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::overlay_map::EnvCell;
use flow_data_structure_wrapper::overlay_map::EnvCellMapBase;
use flow_data_structure_wrapper::overlay_map::OverlayMap;
use flow_data_structure_wrapper::overlay_map::OverlayMapDelta;
use flow_data_structure_wrapper::overlay_map::apply_delta_to_base_map;
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
}

impl Default for CollatedErrors {
    fn default() -> Self {
        Self::empty()
    }
}

impl CollatedErrors {
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

type CollatedErrorMap = OverlayMap<
    FileKey,
    ConcreteLocPrintableErrorSet,
    EnvCellMapBase<CollatedErrors, FileKey, ConcreteLocPrintableErrorSet>,
>;
type CollatedSuppressedErrorMap = OverlayMap<
    FileKey,
    Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>,
    EnvCellMapBase<CollatedErrors, FileKey, Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>>,
>;

#[derive(Clone)]
pub struct OverlayCollatedErrors {
    base_cell: Arc<EnvCell<CollatedErrors>>,
    pub collated_duplicate_providers_errors: Vec<(PrintableError<Loc>, FileKey, FileKey)>,
    pub collated_local_errors: CollatedErrorMap,
    pub collated_merge_errors: CollatedErrorMap,
    pub collated_warning_map: CollatedErrorMap,
    pub collated_suppressed_errors: CollatedSuppressedErrorMap,
    pub error_state_timestamps: ErrorStateTimestamps,
}

struct CollatedErrorsDelta {
    collated_duplicate_providers_errors: Vec<(PrintableError<Loc>, FileKey, FileKey)>,
    collated_local_errors: OverlayMapDelta<FileKey, ConcreteLocPrintableErrorSet>,
    collated_merge_errors: OverlayMapDelta<FileKey, ConcreteLocPrintableErrorSet>,
    collated_warning_map: OverlayMapDelta<FileKey, ConcreteLocPrintableErrorSet>,
    collated_suppressed_errors:
        OverlayMapDelta<FileKey, Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>>,
    error_state_timestamps: ErrorStateTimestamps,
}

impl CollatedErrorsDelta {
    fn apply_to_base(self, base: &mut CollatedErrors) {
        let Self {
            collated_duplicate_providers_errors,
            collated_local_errors,
            collated_merge_errors,
            collated_warning_map,
            collated_suppressed_errors,
            error_state_timestamps,
        } = self;

        base.collated_duplicate_providers_errors = collated_duplicate_providers_errors;
        apply_delta_to_base_map(&mut base.collated_local_errors, collated_local_errors);
        apply_delta_to_base_map(&mut base.collated_merge_errors, collated_merge_errors);
        apply_delta_to_base_map(&mut base.collated_warning_map, collated_warning_map);
        apply_delta_to_base_map(
            &mut base.collated_suppressed_errors,
            collated_suppressed_errors,
        );
        base.error_state_timestamps = error_state_timestamps;
    }
}

impl OverlayCollatedErrors {
    pub fn new(base_cell: Arc<EnvCell<CollatedErrors>>) -> Self {
        let (collated_duplicate_providers_errors, error_state_timestamps) = {
            let base = base_cell.read();
            (
                base.collated_duplicate_providers_errors.clone(),
                base.error_state_timestamps,
            )
        };
        Self {
            base_cell: base_cell.dupe(),
            collated_duplicate_providers_errors,
            collated_local_errors: OverlayMap::with_base(EnvCellMapBase::new(
                base_cell.dupe(),
                |errors| &errors.collated_local_errors,
            )),
            collated_merge_errors: OverlayMap::with_base(EnvCellMapBase::new(
                base_cell.dupe(),
                |errors| &errors.collated_merge_errors,
            )),
            collated_warning_map: OverlayMap::with_base(EnvCellMapBase::new(
                base_cell.dupe(),
                |errors| &errors.collated_warning_map,
            )),
            collated_suppressed_errors: OverlayMap::with_base(EnvCellMapBase::new(
                base_cell,
                |errors| &errors.collated_suppressed_errors,
            )),
            error_state_timestamps,
        }
    }

    pub fn from_collated_errors(collated_errors: CollatedErrors) -> Self {
        Self::new(Arc::new(EnvCell::new(collated_errors)))
    }

    fn into_delta(self) -> (Arc<EnvCell<CollatedErrors>>, CollatedErrorsDelta) {
        let OverlayCollatedErrors {
            base_cell,
            collated_duplicate_providers_errors,
            collated_local_errors,
            collated_merge_errors,
            collated_warning_map,
            collated_suppressed_errors,
            error_state_timestamps,
        } = self;
        let delta = CollatedErrorsDelta {
            collated_duplicate_providers_errors,
            collated_local_errors: collated_local_errors.into_delta(),
            collated_merge_errors: collated_merge_errors.into_delta(),
            collated_warning_map: collated_warning_map.into_delta(),
            collated_suppressed_errors: collated_suppressed_errors.into_delta(),
            error_state_timestamps,
        };
        (base_cell, delta)
    }

    pub fn apply_to_base(self, base: &mut CollatedErrors) {
        let (_base_cell, delta) = self.into_delta();
        delta.apply_to_base(base);
    }

    pub fn commit(self) {
        let (base_cell, delta) = self.into_delta();
        let mut base = base_cell.write();
        delta.apply_to_base(&mut base);
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
