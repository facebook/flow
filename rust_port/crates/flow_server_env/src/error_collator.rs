/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::options::Options;
use flow_common_errors::error_codes::ErrorCode;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_common_errors::error_utils::code_of_printable_error;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_typing_errors::error_message::EDuplicateModuleProviderData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::flow_error::error_of_msg;
use flow_typing_errors::intermediate_error::make_intermediate_error;
use flow_typing_errors::intermediate_error::to_printable_error;
use flow_typing_errors::intermediate_error_types::IntermediateError;

use crate::collated_errors::CollatedErrors;
use crate::collated_errors::ErrorStateTimestamps;
use crate::monitor_rpc;
use crate::server_env::Env;
use crate::server_env::Errors;
use crate::server_status;

fn add_suppression_warnings(
    root: &Path,
    checked: &CheckedSet,
    unused: &ErrorSuppressions,
    warnings: &mut BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
) {
    // For each unused suppression, create an warning
    let all_locs = unused.all_unused_locs();
    for loc in &all_locs {
        let source_file = match &loc.source {
            Some(x) => x.dupe(),
            None => FileKey::new(FileKeyInner::SourceFile("-".to_string())),
        };
        // In lazy mode, dependencies are modules which we typecheck not because we care about
        // them, but because something important (a focused file or a focused file's dependent)
        // needs these dependencies. Therefore, we might not typecheck a dependency's dependents.
        //
        // This means there might be an unused suppression comment warning in a dependency which
        // only shows up in lazy mode. To avoid this, we'll just avoid raising this kind of
        // warning in any dependency.
        if !checked.mem_dependency(&source_file) {
            let msg = ErrorMessage::EUnusedSuppression(loc.dupe());
            let flow_error = error_of_msg(source_file.dupe(), msg);
            let intermediate = make_intermediate_error(Loc::dupe, false, &flow_error);
            let err = to_printable_error(
                Loc::dupe,
                |_: &FileKey| -> Option<Arc<Program<Loc, Loc>>> { None },
                Some(root),
                intermediate,
            );
            let file_warnings = warnings
                .entry(source_file)
                .or_insert_with(ConcreteLocPrintableErrorSet::empty);
            file_warnings.add(err);
        }
    }
}

fn collate_duplicate_providers(
    root: &Path,
    duplicate_providers: &BTreeMap<FlowSmolStr, (FileKey, vec1::Vec1<FileKey>)>,
    acc: &mut Vec<(PrintableError<Loc>, FileKey, FileKey)>,
) {
    let pos = Position { line: 1, column: 0 };
    for (module_name, (provider_file, duplicates)) in duplicate_providers {
        let provider = Loc {
            source: Some(provider_file.dupe()),
            start: pos,
            end: pos,
        };
        for duplicate in duplicates {
            let conflict = Loc {
                source: Some(duplicate.dupe()),
                start: pos,
                end: pos,
            };
            let msg =
                ErrorMessage::EDuplicateModuleProvider(Box::new(EDuplicateModuleProviderData {
                    module_name: module_name.dupe(),
                    provider: provider.dupe(),
                    conflict,
                }));
            let flow_error = error_of_msg(duplicate.dupe(), msg);
            let intermediate = make_intermediate_error(Loc::dupe, false, &flow_error);
            let err = to_printable_error(
                Loc::dupe,
                |_: &FileKey| -> Option<Arc<Program<Loc, Loc>>> { None },
                Some(root),
                intermediate,
            );
            acc.push((err, provider_file.dupe(), duplicate.dupe()));
        }
    }
}

pub fn update_local_collated_errors<F, G>(
    loc_of_aloc: &F,
    get_ast: &G,
    root: &Path,
    file_options: &flow_common::files::FileOptions,
    node_modules_errors: bool,
    unsuppressable_error_codes: &BTreeSet<FlowSmolStr>,
    suppressions: &ErrorSuppressions,
    errors: &BTreeMap<FileKey, ErrorSet>,
    acc: &mut CollatedErrors,
) where
    F: Fn(&ALoc) -> Loc,
    G: Fn(&FileKey) -> Option<Arc<Program<Loc, Loc>>>,
{
    for (filename, file_errs) in errors {
        let file_options = Some(file_options);
        // It is okay to ignore suppressions here, since local errors, ie. parse
        // errors, parse exceptions and docblock errors, are insuppressible.
        let mut unused = ErrorSuppressions::empty();
        let (file_errs, _file_suppressed) = suppressions.filter_suppressed_errors(
            root,
            file_options,
            node_modules_errors,
            unsuppressable_error_codes,
            loc_of_aloc,
            get_ast,
            file_errs.dupe(),
            &mut unused,
        );
        acc.collated_local_errors.insert(filename.dupe(), file_errs);
    }
}

// Note on suppressions: The suppressions within the Server.errors parameter
// only account for the files that were most recently checked. Due to bugs in the
// checker it is possible for errors that are included in that same set to appear
// in files other than the ones in checked_files. To effectively suppress these
// we need to pass in an additional `all_suppressions` parameter. This is the
// suppression set we pass over to Error_suppressions.filter_suppressed_errors.
// Finally, we compute unused suppression warnings over the set of suppressions
// that we found during checking (not all_suppressions).
pub fn update_collated_errors<F, G>(
    loc_of_aloc: &F,
    get_ast: &G,
    options: &Options,
    checked_files: &CheckedSet,
    all_suppressions: &ErrorSuppressions,
    errors: &Errors,
    acc: &mut CollatedErrors,
) where
    F: Fn(&ALoc) -> Loc,
    G: Fn(&FileKey) -> Option<Arc<Program<Loc, Loc>>>,
{
    let root = &*options.root;
    let node_modules_errors = options.node_modules_errors;
    let unsuppressable_error_codes: BTreeSet<FlowSmolStr> =
        options.unsuppressable_error_codes.iter().cloned().collect();
    let Errors {
        local_errors,
        duplicate_providers,
        merge_errors,
        warnings,
        suppressions,
    } = errors;

    let acc_fun =
        |filename: &FileKey,
         file_errs: &ErrorSet,
         errors: &mut BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
         suppressed: &mut BTreeMap<FileKey, Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>>,
         unused: &mut ErrorSuppressions| {
            let file_options = Some(&*options.file_options);
            let (file_errs, file_suppressed) = all_suppressions.filter_suppressed_errors(
                root,
                file_options,
                node_modules_errors,
                &unsuppressable_error_codes,
                loc_of_aloc,
                get_ast,
                file_errs.clone(),
                unused,
            );
            errors.insert(filename.dupe(), file_errs);
            suppressed.insert(filename.dupe(), file_suppressed);
        };

    monitor_rpc::status_update(server_status::Event::CollatingErrorsStart);

    collate_duplicate_providers(
        root,
        duplicate_providers,
        &mut acc.collated_duplicate_providers_errors,
    );

    let mut unused = suppressions.clone();
    for (filename, file_errs) in local_errors {
        acc_fun(
            filename,
            file_errs,
            &mut acc.collated_local_errors,
            &mut acc.collated_suppressed_errors,
            &mut unused,
        );
    }

    for (filename, file_errs) in merge_errors {
        acc_fun(
            filename,
            file_errs,
            &mut acc.collated_merge_errors,
            &mut acc.collated_suppressed_errors,
            &mut unused,
        );
    }

    let mut warning_results: BTreeMap<FileKey, ConcreteLocPrintableErrorSet> = BTreeMap::new();
    for (filename, file_errs) in warnings {
        acc_fun(
            filename,
            file_errs,
            &mut warning_results,
            &mut acc.collated_suppressed_errors,
            &mut unused,
        );
    }

    // Compute "unused suppression warnings" based on the suppression set that
    // emerged from the current checked set, not the entire set of suppressions.
    add_suppression_warnings(root, checked_files, &unused, &mut warning_results);
    for (file, errs) in warning_results {
        acc.collated_warning_map
            .entry(file)
            .and_modify(|existing| existing.union(&errs))
            .or_insert(errs);
    }
}

fn get_with_separate_warnings_internal(
    env: &Env,
) -> (
    ConcreteLocPrintableErrorSet,
    &BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
    &BTreeMap<FileKey, Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>>,
) {
    let collated_errors = &env.collated_errors;
    let CollatedErrors {
        collated_duplicate_providers_errors,
        collated_local_errors,
        collated_merge_errors,
        collated_warning_map,
        collated_suppressed_errors,
        error_state_timestamps: _,
    } = collated_errors;
    let mut collated_errorset = ConcreteLocPrintableErrorSet::empty();
    for (err, _, _) in collated_duplicate_providers_errors {
        collated_errorset.add(err.clone());
    }
    for errs in collated_local_errors.values() {
        collated_errorset.union(errs);
    }
    for errs in collated_merge_errors.values() {
        collated_errorset.union(errs);
    }
    (
        collated_errorset,
        collated_warning_map,
        collated_suppressed_errors,
    )
}

fn type_error_stat(collated_errors: &CollatedErrors) -> (bool, bool, bool, bool) {
    let collated_merge_errors = &collated_errors.collated_merge_errors;
    let (
        have_type_errors,
        all_type_errors_in_one_file,
        have_subtyping_errors,
        all_subtyping_errors_in_one_file,
    ) = collated_merge_errors.values().fold(
        (false, true, false, true),
        |(
            have_type_errors_before,
            all_errors_in_one_file,
            have_subtyping_errors_before,
            all_subtyping_errors_in_one_file,
        ),
         errors| {
            let have_type_errors_in_file = !errors.is_empty();
            let all_type_errors_in_one_file =
                all_errors_in_one_file && !(have_type_errors_before && have_type_errors_in_file);
            let have_subtyping_errors_in_file = errors.exists(|error| {
                matches!(
                    code_of_printable_error(error),
                    Some(
                        ErrorCode::IncompatibleExact
                            | ErrorCode::IncompatibleFunctionIndexer
                            | ErrorCode::IncompatibleIndexer
                            | ErrorCode::IncompatibleType
                            | ErrorCode::IncompatibleTypeGuard
                            | ErrorCode::IncompatibleUse
                            | ErrorCode::IncompatibleVariance
                            | ErrorCode::PropMissing
                    )
                )
            });
            let all_subtyping_errors_in_one_file = all_subtyping_errors_in_one_file
                && !(have_subtyping_errors_before && have_subtyping_errors_in_file);
            (
                have_type_errors_before || have_type_errors_in_file,
                all_type_errors_in_one_file,
                have_subtyping_errors_before || have_subtyping_errors_in_file,
                all_subtyping_errors_in_one_file,
            )
        },
    );
    let have_type_errors_and_all_in_one_file = have_type_errors && all_type_errors_in_one_file;
    let have_subtyping_errors_and_all_in_one_file =
        have_subtyping_errors && all_subtyping_errors_in_one_file;
    (
        have_type_errors,
        have_type_errors_and_all_in_one_file,
        have_subtyping_errors,
        have_subtyping_errors_and_all_in_one_file,
    )
}

pub struct ErrorResolutionStat {
    pub time_to_resolve_all_type_errors: Option<f64>,
    pub time_to_resolve_all_type_errors_in_one_file: Option<f64>,
    pub time_to_resolve_all_subtyping_errors: Option<f64>,
    pub time_to_resolve_all_subtyping_errors_in_one_file: Option<f64>,
}

pub fn update_error_state_timestamps(collated_errors: &mut CollatedErrors) -> ErrorResolutionStat {
    let current_time = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    let init_timestamps = collated_errors.error_state_timestamps;
    let (
        have_type_errors,
        have_type_errors_and_all_in_one_file,
        have_subtyping_errors,
        have_subtyping_errors_and_all_in_one_file,
    ) = type_error_stat(collated_errors);
    let timestamp_start_of_non_zero_type_errors = if have_type_errors {
        init_timestamps
            .timestamp_start_of_non_zero_type_errors
            .or(Some(current_time))
    } else {
        None
    };
    let timestamp_start_of_non_zero_type_errors_all_in_one_file =
        if have_type_errors_and_all_in_one_file {
            init_timestamps
                .timestamp_start_of_non_zero_type_errors_all_in_one_file
                .or(Some(current_time))
        } else {
            None
        };
    let timestamp_start_of_non_zero_subtyping_errors = if have_subtyping_errors {
        init_timestamps
            .timestamp_start_of_non_zero_subtyping_errors
            .or(Some(current_time))
    } else {
        None
    };
    let timestamp_start_of_non_zero_subtyping_errors_all_in_one_file =
        if have_subtyping_errors_and_all_in_one_file {
            init_timestamps
                .timestamp_start_of_non_zero_subtyping_errors_all_in_one_file
                .or(Some(current_time))
        } else {
            None
        };
    let time_to_resolve_errors =
        |init_error_time: Option<f64>, current_error_time: Option<f64>| -> Option<f64> {
            match (init_error_time, current_error_time) {
                // If we start with no errors, then we are not resolving any errors.
                (None, _) => None,
                // If we end with errors, then we are not fully resolving all errors.
                (_, Some(_)) => None,
                (Some(t_start), None) => Some(current_time - t_start),
            }
        };
    let time_to_resolve_all_type_errors = time_to_resolve_errors(
        init_timestamps.timestamp_start_of_non_zero_type_errors,
        timestamp_start_of_non_zero_type_errors,
    );
    let time_to_resolve_all_type_errors_in_one_file = time_to_resolve_errors(
        init_timestamps.timestamp_start_of_non_zero_type_errors_all_in_one_file,
        timestamp_start_of_non_zero_type_errors,
    );
    let time_to_resolve_all_subtyping_errors = time_to_resolve_errors(
        init_timestamps.timestamp_start_of_non_zero_subtyping_errors,
        timestamp_start_of_non_zero_subtyping_errors,
    );
    let time_to_resolve_all_subtyping_errors_in_one_file = time_to_resolve_errors(
        init_timestamps.timestamp_start_of_non_zero_subtyping_errors_all_in_one_file,
        timestamp_start_of_non_zero_subtyping_errors,
    );
    collated_errors.error_state_timestamps = ErrorStateTimestamps {
        timestamp_start_of_non_zero_type_errors,
        timestamp_start_of_non_zero_type_errors_all_in_one_file,
        timestamp_start_of_non_zero_subtyping_errors,
        timestamp_start_of_non_zero_subtyping_errors_all_in_one_file,
    };
    ErrorResolutionStat {
        time_to_resolve_all_type_errors,
        time_to_resolve_all_type_errors_in_one_file,
        time_to_resolve_all_subtyping_errors,
        time_to_resolve_all_subtyping_errors_in_one_file,
    }
}

pub fn get_without_suppressed(
    env: &Env,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let (errors, warning_map, _suppressed_errors) = get_with_separate_warnings_internal(env);
    let mut warnings = ConcreteLocPrintableErrorSet::empty();
    for errs in warning_map.values() {
        warnings.union(errs);
    }
    (errors, warnings)
}

pub fn get(
    env: &Env,
) -> (
    ConcreteLocPrintableErrorSet,
    ConcreteLocPrintableErrorSet,
    Vec<(IntermediateError<ALoc>, BTreeSet<Loc>)>,
) {
    let (errors, warning_map, suppressed_errors) = get_with_separate_warnings_internal(env);
    let mut warnings = ConcreteLocPrintableErrorSet::empty();
    for errs in warning_map.values() {
        warnings.union(errs);
    }
    let mut all_suppressed = Vec::new();
    for errs in suppressed_errors.values() {
        all_suppressed.extend(errs.iter().cloned());
    }
    (errors, warnings, all_suppressed)
}

pub fn get_with_separate_warnings(
    env: &Env,
) -> (
    ConcreteLocPrintableErrorSet,
    &BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
) {
    let (collated_errorset, collated_warning_map, _collated_suppressed_errors) =
        get_with_separate_warnings_internal(env);
    (collated_errorset, collated_warning_map)
}
