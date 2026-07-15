/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::io;
use std::path::Path;

use crate::flow_result::FlowError;
use crate::flow_result::FlowLoc;
use crate::flow_result::FlowResult;
use crate::utils::r#async::exec_manual;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ErrorCheckCommand {
    Check,
    FullCheck,
    Status,
}

fn get_flow_errors_impl(
    bin: &str,
    error_check_command: ErrorCheckCommand,
    root: &Path,
    with_warnings: bool,
    flowconfig_name: &str,
) -> io::Result<FlowResult> {
    let args = flow_args_for_errors(error_check_command, with_warnings, root, flowconfig_name);
    let output = exec_manual(bin, &args, Some(root), None)?;
    if output.status.success() || output.status.code() == Some(2) {
        serde_json::from_slice(&output.stdout).map_err(io_other)
    } else {
        Err(io::Error::other(format!(
            "Flow check failed with status {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        )))
    }
}

pub(crate) fn get_flow_errors_with_warnings(
    bin: &str,
    error_check_command: ErrorCheckCommand,
    root: &Path,
    flowconfig_name: &str,
) -> io::Result<FlowResult> {
    get_flow_errors_impl(bin, error_check_command, root, true, flowconfig_name)
}

pub(crate) fn get_flow_errors(
    bin: &str,
    error_check_command: ErrorCheckCommand,
    root: &Path,
    flowconfig_name: &str,
) -> io::Result<FlowResult> {
    get_flow_errors_impl(bin, error_check_command, root, false, flowconfig_name)
}

pub(crate) fn is_unused_suppression(error: &FlowError) -> bool {
    match error.message.as_slice() {
        [first, second, ..]
            if first.descr == "Error suppressing comment"
                && second.descr == "Unused suppression" =>
        {
            true
        }
        [first, ..] => first.descr.starts_with("Unused suppression comment."),
        [] => false,
    }
}

pub(crate) fn get_unused_suppression_errors(
    bin: &str,
    error_check_command: ErrorCheckCommand,
    root: &Path,
    flowconfig_name: &str,
) -> io::Result<Vec<FlowError>> {
    let result = get_flow_errors_with_warnings(bin, error_check_command, root, flowconfig_name)?;

    Ok(result
        .errors
        .into_iter()
        .filter(is_unused_suppression)
        .collect())
}

pub(crate) fn collate_locs(errors: Vec<FlowError>) -> BTreeMap<String, Vec<FlowLoc>> {
    collate_errors(errors)
        .into_iter()
        .map(|(file, errors)| {
            let locs = errors
                .into_iter()
                .filter_map(|error| {
                    error
                        .message
                        .first()
                        .and_then(|message| message.loc.clone())
                })
                .collect();
            (file, locs)
        })
        .collect()
}

pub(crate) fn main_source_loc_of_error(error: &FlowError) -> Option<FlowLoc> {
    error
        .operation
        .iter()
        .chain(error.message.iter())
        .filter_map(|message| message.loc.as_ref())
        .find(|loc| loc.source_type.as_deref() == Some("SourceFile"))
        .cloned()
}

pub(crate) fn filter_errors(errors: &[FlowError]) -> Vec<FlowError> {
    errors
        .iter()
        .filter(|error| main_source_loc_of_error(error).is_some())
        .cloned()
        .collect()
}

pub(crate) fn collate_errors(errors: Vec<FlowError>) -> BTreeMap<String, Vec<FlowError>> {
    let mut errors_by_file: BTreeMap<String, Vec<FlowError>> = BTreeMap::new();
    for error in errors {
        let Some(message) = error.message.first() else {
            continue;
        };
        let Some(loc) = &message.loc else {
            continue;
        };
        let Some(source) = loc.source.clone() else {
            continue;
        };
        errors_by_file.entry(source).or_default().push(error);
    }
    errors_by_file
}

fn io_other(err: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> io::Error {
    io::Error::other(err)
}

fn flow_args_for_errors(
    error_check_command: ErrorCheckCommand,
    with_warnings: bool,
    root: &Path,
    flowconfig_name: &str,
) -> Vec<String> {
    let mut args = match error_check_command {
        ErrorCheckCommand::Check | ErrorCheckCommand::FullCheck => {
            vec!["full-check".to_string(), "--json".to_string()]
        }
        ErrorCheckCommand::Status => vec![
            "status".to_string(),
            "--no-auto-start".to_string(),
            "--json".to_string(),
        ],
    };
    if with_warnings {
        args.push("--include-warnings".to_string());
    }
    args.extend([
        "--flowconfig-name".to_string(),
        flowconfig_name.to_string(),
        root.to_string_lossy().to_string(),
    ]);
    args
}
