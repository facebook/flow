/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use flow_server_env::server_prot;
use flow_services_coverage::FileCoverage;
use serde_json::json;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "batch-coverage",
        "Shows aggregate coverage information for a group of files or directories ",
        command_spec::Visibility::Public,
        format!(
            "Usage: {exe_name} batch-coverage [OPTION]... [FILE...] \n\ne.g. {exe_name} batch-coverage foo.js bar.js baz.js dirname1 dirname2 --show-all \nor   {exe_name} batch-coverage --input-file filenames.txt\n"
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_flags_no_lazy(spec);
    let spec = command_utils::add_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
        "File containing list of files or directories to compute coverage for, one per line. If -, the list is read from standard input.",
        None,
    )
    .flag(
        "--show-all",
        &arg_spec::truthy(),
        "Whether to output the coverage for all files. If not specified, this command will only print coverage for 50 files.",
        None,
    )
    .anon("FILE...", &arg_spec::list_of(arg_spec::string()))
}

fn output_results(
    root: &Path,
    strip_root: bool,
    json: bool,
    pretty: bool,
    show_all: bool,
    stats: Vec<(flow_parser::file_key::FileKey, FileCoverage)>,
) {
    fn percent(top: i32, bottom: i32) -> f64 {
        if bottom == 0 {
            0.0
        } else {
            top as f64 / bottom as f64 * 100.0
        }
    }

    // Compute aggregate stats
    let (covered, any, empty) = stats.iter().fold(
        (0, 0, 0),
        |(acc_checked, acc_any, acc_empty), (_, coverage)| {
            (
                acc_checked + coverage.checked,
                acc_any + coverage.uncovered,
                acc_empty + coverage.empty,
            )
        },
    );
    let num_files_in_dir = stats.len();
    let total = covered + any + empty;
    let percentage = percent(covered, total);
    let file_stats = |stats: &(flow_parser::file_key::FileKey, FileCoverage)| {
        let (file_key, coverage) = stats;
        let total = coverage.checked + coverage.uncovered + coverage.empty;
        let percentage = percent(coverage.checked, total);
        let file = file_key.to_absolute();
        let file = if strip_root {
            flow_common::files::relative_path(root, &file)
        } else {
            file
        };
        (file, coverage.checked, total, percentage)
    };

    if json {
        let mut sorted_stats = stats.iter().map(file_stats).collect::<Vec<_>>();
        sorted_stats.sort_by(
            |(a, _, _, _): &(String, i32, i32, f64), (b, _, _, _): &(String, i32, i32, f64)| {
                a.cmp(b)
            },
        );
        let file_list: Vec<serde_json::Value> = sorted_stats
            .into_iter()
            .map(
                |(file, covered, total, percentage): (String, i32, i32, f64)| {
                    json!({
                        "file": file,
                        "percentage": format!("{percentage:.2}").parse::<f64>().unwrap(),
                        "covered": covered,
                        "total": total,
                    })
                },
            )
            .collect();
        let json_output = json!({
            "files": file_list,
            "statistics": {
                "files_in_directory": num_files_in_dir,
                "covered_expressions": covered,
                "total_expressions": total,
                "percentage": format!("{percentage:.2}").parse::<f64>().unwrap(),
            },
        });
        flow_hh_json::print_json_endline(pretty, &json_output);
        return;
    }

    let (truncation_text, truncated_stats) = if num_files_in_dir > 50 && !show_all {
        (
            format!(
                "\nOnly showing coverage for 50 of {} files. To show more, rerun with --show-all.\n",
                num_files_in_dir
            ),
            stats.into_iter().take(50).collect::<Vec<_>>(),
        )
    } else {
        (String::new(), stats)
    };

    if num_files_in_dir > 0 {
        println!("\nCoverage results from {} file(s):\n", num_files_in_dir);
        for stats in &truncated_stats {
            let (file, covered, total, percentage) = file_stats(stats);
            println!("{file}: {percentage:.2}% ({covered} of {total} expressions)");
        }
        println!("{truncation_text}");
    }

    println!("-----------------------------------");
    println!("Aggregate coverage statistics");
    println!("-----------------------------------");
    println!("Files                : {}", num_files_in_dir);
    println!("Expressions          :");
    println!("  Covered            : {}", covered);
    println!("  Total              : {}", total);
    println!("  Covered Percentage : {percentage:.2}%");
    println!();
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let show_all = command_spec::get(args, "--show-all", &arg_spec::truthy()).unwrap();
    let files = command_spec::get(args, "FILE...", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();

    let batch = command_utils::get_filenames_from_input(false, input_file.as_deref(), Some(&files))
        .into_iter()
        .map(|filename| command_utils::expand_path(&filename))
        .collect::<Vec<_>>();
    let input = batch
        .first()
        .map(|filename| flow_server_utils::file_input::FileInput::FileName(filename.clone()));
    let root = command_utils::get_the_root(&flowconfig_name, root_arg.as_deref(), input.as_ref());
    // pretty implies json
    let json = json_flags.json || json_flags.pretty;
    let request = server_prot::request::Command::BATCH_COVERAGE {
        batch,
        wait_for_recheck,
    };
    let response =
        command_utils::connect_and_make_request(&flowconfig_name, &connect_flags, &root, &request);
    match response {
        server_prot::response::Response::BATCH_COVERAGE(Err(msg)) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError);
        }
        server_prot::response::Response::BATCH_COVERAGE(Ok(response)) => {
            output_results(
                &root,
                strip_root,
                json,
                json_flags.pretty,
                show_all,
                response,
            );
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
