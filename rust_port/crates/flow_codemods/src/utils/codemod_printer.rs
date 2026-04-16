/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use flow_parser::file_key::FileKey;
use flow_parser_utils_output::replacement_printer;
use flow_parser_utils_output::replacement_printer::Patch;
use flow_server_utils::file_input::FileInput;

use super::codemod_report::Reporter;
use super::codemod_report::ReporterOptions;

fn diff_heaps_get_diff(file: &FileKey) -> Option<Patch> {
    super::diff_heaps::get_diff(file)
}

pub fn print_results<A>(reporter_options: &ReporterOptions, report: &Reporter<A>, result: &A) {
    match report {
        Reporter::StringReporter(r) => {
            println!(
                ">>> Launching report...\n\n{}\n",
                r(reporter_options, result)
            );
        }
        Reporter::UnitReporter(r) => {
            println!(">>> Launching report...");
            r(reporter_options, result);
        }
    }
}

pub fn print_ast_file_dry(strip_root: &Option<PathBuf>, file: &FileKey) {
    let file_path = file.to_absolute();
    let file_input = FileInput::FileName(file_path);
    let diff = diff_heaps_get_diff(file);
    match diff.as_deref() {
        Some([_, ..]) => {
            let diff = diff.as_ref().unwrap();
            let source = replacement_printer::print_unsafe(diff, &file_input);
            let display_path = flow_common::reason::string_of_source(
                strip_root.as_ref().map(|p| p.to_str().unwrap_or("")),
                file,
            );
            println!(">>> {} (#changes: {})", display_path, diff.len());
            println!("{}", source);
        }
        Some([]) | None => {}
    }
}

pub async fn print_ast_file_real(file: FileKey) -> Option<FileKey> {
    let file_path = file.to_absolute();
    let file_input = FileInput::FileName(file_path.clone());
    let diff = diff_heaps_get_diff(&file);
    match diff.as_deref() {
        Some([_, ..]) => {
            let diff = diff.as_ref().unwrap();
            let source = replacement_printer::print_unsafe(diff, &file_input);
            std::fs::write(&file_path, source).expect("failed to write codemod output");
            Some(file)
        }
        Some([]) | None => None,
    }
}

const MAX_FILES_OPEN: usize = 1024;

pub async fn print_asts(
    strip_root: &Option<PathBuf>,
    write: bool,
    files: Vec<FileKey>,
) -> Option<Vec<FileKey>> {
    fn print_dry(strip_root: &Option<PathBuf>, mut files: Vec<FileKey>) -> Option<Vec<FileKey>> {
        files.sort();
        for file in &files {
            print_ast_file_dry(strip_root, file);
        }
        None
    }
    async fn print_real(files: Vec<FileKey>) -> Option<Vec<FileKey>> {
        let buckets: Vec<Vec<FileKey>> = files
            .chunks(MAX_FILES_OPEN)
            .map(|chunk| chunk.to_vec())
            .collect();
        let mut changed_files = Vec::new();
        for bucket in buckets {
            let mut bucket_changed = Vec::new();
            for file in bucket {
                if let Some(f) = print_ast_file_real(file).await {
                    bucket_changed.push(f);
                }
            }
            bucket_changed.reverse();
            bucket_changed.extend(changed_files);
            changed_files = bucket_changed;
        }
        Some(changed_files)
    }
    if write {
        print_real(files).await
    } else {
        print_dry(strip_root, files)
    }
}
