/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::cli_output;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_services_inference::type_service;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::command_utils;

pub(crate) enum Printer<'a> {
    Cli(&'a cli_output::ErrorFlags),
}

// helper - print errors. used in check-and-die runs
pub(crate) fn format_errors(
    printer: &Printer<'_>,
    client_include_warnings: bool,
    options: &Options,
    errors: &ConcreteLocPrintableErrorSet,
    warnings: &ConcreteLocPrintableErrorSet,
) {
    let include_warnings = client_include_warnings || options.include_warnings;
    let empty = ConcreteLocPrintableErrorSet::empty();
    let warnings = if include_warnings { warnings } else { &empty };
    let strip_root = if options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };
    match printer {
        Printer::Cli(flags) => {
            let stdout = std::io::stdout();
            let mut out = std::io::BufWriter::new(stdout.lock());
            cli_output::print_errors(&mut out, flags, &None, strip_root, errors, warnings, None)
                .expect("failed to write errors");
            {
                use std::io::Write;
                out.flush().unwrap();
                drop(out);
            }
        }
    }
}

pub(crate) fn full_check_main(args: &[String]) {
    // Parse CLI flags
    let mut strip_root = false;
    let mut show_all_errors = false;
    let mut no_flowlib = false;
    let mut verbose = false;
    let mut ignore_version = std::env::var("FLOW_IGNORE_VERSION").is_ok_and(|v| !v.is_empty());
    for arg in args {
        match arg.as_str() {
            "--strip-root" => strip_root = true,
            "--show-all-errors" => show_all_errors = true,
            "--no-flowlib" => no_flowlib = true,
            "--verbose" => verbose = true,
            "--ignore-version" => ignore_version = true,
            _ => {}
        }
    }

    let root = Path::new(".").canonicalize().unwrap();
    let mut options = crate::get_options(no_flowlib, ignore_version);
    if strip_root {
        Arc::make_mut(&mut options).strip_root = true;
    }
    if verbose {
        Arc::make_mut(&mut options).verbose = Some(Arc::new(flow_common::verbose::Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: None,
        }));
    }
    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));

    let (errors, warnings) =
        type_service::check_once(options.dupe(), &pool, &shared_mem, &root, None);

    let error_flags = cli_output::ErrorFlags {
        rendering_mode: cli_output::RenderingMode::CliColorNever,
        include_warnings: options.include_warnings,
        max_warnings: None,
        one_line: false,
        list_files: false,
        show_all_errors,
        show_all_branches: false,
        unicode: false,
        message_width: 120,
    };
    format_errors(
        &Printer::Cli(&error_flags),
        error_flags.include_warnings,
        &options,
        &errors,
        &warnings,
    );

    let exit_status =
        command_utils::get_check_or_status_exit_code(&errors, &warnings, error_flags.max_warnings);
    flow_common_exit_status::exit(exit_status)
}

pub(crate) fn focus_check_main(args: &[String]) {
    // Parse CLI flags (OCaml uses CommandSpec.ArgSpec for this)
    let mut strip_root = false;
    let mut show_all_errors = false;
    let mut no_flowlib = false;
    let mut verbose = false;
    let mut ignore_version = std::env::var("FLOW_IGNORE_VERSION").is_ok_and(|v| !v.is_empty());
    let mut verbose_focus = false;
    let mut root_flag: Option<String> = None;
    let mut input_file: Option<String> = None;
    let mut positional_args: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--strip-root" => strip_root = true,
            "--show-all-errors" => show_all_errors = true,
            "--no-flowlib" => no_flowlib = true,
            "--verbose" => verbose = true,
            "--ignore-version" => ignore_version = true,
            "--verbose-focus" => verbose_focus = true,
            "--root" => {
                i += 1;
                if i < args.len() {
                    root_flag = Some(args[i].clone());
                }
            }
            "--input-file" => {
                i += 1;
                if i < args.len() {
                    input_file = Some(args[i].clone());
                }
            }
            arg if arg.starts_with("--root=") => {
                root_flag = Some(arg.strip_prefix("--root=").unwrap().to_string());
            }
            arg if arg.starts_with("--input-file=") => {
                input_file = Some(arg.strip_prefix("--input-file=").unwrap().to_string());
            }
            arg if !arg.starts_with('-') => {
                positional_args.push(arg.to_string());
            }
            _ => {}
        }
        i += 1;
    }

    // If --root is explicitly set, then use that as the root. Otherwise, use the first file
    let filenames = command_utils::get_filenames_from_input(
        input_file.as_deref(),
        if positional_args.is_empty() {
            None
        } else {
            Some(&positional_args)
        },
    );

    let root = if let Some(ref r) = root_flag {
        Path::new(r).canonicalize().unwrap_or_else(|_| {
            eprintln!("Invalid root: {}", r);
            std::process::exit(1);
        })
    } else if let Some(first_file) = filenames.first() {
        // CommandUtils.guess_root: find .flowconfig starting from the first file
        let p = Path::new(first_file);
        let start_dir = if p.is_file() {
            p.parent().unwrap_or(p)
        } else {
            p
        };
        let mut dir = start_dir
            .canonicalize()
            .unwrap_or_else(|_| start_dir.to_path_buf());
        loop {
            if dir.join(".flowconfig").exists() {
                break dir;
            }
            if !dir.pop() {
                eprintln!(
                    "Could not find a .flowconfig in {} or any of its parent directories",
                    start_dir.display()
                );
                std::process::exit(8);
            }
        }
    } else {
        Path::new(".").canonicalize().unwrap()
    };

    let mut options = crate::get_options_with_root(no_flowlib, ignore_version, &root);
    if strip_root {
        Arc::make_mut(&mut options).strip_root = true;
    }

    if verbose_focus {
        let focused_files_list: Vec<String> = filenames.clone();
        Arc::make_mut(&mut options).verbose = Some(Arc::new(flow_common::verbose::Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: Some(focused_files_list),
        }));
    } else if verbose {
        Arc::make_mut(&mut options).verbose = Some(Arc::new(flow_common::verbose::Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: None,
        }));
    }

    let expanded_filenames =
        command_utils::expand_file_list(&filenames, Some(&options.file_options));
    eprintln!("Checking {} files", expanded_filenames.len());

    let focus_targets: FlowOrdSet<FileKey> = expanded_filenames
        .iter()
        .map(|file| {
            let abs_path = flow_common::files::cached_canonicalize(Path::new(file.as_str()))
                .unwrap_or_else(|_| std::path::PathBuf::from(file.as_str()));
            FileKey::new(FileKeyInner::SourceFile(
                abs_path.to_string_lossy().to_string(),
            ))
        })
        .collect();

    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));

    let (errors, warnings) = type_service::check_once(
        options.dupe(),
        &pool,
        &shared_mem,
        &root,
        Some(focus_targets),
    );

    let error_flags = cli_output::ErrorFlags {
        rendering_mode: cli_output::RenderingMode::CliColorNever,
        include_warnings: options.include_warnings,
        max_warnings: None,
        one_line: false,
        list_files: false,
        show_all_errors,
        show_all_branches: false,
        unicode: false,
        message_width: 120,
    };
    format_errors(
        &Printer::Cli(&error_flags),
        error_flags.include_warnings,
        &options,
        &errors,
        &warnings,
    );

    let exit_status =
        command_utils::get_check_or_status_exit_code(&errors, &warnings, error_flags.max_warnings);
    flow_common_exit_status::exit(exit_status)
}
