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
use flow_common::options::SavedStateFetcher;
use flow_common::verbose::Verbose;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::cli_output;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_services_inference::type_service;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::command_spec;
use crate::command_utils;

pub(crate) enum Printer<'a> {
    Cli(&'a cli_output::ErrorFlags),
}

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

fn verbose_flags(args: &command_spec::Values, verbose_focus: bool) -> Option<Arc<Verbose>> {
    let verbose = command_spec::get(args, "--verbose", &command_spec::truthy()).unwrap();
    if !verbose && !verbose_focus {
        return None;
    }
    Some(Arc::new(Verbose {
        indent: 0,
        depth: 1,
        enabled_during_flowlib: false,
        focused_files: None,
    }))
}

fn error_flags(options: &Options, show_all_errors: bool) -> cli_output::ErrorFlags {
    cli_output::ErrorFlags {
        rendering_mode: cli_output::RenderingMode::CliColorNever,
        include_warnings: options.include_warnings,
        max_warnings: None,
        one_line: false,
        list_files: false,
        show_all_errors,
        show_all_branches: false,
        unicode: false,
        message_width: 120,
    }
}

fn foreground_check_overrides(disable_saved_state: bool) -> command_utils::MakeOptionsOverrides {
    let mut overrides = command_utils::MakeOptionsOverrides {
        autoimports: Some(false),
        lazy_mode: Some(false),
        ..Default::default()
    };
    if disable_saved_state {
        overrides.saved_state_fetcher = Some(SavedStateFetcher::DummyFetcher);
        overrides.saved_state_force_recheck = Some(false);
        overrides.saved_state_no_fallback = Some(false);
        overrides.saved_state_skip_version_check = Some(false);
        overrides.saved_state_verify = Some(false);
    }
    overrides
}

fn run_check(force_full_check: bool, args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &command_spec::truthy()).unwrap();
    let show_all_errors =
        command_spec::get(args, "--show-all-errors", &command_spec::truthy()).unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let long_lived_workers = command_spec::get(
        args,
        "--long-lived-workers",
        &command_spec::required(Some(false), command_spec::bool_flag()),
    )
    .unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let mut options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        foreground_check_overrides(force_full_check),
    );
    if strip_root {
        Arc::make_mut(&mut options).strip_root = true;
    }
    Arc::make_mut(&mut options).long_lived_workers = long_lived_workers;
    if let Some(verbose) = verbose_flags(args, false) {
        Arc::make_mut(&mut options).verbose = Some(verbose);
    }

    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));

    let (errors, warnings) =
        type_service::check_once(options.dupe(), &pool, &shared_mem, root.as_path(), None);
    let error_flags = error_flags(&options, show_all_errors);
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

fn run_focus_check(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &command_spec::truthy()).unwrap();
    let show_all_errors =
        command_spec::get(args, "--show-all-errors", &command_spec::truthy()).unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let long_lived_workers = command_spec::get(
        args,
        "--long-lived-workers",
        &command_spec::required(Some(false), command_spec::bool_flag()),
    )
    .unwrap();
    let verbose_focus =
        command_spec::get(args, "--verbose-focus", &command_spec::truthy()).unwrap();
    let root_flag = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let positional_args =
        command_spec::get(args, "root", &command_spec::list_of(command_spec::string())).unwrap();

    let filenames =
        command_utils::get_filenames_from_input(input_file.as_deref(), Some(&positional_args));
    let root = if let Some(root_flag) = &root_flag {
        Path::new(root_flag).canonicalize().unwrap_or_else(|_| {
            eprintln!("Invalid root: {}", root_flag);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InputError);
        })
    } else if let Some(first_file) = filenames.first() {
        command_utils::guess_root(&flowconfig_name, Some(first_file))
    } else {
        command_utils::guess_root(&flowconfig_name, None)
    };

    let mut options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        foreground_check_overrides(false),
    );
    if strip_root {
        Arc::make_mut(&mut options).strip_root = true;
    }
    Arc::make_mut(&mut options).long_lived_workers = long_lived_workers;
    if verbose_focus {
        Arc::make_mut(&mut options).verbose = Some(Arc::new(Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: Some(filenames.clone()),
        }));
    } else if let Some(verbose) = verbose_flags(args, false) {
        Arc::make_mut(&mut options).verbose = Some(verbose);
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
        root.as_path(),
        Some(focus_targets),
    );

    let error_flags = error_flags(&options, show_all_errors);
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

fn common_spec(name: &str, doc: &str, usage: &str) -> command_spec::Spec {
    command_spec::Spec::new(name, doc, usage.to_string())
        .flag(
            "--flowconfig-name",
            &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
            "Set the name of the flow configuration file. (default: .flowconfig)",
            Some("FLOW_CONFIG_NAME"),
        )
        .flag(
            "--show-all-errors",
            &command_spec::truthy(),
            "Print all errors (the default is to truncate after 50 errors)",
            None,
        )
        .flag(
            "--strip-root",
            &command_spec::truthy(),
            "Print paths without the root",
            None,
        )
        .flag(
            "--no-flowlib",
            &command_spec::truthy(),
            "Do not use the bundled flowlib",
            None,
        )
        .flag(
            "--verbose",
            &command_spec::truthy(),
            "Print verbose info during typecheck",
            None,
        )
        .flag(
            "--ignore-version",
            &command_spec::truthy(),
            "Ignore the version constraint in .flowconfig",
            Some("FLOW_IGNORE_VERSION"),
        )
        .flag(
            "--long-lived-workers",
            &command_spec::required(Some(false), command_spec::bool_flag()),
            "Enable or disable long-lived workers (default: false)",
            None,
        )
        .flag(
            "--from",
            &command_spec::optional(command_spec::string()),
            "Specify who is calling this CLI command (used by logging)",
            None,
        )
        .flag(
            "--no-cgroup",
            &command_spec::truthy(),
            "Don't automatically run this command in a cgroup (if cgroups are available)",
            None,
        )
}

fn check_spec() -> command_spec::Spec {
    common_spec(
        "check",
        "Does a full Flow check and prints the results",
        "Usage: flow check [OPTION]... [ROOT]\n\nDoes a full Flow check and prints the results.\n\nIf check_is_status=true is set in .flowconfig, this command connects to the\nFlow server (like `flow status`) instead of doing a standalone check.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n",
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

fn full_check_spec() -> command_spec::Spec {
    common_spec(
        "full-check",
        "Does a full Flow check in the foreground and prints the results",
        "Usage: flow full-check [OPTION]... [ROOT]\n\nDoes a full Flow check in the foreground and prints the results.\n\nThis command runs the type checker directly in the foreground without connecting to a Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n",
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

fn focus_check_spec() -> command_spec::Spec {
    common_spec(
        "focus-check",
        "EXPERIMENTAL: Does a focused Flow check on a file (and its dependents and their dependencies) and prints the results",
        "Usage: flow focus-check [OPTION]... [FILES/DIRS]\n\nEXPERIMENTAL: Does a focused Flow check on the input files/directories (and each of their dependents and dependencies) and prints the results.\n\nIf --root is not specified, Flow will search upward for a .flowconfig file from the first file or dir in FILES/DIR.\nIf --root is not specified and FILES/DIR is omitted, a focus check is ran on the current directory.\n",
    )
    .flag(
        "--verbose-focus",
        &command_spec::truthy(),
        "Print verbose output about target file only (implies --verbose)",
        None,
    )
    .flag(
        "--root",
        &command_spec::optional(command_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
    .flag(
        "--input-file",
        &command_spec::optional(command_spec::string()),
        "File containing list of files to check, one per line. If -, list of files is read from the standard input.",
        None,
    )
    .anon("root", &command_spec::list_of(command_spec::string()))
}

pub(crate) fn check_command() -> command_spec::Command {
    command_spec::command(check_spec(), |args| run_check(false, args))
}

pub(crate) fn full_check_command() -> command_spec::Command {
    command_spec::command(full_check_spec(), |args| run_check(true, args))
}

pub(crate) fn focus_check_command() -> command_spec::Command {
    command_spec::command(focus_check_spec(), run_focus_check)
}
