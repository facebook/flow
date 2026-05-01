/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::path::Path;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::options::SavedStateFetcher;
use flow_common::verbose::Verbose;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_common_errors::error_utils::cli_output;
use flow_common_errors::error_utils::json_output;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_services_inference::type_service;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

enum Printer<'a> {
    Json {
        pretty: bool,
        version: Option<json_output::JsonVersion>,
    },
    Cli(&'a cli_output::ErrorFlags),
}

// helper - print errors. used in check-and-die runs
fn format_errors<'a>(
    printer: &'a Printer<'a>,
    client_include_warnings: bool,
    offset_kind: flow_parser::offset_utils::OffsetKind,
    options: &'a flow_common::options::Options,
    (errors, warnings, suppressed_errors): (
        &'a ConcreteLocPrintableErrorSet,
        &'a ConcreteLocPrintableErrorSet,
        &'a [(PrintableError<Loc>, BTreeSet<Loc>)],
    ),
) -> Box<dyn FnOnce(Option<serde_json::Value>) + 'a> {
    let include_warnings = client_include_warnings || options.include_warnings;
    let empty = ConcreteLocPrintableErrorSet::empty();
    let warnings = if include_warnings { warnings } else { &empty };
    let strip_root = if options.strip_root {
        Some(options.root.as_path())
    } else {
        None
    };

    // The print_errors functions in the Errors modules are carefully defined to
    // perform any expensive, non-printing work when partially applied, before
    // receiving the `profiling` argument.
    //
    // We use this trick in order to actually profile this work. So, the
    // annotation on the `print_errors` binding below serves to ensure that the
    // error functions are applied enough that this expensive work happens.
    let print_errors: Box<dyn FnOnce(Option<serde_json::Value>) + 'a> = match printer {
        Printer::Json { pretty, version } => {
            let strip_root = strip_root.map(|path| path.to_string_lossy().into_owned());
            let pretty = *pretty;
            let get_json = json_output::full_status_json_of_errors(
                strip_root.as_deref(),
                suppressed_errors,
                version.clone().unwrap_or(json_output::JsonVersion::JsonV1),
                &None,
                offset_kind,
                errors,
                warnings,
            );
            let finish_formatting = move |profiling_props| {
                let res = get_json(profiling_props);
                let stdout = std::io::stdout();
                let mut out = std::io::BufWriter::new(stdout.lock());
                use std::io::Write;
                if pretty {
                    write!(out, "{}", flow_hh_json::json_to_multiline(&res))
                        .expect("failed to write errors");
                } else {
                    write!(out, "{}", flow_hh_json::json_string_of_value(&res))
                        .expect("failed to write errors");
                }
                out.flush().expect("failed to flush errors");
            };
            Box::new(move |profiling| {
                let profiling_props = match profiling {
                    Some(serde_json::Value::Object(profiling_props)) => {
                        profiling_props.into_iter().collect()
                    }
                    Some(_) | None => vec![],
                };
                finish_formatting(profiling_props);
            })
        }
        Printer::Cli(flags) => {
            let errors = suppressed_errors
                .iter()
                .fold(errors.clone(), |mut acc, (error, _)| {
                    acc.add(error.clone());
                    acc
                });
            let stdout = std::io::stdout();
            let mut out = std::io::BufWriter::new(stdout.lock());
            cli_output::print_errors(&mut out, flags, &None, strip_root, &errors, warnings, None)
                .expect("failed to write errors");
            use std::io::Write;
            out.flush().expect("failed to flush errors");
            Box::new(move |_profiling| ())
        }
    };

    Box::new(move |profiling| {
        if options.profile {
            print_errors(profiling)
        } else {
            print_errors(None)
        }
    })
}

fn check_main(
    base_flags: command_utils::BaseFlags,
    error_flags: command_utils::ErrorFlagsArgs,
    options_flags: command_utils::OptionsFlags,
    json: bool,
    pretty: bool,
    json_version: Option<json_output::JsonVersion>,
    offset_style: Option<command_utils::OffsetStyle>,
    shm_flags: command_utils::SharedMemParams,
    ignore_version: bool,
    path_opt: Option<String>,
    (): (),
) {
    let flowconfig_name = base_flags.flowconfig_name;
    let root = command_utils::guess_root(&flowconfig_name, path_opt.as_deref());
    let flowconfig_path = root.join(&flowconfig_name);
    let flowconfig_path = flowconfig_path.to_string_lossy().to_string();
    let (flowconfig, flowconfig_hash) =
        command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
    let flowconfig_for_assert = flowconfig.clone();
    let mut options = {
        let lazy_mode = Some(flow_config::LazyMode::NonLazy);
        // Saved state doesn't make sense for `flow full-check`, so disable it.
        let saved_state_options_flags = command_utils::SavedStateFlags {
            // None would mean we would just use the value in the .flowconfig, if available.
            // Instead, let's override that and turn off saved state entirely.
            saved_state_fetcher: Some(SavedStateFetcher::DummyFetcher),
            saved_state_force_recheck: false,
            saved_state_no_fallback: false,
            saved_state_skip_version_check: false,
            saved_state_verify: false,
        };
        Arc::new(command_utils::make_options(
            flowconfig_name,
            flowconfig_hash,
            flowconfig,
            lazy_mode,
            root.clone(),
            options_flags.clone(),
            saved_state_options_flags,
        ))
    };
    // Auto-imports indexing is only useful for IDE/LSP features (autocomplete,
    // code actions). Foreground check commands never serve those requests, so
    // skip building the export index to save time and memory.
    Arc::make_mut(&mut options).autoimports = false;
    let _init_id = crate::random_id_short_string();
    let offset_kind = command_utils::offset_kind_of_offset_style(offset_style);
    // initialize loggers before doing too much, especially anything that might exit
    flow_logging_utils::init_loggers(&options, Some(flow_hh_logger::Level::Error));

    if !ignore_version {
        command_utils::assert_version(&flowconfig_for_assert);
    }
    let _shared_mem_config = command_utils::shm_config(&shm_flags, &flowconfig_for_assert);
    let error_flags = command_utils::collect_error_flags(
        error_flags.color,
        error_flags.include_warnings,
        error_flags.max_warnings,
        error_flags.one_line,
        error_flags.list_files,
        error_flags.show_all_errors,
        error_flags.show_all_branches,
        error_flags.unicode,
        error_flags.message_width,
    );
    let client_include_warnings = error_flags.include_warnings;
    let printer = if json || json_version.is_some() || pretty {
        Printer::Json {
            pretty,
            version: json_version,
        }
    } else {
        Printer::Cli(&error_flags)
    };
    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));
    let (errors, warnings, suppressed_errors) =
        type_service::check_once(options.dupe(), &pool, &shared_mem, root.as_path(), None);
    let format_errors = format_errors(
        &printer,
        client_include_warnings,
        offset_kind,
        &options,
        (&errors, &warnings, &suppressed_errors),
    );
    format_errors(None);
    flow_common_exit_status::exit(command_utils::get_check_or_status_exit_code(
        &errors,
        &warnings,
        error_flags.max_warnings,
    ))
}

mod full_check_command {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "full-check",
            "Type-checks all files in the foreground (no server, can be slow on large codebases)",
            command_spec::Visibility::Public,
            format!(
                "Usage: {exe_name} full-check [OPTION]... [ROOT]\n\nDoes a full Flow check in the foreground and prints the results.\n\nThis command runs the type checker directly in the foreground without connecting to a Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n"
            ),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_error_flags(spec);
        let spec = command_utils::add_options_and_json_flags(spec);
        let spec = command_utils::add_json_version_flag(spec);
        let spec = command_utils::add_offset_style_flag(spec);
        let spec = command_utils::add_shm_flags(spec);
        let spec = command_utils::add_ignore_version_flag(spec);
        let spec = command_utils::add_from_flag(spec);
        let spec = command_utils::add_no_cgroup_flag(spec);
        spec.anon("root", &arg_spec::optional(arg_spec::string()))
    }

    pub(super) fn command() -> command_spec::Command {
        let check_main = |args: &arg_spec::Values| {
            let json_flags = command_utils::get_json_flags(args);
            let options_flags = command_utils::get_options_flags(args);
            let options_flags = command_utils::OptionsFlags {
                quiet: options_flags.quiet || json_flags.json || json_flags.pretty,
                ..options_flags
            };
            super::check_main(
                command_utils::get_base_flags(args),
                command_utils::get_error_flags_args(args),
                options_flags,
                json_flags.json,
                json_flags.pretty,
                command_utils::get_json_version(args),
                command_utils::get_offset_style(args),
                command_utils::get_shm_flags(args),
                command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap(),
                command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap(),
                (),
            );
        };
        command_spec::command(spec(), check_main)
    }
}

mod focus_check_command {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "focus-check",
            "Type-checks specific files and their dependents in the foreground (no server)",
            command_spec::Visibility::Public,
            format!(
                "Usage: {exe_name} focus-check [OPTION]... [FILES/DIRS]\n\nType-checks the input files/directories and their dependents in the foreground (no server).\n\nIf --root is not specified, Flow will search upward for a .flowconfig file from the first file or dir in FILES/DIR.\nIf --root is not specified and FILES/DIR is omitted, a focus check is ran on the current directory.\n"
            ),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_error_flags(spec);
        let spec = command_utils::add_options_and_json_flags(spec);
        let spec = command_utils::add_json_version_flag(spec);
        let spec = command_utils::add_saved_state_flags(spec);
        let spec = command_utils::add_offset_style_flag(spec);
        let spec = command_utils::add_shm_flags(spec);
        let spec = command_utils::add_ignore_version_flag(spec);
        let spec = command_utils::add_verbose_focus_flag(spec);
        let spec = command_utils::add_from_flag(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_input_file_flag(spec, "check");
        let spec = command_utils::add_no_cgroup_flag(spec);
        spec.anon("root", &arg_spec::list_of(arg_spec::string()))
    }

    fn main(
        base_flags: command_utils::BaseFlags,
        error_flags: command_utils::ErrorFlagsArgs,
        options_flags: command_utils::OptionsFlags,
        json: bool,
        pretty: bool,
        json_version: Option<json_output::JsonVersion>,
        saved_state_options_flags: command_utils::SavedStateFlags,
        offset_style: Option<command_utils::OffsetStyle>,
        shm_flags: command_utils::SharedMemParams,
        ignore_version: bool,
        verbose_focus: bool,
        root: Option<String>,
        input_file: Option<String>,
        filenames: Vec<String>,
        (): (),
    ) {
        let filenames =
            command_utils::get_filenames_from_input(false, input_file.as_deref(), Some(&filenames));
        let flowconfig_name = base_flags.flowconfig_name;
        // If --root is explicitly set, then use that as the root. Otherwise, use the first file
        let root = command_utils::guess_root(
            &flowconfig_name,
            if root.is_some() {
                root.as_deref()
            } else {
                filenames.first().map(String::as_str)
            },
        );
        let flowconfig_path = root.join(&flowconfig_name);
        let flowconfig_path = flowconfig_path.to_string_lossy().to_string();
        let (flowconfig, flowconfig_hash) =
            command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
        let flowconfig_for_assert = flowconfig.clone();
        let mut options = Arc::new(command_utils::make_options(
            flowconfig_name,
            flowconfig_hash,
            flowconfig,
            Some(flow_config::LazyMode::NonLazy),
            root.clone(),
            options_flags.clone(),
            saved_state_options_flags,
        ));
        // Auto-imports indexing is only useful for IDE/LSP features.
        Arc::make_mut(&mut options).autoimports = false;
        if verbose_focus {
            let opt_verbose = options.verbose.clone();
            let opt_verbose = match opt_verbose {
                Some(opt_verbose) => Arc::new(Verbose {
                    indent: opt_verbose.indent,
                    depth: opt_verbose.depth,
                    enabled_during_flowlib: opt_verbose.enabled_during_flowlib,
                    focused_files: Some(filenames.clone()),
                }),
                None => Arc::new(Verbose {
                    indent: 0,
                    depth: 1,
                    enabled_during_flowlib: false,
                    focused_files: Some(filenames.clone()),
                }),
            };
            Arc::make_mut(&mut options).verbose = Some(opt_verbose);
        }
        let _init_id = crate::random_id_short_string();
        let offset_kind = command_utils::offset_kind_of_offset_style(offset_style);
        // initialize loggers before doing too much, especially anything that might exit
        flow_logging_utils::init_loggers(&options, None);

        // do this after loggers are initialized, so we can complain properly
        let expanded_filenames =
            command_utils::expand_file_list(&filenames, Some(&options.file_options));
        flow_hh_logger::info!("Checking {} files", expanded_filenames.len());

        if !ignore_version {
            command_utils::assert_version(&flowconfig_for_assert);
        }

        let _shared_mem_config = command_utils::shm_config(&shm_flags, &flowconfig_for_assert);
        let focus_targets: FlowOrdSet<FileKey> = expanded_filenames
            .iter()
            .map(|file| {
                let abs_path = flow_common::files::cached_canonicalize(Path::new(file.as_str()))
                    .unwrap_or_else(|_| std::path::PathBuf::from(file.as_str()));
                FileKey::source_file_of_absolute(&abs_path.to_string_lossy())
            })
            .collect();
        let error_flags = command_utils::collect_error_flags(
            error_flags.color,
            error_flags.include_warnings,
            error_flags.max_warnings,
            error_flags.one_line,
            error_flags.list_files,
            error_flags.show_all_errors,
            error_flags.show_all_branches,
            error_flags.unicode,
            error_flags.message_width,
        );
        let client_include_warnings = error_flags.include_warnings;
        let printer = if json || json_version.is_some() || pretty {
            Printer::Json {
                pretty,
                version: json_version,
            }
        } else {
            Printer::Cli(&error_flags)
        };
        let shared_mem = Arc::new(SharedMem::new());
        let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
            std::num::NonZeroUsize::new(options.max_workers as usize)
                .expect("max_workers should be positive"),
        ));
        let (errors, warnings, suppressed_errors) = type_service::check_once(
            options.dupe(),
            &pool,
            &shared_mem,
            root.as_path(),
            Some(focus_targets),
        );
        let format_errors = format_errors(
            &printer,
            client_include_warnings,
            offset_kind,
            &options,
            (&errors, &warnings, &suppressed_errors),
        );
        format_errors(None);
        flow_common_exit_status::exit(command_utils::get_check_or_status_exit_code(
            &errors,
            &warnings,
            error_flags.max_warnings,
        ))
    }

    pub(super) fn command() -> command_spec::Command {
        let main = |args: &arg_spec::Values| {
            let json_flags = command_utils::get_json_flags(args);
            let options_flags = command_utils::get_options_flags(args);
            let options_flags = command_utils::OptionsFlags {
                quiet: options_flags.quiet || json_flags.json || json_flags.pretty,
                ..options_flags
            };
            self::main(
                command_utils::get_base_flags(args),
                command_utils::get_error_flags_args(args),
                options_flags,
                json_flags.json,
                json_flags.pretty,
                command_utils::get_json_version(args),
                command_utils::get_saved_state_flags(args),
                command_utils::get_offset_style(args),
                command_utils::get_shm_flags(args),
                command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap(),
                command_spec::get(args, "--verbose-focus", &arg_spec::truthy()).unwrap(),
                command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap(),
                command_spec::get(
                    args,
                    "--input-file",
                    &arg_spec::optional(arg_spec::string()),
                )
                .unwrap(),
                command_spec::get(args, "root", &arg_spec::list_of(arg_spec::string()))
                    .unwrap()
                    .unwrap_or_default(),
                (),
            );
        };
        command_spec::command(spec(), main)
    }
}

pub(crate) fn full_check_command() -> command_spec::Command {
    full_check_command::command()
}

pub(crate) fn focus_check_command() -> command_spec::Command {
    focus_check_command::command()
}
