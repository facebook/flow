/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::io::BufRead;
use std::io::Read;
use std::path::Path;
use std::process::Command as ProcessCommand;
use std::process::Stdio;
use std::sync::Arc;

use flow_common::options;
use flow_common::options::CastingSyntax;
use flow_common::options::Format;
use flow_common::options::GcControl;
use flow_common::options::LogSaving;
use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::path_matcher::PathMatcher;
use flow_common::verbose::Verbose;
use flow_common_errors::error_utils::cli_output;
use flow_common_exit::FlowExitStatus;
use flow_common_vcs::vcs::Vcs;
use flow_config::FlowConfig;
use flow_config::LazyMode;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_flowlib::LibDir as FlowlibDir;
use flow_server_env::server_prot;
use flow_server_monitor::FlowServerMonitorOptions;
use flow_server_utils::file_input::FileInput;
use regex::Regex;
use tracing::level_filters::LevelFilter;

use crate::command_spec;
use crate::command_spec::Command;
use crate::command_spec::arg_spec;

#[derive(Clone, Debug)]
pub(crate) enum UnicodeMode {
    Never,
    Always,
    Auto,
}

pub(super) fn run_command(command: &Command, argv: &[String]) {
    match command_spec::parse_or_show_help(command, argv) {
        Ok(Err(_)) => {
            println!("{}", command.string_of_usage());
            flow_common_exit::exit(FlowExitStatus::NoError, None);
        }
        Ok(Ok(args)) => {
            apply_from_flag(command, &args);
            if command.flags().contains_key("--no-cgroup") {
                maybe_run_in_cgroup(&args);
            }
            command.run(&args)
        }
        Err(error) => {
            let is_pretty_or_json_arg =
                |s: &str| s.starts_with("--pretty") || s.starts_with("--json");
            if let Some(json_arg) = argv.iter().find(|s| is_pretty_or_json_arg(s)) {
                let pretty = json_arg.starts_with("--pretty");
                flow_common_exit::set_json_mode(pretty);
            }
            let msg = command_spec::error_message(&error);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
    }
}

pub(super) fn expand_file_list(
    filenames: &[String],
    options: Option<&flow_common::files::FileOptions>,
) -> BTreeSet<String> {
    let paths: Vec<String> = filenames
        .iter()
        .map(|f| {
            flow_common::files::cached_canonicalize(std::path::Path::new(f))
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_else(|_| f.to_string())
        })
        .collect();
    let mut next_files: Box<dyn FnMut() -> Vec<String>> = match paths.as_slice() {
        [] => Box::new(Vec::new),
        _ => {
            let filter: Box<dyn Fn(&str) -> bool> = match options {
                Some(opts) => {
                    let opts = opts.clone();
                    Box::new(move |path: &str| flow_common::files::is_valid_path(&opts, path))
                }
                None => Box::new(|path: &str| path.ends_with(".js")),
            };
            let root = paths[0].clone();
            let others = paths[1..].to_vec();
            Box::new(flow_utils_find::make_next_files(filter, others, root))
        }
    };
    flow_common::files::get_all(&mut *next_files)
}

pub(super) fn get_filenames_from_input(
    allow_imaginary: bool,
    input_file: Option<&str>,
    filenames: Option<&[String]>,
) -> Vec<String> {
    let cwd = std::env::current_dir()
        .expect("failed to get current directory")
        .to_string_lossy()
        .to_string();
    let handle_imaginary = |filename: &str| -> String {
        if allow_imaginary {
            flow_common::files::imaginary_realpath(filename)
        } else {
            let msg = format!("File not found: {:?}", filename);
            flow_common_exit::exit(FlowExitStatus::NoInput, Some(&msg));
        }
    };
    let input_file_filenames = match input_file {
        Some("-") => {
            let stdin = std::io::stdin();
            let lines: Vec<String> = stdin
                .lock()
                .lines()
                .map(|l| l.expect("failed to read stdin"))
                .collect();
            flow_common::files::canonicalize_filenames(&cwd, &handle_imaginary, &lines)
        }
        Some(input_file) => {
            let content = std::fs::read_to_string(input_file).expect("failed to read input file");
            let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
            let file_dir = Path::new(input_file)
                .parent()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|| cwd.clone());
            flow_common::files::canonicalize_filenames(&file_dir, &handle_imaginary, &lines)
        }
        None => vec![],
    };
    let cli_filenames = match filenames {
        Some(filenames) => {
            let names: Vec<String> = filenames.to_vec();
            flow_common::files::canonicalize_filenames(&cwd, &handle_imaginary, &names)
        }
        None => vec![],
    };
    let mut result = cli_filenames;
    result.extend(input_file_filenames);
    result
}

pub(super) fn print_version() {
    println!(
        "Flow, a static type checker for JavaScript, version {}",
        flow_common::flow_version::VERSION
    );
}

pub(crate) fn expand_path(path: &str) -> String {
    let path = Path::new(path);
    if path.exists() {
        path.canonicalize()
            .unwrap_or_else(|_| path.to_path_buf())
            .to_string_lossy()
            .to_string()
    } else {
        let file = std::env::current_dir()
            .expect("failed to get current directory")
            .join(path);
        if file.exists() {
            file.canonicalize()
                .unwrap_or(file)
                .to_string_lossy()
                .to_string()
        } else {
            let msg = format!("File not found: {}", file.to_string_lossy());
            flow_common_exit::exit(FlowExitStatus::InputError, Some(&msg));
        }
    }
}

pub(crate) fn collect_error_flags(
    rendering_mode: cli_output::RenderingMode,
    include_warnings: bool,
    max_warnings: Option<i32>,
    one_line: bool,
    list_files: bool,
    show_all_errors: bool,
    show_all_branches: bool,
    unicode: UnicodeMode,
    message_width: Option<i32>,
) -> cli_output::ErrorFlags {
    let include_warnings = match max_warnings {
        Some(_) => true,
        None => include_warnings,
    };
    let unicode = match unicode {
        UnicodeMode::Never => false,
        UnicodeMode::Always => true,
        UnicodeMode::Auto => flow_utils_tty::supports_emoji(),
    };
    let message_width = match message_width {
        Some(message_width) => message_width,
        None => flow_utils_tty::get_term_cols()
            .map(|cols| std::cmp::min(120, cols))
            .unwrap_or(120),
    };
    cli_output::ErrorFlags {
        rendering_mode,
        include_warnings,
        max_warnings,
        one_line,
        list_files,
        show_all_errors,
        show_all_branches,
        unicode,
        message_width,
    }
}

pub(crate) fn add_warning_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--include-warnings",
        &arg_spec::truthy(),
        "Include warnings in the error output (warnings are excluded by default)",
        None,
    )
    .flag(
        "--max-warnings",
        &arg_spec::optional(arg_spec::int()),
        "Warnings above this number will cause a nonzero exit code (implies --include-warnings)",
        None,
    )
}

pub(crate) fn add_profile_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--profile",
        &arg_spec::truthy(),
        "Output profiling information",
        Some("FLOW_PROFILE"),
    )
}

pub(crate) fn get_profile_flag(args: &arg_spec::Values) -> bool {
    command_spec::get(args, "--profile", &arg_spec::truthy()).unwrap()
}

#[derive(Clone, Debug)]
pub(crate) struct ErrorFlagsArgs {
    pub(crate) color: cli_output::RenderingMode,
    pub(crate) include_warnings: bool,
    pub(crate) max_warnings: Option<i32>,
    pub(crate) one_line: bool,
    pub(crate) list_files: bool,
    pub(crate) show_all_errors: bool,
    pub(crate) show_all_branches: bool,
    pub(crate) unicode: UnicodeMode,
    pub(crate) message_width: Option<i32>,
}

pub(crate) fn add_error_flags(spec: command_spec::Spec) -> command_spec::Spec {
    add_warning_flags(
        spec.flag(
            "--color",
            &arg_spec::required(
                Some(cli_output::RenderingMode::CliColorAuto),
                arg_spec::enum_flag(vec![
                    ("never", cli_output::RenderingMode::CliColorNever),
                    ("always", cli_output::RenderingMode::CliColorAlways),
                    ("auto", cli_output::RenderingMode::CliColorAuto),
                    (
                        "unstable_ide_mode_EXPOSED_FOR_TESTING",
                        cli_output::RenderingMode::IdeDetailedError,
                    ),
                ]),
            ),
            "Display terminal output in color. never, always, auto (default: auto)",
            None,
        ),
    )
    .flag(
        "--one-line",
        &arg_spec::truthy(),
        "Escapes newlines so that each error prints on one line",
        None,
    )
    .flag(
        "--list-files",
        &arg_spec::truthy(),
        "List files with errors",
        None,
    )
    .flag(
        "--show-all-errors",
        &arg_spec::truthy(),
        "Print all errors (the default is to truncate after 50 errors)",
        None,
    )
    .flag(
        "--show-all-branches",
        &arg_spec::truthy(),
        "Print all branch errors (the default is to print the most relevant branches)",
        None,
    )
    .flag(
        "--unicode",
        &arg_spec::required(
            Some(UnicodeMode::Auto),
            arg_spec::enum_flag(vec![
                ("never", UnicodeMode::Never),
                ("always", UnicodeMode::Always),
                ("auto", UnicodeMode::Auto),
            ]),
        ),
        "Display terminal output with unicode decoration. never, always, auto (default: auto)",
        None,
    )
    .flag(
        "--message-width",
        &arg_spec::optional(arg_spec::int()),
        "Sets the width of messages but not code snippets (defaults to the smaller of 120 or the terminal width)",
        None,
    )
}

pub(crate) fn get_error_flags_args(args: &arg_spec::Values) -> ErrorFlagsArgs {
    ErrorFlagsArgs {
        color: command_spec::get(
            args,
            "--color",
            &arg_spec::required(
                Some(cli_output::RenderingMode::CliColorAuto),
                arg_spec::enum_flag(vec![
                    ("never", cli_output::RenderingMode::CliColorNever),
                    ("always", cli_output::RenderingMode::CliColorAlways),
                    ("auto", cli_output::RenderingMode::CliColorAuto),
                    (
                        "unstable_ide_mode_EXPOSED_FOR_TESTING",
                        cli_output::RenderingMode::IdeDetailedError,
                    ),
                ]),
            ),
        )
        .unwrap(),
        include_warnings: command_spec::get(args, "--include-warnings", &arg_spec::truthy())
            .unwrap(),
        max_warnings: command_spec::get(
            args,
            "--max-warnings",
            &arg_spec::optional(arg_spec::int()),
        )
        .unwrap(),
        one_line: command_spec::get(args, "--one-line", &arg_spec::truthy()).unwrap(),
        list_files: command_spec::get(args, "--list-files", &arg_spec::truthy()).unwrap(),
        show_all_errors: command_spec::get(args, "--show-all-errors", &arg_spec::truthy()).unwrap(),
        show_all_branches: command_spec::get(args, "--show-all-branches", &arg_spec::truthy())
            .unwrap(),
        unicode: command_spec::get(
            args,
            "--unicode",
            &arg_spec::required(
                Some(UnicodeMode::Auto),
                arg_spec::enum_flag(vec![
                    ("never", UnicodeMode::Never),
                    ("always", UnicodeMode::Always),
                    ("auto", UnicodeMode::Auto),
                ]),
            ),
        )
        .unwrap(),
        message_width: command_spec::get(
            args,
            "--message-width",
            &arg_spec::optional(arg_spec::int()),
        )
        .unwrap(),
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct JsonFlags {
    pub(crate) json: bool,
    pub(crate) pretty: bool,
}

pub(crate) fn add_json_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--json",
        &arg_spec::truthy(),
        "Output results in JSON format",
        None,
    )
    .flag(
        "--pretty",
        &arg_spec::truthy(),
        "Pretty-print JSON output",
        None,
    )
}

pub(crate) fn get_json_flags(args: &arg_spec::Values) -> JsonFlags {
    let json = command_spec::get(args, "--json", &arg_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &arg_spec::truthy()).unwrap();
    if json || pretty {
        flow_common_exit::set_json_mode(pretty);
    }
    JsonFlags { json, pretty }
}

pub(crate) fn add_temp_dir_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--temp-dir",
        &arg_spec::optional(arg_spec::string()),
        "Directory in which to store temp files (default: FLOW_TEMP_DIR, or /tmp/flow/)",
        Some("FLOW_TEMP_DIR"),
    )
}

pub(crate) fn add_lazy_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--lazy",
        &arg_spec::truthy(),
        "Only check changed files. Shorthand for `--lazy-mode true`",
        None,
    )
    .flag(
        "--lazy-mode",
        &arg_spec::optional(lazy_mode_flag()),
        "If true, only check changed files",
        Some("FLOW_LAZY_MODE"),
    )
}

pub(crate) fn lazy_mode_flag() -> arg_spec::FlagType<Option<LazyMode>> {
    arg_spec::enum_flag(vec![
        ("true", LazyMode::Lazy),
        ("false", LazyMode::NonLazy),
        // legacy, deprecated options
        ("fs", LazyMode::Lazy),
        ("watchman", LazyMode::WatchmanDeprecated),
        ("none", LazyMode::NonLazy),
    ])
}

pub(crate) fn get_lazy_flags(args: &arg_spec::Values) -> Option<LazyMode> {
    let lazy_ = command_spec::get(args, "--lazy", &arg_spec::truthy()).unwrap();
    let lazy_mode =
        command_spec::get(args, "--lazy-mode", &arg_spec::optional(lazy_mode_flag())).unwrap();
    if lazy_ && lazy_mode.is_none() {
        // --lazy === --lazy-mode true
        Some(LazyMode::Lazy)
    } else {
        lazy_mode
    }
}

pub(crate) fn lazy_mode_arg(lazy_mode: LazyMode) -> &'static str {
    match lazy_mode {
        LazyMode::Lazy => "fs",
        LazyMode::NonLazy => "none",
        LazyMode::WatchmanDeprecated => "watchman",
    }
}

pub(crate) fn add_input_file_flag(spec: command_spec::Spec, verb: &str) -> command_spec::Spec {
    spec.flag(
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
        &format!(
            "File containing list of files to {}, one per line. If -, list of files is read from the standard input.",
            verb
        ),
        None,
    )
}

pub(crate) fn add_verbose_focus_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--verbose-focus",
        &arg_spec::truthy(),
        "Print verbose output about target file only (implies --verbose)",
        None,
    )
}

#[derive(Clone, Debug, Default)]
pub(crate) struct SharedMemParams {
    pub(crate) shm_heap_size: Option<u64>,
    pub(crate) shm_hash_table_pow: Option<u32>,
}

pub(crate) fn get_shm_flags(args: &arg_spec::Values) -> SharedMemParams {
    let shm_heap_size = command_spec::get(
        args,
        "--sharedmemory-heap-size",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap()
    .map(|value| match value.parse::<u64>() {
        Ok(value) => value,
        Err(_) => {
            let msg = format!("Invalid --sharedmemory-heap-size value: {}", value);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
    });
    let shm_hash_table_pow = command_spec::get(
        args,
        "--sharedmemory-hash-table-pow",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap()
    .map(|value| match value.parse::<u32>() {
        Ok(value) => value,
        Err(_) => {
            let msg = format!("Invalid --sharedmemory-hash-table-pow value: {}", value);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
    });
    SharedMemParams {
        shm_heap_size,
        shm_hash_table_pow,
    }
}

pub(crate) fn add_shm_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--sharedmemory-heap-size",
        &arg_spec::optional(arg_spec::string()),
        "The maximum size of the shared memory heap. The default is 26843545600 (25 * 2^30 bytes = 25 GiB)",
        Some("FLOW_SHAREDMEM_HEAP_SIZE"),
    )
    .flag(
        "--sharedmemory-hash-table-pow",
        &arg_spec::optional(arg_spec::string()),
        "The exponent for the size of the shared memory hash table. The default is 19, implying a size of 2^19 bytes",
        None,
    )
}

#[derive(Clone, Debug)]
pub(crate) struct ShmConfig {
    pub(crate) heap_size: u64,
    pub(crate) hash_table_pow: u32,
}

pub(crate) fn shm_config(shm_flags: &SharedMemParams, flowconfig: &FlowConfig) -> ShmConfig {
    let heap_size = shm_flags
        .shm_heap_size
        .unwrap_or(flowconfig.options.shm_heap_size);
    let hash_table_pow = shm_flags
        .shm_hash_table_pow
        .unwrap_or(flowconfig.options.shm_hash_table_pow);
    ShmConfig {
        heap_size,
        hash_table_pow,
    }
}

pub(crate) fn add_from_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--from",
        &arg_spec::optional(arg_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
}

fn apply_from_flag(command: &Command, args: &arg_spec::Values) {
    if !command.flags().contains_key("--from") {
        return;
    }
    let from = args
        .get("--from")
        .and_then(|values| values.first().cloned());
    let from = match from {
        Some(from) => Some(from),
        None => default_from_flag(),
    };
    flow_event_logger::set_from(from);
    flow_event_logger::set_agent_id(std::env::var("META_3PAI_INVOCATION_ID").ok());
}

fn default_from_flag() -> Option<String> {
    #[cfg(unix)]
    {
        let ppid = std::fs::read_to_string("/proc/self/status")
            .ok()
            .and_then(|status| {
                status.lines().find_map(|line| {
                    let mut words = line.split_whitespace();
                    match (words.next(), words.next()) {
                        (Some("PPid:"), Some(ppid)) => ppid.parse::<u32>().ok(),
                        _ => None,
                    }
                })
            })?;
        if ppid == 0 {
            return None;
        }
        let path = format!("/proc/{}/cmdline", ppid);
        let mut bytes = std::fs::read(path).ok()?;
        while bytes.last() == Some(&0) {
            bytes.pop();
        }
        let cmdline = String::from_utf8(bytes).ok()?;
        let cmdline = cmdline.replace('\0', " ");
        let cmdline = cmdline.trim();
        if cmdline.is_empty() {
            None
        } else {
            Some(format!("parent cmdline: {}", cmdline))
        }
    }
    #[cfg(not(unix))]
    {
        None
    }
}

pub(crate) fn add_strip_root_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--strip-root",
        &arg_spec::truthy(),
        "Print paths without the root",
        None,
    )
}

pub(crate) fn add_wait_for_recheck_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
        "If the server is rechecking, wait for it to complete rather than run sooner using outdated data",
        None,
    )
}

pub(crate) fn add_vpn_less_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--vpn-less",
        &arg_spec::optional(arg_spec::bool_flag()),
        "True if enable the VPN-Less mode to query and download saved state",
        None,
    )
}

pub(crate) fn add_path_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--path",
        &arg_spec::optional(arg_spec::string()),
        "Specify (fake) path to file when reading data from stdin",
        None,
    )
}

pub(crate) fn add_autostop_flag(spec: command_spec::Spec) -> command_spec::Spec {
    // empty to omit it from --help
    spec.flag("--autostop", &arg_spec::truthy(), "", None)
}

pub(crate) fn add_verbose_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--verbose",
        &arg_spec::truthy(),
        "Print verbose info during typecheck",
        None,
    )
    .flag(
        "--verbose-indent",
        &arg_spec::truthy(),
        "Indent verbose info during typecheck (implies --verbose)",
        None,
    )
    .flag(
        "--verbose-depth",
        &arg_spec::optional(arg_spec::string()),
        "Recursively print types up to specified depth (default 1, implies --verbose)",
        None,
    )
    .flag(
        "--verbose-flowlib",
        &arg_spec::truthy(),
        "Print verbose info while initializing the flowlib",
        None,
    )
}

pub(crate) fn verbose_flags(args: &arg_spec::Values) -> Option<Verbose> {
    let verbose = command_spec::get(args, "--verbose", &arg_spec::truthy()).unwrap();
    let indent = command_spec::get(args, "--verbose-indent", &arg_spec::truthy()).unwrap();
    let depth = command_spec::get(
        args,
        "--verbose-depth",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let enabled_during_flowlib =
        command_spec::get(args, "--verbose-flowlib", &arg_spec::truthy()).unwrap();
    if !verbose && !indent && depth.is_none() {
        None
    } else {
        let depth = match depth {
            Some(depth) => depth.parse::<i32>().unwrap_or(1),
            None => 1,
        };
        Some(Verbose {
            indent: if indent { 2 } else { 0 },
            depth: if depth >= 0 { depth as u32 } else { 1 },
            enabled_during_flowlib,
            focused_files: None,
        })
    }
}

pub(crate) fn add_slow_to_check_logging_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--log-slow-files-interval",
        &arg_spec::optional(arg_spec::int()),
        "Specify the interval in seconds to log slow to check files. (default: 5 seconds)",
        None,
    )
    .flag(
        "--log-slow-components-threshold",
        &arg_spec::optional(arg_spec::int()),
        "Specify the threshold in seconds to log slow to check component. (default to not logging anything)",
        None,
    )
    .flag(
        "--log-slow-expressions-threshold",
        &arg_spec::optional(arg_spec::int()),
        "Specify the threshold in seconds to log slow to check expressions. (default to not logging anything)",
        None,
    )
}

pub(crate) fn add_quiet_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--quiet",
        &arg_spec::truthy(),
        "Suppress output about server startup",
        None,
    )
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) enum OnMismatchBehavior {
    #[default]
    ChooseNewest,
    StopServer,
    RestartClient,
    ErrorClient,
}

fn add_on_mismatch_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--on-mismatch",
        &arg_spec::required(
            Some(OnMismatchBehavior::ChooseNewest),
            arg_spec::enum_flag(vec![
                ("choose-newest", OnMismatchBehavior::ChooseNewest),
                ("stop-server", OnMismatchBehavior::StopServer),
                ("restart-client", OnMismatchBehavior::RestartClient),
                ("error-client", OnMismatchBehavior::ErrorClient),
            ]),
        ),
        "What to do when the client and server are different versions (choose-newest, stop-server, restart-client, error-client) (default: choose-newest)",
        None,
    )
}

pub(crate) fn add_root_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--root",
        &arg_spec::optional(arg_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
}

pub(crate) fn add_ignore_version_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--ignore-version",
        &arg_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
}

pub(crate) fn add_log_file_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--log-file",
        &arg_spec::optional(arg_spec::string()),
        "Path to log file (default: /tmp/flow/<escaped root path>.log)",
        Some("FLOW_LOG_FILE"),
    )
    .flag(
        "--monitor-log-file",
        &arg_spec::optional(arg_spec::string()),
        "Path to log file (default: /tmp/flow/<escaped root path>.monitor_log)",
        Some("FLOW_MONITOR_LOG_FILE"),
    )
}

pub(crate) fn get_log_file_flags(args: &arg_spec::Values) -> (Option<String>, Option<String>) {
    fn normalize(log_file: String) -> String {
        let path = std::path::Path::new(&log_file);
        let dirname = path.parent().unwrap_or(std::path::Path::new("."));
        let basename = path.file_name().unwrap_or_default();
        dirname.join(basename).to_string_lossy().to_string()
    }
    let server_log_file =
        command_spec::get(args, "--log-file", &arg_spec::optional(arg_spec::string()))
            .unwrap()
            .map(normalize);
    let monitor_log_file = command_spec::get(
        args,
        "--monitor-log-file",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap()
    .map(normalize);
    (server_log_file, monitor_log_file)
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) enum OffsetStyle {
    #[default]
    Utf8Bytes,
    JsIndices,
}

pub(crate) fn add_offset_style_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--offset-style",
        &arg_spec::optional(arg_spec::enum_flag(vec![
            ("utf8-bytes", OffsetStyle::Utf8Bytes),
            ("js-indices", OffsetStyle::JsIndices),
        ])),
        "How to compute offsets in JSON output (utf8-bytes, js-indices) (default: utf8-bytes)",
        None,
    )
}

pub(crate) fn get_offset_style(args: &arg_spec::Values) -> Option<OffsetStyle> {
    command_spec::get(
        args,
        "--offset-style",
        &arg_spec::optional(arg_spec::enum_flag(vec![
            ("utf8-bytes", OffsetStyle::Utf8Bytes),
            ("js-indices", OffsetStyle::JsIndices),
        ])),
    )
    .unwrap()
}

pub(crate) fn offset_kind_of_offset_style(
    offset_style: Option<OffsetStyle>,
) -> flow_parser::offset_utils::OffsetKind {
    match offset_style {
        None | Some(OffsetStyle::Utf8Bytes) => flow_parser::offset_utils::OffsetKind::Utf8,
        Some(OffsetStyle::JsIndices) => flow_parser::offset_utils::OffsetKind::JavaScript,
    }
}

pub(super) fn flowconfig_multi_error(errors: &[(u32, String)]) -> ! {
    let msg = errors
        .iter()
        .map(|(line, msg)| format!(".flowconfig:{} {}", line, msg))
        .collect::<Vec<_>>()
        .join("\n");
    flow_common_exit::exit(FlowExitStatus::InvalidFlowconfig, Some(&msg));
}

pub(super) fn flowconfig_multi_warn(errors: &[(u32, String)]) {
    for (line, msg) in errors {
        eprintln!(".flowconfig:{} {}", line, msg);
    }
}

pub(super) fn read_config_or_exit(flowconfig_path: &str, enforce_warnings: bool) -> FlowConfig {
    let (flowconfig, warnings, _flowconfig_hash) =
        match flow_config::get_with_ignored_version(flowconfig_path, false) {
            Ok(r) => r,
            Err(flow_config::Error(line, message)) => {
                flowconfig_multi_error(&[(line, message)]);
            }
        };
    if warnings.is_empty() {
        flowconfig
    } else {
        let warnings = warnings
            .into_iter()
            .map(|flow_config::Warning(line, message)| (line, message))
            .collect::<Vec<_>>();
        if enforce_warnings {
            flowconfig_multi_error(&warnings);
        } else {
            flowconfig_multi_warn(&warnings);
        }
        flowconfig
    }
}

pub(super) fn read_config_and_hash_or_exit(
    flowconfig_path: &str,
    enforce_warnings: bool,
) -> (FlowConfig, String) {
    let (flowconfig, warnings, flowconfig_hash) =
        match flow_config::get_with_ignored_version(flowconfig_path, false) {
            Ok(r) => r,
            Err(flow_config::Error(line, message)) => {
                flowconfig_multi_error(&[(line, message)]);
            }
        };
    if warnings.is_empty() {
        return (flowconfig, flowconfig_hash);
    }
    let warnings = warnings
        .into_iter()
        .map(|flow_config::Warning(line, message)| (line, message))
        .collect::<Vec<_>>();
    if enforce_warnings {
        flowconfig_multi_error(&warnings);
    } else {
        flowconfig_multi_warn(&warnings);
    }
    (flowconfig, flowconfig_hash)
}

pub(super) fn check_version(required_version: &Option<String>) -> Result<(), String> {
    match required_version {
        None => Ok(()),
        Some(version_constraint) => {
            // For the purposes of checking whether the *currently-running* version of Flow is compatible
            // with the given project, we'll include pre-releases. For example, this means that 0.61.0-beta
            // is compatible with >0.60.0, because it presumably implements the minimum necessary features
            // of 0.60.0.
            //
            // This is subtly different than determining which version of Flow to run in the first place,
            // like when npm/yarn is solving the `flow-bin` constraint. In that case, we do not want
            // >0.60.0 to opt into pre-releases automatically. Those sorts of checks should not pass
            // `~includes_prereleases`.
            //
            // So, if you've explicitly run v0.61.0-beta, and the flowconfig says `>0.60.0`, we'll allow it;
            // but if we were looking at the flowconfig to decide which version to run, you should not
            // choose the beta.
            match flow_common_semver::semver::satisfies(
                Some(true),
                version_constraint,
                flow_common::flow_version::VERSION,
            ) {
                Ok(true) => Ok(()),
                _ => {
                    let msg = format!(
                        "Wrong version of Flow. The config specifies version {} but this is version {}",
                        version_constraint,
                        flow_common::flow_version::VERSION
                    );
                    Err(msg)
                }
            }
        }
    }
}

pub(super) fn assert_version(flowconfig: &FlowConfig) {
    let required_version = &flowconfig.version;
    match check_version(required_version) {
        Ok(()) => {}
        Err(msg) => flow_common_exit::exit(FlowExitStatus::InvalidFlowconfig, Some(&msg)),
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct FlowconfigFlags {
    pub(crate) ignores: Vec<String>,
    pub(crate) includes: Vec<String>,
    pub(crate) libs: Vec<String>,
    // We store raw_lint_severities as a string list instead of as a LintSettings.t so we
    // can defer parsing of the lint settings until after the flowconfig lint settings
    // are known, to properly detect redundant settings (and avoid false positives)
    pub(crate) raw_lint_severities: Vec<String>,
    pub(crate) untyped: Vec<String>,
    pub(crate) declarations: Vec<String>,
}

pub(crate) fn list_of_string_arg(arg: Option<String>) -> Vec<String> {
    match arg {
        Some(arg) if arg.is_empty() => vec![],
        Some(arg) => arg.split(',').map(ToOwned::to_owned).collect(),
        None => vec![],
    }
}

pub(crate) fn remove_exclusion(pattern: &str) -> &str {
    pattern.strip_prefix('!').unwrap_or(pattern)
}

pub(crate) fn file_options(
    flowconfig: &FlowConfig,
    root: &Path,
    no_flowlib: bool,
    temp_dir: &Path,
    ignores: Vec<(String, Option<String>)>,
    includes: Vec<String>,
    libs: Vec<String>,
    untyped: Vec<String>,
    declarations: Vec<String>,
) -> Arc<flow_common::files::FileOptions> {
    use flow_common::files::FileOptions;

    let flowlib_dir = flow_flowlib::libdir(no_flowlib, temp_dir);
    let default_lib_dir = Some(match flowlib_dir {
        flow_flowlib::LibDir::Prelude(path) => flow_common::files::LibDir::Prelude(path),
        flow_flowlib::LibDir::Flowlib(path) => flow_common::files::LibDir::Flowlib(path),
    });

    let implicitly_include_root = flowconfig.options.files_implicitly_include_root;

    let all_ignores: Vec<(String, Option<String>)> = {
        let mut merged = ignores;
        merged.extend(flowconfig.ignores.iter().cloned());
        merged
    };
    let ignores = all_ignores
        .into_iter()
        .map(|(path, backup)| {
            let pattern = remove_exclusion(&path);
            let expanded = flow_common::files::expand_project_root_token(root, pattern);
            let regex = Regex::new(&expanded).unwrap();
            ((path, backup), regex)
        })
        .collect();

    let all_untyped: Vec<String> = {
        let mut merged = untyped;
        merged.extend(flowconfig.untyped.iter().cloned());
        merged
    };
    let untyped = all_untyped
        .into_iter()
        .map(|pattern| {
            let expanded =
                flow_common::files::expand_project_root_token(root, remove_exclusion(&pattern));
            let regex = Regex::new(&expanded).unwrap();
            (pattern, regex)
        })
        .collect();

    let all_declarations: Vec<String> = {
        let mut merged = declarations;
        merged.extend(flowconfig.declarations.iter().cloned());
        merged
    };
    let declarations = all_declarations
        .into_iter()
        .map(|pattern| {
            let expanded =
                flow_common::files::expand_project_root_token(root, remove_exclusion(&pattern));
            let regex = Regex::new(&expanded).unwrap();
            (pattern, regex)
        })
        .collect();

    let flowtyped_path = flow_common::files::make_path_absolute(root, "flow-typed");
    let mut has_explicit_flowtyped_lib = false;
    let mut lib_paths: Vec<(Option<String>, std::path::PathBuf)> = flowconfig
        .libs
        .iter()
        .map(|(scope, path)| {
            let scope_str = scope.as_ref().map(|s| s.to_string());
            let expanded = flow_common::files::expand_project_root_token(root, path);
            let path_buf = flow_common::files::make_path_absolute(root, &expanded);
            if path_buf == flowtyped_path {
                has_explicit_flowtyped_lib = true;
            }
            (scope_str, path_buf)
        })
        .collect();
    // "flow-typed" is always included in the libs list for convenience,
    // but there's no guarantee that it exists on the filesystem.
    if !has_explicit_flowtyped_lib && flowtyped_path.exists() {
        lib_paths.push((None, flowtyped_path));
    }
    for lib in libs {
        lib_paths.push((None, flow_common::files::make_path_absolute(root, &lib)));
    }

    let includes = {
        let mut merged_includes = includes;
        merged_includes.extend(flowconfig.includes.iter().cloned());
        let mut matcher = PathMatcher::empty();
        for include in merged_includes {
            matcher.add_path(&flow_common::files::make_path_absolute(root, &include));
        }
        // Implicitly included paths are added only if they're not already being watched
        let mut implicit_paths = Vec::new();
        if implicitly_include_root {
            implicit_paths.push(root.to_path_buf());
        }
        implicit_paths.extend(lib_paths.iter().map(|(_, path)| path.clone()));
        // Shortest path first
        implicit_paths.sort_by_key(|path| path.to_string_lossy().len());
        for path in implicit_paths {
            let path_str = path.to_string_lossy().to_string();
            // If this include is already covered by an explicit include or a shorter implicit include,
            // then skip it
            if matcher.matches(&path_str) {
                continue;
            }
            matcher.add_path(&path);
        }
        matcher
    };

    let haste_paths_excludes = flowconfig
        .options
        .haste_paths_excludes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect();

    let haste_paths_includes = flowconfig
        .options
        .haste_paths_includes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect();

    let module_declaration_dirnames = flowconfig
        .options
        .module_declaration_dirnames
        .iter()
        .map(|dir| {
            let expanded = flow_common::files::expand_project_root_token(root, dir);
            flow_common::files::cached_canonicalize(std::path::Path::new(&expanded))
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or(expanded)
        })
        .collect();

    let mut module_file_exts = flowconfig.options.module_file_exts.clone();
    if flowconfig.options.typescript_library_definition_support {
        for ext in [".d.cts", ".d.mts", ".d.ts"] {
            if !module_file_exts.contains(&ext.into()) {
                module_file_exts.insert(0, ext.into());
            }
        }
    }

    Arc::new(FileOptions {
        default_lib_dir,
        ignores,
        untyped,
        declarations,
        implicitly_include_root,
        haste_paths_excludes,
        haste_paths_includes,
        includes,
        lib_paths,
        module_declaration_dirnames,
        module_file_exts,
        module_resource_exts: flowconfig
            .options
            .module_resource_exts
            .iter()
            .cloned()
            .collect(),
        multi_platform: flowconfig.options.multi_platform.unwrap_or(false),
        multi_platform_extensions: flowconfig.options.multi_platform_extensions.to_vec(),
        multi_platform_extension_group_mapping: flowconfig
            .options
            .multi_platform_extension_group_mapping
            .to_vec(),
        node_resolver_dirnames: flowconfig.options.node_resolver_dirnames.clone(),
    })
}

pub(crate) fn file_options_of_flowconfig(
    root: &Path,
    flowconfig: &FlowConfig,
) -> Arc<flow_common::files::FileOptions> {
    file_options(
        flowconfig,
        root,
        true,
        flow_server_files::server_files_js::default_temp_dir().as_path(),
        vec![],
        vec![],
        vec![],
        vec![],
        vec![],
    )
}

pub(crate) fn add_ignore_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--ignore",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more ignore patterns, comma separated",
        None,
    )
}

pub(crate) fn add_untyped_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--untyped",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more patterns, comma separated, for files to treat as untyped",
        None,
    )
}

pub(crate) fn add_declaration_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--declaration",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more patterns, comma separated, for files to treat as declarations",
        None,
    )
}

pub(crate) fn add_include_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--include",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more include patterns, comma separated",
        None,
    )
}

pub(crate) fn add_lib_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--lib",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more lib files/directories, comma separated",
        None,
    )
}

pub(crate) fn add_lints_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--lints",
        &arg_spec::optional(arg_spec::string()),
        "Specify one or more lint rules, comma separated",
        None,
    )
}

pub(crate) fn add_no_restart_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--no-auto-restart",
        &arg_spec::truthy(),
        "If the server dies, do not try and restart it; just exit",
        None,
    )
}

pub(crate) fn add_flowconfig_flags(spec: command_spec::Spec) -> command_spec::Spec {
    add_lints_flag(add_lib_flag(add_include_flag(add_declaration_flag(
        add_untyped_flag(add_ignore_flag(spec)),
    ))))
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ConnectParams {
    pub(crate) retries: i32,
    pub(crate) timeout: Option<i32>,
    pub(crate) no_auto_start: bool,
    pub(crate) autostop: bool,
    pub(crate) lazy_mode: Option<LazyMode>,
    pub(crate) temp_dir: Option<String>,
    pub(crate) shm_flags: SharedMemParams,
    pub(crate) ignore_version: bool,
    pub(crate) quiet: bool,
    pub(crate) on_mismatch: OnMismatchBehavior,
}

pub(crate) fn get_connect_flags(args: &arg_spec::Values) -> ConnectParams {
    let lazy_ = command_spec::get(args, "--lazy", &arg_spec::truthy()).unwrap();
    let lazy_mode =
        command_spec::get(args, "--lazy-mode", &arg_spec::optional(lazy_mode_flag())).unwrap();
    let lazy_mode = if lazy_ && lazy_mode.is_none() {
        Some(LazyMode::Lazy)
    } else {
        lazy_mode
    };
    let timeout = command_spec::get(args, "--timeout", &arg_spec::int()).unwrap();
    match timeout {
        Some(n) if n <= 0 => {
            let msg = format!("Timeout must be a positive integer. Got {}", n);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
        _ => {}
    }
    ConnectParams {
        retries: command_spec::get(args, "--retries", &arg_spec::int())
            .unwrap()
            .unwrap_or(3),
        timeout,
        no_auto_start: command_spec::get(args, "--no-auto-start", &arg_spec::truthy()).unwrap(),
        autostop: false,
        lazy_mode,
        temp_dir: command_spec::get(args, "--temp-dir", &arg_spec::optional(arg_spec::string()))
            .unwrap(),
        shm_flags: get_shm_flags(args),
        ignore_version: command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap(),
        quiet: command_spec::get(args, "--quiet", &arg_spec::truthy()).unwrap(),
        on_mismatch: command_spec::get(
            args,
            "--on-mismatch",
            &arg_spec::required(
                Some(OnMismatchBehavior::ChooseNewest),
                arg_spec::enum_flag(vec![
                    ("choose-newest", OnMismatchBehavior::ChooseNewest),
                    ("stop-server", OnMismatchBehavior::StopServer),
                    ("restart-client", OnMismatchBehavior::RestartClient),
                    ("error-client", OnMismatchBehavior::ErrorClient),
                ]),
            ),
        )
        .unwrap(),
    }
}

fn add_connect_flags_with_lazy_collector(spec: command_spec::Spec) -> command_spec::Spec {
    let spec = spec
        .flag(
            "--timeout",
            &arg_spec::int(),
            "Maximum time to wait, in seconds",
            None,
        )
        .flag(
            "--retries",
            &arg_spec::int(),
            "Set the number of retries. (default: 3)",
            None,
        )
        .flag(
            "--no-auto-start",
            &arg_spec::truthy(),
            "If the server is not running, do not start it; just exit",
            None,
        );
    let spec = add_temp_dir_flag(spec);
    let spec = add_shm_flags(spec);
    let spec = add_from_flag(spec);
    let spec = add_ignore_version_flag(spec);
    let spec = add_quiet_flag(spec);
    add_on_mismatch_flag(spec)
}

pub(crate) fn add_connect_flags_no_lazy(spec: command_spec::Spec) -> command_spec::Spec {
    add_connect_flags_with_lazy_collector(spec)
}

pub(crate) fn add_connect_flags(spec: command_spec::Spec) -> command_spec::Spec {
    add_connect_flags_with_lazy_collector(add_lazy_flags(spec))
}

pub(crate) fn add_connect_and_json_flags(spec: command_spec::Spec) -> command_spec::Spec {
    // For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet
    add_json_flags(add_connect_flags(spec))
}

pub(crate) fn get_connect_and_json_flags(args: &arg_spec::Values) -> (ConnectParams, bool, bool) {
    let connect_flags = get_connect_flags(args);
    let JsonFlags { json, pretty } = get_json_flags(args);
    (
        ConnectParams {
            quiet: connect_flags.quiet || json || pretty,
            ..connect_flags
        },
        json,
        pretty,
    )
}

pub(crate) fn server_log_file(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &std::path::Path,
) -> String {
    flow_server_files::server_files_js::log_file(flowconfig_name, tmp_dir, root)
}

pub(crate) fn monitor_log_file(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &std::path::Path,
) -> String {
    flow_server_files::server_files_js::monitor_log_file(flowconfig_name, tmp_dir, root)
}

#[derive(Clone, Debug)]
pub(crate) struct OptionsFlags {
    pub(crate) all: bool,
    pub(crate) debug: bool,
    pub(crate) flowconfig_flags: FlowconfigFlags,
    pub(crate) include_warnings: bool,
    pub(crate) max_warnings: Option<i32>,
    pub(crate) max_workers: Option<i32>,
    pub(crate) merge_timeout: Option<i32>,
    pub(crate) munge_underscore_members: bool,
    pub(crate) no_flowlib: bool,
    pub(crate) profile: bool,
    pub(crate) quiet: bool,
    pub(crate) slow_to_check_logging: flow_common::slow_to_check_logging::SlowToCheckLogging,
    pub(crate) strip_root: bool,
    pub(crate) temp_dir: Option<String>,
    pub(crate) verbose: Option<Verbose>,
    pub(crate) wait_for_recheck: Option<bool>,
    pub(crate) vpn_less: Option<bool>,
    pub(crate) include_suppressions: bool,
    pub(crate) estimate_recheck_time: Option<bool>,
    pub(crate) long_lived_workers: Option<bool>,
    pub(crate) distributed: bool,
    pub(crate) no_autoimports: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct SavedStateFlags {
    pub(crate) saved_state_fetcher: Option<SavedStateFetcher>,
    pub(crate) saved_state_force_recheck: bool,
    pub(crate) saved_state_no_fallback: bool,
    pub(crate) saved_state_skip_version_check: bool,
    pub(crate) saved_state_verify: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct BaseFlags {
    pub(crate) flowconfig_name: String,
}

pub(crate) fn parse_lints_flag(
    base_settings: flow_lint_settings::lint_settings::LintSettings<
        flow_lint_settings::severity::Severity,
    >,
    flag_settings: &[String],
) -> flow_lint_settings::lint_settings::LintSettings<flow_lint_settings::severity::Severity> {
    let lines: Vec<(u32, String)> = flag_settings
        .iter()
        .enumerate()
        .map(|(i, s)| ((i + 1) as u32, s.clone()))
        .collect();
    let settings =
        match flow_lint_settings::lint_settings::LintSettings::of_lines(base_settings, lines) {
            Ok((settings, warnings)) if warnings.is_empty() => Ok(settings),
            Ok((_, warnings)) => {
                // upgrade CLI warnings to errors
                let first = &warnings[0];
                Err((first.line, first.message.clone()))
            }
            Err(err) => Err((err.line, err.message)),
        };
    match settings {
        Ok(settings) => settings,
        Err((line, msg)) => {
            let msg = format!("Error parsing --lints (rule {}): {}", line, msg);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
    }
}

pub(crate) fn add_options_flags(spec: command_spec::Spec) -> command_spec::Spec {
    let spec = add_profile_flag(spec.flag(
        "--debug",
        &arg_spec::truthy(),
        "Print debug info during typecheck",
        Some("FLOW_DEBUG"),
    ))
    .flag(
        "--all",
        &arg_spec::truthy(),
        "Typecheck all files, not just @flow",
        None,
    )
    .flag(
        "--wait-for-recheck",
        &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        "If true, always wait for rechecks to finish before serving commands (default: false)",
        None,
    )
    .flag(
        "--no-flowlib",
        &arg_spec::truthy(),
        "Do not include embedded declarations",
        Some("NO_FLOWLIB"),
    )
    .flag(
        "--munge-underscore-members",
        &arg_spec::truthy(),
        "Treat any class member name with a leading underscore as private",
        None,
    )
    .flag(
        "--max-workers",
        &arg_spec::optional(arg_spec::int()),
        "Maximum number of workers to create (capped by number of cores)",
        Some("FLOW_MAX_WORKERS"),
    );
    let spec = add_warning_flags(spec);
    let spec = add_flowconfig_flags(spec);
    let spec = add_verbose_flags(spec);
    let spec = add_slow_to_check_logging_flags(spec);
    let spec = add_strip_root_flag(spec);
    let spec = add_temp_dir_flag(spec);
    let spec = add_vpn_less_flag(spec);
    add_quiet_flag(spec)
    .flag(
        "--merge-timeout",
        &arg_spec::int(),
        "The maximum time in seconds to attempt to typecheck a file or cycle of files. 0 means no timeout (default: 100)",
        Some("FLOW_MERGE_TIMEOUT"),
    )
    .flag(
        "--include-suppressed",
        &arg_spec::truthy(),
        "Ignore any `suppress_comment` lines in .flowconfig",
        None,
    )
    // restarting to save time is a hack and should be removed. this should
    // not be part of our public API, so not included in the docs.
    .flag(
        "--estimate-recheck-time",
        &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        "",
        Some("FLOW_ESTIMATE_RECHECK_TIME"),
    )
    .flag(
        "--long-lived-workers",
        &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        "",
        Some("FLOW_LONG_LIVED_WORKERS"),
    )
    .flag("--distributed", &arg_spec::truthy(), "", None)
    .flag(
        "--no-autoimports",
        &arg_spec::truthy(),
        "Disable auto-imports (always disabled for foreground commands like full-check/focus-check)",
        None,
    )
}

pub(crate) fn get_options_flags(args: &arg_spec::Values) -> OptionsFlags {
    let merge_timeout = command_spec::get(args, "--merge-timeout", &arg_spec::int()).unwrap();
    match merge_timeout {
        Some(timeout) if timeout < 0 => {
            flow_common_exit::exit(
                FlowExitStatus::CommandlineUsageError,
                Some("--merge-timeout must be non-negative"),
            );
        }
        _ => {}
    }

    let ignores_str =
        command_spec::get(args, "--ignore", &arg_spec::optional(arg_spec::string())).unwrap();
    let untyped_str =
        command_spec::get(args, "--untyped", &arg_spec::optional(arg_spec::string())).unwrap();
    let declarations_str = command_spec::get(
        args,
        "--declaration",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let includes_str =
        command_spec::get(args, "--include", &arg_spec::optional(arg_spec::string())).unwrap();
    let lib_str =
        command_spec::get(args, "--lib", &arg_spec::optional(arg_spec::string())).unwrap();
    let lints_str =
        command_spec::get(args, "--lints", &arg_spec::optional(arg_spec::string())).unwrap();

    OptionsFlags {
        all: command_spec::get(args, "--all", &arg_spec::truthy()).unwrap(),
        debug: command_spec::get(args, "--debug", &arg_spec::truthy()).unwrap(),
        flowconfig_flags: FlowconfigFlags {
            ignores: list_of_string_arg(ignores_str),
            includes: list_of_string_arg(includes_str),
            libs: list_of_string_arg(lib_str),
            raw_lint_severities: list_of_string_arg(lints_str),
            untyped: list_of_string_arg(untyped_str),
            declarations: list_of_string_arg(declarations_str),
        },
        include_warnings: command_spec::get(args, "--include-warnings", &arg_spec::truthy())
            .unwrap(),
        max_warnings: command_spec::get(
            args,
            "--max-warnings",
            &arg_spec::optional(arg_spec::int()),
        )
        .unwrap(),
        max_workers: command_spec::get(args, "--max-workers", &arg_spec::optional(arg_spec::int()))
            .unwrap(),
        merge_timeout,
        munge_underscore_members: command_spec::get(
            args,
            "--munge-underscore-members",
            &arg_spec::truthy(),
        )
        .unwrap(),
        no_flowlib: command_spec::get(args, "--no-flowlib", &arg_spec::truthy()).unwrap(),
        profile: command_spec::get(args, "--profile", &arg_spec::truthy()).unwrap(),
        quiet: command_spec::get(args, "--quiet", &arg_spec::truthy()).unwrap(),
        slow_to_check_logging: flow_common::slow_to_check_logging::SlowToCheckLogging {
            slow_files_logging_internal: command_spec::get(
                args,
                "--log-slow-files-interval",
                &arg_spec::optional(arg_spec::int()),
            )
            .unwrap()
            .map(|x| x as f64),
            slow_components_logging_threshold: command_spec::get(
                args,
                "--log-slow-components-threshold",
                &arg_spec::optional(arg_spec::int()),
            )
            .unwrap()
            .map(|x| x as f64),
            slow_expressions_logging_threshold: command_spec::get(
                args,
                "--log-slow-expressions-threshold",
                &arg_spec::optional(arg_spec::int()),
            )
            .unwrap()
            .map(|x| x as f64),
        },
        strip_root: command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap(),
        temp_dir: command_spec::get(args, "--temp-dir", &arg_spec::optional(arg_spec::string()))
            .unwrap(),
        verbose: verbose_flags(args),
        wait_for_recheck: command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        )
        .unwrap(),
        vpn_less: command_spec::get(
            args,
            "--vpn-less",
            &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        )
        .unwrap(),
        include_suppressions: command_spec::get(args, "--include-suppressed", &arg_spec::truthy())
            .unwrap(),
        estimate_recheck_time: command_spec::get(
            args,
            "--estimate-recheck-time",
            &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        )
        .unwrap(),
        long_lived_workers: command_spec::get(
            args,
            "--long-lived-workers",
            &arg_spec::optional_value_with_default(true, arg_spec::bool_flag()),
        )
        .unwrap(),
        distributed: command_spec::get(args, "--distributed", &arg_spec::truthy()).unwrap(),
        no_autoimports: command_spec::get(args, "--no-autoimports", &arg_spec::truthy()).unwrap(),
    }
}

pub(crate) fn saved_state_fetcher_flag() -> arg_spec::FlagType<Option<SavedStateFetcher>> {
    arg_spec::enum_flag(vec![
        ("none", SavedStateFetcher::DummyFetcher),
        ("local", SavedStateFetcher::LocalFetcher),
        ("scm", SavedStateFetcher::ScmFetcher),
        ("fb", SavedStateFetcher::FbFetcher),
    ])
}

pub(crate) fn add_saved_state_flags(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--saved-state-fetcher",
        &saved_state_fetcher_flag(),
        "Which saved state fetcher Flow should use (none, local) (default: none)",
        None,
    )
    .flag(
        "--saved-state-force-recheck",
        &arg_spec::truthy(),
        "Force a lazy server to recheck the changes since the saved state was generated",
        None,
    )
    .flag(
        "--saved-state-no-fallback",
        &arg_spec::truthy(),
        "If saved state fails to load, exit (normally fallback is to initialize from scratch)",
        None,
    )
    // This is really unsafe! Saved state is marshal'd OCaml data and it's
    // easy to introduce serialization differences that would lead to
    // segfaults. This is only for debugging.
    .flag(
        "--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED",
        &arg_spec::truthy(),
        "",
        Some("FLOW_SAVED_STATE_SKIP_VERSION_CHECK_DO_NOT_USE_OR_YOU_WILL_BE_FIRED"),
    )
    .flag(
        "--saved-state-verify",
        &arg_spec::truthy(),
        "Verifies that the saved state matches what is on disk (for debugging only)",
        None,
    )
}

pub(crate) fn get_saved_state_flags(args: &arg_spec::Values) -> SavedStateFlags {
    SavedStateFlags {
        saved_state_fetcher: command_spec::get(
            args,
            "--saved-state-fetcher",
            &saved_state_fetcher_flag(),
        )
        .unwrap(),
        saved_state_force_recheck: command_spec::get(
            args,
            "--saved-state-force-recheck",
            &arg_spec::truthy(),
        )
        .unwrap(),
        saved_state_no_fallback: command_spec::get(
            args,
            "--saved-state-no-fallback",
            &arg_spec::truthy(),
        )
        .unwrap(),
        saved_state_skip_version_check: command_spec::get(
            args,
            "--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED",
            &arg_spec::truthy(),
        )
        .unwrap(),
        saved_state_verify: command_spec::get(args, "--saved-state-verify", &arg_spec::truthy())
            .unwrap(),
    }
}

pub(crate) fn add_flowconfig_name_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--flowconfig-name",
        &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
}

pub(crate) fn add_base_flags(spec: command_spec::Spec) -> command_spec::Spec {
    add_flowconfig_name_flag(spec)
}

pub(crate) fn get_base_flags(args: &arg_spec::Values) -> BaseFlags {
    BaseFlags {
        flowconfig_name: command_spec::get(
            args,
            "--flowconfig-name",
            &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        )
        .unwrap(),
    }
}

const DEFAULT_FILE_WATCHER_TIMEOUT: u32 = 120;

const DEFAULT_FILE_WATCHER_MERGEBASE_WITH: &str = "master";

pub(crate) fn file_watcher_flag() -> arg_spec::FlagType<Option<flow_config::FileWatcher>> {
    arg_spec::enum_flag(vec![
        ("none", flow_config::FileWatcher::NoFileWatcher),
        ("dfind", flow_config::FileWatcher::DFind),
        ("watchman", flow_config::FileWatcher::Watchman),
        ("edenfs", flow_config::FileWatcher::EdenFS),
    ])
}

pub(crate) fn add_file_watcher_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--file-watcher",
        &arg_spec::optional(file_watcher_flag()),
        "Which file watcher Flow should use (none, dfind, watchman, edenfs). Flow will ignore file system events if this is set to none. (default: dfind)",
        None,
    )
    .flag(
        "--file-watcher-debug",
        &arg_spec::truthy(),
        "Enable debug logging for the file watcher. This is very noisy",
        Some("FLOW_FILE_WATCHER_DEBUG"),
    )
    .flag(
        "--file-watcher-timeout",
        &arg_spec::optional(arg_spec::uint()),
        &format!(
            "Maximum time to wait for the file watcher to initialize, in seconds. 0 means no timeout (default: {})",
            DEFAULT_FILE_WATCHER_TIMEOUT
        ),
        None,
    )
    .flag(
        "--file-watcher-mergebase-with",
        &arg_spec::optional(arg_spec::string()),
        &format!(
            "Symbolic commit against which to compute the mergebase used to find changed files. (default: {})",
            DEFAULT_FILE_WATCHER_MERGEBASE_WITH
        ),
        None,
    )
    .flag(
        "--file-watcher-sync-timeout",
        &arg_spec::optional(arg_spec::uint()),
        "Maximum time to wait for the file watcher to synchronize, in milliseconds. 0 means no timeout. Currently only used by Watchman.",
        None,
    )
}

pub(crate) struct FileWatcherFlags {
    pub(crate) file_watcher: Option<flow_config::FileWatcher>,
    pub(crate) file_watcher_debug: bool,
    pub(crate) file_watcher_timeout: Option<u32>,
    pub(crate) file_watcher_mergebase_with: Option<String>,
    pub(crate) file_watcher_sync_timeout: Option<u32>,
}

pub(crate) fn get_file_watcher_flags(args: &arg_spec::Values) -> FileWatcherFlags {
    FileWatcherFlags {
        file_watcher: command_spec::get(
            args,
            "--file-watcher",
            &arg_spec::optional(file_watcher_flag()),
        )
        .unwrap(),
        file_watcher_debug: command_spec::get(args, "--file-watcher-debug", &arg_spec::truthy())
            .unwrap(),
        file_watcher_timeout: command_spec::get(
            args,
            "--file-watcher-timeout",
            &arg_spec::optional(arg_spec::uint()),
        )
        .unwrap()
        .map(|x| x as u32),
        file_watcher_mergebase_with: command_spec::get(
            args,
            "--file-watcher-mergebase-with",
            &arg_spec::optional(arg_spec::string()),
        )
        .unwrap(),
        file_watcher_sync_timeout: command_spec::get(
            args,
            "--file-watcher-sync-timeout",
            &arg_spec::optional(arg_spec::uint()),
        )
        .unwrap()
        .map(|x| x as u32),
    }
}

pub(crate) fn add_options_and_json_flags(spec: command_spec::Spec) -> command_spec::Spec {
    // For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet
    add_json_flags(add_options_flags(spec))
}

pub(crate) fn add_json_version_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--json-version",
        &arg_spec::optional(arg_spec::string()),
        "Version of the JSON output format (1 or 2)",
        None,
    )
}

pub(crate) fn get_json_version(
    args: &arg_spec::Values,
) -> Option<flow_common_errors::error_utils::json_output::JsonVersion> {
    command_spec::get(
        args,
        "--json-version",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap()
    .map(|version| match version.as_str() {
        "1" => flow_common_errors::error_utils::json_output::JsonVersion::JsonV1,
        "2" => flow_common_errors::error_utils::json_output::JsonVersion::JsonV2,
        _ => {
            let msg = format!("Invalid --json-version value: {}", version);
            flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
        }
    })
}

pub(crate) fn add_no_cgroup_flag(spec: command_spec::Spec) -> command_spec::Spec {
    // If a command uses this flag, then it will automatically exec systemd-run (if systemd-run is
    // available). This can add a couple hundred ms to the start up time of the command. Only commands
    // that are resource-intensive (or spawn resource intensive processes) and probably should run in
    // cgroups should use this flag.
    spec.flag(
        "--no-cgroup",
        &arg_spec::truthy(),
        "Don't automatically run this command in a cgroup (if cgroups are available)",
        None,
    )
}

pub(crate) fn maybe_run_in_cgroup(args: &arg_spec::Values) {
    let no_cgroup = command_spec::get(args, "--no-cgroup", &arg_spec::truthy()).unwrap();
    if no_cgroup {
        return;
    }
    #[cfg(unix)]
    exec_in_cgroup_if_systemd_available();
}

#[cfg(unix)]
fn exec_in_cgroup_if_systemd_available() {
    // We only trigger this behavior if we're on Unix and systemd-run is in the path
    fn can_run_systemd() -> bool {
        // Use `timeout` in case it hangs mysteriously. `--quiet` only suppresses stdout.
        ProcessCommand::new("timeout")
            .arg("1")
            .arg("systemd-run")
            .arg("--quiet")
            .arg("--user")
            .arg("--scope")
            .arg("--")
            .arg("true")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            // If all goes right, `systemd-run` will return immediately with exit code 0 and run `true`
            // asynchronously as a service. If it goes wrong, it will exit with a non-zero exit code
            .status()
            .is_ok_and(|status| status.success())
    }

    // Sometimes systemd-run is available but we can't use it. For example, the systemd might not have
    // a proper working user session, so we might not be able to run commands via systemd-run as a
    // user process. Notably, `--user --scope` is broken under cgroupv2 in systemd < 238, and exits
    // code 1 (https://github.com/facebook/flow/issues/8012).
    if !can_run_systemd() {
        return;
    }

    let argv: Vec<_> = std::env::args_os().collect();
    if argv.len() < 2 {
        return;
    }

    let flow_exe = argv[0].clone();
    let command = argv[1].clone();
    let mut flow_args = vec![flow_exe];
    flow_args.push(command);
    flow_args.push("--no-cgroup".into());
    flow_args.extend(argv.into_iter().skip(2));

    // Basically re-exec ourselves with the --no-cgroup flag using systemd-run
    use std::os::unix::process::CommandExt;

    let err = ProcessCommand::new("systemd-run")
        .arg("--quiet")
        .arg("--user")
        .arg("--scope")
        .arg("--slice")
        .arg("flow.slice")
        .arg("--")
        .args(&flow_args)
        .exec();
    eprintln!("Failed to re-exec under systemd-run: {}", err);
}

pub(crate) fn get_temp_dir(cli_value: &Option<String>) -> String {
    match cli_value {
        Some(v) => v.clone(),
        None => flow_server_files::server_files_js::default_temp_dir()
            .to_string_lossy()
            .to_string(),
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct MakeOptionsOverrides {
    pub(crate) autoimports: Option<bool>,
    pub(crate) all: Option<bool>,
    pub(crate) debug: bool,
    pub(crate) distributed: bool,
    pub(crate) estimate_recheck_time: Option<bool>,
    pub(crate) flowconfig_flags: Option<FlowconfigFlags>,
    pub(crate) include_suppressions: Option<bool>,
    pub(crate) include_warnings: bool,
    pub(crate) lazy_mode: Option<flow_config::LazyMode>,
    pub(crate) long_lived_workers: Option<bool>,
    pub(crate) max_warnings: Option<i32>,
    pub(crate) max_workers: Option<i32>,
    pub(crate) merge_timeout: Option<i32>,
    pub(crate) munge_underscore_members: bool,
    pub(crate) no_autoimports: bool,
    pub(crate) profile: Option<bool>,
    pub(crate) quiet: bool,
    pub(crate) saved_state_fetcher: Option<SavedStateFetcher>,
    pub(crate) saved_state_force_recheck: Option<bool>,
    pub(crate) saved_state_no_fallback: Option<bool>,
    pub(crate) saved_state_skip_version_check: Option<bool>,
    pub(crate) saved_state_verify: Option<bool>,
    pub(crate) slow_to_check_logging:
        Option<flow_common::slow_to_check_logging::SlowToCheckLogging>,
    pub(crate) strip_root: bool,
    pub(crate) temp_dir: Option<String>,
    pub(crate) verbose: Option<Verbose>,
    pub(crate) vpn_less: Option<bool>,
    pub(crate) wait_for_recheck: Option<bool>,
}

pub(super) fn make_options(
    flowconfig: FlowConfig,
    flowconfig_hash: String,
    flowconfig_name: String,
    root: std::path::PathBuf,
    temp_dir: String,
    cli_no_flowlib: bool,
    overrides: MakeOptionsOverrides,
) -> Options {
    let FlowConfig {
        rollouts,
        ignores,
        untyped,
        declarations,
        includes,
        libs,
        lint_severities,
        strict_mode,
        options:
            flow_config::opts::Opts {
                abstract_classes,
                all,
                autoimports,
                autoimports_min_characters,
                autoimports_ranked_by_usage,
                autoimports_ranked_by_usage_boost_exact_match_min_length,
                automatic_require_default,
                babel_loose_array_spread,
                ban_spread_key_props,
                casting_syntax,
                casting_syntax_only_support_as_excludes,
                channel_mode,
                component_syntax,
                async_component_syntax,
                async_component_syntax_includes,
                dev_only_refinement_info_as_errors,
                emoji: _emoji,
                enable_const_params,
                enums,
                estimate_recheck_time,
                exact_by_default,
                facebook_fbs,
                facebook_fbt,
                facebook_module_interop,
                file_watcher: _file_watcher,
                file_watcher_edenfs_throttle_time_ms: _file_watcher_edenfs_throttle_time_ms,
                file_watcher_edenfs_timeout: _file_watcher_edenfs_timeout,
                file_watcher_mergebase_with: _file_watcher_mergebase_with,
                file_watcher_mergebase_with_git: _file_watcher_mergebase_with_git,
                file_watcher_mergebase_with_hg: _file_watcher_mergebase_with_hg,
                file_watcher_timeout: _file_watcher_timeout,
                files_implicitly_include_root,
                format_bracket_spacing,
                format_single_quotes,
                gc_worker_custom_major_ratio,
                gc_worker_custom_minor_max_size,
                gc_worker_custom_minor_ratio,
                gc_worker_major_heap_increment,
                gc_worker_minor_heap_size,
                gc_worker_space_overhead,
                gc_worker_window_size,
                haste_module_ref_prefix,
                haste_paths_excludes,
                haste_paths_includes,
                hook_compatibility,
                hook_compatibility_includes,
                hook_compatibility_excludes,
                ignore_non_literal_requires,
                instance_t_objkit_fix,
                include_warnings,
                jest_integration,
                lazy_mode,
                llm_context_include_imports,
                log_per_error_typing_telemetry,
                log_saving,
                long_lived_workers,
                max_files_checked_per_worker,
                max_files_checked_per_worker_rust_port,
                max_header_tokens,
                max_seconds_for_check_per_worker,
                max_workers,
                merge_timeout,
                missing_module_generators,
                module_declaration_dirnames,
                mut module_file_exts,
                module_name_mappers,
                module_resource_exts,
                module_system,
                modules_are_use_strict,
                multi_platform,
                multi_platform_extensions,
                multi_platform_extension_group_mapping,
                multi_platform_ambient_supports_platform_project_overrides,
                munge_underscores,
                no_flowlib,
                no_unchecked_indexed_access,
                node_modules_errors,
                node_main_fields,
                node_package_export_conditions,
                node_resolver_allow_root_relative,
                node_resolver_dirnames,
                node_resolver_root_relative_dirnames,
                opaque_type_new_bound_syntax,
                pattern_matching,
                pattern_matching_instance_patterns,
                projects,
                projects_overlap_mapping,
                projects_path_mapping,
                projects_strict_boundary,
                projects_strict_boundary_validate_import_pattern_opt_outs,
                projects_strict_boundary_import_pattern_opt_outs,
                react_custom_jsx_typing,
                stylex_shorthand_prop,
                react_ref_as_prop,
                react_rules,
                react_runtime,
                records,
                records_includes,
                recursion_limit,
                relay_integration,
                relay_integration_esmodules,
                relay_integration_excludes,
                relay_integration_module_prefix,
                relay_integration_module_prefix_includes,
                root_name,
                saved_state_direct_serialization,
                saved_state_parallel_decompress,
                saved_state_fetcher,
                saved_state_persist_export_index,
                saved_state_reinit_on_lib_change,
                saved_state_skip_version_check,
                shm_hash_table_pow: _shm_hash_table_pow,
                shm_heap_size: _shm_heap_size,
                supported_operating_systems,
                strict_es6_import_export,
                ts_syntax,
                allow_readonly_variance,
                allow_variance_keywords,
                ts_utility_syntax,
                tslib_syntax,
                typescript_library_definition_support,
                deprecated_utilities,
                deprecated_utilities_excludes,
                deprecated_colon_extends,
                deprecated_colon_extends_excludes,
                enable_custom_error,
                assert_operator,
                type_expansion_recursion_limit,
                unsuppressable_error_codes,
                use_unknown_in_catch_variables,
                vpn_less,
                wait_for_recheck,
                watchman_defer_states: _watchman_defer_states,
                watchman_sync_timeout: _watchman_sync_timeout,
                max_workers_full_check,
            },
        version: _version,
    } = flowconfig;

    let MakeOptionsOverrides {
        autoimports: autoimports_override,
        all: all_override,
        debug: debug_override,
        distributed: distributed_override,
        estimate_recheck_time: estimate_recheck_time_override,
        flowconfig_flags: flowconfig_flags_override,
        include_suppressions: include_suppressions_override,
        include_warnings: include_warnings_override,
        lazy_mode: lazy_mode_override,
        long_lived_workers: long_lived_workers_override,
        max_warnings: max_warnings_override,
        max_workers: max_workers_override,
        merge_timeout: merge_timeout_override,
        munge_underscore_members: munge_underscore_members_override,
        no_autoimports: no_autoimports_override,
        profile: profile_override,
        quiet: quiet_override,
        saved_state_fetcher: saved_state_fetcher_override,
        saved_state_force_recheck,
        saved_state_no_fallback,
        saved_state_skip_version_check: saved_state_skip_version_check_override,
        saved_state_verify,
        slow_to_check_logging: slow_to_check_logging_override,
        strip_root: strip_root_override,
        temp_dir: temp_dir_override,
        verbose: verbose_override,
        vpn_less: vpn_less_override,
        wait_for_recheck: wait_for_recheck_override,
    } = overrides;

    let FlowconfigFlags {
        ignores: ignores_override,
        includes: includes_override,
        libs: libs_override,
        raw_lint_severities,
        untyped: untyped_override,
        declarations: declarations_override,
    } = flowconfig_flags_override.unwrap_or_default();

    let all = all_override.unwrap_or(all.unwrap_or(false));
    let autoimports =
        !no_autoimports_override && autoimports_override.unwrap_or(autoimports.unwrap_or(true));
    let autoimports_min_characters = autoimports_min_characters.unwrap_or(0) as i32;
    let autoimports_ranked_by_usage_boost_exact_match_min_length =
        autoimports_ranked_by_usage_boost_exact_match_min_length as i32;
    let automatic_require_default = automatic_require_default.unwrap_or(false);
    let babel_loose_array_spread = babel_loose_array_spread.unwrap_or(false);
    let ban_spread_key_props = ban_spread_key_props.unwrap_or(false);
    let casting_syntax = casting_syntax.unwrap_or(CastingSyntax::Both);
    let channel_mode = match channel_mode {
        Some(flow_config::ChannelMode::Pipe) => options::ChannelMode::Pipe,
        Some(flow_config::ChannelMode::Socket) => options::ChannelMode::Socket,
        None => options::ChannelMode::Pipe,
    };
    let enable_const_params = enable_const_params.unwrap_or(false);
    let enable_pattern_matching = pattern_matching.unwrap_or(false);
    let enable_pattern_matching_instance_patterns =
        pattern_matching_instance_patterns.unwrap_or(false);
    let enable_records = records.unwrap_or(false);
    let estimate_recheck_time = estimate_recheck_time_override
        .or(estimate_recheck_time)
        .unwrap_or(true);
    let exact_by_default = exact_by_default.unwrap_or(true);
    let lazy_mode = matches!(
        lazy_mode_override.unwrap_or(lazy_mode.unwrap_or(LazyMode::NonLazy)),
        LazyMode::Lazy | LazyMode::WatchmanDeprecated
    );
    let max_files_checked_per_worker =
        max_files_checked_per_worker_rust_port.unwrap_or(max_files_checked_per_worker) as i32;
    let max_header_tokens = max_header_tokens as i32;
    let merge_timeout = match merge_timeout_override {
        Some(0) => None,
        Some(timeout) => Some(timeout as f64),
        None => merge_timeout.map(|t| t as f64),
    };
    let use_unknown_in_catch_variables = use_unknown_in_catch_variables.unwrap_or(false);
    let type_expansion_recursion_limit = type_expansion_recursion_limit as i32;
    let lint_severities = parse_lints_flag(lint_severities, &raw_lint_severities);

    let enable_jest_integration = jest_integration;
    let enable_relay_integration = relay_integration;
    let temp_dir = temp_dir_override.unwrap_or(temp_dir);
    let vpn_less = vpn_less_override.unwrap_or(vpn_less);

    let unsuppressable_error_codes: Arc<HashSet<FlowSmolStr>> = Arc::new(
        unsuppressable_error_codes
            .into_iter()
            .map(FlowSmolStr::new)
            .collect(),
    );

    let log_saving: Arc<BTreeMap<String, LogSaving>> = Arc::new(
        log_saving
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    LogSaving {
                        threshold_time_ms: v.threshold_time_ms,
                        limit: v.limit,
                        rate: v.rate,
                    },
                )
            })
            .collect(),
    );

    let flowlib_dir = flow_flowlib::libdir(
        no_flowlib || cli_no_flowlib,
        &std::path::PathBuf::from(&temp_dir),
    );
    flow_flowlib::extract(&flowlib_dir);
    let flowlib_path = flow_flowlib::path_of_libdir(&flowlib_dir).to_path_buf();

    // Initialize File_key root prefixes for relative path storage
    flow_parser::file_key::set_project_root(&root.to_string_lossy());
    flow_parser::file_key::set_flowlib_root(&flowlib_path.to_string_lossy());

    let casting_syntax_only_support_as_excludes: Arc<[Regex]> =
        casting_syntax_only_support_as_excludes
            .iter()
            .map(|s| {
                let expanded = flow_common::files::expand_project_root_token(&root, s);
                Regex::new(&expanded).unwrap()
            })
            .collect::<Vec<_>>()
            .into();

    let hook_compatibility_excludes: Arc<[Regex]> = hook_compatibility_excludes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();

    let hook_compatibility_includes: Arc<[Regex]> = hook_compatibility_includes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();

    let async_component_syntax_includes: Arc<[Regex]> = async_component_syntax_includes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();

    let relay_integration_excludes: Arc<[Regex]> = relay_integration_excludes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();

    let relay_integration_module_prefix_includes: Arc<[Regex]> =
        relay_integration_module_prefix_includes
            .iter()
            .map(|s| {
                let expanded = flow_common::files::expand_project_root_token(&root, s);
                Regex::new(&expanded).unwrap()
            })
            .collect::<Vec<_>>()
            .into();

    let deprecated_utilities: Arc<BTreeMap<String, Vec<String>>> = Arc::new(
        deprecated_utilities
            .into_iter()
            .map(|(k, v)| {
                let v = v
                    .into_iter()
                    .map(|s| {
                        let s = flow_common::files::expand_project_root_token(&root, &s);
                        flow_common::files::expand_builtin_root_token(&flowlib_path, &s)
                    })
                    .collect();
                (k, v)
            })
            .collect(),
    );

    let deprecated_utilities_excludes: Arc<[Regex]> = deprecated_utilities_excludes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();

    let enabled_rollouts: Arc<BTreeMap<String, String>> = Arc::new(
        rollouts
            .into_iter()
            .map(|(rollout, config)| (rollout, config.enabled_group))
            .collect(),
    );
    let gc_worker = GcControl {
        minor_heap_size: gc_worker_minor_heap_size.or(Some(1024 * 1024 * 2)),
        major_heap_increment: gc_worker_major_heap_increment,
        space_overhead: gc_worker_space_overhead,
        window_size: gc_worker_window_size,
        custom_major_ratio: gc_worker_custom_major_ratio,
        custom_minor_ratio: gc_worker_custom_minor_ratio,
        custom_minor_max_size: gc_worker_custom_minor_max_size,
    };

    let format = Format {
        bracket_spacing: format_bracket_spacing.unwrap_or(true),
        single_quotes: format_single_quotes.unwrap_or(false),
    };

    let available = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    // Priority: CLI --max-workers > server.max_workers.full_check (non-lazy only)
    // > server.max_workers > system default (nbr_procs).
    // The result is capped at nbr_procs.
    let config_max_workers = if !lazy_mode {
        max_workers_full_check.or(max_workers)
    } else {
        max_workers
    };
    let max_workers = max_workers_override.unwrap_or_else(|| {
        std::env::var("FLOW_MAX_WORKERS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or_else(|| config_max_workers.map(|w| w as usize).unwrap_or(available))
            as i32
    });

    let log_file = std::path::PathBuf::from(flow_server_files::server_files_js::log_file(
        &flowconfig_name,
        &temp_dir,
        &root,
    ));

    if typescript_library_definition_support {
        for ext in [".d.cts", ".d.mts", ".d.ts"] {
            if !module_file_exts.contains(&ext.into()) {
                module_file_exts.insert(0, ext.into());
            }
        }
    }

    let node_package_export_conditions = if typescript_library_definition_support {
        // When typescript_library_definition_support is enabled, we add "types"
        // and "import" to the valid export conditions. "types" points to .d.ts
        // declaration files. "import" matches the ESM entry point, which is
        // appropriate when packages provide .d.ts or .d.mts types for their
        // ESM exports. We only add these when not already user-configured.
        let mut conditions = node_package_export_conditions;
        if !conditions.contains(&"types".to_string()) {
            conditions.insert(0, "types".to_string());
        }
        if !conditions.contains(&"import".to_string()) {
            conditions.insert(0, "import".to_string());
        }
        conditions
    } else {
        node_package_export_conditions
    };

    let file_options = {
        use flow_common::files::FileOptions;

        let ignores = {
            let mut merged: Vec<(String, Option<String>)> = ignores_override
                .into_iter()
                .map(|ignore| (ignore, None))
                .collect();
            merged.extend(ignores);
            merged
        };
        let ignores_processed = ignores
            .into_iter()
            .map(|(path, backup)| {
                let pattern = remove_exclusion(&path);
                let expanded = flow_common::files::expand_project_root_token(&root, pattern);
                let regex = Regex::new(&expanded).unwrap();
                ((path, backup), regex)
            })
            .collect();

        let untyped = {
            let mut merged = untyped_override;
            merged.extend(untyped);
            merged
        };
        let untyped_processed = untyped
            .into_iter()
            .map(|pattern| {
                let expanded = flow_common::files::expand_project_root_token(
                    &root,
                    remove_exclusion(&pattern),
                );
                let regex = Regex::new(&expanded).unwrap();
                (pattern, regex)
            })
            .collect();

        let declarations = {
            let mut merged = declarations_override;
            merged.extend(declarations);
            merged
        };
        let declarations_processed = declarations
            .into_iter()
            .map(|pattern| {
                let expanded = flow_common::files::expand_project_root_token(
                    &root,
                    remove_exclusion(&pattern),
                );
                let regex = Regex::new(&expanded).unwrap();
                (pattern, regex)
            })
            .collect();

        let flowtyped_path = flow_common::files::make_path_absolute(&root, "flow-typed");
        let mut has_explicit_flowtyped_lib = false;
        let mut lib_paths_processed = libs
            .into_iter()
            .map(|(scope, path)| {
                let scope_str = scope.map(|s| s.to_string());
                let expanded = flow_common::files::expand_project_root_token(&root, &path);
                let path_buf = flow_common::files::make_path_absolute(&root, &expanded);
                if path_buf == flowtyped_path {
                    has_explicit_flowtyped_lib = true;
                }
                (scope_str, path_buf)
            })
            .collect::<Vec<_>>();
        if !has_explicit_flowtyped_lib && flowtyped_path.exists() {
            lib_paths_processed.insert(0, (None, flowtyped_path));
        }
        for path in libs_override {
            lib_paths_processed.push((None, flow_common::files::make_path_absolute(&root, &path)));
        }

        let includes_processed = {
            let mut merged_includes = includes_override;
            merged_includes.extend(includes);
            let mut matcher = PathMatcher::empty();
            // Explicitly included paths are always added to the path_matcher
            for include in merged_includes {
                let expanded = flow_common::files::expand_project_root_token(&root, &include);
                matcher.add_path(&flow_common::files::make_path_absolute(&root, &expanded));
            }
            // Implicitly included paths are added only if they're not already being watched
            let mut implicit_paths = Vec::new();
            if files_implicitly_include_root {
                implicit_paths.push(root.to_path_buf());
            }
            implicit_paths.extend(lib_paths_processed.iter().map(|(_, path)| path.clone()));
            // Shortest path first
            implicit_paths.sort_by_key(|path| path.to_string_lossy().len());
            for path in implicit_paths {
                let path_str = path.to_string_lossy().to_string();
                // If this include is already covered by an explicit include or a shorter implicit include,
                // then skip it
                if matcher.matches(&path_str) {
                    continue;
                }
                matcher.add_path(&path);
            }
            matcher
        };

        let haste_paths_excludes_processed = haste_paths_excludes
            .into_iter()
            .map(|s| {
                let expanded = flow_common::files::expand_project_root_token(&root, &s);
                Regex::new(&expanded).unwrap()
            })
            .collect();

        let haste_paths_includes_processed = haste_paths_includes
            .into_iter()
            .map(|s| {
                let expanded = flow_common::files::expand_project_root_token(&root, &s);
                Regex::new(&expanded).unwrap()
            })
            .collect();

        Arc::new(FileOptions {
            default_lib_dir: Some(match flowlib_dir {
                FlowlibDir::Prelude(path) => flow_common::files::LibDir::Prelude(path),
                FlowlibDir::Flowlib(path) => flow_common::files::LibDir::Flowlib(path),
            }),
            ignores: ignores_processed,
            untyped: untyped_processed,
            declarations: declarations_processed,
            implicitly_include_root: files_implicitly_include_root,
            haste_paths_excludes: haste_paths_excludes_processed,
            haste_paths_includes: haste_paths_includes_processed,
            includes: includes_processed,
            lib_paths: lib_paths_processed,
            module_declaration_dirnames: module_declaration_dirnames
                .into_iter()
                .map(|dir| {
                    let expanded = flow_common::files::expand_project_root_token(&root, &dir);
                    flow_common::files::cached_canonicalize(std::path::Path::new(&expanded))
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or(expanded)
                })
                .collect(),
            module_file_exts,
            module_resource_exts: module_resource_exts.into_iter().collect(),
            multi_platform: multi_platform.unwrap_or(false),
            multi_platform_extensions: multi_platform_extensions.to_vec(),
            multi_platform_extension_group_mapping: multi_platform_extension_group_mapping.to_vec(),
            node_resolver_dirnames,
        })
    };

    let projects_options = {
        use flow_common::flow_projects::*;

        let projects_vec = vec1::Vec1::try_from_vec(projects)
            .unwrap_or_else(|_| vec1::vec1![FlowSmolStr::new_inline("default")]);

        let projects_overlap_mapping_vec: BTreeMap<FlowSmolStr, Vec<FlowSmolStr>> =
            projects_overlap_mapping
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().collect()))
                .collect();
        let map_path = move |path: String| {
            let expanded = flow_common::files::expand_project_root_token_as_relative(&path);
            Regex::new(&format!("^{}", expanded)).unwrap()
        };

        Arc::new(ProjectsOptions::mk(
            projects_vec,
            projects_overlap_mapping_vec,
            map_path,
            projects_path_mapping,
            projects_strict_boundary,
            projects_strict_boundary_validate_import_pattern_opt_outs,
            projects_strict_boundary_import_pattern_opt_outs,
            multi_platform_ambient_supports_platform_project_overrides,
        ))
    };

    let deprecated_colon_extends: Arc<[String]> = deprecated_colon_extends
        .into_iter()
        .map(|s| {
            let s = flow_common::files::expand_project_root_token(&root, &s);
            flow_common::files::expand_builtin_root_token(&flowlib_path, &s)
        })
        .collect::<Vec<_>>()
        .into();
    let deprecated_colon_extends_excludes: Arc<[Regex]> = deprecated_colon_extends_excludes
        .iter()
        .map(|s| {
            let expanded = flow_common::files::expand_project_root_token(&root, s);
            Regex::new(&expanded).unwrap()
        })
        .collect::<Vec<_>>()
        .into();
    let saved_state_direct_serialization =
        match std::env::var("FLOW_SAVED_STATE_DIRECT_SERIALIZATION")
            .ok()
            .as_deref()
        {
            Some("1" | "true") => true,
            Some("0" | "false") => false,
            _ => saved_state_direct_serialization,
        };
    let saved_state_persist_export_index =
        match std::env::var("FLOW_SAVED_STATE_PERSIST_EXPORT_INDEX")
            .ok()
            .as_deref()
        {
            Some("1" | "true") => true,
            Some("0" | "false") => false,
            _ => saved_state_persist_export_index,
        };
    Options {
        abstract_classes,
        all,
        assert_operator,
        autoimports,
        autoimports_min_characters,
        autoimports_ranked_by_usage,
        autoimports_ranked_by_usage_boost_exact_match_min_length,
        automatic_require_default,
        babel_loose_array_spread,
        ban_spread_key_props,
        casting_syntax,
        casting_syntax_only_support_as_excludes,
        channel_mode,
        component_syntax,
        async_component_syntax,
        async_component_syntax_includes,
        debug: debug_override,
        deprecated_utilities,
        deprecated_utilities_excludes,
        deprecated_colon_extends,
        deprecated_colon_extends_excludes,
        dev_only_refinement_info_as_errors,
        distributed: distributed_override,
        enable_const_params,
        enable_custom_error,
        enable_jest_integration,
        enable_pattern_matching,
        enable_pattern_matching_instance_patterns,
        enable_records,
        enable_relay_integration,
        enabled_rollouts,
        enums,
        estimate_recheck_time,
        exact_by_default,
        facebook_fbs: facebook_fbs.map(FlowSmolStr::new),
        facebook_fbt: facebook_fbt.map(FlowSmolStr::new),
        facebook_module_interop,
        file_options,
        flowconfig_hash: FlowSmolStr::new(flowconfig_hash),
        flowconfig_name: FlowSmolStr::new(flowconfig_name),
        format,
        gc_worker,
        haste_module_ref_prefix: haste_module_ref_prefix.map(FlowSmolStr::new),
        hook_compatibility,
        hook_compatibility_excludes,
        hook_compatibility_includes,
        ignore_non_literal_requires,
        include_suppressions: include_suppressions_override.unwrap_or(false),
        include_warnings: include_warnings_override
            || max_warnings_override.is_some()
            || include_warnings,
        instance_t_objkit_fix,
        lazy_mode,
        llm_context_include_imports,
        log_per_error_typing_telemetry,
        lint_severities,
        log_file: Arc::new(log_file),
        log_saving,
        long_lived_workers: long_lived_workers_override.unwrap_or(long_lived_workers),
        max_files_checked_per_worker,
        max_header_tokens,
        max_seconds_for_check_per_worker,
        max_workers,
        merge_timeout,
        missing_module_generators: Arc::from(missing_module_generators),
        module_system,
        module_name_mappers: Arc::from(module_name_mappers),
        modules_are_use_strict,
        munge_underscores: munge_underscore_members_override || munge_underscores,
        no_unchecked_indexed_access,
        node_modules_errors,
        node_main_fields: Arc::from(node_main_fields),
        node_package_export_conditions: Arc::from(node_package_export_conditions),
        node_resolver_allow_root_relative,
        node_resolver_root_relative_dirnames: Arc::from(
            node_resolver_root_relative_dirnames
                .into_iter()
                .map(|(applicable_dir_opt, dirname)| {
                    (
                        applicable_dir_opt
                            .map(|s| flow_common::files::expand_project_root_token_as_relative(&s)),
                        dirname,
                    )
                })
                .collect::<Vec<_>>(),
        ),
        opaque_type_new_bound_syntax,
        profile: profile_override.unwrap_or(false),
        projects_options,
        quiet: quiet_override,
        records_includes: Arc::from(
            records_includes
                .iter()
                .map(|s| flow_common::files::expand_project_root_token(&root, s))
                .collect::<Vec<_>>(),
        ),
        react_custom_jsx_typing,
        react_ref_as_prop,
        react_rules: Arc::from(react_rules),
        react_runtime,
        recursion_limit,
        relay_integration_esmodules,
        relay_integration_excludes,
        relay_integration_module_prefix: relay_integration_module_prefix.map(FlowSmolStr::new),
        relay_integration_module_prefix_includes,
        root: Arc::new(root),
        root_name: root_name.map(FlowSmolStr::new),
        saved_state_direct_serialization,
        saved_state_parallel_decompress,
        // The CLI flag overrides the .flowconfig
        saved_state_fetcher: saved_state_fetcher_override.unwrap_or(saved_state_fetcher),
        saved_state_force_recheck: saved_state_force_recheck.unwrap_or(false),
        saved_state_no_fallback: saved_state_no_fallback.unwrap_or(false),
        saved_state_persist_export_index,
        saved_state_reinit_on_lib_change,
        saved_state_skip_version_check: saved_state_skip_version_check_override.unwrap_or(false)
            || saved_state_skip_version_check,
        saved_state_verify: saved_state_verify.unwrap_or(false),
        slow_to_check_logging: slow_to_check_logging_override.unwrap_or_default(),
        strict_es6_import_export,
        strict_mode,
        strip_root: strip_root_override,
        supported_operating_systems,
        stylex_shorthand_prop,
        temp_dir: FlowSmolStr::new(temp_dir),
        ts_syntax,
        allow_readonly_variance,
        allow_variance_keywords,
        tslib_syntax,
        typescript_library_definition_support,
        ts_utility_syntax,
        type_expansion_recursion_limit,
        unsuppressable_error_codes,
        use_unknown_in_catch_variables,
        verbose: verbose_override.map(Arc::new),
        vpn_less,
        wait_for_recheck: wait_for_recheck_override.unwrap_or(wait_for_recheck),
    }
}

fn make_env<'a>(
    flowconfig: &FlowConfig,
    flowconfig_name: &'a str,
    connect_flags: &'a ConnectParams,
    root: &'a std::path::Path,
    tmp_dir: &'a str,
) -> crate::command_connect::Env<'a> {
    let retries = connect_flags.retries;
    let expiry = connect_flags
        .timeout
        .map(|n| std::time::Instant::now() + std::time::Duration::from_secs(n as u64));
    let rerun_on_mismatch = match connect_flags.on_mismatch {
        OnMismatchBehavior::ChooseNewest | OnMismatchBehavior::RestartClient => true,
        OnMismatchBehavior::StopServer | OnMismatchBehavior::ErrorClient => false,
    };
    let lazy_mode: Option<&'static str> = connect_flags.lazy_mode.map(|lm| match lm {
        LazyMode::Lazy => "true",
        LazyMode::NonLazy => "false",
        LazyMode::WatchmanDeprecated => "watchman",
    });
    crate::command_connect::Env {
        root,
        autostart: !connect_flags.no_auto_start,
        lazy_mode,
        retries,
        expiry,
        autostop: connect_flags.autostop,
        tmp_dir,
        shm_hash_table_pow: connect_flags.shm_flags.shm_hash_table_pow,
        ignore_version: connect_flags.ignore_version,
        emoji: flowconfig.options.emoji.unwrap_or(false),
        quiet: connect_flags.quiet,
        flowconfig_name,
        rerun_on_mismatch,
    }
}

fn search_for_root(
    flowconfig_name: &str,
    start: std::path::PathBuf,
    recursion_limit: i32,
) -> Option<std::path::PathBuf> {
    let parent = start.parent().map(Path::to_path_buf);
    match parent {
        None => None,
        // Reach fs root, nothing to do.
        Some(parent) if start == parent => None,
        Some(parent) => {
            if start.join(flowconfig_name).exists() {
                Some(start)
            } else if recursion_limit <= 0 {
                None
            } else {
                search_for_root(flowconfig_name, parent, recursion_limit - 1)
            }
        }
    }
}

pub(super) fn guess_root(flowconfig_name: &str, dir_or_file: Option<&str>) -> std::path::PathBuf {
    // Given a valid file or directory, find a valid Flow root directory
    // NOTE: exits on invalid file or .flowconfig not found!
    let dir_or_file = dir_or_file.unwrap_or(".");
    let path = Path::new(dir_or_file);
    if !path.exists() {
        let msg = format!(
            "Could not find file or directory {}; canceling search for {}.\nSee \"flow init --help\" for more info",
            dir_or_file, flowconfig_name
        );
        flow_common_exit::exit(FlowExitStatus::CouldNotFindFlowconfig, Some(&msg));
    }
    let dir = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(|p| {
                if p.as_os_str().is_empty() {
                    Path::new(".").to_path_buf()
                } else {
                    p.to_path_buf()
                }
            })
            .unwrap_or_else(|| Path::new(".").to_path_buf())
    };
    let resolved = flow_common::files::cached_canonicalize(&dir).unwrap_or_else(|_| dir.clone());
    match search_for_root(flowconfig_name, resolved, 50) {
        Some(root) => {
            flow_event_logger::set_root(Some(root.to_string_lossy().to_string()));
            root
        }
        None => {
            let msg = format!(
                "Could not find a {} in {} or any of its parent directories.\nSee \"flow init --help\" for more info\n",
                flowconfig_name,
                dir.display()
            );
            flow_common_exit::exit(FlowExitStatus::CouldNotFindFlowconfig, Some(&msg));
        }
    }
}

pub(crate) fn find_a_root(
    flowconfig_name: &str,
    root: Option<&str>,
    input: Option<&FileInput>,
) -> std::path::PathBuf {
    // Favor the root argument, over the input file, over the current directory
    // as the place to begin searching for the root.
    guess_root(
        flowconfig_name,
        match (root, input) {
            (Some(provided_root), _) => Some(provided_root),
            (None, Some(provided_input)) => provided_input.path_of_file_input(),
            (None, None) => None,
        },
    )
}

pub(crate) fn get_the_root(
    flowconfig_name: &str,
    root: Option<&str>,
    input: Option<&FileInput>,
) -> std::path::PathBuf {
    // If a root is given then validate it and use it. Otherwise, favor the input file
    // over the current directory as the place to begin searching for the root.
    match root {
        Some(provided_root) => {
            let root_dir = Path::new(provided_root);
            if root_dir.exists() && root_dir.is_dir() {
                let root_dir = expand_path(provided_root);
                let root_config = Path::new(&root_dir).join(flowconfig_name);
                if root_config.exists() {
                    Path::new(&root_dir).to_path_buf()
                } else {
                    let msg = format!("Failed to open {}", root_config.display());
                    flow_common_exit::exit(FlowExitStatus::CouldNotFindFlowconfig, Some(&msg));
                }
            } else {
                let msg = format!("Invalid root directory {}", provided_root);
                flow_common_exit::exit(FlowExitStatus::CouldNotFindFlowconfig, Some(&msg));
            }
        }
        None => guess_root(
            flowconfig_name,
            input.and_then(FileInput::path_of_file_input),
        ),
    }
}

pub(crate) fn convert_input_pos(line: i32, column: i32) -> (i32, i32) {
    // convert 1,1 based line/column to 1,0 for internal use
    let column = if column > 1 { column - 1 } else { 0 };
    (line, column)
}

pub(super) fn get_path_of_file(file: &str) -> String {
    // copied (and adapted) from Hack's ClientCheck module
    if Path::new(file).exists() {
        expand_path(file)
    } else {
        // Filename.concat does not return a normalized path when the file does
        // not exist. Thus, we do it on our own...
        let cwd = std::env::current_dir()
            .expect("failed to get current directory")
            .to_string_lossy()
            .to_string();
        flow_common::files::normalize_path(&cwd, file)
    }
}

pub(crate) fn get_file_from_filename_or_stdin(
    cmd: &str,
    filename: Option<&str>,
    path: Option<&str>,
) -> FileInput {
    match filename {
        Some(filename) => {
            let file_path = Path::new(filename);
            if !file_path.exists() {
                let msg = format!(
                    "Could not find file {}; canceling.\nSee \"flow {} --help\" for more info",
                    filename, cmd
                );
                flow_common_exit::exit(FlowExitStatus::NoInput, Some(&msg));
            } else if file_path.is_dir() {
                let msg = format!(
                    "Provided argument {} is not a file; canceling.\nSee \"flow {} --help\" for more info",
                    filename, cmd
                );
                flow_common_exit::exit(FlowExitStatus::PathIsNotAFile, Some(&msg));
            } else {
                FileInput::FileName(expand_path(filename))
            }
        }
        None => {
            let mut content = String::new();
            std::io::stdin()
                .lock()
                .read_to_string(&mut content)
                .expect("failed to read stdin");
            let filename = match path {
                Some("") | None => None,
                Some(path) => Some(get_path_of_file(path)),
            };
            FileInput::FileContent(filename, content)
        }
    }
}

pub(crate) fn parse_location_with_optional_filename(
    spec: &Command,
    path: Option<&str>,
    args: &[String],
) -> (FileInput, i32, i32) {
    // Takes a list of strings. If there are 2 then they are both parsed as intengers
    // and stdin is read from. If there are 3 then the first is treated as a input file
    // and the following 2 are parsed as integers.
    let exit = || -> ! {
        let msg = spec.string_of_usage();
        flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(&msg));
    };
    let (file, line, column) = match args {
        [file, line, column] => (FileInput::FileName(expand_path(file)), line, column),
        [line, column] => (
            get_file_from_filename_or_stdin(spec.name(), None, path),
            line,
            column,
        ),
        _ => exit(),
    };
    let (line, column) = match (line.parse::<i32>(), column.parse::<i32>()) {
        (Ok(line), Ok(column)) => (line, column),
        _ => exit(),
    };
    let (line, column) = convert_input_pos(line, column);
    (file, line, column)
}

pub(crate) fn exe_name() -> String {
    std::env::args()
        .next()
        .and_then(|arg| {
            std::path::Path::new(&arg)
                .file_name()
                .map(|name| name.to_string_lossy().to_string())
        })
        .unwrap_or_else(|| "flow".to_string())
}

fn connect_and_make_request_inner(
    flowconfig_name: &str,
    connect_flags: &ConnectParams,
    root: &std::path::Path,
    request: &server_prot::request::Command,
) -> server_prot::response::Response {
    // What should we do when we connect to the flow server monitor, but it dies before responding to
    // us? Well, we should consume a retry and try to connect again, potentially even starting a new
    // server
    let path = flow_server_files::server_files_js::config_file(flowconfig_name, root);
    // let the server enforce flowconfig warnings; we only care about the flowconfig
    // as far as it pertains to connecting to the server.
    let enforce_warnings = false;
    let flowconfig = read_config_or_exit(&path, enforce_warnings);

    // connect handles timeouts itself
    let tmp_dir_string = get_temp_dir(&connect_flags.temp_dir);
    let tmp_dir_normalized = std::path::Path::new(&tmp_dir_string)
        .canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(&tmp_dir_string));
    let tmp_dir_str = tmp_dir_normalized.to_string_lossy();
    let env = make_env(
        &flowconfig,
        flowconfig_name,
        connect_flags,
        root,
        &tmp_dir_str,
    );

    let logging_context = flow_event_logger::get_context();
    let client_logging_context = flow_monitor_rpc::lsp_prot::LoggingContext {
        from: logging_context.from,
        agent_id: logging_context.agent_id,
    };
    let command = flow_monitor_rpc::server_command_with_context::ServerCommandWithContext {
        client_logging_context,
        command: request.clone(),
    };
    let server_response = crate::command_connect::connect(
        &env,
        &crate::command_connect_simple::ConnectRequest::Command(command),
    );
    match server_response {
        crate::command_connect_simple::ConnectResponse::Data(response) => response,
        crate::command_connect_simple::ConnectResponse::ServerException(message) => {
            let msg = format!("Server threw an exception: {}", message);
            flow_common_exit_status::exit_with_msg(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                &msg,
            )
        }
        crate::command_connect_simple::ConnectResponse::ShutdownAck => {
            flow_common_exit_status::exit_with_msg(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "Unexpected ShutdownAck response for non-shutdown request",
            )
        }
    }
}

pub(crate) fn connect_and_make_request(
    flowconfig_name: &str,
    connect_flags: &ConnectParams,
    root: &std::path::Path,
    request: &server_prot::request::Command,
) -> server_prot::response::Response {
    // If --timeout is set, wrap connect_and_make_request in a timeout
    // Set File_key root paths for this client process. Server responses
    // contain File_key.t values with relative suffixes; to_string needs
    // the roots to reconstruct absolute paths.
    flow_parser::file_key::set_project_root(&root.to_string_lossy());
    let temp_dir = std::path::PathBuf::from(get_temp_dir(&connect_flags.temp_dir));
    match flow_flowlib::libdir(false, &temp_dir) {
        flow_flowlib::LibDir::Prelude(ref path) => {
            flow_parser::file_key::set_flowlib_root(&path.to_string_lossy());
        }
        flow_flowlib::LibDir::Flowlib(ref path) => {
            flow_parser::file_key::set_flowlib_root(&path.to_string_lossy());
        }
    }
    connect_and_make_request_inner(flowconfig_name, connect_flags, root, request)
}

pub(crate) fn failwith_bad_response(
    request: &server_prot::request::Command,
    response: &server_prot::response::Response,
) -> ! {
    let msg = format!(
        "Bad response to {:?}: received {:?}",
        server_prot::request::to_string(request),
        server_prot::response::to_string(response),
    );
    flow_common_exit_status::exit_with_msg(
        flow_common_exit_status::FlowExitStatus::UnknownError,
        &msg,
    )
}

pub(super) fn get_check_or_status_exit_code(
    errors: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    warnings: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    max_warnings: Option<i32>,
) -> FlowExitStatus {
    if errors.is_empty() {
        match max_warnings {
            Some(x) if warnings.cardinal() as i32 > x => FlowExitStatus::TypeError,
            None | Some(_) => FlowExitStatus::NoError,
        }
    } else {
        FlowExitStatus::TypeError
    }
}

pub(crate) fn choose_file_watcher(
    flowconfig: &FlowConfig,
    lazy_mode: Option<LazyMode>,
    file_watcher: Option<flow_config::FileWatcher>,
    file_watcher_debug: bool,
    sync_timeout: Option<u32>,
) -> FlowServerMonitorOptions::FileWatcher {
    let file_watcher = match (file_watcher, lazy_mode) {
        (None, Some(LazyMode::WatchmanDeprecated)) => flow_config::FileWatcher::Watchman,
        (Some(file_watcher), _) => file_watcher,
        (None, _) => flowconfig
            .options
            .file_watcher
            .unwrap_or(flow_config::FileWatcher::DFind),
    };
    match file_watcher {
        flow_config::FileWatcher::NoFileWatcher => FlowServerMonitorOptions::NoFileWatcher,
        flow_config::FileWatcher::DFind => FlowServerMonitorOptions::DFind,
        flow_config::FileWatcher::Watchman => {
            let sync_timeout = sync_timeout.or(flowconfig.options.watchman_sync_timeout);
            let defer_states = flowconfig.options.watchman_defer_states.clone();
            FlowServerMonitorOptions::Watchman(FlowServerMonitorOptions::WatchmanOptions {
                debug: file_watcher_debug,
                defer_states,
                sync_timeout,
            })
        }
        flow_config::FileWatcher::EdenFS => {
            let sync_timeout = sync_timeout.or(flowconfig.options.watchman_sync_timeout);
            let defer_states = flowconfig.options.watchman_defer_states.clone();
            let watchman_fallback = FlowServerMonitorOptions::WatchmanOptions {
                debug: file_watcher_debug,
                defer_states: defer_states.clone(),
                sync_timeout,
            };
            // Watchman natively defers during hg operations via Defer_changes subscribe mode.
            // EdenFS requires explicit state names, so we hardcode the hg states here.
            let mut edenfs_defer_states =
                vec!["hg.update".to_string(), "hg.transaction".to_string()];
            edenfs_defer_states.extend(defer_states);
            FlowServerMonitorOptions::EdenFS(FlowServerMonitorOptions::EdenfsOptions {
                edenfs_debug: file_watcher_debug,
                edenfs_timeout_secs: flowconfig.options.file_watcher_edenfs_timeout,
                edenfs_throttle_time_ms: flowconfig.options.file_watcher_edenfs_throttle_time_ms,
                edenfs_defer_states,
                edenfs_watchman_fallback: watchman_fallback,
            })
        }
    }
}

pub(crate) fn choose_file_watcher_mergebase_with(
    flowconfig: &FlowConfig,
    vcs: Option<Vcs>,
    mergebase_with: Option<&str>,
) -> String {
    match mergebase_with {
        Some(x) => x.to_string(),
        None => {
            let mergebase_with = match vcs {
                Some(Vcs::Git) => flowconfig.options.file_watcher_mergebase_with_git.clone(),
                Some(Vcs::Hg) => flowconfig.options.file_watcher_mergebase_with_hg.clone(),
                None => None,
            };
            let mergebase_with =
                mergebase_with.or_else(|| flowconfig.options.file_watcher_mergebase_with.clone());
            match mergebase_with {
                Some(x) => x,
                None => DEFAULT_FILE_WATCHER_MERGEBASE_WITH.to_string(),
            }
        }
    }
}

pub(crate) fn choose_file_watcher_timeout(
    flowconfig: &FlowConfig,
    cli_timeout: Option<u32>,
) -> Option<f64> {
    match cli_timeout.or(flowconfig.options.file_watcher_timeout) {
        Some(0) => None,
        Some(x) => Some(f64::from(x)),
        None => Some(f64::from(DEFAULT_FILE_WATCHER_TIMEOUT)),
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CodemodParams {
    pub(crate) options_flags: OptionsFlags,
    pub(crate) saved_state_options_flags: SavedStateFlags,
    pub(crate) shm_flags: SharedMemParams,
    pub(crate) ignore_version: bool,
    pub(crate) write: bool,
    pub(crate) repeat: bool,
    pub(crate) log_level: Option<LevelFilter>,
    pub(crate) root: Option<String>,
    pub(crate) input_file: Option<String>,
    pub(crate) base_flag: BaseFlags,
    pub(crate) anon: Option<Vec<String>>,
}

pub(crate) fn get_codemod_flags(args: &arg_spec::Values) -> CodemodParams {
    let options_flags = get_options_flags(args);
    let saved_state_options_flags = get_saved_state_flags(args);
    let shm_flags = get_shm_flags(args);
    let ignore_version = command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap();
    let write = command_spec::get(args, "--write", &arg_spec::truthy()).unwrap();
    let repeat = command_spec::get(args, "--repeat", &arg_spec::truthy()).unwrap();
    let log_level =
        command_spec::get(args, "--log-level", &arg_spec::optional(log_level_flag())).unwrap();
    let root = command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let base_flag = get_base_flags(args);
    let anon = command_spec::get(args, "FILE...", &arg_spec::list_of(arg_spec::string())).unwrap();
    let anon = anon.filter(|v| !v.is_empty());

    if !write && repeat {
        let msg = "Error: cannot run codemod with --repeat flag unless --write is also passed";
        flow_common_exit::exit(FlowExitStatus::CommandlineUsageError, Some(msg));
    }

    CodemodParams {
        options_flags,
        saved_state_options_flags,
        shm_flags,
        ignore_version,
        write,
        repeat,
        log_level,
        root,
        input_file,
        base_flag,
        anon,
    }
}

fn log_level_flag() -> arg_spec::FlagType<Option<LevelFilter>> {
    arg_spec::enum_flag(vec![
        ("off", LevelFilter::OFF),
        ("fatal", LevelFilter::ERROR),
        ("error", LevelFilter::ERROR),
        ("warn", LevelFilter::WARN),
        ("info", LevelFilter::INFO),
        ("debug", LevelFilter::DEBUG),
    ])
}

pub(crate) fn add_codemod_flags(spec: command_spec::Spec) -> command_spec::Spec {
    let spec = add_options_flags(spec);
    let spec = add_saved_state_flags(spec);
    let spec = add_shm_flags(spec);
    let spec = add_from_flag(spec);
    let spec = add_ignore_version_flag(spec);
    let spec = spec
        .flag("--write", &arg_spec::truthy(), "Edit files in place", None)
        .flag(
            "--repeat",
            &arg_spec::truthy(),
            "Run this codemod repeatedly until no more files change",
            None,
        )
        .flag(
            "--log-level",
            &arg_spec::optional(log_level_flag()),
            "Verbosity of logging (off|fatal|error|warn|info|debug)",
            Some("FLOW_LOG_LEVEL"),
        );
    let spec = add_root_flag(spec);
    let spec = add_input_file_flag(
        spec,
        "File containing a list of paths to transform. Incompatible with stdin and FILE...",
    );
    let spec = add_base_flags(spec);
    spec.anon("FILE...", &arg_spec::list_of(arg_spec::string()))
}

pub(crate) fn subcommand_spec<T: Clone + Send + Sync + 'static>(
    name: &str,
    doc: &str,
    cmd_list: Vec<(&'static str, T, String)>,
) -> command_spec::Spec {
    let mut command_info = cmd_list
        .iter()
        .filter(|(cmd, _, subdoc)| !cmd.is_empty() && !subdoc.is_empty())
        .map(|(cmd, _, subdoc)| (cmd.to_string(), subdoc.clone()))
        .collect::<Vec<_>>();
    command_info.sort_by(|(a, _), (b, _)| a.cmp(b));
    let command_info = command_spec::format_two_columns(None, None, 1, &command_info);
    command_spec::Spec::new(
        name,
        doc,
        format!(
            "Usage: {} {} SUBCOMMAND [OPTIONS]...\n\nValid values for SUBCOMMAND:\n{}\n",
            exe_name(),
            name,
            command_info,
        ),
    )
    .anon(
        "subcommand",
        &arg_spec::required(
            None,
            arg_spec::command_flag(
                cmd_list
                    .into_iter()
                    .map(|(cmd, subcommand, _)| (cmd, subcommand))
                    .collect(),
            ),
        ),
    )
}
