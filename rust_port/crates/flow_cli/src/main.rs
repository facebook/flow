/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(never_type)]

#[allow(non_upper_case_globals)]
#[unsafe(no_mangle)]
#[used]
static malloc_conf: &str = "metadata_thp:always\0";

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::sync::RwLock;

use crossbeam::channel;
use dupe::Dupe;
use flow_common::files;
use flow_common::options::Options;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_parsing::parsing_service;
use flow_services_inference::dep_service;
use flow_services_inference::type_service;
use flow_utils_concurrency::map_reduce;
use flow_utils_concurrency::thread_pool;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

mod ast_command;
mod check_commands;
mod command_connect;
mod command_spec;
mod command_utils;
mod config_command;
mod env_builder_debug_command;
mod flow_server;

mod force_recheck_command;
mod ls_command;
mod server_command;
mod start_command;
mod status_command;
mod version_command;

fn semver_satisfies(range: &str, version: &str) -> bool {
    fn parse_version(s: &str) -> Option<(u64, u64, u64)> {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return None;
        }
        Some((
            parts[0].parse().ok()?,
            parts[1].parse().ok()?,
            parts[2].parse().ok()?,
        ))
    }
    let (major, minor, patch) = match parse_version(version) {
        Some(v) => v,
        None => return false,
    };
    if let Some(caret_range) = range.strip_prefix('^') {
        let (rmaj, rmin, rpatch) = match parse_version(caret_range) {
            Some(v) => v,
            None => return false,
        };
        if rmaj > 0 {
            major == rmaj && (minor > rmin || (minor == rmin && patch >= rpatch))
        } else if rmin > 0 {
            major == 0 && minor == rmin && patch >= rpatch
        } else {
            major == 0 && minor == 0 && patch == rpatch
        }
    } else {
        let (rmaj, rmin, rpatch) = match parse_version(range) {
            Some(v) => v,
            None => return false,
        };
        major == rmaj && minor == rmin && patch == rpatch
    }
}

pub(crate) fn get_options(cli_no_flowlib: bool, ignore_version: bool) -> Arc<Options> {
    get_options_with_root_and_flowconfig_name(
        cli_no_flowlib,
        ignore_version,
        Path::new("."),
        ".flowconfig",
        command_utils::MakeOptionsOverrides::default(),
    )
}

pub(crate) fn get_options_with_root_and_flowconfig_name(
    cli_no_flowlib: bool,
    ignore_version: bool,
    root: &Path,
    flowconfig_name: &str,
    overrides: command_utils::MakeOptionsOverrides,
) -> Arc<Options> {
    let flowconfig_path = root.join(flowconfig_name);
    let flowconfig_path_str = flowconfig_path.to_string_lossy();
    let (flowconfig, warnings, flowconfig_hash) = match flow_config::get(&flowconfig_path_str) {
        Ok(r) => r,
        Err(flow_config::Error(line, message)) => {
            eprintln!(".flowconfig:{} {}", line, message);
            std::process::exit(1);
        }
    };
    if !ignore_version && !warnings.is_empty() {
        for flow_config::Warning(line, message) in &warnings {
            eprintln!(".flowconfig:{} {}", line, message);
        }
        std::process::exit(1);
    }
    if !ignore_version {
        if let Some(ref version_constraint) = flowconfig.version {
            if !semver_satisfies(version_constraint, flow_common::flow_version::VERSION) {
                eprintln!(
                    "Wrong version of Flow. The config specifies version {} but this is version {}",
                    version_constraint,
                    flow_common::flow_version::VERSION,
                );
                std::process::exit(8);
            }
        }
    }
    let root = root.canonicalize().unwrap();
    let options = command_utils::make_options(
        flowconfig,
        flowconfig_hash,
        flowconfig_name.to_owned(),
        root,
        std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned()),
        cli_no_flowlib,
        overrides,
    );
    Arc::new(options)
}

fn dump_impl_deps(
    options: Arc<Options>,
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    root: &Path,
) {
    let (_ordered_libs, all_unordered_libs) =
        files::ordered_and_unordered_lib_paths(&options.file_options);
    let all_unordered_libs = Arc::new(all_unordered_libs);

    let file_opts = options.file_options.dupe();
    let root_buf = root.to_path_buf();

    let node_modules_containers: Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>> =
        Arc::new(RwLock::new(BTreeMap::new()));

    let (sender, receiver) = channel::unbounded::<Vec<FileKey>>();
    let receiver = Arc::new(receiver);

    let node_modules_containers_for_thread = node_modules_containers.clone();
    let all_libs_for_thread = all_unordered_libs.clone();
    let handle = std::thread::spawn(move || {
        type_service::make_next_files(
            &root_buf,
            file_opts,
            true,
            all_libs_for_thread,
            &node_modules_containers_for_thread,
            |files| {
                sender.send(files).unwrap();
            },
        );
        drop(sender);
    });
    let receiver_for_next = receiver.dupe();
    let next: parsing_service::Next = Box::new(move || receiver_for_next.recv().ok());

    let results = parsing_service::parse_with_defaults(pool, shared_mem, &options, &[], next);
    handle.join().unwrap();

    eprintln!("Parsed {} files", results.parsed.len());

    let dirty_modules_ordered: flow_common_modulename::ModulenameSet =
        results.dirty_modules.into_iter().collect();

    let (_changed_modules, _duplicate_providers) =
        flow_services_module::commit_modules(pool, &options, shared_mem, dirty_modules_ordered);

    let parsed_with_typed: FlowOrdSet<FileKey> = results
        .parsed
        .iter()
        .filter(|f| shared_mem.get_typed_parse(f).is_some())
        .map(|f| f.dupe())
        .collect();

    // Resolve requires stage
    let parsed_files: Vec<FileKey> = parsed_with_typed.iter().map(|f| f.dupe()).collect();
    let next = map_reduce::make_next(pool.num_workers(), None, parsed_files);

    let options_clone = options.dupe();
    let shared_mem_clone = shared_mem.dupe();
    let nmc_for_resolve = node_modules_containers.clone();
    map_reduce::iter(pool, next, move |batch| {
        for file in batch {
            if let Err(_e) = flow_services_module::add_parsed_resolved_requires(
                &options_clone,
                &shared_mem_clone,
                &nmc_for_resolve,
                &file,
            ) {}
        }
    });

    let dependency_info = dep_service::calc_dependency_info(pool, shared_mem, &parsed_with_typed);
    let implementation_dependency_graph = dependency_info.implementation_dependency_graph();

    let graph_map = implementation_dependency_graph.to_map();
    let json_map: BTreeMap<String, Vec<String>> = graph_map
        .into_iter()
        .map(|(k, v)| {
            let key = k.as_str().to_string();
            let mut deps: Vec<String> = v.into_iter().map(|f| f.as_str().to_string()).collect();
            deps.sort();
            (key, deps)
        })
        .collect();

    println!("{}", serde_json::to_string_pretty(&json_map).unwrap());
}

fn parse_dir(options: Arc<Options>, pool: &ThreadPool, shared_mem: &Arc<SharedMem>, root: &Path) {
    let (_ordered_libs, all_unordered_libs) =
        files::ordered_and_unordered_lib_paths(&options.file_options);
    let all_unordered_libs = Arc::new(all_unordered_libs);

    let file_opts = options.file_options.dupe();
    let root_buf = root.to_path_buf();

    let node_modules_containers: Arc<RwLock<BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>>> =
        Arc::new(RwLock::new(BTreeMap::new()));

    let (sender, receiver) = channel::unbounded::<Vec<FileKey>>();
    let receiver = Arc::new(receiver);

    let node_modules_containers_for_thread = node_modules_containers.clone();
    let all_libs_for_thread = all_unordered_libs.clone();
    let handle = std::thread::spawn(move || {
        type_service::make_next_files(
            &root_buf,
            file_opts,
            true,
            all_libs_for_thread,
            &node_modules_containers_for_thread,
            |files| {
                sender.send(files).unwrap();
            },
        );
        drop(sender);
    });
    let receiver_for_next = receiver.dupe();
    let next: parsing_service::Next = Box::new(move || receiver_for_next.recv().ok());

    let results = parsing_service::parse_with_defaults(pool, shared_mem, &options, &[], next);
    handle.join().unwrap();

    let num_parsed = results.parsed.len();
    let num_unparsed = results.unparsed.len();
    let num_not_found = results.not_found.len();
    let num_failed = results.failed.0.len();

    println!("=== RESULTS ===");
    println!("Successfully parsed: {}", num_parsed);
    println!("Skipped (not @flow): {}", num_unparsed);
    println!("Not found on disk:   {}", num_not_found);
    println!("Failed to parse:     {}", num_failed);

    if num_failed > 0 {
        println!("\n=== PARSE FAILURES ===");
        for (file, failure) in results.failed.0.iter().zip(results.failed.1.iter()) {
            match failure {
                parsing_service::ParseFailure::ParseError((loc, err)) => {
                    println!("  {} ({:?}): {}", file.as_str(), loc, err);
                }
                parsing_service::ParseFailure::DocblockErrors(errs) => {
                    println!("  {}: {} docblock error(s)", file.as_str(), errs.len());
                }
                parsing_service::ParseFailure::UncaughtException(msg) => {
                    println!("  {}: parser crash: {}", file.as_str(), msg);
                }
            }
        }
    }

    if num_failed == 0 {
        println!(
            "\nPASS: All {} files parsed with zero parse errors.",
            num_parsed
        );
    } else {
        println!("\nFAIL: {} files had parse errors.", num_failed);
        std::process::exit(1);
    }
}

#[derive(Clone)]
enum RootSubcommand {
    Version,
    Stop,
    Config,
    Ast,
    Ls,
    Check,
    FullCheck,
    FocusCheck,
    DumpImplDeps,
    ParseDir,
    EnvBuilderDebug,
    Server,
    Start,
    Status,
    ForceRecheck,
}

fn root_command() -> command_spec::Command {
    let spec = command_spec::Spec::new(
        "flow",
        "Flow CLI",
        "Usage: flow COMMAND [ARGS]...".to_string(),
    )
    .anon(
        "subcommand",
        &command_spec::required(
            None,
            command_spec::command_flag(vec![
                ("version", RootSubcommand::Version),
                ("stop", RootSubcommand::Stop),
                ("config", RootSubcommand::Config),
                ("ast", RootSubcommand::Ast),
                ("ls", RootSubcommand::Ls),
                ("check", RootSubcommand::Check),
                ("full-check", RootSubcommand::FullCheck),
                ("focus-check", RootSubcommand::FocusCheck),
                ("dump-impl-deps", RootSubcommand::DumpImplDeps),
                ("parse-dir", RootSubcommand::ParseDir),
                ("env-builder-debug", RootSubcommand::EnvBuilderDebug),
                ("server", RootSubcommand::Server),
                ("start", RootSubcommand::Start),
                ("status", RootSubcommand::Status),
                ("force-recheck", RootSubcommand::ForceRecheck),
            ]),
        ),
    );
    command_spec::command(spec, |args| {
        let (subcommand, argv) = command_spec::get(
            args,
            "subcommand",
            &command_spec::required(
                None,
                command_spec::command_flag(vec![
                    ("version", RootSubcommand::Version),
                    ("stop", RootSubcommand::Stop),
                    ("config", RootSubcommand::Config),
                    ("ast", RootSubcommand::Ast),
                    ("ls", RootSubcommand::Ls),
                    ("check", RootSubcommand::Check),
                    ("full-check", RootSubcommand::FullCheck),
                    ("focus-check", RootSubcommand::FocusCheck),
                    ("dump-impl-deps", RootSubcommand::DumpImplDeps),
                    ("parse-dir", RootSubcommand::ParseDir),
                    ("env-builder-debug", RootSubcommand::EnvBuilderDebug),
                    ("server", RootSubcommand::Server),
                    ("start", RootSubcommand::Start),
                    ("status", RootSubcommand::Status),
                    ("force-recheck", RootSubcommand::ForceRecheck),
                ]),
            ),
        )
        .unwrap();
        match subcommand {
            RootSubcommand::Version => {
                command_utils::run_command(&version_command::command(), &argv)
            }
            RootSubcommand::Stop => {
                // Try to connect to the server and send a shutdown request
                let root = if let Some(dir) = argv.first() {
                    command_utils::guess_root(".flowconfig", Some(dir))
                } else {
                    command_utils::guess_root(".flowconfig", None)
                };
                let tmp_dir =
                    std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned());
                let request = command_connect::ServerRequest::Shutdown;
                match command_connect::connect_and_make_request(
                    ".flowconfig",
                    &tmp_dir,
                    &root,
                    request,
                ) {
                    Ok(_) | Err(_) => {}
                }
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
            }
            RootSubcommand::Config => command_utils::run_command(&config_command::command(), &argv),
            RootSubcommand::Ast => command_utils::run_command(&ast_command::command(), &argv),
            RootSubcommand::Ls => command_utils::run_command(&ls_command::command(), &argv),
            RootSubcommand::Check => {
                command_utils::run_command(&check_commands::check_command(), &argv)
            }
            RootSubcommand::FullCheck => {
                command_utils::run_command(&check_commands::full_check_command(), &argv)
            }
            RootSubcommand::FocusCheck => {
                command_utils::run_command(&check_commands::focus_check_command(), &argv)
            }
            RootSubcommand::DumpImplDeps => {
                let root = Path::new(".").canonicalize().unwrap();
                let options = get_options(false, false);
                let shared_mem = Arc::new(SharedMem::new());
                let pool = ThreadPool::new();
                dump_impl_deps(options, &pool, &shared_mem, &root);
            }
            RootSubcommand::ParseDir => {
                let root = Path::new(".").canonicalize().unwrap();
                let options = get_options(false, false);
                let shared_mem = Arc::new(SharedMem::new());
                let pool = ThreadPool::new();
                parse_dir(options, &pool, &shared_mem, &root);
            }
            RootSubcommand::EnvBuilderDebug => {
                if argv.len() != 1 {
                    eprintln!("Usage: flow env-builder-debug FILE");
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
                    );
                }
                env_builder_debug_command::main(&argv[0]);
            }
            RootSubcommand::Server => command_utils::run_command(&server_command::command(), &argv),
            RootSubcommand::Start => command_utils::run_command(&start_command::command(), &argv),
            RootSubcommand::Status => command_utils::run_command(&status_command::command(), &argv),
            RootSubcommand::ForceRecheck => {
                command_utils::run_command(&force_recheck_command::command(), &argv)
            }
        }
    })
}

fn is_root_command(name: &str) -> bool {
    matches!(
        name,
        "version"
            | "stop"
            | "config"
            | "ast"
            | "ls"
            | "check"
            | "full-check"
            | "focus-check"
            | "dump-impl-deps"
            | "parse-dir"
            | "env-builder-debug"
            | "server"
            | "start"
            | "status"
            | "force-recheck"
    )
}

fn main() {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();

    if let Ok(workers_str) = std::env::var("FLOW_MAX_WORKERS") {
        if let Ok(count) = workers_str.parse::<std::num::NonZeroUsize>() {
            thread_pool::init_thread_pool(ThreadCount::NumThreads(count));
        }
    }

    if arguments
        .first()
        .is_some_and(|arg| arg.starts_with('-') || is_root_command(arg))
    {
        command_utils::run_command(&root_command(), &arguments);
        return;
    }

    for filename in &arguments {
        let content = fs::read_to_string(filename).unwrap();
        let json = flow_parser::parse_to_json(
            Some(flow_parser::PERMISSIVE_PARSE_OPTIONS),
            true,
            Ok(&content),
        );
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
    }
}
