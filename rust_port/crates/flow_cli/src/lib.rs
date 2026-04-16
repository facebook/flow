/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(box_patterns)]
#![feature(never_type)]

use std::path::Path;
use std::sync::Arc;

use flow_common::options::Options;

mod apply_code_action_command;
mod ast_command;
mod autocomplete_command;
mod autofix_command;
mod batch_coverage_command;
mod check_contents_command;
mod codemod_command;
mod command_connect;
mod command_connect_simple;
mod command_mean_kill;
mod command_spec;
mod command_utils;
mod config_command;
mod coverage_command;
mod cycle_command;
mod daemon;
mod dump_impl_deps_command;
mod dump_types_command;
mod env_builder_debug_command;
mod extra_commands;
mod find_module_command;
mod flow_event_logger;
mod force_recheck_command;
mod foreground_check_commands;
mod get_def_command;
mod glean_command;
mod glean_runner;
mod glean_schema;
mod graph_command;
mod init_command;
mod inlay_hint_command;
mod llm_context_command;
mod ls_command;
mod lsp_command;
mod offset_cache;
mod save_state_command;
mod server_command;
mod shell_complete_command;
mod start_command;
mod status_command;
mod stop_command;
mod type_at_pos_command;
mod type_of_name_command;
mod version_command;

pub(crate) fn get_options_with_root_and_flowconfig_name(
    no_flowlib: bool,
    ignore_version: bool,
    root: &Path,
    flowconfig_name: &str,
    overrides: command_utils::MakeOptionsOverrides,
) -> Arc<Options> {
    let flowconfig_path = root.join(flowconfig_name);
    let flowconfig_path = flowconfig_path.to_string_lossy().to_string();
    let (flowconfig, flowconfig_hash) =
        command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
    let temp_dir = command_utils::get_temp_dir(&overrides.temp_dir);
    Arc::new(command_utils::make_options(
        flowconfig,
        flowconfig_hash,
        flowconfig_name.to_string(),
        root.to_path_buf(),
        temp_dir,
        no_flowlib,
        overrides,
    ))
}

fn explicit_commands() -> Vec<command_spec::Command> {
    vec![
        apply_code_action_command::command(),
        ast_command::command(),
        autocomplete_command::command(),
        autofix_command::command(),
        batch_coverage_command::command(),
        foreground_check_commands::full_check_command(),
        foreground_check_commands::focus_check_command(),
        check_contents_command::command(),
        codemod_command::command(),
        config_command::command(),
        coverage_command::command(),
        cycle_command::command(),
        dump_impl_deps_command::command(),
        dump_types_command::command(),
        env_builder_debug_command::command(),
        find_module_command::command(),
        force_recheck_command::command(),
        get_def_command::command(),
        glean_command::command(),
        graph_command::command(),
        init_command::command(),
        inlay_hint_command::command(),
        llm_context_command::command(),
        lsp_command::command(),
        ls_command::command(),
        save_state_command::command(),
        server_command::command(),
        start_command::command(),
        stop_command::command(),
        type_at_pos_command::command(),
        type_of_name_command::command(),
        version_command::command(),
    ]
}

fn registered_commands() -> Vec<command_spec::Command> {
    let explicit = explicit_commands();
    let extra = extra_commands::extra_commands();
    let mut commands = Vec::with_capacity(explicit.len() + extra.len());
    commands.extend(explicit);
    commands.extend(extra);
    commands
}

fn all_commands() -> Vec<command_spec::Command> {
    let registered = registered_commands();
    let mut commands = Vec::with_capacity(registered.len() + 4);
    commands.push(shell_complete_command::command());
    commands.push(status_command::default_command(default_command_docs()));
    commands.push(status_command::check_command());
    commands.push(status_command::status_command());
    commands.extend(registered);
    commands
}

fn find_command(subcmd: &str) -> Option<command_spec::Command> {
    all_commands()
        .into_iter()
        .find(|command| command.name() == subcmd)
}

pub(crate) fn random_id_short_string() -> String {
    use std::time::SystemTime;

    let nanos = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{:x}", nanos)
}

fn startup_max_workers(arguments: &[String]) -> Option<std::num::NonZeroUsize> {
    let (subcmd, rest) = arguments.split_first()?;
    let subcmd = subcmd.to_ascii_lowercase();
    if subcmd != "start" && subcmd != "server" {
        return None;
    }
    let mut rest = rest.iter();
    while let Some(arg) = rest.next() {
        if let Some(value) = arg.strip_prefix("--max-workers=") {
            return value
                .parse::<usize>()
                .ok()
                .and_then(std::num::NonZeroUsize::new);
        }
        if arg == "--max-workers" {
            return rest
                .next()
                .and_then(|value| value.parse::<usize>().ok())
                .and_then(std::num::NonZeroUsize::new);
        }
    }
    None
}

fn flow_shell_main() {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();

    let max_workers = startup_max_workers(&arguments).or_else(|| {
        std::env::var("FLOW_MAX_WORKERS")
            .ok()
            .and_then(|workers_str| workers_str.parse::<std::num::NonZeroUsize>().ok())
    });
    if let Some(count) = max_workers {
        flow_utils_concurrency::thread_pool::init_thread_pool(
            flow_utils_concurrency::thread_pool::ThreadCount::NumThreads(count),
        );
    }

    let default_command = status_command::default_command(default_command_docs());
    let (command, argv) = match arguments.split_first() {
        None => (default_command, vec![]),
        Some((subcmd, rest)) => {
            let subcmd = subcmd.to_ascii_lowercase();
            match find_command(&subcmd) {
                Some(command) => (command, rest.to_vec()),
                None => (default_command, arguments),
            }
        }
    };
    let command_string = command.name().to_string();
    flow_event_logger::set_command(Some(command_string));
    let init_id = random_id_short_string();
    flow_event_logger::init_flow_command(&init_id);
    command_utils::run_command(&command, &argv);
}

pub fn main() {
    #[cfg(unix)]
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_IGN);
    }

    daemon::check_entry_point();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(flow_shell_main));
    match result {
        Ok(()) => {
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
        }
        Err(err) => {
            let err_msg = if let Some(s) = err.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = err.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic".to_string()
            };
            if err_msg.contains("Out_of_shared_memory") || err_msg.contains("Out of shared memory")
            {
                eprintln!("Out of shared memory");
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::OutOfSharedMemory,
                );
            } else {
                eprintln!("Unhandled exception: {}", err_msg);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::UnknownError,
                );
            }
        }
    }
}

fn default_command_docs() -> Vec<(String, String)> {
    let mut commands = Vec::with_capacity(registered_commands().len() + 2);
    commands.push(status_command::check_command());
    commands.push(status_command::status_command());
    commands.extend(registered_commands());
    commands
        .into_iter()
        .filter_map(|command| {
            let name = command.name().to_string();
            let doc = command.doc().to_string();
            if name.is_empty() || doc.is_empty() {
                None
            } else {
                Some((name, doc))
            }
        })
        .collect()
}
