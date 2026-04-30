/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_common_exit_status::FlowExitStatus;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::file_key::FileKey;
use flow_server_env::monitor_rpc;
use flow_services_inference::type_service;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let spec = command_utils::add_profile_flag(command_utils::add_json_flags(
        command_spec::Spec::new(
            "unstable-dump-impl-deps",
            "Outputs the implementation dependency graph as JSON",
            command_spec::Visibility::Internal,
            format!(
                "Usage: {} dump-impl-deps [OPTION]... [ROOT]\n\nOutputs the implementation dependency graph as JSON to stdout.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n",
                command_utils::exe_name(),
            ),
        ),
    ));
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_shm_flags(spec);
    let spec = command_utils::add_ignore_version_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_no_cgroup_flag(spec);
    spec.anon("root", &arg_spec::optional(arg_spec::string()))
}

fn graph_to_json(
    graph: &std::collections::BTreeMap<FileKey, std::collections::BTreeSet<FileKey>>,
) -> serde_json::Value {
    let json_pairs: serde_json::Map<String, serde_json::Value> = graph
        .iter()
        .map(|(file, deps)| {
            let file_str = file.to_absolute();
            let mut deps_list: Vec<String> = deps.iter().map(|dep| dep.to_absolute()).collect();
            deps_list.sort();
            let deps_json = serde_json::Value::Array(
                deps_list
                    .into_iter()
                    .map(serde_json::Value::String)
                    .collect(),
            );
            (file_str, deps_json)
        })
        .collect();
    serde_json::Value::Object(json_pairs)
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let ignore_version = command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap();
    let profile = command_utils::get_profile_flag(args);
    let root_arg =
        command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let flowconfig_path = root.join(&flowconfig_name);
    let flowconfig_path = flowconfig_path.to_string_lossy().to_string();
    let (flowconfig, flowconfig_hash) =
        command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
    if !ignore_version {
        command_utils::assert_version(&flowconfig);
    }
    let options = Arc::new(command_utils::make_options(
        flowconfig,
        flowconfig_hash,
        flowconfig_name.clone(),
        root.clone(),
        std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned()),
        false,
        command_utils::MakeOptionsOverrides {
            lazy_mode: Some(flow_config::LazyMode::NonLazy),
            profile: Some(profile),
            saved_state_fetcher: Some(flow_common::options::SavedStateFetcher::DummyFetcher),
            ..Default::default()
        },
    ));
    flow_logging_utils::init_loggers(&options, Some(flow_hh_logger::Level::Error));
    // Disable logging to keep stdout clean for JSON output
    monitor_rpc::disable();
    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));
    let (env, libs_ok, _node_modules_containers) =
        type_service::init_from_scratch(&options, &pool, &shared_mem, &root);
    if !libs_ok {
        eprintln!("Library initialization failed");
        flow_common_exit_status::exit(FlowExitStatus::CouldNotFindFlowconfig);
    }

    // Get the implementation dependency graph
    let impl_graph = env.dependency_info.implementation_dependency_graph();
    let json = graph_to_json(&impl_graph.to_map());
    flow_hh_json::print_json_endline(true, &json);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
