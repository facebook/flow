/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_lsp::flow_lsp;
use flow_server_env::lsp_connect_params::ConnectParams;
use flow_server_env::lsp_connect_params::OnMismatchBehavior;
use flow_server_env::lsp_connect_params::SharedMemParams;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "lsp",
        "Acts as a server for the Language Server Protocol over stdin/stdout [experimental]",
        format!("Usage: {exe_name} lsp\n\nRuns a server for the Language Server Protocol\n"),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_temp_dir_flag(spec);
    let spec = command_utils::add_shm_flags(spec);
    let spec = spec
        .flag(
            "--lazy",
            &arg_spec::truthy(),
            "Deprecated, has no effect",
            None,
        )
        .flag(
            "--lazy-mode",
            &arg_spec::optional(arg_spec::string()),
            "Deprecated, has no effect",
            None,
        );
    let spec = command_utils::add_autostop_flag(spec);
    command_utils::add_from_flag(spec)
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let temp_dir =
        command_spec::get(args, "--temp-dir", &arg_spec::optional(arg_spec::string())).unwrap();
    let shm_flags = command_utils::get_shm_flags(args);
    let _lazy = command_spec::get(args, "--lazy", &arg_spec::truthy()).unwrap();
    let _lazy_mode =
        command_spec::get(args, "--lazy-mode", &arg_spec::optional(arg_spec::string())).unwrap();
    let autostop = command_spec::get(args, "--autostop", &arg_spec::truthy()).unwrap();

    // always set `quiet`, since the LSP doesn't want any log spew. this only applies to the
    // `start` command and does not imply a quiet server, which will still write to its log
    // file.
    let quiet = true;
    let connect_params = ConnectParams {
        retries: 0,
        timeout: None,
        no_auto_start: false,
        autostop,
        from: flow_event_logger::get_from_i_am_a_clown(),
        lazy_mode: None,
        temp_dir,
        shm_flags: SharedMemParams {
            shm_heap_size: shm_flags.shm_heap_size,
            shm_hash_table_pow: shm_flags.shm_hash_table_pow,
        },
        ignore_version: false,
        quiet,
        on_mismatch: OnMismatchBehavior::ChooseNewest,
    };
    flow_lsp::run(&flowconfig_name, connect_params);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
