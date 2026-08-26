/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;
use flow_server_env::server_prot;

const CMD_NAME: &str = "llm-context-experimental";

// ***********************************************************************
// flow llm-context command
// ***********************************************************************

fn spec() -> flow_command_spec::Spec {
    let exe_name = flow_command_utils::exe_name();
    let spec = flow_command_spec::Spec::new(
        CMD_NAME,
        "Outputs LLM context information for given files",
        flow_command_spec::Visibility::Experimental,
        format!(
            "Usage: {exe_name} llm-context [OPTION]... [FILE]...\n\ne.g. {exe_name} llm-context foo.js bar.js\n"
        ),
    );
    let spec = flow_command_utils::add_base_flags(spec);
    let spec = flow_command_utils::add_connect_and_json_flags(spec);
    let spec = flow_command_utils::add_root_flag(spec);
    let spec = flow_command_utils::add_from_flag(spec);
    let spec = flow_command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
        "--token-budget",
        &arg_spec::required(Some(4000), arg_spec::int()),
        "Maximum token budget for context (default 4000)",
        None,
    )
    .anon("files", &arg_spec::list_of(arg_spec::string()))
}

fn handle_response(json: bool, pretty: bool, response: server_prot::response::llm_context::T) {
    let server_prot::response::llm_context::T {
        llm_context,
        files_processed,
        tokens_used,
        truncated,
    } = response;
    if json {
        flow_hh_json::print_json_endline(
            pretty,
            &serde_json::json!({
                "llmContext": llm_context,
                "filesProcessed": files_processed,
                "tokensUsed": tokens_used,
                "truncated": truncated,
            }),
        );
    } else {
        println!("{}", llm_context);
        println!("\n--- Stats ---");
        println!("Files processed: {}", files_processed.join(", "));
        println!("Tokens used: {}", tokens_used);
        if truncated {
            println!("Note: Output was truncated due to token budget");
        }
    }
}

fn handle_error(err: String, json: bool, pretty: bool) {
    if json {
        flow_hh_json::prerr_json_endline(
            pretty,
            &serde_json::json!({
                "error": err,
            }),
        );
    } else {
        eprintln!("{}", err);
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = flow_command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = flow_command_utils::get_connect_flags(args);
    let json_flags = flow_command_utils::get_json_flags(args);
    let root_arg =
        flow_command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = flow_command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let token_budget = flow_command_spec::get(
        args,
        "--token-budget",
        &arg_spec::required(Some(4000), arg_spec::int()),
    )
    .unwrap();
    let files = flow_command_spec::get(args, "files", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();
    if files.is_empty() {
        eprintln!("Error: At least one file must be specified");
        std::process::exit(1);
    }

    let root = match root_arg {
        Some(root) => std::path::PathBuf::from(root),
        None => match files.first() {
            Some(file) => flow_command_utils::find_a_root(
                &flowconfig_name,
                None,
                Some(&flow_server_utils::file_input::FileInput::FileName(
                    file.clone(),
                )),
            ),
            None => {
                eprintln!("Error: Could not determine root");
                std::process::exit(1);
            }
        },
    };
    let files = files
        .into_iter()
        .map(|file| {
            if std::path::Path::new(&file).is_relative() {
                let cwd = std::env::current_dir().unwrap();
                cwd.join(&file).to_string_lossy().into_owned()
            } else {
                file
            }
        })
        .collect::<Vec<_>>();
    let request = server_prot::request::Command::LLM_CONTEXT(server_prot::llm_context_options::T {
        files,
        token_budget,
        wait_for_recheck,
    });
    let json = json_flags.json || json_flags.pretty;
    let response = flow_command_utils::connect_and_make_request(
        &flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::LLM_CONTEXT(Err(err)) => {
            handle_error(err, json, json_flags.pretty);
        }
        server_prot::response::Response::LLM_CONTEXT(Ok(response)) => {
            handle_response(json, json_flags.pretty, response);
        }
        response => {
            flow_command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(spec(), main)
}
