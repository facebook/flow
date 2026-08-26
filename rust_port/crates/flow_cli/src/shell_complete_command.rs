/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;

fn spec() -> flow_command_spec::Spec {
    let spec = flow_command_spec::Spec::new(
        "shell-complete",
        "Shell completion helper",
        flow_command_spec::Visibility::Internal,
        format!(
            "Usage: {} shell-complete --current N -- ARGV\n",
            flow_command_utils::exe_name()
        ),
    );
    let spec = flow_command_utils::add_from_flag(spec);
    spec.flag(
        "--current",
        &arg_spec::optional(arg_spec::int()),
        "Current term in the argument list being completed.",
        None,
    )
    .anon("argv", &arg_spec::rest())
}

fn is_partial_flag(substr: &str) -> bool {
    substr.starts_with('-')
}

fn find_flag(key: &str, command: &flow_command_spec::Command) -> Option<arg_spec::FlagArgCount> {
    if !is_partial_flag(key) {
        None
    } else {
        flow_command_spec::find_flag(key, command.flags()).map(|(_, metadata)| metadata.arg_count)
    }
}

fn get_completion(command: &flow_command_spec::Command, current: usize, rest: &[String]) -> String {
    let flags = command.flags();
    let prev = &rest[current - 1];
    match find_flag(prev, command) {
        Some(arg_spec::FlagArgCount::Truthy) | None => {
            if current < rest.len() && is_partial_flag(&rest[current]) {
                flags.keys().cloned().collect::<Vec<_>>().join(" ")
            } else {
                "FILE".to_string()
            }
        }
        _ => "ARGUMENT".to_string(),
    }
}

fn main(args: &arg_spec::Values) {
    let current = flow_command_spec::get(args, "--current", &arg_spec::optional(arg_spec::int()))
        .unwrap()
        .unwrap_or(0);
    let rest = flow_command_spec::get(args, "argv", &arg_spec::rest()).unwrap();
    let commands = crate::all_commands();

    if current <= 1 {
        println!(
            "{}",
            commands
                .iter()
                .map(|command| command.name().to_string())
                .collect::<Vec<_>>()
                .join(" ")
        );
    } else {
        let cmdstr = rest[1].to_ascii_lowercase();
        if let Some(command) = commands
            .into_iter()
            .find(|command| command.name() == cmdstr)
        {
            println!("{}", get_completion(&command, current as usize, &rest));
        }
    }
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(spec(), main)
}
