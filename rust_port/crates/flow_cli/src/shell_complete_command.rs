/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "shell-complete",
        "Shell completion helper",
        command_spec::Visibility::Internal,
        format!(
            "Usage: {} shell-complete --current N -- ARGV\n",
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_from_flag(spec);
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

fn find_flag(key: &str, command: &command_spec::Command) -> Option<arg_spec::FlagArgCount> {
    if !is_partial_flag(key) {
        None
    } else {
        command_spec::find_flag(key, command.flags()).map(|(_, metadata)| metadata.arg_count)
    }
}

fn get_completion(command: &command_spec::Command, current: usize, rest: &[String]) -> String {
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
    let current = command_spec::get(args, "--current", &arg_spec::optional(arg_spec::int()))
        .unwrap()
        .unwrap_or(0);
    let rest = command_spec::get(args, "argv", &arg_spec::rest()).unwrap();
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

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
