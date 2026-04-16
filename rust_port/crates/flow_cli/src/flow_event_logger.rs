/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::SystemTime;

#[derive(Clone, Debug, Default)]
struct LoggingContext {
    from: Option<String>,
    agent_id: Option<String>,
    command: Option<String>,
    root: Option<String>,
    init_id: Option<String>,
    start_time_unix_secs: Option<f64>,
}

fn context() -> &'static Mutex<LoggingContext> {
    static CONTEXT: OnceLock<Mutex<LoggingContext>> = OnceLock::new();
    CONTEXT.get_or_init(|| Mutex::new(LoggingContext::default()))
}

pub(crate) fn get_from_i_am_a_clown() -> Option<String> {
    context().lock().unwrap().from.clone()
}

pub(crate) fn set_command(command: Option<String>) {
    context().lock().unwrap().command = command;
}

pub(crate) fn init_flow_command(init_id: &str) {
    let start_time_unix_secs = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    let snapshot = {
        let mut context = context().lock().unwrap();
        context.init_id = Some(init_id.to_owned());
        context.start_time_unix_secs = Some(start_time_unix_secs);
        context.clone()
    };
    tracing::info!(
        init_id = init_id,
        command = snapshot.command.as_deref().unwrap_or(""),
        from = snapshot.from.as_deref().unwrap_or(""),
        agent_id = snapshot.agent_id.as_deref().unwrap_or(""),
        root = snapshot.root.as_deref().unwrap_or(""),
        start_time_unix_secs,
        "flow command init"
    );
}

pub(crate) fn set_root(root: Option<String>) {
    context().lock().unwrap().root = root;
}

#[allow(dead_code)]
pub(crate) fn set_from(from: Option<String>) {
    context().lock().unwrap().from = from;
}

#[allow(dead_code)]
pub(crate) fn set_agent_id(agent_id: Option<String>) {
    context().lock().unwrap().agent_id = agent_id;
}
