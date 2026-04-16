/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Mutex;
use std::sync::OnceLock;

pub use flow_common_exit_status::FlowExitStatus;
pub use flow_common_exit_status::error_code;
pub use flow_common_exit_status::json_props_of_t;
pub use flow_common_exit_status::to_string;
use flow_hh_json::print_json_endline;

fn json_mode() -> &'static Mutex<Option<bool>> {
    static JSON_MODE: OnceLock<Mutex<Option<bool>>> = OnceLock::new();
    JSON_MODE.get_or_init(|| Mutex::new(None))
}

pub fn set_json_mode(pretty: bool) {
    *json_mode().lock().unwrap() = Some(pretty);
}

pub fn unset_json_mode() {
    *json_mode().lock().unwrap() = None;
}

pub fn exit(status: FlowExitStatus, msg: Option<&str>) -> ! {
    if let Some(msg) = msg {
        eprintln!("{}", msg);
    }
    if !matches!(status, FlowExitStatus::NoError | FlowExitStatus::TypeError) {
        if let Some(pretty) = *json_mode().lock().unwrap() {
            let json =
                serde_json::Value::Object(json_props_of_t(status, msg).into_iter().collect());
            print_json_endline(pretty, &json);
        }
    }
    flow_common_exit_status::exit(status)
}
