/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_env::file_watcher_status;
use flow_server_env::lsp_connect_params::ConnectParams;
use flow_server_env::lsp_prot;
use flow_server_env::server_prot;
use flow_server_env::server_status;

use crate::flow_lsp::*;

fn addline(b: &mut String, prefix: &str, s: &str) {
    b.push_str(prefix);
    b.push_str(s);
    b.push('\n');
}

pub fn string_of_lazy_stats(lazy_stats: &server_prot::response::LazyStats) -> String {
    format!(
        "lazy_mode={}, checked_files={}, checked_libdef_files={}, total_files={}, total_libdef_files={}",
        lazy_stats.lazy_mode,
        lazy_stats.checked_files,
        lazy_stats.checked_libdef_files,
        lazy_stats.total_files,
        lazy_stats.total_libdef_files,
    )
}

pub fn string_of_lazy_mode(mode: &flow_config::LazyMode) -> &'static str {
    match mode {
        flow_config::LazyMode::Lazy => "true",
        flow_config::LazyMode::WatchmanDeprecated => "watchman",
        flow_config::LazyMode::NonLazy => "false",
    }
}

pub fn string_of_connect_params(p: &ConnectParams) -> String {
    format!(
        "retries={}, no_auto_start={}, autostop={}, ignore_version={} quiet={}, temp_dir={}, timeout={}, lazy_mode={}",
        p.retries,
        p.no_auto_start,
        p.autostop,
        p.ignore_version,
        p.quiet,
        p.temp_dir.as_deref().unwrap_or("None"),
        p.timeout
            .map(|t| t.to_string())
            .unwrap_or_else(|| "None".to_string()),
        p.lazy_mode
            .as_ref()
            .map(string_of_lazy_mode)
            .unwrap_or("None"),
    )
}

pub fn string_of_open_file(ofi: &OpenFileInfo) -> String {
    let OpenFileInfo {
        o_open_doc,
        o_ast,
        o_unsaved,
    } = ofi;
    format!(
        "(uri={} version={} text=[{} bytes] ast=[{}] unsaved={})",
        o_open_doc.uri,
        o_open_doc.version,
        o_open_doc.text.len(),
        if o_ast.is_some() { "present" } else { "absent" },
        o_unsaved,
    )
}

pub fn string_of_open_files(files: &lsp_prot::UriMap<OpenFileInfo>) -> String {
    files
        .values()
        .map(string_of_open_file)
        .collect::<Vec<_>>()
        .join(",")
}

pub fn string_of_show_status(show_status: &ShowStatusT) -> String {
    match show_status {
        ShowStatusT::NeverShown => "Never_shown".to_string(),
        ShowStatusT::Shown(id_opt, params) => {
            let id_str = match id_opt {
                None => "None".to_string(),
                Some(id) => lsp_fmt_id_to_string(id),
            };
            let params_json = serde_json::to_string(&serde_json::json!({
                "type": serde_json::to_value(params.request.typ).unwrap_or_default(),
                "message": params.request.message,
                "actions": params.request.actions.as_ref().map(|actions| {
                    actions.iter().map(|a| serde_json::json!({"title": a.title})).collect::<Vec<_>>()
                }),
                "shortMessage": params.short_message,
                "progress": params.progress,
                "total": params.total,
            })).unwrap_or_default();
            format!("Shown id={} params={}", id_str, params_json)
        }
    }
}

pub fn add_ienv(b: &mut String, ienv: &InitializedEnv) {
    addline(
        b,
        "i_connect_params=",
        &string_of_connect_params(&ienv.i_connect_params),
    );
    addline(b, "i_root=", &ienv.i_root.display().to_string());
    addline(b, "i_version=", ienv.i_version.as_deref().unwrap_or("None"));
    addline(b, "i_server_id=", &ienv.i_server_id.to_string());
    addline(
        b,
        "i_can_autostart_after_version_mismatch=",
        &ienv.i_can_autostart_after_version_mismatch.to_string(),
    );
    addline(
        b,
        "i_outstanding_local_handlers=",
        &ienv
            .i_outstanding_local_handlers
            .keys()
            .map(lsp_fmt_id_to_string)
            .collect::<Vec<_>>()
            .join(","),
    );
    addline(
        b,
        "i_outstanding_local_requests=",
        &ienv
            .i_outstanding_local_requests
            .iter()
            .map(|(id, req)| {
                format!(
                    "{}:{}",
                    lsp_fmt_id_to_string(id),
                    request_name_to_string(req)
                )
            })
            .collect::<Vec<_>>()
            .join(","),
    );
    addline(
        b,
        "i_outstanding_requests_from_server=",
        &ienv
            .i_outstanding_requests_from_server
            .iter()
            .map(|(id, req)| {
                format!(
                    "#{}:{}:{}",
                    id.server_id,
                    lsp_fmt_id_to_string(&id.message_id),
                    request_name_to_string(req)
                )
            })
            .collect::<Vec<_>>()
            .join(","),
    );
    addline(b, "i_isConnected=", &ienv.i_is_connected.to_string());
    addline(b, "i_status=", &string_of_show_status(&ienv.i_status));
    addline(
        b,
        "i_open_files=",
        &string_of_open_files(&ienv.i_open_files),
    );
}

pub fn add_denv(b: &mut String, denv: &DisconnectedEnv) {
    let (server_status, watcher_status) = match &denv.d_server_status {
        None => (None, None),
        Some((s, w)) => (Some(s), Some(w)),
    };
    add_ienv(b, &denv.d_ienv);
    addline(b, "d_autostart=", &denv.d_autostart.to_string());
    addline(
        b,
        "d_server_status:server=",
        &server_status
            .map(|s| server_status::string_of_status(false, false, s))
            .unwrap_or_else(|| "None".to_string()),
    );
    addline(
        b,
        "d_server_status:watcher=",
        &watcher_status
            .map(file_watcher_status::string_of_status)
            .unwrap_or_else(|| "None".to_string()),
    );
}

pub fn add_cenv(b: &mut String, cenv: &ConnectedEnv) {
    let (ref server_status, ref watcher_status) = cenv.c_server_status;
    add_ienv(b, &cenv.c_ienv);
    addline(
        b,
        "c_server_status:server=",
        &server_status::string_of_status(false, false, server_status),
    );
    addline(
        b,
        "c_server_status:watcher=",
        &watcher_status
            .as_ref()
            .map(file_watcher_status::string_of_status)
            .unwrap_or_else(|| "None".to_string()),
    );
    addline(
        b,
        "c_about_to_exit_code=",
        &cenv
            .c_about_to_exit_code
            .as_ref()
            .map(|c| flow_common_exit_status::to_string(*c).to_string())
            .unwrap_or_else(|| "None".to_string()),
    );
    addline(b, "c_is_rechecking=", &cenv.c_is_rechecking.to_string());
    addline(
        b,
        "c_lazy_stats=",
        &cenv
            .c_lazy_stats
            .as_ref()
            .map(string_of_lazy_stats)
            .unwrap_or_else(|| "None".to_string()),
    );
    addline(
        b,
        "c_outstanding_requests_to_server=",
        &cenv
            .c_outstanding_requests_to_server
            .iter()
            .map(lsp_fmt_id_to_string)
            .collect::<Vec<_>>()
            .join(","),
    );
}

pub fn string_of_state(state: &ServerState) -> String {
    let mut b = String::with_capacity(10000);
    match state {
        ServerState::Disconnected(denv) => {
            b.push_str("Disconnected:\n");
            add_denv(&mut b, denv);
        }
        ServerState::Connected(cenv) => {
            b.push_str("Connected:\n");
            add_cenv(&mut b, cenv);
        }
    }
    b
}
