/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use flow_common_vcs::git;
use flow_common_vcs::hg;
use flow_common_vcs::vcs;
use flow_common_vcs::vcs_utils;
use flow_event_logger as FlowEventLogger;
use flow_hh_json as Hh_json;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio::sync::Mutex;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    NotInstalled {
        path: String,
    },
    SocketUnavailable {
        msg: String,
    },
    ResponseError {
        request: Option<String>,
        response: String,
        msg: String,
    },
    FreshInstance,
    SubscriptionCanceled,
    UnsupportedWatchRoots {
        roots: Vec<String>,
        failed_paths: BTreeMap<String, ErrorKind>,
    },
}

#[derive(Debug, Clone)]
pub enum Failure {
    Dead,
    Restarted,
}

fn log_msg(request: Option<&str>, response: Option<&str>, msg: &str) {
    let truncated = response.map(|r| {
        if r.len() > 100000 {
            r[..100000].to_string()
        } else {
            r.to_string()
        }
    });
    FlowEventLogger::watchman_error(request, truncated.as_deref(), msg);
    flow_hh_logger::error!("{}", msg);
}

fn log_error(err: &ErrorKind) {
    match err {
        ErrorKind::NotInstalled { path } => {
            let msg = format!("watchman not found on PATH: {}", path);
            log_msg(None, None, &msg);
        }
        ErrorKind::SocketUnavailable { msg } => log_msg(None, None, msg),
        ErrorKind::ResponseError {
            request,
            response,
            msg,
        } => log_msg(request.as_deref(), Some(response), msg),
        ErrorKind::FreshInstance => log_msg(None, None, "Watchman missed some changes"),
        ErrorKind::SubscriptionCanceled => log_msg(None, None, "Subscription canceled by watchman"),
        ErrorKind::UnsupportedWatchRoots {
            roots,
            failed_paths,
        } => {
            if roots.len() >= 2 {
                let msg = format!(
                    "Can't watch paths across multiple Watchman watch_roots. Found {} watch roots:\n{}",
                    roots.len(),
                    roots.join("\n")
                );
                log_msg(None, None, &msg);
            } else {
                let mut rev_paths: Vec<String> = Vec::new();
                for (path, err) in failed_paths.iter() {
                    log_error(err);
                    rev_paths.insert(0, path.clone());
                }
                let path_list = {
                    let mut v = rev_paths.clone();
                    v.reverse();
                    v.join("\n")
                };
                let msg = match roots.as_slice() {
                    [root] => format!(
                        "Found watch root {}, but could not watch paths:\n{}",
                        root, path_list
                    ),
                    _ => format!("Cannot deduce watch root for paths:\n{}", path_list),
                };
                log_msg(None, None, &msg);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubscribeMode {
    AllChanges,
    DeferChanges,
}

pub type Timeout = Option<f64>;

#[derive(Debug, Clone)]
pub struct InitSettings {
    pub debug_logging: bool,
    pub defer_states: Vec<String>,
    pub expression_terms: Vec<serde_json::Value>,
    pub mergebase_with: String,
    pub roots: Vec<PathBuf>,
    pub should_track_mergebase: bool,
    pub subscribe_mode: SubscribeMode,
    pub subscription_prefix: String,
    pub sync_timeout: Option<i64>,
}

#[derive(Debug, Clone)]
pub enum PushedChanges {
    StateEnter(String, Option<serde_json::Value>),
    StateLeave(String, Option<serde_json::Value>),
    FilesChanged {
        changes: BTreeSet<String>,
        changed_mergebase: Option<bool>,
    },
    MissedChanges {
        prev_mergebase: String,
        mergebase: String,
        changes_since_mergebase: BTreeSet<String>,
    },
}

pub mod capability {
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum T {
        ScmSince,
        ScmHg,
        ScmGit,
    }

    pub fn to_string(t: T) -> &'static str {
        match t {
            T::ScmSince => "scm-since",
            T::ScmGit => "scm-git",
            T::ScmHg => "scm-hg",
        }
    }

    pub fn all() -> Vec<T> {
        vec![T::ScmSince, T::ScmHg, T::ScmGit]
    }

    pub type Set = BTreeSet<T>;
}

mod jget {

    pub fn string_opt(obj: Option<&serde_json::Value>, key: &str) -> Option<String> {
        obj.and_then(|v| v.get(key))
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    }

    pub fn string_exn(obj: Option<&serde_json::Value>, key: &str) -> String {
        string_opt(obj, key).unwrap_or_default()
    }

    pub fn bool_opt(obj: Option<&serde_json::Value>, key: &str) -> Option<bool> {
        obj.and_then(|v| v.get(key)).and_then(|v| v.as_bool())
    }
}

mod j {

    pub fn strlist(items: &[&str]) -> serde_json::Value {
        serde_json::Value::Array(
            items
                .iter()
                .map(|s| serde_json::Value::String((*s).to_string()))
                .collect(),
        )
    }

    pub fn pred(name: &str, items: Vec<serde_json::Value>) -> serde_json::Value {
        let mut arr = vec![serde_json::Value::String(name.to_string())];
        arr.extend(items);
        serde_json::Value::Array(arr)
    }

    pub fn try_get_val(key: &str, data: &serde_json::Value) -> Option<serde_json::Value> {
        data.get(key).cloned()
    }

    pub fn get_string_val(key: &str, data: &serde_json::Value) -> String {
        get_string_val_with_default(None, key, data)
    }

    pub fn get_string_val_with_default(
        default: Option<&str>,
        key: &str,
        data: &serde_json::Value,
    ) -> String {
        match data.get(key).and_then(|v| v.as_str()) {
            Some(s) => s.to_string(),
            None => default.unwrap_or("").to_string(),
        }
    }
}

fn json_of_string(s: &str) -> Result<serde_json::Value, String> {
    match serde_json::from_str::<serde_json::Value>(s) {
        Ok(v) => Ok(v),
        Err(err) => Err(format!("Failed to parse string as JSON: {}", err)),
    }
}

fn parse_response(debug_logging: bool, output: &str) -> Result<serde_json::Value, String> {
    fn log_warnings(obj: &serde_json::Value) {
        match jget::string_opt(Some(obj), "warning") {
            None => (),
            Some(warning) => {
                FlowEventLogger::watchman_warning(&warning);
                flow_hh_logger::info!("Watchman warning: {}\n", warning);
            }
        }
    }
    fn handle_errors(obj: serde_json::Value) -> Result<serde_json::Value, String> {
        match jget::string_opt(Some(&obj), "error") {
            None => Ok(obj),
            Some(error) => Err(error),
        }
    }
    if debug_logging {
        flow_hh_logger::info!("Watchman response: {}", output);
    }
    let json = json_of_string(output)?;
    log_warnings(&json);
    handle_errors(json)
}

pub struct Conn {
    reader: Mutex<BufReader<Box<dyn AsyncRead + Unpin + Send>>>,
    writer: Mutex<Box<dyn AsyncWrite + Unpin + Send>>,
}

const MAX_REINIT_ATTEMPTS: u32 = 8;

const MAX_RETRY_ATTEMPTS: u32 = 3;

#[derive(Debug, Clone)]
pub struct Watch {
    pub watch_root: String,
    pub watched_path_expression_terms: Option<serde_json::Value>,
}

#[derive(Clone)]
pub struct DeadEnv {
    pub prior_settings: InitSettings,
    pub dead_since: f64,
    pub prior_clockspec: String,
    pub prior_watch_root: String,
}

pub struct Env {
    pub clockspec: String,
    pub finished_an_hg_update: bool,
    pub mergebase: Option<String>,
    pub conn: Arc<Conn>,
    pub settings: InitSettings,
    pub should_track_mergebase: bool,
    pub subscription: String,
    pub vcs: Option<vcs::Vcs>,
    pub watch: Watch,
}

fn dead_env_from_alive(env: &Env) -> DeadEnv {
    DeadEnv {
        prior_settings: env.settings.clone(),
        dead_since: SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs_f64())
            .unwrap_or(0.0),
        prior_clockspec: env.clockspec.clone(),
        prior_watch_root: env.watch.watch_root.clone(),
    }
}

fn project_bool(m: Option<bool>) -> bool {
    m.unwrap_or(false)
}

fn response_error_of_json(
    query: Option<&serde_json::Value>,
    response: &serde_json::Value,
    msg: String,
) -> ErrorKind {
    let request = query.map(Hh_json::json_string_of_value);
    let response = Hh_json::json_string_of_value(response);
    ErrorKind::ResponseError {
        request,
        response,
        msg,
    }
}

fn get_string_prop(
    query: Option<&serde_json::Value>,
    prop: &str,
    json: &serde_json::Value,
) -> Result<String, ErrorKind> {
    match json.get(prop).and_then(|v| v.as_str()) {
        Some(result) => Ok(result.to_string()),
        None => {
            let msg = format!("Missing string property '{}'", prop);
            Err(response_error_of_json(query, json, msg))
        }
    }
}

fn clock(watch: &Watch) -> serde_json::Value {
    j::strlist(&["clock", &watch.watch_root])
}

enum WatchCommand {
    Subscribe,
    Query,
}

fn request_json(
    extra_kv: Vec<(String, serde_json::Value)>,
    extra_expressions: Vec<serde_json::Value>,
    watchman_command: WatchCommand,
    env: &Env,
) -> serde_json::Value {
    use serde_json::Value;
    let command = match watchman_command {
        WatchCommand::Subscribe => "subscribe",
        WatchCommand::Query => "query",
    };
    let mut header = vec![
        Value::String(command.to_string()),
        Value::String(env.watch.watch_root.clone()),
    ];
    match watchman_command {
        WatchCommand::Subscribe => header.push(Value::String(env.subscription.clone())),
        _ => (),
    }
    let mut expressions = extra_expressions;
    expressions.extend(env.settings.expression_terms.iter().cloned());
    let expressions = match &env.watch.watched_path_expression_terms {
        Some(terms) => {
            let mut v = vec![terms.clone()];
            v.extend(expressions);
            v
        }
        None => expressions,
    };
    assert!(!expressions.is_empty());
    let mut extra_kv_full: Vec<(String, Value)> = match env.settings.sync_timeout {
        Some(sync_timeout) => {
            let mut v = vec![(
                "sync_timeout".to_string(),
                serde_json::Number::from(sync_timeout).into(),
            )];
            v.extend(extra_kv);
            v
        }
        None => extra_kv,
    };
    extra_kv_full.push(("fields".to_string(), j::strlist(&["name"])));
    extra_kv_full.push(("expression".to_string(), j::pred("allof", expressions)));
    let mut object = serde_json::Map::new();
    for (k, v) in extra_kv_full {
        object.insert(k, v);
    }
    let directives = vec![Value::Object(object)];
    let mut request = header;
    request.extend(directives);
    Value::Array(request)
}

fn get_changes_since_mergebase_query(env: &Env) -> serde_json::Value {
    let mergebase_with = &env.settings.mergebase_with;
    let mut mergebase_obj = serde_json::Map::new();
    mergebase_obj.insert(
        "mergebase-with".to_string(),
        serde_json::Value::String(mergebase_with.clone()),
    );
    let mut scm_obj = serde_json::Map::new();
    scm_obj.insert("scm".to_string(), serde_json::Value::Object(mergebase_obj));
    let extra_kv = vec![("since".to_string(), serde_json::Value::Object(scm_obj))];
    request_json(extra_kv, vec![], WatchCommand::Query, env)
}

fn subscribe_query(env: &Env) -> serde_json::Value {
    let since = (
        "since".to_string(),
        serde_json::Value::String(env.clockspec.clone()),
    );
    let empty_on_fresh_instance = (
        "empty_on_fresh_instance".to_string(),
        serde_json::Value::Bool(true),
    );
    let mode = {
        let mut states = vec!["hg.update".to_string()];
        states.extend(env.settings.defer_states.iter().cloned());
        match env.settings.subscribe_mode {
            SubscribeMode::AllChanges => vec![],
            SubscribeMode::DeferChanges => {
                let states_refs: Vec<&str> = states.iter().map(String::as_str).collect();
                vec![("defer".to_string(), j::strlist(&states_refs))]
            }
        }
    };
    let mut extra_kv = vec![since, empty_on_fresh_instance];
    extra_kv.extend(mode);
    request_json(extra_kv, vec![], WatchCommand::Subscribe, env)
}

fn is_fresh_instance_response(obj: &serde_json::Value) -> bool {
    project_bool(jget::bool_opt(Some(obj), "is_fresh_instance"))
}

fn supports_scm_queries(caps: &capability::Set, vcs: Option<vcs::Vcs>) -> bool {
    caps.contains(&capability::T::ScmSince)
        && match vcs {
            Some(vcs::Vcs::Hg) => caps.contains(&capability::T::ScmHg),
            Some(vcs::Vcs::Git) => caps.contains(&capability::T::ScmGit),
            None => false,
        }
}

fn subscription_name(settings: &InitSettings) -> String {
    let pretty_pid = std::process::id();
    format!("{}.{}", settings.subscription_prefix, pretty_pid)
}

async fn send_request(
    debug_logging: bool,
    conn: &Conn,
    json: &serde_json::Value,
) -> Result<(), ErrorKind> {
    let json_str = Hh_json::json_string_of_value(json);
    if debug_logging {
        flow_hh_logger::info!("Watchman request: {}", json_str);
    }
    let mut writer = conn.writer.lock().await;
    let mut buf = json_str.into_bytes();
    buf.push(b'\n');
    match writer.write_all(&buf).await {
        Ok(()) => match writer.flush().await {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {
                let msg = "Connection closed (EPIPE)".to_string();
                Err(ErrorKind::SocketUnavailable { msg })
            }
            Err(e) => Err(ErrorKind::SocketUnavailable {
                msg: format!("write error: {}", e),
            }),
        },
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {
            let msg = "Connection closed (EPIPE)".to_string();
            Err(ErrorKind::SocketUnavailable { msg })
        }
        Err(e) => Err(ErrorKind::SocketUnavailable {
            msg: format!("write error: {}", e),
        }),
    }
}

async fn get_sockname() -> Result<String, ErrorKind> {
    let cmd = "watchman";
    let args = ["--no-pretty", "get-sockname"];
    let output_res = tokio::process::Command::new(cmd).args(args).output().await;
    let output = match output_res {
        Ok(o) => o,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let path = std::env::var("PATH").unwrap_or_else(|_| "(not set)".to_string());
            return Err(ErrorKind::NotInstalled { path });
        }
        Err(e) => {
            let msg = format!("get-sockname failed to execute: {}", e);
            return Err(ErrorKind::SocketUnavailable { msg });
        }
    };
    let status = output.status;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    if status.success() {
        let json = match serde_json::from_str::<serde_json::Value>(&stdout) {
            Ok(v) => v,
            Err(e) => {
                let msg = format!("get-sockname returned invalid JSON: {}", e);
                return Err(ErrorKind::SocketUnavailable { msg });
            }
        };
        return Ok(j::get_string_val("sockname", &json));
    }
    if let Some(127) = status.code() {
        let path = std::env::var("PATH").unwrap_or_else(|_| "(not set)".to_string());
        return Err(ErrorKind::NotInstalled { path });
    }
    if let Some(code) = status.code() {
        let msg = format!("get-sockname exited code {}, stderr = {:?}", code, stderr);
        return Err(ErrorKind::SocketUnavailable { msg });
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = status.signal() {
            let signal_name = string_of_signal(signal);
            let msg = format!("get-sockname signaled with {} signal", signal_name);
            return Err(ErrorKind::SocketUnavailable { msg });
        }
    }
    let msg = "get-sockname terminated abnormally".to_string();
    Err(ErrorKind::SocketUnavailable { msg })
}

#[cfg(unix)]
fn string_of_signal(signal: i32) -> String {
    match signal {
        1 => "SIGHUP".to_string(),
        2 => "SIGINT".to_string(),
        3 => "SIGQUIT".to_string(),
        4 => "SIGILL".to_string(),
        5 => "SIGTRAP".to_string(),
        6 => "SIGABRT".to_string(),
        7 => "SIGBUS".to_string(),
        8 => "SIGFPE".to_string(),
        9 => "SIGKILL".to_string(),
        10 => "SIGUSR1".to_string(),
        11 => "SIGSEGV".to_string(),
        12 => "SIGUSR2".to_string(),
        13 => "SIGPIPE".to_string(),
        14 => "SIGALRM".to_string(),
        15 => "SIGTERM".to_string(),
        _ => format!("signal {}", signal),
    }
}

async fn open_connection() -> Result<Arc<Conn>, ErrorKind> {
    let sockname = get_sockname().await?;
    #[cfg(unix)]
    let (read_half, write_half): (
        Box<dyn AsyncRead + Unpin + Send>,
        Box<dyn AsyncWrite + Unpin + Send>,
    ) = match tokio::net::UnixStream::connect(&sockname).await {
        Ok(s) => {
            let (r, w) = tokio::io::split(s);
            (Box::new(r), Box::new(w))
        }
        Err(e) => {
            let msg = format!("{} (socket: {})", e, sockname);
            return Err(ErrorKind::SocketUnavailable { msg });
        }
    };
    #[cfg(windows)]
    let (read_half, write_half): (
        Box<dyn AsyncRead + Unpin + Send>,
        Box<dyn AsyncWrite + Unpin + Send>,
    ) = match tokio::net::windows::named_pipe::ClientOptions::new().open(&sockname) {
        Ok(p) => {
            let (r, w) = tokio::io::split(p);
            (Box::new(r), Box::new(w))
        }
        Err(e) => {
            let msg = format!("{} (socket: {})", e, sockname);
            return Err(ErrorKind::SocketUnavailable { msg });
        }
    };
    let reader = BufReader::new(read_half);
    Ok(Arc::new(Conn {
        reader: Mutex::new(reader),
        writer: Mutex::new(write_half),
    }))
}

async fn close_connection(conn: &Conn) {
    let mut writer = conn.writer.lock().await;
    let _ = writer.shutdown().await;
}

async fn with_watchman_conn<F, Fut>(f: F) -> Result<serde_json::Value, ErrorKind>
where
    F: FnOnce(Arc<Conn>) -> Fut,
    Fut: std::future::Future<Output = Result<serde_json::Value, ErrorKind>>,
{
    match open_connection().await {
        Ok(conn) => {
            let result = f(conn.clone()).await;
            close_connection(&conn).await;
            result
        }
        Err(err) => Err(err),
    }
}

async fn read_line(conn: &Conn) -> Result<String, ErrorKind> {
    let mut reader = conn.reader.lock().await;
    let mut line = String::new();
    match reader.read_line(&mut line).await {
        Ok(0) => {
            let msg = "Connection closed (end of file)".to_string();
            Err(ErrorKind::SocketUnavailable { msg })
        }
        Ok(_) => {
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(line)
        }
        Err(e) => match e.kind() {
            std::io::ErrorKind::ConnectionReset => {
                let msg = "Connection closed (connection reset)".to_string();
                Err(ErrorKind::SocketUnavailable { msg })
            }
            _ => {
                let raw = e.raw_os_error().unwrap_or(0);
                let msg = if raw == libc_ebadf() {
                    "Connection closed (bad file descriptor)".to_string()
                } else {
                    format!("Connection closed: {}", e)
                };
                Err(ErrorKind::SocketUnavailable { msg })
            }
        },
    }
}

#[cfg(unix)]
fn libc_ebadf() -> i32 {
    9
}
#[cfg(not(unix))]
fn libc_ebadf() -> i32 {
    -1
}

async fn read_line_with_timeout(timeout: f64, conn: &Conn) -> Result<String, ErrorKind> {
    let dur = Duration::from_secs_f64(timeout);
    match tokio::time::timeout(dur, read_line(conn)).await {
        Ok(res) => res,
        Err(_) => {
            let msg = format!("Timed out reading payload after {} seconds", timeout);
            Err(ErrorKind::SocketUnavailable { msg })
        }
    }
}

fn request<'a>(
    debug_logging: bool,
    conn: Option<Arc<Conn>>,
    json: serde_json::Value,
) -> std::pin::Pin<
    Box<dyn std::future::Future<Output = Result<serde_json::Value, ErrorKind>> + Send + 'a>,
> {
    Box::pin(async move {
        match conn {
            None => {
                let json_clone = json.clone();
                let debug_logging_inner = debug_logging;
                with_watchman_conn(move |conn| async move {
                    request(debug_logging_inner, Some(conn), json_clone).await
                })
                .await
            }
            Some(conn) => {
                send_request(debug_logging, &conn, &json).await?;
                let response = read_line(&conn).await?;
                match parse_response(debug_logging, &response) {
                    Ok(v) => Ok(v),
                    Err(msg) => {
                        let request = Some(Hh_json::json_string_of_value(&json));
                        Err(ErrorKind::ResponseError {
                            request,
                            response,
                            msg,
                        })
                    }
                }
            }
        }
    })
}

async fn blocking_read(debug_logging: bool, conn: &Conn) -> Result<serde_json::Value, ErrorKind> {
    let response = read_line_with_timeout(40.0, conn).await?;
    match parse_response(debug_logging, &response) {
        Ok(v) => Ok(v),
        Err(msg) => Err(ErrorKind::ResponseError {
            request: None,
            response,
            msg,
        }),
    }
}

async fn get_capabilities(
    debug_logging: bool,
    conn: Option<Arc<Conn>>,
) -> Result<capability::Set, ErrorKind> {
    use serde_json::Value;
    let names: Vec<&str> = capability::all()
        .iter()
        .map(|c| capability::to_string(*c))
        .collect();
    let mut optional_obj = serde_json::Map::new();
    optional_obj.insert("optional".to_string(), j::strlist(&names));
    let query = Value::Array(vec![
        Value::String("version".to_string()),
        Value::Object(optional_obj),
    ]);
    match request(debug_logging, conn, query).await {
        Ok(obj) => {
            let mut set = capability::Set::new();
            for cap in capability::all() {
                let name = capability::to_string(cap);
                let value = project_bool(
                    obj.get("capabilities")
                        .and_then(|c| c.get(name))
                        .and_then(|v| v.as_bool()),
                );
                if value {
                    set.insert(cap);
                }
            }
            Ok(set)
        }
        Err(err) => Err(err),
    }
}

async fn is_fresh_instance_since(
    conn: Option<Arc<Conn>>,
    debug_logging: bool,
    watch_root: &str,
    clockspec: &str,
) -> Result<bool, ErrorKind> {
    use serde_json::Value;
    let hard_to_match_name = "irrelevant.potato";
    let mut q_obj = serde_json::Map::new();
    q_obj.insert("since".to_string(), Value::String(clockspec.to_string()));
    q_obj.insert("empty_on_fresh_instance".to_string(), Value::Bool(true));
    q_obj.insert(
        "expression".to_string(),
        Value::Array(vec![
            Value::String("name".to_string()),
            Value::String(hard_to_match_name.to_string()),
        ]),
    );
    q_obj.insert(
        "sync_timeout".to_string(),
        Value::Number(serde_json::Number::from(0)),
    );
    let query = Value::Array(vec![
        Value::String("query".to_string()),
        Value::String(watch_root.to_string()),
        Value::Object(q_obj),
    ]);
    match request(debug_logging, conn, query.clone()).await {
        Ok(response) => match jget::bool_opt(Some(&response), "is_fresh_instance") {
            Some(has_restarted) => Ok(has_restarted),
            None => {
                let msg = "Expected an is_fresh_instance property".to_string();
                Err(response_error_of_json(Some(&query), &response, msg))
            }
        },
        Err(ErrorKind::ResponseError { .. }) => Ok(true),
        Err(err) => Err(err),
    }
}

fn prepend_relative_path_term(
    relative_path: &str,
    terms: Option<Vec<serde_json::Value>>,
) -> Option<Vec<serde_json::Value>> {
    match terms {
        None => None,
        Some(_) if relative_path.is_empty() => None,
        Some(terms) => {
            let mut v = vec![
                j::strlist(&["dirname", relative_path]),
                j::strlist(&["name", relative_path]),
            ];
            v.extend(terms);
            Some(v)
        }
    }
}

async fn get_clockspec(
    debug_logging: bool,
    conn: Arc<Conn>,
    watch: &Watch,
    prior_clockspec: Option<String>,
) -> Result<String, ErrorKind> {
    match prior_clockspec {
        Some(clockspec) => Ok(clockspec),
        None => {
            let query = clock(watch);
            let response = request(debug_logging, Some(conn), query.clone()).await?;
            get_string_prop(Some(&query), "clock", &response)
        }
    }
}

async fn watch_project(
    debug_logging: bool,
    conn: Arc<Conn>,
    root: &Path,
) -> Result<(String, String), ErrorKind> {
    let root_str = root.to_string_lossy().to_string();
    let query = j::strlist(&["watch-project", &root_str]);
    let response = request(debug_logging, Some(conn), query).await?;
    let watch_root = j::get_string_val("watch", &response);
    let relative_path = j::get_string_val_with_default(Some(""), "relative_path", &response);
    Ok((watch_root, relative_path))
}

async fn watch_paths(
    debug_logging: bool,
    conn: Arc<Conn>,
    paths: &[PathBuf],
) -> Result<
    (
        Option<Vec<serde_json::Value>>,
        BTreeSet<String>,
        BTreeMap<String, ErrorKind>,
    ),
    ErrorKind,
> {
    let mut terms: Option<Vec<serde_json::Value>> = Some(vec![]);
    let mut watch_roots: BTreeSet<String> = BTreeSet::new();
    let mut failed_paths: BTreeMap<String, ErrorKind> = BTreeMap::new();
    for path in paths {
        match watch_project(debug_logging, conn.clone(), path).await {
            Ok((watch_root, relative_path)) => {
                terms = prepend_relative_path_term(&relative_path, terms);
                watch_roots.insert(watch_root);
            }
            Err(err @ ErrorKind::ResponseError { .. }) => {
                failed_paths.insert(path.to_string_lossy().to_string(), err);
            }
            Err(err) => return Err(err),
        }
    }
    Ok((terms, watch_roots, failed_paths))
}

async fn watch(
    debug_logging: bool,
    conn: Arc<Conn>,
    roots: &[PathBuf],
) -> Result<Watch, ErrorKind> {
    fn guess_missing_relative_paths(
        terms: Option<Vec<serde_json::Value>>,
        watch_root: &str,
        failed_paths: BTreeMap<String, ErrorKind>,
    ) -> Result<Option<Vec<serde_json::Value>>, ErrorKind> {
        let watch_root = {
            let normalized = normalize_filename_dir_sep(watch_root);
            if normalized.ends_with('/') {
                normalized
            } else {
                format!("{}/", normalized)
            }
        };
        let mut terms = terms;
        let mut new_failed_paths: BTreeMap<String, ErrorKind> = BTreeMap::new();
        for (path, err) in failed_paths.into_iter() {
            let path_norm = normalize_filename_dir_sep(&path);
            if path_norm.starts_with(&watch_root) {
                let relative_path = &path_norm[watch_root.len()..];
                terms = prepend_relative_path_term(relative_path, terms);
            } else {
                new_failed_paths.insert(path_norm, err);
            }
        }
        if new_failed_paths.is_empty() {
            Ok(terms)
        } else {
            Err(ErrorKind::UnsupportedWatchRoots {
                roots: vec![watch_root],
                failed_paths: new_failed_paths,
            })
        }
    }
    fn consolidate_watch_roots(
        watch_roots: BTreeSet<String>,
        failed_paths: BTreeMap<String, ErrorKind>,
    ) -> Result<String, ErrorKind> {
        let elements: Vec<String> = watch_roots.into_iter().collect();
        match elements.as_slice() {
            [watch_root] => Ok(watch_root.clone()),
            _ => Err(ErrorKind::UnsupportedWatchRoots {
                roots: elements,
                failed_paths,
            }),
        }
    }
    let (terms, watch_roots, failed_paths) = watch_paths(debug_logging, conn, roots).await?;
    let watch_root = consolidate_watch_roots(watch_roots, failed_paths.clone())?;
    let terms = guess_missing_relative_paths(terms, &watch_root, failed_paths)?;
    let watched_path_expression_terms = terms.map(|t| j::pred("anyof", t));
    Ok(Watch {
        watch_root,
        watched_path_expression_terms,
    })
}

fn normalize_filename_dir_sep(s: &str) -> String {
    if cfg!(windows) {
        s.replace('\\', "/")
    } else {
        s.to_string()
    }
}

async fn with_retry<X, T, F, FFut, OR, ORFut>(
    max_attempts: u32,
    on_retry: OR,
    mut f: F,
    mut x: X,
) -> Result<T, Failure>
where
    X: Clone + Send,
    T: Send,
    F: FnMut(X) -> FFut + Send,
    FFut: std::future::Future<Output = Result<T, ErrorKind>> + Send,
    OR: Fn(u32, X) -> ORFut + Send,
    ORFut: std::future::Future<Output = Result<X, Failure>> + Send,
{
    let mut attempt: u32 = 0;
    loop {
        match f(x.clone()).await {
            Ok(ok) => return Ok(ok),
            Err(err) => {
                log_error(&err);
                match err {
                    ErrorKind::NotInstalled { .. } | ErrorKind::UnsupportedWatchRoots { .. } => {
                        return Err(Failure::Dead);
                    }
                    ErrorKind::FreshInstance => return Err(Failure::Restarted),
                    ErrorKind::ResponseError { .. }
                    | ErrorKind::SocketUnavailable { .. }
                    | ErrorKind::SubscriptionCanceled => {
                        if attempt >= max_attempts {
                            flow_hh_logger::error!(
                                "Watchman has failed {} times in a row. Giving up.",
                                attempt
                            );
                            return Err(Failure::Dead);
                        } else {
                            match on_retry(attempt, x.clone()).await {
                                Ok(new_x) => {
                                    x = new_x;
                                    attempt += 1;
                                }
                                Err(err) => return Err(err),
                            }
                        }
                    }
                }
            }
        }
    }
}

async fn re_init(
    prior_clockspec: Option<String>,
    settings: InitSettings,
) -> Result<Env, ErrorKind> {
    let debug_logging = settings.debug_logging;
    let conn = open_connection().await?;
    let capabilities = get_capabilities(debug_logging, Some(conn.clone())).await?;
    let watch_v = watch(debug_logging, conn.clone(), &settings.roots).await?;
    let clockspec = get_clockspec(debug_logging, conn.clone(), &watch_v, prior_clockspec).await?;
    let subscription = subscription_name(&settings);
    let vcs = vcs::find(None, std::path::Path::new(&watch_v.watch_root));
    let should_track_mergebase =
        settings.should_track_mergebase && supports_scm_queries(&capabilities, vcs);
    let env = Env {
        settings,
        conn: conn.clone(),
        watch: watch_v,
        clockspec,
        subscription,
        vcs,
        should_track_mergebase,
        mergebase: None,
        finished_an_hg_update: false,
    };
    let response = request(debug_logging, Some(conn), subscribe_query(&env)).await?;
    let _ = response;
    Ok(env)
}

fn backoff_delay(attempts: u32) -> f64 {
    let attempts = attempts.min(3);
    (4u32 * (1u32 << attempts)) as f64
}

fn extract_file_names(env: &Env, json: &serde_json::Value) -> BTreeSet<String> {
    let files = match json.get("files").and_then(|v| v.as_array()) {
        Some(arr) => arr.clone(),
        None => vec![],
    };
    let root = &env.watch.watch_root;
    let mut out: BTreeSet<String> = BTreeSet::new();
    for json in files.iter() {
        let s = json.as_str().unwrap_or("");
        let mut abs = String::with_capacity(root.len() + 1 + s.len());
        abs.push_str(root);
        abs.push('/');
        abs.push_str(s);
        if cfg!(windows) {
            abs = abs.replace('/', "\\");
        }
        out.insert(abs);
    }
    out
}

async fn can_re_init(allow_fresh_instance: bool, dead_env: &DeadEnv) -> Result<bool, ErrorKind> {
    if allow_fresh_instance {
        Ok(true)
    } else {
        let debug_logging = dead_env.prior_settings.debug_logging;
        let clockspec = &dead_env.prior_clockspec;
        let watch_root = &dead_env.prior_watch_root;
        match is_fresh_instance_since(None, debug_logging, watch_root, clockspec).await {
            Ok(missed_changes) => Ok(!missed_changes),
            Err(err) => Err(err),
        }
    }
}

async fn re_init_dead_env_once(
    allow_fresh_instance: bool,
    dead_env: DeadEnv,
) -> Result<Env, ErrorKind> {
    let timeout = dead_env.prior_settings.sync_timeout.unwrap_or(0) + 120000;
    let dur = Duration::from_secs_f64(timeout as f64 / 1000.0);
    let timeout_for_msg = timeout;
    match tokio::time::timeout(dur, async move {
        match can_re_init(allow_fresh_instance, &dead_env).await {
            Ok(true) => {
                flow_hh_logger::info!("Attempting to reestablish watchman subscription");
                let prior_clockspec = if allow_fresh_instance {
                    None
                } else {
                    Some(dead_env.prior_clockspec.clone())
                };
                re_init(prior_clockspec, dead_env.prior_settings.clone()).await
            }
            Ok(false) => {
                flow_hh_logger::info!("Unable to resubscribe to a fresh watchman instance");
                Err(ErrorKind::FreshInstance)
            }
            Err(err) => Err(err),
        }
    })
    .await
    {
        Ok(res) => res,
        Err(_) => Err(ErrorKind::SocketUnavailable {
            msg: format!("Timed out after {}ds", timeout_for_msg),
        }),
    }
}

async fn re_init_dead_env(allow_fresh_instance: bool, dead_env: DeadEnv) -> Result<Env, Failure> {
    let on_retry = |attempt: u32, dead_env: DeadEnv| async move {
        let backoff = backoff_delay(attempt);
        flow_hh_logger::info!(
            "Waiting {}s before reestablishing watchman subscription",
            backoff
        );
        tokio::time::sleep(Duration::from_secs_f64(backoff)).await;
        Ok(dead_env)
    };
    let dead_since = dead_env.dead_since;
    match with_retry(
        MAX_REINIT_ATTEMPTS,
        on_retry,
        |dead_env| re_init_dead_env_once(allow_fresh_instance, dead_env),
        dead_env,
    )
    .await
    {
        Ok(env) => {
            flow_hh_logger::info!("Watchman connection reestablished.");
            let now = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0);
            let downtime = now - dead_since;
            FlowEventLogger::watchman_connection_reestablished(downtime);
            Ok(env)
        }
        Err(err) => Err(err),
    }
}

async fn close_channel_on_instance(env: &Env) -> DeadEnv {
    close_connection(&env.conn).await;
    dead_env_from_alive(env)
}

pub async fn close(env: &Env) {
    close_connection(&env.conn).await
}

enum StateChange {
    Enter,
    Leave,
}

fn make_state_change_response(
    state: StateChange,
    name: String,
    data: &serde_json::Value,
) -> PushedChanges {
    let metadata = j::try_get_val("metadata", data);
    match state {
        StateChange::Enter => PushedChanges::StateEnter(name, metadata),
        StateChange::Leave => PushedChanges::StateLeave(name, metadata),
    }
}

fn extract_mergebase(data: &serde_json::Value) -> Option<(String, String)> {
    let fat_clock = data.get("clock")?;
    let clock = fat_clock.get("clock")?.as_str()?;
    let mergebase = fat_clock.get("scm")?.get("mergebase")?.as_str()?;
    Some((clock.to_string(), mergebase.to_string()))
}

fn subscription_is_cancelled(data: &serde_json::Value) -> bool {
    match jget::bool_opt(Some(data), "canceled") {
        None | Some(false) => false,
        Some(true) => true,
    }
}

async fn get_mergebase_from_vcs(
    vcs: Option<vcs::Vcs>,
    root: &str,
    mergebase_with: &str,
) -> Result<Option<String>, vcs_utils::ErrorStatus> {
    match vcs {
        Some(vcs::Vcs::Hg) => match hg::merge_base(Some(root), ".", mergebase_with).await {
            Ok(hash) => Ok(Some(hash)),
            Err(err) => Err(err),
        },
        Some(vcs::Vcs::Git) => match git::merge_base(Some(root), mergebase_with, "HEAD").await {
            Ok(hash) => Ok(Some(hash)),
            Err(err) => Err(err),
        },
        None => Ok(None),
    }
}

async fn get_mergebase(env: &Env) -> Result<Option<String>, vcs_utils::ErrorStatus> {
    if env.should_track_mergebase {
        let vcs = env.vcs;
        let root = &env.watch.watch_root;
        let mergebase_with = &env.settings.mergebase_with;
        get_mergebase_from_vcs(vcs, root, mergebase_with).await
    } else {
        Ok(None)
    }
}

async fn get_mergebase_and_changes(
    env: &Env,
) -> Result<Option<(String, BTreeSet<String>)>, Failure> {
    if env.should_track_mergebase {
        let on_retry = |attempt: u32, env: ()| async move {
            tokio::time::sleep(Duration::from_secs_f64(backoff_delay(attempt))).await;
            Ok(env)
        };
        let env_ref = env;
        with_retry(
            MAX_RETRY_ATTEMPTS,
            on_retry,
            |_unit: ()| async move {
                let debug_logging = env_ref.settings.debug_logging;
                let query = get_changes_since_mergebase_query(env_ref);
                let response = request(debug_logging, None, query.clone()).await?;
                match extract_mergebase(&response) {
                    Some((_clock, mergebase)) => {
                        let changes = extract_file_names(env_ref, &response);
                        Ok(Some((mergebase, changes)))
                    }
                    None => Err(response_error_of_json(
                        Some(&query),
                        &response,
                        "Failed to extract mergebase".to_string(),
                    )),
                }
            },
            (),
        )
        .await
    } else {
        Ok(None)
    }
}

async fn check_for_changed_mergebase(env: &mut Env) -> Option<bool> {
    match env.mergebase.clone() {
        None => {
            flow_hh_logger::debug!(
                "Unable to check for changed mergebase: unknown previous mergebase"
            );
            None
        }
        Some(old_mergebase) => {
            let new_mergebase = match get_mergebase(env).await {
                Ok(mergebase) => mergebase,
                Err(vcs_utils::ErrorStatus::NotInstalled { .. }) => {
                    panic!("Failed to query mergebase: unable to find vcs");
                }
                Err(vcs_utils::ErrorStatus::Errored(msg)) => {
                    panic!("Failed to query mergebase: {}", msg);
                }
            };
            match new_mergebase {
                Some(mergebase) => {
                    let changed_mergebase = mergebase != old_mergebase;
                    if changed_mergebase {
                        flow_hh_logger::info!(
                            "Watchman reports mergebase changed from {:?} to {:?}",
                            old_mergebase,
                            mergebase
                        );
                        env.mergebase = Some(mergebase);
                    } else {
                        flow_hh_logger::debug!(
                            "Watchman reports mergebase is unchanged at {:?}",
                            mergebase
                        );
                    }
                    Some(changed_mergebase)
                }
                None => {
                    flow_hh_logger::warn!("Unable to fetch current mergebase");
                    None
                }
            }
        }
    }
}

pub async fn recover_from_restart(env: Env) -> Result<(Env, PushedChanges), Failure> {
    match env.mergebase.clone() {
        None => Err(Failure::Restarted),
        Some(prev_mergebase) => {
            let dead_env = close_channel_on_instance(&env).await;
            match re_init_dead_env(true, dead_env).await {
                Ok(mut env) => match get_mergebase_and_changes(&env).await {
                    Err(err) => Err(err),
                    Ok(None) => Err(Failure::Restarted),
                    Ok(Some((mergebase, changes_since_mergebase))) => {
                        env.mergebase = Some(mergebase.clone());
                        let pushed_changes = PushedChanges::MissedChanges {
                            prev_mergebase,
                            mergebase,
                            changes_since_mergebase,
                        };
                        Ok((env, pushed_changes))
                    }
                },
                Err(err) => Err(err),
            }
        }
    }
}

async fn transform_asynchronous_get_changes_response(
    env: &mut Env,
    data: &serde_json::Value,
) -> Result<PushedChanges, ErrorKind> {
    if is_fresh_instance_response(data) {
        return Err(ErrorKind::FreshInstance);
    }
    if subscription_is_cancelled(data) {
        return Err(ErrorKind::SubscriptionCanceled);
    }
    env.clockspec = jget::string_exn(Some(data), "clock");
    match jget::string_opt(Some(data), "state-enter") {
        Some(state) => Ok(make_state_change_response(StateChange::Enter, state, data)),
        None => match jget::string_opt(Some(data), "state-leave") {
            Some(state) => {
                if state == "hg.update" {
                    env.finished_an_hg_update = true;
                }
                Ok(make_state_change_response(StateChange::Leave, state, data))
            }
            None => {
                let changes = extract_file_names(env, data);
                let changed_mergebase = if env.finished_an_hg_update {
                    env.finished_an_hg_update = false;
                    check_for_changed_mergebase(env).await
                } else {
                    None
                };
                Ok(PushedChanges::FilesChanged {
                    changes,
                    changed_mergebase,
                })
            }
        },
    }
}

pub async fn get_changes(mut env: Env) -> Result<(Env, PushedChanges), Failure> {
    let mut attempt: u32 = 0;
    loop {
        let debug_logging = env.settings.debug_logging;
        let conn = env.conn.clone();
        let result = match blocking_read(debug_logging, &conn).await {
            Ok(response) => transform_asynchronous_get_changes_response(&mut env, &response).await,
            Err(err) => Err(err),
        };
        match result {
            Ok(changes) => return Ok((env, changes)),
            Err(err) => {
                log_error(&err);
                match err {
                    ErrorKind::NotInstalled { .. } | ErrorKind::UnsupportedWatchRoots { .. } => {
                        return Err(Failure::Dead);
                    }
                    ErrorKind::FreshInstance => return Err(Failure::Restarted),
                    ErrorKind::ResponseError { .. }
                    | ErrorKind::SocketUnavailable { .. }
                    | ErrorKind::SubscriptionCanceled => {
                        if attempt >= MAX_RETRY_ATTEMPTS {
                            flow_hh_logger::error!(
                                "Watchman has failed {} times in a row. Giving up.",
                                attempt
                            );
                            return Err(Failure::Dead);
                        }
                        let dead_env = close_channel_on_instance(&env).await;
                        match re_init_dead_env(false, dead_env).await {
                            Ok(new_env) => {
                                env = new_env;
                                attempt += 1;
                            }
                            Err(err) => return Err(err),
                        }
                    }
                }
            }
        }
    }
}

pub async fn init(settings: InitSettings) -> Result<(Env, BTreeSet<String>), String> {
    let on_retry = |attempt: u32, settings: InitSettings| async move {
        tokio::time::sleep(Duration::from_secs_f64(backoff_delay(attempt))).await;
        Ok(settings)
    };
    let res = with_retry(
        MAX_REINIT_ATTEMPTS,
        on_retry,
        |settings| re_init(None, settings),
        settings,
    )
    .await;
    match res {
        Ok(mut env) => match get_mergebase_and_changes(&env).await {
            Ok(mergebase_and_changes) => {
                let (mergebase, files) = match mergebase_and_changes {
                    Some((mergebase, changes)) => {
                        flow_hh_logger::info!(
                            "Watchman reports the initial mergebase as {:?}, and {} changes",
                            mergebase,
                            changes.len()
                        );
                        (Some(mergebase), changes)
                    }
                    None => {
                        if env.should_track_mergebase {
                            flow_hh_logger::warn!(
                                "Not checking changes since mergebase! SCM-aware queries are not supported for your VCS by your version of Watchman."
                            );
                        }
                        (None, BTreeSet::new())
                    }
                };
                env.mergebase = mergebase;
                Ok((env, files))
            }
            Err(_) => Err("Failed to query initial mergebase from Watchman".to_string()),
        },
        Err(_) => Err("Failed to initialize watchman".to_string()),
    }
}

pub mod testing {
    use super::*;

    pub type ErrorKindAlias = ErrorKind;

    pub fn test_settings() -> InitSettings {
        InitSettings {
            debug_logging: false,
            defer_states: vec![],
            expression_terms: vec![],
            mergebase_with: "hash".to_string(),
            roots: vec![PathBuf::from("/dummy")],
            should_track_mergebase: false,
            subscribe_mode: SubscribeMode::DeferChanges,
            subscription_prefix: "dummy_prefix".to_string(),
            sync_timeout: None,
        }
    }

    pub async fn get_test_env() -> Env {
        let (a, _b) = tokio::io::duplex(64);
        let (read_half, write_half) = tokio::io::split(a);
        let conn = Arc::new(Conn {
            reader: Mutex::new(BufReader::new(
                Box::new(read_half) as Box<dyn AsyncRead + Unpin + Send>
            )),
            writer: Mutex::new(Box::new(write_half) as Box<dyn AsyncWrite + Unpin + Send>),
        });
        Env {
            settings: test_settings(),
            conn,
            clockspec: String::new(),
            watch: Watch {
                watch_root: "/path/to/root".to_string(),
                watched_path_expression_terms: Some(j::pred(
                    "anyof",
                    vec![
                        j::strlist(&["dirname", "foo"]),
                        j::strlist(&["name", "foo"]),
                    ],
                )),
            },
            vcs: None,
            should_track_mergebase: false,
            mergebase: None,
            finished_an_hg_update: false,
            subscription: "dummy_prefix.123456789".to_string(),
        }
    }

    pub async fn transform_asynchronous_get_changes_response(
        env: &mut Env,
        json: &serde_json::Value,
    ) -> Result<PushedChanges, ErrorKindAlias> {
        super::transform_asynchronous_get_changes_response(env, json).await
    }
}
