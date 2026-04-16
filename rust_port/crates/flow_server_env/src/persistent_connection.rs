/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use flow_common_exit_status::FlowExitStatus;
use flow_common_utils::filename_cache;
use flow_services_inference_types::AutocompleteArtifacts;
use flow_services_inference_types::FileArtifacts;
use flow_services_inference_types::TypeContentsError;
use lsp_types::InitializeParams;

use crate::file_watcher_status;
use crate::flow_lsp_conversions;
use crate::lsp_handler;
use crate::lsp_helpers;
use crate::lsp_prot as Prot;
use crate::monitor_rpc;
use crate::server_prot::response::LazyStats;
use crate::server_status;

pub mod client_config {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ClientToggle {
        Default,
        True,
        False,
    }

    #[derive(Debug, Clone)]
    pub struct T {
        pub rank_autoimports_by_usage: ClientToggle,
        pub suggest_autoimports: bool,
        pub show_suggest_ranking_info: bool,
    }

    pub fn rank_autoimports_by_usage(t: &T) -> &ClientToggle {
        &t.rank_autoimports_by_usage
    }

    pub fn suggest_autoimports(t: &T) -> bool {
        t.suggest_autoimports
    }

    pub fn show_suggest_ranking_info(t: &T) -> bool {
        t.show_suggest_ranking_info
    }
}

#[derive(Clone)]
struct AutocompleteToken {
    ac_type: String,
    loc: (i32, i32, Option<flow_parser::file_key::FileKey>),
}

pub struct SingleClient {
    client_id: Prot::ClientId,
    generation: u64,
    type_parse_artifacts_cache:
        filename_cache::Cache<Result<FileArtifacts<'static>, TypeContentsError>>,
    autocomplete_artifacts_cache:
        filename_cache::Cache<Result<AutocompleteArtifacts<'static>, TypeContentsError>>,
    cache_epoch: u64,
}

pub type SingleClientRef = Rc<RefCell<SingleClient>>;

struct RegistryEntry {
    generation: u64,
    lsp_initialize_params: InitializeParams,
    subscribed: bool,
    opened_files: BTreeMap<String, String>,
    client_config: client_config::T,
    outstanding_handlers: HashMap<Prot::LspId, lsp_handler::UnitLspHandler>,
    autocomplete_token: Option<AutocompleteToken>,
    autocomplete_session_length: i32,
    pending_messages: VecDeque<Prot::MessageFromServer>,
}

#[derive(Debug, Clone)]
pub struct PersistentConnection(pub Vec<Prot::ClientId>);

const CACHE_MAX_SIZE: usize = 10;
const MAX_PENDING_MESSAGES: usize = 1024;

fn remove_cache_entry(autocomplete: bool, client: &mut SingleClient, filename: &str) {
    let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(filename);
    filename_cache::remove_entry(&file_key, &client.type_parse_artifacts_cache);
    if autocomplete {
        filename_cache::remove_entry(&file_key, &client.autocomplete_artifacts_cache)
    }
}

static ACTIVE_CLIENTS: LazyLock<Mutex<BTreeMap<Prot::ClientId, RegistryEntry>>> =
    LazyLock::new(|| Mutex::new(BTreeMap::new()));
static CACHE_CLEAR_EPOCH: AtomicU64 = AtomicU64::new(0);
static CLIENT_GENERATION: AtomicU64 = AtomicU64::new(0);

thread_local! {
    static LOCAL_CLIENTS: RefCell<BTreeMap<Prot::ClientId, SingleClientRef>> =
        const { RefCell::new(BTreeMap::new()) };
}

fn current_cache_epoch() -> u64 {
    CACHE_CLEAR_EPOCH.load(Ordering::SeqCst)
}

fn default_client_config() -> client_config::T {
    client_config::T {
        suggest_autoimports: true,
        rank_autoimports_by_usage: client_config::ClientToggle::Default,
        show_suggest_ranking_info: false,
    }
}

fn make_single_client(client_id: Prot::ClientId, generation: u64) -> SingleClient {
    SingleClient {
        client_id,
        generation,
        type_parse_artifacts_cache: filename_cache::make(CACHE_MAX_SIZE),
        autocomplete_artifacts_cache: filename_cache::make(CACHE_MAX_SIZE),
        cache_epoch: current_cache_epoch(),
    }
}

pub fn get_client(client_id: Prot::ClientId) -> Option<SingleClientRef> {
    let (generation, active_generations) = {
        let clients = ACTIVE_CLIENTS.lock().unwrap();
        let generation = clients.get(&client_id).map(|entry| entry.generation)?;
        let active_generations = clients
            .iter()
            .map(|(client_id, entry)| (*client_id, entry.generation))
            .collect::<BTreeMap<_, _>>();
        Some((generation, active_generations))
    }?;
    LOCAL_CLIENTS.with(|clients| {
        let mut clients = clients.borrow_mut();
        clients.retain(|cached_client_id, client| {
            active_generations
                .get(cached_client_id)
                .is_some_and(|generation| client.borrow().generation == *generation)
        });
        let client = match clients.get(&client_id) {
            Some(client) if client.borrow().generation == generation => client.clone(),
            Some(_) | None => {
                let client = Rc::new(RefCell::new(make_single_client(client_id, generation)));
                clients.insert(client_id, client.clone());
                client
            }
        };
        {
            let mut client = client.borrow_mut();
            let cache_epoch = current_cache_epoch();
            if client.cache_epoch != cache_epoch {
                filename_cache::clear(&client.type_parse_artifacts_cache);
                filename_cache::clear(&client.autocomplete_artifacts_cache);
                client.cache_epoch = cache_epoch;
            }
        }
        Some(client)
    })
}

fn with_registry<R>(client_id: Prot::ClientId, f: impl FnOnce(&RegistryEntry) -> R) -> Option<R> {
    let clients = ACTIVE_CLIENTS.lock().unwrap();
    clients.get(&client_id).map(f)
}

fn with_registry_mut<R>(
    client_id: Prot::ClientId,
    f: impl FnOnce(&mut RegistryEntry) -> R,
) -> Option<R> {
    let mut clients = ACTIVE_CLIENTS.lock().unwrap();
    clients.get_mut(&client_id).map(f)
}

pub fn empty() -> PersistentConnection {
    PersistentConnection(Vec::new())
}

pub fn all_clients() -> PersistentConnection {
    let clients = ACTIVE_CLIENTS
        .lock()
        .unwrap()
        .keys()
        .copied()
        .collect::<Vec<_>>();
    PersistentConnection(clients)
}

pub fn pop_message(client_id: Prot::ClientId) -> Result<Option<Prot::MessageFromServer>, String> {
    let mut clients = ACTIVE_CLIENTS.lock().unwrap();
    match clients.get_mut(&client_id) {
        Some(entry) => Ok(entry.pending_messages.pop_front()),
        None => Err(format!("Unknown persistent client {}", client_id)),
    }
}

pub fn has_client(client_id: Prot::ClientId) -> bool {
    ACTIVE_CLIENTS.lock().unwrap().contains_key(&client_id)
}

fn send_message_to_client(response: Prot::MessageFromServer, client: &SingleClientRef) {
    let client_id = client.borrow().client_id;
    let should_disconnect = {
        let mut clients = ACTIVE_CLIENTS.lock().unwrap();
        match clients.get_mut(&client_id) {
            Some(entry) if entry.pending_messages.len() >= MAX_PENDING_MESSAGES => true,
            Some(entry) => {
                entry.pending_messages.push_back(response.clone());
                false
            }
            None => return,
        }
    };
    if should_disconnect {
        log::warn!(
            "Persistent client #{} exceeded the pending message limit ({}). Disconnecting it.",
            client_id,
            MAX_PENDING_MESSAGES
        );
        remove_client(client_id);
        return;
    }
    monitor_rpc::respond_to_persistent_connection(client_id, response)
}

pub fn send_response(response: Prot::ResponseWithMetadata, client: &SingleClientRef) {
    send_message_to_client(Prot::MessageFromServer::RequestResponse(response), client)
}

fn send_notification(response: Prot::NotificationFromServer, client: &SingleClientRef) {
    send_message_to_client(
        Prot::MessageFromServer::NotificationFromServer(response),
        client,
    )
}

fn send_errors(
    errors_reason: Prot::ErrorsReason,
    errors: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    warnings: &BTreeMap<
        flow_parser::file_key::FileKey,
        flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    >,
    client: &SingleClientRef,
) {
    use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
    use flow_parser::file_key::FileKey;

    let client_id = get_id(client);
    let Some((opened_files, vscode_detailed_diagnostics)) = with_registry(client_id, |entry| {
        (
            entry.opened_files.clone(),
            entry
                .lsp_initialize_params
                .initialization_options
                .as_ref()
                .and_then(|opts| opts.get("detailedErrorRendering"))
                .and_then(|value| value.as_bool())
                .unwrap_or(false),
        )
    }) else {
        return;
    };

    fn get_first_contained(
        warn_map: &BTreeMap<FileKey, ConcreteLocPrintableErrorSet>,
        filenames: &[FileKey],
    ) -> ConcreteLocPrintableErrorSet {
        match filenames.split_first() {
            None => ConcreteLocPrintableErrorSet::new(),
            Some((filename, filenames)) => match warn_map.get(filename) {
                Some(errs) => errs.clone(),
                None => get_first_contained(warn_map, filenames),
            },
        }
    }

    let get_warnings_for_file = |filename: &str,
                                 warn_map: &BTreeMap<FileKey, ConcreteLocPrintableErrorSet>|
     -> ConcreteLocPrintableErrorSet {
        get_first_contained(
            warn_map,
            &[
                FileKey::source_file_of_absolute(filename),
                FileKey::lib_file_of_absolute(filename),
                FileKey::json_file_of_absolute(filename),
                FileKey::resource_file_of_absolute(filename),
            ],
        )
    };

    let warnings = opened_files.iter().fold(
        ConcreteLocPrintableErrorSet::new(),
        |mut warn_acc, (filename, _)| {
            let file_warns = get_warnings_for_file(filename, warnings);
            warn_acc.union(&file_warns);
            warn_acc
        },
    );
    let should_include_vscode_detailed_diagnostics =
        |error: &flow_common_errors::error_utils::PrintableError<flow_parser::loc::Loc>| -> bool {
            if vscode_detailed_diagnostics {
                let loc = flow_common_errors::error_utils::loc_of_printable_error(error);
                match &loc.source {
                    None => false,
                    Some(source) => opened_files.contains_key(source.as_str()),
                }
            } else {
                false
            }
        };
    let unsaved_content: flow_common_errors::error_utils::StdinFile = None;
    let diagnostics = flow_lsp_conversions::diagnostics_of_flow_errors(
        &unsaved_content,
        &should_include_vscode_detailed_diagnostics,
        errors,
        &warnings,
    );
    send_notification(
        Prot::NotificationFromServer::Errors {
            diagnostics,
            errors_reason,
        },
        client,
    )
}

pub fn send_errors_if_subscribed(
    client: &SingleClientRef,
    errors_reason: Prot::ErrorsReason,
    errors: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    warnings: &BTreeMap<
        flow_parser::file_key::FileKey,
        flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    >,
) {
    let subscribed = with_registry(get_id(client), |entry| entry.subscribed).unwrap_or(false);
    if subscribed {
        send_errors(errors_reason, errors, warnings, client)
    }
}

fn send_single_lsp(json: (Option<Prot::LspMessage>, Prot::Metadata), client: &SingleClientRef) {
    let (message, metadata) = json;
    send_response((Prot::Response::LspFromServer(message), metadata), client)
}

fn send_single_start_recheck(client: &SingleClientRef) {
    send_notification(Prot::NotificationFromServer::StartRecheck, client)
}

fn send_single_end_recheck(lazy_stats: LazyStats, client: &SingleClientRef) {
    send_notification(Prot::NotificationFromServer::EndRecheck(lazy_stats), client)
}

fn send_single_status(
    status: server_status::Status,
    watcher_status: file_watcher_status::Status,
    client: &SingleClientRef,
) {
    send_notification(
        Prot::NotificationFromServer::PleaseHold(status, watcher_status),
        client,
    )
}

fn send_single_server_exit(exit_status: FlowExitStatus, client: &SingleClientRef) {
    send_notification(
        Prot::NotificationFromServer::ServerExit(exit_status),
        client,
    )
}

fn send_single_telemetry(telemetry: Prot::TelemetryFromServer, client: &SingleClientRef) {
    send_notification(Prot::NotificationFromServer::Telemetry(telemetry), client)
}

pub fn add_client(client_id: Prot::ClientId, lsp_initialize_params: InitializeParams) -> bool {
    let mut active_clients = ACTIVE_CLIENTS.lock().unwrap();
    if active_clients.contains_key(&client_id) {
        return false;
    }
    let generation = CLIENT_GENERATION.fetch_add(1, Ordering::SeqCst) + 1;
    active_clients.insert(
        client_id,
        RegistryEntry {
            generation,
            lsp_initialize_params,
            subscribed: false,
            opened_files: BTreeMap::new(),
            client_config: default_client_config(),
            outstanding_handlers: HashMap::new(),
            autocomplete_token: None,
            autocomplete_session_length: 0,
            pending_messages: VecDeque::new(),
        },
    );
    log::info!("Adding new persistent connection #{}", client_id);
    true
}

pub fn remove_client(client_id: Prot::ClientId) {
    log::info!("Removing persistent connection client #{}", client_id);
    ACTIVE_CLIENTS.lock().unwrap().remove(&client_id);
    LOCAL_CLIENTS.with(|clients| {
        clients.borrow_mut().remove(&client_id);
    });
}

pub fn add_client_to_clients(
    clients: PersistentConnection,
    client_id: Prot::ClientId,
) -> PersistentConnection {
    let mut clients = clients
        .0
        .into_iter()
        .filter(|id| *id != client_id)
        .collect::<Vec<_>>();
    clients.insert(0, client_id);
    PersistentConnection(clients)
}

pub fn remove_client_from_clients(
    clients: PersistentConnection,
    client_id: Prot::ClientId,
) -> PersistentConnection {
    PersistentConnection(
        clients
            .0
            .into_iter()
            .filter(|id| *id != client_id)
            .collect(),
    )
}

fn get_subscribed_clients(clients: &PersistentConnection) -> Vec<SingleClientRef> {
    let mut acc = Vec::new();
    for client_id in &clients.0 {
        match get_client(*client_id) {
            Some(client)
                if with_registry(*client_id, |entry| entry.subscribed).unwrap_or(false) =>
            {
                acc.push(client)
            }
            _ => {}
        }
    }
    acc.reverse();
    acc
}

pub fn update_clients(
    clients: &PersistentConnection,
    errors_reason: Prot::ErrorsReason,
    calc_errors_and_warnings: impl FnOnce() -> (
        flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
        BTreeMap<
            flow_parser::file_key::FileKey,
            flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
        >,
    ),
) {
    let subscribed_clients = get_subscribed_clients(clients);
    let subscribed_client_count = subscribed_clients.len();
    let all_client_count = clients.0.len();
    if !subscribed_clients.is_empty() {
        let (errors, warnings) = calc_errors_and_warnings();
        let error_count = errors.cardinal();
        let warning_file_count = warnings.len();
        log::info!(
            "sending ({} errors) and (warnings from {} files) to {} subscribed clients (of {} total)",
            error_count,
            warning_file_count,
            subscribed_client_count,
            all_client_count
        );
        for client in &subscribed_clients {
            send_errors(errors_reason.clone(), &errors, &warnings, client);
        }
    }
}

pub fn send_lsp(clients: &PersistentConnection, json: (Option<Prot::LspMessage>, Prot::Metadata)) {
    for client in &get_subscribed_clients(clients) {
        send_single_lsp(json.clone(), client);
    }
}

pub fn send_start_recheck(clients: &PersistentConnection) {
    for client in &get_subscribed_clients(clients) {
        send_single_start_recheck(client);
    }
}

pub fn send_end_recheck(lazy_stats: LazyStats, clients: &PersistentConnection) {
    for client in &get_subscribed_clients(clients) {
        send_single_end_recheck(lazy_stats.clone(), client);
    }
}

pub fn send_status(
    status: server_status::Status,
    watcher_status: file_watcher_status::Status,
    clients: &PersistentConnection,
) {
    for client_id in &clients.0 {
        if let Some(client) = get_client(*client_id) {
            send_single_status(status.clone(), watcher_status.clone(), &client);
        }
    }
}

pub fn send_server_exit(exit_status: FlowExitStatus, clients: &PersistentConnection) {
    for client_id in &clients.0 {
        if let Some(client) = get_client(*client_id) {
            send_single_server_exit(exit_status, &client);
        }
    }
}

pub fn send_telemetry(telemetry: Prot::TelemetryFromServer, clients: &PersistentConnection) {
    for client_id in &clients.0 {
        if let Some(client) = get_client(*client_id) {
            send_single_telemetry(telemetry.clone(), &client);
        }
    }
}

pub fn subscribe_client(
    client: &SingleClientRef,
    current_errors: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    current_warnings: &BTreeMap<
        flow_parser::file_key::FileKey,
        flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    >,
) {
    log::info!("Subscribing client #{} to push diagnostics", get_id(client));
    let subscribed = with_registry(get_id(client), |entry| entry.subscribed).unwrap_or(false);
    if subscribed {
    } else {
        let errors_reason = Prot::ErrorsReason::NewSubscription;
        send_errors(errors_reason, current_errors, current_warnings, client);
        let _ = with_registry_mut(get_id(client), |entry| {
            entry.subscribed = true;
        });
    }
}

pub fn client_did_open(client: &SingleClientRef, files: &[(String, String)]) -> bool {
    let client_id = get_id(client);
    {
        let mut client = client.borrow_mut();
        match files.len() {
            1 => log::info!("Client #{} opened {}", client.client_id, files[0].0),
            len => log::info!("Client #{} opened {} files", client.client_id, len),
        }
        for (filename, _content) in files {
            remove_cache_entry(true, &mut client, filename);
        }
    }
    with_registry_mut(client_id, |entry| {
        let mut changed = false;
        for (filename, content) in files {
            match entry.opened_files.get(filename) {
                Some(existing_content) if existing_content == content => {}
                _ => {
                    entry.opened_files.insert(filename.clone(), content.clone());
                    changed = true;
                }
            }
        }
        changed
    })
    .unwrap_or(false)
}

pub fn client_did_change(
    client: &SingleClientRef,
    filename: &str,
    changes: &[lsp_types::TextDocumentContentChangeEvent],
) -> Result<(), (String, String)> {
    {
        let mut client = client.borrow_mut();
        remove_cache_entry(false, &mut client, filename);
    }
    let Some(content) = with_registry(get_id(client), |entry| {
        entry.opened_files.get(filename).cloned()
    })
    .flatten() else {
        return Err((
            format!("File {} wasn't open to change", filename),
            String::new(),
        ));
    };
    match lsp_helpers::apply_changes(&content, changes) {
        Err((reason, stack)) => Err((reason, stack)),
        Ok(new_content) => {
            let _ = with_registry_mut(get_id(client), |entry| {
                entry.opened_files.insert(filename.to_string(), new_content);
            });
            Ok(())
        }
    }
}

pub fn client_did_close(client: &SingleClientRef, filenames: &[String]) -> bool {
    let client_id = get_id(client);
    {
        let mut client = client.borrow_mut();
        match filenames.len() {
            1 => log::info!("Client #{} closed {}", client.client_id, filenames[0]),
            len => log::info!("Client #{} closed {} files", client.client_id, len),
        }
        for filename in filenames {
            remove_cache_entry(true, &mut client, filename);
        }
    }
    with_registry_mut(client_id, |entry| {
        let mut changed = false;
        for filename in filenames {
            changed |= entry.opened_files.remove(filename).is_some();
        }
        changed
    })
    .unwrap_or(false)
}

pub fn client_did_change_configuration(client: &SingleClientRef, new_config: client_config::T) {
    let client_id = get_id(client);
    let Some((logged_client_id, old_config)) =
        with_registry(client_id, |entry| (client_id, entry.client_config.clone()))
    else {
        return;
    };
    log::info!("Client #{} changed configuration", logged_client_id);

    let client_toggle_to_string = |client_toggle: &client_config::ClientToggle| -> &'static str {
        match client_toggle {
            client_config::ClientToggle::Default => "default",
            client_config::ClientToggle::True => "true",
            client_config::ClientToggle::False => "false",
        }
    };

    let old_suggest_autoimports = client_config::suggest_autoimports(&old_config);
    let new_suggest_autoimports = client_config::suggest_autoimports(&new_config);
    if new_suggest_autoimports != old_suggest_autoimports {
        log::info!(
            "  suggest_autoimports: {} -> {}",
            old_suggest_autoimports,
            new_suggest_autoimports
        );
    }

    let old_rank_autoimports_by_usage = client_config::rank_autoimports_by_usage(&old_config);
    let new_rank_autoimports_by_usage = client_config::rank_autoimports_by_usage(&new_config);
    if new_rank_autoimports_by_usage != old_rank_autoimports_by_usage {
        log::info!(
            "  rank_autoimports_by_usage: {} -> {}",
            client_toggle_to_string(old_rank_autoimports_by_usage),
            client_toggle_to_string(new_rank_autoimports_by_usage)
        );
    }

    let old_show_suggest_ranking_info = client_config::show_suggest_ranking_info(&old_config);
    let new_show_suggest_ranking_info = client_config::show_suggest_ranking_info(&new_config);
    if new_show_suggest_ranking_info != old_show_suggest_ranking_info {
        log::info!(
            "  show_suggest_ranking_info: {} -> {}",
            old_show_suggest_ranking_info,
            new_show_suggest_ranking_info
        );
    }

    let _ = with_registry_mut(client_id, |entry| {
        entry.client_config = new_config;
    });
}

pub fn get_file(
    client: &SingleClientRef,
    filename: &str,
) -> flow_server_utils::file_input::FileInput {
    use flow_server_utils::file_input::FileInput;

    match with_registry(get_id(client), |entry| {
        entry.opened_files.get(filename).cloned()
    })
    .flatten()
    {
        None => FileInput::FileName(filename.to_string()),
        Some(content) => FileInput::FileContent(Some(filename.to_string()), content),
    }
}

pub fn get_id(client: &SingleClientRef) -> Prot::ClientId {
    client.borrow().client_id
}

pub fn lsp_initialize_params(client: &SingleClientRef) -> InitializeParams {
    with_registry(get_id(client), |entry| entry.lsp_initialize_params.clone()).unwrap()
}

pub fn client_config(client: &SingleClientRef) -> client_config::T {
    with_registry(get_id(client), |entry| entry.client_config.clone()).unwrap()
}

pub fn type_parse_artifacts_cache(
    client: &SingleClientRef,
) -> filename_cache::Cache<Result<FileArtifacts<'static>, TypeContentsError>> {
    client.borrow().type_parse_artifacts_cache.clone()
}

pub fn autocomplete_artifacts_cache(
    client: &SingleClientRef,
) -> filename_cache::Cache<Result<AutocompleteArtifacts<'static>, TypeContentsError>> {
    client.borrow().autocomplete_artifacts_cache.clone()
}

pub fn clear_type_parse_artifacts_caches() {
    CACHE_CLEAR_EPOCH.fetch_add(1, Ordering::SeqCst);
    LOCAL_CLIENTS.with(|clients| {
        for client in clients.borrow().values() {
            let mut client = client.borrow_mut();
            filename_cache::clear(&client.type_parse_artifacts_cache);
            filename_cache::clear(&client.autocomplete_artifacts_cache);
            client.cache_epoch = current_cache_epoch();
        }
    });
}

pub fn push_outstanding_handler(
    client: &SingleClientRef,
    id: Prot::LspId,
    handler: lsp_handler::UnitLspHandler,
) {
    let _ = with_registry_mut(get_id(client), |entry| {
        entry.outstanding_handlers.insert(id, handler);
    });
}

pub fn pop_outstanding_handler(
    client: &SingleClientRef,
    id: &Prot::LspId,
) -> Option<lsp_handler::UnitLspHandler> {
    with_registry_mut(get_id(client), |entry| {
        entry.outstanding_handlers.remove(id)
    })
    .flatten()
}

pub fn autocomplete_session(
    client: &SingleClientRef,
    ac_type: &str,
    loc: (i32, i32, Option<flow_parser::file_key::FileKey>),
) -> i32 {
    with_registry_mut(get_id(client), |entry| {
        match &entry.autocomplete_token {
            Some(AutocompleteToken {
                ac_type: prev_ac_type,
                loc: prev_loc,
            }) if ac_type == prev_ac_type && &loc == prev_loc => {
                entry.autocomplete_session_length += 1;
            }
            _ => {
                entry.autocomplete_token = Some(AutocompleteToken {
                    ac_type: ac_type.to_string(),
                    loc: loc.clone(),
                });
                entry.autocomplete_session_length = 1;
            }
        }
        entry.autocomplete_session_length
    })
    .unwrap_or(0)
}
