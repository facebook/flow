/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)]

use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;

use dupe::Dupe;
use flow_common::flow_projects::FlowProjects;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_parser::loc_sig::LocSig;
use flow_server_env::flow_lsp_conversions;
use flow_server_env::lsp_handler;
use flow_server_env::lsp_helpers;
use flow_server_env::lsp_mapper;
use flow_server_env::lsp_mapper::LspMessage;
use flow_server_env::lsp_mapper::LspNotification;
use flow_server_env::lsp_mapper::LspRequest;
use flow_server_env::lsp_mapper::LspResult;
use flow_server_env::lsp_mapper::lsp_error;
use flow_server_env::lsp_prot;
use flow_server_env::monitor_prot;
use flow_server_env::persistent_connection;
use flow_server_env::server_command_with_context::ServerCommandWithContext;
use flow_server_env::server_env;
use flow_server_env::server_monitor_listener_state;
use flow_server_env::server_prot;
use flow_server_utils::file_input::FileInput;
use flow_services_autocomplete::autocomplete_service_js::ac_completion;
use flow_services_inference::type_contents::parse_contents;
use flow_services_inference::type_contents::printable_errors_of_file_artifacts_result;
use flow_services_inference::type_contents::type_parse_artifacts;
use flow_services_inference_types::AutocompleteArtifacts;
use flow_services_inference_types::FileArtifacts;
use flow_services_inference_types::ParseArtifacts;
use flow_services_inference_types::TypeContentsError;
use lsp_types::MessageType;

type CheckResult<'cx> = FileArtifacts<'cx>;
type FileArtifactsResult<'cx> = Result<CheckResult<'cx>, TypeContentsError>;
type AcArtifactsResult<'cx> = Result<AutocompleteArtifacts<'cx>, TypeContentsError>;
type TypeParseArtifactsCache =
    flow_common_utils::filename_cache::Cache<Result<FileArtifacts<'static>, TypeContentsError>>;
type AutocompleteArtifactsCache = flow_common_utils::filename_cache::Cache<
    Result<AutocompleteArtifacts<'static>, TypeContentsError>,
>;
type AcResult = Option<(
    flow_common::docblock::Docblock,
    Vec<(flow_parser::loc::Loc, String)>,
    Option<String>,
    Option<flow_parser::loc::Loc>,
    String,
    flow_services_autocomplete::autocomplete_service_js::AutocompleteServiceResult,
)>;
type AutocompleteResponse = Result<
    (
        Option<String>,
        flow_server_env::server_prot::response::completion::T,
        Option<flow_parser::loc::Loc>,
        flow_server_env::server_prot::response::AcType,
    ),
    String,
>;

pub const CHECKED_DEPENDENCIES_RETRY_SENTINEL: &str = "__flow_checked_dependencies_retry__";

#[derive(Debug)]
pub struct WorkloadCanceled;

static DID_OPEN_PENDING_FILES: LazyLock<Mutex<std::collections::BTreeMap<String, String>>> =
    LazyLock::new(|| Mutex::new(std::collections::BTreeMap::new()));
static URI_TO_LATEST_METADATA_MAP: LazyLock<
    Mutex<std::collections::BTreeMap<String, lsp_prot::Metadata>>,
> = LazyLock::new(|| Mutex::new(std::collections::BTreeMap::new()));

fn lsp_uri_to_flow_path(uri: &lsp_types::Url) -> String {
    let path = uri
        .to_file_path()
        .map(|path| path.to_string_lossy().to_string())
        .unwrap_or_else(|_| uri.path().to_string());
    match std::fs::canonicalize(&path) {
        Ok(path) => path.to_string_lossy().to_string(),
        Err(_) => path,
    }
}

fn lsp_document_identifier_to_flow_path(t: &lsp_types::TextDocumentIdentifier) -> String {
    lsp_uri_to_flow_path(&t.uri)
}

fn live_errors_mark_latest_metadata(uri: &str, metadata: &lsp_prot::Metadata) {
    let mut map = URI_TO_LATEST_METADATA_MAP.lock().unwrap();
    map.insert(uri.to_string(), metadata.clone());
}

fn is_latest_live_errors_metadata(uri: &str, metadata: &lsp_prot::Metadata) -> bool {
    let map = URI_TO_LATEST_METADATA_MAP.lock().unwrap();
    match map.get(uri) {
        Some(latest_metadata) => latest_metadata.start_wall_time == metadata.start_wall_time,
        None => false,
    }
}

fn loc_to_lsp_range(loc: &flow_parser::loc::Loc) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: loc.start.line.saturating_sub(1) as u32,
            character: loc.start.column.max(0) as u32,
        },
        end: lsp_types::Position {
            line: loc.end.line.saturating_sub(1) as u32,
            character: loc.end.column.max(0) as u32,
        },
    }
}

fn lsp_range_to_flow_loc(
    source: Option<flow_parser::file_key::FileKey>,
    range: &lsp_types::Range,
) -> flow_parser::loc::Loc {
    flow_parser::loc::Loc {
        source,
        start: flow_parser::loc::Position {
            line: range.start.line as i32 + 1,
            column: range.start.character as i32,
        },
        end: flow_parser::loc::Position {
            line: range.end.line as i32 + 1,
            column: range.end.character as i32,
        },
    }
}

use flow_common_tarjan::topsort;
use flow_parsing::docblock_parser::parse_docblock;
use flow_server_env::error_collator;
use flow_services_autocomplete::autocomplete_js;
use flow_services_export::export_search;
use flow_services_export::export_search::ExportSearch;
use flow_services_export::export_search::SearchOptions;
use flow_services_export::export_search_types::SearchResults;
use flow_services_get_def::get_def_js;
use flow_services_get_def::get_def_js::GetDefResult;
use flow_services_get_def::get_def_types::Purpose;

const DOCBLOCK_MAX_TOKENS: usize = 10;

// Mirrors OCaml's `Lwt.t` returning `(response, json_data) option` or raising
// `Lwt.Canceled`. The Err variant carries `WorkloadCanceled`, which the
// `run_command_in_parallel`/`run_command_in_serial` functions translate into
// either a workload deferral or an inline recheck-and-retry.
pub type EphemeralParallelizableResult =
    Result<(server_prot::response::Response, Option<lsp_prot::Json>), WorkloadCanceled>;

pub type EphemeralNonparallelizableResult =
    Result<(server_prot::response::Response, Option<lsp_prot::Json>), WorkloadCanceled>;

pub type PersistentParallelizableResult =
    Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled>;

pub type PersistentNonparallelizableResult =
    Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled>;

pub enum IdeFileError {
    Skipped(String),
    Failed(String),
}

pub enum CommandHandler {
    HandleImmediately,
    HandleParallelizable,
    HandleNonparallelizable,
}

type PersistentImmediateWorkload =
    Box<dyn FnOnce() -> (lsp_prot::Response, lsp_prot::Metadata) + Send>;
type PersistentParallelizableWorkload = Box<
    dyn FnOnce(
            &server_env::Env,
        ) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled>
        + Send,
>;
type PersistentNonparallelizableWorkload = Box<
    dyn FnOnce(
            &server_env::Env,
        ) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled>
        + Send,
>;
pub enum PersistentCommandHandler {
    HandlePersistentImmediately(PersistentImmediateWorkload),
    HandleParallelizablePersistent(PersistentParallelizableWorkload),
    HandleNonparallelizablePersistent(PersistentNonparallelizableWorkload),
}

fn type_parse_artifacts_with_cache(
    options: &Options,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    master_cx: Arc<flow_typing_context::MasterContext>,
    file: flow_parser::file_key::FileKey,
    artifacts: (
        Option<ParseArtifacts>,
        flow_typing_errors::flow_error::ErrorSet,
    ),
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
) -> (FileArtifactsResult<'static>, Option<bool>) {
    match type_parse_artifacts_cache {
        None => {
            let result = type_parse_artifacts(
                options,
                shared_mem,
                master_cx,
                file,
                artifacts,
                node_modules_containers,
            );
            (result, None)
        }
        Some(cache) => {
            let file_for_result = file.dupe();
            let (result, did_hit) = cache.with_cache_sync(
                |_| true,
                file,
                || {
                    type_parse_artifacts(
                        options,
                        shared_mem,
                        master_cx,
                        file_for_result,
                        artifacts,
                        node_modules_containers,
                    )
                },
            );
            (result, Some(did_hit))
        }
    }
}

fn add_cache_hit_data_to_json(
    mut json_props: Vec<(String, lsp_prot::Json)>,
    did_hit: Option<bool>,
) -> Vec<(String, lsp_prot::Json)> {
    match did_hit {
        None => json_props,
        Some(did_hit) => {
            json_props.insert(0, ("cached".to_string(), lsp_prot::Json::Bool(did_hit)));
            json_props
        }
    }
}

fn try_with<T, F: FnOnce() -> Result<T, String>>(f: F) -> Result<T, String> {
    f()
}

fn try_with_lwt<T, F: FnOnce() -> Result<T, String>>(f: F) -> Result<T, String> {
    f()
}

fn try_with_json<T, J, F: FnOnce() -> (Result<T, String>, Option<J>)>(
    f: F,
) -> (Result<T, String>, Option<J>) {
    f()
}

fn type_contents_error_to_string(
    error: &TypeContentsError,
    fallback: &str,
) -> Result<String, WorkloadCanceled> {
    match error {
        TypeContentsError::CheckedDependenciesCanceled => Err(WorkloadCanceled),
        TypeContentsError::Errors(_) => Ok(fallback.to_string()),
    }
}

fn is_checked_dependencies_retry_error(error: &str) -> bool {
    error == CHECKED_DEPENDENCIES_RETRY_SENTINEL
}

pub fn standalone_response_needs_checked_dependencies_retry(
    command: &server_prot::request::Command,
    response: &server_prot::response::Response,
) -> bool {
    match (command, response) {
        (
            server_prot::request::Command::AUTOCOMPLETE { .. },
            server_prot::response::Response::AUTOCOMPLETE(Err(error)),
        )
        | (
            server_prot::request::Command::AUTOFIX_EXPORTS { .. },
            server_prot::response::Response::AUTOFIX_EXPORTS(Err(error)),
        )
        | (
            server_prot::request::Command::AUTOFIX_MISSING_LOCAL_ANNOT { .. },
            server_prot::response::Response::AUTOFIX_MISSING_LOCAL_ANNOT(Err(error)),
        )
        | (
            server_prot::request::Command::COVERAGE { .. },
            server_prot::response::Response::COVERAGE(Err(error)),
        )
        | (
            server_prot::request::Command::DUMP_TYPES { .. },
            server_prot::response::Response::DUMP_TYPES(Err(error)),
        )
        | (
            server_prot::request::Command::GET_DEF { .. },
            server_prot::response::Response::GET_DEF(Err(error)),
        )
        | (
            server_prot::request::Command::INFER_TYPE(_),
            server_prot::response::Response::INFER_TYPE(Err(error)),
        )
        | (
            server_prot::request::Command::INSERT_TYPE { .. },
            server_prot::response::Response::INSERT_TYPE(Err(error)),
        )
        | (
            server_prot::request::Command::INLAY_HINT(_),
            server_prot::response::Response::INLAY_HINT(Err(error)),
        ) => is_checked_dependencies_retry_error(error),
        (
            server_prot::request::Command::AUTOFIX_EXPORTS { .. },
            server_prot::response::Response::AUTOFIX_EXPORTS(Ok((_patch, errors))),
        ) => errors
            .iter()
            .any(|error| is_checked_dependencies_retry_error(error)),
        (
            server_prot::request::Command::TYPE_OF_NAME(_),
            server_prot::response::Response::TYPE_OF_NAME(errors),
        ) => errors.iter().any(|error| {
            matches!(
                error,
                Err(error) if is_checked_dependencies_retry_error(error)
            )
        }),
        _ => false,
    }
}

fn status_log(errors: &ConcreteLocPrintableErrorSet) {
    if errors.is_empty() {
        log::info!("Status: OK");
    } else {
        log::info!("Status: Error");
    }
}

fn convert_errors(
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    options: &Options,
    errors: ConcreteLocPrintableErrorSet,
    warnings: ConcreteLocPrintableErrorSet,
    suppressed_errors: Vec<(
        flow_typing_errors::intermediate_error_types::IntermediateError<flow_aloc::ALoc>,
        std::collections::BTreeSet<flow_parser::loc::Loc>,
    )>,
) -> flow_server_env::server_prot::response::StatusResponse {
    if errors.is_empty() && warnings.is_empty() && suppressed_errors.is_empty() {
        flow_server_env::server_prot::response::StatusResponse::NO_ERRORS
    } else {
        let strip_root = if options.strip_root {
            Some(options.root.as_path())
        } else {
            None
        };
        let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
        let get_ast = |file: &flow_parser::file_key::FileKey| shared_mem.get_ast(file);
        let suppressed_errors: Vec<_> = suppressed_errors
            .into_iter()
            .map(|(e, loc_set)| {
                let printable = flow_typing_errors::intermediate_error::to_printable_error(
                    &loc_of_aloc,
                    get_ast,
                    strip_root,
                    e,
                );
                (printable, loc_set)
            })
            .collect();
        flow_server_env::server_prot::response::StatusResponse::ERRORS {
            errors,
            warnings,
            suppressed_errors,
        }
    }
}

fn json_of_parse_error<E: std::fmt::Display>(
    loc: &flow_parser::loc::Loc,
    err: &E,
) -> lsp_prot::Json {
    fn int(x: i32) -> lsp_prot::Json {
        lsp_prot::Json::Number(serde_json::Number::from(x))
    }
    fn position(p: &flow_parser::loc::Position) -> lsp_prot::Json {
        serde_json::json!({
            "line": int(p.line),
            "column": int(p.column),
        })
    }
    fn location(loc: &flow_parser::loc::Loc) -> lsp_prot::Json {
        serde_json::json!({
            "start": position(&loc.start),
            "end": position(&loc.end),
        })
    }
    serde_json::json!({
        "loc": location(loc),
        "message": err.to_string(),
    })
}

fn fold_json_of_parse_errors<E: std::fmt::Display>(
    parse_errors: &[(flow_parser::loc::Loc, E)],
    mut acc: Vec<(String, lsp_prot::Json)>,
) -> Vec<(String, lsp_prot::Json)> {
    match parse_errors.first() {
        Some((loc, err)) => {
            acc.insert(
                0,
                ("parse_error".to_string(), json_of_parse_error(loc, err)),
            );
            acc.insert(
                1,
                (
                    "parse_error_count".to_string(),
                    lsp_prot::Json::Number(serde_json::Number::from(parse_errors.len())),
                ),
            );
            acc
        }
        None => acc,
    }
}

fn file_input_of_text_document_identifier(
    client_id: lsp_prot::ClientId,
    t: &lsp_types::TextDocumentIdentifier,
) -> FileInput {
    let filename = lsp_document_identifier_to_flow_path(t);
    match persistent_connection::get_client(client_id) {
        Some(client) => persistent_connection::get_file(&client, &filename),
        None => FileInput::FileName(filename),
    }
}

fn file_input_of_text_document_identifier_opt(
    client_id: lsp_prot::ClientId,
    t: &lsp_types::TextDocumentIdentifier,
) -> Option<FileInput> {
    persistent_connection::get_client(client_id).map(|client| {
        let filename = lsp_document_identifier_to_flow_path(t);
        persistent_connection::get_file(&client, &filename)
    })
}

fn file_input_of_text_document_position(
    client_id: lsp_prot::ClientId,
    t: &lsp_types::TextDocumentPositionParams,
) -> FileInput {
    let text_document = &t.text_document;
    file_input_of_text_document_identifier(client_id, text_document)
}

fn file_input_of_text_document_position_opt(
    client_id: lsp_prot::ClientId,
    t: &lsp_types::TextDocumentPositionParams,
) -> Option<FileInput> {
    let text_document = &t.text_document;
    file_input_of_text_document_identifier_opt(client_id, text_document)
}

fn file_key_of_file_input_without_env(
    options: &Options,
    all_unordered_libs: &std::collections::BTreeSet<String>,
    file_input: &FileInput,
) -> flow_parser::file_key::FileKey {
    let file_options = &options.file_options;
    flow_common::files::filename_from_string(
        file_options,
        true,
        all_unordered_libs,
        file_input.filename_of_file_input(),
    )
}

fn file_key_of_file_input(
    options: &Options,
    env: &server_env::Env,
    file_input: &FileInput,
) -> flow_parser::file_key::FileKey {
    let all_unordered_libs: std::collections::BTreeSet<String> = env
        .all_unordered_libs
        .iter()
        .map(|s| s.to_string())
        .collect();
    file_key_of_file_input_without_env(options, &all_unordered_libs, file_input)
}

fn check_that_we_care_about_this_file(
    options: &Options,
    env: &server_env::Env,
    file_key: &flow_parser::file_key::FileKey,
    content: &str,
) -> Result<(), &'static str> {
    use flow_common::files;

    fn is_stdin(file_path: &str) -> bool {
        file_path == "-"
    }

    fn check_file_not_ignored(
        file_options: &flow_common::files::FileOptions,
        all_unordered_libs: &std::collections::BTreeSet<String>,
        file_path: &str,
    ) -> Result<(), &'static str> {
        if files::wanted(file_options, true, all_unordered_libs, file_path) {
            Ok(())
        } else {
            Err("File is ignored")
        }
    }

    fn check_file_included(
        options: &Options,
        file_options: &flow_common::files::FileOptions,
        file_path: &str,
    ) -> Result<(), &'static str> {
        let file_is_implicitly_included = {
            let root_str = format!(
                "{}{}",
                options.root.to_string_lossy(),
                std::path::MAIN_SEPARATOR
            );
            file_options.implicitly_include_root && file_path.starts_with(&root_str)
        };
        if file_is_implicitly_included || files::is_included(file_options, file_path) {
            Ok(())
        } else {
            Err("File is not implicitly or explicitly included")
        }
    }

    fn check_is_flow_file(
        file_options: &flow_common::files::FileOptions,
        file_path: &str,
    ) -> Result<(), &'static str> {
        if files::is_flow_file(file_options, file_path) {
            Ok(())
        } else {
            Err("File is not a Flow file")
        }
    }

    fn check_flow_pragma(
        options: &Options,
        _content: &str,
        file_key: &flow_parser::file_key::FileKey,
    ) -> Result<(), &'static str> {
        if options.all || file_key.is_lib_file() {
            Ok(())
        } else {
            let (_errs, docblock) = parse_docblock(
                DOCBLOCK_MAX_TOKENS,
                &options.file_options,
                file_key,
                _content,
            );
            if docblock.is_flow() {
                Ok(())
            } else {
                Err("File is missing @flow pragma and `all` is not set to `true`")
            }
        }
    }

    let file_path = file_key.to_absolute();
    if is_stdin(&file_path) || files::is_in_flowlib(&options.file_options, &file_path) {
        Ok(())
    } else {
        let file_path = files::imaginary_realpath(&file_path);
        let file_options = &options.file_options;
        let all_unordered_libs: std::collections::BTreeSet<String> = env
            .all_unordered_libs
            .iter()
            .map(|s| s.to_string())
            .collect();
        check_file_not_ignored(file_options, &all_unordered_libs, &file_path)
            .and_then(|()| check_file_included(options, file_options, &file_path))
            .and_then(|()| check_is_flow_file(file_options, &file_path))
            .and_then(|()| check_flow_pragma(options, content, file_key))
    }
}

fn json_props_of_skipped(reason: &str) -> Vec<(String, lsp_prot::Json)> {
    vec![
        ("skipped".to_string(), lsp_prot::Json::Bool(true)),
        (
            "skip_reason".to_string(),
            lsp_prot::Json::String(reason.to_string()),
        ),
    ]
}

fn json_of_skipped(reason: &str) -> Option<lsp_prot::Json> {
    Some(lsp_prot::Json::Object(
        json_props_of_skipped(reason).into_iter().collect(),
    ))
}

fn of_file_input(
    options: &Options,
    env: &server_env::Env,
    file_input: &FileInput,
) -> Result<(flow_parser::file_key::FileKey, String), IdeFileError> {
    let file_key = file_key_of_file_input(options, env, file_input);
    match file_input.content_of_file_input() {
        Err(msg) => Err(IdeFileError::Failed(msg)),
        Ok(file_contents) => {
            match check_that_we_care_about_this_file(options, env, &file_key, &file_contents) {
                Err(reason) => Err(IdeFileError::Skipped(reason.to_string())),
                Ok(()) => Ok((file_key, file_contents)),
            }
        }
    }
}

fn get_haste_module_info(
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    f: &flow_parser::file_key::FileKey,
) -> Option<flow_common_modulename::HasteModuleInfo> {
    shared_mem.get_haste_module_info(f)
}

fn mk_module_system_info(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
) -> flow_services_autocomplete::module_system_info::LspModuleSystemInfo {
    let node_resolver_root_relative_dirnames = if options.node_resolver_allow_root_relative {
        let root = options.root.to_string_lossy().to_string();
        options
            .node_resolver_root_relative_dirnames
            .iter()
            .map(|(prefix_opt, root_relative)| {
                (
                    prefix_opt.clone(),
                    flow_common::files::normalize_path(&root, root_relative),
                )
            })
            .collect()
    } else {
        vec![]
    };
    let shared_mem_clone = shared_mem.clone();
    let shared_mem_clone2 = shared_mem.clone();
    let shared_mem_clone3 = shared_mem.clone();
    let projects_options = options.projects_options.dupe();
    flow_services_autocomplete::module_system_info::LspModuleSystemInfo {
        file_options: options.file_options.dupe(),
        haste_module_system: options.module_system == flow_common::options::ModuleSystem::Haste,
        get_haste_module_info: Arc::new(move |f| shared_mem_clone.get_haste_module_info(f)),
        get_package_info: Box::new(move |f| {
            shared_mem_clone2
                .get_package_info(f)
                .map(|pkg| Ok((*pkg).clone()))
        }),
        is_package_file: Box::new(move |module_path, module_name| {
            let dependency = FlowProjects::from_path(
                &projects_options,
                &flow_parser::file_key::strip_project_root(module_path),
            )
            .and_then(|namespace| {
                shared_mem_clone3.get_dependency(&Modulename::Haste(HasteModuleInfo::mk(
                    module_name.into(),
                    namespace.to_bitset(),
                )))
            });
            match dependency.and_then(|dependency| shared_mem_clone3.get_provider(&dependency)) {
                Some(addr) => shared_mem_clone3.is_package_file(&addr),
                None => false,
            }
        }),
        node_resolver_root_relative_dirnames,
        resolves_to_real_path: Box::new(|from, to_real_path| {
            std::fs::canonicalize(from)
                .ok()
                .map(|p| p.to_string_lossy().to_string())
                == Some(to_real_path.to_string())
        }),
    }
}

fn get_status(
    options: &Options,
    env: &server_env::Env,
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
) -> (
    server_prot::response::StatusResponse,
    flow_server_env::server_prot::response::LazyStats,
) {
    let lazy_stats = flow_server_rechecker::rechecker::get_lazy_stats(options, env);
    let status_response = {
        let (errors, warnings, suppressed_errors) = {
            if options.include_suppressions {
                error_collator::get(env)
            } else {
                let (errors, warnings) = error_collator::get_without_suppressed(env);
                (
                    errors,
                    warnings,
                    Vec::<(
                        flow_typing_errors::intermediate_error_types::IntermediateError<
                            flow_aloc::ALoc,
                        >,
                        std::collections::BTreeSet<flow_parser::loc::Loc>,
                    )>::new(),
                )
            }
        };
        let warnings = if options.include_warnings {
            warnings
        } else {
            ConcreteLocPrintableErrorSet::new()
        };
        status_log(&errors);
        flow_event_logger::status_response(errors.cardinal() as i32);
        convert_errors(shared_mem, options, errors, warnings, suppressed_errors)
    };
    (status_response, lazy_stats)
}

fn autoimport_options(
    ac_options: &flow_services_autocomplete::autocomplete_service_js::AcOptions,
) -> SearchOptions {
    let mut opts = export_search::default_options();
    opts.max_results = 100;
    opts.num_threads = std::cmp::max(1, 4_usize.saturating_sub(2)); // num_cpus not available, using default
    opts.weighted = ac_options.imports_ranked_usage;
    opts
}

fn search_exported_values(
    ac_options: &flow_services_autocomplete::autocomplete_service_js::AcOptions,
    before: &str,
    exports: &mut ExportSearch,
) -> SearchResults {
    let options = autoimport_options(ac_options);
    export_search::search_values(Some(&options), before, exports)
}

fn search_exported_types(
    ac_options: &flow_services_autocomplete::autocomplete_service_js::AcOptions,
    before: &str,
    exports: &mut ExportSearch,
) -> SearchResults {
    let options = autoimport_options(ac_options);
    export_search::search_types(Some(&options), before, exports)
}

fn json_of_autocomplete_result(
    initial_json_props: Vec<(String, lsp_prot::Json)>,
    ac_result: AcResult,
) -> (AutocompleteResponse, Option<lsp_prot::Json>) {
    match ac_result {
        None => {
            let err_str = "Couldn't parse file in parse_contents".to_string();
            (
                Err(err_str.clone()),
                Some(lsp_prot::Json::Object(
                    vec![
                        (
                            "errors".to_string(),
                            lsp_prot::Json::Array(vec![lsp_prot::Json::String(err_str)]),
                        ),
                        (
                            "result".to_string(),
                            lsp_prot::Json::String("FAILURE_CHECK_CONTENTS".to_string()),
                        ),
                        (
                            "count".to_string(),
                            lsp_prot::Json::Number(serde_json::Number::from(0)),
                        ),
                    ]
                    .into_iter()
                    .chain(initial_json_props)
                    .collect(),
                )),
            )
        }
        Some((info, parse_errors, token_opt, ac_loc, ac_type_string, results_res)) => {
            let json_props_to_log: Vec<(String, lsp_prot::Json)> = vec![
                (
                    "ac_type".to_string(),
                    lsp_prot::Json::String(ac_type_string.clone()),
                ),
                (
                    "docblock".to_string(),
                    serde_json::to_value(&info).unwrap_or(serde_json::Value::Null),
                ),
                (
                    "token".to_string(),
                    match &token_opt {
                        None => lsp_prot::Json::Null,
                        Some(token) => lsp_prot::Json::String(token.clone()),
                    },
                ),
            ]
            .into_iter()
            .chain(initial_json_props)
            .collect();
            let (response, json_props_to_log) = match results_res {
                flow_services_autocomplete::autocomplete_service_js::AutocompleteServiceResultGeneric::AcResult(
                    flow_services_autocomplete::autocomplete_service_js::AcResult {
                        result,
                        errors_to_log,
                    },
                ) => {
                    let ac_completion::T { items, is_incomplete } = result;
                    let result = flow_server_env::server_prot::response::completion::T {
                        items: items
                            .into_iter()
                            .map(|item| {
                                let (documentation, tags) = item.documentation_and_tags;
                                server_prot::response::completion::CompletionItem {
                                    kind: item.kind,
                                    name: item.name,
                                    labelDetail: item.labelDetail,
                                    description: item.description,
                                    itemDetail: item.itemDetail,
                                    text_edit: item.text_edit.map(|text_edit| {
                                        server_prot::response::InsertReplaceEdit {
                                            newText: text_edit.newText,
                                            insert: text_edit.insert,
                                            replace: text_edit.replace,
                                        }
                                    }),
                                    additional_text_edits: item.additional_text_edits,
                                    sort_text: item.sort_text,
                                    preselect: item.preselect,
                                    documentation,
                                    tags,
                                    log_info: item.log_info,
                                    insert_text_format: item.insert_text_format,
                                }
                            })
                            .collect(),
                        is_incomplete,
                    };
                    let result_string = if errors_to_log.is_empty() {
                        "SUCCESS"
                    } else if result.items.is_empty() {
                        "FAILURE"
                    } else {
                        "PARTIAL"
                    };
                    let at_least_one_result_has_documentation = result
                        .items
                        .iter()
                        .any(|server_prot::response::completion::CompletionItem {
                                 documentation,
                                 ..
                             }| documentation.is_some());
                    (
                        Ok((token_opt, result.clone(), ac_loc, ac_type_string)),
                        vec![
                            (
                                "result".to_string(),
                                lsp_prot::Json::String(result_string.to_string()),
                            ),
                            (
                                "count".to_string(),
                                lsp_prot::Json::Number(serde_json::Number::from(result.items.len())),
                            ),
                            (
                                "errors".to_string(),
                                lsp_prot::Json::Array(
                                    errors_to_log
                                        .into_iter()
                                        .map(lsp_prot::Json::String)
                                        .collect(),
                                ),
                            ),
                            (
                                "documentation".to_string(),
                                lsp_prot::Json::Bool(at_least_one_result_has_documentation),
                            ),
                        ]
                        .into_iter()
                        .chain(json_props_to_log)
                        .collect(),
                    )
                }
                flow_services_autocomplete::autocomplete_service_js::AutocompleteServiceResultGeneric::AcEmpty(
                    reason,
                ) => (
                    Ok((
                        token_opt,
                        server_prot::response::completion::T {
                            items: vec![],
                            is_incomplete: false,
                        },
                        ac_loc,
                        ac_type_string,
                    )),
                    vec![
                        (
                            "result".to_string(),
                            lsp_prot::Json::String("SUCCESS".to_string()),
                        ),
                        (
                            "count".to_string(),
                            lsp_prot::Json::Number(serde_json::Number::from(0)),
                        ),
                        (
                            "empty_reason".to_string(),
                            lsp_prot::Json::String(reason),
                        ),
                    ]
                    .into_iter()
                    .chain(json_props_to_log)
                    .collect(),
                ),
                flow_services_autocomplete::autocomplete_service_js::AutocompleteServiceResultGeneric::AcFatalError(
                    error,
                ) => (
                    Err(error.clone()),
                    vec![
                        (
                            "result".to_string(),
                            lsp_prot::Json::String("FAILURE".to_string()),
                        ),
                        (
                            "errors".to_string(),
                            lsp_prot::Json::Array(vec![lsp_prot::Json::String(error)]),
                        ),
                    ]
                    .into_iter()
                    .chain(json_props_to_log)
                    .collect(),
                ),
            };
            let json_props_to_log = fold_json_of_parse_errors(&parse_errors, json_props_to_log);
            (
                response,
                Some(lsp_prot::Json::Object(
                    json_props_to_log.into_iter().collect(),
                )),
            )
        }
    }
}

fn type_parse_artifacts_for_ac_with_cache(
    options: &Options,
    type_parse_artifacts_cache: Option<&AutocompleteArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    master_cx: Arc<flow_typing_context::MasterContext>,
    file: flow_parser::file_key::FileKey,
    contents: String,
    artifacts: (
        Option<ParseArtifacts>,
        flow_typing_errors::flow_error::ErrorSet,
    ),
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
) -> (AcArtifactsResult<'static>, Option<bool>) {
    let file_for_result = file.dupe();
    let type_parse_artifacts = || match artifacts {
        (None, errs) => Err(TypeContentsError::Errors(errs)),
        (Some(parse_artifacts), _errs) => {
            let ParseArtifacts {
                ref docblock,
                docblock_errors: _,
                ref ast,
                ref requires,
                ref file_sig,
                tolerable_errors: _,
                parse_errors: _,
            } = parse_artifacts;
            let (cx, aloc_ast) =
                match flow_services_inference::type_contents::compute_env_of_contents(
                    options,
                    shared_mem.clone(),
                    master_cx.clone(),
                    file_for_result.dupe(),
                    Arc::new(docblock.clone()),
                    Arc::new(ast.clone()),
                    requires,
                    file_sig.dupe(),
                    node_modules_containers,
                ) {
                    Ok(Ok(v)) => v,
                    Ok(Err(_)) | Err(_) => {
                        return Err(TypeContentsError::CheckedDependenciesCanceled);
                    }
                };
            Ok((contents.clone(), parse_artifacts, cx, aloc_ast))
        }
    };
    let cond = |result: &AcArtifactsResult<'static>| match result {
        Err(_) => false,
        Ok((contents_prime, _, _, _)) => contents == *contents_prime,
    };
    match type_parse_artifacts_cache {
        Some(cache) => {
            let (result, did_hit) = cache.with_cache_sync(cond, file, type_parse_artifacts);
            (result, Some(did_hit))
        }
        _ => (type_parse_artifacts(), None),
    }
}

fn autocomplete_on_parsed(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: Option<lsp_prot::ClientId>,
    filename: &flow_parser::file_key::FileKey,
    contents: &str,
    trigger_character: Option<&str>,
    cursor: (i32, i32),
    imports: bool,
    imports_min_characters: i32,
    imports_ranked_usage: bool,
    imports_ranked_usage_boost_exact_match_min_length: usize,
    show_ranking_info: bool,
) -> Result<(Vec<(String, lsp_prot::Json)>, AcResult), WorkloadCanceled> {
    let (line, column) = cursor;
    let cursor_loc = flow_parser::loc::Loc::cursor(Some(filename.dupe()), line, column);
    let (contents, broader_context, canon_token) =
        flow_services_autocomplete::autocomplete_sigil::add(
            Some(filename),
            contents,
            line,
            column as usize,
        );
    let canon_cursor = match &canon_token {
        Some(token) => {
            flow_services_autocomplete::autocomplete_sigil::canonical::cursor(token).clone()
        }
        None => cursor_loc.clone(),
    };
    autocomplete_js::autocomplete_set_hooks(&canon_cursor);
    let initial_json_props: Vec<(String, lsp_prot::Json)> = vec![
        (
            "ac_trigger".to_string(),
            lsp_prot::Json::String(trigger_character.unwrap_or("None").to_string()),
        ),
        (
            "broader_context".to_string(),
            lsp_prot::Json::String(broader_context),
        ),
    ];
    let type_parse_artifacts_cache = client_id
        .and_then(persistent_connection::get_client)
        .map(|client| persistent_connection::autocomplete_artifacts_cache(&client));
    let parse_result = parse_contents(options, &contents, filename);
    let (file_artifacts_result, did_hit) = type_parse_artifacts_for_ac_with_cache(
        options,
        type_parse_artifacts_cache.as_ref(),
        shared_mem.clone(),
        env.master_cx.clone(),
        filename.dupe(),
        contents.clone(),
        parse_result,
        node_modules_containers,
    );
    let checked_dependencies_canceled = matches!(
        file_artifacts_result,
        Err(TypeContentsError::CheckedDependenciesCanceled)
    );
    let initial_json_props = add_cache_hit_data_to_json(initial_json_props, did_hit);
    let ac_typing_artifacts = match file_artifacts_result {
        Err(_) => None,
        Ok((_contents, parse_artifacts, cx, aloc_ast)) => {
            let ParseArtifacts {
                docblock: _info,
                docblock_errors: _,
                ast,
                requires: _,
                file_sig,
                tolerable_errors: _,
                parse_errors: _parse_errors,
            } = parse_artifacts;
            Some((_info, file_sig, ast, _parse_errors, cx, aloc_ast))
        }
    };
    let ac_result = match ac_typing_artifacts {
        None => None,
        Some((info, file_sig, ast, parse_errors, cx, aloc_ast)) => {
            let search_exported_values_fn: Box<
                dyn Fn(
                    &flow_services_autocomplete::autocomplete_service_js::AcOptions,
                    &str,
                ) -> SearchResults,
            > = match &env.exports {
                Some(exports) => {
                    let exports = std::cell::RefCell::new(exports.clone());
                    Box::new(move |ac_options, before| {
                        search_exported_values(ac_options, before, &mut exports.borrow_mut())
                    })
                }
                None => Box::new(|_ac_options, _before| {
                    flow_services_export::export_search_types::empty_search_results()
                }),
            };
            let search_exported_types_fn: Box<
                dyn Fn(
                    &flow_services_autocomplete::autocomplete_service_js::AcOptions,
                    &str,
                ) -> SearchResults,
            > = match &env.exports {
                Some(exports) => {
                    let exports = std::cell::RefCell::new(exports.clone());
                    Box::new(move |ac_options, before| {
                        search_exported_types(ac_options, before, &mut exports.borrow_mut())
                    })
                }
                None => Box::new(|_ac_options, _before| {
                    flow_services_export::export_search_types::empty_search_results()
                }),
            };
            let layout_options =
                flow_services_code_action::code_action_utils::layout_options(options);
            let module_system_info = mk_module_system_info(options, shared_mem.clone());
            let shared_mem_loa = shared_mem.clone();
            let loc_of_aloc: Box<dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc> =
                Box::new(move |aloc| shared_mem_loa.loc_of_aloc(aloc));
            let shared_mem_ast = shared_mem.clone();
            let get_ast_from_shared_mem: Box<
                dyn Fn(
                    &flow_parser::file_key::FileKey,
                ) -> Option<
                    flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
                >,
            > = Box::new(move |f| shared_mem_ast.get_ast(f).map(|a| (*a).clone()));
            let canon_token_owned = canon_token;
            let typing = flow_services_autocomplete::autocomplete_service_js::mk_typing_artifacts(
                &layout_options,
                loc_of_aloc,
                get_ast_from_shared_mem,
                &module_system_info,
                &*search_exported_values_fn,
                &*search_exported_types_fn,
                &cx,
                file_sig,
                ast,
                aloc_ast,
                canon_token_owned.as_ref(),
            );
            let ac_options = flow_services_autocomplete::autocomplete_service_js::AcOptions {
                imports,
                imports_min_characters,
                imports_ranked_usage,
                imports_ranked_usage_boost_exact_match_min_length,
                show_ranking_info,
            };
            // LSP boundary: convert any JobError (cancel/timeout) escaping the
            // autocomplete pipeline into "no autocomplete suggestion" (an empty
            // AcResult) exactly once. The cascade itself stays Result-typed.
            let (token_opt, ac_loc, ac_type_string, results_res) =
                match flow_services_autocomplete::autocomplete_service_js::autocomplete_get_results(
                    &typing,
                    &ac_options,
                    trigger_character,
                    &cursor_loc,
                ) {
                    Ok(tuple) => tuple,
                    Err(_job_err) => (
                        None,
                        None,
                        "None".to_string(),
                        flow_services_autocomplete::autocomplete_service_js::AutocompleteServiceResultGeneric::AcResult(
                            flow_services_autocomplete::autocomplete_service_js::AcResult {
                                result: ac_completion::T {
                                    items: Vec::new(),
                                    is_incomplete: false,
                                },
                                errors_to_log: vec![
                                    "no autocomplete suggestion".to_string(),
                                ],
                            },
                        ),
                    ),
                };
            let parse_errors = parse_errors
                .into_iter()
                .map(|(loc, err)| (loc, err.to_string()))
                .collect();
            Some((
                info,
                parse_errors,
                token_opt,
                ac_loc,
                ac_type_string,
                results_res,
            ))
        }
    };
    autocomplete_js::autocomplete_unset_hooks();
    if checked_dependencies_canceled {
        return Err(WorkloadCanceled);
    }
    Ok((initial_json_props, ac_result))
}

fn autofix_errors_cli(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    include_best_effort_fix: bool,
    input: &FileInput,
) -> server_prot::response::ApplyCodeActionResponse {
    let file_key = file_key_of_file_input(options, env, input);
    let file_content = input.content_of_file_input()?;
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc: Arc<dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc> =
        Arc::new(move |aloc| shared_mem_loa.loc_of_aloc(aloc));
    let shared_mem_ast = shared_mem.clone();
    let get_ast_from_shared_mem: Arc<
        dyn Fn(
            &flow_parser::file_key::FileKey,
        )
            -> Option<flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>>,
    > = Arc::new(move |f| shared_mem_ast.get_ast(f).map(|a| (*a).clone()));
    let module_system_info = mk_module_system_info(options, shared_mem.clone());
    let shared_mem_ts = shared_mem.clone();
    let get_type_sig: Arc<
        dyn Fn(
            &flow_parser::file_key::FileKey,
        ) -> Option<
            flow_type_sig::packed_type_sig::Module<
                flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
            >,
        >,
    > = Arc::new(move |f| {
        shared_mem_ts.get_type_sig(f).map(|arc| {
            let bytes = bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                .expect("get_type_sig: serialize");
            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                .expect("get_type_sig: deserialize")
                .0
        })
    });
    let edits = flow_services_code_action::code_action_service::autofix_errors_cli(
        options,
        env,
        shared_mem,
        node_modules_containers,
        loc_of_aloc,
        get_ast_from_shared_mem,
        &module_system_info,
        get_type_sig,
        include_best_effort_fix,
        &file_key,
        &file_content,
    )?;
    Ok(flow_parser_utils_output::replacement_printer::loc_patch_to_patch(&file_content, &edits))
}

fn autofix_imports_cli(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> server_prot::response::ApplyCodeActionResponse {
    let file_key = file_key_of_file_input(options, env, input);
    let file_content = input.content_of_file_input()?;
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc = move |aloc: &flow_aloc::ALoc| shared_mem_loa.loc_of_aloc(aloc);
    let module_system_info = mk_module_system_info(options, shared_mem.clone());
    let edits = flow_services_code_action::code_action_service::autofix_imports_cli(
        options,
        env,
        shared_mem,
        node_modules_containers,
        &loc_of_aloc,
        &module_system_info,
        &file_key,
        &file_content,
    )?;
    Ok(flow_parser_utils_output::replacement_printer::loc_patch_to_patch(&file_content, &edits))
}

fn suggest_imports_cli(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> server_prot::response::SuggestImportsResponse {
    let file_key = file_key_of_file_input(options, env, input);
    let file_content = input.content_of_file_input()?;
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc = move |aloc: &flow_aloc::ALoc| shared_mem_loa.loc_of_aloc(aloc);
    let module_system_info = mk_module_system_info(options, shared_mem.clone());
    let result = flow_services_code_action::code_action_service::suggest_imports_cli(
        options,
        env,
        shared_mem,
        node_modules_containers,
        &loc_of_aloc,
        &module_system_info,
        &file_key,
        &file_content,
    )?;
    let map: std::collections::BTreeMap<String, Vec<lsp_types::CodeActionOrCommand>> =
        result.into_iter().collect();
    Ok(serde_json::to_string(&map).expect("suggest_imports_cli: serialize"))
}

fn autocomplete(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: Option<lsp_prot::ClientId>,
    input: &FileInput,
    trigger_character: Option<&str>,
    cursor: (i32, i32),
    imports: bool,
    imports_ranked_usage: bool,
    show_ranking_info: bool,
) -> Result<(AutocompleteResponse, Option<lsp_prot::Json>), WorkloadCanceled> {
    match of_file_input(options, env, input) {
        Err(IdeFileError::Failed(e)) => Ok((Err(e), None)),
        Err(IdeFileError::Skipped(reason)) => {
            let response = (
                None,
                server_prot::response::completion::T {
                    items: vec![],
                    is_incomplete: false,
                },
                None,
                "Skipped".to_string(),
            );
            let extra_data = json_of_skipped(&reason);
            Ok((Ok(response), extra_data))
        }
        Ok((filename, contents)) => {
            let imports_min_characters = options.autoimports_min_characters;
            let imports_ranked_usage_boost_exact_match_min_length =
                options.autoimports_ranked_by_usage_boost_exact_match_min_length as usize;
            let (initial_json_props, ac_result) = autocomplete_on_parsed(
                options,
                env,
                shared_mem,
                node_modules_containers,
                client_id,
                &filename,
                &contents,
                trigger_character,
                cursor,
                imports,
                imports_min_characters,
                imports_ranked_usage,
                imports_ranked_usage_boost_exact_match_min_length,
                show_ranking_info,
            )?;
            Ok(json_of_autocomplete_result(initial_json_props, ac_result))
        }
    }
}

enum ErrorsOfFileError {
    NotCovered,
    Canceled,
}

fn errors_of_file(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    force: bool,
    file_input: &FileInput,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
) -> Result<(ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet), ErrorsOfFileError> {
    let mut options = options.clone();
    options.all = options.all || force;
    match of_file_input(&options, env, file_input) {
        Err(IdeFileError::Failed(_)) | Err(IdeFileError::Skipped(_)) => {
            Err(ErrorsOfFileError::NotCovered)
        }
        Ok((file_key, content)) => {
            let intermediate_result = parse_contents(&options, &content, &file_key);
            let result = if !intermediate_result.1.is_empty() {
                Err(TypeContentsError::Errors(intermediate_result.1))
            } else {
                type_parse_artifacts(
                    &options,
                    shared_mem.clone(),
                    env.master_cx.clone(),
                    file_key.clone(),
                    intermediate_result,
                    node_modules_containers,
                )
            };
            if matches!(result, Err(TypeContentsError::CheckedDependenciesCanceled)) {
                return Err(ErrorsOfFileError::Canceled);
            }
            let (errors, warnings) = printable_errors_of_file_artifacts_result(
                &options,
                env,
                &shared_mem,
                &file_key,
                result.as_ref(),
            );
            Ok((errors, warnings))
        }
    }
}

fn check_file(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    force: bool,
    file_input: &FileInput,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
) -> Result<server_prot::response::StatusResponse, WorkloadCanceled> {
    match errors_of_file(
        options,
        env,
        shared_mem.clone(),
        force,
        file_input,
        node_modules_containers,
    ) {
        Err(ErrorsOfFileError::NotCovered) => {
            Ok(server_prot::response::StatusResponse::NOT_COVERED)
        }
        // OCaml: `Lwt.Canceled` propagates out of `check_file`. The Rust port
        // surfaces it via the typed Err so that the workload wrapper can defer.
        Err(ErrorsOfFileError::Canceled) => Err(WorkloadCanceled),
        Ok((errors, warnings)) => Ok(convert_errors(
            &shared_mem,
            options,
            errors,
            warnings,
            Vec::<(
                flow_typing_errors::intermediate_error_types::IntermediateError<flow_aloc::ALoc>,
                std::collections::BTreeSet<flow_parser::loc::Loc>,
            )>::new(),
        )),
    }
}

fn get_def_of_check_result(
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    file: &flow_parser::file_key::FileKey,
    line: u32,
    col: u32,
    check_result: &CheckResult<'_>,
    file_content: Option<&str>,
    purpose: &flow_services_get_def::get_def_types::Purpose,
) -> (
    server_prot::response::GetDefResponse,
    Option<Vec<(String, lsp_prot::Json)>>,
) {
    let loc = flow_parser::loc::Loc::cursor(Some(file.clone()), line as i32, col as i32);
    let (ref parse_artifacts, ref typecheck_artifacts) = *check_result;
    let result = match get_def_js::get_def(
        &|aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc),
        &typecheck_artifacts.cx,
        &parse_artifacts.file_sig,
        file_content,
        &parse_artifacts.ast,
        flow_typing_utils::typed_ast_utils::AvailableAst::TypedAst(
            typecheck_artifacts.typed_ast.clone(),
        ),
        purpose,
        &loc,
    ) {
        Ok(r) => r,
        Err(_canceled) => GetDefResult::DefError("Worker canceled".to_string()),
    };
    let json_props = fold_json_of_parse_errors(&parse_artifacts.parse_errors, vec![]);
    match result {
        GetDefResult::Def(locs, _) => (
            Ok(locs.into_iter().collect()),
            Some(
                vec![(
                    "result".to_string(),
                    lsp_prot::Json::String("SUCCESS".to_string()),
                )]
                .into_iter()
                .chain(json_props)
                .collect(),
            ),
        ),
        GetDefResult::Partial(locs, _, msg) => (
            Ok(locs.into_iter().collect()),
            Some(
                vec![
                    (
                        "result".to_string(),
                        lsp_prot::Json::String("PARTIAL_FAILURE".to_string()),
                    ),
                    ("error".to_string(), lsp_prot::Json::String(msg)),
                ]
                .into_iter()
                .chain(json_props)
                .collect(),
            ),
        ),
        GetDefResult::BadLoc(msg) => (
            Ok(vec![]),
            Some(
                vec![
                    (
                        "result".to_string(),
                        lsp_prot::Json::String("BAD_LOC".to_string()),
                    ),
                    ("error".to_string(), lsp_prot::Json::String(msg)),
                ]
                .into_iter()
                .chain(json_props)
                .collect(),
            ),
        ),
        GetDefResult::DefError(msg) => (
            Err(msg.clone()),
            Some(
                vec![
                    (
                        "result".to_string(),
                        lsp_prot::Json::String("FAILURE".to_string()),
                    ),
                    ("error".to_string(), lsp_prot::Json::String(msg)),
                ]
                .into_iter()
                .chain(json_props)
                .collect(),
            ),
        ),
    }
}

fn infer_type_to_response(
    json: bool,
    expanded: bool,
    exact_by_default: bool,
    ts_syntax: bool,
    strip_root: Option<&std::path::Path>,
    loc: flow_parser::loc::Loc,
    refining_locs: Vec<flow_parser::loc::Loc>,
    refinement_invalidated: Vec<(
        flow_parser::loc::Loc,
        flow_common::refinement_invalidation::Reason,
    )>,
    documentation: Option<String>,
    tys: Option<flow_common_ty::ty::TypeAtPosResult>,
    loc_of_aloc: &dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc,
) -> server_prot::response::infer_type::T {
    let converter = flow_common_ty::ty_debug::AlocToLocFn::new(loc_of_aloc);
    let tys = if json {
        let printer_opts = flow_common_ty::ty_printer::PrinterOptions {
            exact_by_default,
            ts_syntax,
            ..Default::default()
        };
        // The "expanded" field carries `Hh_json` output that may contain duplicate
        // keys (`json_of_utility` adds a second `"kind"` to the outer wrapper).
        // `serde_json::Map` deduplicates, so we use `Box<RawValue>` for that field
        // and `ty_debug::json_of_elt_raw` to keep the duplicates intact.
        #[derive(serde::Serialize)]
        struct InnerType {
            #[serde(skip_serializing_if = "Option::is_none")]
            expanded: Option<Box<serde_json::value::RawValue>>,
            #[serde(rename = "type")]
            type_: String,
        }
        #[derive(serde::Serialize)]
        struct TypesPayload {
            evaluated: Option<InnerType>,
            unevaluated: InnerType,
        }
        let json_string: String = match &tys {
            Some(result) => {
                let type_json = |elt: &flow_common_ty::ty::ALocElt| -> InnerType {
                    InnerType {
                        expanded: if expanded {
                            Some(
                                flow_common_ty::ty_debug::json_of_elt_raw::<flow_aloc::ALoc>(
                                    &converter, elt, strip_root,
                                ),
                            )
                        } else {
                            None
                        },
                        type_: flow_common_ty::ty_printer::string_of_elt_single_line(
                            elt,
                            &printer_opts,
                        ),
                    }
                };
                let payload = TypesPayload {
                    evaluated: result.evaluated.as_ref().map(&type_json),
                    unevaluated: type_json(&result.unevaluated),
                };
                serde_json::to_string(&payload).expect("infer_type: serialize TypesPayload")
            }
            None => "null".to_string(),
        };
        server_prot::response::infer_type::Payload::Json(json_string)
    } else {
        let printer_opts = flow_common_ty::ty_printer::PrinterOptions {
            exact_by_default,
            ts_syntax,
            ..Default::default()
        };
        server_prot::response::infer_type::Payload::Friendly(tys.map(|r| {
            let (type_str, refs) = flow_common_ty::ty_printer::string_of_type_at_pos_result(
                &r.unevaluated,
                &r.evaluated,
                &r.refs,
                &printer_opts,
            );
            server_prot::response::infer_type::FriendlyResponse { type_str, refs }
        }))
    };
    server_prot::response::infer_type::T {
        loc,
        tys,
        refining_locs,
        refinement_invalidated,
        documentation,
    }
}

fn documentation_at_loc(
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    file_key: &flow_parser::file_key::FileKey,
    line: u32,
    column: u32,
    check_result: &CheckResult<'_>,
    ast: &flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
) -> Option<String> {
    let (getdef_loc_result, _) = try_with_json(|| {
        let (result, _json_props) = get_def_of_check_result(
            shared_mem,
            file_key,
            line,
            column,
            check_result,
            None,
            &Purpose::JSDoc,
        );
        (result, None::<lsp_prot::Json>)
    });
    let get_def_documentation = match getdef_loc_result {
        Ok(ref locs) if locs.len() == 1 => {
            let getdef_loc = locs.first().unwrap().clone();
            flow_services_autocomplete::find_documentation::jsdoc_of_getdef_loc(
                ast,
                &|file_key| shared_mem.get_ast(file_key).map(|a| (*a).clone()),
                getdef_loc,
            )
            .as_ref()
            .and_then(flow_services_autocomplete::find_documentation::documentation_of_jsdoc)
        }
        _ => None,
    };
    let hardcoded_documentation =
        flow_services_autocomplete::find_documentation::hardcoded_documentation_at_loc(
            ast,
            flow_parser::loc::Loc::cursor(Some(file_key.clone()), line as i32, column as i32),
        );
    match (get_def_documentation, hardcoded_documentation) {
        (None, None) => None,
        (Some(d), None) | (None, Some(d)) => Some(d),
        (Some(d1), Some(d2)) => Some(format!("{}\n\n{}", d1, d2)),
    }
}

fn infer_type(
    options: &Options,
    env: &server_env::Env,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::infer_type_options::T,
    include_refinement_info: bool,
) -> Result<
    (
        server_prot::response::InferTypeResponse,
        Option<lsp_prot::Json>,
    ),
    WorkloadCanceled,
> {
    let server_prot::infer_type_options::T {
        input: ref file_input,
        line,
        r#char: column,
        ref verbose,
        omit_targ_defaults,
        wait_for_recheck: _,
        verbose_normalizer,
        max_depth,
        json,
        ref strip_root,
        expanded,
        debug_print_internal_repr: _,
        no_typed_ast_for_imports,
    } = *input;
    let mut options = options.clone();
    options.verbose = verbose.as_ref().map(|v| std::sync::Arc::new(v.clone()));
    match of_file_input(&options, env, file_input) {
        Err(IdeFileError::Failed(msg)) => Ok((Err(msg), None)),
        Err(IdeFileError::Skipped(reason)) => {
            let tys = if json {
                server_prot::response::infer_type::Payload::Json("null".to_string())
            } else {
                server_prot::response::infer_type::Payload::Friendly(None)
            };
            let response = server_prot::response::infer_type::T {
                loc: flow_parser::loc::Loc::none(),
                tys,
                refining_locs: vec![],
                refinement_invalidated: vec![],
                documentation: None,
            };
            let extra_data = json_of_skipped(&reason);
            Ok((Ok(response), extra_data))
        }
        Ok((file_key, content)) => {
            let parse_result = parse_contents(&options, &content, &file_key);
            let (file_artifacts_result, did_hit_cache) = type_parse_artifacts_with_cache(
                &options,
                type_parse_artifacts_cache,
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.dupe(),
                parse_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(parse_errors) => {
                    let err_str = type_contents_error_to_string(
                        &parse_errors,
                        "Couldn't parse file in parse_artifacts",
                    )?;
                    let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
                    Ok((
                        Err(err_str),
                        Some(serde_json::Value::Object(json_props.into_iter().collect())),
                    ))
                }
                Ok(ref check_result) => {
                    let (ref parse_artifacts, ref typecheck_artifacts) = *check_result;
                    let loc_of_aloc = |aloc: &flow_aloc::ALoc| -> flow_parser::loc::Loc {
                        shared_mem.loc_of_aloc(aloc)
                    };
                    let ((loc, tys, refining_locs, refinement_invalidated), type_at_pos_json_props) =
                        match flow_services_type_info::type_info_service::type_at_pos(
                            &typecheck_artifacts.cx,
                            parse_artifacts.file_sig.clone(),
                            &typecheck_artifacts.typed_ast,
                            omit_targ_defaults,
                            max_depth as u32,
                            verbose_normalizer,
                            no_typed_ast_for_imports,
                            Some(&loc_of_aloc),
                            if include_refinement_info {
                                Some(&loc_of_aloc)
                            } else {
                                None
                            },
                            file_key.dupe(),
                            line,
                            column,
                        ) {
                            Ok(v) => v,
                            Err(e) => {
                                let err_str = format!("infer_type: cancel/timeout: {:?}", e);
                                let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
                                return Ok((
                                    Err(err_str),
                                    Some(serde_json::Value::Object(
                                        json_props.into_iter().collect(),
                                    )),
                                ));
                            }
                        };
                    let documentation = documentation_at_loc(
                        &shared_mem,
                        &file_key,
                        line as u32,
                        column as u32,
                        check_result,
                        &parse_artifacts.ast,
                    );
                    let mut json_props: Vec<(String, lsp_prot::Json)> =
                        type_at_pos_json_props.into_iter().collect();
                    json_props = add_cache_hit_data_to_json(json_props, did_hit_cache);
                    json_props.insert(
                        0,
                        (
                            "documentation".to_string(),
                            serde_json::Value::Bool(documentation.is_some()),
                        ),
                    );
                    let exact_by_default = options.exact_by_default;
                    let response = infer_type_to_response(
                        json,
                        expanded,
                        exact_by_default,
                        options.ts_syntax,
                        strip_root.as_deref(),
                        loc,
                        refining_locs,
                        refinement_invalidated,
                        documentation,
                        tys,
                        &loc_of_aloc,
                    );
                    Ok((
                        Ok(response),
                        Some(serde_json::Value::Object(json_props.into_iter().collect())),
                    ))
                }
            }
        }
    }
}

fn type_of_name(
    options: &Options,
    env: &server_env::Env,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::type_of_name_options::T,
) -> Result<Vec<server_prot::response::InferTypeOfNameResponse>, WorkloadCanceled> {
    let server_prot::type_of_name_options::T {
        input: ref file_input,
        ref names,
        ref verbose,
        ..
    } = *input;
    match of_file_input(options, env, file_input) {
        Err(IdeFileError::Failed(e)) => Ok(names.iter().map(|_| Err(e.clone())).collect()),
        Err(IdeFileError::Skipped(reason)) => Ok(names
            .iter()
            .map(|_| Err(format!("file skipped: {}", reason)))
            .collect()),
        Ok((file_key, content)) => {
            let mut options = options.clone();
            options.verbose = verbose.as_ref().map(|v| std::sync::Arc::new(v.clone()));
            let parse_result = parse_contents(&options, &content, &file_key);
            let (file_artifacts_result, _did_hit_cache) = type_parse_artifacts_with_cache(
                &options,
                type_parse_artifacts_cache,
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.dupe(),
                parse_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(parse_errors) => {
                    let err_str = type_contents_error_to_string(&parse_errors, "Cannot parse")?;
                    Ok(names.iter().map(|_| Err(err_str.clone())).collect())
                }
                Ok(check_result) => {
                    let doc_at_loc = |reader: &flow_heap::parsing_heaps::SharedMem,
                                      check_result: &CheckResult<'_>,
                                      ast: &flow_parser::ast::Program<
                        flow_parser::loc::Loc,
                        flow_parser::loc::Loc,
                    >,
                                      file_key: &flow_parser::file_key::FileKey,
                                      line: i32,
                                      column: i32|
                     -> Option<String> {
                        documentation_at_loc(
                            reader,
                            file_key,
                            line as u32,
                            column as u32,
                            check_result,
                            ast,
                        )
                    };
                    Ok(flow_services_type_of_name::type_of_name::type_of_name(
                        &options,
                        shared_mem,
                        env,
                        &doc_at_loc,
                        file_key,
                        input,
                        &check_result,
                    ))
                }
            }
        }
    }
}

fn inlay_hint(
    options: &Options,
    env: &server_env::Env,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::inlay_hint_options::T,
) -> Result<
    (
        server_prot::response::inlay_hint::Response,
        Option<lsp_prot::Json>,
    ),
    WorkloadCanceled,
> {
    let server_prot::inlay_hint_options::T {
        input: ref file_input,
        ref verbose,
        omit_targ_defaults,
        wait_for_recheck: _,
        verbose_normalizer,
        max_depth,
        no_typed_ast_for_imports,
    } = *input;
    let mut options = options.clone();
    options.verbose = verbose.as_ref().map(|v| std::sync::Arc::new(v.clone()));
    match of_file_input(&options, env, file_input) {
        Err(IdeFileError::Failed(e)) => Ok((Err(e), None)),
        Err(IdeFileError::Skipped(reason)) => {
            let extra_data = json_of_skipped(&reason);
            Ok((Ok(vec![]), extra_data))
        }
        Ok((file_key, content)) => {
            let parse_result = parse_contents(&options, &content, &file_key);
            let (file_artifacts_result, did_hit_cache) = match type_parse_artifacts_with_cache(
                &options,
                type_parse_artifacts_cache,
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.dupe(),
                parse_result,
                node_modules_containers,
            ) {
                (Ok(result), did_hit_cache) => (Ok(result), did_hit_cache),
                (Err(parse_errors), did_hit_cache) => (
                    Err(type_contents_error_to_string(
                        &parse_errors,
                        "Couldn't parse file in parse_contents",
                    )?),
                    did_hit_cache,
                ),
            };
            match &file_artifacts_result {
                Err(msg) => {
                    let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
                    Ok((
                        Err(msg.clone()),
                        Some(serde_json::Value::Object(json_props.into_iter().collect())),
                    ))
                }
                Ok(check_result) => {
                    let (parse_artifacts, typecheck_artifacts) = check_result;
                    let loc_of_aloc = |aloc: &flow_aloc::ALoc| -> flow_parser::loc::Loc {
                        shared_mem.loc_of_aloc(aloc)
                    };
                    // LSP boundary: convert JobError (cancel/timeout) escaping
                    // batched_type_at_pos_from_special_comments into an empty
                    // inlay-hint response exactly once.
                    let (result, json_data) =
                        match flow_services_type_info::type_info_service::batched_type_at_pos_from_special_comments(
                            &typecheck_artifacts.cx,
                            parse_artifacts.file_sig.clone(),
                            &typecheck_artifacts.typed_ast,
                            omit_targ_defaults,
                            max_depth as u32,
                            verbose_normalizer,
                            no_typed_ast_for_imports,
                            &loc_of_aloc,
                            file_key.dupe(),
                        ) {
                            Ok(tuple) => tuple,
                            Err(_job_err) => {
                                return Ok((Ok(vec![]), None));
                            }
                        };
                    let mut result_with_docs: Vec<server_prot::response::inlay_hint::Item> =
                        Vec::with_capacity(result.len());
                    for (cursor_loc, type_loc, tys, refining_locs, refinement_invalidated) in result
                    {
                        let line = type_loc.start.line;
                        let column = type_loc.start.column;
                        let documentation = documentation_at_loc(
                            &shared_mem,
                            &file_key,
                            line as u32,
                            column as u32,
                            check_result,
                            &parse_artifacts.ast,
                        );
                        let tys = tys.map(|r| {
                            let (type_str, refs) =
                                flow_common_ty::ty_printer::string_of_type_at_pos_result(
                                    &r.unevaluated,
                                    &r.evaluated,
                                    &r.refs,
                                    &flow_common_ty::ty_printer::PrinterOptions {
                                        exact_by_default: options.exact_by_default,
                                        ts_syntax: options.ts_syntax,
                                        ..Default::default()
                                    },
                                );
                            server_prot::response::infer_type::FriendlyResponse { type_str, refs }
                        });
                        result_with_docs.push(server_prot::response::inlay_hint::Item {
                            cursor_loc,
                            type_loc,
                            tys,
                            refining_locs,
                            refinement_invalidated,
                            documentation,
                        });
                    }
                    Ok((Ok(result_with_docs), Some(json_data)))
                }
            }
        }
    }
}

fn insert_type(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    file_input: &FileInput,
    target: &flow_parser::loc::Loc,
    omit_targ_defaults: bool,
    location_is_strict: bool,
) -> Result<server_prot::response::InsertTypeResponse, WorkloadCanceled> {
    let file_key = file_key_of_file_input(options, env, file_input);
    let file_content = match file_input.content_of_file_input() {
        Ok(c) => c,
        Err(s) => return Ok(Err(s)),
    };
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc = move |aloc: &flow_aloc::ALoc| shared_mem_loa.loc_of_aloc(aloc);
    let shared_mem_ast = shared_mem.clone();
    let get_ast_from_shared_mem =
        move |f: &flow_parser::file_key::FileKey| shared_mem_ast.get_ast(f).map(|a| (*a).clone());
    let shared_mem_hmi = shared_mem.clone();
    let get_haste_module_info =
        move |f: &flow_parser::file_key::FileKey| shared_mem_hmi.get_haste_module_info(f);
    let shared_mem_ts = shared_mem.clone();
    let get_type_sig = move |f: &flow_parser::file_key::FileKey| -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    > {
        shared_mem_ts.get_type_sig(f).map(|arc| {
            let bytes = bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                .expect("get_type_sig: serialize");
            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                .expect("get_type_sig: deserialize")
                .0
        })
    };
    let intermediate_result = parse_contents(options, &file_content, &file_key);
    let file_artifacts_result = type_parse_artifacts(
        options,
        shared_mem,
        env.master_cx.clone(),
        file_key,
        intermediate_result,
        node_modules_containers,
    );
    match &file_artifacts_result {
        Ok((parse_artifacts, typecheck_artifacts)) => {
            let edits = match flow_services_code_action::code_action_service::insert_type_fn(
                options,
                &loc_of_aloc,
                &get_ast_from_shared_mem,
                &get_haste_module_info,
                &get_type_sig,
                &file_content,
                target.clone(),
                omit_targ_defaults,
                location_is_strict,
                parse_artifacts,
                typecheck_artifacts,
            ) {
                Ok(e) => e,
                Err(s) => return Ok(Err(s)),
            };
            Ok(Ok(
                flow_parser_utils_output::replacement_printer::loc_patch_to_patch(
                    &file_content,
                    &edits,
                ),
            ))
        }
        Err(error) => Ok(Err(type_contents_error_to_string(
            error,
            "Failed to type-check file",
        )?)),
    }
}

fn autofix_exports(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> Result<server_prot::response::AutofixExportsResponse, WorkloadCanceled> {
    let file_key = file_key_of_file_input(options, env, input);
    let file_content = match input.content_of_file_input() {
        Ok(c) => c,
        Err(s) => return Ok(Err(s)),
    };
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc: Arc<dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc> =
        Arc::new(move |aloc| shared_mem_loa.loc_of_aloc(aloc));
    let shared_mem_ast = shared_mem.clone();
    let get_ast_from_shared_mem =
        move |f: &flow_parser::file_key::FileKey| shared_mem_ast.get_ast(f).map(|a| (*a).clone());
    let shared_mem_hmi = shared_mem.clone();
    let get_haste_module_info: Arc<
        dyn Fn(&flow_parser::file_key::FileKey) -> Option<flow_common_modulename::HasteModuleInfo>,
    > = Arc::new(move |f| shared_mem_hmi.get_haste_module_info(f));
    let shared_mem_ts = shared_mem.clone();
    let get_type_sig: Arc<
        dyn Fn(
            &flow_parser::file_key::FileKey,
        ) -> Option<
            flow_type_sig::packed_type_sig::Module<
                flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
            >,
        >,
    > = Arc::new(move |f| {
        shared_mem_ts.get_type_sig(f).map(|arc| {
            let bytes = bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                .expect("get_type_sig: serialize");
            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                .expect("get_type_sig: deserialize")
                .0
        })
    });
    let intermediate_result = parse_contents(options, &file_content, &file_key);
    let file_artifacts_result = type_parse_artifacts(
        options,
        shared_mem,
        env.master_cx.clone(),
        file_key.clone(),
        intermediate_result,
        node_modules_containers,
    );
    match &file_artifacts_result {
        Ok((parse_artifacts, typecheck_artifacts)) => {
            let (edits, errors) =
                flow_services_code_action::code_action_service::autofix_exports_fn(
                    options,
                    loc_of_aloc,
                    &get_ast_from_shared_mem,
                    get_haste_module_info,
                    get_type_sig,
                    file_key,
                    &file_content,
                    parse_artifacts,
                    typecheck_artifacts,
                );
            if errors
                .iter()
                .any(|error| is_checked_dependencies_retry_error(error))
            {
                return Err(WorkloadCanceled);
            }
            Ok(Ok((
                flow_parser_utils_output::replacement_printer::loc_patch_to_patch(
                    &file_content,
                    &edits,
                ),
                errors,
            )))
        }
        Err(error) => Ok(Err(type_contents_error_to_string(
            error,
            "Failed to type-check file",
        )?)),
    }
}

fn autofix_missing_local_annot(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> Result<server_prot::response::AutofixMissingLocalAnnotResponse, WorkloadCanceled> {
    let file_key = file_key_of_file_input(options, env, input);
    let file_content = match input.content_of_file_input() {
        Ok(c) => c,
        Err(s) => return Ok(Err(s)),
    };
    let shared_mem_loa = shared_mem.clone();
    let loc_of_aloc = move |aloc: &flow_aloc::ALoc| shared_mem_loa.loc_of_aloc(aloc);
    let shared_mem_ast = shared_mem.clone();
    let get_ast_from_shared_mem =
        move |f: &flow_parser::file_key::FileKey| shared_mem_ast.get_ast(f).map(|a| (*a).clone());
    let shared_mem_hmi = shared_mem.clone();
    let get_haste_module_info =
        move |f: &flow_parser::file_key::FileKey| shared_mem_hmi.get_haste_module_info(f);
    let shared_mem_ts = shared_mem.clone();
    let get_type_sig = move |f: &flow_parser::file_key::FileKey| -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    > {
        shared_mem_ts.get_type_sig(f).map(|arc| {
            let bytes = bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                .expect("get_type_sig: serialize");
            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                .expect("get_type_sig: deserialize")
                .0
        })
    };
    let intermediate_result = parse_contents(options, &file_content, &file_key);
    let file_artifacts_result = type_parse_artifacts(
        options,
        shared_mem,
        env.master_cx.clone(),
        file_key,
        intermediate_result,
        node_modules_containers,
    );
    let file_artifacts = match file_artifacts_result {
        Ok(x) => x,
        Err(e) => {
            return Ok(Err(type_contents_error_to_string(
                &e,
                "Failed to type-check file",
            )?));
        }
    };
    let (ref parse_artifacts, ref typecheck_artifacts) = file_artifacts;
    let edits = match flow_services_code_action::code_action_service::autofix_missing_local_annot_fn(
        options,
        &loc_of_aloc,
        &get_ast_from_shared_mem,
        &get_haste_module_info,
        &get_type_sig,
        &file_content,
        parse_artifacts,
        typecheck_artifacts,
    ) {
        Ok(e) => e,
        Err(s) => return Ok(Err(s)),
    };
    Ok(Ok(
        flow_parser_utils_output::replacement_printer::loc_patch_to_patch(&file_content, &edits),
    ))
}

fn collect_rage(
    options: &Options,
    env: &server_env::Env,
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    files: Option<&[String]>,
) -> Vec<(String, String)> {
    let mut items = Vec::new();
    let data = format!("lazy_mode={}\n", options.lazy_mode);
    items.push(("options".to_string(), data));
    let data = format!(
        "{}\n\n{}\n",
        env.checked_files.debug_counts_to_string(),
        env.checked_files.debug_to_string(Some(200)),
    );
    items.push(("env.checked_files".to_string(), data));
    let dep_graph = env.dependency_info.implementation_dependency_graph();
    let dep_map = dep_graph.to_map();
    let mut dep_lines: Vec<String> = dep_map
        .iter()
        .map(|(file, deps)| {
            let file_str = file.as_str();
            let dep_strs: Vec<&str> = deps.iter().take(20).map(|d| d.as_str()).collect();
            let suffix = if deps.len() > 20 {
                format!(" ...{} more", deps.len() - 20)
            } else {
                String::new()
            };
            format!("{}:{}{}\n", file_str, dep_strs.join(","), suffix)
        })
        .collect();
    if dep_lines.len() > 200 {
        let total = dep_lines.len();
        dep_lines.truncate(200);
        dep_lines.push(format!("[shown 200/{}]\n", total));
    }
    let dependencies = dep_lines.join("");
    let data = format!("DEPENDENCIES:\n{}", dependencies);
    items.push(("env.dependencies".to_string(), data));
    let (errors, warnings) = error_collator::get_without_suppressed(env);
    let json = flow_common_errors::error_utils::json_output::json_of_errors_with_context(
        None,
        &None,
        &[],
        flow_common_errors::error_utils::json_output::JsonVersion::JsonV1,
        flow_parser::offset_utils::OffsetKind::Utf8,
        &errors,
        &warnings,
    );
    let data = format!(
        "ERRORS:\n{}",
        serde_json::to_string_pretty(&json).unwrap_or_default()
    );
    items.push(("env.errors".to_string(), data));
    if let Some(files) = files {
        let mut data = String::from(
            "Does the content on the disk match the most recent version of the file?\n\n",
        );
        for file in files {
            let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(file);
            let file_state = if !env.files.contains(&file_key) {
                "FILE NOT PARSED BY FLOW (likely ignored implicitly or explicitly)".to_string()
            } else {
                match std::fs::read_to_string(file) {
                    Err(_) => "ERROR! FAILED TO READ".to_string(),
                    Ok(content) => {
                        if flow_parsing::parsing_service::does_content_match_file_hash(
                            shared_mem, &file_key, &content,
                        ) {
                            "OK".to_string()
                        } else {
                            "HASH OUT OF DATE".to_string()
                        }
                    }
                }
            };
            data.push_str(&format!("{}: {}\n", file, file_state));
        }
        items.push(("file hash check".to_string(), data));
    }
    items
}

fn dump_types(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    evaluate_type_destructors: flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode,
    for_tool: Option<i32>,
    file_input: &FileInput,
) -> Result<Result<Vec<(flow_parser::loc::Loc, String)>, String>, WorkloadCanceled> {
    let file_key = file_key_of_file_input(options, env, file_input);
    let content = match file_input.content_of_file_input() {
        Ok(c) => c,
        Err(s) => return Ok(Err(s)),
    };
    let intermediate_result = parse_contents(options, &content, &file_key);
    let file_artifacts_result = type_parse_artifacts(
        options,
        shared_mem,
        env.master_cx.clone(),
        file_key,
        intermediate_result,
        node_modules_containers,
    );
    let (parse_artifacts, typecheck_artifacts) = match file_artifacts_result {
        Ok(x) => x,
        Err(parse_errors) => {
            return Ok(Err(type_contents_error_to_string(
                &parse_errors,
                "Couldn't parse file in parse_contents",
            )?));
        }
    };
    Ok(Ok(flow_services_type_info::type_info_service::dump_types(
        evaluate_type_destructors,
        for_tool,
        &typecheck_artifacts.cx,
        parse_artifacts.file_sig.clone(),
        &typecheck_artifacts.typed_ast,
    )))
}

fn coverage(
    options: &Options,
    env: &server_env::Env,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    file_key: &flow_parser::file_key::FileKey,
    content: &str,
    force: bool,
) -> Result<
    (
        server_prot::response::CoverageResponse,
        Option<lsp_prot::Json>,
    ),
    WorkloadCanceled,
> {
    let mut options = options.clone();
    options.all = options.all || force;
    let intermediate_result = parse_contents(&options, content, file_key);
    let (file_artifacts_result, did_hit_cache) = type_parse_artifacts_with_cache(
        &options,
        type_parse_artifacts_cache,
        shared_mem,
        env.master_cx.clone(),
        file_key.clone(),
        intermediate_result,
        node_modules_containers,
    );
    let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
    let extra_data = serde_json::Value::Object(json_props.into_iter().collect());
    match file_artifacts_result {
        Ok((ref _parse_artifacts, ref typecheck_artifacts)) => {
            let coverage = flow_services_type_info::type_info_service::coverage(
                &typecheck_artifacts.cx,
                &typecheck_artifacts.typed_ast,
                force,
                file_key,
                content,
            );
            Ok((Ok(coverage), Some(extra_data)))
        }
        Err(parse_errors) => Ok((
            Err(type_contents_error_to_string(
                &parse_errors,
                "Couldn't parse file in parse_contents",
            )?),
            Some(extra_data),
        )),
    }
}

fn batch_coverage(
    options: &Options,
    env: &server_env::Env,
    batch: &[String],
) -> server_prot::response::BatchCoverageResponse {
    if options.lazy_mode {
        Err(
            "Batch coverage cannot be run in lazy mode.\n\nRestart the Flow server with '--no-lazy' to enable this command."
                .to_string(),
        )
    } else {
        let filter = |key: &str| batch.iter().any(|elt| key.starts_with(elt.as_str()));
        let mut response: Vec<_> = env
            .coverage
            .iter()
            .filter(|(key, _)| !key.is_lib_file() && filter(&key.to_absolute()))
            .map(|(key, coverage)| (key.dupe(), coverage.clone()))
            .collect();
        response.reverse();
        Ok(response)
    }
}

// Convert from map/set to lists for serialization to client.
fn serialize_graph(
    graph: &std::collections::BTreeMap<
        flow_parser::file_key::FileKey,
        std::collections::BTreeSet<flow_parser::file_key::FileKey>,
    >,
) -> server_prot::response::GraphResponseSubgraph {
    let mut result: Vec<_> = graph
        .iter()
        .map(|(f, dep_fs)| {
            let f_str = f.to_absolute();
            let mut dep_strs: Vec<String> =
                dep_fs.iter().map(|dep_f| dep_f.to_absolute()).collect();
            dep_strs.reverse();
            (f_str, dep_strs)
        })
        .collect();
    result.reverse();
    result
}

fn output_dependencies(
    env: &server_env::Env,
    root: &str,
    strip_root: bool,
    types_only: bool,
    outfile: &str,
) -> Result<(), String> {
    let strip_root_fn: Box<dyn Fn(&str) -> String> = if strip_root {
        let root = root.to_string();
        Box::new(move |x: &str| flow_common::files::relative_path(std::path::Path::new(&root), x))
    } else {
        Box::new(|x: &str| x.to_string())
    };
    let dep_graph = if types_only {
        env.dependency_info.sig_dependency_graph()
    } else {
        env.dependency_info.implementation_dependency_graph()
    };
    let graph = serialize_graph(&dep_graph.to_map());
    eprintln!("printing dependency graph to {}", outfile);
    let mut out =
        std::fs::File::create(outfile).map_err(|e| format!("Failed to open {}: {}", outfile, e))?;
    use std::io::Write;
    writeln!(out, "digraph {{").map_err(|e| e.to_string())?;
    for (f, dep_fs) in &graph {
        for dep_f in dep_fs {
            writeln!(
                out,
                "  \"{}\" -> \"{}\"",
                strip_root_fn(f),
                strip_root_fn(dep_f)
            )
            .map_err(|e| e.to_string())?;
        }
    }
    write!(out, "}}").map_err(|e| e.to_string())?;
    Ok(())
}

fn get_cycle(
    env: &server_env::Env,
    filename: &flow_parser::file_key::FileKey,
    types_only: bool,
) -> server_prot::response::GraphResponse {
    let parsed = &env.files;
    let dependency_graph = if types_only {
        env.dependency_info.sig_dependency_graph()
    } else {
        env.dependency_info.implementation_dependency_graph()
    };
    let components = topsort(parsed.iter().cloned(), dependency_graph);
    let component = components
        .into_iter()
        .find(|c| c.iter().any(|f| f == filename));
    let component = match component {
        Some(c) => c,
        None => return Ok(vec![]),
    };
    let dep_map = dependency_graph.to_map();
    let mut result = std::collections::BTreeMap::new();
    for f in component.iter() {
        if let Some(deps) = dep_map.get(f) {
            let subdeps: std::collections::BTreeSet<flow_parser::file_key::FileKey> = deps
                .iter()
                .filter(|d| component.iter().any(|c| c == *d))
                .cloned()
                .collect();
            if !subdeps.is_empty() {
                result.insert(f.clone(), subdeps);
            }
        }
    }
    Ok(serialize_graph(&result))
}

fn find_module(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    moduleref: &str,
    filename: &str,
) -> server_prot::response::FindModuleResponse {
    let file = flow_parser::file_key::FileKey::source_file_of_absolute(filename);
    let mut phantom_acc = flow_services_module::PhantomAcc::default();
    let import_specifier =
        flow_common::flow_import_specifier::FlowImportSpecifier::userland(moduleref.into());
    let resolved_module = flow_services_module::imported_module(
        options,
        &shared_mem,
        node_modules_containers,
        &file,
        Some(&mut phantom_acc),
        &import_specifier,
    );
    let phantom_deps: Vec<String> = phantom_acc
        .keys()
        .map(|mname| match mname {
            flow_common_modulename::Modulename::Haste(info) => format!("Haste: {}", info),
            flow_common_modulename::Modulename::Filename(f) => format!("File: {}", f.as_str()),
        })
        .collect();
    let provider = match resolved_module {
        Ok(ref m) => shared_mem.get_provider(m),
        Err(_) => None,
    };
    match provider {
        Some(file_key) => (Some(file_key), phantom_deps),
        None => (None, phantom_deps),
    }
}

fn get_def(
    options: &Options,
    env: &server_env::Env,
    type_parse_artifacts_cache: Option<&TypeParseArtifactsCache>,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    file_input: &FileInput,
    line: u32,
    col: u32,
) -> Result<
    (
        server_prot::response::GetDefResponse,
        Option<lsp_prot::Json>,
    ),
    WorkloadCanceled,
> {
    match of_file_input(options, env, file_input) {
        Err(IdeFileError::Failed(msg)) => Ok((Err(msg), None)),
        Err(IdeFileError::Skipped(reason)) => {
            let mut json_props = json_props_of_skipped(&reason);
            json_props.insert(
                0,
                (
                    "result".to_string(),
                    lsp_prot::Json::String("SKIPPED".to_string()),
                ),
            );
            Ok((
                Ok(vec![]),
                Some(lsp_prot::Json::Object(json_props.into_iter().collect())),
            ))
        }
        Ok((file_key, content)) => {
            let intermediate_result = parse_contents(options, &content, &file_key);
            let (check_result, did_hit_cache) = match type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache,
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.clone(),
                intermediate_result,
                node_modules_containers,
            ) {
                (Ok(result), did_hit_cache) => (Ok(result), did_hit_cache),
                (Err(parse_errors), did_hit_cache) => (
                    Err(type_contents_error_to_string(
                        &parse_errors,
                        "Couldn't parse file in parse_contents",
                    )?),
                    did_hit_cache,
                ),
            };
            match check_result {
                Err(msg) => {
                    let mut json_props =
                        vec![("error".to_string(), lsp_prot::Json::String(msg.clone()))];
                    json_props = add_cache_hit_data_to_json(json_props, did_hit_cache);
                    Ok((
                        Err(msg),
                        Some(lsp_prot::Json::Object(json_props.into_iter().collect())),
                    ))
                }
                Ok(ref check_result) => {
                    let (result, json_props) = get_def_of_check_result(
                        &shared_mem,
                        &file_key,
                        line,
                        col,
                        check_result,
                        Some(&content),
                        &Purpose::GoToDefinition,
                    );
                    let mut json_props = json_props.unwrap_or_default();
                    match &result {
                        Ok(locs) if locs.is_empty() => {
                            let broader_context = match std::panic::catch_unwind(|| {
                                let offset = flow_server_utils::file_content::get_offset(
                                    &content,
                                    &flow_server_utils::file_content::Position {
                                        line: line as usize,
                                        column: col as usize + 1,
                                    },
                                );
                                let before_start = offset.saturating_sub(100);
                                let before = content.get(before_start..offset);
                                let after_end = std::cmp::min(offset + 100, content.len());
                                let after = content.get(offset..after_end);
                                match (before, after) {
                                    (Some(before), Some(after)) => {
                                        lsp_prot::Json::String(format!("{}|{}", before, after))
                                    }
                                    _ => lsp_prot::Json::Null,
                                }
                            }) {
                                Ok(broader_context) => broader_context,
                                Err(_) => lsp_prot::Json::Null,
                            };
                            json_props.insert(0, ("broader_context".to_string(), broader_context));
                        }
                        _ => {}
                    }
                    json_props = add_cache_hit_data_to_json(json_props, did_hit_cache);
                    Ok((
                        result,
                        Some(lsp_prot::Json::Object(json_props.into_iter().collect())),
                    ))
                }
            }
        }
    }
}

fn save_state(
    genv: &server_env::Genv,
    env: &server_env::Env,
    saved_state_filename: &str,
) -> Result<String, String> {
    flow_saved_state::save(
        std::path::Path::new(saved_state_filename),
        &genv.shared_mem,
        env,
        &genv.options,
        genv.node_modules_containers.as_ref(),
    )
    .map(|_| saved_state_filename.to_string())
    .map_err(|reason| reason.to_string())
}

fn auto_close_jsx(
    ast: &flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    target_pos: &flow_parser::loc::Position,
) -> Option<String> {
    flow_services_jsx::auto_close_jsx::get_snippet(ast, *target_pos)
}

fn linked_editing_range(
    ast: &flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    loc: &flow_parser::loc::Loc,
) -> Option<Vec<lsp_types::Range>> {
    flow_services_jsx::linked_editing_jsx::get_linked_locs(ast, loc.clone())
        .map(|locs| locs.iter().map(loc_to_lsp_range).collect())
}

fn vscode_detailed_diagnostics(client_id: lsp_prot::ClientId) -> bool {
    persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::lsp_initialize_params(&client))
        .and_then(|params| params.initialization_options)
        .and_then(|opts| match opts.get("detailedErrorRendering") {
            Some(serde_json::Value::Bool(b)) => Some(*b),
            Some(serde_json::Value::String(s)) if s == "true" => Some(true),
            Some(serde_json::Value::String(s)) if s == "false" => Some(false),
            _ => None,
        })
        .unwrap_or(false)
}

fn semantic_decorations(client_id: lsp_prot::ClientId) -> bool {
    persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::lsp_initialize_params(&client))
        .and_then(|params| params.initialization_options)
        .and_then(|opts| match opts.get("semanticDecorations") {
            Some(serde_json::Value::Bool(b)) => Some(*b),
            _ => None,
        })
        .unwrap_or(false)
}

fn refinement_info_on_hover(client_id: lsp_prot::ClientId) -> bool {
    persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::lsp_initialize_params(&client))
        .and_then(|params| params.initialization_options)
        .and_then(|opts| match opts.get("refinementInformationOnHover") {
            Some(serde_json::Value::Bool(b)) => Some(*b),
            _ => None,
        })
        .unwrap_or(false)
}

fn rank_autoimports_by_usage(options: &Options, client_id: lsp_prot::ClientId) -> bool {
    match persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::client_config(&client))
    {
        Some(config) => match config.rank_autoimports_by_usage {
            persistent_connection::client_config::ClientToggle::True => true,
            persistent_connection::client_config::ClientToggle::False => false,
            persistent_connection::client_config::ClientToggle::Default => {
                options.autoimports_ranked_by_usage
            }
        },
        None => options.autoimports_ranked_by_usage,
    }
}

fn handle_apply_code_action(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    action: &server_prot::code_action::T,
    file_input: &FileInput,
) -> EphemeralParallelizableResult {
    match action {
        server_prot::code_action::T::Quickfix {
            include_best_effort_fix,
        } => {
            let result: Result<_, String> = try_with(|| {
                autofix_errors_cli(
                    options,
                    env,
                    shared_mem.clone(),
                    node_modules_containers,
                    *include_best_effort_fix,
                    file_input,
                )
            });
            Ok((
                server_prot::response::Response::APPLY_CODE_ACTION(result),
                None,
            ))
        }
        server_prot::code_action::T::SourceAddMissingImports => {
            let result: Result<_, String> = try_with(|| {
                autofix_imports_cli(
                    options,
                    env,
                    shared_mem.clone(),
                    node_modules_containers,
                    file_input,
                )
            });
            Ok((
                server_prot::response::Response::APPLY_CODE_ACTION(result),
                None,
            ))
        }
        server_prot::code_action::T::SuggestImports => {
            let result = try_with(|| {
                suggest_imports_cli(
                    options,
                    env,
                    shared_mem.clone(),
                    node_modules_containers,
                    file_input,
                )
            });
            Ok((
                server_prot::response::Response::SUGGEST_IMPORTS(result),
                None,
            ))
        }
    }
}

fn handle_autocomplete(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
    trigger_character: Option<&str>,
    cursor: (i32, i32),
    imports: bool,
    imports_ranked_usage: bool,
    show_ranking_info: bool,
) -> EphemeralParallelizableResult {
    let (result, json_data) = autocomplete(
        options,
        env,
        shared_mem,
        node_modules_containers,
        None,
        input,
        trigger_character,
        cursor,
        imports,
        imports_ranked_usage,
        show_ranking_info,
    )?;
    let result = result.map(|(_token, completions, _token_loc, ac_type)| (completions, ac_type));
    Ok((
        server_prot::response::Response::AUTOCOMPLETE(result),
        json_data,
    ))
}

fn handle_autofix_exports(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> EphemeralParallelizableResult {
    let result = autofix_exports(options, env, shared_mem, node_modules_containers, input)?;
    Ok((
        server_prot::response::Response::AUTOFIX_EXPORTS(result),
        None,
    ))
}

fn handle_autofix_missing_local_annot(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
) -> EphemeralParallelizableResult {
    let result =
        autofix_missing_local_annot(options, env, shared_mem, node_modules_containers, input)?;
    Ok((
        server_prot::response::Response::AUTOFIX_MISSING_LOCAL_ANNOT(result),
        None,
    ))
}

fn handle_check_file(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    force: bool,
    input: &FileInput,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
) -> EphemeralParallelizableResult {
    let response = check_file(
        options,
        env,
        shared_mem,
        force,
        input,
        node_modules_containers,
    )?;
    Ok((server_prot::response::Response::CHECK_FILE(response), None))
}

fn handle_coverage(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
    force: bool,
) -> EphemeralParallelizableResult {
    let mut options = options.clone();
    options.all = options.all || force;
    let (response, json_data) = match of_file_input(&options, env, input) {
        Err(IdeFileError::Failed(msg)) => (Err(msg), None),
        Err(IdeFileError::Skipped(reason)) => (Err(reason.clone()), json_of_skipped(&reason)),
        Ok((file_key, file_contents)) => coverage(
            &options,
            env,
            None,
            shared_mem.clone(),
            node_modules_containers,
            &file_key,
            &file_contents,
            force,
        )?,
    };
    Ok((
        server_prot::response::Response::COVERAGE(response),
        json_data,
    ))
}

fn handle_batch_coverage(
    options: &Options,
    env: &server_env::Env,
    batch: &[String],
) -> EphemeralParallelizableResult {
    let response: server_prot::response::BatchCoverageResponse =
        batch_coverage(options, env, batch);
    Ok((
        server_prot::response::Response::BATCH_COVERAGE(response),
        None,
    ))
}

fn handle_cycle(
    env: &server_env::Env,
    filename: &flow_parser::file_key::FileKey,
    types_only: bool,
) -> EphemeralNonparallelizableResult {
    let response = get_cycle(env, filename, types_only);
    Ok((server_prot::response::Response::CYCLE(response), None))
}

fn handle_dump_types(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    evaluate_type_destructors: flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode,
    for_tool: Option<i32>,
    input: &FileInput,
) -> EphemeralParallelizableResult {
    let response = dump_types(
        options,
        env,
        shared_mem,
        node_modules_containers,
        evaluate_type_destructors,
        for_tool,
        input,
    )?;
    Ok((server_prot::response::Response::DUMP_TYPES(response), None))
}

fn handle_find_module(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    moduleref: &str,
    filename: &str,
) -> EphemeralParallelizableResult {
    let response = find_module(
        options,
        shared_mem,
        node_modules_containers,
        moduleref,
        filename,
    );
    Ok((server_prot::response::Response::FIND_MODULE(response), None))
}

fn handle_force_recheck(
    files: Vec<String>,
    focus: bool,
    missed_changes: bool,
    changed_mergebase: bool,
) -> (server_prot::response::Response, Option<lsp_prot::Json>) {
    let fileset: std::collections::BTreeSet<String> = files.into_iter().collect();
    let metadata = monitor_prot::FileWatcherMetadata {
        missed_changes,
        changed_mergebase: Some(changed_mergebase),
    };
    if focus {
        server_monitor_listener_state::push_files_to_force_focused_and_recheck(fileset);
    } else {
        server_monitor_listener_state::push_files_to_recheck_with_metadata(Some(metadata), fileset);
    }
    (server_prot::response::Response::FORCE_RECHECK, None)
}

fn handle_get_def(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &FileInput,
    line: u32,
    col: u32,
) -> EphemeralParallelizableResult {
    let (result, json_data) = get_def(
        options,
        env,
        None,
        shared_mem,
        node_modules_containers,
        input,
        line,
        col,
    )?;
    Ok((server_prot::response::Response::GET_DEF(result), json_data))
}

fn handle_graph_dep_graph(
    env: &server_env::Env,
    root: &str,
    strip_root: bool,
    outfile: &str,
    types_only: bool,
) -> EphemeralNonparallelizableResult {
    let response = output_dependencies(env, root, strip_root, types_only, outfile);
    Ok((
        server_prot::response::Response::GRAPH_DEP_GRAPH(response),
        None,
    ))
}

fn handle_infer_type(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::infer_type_options::T,
) -> EphemeralParallelizableResult {
    let (result, json_data) = infer_type(
        options,
        env,
        None,
        shared_mem.clone(),
        node_modules_containers,
        input,
        true,
    )?;
    Ok((
        server_prot::response::Response::INFER_TYPE(result),
        json_data,
    ))
}

fn handle_type_of_name(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::type_of_name_options::T,
) -> EphemeralParallelizableResult {
    let (result, json_data): (
        Vec<server_prot::response::InferTypeOfNameResponse>,
        Option<lsp_prot::Json>,
    ) = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        type_of_name(
            options,
            env,
            None,
            shared_mem,
            node_modules_containers,
            input,
        )
    })) {
        Ok(result) => (result?, None),
        Err(_exn) => {
            let names = &input.names;
            (
                names
                    .iter()
                    .map(|_| Err("type_of_name: internal error".to_string()))
                    .collect(),
                None,
            )
        }
    };
    Ok((
        server_prot::response::Response::TYPE_OF_NAME(result),
        json_data,
    ))
}

fn handle_inlay_hint(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::inlay_hint_options::T,
) -> EphemeralParallelizableResult {
    let (result, json_data) = inlay_hint(
        options,
        env,
        None,
        shared_mem.clone(),
        node_modules_containers,
        input,
    )?;
    Ok((
        server_prot::response::Response::INLAY_HINT(result),
        json_data,
    ))
}

fn handle_llm_context(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    input: &server_prot::llm_context_options::T,
) -> EphemeralParallelizableResult {
    let server_prot::llm_context_options::T {
        files,
        token_budget,
        wait_for_recheck: _,
    } = input;
    let strip_root = Some(options.root.to_string_lossy().to_string());
    let strip_root_ref = strip_root.as_deref();
    let include_imports = options.llm_context_include_imports;
    let file_sig_opts = flow_parser_utils::file_sig::FileSigOptions::default();
    let file_contexts: Vec<crate::llm_typed_context_provider::FileContext> = files
        .iter()
        .filter_map(|file_path| {
            let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(file_path);
            let file_input = FileInput::FileName(file_path.clone());
            let (_file_key, content) = match of_file_input(options, env, &file_input) {
                Err(_) => return None,
                Ok(v) => v,
            };
            let intermediate_result = parse_contents(options, &content, &file_key);
            let file_artifacts_result = type_parse_artifacts(
                options,
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.clone(),
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_) => None,
                Ok((parse_artifacts, typecheck_artifacts)) => {
                    if include_imports {
                        let context = crate::llm_typed_context_provider::generate_file_context(
                            strip_root_ref,
                            &file_key,
                            &parse_artifacts.ast,
                            &typecheck_artifacts.cx,
                            &typecheck_artifacts.typed_ast,
                            &shared_mem,
                            &file_sig_opts,
                        );
                        let tokens = crate::llm_typed_context_provider::count_tokens(&context);
                        let path = flow_common::reason::string_of_source(strip_root_ref, &file_key);
                        Some(crate::llm_typed_context_provider::FileContext {
                            path,
                            context,
                            tokens,
                        })
                    } else {
                        None
                    }
                }
            }
        })
        .collect();
    let header = crate::llm_typed_context_provider::legacy_syntax_header();
    let header_tokens = crate::llm_typed_context_provider::count_tokens(&header);
    let mut remaining_budget = token_budget - header_tokens;
    let mut acc_context = String::new();
    let mut files_processed = Vec::new();
    let mut truncated = false;
    for fc in &file_contexts {
        if fc.tokens <= remaining_budget {
            remaining_budget -= fc.tokens;
            acc_context.push_str(&fc.context);
            acc_context.push('\n');
            files_processed.push(fc.path.clone());
        } else {
            truncated = true;
            break;
        }
    }
    let full_context = format!("{}\n{}", header, acc_context);
    let tokens_used = crate::llm_typed_context_provider::count_tokens(&full_context);
    let result = Ok(server_prot::response::llm_context::T {
        llm_context: full_context,
        files_processed,
        tokens_used,
        truncated,
    });
    Ok((server_prot::response::Response::LLM_CONTEXT(result), None))
}

fn handle_insert_type(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    file_input: &FileInput,
    target: &flow_parser::loc::Loc,
    omit_targ_defaults: bool,
    location_is_strict: bool,
) -> EphemeralParallelizableResult {
    let result = insert_type(
        options,
        env,
        shared_mem,
        node_modules_containers,
        file_input,
        target,
        omit_targ_defaults,
        location_is_strict,
    )?;
    Ok((server_prot::response::Response::INSERT_TYPE(result), None))
}

fn handle_rage(
    options: &Options,
    env: &server_env::Env,
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
    files: Option<&[String]>,
) -> EphemeralParallelizableResult {
    let items = collect_rage(options, env, shared_mem, files);
    Ok((server_prot::response::Response::RAGE(items), None))
}

fn handle_status(
    options: &Options,
    env: &server_env::Env,
    shared_mem: &flow_heap::parsing_heaps::SharedMem,
) -> EphemeralNonparallelizableResult {
    let (status_response, lazy_stats) = get_status(options, env, shared_mem);
    Ok((
        server_prot::response::Response::STATUS {
            status_response,
            lazy_stats,
        },
        None,
    ))
}

fn handle_save_state(
    genv: &server_env::Genv,
    env: &server_env::Env,
    saved_state_filename: &str,
) -> EphemeralNonparallelizableResult {
    let result = save_state(genv, env, saved_state_filename);
    Ok((server_prot::response::Response::SAVE_STATE(result), None))
}

pub fn handle_ephemeral_command_for_standalone(
    genv: &server_env::Genv,
    env: &server_env::Env,
    command: server_prot::request::Command,
) -> EphemeralParallelizableResult {
    let options = &*genv.options;
    let shared_mem = genv.shared_mem.dupe();
    let node_modules_containers = genv.node_modules_containers.as_ref();
    match command {
        server_prot::request::Command::APPLY_CODE_ACTION {
            input,
            action,
            wait_for_recheck: _,
        } => handle_apply_code_action(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &action,
            &input,
        ),
        server_prot::request::Command::AUTOCOMPLETE {
            input,
            cursor,
            trigger_character,
            wait_for_recheck: _,
            imports,
            imports_ranked_usage,
            show_ranking_info,
        } => handle_autocomplete(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &input,
            trigger_character.as_deref(),
            cursor,
            imports,
            imports_ranked_usage,
            show_ranking_info,
        ),
        server_prot::request::Command::AUTOFIX_EXPORTS {
            input,
            verbose: _,
            wait_for_recheck: _,
        } => handle_autofix_exports(options, env, shared_mem, node_modules_containers, &input),
        server_prot::request::Command::AUTOFIX_MISSING_LOCAL_ANNOT {
            input,
            verbose: _,
            wait_for_recheck: _,
        } => handle_autofix_missing_local_annot(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &input,
        ),
        server_prot::request::Command::CHECK_FILE {
            input,
            verbose: _,
            force,
            include_warnings,
            wait_for_recheck: _,
        } => {
            let mut options = options.clone();
            options.include_warnings = options.include_warnings || include_warnings;
            handle_check_file(
                &options,
                env,
                shared_mem,
                force,
                &input,
                node_modules_containers,
            )
        }
        server_prot::request::Command::COVERAGE {
            input,
            force,
            wait_for_recheck: _,
        } => handle_coverage(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &input,
            force,
        ),
        server_prot::request::Command::BATCH_COVERAGE {
            batch,
            wait_for_recheck: _,
        } => handle_batch_coverage(options, env, &batch),
        server_prot::request::Command::CYCLE {
            filename,
            types_only,
        } => {
            let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(&filename);
            handle_cycle(env, &file_key, types_only)
        }
        server_prot::request::Command::DUMP_TYPES {
            input,
            evaluate_type_destructors,
            for_tool,
            wait_for_recheck: _,
        } => handle_dump_types(
            options,
            env,
            shared_mem,
            node_modules_containers,
            if evaluate_type_destructors {
                flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode::EvaluateAll
            } else {
                flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode::EvaluateNone
            },
            for_tool,
            &input,
        ),
        server_prot::request::Command::FIND_MODULE {
            moduleref,
            filename,
            wait_for_recheck: _,
        } => handle_find_module(
            options,
            shared_mem,
            node_modules_containers,
            &moduleref,
            &filename,
        ),
        server_prot::request::Command::FORCE_RECHECK {
            files,
            focus,
            missed_changes,
            changed_mergebase,
        } => Ok(handle_force_recheck(
            files,
            focus,
            missed_changes,
            changed_mergebase,
        )),
        server_prot::request::Command::GET_DEF {
            input,
            line,
            r#char,
            wait_for_recheck: _,
        } => handle_get_def(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &input,
            line as u32,
            r#char as u32,
        ),
        server_prot::request::Command::GRAPH_DEP_GRAPH {
            root,
            strip_root,
            outfile,
            types_only,
        } => handle_graph_dep_graph(env, &root, strip_root, &outfile, types_only),
        server_prot::request::Command::INFER_TYPE(input) => {
            handle_infer_type(options, env, shared_mem, node_modules_containers, &input)
        }
        server_prot::request::Command::INLAY_HINT(input) => {
            handle_inlay_hint(options, env, shared_mem, node_modules_containers, &input)
        }
        server_prot::request::Command::TYPE_OF_NAME(input) => {
            handle_type_of_name(options, env, shared_mem, node_modules_containers, &input)
        }
        server_prot::request::Command::INSERT_TYPE {
            input,
            target,
            verbose: _,
            location_is_strict,
            wait_for_recheck: _,
            omit_targ_defaults,
        } => handle_insert_type(
            options,
            env,
            shared_mem,
            node_modules_containers,
            &input,
            &target,
            omit_targ_defaults,
            location_is_strict,
        ),
        server_prot::request::Command::RAGE { files } => {
            handle_rage(options, env, &shared_mem, Some(&files))
        }
        server_prot::request::Command::SAVE_STATE { out } => {
            let filename = match out {
                server_prot::request::SaveStateOut::File(path) => {
                    Some(path.to_string_lossy().to_string())
                }
                server_prot::request::SaveStateOut::Scm => {
                    flow_saved_state::output_filename(options)
                        .ok()
                        .map(|path| path.to_string_lossy().to_string())
                }
            };
            match filename {
                Some(filename) => handle_save_state(genv, env, &filename),
                None => Ok((
                    server_prot::response::Response::SAVE_STATE(Err(
                        "Failed to determine saved-state output filename".to_string(),
                    )),
                    None,
                )),
            }
        }
        server_prot::request::Command::STATUS { include_warnings } => {
            let mut options = options.clone();
            options.include_warnings = options.include_warnings || include_warnings;
            handle_status(&options, env, &shared_mem)
        }
        server_prot::request::Command::LLM_CONTEXT(input) => {
            handle_llm_context(options, env, shared_mem, node_modules_containers, &input)
        }
    }
}

pub fn handle_ephemeral_command_for_standalone_wrapped(
    genv: &server_env::Genv,
    env: &server_env::Env,
    command: server_prot::request::Command,
) -> EphemeralParallelizableResult {
    struct CommandSummaryGuard {
        cmd_str: String,
    }

    impl Drop for CommandSummaryGuard {
        fn drop(&mut self) {
            send_command_summary(&(), &self.cmd_str);
        }
    }

    let cmd_str = server_prot::request::to_string(&command);
    eprintln!("{}", cmd_str);
    flow_server_env::monitor_rpc::status_update(
        flow_server_env::server_status::Event::HandlingRequestStart,
    );
    let _guard = CommandSummaryGuard {
        cmd_str: cmd_str.clone(),
    };
    handle_ephemeral_command_for_standalone(genv, env, command)
}

fn find_code_actions(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    params: &lsp_types::CodeActionParams,
) -> (
    Result<Vec<lsp_types::CodeActionOrCommand>, String>,
    Option<lsp_prot::Json>,
) {
    let text_document = &params.text_document;
    let range = &params.range;
    let only = params.context.only.as_deref();
    let diagnostics = &params.context.diagnostics;
    if !flow_services_code_action::code_action_service::kind_is_supported(only) {
        return (Ok(vec![]), None);
    }
    let file_input = file_input_of_text_document_identifier(client_id, text_document);
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(msg)) => (Err(msg), None),
        Err(IdeFileError::Skipped(reason)) => {
            let extra_data = json_of_skipped(&reason);
            (Ok(vec![]), extra_data)
        }
        Ok((file_key, file_contents)) => {
            let intermediate_result = parse_contents(options, &file_contents, &file_key);
            let (file_artifacts_result, _did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.dupe(),
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_) => (Ok(vec![]), None),
                Ok((parse_artifacts, typecheck_artifacts)) => {
                    let uri = &text_document.uri;
                    let loc = lsp_range_to_flow_loc(Some(file_key.dupe()), range);
                    let lsp_init_params = persistent_connection::get_client(client_id)
                        .map(|client| persistent_connection::lsp_initialize_params(&client))
                        .unwrap_or_default();
                    let imports_ranked_usage = rank_autoimports_by_usage(options, client_id);
                    let scope_info = flow_analysis::scope_builder::program(
                        typecheck_artifacts.cx.enable_enums(),
                        false,
                        &parse_artifacts.ast,
                    );
                    let shared_mem_clone = shared_mem.clone();
                    let loc_of_aloc: Arc<dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc> =
                        Arc::new(move |aloc| shared_mem_clone.loc_of_aloc(aloc));
                    let shared_mem_clone2 = shared_mem.clone();
                    let get_ast_from_shared_mem: Arc<
                        dyn Fn(
                            &flow_parser::file_key::FileKey,
                        ) -> Option<
                            flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
                        >,
                    > = Arc::new(move |fk| shared_mem_clone2.get_ast(fk).map(|arc| (*arc).clone()));
                    let shared_mem_ts = shared_mem.clone();
                    let get_type_sig: Arc<
                        dyn Fn(
                            &flow_parser::file_key::FileKey,
                        ) -> Option<
                            flow_type_sig::packed_type_sig::Module<
                                flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
                            >,
                        >,
                    > = Arc::new(move |fk| {
                        shared_mem_ts.get_type_sig(fk).map(|arc| {
                            let bytes =
                                bincode::serde::encode_to_vec(&*arc, bincode::config::legacy())
                                    .expect("get_type_sig: serialize");
                            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                                .expect("get_type_sig: deserialize")
                                .0
                        })
                    });
                    let module_system_info = mk_module_system_info(options, shared_mem.clone());
                    let code_actions =
                        flow_services_code_action::code_action_service::code_actions_at_loc(
                            options,
                            &lsp_init_params,
                            imports_ranked_usage,
                            env,
                            loc_of_aloc,
                            get_ast_from_shared_mem,
                            get_type_sig,
                            &module_system_info,
                            &typecheck_artifacts.cx,
                            &parse_artifacts.file_sig,
                            &parse_artifacts.tolerable_errors,
                            &file_contents,
                            &parse_artifacts.ast,
                            &typecheck_artifacts.typed_ast,
                            &scope_info,
                            &parse_artifacts.parse_errors,
                            diagnostics,
                            only,
                            uri,
                            loc,
                        );
                    let extra_data = match &code_actions {
                        Err(_) => None,
                        Ok(actions) => {
                            let actions_json: Vec<serde_json::Value> = actions
                                .iter()
                                .map(|action| match action {
                                    lsp_types::CodeActionOrCommand::Command(cmd) => {
                                        serde_json::Value::String(cmd.title.clone())
                                    }
                                    lsp_types::CodeActionOrCommand::CodeAction(ca) => {
                                        serde_json::Value::String(ca.title.clone())
                                    }
                                })
                                .collect();
                            Some(serde_json::json!({
                                "actions": actions_json
                            }))
                        }
                    };
                    (code_actions, extra_data)
                }
            }
        }
    }
}

fn add_missing_imports(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    text_document: &lsp_types::TextDocumentIdentifier,
) -> Result<Vec<lsp_types::TextEdit>, String> {
    let file_input = file_input_of_text_document_identifier(client_id, text_document);
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    let file_key = file_key_of_file_input(options, env, &file_input);
    match file_input.content_of_file_input() {
        Err(msg) => Err(msg),
        Ok(file_contents) => {
            let uri = &text_document.uri;
            let intermediate_result = parse_contents(options, &file_contents, &file_key);
            let (file_artifacts_result, _did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key,
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_) => Ok(vec![]),
                Ok((ref parse_artifacts, ref typecheck_artifacts)) => {
                    let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
                    let module_system_info = mk_module_system_info(options, shared_mem.clone());
                    Ok(
                        flow_services_code_action::code_action_service::autofix_imports_lsp(
                            options,
                            env,
                            &loc_of_aloc,
                            &module_system_info,
                            &typecheck_artifacts.cx,
                            &parse_artifacts.ast,
                            uri,
                        ),
                    )
                }
            }
        }
    }
}

fn organize_imports(
    options: &Options,
    client_id: lsp_prot::ClientId,
    text_document: &lsp_types::TextDocumentIdentifier,
) -> Result<Vec<lsp_types::TextEdit>, String> {
    let file_input = file_input_of_text_document_identifier(client_id, text_document);
    let file_key = file_key_of_file_input_without_env(
        options,
        &std::collections::BTreeSet::new(),
        &file_input,
    );
    match file_input.content_of_file_input() {
        Err(msg) => Err(msg),
        Ok(file_contents) => {
            let (parse_artifacts, _parse_errors) =
                parse_contents(options, &file_contents, &file_key);
            match parse_artifacts {
                None => Ok(vec![]),
                Some(ParseArtifacts { ref ast, .. }) => Ok(
                    flow_services_code_action::code_action_service::organize_imports_fn(
                        options, ast,
                    ),
                ),
            }
        }
    }
}

fn mk_parallelizable(wait_for_recheck: Option<bool>, options: &Options) -> CommandHandler {
    let wait_for_recheck = wait_for_recheck.unwrap_or(options.wait_for_recheck);
    if wait_for_recheck {
        CommandHandler::HandleNonparallelizable
    } else {
        CommandHandler::HandleParallelizable
    }
}

fn get_ephemeral_handler(
    genv: &server_env::Genv,
    command: &server_prot::request::Command,
) -> CommandHandler {
    let options = &*genv.options;
    match command {
        server_prot::request::Command::APPLY_CODE_ACTION {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::AUTOCOMPLETE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::AUTOFIX_EXPORTS {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::AUTOFIX_MISSING_LOCAL_ANNOT {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::CHECK_FILE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::COVERAGE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::BATCH_COVERAGE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::CYCLE { .. } => CommandHandler::HandleNonparallelizable,
        server_prot::request::Command::DUMP_TYPES {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::FIND_MODULE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::FORCE_RECHECK { .. } => CommandHandler::HandleImmediately,
        server_prot::request::Command::GET_DEF {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::GRAPH_DEP_GRAPH { .. } => {
            CommandHandler::HandleNonparallelizable
        }
        server_prot::request::Command::INFER_TYPE(input) => {
            mk_parallelizable(input.wait_for_recheck, options)
        }
        server_prot::request::Command::INLAY_HINT(input) => {
            mk_parallelizable(input.wait_for_recheck, options)
        }
        server_prot::request::Command::TYPE_OF_NAME(input) => {
            mk_parallelizable(input.wait_for_recheck, options)
        }
        server_prot::request::Command::RAGE { .. } => mk_parallelizable(None, options),
        server_prot::request::Command::INSERT_TYPE {
            wait_for_recheck, ..
        } => mk_parallelizable(*wait_for_recheck, options),
        server_prot::request::Command::STATUS { .. } => CommandHandler::HandleNonparallelizable,
        server_prot::request::Command::SAVE_STATE { .. } => CommandHandler::HandleNonparallelizable,
        server_prot::request::Command::LLM_CONTEXT(input) => {
            mk_parallelizable(input.wait_for_recheck, options)
        }
    }
}

pub fn classify_ephemeral_command(
    genv: &server_env::Genv,
    command: &server_prot::request::Command,
) -> CommandHandler {
    get_ephemeral_handler(genv, command)
}

fn send_command_summary(_profiling: &(), name: &str) {
    flow_server_env::monitor_rpc::send_telemetry(lsp_prot::TelemetryFromServer::CommandSummary {
        name: name.to_string(),
        duration: 0.0,
    });
    flow_server_env::monitor_rpc::status_update(
        flow_server_env::server_status::Event::HandlingRequestEnd,
    );
}

fn send_ephemeral_response<T>(
    cmd_str: &str,
    request_id: &str,
    _client_context: &lsp_prot::LoggingContext,
    result: Result<
        (T, server_prot::response::Response, Option<lsp_prot::Json>),
        (String, Option<lsp_prot::Json>),
    >,
) -> Result<T, ()> {
    match result {
        Ok((ret, response, _json_data)) => {
            flow_server_env::monitor_rpc::respond_to_request(request_id.to_string(), response);
            eprintln!("Finished {}", cmd_str);
            Ok(ret)
        }
        Err((exn_str, _json_data)) => {
            flow_server_env::monitor_rpc::request_failed(request_id.to_string(), exn_str);
            Err(())
        }
    }
}

fn format_client_context(client_context: &lsp_prot::LoggingContext) -> String {
    let mut parts = Vec::new();
    if let Some(from) = client_context.from.as_deref() {
        parts.push(format!("from={from}"));
    }
    if let Some(agent_id) = client_context.agent_id.as_deref() {
        parts.push(format!("agent_id={agent_id}"));
    }
    if parts.is_empty() {
        String::new()
    } else {
        format!(" [{}]", parts.join(" "))
    }
}

fn handle_ephemeral_uncaught_exception<T>(
    cmd_str: &str,
    exn_str: String,
) -> Result<T, (String, Option<lsp_prot::Json>)> {
    let json_data = Some(serde_json::json!({ "exn": &exn_str }));
    eprintln!(
        "Uncaught exception while handling a request ({}): {}",
        cmd_str, &exn_str
    );
    Err((exn_str, json_data))
}

fn wrap_ephemeral_handler(
    genv: &server_env::Genv,
    request_id: &str,
    client_context: &lsp_prot::LoggingContext,
    cmd_str: &str,
    handler: impl FnOnce() -> Result<
        ((), server_prot::response::Response, Option<lsp_prot::Json>),
        EphemeralHandlerError,
    >,
) -> Result<(), EphemeralWrapError> {
    eprintln!("{cmd_str}{}", format_client_context(client_context));
    flow_server_env::monitor_rpc::status_update(
        flow_server_env::server_status::Event::HandlingRequestStart,
    );
    let _should_print_summary = genv.options.profile;
    let result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(handler)).unwrap_or_else(|e| {
            let exn_str = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic".to_string()
            };
            handle_ephemeral_uncaught_exception(cmd_str, exn_str)
                .map_err(EphemeralHandlerError::Failure)
        });
    send_command_summary(&(), cmd_str);
    let to_send: Result<
        ((), server_prot::response::Response, Option<lsp_prot::Json>),
        (String, Option<lsp_prot::Json>),
    > = match result {
        Ok(ok) => Ok(ok),
        Err(EphemeralHandlerError::Failure(f)) => Err(f),
        Err(EphemeralHandlerError::Canceled) => return Err(EphemeralWrapError::Canceled),
    };
    send_ephemeral_response(cmd_str, request_id, client_context, to_send)
        .map_err(|()| EphemeralWrapError::SendFailed)?;
    Ok(())
}

// The error type the handler closure passed to `wrap_ephemeral_handler` may
// return.
enum EphemeralHandlerError {
    Failure((String, Option<lsp_prot::Json>)),
    Canceled,
}

// The error returned by `wrap_ephemeral_handler` to its caller. `SendFailed`
// indicates `send_ephemeral_response` failed to deliver the result; `Canceled`
// indicates the workload signaled cancellation and no response was sent.
enum EphemeralWrapError {
    SendFailed,
    Canceled,
}

fn wrap_immediate_ephemeral_handler(
    genv: &server_env::Genv,
    request_id: &str,
    client_context: &lsp_prot::LoggingContext,
    cmd_str: &str,
    handler: impl FnOnce() -> Result<
        ((), server_prot::response::Response, Option<lsp_prot::Json>),
        (String, Option<lsp_prot::Json>),
    >,
) -> Result<(), ()> {
    eprintln!("{cmd_str}{}", format_client_context(client_context));
    let _should_print_summary = genv.options.profile;
    let result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(handler)).unwrap_or_else(|e| {
            let exn_str = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "unknown panic".to_string()
            };
            handle_ephemeral_uncaught_exception(cmd_str, exn_str)
        });
    send_command_summary(&(), cmd_str);
    send_ephemeral_response(cmd_str, request_id, client_context, result)?;
    Ok(())
}

fn handle_ephemeral_immediately_unsafe(
    workload: impl FnOnce() -> (server_prot::response::Response, Option<lsp_prot::Json>),
) -> ((), server_prot::response::Response, Option<lsp_prot::Json>) {
    let (response, json_data) = workload();
    ((), response, json_data)
}

fn handle_ephemeral_immediately(
    genv: &server_env::Genv,
    request_id: &str,
    client_context: &lsp_prot::LoggingContext,
    cmd_str: &str,
    workload: impl FnOnce() -> (server_prot::response::Response, Option<lsp_prot::Json>),
) -> Result<(), ()> {
    wrap_immediate_ephemeral_handler(genv, request_id, client_context, cmd_str, || {
        Ok(handle_ephemeral_immediately_unsafe(workload))
    })
}

fn run_command_in_parallel(
    env: &server_env::Env,
    name: &str,
    workload: impl FnOnce(&server_env::Env) -> EphemeralParallelizableResult,
    mk_workload: impl FnOnce() -> flow_server_env::workload_stream::ParallelizableWorkload,
) -> EphemeralParallelizableResult {
    match workload(env) {
        Ok((response, json_data)) => Ok((response, json_data)),
        Err(WorkloadCanceled) => {
            eprintln!(
                "Command successfully canceled. Requeuing the command for after the next recheck."
            );
            server_monitor_listener_state::defer_parallelizable_workload(name, mk_workload());
            Err(WorkloadCanceled)
        }
    }
}

fn run_command_in_serial(
    genv: &Arc<server_env::Genv>,
    mut env: server_env::Env,
    workload: &mut dyn FnMut(&server_env::Env) -> EphemeralNonparallelizableResult,
) -> (server_env::Env, EphemeralNonparallelizableResult) {
    loop {
        match workload(&env) {
            Ok((response, json_data)) => return (env, Ok((response, json_data))),
            Err(WorkloadCanceled) => {
                eprintln!(
                    "Command successfully canceled. Running a recheck before restarting the command"
                );
                let node_modules_containers_arc = std::sync::Arc::new(std::sync::RwLock::new(
                    (*genv.node_modules_containers).clone(),
                ));
                let (_recheck_profiling, new_env) = flow_server_rechecker::rechecker::recheck_loop(
                    genv,
                    env,
                    &genv.shared_mem,
                    &node_modules_containers_arc,
                );
                env = new_env;
                eprintln!("Now restarting the command");
                continue;
            }
        }
    }
}

fn handle_parallelizable_ephemeral_unsafe(
    env: &server_env::Env,
    cmd_str: &str,
    workload: impl FnOnce(&server_env::Env) -> EphemeralParallelizableResult,
    mk_workload: impl FnOnce() -> flow_server_env::workload_stream::ParallelizableWorkload,
) -> Result<((), server_prot::response::Response, Option<lsp_prot::Json>), EphemeralHandlerError> {
    match run_command_in_parallel(env, cmd_str, workload, mk_workload) {
        Ok((response, json_data)) => Ok(((), response, json_data)),
        Err(WorkloadCanceled) => Err(EphemeralHandlerError::Canceled),
    }
}

fn handle_parallelizable_ephemeral(
    genv: Arc<server_env::Genv>,
    request_id: String,
    client_context: lsp_prot::LoggingContext,
    cmd_str: String,
    command: server_prot::request::Command,
) -> flow_server_env::workload_stream::ParallelizableWorkload {
    let parallelizable_workload_should_be_cancelled: Box<dyn Fn() -> bool + Send> =
        Box::new(|| false);
    let parallelizable_workload_handler: flow_server_env::workload_stream::ParallelizableWorkloadHandler = Box::new(
        move |env: &server_env::Env| {
            let workload_command = command.clone();
            let workload_genv = genv.clone();
            let mk_workload = || {
                handle_parallelizable_ephemeral(
                    genv.clone(),
                    request_id.clone(),
                    client_context.clone(),
                    cmd_str.clone(),
                    command.clone(),
                )
            };
            let result = wrap_ephemeral_handler(&genv, &request_id, &client_context, &cmd_str, || {
                handle_parallelizable_ephemeral_unsafe(
                    env,
                    &cmd_str,
                    |env| handle_ephemeral_command_for_standalone(&workload_genv, env, workload_command),
                    mk_workload,
                )
            });
            match result {
                Ok(()) => {}
                Err(EphemeralWrapError::SendFailed) => {}
                Err(EphemeralWrapError::Canceled) => {
                    // It's fine for parallelizable commands to be canceled
                    // they'll be run again later"
                }
            }
        },
    );
    flow_server_env::workload_stream::ParallelizableWorkload {
        parallelizable_workload_should_be_cancelled,
        parallelizable_workload_handler,
    }
}

// Returns the post-recheck `env` alongside the result so that callers (which
// must hand the env back to the WorkloadStream) can sequence env-flow
// without relying on side effects.
fn handle_nonparallelizable_ephemeral_unsafe(
    genv: &Arc<server_env::Genv>,
    env: server_env::Env,
    workload: &mut dyn FnMut(&server_env::Env) -> EphemeralNonparallelizableResult,
) -> (
    server_env::Env,
    Result<((), server_prot::response::Response, Option<lsp_prot::Json>), EphemeralHandlerError>,
) {
    let (env, result) = run_command_in_serial(genv, env, workload);
    let mapped = match result {
        Ok((response, json_data)) => Ok(((), response, json_data)),
        Err(WorkloadCanceled) => Err(EphemeralHandlerError::Canceled),
    };
    (env, mapped)
}

fn handle_nonparallelizable_ephemeral(
    genv: Arc<server_env::Genv>,
    request_id: String,
    client_context: lsp_prot::LoggingContext,
    cmd_str: String,
    command: server_prot::request::Command,
) -> flow_server_env::workload_stream::Workload {
    let workload_should_be_cancelled: Box<dyn Fn() -> bool + Send> = Box::new(|| false);
    let workload_handler: flow_server_env::workload_stream::WorkloadHandler =
        Box::new(move |env: server_env::Env| {
            let workload_genv = genv.clone();
            let mut workload_fn =
                |inner_env: &server_env::Env| -> EphemeralNonparallelizableResult {
                    handle_ephemeral_command_for_standalone(
                        &workload_genv,
                        inner_env,
                        command.clone(),
                    )
                };
            let (env, mapped) =
                handle_nonparallelizable_ephemeral_unsafe(&genv, env, &mut workload_fn);
            match wrap_ephemeral_handler(&genv, &request_id, &client_context, &cmd_str, || mapped) {
                Ok(()) => {}
                Err(EphemeralWrapError::SendFailed) => {}
                Err(EphemeralWrapError::Canceled) => {}
            }
            env
        });
    flow_server_env::workload_stream::Workload {
        workload_should_be_cancelled,
        workload_handler,
    }
}

fn handle_ephemeral_immediate_command(
    command: server_prot::request::Command,
) -> (server_prot::response::Response, Option<lsp_prot::Json>) {
    match command {
        server_prot::request::Command::FORCE_RECHECK {
            files,
            focus,
            missed_changes,
            changed_mergebase,
        } => handle_force_recheck(files, focus, missed_changes, changed_mergebase),
        _ => unreachable!(
            "unexpected immediate ephemeral command: {}",
            server_prot::request::to_string(&command)
        ),
    }
}

pub fn enqueue_or_handle_ephemeral(
    genv: &Arc<server_env::Genv>,
    (request_id, command_with_context): (monitor_prot::RequestId, ServerCommandWithContext),
) {
    let ServerCommandWithContext {
        client_logging_context: client_context,
        command,
    } = command_with_context;
    let cmd_str = format!(
        "{}: {}",
        request_id,
        server_prot::request::to_string(&command)
    );
    match get_ephemeral_handler(genv, &command) {
        CommandHandler::HandleImmediately => {
            let _result = handle_ephemeral_immediately(
                genv,
                &request_id,
                &client_context,
                &cmd_str,
                move || handle_ephemeral_immediate_command(command),
            );
            match _result {
                Ok(()) | Err(()) => {}
            }
        }
        CommandHandler::HandleParallelizable => {
            let queued_client_context = client_context.clone();
            let workload = handle_parallelizable_ephemeral(
                genv.clone(),
                request_id.clone(),
                queued_client_context,
                cmd_str.clone(),
                command,
            );
            server_monitor_listener_state::push_new_parallelizable_workload(&cmd_str, workload);
        }
        CommandHandler::HandleNonparallelizable => {
            let queued_client_context = client_context.clone();
            let workload = handle_nonparallelizable_ephemeral(
                genv.clone(),
                request_id,
                queued_client_context,
                cmd_str.clone(),
                command,
            );
            server_monitor_listener_state::push_new_workload(&cmd_str, workload);
        }
    }
}

fn with_error(
    stack: Option<String>,
    reason: String,
    metadata: lsp_prot::Metadata,
) -> lsp_prot::Metadata {
    let stack = stack.unwrap_or_default();
    let error_info = Some((lsp_prot::ErrorKind::ExpectedError, reason, stack));
    lsp_prot::Metadata {
        error_info,
        ..metadata
    }
}

fn handle_persistent_canceled(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let e = lsp_error::T {
        code: lsp_error::Code::RequestCancelled,
        message: "cancelled".to_string(),
        data: None,
    };
    let response = LspMessage::ResponseMessage(id, LspResult::ErrorResult(e, String::new()));
    let metadata = with_error(Some(String::new()), "cancelled".to_string(), metadata);
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn cancelled_request_id_opt(request: &lsp_prot::Request) -> Option<lsp_prot::LspId> {
    match request {
        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(id, _)) => {
            let cancellation_requests = server_monitor_listener_state::cancellation_requests()
                .lock()
                .unwrap();
            let id_str = match id {
                lsp_types::NumberOrString::Number(n) => n.to_string(),
                lsp_types::NumberOrString::String(s) => s.clone(),
            };
            if cancellation_requests.contains(&id_str) {
                Some(id.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn check_if_cancelled(
    request: &lsp_prot::Request,
    metadata: lsp_prot::Metadata,
) -> Option<(lsp_prot::Response, lsp_prot::Metadata)> {
    cancelled_request_id_opt(request).map(|id| {
        eprintln!(
            "Skipping canceled persistent request: {}",
            lsp_prot::string_of_request(request)
        );
        handle_persistent_canceled(id, metadata)
    })
}

fn handle_persistent_uncaught_exception(
    request: lsp_prot::Request,
    exception_constructor: String,
    stack: String,
) -> lsp_prot::Response {
    lsp_prot::Response::UncaughtException {
        request,
        exception_constructor,
        stack,
    }
}

fn persistent_profiling_json(duration_secs: f64) -> lsp_prot::ProfilingFinished {
    serde_json::json!({
        "timing": {
            "total_wall_duration": duration_secs,
        },
        "memory": {},
    })
}

fn persistent_server_logging_context() -> lsp_prot::LoggingContext {
    lsp_prot::LoggingContext {
        from: None,
        agent_id: None,
    }
}

fn send_persistent_response<T>(
    client_id: lsp_prot::ClientId,
    profiling: lsp_prot::ProfilingFinished,
    result: (T, lsp_prot::Response, lsp_prot::Metadata),
) -> T {
    let (ret, lsp_response, metadata) = result;
    let metadata = lsp_prot::Metadata {
        server_profiling: Some(profiling),
        server_logging_context: Some(persistent_server_logging_context()),
        ..metadata
    };
    let response = (lsp_response.clone(), metadata);
    if let Some(client) = persistent_connection::get_client(client_id) {
        persistent_connection::send_response(response, &client);
    }
    eprintln!(
        "Persistent response: {}",
        lsp_prot::string_of_response(&lsp_response)
    );
    ret
}

fn wrap_persistent_handler<T: Default>(
    options: &Options,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    default_ret: T,
    handler: impl FnOnce() -> Result<(T, lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled>,
) -> Result<T, WorkloadCanceled> {
    let (request, metadata) = request;
    if persistent_connection::get_client(client_id).is_none() {
        eprintln!(
            "Unknown persistent client {}. Maybe connection went away?",
            client_id
        );
        return Ok(default_ret);
    }
    eprintln!(
        "Persistent request: {}",
        lsp_prot::string_of_request(&request)
    );
    flow_server_env::monitor_rpc::status_update(
        flow_server_env::server_status::Event::HandlingRequestStart,
    );
    let start = std::time::Instant::now();
    let _should_print_summary = options.profile;
    let result = match check_if_cancelled(&request, metadata.clone()) {
        Some((response, json_data)) => (default_ret, response, json_data),
        None => match std::panic::catch_unwind(std::panic::AssertUnwindSafe(handler)) {
            Ok(Ok(triple)) => triple,
            Ok(Err(WorkloadCanceled)) => return Err(WorkloadCanceled),
            Err(e) => {
                let exn_str = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else {
                    "unknown panic".to_string()
                };
                let response =
                    handle_persistent_uncaught_exception(request.clone(), exn_str.clone(), exn_str);
                (T::default(), response, metadata)
            }
        },
    };
    let profiling = persistent_profiling_json(start.elapsed().as_secs_f64());
    let ret = send_persistent_response(client_id, profiling.clone(), result);
    send_command_summary(&(), &lsp_prot::string_of_request(&request));
    Ok(ret)
}

fn handle_parallelizable_persistent_unsafe(
    _name: &str,
) -> ((), lsp_prot::Response, lsp_prot::Metadata) {
    (
        (),
        lsp_prot::Response::LspFromServer(None),
        lsp_prot::empty_metadata(),
    )
}

fn handle_parallelizable_persistent(
    genv: &'static server_env::Genv,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    workload: PersistentParallelizableWorkload,
) -> flow_server_env::workload_stream::ParallelizableWorkload {
    let request_for_cancel = request.0.clone();
    let parallelizable_workload_should_be_cancelled: Box<dyn Fn() -> bool + Send> =
        Box::new(move || cancelled_request_id_opt(&request_for_cancel).is_some());
    let parallelizable_workload_handler: flow_server_env::workload_stream::ParallelizableWorkloadHandler = Box::new(
        move |env: &server_env::Env| {
            let _result: Result<(), WorkloadCanceled> =
                wrap_persistent_handler(&genv.options, client_id, request, (), || {
                    let (response, metadata) = workload(env)?;
                    Ok(((), response, metadata))
                });
        },
    );
    flow_server_env::workload_stream::ParallelizableWorkload {
        parallelizable_workload_should_be_cancelled,
        parallelizable_workload_handler,
    }
}

fn handle_nonparallelizable_persistent_unsafe(
    _genv: &server_env::Genv,
    _env: server_env::Env,
) -> server_env::Env {
    _env
}

fn handle_nonparallelizable_persistent(
    genv: &'static server_env::Genv,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    workload: PersistentNonparallelizableWorkload,
) -> flow_server_env::workload_stream::Workload {
    let request_for_cancel = request.0.clone();
    let workload_should_be_cancelled: Box<dyn Fn() -> bool + Send> =
        Box::new(move || cancelled_request_id_opt(&request_for_cancel).is_some());
    let workload_handler: flow_server_env::workload_stream::WorkloadHandler =
        Box::new(move |env: server_env::Env| {
            let _: Result<(), WorkloadCanceled> =
                wrap_persistent_handler(&genv.options, client_id, request, (), || {
                    let (response, metadata) = workload(&env)?;
                    Ok(((), response, metadata))
                });
            env
        });
    flow_server_env::workload_stream::Workload {
        workload_should_be_cancelled,
        workload_handler,
    }
}

fn did_open(env: &server_env::Env, client_id: lsp_prot::ClientId) {
    let (errors, warnings) = flow_server_env::error_collator::get_with_separate_warnings(env);
    if let Some(client) = persistent_connection::get_client(client_id) {
        persistent_connection::send_errors_if_subscribed(
            &client,
            lsp_prot::ErrorsReason::EnvChange,
            &errors,
            warnings,
        );
    }
}

fn did_close(env: &server_env::Env, client_id: lsp_prot::ClientId) {
    let (errors, warnings) = flow_server_env::error_collator::get_with_separate_warnings(env);
    if let Some(client) = persistent_connection::get_client(client_id) {
        persistent_connection::send_errors_if_subscribed(
            &client,
            lsp_prot::ErrorsReason::EnvChange,
            &errors,
            warnings,
        );
    }
}

fn keyvals_of_json(json: Option<lsp_prot::Json>) -> Vec<(String, lsp_prot::Json)> {
    match json {
        None => vec![],
        Some(lsp_prot::Json::Object(keyvals)) => keyvals.into_iter().collect(),
        Some(json) => vec![("json_data".to_string(), json)],
    }
}

fn with_data(
    extra_data: Option<lsp_prot::Json>,
    metadata: lsp_prot::Metadata,
) -> lsp_prot::Metadata {
    let mut new_extra_data = metadata.extra_data.clone();
    new_extra_data.extend(keyvals_of_json(extra_data));
    lsp_prot::Metadata {
        extra_data: new_extra_data,
        ..metadata
    }
}

fn mk_lsp_error_response(
    id: Option<lsp_prot::LspId>,
    reason: String,
    stack: Option<String>,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let metadata = with_error(stack.clone(), reason, metadata);
    let (_, reason, stack) = metadata.error_info.as_ref().unwrap();
    let message = match id {
        Some(id) => {
            eprintln!("Error: {}\n{}", reason, stack);
            let friendly_message = "Flow encountered an unexpected error while handling this request. See the Flow logs for more details.";
            let e = lsp_error::T {
                code: lsp_error::Code::UnknownErrorCode,
                message: friendly_message.to_string(),
                data: None,
            };
            LspMessage::ResponseMessage(id, LspResult::ErrorResult(e, stack.clone()))
        }
        None => {
            let text = format!("{} [UnknownErrorCode]\n{}", reason, stack);
            LspMessage::NotificationMessage(LspNotification::TelemetryNotification(
                lsp_types::LogMessageParams {
                    typ: MessageType::ERROR,
                    message: text,
                },
            ))
        }
    };
    (lsp_prot::Response::LspFromServer(Some(message)), metadata)
}

fn handle_persistent_subscribe(
    client_id: lsp_prot::ClientId,
    metadata: lsp_prot::Metadata,
    env: &server_env::Env,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (current_errors, current_warnings) =
        flow_server_env::error_collator::get_with_separate_warnings(env);
    if let Some(client) = persistent_connection::get_client(client_id) {
        persistent_connection::subscribe_client(&client, &current_errors, current_warnings);
    }
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn enqueue_did_open_files(files: &[(String, String)]) {
    let mut pending = DID_OPEN_PENDING_FILES.lock().unwrap();
    for (filename, file_content) in files {
        pending.insert(filename.clone(), file_content.clone());
    }
}

fn get_and_clear_did_open_files() -> Vec<(String, String)> {
    let mut pending = DID_OPEN_PENDING_FILES.lock().unwrap();
    let files = pending
        .iter()
        .map(|(filename, file_content)| (filename.clone(), file_content.clone()))
        .collect();
    pending.clear();
    files
}

fn handle_persistent_did_open_notification(
    client_id: lsp_prot::ClientId,
    metadata: lsp_prot::Metadata,
    env: &server_env::Env,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    if !get_and_clear_did_open_files().is_empty() {
        did_open(env, client_id);
    }
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_did_open_notification_no_op(
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_did_change_notification(
    client_id: lsp_prot::ClientId,
    params: lsp_types::DidChangeTextDocumentParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let lsp_types::DidChangeTextDocumentParams {
        text_document,
        content_changes,
    } = params;
    let uri = text_document.uri;
    let filename = lsp_uri_to_flow_path(&uri);
    let Some(client) = persistent_connection::get_client(client_id) else {
        return mk_lsp_error_response(
            None,
            format!("File {} wasn't open to change", filename),
            Some(String::new()),
            metadata,
        );
    };
    match persistent_connection::client_did_change(&client, &filename, &content_changes) {
        Ok(()) => (lsp_prot::Response::LspFromServer(None), metadata),
        Err((reason, stack)) => mk_lsp_error_response(None, reason, Some(stack), metadata),
    }
}

fn handle_persistent_did_save_notification(
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_did_close_notification(
    client_id: lsp_prot::ClientId,
    metadata: lsp_prot::Metadata,
    env: &server_env::Env,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    did_close(env, client_id);
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_did_close_notification_no_op(
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_cancel_notification(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let mut cancellation_requests = server_monitor_listener_state::cancellation_requests()
        .lock()
        .unwrap();
    let id_str = match &id {
        lsp_types::NumberOrString::Number(n) => n.to_string(),
        lsp_types::NumberOrString::String(s) => s.clone(),
    };
    cancellation_requests.remove(&id_str);
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_did_change_configuration_notification(
    client_id: lsp_prot::ClientId,
    params: lsp_types::DidChangeConfigurationParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let settings = params.settings;
    match &settings {
        serde_json::Value::Null => {}
        _ => {
            let Some(client) = persistent_connection::get_client(client_id) else {
                return (lsp_prot::Response::LspFromServer(None), metadata);
            };
            let mut config = persistent_connection::client_config(&client);
            let suggest = settings.get("suggest");
            if let Some(suggest_obj) = suggest {
                if let Some(auto_imports) = suggest_obj.get("autoImports").and_then(|v| v.as_bool())
                {
                    config.suggest_autoimports = auto_imports;
                }
                config.rank_autoimports_by_usage = match suggest_obj.get("rankAutoimportsByUsage") {
                    Some(serde_json::Value::String(s)) if s == "true" => {
                        persistent_connection::client_config::ClientToggle::True
                    }
                    Some(serde_json::Value::Bool(true)) => {
                        persistent_connection::client_config::ClientToggle::True
                    }
                    Some(serde_json::Value::String(s)) if s == "false" => {
                        persistent_connection::client_config::ClientToggle::False
                    }
                    Some(serde_json::Value::Bool(false)) => {
                        persistent_connection::client_config::ClientToggle::False
                    }
                    _ => persistent_connection::client_config::ClientToggle::Default,
                };
                config.show_suggest_ranking_info = suggest_obj
                    .get("showRankingInfo")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
            }
            persistent_connection::client_did_change_configuration(&client, config);
        }
    }
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn handle_persistent_get_def(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::GotoDefinitionParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled> {
    let file_input = match file_input {
        Some(file_input) => file_input.clone(),
        None => {
            file_input_of_text_document_position(client_id, &params.text_document_position_params)
        }
    };
    let line = params.text_document_position_params.position.line + 1;
    let col = params.text_document_position_params.position.character;
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    let (result, extra_data) = get_def(
        options,
        env,
        type_parse_artifacts_cache.as_ref(),
        shared_mem,
        node_modules_containers,
        &file_input,
        line,
        col,
    )?;
    let metadata = with_data(extra_data, metadata);
    match result {
        Ok(locs) => {
            let default_uri = &params.text_document_position_params.text_document.uri;
            let locations: Vec<lsp_types::Location> = locs
                .iter()
                .map(|loc| {
                    let uri = loc
                        .source
                        .as_ref()
                        .and_then(|f| lsp_types::Url::from_file_path(f.to_path_buf()).ok())
                        .unwrap_or_else(|| default_uri.clone());
                    lsp_types::Location {
                        uri,
                        range: loc_to_lsp_range(loc),
                    }
                })
                .collect();
            let response = LspMessage::ResponseMessage(id, LspResult::DefinitionResult(locations));
            Ok((lsp_prot::Response::LspFromServer(Some(response)), metadata))
        }
        Err(reason) => Ok(mk_lsp_error_response(Some(id), reason, None, metadata)),
    }
}

fn loc_to_vscode_linked_location_in_markdown(
    default_uri: &lsp_types::Url,
    loc: &flow_parser::loc::Loc,
) -> Option<String> {
    let source = &loc.source;
    let line = loc.start.line;
    let column = loc.start.column;
    match source {
        None => None,
        Some(file) => {
            let uri = lsp_types::Url::from_file_path(file.to_path_buf())
                .unwrap_or_else(|_| default_uri.clone());
            let lib = if file.is_lib_file() { "(lib) " } else { "" };
            let abs = file.to_absolute();
            let basename = std::path::Path::new(&abs)
                .file_name()
                .map(|f| f.to_string_lossy().to_string())
                .unwrap_or_else(|| abs.clone());
            let fragment = loc.start_pos_to_string_for_vscode_loc_uri_fragment();
            Some(format!(
                "[`{}{}:{}:{}`]({}{})",
                lib,
                basename,
                line,
                column,
                uri.as_str(),
                fragment,
            ))
        }
    }
}

fn handle_persistent_infer_type(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::HoverParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled> {
    let file_input = match file_input {
        Some(file_input) => file_input.clone(),
        None => {
            file_input_of_text_document_position(client_id, &params.text_document_position_params)
        }
    };
    let line = params.text_document_position_params.position.line + 1;
    let column = params.text_document_position_params.position.character;
    let include_refinement_info = refinement_info_on_hover(client_id);
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    let input = server_prot::infer_type_options::T {
        input: file_input.clone(),
        line: line as i32,
        r#char: column as i32,
        verbose: None,
        omit_targ_defaults: false,
        wait_for_recheck: None,
        verbose_normalizer: false,
        max_depth: 40,
        json: false,
        strip_root: None,
        expanded: false,
        debug_print_internal_repr: false,
        no_typed_ast_for_imports: false,
    };
    let (result, extra_data) = infer_type(
        options,
        env,
        type_parse_artifacts_cache.as_ref(),
        shared_mem.clone(),
        node_modules_containers,
        &input,
        include_refinement_info,
    )?;
    let metadata = with_data(extra_data, metadata);
    match result {
        Ok(infer_result) => {
            let default_uri = &params.text_document_position_params.text_document.uri;
            let location_range = loc_to_lsp_range(&infer_result.loc);
            let range = if infer_result.loc == flow_parser::loc::Loc::none() {
                None
            } else {
                Some(location_range)
            };
            let invalidation_info = {
                let invalidation_info: Vec<_> = infer_result
                    .refinement_invalidated
                    .iter()
                    .filter_map(|(loc, reason)| {
                        loc_to_vscode_linked_location_in_markdown(default_uri, loc).map(|loc| {
                            (
                                loc,
                                flow_common::refinement_invalidation::string_of_reason(*reason),
                            )
                        })
                    })
                    .collect();
                if invalidation_info.is_empty() {
                    vec![]
                } else {
                    let reasons_str = invalidation_info
                        .into_iter()
                        .map(|(loc, reason)| format!("{reason} at {loc}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    vec![lsp_types::MarkedString::String(format!(
                        "Refinement invalidated due to {reasons_str}. Refactor this property to a const variable to keep refinements."
                    ))]
                }
            };
            let refinement_info = {
                let refining_locs: Vec<_> = infer_result
                    .refining_locs
                    .iter()
                    .filter_map(|loc| loc_to_vscode_linked_location_in_markdown(default_uri, loc))
                    .collect();
                if refining_locs.is_empty() {
                    vec![]
                } else {
                    vec![lsp_types::MarkedString::String(format!(
                        "Refined at {}",
                        refining_locs.join(", ")
                    ))]
                }
            };
            let contents: Vec<lsp_types::MarkedString> = match &infer_result.tys {
                server_prot::response::infer_type::Payload::Friendly(Some(friendly)) => {
                    let types = vec![lsp_types::MarkedString::LanguageString(
                        lsp_types::LanguageString {
                            language: "flow".to_string(),
                            value: friendly.type_str.clone(),
                        },
                    )];
                    let refs = friendly
                        .refs
                        .as_ref()
                        .map(|refs| {
                            refs.iter()
                                .filter_map(|(name, loc)| {
                                    loc_to_vscode_linked_location_in_markdown(default_uri, loc).map(
                                        |loc_str| {
                                            lsp_types::MarkedString::String(format!(
                                                "`{name}` defined at {loc_str}"
                                            ))
                                        },
                                    )
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    let docs = infer_result
                        .documentation
                        .iter()
                        .cloned()
                        .map(lsp_types::MarkedString::String)
                        .collect::<Vec<_>>();
                    let mut contents = Vec::new();
                    contents.extend(invalidation_info);
                    contents.extend(types);
                    contents.extend(refs);
                    contents.extend(refinement_info);
                    contents.extend(docs);
                    if contents.is_empty() {
                        vec![lsp_types::MarkedString::String("?".to_string())]
                    } else {
                        contents
                    }
                }
                _ => {
                    let docs = infer_result
                        .documentation
                        .iter()
                        .cloned()
                        .map(lsp_types::MarkedString::String)
                        .collect::<Vec<_>>();
                    let mut contents = Vec::new();
                    contents.extend(invalidation_info);
                    contents.extend(refinement_info);
                    contents.extend(docs);
                    if contents.is_empty() {
                        vec![lsp_types::MarkedString::String("?".to_string())]
                    } else {
                        contents
                    }
                }
            };
            let r = match (&range, &infer_result.tys, &infer_result.documentation) {
                (None, server_prot::response::infer_type::Payload::Friendly(None), None) => None,
                _ => Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Array(contents),
                    range,
                }),
            };
            let response = LspMessage::ResponseMessage(id, LspResult::HoverResult(r));
            Ok((lsp_prot::Response::LspFromServer(Some(response)), metadata))
        }
        Err(reason) => Ok(mk_lsp_error_response(Some(id), reason, None, metadata)),
    }
}

fn handle_persistent_code_action_request(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::CodeActionParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (result, extra_data) = find_code_actions(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        params,
    );
    let metadata = with_data(extra_data, metadata);
    match result {
        Ok(code_actions) => {
            let response =
                LspMessage::ResponseMessage(id, LspResult::CodeActionResult(code_actions));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
    }
}

fn handle_persistent_autocomplete_lsp(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::CompletionParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled> {
    let client = persistent_connection::get_client(client_id);
    let client_config = client.as_ref().map(persistent_connection::client_config);
    let lsp_init_params = client
        .as_ref()
        .map(persistent_connection::lsp_initialize_params)
        .unwrap_or_default();
    let is_snippet_supported = lsp_helpers::supports_snippets(&lsp_init_params);
    let is_tags_supported = |tag| lsp_helpers::supports_tags(&lsp_init_params, tag);
    let is_preselect_supported = lsp_helpers::supports_preselect(&lsp_init_params);
    let is_label_detail_supported =
        lsp_helpers::supports_completion_item_label_details(&lsp_init_params);
    let is_insert_replace_supported =
        lsp_helpers::supports_completion_item_insert_replace(&lsp_init_params);
    let lsp_loc = &params.text_document_position;
    let line = lsp_loc.position.line as i32 + 1;
    let col = lsp_loc.position.character as i32;
    let trigger_character = params
        .context
        .as_ref()
        .and_then(|ctx| ctx.trigger_character.as_deref());
    let default_input;
    let file_input = match file_input {
        Some(fi) => fi,
        None => {
            default_input = file_input_of_text_document_position(client_id, lsp_loc);
            &default_input
        }
    };
    let imports = client_config
        .as_ref()
        .map(|client_config| {
            persistent_connection::client_config::suggest_autoimports(client_config)
        })
        .unwrap_or(true)
        && options.autoimports;
    let imports_ranked_usage = rank_autoimports_by_usage(options, client_id);
    let show_ranking_info = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::client_config(&client))
        .map(|client_config| {
            persistent_connection::client_config::show_suggest_ranking_info(&client_config)
        })
        .unwrap_or(false);
    let (result, extra_data) = autocomplete(
        options,
        env,
        shared_mem,
        node_modules_containers,
        Some(client_id),
        file_input,
        trigger_character,
        (line, col),
        imports,
        imports_ranked_usage,
        show_ranking_info,
    )?;
    let metadata = with_data(extra_data, metadata);
    match result {
        Ok((token, completions, token_loc, ac_type)) => {
            let file_key = token_loc
                .as_ref()
                .and_then(|token_loc| token_loc.source.clone());
            let (token_line, token_char) = match token_loc.as_ref() {
                None => (None, None),
                Some(token_loc) => (Some(token_loc.start.line), Some(token_loc.start.column)),
            };
            let autocomplete_session_length = match (token_line, token_char, file_key.clone()) {
                (Some(token_line), Some(token_char), file_key) => client.as_ref().map(|client| {
                    persistent_connection::autocomplete_session(
                        client,
                        &ac_type,
                        (token_line, token_char, file_key),
                    )
                }),
                _ => None,
            };
            let typed_len = token_char.map(|token_char| col - token_char);
            let metadata = with_data(
                Some(lsp_prot::Json::Object(
                    [
                        (
                            "session_requests".to_string(),
                            autocomplete_session_length.map_or(lsp_prot::Json::Null, |value| {
                                lsp_prot::Json::Number(serde_json::Number::from(value))
                            }),
                        ),
                        (
                            "typed_length".to_string(),
                            typed_len.map_or(lsp_prot::Json::Null, |value| {
                                lsp_prot::Json::Number(serde_json::Number::from(value))
                            }),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                )),
                metadata,
            );
            let result = flow_lsp_conversions::flow_completions_to_lsp(
                token.as_deref(),
                autocomplete_session_length,
                typed_len,
                &ac_type,
                is_snippet_supported,
                &is_tags_supported,
                is_preselect_supported,
                is_label_detail_supported,
                is_insert_replace_supported,
                completions,
            );
            let response = lsp_prot::Response::LspFromServer(Some(LspMessage::ResponseMessage(
                id,
                LspResult::CompletionResult(result),
            )));
            Ok((response, metadata))
        }
        Err(reason) => Ok(mk_lsp_error_response(Some(id), reason, None, metadata)),
    }
}

fn func_details_result_to_lsp(
    result: &server_prot::response::FuncDetailsResult,
) -> lsp_types::SignatureInformation {
    let doc_opt = |doc: &Option<String>| -> Option<lsp_types::Documentation> {
        doc.as_ref().map(|d| {
            lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: d.clone(),
            })
        })
    };
    match result {
        server_prot::response::FuncDetailsResult::SigHelpFunc {
            func_documentation,
            param_tys,
            return_ty,
        } => {
            let mut label_buf = String::from("(");
            let parameters: Vec<lsp_types::ParameterInformation> = param_tys
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    let label = format!("{}: {}", param.param_name, param.param_ty);
                    if i > 0 {
                        label_buf.push_str(", ");
                    }
                    label_buf.push_str(&label);
                    lsp_types::ParameterInformation {
                        label: lsp_types::ParameterLabel::Simple(label),
                        documentation: doc_opt(&param.param_documentation),
                    }
                })
                .collect();
            label_buf.push_str("): ");
            label_buf.push_str(return_ty);
            lsp_types::SignatureInformation {
                label: label_buf,
                documentation: doc_opt(func_documentation),
                parameters: Some(parameters),
                active_parameter: None,
            }
        }
        server_prot::response::FuncDetailsResult::SigHelpJsxAttr {
            documentation,
            name,
            ty,
            optional,
        } => {
            let opt_str = if *optional { "?" } else { "" };
            let label = format!("{}{}: {}", name, opt_str, ty);
            let label_buf = label.clone();
            let parameters = vec![lsp_types::ParameterInformation {
                label: lsp_types::ParameterLabel::Simple(label),
                documentation: doc_opt(documentation),
            }];
            lsp_types::SignatureInformation {
                label: label_buf,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: None,
            }
        }
    }
}

fn flow_signature_help_to_lsp(
    details: &Option<(Vec<server_prot::response::FuncDetailsResult>, i32)>,
) -> Option<lsp_types::SignatureHelp> {
    match details {
        None => None,
        Some((signatures, active_parameter)) => {
            let signatures: Vec<lsp_types::SignatureInformation> =
                signatures.iter().map(func_details_result_to_lsp).collect();
            Some(lsp_types::SignatureHelp {
                signatures,
                active_signature: Some(0),
                active_parameter: Some(*active_parameter as u32),
            })
        }
    }
}

fn handle_persistent_signaturehelp_lsp(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::SignatureHelpParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let lsp_loc = &params.text_document_position_params;
    let line = lsp_loc.position.line as i32 + 1;
    let col = lsp_loc.position.character as i32;
    let file_input = match file_input {
        Some(file_input) => file_input.clone(),
        None => file_input_of_text_document_position(client_id, lsp_loc),
    };
    let fn_content = match &file_input {
        FileInput::FileContent(filename, content) => Ok((filename.clone(), content.clone())),
        FileInput::FileName(filename) => match std::fs::read_to_string(filename) {
            Ok(content) => Ok((Some(filename.clone()), content)),
            Err(error) => Err((error.to_string(), format!("{error:?}"))),
        },
    };
    match fn_content {
        Err((reason, stack)) => mk_lsp_error_response(Some(id), reason, Some(stack), metadata),
        Ok((filename, contents)) => {
            let path = match filename {
                Some(filename) => {
                    flow_parser::file_key::FileKey::source_file_of_absolute(&filename)
                }
                None => flow_parser::file_key::FileKey::source_file_of_absolute("-"),
            };
            let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
                .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
            let intermediate_result = parse_contents(options, &contents, &path);
            let (file_artifacts_result, did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem.clone(),
                env.master_cx.clone(),
                path.dupe(),
                intermediate_result,
                node_modules_containers,
            );
            let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
            let metadata = with_data(
                Some(lsp_prot::Json::Object(json_props.into_iter().collect())),
                metadata,
            );
            match file_artifacts_result {
                Err(_parse_errors) => mk_lsp_error_response(
                    Some(id),
                    "Couldn't parse file in parse_artifacts".to_string(),
                    None,
                    metadata,
                ),
                Ok((parse_artifacts, typecheck_artifacts)) => {
                    let loc_of_aloc = |aloc: &flow_aloc::ALoc| -> flow_parser::loc::Loc {
                        shared_mem.loc_of_aloc(aloc)
                    };
                    let get_ast_from_shared_mem = |file_key: &flow_parser::file_key::FileKey| -> Option<
                        flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
                    > {
                        shared_mem.get_ast(file_key).map(|arc| (*arc).clone())
                    };
                    let cursor_loc = flow_parser::loc::Loc::cursor(Some(path.dupe()), line, col);
                    let func_details =
                        match flow_services_type_info::signature_help::find_signatures(
                            &loc_of_aloc,
                            &get_ast_from_shared_mem,
                            &typecheck_artifacts.cx,
                            parse_artifacts.file_sig.clone(),
                            &parse_artifacts.ast,
                            &typecheck_artifacts.typed_ast,
                            cursor_loc,
                        ) {
                            Ok(inner) => inner,
                            Err(_canceled) => {
                                return mk_lsp_error_response(
                                    Some(id),
                                    "Worker canceled".to_string(),
                                    None,
                                    metadata,
                                );
                            }
                        };
                    match func_details {
                        Ok(details) => {
                            let lsp_result = flow_signature_help_to_lsp(&details);
                            let r = LspResult::SignatureHelpResult(lsp_result);
                            let response = LspMessage::ResponseMessage(id, r);
                            let has_any_documentation = match &details {
                                None => false,
                                Some((details_list, _)) => details_list.iter().any(|d| match d {
                                    server_prot::response::FuncDetailsResult::SigHelpFunc {
                                        func_documentation,
                                        param_tys,
                                        ..
                                    } => {
                                        func_documentation.is_some()
                                            || param_tys
                                                .iter()
                                                .any(|p| p.param_documentation.is_some())
                                    }
                                    server_prot::response::FuncDetailsResult::SigHelpJsxAttr {
                                        documentation,
                                        ..
                                    } => documentation.is_some(),
                                }),
                            };
                            let extra_data = Some(serde_json::Value::Object(
                                vec![(
                                    "documentation".to_string(),
                                    serde_json::Value::Bool(has_any_documentation),
                                )]
                                .into_iter()
                                .collect(),
                            ));
                            (
                                lsp_prot::Response::LspFromServer(Some(response)),
                                with_data(extra_data, metadata),
                            )
                        }
                        Err(_) => mk_lsp_error_response(
                            Some(id),
                            "Failed to normalize type".to_string(),
                            None,
                            metadata,
                        ),
                    }
                }
            }
        }
    }
}

fn handle_persistent_workspace_symbol(
    options: &Options,
    env: &server_env::Env,
    id: lsp_prot::LspId,
    params: lsp_types::WorkspaceSymbolParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let query = &params.query;
    let results = if query.len() < options.autoimports_min_characters as usize {
        vec![]
    } else {
        match &env.exports {
            None => vec![],
            Some(exports) => {
                let mut search_options = export_search::default_options();
                search_options.max_results = 100;
                search_options.num_threads = std::cmp::max(
                    1,
                    std::thread::available_parallelism().map_or(1, |n| n.get().saturating_sub(2)),
                );
                search_options.weighted = true;
                let mut exports_clone = exports.clone();
                let SearchResults {
                    results,
                    is_incomplete: _,
                } = export_search::search_both_values_and_types(
                    Some(&search_options),
                    query,
                    &mut exports_clone,
                );
                results
                    .into_iter()
                    .filter_map(|result| {
                        let flow_services_export::export_search_types::SearchResultScored {
                            search_result:
                                flow_services_export::export_search_types::SearchResult {
                                    name,
                                    source,
                                    kind: _,
                                },
                            score: _,
                            weight: _,
                        } = result;
                        match source {
                            flow_services_export::export_index::Source::Global
                            | flow_services_export::export_index::Source::Builtin(_) => None,
                            flow_services_export::export_index::Source::FileKey(file) => {
                                let path = file.to_path_buf();
                                match lsp_types::Url::from_file_path(&path) {
                                    Err(_) => None,
                                    Ok(uri) => Some(lsp_mapper::workspace_symbol_information::T {
                                        name: name.to_string(),
                                        kind: lsp_types::SymbolKind::VARIABLE,
                                        location: lsp_types::TextDocumentIdentifier { uri },
                                        container_name: None,
                                    }),
                                }
                            }
                        }
                    })
                    .collect()
            }
        }
    };
    let r = LspResult::WorkspaceSymbolResult(
        lsp_mapper::workspace_symbol_result::T::WorkspaceSymbolInformation(results),
    );
    let response = LspMessage::ResponseMessage(id, r);
    (
        lsp_prot::Response::LspFromServer(Some(response)),
        with_data(None, metadata),
    )
}

fn get_file_artifacts(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    pos: &lsp_types::TextDocumentPositionParams,
    file_input: Option<&FileInput>,
) -> (
    Result<Option<(FileArtifacts<'static>, flow_parser::file_key::FileKey)>, String>,
    Option<lsp_prot::Json>,
) {
    let default_file_input;
    let file_input = match file_input {
        Some(file_input) => file_input,
        None => {
            default_file_input = file_input_of_text_document_position(client_id, pos);
            &default_file_input
        }
    };
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    match of_file_input(options, env, file_input) {
        Err(IdeFileError::Failed(reason)) => (Err(reason), None),
        Err(IdeFileError::Skipped(reason)) => (Ok(None), json_of_skipped(&reason)),
        Ok((file_key, content)) => {
            let intermediate_result = parse_contents(options, &content, &file_key);
            let (file_artifacts_result, did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem,
                env.master_cx.clone(),
                file_key.dupe(),
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_parse_errors) => {
                    let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
                    (
                        Err("Couldn't parse file in parse_artifacts".to_string()),
                        Some(lsp_prot::Json::Object(json_props.into_iter().collect())),
                    )
                }
                Ok(file_artifacts) => (Ok(Some((file_artifacts, file_key))), None),
            }
        }
    }
}

fn find_local_references<'cx>(
    loc_of_aloc: &dyn Fn(&flow_aloc::ALoc) -> flow_parser::loc::Loc,
    file_artifacts: &FileArtifacts<'cx>,
    kind: flow_services_references::find_refs_types::Kind,
    file_key: &flow_parser::file_key::FileKey,
    pos: &lsp_types::TextDocumentPositionParams,
) -> (
    Result<
        (
            flow_services_get_def::get_def_types::DefInfo,
            flow_services_references::find_refs_types::FindRefsOk,
        ),
        String,
    >,
    Option<lsp_prot::Json>,
) {
    let parse_artifacts = &file_artifacts.0;
    let typecheck_artifacts = &file_artifacts.1;
    let line = pos.position.line + 1;
    let col = pos.position.character;
    let ast_info: flow_services_get_def::find_refs_utils::AstInfo = (
        parse_artifacts.ast.clone(),
        parse_artifacts.file_sig.clone(),
        parse_artifacts.docblock.clone(),
    );
    let refs_results = match flow_services_references::find_refs_js::find_local_refs(
        loc_of_aloc,
        file_key,
        &ast_info,
        &typecheck_artifacts.cx,
        &typecheck_artifacts.typed_ast,
        &typecheck_artifacts.obj_to_obj_map,
        kind,
        line,
        col,
    ) {
        Ok(r) => r,
        Err(_canceled) => Err("Worker canceled".to_string()),
    };
    let extra_data = match &refs_results {
        Ok((_, flow_services_references::find_refs_types::FindRefsOk::FoundReferences(_))) => {
            Some(serde_json::json!({"result": "SUCCESS"}))
        }
        Ok((
            _,
            flow_services_references::find_refs_types::FindRefsOk::NoDefinition(no_def_reason),
        )) => {
            let error = no_def_reason.as_deref().unwrap_or("No reason given");
            Some(serde_json::json!({"result": "BAD_LOC", "error": error}))
        }
        Err(msg) => Some(serde_json::json!({"result": "FAILURE", "error": msg})),
    };
    (refs_results, extra_data)
}

fn map_local_find_references_results<T>(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    f: &dyn Fn(&flow_services_references::find_refs_types::SingleRef) -> Option<T>,
    text_doc_position: &lsp_types::TextDocumentPositionParams,
    file_input: Option<&FileInput>,
) -> (Result<Vec<T>, String>, Option<lsp_prot::Json>) {
    let (file_artifacts_opt, extra_parse_data) = get_file_artifacts(
        options,
        env,
        shared_mem.clone(),
        node_modules_containers,
        client_id,
        text_doc_position,
        file_input,
    );
    match file_artifacts_opt {
        Ok(Some((file_artifacts, file_key))) => {
            let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
            let (local_refs, extra_data) = find_local_references(
                &loc_of_aloc,
                &file_artifacts,
                flow_services_references::find_refs_types::Kind::FindReferences,
                &file_key,
                text_doc_position,
            );
            let mapped_refs = match local_refs {
                Ok((
                    _,
                    flow_services_references::find_refs_types::FindRefsOk::FoundReferences(refs),
                )) => Ok(refs.iter().filter_map(f).collect()),
                Ok((_, flow_services_references::find_refs_types::FindRefsOk::NoDefinition(_))) => {
                    Ok(vec![])
                }
                Err(e) => Err(e),
            };
            (mapped_refs, extra_data)
        }
        Ok(None) => (Ok(vec![]), extra_parse_data),
        Err(e) => (Err(e), extra_parse_data),
    }
}

fn handle_global_find_references_local_only(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    text_doc_position: &lsp_types::TextDocumentPositionParams,
    include_declaration: bool,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let ref_to_location =
        |single_ref: &flow_services_references::find_refs_types::SingleRef| -> Option<
            lsp_types::Location,
        > {
            let (_, loc) = single_ref;
            let uri = loc
                .source
                .as_ref()
                .and_then(|f| lsp_types::Url::from_file_path(f.to_path_buf()).ok())?;
            Some(lsp_types::Location {
                uri,
                range: loc_to_lsp_range(loc),
            })
        };
    let (file_artifacts_opt, extra_parse_data) = get_file_artifacts(
        options,
        env,
        shared_mem.clone(),
        node_modules_containers,
        client_id,
        text_doc_position,
        None,
    );
    match file_artifacts_opt {
        Err(reason) => {
            let metadata = with_data(
                Some(serde_json::json!({"result": "FAILURE", "error": &reason})),
                metadata,
            );
            mk_lsp_error_response(Some(id), reason, None, metadata)
        }
        Ok(None) => {
            let metadata = with_data(extra_parse_data, metadata);
            let response = LspMessage::ResponseMessage(id, LspResult::FindReferencesResult(vec![]));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Ok(Some((file_artifacts, file_key))) => {
            let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
            let line = text_doc_position.position.line + 1;
            let col = text_doc_position.position.character;
            let ast_info: flow_services_get_def::find_refs_utils::AstInfo = (
                file_artifacts.0.ast.clone(),
                file_artifacts.0.file_sig.clone(),
                file_artifacts.0.docblock.clone(),
            );
            let inner = match flow_services_references::find_refs_js::find_local_refs(
                &loc_of_aloc,
                &file_key,
                &ast_info,
                &file_artifacts.1.cx,
                &file_artifacts.1.typed_ast,
                &file_artifacts.1.obj_to_obj_map,
                flow_services_references::find_refs_types::Kind::FindReferences,
                line,
                col,
            ) {
                Ok(inner) => inner,
                Err(_canceled) => Err("Worker canceled".to_string()),
            };
            match inner {
                Err(reason) => {
                    let metadata = with_data(
                        Some(serde_json::json!({"result": "FAILURE", "error": &reason})),
                        metadata,
                    );
                    mk_lsp_error_response(Some(id), reason, None, metadata)
                }
                Ok((_, flow_services_references::find_refs_types::FindRefsOk::NoDefinition(_))) => {
                    let metadata =
                        with_data(Some(serde_json::json!({"result": "BAD_LOC"})), metadata);
                    let response =
                        LspMessage::ResponseMessage(id, LspResult::FindReferencesResult(vec![]));
                    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
                }
                Ok((
                    def_info,
                    flow_services_references::find_refs_types::FindRefsOk::FoundReferences(refs),
                )) => {
                    let mut locs: Vec<lsp_types::Location> =
                        refs.iter().filter_map(ref_to_location).collect();
                    if !include_declaration {
                        let def_locs =
                            flow_services_get_def::get_def_utils::all_locs_of_def_info(&def_info);
                        locs.retain(|loc| {
                            !def_locs.iter().any(|dl| {
                                let lsp_range = loc_to_lsp_range(dl);
                                loc.range == lsp_range
                            })
                        });
                    }
                    locs.sort_by(|a, b| {
                        a.uri
                            .as_str()
                            .cmp(b.uri.as_str())
                            .then(a.range.start.line.cmp(&b.range.start.line))
                            .then(a.range.start.character.cmp(&b.range.start.character))
                    });
                    locs.dedup();
                    let metadata =
                        with_data(Some(serde_json::json!({"result": "SUCCESS"})), metadata);
                    let response =
                        LspMessage::ResponseMessage(id, LspResult::FindReferencesResult(locs));
                    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
                }
            }
        }
    }
}

fn handle_persistent_find_references(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::ReferenceParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let text_doc_position = &params.text_document_position;
    let include_declaration = params.context.include_declaration;
    handle_global_find_references_local_only(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        id,
        text_doc_position,
        include_declaration,
        metadata,
    )
}

fn handle_persistent_document_highlight(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::DocumentHighlightParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let ref_to_highlight =
        |single_ref: &flow_services_references::find_refs_types::SingleRef| -> Option<
            lsp_types::DocumentHighlight,
        > {
            let (_, loc) = single_ref;
            Some(lsp_types::DocumentHighlight {
                range: loc_to_lsp_range(loc),
                kind: Some(lsp_types::DocumentHighlightKind::TEXT),
            })
        };
    let (result, extra_data) = map_local_find_references_results(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        &ref_to_highlight,
        &params.text_document_position_params,
        file_input,
    );
    let metadata = with_data(extra_data, metadata);
    match result {
        Ok(result) => {
            let response =
                LspMessage::ResponseMessage(id, LspResult::DocumentHighlightResult(result));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
    }
}

fn handle_persistent_prepare_rename(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::TextDocumentPositionParams,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (file_artifacts_opt, extra_parse_data) = get_file_artifacts(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        params,
        file_input,
    );
    match file_artifacts_opt {
        Err(reason) => {
            let metadata = with_data(
                Some(serde_json::json!({"result": "FAILURE", "error": &reason})),
                metadata,
            );
            mk_lsp_error_response(Some(id), reason, None, metadata)
        }
        Ok(None) => {
            let metadata = with_data(extra_parse_data, metadata);
            let resp = LspMessage::ResponseMessage(id, LspResult::PrepareRenameResult(None));
            let metadata = with_data(Some(serde_json::json!({"result": "SUCCESS"})), metadata);
            (lsp_prot::Response::LspFromServer(Some(resp)), metadata)
        }
        Ok(Some((file_artifacts, file_key))) => {
            let line = params.position.line as i32 + 1;
            let col = params.position.character as i32;
            let cursor_loc = flow_parser::loc::Loc::cursor(Some(file_key), line, col);
            let rename_loc = flow_services_references::prepare_rename_searcher::search_rename_loc(
                &file_artifacts.0.ast,
                &cursor_loc,
            );
            let resp = LspMessage::ResponseMessage(
                id,
                LspResult::PrepareRenameResult(rename_loc.map(|loc| loc_to_lsp_range(&loc))),
            );
            let metadata = with_data(Some(serde_json::json!({"result": "SUCCESS"})), metadata);
            (lsp_prot::Response::LspFromServer(Some(resp)), metadata)
        }
    }
}

fn handle_persistent_rename(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_types::RenameParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let new_name = &params.new_name;
    let text_doc_position = lsp_types::TextDocumentPositionParams {
        text_document: params.text_document_position.text_document.clone(),
        position: params.text_document_position.position,
    };
    let (file_artifacts_opt, extra_parse_data) = get_file_artifacts(
        options,
        env,
        shared_mem.clone(),
        node_modules_containers,
        client_id,
        &text_doc_position,
        None,
    );
    match file_artifacts_opt {
        Err(reason) => {
            let metadata = with_data(
                Some(serde_json::json!({"result": "FAILURE", "error": &reason})),
                metadata,
            );
            mk_lsp_error_response(Some(id), reason, None, metadata)
        }
        Ok(None) => {
            let metadata = with_data(extra_parse_data, metadata);
            let edit = lsp_types::WorkspaceEdit {
                changes: Some(std::collections::HashMap::new()),
                ..Default::default()
            };
            let response = LspMessage::ResponseMessage(id, LspResult::RenameResult(edit));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Ok(Some((file_artifacts, file_key))) => {
            let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
            let (local_refs, extra_data) = find_local_references(
                &loc_of_aloc,
                &file_artifacts,
                flow_services_references::find_refs_types::Kind::Rename,
                &file_key,
                &text_doc_position,
            );
            let metadata = with_data(extra_data, metadata);
            match local_refs {
                Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
                Ok((_, flow_services_references::find_refs_types::FindRefsOk::NoDefinition(_))) => {
                    let metadata =
                        with_data(Some(serde_json::json!({"result": "BAD_LOC"})), metadata);
                    let edit = lsp_types::WorkspaceEdit {
                        changes: Some(std::collections::HashMap::new()),
                        ..Default::default()
                    };
                    let response = LspMessage::ResponseMessage(id, LspResult::RenameResult(edit));
                    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
                }
                Ok((
                    _,
                    flow_services_references::find_refs_types::FindRefsOk::FoundReferences(refs),
                )) => {
                    let mut ref_map = std::collections::BTreeMap::new();
                    for (ref_kind, loc) in &refs {
                        ref_map.insert(loc.clone(), *ref_kind);
                    }
                    let current_ast = &file_artifacts.0.ast;
                    let renamed_ast = flow_services_references::rename_mapper::rename(
                        true,
                        &ref_map,
                        new_name,
                        current_ast,
                    );
                    let all_diffs =
                        flow_parser_utils::flow_ast_differ::program(current_ast, &renamed_ast);
                    let opts = flow_parser_utils_output::js_layout_generator::Opts {
                        bracket_spacing: options.format.bracket_spacing,
                        single_quotes: options.format.single_quotes,
                        ..flow_parser_utils_output::js_layout_generator::default_opts()
                    };
                    let loc_patches =
                        flow_parser_utils_output::replacement_printer::mk_loc_patch_ast_differ(
                            &opts, &all_diffs,
                        );
                    let mut changes: std::collections::HashMap<
                        lsp_types::Url,
                        Vec<lsp_types::TextEdit>,
                    > = std::collections::HashMap::new();
                    for (loc, new_text) in loc_patches {
                        if let Some(source) = &loc.source {
                            if let Ok(uri) = lsp_types::Url::from_file_path(source.to_path_buf()) {
                                let edit = lsp_types::TextEdit {
                                    range: loc_to_lsp_range(&loc),
                                    new_text,
                                };
                                changes.entry(uri).or_default().push(edit);
                            }
                        }
                    }
                    let workspace_edit = lsp_types::WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    };
                    let response =
                        LspMessage::ResponseMessage(id, LspResult::RenameResult(workspace_edit));
                    let metadata =
                        with_data(Some(serde_json::json!({"result": "SUCCESS"})), metadata);
                    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
                }
            }
        }
    }
}

fn handle_persistent_coverage(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_mapper::type_coverage::Params,
    file_input: Option<&FileInput>,
    metadata: lsp_prot::Metadata,
) -> Result<(lsp_prot::Response, lsp_prot::Metadata), WorkloadCanceled> {
    let text_document = &params.text_document;
    let file_input = match file_input {
        Some(file_input) => file_input.clone(),
        None => file_input_of_text_document_identifier(client_id, text_document),
    };
    match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(reason)) => {
            Ok(mk_lsp_error_response(Some(id), reason, None, metadata))
        }
        Err(IdeFileError::Skipped(reason)) => {
            let range = lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 1,
                    character: 0,
                },
            };
            let r = LspResult::TypeCoverageResult(lsp_mapper::type_coverage::Result {
                covered_percent: 0,
                uncovered_ranges: vec![lsp_mapper::type_coverage::UncoveredRange {
                    range,
                    message: None,
                }],
                default_message: "Use @flow to get type coverage for this file".to_string(),
            });
            let response = LspMessage::ResponseMessage(id, r);
            let metadata = with_data(json_of_skipped(&reason), metadata);
            Ok((lsp_prot::Response::LspFromServer(Some(response)), metadata))
        }
        Ok((file_key, file_contents)) => {
            let force = options.all;
            let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
                .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
            let (result, extra_data) = coverage(
                options,
                env,
                type_parse_artifacts_cache.as_ref(),
                shared_mem,
                node_modules_containers,
                &file_key,
                &file_contents,
                force,
            )?;
            let metadata = with_data(extra_data, metadata);
            match result {
                Ok(all_locs) => {
                    let (covered, total) =
                        all_locs
                            .iter()
                            .fold((0i32, 0i32), |(covered, total), (_loc, cov)| {
                                let covered = match cov {
                                    flow_services_coverage::Kind::Checked => covered + 1,
                                    flow_services_coverage::Kind::Any
                                    | flow_services_coverage::Kind::Empty => covered,
                                };
                                (covered, total + 1)
                            });
                    let covered_percent = if total == 0 {
                        100
                    } else {
                        100 * covered / total
                    };
                    let uncovereds: Vec<flow_parser::loc::Loc> = all_locs
                        .iter()
                        .filter_map(|(loc, cov)| match cov {
                            flow_services_coverage::Kind::Checked => None,
                            flow_services_coverage::Kind::Any
                            | flow_services_coverage::Kind::Empty => Some(loc.clone()),
                        })
                        .collect();
                    let mut sorted = uncovereds;
                    sorted.sort();
                    let singles: Vec<flow_parser::loc::Loc> = match sorted.first() {
                        None => vec![],
                        Some(_) => {
                            let (final_candidate, mut singles) = sorted.iter().skip(1).fold(
                                (sorted[0].clone(), vec![]),
                                |(candidate, mut acc), loc| {
                                    if candidate.contains(loc) {
                                        (loc.clone(), acc)
                                    } else {
                                        acc.push(candidate);
                                        (loc.clone(), acc)
                                    }
                                },
                            );
                            singles.push(final_candidate);
                            singles
                        }
                    };
                    let uncovered_ranges: Vec<lsp_mapper::type_coverage::UncoveredRange> = singles
                        .iter()
                        .map(|loc| lsp_mapper::type_coverage::UncoveredRange {
                            range: loc_to_lsp_range(loc),
                            message: None,
                        })
                        .collect();
                    let r = LspResult::TypeCoverageResult(lsp_mapper::type_coverage::Result {
                        covered_percent,
                        uncovered_ranges,
                        default_message: "Un-type checked code. Consider adding type annotations."
                            .to_string(),
                    });
                    let response = LspMessage::ResponseMessage(id, r);
                    Ok((lsp_prot::Response::LspFromServer(Some(response)), metadata))
                }
                Err(reason) => Ok(mk_lsp_error_response(Some(id), reason, None, metadata)),
            }
        }
    }
}

fn handle_persistent_llm_context(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_mapper::llm_context::Params,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let lsp_mapper::llm_context::Params {
        edited_file_paths,
        environment_details: _,
        token_budget,
    } = params;
    let strip_root = Some(options.root.to_string_lossy().to_string());
    let strip_root_ref = strip_root.as_deref();
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    let include_imports = options.llm_context_include_imports;
    let file_sig_opts = flow_parser_utils::file_sig::FileSigOptions::default();
    let file_artifacts = edited_file_paths
        .iter()
        .filter_map(|file_uri_str| {
            let file_path = lsp_types::Url::parse(file_uri_str)
                .ok()
                .map(|uri| lsp_uri_to_flow_path(&uri))
                .unwrap_or_else(|| file_uri_str.clone());
            let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(&file_path);
            let text_document = lsp_types::TextDocumentIdentifier {
                uri: lsp_types::Url::parse(file_uri_str).ok()?,
            };
            let file_input = file_input_of_text_document_identifier(client_id, &text_document);
            let (_file_key, content) = match of_file_input(options, env, &file_input) {
                Err(_) => return None,
                Ok(v) => v,
            };
            let intermediate_result = parse_contents(options, &content, &file_key);
            let (file_artifacts_result, _did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.clone(),
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_) => None,
                Ok(file_artifacts) => Some((file_artifacts, file_key)),
            }
        })
        .collect::<Vec<_>>();
    let typed_files = file_artifacts
        .iter()
        .map(|((parse_artifacts, typecheck_artifacts), file_key)| {
            crate::llm_typed_context_provider::TypedFileInfo {
                file_key: file_key.dupe(),
                ast: parse_artifacts.ast.clone(),
                cx: &typecheck_artifacts.cx,
                typed_ast: typecheck_artifacts.typed_ast.clone(),
                shared_mem: shared_mem.as_ref(),
            }
        })
        .collect::<Vec<_>>();
    let result = crate::llm_typed_context_provider::generate_context(
        strip_root_ref,
        &typed_files,
        *token_budget,
        include_imports,
        &file_sig_opts,
    );
    let response = LspMessage::ResponseMessage(id, LspResult::LLMContextResult(result));
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_persistent_rage(
    genv: &server_env::Genv,
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
    env: &server_env::Env,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let root = genv.options.root.display().to_string();
    let items = collect_rage(&genv.options, env, genv.shared_mem.as_ref(), None)
        .into_iter()
        .map(|(title, data)| lsp_mapper::rage::RageItem {
            title: Some(format!("{}:{}", root, title)),
            data,
        })
        .collect();
    let response = LspMessage::ResponseMessage(id, LspResult::RageResult(items));
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_persistent_ping(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let start_server_status = metadata
        .start_server_status
        .as_ref()
        .map(|status| flow_server_env::server_status::string_of_status(false, true, status));
    let response = LspMessage::ResponseMessage(
        id,
        LspResult::PingResult(lsp_mapper::ping::Result {
            start_server_status,
        }),
    );
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_persistent_log_command(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let response = LspMessage::ResponseMessage(id, LspResult::ExecuteCommandResult(()));
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn send_workspace_edit(
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
    label: Option<String>,
    edit: lsp_types::WorkspaceEdit,
    on_response: Box<dyn FnOnce(lsp_types::ApplyWorkspaceEditResponse) + Send>,
    on_error: Box<dyn FnOnce((lsp_mapper::lsp_error::T, String)) + Send>,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let prefix = match &id {
        lsp_types::NumberOrString::Number(n) => n.to_string(),
        lsp_types::NumberOrString::String(s) => s.clone(),
    };
    let req_id = lsp_types::NumberOrString::String(format!("{}:applyEdit", prefix));
    let request = LspMessage::RequestMessage(
        req_id.clone(),
        LspRequest::ApplyWorkspaceEditRequest(lsp_types::ApplyWorkspaceEditParams { label, edit }),
    );
    let handler = lsp_handler::UnitLspResultHandler::ApplyWorkspaceEditHandler(on_response);
    if let Some(client) = persistent_connection::get_client(client_id) {
        persistent_connection::push_outstanding_handler(&client, req_id, (handler, on_error));
    }
    (lsp_prot::Response::LspFromServer(Some(request)), metadata)
}

fn handle_persistent_add_missing_imports_command(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    text_document: &lsp_types::TextDocumentIdentifier,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let edits = add_missing_imports(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        text_document,
    );
    match edits {
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
        Ok(ref e) if e.is_empty() => {
            let response = LspMessage::ResponseMessage(id, LspResult::ExecuteCommandResult(()));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Ok(edits) => {
            let uri = text_document.uri.clone();
            let mut changes = std::collections::HashMap::new();
            changes.insert(uri, edits);
            let edit = lsp_types::WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            };
            let id_clone = id.clone();
            let metadata_clone = metadata.clone();
            send_workspace_edit(
                client_id,
                id,
                metadata,
                Some("Add missing imports".to_string()),
                edit,
                Box::new(move |_result| {
                    let response =
                        LspMessage::ResponseMessage(id_clone, LspResult::ExecuteCommandResult(()));
                    if let Some(client) = persistent_connection::get_client(client_id) {
                        persistent_connection::send_response(
                            (
                                lsp_prot::Response::LspFromServer(Some(response)),
                                metadata_clone,
                            ),
                            &client,
                        );
                    }
                }),
                Box::new(|_| {}),
            )
        }
    }
}

fn handle_persistent_organize_imports_command(
    options: &Options,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    text_document: &lsp_types::TextDocumentIdentifier,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    match organize_imports(options, client_id, text_document) {
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
        Ok(ref e) if e.is_empty() => {
            let response = LspMessage::ResponseMessage(id, LspResult::ExecuteCommandResult(()));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Ok(edits) => {
            let uri = text_document.uri.clone();
            let mut changes = std::collections::HashMap::new();
            changes.insert(uri, edits);
            let edit = lsp_types::WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            };
            let id_clone = id.clone();
            let metadata_clone = metadata.clone();
            send_workspace_edit(
                client_id,
                id,
                metadata,
                Some("Organize imports".to_string()),
                edit,
                Box::new(move |_result| {
                    let response =
                        LspMessage::ResponseMessage(id_clone, LspResult::ExecuteCommandResult(()));
                    if let Some(client) = persistent_connection::get_client(client_id) {
                        persistent_connection::send_response(
                            (
                                lsp_prot::Response::LspFromServer(Some(response)),
                                metadata_clone,
                            ),
                            &client,
                        );
                    }
                }),
                Box::new(|_| {}),
            )
        }
    }
}

fn handle_persistent_auto_close_jsx(
    options: &Options,
    env: &server_env::Env,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: lsp_mapper::auto_close_jsx::Params,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (result, extra_data) = auto_close_jsx_handler(options, env, client_id, &params);
    let metadata = with_data(extra_data, metadata);
    match result {
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
        Ok(text_opt) => {
            let response = LspMessage::ResponseMessage(id, LspResult::AutoCloseJsxResult(text_opt));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
    }
}

fn auto_close_jsx_handler(
    options: &Options,
    env: &server_env::Env,
    client_id: lsp_prot::ClientId,
    params: &lsp_mapper::auto_close_jsx::Params,
) -> (Result<Option<String>, String>, Option<lsp_prot::Json>) {
    let text_document = &params.text_document;
    let file_input = file_input_of_text_document_identifier(client_id, text_document);
    match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(e)) => (Err(e), None),
        Err(IdeFileError::Skipped(reason)) => {
            let extra_data = json_of_skipped(&reason);
            (Ok(None), extra_data)
        }
        Ok((filename, contents)) => {
            let (parse_result, _) = parse_contents(options, &contents, &filename);
            match parse_result {
                None => (Ok(None), None),
                Some(ParseArtifacts { ast, .. }) => {
                    let target_pos = flow_parser::loc::Position {
                        line: params.position.line as i32 + 1,
                        column: params.position.character as i32,
                    };
                    let edit = auto_close_jsx(&ast, &target_pos);
                    (Ok(edit), None)
                }
            }
        }
    }
}

fn prepare_document_paste(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    params: &lsp_mapper::document_paste::PrepareParams,
) -> (
    Vec<flow_services_code_action::document_paste::ImportItem>,
    Option<lsp_prot::Json>,
) {
    let text_document = lsp_types::TextDocumentIdentifier {
        uri: params.uri.clone(),
    };
    let file_input = file_input_of_text_document_identifier(client_id, &text_document);
    let type_parse_artifacts_cache = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::type_parse_artifacts_cache(&client));
    match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(_)) => (vec![], None),
        Err(IdeFileError::Skipped(reason)) => {
            let extra_data = json_of_skipped(&reason);
            (vec![], extra_data)
        }
        Ok((file_key, contents)) => {
            let intermediate_result = parse_contents(options, &contents, &file_key);
            let (file_artifacts_result, _did_hit_cache) = type_parse_artifacts_with_cache(
                options,
                type_parse_artifacts_cache.as_ref(),
                shared_mem.clone(),
                env.master_cx.clone(),
                file_key.dupe(),
                intermediate_result,
                node_modules_containers,
            );
            match file_artifacts_result {
                Err(_parse_errors) => (
                    vec![],
                    Some(serde_json::json!({"error": "Couldn't parse file in parse_contents"})),
                ),
                Ok((ref parse_artifacts, ref typecheck_artifacts)) => {
                    let loc_of_aloc = |aloc: &flow_aloc::ALoc| shared_mem.loc_of_aloc(aloc);
                    let ranges: Vec<flow_parser::loc::Loc> = params
                        .ranges
                        .iter()
                        .map(|r| lsp_range_to_flow_loc(Some(file_key.dupe()), r))
                        .collect();
                    let import_items =
                        flow_services_code_action::document_paste::prepare_document_paste(
                            &typecheck_artifacts.cx,
                            &loc_of_aloc,
                            &parse_artifacts.ast,
                            &typecheck_artifacts.typed_ast,
                            &ranges,
                        );
                    (import_items, Some(serde_json::json!({})))
                }
            }
        }
    }
}

fn provide_document_paste(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    params: &lsp_mapper::document_paste::ProvideParams,
) -> (lsp_types::WorkspaceEdit, Option<lsp_prot::Json>) {
    let uri = &params.text_document.uri;
    let text = &params.text_document.text;
    let lsp_mapper::document_paste::DataTransfer::ImportMetadata {
        imports: lsp_imports,
    } = &params.data_transfer;
    let imports: Vec<flow_services_code_action::document_paste::ImportItem> = lsp_imports
        .iter()
        .map(|item| {
            let import_type = match &item.import_type {
                lsp_mapper::document_paste::ImportType::ImportNamedValue => {
                    flow_services_code_action::document_paste::ImportType::ImportNamedValue
                }
                lsp_mapper::document_paste::ImportType::ImportValueAsNamespace => {
                    flow_services_code_action::document_paste::ImportType::ImportValueAsNamespace
                }
                lsp_mapper::document_paste::ImportType::ImportNamedType => {
                    flow_services_code_action::document_paste::ImportType::ImportNamedType
                }
                lsp_mapper::document_paste::ImportType::ImportNamedTypeOf => {
                    flow_services_code_action::document_paste::ImportType::ImportNamedTypeOf
                }
                lsp_mapper::document_paste::ImportType::ImportTypeOfAsNamespace => {
                    flow_services_code_action::document_paste::ImportType::ImportTypeOfAsNamespace
                }
            };
            flow_services_code_action::document_paste::ImportItem {
                remote_name: item.remote_name.clone(),
                local_name: item.local_name.clone(),
                import_type,
                import_source: item.import_source.clone(),
                import_source_is_resolved: item.import_source_is_resolved,
            }
        })
        .collect();
    let file_key = flow_parser::file_key::FileKey::source_file_of_absolute(
        &lsp_document_identifier_to_flow_path(&lsp_types::TextDocumentIdentifier {
            uri: uri.clone(),
        }),
    );
    let (parse_result, _errors) = parse_contents(options, text, &file_key);
    let (edits, extra_data) = match parse_result {
        None => (
            vec![],
            Some(serde_json::json!({"error": "Failed to parse"})),
        ),
        Some(ref parse_artifacts) => {
            let file_path = file_key.to_absolute();
            let src_dir = std::path::Path::new(&file_path)
                .parent()
                .map(|p| p.to_string_lossy().to_string());
            let module_system_info = mk_module_system_info(options, shared_mem);
            let layout_options =
                flow_services_code_action::code_action_utils::layout_options(options);
            let edits = flow_services_code_action::document_paste::provide_document_paste_edits(
                &layout_options,
                &module_system_info,
                src_dir.as_deref(),
                &parse_artifacts.ast,
                &imports,
            );
            (edits, Some(serde_json::json!({})))
        }
    };
    let text_edits: Vec<lsp_types::TextEdit> = edits
        .iter()
        .map(|(loc, new_text)| lsp_types::TextEdit {
            range: loc_to_lsp_range(loc),
            new_text: new_text.clone(),
        })
        .collect();
    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), text_edits);
    let workspace_edit = lsp_types::WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    };
    (workspace_edit, extra_data)
}

fn handle_persistent_prepare_document_paste(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_mapper::document_paste::PrepareParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (imports, extra_data) = prepare_document_paste(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        params,
    );
    let metadata = with_data(extra_data, metadata);
    let imports_lsp: Vec<lsp_mapper::document_paste::ImportItem> = imports
        .into_iter()
        .map(|item| {
            let import_type = match item.import_type {
                flow_services_code_action::document_paste::ImportType::ImportNamedValue => {
                    lsp_mapper::document_paste::ImportType::ImportNamedValue
                }
                flow_services_code_action::document_paste::ImportType::ImportValueAsNamespace => {
                    lsp_mapper::document_paste::ImportType::ImportValueAsNamespace
                }
                flow_services_code_action::document_paste::ImportType::ImportNamedType => {
                    lsp_mapper::document_paste::ImportType::ImportNamedType
                }
                flow_services_code_action::document_paste::ImportType::ImportNamedTypeOf => {
                    lsp_mapper::document_paste::ImportType::ImportNamedTypeOf
                }
                flow_services_code_action::document_paste::ImportType::ImportTypeOfAsNamespace => {
                    lsp_mapper::document_paste::ImportType::ImportTypeOfAsNamespace
                }
            };
            lsp_mapper::document_paste::ImportItem {
                remote_name: item.remote_name,
                local_name: item.local_name,
                import_type,
                import_source: item.import_source,
                import_source_is_resolved: item.import_source_is_resolved,
            }
        })
        .collect();
    let data_transfer = lsp_mapper::document_paste::DataTransfer::ImportMetadata {
        imports: imports_lsp,
    };
    let response =
        LspMessage::ResponseMessage(id, LspResult::PrepareDocumentPasteResult(data_transfer));
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_persistent_provide_document_paste_edits(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    id: lsp_prot::LspId,
    params: &lsp_mapper::document_paste::ProvideParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (workspace_edit, extra_data) = provide_document_paste(options, shared_mem, params);
    let metadata = with_data(extra_data, metadata);
    let response =
        LspMessage::ResponseMessage(id, LspResult::ProvideDocumentPasteResult(workspace_edit));
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_persistent_linked_editing_range(
    options: &Options,
    env: &server_env::Env,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: lsp_types::LinkedEditingRangeParams,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let (result, extra_data) = linked_editing_range_handler(options, env, client_id, &params);
    let metadata = with_data(extra_data, metadata);
    match result {
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
        Ok(result) => {
            let response =
                LspMessage::ResponseMessage(id, LspResult::LinkedEditingRangeResult(result));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
    }
}

fn linked_editing_range_handler(
    options: &Options,
    env: &server_env::Env,
    client_id: lsp_prot::ClientId,
    params: &lsp_types::LinkedEditingRangeParams,
) -> (
    Result<Option<lsp_types::LinkedEditingRanges>, String>,
    Option<lsp_prot::Json>,
) {
    let text_document = &params.text_document_position_params.text_document;
    let file_input = file_input_of_text_document_identifier(client_id, text_document);
    match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(e)) => (Err(e), None),
        Err(IdeFileError::Skipped(reason)) => {
            let extra_data = json_of_skipped(&reason);
            (Ok(None), extra_data)
        }
        Ok((filename, contents)) => {
            let (parse_result, parse_errors) = parse_contents(options, &contents, &filename);
            let has_parse_errors = !parse_errors.is_empty();
            if has_parse_errors {
                return (Ok(None), None);
            }
            match parse_result {
                None => (Ok(None), None),
                Some(ParseArtifacts { ast, .. }) => {
                    let target_pos = flow_parser::loc::Position {
                        line: params.text_document_position_params.position.line as i32 + 1,
                        column: params.text_document_position_params.position.character as i32,
                    };
                    let target_loc = flow_parser::loc::Loc::cursor(
                        Some(filename),
                        target_pos.line,
                        target_pos.column,
                    );
                    let result = linked_editing_range(&ast, &target_loc).map(|ranges| {
                        lsp_types::LinkedEditingRanges {
                            ranges,
                            word_pattern: None,
                        }
                    });
                    (Ok(result), None)
                }
            }
        }
    }
}

fn rename_module_file_sig_options(
    options: &Options,
) -> flow_parser_utils::file_sig::FileSigOptions {
    flow_parser_utils::file_sig::FileSigOptions {
        enable_enums: options.enums,
        enable_jest_integration: options.enable_jest_integration,
        enable_relay_integration: options.enable_relay_integration,
        explicit_available_platforms: None,
        file_options: options.file_options.dupe(),
        haste_module_ref_prefix: options.haste_module_ref_prefix.dupe(),
        project_options: options.projects_options.dupe(),
        relay_integration_module_prefix: options.relay_integration_module_prefix.dupe(),
    }
}

fn rename_module_edits_for_file(
    old_haste_name: &str,
    new_haste_name: &str,
    file_sig: &flow_parser_utils::file_sig::FileSig,
) -> Vec<lsp_types::TextEdit> {
    use flow_parser_utils::file_sig::Require;

    file_sig
        .requires()
        .iter()
        .filter_map(|require| {
            let (loc, replacement) = match require {
                Require::Require { source, prefix, .. }
                    if source.name().as_str() == old_haste_name =>
                {
                    (
                        source.loc().clone(),
                        match prefix {
                            Some(prefix) => format!("{}{}", prefix, new_haste_name),
                            None => new_haste_name.to_string(),
                        },
                    )
                }
                Require::ImportDynamic { source, .. }
                | Require::Import0 { source }
                | Require::Import { source, .. }
                | Require::ExportFrom { source }
                    if source.name().as_str() == old_haste_name =>
                {
                    (source.loc().clone(), new_haste_name.to_string())
                }
                _ => return None,
            };
            Some(lsp_types::TextEdit {
                range: loc_to_lsp_range(&loc),
                new_text: serde_json::to_string(&replacement).unwrap_or_else(|_| {
                    format!(
                        "\"{}\"",
                        replacement.replace('\\', "\\\\").replace('"', "\\\"")
                    )
                }),
            })
        })
        .collect()
}

fn handle_persistent_rename_file_imports(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_mapper::rename_file_imports::Params,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let text_document_identifier = lsp_types::TextDocumentIdentifier {
        uri: params.old_uri.clone(),
    };
    let file_input = file_input_of_text_document_identifier(client_id, &text_document_identifier);
    let result = match of_file_input(options, env, &file_input) {
        Err(IdeFileError::Failed(reason)) => Err(reason),
        Err(IdeFileError::Skipped(reason)) => Err(reason),
        Ok((file_key, _content)) => {
            let old_haste_name = flow_services_module::exported_module(
                options,
                &file_key,
                &flow_services_module::PackageInfo::none(),
            );
            let new_flowpath = lsp_uri_to_flow_path(&params.new_uri);
            let new_file_key = file_key.map(|_| new_flowpath.clone());
            let new_haste_name = flow_services_module::exported_module(
                options,
                &new_file_key,
                &flow_services_module::PackageInfo::none(),
            );
            match (old_haste_name, new_haste_name) {
                (Some(old_haste_info), Some(new_haste_info)) => {
                    let mut changes = std::collections::HashMap::new();
                    let file_sig_opts = rename_module_file_sig_options(options);
                    if let Some(haste_module) = shared_mem.get_haste_module(&old_haste_info) {
                        for dependent in haste_module.get_dependents() {
                            let Some(ast) = shared_mem.get_ast(&dependent) else {
                                continue;
                            };
                            let Ok(uri) = lsp_types::Url::from_file_path(dependent.to_path_buf())
                            else {
                                continue;
                            };
                            let dependent_file_sig =
                                flow_parser_utils::file_sig::FileSig::from_program(
                                    &dependent,
                                    &ast,
                                    &file_sig_opts,
                                );
                            let edits = rename_module_edits_for_file(
                                old_haste_info.module_name().as_str(),
                                new_haste_info.module_name().as_str(),
                                &dependent_file_sig,
                            );
                            if !edits.is_empty() {
                                changes.insert(uri, edits);
                            }
                        }
                    }
                    Ok(lsp_types::WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    })
                }
                _ => Err("Error converting file names to Haste paths".to_string()),
            }
        }
    };
    match result {
        Ok(result) => {
            let response =
                LspMessage::ResponseMessage(id, LspResult::RenameFileImportsResult(result));
            (lsp_prot::Response::LspFromServer(Some(response)), metadata)
        }
        Err(reason) => mk_lsp_error_response(Some(id), reason, None, metadata),
    }
}

fn handle_persistent_malformed_command(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    mk_lsp_error_response(Some(id), "Malformed command".to_string(), None, metadata)
}

fn handle_persistent_unsupported(
    id: Option<lsp_prot::LspId>,
    unhandled: &str,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let message = format!("Unhandled method {}", unhandled);
    let response = match id {
        Some(id) => {
            let e = lsp_error::T {
                code: lsp_error::Code::MethodNotFound,
                message: message.clone(),
                data: None,
            };
            LspMessage::ResponseMessage(id, LspResult::ErrorResult(e, String::new()))
        }
        None => LspMessage::NotificationMessage(LspNotification::TelemetryNotification(
            lsp_types::LogMessageParams {
                typ: MessageType::ERROR,
                message,
            },
        )),
    };
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn handle_result_from_client(
    id: lsp_prot::LspId,
    metadata: lsp_prot::Metadata,
    result: LspResult,
    client_id: lsp_prot::ClientId,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let Some(client) = persistent_connection::get_client(client_id) else {
        return (lsp_prot::Response::LspFromServer(None), metadata);
    };
    match persistent_connection::pop_outstanding_handler(&client, &id) {
        Some((handler, handle_error)) => match (result, handler) {
            (
                LspResult::ApplyWorkspaceEditResult(result),
                lsp_handler::UnitLspResultHandler::ApplyWorkspaceEditHandler(handle),
            ) => {
                handle(result);
            }
            (LspResult::ErrorResult(e, msg), _) => {
                handle_error((e, msg));
            }
            _ => {}
        },
        None => {}
    }
    (lsp_prot::Response::LspFromServer(None), metadata)
}

fn live_diagnostics_of_uri(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    uri: &str,
    metadata: lsp_prot::Metadata,
) -> (
    Result<lsp_prot::LiveErrorsResponse, (lsp_prot::LiveErrorsFailure, FileInput)>,
    lsp_prot::Metadata,
) {
    let live_errors_uri = match lsp_types::Url::parse(uri) {
        Ok(u) => u,
        Err(_) => lsp_types::Url::from_file_path(uri)
            .unwrap_or_else(|_| lsp_types::Url::parse(&format!("file://{}", uri)).unwrap()),
    };
    let file_path = lsp_uri_to_flow_path(&live_errors_uri);
    let file_input = persistent_connection::get_client(client_id)
        .map(|client| persistent_connection::get_file(&client, &file_path))
        .unwrap_or_else(|| FileInput::FileName(file_path.clone()));
    match &file_input {
        FileInput::FileName(_) => (
            Err((
                lsp_prot::LiveErrorsFailure {
                    live_errors_failure_kind: lsp_prot::ErrorResponseKind::ErroredErrorResponse,
                    live_errors_failure_reason: format!(
                        "Cannot get live errors for {}: File not open",
                        file_path
                    ),
                    live_errors_failure_uri: live_errors_uri,
                },
                file_input,
            )),
            metadata,
        ),
        FileInput::FileContent(_, content) => {
            let (
                live_errors,
                live_warnings,
                switch_to_match_eligible_locations,
                refined_locations,
                metadata,
            ) = {
                let file_key = file_key_of_file_input(options, env, &file_input);
                match check_that_we_care_about_this_file(options, env, &file_key, content) {
                    Ok(()) => {
                        let all_unordered_libs: std::collections::BTreeSet<String> = env
                            .all_unordered_libs
                            .iter()
                            .map(|s| s.to_string())
                            .collect();
                        let file_key = flow_common::files::filename_from_string(
                            &options.file_options,
                            true,
                            &all_unordered_libs,
                            &file_path,
                        );
                        let type_parse_artifacts_cache =
                            persistent_connection::get_client(client_id).map(|client| {
                                persistent_connection::type_parse_artifacts_cache(&client)
                            });
                        let intermediate_result = parse_contents(options, content, &file_key);
                        let (result, did_hit_cache) = type_parse_artifacts_with_cache(
                            options,
                            type_parse_artifacts_cache.as_ref(),
                            shared_mem.clone(),
                            env.master_cx.clone(),
                            file_key.dupe(),
                            intermediate_result,
                            node_modules_containers,
                        );
                        let (live_errors, live_warnings) =
                            printable_errors_of_file_artifacts_result(
                                options,
                                env,
                                &shared_mem,
                                &file_key,
                                result.as_ref(),
                            );
                        let switch_to_match_eligible_locations: Vec<flow_parser::loc::Loc> =
                            match &result {
                                Ok((_pa, ta)) if semantic_decorations(client_id) => ta
                                    .cx
                                    .switch_to_match_eligible_locations()
                                    .iter()
                                    .map(|aloc| shared_mem.loc_of_aloc(aloc))
                                    .collect(),
                                _ => vec![],
                            };
                        let refined_locations: Vec<flow_parser::loc::Loc> = match &result {
                            Ok((_pa, ta)) if semantic_decorations(client_id) => ta
                                .cx
                                .refined_locations()
                                .iter()
                                .map(|(aloc, _)| shared_mem.loc_of_aloc(aloc))
                                .collect(),
                            _ => vec![],
                        };
                        let json_props = add_cache_hit_data_to_json(vec![], did_hit_cache);
                        let json = serde_json::Value::Object(json_props.into_iter().collect());
                        let metadata = with_data(Some(json), metadata);
                        (
                            live_errors,
                            live_warnings,
                            switch_to_match_eligible_locations,
                            refined_locations,
                            metadata,
                        )
                    }
                    Err(reason) => {
                        eprintln!(
                            "Not reporting live errors for file {:?}: {}",
                            file_path, reason
                        );
                        let extra_data = json_of_skipped(reason);
                        let metadata = with_data(extra_data, metadata);
                        (
                            ConcreteLocPrintableErrorSet::new(),
                            ConcreteLocPrintableErrorSet::new(),
                            vec![],
                            vec![],
                            metadata,
                        )
                    }
                }
            };
            let vscode_dd = vscode_detailed_diagnostics(client_id);
            let should_include = |_error: &flow_common_errors::error_utils::PrintableError<
                flow_parser::loc::Loc,
            >| { vscode_dd };
            let unsaved_content: flow_common_errors::error_utils::StdinFile =
                Some((std::path::PathBuf::from(&file_path), content.clone()));
            let diagnostics_map = flow_lsp_conversions::diagnostics_of_flow_errors(
                &unsaved_content,
                &should_include,
                &live_errors,
                &live_warnings,
            );
            let mut live_diagnostics = diagnostics_map
                .get(&live_errors_uri)
                .cloned()
                .unwrap_or_default();
            for loc in &switch_to_match_eligible_locations {
                if loc.source.is_some() {
                    live_diagnostics.push(lsp_types::Diagnostic {
                        range: loc_to_lsp_range(loc),
                        severity: Some(lsp_types::DiagnosticSeverity::HINT),
                        code: Some(lsp_types::NumberOrString::String(
                            "switch-to-match".to_string(),
                        )),
                        source: Some("Flow".to_string()),
                        message: "This switch statement can be converted to use match syntax"
                            .to_string(),
                        tags: None,
                        related_information: None,
                        data: None,
                        code_description: None,
                    });
                }
            }
            for loc in &refined_locations {
                if loc.source.is_some() {
                    live_diagnostics.push(lsp_types::Diagnostic {
                        range: loc_to_lsp_range(loc),
                        severity: Some(lsp_types::DiagnosticSeverity::HINT),
                        code: None,
                        source: Some("Flow".to_string()),
                        message: "refined-value".to_string(),
                        tags: None,
                        related_information: None,
                        data: Some(serde_json::json!("RefinementInformation")),
                        code_description: None,
                    });
                }
            }
            (
                Ok(lsp_prot::LiveErrorsResponse {
                    live_diagnostics,
                    live_errors_uri,
                }),
                metadata,
            )
        }
    }
}

fn handle_live_errors_request(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    uri: &str,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    if !is_latest_live_errors_metadata(uri, &metadata) {
        return (
            lsp_prot::Response::LiveErrorsResponse(Err(lsp_prot::LiveErrorsFailure {
                live_errors_failure_kind: lsp_prot::ErrorResponseKind::CanceledErrorResponse,
                live_errors_failure_reason: "Subsumed by a later request".to_string(),
                live_errors_failure_uri: lsp_types::Url::parse(uri)
                    .or_else(|_| lsp_types::Url::from_file_path(uri).map_err(|_| ()))
                    .unwrap_or_else(|_| lsp_types::Url::parse(&format!("file://{}", uri)).unwrap()),
            })),
            metadata,
        );
    }

    let ret = match live_diagnostics_of_uri(
        options,
        env,
        shared_mem,
        node_modules_containers,
        client_id,
        uri,
        metadata,
    ) {
        (Err((e, _file_input)), metadata) => {
            (lsp_prot::Response::LiveErrorsResponse(Err(e)), metadata)
        }
        (Ok(diagnostics), metadata) => (
            lsp_prot::Response::LiveErrorsResponse(Ok(diagnostics)),
            metadata,
        ),
    };

    if is_latest_live_errors_metadata(uri, &ret.1) {
        let mut map = URI_TO_LATEST_METADATA_MAP.lock().unwrap();
        map.remove(uri);
    }

    ret
}

fn handle_persistent_text_document_diagnostics_lsp(
    options: &Options,
    env: &server_env::Env,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
    node_modules_containers: &std::collections::BTreeMap<
        flow_data_structure_wrapper::smol_str::FlowSmolStr,
        std::collections::BTreeSet<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
    >,
    client_id: lsp_prot::ClientId,
    id: lsp_prot::LspId,
    params: &lsp_mapper::text_document_diagnostics::Params,
    metadata: lsp_prot::Metadata,
) -> (lsp_prot::Response, lsp_prot::Metadata) {
    let uri = &params.text_document.uri;
    let (result, metadata) = live_diagnostics_of_uri(
        options,
        env,
        shared_mem.clone(),
        node_modules_containers,
        client_id,
        uri.as_str(),
        metadata,
    );
    let response = match result {
        Ok(lsp_prot::LiveErrorsResponse {
            live_diagnostics,
            live_errors_uri: _,
        }) => LspMessage::ResponseMessage(
            id,
            LspResult::TextDocumentDiagnosticsResult(live_diagnostics),
        ),
        Err((_failure, file_input)) => {
            match errors_of_file(
                options,
                env,
                shared_mem,
                false,
                &file_input,
                node_modules_containers,
            ) {
                Err(_) => LspMessage::ResponseMessage(
                    id,
                    LspResult::TextDocumentDiagnosticsResult(vec![]),
                ),
                Ok((errors, warnings)) => {
                    let no_detailed = |_: &flow_common_errors::error_utils::PrintableError<
                        flow_parser::loc::Loc,
                    >| { false };
                    let unsaved_content: flow_common_errors::error_utils::StdinFile = None;
                    let diagnostics_map = flow_lsp_conversions::diagnostics_of_flow_errors(
                        &unsaved_content,
                        &no_detailed,
                        &errors,
                        &warnings,
                    );
                    let live_diagnostics = diagnostics_map.get(uri).cloned().unwrap_or_default();
                    LspMessage::ResponseMessage(
                        id,
                        LspResult::TextDocumentDiagnosticsResult(live_diagnostics),
                    )
                }
            }
        }
    };
    (lsp_prot::Response::LspFromServer(Some(response)), metadata)
}

fn mk_parallelizable_persistent(
    options: &Options,
    f: PersistentParallelizableWorkload,
) -> PersistentCommandHandler {
    let wait_for_recheck = options.wait_for_recheck;
    if wait_for_recheck {
        PersistentCommandHandler::HandleNonparallelizablePersistent(f)
    } else {
        PersistentCommandHandler::HandleParallelizablePersistent(f)
    }
}

fn get_persistent_handler(
    genv: &server_env::Genv,
    client_id: lsp_prot::ClientId,
    request: &lsp_prot::RequestWithMetadata,
) -> PersistentCommandHandler {
    let options = &*genv.options;
    let (ref request_inner, ref metadata) = *request;
    if let lsp_prot::Request::LspToServer(LspMessage::RequestMessage(id, _)) = request_inner {
        let id_str = match id {
            lsp_types::NumberOrString::Number(n) => n.to_string(),
            lsp_types::NumberOrString::String(s) => s.clone(),
        };
        let is_cancelled = server_monitor_listener_state::cancellation_requests()
            .lock()
            .unwrap()
            .contains(&id_str);
        if is_cancelled {
            let id = id.clone();
            let metadata = metadata.clone();
            return PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_canceled(id, metadata)
            }));
        }
    }
    match request_inner {
        lsp_prot::Request::Subscribe => {
            let metadata = metadata.clone();
            PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |env| {
                Ok(handle_persistent_subscribe(client_id, metadata, env))
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::DidOpenNotification(params),
        )) => {
            let text_document = &params.text_document;
            let text = text_document.text.clone();
            let fn_ = lsp_uri_to_flow_path(&text_document.uri);
            let files = vec![(fn_, text)];
            let did_anything_change = persistent_connection::get_client(client_id)
                .map(|client| persistent_connection::client_did_open(&client, &files))
                .unwrap_or(false);
            let metadata = metadata.clone();
            if did_anything_change {
                enqueue_did_open_files(&files);
                // This mutates env, so it can't run in parallel
                PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |env| {
                    Ok(handle_persistent_did_open_notification(
                        client_id, metadata, env,
                    ))
                }))
            } else {
                PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                    handle_persistent_did_open_notification_no_op(metadata)
                }))
            }
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::DidChangeNotification(params),
        )) => {
            let params = params.clone();
            let metadata = metadata.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_did_change_notification(client_id, params, metadata)
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::DidSaveNotification { .. },
        )) => {
            let metadata = metadata.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_did_save_notification(metadata)
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::DidCloseNotification(params),
        )) => {
            let text_document = &params.text_document;
            let fn_ = lsp_uri_to_flow_path(&text_document.uri);
            let filenames = vec![fn_];
            // Close this file immediately in case another didOpen comes soon
            let did_anything_change = persistent_connection::get_client(client_id)
                .map(|client| persistent_connection::client_did_close(&client, &filenames))
                .unwrap_or(false);
            let metadata = metadata.clone();
            // This mutates env, so it can't run in parallel
            if did_anything_change {
                PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |env| {
                    Ok(handle_persistent_did_close_notification(
                        client_id, metadata, env,
                    ))
                }))
            } else {
                PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                    handle_persistent_did_close_notification_no_op(metadata)
                }))
            }
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::CancelRequestNotification(params),
        )) => {
            let id = params.id.clone();
            {
                let id_str = match &id {
                    lsp_types::NumberOrString::Number(n) => n.to_string(),
                    lsp_types::NumberOrString::String(s) => s.clone(),
                };
                server_monitor_listener_state::cancellation_requests()
                    .lock()
                    .unwrap()
                    .insert(id_str);
            }
            let metadata = metadata.clone();
            PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |_env| {
                Ok(handle_persistent_cancel_notification(id, metadata))
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::NotificationMessage(
            LspNotification::DidChangeConfigurationNotification(params),
        )) => {
            let params = params.clone();
            let metadata = metadata.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_did_change_configuration_notification(client_id, params, metadata)
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::DefinitionRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input = file_input_of_text_document_position_opt(
                client_id,
                &params.text_document_position_params,
            );
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    handle_persistent_get_def(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    )
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::HoverRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input = file_input_of_text_document_position_opt(
                client_id,
                &params.text_document_position_params,
            );
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    handle_persistent_infer_type(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    )
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::CodeActionRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_code_action_request(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::CompletionRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input =
                file_input_of_text_document_position_opt(client_id, &params.text_document_position);
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    handle_persistent_autocomplete_lsp(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    )
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::SignatureHelpRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input = file_input_of_text_document_position_opt(
                client_id,
                &params.text_document_position_params,
            );
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_signaturehelp_lsp(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::TextDocumentDiagnosticsRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = Arc::new(options.clone());
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_text_document_diagnostics_lsp(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::DocumentHighlightRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input = file_input_of_text_document_position_opt(
                client_id,
                &params.text_document_position_params,
            );
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_document_highlight(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::FindReferencesRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |env| {
                Ok(handle_persistent_find_references(
                    &options_arc,
                    env,
                    shared_mem,
                    &node_modules_containers,
                    client_id,
                    id,
                    &params,
                    metadata,
                ))
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::PrepareRenameRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input = file_input_of_text_document_position_opt(client_id, &params);
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_prepare_rename(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::RenameRequest(params),
        )) => {
            let id = id.clone();
            let params = params.clone();
            let metadata = metadata.clone();
            let options_arc = Arc::new(genv.options.clone());
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            PersistentCommandHandler::HandleNonparallelizablePersistent(Box::new(move |env| {
                Ok(handle_persistent_rename(
                    &options_arc,
                    env,
                    shared_mem,
                    &node_modules_containers,
                    client_id,
                    id,
                    &params,
                    metadata,
                ))
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::WorkspaceSymbolRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_workspace_symbol(
                        &options_arc,
                        env,
                        id,
                        params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::TypeCoverageRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let file_input =
                file_input_of_text_document_identifier_opt(client_id, &params.text_document);
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    handle_persistent_coverage(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        file_input.as_ref(),
                        metadata,
                    )
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(id, LspRequest::RageRequest)) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    let root = options_arc.root.display().to_string();
                    let items = collect_rage(&options_arc, env, shared_mem.as_ref(), None)
                        .into_iter()
                        .map(|(title, data)| lsp_mapper::rage::RageItem {
                            title: Some(format!("{}:{}", root, title)),
                            data,
                        })
                        .collect();
                    let response = LspMessage::ResponseMessage(id, LspResult::RageResult(items));
                    Ok((lsp_prot::Response::LspFromServer(Some(response)), metadata))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(id, LspRequest::PingRequest)) => {
            let id = id.clone();
            let metadata = metadata.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |_env| Ok(handle_persistent_ping(id, metadata))),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::ExecuteCommandRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            match params.command.as_str() {
                "log" => {
                    PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                        handle_persistent_log_command(id, metadata)
                    }))
                }
                "source.addMissingImports" => {
                    let text_document: Option<lsp_types::TextDocumentIdentifier> = params
                        .arguments
                        .first()
                        .and_then(|json: &serde_json::Value| {
                            serde_json::from_value::<lsp_types::TextDocumentIdentifier>(
                                json.clone(),
                            )
                            .ok()
                        });
                    match text_document {
                        Some(text_document) => {
                            let options_arc = genv.options.clone();
                            let shared_mem = genv.shared_mem.clone();
                            let node_modules_containers = genv.node_modules_containers.clone();
                            mk_parallelizable_persistent(
                                options,
                                Box::new(move |env| {
                                    Ok(handle_persistent_add_missing_imports_command(
                                        &options_arc,
                                        env,
                                        shared_mem,
                                        &node_modules_containers,
                                        client_id,
                                        id,
                                        &text_document,
                                        metadata,
                                    ))
                                }),
                            )
                        }
                        None => PersistentCommandHandler::HandlePersistentImmediately(Box::new(
                            move || handle_persistent_malformed_command(id, metadata),
                        )),
                    }
                }
                "source.organizeImports" => {
                    let text_document: Option<lsp_types::TextDocumentIdentifier> = params
                        .arguments
                        .first()
                        .and_then(|json: &serde_json::Value| {
                            serde_json::from_value::<lsp_types::TextDocumentIdentifier>(
                                json.clone(),
                            )
                            .ok()
                        });
                    match text_document {
                        Some(text_document) => {
                            let options_arc = genv.options.clone();
                            PersistentCommandHandler::HandlePersistentImmediately(Box::new(
                                move || {
                                    handle_persistent_organize_imports_command(
                                        &options_arc,
                                        client_id,
                                        id,
                                        &text_document,
                                        metadata,
                                    )
                                },
                            ))
                        }
                        None => PersistentCommandHandler::HandlePersistentImmediately(Box::new(
                            move || handle_persistent_malformed_command(id, metadata),
                        )),
                    }
                }
                _ => PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                    handle_persistent_malformed_command(id, metadata)
                })),
            }
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::AutoCloseJsxRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_auto_close_jsx(
                        &options_arc,
                        env,
                        client_id,
                        id,
                        params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::PrepareDocumentPasteRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_prepare_document_paste(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::ProvideDocumentPasteRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            let shared_mem = genv.shared_mem.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_provide_document_paste_edits(
                    &options_arc,
                    shared_mem,
                    id,
                    &params,
                    metadata,
                )
            }))
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::LinkedEditingRangeRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let options_arc = genv.options.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_linked_editing_range(
                        &options_arc,
                        env,
                        client_id,
                        id,
                        params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::RenameFileImportsRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let shared_mem = genv.shared_mem.clone();
            let options_for_closure = genv.options.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_rename_file_imports(
                        &options_for_closure,
                        env,
                        shared_mem.clone(),
                        client_id,
                        id,
                        &params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::RequestMessage(
            id,
            LspRequest::LLMContextRequest(params),
        )) => {
            let id = id.clone();
            let metadata = metadata.clone();
            let params = params.clone();
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            let options_for_closure = genv.options.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_persistent_llm_context(
                        &options_for_closure,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        id,
                        &params,
                        metadata,
                    ))
                }),
            )
        }

        lsp_prot::Request::LspToServer(LspMessage::ResponseMessage(id, result)) => {
            let id = id.clone();
            let result = result.clone();
            let metadata = metadata.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_result_from_client(id, metadata, result, client_id)
            }))
        }

        lsp_prot::Request::LspToServer(unhandled) => {
            let id = match &unhandled {
                lsp_prot::LspMessage::RequestMessage(id, _) => Some(id.clone()),
                _ => None,
            };
            let unhandled_str = format!("{:?}", unhandled);
            let metadata = metadata.clone();
            PersistentCommandHandler::HandlePersistentImmediately(Box::new(move || {
                handle_persistent_unsupported(id.clone(), &unhandled_str, metadata)
            }))
        }

        lsp_prot::Request::LiveErrorsRequest(uri) => {
            let uri = uri.to_string();
            let metadata = metadata.clone();
            live_errors_mark_latest_metadata(&uri, &metadata);
            let options_arc = Arc::new(options.clone());
            let shared_mem = genv.shared_mem.clone();
            let node_modules_containers = genv.node_modules_containers.clone();
            mk_parallelizable_persistent(
                options,
                Box::new(move |env| {
                    Ok(handle_live_errors_request(
                        &options_arc,
                        env,
                        shared_mem,
                        &node_modules_containers,
                        client_id,
                        &uri,
                        metadata,
                    ))
                }),
            )
        }
    }
}

fn wrap_immediate_persistent_handler<T: Default>(
    options: &Options,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    default_ret: T,
    handler: impl FnOnce() -> (T, lsp_prot::Response, lsp_prot::Metadata),
) -> T {
    let (request, metadata) = request;
    if persistent_connection::get_client(client_id).is_none() {
        eprintln!(
            "Unknown persistent client {}. Maybe connection went away?",
            client_id
        );
        return default_ret;
    }
    eprintln!(
        "Persistent request: {}",
        lsp_prot::string_of_request(&request)
    );
    let start = std::time::Instant::now();
    let _should_print_summary = options.profile;
    let result = match check_if_cancelled(&request, metadata.clone()) {
        Some((response, json_data)) => (default_ret, response, json_data),
        None => {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(handler)).unwrap_or_else(|e| {
                let exn_str = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else {
                    "unknown panic".to_string()
                };
                let response =
                    handle_persistent_uncaught_exception(request.clone(), exn_str.clone(), exn_str);
                (T::default(), response, metadata)
            })
        }
    };
    let profiling = persistent_profiling_json(start.elapsed().as_secs_f64());
    send_persistent_response(client_id, profiling, result)
}

fn handle_persistent_immediately_unsafe(
    workload: impl FnOnce() -> (lsp_prot::Response, lsp_prot::Metadata),
) -> ((), lsp_prot::Response, lsp_prot::Metadata) {
    let (response, json_data) = workload();
    ((), response, json_data)
}

fn handle_persistent_immediately(
    genv: &server_env::Genv,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    workload: impl FnOnce() -> (lsp_prot::Response, lsp_prot::Metadata),
) {
    wrap_immediate_persistent_handler(&genv.options, client_id, request, (), || {
        handle_persistent_immediately_unsafe(workload)
    });
}

pub fn enqueue_persistent(
    genv: &Arc<server_env::Genv>,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
) {
    let name = lsp_prot::string_of_request(&request.0);
    match get_persistent_handler(genv, client_id, &request) {
        PersistentCommandHandler::HandlePersistentImmediately(workload) => {
            handle_persistent_immediately(genv, client_id, request, workload);
        }
        PersistentCommandHandler::HandleParallelizablePersistent(workload) => {
            let pw = mk_parallelizable_persistent_workload(
                genv.clone(),
                client_id,
                request,
                name.clone(),
                workload,
            );
            server_monitor_listener_state::push_new_parallelizable_workload(&name, pw);
        }
        PersistentCommandHandler::HandleNonparallelizablePersistent(workload) => {
            let w = mk_nonparallelizable_persistent_workload(
                genv.clone(),
                client_id,
                request,
                workload,
            );
            server_monitor_listener_state::push_new_workload(&name, w);
        }
    }
}

fn mk_parallelizable_persistent_workload(
    genv: Arc<server_env::Genv>,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    name: String,
    workload: PersistentParallelizableWorkload,
) -> flow_server_env::workload_stream::ParallelizableWorkload {
    let request_for_cancel = request.0.clone();
    let options = genv.options.clone();
    let request_for_handler = request.clone();
    let genv_for_handler = genv.clone();
    let name_for_handler = name.clone();
    flow_server_env::workload_stream::ParallelizableWorkload {
        parallelizable_workload_should_be_cancelled: Box::new(move || {
            cancelled_request_id_opt(&request_for_cancel).is_some()
        }),
        parallelizable_workload_handler: Box::new(move |env: &server_env::Env| {
            let result: Result<(), WorkloadCanceled> = wrap_persistent_handler(
                &options,
                client_id,
                request_for_handler.clone(),
                (),
                || {
                    let (response, metadata) = workload(env)?;
                    Ok(((), response, metadata))
                },
            );
            if let Err(WorkloadCanceled) = result {
                eprintln!(
                    "Command successfully canceled. Requeuing the command for after the next recheck."
                );
                if let PersistentCommandHandler::HandleParallelizablePersistent(new_workload) =
                    get_persistent_handler(&genv_for_handler, client_id, &request_for_handler)
                {
                    let recreated = mk_parallelizable_persistent_workload(
                        genv_for_handler.clone(),
                        client_id,
                        request_for_handler.clone(),
                        name_for_handler.clone(),
                        new_workload,
                    );
                    server_monitor_listener_state::defer_parallelizable_workload(
                        &name_for_handler,
                        recreated,
                    );
                }
            }
        }),
    }
}

fn mk_nonparallelizable_persistent_workload(
    genv: Arc<server_env::Genv>,
    client_id: lsp_prot::ClientId,
    request: lsp_prot::RequestWithMetadata,
    workload: PersistentNonparallelizableWorkload,
) -> flow_server_env::workload_stream::Workload {
    let request_for_cancel = request.0.clone();
    let options = genv.options.clone();
    let request_for_handler = request.clone();
    let genv_for_handler = genv.clone();
    flow_server_env::workload_stream::Workload {
        workload_should_be_cancelled: Box::new(move || {
            cancelled_request_id_opt(&request_for_cancel).is_some()
        }),
        workload_handler: Box::new(move |env: server_env::Env| {
            let mut env = env;
            let mut current_workload = Some(workload);
            loop {
                let workload = current_workload.take().expect("loop invariant");
                let result: Result<(), WorkloadCanceled> = wrap_persistent_handler(
                    &options,
                    client_id,
                    request_for_handler.clone(),
                    (),
                    || {
                        let (response, metadata) = workload(&env)?;
                        Ok(((), response, metadata))
                    },
                );
                match result {
                    Ok(()) => return env,
                    Err(WorkloadCanceled) => {
                        eprintln!(
                            "Command successfully canceled. Running a recheck before restarting the command"
                        );
                        let node_modules_containers_arc =
                            std::sync::Arc::new(std::sync::RwLock::new(
                                (*genv_for_handler.node_modules_containers).clone(),
                            ));
                        let (_recheck_profiling, new_env) =
                            flow_server_rechecker::rechecker::recheck_loop(
                                &genv_for_handler,
                                env,
                                &genv_for_handler.shared_mem,
                                &node_modules_containers_arc,
                            );
                        env = new_env;
                        eprintln!("Now restarting the command");
                        match get_persistent_handler(
                            &genv_for_handler,
                            client_id,
                            &request_for_handler,
                        ) {
                            PersistentCommandHandler::HandleNonparallelizablePersistent(w) => {
                                current_workload = Some(w);
                            }
                            _ => return env,
                        }
                    }
                }
            }
        }),
    }
}
