/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_check_cache::CheckContentsCache;
use flow_common::files;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::Transaction;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parsing::docblock_parser;
use flow_parsing::parsing_service;
use flow_parsing::parsing_service::ParseResult;
use flow_parsing::parsing_service::ParseSkipReason;
use flow_server_env::server_env::Env;
use flow_server_env::server_orchestrator::ServerOrchestratorHandle;
use flow_services_inference_types::CheckedDependenciesCanceled;
use flow_services_inference_types::FileArtifacts;
use flow_services_inference_types::ParseArtifacts;
use flow_services_inference_types::TypeContentsError;
use flow_services_inference_types::TypecheckArtifacts;
use flow_typing_context::Context;
use flow_typing_context::MasterContext;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::intermediate_error;
use flow_typing_flow_js::type_inference_hooks_js;
use flow_typing_type::type_::Type;

use crate::inference_utils;
use crate::merge_service;
use crate::obj_to_obj_hook;

// Note that there may be parse errors
pub enum ParseContentsReturn {
    Parsed(ParseArtifacts),
    Skipped,
}

// This puts a nicer interface for do_parse. At some point, `do_parse` itself should be
// rethought, at which point `parse_contents` could call it directly without confusion. This would
// also benefit the other callers of `do_parse`. In the meantime, this function provides the
// interface we would like here.
fn do_parse_wrapper(
    options: &Options,
    all_unordered_libs: Arc<BTreeSet<FlowSmolStr>>,
    filename: &FileKey,
    contents: &str,
) -> ParseContentsReturn {
    let max_tokens = options.max_header_tokens;
    let (docblock_errors, docblock) = docblock_parser::parse_docblock(
        max_tokens as usize,
        &options.file_options,
        filename,
        contents,
    );
    let parse_result = parsing_service::do_parse(
        options,
        &docblock,
        &[],
        Ok(contents),
        filename,
        files::is_lib_file(&all_unordered_libs, filename),
    );
    match parse_result {
        ParseResult::ParseOk {
            ast,
            requires,
            file_sig,
            tolerable_errors,
            ..
        } => ParseContentsReturn::Parsed(ParseArtifacts {
            docblock: Arc::new(docblock),
            docblock_errors,
            ast: Arc::new(ast),
            requires,
            file_sig,
            tolerable_errors,
            parse_errors: vec![],
        }),
        ParseResult::ParseRecovered {
            ast,
            requires,
            file_sig,
            tolerable_errors,
            parse_errors,
        } => ParseContentsReturn::Parsed(ParseArtifacts {
            docblock: Arc::new(docblock),
            docblock_errors,
            ast: Arc::new(ast),
            requires,
            file_sig,
            tolerable_errors,
            parse_errors: parse_errors.into_vec(),
        }),
        ParseResult::ParseExn(exn) => {
            panic!("{}", exn)
        }
        ParseResult::ParseSkip(
            ParseSkipReason::SkipNonFlowFile
            | ParseSkipReason::SkipResourceFile
            | ParseSkipReason::SkipPackageJson(_),
        ) => ParseContentsReturn::Skipped,
    }
}

#[allow(dead_code)]
fn with_timer<T>(options: &Options, timer: &str, f: impl FnOnce() -> T) -> T {
    let should_print = options.profile;
    let start = Instant::now();
    let result = f();
    if should_print {
        eprintln!("[{}] {:.3}s", timer, start.elapsed().as_secs_f64());
    }
    result
}

#[allow(dead_code)]
pub fn parse_contents(
    options: &Options,
    all_unordered_libs: Arc<BTreeSet<FlowSmolStr>>,
    contents: &str,
    filename: &FileKey,
) -> (Option<ParseArtifacts>, ErrorSet) {
    with_timer(options, "Parsing", || {
        match do_parse_wrapper(options, all_unordered_libs, filename, contents) {
            ParseContentsReturn::Parsed(parse_artifacts) => {
                let errors = match parse_artifacts.parse_errors.as_slice() {
                    [first_parse_error, ..] => {
                        let mut errors = inference_utils::set_of_docblock_errors(
                            filename.dupe(),
                            &parse_artifacts.docblock_errors,
                        );
                        let err = inference_utils::error_of_parse_error(
                            filename.dupe(),
                            first_parse_error.clone(),
                        );
                        errors.add(err);
                        errors
                    }
                    _ => ErrorSet::empty(),
                };
                (Some(parse_artifacts), errors)
            }
            ParseContentsReturn::Skipped => (None, ErrorSet::empty()),
        }
    })
}

// Callers have already had a chance to inspect parse errors, so they are not included here.
// Typically, type errors in the face of parse errors are meaningless, so callers should probably
// not call this function if parse errors have been found.
#[allow(dead_code)]
fn errors_of_file_artifacts(
    options: &Options,
    env: &Env,
    loc_of_aloc: impl Fn(&ALoc) -> Loc,
    get_ast: impl Fn(&FileKey) -> Option<Arc<ast::Program<Loc, Loc>>>,
    filename: &FileKey,
    file_artifacts: &FileArtifacts,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let (parse_artifacts, typecheck_artifacts) = file_artifacts;
    let docblock_errors = &parse_artifacts.docblock_errors;
    let tolerable_errors = &parse_artifacts.tolerable_errors;
    let cx = &typecheck_artifacts.cx;
    let mut errors = cx.errors();
    let tolerable_error_set =
        inference_utils::set_of_file_sig_tolerable_errors(filename.dupe(), tolerable_errors);
    errors = errors.union(&tolerable_error_set);
    let docblock_error_set =
        inference_utils::set_of_docblock_errors(filename.dupe(), docblock_errors);
    errors = errors.union(&docblock_error_set);
    // Suppressions for errors in this file can come from dependencies
    let mut suppressions = env.errors().suppressions.clone();
    let new_suppressions = cx.error_suppressions().clone();
    suppressions.update_suppressions(new_suppressions);
    let severity_cover = cx.severity_cover();
    let include_suppressions = cx.include_suppressions();
    let aloc_tables = cx.aloc_tables();
    let (errors, warnings) =
        suppressions.filter_lints(errors, &aloc_tables, include_suppressions, &severity_cover);
    let root = &*options.root;
    let file_options = Some(&*options.file_options);
    let node_modules_errors = options.node_modules_errors;
    let unsuppressable_error_codes: BTreeSet<FlowSmolStr> =
        options.unsuppressable_error_codes.iter().cloned().collect();
    // Filter out suppressed errors
    let mut unused = ErrorSuppressions::empty();
    let (errors, _) = suppressions.filter_suppressed_errors(
        root,
        file_options,
        node_modules_errors,
        &unsuppressable_error_codes,
        &loc_of_aloc,
        &get_ast,
        |file| cx.is_global_libdef(file),
        &errors,
        &mut unused,
    );
    // Filter out suppressed warnings
    let mut unused2 = ErrorSuppressions::empty();
    let (warnings, _) = suppressions.filter_suppressed_errors(
        root,
        file_options,
        node_modules_errors,
        &unsuppressable_error_codes,
        &loc_of_aloc,
        &get_ast,
        |file| cx.is_global_libdef(file),
        &warnings,
        &mut unused2,
    );
    let warnings = if options.include_warnings {
        warnings
    } else {
        ConcreteLocPrintableErrorSet::empty()
    };
    (errors, warnings)
}

#[allow(dead_code)]
pub fn printable_errors_of_file_artifacts_result(
    options: &Options,
    env: &Env,
    transaction: &Transaction,
    filename: &FileKey,
    result: Result<&FileArtifacts, &TypeContentsError>,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let root = &*options.root;
    let loc_of_aloc = |aloc: &ALoc| -> Loc { transaction.loc_of_aloc(aloc) };
    let get_ast =
        |file: &FileKey| -> Option<Arc<ast::Program<Loc, Loc>>> { transaction.get_ast(file) };
    match result {
        Ok(file_artifacts) => {
            let (errors, warnings) = errors_of_file_artifacts(
                options,
                env,
                loc_of_aloc,
                get_ast,
                filename,
                file_artifacts,
            );
            (errors, warnings)
        }
        Err(TypeContentsError::Errors(errors)) => {
            let errors = intermediate_error::make_errors_printable(
                loc_of_aloc,
                get_ast,
                Some(root.as_path()),
                errors.clone(),
                FileKey::is_lib_file,
            );
            (errors, ConcreteLocPrintableErrorSet::empty())
        }
        Err(TypeContentsError::CheckedDependenciesCanceled) => (
            ConcreteLocPrintableErrorSet::empty(),
            ConcreteLocPrintableErrorSet::empty(),
        ),
    }
}

// Resolves dependencies specifically for checking contents, rather than for
// persisting in the heap. Notably, does not error if a required module is not
// found.
#[allow(dead_code)]
fn unchecked_dependencies(
    options: &Options,
    transaction: &Transaction,
    file: &FileKey,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
) -> FlowOrdSet<FileKey> {
    fn unchecked_dependency(
        transaction: &Transaction,
        m: &flow_heap::resolved_requires::Dependency,
    ) -> Option<FileKey> {
        let file = transaction.get_provider(m)?;
        let _parse = transaction.get_typed_parse(&file)?;
        match transaction.get_leader(&file) {
            None => Some(file),
            Some(_) => None,
        }
    }

    // let node_modules_containers = !Files.node_modules_containers in
    let node_modules_containers = files::node_modules_containers.read().unwrap();
    requires.iter().fold(
        FlowOrdSet::new(),
        |mut acc, r| match flow_services_module::imported_module(
            options,
            transaction,
            &node_modules_containers,
            file,
            None,
            r,
        ) {
            Err(_) => acc,
            Ok(m) => match unchecked_dependency(transaction, &m) {
                None => acc,
                Some(f) => {
                    acc.insert(f);
                    acc
                }
            },
        },
    )
}

fn prioritize_unchecked(
    orchestrator: Option<&ServerOrchestratorHandle>,
    unchecked: FlowOrdSet<FileKey>,
) -> Result<(), CheckedDependenciesCanceled> {
    if unchecked.is_empty() {
        return Ok(());
    }
    let n = unchecked.len();
    flow_hh_logger::info!("Canceling command due to {} unchecked dependencies", n);
    let cap = 10;
    for (i, f) in unchecked.iter().enumerate() {
        let i = i + 1;
        if i <= cap {
            flow_hh_logger::info!("{}/{}: {}", i, n, f.as_str());
        } else if flow_hh_logger::level::passes_min_level(flow_hh_logger::Level::Debug) {
            flow_hh_logger::debug!("{}/{}: {}", i, n, f.as_str());
        } else if i == cap + 1 {
            flow_hh_logger::info!("...");
        }
    }
    // `None` outside a server (the fox batch tools): nothing schedules rechecks, so the caller
    // just learns its dependencies are unchecked.
    if let Some(orchestrator) = orchestrator {
        flow_server_env::server_monitor_listener_state::push_dependencies_to_prioritize(
            orchestrator.recheck(),
            unchecked,
        );
    }
    Err(CheckedDependenciesCanceled)
}

/// `ensure_checked_dependencies` for a whole set, cancelling once for all of them rather than once
/// per file. A recheck merges everything the files it is given need, so scheduling the direct
/// dependencies is enough — the rest of the closure comes with them.
pub fn ensure_checked_dependencies_of_set<'a>(
    orchestrator: Option<&ServerOrchestratorHandle>,
    options: &Options,
    transaction: &Transaction,
    files: impl IntoIterator<Item = &'a FileKey>,
) -> Result<(), CheckedDependenciesCanceled> {
    let unchecked: FlowOrdSet<FileKey> = files
        .into_iter()
        .filter_map(|file| {
            let requires = transaction.get_requires(file)?;
            Some(unchecked_dependencies(
                options,
                transaction,
                file,
                &requires,
            ))
        })
        .flatten()
        .collect();

    prioritize_unchecked(orchestrator, unchecked)
}

// Ensures that dependencies are checked; schedules them to be checked and aborts the
// command if not.
//
// This is necessary because `check_contents` needs all of the dep type sigs to be
// available, but since it doesn't use workers it can't go parse everything itself.
fn ensure_checked_dependencies(
    orchestrator: Option<&ServerOrchestratorHandle>,
    options: &Options,
    transaction: &Transaction,
    file: &FileKey,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
) -> Result<(), CheckedDependenciesCanceled> {
    prioritize_unchecked(
        orchestrator,
        unchecked_dependencies(options, transaction, file, requires),
    )
}

// file+contents may not agree with file system state
pub fn check_contents(
    orchestrator: Option<&ServerOrchestratorHandle>,
    cache: &CheckContentsCache,
    options: &Options,
    transaction: Arc<Transaction>,
    all_unordered_libs: Arc<BTreeSet<FlowSmolStr>>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    docblock: Arc<flow_common::docblock::Docblock>,
    ast: Arc<ast::Program<Loc, Loc>>,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
) -> Result<
    Result<(Context<'static>, ast::Program<ALoc, (ALoc, Type)>), CheckedDependenciesCanceled>,
    flow_utils_concurrency::job_error::JobError,
> {
    with_timer(options, "MergeContents", || {
        if let Err(e) =
            ensure_checked_dependencies(orchestrator, options, &transaction, &filename, requires)
        {
            return Ok(Err(e));
        }
        Ok(Ok(merge_service::check_contents_context(
            cache,
            transaction,
            Arc::new(options.clone()),
            all_unordered_libs,
            master_cx,
            filename,
            ast,
            docblock,
            file_sig,
        )?))
    })
}

// IDE service: enable for_ide flag to ensure declaration files are fully checked
pub fn compute_env_of_contents(
    orchestrator: Option<&ServerOrchestratorHandle>,
    cache: &CheckContentsCache,
    options: &Options,
    transaction: Arc<Transaction>,
    all_unordered_libs: Arc<BTreeSet<FlowSmolStr>>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    docblock: Arc<flow_common::docblock::Docblock>,
    ast: Arc<ast::Program<Loc, Loc>>,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
) -> Result<
    Result<Context<'static>, CheckedDependenciesCanceled>,
    flow_utils_concurrency::job_error::JobError,
> {
    type_inference_hooks_js::with_for_ide(true, || {
        with_timer(options, "MergeContents", || {
            if let Err(e) = ensure_checked_dependencies(
                orchestrator,
                options,
                &transaction,
                &filename,
                requires,
            ) {
                return Ok(Err(e));
            }
            Ok(Ok(merge_service::compute_env_of_contents(
                cache,
                transaction,
                Arc::new(options.clone()),
                all_unordered_libs,
                master_cx,
                filename,
                ast,
                docblock,
                file_sig,
            )?))
        })
    })
}

// We assume that callers have already inspected the parse errors, so we discard them here.
pub fn type_parse_artifacts(
    orchestrator: Option<&ServerOrchestratorHandle>,
    cache: &CheckContentsCache,
    options: &Options,
    all_unordered_libs: Arc<BTreeSet<FlowSmolStr>>,
    transaction: Arc<Transaction>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    intermediate_result: (Option<ParseArtifacts>, ErrorSet),
) -> Result<FileArtifacts<'static>, TypeContentsError> {
    match intermediate_result {
        (Some(parse_artifacts), _errs) => {
            let ParseArtifacts {
                docblock,
                docblock_errors,
                ast,
                requires,
                file_sig,
                tolerable_errors,
                parse_errors,
            } = parse_artifacts;
            let (result, obj_to_obj_map) = {
                let loc_of_aloc = |loc: &ALoc| -> Loc { transaction.loc_of_aloc(loc) };
                type_inference_hooks_js::with_for_ide(true, || {
                    obj_to_obj_hook::with_obj_to_obj_hook(true, &loc_of_aloc, || {
                        check_contents(
                            orchestrator,
                            cache,
                            options,
                            transaction.dupe(),
                            all_unordered_libs,
                            master_cx,
                            filename,
                            docblock.dupe(),
                            ast.dupe(),
                            &requires,
                            file_sig.dupe(),
                        )
                    })
                })
            };
            let (cx, typed_ast) = match result {
                Ok(Ok(v)) => v,
                Ok(Err(CheckedDependenciesCanceled)) => {
                    return Err(TypeContentsError::CheckedDependenciesCanceled);
                }
                Err(_canceled) => {
                    return Err(TypeContentsError::CheckedDependenciesCanceled);
                }
            };
            Ok((
                Rc::new(ParseArtifacts {
                    docblock,
                    docblock_errors,
                    ast,
                    requires,
                    file_sig,
                    tolerable_errors,
                    parse_errors,
                }),
                Rc::new(TypecheckArtifacts {
                    cx,
                    typed_ast,
                    obj_to_obj_map,
                }),
            ))
        }
        (None, errs) => Err(TypeContentsError::Errors(errs)),
    }
}
