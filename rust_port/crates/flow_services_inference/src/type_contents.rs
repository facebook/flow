/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;
use std::time::Instant;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::options::Options;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_heap::parsing_heaps::SharedMem;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parsing::docblock_parser;
use flow_parsing::parsing_service;
use flow_parsing::parsing_service::ParseResult;
use flow_parsing::parsing_service::ParseSkipReason;
use flow_server_env::server_env::Env;
use flow_server_env::server_monitor_listener_state;
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
fn do_parse_wrapper(options: &Options, filename: &FileKey, contents: &str) -> ParseContentsReturn {
    let max_tokens = options.max_header_tokens;
    let (docblock_errors, docblock) = docblock_parser::parse_docblock(
        max_tokens as usize,
        &options.file_options,
        filename,
        contents,
    );
    let parse_result = parsing_service::do_parse(options, &docblock, &[], Ok(contents), filename);
    match parse_result {
        ParseResult::ParseOk {
            ast,
            requires,
            file_sig,
            tolerable_errors,
            ..
        } => ParseContentsReturn::Parsed(ParseArtifacts {
            docblock,
            docblock_errors,
            ast,
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
            docblock,
            docblock_errors,
            ast,
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
    contents: &str,
    filename: &FileKey,
) -> (Option<ParseArtifacts>, ErrorSet) {
    with_timer(options, "Parsing", || {
        match do_parse_wrapper(options, filename, contents) {
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
    let mut suppressions = env.errors.suppressions.clone();
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
        errors,
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
        warnings,
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
    shared_mem: &SharedMem,
    filename: &FileKey,
    result: Result<&FileArtifacts, &TypeContentsError>,
) -> (ConcreteLocPrintableErrorSet, ConcreteLocPrintableErrorSet) {
    let root = &*options.root;
    let loc_of_aloc = |aloc: &ALoc| -> Loc { shared_mem.loc_of_aloc(aloc) };
    let get_ast =
        |file: &FileKey| -> Option<Arc<ast::Program<Loc, Loc>>> { shared_mem.get_ast(file) };
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
    shared_mem: &SharedMem,
    file: &FileKey,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    node_modules_containers: &std::collections::BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> FlowOrdSet<FileKey> {
    fn unchecked_dependency(
        shared_mem: &SharedMem,
        m: &flow_heap::entity::Dependency,
    ) -> Option<FileKey> {
        let file = shared_mem.get_provider(m)?;
        let _parse = shared_mem.get_typed_parse(&file)?;
        match shared_mem.get_leader(&file) {
            None => Some(file),
            Some(_) => None,
        }
    }

    requires.iter().fold(
        FlowOrdSet::new(),
        |mut acc, r| match flow_services_module::imported_module(
            options,
            shared_mem,
            node_modules_containers,
            file,
            None,
            r,
        ) {
            Err(_) => acc,
            Ok(m) => match unchecked_dependency(shared_mem, &m) {
                None => acc,
                Some(f) => {
                    acc.insert(f);
                    acc
                }
            },
        },
    )
}

// Ensures that dependencies are checked; schedules them to be checked and aborts the
// command if not.
//
// This is necessary because `check_contents` needs all of the dep type sigs to be
// available, but since it doesn't use workers it can't go parse everything itself.
fn ensure_checked_dependencies(
    options: &Options,
    shared_mem: &SharedMem,
    file: &FileKey,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    node_modules_containers: &std::collections::BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> Result<(), CheckedDependenciesCanceled> {
    let unchecked_deps =
        unchecked_dependencies(options, shared_mem, file, requires, node_modules_containers);
    if !unchecked_deps.is_empty() {
        let n = unchecked_deps.len();
        eprintln!("Canceling command due to {} unchecked dependencies", n);
        server_monitor_listener_state::push_dependencies_to_prioritize(unchecked_deps);
        Err(CheckedDependenciesCanceled)
    } else {
        Ok(())
    }
}

// file+contents may not agree with file system state
pub fn check_contents(
    options: &Options,
    shared_mem: Arc<SharedMem>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    docblock: Arc<flow_common::docblock::Docblock>,
    ast: Arc<ast::Program<Loc, Loc>>,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
    node_modules_containers: &std::collections::BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> Result<
    Result<(Context<'static>, ast::Program<ALoc, (ALoc, Type)>), CheckedDependenciesCanceled>,
    flow_utils_concurrency::job_error::JobError,
> {
    with_timer(options, "MergeContents", || {
        if let Err(e) = ensure_checked_dependencies(
            options,
            &shared_mem,
            &filename,
            requires,
            node_modules_containers,
        ) {
            return Ok(Err(e));
        }
        Ok(Ok(merge_service::check_contents_context(
            shared_mem,
            Arc::new(options.clone()),
            master_cx,
            filename,
            ast,
            docblock,
            file_sig,
            node_modules_containers,
        )?))
    })
}

// IDE service: enable for_ide flag to ensure declaration files are fully checked
pub fn compute_env_of_contents(
    options: &Options,
    shared_mem: Arc<SharedMem>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    docblock: Arc<flow_common::docblock::Docblock>,
    ast: Arc<ast::Program<Loc, Loc>>,
    requires: &[flow_common::flow_import_specifier::FlowImportSpecifier],
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
    node_modules_containers: &std::collections::BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
) -> Result<
    Result<(Context<'static>, ast::Program<ALoc, ALoc>), CheckedDependenciesCanceled>,
    flow_utils_concurrency::job_error::JobError,
> {
    type_inference_hooks_js::with_for_ide(true, || {
        with_timer(options, "MergeContents", || {
            if let Err(e) = ensure_checked_dependencies(
                options,
                &shared_mem,
                &filename,
                requires,
                node_modules_containers,
            ) {
                return Ok(Err(e));
            }
            Ok(Ok(merge_service::compute_env_of_contents(
                shared_mem,
                Arc::new(options.clone()),
                master_cx,
                filename,
                ast,
                docblock,
                file_sig,
                node_modules_containers,
            )?))
        })
    })
}

// We assume that callers have already inspected the parse errors, so we discard them here.
pub fn type_parse_artifacts(
    options: &Options,
    shared_mem: Arc<SharedMem>,
    master_cx: Arc<MasterContext>,
    filename: FileKey,
    intermediate_result: (Option<ParseArtifacts>, ErrorSet),
    node_modules_containers: &std::collections::BTreeMap<FlowSmolStr, BTreeSet<FlowSmolStr>>,
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
                let loc_of_aloc = |loc: &ALoc| -> Loc { shared_mem.loc_of_aloc(loc) };
                type_inference_hooks_js::with_for_ide(true, || {
                    obj_to_obj_hook::with_obj_to_obj_hook(true, &loc_of_aloc, || {
                        check_contents(
                            options,
                            shared_mem.dupe(),
                            master_cx,
                            filename,
                            Arc::new(docblock.clone()),
                            Arc::new(ast.clone()),
                            &requires,
                            file_sig.dupe(),
                            node_modules_containers,
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
                ParseArtifacts {
                    docblock,
                    docblock_errors,
                    ast,
                    requires,
                    file_sig,
                    tolerable_errors,
                    parse_errors,
                },
                TypecheckArtifacts {
                    cx,
                    typed_ast,
                    obj_to_obj_map,
                },
            ))
        }
        (None, errs) => Err(TypeContentsError::Errors(errs)),
    }
}
