/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! This module sets up the definitions for JavaScript globals. Eventually, this
//! module should become redundant: we should be able to automatically interpret
//! TypeScript type definition files for these and many other primitives. That
//! said, in some cases handcoding may turn out to be necessary because the type
//! system is not powerful enough to encode the invariants of a library
//! function. In any case, this part of the design must be revisited in the
//! future.

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocTable;
use flow_common::flow_projects::FlowProjects;
use flow_common::options::Options;
use flow_heap::parsing_heaps::SharedMem;
use flow_imports_exports::exports;
use flow_imports_exports::exports::Exports;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_type_sig::type_sig_options::TypeSigOptions;
use flow_typing::merge;
use flow_typing::type_inference;
use flow_typing_builtins::builtins::Builtins;
use flow_typing_context::Context;
use flow_typing_context::MasterContext;
use flow_typing_context::ResolvedRequire;
use flow_typing_context::metadata_of_options;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::flow_error::FlowError;
use flow_typing_flow_common::flow_js_utils::add_output_non_speculating;

/// process all lib files: parse, infer, and add the symbols they define
/// to the builtins object.
///
/// Note: we support overrides of definitions found earlier in the list of
/// files by those of the same name found in later ones, so caller must
/// preserve lib path declaration order in the (flattened) list of files
/// passed.
///
/// returns (success, parse and signature errors, exports)
fn load_lib_files(
    ccx: &Rc<flow_typing_context::ComponentT<'static>>,
    options: &Options,
    reader: &SharedMem,
    files: &[(Option<String>, String)],
) -> (
    bool,
    MasterContext,
    Option<Context<'static>>,
    (Exports, Vec<(FlowProjects, Exports)>),
) {
    let mut ok = true;
    let mut ordered_asts: Vec<(Option<String>, Arc<ast::Program<Loc, Loc>>)> = Vec::new();
    for (scoped_dir_opt, file) in files {
        let lib_file = FileKey::lib_file_of_absolute(file);
        match reader.get_ast(&lib_file) {
            Some(ast) => {
                ordered_asts.push((scoped_dir_opt.clone(), ast));
            }
            None => {
                flow_hh_logger::info!("Failed to find {:?} in parsing heap.", lib_file);
                ok = false;
            }
        }
    }
    ordered_asts.reverse();

    let (builtin_exports, master_cx, cx_opt) = if ok {
        let sig_opts = TypeSigOptions::builtin_options(options);
        let (builtin_errors, master_cx) =
            merge::merge_lib_files(&options.projects_options, &sig_opts, &ordered_asts);
        match master_cx {
            MasterContext::EmptyMasterContext => (
                (Exports::empty(), Vec::new()),
                MasterContext::EmptyMasterContext,
                None,
            ),
            MasterContext::NonEmptyMasterContext {
                ref builtin_leader_file_key,
                ref unscoped_builtins,
                ref scoped_builtins,
            } => {
                let mut metadata = metadata_of_options(options);
                metadata.overridable.checked = false;
                let cx = Context::make(
                    ccx.dupe(),
                    metadata,
                    builtin_leader_file_key.dupe(),
                    {
                        let builtin_leader_file_key = builtin_leader_file_key.dupe();
                        Rc::new(LazyCell::new(Box::new(move || {
                            Rc::new(ALocTable::empty(builtin_leader_file_key))
                        })
                            as Box<dyn FnOnce() -> Rc<ALocTable>>))
                    },
                    Rc::new(move |_cx: &Context, _| ResolvedRequire::MissingModule),
                    Rc::new(move |_cx: &Context| Builtins::empty()),
                    flow_utils_concurrency::check_budget::CheckBudget::new(
                        options
                            .merge_timeout
                            .map(std::time::Duration::from_secs_f64),
                    ),
                );
                let file_keys_with_comments: Vec<(FileKey, &[flow_parser::ast::Comment<Loc>])> =
                    ordered_asts
                        .iter()
                        .map(|(_, ast)| {
                            let loc = &ast.loc;
                            let source = loc
                                .source
                                .as_ref()
                                .expect("libdef ast missing source")
                                .dupe();
                            let comments: &[flow_parser::ast::Comment<Loc>] = &ast.all_comments;
                            (source, comments)
                        })
                        .collect();
                let (severity_cover, suppressions, suppression_errors) =
                    type_inference::scan_for_suppressions(
                        true,
                        &options.lint_severities,
                        file_keys_with_comments,
                    );
                cx.reset_errors(builtin_errors);
                cx.add_severity_covers(severity_cover);
                cx.add_error_suppressions(suppressions);
                for error in suppression_errors {
                    add_output_non_speculating(&cx, error);
                }
                let exports_of_builtins = |builtins: &flow_typing_context::BuiltinsGroup| {
                    exports::of_builtins(&builtins.builtins)
                };
                let scoped_exports = scoped_builtins
                    .iter()
                    .map(|(project, builtins)| (*project, exports_of_builtins(builtins)))
                    .collect();
                (
                    (exports_of_builtins(unscoped_builtins), scoped_exports),
                    master_cx,
                    Some(cx),
                )
            }
        }
    } else {
        (
            (Exports::empty(), Vec::new()),
            MasterContext::EmptyMasterContext,
            None,
        )
    };
    (ok, master_cx, cx_opt, builtin_exports)
}

pub struct InitResult {
    pub ok: bool,
    pub errors: BTreeMap<FileKey, ErrorSet>,
    pub warnings: BTreeMap<FileKey, ErrorSet>,
    pub suppressions: ErrorSuppressions,
    pub exports: (Exports, Vec<(FlowProjects, Exports)>),
    pub master_cx: Arc<MasterContext>,
}

// OCaml: let error_set_to_filemap ~init err_set =
fn error_set_to_filemap(map: &mut BTreeMap<FileKey, ErrorSet>, err_set: ErrorSet) {
    err_set.fold((), |(), error: FlowError<ALoc>| {
        let file = error.source_file().dupe();
        match map.get(&file) {
            None => {
                map.insert(file, ErrorSet::singleton(error));
            }
            Some(set) => {
                let mut new_set = set.clone();
                new_set.add(error);
                map.insert(file, new_set);
            }
        }
    });
}

/// initialize builtins:
/// parse and do local inference on library files, and set up master context.
/// returns list of (lib file, success) pairs.
pub fn init(
    options: &Options,
    reader: &SharedMem,
    lib_files: Vec<(Option<String>, String)>,
) -> InitResult {
    let ccx = Rc::new(flow_typing_context::make_ccx());
    let (ok, master_cx, cx_opt, exports) = load_lib_files(&ccx, options, reader, &lib_files);
    let (errors, warnings, suppressions) = match cx_opt {
        None => (BTreeMap::new(), BTreeMap::new(), ErrorSuppressions::empty()),
        Some(cx) => {
            let errors = cx.errors();
            let suppressions = cx.error_suppressions().clone();
            let severity_cover = cx.severity_cover().dupe();
            let include_suppressions = cx.include_suppressions();
            let aloc_tables = cx.aloc_tables();
            let mut suppressions = suppressions;
            let (errors, warnings) = suppressions.filter_lints(
                errors,
                &aloc_tables,
                include_suppressions,
                &severity_cover,
            );
            // Break Rc cycles in the init Context before dropping it.
            cx.post_inference_cleanup();
            let mut error_map = BTreeMap::new();
            let mut warning_map = BTreeMap::new();
            for (file, _) in aloc_tables.iter() {
                error_map.insert(file.dupe(), ErrorSet::empty());
                warning_map.insert(file.dupe(), ErrorSet::empty());
            }
            error_set_to_filemap(&mut error_map, errors);
            error_set_to_filemap(&mut warning_map, warnings);
            (error_map, warning_map, suppressions)
        }
    };
    InitResult {
        ok,
        errors,
        warnings,
        suppressions,
        exports,
        master_cx: Arc::new(master_cx),
    }
}
