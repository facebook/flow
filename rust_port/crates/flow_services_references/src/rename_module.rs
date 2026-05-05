/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/references/renameModule.ml`

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::options::Options;
use flow_heap::parsing_heaps::SharedMem;
use flow_lsp::lsp::loc_to_lsp_range;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::FileSigOptions;
use flow_parser_utils::file_sig::Require;
use flow_parser_utils::file_sig::Source;
use flow_parser_utils_output::js_layout_generator;
use flow_parser_utils_output::pretty_printer;
use flow_server_env::flow_lsp_conversions;
use lsp_types::TextEdit;
use lsp_types::Url;
use lsp_types::WorkspaceEdit;

fn get_dependents(
    shared_mem: &SharedMem,
    file_key: &FileKey,
) -> Option<Vec<(FileKey, Option<Arc<ast::Program<Loc, Loc>>>)>> {
    let haste_info = shared_mem.get_haste_info(file_key);
    haste_info.map(|haste_info| {
        let haste_module = shared_mem.get_haste_module_unsafe(&haste_info);
        let haste_dependents = haste_module.get_dependents();
        let mut dependents = Vec::new();
        for file_key in haste_dependents {
            let ast = shared_mem.get_ast(&file_key);
            dependents.insert(0, (file_key, ast));
        }
        dependents
    })
}

// TODO: This only handles global haste paths. It needs to handle relative
// paths, haste package + relative paths, and node requires
fn get_loc_to_replacement_map(
    old_haste_name: &str,
    new_haste_name: &str,
    file_sig: &FileSig,
) -> BTreeMap<Loc, String> {
    file_sig
        .requires()
        .iter()
        .fold(BTreeMap::new(), |mut acc, require| match require {
            Require::Require {
                source: Source(loc, mref),
                prefix: Some(pre),
                ..
            } => {
                if old_haste_name == mref.as_str() {
                    acc.insert(loc.dupe(), format!("{}{}", pre, new_haste_name));
                    acc
                } else {
                    acc
                }
            }
            Require::Require {
                source: Source(loc, mref),
                prefix: None,
                ..
            }
            | Require::ImportDynamic {
                source: Source(loc, mref),
                ..
            }
            | Require::Import0 {
                source: Source(loc, mref),
            }
            | Require::Import {
                source: Source(loc, mref),
                ..
            }
            | Require::ExportFrom {
                source: Source(loc, mref),
            } => {
                if old_haste_name == mref.as_str() {
                    acc.insert(loc.dupe(), new_haste_name.to_string());
                    acc
                } else {
                    acc
                }
            }
            Require::ImportSyntheticUserland { .. } | Require::ImportSyntheticHaste { .. } => acc,
        })
}

fn get_edits_for_file(
    old_haste_name: &str,
    new_haste_name: &str,
    file_sig: &FileSig,
) -> Vec<TextEdit> {
    let loc_to_replacement_map =
        get_loc_to_replacement_map(old_haste_name, new_haste_name, file_sig);
    let mut acc = Vec::new();
    for (loc, replacement) in loc_to_replacement_map {
        let string_layout = js_layout_generator::string_literal(
            &js_layout_generator::default_opts(),
            &Loc::none(),
            &ast_builder::string_literal(None, &replacement),
        );
        let new_text = pretty_printer::print(true, &string_layout).contents();
        acc.insert(
            0,
            TextEdit {
                range: loc_to_lsp_range(&loc),
                new_text,
            },
        );
    }
    acc
}

pub fn get_rename_edits(
    shared_mem: &SharedMem,
    options: &Options,
    old_haste_name: &str,
    new_haste_name: &str,
    old_file_key: &FileKey,
) -> Result<WorkspaceEdit, String> {
    let opts = FileSigOptions {
        enable_enums: options.enums,
        enable_jest_integration: options.enable_jest_integration,
        enable_relay_integration: options.enable_relay_integration,
        // This field is only necessary for implicit imports for multiplatform purposes.
        // Renaming will never edit these implicit imports.
        explicit_available_platforms: None,
        file_options: options.file_options.dupe(),
        haste_module_ref_prefix: options.haste_module_ref_prefix.dupe(),
        project_options: options.projects_options.dupe(),
        relay_integration_module_prefix: options.relay_integration_module_prefix.dupe(),
    };
    let workspace_edit = {
        get_dependents(shared_mem, old_file_key).map(|dependents| {
            // TODO: Allow partial edits
            let changes: Result<HashMap<Url, Vec<TextEdit>>, String> = dependents
                .into_iter()
                .try_fold(HashMap::new(), |mut uri_map, (file_key, ast_)| {
                    let Some(ast) = ast_ else {
                        return Ok(uri_map);
                    };
                    let uri = flow_lsp_conversions::file_key_to_uri(Some(&file_key))?;
                    let dependent_file_sig = FileSig::from_program(&file_key, &ast, &opts);
                    let edits =
                        get_edits_for_file(old_haste_name, new_haste_name, &dependent_file_sig);
                    uri_map.insert(uri, edits);
                    Ok(uri_map)
                });
            changes.map(|changes| WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            })
        })
    };
    workspace_edit.unwrap_or_else(|| {
        Ok(WorkspaceEdit {
            changes: Some(HashMap::new()),
            ..Default::default()
        })
    })
}
