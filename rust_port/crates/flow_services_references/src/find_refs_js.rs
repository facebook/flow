/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/references/findRefs_js.ml`

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_analysis::scope_builder;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_services_get_def::find_refs_utils::AstInfo;
use flow_services_get_def::get_def_types::DefInfo;
use flow_services_get_def::get_def_types::PropertyDefInfo;
use flow_services_get_def::get_def_types::Purpose;
use flow_services_get_def::get_def_types::SinglePropertyDefInfo;
use flow_services_get_def::get_def_utils;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use vec1::Vec1;

use crate::find_refs_types;
use crate::find_refs_types::FindRefsFound;
use crate::find_refs_types::FindRefsOk;
use crate::find_refs_types::Kind;
use crate::find_refs_types::RefKind;
use crate::find_refs_types::Request;
use crate::local_import_ref_searcher;
use crate::property_find_refs;
use crate::variable_find_refs;

// Sort and dedup by loc.
//
// This will have to be revisited if we ever need to report multiple ref kinds for
// a single location.
fn sort_and_dedup(refs: FindRefsFound) -> FindRefsFound {
    let mut refs = refs;
    refs.sort_by(|(_, loc1), (_, loc2)| loc1.cmp(loc2));
    refs.reverse();
    refs.dedup_by(|(_, loc1), (_, loc2)| *loc1 == *loc2);
    refs.reverse();
    refs
}

pub fn local_refs_of_find_ref_request<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    ast_info: &AstInfo,
    cx: &Context<'cx>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    file_key: &FileKey,
    request: &Request,
) -> Result<Result<FindRefsOk, String>, flow_utils_concurrency::job_error::JobError> {
    let def_info = &request.def_info;
    let var_refs = |prop_refs: &[find_refs_types::SingleRef]| -> Result<
        Vec<find_refs_types::SingleRef>,
        flow_utils_concurrency::job_error::JobError,
    > {
        let def_locs = get_def_utils::all_locs_of_def_info(def_info);
        let (ast, file_sig, _) = ast_info;
        let search_result = local_import_ref_searcher::search(
            loc_of_aloc,
            cx,
            file_sig,
            ast,
            typed_ast,
            &def_locs,
        )?;
        let import_def_locs = search_result.local_locs;
        let remote_locs = search_result.remote_locs;
        let scope_info = scope_builder::program(cx.enable_enums(), true, ast);
        // Property refs might contain binding destructuring pattern identifiers.
        // We should find all local references of them.
        let prop_ref_locs: Vec<Loc> = prop_refs.iter().map(|(_, loc)| loc.dupe()).collect();
        let mut starting_locs = Vec::new();
        starting_locs.extend(prop_ref_locs);
        starting_locs.extend(import_def_locs);
        starting_locs.extend(def_locs);
        let remote_refs: Vec<find_refs_types::SingleRef> = remote_locs
            .into_iter()
            .map(|l| (RefKind::Local, l))
            .collect();
        let local_refs =
            variable_find_refs::local_find_refs(&scope_info, &starting_locs).unwrap_or_default();
        let mut result = remote_refs;
        result.extend(local_refs);
        Ok(result)
    };
    let merge_with_var_refs = |prop_refs_result: Result<
        Vec<find_refs_types::SingleRef>,
        String,
    >|
     -> Result<
        Result<FindRefsOk, String>,
        flow_utils_concurrency::job_error::JobError,
    > {
        match prop_refs_result {
            Ok(prop_refs) => {
                let vr = var_refs(&prop_refs)?;
                let mut all_refs = prop_refs;
                all_refs.extend(vr);
                Ok(Ok(FindRefsOk::FoundReferences(all_refs)))
            }
            Err(e) => {
                let vr = var_refs(&[])?;
                if vr.is_empty() {
                    Ok(Err(e))
                } else {
                    Ok(Ok(FindRefsOk::FoundReferences(vr)))
                }
            }
        }
    };
    match def_info {
        DefInfo::VariableDefinition(def_locs, name) => {
            let obj_props: Vec<SinglePropertyDefInfo> = def_locs
                .iter()
                .map(|l| SinglePropertyDefInfo::ObjectProperty(l.clone()))
                .collect();
            let prop_refs = match (obj_props.as_slice(), name) {
                ([], _) => Ok(Vec::new()),
                (_, None) => Err("No available names to find property refs".to_string()),
                ([hd, tl @ ..], Some(name)) => {
                    let mut props_vec = vec![hd.clone()];
                    props_vec.extend(tl.iter().cloned());
                    let props_info = Vec1::try_from_vec(props_vec).unwrap();
                    property_find_refs::property_find_refs_in_file(
                        loc_of_aloc,
                        ast_info,
                        cx,
                        typed_ast,
                        obj_to_obj_map,
                        file_key,
                        &PropertyDefInfo::OrdinaryProperty {
                            props_info,
                            name: name.clone(),
                        },
                    )?
                }
            };
            merge_with_var_refs(prop_refs)
        }
        DefInfo::PropertyDefinition(props_info) => {
            let prop_refs = property_find_refs::property_find_refs_in_file(
                loc_of_aloc,
                ast_info,
                cx,
                typed_ast,
                obj_to_obj_map,
                file_key,
                props_info,
            )?;
            merge_with_var_refs(prop_refs)
        }
        DefInfo::NoDefinition(no_def_reason) => {
            Ok(Ok(FindRefsOk::NoDefinition(no_def_reason.clone())))
        }
    }
}

pub fn find_local_refs<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    file_key: &FileKey,
    ast_info: &AstInfo,
    cx: &Context<'cx>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    kind: Kind,
    line: u32,
    col: u32,
) -> Result<Result<(DefInfo, FindRefsOk), String>, flow_utils_concurrency::job_error::JobError> {
    let cursor_loc = Loc::cursor(Some(file_key.dupe()), line as i32, col as i32);
    let def_info = match get_def_utils::get_def_info(
        loc_of_aloc,
        &Purpose::FindReferences,
        ast_info,
        cx,
        typed_ast,
        obj_to_obj_map,
        &cursor_loc,
    )? {
        Ok(d) => d,
        Err(e) => return Ok(Err(e)),
    };
    let request = Request {
        def_info: def_info.clone(),
        kind,
    };
    let result = match local_refs_of_find_ref_request(
        loc_of_aloc,
        ast_info,
        cx,
        typed_ast,
        obj_to_obj_map,
        file_key,
        &request,
    )? {
        Ok(r) => r,
        Err(e) => return Ok(Err(e)),
    };
    let result = match result {
        FindRefsOk::FoundReferences(refs) => FindRefsOk::FoundReferences(sort_and_dedup(refs)),
        FindRefsOk::NoDefinition(no_def_reason) => FindRefsOk::NoDefinition(no_def_reason),
    };
    Ok(Ok((def_info, result)))
}
