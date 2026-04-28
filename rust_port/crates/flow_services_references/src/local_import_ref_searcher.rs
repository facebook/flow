/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::Require;
use flow_parser_utils::file_sig::RequireBindings;
use flow_services_get_def::get_def_js;
use flow_services_get_def::get_def_js::GetDefResult;
use flow_services_get_def::get_def_types::Purpose;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use flow_typing_utils::typed_ast_utils::AvailableAst;
use vec1::Vec1;

pub struct SearchResult {
    pub local_locs: Vec<Loc>,
    pub remote_locs: Vec<Loc>,
}

pub fn search<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    cx: &Context<'cx>,
    file_sig: &Arc<FileSig>,
    ast: &ast::Program<Loc, Loc>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    def_locs: &[Loc],
) -> Result<SearchResult, flow_utils_concurrency::job_error::JobError> {
    let mut require_name_locs: Vec<(Loc, Loc)> = Vec::new();
    for require in file_sig.requires() {
        match require {
            Require::Require {
                bindings: Some(bindings),
                ..
            } => {
                fn collect_bindings(acc: &mut Vec<(Loc, Loc)>, bindings: &RequireBindings) {
                    match bindings {
                        RequireBindings::BindIdent(ident) => {
                            acc.push((ident.0.dupe(), ident.0.dupe()));
                        }
                        RequireBindings::BindNamed(names) => {
                            for (_, nested_bindings) in names {
                                collect_bindings(acc, nested_bindings);
                            }
                        }
                    }
                }
                collect_bindings(&mut require_name_locs, bindings);
            }
            Require::Import {
                named,
                types,
                ns,
                type_ns: _,
                ..
            } => {
                let add_map = |map: &BTreeMap<
                    FlowSmolStr,
                    BTreeMap<FlowSmolStr, Vec1<file_sig::ImportedLocs>>,
                >,
                               acc: &mut Vec<(Loc, Loc)>| {
                    for inner_map in map.values() {
                        for nel in inner_map.values() {
                            for l in nel.iter() {
                                acc.push((l.remote_loc.dupe(), l.local_loc.dupe()));
                            }
                        }
                    }
                };
                if let Some(ns_ident) = ns {
                    require_name_locs.push((ns_ident.0.dupe(), ns_ident.0.dupe()));
                }
                add_map(named, &mut require_name_locs);
                add_map(types, &mut require_name_locs);
            }
            Require::Require { bindings: None, .. }
            | Require::ImportDynamic { .. }
            | Require::Import0 { .. }
            | Require::ImportSyntheticUserland { .. }
            | Require::ImportSyntheticHaste { .. }
            | Require::ExportFrom { .. } => {}
        }
    }
    //   let (local_locs, remote_locs) =
    //     Base.List.fold
    //       require_name_locs
    //       ~init:(LocSet.empty, LocSet.empty)
    //       ~f:(fun ((local_locs, remote_locs) as acc) (remote_loc, local_loc) ->
    let mut local_locs: BTreeSet<Loc> = BTreeSet::new();
    let mut remote_locs: BTreeSet<Loc> = BTreeSet::new();
    for (remote_loc, local_loc) in &require_name_locs {
        //         match
        //           GetDef_js.get_def
        //             ~loc_of_aloc
        //             ~cx
        //             ~file_sig
        //             ~ast
        //             ~available_ast:(Typed_ast_utils.Typed_ast typed_ast)
        //             ~purpose:Get_def_types.Purpose.FindReferences
        //             remote_loc
        //         with
        let result = get_def_js::get_def(
            loc_of_aloc,
            cx,
            file_sig,
            None,
            ast,
            AvailableAst::TypedAst(typed_ast.clone()),
            &Purpose::FindReferences,
            remote_loc,
        )?;
        match result {
            GetDefResult::Def(locs, _) | GetDefResult::Partial(locs, _, _) => {
                if locs.iter().any(|l| def_locs.contains(l)) {
                    if *remote_loc == *local_loc {
                        local_locs.insert(local_loc.dupe());
                    } else {
                        remote_locs.insert(remote_loc.dupe());
                    }
                }
            }
            GetDefResult::BadLoc(_) | GetDefResult::DefError(_) => {}
        }
    }
    Ok(SearchResult {
        local_locs: local_locs.into_iter().collect(),
        remote_locs: remote_locs.into_iter().collect(),
    })
}
