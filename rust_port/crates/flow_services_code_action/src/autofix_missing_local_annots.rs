/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/autofix_missing_local_annots.ml`

use std::collections::BTreeMap;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_util;

use crate::insert_type;
use crate::insert_type_imports;

pub fn map_of_fixable_missing_local_params(cx: &Context) -> BTreeMap<Loc, Type> {
    let all_errors = cx.errors();
    let aloc_tables = cx.aloc_tables();
    let missing_local_annot_lower_bounds = cx.missing_local_annot_lower_bounds();
    let mut acc = BTreeMap::new();
    for err in all_errors.iter() {
        match err.msg_of_error() {
            ErrorMessage::EMissingLocalAnnotation { .. } => {
                if let Some(aloc) = err.loc_of_error() {
                    let loc = aloc.to_loc_with_tables(&aloc_tables);
                    let aloc_fuzzy = flow_aloc::ALocFuzzy::new(aloc.dupe());
                    match missing_local_annot_lower_bounds.get(&aloc_fuzzy) {
                        None => {}
                        Some(types) => {
                            if let Some((t1, ts)) = types.split_first() {
                                let reason = type_util::reason_of_t(t1);
                                let t = match ts {
                                    [] => t1.dupe(),
                                    [t2, rest @ ..] => Type::new(TypeInner::UnionT(
                                        reason.dupe(),
                                        union_rep::make(
                                            None,
                                            union_rep::UnionKind::UnknownKind,
                                            t1.dupe(),
                                            t2.dupe(),
                                            rest.iter()
                                                .map(|t| t.dupe())
                                                .collect::<Vec<_>>()
                                                .into(),
                                        ),
                                    )),
                                };
                                acc.insert(loc, t);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    acc
}

pub fn fix_missing_param_annot_at_loc<'a, 'cx>(
    remote_converter: Option<&mut insert_type_imports::imports_helper::RemoteConverter<'a>>,
    cx: &Context<'cx>,
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    get_ast_from_shared_mem: &'a dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    get_haste_module_info: &'a dyn Fn(&FileKey) -> Option<flow_common_modulename::HasteModuleInfo>,
    get_type_sig: &'a dyn Fn(
        &FileKey,
    ) -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    >,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    ast: &ast::Program<Loc, Loc>,
    target: Loc,
    type_t: Type,
) -> insert_type::InsertTypeResult<ast::Program<Loc, Loc>> {
    insert_type::insert_type_t(
        cx,
        loc_of_aloc,
        get_ast_from_shared_mem,
        get_haste_module_info,
        get_type_sig,
        file_sig,
        typed_ast,
        false, // ~omit_targ_defaults:false
        false, // ~strict:false
        remote_converter,
        ast,
        target,
        type_t,
    )
}

pub fn fix_all_missing_param_annot_errors_in_file<'a, 'cx>(
    mut remote_converter: Option<&mut insert_type_imports::imports_helper::RemoteConverter<'a>>,
    cx: &Context<'cx>,
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    get_ast_from_shared_mem: &'a dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    get_haste_module_info: &'a dyn Fn(&FileKey) -> Option<flow_common_modulename::HasteModuleInfo>,
    get_type_sig: &'a dyn Fn(
        &FileKey,
    ) -> Option<
        flow_type_sig::packed_type_sig::Module<
            flow_type_sig::compact_table::Index<flow_aloc::ALoc>,
        >,
    >,
    file_sig: &Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    ast: Arc<ast::Program<Loc, Loc>>,
) -> insert_type::InsertTypeResult<ast::Program<Loc, Loc>> {
    let fixable_locs = map_of_fixable_missing_local_params(cx);
    let mut ast = Arc::unwrap_or_clone(ast);
    for (loc, t) in fixable_locs {
        ast = insert_type::insert_type_t(
            cx,
            loc_of_aloc,
            get_ast_from_shared_mem,
            get_haste_module_info,
            get_type_sig,
            file_sig,
            typed_ast,
            false, // ~omit_targ_defaults:false
            false, // ~strict:false
            remote_converter.as_deref_mut(),
            &ast,
            loc,
            t,
        )?;
    }
    Ok(ast)
}
