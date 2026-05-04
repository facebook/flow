/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/autofix_exports.ml`

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::files::FileOptions;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_type_sig::signature_error::SignatureError;
use flow_type_sig::signature_error::TolerableError;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;

use crate::insert_type;
use crate::insert_type::InsertTypeResult;
use crate::insert_type_imports;

pub fn set_of_fixable_signature_verification_locations(
    tolerable_errors: &[TolerableError<Loc>],
) -> BTreeSet<Loc> {
    let add_fixable_sig_ver_error =
        |mut acc: BTreeSet<Loc>, error: &TolerableError<Loc>| -> BTreeSet<Loc> {
            match error {
                TolerableError::SignatureVerificationError(
                    SignatureError::ExpectedAnnotation(loc, _)
                    | SignatureError::UnexpectedExpression(loc, _)
                    | SignatureError::UnexpectedObjectKey(loc, _)
                    | SignatureError::EmptyArray(loc)
                    | SignatureError::EmptyObject(loc)
                    | SignatureError::UnexpectedArraySpread(loc, _),
                ) => {
                    acc.insert(loc.dupe());
                    acc
                }
                _ => acc,
            }
        };
    tolerable_errors.iter().fold(BTreeSet::new(), |acc, error| {
        add_fixable_sig_ver_error(acc, error)
    })
}

pub(crate) fn fix_signature_verification_error_at_loc<'a, 'cx>(
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
) -> InsertTypeResult<ast::Program<Loc, Loc>> {
    insert_type::insert_type(
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
    )
}

pub fn fix_signature_verification_errors<'a, 'cx>(
    file_key: FileKey,
    cx: &Context<'cx>,
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    file_options: Arc<FileOptions>,
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
    locs: &BTreeSet<Loc>,
) -> (ast::Program<Loc, Loc>, Vec<String>) {
    let mut remote_converter = insert_type_imports::imports_helper::RemoteConverter::new(
        Box::new(|aloc| loc_of_aloc(aloc)),       // ~loc_of_aloc
        file_options,                             // ~file_options
        Box::new(|fk| get_haste_module_info(fk)), // ~get_haste_module_info
        Box::new(|fk| get_type_sig(fk)),          // ~get_type_sig
        0,                                        // ~iteration:0
        file_key,                                 // ~file:file_key
        BTreeSet::new(),                          // ~reserved_names:SSet.empty
    );
    let mut ast = Arc::unwrap_or_clone(ast);
    let mut it_errors = Vec::<String>::new();
    for loc in locs.iter() {
        match fix_signature_verification_error_at_loc(
            Some(&mut remote_converter),
            cx,
            loc_of_aloc,
            get_ast_from_shared_mem,
            get_haste_module_info,
            get_type_sig,
            file_sig,
            typed_ast,
            &ast,
            loc.dupe(),
        ) {
            Ok(new_ast) => {
                ast = new_ast;
            }
            Err(err) => {
                it_errors.push(insert_type::error_to_string(&err));
            }
        }
    }
    let statements: Arc<[_]> = insert_type::add_imports(&remote_converter, &ast.statements).into();
    let ast = ast::Program {
        loc: ast.loc,
        statements,
        ..ast
    };
    (ast, it_errors)
}
