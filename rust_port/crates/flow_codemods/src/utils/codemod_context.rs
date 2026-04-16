/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_aloc::ALoc;
use flow_common::docblock::Docblock;
use flow_common::options::Options;
use flow_common_ty::ty::ALocElt;
use flow_heap::parsing_heaps::SharedMem;
use flow_lint_settings::lint_settings::LintSettings;
use flow_lint_settings::severity::Severity;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_type_sig::packed_type_sig;
use flow_typing::ty_normalizer_flow;
use flow_typing::typed_ast_finder;
use flow_typing_context::Context;
use flow_typing_context::Metadata;
use flow_typing_ty_normalizer::normalizer::Error as NormalizerError;
use flow_typing_type::type_::Type;

pub mod typed {
    use super::*;

    pub struct TypedCodemodContext<'cx> {
        pub file: FileKey,
        pub type_sig: Arc<packed_type_sig::Module<Loc>>,
        pub file_sig: Arc<FileSig>,
        pub metadata: Metadata,
        pub options: Options,
        pub cx: Context<'cx>,
        pub typed_ast: ast::Program<ALoc, (ALoc, Type)>,
        pub docblock: Docblock,
        pub iteration: i32,
        pub reader: Arc<SharedMem>,
    }

    pub enum Error {
        MissingTypeAnnotation,
        NormalizationError(NormalizerError),
    }

    pub fn file<'a>(ccx: &'a TypedCodemodContext<'_>) -> &'a FileKey {
        &ccx.file
    }

    pub fn ty_at_loc<'a, 'cx>(
        norm_opts: flow_typing_ty_normalizer::env::Options,
        ccx: &'a TypedCodemodContext<'cx>,
        loc: Loc,
    ) -> Result<ALocElt, Error> {
        let TypedCodemodContext {
            cx,
            file_sig,
            typed_ast,
            ..
        } = ccx;
        let aloc: ALoc = ALoc::of_loc(loc);
        match typed_ast_finder::find_exact_match_annotation(typed_ast, aloc) {
            None => Err(Error::MissingTypeAnnotation),
            Some(t) => {
                let genv =
                    ty_normalizer_flow::mk_genv(norm_opts, cx, Some(typed_ast), file_sig.clone());
                match ty_normalizer_flow::from_type(&genv, &t) {
                    Ok(ty) => Ok(ty),
                    Err(e) => Err(Error::NormalizationError(e)),
                }
            }
        }
    }

    pub fn file_sig<'a>(ccx: &'a TypedCodemodContext<'_>) -> &'a Arc<FileSig> {
        &ccx.file_sig
    }

    pub fn metadata<'a>(ccx: &'a TypedCodemodContext<'_>) -> &'a Metadata {
        &ccx.metadata
    }

    pub fn context<'a, 'cx>(ccx: &'a TypedCodemodContext<'cx>) -> &'a Context<'cx> {
        &ccx.cx
    }

    pub fn typed_ast<'a>(ccx: &'a TypedCodemodContext<'_>) -> &'a ast::Program<ALoc, (ALoc, Type)> {
        &ccx.typed_ast
    }

    pub fn lint_severities(ccx: &TypedCodemodContext<'_>) -> LintSettings<Severity> {
        let TypedCodemodContext {
            docblock,
            metadata,
            options,
            file,
            ..
        } = ccx;
        let metadata = flow_typing_context::docblock_overrides(docblock, file, metadata.clone());
        let strict: bool = metadata.overridable.strict;
        let strict_local: bool = metadata.overridable.strict_local;
        if strict || strict_local {
            options.strict_mode.fold(
                options.lint_severities.clone(),
                |lint_kind, mut lint_severities| {
                    lint_severities.set_value(lint_kind, (Severity::Err, None));
                    lint_severities
                },
            )
        } else {
            options.lint_severities.clone()
        }
    }

    pub fn flowfixme_ast(
        lint_severities: &LintSettings<Severity>,
        ccx: &TypedCodemodContext<'_>,
    ) -> ast::types::Type<Loc, Loc> {
        let TypedCodemodContext { options, .. } = ccx;
        let exact_by_default: bool = options.exact_by_default;
        flow_services_code_action::insert_type_utils::builtins::flowfixme_ast(
            exact_by_default,
            lint_severities,
        )
    }
}

pub mod untyped {
    use super::*;

    pub struct UntypedCodemodContext {
        pub file: FileKey,
    }

    pub fn file(ccx: &UntypedCodemodContext) -> &FileKey {
        &ccx.file
    }
}

pub mod untyped_flow_init {
    use super::*;

    pub struct UntypedFlowInitCodemodContext {
        pub file: FileKey,
        pub reader: Arc<SharedMem>,
    }

    pub fn file(ccx: &UntypedFlowInitCodemodContext) -> &FileKey {
        &ccx.file
    }
}
