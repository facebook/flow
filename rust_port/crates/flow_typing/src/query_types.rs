/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_common_ty::ty::ALocElt;
use flow_common_ty::ty::Decl;
use flow_common_ty::ty::DeclModuleDeclData;
use flow_common_ty::ty::Elt;
use flow_common_ty::ty::TypeAtPosResult;
use flow_common_ty::ty::symbols_of_elt;
use flow_common_ty::ty_symbol::Provenance;
use flow_common_ty::ty_symbol::Symbol;
use flow_common_ty::ty_utils;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_debug;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_ty_normalizer::env::EvaluateTypeDestructorsMode;
use flow_typing_ty_normalizer::env::Options;
use flow_typing_ty_normalizer::normalizer::Error;
use flow_typing_type::type_::Type;
use flow_typing_type::type_util;
use flow_typing_utils::convert_types;
use flow_typing_utils::type_env;
use flow_typing_utils::typed_ast_utils;
use serde_json::Value as Json;

use crate::ty_normalizer_flow;
use crate::typed_ast_finder;
use crate::typed_ast_finder::type_at_pos::TypeAtPosResult as FinderResult;

pub enum QueryResult<A> {
    FailureNoMatch,
    FailureUnparseable(Loc, Type, String),
    Success(Loc, A),
}
fn concretize_loc_pairs<T>(pair_list: Vec<(ALoc, T)>) -> Vec<(Loc, T)> {
    pair_list
        .into_iter()
        .map(|(loc, x)| (loc.to_loc_exn().dupe(), x))
        .collect()
}

fn sort_loc_pairs<T>(mut pair_list: Vec<(Loc, T)>) -> Vec<(Loc, T)> {
    pair_list.sort_by(|(a, _), (b, _)| a.cmp(b));
    pair_list
}

fn result_of_normalizer_error<A>(loc: Loc, t: Type, err: Error) -> QueryResult<A> {
    let msg = err.to_string();
    QueryResult::FailureUnparseable(loc, t, msg)
}

const MAX_SIZE_OF_EVALUATED_TYPE: usize = 100;

pub fn dump_type_at_pos(
    cx: &Context<'_>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    loc: Loc,
) -> Option<(Loc, String)> {
    match typed_ast_finder::find_type_at_pos_annotation(cx, typed_ast, loc) {
        FinderResult::NoResult => None,
        FinderResult::HardcodedModuleResult(loc, _) => Some((loc, "ModuleT".to_string())),
        FinderResult::TypeResult(loc, _, t) => {
            Some((loc, flow_typing_debug::dump_t(Some(10), cx, &t)))
        }
    }
}

pub fn type_at_pos_type<'a>(
    cx: &Context<'a>,
    file_sig: Arc<FileSig>,
    omit_targ_defaults: bool,
    verbose_normalizer: bool,
    max_depth: u32,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    no_typed_ast_for_imports: bool,
    include_refs: Option<&dyn Fn(&ALoc) -> Loc>,
    loc: Loc,
) -> QueryResult<TypeAtPosResult> {
    match typed_ast_finder::find_type_at_pos_annotation(cx, typed_ast, loc) {
        FinderResult::NoResult => QueryResult::FailureNoMatch,
        FinderResult::HardcodedModuleResult(loc, name) => {
            let module_symbol = Symbol {
                sym_provenance: Provenance::Local,
                sym_name: Name::new(name),
                sym_anonymous: false,
                sym_def_loc: ALoc::of_loc(loc.dupe()),
            };
            let unevaluated = Elt::Decl(Decl::ModuleDecl(Box::new(DeclModuleDeclData {
                name: Some(module_symbol),
                exports: Arc::from([]),
                default: None,
            })));
            QueryResult::Success(
                loc,
                TypeAtPosResult {
                    unevaluated,
                    evaluated: None,
                    refs: None,
                },
            )
        }
        FinderResult::TypeResult(loc, toplevel_is_type_identifier_reference, t) => {
            let typed_ast_opt = if no_typed_ast_for_imports {
                None
            } else {
                Some(typed_ast)
            };
            let options = |evaluate_type_destructors: EvaluateTypeDestructorsMode| Options {
                expand_internal_types: false,
                expand_enum_members: false,
                evaluate_type_destructors,
                optimize_types: true,
                omit_targ_defaults_option: omit_targ_defaults,
                merge_bot_and_any_kinds: true,
                verbose_normalizer,
                max_depth: Some(max_depth),
                toplevel_is_type_identifier_reference,
            };

            let from_type = |evaluate_type_destructors: EvaluateTypeDestructorsMode| {
                let options = options(evaluate_type_destructors);
                let genv = ty_normalizer_flow::mk_genv(options, cx, typed_ast_opt, file_sig.dupe());
                ty_normalizer_flow::from_type_with_found_computed_type(&genv, &t)
            };
            let (unevaluated, found_computed_type) =
                from_type(EvaluateTypeDestructorsMode::EvaluateNone);
            let evaluated = if found_computed_type {
                // We need to roll back caches and errors, because server state persists
                // through IDE requests. If evaluation results in new errors, future
                // requests at the same location should also result in "new" errors.
                cx.run_and_rolled_back_cache(|| {
                    let errors = cx.errors();
                    let (evaluated, _) = from_type(EvaluateTypeDestructorsMode::EvaluateAll);
                    let errors_prime = cx.errors();
                    cx.reset_errors(errors.dupe());
                    if errors == errors_prime {
                        Some(evaluated)
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            let refs = |unevaluated: &ALocElt,
                        evaluated: &Option<ALocElt>|
             -> Option<BTreeSet<Symbol<Loc>>> {
                match &include_refs {
                    None => None,
                    Some(loc_of_aloc) => {
                        let syms = symbols_of_elt(*loc_of_aloc, unevaluated);
                        Some(match evaluated {
                            None => syms,
                            Some(e) => {
                                let other_syms = symbols_of_elt(*loc_of_aloc, e);
                                &syms | &other_syms
                            }
                        })
                    }
                }
            };
            let tys = match (&unevaluated, &evaluated) {
                (Ok(uneval), Some(Ok(eval))) => {
                    match ty_utils::size_of_elt(Some(MAX_SIZE_OF_EVALUATED_TYPE), eval) {
                        Some(_) => Ok((uneval.clone(), Some(eval.clone()))),
                        None => Ok((uneval.clone(), None)),
                    }
                }
                (Ok(uneval), _) => Ok((uneval.clone(), None)),
                (Err(err), _) => Err(err.clone()),
            };
            match tys {
                Ok((unevaluated, evaluated)) => {
                    let refs = refs(&unevaluated, &evaluated);
                    QueryResult::Success(
                        loc,
                        TypeAtPosResult {
                            unevaluated,
                            evaluated,
                            refs,
                        },
                    )
                }
                Err(err) => result_of_normalizer_error(loc, t, err),
            }
        }
    }
}

pub fn dump_types<'a>(
    printer: &dyn Fn(&ALocElt) -> String,
    evaluate_type_destructors: EvaluateTypeDestructorsMode,
    cx: &Context<'a>,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
) -> Vec<(Loc, String)> {
    let options = Options {
        evaluate_type_destructors,
        ..Options::default()
    };
    let genv = ty_normalizer_flow::mk_genv(options, cx, Some(typed_ast), file_sig);
    let result =
        ty_normalizer_flow::from_types(None, &genv, typed_ast_utils::typed_ast_to_list(typed_ast));
    let print_ok = |(l, r): (ALoc, Result<ALocElt, Error>)| -> Option<(ALoc, String)> {
        match r {
            Ok(t) => Some((l, printer(&t))),
            _ => None,
        }
    };
    let filtered: Vec<_> = result.into_iter().filter_map(print_ok).collect();
    sort_loc_pairs(concretize_loc_pairs(filtered))
}

pub fn dump_types_for_tool(
    cx: &Context<'_>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    depth: i32,
) -> Vec<(Loc, String)> {
    let types = typed_ast_utils::typed_ast_to_list(typed_ast);
    let env = cx.environment();
    let env_values = &env.var_info.env_values;
    let type_to_json = |t: &Type| -> Json {
        let concrete =
            FlowJs::singleton_concrete_type_for_inspection(cx, type_util::reason_of_t(t), t)
                .unwrap_or_else(|_| t.dupe());
        convert_types::type_to_json(cx, depth, &concrete)
    };
    let print_type_json = |(loc, t): (ALoc, Type)| -> (ALoc, String) {
        let expression_type = type_to_json(&t);
        let mut fields: Vec<(String, Json)> =
            vec![("expression_type".to_string(), expression_type)];
        match env_values.get(&loc) {
            Some(read) if let Some(dl) = &read.def_loc => {
                let dl = dl.dupe();
                let provider_json =
                    type_to_json(&type_env::provider_type_for_def_loc(false, cx, &env, dl));
                fields.push(("provider_type".to_string(), provider_json));
            }
            _ => {}
        }
        let obj = Json::Object(fields.into_iter().collect());
        (loc, obj.to_string())
    };
    let mapped: Vec<_> = types.into_iter().map(print_type_json).collect();
    sort_loc_pairs(concretize_loc_pairs(mapped))
}

pub fn insert_type_normalize<'a, 'cx>(
    cx: &'a Context<'cx>,
    file_sig: Arc<FileSig>,
    omit_targ_defaults: bool,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    loc: Loc,
    t: &Type,
) -> QueryResult<ALocElt> {
    let options = Options {
        expand_internal_types: false,
        expand_enum_members: false,
        evaluate_type_destructors: EvaluateTypeDestructorsMode::EvaluateNone,
        optimize_types: false,
        omit_targ_defaults_option: omit_targ_defaults,
        merge_bot_and_any_kinds: true,
        verbose_normalizer: false,
        max_depth: None,
        toplevel_is_type_identifier_reference: false,
    };
    let genv = ty_normalizer_flow::mk_genv(options, cx, Some(typed_ast), file_sig);
    match ty_normalizer_flow::from_type(&genv, t) {
        Ok(elt) => QueryResult::Success(loc, elt),
        Err(err) => result_of_normalizer_error(loc, t.dupe(), err),
    }
}
