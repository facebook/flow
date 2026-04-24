/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_common_ty::ty::ALocElt;
use flow_common_ty::ty::ALocTy;
use flow_common_ty::ty_printer;
use flow_parser::ast;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_js::flow_js;
use flow_typing_ty_normalizer::env::Env;
use flow_typing_ty_normalizer::env::Genv;
use flow_typing_ty_normalizer::env::Options;
use flow_typing_ty_normalizer::normalizer::Error;
use flow_typing_ty_normalizer::normalizer::Normalizer;
use flow_typing_ty_normalizer::normalizer::NormalizerInput;
use flow_typing_ty_normalizer::normalizer::State;
use flow_typing_ty_normalizer::normalizer::lookahead;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeDestructorT;
use flow_typing_type::type_::TypeDestructorTInner;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::eval;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util;
use once_cell::unsync::Lazy;

use crate::ty_normalizer_imports;

struct FlowInput;

impl NormalizerInput for FlowInput {
    fn eval<'cx>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        should_eval: bool,
        cont: impl FnOnce(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<ALocTy, Error>,
        default: impl FnOnce(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<ALocTy, Error>,
        non_eval: impl FnOnce(
            &mut Env<'_, 'cx>,
            &mut State,
            &Type,
            &Destructor,
        ) -> Result<ALocTy, Error>,
        x: (&Type, &TypeDestructorT, &eval::Id),
    ) -> Result<ALocTy, Error> {
        let (t, defer_use_t, id) = x;
        let TypeDestructorTInner(use_op, reason, d) = defer_use_t.deref();
        if should_eval {
            let tout = flow_js::mk_type_destructor(cx, use_op.dupe(), reason, t, d, id.dupe());
            match tout {
                Ok(tout) => match lookahead::peek(cx, &tout) {
                    lookahead::Lookahead::LowerBounds(ref bounds) if bounds.len() == 1 => {
                        cont(env, state, &bounds[0])
                    }
                    _ => default(env, state, &tout),
                },
                Err(_) => default(env, state, t),
            }
        } else {
            non_eval(env, state, t, d)
        }
    }

    fn keys<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        should_evaluate: bool,
        cont: impl FnOnce(&mut Env<'_, 'cx>, &mut State, Type) -> A,
        default: impl FnOnce(&mut Env<'_, 'cx>, &mut State) -> A,
        reason: Reason,
        t: Type,
    ) -> A {
        if should_evaluate {
            let reason_clone = reason.dupe();
            let t_clone = t.dupe();
            let tout = flow_typing_tvar::mk_where(cx, reason.dupe(), move |cx, tout| {
                let use_t = UseT::new(UseTInner::GetKeysT(
                    reason_clone.dupe(),
                    Box::new(UseT::new(UseTInner::UseT(unknown_use(), tout.dupe()))),
                ));
                flow_js::flow_non_speculating(cx, (&t_clone, &use_t))
            });
            match lookahead::peek(cx, &tout) {
                lookahead::Lookahead::LowerBounds(ref bounds) if bounds.len() == 1 => {
                    // We patch the reason here to avoid having a identifier reason description,
                    // which will hide the underlying type.
                    let t = type_util::mod_reason_of_t(
                        &|r| r.replace_desc(VirtualReasonDesc::RKeySet),
                        &bounds[0],
                    );
                    cont(env, state, t)
                }
                _ => default(env, state),
            }
        } else {
            default(env, state)
        }
    }

    fn typeapp<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, &Type) -> A,
        _type_: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, &Type) -> A,
        _app: impl FnOnce(A, Vec<A>) -> A,
        from_value: bool,
        reason: Reason,
        t: Type,
        targs: &[Type],
    ) -> A {
        let t = flow_js::mk_typeapp_instance_annot_non_speculating(
            cx,
            unknown_use(),
            &reason,
            &reason,
            from_value,
            &t,
            Rc::from(targs),
        );
        cont(env, state, &t)
    }

    fn builtin_type<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> A,
        reason: Reason,
        name: &str,
    ) -> A {
        let t = flow_js::get_builtin_type_non_speculating(cx, &reason, None, name);
        cont(env, state, t)
    }

    fn builtin_typeapp<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> A,
        _type_: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> A,
        _app: impl FnOnce(A, Vec<A>) -> A,
        reason: Reason,
        name: &str,
        targs: &[Type],
    ) -> A {
        let t = flow_js_utils::lookup_builtin_type(cx, name, reason.dupe());
        let t = type_util::typeapp(false, false, reason, t, targs.to_vec());
        cont(env, state, t)
    }
}

type FlowNormalizer = Normalizer<FlowInput>;

// Exposed API

fn print_normalizer_banner(genv: &Genv<'_, '_>) {
    if genv.options.verbose_normalizer {
        let banner = "\n======================================== Normalization =======================================\n";
        eprintln!("{}", banner);
    }
}

// The following differ from mapping `from_type` on each input as it folds over
// the input elements of the input propagating the state (caches) after each
// transformation to the next element.
pub fn from_types<A>(
    f: Option<&dyn Fn(&A)>,
    genv: &Genv<'_, '_>,
    ts: Vec<(A, Type)>,
) -> Vec<(A, Result<ALocElt, Error>)> {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    let mut result = Vec::new();
    for (a, t) in ts {
        if let Some(f) = f {
            f(&a);
        }
        let r = FlowNormalizer::run_type(genv, &mut state, &t);
        match r {
            Ok(t) => result.push((a, Ok(t))),
            Err(s) => result.push((a, Err(s))),
        }
    }
    result
}

pub fn from_type_with_found_computed_type(
    genv: &Genv<'_, '_>,
    t: &Type,
) -> (Result<ALocElt, Error>, bool) {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    let result = FlowNormalizer::run_type(genv, &mut state, t);
    (result, state.found_computed_type())
}

pub fn from_type(genv: &Genv<'_, '_>, t: &Type) -> Result<ALocElt, Error> {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    FlowNormalizer::run_type(genv, &mut state, t)
}

pub fn from_module_type(
    genv: &Genv<'_, '_>,
    t: &ModuleType,
) -> Result<flow_common_ty::ty::Decl<ALoc>, Error> {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    FlowNormalizer::run_module_type(genv, &mut state, t)
}

pub fn expand_members(
    force_instance: bool,
    allowed_prop_names: Option<Vec<Name>>,
    genv: &Genv<'_, '_>,
    t: &Type,
) -> Result<ALocTy, Error> {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    FlowNormalizer::run_expand_members(force_instance, allowed_prop_names, genv, &mut state, t)
}

pub fn expand_literal_union(genv: &Genv<'_, '_>, t: &Type) -> Result<ALocTy, Error> {
    print_normalizer_banner(genv);
    let mut state = State::empty();
    FlowNormalizer::run_expand_literal_union(genv, &mut state, t)
}

pub fn mk_genv<'a, 'cx>(
    options: Options,
    cx: &'a Context<'cx>,
    typed_ast_opt: Option<&'a ast::Program<ALoc, (ALoc, Type)>>,
    file_sig: Arc<FileSig>,
) -> Genv<'a, 'cx> {
    let file_sig_for_lazy = file_sig.dupe();
    let options_for_lazy = options.dupe();
    let imported_names = Rc::new(Lazy::new(Box::new(move || {
        let import_types =
            ty_normalizer_imports::extract_types(cx, &file_sig_for_lazy, typed_ast_opt);
        let import_types = import_types
            .into_iter()
            .map(|(name, loc, mode, t)| (name.to_string(), loc, mode, t))
            .collect();
        FlowNormalizer::normalize_imports(
            cx,
            file_sig_for_lazy,
            typed_ast_opt,
            &options_for_lazy,
            import_types,
        )
    }) as Box<dyn FnOnce() -> _>));

    Genv {
        options,
        cx,
        typed_ast_opt,
        file_sig,
        imported_names,
        ref_type_bodies: None,
    }
}

// A debugging facility for getting quick string representations of Type.t
pub fn debug_string_of_t<'cx>(cx: &Context<'cx>, t: &Type) -> String {
    thread_local! {
        static EMPTY_TYPED_AST: &'static ast::Program<ALoc, (ALoc, Type)> =
            Box::leak(Box::new(ast::Program {
                loc: ALoc::default(),
                statements: Arc::from([]),
                interpreter: None,
                comments: None,
                all_comments: Arc::from([]),
            }));
    }
    let typed_ast = EMPTY_TYPED_AST.with(|ast| *ast);
    let file_sig = Arc::new(FileSig::empty());
    let options = Options::default();
    let genv = mk_genv(options, cx, Some(typed_ast), file_sig);
    match from_type(&genv, t) {
        Err(e) => format!("<Error {}>", e.kind),
        Ok(elt) => {
            let opts = ty_printer::PrinterOptions {
                exact_by_default: true,
                ts_syntax: cx.ts_syntax(),
                ..Default::default()
            };
            ty_printer::string_of_elt_single_line(&elt, &opts)
        }
    }
}
