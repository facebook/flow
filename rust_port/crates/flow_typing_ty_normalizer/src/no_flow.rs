/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! No-Flow normalizer implementation.
//!
//! Port of `ty_normalizer_no_flow.ml`
//!
//! This module provides a normalizer implementation that doesn't require
//! Flow type inference context, suitable for simpler use cases.

use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Reason;
use flow_common_ty::ty::ALocElt;
use flow_common_ty::ty::ALocTy;
use flow_lazy::Lazy;
use flow_parser::ast;
use flow_parser_utils::file_sig::FileSig;
use flow_typing_context::Context;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::eval;

use crate::env::Env;
use crate::env::Genv;
use crate::env::Options;
use crate::normalizer::Error;
use crate::normalizer::Normalizer;
use crate::normalizer::NormalizerInput;
use crate::normalizer::State;

pub struct NoFlowInput;

impl NormalizerInput for NoFlowInput {
    fn eval<'cx>(
        _cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        _should_eval: bool,
        _cont: impl FnOnce(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<ALocTy, Error>,
        _default: impl FnOnce(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<ALocTy, Error>,
        non_eval: impl FnOnce(
            &mut Env<'_, 'cx>,
            &mut State,
            &Type,
            &Destructor,
        ) -> Result<ALocTy, Error>,
        x: (&Type, &flow_typing_type::type_::TypeDestructorT, &eval::Id),
    ) -> Result<ALocTy, Error> {
        let (t, defer_use_t, _id) = x;
        let flow_typing_type::type_::TypeDestructorTInner(_, _, d) = defer_use_t.deref();
        non_eval(env, state, t, d)
    }

    fn keys<'cx, A>(
        _cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        _should_evaluate: bool,
        _cont: impl FnOnce(&mut Env<'_, 'cx>, &mut State, Type) -> Result<A, Error>,
        default: impl FnOnce(&mut Env<'_, 'cx>, &mut State) -> Result<A, Error>,
        _reason: Reason,
        _t: Type,
    ) -> Result<A, Error> {
        default(env, state)
    }

    fn typeapp<'cx, A>(
        _cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        _cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<A, Error>,
        type_: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, &Type) -> Result<A, Error>,
        app: impl FnOnce(Result<A, Error>, Vec<Result<A, Error>>) -> Result<A, Error>,
        _from_value: bool,
        _reason: Reason,
        t: Type,
        targs: &[Type],
    ) -> Result<A, Error> {
        let c = type_(env, state, &t);
        let targs = targs.iter().map(|t| type_(env, state, t)).collect();
        app(c, targs)
    }

    fn builtin_type<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> Result<A, Error>,
        reason: Reason,
        name: &str,
    ) -> Result<A, Error> {
        use flow_typing_type::type_::DefTInner;
        use flow_typing_type::type_::TypeInner;
        let t = flow_typing_flow_common::flow_js_utils::lookup_builtin_type(cx, name, reason);
        let t = match t.deref() {
            TypeInner::DefT(_, def_t) => match def_t.deref() {
                DefTInner::TypeT(_, inner_t) => inner_t.dupe(),
                _ => t,
            },
            _ => t,
        };
        cont(env, state, t)
    }

    fn builtin_typeapp<'cx, A>(
        cx: &Context<'cx>,
        env: &mut Env<'_, 'cx>,
        state: &mut State,
        cont: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> Result<A, Error>,
        _type_: &mut dyn FnMut(&mut Env<'_, 'cx>, &mut State, Type) -> Result<A, Error>,
        _app: impl FnOnce(Result<A, Error>, Vec<Result<A, Error>>) -> Result<A, Error>,
        reason: Reason,
        name: &str,
        targs: &[Type],
    ) -> Result<A, Error> {
        let t =
            flow_typing_flow_common::flow_js_utils::lookup_builtin_type(cx, name, reason.dupe());
        let t = flow_typing_type::type_util::typeapp(false, false, reason, t, targs.to_vec());
        cont(env, state, t)
    }
}

pub type NoFlowNormalizer = Normalizer<NoFlowInput>;

pub fn from_type(genv: &Genv<'_, '_>, t: &Type) -> Result<ALocElt, Error> {
    let mut state = State::empty();
    NoFlowNormalizer::run_type(genv, &mut state, t)
}

pub fn mk_genv<'a, 'cx: 'a>(
    options: Options,
    cx: &'a Context<'cx>,
    typed_ast_opt: Option<&'a ast::Program<ALoc, (ALoc, Type)>>,
    file_sig: Arc<FileSig>,
) -> Genv<'a, 'cx> {
    // Computing proper import information would introduce cycles making this module
    // unusable in any code depending on Flow_js.
    let dummy_import_list = vec![];
    let file_sig_clone = file_sig.clone();
    let options_clone = options.dupe();
    let imported_names = Rc::new(Lazy::new(Box::new(move |cx: &Context<'cx>| {
        Ok(NoFlowNormalizer::normalize_imports(
            cx,
            file_sig_clone,
            typed_ast_opt,
            &options_clone,
            dummy_import_list,
        ))
    })
        as Box<dyn FnOnce(&Context<'cx>) -> _ + 'a>));
    Genv {
        options,
        cx,
        typed_ast_opt,
        file_sig,
        imported_names,
        ref_type_bodies: None,
    }
}

pub fn mk_default_genv<'a, 'cx: 'a>(
    options: Option<Options>,
    cx: &'a Context<'cx>,
) -> Genv<'a, 'cx> {
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
    let typed_ast_opt = Some(EMPTY_TYPED_AST.with(|ast| *ast));

    let file_sig = Arc::new(FileSig::empty());
    mk_genv(options.unwrap_or_default(), cx, typed_ast_opt, file_sig)
}

pub fn debug_string_of_t<'a, 'cx: 'a>(cx: &'a Context<'cx>, t: &Type) -> String {
    let genv = mk_default_genv(None, cx);
    match from_type(&genv, t) {
        Err(e) => format!("<Error {}>", e.kind),
        Ok(elt) => {
            let opts = flow_common_ty::ty_printer::PrinterOptions {
                exact_by_default: true,
                ts_syntax: cx.ts_syntax(),
                ..Default::default()
            };
            flow_common_ty::ty_printer::string_of_elt_single_line(&elt, &opts)
        }
    }
}

pub fn type_to_desc_for_errors(
    genv: &Genv<'_, '_>,
    t: &Type,
) -> Result<ALocTy, flow_common::reason::ReasonDesc> {
    use flow_common::reason::VirtualReasonDesc;
    let desc = flow_typing_type::type_util::desc_of_t(t);
    match desc {
        VirtualReasonDesc::RArrayLit
        | VirtualReasonDesc::RArrayLitUnsound
        | VirtualReasonDesc::RObjectLit
        | VirtualReasonDesc::RObjectLitUnsound => Err(desc.clone()),
        // Why do we try to use the printed types? For these non-leaf-node types, it's
        // reference locations can be spread across multiple locations, but the reason infra
        // can only point code reference to one place, instead of multiple places that
        // contribute to the composition of a type.
        //
        // e.g.
        //```js
        // declare function f<T>(x: T): Array<T>;
        // //                           ^^^^^^^^
        // const x = f(1);
        // //          ^
        // ```
        //
        // To fully describe the type of `x`, we need to point to multiple locations, which
        // is not well supported by the reason infra. So we use the printed types instead.
        // The location might still be non-ideal, but at least the description makes sense.
        _ => match from_type(genv, t) {
            Err(_) => Err(desc.clone()),
            Ok(elt) => match flow_common_ty::ty_utils::typify_elt(elt) {
                None => Err(desc.clone()),
                Some(t) => Ok(t),
            },
        },
    }
}
