/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! env_resolution - corresponds to OCaml's Env_resolution module in typing
//!
//! Resolves environment entries during type checking. Handles hint resolution,
//! predicate function resolution, and component resolution in the name definition
//! ordering graph.

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::flow_import_specifier;
use flow_common::reason;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_env_builder::env_api;
use flow_env_builder::env_api::EnvMap;
use flow_env_builder::name_def_types;
use flow_env_builder::name_def_types::Binding;
use flow_env_builder::name_def_types::Def;
use flow_env_builder::name_def_types::Import;
use flow_env_builder::name_def_types::Root;
use flow_env_builder::name_def_types::ScopeKind;
use flow_env_builder::selector;
use flow_env_builder_resolver::name_def_ordering;
use flow_parser::ast;
use flow_parser::ast_utils;
use flow_parser::loc_sig::LocSig;
use flow_typing_context::Context;
use flow_typing_context::TypingMode;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::intermediate_error_types::ExpectedModulePurpose;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::tvar_resolver;
use flow_typing_loc_env::match_pattern_ir;
use flow_typing_statement::statement;
use flow_typing_statement::type_annotation;
use flow_typing_type::type_;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_util;
use flow_typing_utils::abnormal::AbnormalControlFlow;
use flow_typing_utils::exhaustive;
use flow_typing_utils::type_env;
use flow_typing_utils::type_hint;
use flow_typing_utils::type_operation_utils;
use once_cell::unsync::Lazy;

fn mk_tparams_map(
    cx: &Context,
    tparams_map: &FlowOrdMap<ALoc, FlowSmolStr>,
) -> FlowOrdMap<SubstName, Type> {
    let env = cx.environment();
    let mut subst_map = FlowOrdMap::new();
    for (l, _) in tparams_map.iter() {
        if let Some((name, _, ty)) = env.tparams.get_ordinary(l) {
            subst_map.insert(name.dupe(), ty.dupe());
        }
    }
    subst_map
}

fn try_cache_result<T>(
    cx: &Context,
    target_loc: Option<ALoc>,
    check: impl FnOnce() -> Result<T, AbnormalControlFlow>,
    cache: impl FnOnce(&T),
) -> Result<T, AbnormalControlFlow> {
    if !matches!(
        &*cx.typing_mode(),
        flow_typing_context::TypingMode::CheckingMode
    ) {
        let original_errors = cx.errors();
        cx.reset_errors(ErrorSet::new());
        let (produced_uncacheable_result, result) =
            cx.run_in_synthesis_mode(false, target_loc, check);
        let can_cache = !produced_uncacheable_result && cx.errors().is_empty();
        cx.reset_errors(original_errors);
        let e = result?;
        if can_cache {
            cache(&e);
        }
        Ok(e)
    } else {
        let e = check()?;
        cache(&e);
        Ok(e)
    }
}

// Helper to incrementally build PatternUnion state from a previous pattern location.
// Returns the pattern_union_state to use for the current pattern.
fn get_pattern_union_state_from_prev(
    cx: &Context,
    prev_pattern_loc: Option<ALoc>,
) -> (match_pattern_ir::pattern_union::PatternUnion, usize) {
    match prev_pattern_loc {
        // First pattern: start with empty PatternUnion
        None => (match_pattern_ir::pattern_union::empty(), 0),
        Some(prev_loc) => {
            let node_cache = cx.node_cache();
            match node_cache.get_match_pattern_union(&prev_loc) {
                Some(prev_pattern_union_state) => {
                    let prev_pattern = node_cache
                        .get_match_pattern(&prev_loc)
                        .expect("match pattern cache missing");
                    exhaustive::pattern_union_builder::add_pattern(
                        cx,
                        false,
                        (
                            prev_pattern_union_state.0,
                            prev_pattern_union_state.1 as usize,
                        ),
                        (&prev_pattern.0, prev_pattern.1),
                        false,
                    )
                }
                None => panic!("ERROR: pattern_union cache missing - dependency ordering issue"),
            }
        }
    }
}

fn expression(
    cx: &Context,
    encl_ctx: Option<EnclosingContext>,
    decl: Option<ast::VariableKind>,
    as_const: Option<bool>,
    exp: &ast::expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let cache = cx.node_cache();
    let target_loc = match &*cx.typing_mode() {
        TypingMode::SynthesisMode { target_loc } => target_loc.clone(),
        _ => None,
    };
    let exp_clone = exp.dupe();
    let cx_clone = cx.dupe();
    // OCaml: let ((_, t), _) = try_cache cx ~target_loc
    //   ~check:(fun () -> Statement.expression ?encl_ctx ?decl ?as_const cx exp)
    //   ~cache:(Node_cache.set_expression cache) in t
    let result = try_cache_result(
        cx,
        target_loc,
        move || statement::expression(encl_ctx, decl, as_const, &cx_clone, &exp_clone),
        |e| {
            cache.set_expression(e.dupe());
        },
    )?;
    let (_loc, t) = result.loc().dupe();
    Ok(t)
}

fn make_hooklike(cx: &Context, t: Type) -> Type {
    if cx.hook_compatibility() {
        match t.deref() {
            TypeInner::DefT(_, def_t) if matches!(def_t.deref(), type_::DefTInner::TypeT(..)) => t,
            _ => {
                let reason = type_util::reason_of_t(&t).dupe();
                let cx_inner = cx.dupe();
                let reason_inner = reason.dupe();
                flow_js_utils::map_on_resolved_type(cx, reason.dupe(), t, move |t| {
                    let cx_ref = &cx_inner;
                    let reason_for_tvar = reason_inner.dupe();
                    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                        cx_ref,
                        reason_inner,
                        |_tvar_reason, tvar_id| {
                            let tvar = Tvar::new(reason_for_tvar.dupe(), tvar_id as u32);
                            let use_t = UseT::new(type_::UseTInner::HooklikeT(Box::new(tvar)));
                            flow_js::flow_non_speculating(cx_ref, (&t, &use_t));
                        },
                    )
                })
            }
        }
    } else {
        t
    }
}

fn resolve_annotation(
    cx: &Context,
    tparams_map: &FlowOrdMap<ALoc, FlowSmolStr>,
    react_deep_read_only: Option<(ALoc, name_def_types::DroAnnot)>,
    anno: &ast::types::Annotation<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let tparams_map = mk_tparams_map(cx, tparams_map);
    let (t, anno_typed) = type_annotation::mk_type_available_annotation(cx, tparams_map, anno);
    let t = match react_deep_read_only {
        Some((param_loc, kind)) => {
            let enabled = match kind {
                name_def_types::DroAnnot::Hook => {
                    cx.react_rule_enabled(flow_common::options::ReactRule::DeepReadOnlyProps)
                }
                name_def_types::DroAnnot::Comp => {
                    cx.react_rule_enabled(flow_common::options::ReactRule::DeepReadOnlyProps)
                }
            };
            if enabled {
                let reason = type_util::reason_of_t(&t).dupe();
                let dro_type = match kind {
                    name_def_types::DroAnnot::Hook => type_::DroType::HookArg,
                    name_def_types::DroAnnot::Comp => type_::DroType::Props,
                };
                let react_dro = type_::ReactDro(param_loc, dro_type);
                let destructor = Rc::new(type_::Destructor::ReactDRO(react_dro));
                let eval_id = type_::eval::Id::generate_id();
                flow_js::FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                    cx,
                    type_::unknown_use(),
                    &reason,
                    &t,
                    &destructor,
                    eval_id,
                )
                .expect("non speculative")
            } else {
                t
            }
        }
        _ => t,
    };
    if matches!(
        &*cx.typing_mode(),
        flow_typing_context::TypingMode::CheckingMode
    ) {
        cache.set_annotation(anno_typed);
    }
    t
}

fn synthesizable_expression(
    cx: &Context,
    encl_ctx: EnclosingContext,
    exp: &ast::expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    use ast::expression::ExpressionInner;
    Ok(match exp.deref() {
        ExpressionInner::Identifier { loc, inner } => {
            statement::identifier(encl_ctx, cx, inner, loc.dupe())
        }
        ExpressionInner::StringLiteral { loc, inner } => {
            statement::string_literal(cx, encl_ctx, loc.dupe(), inner)
        }
        ExpressionInner::BooleanLiteral { loc, inner } => {
            statement::boolean_literal(cx, encl_ctx, loc.dupe(), inner)
        }
        ExpressionInner::NullLiteral { loc, .. } => statement::null_literal(loc.dupe()),
        ExpressionInner::NumberLiteral { loc, inner } => {
            statement::number_literal(cx, encl_ctx, loc.dupe(), inner)
        }
        ExpressionInner::BigIntLiteral { loc, inner } => {
            statement::bigint_literal(cx, encl_ctx, loc.dupe(), inner)
        }
        ExpressionInner::RegExpLiteral { loc, .. } => statement::regexp_literal(cx, loc.dupe()),
        ExpressionInner::ModuleRefLiteral { loc, inner } => {
            let (t, _lit) = statement::module_ref_literal(cx, loc.dupe(), inner);
            t
        }
        ExpressionInner::AsExpression { inner, .. } => {
            resolve_annotation(cx, &FlowOrdMap::default(), None, &inner.annot)
        }
        ExpressionInner::TypeCast { inner, .. } => {
            resolve_annotation(cx, &FlowOrdMap::default(), None, &inner.annot)
        }
        ExpressionInner::Member { loc, inner }
            if let ast::expression::member::Property::PropertyIdentifier(id) = &inner.property =>
        {
            let ploc = &id.loc;
            let name = &id.name;
            let t = synthesizable_expression(cx, encl_ctx, &inner.object)?;
            match flow_typing_statement::refinement::get(false, cx, exp, loc.dupe()) {
                Some(t) => t,
                None => {
                    let expr_reason = reason::mk_expression_reason(exp);
                    let prop_reason = reason::mk_reason(
                        reason::VirtualReasonDesc::RProperty(Some(reason::Name::new(name.dupe()))),
                        ploc.dupe(),
                    );
                    // TODO(jmbrown) This feels incorrect
                    let use_op = type_::VirtualUseOp::Op(Arc::new(
                        type_::VirtualRootUseOp::GetProperty(expr_reason.dupe()),
                    ));
                    statement::get_prop(
                        EnclosingContext::NoContext,
                        cx,
                        expr_reason,
                        use_op,
                        type_env::get_hint(cx, loc.dupe()),
                        t,
                        prop_reason,
                        name,
                    )
                }
            }
        }
        _ => return expression(cx, Some(encl_ctx), None, None, exp),
    })
}

fn mk_selector_reason_has_default(
    cx: &Context,
    loc: &ALoc,
    selector: &selector::Selector<ALoc, ALoc>,
) -> Result<(type_::Selector, Reason, bool), AbnormalControlFlow> {
    use flow_env_builder::selector::Selector;
    Ok(match selector {
        Selector::Elem {
            index: n,
            has_default,
        } => {
            let key = Type::new(TypeInner::DefT(
                reason::mk_reason(reason::VirtualReasonDesc::RNumber, loc.dupe()),
                DefT::new(DefTInner::SingletonNumT {
                    from_annot: false,
                    value: type_::NumberLiteral(*n as f64, FlowSmolStr::from(n.to_string())),
                }),
            ));
            (
                type_::Selector::Elem(key),
                reason::mk_reason(
                    reason::VirtualReasonDesc::RArrayNthElement(*n as i32),
                    loc.dupe(),
                ),
                *has_default,
            )
        }
        Selector::Prop {
            prop,
            prop_loc,
            has_default,
        } => (
            type_::Selector::Prop(prop.dupe(), *has_default),
            reason::mk_reason(
                reason::VirtualReasonDesc::RProperty(Some(reason::Name::new(prop.dupe()))),
                prop_loc.dupe(),
            ),
            *has_default,
        ),
        Selector::ArrRest(n) => (
            type_::Selector::ArrRest(*n as i32),
            reason::mk_reason(reason::VirtualReasonDesc::RArrayPatternRestProp, loc.dupe()),
            false,
        ),
        Selector::ObjRest {
            used_props,
            after_computed: _,
        } => {
            // TODO: eveyrthing after a computed prop should be optional
            (
                type_::Selector::ObjRest(used_props.to_vec().into()),
                reason::mk_reason(
                    reason::VirtualReasonDesc::RObjectPatternRestProp,
                    loc.dupe(),
                ),
                false,
            )
        }
        Selector::Computed {
            expression: exp,
            has_default,
        } => {
            let t = expression(cx, Some(EnclosingContext::IndexContext), None, None, exp)?;
            (
                type_::Selector::Elem(t),
                reason::mk_reason(reason::VirtualReasonDesc::RProperty(None), loc.dupe()),
                *has_default,
            )
        }
        Selector::Default => (
            type_::Selector::Default,
            reason::mk_reason(reason::VirtualReasonDesc::RDefaultValue, loc.dupe()),
            false,
        ),
    })
}

fn synthesize_expression_for_instantiation(
    cx: &Context,
    target_loc: Option<ALoc>,
    e: &ast::expression::Expression<ALoc, ALoc>,
) -> ast::expression::Expression<ALoc, (ALoc, Type)> {
    let e_clone = e.dupe();
    let cx_clone_1 = cx.dupe();
    let cx_clone_2 = cx.dupe();
    try_cache_result(
        cx,
        target_loc,
        move || statement::expression(None, None, None, &cx_clone_1, &e_clone),
        |e| {
            let cache = cx_clone_2.node_cache();
            cache.set_expression(e.dupe());
        },
    )
    .expect("unexpected abnormal control flow in synthesize_expression_for_instantiation")
}

fn synthesize_jsx_children_for_instantiation(
    cx: &Context,
    children: &(ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
) -> Result<
    (
        Vec<type_::UnresolvedParam>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    ),
    AbnormalControlFlow,
> {
    let cache = cx.node_cache();
    let children_clone = children.clone();
    let cx_clone = cx.dupe();
    try_cache_result(
        cx,
        None,
        move || statement::collapse_children(&cx_clone, &children_clone),
        |result| {
            cache.set_jsx_children(result.clone());
        },
    )
}

fn synth_arg_list(
    cx: &Context,
    target_loc: Option<ALoc>,
    arg_list: &ast::expression::ArgList<ALoc, ALoc>,
) -> Vec<(ALoc, type_::CallArg)> {
    let arguments = &arg_list.arguments;
    arguments
        .iter()
        .map(|arg| {
            use ast::expression::ExpressionOrSpread;
            match arg {
                ExpressionOrSpread::Expression(e) => {
                    let result = synthesize_expression_for_instantiation(cx, target_loc.dupe(), e);
                    let (loc, t) = result.loc().dupe();
                    (loc, type_::CallArg::arg(t))
                }
                ExpressionOrSpread::Spread(spread) => {
                    let e = &spread.argument;
                    let result = synthesize_expression_for_instantiation(cx, target_loc.dupe(), e);
                    let (loc, t) = result.loc().dupe();
                    (loc, type_::CallArg::spread_arg(t))
                }
            }
        })
        .collect()
}

fn resolve_hint(
    cx: &Context,
    loc: ALoc,
    hint: flow_env_builder::name_def_types::AstHint,
) -> type_hint::ConcrHint {
    use flow_env_builder::name_def_types::ArrayElementPatternHint;
    use flow_env_builder::name_def_types::HintNode;
    use flow_env_builder::name_def_types::ObjectPropPatternHint;
    use flow_typing_type::type_::TypeInner;

    fn resolve_hint_node(cx: &Context, loc: ALoc, hint_node: HintNode) -> Type {
        match hint_node {
            HintNode::AnnotationHint(tparams_map, anno) => {
                resolve_annotation(cx, &tparams_map, None, &anno)
            }
            HintNode::ValueHint(encl_ctx, exp) => expression(cx, Some(encl_ctx), None, None, &exp)
                .expect("unexpected abnormal control flow in resolve_hint_node"),
            HintNode::ProvidersHint(locs) => {
                if locs.len() == 1 {
                    let loc = locs.first().dupe();
                    type_env::checked_find_loc_env_write(
                        cx,
                        flow_env_builder::env_api::DefLocType::OrdinaryNameLoc,
                        loc,
                    )
                } else {
                    let mut iter = locs.iter();
                    let l1 = iter.next().unwrap().dupe();
                    let l2 = iter.next().unwrap().dupe();
                    let rest: Vec<ALoc> = iter.duped().collect();
                    let t1 = type_env::checked_find_loc_env_write(
                        cx,
                        flow_env_builder::env_api::DefLocType::OrdinaryNameLoc,
                        l1.dupe(),
                    );
                    let t2 = type_env::checked_find_loc_env_write(
                        cx,
                        flow_env_builder::env_api::DefLocType::OrdinaryNameLoc,
                        l2,
                    );
                    let ts: Vec<Type> = rest
                        .into_iter()
                        .map(|loc| {
                            type_env::checked_find_loc_env_write(
                                cx,
                                flow_env_builder::env_api::DefLocType::OrdinaryNameLoc,
                                loc,
                            )
                        })
                        .collect();
                    Type::new(TypeInner::UnionT(
                        reason::mk_reason(reason::VirtualReasonDesc::RProviders, l1),
                        type_::union_rep::make(
                            None,
                            type_::union_rep::UnionKind::ProvidersKind,
                            t1,
                            t2,
                            ts.into(),
                        ),
                    ))
                }
            }
            HintNode::WriteLocHint(kind, hint_loc) => {
                type_env::checked_find_loc_env_write(cx, kind, hint_loc)
            }
            HintNode::StringLiteralType(name) => Type::new(TypeInner::DefT(
                reason::mk_reason(
                    reason::VirtualReasonDesc::RIdentifier(reason::Name::new(name.dupe())),
                    loc.dupe(),
                ),
                DefT::new(DefTInner::SingletonStrT {
                    from_annot: true,
                    value: reason::Name::new(name),
                }),
            )),
            HintNode::ReactFragmentType => {
                flow_js_utils::import_export_utils::get_implicitly_imported_react_type(
                    cx,
                    loc.dupe(),
                    &|cx, reason, t| {
                        flow_js::FlowJs::singleton_concretize_type_for_imports_exports(
                            cx, &reason, &t,
                        )
                        .map_err(FlowJsException::Speculative)
                    },
                    ExpectedModulePurpose::ReactModuleForJSXFragment,
                )
                .expect("get_implicitly_imported_react_type failed outside speculation")
            }
            HintNode::ReactNodeType => {
                let react_node_reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RType(reason::Name::new(FlowSmolStr::new(
                        "React.Node",
                    ))),
                    loc.dupe(),
                );
                flow_js::get_builtin_react_type_non_speculating(
                    cx,
                    &react_node_reason,
                    None,
                    ExpectedModulePurpose::ReactModuleForReactNodeType,
                )
            }
            HintNode::AnyErrorHint(reason) => type_::any_t::error(reason),
            HintNode::ComposedArrayPatternHint(hint_loc, elements) => {
                let array_reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RDestructuring, hint_loc.dupe());
                let is_empty = elements.is_empty();
                let elem_spread_list: Vec<type_::UnresolvedParam> = elements
                    .into_iter()
                    .map(|el| match el {
                        ArrayElementPatternHint::ArrayElementPatternHint(h) => {
                            let t = resolve_hint_node(cx, loc.dupe(), h);
                            let elem_reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RArrayElement,
                                hint_loc.dupe(),
                            );
                            type_::UnresolvedParam::UnresolvedArg(
                                type_util::mk_tuple_element(
                                    elem_reason,
                                    t,
                                    None,
                                    false,
                                    flow_common::polarity::Polarity::Neutral,
                                ),
                                None,
                            )
                        }
                        ArrayElementPatternHint::ArrayRestElementPatternHint(h) => {
                            type_::UnresolvedParam::UnresolvedSpreadArg(resolve_hint_node(
                                cx,
                                loc.dupe(),
                                h,
                            ))
                        }
                    })
                    .collect();
                flow_typing_tvar::mk_where(cx, array_reason.dupe(), |tout| {
                    let reason_op = array_reason.dupe();
                    let element_reason = reason_op.dupe().replace_desc(
                        reason::VirtualReasonDesc::RInferredUnionElemArray {
                            instantiable: false,
                            is_empty,
                        },
                    );
                    let elem_t = flow_typing_tvar::mk(cx, element_reason);
                    flow_js::FlowJs::resolve_spread_list(
                        cx,
                        type_::unknown_use(),
                        &reason_op,
                        elem_spread_list,
                        type_::SpreadResolve::ResolveSpreadsToArrayLiteral {
                            id: reason::mk_id() as i32,
                            as_const: false,
                            elem_t,
                            tout: tout.dupe(),
                        },
                    )
                    .expect("Non speculating");
                })
            }
            HintNode::ComposedObjectPatternHint(hint_loc, properties) => {
                let acc = properties.into_iter().fold(
                    statement::object_expression_acc::ObjectExpressionAcc::empty(),
                    |acc, prop| match prop {
                        ObjectPropPatternHint::ObjectPropPatternHint(n, l, h) => {
                            let t = resolve_hint_node(cx, loc.dupe(), h);
                            acc.add_prop(move |mut props_map| {
                                props_map.add_field(
                                    reason::Name::new(n),
                                    flow_common::polarity::Polarity::Neutral,
                                    None,
                                    Some(l),
                                    t,
                                );
                                props_map
                            })
                        }
                        ObjectPropPatternHint::ObjectSpreadPropPatternHint(h) => {
                            let t = resolve_hint_node(cx, loc.dupe(), h);
                            acc.add_spread(t)
                        }
                    },
                );
                let obj_reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RDestructuring, hint_loc.dupe());
                let default_proto = Type::new(TypeInner::ObjProtoT(obj_reason.dupe()));
                acc.mk_object_from_spread_acc(cx, obj_reason, false, false, default_proto)
            }
        }
    }

    let cx_clone = cx.dupe();
    let loc_clone = loc.dupe();
    let map_base_hint = Rc::new(move |h| resolve_hint_node(&cx_clone, loc_clone.dupe(), h));

    let cx_clone2 = cx.dupe();
    let map_targs = Rc::new(
        move |targs: Option<ast::expression::CallTypeArgs<ALoc, ALoc>>| {
            statement::convert_call_targs_opt_prime(&cx_clone2, targs.as_ref())
        },
    );

    let cx_clone3 = cx.dupe();
    let map_arg_list = Rc::new(
        move |arg_list: ast::expression::ArgList<ALoc, ALoc>| -> type_hint::ConcrArgListFn {
            let l = arg_list.loc.dupe();
            let cx = cx_clone3.dupe();
            let arg_list = std::sync::Arc::new(arg_list);
            Rc::new(move |target_loc: Option<ALoc>| {
                if let Some(result) = cx
                    .hint_map_arglist_cache()
                    .get(&(l.dupe(), target_loc.dupe()))
                    .cloned()
                {
                    result
                } else {
                    let result = synth_arg_list(&cx, target_loc.dupe(), &arg_list);
                    cx.hint_map_arglist_cache_mut()
                        .insert((l.dupe(), target_loc), result.clone());
                    result
                }
            })
        },
    );

    let cx_clone4 = cx.dupe();
    type PropsAndChildren = (
        std::sync::Arc<[ast::jsx::OpeningAttribute<ALoc, ALoc>]>,
        (ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
    );
    let map_jsx = Rc::new(
        move |jsx_reason: Reason, name: FlowSmolStr, (props, children): PropsAndChildren| {
            let key_locs: Vec<ALoc> = props
                .iter()
                .map(|attr| match attr {
                    ast::jsx::OpeningAttribute::Attribute(attr) => attr.loc.dupe(),
                    ast::jsx::OpeningAttribute::SpreadAttribute(spread) => spread.loc.dupe(),
                })
                .collect();
            let children_loc = children.0.dupe();
            let cache_key = (jsx_reason.dupe(), name.to_string(), key_locs, children_loc);

            if let Some(result) = cx_clone4.hint_map_jsx_cache().get(&cache_key).duped() {
                result
            } else {
                let original_errors = cx_clone4.errors();
                cx_clone4.reset_errors(ErrorSet::new());
                let cx_lazy = cx_clone4.dupe();
                let props_clone = props.dupe();
                let children_clone = children.clone();
                let jsx_reason_clone = jsx_reason.dupe();
                let name_clone = name.dupe();
                let lazy_props: Rc<LazyCell<Type, Box<dyn Fn() -> Type>>> =
                    Rc::new(LazyCell::new(Box::new(move || {
                        let (jsx_props_t, _, _, _) = statement::jsx_mk_props(
                            &cx_lazy,
                            jsx_reason_clone.dupe(),
                            &|_hint, cx, expr| {
                                Ok(synthesize_expression_for_instantiation(cx, None, expr))
                            },
                            &|cx, children| synthesize_jsx_children_for_instantiation(cx, children),
                            &name_clone,
                            &props_clone,
                            &children_clone,
                        )
                        .expect("Non speculating");
                        jsx_props_t
                    })));
                cx_clone4.reset_errors(original_errors);
                cx_clone4
                    .hint_map_jsx_cache_mut()
                    .insert(cache_key, lazy_props.dupe());
                lazy_props
            }
        },
    );

    hint.map(&map_base_hint, &map_targs, &map_arg_list, &map_jsx)
}

fn resolve_hints(
    cx: &Context,
    loc: ALoc,
    hints: &[flow_env_builder::name_def_types::AstHint],
) -> Vec<type_hint::ConcrHint> {
    hints
        .iter()
        .map(|h| resolve_hint(cx, loc.dupe(), h.clone()))
        .collect()
}

pub fn lazily_resolve_hints(
    cx: &Context,
    loc: ALoc,
    hints: &[flow_env_builder::name_def_types::AstHint],
) -> type_::LazyHintT {
    let has_hint = !hints.is_empty();
    let hints_owned: Vec<flow_env_builder::name_def_types::AstHint> = hints.to_vec();
    let cx = cx.dupe();
    let lazy_hint = move |expected_only: bool,
                          skip_optional: Option<bool>,
                          r: Reason|
          -> type_::HintEvalResult {
        let resolved = resolve_hints(&cx, loc.dupe(), &hints_owned);
        type_hint::evaluate_hints(&cx, expected_only, skip_optional, &r, resolved)
    };
    type_::LazyHintT(has_hint, Rc::new(lazy_hint))
}

pub fn resolve_pred_func(
    cx: &Context,
    info: &env_api::PredFuncInfo<ALoc>,
) -> Rc<Lazy<type_::PredFuncallInfo, Box<dyn FnOnce() -> type_::PredFuncallInfo>>> {
    let cx = cx.dupe();
    let info = info.clone();
    Rc::new(Lazy::new(Box::new(move || {
        let loc = info.callee.loc();
        let use_op = type_::VirtualUseOp::Op(Arc::new(type_::VirtualRootUseOp::FunCall {
            op: reason::mk_expression_reason(&info.call_expr),
            fn_: reason::mk_expression_reason(&info.callee),
            args: reason::mk_initial_arguments_reason(&info.arguments).into(),
            local: true,
        }));
        // [callee] might be a member access expression. Since we are explicitly unbinding it from
        // the call, make sure we don't raise a method-unbinding error.
        let callee_t = type_env::with_class_stack(&cx, info.class_stack.dupe(), || {
            // Synthesis mode to avoid caching
            let (_, (t, new_errors)) = cx.run_in_synthesis_mode_with_errors(None, || {
                expression(&cx, None, None, None, &info.callee)
            });
            if new_errors.is_lint_only_errorset() {
                match t {
                    Ok(t) => t,
                    Err(_) => any_t::at(AnySource::AnyError(None), loc.dupe()),
                }
            } else {
                any_t::at(AnySource::AnyError(None), loc.dupe())
            }
        });
        let targs = statement::convert_call_targs_opt_prime(&cx, info.targs.as_ref());
        let arguments = &info.arguments.arguments;
        let argts: Rc<[type_::CallArg]> = arguments
            .iter()
            .map(|e| {
                let (t, _) = statement::expression_or_spread(&cx, e).expect("Non speculating");
                t
            })
            .collect();
        let ex_loc = info.call_expr.loc();
        type_::PredFuncallInfo(use_op, ex_loc.dupe(), callee_t, targs.map(Rc::from), argts)
    })))
}

fn resolve_annotated_function(
    cx: &Context,
    scope_kind: ScopeKind,
    bind_this: bool,
    statics: &BTreeMap<FlowSmolStr, env_api::EnvKey<ALoc>>,
    hook_like: bool,
    reason: Reason,
    tparams_map: &FlowOrdMap<ALoc, FlowSmolStr>,
    function_loc: ALoc,
    function_: &ast::function::Function<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let sig_loc = function_.sig_loc.dupe();
    let effect_ = &function_.effect_;
    if (scope_kind == ScopeKind::ComponentOrHookBody
        || scope_kind == ScopeKind::AsyncComponentOrHookBody)
        && *effect_ == ast::function::Effect::Hook
    {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::ENestedHook(reason.dupe()),
        );
    }
    if function_.async_
        && *effect_ == ast::function::Effect::Hook
        && !cx.metadata().frozen.async_component_syntax
    {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::EUnsupportedSyntax(
                function_loc.dupe(),
                flow_typing_errors::intermediate_error_types::UnsupportedSyntax::AsyncHookSyntax,
            ),
        );
    }
    let cache = cx.node_cache();
    let tparams_map = mk_tparams_map(cx, tparams_map);
    let default_this = flow_js_utils::default_this_type(cx, bind_this, function_)
        .unwrap_or_else(|_| type_::mixed_t::at(function_loc.dupe()));
    let sig_data = statement::mk_func_sig(
        cx,
        false,
        false,
        false,
        statics,
        &tparams_map,
        reason.dupe(),
        function_,
    )?;
    let (ref func_sig, _) = sig_data;
    cache.set_function_sig(sig_loc, sig_data.clone());
    let t = flow_typing_statement::func_sig::functiontype(
        cx,
        !bind_this, // arrow
        Some(function_loc),
        default_this,
        func_sig,
    );
    Ok(if *effect_ != ast::function::Effect::Hook && hook_like {
        make_hooklike(cx, t)
    } else {
        t
    })
}

fn resolve_annotated_component(
    cx: &Context,
    scope_kind: ScopeKind,
    reason: Reason,
    tparams_map: &FlowOrdMap<ALoc, FlowSmolStr>,
    component_loc: ALoc,
    component: &ast::statement::ComponentDeclaration<ALoc, ALoc>,
) -> Type {
    if !cx.component_syntax() {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::EUnsupportedSyntax(
                component_loc.dupe(),
                flow_typing_errors::intermediate_error_types::UnsupportedSyntax::ComponentSyntax,
            ),
        );
        any_t::at(AnySource::AnyError(None), component_loc)
    } else {
        if scope_kind == ScopeKind::ComponentOrHookBody
            || scope_kind == ScopeKind::AsyncComponentOrHookBody
        {
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::ENestedComponent(reason.dupe()),
            );
        }
        if component.async_ && !cx.metadata().frozen.async_component_syntax {
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::EUnsupportedSyntax(
                    component_loc.dupe(),
                    flow_typing_errors::intermediate_error_types::UnsupportedSyntax::AsyncComponentSyntax,
                ),
            );
        }
        let tparams_map = mk_tparams_map(cx, tparams_map);
        let tparams_map: BTreeMap<SubstName, Type> = tparams_map
            .iter()
            .map(|(k, v)| (k.dupe(), v.dupe()))
            .collect();
        let sig_loc = component.sig_loc.dupe();
        let body = &component.body;
        let sig_data = statement::mk_component_sig(cx, &tparams_map, reason.dupe(), component);
        let (ref component_sig, _) = sig_data;
        let cache = cx.node_cache();
        cache.set_component_sig(sig_loc, sig_data.clone());
        let in_annotation = body.is_none();
        flow_typing_statement::component_sig::component_type::<
            flow_typing_statement::component_params::DeclarationConfig,
        >(
            cx,
            in_annotation,
            &component_sig.reason,
            component_sig.tparams.clone(),
            &component_sig.cparams.params,
            component_sig.cparams.rest.as_ref(),
            component_sig.renders_t.dupe(),
            component_sig.id_opt.as_ref().map(|(loc, name)| (loc, name)),
        )
    }
}

fn binding_has_annot(binding: &Binding) -> bool {
    match binding {
        Binding::Root(Root::Annotation { .. }) => true,
        Binding::Hooklike(b) | Binding::Select { parent: (_, b), .. } => binding_has_annot(b),
        _ => false,
    }
}

fn resolve_binding(
    cx: &Context,
    def_scope_kind: ScopeKind,
    reason: Reason,
    loc: ALoc,
    b: &Binding,
) -> Result<Type, AbnormalControlFlow> {
    match b {
        Binding::Root(Root::Annotation {
            tparams_map,
            optional,
            has_default_expression,
            param_loc,
            annot,
            react_deep_read_only,
            concrete: _,
        }) => {
            let t = resolve_annotation(
                cx,
                tparams_map,
                match (param_loc, react_deep_read_only) {
                    (Some(param_loc), Some(name_def_types::DroAnnot::Comp)) => {
                        Some((param_loc.dupe(), name_def_types::DroAnnot::Comp))
                    }
                    (Some(param_loc), Some(name_def_types::DroAnnot::Hook)) => {
                        Some((param_loc.dupe(), name_def_types::DroAnnot::Hook))
                    }
                    _ => None,
                },
                &ast::types::Annotation {
                    loc: annot.0.dupe(),
                    annotation: annot.1.dupe(),
                },
            );
            if let Some(param_loc) = param_loc {
                type_env::bind_function_param(cx, t.dupe(), param_loc.dupe());
            }
            if *optional && !*has_default_expression {
                Ok(type_util::optional(t, None, false))
            } else {
                Ok(t)
            }
        }
        Binding::Root(Root::Value(name_def_types::Value {
            hints: _,
            expr,
            decl_kind,
            as_const,
        })) => expression(cx, None, *decl_kind, Some(*as_const), expr),
        Binding::Root(Root::MatchCaseRoot {
            case_match_root_loc,
            root_pattern_loc,
            prev_pattern_loc,
        }) => {
            let unfiltered_t = type_env::var_ref(
                None,
                cx,
                None,
                reason::Name::new(FlowSmolStr::new("<match_root>")),
                case_match_root_loc.dupe(),
            );
            let node_cache = cx.node_cache();
            let pattern_union_state =
                get_pattern_union_state_from_prev(cx, prev_pattern_loc.dupe());
            // Optimization: if the previous value_left was only inexhaustible (e.g., just `string`),
            // filtering won't change it, so we can reuse it directly.
            let value_left = prev_pattern_loc.as_ref().and_then(|prev_loc| {
                node_cache
                    .get_match_pattern_value_union(prev_loc)
                    .filter(|vu| vu.is_only_inexhaustible())
            });
            let value_left = match value_left {
                Some(value_left) => value_left,
                None => {
                    let pattern_union =
                        exhaustive::pattern_union_builder::finalize(pattern_union_state.0.clone());
                    exhaustive::filter_by_pattern_union(cx, &unfiltered_t, &pattern_union)
                }
            };
            node_cache.set_match_pattern_value_union(root_pattern_loc.dupe(), value_left.dupe());
            node_cache.set_match_pattern_union(
                root_pattern_loc.dupe(),
                (pattern_union_state.0, pattern_union_state.1 as i32),
            );
            Ok(value_left.to_type(type_util::reason_of_t(&unfiltered_t).dupe()))
        }
        Binding::Root(Root::ObjectValue {
            obj_loc,
            obj,
            synthesizable: name_def_types::ObjectSynthKind::ObjectSynthesizable { .. },
        }) => {
            let resolve_prop = |bind_this: bool,
                                prop_loc: ALoc,
                                fn_loc: ALoc,
                                fn_: &ast::function::Function<ALoc, ALoc>|
             -> Result<Type, AbnormalControlFlow> {
                let reason = reason::func_reason(false, false, prop_loc);
                resolve_annotated_function(
                    cx,
                    def_scope_kind,
                    bind_this,
                    &BTreeMap::new(),
                    false,
                    reason,
                    &FlowOrdMap::default(),
                    fn_loc,
                    fn_,
                )
            };

            fn mk_expression(
                cx: &Context,
                def_scope_kind: ScopeKind,
                resolve_prop: &dyn Fn(
                    bool,
                    ALoc,
                    ALoc,
                    &ast::function::Function<ALoc, ALoc>,
                ) -> Result<Type, AbnormalControlFlow>,
                reason: &Reason,
                expr: &ast::expression::Expression<ALoc, ALoc>,
            ) -> Result<Type, AbnormalControlFlow> {
                Ok(match expr.deref() {
                    ast::expression::ExpressionInner::StringLiteral { .. }
                    | ast::expression::ExpressionInner::NumberLiteral { .. }
                    | ast::expression::ExpressionInner::NullLiteral { .. }
                    | ast::expression::ExpressionInner::BooleanLiteral { .. }
                    | ast::expression::ExpressionInner::BigIntLiteral { .. }
                    | ast::expression::ExpressionInner::RegExpLiteral { .. }
                    | ast::expression::ExpressionInner::ModuleRefLiteral { .. }
                    | ast::expression::ExpressionInner::Identifier { .. }
                    | ast::expression::ExpressionInner::TypeCast { .. }
                    | ast::expression::ExpressionInner::AsConstExpression { .. }
                    | ast::expression::ExpressionInner::AsExpression { .. }
                    | ast::expression::ExpressionInner::Member { .. } => {
                        synthesizable_expression(cx, EnclosingContext::NoContext, expr)?
                    }
                    ast::expression::ExpressionInner::Function { loc, inner: fn_ } => {
                        let sig_loc = fn_.sig_loc.dupe();
                        resolve_prop(true, sig_loc, loc.dupe(), fn_)?
                    }
                    ast::expression::ExpressionInner::ArrowFunction { loc, inner: fn_ } => {
                        let sig_loc = fn_.sig_loc.dupe();
                        resolve_prop(false, sig_loc, loc.dupe(), fn_)?
                    }
                    ast::expression::ExpressionInner::Object { loc, inner: obj } => {
                        mk_obj(cx, def_scope_kind, resolve_prop, reason, loc.dupe(), obj)?
                    }
                    ast::expression::ExpressionInner::Array { loc, inner: arr }
                        if arr.elements.is_empty() =>
                    {
                        let (_, elem_t) = statement::empty_array(cx, loc.dupe());
                        Type::new(TypeInner::DefT(
                            reason.dupe(),
                            DefT::new(DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT {
                                elem_t,
                                tuple_view: Some(type_::empty_tuple_view()),
                                react_dro: None,
                            }))),
                        ))
                    }
                    ast::expression::ExpressionInner::Array { loc, inner: arr } => {
                        let elements = &arr.elements;
                        let is_empty = elements.is_empty();
                        // TODO merge code with statement.ml implementation

                        let elem_spread_list: Vec<type_::UnresolvedParam> = elements
                            .iter()
                            .map(|e| -> Result<type_::UnresolvedParam, AbnormalControlFlow> {
                                use ast::expression::ArrayElement;
                                match e {
                                    ArrayElement::Expression(e) => {
                                        let t = mk_expression(
                                            cx,
                                            def_scope_kind,
                                            resolve_prop,
                                            reason,
                                            e,
                                        )?;
                                        let e_loc = e.loc();
                                        let elem_reason = reason::mk_reason(
                                            reason::VirtualReasonDesc::RArrayElement,
                                            e_loc.dupe(),
                                        );
                                        Ok(type_::UnresolvedParam::UnresolvedArg(
                                            type_util::mk_tuple_element(
                                                elem_reason,
                                                t,
                                                None,
                                                false,
                                                flow_common::polarity::Polarity::Neutral,
                                            ),
                                            None,
                                        ))
                                    }
                                    ArrayElement::Hole(hole_loc) => {
                                        let t = type_::empty_t::at(loc.dupe());
                                        let elem_reason = reason::mk_reason(
                                            reason::VirtualReasonDesc::RArrayElement,
                                            hole_loc.dupe(),
                                        );
                                        Ok(type_::UnresolvedParam::UnresolvedArg(
                                            type_util::mk_tuple_element(
                                                elem_reason,
                                                t,
                                                None,
                                                false,
                                                flow_common::polarity::Polarity::Neutral,
                                            ),
                                            None,
                                        ))
                                    }
                                    ArrayElement::Spread(spread) => {
                                        let t = synthesizable_expression(
                                            cx,
                                            EnclosingContext::NoContext,
                                            &spread.argument,
                                        )?;
                                        Ok(type_::UnresolvedParam::UnresolvedSpreadArg(t))
                                    }
                                }
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        let arr_reason =
                            reason::mk_reason(reason::VirtualReasonDesc::RArrayLit, loc.dupe());
                        flow_typing_tvar::mk_where(cx, arr_reason.dupe(), |tout| {
                            let reason_op = arr_reason.dupe();
                            let element_reason = reason_op.dupe().replace_desc(
                                reason::VirtualReasonDesc::RInferredUnionElemArray {
                                    instantiable: false,
                                    is_empty,
                                },
                            );
                            let elem_t = flow_typing_tvar::mk(cx, element_reason);
                            flow_js::FlowJs::resolve_spread_list(
                                cx,
                                type_::unknown_use(),
                                &reason_op,
                                elem_spread_list,
                                type_::SpreadResolve::ResolveSpreadsToArrayLiteral {
                                    id: reason::mk_id() as i32,
                                    as_const: false,
                                    elem_t,
                                    tout: tout.dupe(),
                                },
                            )
                            .expect("Non speculating");
                        })
                    }
                    ast::expression::ExpressionInner::Assignment { .. }
                    | ast::expression::ExpressionInner::Binary { .. }
                    | ast::expression::ExpressionInner::Call { .. }
                    | ast::expression::ExpressionInner::Class { .. }
                    | ast::expression::ExpressionInner::Conditional { .. }
                    | ast::expression::ExpressionInner::Import { .. }
                    | ast::expression::ExpressionInner::JSXElement { .. }
                    | ast::expression::ExpressionInner::JSXFragment { .. }
                    | ast::expression::ExpressionInner::Logical { .. }
                    | ast::expression::ExpressionInner::Match { .. }
                    | ast::expression::ExpressionInner::MetaProperty { .. }
                    | ast::expression::ExpressionInner::New { .. }
                    | ast::expression::ExpressionInner::OptionalCall { .. }
                    | ast::expression::ExpressionInner::OptionalMember { .. }
                    | ast::expression::ExpressionInner::Record { .. }
                    | ast::expression::ExpressionInner::Sequence { .. }
                    | ast::expression::ExpressionInner::Super { .. }
                    | ast::expression::ExpressionInner::TaggedTemplate { .. }
                    | ast::expression::ExpressionInner::TemplateLiteral { .. }
                    | ast::expression::ExpressionInner::This { .. }
                    | ast::expression::ExpressionInner::TSSatisfies { .. }
                    | ast::expression::ExpressionInner::Unary { .. }
                    | ast::expression::ExpressionInner::Update { .. }
                    | ast::expression::ExpressionInner::Yield { .. } => {
                        panic!("Object not synthesizable")
                    }
                })
            }

            fn mk_obj(
                cx: &Context,
                def_scope_kind: ScopeKind,
                resolve_prop: &dyn Fn(
                    bool,
                    ALoc,
                    ALoc,
                    &ast::function::Function<ALoc, ALoc>,
                ) -> Result<Type, AbnormalControlFlow>,
                reason: &Reason,
                obj_loc: ALoc,
                obj: &ast::expression::Object<ALoc, ALoc>,
            ) -> Result<Type, AbnormalControlFlow> {
                let obj_reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RObjectLit, obj_loc.dupe());
                let obj_proto = Type::new(TypeInner::ObjProtoT(obj_reason.dupe()));
                let acc = obj.properties.iter().try_fold(
                    statement::object_expression_acc::ObjectExpressionAcc::empty(),
                    |acc, prop| -> Result<_, AbnormalControlFlow> {
                        use ast::expression::object::*;
                        Ok(match prop {
                            Property::SpreadProperty(SpreadProperty { argument: exp, .. })
                                if matches!(
                                    exp.deref(),
                                    ast::expression::ExpressionInner::Identifier { .. }
                                ) =>
                            {
                                let spread =
                                    synthesizable_expression(cx, EnclosingContext::NoContext, exp)?;
                                acc.add_spread(spread)
                            }
                            Property::NormalProperty(NormalProperty::Method {
                                loc: prop_loc,
                                key,
                                value: (fn_loc, fn_),
                            }) => {
                                let t = resolve_prop(false, prop_loc.dupe(), fn_loc.dupe(), fn_)?;
                                match key {
                                    Key::Identifier(id) => {
                                        let name_loc = id.loc.dupe();
                                        let name = id.name.dupe();
                                        acc.add_prop(move |mut props_map| {
                                            props_map.add_method(
                                                reason::Name::new(name),
                                                Some(name_loc.dupe()),
                                                t,
                                            );
                                            props_map
                                        })
                                    }
                                    Key::StringLiteral((name_loc, lit)) => {
                                        let name = lit.value.dupe();
                                        acc.add_prop(move |mut props_map| {
                                            props_map.add_method(
                                                reason::Name::new(name),
                                                Some(name_loc.dupe()),
                                                t,
                                            );
                                            props_map
                                        })
                                    }
                                    _ => panic!("Object not synthesizable"),
                                }
                            }
                            Property::NormalProperty(NormalProperty::Init {
                                key, value, ..
                            }) => {
                                let t =
                                    mk_expression(cx, def_scope_kind, resolve_prop, reason, value)?;
                                match key {
                                    Key::Identifier(id) => {
                                        let name_loc = id.loc.dupe();
                                        let name = id.name.dupe();
                                        acc.add_prop(move |mut props_map| {
                                            props_map.add_field(
                                                reason::Name::new(name),
                                                flow_common::polarity::Polarity::Neutral,
                                                None,
                                                Some(name_loc.dupe()),
                                                t,
                                            );
                                            props_map
                                        })
                                    }
                                    Key::StringLiteral((name_loc, lit)) => {
                                        let name = lit.value.dupe();
                                        acc.add_prop(move |mut props_map| {
                                            props_map.add_field(
                                                reason::Name::new(name),
                                                flow_common::polarity::Polarity::Neutral,
                                                None,
                                                Some(name_loc.dupe()),
                                                t,
                                            );
                                            props_map
                                        })
                                    }
                                    _ => panic!("Object not synthesizable"),
                                }
                            }
                            _ => panic!("Object not synthesizable"),
                        })
                    },
                )?;
                Ok(acc.mk_object_from_spread_acc(cx, obj_reason, false, false, obj_proto))
            }

            mk_obj(
                cx,
                def_scope_kind,
                &resolve_prop,
                &reason,
                obj_loc.dupe(),
                obj,
            )
        }
        Binding::Root(Root::ObjectValue {
            obj_loc,
            obj,
            synthesizable: _,
        }) => {
            let expr = ast::expression::Expression::new(ast::expression::ExpressionInner::Object {
                loc: obj_loc.dupe(),
                inner: obj.clone().into(),
            });
            expression(cx, None, None, None, &expr)
        }
        Binding::Root(Root::FunctionValue {
            hints: _,
            synthesizable_from_annotation: name_def_types::FunctionSynthKind::FunctionSynthesizable,
            function_loc,
            function_,
            statics,
            arrow,
            tparams_map,
        }) => {
            let cache = cx.node_cache();
            let tparams_map = mk_tparams_map(cx, tparams_map);
            let sig_loc = function_.sig_loc.dupe();
            let async_ = function_.async_;
            let generator = function_.generator;
            let reason_fun = reason::func_reason(
                async_,
                generator,
                if *arrow {
                    function_loc.dupe()
                } else {
                    sig_loc.dupe()
                },
            );
            let default_this = flow_js_utils::default_this_type(cx, !*arrow, function_)
                .unwrap_or_else(|_| type_::mixed_t::at(function_loc.dupe()));
            let sig_data = statement::mk_func_sig(
                cx,
                false,
                false,
                false,
                statics,
                &tparams_map,
                reason_fun,
                function_,
            )?;
            let (ref func_sig, _) = sig_data;
            let t = flow_typing_statement::func_sig::functiontype(
                cx,
                *arrow,
                Some(function_loc.dupe()),
                default_this,
                func_sig,
            );
            cache.set_function_sig(sig_loc, sig_data);
            Ok(t)
        }
        Binding::Root(Root::FunctionValue {
            hints: _,
            synthesizable_from_annotation: _,
            function_loc,
            function_,
            statics,
            arrow,
            tparams_map: _,
        }) => {
            let id = &function_.id;
            let async_ = function_.async_;
            let generator = function_.generator;
            let sig_loc = function_.sig_loc.dupe();
            let reason_fun = reason::func_reason(
                async_,
                generator,
                if *arrow {
                    function_loc.dupe()
                } else {
                    sig_loc.dupe()
                },
            );
            let func: (Type, ast::function::Function<ALoc, (ALoc, Type)>) = if *arrow {
                statement::mk_arrow(cx, statics, reason_fun, function_)?
            } else {
                statement::mk_function(
                    cx,
                    true,
                    None,
                    statics,
                    reason_fun,
                    function_loc.dupe(),
                    function_,
                )?
            };
            let (func_type, func_ast) = func;
            let cache = cx.node_cache();
            match id {
                Some(id) => {
                    cache.set_function(id.loc.dupe(), (func_type.dupe(), func_ast.clone()));
                }
                None => {
                    cache.set_function(function_loc.dupe(), (func_type.dupe(), func_ast.clone()));
                }
            }
            let expr = ast::expression::Expression::new(if *arrow {
                ast::expression::ExpressionInner::ArrowFunction {
                    loc: (function_loc.dupe(), func_type.dupe()),
                    inner: func_ast.into(),
                }
            } else {
                ast::expression::ExpressionInner::Function {
                    loc: (function_loc.dupe(), func_type.dupe()),
                    inner: func_ast.into(),
                }
            });
            cache.set_expression(expr);
            Ok(func_type)
        }
        Binding::Root(Root::EmptyArray {
            array_providers,
            arr_loc,
        }) => {
            let (elem_t, tuple_view, arr_reason) = if !array_providers.is_empty() {
                let ts: Vec<Type> = array_providers
                    .iter()
                    .map(|provider_loc| {
                        type_env::checked_find_loc_env_write(
                            cx,
                            env_api::DefLocType::ArrayProviderLoc,
                            provider_loc.dupe(),
                        )
                    })
                    .collect();
                let elem_t = flow_typing_tvar::mk_where(
                    cx,
                    reason::mk_reason(reason::VirtualReasonDesc::REmptyArrayElement, loc.dupe()),
                    |tvar| {
                        for t in &ts {
                            let use_t = UseT::new(type_::UseTInner::UseT(
                                type_::unknown_use(),
                                tvar.dupe(),
                            ));
                            flow_js::flow_non_speculating(cx, (t, &use_t));
                        }
                    },
                );
                (elem_t, None, reason.dupe())
            } else {
                let elem_t = type_::empty_t::make(reason::mk_reason(
                    reason::VirtualReasonDesc::REmptyArrayElement,
                    loc.dupe(),
                ));
                flow_js_utils::add_output_non_speculating(
                    cx,
                    flow_typing_errors::error_message::ErrorMessage::EEmptyArrayNoProvider {
                        loc: loc.dupe(),
                    },
                );
                (
                    elem_t,
                    Some(type_::empty_tuple_view()),
                    reason
                        .dupe()
                        .replace_desc(reason::VirtualReasonDesc::REmptyArrayLit),
                )
            };
            let t = Type::new(TypeInner::DefT(
                arr_reason,
                DefT::new(DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT {
                    elem_t,
                    tuple_view,
                    react_dro: None,
                }))),
            ));
            let cache = cx.node_cache();
            let exp: ast::expression::Expression<ALoc, (ALoc, Type)> =
                ast::expression::Expression::new(ast::expression::ExpressionInner::Array {
                    loc: (arr_loc.dupe(), t.dupe()),
                    inner: ast::expression::Array {
                        elements: vec![].into(),
                        comments: None,
                    }
                    .into(),
                });
            cache.set_expression(exp);
            Ok(t)
        }
        Binding::Root(Root::Contextual {
            reason: ctx_reason,
            hints,
            optional,
            default_expression,
        }) => {
            let param_loc = ctx_reason.loc().dupe();
            let type_::LazyHintT(has_hint, lazy_hint) = lazily_resolve_hints(cx, loc.dupe(), hints);
            let t = match lazy_hint(false, None, ctx_reason.dupe()) {
                type_::HintEvalResult::HintAvailable(t, _) => {
                    let t = if default_expression.is_some() {
                        flow_typing_tvar::mk_where(cx, ctx_reason.dupe(), |tout| {
                            let use_t = UseT::new(type_::UseTInner::FilterOptionalT(
                                type_::unknown_use(),
                                tout.dupe(),
                            ));
                            flow_js::flow_non_speculating(cx, (&t, &use_t));
                        })
                    } else {
                        t
                    };
                    type_util::mod_reason_of_t(&|_| ctx_reason.dupe(), &t)
                }
                type_::HintEvalResult::NoHint
                | type_::HintEvalResult::DecompositionError
                | type_::HintEvalResult::EncounteredPlaceholder => {
                    if has_hint {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            flow_typing_errors::error_message::ErrorMessage::EMissingLocalAnnotation {
                                reason: ctx_reason.dupe(),
                                hint_available: true,
                                from_generic_function: false,
                            },
                        );
                    }
                    any_t::make(
                        AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                        ctx_reason.dupe(),
                    )
                }
            };
            if hints.is_empty() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    flow_typing_errors::error_message::ErrorMessage::EMissingLocalAnnotation {
                        reason: ctx_reason.dupe(),
                        hint_available: false,
                        from_generic_function: false,
                    },
                );
            }
            type_env::bind_function_param(cx, t.dupe(), param_loc);
            if *optional && default_expression.is_none() {
                Ok(type_util::optional(t, None, false))
            } else {
                Ok(t)
            }
        }
        Binding::Root(Root::UnannotatedParameter(unannotated_reason)) => {
            let t = any_t::make(
                AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                unannotated_reason.dupe(),
            );
            type_env::bind_function_param(cx, t.dupe(), unannotated_reason.loc().dupe());
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::EMissingLocalAnnotation {
                    reason: unannotated_reason.dupe(),
                    hint_available: false,
                    from_generic_function: false,
                },
            );
            Ok(t)
        }
        Binding::Root(Root::CatchUnannotated) => {
            if cx.use_unknown_in_catch_variables() {
                Ok(type_::mixed_t::at(loc))
            } else {
                let r = reason::mk_reason(reason::VirtualReasonDesc::RAnyImplicit, loc);
                Ok(any_t::make(
                    AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                    r,
                ))
            }
        }
        Binding::Root(Root::DeclareVariableMissingAnnotationAndInit) => {
            let r = reason::mk_reason(reason::VirtualReasonDesc::RAnyImplicit, loc);
            Ok(any_t::make(
                AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                r,
            ))
        }
        Binding::Root(Root::For(kind, (_, exp_inner))) => {
            let right_t = expression(
                cx,
                Some(EnclosingContext::OtherTestContext),
                None,
                None,
                exp_inner,
            )?;
            Ok(match kind {
                name_def_types::ForKind::In => {
                    type_operation_utils::type_assertions::assert_for_in_rhs(cx, &right_t);
                    type_::str_module_t::at(loc)
                }
                name_def_types::ForKind::Of { await_ } => {
                    let reason = reason::mk_reason(
                        reason::VirtualReasonDesc::RForOfElement,
                        // TODO: loc should be loc of loop
                        loc,
                    );
                    statement::for_of_elemt(cx, right_t, reason, *await_)
                }
            })
        }
        Binding::Hooklike(binding) => {
            let t = resolve_binding(cx, def_scope_kind, reason, loc, binding)?;
            Ok(make_hooklike(cx, t))
        }
        Binding::Select {
            selector,
            parent: (parent_loc, binding),
        } => {
            let node_cache = cx.node_cache();
            let filtered_pattern_type = node_cache
                .get_match_pattern_value_union(parent_loc)
                .and_then(|vu| vu.select(selector))
                .map(|value_left| {
                    node_cache.set_match_pattern_value_union(loc.dupe(), value_left.clone());
                    value_left.to_type(reason.dupe())
                });
            Ok(match filtered_pattern_type {
                Some(t) => t,
                None => {
                    let refined_type = match selector {
                        selector::Selector::Prop { prop, prop_loc, .. } => {
                            let desc = reason::VirtualReasonDesc::RProperty(Some(
                                reason::Name::new(prop.dupe()),
                            ));
                            type_env::get_refinement(cx, desc, prop_loc.dupe())
                        }
                        _ => None,
                    };
                    match refined_type {
                        // When we can get a refined value on a destructured property,
                        // we must be in an assignment position and the type must have been resolved.
                        Some(t) => t,
                        None => {
                            let t = type_env::checked_find_loc_env_write(
                                cx,
                                env_api::DefLocType::PatternLoc,
                                parent_loc.dupe(),
                            );
                            let has_anno = binding_has_annot(binding);
                            let (selector_ty, sel_reason, has_default) =
                                mk_selector_reason_has_default(cx, &loc, selector)?;
                            let kind = if has_anno {
                                type_::DestructKind::DestructAnnot
                            } else {
                                type_::DestructKind::DestructInfer
                            };
                            let sel_reason_c = sel_reason.dupe();
                            let selector_c = selector_ty.clone();
                            let kind_c = kind.clone();
                            let cx_c = cx.dupe();
                            let t = flow_js_utils::map_on_resolved_type(
                                cx,
                                sel_reason.dupe(),
                                t,
                                move |t| {
                                    let cx = &cx_c;
                                    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                                        cx,
                                        sel_reason_c.dupe(),
                                        |tout_reason, tout_id| {
                                            let tout =
                                                Tvar::new(tout_reason.dupe(), tout_id as u32);
                                            let use_t =
                                                UseT::new(type_::UseTInner::DestructuringT(
                                                    sel_reason_c.dupe(),
                                                    kind_c.clone(),
                                                    selector_c.clone(),
                                                    Box::new(tout),
                                                    reason::mk_id() as i32,
                                                ));
                                            flow_js::flow_non_speculating(cx, (&t, &use_t));
                                        },
                                    )
                                },
                            );
                            if has_default {
                                let (default_selector, default_reason, _) =
                                    mk_selector_reason_has_default(
                                        cx,
                                        &loc,
                                        &selector::Selector::Default,
                                    )?;
                                let default_reason_c = default_reason.dupe();
                                let cx_c2 = cx.dupe();
                                flow_js_utils::map_on_resolved_type(
                                    cx,
                                    default_reason.dupe(),
                                    t,
                                    move |t| {
                                        let cx = &cx_c2;
                                        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                                            cx,
                                            default_reason_c.dupe(),
                                            |tout_reason, tout_id| {
                                                let tout =
                                                    Tvar::new(tout_reason.dupe(), tout_id as u32);
                                                let use_t =
                                                    UseT::new(type_::UseTInner::DestructuringT(
                                                        default_reason_c.dupe(),
                                                        kind.clone(),
                                                        default_selector.clone(),
                                                        Box::new(tout),
                                                        reason::mk_id() as i32,
                                                    ));
                                                flow_js::flow_non_speculating(cx, (&t, &use_t));
                                            },
                                        )
                                    },
                                )
                            } else {
                                t
                            }
                        }
                    }
                }
            })
        }
    }
}

fn resolve_inferred_function(
    cx: &Context,
    scope_kind: ScopeKind,
    statics: &BTreeMap<FlowSmolStr, env_api::EnvKey<ALoc>>,
    needs_this_param: bool,
    id_loc: ALoc,
    reason: Reason,
    function_loc: ALoc,
    function_: &ast::function::Function<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let cache = cx.node_cache();
    let fn_ = statement::mk_function(
        cx,
        needs_this_param,
        None,
        statics,
        reason.dupe(),
        function_loc.dupe(),
        function_,
    )?;
    let (fun_type, _) = &fn_;
    let fun_type = fun_type.dupe();
    cache.set_function(id_loc, fn_);
    if (scope_kind == ScopeKind::ComponentOrHookBody
        || scope_kind == ScopeKind::AsyncComponentOrHookBody)
        && function_.effect_ == ast::function::Effect::Hook
    {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::ENestedHook(reason.dupe()),
        );
    }
    if function_.async_
        && function_.effect_ == ast::function::Effect::Hook
        && !cx.metadata().frozen.async_component_syntax
    {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::EUnsupportedSyntax(
                function_loc.dupe(),
                flow_typing_errors::intermediate_error_types::UnsupportedSyntax::AsyncHookSyntax,
            ),
        );
    }
    Ok(
        if function_.effect_ != ast::function::Effect::Hook
            && ast_utils::hook_function(function_).is_some()
        {
            make_hooklike(cx, fun_type)
        } else {
            fun_type
        },
    )
}

fn resolve_class(
    cx: &Context,
    id_loc: ALoc,
    reason: Reason,
    kind: &name_def_types::ClassKind,
    class_loc: ALoc,
    class_: &ast::class::Class<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let cache = cx.node_cache();
    let inst_kind = match kind {
        name_def_types::ClassKind::Class => type_::InstanceKind::ClassKind,
        name_def_types::ClassKind::Record { defaulted_props } => type_::InstanceKind::RecordKind {
            defaulted_props: defaulted_props.iter().duped().collect(),
        },
    };
    let sig_info =
        statement::mk_class_sig(cx, id_loc, class_loc.dupe(), inst_kind, reason, class_)?;
    let (class_t, class_t_internal, class_sig, reconstruct) = sig_info;
    // Convert Box<dyn FnOnce> to Rc<dyn Fn> for node_cache storage
    let reconstruct_cell = std::cell::RefCell::new(Some(reconstruct));
    let reconstruct_fn: Rc<dyn Fn(Type) -> ast::class::Class<ALoc, (ALoc, Type)>> =
        Rc::new(move |t| {
            reconstruct_cell
                .borrow_mut()
                .take()
                .expect("class reconstruct called more than once")(t)
        });
    cache.set_class_sig(
        class_loc.dupe(),
        (
            class_t.dupe(),
            class_t_internal.dupe(),
            class_sig,
            reconstruct_fn,
        ),
    );
    type_env::bind_class_self_type(cx, class_t_internal, class_loc);
    Ok(class_t)
}

fn resolve_record(
    cx: &Context,
    id_loc: ALoc,
    reason: Reason,
    record_loc: ALoc,
    defaulted_props: &BTreeSet<FlowSmolStr>,
    record: &ast::statement::RecordDeclaration<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let cache = cx.node_cache();
    let sig_info = statement::mk_record_sig(
        cx,
        id_loc,
        record_loc.dupe(),
        defaulted_props,
        reason,
        record,
    )?;
    let record_t = sig_info.0.dupe();
    let record_t_internal = sig_info.1.dupe();
    cache.set_record_sig(record_loc.dupe(), sig_info);
    type_env::bind_class_self_type(cx, record_t_internal, record_loc);
    Ok(record_t)
}

fn resolve_op_assign(
    cx: &Context,
    exp_loc: ALoc,
    lhs: &ast::pattern::Pattern<ALoc, ALoc>,
    assertion: bool,
    op: ast::expression::AssignmentOperator,
    rhs: &ast::expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let reason = reason::mk_reason(
        reason::VirtualReasonDesc::RCustom(ast_utils::string_of_assignment_operator(op).into()),
        exp_loc.dupe(),
    );
    match op {
        ast::expression::AssignmentOperator::PlusAssign
        | ast::expression::AssignmentOperator::MinusAssign
        | ast::expression::AssignmentOperator::MultAssign
        | ast::expression::AssignmentOperator::ExpAssign
        | ast::expression::AssignmentOperator::DivAssign
        | ast::expression::AssignmentOperator::ModAssign
        | ast::expression::AssignmentOperator::LShiftAssign
        | ast::expression::AssignmentOperator::RShiftAssign
        | ast::expression::AssignmentOperator::RShift3Assign
        | ast::expression::AssignmentOperator::BitOrAssign
        | ast::expression::AssignmentOperator::BitXorAssign
        | ast::expression::AssignmentOperator::BitAndAssign => {
            // lhs (op)= rhs
            let lhs_patt = statement::assignment_lhs(cx, lhs)?;
            let (lhs_loc, lhs_t) = lhs_patt.loc();
            let lhs_t = if assertion {
                let reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RNonnullAssert, lhs_loc.dupe());
                type_operation_utils::operators::non_maybe(cx, &reason, lhs_t)
            } else {
                lhs_t.dupe()
            };
            let rhs_t = expression(cx, None, None, None, rhs)?;
            Ok(type_operation_utils::operators::arith(
                cx,
                &reason,
                &type_::arith_kind::ArithKind::of_assignment_operator(op),
                &lhs_t,
                &rhs_t,
            ))
        }
        ast::expression::AssignmentOperator::AndAssign
        | ast::expression::AssignmentOperator::OrAssign
        | ast::expression::AssignmentOperator::NullishAssign => {
            let lhs_patt = statement::assignment_lhs(cx, lhs)?;
            let (_, lhs_t) = lhs_patt.loc();
            let (rhs_result, right_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    statement::expression(None, None, None, cx, rhs)
                });
            let rhs_t = rhs_result.loc().1.dupe();
            let rhs_t = if right_throws {
                type_::empty_t::at(exp_loc.dupe())
            } else {
                rhs_t
            };
            Ok(match op {
                ast::expression::AssignmentOperator::NullishAssign => {
                    type_operation_utils::operators::logical_nullish_coalesce(
                        cx, &reason, lhs_t, &rhs_t,
                    )
                }
                ast::expression::AssignmentOperator::AndAssign => {
                    type_operation_utils::operators::logical_and(cx, &reason, lhs_t, &rhs_t)
                }
                ast::expression::AssignmentOperator::OrAssign => {
                    type_operation_utils::operators::logical_or(cx, &reason, lhs_t, &rhs_t)
                }
                _ => unreachable!("Bad conditional guard"),
            })
        }
    }
}

fn resolve_update(cx: &Context, id_loc: ALoc, exp_loc: ALoc, id_reason: Reason) -> Type {
    let reason = reason::mk_reason(reason::VirtualReasonDesc::RUpdate, exp_loc);
    let id_t = type_env::ref_entry_exn(type_env::LookupMode::ForValue, cx, id_loc, id_reason);
    type_operation_utils::operators::unary_arith(cx, &reason, &type_::UnaryArithKind::Update, &id_t)
}

fn resolve_type_alias(
    cx: &Context,
    loc: ALoc,
    alias: &ast::statement::TypeAlias<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::type_alias(cx, loc.dupe(), alias);
    cache.set_alias(loc, (t.dupe(), ast));
    t
}

fn resolve_opaque_type(
    cx: &Context,
    loc: ALoc,
    opaque: &ast::statement::OpaqueType<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::opaque_type(cx, loc.dupe(), opaque);
    cache.set_opaque(loc, (t.dupe(), ast));
    t
}

fn resolve_import(
    cx: &Context,
    id_loc: ALoc,
    import_reason: Reason,
    import_kind: ast::statement::ImportKind,
    module_name: &FlowSmolStr,
    source_loc: ALoc,
    import: &Import,
) -> Type {
    let module_name_userland = flow_import_specifier::Userland::from_smol_str(module_name.dupe());
    let source_module = {
        let import_kind_for_untyped_import_validation = match import_kind {
            ast::statement::ImportKind::ImportType => Some(type_::ImportKind::ImportType),
            ast::statement::ImportKind::ImportTypeof => Some(type_::ImportKind::ImportTypeof),
            ast::statement::ImportKind::ImportValue => Some(type_::ImportKind::ImportValue),
        };
        flow_js_utils::import_export_utils::get_module_type_or_any(
            cx,
            false,
            import_kind_for_untyped_import_validation,
            source_loc,
            module_name_userland.dupe(),
        )
        .expect("Non speculating")
    };
    match import {
        Import::Named {
            kind,
            remote,
            local,
        } => {
            let import_kind = kind.unwrap_or(import_kind);
            let singleton_concretize =
                |cx: &Context, reason: Reason, t: Type| -> Result<Type, FlowJsException> {
                    flow_js::FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                        .map_err(|e| e.into())
                };
            let (_, t) = flow_js_utils::import_export_utils::import_named_specifier_type(
                cx,
                import_reason,
                &singleton_concretize,
                &import_kind,
                module_name_userland.dupe(),
                &source_module,
                remote,
                local,
            )
            .expect("Non speculating");
            if ast_utils::hook_name(local.as_str()) {
                make_hooklike(cx, t)
            } else {
                t
            }
        }
        Import::Namespace(name) => {
            let namespace_symbol =
                flow_common::flow_symbol::Symbol::mk_namespace_symbol(name.dupe(), id_loc.dupe());
            let t = flow_js_utils::import_export_utils::import_namespace_specifier_type(
                cx,
                import_reason,
                &import_kind,
                module_name_userland.dupe(),
                namespace_symbol,
                &source_module,
                id_loc.dupe(),
            )
            .expect("Non speculating");
            if ast_utils::hook_name(name.as_str()) {
                make_hooklike(cx, t)
            } else {
                t
            }
        }
        Import::Default(local_name) => {
            let singleton_concretize =
                |cx: &Context, reason: Reason, t: Type| -> Result<Type, FlowJsException> {
                    flow_js::FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                        .map_err(|e| e.into())
                };
            let (_, t) = flow_js_utils::import_export_utils::import_default_specifier_type(
                cx,
                import_reason,
                &singleton_concretize,
                &import_kind,
                module_name_userland.dupe(),
                &source_module,
                local_name,
            )
            .expect("Non speculating");
            if ast_utils::hook_name(local_name.as_str()) {
                make_hooklike(cx, t)
            } else {
                t
            }
        }
    }
}

fn resolve_interface(
    cx: &Context,
    loc: ALoc,
    inter: &ast::statement::Interface<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::interface(cx, loc.dupe(), inter);
    cache.set_interface(loc, (t.dupe(), ast));
    t
}

fn resolve_declare_class(
    cx: &Context,
    loc: ALoc,
    class_: &ast::statement::DeclareClass<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::declare_class(cx, loc.dupe(), class_);
    cache.set_declared_class(loc, (t.dupe(), ast));
    t
}

fn resolve_declare_component(
    cx: &Context,
    loc: ALoc,
    component: &ast::statement::DeclareComponent<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::declare_component(cx, loc.dupe(), component);
    cache.set_declared_component(loc, (t.dupe(), ast));
    t
}

fn resolve_declare_namespace(
    cx: &Context,
    loc: ALoc,
    ns: &ast::statement::DeclareNamespace<ALoc, ALoc>,
) -> Type {
    let cache = cx.node_cache();
    let (t, ast) = statement::declare_namespace(cx, loc.dupe(), ns);
    cache.set_declared_namespace(loc, (t.dupe(), ast));
    t
}

fn resolve_enum(
    cx: &Context,
    id_loc: ALoc,
    enum_reason: Reason,
    enum_loc: ALoc,
    name: &str,
    enum_: &ast::statement::enum_declaration::Body<ALoc>,
) -> Type {
    if cx.enable_enums() {
        let enum_info = statement::mk_enum(cx, enum_reason.dupe(), id_loc, name, enum_);
        type_::mk_enum_object_type(
            enum_reason,
            type_::EnumInfo::new(type_::EnumInfoInner::ConcreteEnum(
                type_::EnumConcreteInfo::new(enum_info),
            ))
            .into(),
        )
    } else {
        flow_js_utils::add_output_non_speculating(
            cx,
            flow_typing_errors::error_message::ErrorMessage::EEnumError(
                flow_typing_errors::error_message::EnumErrorKind::EnumsNotEnabled(enum_loc),
            ),
        );
        any_t::error(enum_reason)
    }
}

fn resolve_type_param(cx: &Context, id_loc: &ALoc) -> Type {
    let env = cx.environment();
    let (_, _, t) = env.tparams.get_ordinary(id_loc).expect("tparam not found");
    let reason = type_util::reason_of_t(t).dupe();
    Type::new(TypeInner::DefT(
        reason,
        DefT::new(DefTInner::TypeT(type_::TypeTKind::TypeParamKind, t.dupe())),
    ))
}

fn resolve_chain_expression(
    cx: &Context,
    cond: EnclosingContext,
    exp: &ast::expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let cache = cx.node_cache();
    let (t, _, exp_typed) = statement::optional_chain(cond, cx, exp)?;
    cache.set_expression(exp_typed);
    Ok(t)
}

fn resolve_write_expression(
    cx: &Context,
    cond: EnclosingContext,
    exp: &ast::expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    synthesizable_expression(cx, cond, exp)
}

fn resolve_match_pattern(
    cx: &Context,
    def_reason: Reason,
    case_match_root_loc: ALoc,
    has_guard: bool,
    prev_pattern_loc: Option<ALoc>,
    pattern: &ast::match_pattern::MatchPattern<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    let typed_pattern = statement::match_pattern(cx, case_match_root_loc, has_guard, pattern)?;
    let pattern_loc = typed_pattern.loc().dupe();
    // Set the match pattern union cache for incremental PatternUnion building.
    // This allows subsequent patterns to depend on this one and build incrementally.
    let pattern_union_state = get_pattern_union_state_from_prev(cx, prev_pattern_loc);
    let cache = cx.node_cache();
    cache.set_match_pattern_union(
        pattern_loc,
        (pattern_union_state.0, pattern_union_state.1 as i32),
    );
    Ok(type_::mixed_t::why(def_reason))
}

fn resolve_generator_next(
    cx: &Context,
    reason: Reason,
    gen_info: Option<&name_def_types::GeneratorAnnot>,
) -> Type {
    match gen_info {
        None => {
            let r = reason
                .dupe()
                .replace_desc(reason::VirtualReasonDesc::RUnannotatedNext);
            type_::void::make(r)
        }
        Some(gen_info) => {
            let cache = cx.node_cache();
            let tparams_map = mk_tparams_map(cx, &gen_info.tparams_map);
            let (t, anno) = type_annotation::mk_type_available_annotation(
                cx,
                tparams_map,
                &ast::types::Annotation {
                    loc: gen_info.return_annot.0.dupe(),
                    annotation: gen_info.return_annot.1.dupe(),
                },
            );
            cache.set_annotation(anno);
            let return_t = t;
            let gen_name = if gen_info.async_ {
                "AsyncGenerator"
            } else {
                "Generator"
            };
            flow_typing_tvar::mk_where(cx, reason.dupe(), |next| {
                let yield_reason = reason
                    .dupe()
                    .replace_desc(reason::VirtualReasonDesc::RUnusedYield);
                let return_reason = reason
                    .dupe()
                    .replace_desc(reason::VirtualReasonDesc::RUnusedReturn);
                let yield_tvar = flow_typing_tvar::mk(cx, yield_reason);
                let return_tvar = flow_typing_tvar::mk(cx, return_reason);
                let t = flow_js::FlowJs::get_builtin_typeapp(
                    cx,
                    &reason,
                    None,
                    gen_name,
                    vec![yield_tvar, return_tvar, next.dupe()],
                );
                let return_t_reason = type_util::reason_of_t(&return_t);
                let return_t_loc = return_t_reason.loc().dupe();
                let t = flow_js::reposition_non_speculating(cx, return_t_loc, t);
                flow_js::flow_t_non_speculating(cx, (&t, &return_t));
            })
        }
    }
}

fn resolve(
    cx: &Context,
    def_kind: env_api::DefLocType,
    id_loc: ALoc,
    def: &Def,
    def_scope_kind: ScopeKind,
    class_stack: &name_def_types::ClassStack,
    def_reason: Reason,
) -> Result<(), AbnormalControlFlow> {
    {
        let mut env = cx.environment_mut();
        env.scope_kind = def_scope_kind;
        env.class_stack = class_stack.dupe();
    }
    let t = match def {
        Def::Binding(b) => {
            resolve_binding(cx, def_scope_kind, def_reason.dupe(), id_loc.dupe(), b)?
        }
        Def::MatchCasePattern {
            case_match_root_loc,
            has_guard,
            pattern,
            prev_pattern_loc,
        } => resolve_match_pattern(
            cx,
            def_reason,
            case_match_root_loc.dupe(),
            *has_guard,
            prev_pattern_loc.dupe(),
            &pattern.1,
        )?,
        Def::ExpressionDef(name_def_types::ExpressionDef {
            cond_context,
            expr,
            chain: true,
            ..
        }) => resolve_chain_expression(cx, cond_context.dupe(), expr)?,
        Def::ExpressionDef(name_def_types::ExpressionDef {
            cond_context,
            expr,
            chain: false,
            ..
        }) => resolve_write_expression(cx, cond_context.dupe(), expr)?,
        Def::Component {
            component,
            component_loc,
            tparams_map,
        } => resolve_annotated_component(
            cx,
            def_scope_kind,
            def_reason,
            tparams_map,
            component_loc.dupe(),
            component,
        ),
        Def::Function {
            function_,
            synthesizable_from_annotation: name_def_types::FunctionSynthKind::FunctionSynthesizable,
            arrow,
            has_this_def: _,
            function_loc,
            tparams_map,
            statics,
            hints: _,
        } => {
            let hook_like = ast_utils::hook_function(function_).is_some();
            resolve_annotated_function(
                cx,
                def_scope_kind,
                !*arrow,
                statics,
                hook_like,
                def_reason,
                tparams_map,
                function_loc.dupe(),
                function_,
            )?
        }
        Def::Function {
            function_,
            synthesizable_from_annotation: _,
            arrow,
            has_this_def: _,
            function_loc,
            tparams_map: _,
            statics,
            hints: _,
        } => resolve_inferred_function(
            cx,
            def_scope_kind,
            statics,
            !*arrow,
            id_loc.dupe(),
            def_reason,
            function_loc.dupe(),
            function_,
        )?,
        Def::Class {
            class_,
            class_loc,
            kind,
            this_super_write_locs: _,
        } => resolve_class(
            cx,
            id_loc.dupe(),
            def_reason,
            kind,
            class_loc.dupe(),
            class_,
        )?,
        Def::Record {
            record,
            record_loc,
            this_super_write_locs: _,
            defaulted_props,
        } => resolve_record(
            cx,
            id_loc.dupe(),
            def_reason,
            record_loc.dupe(),
            defaulted_props,
            record,
        )?,
        Def::MemberAssign {
            member_loc: _,
            member: _,
            rhs,
        } => expression(cx, None, None, None, &rhs.1)?,
        Def::OpAssign {
            exp_loc,
            lhs,
            op,
            rhs,
            assertion,
        } => resolve_op_assign(cx, exp_loc.dupe(), &lhs.1, *assertion, *op, &rhs.1)?,
        Def::Update { exp_loc, op: _ } => {
            resolve_update(cx, id_loc.dupe(), exp_loc.dupe(), def_reason)
        }
        Def::TypeAlias(loc, alias) => resolve_type_alias(cx, loc.dupe(), alias),
        Def::OpaqueType(loc, opaque) => resolve_opaque_type(cx, loc.dupe(), opaque),
        Def::Import {
            import_kind,
            source,
            source_loc,
            import,
        } => resolve_import(
            cx,
            id_loc.dupe(),
            def_reason,
            *import_kind,
            source,
            source_loc.dupe(),
            import,
        ),
        Def::Interface(loc, inter) => resolve_interface(cx, loc.dupe(), inter),
        Def::DeclaredClass(loc, class_) => resolve_declare_class(cx, loc.dupe(), class_),
        Def::DeclaredComponent(loc, comp) => resolve_declare_component(cx, loc.dupe(), comp),
        Def::Enum(enum_loc, name, enum_) => resolve_enum(
            cx,
            id_loc.dupe(),
            def_reason,
            enum_loc.dupe(),
            name.as_str(),
            enum_,
        ),
        Def::TypeParam { .. } => resolve_type_param(cx, &id_loc),
        Def::GeneratorNext(gen_info) => resolve_generator_next(cx, def_reason, gen_info.as_ref()),
        Def::DeclaredNamespace(loc, ns) => resolve_declare_namespace(cx, loc.dupe(), ns),
        Def::MissingThisAnnot => any_t::at(AnySource::AnyError(None), id_loc.dupe()),
    };

    fn has_array_or_object_without_hint(expr: &ast::expression::Expression<ALoc, ALoc>) -> bool {
        match expr.deref() {
            ast::expression::ExpressionInner::Array { .. }
            | ast::expression::ExpressionInner::Object { .. } => true,
            ast::expression::ExpressionInner::Conditional { inner, .. } => {
                let ast::expression::Conditional {
                    consequent,
                    alternate,
                    ..
                } = inner.as_ref();
                has_array_or_object_without_hint(consequent)
                    || has_array_or_object_without_hint(alternate)
            }
            _ => false,
        }
    }
    let add_array_or_object_literal_declaration_tracking = match def {
        Def::Binding(Binding::Root(Root::ObjectValue { .. })) => true,
        Def::Binding(Binding::Root(Root::EmptyArray { .. })) => true,
        Def::Binding(Binding::Root(Root::Value(name_def_types::Value {
            hints: _,
            expr,
            decl_kind: Some(_),
            as_const: _,
        }))) if has_array_or_object_without_hint(expr) => true,
        _ => false,
    };
    flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
        vec![format!(
            "Setting variable at {} to {}",
            id_loc.debug_to_string(true),
            flow_typing_debug::dump_t(None, cx, &t),
        )]
    });
    type_env::resolve_env_entry(
        cx,
        t,
        def_kind,
        add_array_or_object_literal_declaration_tracking,
        id_loc,
    );
    Ok(())
}

fn entries_of_def(
    graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
    kind: env_api::DefLocType,
    loc: ALoc,
) -> env_api::EnvSet<ALoc> {
    let mut acc = env_api::EnvSet::empty();
    acc.insert(env_api::EnvKey::new(kind, loc.dupe()));

    fn add_from_bindings(acc: &mut env_api::EnvSet<ALoc>, b: &Binding) {
        match b {
            Binding::Root(Root::UnannotatedParameter(r)) => {
                acc.insert(env_api::EnvKey::new(
                    env_api::DefLocType::FunctionParamLoc,
                    r.loc().dupe(),
                ));
            }
            Binding::Root(Root::Annotation {
                param_loc: Some(l), ..
            }) => {
                acc.insert(env_api::EnvKey::new(
                    env_api::DefLocType::FunctionParamLoc,
                    l.dupe(),
                ));
            }
            Binding::Root(Root::Contextual { reason, .. }) => {
                let l = reason.loc().dupe();
                acc.insert(env_api::EnvKey::new(
                    env_api::DefLocType::FunctionParamLoc,
                    l,
                ));
            }
            Binding::Root(Root::FunctionValue {
                function_loc,
                arrow: false,
                function_,
                ..
            }) if function_.params.this_.is_none() => {
                acc.insert(env_api::EnvKey::new(
                    env_api::DefLocType::FunctionThisLoc,
                    function_loc.dupe(),
                ));
            }
            Binding::Root(Root::ObjectValue {
                synthesizable:
                    name_def_types::ObjectSynthKind::ObjectSynthesizable { this_write_locs },
                ..
            }) => {
                for entry in this_write_locs.iter() {
                    acc.insert(entry.dupe());
                }
            }
            Binding::Hooklike(bind) => {
                add_from_bindings(acc, bind);
            }
            Binding::Root(_) => {}
            Binding::Select { .. } => {}
        }
    }
    if let Some((def, _, _, _)) = graph.get(&env_api::EnvKey::new(kind, loc)) {
        match def {
            Def::Binding(b) => {
                add_from_bindings(&mut acc, b);
            }
            Def::Class {
                this_super_write_locs,
                ..
            } => {
                for entry in this_super_write_locs.iter() {
                    acc.insert(entry.dupe());
                }
            }
            Def::Record {
                this_super_write_locs,
                ..
            } => {
                for entry in this_super_write_locs.iter() {
                    acc.insert(entry.dupe());
                }
            }
            Def::Function {
                has_this_def: true,
                function_loc,
                function_,
                ..
            } if function_.params.this_.is_none() => {
                acc.insert(env_api::EnvKey::new(
                    env_api::DefLocType::FunctionThisLoc,
                    function_loc.dupe(),
                ));
            }
            _ => {}
        }
    }
    acc
}

fn entries_of_component(
    graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
    component: &name_def_ordering::OrderingResult,
) -> env_api::EnvSet<ALoc> {
    fn element_key(elt: &name_def_ordering::Element) -> (env_api::DefLocType, ALoc) {
        match elt {
            name_def_ordering::Element::Normal(key)
            | name_def_ordering::Element::Resolvable(key) => (key.def_loc_type, key.loc.dupe()),
            name_def_ordering::Element::Illegal(blame) => {
                (blame.payload.def_loc_type, blame.payload.loc.dupe())
            }
        }
    }

    match component {
        name_def_ordering::OrderingResult::Singleton(elt) => {
            let (kind, loc) = element_key(elt);
            entries_of_def(graph, kind, loc)
        }
        name_def_ordering::OrderingResult::ResolvableSCC(elts) => {
            let mut acc = env_api::EnvSet::empty();
            for elt in elts.iter() {
                let (kind, loc) = element_key(elt);
                for entry in entries_of_def(graph, kind, loc).iter() {
                    acc.insert(entry.dupe());
                }
            }
            acc
        }
        name_def_ordering::OrderingResult::IllegalSCC(elts) => {
            let mut acc = env_api::EnvSet::empty();
            for (blame, _) in elts.iter() {
                let (kind, loc) = element_key(&blame.payload);
                for entry in entries_of_def(graph, kind, loc).iter() {
                    acc.insert(entry.dupe());
                }
            }
            acc
        }
    }
}

fn init_type_param(
    cx: &Context,
    graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
    def_loc: ALoc,
) -> (SubstName, type_::TypeParam, Type) {
    fn get_type_param(
        cx: &Context,
        graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
        l: ALoc,
    ) -> (SubstName, type_::TypeParam, Type) {
        let cached = {
            let env = cx.environment();
            env.tparams.get_ordinary(&l).map(|entry| entry.dupe())
        };
        match cached {
            Some(entry) => entry,
            None => init_type_param(cx, graph, l),
        }
    }

    fn mk_tparams_map_from_graph(
        cx: &Context,
        graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
        tparams_map: &FlowOrdMap<ALoc, FlowSmolStr>,
    ) -> FlowOrdMap<SubstName, Type> {
        let mut subst_map = FlowOrdMap::new();
        for (l, _) in tparams_map.iter() {
            let (name, _, ty) = get_type_param(cx, graph, l.dupe());
            subst_map.insert(name, ty);
        }
        subst_map
    }

    let (def, _, _, reason) = graph
        .get_ordinary(&def_loc)
        .expect("init_type_param: def_loc not found in graph");
    let tparam_entry = match def {
        Def::TypeParam {
            tparams_map: tparams_locs,
            kind,
            tparam,
        } => {
            let tparams_map = mk_tparams_map_from_graph(cx, graph, tparams_locs);
            let (_, tparam_inner) = tparam;
            let info = type_annotation::mk_type_param(cx, tparams_map, *kind, tparam_inner);
            let cache = cx.node_cache();
            let (_, ref tparam_result, ref t) = info;
            let result = (tparam_result.name.dupe(), tparam_result.dupe(), t.dupe());
            cache.set_tparam(info);
            result
        }
        Def::Class { class_loc, .. } => {
            let self_ = type_env::read_class_self_type(cx, class_loc.dupe());
            let (this_param, this_t) =
                flow_typing_statement::class_sig::mk_this(self_, cx, reason.dupe());
            (
                SubstName::name(FlowSmolStr::new("this")),
                this_param,
                this_t,
            )
        }
        Def::Record { record_loc, .. } => {
            let self_ = type_env::read_class_self_type(cx, record_loc.dupe());
            let (this_param, this_t) =
                flow_typing_statement::class_sig::mk_this(self_, cx, reason.dupe());
            (
                SubstName::name(FlowSmolStr::new("this")),
                this_param,
                this_t,
            )
        }
        _ => {
            panic!(
                "tparam_locs contain a non-tparam location: {:?}",
                def_loc.debug_to_string(true)
            )
        }
    };
    cx.environment_mut()
        .tparams
        .insert(env_api::EnvKey::ordinary(def_loc), tparam_entry.dupe());
    tparam_entry
}

fn resolve_component_type_params(
    cx: &Context,
    graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
    component: &name_def_ordering::OrderingResult,
) {
    let resolve_illegal =
        |loc: ALoc, def: &(Def, ScopeKind, name_def_types::ClassStack, Reason)| {
            let (def, _, _, _) = def;
            match def {
                Def::TypeParam {
                    tparam: (_, tparam),
                    ..
                } => {
                    let name_loc = tparam.name.loc.dupe();
                    let str_name = tparam.name.name.dupe();
                    let name = SubstName::name(str_name.dupe());
                    let tp_reason = reason::mk_annot_reason(
                        reason::VirtualReasonDesc::RType(reason::Name::new(str_name)),
                        name_loc.dupe(),
                    );
                    let tp = type_::TypeParam::new(type_::TypeParamInner {
                        reason: tp_reason.dupe(),
                        name: name.dupe(),
                        bound: Type::new(TypeInner::DefT(
                            tp_reason.dupe(),
                            DefT::new(DefTInner::MixedT(type_::MixedFlavor::MixedEverything)),
                        )),
                        polarity: flow_common::polarity::Polarity::Neutral,
                        default: None,
                        is_this: false,
                        is_const: false,
                    });
                    cx.environment_mut().tparams.insert(
                        env_api::EnvKey::ordinary(loc.dupe()),
                        (name, tp, any_t::at(AnySource::AnyError(None), loc)),
                    );
                }
                Def::Class { .. } => {
                    let name = SubstName::name(FlowSmolStr::new("this"));
                    let tp_reason =
                        reason::mk_annot_reason(reason::VirtualReasonDesc::RThis, loc.dupe());
                    let tp = type_::TypeParam::new(type_::TypeParamInner {
                        reason: tp_reason.dupe(),
                        name: name.dupe(),
                        bound: Type::new(TypeInner::DefT(
                            tp_reason.dupe(),
                            DefT::new(DefTInner::MixedT(type_::MixedFlavor::MixedEverything)),
                        )),
                        polarity: flow_common::polarity::Polarity::Neutral,
                        default: None,
                        is_this: true,
                        is_const: false,
                    });
                    cx.environment_mut().tparams.insert(
                        env_api::EnvKey::ordinary(loc.dupe()),
                        (name, tp, any_t::at(AnySource::AnyError(None), loc)),
                    );
                }
                Def::Record { .. } => {
                    let name = SubstName::name(FlowSmolStr::new("this"));
                    let tp_reason =
                        reason::mk_annot_reason(reason::VirtualReasonDesc::RThis, loc.dupe());
                    let tp = type_::TypeParam::new(type_::TypeParamInner {
                        reason: tp_reason.dupe(),
                        name: name.dupe(),
                        bound: Type::new(TypeInner::DefT(
                            tp_reason.dupe(),
                            DefT::new(DefTInner::MixedT(type_::MixedFlavor::MixedEverything)),
                        )),
                        polarity: flow_common::polarity::Polarity::Neutral,
                        default: None,
                        is_this: true,
                        is_const: false,
                    });
                    cx.environment_mut().tparams.insert(
                        env_api::EnvKey::ordinary(loc.dupe()),
                        (name, tp, any_t::at(AnySource::AnyError(None), loc)),
                    );
                }
                _ => {}
            }
        };

    let resolve_element = |elt: &name_def_ordering::Element| match elt {
        name_def_ordering::Element::Illegal(blame) => {
            let key = &blame.payload;
            let loc = key.loc.dupe();
            if let Some(def_data) = graph.get(key) {
                resolve_illegal(loc, def_data);
            }
        }
        name_def_ordering::Element::Normal(key) | name_def_ordering::Element::Resolvable(key) => {
            if let Some((def, _, _, _)) = graph.get(key)
                && let Def::TypeParam { .. } | Def::Class { .. } | Def::Record { .. } = def
            {
                init_type_param(cx, graph, key.loc.dupe());
            }
        }
    };

    match component {
        name_def_ordering::OrderingResult::IllegalSCC(elts) => {
            for (blame, _) in elts.iter() {
                let key = match &blame.payload {
                    name_def_ordering::Element::Illegal(inner_blame) => &inner_blame.payload,
                    name_def_ordering::Element::Normal(key)
                    | name_def_ordering::Element::Resolvable(key) => key,
                };
                let loc = key.loc.dupe();
                if let Some(def_data) = graph.get(key) {
                    resolve_illegal(loc, def_data);
                }
            }
        }
        name_def_ordering::OrderingResult::Singleton(elt) => resolve_element(elt),
        name_def_ordering::OrderingResult::ResolvableSCC(elts) => {
            for elt in elts.iter() {
                resolve_element(elt);
            }
        }
    }
}

pub fn resolve_component(
    cx: &Context,
    graph: &EnvMap<ALoc, (Def, ScopeKind, name_def_types::ClassStack, Reason)>,
    component: &name_def_ordering::OrderingResult,
) {
    cx.constraint_cache_mut().clear();
    cx.eval_repos_cache_mut().clear();

    let resolve_illegal = |entries: &env_api::EnvSet<ALoc>| {
        for entry in entries.iter() {
            let kind = entry.def_loc_type;
            let loc = entry.loc.dupe();
            type_env::resolve_env_entry(
                cx,
                any_t::at(AnySource::AnyError(None), loc.dupe()),
                kind,
                false,
                loc,
            );
        }
    };

    let resolve_element = |elt: &name_def_ordering::Element| match elt {
        name_def_ordering::Element::Illegal(blame) => {
            let key = &blame.payload;
            resolve_illegal(&entries_of_def(graph, key.def_loc_type, key.loc.dupe()));
        }
        name_def_ordering::Element::Normal(key) | name_def_ordering::Element::Resolvable(key) => {
            if let Some((def, scope_kind, class_stack, reason)) = graph.get(key) {
                flow_typing_utils::abnormal::try_with_abnormal_exn(
                    || {
                        resolve(
                            cx,
                            key.def_loc_type,
                            key.loc.dupe(),
                            def,
                            *scope_kind,
                            class_stack,
                            reason.dupe(),
                        )
                    },
                    // When there is an unhandled exception, it means that the initialization of the env slot
                    // won't be completed and will never be written in the new-env, so it's OK to do nothing.
                    |_| (),
                );
            }
        }
    };

    flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
        vec![format!(
            "Resolving component {}",
            name_def_ordering::string_of_component(graph, component),
        )]
    });

    let log_slow_to_check = |f: &dyn Fn()| {
        let slow_to_check = cx.slow_to_check_logging();
        match slow_to_check.slow_components_logging_threshold {
            Some(threshold) => {
                let start_time = std::time::Instant::now();
                f();
                let run_time = start_time.elapsed().as_secs_f64();
                if run_time > threshold {
                    eprintln!(
                        "[{}] Slow CHECK component {} ({} seconds)",
                        std::process::id(),
                        name_def_ordering::string_of_component(graph, component),
                        run_time,
                    );
                }
            }
            None => f(),
        }
    };

    let f = || {
        let entries_for_resolution = entries_of_component(graph, component);
        type_env::make_env_entries_under_resolution(cx, entries_for_resolution.dupe());

        resolve_component_type_params(cx, graph, component);

        match component {
            name_def_ordering::OrderingResult::IllegalSCC(_) => {
                resolve_illegal(&entries_for_resolution)
            }
            name_def_ordering::OrderingResult::Singleton(elt) => resolve_element(elt),
            name_def_ordering::OrderingResult::ResolvableSCC(elts) => {
                for elt in elts.iter() {
                    resolve_element(elt);
                }
            }
        }

        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec!["Finished resolving component".to_string()]
        });
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec!["Forcing all lazy tvars after resolving component".to_string()]
        });
        {
            let env = cx.environment();
            for entry in entries_for_resolution.iter() {
                let kind = entry.def_loc_type;
                let loc = entry.loc.dupe();
                if let Some(type_entry) = env.find_write(kind, loc) {
                    match type_entry.t.deref() {
                        TypeInner::OpenT(tvar) => {
                            let id = tvar.id() as i32;
                            cx.inspect_constraints(id, |root_id, constraints| {
                                if let type_::constraint::Constraints::FullyResolved(s) =
                                    constraints
                                {
                                    cx.add_post_component_tvar_forcing_state(root_id, s.clone());
                                }
                            });
                        }
                        _ => {}
                    }
                }
            }
        }
        let forcing_states = cx.post_component_tvar_forcing_states();
        for s in forcing_states.iter() {
            cx.force_fully_resolved_tvar(s);
        }
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec!["Forced all lazy tvars after resolving component".to_string()]
        });
    };

    log_slow_to_check(&f);
}
