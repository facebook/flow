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
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_common::reason::react_element_desc_of_component_reason;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::intermediate_error_types::ExpectedModulePurpose;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::flow_js_utils::callee_recorder;
use flow_typing_flow_common::obj_type;
use flow_typing_type::type_::CallMData;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::ComponentKind;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::GetPropTData;
use flow_typing_type::type_::HasOwnPropTData;
use flow_typing_type::type_::Literal;
use flow_typing_type::type_::LookupAction;
use flow_typing_type::type_::LookupKind;
use flow_typing_type::type_::LookupPropForSubtypingData;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::MethodAction;
use flow_typing_type::type_::MethodCallType;
use flow_typing_type::type_::MethodTData;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::NonstrictReturningData;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::ObjType;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::PropertyType;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ReactEffectType;
use flow_typing_type::type_::SpecializedCallee;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeDestructorT;
use flow_typing_type::type_::TypeDestructorTInner;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UnifyCause;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualRootUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::drop_generic;
use flow_typing_type::type_::eval;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::mixed_t;
use flow_typing_type::type_::object;
use flow_typing_type::type_::object::ObjectToolReactConfigData;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::react;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util;
use flow_typing_type::type_util::mk_named_prop;

use crate::flow_js::FlowJs;
use crate::renders_kit;
use crate::tvar_resolver;

pub fn err_incompatible<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: &Reason,
) -> Result<(), FlowJsException> {
    let err = ErrorMessage::ENotAReactComponent {
        reason: reason.dupe(),
        use_op,
    };
    flow_js_utils::add_output(cx, err)
}

pub fn component_class<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    props: &Type,
) -> Result<Type, FlowJsException> {
    Ok(Type::new(TypeInner::DefT(
        reason.dupe(),
        DefT::new(DefTInner::ClassT(FlowJs::get_builtin_react_typeapp(
            cx,
            reason,
            None,
            ExpectedModulePurpose::ReactModuleForReactClassComponent,
            vec![props.dupe(), flow_typing_tvar::mk(cx, reason.dupe())],
        )?)),
    )))
}

enum Artifact {
    Props,
    Instance,
}

enum IntrinsicLiteral {
    Literal(Name),
    General(Literal),
}

fn get_intrinsic<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    component_reason: &Reason,
    reason_op: &Reason,
    artifact: &Artifact,
    literal: &IntrinsicLiteral,
    prop_polarity: Polarity,
    tout: &Type,
) -> Result<(), FlowJsException> {
    let reason = component_reason;
    // Get the internal $JSXIntrinsics map.
    let intrinsics = {
        let reason = mk_reason(
            VirtualReasonDesc::RType(Name::new("$JSXIntrinsics")),
            component_reason.loc().dupe(),
        );
        FlowJs::get_builtin_type(cx, None, &reason, None, "$JSXIntrinsics")?
    };
    // Create a use_op for the upcoming operations.
    let use_op = VirtualUseOp::Op(Arc::new(VirtualRootUseOp::ReactGetIntrinsic {
        literal: match literal {
            IntrinsicLiteral::Literal(name) => reason
                .dupe()
                .replace_desc(VirtualReasonDesc::RIdentifier(name.dupe())),
            IntrinsicLiteral::General(_) => reason.dupe(),
        },
    }));
    // GetPropT with a non-literal when there is not a dictionary will propagate
    // any. Run the HasOwnPropT check to give the user an error if they use a
    // non-literal without a dictionary.
    match literal {
        IntrinsicLiteral::Literal(_) => {}
        IntrinsicLiteral::General(gen_lit) => {
            FlowJs::rec_flow(
                cx,
                trace,
                &intrinsics,
                &UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                    use_op: use_op.dupe(),
                    reason: reason.dupe(),
                    type_: Type::new(TypeInner::DefT(
                        reason.dupe(),
                        DefT::new(DefTInner::StrGeneralT(gen_lit.clone())),
                    )),
                }))),
            )?;
        }
    }

    // Get the intrinsic from the map.
    let intrinsic = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where_result(
        cx,
        reason.dupe(),
        |cx, tout_reason, tout_id| {
            let propref = match literal {
                IntrinsicLiteral::Literal(name) => {
                    let reason = reason
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RReactElement {
                            name_opt: Some(name.dupe()),
                            from_component_syntax: false,
                        });
                    mk_named_prop(reason, false, name.dupe())
                }
                IntrinsicLiteral::General(_) => PropRef::Computed(Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::StrGeneralT(Literal::AnyLiteral)),
                ))),
            };
            FlowJs::rec_flow(
                cx,
                trace,
                &intrinsics,
                &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                    use_op: use_op.dupe(),
                    reason: reason.dupe(),
                    id: None,
                    from_annot: false,
                    skip_optional: false,
                    propref: Box::new(propref),
                    tout: Box::new(Tvar::new(tout_reason.dupe(), tout_id as u32)),
                    hint: hint_unavailable(),
                }))),
            )?;
            Ok::<(), FlowJsException>(())
        },
    )?;

    // Get the artifact from the intrinsic.
    let propref = {
        let name_str = match artifact {
            Artifact::Props => "props",
            Artifact::Instance => "instance",
        };
        let name = Name::new(name_str);
        let reason = reason_op
            .dupe()
            .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe())));
        mk_named_prop(reason, false, name)
    };
    // if intrinsic is null, we will treat it like prototype termination,
    // but we should error like a GetPropT would instead.
    FlowJs::rec_flow(
        cx,
        trace,
        &intrinsic,
        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
            reason: reason_op.dupe(),
            lookup_kind: Box::new(LookupKind::Strict(reason_op.dupe())),
            try_ts_on_failure: vec![].into(),
            propref: Box::new(propref),
            lookup_action: Box::new(LookupAction::LookupPropForTvarPopulation {
                tout: tout.dupe(),
                polarity: prop_polarity,
            }),
            method_accessible: true,
            ids: Some(properties::Set::new()),
            ignore_dicts: false,
        }))),
    )
}

pub fn subtype_class_component_render<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    class_component_instance: &Type,
    reason_op: &Reason,
    upper_render: &Type,
) -> Result<(), FlowJsException> {
    let name = FlowSmolStr::new_inline("render");
    let reason_prop = reason_op
        .dupe()
        .replace_desc(VirtualReasonDesc::RMethod(Some(name.dupe())));
    let propref = mk_named_prop(reason_prop, false, Name::new(name));
    let tvar = flow_typing_tvar::mk_no_wrap(cx, reason_op);
    let action = MethodAction::CallM(Box::new(CallMData {
        methodcalltype: MethodCallType {
            meth_generic_this: None,
            meth_targs: None,
            meth_args_tlist: vec![].into(),
            meth_tout: Tvar::new(reason_op.dupe(), tvar as u32),
            meth_strict_arity: true,
        },
        return_hint: hint_unavailable(),
        specialized_callee: None,
    }));
    // Call the `render` method.
    FlowJs::rec_flow(
        cx,
        trace,
        class_component_instance,
        &UseT::new(UseTInner::MethodT(Box::new(MethodTData {
            use_op: unknown_use(),
            reason: reason_op.dupe(),
            prop_reason: reason_op.dupe(),
            propref: Box::new(propref),
            method_action: Box::new(action),
        }))),
    )?;
    FlowJs::rec_flow_t(
        cx,
        trace,
        use_op,
        &Type::new(TypeInner::OpenT(Tvar::new(reason_op.dupe(), tvar as u32))),
        upper_render,
    )
}

// Lookup the defaultProps of a component and flow with upper depending
// on the given polarity.
fn lookup_defaults<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    component: &Type,
    reason_op: &Reason,
    upper: &Type,
    pole: Polarity,
) -> Result<(), FlowJsException> {
    let name = Name::new("defaultProps");
    let reason_missing = type_util::reason_of_t(component)
        .dupe()
        .replace_desc(VirtualReasonDesc::RReactDefaultProps);
    let reason_prop = reason_op
        .dupe()
        .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe())));
    let lookup_kind = LookupKind::NonstrictReturning(Box::new(NonstrictReturningData(
        Some((
            Type::new(TypeInner::DefT(reason_missing, DefT::new(DefTInner::VoidT))),
            upper.dupe(),
        )),
        None,
    )));
    let propref = mk_named_prop(reason_prop, false, name);
    let action = LookupAction::LookupPropForTvarPopulation {
        tout: upper.dupe(),
        polarity: pole,
    };
    // Lookup the `defaultProps` property.
    FlowJs::rec_flow(
        cx,
        trace,
        component,
        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
            reason: reason_op.dupe(),
            lookup_kind: Box::new(lookup_kind),
            try_ts_on_failure: vec![].into(),
            propref: Box::new(propref),
            lookup_action: Box::new(action),
            method_accessible: true,
            ids: Some(properties::Set::new()),
            ignore_dicts: false,
        }))),
    )?;
    Ok(())
}

// Get a type for the default props of a component. If a component has no
// default props then either the type will be Some {||} or we will
// return None.
fn get_defaults<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    component: &Type,
    reason_op: &Reason,
) -> Result<Option<Type>, FlowJsException> {
    let component_non_generic = drop_generic(component.dupe());
    let TypeInner::DefT(_, def_t) = component_non_generic.deref() else {
        // Everything else will not have default props we should diff out.
        return Ok(None);
    };
    let DefTInner::ClassT(_) = def_t.deref() else {
        // Everything else will not have default props we should diff out.
        return Ok(None);
    };
    let tvar = flow_typing_tvar::mk(cx, reason_op.dupe());
    lookup_defaults(cx, trace, component, reason_op, &tvar, Polarity::Positive)?;
    Ok(Some(tvar))
}

fn add_optional_ref_prop_to_props<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    props: &Type,
    reason_op: &Reason,
    instance: &Type,
    tout: Type,
) -> Result<(), FlowJsException> {
    let ref_setter_type = FlowJs::get_builtin_react_typeapp(
        cx,
        reason_op,
        None,
        ExpectedModulePurpose::ReactModuleForReactRefSetterType,
        vec![instance.dupe()],
    )?;
    FlowJs::rec_flow(
        cx,
        trace,
        props,
        &UseT::new(UseTInner::ObjKitT(
            unknown_use(),
            reason_op.dupe(),
            Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
            Box::new(object::Tool::ReactConfig(Box::new(
                ObjectToolReactConfigData {
                    state: object::react_config::State::Config {
                        component_default_props: None,
                    },
                    ref_manipulation: object::react_config::RefManipulation::AddRef(Type::new(
                        TypeInner::OptionalT {
                            reason: reason_op.dupe(),
                            use_desc: false,
                            type_: ref_setter_type,
                        },
                    )),
                },
            ))),
            tout,
        )),
    )?;
    Ok(())
}

fn props_to_tout<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    component: &Type,
    use_op: &UseOp,
    reason_op: &Reason,
    tout: Type,
) -> Result<(), FlowJsException> {
    let dropped = drop_generic(component.dupe());
    match dropped.deref() {
        TypeInner::DefT(r, def_t) => match def_t.deref() {
            // Class components or legacy components.
            DefTInner::ClassT(i) => {
                let props = flow_typing_tvar::mk(cx, reason_op.dupe());
                FlowJs::rec_flow_t(
                    cx,
                    trace,
                    unknown_use(),
                    component,
                    &component_class(cx, r, &props)?,
                )?;
                add_optional_ref_prop_to_props(cx, trace, &props, reason_op, i, tout.dupe())?;
                Ok(())
            }
            //  Functional components.
            DefTInner::FunT(_, fun_t) => {
                match fun_t.deref() {
                    ft if ft.rest_param.is_none()
                        && match &ft.type_guard {
                            None => true,
                            Some(tg) => tg.inferred,
                        }
                        && matches!(
                            ft.effect_,
                            ReactEffectType::ArbitraryEffect | ReactEffectType::AnyEffect
                        ) =>
                    {
                        // Contravariance
                        let t = ft
                            .params
                            .first()
                            .map(|p| p.1.dupe())
                            .unwrap_or_else(|| obj_type::mk(ObjKind::Exact, cx, r.dupe()));
                        FlowJs::rec_flow_t(cx, trace, unknown_use(), &t, &tout)?;
                    }
                    _ => {
                        err_incompatible(cx, unknown_use(), r)?;
                        FlowJs::rec_flow_t(
                            cx,
                            trace,
                            unknown_use(),
                            &any_t::error(reason_op.dupe()),
                            &tout,
                        )?;
                    }
                }
                Ok(())
            }
            DefTInner::ObjT(obj_t) if obj_t.call_t.is_some() => {
                let id = obj_t.call_t.unwrap();
                let call_t = cx.find_call(id);
                if let TypeInner::DefT(_, inner_def_t) = call_t.deref()
                    && let DefTInner::FunT(_, ft) = inner_def_t.deref()
                    && ft.rest_param.is_none()
                    && match &ft.type_guard {
                        None => true,
                        Some(tg) => tg.inferred,
                    }
                {
                    // Keep the object's reason for better error reporting
                    let modified = type_util::mod_reason_of_t(&|_| r.dupe(), &call_t);
                    props_to_tout(cx, trace, &modified, use_op, reason_op, tout)?;
                } else {
                    err_incompatible(cx, unknown_use(), r)?;
                    FlowJs::rec_flow_t(
                        cx,
                        trace,
                        unknown_use(),
                        &any_t::error(reason_op.dupe()),
                        &tout,
                    )?;
                }
                Ok(())
            }
            // Special case for intrinsic components.
            DefTInner::SingletonStrT { value: name, .. } => {
                let props = flow_typing_tvar::mk(cx, reason_op.dupe());
                get_intrinsic(
                    cx,
                    trace,
                    type_util::reason_of_t(component),
                    reason_op,
                    &Artifact::Props,
                    &IntrinsicLiteral::Literal(name.dupe()),
                    Polarity::Positive,
                    &props,
                )?;
                let i = tvar_resolver::mk_tvar_and_fully_resolve_where_result(
                    cx,
                    reason_op.dupe(),
                    |cx, tout_t| {
                        get_intrinsic(
                            cx,
                            trace,
                            type_util::reason_of_t(component),
                            reason_op,
                            &Artifact::Instance,
                            &IntrinsicLiteral::Literal(name.dupe()),
                            Polarity::Positive,
                            tout_t,
                        )
                    },
                )?;
                add_optional_ref_prop_to_props(cx, trace, &props, reason_op, &i, tout)?;
                Ok(())
            }
            DefTInner::StrGeneralT(gen_lit) => {
                let props = flow_typing_tvar::mk(cx, reason_op.dupe());
                let i = tvar_resolver::mk_tvar_and_fully_resolve_where_result(
                    cx,
                    reason_op.dupe(),
                    |cx, tout_t| {
                        get_intrinsic(
                            cx,
                            trace,
                            type_util::reason_of_t(component),
                            reason_op,
                            &Artifact::Instance,
                            &IntrinsicLiteral::General(gen_lit.clone()),
                            Polarity::Positive,
                            tout_t,
                        )
                    },
                )?;
                get_intrinsic(
                    cx,
                    trace,
                    type_util::reason_of_t(component),
                    reason_op,
                    &Artifact::Props,
                    &IntrinsicLiteral::General(gen_lit.clone()),
                    Polarity::Positive,
                    &tout,
                )?;
                add_optional_ref_prop_to_props(cx, trace, &props, reason_op, &i, tout)?;
                Ok(())
            }
            DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                config, ..
            }) => {
                FlowJs::rec_flow(
                    cx,
                    trace,
                    config,
                    &UseT::new(UseTInner::ConvertEmptyPropsToMixedT(r.dupe(), tout)),
                )?;
                Ok(())
            }
            // ...otherwise, error.
            _ => {
                err_incompatible(cx, use_op.dupe(), type_util::reason_of_t(component))?;
                FlowJs::rec_flow_t(
                    cx,
                    trace,
                    unknown_use(),
                    &any_t::error(reason_op.dupe()),
                    &tout,
                )?;
                Ok(())
            }
        },
        // Any and any specializations
        TypeInner::AnyT(reason, src) => {
            FlowJs::rec_flow_t(
                cx,
                trace,
                unknown_use(),
                &any_t::why(src.clone(), reason.dupe()),
                &tout,
            )?;
            Ok(())
        }
        _ => {
            // ...otherwise, error.
            err_incompatible(cx, use_op.dupe(), type_util::reason_of_t(component))?;
            FlowJs::rec_flow_t(
                cx,
                trace,
                unknown_use(),
                &any_t::error(reason_op.dupe()),
                &tout,
            )?;
            Ok(())
        }
    }
}

// Creates the type that we expect for a React config by diffing out default
// props with ObjKitT(Rest). The config does not include types for `key`
// or `ref`.
//
// There is some duplication between the logic used here to get a config type
// and ObjKitT(ReactConfig). In create_element, we want to produce a props
// object from the config object and the defaultProps object. This way we can
// add a lower bound to components who have a type variable for props. e.g.
//
//     const MyComponent = props => null;
//     <MyComponent foo={42} />;
//
// Here, MyComponent has no annotation for props so Flow must infer a type.
// However, get_config must produce a valid type from only the component type.
//
// This approach may stall if props never gets a lower bound. Using the result
// of get_config as an upper bound won't give props a lower bound. However,
// the places in which this approach stalls are the same places as other type
// destructor annotations. Like object spread, $Diff, and $Rest.
pub fn get_config<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    component: &Type,
    use_op: UseOp,
    reason_op: &Reason,
    _u: &react::Tool<Context<'cx>>,
    pole: Polarity,
    tout: &Type,
) -> Result<(), FlowJsException> {
    let dropped = drop_generic(component.dupe());
    if let TypeInner::DefT(_, def_t) = dropped.deref()
        && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
            config, ..
        }) = def_t.deref()
    {
        let use_op = VirtualUseOp::Frame(
            Arc::new(VirtualFrameUseOp::ReactGetConfig { polarity: pole }),
            Arc::new(use_op),
        );
        match pole {
            Polarity::Positive => {
                FlowJs::rec_flow_t(cx, trace, use_op, config, tout)?;
            }
            Polarity::Negative => {
                FlowJs::rec_flow_t(cx, trace, use_op, tout, config)?;
            }
            Polarity::Neutral => {
                FlowJs::rec_unify(
                    cx,
                    trace,
                    use_op,
                    UnifyCause::Uncategorized,
                    None,
                    tout,
                    config,
                )?;
            }
        }
        return Ok(());
    }
    let reason_component = type_util::reason_of_t(component);
    let props = {
        let reason = reason_component
            .dupe()
            .update_desc(|desc| VirtualReasonDesc::RPropsOfComponent(Arc::new(desc)));
        let use_op_clone = use_op.dupe();
        flow_typing_tvar::mk_where_result(cx, reason.dupe(), |cx, tout_t| {
            props_to_tout(cx, trace, component, &use_op_clone, &reason, tout_t.dupe())
        })?
    };
    let defaults = get_defaults(cx, trace, component, reason_op)?;
    match defaults {
        None => {
            FlowJs::rec_flow(
                cx,
                trace,
                &props,
                &UseT::new(UseTInner::UseT(use_op, tout.dupe())),
            )?;
            Ok(())
        }
        Some(defaults) => {
            let tool = object::ResolveTool::Resolve(object::Resolve::Next);
            let state = object::rest::State::One(defaults);
            FlowJs::rec_flow(
                cx,
                trace,
                &props,
                &UseT::new(UseTInner::ObjKitT(
                    use_op,
                    reason_op.dupe(),
                    Box::new(tool),
                    Box::new(object::Tool::Rest(Box::new((
                        object::rest::MergeMode::ReactConfigMerge(pole),
                        state,
                    )))),
                    tout.dupe(),
                )),
            )?;
            Ok(())
        }
    }
}

// let run cx trace ~use_op reason_op l u =
pub fn run<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    l: &Type,
    u: &react::Tool<Context<'cx>>,
) -> Result<(), FlowJsException> {
    // This function creates a constraint *from* tin *to* props so that props is
    // an upper bound on tin. This is important because when the type of a
    // component's props is inferred (such as when a stateless functional
    // component has an unannotated props argument) we want to create a constraint
    // *from* the props input *to* tin which should then be propagated to the
    // inferred props type.
    fn tin_to_props<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: &UseOp,
        reason_op: &Reason,
        l: &Type,
        tin: &Type,
    ) -> Result<(), FlowJsException> {
        let component = l;
        let dropped = drop_generic(component.dupe());
        match dropped.deref() {
            TypeInner::DefT(r, def_t) => match &**def_t {
                // Class components or legacy components.
                DefTInner::ClassT(_) => {
                    let c = &dropped;
                    let props = Type::new(TypeInner::EvalT {
                        type_: c.dupe(),
                        defer_use_t: TypeDestructorT::new(TypeDestructorTInner(
                            unknown_use(),
                            reason_op.dupe(),
                            Rc::new(Destructor::ReactElementConfigType),
                        )),
                        id: eval::Id::generate_id(),
                    });
                    FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &props)?;
                    Ok(())
                }
                // Stateless functional components.
                DefTInner::FunT(_, fun_t) => {
                    match fun_t.deref() {
                        ft if ft.rest_param.is_none()
                            && matches!(
                                ft.effect_,
                                ReactEffectType::ArbitraryEffect | ReactEffectType::AnyEffect
                            ) =>
                        {
                            // Contravariance
                            let t = ft
                                .params
                                .first()
                                .map(|p| p.1.dupe())
                                .unwrap_or_else(|| obj_type::mk(ObjKind::Exact, cx, r.dupe()));
                            FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &t)?;
                            if !cx.in_implicit_instantiation() {
                                FlowJs::rec_flow_t(
                                    cx,
                                    trace,
                                    unknown_use(),
                                    &ft.return_t,
                                    &FlowJs::get_builtin_react_type(
                                        cx,
                                        None,
                                        reason_op,
                                        None,
                                        ExpectedModulePurpose::ReactModuleForReactNodeType,
                                    )?,
                                )?;
                            }
                        }
                        _ => {
                            err_incompatible(cx, unknown_use(), r)?;
                            FlowJs::rec_flow_t(
                                cx,
                                trace,
                                unknown_use(),
                                &any_t::error(reason_op.dupe()),
                                tin,
                            )?;
                        }
                    }
                    Ok(())
                }
                // Functional components, again. This time for callable `ObjT`s.
                DefTInner::ObjT(obj_t) if obj_t.call_t.is_some() => {
                    let id = obj_t.call_t.unwrap();
                    let call_t = cx.find_call(id);
                    if let TypeInner::DefT(_, inner_def_t) = call_t.deref()
                        && let DefTInner::FunT(_, ft) = inner_def_t.deref()
                        && ft.rest_param.is_none()
                        && match &ft.type_guard {
                            None => true,
                            Some(tg) => tg.inferred,
                        }
                    {
                        // Keep the object's reason for better error reporting
                        let modified = type_util::mod_reason_of_t(&|_| r.dupe(), &call_t);
                        return tin_to_props(cx, trace, use_op, reason_op, &modified, tin);
                    }
                    err_incompatible(cx, unknown_use(), r)?;
                    FlowJs::rec_flow_t(
                        cx,
                        trace,
                        unknown_use(),
                        &any_t::error(reason_op.dupe()),
                        tin,
                    )?;
                    Ok(())
                }
                // Abstract components.
                DefTInner::ReactAbstractComponentT(_) => {
                    FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &mixed_t::why(r.dupe()))?;
                    Ok(())
                }
                // Intrinsic components.
                DefTInner::SingletonStrT { .. } => {
                    let c = &dropped;
                    let props = Type::new(TypeInner::EvalT {
                        type_: c.dupe(),
                        defer_use_t: TypeDestructorT::new(TypeDestructorTInner(
                            unknown_use(),
                            reason_op.dupe(),
                            Rc::new(Destructor::ReactElementConfigType),
                        )),
                        id: eval::Id::generate_id(),
                    });
                    FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &props)?;
                    Ok(())
                }
                DefTInner::StrGeneralT(_) => {
                    let c = &dropped;
                    let props = Type::new(TypeInner::EvalT {
                        type_: c.dupe(),
                        defer_use_t: TypeDestructorT::new(TypeDestructorTInner(
                            unknown_use(),
                            reason_op.dupe(),
                            Rc::new(Destructor::ReactElementConfigType),
                        )),
                        id: eval::Id::generate_id(),
                    });
                    FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &props)?;
                    Ok(())
                }
                // ...otherwise, error.
                _ => {
                    let reason = type_util::reason_of_t(component);
                    err_incompatible(cx, use_op.dupe(), reason)?;
                    FlowJs::rec_flow_t(
                        cx,
                        trace,
                        unknown_use(),
                        tin,
                        &any_t::error(reason.dupe()),
                    )?;
                    Ok(())
                }
            },
            TypeInner::AnyT(reason, source) => {
                FlowJs::rec_flow_t(
                    cx,
                    trace,
                    unknown_use(),
                    tin,
                    &any_t::why(source.clone(), reason.dupe()),
                )?;
                Ok(())
            }
            // ...otherwise, error.
            _ => {
                let reason = type_util::reason_of_t(component);
                err_incompatible(cx, use_op.dupe(), reason)?;
                FlowJs::rec_flow_t(cx, trace, unknown_use(), tin, &any_t::error(reason.dupe()))?;
                Ok(())
            }
        }
    }

    fn config_check<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        l: &Type,
        jsx_props: &Type,
    ) -> Result<(), FlowJsException> {
        // Create a type variable for our props.
        let dropped = drop_generic(l.dupe());
        let (component_props, component_default_props) = if let TypeInner::DefT(_, def_t) =
            dropped.deref()
            && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                config,
                ..
            }) = def_t.deref()
        {
            (config.dupe(), None)
        } else {
            let use_op_clone = use_op.dupe();
            let props = flow_typing_tvar::mk_where_result(cx, reason_op.dupe(), |cx, tout_t| {
                tin_to_props(cx, trace, &use_op_clone, reason_op, l, tout_t)
            })?;
            // For class components and function components we want to lookup the
            // static default props property so that we may add it to our config input.
            let defaults = get_defaults(cx, trace, l, reason_op)?;
            (props, defaults)
        };
        let ref_manipulation = object::react_config::RefManipulation::KeepRef;

        // Use object spread to add children to config (if we have children)
        // and remove key and ref since we already checked key and ref. Finally in
        // this block we will flow the final config to our props type.
        //
        // NOTE: We don't eagerly run this check so that create_element can constrain the
        // ref and key pseudoprops before we run the config check.

        let reason = type_util::reason_of_t(jsx_props)
            .dupe()
            .replace_desc_new(VirtualReasonDesc::RReactProps);
        let use_op = VirtualUseOp::Frame(
            Arc::new(VirtualFrameUseOp::ReactConfigCheck),
            Arc::new(use_op),
        );
        // Create the final config object using the ReactConfig object kit tool
        // and flow it to our type for props.
        //
        // We wrap our use_op in a ReactConfigCheck frame to increment the
        // speculation error message score. Usually we will already have a
        // ReactCreateElementCall use_op, but we want errors after this point to
        // win when picking the best errors speculation discovered.
        FlowJs::rec_flow(
            cx,
            trace,
            jsx_props,
            &UseT::new(UseTInner::ObjKitT(
                use_op,
                reason,
                Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
                Box::new(object::Tool::ReactConfig(Box::new(
                    ObjectToolReactConfigData {
                        state: object::react_config::State::Config {
                            component_default_props,
                        },
                        ref_manipulation,
                    },
                ))),
                component_props,
            )),
        )?;
        Ok(())
    }

    fn create_element<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        original_use_op: UseOp,
        reason_op: &Reason,
        l: &Type,
        u: &react::Tool<Context<'cx>>,
        component: &Type,
        jsx_props: &Type,
        should_generalize: bool,
        record_monomorphized_result: bool,
        inferred_targs: &Option<Rc<[(Type, flow_common::subst_name::SubstName)]>>,
        specialized_component: &Option<SpecializedCallee>,
        tout: &Tvar,
    ) -> Result<(), FlowJsException> {
        // Why do we try to remove the OpaqueTypeUpperBound frame here?
        // The frame will be added when we unwrap the opaque type bound of `React$CreateElement`.
        // The error printing logic will unconditionally use the reason of `React$CreateElement`
        // tracked here to replace the actual lower bound.
        // TODO: generate reasons in a more principled way everywhere, so we don't need this hack.
        fn unwrap(use_op: UseOp) -> UseOp {
            match use_op {
                VirtualUseOp::Frame(f, inner)
                    if matches!(&*f, VirtualFrameUseOp::OpaqueTypeUpperBound { .. }) =>
                {
                    unwrap((*inner).dupe())
                }
                VirtualUseOp::Frame(f, inner) => {
                    VirtualUseOp::Frame(f, Arc::new(unwrap((*inner).dupe())))
                }
                VirtualUseOp::Op(_) => use_op,
            }
        }
        let use_op = unwrap(original_use_op);
        config_check(cx, trace, use_op.dupe(), reason_op, l, jsx_props)?;

        // If our jsx props is void or null then we want to replace it with an
        // empty object.
        //
        // NOTE: We only need the normalized config to look up the key
        // and ref.
        let normalized_jsx_props = {
            let use_op_clone = use_op.dupe();
            flow_typing_tvar::mk_where_result(
                cx,
                type_util::reason_of_t(jsx_props).dupe(),
                |cx, normalized_config| {
                    let reason = type_util::reason_of_t(jsx_props).dupe();
                    FlowJs::rec_flow(
                        cx,
                        trace,
                        jsx_props,
                        &UseT::new(UseTInner::ObjKitT(
                            use_op_clone.dupe(),
                            reason,
                            Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
                            Box::new(object::Tool::ObjectRep),
                            normalized_config.dupe(),
                        )),
                    )
                },
            )?
        };

        // Check the type of React keys in the config input.
        //
        // NOTE: We are intentionally being unsound here. If config is inexact
        // and we can't find a key prop in config then the sound thing to do
        // would be to assume that the type of key is mixed. Instead we are unsound
        // and don't check a type for key. Otherwise we would cause a lot of issues
        // in existing React code.
        {
            let reason_key = type_util::reason_of_t(&normalized_jsx_props)
                .dupe()
                .replace_desc(VirtualReasonDesc::RReactKey);
            // Create the key type.
            let key_t = type_util::optional(
                type_util::maybe(FlowJs::get_builtin_type(
                    cx,
                    None,
                    &reason_key,
                    None,
                    "React$Key",
                )?),
                None,
                false,
            );
            let lookup_kind =
                LookupKind::NonstrictReturning(Box::new(NonstrictReturningData(None, None)));
            let prop_name = Name::new("key");
            let propref = mk_named_prop(reason_key.dupe(), false, prop_name.dupe());
            let action =
                LookupAction::LookupPropForSubtyping(Box::new(LookupPropForSubtypingData {
                    use_op: use_op.dupe(),
                    prop: PropertyType::OrdinaryField {
                        type_: key_t,
                        polarity: Polarity::Positive,
                    },
                    prop_name: prop_name.dupe(),
                    reason_lower: type_util::reason_of_t(&normalized_jsx_props).dupe(),
                    reason_upper: reason_key.dupe(),
                }));
            FlowJs::rec_flow(
                cx,
                trace,
                &normalized_jsx_props,
                &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                    reason: reason_key,
                    lookup_kind: Box::new(lookup_kind),
                    try_ts_on_failure: vec![].into(),
                    propref: Box::new(propref),
                    lookup_action: Box::new(action),
                    method_accessible: true,
                    ids: Some(properties::Set::new()),
                    ignore_dicts: false,
                }))),
            )?;
        }

        let annot_loc = reason_op.loc().dupe();
        let elem_reason = {
            let desc = react_element_desc_of_component_reason(type_util::reason_of_t(l));
            reason_op
                .dupe()
                .replace_desc(desc)
                .annotate(annot_loc.dupe())
        };
        let elem = {
            let use_op_clone = use_op.dupe();
            FlowJs::get_builtin_typeapp(
                cx,
                &elem_reason,
                Some(true),
                "ExactReactElement_DEPRECATED",
                vec![
                    component.dupe(),
                    flow_typing_tvar::mk_where_result(cx, reason_op.dupe(), |cx, tout_t| {
                        get_config(
                            cx,
                            trace,
                            l,
                            use_op_clone.dupe(),
                            reason_op,
                            u,
                            Polarity::Positive,
                            tout_t,
                        )
                    })?,
                ],
            )
        };

        // Concretize to an ObjT so that we can asssociate the monomorphized component with the props id
        let elem = {
            let result = FlowJs::singleton_concrete_type_for_inspection(cx, &elem_reason, &elem)?;
            if let TypeInner::NominalT {
                reason: _,
                nominal_type: opq,
            } = result.deref()
                && let Some(upper_t) = &opq.upper_t
                && let TypeInner::DefT(super_r, def_t) = upper_t.deref()
                && let DefTInner::ObjT(obj_t) = def_t.deref()
            {
                if record_monomorphized_result {
                    let new_props_tmap =
                        cx.generate_property_map(cx.find_props(obj_t.props_tmap.dupe()));
                    let t = Type::new(TypeInner::NominalT {
                        reason: elem_reason.dupe(),
                        nominal_type: Rc::new(NominalType::new(NominalTypeInner {
                            nominal_id: opq.nominal_id.clone(),
                            underlying_t: opq.underlying_t.clone(),
                            lower_t: opq.lower_t.dupe(),
                            upper_t: Some(Type::new(TypeInner::DefT(
                                super_r.dupe(),
                                DefT::new(DefTInner::ObjT(Rc::new(ObjType {
                                    props_tmap: new_props_tmap.dupe(),
                                    flags: obj_t.flags.clone(),
                                    proto_t: obj_t.proto_t.dupe(),
                                    call_t: obj_t.call_t,
                                    reachable_targs: obj_t.reachable_targs.dupe(),
                                }))),
                            ))),
                            nominal_type_args: opq.nominal_type_args.dupe(),
                        })),
                    });
                    cx.add_monomorphized_component(new_props_tmap, l.dupe());
                    t
                } else {
                    elem
                }
            } else {
                // TODO(jmbrown): Internal Error
                elem
            }
        };

        // Record the instantiated type for hover types.
        {
            let component = l;
            let dropped = drop_generic(component.dupe());
            if let TypeInner::DefT(r, def_t) = dropped.deref() {
                match (def_t.deref(), inferred_targs) {
                    (
                        DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                            config,
                            renders,
                            component_kind: ComponentKind::Nominal(loc, name, _),
                        }),
                        Some(inferred_targs_val),
                    ) => {
                        let ts = Some(
                            inferred_targs_val
                                .iter()
                                .map(|(t, _)| t.dupe())
                                .collect::<Rc<[_]>>(),
                        );
                        let inst_component = Type::new(TypeInner::DefT(
                            r.dupe(),
                            DefT::new(DefTInner::ReactAbstractComponentT(Box::new(
                                ReactAbstractComponentTData {
                                    config: config.dupe(),
                                    renders: renders.dupe(),
                                    component_kind: ComponentKind::Nominal(
                                        loc.dupe(),
                                        name.dupe(),
                                        ts,
                                    ),
                                },
                            ))),
                        ));
                        callee_recorder::add_callee(
                            cx,
                            callee_recorder::Kind::Tast,
                            inst_component,
                            specialized_component.as_ref(),
                        );
                    }
                    (DefTInner::FunT(..), _) => {
                        let fn_t = dropped.dupe();
                        callee_recorder::add_callee(
                            cx,
                            callee_recorder::Kind::Tast,
                            fn_t,
                            specialized_component.as_ref(),
                        );
                    }
                    _ => {}
                }
            }
        }

        let elem = if should_generalize {
            match renders_kit::try_synthesize_render_type(cx, false, &elem)? {
                None => FlowJs::get_builtin_react_type(
                    cx,
                    Some(trace),
                    &elem_reason,
                    None,
                    ExpectedModulePurpose::ReactModuleForReactMixedElementType,
                )?,
                Some((renders_variant, ts)) => Type::new(TypeInner::DefT(
                    elem_reason.dupe(),
                    DefT::new(DefTInner::RendersT(Rc::new(
                        CanonicalRendersForm::StructuralRenders {
                            renders_variant,
                            renders_structural_type: type_util::union_of_ts(
                                elem_reason.dupe(),
                                ts,
                                None,
                            ),
                        },
                    ))),
                )),
            }
        } else {
            elem
        };
        FlowJs::rec_flow_t(
            cx,
            trace,
            unknown_use(),
            &elem,
            &Type::new(TypeInner::OpenT(tout.dupe())),
        )?;
        Ok(())
    }

    match u {
        react::Tool::CreateElement(box react::CreateElementData {
            component,
            jsx_props,
            tout,
            targs: _,
            should_generalize,
            return_hint: _,
            record_monomorphized_result,
            inferred_targs,
            specialized_component,
        }) => create_element(
            cx,
            trace,
            use_op,
            reason_op,
            l,
            u,
            component,
            jsx_props,
            *should_generalize,
            *record_monomorphized_result,
            inferred_targs,
            specialized_component,
            tout,
        ),
        react::Tool::ConfigCheck { props: jsx_props } => {
            config_check(cx, trace, use_op, reason_op, l, jsx_props)
        }
        react::Tool::GetConfig { tout } => {
            get_config(cx, trace, l, use_op, reason_op, u, Polarity::Positive, tout)
        }
    }
}
