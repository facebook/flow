/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_common::reason::Reason;
use flow_typing_context::Cx;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::flow_js_utils::callee_recorder;
use flow_typing_flow_js_env::FlowJsEnv;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrRestTData;
use flow_typing_type::type_::BindTData;
use flow_typing_type::type_::CallAction;
use flow_typing_type::type_::CallElemTData;
use flow_typing_type::type_::CallMData;
use flow_typing_type::type_::CallTData;
use flow_typing_type::type_::ChainMData;
use flow_typing_type::type_::CondTData;
use flow_typing_type::type_::ConditionalTData;
use flow_typing_type::type_::Cont;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::ElemAction;
use flow_typing_type::type_::ElemTData;
use flow_typing_type::type_::GetElemTData;
use flow_typing_type::type_::GetEnumTData;
use flow_typing_type::type_::GetTypeFromNamespaceTData;
use flow_typing_type::type_::LookupAction;
use flow_typing_type::type_::LookupActionMatchPropData;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::MapTypeTData;
use flow_typing_type::type_::MethodAction;
use flow_typing_type::type_::MethodTData;
use flow_typing_type::type_::MixedFlavor;
use flow_typing_type::type_::OptionalIndexedAccessTData;
use flow_typing_type::type_::ReactKitTData;
use flow_typing_type::type_::ReadElemData;
use flow_typing_type::type_::ReadPropData;
use flow_typing_type::type_::ResolveSpreadTData;
use flow_typing_type::type_::ResolveSpreadsToMultiflowPartialData;
use flow_typing_type::type_::ResolveUnionTData;
use flow_typing_type::type_::SealGenericTData;
use flow_typing_type::type_::SetElemTData;
use flow_typing_type::type_::SpecializeTData;
use flow_typing_type::type_::SpecializedCallee;
use flow_typing_type::type_::SpreadResolve;
use flow_typing_type::type_::TestPropTData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::ValueToTypeReferenceTData;
use flow_typing_type::type_::WriteElemData;
use flow_typing_type::type_::WritePropData;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::inter_rep;
use flow_typing_type::type_::react;
use flow_typing_type::type_::unknown_use;

/// Resolves the outputs of `u` to `any`, flowing with `flow`, which runs with `cx`.
pub fn default_resolve_touts<C>(
    cx: &mut C,
    flow: &dyn Fn(&mut C, &Type, &UseT<Cx>) -> Result<(), FlowJsException>,
    resolve_callee: Option<&(Reason, Vec<Type>)>,
    env: &FlowJsEnv,
    loc: ALoc,
    u: &UseT<Cx>,
) -> Result<(), FlowJsException> {
    let any = any_t::at(AnySource::AnyError(None), loc.dupe());
    let resolve =
        |cx: &mut C, t: Type| flow(cx, &any, &UseT::new(UseTInner::UseT(unknown_use(), t)));
    let resolve_tvar = |cx: &mut C, t: &Tvar| resolve(cx, Type::new(TypeInner::OpenT(t.dupe())));
    let map_opt = |cx: &mut C, t: &Option<Type>| -> Result<(), FlowJsException> {
        if let Some(t) = t {
            resolve(cx, t.dupe())?;
        }
        Ok(())
    };
    let resolve_specialized_callee = |specialized_callee: &Option<SpecializedCallee>| {
        let resolve_callee_t = match resolve_callee {
            None => any.dupe(),
            Some((r, ts)) if ts.is_empty() => Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
            )),
            Some((_, ts)) if ts.len() == 1 => ts[0].dupe(),
            Some((r, ts)) => {
                let mut iter = ts.iter();
                let t1 = iter.next().unwrap().dupe();
                let t2 = iter.next().unwrap().dupe();
                let rest: Rc<[Type]> = iter.duped().collect();
                Type::new(TypeInner::IntersectionT(
                    r.dupe(),
                    inter_rep::make(t1, t2, rest),
                ))
            }
        };
        callee_recorder::add_callee(
            env,
            callee_recorder::Kind::All,
            resolve_callee_t,
            specialized_callee.as_ref(),
        );
    };
    let resolve_method_action =
        |cx: &mut C, action: &MethodAction<Cx>| -> Result<(), FlowJsException> {
            match action {
                MethodAction::ChainM(box ChainMData {
                    exp_reason: _,
                    lhs_reason: _,
                    lhs_expression: _,
                    methodcalltype,
                    voided_out_collector: _,
                    return_hint: _,
                    specialized_callee,
                }) => {
                    resolve_tvar(cx, &methodcalltype.meth_tout)?;
                    resolve_specialized_callee(specialized_callee);
                    Ok(())
                }
                MethodAction::CallM(box CallMData {
                    methodcalltype,
                    return_hint: _,
                    specialized_callee,
                }) => {
                    resolve_tvar(cx, &methodcalltype.meth_tout)?;
                    resolve_specialized_callee(specialized_callee);
                    Ok(())
                }
                MethodAction::NoMethodAction(tout) => {
                    tout.add(any.dupe());
                    Ok(())
                }
            }
        };
    let resolve_lookup_action = |cx: &mut C,
                                 action: &LookupAction|
     -> Result<(), FlowJsException> {
        match action {
            LookupAction::ReadProp(box ReadPropData { tout, .. }) => resolve_tvar(cx, tout),
            LookupAction::WriteProp(box WritePropData { prop_tout, .. }) => map_opt(cx, prop_tout),
            LookupAction::LookupPropForTvarPopulation { .. }
            | LookupAction::LookupPropsForSubtyping(..)
            | LookupAction::SuperProp(..)
            | LookupAction::MatchProp(box LookupActionMatchPropData { .. }) => Ok(()),
        }
    };
    let resolve_elem_action =
        |cx: &mut C, action: &ElemAction<Cx>| -> Result<(), FlowJsException> {
            match action {
                ElemAction::ReadElem(box ReadElemData { tout, .. }) => resolve_tvar(cx, tout),
                ElemAction::WriteElem(box WriteElemData { tout, .. }) => map_opt(cx, tout),
                ElemAction::CallElem(_, action) => resolve_method_action(cx, action),
            }
        };
    let resolve_react_tool = |cx: &mut C, tool: &react::Tool<Cx>| -> Result<(), FlowJsException> {
        match tool {
            react::Tool::CreateElement(box react::CreateElementData { tout, .. }) => {
                resolve_tvar(cx, tout)
            }
            react::Tool::ConfigCheck { props } => resolve(cx, props.dupe()),
            react::Tool::GetConfig { tout } => resolve(cx, tout.dupe()),
        }
    };
    let resolve_spread_resolve = |cx: &mut C,
                                  resolve_tool: &SpreadResolve|
     -> Result<(), FlowJsException> {
        match resolve_tool {
            SpreadResolve::ResolveSpreadsToTupleType { tout, .. } => resolve(cx, tout.dupe()),
            SpreadResolve::ResolveSpreadsToArrayLiteral { tout, .. } => resolve(cx, tout.dupe()),
            SpreadResolve::ResolveSpreadsToArray(_, t) => resolve(cx, t.dupe()),
            SpreadResolve::ResolveSpreadsToMultiflowCallFull(..) => Ok(()),
            SpreadResolve::ResolveSpreadsToMultiflowPartial(
                box ResolveSpreadsToMultiflowPartialData(_, _, _, t),
            ) => resolve(cx, t.dupe()),
            SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(..) => Ok(()),
        }
    };
    let resolve_cont = |cx: &mut C, cont: &Cont<Cx>| -> Result<(), FlowJsException> {
        match cont {
            Cont::Upper(use_) => {
                default_resolve_touts(cx, flow, resolve_callee, env, loc.dupe(), use_)
            }
            Cont::Lower(..) => Ok(()),
            Cont::Collect(collector) => {
                collector.add(any.dupe());
                Ok(())
            }
        }
    };
    match u.deref() {
        UseTInner::UseT(..) => Ok(()),
        UseTInner::BindT(box BindTData {
            funcall_type: funcalltype,
            ..
        }) => resolve_tvar(cx, &funcalltype.call_tout),
        UseTInner::CallT(box CallTData {
            call_action: box CallAction::Funcalltype(box funcalltype),
            ..
        }) => {
            resolve_tvar(cx, &funcalltype.call_tout)?;
            resolve_specialized_callee(&funcalltype.call_specialized_callee);
            Ok(())
        }
        UseTInner::CallT(box CallTData {
            call_action: box CallAction::ConcretizeCallee(tout),
            ..
        }) => {
            tout.add(any.dupe());
            Ok(())
        }
        UseTInner::ConditionalT(box ConditionalTData { tout, .. }) => resolve_tvar(cx, tout),
        UseTInner::MethodT(box MethodTData {
            method_action: action,
            ..
        }) => resolve_method_action(cx, action),
        UseTInner::PrivateMethodT(data) => resolve_method_action(cx, &data.method_action),
        UseTInner::SetPropT(_, _, _, _, _, _, topt) => map_opt(cx, topt),
        UseTInner::SetPrivatePropT(data) => map_opt(cx, &data.tout),
        UseTInner::GetTypeFromNamespaceT(box GetTypeFromNamespaceTData { tout: tvar, .. })
        | UseTInner::TestPropT(box TestPropTData { tout: tvar, .. }) => resolve_tvar(cx, tvar),
        UseTInner::GetPrivatePropT(data) => resolve_tvar(cx, &data.tout),
        UseTInner::GetPropT(data) => resolve_tvar(cx, &data.tout),
        UseTInner::SetElemT(box SetElemTData { tout: topt, .. }) => map_opt(cx, topt),
        UseTInner::GetElemT(box GetElemTData { tout, .. }) => resolve_tvar(cx, tout),
        UseTInner::CallElemT(box CallElemTData {
            method_action: action,
            ..
        }) => resolve_method_action(cx, action),
        UseTInner::GetStaticsT(_, collector) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::GetProtoT(_, collector) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::SetProtoT(..) => Ok(()),
        UseTInner::ReposLowerT { .. } | UseTInner::ReposUseT(..) => Ok(()),
        UseTInner::ConstructorT(data) => resolve(cx, data.tout.dupe()),
        UseTInner::SuperT(..) => Ok(()),
        UseTInner::ImplementsT(..) => Ok(()),
        UseTInner::MixinT(_, collector) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::ToStringT { t_out, .. } => {
            default_resolve_touts(cx, flow, resolve_callee, env, loc.dupe(), t_out)
        }
        UseTInner::SpecializeT(box SpecializeTData { upper, .. }) => match upper {
            Some(box Cont::Upper(upper)) => flow(cx, &any, upper),
            Some(box Cont::Collect(collector)) => {
                collector.add(any.dupe());
                Ok(())
            }
            Some(box Cont::Lower(..)) | None => Ok(()),
        },
        UseTInner::ThisSpecializeT(_, _, k) => resolve_cont(cx, k),
        UseTInner::ValueToTypeReferenceT(box ValueToTypeReferenceTData { tout: tvar, .. }) => {
            resolve_tvar(cx, tvar)
        }
        UseTInner::ConcretizeTypeAppsT(..) => Ok(()),
        UseTInner::LookupT(box LookupTData { lookup_action, .. }) => {
            resolve_lookup_action(cx, lookup_action)
        }
        UseTInner::ObjRestT(_, _, t, _) | UseTInner::ObjTestT(_, _, t) => resolve(cx, t.dupe()),
        UseTInner::ArrRestT(box ArrRestTData { tout: t, .. }) => resolve(cx, t.dupe()),
        UseTInner::ObjTestProtoT(_, collector) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::GetDictValuesT(_, use_) => {
            default_resolve_touts(cx, flow, resolve_callee, env, loc.dupe(), use_)
        }
        UseTInner::GetKeysT { t_out, .. } | UseTInner::GetKeysDictKeyT { t_out, .. } => {
            default_resolve_touts(cx, flow, resolve_callee, env, loc.dupe(), t_out)
        }
        UseTInner::HasOwnPropT(..) => Ok(()),
        UseTInner::GetValuesT(_, t) => resolve(cx, t.dupe()),
        UseTInner::ElemT(box ElemTData { action, .. }) => resolve_elem_action(cx, action),
        UseTInner::MapTypeT(box MapTypeTData { tout: t, .. })
        | UseTInner::ObjKitT(_, _, _, _, t) => resolve(cx, t.dupe()),
        UseTInner::ReactKitT(box ReactKitTData { tool, .. }) => resolve_react_tool(cx, tool),
        UseTInner::ConcretizeT(..) => Ok(()),
        UseTInner::ResolveSpreadT(box ResolveSpreadTData {
            resolve_spread_type,
            ..
        }) => resolve_spread_resolve(cx, &resolve_spread_type.rrt_resolve_to),
        UseTInner::CondT(box CondTData { collector, .. }) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::ExtendsUseT(..) => Ok(()),
        UseTInner::ResolveUnionT(box ResolveUnionTData { upper, .. }) => {
            default_resolve_touts(cx, flow, resolve_callee, env, loc.dupe(), upper)
        }
        UseTInner::GetEnumT(box GetEnumTData { collector, .. }) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::HooklikeT(tvar) | UseTInner::DeepReadOnlyT(tvar, _) => resolve_tvar(cx, tvar),
        UseTInner::FilterOptionalT(_, t) | UseTInner::FilterMaybeT(_, t) => resolve(cx, t.dupe()),
        UseTInner::SealGenericT(box SealGenericTData { cont, .. }) => resolve_cont(cx, cont),
        UseTInner::OptionalIndexedAccessT(box OptionalIndexedAccessTData { tout_tvar, .. }) => {
            resolve_tvar(cx, tout_tvar)
        }
        UseTInner::CheckUnusedPromiseT { .. } => Ok(()),
        UseTInner::ConvertEmptyPropsToMixedT(_, collector) => {
            collector.add(any.dupe());
            Ok(())
        }
        UseTInner::ExitRendersT {
            renders_reason: _,
            u,
        } => default_resolve_touts(cx, flow, resolve_callee, env, loc, u),
        UseTInner::EvalTypeDestructorT(..) => Ok(()),
    }
}
