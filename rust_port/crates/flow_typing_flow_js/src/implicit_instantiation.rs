/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::subst_name;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_typing_context::Context;
use flow_typing_errors::error_message::EImplicitInstantiationUnderconstrainedErrorData;
use flow_typing_errors::error_message::ETooFewTypeArgsData;
use flow_typing_errors::error_message::ETooManyTypeArgsData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_flow_common::concrete_type_eq;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::instantiation_utils;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_common::type_subst;
use flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrRestTData;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::ArrayATData;
use flow_typing_type::type_::CallAction;
use flow_typing_type::type_::CallTData;
use flow_typing_type::type_::ComponentKind;
use flow_typing_type::type_::ConstructorTData;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DestructorConditionalTypeData;
use flow_typing_type::type_::DestructorMappedTypeData;
use flow_typing_type::type_::DestructorSpreadTupleTypeData;
use flow_typing_type::type_::DestructorSpreadTypeData;
use flow_typing_type::type_::EvalTypeDestructorTData;
use flow_typing_type::type_::FieldData;
use flow_typing_type::type_::FunParam;
use flow_typing_type::type_::FunRestParam;
use flow_typing_type::type_::FuncallType;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::GetElemTData;
use flow_typing_type::type_::GetEnumKind;
use flow_typing_type::type_::GetEnumTData;
use flow_typing_type::type_::HintEvalResult;
use flow_typing_type::type_::LazyHintT;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ReactKitTData;
use flow_typing_type::type_::ReposUseTData;
use flow_typing_type::type_::ResolveSpreadTData;
use flow_typing_type::type_::ResolveUnionTData;
use flow_typing_type::type_::ResolvedParam;
use flow_typing_type::type_::SpreadResolve;
use flow_typing_type::type_::Targ;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::TypeParamInner;
use flow_typing_type::type_::UnifyCause;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::constraint;
use flow_typing_type::type_::empty_t;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::mixed_t;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::num_module_t;
use flow_typing_type::type_::object;
use flow_typing_type::type_::object::ObjectToolObjectMapData;
use flow_typing_type::type_::object::ObjectToolReactConfigData;
use flow_typing_type::type_::open_tvar;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::react;
use flow_typing_type::type_::string_of_use_ctor;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_::unsoundness;
use flow_typing_type::type_util;
use flow_typing_visitors::type_visitor;
use flow_typing_visitors::type_visitor::TypeVisitor;
use vec1::Vec1;

use crate::flow_js::FlowJs;
use crate::marked::Marked;
use crate::natural_inference;
use crate::speculation_kit;

fn union_flatten_list(ts: impl IntoIterator<Item = Type>) -> Vec<Type> {
    ts.into_iter().flat_map(union_flatten).collect()
}

fn union_flatten(t: Type) -> Vec<Type> {
    match t.deref() {
        TypeInner::UnionT(_, rep) => union_flatten_list(rep.members_iter().duped()),
        _ => vec![t],
    }
}

// Makes sure the solution does not "leak" a potentially overly precise type to
// the output of a generic call, when this precision is not necessary for checking
// the call. Condider for example
//
// ```js
// declare function useState<S>(init: S): [S, (S)=>void];
// const [st, setSt] = useState(42);
// ```
//
// We would like the type of `st` and `setSt` to use the general form of the type
// for the initial state `42`, i.e. number (resp. `(number)=>void`), instead of
// the rather impractical `42` (resp. `(42)=>void`).
fn generalize_singletons<'cx>(
    cx: &Context<'cx>,
    call_loc: &flow_aloc::ALoc,
    has_syntactic_hint: bool,
    t: Type,
) -> Type {
    let needs_precise = has_syntactic_hint
        || cx.get_enclosing_context_for_call(call_loc) == Some(EnclosingContext::IndexContext);
    if needs_precise {
        t
    } else {
        let singleton_action = |loc: &flow_aloc::ALoc| {
            let checked = cx.is_primitive_literal_checked(loc);
            if checked {
                natural_inference::SingletonAction::KeepAsIs
            } else {
                natural_inference::SingletonAction::DoNotKeep
            }
        };
        natural_inference::convert_implicit_instantiation_literal_type(cx, singleton_action, t)
    }
}

#[derive(Debug, Clone)]
pub struct InferredTarg {
    pub tparam: TypeParam,
    pub inferred: Type,
}

impl InferredTarg {
    pub fn to_type(&self) -> Type {
        self.inferred.dupe()
    }
}

#[derive(Debug, Clone)]
pub struct GeneralizedTarg {
    pub tparam: TypeParam,
    pub inferred: Type,
    pub generalized: Type,
}

impl GeneralizedTarg {
    pub fn to_type(&self) -> Type {
        self.generalized.dupe()
    }
}

trait Observer {
    fn on_pinned_tparam<'cx>(cx: &Context<'cx>, tparam: &TypeParam, inferred: Type)
    -> InferredTarg;
    fn on_constant_tparam_missing_bounds<'cx>(
        cx: &Context<'cx>,
        tparam: &TypeParam,
    ) -> Result<InferredTarg, FlowJsException>;
    fn on_missing_bounds<'cx>(
        cx: &Context<'cx>,
        use_op: &UseOp,
        tparam: &TypeParam,
        tparam_binder_reason: &Reason,
        instantiation_reason: &Reason,
    ) -> Result<InferredTarg, FlowJsException>;
    fn on_upper_non_t<'cx>(
        cx: &Context<'cx>,
        use_op: &UseOp,
        u: &UseT<Context<'cx>>,
        tparam: &TypeParam,
        tparam_binder_reason: &Reason,
        instantiation_reason: &Reason,
    ) -> Result<InferredTarg, FlowJsException>;
}

fn get_t<'cx>(cx: &Context<'cx>, t: &Type) -> Type {
    let no_lowers = |_cx: &Context, r: &Reason| unsoundness::merged_any(r.dupe());
    match t.deref() {
        TypeInner::OpenT(tvar) => {
            let r = tvar.reason();
            let id = tvar.id() as i32;
            flow_js_utils::merge_tvar(cx, false, no_lowers, r, id)
        }
        _ => t.dupe(),
    }
}

// Sorting of the upper bounds is mostly done for compatibility with the version
// before EvalTypeDestructorT was a use_t.
fn sort_upper_bounds_for_merging<CX>(
    xs: Vec<(UseT<CX>, DepthTrace)>,
) -> Vec<(UseT<CX>, DepthTrace)> {
    let (use_t, rest_t): (Vec<_>, Vec<_>) = xs.into_iter().partition(|(u, _): &(UseT<CX>, _)| {
        matches!(
            u.deref(),
            UseTInner::UseT(_, _) | UseTInner::EvalTypeDestructorT { .. }
        )
    });
    use_t.into_iter().chain(rest_t).collect()
}

fn speculative_subtyping_succeeds<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    l: &Type,
    u: &Type,
) -> Result<bool, flow_utils_concurrency::job_error::JobError> {
    match speculation_kit::try_singleton_throw_on_failure(
        cx,
        trace,
        l.dupe(),
        UseT::new(UseTInner::UseT(use_op, u.dupe())),
    ) {
        Ok(()) => Ok(true),
        Err(FlowJsException::SpeculationSingletonError) => Ok(false),
        // WorkerCanceled, TimedOut, and DebugThrow must propagate past
        // speculation; see plan.md §"JobError — unified error type for cancel +
        // timeout".
        Err(FlowJsException::WorkerCanceled(c)) => {
            Err(flow_utils_concurrency::job_error::JobError::Canceled(c))
        }
        Err(FlowJsException::TimedOut(t)) => {
            Err(flow_utils_concurrency::job_error::JobError::TimedOut(t))
        }
        Err(FlowJsException::DebugThrow { loc }) => {
            Err(flow_utils_concurrency::job_error::JobError::DebugThrow { loc })
        }
        // Other exceptions (LimitExceeded, Speculative) propagate in OCaml;
        // here we treat them as false since the return type is bool
        Err(FlowJsException::Speculative(_)) | Err(FlowJsException::LimitExceeded) => Ok(false),
    }
}

struct ImplicitInstantiationVisitor<'a, 'cx> {
    tparams_map: &'a BTreeMap<SubstName, TypeParam>,
    cx: &'a Context<'cx>,
}

impl TypeVisitor<(Marked<SubstName>, FlowOrdSet<SubstName>)>
    for ImplicitInstantiationVisitor<'_, '_>
{
    fn type_<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        acc: (Marked<SubstName>, FlowOrdSet<SubstName>),
        t: &Type,
    ) -> (Marked<SubstName>, FlowOrdSet<SubstName>) {
        let (mut marked, tparam_names) = acc;
        match t.deref() {
            TypeInner::GenericT(box GenericTData { name: s, .. }) => {
                if tparam_names.contains(s) {
                    match marked.add(s.dupe(), pole) {
                        None => (marked, tparam_names),
                        Some(_) => match self.tparams_map.get(s) {
                            None => (marked, tparam_names),
                            Some(tparam) => {
                                let bound = tparam.bound.dupe();
                                self.type_(cx, pole, (marked, tparam_names), &bound)
                            }
                        },
                    }
                } else {
                    type_visitor::type_default(self, cx, pole, (marked, tparam_names), t)
                }
            }
            // We remove any tparam names from the map when entering a PolyT to avoid naming conflicts.
            TypeInner::DefT(_, def_t) => {
                if let DefTInner::PolyT(box PolyTData {
                    tparams, t_out: t, ..
                }) = def_t.deref()
                {
                    let mut tparam_names_prime = tparam_names.clone();
                    for tp in tparams.iter() {
                        tparam_names_prime.remove(&tp.name);
                    }
                    let (marked, _) = self.type_(cx, pole, (marked, tparam_names_prime), t);
                    // TODO(jmbrown): Handle defaults on type parameters
                    (marked, tparam_names)
                } else {
                    type_visitor::type_default(self, cx, pole, (marked, tparam_names), t)
                }
            }
            TypeInner::TypeAppT(box TypeAppTData { type_, targs, .. }) => {
                self.typeapp(targs, self.cx, pole, (marked, tparam_names), type_)
            }
            // ThisTypeAppT is created from a new expression, which cannot be used as an annotation,
            // so we do not special case it like we do with TypeAppT
            _ => type_visitor::type_default(self, cx, pole, (marked, tparam_names), t),
        }
    }
}

impl ImplicitInstantiationVisitor<'_, '_> {
    fn typeapp<'cx>(
        &mut self,
        targs: &[Type],
        cx: &Context<'cx>,
        pole: Polarity,
        acc: (Marked<SubstName>, FlowOrdSet<SubstName>),
        type_: &Type,
    ) -> (Marked<SubstName>, FlowOrdSet<SubstName>) {
        fn loop_<'cx>(
            visitor: &mut ImplicitInstantiationVisitor,
            cx: &Context<'cx>,
            pole: Polarity,
            mut acc: (Marked<SubstName>, FlowOrdSet<SubstName>),
            tparams: Option<&[TypeParam]>,
            targs: &[Type],
        ) -> (Marked<SubstName>, FlowOrdSet<SubstName>) {
            match (tparams, targs) {
                // Any arity erors are already handled in Flow_js
                (_, []) => acc,
                (Some([]), _) => acc,
                (None, [targ, rest @ ..]) => {
                    // In the absence of tparams we will just visit the args with a neutral polarity.
                    let param_polarity = Polarity::Neutral;
                    acc = visitor.type_(cx, param_polarity, acc, targ);
                    loop_(visitor, cx, pole, acc, None, rest)
                }
                (Some([tparam, rest_tparams @ ..]), [targ, rest_targs @ ..]) => {
                    let param_polarity = Polarity::mult(pole, tparam.polarity);
                    acc = visitor.type_(cx, param_polarity, acc, targ);
                    loop_(visitor, cx, pole, acc, Some(rest_tparams), rest_targs)
                }
            }
        }

        let resolved = get_t(cx, type_);
        match resolved.deref() {
            TypeInner::AnnotT(_, t, _) => self.typeapp(targs, cx, pole, acc, t),
            TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::PolyT(_)) => {
                if let DefTInner::PolyT(box PolyTData { tparams, .. }) = def_t.deref() {
                    loop_(self, cx, pole, acc, Some(tparams), targs)
                } else {
                    unreachable!()
                }
            }
            TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::EmptyT) => {
                loop_(self, cx, pole, acc, None, targs)
            }
            TypeInner::AnyT(..) => loop_(self, cx, pole, acc, None, targs),
            _ => loop_(self, cx, pole, acc, None, targs),
        }
    }
}

enum UseTResult<'cx> {
    UpperEmpty,
    UpperNonT(UseT<Context<'cx>>),
    UpperT(Type),
}

// and rec t_of_use_t cx seen tvar u =
fn t_of_use_t<'cx>(
    cx: &Context<'cx>,
    seen: &mut BTreeSet<i32>,
    tvar: &Type,
    u: &UseT<Context<'cx>>,
) -> Result<UseTResult<'cx>, FlowJsException> {
    fn use_t_result_of_t_option<'a>(
        opt: Result<Option<Type>, FlowJsException>,
    ) -> Result<UseTResult<'a>, FlowJsException> {
        match opt? {
            Some(t) => Ok(UseTResult::UpperT(t)),
            None => Ok(UseTResult::UpperEmpty),
        }
    }
    fn merge_lower_or_upper_bounds<'cx>(
        cx: &Context<'cx>,
        seen: &mut BTreeSet<i32>,
        t: &Type,
    ) -> Result<UseTResult<'cx>, FlowJsException> {
        match merge_lower_bounds(cx, t)? {
            Some(t) => Ok(UseTResult::UpperT(t)),
            None => merge_upper_bounds(cx, seen, t),
        }
    }
    fn bind_use_t_result<'a>(
        result: UseTResult<'a>,
        f: &dyn Fn(Type) -> Result<UseTResult<'a>, FlowJsException>,
    ) -> Result<UseTResult<'a>, FlowJsException> {
        match result {
            UseTResult::UpperEmpty => Ok(UseTResult::UpperEmpty),
            UseTResult::UpperNonT(u) => Ok(UseTResult::UpperNonT(u)),
            UseTResult::UpperT(t) => f(t),
        }
    }

    match u.deref() {
        UseTInner::UseT(_, t) if let TypeInner::OpenT(_) = t.deref() => {
            merge_upper_bounds(cx, seen, t)
        }
        UseTInner::UseT(_, t) => Ok(UseTResult::UpperT(t.dupe())),
        UseTInner::EvalTypeDestructorT(box EvalTypeDestructorTData {
            reason: r,
            destructor,
            tout,
            ..
        }) => {
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            match destructor {
                box Destructor::PropertyType { .. }
                | box Destructor::ElementType { .. }
                | box Destructor::OptionalIndexedAccessNonMaybeType { .. }
                | box Destructor::OptionalIndexedAccessResultType { .. }
                | box Destructor::ValuesType
                | box Destructor::ConditionalType(box DestructorConditionalTypeData { .. })
                | box Destructor::TypeMap(_)
                | box Destructor::MappedType(box DestructorMappedTypeData { .. }) => {
                    // Mapped Type reversals
                    Ok(UseTResult::UpperEmpty)
                }
                box Destructor::EnumType => {
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    bind_use_t_result(result, &|tout_val: Type| {
                        let result_t = flow_typing_tvar::mk_no_wrap_where(
                            cx,
                            r.dupe(),
                            |cx, _reason, t_prime_id| {
                                let t_prime_tvar = Tvar::new(r.dupe(), t_prime_id as u32);
                                let open_t_prime = Type::new(TypeInner::OpenT(t_prime_tvar));
                                let u_inner = UseTInner::GetEnumT(Box::new(GetEnumTData {
                                    use_op: unknown_use(),
                                    reason: r.dupe(),
                                    orig_t: None,
                                    kind: GetEnumKind::GetEnumValue,
                                    tout: open_t_prime,
                                }));
                                FlowJs::flow(cx, &tout_val, &UseT::new(u_inner))?;
                                Ok::<(), FlowJsException>(())
                            },
                        )?;
                        use_t_result_of_t_option(merge_lower_bounds(cx, &result_t))
                    })
                }
                box Destructor::ReactElementConfigType => {
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    bind_use_t_result(result, &|config: Type| {
                        let react_node = FlowJs::get_builtin_react_type(
                            cx,
                            None,
                            r,
                            None,
                            flow_typing_errors::intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactNodeType,
                        )?;
                        Ok(UseTResult::UpperT(Type::new(TypeInner::DefT(
                            r.dupe(),
                            DefT::new(DefTInner::ReactAbstractComponentT(Box::new(
                                ReactAbstractComponentTData {
                                    config,
                                    renders: react_node,
                                    component_kind: ComponentKind::Structural,
                                },
                            ))),
                        ))))
                    })
                }
                box Destructor::RestType(_, t_rest) => {
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    let t_rest = t_rest.dupe();
                    bind_use_t_result(result, &|tout_val: Type| {
                        let reversed = reverse_obj_kit_rest(cx, r, &t_rest, &tout_val)?;
                        use_t_result_of_t_option(merge_lower_bounds(cx, &reversed))
                    })
                }
                box Destructor::NonMaybeType => {
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    bind_use_t_result(result, &|t: Type| {
                        Ok(UseTResult::UpperT(Type::new(TypeInner::MaybeT(
                            r.dupe(),
                            t,
                        ))))
                    })
                }
                box Destructor::ExactType
                | box Destructor::ReadOnlyType
                | box Destructor::ReactDRO(_)
                | box Destructor::PartialType
                | box Destructor::RequiredType => merge_lower_or_upper_bounds(cx, seen, &tout_t),
                box Destructor::ReactCheckComponentConfig {
                    props: pmap,
                    allow_ref_in_spread: _,
                } => {
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    let pmap = pmap.clone();
                    bind_use_t_result(result, &|t: Type| {
                        let reversed = reverse_component_check_config(cx, r, &pmap, &t)?;
                        use_t_result_of_t_option(merge_lower_bounds(cx, &reversed))
                    })
                }
                box Destructor::SpreadType(box DestructorSpreadTypeData(
                    _,
                    todo_rev,
                    head_slice,
                )) => {
                    let acc_elements: Vec<object::spread::AccElement> = match head_slice {
                        Some(x) => vec![object::spread::AccElement::InlineSlice(x.clone())],
                        None => vec![],
                    };
                    let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                    let todo_rev = todo_rev.clone();
                    bind_use_t_result(result, &|t: Type| {
                        let reversed = reverse_obj_spread(cx, r, &todo_rev, &acc_elements, &t)?;
                        use_t_result_of_t_option(merge_lower_bounds(cx, &reversed))
                    })
                }
                box Destructor::SpreadTupleType(box DestructorSpreadTupleTypeData {
                    reason_tuple: _,
                    inexact,
                    reason_spread,
                    resolved: resolved_rev,
                    unresolved,
                }) => {
                    fn is_spread(rp: &ResolvedParam) -> bool {
                        match rp {
                            ResolvedParam::ResolvedArg(..) => false,
                            ResolvedParam::ResolvedSpreadArg(..)
                            | ResolvedParam::ResolvedAnySpreadArg(..) => true,
                        }
                    }
                    // Reverse if the spread is the last item in the tuple, there are
                    // no other spreads, and the tuple type is not inexact.
                    match (&**unresolved, resolved_rev.iter().any(is_spread)) {
                        ([], false) if !inexact => {
                            let n = resolved_rev.len() as i32;
                            let result = merge_lower_or_upper_bounds(cx, seen, &tout_t)?;
                            let reason_spread = reason_spread.dupe();
                            bind_use_t_result(result, &|t: Type| {
                                let arr_rest_result = flow_typing_tvar::mk_where(
                                    cx,
                                    reason_spread.dupe(),
                                    |cx, t_prime| {
                                        let u_inner = UseTInner::ArrRestT(Box::new(ArrRestTData {
                                            use_op: unknown_use(),
                                            reason: reason_spread.dupe(),
                                            index: n,
                                            tout: t_prime.dupe(),
                                        }));
                                        FlowJs::flow(cx, &t, &UseT::new(u_inner))?;
                                        Ok::<(), FlowJsException>(())
                                    },
                                )?;
                                use_t_result_of_t_option(merge_lower_bounds(cx, &arr_rest_result))
                            })
                        }
                        _ => Ok(UseTResult::UpperNonT(u.dupe())),
                    }
                }
            }
        }
        UseTInner::ArrRestT(box ArrRestTData { index: i, tout, .. }) => {
            if let TypeInner::DefT(_, def_t) = get_t(cx, tout).deref()
                && let DefTInner::ArrT(arr_t) = def_t.deref()
            {
                match arr_t.as_ref() {
                    ArrType::ArrayAT(box ArrayATData {
                        tuple_view: None, ..
                    })
                    | ArrType::ROArrayAT(box (..)) => {
                        identity_reverse_upper_bound(cx, seen, tvar, tout)
                    }
                    ArrType::ArrayAT(box ArrayATData {
                        tuple_view: Some(_),
                        ..
                    })
                    | ArrType::TupleAT(box TupleATData { .. })
                        if *i == 0 =>
                    {
                        identity_reverse_upper_bound(cx, seen, tvar, tout)
                    }
                    _ => Ok(UseTResult::UpperEmpty),
                }
            } else {
                Ok(UseTResult::UpperEmpty)
            }
        }
        // Call related upper bounds are ignored because there is not enough info to reverse.
        // Get/set-prop related upper bounds are ignored
        // because there is not enough info to reverse.
        UseTInner::BindT(..)
        | UseTInner::CallT(..)
        | UseTInner::ConditionalT(..)
        | UseTInner::MethodT(..)
        | UseTInner::PrivateMethodT(..)
        | UseTInner::ConstructorT(..)
        | UseTInner::ToStringT { .. }
        | UseTInner::MapTypeT(..)
        | UseTInner::SetPropT(..)
        | UseTInner::SetPrivatePropT(..)
        | UseTInner::GetElemT { .. }
        | UseTInner::SetElemT(..)
        | UseTInner::CallElemT(..)
        | UseTInner::GetTypeFromNamespaceT { .. }
        | UseTInner::GetPropT(..)
        | UseTInner::GetPrivatePropT(..)
        | UseTInner::TestPropT { .. }
        | UseTInner::GetStaticsT(..)
        | UseTInner::GetProtoT(..)
        | UseTInner::SetProtoT(..)
        | UseTInner::ObjTestProtoT(..)
        | UseTInner::HasOwnPropT(..)
        | UseTInner::LookupT { .. }
        | UseTInner::OptionalIndexedAccessT { .. }
        | UseTInner::GetKeysT(..)
        | UseTInner::GetValuesT(..)
        | UseTInner::GetDictValuesT(..)
        | UseTInner::SuperT(..)
        | UseTInner::ImplementsT(..)
        | UseTInner::MixinT(..)
        | UseTInner::ExtendsUseT(..)
        | UseTInner::ObjTestT(..)
        | UseTInner::GetEnumT { .. }
        | UseTInner::CondT(..)
        | UseTInner::CheckUnusedPromiseT { .. }
        | UseTInner::ConvertEmptyPropsToMixedT(..)
        | UseTInner::ValueToTypeReferenceT(..)
        | UseTInner::ExitRendersT { .. } => Ok(UseTResult::UpperEmpty),
        // Remaining unhandled upper bounds
        UseTInner::SpecializeT(..)
        | UseTInner::ThisSpecializeT(..)
        | UseTInner::ConcretizeTypeAppsT(..)
        | UseTInner::ObjRestT(..)
        | UseTInner::ElemT(..)
        | UseTInner::ReactKitT(..)
        | UseTInner::ConcretizeT(..)
        | UseTInner::FilterOptionalT(..)
        | UseTInner::FilterMaybeT(..)
        | UseTInner::SealGenericT { .. } => Ok(UseTResult::UpperNonT(u.dupe())),
        UseTInner::DeepReadOnlyT(tvar_out, _) => {
            let tout = Type::new(TypeInner::OpenT((**tvar_out).dupe()));
            identity_reverse_upper_bound(cx, seen, tvar, &tout)
        }
        UseTInner::HooklikeT(tvar_out) => {
            let tout = Type::new(TypeInner::OpenT((**tvar_out).dupe()));
            identity_reverse_upper_bound(cx, seen, tvar, &tout)
        }
        UseTInner::ReposLowerT { use_t, .. } => t_of_use_t(cx, seen, tvar, use_t),
        UseTInner::ReposUseT(box ReposUseTData { type_: t, .. }) => {
            FlowJs::flow_t(cx, t, tvar)?;
            Ok(UseTResult::UpperT(t.dupe()))
        }
        UseTInner::ResolveSpreadT(box ResolveSpreadTData {
            reason,
            resolve_spread_type,
            ..
        }) if resolve_spread_type.rrt_unresolved.is_empty() => {
            match &resolve_spread_type.rrt_resolve_to {
                SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(_, funtype) => {
                    match reverse_resolve_spread_multiflow_subtype_full_partial_resolution(
                        cx,
                        tvar,
                        reason,
                        &resolve_spread_type.rrt_resolved,
                        &funtype.params,
                        &funtype.rest_param,
                    )? {
                        None => Ok(UseTResult::UpperNonT(u.dupe())),
                        Some(solution) => Ok(UseTResult::UpperT(solution)),
                    }
                }
                SpreadResolve::ResolveSpreadsToTupleType { .. }
                | SpreadResolve::ResolveSpreadsToArrayLiteral { .. }
                | SpreadResolve::ResolveSpreadsToArray(..)
                | SpreadResolve::ResolveSpreadsToMultiflowCallFull(..)
                | SpreadResolve::ResolveSpreadsToMultiflowPartial(..) => {
                    Ok(UseTResult::UpperNonT(u.dupe()))
                }
            }
        }
        UseTInner::ResolveSpreadT(..) => Ok(UseTResult::UpperNonT(u.dupe())),
        UseTInner::ResolveUnionT(box ResolveUnionTData { upper, .. }) => {
            t_of_use_t(cx, seen, tvar, upper)
        }
        UseTInner::ObjKitT(_, r, _, box tool, tout) => match tool {
            object::Tool::MakeExact
            | object::Tool::ReadOnly
            | object::Tool::Partial
            | object::Tool::Required
            | object::Tool::ObjectRep => identity_reverse_upper_bound(cx, seen, tvar, tout),
            object::Tool::ReactConfig(box ObjectToolReactConfigData {
                ref_manipulation: object::react_config::RefManipulation::KeepRef,
                ..
            }) => identity_reverse_upper_bound(cx, seen, tvar, tout),
            object::Tool::ReactConfig(box ObjectToolReactConfigData {
                ref_manipulation: object::react_config::RefManipulation::AddRef(ref_t),
                ..
            }) => {
                let solution = merge_upper_bounds(cx, seen, tout)?;
                match solution {
                    UseTResult::UpperEmpty => Ok(UseTResult::UpperEmpty),
                    UseTResult::UpperNonT(u) => Ok(UseTResult::UpperNonT(u)),
                    UseTResult::UpperT(t) => {
                        let pmap = properties::PropertiesMap::from_btree_map(BTreeMap::from([(
                            flow_common::reason::Name::new("ref"),
                            Property::new(PropertyInner::Field(Box::new(FieldData {
                                preferred_def_locs: None,
                                key_loc: None,
                                type_: ref_t.dupe(),
                                polarity: Polarity::Neutral,
                            }))),
                        )]));
                        let reversed = reverse_component_check_config(cx, r, &pmap, &t)?;
                        match merge_lower_bounds(cx, &reversed)? {
                            None => Ok(UseTResult::UpperEmpty),
                            Some(reversed) => {
                                FlowJs::flow_t(cx, &reversed, tvar)?;
                                Ok(UseTResult::UpperT(reversed))
                            }
                        }
                    }
                }
            }
            object::Tool::ReactCheckComponentConfig {
                props: pmap,
                allow_ref_in_spread: _,
            } => {
                let solution = merge_upper_bounds(cx, seen, tout)?;
                match solution {
                    UseTResult::UpperEmpty => Ok(UseTResult::UpperEmpty),
                    UseTResult::UpperNonT(u) => Ok(UseTResult::UpperNonT(u)),
                    UseTResult::UpperT(t) => {
                        let reversed = reverse_component_check_config(cx, r, pmap, &t)?;
                        match merge_lower_bounds(cx, &reversed)? {
                            None => Ok(UseTResult::UpperEmpty),
                            Some(reversed) => {
                                FlowJs::flow_t(cx, &reversed, tvar)?;
                                Ok(UseTResult::UpperT(reversed))
                            }
                        }
                    }
                }
            }
            object::Tool::ObjectMap(box ObjectToolObjectMapData { .. }) => {
                Ok(UseTResult::UpperEmpty)
            }
            object::Tool::Spread(box (_, state)) => {
                let solution = merge_upper_bounds(cx, seen, tout)?;
                match solution {
                    UseTResult::UpperEmpty => Ok(UseTResult::UpperEmpty),
                    UseTResult::UpperNonT(u) => Ok(UseTResult::UpperNonT(u)),
                    UseTResult::UpperT(t) => {
                        let reversed = reverse_obj_spread(cx, r, &state.todo_rev, &state.acc, &t)?;
                        match merge_lower_bounds(cx, &reversed)? {
                            None => Ok(UseTResult::UpperEmpty),
                            Some(reversed) => {
                                FlowJs::flow_t(cx, &reversed, tvar)?;
                                Ok(UseTResult::UpperT(reversed))
                            }
                        }
                    }
                }
            }
            object::Tool::Rest(box (_, object::rest::State::One(t_rest))) => {
                let result = merge_upper_bounds(cx, seen, tout)?;
                let t_rest = t_rest.dupe();
                bind_use_t_result(result, &|t: Type| {
                    let reversed = reverse_obj_kit_rest(cx, r, &t_rest, &t)?;
                    match merge_lower_bounds(cx, &reversed)? {
                        None => Ok(UseTResult::UpperEmpty),
                        Some(reversed) => {
                            FlowJs::flow_t(cx, &reversed, tvar)?;
                            Ok(UseTResult::UpperT(reversed))
                        }
                    }
                })
            }
            object::Tool::Rest(box (_, object::rest::State::Done(_))) => {
                Ok(UseTResult::UpperNonT(u.dupe()))
            }
        },
    }
}

fn identity_reverse_upper_bound<'cx>(
    cx: &Context<'cx>,
    seen: &mut BTreeSet<i32>,
    tvar: &Type,
    tout: &Type,
) -> Result<UseTResult<'cx>, FlowJsException> {
    let solution = merge_upper_bounds(cx, seen, tout)?;
    match &solution {
        UseTResult::UpperT(t) => FlowJs::flow_t(cx, t, tvar)?,
        _ => {}
    }
    Ok(solution)
}

fn reverse_obj_spread<'cx>(
    cx: &Context<'cx>,
    r: &Reason,
    todo_rev: &[object::spread::Operand],
    acc_elements: &[object::spread::AccElement],
    tout: &Type,
) -> Result<Type, FlowJsException> {
    let inline_slice_to_t = |s: &object::spread::OperandSlice| -> Type {
        let obj_kind = obj_type::obj_kind_from_optional_dict(s.dict.clone(), ObjKind::Exact);
        obj_type::mk_with_proto(
            cx,
            r.dupe(),
            obj_kind,
            Some(s.reachable_targs.dupe()),
            None,
            Some(
                s.prop_map
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.dupe()))
                    .collect(),
            ),
            None,
            Type::new(TypeInner::ObjProtoT(r.dupe())),
        )
    };

    let slice_to_t = |s: &object::Slice| -> Type {
        let props: properties::PropertiesMap = s
            .props
            .iter()
            .map(|(k, prop)| {
                (
                    k.dupe(),
                    Property::new(PropertyInner::Field(Box::new(FieldData {
                        preferred_def_locs: None,
                        key_loc: prop.key_loc.dupe(),
                        type_: prop.prop_t.dupe(),
                        polarity: Polarity::Neutral,
                    }))),
                )
            })
            .collect();
        obj_type::mk_with_proto(
            cx,
            r.dupe(),
            s.flags.obj_kind.clone(),
            Some(s.reachable_targs.dupe()),
            None,
            Some(props),
            None,
            Type::new(TypeInner::ObjProtoT(r.dupe())),
        )
    };

    let operand_to_t = |o: &object::spread::Operand| -> Type {
        match o {
            object::spread::Operand::Slice(s) => inline_slice_to_t(s),
            object::spread::Operand::Type(t) => t.dupe(),
        }
    };

    let acc_element_to_ts = |e: &object::spread::AccElement| -> Vec<Type> {
        match e {
            object::spread::AccElement::InlineSlice(s) => vec![inline_slice_to_t(s)],
            object::spread::AccElement::ResolvedSlice(resolved) => {
                resolved.0.iter().map(slice_to_t).collect()
            }
        }
    };

    let rest_type = |l: Type, rest: Type| -> Result<Type, FlowJsException> {
        flow_typing_tvar::mk_where(cx, r.dupe(), |cx, tout| {
            let u_inner = UseTInner::ObjKitT(
                unknown_use(),
                r.dupe(),
                Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
                Box::new(object::Tool::Rest(Box::new((
                    object::rest::MergeMode::SpreadReversal,
                    object::rest::State::One(rest),
                )))),
                tout.dupe(),
            );
            FlowJs::flow(cx, &l, &UseT::new(u_inner))?;
            Ok::<(), FlowJsException>(())
        })
    };

    let mut tout_val = flow_typing_tvar::mk_where(cx, r.dupe(), |cx, t_prime| {
        FlowJs::flow_t(cx, tout, t_prime)?;
        Ok::<(), FlowJsException>(())
    })?;
    for e in acc_elements.iter() {
        for t in acc_element_to_ts(e) {
            tout_val = rest_type(tout_val, t)?;
        }
    }
    for o in todo_rev.iter() {
        tout_val = rest_type(tout_val, operand_to_t(o))?;
    }
    Ok(tout_val)
}

fn reverse_component_check_config<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    pmap: &properties::PropertiesMap,
    tout: &Type,
) -> Result<Type, FlowJsException> {
    if pmap.is_empty() {
        return Ok(tout.dupe());
    }
    flow_typing_tvar::mk_where(cx, reason.dupe(), |cx, t_prime| {
        let rest = obj_type::mk_with_proto(
            cx,
            reason.dupe(),
            ObjKind::Exact,
            None,
            None,
            Some(pmap.iter().map(|(k, v)| (k.dupe(), v.dupe())).collect()),
            None,
            Type::new(TypeInner::ObjProtoT(reason.dupe())),
        );
        let u_inner = UseTInner::ObjKitT(
            unknown_use(),
            reason.dupe(),
            Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
            Box::new(object::Tool::Rest(Box::new((
                object::rest::MergeMode::SpreadReversal,
                object::rest::State::One(rest),
            )))),
            t_prime.dupe(),
        );
        FlowJs::flow(cx, tout, &UseT::new(u_inner))?;
        Ok(())
    })
}

fn reverse_obj_kit_rest<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    t_rest: &Type,
    tout: &Type,
) -> Result<Type, FlowJsException> {
    flow_typing_tvar::mk_no_wrap_where(cx, reason.dupe(), |cx, _r, t_prime_id| {
        let tool = object::ResolveTool::Resolve(object::Resolve::Next);
        let options = object::spread::Target::Value {
            make_seal: obj_type::mk_seal(false, false),
        };
        let state = object::spread::State {
            todo_rev: vec![object::spread::Operand::Type(t_rest.dupe())].into(),
            acc: Rc::from([]),
            spread_id: flow_common::reason::mk_id() as i32,
            union_reason: None,
            curr_resolve_idx: 0,
        };
        let open_t_prime = Type::new(TypeInner::OpenT(Tvar::new(
            reason.dupe(),
            t_prime_id as u32,
        )));
        let u_inner = UseTInner::ObjKitT(
            unknown_use(),
            reason.dupe(),
            Box::new(tool),
            Box::new(object::Tool::Spread(Box::new((options, state)))),
            open_t_prime,
        );
        FlowJs::flow(cx, tout, &UseT::new(u_inner))?;
        Ok(())
    })
}

// and reverse_resolve_spread_multiflow_subtype_full_no_resolution cx tvar reason params rest_param =
fn reverse_resolve_spread_multiflow_subtype_full_no_resolution<'cx>(
    cx: &Context<'cx>,
    tvar: &Type,
    reason: &Reason,
    params: &[FunParam],
    rest_param: &Option<FunRestParam>,
) -> Result<Type, FlowJsException> {
    let mut tuple_elements = Vec::new();
    let mut tuple_ts = Vec::new();
    for param in params.iter() {
        let name = &param.0;
        let t = &param.1;
        let el = type_util::mk_tuple_element(
            type_util::reason_of_t(t).dupe(),
            t.dupe(),
            name.as_ref().map(|s| s.dupe()),
            false,
            Polarity::Neutral,
        );
        tuple_elements.push(el);
        tuple_ts.push(t.dupe());
    }
    let (reason_out, arr_type) = match rest_param {
        None => {
            let elem_t = match tuple_ts.as_slice() {
                [] => empty_t::why(reason.dupe()),
                [t] => t.dupe(),
                [t0, t1, ts @ ..] => Type::new(TypeInner::UnionT(
                    reason.dupe(),
                    union_rep::make(
                        None,
                        union_rep::UnionKind::ImplicitInstantiationKind,
                        t0.dupe(),
                        t1.dupe(),
                        ts.iter().duped().collect::<Rc<[_]>>(),
                    ),
                )),
            };
            let len = tuple_ts.len() as i32;
            let t = ArrType::TupleAT(Box::new(TupleATData {
                elem_t,
                elements: tuple_elements.into(),
                arity: (len, len),
                inexact: false,
                react_dro: None,
            }));
            let reason_out = reason
                .dupe()
                .update_desc(|_| flow_common::reason::VirtualReasonDesc::RTupleType);
            (reason_out, t)
        }
        Some(rest_param_val) => {
            let rest_param_t = &rest_param_val.2;
            let rest_elem_t =
                flow_typing_tvar::mk_no_wrap_where(cx, reason.dupe(), |cx, _r, tout_id| {
                    let tout_tvar = Tvar::new(reason.dupe(), tout_id as u32);
                    let u_inner = UseTInner::GetElemT(Box::new(GetElemTData {
                        use_op: unknown_use(),
                        reason: reason.dupe(),
                        id: None,
                        from_annot: true,
                        skip_optional: false,
                        access_iterables: false,
                        key_t: num_module_t::make(reason.dupe()),
                        tout: Box::new(tout_tvar),
                    }));
                    FlowJs::flow(cx, rest_param_t, &UseT::new(u_inner))?;
                    Ok::<(), FlowJsException>(())
                })?;
            let elem_t = match tuple_ts.as_slice() {
                [] => rest_elem_t,
                [t, ts @ ..] => Type::new(TypeInner::UnionT(
                    reason.dupe(),
                    union_rep::make(
                        None,
                        union_rep::UnionKind::ImplicitInstantiationKind,
                        rest_elem_t,
                        t.dupe(),
                        ts.iter().duped().collect::<Rc<[_]>>(),
                    ),
                )),
            };
            let t = ArrType::ArrayAT(Box::new(ArrayATData {
                elem_t,
                tuple_view: None,
                react_dro: None,
            }));
            let reason_out = reason
                .dupe()
                .update_desc(|_| flow_common::reason::VirtualReasonDesc::RArray);
            (reason_out, t)
        }
    };
    let solution = Type::new(TypeInner::DefT(
        reason_out,
        DefT::new(DefTInner::ArrT(std::rc::Rc::new(arr_type))),
    ));
    FlowJs::flow_t(cx, &solution, tvar)?;
    Ok(solution)
}

fn reverse_resolve_spread_multiflow_subtype_full_partial_resolution<'cx>(
    cx: &Context<'cx>,
    tvar: &Type,
    reason: &Reason,
    resolved: &[ResolvedParam],
    params: &[FunParam],
    rest_param: &Option<FunRestParam>,
) -> Result<Option<Type>, FlowJsException> {
    // We remove resolved params one by one from the start.
    // When we run out of params but we still have resolved_params,
    // we record the number of rest_param we need to remove,
    // and later perform an ArrayRest to remove them.
    let resolved_len = resolved.len();
    let params_len = params.len();
    match if resolved_len <= params_len {
        Some((params_len - resolved_len, 0))
    } else if rest_param.is_some() {
        Some((0, resolved_len - params_len))
    } else {
        None
    } {
        None => Ok(None),
        Some((remaining_params_count, _rest_index)) => {
            let remaining_params = &params[params.len() - remaining_params_count..];
            Ok(Some(
                reverse_resolve_spread_multiflow_subtype_full_no_resolution(
                    cx,
                    tvar,
                    reason,
                    remaining_params,
                    rest_param,
                )?,
            ))
        }
    }
}

// merge_upper_bounds is complex — it looks at upper bounds and tries to find a consistent type.
// For now we provide a simplified version that handles the common cases.
fn merge_upper_bounds<'cx>(
    cx: &Context<'cx>,
    seen: &mut BTreeSet<i32>,
    tvar: &Type,
) -> Result<UseTResult<'cx>, FlowJsException> {
    let filter_placeholder = |t: Type| -> UseTResult<'cx> {
        if flow_js_utils::tvar_visitors::has_placeholders(cx, &t) {
            UseTResult::UpperEmpty
        } else {
            UseTResult::UpperT(t)
        }
    };
    let equal = |t1: &Type, t2: &Type| concrete_type_eq::eq(cx, t1, t2);

    match tvar.deref() {
        TypeInner::OpenT(open_t) => {
            let id = open_t.id() as i32;
            if seen.contains(&id) {
                return Ok(UseTResult::UpperEmpty);
            }
            let constraints = cx.find_graph(id);
            match constraints {
                constraint::Constraints::FullyResolved(s) => {
                    Ok(filter_placeholder(cx.force_fully_resolved_tvar(&s)))
                }
                constraint::Constraints::Resolved(t) => Ok(filter_placeholder(t)),
                constraint::Constraints::Unresolved(bounds) => {
                    let uppers: Vec<_> = bounds
                        .borrow()
                        .upper
                        .iter()
                        .map(|(k, v)| (k.use_t.dupe(), *v))
                        .collect();
                    let inserted = seen.insert(id);
                    let sorted_uppers = sort_upper_bounds_for_merging(uppers);

                    let mut acc = UseTResult::UpperEmpty;
                    for (u, _) in sorted_uppers {
                        let result = t_of_use_t(cx, seen, tvar, &u)?;
                        acc = match (acc, result) {
                            (UseTResult::UpperNonT(u), _) => UseTResult::UpperNonT(u),
                            (_, UseTResult::UpperNonT(u)) => UseTResult::UpperNonT(u),
                            (UseTResult::UpperEmpty, UseTResult::UpperT(t)) => {
                                filter_placeholder(t)
                            }
                            (UseTResult::UpperT(old_t), UseTResult::UpperT(t))
                                if flow_js_utils::tvar_visitors::has_placeholders(cx, &t) =>
                            {
                                UseTResult::UpperT(old_t)
                            }
                            (UseTResult::UpperT(t_prime), UseTResult::UpperT(t)) => {
                                if equal(&t_prime, &t)
                                    || type_util::quick_subtype(None::<&fn(&Type)>, &t, &t_prime)
                                    || speculative_subtyping_succeeds(
                                        cx,
                                        DepthTrace::dummy_trace(),
                                        unknown_use(),
                                        &t,
                                        &t_prime,
                                    )?
                                {
                                    UseTResult::UpperT(t)
                                } else {
                                    UseTResult::UpperT(t_prime)
                                }
                            }
                            (acc @ UseTResult::UpperT(_), UseTResult::UpperEmpty) => acc,
                            (UseTResult::UpperEmpty, UseTResult::UpperEmpty) => {
                                UseTResult::UpperEmpty
                            }
                        };
                    }
                    if inserted {
                        seen.remove(&id);
                    }
                    Ok(acc)
                }
            }
        }
        _ => Ok(UseTResult::UpperT(tvar.dupe())),
    }
}

fn merge_lower_bounds<'cx>(cx: &Context<'cx>, t: &Type) -> Result<Option<Type>, FlowJsException> {
    // When the input tvar has a ReposUseT upper bound it means that we might be
    // discounting lower bounds that are just waiting to be added as soon as the
    // ReposUseT fires. Here, we make sure we record the result of the ReposUseT
    // before we make a decision based on lower bounds.
    let t_resolved = match t.deref() {
        TypeInner::OpenT(open_t) => {
            let id = open_t.id() as i32;
            let constraints = cx.find_graph(id);
            match constraints {
                constraint::Constraints::Unresolved(bounds) => {
                    let upper = bounds.borrow().upper.clone();
                    for (key, _) in upper.iter() {
                        if let UseTInner::ReposUseT(box ReposUseTData { type_: ref l, .. }) =
                            *key.use_t
                        {
                            FlowJs::flow_t(cx, l, t)?;
                        }
                    }
                    t.dupe()
                }
                _ => t.dupe(),
            }
        }
        _ => t.dupe(),
    };

    Ok(match t_resolved.deref() {
        TypeInner::OpenT(open_t) => {
            let r = open_t.reason();
            let id = open_t.id() as i32;
            let constraints = cx.find_graph(id);
            match constraints {
                constraint::Constraints::FullyResolved(s) => {
                    let t = cx.force_fully_resolved_tvar(&s);
                    if flow_js_utils::tvar_visitors::has_placeholders(cx, &t) {
                        None
                    } else {
                        Some(t)
                    }
                }
                constraint::Constraints::Resolved(t) => {
                    if flow_js_utils::tvar_visitors::has_placeholders(cx, &t) {
                        None
                    } else {
                        Some(t)
                    }
                }
                constraint::Constraints::Unresolved(bounds) => {
                    let lower_types: Vec<Type> = bounds.borrow().lower.keys().duped().collect();
                    if lower_types.is_empty() {
                        None
                    } else {
                        let mut seen = BTreeSet::new();
                        seen.insert(id);
                        let mut collected = Vec::new();
                        flow_js_utils::collect_lowers(
                            false,
                            cx,
                            &mut seen,
                            &mut collected,
                            lower_types,
                        );
                        let flattened = union_flatten_list(collected);
                        let filtered: Vec<Type> = flattened
                            .into_iter()
                            .filter(|t| !flow_js_utils::tvar_visitors::has_placeholders(cx, t))
                            .collect();
                        type_util::union_of_ts_opt(
                            r.dupe(),
                            filtered,
                            Some(union_rep::UnionKind::ImplicitInstantiationKind),
                        )
                    }
                }
            }
        }
        _ => Some(t_resolved),
    })
}

fn on_missing_bounds<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    tparam: &TypeParam,
    default_bound: &Option<Type>,
    tparam_binder_reason: &Reason,
    instantiation_reason: &Reason,
) -> Result<InferredTarg, FlowJsException> {
    match default_bound {
        Some(t) => Ok(Obs::on_pinned_tparam(cx, tparam, t.dupe())),
        None => Obs::on_missing_bounds(
            cx,
            use_op,
            tparam,
            tparam_binder_reason,
            instantiation_reason,
        ),
    }
}

fn use_upper_bounds<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    tparam: &TypeParam,
    tvar: &Type,
    default_bound: &Option<Type>,
    on_upper_empty: Option<
        &dyn Fn(
            &Context<'cx>,
            &TypeParam,
            &Reason,
            &Reason,
        ) -> Result<InferredTarg, FlowJsException>,
    >,
    tparam_binder_reason: &Reason,
    instantiation_reason: &Reason,
) -> Result<InferredTarg, FlowJsException> {
    let upper_t = merge_upper_bounds(cx, &mut BTreeSet::new(), tvar)?;
    match upper_t {
        UseTResult::UpperEmpty => match on_upper_empty {
            Some(f) => f(cx, tparam, tparam_binder_reason, instantiation_reason),
            None => on_missing_bounds::<Obs>(
                cx,
                use_op,
                tparam,
                default_bound,
                tparam_binder_reason,
                instantiation_reason,
            ),
        },
        UseTResult::UpperNonT(u) => Obs::on_upper_non_t(
            cx,
            use_op,
            &u,
            tparam,
            tparam_binder_reason,
            instantiation_reason,
        ),
        UseTResult::UpperT(inferred) => Ok(Obs::on_pinned_tparam(cx, tparam, inferred)),
    }
}

fn check_instantiation<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    tparams: &[TypeParam],
    marked_tparams: Marked<SubstName>,
    check: &ImplicitInstantiationCheck,
) -> Result<
    (
        Vec<(SubstName, Type, Type, bool)>,
        Marked<SubstName>,
        Option<Type>,
    ),
    FlowJsException,
> {
    let (ref use_op, ref reason_op, ref op) = check.operation;
    let reason_tapp = type_util::reason_of_t(&check.lhs);
    let (ref tparams_loc, _, _) = check.poly_t;

    let merge_targs = |explicit_targs: &Option<Rc<[Targ]>>| -> Result<
        (Vec<Targ>, Vec<(SubstName, Type, Type, bool)>),
        FlowJsException,
    > {
        match explicit_targs {
            None => {
                let mut targs: Vec<Targ> = Vec::new();
                let mut inferred_targ_and_bound_list: Vec<(SubstName, Type, Type, bool)> =
                    Vec::new();
                for tparam in tparams.iter() {
                    let targ = instantiation_utils::implicit_type_argument::mk_targ(
                        cx,
                        tparam,
                        reason_op,
                        reason_tapp,
                    );
                    targs.push(Targ::ExplicitArg(targ.dupe()));
                    inferred_targ_and_bound_list.push((
                        tparam.name.dupe(),
                        targ,
                        tparam.bound.dupe(),
                        true,
                    ));
                }
                Ok((targs, inferred_targ_and_bound_list))
            }
            Some(explicit_targs) => {
                let maximum_arity = tparams.len();
                let arity_loc = tparams_loc;
                let minimum_arity = flow_js_utils::poly_minimum_arity(
                    &Vec1::try_from_vec(tparams.to_vec()).unwrap(),
                );
                if explicit_targs.len() > maximum_arity {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::ETooManyTypeArgs(Box::new(ETooManyTypeArgsData {
                            reason_tapp: reason_tapp.dupe(),
                            arity_loc: arity_loc.dupe(),
                            maximum_arity: maximum_arity as i32,
                        })),
                    )?;
                }

                let mut targs = Vec::new();
                let mut inferred_targ_and_bound_list = Vec::new();
                let mut tparams_iter = tparams.iter();
                let mut explicit_targs_iter = explicit_targs.iter();
                loop {
                    match (tparams_iter.next(), explicit_targs_iter.next()) {
                        (None, _) => {
                            break;
                        }
                        (Some(tparam), None) => {
                            let targ = instantiation_utils::implicit_type_argument::mk_targ(
                                cx,
                                tparam,
                                reason_op,
                                reason_tapp,
                            );
                            if tparam.default.is_none() {
                                flow_js_utils::add_output(
                                    cx,
                                    ErrorMessage::ETooFewTypeArgs(Box::new(ETooFewTypeArgsData {
                                        reason_tapp: reason_tapp.dupe(),
                                        arity_loc: arity_loc.dupe(),
                                        minimum_arity: minimum_arity as i32,
                                    })),
                                )?;
                            }
                            targs.push(Targ::ExplicitArg(targ.dupe()));
                            inferred_targ_and_bound_list.push((
                                tparam.name.dupe(),
                                targ,
                                tparam.bound.dupe(),
                                true,
                            ));
                        }
                        (Some(tparam), Some(explicit_targ)) => {
                            match explicit_targ {
                                Targ::ExplicitArg(targ) => {
                                    targs.push(Targ::ExplicitArg(targ.dupe()));
                                    inferred_targ_and_bound_list.push((
                                        tparam.name.dupe(),
                                        targ.dupe(),
                                        tparam.bound.dupe(),
                                        false,
                                    ));
                                }
                                Targ::ImplicitArg(tvar) => {
                                    let r = tvar.reason();
                                    let reason = flow_common::reason::mk_reason(
                                        VirtualReasonDesc::RImplicitInstantiation,
                                        r.loc().dupe(),
                                    );
                                    let targ = instantiation_utils::implicit_type_argument::mk_targ(
                                        cx,
                                        tparam,
                                        &reason,
                                        reason_tapp,
                                    );
                                    FlowJs::flow(
                                        cx,
                                        &targ,
                                        &UseT::new(UseTInner::UseT(
                                            use_op.dupe(),
                                            Type::new(TypeInner::OpenT(tvar.dupe())),
                                        )),
                                    )?;
                                    // It is important to convert implicit args to explicit args so there won't be
                                    // infinite loops between this module and flow_js.
                                    targs.push(Targ::ExplicitArg(targ.dupe()));
                                    inferred_targ_and_bound_list.push((
                                        tparam.name.dupe(),
                                        targ,
                                        tparam.bound.dupe(),
                                        true,
                                    ));
                                }
                            }
                        }
                    }
                }
                Ok((targs, inferred_targ_and_bound_list))
            }
        }
    };

    let (inferred_targ_list, lower_t, use_t, tout) = match op {
        flow_typing_implicit_instantiation_check::Operation::Call(calltype) => {
            let new_tout = flow_typing_tvar::mk_no_wrap(cx, reason_op);
            let (call_targs, inferred_targ_list) = merge_targs(&calltype.call_targs)?;
            let call_t = UseT::new(UseTInner::CallT(Box::new(CallTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                call_action: Box::new(CallAction::Funcalltype(Box::new(FuncallType {
                    call_targs: Some(call_targs.into()),
                    call_tout: Tvar::new(reason_op.dupe(), new_tout as u32),
                    ..calltype.clone()
                }))),
                return_hint: hint_unavailable(),
            })));
            (
                inferred_targ_list,
                check.lhs.dupe(),
                call_t,
                Some(Type::new(TypeInner::OpenT(Tvar::new(
                    reason_op.dupe(),
                    new_tout as u32,
                )))),
            )
        }
        flow_typing_implicit_instantiation_check::Operation::SubtypeLowerPoly(u) => {
            let (_, inferred_targ_list) = merge_targs(&None)?;
            let targs: Rc<[Type]> = inferred_targ_list
                .iter()
                .map(|(_, t, _, _)| t.dupe())
                .collect();
            (
                inferred_targ_list,
                FlowJs::mk_typeapp_instance_annot(
                    cx,
                    None,
                    use_op.dupe(),
                    reason_op,
                    reason_tapp,
                    false,
                    None,
                    &check.lhs,
                    targs,
                )?,
                UseT::new(UseTInner::UseT(use_op.dupe(), u.dupe())),
                None,
            )
        }
        flow_typing_implicit_instantiation_check::Operation::Constructor(
            explicit_targs,
            call_args,
        ) => {
            let new_tout = flow_typing_tvar::mk(cx, reason_op.dupe());
            let explicit_targs_rc: Option<Rc<[Targ]>> = explicit_targs.dupe();
            let (call_targs, inferred_targ_list) = merge_targs(&explicit_targs_rc)?;
            let constructor_t = UseT::new(UseTInner::ConstructorT(Box::new(ConstructorTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                targs: Some(call_targs.into()),
                args: call_args.dupe(),
                tout: new_tout.dupe(),
                return_hint: hint_unavailable(),
                specialized_ctor: None,
            })));
            (
                inferred_targ_list,
                check.lhs.dupe(),
                constructor_t,
                Some(new_tout),
            )
        }
        flow_typing_implicit_instantiation_check::Operation::ReactJSX {
            component,
            jsx_props,
            targs,
            should_generalize,
        } => {
            let new_tout = flow_typing_tvar::mk(cx, reason_op.dupe());
            let targs_rc: Option<Rc<[Targ]>> = targs.dupe();
            let (_, inferred_targ_list) = merge_targs(&targs_rc)?;
            let new_tout_tvar = open_tvar(&new_tout).dupe();
            let react_kit_t = UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                tool: Box::new(react::Tool::<Context<'cx>>::CreateElement(Box::new(
                    react::CreateElementData {
                        component: component.dupe(),
                        jsx_props: jsx_props.dupe(),
                        targs: None,
                        tout: new_tout_tvar,
                        should_generalize: *should_generalize,
                        return_hint: hint_unavailable(),
                        record_monomorphized_result: false,
                        inferred_targs: None,
                        specialized_component: None,
                    },
                ))),
            })));
            let lower = FlowJs::mk_typeapp_instance_annot(
                cx,
                None,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                true,
                None,
                &check.lhs,
                inferred_targ_list
                    .iter()
                    .map(|(_, t, _, _)| t.dupe())
                    .collect::<Rc<[_]>>(),
            )?;
            (inferred_targ_list, lower, react_kit_t, Some(new_tout))
        }
    };
    FlowJs::flow(cx, &lower_t, &use_t)?;
    Ok((inferred_targ_list, marked_tparams, tout))
}

fn make_pin_type<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    tparam: &TypeParam,
    polarity: Option<Polarity>,
    default_bound: &Option<Type>,
    instantiation_reason: &Reason,
    t: &Type,
) -> Result<InferredTarg, FlowJsException> {
    let tparam_binder_reason = type_util::reason_of_t(t);
    let pin_tparam = |inferred: Type| Ok(Obs::on_pinned_tparam(cx, tparam, inferred));

    match polarity {
        None => match merge_lower_bounds(cx, t)? {
            None => {
                let on_upper_empty = |cx: &Context<'_>,
                                      tparam: &TypeParam,
                                      _tparam_binder_reason: &Reason,
                                      _instantiation_reason: &Reason|
                 -> Result<InferredTarg, FlowJsException> {
                    Obs::on_constant_tparam_missing_bounds(cx, tparam)
                };
                use_upper_bounds::<Obs>(
                    cx,
                    use_op,
                    tparam,
                    t,
                    default_bound,
                    Some(&on_upper_empty),
                    tparam_binder_reason,
                    instantiation_reason,
                )
            }
            Some(inferred) => pin_tparam(inferred),
        },
        // TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
        // to avoid cluttering the output we are actually interested in from this module,
        // I'm not going to start doing that until we need error diff information for
        // switching to Pierce's algorithm for implicit instantiation
        Some(Polarity::Neutral) => match merge_lower_bounds(cx, t)? {
            None => use_upper_bounds::<Obs>(
                cx,
                use_op,
                tparam,
                t,
                default_bound,
                None,
                tparam_binder_reason,
                instantiation_reason,
            ),
            Some(inferred) => pin_tparam(inferred),
        },
        Some(Polarity::Positive) => match merge_lower_bounds(cx, t)? {
            None => use_upper_bounds::<Obs>(
                cx,
                use_op,
                tparam,
                t,
                default_bound,
                None,
                tparam_binder_reason,
                instantiation_reason,
            ),
            Some(inferred) => pin_tparam(inferred),
        },
        Some(Polarity::Negative) => {
            let on_upper_empty = |cx: &Context<'_>,
                                  tparam: &TypeParam,
                                  tparam_binder_reason: &Reason,
                                  instantiation_reason: &Reason|
             -> Result<InferredTarg, FlowJsException> {
                match merge_lower_bounds(cx, t)? {
                    None => on_missing_bounds::<Obs>(
                        cx,
                        use_op,
                        tparam,
                        default_bound,
                        tparam_binder_reason,
                        instantiation_reason,
                    ),
                    Some(inferred) => Ok(Obs::on_pinned_tparam(cx, tparam, inferred)),
                }
            };
            use_upper_bounds::<Obs>(
                cx,
                use_op,
                tparam,
                t,
                default_bound,
                Some(&on_upper_empty),
                tparam_binder_reason,
                instantiation_reason,
            )
        }
    }
}

fn pin_types<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    has_new_errors: bool,
    allow_underconstrained: bool,
    has_syntactic_hint: bool,
    inferred_targ_list: &[(SubstName, Type, Type, bool)],
    marked_tparams: &Marked<SubstName>,
    tparams_map: &BTreeMap<SubstName, TypeParam>,
    check: &ImplicitInstantiationCheck,
) -> Result<(BTreeMap<SubstName, GeneralizedTarg>, Vec<(Type, SubstName)>), FlowJsException> {
    let (_, ref instantiation_reason, ref op) = check.operation;

    let subst_map: FlowOrdMap<SubstName, Type> =
        inferred_targ_list
            .iter()
            .fold(FlowOrdMap::new(), |mut acc, (name, t, _, _)| {
                acc.insert(name.dupe(), t.dupe());
                acc
            });

    let mut result_map = BTreeMap::new();
    let mut result_list = Vec::new();

    for (name, t, bound, is_inferred) in inferred_targ_list.iter().rev() {
        let tparam = match tparams_map.get(name) {
            Some(tp) => tp,
            None => continue,
        };
        let polarity = if allow_underconstrained {
            None
        } else {
            marked_tparams.get(name)
        };

        let InferredTarg {
            tparam: result_tparam,
            inferred,
        } = if *is_inferred {
            let default_bound = if has_new_errors {
                match *cx.typing_mode() {
                    flow_typing_context::TypingMode::CheckingMode => {
                        Some(any_t::error(type_util::reason_of_t(t).dupe()))
                    }
                    flow_typing_context::TypingMode::SynthesisMode { .. }
                    | flow_typing_context::TypingMode::HintEvaluationMode => {
                        Some(any_t::placeholder(type_util::reason_of_t(t).dupe()))
                    }
                }
            } else {
                None
            };
            make_pin_type::<Obs>(
                cx,
                use_op,
                tparam,
                polarity,
                &default_bound,
                instantiation_reason,
                t,
            )?
        } else {
            Obs::on_pinned_tparam(cx, tparam, t.dupe())
        };

        let bound_t = type_subst::subst(
            cx,
            Some(unknown_use()),
            true,
            false,
            type_subst::Purpose::Normal,
            &subst_map,
            bound.dupe(),
        );
        FlowJs::flow_t(cx, t, &bound_t)?;

        let call_loc = instantiation_reason.loc().dupe();
        let generalized = match op {
            flow_typing_implicit_instantiation_check::Operation::Call(_)
            | flow_typing_implicit_instantiation_check::Operation::Constructor(_, _)
                if result_tparam.is_const =>
            {
                // Adjust 'const' type parameters. Keeping container reasons for
                // compatibility with existing code. *)
                natural_inference::convert_literal_type_to_const(call_loc, cx, inferred.dupe())
            }
            _ => {
                // Prevent leaking of precise singleton types
                generalize_singletons(cx, &call_loc, has_syntactic_hint, inferred.dupe())
            }
        };
        let result = GeneralizedTarg {
            tparam: result_tparam,
            inferred,
            generalized: generalized.dupe(),
        };
        result_map.insert(name.dupe(), result);
        result_list.push((generalized, name.dupe()));
    }

    result_list.reverse();
    Ok((result_map, result_list))
}

fn check_fun<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    tparams: &[TypeParam],
    tparams_map: &BTreeMap<SubstName, TypeParam>,
    return_t: &Type,
    check: &ImplicitInstantiationCheck,
) -> Result<
    (
        Vec<(SubstName, Type, Type, bool)>,
        Marked<SubstName>,
        Option<Type>,
    ),
    FlowJsException,
> {
    let mut visitor = ImplicitInstantiationVisitor { tparams_map, cx };
    let tparam_names: FlowOrdSet<SubstName> =
        tparams.iter().fold(FlowOrdSet::new(), |mut set, tparam| {
            set.insert(tparam.name.dupe());
            set
        });
    let (marked_tparams, _) = visitor.type_(
        cx,
        Polarity::Positive,
        (Marked::new(), tparam_names),
        return_t,
    );
    check_instantiation::<Obs>(cx, tparams, marked_tparams, check)
}

fn check_react_fun<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    tparams: &[TypeParam],
    tparams_map: &BTreeMap<SubstName, TypeParam>,
    props: Option<&Type>,
    check: &ImplicitInstantiationCheck,
) -> Result<
    (
        Vec<(SubstName, Type, Type, bool)>,
        Marked<SubstName>,
        Option<Type>,
    ),
    FlowJsException,
> {
    match props {
        None => {
            let marked_tparams = Marked::new();
            check_instantiation::<Obs>(cx, tparams, marked_tparams, check)
        }
        // The return of a React component when it is createElement-ed isn't actually the return type denoted on the
        // component. Instead, it is a ExactReactElement_DEPRECATED<typeof Component>. In order to get the
        // polarities for the type parameters in the return, it is sufficient to look at the Props
        // type and use the polarities there.
        //
        // In practice, the props accessible via the element are read-only, so a possible future improvement
        // here would only look at the properties on the Props type with a covariant polarity instead of the
        // Neutral default that will be common due to syntactic conveniences.
        Some(props) => check_fun::<Obs>(cx, tparams, tparams_map, props, check),
    }
}

// let check_instance cx ~tparams ~implicit_instantiation =
fn check_instance<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    tparams: &[TypeParam],
    check: &ImplicitInstantiationCheck,
) -> Result<
    (
        Vec<(SubstName, Type, Type, bool)>,
        Marked<SubstName>,
        Option<Type>,
    ),
    FlowJsException,
> {
    let marked_tparams = tparams.iter().fold(Marked::new(), |mut marked, tparam| {
        match marked.add(tparam.name.dupe(), tparam.polarity) {
            None => {}
            Some(_) => {}
        }
        marked
    });
    check_instantiation::<Obs>(cx, tparams, marked_tparams, check)
}

fn implicitly_instantiate<'cx, Obs: Observer>(
    cx: &Context<'cx>,
    check: &ImplicitInstantiationCheck,
) -> Result<
    (
        Vec<(SubstName, Type, Type, bool)>,
        Marked<SubstName>,
        BTreeMap<SubstName, TypeParam>,
        Option<Type>,
    ),
    FlowJsException,
> {
    let (_, ref tparams, ref t) = check.poly_t;
    let tparams_list: Vec<TypeParam> = tparams.iter().duped().collect();
    let tparams_map: BTreeMap<SubstName, TypeParam> =
        tparams_list.iter().fold(BTreeMap::new(), |mut map, x| {
            map.insert(x.name.dupe(), x.dupe());
            map
        });

    let (_, _, ref op) = check.operation;
    let resolved_t = get_t(cx, t);

    if let flow_typing_implicit_instantiation_check::Operation::SubtypeLowerPoly(_) = op {
        let marked_tparams = Marked::new();
        let (inferred_targ_list, marked_tparams, tout) =
            check_instantiation::<Obs>(cx, &tparams_list, marked_tparams, check)?;
        return Ok((inferred_targ_list, marked_tparams, tparams_map, tout));
    }
    let TypeInner::DefT(_, def_t) = resolved_t.deref() else {
        // There are no other valid cases of implicit instantiation, but it is still possible
        // reach this case via non-sensical cases that usually are downstream of some other error.
        // Since there's no reasonable thing to do in these cases we just ignore it.
        return Ok((Vec::new(), Marked::new(), tparams_map, None));
    };
    let (inferred_targ_list, marked_tparams, tout) = match (def_t.deref(), op) {
        (DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData { config, .. }), _) => {
            check_react_fun::<Obs>(cx, &tparams_list, &tparams_map, Some(config), check)?
        }
        (
            DefTInner::FunT(_, funtype),
            flow_typing_implicit_instantiation_check::Operation::ReactJSX { .. },
        ) => {
            let props = funtype.params.first().map(|p| &p.1);
            check_react_fun::<Obs>(cx, &tparams_list, &tparams_map, props, check)?
        }
        (DefTInner::FunT(_, funtype), _) => {
            check_fun::<Obs>(cx, &tparams_list, &tparams_map, &funtype.return_t, check)?
        }
        (
            DefTInner::ClassT(inner),
            flow_typing_implicit_instantiation_check::Operation::Call(_),
        ) if matches!(inner.deref(), TypeInner::ThisInstanceT(..)) => {
            // This case is hit when calling a static function. We will implicitly
            // instantiate the type variables on the class, but using an instance's
            // type params in a static method does not make sense. We ignore this case
            // intentionally
            (Vec::new(), Marked::new(), None)
        }
        (DefTInner::ClassT(inner), _) if matches!(inner.deref(), TypeInner::ThisInstanceT(..)) => {
            check_instance::<Obs>(cx, &tparams_list, check)?
        }
        // There are no other valid cases of implicit instantiation, but it is still possible
        // reach this case via non-sensical cases that usually are downstream of some other error.
        // Since there's no reasonable thing to do in these cases we just ignore it.
        _ => (Vec::new(), Marked::new(), None),
    };

    Ok((inferred_targ_list, marked_tparams, tparams_map, tout))
}

pub mod pin_types {

    use super::*;

    struct PinTypesObserver;

    impl Observer for PinTypesObserver {
        fn on_constant_tparam_missing_bounds<'cx>(
            cx: &Context<'cx>,
            tparam: &TypeParam,
        ) -> Result<InferredTarg, FlowJsException> {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EInternal(Box::new((
                    tparam.reason.loc().dupe(),
                    flow_typing_errors::error_message::InternalError::ImplicitInstantiationInvariant(
                        "Constant tparam is unsupported.".into(),
                    ),
                ))),
            )?;
            Ok(InferredTarg {
                tparam: tparam.dupe(),
                inferred: any_t::error(tparam.reason.dupe()),
            })
        }

        fn on_pinned_tparam<'cx>(
            _cx: &Context<'cx>,
            tparam: &TypeParam,
            inferred: Type,
        ) -> InferredTarg {
            InferredTarg {
                tparam: tparam.dupe(),
                inferred,
            }
        }

        fn on_missing_bounds<'cx>(
            cx: &Context<'cx>,
            _use_op: &UseOp,
            tparam: &TypeParam,
            tparam_binder_reason: &Reason,
            _instantiation_reason: &Reason,
        ) -> Result<InferredTarg, FlowJsException> {
            Ok(InferredTarg {
                tparam: tparam.dupe(),
                inferred: flow_typing_tvar::mk(cx, tparam_binder_reason.dupe()),
            })
        }

        fn on_upper_non_t<'cx>(
            cx: &Context<'cx>,
            _use_op: &UseOp,
            _u: &UseT<Context<'cx>>,
            tparam: &TypeParam,
            tparam_binder_reason: &Reason,
            _instantiation_reason: &Reason,
        ) -> Result<InferredTarg, FlowJsException> {
            Ok(InferredTarg {
                tparam: tparam.dupe(),
                inferred: flow_typing_tvar::mk(cx, tparam_binder_reason.dupe()),
            })
        }
    }

    pub fn pin_type<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        let polarity = Polarity::Neutral;
        let tparam = TypeParam::new(TypeParamInner {
            reason: reason.dupe(),
            name: SubstName::name("".into()),
            bound: mixed_t::why(reason.dupe()),
            polarity,
            default: None,
            is_this: false,
            is_const: false,
        });
        let InferredTarg { inferred, .. } = make_pin_type::<PinTypesObserver>(
            cx,
            &use_op,
            &tparam,
            Some(polarity),
            &None,
            reason,
            t,
        )?;
        Ok(inferred)
    }
} // mod pin_types

pub mod instantiation_solver {

    use super::*;

    struct MainObserver;

    impl Observer for MainObserver {
        fn on_constant_tparam_missing_bounds<'cx>(
            cx: &Context<'cx>,
            tparam: &TypeParam,
        ) -> Result<InferredTarg, FlowJsException> {
            let inferred = match &tparam.default {
                None => type_util::mod_reason_of_t(
                    &|r: Reason| r.update_desc(|d| VirtualReasonDesc::RTypeParamBound(Arc::new(d))),
                    &tparam.bound,
                ),
                Some(t) => type_util::mod_reason_of_t(
                    &|r: Reason| {
                        r.update_desc(|d| VirtualReasonDesc::RTypeParamDefault(Arc::new(d)))
                    },
                    t,
                ),
            };
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Constant type parameter {} is pinned to {}",
                    tparam.name.string_of_subst_name(),
                    flow_typing_debug::dump_t(Some(3), cx, &inferred),
                )]
            });
            Ok(InferredTarg {
                tparam: tparam.dupe(),
                inferred,
            })
        }

        fn on_pinned_tparam<'cx>(
            cx: &Context<'cx>,
            tparam: &TypeParam,
            inferred: Type,
        ) -> InferredTarg {
            // Debug_js.Verbose.print_if_verbose_lazy cx
            //   (lazy [spf "Type parameter %s is pinned to %s"
            //     (Subst_name.string_of_subst_name tparam.name) (Debug_js.dump_t cx ~depth:3 inferred)])
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Type parameter {} is pinned to {}",
                    tparam.name.string_of_subst_name(),
                    flow_typing_debug::dump_t(Some(3), cx, &inferred),
                )]
            });
            InferredTarg {
                tparam: tparam.dupe(),
                inferred,
            }
        }

        fn on_missing_bounds<'cx>(
            cx: &Context<'cx>,
            use_op: &UseOp,
            tparam: &TypeParam,
            tparam_binder_reason: &Reason,
            instantiation_reason: &Reason,
        ) -> Result<InferredTarg, FlowJsException> {
            match &tparam.default {
                Some(inferred) => {
                    let inferred = type_util::mod_reason_of_t(
                        &|r: Reason| {
                            r.update_desc(|d| VirtualReasonDesc::RTypeParamDefault(Arc::new(d)))
                        },
                        inferred,
                    );
                    Ok(InferredTarg {
                        tparam: tparam.dupe(),
                        inferred,
                    })
                }
                None => {
                    if !matches!(
                        *cx.typing_mode(),
                        flow_typing_context::TypingMode::CheckingMode
                    ) {
                        Ok(InferredTarg {
                            tparam: tparam.dupe(),
                            inferred: cx.mk_placeholder(tparam_binder_reason.dupe()),
                        })
                    } else {
                        flow_js_utils::add_output(
                            cx,
                            ErrorMessage::EImplicitInstantiationUnderconstrainedError(Box::new(
                                EImplicitInstantiationUnderconstrainedErrorData {
                                    bound: tparam.name.string_of_subst_name().dupe(),
                                    reason_call: instantiation_reason.dupe(),
                                    reason_tparam: tparam_binder_reason.dupe(),
                                    use_op: use_op.dupe(),
                                },
                            )),
                        )?;
                        Ok(InferredTarg {
                            tparam: tparam.dupe(),
                            inferred: any_t::why(
                                AnySource::AnyError(None),
                                tparam_binder_reason.dupe(),
                            ),
                        })
                    }
                }
            }
        }

        fn on_upper_non_t<'cx>(
            cx: &Context<'cx>,
            use_op: &UseOp,
            u: &UseT<Context<'cx>>,
            tparam: &TypeParam,
            tparam_binder_reason: &Reason,
            instantiation_reason: &Reason,
        ) -> Result<InferredTarg, FlowJsException> {
            if !matches!(
                *cx.typing_mode(),
                flow_typing_context::TypingMode::CheckingMode
            ) {
                Ok(InferredTarg {
                    tparam: tparam.dupe(),
                    inferred: cx.mk_placeholder(tparam_binder_reason.dupe()),
                })
            } else {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![
                        "Underconstrained due to upper_non_t".to_string(),
                        string_of_use_ctor(u),
                    ]
                });
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EImplicitInstantiationUnderconstrainedError(Box::new(
                        EImplicitInstantiationUnderconstrainedErrorData {
                            bound: tparam.name.string_of_subst_name().dupe(),
                            reason_call: instantiation_reason.dupe(),
                            reason_tparam: tparam_binder_reason.dupe(),
                            use_op: use_op.dupe(),
                        },
                    )),
                )?;
                Ok(InferredTarg {
                    tparam: tparam.dupe(),
                    inferred: any_t::why(AnySource::AnyError(None), tparam_binder_reason.dupe()),
                })
            }
        }
    }

    pub fn pin_type<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        tparam: &TypeParam,
        polarity: Option<Polarity>,
        default_bound: Option<Type>,
        reason: &Reason,
        t: &Type,
    ) -> Result<InferredTarg, FlowJsException> {
        make_pin_type::<MainObserver>(cx, &use_op, tparam, polarity, &default_bound, reason, t)
    }

    pub fn solve_targs<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        allow_underconstrained: bool,
        has_syntactic_hint: bool,
        return_hint: Option<(Type, flow_common::hint::HintKind)>,
        check: &ImplicitInstantiationCheck,
    ) -> Result<
        (
            BTreeMap<subst_name::SubstName, GeneralizedTarg>,
            Vec<(Type, subst_name::SubstName)>,
        ),
        FlowJsException,
    > {
        // Force pending lazy tvars (e.g. from qualified type lookups like React.Foo
        // in explicit type args) so their errors are captured in init_errors below,
        // rather than being discarded by the error filtering in the finally block.
        for s in cx.post_component_tvar_forcing_states().iter() {
            cx.force_fully_resolved_tvar(s);
        }
        // Push a preservation level OUTSIDE run_and_rolled_back_cache so it
        // sits below the rollback truncation line and survives the restore.
        // Entries moved here remain visible to add() (which checks all levels),
        // so subsequent operations still get cache hits — exactly matching the
        // original behavior where the captured snapshot stayed in the live cache.
        let preserve_level = cx.constraint_cache().level_count();
        cx.constraint_cache_mut().push_level();
        let mut preserved = false;
        let result = cx.run_and_rolled_back_cache(|| {
            let init_errors = cx.errors();
            let mut cache_snapshot = match &return_hint {
                Some((_, flow_common::hint::HintKind::BestEffortHint)) => {
                    Some(cx.take_cache_snapshot())
                }
                _ => None,
            };

            let (inferred_targ_list, marked_tparams, tparams_map, tout) =
                implicitly_instantiate::<MainObserver>(cx, check)?;
            let errors_before_using_return_hint = cx.errors();
            let has_new_errors = !init_errors.ptr_eq(&errors_before_using_return_hint);
            // Capture constraint cache right after implicitly_instantiate, before
            // the return hint flow or pin_types can add entries that may be
            // associated with error-producing checks. Only safe if
            // implicitly_instantiate did not produce errors.
            // if not has_new_errors then preserved_constraint_cache := Some !constraint_cache;
            if !has_new_errors {
                cx.constraint_cache_mut()
                    .move_top_entries_to_level(preserve_level);
                preserved = true;
            }

            let (inferred_targ_list, marked_tparams, tparams_map, has_new_errors) =
                match (&return_hint, &tout) {
                    (_, None) | (None, _) => (
                        inferred_targ_list,
                        marked_tparams,
                        tparams_map,
                        has_new_errors,
                    ),
                    (Some((hint, kind)), Some(tout)) => {
                        let speculative_exn: Option<FlowJsException> =
                            match FlowJs::flow_t(cx, tout, hint) {
                                Ok(()) => None,
                                Err(FlowJsException::WorkerCanceled(c)) => {
                                    return Err(FlowJsException::WorkerCanceled(c));
                                }
                                Err(FlowJsException::TimedOut(t)) => {
                                    return Err(FlowJsException::TimedOut(t));
                                }
                                Err(e) => Some(e),
                            };

                        let errors_after_using_return_hint = cx.errors();
                        let return_hint_has_errors = !errors_before_using_return_hint
                            .ptr_eq(&errors_after_using_return_hint);

                        if (speculative_exn.is_some() || return_hint_has_errors)
                            && *kind == flow_common::hint::HintKind::BestEffortHint
                        {
                            // Restore state
                            cx.restore_cache_snapshot(cache_snapshot.take().expect(
                                "best-effort return hints should have captured a cache snapshot",
                            ));
                            cx.reset_errors(init_errors.dupe());
                            // Clear the preservation level before retry so the
                            // re-run starts from the same base state as the original.
                            cx.constraint_cache_mut().clear_level(preserve_level);
                            preserved = false;
                            // Re-run the implicit instantiation
                            let (inferred_targ_list, marked_tparams, tparams_map, _tout) =
                                implicitly_instantiate::<MainObserver>(cx, check)?;
                            let has_new_errors = !init_errors.ptr_eq(&cx.errors());
                            // Re-capture constraint cache after retry
                            if !has_new_errors {
                                cx.constraint_cache_mut()
                                    .move_top_entries_to_level(preserve_level);
                                preserved = true;
                            }
                            (
                                inferred_targ_list,
                                marked_tparams,
                                tparams_map,
                                has_new_errors,
                            )
                        } else {
                            // We're keeping the results with the current hint, but if there was
                            // an exception that we caught, we need to rethrow it.
                            if let Some(e) = speculative_exn {
                                Err(e)?;
                            }
                            (
                                inferred_targ_list,
                                marked_tparams,
                                tparams_map,
                                has_new_errors,
                            )
                        }
                    }
                };

            cx.reset_errors(Default::default());

            let result = pin_types::<MainObserver>(
                cx,
                &use_op,
                has_new_errors,
                allow_underconstrained,
                has_syntactic_hint,
                &inferred_targ_list,
                &marked_tparams,
                &tparams_map,
                check,
            );
            let implicit_instantiation_errors = cx.errors().filter(|error| {
                // Since we will be performing the same check again using the solution
                // of the implicit instantiation, we only need to keep errors related
                // to pinning types, eg. [underconstrained-implicit-instantiation].
                matches!(
                    error.msg,
                    ErrorMessage::EImplicitInstantiationUnderconstrainedError(box EImplicitInstantiationUnderconstrainedErrorData { .. })
                        | ErrorMessage::EInternal(box (_, _))
                )
            });
            cx.reset_errors(init_errors.union(&implicit_instantiation_errors));
            result
        });
        // After run_and_rolled_back_cache truncates above the preserve level,
        // the preserve level survives with the entries from implicitly_instantiate.
        // If preserved, leave the level as-is — entries are visible to add()/fold().
        // If not preserved, discard the empty preserve level.
        // Base.Option.iter !preserved_constraint_cache ~f:(fun cc -> constraint_cache := cc);
        if !preserved {
            cx.constraint_cache_mut().truncate_to(preserve_level);
        }
        result
    }

    pub(crate) fn solve_conditional_type_targs<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: &UseOp,
        reason: &Reason,
        tparams: &[TypeParam],
        check_t: &Type,
        extends_t: &Type,
        true_t: &Type,
    ) -> Result<Option<FlowOrdMap<SubstName, Type>>, FlowJsException> {
        let mut subst_map = FlowOrdMap::new();
        let mut inferred_targ_list: Vec<(SubstName, Type, Type)> = Vec::new();
        for tp in tparams {
            let targ = instantiation_utils::implicit_type_argument::mk_targ(cx, tp, reason, reason);
            subst_map.insert(tp.name.dupe(), targ.dupe());
            inferred_targ_list.push((tp.name.dupe(), targ, tp.bound.dupe()));
        }

        //   if speculative_subtyping_succeeds cx trace ~use_op check_t
        //        (Type_subst.subst cx ~use_op:unknown_use subst_map extends_t) then
        let extends_subst = type_subst::subst(
            cx,
            Some(unknown_use()),
            true,
            false,
            type_subst::Purpose::Normal,
            &subst_map,
            extends_t.dupe(),
        );
        if speculative_subtyping_succeeds(cx, trace, use_op.dupe(), check_t, &extends_subst)? {
            let (tparams_map, tparams_set) = tparams.iter().fold(
                (BTreeMap::new(), FlowOrdSet::new()),
                |(mut map, mut set), tp| {
                    map.insert(tp.name.dupe(), tp.dupe());
                    set.insert(tp.name.dupe());
                    (map, set)
                },
            );
            let (marked_tparams, _) = {
                let mut visitor = ImplicitInstantiationVisitor {
                    tparams_map: &tparams_map,
                    cx,
                };
                visitor.type_(cx, Polarity::Positive, (Marked::new(), tparams_set), true_t)
            };

            let mut result_map = FlowOrdMap::new();
            for (name, targ, bound) in &inferred_targ_list {
                let tparam = match tparams_map.get(name) {
                    Some(tp) => tp,
                    None => return Ok(None),
                };
                let polarity = marked_tparams.get(name);
                let InferredTarg { inferred, .. } = make_pin_type::<MainObserver>(
                    cx,
                    use_op,
                    tparam,
                    polarity,
                    &Some(bound.dupe()),
                    reason,
                    targ,
                )?;
                if speculative_subtyping_succeeds(cx, trace, unknown_use(), &inferred, bound)? {
                    result_map.insert(name.dupe(), inferred);
                } else {
                    return Ok(None);
                }
            }
            Ok(Some(result_map))
        } else {
            Ok(None)
        }
    }

    pub fn fold<'cx, Acc>(
        implicit_instantiation_cx: &Context<'cx>,
        cx: &Context<'cx>,
        f: impl Fn(
            &Context,
            Acc,
            &ImplicitInstantiationCheck,
            &BTreeMap<SubstName, GeneralizedTarg>,
        ) -> Result<Acc, FlowJsException>,
        init: Acc,
        post: impl FnOnce(&Context, &Context),
        implicit_instantiation_checks: &[ImplicitInstantiationCheck],
    ) -> Result<Acc, FlowJsException> {
        let r = implicit_instantiation_checks
            .iter()
            .try_fold(init, |acc, check| {
                let (ref use_op, _, _) = check.operation;
                let (pinned, _) = solve_targs(
                    implicit_instantiation_cx,
                    use_op.dupe(),
                    false,
                    false,
                    None,
                    check,
                )?;
                f(implicit_instantiation_cx, acc, check, &pinned)
            })?;
        post(cx, implicit_instantiation_cx);
        Ok(r)
    }
}

pub mod kit {
    use super::instantiation_solver::solve_conditional_type_targs;
    use super::instantiation_solver::solve_targs;
    use super::*;

    fn instantiate_poly_with_subst_map<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        poly_t: &Type,
        subst_map: &BTreeMap<SubstName, GeneralizedTarg>,
        use_op: &UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
    ) -> Result<Type, FlowJsException> {
        let generalized_targ_map: BTreeMap<SubstName, GeneralizedTarg> = subst_map
            .iter()
            .map(
                |(name, gt)| -> Result<(SubstName, GeneralizedTarg), FlowJsException> {
                    // Create an OpenT indirection of inferred result.
                    // We specifically want the inferred type argument to have RTypeParam reason desc,
                    // so that we can detect loops in type expansion.
                    // See Instantiation_utils.ImplicitTypeArgument.abstract_targ.
                    //
                    // This OpenT indirection is also needed for performance purposes, since it prevents
                    // unnecessary deep substitution traversals.
                    let generalized_prime = instantiation_utils::implicit_type_argument::mk_targ(
                        cx,
                        &gt.tparam,
                        reason_op,
                        reason_tapp,
                    );
                    FlowJs::rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        Some(true),
                        &gt.generalized,
                        &generalized_prime,
                    )?;
                    Ok((
                        name.dupe(),
                        GeneralizedTarg {
                            tparam: gt.tparam.dupe(),
                            inferred: gt.inferred.dupe(),
                            generalized: generalized_prime,
                        },
                    ))
                },
            )
            .collect::<Result<BTreeMap<_, _>, FlowJsException>>()?;

        let final_subst_map: FlowOrdMap<SubstName, Type> = generalized_targ_map
            .iter()
            .map(|(name, gt)| (name.dupe(), gt.generalized.dupe()))
            .collect();

        for gt in generalized_targ_map.values() {
            let frame = UseOp::Frame(
                std::sync::Arc::new(VirtualFrameUseOp::TypeParamBound {
                    name: gt.tparam.name.dupe(),
                }),
                std::sync::Arc::new(use_op.dupe()),
            );
            let bound_subst = type_subst::subst(
                cx,
                Some(use_op.dupe()),
                true,
                false,
                type_subst::Purpose::Normal,
                &final_subst_map,
                gt.tparam.bound.dupe(),
            );
            FlowJs::rec_flow_t(cx, trace, frame, &gt.generalized, &bound_subst)?;
        }

        let result = type_subst::subst(
            cx,
            Some(use_op.dupe()),
            true,
            false,
            type_subst::Purpose::Normal,
            &final_subst_map,
            poly_t.dupe(),
        );
        FlowJs::reposition(
            cx,
            Some(trace),
            reason_tapp.loc().dupe(),
            None,
            None,
            result,
        )
    }

    pub fn run_call<'cx>(
        cx: &Context<'cx>,
        check: &ImplicitInstantiationCheck,
        return_hint: &LazyHintT<Context<'cx>>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
    ) -> Result<(Type, Vec<(Type, subst_name::SubstName)>), FlowJsException> {
        let (allow_underconstrained, rh) = match (return_hint.1)(cx, false, None, reason_op.dupe())?
        {
            HintEvalResult::HintAvailable(t, kind) => (true, Some((t, kind))),
            HintEvalResult::DecompositionError => (true, None),
            HintEvalResult::NoHint | HintEvalResult::EncounteredPlaceholder => (false, None),
        };

        // The `has_syntactic_hint` flag will inform the decision on whether to keep
        // precise literal types in the instantiation result. Here, we avoid using
        // Type_env.has_hint, as this function always returns `false` in non-checking
        // mode.
        let has_syntactic_hint = {
            let env = cx.environment();
            env.hint_map
                .get_ordinary(reason_op.loc())
                .map(|h| h.0)
                .unwrap_or(false)
        };

        cx.run_in_implicit_instantiation_mode(|| {
            let (_, _, ref t) = check.poly_t;
            let (soln, inferred_targs) = solve_targs(
                cx,
                use_op.dupe(),
                allow_underconstrained,
                has_syntactic_hint,
                rh,
                check,
            )?;
            let result_t = instantiate_poly_with_subst_map(
                cx,
                trace,
                t,
                &soln,
                &use_op,
                reason_op,
                reason_tapp,
            )?;
            Ok((result_t, inferred_targs))
        })
    }

    pub fn run_render_extractor<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        let name = SubstName::name("T".into());
        let bound = mixed_t::make(reason.dupe());
        let id = cx.make_generic_id(name.dupe(), reason.loc());
        let generic_t = Type::new(TypeInner::GenericT(Box::new(GenericTData {
            reason: reason.dupe(),
            name: name.dupe(),
            bound: bound.dupe(),
            no_infer: false,
            id,
        })));
        let tparam = TypeParam::new(TypeParamInner {
            reason: reason.dupe(),
            name: name.dupe(),
            bound: bound.dupe(),
            polarity: Polarity::Positive,
            default: None,
            is_this: false,
            is_const: false,
        });

        let result = cx.run_in_implicit_instantiation_mode(|| {
            let extends_t = Type::new(TypeInner::DefT(
                reason.dupe(),
                DefT::new(DefTInner::ReactAbstractComponentT(Box::new(
                    ReactAbstractComponentTData {
                        config: empty_t::why(reason.dupe()),
                        renders: generic_t.dupe(),
                        component_kind: ComponentKind::Structural,
                    },
                ))),
            ));
            solve_conditional_type_targs(
                cx,
                DepthTrace::dummy_trace(),
                &use_op,
                reason,
                &[tparam],
                t,
                &extends_t,
                &generic_t,
            )
        });
        match result? {
            Some(subst_map) => Ok(type_subst::subst(
                cx,
                Some(unknown_use()),
                true,
                false,
                type_subst::Purpose::Normal,
                &subst_map,
                generic_t,
            )),
            None => Ok(empty_t::why(reason.dupe())),
        }
    }

    // TODO: await should look up Promise in the environment instead of going directly to
    // the core definition. Otherwise, the following won't work with a polyfilled Promise! **)
    // If argument is a Promise<T>, then (await argument) returns T.
    // otherwise it just returns the argument type.
    // TODO update this comment when recursive unwrapping of Promise is done.
    pub fn run_await<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        let name = SubstName::name("T".into());
        let bound = mixed_t::make(reason.dupe());
        let id = cx.make_generic_id(name.dupe(), reason.loc());
        let generic_t = Type::new(TypeInner::GenericT(Box::new(GenericTData {
            reason: reason.dupe(),
            name: name.dupe(),
            bound: bound.dupe(),
            no_infer: false,
            id,
        })));
        let tparam = TypeParam::new(TypeParamInner {
            reason: reason.dupe(),
            name: name.dupe(),
            bound: bound.dupe(),
            polarity: Polarity::Positive,
            default: None,
            is_this: false,
            is_const: false,
        });

        let result = cx.run_in_implicit_instantiation_mode(|| {
            let promise_t = flow_js_utils::lookup_builtin_type(cx, "Promise", reason.dupe());
            let extends_t = type_util::typeapp(
                false,
                false,
                reason.dupe(),
                promise_t,
                vec![generic_t.dupe()],
            );
            solve_conditional_type_targs(
                cx,
                DepthTrace::dummy_trace(),
                &use_op,
                reason,
                &[tparam],
                t,
                &extends_t,
                &generic_t,
            )
        });
        match result? {
            Some(subst_map) => {
                flow_typing_debug::verbose::print_if_verbose(
                    cx,
                    None,
                    None,
                    None,
                    vec!["We are awaiting a Promise.".to_string()],
                );
                Ok(type_subst::subst(
                    cx,
                    Some(unknown_use()),
                    true,
                    false,
                    type_subst::Purpose::Normal,
                    &subst_map,
                    generic_t,
                ))
            }
            None => {
                flow_typing_debug::verbose::print_if_verbose(
                    cx,
                    None,
                    None,
                    None,
                    vec!["We are awaiting a non-Promise.".to_string()],
                );
                Ok(t.dupe())
            }
        }
    }

    pub fn run_monomorphize<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        tparams: Vec1<TypeParam>,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        let mut subst_map = BTreeMap::new();
        for tparam in tparams.iter() {
            let targ = instantiation_utils::implicit_type_argument::mk_targ(
                cx,
                tparam,
                reason_op,
                reason_tapp,
            );
            let InferredTarg {
                tparam: tp,
                inferred,
            } = super::instantiation_solver::pin_type(
                cx,
                use_op.dupe(),
                tparam,
                None,
                None,
                reason_op,
                &targ,
            )?;
            let gt = GeneralizedTarg {
                tparam: tp.dupe(),
                inferred: inferred.dupe(),
                generalized: inferred,
            };
            subst_map.insert(tp.name.dupe(), gt);
        }
        instantiate_poly_with_subst_map(cx, trace, t, &subst_map, &use_op, reason_op, reason_tapp)
    }

    pub fn run_conditional<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason: &Reason,
        tparams: &[TypeParam],
        check_t: &Type,
        extends_t: &Type,
        true_t: &Type,
        false_t: &Type,
    ) -> Result<Type, FlowJsException> {
        if flow_js_utils::tvar_visitors::has_placeholders(cx, check_t)
            || flow_js_utils::tvar_visitors::has_placeholders(cx, extends_t)
            || flow_js_utils::tvar_visitors::has_placeholders(cx, true_t)
            || flow_js_utils::tvar_visitors::has_placeholders(cx, false_t)
        {
            flow_typing_debug::verbose::print_if_verbose(
                cx,
                None,
                None,
                None,
                vec![
                    "Conditional type refuses to evaluate because we have placeholders".to_string(),
                ],
            );
            // Placeholder in, placeholder out
            return Ok(cx.mk_placeholder(reason.dupe()));
        }
        if cx.in_implicit_instantiation()
            && (flow_js_utils::tvar_visitors::has_unresolved_tvars(cx, check_t)
                || flow_js_utils::tvar_visitors::has_unresolved_tvars(cx, extends_t)
                || flow_js_utils::tvar_visitors::has_unresolved_tvars(cx, true_t)
                || flow_js_utils::tvar_visitors::has_unresolved_tvars(cx, false_t))
        {
            // When we are in nested instantiation, we can't meaningfully decide which branch to take,
            // so we will give up and produce placeholder instead.
            return Ok(cx.mk_placeholder(reason.dupe()));
        }

        let t = {
            let result = cx.run_in_implicit_instantiation_mode(|| {
                solve_conditional_type_targs(
                    cx, trace, &use_op, reason, tparams, check_t, extends_t, true_t,
                )
            });
            match result? {
                // If the subtyping can succeed even when the GenericTs are still abstract, then it must
                // succeed under every possible instantiation, so we can take the true branch.
                Some(subst_map) => {
                    flow_typing_debug::verbose::print_if_verbose(
                        cx,
                        None,
                        None,
                        None,
                        vec!["Conditional type evaluates to the true branch.".to_string()],
                    );
                    type_subst::subst(
                        cx,
                        Some(unknown_use()),
                        true,
                        false,
                        type_subst::Purpose::Normal,
                        &subst_map,
                        true_t.dupe(),
                    )
                }
                None => {
                    let free_vars = {
                        let bound: FlowOrdSet<SubstName> =
                            tparams.iter().map(|tp| tp.name.dupe()).collect();
                        let fv1 = type_subst::free_var_finder(cx, None, check_t);
                        let fv2 = type_subst::free_var_finder(cx, Some(bound), extends_t);
                        fv1.union(fv2)
                    };
                    if free_vars.is_empty() {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec!["Conditional type evaluates to the false branch.".to_string()],
                        );
                        false_t.dupe()
                    } else {
                        let mut any_subst_map: FlowOrdMap<SubstName, Type> = free_vars
                            .iter()
                            .map(|name| (name.dupe(), any_t::placeholder(reason.dupe())))
                            .collect();
                        for tp in tparams {
                            any_subst_map.insert(tp.name.dupe(), any_t::placeholder(reason.dupe()));
                        }
                        let any_check = type_subst::subst(
                            cx,
                            Some(unknown_use()),
                            true,
                            false,
                            type_subst::Purpose::ConditionalTypeAnySubst,
                            &any_subst_map,
                            check_t.dupe(),
                        );
                        let any_extends = type_subst::subst(
                            cx,
                            Some(unknown_use()),
                            true,
                            false,
                            type_subst::Purpose::ConditionalTypeAnySubst,
                            &any_subst_map,
                            extends_t.dupe(),
                        );

                        match speculation_kit::try_singleton_throw_on_failure(
                            cx,
                            trace,
                            any_check,
                            UseT::new(UseTInner::UseT(use_op.dupe(), any_extends)),
                        ) {
                            Err(FlowJsException::SpeculationSingletonError) => {
                                // When all the GenericT and infer types are replaced with any, and subtyping
                                // check still cannot succeed, then we can safely conclude that, in every possible
                                // instantiation, we will always take the false branch
                                flow_typing_debug::verbose::print_if_verbose(
                                    cx,
                                    None,
                                    None,
                                    None,
                                    vec!["Conditional type evaluates to the false branch because we will always enter the false branch.".to_string()],
                                );
                                false_t.dupe()
                            }
                            Ok(()) => {
                                flow_typing_debug::verbose::print_if_verbose(
                                    cx,
                                    None,
                                    None,
                                    None,
                                    vec!["Conditional type is kept abstract.".to_string()],
                                );
                                // A conditional type with GenericTs in check type and extends type is tricky.
                                // We cannot conservatively decide which branch we will take. To maintain
                                // soundness in this general case, we make the type abstract.
                                let bound = Type::new(TypeInner::UnionT(
                                    reason.dupe(),
                                    union_rep::make(
                                        None,
                                        union_rep::UnionKind::UnknownKind,
                                        true_t.dupe(),
                                        false_t.dupe(),
                                        Rc::from([]),
                                    ),
                                ));
                                let nominal_type_args: Rc<[_]> =
                                    [check_t, extends_t, true_t, false_t]
                                        .iter()
                                        .enumerate()
                                        .map(|(i, t)| {
                                            (
                                                SubstName::synthetic(
                                                    i.to_string().into(),
                                                    None,
                                                    vec![],
                                                ),
                                                type_util::reason_of_t(t).dupe(),
                                                (*t).dupe(),
                                                Polarity::Neutral,
                                            )
                                        })
                                        .collect();
                                Type::new(TypeInner::NominalT {
                                    reason: reason.dupe(),
                                    nominal_type: std::rc::Rc::new(NominalType::new(
                                        NominalTypeInner {
                                            nominal_id: nominal::Id::StuckEval(
                                                nominal::StuckEvalKind::StuckEvalForConditionalType,
                                            ),
                                            underlying_t: nominal::UnderlyingT::FullyOpaque,
                                            lower_t: None,
                                            upper_t: Some(bound),
                                            nominal_type_args,
                                        },
                                    )),
                                })
                            }
                            Err(e) => return Err(e),
                        }
                    }
                }
            }
        };
        FlowJs::reposition(cx, Some(trace), reason.loc().dupe(), None, None, t)
    }
}
