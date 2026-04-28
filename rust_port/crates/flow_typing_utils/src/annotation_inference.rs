/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::mk_id;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_errors::error_message::EIncompatibleData;
use flow_typing_errors::error_message::EMissingTypeArgsData;
use flow_typing_errors::error_message::EPropNotFoundInLookupData;
use flow_typing_errors::error_message::EnumInvalidMemberAccessData;
use flow_typing_errors::error_message::EnumMemberUsedAsTypeData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_js::slice_utils;
use flow_typing_type::type_;
use flow_typing_type::type_::ArrayATData;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DestructorSpreadTypeData;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeMap;
use flow_typing_type::type_::TypeTKind;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::aconstraint::AConstraintInner;
use flow_typing_type::type_::aconstraint::AnnotArithTData;
use flow_typing_type::type_::aconstraint::AnnotDeepReadOnlyTData;
use flow_typing_type::type_::aconstraint::AnnotGetPropTData;
use flow_typing_type::type_::aconstraint::AnnotGetTypeFromNamespaceTData;
use flow_typing_type::type_::aconstraint::AnnotLookupTData;
use flow_typing_type::type_::aconstraint::AnnotObjKitTData;
use flow_typing_type::type_::aconstraint::AnnotSpecializeTData;
use flow_typing_type::type_::aconstraint::Op;
use flow_typing_type::type_::aconstraint::OpInner;
use flow_typing_type::type_::constraint::Constraints;
use flow_typing_type::type_::constraint::forcing_state::ForcingState;
use flow_typing_type::type_::constraint::forcing_state::ModuleTypeForcingState;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::type_collector::TypeCollector;
use flow_typing_type::type_util;

use crate::avar;

fn object_like_op<'cx>(op: &OpInner<'cx>) -> bool {
    match op {
        OpInner::AnnotSpecializeT(_)
        | OpInner::AnnotThisSpecializeT { .. }
        | OpInner::AnnotUseTTypeT { .. }
        | OpInner::AnnotConcretizeForImportsExports(_, _)
        | OpInner::AnnotConcretizeForCJSExtractNamedExportsAndTypeExports(_)
        | OpInner::AnnotConcretizeForInspection { .. }
        | OpInner::AnnotImportTypeofT { .. }
        | OpInner::AnnotAssertExportIsTypeT { .. }
        | OpInner::AnnotElemT { .. }
        | OpInner::AnnotGetStaticsT(_)
        | OpInner::AnnotMixinT(_)
        | OpInner::AnnotObjKitT(_)
        | OpInner::AnnotObjTestProtoT(_)
        | OpInner::AnnotArithT(_)
        | OpInner::AnnotUnaryArithT { .. }
        | OpInner::AnnotNotT(_)
        | OpInner::AnnotObjKeyMirror(_)
        | OpInner::AnnotGetKeysT(_)
        | OpInner::AnnotGetEnumT(_)
        | OpInner::AnnotDeepReadOnlyT(_)
        | OpInner::AnnotToStringT { .. } => false,

        OpInner::AnnotGetTypeFromNamespaceT(_)
        | OpInner::AnnotGetPropT(_)
        | OpInner::AnnotGetElemT { .. }
        | OpInner::AnnotLookupT(_)
        | OpInner::AnnotObjRestT { .. }
        | OpInner::AnnotGetValuesT(_) => true,
    }
}

fn primitive_promoting_op<'cx>(op: &OpInner<'cx>) -> bool {
    match op {
        OpInner::AnnotGetPropT(_) | OpInner::AnnotGetElemT { .. } | OpInner::AnnotLookupT(_) => {
            true
        }
        // TODO: enumerate all use types
        _ => false,
    }
}

fn get_fully_resolved_type_state<'cx>(
    cx: &Context<'cx>,
    id: i32,
) -> ForcingState<'cx, Context<'cx>> {
    let (_, constraints) = cx.find_constraints(id);
    match constraints {
        Constraints::FullyResolved(s) => s,
        Constraints::Resolved(_) | Constraints::Unresolved(_) => {
            panic!("unexpected unresolved constraint in annotation inference")
        }
    }
}
fn get_fully_resolved_type_helper<'cx>(dst_cx: &Context<'cx>, cx: &Context<'cx>, id: i32) -> Type {
    let s = get_fully_resolved_type_state(cx, id);
    let t = cx.force_fully_resolved_tvar(&s);
    flow_js_utils::invalid_cyclic_type_validation::validate_type_sig_type(dst_cx, cx, &t);
    t
}

fn get_builtin_typeapp<'cx>(cx: &Context<'cx>, reason: Reason, x: &str, targs: Vec<Type>) -> Type {
    let t = flow_js_utils::lookup_builtin_type(cx, x, reason.dupe());
    type_util::typeapp(false, false, reason, t, targs)
}

// Errors created with [error_unsupported] are actually reported. Compare this to
// errors created with Flow_js_utils.add_output which are recorded in the context
// of the source of the annotations, and are therefore ignored. This function checks
// that dst_cx_ref has been set and uses that as the target context.
//
// The only kind of errors that are reported here are "unsupported" cases. These
// are mostly cases that rely on subtyping, which is not implemented here; most
// commonly evaluating call-like EvalTs and speculation.
fn error_unsupported_reason(
    suggestion: Option<FlowSmolStr>,
    cx: &Context,
    dst_cx: &Context,
    reason: Reason,
    reason_op: Reason,
) -> Type {
    let loc = reason_op.loc().dupe();
    let msg =
        ErrorMessage::EAnnotationInference(Box::new((loc, reason_op.dupe(), reason, suggestion)));
    flow_js_utils::add_annot_inference_error(cx, dst_cx, msg);
    type_::any_t::error(reason_op)
}

fn error_unsupported<'cx>(
    suggestion: Option<FlowSmolStr>,
    cx: &Context,
    dst_cx: &Context,
    reason: Reason,
    op: &Op<'cx>,
) -> Type {
    let reason_op = op.display_reason();
    error_unsupported_reason(suggestion, cx, dst_cx, reason, reason_op)
}

pub(crate) fn error_recursive(cx: &Context, dst_cx: &Context, reason: &Reason) -> Type {
    let loc = reason.loc().dupe();
    let msg = ErrorMessage::ETrivialRecursiveDefinition(Box::new((loc.dupe(), reason.dupe())));
    flow_js_utils::add_annot_inference_error(cx, dst_cx, msg);
    type_::any_t::error(reason.dupe())
}

// (* OCaml: let force_module_type_thunk cx s () = *)
// (*   Type.Constraint.ForcingState.force ~on_error:(fun r -> Error (error_recursive cx r)) s *)
pub fn force_module_type_thunk<'cx>(
    src_cx: Context<'cx>,
    s: ModuleTypeForcingState<'cx, Context<'cx>>,
) -> Rc<dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<type_::ModuleType, Type> + 'cx> {
    let s = Rc::new(s);
    Rc::new(move |_cx: &Context<'cx>, dst_cx: &Context<'cx>| {
        s.force(&src_cx, |r| Err(error_recursive(&src_cx, dst_cx, r)))
    })
}

fn get_lazy_module_type_or_any_src<'cx>(
    resolved_require: &flow_typing_context::ResolvedRequire<'cx>,
) -> Rc<
    flow_lazy::Lazy<
        Context<'cx>,
        Result<type_::ModuleType, type_::AnySource>,
        Box<dyn FnOnce(&Context<'cx>) -> Result<type_::ModuleType, type_::AnySource> + 'cx>,
    >,
> {
    use flow_typing_context::ResolvedRequire;
    use flow_typing_type::type_::AnyErrorKind;
    use flow_typing_type::type_::AnySource;

    match resolved_require {
        ResolvedRequire::TypedModule(f) => {
            let f = f.clone();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                // Use merge_dst_cx if set (during cross-file merge), otherwise cx.
                let dst_cx = cx.merge_dst_cx().unwrap_or_else(|| cx.dupe());
                match f(cx, &dst_cx) {
                    Err(_) => Err(AnySource::AnyError(None)),
                    Ok(module_type) => Ok(module_type),
                }
            })))
        }
        ResolvedRequire::UncheckedModule(_) => {
            Rc::new(flow_lazy::Lazy::new(Box::new(|_cx: &Context| {
                Err(AnySource::Untyped)
            })))
        }
        ResolvedRequire::MissingModule => {
            Rc::new(flow_lazy::Lazy::new(Box::new(|_cx: &Context| {
                Err(AnySource::AnyError(Some(AnyErrorKind::UnresolvedName)))
            })))
        }
    }
}

fn error_internal_reason(cx: &Context, dst_cx: &Context, msg: &str, reason_op: Reason) -> Type {
    let loc = reason_op.loc().dupe();
    let err_msg = ErrorMessage::EInternal(Box::new((
        loc,
        flow_typing_errors::error_message::InternalError::UnexpectedAnnotationInference(msg.into()),
    )));
    flow_js_utils::add_annot_inference_error(cx, dst_cx, err_msg);
    type_::any_t::error(reason_op)
}

fn error_internal<'cx>(cx: &Context, dst_cx: &Context, msg: &str, op: &Op<'cx>) -> Type {
    let reason_op = op.display_reason();
    error_internal_reason(cx, dst_cx, msg, reason_op)
}

fn dummy_trace() -> DepthTrace {
    DepthTrace::dummy_trace()
}

// Repositioning does not seem to have any perceptible impact in annotation
// inference. Instead of replicating the convoluted implementation of Flow_js
// here, we just return the same type intact.
pub fn reposition(_cx: &Context, _loc: ALoc, t: Type) -> Type {
    t
}

/// Returns the effective destination context for error reporting.
/// During cross-file merge, errors should go to the importing file (stored
/// in merge_dst_cx). Otherwise, errors go to cx itself.
fn effective_dst_cx<'cx>(cx: &Context<'cx>) -> Context<'cx> {
    cx.merge_dst_cx().unwrap_or_else(|| cx.dupe())
}

// (*****************)
// (* Instantiation *)
// (*****************)
struct AnnotInstantiationHelper;

impl flow_js_utils::InstantiationHelper for AnnotInstantiationHelper {
    // We will not be solving implicit instantiation problems here. The only case
    // where we will need to use this function is when a PolyT needs to be used
    // as a monomorphic type. In this case, the only sensible thing to do is to
    // use the bound of each parameter as the argument to the intantiation. *)
    fn mk_targ<'cx>(
        _cx: &Context<'cx>,
        typeparam: &type_::TypeParam,
        _reason_op: &Reason,
        _reason_tapp: &Reason,
    ) -> Type {
        typeparam.bound.dupe()
    }

    fn is_subtype<'cx>(
        _cx: &Context<'cx>,
        _trace: DepthTrace,
        _use_op: UseOp,
        _t1: Type,
        _t2: Type,
    ) -> Result<(), FlowJsException> {
        Ok(())
    }

    fn unify<'cx>(
        _cx: &Context<'cx>,
        _trace: DepthTrace,
        _use_op: UseOp,
        _t1: Type,
        _t2: Type,
    ) -> Result<(), FlowJsException> {
        Ok(())
    }

    fn reposition<'cx>(
        _cx: &Context<'cx>,
        _trace: Option<DepthTrace>,
        _loc: ALoc,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        Ok(t)
    }
}

fn instantiate_poly<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    tparams_loc: ALoc,
    ids: Vec<type_::TypeParam>,
    t: Type,
) -> Type {
    let xs = vec1::Vec1::try_from_vec(ids).expect("tparams should be non-empty");
    let (t, _) = flow_js_utils::instantiation_kit::instantiate_poly::<AnnotInstantiationHelper>(
        cx,
        dummy_trace(),
        use_op,
        &reason_op,
        &reason_tapp,
        false,
        tparams_loc,
        &xs,
        t,
    )
    // Annotation inference is never speculative
    .unwrap();
    t
}

fn mk_typeapp_of_poly<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    id: type_::poly::Id,
    tparams_loc: ALoc,
    xs: Vec<type_::TypeParam>,
    t: Type,
    ts: Rc<[Type]>,
) -> Type {
    let xs = vec1::Vec1::try_from_vec(xs).expect("tparams should be non-empty");
    flow_js_utils::instantiation_kit::mk_typeapp_of_poly::<AnnotInstantiationHelper>(
        cx,
        dummy_trace(),
        use_op,
        &reason_op,
        &reason_tapp,
        id,
        tparams_loc,
        &xs,
        t,
        ts,
    )
    // Annotation inference is never speculative
    .unwrap()
}

fn with_concretized_type<'cx>(
    cx: &Context<'cx>,
    r: Reason,
    f: Rc<dyn Fn(Type) -> Type + 'cx>,
    t: Type,
) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotConcretizeForImportsExports(r, f)),
    )
}

// (***********)
// (* GetProp *)
// (***********)

fn cg_lookup_<'cx>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    t: Type,
    reason_op: &Reason,
    propref: &type_::PropRef,
    objt: &Type,
) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotLookupT(Box::new(AnnotLookupTData {
            reason: reason_op.dupe(),
            use_op: use_op.dupe(),
            prop_ref: propref.clone(),
            type_: objt.dupe(),
        }))),
    )
}

struct AnnotGetPropHelper;

impl flow_js_utils::GetPropHelper for AnnotGetPropHelper {
    type R = Type;

    fn error_type<'cx>(
        _cx: &Context<'cx>,
        _trace: DepthTrace,
        reason: Reason,
    ) -> Result<Type, FlowJsException> {
        Ok(type_::any_t::error(reason))
    }

    fn return_<'cx>(
        cx: &Context<'cx>,
        _use_op: UseOp,
        _trace: DepthTrace,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        // We could have just returned `t` here. The OpenT indirection is for compatibility
        // with Flow_js. Specifically, without the OpenT the transformation in
        // https://github.com/facebook/flow/blob/8c3825a1be188e9ade4ad4ed515361bb28c65d8a/src/typing/flow_js.ml#L1744-L1755
        // would fire, causing a divergence in the behavior of this module and Flow_js.
        match t.deref() {
            TypeInner::OpenT(_) => Ok(t),
            _ => {
                let reason = type_util::reason_of_t(&t).dupe();
                Ok(flow_typing_tvar::mk_fully_resolved(cx, reason, t))
            }
        }
    }

    fn dict_read_check<'cx>(
        _cx: &Context<'cx>,
        _trace: DepthTrace,
        _use_op: &UseOp,
        _pair: (&Type, &Type),
    ) -> Result<(), FlowJsException> {
        // We will not be doing subtyping checks in annotation inference.
        Ok(())
    }

    fn reposition<'cx>(
        cx: &Context<'cx>,
        _trace: Option<DepthTrace>,
        loc: ALoc,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        Ok(reposition(cx, loc, t))
    }

    // let cg_lookup cx _trace ~obj_t ~method_accessible:_ t (reason_op, _kind, propref, use_op, _ids) =
    //   cg_lookup_ cx use_op t reason_op propref obj_t
    fn cg_lookup<'cx>(
        cx: &Context<'cx>,
        _trace: DepthTrace,
        obj_t: Type,
        _method_accessible: bool,
        super_t: Type,
        args: (
            Reason,
            type_::LookupKind,
            type_::PropRef,
            UseOp,
            properties::Set,
        ),
    ) -> Result<Type, FlowJsException> {
        let (reason_op, _kind, propref, use_op, _ids) = args;
        Ok(cg_lookup_(
            cx, &use_op, super_t, &reason_op, &propref, &obj_t,
        ))
    }

    fn cg_get_prop<'cx>(
        cx: &Context<'cx>,
        _trace: DepthTrace,
        t: Type,
        args: (UseOp, Reason, Option<i32>, (Reason, Name)),
    ) -> Result<Type, FlowJsException> {
        let (use_op, access_reason, _, (prop_reason, name)) = args;
        Ok(elab_t(
            cx,
            &effective_dst_cx(cx),
            None,
            t,
            Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
                reason: access_reason,
                use_op,
                from_annot: false,
                prop_ref: type_util::mk_named_prop(prop_reason, false, name),
            }))),
        ))
    }

    fn mk_react_dro<'cx>(
        cx: &Context<'cx>,
        _use_op: UseOp,
        dro: &type_::ReactDro,
        t: Type,
    ) -> Type {
        let reason = type_util::reason_of_t(&t).dupe();
        elab_t(
            cx,
            &effective_dst_cx(cx),
            None,
            t,
            Op::new(OpInner::AnnotDeepReadOnlyT(Box::new(
                AnnotDeepReadOnlyTData {
                    reason: reason.dupe(),
                    loc: dro.0.dupe(),
                    dro_type: dro.1.clone(),
                },
            ))),
        )
    }

    fn prop_overlaps_with_indexer() -> Option<
        fn(
            &Context,
            &Name,
            &Reason,
            &Type,
        ) -> Result<bool, flow_utils_concurrency::job_error::JobError>,
    > {
        None
    }
}

/// [ensure_annot_resolved cx reason id] ensures that the annotation constraint
/// associated with [id] has been resolved. If the respective constraint is already
/// resolved then it returns immediately. Otherwise, it resolves [id] immediately
/// to the 'any' type. In the case of an [Anno_op (_, _, dep_id)] constraint we also
/// update the "dependents" set of [dep_id], so that we don't attempt to resolve
/// [id] once again when [dep_id] gets resolved.
fn ensure_annot_resolved<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    reason: Reason,
    id: i32,
) -> Type {
    match cx.find_avar_opt(id) {
        None => get_fully_resolved_type(cx, dst_cx, id),
        Some(constraint) => match constraint.deref() {
            AConstraintInner::AnnotUnresolved { .. } => {
                let t = error_recursive(cx, dst_cx, &reason);
                resolve_id(cx, dst_cx, reason, id, t.dupe());
                t
            }
            AConstraintInner::AnnotOp { id: dep_id, .. } => {
                let dep_id = *dep_id;
                let dep_constraint = cx.find_avar(dep_id);
                dep_constraint.update_deps(|mut deps| {
                    deps.remove(&id);
                    deps
                });
                let t = error_recursive(cx, dst_cx, &reason);
                resolve_id(cx, dst_cx, reason, id, t.dupe());
                t
            }
        },
    }
}

fn get_fully_resolved_type<'cx>(cx: &Context<'cx>, dst_cx: &Context<'cx>, id: i32) -> Type {
    get_fully_resolved_type_helper(dst_cx, cx, id)
}

fn mk_lazy_tvar<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    f: impl FnOnce(&Context<'cx>, i32) + 'cx,
) -> Type {
    let id = mk_id() as i32;
    let tvar = Type::new(TypeInner::OpenT(type_::Tvar::new(reason.dupe(), id as u32)));
    let reason2 = reason.dupe();
    let cx2 = cx.dupe();
    let lazy_fn = move |_cx: &Context<'cx>| {
        avar::unresolved_with_id(&cx2, id, reason2.dupe());
        f(&cx2, id);
        // Before forcing the type constraint of [id] we need to make sure the
        // respective annotation constraint has been processed. If not we infer
        // the empty type.
        let dst_cx2 = cx2.merge_dst_cx().unwrap_or_else(|| cx2.dupe());
        ensure_annot_resolved(&cx2, &dst_cx2, reason2.dupe(), id)
    };
    let forcing_state = ForcingState::of_lazy_t(reason.dupe(), lazy_fn);
    let node = flow_utils_union_find::Node::create_root(Constraints::FullyResolved(forcing_state));
    cx.add_tvar(id, node);
    tvar
}

pub fn mk_sig_tvar<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    resolved: Rc<flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>>,
) -> Type {
    let reason2 = reason.dupe();
    mk_lazy_tvar(cx, reason.dupe(), move |cx, id| {
        let t = resolved.get_forced(cx).dupe();
        let dst_cx = cx.merge_dst_cx().unwrap_or_else(|| cx.dupe());
        resolve_id(cx, &dst_cx, reason2, id, t);
    })
}

/// [resolve_id cx id1 t] resolves an annotation tvar [id1] to a type [t] *
/// - If [t] is a concrete type, we mark [id1] as a resolved annotation tvar and
///   record it as fully resolved in the type graph. *
/// - If [t] is an OpenT (_, id2), then we unify [id1] and [id2]. (See merge_ids.)
fn resolve_id<'cx>(cx: &Context<'cx>, dst_cx: &Context<'cx>, reason: Reason, id: i32, t: Type) {
    match cx.find_avar_opt(id) {
        None => {
            // The avar is already resolved. This happens when the avar is recursively
            // reachable and becomes resolved to any.
        }
        Some(constraints1) => {
            let t = match t.deref() {
                TypeInner::OpenT(tvar) => {
                    ensure_annot_resolved(cx, dst_cx, reason.dupe(), tvar.id() as i32)
                }
                _ => t,
            };
            cx.remove_avar(id);
            if cx.graph().borrow().get(&id).is_some()
                && !get_fully_resolved_type_state(cx, id).already_forced_with_cyclic_error()
            {
                // It is possible that this tvar is cyclic in a bad way, and at this point,
                // the ensure_annot_resolved above has already discovered that and resolve the
                // tvar to any. If we still unconditionally try to add the tvar here, it will
                // undo the work of turning the bad tvar to any.
                //
                // e.g. Consider OpenT(id=1). 1=AnnotT(OpenT(id=1)).
                // ensure_annot_resolved, while visiting the nested OpenT in Annot, will resolve
                // the tvar to any and error, but if we unconditionally add the tvar with
                // t = AnnotT(OpenT(id=1)), the work is undone.
                cx.add_tvar(
                    id,
                    flow_utils_union_find::Node::create_root(Constraints::FullyResolved(
                        ForcingState::of_non_lazy_t(t.dupe()),
                    )),
                );
            }
            let dependents1 = constraints1.deps();
            resolve_dependent_set(cx, dst_cx, reason, &dependents1.borrow(), t);
        }
    }
}

fn resolve_dependent_set<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    reason: Reason,
    dependents: &FlowOrdSet<i32>,
    t: Type,
) {
    cx.iter_annot_dependent_set(
        |id, op| {
            let result = elab_t(cx, dst_cx, None, t.dupe(), op.dupe());
            resolve_id(cx, dst_cx, reason.dupe(), id, result);
        },
        dependents,
    );
}

fn elab_open<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    seen: &FlowOrdSet<i32>,
    reason: Reason,
    id: i32,
    op: Op<'cx>,
) -> Type {
    if seen.contains(&id) {
        return error_recursive(cx, dst_cx, &reason);
    }
    match cx.find_avar_opt(id) {
        None => {
            // [id] may refer to a lazily resolved constraint (e.g. created through
            // [mk_lazy_tvar]). To protect against trying to force recursive lazy
            // structures, we introduce a lazy indirection around the resulting
            // constraint. An example that would have cause this unwanted behavior is
            //
            //   declare var x: {
            //     p: number;
            //     q: typeof (x.p);
            //   };
            //
            // This lazy indirection allows the type of `x` to be resolved, before we
            // attempt to force the constraint for `x.p`.
            let mut seen2 = seen.dupe();
            seen2.insert(id);
            let op2 = op.dupe();
            let resolved = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                // Use merge_dst_cx if set (during cross-file merge), otherwise cx.
                let dst_cx = cx.merge_dst_cx().unwrap_or_else(|| cx.dupe());
                let t = get_fully_resolved_type(cx, &dst_cx, id);
                elab_t(cx, &dst_cx, Some(seen2.dupe()), t, op2.dupe())
            })
                as Box<dyn FnOnce(&Context<'cx>) -> Type>));
            mk_sig_tvar(cx, op.reason(), resolved)
        }
        Some(constraint) => match constraint.deref() {
            AConstraintInner::AnnotUnresolved { .. } | AConstraintInner::AnnotOp { .. } => {
                let fresh_id = avar::constrained(cx, op, id);
                Type::new(TypeInner::OpenT(type_::Tvar::new(reason, fresh_id as u32)))
            }
        },
    }
}

pub fn elab_t<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    seen: Option<FlowOrdSet<i32>>,
    t: Type,
    op: Op<'cx>,
) -> Type {
    let seen = seen.unwrap_or_default();
    match (t.deref(), op.deref()) {
        (
            TypeInner::EvalT {
                type_: inner_t,
                defer_use_t,
                ..
            },
            _,
        ) => {
            let use_op = defer_use_t.0.dupe();
            let reason = defer_use_t.1.dupe();
            match defer_use_t.2.deref() {
                Destructor::ReadOnlyType => {
                    let t = make_readonly(cx, use_op, reason, inner_t.dupe());
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::ReactDRO(box react_dro) => {
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotDeepReadOnlyT(Box::new(
                            AnnotDeepReadOnlyTData {
                                reason: reason.dupe(),
                                loc: react_dro.0.dupe(),
                                dro_type: react_dro.1.clone(),
                            },
                        ))),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::ExactType => {
                    let t = make_exact(cx, reason, inner_t.dupe());
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::PartialType => {
                    let t = make_partial(cx, use_op, reason, inner_t.dupe());
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::RequiredType => {
                    let t = make_required(cx, use_op, reason, inner_t.dupe());
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::SpreadType(box DestructorSpreadTypeData(
                    target,
                    todo_rev,
                    head_slice,
                )) => {
                    let acc: Rc<[flow_typing_type::type_::object::spread::AccElement]> =
                        match head_slice {
                            Some(x) => vec![
                                flow_typing_type::type_::object::spread::AccElement::InlineSlice(
                                    x.clone(),
                                ),
                            ]
                            .into(),
                            None => Rc::from([]),
                        };
                    let state = type_::object::spread::State {
                        todo_rev: todo_rev.clone(),
                        acc,
                        spread_id: flow_common::reason::mk_id() as i32,
                        union_reason: None,
                        curr_resolve_idx: 0,
                    };
                    let t =
                        object_spread(cx, use_op, reason, target.clone(), state, inner_t.dupe());
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::RestType(options, r) => {
                    let state = type_::object::rest::State::One(r.dupe());
                    let t = object_rest_internal(
                        cx,
                        use_op,
                        reason,
                        options.clone(),
                        state,
                        inner_t.dupe(),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::TypeMap(TypeMap::ObjectKeyMirror) => {
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotObjKeyMirror(reason)),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::ValuesType => {
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotGetValuesT(reason)),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::PropertyType { name } => {
                    let reason_op = reason.dupe().replace_desc(
                        flow_common::reason::VirtualReasonDesc::RProperty(Some(name.dupe())),
                    );
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
                            reason: reason_op,
                            use_op,
                            from_annot: true,
                            prop_ref: type_::PropRef::Named {
                                reason: reason.dupe(),
                                name: name.dupe(),
                                from_indexed_access: true,
                            },
                        }))),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::ElementType { index_type } => {
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotGetElemT {
                            reason,
                            use_op,
                            key: index_type.dupe(),
                        }),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                Destructor::EnumType => {
                    let t = elab_t(
                        cx,
                        dst_cx,
                        None,
                        inner_t.dupe(),
                        Op::new(OpInner::AnnotGetEnumT(reason)),
                    );
                    elab_t(cx, dst_cx, None, t, op)
                }
                _ => error_unsupported(None, cx, dst_cx, reason, &op),
            }
        }
        (TypeInner::OpenT(tvar), OpInner::AnnotConcretizeForInspection { .. }) => {
            let t = elab_open(
                cx,
                dst_cx,
                &seen,
                tvar.reason().dupe(),
                tvar.id() as i32,
                op,
            );
            if let TypeInner::OpenT(tvar2) = t.deref() {
                // Force the type so that the concretized results are eagarly added to the collector
                get_fully_resolved_type(cx, dst_cx, tvar2.id() as i32);
            }
            t
        }
        (TypeInner::OpenT(tvar), _) => elab_open(
            cx,
            dst_cx,
            &seen,
            tvar.reason().dupe(),
            tvar.id() as i32,
            op,
        ),
        (TypeInner::AnnotT(r, inner_t, _), _) => {
            let t = reposition(cx, r.loc().dupe(), inner_t.dupe());
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        // The remaining cases are dispatched to elab_t_concrete.
        // This split avoids an extremely long match expression.
        _ => elab_t_concrete(cx, dst_cx, seen, t, op),
    }
}

fn general_error<'cx>(cx: &Context<'cx>, _dst_cx: &Context<'cx>, t: &Type, op: &Op<'cx>) -> Type {
    use flow_typing_type::type_::any_t;

    let reason_op = op.reason();
    let lower = (
        type_util::reason_of_t(t).dupe(),
        flow_js_utils::error_message_kind_of_lower(t),
    );
    let upper = (
        reason_op.dupe(),
        flow_typing_errors::error_message::UpperKind::IncompatibleUnclassified(
            op.string_of_operation().into(),
        ),
    );
    let use_op = op.use_op();
    flow_js_utils::add_output_non_speculating(
        cx,
        flow_typing_errors::error_message::ErrorMessage::EIncompatible(Box::new(
            EIncompatibleData {
                lower,
                upper,
                use_op,
            },
        )),
    );
    any_t::error(reason_op)
}

fn elab_t_concrete<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    seen: FlowOrdSet<i32>,
    t: Type,
    op: Op<'cx>,
) -> Type {
    use flow_typing_type::type_::DefTInner;
    use flow_typing_type::type_::EnumInfoInner;
    use flow_typing_type::type_::PolyTData;
    use flow_typing_type::type_::any_t;

    match (t.deref(), op.deref()) {
        // *******************************************************************
        // UseT TypeT (runtime types derive static types through annotation)
        // *******************************************************************
        // First handle catch-all cases of subtyping_kit.ml
        (TypeInner::MaybeT(reason, _), OpInner::AnnotUseTTypeT { .. })
        | (TypeInner::OptionalT { reason, .. }, OpInner::AnnotUseTTypeT { .. }) => {
            error_unsupported(None, cx, dst_cx, reason.dupe(), &op)
        }
        (
            TypeInner::ThisTypeAppT(box ThisTypeAppTData {
                reason,
                type_,
                this_t,
                targs,
            }),
            OpInner::AnnotUseTTypeT { .. },
        ) => {
            let reason_tapp = reason.dupe();
            let reason_op = op.reason();
            let tc = specialize_class(
                cx,
                type_.dupe(),
                reason_op,
                reason_tapp.dupe(),
                targs.clone(),
            );
            let t = this_specialize(cx, reason_tapp, this_t.dupe(), tc);
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason,
                use_op,
                type_,
                targs,
                from_value,
                ..
            }),
            OpInner::AnnotUseTTypeT { .. },
        ) => {
            // NOTE omitting TypeAppExpansion.push_unless_loop check.
            let reason_op = op.reason();
            let t = mk_typeapp_instance(
                cx,
                use_op.dupe(),
                reason_op,
                reason.dupe(),
                *from_value,
                type_.dupe(),
                targs.clone(),
            );
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        (TypeInner::DefT(def_reason, def_t), OpInner::AnnotUseTTypeT { reason, kind }) => {
            match def_t.deref() {
                // | ( DefT (_, PolyT { tparams = ids; t_out = DefT (_, ReactAbstractComponentT _) as t; _ }),
                //     Annot_UseT_TypeT (reason_op, RenderTypeKind)
                //   ) ->
                //   let subst_map = ...
                //   let t_ = subst cx subst_map t in
                //   elab_t cx t_ op
                DefTInner::PolyT(box PolyTData { tparams, t_out, .. })
                    if matches!(kind, flow_typing_type::type_::TypeTKind::RenderTypeKind)
                        && matches!(
                            t_out.deref(),
                            TypeInner::DefT(_, dt) if matches!(dt.deref(), DefTInner::ReactAbstractComponentT(_))
                        ) =>
                {
                    let mut subst_map = flow_data_structure_wrapper::ord_map::FlowOrdMap::new();
                    for tparam in tparams.iter() {
                        subst_map.insert(tparam.name.dupe(), any_t::untyped(reason.dupe()));
                    }
                    let t_ = flow_typing_flow_common::type_subst::subst(
                        cx,
                        None,
                        true,
                        false,
                        flow_typing_flow_common::type_subst::Purpose::Normal,
                        &subst_map,
                        t_out.dupe(),
                    );
                    elab_t(cx, dst_cx, Some(seen), t_, op)
                }
                DefTInner::PolyT(box PolyTData {
                    tparams_loc,
                    tparams: ids,
                    t_out,
                    id,
                }) if flow_common::files::has_ts_ext(cx.file())
                    && ids.iter().all(|tp| tp.default.is_some()) =>
                {
                    // In .ts files, treat missing type args the same as empty type args (Foo = Foo<>),
                    // matching TypeScript behavior where defaults are used.
                    // Only when all params have defaults; otherwise fall through to EMissingTypeArgs.
                    let reason_tapp = def_reason.dupe();
                    let t = mk_typeapp_of_poly(
                        cx,
                        type_::unknown_use(),
                        reason.dupe(),
                        reason_tapp,
                        id.dupe(),
                        tparams_loc.dupe(),
                        ids.to_vec(),
                        t_out.dupe(),
                        Rc::from([]),
                    );
                    elab_t(cx, dst_cx, Some(seen), t, op)
                }
                DefTInner::PolyT(box PolyTData {
                    tparams_loc,
                    tparams,
                    ..
                }) => {
                    let reason_tapp = def_reason.dupe();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EMissingTypeArgs(
                            Box::new(EMissingTypeArgsData {
                                reason_op: reason.dupe(),
                                reason_tapp,
                                arity_loc: tparams_loc.dupe(),
                                min_arity: tparams.iter().filter(|tp| tp.default.is_none()).count()
                                    as i32,
                                max_arity: tparams.len() as i32,
                            }),
                        ),
                    );
                    any_t::error(reason.dupe())
                }
                DefTInner::ClassT(instance) => match instance.deref() {
                    TypeInner::ThisInstanceT(box ThisInstanceTData {
                        reason: r,
                        instance: i,
                        is_this,
                        subst_name,
                    }) => {
                        let fixed = flow_js_utils::fix_this_instance(
                            cx,
                            reason.dupe(),
                            r.dupe(),
                            i,
                            *is_this,
                            subst_name.dupe(),
                        );
                        let c = Type::new(TypeInner::DefT(
                            def_reason.dupe(),
                            type_::DefT::new(DefTInner::ClassT(fixed)),
                        ));
                        elab_t(cx, dst_cx, Some(seen), c, op)
                    }
                    //  a class value annotation becomes the instance type
                    _ => reposition(cx, reason.loc().dupe(), instance.dupe()),
                },
                // a component syntax value annotation becomes an element of that component
                DefTInner::ReactAbstractComponentT(_) => {
                    get_builtin_typeapp(cx, reason.dupe(), "React$RendersExactly", vec![t.dupe()])
                }
                DefTInner::TypeT(_, l) => l.dupe(),
                // an enum object value annotation becomes the enum value type
                DefTInner::EnumObjectT { enum_value_t, .. } => enum_value_t.dupe(),
                DefTInner::EnumValueT(_) => {
                    let enum_reason = def_reason.dupe();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EEnumError(
                            flow_typing_errors::error_message::EnumErrorKind::EnumMemberUsedAsType(
                                Box::new(EnumMemberUsedAsTypeData {
                                    reason: reason.dupe(),
                                    enum_reason,
                                }),
                            ),
                        ),
                    );
                    any_t::error(reason.dupe())
                }
                _ => {
                    let reason_use = reason.dupe();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EValueUsedAsType {
                            reason_use: reason_use.dupe(),
                        },
                    );
                    any_t::error(reason_use)
                }
            }
        }
        (
            TypeInner::AnyT(_, type_::AnySource::AnyError(_)),
            OpInner::AnnotUseTTypeT { reason, .. },
        ) => {
            // Short-circuit as we already error on the unresolved name.
            any_t::error(reason.dupe())
        }
        (TypeInner::AnyT(_, _), OpInner::AnnotUseTTypeT { reason, .. }) => {
            let reason_use = reason.dupe();
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::EAnyValueUsedAsType {
                    reason_use: reason_use.dupe(),
                },
            );
            any_t::error(reason_use)
        }
        (_, OpInner::AnnotUseTTypeT { reason, .. }) => {
            let reason_use = reason.dupe();
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::EValueUsedAsType {
                    reason_use: reason_use.dupe(),
                },
            );
            any_t::error(reason_use)
        }
        // *****************
        //  `import typeof`
        // *****************
        (_, OpInner::AnnotImportTypeofT { reason, name }) => {
            // Annotation inference is never speculative
            flow_js_utils::import_typeof_t_kit::on_concrete_type(
                cx,
                reason.dupe(),
                name.as_str(),
                &t,
            )
        }
        // ****************
        //  Module exports
        // ****************
        (_, OpInner::AnnotAssertExportIsTypeT { name, .. }) => {
            // Annotation inference is never speculative
            flow_js_utils::assert_export_is_type_t_kit::on_concrete_type(cx, name.dupe(), t)
                .unwrap()
        }
        (_, OpInner::AnnotConcretizeForCJSExtractNamedExportsAndTypeExports(_)) => t,
        // **********************************
        // Wildcards (idx, maybe, optional)
        // **********************************
        (TypeInner::MaybeT(reason, _), _) | (TypeInner::OptionalT { reason, .. }, _) => {
            // These are rare in practice. Will consider adding support if we hit this error case.
            error_unsupported(None, cx, dst_cx, reason.dupe(), &op)
        }
        // *******************
        //  Type applications
        // *******************
        (
            TypeInner::ThisTypeAppT(box ThisTypeAppTData {
                reason,
                type_,
                this_t,
                targs,
            }),
            _,
        ) => {
            let reason_tapp = reason.dupe();
            let reason_op = op.reason();
            let tc = specialize_class(
                cx,
                type_.dupe(),
                reason_op,
                reason_tapp.dupe(),
                targs.clone(),
            );
            let t = this_specialize(cx, reason_tapp, this_t.dupe(), tc);
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason,
                use_op,
                type_,
                targs,
                from_value,
                ..
            }),
            _,
        ) => {
            let reason_op = op.reason();
            let t = mk_typeapp_instance(
                cx,
                use_op.dupe(),
                reason_op,
                reason.dupe(),
                *from_value,
                type_.dupe(),
                targs.clone(),
            );
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        (_, OpInner::AnnotConcretizeForImportsExports(_, f)) => f(t),
        // **************
        //  Opaque types
        // **************
        (TypeInner::NominalT { nominal_type, .. }, OpInner::AnnotToStringT { reason, .. })
            if nominal_type.upper_t.is_some() =>
        {
            let upper_t = nominal_type.upper_t.as_ref().unwrap().dupe();
            elab_t(
                cx,
                dst_cx,
                Some(seen),
                upper_t,
                Op::new(OpInner::AnnotToStringT {
                    orig_t: Some(t.dupe()),
                    reason: reason.dupe(),
                }),
            )
        }
        (
            TypeInner::NominalT {
                reason,
                nominal_type,
            },
            _,
        ) if let type_::nominal::UnderlyingT::OpaqueWithLocal { t: inner_t } =
            &nominal_type.underlying_t
            && reason.loc().source() == reason.def_loc().source() =>
        {
            elab_t(cx, dst_cx, Some(seen), inner_t.dupe(), op)
        }
        //   elab_t cx ~seen t op
        (TypeInner::NominalT { nominal_type, .. }, _)
            if let type_::nominal::UnderlyingT::CustomError(
                box type_::nominal::CustomErrorData { t: inner_t, .. },
            ) = &nominal_type.underlying_t =>
        {
            elab_t(cx, dst_cx, Some(seen), inner_t.dupe(), op)
        }
        // ******
        //  Keys
        // ******
        (TypeInner::KeysT(_, _), OpInner::AnnotToStringT { .. }) => t,
        (TypeInner::KeysT(reason, inner), _) => {
            let keys_t = elab_t(
                cx,
                dst_cx,
                Some(seen.dupe()),
                inner.dupe(),
                Op::new(OpInner::AnnotGetKeysT(reason.dupe())),
            );
            elab_t(cx, dst_cx, Some(seen), keys_t, op)
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetKeysT(reason_op))
            if let DefTInner::ObjT(obj) = def_t.deref() =>
        {
            let dict_t = flow_typing_flow_common::obj_type::get_dict_opt(&obj.flags.obj_kind);
            let props = cx.find_props(obj.props_tmap.dupe());
            let mut keylist = flow_js_utils::keylist_of_props(&props, reason_op);
            if let Some(dict) = dict_t {
                let key = elab_t(
                    cx,
                    dst_cx,
                    Some(seen.dupe()),
                    dict.key.dupe(),
                    Op::new(OpInner::AnnotToStringT {
                        orig_t: None,
                        reason: reason_op.dupe(),
                    }),
                );
                keylist.push(key);
            }
            type_util::union_of_ts(reason_op.dupe(), keylist, None)
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetKeysT(reason_op))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            // methods are not enumerable, so only walk fields
            let own_props = cx.find_props(inst_t.inst.own_props.dupe());
            let keylist = flow_js_utils::keylist_of_props(&own_props, reason_op);
            type_util::union_of_ts(reason_op.dupe(), keylist, None)
        }
        (TypeInner::AnyT(_, _), OpInner::AnnotGetKeysT(reason_op)) => {
            type_::str_module_t::why(reason_op.dupe())
        }
        // **********
        //  $Values
        // **********
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetValuesT(reason))
            if let DefTInner::ObjT(obj) = def_t.deref() =>
        {
            flow_js_utils::get_values_type_of_obj_t(cx, obj, reason.dupe())
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetValuesT(reason))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            flow_js_utils::get_values_type_of_instance_t(
                cx,
                inst_t.inst.own_props.dupe(),
                inst_t.inst.inst_dict.as_ref(),
                reason.dupe(),
            )
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetValuesT(reason))
            if let DefTInner::ArrT(arr) = def_t.deref() =>
        {
            let elem_t = type_::elemt_of_arrtype(arr.as_ref());
            type_util::mod_reason_of_t(&|_| reason.dupe(), &elem_t)
        }
        (TypeInner::AnyT(_, src), OpInner::AnnotGetValuesT(reason)) => {
            any_t::why(src.clone(), reason.dupe())
        }
        // ******************************
        //  Union and intersection types
        // ******************************
        (TypeInner::UnionT(_, _), OpInner::AnnotObjKitT(data)) => object_kit_concrete(
            cx,
            dst_cx,
            data.use_op.dupe(),
            &op,
            data.reason.dupe(),
            data.resolve_tool.clone(),
            data.tool.clone(),
            t,
        ),
        (TypeInner::UnionT(_, rep), _) => {
            let reason = op.reason();
            let ts: Vec<Type> = rep
                .members_iter()
                .map(|member| elab_t(cx, dst_cx, Some(seen.dupe()), member.dupe(), op.dupe()))
                .collect();
            type_util::union_of_ts(reason, ts, None)
        }
        (TypeInner::IntersectionT(_, _), OpInner::AnnotObjKitT(data)) => object_kit_concrete(
            cx,
            dst_cx,
            data.use_op.dupe(),
            &op,
            data.reason.dupe(),
            data.resolve_tool.clone(),
            data.tool.clone(),
            t,
        ),
        (TypeInner::IntersectionT(reason, _), _) => {
            error_unsupported(None, cx, dst_cx, reason.dupe(), &op)
        }
        // *************************
        //  ConcretizeForInspection
        // *************************
        (_, OpInner::AnnotConcretizeForInspection { collector, .. }) => {
            collector.add(t.dupe());
            t
        }
        // ***********
        //  Unary not
        // ***********
        (_, OpInner::AnnotNotT(reason)) => {
            match t.deref() {
                // any propagation
                TypeInner::AnyT(_, _) => return t,
                TypeInner::DefT(_, def_t) => {
                    match def_t.deref() {
                        DefTInner::BoolGeneralT
                        | DefTInner::StrGeneralT(flow_typing_type::type_::Literal::AnyLiteral)
                        | DefTInner::NumGeneralT(flow_typing_type::type_::Literal::AnyLiteral) => {
                            return type_::bool_module_t::at(reason.loc().dupe());
                        }
                        // !x when x is falsy
                        DefTInner::SingletonBoolT { value: false, .. }
                        | DefTInner::NullT
                        | DefTInner::VoidT => {
                            let reason = reason.dupe().replace_desc(
                                flow_common::reason::VirtualReasonDesc::RBooleanLit(true),
                            );
                            return Type::new(TypeInner::DefT(
                                reason,
                                type_::DefT::new(DefTInner::SingletonBoolT {
                                    value: true,
                                    from_annot: false,
                                }),
                            ));
                        }
                        DefTInner::SingletonStrT { value, .. }
                            if value == &flow_common::reason::Name::new("") =>
                        {
                            let reason = reason.dupe().replace_desc(
                                flow_common::reason::VirtualReasonDesc::RBooleanLit(true),
                            );
                            return Type::new(TypeInner::DefT(
                                reason,
                                type_::DefT::new(DefTInner::SingletonBoolT {
                                    value: true,
                                    from_annot: false,
                                }),
                            ));
                        }
                        DefTInner::SingletonNumT { value, .. } if value.0 == 0.0 => {
                            let reason = reason.dupe().replace_desc(
                                flow_common::reason::VirtualReasonDesc::RBooleanLit(true),
                            );
                            return Type::new(TypeInner::DefT(
                                reason,
                                type_::DefT::new(DefTInner::SingletonBoolT {
                                    value: true,
                                    from_annot: false,
                                }),
                            ));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
            // !x when x is truthy
            let reason = reason
                .dupe()
                .replace_desc(flow_common::reason::VirtualReasonDesc::RBooleanLit(false));
            Type::new(TypeInner::DefT(
                reason,
                type_::DefT::new(DefTInner::SingletonBoolT {
                    value: false,
                    from_annot: false,
                }),
            ))
        }
        // ********
        //  Mixins
        // ********
        (TypeInner::AnyT(_, src), OpInner::AnnotMixinT(r)) => any_t::why(*src, r.dupe()),
        (_, OpInner::AnnotMixinT(r)) => {
            let make_mixin_instance = |inst_r: &Reason,
                                       inst: &type_::InstanceT,
                                       is_this: bool,
                                       this_name: &flow_common::subst_name::SubstName,
                                       class_r: &Reason,
                                       r: &Reason|
             -> Type {
                let static_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
                let super_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
                let instance = type_::InstanceT::new(type_::InstanceTInner {
                    static_,
                    super_,
                    implements: Rc::from([]),
                    inst: inst.inst.clone(),
                });
                Type::new(TypeInner::DefT(
                    class_r.dupe(),
                    type_::DefT::new(DefTInner::ClassT(Type::new(TypeInner::ThisInstanceT(
                        Box::new(ThisInstanceTData {
                            reason: inst_r.dupe(),
                            instance,
                            is_this,
                            subst_name: this_name.clone(),
                        }),
                    )))),
                ))
            };

            if let TypeInner::DefT(class_r, def_t) = t.deref() {
                match def_t.deref() {
                    DefTInner::ClassT(inner) => {
                        if let TypeInner::ThisInstanceT(box ThisInstanceTData {
                            reason: inst_r,
                            instance: i,
                            is_this,
                            subst_name,
                        }) = inner.deref()
                        {
                            make_mixin_instance(inst_r, i, *is_this, subst_name, class_r, r)
                        } else {
                            error_unsupported(
                                None,
                                cx,
                                dst_cx,
                                type_util::reason_of_t(&t).dupe(),
                                &op,
                            )
                        }
                    }
                    DefTInner::PolyT(box PolyTData {
                        tparams_loc,
                        tparams: xs,
                        t_out,
                        ..
                    }) => {
                        if let TypeInner::DefT(class_r2, inner_def_t) = t_out.deref() {
                            if let DefTInner::ClassT(inner) = inner_def_t.deref() {
                                if let TypeInner::ThisInstanceT(box ThisInstanceTData {
                                    reason: inst_r,
                                    instance: i,
                                    is_this,
                                    subst_name,
                                }) = inner.deref()
                                {
                                    let t_out = make_mixin_instance(
                                        inst_r, i, *is_this, subst_name, class_r2, r,
                                    );
                                    type_util::poly_type(
                                        type_::poly::Id::generate_id(),
                                        tparams_loc.dupe(),
                                        vec1::Vec1::try_from_vec(xs.to_vec())
                                            .expect("tparams should be non-empty"),
                                        t_out,
                                    )
                                } else {
                                    error_unsupported(
                                        None,
                                        cx,
                                        dst_cx,
                                        type_util::reason_of_t(&t).dupe(),
                                        &op,
                                    )
                                }
                            } else {
                                error_unsupported(
                                    None,
                                    cx,
                                    dst_cx,
                                    type_util::reason_of_t(&t).dupe(),
                                    &op,
                                )
                            }
                        } else {
                            error_unsupported(
                                None,
                                cx,
                                dst_cx,
                                type_util::reason_of_t(&t).dupe(),
                                &op,
                            )
                        }
                    }
                    _ => {
                        error_unsupported(None, cx, dst_cx, type_util::reason_of_t(&t).dupe(), &op)
                    }
                }
            } else {
                error_unsupported(None, cx, dst_cx, type_util::reason_of_t(&t).dupe(), &op)
            }
        }

        // ********************
        //  Type specialization
        // ********************
        (TypeInner::DefT(_, def_t), OpInner::AnnotSpecializeT(data))
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: xs,
                t_out,
                id,
            }) = def_t.deref() =>
        {
            let ts: Rc<[Type]> = data
                .types
                .as_ref()
                .map(|rc| rc.dupe())
                .unwrap_or_else(|| Rc::from([]));
            mk_typeapp_of_poly(
                cx,
                data.use_op.dupe(),
                data.reason.dupe(),
                data.reason2.dupe(),
                id.clone(),
                tparams_loc.dupe(),
                xs.to_vec(),
                t_out.dupe(),
                ts,
            )
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotSpecializeT(data))
            if data.types.is_none() && matches!(def_t.deref(), DefTInner::ClassT(_)) =>
        {
            t
        }
        (TypeInner::AnyT(_, _), OpInner::AnnotSpecializeT(_)) => t,
        (
            TypeInner::DefT(_, def_t),
            OpInner::AnnotThisSpecializeT {
                reason,
                type_: this,
            },
        ) if let DefTInner::ClassT(instance) = def_t.deref()
            && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: i,
                is_this: _,
                subst_name,
            }) = instance.deref() =>
        {
            let this_type = this.dupe();
            let map = flow_data_structure_wrapper::ord_map::FlowOrdMap::from_iter(std::iter::once(
                (subst_name.clone(), this_type),
            ));
            let i = flow_typing_flow_common::type_subst::subst_instance_type(
                cx,
                None,
                false,
                false,
                flow_typing_flow_common::type_subst::Purpose::Normal,
                &map,
                i,
            );
            reposition(
                cx,
                reason.loc().dupe(),
                Type::new(TypeInner::DefT(
                    r.dupe(),
                    type_::DefT::new(DefTInner::InstanceT(Rc::new(i))),
                )),
            )
        }
        // this-specialization of non-this-abstracted classes is a no-op
        (TypeInner::DefT(_, def_t), OpInner::AnnotThisSpecializeT { reason, .. })
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            reposition(cx, reason.loc().dupe(), instance.dupe())
        }
        (TypeInner::AnyT(_, _), OpInner::AnnotThisSpecializeT { reason, .. }) => {
            reposition(cx, reason.loc().dupe(), t)
        }

        // *********************
        //  Type instantiation
        // *********************
        (TypeInner::DefT(reason_tapp, def_t), _)
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: ids,
                t_out,
                ..
            }) = def_t.deref() =>
        {
            let use_op = type_::unknown_use();
            let reason_op = op.reason();
            let t = instantiate_poly(
                cx,
                use_op,
                reason_op,
                reason_tapp.dupe(),
                tparams_loc.dupe(),
                ids.to_vec(),
                t_out.dupe(),
            );
            elab_t(cx, dst_cx, Some(seen), t, op)
        }
        (
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: i,
                is_this,
                subst_name,
            }),
            _,
        ) => {
            let reason = op.reason();
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason,
                r.dupe(),
                i,
                *is_this,
                subst_name.dupe(),
            );
            elab_t(cx, dst_cx, Some(seen), fixed, op)
        }
        // ***************************
        //  React Abstract Components
        // ***************************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(def_t.deref(), DefTInner::ReactAbstractComponentT(_))
                && matches!(
                    op.deref(),
                    OpInner::AnnotGetPropT(_) | OpInner::AnnotGetElemT { .. }
                ) =>
        {
            let statics = flow_js_utils::lookup_builtin_type(
                cx,
                "React$AbstractComponentStatics",
                reason.dupe(),
            );
            elab_t(cx, dst_cx, Some(seen), statics, op)
        }
        // ***************
        //  ObjTestProtoT
        // ***************
        (TypeInner::AnyT(_, src), OpInner::AnnotObjTestProtoT(reason_op)) => {
            any_t::why(src.clone(), reason_op.dupe())
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotObjTestProtoT(reason_op))
            if matches!(def_t.deref(), DefTInner::NullT) =>
        {
            type_::null_proto::why(reason_op.dupe())
        }
        (_, OpInner::AnnotObjTestProtoT(reason_op)) => {
            if flow_js_utils::object_like(&t) {
                reposition(cx, reason_op.loc().dupe(), t)
            } else {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    flow_typing_errors::error_message::ErrorMessage::EInvalidPrototype(Box::new((
                        reason_op.loc().dupe(),
                        type_util::reason_of_t(&t).dupe(),
                    ))),
                );
                type_::obj_proto::why(reason_op.dupe())
            }
        }
        // *************
        //  Get statics
        // *************
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetStaticsT(reason_op))
            if let DefTInner::InstanceT(inst) = def_t.deref() =>
        {
            reposition(cx, reason_op.loc().dupe(), inst.static_.dupe())
        }
        (TypeInner::AnyT(_, src), OpInner::AnnotGetStaticsT(reason_op)) => {
            any_t::why(src.clone(), reason_op.dupe())
        }
        (TypeInner::ObjProtoT(_), OpInner::AnnotGetStaticsT(reason_op)) => {
            // ObjProtoT not only serves as the instance type of the root class, but
            // also as the statics of the root class.
            reposition(cx, reason_op.loc().dupe(), t)
        }

        // *************
        //  LookupT pt1
        // *************
        (TypeInner::AnyT(_, _), OpInner::AnnotLookupT(data)) => any_t::untyped(data.reason.dupe()),
        (_, OpInner::AnnotLookupT(data)) => {
            let AnnotLookupTData {
                reason: reason_op,
                use_op,
                prop_ref: propref,
                type_: objt,
            } = data.as_ref();
            use flow_typing_flow_common::flow_js_utils::get_prop_t_kit;
            use flow_typing_type::type_::property;

            match t.deref() {
                TypeInner::DefT(_lreason, def_t)
                    if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
                {
                    match propref {
                        type_::PropRef::Named { .. } => {
                            let react_dro = match objt.deref() {
                                TypeInner::DefT(_, d) => match d.deref() {
                                    DefTInner::ObjT(o) => o.flags.react_dro.clone(),
                                    _ => None,
                                },
                                _ => None,
                            };
                            let trace = dummy_trace();
                            match get_prop_t_kit::get_instance_prop::<AnnotGetPropHelper>(
                                cx,
                                &trace,
                                use_op,
                                true,
                                &inst_t.inst,
                                propref,
                                reason_op,
                            )
                            // Annotation inference is never speculative
                            .unwrap()
                            {
                                Some((p, _)) => {
                                    get_prop_t_kit::perform_read_prop_action::<AnnotGetPropHelper>(
                                        cx,
                                        &trace,
                                        use_op.dupe(),
                                        propref,
                                        property::type_(&p),
                                        reason_op,
                                        &react_dro,
                                    )
                                    // Annotation inference is never speculative
                                    .unwrap()
                                }
                                None => cg_lookup_(
                                    cx,
                                    use_op,
                                    inst_t.super_.dupe(),
                                    reason_op,
                                    propref,
                                    objt,
                                ),
                            }
                        }
                        type_::PropRef::Computed(_) => {
                            error_unsupported(None, cx, dst_cx, _lreason.dupe(), &op)
                        }
                    }
                }
                TypeInner::DefT(_, def_t) if let DefTInner::ObjT(o) = def_t.deref() => {
                    let react_dro = match objt.deref() {
                        TypeInner::DefT(_, d) => match d.deref() {
                            DefTInner::ObjT(o) => o.flags.react_dro.clone(),
                            _ => None,
                        },
                        _ => None,
                    };
                    let trace = dummy_trace();
                    match get_prop_t_kit::get_obj_prop::<AnnotGetPropHelper>(
                        cx,
                        &trace,
                        &type_::unknown_use(),
                        false,
                        true,
                        o.as_ref(),
                        propref,
                        reason_op,
                    )
                    // Annotation inference is never speculative
                    .unwrap()
                    {
                        Some((p, _)) => {
                            get_prop_t_kit::perform_read_prop_action::<AnnotGetPropHelper>(
                                cx,
                                &trace,
                                use_op.dupe(),
                                propref,
                                p,
                                reason_op,
                                &react_dro,
                            )
                            // Annotation inference is never speculative
                            .unwrap()
                        }
                        None => cg_lookup_(cx, use_op, o.proto_t.dupe(), reason_op, propref, objt),
                    }
                }
                // ***************
                //  LookupT pt2
                // ***************
                TypeInner::ObjProtoT(_)
                    if let flow_typing_type::type_::PropRef::Named { name, .. } = propref
                        && flow_js_utils::is_object_prototype_method(name) =>
                {
                    flow_js_utils::lookup_builtin_value(cx, "Object", reason_op.dupe())
                }
                TypeInner::FunProtoT(_)
                    if let flow_typing_type::type_::PropRef::Named { name, .. } = propref
                        && flow_js_utils::is_function_prototype(name) =>
                {
                    flow_js_utils::lookup_builtin_value(cx, "Function", reason_op.dupe())
                }
                TypeInner::DefT(_, def_t)
                    if matches!(def_t.deref(), DefTInner::NullT)
                        && let type_::PropRef::Named {
                            reason: reason_prop,
                            name,
                            ..
                        } = propref =>
                {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EPropNotFoundInLookup(
                            Box::new(EPropNotFoundInLookupData {
                                reason_prop: reason_prop.dupe(),
                                reason_obj: reason_op.dupe(),
                                prop_name: Some(name.dupe()),
                                use_op: use_op.dupe(),
                                suggestion: None,
                            }),
                        ),
                    );
                    any_t::error_of_kind(type_::AnyErrorKind::UnresolvedName, reason_op.dupe())
                }
                TypeInner::ObjProtoT(_) | TypeInner::FunProtoT(_)
                    if let type_::PropRef::Named {
                        reason: reason_prop,
                        name,
                        ..
                    } = propref =>
                {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EPropNotFoundInLookup(
                            Box::new(EPropNotFoundInLookupData {
                                reason_prop: reason_prop.dupe(),
                                reason_obj: reason_op.dupe(),
                                prop_name: Some(name.dupe()),
                                use_op: use_op.dupe(),
                                suggestion: None,
                            }),
                        ),
                    );
                    any_t::error_of_kind(type_::AnyErrorKind::UnresolvedName, reason_op.dupe())
                }
                _ => elab_t_wildcard_op(cx, dst_cx, seen, t, op),
            }
        }

        // ******
        //  DRO
        // ******
        (_, OpInner::AnnotDeepReadOnlyT(data)) => {
            use flow_typing_type::type_::ArrType;
            use flow_typing_type::type_::ArrayATData;
            use flow_typing_type::type_::ReactDro;
            use flow_typing_type::type_::TupleATData;

            let dro = Some(ReactDro(data.loc.dupe(), data.dro_type.clone()));

            match t.deref() {
                TypeInner::DefT(r, def_t) => match def_t.deref() {
                    DefTInner::ObjT(o) => {
                        let mut o = (**o).clone();
                        o.flags.react_dro = dro;
                        Type::new(TypeInner::DefT(
                            r.dupe(),
                            type_::DefT::new(DefTInner::ObjT(Rc::new(o))),
                        ))
                    }
                    DefTInner::ArrT(arr) => match arr.deref() {
                        ArrType::TupleAT(box TupleATData {
                            elem_t,
                            elements,
                            arity,
                            inexact,
                            react_dro: _,
                        }) => Type::new(TypeInner::DefT(
                            r.dupe(),
                            type_::DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT(Box::new(
                                TupleATData {
                                    react_dro: dro,
                                    elem_t: elem_t.dupe(),
                                    elements: elements.dupe(),
                                    arity: *arity,
                                    inexact: *inexact,
                                },
                            ))))),
                        )),
                        ArrType::ArrayAT(box ArrayATData {
                            elem_t,
                            tuple_view,
                            react_dro: _,
                        }) => Type::new(TypeInner::DefT(
                            r.dupe(),
                            type_::DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                                ArrayATData {
                                    react_dro: dro,
                                    elem_t: elem_t.dupe(),
                                    tuple_view: tuple_view.clone(),
                                },
                            ))))),
                        )),
                        ArrType::ROArrayAT(box (inner_t, _)) => Type::new(TypeInner::DefT(
                            r.dupe(),
                            type_::DefT::new(DefTInner::ArrT(Rc::new(ArrType::ROArrayAT(
                                Box::new((inner_t.dupe(), dro)),
                            )))),
                        )),
                    },
                    _ => t,
                },
                _ => t,
            }
        }

        // **********
        //  ObjRestT
        // **********
        (TypeInner::AnyT(_, src), OpInner::AnnotObjRestT { reason, .. }) => {
            any_t::why(src.clone(), reason.dupe())
        }
        (
            _,
            OpInner::AnnotObjRestT {
                reason: reason_op,
                keys: xs,
            },
        ) => {
            match t.deref() {
                TypeInner::DefT(reason_obj, def_t) if let DefTInner::ObjT(o) = def_t.deref() => {
                    flow_js_utils::objt_to_obj_rest(
                        cx,
                        o.props_tmap.dupe(),
                        Some(Rc::from([])),
                        o.flags.obj_kind.clone(),
                        reason_op,
                        reason_obj,
                        xs,
                    )
                    // Annotation inference is never speculative
                    .unwrap()
                }
                TypeInner::DefT(reason, def_t)
                    if matches!(def_t.deref(), DefTInner::InstanceT(_)) =>
                {
                    // This implementation relies on unsealed objects and set-prop logic that is
                    // hard to implement in annotation inference.
                    error_unsupported(None, cx, dst_cx, reason.dupe(), &op)
                }
                TypeInner::ObjProtoT(_) => flow_typing_flow_common::obj_type::mk_with_proto(
                    cx,
                    reason_op.dupe(),
                    type_::ObjKind::Exact,
                    None,
                    None,
                    None,
                    None,
                    t,
                ),
                TypeInner::DefT(_, def_t)
                    if matches!(def_t.deref(), DefTInner::NullT | DefTInner::VoidT) =>
                {
                    flow_typing_flow_common::obj_type::mk(
                        type_::ObjKind::Exact,
                        cx,
                        reason_op.dupe(),
                    )
                }
                // | (NamespaceT { namespace_symbol = _; values_type; types_tmap = _ }, _) ->
                //     elab_t cx ~seen values_type op
                TypeInner::NamespaceT(ns) => {
                    elab_t(cx, dst_cx, Some(seen), ns.values_type.dupe(), op)
                }
                _ => {
                    let reason_op_catch = op.reason();
                    let lower = (
                        type_util::reason_of_t(&t).dupe(),
                        flow_js_utils::error_message_kind_of_lower(&t),
                    );
                    let upper = (
                        reason_op_catch.dupe(),
                        flow_typing_errors::error_message::UpperKind::IncompatibleUnclassified(
                            op.string_of_operation().into(),
                        ),
                    );
                    let use_op = op.use_op();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        flow_typing_errors::error_message::ErrorMessage::EIncompatible(Box::new(
                            EIncompatibleData {
                                lower,
                                upper,
                                use_op,
                            },
                        )),
                    );
                    any_t::error(reason_op_catch)
                }
            }
        }

        // **********************************
        //  Namespace and type qualification
        // **********************************
        (TypeInner::NamespaceT(ns), OpInner::AnnotGetTypeFromNamespaceT(data)) => {
            let AnnotGetTypeFromNamespaceTData {
                reason,
                use_op,
                prop_ref,
            } = data.as_ref();
            let (prop_ref_reason, prop_name) = prop_ref;
            match cx
                .find_props(ns.types_tmap.dupe())
                .get(prop_name)
                .and_then(type_::property::read_t)
            {
                Some(prop) => prop,
                None => elab_t(
                    cx,
                    dst_cx,
                    Some(seen),
                    ns.values_type.dupe(),
                    Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
                        reason: reason.dupe(),
                        use_op: use_op.dupe(),
                        from_annot: false,
                        prop_ref: type_util::mk_named_prop(
                            prop_ref_reason.dupe(),
                            false,
                            prop_name.dupe(),
                        ),
                    }))),
                ),
            }
        }
        (TypeInner::NamespaceT(ns), _) => elab_t(cx, dst_cx, Some(seen), ns.values_type.dupe(), op),
        (_, OpInner::AnnotGetTypeFromNamespaceT(data)) => {
            let AnnotGetTypeFromNamespaceTData {
                reason,
                use_op,
                prop_ref,
            } = data.as_ref();
            let (prop_ref_reason, prop_name) = prop_ref;
            elab_t(
                cx,
                dst_cx,
                Some(seen),
                t,
                Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
                    reason: reason.dupe(),
                    use_op: use_op.dupe(),
                    from_annot: false,
                    prop_ref: type_util::mk_named_prop(
                        prop_ref_reason.dupe(),
                        false,
                        prop_name.dupe(),
                    ),
                }))),
            )
        }
        // **********
        //  GetPropT
        // **********
        (TypeInner::AnyT(_, _), OpInner::AnnotGetPropT(data)) => Type::new(TypeInner::AnyT(
            data.reason.dupe(),
            type_::AnySource::Untyped,
        )),
        (_, OpInner::AnnotGetPropT(data)) => {
            let AnnotGetPropTData {
                reason: reason_op,
                use_op,
                from_annot,
                prop_ref: propref,
            } = data.as_ref();
            use flow_typing_flow_common::flow_js_utils::get_prop_t_kit;

            match t.deref() {
                TypeInner::DefT(reason_instance, def_t)
                    if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
                {
                    match propref {
                        type_::PropRef::Named { .. } => {
                            let trace = dummy_trace();
                            get_prop_t_kit::read_instance_prop::<AnnotGetPropHelper>(
                                cx,
                                &trace,
                                use_op,
                                &t,
                                None,
                                false,
                                inst_t.super_.dupe(),
                                type_::LookupKind::Strict(reason_instance.dupe()),
                                &type_::hint_unavailable(),
                                false,
                                &inst_t.inst,
                                propref,
                                reason_op,
                            )
                            // Annotation inference is never speculative
                            .unwrap()
                        }
                        type_::PropRef::Computed(_) => {
                            error_unsupported(None, cx, dst_cx, reason_instance.dupe(), &op)
                        }
                    }
                }
                TypeInner::DefT(reason_obj, def_t) if let DefTInner::ObjT(o) = def_t.deref() => {
                    if let type_::PropRef::Named { name, .. } = propref
                        && name.as_str() == "constructor"
                    {
                        type_::unsoundness::why(
                            type_::UnsoundnessKind::Constructor,
                            reason_op.dupe(),
                        )
                    } else {
                        let trace = dummy_trace();
                        get_prop_t_kit::read_obj_prop::<AnnotGetPropHelper>(
                            cx,
                            &trace,
                            use_op.dupe(),
                            *from_annot,
                            false,
                            o.as_ref(),
                            propref,
                            reason_obj.dupe(),
                            reason_op.dupe(),
                            None,
                        )
                        // Annotation inference is never speculative
                        .unwrap()
                    }
                }
                TypeInner::DefT(reason, def_t)
                    if let DefTInner::ClassT(instance) = def_t.deref()
                        && let type_::PropRef::Named { name, .. } = propref
                        && name.as_str() == "prototype" =>
                {
                    reposition(cx, reason.loc().dupe(), instance.dupe())
                }
                // *********
                // * Enums *
                //  *********
                TypeInner::DefT(enum_reason, def_t)
                    if let DefTInner::EnumObjectT {
                        enum_value_t,
                        enum_info,
                    } = def_t.deref()
                        && let EnumInfoInner::ConcreteEnum(enum_concrete_info) =
                            enum_info.deref().deref()
                        && let type_::PropRef::Named {
                            reason: prop_reason,
                            name,
                            ..
                        } = propref =>
                {
                    let trace = dummy_trace();
                    let access = (
                        use_op.dupe(),
                        reason_op.dupe(),
                        None,
                        (prop_reason.dupe(), name.dupe()),
                    );
                    flow_js_utils::get_prop_t_kit::on_enum_object_t::<AnnotGetPropHelper>(
                        cx,
                        &trace,
                        enum_reason,
                        t.dupe(),
                        enum_value_t.dupe(),
                        enum_concrete_info,
                        &access,
                    )
                    // Annotation inference is never speculative
                    .unwrap()
                }
                // **********************
                //  GetPropT (for arrays)
                // **********************
                TypeInner::DefT(reason, def_t)
                    if let DefTInner::ArrT(arr) = def_t.deref()
                        && let flow_typing_type::type_::ArrType::TupleAT(box TupleATData {
                            arity,
                            inexact,
                            ..
                        }) = arr.deref()
                        && let type_::PropRef::Named { name, .. } = propref
                        && name == &Name::new("length") =>
                {
                    let trace = dummy_trace();
                    flow_js_utils::get_prop_t_kit::on_array_length::<AnnotGetPropHelper>(
                        cx,
                        &trace,
                        reason.dupe(),
                        *inexact,
                        *arity,
                        reason_op,
                    )
                    // Annotation inference is never speculative
                    .unwrap()
                }
                TypeInner::DefT(reason, def_t) if let DefTInner::ArrT(arr) = def_t.deref() => {
                    use flow_typing_type::type_::ArrType;
                    match arr.as_ref() {
                        ArrType::ArrayAT(box ArrayATData { elem_t, .. }) => {
                            let arr_t = get_builtin_typeapp(
                                cx,
                                reason.dupe(),
                                "Array",
                                vec![elem_t.dupe()],
                            );
                            elab_t(cx, dst_cx, Some(seen), arr_t, op)
                        }
                        ArrType::TupleAT(box TupleATData { .. })
                        | ArrType::ROArrayAT(box (_, _)) => {
                            let elem_t = type_::elemt_of_arrtype(arr.as_ref());
                            let arr_t = get_builtin_typeapp(
                                cx,
                                reason.dupe(),
                                "$ReadOnlyArray",
                                vec![elem_t],
                            );
                            elab_t(cx, dst_cx, Some(seen), arr_t, op)
                        }
                    }
                }
                _ => elab_t_wildcard_op(cx, dst_cx, seen, t, op),
            }
        }
        // ************
        //  Object Kit
        // ************
        (_, OpInner::AnnotObjKitT(data)) => object_kit_concrete(
            cx,
            dst_cx,
            data.use_op.dupe(),
            &op,
            data.reason.dupe(),
            data.resolve_tool.clone(),
            data.tool.clone(),
            t,
        ),
        // ******************
        //  GetElemT / ElemT
        // ******************
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetElemT { reason, .. })
            if matches!(
                def_t.deref(),
                DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
            ) =>
        {
            // NOTE bypassing check that index is a number
            type_::str_module_t::why(reason.dupe())
        }
        (
            TypeInner::AnyT(_, _),
            OpInner::AnnotGetElemT {
                reason,
                use_op,
                key,
            },
        ) => {
            let key_type = key.dupe();
            elab_t(
                cx,
                dst_cx,
                Some(seen),
                key_type,
                Op::new(OpInner::AnnotElemT {
                    reason: reason.dupe(),
                    use_op: use_op.dupe(),
                    from_annot: false,
                    source: t.dupe(),
                }),
            )
        }
        (
            TypeInner::DefT(_, def_t),
            OpInner::AnnotGetElemT {
                reason,
                use_op,
                key,
            },
        ) if matches!(
            def_t.deref(),
            DefTInner::ObjT(_) | DefTInner::ArrT(_) | DefTInner::InstanceT(_)
        ) =>
        {
            let key_type = key.dupe();
            elab_t(
                cx,
                dst_cx,
                Some(seen),
                key_type,
                Op::new(OpInner::AnnotElemT {
                    reason: reason.dupe(),
                    use_op: use_op.dupe(),
                    from_annot: false,
                    source: t.dupe(),
                }),
            )
        }
        (
            _,
            OpInner::AnnotElemT {
                reason: reason_op,
                use_op,
                from_annot,
                source,
            },
        ) => {
            let source_type = source.dupe();
            match (t.deref(), source.deref()) {
                (_, TypeInner::DefT(_, source_def_t))
                    if matches!(
                        source_def_t.deref(),
                        DefTInner::ObjT(_) | DefTInner::InstanceT(_)
                    ) =>
                {
                    let prop_ref = flow_js_utils::propref_for_elem_t(cx, &t);
                    elab_t(
                        cx,
                        dst_cx,
                        Some(seen),
                        source_type,
                        Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
                            reason: reason_op.dupe(),
                            from_annot: *from_annot,
                            use_op: use_op.dupe(),
                            prop_ref,
                        }))),
                    )
                }
                (_, TypeInner::AnyT(_, _)) => {
                    let value = type_::any_t::untyped(reason_op.dupe());
                    reposition(cx, reason_op.loc().dupe(), value)
                }
                (TypeInner::AnyT(_, _), TypeInner::DefT(_, source_def_t))
                    if let DefTInner::ArrT(arrtype) = source_def_t.deref() =>
                {
                    let value = type_::elemt_of_arrtype(arrtype.as_ref());
                    reposition(cx, reason_op.loc().dupe(), value)
                }
                (TypeInner::DefT(_, elem_def_t), TypeInner::DefT(reason_tup, source_def_t))
                    if matches!(
                        elem_def_t.deref(),
                        DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
                    ) && let DefTInner::ArrT(arrtype) = source_def_t.deref() =>
                {
                    let (value, _, _, _) = flow_js_utils::array_elem_check(
                        cx,
                        false,
                        *from_annot,
                        &t,
                        use_op.dupe(),
                        reason_op,
                        reason_tup,
                        arrtype.as_ref(),
                    )
                    // Annotation inference is never speculative
                    .unwrap();
                    reposition(cx, reason_op.loc().dupe(), value)
                }
                _ => general_error(cx, dst_cx, &t, &op),
            }
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotObjKeyMirror(reason_op))
            if let DefTInner::ObjT(o) = def_t.deref() =>
        {
            flow_js_utils::obj_key_mirror(cx, o, reason_op)
        }
        (_, OpInner::AnnotObjKeyMirror(_)) => {
            error_unsupported(None, cx, dst_cx, type_util::reason_of_t(&t).dupe(), &op)
        }
        (TypeInner::DefT(_, def_t), OpInner::AnnotGetEnumT(reason))
            if let DefTInner::EnumValueT(enum_info) = def_t.deref() =>
        {
            Type::new(TypeInner::DefT(
                reason.dupe(),
                type_::DefT::new(DefTInner::EnumObjectT {
                    enum_value_t: t.dupe(),
                    enum_info: enum_info.clone(),
                }),
            ))
        }
        (_, OpInner::AnnotGetEnumT(_)) => {
            error_unsupported(None, cx, dst_cx, type_util::reason_of_t(&t).dupe(), &op)
        }
        // *********************
        //  Opaque types (pt 2)
        // *********************
        (TypeInner::NominalT { nominal_type, .. }, _) if nominal_type.upper_t.is_some() => elab_t(
            cx,
            dst_cx,
            Some(seen),
            nominal_type.upper_t.as_ref().unwrap().dupe(),
            op,
        ),
        // ************************
        //  Binary arith operators
        // ************************
        (_, OpInner::AnnotArithT(data)) => {
            let AnnotArithTData {
                reason,
                flip,
                rhs_t,
                kind,
            } = data.as_ref();
            let rhs_type = rhs_t.dupe();
            if flow_js_utils::needs_resolution(&rhs_type) || flow_js_utils::is_generic(&rhs_type) {
                elab_t(
                    cx,
                    dst_cx,
                    Some(seen),
                    rhs_type,
                    Op::new(OpInner::AnnotArithT(Box::new(AnnotArithTData {
                        reason: reason.dupe(),
                        flip: !flip,
                        rhs_t: t.dupe(),
                        kind: kind.clone(),
                    }))),
                )
            } else {
                let (lhs_t, rhs_t) = if *flip {
                    (&rhs_type, &t)
                } else {
                    (&t, &rhs_type)
                };
                // Annotation inference is never speculative
                flow_js_utils::flow_arith(cx, reason.dupe(), lhs_t, rhs_t, kind.clone()).unwrap()
            }
        }
        // ***********************
        //  Unary arith operators
        // ***********************
        (_, OpInner::AnnotUnaryArithT { reason, kind }) => {
            // Annotation inference is never speculative
            flow_js_utils::flow_unary_arith(cx, &t, reason.dupe(), kind.clone()).unwrap()
        }
        // ***************************
        //  Singleton primitive types
        // ***************************
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::NumericStrKeyT(num_lit) = def_t.deref() =>
        {
            let s = &num_lit.1;
            let new_t = Type::new(TypeInner::DefT(
                reason.dupe(),
                type_::DefT::new(DefTInner::SingletonStrT {
                    value: flow_common::reason::Name::new(s.as_str()),
                    from_annot: false,
                }),
            ));
            elab_t(cx, dst_cx, Some(seen), new_t, op)
        }
        (TypeInner::NullProtoT(reason), _) => {
            let new_t = Type::new(TypeInner::DefT(
                reason.dupe(),
                type_::DefT::new(DefTInner::NullT),
            ));
            elab_t(cx, dst_cx, Some(seen), new_t, op)
        }
        // ******************
        //  Function Statics
        // ******************
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::FunT(static_, _) = def_t.deref()
                && object_like_op(op.deref()) =>
        {
            let static_ = reposition(cx, reason.loc().dupe(), static_.dupe());
            elab_t(cx, dst_cx, Some(seen), static_, op)
        }
        // ***************
        //  Class statics
        // ***************
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ClassT(instance) = def_t.deref()
                && object_like_op(op.deref()) =>
        {
            let statics = get_statics(cx, reason.dupe(), instance.dupe());
            elab_t(cx, dst_cx, Some(seen), statics, op)
        }
        (
            TypeInner::DefT(enum_reason, def_t),
            OpInner::AnnotGetElemT {
                reason: reason_op,
                use_op: _,
                key: elem,
            },
        ) if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) => {
            let elem_type = elem.dupe();
            let reason = type_util::reason_of_t(&elem_type);
            flow_js_utils::add_output_non_speculating(
                cx,
                flow_typing_errors::error_message::ErrorMessage::EEnumError(
                    flow_typing_errors::error_message::EnumErrorKind::EnumInvalidMemberAccess(
                        Box::new(EnumInvalidMemberAccessData {
                            member_name: None,
                            suggestion: None,
                            reason: reason.dupe(),
                            enum_reason: enum_reason.dupe(),
                        }),
                    ),
                ),
            );
            any_t::error(reason_op.dupe())
        }
        // **************************************
        //  Object, function, etc. library calls
        // **************************************
        (TypeInner::ObjProtoT(reason), _) => {
            let obj_proto = get_builtin_type(cx, reason.dupe(), Some(true), "Object");
            elab_t(cx, dst_cx, Some(seen), obj_proto, op)
        }
        (TypeInner::FunProtoT(reason), _) => {
            let fun_proto = get_builtin_type(cx, reason.dupe(), Some(true), "Function");
            elab_t(cx, dst_cx, Some(seen), fun_proto, op)
        }
        // ***********
        //  ToStringT
        // ***********
        (TypeInner::DefT(_, def_t), OpInner::AnnotToStringT { .. })
            if matches!(
                def_t.deref(),
                DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
            ) =>
        {
            t
        }
        (_, OpInner::AnnotToStringT { reason, .. }) => type_::str_module_t::why(reason.dupe()),
        // **********************
        //  Promoting primitives
        // **********************
        (TypeInner::DefT(reason, def_t), _)
            if primitive_promoting_op(op.deref())
                && matches!(
                    def_t.deref(),
                    DefTInner::StrGeneralT(_)
                        | DefTInner::SingletonStrT { .. }
                        | DefTInner::NumGeneralT(_)
                        | DefTInner::SingletonNumT { .. }
                        | DefTInner::BoolGeneralT
                        | DefTInner::SingletonBoolT { .. }
                        | DefTInner::SymbolT
                        | DefTInner::UniqueSymbolT(_)
                ) =>
        {
            let name = match def_t.deref() {
                DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. } => "String",
                DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. } => "Number",
                DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. } => "Boolean",
                DefTInner::SymbolT | DefTInner::UniqueSymbolT(_) => "Symbol",
                _ => unreachable!(),
            };
            let builtin = get_builtin_type(cx, reason.dupe(), Some(true), name);
            elab_t(cx, dst_cx, Some(seen), builtin, op)
        }
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::MixedT(type_::MixedFlavor::MixedFunction)
            ) && matches!(
                op.deref(),
                OpInner::AnnotGetPropT(_) | OpInner::AnnotLookupT(_)
            ) =>
        {
            elab_t(
                cx,
                dst_cx,
                Some(seen),
                Type::new(TypeInner::FunProtoT(reason.dupe())),
                op,
            )
        }
        (_, _) => general_error(cx, dst_cx, &t, &op),
    }
}

fn elab_t_wildcard_op<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    seen: FlowOrdSet<i32>,
    t: Type,
    op: Op<'cx>,
) -> Type {
    use flow_typing_type::type_::DefTInner;

    // ******************
    //  Function Statics
    // ******************
    if let TypeInner::DefT(reason, def_t) = t.deref()
        && let DefTInner::FunT(static_, _) = def_t.deref()
        && object_like_op(op.deref())
    {
        let static_ = reposition(cx, reason.loc().dupe(), static_.dupe());
        return elab_t(cx, dst_cx, Some(seen), static_, op);
    }
    // ***************
    //  Class statics
    // ***************
    if let TypeInner::DefT(reason, def_t) = t.deref()
        && let DefTInner::ClassT(instance) = def_t.deref()
        && object_like_op(op.deref())
    {
        let statics = get_statics(cx, reason.dupe(), instance.dupe());
        return elab_t(cx, dst_cx, Some(seen), statics, op);
    }
    // **************************************
    //  Object, function, etc. library calls
    // **************************************
    if let TypeInner::ObjProtoT(reason) = t.deref() {
        let obj_proto = get_builtin_type(cx, reason.dupe(), Some(true), "Object");
        return elab_t(cx, dst_cx, Some(seen), obj_proto, op);
    }
    if let TypeInner::FunProtoT(reason) = t.deref() {
        let fun_proto = get_builtin_type(cx, reason.dupe(), Some(true), "Function");
        return elab_t(cx, dst_cx, Some(seen), fun_proto, op);
    }
    // **********************
    //  Promoting primitives
    // **********************
    if let TypeInner::DefT(reason, def_t) = t.deref()
        && primitive_promoting_op(op.deref())
    {
        let name = match def_t.deref() {
            DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. } => Some("String"),
            DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. } => Some("Number"),
            DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. } => Some("Boolean"),
            DefTInner::SymbolT | DefTInner::UniqueSymbolT(_) => Some("Symbol"),
            _ => None,
        };
        if let Some(name) = name {
            let builtin = get_builtin_type(cx, reason.dupe(), Some(true), name);
            return elab_t(cx, dst_cx, Some(seen), builtin, op);
        }
    }
    if let TypeInner::DefT(reason, def_t) = t.deref()
        && matches!(
            def_t.deref(),
            DefTInner::MixedT(type_::MixedFlavor::MixedFunction)
        )
        && matches!(
            op.deref(),
            OpInner::AnnotGetPropT(_) | OpInner::AnnotLookupT(_)
        )
    {
        return elab_t(
            cx,
            dst_cx,
            Some(seen),
            Type::new(TypeInner::FunProtoT(reason.dupe())),
            op,
        );
    }
    general_error(cx, dst_cx, &t, &op)
}

pub fn get_builtin_type<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    use_desc: Option<bool>,
    name: &str,
) -> Type {
    let use_desc = use_desc.unwrap_or(false);
    let t = flow_js_utils::lookup_builtin_type(cx, name, reason.dupe());
    let reason_type = type_util::reason_of_t(&t);
    mk_instance_raw(cx, None, reason, Some(use_desc), reason_type.dupe(), t)
}

pub fn specialize<'cx>(
    cx: &Context<'cx>,
    t: Type,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    ts: Option<Rc<[Type]>>,
) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotSpecializeT(Box::new(AnnotSpecializeTData {
            use_op,
            reason: reason_op,
            reason2: reason_tapp,
            types: ts,
        }))),
    )
}

fn this_specialize<'cx>(cx: &Context<'cx>, reason: Reason, this: Type, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotThisSpecializeT {
            reason,
            type_: this,
        }),
    )
}

fn specialize_class<'cx>(
    cx: &Context<'cx>,
    c: Type,
    reason_op: Reason,
    reason_tapp: Reason,
    ts: Option<Rc<[Type]>>,
) -> Type {
    match ts {
        None => c,
        Some(ts) => specialize(
            cx,
            c,
            type_::unknown_use(),
            reason_op,
            reason_tapp,
            Some(ts),
        ),
    }
}

pub fn mk_type_reference<'cx>(
    cx: &Context<'cx>,
    type_t_kind: TypeTKind,
    reason: Reason,
    c: Type,
) -> Type {
    let reason2 = reason.dupe();
    let type_t_kind2 = type_t_kind;
    let c2 = c.dupe();
    let tvar = mk_lazy_tvar(cx, reason.dupe(), move |cx, id| {
        let dst_cx = effective_dst_cx(cx);
        let t = elab_t(
            cx,
            &dst_cx,
            None,
            c2.dupe(),
            Op::new(OpInner::AnnotUseTTypeT {
                reason: reason2.dupe(),
                kind: type_t_kind2,
            }),
        );
        resolve_id(cx, &dst_cx, reason2.dupe(), id, t);
    });
    Type::new(TypeInner::AnnotT(reason, tvar, false))
}

pub fn mk_instance<'cx>(
    cx: &Context<'cx>,
    type_t_kind: Option<TypeTKind>,
    instance_reason: Reason,
    use_desc: Option<bool>,
    c: Type,
) -> Type {
    mk_instance_raw(
        cx,
        type_t_kind,
        instance_reason.dupe(),
        use_desc,
        instance_reason,
        c,
    )
}

fn mk_instance_raw<'cx>(
    cx: &Context<'cx>,
    type_t_kind: Option<TypeTKind>,
    instance_reason: Reason,
    use_desc: Option<bool>,
    reason_type: Reason,
    c: Type,
) -> Type {
    let type_t_kind = type_t_kind.unwrap_or(TypeTKind::InstanceKind);
    let use_desc = use_desc.unwrap_or(false);
    let source = elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        c,
        Op::new(OpInner::AnnotUseTTypeT {
            reason: reason_type,
            kind: type_t_kind,
        }),
    );
    Type::new(TypeInner::AnnotT(instance_reason, source, use_desc))
}

fn mk_typeapp_instance<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    from_value: bool,
    c: Type,
    ts: Rc<[Type]>,
) -> Type {
    let t = specialize(
        cx,
        c.dupe(),
        use_op,
        reason_op,
        reason_tapp.dupe(),
        Some(ts),
    );
    if from_value {
        type_util::mod_reason_of_t(&|_| reason_tapp.dupe(), &t)
    } else {
        let reason_type = type_util::reason_of_t(&c);
        mk_instance_raw(cx, None, reason_tapp, None, reason_type.dupe(), t)
    }
}

fn get_statics<'cx>(cx: &Context<'cx>, reason: Reason, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotGetStaticsT(reason)),
    )
}

pub fn get_prop<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: Reason,
    op_reason: Option<Reason>,
    name: Name,
    t: Type,
) -> Type {
    let op_reason = op_reason.unwrap_or_else(|| reason.dupe());
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotGetPropT(Box::new(AnnotGetPropTData {
            reason: op_reason,
            use_op,
            from_annot: false,
            prop_ref: type_util::mk_named_prop(reason, false, name),
        }))),
    )
}

pub fn get_elem<'cx>(cx: &Context<'cx>, use_op: UseOp, reason: Reason, key: Type, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotGetElemT {
            reason,
            use_op,
            key,
        }),
    )
}

pub fn qualify_type<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: Reason,
    op_reason: Reason,
    prop_name: Name,
    t: Type,
) -> Type {
    let op_reason2 = op_reason.dupe();
    let reason2 = reason.dupe();
    let use_op2 = use_op.dupe();
    let prop_name2 = prop_name.dupe();
    let t2 = t.dupe();
    mk_lazy_tvar(cx, op_reason.dupe(), move |cx, id| {
        let dst_cx = effective_dst_cx(cx);
        let t = elab_t(
            cx,
            &dst_cx,
            None,
            t2.dupe(),
            Op::new(OpInner::AnnotGetTypeFromNamespaceT(Box::new(
                AnnotGetTypeFromNamespaceTData {
                    reason: op_reason2.dupe(),
                    use_op: use_op2.dupe(),
                    prop_ref: (reason2.dupe(), prop_name2.dupe()),
                },
            ))),
        );
        resolve_id(cx, &dst_cx, op_reason2, id, t);
    })
}

pub fn assert_export_is_type<'cx>(cx: &Context<'cx>, reason: Reason, name: &str, t: Type) -> Type {
    let reason2 = reason.dupe();
    let name2 = Name::new(name);
    let t2 = t.dupe();
    mk_lazy_tvar(cx, reason.dupe(), move |cx, id| {
        let dst_cx = effective_dst_cx(cx);
        let t = elab_t(
            cx,
            &dst_cx,
            None,
            t2.dupe(),
            Op::new(OpInner::AnnotAssertExportIsTypeT {
                reason: reason2.dupe(),
                name: name2.dupe(),
            }),
        );
        resolve_id(cx, &dst_cx, reason2, id, t);
    })
}

pub fn cjs_require<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    namespace_symbol: flow_common::flow_symbol::Symbol,
    is_strict: bool,
    standard_cjs_esm_interop: bool,
    resolved_require: flow_typing_context::ResolvedRequire<'cx>,
) -> Type {
    let reason2 = reason.dupe();
    let namespace_symbol2 = namespace_symbol.clone();
    let lz = get_lazy_module_type_or_any_src(&resolved_require);
    let resolved = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
        let result = lz.get_forced(cx).clone();
        match result {
            Ok(module_type) => {
                let (t, _) = flow_js_utils::cjs_require_t_kit::on_module_t(
                    cx,
                    |_cx, _loc, t| Ok(t),
                    reason2.dupe(),
                    namespace_symbol2.clone(),
                    is_strict,
                    standard_cjs_esm_interop,
                    &module_type,
                )
                .unwrap_or_else(|_| {
                    (
                        type_::any_t::error(reason2.dupe()),
                        flow_aloc::ALoc::default(),
                    )
                });
                t
            }
            Err(src) => type_::any_t::why(src, reason2.dupe()),
        }
    })
        as Box<dyn FnOnce(&Context<'cx>) -> Type>));
    mk_sig_tvar(cx, reason, resolved)
}

pub fn lazy_cjs_extract_named_exports<'cx>(
    reason: Reason,
    local_module: type_::ModuleType,
    t: Type,
) -> Rc<
    flow_lazy::Lazy<
        Context<'cx>,
        type_::ModuleType,
        Box<dyn FnOnce(&Context<'cx>) -> type_::ModuleType + 'cx>,
    >,
> {
    let reason2 = reason.dupe();
    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
        let reason3 = reason2.dupe();
        let concretize = |t: Type| -> Result<Type, flow_utils_concurrency::job_error::JobError> {
            let result = elab_t(
                cx,
                &effective_dst_cx(cx),
                None,
                t,
                Op::new(
                    OpInner::AnnotConcretizeForCJSExtractNamedExportsAndTypeExports(reason3.dupe()),
                ),
            );
            // | OpenT (_, id) -> get_fully_resolved_type cx id
            // | t -> t
            Ok(match result.deref() {
                TypeInner::OpenT(tvar) => {
                    get_fully_resolved_type(cx, &effective_dst_cx(cx), tvar.id() as i32)
                }
                _ => result,
            })
        };
        // The concretize closure has no `?` call sites in its body, so
        // on_type cannot return Err here. .expect documents the
        // invariant — see annotation_inference::copy_type_exports for
        // the same pattern. Annotation inference is on the type-sig
        // pipeline, not the cancel-aware check pipeline.
        flow_js_utils::cjs_extract_named_exports_t_kit::on_type(
            cx,
            &concretize,
            reason2.dupe(),
            local_module.clone(),
            t.dupe(),
        )
        .expect(
            "annotation_inference::lazy_cjs_extract_named_exports concretize closure is infallible",
        )
    })))
}

pub fn import_typeof<'cx>(cx: &Context<'cx>, reason: Reason, name: &str, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotImportTypeofT {
            reason,
            name: name.into(),
        }),
    )
}

pub fn import_default<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    import_kind: type_::ImportKind,
    export_name: &str,
    module_name: flow_common::flow_import_specifier::Userland,
    is_strict: bool,
    resolved_require: flow_typing_context::ResolvedRequire<'cx>,
) -> Type {
    let on_module_reason = reason.dupe();
    let export_name = export_name.to_owned();
    let on_module = move |cx: &Context<'cx>, m: &type_::ModuleType| -> Type {
        let (_name_loc_opt, t) = flow_js_utils::import_default_t_kit::on_module_t(
            cx,
            &with_concretized_type,
            on_module_reason.dupe(),
            import_kind.clone(),
            &export_name,
            module_name.clone(),
            is_strict,
            m,
        )
        .unwrap_or_else(|_| (None, type_::any_t::error(on_module_reason.dupe())));
        t
    };
    let why_reason = reason.dupe();
    let lz = get_lazy_module_type_or_any_src(&resolved_require);
    let resolved = Rc::new(flow_lazy::Lazy::new(
        Box::new(
            move |lazy_cx: &Context<'cx>| match lz.get_forced(lazy_cx).clone() {
                Ok(m) => on_module(lazy_cx, &m),
                Err(src) => type_::any_t::why(src, why_reason.dupe()),
            },
        ) as Box<dyn FnOnce(&Context<'cx>) -> Type>,
    ));
    mk_sig_tvar(cx, reason, resolved)
}

pub fn import_named<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    import_kind: type_::ImportKind,
    export_name: &str,
    module_name: flow_common::flow_import_specifier::Userland,
    is_strict: bool,
    resolved_require: flow_typing_context::ResolvedRequire<'cx>,
) -> Type {
    let reason2 = reason.dupe();
    let export_name2: FlowSmolStr = export_name.into();
    let module_name2 = module_name.clone();
    let on_module = move |cx: &Context<'cx>, m: &type_::ModuleType| -> Type {
        let (_name_loc_opt, t) = flow_js_utils::import_named_t_kit::on_module_t(
            cx,
            &with_concretized_type,
            reason2.dupe(),
            import_kind.clone(),
            &export_name2,
            module_name2.clone(),
            is_strict,
            m,
        )
        .unwrap_or_else(|_| (None, type_::any_t::error(reason2.dupe())));
        t
    };
    let lz = get_lazy_module_type_or_any_src(&resolved_require);
    let why_reason = reason.dupe();
    let resolved = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
        let result = lz.get_forced(cx).clone();
        match result {
            Ok(m) => on_module(cx, &m),
            Err(src) => type_::any_t::why(src, why_reason.dupe()),
        }
    })
        as Box<dyn FnOnce(&Context<'cx>) -> Type>));
    mk_sig_tvar(cx, reason, resolved)
}

pub fn import_ns<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    namespace_symbol: flow_common::flow_symbol::Symbol,
    is_strict: bool,
    resolved_require: flow_typing_context::ResolvedRequire<'cx>,
) -> Type {
    let reason2 = reason.dupe();
    let namespace_symbol2 = namespace_symbol.clone();
    let lz = get_lazy_module_type_or_any_src(&resolved_require);
    let resolved = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
        let result = lz.get_forced(cx).clone();
        match result {
            Ok(m) => {
                let (values_type, types_tmap) = flow_js_utils::import_module_ns_t_kit::on_module_t(
                    cx,
                    false,
                    reason2.dupe(),
                    is_strict,
                    &m,
                )
                .unwrap_or_else(|_| {
                    (
                        type_::any_t::error(reason2.dupe()),
                        properties::Id::generate_id(),
                    )
                });
                Type::new(TypeInner::NamespaceT(Rc::new(type_::NamespaceType {
                    namespace_symbol: namespace_symbol2.clone(),
                    values_type,
                    types_tmap,
                })))
            }
            Err(src) => type_::any_t::why(src, reason2.dupe()),
        }
    })
        as Box<dyn FnOnce(&Context<'cx>) -> Type>));
    mk_sig_tvar(cx, reason, resolved)
}

pub fn copy_named_exports<'cx>(
    cx: &Context<'cx>,
    source_module: Result<type_::ModuleType, Type>,
    target_module_type: &type_::ModuleType,
) {
    match source_module {
        Ok(m) => {
            flow_js_utils::copy_named_exports_t_kit::mod_module_t(cx, target_module_type, &m);
        }
        Err(_) => (),
    }
}

pub fn copy_type_exports<'cx>(
    cx: &Context<'cx>,
    source_module: Result<type_::ModuleType, Type>,
    reason: Reason,
    target_module_type: &type_::ModuleType,
) {
    match source_module {
        Ok(m) => {
            let reason2 = reason.dupe();
            let concretize_export_type =
                |cx: &Context<'cx>,
                 _r: Reason,
                 t: Type|
                 -> Result<Type, flow_utils_concurrency::job_error::JobError> {
                    let result = elab_t(
                        cx,
                        &effective_dst_cx(cx),
                        None,
                        t,
                        Op::new(
                            OpInner::AnnotConcretizeForCJSExtractNamedExportsAndTypeExports(
                                reason2.dupe(),
                            ),
                        ),
                    );
                    Ok(match result.deref() {
                        TypeInner::OpenT(tvar) => {
                            get_fully_resolved_type(cx, &effective_dst_cx(cx), tvar.id() as i32)
                        }
                        _ => result,
                    })
                };
            // The closure above never propagates a JobError (no `?`-failing
            // call sites in its body), so mod_module_t cannot fail here.
            // .expect documents the invariant; this site does not need a
            // cancel/timeout boundary because annotation inference runs on
            // the type-sig pipeline, not the cancel-aware check pipeline.
            flow_js_utils::copy_type_exports_t_kit::mod_module_t(
                cx,
                concretize_export_type,
                reason,
                target_module_type,
                &m,
            )
            .expect("annotation_inference::copy_type_exports concretize closure is infallible");
        }
        Err(_) => (),
    }
}

pub fn mk_non_generic_render_type<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    renders_variant: type_::RendersVariant,
    t: Type,
) -> Type {
    let concretize = |cx: &Context<'cx>,
                      t: &Type|
     -> Result<Vec<Type>, flow_utils_concurrency::job_error::JobError> {
        let c = TypeCollector::create();
        elab_t(
            cx,
            &effective_dst_cx(cx),
            None,
            t.dupe(),
            Op::new(OpInner::AnnotConcretizeForInspection {
                reason: type_util::reason_of_t(t).dupe(),
                collector: c.clone(),
            }),
        );
        Ok(c.collect().into_iter().collect())
    };
    let is_iterable_for_better_error =
        |_cx: &Context<'cx>,
         _t: &Type|
         -> Result<bool, flow_utils_concurrency::job_error::JobError> { Ok(false) };
    flow_js_utils::render_types::mk_non_generic_render_type(
        cx,
        reason,
        renders_variant,
        false,
        concretize,
        is_iterable_for_better_error,
        t,
    )
}

pub fn arith<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    lhs_t: Type,
    rhs_t: Type,
    kind: type_::arith_kind::ArithKind,
) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        lhs_t,
        Op::new(OpInner::AnnotArithT(Box::new(AnnotArithTData {
            reason,
            flip: false,
            rhs_t,
            kind,
        }))),
    )
}

pub fn unary_arith<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    t: Type,
    kind: type_::UnaryArithKind,
) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotUnaryArithT { reason, kind }),
    )
}

pub fn unary_not<'cx>(cx: &Context<'cx>, reason: Reason, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotNotT(reason)),
    )
}

pub fn mixin<'cx>(cx: &Context<'cx>, reason: Reason, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotMixinT(reason)),
    )
}

// and obj_rest cx reason xs t = elab_t cx t (Annot_ObjRestT (reason, xs))
pub fn obj_rest<'cx>(cx: &Context<'cx>, reason: Reason, xs: Vec<FlowSmolStr>, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotObjRestT {
            reason,
            keys: xs.into(),
        }),
    )
}

pub fn arr_rest<'cx>(cx: &Context<'cx>, _use_op: UseOp, reason: Reason, _i: i32, t: Type) -> Type {
    error_unsupported_reason(
        None,
        cx,
        &effective_dst_cx(cx),
        type_util::reason_of_t(&t).dupe(),
        reason,
    )
}

fn object_kit_concrete<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    use_op: UseOp,
    op: &Op<'cx>,
    reason: Reason,
    resolve_tool: type_::object::ResolveTool,
    tool: type_::object::Tool,
    t: Type,
) -> Type {
    use flow_typing_flow_common::flow_js_utils::FlowJsException;
    use flow_typing_type::type_::object;
    use flow_typing_type::type_::object::ObjectToolObjectMapData;
    use flow_typing_type::type_::object::ObjectToolReactConfigData;

    let add_output = |cx: &Context<'cx>,
                      msg: flow_typing_errors::error_message::ErrorMessage<flow_aloc::ALoc>|
     -> Result<(), FlowJsException> {
        flow_js_utils::add_output(cx, msg)?;
        Ok(())
    };
    let return_ =
        |_cx: &Context<'cx>, _use_op: UseOp, t: Type| -> Result<Type, FlowJsException> { Ok(t) };
    let recurse = |cx: &Context<'cx>,
                   use_op: UseOp,
                   reason: &Reason,
                   resolve_tool: object::ResolveTool,
                   tool: &object::Tool,
                   x: Type|
     -> Result<Type, FlowJsException> {
        Ok(object_kit(
            cx,
            dst_cx,
            use_op,
            reason.dupe(),
            resolve_tool,
            tool.clone(),
            x,
        ))
    };
    let statics = |cx: &Context<'cx>, reason: &Reason, t: &Type| -> Result<Type, FlowJsException> {
        Ok(get_statics(cx, reason.dupe(), t.dupe()))
    };
    let op2 = op.clone();
    let next = |cx: &Context<'cx>,
                use_op: UseOp,
                tool: &object::Tool,
                reason: &Reason,
                x: vec1::Vec1<object::Slice>|
     -> Result<Type, FlowJsException> {
        match tool {
            object::Tool::MakeExact => slice_utils::object_make_exact(cx, reason, x),
            object::Tool::Spread(box (options, state)) => slice_utils::object_spread(
                &|_cx, _use_op, _d1, _d2| Ok(()),
                &add_output,
                &return_,
                &|cx, use_op, reason, resolve_tool, tool, x| {
                    recurse(cx, use_op, reason, resolve_tool, &tool, x)
                },
                options,
                state.clone(),
                cx,
                use_op,
                reason,
                x,
            ),
            object::Tool::Rest(box (options, state)) => {
                let rest_return =
                    |_cx: &Context<'cx>,
                     _use_op: Box<dyn Fn(flow_common::polarity::Polarity) -> UseOp>,
                     _options: object::rest::MergeMode,
                     t: Type|
                     -> Result<Type, FlowJsException> { Ok(t) };
                let subt_check = |_use_op: UseOp,
                                  _cx: &Context<'cx>,
                                  (_t1, _t2): (&Type, &Type)|
                 -> Result<(), FlowJsException> { Ok(()) };
                slice_utils::object_rest(
                    &add_output,
                    &rest_return,
                    &|cx, use_op, reason, resolve_tool, tool, x| {
                        recurse(cx, use_op, reason, resolve_tool, &tool, x)
                    },
                    &subt_check,
                    options,
                    state,
                    cx,
                    use_op,
                    reason,
                    x,
                )
            }
            object::Tool::Partial => Ok(slice_utils::object_update_optionality(
                slice_utils::ObjectUpdateOptionalityKind::Partial,
                cx,
                reason,
                x,
            )),
            object::Tool::Required => Ok(slice_utils::object_update_optionality(
                slice_utils::ObjectUpdateOptionalityKind::Required,
                cx,
                reason,
                x,
            )),
            object::Tool::ReadOnly => Ok(slice_utils::object_read_only(cx, reason, x)),
            object::Tool::ReactConfig(box ObjectToolReactConfigData { .. }) => {
                Ok(error_internal(cx, dst_cx, "ReactConfig", &op2))
            }
            object::Tool::ReactCheckComponentConfig {
                props,
                allow_ref_in_spread,
            } => slice_utils::check_component_config(
                &add_output,
                &return_,
                *allow_ref_in_spread,
                props,
                cx,
                use_op,
                reason,
                x,
            ),
            object::Tool::ObjectRep => Ok(error_internal(cx, dst_cx, "ObjectRep", &op2)),
            // TODO(jmbrown): Annotation inference for Mapped Types
            object::Tool::ObjectMap(box ObjectToolObjectMapData { .. }) => {
                Ok(error_internal(cx, dst_cx, "ObjectMap", &op2))
            }
        }
    };
    slice_utils::run(
        &add_output,
        &return_,
        &next,
        &recurse,
        &statics,
        cx,
        use_op,
        &reason,
        resolve_tool,
        &tool,
        &t,
    )
    .unwrap()
}

fn object_kit<'cx>(
    cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    use_op: UseOp,
    reason: Reason,
    resolve_tool: type_::object::ResolveTool,
    tool: type_::object::Tool,
    t: Type,
) -> Type {
    elab_t(
        cx,
        dst_cx,
        None,
        t,
        Op::new(OpInner::AnnotObjKitT(Box::new(AnnotObjKitTData {
            reason,
            use_op,
            resolve_tool,
            tool,
        }))),
    )
}

pub fn object_spread<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: Reason,
    target: type_::object::spread::Target,
    state: type_::object::spread::State,
    t: Type,
) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    let tool = type_::object::Tool::Spread(Box::new((target, state)));
    object_kit(
        cx,
        &effective_dst_cx(cx),
        use_op,
        reason,
        resolve_tool,
        tool,
        t,
    )
}

fn object_rest_internal<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: Reason,
    target: type_::object::rest::MergeMode,
    state: type_::object::rest::State,
    t: Type,
) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    let tool = type_::object::Tool::Rest(Box::new((target, state)));
    object_kit(
        cx,
        &effective_dst_cx(cx),
        use_op,
        reason,
        resolve_tool,
        tool,
        t,
    )
}

fn make_readonly<'cx>(cx: &Context<'cx>, use_op: UseOp, reason: Reason, t: Type) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    object_kit(
        cx,
        &effective_dst_cx(cx),
        use_op,
        reason,
        resolve_tool,
        type_::object::Tool::ReadOnly,
        t,
    )
}

fn make_partial<'cx>(cx: &Context<'cx>, use_op: UseOp, reason: Reason, t: Type) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    object_kit(
        cx,
        &effective_dst_cx(cx),
        use_op,
        reason,
        resolve_tool,
        type_::object::Tool::Partial,
        t,
    )
}

fn make_required<'cx>(cx: &Context<'cx>, use_op: UseOp, reason: Reason, t: Type) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    object_kit(
        cx,
        &effective_dst_cx(cx),
        use_op,
        reason,
        resolve_tool,
        type_::object::Tool::Required,
        t,
    )
}

fn make_exact<'cx>(cx: &Context<'cx>, reason: Reason, t: Type) -> Type {
    let resolve_tool = type_::object::ResolveTool::Resolve(type_::object::Resolve::Next);
    object_kit(
        cx,
        &effective_dst_cx(cx),
        type_::unknown_use(),
        reason,
        resolve_tool,
        type_::object::Tool::MakeExact,
        t,
    )
}

pub fn obj_test_proto<'cx>(cx: &Context<'cx>, reason: Reason, t: Type) -> Type {
    elab_t(
        cx,
        &effective_dst_cx(cx),
        None,
        t,
        Op::new(OpInner::AnnotObjTestProtoT(reason)),
    )
}
