/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils::map_on_resolved_type;
use flow_typing_flow_common::flow_js_utils::value_to_type_reference_transform;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_flow_js::tvar_resolver;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::GetPropTData;
use flow_typing_type::type_::GetTypeFromNamespaceTData;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::SpecializeTData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeTKind;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::ValueToTypeReferenceTData;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util::reason_of_t;

pub fn specialize<'a>(
    cx: &Context<'a>,
    c: Type,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    targs: Option<Vec<Type>>,
) -> Type {
    let reason = reason_of_t(&c).dupe();
    let reason_inner = reason.dupe();
    let f = move |cx: &Context<'_>, c: Type| -> Type {
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason_inner.dupe(), move |cx, tvar| {
            let use_t = UseT::new(UseTInner::SpecializeT(Box::new(SpecializeTData {
                use_op,
                reason: reason_op,
                reason2: reason_tapp,
                targs: targs.map(Rc::from),
                tvar: tvar.dupe(),
            })));
            flow_js::flow_non_speculating(cx, (&c, &use_t));
        })
    };
    map_on_resolved_type(cx, reason, c, f)
}

pub fn mixin<'a>(cx: &Context<'a>, reason: Reason, i: Type) -> Type {
    let reason_inner = reason.dupe();
    let f = move |cx: &Context<'_>, i: Type| -> Type {
        let reason_for_mixin = reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason_inner, move |cx, tout| {
            let use_t = UseT::new(UseTInner::MixinT(reason_for_mixin, tout.dupe()));
            flow_js::flow_non_speculating(cx, (&i, &use_t));
        })
    };
    map_on_resolved_type(cx, reason, i, f)
}

pub fn obj_test_proto<'a>(cx: &Context<'a>, reason: Reason, t: Type) -> Type {
    let reason_inner = reason.dupe();
    let f = move |cx: &Context<'_>, t: Type| -> Type {
        let reason_for_proto = reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason_inner, move |cx, tout| {
            let use_t = UseT::new(UseTInner::ObjTestProtoT(reason_for_proto, tout.dupe()));
            flow_js::flow_non_speculating(cx, (&t, &use_t));
        })
    };
    map_on_resolved_type(cx, reason, t, f)
}

// let get_prop cx use_op reason ?(op_reason = reason) name l =
//   let f l =
//     Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx op_reason (fun tout ->
//         Flow.flow
//           cx
//           ( l,
//             GetPropT
//               {
//                 use_op;
//                 reason = op_reason;
//                 id = None;
//                 from_annot = false;
//                 skip_optional = false;
//                 propref = mk_named_prop ~reason name;
//                 tout;
//                 hint = hint_unavailable;
//               }
//           )
//     )
//   in
//   map_on_resolved_type cx op_reason l f
pub fn get_prop<'a>(
    cx: &Context<'a>,
    use_op: UseOp,
    reason: Reason,
    op_reason: Option<Reason>,
    name: Name,
    l: Type,
) -> Type {
    let op_reason = op_reason.unwrap_or_else(|| reason.dupe());
    let op_reason_inner = op_reason.dupe();
    let f = move |cx: &Context<'_>, l: Type| -> Type {
        let op_reason_for_flow = op_reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
            cx,
            op_reason_inner,
            move |cx, tout_reason, tout_id| {
                let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                let use_t = UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                    use_op,
                    reason: op_reason_for_flow,
                    id: None,
                    from_annot: false,
                    skip_optional: false,
                    propref: Box::new(PropRef::Named {
                        reason,
                        name,
                        from_indexed_access: false,
                    }),
                    tout: Box::new(tout),
                    hint: hint_unavailable(),
                })));
                flow_js::flow_non_speculating(cx, (&l, &use_t))
            },
        )
    };
    map_on_resolved_type(cx, op_reason, l, f)
}

pub fn qualify_type<'a>(
    cx: &Context<'a>,
    use_op: UseOp,
    reason: Reason,
    op_reason: Reason,
    prop_name: Name,
    l: Type,
) -> Type {
    let op_reason_inner = op_reason.dupe();
    let f = move |cx: &Context<'_>, l: Type| -> Type {
        let op_reason_for_flow = op_reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
            cx,
            op_reason_inner,
            move |cx, tout_reason, tout_id| {
                let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                let use_t = UseT::new(UseTInner::GetTypeFromNamespaceT(Box::new(
                    GetTypeFromNamespaceTData {
                        use_op,
                        reason: op_reason_for_flow,
                        prop_ref: (reason, prop_name),
                        tout: Box::new(tout),
                    },
                )));
                flow_js::flow_non_speculating(cx, (&l, &use_t));
            },
        )
    };
    map_on_resolved_type(cx, op_reason, l, f)
}

pub fn mk_instance<'a>(
    cx: &Context<'a>,
    instance_reason: Reason,
    l: Type,
    type_t_kind: Option<TypeTKind>,
    use_desc: Option<bool>,
) -> Type {
    let type_t_kind = type_t_kind.unwrap_or(TypeTKind::InstanceKind);
    let use_desc = use_desc.unwrap_or(false);
    let instance_reason_clone = instance_reason.dupe();
    let f = move |cx: &Context<'_>, t: Type| {
        let concrete =
            FlowJs::singleton_concrete_type_for_inspection(cx, &instance_reason_clone, &t)
                .expect("Should not be under speculation");
        match concrete.deref() {
            TypeInner::DefT(_, def_t)
                if let DefTInner::PolyT(box PolyTData { tparams: ids, .. }) = def_t.deref()
                    && flow_common::files::has_ts_ext(cx.file())
                    && ids.iter().all(|tp| tp.default.is_some()) =>
            {
                // In .ts files, treat missing type args the same as empty type args (Foo = Foo<>),
                // matching TypeScript behavior where defaults are used. Route through flow engine
                // so the PolyT + ValueToTypeReferenceT handler in flow_js.ml applies.
                // Only when all params have defaults; otherwise fall through to EMissingTypeArgs.
                tvar_resolver::mk_tvar_and_fully_resolve_where(
                    cx,
                    instance_reason_clone.dupe(),
                    |cx, tout| {
                        let tvar = match tout.deref() {
                            TypeInner::OpenT(tvar) => tvar.dupe(),
                            _ => unreachable!("mk_where always creates OpenT"),
                        };
                        let use_t = UseT::new(UseTInner::ValueToTypeReferenceT(Box::new(
                            ValueToTypeReferenceTData {
                                use_op: unknown_use(),
                                reason: instance_reason_clone.dupe(),
                                kind: type_t_kind,
                                tout: Box::new(tvar),
                            },
                        )));
                        flow_js::flow_non_speculating(cx, (&concrete, &use_t));
                    },
                )
            }
            _ => {
                let t = value_to_type_reference_transform::run_on_concrete_type(
                    cx,
                    unknown_use(),
                    &instance_reason_clone,
                    type_t_kind,
                    concrete,
                )
                .expect("Should not be under speculation");
                tvar_resolver::resolved_t(tvar_resolver::default_no_lowers, true, cx, t)
            }
        }
    };
    Type::new(TypeInner::AnnotT(
        instance_reason.dupe(),
        map_on_resolved_type(cx, instance_reason, l, f),
        use_desc,
    ))
}
