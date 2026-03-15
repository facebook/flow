/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
use flow_typing_type::type_::GetPropTData;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeTKind;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util::reason_of_t;

pub fn specialize(
    cx: &Context,
    c: Type,
    use_op: UseOp,
    reason_op: Reason,
    reason_tapp: Reason,
    targs: Option<Vec<Type>>,
) -> Type {
    let reason = reason_of_t(&c).dupe();
    let cx_inner = cx.dupe();
    let reason_inner = reason.dupe();
    let f = move |c: Type| -> Type {
        let cx_ref = &cx_inner;
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx_ref, reason_inner.dupe(), move |tvar| {
            let use_t = UseT::new(UseTInner::SpecializeT(
                use_op,
                reason_op,
                reason_tapp,
                targs.map(Rc::from),
                tvar.dupe(),
            ));
            flow_js::flow_non_speculating(cx_ref, (&c, &use_t));
        })
    };
    map_on_resolved_type(cx, reason, c, f)
}

pub fn mixin(cx: &Context, reason: Reason, i: Type) -> Type {
    let cx_inner = cx.dupe();
    let reason_inner = reason.dupe();
    let f = move |i: Type| -> Type {
        let cx_ref = &cx_inner;
        let reason_for_mixin = reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx_ref, reason_inner, move |tout| {
            let use_t = UseT::new(UseTInner::MixinT(reason_for_mixin, tout.dupe()));
            flow_js::flow_non_speculating(cx_ref, (&i, &use_t));
        })
    };
    map_on_resolved_type(cx, reason, i, f)
}

pub fn obj_test_proto(cx: &Context, reason: Reason, t: Type) -> Type {
    let cx_inner = cx.dupe();
    let reason_inner = reason.dupe();
    let f = move |t: Type| -> Type {
        let cx_ref = &cx_inner;
        let reason_for_proto = reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx_ref, reason_inner, move |tout| {
            let use_t = UseT::new(UseTInner::ObjTestProtoT(reason_for_proto, tout.dupe()));
            flow_js::flow_non_speculating(cx_ref, (&t, &use_t));
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
pub fn get_prop(
    cx: &Context,
    use_op: UseOp,
    reason: Reason,
    op_reason: Option<Reason>,
    name: Name,
    l: Type,
) -> Type {
    let op_reason = op_reason.unwrap_or_else(|| reason.dupe());
    let cx_inner = cx.dupe();
    let op_reason_inner = op_reason.dupe();
    let f = move |l: Type| -> Type {
        let cx_ref = &cx_inner;
        let op_reason_for_flow = op_reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
            cx_ref,
            op_reason_inner,
            move |tout_reason, tout_id| {
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
                flow_js::flow_non_speculating(cx_ref, (&l, &use_t))
            },
        )
    };
    map_on_resolved_type(cx, op_reason, l, f)
}

pub fn qualify_type(
    cx: &Context,
    use_op: UseOp,
    reason: Reason,
    op_reason: Reason,
    prop_name: Name,
    l: Type,
) -> Type {
    let cx_inner = cx.dupe();
    let op_reason_inner = op_reason.dupe();
    let f = move |l: Type| -> Type {
        let cx_ref = &cx_inner;
        let op_reason_for_flow = op_reason_inner.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
            cx_ref,
            op_reason_inner,
            move |tout_reason, tout_id| {
                let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                let use_t = UseT::new(UseTInner::GetTypeFromNamespaceT {
                    use_op,
                    reason: op_reason_for_flow,
                    prop_ref: (reason, prop_name),
                    tout: Box::new(tout),
                });
                flow_js::flow_non_speculating(cx_ref, (&l, &use_t));
            },
        )
    };
    map_on_resolved_type(cx, op_reason, l, f)
}

pub fn mk_instance(
    cx: &Context,
    instance_reason: Reason,
    l: Type,
    type_t_kind: Option<TypeTKind>,
    use_desc: Option<bool>,
) -> Type {
    let type_t_kind = type_t_kind.unwrap_or(TypeTKind::InstanceKind);
    let use_desc = use_desc.unwrap_or(false);
    let cx_clone = cx.dupe();
    let instance_reason_clone = instance_reason.dupe();
    let f = move |t: Type| {
        let t =
            FlowJs::singleton_concrete_type_for_inspection(&cx_clone, &instance_reason_clone, &t)
                .expect("Should not be under speculation");
        let t = value_to_type_reference_transform::run_on_concrete_type(
            &cx_clone,
            unknown_use(),
            &instance_reason_clone,
            type_t_kind,
            t,
        )
        .expect("Should not be under speculation");
        tvar_resolver::resolved_t(tvar_resolver::default_no_lowers, true, &cx_clone, t)
    };
    Type::new(TypeInner::AnnotT(
        instance_reason.dupe(),
        map_on_resolved_type(cx, instance_reason, l, f),
        use_desc,
    ))
}
