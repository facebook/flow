/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_typing_context::Context;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::Flags;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::mk_objecttype;
use flow_typing_type::type_::obj_proto;
use flow_typing_type::type_::object::spread::SealType;
use flow_typing_type::type_::properties;

pub fn mk_seal(frozen: bool, as_const: bool) -> SealType {
    if frozen {
        SealType::Frozen
    } else if as_const {
        SealType::AsConst
    } else {
        SealType::Sealed
    }
}

pub fn mk_with_proto(
    cx: &Context,
    reason: Reason,
    obj_kind: ObjKind,
    reachable_targs: Option<Rc<[(Type, Polarity)]>>,
    call: Option<Type>,
    props: Option<properties::PropertiesMap>,
    id: Option<properties::Id>,
    proto: Type,
) -> Type {
    let flags = Flags {
        obj_kind,
        react_dro: None,
    };
    let call = call.map(|t| cx.make_call_prop(t));
    let pmap = match id {
        None => {
            let props = props.unwrap_or_else(properties::PropertiesMap::new);
            cx.generate_property_map(props)
        }
        Some(id) => {
            let props = props.unwrap_or_else(properties::PropertiesMap::new);
            cx.add_property_map(id.dupe(), props);
            id
        }
    };
    Type::new(TypeInner::DefT(
        reason,
        DefT::new(DefTInner::ObjT(Rc::new(mk_objecttype(
            Some(flags),
            reachable_targs,
            call,
            pmap,
            proto,
        )))),
    ))
}

pub fn mk(obj_kind: ObjKind, cx: &Context, reason: Reason) -> Type {
    mk_with_proto(
        cx,
        reason.dupe(),
        obj_kind,
        None,
        None,
        None,
        None,
        obj_proto::make(reason),
    )
}

pub fn is_exact(obj_kind: &ObjKind) -> bool {
    match obj_kind {
        ObjKind::Exact => true,
        _ => false,
    }
}

pub fn get_dict_opt(obj_kind: &ObjKind) -> Option<&DictType> {
    match obj_kind {
        ObjKind::Indexed(d) => Some(d),
        ObjKind::Exact | ObjKind::Inexact => None,
    }
}

pub fn map_dict<F>(f: F, kind: ObjKind) -> ObjKind
where
    F: FnOnce(DictType) -> DictType,
{
    match kind {
        ObjKind::Indexed(d) => ObjKind::Indexed(f(d)),
        ObjKind::Exact | ObjKind::Inexact => kind,
    }
}

pub fn obj_kind_from_optional_dict(dict: Option<DictType>, otherwise: ObjKind) -> ObjKind {
    match dict {
        Some(d) => ObjKind::Indexed(d),
        None => otherwise,
    }
}
