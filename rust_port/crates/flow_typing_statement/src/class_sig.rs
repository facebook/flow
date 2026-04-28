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
use flow_aloc::ALoc;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_js::flow_js;
use flow_typing_loc_env::func_class_sig_types;
use flow_typing_loc_env::func_class_sig_types::ConfigTypes;
use flow_typing_loc_env::func_class_sig_types::class as class_types;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::*;
use flow_typing_type::type_util;
use flow_typing_utils::abnormal::CheckExprError;
use flow_typing_utils::type_env;

use crate::func_sig;
use crate::type_annotation_cons_gen;

pub fn empty<C: ConfigTypes>(
    id: flow_aloc::ALocId,
    class_name: Option<FlowSmolStr>,
    class_loc: ALoc,
    reason: Reason,
    tparams: TypeParams,
    tparams_map: BTreeMap<SubstName, Type>,
    super_: class_types::Super,
) -> class_types::Class<C> {
    let empty_sig = |reason: Reason| -> class_types::Signature<C> {
        class_types::Signature {
            reason,
            fields: BTreeMap::new(),
            private_fields: BTreeMap::new(),
            proto_fields: BTreeMap::new(),
            methods: BTreeMap::new(),
            private_methods: BTreeMap::new(),
            getters: BTreeMap::new(),
            setters: BTreeMap::new(),
            calls: Vec::new(),
            dict: None,
        }
    };
    let constructor = Vec::new();
    let static_ = {
        let reason = reason
            .dupe()
            .update_desc(|desc| VirtualReasonDesc::RStatics(Arc::new(desc)));
        empty_sig(reason)
    };
    let instance = empty_sig(reason);
    class_types::Class {
        id,
        class_name,
        class_loc,
        tparams,
        tparams_map,
        super_,
        constructor,
        static_,
        instance,
    }
}

// (* let structural x = *)
// (*   match x.super with *)
// (*   | Interface _ -> true *)
// (*   | Class _ -> false *)
fn structural<C: ConfigTypes>(x: &class_types::Class<C>) -> bool {
    match &x.super_ {
        class_types::Super::Interface(_) => true,
        class_types::Super::Class(_) => false,
    }
}

fn get_inst_kind<C: ConfigTypes>(x: &class_types::Class<C>) -> InstanceKind {
    match &x.super_ {
        class_types::Super::Interface(iface) => InstanceKind::InterfaceKind {
            inline: iface.inline,
        },
        class_types::Super::Class(_) => InstanceKind::ClassKind,
    }
}

fn map_sig<C: ConfigTypes>(
    static_: bool,
    f: impl FnOnce(&mut class_types::Signature<C>),
    s: &mut class_types::Class<C>,
) {
    if static_ {
        f(&mut s.static_);
    } else {
        f(&mut s.instance);
    }
}

fn with_sig<C: ConfigTypes, R>(
    static_: bool,
    f: impl FnOnce(&class_types::Signature<C>) -> R,
    s: &class_types::Class<C>,
) -> R {
    if static_ {
        f(&s.static_)
    } else {
        f(&s.instance)
    }
}

pub fn add_private_field<C: ConfigTypes>(
    name: FlowSmolStr,
    loc: ALoc,
    polarity: Polarity,
    field: class_types::Field<C>,
    static_: bool,
    x: &mut class_types::Class<C>,
) {
    map_sig(
        static_,
        |s| {
            s.private_fields.insert(name, (Some(loc), polarity, field));
        },
        x,
    );
}

pub fn add_private_method<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    id_loc: ALoc,
    this_write_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: class_types::SetAsts<C>,
    set_type: class_types::SetType,
    x: &mut class_types::Class<C>,
) {
    let func_info = class_types::FuncInfo {
        id_loc: Some(id_loc),
        this_write_loc,
        func_sig: func_sig_,
        set_asts,
        set_type,
    };
    map_sig(
        static_,
        |s| {
            s.private_methods.insert(name, func_info);
        },
        x,
    );
}

pub fn public_fields_of_signature<C: ConfigTypes>(
    static_: bool,
    s: &class_types::Class<C>,
) -> &BTreeMap<FlowSmolStr, class_types::FieldPrime<C>> {
    if static_ {
        &s.static_.fields
    } else {
        &s.instance.fields
    }
}

pub fn private_fields_of_signature<C: ConfigTypes>(
    static_: bool,
    s: &class_types::Class<C>,
) -> &BTreeMap<FlowSmolStr, class_types::FieldPrime<C>> {
    if static_ {
        &s.static_.private_fields
    } else {
        &s.instance.private_fields
    }
}

pub fn add_constructor<C: ConfigTypes>(
    id_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    s: &mut class_types::Class<C>,
) {
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    s.constructor = vec![class_types::FuncInfo {
        id_loc,
        this_write_loc: None,
        func_sig: func_sig::to_ctor_sig(func_sig_),
        set_asts,
        set_type,
    }];
}

pub fn add_default_constructor<C: ConfigTypes>(reason: Reason, s: &mut class_types::Class<C>) {
    let func_sig_ = func_sig::default_constructor(reason);
    add_constructor(None, func_sig_, None, None, s);
}

pub fn append_constructor<C: ConfigTypes>(
    id_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    s: &mut class_types::Class<C>,
) {
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    s.constructor.push(class_types::FuncInfo {
        id_loc,
        this_write_loc: None,
        func_sig: func_sig::to_ctor_sig(func_sig_),
        set_asts,
        set_type,
    });
}

fn add_field_inner<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    fld: class_types::FieldPrime<C>,
    x: &mut class_types::Class<C>,
) {
    let flat = static_ || structural(x);
    map_sig(
        static_,
        |s| {
            s.fields.insert(name.dupe(), fld);
            if flat {
                s.proto_fields.remove(&name);
                s.methods.remove(&name);
                s.getters.remove(&name);
                s.setters.remove(&name);
            }
        },
        x,
    );
}

pub fn add_field<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    loc: ALoc,
    polarity: Polarity,
    field: class_types::Field<C>,
    x: &mut class_types::Class<C>,
) {
    add_field_inner(static_, name, (Some(loc), polarity, field), x);
}

pub fn add_indexer<C: ConfigTypes>(static_: bool, dict: DictType, x: &mut class_types::Class<C>) {
    map_sig(
        static_,
        |s| {
            s.dict = Some(dict);
        },
        x,
    );
}

pub fn has_indexer<C: ConfigTypes>(static_: bool, x: &class_types::Class<C>) -> bool {
    with_sig(static_, |s| s.dict.is_some(), x)
}

pub fn add_name_field<C: ConfigTypes>(x: &mut class_types::Class<C>) {
    let r = x
        .instance
        .reason
        .dupe()
        .update_desc(|desc| VirtualReasonDesc::RNameProperty(Arc::new(desc)));
    let t = str_module_t::why(r);
    add_field_inner(
        true,
        FlowSmolStr::new("name"),
        (None, Polarity::Neutral, class_types::Field::Annot(t)),
        x,
    );
}

pub fn add_proto_field<C: ConfigTypes>(
    name: FlowSmolStr,
    loc: ALoc,
    polarity: Polarity,
    field: class_types::Field<C>,
    x: &mut class_types::Class<C>,
) {
    map_sig(
        false,
        |s| {
            s.proto_fields
                .insert(name.dupe(), (Some(loc), polarity, field));
            s.methods.remove(&name);
            s.getters.remove(&name);
            s.setters.remove(&name);
        },
        x,
    );
}

pub fn add_method<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    id_loc: ALoc,
    this_write_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    x: &mut class_types::Class<C>,
) {
    let flat = static_ || structural(x);
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    let func_info = class_types::FuncInfo {
        id_loc: Some(id_loc),
        this_write_loc,
        func_sig: func_sig_,
        set_asts,
        set_type,
    };
    map_sig(
        static_,
        |s| {
            if flat {
                s.fields.remove(&name);
            }
            s.proto_fields.remove(&name);
            s.methods.insert(name.dupe(), vec1::vec1![func_info]);
            s.getters.remove(&name);
            s.setters.remove(&name);
        },
        x,
    );
}

/// Appending a method builds a list of function signatures. This implements the  
/// bahvior of interfaces and declared classes, which interpret duplicate  
/// definitions as branches of a single overloaded method.  
pub fn append_method<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    id_loc: ALoc,
    this_write_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    x: &mut class_types::Class<C>,
) {
    let flat = static_ || structural(x);
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    let func_info = class_types::FuncInfo {
        id_loc: Some(id_loc),
        this_write_loc,
        func_sig: func_sig_,
        set_asts,
        set_type,
    };
    map_sig(
        static_,
        |s| {
            if flat {
                s.fields.remove(&name);
            }
            s.proto_fields.remove(&name);
            match s.methods.get_mut(&name) {
                Some(fsigs) => {
                    fsigs.push(func_info);
                }
                None => {
                    s.methods.insert(name.dupe(), vec1::vec1![func_info]);
                }
            }
            s.getters.remove(&name);
            s.setters.remove(&name);
        },
        x,
    );
}

pub fn append_call<C: ConfigTypes>(static_: bool, t: Type, x: &mut class_types::Class<C>) {
    map_sig(
        static_,
        |s| {
            s.calls.push(t);
        },
        x,
    );
}

pub fn add_getter<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    id_loc: ALoc,
    this_write_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    x: &mut class_types::Class<C>,
) {
    let flat = static_ || structural(x);
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    let func_info = class_types::FuncInfo {
        id_loc: Some(id_loc),
        this_write_loc,
        func_sig: func_sig_,
        set_asts,
        set_type,
    };
    map_sig(
        static_,
        |s| {
            if flat {
                s.fields.remove(&name);
            }
            s.proto_fields.remove(&name);
            s.methods.remove(&name);
            s.getters.insert(name, func_info);
        },
        x,
    );
}

pub fn add_setter<C: ConfigTypes>(
    static_: bool,
    name: FlowSmolStr,
    id_loc: ALoc,
    this_write_loc: Option<ALoc>,
    func_sig_: func_class_sig_types::func::Func<C>,
    set_asts: Option<class_types::SetAsts<C>>,
    set_type: Option<class_types::SetType>,
    x: &mut class_types::Class<C>,
) {
    let flat = static_ || structural(x);
    let set_asts = set_asts.unwrap_or_else(|| Rc::new(|_| {}));
    let set_type = set_type.unwrap_or_else(|| Rc::new(|_| {}));
    let func_info = class_types::FuncInfo {
        id_loc: Some(id_loc),
        this_write_loc,
        func_sig: func_sig_,
        set_asts,
        set_type,
    };
    map_sig(
        static_,
        |s| {
            if flat {
                s.fields.remove(&name);
            }
            s.proto_fields.remove(&name);
            s.methods.remove(&name);
            s.setters.insert(name, func_info);
        },
        x,
    );
}

pub fn mem_constructor<C: ConfigTypes>(x: &class_types::Class<C>) -> bool {
    !x.constructor.is_empty()
}

fn iter_methods_with_name<C: ConfigTypes>(
    f: &mut impl FnMut(&str, &class_types::FuncInfo<C>) -> Result<(), CheckExprError>,
    s: &class_types::Signature<C>,
) -> Result<(), CheckExprError> {
    for (name, func_infos) in &s.methods {
        for func_info in func_infos {
            f(name, func_info)?;
        }
    }
    for (name, func_info) in &s.private_methods {
        f(name, func_info)?;
    }
    for (name, func_info) in &s.getters {
        f(name, func_info)?;
    }
    for (name, func_info) in &s.setters {
        f(name, func_info)?;
    }
    Ok(())
}

fn iter_methods<C: ConfigTypes>(
    f: &mut impl FnMut(&class_types::FuncInfo<C>) -> Result<(), CheckExprError>,
    s: &class_types::Signature<C>,
) -> Result<(), CheckExprError> {
    iter_methods_with_name(&mut |_, info| f(info), s)
}

fn this_tparam<C: ConfigTypes>(x: &class_types::Class<C>) -> Option<&TypeParam> {
    match &x.super_ {
        class_types::Super::Class(class_super) => Some(&class_super.this_tparam),
        class_types::Super::Interface(_) => None,
    }
}

fn this_t<C: ConfigTypes>(x: &class_types::Class<C>) -> Option<&Type> {
    match &x.super_ {
        class_types::Super::Class(class_super) => Some(&class_super.this_t),
        class_types::Super::Interface(_) => None,
    }
}

fn this_or_mixed<C: ConfigTypes>(loc: ALoc, x: &class_types::Class<C>) -> Type {
    this_t(x)
        .map(|t| t.dupe())
        .unwrap_or_else(|| dummy_this(loc))
}

fn tparams_with_this(tparams: &TypeParams, this_tp: TypeParam) -> TypeParams {
    // Use the loc for the original tparams,
    // or just the loc for the this type if there are no tparams
    let loc = match tparams {
        Some((loc, _)) => loc.dupe(),
        None => this_tp.reason.loc().dupe(),
    };
    // Add the type of `this` to the end of the list of type
    // parameters. Remember, order is important, since we don't have recursive
    // bounds (aka F-bounds): the bound of This refers to all the other type
    // parameters!
    let mut tparams_lst: Vec<TypeParam> = match tparams {
        Some((_, nel)) => nel.iter().cloned().collect(),
        None => Vec::new(),
    };
    tparams_lst.push(this_tp);
    let tparams_nel = vec1::Vec1::try_from_vec(tparams_lst).expect("tparams_lst is non-empty");
    Some((loc, tparams_nel))
}

fn to_field<C: ConfigTypes>(entry: &class_types::FieldPrime<C>) -> Property {
    let (key_loc, polarity, field) = entry;
    let type_ = match field {
        class_types::Field::Annot(t) => t.dupe(),
        class_types::Field::Infer(fsig, _) => func_sig::gettertype(fsig),
    };
    Property::new(PropertyInner::Field(Box::new(FieldData {
        preferred_def_locs: None,
        key_loc: key_loc.dupe(),
        type_,
        polarity: *polarity,
    })))
}

fn to_method<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    this_default: &Type,
    info: &class_types::FuncInfo<C>,
) -> Property {
    Property::new(PropertyInner::Method {
        key_loc: info.id_loc.dupe(),
        type_: func_sig::methodtype(
            cx,
            info.this_write_loc.dupe(),
            this_default.dupe(),
            &info.func_sig,
        ),
    })
}

pub fn fields_to_prop_map<'a, C: ConfigTypes>(
    cx: &Context<'a>,
    fields: &BTreeMap<FlowSmolStr, class_types::FieldPrime<C>>,
) -> properties::Id {
    let pmap: properties::PropertiesMap = fields
        .iter()
        .map(|(name, entry)| (Name::new(name.dupe()), to_field(entry)))
        .collect();
    cx.generate_property_map(pmap)
}

fn methods_to_prop_map<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    this_default: &Type,
    methods: &BTreeMap<FlowSmolStr, class_types::FuncInfo<C>>,
) -> properties::Id {
    let pmap: properties::PropertiesMap = methods
        .iter()
        .map(|(name, info)| (Name::new(name.dupe()), to_method(cx, this_default, info)))
        .collect();
    cx.generate_property_map(pmap)
}

// (* let elements cx ~this ?constructor s super = *)
fn elements<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    this: Type,
    constructor: Option<(Option<ALoc>, Type)>,
    s: &class_types::Signature<C>,
    super_: &class_types::Super,
) -> (
    FlowOrdSet<FlowSmolStr>,
    BTreeMap<FlowSmolStr, Property>,
    BTreeMap<FlowSmolStr, Property>,
    Option<Type>,
) {
    // To determine the default `this` parameter for a method without `this` annotation, we
    // default to the instance/static `this` type
    let this_default = |x: &func_class_sig_types::func::Func<C>| -> Type {
        match (&x.body, super_) {
            // We can use mixed for declared class methods here for two reasons:
            // 1) They can never be unbound
            // 2) They have no body
            (None, class_types::Super::Class(_)) => implicit_mixed_this(x.reason.dupe()),
            (Some(_), class_types::Super::Class(_)) => type_util::mod_reason_of_t(
                &|r: Reason| r.update_desc(|desc| VirtualReasonDesc::RImplicitThis(Arc::new(desc))),
                &this,
            ),
            (_, class_types::Super::Interface(_)) => implicit_mixed_this(x.reason.dupe()),
        }
    };
    // If this is an overloaded method, create an intersection, attributed
    // to the first declared function signature. If there is a single
    // function signature for this method, simply return the method type.
    let mut methods: BTreeMap<FlowSmolStr, (Option<ALoc>, Type)> = BTreeMap::new();
    for (name, xs) in &s.methods {
        let ms: Vec<(Option<ALoc>, Type, &class_types::SetType)> = xs
            .iter()
            .map(|fi| {
                let t = func_sig::methodtype(
                    cx,
                    fi.this_write_loc.dupe(),
                    this_default(&fi.func_sig),
                    &fi.func_sig,
                );
                (fi.id_loc.dupe(), t, &fi.set_type)
            })
            .collect();
        // Keep track of these before intersections are merged, to enable
        // type information on every member of the intersection.
        for (loc, t, set_type) in &ms {
            if loc.is_some() {
                (set_type)(t.dupe());
            }
        }
        let result = match ms.as_slice() {
            [(loc, t, _)] => (loc.dupe(), t.dupe()),
            [(loc0, t0, _), (_, t1, _), rest @ ..] => {
                let ts: Vec<Type> = rest.iter().map(|(_, t, _)| t.dupe()).collect();
                (
                    loc0.dupe(),
                    Type::new(TypeInner::IntersectionT(
                        type_util::reason_of_t(t0).dupe(),
                        inter_rep::make(t0.dupe(), t1.dupe(), ts.into()),
                    )),
                )
            }
            [] => unreachable!("Nel is non-empty"),
        };
        methods.insert(name.dupe(), result);
    }
    let private_this_default = |x: &func_class_sig_types::func::Func<C>| -> Type {
        any_t::make(
            AnySource::Unsound(UnsoundnessKind::BoundFunctionThis),
            x.reason.dupe(),
        )
    };
    for func_info in s.private_methods.values() {
        if func_info.id_loc.is_some() {
            let t = func_sig::methodtype(
                cx,
                func_info.this_write_loc.dupe(),
                private_this_default(&func_info.func_sig),
                &func_info.func_sig,
            );
            (func_info.set_type)(t);
        }
    }
    // Re-add the constructor as a method.
    if let Some(t) = constructor {
        methods.insert(FlowSmolStr::new("constructor"), t);
    }
    // If there is a both a getter and a setter, then flow the setter type to
    // the getter. Otherwise just use the getter type or the setter type
    let getters: BTreeMap<FlowSmolStr, (Option<ALoc>, Type, &class_types::SetType)> = s
        .getters
        .iter()
        .map(|(name, fi)| {
            (
                name.dupe(),
                (
                    fi.id_loc.dupe(),
                    func_sig::gettertype(&fi.func_sig),
                    &fi.set_type,
                ),
            )
        })
        .collect();
    let setters: BTreeMap<FlowSmolStr, (Option<ALoc>, Type, &class_types::SetType)> = s
        .setters
        .iter()
        .map(|(name, fi)| {
            (
                name.dupe(),
                (
                    fi.id_loc.dupe(),
                    func_sig::settertype(&fi.func_sig),
                    &fi.set_type,
                ),
            )
        })
        .collect();
    // Register getters and setters with the typed AST
    for (loc, t, set_type) in getters.values() {
        if loc.is_some() {
            (set_type)(t.dupe());
        }
    }
    for (loc, t, set_type) in setters.values() {
        if loc.is_some() {
            (set_type)(t.dupe());
        }
    }
    let mut getters_and_setters: BTreeMap<FlowSmolStr, Property> = BTreeMap::new();
    let all_accessor_keys: BTreeSet<FlowSmolStr> =
        getters.keys().chain(setters.keys()).duped().collect();
    for name in all_accessor_keys {
        let getter = getters.get(&name);
        let setter = setters.get(&name);
        let prop = match (getter, setter) {
            (Some((get_key_loc, get_type, _)), Some((set_key_loc, set_type, _))) => {
                Property::new(PropertyInner::GetSet(Box::new(GetSetData {
                    get_key_loc: get_key_loc.dupe(),
                    get_type: get_type.dupe(),
                    set_key_loc: set_key_loc.dupe(),
                    set_type: set_type.dupe(),
                })))
            }
            (Some((key_loc, type_, _)), None) => Property::new(PropertyInner::Get {
                key_loc: key_loc.dupe(),
                type_: type_.dupe(),
            }),
            (None, Some((key_loc, type_, _))) => Property::new(PropertyInner::Set {
                key_loc: key_loc.dupe(),
                type_: type_.dupe(),
            }),
            (None, None) => continue,
        };
        getters_and_setters.insert(name, prop);
    }
    let fields: BTreeMap<FlowSmolStr, Property> = s
        .fields
        .iter()
        .map(|(name, entry)| (name.dupe(), to_field(entry)))
        .collect();
    let mut methods_props: BTreeMap<FlowSmolStr, Property> = methods
        .into_iter()
        .map(|(name, (key_loc, type_))| {
            (
                name,
                Property::new(PropertyInner::Method { key_loc, type_ }),
            )
        })
        .collect();
    // Treat proto fields as methods, as they are on the proto object
    for (name, fld) in &s.proto_fields {
        methods_props.insert(name.dupe(), to_field(fld));
    }
    // Treat getters and setters as methods
    for (name, prop) in getters_and_setters {
        methods_props.insert(name, prop);
    }
    let call = {
        let mut iter = s.calls.iter().duped();
        if let Some(t0) = iter.next() {
            if let Some(t1) = iter.next() {
                let ts: Vec<Type> = iter.collect();
                Some(Type::new(TypeInner::IntersectionT(
                    type_util::reason_of_t(&t0).dupe(),
                    inter_rep::make(t0, t1, ts.into()),
                )))
            } else {
                Some(t0)
            }
        } else {
            None
        }
    };
    // Only un-initialized fields require annotations, so determine now
    // (syntactically) which fields have initializers
    let mut initialized_fields = FlowOrdSet::new();
    for (x, (_, _, field)) in &s.fields {
        match field {
            class_types::Field::Annot(_) => {}
            class_types::Field::Infer(_, _) => {
                initialized_fields.insert(x.dupe());
            }
        }
    }
    (initialized_fields, fields, methods_props, call)
}

fn specialize<'a>(cx: &Context<'a>, use_op: UseOp, targs: Option<Vec<Type>>, c: Type) -> Type {
    let reason = type_util::reason_of_t(&c).dupe();
    type_annotation_cons_gen::specialize(cx, c, use_op, reason.dupe(), reason, targs)
}

fn statictype<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    static_proto: Type,
    x: &class_types::Class<C>,
) -> (FlowOrdSet<FlowSmolStr>, ObjType) {
    let s = &x.static_;
    let loc = x.static_.reason.loc().dupe();
    let this = type_util::class_type(this_or_mixed(loc, x), false, None);
    let (inited_fields, fields, methods, call) = elements(cx, this, None, s, &x.super_);
    let props: properties::PropertiesMap = {
        let mut seen = std::collections::BTreeSet::new();
        fields
            .iter()
            .chain(methods.iter())
            .map(|(name, prop)| {
                let name_key = Name::new(name.as_str());
                if !seen.insert(name_key.dupe()) {
                    panic!(
                        "static fields and methods must be disjoint: {}",
                        flow_common::reason::dump_reason(None, &s.reason)
                    );
                }
                (name_key, prop.dupe())
            })
            .collect()
    };
    // Statics are not exact, because we allow width subtyping between them.
    // Specifically, given class A and class B extends A, Class<B> <: Class<A>.
    let static_ = obj_type::mk_with_proto(
        cx,
        s.reason.dupe(),
        ObjKind::Inexact,
        None,
        call,
        Some(props),
        None,
        static_proto,
    );
    match static_.deref() {
        TypeInner::DefT(_, def_t) => match def_t.deref() {
            DefTInner::ObjT(o) => (inited_fields, o.as_ref().clone()),
            _ => panic!("statics must be an ObjT"),
        },
        _ => panic!("statics must be an ObjT"),
    }
}

fn insttype<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    initialized_static_fields: FlowOrdSet<FlowSmolStr>,
    inst_kind: Option<InstanceKind>,
    s: &class_types::Class<C>,
) -> InstType {
    let constructor = {
        // Constructors do not bind `this`
        let ts: Vec<(Option<ALoc>, Type)> = s
            .constructor
            .iter()
            .map(|fi| {
                let t = func_sig::methodtype(
                    cx,
                    None,
                    dummy_this(s.instance.reason.loc().dupe()),
                    &fi.func_sig,
                );
                if fi.id_loc.is_some() {
                    (fi.set_type)(t.dupe());
                }
                (fi.id_loc.dupe(), t)
            })
            .collect();
        match ts.as_slice() {
            [] => None,
            [(loc, t)] => Some((loc.dupe(), t.dupe())),
            [(loc0, t0), (_, t1), rest @ ..] => {
                let ts: Vec<Type> = rest.iter().map(|(_, t)| t.dupe()).collect();
                Some((
                    loc0.dupe(),
                    Type::new(TypeInner::IntersectionT(
                        type_util::reason_of_t(t0).dupe(),
                        inter_rep::make(t0.dupe(), t1.dupe(), ts.into()),
                    )),
                ))
            }
        }
    };
    let type_args: Vec<(SubstName, Reason, Type, Polarity)> = match &s.tparams {
        None => Vec::new(),
        Some((_, nel)) => nel
            .iter()
            .map(|tp| {
                let t = s
                    .tparams_map
                    .get(&tp.name)
                    .expect("tparam not found in tparams_map")
                    .dupe();
                (tp.name.dupe(), tp.reason.dupe(), t, tp.polarity)
            })
            .collect(),
    };
    let loc = s.instance.reason.loc().dupe();
    let (initialized_fields, fields, methods, call) = elements(
        cx,
        this_or_mixed(loc, s),
        constructor,
        &s.instance,
        &s.super_,
    );
    let private_this_type = |sig_: &class_types::Signature<C>| -> Type {
        any_t::make(
            AnySource::Unsound(UnsoundnessKind::BoundFunctionThis),
            sig_.reason.dupe(),
        )
    };
    let own_pmap: properties::PropertiesMap = fields
        .iter()
        .map(|(name, prop)| (Name::new(name.dupe()), prop.dupe()))
        .collect();
    let proto_pmap: properties::PropertiesMap = methods
        .iter()
        .map(|(name, prop)| (Name::new(name.dupe()), prop.dupe()))
        .collect();
    InstType::new(InstTypeInner {
        class_id: s.id.dupe(),
        inst_react_dro: None,
        class_name: s.class_name.dupe(),
        type_args: type_args.into(),
        own_props: cx.generate_property_map(own_pmap),
        proto_props: cx.generate_property_map(proto_pmap),
        inst_call_t: call.map(|t| cx.make_call_prop(t)),
        initialized_fields,
        initialized_static_fields,
        inst_kind: inst_kind.unwrap_or_else(|| get_inst_kind(s)),
        inst_dict: s.instance.dict.clone(),
        class_private_fields: fields_to_prop_map(cx, &s.instance.private_fields),
        class_private_static_fields: fields_to_prop_map(cx, &s.static_.private_fields),
        class_private_methods: methods_to_prop_map(
            cx,
            &private_this_type(&s.instance),
            &s.instance.private_methods,
        ),
        class_private_static_methods: methods_to_prop_map(
            cx,
            &private_this_type(&s.static_),
            &s.static_.private_methods,
        ),
    })
}

pub fn mk_this<'a>(self_: Type, cx: &Context<'a>, reason: Reason) -> (TypeParam, Type) {
    let this_reason = reason.replace_desc(VirtualReasonDesc::RThisType);
    let this_tp = TypeParam::new(TypeParamInner {
        name: SubstName::name(FlowSmolStr::new("this")),
        reason: this_reason,
        bound: self_,
        polarity: Polarity::Positive,
        default: None,
        is_this: true,
        is_const: false,
    });
    let generic = flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), &this_tp);
    (this_tp, generic)
}

fn supertype<'a, C: ConfigTypes>(cx: &Context<'a>, x: &class_types::Class<C>) -> (Type, Type) {
    let super_reason = x
        .instance
        .reason
        .dupe()
        .update_desc(|d| VirtualReasonDesc::RSuperOf(Arc::new(d)));
    let static_reason = x.static_.reason.dupe();
    match &x.super_ {
        class_types::Super::Interface(iface) => {
            let extends: Vec<Type> = iface
                .extends
                .iter()
                .map(|(annot_loc, c, targs_opt)| match targs_opt {
                    None => {
                        let reason = type_util::reason_of_t(c)
                            .dupe()
                            .reposition(annot_loc.dupe())
                            .annotate(annot_loc.dupe());
                        type_annotation_cons_gen::mk_instance(cx, reason, c.dupe(), None, None)
                    }
                    Some(targs) => type_util::typeapp_annot(
                        false,
                        false,
                        annot_loc.dupe(),
                        c.dupe(),
                        targs.clone(),
                    ),
                })
                .collect();
            // If the interface definition includes a callable property, add the
            // function prototype to the super type
            let extends = if iface.callable {
                let mut e = extends;
                e.push(Type::new(TypeInner::FunProtoT(super_reason.dupe())));
                e
            } else {
                extends
            };
            // Interfaces support multiple inheritance, which is modelled as an
            // intersection of super types. TODO: Instead of using an intersection for
            // this, we should resolve the extends and build a flattened type, and just
            // use FunProtoT/ObjProtoT as the prototype
            let super_ = match extends.len() {
                0 => Type::new(TypeInner::ObjProtoT(super_reason.dupe())),
                1 => extends.into_iter().next().unwrap(),
                _ => {
                    let mut iter = extends.into_iter();
                    let t0 = iter.next().unwrap();
                    let t1 = iter.next().unwrap();
                    let ts: Vec<Type> = iter.collect();
                    Type::new(TypeInner::IntersectionT(
                        super_reason.dupe(),
                        inter_rep::make(t0, t1, ts.into()),
                    ))
                }
            };
            let static_proto = Type::new(TypeInner::NullProtoT(static_reason));
            (super_, static_proto)
        }
        class_types::Super::Class(class_super) => {
            let (extends_t, static_proto) = match &class_super.extends {
                class_types::Extends::Explicit((annot_loc, c, targs)) => {
                    // Eagerly specialize when there are no targs
                    let c = if targs.is_none() {
                        let use_op = UseOp::Op(Arc::new(RootUseOp::ClassExtendsCheck {
                            def: type_util::reason_of_t(c).dupe(),
                            extends: x.instance.reason.dupe(),
                        }));
                        specialize(cx, use_op, targs.clone(), c.dupe())
                    } else {
                        c.dupe()
                    };
                    let t = type_util::this_typeapp(
                        c.dupe(),
                        class_super.this_t.dupe(),
                        targs.clone(),
                        Some(annot_loc.dupe()),
                    );
                    // class B extends A {}; B.__proto__ === A
                    let static_proto =
                        type_util::class_type(t.dupe(), false, Some(annot_loc.dupe()));
                    (t, static_proto)
                }
                class_types::Extends::Implicit { null } => {
                    let t = if *null {
                        Type::new(TypeInner::NullProtoT(super_reason.dupe()))
                    } else {
                        Type::new(TypeInner::ObjProtoT(super_reason.dupe()))
                    };
                    // class A {}; A.__proto__ === Function.prototype
                    let static_proto = Type::new(TypeInner::FunProtoT(static_reason));
                    (t, static_proto)
                }
            };
            let mixins: Vec<Type> = class_super
                .mixins
                .iter()
                .map(|(annot_loc, c, targs)| {
                    // Eagerly specialize when there are no targs
                    let c = if targs.is_none() {
                        specialize(cx, unknown_use(), targs.clone(), c.dupe())
                    } else {
                        c.dupe()
                    };
                    type_util::this_typeapp(
                        c,
                        class_super.this_t.dupe(),
                        targs.clone(),
                        Some(annot_loc.dupe()),
                    )
                })
                .collect();
            let mut all: Vec<Type> = mixins;
            all.push(extends_t);
            let super_ = match all.len() {
                0 => panic!("impossible"),
                1 => all.into_iter().next().unwrap(),
                _ => {
                    let mut iter = all.into_iter();
                    let t0 = iter.next().unwrap();
                    let t1 = iter.next().unwrap();
                    let ts: Vec<Type> = iter.collect();
                    Type::new(TypeInner::IntersectionT(
                        super_reason,
                        inter_rep::make(t0, t1, ts.into()),
                    ))
                }
            };
            (super_, static_proto)
        }
    }
}

fn this_instance_type<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    inst_kind: Option<InstanceKind>,
    x: &class_types::Class<C>,
) -> (Reason, InstanceT) {
    let sreason = x.static_.reason.dupe();
    let reason = x.instance.reason.dupe();
    let (super_, static_proto) = supertype(cx, x);
    let implements: Vec<Type> = match &x.super_ {
        class_types::Super::Interface(_) => Vec::new(),
        class_types::Super::Class(class_super) => class_super
            .implements
            .iter()
            .map(|(annot_loc, c, targs_opt)| match targs_opt {
                None => {
                    let reason = type_util::reason_of_t(c)
                        .dupe()
                        .reposition(annot_loc.dupe())
                        .annotate(annot_loc.dupe());
                    type_annotation_cons_gen::mk_instance(cx, reason, c.dupe(), None, None)
                }
                Some(targs) => type_util::typeapp_annot(
                    false,
                    false,
                    annot_loc.dupe(),
                    c.dupe(),
                    targs.clone(),
                ),
            })
            .collect(),
    };
    let (initialized_static_fields, static_objtype) = statictype(cx, static_proto, x);
    let inst = insttype(cx, initialized_static_fields, inst_kind, x);
    let static_ = Type::new(TypeInner::DefT(
        sreason,
        DefT::new(DefTInner::ObjT(Rc::new(static_objtype))),
    ));
    (
        reason,
        InstanceT::new(InstanceTInner {
            inst,
            static_,
            super_,
            implements: implements.into(),
        }),
    )
}

pub fn thistype<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    x: &class_types::Class<C>,
) -> Type {
    let (reason, instance_t) = this_instance_type(cx, None, x);
    Type::new(TypeInner::DefT(
        reason,
        DefT::new(DefTInner::InstanceT(Rc::new(instance_t))),
    ))
}

fn check_methods<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    def_reason: Reason,
    x: &class_types::Class<C>,
) {
    let self_ = match &x.super_ {
        class_types::Super::Interface(_) => thistype(cx, x),
        class_types::Super::Class(class_super) => class_super.this_t.dupe(),
    };
    let check_method =
        |msig: &func_class_sig_types::func::Func<C>, static_: bool, name: &str, id_loc: ALoc| {
            if let Some(this_param) = func_sig::this_param(&msig.fparams) {
                let self_val = if static_ {
                    type_util::class_type(self_.dupe(), false, None)
                } else {
                    self_.dupe()
                };
                let reason = mk_reason(
                    VirtualReasonDesc::RMethod(Some(FlowSmolStr::new(name))),
                    id_loc,
                );
                let use_op = UseOp::Op(Arc::new(RootUseOp::ClassMethodDefinition {
                    def: reason.dupe(),
                    name: def_reason.dupe(),
                }));
                cx.add_post_inference_subtyping_check(self_val, use_op, this_param);
            }
        };
    with_sig(
        true,
        |s| {
            iter_methods_with_name(
                &mut |name: &str, fi: &class_types::FuncInfo<C>| {
                    if let Some(id_loc) = &fi.id_loc {
                        check_method(&fi.func_sig, true, name, id_loc.dupe());
                    }
                    Ok(())
                },
                s,
            )
            .unwrap();
        },
        x,
    );
    with_sig(
        false,
        |s| {
            iter_methods_with_name(
                &mut |name: &str, fi: &class_types::FuncInfo<C>| {
                    if let Some(id_loc) = &fi.id_loc {
                        check_method(&fi.func_sig, false, name, id_loc.dupe());
                    }
                    Ok(())
                },
                s,
            )
            .unwrap();
        },
        x,
    );
}

fn check_implements<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    def_reason: Reason,
    x: &class_types::Class<C>,
) {
    match &x.super_ {
        class_types::Super::Interface(_) => {}
        class_types::Super::Class(class_super) => {
            let this = thistype(cx, x);
            let reason = x.instance.reason.dupe();
            for (annot_loc, c, targs_opt) in &class_super.implements {
                let i = match targs_opt {
                    None => {
                        let r = type_util::reason_of_t(c)
                            .dupe()
                            .reposition(annot_loc.dupe())
                            .annotate(annot_loc.dupe());
                        type_annotation_cons_gen::mk_instance(cx, r, c.dupe(), None, None)
                    }
                    Some(targs) => type_util::typeapp_annot(
                        false,
                        false,
                        annot_loc.dupe(),
                        c.dupe(),
                        targs.clone(),
                    ),
                };
                let use_op = UseOp::Op(Arc::new(RootUseOp::ClassImplementsCheck(Box::new(
                    ClassImplementsCheckData {
                        def: def_reason.dupe(),
                        name: reason.dupe(),
                        implements: type_util::reason_of_t(&i).dupe(),
                    },
                ))));
                cx.add_post_inference_validation_flow(
                    i,
                    UseT::new(UseTInner::ImplementsT(use_op, this.dupe())),
                );
            }
        }
    }
}

fn check_super<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    def_reason: Reason,
    x: &class_types::Class<C>,
) {
    let reason = x.instance.reason.dupe();
    // NOTE: SuperT ignores the constructor anyway, so we don't pass it here.
    // Call properties are also ignored, so we ignore that result.
    // The this parameter of a class method is irrelvant for class subtyping, since
    // dynamic dispatch enforces that the method is called on the right subclass
    // at runtime even if the static type is a supertype.
    let inst_loc = reason.loc().dupe();
    let (_, own, proto, _call) = elements(
        cx,
        this_or_mixed(inst_loc.dupe(), x),
        None,
        &x.instance,
        &x.super_,
    );
    let static_ = {
        let this = type_util::class_type(this_or_mixed(inst_loc, x), false, None);
        // NOTE: The own, proto maps are disjoint by construction.
        let (_, own, proto, _call) = elements(cx, this, None, &x.static_, &x.super_);
        let mut merged = own;
        for (k, v) in proto {
            merged.insert(k, v);
        }
        merged
    };
    for (x_name, p1) in &own {
        if let Some(p2) = proto.get(x_name) {
            let prop = Name::new(x_name.as_str());
            let use_op = UseOp::Op(Arc::new(RootUseOp::ClassOwnProtoCheck(Box::new(
                ClassOwnProtoCheckData {
                    prop: prop.dupe(),
                    own_loc: property::first_loc(p1),
                    proto_loc: property::first_loc(p2),
                },
            ))));
            let propref = type_util::mk_named_prop(reason.dupe(), false, prop);
            let pt1 = property::type_(p1);
            let pt2 = property::type_(p2);
            flow_js::FlowJs::flow_p(cx, use_op, &reason, &reason, &propref, &pt1, &pt2)
                .expect("flow_p should not fail outside speculation");
        }
    }
    let (super_, _) = supertype(cx, x);
    let use_op = UseOp::Op(Arc::new(RootUseOp::ClassExtendsCheck {
        def: def_reason,
        extends: type_util::reason_of_t(&super_).dupe(),
    }));
    let mut own_name_map: FlowOrdMap<Name, Property> = FlowOrdMap::new();
    for (k, v) in &own {
        own_name_map.insert(Name::new(k.as_str()), v.dupe());
    }
    let mut proto_name_map: FlowOrdMap<Name, Property> = FlowOrdMap::new();
    for (k, v) in &proto {
        proto_name_map.insert(Name::new(k.as_str()), v.dupe());
    }
    let mut static_name_map: FlowOrdMap<Name, Property> = FlowOrdMap::new();
    for (k, v) in &static_ {
        static_name_map.insert(Name::new(k.as_str()), v.dupe());
    }
    cx.add_post_inference_validation_flow(
        super_,
        UseT::new(UseTInner::SuperT(Box::new(SuperTData {
            use_op,
            reason,
            derived_type: DerivedType {
                own: own_name_map,
                proto: proto_name_map,
                static_: static_name_map,
            },
        }))),
    );
}

pub fn check_signature_compatibility<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    def_reason: Reason,
    x: &class_types::Class<C>,
) {
    check_super(cx, def_reason.dupe(), x);
    check_implements(cx, def_reason.dupe(), x);
    check_methods(cx, def_reason, x);
}

// TODO: Ideally we should check polarity for all class types, but this flag is
// flipped off for interface/declare class currently.
pub fn classtype<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    check_polarity_flag: bool,
    inst_kind: InstanceKind,
    x: &class_types::Class<C>,
) -> (
    Type,
    Type,
    (
        flow_typing_type::type_::properties::Id,
        flow_typing_type::type_::properties::Id,
    ),
) {
    let (this_reason, this_instance_t) = this_instance_type(cx, Some(inst_kind), x);
    let inst_prop_ids = (
        this_instance_t.inst.own_props.dupe(),
        this_instance_t.inst.proto_props.dupe(),
    );
    let this = Type::new(TypeInner::DefT(
        this_reason.dupe(),
        DefT::new(DefTInner::InstanceT(Rc::new(this_instance_t.dupe()))),
    ));
    let this_tp = this_tparam(x);
    let tparams_with_this_val: TypeParams = match this_tp {
        Some(tp) => tparams_with_this(&x.tparams, tp.dupe()),
        None => x.tparams.clone(),
    };
    let this_name = SubstName::name(FlowSmolStr::new("this"));
    match &tparams_with_this_val {
        Some((_, tps)) if check_polarity_flag => {
            // TODO: use tparams_map instead of calculating this here
            let mut tparams_map = BTreeMap::new();
            for tp in tps {
                tparams_map.insert(tp.name.dupe(), tp.dupe());
            }
            // Check delayed so that we don't have to force the currently unresolved OpenT
            // with implicit this tparam.
            cx.add_post_inference_polarity_check(tparams_map, Polarity::Positive, this.dupe());
        }
        _ => {}
    }
    let (t_inner, t_outer) = if structural(x) {
        (this.dupe(), type_util::class_type(this, true, None))
    } else {
        let t_inner = Type::new(TypeInner::ThisInstanceT(Box::new(ThisInstanceTData {
            reason: this_reason.dupe(),
            instance: this_instance_t.dupe(),
            is_this: true,
            subst_name: this_name.dupe(),
        })));
        let t_outer = type_util::class_type(
            Type::new(TypeInner::ThisInstanceT(Box::new(ThisInstanceTData {
                reason: this_reason,
                instance: this_instance_t,
                is_this: false,
                subst_name: this_name,
            }))),
            false,
            None,
        );
        (t_inner, t_outer)
    };
    let poly = |t: Type| -> Type {
        type_util::poly_type_of_tparams(poly::Id::generate_id(), x.tparams.clone(), t)
    };
    (t_inner, poly(t_outer), inst_prop_ids)
}

pub fn mk_class_binding<'a, C: ConfigTypes>(
    _cx: &Context<'a>,
    x: &class_types::Class<C>,
) -> ClassBinding {
    ClassBinding {
        class_binding_id: x.id.dupe(),
    }
}

pub fn make_thises<'a, C: ConfigTypes>(
    cx: &Context<'a>,
    x: &class_types::Class<C>,
) -> (Type, Type, Type, Type) {
    let super_reason = x
        .instance
        .reason
        .dupe()
        .update_desc(|d| VirtualReasonDesc::RSuperOf(Arc::new(d)));
    match &x.super_ {
        class_types::Super::Interface(_) => {
            panic!("tried to evaluate toplevel of interface")
        }
        class_types::Super::Class(class_super) => {
            let (super_, static_super) = match &class_super.extends {
                class_types::Extends::Explicit((annot_loc, c, targs)) => {
                    // Eagerly specialize when there are no targs *)
                    // TODO: We can also specialize when there are targs, because this
                    // code is not instantiated. However, the type normalizer
                    // expects a PolyT here.
                    let c = if targs.is_none() {
                        let use_op = UseOp::Op(Arc::new(RootUseOp::ClassExtendsCheck {
                            def: type_util::reason_of_t(c).dupe(),
                            extends: x.instance.reason.dupe(),
                        }));
                        specialize(cx, use_op, targs.clone(), c.dupe())
                    } else {
                        c.dupe()
                    };
                    let t = type_util::this_typeapp(
                        c,
                        class_super.this_t.dupe(),
                        targs.clone(),
                        Some(annot_loc.dupe()),
                    );
                    (
                        t.dupe(),
                        type_util::class_type(t, false, Some(annot_loc.dupe())),
                    )
                }
                class_types::Extends::Implicit { null } => {
                    let t = if *null {
                        Type::new(TypeInner::NullProtoT(super_reason.dupe()))
                    } else {
                        Type::new(TypeInner::ObjProtoT(super_reason.dupe()))
                    };
                    let static_super = Type::new(TypeInner::FunProtoT(super_reason));
                    (t, static_super)
                }
            };
            (
                class_super.this_t.dupe(),
                type_util::class_type(class_super.this_t.dupe(), false, None),
                super_,
                static_super,
            )
        }
    }
}

/// Processes the bodies of instance and static class members.
pub fn toplevels<'a, C: crate::func_params_intf::Config>(
    cx: &Context<'a>,
    x: &class_types::Class<C>,
) -> Result<(), CheckExprError> {
    type_env::in_class_scope(cx, x.class_loc.dupe(), || {
        let method_ = |set_asts: &class_types::SetAsts<C>,
                       f: &func_class_sig_types::func::Func<C>| {
            let (params_ast, body_ast, init_ast) = func_sig::toplevels(cx, f)?;
            if let Some(ref init) = init_ast {
                let (_, t) = init.loc();
                cx.add_missing_local_annot_lower_bound(f.ret_annot_loc.dupe(), t.dupe());
            }
            (set_asts)((params_ast, body_ast, init_ast));
            Ok(())
        };
        let field =
            |_name: &str, entry: &class_types::FieldPrime<C>| -> Result<(), CheckExprError> {
                let (_, _, value) = entry;
                match value {
                    class_types::Field::Annot(_) => {}
                    class_types::Field::Infer(fsig, set_asts) => {
                        method_(set_asts, fsig)?;
                    }
                }
                Ok(())
            };
        with_sig(
            true,
            |s| -> Result<(), CheckExprError> {
                // process static methods and fields
                iter_methods(
                    &mut |fi: &class_types::FuncInfo<C>| method_(&fi.set_asts, &fi.func_sig),
                    s,
                )?;
                for (name, entry) in &s.fields {
                    field(name.as_ref(), entry)?;
                }
                for (name, entry) in &s.private_fields {
                    field(name.as_ref(), entry)?;
                }
                Ok(())
            },
            x,
        )?;
        with_sig(
            false,
            |s| -> Result<(), CheckExprError> {
                // process constructor
                for fi in &x.constructor {
                    method_(&fi.set_asts, &fi.func_sig)?;
                }
                // process instance methods and fields
                iter_methods(
                    &mut |fi: &class_types::FuncInfo<C>| method_(&fi.set_asts, &fi.func_sig),
                    s,
                )?;
                for (name, entry) in &s.fields {
                    field(name.as_ref(), entry)?;
                }
                for (name, entry) in &s.private_fields {
                    field(name.as_ref(), entry)?;
                }
                for (name, entry) in &s.proto_fields {
                    field(name.as_ref(), entry)?;
                }
                Ok(())
            },
            x,
        )?;
        Ok(())
    })
}
