/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::string_of_reason;
use flow_parser::loc_sig::LocSig;
use flow_typing_context::Context;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::ArrayATData;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::ComponentKind;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DestructorConditionalTypeData;
use flow_typing_type::type_::DestructorMappedTypeData;
use flow_typing_type::type_::DestructorSpreadTupleTypeData;
use flow_typing_type::type_::DestructorSpreadTypeData;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::DroType;
use flow_typing_type::type_::EnumInfoInner;
use flow_typing_type::type_::Flags;
use flow_typing_type::type_::FunType;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::InstType;
use flow_typing_type::type_::InstanceT;
use flow_typing_type::type_::MappedTypeHomomorphicFlag;
use flow_typing_type::type_::MappedTypeOptionality;
use flow_typing_type::type_::MappedTypeVariance;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::ObjType;
use flow_typing_type::type_::OptionalIndexedAccessIndex;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ReactEffectType;
use flow_typing_type::type_::RendersVariant;
use flow_typing_type::type_::StrUtilOp;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::ThisStatus;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::TupleElement;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeTKind;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::object;
use flow_typing_type::type_::properties;
use flow_typing_type::type_util;
use serde_json::Value as Json;
use serde_json::json;

// Helper function to create a JSON object with a "kind" field
fn json_with_type(type_name: &str, mut fields: Vec<(&str, Json)>) -> Json {
    let mut result = vec![("kind".to_string(), Json::String(type_name.to_string()))];
    for (k, v) in fields.drain(..) {
        result.push((k.to_string(), v));
    }
    Json::Object(result.into_iter().collect())
}

// Convert type_t_kind to JSON
fn json_of_type_t_kind(kind: &TypeTKind) -> Json {
    match kind {
        TypeTKind::TypeAliasKind => Json::String("TypeAliasKind".to_string()),
        TypeTKind::TypeParamKind => Json::String("TypeParamKind".to_string()),
        TypeTKind::OpaqueKind => Json::String("OpaqueKind".to_string()),
        TypeTKind::ImportTypeofKind => Json::String("ImportTypeofKind".to_string()),
        TypeTKind::ImportClassKind => Json::String("ImportClassKind".to_string()),
        TypeTKind::ImportEnumKind => Json::String("ImportEnumKind".to_string()),
        TypeTKind::InstanceKind => Json::String("InstanceKind".to_string()),
        TypeTKind::RenderTypeKind => Json::String("RenderTypeKind".to_string()),
    }
}

// Convert funtype to JSON
fn funtype_to_json<'cx>(cx: &Context<'cx>, depth: i32, funtype: &FunType) -> Json {
    let (this_t_type, this_status) = &funtype.this_t;
    let this_status_json = match this_status {
        ThisStatus::ThisMethod { unbound } => {
            json!({"kind": "This_Method", "unbound": unbound})
        }
        ThisStatus::ThisFunction => {
            json!({"kind": "This_Function"})
        }
    };

    let params_json = Json::Array(
        funtype
            .params
            .iter()
            .map(|param| {
                let name_json = match &param.0 {
                    None => Json::Null,
                    Some(name) => Json::String(name.to_string()),
                };
                json!({
                    "name": name_json,
                    "type": type_to_json(cx, depth - 1, &param.1),
                })
            })
            .collect(),
    );

    let rest_param_json = match &funtype.rest_param {
        None => Json::Null,
        Some(rest) => {
            let name_json = match &rest.0 {
                None => Json::Null,
                Some(name) => Json::String(name.to_string()),
            };
            json!({
                "name": name_json,
                "type": type_to_json(cx, depth - 1, &rest.2),
            })
        }
    };

    let type_guard_json = match &funtype.type_guard {
        None => Json::Null,
        Some(tg) => {
            let (_, name) = &tg.param_name;
            json!({
                "inferred": tg.inferred,
                "param_name": name.to_string(),
                "type_guard": type_to_json(cx, depth - 1, &tg.type_guard),
                "one_sided": tg.one_sided,
            })
        }
    };

    let effect_json = match &funtype.effect_ {
        ReactEffectType::HookDecl(id) => {
            json!({"kind": "HookDecl", "id": LocSig::debug_to_string(&id.0, false)})
        }
        ReactEffectType::HookAnnot => json!({"kind": "HookAnnot"}),
        ReactEffectType::ArbitraryEffect => json!({"kind": "ArbitraryEffect"}),
        ReactEffectType::AnyEffect => json!({"kind": "AnyEffect"}),
    };

    json!({
        "this_t": {
            "type": type_to_json(cx, depth - 1, this_t_type),
            "status": this_status_json,
        },
        "params": params_json,
        "rest_param": rest_param_json,
        "return_t": type_to_json(cx, depth - 1, &funtype.return_t),
        "type_guard": type_guard_json,
        "effect": effect_json,
    })
}

// Forward declarations for recursive types
pub fn type_to_json<'cx>(cx: &Context<'cx>, depth: i32, t: &Type) -> Json {
    if depth < 0 {
        return Json::String("<recursion>".to_string());
    }
    match t.deref() {
        TypeInner::OpenT(_) => {
            let reason = type_util::reason_of_t(t);
            let resolved = FlowJs::singleton_concrete_type_for_inspection(cx, reason, t)
                .unwrap_or_else(|_| t.dupe());
            type_to_json(cx, depth - 1, &resolved)
        }
        TypeInner::DefT(r, def_t) => json_with_type(
            "Def",
            vec![
                ("def", def_t_to_json(cx, depth, def_t)),
                (
                    "reason",
                    Json::String(string_of_reason(Some(cx.root().to_str().unwrap_or("")), r)),
                ),
            ],
        ),
        TypeInner::EvalT {
            type_,
            defer_use_t,
            id: _,
        } => {
            let destructor_json = {
                let destructor = &defer_use_t.2;
                json_of_destructor(cx, depth, destructor)
            };
            json_with_type(
                "Eval",
                vec![
                    ("type", type_to_json(cx, depth - 1, type_)),
                    ("destructor", destructor_json),
                ],
            )
        }
        TypeInner::GenericT(box GenericTData {
            reason: _,
            name,
            bound,
            no_infer,
            id: _,
        }) => json_with_type(
            "Generic",
            vec![
                (
                    "name",
                    Json::String(name.string_of_subst_name().to_string()),
                ),
                ("bound", type_to_json(cx, depth - 1, bound)),
                ("no_infer", Json::Bool(*no_infer)),
            ],
        ),
        TypeInner::ThisInstanceT(box ThisInstanceTData {
            reason: _,
            instance,
            is_this,
            subst_name,
        }) => json_with_type(
            "ThisInstance",
            vec![
                ("instance", json_of_instance_t(cx, depth, instance)),
                ("is_this", Json::Bool(*is_this)),
                (
                    "name",
                    Json::String(subst_name.string_of_subst_name().to_string()),
                ),
            ],
        ),
        TypeInner::ThisTypeAppT(box ThisTypeAppTData {
            reason: _,
            this_t,
            type_,
            targs,
        }) => {
            let mut fields = vec![
                ("t1", type_to_json(cx, depth - 1, this_t)),
                ("t2", type_to_json(cx, depth - 1, type_)),
            ];
            if let Some(t_list) = targs {
                fields.push((
                    "t_list",
                    Json::Array(
                        t_list
                            .iter()
                            .map(|t| type_to_json(cx, depth - 1, t))
                            .collect(),
                    ),
                ));
            }
            json_with_type("ThisTypeApp", fields)
        }
        TypeInner::TypeAppT(box TypeAppTData {
            reason: _,
            use_op: _,
            type_,
            targs,
            from_value,
            use_desc,
        }) => json_with_type(
            "TypeApp",
            vec![
                ("type", type_to_json(cx, depth - 1, type_)),
                (
                    "targs",
                    Json::Array(
                        targs
                            .iter()
                            .map(|t| type_to_json(cx, depth - 1, t))
                            .collect(),
                    ),
                ),
                ("from_value", Json::Bool(*from_value)),
                ("use_desc", Json::Bool(*use_desc)),
            ],
        ),
        TypeInner::FunProtoT(_) => json_with_type("FunProto", vec![]),
        TypeInner::ObjProtoT(_) => json_with_type("ObjProto", vec![]),
        TypeInner::NullProtoT(_) => json_with_type("NullProto", vec![]),
        TypeInner::FunProtoBindT(_) => json_with_type("FunProtoBind", vec![]),
        TypeInner::IntersectionT(_, inter_rep) => {
            let members: Vec<Json> = inter_rep
                .members_iter()
                .map(|t| type_to_json(cx, depth - 1, t))
                .collect();
            json_with_type("Intersection", vec![("members", Json::Array(members))])
        }
        TypeInner::UnionT(_, union_rep) => {
            let members: Vec<Json> = union_rep
                .members_iter()
                .map(|t| type_to_json(cx, depth - 1, t))
                .collect();
            json_with_type("Union", vec![("members", Json::Array(members))])
        }
        TypeInner::MaybeT(_, t) => {
            json_with_type("Maybe", vec![("type", type_to_json(cx, depth - 1, t))])
        }
        TypeInner::OptionalT {
            reason: _,
            type_,
            use_desc,
        } => json_with_type(
            "Optional",
            vec![
                ("type", type_to_json(cx, depth - 1, type_)),
                ("use_desc", Json::Bool(*use_desc)),
            ],
        ),
        TypeInner::KeysT(_, t) => {
            json_with_type("Keys", vec![("type", type_to_json(cx, depth - 1, t))])
        }
        TypeInner::AnnotT(_, t, use_desc) => json_with_type(
            "Annot",
            vec![
                ("type", type_to_json(cx, depth - 1, t)),
                ("use_desc", Json::Bool(*use_desc)),
            ],
        ),
        TypeInner::NominalT {
            reason: _,
            nominal_type,
        } => {
            let underlying_t_json = match &nominal_type.underlying_t {
                nominal::UnderlyingT::FullyOpaque => Json::Null,
                nominal::UnderlyingT::CustomError(box nominal::CustomErrorData { t, .. })
                | nominal::UnderlyingT::OpaqueWithLocal { t, .. } => type_to_json(cx, depth - 1, t),
            };
            let super_t_json = match &nominal_type.upper_t {
                None => Json::Null,
                Some(t) => type_to_json(cx, depth - 1, t),
            };
            let sub_t_json = match &nominal_type.lower_t {
                None => Json::Null,
                Some(t) => type_to_json(cx, depth - 1, t),
            };
            let nominal_type_args_json = Json::Array(
                nominal_type
                    .nominal_type_args
                    .iter()
                    .map(|(name, _, t, polarity)| {
                        json!({
                            "name": name.string_of_subst_name().to_string(),
                            "type": type_to_json(cx, depth - 1, t),
                            "polarity": polarity_to_string(polarity),
                        })
                    })
                    .collect(),
            );
            json_with_type(
                "Opaque",
                vec![(
                    "nominal_type",
                    json!({
                        "nominal_id": nominal_type.nominal_id.to_string(),
                        "underlying_t": underlying_t_json,
                        "super_t": super_t_json,
                        "sub_t": sub_t_json,
                        "nominal_type_args": nominal_type_args_json,
                    }),
                )],
            )
        }
        TypeInner::NamespaceT(namespace_t) => json_with_type(
            "Namespace",
            vec![
                (
                    "namespace_symbol",
                    json!({"symbol": namespace_t.namespace_symbol.name().to_string()}),
                ),
                (
                    "values_type",
                    type_to_json(cx, depth - 1, &namespace_t.values_type),
                ),
                (
                    "types_tmap",
                    json_of_property_map(cx, depth, &namespace_t.types_tmap),
                ),
            ],
        ),
        TypeInner::AnyT(_, _) => json_with_type("Any", vec![]),
        TypeInner::StrUtilT {
            reason: _,
            op,
            remainder,
        } => {
            let mut fields: Vec<(&str, Json)> = match op {
                StrUtilOp::StrPrefix(s) => vec![
                    ("op", Json::String("StrPrefix".to_string())),
                    ("prefix", Json::String(s.to_string())),
                ],
                StrUtilOp::StrSuffix(s) => vec![
                    ("op", Json::String("StrSuffix".to_string())),
                    ("suffix", Json::String(s.to_string())),
                ],
            };
            if let Some(t) = remainder {
                fields.push(("remainder", type_to_json(cx, depth - 1, t)));
            }
            json_with_type("StrUtil", fields)
        }
    }
}

// Convert typeparams to JSON
fn json_of_typeparams<'cx>(
    cx: &Context<'cx>,
    depth: i32,
    tparams: &[flow_typing_type::type_::TypeParam],
) -> Json {
    Json::Array(
        tparams
            .iter()
            .map(|typeparam| {
                let default_json = match &typeparam.default {
                    None => Json::Null,
                    Some(t) => type_to_json(cx, depth - 1, t),
                };
                json!({
                    "name": typeparam.name.string_of_subst_name().to_string(),
                    "bound": type_to_json(cx, depth - 1, &typeparam.bound),
                    "polarity": polarity_to_string(&typeparam.polarity),
                    "default": default_json,
                })
            })
            .collect(),
    )
}

// Convert enum_info to JSON
fn json_of_enum_info<'cx>(cx: &Context<'cx>, depth: i32, enum_info: &EnumInfoInner) -> Json {
    match enum_info {
        EnumInfoInner::ConcreteEnum(enum_concrete_info) => {
            let members_json = Json::Array(
                enum_concrete_info
                    .members
                    .keys()
                    .map(|k| Json::String(k.to_string()))
                    .collect(),
            );
            json!({
                "kind": "ConcreteEnum",
                "enum_name": enum_concrete_info.enum_name.to_string(),
                "enum_id": LocSig::debug_to_string(&enum_concrete_info.enum_id.0, true),
                "members": members_json,
                "representation_t": type_to_json(cx, depth - 1, &enum_concrete_info.representation_t),
                "has_unknown_members": enum_concrete_info.has_unknown_members,
            })
        }
        EnumInfoInner::AbstractEnum { representation_t } => {
            json!({
                "kind": "AbstractEnum",
                "representation_t": type_to_json(cx, depth - 1, representation_t),
            })
        }
    }
}

// Convert canonical_renders_form to JSON
fn json_of_canonical_renders_form<'cx>(
    cx: &Context<'cx>,
    depth: i32,
    renders: &CanonicalRendersForm,
) -> Json {
    match renders {
        CanonicalRendersForm::IntrinsicRenders(name) => {
            json!({"kind": "IntrinsicRenders", "name": name.to_string()})
        }
        CanonicalRendersForm::NominalRenders {
            renders_id,
            renders_name,
            renders_super,
        } => {
            json!({
                "kind": "NominalRenders",
                "renders_id": LocSig::debug_to_string(&renders_id.0, true),
                "renders_name": renders_name.to_string(),
                "renders_super": type_to_json(cx, depth - 1, renders_super),
            })
        }
        CanonicalRendersForm::StructuralRenders {
            renders_variant,
            renders_structural_type,
        } => {
            let variant_str = match renders_variant {
                RendersVariant::RendersNormal => "RendersNormal",
                RendersVariant::RendersMaybe => "RendersMaybe",
                RendersVariant::RendersStar => "RendersStar",
            };
            json!({
                "kind": "StructuralRenders",
                "renders_variant": variant_str,
                "renders_structural_type": type_to_json(cx, depth - 1, renders_structural_type),
            })
        }
        CanonicalRendersForm::DefaultRenders => {
            json!({"kind": "DefaultRenders"})
        }
    }
}

// Convert instance_t to JSON
fn json_of_instance_t<'cx>(cx: &Context<'cx>, depth: i32, instance_t: &InstanceT) -> Json {
    json!({
        "inst": json_of_insttype(cx, depth, &instance_t.inst),
        "static": type_to_json(cx, depth - 1, &instance_t.static_),
        "super": type_to_json(cx, depth - 1, &instance_t.super_),
        "implements": Json::Array(
            instance_t.implements.iter().map(|t| type_to_json(cx, depth - 1, t)).collect()
        ),
    })
}

fn json_of_insttype<'cx>(cx: &Context<'cx>, depth: i32, inst: &InstType) -> Json {
    let class_name_json = match &inst.class_name {
        None => Json::Null,
        Some(name) => Json::String(name.to_string()),
    };
    let type_args_json = Json::Array(
        inst.type_args
            .iter()
            .map(|(name, _, t, polarity)| {
                json!({
                    "name": name.string_of_subst_name().to_string(),
                    "type": type_to_json(cx, depth - 1, t),
                    "polarity": polarity_to_string(polarity),
                })
            })
            .collect(),
    );
    let call_t_json = match inst.inst_call_t {
        None => Json::Null,
        Some(id) => {
            let call = cx.find_call(id);
            json!({
                "id": id,
                "call": type_to_json(cx, depth - 1, &call),
            })
        }
    };
    json!({
        "class_name": class_name_json,
        "class_id": LocSig::debug_to_string(&inst.class_id.0, true),
        "type_args": type_args_json,
        "own_props": json_of_property_map(cx, depth, &inst.own_props),
        "proto_props": json_of_property_map(cx, depth, &inst.proto_props),
        "call_t": call_t_json,
    })
}

// Convert def_t to JSON
fn def_t_to_json<'cx>(cx: &Context<'cx>, depth: i32, def_t: &DefT) -> Json {
    match def_t.deref() {
        DefTInner::NumGeneralT(_) => json_with_type("NumGeneral", vec![]),
        DefTInner::StrGeneralT(_) => json_with_type("StrGeneral", vec![]),
        DefTInner::BoolGeneralT => json_with_type("BoolGeneral", vec![]),
        DefTInner::BigIntGeneralT(_) => json_with_type("BigIntGeneral", vec![]),
        DefTInner::EmptyT => json_with_type("Empty", vec![]),
        DefTInner::MixedT(_) => json_with_type("Mixed", vec![]),
        DefTInner::NullT => json_with_type("Null", vec![]),
        DefTInner::VoidT => json_with_type("Void", vec![]),
        DefTInner::SymbolT => json_with_type("Symbol", vec![]),
        DefTInner::UniqueSymbolT(_) => json_with_type("UniqueSymbol", vec![]),
        DefTInner::FunT(static_, funtype) => json_with_type(
            "Fun",
            vec![
                ("static", type_to_json(cx, depth - 1, static_)),
                ("funtype", funtype_to_json(cx, depth, funtype)),
            ],
        ),
        DefTInner::ObjT(objtype) => json_with_type(
            "Obj",
            vec![("objtype", objtype_to_json(cx, depth, objtype))],
        ),
        DefTInner::ArrT(arrtype) => json_with_type(
            "Arr",
            vec![("arrtype", arrtype_to_json(cx, depth, arrtype))],
        ),
        DefTInner::ClassT(t) => {
            json_with_type("Class", vec![("type", type_to_json(cx, depth - 1, t))])
        }
        DefTInner::InstanceT(instance_t) => json_with_type(
            "Instance",
            vec![("instance", json_of_instance_t(cx, depth, instance_t))],
        ),
        DefTInner::SingletonStrT { from_annot, value } => json_with_type(
            "SingletonStr",
            vec![
                ("from_annot", Json::Bool(*from_annot)),
                ("value", Json::String(value.as_str().to_string())),
            ],
        ),
        DefTInner::NumericStrKeyT(num_lit) => json_with_type(
            "NumericStrKey",
            vec![
                (
                    "number",
                    serde_json::Number::from_f64(num_lit.0)
                        .map(Json::Number)
                        .unwrap_or(Json::Null),
                ),
                ("string", Json::String(num_lit.1.to_string())),
            ],
        ),
        DefTInner::SingletonNumT { from_annot, value } => {
            let s = &value.1;
            json_with_type(
                "SingletonNum",
                vec![
                    ("from_annot", Json::Bool(*from_annot)),
                    (
                        "number",
                        serde_json::Number::from_f64(value.0)
                            .map(Json::Number)
                            .unwrap_or(Json::Null),
                    ),
                    ("string", Json::String(s.to_string())),
                ],
            )
        }
        DefTInner::SingletonBoolT { from_annot, value } => json_with_type(
            "SingletonBool",
            vec![
                ("from_annot", Json::Bool(*from_annot)),
                ("value", Json::Bool(*value)),
            ],
        ),
        DefTInner::SingletonBigIntT { from_annot, value } => {
            let s = &value.1;
            json_with_type(
                "SingletonBigInt",
                vec![
                    ("from_annot", Json::Bool(*from_annot)),
                    ("value", Json::String(s.to_string())),
                ],
            )
        }
        DefTInner::TypeT(kind, t) => json_with_type(
            "Type",
            vec![
                ("type_kind", json_of_type_t_kind(kind)),
                ("type", type_to_json(cx, depth - 1, t)),
            ],
        ),
        DefTInner::PolyT(box PolyTData {
            tparams_loc: _,
            tparams,
            t_out,
            id,
        }) => json_with_type(
            "Poly",
            vec![
                ("tparams", json_of_typeparams(cx, depth, tparams)),
                ("t_out", type_to_json(cx, depth - 1, t_out)),
                ("id", Json::String(id.stable_string())),
            ],
        ),
        DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
            config,
            renders,
            component_kind,
        }) => {
            let component_kind_json = match component_kind {
                ComponentKind::Structural => json!({"kind": "Structural"}),
                ComponentKind::Nominal(id, name, types_opt) => {
                    let types_json = match types_opt {
                        None => Json::Null,
                        Some(types) => Json::Array(
                            types
                                .iter()
                                .map(|t| type_to_json(cx, depth - 1, t))
                                .collect(),
                        ),
                    };
                    json!({
                        "kind": "Nominal",
                        "id": LocSig::debug_to_string(&id.0, true),
                        "name": name.to_string(),
                        "types": types_json,
                    })
                }
            };
            json_with_type(
                "ReactAbstractComponent",
                vec![
                    ("config", type_to_json(cx, depth - 1, config)),
                    ("renders", type_to_json(cx, depth - 1, renders)),
                    ("component_kind", component_kind_json),
                ],
            )
        }
        DefTInner::RendersT(form) => json_with_type(
            "Renders",
            vec![("form", json_of_canonical_renders_form(cx, depth, form))],
        ),
        DefTInner::EnumValueT(enum_info) => json_with_type(
            "EnumValue",
            vec![("enum_info", json_of_enum_info(cx, depth, enum_info))],
        ),
        DefTInner::EnumObjectT {
            enum_value_t,
            enum_info,
        } => json_with_type(
            "EnumObject",
            vec![
                ("enum_value_t", type_to_json(cx, depth - 1, enum_value_t)),
                ("enum_info", json_of_enum_info(cx, depth, enum_info)),
            ],
        ),
    }
}

// Convert obj_kind to JSON
fn json_of_obj_kind<'cx>(cx: &Context<'cx>, depth: i32, obj_kind: &ObjKind) -> Json {
    match obj_kind {
        ObjKind::Exact => json!({"kind": "Exact"}),
        ObjKind::Inexact => json!({"kind": "Inexact"}),
        ObjKind::Indexed(dicttype) => {
            json!({"kind": "Indexed", "dicttype": json_of_dicttype(cx, depth, dicttype)})
        }
    }
}

// Convert dicttype to JSON
fn json_of_dicttype<'cx>(cx: &Context<'cx>, depth: i32, dicttype: &DictType) -> Json {
    let dict_name_json = match &dicttype.dict_name {
        None => Json::Null,
        Some(name) => Json::String(name.to_string()),
    };
    json!({
        "dict_name": dict_name_json,
        "key": type_to_json(cx, depth - 1, &dicttype.key),
        "value": type_to_json(cx, depth - 1, &dicttype.value),
        "dict_polarity": polarity_to_string(&dicttype.dict_polarity),
    })
}

// Convert arrtype to JSON
fn arrtype_to_json<'cx>(cx: &Context<'cx>, depth: i32, arrtype: &ArrType) -> Json {
    match arrtype {
        ArrType::ArrayAT(box ArrayATData {
            react_dro: _,
            elem_t,
            tuple_view: _,
        }) => {
            json!({"kind": "ArrayAT", "elem_t": type_to_json(cx, depth - 1, elem_t)})
        }
        ArrType::TupleAT(box TupleATData {
            react_dro: _,
            elem_t,
            elements,
            arity: (min_arity, max_arity),
            inexact,
        }) => {
            json!({
                "kind": "TupleAT",
                "elem_t": type_to_json(cx, depth - 1, elem_t),
                "elements": Json::Array(elements.iter().map(|e| json_of_tuple_element(cx, depth, e)).collect()),
                "min_arity": min_arity,
                "max_arity": max_arity,
                "inexact": inexact,
            })
        }
        ArrType::ROArrayAT(box (elem_t, _)) => {
            json!({"kind": "ROArrayAT", "elem_t": type_to_json(cx, depth - 1, elem_t)})
        }
    }
}

// Convert tuple_element to JSON
fn json_of_tuple_element<'cx>(cx: &Context<'cx>, depth: i32, element: &TupleElement) -> Json {
    let TupleElement {
        reason: _,
        name,
        t,
        polarity,
        optional,
    } = element;
    let name_json = match name {
        None => Json::Null,
        Some(name) => Json::String(name.to_string()),
    };
    json!({
        "name": name_json,
        "t": type_to_json(cx, depth - 1, t),
        "polarity": polarity_to_string(polarity),
        "optional": optional,
    })
}

// Convert flags to JSON
fn json_of_flags<'cx>(cx: &Context<'cx>, depth: i32, flags: &Flags) -> Json {
    json!({"obj_kind": json_of_obj_kind(cx, depth, &flags.obj_kind)})
}

// Convert property to JSON
fn json_of_property<'cx>(cx: &Context<'cx>, depth: i32, property: &Property) -> Json {
    match &**property {
        PropertyInner::Field(fd) => {
            json!({
                "kind": "Field",
                "type": type_to_json(cx, depth - 1, &fd.type_),
                "polarity": polarity_to_string(&fd.polarity),
            })
        }
        PropertyInner::Get { type_, .. } => {
            json!({"kind": "Get", "type": type_to_json(cx, depth - 1, type_)})
        }
        PropertyInner::Set {
            key_loc: _, type_, ..
        } => {
            json!({"kind": "Set", "type": type_to_json(cx, depth - 1, type_)})
        }
        PropertyInner::GetSet(gs) => {
            json!({
                "kind": "GetSet",
                "get_type": type_to_json(cx, depth - 1, &gs.get_type),
                "set_type": type_to_json(cx, depth - 1, &gs.set_type),
            })
        }
        PropertyInner::Method { type_, .. } => {
            json!({"kind": "Method", "type": type_to_json(cx, depth - 1, type_)})
        }
    }
}

// Convert property map to JSON
fn json_of_property_map<'cx>(cx: &Context<'cx>, depth: i32, props_id: &properties::Id) -> Json {
    match cx.find_props_opt(props_id.clone()) {
        Some(props) => json_of_property_map_value(cx, depth, &props),
        None => json!({"error": "Property map not found"}),
    }
}

fn json_of_property_map_value<'cx>(
    cx: &Context<'cx>,
    depth: i32,
    props: &properties::PropertiesMap,
) -> Json {
    let props_json: serde_json::Map<String, Json> = props
        .iter()
        .map(|(name, prop)| (name.as_str().to_string(), json_of_property(cx, depth, prop)))
        .collect();
    Json::Object(props_json)
}

// Convert objtype to JSON
fn objtype_to_json<'cx>(cx: &Context<'cx>, depth: i32, objtype: &ObjType) -> Json {
    let call_t_json = match objtype.call_t {
        None => Json::Null,
        Some(id) => json!(id),
    };
    json!({
        "flags": json_of_flags(cx, depth, &objtype.flags),
        "props": json_of_property_map(cx, depth, &objtype.props_tmap),
        "proto_t": type_to_json(cx, depth - 1, &objtype.proto_t),
        "call_t": call_t_json,
    })
}

fn json_of_destructor<'cx>(cx: &Context<'cx>, depth: i32, destructor: &Destructor) -> Json {
    match destructor {
        Destructor::NonMaybeType => json!({"kind": "NonMaybeType"}),
        Destructor::PropertyType { name } => {
            json!({"kind": "PropertyType", "name": name.as_str()})
        }
        Destructor::ElementType { index_type } => {
            json!({"kind": "ElementType", "index_type": type_to_json(cx, depth - 1, index_type)})
        }
        Destructor::OptionalIndexedAccessNonMaybeType { index } => {
            let index_json = match index {
                OptionalIndexedAccessIndex::OptionalIndexedAccessStrLitIndex(name) => {
                    json!({"kind": "StrLitIndex", "name": name.as_str()})
                }
                OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(t) => {
                    json!({"kind": "TypeIndex", "type": type_to_json(cx, depth - 1, t)})
                }
            };
            json!({"kind": "OptionalIndexedAccessNonMaybeType", "index": index_json})
        }
        Destructor::OptionalIndexedAccessResultType { void_reason: _ } => {
            json!({"kind": "OptionalIndexedAccessResultType"})
        }
        Destructor::ExactType => json!({"kind": "ExactType"}),
        Destructor::ReadOnlyType => json!({"kind": "ReadOnlyType"}),
        Destructor::PartialType => json!({"kind": "PartialType"}),
        Destructor::RequiredType => json!({"kind": "RequiredType"}),
        Destructor::SpreadType(box DestructorSpreadTypeData(
            target,
            operands,
            operand_slice_opt,
        )) => {
            let target_json = match target {
                object::spread::Target::Value { make_seal } => {
                    let seal_str = match make_seal {
                        object::spread::SealType::Sealed => "Sealed",
                        object::spread::SealType::Frozen => "Frozen",
                        object::spread::SealType::AsConst => "As_Const",
                    };
                    json!({"kind": "Value", "make_seal": seal_str})
                }
                object::spread::Target::Annot { make_exact } => {
                    json!({"kind": "Annot", "make_exact": make_exact})
                }
            };
            let operands_json = Json::Array(
                operands
                    .iter()
                    .map(|operand| match operand {
                        object::spread::Operand::Slice(slice) => json_of_slice(cx, depth, slice),
                        object::spread::Operand::Type(t) => {
                            json!({"kind": "Type", "type": type_to_json(cx, depth - 1, t)})
                        }
                    })
                    .collect(),
            );
            let operand_slice_json = match operand_slice_opt {
                None => Json::Null,
                Some(slice) => json_of_slice(cx, depth, slice),
            };
            json!({
                "kind": "SpreadType",
                "target": target_json,
                "operands": operands_json,
                "operand_slice": operand_slice_json,
            })
        }
        Destructor::SpreadTupleType(box DestructorSpreadTupleTypeData {
            reason_tuple: _,
            reason_spread: _,
            inexact,
            resolved: resolved_rev,
            unresolved,
        }) => {
            json!({
                "kind": "SpreadTupleType",
                "inexact": inexact,
                "resolved_rev": resolved_rev.len().to_string(),
                "unresolved": unresolved.len().to_string(),
            })
        }
        Destructor::RestType(merge_mode, t) => {
            let merge_mode_json = match merge_mode {
                object::rest::MergeMode::SpreadReversal => json_with_type("SpreadReversal", vec![]),
                object::rest::MergeMode::ReactConfigMerge(polarity) => {
                    json!({
                        "kind": "ReactConfigMerge",
                        "polarity": polarity_to_string(polarity),
                    })
                }
                object::rest::MergeMode::Omit => json_with_type("Omit", vec![]),
            };
            json!({
                "kind": "RestType",
                "merge_mode": merge_mode_json,
                "type": type_to_json(cx, depth - 1, t),
            })
        }
        Destructor::ValuesType => json!({"kind": "ValuesType"}),
        Destructor::ConditionalType(box DestructorConditionalTypeData {
            distributive_tparam_name,
            infer_tparams,
            extends_t,
            true_t,
            false_t,
        }) => {
            let dist_name_json = match distributive_tparam_name {
                None => Json::Null,
                Some(name) => Json::String(name.string_of_subst_name().to_string()),
            };
            json!({
                "kind": "ConditionalType",
                "distributive_tparam_name": dist_name_json,
                "infer_tparams": infer_tparams.len().to_string(),
                "extends_t": type_to_json(cx, depth - 1, extends_t),
                "true_t": type_to_json(cx, depth - 1, true_t),
                "false_t": type_to_json(cx, depth - 1, false_t),
            })
        }
        Destructor::TypeMap(flow_typing_type::type_::TypeMap::ObjectKeyMirror) => {
            json!({"kind": "ObjectKeyMirror"})
        }
        Destructor::ReactElementConfigType => json!({"kind": "ReactElementConfigType"}),
        Destructor::ReactCheckComponentConfig {
            props,
            allow_ref_in_spread: _,
        } => {
            let props_json: serde_json::Map<String, Json> = props
                .iter()
                .map(|(name, prop)| (name.as_str().to_string(), json_of_property(cx, depth, prop)))
                .collect();
            json!({
                "kind": "ReactCheckComponentConfig",
                "props": Json::Object(props_json),
            })
        }
        Destructor::ReactDRO(react_dro) => {
            let dro_type_str = match react_dro.1 {
                DroType::HookReturn => "HookReturn",
                DroType::HookArg => "HookArg",
                DroType::Props => "Props",
                DroType::DebugAnnot => "DebugAnnot",
            };
            json!({"kind": "ReactDRO", "dro_type": dro_type_str})
        }
        Destructor::MappedType(box DestructorMappedTypeData {
            homomorphic,
            distributive_tparam_name,
            property_type,
            mapped_type_flags,
        }) => {
            let homomorphic_json = match homomorphic {
                MappedTypeHomomorphicFlag::Homomorphic => json_with_type("Homomorphic", vec![]),
                MappedTypeHomomorphicFlag::Unspecialized => json_with_type("Unspecialized", vec![]),
                MappedTypeHomomorphicFlag::SemiHomomorphic(t) => {
                    json!({"kind": "SemiHomomorphic", "type": type_to_json(cx, depth - 1, t)})
                }
            };
            let dist_name_json = match distributive_tparam_name {
                None => Json::Null,
                Some(name) => Json::String(name.string_of_subst_name().to_string()),
            };
            let optional_str = match mapped_type_flags.optional {
                MappedTypeOptionality::MakeOptional => "MakeOptional",
                MappedTypeOptionality::RemoveOptional => "RemoveOptional",
                MappedTypeOptionality::KeepOptionality => "KeepOptionality",
            };
            json!({
                "kind": "MappedType",
                "homomorphic": homomorphic_json,
                "distributive_tparam_name": dist_name_json,
                "property_type": type_to_json(cx, depth - 1, property_type),
                "mapped_type_flags": {
                    "variance": match mapped_type_flags.variance {
                        MappedTypeVariance::OverrideVariance(Polarity::Positive) => "override_positive",
                        MappedTypeVariance::OverrideVariance(Polarity::Negative) => "override_negative",
                        MappedTypeVariance::OverrideVariance(Polarity::Neutral) => {
                            panic!("OverrideVariance Neutral is not reachable")
                        }
                        MappedTypeVariance::RemoveVariance(Polarity::Positive) => "remove_positive",
                        MappedTypeVariance::RemoveVariance(Polarity::Negative) => {
                            panic!("RemoveVariance Negative is not reachable")
                        }
                        MappedTypeVariance::RemoveVariance(Polarity::Neutral) => {
                            panic!("RemoveVariance Neutral is not reachable")
                        }
                        MappedTypeVariance::KeepVariance => "keep",
                    },
                    "optional": optional_str,
                },
            })
        }
        Destructor::EnumType => json!({"kind": "EnumType"}),
    }
}

fn json_of_slice<'cx>(cx: &Context<'cx>, depth: i32, slice: &object::spread::OperandSlice) -> Json {
    let dict_json = match &slice.dict {
        None => Json::Null,
        Some(dict) => json_of_dicttype(cx, depth, dict),
    };
    let reachable_targs_json = Json::Array(
        slice
            .reachable_targs
            .iter()
            .map(|(t, polarity)| {
                json!({
                    "type": type_to_json(cx, depth - 1, t),
                    "polarity": polarity_to_string(polarity),
                })
            })
            .collect(),
    );
    // TODO: prop_map needs more investigation
    //   ( "generics",
    //     JSON_Array
    //       (Base.List.map
    //          ~f:(fun gen ->
    //            JSON_String (Subst_name.string_of_subst_name gen.Generic.generic.Generic.name))
    //          slice.Type.Object.Spread.generics
    //       )
    //   );
    let generics_json = Json::Array(
        slice
            .generics
            .iter()
            .map(|g| Json::String(g.generic.name.string_of_subst_name().to_string()))
            .collect(),
    );
    json_with_type(
        "Slice",
        vec![
            (
                "prop_map",
                json_of_property_map_from_btreemap(cx, depth, &slice.prop_map),
            ),
            ("generics", generics_json),
            ("dict", dict_json),
            ("reachable_targs", reachable_targs_json),
        ],
    )
}

fn json_of_property_map_from_btreemap<'cx>(
    cx: &Context<'cx>,
    depth: i32,
    props: &flow_data_structure_wrapper::ord_map::FlowOrdMap<Name, Property>,
) -> Json {
    let props_json: serde_json::Map<String, Json> = props
        .iter()
        .map(|(name, prop)| (name.as_str().to_string(), json_of_property(cx, depth, prop)))
        .collect();
    Json::Object(props_json)
}

fn polarity_to_string(polarity: &Polarity) -> Json {
    Json::String(
        match polarity {
            Polarity::Positive => "positive",
            Polarity::Negative => "negative",
            Polarity::Neutral => "neutral",
        }
        .to_string(),
    )
}
