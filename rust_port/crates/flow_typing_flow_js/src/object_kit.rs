/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::collections::BTreeMap;
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
use flow_common::subst_name;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::obj_type;
use flow_typing_type::type_::CondTData;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::FieldData;
use flow_typing_type::type_::Flags;
use flow_typing_type::type_::MappedTypeFlags;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UnifyCause;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::mixed_t;
use flow_typing_type::type_::num_module_t;
use flow_typing_type::type_::object;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::str_module_t;
use flow_typing_type::type_::symbol_t;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util;
use vec1::Vec1;

use crate::flow_js::FlowJs;
use crate::slice_utils;

// We use this function to transform a resolved UnionT into a ((name * reason) list * Type.t),
// which represents a tuple of key names (with reasons for error messages) and an indexer to be
// used to create a new object. All SinlgetonStrTs/StrTs are turned into keys and all other
// non-empty types are turned into indexers. This is how we support non-homomorphic mapped types,
// like {[key in 'a' | 'b' | number]: string} and also how we support mapped types that use
// tparams with $Keys/keyof upper bounds:
// type Pick<O: {...}, Keys: $Keys<O>> = {[key in Keys]: O[key]}
//
// Note that we use the ConcretizeT constraint to flatten the union. A non-homomorphic mapped
// type *must not* operate over Unresolved tvar because additional keys received in the future
// will cause us to incorrectly output a union. While this approach of eagerly resolving the type
// may lead to edge cases where we miss keys, it seems like the best way forward. To get rid of
// these bugs in 100% of cases we'd need to make our EvalT machinery only operate over resolved
// types.
fn partition_keys_and_indexer<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason: &Reason,
    keys: &Type,
) -> Result<(Vec<(Name, Reason)>, Vec<Type>), FlowJsException> {
    let key_upper_bound_reason = |desc| flow_common::reason::mk_reason(desc, reason.loc().dupe());
    let str_t = str_module_t::make(key_upper_bound_reason(VirtualReasonDesc::RString));
    let num_t = num_module_t::make(key_upper_bound_reason(VirtualReasonDesc::RNumber));
    let symbol_t = symbol_t::make(key_upper_bound_reason(VirtualReasonDesc::RSymbol));
    let union = Type::new(TypeInner::UnionT(
        reason.dupe(),
        union_rep::make(
            None,
            union_rep::UnionKind::UnknownKind,
            str_t,
            num_t,
            vec![symbol_t].into(),
        ),
    ));
    let compatibility_use_op = VirtualUseOp::Frame(
        Arc::new(VirtualFrameUseOp::MappedTypeKeyCompatibility {
            source_type: type_util::reason_of_t(keys).dupe(),
            mapped_type: reason.dupe(),
        }),
        Arc::new(use_op.dupe()),
    );
    // All keys must be a subtype of string | number | symbol
    FlowJs::rec_flow_t(cx, trace, compatibility_use_op, keys, &union)?;
    let possible_types = FlowJs::possible_concrete_types_for_inspection(cx, reason, keys)?;
    let mut keys_result: Vec<(Name, Reason)> = Vec::new();
    let mut indexers: Vec<Type> = Vec::new();
    for t in possible_types {
        match t.deref() {
            TypeInner::DefT(r, def_t)
                if let DefTInner::SingletonStrT { value: name, .. } = def_t.deref() =>
            {
                keys_result.push((name.dupe(), r.dupe()));
                continue;
            }
            TypeInner::DefT(_, def_t) if let DefTInner::EmptyT = def_t.deref() => continue,
            _ => {}
        }

        indexers.push(t.dupe());
    }
    Ok((keys_result, indexers))
}

pub fn mapped_type_of_keys<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason: &Reason,
    keys: &Type,
    property_type: &Type,
    mapped_type_flags: &MappedTypeFlags,
) -> Result<Type, FlowJsException> {
    let (keys_with_reasons, indexers) =
        partition_keys_and_indexer(cx, trace, use_op.dupe(), reason, keys)?;
    // To go from a union to a MappedType we first build an object with all the keys we
    // extract from the union and create an object with all mixed values. Then we push it
    // through the mapped type machinery. The specific type we choose for the properties
    // does not matter because the mapped type code does not inspect the value types
    let mixed = mixed_t::why(reason.dupe());
    let mixed_prop_t = |key_reason: &Reason| -> object::Prop {
        let key_loc = Some(key_reason.loc().dupe());
        object::Prop {
            prop_t: mixed.dupe(),
            is_own: true,
            is_method: false,
            polarity: Polarity::Neutral,
            key_loc,
        }
    };
    let props: object::Props = keys_with_reasons
        .iter()
        .map(|(key, key_reason)| (key.dupe(), mixed_prop_t(key_reason)))
        .collect();
    let generics = flow_typing_generics::spread_empty();
    let obj_kind = match indexers.as_slice() {
        [] => ObjKind::Exact,
        [t1, ts @ ..] => {
            let key_t = match ts {
                [] => t1.dupe(),
                [t2, rest @ ..] => Type::new(TypeInner::UnionT(
                    reason.dupe(),
                    union_rep::make(
                        None,
                        union_rep::UnionKind::UnknownKind,
                        t1.dupe(),
                        t2.dupe(),
                        rest.iter().duped().collect::<Rc<[_]>>(),
                    ),
                )),
            };
            ObjKind::Indexed(DictType {
                dict_name: None,
                key: key_t,
                value: mixed.dupe(),
                dict_polarity: Polarity::Neutral,
            })
        }
    };
    let flags = Flags {
        obj_kind,
        react_dro: None,
    };
    let interface = None;
    let obj_reason = reason.dupe().replace_desc(VirtualReasonDesc::RObjectType);
    let slice = object::Slice {
        reason: obj_reason,
        props,
        flags,
        frozen: false,
        generics,
        interface,
        reachable_targs: Rc::from([]),
    };
    Ok(slice_utils::map_object(
        property_type,
        mapped_type_flags,
        cx,
        reason,
        &use_op,
        None,
        &slice,
    ))
}

pub fn run<'cx>(
    trace: DepthTrace,
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: &Reason,
    resolve_tool: &object::ResolveTool,
    tool: &object::Tool,
    l: &Type,
    tout: &Type,
) -> Result<(), FlowJsException> {
    let object_spread = |options: &object::spread::Target,
                         state: &object::spread::State,
                         cx: &Context<'cx>,
                         use_op: UseOp,
                         reason: &Reason,
                         x: Vec1<object::Slice>,
                         tout: &Type| {
        let dict_check = |cx: &Context<'cx>,
                          use_op: UseOp,
                          d1: &DictType,
                          d2: &DictType|
         -> Result<(), FlowJsException> {
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), &d2.key, &d1.key)?;
            FlowJs::rec_flow_t(cx, trace, use_op, &d2.value, &d1.value)?;
            Ok(())
        };
        let return_ = |cx: &Context<'cx>, use_op: UseOp, t: Type| {
            FlowJs::rec_flow_t(cx, trace, use_op, &t, tout)?;
            Ok(())
        };
        let recurse = |cx: &Context<'cx>,
                       use_op: UseOp,
                       reason: &Reason,
                       resolve_tool: object::ResolveTool,
                       tool: object::Tool,
                       t: Type|
         -> Result<(), FlowJsException> {
            FlowJs::rec_flow(
                cx,
                trace,
                &t,
                &UseT::new(UseTInner::ObjKitT(
                    use_op,
                    reason.dupe(),
                    Box::new(resolve_tool),
                    Box::new(tool),
                    tout.dupe(),
                )),
            )?;
            Ok(())
        };
        slice_utils::object_spread(
            &dict_check,
            &flow_js_utils::add_output,
            &return_,
            &recurse,
            options,
            state.clone(),
            cx,
            use_op,
            reason,
            x,
        )
    };

    // **************************
    // * Check component config *
    // **************************
    let check_component_config = |allow_ref_in_spread: bool,
                                  pmap: &properties::PropertiesMap,
                                  cx: &Context<'cx>,
                                  use_op: UseOp,
                                  reason: &Reason,
                                  x: Vec1<object::Slice>,
                                  tout: &Type|
     -> Result<(), FlowJsException> {
        let return_ = |cx: &Context<'cx>, use_op: UseOp, t: Type| {
            FlowJs::rec_flow_t(cx, trace, use_op, &t, tout)?;
            Ok(())
        };
        slice_utils::check_component_config(
            &|cx, msg| flow_js_utils::add_output(cx, msg),
            &return_,
            allow_ref_in_spread,
            pmap,
            cx,
            use_op,
            reason,
            x,
        )
    };

    // ***************
    // * Object Rest *
    // ***************
    let object_rest = |options: &object::rest::MergeMode,
                       state: &object::rest::State,
                       cx: &Context<'cx>,
                       use_op: UseOp,
                       reason: &Reason,
                       x: Vec1<object::Slice>,
                       tout: &Type| {
        let return_ = |cx: &Context<'cx>,
                       use_op: Box<dyn Fn(Polarity) -> UseOp>,
                       options: object::rest::MergeMode,
                       t: Type| {
            match options {
                object::rest::MergeMode::ReactConfigMerge(Polarity::Neutral) => {
                    FlowJs::rec_unify(
                        cx,
                        trace,
                        use_op(Polarity::Neutral),
                        UnifyCause::Uncategorized,
                        None,
                        &t,
                        tout,
                    )?;
                }
                object::rest::MergeMode::ReactConfigMerge(Polarity::Negative) => {
                    FlowJs::rec_flow_t(cx, trace, use_op(Polarity::Negative), tout, &t)?;
                }
                object::rest::MergeMode::ReactConfigMerge(Polarity::Positive) => {
                    FlowJs::rec_flow_t(cx, trace, use_op(Polarity::Positive), &t, tout)?;
                }
                _ => {
                    // Intentional UnknownUse here.
                    FlowJs::rec_flow_t(cx, trace, unknown_use(), &t, tout)?;
                }
            }
            Ok(())
        };
        let recurse = |cx: &Context<'cx>,
                       use_op: UseOp,
                       reason: &Reason,
                       resolve_tool: object::ResolveTool,
                       tool: object::Tool,
                       t: Type| {
            FlowJs::rec_flow(
                cx,
                trace,
                &t,
                &UseT::new(UseTInner::ObjKitT(
                    use_op,
                    reason.dupe(),
                    Box::new(resolve_tool),
                    Box::new(tool),
                    tout.dupe(),
                )),
            )?;
            Ok(())
        };
        let subt_check = |use_op: UseOp,
                          cx: &Context<'cx>,
                          (t1, t2): (&Type, &Type)|
         -> Result<(), FlowJsException> {
            FlowJs::rec_flow_t(cx, trace, use_op, t1, t2)?;
            Ok(())
        };
        slice_utils::object_rest(
            &|cx, msg| flow_js_utils::add_output(cx, msg),
            &return_,
            &recurse,
            &subt_check,
            options,
            state,
            cx,
            use_op,
            reason,
            x,
        )
    };

    // *********************
    // * Object Make Exact *
    // *********************
    let object_make_exact = |cx: &Context<'cx>,
                             _use_op: UseOp,
                             reason: &Reason,
                             x: Vec1<object::Slice>,
                             tout: &Type|
     -> Result<(), FlowJsException> {
        // We always use an unknown_use intentionally when flowing to the tout. The use_op associated
        // with the tvar is more relevant with the use of the $Exact type than the use_op associated
        // with the $Exact instantiation *)
        let t = slice_utils::object_make_exact(cx, reason, x)?;
        FlowJs::rec_flow_t(cx, trace, unknown_use(), &t, tout)?;
        Ok(())
    };

    // ********************
    let object_read_only = |cx: &Context<'cx>,
                            _use_op: UseOp,
                            reason: &Reason,
                            x: Vec1<object::Slice>,
                            tout: &Type|
     -> Result<(), FlowJsException> {
        // We always use an unknown_use intentionally when flowing to the tout. The use_op associated
        // with the tvar is more relevant with the use of the ReadOnly type than the use_op associated
        // with the ReadOnly instantiation
        FlowJs::rec_flow_t(
            cx,
            trace,
            unknown_use(),
            &slice_utils::object_read_only(cx, reason, x),
            tout,
        )?;
        Ok(())
    };

    // ******************
    // * Object Partial *
    // ******************
    let object_partial = |cx: &Context<'cx>,
                          _use_op: UseOp,
                          reason: &Reason,
                          x: Vec1<object::Slice>,
                          tout: &Type|
     -> Result<(), FlowJsException> {
        // We always use an unknown_use intentionally when flowing to the tout. The use_op associated
        // with the tvar is more relevant with the use of the Partial type than the use_op associated
        // with the Partial instantiation
        FlowJs::rec_flow_t(
            cx,
            trace,
            unknown_use(),
            &slice_utils::object_update_optionality(
                slice_utils::ObjectUpdateOptionalityKind::Partial,
                cx,
                reason,
                x,
            ),
            tout,
        )?;
        Ok(())
    };

    // *******************
    // * Object Required *
    // *******************
    let object_required = |cx: &Context<'cx>,
                           _use_op: UseOp,
                           reason: &Reason,
                           x: Vec1<object::Slice>,
                           tout: &Type|
     -> Result<(), FlowJsException> {
        // We always use an unknown_use intentionally when flowing to the tout. The use_op associated
        // with the tvar is more relevant with the use of the Required type than the use_op associated
        // with the Required instantiation
        FlowJs::rec_flow_t(
            cx,
            trace,
            unknown_use(),
            &slice_utils::object_update_optionality(
                slice_utils::ObjectUpdateOptionalityKind::Required,
                cx,
                reason,
                x,
            ),
            tout,
        )?;
        Ok(())
    };

    // **************
    // * Object Rep *
    // **************
    let object_rep = |cx: &Context<'cx>,
                      use_op: UseOp,
                      reason: &Reason,
                      x: Vec1<object::Slice>,
                      tout: &Type|
     -> Result<(), FlowJsException> {
        let mk_object = |slice: &object::Slice| -> Type {
            let object::Slice {
                reason: r,
                props,
                flags,
                frozen: _,
                generics,
                interface: _,
                reachable_targs,
            } = slice;
            // TODO(jmbrown): Add polarity information to props
            let polarity = Polarity::Neutral;
            let pmap = properties::PropertiesMap::from_btree_map(
                props
                    .iter()
                    .map(|(name, prop)| {
                        let object::Prop {
                            prop_t: t,
                            is_own: _,
                            is_method,
                            polarity: _,
                            key_loc,
                        } = prop;
                        let p = if *is_method {
                            Property::new(PropertyInner::Method {
                                key_loc: key_loc.dupe(),
                                type_: t.dupe(),
                            })
                        } else {
                            Property::new(PropertyInner::Field(Box::new(FieldData {
                                preferred_def_locs: None,
                                key_loc: key_loc.dupe(),
                                type_: t.dupe(),
                                polarity,
                            })))
                        };
                        (name.dupe(), p)
                    })
                    .collect(),
            );
            let new_flags = Flags {
                obj_kind: obj_type::map_dict(
                    |mut dict| {
                        dict.dict_polarity = polarity;
                        dict
                    },
                    flags.obj_kind.clone(),
                ),
                react_dro: flags.react_dro.clone(),
            };
            let call = None;
            let id = cx.generate_property_map(pmap);
            let proto = Type::new(TypeInner::ObjProtoT(reason.dupe()));
            slice_utils::mk_object_type(
                cx,
                r,
                false,
                true,
                None,
                reachable_targs,
                subst_name::OpKind::CreateElement,
                &new_flags,
                call,
                id,
                proto,
                generics.clone(),
            )
        };
        let ts = x.mapped_ref(mk_object);
        let (t0, rest) = ts.split_off_first();
        let t = if rest.is_empty() {
            t0
        } else {
            let mut iter = rest.into_iter();
            let t1 = iter.next().unwrap();
            let remaining: Rc<[Type]> = iter.collect();
            Type::new(TypeInner::UnionT(
                reason.dupe(),
                union_rep::make(None, union_rep::UnionKind::UnknownKind, t0, t1, remaining),
            ))
        };
        FlowJs::rec_flow_t(cx, trace, use_op, &t, tout)?;
        Ok(())
    };

    // ****************
    let react_config = |ref_manipulation: &object::react_config::RefManipulation,
                        state: &object::react_config::State,
                        cx: &Context<'cx>,
                        use_op: UseOp,
                        reason: &Reason,
                        x: Vec1<object::Slice>,
                        tout: &Type|
     -> Result<(), FlowJsException> {
        // All props currently have a neutral polarity. However, they should have a
        // positive polarity (or even better, constant) since React.createElement()
        // freezes the type of props. We use a neutral polarity today because the
        // props type we flow the config into is written by users who very rarely
        // add a positive variance annotation. We may consider marking that type as
        // constant in the future as well.
        let prop_polarity = Polarity::Neutral;

        //       let finish ~ref_manipulation cx trace reason config defaults =
        let finish = |config: &object::Slice,
                      defaults: Option<&object::Slice>|
         -> Result<Type, FlowJsException> {
            let object::Slice {
                reason: config_reason,
                props: config_props,
                flags: config_flags,
                frozen: _,
                generics: config_generics,
                interface: _,
                reachable_targs: config_targs,
            } = config;
            let config_dict = obj_type::get_dict_opt(&config_flags.obj_kind);
            let key_name = Name::new("key");
            let ref_name = Name::new("ref");
            let make_property = |key_loc: Option<ALoc>, type_: Type, is_method: bool| {
                if is_method {
                    Property::new(PropertyInner::Method { key_loc, type_ })
                } else {
                    Property::new(PropertyInner::Field(Box::new(FieldData {
                        preferred_def_locs: None,
                        key_loc,
                        type_,
                        polarity: prop_polarity,
                    })))
                }
            };
            let config_prop = |key: &Name| -> Option<Cow<'_, object::Prop>> {
                if key == &key_name {
                    None
                } else {
                    match ref_manipulation {
                        object::react_config::RefManipulation::AddRef(prop_t)
                            if key == &ref_name =>
                        {
                            Some(Cow::Owned(object::Prop {
                                prop_t: prop_t.dupe(),
                                is_own: true,
                                is_method: false,
                                polarity: prop_polarity,
                                key_loc: None,
                            }))
                        }
                        _ => {
                            slice_utils::get_prop(config_reason, config_props.get(key), config_dict)
                        }
                    }
                }
            };
            // Create the final props map and dict.
            //
            // NOTE: React will copy any enumerable prop whether or not it
            // is own to the config.
            let (props_map, flags, generics, reachable_targs): (
                BTreeMap<Name, Property>,
                Flags,
                object::GenericSpreadId,
                Rc<[(Type, Polarity)]>,
            ) = match defaults {
                // If we have some default props then we want to add the types for those
                // default props to our final props object.
                Some(defaults_slice) => {
                    let object::Slice {
                        reason: defaults_reason,
                        props: defaults_props,
                        flags: defaults_flags,
                        frozen: _,
                        generics: defaults_generics,
                        interface: _,
                        reachable_targs: defaults_targs,
                    } = defaults_slice;
                    let defaults_dict = obj_type::get_dict_opt(&defaults_flags.obj_kind);
                    //  Merge our props and default props.
                    let mut merged_props: BTreeMap<Name, Property> = defaults_props
                        .iter()
                        .map(|(key, prop)| {
                            (
                                key.dupe(),
                                make_property(
                                    prop.key_loc.dupe(),
                                    prop.prop_t.dupe(),
                                    prop.is_method,
                                ),
                            )
                        })
                        .collect();
                    let mut merge_key = |key: &Name| -> Result<(), FlowJsException> {
                        let p1 = config_prop(key);
                        let p2 = slice_utils::get_prop(
                            defaults_reason,
                            defaults_props.get(key),
                            defaults_dict,
                        );
                        let result = match (p1.as_deref(), p2.as_deref()) {
                            (None, None) => None,
                            (
                                Some(object::Prop {
                                    prop_t: t,
                                    is_own: _,
                                    is_method: m,
                                    polarity: _,
                                    key_loc: l,
                                }),
                                None,
                            ) => Some(make_property(l.dupe(), t.dupe(), *m)),
                            (
                                None,
                                Some(object::Prop {
                                    prop_t: t,
                                    is_own: _,
                                    is_method: m,
                                    polarity: _,
                                    key_loc: l,
                                }),
                            ) => Some(make_property(l.dupe(), t.dupe(), *m)),
                            // If a property is defined in both objects, and the first property's
                            // type includes void then we want to replace every occurrence of void
                            // with the second property's type. This is consistent with the behavior
                            // of function default arguments. If you call a function, `f`, like:
                            // `f(undefined)` and there is a default value for the first argument,
                            // then we will ignore the void type and use the type for the default
                            // parameter instead.
                            (
                                Some(object::Prop {
                                    prop_t: t1,
                                    is_own: _,
                                    is_method: m1,
                                    polarity: _,
                                    key_loc: l,
                                }),
                                Some(object::Prop {
                                    prop_t: t2,
                                    is_own: _,
                                    is_method: m2,
                                    polarity: _,
                                    key_loc: _,
                                }),
                            ) => {
                                // Use CondT to replace void with t1.
                                let t1_clone = t1.dupe();
                                let t2_clone = t2.dupe();
                                let t = flow_typing_tvar::mk_where_result(
                                    cx,
                                    reason.dupe(),
                                    |cx, tvar| {
                                        let filter_id = FlowJs::filter_optional(
                                            cx,
                                            Some(trace),
                                            reason,
                                            &t1_clone,
                                        )?;
                                        let open_t = Type::new(TypeInner::OpenT(Tvar::new(
                                            reason.dupe(),
                                            filter_id,
                                        )));
                                        FlowJs::rec_flow(
                                            cx,
                                            trace,
                                            &open_t,
                                            &UseT::new(UseTInner::CondT(Box::new(CondTData {
                                                reason: reason.dupe(),
                                                opt_type: None,
                                                true_t: t2_clone.dupe(),
                                                false_t: tvar.dupe(),
                                            }))),
                                        )?;
                                        Ok::<(), FlowJsException>(())
                                    },
                                )?;
                                Some(make_property(l.dupe(), t, *m1 || *m2))
                            }
                        };
                        if let Some(prop) = result {
                            merged_props.insert(key.dupe(), prop);
                        }
                        Ok(())
                    };
                    for key in config_props.keys() {
                        if key != &key_name {
                            merge_key(key)?;
                        }
                    }
                    if matches!(
                        ref_manipulation,
                        object::react_config::RefManipulation::AddRef(_)
                    ) {
                        merge_key(&ref_name)?;
                    }
                    // Merge the dictionary from our config with the defaults dictionary.
                    let dict: Option<DictType> = match (config_dict, defaults_dict) {
                        (Some(d1), Some(d2)) => Some(DictType {
                            dict_name: None,
                            key: Type::new(TypeInner::UnionT(
                                reason.dupe(),
                                union_rep::make(
                                    None,
                                    union_rep::UnionKind::UnknownKind,
                                    d1.key.dupe(),
                                    d2.key.dupe(),
                                    Rc::from([]),
                                ),
                            )),
                            value: Type::new(TypeInner::UnionT(
                                reason.dupe(),
                                union_rep::make(
                                    None,
                                    union_rep::UnionKind::UnknownKind,
                                    slice_utils::read_dict(config_reason, d1),
                                    slice_utils::read_dict(defaults_reason, d2),
                                    Rc::from([]),
                                ),
                            )),
                            dict_polarity: prop_polarity,
                        }),
                        (Some(d), None) | (None, Some(d)) => Some(d.clone()),
                        (None, None) => None,
                    };
                    // React freezes the config so we set the frozen flag to true. The
                    // final object is only exact if both the config and defaults objects
                    // are exact.
                    let obj_kind = match dict {
                        Some(d) => ObjKind::Indexed(d),
                        None => {
                            if obj_type::is_exact(&config_flags.obj_kind)
                                && obj_type::is_exact(&defaults_flags.obj_kind)
                            {
                                ObjKind::Exact
                            } else {
                                ObjKind::Inexact
                            }
                        }
                    };
                    let flags = Flags {
                        obj_kind,
                        react_dro: None,
                    };
                    let generics =
                        flow_typing_generics::spread_append(config_generics, defaults_generics);
                    let mut targs: Vec<(Type, Polarity)> = config_targs.to_vec();
                    targs.extend(defaults_targs.iter().cloned());
                    (merged_props, flags, generics, targs.into())
                }
                // Otherwise turn our slice props map into an object props.
                None => {
                    let mut props_map: BTreeMap<Name, Property> = config_props
                        .keys()
                        .filter_map(|name| {
                            config_prop(name).map(|prop| {
                                let prop = prop.as_ref();
                                (
                                    name.dupe(),
                                    make_property(
                                        prop.key_loc.dupe(),
                                        prop.prop_t.dupe(),
                                        prop.is_method,
                                    ),
                                )
                            })
                        })
                        .collect();
                    if let object::react_config::RefManipulation::AddRef(prop_t) = ref_manipulation
                    {
                        props_map
                            .entry(ref_name.dupe())
                            .or_insert_with(|| make_property(None, prop_t.dupe(), false));
                    }
                    // Create a new dictionary from our config's dictionary with a
                    // positive polarity.
                    let dict = config_dict.map(|d| DictType {
                        dict_name: None,
                        key: d.key.dupe(),
                        value: d.value.dupe(),
                        dict_polarity: prop_polarity,
                    });
                    let obj_kind = obj_type::obj_kind_from_optional_dict(
                        dict,
                        if obj_type::is_exact(&config_flags.obj_kind) {
                            ObjKind::Exact
                        } else {
                            ObjKind::Inexact
                        },
                    );
                    let flags = Flags {
                        obj_kind,
                        react_dro: None,
                    };
                    (
                        props_map,
                        flags,
                        config_generics.clone(),
                        config_targs.dupe(),
                    )
                }
            };
            let call = None;
            // Finish creating our props object.
            let pmap = properties::PropertiesMap::from_btree_map(props_map);
            let id = cx.generate_property_map(pmap);
            let proto = Type::new(TypeInner::ObjProtoT(reason.dupe()));
            Ok(slice_utils::mk_object_type(
                cx,
                reason,
                false,
                false,
                None,
                &reachable_targs,
                subst_name::OpKind::ReactConfig,
                &flags,
                call,
                id,
                proto,
                generics,
            ))
        };

        match state {
            // If we have some type for default props then we need to wait for that
            // type to resolve before finishing our props type.
            object::react_config::State::Config {
                component_default_props: Some(t),
            } => {
                let resolve_tool = object::ResolveTool::Resolve(object::Resolve::Next);
                let new_state = object::react_config::State::Defaults {
                    config: object::Resolved(x),
                };
                FlowJs::rec_flow(
                    cx,
                    trace,
                    t,
                    &UseT::new(UseTInner::ObjKitT(
                        use_op,
                        reason.dupe(),
                        Box::new(resolve_tool),
                        Box::new(object::Tool::ReactConfig {
                            state: new_state,
                            ref_manipulation: ref_manipulation.clone(),
                        }),
                        tout.dupe(),
                    )),
                )?;
                Ok(())
            }
            // If we have no default props then finish our object and flow it to our tout type.
            object::react_config::State::Config {
                component_default_props: None,
            } => {
                let mut ts = Vec::new();
                for c in x.iter() {
                    ts.push(finish(c, None)?);
                }
                let mut iter = ts.into_iter();
                let t0 = iter.next().unwrap();
                let t = if let Some(t1) = iter.next() {
                    let remaining: Rc<[Type]> = iter.collect();
                    Type::new(TypeInner::UnionT(
                        reason.dupe(),
                        union_rep::make(None, union_rep::UnionKind::UnknownKind, t0, t1, remaining),
                    ))
                } else {
                    t0
                };
                FlowJs::rec_flow(
                    cx,
                    trace,
                    &t,
                    &UseT::new(UseTInner::UseT(use_op, tout.dupe())),
                )?;
                Ok(())
            }
            // If we had default props and those defaults resolved then finish our
            // props object with those default props. *)
            object::react_config::State::Defaults { config } => {
                let mut all_results: Vec<Type> = Vec::new();
                for c in config.0.iter() {
                    for d in x.iter() {
                        all_results.push(finish(c, Some(d))?);
                    }
                }
                let t = if all_results.len() == 1 {
                    all_results.into_iter().next().unwrap()
                } else {
                    let mut iter = all_results.into_iter();
                    let t0 = iter.next().unwrap();
                    let t1 = iter.next().unwrap();
                    let remaining: Rc<[Type]> = iter.collect();
                    Type::new(TypeInner::UnionT(
                        reason.dupe(),
                        union_rep::make(None, union_rep::UnionKind::UnknownKind, t0, t1, remaining),
                    ))
                };
                FlowJs::rec_flow(
                    cx,
                    trace,
                    &t,
                    &UseT::new(UseTInner::UseT(use_op, tout.dupe())),
                )?;
                Ok(())
            }
        }
    };

    // **************
    // * Object Map *
    // **************
    let object_map = |prop_type: &Type,
                      mapped_type_flags: &MappedTypeFlags,
                      selected_keys_opt: &Option<Type>,
                      cx: &Context<'cx>,
                      use_op: UseOp,
                      reason: &Reason,
                      x: Vec1<object::Slice>,
                      tout: &Type|
     -> Result<(), FlowJsException> {
        let selected_keys = selected_keys_opt
            .as_ref()
            .map(|keys| partition_keys_and_indexer(cx, trace, use_op.dupe(), reason, keys))
            .transpose()?;
        let ts = x.mapped_ref(|slice| {
            slice_utils::map_object(
                prop_type,
                mapped_type_flags,
                cx,
                reason,
                &use_op,
                selected_keys.clone(),
                slice,
            )
        });
        let (t0, rest) = ts.split_off_first();
        let t = if rest.is_empty() {
            t0
        } else {
            let mut iter = rest.into_iter();
            let t1 = iter.next().unwrap();
            let remaining: Rc<[Type]> = iter.collect();
            Type::new(TypeInner::UnionT(
                reason.dupe(),
                union_rep::make(None, union_rep::UnionKind::UnknownKind, t0, t1, remaining),
            ))
        };
        // Intentional UnknownUse here.
        FlowJs::rec_flow_t(cx, trace, unknown_use(), &t, tout)?;
        Ok(())
    };

    // *********************
    // * Object Resolution *
    // *********************
    let next = |cx: &Context<'cx>,
                use_op: UseOp,
                tool: &object::Tool,
                reason: &Reason,
                x: Vec1<object::Slice>|
     -> Result<(), FlowJsException> {
        match tool {
            object::Tool::MakeExact => object_make_exact(cx, use_op, reason, x, tout),
            object::Tool::Spread(options, state) => {
                object_spread(options, state, cx, use_op, reason, x, tout)?;
                Ok(())
            }
            object::Tool::Rest(options, state) => {
                object_rest(options, state, cx, use_op, reason, x, tout)?;
                Ok(())
            }
            object::Tool::ReactConfig {
                state,
                ref_manipulation,
            } => react_config(ref_manipulation, state, cx, use_op, reason, x, tout),
            object::Tool::ReadOnly => object_read_only(cx, use_op, reason, x, tout),
            object::Tool::Partial => object_partial(cx, use_op, reason, x, tout),
            object::Tool::Required => object_required(cx, use_op, reason, x, tout),
            object::Tool::ObjectRep => object_rep(cx, use_op, reason, x, tout),
            object::Tool::ReactCheckComponentConfig {
                props: pmap,
                allow_ref_in_spread,
            } => check_component_config(*allow_ref_in_spread, pmap, cx, use_op, reason, x, tout),
            object::Tool::ObjectMap {
                prop_type,
                mapped_type_flags,
                selected_keys_opt,
            } => object_map(
                prop_type,
                mapped_type_flags,
                selected_keys_opt,
                cx,
                use_op,
                reason,
                x,
                tout,
            ),
        }
    };

    let return_ = |cx: &Context<'cx>, use_op: UseOp, t: Type| -> Result<(), FlowJsException> {
        FlowJs::rec_flow_t(cx, trace, use_op, &t, tout)?;
        Ok(())
    };
    let next_wrapped = |cx: &Context<'cx>,
                        use_op: UseOp,
                        tool: &object::Tool,
                        reason: &Reason,
                        x: Vec1<object::Slice>|
     -> Result<(), FlowJsException> { next(cx, use_op, tool, reason, x) };
    let recurse = |cx: &Context<'cx>,
                   use_op: UseOp,
                   reason: &Reason,
                   resolve_tool: object::ResolveTool,
                   tool: &object::Tool,
                   t: Type|
     -> Result<(), FlowJsException> {
        FlowJs::rec_flow(
            cx,
            trace,
            &t,
            &UseT::new(UseTInner::ObjKitT(
                use_op,
                reason.dupe(),
                Box::new(resolve_tool),
                Box::new(tool.clone()),
                tout.dupe(),
            )),
        )?;
        Ok(())
    };
    let statics = |cx: &Context<'cx>, r: &Reason, i: &Type| -> Result<Type, FlowJsException> {
        flow_typing_tvar::mk_no_wrap_where_result(cx, r.dupe(), |cx, reason, tvar_id| {
            let tvar = Tvar::new(reason.dupe(), tvar_id as u32);
            FlowJs::rec_flow(
                cx,
                trace,
                i,
                &UseT::new(UseTInner::GetStaticsT(Box::new(tvar))),
            )?;
            Ok(())
        })
    };
    slice_utils::run(
        &flow_js_utils::add_output,
        &return_,
        &next_wrapped,
        &recurse,
        &statics,
        cx,
        use_op,
        reason,
        resolve_tool.clone(),
        tool,
        l,
    )
}
