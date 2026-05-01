/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_common::subst_name::SubstNameInner;
use flow_common_utils::list_utils;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DestructorConditionalTypeData;
use flow_typing_type::type_::DestructorMappedTypeData;
use flow_typing_type::type_::FieldData;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::GetSetData;
use flow_typing_type::type_::InstanceT;
use flow_typing_type::type_::MappedTypeHomomorphicFlag;
use flow_typing_type::type_::ObjType;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Predicate;
use flow_typing_type::type_::PredicateInner;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::TypeParamInner;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::exports;
use flow_typing_type::type_::poly;
use flow_typing_type::type_::properties;
use flow_typing_type::type_util::mod_reason_of_t;
use flow_typing_type::type_util::reason_of_t;
use flow_typing_type::type_util::union_of_ts;
use flow_typing_visitors::type_mapper;
use flow_typing_visitors::type_mapper::TypeMapper;
use flow_typing_visitors::type_visitor::TypeVisitor;

use crate::flow_cache;

// (*****************)
// (* substitutions *)
// (*****************)

#[derive(Debug, Clone)]
pub enum Replacement<'cx> {
    TypeSubst(
        Type,
        // Free vars in type (lazy for memoization).
        // CX = Context<'cx> so the lazy can receive cx at force time
        // (no context capture needed).
        Rc<
            flow_lazy::Lazy<
                Context<'cx>,
                FlowOrdSet<SubstName>,
                Box<dyn FnOnce(&Context<'cx>) -> FlowOrdSet<SubstName> + 'cx>,
            >,
        >,
    ),
    AlphaRename(SubstName),
}

#[derive(Debug, Clone)]
struct FvAcc {
    bound: FlowOrdSet<SubstName>,
    free: FlowOrdSet<SubstName>,
}

struct FreeVarVisitor;

impl TypeVisitor<FvAcc> for FreeVarVisitor {
    fn type_<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, mut acc: FvAcc, t: &Type) -> FvAcc {
        match t.deref() {
            TypeInner::GenericT(box GenericTData { name, .. }) if !acc.bound.contains(name) => {
                acc.free.insert(name.dupe());
                flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t)
            }
            TypeInner::DefT(_, def_t) => {
                if let DefTInner::PolyT(box PolyTData {
                    tparams: xs,
                    t_out: inner,
                    ..
                }) = def_t.deref()
                {
                    let orig_bound = acc.bound.dupe();
                    for tp in xs.iter() {
                        acc.bound.insert(tp.name.dupe());
                        acc = self.type_param(cx, pole, acc, tp);
                    }
                    acc = self.type_(cx, pole, acc, inner);
                    acc.bound = orig_bound;
                    acc
                } else {
                    flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t)
                }
            }
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                instance,
                subst_name: this_name,
                ..
            }) => {
                let orig_bound = acc.bound.dupe();
                acc.bound.insert(this_name.dupe());
                acc = self.instance_type(cx, pole, acc, instance);
                acc.bound = orig_bound;
                acc
            }
            _ => flow_typing_visitors::type_visitor::type_default(self, cx, pole, acc, t),
        }
    }

    fn destructor<'cx>(&mut self, cx: &Context<'cx>, acc: FvAcc, t: &Destructor) -> FvAcc {
        match t {
            Destructor::ConditionalType(box DestructorConditionalTypeData {
                distributive_tparam_name,
                infer_tparams,
                extends_t,
                true_t,
                false_t,
            }) => self.with_distributive_tparam_name(
                acc,
                distributive_tparam_name,
                |this, mut acc| {
                    let pole = Polarity::Neutral;
                    acc = this.type_(cx, pole, acc, false_t);
                    for tp in infer_tparams.iter() {
                        acc.bound.insert(tp.name.dupe());
                        acc = this.type_param(cx, pole, acc, tp);
                    }
                    acc = this.type_(cx, pole, acc, extends_t);
                    this.type_(cx, pole, acc, true_t)
                },
            ),
            Destructor::MappedType(box DestructorMappedTypeData {
                distributive_tparam_name,
                property_type,
                homomorphic,
                ..
            }) => self.with_distributive_tparam_name(
                acc,
                distributive_tparam_name,
                |this, mut acc| {
                    let pole_todo = Polarity::Neutral;
                    acc = this.type_(cx, pole_todo, acc, property_type);
                    match homomorphic {
                        MappedTypeHomomorphicFlag::SemiHomomorphic(t) => {
                            this.type_(cx, pole_todo, acc, t)
                        }
                        MappedTypeHomomorphicFlag::Homomorphic
                        | MappedTypeHomomorphicFlag::Unspecialized => acc,
                    }
                },
            ),
            _ => flow_typing_visitors::type_visitor::destructor_default(self, cx, acc, t),
        }
    }
}

impl FreeVarVisitor {
    fn with_distributive_tparam_name<F>(
        &mut self,
        mut acc: FvAcc,
        name: &Option<SubstName>,
        f: F,
    ) -> FvAcc
    where
        F: FnOnce(&mut Self, FvAcc) -> FvAcc,
    {
        let orig_bound = acc.bound.dupe();
        if let Some(n) = name {
            acc.bound.insert(n.dupe());
        }
        acc = f(self, acc);
        acc.bound = orig_bound;
        acc
    }
}

pub fn free_var_finder<'cx>(
    cx: &Context<'cx>,
    bound: Option<FlowOrdSet<SubstName>>,
    t: &Type,
) -> FlowOrdSet<SubstName> {
    let bound = bound.unwrap_or_default();
    let mut visitor = FreeVarVisitor;
    let result = visitor.type_(
        cx,
        Polarity::Positive,
        FvAcc {
            free: FlowOrdSet::new(),
            bound,
        },
        t,
    );
    result.free
}

pub fn free_var_finder_in_destructor<'cx>(
    cx: &Context<'cx>,
    bound: Option<FlowOrdSet<SubstName>>,
    d: &Destructor,
) -> FlowOrdSet<SubstName> {
    let bound = bound.unwrap_or_default();
    let mut visitor = FreeVarVisitor;
    let result = visitor.destructor(
        cx,
        FvAcc {
            free: FlowOrdSet::new(),
            bound,
        },
        d,
    );
    result.free
}

// Substitute bound type variables with associated types in a type.

pub fn new_name(name: &SubstName, fvs: &FlowOrdSet<SubstName>) -> SubstName {
    let (ct, n): (i32, FlowSmolStr) = match name.deref() {
        SubstNameInner::Synthetic { name, .. } => {
            panic!("Cannot rename synthetic name {}", name)
        }
        SubstNameInner::Name(n) => (0, n.dupe()),
        SubstNameInner::Id(ct, n) => (*ct, n.dupe()),
    };

    let mut ct = ct + 1;
    loop {
        let name = SubstName::id(ct, n.dupe());
        if !fvs.contains(&name) {
            return name;
        }
        ct += 1;
    }
}

fn new_name_avoiding_map(
    name: &SubstName,
    fvs: &FlowOrdSet<SubstName>,
    map: &FlowOrdMap<SubstName, Replacement>,
) -> SubstName {
    let (ct, n): (i32, FlowSmolStr) = match name.deref() {
        SubstNameInner::Synthetic { name, .. } => {
            panic!("Cannot rename synthetic name {}", name)
        }
        SubstNameInner::Name(n) => (0, n.dupe()),
        SubstNameInner::Id(ct, n) => (*ct, n.dupe()),
    };

    let mut ct = ct + 1;
    loop {
        let candidate = SubstName::id(ct, n.dupe());
        if !fvs.contains(&candidate) && !map.contains_key(&candidate) {
            return candidate;
        }
        ct += 1;
    }
}

fn fvs_of_map<'cx>(
    cx: &Context<'cx>,
    map: &FlowOrdMap<SubstName, Replacement<'cx>>,
) -> FlowOrdSet<SubstName> {
    let mut acc = FlowOrdSet::new();
    for replacement in map.values() {
        match replacement {
            Replacement::TypeSubst(_, fvs) => {
                acc = acc.union(fvs.get_forced(cx).dupe());
            }
            Replacement::AlphaRename(_) => {}
        }
    }
    acc
}

fn union_ident_map_and_dedup<'cx, F>(
    cx: &Context<'cx>,
    mut f: F,
    t: Type,
    r: &Reason,
    rep: &flow_typing_type::type_::union_rep::UnionRep,
) -> Type
where
    F: FnMut(Type) -> Type,
{
    fn union_flatten<'cx>(cx: &Context<'cx>, ts: impl IntoIterator<Item = Type>) -> Vec<Type> {
        ts.into_iter().flat_map(|t| flatten(cx, t)).collect()
    }

    fn flatten<'cx>(cx: &Context<'cx>, t: Type) -> Vec<Type> {
        match t.deref() {
            TypeInner::UnionT(_, rep) => union_flatten(cx, rep.members_iter().map(|t| t.dupe())),
            TypeInner::MaybeT(r, inner_t) => {
                let null_t = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::NullT)));
                let void_t = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::VoidT)));
                let mut result = vec![null_t, void_t];
                result.extend(flatten(cx, inner_t.dupe()));
                result
            }
            TypeInner::OptionalT {
                reason,
                type_,
                use_desc,
            } => {
                let void_t =
                    flow_typing_type::type_::void::why_with_use_desc(*use_desc, reason.dupe());
                let mut result = vec![void_t];
                result.extend(flatten(cx, type_.dupe()));
                result
            }
            _ => vec![t],
        }
    }

    let members: Vec<Type> = rep.members_iter().map(|t| t.dupe()).collect();
    let t0 = members[0].dupe();
    let t1 = members[1].dupe();
    let ts: Rc<[Type]> = members[2..].iter().map(|t| t.dupe()).collect();

    let t0_ = f(t0.dupe());
    let t1_ = f(t1.dupe());
    let ts_ = list_utils::ident_map(|t_elem| f(t_elem.dupe()), Type::ptr_eq, ts.dupe());
    if t0.ptr_eq(&t0_) && t1.ptr_eq(&t1_) && Rc::ptr_eq(&ts, &ts_) {
        t
    } else {
        let mut all_ts = vec![t0_, t1_];
        all_ts.extend(ts_.iter().map(|t| t.dupe()));
        let flattened = union_flatten(cx, all_ts);
        let mut acc: Vec<Type> = Vec::new();
        let mut seen = HashSet::new();
        for t_item in flattened {
            if seen.insert(t_item.dupe()) {
                acc.push(t_item);
            }
        }
        union_of_ts(r.dupe(), acc, None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Purpose {
    #[default]
    Normal,
    // In generic conditional type, we have a step to replace all GenericT with any, to see whether
    // the check will still fail (so that we can always take the false branch).
    // During the substitution, we might encounter `Subst_name.Synthetic`. We shouldn't crash
    // over there. Instead, we should keep them as is. *)
    ConditionalTypeAnySubst,
}

type MapCx<'cx> = (
    FlowOrdMap<SubstName, Replacement<'cx>>,
    bool,
    bool,
    Purpose,
    Option<UseOp>,
);

pub fn call_prop<'cx, MapCx, M: TypeMapper<'cx, MapCx>>(
    mapper: &mut M,
    cx: &Context<'cx>,
    map_cx: &MapCx,
    id: i32,
) -> i32 {
    let t = cx.find_call(id);
    let t_prime = mapper.type_(cx, map_cx, t.dupe());
    if t.ptr_eq(&t_prime) {
        id
    } else {
        cx.make_call_prop(t_prime)
    }
}

pub fn props<'cx, MapCx, M: TypeMapper<'cx, MapCx>>(
    mapper: &mut M,
    cx: &Context<'cx>,
    map_cx: &MapCx,
    id: properties::Id,
) -> properties::Id {
    let props_map = cx.find_props(id.dupe());
    let mut props_map_prime = None;
    for (name, prop) in props_map.iter() {
        let new_prop = match prop.deref() {
            PropertyInner::Field(fd) => {
                let type_prime = mapper.type_(cx, map_cx, fd.type_.dupe());
                if fd.type_.ptr_eq(&type_prime) {
                    prop.dupe()
                } else {
                    Property::new(PropertyInner::Field(Box::new(FieldData {
                        preferred_def_locs: fd.preferred_def_locs.clone(),
                        key_loc: fd.key_loc.dupe(),
                        type_: type_prime,
                        polarity: fd.polarity,
                    })))
                }
            }
            PropertyInner::Get { key_loc, type_ } => {
                let type_prime = mapper.type_(cx, map_cx, type_.dupe());
                if type_.ptr_eq(&type_prime) {
                    prop.dupe()
                } else {
                    Property::new(PropertyInner::Get {
                        key_loc: key_loc.dupe(),
                        type_: type_prime,
                    })
                }
            }
            PropertyInner::Set { key_loc, type_ } => {
                let type_prime = mapper.type_(cx, map_cx, type_.dupe());
                if type_.ptr_eq(&type_prime) {
                    prop.dupe()
                } else {
                    Property::new(PropertyInner::Set {
                        key_loc: key_loc.dupe(),
                        type_: type_prime,
                    })
                }
            }
            PropertyInner::GetSet(gs) => {
                let get_type_prime = mapper.type_(cx, map_cx, gs.get_type.dupe());
                let set_type_prime = mapper.type_(cx, map_cx, gs.set_type.dupe());
                if gs.get_type.ptr_eq(&get_type_prime) && gs.set_type.ptr_eq(&set_type_prime) {
                    prop.dupe()
                } else {
                    Property::new(PropertyInner::GetSet(Box::new(GetSetData {
                        get_key_loc: gs.get_key_loc.dupe(),
                        get_type: get_type_prime,
                        set_key_loc: gs.set_key_loc.dupe(),
                        set_type: set_type_prime,
                    })))
                }
            }
            PropertyInner::Method { key_loc, type_ } => {
                let type_prime = mapper.type_(cx, map_cx, type_.dupe());
                if type_.ptr_eq(&type_prime) {
                    prop.dupe()
                } else {
                    Property::new(PropertyInner::Method {
                        key_loc: key_loc.dupe(),
                        type_: type_prime,
                    })
                }
            }
        };
        if !prop.ptr_eq(&new_prop) {
            props_map_prime
                .get_or_insert_with(|| props_map.clone())
                .insert(name.dupe(), new_prop);
        }
    }
    if let Some(props_map_prime) = props_map_prime {
        // When substitution results in a new property map, we have to use a
        // generated id, rather than a location from source. The substituted
        // object will have the same location as the generic version, meaning
        // that this location will not serve as a unique identifier.
        cx.generate_property_map(props_map_prime)
    } else {
        id
    }
}

pub fn exports<'cx, MapCx, M: TypeMapper<'cx, MapCx>>(
    mapper: &mut M,
    cx: &Context<'cx>,
    map_cx: &MapCx,
    id: exports::Id,
) -> exports::Id {
    // let exps = Context.find_exports cx id in
    let exps = cx.find_exports(id);
    let mut exps_prime = None;
    for (name, ns) in exps.iter() {
        let type_prime = mapper.type_(cx, map_cx, ns.type_.dupe());
        if !ns.type_.ptr_eq(&type_prime) {
            exps_prime.get_or_insert_with(|| exps.clone()).insert(
                name.dupe(),
                flow_typing_type::type_::NamedSymbol::new(
                    ns.name_loc.dupe(),
                    ns.preferred_def_locs.clone(),
                    type_prime,
                ),
            );
        }
    }
    if let Some(exps_prime) = exps_prime {
        cx.make_export_map(exps_prime)
    } else {
        id
    }
}

struct Substituter<'cx> {
    change_id: bool,
    obj_reachable_targs: Option<Vec<(Type, Polarity)>>,
    cached_fvs: Option<(
        FlowOrdMap<SubstName, Replacement<'cx>>,
        FlowOrdSet<SubstName>,
    )>,
}

impl<'cx> Substituter<'cx> {
    fn new() -> Self {
        Self {
            change_id: false,
            obj_reachable_targs: None,
            cached_fvs: None,
        }
    }

    fn get_fvs_of_map(
        &mut self,
        cx: &Context<'cx>,
        map: &FlowOrdMap<SubstName, Replacement<'cx>>,
    ) -> FlowOrdSet<SubstName> {
        match &self.cached_fvs {
            Some((cached_map, fvs)) if cached_map.as_inner().ptr_eq(map.as_inner()) => fvs.dupe(),
            _ => {
                let fvs = fvs_of_map(cx, map);
                self.cached_fvs = Some((map.dupe(), fvs.dupe()));
                fvs
            }
        }
    }

    fn name_in_fvs_of_map(
        &self,
        cx: &Context<'cx>,
        map: &FlowOrdMap<SubstName, Replacement<'cx>>,
        name: &SubstName,
    ) -> bool {
        match &self.cached_fvs {
            Some((cached_map, fvs)) if cached_map.as_inner().ptr_eq(map.as_inner()) => {
                fvs.contains(name)
            }
            _ => map.values().any(|replacement| match replacement {
                Replacement::TypeSubst(_, fvs) => fvs.get_forced(cx).contains(name),
                Replacement::AlphaRename(_) => false,
            }),
        }
    }

    fn avoid_capture(
        &mut self,
        cx: &Context<'cx>,
        map: FlowOrdMap<SubstName, Replacement<'cx>>,
        name: SubstName,
    ) -> (SubstName, FlowOrdMap<SubstName, Replacement<'cx>>) {
        if self.name_in_fvs_of_map(cx, &map, &name) {
            let fvs = self.get_fvs_of_map(cx, &map);
            let new_name_val = new_name_avoiding_map(&name, &fvs, &map);
            let mut new_map = map;
            new_map.insert(name, Replacement::AlphaRename(new_name_val.dupe()));
            self.cached_fvs = Some((new_map.dupe(), fvs));
            (new_name_val, new_map)
        } else if map.contains_key(&name) {
            let cached_fvs = match &self.cached_fvs {
                Some((cached_map, fvs)) if cached_map.as_inner().ptr_eq(map.as_inner()) => {
                    Some(fvs.dupe())
                }
                _ => None,
            };
            let mut new_map = map;
            new_map.remove(&name);
            if let Some(fvs) = cached_fvs {
                self.cached_fvs = Some((new_map.dupe(), fvs));
            }
            (name, new_map)
        } else {
            (name, map)
        }
    }

    fn subst_distributive_tparam_name(
        &mut self,
        cx: &Context<'cx>,
        name: &Option<SubstName>,
        map: FlowOrdMap<SubstName, Replacement<'cx>>,
    ) -> (Option<SubstName>, FlowOrdMap<SubstName, Replacement<'cx>>) {
        match name {
            None => (None, map),
            Some(name) => {
                let (new_name, new_map) = self.avoid_capture(cx, map, name.dupe());
                (Some(new_name), new_map)
            }
        }
    }
}

impl<'cx> TypeMapper<'cx, MapCx<'cx>> for Substituter<'cx> {
    fn tvar(&mut self, _cx: &Context<'cx>, _map_cx: &MapCx<'cx>, _r: &Reason, id: u32) -> u32 {
        id
    }

    fn call_prop(&mut self, cx: &Context<'cx>, map_cx: &MapCx<'cx>, id: i32) -> i32 {
        call_prop(self, cx, map_cx, id)
    }

    fn props(
        &mut self,
        cx: &Context<'cx>,
        map_cx: &MapCx<'cx>,
        id: properties::Id,
    ) -> properties::Id {
        props(self, cx, map_cx, id)
    }

    fn exports(&mut self, cx: &Context<'cx>, map_cx: &MapCx<'cx>, id: exports::Id) -> exports::Id {
        exports(self, cx, map_cx, id)
    }

    fn type_(&mut self, cx: &Context<'cx>, map_cx: &MapCx<'cx>, t: Type) -> Type {
        let (map, force, placeholder_no_infer, purpose, use_op) = map_cx;
        if map.is_empty() {
            return t;
        }
        let t_out = match t.deref() {
            TypeInner::GenericT(box GenericTData {
                name: subst_name,
                reason,
                ..
            }) if matches!(subst_name.deref(), SubstNameInner::Synthetic { .. }) => {
                let (name, op_kind, ids) = match &**subst_name {
                    SubstNameInner::Synthetic { name, op_kind, ts } => (name, op_kind, ts),
                    _ => unreachable!(),
                };
                if *purpose == Purpose::Normal && ids.iter().any(|name| map.contains_key(name)) {
                    let op_kind_str = match op_kind {
                        None => "None".to_string(),
                        Some(kind) => format!("{:?}", kind),
                    };
                    panic!(
                        "Cannot substitute through synthetic name {}(kind={}) at {:?}.",
                        name, op_kind_str, reason
                    );
                } else {
                    type_mapper::type_default(self, cx, map_cx, t.dupe())
                }
            }
            TypeInner::GenericT(box GenericTData {
                reason: tp_reason,
                name,
                no_infer,
                bound,
                id: gen_id,
            }) => {
                let annot_loc = tp_reason.loc();
                match map.get(name) {
                    None => type_mapper::type_default(self, cx, map_cx, t.dupe()),
                    Some(Replacement::TypeSubst(param_t, _)) => {
                        self.change_id = true;
                        if *placeholder_no_infer && *no_infer {
                            cx.mk_placeholder(reason_of_t(param_t).dupe())
                        } else {
                            match (&mut self.obj_reachable_targs, &**param_t) {
                                (_, TypeInner::GenericT(..)) => mod_reason_of_t(
                                    &|param_reason| {
                                        param_reason
                                            .reposition(annot_loc.dupe())
                                            .annotate(annot_loc.dupe())
                                    },
                                    param_t,
                                ),
                                (Some(targs), TypeInner::OpenT(_)) => {
                                    targs.push((param_t.dupe(), Polarity::Neutral));
                                    param_t.dupe()
                                }
                                _ => param_t.dupe(),
                            }
                        }
                    }
                    Some(Replacement::AlphaRename(name_prime)) => {
                        let t = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                            reason: tp_reason.dupe(),
                            name: name_prime.dupe(),
                            bound: bound.dupe(),
                            no_infer: *no_infer,
                            id: gen_id.clone(),
                        })));
                        type_mapper::type_default(self, cx, map_cx, t)
                    }
                }
            }
            TypeInner::DefT(reason, def_t) => {
                if let DefTInner::PolyT(box PolyTData {
                    tparams_loc,
                    tparams: xs,
                    t_out: inner,
                    id: poly_id,
                }) = def_t.deref()
                {
                    let prev_change_id = self.change_id;
                    self.change_id = false;
                    let mut xs_new: Vec<TypeParam> = Vec::new();
                    let mut current_map = map.dupe();
                    let mut changed = false;

                    for typeparam in xs.iter() {
                        let bound = self.type_(
                            cx,
                            &(
                                current_map.dupe(),
                                *force,
                                *placeholder_no_infer,
                                *purpose,
                                use_op.dupe(),
                            ),
                            typeparam.bound.dupe(),
                        );
                        let default = match &typeparam.default {
                            None => None,
                            Some(default) => {
                                let default_prime = self.type_(
                                    cx,
                                    &(
                                        current_map.dupe(),
                                        *force,
                                        *placeholder_no_infer,
                                        *purpose,
                                        use_op.dupe(),
                                    ),
                                    default.dupe(),
                                );
                                if default.ptr_eq(&default_prime) {
                                    typeparam.default.dupe()
                                } else {
                                    Some(default_prime)
                                }
                            }
                        };
                        let (name, new_map) =
                            self.avoid_capture(cx, current_map, typeparam.name.dupe());
                        current_map = new_map;
                        let bound_changed = !typeparam.bound.ptr_eq(&bound);
                        let default_changed = match (&typeparam.default, &default) {
                            (None, None) => false,
                            (Some(a), Some(b)) => !a.ptr_eq(b),
                            _ => true,
                        };
                        changed = changed || bound_changed || default_changed;
                        xs_new.push(TypeParam::new(TypeParamInner {
                            reason: typeparam.reason.dupe(),
                            name,
                            bound,
                            polarity: typeparam.polarity,
                            default,
                            is_this: typeparam.is_this,
                            is_const: typeparam.is_const,
                        }));
                    }

                    let inner_prime = self.type_(
                        cx,
                        &(current_map, false, *placeholder_no_infer, *purpose, None),
                        inner.dupe(),
                    );
                    let changed = changed || !inner.ptr_eq(&inner_prime);
                    let new_id = if self.change_id {
                        poly::Id::generate_id()
                    } else {
                        poly_id.dupe()
                    };
                    self.change_id = prev_change_id || self.change_id;
                    if changed {
                        Type::new(TypeInner::DefT(
                            reason.dupe(),
                            DefT::new(DefTInner::PolyT(Box::new(PolyTData {
                                tparams_loc: tparams_loc.dupe(),
                                tparams: xs_new.into(),
                                t_out: inner_prime,
                                id: new_id,
                            }))),
                        ))
                    } else {
                        t.dupe()
                    }
                } else {
                    type_mapper::type_default(self, cx, map_cx, t.dupe())
                }
            }
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: this,
                is_this: i,
                subst_name: this_name,
            }) => {
                let (name, new_map) = self.avoid_capture(cx, map.dupe(), this_name.dupe());
                let this_prime = self.instance_type(
                    cx,
                    &(
                        new_map,
                        *force,
                        *placeholder_no_infer,
                        *purpose,
                        use_op.dupe(),
                    ),
                    this,
                );
                if this.ptr_eq(&this_prime) && &name == this_name {
                    t.dupe()
                } else {
                    Type::new(TypeInner::ThisInstanceT(Box::new(ThisInstanceTData {
                        reason: r.dupe(),
                        instance: this_prime,
                        is_this: *i,
                        subst_name: name,
                    })))
                }
            }
            TypeInner::TypeAppT(box TypeAppTData {
                reason,
                use_op: op,
                type_: type_inner,
                targs,
                from_value,
                use_desc,
            }) => {
                let type_prime = self.type_(cx, map_cx, type_inner.dupe());
                let targs_prime = list_utils::ident_map(
                    |targ| self.type_(cx, map_cx, targ.dupe()),
                    Type::ptr_eq,
                    targs.dupe(),
                );
                if type_inner.ptr_eq(&type_prime) && Rc::ptr_eq(targs, &targs_prime) {
                    t.dupe()
                } else {
                    // If the TypeAppT changed then one of the type arguments had a
                    // GenericT that was substituted. In this case, also change the use_op
                    // so we can point at the op which instantiated the types that
                    // were substituted. *)
                    let new_use_op = use_op.as_ref().unwrap_or(op).dupe();
                    Type::new(TypeInner::TypeAppT(Box::new(TypeAppTData {
                        reason: reason.dupe(),
                        use_op: new_use_op,
                        type_: type_prime,
                        targs: targs_prime,
                        from_value: *from_value,
                        use_desc: *use_desc,
                    })))
                }
            }
            TypeInner::EvalT {
                type_: x,
                defer_use_t,
                ..
            } => {
                let op = &defer_use_t.0;
                let r = &defer_use_t.1;
                let d = &defer_use_t.2;
                let x_prime = self.type_(cx, map_cx, x.dupe());
                let d_prime = self.destructor(cx, map_cx, d.dupe());
                if x.ptr_eq(&x_prime) && Rc::ptr_eq(d, &d_prime) {
                    t.dupe()
                } else {
                    // If the EvalT changed then either the target or destructor had a
                    // GenericT that was substituted. In this case, also change the use_op
                    // so we can point at the op which instantiated the types that
                    // were substituted.
                    let new_use_op = use_op.as_ref().unwrap_or(op).dupe();
                    let new_defer_use = flow_typing_type::type_::TypeDestructorT::new(
                        flow_typing_type::type_::TypeDestructorTInner(
                            new_use_op,
                            r.dupe(),
                            d_prime,
                        ),
                    );
                    flow_cache::eval::id(cx, x_prime, new_defer_use)
                }
            }
            TypeInner::UnionT(r, urep) => union_ident_map_and_dedup(
                cx,
                |t_elem| self.type_(cx, map_cx, t_elem),
                t.dupe(),
                r,
                urep,
            ),
            _ => type_mapper::type_default(self, cx, map_cx, t.dupe()),
        };
        if t.ptr_eq(&t_out) {
            t
        } else {
            use flow_common::reason::VirtualReasonDesc;
            let t_out_reason = reason_of_t(&t_out);
            match t_out_reason.desc(false) {
                VirtualReasonDesc::RTypeAlias(box (name, Some(_), desc)) => {
                    let new_desc = flow_common::reason::VirtualReasonDesc::RTypeAlias(Box::new((
                        name.dupe(),
                        None,
                        desc.dupe(),
                    )));
                    mod_reason_of_t(&|r| r.replace_desc(new_desc.clone()), &t_out)
                }
                _ => t_out,
            }
        }
    }

    fn predicate(&mut self, cx: &Context<'cx>, map_cx: &MapCx<'cx>, p: &Predicate) -> Predicate {
        match p.deref() {
            PredicateInner::LatentP(..) => type_mapper::predicate_default(self, cx, map_cx, p),
            PredicateInner::LatentThisP(..) => type_mapper::predicate_default(self, cx, map_cx, p),
            _ => p.dupe(),
        }
    }

    fn obj_type(&mut self, cx: &Context<'cx>, map_cx: &MapCx<'cx>, t: Rc<ObjType>) -> Rc<ObjType> {
        let old_obj_reachable_targs = self.obj_reachable_targs.replace(Vec::new());
        let t_prime = type_mapper::obj_type_default(self, cx, map_cx, t.dupe());
        let reachable_targs = self
            .obj_reachable_targs
            .take()
            .expect("Invariant violation: reachable targs are None after visiting an object");
        self.obj_reachable_targs = match old_obj_reachable_targs {
            Some(mut targs) => {
                targs.extend(reachable_targs.iter().map(|(t, p)| (t.dupe(), *p)));
                Some(targs)
            }
            None => None,
        };
        let targs_same = reachable_targs.len() == t_prime.reachable_targs.len()
            && reachable_targs
                .iter()
                .zip(t_prime.reachable_targs.iter())
                .all(|((t1, p1), (t2, p2))| t1.ptr_eq(t2) && p1 == p2);
        if targs_same {
            t_prime
        } else {
            Rc::new(ObjType {
                flags: t_prime.flags.clone(),
                props_tmap: t_prime.props_tmap.dupe(),
                proto_t: t_prime.proto_t.dupe(),
                call_t: t_prime.call_t,
                reachable_targs: reachable_targs.into(),
            })
        }
    }

    fn destructor(
        &mut self,
        cx: &Context<'cx>,
        map_cx: &MapCx<'cx>,
        t: Rc<Destructor>,
    ) -> Rc<Destructor> {
        let (map, _, placeholder_no_infer, purpose, _) = map_cx;
        match t.deref() {
            Destructor::ConditionalType(box DestructorConditionalTypeData {
                distributive_tparam_name,
                infer_tparams,
                extends_t,
                true_t,
                false_t,
            }) => {
                let (distributive_tparam_name_new, mut current_map) =
                    self.subst_distributive_tparam_name(cx, distributive_tparam_name, map.dupe());
                let false_t_prime = self.type_(
                    cx,
                    &(
                        current_map.dupe(),
                        false,
                        *placeholder_no_infer,
                        *purpose,
                        None,
                    ),
                    false_t.dupe(),
                );
                let mut tparams: Vec<TypeParam> = Vec::new();
                let mut changed = false;
                for typeparam in infer_tparams.iter() {
                    let bound = self.type_(
                        cx,
                        &(
                            current_map.dupe(),
                            false,
                            *placeholder_no_infer,
                            *purpose,
                            None,
                        ),
                        typeparam.bound.dupe(),
                    );
                    let (name, new_map) =
                        self.avoid_capture(cx, current_map, typeparam.name.dupe());
                    current_map = new_map;
                    let bound_changed = !typeparam.bound.ptr_eq(&bound);
                    changed = changed || bound_changed;
                    tparams.push(TypeParam::new(TypeParamInner {
                        reason: typeparam.reason.dupe(),
                        name,
                        bound,
                        polarity: typeparam.polarity,
                        default: typeparam.default.dupe(),
                        is_this: typeparam.is_this,
                        is_const: typeparam.is_const,
                    }));
                }
                let extends_t_prime = self.type_(
                    cx,
                    &(
                        current_map.dupe(),
                        false,
                        *placeholder_no_infer,
                        *purpose,
                        None,
                    ),
                    extends_t.dupe(),
                );
                let true_t_prime = self.type_(
                    cx,
                    &(current_map, false, *placeholder_no_infer, *purpose, None),
                    true_t.dupe(),
                );
                if changed
                    || !extends_t.ptr_eq(&extends_t_prime)
                    || !true_t.ptr_eq(&true_t_prime)
                    || !false_t.ptr_eq(&false_t_prime)
                {
                    Rc::new(Destructor::ConditionalType(Box::new(
                        DestructorConditionalTypeData {
                            distributive_tparam_name: distributive_tparam_name_new,
                            infer_tparams: tparams.into(),
                            extends_t: extends_t_prime,
                            true_t: true_t_prime,
                            false_t: false_t_prime,
                        },
                    )))
                } else {
                    t
                }
            }
            Destructor::MappedType(box DestructorMappedTypeData {
                distributive_tparam_name,
                property_type,
                mapped_type_flags,
                homomorphic,
            }) => {
                let (distributive_tparam_name_new, new_map) =
                    self.subst_distributive_tparam_name(cx, distributive_tparam_name, map.dupe());
                let property_type_prime = self.type_(
                    cx,
                    &(new_map.dupe(), false, *placeholder_no_infer, *purpose, None),
                    property_type.dupe(),
                );
                let (homomorphic_prime, homomorphic_changed) = match homomorphic {
                    MappedTypeHomomorphicFlag::SemiHomomorphic(hom_t) => {
                        let hom_t_prime = self.type_(
                            cx,
                            &(new_map, false, *placeholder_no_infer, *purpose, None),
                            hom_t.dupe(),
                        );
                        if hom_t.ptr_eq(&hom_t_prime) {
                            (homomorphic.dupe(), false)
                        } else {
                            (
                                MappedTypeHomomorphicFlag::SemiHomomorphic(hom_t_prime),
                                true,
                            )
                        }
                    }
                    MappedTypeHomomorphicFlag::Homomorphic
                    | MappedTypeHomomorphicFlag::Unspecialized => (homomorphic.dupe(), false),
                };
                if property_type.ptr_eq(&property_type_prime) && !homomorphic_changed {
                    t
                } else {
                    Rc::new(Destructor::MappedType(Box::new(DestructorMappedTypeData {
                        distributive_tparam_name: distributive_tparam_name_new,
                        property_type: property_type_prime,
                        mapped_type_flags: *mapped_type_flags,
                        homomorphic: homomorphic_prime,
                    })))
                }
            }
            _ => type_mapper::destructor_default(self, cx, map_cx, t),
        }
    }

    fn eval_id(
        &mut self,
        _cx: &Context<'cx>,
        _map_cx: &MapCx<'cx>,
        _id: flow_typing_type::type_::eval::Id,
    ) -> flow_typing_type::type_::eval::Id {
        unreachable!("eval_id should never be called on substituter")
    }
}

pub fn subst<'cx>(
    cx: &Context<'cx>,
    use_op: Option<UseOp>,
    force: bool,
    placeholder_no_infer: bool,
    purpose: Purpose,
    map: &FlowOrdMap<SubstName, Type>,
    t: Type,
) -> Type {
    // Optimization: if map is empty, return type unchanged
    if map.is_empty() {
        return t;
    }

    let replacement_map: FlowOrdMap<SubstName, Replacement<'cx>> = map
        .iter()
        .map(|(k, v)| {
            let v = v.dupe();
            let v_c = v.dupe();
            (
                k.dupe(),
                Replacement::TypeSubst(
                    v,
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        free_var_finder(cx, None, &v_c)
                    }))),
                ),
            )
        })
        .collect();

    // substituter#type_ cx (map, force, placeholder_no_infer, purpose, use_op) ty
    let mut substituter = Substituter::new();
    substituter.type_(
        cx,
        &(
            replacement_map,
            force,
            placeholder_no_infer,
            purpose,
            use_op,
        ),
        t,
    )
}

pub fn subst_destructor<'cx>(
    cx: &Context<'cx>,
    use_op: Option<UseOp>,
    force: bool,
    placeholder_no_infer: bool,
    purpose: Purpose,
    map: &FlowOrdMap<SubstName, Type>,
    destructor: Rc<Destructor>,
) -> Rc<Destructor> {
    // Optimization: if map is empty, return destructor unchanged
    if map.is_empty() {
        return destructor;
    }

    let replacement_map: FlowOrdMap<SubstName, Replacement<'cx>> = map
        .iter()
        .map(|(k, v)| {
            let v = v.dupe();
            let v_c = v.dupe();
            (
                k.dupe(),
                Replacement::TypeSubst(
                    v,
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        free_var_finder(cx, None, &v_c)
                    }))),
                ),
            )
        })
        .collect();

    // substituter#destructor cx (map, force, placeholder_no_infer, purpose, use_op) des
    let mut substituter = Substituter::new();
    substituter.destructor(
        cx,
        &(
            replacement_map,
            force,
            placeholder_no_infer,
            purpose,
            use_op,
        ),
        destructor,
    )
}

pub fn subst_instance_type<'cx>(
    cx: &Context<'cx>,
    use_op: Option<UseOp>,
    force: bool,
    placeholder_no_infer: bool,
    purpose: Purpose,
    map: &FlowOrdMap<SubstName, Type>,
    instance: &InstanceT,
) -> InstanceT {
    // Optimization: if map is empty, return instance unchanged
    if map.is_empty() {
        return instance.dupe();
    }

    let replacement_map: FlowOrdMap<SubstName, Replacement<'cx>> = map
        .iter()
        .map(|(k, v)| {
            let v = v.dupe();
            let v_c = v.dupe();
            (
                k.dupe(),
                Replacement::TypeSubst(
                    v,
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        free_var_finder(cx, None, &v_c)
                    }))),
                ),
            )
        })
        .collect();

    let mut substituter = Substituter::new();
    substituter.instance_type(
        cx,
        &(
            replacement_map,
            force,
            placeholder_no_infer,
            purpose,
            use_op,
        ),
        instance,
    )
}
