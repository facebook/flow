/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module defines types representing signatures extracted from Flow source
// files. These signatures are used to increase the parallelism of type
// checking, similar to header/interface files in other languages.
//
// Note that these definitions are not recursive, but rather parametric over
// some type 'a. In practice these types are parameterized to create a fixed
// point.
//
// This parameterization is useful to re-use the same structure through separate
// phases of the analysis. For example, the parsing phase parameterizes these
// types including the possibility of a name reference at every step. The
// resolve phase then replaces these by-name references with the referent.
//
// As a matter of style, this module makes extensive use of inline records to
// avoid record label conflicts.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::flow_import_specifier;
use flow_common::polarity::Polarity;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use vec1::Vec1;

use crate::signature_error;

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TArg<Loc, T> {
    ImplicitArg(Loc),
    ExplicitArg(T),
}

impl<Loc, T> TArg<Loc, T> {
    pub fn map<CX, Loc2>(&self, cx: &mut CX, f: impl Fn(&mut CX, &Loc) -> Loc2) -> TArg<Loc2, T>
    where
        T: Clone,
    {
        match self {
            TArg::ImplicitArg(loc) => TArg::ImplicitArg(f(cx, loc)),
            // clone needed: generic T with Clone bound
            TArg::ExplicitArg(t) => TArg::ExplicitArg(t.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Arg<T> {
    Arg(T),
    SpreadArg(T),
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct TypeGuard<Loc, T> {
    pub loc: Loc,
    pub param_name: (Loc, FlowSmolStr),
    pub type_guard: T,
    pub one_sided: bool,
}

impl<Loc, T> TypeGuard<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> TypeGuard<Loc2, T2> {
        TypeGuard {
            loc: f_loc(cx, &self.loc),
            param_name: (f_loc(cx, &self.param_name.0), self.param_name.1.dupe()),
            type_guard: f_t(cx, &self.type_guard),
            one_sided: self.one_sided,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct TParam<Loc, T> {
    pub name_loc: Loc,
    pub name: FlowSmolStr,
    pub polarity: Polarity,
    pub bound: Option<T>,
    pub default: Option<T>,
    pub is_const: bool,
}

impl<Loc, T> TParam<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> TParam<Loc2, T2> {
        TParam {
            name_loc: f_loc(cx, &self.name_loc),
            name: self.name.dupe(),
            polarity: self.polarity,
            bound: self.bound.as_ref().map(|t| f_t(cx, t)),
            default: self.default.as_ref().map(|t| f_t(cx, t)),
            is_const: self.is_const,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TParams<Loc, T> {
    Mono,
    Poly(Loc, Vec1<TParam<Loc, T>>),
}

impl<Loc, T> TParams<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            TParams::Mono => {}
            TParams::Poly(loc, tparams) => {
                f_loc(cx, loc);
                for tp in tparams.iter() {
                    f_loc(cx, &tp.name_loc);
                    if let Some(b) = &tp.bound {
                        f_t(cx, b);
                    }
                    if let Some(d) = &tp.default {
                        f_t(cx, d);
                    }
                }
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> TParams<Loc2, T2> {
        match self {
            TParams::Mono => TParams::Mono,
            TParams::Poly(loc, tparams) => TParams::Poly(
                f_loc(cx, loc),
                tparams.mapped_ref(|tp| tp.map(cx, &f_loc, &f_t)),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunParam<T> {
    pub name: Option<FlowSmolStr>,
    pub t: T,
}

impl<T> FunParam<T> {
    pub fn map<CX, T2>(&self, cx: &mut CX, f_t: impl Fn(&mut CX, &T) -> T2) -> FunParam<T2> {
        FunParam {
            name: self.name.dupe(),
            t: f_t(cx, &self.t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunRestParam<Loc, T> {
    pub name: Option<FlowSmolStr>,
    pub loc: Loc,
    pub t: T,
}

impl<Loc, T> FunRestParam<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> FunRestParam<Loc2, T2> {
        FunRestParam {
            name: self.name.dupe(),
            loc: f_loc(cx, &self.loc),
            t: f_t(cx, &self.t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ReactEffect<Loc> {
    HookDecl(Loc),
    HookAnnot,
    ArbitraryEffect,
    AnyEffect,
}

impl<Loc> ReactEffect<Loc> {
    pub fn iter<CX>(&self, cx: &mut CX, f_loc: &impl Fn(&mut CX, &Loc)) {
        match self {
            ReactEffect::HookDecl(loc) => f_loc(cx, loc),
            ReactEffect::HookAnnot | ReactEffect::ArbitraryEffect | ReactEffect::AnyEffect => {}
        }
    }

    pub fn map<CX, Loc2>(
        &self,
        cx: &mut CX,
        f: impl Fn(&mut CX, &Loc) -> Loc2,
    ) -> ReactEffect<Loc2> {
        match self {
            ReactEffect::HookDecl(loc) => ReactEffect::HookDecl(f(cx, loc)),
            ReactEffect::HookAnnot => ReactEffect::HookAnnot,
            ReactEffect::ArbitraryEffect => ReactEffect::ArbitraryEffect,
            ReactEffect::AnyEffect => ReactEffect::AnyEffect,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunSig<Loc, T> {
    pub tparams: TParams<Loc, T>,
    pub params: Vec<FunParam<T>>,
    pub rest_param: Option<FunRestParam<Loc, T>>,
    pub this_param: Option<T>,
    pub return_: T,
    pub type_guard: Option<TypeGuard<Loc, T>>,
    pub effect_: ReactEffect<Loc>,
}

impl<Loc, T> FunSig<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        self.tparams.iter(cx, f_loc, f_t);
        for p in &self.params {
            f_t(cx, &p.t);
        }
        if let Some(rp) = &self.rest_param {
            f_loc(cx, &rp.loc);
            f_t(cx, &rp.t);
        }
        if let Some(t) = &self.this_param {
            f_t(cx, t);
        }
        f_t(cx, &self.return_);
        if let Some(tg) = &self.type_guard {
            f_loc(cx, &tg.loc);
            f_loc(cx, &tg.param_name.0);
            f_t(cx, &tg.type_guard);
        }
        self.effect_.iter(cx, f_loc);
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> FunSig<Loc2, T2> {
        FunSig {
            tparams: self.tparams.map(cx, &f_loc, &f_t),
            params: self.params.iter().map(|p| p.map(cx, &f_t)).collect(),
            rest_param: self.rest_param.as_ref().map(|rp| rp.map(cx, &f_loc, &f_t)),
            this_param: self.this_param.as_ref().map(|t| f_t(cx, t)),
            return_: f_t(cx, &self.return_),
            type_guard: self.type_guard.as_ref().map(|tg| tg.map(cx, &f_loc, &f_t)),
            effect_: self.effect_.map(cx, &f_loc),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ComponentParam<Loc, T> {
    pub name: FlowSmolStr,
    pub name_loc: Loc,
    pub t: T,
}

impl<Loc, T> ComponentParam<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ComponentParam<Loc2, T2> {
        ComponentParam {
            name: self.name.dupe(),
            name_loc: f_loc(cx, &self.name_loc),
            t: f_t(cx, &self.t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ComponentRestParam<T> {
    pub t: T,
}

impl<T> ComponentRestParam<T> {
    pub fn map<CX, T2>(
        &self,
        cx: &mut CX,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ComponentRestParam<T2> {
        ComponentRestParam {
            t: f_t(cx, &self.t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ComponentSig<Loc, T> {
    pub params_loc: Loc,
    pub tparams: TParams<Loc, T>,
    pub params: Vec<ComponentParam<Loc, T>>,
    pub rest_param: Option<ComponentRestParam<T>>,
    pub renders: T,
}

impl<Loc, T> ComponentSig<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        f_loc(cx, &self.params_loc);
        self.tparams.iter(cx, f_loc, f_t);
        for p in &self.params {
            f_loc(cx, &p.name_loc);
            f_t(cx, &p.t);
        }
        if let Some(rp) = &self.rest_param {
            f_t(cx, &rp.t);
        }
        f_t(cx, &self.renders);
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ComponentSig<Loc2, T2> {
        ComponentSig {
            params_loc: f_loc(cx, &self.params_loc),
            tparams: self.tparams.map(cx, &f_loc, &f_t),
            params: self
                .params
                .iter()
                .map(|p| p.map(cx, &f_loc, &f_t))
                .collect(),
            rest_param: self.rest_param.as_ref().map(|rp| rp.map(cx, &f_t)),
            renders: f_t(cx, &self.renders),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum TupleElement<Loc, T> {
    TupleElement {
        loc: Loc,
        name: Option<FlowSmolStr>,
        t: T,
        polarity: Polarity,
        optional: bool,
    },
    TupleSpread {
        loc: Loc,
        name: Option<FlowSmolStr>,
        t: T,
    },
}

impl<Loc, T> TupleElement<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> TupleElement<Loc2, T2> {
        match self {
            TupleElement::TupleElement {
                loc,
                name,
                t,
                polarity,
                optional,
            } => TupleElement::TupleElement {
                loc: f_loc(cx, loc),
                name: name.dupe(),
                t: f_t(cx, t),
                polarity: *polarity,
                optional: *optional,
            },
            TupleElement::TupleSpread { loc, name, t } => TupleElement::TupleSpread {
                loc: f_loc(cx, loc),
                name: name.dupe(),
                t: f_t(cx, t),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjAnnotProto<Loc, T> {
    ObjAnnotImplicitProto,
    ObjAnnotExplicitProto(Loc, T),
    ObjAnnotCallable { ts: Vec1<T> },
}

impl<Loc, T> ObjAnnotProto<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ObjAnnotProto<Loc2, T2> {
        match self {
            ObjAnnotProto::ObjAnnotImplicitProto => ObjAnnotProto::ObjAnnotImplicitProto,
            ObjAnnotProto::ObjAnnotExplicitProto(loc, t) => {
                ObjAnnotProto::ObjAnnotExplicitProto(f_loc(cx, loc), f_t(cx, t))
            }
            ObjAnnotProto::ObjAnnotCallable { ts } => ObjAnnotProto::ObjAnnotCallable {
                ts: ts.mapped_ref(|t| f_t(cx, t)),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Accessor<Loc, T> {
    Get(Loc, T),
    Set(Loc, T),
    GetSet(Loc, T, Loc, T),
}

impl<Loc, T> Accessor<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            Accessor::Get(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Accessor::Set(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Accessor::GetSet(loc1, t1, loc2, t2) => {
                f_loc(cx, loc1);
                f_t(cx, t1);
                f_loc(cx, loc2);
                f_t(cx, t2);
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> Accessor<Loc2, T2> {
        match self {
            Accessor::Get(loc, t) => Accessor::Get(f_loc(cx, loc), f_t(cx, t)),
            Accessor::Set(loc, t) => Accessor::Set(f_loc(cx, loc), f_t(cx, t)),
            Accessor::GetSet(loc1, t1, loc2, t2) => {
                Accessor::GetSet(f_loc(cx, loc1), f_t(cx, t1), f_loc(cx, loc2), f_t(cx, t2))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjValueProp<Loc, T> {
    ObjValueField(Loc, T, Polarity),
    ObjValueAccess(Accessor<Loc, T>),
    ObjValueMethod {
        id_loc: Loc,
        fn_loc: Loc,
        async_: bool,
        generator: bool,
        def: FunSig<Loc, T>,
    },
}

impl<Loc, T> ObjValueProp<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            ObjValueProp::ObjValueField(loc, t, _) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            ObjValueProp::ObjValueAccess(accessor) => accessor.iter(cx, f_loc, f_t),
            ObjValueProp::ObjValueMethod {
                id_loc,
                fn_loc,
                async_: _,
                generator: _,
                def,
            } => {
                f_loc(cx, id_loc);
                f_loc(cx, fn_loc);
                def.iter(cx, f_loc, f_t);
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ObjValueProp<Loc2, T2> {
        match self {
            ObjValueProp::ObjValueField(loc, t, pol) => {
                ObjValueProp::ObjValueField(f_loc(cx, loc), f_t(cx, t), *pol)
            }
            ObjValueProp::ObjValueAccess(accessor) => {
                ObjValueProp::ObjValueAccess(accessor.map(cx, &f_loc, &f_t))
            }
            ObjValueProp::ObjValueMethod {
                id_loc,
                fn_loc,
                async_,
                generator,
                def,
            } => ObjValueProp::ObjValueMethod {
                id_loc: f_loc(cx, id_loc),
                fn_loc: f_loc(cx, fn_loc),
                async_: *async_,
                generator: *generator,
                def: def.map(cx, &f_loc, &f_t),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjAnnotProp<Loc, T> {
    ObjAnnotField(Loc, T, Polarity),
    ObjAnnotAccess(Accessor<Loc, T>),
    ObjAnnotMethod {
        id_loc: Loc,
        fn_loc: Loc,
        def: FunSig<Loc, T>,
    },
}

impl<Loc, T> ObjAnnotProp<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ObjAnnotProp<Loc2, T2> {
        match self {
            ObjAnnotProp::ObjAnnotField(loc, t, pol) => {
                ObjAnnotProp::ObjAnnotField(f_loc(cx, loc), f_t(cx, t), *pol)
            }
            ObjAnnotProp::ObjAnnotAccess(accessor) => {
                ObjAnnotProp::ObjAnnotAccess(accessor.map(cx, &f_loc, &f_t))
            }
            ObjAnnotProp::ObjAnnotMethod {
                id_loc,
                fn_loc,
                def,
            } => ObjAnnotProp::ObjAnnotMethod {
                id_loc: f_loc(cx, id_loc),
                fn_loc: f_loc(cx, fn_loc),
                def: def.map(cx, &f_loc, &f_t),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum InterfaceProp<Loc, T> {
    InterfaceField(Option<Loc>, T, Polarity),
    InterfaceAccess(Accessor<Loc, T>),
    InterfaceMethod(Vec1<(Loc, Loc, FunSig<Loc, T>)>),
}

impl<Loc, T> InterfaceProp<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            InterfaceProp::InterfaceField(loc_opt, t, _) => {
                if let Some(loc) = loc_opt {
                    f_loc(cx, loc);
                }
                f_t(cx, t);
            }
            InterfaceProp::InterfaceAccess(accessor) => accessor.iter(cx, f_loc, f_t),
            InterfaceProp::InterfaceMethod(methods) => {
                for (loc1, loc2, sig) in methods.iter() {
                    f_loc(cx, loc1);
                    f_loc(cx, loc2);
                    sig.iter(cx, f_loc, f_t);
                }
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> InterfaceProp<Loc2, T2> {
        match self {
            InterfaceProp::InterfaceField(loc_opt, t, pol) => InterfaceProp::InterfaceField(
                loc_opt.as_ref().map(|loc| f_loc(cx, loc)),
                f_t(cx, t),
                *pol,
            ),
            InterfaceProp::InterfaceAccess(accessor) => {
                InterfaceProp::InterfaceAccess(accessor.map(cx, &f_loc, &f_t))
            }
            InterfaceProp::InterfaceMethod(methods) => {
                InterfaceProp::InterfaceMethod(methods.mapped_ref(|(loc1, loc2, sig)| {
                    (f_loc(cx, loc1), f_loc(cx, loc2), sig.map(cx, &f_loc, &f_t))
                }))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ObjAnnotDict<T> {
    pub name: Option<FlowSmolStr>,
    pub polarity: Polarity,
    pub key: T,
    pub value: T,
}

impl<T> ObjAnnotDict<T> {
    pub fn map<CX, T2>(&self, cx: &mut CX, f_t: impl Fn(&mut CX, &T) -> T2) -> ObjAnnotDict<T2> {
        ObjAnnotDict {
            name: self.name.dupe(),
            polarity: self.polarity,
            key: f_t(cx, &self.key),
            value: f_t(cx, &self.value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjSpreadAnnotElem<Loc, T> {
    ObjSpreadAnnotElem(T),
    ObjSpreadAnnotSlice {
        dict: Option<ObjAnnotDict<T>>,
        props: BTreeMap<FlowSmolStr, ObjAnnotProp<Loc, T>>,
        computed_props: Vec<(T, ObjAnnotProp<Loc, T>)>,
    },
}

impl<Loc, T> ObjSpreadAnnotElem<Loc, T> {
    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ObjSpreadAnnotElem<Loc2, T2> {
        match self {
            ObjSpreadAnnotElem::ObjSpreadAnnotElem(t) => {
                ObjSpreadAnnotElem::ObjSpreadAnnotElem(f_t(cx, t))
            }
            ObjSpreadAnnotElem::ObjSpreadAnnotSlice {
                dict,
                props,
                computed_props,
            } => ObjSpreadAnnotElem::ObjSpreadAnnotSlice {
                dict: dict.as_ref().map(|d| d.map(cx, &f_t)),
                props: props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                    .collect(),
                computed_props: computed_props
                    .iter()
                    .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                    .collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjValueSpreadElem<Loc, T> {
    ObjValueSpreadElem(T),
    ObjValueSpreadSlice(BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>),
}

impl<Loc, T> ObjValueSpreadElem<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            ObjValueSpreadElem::ObjValueSpreadElem(t) => f_t(cx, t),
            ObjValueSpreadElem::ObjValueSpreadSlice(props) => {
                for v in props.values() {
                    v.iter(cx, f_loc, f_t);
                }
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ObjValueSpreadElem<Loc2, T2> {
        match self {
            ObjValueSpreadElem::ObjValueSpreadElem(t) => {
                ObjValueSpreadElem::ObjValueSpreadElem(f_t(cx, t))
            }
            ObjValueSpreadElem::ObjValueSpreadSlice(props) => {
                ObjValueSpreadElem::ObjValueSpreadSlice(
                    props
                        .iter()
                        .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                        .collect(),
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ClassExtends<Loc, T> {
    ClassExplicitExtends { loc: Loc, t: T },
    ClassExplicitExtendsApp { loc: Loc, t: T, targs: Vec<T> },
    ClassImplicitExtends,
    ObjectPrototypeExtendsNull,
}

impl<Loc, T> ClassExtends<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            ClassExtends::ClassExplicitExtends { loc, t } => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            ClassExtends::ClassExplicitExtendsApp { loc, t, targs } => {
                f_loc(cx, loc);
                f_t(cx, t);
                for t in targs {
                    f_t(cx, t);
                }
            }
            ClassExtends::ClassImplicitExtends | ClassExtends::ObjectPrototypeExtendsNull => {}
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ClassExtends<Loc2, T2> {
        match self {
            ClassExtends::ClassExplicitExtends { loc, t } => ClassExtends::ClassExplicitExtends {
                loc: f_loc(cx, loc),
                t: f_t(cx, t),
            },
            ClassExtends::ClassExplicitExtendsApp { loc, t, targs } => {
                ClassExtends::ClassExplicitExtendsApp {
                    loc: f_loc(cx, loc),
                    t: f_t(cx, t),
                    targs: targs.iter().map(|t| f_t(cx, t)).collect(),
                }
            }
            ClassExtends::ClassImplicitExtends => ClassExtends::ClassImplicitExtends,
            ClassExtends::ObjectPrototypeExtendsNull => ClassExtends::ObjectPrototypeExtendsNull,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ClassMixins<Loc, T> {
    ClassMixin { loc: Loc, t: T },
    ClassMixinApp { loc: Loc, t: T, targs: Vec<T> },
}

impl<Loc, T> ClassMixins<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            ClassMixins::ClassMixin { loc, t } => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            ClassMixins::ClassMixinApp { loc, t, targs } => {
                f_loc(cx, loc);
                f_t(cx, t);
                for t in targs {
                    f_t(cx, t);
                }
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ClassMixins<Loc2, T2> {
        match self {
            ClassMixins::ClassMixin { loc, t } => ClassMixins::ClassMixin {
                loc: f_loc(cx, loc),
                t: f_t(cx, t),
            },
            ClassMixins::ClassMixinApp { loc, t, targs } => ClassMixins::ClassMixinApp {
                loc: f_loc(cx, loc),
                t: f_t(cx, t),
                targs: targs.iter().map(|t| f_t(cx, t)).collect(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ClassSig<Loc, T> {
    pub tparams: TParams<Loc, T>,
    pub extends: ClassExtends<Loc, T>,
    pub implements: Vec<T>,
    pub static_props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
    pub proto_props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
    pub own_props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
    pub dict: Option<ObjAnnotDict<T>>,
}

impl<Loc, T> ClassSig<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        self.tparams.iter(cx, f_loc, f_t);
        self.extends.iter(cx, f_loc, f_t);
        for t in &self.implements {
            f_t(cx, t);
        }
        for v in self.static_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for v in self.proto_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for v in self.own_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        if let Some(ref d) = self.dict {
            f_t(cx, &d.key);
            f_t(cx, &d.value);
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> ClassSig<Loc2, T2> {
        ClassSig {
            tparams: self.tparams.map(cx, &f_loc, &f_t),
            extends: self.extends.map(cx, &f_loc, &f_t),
            implements: self.implements.iter().map(|t| f_t(cx, t)).collect(),
            static_props: self
                .static_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            proto_props: self
                .proto_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            own_props: self
                .own_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            dict: self.dict.as_ref().map(|d| d.map(cx, &f_t)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DeclareClassSig<Loc, T> {
    pub tparams: TParams<Loc, T>,
    pub extends: ClassExtends<Loc, T>,
    pub mixins: Vec<ClassMixins<Loc, T>>,
    pub implements: Vec<T>,
    pub static_props: BTreeMap<FlowSmolStr, InterfaceProp<Loc, T>>,
    pub own_props: BTreeMap<FlowSmolStr, InterfaceProp<Loc, T>>,
    pub proto_props: BTreeMap<FlowSmolStr, InterfaceProp<Loc, T>>,
    pub computed_own_props: Vec<(T, InterfaceProp<Loc, T>)>,
    pub computed_proto_props: Vec<(T, InterfaceProp<Loc, T>)>,
    pub computed_static_props: Vec<(T, InterfaceProp<Loc, T>)>,
    pub static_calls: Vec<T>,
    pub calls: Vec<T>,
    pub dict: Option<ObjAnnotDict<T>>,
    pub static_dict: Option<ObjAnnotDict<T>>,
}

impl<Loc, T> DeclareClassSig<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        self.tparams.iter(cx, f_loc, f_t);
        self.extends.iter(cx, f_loc, f_t);
        for m in &self.mixins {
            m.iter(cx, f_loc, f_t);
        }
        for t in &self.implements {
            f_t(cx, t);
        }
        for v in self.static_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for v in self.own_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for v in self.proto_props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for (t, prop) in &self.computed_own_props {
            f_t(cx, t);
            prop.iter(cx, f_loc, f_t);
        }
        for (t, prop) in &self.computed_proto_props {
            f_t(cx, t);
            prop.iter(cx, f_loc, f_t);
        }
        for (t, prop) in &self.computed_static_props {
            f_t(cx, t);
            prop.iter(cx, f_loc, f_t);
        }
        for t in &self.static_calls {
            f_t(cx, t);
        }
        for t in &self.calls {
            f_t(cx, t);
        }
        if let Some(d) = &self.dict {
            f_t(cx, &d.key);
            f_t(cx, &d.value);
        }
        if let Some(d) = &self.static_dict {
            f_t(cx, &d.key);
            f_t(cx, &d.value);
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> DeclareClassSig<Loc2, T2> {
        DeclareClassSig {
            tparams: self.tparams.map(cx, &f_loc, &f_t),
            extends: self.extends.map(cx, &f_loc, &f_t),
            mixins: self
                .mixins
                .iter()
                .map(|m| m.map(cx, &f_loc, &f_t))
                .collect(),
            implements: self.implements.iter().map(|t| f_t(cx, t)).collect(),
            static_props: self
                .static_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            own_props: self
                .own_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            proto_props: self
                .proto_props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            computed_own_props: self
                .computed_own_props
                .iter()
                .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                .collect(),
            computed_proto_props: self
                .computed_proto_props
                .iter()
                .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                .collect(),
            computed_static_props: self
                .computed_static_props
                .iter()
                .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                .collect(),
            static_calls: self.static_calls.iter().map(|t| f_t(cx, t)).collect(),
            calls: self.calls.iter().map(|t| f_t(cx, t)).collect(),
            dict: self.dict.as_ref().map(|d| d.map(cx, &f_t)),
            static_dict: self.static_dict.as_ref().map(|d| d.map(cx, &f_t)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct InterfaceSig<Loc, T> {
    pub extends: Vec<T>,
    pub props: BTreeMap<FlowSmolStr, InterfaceProp<Loc, T>>,
    pub computed_props: Vec<(T, InterfaceProp<Loc, T>)>,
    pub calls: Vec<T>,
    pub dict: Option<ObjAnnotDict<T>>,
}

impl<Loc, T> InterfaceSig<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        for t in &self.extends {
            f_t(cx, t);
        }
        for v in self.props.values() {
            v.iter(cx, f_loc, f_t);
        }
        for (t, prop) in &self.computed_props {
            f_t(cx, t);
            prop.iter(cx, f_loc, f_t);
        }
        for t in &self.calls {
            f_t(cx, t);
        }
        if let Some(d) = &self.dict {
            f_t(cx, &d.key);
            f_t(cx, &d.value);
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> InterfaceSig<Loc2, T2> {
        InterfaceSig {
            extends: self.extends.iter().map(|t| f_t(cx, t)).collect(),
            props: self
                .props
                .iter()
                .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                .collect(),
            computed_props: self
                .computed_props
                .iter()
                .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                .collect(),
            calls: self.calls.iter().map(|t| f_t(cx, t)).collect(),
            dict: self.dict.as_ref().map(|d| d.map(cx, &f_t)),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum EnumRep {
    BoolRep(Option<bool>),
    NumberRep { truthy: bool },
    StringRep { truthy: bool },
    SymbolRep,
    BigIntRep { truthy: bool },
}

// Definitions represent the structure of things which can be found when
// resolving a name, or as in the case of exports, a module reference.
//
// The signature format is designed to exploit these forms of indirection to
// preserve sharing and minimize size. Two references to a given definition will
// always be represented as pointers to a shared signature of that definition.

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefTypeAlias<Loc, T> {
    pub id_loc: Loc,
    pub custom_error_loc_opt: Option<Loc>,
    pub name: FlowSmolStr,
    pub tparams: TParams<Loc, T>,
    pub body: T,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefOpaqueType<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub tparams: TParams<Loc, T>,
    pub lower_bound: Option<T>,
    pub upper_bound: Option<T>,
    pub body: Option<T>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefInterface<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub tparams: TParams<Loc, T>,
    pub def: InterfaceSig<Loc, T>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefClassBinding<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub def: ClassSig<Loc, T>,
    pub namespace_types: BTreeMap<FlowSmolStr, (Loc, T)>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefDeclareClassBinding<Loc, T> {
    pub id_loc: Loc,
    pub nominal_id_loc: Loc,
    pub name: FlowSmolStr,
    pub def: DeclareClassSig<Loc, T>,
    pub namespace_types: BTreeMap<FlowSmolStr, (Loc, T)>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefRecordBinding<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub def: ClassSig<Loc, T>,
    pub defaulted_props: BTreeSet<FlowSmolStr>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefDisabledRecordBinding<Loc> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefFunBinding<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub async_: bool,
    pub generator: bool,
    pub fn_loc: Loc,
    pub def: FunSig<Loc, T>,
    pub statics: BTreeMap<FlowSmolStr, (Loc, T)>,
    pub namespace_types: BTreeMap<FlowSmolStr, (Loc, T)>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefDeclareFun<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub fn_loc: Loc,
    pub def: FunSig<Loc, T>,
    pub statics: BTreeMap<FlowSmolStr, (Loc, T)>,
    pub namespace_types: BTreeMap<FlowSmolStr, (Loc, T)>,
    pub tail: Vec<(Loc, Loc, FunSig<Loc, T>)>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefComponentBinding<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub fn_loc: Loc,
    pub def: ComponentSig<Loc, T>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefDisabledComponentBinding<Loc> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefVariable<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub def: T,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefParameter<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub def: T,
    pub tparams: TParams<Loc, T>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefEnumBinding<Loc> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub rep: Option<EnumRep>,
    pub members: BTreeMap<FlowSmolStr, Loc>,
    pub has_unknown_members: bool,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefDisabledEnumBinding<Loc> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub struct DefNamespaceBinding<Loc, T> {
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub values: BTreeMap<FlowSmolStr, (Loc, T)>,
    pub types: BTreeMap<FlowSmolStr, (Loc, T)>,
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Def<Loc, T> {
    TypeAlias(Box<DefTypeAlias<Loc, T>>),
    OpaqueType(Box<DefOpaqueType<Loc, T>>),
    Interface(Box<DefInterface<Loc, T>>),
    ClassBinding(Box<DefClassBinding<Loc, T>>),
    DeclareClassBinding(Box<DefDeclareClassBinding<Loc, T>>),
    RecordBinding(Box<DefRecordBinding<Loc, T>>),
    DisabledRecordBinding(Box<DefDisabledRecordBinding<Loc>>),
    FunBinding(Box<DefFunBinding<Loc, T>>),
    DeclareFun(Box<DefDeclareFun<Loc, T>>),
    ComponentBinding(Box<DefComponentBinding<Loc, T>>),
    DisabledComponentBinding(Box<DefDisabledComponentBinding<Loc>>),
    Variable(Box<DefVariable<Loc, T>>),
    Parameter(Box<DefParameter<Loc, T>>),
    EnumBinding(Box<DefEnumBinding<Loc>>),
    DisabledEnumBinding(Box<DefDisabledEnumBinding<Loc>>),
    NamespaceBinding(Box<DefNamespaceBinding<Loc, T>>),
}

impl<Loc: Clone, T> Def<Loc, T> {
    // These accessors will compile to code that does not have a branch because
    // id_loc and name have the same offset for each constructor.
    pub fn id_loc(&self) -> Loc {
        match self {
            Def::TypeAlias(inner) => inner.id_loc.clone(),
            Def::OpaqueType(inner) => inner.id_loc.clone(),
            Def::Interface(inner) => inner.id_loc.clone(),
            Def::ClassBinding(inner) => inner.id_loc.clone(),
            Def::DeclareClassBinding(inner) => inner.id_loc.clone(),
            Def::RecordBinding(inner) => inner.id_loc.clone(),
            Def::DisabledRecordBinding(inner) => inner.id_loc.clone(),
            Def::FunBinding(inner) => inner.id_loc.clone(),
            Def::DeclareFun(inner) => inner.id_loc.clone(),
            Def::ComponentBinding(inner) => inner.id_loc.clone(),
            Def::DisabledComponentBinding(inner) => inner.id_loc.clone(),
            Def::Variable(inner) => inner.id_loc.clone(),
            Def::Parameter(inner) => inner.id_loc.clone(),
            Def::EnumBinding(inner) => inner.id_loc.clone(),
            Def::DisabledEnumBinding(inner) => inner.id_loc.clone(),
            Def::NamespaceBinding(inner) => inner.id_loc.clone(),
        }
    }

    pub fn name(&self) -> &FlowSmolStr {
        match self {
            Def::TypeAlias(inner) => &inner.name,
            Def::OpaqueType(inner) => &inner.name,
            Def::Interface(inner) => &inner.name,
            Def::ClassBinding(inner) => &inner.name,
            Def::DeclareClassBinding(inner) => &inner.name,
            Def::RecordBinding(inner) => &inner.name,
            Def::DisabledRecordBinding(inner) => &inner.name,
            Def::FunBinding(inner) => &inner.name,
            Def::DeclareFun(inner) => &inner.name,
            Def::ComponentBinding(inner) => &inner.name,
            Def::DisabledComponentBinding(inner) => &inner.name,
            Def::Variable(inner) => &inner.name,
            Def::Parameter(inner) => &inner.name,
            Def::EnumBinding(inner) => &inner.name,
            Def::DisabledEnumBinding(inner) => &inner.name,
            Def::NamespaceBinding(inner) => &inner.name,
        }
    }

    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            Def::TypeAlias(inner) => {
                f_loc(cx, &inner.id_loc);
                if let Some(loc) = &inner.custom_error_loc_opt {
                    f_loc(cx, loc);
                }
                inner.tparams.iter(cx, f_loc, f_t);
                f_t(cx, &inner.body);
            }
            Def::OpaqueType(inner) => {
                f_loc(cx, &inner.id_loc);
                inner.tparams.iter(cx, f_loc, f_t);
                if let Some(t) = &inner.lower_bound {
                    f_t(cx, t);
                }
                if let Some(t) = &inner.upper_bound {
                    f_t(cx, t);
                }
                if let Some(t) = &inner.body {
                    f_t(cx, t);
                }
            }
            Def::Interface(inner) => {
                f_loc(cx, &inner.id_loc);
                inner.tparams.iter(cx, f_loc, f_t);
                inner.def.iter(cx, f_loc, f_t);
            }
            Def::ClassBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                inner.def.iter(cx, f_loc, f_t);
                for (loc, t) in inner.namespace_types.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Def::DeclareClassBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                f_loc(cx, &inner.nominal_id_loc);
                inner.def.iter(cx, f_loc, f_t);
                for (loc, t) in inner.namespace_types.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Def::RecordBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                inner.def.iter(cx, f_loc, f_t);
            }
            Def::DisabledRecordBinding(inner) => {
                f_loc(cx, &inner.id_loc);
            }
            Def::FunBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                f_loc(cx, &inner.fn_loc);
                inner.def.iter(cx, f_loc, f_t);
                for (loc, t) in inner.statics.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for (loc, t) in inner.namespace_types.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Def::DeclareFun(inner) => {
                f_loc(cx, &inner.id_loc);
                f_loc(cx, &inner.fn_loc);
                inner.def.iter(cx, f_loc, f_t);
                for (loc, t) in inner.statics.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for (loc, t) in inner.namespace_types.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for (loc1, loc2, sig) in &inner.tail {
                    f_loc(cx, loc1);
                    f_loc(cx, loc2);
                    sig.iter(cx, f_loc, f_t);
                }
            }
            Def::ComponentBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                f_loc(cx, &inner.fn_loc);
                inner.def.iter(cx, f_loc, f_t);
            }
            Def::DisabledComponentBinding(inner) => {
                f_loc(cx, &inner.id_loc);
            }
            Def::Variable(inner) => {
                f_loc(cx, &inner.id_loc);
                f_t(cx, &inner.def);
            }
            Def::Parameter(inner) => {
                f_loc(cx, &inner.id_loc);
                f_t(cx, &inner.def);
                inner.tparams.iter(cx, f_loc, f_t);
            }
            Def::EnumBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                for loc in inner.members.values() {
                    f_loc(cx, loc);
                }
            }
            Def::DisabledEnumBinding(inner) => {
                f_loc(cx, &inner.id_loc);
            }
            Def::NamespaceBinding(inner) => {
                f_loc(cx, &inner.id_loc);
                for (loc, t) in inner.values.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for (loc, t) in inner.types.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> Def<Loc2, T2> {
        match self {
            Def::TypeAlias(inner) => Def::TypeAlias(Box::new(DefTypeAlias {
                id_loc: f_loc(cx, &inner.id_loc),
                custom_error_loc_opt: inner
                    .custom_error_loc_opt
                    .as_ref()
                    .map(|loc| f_loc(cx, loc)),
                name: inner.name.dupe(),
                tparams: inner.tparams.map(cx, &f_loc, &f_t),
                body: f_t(cx, &inner.body),
            })),
            Def::OpaqueType(inner) => Def::OpaqueType(Box::new(DefOpaqueType {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                tparams: inner.tparams.map(cx, &f_loc, &f_t),
                lower_bound: inner.lower_bound.as_ref().map(|t| f_t(cx, t)),
                upper_bound: inner.upper_bound.as_ref().map(|t| f_t(cx, t)),
                body: inner.body.as_ref().map(|t| f_t(cx, t)),
            })),
            Def::Interface(inner) => Def::Interface(Box::new(DefInterface {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                tparams: inner.tparams.map(cx, &f_loc, &f_t),
                def: inner.def.map(cx, &f_loc, &f_t),
            })),
            Def::ClassBinding(inner) => Def::ClassBinding(Box::new(DefClassBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                def: inner.def.map(cx, &f_loc, &f_t),
                namespace_types: inner
                    .namespace_types
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            })),
            Def::DeclareClassBinding(inner) => {
                Def::DeclareClassBinding(Box::new(DefDeclareClassBinding {
                    id_loc: f_loc(cx, &inner.id_loc),
                    nominal_id_loc: f_loc(cx, &inner.nominal_id_loc),
                    name: inner.name.dupe(),
                    def: inner.def.map(cx, &f_loc, &f_t),
                    namespace_types: inner
                        .namespace_types
                        .iter()
                        .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                        .collect(),
                }))
            }
            Def::RecordBinding(inner) => Def::RecordBinding(Box::new(DefRecordBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                def: inner.def.map(cx, &f_loc, &f_t),
                defaulted_props: inner.defaulted_props.clone(),
            })),
            Def::DisabledRecordBinding(inner) => {
                Def::DisabledRecordBinding(Box::new(DefDisabledRecordBinding {
                    id_loc: f_loc(cx, &inner.id_loc),
                    name: inner.name.dupe(),
                }))
            }
            Def::FunBinding(inner) => Def::FunBinding(Box::new(DefFunBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                async_: inner.async_,
                generator: inner.generator,
                fn_loc: f_loc(cx, &inner.fn_loc),
                def: inner.def.map(cx, &f_loc, &f_t),
                statics: inner
                    .statics
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
                namespace_types: inner
                    .namespace_types
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            })),
            Def::DeclareFun(inner) => Def::DeclareFun(Box::new(DefDeclareFun {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                fn_loc: f_loc(cx, &inner.fn_loc),
                def: inner.def.map(cx, &f_loc, &f_t),
                statics: inner
                    .statics
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
                namespace_types: inner
                    .namespace_types
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
                tail: inner
                    .tail
                    .iter()
                    .map(|(loc1, loc2, sig)| {
                        (f_loc(cx, loc1), f_loc(cx, loc2), sig.map(cx, &f_loc, &f_t))
                    })
                    .collect(),
            })),
            Def::ComponentBinding(inner) => Def::ComponentBinding(Box::new(DefComponentBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                fn_loc: f_loc(cx, &inner.fn_loc),
                def: inner.def.map(cx, &f_loc, &f_t),
            })),
            Def::DisabledComponentBinding(inner) => {
                Def::DisabledComponentBinding(Box::new(DefDisabledComponentBinding {
                    id_loc: f_loc(cx, &inner.id_loc),
                    name: inner.name.dupe(),
                }))
            }
            Def::Variable(inner) => Def::Variable(Box::new(DefVariable {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                def: f_t(cx, &inner.def),
            })),
            Def::Parameter(inner) => Def::Parameter(Box::new(DefParameter {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                def: f_t(cx, &inner.def),
                tparams: inner.tparams.map(cx, &f_loc, &f_t),
            })),
            Def::EnumBinding(inner) => Def::EnumBinding(Box::new(DefEnumBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                rep: inner.rep,
                members: inner
                    .members
                    .iter()
                    .map(|(k, loc)| (k.dupe(), f_loc(cx, loc)))
                    .collect(),
                has_unknown_members: inner.has_unknown_members,
            })),
            Def::DisabledEnumBinding(inner) => {
                Def::DisabledEnumBinding(Box::new(DefDisabledEnumBinding {
                    id_loc: f_loc(cx, &inner.id_loc),
                    name: inner.name.dupe(),
                }))
            }
            Def::NamespaceBinding(inner) => Def::NamespaceBinding(Box::new(DefNamespaceBinding {
                id_loc: f_loc(cx, &inner.id_loc),
                name: inner.name.dupe(),
                values: inner
                    .values
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
                types: inner
                    .types
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            })),
        }
    }
}

// The signature extractor relies heavily on annotations, but will extract
// signatures corresponding to some literal expressions as well. The
// representation of these things are kept distinct from annotations, below.

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ValueFunExpr<Loc, T> {
    pub loc: Loc,
    pub async_: bool,
    pub generator: bool,
    pub def: FunSig<Loc, T>,
    pub statics: BTreeMap<FlowSmolStr, (Loc, T)>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ValueDeclareModuleImplicitlyExportedObject<Loc, T> {
    pub loc: Loc,
    pub module_name: flow_import_specifier::Userland,
    pub props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ValueObjLit<Loc, T> {
    pub loc: Loc,
    pub frozen: bool,
    pub proto: Option<(Loc, T)>,
    pub props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ValueObjSpreadLit<Loc, T> {
    pub loc: Loc,
    pub frozen: bool,
    pub proto: Option<(Loc, T)>,
    pub elems: Vec1<ObjValueSpreadElem<Loc, T>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Value<Loc, T> {
    ClassExpr(Box<(Loc, ClassSig<Loc, T>)>),
    FunExpr(Box<ValueFunExpr<Loc, T>>),
    StringVal(Loc),
    StringLit(Box<(Loc, FlowSmolStr)>),
    NumberVal(Loc),
    NumberLit(Box<(Loc, f64, FlowSmolStr)>),
    BigIntVal(Loc),
    BigIntLit(Box<(Loc, Option<i64>, FlowSmolStr)>),
    BooleanVal(Loc),
    BooleanLit(Loc, bool),
    NullLit(Loc),
    DeclareModuleImplicitlyExportedObject(Box<ValueDeclareModuleImplicitlyExportedObject<Loc, T>>),
    ObjLit(Box<ValueObjLit<Loc, T>>),
    ObjSpreadLit(Box<ValueObjSpreadLit<Loc, T>>),
    EmptyConstArrayLit(Loc),
    ArrayLit(Box<(Loc, T, Vec<T>)>),
    AsConst(Box<Value<Loc, T>>),
}

impl<Loc: std::hash::Hash, T: std::hash::Hash> std::hash::Hash for Value<Loc, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::ClassExpr(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Value::FunExpr(inner) => {
                inner.loc.hash(state);
                inner.async_.hash(state);
                inner.generator.hash(state);
                inner.def.hash(state);
                inner.statics.hash(state);
            }
            Value::StringVal(loc) => loc.hash(state),
            Value::StringLit(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Value::NumberVal(loc) => loc.hash(state),
            Value::NumberLit(inner) => {
                inner.0.hash(state);
                inner.1.to_bits().hash(state);
                inner.2.hash(state);
            }
            Value::BigIntVal(loc) => loc.hash(state),
            Value::BigIntLit(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
                inner.2.hash(state);
            }
            Value::BooleanVal(loc) => loc.hash(state),
            Value::BooleanLit(loc, b) => {
                loc.hash(state);
                b.hash(state);
            }
            Value::NullLit(loc) => loc.hash(state),
            Value::DeclareModuleImplicitlyExportedObject(inner) => {
                inner.loc.hash(state);
                inner.module_name.hash(state);
                inner.props.hash(state);
            }
            Value::ObjLit(inner) => {
                inner.loc.hash(state);
                inner.frozen.hash(state);
                inner.proto.hash(state);
                inner.props.hash(state);
            }
            Value::ObjSpreadLit(inner) => {
                inner.loc.hash(state);
                inner.frozen.hash(state);
                inner.proto.hash(state);
                inner.elems.hash(state);
            }
            Value::EmptyConstArrayLit(loc) => loc.hash(state),
            Value::ArrayLit(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
                inner.2.hash(state);
            }
            Value::AsConst(v) => v.hash(state),
        }
    }
}

impl<Loc, T> Value<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            Value::ClassExpr(inner) => {
                f_loc(cx, &inner.0);
                inner.1.iter(cx, f_loc, f_t);
            }
            Value::FunExpr(inner) => {
                f_loc(cx, &inner.loc);
                inner.def.iter(cx, f_loc, f_t);
                for (loc, t) in inner.statics.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Value::StringVal(loc) => f_loc(cx, loc),
            Value::StringLit(inner) => f_loc(cx, &inner.0),
            Value::NumberVal(loc) => f_loc(cx, loc),
            Value::NumberLit(inner) => f_loc(cx, &inner.0),
            Value::BigIntVal(loc) => f_loc(cx, loc),
            Value::BigIntLit(inner) => f_loc(cx, &inner.0),
            Value::BooleanVal(loc) => f_loc(cx, loc),
            Value::BooleanLit(loc, _) => f_loc(cx, loc),
            Value::NullLit(loc) => f_loc(cx, loc),
            Value::DeclareModuleImplicitlyExportedObject(inner) => {
                f_loc(cx, &inner.loc);
                for v in inner.props.values() {
                    v.iter(cx, f_loc, f_t);
                }
            }
            Value::ObjLit(inner) => {
                f_loc(cx, &inner.loc);
                if let Some((loc, t)) = &inner.proto {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for v in inner.props.values() {
                    v.iter(cx, f_loc, f_t);
                }
            }
            Value::ObjSpreadLit(inner) => {
                f_loc(cx, &inner.loc);
                if let Some((loc, t)) = &inner.proto {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for elem in inner.elems.iter() {
                    elem.iter(cx, f_loc, f_t);
                }
            }
            Value::EmptyConstArrayLit(loc) => f_loc(cx, loc),
            Value::ArrayLit(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
                for t in &inner.2 {
                    f_t(cx, t);
                }
            }
            Value::AsConst(v) => v.iter(cx, f_loc, f_t),
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: &dyn Fn(&mut CX, &Loc) -> Loc2,
        f_t: &dyn Fn(&mut CX, &T) -> T2,
    ) -> Value<Loc2, T2> {
        match self {
            Value::ClassExpr(inner) => {
                Value::ClassExpr(Box::new((f_loc(cx, &inner.0), inner.1.map(cx, f_loc, f_t))))
            }
            Value::FunExpr(inner) => Value::FunExpr(Box::new(ValueFunExpr {
                loc: f_loc(cx, &inner.loc),
                async_: inner.async_,
                generator: inner.generator,
                def: inner.def.map(cx, f_loc, f_t),
                statics: inner
                    .statics
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            })),
            Value::StringVal(loc) => Value::StringVal(f_loc(cx, loc)),
            Value::StringLit(inner) => {
                Value::StringLit(Box::new((f_loc(cx, &inner.0), inner.1.dupe())))
            }
            Value::NumberVal(loc) => Value::NumberVal(f_loc(cx, loc)),
            Value::NumberLit(inner) => {
                Value::NumberLit(Box::new((f_loc(cx, &inner.0), inner.1, inner.2.dupe())))
            }
            Value::BigIntVal(loc) => Value::BigIntVal(f_loc(cx, loc)),
            Value::BigIntLit(inner) => {
                Value::BigIntLit(Box::new((f_loc(cx, &inner.0), inner.1, inner.2.dupe())))
            }
            Value::BooleanVal(loc) => Value::BooleanVal(f_loc(cx, loc)),
            Value::BooleanLit(loc, b) => Value::BooleanLit(f_loc(cx, loc), *b),
            Value::NullLit(loc) => Value::NullLit(f_loc(cx, loc)),
            Value::DeclareModuleImplicitlyExportedObject(inner) => {
                Value::DeclareModuleImplicitlyExportedObject(Box::new(
                    ValueDeclareModuleImplicitlyExportedObject {
                        loc: f_loc(cx, &inner.loc),
                        module_name: inner.module_name.clone(),
                        props: inner
                            .props
                            .iter()
                            .map(|(k, v)| (k.dupe(), v.map(cx, f_loc, f_t)))
                            .collect(),
                    },
                ))
            }
            Value::ObjLit(inner) => Value::ObjLit(Box::new(ValueObjLit {
                loc: f_loc(cx, &inner.loc),
                frozen: inner.frozen,
                proto: inner
                    .proto
                    .as_ref()
                    .map(|(loc, t)| (f_loc(cx, loc), f_t(cx, t))),
                props: inner
                    .props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, f_loc, f_t)))
                    .collect(),
            })),
            Value::ObjSpreadLit(inner) => Value::ObjSpreadLit(Box::new(ValueObjSpreadLit {
                loc: f_loc(cx, &inner.loc),
                frozen: inner.frozen,
                proto: inner
                    .proto
                    .as_ref()
                    .map(|(loc, t)| (f_loc(cx, loc), f_t(cx, t))),
                elems: inner.elems.mapped_ref(|elem| elem.map(cx, f_loc, f_t)),
            })),
            Value::EmptyConstArrayLit(loc) => Value::EmptyConstArrayLit(f_loc(cx, loc)),
            Value::ArrayLit(inner) => Value::ArrayLit(Box::new((
                f_loc(cx, &inner.0),
                f_t(cx, &inner.1),
                inner.2.iter().map(|t| f_t(cx, t)).collect(),
            ))),
            Value::AsConst(v) => Value::AsConst(Box::new(v.map(cx, &f_loc, &f_t))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ObjKind<T> {
    ExactObj,
    InexactObj,
    IndexedObj(ObjAnnotDict<T>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotUnion<Loc, T> {
    pub loc: Loc,
    pub t0: T,
    pub t1: T,
    pub ts: Vec<T>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotIntersection<Loc, T> {
    pub loc: Loc,
    pub t0: T,
    pub t1: T,
    pub ts: Vec<T>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotTuple<Loc, T> {
    pub loc: Loc,
    pub elems: Vec<TupleElement<Loc, T>>,
    pub inexact: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotStringPrefix<Loc, T> {
    pub loc: Loc,
    pub prefix: FlowSmolStr,
    pub remainder: Option<T>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotStringSuffix<Loc, T> {
    pub loc: Loc,
    pub suffix: FlowSmolStr,
    pub remainder: Option<T>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotTypeof<Loc, T> {
    pub loc: Loc,
    pub qname: Vec<FlowSmolStr>,
    pub t: T,
    pub targs: Option<Vec<T>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotBound<Loc> {
    pub ref_loc: Loc,
    pub name: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotPropertyType<Loc, T> {
    pub loc: Loc,
    pub obj: T,
    pub prop: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotElementType<Loc, T> {
    pub loc: Loc,
    pub obj: T,
    pub elem: T,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotOptionalIndexedAccessNonMaybeType<Loc, T> {
    pub loc: Loc,
    pub obj: T,
    pub index: T,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotOptionalIndexedAccessResultType<Loc, T> {
    pub loc: Loc,
    pub non_maybe_result: T,
    pub void_loc: Loc,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotRenders<Loc, T> {
    pub loc: Loc,
    pub arg: T,
    pub variant: ast::types::RendersVariant,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotConditional<Loc, T> {
    pub loc: Loc,
    pub distributive_tparam: Option<TParam<Loc, T>>,
    pub infer_tparams: TParams<Loc, T>,
    pub check_type: T,
    pub extends_type: T,
    pub true_type: T,
    pub false_type: T,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotObjKeyMirror<Loc, T> {
    pub loc: Loc,
    pub obj: T,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotMappedTypeAnnot<Loc, T> {
    pub loc: Loc,
    pub source_type: T,
    pub property_type: T,
    pub key_tparam: TParam<Loc, T>,
    pub variance: Polarity,
    pub variance_op: Option<ast::types::object::MappedTypeVarianceOp>,
    pub optional: ast::types::object::MappedTypeOptionalFlag,
    pub inline_keyof: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotObjAnnot<Loc, T> {
    pub loc: Loc,
    pub obj_kind: ObjKind<T>,
    pub props: BTreeMap<FlowSmolStr, ObjAnnotProp<Loc, T>>,
    pub computed_props: Vec<(T, ObjAnnotProp<Loc, T>)>,
    pub proto: ObjAnnotProto<Loc, T>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnnotObjSpreadAnnot<Loc, T> {
    pub loc: Loc,
    pub exact: bool,
    pub elems: Vec1<ObjSpreadAnnotElem<Loc, T>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Annot<Loc, T> {
    Any(Loc),
    Mixed(Loc),
    Empty(Loc),
    Void(Loc),
    Null(Loc),
    Symbol(Loc),
    UniqueSymbol(Loc),
    Number(Loc),
    BigInt(Loc),
    String(Loc),
    Boolean(Loc),
    Exists(Loc),
    Optional(T),
    Maybe(Box<(Loc, T)>),
    Union(Box<AnnotUnion<Loc, T>>),
    Intersection(Box<AnnotIntersection<Loc, T>>),
    Tuple(Box<AnnotTuple<Loc, T>>),
    Array(Box<(Loc, T)>),
    ReadOnlyArray(Box<(Loc, T)>),
    SingletonString(Box<(Loc, FlowSmolStr)>),
    SingletonNumber(Box<(Loc, f64, FlowSmolStr)>),
    SingletonBigInt(Box<(Loc, Option<i64>, FlowSmolStr)>),
    SingletonBoolean(Loc, bool),
    StringPrefix(Box<AnnotStringPrefix<Loc, T>>),
    StringSuffix(Box<AnnotStringSuffix<Loc, T>>),
    Typeof(Box<AnnotTypeof<Loc, T>>),
    Bound(Box<AnnotBound<Loc>>),
    NoInfer(T),
    PropertyType(Box<AnnotPropertyType<Loc, T>>),
    ElementType(Box<AnnotElementType<Loc, T>>),
    EnumValue(Box<(Loc, T)>),
    Enum(Box<(Loc, T)>),
    OptionalIndexedAccessNonMaybeType(Box<AnnotOptionalIndexedAccessNonMaybeType<Loc, T>>),
    OptionalIndexedAccessResultType(Box<AnnotOptionalIndexedAccessResultType<Loc, T>>),
    NonMaybeType(Box<(Loc, T)>),
    Omit(Box<(Loc, T, T)>),
    ReadOnly(Box<(Loc, T)>),
    Partial(Box<(Loc, T)>),
    Required(Box<(Loc, T)>),
    Keys(Box<(Loc, T)>),
    Renders(Box<AnnotRenders<Loc, T>>),
    ComponentMissingRenders(Loc),
    Values(Box<(Loc, T)>),
    Exact(Box<(Loc, T)>),
    ExportsT(Box<(Loc, flow_import_specifier::Userland)>),
    Conditional(Box<AnnotConditional<Loc, T>>),
    ObjKeyMirror(Box<AnnotObjKeyMirror<Loc, T>>),
    ClassT(Box<(Loc, T)>),
    FunctionBind(Loc),
    ReactElementConfig(Box<(Loc, T)>),
    FunAnnot(Box<(Loc, FunSig<Loc, T>)>),
    ComponentAnnot(Box<(Loc, ComponentSig<Loc, T>)>),
    MappedTypeAnnot(Box<AnnotMappedTypeAnnot<Loc, T>>),
    ObjAnnot(Box<AnnotObjAnnot<Loc, T>>),
    ObjSpreadAnnot(Box<AnnotObjSpreadAnnot<Loc, T>>),
    InlineInterface(Box<(Loc, InterfaceSig<Loc, T>)>),
}

impl<Loc: std::hash::Hash, T: std::hash::Hash> std::hash::Hash for Annot<Loc, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Annot::Any(l)
            | Annot::Mixed(l)
            | Annot::Empty(l)
            | Annot::Void(l)
            | Annot::Null(l)
            | Annot::Symbol(l)
            | Annot::UniqueSymbol(l)
            | Annot::Number(l)
            | Annot::BigInt(l)
            | Annot::String(l)
            | Annot::Boolean(l)
            | Annot::Exists(l) => l.hash(state),
            Annot::Optional(t) => t.hash(state),
            Annot::Maybe(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::Union(inner) => {
                inner.loc.hash(state);
                inner.t0.hash(state);
                inner.t1.hash(state);
                inner.ts.hash(state);
            }
            Annot::Intersection(inner) => {
                inner.loc.hash(state);
                inner.t0.hash(state);
                inner.t1.hash(state);
                inner.ts.hash(state);
            }
            Annot::Tuple(inner) => {
                inner.loc.hash(state);
                inner.elems.hash(state);
                inner.inexact.hash(state);
            }
            Annot::Array(inner) | Annot::ReadOnlyArray(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::SingletonString(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::SingletonNumber(inner) => {
                inner.0.hash(state);
                inner.1.to_bits().hash(state);
                inner.2.hash(state);
            }
            Annot::SingletonBigInt(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
                inner.2.hash(state);
            }
            Annot::SingletonBoolean(l, b) => {
                l.hash(state);
                b.hash(state);
            }
            Annot::StringPrefix(inner) => {
                inner.loc.hash(state);
                inner.prefix.hash(state);
                inner.remainder.hash(state);
            }
            Annot::StringSuffix(inner) => {
                inner.loc.hash(state);
                inner.suffix.hash(state);
                inner.remainder.hash(state);
            }
            Annot::Typeof(inner) => {
                inner.loc.hash(state);
                inner.qname.hash(state);
                inner.t.hash(state);
                inner.targs.hash(state);
            }
            Annot::Bound(inner) => {
                inner.ref_loc.hash(state);
                inner.name.hash(state);
            }
            Annot::NoInfer(t) => t.hash(state),
            Annot::PropertyType(inner) => {
                inner.loc.hash(state);
                inner.obj.hash(state);
                inner.prop.hash(state);
            }
            Annot::ElementType(inner) => {
                inner.loc.hash(state);
                inner.obj.hash(state);
                inner.elem.hash(state);
            }
            Annot::EnumValue(inner) | Annot::Enum(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::OptionalIndexedAccessNonMaybeType(inner) => {
                inner.loc.hash(state);
                inner.obj.hash(state);
                inner.index.hash(state);
            }
            Annot::OptionalIndexedAccessResultType(inner) => {
                inner.loc.hash(state);
                inner.non_maybe_result.hash(state);
                inner.void_loc.hash(state);
            }
            Annot::NonMaybeType(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::Omit(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
                inner.2.hash(state);
            }
            Annot::ReadOnly(inner)
            | Annot::Partial(inner)
            | Annot::Required(inner)
            | Annot::Keys(inner)
            | Annot::Values(inner)
            | Annot::Exact(inner)
            | Annot::ClassT(inner)
            | Annot::ReactElementConfig(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::Renders(inner) => {
                inner.loc.hash(state);
                inner.arg.hash(state);
                inner.variant.hash(state);
            }
            Annot::ComponentMissingRenders(l) | Annot::FunctionBind(l) => l.hash(state),
            Annot::ExportsT(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::Conditional(inner) => {
                inner.loc.hash(state);
                inner.distributive_tparam.hash(state);
                inner.infer_tparams.hash(state);
                inner.check_type.hash(state);
                inner.extends_type.hash(state);
                inner.true_type.hash(state);
                inner.false_type.hash(state);
            }
            Annot::ObjKeyMirror(inner) => {
                inner.loc.hash(state);
                inner.obj.hash(state);
            }
            Annot::FunAnnot(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::ComponentAnnot(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
            Annot::MappedTypeAnnot(inner) => {
                inner.loc.hash(state);
                inner.source_type.hash(state);
                inner.property_type.hash(state);
                inner.key_tparam.hash(state);
                inner.variance.hash(state);
                inner.variance_op.hash(state);
                inner.optional.hash(state);
                inner.inline_keyof.hash(state);
            }
            Annot::ObjAnnot(inner) => {
                inner.loc.hash(state);
                inner.obj_kind.hash(state);
                inner.props.hash(state);
                inner.computed_props.hash(state);
                inner.proto.hash(state);
            }
            Annot::ObjSpreadAnnot(inner) => {
                inner.loc.hash(state);
                inner.exact.hash(state);
                inner.elems.hash(state);
            }
            Annot::InlineInterface(inner) => {
                inner.0.hash(state);
                inner.1.hash(state);
            }
        }
    }
}

impl<Loc, T> Annot<Loc, T> {
    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            Annot::Any(loc)
            | Annot::Mixed(loc)
            | Annot::Empty(loc)
            | Annot::Void(loc)
            | Annot::Null(loc)
            | Annot::Symbol(loc)
            | Annot::UniqueSymbol(loc)
            | Annot::Number(loc)
            | Annot::BigInt(loc)
            | Annot::String(loc)
            | Annot::Boolean(loc)
            | Annot::Exists(loc) => f_loc(cx, loc),
            Annot::Optional(t) => f_t(cx, t),
            Annot::Maybe(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
            }
            Annot::Union(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.t0);
                f_t(cx, &inner.t1);
                for t in &inner.ts {
                    f_t(cx, t);
                }
            }
            Annot::Intersection(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.t0);
                f_t(cx, &inner.t1);
                for t in &inner.ts {
                    f_t(cx, t);
                }
            }
            Annot::Tuple(inner) => {
                f_loc(cx, &inner.loc);
                for elem in &inner.elems {
                    match elem {
                        TupleElement::TupleElement {
                            loc,
                            name: _,
                            t,
                            polarity: _,
                            optional: _,
                        } => {
                            f_loc(cx, loc);
                            f_t(cx, t);
                        }
                        TupleElement::TupleSpread { loc, name: _, t } => {
                            f_loc(cx, loc);
                            f_t(cx, t);
                        }
                    }
                }
            }
            Annot::Array(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
            }
            Annot::ReadOnlyArray(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
            }
            Annot::SingletonString(inner) => f_loc(cx, &inner.0),
            Annot::SingletonNumber(inner) => f_loc(cx, &inner.0),
            Annot::SingletonBigInt(inner) => f_loc(cx, &inner.0),
            Annot::SingletonBoolean(loc, _) => f_loc(cx, loc),
            Annot::StringPrefix(inner) => {
                f_loc(cx, &inner.loc);
                if let Some(t) = &inner.remainder {
                    f_t(cx, t);
                }
            }
            Annot::StringSuffix(inner) => {
                f_loc(cx, &inner.loc);
                if let Some(t) = &inner.remainder {
                    f_t(cx, t);
                }
            }
            Annot::Typeof(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.t);
                if let Some(targs) = &inner.targs {
                    for t in targs {
                        f_t(cx, t);
                    }
                }
            }
            Annot::Bound(inner) => f_loc(cx, &inner.ref_loc),
            Annot::NoInfer(t) => f_t(cx, t),
            Annot::PropertyType(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.obj);
            }
            Annot::ElementType(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.obj);
                f_t(cx, &inner.elem);
            }
            Annot::EnumValue(inner) | Annot::Enum(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
            }
            Annot::OptionalIndexedAccessNonMaybeType(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.obj);
                f_t(cx, &inner.index);
            }
            Annot::OptionalIndexedAccessResultType(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.non_maybe_result);
                f_loc(cx, &inner.void_loc);
            }
            Annot::NonMaybeType(inner)
            | Annot::ReadOnly(inner)
            | Annot::Partial(inner)
            | Annot::Required(inner)
            | Annot::Keys(inner)
            | Annot::Values(inner)
            | Annot::Exact(inner)
            | Annot::ClassT(inner)
            | Annot::ReactElementConfig(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
            }
            Annot::Omit(inner) => {
                f_loc(cx, &inner.0);
                f_t(cx, &inner.1);
                f_t(cx, &inner.2);
            }
            Annot::Renders(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.arg);
            }
            Annot::ComponentMissingRenders(loc) | Annot::FunctionBind(loc) => f_loc(cx, loc),
            Annot::ExportsT(inner) => f_loc(cx, &inner.0),
            Annot::Conditional(inner) => {
                f_loc(cx, &inner.loc);
                if let Some(tp) = &inner.distributive_tparam {
                    f_loc(cx, &tp.name_loc);
                    if let Some(b) = &tp.bound {
                        f_t(cx, b);
                    }
                    if let Some(d) = &tp.default {
                        f_t(cx, d);
                    }
                }
                inner.infer_tparams.iter(cx, f_loc, f_t);
                f_t(cx, &inner.check_type);
                f_t(cx, &inner.extends_type);
                f_t(cx, &inner.true_type);
                f_t(cx, &inner.false_type);
            }
            Annot::ObjKeyMirror(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.obj);
            }
            Annot::FunAnnot(inner) => {
                f_loc(cx, &inner.0);
                inner.1.iter(cx, f_loc, f_t);
            }
            Annot::ComponentAnnot(inner) => {
                f_loc(cx, &inner.0);
                inner.1.iter(cx, f_loc, f_t);
            }
            Annot::MappedTypeAnnot(inner) => {
                f_loc(cx, &inner.loc);
                f_t(cx, &inner.source_type);
                f_t(cx, &inner.property_type);
                f_loc(cx, &inner.key_tparam.name_loc);
                if let Some(b) = &inner.key_tparam.bound {
                    f_t(cx, b);
                }
                if let Some(d) = &inner.key_tparam.default {
                    f_t(cx, d);
                }
            }
            Annot::ObjAnnot(inner) => {
                f_loc(cx, &inner.loc);
                match &inner.obj_kind {
                    ObjKind::ExactObj | ObjKind::InexactObj => {}
                    ObjKind::IndexedObj(dict) => {
                        f_t(cx, &dict.key);
                        f_t(cx, &dict.value);
                    }
                }
                for v in inner.props.values() {
                    match v {
                        ObjAnnotProp::ObjAnnotField(loc, t, _) => {
                            f_loc(cx, loc);
                            f_t(cx, t);
                        }
                        ObjAnnotProp::ObjAnnotAccess(accessor) => accessor.iter(cx, f_loc, f_t),
                        ObjAnnotProp::ObjAnnotMethod {
                            id_loc,
                            fn_loc,
                            def,
                        } => {
                            f_loc(cx, id_loc);
                            f_loc(cx, fn_loc);
                            def.iter(cx, f_loc, f_t);
                        }
                    }
                }
                for (t, prop) in &inner.computed_props {
                    f_t(cx, t);
                    match prop {
                        ObjAnnotProp::ObjAnnotField(loc, t, _) => {
                            f_loc(cx, loc);
                            f_t(cx, t);
                        }
                        ObjAnnotProp::ObjAnnotAccess(accessor) => accessor.iter(cx, f_loc, f_t),
                        ObjAnnotProp::ObjAnnotMethod {
                            id_loc,
                            fn_loc,
                            def,
                        } => {
                            f_loc(cx, id_loc);
                            f_loc(cx, fn_loc);
                            def.iter(cx, f_loc, f_t);
                        }
                    }
                }
                match &inner.proto {
                    ObjAnnotProto::ObjAnnotImplicitProto => {}
                    ObjAnnotProto::ObjAnnotExplicitProto(loc, t) => {
                        f_loc(cx, loc);
                        f_t(cx, t);
                    }
                    ObjAnnotProto::ObjAnnotCallable { ts } => {
                        for t in ts.iter() {
                            f_t(cx, t);
                        }
                    }
                }
            }
            Annot::ObjSpreadAnnot(inner) => {
                f_loc(cx, &inner.loc);
                for elem in inner.elems.iter() {
                    match elem {
                        ObjSpreadAnnotElem::ObjSpreadAnnotElem(t) => f_t(cx, t),
                        ObjSpreadAnnotElem::ObjSpreadAnnotSlice {
                            dict,
                            props,
                            computed_props,
                        } => {
                            if let Some(d) = dict {
                                f_t(cx, &d.key);
                                f_t(cx, &d.value);
                            }
                            for v in props.values() {
                                match v {
                                    ObjAnnotProp::ObjAnnotField(loc, t, _) => {
                                        f_loc(cx, loc);
                                        f_t(cx, t);
                                    }
                                    ObjAnnotProp::ObjAnnotAccess(accessor) => {
                                        accessor.iter(cx, f_loc, f_t)
                                    }
                                    ObjAnnotProp::ObjAnnotMethod {
                                        id_loc,
                                        fn_loc,
                                        def,
                                    } => {
                                        f_loc(cx, id_loc);
                                        f_loc(cx, fn_loc);
                                        def.iter(cx, f_loc, f_t);
                                    }
                                }
                            }
                            for (t, prop) in computed_props {
                                f_t(cx, t);
                                match prop {
                                    ObjAnnotProp::ObjAnnotField(loc, t, _) => {
                                        f_loc(cx, loc);
                                        f_t(cx, t);
                                    }
                                    ObjAnnotProp::ObjAnnotAccess(accessor) => {
                                        accessor.iter(cx, f_loc, f_t)
                                    }
                                    ObjAnnotProp::ObjAnnotMethod {
                                        id_loc,
                                        fn_loc,
                                        def,
                                    } => {
                                        f_loc(cx, id_loc);
                                        f_loc(cx, fn_loc);
                                        def.iter(cx, f_loc, f_t);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Annot::InlineInterface(inner) => {
                f_loc(cx, &inner.0);
                inner.1.iter(cx, f_loc, f_t);
            }
        }
    }

    pub fn map<CX, Loc2, T2>(
        &self,
        cx: &mut CX,
        f_loc: impl Fn(&mut CX, &Loc) -> Loc2,
        f_t: impl Fn(&mut CX, &T) -> T2,
    ) -> Annot<Loc2, T2> {
        match self {
            Annot::Any(loc) => Annot::Any(f_loc(cx, loc)),
            Annot::Mixed(loc) => Annot::Mixed(f_loc(cx, loc)),
            Annot::Empty(loc) => Annot::Empty(f_loc(cx, loc)),
            Annot::Void(loc) => Annot::Void(f_loc(cx, loc)),
            Annot::Null(loc) => Annot::Null(f_loc(cx, loc)),
            Annot::Symbol(loc) => Annot::Symbol(f_loc(cx, loc)),
            Annot::UniqueSymbol(loc) => Annot::UniqueSymbol(f_loc(cx, loc)),
            Annot::Number(loc) => Annot::Number(f_loc(cx, loc)),
            Annot::BigInt(loc) => Annot::BigInt(f_loc(cx, loc)),
            Annot::String(loc) => Annot::String(f_loc(cx, loc)),
            Annot::Boolean(loc) => Annot::Boolean(f_loc(cx, loc)),
            Annot::Exists(loc) => Annot::Exists(f_loc(cx, loc)),
            Annot::Optional(t) => Annot::Optional(f_t(cx, t)),
            Annot::Maybe(inner) => Annot::Maybe(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1)))),
            Annot::Union(inner) => Annot::Union(Box::new(AnnotUnion {
                loc: f_loc(cx, &inner.loc),
                t0: f_t(cx, &inner.t0),
                t1: f_t(cx, &inner.t1),
                ts: inner.ts.iter().map(|t| f_t(cx, t)).collect(),
            })),
            Annot::Intersection(inner) => Annot::Intersection(Box::new(AnnotIntersection {
                loc: f_loc(cx, &inner.loc),
                t0: f_t(cx, &inner.t0),
                t1: f_t(cx, &inner.t1),
                ts: inner.ts.iter().map(|t| f_t(cx, t)).collect(),
            })),
            Annot::Tuple(inner) => Annot::Tuple(Box::new(AnnotTuple {
                loc: f_loc(cx, &inner.loc),
                elems: inner
                    .elems
                    .iter()
                    .map(|elem| elem.map(cx, &f_loc, &f_t))
                    .collect(),
                inexact: inner.inexact,
            })),
            Annot::Array(inner) => Annot::Array(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1)))),
            Annot::ReadOnlyArray(inner) => {
                Annot::ReadOnlyArray(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::SingletonString(inner) => {
                Annot::SingletonString(Box::new((f_loc(cx, &inner.0), inner.1.dupe())))
            }
            Annot::SingletonNumber(inner) => {
                Annot::SingletonNumber(Box::new((f_loc(cx, &inner.0), inner.1, inner.2.dupe())))
            }
            Annot::SingletonBigInt(inner) => {
                Annot::SingletonBigInt(Box::new((f_loc(cx, &inner.0), inner.1, inner.2.dupe())))
            }
            Annot::SingletonBoolean(loc, b) => Annot::SingletonBoolean(f_loc(cx, loc), *b),
            Annot::StringPrefix(inner) => Annot::StringPrefix(Box::new(AnnotStringPrefix {
                loc: f_loc(cx, &inner.loc),
                prefix: inner.prefix.dupe(),
                remainder: inner.remainder.as_ref().map(|t| f_t(cx, t)),
            })),
            Annot::StringSuffix(inner) => Annot::StringSuffix(Box::new(AnnotStringSuffix {
                loc: f_loc(cx, &inner.loc),
                suffix: inner.suffix.dupe(),
                remainder: inner.remainder.as_ref().map(|t| f_t(cx, t)),
            })),
            Annot::Typeof(inner) => Annot::Typeof(Box::new(AnnotTypeof {
                loc: f_loc(cx, &inner.loc),
                qname: inner.qname.clone(),
                t: f_t(cx, &inner.t),
                targs: inner
                    .targs
                    .as_ref()
                    .map(|targs| targs.iter().map(|t| f_t(cx, t)).collect()),
            })),
            Annot::Bound(inner) => Annot::Bound(Box::new(AnnotBound {
                ref_loc: f_loc(cx, &inner.ref_loc),
                name: inner.name.dupe(),
            })),
            Annot::NoInfer(t) => Annot::NoInfer(f_t(cx, t)),
            Annot::PropertyType(inner) => Annot::PropertyType(Box::new(AnnotPropertyType {
                loc: f_loc(cx, &inner.loc),
                obj: f_t(cx, &inner.obj),
                prop: inner.prop.dupe(),
            })),
            Annot::ElementType(inner) => Annot::ElementType(Box::new(AnnotElementType {
                loc: f_loc(cx, &inner.loc),
                obj: f_t(cx, &inner.obj),
                elem: f_t(cx, &inner.elem),
            })),
            Annot::EnumValue(inner) => {
                Annot::EnumValue(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Enum(inner) => Annot::Enum(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1)))),
            Annot::OptionalIndexedAccessNonMaybeType(inner) => {
                Annot::OptionalIndexedAccessNonMaybeType(Box::new(
                    AnnotOptionalIndexedAccessNonMaybeType {
                        loc: f_loc(cx, &inner.loc),
                        obj: f_t(cx, &inner.obj),
                        index: f_t(cx, &inner.index),
                    },
                ))
            }
            Annot::OptionalIndexedAccessResultType(inner) => {
                Annot::OptionalIndexedAccessResultType(Box::new(
                    AnnotOptionalIndexedAccessResultType {
                        loc: f_loc(cx, &inner.loc),
                        non_maybe_result: f_t(cx, &inner.non_maybe_result),
                        void_loc: f_loc(cx, &inner.void_loc),
                    },
                ))
            }
            Annot::NonMaybeType(inner) => {
                Annot::NonMaybeType(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Omit(inner) => Annot::Omit(Box::new((
                f_loc(cx, &inner.0),
                f_t(cx, &inner.1),
                f_t(cx, &inner.2),
            ))),
            Annot::ReadOnly(inner) => {
                Annot::ReadOnly(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Partial(inner) => {
                Annot::Partial(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Required(inner) => {
                Annot::Required(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Keys(inner) => Annot::Keys(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1)))),
            Annot::Renders(inner) => Annot::Renders(Box::new(AnnotRenders {
                loc: f_loc(cx, &inner.loc),
                arg: f_t(cx, &inner.arg),
                variant: inner.variant,
            })),
            Annot::ComponentMissingRenders(loc) => Annot::ComponentMissingRenders(f_loc(cx, loc)),
            Annot::Values(inner) => {
                Annot::Values(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::Exact(inner) => Annot::Exact(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1)))),
            Annot::ExportsT(inner) => {
                Annot::ExportsT(Box::new((f_loc(cx, &inner.0), inner.1.clone())))
            }
            Annot::Conditional(inner) => Annot::Conditional(Box::new(AnnotConditional {
                loc: f_loc(cx, &inner.loc),
                distributive_tparam: inner
                    .distributive_tparam
                    .as_ref()
                    .map(|tp| tp.map(cx, &f_loc, &f_t)),
                infer_tparams: inner.infer_tparams.map(cx, &f_loc, &f_t),
                check_type: f_t(cx, &inner.check_type),
                extends_type: f_t(cx, &inner.extends_type),
                true_type: f_t(cx, &inner.true_type),
                false_type: f_t(cx, &inner.false_type),
            })),
            Annot::ObjKeyMirror(inner) => Annot::ObjKeyMirror(Box::new(AnnotObjKeyMirror {
                loc: f_loc(cx, &inner.loc),
                obj: f_t(cx, &inner.obj),
            })),
            Annot::ClassT(inner) => {
                Annot::ClassT(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::FunctionBind(loc) => Annot::FunctionBind(f_loc(cx, loc)),
            Annot::ReactElementConfig(inner) => {
                Annot::ReactElementConfig(Box::new((f_loc(cx, &inner.0), f_t(cx, &inner.1))))
            }
            Annot::FunAnnot(inner) => Annot::FunAnnot(Box::new((
                f_loc(cx, &inner.0),
                inner.1.map(cx, &f_loc, &f_t),
            ))),
            Annot::ComponentAnnot(inner) => Annot::ComponentAnnot(Box::new((
                f_loc(cx, &inner.0),
                inner.1.map(cx, &f_loc, &f_t),
            ))),
            Annot::MappedTypeAnnot(inner) => {
                Annot::MappedTypeAnnot(Box::new(AnnotMappedTypeAnnot {
                    loc: f_loc(cx, &inner.loc),
                    source_type: f_t(cx, &inner.source_type),
                    property_type: f_t(cx, &inner.property_type),
                    key_tparam: inner.key_tparam.map(cx, &f_loc, &f_t),
                    variance: inner.variance,
                    variance_op: inner.variance_op,
                    optional: inner.optional,
                    inline_keyof: inner.inline_keyof,
                }))
            }
            Annot::ObjAnnot(inner) => Annot::ObjAnnot(Box::new(AnnotObjAnnot {
                loc: f_loc(cx, &inner.loc),
                obj_kind: match &inner.obj_kind {
                    ObjKind::ExactObj => ObjKind::ExactObj,
                    ObjKind::InexactObj => ObjKind::InexactObj,
                    ObjKind::IndexedObj(dict) => ObjKind::IndexedObj(dict.map(cx, &f_t)),
                },
                props: inner
                    .props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                    .collect(),
                computed_props: inner
                    .computed_props
                    .iter()
                    .map(|(t, prop)| (f_t(cx, t), prop.map(cx, &f_loc, &f_t)))
                    .collect(),
                proto: inner.proto.map(cx, &f_loc, &f_t),
            })),
            Annot::ObjSpreadAnnot(inner) => Annot::ObjSpreadAnnot(Box::new(AnnotObjSpreadAnnot {
                loc: f_loc(cx, &inner.loc),
                exact: inner.exact,
                elems: inner.elems.mapped_ref(|elem| elem.map(cx, &f_loc, &f_t)),
            })),
            Annot::InlineInterface(inner) => Annot::InlineInterface(Box::new((
                f_loc(cx, &inner.0),
                inner.1.map(cx, &f_loc, &f_t),
            ))),
        }
    }
}

// Along with literal expressions, the signature extractor also encodes some
// operations over values and annotations, like unary operators and
// destructuring.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum Op<T> {
    Arith(ast::expression::BinaryOperator, T),
    GetProp(FlowSmolStr),
    GetElem(T),
    Unary(ast::expression::UnaryOperator),
    Update,
}

impl<T> Op<T> {
    pub fn iter(&self, mut f_t: impl FnMut(&T)) {
        match self {
            Op::Arith(_, t) => f_t(t),
            Op::GetProp(_) => {}
            Op::GetElem(t) => f_t(t),
            Op::Unary(_) => {}
            Op::Update => {}
        }
    }

    pub fn map<CX, T2>(&self, cx: &mut CX, f_t: impl Fn(&mut CX, &T) -> T2) -> Op<T2> {
        match self {
            Op::Arith(op, t) => Op::Arith(*op, f_t(cx, t)),
            Op::GetProp(s) => Op::GetProp(s.dupe()),
            Op::GetElem(t) => Op::GetElem(f_t(cx, t)),
            Op::Unary(op) => Op::Unary(*op),
            Op::Update => Op::Update,
        }
    }
}

// Parsing out a signature can fail. There are three interesting failure modes,
// which are all conflated in this type.
//
// 1. If there are insufficient annotations to extract a signature, we should
//    should inform the user and proceeed as if the unknown type was any.
// 2. If some unsupported or unexpected syntax was detected, we should proceed
//    as if the type was any, but not inform the user. The checking phase will
//    uncover the same error.
// 3. If there is some value syntax, which is just not supported by the
//    signature extractor, we should fix the signature extractor to support that
//    syntax. These represent missing functionality in the analysis which should
//    be fixed.
#[derive(Debug, Clone, PartialEq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Errno<Loc> {
    CheckError,
    BindingValidationError(signature_error::BindingValidation<Loc>),
    SigError(signature_error::SignatureError<Loc>),
}

impl<Loc> Errno<Loc> {
    pub fn iter(&self, f_loc: impl FnMut(&Loc)) {
        match self {
            Errno::CheckError => {}
            Errno::BindingValidationError(err) => {
                err.iter(f_loc);
            }
            Errno::SigError(err) => {
                err.iter(f_loc);
            }
        }
    }

    pub fn map<CX, Loc2>(&self, cx: &mut CX, f: impl Fn(&mut CX, &Loc) -> Loc2) -> Errno<Loc2> {
        match self {
            Errno::CheckError => Errno::CheckError,
            Errno::BindingValidationError(err) => Errno::BindingValidationError(err.map(cx, f)),
            Errno::SigError(err) => Errno::SigError(err.map(cx, f)),
        }
    }
}

impl<Loc: std::fmt::Display> std::fmt::Display for Errno<Loc> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Errno::CheckError => write!(f, "CheckError"),
            Errno::BindingValidationError(err) => write!(f, "BindingValidationError ({})", err),
            Errno::SigError(err) => write!(f, "SigError ({})", err),
        }
    }
}
