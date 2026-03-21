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
            ObjSpreadAnnotElem::ObjSpreadAnnotSlice { dict, props } => {
                ObjSpreadAnnotElem::ObjSpreadAnnotSlice {
                    dict: dict.as_ref().map(|d| d.map(cx, &f_t)),
                    props: props
                        .iter()
                        .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                        .collect(),
                }
            }
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
pub enum Def<Loc, T> {
    TypeAlias {
        id_loc: Loc,
        custom_error_loc_opt: Option<Loc>,
        name: FlowSmolStr,
        tparams: TParams<Loc, T>,
        body: T,
    },
    OpaqueType {
        id_loc: Loc,
        name: FlowSmolStr,
        tparams: TParams<Loc, T>,
        lower_bound: Option<T>,
        upper_bound: Option<T>,
        body: Option<T>,
    },
    Interface {
        id_loc: Loc,
        name: FlowSmolStr,
        tparams: TParams<Loc, T>,
        def: InterfaceSig<Loc, T>,
    },
    ClassBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        def: ClassSig<Loc, T>,
    },
    DeclareClassBinding {
        id_loc: Loc,
        nominal_id_loc: Loc,
        name: FlowSmolStr,
        def: DeclareClassSig<Loc, T>,
    },
    RecordBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        def: ClassSig<Loc, T>,
        defaulted_props: BTreeSet<FlowSmolStr>,
    },
    DisabledRecordBinding {
        id_loc: Loc,
        name: FlowSmolStr,
    },
    FunBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        async_: bool,
        generator: bool,
        fn_loc: Loc,
        def: FunSig<Loc, T>,
        statics: BTreeMap<FlowSmolStr, (Loc, T)>,
    },
    DeclareFun {
        id_loc: Loc,
        name: FlowSmolStr,
        fn_loc: Loc,
        def: FunSig<Loc, T>,
        tail: Vec<(Loc, Loc, FunSig<Loc, T>)>,
    },
    ComponentBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        fn_loc: Loc,
        def: ComponentSig<Loc, T>,
    },
    DisabledComponentBinding {
        id_loc: Loc,
        name: FlowSmolStr,
    },
    Variable {
        id_loc: Loc,
        name: FlowSmolStr,
        def: T,
    },
    Parameter {
        id_loc: Loc,
        name: FlowSmolStr,
        def: T,
        tparams: TParams<Loc, T>,
    },
    EnumBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        rep: EnumRep,
        members: BTreeMap<FlowSmolStr, Loc>,
        has_unknown_members: bool,
    },
    DisabledEnumBinding {
        id_loc: Loc,
        name: FlowSmolStr,
    },
    NamespaceBinding {
        id_loc: Loc,
        name: FlowSmolStr,
        values: BTreeMap<FlowSmolStr, (Loc, T)>,
        types: BTreeMap<FlowSmolStr, (Loc, T)>,
    },
}

impl<Loc: Clone, T> Def<Loc, T> {
    // These accessors will compile to code that does not have a branch because
    // id_loc and name have the same offset for each constructor.
    pub fn id_loc(&self) -> Loc {
        match self {
            Def::TypeAlias { id_loc, .. }
            | Def::OpaqueType { id_loc, .. }
            | Def::Interface { id_loc, .. }
            | Def::ClassBinding { id_loc, .. }
            | Def::DeclareClassBinding { id_loc, .. }
            | Def::RecordBinding { id_loc, .. }
            | Def::DisabledRecordBinding { id_loc, .. }
            | Def::FunBinding { id_loc, .. }
            | Def::DeclareFun { id_loc, .. }
            | Def::ComponentBinding { id_loc, .. }
            | Def::DisabledComponentBinding { id_loc, .. }
            | Def::Variable { id_loc, .. }
            | Def::Parameter { id_loc, .. }
            | Def::EnumBinding { id_loc, .. }
            | Def::DisabledEnumBinding { id_loc, .. }
            | Def::NamespaceBinding { id_loc, .. } => {
                // clone needed: generic Loc type
                id_loc.clone()
            }
        }
    }

    pub fn name(&self) -> &FlowSmolStr {
        match self {
            Def::TypeAlias { name, .. }
            | Def::OpaqueType { name, .. }
            | Def::Interface { name, .. }
            | Def::ClassBinding { name, .. }
            | Def::DeclareClassBinding { name, .. }
            | Def::RecordBinding { name, .. }
            | Def::DisabledRecordBinding { name, .. }
            | Def::FunBinding { name, .. }
            | Def::DeclareFun { name, .. }
            | Def::ComponentBinding { name, .. }
            | Def::DisabledComponentBinding { name, .. }
            | Def::Variable { name, .. }
            | Def::Parameter { name, .. }
            | Def::EnumBinding { name, .. }
            | Def::DisabledEnumBinding { name, .. }
            | Def::NamespaceBinding { name, .. } => name,
        }
    }

    pub fn iter<CX>(
        &self,
        cx: &mut CX,
        f_loc: &impl Fn(&mut CX, &Loc),
        f_t: &impl Fn(&mut CX, &T),
    ) {
        match self {
            Def::TypeAlias {
                id_loc,
                custom_error_loc_opt,
                name: _,
                tparams,
                body,
            } => {
                f_loc(cx, id_loc);
                if let Some(loc) = custom_error_loc_opt {
                    f_loc(cx, loc);
                }
                tparams.iter(cx, f_loc, f_t);
                f_t(cx, body);
            }
            Def::OpaqueType {
                id_loc,
                name: _,
                tparams,
                lower_bound,
                upper_bound,
                body,
            } => {
                f_loc(cx, id_loc);
                tparams.iter(cx, f_loc, f_t);
                if let Some(t) = lower_bound {
                    f_t(cx, t);
                }
                if let Some(t) = upper_bound {
                    f_t(cx, t);
                }
                if let Some(t) = body {
                    f_t(cx, t);
                }
            }
            Def::Interface {
                id_loc,
                name: _,
                tparams,
                def,
            } => {
                f_loc(cx, id_loc);
                tparams.iter(cx, f_loc, f_t);
                def.iter(cx, f_loc, f_t);
            }
            Def::ClassBinding {
                id_loc,
                name: _,
                def,
            } => {
                f_loc(cx, id_loc);
                def.iter(cx, f_loc, f_t);
            }
            Def::DeclareClassBinding {
                id_loc,
                nominal_id_loc,
                name: _,
                def,
            } => {
                f_loc(cx, id_loc);
                f_loc(cx, nominal_id_loc);
                def.iter(cx, f_loc, f_t);
            }
            Def::RecordBinding {
                id_loc,
                name: _,
                def,
                defaulted_props: _,
            } => {
                f_loc(cx, id_loc);
                def.iter(cx, f_loc, f_t);
            }
            Def::DisabledRecordBinding { id_loc, name: _ } => {
                f_loc(cx, id_loc);
            }
            Def::FunBinding {
                id_loc,
                name: _,
                async_: _,
                generator: _,
                fn_loc,
                def,
                statics,
            } => {
                f_loc(cx, id_loc);
                f_loc(cx, fn_loc);
                def.iter(cx, f_loc, f_t);
                for (loc, t) in statics.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Def::DeclareFun {
                id_loc,
                name: _,
                fn_loc,
                def,
                tail,
            } => {
                f_loc(cx, id_loc);
                f_loc(cx, fn_loc);
                def.iter(cx, f_loc, f_t);
                for (loc1, loc2, sig) in tail {
                    f_loc(cx, loc1);
                    f_loc(cx, loc2);
                    sig.iter(cx, f_loc, f_t);
                }
            }
            Def::ComponentBinding {
                id_loc,
                name: _,
                fn_loc,
                def,
            } => {
                f_loc(cx, id_loc);
                f_loc(cx, fn_loc);
                def.iter(cx, f_loc, f_t);
            }
            Def::DisabledComponentBinding { id_loc, name: _ } => {
                f_loc(cx, id_loc);
            }
            Def::Variable {
                id_loc,
                name: _,
                def,
            } => {
                f_loc(cx, id_loc);
                f_t(cx, def);
            }
            Def::Parameter {
                id_loc,
                name: _,
                def,
                tparams,
            } => {
                f_loc(cx, id_loc);
                f_t(cx, def);
                tparams.iter(cx, f_loc, f_t);
            }
            Def::EnumBinding {
                id_loc,
                name: _,
                rep: _,
                members,
                has_unknown_members: _,
            } => {
                f_loc(cx, id_loc);
                for loc in members.values() {
                    f_loc(cx, loc);
                }
            }
            Def::DisabledEnumBinding { id_loc, name: _ } => {
                f_loc(cx, id_loc);
            }
            Def::NamespaceBinding {
                id_loc,
                name: _,
                values,
                types,
            } => {
                f_loc(cx, id_loc);
                for (loc, t) in values.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for (loc, t) in types.values() {
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
            Def::TypeAlias {
                id_loc,
                custom_error_loc_opt,
                name,
                tparams,
                body,
            } => Def::TypeAlias {
                id_loc: f_loc(cx, id_loc),
                custom_error_loc_opt: custom_error_loc_opt.as_ref().map(|loc| f_loc(cx, loc)),
                name: name.dupe(),
                tparams: tparams.map(cx, &f_loc, &f_t),
                body: f_t(cx, body),
            },
            Def::OpaqueType {
                id_loc,
                name,
                tparams,
                lower_bound,
                upper_bound,
                body,
            } => Def::OpaqueType {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                tparams: tparams.map(cx, &f_loc, &f_t),
                lower_bound: lower_bound.as_ref().map(|t| f_t(cx, t)),
                upper_bound: upper_bound.as_ref().map(|t| f_t(cx, t)),
                body: body.as_ref().map(|t| f_t(cx, t)),
            },
            Def::Interface {
                id_loc,
                name,
                tparams,
                def,
            } => Def::Interface {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                tparams: tparams.map(cx, &f_loc, &f_t),
                def: def.map(cx, &f_loc, &f_t),
            },
            Def::ClassBinding { id_loc, name, def } => Def::ClassBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                def: def.map(cx, &f_loc, &f_t),
            },
            Def::DeclareClassBinding {
                id_loc,
                nominal_id_loc,
                name,
                def,
            } => Def::DeclareClassBinding {
                id_loc: f_loc(cx, id_loc),
                nominal_id_loc: f_loc(cx, nominal_id_loc),
                name: name.dupe(),
                def: def.map(cx, &f_loc, &f_t),
            },
            Def::RecordBinding {
                id_loc,
                name,
                def,
                defaulted_props,
            } => Def::RecordBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                def: def.map(cx, &f_loc, &f_t),
                defaulted_props: defaulted_props.clone(),
            },
            Def::DisabledRecordBinding { id_loc, name } => Def::DisabledRecordBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
            },
            Def::FunBinding {
                id_loc,
                name,
                async_,
                generator,
                fn_loc,
                def,
                statics,
            } => Def::FunBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                async_: *async_,
                generator: *generator,
                fn_loc: f_loc(cx, fn_loc),
                def: def.map(cx, &f_loc, &f_t),
                statics: statics
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            },
            Def::DeclareFun {
                id_loc,
                name,
                fn_loc,
                def,
                tail,
            } => Def::DeclareFun {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                fn_loc: f_loc(cx, fn_loc),
                def: def.map(cx, &f_loc, &f_t),
                tail: tail
                    .iter()
                    .map(|(loc1, loc2, sig)| {
                        (f_loc(cx, loc1), f_loc(cx, loc2), sig.map(cx, &f_loc, &f_t))
                    })
                    .collect(),
            },
            Def::ComponentBinding {
                id_loc,
                name,
                fn_loc,
                def,
            } => Def::ComponentBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                fn_loc: f_loc(cx, fn_loc),
                def: def.map(cx, &f_loc, &f_t),
            },
            Def::DisabledComponentBinding { id_loc, name } => Def::DisabledComponentBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
            },
            Def::Variable { id_loc, name, def } => Def::Variable {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                def: f_t(cx, def),
            },
            Def::Parameter {
                id_loc,
                name,
                def,
                tparams,
            } => Def::Parameter {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                def: f_t(cx, def),
                tparams: tparams.map(cx, &f_loc, &f_t),
            },
            Def::EnumBinding {
                id_loc,
                name,
                rep,
                members,
                has_unknown_members,
            } => Def::EnumBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                rep: *rep,
                members: members
                    .iter()
                    .map(|(k, loc)| (k.dupe(), f_loc(cx, loc)))
                    .collect(),
                has_unknown_members: *has_unknown_members,
            },
            Def::DisabledEnumBinding { id_loc, name } => Def::DisabledEnumBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
            },
            Def::NamespaceBinding {
                id_loc,
                name,
                values,
                types,
            } => Def::NamespaceBinding {
                id_loc: f_loc(cx, id_loc),
                name: name.dupe(),
                values: values
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
                types: types
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            },
        }
    }
}

// The signature extractor relies heavily on annotations, but will extract
// signatures corresponding to some literal expressions as well. The
// representation of these things are kept distinct from annotations, below.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Value<Loc, T> {
    ClassExpr(Loc, ClassSig<Loc, T>),
    FunExpr {
        loc: Loc,
        async_: bool,
        generator: bool,
        def: FunSig<Loc, T>,
        statics: BTreeMap<FlowSmolStr, (Loc, T)>,
    },
    StringVal(Loc),
    StringLit(Loc, FlowSmolStr),
    NumberVal(Loc),
    NumberLit(Loc, f64, FlowSmolStr),
    BigIntVal(Loc),
    BigIntLit(Loc, Option<i64>, FlowSmolStr),
    BooleanVal(Loc),
    BooleanLit(Loc, bool),
    NullLit(Loc),
    DeclareModuleImplicitlyExportedObject {
        loc: Loc,
        module_name: flow_import_specifier::Userland,
        props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
    },
    ObjLit {
        loc: Loc,
        frozen: bool,
        proto: Option<(Loc, T)>,
        props: BTreeMap<FlowSmolStr, ObjValueProp<Loc, T>>,
    },
    ObjSpreadLit {
        loc: Loc,
        frozen: bool,
        proto: Option<(Loc, T)>,
        elems: Vec1<ObjValueSpreadElem<Loc, T>>,
    },
    EmptyConstArrayLit(Loc),
    ArrayLit(Loc, T, Vec<T>),
    AsConst(Box<Value<Loc, T>>),
}

impl<Loc: std::hash::Hash, T: std::hash::Hash> std::hash::Hash for Value<Loc, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::ClassExpr(loc, sig) => {
                loc.hash(state);
                sig.hash(state);
            }
            Value::FunExpr {
                loc,
                async_,
                generator,
                def,
                statics,
            } => {
                loc.hash(state);
                async_.hash(state);
                generator.hash(state);
                def.hash(state);
                statics.hash(state);
            }
            Value::StringVal(loc) => loc.hash(state),
            Value::StringLit(loc, s) => {
                loc.hash(state);
                s.hash(state);
            }
            Value::NumberVal(loc) => loc.hash(state),
            Value::NumberLit(loc, n, s) => {
                loc.hash(state);
                n.to_bits().hash(state);
                s.hash(state);
            }
            Value::BigIntVal(loc) => loc.hash(state),
            Value::BigIntLit(loc, n, s) => {
                loc.hash(state);
                n.hash(state);
                s.hash(state);
            }
            Value::BooleanVal(loc) => loc.hash(state),
            Value::BooleanLit(loc, b) => {
                loc.hash(state);
                b.hash(state);
            }
            Value::NullLit(loc) => loc.hash(state),
            Value::DeclareModuleImplicitlyExportedObject {
                loc,
                module_name,
                props,
            } => {
                loc.hash(state);
                module_name.hash(state);
                props.hash(state);
            }
            Value::ObjLit {
                loc,
                frozen,
                proto,
                props,
            } => {
                loc.hash(state);
                frozen.hash(state);
                proto.hash(state);
                props.hash(state);
            }
            Value::ObjSpreadLit {
                loc,
                frozen,
                proto,
                elems,
            } => {
                loc.hash(state);
                frozen.hash(state);
                proto.hash(state);
                elems.hash(state);
            }
            Value::EmptyConstArrayLit(loc) => loc.hash(state),
            Value::ArrayLit(loc, t, ts) => {
                loc.hash(state);
                t.hash(state);
                ts.hash(state);
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
            Value::ClassExpr(loc, sig) => {
                f_loc(cx, loc);
                sig.iter(cx, f_loc, f_t);
            }
            Value::FunExpr {
                loc,
                async_: _,
                generator: _,
                def,
                statics,
            } => {
                f_loc(cx, loc);
                def.iter(cx, f_loc, f_t);
                for (loc, t) in statics.values() {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
            }
            Value::StringVal(loc) => f_loc(cx, loc),
            Value::StringLit(loc, _) => f_loc(cx, loc),
            Value::NumberVal(loc) => f_loc(cx, loc),
            Value::NumberLit(loc, _, _) => f_loc(cx, loc),
            Value::BigIntVal(loc) => f_loc(cx, loc),
            Value::BigIntLit(loc, _, _) => f_loc(cx, loc),
            Value::BooleanVal(loc) => f_loc(cx, loc),
            Value::BooleanLit(loc, _) => f_loc(cx, loc),
            Value::NullLit(loc) => f_loc(cx, loc),
            Value::DeclareModuleImplicitlyExportedObject {
                loc,
                module_name: _,
                props,
            } => {
                f_loc(cx, loc);
                for v in props.values() {
                    v.iter(cx, f_loc, f_t);
                }
            }
            Value::ObjLit {
                loc,
                frozen: _,
                proto,
                props,
            } => {
                f_loc(cx, loc);
                if let Some((loc, t)) = proto {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for v in props.values() {
                    v.iter(cx, f_loc, f_t);
                }
            }
            Value::ObjSpreadLit {
                loc,
                frozen: _,
                proto,
                elems,
            } => {
                f_loc(cx, loc);
                if let Some((loc, t)) = proto {
                    f_loc(cx, loc);
                    f_t(cx, t);
                }
                for elem in elems.iter() {
                    elem.iter(cx, f_loc, f_t);
                }
            }
            Value::EmptyConstArrayLit(loc) => f_loc(cx, loc),
            Value::ArrayLit(loc, t, ts) => {
                f_loc(cx, loc);
                f_t(cx, t);
                for t in ts {
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
            Value::ClassExpr(loc, sig) => Value::ClassExpr(f_loc(cx, loc), sig.map(cx, f_loc, f_t)),
            Value::FunExpr {
                loc,
                async_,
                generator,
                def,
                statics,
            } => Value::FunExpr {
                loc: f_loc(cx, loc),
                async_: *async_,
                generator: *generator,
                def: def.map(cx, f_loc, f_t),
                statics: statics
                    .iter()
                    .map(|(k, (loc, t))| (k.dupe(), (f_loc(cx, loc), f_t(cx, t))))
                    .collect(),
            },
            Value::StringVal(loc) => Value::StringVal(f_loc(cx, loc)),
            Value::StringLit(loc, s) => Value::StringLit(f_loc(cx, loc), s.dupe()),
            Value::NumberVal(loc) => Value::NumberVal(f_loc(cx, loc)),
            Value::NumberLit(loc, n, s) => Value::NumberLit(f_loc(cx, loc), *n, s.dupe()),
            Value::BigIntVal(loc) => Value::BigIntVal(f_loc(cx, loc)),
            Value::BigIntLit(loc, i, s) => Value::BigIntLit(f_loc(cx, loc), *i, s.dupe()),
            Value::BooleanVal(loc) => Value::BooleanVal(f_loc(cx, loc)),
            Value::BooleanLit(loc, b) => Value::BooleanLit(f_loc(cx, loc), *b),
            Value::NullLit(loc) => Value::NullLit(f_loc(cx, loc)),
            Value::DeclareModuleImplicitlyExportedObject {
                loc,
                module_name,
                props,
            } => Value::DeclareModuleImplicitlyExportedObject {
                loc: f_loc(cx, loc),
                module_name: module_name.clone(),
                props: props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, f_loc, f_t)))
                    .collect(),
            },
            Value::ObjLit {
                loc,
                frozen,
                proto,
                props,
            } => Value::ObjLit {
                loc: f_loc(cx, loc),
                frozen: *frozen,
                proto: proto.as_ref().map(|(loc, t)| (f_loc(cx, loc), f_t(cx, t))),
                props: props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, f_loc, f_t)))
                    .collect(),
            },
            Value::ObjSpreadLit {
                loc,
                frozen,
                proto,
                elems,
            } => Value::ObjSpreadLit {
                loc: f_loc(cx, loc),
                frozen: *frozen,
                proto: proto.as_ref().map(|(loc, t)| (f_loc(cx, loc), f_t(cx, t))),
                elems: elems.mapped_ref(|elem| elem.map(cx, f_loc, f_t)),
            },
            Value::EmptyConstArrayLit(loc) => Value::EmptyConstArrayLit(f_loc(cx, loc)),
            Value::ArrayLit(loc, t, ts) => Value::ArrayLit(
                f_loc(cx, loc),
                f_t(cx, t),
                ts.iter().map(|t| f_t(cx, t)).collect(),
            ),
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
    Maybe(Loc, T),
    Union {
        loc: Loc,
        t0: T,
        t1: T,
        ts: Vec<T>,
    },
    Intersection {
        loc: Loc,
        t0: T,
        t1: T,
        ts: Vec<T>,
    },
    Tuple {
        loc: Loc,
        elems: Vec<TupleElement<Loc, T>>,
        inexact: bool,
    },
    Array(Loc, T),
    ReadOnlyArray(Loc, T),
    SingletonString(Loc, FlowSmolStr),
    SingletonNumber(Loc, f64, FlowSmolStr),
    SingletonBigInt(Loc, Option<i64>, FlowSmolStr),
    SingletonBoolean(Loc, bool),
    StringPrefix {
        loc: Loc,
        prefix: FlowSmolStr,
        remainder: Option<T>,
    },
    StringSuffix {
        loc: Loc,
        suffix: FlowSmolStr,
        remainder: Option<T>,
    },
    Typeof {
        loc: Loc,
        qname: Vec<FlowSmolStr>,
        t: T,
        targs: Option<Vec<T>>,
    },
    Bound {
        ref_loc: Loc,
        name: FlowSmolStr,
    },
    NoInfer(T),
    PropertyType {
        loc: Loc,
        obj: T,
        prop: FlowSmolStr,
    },
    ElementType {
        loc: Loc,
        obj: T,
        elem: T,
    },
    EnumValue(Loc, T),
    Enum(Loc, T),
    OptionalIndexedAccessNonMaybeType {
        loc: Loc,
        obj: T,
        index: T,
    },
    OptionalIndexedAccessResultType {
        loc: Loc,
        non_maybe_result: T,
        void_loc: Loc,
    },
    NonMaybeType(Loc, T),
    Omit(Loc, T, T),
    ReadOnly(Loc, T),
    Partial(Loc, T),
    Required(Loc, T),
    Keys(Loc, T),
    Renders {
        loc: Loc,
        arg: T,
        variant: ast::types::RendersVariant,
    },
    ComponentMissingRenders(Loc),
    Values(Loc, T),
    Exact(Loc, T),
    ExportsT(Loc, flow_import_specifier::Userland),
    Conditional {
        loc: Loc,
        distributive_tparam: Option<TParam<Loc, T>>,
        infer_tparams: TParams<Loc, T>,
        check_type: T,
        extends_type: T,
        true_type: T,
        false_type: T,
    },
    ObjKeyMirror {
        loc: Loc,
        obj: T,
    },
    ClassT(Loc, T),
    FunctionBind(Loc),
    ReactElementConfig(Loc, T),
    FunAnnot(Loc, FunSig<Loc, T>),
    ComponentAnnot(Loc, ComponentSig<Loc, T>),
    MappedTypeAnnot {
        loc: Loc,
        source_type: T,
        property_type: T,
        key_tparam: TParam<Loc, T>,
        variance: Polarity,
        variance_op: Option<ast::types::object::MappedTypeVarianceOp>,
        optional: ast::types::object::MappedTypeOptionalFlag,
        inline_keyof: bool,
    },
    ObjAnnot {
        loc: Loc,
        obj_kind: ObjKind<T>,
        props: BTreeMap<FlowSmolStr, ObjAnnotProp<Loc, T>>,
        proto: ObjAnnotProto<Loc, T>,
    },
    ObjSpreadAnnot {
        loc: Loc,
        exact: bool,
        elems: Vec1<ObjSpreadAnnotElem<Loc, T>>,
    },
    InlineInterface(Loc, InterfaceSig<Loc, T>),
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
            Annot::Maybe(l, t) => {
                l.hash(state);
                t.hash(state);
            }
            Annot::Union { loc, t0, t1, ts } | Annot::Intersection { loc, t0, t1, ts } => {
                loc.hash(state);
                t0.hash(state);
                t1.hash(state);
                ts.hash(state);
            }
            Annot::Tuple {
                loc,
                elems,
                inexact,
            } => {
                loc.hash(state);
                elems.hash(state);
                inexact.hash(state);
            }
            Annot::Array(l, t) | Annot::ReadOnlyArray(l, t) => {
                l.hash(state);
                t.hash(state);
            }
            Annot::SingletonString(l, s) => {
                l.hash(state);
                s.hash(state);
            }
            Annot::SingletonNumber(l, n, s) => {
                l.hash(state);
                n.to_bits().hash(state);
                s.hash(state);
            }
            Annot::SingletonBigInt(l, n, s) => {
                l.hash(state);
                n.hash(state);
                s.hash(state);
            }
            Annot::SingletonBoolean(l, b) => {
                l.hash(state);
                b.hash(state);
            }
            Annot::StringPrefix {
                loc,
                prefix,
                remainder,
            } => {
                loc.hash(state);
                prefix.hash(state);
                remainder.hash(state);
            }
            Annot::StringSuffix {
                loc,
                suffix,
                remainder,
            } => {
                loc.hash(state);
                suffix.hash(state);
                remainder.hash(state);
            }
            Annot::Typeof {
                loc,
                qname,
                t,
                targs,
            } => {
                loc.hash(state);
                qname.hash(state);
                t.hash(state);
                targs.hash(state);
            }
            Annot::Bound { ref_loc, name } => {
                ref_loc.hash(state);
                name.hash(state);
            }
            Annot::NoInfer(t) => t.hash(state),
            Annot::PropertyType { loc, obj, prop } => {
                loc.hash(state);
                obj.hash(state);
                prop.hash(state);
            }
            Annot::ElementType { loc, obj, elem } => {
                loc.hash(state);
                obj.hash(state);
                elem.hash(state);
            }
            Annot::EnumValue(l, t) | Annot::Enum(l, t) => {
                l.hash(state);
                t.hash(state);
            }
            Annot::OptionalIndexedAccessNonMaybeType { loc, obj, index } => {
                loc.hash(state);
                obj.hash(state);
                index.hash(state);
            }
            Annot::OptionalIndexedAccessResultType {
                loc,
                non_maybe_result,
                void_loc,
            } => {
                loc.hash(state);
                non_maybe_result.hash(state);
                void_loc.hash(state);
            }
            Annot::NonMaybeType(l, t) => {
                l.hash(state);
                t.hash(state);
            }
            Annot::Omit(l, t1, t2) => {
                l.hash(state);
                t1.hash(state);
                t2.hash(state);
            }
            Annot::ReadOnly(l, t)
            | Annot::Partial(l, t)
            | Annot::Required(l, t)
            | Annot::Keys(l, t)
            | Annot::Values(l, t)
            | Annot::Exact(l, t)
            | Annot::ClassT(l, t)
            | Annot::ReactElementConfig(l, t) => {
                l.hash(state);
                t.hash(state);
            }
            Annot::Renders { loc, arg, variant } => {
                loc.hash(state);
                arg.hash(state);
                variant.hash(state);
            }
            Annot::ComponentMissingRenders(l) | Annot::FunctionBind(l) => l.hash(state),
            Annot::ExportsT(l, u) => {
                l.hash(state);
                u.hash(state);
            }
            Annot::Conditional {
                loc,
                distributive_tparam,
                infer_tparams,
                check_type,
                extends_type,
                true_type,
                false_type,
            } => {
                loc.hash(state);
                distributive_tparam.hash(state);
                infer_tparams.hash(state);
                check_type.hash(state);
                extends_type.hash(state);
                true_type.hash(state);
                false_type.hash(state);
            }
            Annot::ObjKeyMirror { loc, obj } => {
                loc.hash(state);
                obj.hash(state);
            }
            Annot::FunAnnot(l, sig) => {
                l.hash(state);
                sig.hash(state);
            }
            Annot::ComponentAnnot(l, sig) => {
                l.hash(state);
                sig.hash(state);
            }
            Annot::MappedTypeAnnot {
                loc,
                source_type,
                property_type,
                key_tparam,
                variance,
                variance_op,
                optional,
                inline_keyof,
            } => {
                loc.hash(state);
                source_type.hash(state);
                property_type.hash(state);
                key_tparam.hash(state);
                variance.hash(state);
                variance_op.hash(state);
                optional.hash(state);
                inline_keyof.hash(state);
            }
            Annot::ObjAnnot {
                loc,
                obj_kind,
                props,
                proto,
            } => {
                loc.hash(state);
                obj_kind.hash(state);
                props.hash(state);
                proto.hash(state);
            }
            Annot::ObjSpreadAnnot { loc, exact, elems } => {
                loc.hash(state);
                exact.hash(state);
                elems.hash(state);
            }
            Annot::InlineInterface(l, sig) => {
                l.hash(state);
                sig.hash(state);
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
            Annot::Maybe(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Annot::Union { loc, t0, t1, ts } => {
                f_loc(cx, loc);
                f_t(cx, t0);
                f_t(cx, t1);
                for t in ts {
                    f_t(cx, t);
                }
            }
            Annot::Intersection { loc, t0, t1, ts } => {
                f_loc(cx, loc);
                f_t(cx, t0);
                f_t(cx, t1);
                for t in ts {
                    f_t(cx, t);
                }
            }
            Annot::Tuple {
                loc,
                elems,
                inexact: _,
            } => {
                f_loc(cx, loc);
                for elem in elems {
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
            Annot::Array(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Annot::ReadOnlyArray(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Annot::SingletonString(loc, _)
            | Annot::SingletonNumber(loc, _, _)
            | Annot::SingletonBigInt(loc, _, _)
            | Annot::SingletonBoolean(loc, _) => f_loc(cx, loc),
            Annot::StringPrefix {
                loc,
                prefix: _,
                remainder,
            } => {
                f_loc(cx, loc);
                if let Some(t) = remainder {
                    f_t(cx, t);
                }
            }
            Annot::StringSuffix {
                loc,
                suffix: _,
                remainder,
            } => {
                f_loc(cx, loc);
                if let Some(t) = remainder {
                    f_t(cx, t);
                }
            }
            Annot::Typeof {
                loc,
                qname: _,
                t,
                targs,
            } => {
                f_loc(cx, loc);
                f_t(cx, t);
                if let Some(targs) = targs {
                    for t in targs {
                        f_t(cx, t);
                    }
                }
            }
            Annot::Bound { ref_loc, name: _ } => f_loc(cx, ref_loc),
            Annot::NoInfer(t) => f_t(cx, t),
            Annot::PropertyType { loc, obj, prop: _ } => {
                f_loc(cx, loc);
                f_t(cx, obj);
            }
            Annot::ElementType { loc, obj, elem } => {
                f_loc(cx, loc);
                f_t(cx, obj);
                f_t(cx, elem);
            }
            Annot::EnumValue(loc, t) | Annot::Enum(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Annot::OptionalIndexedAccessNonMaybeType { loc, obj, index } => {
                f_loc(cx, loc);
                f_t(cx, obj);
                f_t(cx, index);
            }
            Annot::OptionalIndexedAccessResultType {
                loc,
                non_maybe_result,
                void_loc,
            } => {
                f_loc(cx, loc);
                f_t(cx, non_maybe_result);
                f_loc(cx, void_loc);
            }
            Annot::NonMaybeType(loc, t)
            | Annot::ReadOnly(loc, t)
            | Annot::Partial(loc, t)
            | Annot::Required(loc, t)
            | Annot::Keys(loc, t)
            | Annot::Values(loc, t)
            | Annot::Exact(loc, t)
            | Annot::ClassT(loc, t)
            | Annot::ReactElementConfig(loc, t) => {
                f_loc(cx, loc);
                f_t(cx, t);
            }
            Annot::Omit(loc, t1, t2) => {
                f_loc(cx, loc);
                f_t(cx, t1);
                f_t(cx, t2);
            }
            Annot::Renders {
                loc,
                arg,
                variant: _,
            } => {
                f_loc(cx, loc);
                f_t(cx, arg);
            }
            Annot::ComponentMissingRenders(loc) | Annot::FunctionBind(loc) => f_loc(cx, loc),
            Annot::ExportsT(loc, _) => f_loc(cx, loc),
            Annot::Conditional {
                loc,
                distributive_tparam,
                infer_tparams,
                check_type,
                extends_type,
                true_type,
                false_type,
            } => {
                f_loc(cx, loc);
                if let Some(tp) = distributive_tparam {
                    f_loc(cx, &tp.name_loc);
                    if let Some(b) = &tp.bound {
                        f_t(cx, b);
                    }
                    if let Some(d) = &tp.default {
                        f_t(cx, d);
                    }
                }
                infer_tparams.iter(cx, f_loc, f_t);
                f_t(cx, check_type);
                f_t(cx, extends_type);
                f_t(cx, true_type);
                f_t(cx, false_type);
            }
            Annot::ObjKeyMirror { loc, obj } => {
                f_loc(cx, loc);
                f_t(cx, obj);
            }
            Annot::FunAnnot(loc, sig) => {
                f_loc(cx, loc);
                sig.iter(cx, f_loc, f_t);
            }
            Annot::ComponentAnnot(loc, sig) => {
                f_loc(cx, loc);
                sig.iter(cx, f_loc, f_t);
            }
            Annot::MappedTypeAnnot {
                loc,
                source_type,
                property_type,
                key_tparam,
                variance: _,
                variance_op: _,
                optional: _,
                inline_keyof: _,
            } => {
                f_loc(cx, loc);
                f_t(cx, source_type);
                f_t(cx, property_type);
                f_loc(cx, &key_tparam.name_loc);
                if let Some(b) = &key_tparam.bound {
                    f_t(cx, b);
                }
                if let Some(d) = &key_tparam.default {
                    f_t(cx, d);
                }
            }
            Annot::ObjAnnot {
                loc,
                obj_kind,
                props,
                proto,
            } => {
                f_loc(cx, loc);
                match obj_kind {
                    ObjKind::ExactObj | ObjKind::InexactObj => {}
                    ObjKind::IndexedObj(dict) => {
                        f_t(cx, &dict.key);
                        f_t(cx, &dict.value);
                    }
                }
                for v in props.values() {
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
                match proto {
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
            Annot::ObjSpreadAnnot {
                loc,
                exact: _,
                elems,
            } => {
                f_loc(cx, loc);
                for elem in elems.iter() {
                    match elem {
                        ObjSpreadAnnotElem::ObjSpreadAnnotElem(t) => f_t(cx, t),
                        ObjSpreadAnnotElem::ObjSpreadAnnotSlice { dict, props } => {
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
                        }
                    }
                }
            }
            Annot::InlineInterface(loc, sig) => {
                f_loc(cx, loc);
                sig.iter(cx, f_loc, f_t);
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
            Annot::Maybe(loc, t) => Annot::Maybe(f_loc(cx, loc), f_t(cx, t)),
            Annot::Union { loc, t0, t1, ts } => Annot::Union {
                loc: f_loc(cx, loc),
                t0: f_t(cx, t0),
                t1: f_t(cx, t1),
                ts: ts.iter().map(|t| f_t(cx, t)).collect(),
            },
            Annot::Intersection { loc, t0, t1, ts } => Annot::Intersection {
                loc: f_loc(cx, loc),
                t0: f_t(cx, t0),
                t1: f_t(cx, t1),
                ts: ts.iter().map(|t| f_t(cx, t)).collect(),
            },
            Annot::Tuple {
                loc,
                elems,
                inexact,
            } => Annot::Tuple {
                loc: f_loc(cx, loc),
                elems: elems
                    .iter()
                    .map(|elem| elem.map(cx, &f_loc, &f_t))
                    .collect(),
                inexact: *inexact,
            },
            Annot::Array(loc, t) => Annot::Array(f_loc(cx, loc), f_t(cx, t)),
            Annot::ReadOnlyArray(loc, t) => Annot::ReadOnlyArray(f_loc(cx, loc), f_t(cx, t)),
            Annot::SingletonString(loc, s) => Annot::SingletonString(f_loc(cx, loc), s.dupe()),
            Annot::SingletonNumber(loc, n, s) => {
                Annot::SingletonNumber(f_loc(cx, loc), *n, s.dupe())
            }
            Annot::SingletonBigInt(loc, i, s) => {
                Annot::SingletonBigInt(f_loc(cx, loc), *i, s.dupe())
            }
            Annot::SingletonBoolean(loc, b) => Annot::SingletonBoolean(f_loc(cx, loc), *b),
            Annot::StringPrefix {
                loc,
                prefix,
                remainder,
            } => Annot::StringPrefix {
                loc: f_loc(cx, loc),
                prefix: prefix.dupe(),
                remainder: remainder.as_ref().map(|t| f_t(cx, t)),
            },
            Annot::StringSuffix {
                loc,
                suffix,
                remainder,
            } => Annot::StringSuffix {
                loc: f_loc(cx, loc),
                suffix: suffix.dupe(),
                remainder: remainder.as_ref().map(|t| f_t(cx, t)),
            },
            Annot::Typeof {
                loc,
                qname,
                t,
                targs,
            } => Annot::Typeof {
                loc: f_loc(cx, loc),
                qname: qname.clone(),
                t: f_t(cx, t),
                targs: targs
                    .as_ref()
                    .map(|targs| targs.iter().map(|t| f_t(cx, t)).collect()),
            },
            Annot::Bound { ref_loc, name } => Annot::Bound {
                ref_loc: f_loc(cx, ref_loc),
                name: name.dupe(),
            },
            Annot::NoInfer(t) => Annot::NoInfer(f_t(cx, t)),
            Annot::PropertyType { loc, obj, prop } => Annot::PropertyType {
                loc: f_loc(cx, loc),
                obj: f_t(cx, obj),
                prop: prop.dupe(),
            },
            Annot::ElementType { loc, obj, elem } => Annot::ElementType {
                loc: f_loc(cx, loc),
                obj: f_t(cx, obj),
                elem: f_t(cx, elem),
            },
            Annot::EnumValue(loc, t) => Annot::EnumValue(f_loc(cx, loc), f_t(cx, t)),
            Annot::Enum(loc, t) => Annot::Enum(f_loc(cx, loc), f_t(cx, t)),
            Annot::OptionalIndexedAccessNonMaybeType { loc, obj, index } => {
                Annot::OptionalIndexedAccessNonMaybeType {
                    loc: f_loc(cx, loc),
                    obj: f_t(cx, obj),
                    index: f_t(cx, index),
                }
            }
            Annot::OptionalIndexedAccessResultType {
                loc,
                non_maybe_result,
                void_loc,
            } => Annot::OptionalIndexedAccessResultType {
                loc: f_loc(cx, loc),
                non_maybe_result: f_t(cx, non_maybe_result),
                void_loc: f_loc(cx, void_loc),
            },
            Annot::NonMaybeType(loc, t) => Annot::NonMaybeType(f_loc(cx, loc), f_t(cx, t)),
            Annot::Omit(loc, t1, t2) => Annot::Omit(f_loc(cx, loc), f_t(cx, t1), f_t(cx, t2)),
            Annot::ReadOnly(loc, t) => Annot::ReadOnly(f_loc(cx, loc), f_t(cx, t)),
            Annot::Partial(loc, t) => Annot::Partial(f_loc(cx, loc), f_t(cx, t)),
            Annot::Required(loc, t) => Annot::Required(f_loc(cx, loc), f_t(cx, t)),
            Annot::Keys(loc, t) => Annot::Keys(f_loc(cx, loc), f_t(cx, t)),
            Annot::Renders { loc, arg, variant } => Annot::Renders {
                loc: f_loc(cx, loc),
                arg: f_t(cx, arg),
                variant: *variant,
            },
            Annot::ComponentMissingRenders(loc) => Annot::ComponentMissingRenders(f_loc(cx, loc)),
            Annot::Values(loc, t) => Annot::Values(f_loc(cx, loc), f_t(cx, t)),
            Annot::Exact(loc, t) => Annot::Exact(f_loc(cx, loc), f_t(cx, t)),
            Annot::ExportsT(loc, userland) => Annot::ExportsT(f_loc(cx, loc), userland.clone()),
            Annot::Conditional {
                loc,
                distributive_tparam,
                infer_tparams,
                check_type,
                extends_type,
                true_type,
                false_type,
            } => Annot::Conditional {
                loc: f_loc(cx, loc),
                distributive_tparam: distributive_tparam
                    .as_ref()
                    .map(|tp| tp.map(cx, &f_loc, &f_t)),
                infer_tparams: infer_tparams.map(cx, &f_loc, &f_t),
                check_type: f_t(cx, check_type),
                extends_type: f_t(cx, extends_type),
                true_type: f_t(cx, true_type),
                false_type: f_t(cx, false_type),
            },
            Annot::ObjKeyMirror { loc, obj } => Annot::ObjKeyMirror {
                loc: f_loc(cx, loc),
                obj: f_t(cx, obj),
            },
            Annot::ClassT(loc, t) => Annot::ClassT(f_loc(cx, loc), f_t(cx, t)),
            Annot::FunctionBind(loc) => Annot::FunctionBind(f_loc(cx, loc)),
            Annot::ReactElementConfig(loc, t) => {
                Annot::ReactElementConfig(f_loc(cx, loc), f_t(cx, t))
            }
            Annot::FunAnnot(loc, sig) => Annot::FunAnnot(f_loc(cx, loc), sig.map(cx, &f_loc, &f_t)),
            Annot::ComponentAnnot(loc, sig) => {
                Annot::ComponentAnnot(f_loc(cx, loc), sig.map(cx, &f_loc, &f_t))
            }
            Annot::MappedTypeAnnot {
                loc,
                source_type,
                property_type,
                key_tparam,
                variance,
                variance_op,
                optional,
                inline_keyof,
            } => Annot::MappedTypeAnnot {
                loc: f_loc(cx, loc),
                source_type: f_t(cx, source_type),
                property_type: f_t(cx, property_type),
                key_tparam: key_tparam.map(cx, &f_loc, &f_t),
                variance: *variance,
                variance_op: *variance_op,
                optional: *optional,
                inline_keyof: *inline_keyof,
            },
            Annot::ObjAnnot {
                loc,
                obj_kind,
                props,
                proto,
            } => Annot::ObjAnnot {
                loc: f_loc(cx, loc),
                obj_kind: match obj_kind {
                    ObjKind::ExactObj => ObjKind::ExactObj,
                    ObjKind::InexactObj => ObjKind::InexactObj,
                    ObjKind::IndexedObj(dict) => ObjKind::IndexedObj(dict.map(cx, &f_t)),
                },
                props: props
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.map(cx, &f_loc, &f_t)))
                    .collect(),
                proto: proto.map(cx, &f_loc, &f_t),
            },
            Annot::ObjSpreadAnnot { loc, exact, elems } => Annot::ObjSpreadAnnot {
                loc: f_loc(cx, loc),
                exact: *exact,
                elems: elems.mapped_ref(|elem| elem.map(cx, &f_loc, &f_t)),
            },
            Annot::InlineInterface(loc, sig) => {
                Annot::InlineInterface(f_loc(cx, loc), sig.map(cx, &f_loc, &f_t))
            }
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
