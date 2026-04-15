/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use flow_common::flow_import_specifier;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::platform_set;
use flow_common::polarity::Polarity;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::loc::Loc;

use crate::compact_table::Index;
use crate::type_sig::*;
use crate::type_sig_parse as parse;
use crate::type_sig_parse::LocNode;
use crate::type_sig_parse::ModuleRefNode;

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum RemoteRef<Loc> {
    Import {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
        remote: FlowSmolStr,
    },
    ImportType {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
        remote: FlowSmolStr,
    },
    ImportTypeof {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
        remote: FlowSmolStr,
    },
    ImportTypeofNs {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
    },
    ImportTypeNs {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
    },
    ImportNs {
        id_loc: Loc,
        name: FlowSmolStr,
        index: Index<FlowImportSpecifier>,
    },
}

impl<Loc> RemoteRef<Loc> {
    pub fn loc(&self) -> &Loc {
        match self {
            RemoteRef::Import { id_loc, .. }
            | RemoteRef::ImportType { id_loc, .. }
            | RemoteRef::ImportTypeof { id_loc, .. }
            | RemoteRef::ImportNs { id_loc, .. }
            | RemoteRef::ImportTypeofNs { id_loc, .. }
            | RemoteRef::ImportTypeNs { id_loc, .. } => id_loc,
        }
    }

    pub fn name(&self) -> &FlowSmolStr {
        match self {
            RemoteRef::Import { name, .. }
            | RemoteRef::ImportType { name, .. }
            | RemoteRef::ImportTypeof { name, .. }
            | RemoteRef::ImportNs { name, .. }
            | RemoteRef::ImportTypeofNs { name, .. }
            | RemoteRef::ImportTypeNs { name, .. } => name,
        }
    }

    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> RemoteRef<Loc2> {
        match self {
            RemoteRef::Import {
                id_loc,
                name,
                index,
                remote,
            } => RemoteRef::Import {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
                remote: remote.dupe(),
            },
            RemoteRef::ImportType {
                id_loc,
                name,
                index,
                remote,
            } => RemoteRef::ImportType {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
                remote: remote.dupe(),
            },
            RemoteRef::ImportTypeof {
                id_loc,
                name,
                index,
                remote,
            } => RemoteRef::ImportTypeof {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
                remote: remote.dupe(),
            },
            RemoteRef::ImportTypeofNs {
                id_loc,
                name,
                index,
            } => RemoteRef::ImportTypeofNs {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
            },
            RemoteRef::ImportTypeNs {
                id_loc,
                name,
                index,
            } => RemoteRef::ImportTypeNs {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
            },
            RemoteRef::ImportNs {
                id_loc,
                name,
                index,
            } => RemoteRef::ImportNs {
                id_loc: f(id_loc),
                name: name.dupe(),
                index: *index,
            },
        }
    }
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum PackedRef<Loc> {
    LocalRef(Box<PackedRefLocal<Loc>>),
    RemoteRef(Box<PackedRefRemote<Loc>>),
    BuiltinRef(Box<PackedRefBuiltin<Loc>>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedRefLocal<Loc> {
    pub ref_loc: Loc,
    pub index: Index<PackedDef<Loc>>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedRefRemote<Loc> {
    pub ref_loc: Loc,
    pub index: Index<RemoteRef<Loc>>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedRefBuiltin<Loc> {
    pub ref_loc: Loc,
    pub type_ref: bool,
    pub name: FlowSmolStr,
}

impl<Loc> PackedRef<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> PackedRef<Loc2> {
        match self {
            PackedRef::LocalRef(inner) => PackedRef::LocalRef(Box::new(PackedRefLocal {
                ref_loc: f(&inner.ref_loc),
                index: Index {
                    index: inner.index.index,
                    _phantom: std::marker::PhantomData,
                },
            })),
            PackedRef::RemoteRef(inner) => PackedRef::RemoteRef(Box::new(PackedRefRemote {
                ref_loc: f(&inner.ref_loc),
                index: Index {
                    index: inner.index.index,
                    _phantom: std::marker::PhantomData,
                },
            })),
            PackedRef::BuiltinRef(inner) => PackedRef::BuiltinRef(Box::new(PackedRefBuiltin {
                ref_loc: f(&inner.ref_loc),
                type_ref: inner.type_ref,
                name: inner.name.dupe(),
            })),
        }
    }
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum TyRef<Loc> {
    Unqualified(PackedRef<Loc>),
    Qualified(Box<TyRefQualified<Loc>>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct TyRefQualified<Loc> {
    pub loc: Loc,
    pub id_loc: Loc,
    pub name: FlowSmolStr,
    pub qualification: Box<TyRef<Loc>>,
}

impl<Loc> TyRef<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> TyRef<Loc2> {
        match self {
            TyRef::Unqualified(r) => TyRef::Unqualified(r.map(f)),
            TyRef::Qualified(inner) => TyRef::Qualified(Box::new(TyRefQualified {
                loc: f(&inner.loc),
                id_loc: f(&inner.id_loc),
                name: inner.name.dupe(),
                qualification: Box::new(inner.qualification.map(f)),
            })),
        }
    }
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum Packed<Loc> {
    Annot(Box<PackedAnnot<Loc>>),
    Value(Box<PackedValue<Loc>>),
    Ref(PackedRef<Loc>),
    TyRef(Box<TyRef<Loc>>),
    TyRefApp(Box<PackedTyRefApp<Loc>>),
    AsyncVoidReturn(Box<Loc>),
    Pattern(Index<Pattern<Loc>>),
    Err(Box<Loc>),
    Eval(Box<PackedEval<Loc>>),
    Require(Box<PackedLocIndex<Loc>>),
    ImportDynamic(Box<PackedLocIndex<Loc>>),
    ModuleRef(Box<PackedLocIndex<Loc>>),
    ImportTypeAnnot(Box<PackedLocIndex<Loc>>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedTyRefApp<Loc> {
    pub loc: Loc,
    pub name: TyRef<Loc>,
    pub targs: Vec<Packed<Loc>>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedEval<Loc> {
    pub loc: Loc,
    pub packed: Box<Packed<Loc>>,
    pub op: Op<Box<Packed<Loc>>>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct PackedLocIndex<Loc> {
    pub loc: Loc,
    pub index: Index<FlowImportSpecifier>,
}

pub type PackedValue<Loc> = Value<Loc, Packed<Loc>>;
pub type PackedAnnot<Loc> = Annot<Loc, Packed<Loc>>;
pub type PackedDef<Loc> = Def<Loc, Packed<Loc>>;

impl<Loc> Packed<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> Packed<Loc2> {
        match self {
            Packed::Annot(annot) => Packed::Annot(Box::new(annot.map(
                &mut (),
                |_: &mut (), loc| f(loc),
                |_: &mut (), t: &Packed<Loc>| t.map(f),
            ))),
            Packed::Value(value) => Packed::Value(Box::new(value.map(
                &mut (),
                &|_: &mut (), loc| f(loc),
                &|_: &mut (), t: &Packed<Loc>| t.map(f),
            ))),
            Packed::Ref(r) => Packed::Ref(r.map(f)),
            Packed::TyRef(r) => Packed::TyRef(Box::new(r.map(f))),
            Packed::TyRefApp(inner) => Packed::TyRefApp(Box::new(PackedTyRefApp {
                loc: f(&inner.loc),
                name: inner.name.map(f),
                targs: inner.targs.iter().map(|t| t.map(f)).collect(),
            })),
            Packed::AsyncVoidReturn(loc) => Packed::AsyncVoidReturn(Box::new(f(loc))),
            Packed::Pattern(index) => Packed::Pattern(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
            Packed::Err(loc) => Packed::Err(Box::new(f(loc))),
            Packed::Eval(inner) => Packed::Eval(Box::new(PackedEval {
                loc: f(&inner.loc),
                packed: Box::new(inner.packed.map(f)),
                op: inner.op.map(&mut (), |_: &mut (), t| Box::new(t.map(f))),
            })),
            Packed::Require(inner) => Packed::Require(Box::new(PackedLocIndex {
                loc: f(&inner.loc),
                index: inner.index,
            })),
            Packed::ImportDynamic(inner) => Packed::ImportDynamic(Box::new(PackedLocIndex {
                loc: f(&inner.loc),
                index: inner.index,
            })),
            Packed::ModuleRef(inner) => Packed::ModuleRef(Box::new(PackedLocIndex {
                loc: f(&inner.loc),
                index: inner.index,
            })),
            Packed::ImportTypeAnnot(inner) => Packed::ImportTypeAnnot(Box::new(PackedLocIndex {
                loc: f(&inner.loc),
                index: inner.index,
            })),
        }
    }
}

impl<Loc> Export<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> Export<Loc2> {
        match self {
            Export::ExportRef(r) => Export::ExportRef(r.map(f)),
            Export::ExportBinding(index) => Export::ExportBinding(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
            Export::ExportDefault(inner) => Export::ExportDefault(Box::new(ExportDefaultData {
                default_loc: f(&inner.default_loc),
                def: inner.def.map(f),
            })),
            Export::ExportDefaultBinding(inner) => {
                Export::ExportDefaultBinding(Box::new(ExportDefaultBindingData {
                    default_loc: f(&inner.default_loc),
                    index: Index {
                        index: inner.index.index,
                        _phantom: std::marker::PhantomData,
                    },
                }))
            }
            Export::ExportFrom(index) => Export::ExportFrom(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
        }
    }
}

impl<Loc> TypeExport<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> TypeExport<Loc2> {
        match self {
            TypeExport::ExportTypeRef(r) => TypeExport::ExportTypeRef(r.map(f)),
            TypeExport::ExportTypeBinding(index) => TypeExport::ExportTypeBinding(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
            TypeExport::ExportTypeFrom(index) => TypeExport::ExportTypeFrom(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
        }
    }
}

impl<Loc> CJSModuleInfo<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> CJSModuleInfo<Loc2> {
        CJSModuleInfo {
            type_export_keys: self.type_export_keys.clone(),
            type_stars: self
                .type_stars
                .iter()
                .map(|(loc, idx)| (f(loc), *idx))
                .collect(),
            strict: self.strict,
            platform_availability_set: self.platform_availability_set.clone(),
        }
    }
}

impl<Loc> ESModuleInfo<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> ESModuleInfo<Loc2> {
        ESModuleInfo {
            type_export_keys: self.type_export_keys.clone(),
            type_stars: self
                .type_stars
                .iter()
                .map(|(loc, idx)| (f(loc), *idx))
                .collect(),
            export_keys: self.export_keys.clone(),
            stars: self.stars.iter().map(|(loc, idx)| (f(loc), *idx)).collect(),
            strict: self.strict,
            platform_availability_set: self.platform_availability_set.clone(),
        }
    }
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum Export<Loc> {
    ExportRef(PackedRef<Loc>),
    ExportBinding(Index<PackedDef<Loc>>),
    ExportDefault(Box<ExportDefaultData<Loc>>),
    ExportDefaultBinding(Box<ExportDefaultBindingData<Loc>>),
    ExportFrom(Index<RemoteRef<Loc>>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct ExportDefaultData<Loc> {
    pub default_loc: Loc,
    pub def: Packed<Loc>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct ExportDefaultBindingData<Loc> {
    pub default_loc: Loc,
    pub index: Index<PackedDef<Loc>>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum TypeExport<Loc> {
    ExportTypeRef(PackedRef<Loc>),
    ExportTypeBinding(Index<PackedDef<Loc>>),
    ExportTypeFrom(Index<RemoteRef<Loc>>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct CJSModuleInfo<Loc> {
    pub type_export_keys: Vec<FlowSmolStr>,
    pub type_stars: Vec<(Loc, Index<FlowImportSpecifier>)>,
    pub strict: bool,
    pub platform_availability_set: Option<platform_set::PlatformSet>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub struct ESModuleInfo<Loc> {
    pub type_export_keys: Vec<FlowSmolStr>,
    pub type_stars: Vec<(Loc, Index<FlowImportSpecifier>)>,
    pub export_keys: Vec<FlowSmolStr>,
    pub stars: Vec<(Loc, Index<FlowImportSpecifier>)>,
    pub strict: bool,
    pub platform_availability_set: Option<platform_set::PlatformSet>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum ModuleKind<Loc> {
    CJSModule {
        type_exports: Vec<TypeExport<Loc>>,
        exports: Option<Packed<Loc>>,
        info: CJSModuleInfo<Loc>,
    },
    ESModule {
        type_exports: Vec<TypeExport<Loc>>,
        exports: Vec<Export<Loc>>,
        info: ESModuleInfo<Loc>,
    },
}

impl<Loc> ModuleKind<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> ModuleKind<Loc2> {
        match self {
            ModuleKind::CJSModule {
                type_exports,
                exports,
                info,
            } => ModuleKind::CJSModule {
                type_exports: type_exports.iter().map(|e| e.map(f)).collect(),
                exports: exports.as_ref().map(|e| e.map(f)),
                info: info.map(f),
            },
            ModuleKind::ESModule {
                type_exports,
                exports,
                info,
            } => ModuleKind::ESModule {
                type_exports: type_exports.iter().map(|e| e.map(f)).collect(),
                exports: exports.iter().map(|e| e.map(f)).collect(),
                info: info.map(f),
            },
        }
    }
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub enum Pattern<Loc> {
    PDef(Index<Packed<Loc>>),
    PropP {
        id_loc: Loc,
        name: FlowSmolStr,
        def: Index<Pattern<Loc>>,
    },
    ComputedP {
        elem: Index<Packed<Loc>>,
        def: Index<Pattern<Loc>>,
    },
    UnsupportedLiteralP(Loc),
    ObjRestP {
        loc: Loc,
        xs: Vec<FlowSmolStr>,
        def: Index<Pattern<Loc>>,
    },
    IndexP {
        loc: Loc,
        i: usize,
        def: Index<Pattern<Loc>>,
    },
    ArrRestP {
        loc: Loc,
        i: usize,
        def: Index<Pattern<Loc>>,
    },
}

impl<Loc> Pattern<Loc> {
    pub fn map<Loc2>(&self, f: &impl Fn(&Loc) -> Loc2) -> Pattern<Loc2> {
        match self {
            Pattern::PDef(index) => Pattern::PDef(Index {
                index: index.index,
                _phantom: std::marker::PhantomData,
            }),
            Pattern::PropP { id_loc, name, def } => Pattern::PropP {
                id_loc: f(id_loc),
                name: name.dupe(),
                def: Index {
                    index: def.index,
                    _phantom: std::marker::PhantomData,
                },
            },
            Pattern::ComputedP { elem, def } => Pattern::ComputedP {
                elem: Index {
                    index: elem.index,
                    _phantom: std::marker::PhantomData,
                },
                def: Index {
                    index: def.index,
                    _phantom: std::marker::PhantomData,
                },
            },
            Pattern::UnsupportedLiteralP(loc) => Pattern::UnsupportedLiteralP(f(loc)),
            Pattern::ObjRestP { loc, xs, def } => Pattern::ObjRestP {
                loc: f(loc),
                xs: xs.clone(),
                def: Index {
                    index: def.index,
                    _phantom: std::marker::PhantomData,
                },
            },
            Pattern::IndexP { loc, i, def } => Pattern::IndexP {
                loc: f(loc),
                i: *i,
                def: Index {
                    index: def.index,
                    _phantom: std::marker::PhantomData,
                },
            },
            Pattern::ArrRestP { loc, i, def } => Pattern::ArrRestP {
                loc: f(loc),
                i: *i,
                def: Index {
                    index: def.index,
                    _phantom: std::marker::PhantomData,
                },
            },
        }
    }
}

pub(super) struct Cx {
    errs: Vec<Errno<Index<Loc>>>,
}

impl Cx {
    pub(super) fn new(errs: Vec<Errno<Index<Loc>>>) -> Self {
        Self { errs }
    }

    pub(super) fn take_errs(self) -> Vec<Errno<Index<Loc>>> {
        self.errs
    }
}

fn pack_loc<'arena>(loc: &LocNode<'arena>) -> Index<Loc> {
    loc.0.index_exn()
}

fn pack_star<'arena>(
    loc: &LocNode<'arena>,
    mref: &ModuleRefNode<'arena>,
) -> (Index<Loc>, Index<FlowImportSpecifier>) {
    (pack_loc(loc), mref.0.index_exn())
}

pub(crate) fn pack_parsed<'arena, 'ast>(
    cx: &mut Cx,
    parsed: &parse::Parsed<'arena, 'ast>,
) -> Packed<Index<Loc>> {
    match parsed {
        parse::Parsed::Annot(t) => Packed::Annot(Box::new(pack_annot(cx, t))),
        parse::Parsed::Value(def) => Packed::Value(Box::new(pack_value(cx, def))),
        parse::Parsed::TyRef(name) => Packed::TyRef(Box::new(pack_tyname(name))),
        parse::Parsed::TyRefApp { loc, name, targs } => pack_tyapp(cx, loc, name, targs),
        parse::Parsed::AsyncVoidReturn(loc) => Packed::AsyncVoidReturn(Box::new(pack_loc(loc))),
        parse::Parsed::BuiltinTyRef { ref_loc, name } => {
            let ref_loc = pack_loc(ref_loc);
            Packed::TyRef(Box::new(TyRef::Unqualified(PackedRef::BuiltinRef(
                Box::new(PackedRefBuiltin {
                    ref_loc,
                    type_ref: true,
                    name: name.dupe(),
                }),
            ))))
        }
        parse::Parsed::Err(loc, err) => {
            let loc = pack_loc(loc);
            let e = err.map(cx, |_cx, loc| pack_loc(loc));
            cx.errs.push(e);
            Packed::Err(Box::new(loc))
        }
        parse::Parsed::ValRef { type_only, ref_ } => Packed::Ref(pack_ref(*type_only, ref_)),
        parse::Parsed::Pattern(p) => Packed::Pattern(p.0.index_exn()),
        parse::Parsed::Eval(loc, t, op) => Packed::Eval(Box::new(PackedEval {
            loc: pack_loc(loc),
            packed: Box::new(pack_parsed(cx, t)),
            op: pack_op(cx, op),
        })),
        parse::Parsed::Require { loc, mref } => {
            let loc = pack_loc(loc);
            Packed::Require(Box::new(PackedLocIndex {
                loc,
                index: mref.0.index_exn(),
            }))
        }
        parse::Parsed::ImportDynamic { loc, mref } => {
            let loc = pack_loc(loc);
            Packed::ImportDynamic(Box::new(PackedLocIndex {
                loc,
                index: mref.0.index_exn(),
            }))
        }
        parse::Parsed::ModuleRef { loc, mref } => {
            let loc = pack_loc(loc);
            Packed::ModuleRef(Box::new(PackedLocIndex {
                loc,
                index: mref.0.index_exn(),
            }))
        }
        parse::Parsed::ImportTypeAnnot { loc, mref } => {
            let loc = pack_loc(loc);
            Packed::ImportTypeAnnot(Box::new(PackedLocIndex {
                loc,
                index: mref.0.index_exn(),
            }))
        }
    }
}

fn pack_tyapp<'arena, 'ast>(
    cx: &mut Cx,
    loc: &LocNode<'arena>,
    name: &parse::TyName<'arena, 'ast>,
    targs: &[parse::Parsed<'arena, 'ast>],
) -> Packed<Index<Loc>> {
    let loc = pack_loc(loc);
    let name = pack_tyname(name);
    let targs = targs.iter().map(|t| pack_parsed(cx, t)).collect();
    Packed::TyRefApp(Box::new(PackedTyRefApp { loc, name, targs }))
}

fn pack_tyname<'arena, 'ast>(name: &parse::TyName<'arena, 'ast>) -> TyRef<Index<Loc>> {
    match name {
        parse::TyName::Unqualified(ref_) => TyRef::Unqualified(pack_ref(true, ref_)),
        parse::TyName::Qualified {
            loc,
            id_loc,
            name,
            qualification,
        } => {
            let loc = pack_loc(loc);
            let id_loc = pack_loc(id_loc);
            TyRef::Qualified(Box::new(TyRefQualified {
                loc,
                id_loc,
                name: name.dupe(),
                qualification: Box::new(pack_tyname(qualification)),
            }))
        }
    }
}

fn pack_ref(type_ref: bool, ref_: &parse::Ref<'_, '_>) -> PackedRef<Index<Loc>> {
    let parse::Ref {
        ref_loc,
        name,
        scope: _,
        resolved,
    } = ref_;
    let ref_loc = pack_loc(ref_loc);
    match resolved.get() {
        Some(Some(parse::BindingNode::LocalBinding(b))) => {
            PackedRef::LocalRef(Box::new(PackedRefLocal {
                ref_loc,
                index: b.0.index_exn(),
            }))
        }
        Some(Some(parse::BindingNode::RemoteBinding(b))) => {
            PackedRef::RemoteRef(Box::new(PackedRefRemote {
                ref_loc,
                index: b.0.index_exn(),
            }))
        }
        _ => PackedRef::BuiltinRef(Box::new(PackedRefBuiltin {
            ref_loc,
            type_ref,
            name: name.dupe(),
        })),
    }
}

pub(crate) fn pack_local_binding<'arena, 'ast>(
    cx: &mut Cx,
    binding: &parse::LocalBinding<'arena, 'ast>,
) -> PackedDef<Index<Loc>> {
    match binding {
        parse::LocalBinding::TypeBinding { id_loc: _, def } => {
            let def = def.as_already_forced();
            pack_def(cx, def)
        }
        parse::LocalBinding::VarBinding { id_loc, name, def } => {
            let id_loc = pack_loc(id_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_parsed(cx, parsed)
            };
            Def::Variable(Box::new(DefVariable {
                id_loc,
                name: name.dupe(),
                def,
            }))
        }
        parse::LocalBinding::LetConstBinding { id_loc, name, def } => {
            let id_loc = pack_loc(id_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_parsed(cx, parsed)
            };
            Def::Variable(Box::new(DefVariable {
                id_loc,
                name: name.dupe(),
                def,
            }))
        }
        parse::LocalBinding::ParamBinding {
            id_loc,
            name,
            def,
            tparams,
        } => {
            let id_loc = pack_loc(id_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_parsed(cx, parsed)
            };
            let tparams = pack_tparams(cx, tparams);
            Def::Parameter(Box::new(DefParameter {
                id_loc,
                name: name.dupe(),
                def,
                tparams,
            }))
        }
        parse::LocalBinding::ConstRefBinding { id_loc, name, ref_ } => {
            let id_loc = pack_loc(id_loc);
            let def = Packed::Ref(pack_ref(false, ref_));
            Def::Variable(Box::new(DefVariable {
                id_loc,
                name: name.dupe(),
                def,
            }))
        }
        parse::LocalBinding::ConstFunBinding {
            id_loc,
            name,
            loc,
            async_,
            generator,
            def,
            statics,
        } => {
            let id_loc = pack_loc(id_loc);
            let loc = pack_loc(loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_fun(cx, parsed)
            };
            let statics = statics
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            let def = Packed::Value(Box::new(Value::FunExpr(Box::new(ValueFunExpr {
                loc,
                async_: *async_,
                generator: *generator,
                def,
                statics,
            }))));
            Def::Variable(Box::new(DefVariable {
                id_loc,
                name: name.dupe(),
                def,
            }))
        }
        parse::LocalBinding::ClassBinding {
            id_loc,
            name,
            def,
            namespace_types,
        } => {
            let id_loc = pack_loc(id_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_class(cx, parsed)
            };
            let namespace_types = namespace_types
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            Def::ClassBinding(Box::new(DefClassBinding {
                id_loc,
                name: name.dupe(),
                def,
                namespace_types,
            }))
        }
        parse::LocalBinding::DeclareClassBinding {
            id_loc,
            nominal_id_loc,
            name,
            def,
            namespace_types,
        } => {
            let id_loc = pack_loc(id_loc);
            let nominal_id_loc = pack_loc(nominal_id_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_declare_class(cx, parsed)
            };
            let namespace_types = namespace_types
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            Def::DeclareClassBinding(Box::new(DefDeclareClassBinding {
                id_loc,
                nominal_id_loc,
                name: name.dupe(),
                def,
                namespace_types,
            }))
        }
        parse::LocalBinding::RecordBinding {
            id_loc,
            name,
            def,
            defaulted_props,
        } => {
            let id_loc = pack_loc(id_loc);
            match def {
                Some(def) => {
                    let def = {
                        let parsed = def.as_already_forced();
                        pack_class(cx, parsed)
                    };
                    Def::RecordBinding(Box::new(DefRecordBinding {
                        id_loc,
                        name: name.dupe(),
                        def,
                        defaulted_props: defaulted_props.clone(),
                    }))
                }
                None => Def::DisabledRecordBinding(Box::new(DefDisabledRecordBinding {
                    id_loc,
                    name: name.dupe(),
                })),
            }
        }
        parse::LocalBinding::FunBinding {
            id_loc,
            name,
            async_,
            generator,
            fn_loc,
            def,
            statics,
            namespace_types,
        } => {
            let id_loc = pack_loc(id_loc);
            let fn_loc = pack_loc(fn_loc);
            let def = {
                let parsed = def.as_already_forced();
                pack_fun(cx, parsed)
            };
            let statics = statics
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            let namespace_types = namespace_types
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            Def::FunBinding(Box::new(DefFunBinding {
                id_loc,
                name: name.dupe(),
                async_: *async_,
                generator: *generator,
                fn_loc,
                def,
                statics,
                namespace_types,
            }))
        }
        parse::LocalBinding::ComponentBinding {
            id_loc,
            name,
            fn_loc,
            def,
        } => {
            let id_loc = pack_loc(id_loc);
            match def {
                Some(def) => {
                    let fn_loc = pack_loc(fn_loc);
                    let def = {
                        let parsed = def.as_already_forced();
                        pack_component(cx, parsed)
                    };
                    Def::ComponentBinding(Box::new(DefComponentBinding {
                        id_loc,
                        name: name.dupe(),
                        fn_loc,
                        def,
                    }))
                }
                None => Def::DisabledComponentBinding(Box::new(DefDisabledComponentBinding {
                    id_loc,
                    name: name.dupe(),
                })),
            }
        }
        parse::LocalBinding::DeclareFunBinding {
            name,
            defs,
            statics,
            namespace_types,
        } => {
            let mut packed_defs: Vec<_> = defs
                .iter()
                .map(|(id_loc, fn_loc, def)| {
                    let id_loc = pack_loc(id_loc);
                    let fn_loc = pack_loc(fn_loc);
                    let def = {
                        let parsed = def.as_already_forced();
                        pack_fun(cx, parsed)
                    };
                    (id_loc, fn_loc, def)
                })
                .collect();
            let statics = statics
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            let namespace_types = namespace_types
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (k.dupe(), (id_loc, t))
                })
                .collect();
            let (id_loc, fn_loc, def) = packed_defs.remove(0);
            Def::DeclareFun(Box::new(DefDeclareFun {
                id_loc,
                name: name.dupe(),
                fn_loc,
                def,
                statics,
                namespace_types,
                tail: packed_defs,
            }))
        }
        parse::LocalBinding::EnumBinding { id_loc, name, def } => {
            let id_loc = pack_loc(id_loc);
            match def {
                None => Def::DisabledEnumBinding(Box::new(DefDisabledEnumBinding {
                    id_loc,
                    name: name.dupe(),
                })),
                Some(def) => {
                    let (rep, members, has_unknown_members) = def.as_already_forced();
                    let members = members
                        .iter()
                        .map(|(k, loc)| (k.dupe(), pack_loc(loc)))
                        .collect();
                    Def::EnumBinding(Box::new(DefEnumBinding {
                        id_loc,
                        name: name.dupe(),
                        rep: *rep,
                        members,
                        has_unknown_members: *has_unknown_members,
                    }))
                }
            }
        }
        parse::LocalBinding::NamespaceBinding {
            id_loc,
            name,
            values,
            types,
        } => {
            let id_loc = pack_loc(id_loc);
            let values = values
                .iter()
                .map(|(k, (loc, parsed))| (k.dupe(), (pack_loc(loc), pack_parsed(cx, parsed))))
                .collect();
            let types = types
                .iter()
                .map(|(k, (loc, parsed))| (k.dupe(), (pack_loc(loc), pack_parsed(cx, parsed))))
                .collect();
            Def::NamespaceBinding(Box::new(DefNamespaceBinding {
                id_loc,
                name: name.dupe(),
                values,
                types,
            }))
        }
    }
}

pub(crate) fn pack_remote_binding<'arena>(
    binding: &parse::RemoteBinding<'arena>,
) -> RemoteRef<Index<Loc>> {
    match binding {
        parse::RemoteBinding::ImportBinding {
            id_loc,
            name,
            mref,
            remote,
        } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::Import {
                id_loc,
                name: name.dupe(),
                index,
                remote: remote.dupe(),
            }
        }
        parse::RemoteBinding::ImportTypeBinding {
            id_loc,
            name,
            mref,
            remote,
        } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::ImportType {
                id_loc,
                name: name.dupe(),
                index,
                remote: remote.dupe(),
            }
        }
        parse::RemoteBinding::ImportTypeofBinding {
            id_loc,
            name,
            mref,
            remote,
        } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::ImportTypeof {
                id_loc,
                name: name.dupe(),
                index,
                remote: remote.dupe(),
            }
        }
        parse::RemoteBinding::ImportNsBinding { id_loc, name, mref } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::ImportNs {
                id_loc,
                name: name.dupe(),
                index,
            }
        }
        parse::RemoteBinding::ImportTypeofNsBinding { id_loc, name, mref } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::ImportTypeofNs {
                id_loc,
                name: name.dupe(),
                index,
            }
        }
        parse::RemoteBinding::ImportTypeNsBinding { id_loc, name, mref } => {
            let id_loc = pack_loc(id_loc);
            let index = mref.0.index_exn();
            RemoteRef::ImportTypeNs {
                id_loc,
                name: name.dupe(),
                index,
            }
        }
    }
}

pub(crate) fn pack_pattern(pattern: &parse::Pattern<'_, '_>) -> Pattern<Index<Loc>> {
    match pattern {
        parse::Pattern::PDef(def) => {
            let def = def.as_already_forced().0.index_exn();
            Pattern::PDef(def)
        }
        parse::Pattern::PropP { def, id_loc, name } => {
            let id_loc = pack_loc(id_loc);
            let def = def.0.index_exn();
            Pattern::PropP {
                id_loc,
                name: name.dupe(),
                def,
            }
        }
        parse::Pattern::ComputedP { def, elem } => {
            let elem = elem.0.index_exn();
            let def = def.0.index_exn();
            Pattern::ComputedP { elem, def }
        }
        parse::Pattern::UnsupportedLiteralP(loc) => Pattern::UnsupportedLiteralP(pack_loc(loc)),
        parse::Pattern::ObjRestP { def, loc, xs } => {
            let loc = pack_loc(loc);
            let def = def.0.index_exn();
            Pattern::ObjRestP {
                loc,
                xs: xs.clone(),
                def,
            }
        }
        parse::Pattern::IndexP { def, loc, i } => {
            let loc = pack_loc(loc);
            let def = def.0.index_exn();
            Pattern::IndexP { loc, i: *i, def }
        }
        parse::Pattern::ArrRestP { def, loc, i } => {
            let loc = pack_loc(loc);
            let def = def.0.index_exn();
            Pattern::ArrRestP { loc, i: *i, def }
        }
    }
}

pub(crate) fn pack_exports<'arena, 'ast>(
    cx: &mut Cx,
    file_loc: &LocNode<'arena>,
    module_name: &FlowSmolStr,
    exports: &parse::Exports<'arena, 'ast>,
) -> ModuleKind<Index<Loc>> {
    fn pack_btreemap<K: Clone + Ord, V, R>(
        map: &BTreeMap<K, V>,
        mut f: impl FnMut(&V) -> R,
    ) -> (Vec<K>, Vec<R>) {
        let mut keys = Vec::new();
        let mut values = Vec::new();
        for (k, v) in map {
            keys.push(k.clone());
            values.push(f(v));
        }
        (keys, values)
    }

    let parse::Exports {
        kind,
        types,
        type_stars,
        strict,
        platform_availability_set,
    } = exports;

    let (type_export_keys, type_exports) = pack_btreemap(types, pack_type_export);
    // OCaml builds type_stars with :: (prepend), so the list is in reverse
    // declaration order. Rust uses push (append), so we reverse here to match.
    let type_stars = type_stars
        .iter()
        .rev()
        .map(|(loc, mref)| pack_star(loc, mref))
        .collect();

    match kind {
        parse::ModuleKind::UnknownModule => {
            let info = CJSModuleInfo {
                type_export_keys,
                type_stars,
                strict: *strict,
                platform_availability_set: *platform_availability_set,
            };
            ModuleKind::CJSModule {
                type_exports,
                exports: None,
                info,
            }
        }
        parse::ModuleKind::CJSModule(t) => {
            let exports = Some(pack_parsed(cx, t));
            let info = CJSModuleInfo {
                type_export_keys,
                type_stars,
                strict: *strict,
                platform_availability_set: *platform_availability_set,
            };
            ModuleKind::CJSModule {
                type_exports,
                exports,
                info,
            }
        }
        parse::ModuleKind::CJSModuleProps(props) => {
            let file_loc = pack_loc(file_loc);
            let props = props
                .iter()
                .map(|(k, (id_loc, t))| {
                    let id_loc = pack_loc(id_loc);
                    let t = pack_parsed(cx, t);
                    (
                        k.dupe(),
                        ObjValueProp::ObjValueField(Box::new((id_loc, t, Polarity::Neutral))),
                    )
                })
                .collect();
            let exports = Some(Packed::Value(Box::new(Value::ObjLit(Box::new(
                ValueObjLit {
                    loc: file_loc,
                    frozen: true,
                    proto: None,
                    props,
                },
            )))));
            let info = CJSModuleInfo {
                type_export_keys,
                type_stars,
                strict: *strict,
                platform_availability_set: *platform_availability_set,
            };
            ModuleKind::CJSModule {
                type_exports,
                exports,
                info,
            }
        }
        parse::ModuleKind::CJSDeclareModule(props) => {
            let file_loc = pack_loc(file_loc);
            let props = props
                .iter()
                .map(|(k, binding)| {
                    let index = binding.0.index_exn();
                    let t = Packed::Ref(PackedRef::LocalRef(Box::new(PackedRefLocal {
                        ref_loc: file_loc,
                        index,
                    })));
                    (
                        k.dupe(),
                        ObjValueProp::ObjValueField(Box::new((file_loc, t, Polarity::Positive))),
                    )
                })
                .collect();
            let exports = Some(Packed::Value(Box::new(
                Value::DeclareModuleImplicitlyExportedObject(Box::new(
                    ValueDeclareModuleImplicitlyExportedObject {
                        loc: file_loc,
                        module_name: flow_import_specifier::Userland::from_smol_str(
                            module_name.dupe(),
                        ),
                        props,
                    },
                )),
            )));
            let info = CJSModuleInfo {
                type_export_keys,
                type_stars,
                strict: *strict,
                platform_availability_set: *platform_availability_set,
            };
            ModuleKind::CJSModule {
                type_exports,
                exports,
                info,
            }
        }
        parse::ModuleKind::ESModule { names, stars } => {
            let (export_keys, exports) = pack_btreemap(names, |e| pack_export(cx, e));
            // OCaml builds stars with :: (prepend), so the list is in reverse
            // declaration order. Rust uses push (append), so we reverse here to match.
            let stars = stars
                .iter()
                .rev()
                .map(|(loc, mref)| pack_star(loc, mref))
                .collect();
            let info = ESModuleInfo {
                type_export_keys,
                type_stars,
                export_keys,
                stars,
                strict: *strict,
                platform_availability_set: *platform_availability_set,
            };
            ModuleKind::ESModule {
                type_exports,
                exports,
                info,
            }
        }
    }
}

fn pack_export<'arena, 'ast>(
    cx: &mut Cx,
    export: &parse::Export<'arena, 'ast>,
) -> Export<Index<Loc>> {
    match export {
        parse::Export::ExportRef(ref_) => {
            let ref_ = pack_ref(false, ref_);
            Export::ExportRef(ref_)
        }
        parse::Export::ExportBinding(binding) => {
            let index = binding.0.index_exn();
            Export::ExportBinding(index)
        }
        parse::Export::ExportDefault { default_loc, def } => {
            let default_loc = pack_loc(default_loc);
            let def = pack_parsed(cx, def);
            Export::ExportDefault(Box::new(ExportDefaultData { default_loc, def }))
        }
        parse::Export::ExportDefaultBinding {
            default_loc,
            name: _,
            binding,
        } => {
            let default_loc = pack_loc(default_loc);
            let index = binding.0.index_exn();
            Export::ExportDefaultBinding(Box::new(ExportDefaultBindingData { default_loc, index }))
        }
        parse::Export::ExportFrom(ref_) => {
            let index = ref_.0.index_exn();
            Export::ExportFrom(index)
        }
    }
}

fn pack_type_export<'arena, 'ast>(
    export: &parse::ExportType<'arena, 'ast>,
) -> TypeExport<Index<Loc>> {
    match export {
        parse::ExportType::ExportTypeRef(ref_) => {
            let ref_ = pack_ref(true, ref_);
            TypeExport::ExportTypeRef(ref_)
        }
        parse::ExportType::ExportTypeBinding(binding) => {
            let index = binding.0.index_exn();
            TypeExport::ExportTypeBinding(index)
        }
        parse::ExportType::ExportTypeFrom(ref_) => {
            let index = ref_.0.index_exn();
            TypeExport::ExportTypeFrom(index)
        }
    }
}

fn pack_value<'arena, 'ast>(
    cx: &mut Cx,
    value: &Value<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> PackedValue<Index<Loc>> {
    value.map(cx, &|_, loc| pack_loc(loc), &|cx, t| pack_parsed(cx, t))
}

fn pack_def<'arena, 'ast>(
    cx: &mut Cx,
    def: &Def<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> PackedDef<Index<Loc>> {
    def.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_tparams<'arena, 'ast>(
    cx: &mut Cx,
    tparams: &TParams<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> TParams<Index<Loc>, Packed<Index<Loc>>> {
    tparams.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_annot<'arena, 'ast>(
    cx: &mut Cx,
    annot: &Annot<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> PackedAnnot<Index<Loc>> {
    annot.map(cx, |_, loc| pack_loc(loc), pack_parsed)
}

fn pack_fun<'arena, 'ast>(
    cx: &mut Cx,
    def: &FunSig<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> FunSig<Index<Loc>, Packed<Index<Loc>>> {
    def.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_component<'arena, 'ast>(
    cx: &mut Cx,
    def: &ComponentSig<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> ComponentSig<Index<Loc>, Packed<Index<Loc>>> {
    def.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_class<'arena, 'ast>(
    cx: &mut Cx,
    def: &ClassSig<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> ClassSig<Index<Loc>, Packed<Index<Loc>>> {
    def.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_declare_class<'arena, 'ast>(
    cx: &mut Cx,
    def: &DeclareClassSig<LocNode<'arena>, parse::Parsed<'arena, 'ast>>,
) -> DeclareClassSig<Index<Loc>, Packed<Index<Loc>>> {
    def.map(cx, |_, loc| pack_loc(loc), |cx, t| pack_parsed(cx, t))
}

fn pack_op<'arena, 'ast>(
    cx: &mut Cx,
    op: &Op<parse::Parsed<'arena, 'ast>>,
) -> Op<Box<Packed<Index<Loc>>>> {
    op.map(cx, |cx, t| Box::new(pack_parsed(cx, t)))
}

pub(crate) fn pack_builtin<'arena, 'ast>(
    binding: parse::BindingNode<'arena, 'ast>,
) -> Index<PackedDef<Index<Loc>>> {
    match binding {
        parse::BindingNode::LocalBinding(b) => b.0.index_exn(),
        parse::BindingNode::RemoteBinding(_) => panic!("unexpected remote builtin"),
    }
}

pub(crate) fn pack_builtin_module<'arena, 'ast>(
    cx: &mut Cx,
    name: &FlowSmolStr,
    (loc, exports): (LocNode<'arena>, &parse::Exports<'arena, 'ast>),
) -> (Index<Loc>, ModuleKind<Index<Loc>>) {
    let module_kind = pack_exports(cx, &loc, name, exports);
    let loc = pack_loc(&loc);
    (loc, module_kind)
}
