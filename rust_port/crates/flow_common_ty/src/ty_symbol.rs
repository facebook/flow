/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_parser::loc_sig::LocSig;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImportMode {
    ValueMode,
    TypeMode,
    TypeofMode,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImportedIdent<L>(pub L, pub String, pub ImportMode);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RemoteInfo<L> {
    pub imported_as: Option<ImportedIdent<L>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Provenance<L> {
    Local,
    Remote(RemoteInfo<L>),
    Library(RemoteInfo<L>),
    Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<L> {
    pub sym_provenance: Provenance<L>,
    pub sym_def_loc: L,
    pub sym_name: Name,
    pub sym_anonymous: bool,
}

pub type ALocImportedIdent = ImportedIdent<ALoc>;
pub type ALocRemoteInfo = RemoteInfo<ALoc>;
pub type ALocProvenance = Provenance<ALoc>;
pub type ALocSymbol = Symbol<ALoc>;

pub fn builtin_symbol(name: Name) -> ALocSymbol {
    Symbol {
        sym_provenance: Provenance::Builtin,
        sym_def_loc: ALoc::none(),
        sym_name: name,
        sym_anonymous: false,
    }
}

impl<L: dupe::Dupe> ImportedIdent<L> {
    pub fn map_locs<F, M>(&self, f: &F) -> ImportedIdent<M>
    where
        F: Fn(&L) -> M,
        M: dupe::Dupe,
    {
        ImportedIdent(f(&self.0), self.1.clone(), self.2)
    }
}

impl<L: dupe::Dupe> RemoteInfo<L> {
    pub fn map_locs<F, M>(&self, f: &F) -> RemoteInfo<M>
    where
        F: Fn(&L) -> M,
        M: dupe::Dupe,
    {
        RemoteInfo {
            imported_as: self.imported_as.as_ref().map(|id| id.map_locs(f)),
        }
    }
}

impl<L: dupe::Dupe> Provenance<L> {
    pub fn map_locs<F, M>(&self, f: &F) -> Provenance<M>
    where
        F: Fn(&L) -> M,
        M: dupe::Dupe,
    {
        match self {
            Provenance::Local => Provenance::Local,
            Provenance::Remote(ri) => Provenance::Remote(ri.map_locs(f)),
            Provenance::Library(ri) => Provenance::Library(ri.map_locs(f)),
            Provenance::Builtin => Provenance::Builtin,
        }
    }
}

impl<L: dupe::Dupe> Symbol<L> {
    pub fn map_locs<F, M>(&self, f: &F) -> Symbol<M>
    where
        F: Fn(&L) -> M,
        M: dupe::Dupe,
    {
        Symbol {
            sym_provenance: self.sym_provenance.map_locs(f),
            sym_def_loc: f(&self.sym_def_loc),
            sym_name: self.sym_name.dupe(),
            sym_anonymous: self.sym_anonymous,
        }
    }
}
