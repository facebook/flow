/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashMap;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use vec1::Vec1;

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, Hash)]
pub enum Kind {
    Var,
    Let,
    ThisAnnot,
    Const,
    DeclaredVar,
    DeclaredLet,
    DeclaredConst,
    Type {
        imported: bool,
        type_only_namespace: bool,
    },
    Interface {
        imported: bool,
        type_only_namespace: bool,
    },
    Enum,
    Function,
    Class,
    DeclaredClass,
    DeclaredNamespace,
    Parameter,
    CatchParameter,
    Import,
    TsImport,
    DeclaredFunction,
    Internal,
    GeneratorNext,
    Component,
    ComponentParameter,
    Record,
}

#[derive(Clone)]
pub struct Entry<Loc> {
    pub loc: Loc,
    pub name: FlowSmolStr,
    pub kind: Kind,
}

#[derive(Clone)]
pub struct Bindings<Loc>(Vec<Entry<Loc>>);

impl<Loc: Dupe> Bindings<Loc> {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn singleton(entry: Entry<Loc>) -> Self {
        Self(vec![entry])
    }

    pub fn add(&mut self, entry: Entry<Loc>) {
        self.0.push(entry);
    }

    pub fn push(&mut self, other: Self) {
        self.0.extend(other.0);
    }

    pub fn exists(&self, pred: impl Fn(&Entry<Loc>) -> bool) -> bool {
        self.0.iter().any(pred)
    }

    pub fn to_assoc(&self) -> Vec<(FlowSmolStr, (Kind, Vec1<Loc>))> {
        let mut result = Vec::new();
        let mut map: HashMap<FlowSmolStr, (Kind, Vec1<Loc>)> = HashMap::new();

        for Entry { loc, name, kind } in self.0.iter() {
            match map.get_mut(name) {
                // First kind wins
                Some((_, locs)) => {
                    locs.push(loc.dupe());
                }
                None => {
                    result.push(name.dupe());
                    map.insert(name.dupe(), (*kind, Vec1::new(loc.dupe())));
                }
            }
        }

        result
            .into_iter()
            .map(|name| {
                let (kind, locs) = map.remove(&name).unwrap();
                (name, (kind, locs))
            })
            .collect()
    }

    pub fn to_map(&self) -> BTreeMap<FlowSmolStr, (Kind, Vec1<(Loc, Kind)>)> {
        let mut map: BTreeMap<FlowSmolStr, (Kind, Vec1<(Loc, Kind)>)> = BTreeMap::new();

        for Entry { loc, name, kind } in self.0.iter() {
            match map.get_mut(name) {
                Some((_, entries)) => {
                    entries.push((loc.dupe(), *kind));
                }
                None => {
                    map.insert(name.dupe(), (*kind, Vec1::new((loc.dupe(), *kind))));
                }
            }
        }

        map
    }
}

impl Kind {
    pub fn allow_forward_ref(&self) -> bool {
        match self {
            Self::DeclaredFunction
            | Self::DeclaredClass
            | Self::DeclaredVar
            | Self::DeclaredLet
            | Self::DeclaredConst
            | Self::DeclaredNamespace
            | Self::Var
            | Self::Function
            | Self::Component
            | Self::Interface { .. } => true,
            _ => false,
        }
    }

    pub fn allow_redeclaration(&self) -> bool {
        match self {
            Self::DeclaredFunction | Self::Var | Self::Parameter | Self::Function => true,
            _ => false,
        }
    }
}
