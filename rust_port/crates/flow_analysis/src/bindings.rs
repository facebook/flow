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
    TypeParam,
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
    Import {
        /// Set for `import * as X`, which binds a namespace: `X.Y` can reach a
        /// type, not only a value. A named or default import cannot.
        namespace: bool,
    },
    TsImport,
    DeclaredFunction,
    Internal,
    Component,
    ComponentParameter,
    Record,
}

/// A variable that the name resolver adds to a scope itself, rather than one the program
/// declares.
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticVar {
    /// Read at the end of a function or component body. Its writes are the `switch` statements
    /// without a `default` through which the end can be reached.
    MaybeExhaustivelyChecked,
    /// The value a generator receives from `next()`. Each `yield` reads it.
    GeneratorNext,
}

/// How a scope identifies a binding: by its name in the source, or as a synthetic variable.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKey {
    Name(FlowSmolStr),
    Synthetic(SyntheticVar),
}

impl VarKey {
    /// The name, if the program declares this variable.
    pub fn into_name(self) -> Option<FlowSmolStr> {
        match self {
            VarKey::Name(name) => Some(name),
            VarKey::Synthetic(_) => None,
        }
    }
}

#[derive(Clone)]
pub struct Entry<Loc> {
    pub loc: Loc,
    pub key: VarKey,
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

    pub fn push(&mut self, mut other: Self) {
        other.0.extend(std::mem::take(&mut self.0));
        self.0 = other.0;
    }

    pub fn exists(&self, pred: impl Fn(&Entry<Loc>) -> bool) -> bool {
        self.0.iter().any(pred)
    }

    pub fn to_assoc(&self) -> Vec<(VarKey, (Kind, Vec1<Loc>))> {
        let mut result = Vec::new();
        let mut map: HashMap<VarKey, (Kind, Vec1<Loc>)> = HashMap::new();

        for Entry { loc, key, kind } in self.0.iter() {
            match map.get_mut(key) {
                // First kind wins
                Some((_, locs)) => {
                    locs.push(loc.dupe());
                }
                None => {
                    result.push(key.dupe());
                    map.insert(key.dupe(), (*kind, Vec1::new(loc.dupe())));
                }
            }
        }

        result
            .into_iter()
            .map(|key| {
                let (kind, locs) = map.remove(&key).unwrap();
                (key, (kind, locs))
            })
            .collect()
    }

    pub fn to_map(&self) -> BTreeMap<VarKey, (Kind, Vec1<(Loc, Kind)>)> {
        let mut map: BTreeMap<VarKey, (Kind, Vec1<(Loc, Kind)>)> = BTreeMap::new();

        for Entry { loc, key, kind } in self.0.iter() {
            match map.get_mut(key) {
                Some((_, entries)) => {
                    entries.push((loc.dupe(), *kind));
                }
                None => {
                    map.insert(key.dupe(), (*kind, Vec1::new((loc.dupe(), *kind))));
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

    /// The namespaces a binding kind populates.
    pub fn namespaces(&self) -> &'static [Namespace] {
        match self {
            Self::Type { .. } | Self::TypeParam | Self::Interface { .. } => &[Namespace::Type],
            Self::Class
            | Self::DeclaredClass
            | Self::Enum
            | Self::DeclaredNamespace
            | Self::Record
            | Self::Import { namespace: true } => &[Namespace::Value, Namespace::Type],
            _ => &[Namespace::Value],
        }
    }

    pub fn same_namespace(&self, other: &Kind) -> bool {
        let ns1 = self.namespaces();
        let ns2 = other.namespaces();
        ns1.iter().any(|n| ns2.contains(n))
    }

    /// Whether two binding kinds for the same name can coexist. Looser than
    /// `!same_namespace`: kinds can coexist when their namespaces are disjoint,
    /// and additionally as a TypeScript-style declaration-merging carve-out
    /// for `Class | DeclaredClass | DeclaredNamespace` paired with `Interface`.
    pub fn can_coexist(&self, other: &Kind) -> bool {
        if !self.same_namespace(other) {
            return true;
        }
        matches!(
            (self, other),
            (
                Self::Class | Self::DeclaredClass | Self::DeclaredNamespace,
                Self::Interface { .. },
            ) | (
                Self::Interface { .. },
                Self::Class | Self::DeclaredClass | Self::DeclaredNamespace,
            )
        )
    }
}

/// TypeScript-style value/type namespace partition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Value,
    Type,
}

impl<Loc: Dupe> Bindings<Loc> {
    /// Splits a `Bindings` into `(value_bindings, type_bindings)` preserving
    /// the original newest-first order within each output. A binding in both
    /// namespaces appears in both halves.
    pub fn split_by_namespace(&self) -> (Bindings<Loc>, Bindings<Loc>) {
        let mut values: Vec<Entry<Loc>> = Vec::new();
        let mut types: Vec<Entry<Loc>> = Vec::new();
        for entry in self.0.iter() {
            let nss = entry.kind.namespaces();
            if nss.contains(&Namespace::Value) {
                values.push(entry.clone());
            }
            if nss.contains(&Namespace::Type) {
                types.push(entry.clone());
            }
        }
        (Bindings(values), Bindings(types))
    }
}
