/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser_utils::file_sig::FileSig;
use flow_parser_utils::file_sig::Require;

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Default,
    Named,
    NamedType,
    Namespace,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Source {
    UnresolvedSource(FlowImportSpecifier),
    Global,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub export: FlowSmolStr,
    pub source: Source,
    pub kind: Kind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Imports(Vec<Import>);

impl Imports {
    pub fn new(imports: Vec<Import>) -> Self {
        Self(imports)
    }

    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Import> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add_globals(&mut self, globals: BTreeSet<FlowSmolStr>) {
        for export in globals {
            self.0.push(Import {
                export,
                kind: Kind::Unknown,
                source: Source::Global,
            });
        }
    }
}

pub fn of_file_sig(file_sig: &FileSig) -> Imports {
    let vec = file_sig
        .requires()
        .iter()
        .fold(Vec::new(), |mut acc, require| {
            match require {
                Require::Import {
                    source,
                    named,
                    types,
                    ns,
                    ..
                } => {
                    let import_source = FlowImportSpecifier::userland(source.name().dupe());

                    for (export, names) in named {
                        if export.as_str() == "default" {
                            for name in names.keys() {
                                acc.push(Import {
                                    export: name.dupe(),
                                    kind: Kind::Default,
                                    source: Source::UnresolvedSource(import_source.clone()), // clone needed: FlowImportSpecifier doesn't implement Dupe
                                });
                            }
                        } else {
                            acc.push(Import {
                                export: export.dupe(),
                                kind: Kind::Named,
                                source: Source::UnresolvedSource(import_source.clone()), // clone needed: FlowImportSpecifier doesn't implement Dupe
                            });
                        }
                    }
                    for export in types.keys() {
                        acc.push(Import {
                            export: export.dupe(),
                            kind: Kind::NamedType,
                            source: Source::UnresolvedSource(import_source.clone()), // clone needed: FlowImportSpecifier doesn't implement Dupe
                        });
                    }
                    if ns.is_some() {
                        acc.push(Import {
                            export: FlowSmolStr::new_inline(""),
                            kind: Kind::Namespace,
                            source: Source::UnresolvedSource(import_source),
                        });
                    }
                    acc
                }
                // TODO: Require, ImportDynamic, etc.
                _ => acc,
            }
        });

    Imports(vec)
}
