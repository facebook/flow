/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;

use flow_parser::file_key::FileKey;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::ordered_set::OrderedSet;

use crate::haste_module_info::HasteModuleInfo;

// Signature for module names. Such names are assumed to be "resolved", and have
// global scope in the sense that they may be used to identify modules uniquely
// in a code base. In contrast, module references (strings that are used to
// refer to modules in require/import statements) have local scope in the sense
// that their meaning is relative to the files they appear in.
//
// There are two ways to construct a module name:
//
// * A module name may be a String that is declared in a file: e.g., in the
// Haste module system such module names are declared via @providesModule.
//
// * A module name may be a Filename: e.g., in the Node module system a module
// is simply known by its path in the file system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Modulename {
    Haste(HasteModuleInfo),
    Filename(FileKey),
}

impl Modulename {
    pub fn as_str(&self) -> &str {
        match self {
            Modulename::Haste(m) => m.module_name().as_str(),
            Modulename::Filename(f) => f.as_str(),
        }
    }

    pub fn eponymous_module(file: FileKey) -> Self {
        let chopped = flow_common::files::chop_declaration_ext(&file);
        Modulename::Filename(chopped)
    }
}

impl Ord for Modulename {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Modulename::Haste(a), Modulename::Haste(b)) => a.cmp(b),
            (Modulename::Filename(a), Modulename::Filename(b)) => a.as_str().cmp(b.as_str()),
            (Modulename::Haste(_), Modulename::Filename(_)) => Ordering::Less,
            (Modulename::Filename(_), Modulename::Haste(_)) => Ordering::Greater,
        }
    }
}

impl PartialOrd for Modulename {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for Modulename {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub type ModulenameSet = OrderedSet<Modulename>;
pub type ModulenameMap<V> = OrderedMap<Modulename, V>;
