/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::reason::string_of_aloc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolKind {
    SymbolClass,
    SymbolComponent,
    SymbolConstant,
    SymbolConstructor,
    SymbolEnum,
    SymbolEnumMember,
    SymbolFile,
    SymbolFunction,
    SymbolInterface,
    SymbolMethod,
    SymbolModule,
    SymbolNamespace,
    SymbolProperty,
    SymbolTypeAlias,
    SymbolTypeParameter,
    SymbolVariable,
}

impl SymbolKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::SymbolClass => "class",
            Self::SymbolComponent => "component",
            Self::SymbolConstant => "const",
            Self::SymbolConstructor => "constructor",
            Self::SymbolEnum => "enum",
            Self::SymbolEnumMember => "enum member",
            Self::SymbolFile => "file",
            Self::SymbolFunction => "function",
            Self::SymbolInterface => "interface",
            Self::SymbolMethod => "method",
            Self::SymbolModule => "module",
            Self::SymbolNamespace => "namespace",
            Self::SymbolProperty => "property",
            Self::SymbolTypeAlias => "type alias",
            Self::SymbolTypeParameter => "type parameter",
            Self::SymbolVariable => "variable",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SymbolInner {
    kind: SymbolKind,
    name: FlowSmolStr,
    def_loc: ALoc,
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(Arc<SymbolInner>);

impl Symbol {
    fn new(kind: SymbolKind, name: FlowSmolStr, def_loc: ALoc) -> Self {
        Symbol(Arc::new(SymbolInner {
            kind,
            name,
            def_loc,
        }))
    }

    pub fn mk_class_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolClass, name, def_loc)
    }

    pub fn mk_component_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolComponent, name, def_loc)
    }

    pub fn mk_constant_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolConstant, name, def_loc)
    }

    pub fn mk_enum_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolEnum, name, def_loc)
    }

    pub fn mk_module_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolModule, name, def_loc)
    }

    pub fn mk_namespace_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolNamespace, name, def_loc)
    }

    pub fn mk_type_alias_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolTypeAlias, name, def_loc)
    }

    pub fn mk_type_parameter_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolTypeParameter, name, def_loc)
    }

    pub fn mk_variable_symbol(name: FlowSmolStr, def_loc: ALoc) -> Self {
        Self::new(SymbolKind::SymbolVariable, name, def_loc)
    }

    pub fn kind(&self) -> SymbolKind {
        self.0.kind
    }

    pub fn name(&self) -> &FlowSmolStr {
        &self.0.name
    }

    pub fn def_loc_of_symbol(&self) -> &ALoc {
        &self.0.def_loc
    }
}

pub fn dump_symbol(symbol: &Symbol) -> String {
    let loc_str = string_of_aloc(None, &symbol.0.def_loc);
    format!("{}: {} {}", loc_str, symbol.0.kind.as_str(), symbol.0.name)
}

pub fn dump_symbol_opt(symbol: Option<&Symbol>) -> String {
    match symbol {
        Some(s) => dump_symbol(s),
        None => "no symbol".to_string(),
    }
}
