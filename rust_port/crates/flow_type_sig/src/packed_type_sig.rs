/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_common::flow_import_specifier::Userland;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::compact_table::Index;
use crate::compact_table::Table;
use crate::type_sig_pack::ModuleKind;
use crate::type_sig_pack::Packed;
use crate::type_sig_pack::PackedDef;
use crate::type_sig_pack::Pattern;
use crate::type_sig_pack::RemoteRef;

#[derive(Debug, Hash, serde::Serialize, serde::Deserialize)]
pub struct Module<Loc> {
    pub module_kind: ModuleKind<Index<Loc>>,
    pub module_refs: Table<Userland>,
    pub local_defs: Table<PackedDef<Index<Loc>>>,
    pub dirty_local_defs: Vec<usize>,
    pub remote_refs: Table<RemoteRef<Index<Loc>>>,
    pub pattern_defs: Table<Packed<Index<Loc>>>,
    pub dirty_pattern_defs: Vec<usize>,
    pub patterns: Table<Pattern<Index<Loc>>>,
}

#[derive(Debug, Hash, serde::Serialize, serde::Deserialize)]
pub struct ModuleDef<Loc> {
    pub loc: Loc,
    pub module_kind: ModuleKind<Loc>,
}

#[derive(Debug, Hash, serde::Serialize, serde::Deserialize)]
pub struct Builtins<Loc> {
    pub module_refs: Table<Userland>,
    pub local_defs: Table<PackedDef<Index<Loc>>>,
    pub remote_refs: Table<RemoteRef<Index<Loc>>>,
    pub pattern_defs: Table<Packed<Index<Loc>>>,
    pub patterns: Table<Pattern<Index<Loc>>>,
    pub global_values: BTreeMap<FlowSmolStr, Index<PackedDef<Index<Loc>>>>,
    pub global_types: BTreeMap<FlowSmolStr, Index<PackedDef<Index<Loc>>>>,
    pub global_modules: BTreeMap<FlowSmolStr, ModuleDef<Index<Loc>>>,
}
