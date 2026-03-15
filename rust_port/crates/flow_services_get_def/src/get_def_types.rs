/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::loc::Loc;
use vec1::Vec1;

#[derive(Debug, Clone)]
pub enum SinglePropertyDefInfo {
    ClassProperty(Loc),
    ObjectProperty(Loc),
}

/// If there are multiple relevant definition locations (e.g. the request was issued on an object
/// literal which is associated with multiple types) then there will be multiple locations in no
/// particular order.
#[derive(Debug, Clone)]
pub enum PropertyDefInfo {
    OrdinaryProperty {
        props_info: Vec1<SinglePropertyDefInfo>,
        name: FlowSmolStr,
    },
    PrivateNameProperty {
        def_loc: Loc,
        references: Vec<Loc>,
        name: FlowSmolStr,
    },
}

#[derive(Debug, Clone)]
pub enum DefInfo {
    VariableDefinition(Vec<Loc>, Option<FlowSmolStr>),
    PropertyDefinition(PropertyDefInfo),
    NoDefinition(Option<String>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Purpose {
    GoToDefinition,
    JSDoc,
    FindReferences,
}
