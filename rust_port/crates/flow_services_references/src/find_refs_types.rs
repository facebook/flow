/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/references/types/findRefsTypes.ml`

use flow_parser::loc::Loc;
use flow_services_get_def::get_def_types::DefInfo;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefKind {
    PropertyDefinition,
    PropertyAccess,
    Local,
}

pub fn string_of_ref_kind(kind: RefKind) -> &'static str {
    match kind {
        RefKind::PropertyDefinition => "PropertyDefinition",
        RefKind::PropertyAccess => "PropertyAccess",
        RefKind::Local => "Local",
    }
}

pub type SingleRef = (RefKind, Loc);

pub type FindRefsFound = Vec<SingleRef>;

#[derive(Debug, Clone)]
pub enum FindRefsOk {
    FoundReferences(FindRefsFound),
    NoDefinition(Option<String>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    FindReferences,
    Rename,
}

#[derive(Debug, Clone)]
pub struct Request {
    pub def_info: DefInfo,
    pub kind: Kind,
}

pub fn empty_request() -> Request {
    Request {
        def_info: DefInfo::NoDefinition(None),
        kind: Kind::FindReferences,
    }
}
