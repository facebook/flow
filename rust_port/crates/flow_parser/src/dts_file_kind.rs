/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::ast::Program;
use crate::ast::statement::StatementInner;
use crate::file_key::DCTS_EXT;
use crate::file_key::DMTS_EXT;
use crate::file_key::DTS_EXT;
use crate::file_key::FileKey;
use crate::file_key::has_dts_ext;
use crate::loc::Loc;

#[derive(
    Copy,
    Clone,
    Debug,
    Dupe,
    Eq,
    Hash,
    Ord,
    PartialEq,
    PartialOrd,
    serde::Deserialize,
    serde::Serialize
)]
pub enum DtsFileKind {
    ExternalModule,
    GlobalLibdef,
    Invalid,
}

#[derive(Copy, Clone, Debug, Dupe, Eq, PartialEq)]
pub enum FileSemanticRole {
    SourceModule,
    GlobalLibdef,
}

impl FileSemanticRole {
    pub fn from_physical_key(file: &FileKey) -> Self {
        if file.is_lib_file() {
            Self::GlobalLibdef
        } else {
            Self::SourceModule
        }
    }

    pub fn from_dts_file_kind(file: &FileKey, kind: Option<DtsFileKind>) -> Self {
        if file.is_lib_file() || kind == Some(DtsFileKind::GlobalLibdef) {
            Self::GlobalLibdef
        } else {
            Self::SourceModule
        }
    }

    pub fn is_global_libdef(self) -> bool {
        self == Self::GlobalLibdef
    }
}

pub fn invalid_dts_file_kind(file: &FileKey) -> Option<DtsFileKind> {
    has_dts_ext(file.as_str()).then_some(DtsFileKind::Invalid)
}

pub fn dts_file_kind(file: &FileKey, ast: &Program<Loc, Loc>) -> Option<DtsFileKind> {
    let filename = file.as_str();
    if !has_dts_ext(filename) {
        None
    } else if filename.ends_with(DMTS_EXT) || filename.ends_with(DCTS_EXT) {
        Some(DtsFileKind::ExternalModule)
    } else {
        debug_assert!(filename.ends_with(DTS_EXT));
        Some(
            if ast
                .statements
                .iter()
                .any(|statement| is_external_module_statement(statement))
            {
                DtsFileKind::ExternalModule
            } else {
                DtsFileKind::GlobalLibdef
            },
        )
    }
}

fn is_external_module_statement(statement: &StatementInner<Loc, Loc>) -> bool {
    match statement {
        StatementInner::ImportDeclaration { .. }
        | StatementInner::ImportEqualsDeclaration { .. }
        | StatementInner::ExportNamedDeclaration { .. }
        | StatementInner::ExportDefaultDeclaration { .. }
        | StatementInner::ExportAssignment { .. }
        | StatementInner::NamespaceExportDeclaration { .. }
        | StatementInner::DeclareExportDeclaration { .. } => true,
        StatementInner::Block { .. }
        | StatementInner::Break { .. }
        | StatementInner::ClassDeclaration { .. }
        | StatementInner::ComponentDeclaration { .. }
        | StatementInner::Continue { .. }
        | StatementInner::Debugger { .. }
        | StatementInner::DeclareClass { .. }
        | StatementInner::DeclareComponent { .. }
        | StatementInner::DeclareEnum { .. }
        | StatementInner::DeclareFunction { .. }
        | StatementInner::DeclareInterface { .. }
        | StatementInner::DeclareModule { .. }
        | StatementInner::DeclareModuleExports { .. }
        | StatementInner::DeclareNamespace { .. }
        | StatementInner::DeclareTypeAlias { .. }
        | StatementInner::DeclareOpaqueType { .. }
        | StatementInner::DeclareVariable { .. }
        | StatementInner::DoWhile { .. }
        | StatementInner::Empty { .. }
        | StatementInner::EnumDeclaration { .. }
        | StatementInner::Expression { .. }
        | StatementInner::For { .. }
        | StatementInner::ForIn { .. }
        | StatementInner::ForOf { .. }
        | StatementInner::FunctionDeclaration { .. }
        | StatementInner::If { .. }
        | StatementInner::InterfaceDeclaration { .. }
        | StatementInner::Labeled { .. }
        | StatementInner::Match { .. }
        | StatementInner::RecordDeclaration { .. }
        | StatementInner::Return { .. }
        | StatementInner::Switch { .. }
        | StatementInner::Throw { .. }
        | StatementInner::Try { .. }
        | StatementInner::TypeAlias { .. }
        | StatementInner::OpaqueType { .. }
        | StatementInner::VariableDeclaration { .. }
        | StatementInner::While { .. }
        | StatementInner::With { .. } => false,
    }
}
