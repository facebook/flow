/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use flow_aloc::ALoc;
use flow_common::docblock::Docblock;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser::parse_error::ParseError;
use flow_parser_utils::file_sig::FileSig;
use flow_parsing::docblock_parser::DocblockError;
use flow_services_coverage::FileCoverage;
use flow_services_references::find_refs_types::SingleRef;
use flow_type_sig::packed_type_sig;
use flow_type_sig::signature_error::TolerableError;
use flow_typing_context::Context;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::properties;

#[derive(Clone)]
pub struct ParseArtifacts {
    pub docblock: Arc<Docblock>,
    pub docblock_errors: Vec<DocblockError>,
    pub ast: Arc<ast::Program<Loc, Loc>>,
    pub requires: Vec<FlowImportSpecifier>,
    pub file_sig: Arc<FileSig>,
    pub tolerable_errors: Vec<TolerableError<Loc>>,
    pub parse_errors: Vec<(Loc, ParseError)>,
}

#[derive(Clone)]
pub struct TypecheckArtifacts<'cx> {
    pub cx: Context<'cx>,
    pub typed_ast: ast::Program<ALoc, (ALoc, Type)>,
    pub obj_to_obj_map: BTreeMap<Loc, BTreeSet<properties::Id>>,
}

pub type FileArtifacts<'cx> = (ParseArtifacts, TypecheckArtifacts<'cx>);

#[derive(Clone, Debug)]
pub struct CheckedDependenciesCanceled;

#[derive(Clone)]
pub enum TypeContentsError {
    Errors(ErrorSet),
    CheckedDependenciesCanceled,
}

pub type AutocompleteArtifacts<'cx> = (
    String,
    ParseArtifacts,
    Context<'cx>,
    ast::Program<ALoc, ALoc>,
);

pub type Duration = f64;

pub type CheckTypeResult<'cx> = (
    Context<'cx>,
    packed_type_sig::Module<Loc>,
    FileSig,
    ast::Program<ALoc, (ALoc, Type)>,
);

pub type CheckErrorResult = (
    ErrorSet,
    ErrorSet,
    ErrorSuppressions,
    FileCoverage,
    Result<Vec<SingleRef>, String>,
    Duration,
);

pub type CheckResult<'cx> = (CheckTypeResult<'cx>, CheckErrorResult);

pub type MergeResult = (ErrorSuppressions, Duration);
