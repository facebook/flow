/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_analysis::scope_builder;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::statement;
use flow_parser::ast::statement::import_declaration;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;

use crate::utils::codemod_ast_mapper::CodemodAstMapper;

pub mod remove_react_import_stats {
    #[derive(Debug, Clone, Default)]
    pub struct RemoveReactImportStats {
        pub removed: usize,
        pub still_useful: usize,
    }

    impl RemoveReactImportStats {
        pub fn empty() -> Self {
            Self {
                removed: 0,
                still_useful: 0,
            }
        }

        pub fn combine(x1: &Self, x2: &Self) -> Self {
            Self {
                removed: x1.removed + x2.removed,
                still_useful: x1.still_useful + x2.still_useful,
            }
        }

        pub fn add_removed(&self) -> Self {
            Self {
                removed: self.removed + 1,
                still_useful: self.still_useful,
            }
        }

        pub fn add_still_useful(&self) -> Self {
            Self {
                removed: self.removed,
                still_useful: self.still_useful + 1,
            }
        }

        pub fn serialize(&self) -> Vec<String> {
            vec![
                format!("removed: {}", self.removed),
                format!("still useful: {}", self.still_useful),
            ]
        }

        pub fn report(&self) -> Vec<String> {
            vec![
                string_of_row(2, "Removed instances", self.removed),
                string_of_row(2, "Still useful instances", self.still_useful),
            ]
        }
    }

    impl flow_services_code_action::insert_type_utils::BaseStats for RemoveReactImportStats {
        fn empty() -> Self {
            Self::empty()
        }
        fn combine(a: &Self, b: &Self) -> Self {
            Self::combine(a, b)
        }
        fn serialize(&self) -> Vec<String> {
            self.serialize()
        }
        fn report(&self) -> Vec<String> {
            self.report()
        }
    }

    const WIDTH: usize = 45;

    fn string_of_row(indent: usize, name: &str, i: usize) -> String {
        let len = name.len();
        let padding = WIDTH.saturating_sub(len + indent + 7);
        format!(
            "{:indent$}{name}:{:padding$}{i:6}",
            "",
            "",
            indent = indent,
            name = name,
            padding = padding,
            i = i,
        )
    }
}

use remove_react_import_stats::RemoveReactImportStats;

pub type Acc = flow_services_code_action::insert_type_utils::UntypedAcc<RemoveReactImportStats>;

pub fn react_import_def_loc_opt_of_stmt(stmt: &statement::Statement<Loc, Loc>) -> Option<Loc> {
    use statement::StatementInner;
    match &**stmt {
        StatementInner::ImportDeclaration { inner, .. }
            if inner.import_kind == statement::ImportKind::ImportValue
                && inner.source.1.value.as_str() == "react"
                && inner.specifiers.is_none()
                && matches!(
                    &inner.default,
                    Some(import_declaration::DefaultIdentifier {
                        identifier,
                        ..
                    }) if identifier.name.as_str() == "React"
                ) =>
        {
            match &inner.default {
                Some(import_declaration::DefaultIdentifier { identifier, .. }) => {
                    Some(identifier.loc.clone())
                }
                _ => None,
            }
        }
        StatementInner::ImportDeclaration { inner, .. }
            if inner.import_kind == statement::ImportKind::ImportValue
                && inner.source.1.value.as_str() == "react"
                && inner.default.is_none()
                && matches!(
                    &inner.specifiers,
                    Some(import_declaration::Specifier::ImportNamespaceSpecifier((
                        _,
                        id,
                    ))) if id.name.as_str() == "React"
                ) =>
        {
            match &inner.specifiers {
                Some(import_declaration::Specifier::ImportNamespaceSpecifier((_, id))) => {
                    Some(id.loc.clone())
                }
                _ => None,
            }
        }
        _ => None,
    }
}

pub struct HasUnaccountedReactValueUsageVisitor {
    in_typeof_type: bool,
    acc: bool,
}

impl HasUnaccountedReactValueUsageVisitor {
    pub fn new() -> Self {
        Self {
            in_typeof_type: false,
            acc: false,
        }
    }

    pub fn eval(&mut self, prog: &ast::Program<Loc, Loc>) -> bool {
        let Ok(()) = self.program(prog);
        self.acc
    }
}

impl<'ast> AstVisitor<'ast, Loc> for HasUnaccountedReactValueUsageVisitor {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn typeof_type(&mut self, typeof_: &'ast ast::types::Typeof<Loc, Loc>) -> Result<(), !> {
        let saved_in_typeof_type = self.in_typeof_type;
        self.in_typeof_type = true;
        let result = ast_visitor::typeof_type_default(self, typeof_);
        self.in_typeof_type = saved_in_typeof_type;
        result
    }

    fn component_declaration(
        &mut self,
        loc: &'ast Loc,
        component: &'ast ast::statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let params = &component.params;
        if params.params.iter().any(|param| {
            let name: &FlowSmolStr = match &param.name {
                statement::component_params::ParamName::Identifier(id) => &id.name,
                statement::component_params::ParamName::StringLiteral((_, sl)) => &sl.value,
            };
            name.as_str() == "ref"
        }) {
            flow_hh_logger::info!(
                "Skipping component with ref {}",
                flow_common::reason::string_of_loc(None, loc)
            );
            self.acc = true;
        }
        ast_visitor::component_declaration_default(self, loc, component)
    }

    fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
        if self.in_typeof_type && id.name.as_str() == "React" {
            flow_hh_logger::info!(
                "Skipping React in typeof {}",
                flow_common::reason::string_of_loc(None, &id.loc)
            );
            self.acc = true;
        }
        ast_visitor::identifier_default(self, id)
    }
}

pub struct RemoveReactImportMapper {
    pub inner: CodemodAstMapper<Acc>,
    file: FileKey,
}

impl RemoveReactImportMapper {
    pub fn new(file: FileKey) -> Self {
        Self {
            inner: CodemodAstMapper::new(FlowSmolStr::default(), Acc::empty()),
            file,
        }
    }

    pub fn program(&mut self, prog: ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        let file = self.file.clone();
        let react_import_def_loc = prog.statements.iter().fold(None, |acc, stmt| {
            if acc.is_some() {
                acc
            } else {
                react_import_def_loc_opt_of_stmt(stmt)
            }
        });
        let react_import_def_loc = match react_import_def_loc {
            None => return prog,
            Some(loc) => loc,
        };
        let scope_info = scope_builder::program(false, false, &prog);
        let unused = {
            let def = scope_info
                .def_of_use_opt(&react_import_def_loc)
                .expect("def_of_use: react_import_def_loc not found in scope_info")
                .clone();
            let uses = scope_info.uses_of_def(&def, true);
            if uses.is_empty() {
                let mut visitor = HasUnaccountedReactValueUsageVisitor::new();
                !visitor.eval(&prog)
            } else {
                let uses_str: Vec<String> = uses
                    .iter()
                    .map(|loc| flow_common::reason::string_of_loc(None, loc))
                    .collect();
                flow_hh_logger::info!("Skipping due to value uses {}", uses_str.join(", "));
                false
            }
        };
        let extra = if unused {
            self.inner.acc.stats.add_removed()
        } else {
            self.inner.acc.stats.add_still_useful()
        };
        self.inner.acc.update_stats(extra);
        if unused {
            self.inner.acc.changed_set.insert(file);
            let filtered_statements: Vec<statement::Statement<Loc, Loc>> = prog
                .statements
                .iter()
                .filter(|stmt| react_import_def_loc_opt_of_stmt(stmt).is_none())
                .cloned()
                .collect();
            ast::Program {
                loc: prog.loc,
                statements: filtered_statements.into(),
                interpreter: prog.interpreter,
                comments: prog.comments,
                all_comments: prog.all_comments,
            }
        } else {
            prog
        }
    }
}
