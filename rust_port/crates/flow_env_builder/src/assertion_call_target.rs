/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_analysis::bindings::Kind;
use flow_analysis::scope_api::ScopeInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::member;
use flow_parser::ast::statement::ImportKind;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc_sig::LocSig;

use crate::find_providers::State;
use crate::provider_api::Info as ProviderInfo;

/// Classification of an assertion function, shared by the targeted callee
/// analysis (producer) and name resolution (consumer).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssertionKind {
    Bare,
    TypeGuard,
}

/// The assertion behavior of a proven assertion callee: which parameter is
/// asserted and whether it carries a type guard (`asserts x is T`) or is bare
/// (`asserts x`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssertionInfo {
    pub parameter_index: usize,
    pub kind: AssertionKind,
}

/// Which export an imported callee root refers to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportedName {
    Default,
    Named(FlowSmolStr),
    Namespace,
}

/// Import identity of a callee root, collected alongside the target so the
/// assertion analysis can resolve imported callees without the ordinary
/// definition graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportedCallee {
    pub source: FlowSmolStr,
    pub remote: ImportedName,
    /// True for value imports; type and typeof imports never classify.
    pub is_value: bool,
}

/// A call whose callee is rooted in a binding that could hold an assertion
/// function: an import, a named definition, an annotated provider, or a plain
/// lexical value binding (which is classified later, erroring when it carries
/// assertion behavior without an annotation).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalleeTarget<L> {
    pub call_loc: L,
    pub callee_loc: L,
    pub root_use_loc: L,
    pub root_binding_loc: L,
    pub root_name: FlowSmolStr,
    pub provider_locs: Vec<L>,
    pub property_path: Vec<FlowSmolStr>,
    pub import: Option<ImportedCallee>,
}

struct Collector<'a, L: LocSig> {
    scope_info: &'a ScopeInfo<L>,
    provider_info: &'a ProviderInfo<L>,
    imports: BTreeMap<L, ImportedCallee>,
    targets: Vec<CalleeTarget<L>>,
}

impl<'a, L: LocSig> Collector<'a, L> {
    fn collect_assertion_flow_calls(&mut self, expression: &Expression<L, L>) {
        match &**expression {
            ExpressionInner::Call { loc, inner } => {
                if let Some(target) = self.target_for_call(loc, &inner.callee) {
                    self.targets.push(target);
                }
            }
            ExpressionInner::Sequence { inner, .. } => {
                for expression in inner.expressions.iter() {
                    self.collect_assertion_flow_calls(expression);
                }
            }
            _ => {}
        }
    }

    fn callee_path(callee: &Expression<L, L>) -> Option<(&L, Vec<FlowSmolStr>)> {
        match &**callee {
            ExpressionInner::Identifier { loc, .. } => Some((loc, vec![])),
            ExpressionInner::Member { inner, .. } => {
                let member::Property::PropertyIdentifier(property) = &inner.property else {
                    return None;
                };
                let (root, mut path) = Self::callee_path(&inner.object)?;
                path.push(property.name.dupe());
                Some((root, path))
            }
            _ => None,
        }
    }

    fn target_for_call(&self, call_loc: &L, callee: &Expression<L, L>) -> Option<CalleeTarget<L>> {
        let (root_use_loc, property_path) = Self::callee_path(callee)?;
        let def = self.scope_info.def_of_use_opt(root_use_loc)?;
        let is_import = matches!(def.kind, Kind::Import { .. } | Kind::TsImport);
        // Functions and classes classify from their merged types, so they
        // are collected even when provider analysis does not attribute an
        // annotated provider to them.
        let is_named_definition = matches!(
            def.kind,
            Kind::Function | Kind::Class | Kind::DeclaredFunction | Kind::DeclaredClass
        );
        // Plain lexical value bindings: an assertion function stored here
        // without an annotation errors when used. Type-like and internal
        // machinery bindings can never hold one.
        let is_lexical_value = matches!(
            def.kind,
            Kind::Var
                | Kind::Let
                | Kind::Const
                | Kind::DeclaredVar
                | Kind::DeclaredLet
                | Kind::DeclaredConst
                | Kind::Parameter
                | Kind::CatchParameter
                | Kind::ComponentParameter
        );
        let provider_locs: Vec<_> = def
            .locs
            .iter()
            .filter_map(|def_loc| self.provider_info.providers_of_def(def_loc))
            .filter(|providers| {
                matches!(providers.state, State::AnnotatedVar { contextual: false })
            })
            .flat_map(|providers| providers.providers.iter())
            .map(|provider| provider.reason.loc().dupe())
            .filter(|provider_loc| self.provider_info.is_provider_of_annotated(provider_loc))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        if !is_import && !is_named_definition && !is_lexical_value && provider_locs.is_empty() {
            return None;
        }

        let root_binding_loc = def.locs.first().dupe();
        let import = is_import
            .then(|| self.imports.get(&root_binding_loc).cloned())
            .flatten();

        Some(CalleeTarget {
            call_loc: call_loc.dupe(),
            callee_loc: callee.loc().dupe(),
            root_use_loc: root_use_loc.dupe(),
            root_binding_loc,
            root_name: def.actual_name.dupe(),
            provider_locs,
            property_path,
            import,
        })
    }
}

/// Maps each import-bound local name to the export it refers to, keyed by the
/// local identifier location (which is also the scope binding location).
fn import_descriptors<L: LocSig>(program: &ast::Program<L, L>) -> BTreeMap<L, ImportedCallee> {
    struct Imports<L> {
        imports: BTreeMap<L, ImportedCallee>,
    }

    impl<'ast, L: LocSig> AstVisitor<'ast, L> for Imports<L> {
        fn normalize_loc(loc: &'ast L) -> &'ast L {
            loc
        }

        fn normalize_type(type_: &'ast L) -> &'ast L {
            type_
        }

        fn import_declaration(
            &mut self,
            _loc: &'ast L,
            decl: &'ast ast::statement::ImportDeclaration<L, L>,
        ) -> Result<(), !> {
            let source = decl.source.1.value.dupe();
            let mut record = |loc: L, remote: ImportedName, kind: ImportKind| {
                self.imports.insert(
                    loc,
                    ImportedCallee {
                        source: source.dupe(),
                        remote,
                        is_value: matches!(kind, ImportKind::ImportValue),
                    },
                );
            };
            if let Some(default) = decl.default.as_ref() {
                record(
                    default.identifier.loc.dupe(),
                    ImportedName::Default,
                    decl.import_kind,
                );
            }
            match decl.specifiers.as_ref() {
                Some(ast::statement::import_declaration::Specifier::ImportNamedSpecifiers(
                    specs,
                )) => {
                    for spec in specs {
                        let remote = if spec.remote.name.as_str() == "default" {
                            ImportedName::Default
                        } else {
                            ImportedName::Named(spec.remote.name.dupe())
                        };
                        let local = spec.local.as_ref().unwrap_or(&spec.remote);
                        let kind = spec.kind.unwrap_or(decl.import_kind);
                        record(local.loc.dupe(), remote, kind);
                    }
                }
                Some(ast::statement::import_declaration::Specifier::ImportNamespaceSpecifier(
                    (_, id),
                )) => {
                    record(id.loc.dupe(), ImportedName::Namespace, decl.import_kind);
                }
                None => {}
            }
            ast_visitor::import_declaration_default(self, _loc, decl)
        }
    }

    let mut imports = Imports {
        imports: BTreeMap::new(),
    };
    let Ok(()) = imports.program(program);
    imports.imports
}

impl<'ast, L: LocSig> AstVisitor<'ast, L> for Collector<'_, L> {
    fn normalize_loc(loc: &'ast L) -> &'ast L {
        loc
    }

    fn normalize_type(type_: &'ast L) -> &'ast L {
        type_
    }

    fn expression_statement(
        &mut self,
        loc: &'ast L,
        statement: &'ast ast::statement::Expression<L, L>,
    ) -> Result<(), !> {
        // The expression itself, or direct operands of a comma expression, must be a call.
        self.collect_assertion_flow_calls(&statement.expression);
        ast_visitor::expression_statement_default(self, loc, statement)
    }
}

/// Collects statically named callees that can be resolved from annotations alone.
pub fn collect<L: LocSig>(
    program: &ast::Program<L, L>,
    scope_info: &ScopeInfo<L>,
    provider_info: &ProviderInfo<L>,
) -> Vec<CalleeTarget<L>> {
    let mut collector = Collector {
        scope_info,
        provider_info,
        imports: import_descriptors(program),
        targets: vec![],
    };
    let Ok(()) = collector.program(program);
    collector.targets
}
