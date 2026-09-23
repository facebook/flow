/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_analysis::bindings::Kind;
use flow_analysis::scope_api::ScopeInfo;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::member;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc_sig::LocSig;

use crate::find_providers::State;
use crate::provider_api::Info as ProviderInfo;

/// A call whose callee is rooted in an imported or explicitly annotated binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalleeTarget<L> {
    pub call_loc: L,
    pub callee_loc: L,
    pub root_use_loc: L,
    pub root_binding_loc: L,
    pub provider_locs: Vec<L>,
    pub property_path: Vec<FlowSmolStr>,
}

struct Collector<'a, L: LocSig> {
    scope_info: &'a ScopeInfo<L>,
    provider_info: &'a ProviderInfo<L>,
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
        if !is_import && provider_locs.is_empty() {
            return None;
        }

        Some(CalleeTarget {
            call_loc: call_loc.dupe(),
            callee_loc: callee.loc().dupe(),
            root_use_loc: root_use_loc.dupe(),
            root_binding_loc: def.locs.first().dupe(),
            provider_locs,
            property_path,
        })
    }
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
        targets: vec![],
    };
    let Ok(()) = collector.program(program);
    collector.targets
}
