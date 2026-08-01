/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;

use dupe::Dupe;
use flow_analysis::bindings::Bindings;
use flow_analysis::bindings::Kind;
use flow_analysis::scope_builder;
use flow_analysis::scope_builder::WithBindings;
use flow_common_tarjan::DependencyComponent;
use flow_common_tarjan::dependency_components;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast_visitor::AstVisitor;

pub struct TypeParamAnalysis<Loc> {
    /// Direct valid sibling dependencies, with one map per source parameter.
    pub dependencies: Vec<BTreeMap<usize, Loc>>,
}

impl<Loc> TypeParamAnalysis<Loc> {
    /// Computes strongly connected components of the valid dependency graph.
    pub fn components(&self) -> Vec<DependencyComponent> {
        let direct_dependencies = self
            .dependencies
            .iter()
            .map(|dependencies| dependencies.keys().copied().collect())
            .collect::<Vec<_>>();
        dependency_components(&direct_dependencies)
    }
}

struct DependencyVisitor<'a, Loc> {
    sibling_indices: &'a BTreeMap<FlowSmolStr, usize>,
    source_index: usize,
    in_default: bool,
    shadowed: Vec<BTreeSet<FlowSmolStr>>,
    dependencies: BTreeMap<usize, Loc>,
}

/// Returns dependency-first bound initialization order. Cyclic parameters are
/// initialized first with error bounds and excluded from the bound graph.
pub fn type_param_order<Loc>(
    tparams: &ast::types::TypeParams<Loc, Loc>,
    cyclic: &BTreeSet<usize>,
) -> Vec<usize>
where
    Loc: Dupe + Eq + Hash + Default + Clone,
{
    let sibling_indices = tparams
        .params
        .iter()
        .enumerate()
        .map(|(index, tparam)| (tparam.name.name.dupe(), index))
        .collect();
    let mut dependencies = Vec::with_capacity(tparams.params.len());
    for (source_index, tparam) in tparams.params.iter().enumerate() {
        let mut visitor = DependencyVisitor {
            sibling_indices: &sibling_indices,
            source_index,
            in_default: false,
            shadowed: Vec::new(),
            dependencies: BTreeMap::new(),
        };
        let Ok(()) = visitor.type_annotation_hint(&tparam.bound);
        let dependencies_for_param = if cyclic.contains(&source_index) {
            BTreeSet::new()
        } else {
            visitor
                .dependencies
                .keys()
                .filter(|dependency| !cyclic.contains(dependency))
                .copied()
                .collect()
        };
        dependencies.push(dependencies_for_param);
    }
    let order = dependency_components(&dependencies);
    assert!(
        order.iter().all(|component| !component.cyclic),
        "excluding cyclic type parameters should make bound order acyclic"
    );
    cyclic
        .iter()
        .copied()
        .chain(
            order
                .iter()
                .rev()
                .flat_map(|component| component.members.iter().copied())
                .filter(|index| !cyclic.contains(index)),
        )
        .collect()
}

impl<Loc: Dupe> WithBindings<Loc, !> for DependencyVisitor<'_, Loc> {
    fn with_bindings<T>(
        &mut self,
        _lexical: bool,
        _loc: Loc,
        bindings: Bindings<Loc>,
        visit: impl FnOnce(&mut Self) -> Result<T, !>,
    ) -> Result<T, !> {
        let shadowed = bindings
            .to_assoc()
            .into_iter()
            .filter_map(|(name, (kind, _))| {
                matches!(kind, Kind::Type { .. } | Kind::TypeParam).then_some(name)
            })
            .collect();
        self.shadowed.push(shadowed);
        let result = visit(self);
        self.shadowed.pop();
        result
    }
}

impl<'ast, Loc> AstVisitor<'ast, Loc> for DependencyVisitor<'_, Loc>
where
    Loc: Dupe + Eq + Hash + Default + Clone,
{
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn type_identifier_reference(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
        if self
            .shadowed
            .iter()
            .rev()
            .any(|scope| scope.contains(&id.name))
        {
            return Ok(());
        }
        let Some(index) = self.sibling_indices.get(&id.name).copied() else {
            return Ok(());
        };
        if self.in_default && index >= self.source_index {
            return Ok(());
        } else {
            self.dependencies
                .entry(index)
                .or_insert_with(|| id.loc.dupe());
        }
        Ok(())
    }

    fn function_type(&mut self, ft: &'ast ast::types::Function<Loc, Loc>) -> Result<(), !> {
        scope_builder::function_type(self, true, ft, &|visitor, visit| visit(visitor))
    }

    fn component_type(
        &mut self,
        _loc: &'ast Loc,
        component: &'ast ast::types::Component<Loc, Loc>,
    ) -> Result<(), !> {
        scope_builder::component_type(self, true, component, &|visitor, visit| visit(visitor))
    }

    fn object_mapped_type_property(
        &mut self,
        mapped_type: &'ast ast::types::object::MappedType<Loc, Loc>,
    ) -> Result<(), !> {
        scope_builder::object_mapped_type_property(self, true, mapped_type, &|visitor, visit| {
            visit(visitor)
        })
    }

    fn conditional_type(
        &mut self,
        conditional: &'ast ast::types::Conditional<Loc, Loc>,
    ) -> Result<(), !> {
        scope_builder::conditional_type(
            self,
            conditional,
            |visitor, extends_type| visitor.type_(extends_type),
            |visitor, loc, tparams, in_tparam_scope| {
                scope_builder::scoped_infer_type_params(
                    visitor,
                    true,
                    loc,
                    tparams,
                    |visitor, id| visitor.binding_type_identifier(id),
                    in_tparam_scope,
                )
            },
        )
    }

    fn infer_type(&mut self, _infer: &'ast ast::types::Infer<Loc, Loc>) -> Result<(), !> {
        Ok(())
    }
}

/// Analyzes sibling references in one syntactic type-parameter list.
pub fn analyze_type_params<Loc>(
    tparams: &ast::types::TypeParams<Loc, Loc>,
) -> TypeParamAnalysis<Loc>
where
    Loc: Dupe + Eq + Hash + Default + Clone,
{
    let sibling_indices = tparams
        .params
        .iter()
        .enumerate()
        .map(|(index, tparam)| (tparam.name.name.dupe(), index))
        .collect();
    let mut dependencies = Vec::with_capacity(tparams.params.len());

    for (source_index, tparam) in tparams.params.iter().enumerate() {
        let mut visitor = DependencyVisitor {
            sibling_indices: &sibling_indices,
            source_index,
            in_default: false,
            shadowed: Vec::new(),
            dependencies: BTreeMap::new(),
        };
        let Ok(()) = visitor.type_annotation_hint(&tparam.bound);
        if let Some(default) = &tparam.default {
            visitor.in_default = true;
            let Ok(()) = visitor.type_(default);
        }
        dependencies.push(visitor.dependencies);
    }

    TypeParamAnalysis { dependencies }
}
