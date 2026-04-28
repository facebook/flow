/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_parser::ast;
use flow_parser::ast::types::TypeParam as AstTypeParam;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::UnresolvedParam;

use crate::component_sig_types;
use crate::func_class_sig_types;
use crate::match_pattern_ir;

pub type DeferredTypeGuardCheck<'cx, CX> =
    Option<Rc<dyn Fn(&CX) -> Result<(), flow_utils_concurrency::job_error::JobError> + 'cx>>;

struct Cache<'cx, CX: Clone> {
    annotations: ALocMap<ast::types::Annotation<ALoc, (ALoc, Type)>>,
    expressions: ALocMap<ast::expression::Expression<ALoc, (ALoc, Type)>>,
    statements: ALocMap<ast::statement::Statement<ALoc, (ALoc, Type)>>,
    jsx_body: ALocMap<(
        Vec<UnresolvedParam>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    )>,
    functions: ALocMap<(Type, ast::function::Function<ALoc, (ALoc, Type)>)>,
    function_sigs: ALocMap<(
        func_class_sig_types::func::Func<func_class_sig_types::StmtConfigTypes>,
        func_class_sig_types::func::Reconstruct,
        DeferredTypeGuardCheck<'cx, CX>,
    )>,
    aliases: ALocMap<(Type, ast::statement::TypeAlias<ALoc, (ALoc, Type)>)>,
    opaques: ALocMap<(Type, ast::statement::OpaqueType<ALoc, (ALoc, Type)>)>,
    interfaces: ALocMap<(Type, ast::statement::Interface<ALoc, (ALoc, Type)>)>,
    declared_classes: ALocMap<(Type, ast::statement::DeclareClass<ALoc, (ALoc, Type)>)>,
    declared_components: ALocMap<(Type, ast::statement::DeclareComponent<ALoc, (ALoc, Type)>)>,
    declared_namespaces: ALocMap<(Type, ast::statement::DeclareNamespace<ALoc, (ALoc, Type)>)>,
    class_sigs: ALocMap<(
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<
            dyn Fn(
                    &CX,
                    Type,
                ) -> Result<
                    ast::class::Class<ALoc, (ALoc, Type)>,
                    flow_utils_concurrency::job_error::JobError,
                > + 'cx,
        >,
    )>,
    record_sigs: ALocMap<(
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<
            dyn Fn(
                    &CX,
                    Type,
                ) -> Result<
                    ast::statement::RecordDeclaration<ALoc, (ALoc, Type)>,
                    flow_utils_concurrency::job_error::JobError,
                > + 'cx,
        >,
    )>,
    tparams: ALocMap<(AstTypeParam<ALoc, (ALoc, Type)>, TypeParam, Type)>,
    component_sigs: ALocMap<(
        component_sig_types::component_sig::ComponentSig,
        component_sig_types::component_sig::ReconstructComponent,
    )>,
    match_patterns: ALocMap<(ast::match_pattern::MatchPattern<ALoc, (ALoc, Type)>, bool)>,
    match_pattern_value_unions: ALocMap<match_pattern_ir::value_union::ValueUnion<'cx, CX>>,
    // Intermediate PatternUnion state for incremental building
    match_pattern_unions: ALocMap<(match_pattern_ir::pattern_union::PatternUnion, i32)>,
}

pub struct NodeCache<'cx, CX: Clone>(Rc<RefCell<Cache<'cx, CX>>>);

impl<'cx, CX: Clone> NodeCache<'cx, CX> {
    /// Clear all cached entries, releasing any closures that may hold
    /// strong references to Context (breaking Rc cycles).
    pub fn clear(&self) {
        let mut cache = self.0.borrow_mut();
        cache.annotations = ALocMap::new();
        cache.expressions = ALocMap::new();
        cache.statements = ALocMap::new();
        cache.jsx_body = ALocMap::new();
        cache.functions = ALocMap::new();
        cache.function_sigs = ALocMap::new();
        cache.aliases = ALocMap::new();
        cache.opaques = ALocMap::new();
        cache.interfaces = ALocMap::new();
        cache.declared_classes = ALocMap::new();
        cache.declared_components = ALocMap::new();
        cache.declared_namespaces = ALocMap::new();
        cache.class_sigs = ALocMap::new();
        cache.record_sigs = ALocMap::new();
        cache.tparams = ALocMap::new();
        cache.component_sigs = ALocMap::new();
        cache.match_patterns = ALocMap::new();
        cache.match_pattern_value_unions = ALocMap::new();
        cache.match_pattern_unions = ALocMap::new();
    }

    pub fn mk_empty() -> NodeCache<'cx, CX> {
        NodeCache(Rc::new(RefCell::new(Cache {
            annotations: ALocMap::new(),
            expressions: ALocMap::new(),
            statements: ALocMap::new(),
            jsx_body: ALocMap::new(),
            functions: ALocMap::new(),
            function_sigs: ALocMap::new(),
            aliases: ALocMap::new(),
            opaques: ALocMap::new(),
            interfaces: ALocMap::new(),
            declared_classes: ALocMap::new(),
            declared_components: ALocMap::new(),
            declared_namespaces: ALocMap::new(),
            class_sigs: ALocMap::new(),
            record_sigs: ALocMap::new(),
            tparams: ALocMap::new(),
            component_sigs: ALocMap::new(),
            match_patterns: ALocMap::new(),
            match_pattern_value_unions: ALocMap::new(),
            match_pattern_unions: ALocMap::new(),
        })))
    }

    pub fn set_annotation(&self, anno: ast::types::Annotation<ALoc, (ALoc, Type)>) {
        let loc = anno.loc.dupe();
        self.0.borrow_mut().annotations.insert(loc, anno);
    }

    pub fn set_expression(&self, exp: ast::expression::Expression<ALoc, (ALoc, Type)>) {
        let (loc, _) = exp.loc();
        let loc = loc.clone();
        self.0.borrow_mut().expressions.insert(loc, exp);
    }

    pub fn set_statement(&self, stmt: ast::statement::Statement<ALoc, (ALoc, Type)>) {
        let loc = stmt.loc().clone();
        self.0.borrow_mut().statements.insert(loc, stmt);
    }

    pub fn set_jsx_children(
        &self,
        children: (
            Vec<UnresolvedParam>,
            (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
        ),
    ) {
        let (_, (loc, _)) = &children;
        let loc = loc.clone();
        self.0.borrow_mut().jsx_body.insert(loc, children);
    }

    pub fn set_function(
        &self,
        loc: ALoc,
        func: (Type, ast::function::Function<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().functions.insert(loc, func);
    }

    pub fn set_function_sig(
        &self,
        loc: ALoc,
        func_sig: (
            func_class_sig_types::func::Func<func_class_sig_types::StmtConfigTypes>,
            func_class_sig_types::func::Reconstruct,
            DeferredTypeGuardCheck<'cx, CX>,
        ),
    ) {
        self.0.borrow_mut().function_sigs.insert(loc, func_sig);
    }

    pub fn set_alias(
        &self,
        loc: ALoc,
        alias: (Type, ast::statement::TypeAlias<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().aliases.insert(loc, alias);
    }

    pub fn set_opaque(
        &self,
        loc: ALoc,
        opaque: (Type, ast::statement::OpaqueType<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().opaques.insert(loc, opaque);
    }

    pub fn set_interface(
        &self,
        loc: ALoc,
        inter: (Type, ast::statement::Interface<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().interfaces.insert(loc, inter);
    }

    pub fn set_declared_component(
        &self,
        loc: ALoc,
        component: (Type, ast::statement::DeclareComponent<ALoc, (ALoc, Type)>),
    ) {
        self.0
            .borrow_mut()
            .declared_components
            .insert(loc, component);
    }

    pub fn set_declared_class(
        &self,
        loc: ALoc,
        class_: (Type, ast::statement::DeclareClass<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().declared_classes.insert(loc, class_);
    }

    pub fn set_declared_namespace(
        &self,
        loc: ALoc,
        ns: (Type, ast::statement::DeclareNamespace<ALoc, (ALoc, Type)>),
    ) {
        self.0.borrow_mut().declared_namespaces.insert(loc, ns);
    }

    pub fn set_class_sig(
        &self,
        loc: ALoc,
        class_: (
            Type,
            Type,
            func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
            Rc<
                dyn Fn(
                        &CX,
                        Type,
                    ) -> Result<
                        ast::class::Class<ALoc, (ALoc, Type)>,
                        flow_utils_concurrency::job_error::JobError,
                    > + 'cx,
            >,
        ),
    ) {
        self.0.borrow_mut().class_sigs.insert(loc, class_);
    }

    pub fn set_record_sig(
        &self,
        loc: ALoc,
        record: (
            Type,
            Type,
            func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
            Rc<
                dyn Fn(
                        &CX,
                        Type,
                    ) -> Result<
                        ast::statement::RecordDeclaration<ALoc, (ALoc, Type)>,
                        flow_utils_concurrency::job_error::JobError,
                    > + 'cx,
            >,
        ),
    ) {
        self.0.borrow_mut().record_sigs.insert(loc, record);
    }

    pub fn set_tparam(&self, param: (AstTypeParam<ALoc, (ALoc, Type)>, TypeParam, Type)) {
        let (ast_tparam, _, _) = &param;
        let loc = ast_tparam.loc.clone();
        self.0.borrow_mut().tparams.insert(loc, param);
    }

    pub fn set_component_sig(
        &self,
        loc: ALoc,
        c: (
            component_sig_types::component_sig::ComponentSig,
            component_sig_types::component_sig::ReconstructComponent,
        ),
    ) {
        self.0.borrow_mut().component_sigs.insert(loc, c);
    }

    pub fn set_match_pattern(
        &self,
        loc: ALoc,
        p: (ast::match_pattern::MatchPattern<ALoc, (ALoc, Type)>, bool),
    ) {
        self.0.borrow_mut().match_patterns.insert(loc, p);
    }

    pub fn set_match_pattern_value_union(
        &self,
        loc: ALoc,
        v: match_pattern_ir::value_union::ValueUnion<'cx, CX>,
    ) {
        self.0
            .borrow_mut()
            .match_pattern_value_unions
            .insert(loc, v);
    }

    pub fn set_match_pattern_union(
        &self,
        loc: ALoc,
        v: (match_pattern_ir::pattern_union::PatternUnion, i32),
    ) {
        self.0.borrow_mut().match_pattern_unions.insert(loc, v);
    }

    pub fn get_annotation(&self, loc: &ALoc) -> Option<ast::types::Annotation<ALoc, (ALoc, Type)>> {
        self.0.borrow().annotations.get(loc).cloned()
    }

    pub fn get_expression(
        &self,
        loc: &ALoc,
    ) -> Option<ast::expression::Expression<ALoc, (ALoc, Type)>> {
        self.0.borrow().expressions.get(loc).cloned()
    }

    pub fn get_statement(
        &self,
        loc: &ALoc,
    ) -> Option<ast::statement::Statement<ALoc, (ALoc, Type)>> {
        self.0.borrow().statements.get(loc).cloned()
    }

    pub fn get_jsx_children(
        &self,
        loc: &ALoc,
    ) -> Option<(
        Vec<UnresolvedParam>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    )> {
        self.0.borrow().jsx_body.get(loc).cloned()
    }

    pub fn get_function_sig(
        &self,
        loc: &ALoc,
    ) -> Option<(
        func_class_sig_types::func::Func<func_class_sig_types::StmtConfigTypes>,
        func_class_sig_types::func::Reconstruct,
        DeferredTypeGuardCheck<'cx, CX>,
    )> {
        self.0.borrow().function_sigs.get(loc).cloned()
    }

    pub fn get_function(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::function::Function<ALoc, (ALoc, Type)>)> {
        self.0.borrow().functions.get(loc).cloned()
    }

    pub fn get_alias(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::TypeAlias<ALoc, (ALoc, Type)>)> {
        self.0.borrow().aliases.get(loc).cloned()
    }

    pub fn get_opaque(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::OpaqueType<ALoc, (ALoc, Type)>)> {
        self.0.borrow().opaques.get(loc).cloned()
    }

    pub fn get_interface(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::Interface<ALoc, (ALoc, Type)>)> {
        self.0.borrow().interfaces.get(loc).cloned()
    }

    pub fn get_declared_class(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::DeclareClass<ALoc, (ALoc, Type)>)> {
        self.0.borrow().declared_classes.get(loc).cloned()
    }

    pub fn get_declared_component(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::DeclareComponent<ALoc, (ALoc, Type)>)> {
        self.0.borrow().declared_components.get(loc).cloned()
    }

    pub fn get_declared_namespace(
        &self,
        loc: &ALoc,
    ) -> Option<(Type, ast::statement::DeclareNamespace<ALoc, (ALoc, Type)>)> {
        self.0.borrow().declared_namespaces.get(loc).cloned()
    }

    pub fn get_class_sig(
        &self,
        loc: &ALoc,
    ) -> Option<(
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<
            dyn Fn(
                    &CX,
                    Type,
                ) -> Result<
                    ast::class::Class<ALoc, (ALoc, Type)>,
                    flow_utils_concurrency::job_error::JobError,
                > + 'cx,
        >,
    )> {
        self.0.borrow().class_sigs.get(loc).cloned()
    }

    pub fn get_record_sig(
        &self,
        loc: &ALoc,
    ) -> Option<(
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<
            dyn Fn(
                    &CX,
                    Type,
                ) -> Result<
                    ast::statement::RecordDeclaration<ALoc, (ALoc, Type)>,
                    flow_utils_concurrency::job_error::JobError,
                > + 'cx,
        >,
    )> {
        self.0.borrow().record_sigs.get(loc).cloned()
    }

    pub fn get_tparam(
        &self,
        loc: &ALoc,
    ) -> Option<(AstTypeParam<ALoc, (ALoc, Type)>, TypeParam, Type)> {
        self.0.borrow().tparams.get(loc).cloned()
    }

    pub fn get_component_sig(
        &self,
        loc: &ALoc,
    ) -> Option<(
        component_sig_types::component_sig::ComponentSig,
        component_sig_types::component_sig::ReconstructComponent,
    )> {
        self.0.borrow().component_sigs.get(loc).cloned()
    }

    pub fn get_match_pattern(
        &self,
        loc: &ALoc,
    ) -> Option<(ast::match_pattern::MatchPattern<ALoc, (ALoc, Type)>, bool)> {
        self.0.borrow().match_patterns.get(loc).cloned()
    }

    pub fn get_match_pattern_value_union(
        &self,
        loc: &ALoc,
    ) -> Option<match_pattern_ir::value_union::ValueUnion<'cx, CX>> {
        self.0.borrow().match_pattern_value_unions.get(loc).cloned()
    }

    pub fn get_match_pattern_union(
        &self,
        loc: &ALoc,
    ) -> Option<(match_pattern_ir::pattern_union::PatternUnion, i32)> {
        self.0.borrow().match_pattern_unions.get(loc).cloned()
    }
}
