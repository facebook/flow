/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_statement::statement;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;

pub fn mk_bound_t<'a>(cx: &Context<'a>, tparam: &TypeParam) -> Type {
    flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), tparam)
}

#[derive(Clone)]
pub enum EnclosingNode<M: Dupe, T: Dupe> {
    EnclosingProgram(ast::Program<M, T>),
    EnclosingStatement(ast::statement::Statement<M, T>),
    EnclosingExpression(ast::expression::Expression<M, T>),
}

pub fn infer_node<'a>(
    cx: &Context<'a>,
    node: EnclosingNode<ALoc, ALoc>,
) -> Result<EnclosingNode<ALoc, (ALoc, Type)>, flow_typing_utils::abnormal::CheckExprError> {
    match node {
        EnclosingNode::EnclosingProgram(prog) => {
            let ast::Program {
                loc: prog_aloc,
                statements,
                interpreter,
                comments,
                all_comments,
            } = prog;
            let statements = statement::statement_list(cx, &statements)?;
            Ok(EnclosingNode::EnclosingProgram(ast::Program {
                loc: prog_aloc,
                statements: statements.into(),
                interpreter,
                comments,
                all_comments,
            }))
        }
        EnclosingNode::EnclosingStatement(stmt) => Ok(EnclosingNode::EnclosingStatement(
            statement::statement(cx, &stmt)?,
        )),
        EnclosingNode::EnclosingExpression(expr) => Ok(EnclosingNode::EnclosingExpression(
            statement::expression(None, None, None, cx, &expr)?,
        )),
    }
}

struct FindTypeAnnotVisitor {
    target_loc: ALoc,
}

impl flow_parser::polymorphic_ast_mapper::LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), Type>
    for FindTypeAnnotVisitor
{
    fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, Type> {
        Ok(loc.dupe())
    }

    fn on_type_annot(&mut self, annot: &(ALoc, Type)) -> Result<(ALoc, Type), Type> {
        let (loc, t) = annot;
        if *loc == self.target_loc {
            Err(t.dupe())
        } else {
            Ok(annot.dupe())
        }
    }
}

pub fn find_type_annot_in_node(
    loc: ALoc,
    node: &EnclosingNode<ALoc, (ALoc, Type)>,
) -> Option<Type> {
    let mut visitor = FindTypeAnnotVisitor { target_loc: loc };
    use flow_parser::polymorphic_ast_mapper;
    let result = match node {
        EnclosingNode::EnclosingProgram(prog) => {
            polymorphic_ast_mapper::program(&mut visitor, prog).map(|_| ())
        }
        EnclosingNode::EnclosingStatement(stmt) => {
            polymorphic_ast_mapper::statement(&mut visitor, stmt).map(|_| ())
        }
        EnclosingNode::EnclosingExpression(expr) => {
            polymorphic_ast_mapper::expression(&mut visitor, expr).map(|_| ())
        }
    };
    result.err()
}

/// Find exact location match
pub mod exact_match_query {
    use flow_parser::ast_visitor;
    use flow_parser::ast_visitor::AstVisitor;
    use flow_parser::polymorphic_ast_mapper;
    use flow_parser::polymorphic_ast_mapper::LocMapper;

    use super::*;

    enum FoundFilteredOut {
        Found(Type),
    }

    struct FilteredOutSearcher {
        target_loc: ALoc,
    }

    impl<'ast> AstVisitor<'ast, ALoc, (ALoc, Type), (), FoundFilteredOut> for FilteredOutSearcher {
        fn normalize_loc(_loc: &'ast ALoc) {}

        fn normalize_type(_type_: &'ast (ALoc, Type)) {}

        fn optional_call(
            &mut self,
            _loc: &'ast (ALoc, Type),
            expr: &'ast ast::expression::OptionalCall<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundFilteredOut> {
            let (filtered_out_loc, filtered_out_t) = &expr.filtered_out;
            if *filtered_out_loc == self.target_loc {
                return Err(FoundFilteredOut::Found(filtered_out_t.dupe()));
            }
            ast_visitor::optional_call_default(self, _loc, expr)
        }

        fn optional_member(
            &mut self,
            _loc: &'ast (ALoc, Type),
            expr: &'ast ast::expression::OptionalMember<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundFilteredOut> {
            let (filtered_out_loc, filtered_out_t) = &expr.filtered_out;
            if *filtered_out_loc == self.target_loc {
                return Err(FoundFilteredOut::Found(filtered_out_t.dupe()));
            }
            ast_visitor::optional_member_default(self, _loc, expr)
        }
    }

    struct ExactMatchSearcher {
        target_loc: ALoc,
    }

    impl LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), Type> for ExactMatchSearcher {
        fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, Type> {
            Ok(loc.dupe())
        }

        fn on_type_annot(&mut self, annot: &(ALoc, Type)) -> Result<(ALoc, Type), Type> {
            let (loc, t) = annot;
            if *loc == self.target_loc {
                Err(t.dupe())
            } else {
                Ok(annot.dupe())
            }
        }
    }

    pub fn find(typed_ast: &ast::Program<ALoc, (ALoc, Type)>, aloc: ALoc) -> Option<Type> {
        let mut filtered_out_searcher = FilteredOutSearcher {
            target_loc: aloc.dupe(),
        };
        if let Err(FoundFilteredOut::Found(t)) = filtered_out_searcher.program(typed_ast) {
            return Some(t);
        }

        let mut searcher = ExactMatchSearcher { target_loc: aloc };
        polymorphic_ast_mapper::program(&mut searcher, typed_ast).err()
    }
}

pub fn find_exact_match_annotation(
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    aloc: ALoc,
) -> Option<Type> {
    exact_match_query::find(typed_ast, aloc)
}

/// Find identifier under location
pub mod type_at_pos {
    use std::ops::Deref;

    use dupe::Dupe;
    use flow_aloc::ALoc;
    use flow_common::polarity::Polarity;
    use flow_common::reason;
    use flow_common::reason::Name;
    use flow_common::reason::VirtualReasonDesc;
    use flow_common::subst_name::SubstName;
    use flow_common_ty::ty::Binder;
    use flow_common_ty::ty::BinderKind;
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;
    use flow_parser::ast;
    use flow_parser::ast_visitor;
    use flow_parser::ast_visitor::AstVisitor;
    use flow_parser::ast_visitor::TypeParamsContext;
    use flow_parser::loc::Loc;
    use flow_typing_context::Context;
    use flow_typing_type::type_::LazyHintT;
    use flow_typing_type::type_::Type;
    use flow_typing_type::type_::TypeParam;
    use flow_typing_type::type_::TypeParamInner;
    use flow_typing_type::type_::mixed_t;
    use flow_typing_type::type_util;
    use flow_typing_utils::type_env;
    use flow_typing_utils::type_hint;
    use flow_typing_utils::typed_ast_utils;

    use super::mk_bound_t;

    /// How the declaration to frame a hover as is determined. A binding site is
    /// resolved by the finder, which is walking the declaration that introduced the
    /// name; a reference carries no such context, so the finder only records what
    /// kind of reference it is and leaves the lookup to the caller.
    pub enum Framing {
        /// The target is a binding's own name.
        Binder(Binder),
        /// The target is a bare identifier; resolve it to the binding it references.
        IdentifierRef,
        /// The target is the `p` of a `o.p` access; resolve `p` in the type of `o`.
        MemberRef {
            name: FlowSmolStr,
            object_type: Type,
        },
    }

    pub enum TypeAtPosResult {
        TypeResult {
            loc: Loc,
            is_type_identifier_reference: bool,
            type_: Type,
            framing: Option<Framing>,
        },
        HardcodedModuleResult(Loc, FlowSmolStr),
        NoResult,
    }

    enum FoundResult {
        FoundType {
            loc: ALoc,
            is_type_identifier_reference: bool,
            type_: Type,
            framing: Option<Framing>,
        },
        FoundHardcodedModule(ALoc, FlowSmolStr),
        Canceled(flow_utils_concurrency::job_error::JobError),
    }

    impl From<flow_utils_concurrency::job_error::JobError> for FoundResult {
        fn from(e: flow_utils_concurrency::job_error::JobError) -> Self {
            FoundResult::Canceled(e)
        }
    }

    // Kinds of nodes that "type-at-pos" is interested in:
    // - identifiers              (handled in t_identifier)
    // - type parameters          (handled in type_param_identifier)
    // - literal object keys      (handled in object_key)
    // - `this`, `super`          (handled in expression)
    // - private property names   (handled in expression)
    struct TypeAtPosSearcher<'a, 'cx> {
        cx: &'a Context<'cx>,
        target_loc: Loc,
        rev_bound_tparams: Vec<TypeParam>,
        /// Name of the class, declared class or record being visited, so that a
        /// member binder can qualify itself as `A.m`.
        enclosing_nominal: Option<FlowSmolStr>,
    }

    impl<'a, 'cx> TypeAtPosSearcher<'a, 'cx> {
        fn covers_target(&self, loc: &ALoc) -> bool {
            reason::in_range(&self.target_loc, loc.to_loc_exn())
        }

        fn covers_target_loc(&self, loc: &Loc) -> bool {
            reason::in_range(&self.target_loc, loc)
        }

        fn find_loc(
            &self,
            loc: &ALoc,
            t: &Type,
            is_type_identifier: bool,
            framing: Option<Framing>,
        ) -> Result<(), FoundResult> {
            Err(FoundResult::FoundType {
                loc: loc.dupe(),
                is_type_identifier_reference: is_type_identifier,
                type_: t.dupe(),
                framing,
            })
        }

        /// Like `find_loc`, but records that the target is the declaration site of
        /// `binder` rather than a reference to it.
        fn find_binder(
            &self,
            loc: &ALoc,
            t: &Type,
            kind: BinderKind,
            name: &FlowSmolStr,
        ) -> Result<(), FoundResult> {
            let owner = match kind {
                BinderKind::Method
                | BinderKind::Getter
                | BinderKind::Setter
                | BinderKind::Property => self.enclosing_nominal.dupe(),
                _ => None,
            };
            Err(FoundResult::FoundType {
                loc: loc.dupe(),
                is_type_identifier_reference: false,
                type_: t.dupe(),
                framing: Some(Framing::Binder(Binder {
                    kind,
                    name: name.dupe(),
                    owner,
                })),
            })
        }

        /// The binder for a `Pattern`, when it binds a single name. Destructuring
        /// patterns bind through nested `pattern_identifier`s that this does not
        /// reach; those reach `identifier` instead and are framed by resolving the
        /// name to its binding.
        fn simple_pattern_binder<'ast>(
            pattern: &'ast ast::pattern::Pattern<ALoc, (ALoc, Type)>,
        ) -> Option<&'ast ast::Identifier<ALoc, (ALoc, Type)>> {
            match pattern {
                ast::pattern::Pattern::Identifier { inner, .. } => Some(&inner.name),
                _ => None,
            }
        }

        fn binder_kind_of_variable_kind(kind: ast::VariableKind) -> BinderKind {
            match kind {
                ast::VariableKind::Var => BinderKind::Var,
                ast::VariableKind::Let => BinderKind::Let,
                ast::VariableKind::Const => BinderKind::Const,
            }
        }

        /// The name a declarator binds, shared by the `declare` form and the
        /// ordinary one.
        fn variable_binder(
            &self,
            kind: ast::VariableKind,
            declarator: &ast::statement::variable::Declarator<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            if let Some(id) = Self::simple_pattern_binder(&declarator.id)
                && let (loc, t) = &id.loc
                && self.covers_target(loc)
            {
                return self.find_binder(
                    loc,
                    t,
                    Self::binder_kind_of_variable_kind(kind),
                    &id.name,
                );
            }
            Ok(())
        }

        /// The name a function declaration binds. A function expression has no
        /// binding of its own, so its name is left to `identifier`.
        fn function_binder(
            &self,
            id: Option<&ast::Identifier<ALoc, (ALoc, Type)>>,
        ) -> Result<(), FoundResult> {
            let Some(id) = id else { return Ok(()) };
            let (loc, t) = &id.loc;
            if self.covers_target(loc) {
                return self.find_binder(loc, t, BinderKind::Function, &id.name);
            }
            Ok(())
        }

        /// Visits `f` with `enclosing_nominal` set, restoring it afterwards.
        fn in_nominal<R>(
            &mut self,
            name: Option<FlowSmolStr>,
            f: impl FnOnce(&mut Self) -> R,
        ) -> R {
            let outer = std::mem::replace(&mut self.enclosing_nominal, name);
            let res = f(self);
            self.enclosing_nominal = outer;
            res
        }

        fn make_typeparam(&self, tparam: &ast::types::TypeParam<ALoc, (ALoc, Type)>) -> TypeParam {
            let (name_loc, _) = &tparam.name.loc;
            let name = &tparam.name.name;
            let reason =
                reason::mk_annot_reason(VirtualReasonDesc::RType(name.dupe()), name_loc.dupe());
            let bound = match &tparam.bound {
                ast::types::AnnotationOrHint::Missing((_, t)) => t.dupe(),
                ast::types::AnnotationOrHint::Available(annotation) => {
                    let (_, t) = annotation.annotation.loc();
                    t.dupe()
                }
            };
            let default = tparam.default.as_ref().map(|ty| {
                let (_, t) = ty.loc();
                t.dupe()
            });
            TypeParam::new(TypeParamInner {
                reason,
                name: SubstName::name(name.dupe()),
                bound,
                polarity: typed_ast_utils::polarity(tparam.variance.as_ref()),
                default,
                is_this: false,
                is_const: tparam.const_.is_some(),
            })
        }

        fn make_class_this(&self, cls: &ast::class::Class<ALoc, (ALoc, Type)>) -> TypeParam {
            let body_loc = &cls.body.loc;
            let bound = match &cls.id {
                Some(id) => {
                    let (_, t) = &id.loc;
                    t.dupe()
                }
                None => {
                    let reason = reason::mk_reason(
                        VirtualReasonDesc::RCustom(FlowSmolStr::new("<<anonymous class>>")),
                        body_loc.dupe(),
                    );
                    mixed_t::make(reason)
                }
            };
            TypeParam::new(TypeParamInner {
                name: SubstName::name(FlowSmolStr::new("this")),
                reason: type_util::reason_of_t(&bound)
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RThisType),
                bound,
                polarity: Polarity::Positive,
                default: None,
                is_this: true,
                is_const: false,
            })
        }

        fn make_declare_class_this(
            &self,
            decl: &ast::statement::DeclareClass<ALoc, (ALoc, Type)>,
        ) -> TypeParam {
            let (_, bound) = &decl.id.loc;
            TypeParam::new(TypeParamInner {
                name: SubstName::name(FlowSmolStr::new("this")),
                reason: type_util::reason_of_t(bound)
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RThisType),
                bound: bound.dupe(),
                polarity: Polarity::Positive,
                default: None,
                is_this: true,
                is_const: false,
            })
        }

        fn make_record_this(
            &self,
            record: &ast::statement::RecordDeclaration<ALoc, (ALoc, Type)>,
        ) -> TypeParam {
            let (_, t) = &record.id.loc;
            TypeParam::new(TypeParamInner {
                name: SubstName::name(FlowSmolStr::new("this")),
                reason: type_util::reason_of_t(t)
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RThisType),
                bound: t.dupe(),
                polarity: Polarity::Positive,
                default: None,
                is_this: true,
                is_const: false,
            })
        }
    }

    impl<'ast, 'a, 'cx> AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, FoundResult>
        for TypeAtPosSearcher<'a, 'cx>
    {
        fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
            loc
        }

        fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
            &type_.0
        }

        fn identifier(
            &mut self,
            id: &'ast ast::Identifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, t) = &id.loc;
            if self.covers_target(loc) {
                self.find_loc(loc, t, false, Some(Framing::IdentifierRef))
            } else {
                ast_visitor::identifier_default(self, id)
            }
        }

        /// Intercepts `o.p` before the walk reaches `p`'s own identifier, which
        /// carries no link back to `o`. Optional chains route through here too:
        /// `optional_member_default` delegates to this hook.
        fn member(
            &mut self,
            loc: &'ast (ALoc, Type),
            expr: &'ast ast::expression::Member<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::expression::member::Property;
            if let Property::PropertyIdentifier(id) = &expr.property
                && let (id_loc, t) = &id.loc
                && self.covers_target(id_loc)
            {
                let (_, object_type) = expr.object.loc();
                return self.find_loc(
                    id_loc,
                    t,
                    false,
                    Some(Framing::MemberRef {
                        name: id.name.dupe(),
                        object_type: object_type.dupe(),
                    }),
                );
            }
            ast_visitor::member_default(self, loc, expr)
        }

        fn type_identifier_reference(
            &mut self,
            id: &'ast ast::Identifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, t) = &id.loc;
            if self.covers_target(loc) {
                self.find_loc(loc, t, true, None)
            } else {
                ast_visitor::identifier_default(self, id)
            }
        }

        fn jsx_identifier(
            &mut self,
            ident: &'ast ast::jsx::Identifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, t) = &ident.loc;
            if self.covers_target(loc) {
                self.find_loc(loc, t, false, None)
            } else {
                ast_visitor::jsx_identifier_default(self, ident)
            }
        }

        fn type_param(
            &mut self,
            kind: &TypeParamsContext,
            tparam: &'ast ast::types::TypeParam<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, _) = &tparam.name.loc;
            if self.covers_target(loc) {
                let tp = self.make_typeparam(tparam);
                self.rev_bound_tparams.push(tp.dupe());
                let t = mk_bound_t(self.cx, &tp);
                self.find_loc(loc, &t, false, None)
            } else {
                let res = ast_visitor::type_param_default(self, kind, tparam);
                let tp = self.make_typeparam(tparam);
                self.rev_bound_tparams.push(tp);
                res
            }
        }

        fn type_params(
            &mut self,
            kind: &TypeParamsContext,
            tparams: &'ast ast::types::TypeParams<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let originally_bound_tparams = self.rev_bound_tparams.clone();
            let res = ast_visitor::type_params_default(self, kind, tparams);
            self.rev_bound_tparams = originally_bound_tparams;
            res
        }

        fn class_(
            &mut self,
            loc: &'ast ALoc,
            cls: &'ast ast::class::Class<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let this_tparam = self.make_class_this(cls);
            let originally_bound_tparams = self.rev_bound_tparams.clone();
            self.rev_bound_tparams.push(this_tparam);
            let name = cls.id.as_ref().map(|id| id.name.dupe());
            let res = self.in_nominal(name, |this| ast_visitor::class_default(this, loc, cls));
            self.rev_bound_tparams = originally_bound_tparams;
            res
        }

        fn declare_class(
            &mut self,
            loc: &'ast ALoc,
            decl: &'ast ast::statement::DeclareClass<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let this_tparam = self.make_declare_class_this(decl);
            let originally_bound_tparams = self.rev_bound_tparams.clone();
            self.rev_bound_tparams.push(this_tparam);
            let name = Some(decl.id.name.dupe());
            let res = self.in_nominal(name, |this| {
                ast_visitor::declare_class_default(this, loc, decl)
            });
            self.rev_bound_tparams = originally_bound_tparams;
            res
        }

        fn record_declaration(
            &mut self,
            loc: &'ast ALoc,
            record: &'ast ast::statement::RecordDeclaration<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let this_tparam = self.make_record_this(record);
            let originally_bound_tparams = self.rev_bound_tparams.clone();
            self.rev_bound_tparams.push(this_tparam);
            let name = Some(record.id.name.dupe());
            let res = self.in_nominal(name, |this| {
                ast_visitor::record_declaration_default(this, loc, record)
            });
            self.rev_bound_tparams = originally_bound_tparams;
            res
        }

        fn object_key(
            &mut self,
            key: &'ast ast::expression::object::Key<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::expression::object::Key;
            match key {
                Key::StringLiteral(((loc, t), _))
                | Key::NumberLiteral(((loc, t), _))
                | Key::BigIntLiteral(((loc, t), _))
                    if self.covers_target(loc) =>
                {
                    self.find_loc(loc, t, false, None)
                }
                _ => ast_visitor::object_key_default(self, key),
            }
        }

        fn expression(
            &mut self,
            expr: &'ast ast::expression::Expression<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::expression::ExpressionInner;
            use ast::expression::member;
            match expr.deref() {
                ExpressionInner::This { loc: (loc, t), .. }
                | ExpressionInner::Super { loc: (loc, t), .. }
                    if self.covers_target(loc) =>
                {
                    self.find_loc(loc, t, false, None)
                }
                ExpressionInner::Member { loc: (_, t), inner }
                    if let member::Property::PropertyPrivateName(pn) = &inner.property
                        && self.covers_target(&pn.loc) =>
                {
                    self.find_loc(&pn.loc, t, false, None)
                }
                ExpressionInner::OptionalMember { loc: (_, t), inner }
                    if let member::Property::PropertyPrivateName(pn) = &inner.member.property
                        && self.covers_target(&pn.loc) =>
                {
                    self.find_loc(&pn.loc, t, false, None)
                }
                _ => ast_visitor::expression_default(self, expr),
            }
        }

        //     Class information
        //     v
        // new C(e1, e2);
        // ^^^^
        // Constructor information
        fn new(
            &mut self,
            loc: &'ast (ALoc, Type),
            expr: &'ast ast::expression::New<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (expr_loc, _) = loc;
            let (callee_loc, _) = expr.callee.loc();
            let expr_start_loc = expr_loc.to_loc_exn().first_char();
            let callee_start_loc = callee_loc.to_loc_exn().char_before().char_before();
            let new_loc = Loc::between(&expr_start_loc, &callee_start_loc);
            if self.covers_target_loc(&new_loc) {
                match self.cx.get_ctor_callee(expr_loc) {
                    Some(t) => self.find_loc(callee_loc, &t, false, None),
                    None => ast_visitor::new_default(self, loc, expr),
                }
            } else {
                ast_visitor::new_default(self, loc, expr)
            }
        }

        fn jsx_attribute_name_identifier(
            &mut self,
            ident: &'ast ast::jsx::Identifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, _) = &ident.loc;
            if self.covers_target(loc) {
                let reason = reason::mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(ident.name.dupe()))),
                    loc.dupe(),
                );
                let LazyHintT(_, lazy_hint) = type_env::get_hint(self.cx, loc.dupe());
                let hint_result = lazy_hint(self.cx, false, None, reason)?;
                // Split with_hint to avoid double &mut self borrow in closures
                let hint_t = type_hint::with_hint(Some, || None, hint_result);
                match hint_t {
                    Some(t) => self.find_loc(loc, &t, false, None),
                    None => ast_visitor::jsx_attribute_name_identifier_default(self, ident),
                }
            } else {
                ast_visitor::jsx_attribute_name_identifier_default(self, ident)
            }
        }

        fn declare_module(
            &mut self,
            loc: &'ast ALoc,
            m: &'ast ast::statement::DeclareModule<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            match &m.id {
                ast::statement::declare_module::Id::Identifier(id) => {
                    let (id_loc, _) = &id.loc;
                    if self.covers_target(id_loc) {
                        return Err(FoundResult::FoundHardcodedModule(
                            id_loc.dupe(),
                            id.name.dupe(),
                        ));
                    }
                }
                ast::statement::declare_module::Id::Literal(_) => {}
            }
            ast_visitor::declare_module_default(self, loc, m)
        }

        fn match_object_pattern_property(
            &mut self,
            prop: &'ast ast::match_pattern::object_pattern::Property<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::match_pattern::object_pattern::Property;
            match prop {
                Property::Valid { loc, property } if self.covers_target(loc) => {
                    // If shorthand, skip looking at the key which doesn't exist in the source.
                    if property.shorthand {
                        ast_visitor::match_pattern_default(self, &property.pattern)?;
                        Ok(())
                    } else {
                        ast_visitor::match_object_pattern_property_default(self, prop)
                    }
                }
                _ => ast_visitor::match_object_pattern_property_default(self, prop),
            }
        }

        // The hooks below fire when the target is a binding's own name, so that
        // hover can frame the type as a declaration. Each checks the bound
        // identifier directly and falls through to the default walk otherwise, so a
        // target anywhere else inside the declaration is unaffected.
        // TODO: also frame `import` specifiers as `(alias)` and enum members as
        // `const e: E.A`, both of which need more than the bound name and its type.

        fn variable_declarator(
            &mut self,
            kind: ast::VariableKind,
            declarator: &'ast ast::statement::variable::Declarator<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            self.variable_binder(kind, declarator)?;
            ast_visitor::variable_declarator_default(self, kind, declarator)
        }

        fn declare_variable_declarator(
            &mut self,
            kind: ast::VariableKind,
            declarator: &'ast ast::statement::variable::Declarator<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            self.variable_binder(kind, declarator)?;
            ast_visitor::declare_variable_declarator_default(self, kind, declarator)
        }

        fn function_declaration(
            &mut self,
            loc: &'ast ALoc,
            func: &'ast ast::function::Function<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            self.function_binder(func.id.as_ref())?;
            ast_visitor::function_declaration_default(self, loc, func)
        }

        fn declare_function(
            &mut self,
            loc: &'ast ALoc,
            decl: &'ast ast::statement::DeclareFunction<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            self.function_binder(decl.id.as_ref())?;
            ast_visitor::declare_function_default(self, loc, decl)
        }

        fn class_method(
            &mut self,
            method: &'ast ast::class::Method<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::class::MethodKind;
            let kind = match method.kind {
                MethodKind::Method => Some(BinderKind::Method),
                MethodKind::Get => Some(BinderKind::Getter),
                MethodKind::Set => Some(BinderKind::Setter),
                // A constructor is reported as its class, not as a member.
                MethodKind::Constructor => None,
            };
            if let Some(kind) = kind
                && let ast::expression::object::Key::Identifier(id) = &method.key
                && let (loc, t) = &id.loc
                && self.covers_target(loc)
            {
                return self.find_binder(loc, t, kind, &id.name);
            }
            ast_visitor::class_method_default(self, method)
        }

        fn class_property(
            &mut self,
            prop: &'ast ast::class::Property<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            if let ast::expression::object::Key::Identifier(id) = &prop.key
                && let (loc, t) = &id.loc
                && self.covers_target(loc)
            {
                return self.find_binder(loc, t, BinderKind::Property, &id.name);
            }
            ast_visitor::class_property_default(self, prop)
        }

        fn function_param(
            &mut self,
            param: &'ast ast::function::Param<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            if let ast::function::Param::RegularParam { argument, .. } = param
                && let Some(id) = Self::simple_pattern_binder(argument)
                && let (loc, t) = &id.loc
                && self.covers_target(loc)
            {
                return self.find_binder(loc, t, BinderKind::Parameter, &id.name);
            }
            ast_visitor::function_param_default(self, param)
        }

        fn call_type_arg(
            &mut self,
            t: &'ast ast::expression::CallTypeArg<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            use ast::expression::CallTypeArg;
            match t {
                CallTypeArg::Implicit(implicit) => {
                    let (loc, ty) = &implicit.loc;
                    if self.covers_target(loc) {
                        self.find_loc(loc, ty, false, None)
                    } else {
                        ast_visitor::call_type_arg_default(self, t)
                    }
                }
                _ => ast_visitor::call_type_arg_default(self, t),
            }
        }
    }

    pub fn find(
        cx: &Context<'_>,
        typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
        loc: Loc,
    ) -> Result<TypeAtPosResult, flow_utils_concurrency::job_error::JobError> {
        let mut searcher = TypeAtPosSearcher {
            cx,
            target_loc: loc,
            rev_bound_tparams: Vec::new(),
            enclosing_nominal: None,
        };
        match searcher.program(typed_ast) {
            Ok(()) => Ok(TypeAtPosResult::NoResult),
            Err(FoundResult::FoundType {
                loc,
                is_type_identifier_reference,
                type_,
                framing,
            }) => Ok(TypeAtPosResult::TypeResult {
                loc: loc.to_loc_exn().dupe(),
                is_type_identifier_reference,
                type_,
                framing,
            }),
            Err(FoundResult::FoundHardcodedModule(loc, name)) => Ok(
                TypeAtPosResult::HardcodedModuleResult(loc.to_loc_exn().dupe(), name),
            ),
            Err(FoundResult::Canceled(e)) => Err(e),
        }
    }
}

pub fn find_type_at_pos_annotation(
    cx: &Context<'_>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    loc: Loc,
) -> Result<type_at_pos::TypeAtPosResult, flow_utils_concurrency::job_error::JobError> {
    type_at_pos::find(cx, typed_ast, loc)
}
