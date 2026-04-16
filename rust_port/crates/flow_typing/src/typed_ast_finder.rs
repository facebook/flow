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

use flow_typing_utils::abnormal::AbnormalControlFlow;

pub fn infer_node<'a>(
    cx: &Context<'a>,
    node: EnclosingNode<ALoc, ALoc>,
) -> Result<EnclosingNode<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    match node {
        EnclosingNode::EnclosingProgram(prog) => {
            let ast::Program {
                loc: prog_aloc,
                statements,
                interpreter,
                comments,
                all_comments,
            } = prog;
            let statements = statement::statement_list(cx, &statements);
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

    pub enum TypeAtPosResult {
        TypeResult(Loc, bool, Type),
        HardcodedModuleResult(Loc, FlowSmolStr),
        NoResult,
    }

    enum FoundResult {
        FoundType(ALoc, bool, Type),
        FoundHardcodedModule(ALoc, FlowSmolStr),
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
        ) -> Result<(), FoundResult> {
            Err(FoundResult::FoundType(
                loc.dupe(),
                is_type_identifier,
                t.dupe(),
            ))
        }

        fn make_typeparam(&self, tparam: &ast::types::TypeParam<ALoc, (ALoc, Type)>) -> TypeParam {
            let (name_loc, _) = &tparam.name.loc;
            let name = &tparam.name.name;
            let reason = reason::mk_annot_reason(
                VirtualReasonDesc::RType(Name::new(name.dupe())),
                name_loc.dupe(),
            );
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
                self.find_loc(loc, t, false)
            } else {
                ast_visitor::identifier_default(self, id)
            }
        }

        fn type_identifier_reference(
            &mut self,
            id: &'ast ast::Identifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), FoundResult> {
            let (loc, t) = &id.loc;
            if self.covers_target(loc) {
                self.find_loc(loc, t, true)
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
                self.find_loc(loc, t, false)
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
                self.find_loc(loc, &t, false)
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
            let res = ast_visitor::class_default(self, loc, cls);
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
            let res = ast_visitor::declare_class_default(self, loc, decl);
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
            let res = ast_visitor::record_declaration_default(self, loc, record);
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
                    self.find_loc(loc, t, false)
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
                    self.find_loc(loc, t, false)
                }
                ExpressionInner::Member { loc: (_, t), inner }
                    if let member::Property::PropertyPrivateName(pn) = &inner.property
                        && self.covers_target(&pn.loc) =>
                {
                    self.find_loc(&pn.loc, t, false)
                }
                ExpressionInner::OptionalMember { loc: (_, t), inner }
                    if let member::Property::PropertyPrivateName(pn) = &inner.member.property
                        && self.covers_target(&pn.loc) =>
                {
                    self.find_loc(&pn.loc, t, false)
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
                    Some(t) => self.find_loc(callee_loc, &t, false),
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
                let hint_result = lazy_hint(self.cx, false, None, reason);
                // Split with_hint_result to avoid double &mut self borrow in closures
                let hint_t = type_hint::with_hint_result(Some, || None, hint_result);
                match hint_t {
                    Some(t) => self.find_loc(loc, &t, false),
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
    }

    pub fn find(
        cx: &Context<'_>,
        typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
        loc: Loc,
    ) -> TypeAtPosResult {
        let mut searcher = TypeAtPosSearcher {
            cx,
            target_loc: loc,
            rev_bound_tparams: Vec::new(),
        };
        match searcher.program(typed_ast) {
            Ok(()) => TypeAtPosResult::NoResult,
            Err(FoundResult::FoundType(loc, is_type_id, scheme)) => {
                TypeAtPosResult::TypeResult(loc.to_loc_exn().dupe(), is_type_id, scheme)
            }
            Err(FoundResult::FoundHardcodedModule(loc, name)) => {
                TypeAtPosResult::HardcodedModuleResult(loc.to_loc_exn().dupe(), name)
            }
        }
    }
}

pub fn find_type_at_pos_annotation(
    cx: &Context<'_>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    loc: Loc,
) -> type_at_pos::TypeAtPosResult {
    type_at_pos::find(cx, typed_ast, loc)
}
