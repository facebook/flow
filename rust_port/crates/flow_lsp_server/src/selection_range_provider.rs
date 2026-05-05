/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::function;
use flow_parser::ast::jsx;
use flow_parser::ast::pattern;
use flow_parser::ast::statement;
use flow_parser::ast::types;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::TypeParamsContext;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_server_env::flow_lsp_conversions::selection_range_of_loc;
use flow_server_env::lsp::lsp_position_to_flow_position;
use lsp_types::Position as LspPosition;
use lsp_types::SelectionRange;

struct SelectionRangeFinder {
    position: Position,
    acc: Vec<Loc>,
}

impl SelectionRangeFinder {
    fn new(position: Position) -> Self {
        SelectionRangeFinder {
            position,
            acc: vec![],
        }
    }

    fn add_loc(&mut self, loc: Loc) {
        match self.acc.last() {
            Some(hd) if *hd == loc => {}
            _ => self.acc.push(loc),
        }
    }

    fn contains(&self, loc: &Loc) -> bool {
        loc.start <= self.position && self.position <= loc.end
    }

    // recurse first, so we end up with the parent at the head of the results
    fn test_loc<F>(&mut self, loc: &Loc, f: F)
    where
        F: FnOnce(&mut Self),
    {
        if self.contains(loc) {
            f(self);
            self.add_loc(loc.dupe());
        }
    }

    // recurse first, so we end up with the parent at the head of the results
    fn test_with_loc<F>(&mut self, loc: &Loc, f: F)
    where
        F: FnOnce(&mut Self),
    {
        if self.contains(loc) {
            f(self);
            self.add_loc(loc.dupe());
        }
    }
}

impl<'ast> AstVisitor<'ast, Loc> for SelectionRangeFinder {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> Result<(), !> {
        let loc = &program.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::program_default(this, program).into_ok();
        });

        // add the whole file.
        let should_add = match self.acc.last() {
            None => true,
            Some(hd) if hd.start.line > 1 || hd.start.column > 0 => true,
            _ => false,
        };
        if should_add {
            let mut full_loc = loc.dupe();
            full_loc.start = Position { line: 1, column: 0 };
            self.add_loc(full_loc);
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &'ast ast::statement::Statement<Loc, Loc>) -> Result<(), !> {
        let loc = stmt.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::statement_default(this, stmt).into_ok();
        });
        Ok(())
    }

    fn expression(&mut self, expr: &'ast expression::Expression<Loc, Loc>) -> Result<(), !> {
        let loc = expr.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::expression_default(this, expr).into_ok();
        });
        Ok(())
    }

    fn arg_list(&mut self, args: &'ast expression::ArgList<Loc, Loc>) -> Result<(), !> {
        let loc = &args.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::arg_list_default(this, args).into_ok();
        });
        Ok(())
    }

    fn call_type_args(
        &mut self,
        args: &'ast ast::expression::CallTypeArgs<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &args.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::call_type_args_default(this, args).into_ok();
        });
        Ok(())
    }

    fn catch_clause(
        &mut self,
        clause: &'ast statement::try_::CatchClause<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &clause.loc;
        self.test_loc(loc, |this| {
            ast_visitor::catch_clause_default(this, clause).into_ok();
        });
        Ok(())
    }

    fn class_body(&mut self, body: &'ast ast::class::Body<Loc, Loc>) -> Result<(), !> {
        let loc = &body.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::class_body_default(this, body).into_ok();
        });
        Ok(())
    }

    fn class_extends(&mut self, extends: &'ast ast::class::Extends<Loc, Loc>) -> Result<(), !> {
        let loc = &extends.loc;
        self.test_loc(loc, |this| {
            ast_visitor::class_extends_default(this, extends).into_ok();
        });
        Ok(())
    }

    fn class_implements(
        &mut self,
        implements: &'ast ast::class::Implements<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &implements.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::class_implements_default(this, implements).into_ok();
        });
        Ok(())
    }

    fn class_implements_interface(
        &mut self,
        iface: &'ast ast::class::implements::Interface<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &iface.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::class_implements_interface_default(this, iface).into_ok();
        });
        Ok(())
    }

    fn class_method(&mut self, meth: &'ast ast::class::Method<Loc, Loc>) -> Result<(), !> {
        let loc = &meth.loc;
        self.test_loc(loc, |this| {
            ast_visitor::class_method_default(this, meth).into_ok();
        });
        Ok(())
    }

    fn class_property(&mut self, prop: &'ast ast::class::Property<Loc, Loc>) -> Result<(), !> {
        let loc = &prop.loc;
        self.test_loc(loc, |this| {
            ast_visitor::class_property_default(this, prop).into_ok();
        });
        Ok(())
    }

    fn class_private_field(
        &mut self,
        field: &'ast ast::class::PrivateField<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &field.loc;
        self.test_loc(loc, |this| {
            ast_visitor::class_private_field_default(this, field).into_ok();
        });
        Ok(())
    }

    fn enum_body(&mut self, body: &'ast statement::enum_declaration::Body<Loc>) -> Result<(), !> {
        let loc = body.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::enum_body_default(this, body).into_ok();
        });
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &'ast Loc,
        func: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        let sig_loc = &func.sig_loc;
        ast_visitor::function_declaration_default(self, loc, func).into_ok();
        if self.contains(sig_loc) {
            self.add_loc(sig_loc.dupe());
        }
        Ok(())
    }

    fn function_expression_or_method(
        &mut self,
        loc: &'ast Loc,
        func: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        let sig_loc = &func.sig_loc;
        ast_visitor::function_expression_or_method_default(self, loc, func).into_ok();
        if self.contains(sig_loc) {
            self.add_loc(sig_loc.dupe());
        }
        Ok(())
    }

    fn function_params(&mut self, params: &'ast function::Params<Loc, Loc>) -> Result<(), !> {
        let loc = &params.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_params_default(this, params).into_ok();
        });
        Ok(())
    }

    fn function_this_param(&mut self, param: &'ast function::ThisParam<Loc, Loc>) -> Result<(), !> {
        let loc = &param.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_this_param_default(this, param).into_ok();
        });
        Ok(())
    }

    fn function_param(&mut self, param: &'ast function::Param<Loc, Loc>) -> Result<(), !> {
        let loc = match param {
            function::Param::RegularParam { loc, .. } => loc,
            function::Param::ParamProperty { loc, .. } => loc,
        };
        self.test_with_loc(loc, |this| {
            ast_visitor::function_param_default(this, param).into_ok();
        });
        Ok(())
    }

    fn function_body(&mut self, body: &'ast (Loc, statement::Block<Loc, Loc>)) -> Result<(), !> {
        let loc = &body.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_body_default(this, body).into_ok();
        });
        Ok(())
    }

    fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
        let loc = &id.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::identifier_default(this, id).into_ok();
        });
        Ok(())
    }

    fn type_(&mut self, t: &'ast types::Type<Loc, Loc>) -> Result<(), !> {
        let loc = t.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::type_default(this, t).into_ok();
        });
        Ok(())
    }

    fn type_annotation(&mut self, annot: &'ast types::Annotation<Loc, Loc>) -> Result<(), !> {
        let loc = &annot.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::type_annotation_default(this, annot).into_ok();
        });
        Ok(())
    }

    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        decl: &'ast statement::variable::Declarator<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &decl.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::variable_declarator_default(this, kind, decl).into_ok();
        });
        Ok(())
    }

    fn pattern(
        &mut self,
        kind: Option<ast::VariableKind>,
        patt: &'ast ast::pattern::Pattern<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = patt.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_default(this, kind, patt).into_ok();
        });
        Ok(())
    }

    fn object_key(&mut self, key: &'ast expression::object::Key<Loc, Loc>) -> Result<(), !> {
        match key {
            expression::object::Key::StringLiteral((loc, _))
            | expression::object::Key::NumberLiteral((loc, _))
            | expression::object::Key::BigIntLiteral((loc, _)) => {
                if self.contains(loc) {
                    self.add_loc(loc.dupe());
                }
            }
            expression::object::Key::Identifier(_)
            | expression::object::Key::PrivateName(_)
            | expression::object::Key::Computed(_) => {}
        }
        ast_visitor::object_key_default(self, key)
    }

    fn object_property(
        &mut self,
        prop: &'ast expression::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = prop.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::object_property_default(this, prop).into_ok();
        });
        Ok(())
    }

    fn spread_element(
        &mut self,
        spread: &'ast expression::SpreadElement<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &spread.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::spread_element_default(this, spread).into_ok();
        });
        Ok(())
    }

    fn spread_property(
        &mut self,
        spread: &'ast expression::object::SpreadProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &spread.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::spread_property_default(this, spread).into_ok();
        });
        Ok(())
    }

    fn switch_case(&mut self, case: &'ast statement::switch::Case<Loc, Loc>) -> Result<(), !> {
        let loc = &case.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::switch_case_default(this, case).into_ok();
        });
        Ok(())
    }

    fn enum_defaulted_member(
        &mut self,
        member: &'ast statement::enum_declaration::DefaultedMember<Loc>,
    ) -> Result<(), !> {
        let loc = &member.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::enum_defaulted_member_default(this, member).into_ok();
        });
        Ok(())
    }

    fn enum_boolean_member(
        &mut self,
        member: &'ast statement::enum_declaration::InitializedMember<ast::BooleanLiteral<Loc>, Loc>,
    ) -> Result<(), !> {
        let loc = &member.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::enum_boolean_member_default(this, member).into_ok();
        });
        Ok(())
    }

    fn enum_number_member(
        &mut self,
        member: &'ast statement::enum_declaration::InitializedMember<ast::NumberLiteral<Loc>, Loc>,
    ) -> Result<(), !> {
        let loc = &member.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::enum_number_member_default(this, member).into_ok();
        });
        Ok(())
    }

    fn enum_string_member(
        &mut self,
        member: &'ast statement::enum_declaration::InitializedMember<ast::StringLiteral<Loc>, Loc>,
    ) -> Result<(), !> {
        let loc = &member.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::enum_string_member_default(this, member).into_ok();
        });
        Ok(())
    }

    fn export_named_declaration_specifier(
        &mut self,
        spec: &'ast statement::export_named_declaration::ExportSpecifier<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &spec.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::export_named_declaration_specifier_default(this, spec).into_ok();
        });
        Ok(())
    }

    fn export_batch_specifier(
        &mut self,
        spec: &'ast statement::export_named_declaration::ExportBatchSpecifier<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &spec.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::export_batch_specifier_default(this, spec).into_ok();
        });
        Ok(())
    }

    fn for_in_left_declaration(
        &mut self,
        left: &'ast (Loc, statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), !> {
        let loc = &left.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::for_in_left_declaration_default(this, left).into_ok();
        });
        Ok(())
    }

    fn for_of_left_declaration(
        &mut self,
        left: &'ast (Loc, statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), !> {
        let loc = &left.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::for_of_left_declaration_default(this, left).into_ok();
        });
        Ok(())
    }

    fn for_init_declaration(
        &mut self,
        init: &'ast (Loc, statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), !> {
        let loc = &init.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::for_init_declaration_default(this, init).into_ok();
        });
        Ok(())
    }

    fn function_param_type(
        &mut self,
        fpt: &'ast types::function::Param<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &fpt.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_param_type_default(this, fpt).into_ok();
        });
        Ok(())
    }

    fn function_rest_param_type(
        &mut self,
        frpt: &'ast types::function::RestParam<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &frpt.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_rest_param_type_default(this, frpt).into_ok();
        });
        Ok(())
    }

    fn function_this_param_type(
        &mut self,
        this_param: &'ast types::function::ThisParam<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &this_param.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_this_param_type_default(this, this_param).into_ok();
        });
        Ok(())
    }

    fn object_type_property_getter(
        &mut self,
        loc: &'ast Loc,
        getter: &'ast types::Function<Loc, Loc>,
    ) -> Result<(), !> {
        self.test_loc(loc, |this| {
            ast_visitor::object_type_property_getter_default(this, loc, getter).into_ok();
        });
        Ok(())
    }

    fn object_type_property_setter(
        &mut self,
        loc: &'ast Loc,
        setter: &'ast types::Function<Loc, Loc>,
    ) -> Result<(), !> {
        self.test_loc(loc, |this| {
            ast_visitor::object_type_property_setter_default(this, loc, setter).into_ok();
        });
        Ok(())
    }

    fn object_property_type(
        &mut self,
        prop: &'ast types::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &prop.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::object_property_type_default(this, prop).into_ok();
        });
        Ok(())
    }

    fn object_spread_property_type(
        &mut self,
        spread: &'ast types::object::SpreadProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &spread.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::object_spread_property_type_default(this, spread).into_ok();
        });
        Ok(())
    }

    fn object_indexer_property_type(
        &mut self,
        indexer: &'ast types::object::Indexer<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &indexer.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::object_indexer_property_type_default(this, indexer).into_ok();
        });
        Ok(())
    }

    fn object_internal_slot_property_type(
        &mut self,
        slot: &'ast types::object::InternalSlot<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &slot.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::object_internal_slot_property_type_default(this, slot).into_ok();
        });
        Ok(())
    }

    fn object_call_property_type(
        &mut self,
        call_prop: &'ast types::object::CallProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &call_prop.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::object_call_property_type_default(this, call_prop).into_ok();
        });
        Ok(())
    }

    fn generic_qualified_identifier_type(
        &mut self,
        qual: &'ast types::generic::Qualified<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &qual.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::generic_qualified_identifier_type_default(this, qual).into_ok();
        });
        Ok(())
    }

    fn variance(&mut self, variance: &'ast ast::Variance<Loc>) -> Result<(), !> {
        let loc = &variance.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::variance_default(this, variance).into_ok();
        });
        Ok(())
    }

    fn type_args(&mut self, targs: &'ast types::TypeArgs<Loc, Loc>) -> Result<(), !> {
        let loc = &targs.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::type_args_default(this, targs).into_ok();
        });
        Ok(())
    }

    fn type_params(
        &mut self,
        kind: &TypeParamsContext,
        tparams: &'ast types::TypeParams<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &tparams.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::type_params_default(this, kind, tparams).into_ok();
        });
        Ok(())
    }

    fn type_param(
        &mut self,
        kind: &TypeParamsContext,
        tparam: &'ast types::TypeParam<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &tparam.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::type_param_default(this, kind, tparam).into_ok();
        });
        Ok(())
    }

    fn private_name(&mut self, id: &'ast ast::PrivateName<Loc>) -> Result<(), !> {
        let loc = &id.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::private_name_default(this, id).into_ok();
        });
        Ok(())
    }

    fn computed_key(&mut self, key: &'ast ast::ComputedKey<Loc, Loc>) -> Result<(), !> {
        let loc = &key.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::computed_key_default(this, key).into_ok();
        });
        Ok(())
    }

    fn import_namespace_specifier(
        &mut self,
        import_kind: statement::ImportKind,
        loc: &'ast Loc,
        id: &'ast ast::Identifier<Loc, Loc>,
    ) -> Result<(), !> {
        self.test_loc(loc, |this| {
            ast_visitor::import_namespace_specifier_default(this, import_kind, loc, id).into_ok();
        });
        Ok(())
    }

    fn jsx_opening_element(&mut self, elem: &'ast jsx::Opening<Loc, Loc>) -> Result<(), !> {
        let loc = &elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_opening_element_default(this, elem).into_ok();
        });
        Ok(())
    }

    fn jsx_closing_element(&mut self, elem: &'ast jsx::Closing<Loc, Loc>) -> Result<(), !> {
        let loc = &elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_closing_element_default(this, elem).into_ok();
        });
        Ok(())
    }

    fn jsx_spread_attribute(
        &mut self,
        attr: &'ast jsx::SpreadAttribute<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &attr.loc;
        self.test_loc(loc, |this| {
            ast_visitor::jsx_spread_attribute_default(this, attr).into_ok();
        });
        Ok(())
    }

    fn jsx_attribute(&mut self, attr: &'ast jsx::Attribute<Loc, Loc>) -> Result<(), !> {
        let loc = &attr.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_attribute_default(this, attr).into_ok();
        });
        Ok(())
    }

    fn jsx_attribute_value_expression(
        &mut self,
        loc: &'ast Loc,
        jsx_expr: &'ast jsx::ExpressionContainer<Loc, Loc>,
    ) -> Result<(), !> {
        self.test_loc(loc, |this| {
            ast_visitor::jsx_attribute_value_expression_default(this, loc, jsx_expr).into_ok();
        });
        Ok(())
    }

    fn jsx_attribute_value_literal(
        &mut self,
        loc: &'ast Loc,
        lit: &'ast ast::StringLiteral<Loc>,
    ) -> Result<(), !> {
        self.test_loc(loc, |this| {
            ast_visitor::jsx_attribute_value_literal_default(this, loc, lit).into_ok();
        });
        Ok(())
    }

    fn jsx_children(
        &mut self,
        loc: &'ast Loc,
        children: &'ast Vec<jsx::Child<Loc, Loc>>,
    ) -> Result<(), !> {
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_children_default(this, loc, children).into_ok();
        });
        Ok(())
    }

    fn jsx_child(&mut self, child: &'ast jsx::Child<Loc, Loc>) -> Result<(), !> {
        let loc = child.loc();
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_child_default(this, child).into_ok();
        });
        Ok(())
    }

    fn jsx_namespaced_name(&mut self, ns: &'ast jsx::NamespacedName<Loc, Loc>) -> Result<(), !> {
        let loc = &ns.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_namespaced_name_default(this, ns).into_ok();
        });
        Ok(())
    }

    fn jsx_member_expression(
        &mut self,
        member: &'ast jsx::MemberExpression<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &member.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_member_expression_default(this, member).into_ok();
        });
        Ok(())
    }

    fn jsx_identifier(&mut self, ident: &'ast jsx::Identifier<Loc, Loc>) -> Result<(), !> {
        let loc = &ident.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::jsx_identifier_default(this, ident).into_ok();
        });
        Ok(())
    }

    fn pattern_object_property(
        &mut self,
        kind: Option<ast::VariableKind>,
        prop: &'ast pattern::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &prop.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_object_property_default(this, kind, prop).into_ok();
        });
        Ok(())
    }

    fn pattern_object_property_string_literal_key(
        &mut self,
        kind: Option<ast::VariableKind>,
        literal: &'ast (Loc, ast::StringLiteral<Loc>),
    ) -> Result<(), !> {
        let loc = &literal.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_object_property_string_literal_key_default(this, kind, literal)
                .into_ok();
        });
        Ok(())
    }

    fn pattern_object_property_number_literal_key(
        &mut self,
        kind: Option<ast::VariableKind>,
        literal: &'ast (Loc, ast::NumberLiteral<Loc>),
    ) -> Result<(), !> {
        let loc = &literal.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_object_property_number_literal_key_default(this, kind, literal)
                .into_ok();
        });
        Ok(())
    }

    fn pattern_object_property_bigint_literal_key(
        &mut self,
        kind: Option<ast::VariableKind>,
        literal: &'ast (Loc, ast::BigIntLiteral<Loc>),
    ) -> Result<(), !> {
        let loc = &literal.0;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_object_property_bigint_literal_key_default(this, kind, literal)
                .into_ok();
        });
        Ok(())
    }

    fn pattern_object_rest_property(
        &mut self,
        kind: Option<ast::VariableKind>,
        rest_elem: &'ast pattern::RestElement<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &rest_elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_object_rest_property_default(this, kind, rest_elem).into_ok();
        });
        Ok(())
    }

    fn pattern_array_element(
        &mut self,
        kind: Option<ast::VariableKind>,
        elem: &'ast pattern::array::NormalElement<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_array_element_default(this, kind, elem).into_ok();
        });
        Ok(())
    }

    fn pattern_array_rest_element(
        &mut self,
        kind: Option<ast::VariableKind>,
        rest_elem: &'ast pattern::RestElement<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &rest_elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::pattern_array_rest_element_default(this, kind, rest_elem).into_ok();
        });
        Ok(())
    }

    fn predicate(&mut self, pred: &'ast types::Predicate<Loc, Loc>) -> Result<(), !> {
        let loc = &pred.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::predicate_default(this, pred).into_ok();
        });
        Ok(())
    }

    fn function_rest_param(&mut self, param: &'ast function::RestParam<Loc, Loc>) -> Result<(), !> {
        let loc = &param.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::function_rest_param_default(this, param).into_ok();
        });
        Ok(())
    }

    fn template_literal_element(
        &mut self,
        elem: &'ast expression::template_literal::Element<Loc>,
    ) -> Result<(), !> {
        let loc = &elem.loc;
        self.test_with_loc(loc, |this| {
            ast_visitor::template_literal_element_default(this, elem).into_ok();
        });
        Ok(())
    }
}

fn selection_range_tree(
    position: Position,
    program: &ast::Program<Loc, Loc>,
) -> Option<SelectionRange> {
    let mut finder = SelectionRangeFinder::new(position);
    finder.program(program).into_ok();
    let acc = finder.acc;
    let mut parent: Option<SelectionRange> = None;
    for loc in acc.into_iter().rev() {
        parent = Some(selection_range_of_loc(parent.map(Box::new), &loc));
    }
    parent
}

pub fn provide_selection_ranges(
    positions: &[LspPosition],
    program: &ast::Program<Loc, Loc>,
) -> Result<Vec<SelectionRange>, String> {
    let mut rev_results = Vec::new();
    for lsp_position in positions {
        let position = lsp_position_to_flow_position(lsp_position);
        match selection_range_tree(position, program) {
            Some(range) => rev_results.push(range),
            None => {
                return Err(format!(
                    "Invalid position: {}:{}",
                    lsp_position.line, lsp_position.character
                ));
            }
        }
    }
    Ok(rev_results)
}
