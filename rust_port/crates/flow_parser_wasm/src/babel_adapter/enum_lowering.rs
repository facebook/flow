/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use super::builders;

#[derive(Clone, Copy)]
pub enum EnumRuntime {
    Default,
    CustomPlaceholder,
}

struct EnumLowerer {
    runtime: EnumRuntime,
    next_placeholder: usize,
}

impl EnumLowerer {
    fn runtime_expression(&mut self) -> expression::Expression<Loc, Loc> {
        match self.runtime {
            EnumRuntime::Default => builders::call(
                builders::identifier("require"),
                vec![builders::string_literal("flow-enums-runtime")],
            ),
            EnumRuntime::CustomPlaceholder => {
                let index = self.next_placeholder;
                self.next_placeholder += 1;
                builders::identifier(&format!("\0flow_enum_runtime_{index}"))
            }
        }
    }

    fn member_name(
        member: &statement::enum_declaration::Member<Loc>,
    ) -> &flow_data_structure_wrapper::smol_str::FlowSmolStr {
        use statement::enum_declaration::Member;

        let name = match member {
            Member::BooleanMember(member) => &member.id,
            Member::NumberMember(member) => &member.id,
            Member::StringMember(member) => &member.id,
            Member::BigIntMember(member) => &member.id,
            Member::DefaultedMember(member) => &member.id,
        };
        match name {
            statement::enum_declaration::MemberName::Identifier(id) => &id.name,
            statement::enum_declaration::MemberName::StringLiteral(_, literal) => &literal.value,
        }
    }

    fn member_key(
        member: &statement::enum_declaration::Member<Loc>,
    ) -> expression::object::Key<Loc, Loc> {
        use statement::enum_declaration::Member;

        let name = match member {
            Member::BooleanMember(member) => &member.id,
            Member::NumberMember(member) => &member.id,
            Member::StringMember(member) => &member.id,
            Member::BigIntMember(member) => &member.id,
            Member::DefaultedMember(member) => &member.id,
        };
        match name {
            statement::enum_declaration::MemberName::Identifier(id) => {
                expression::object::Key::Identifier(id.dupe())
            }
            statement::enum_declaration::MemberName::StringLiteral(loc, literal) => {
                expression::object::Key::StringLiteral((loc.dupe(), literal.clone()))
            }
        }
    }

    fn map_enum(
        &mut self,
        declaration: &statement::EnumDeclaration<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        let runtime = self.runtime_expression();
        let mirrored = declaration.body.members.first().is_none_or(|member| {
            matches!(
                member,
                statement::enum_declaration::Member::DefaultedMember(_)
            )
        }) && declaration
            .body
            .explicit_type
            .as_ref()
            .is_none_or(|(_, explicit)| {
                *explicit == statement::enum_declaration::ExplicitType::String
            });

        let value = if mirrored {
            let elements = declaration
                .body
                .members
                .iter()
                .map(|member| {
                    ast_builder::expressions::array_expression(builders::string_literal(
                        Self::member_name(member).as_str(),
                    ))
                })
                .collect();
            builders::call(
                builders::member(runtime, "Mirrored"),
                vec![ast_builder::expressions::array(
                    Some(builders::generated_loc()),
                    None,
                    elements,
                )],
            )
        } else {
            let properties = declaration
                .body
                .members
                .iter()
                .map(|member| {
                    let value =
                        builders::expression_from_enum_member(member).unwrap_or_else(|| {
                            builders::call(
                                builders::identifier("Symbol"),
                                vec![builders::string_literal(Self::member_name(member).as_str())],
                            )
                        });
                    ast_builder::expressions::object_property(
                        Some(false),
                        Some(builders::generated_loc()),
                        Self::member_key(member),
                        value,
                    )
                })
                .collect();
            builders::call(
                runtime,
                vec![ast_builder::expressions::object_(
                    None,
                    Some(builders::generated_loc()),
                    properties,
                )],
            )
        };

        ast_builder::statements::const_declaration(
            Some(builders::generated_loc()),
            None,
            vec![ast_builder::statements::variable_declarator_generic(
                Some(builders::generated_loc()),
                builders::identifier_pattern(&declaration.id),
                Some(value),
            )],
        )
    }
}

impl<'ast> AstVisitor<'ast, Loc> for EnumLowerer {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_statement(
        &mut self,
        statement: &'ast statement::Statement<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        match &**statement {
            StatementInner::EnumDeclaration { inner, .. } => self.map_enum(inner),
            _ => ast_visitor::map_statement_default(self, statement),
        }
    }

    fn map_statement_list(
        &mut self,
        statements: &'ast Arc<[statement::Statement<Loc, Loc>]>,
    ) -> Arc<[statement::Statement<Loc, Loc>]> {
        let mut lowered = Vec::with_capacity(statements.len());
        for statement in statements.iter() {
            if let StatementInner::ExportDefaultDeclaration { loc, inner } = &**statement
                && let statement::export_default_declaration::Declaration::Declaration(declaration) =
                    &inner.declaration
                && let StatementInner::EnumDeclaration {
                    inner: enum_declaration,
                    ..
                } = &**declaration
            {
                lowered.push(self.map_enum(enum_declaration));
                lowered.push(statement::Statement::new(
                    StatementInner::ExportDefaultDeclaration {
                        loc: loc.dupe(),
                        inner: Arc::new(statement::ExportDefaultDeclaration {
                            default: inner.default.dupe(),
                            declaration:
                                statement::export_default_declaration::Declaration::Expression(
                                    expression::Expression::new(ExpressionInner::Identifier {
                                        loc: enum_declaration.id.loc.dupe(),
                                        inner: enum_declaration.id.dupe(),
                                    }),
                                ),
                            comments: inner.comments.dupe(),
                        }),
                    },
                ));
                continue;
            }
            lowered.push(self.map_statement(statement));
        }
        lowered.into()
    }
}

pub fn lower_program(
    program: &ast::Program<Loc, Loc>,
    runtime: EnumRuntime,
) -> ast::Program<Loc, Loc> {
    EnumLowerer {
        runtime,
        next_placeholder: 0,
    }
    .map_program(program)
}
