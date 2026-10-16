/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::convert::Infallible;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
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
use super::gen_id::GenId;

#[derive(Clone, Copy)]
pub enum EnumRuntime {
    Default,
    CustomPlaceholder,
    Import,
}

struct EnumLowerer {
    runtime: EnumRuntime,
    next_placeholder: usize,
    gen_id: GenId,
    imported_runtime: Option<FlowSmolStr>,
}

impl EnumLowerer {
    fn runtime_expression(&mut self, loc: &Loc) -> expression::Expression<Loc, Loc> {
        match self.runtime {
            EnumRuntime::Default => builders::call(
                loc,
                builders::identifier(loc, "require"),
                vec![builders::string_literal(loc, "flow-enums-runtime")],
            ),
            EnumRuntime::CustomPlaceholder => {
                let index = self.next_placeholder;
                self.next_placeholder += 1;
                builders::identifier(loc, &format!("\0flow_enum_runtime_{index}"))
            }
            EnumRuntime::Import => {
                let name = self
                    .imported_runtime
                    .get_or_insert_with(|| self.gen_id.id());
                expression::Expression::new(ExpressionInner::Identifier {
                    loc: loc.dupe(),
                    inner: ast::Identifier::new(ast::IdentifierInner {
                        loc: loc.dupe(),
                        name: name.dupe(),
                        comments: None,
                    }),
                })
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

    fn member_loc(member: &statement::enum_declaration::Member<Loc>) -> &Loc {
        use statement::enum_declaration::Member;

        match member {
            Member::BooleanMember(member) => &member.loc,
            Member::NumberMember(member) => &member.loc,
            Member::StringMember(member) => &member.loc,
            Member::BigIntMember(member) => &member.loc,
            Member::DefaultedMember(member) => &member.loc,
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

    /// `construct_loc` is the span of the whole `enum ... { ... }` statement. Nodes that
    /// correspond to a specific piece of source (an object property key) take that piece's
    /// location instead; only scaffolding with no source counterpart — the runtime callee, the
    /// call, the `const` wrapper — falls back to the construct.
    fn map_enum(
        &mut self,
        construct_loc: &Loc,
        declaration: &statement::EnumDeclaration<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        let runtime = self.runtime_expression(construct_loc);
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
                        Self::member_loc(member),
                        Self::member_name(member).as_str(),
                    ))
                })
                .collect();
            builders::call(
                construct_loc,
                builders::member(construct_loc, runtime, "Mirrored"),
                vec![ast_builder::expressions::array(
                    Some(construct_loc.dupe()),
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
                    let member_loc = Self::member_loc(member);
                    let value =
                        builders::expression_from_enum_member(member).unwrap_or_else(|| {
                            builders::call(
                                member_loc,
                                builders::identifier(member_loc, "Symbol"),
                                vec![builders::string_literal(
                                    member_loc,
                                    Self::member_name(member).as_str(),
                                )],
                            )
                        });
                    ast_builder::expressions::object_property(
                        Some(false),
                        Some(member_loc.dupe()),
                        Self::member_key(member),
                        value,
                    )
                })
                .collect();
            builders::call(
                construct_loc,
                runtime,
                vec![ast_builder::expressions::object_(
                    None,
                    Some(construct_loc.dupe()),
                    properties,
                )],
            )
        };

        ast_builder::statements::const_declaration(
            Some(construct_loc.dupe()),
            None,
            vec![ast_builder::statements::variable_declarator_generic(
                Some(construct_loc.dupe()),
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
            StatementInner::EnumDeclaration { loc, inner } => self.map_enum(loc, inner),
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
                    loc: enum_loc,
                    inner: enum_declaration,
                } = &**declaration
            {
                // `loc` above is the `export default` statement; the synthesized `const` binding
                // belongs to the enum itself, which has its own narrower span.
                lowered.push(self.map_enum(enum_loc, enum_declaration));
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
    let mut lowerer = EnumLowerer {
        runtime,
        next_placeholder: 0,
        gen_id: GenId::new("enum_runtime"),
        imported_runtime: None,
    };
    if matches!(runtime, EnumRuntime::Import) {
        struct Usages<'a>(&'a mut GenId);
        impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Infallible> for Usages<'_> {
            fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
                loc
            }

            fn normalize_type(loc: &'ast Loc) -> &'ast Loc {
                loc
            }

            fn identifier(
                &mut self,
                identifier: &'ast ast::Identifier<Loc, Loc>,
            ) -> Result<(), Infallible> {
                self.0.add_usage(&identifier.name);
                Ok(())
            }
        }
        let Ok(()) = Usages(&mut lowerer.gen_id).program(program);
    }
    let mut program = lowerer.map_program(program);
    if let Some(name) = lowerer.imported_runtime {
        let import = ast_builder::statements::import_declaration(
            None,
            None,
            None,
            statement::ImportKind::ImportValue,
            (
                Loc::default(),
                ast::StringLiteral {
                    value: "flow-enums-runtime".into(),
                    raw: "'flow-enums-runtime'".into(),
                    comments: None,
                },
            ),
            Some(statement::import_declaration::DefaultIdentifier {
                identifier: ast::Identifier::new(ast::IdentifierInner {
                    loc: Loc::default(),
                    name,
                    comments: None,
                }),
                remote_default_name_def_loc: None,
            }),
            None,
        );
        let mut statements = vec![import];
        statements.extend(program.statements.iter().map(Dupe::dupe));
        program.statements = statements.into();
    }
    program
}
