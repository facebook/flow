/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(never_type)]

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::ast;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::function;
use flow_parser::ast::statement;
use flow_parser::ast::statement::Statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast::types;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::TypeParamsContext;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_parser::parse_program_without_file;
use flow_parser_utils::ast_builder;
use flow_parser_utils::flow_ast_differ::Change;
use flow_parser_utils::flow_ast_differ::DiffResult;
use flow_parser_utils::flow_ast_differ::list_diff;
use flow_parser_utils::flow_ast_differ::program;
use flow_parser_utils_output::ast_diff_printer;
use flow_parser_utils_output::js_layout_generator;
use pretty_assertions::assert_eq;

type Edit = ((i32, i32), &'static str);

#[derive(Clone, Copy)]
enum MapperKind {
    Useless,
    Literal,
    InsertVariance,
    DeleteVariance,
    InsertEnd,
    InsertBegin,
    InsertDup,
    FirstLastDup,
    InsertImport,
    InsertSecondImport,
    InsertSecondCjsImport,
    AddBody,
    Delete,
    DeleteEnd,
    DeleteAnnot,
    InsertAnnot,
    InsertFunctionAnnot,
    InsertImportAndAnnot,
    PropAnnot,
    FuncReturnAnnot,
    InsertTypecast,
    InsertCallTypeArgs,
    AddComment,
    TrueToFalse,
    RemoveAnnotationRest,
    DoubleSequence,
}

#[test]
fn first_last_dup_mapper_is_ported() {
    let _ = MapperKind::FirstLastDup;
}

fn ident(loc: Loc, name: &str) -> ast::Identifier<Loc, Loc> {
    ast_utils::ident_of_source(None, loc, FlowSmolStr::from(name))
}

fn number_type(loc: Loc) -> types::Type<Loc, Loc> {
    types::Type::new(TypeInner::Number {
        loc,
        comments: None,
    })
}

fn any_type(loc: Loc) -> types::Type<Loc, Loc> {
    types::Type::new(TypeInner::Any {
        loc,
        comments: None,
    })
}

fn annotation(loc: Loc, type_: types::Type<Loc, Loc>) -> types::Annotation<Loc, Loc> {
    types::Annotation {
        loc,
        annotation: type_,
    }
}

fn number_annotation(loc: Loc) -> types::Annotation<Loc, Loc> {
    annotation(loc.dupe(), number_type(loc))
}

fn function_type(loc: Loc) -> types::Type<Loc, Loc> {
    types::Type::new(TypeInner::Function {
        loc: loc.dupe(),
        inner: Arc::new(types::Function {
            tparams: None,
            params: types::function::Params {
                loc: loc.dupe(),
                this: None,
                params: Arc::from(Vec::<types::function::Param<Loc, Loc>>::new()),
                rest: None,
                comments: None,
            },
            return_: types::function::ReturnAnnotation::Available(number_annotation(loc.dupe())),
            comments: None,
            effect: function::Effect::Arbitrary,
        }),
    })
}

fn rename_name(name: &FlowSmolStr) -> Option<FlowSmolStr> {
    match name.as_str() {
        "rename" => Some(FlowSmolStr::from("gotRenamed")),
        "Rename" => Some(FlowSmolStr::from("GotRenamed")),
        "RENAME" => Some(FlowSmolStr::from("GOT_RENAMED")),
        _ => None,
    }
}

fn comments_with(
    leading: Vec<ast::Comment<Loc>>,
    trailing: Vec<ast::Comment<Loc>>,
) -> ast::Syntax<Loc, ()> {
    ast::Syntax {
        leading: Arc::from(leading),
        trailing: Arc::from(trailing),
        internal: (),
    }
}

struct UselessMapper;

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for UselessMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_number_literal(
        &mut self,
        lit: &'ast ast::NumberLiteral<Loc>,
    ) -> ast::NumberLiteral<Loc> {
        if lit.value == 4.0 {
            ast::NumberLiteral {
                value: 5.0,
                raw: FlowSmolStr::from("5"),
                comments: None,
            }
        } else {
            ast_visitor::map_number_literal_default(self, lit)
        }
    }

    fn map_string_literal(
        &mut self,
        lit: &'ast ast::StringLiteral<Loc>,
    ) -> ast::StringLiteral<Loc> {
        if lit.value.as_str() == "RenameSL" {
            ast::StringLiteral {
                value: FlowSmolStr::from("GotRenamedSL"),
                raw: FlowSmolStr::from("\"GotRenamedSL\""),
                comments: lit.comments.dupe(),
            }
        } else {
            ast_visitor::map_string_literal_default(self, lit)
        }
    }

    fn map_logical(
        &mut self,
        loc: &'ast Loc,
        expr: &'ast ast::expression::Logical<Loc, Loc>,
    ) -> ast::expression::Logical<Loc, Loc> {
        let mut expr = ast_visitor::map_logical_default(self, loc, expr);
        if expr.operator == ast::expression::LogicalOperator::NullishCoalesce {
            expr.operator = ast::expression::LogicalOperator::Or;
        }
        expr
    }

    fn map_binary(
        &mut self,
        loc: &'ast Loc,
        expr: &'ast ast::expression::Binary<Loc, Loc>,
    ) -> ast::expression::Binary<Loc, Loc> {
        let mut expr = ast_visitor::map_binary_default(self, loc, expr);
        expr.operator = match expr.operator {
            ast::expression::BinaryOperator::Plus => ast::expression::BinaryOperator::Minus,
            ast::expression::BinaryOperator::Mult => ast::expression::BinaryOperator::Plus,
            op => op,
        };
        expr
    }

    fn map_unary_expression(
        &mut self,
        loc: &'ast Loc,
        expr: &'ast ast::expression::Unary<Loc, Loc>,
    ) -> ast::expression::Unary<Loc, Loc> {
        let mut expr = ast_visitor::map_unary_expression_default(self, loc, expr);
        if expr.operator != ast::expression::UnaryOperator::Minus {
            expr.operator = ast::expression::UnaryOperator::Minus;
        }
        expr
    }

    fn map_identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> ast::Identifier<Loc, Loc> {
        if let Some(name) = rename_name(&id.name) {
            ast_utils::ident_of_source(None, id.loc.dupe(), name)
        } else {
            ast_visitor::map_identifier_default(self, id)
        }
    }

    fn map_private_name(&mut self, id: &'ast ast::PrivateName<Loc>) -> ast::PrivateName<Loc> {
        if let Some(name) = rename_name(&id.name) {
            ast::PrivateName {
                loc: id.loc.dupe(),
                name,
                comments: id.comments.dupe(),
            }
        } else {
            ast_visitor::map_private_name_default(self, id)
        }
    }

    fn map_variable_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::VariableDeclaration<Loc, Loc>,
    ) -> statement::VariableDeclaration<Loc, Loc> {
        let mut decl = ast_visitor::map_variable_declaration_default(self, loc, decl);
        if decl.kind == ast::VariableKind::Var {
            decl.kind = ast::VariableKind::Const;
        }
        decl
    }

    fn map_template_literal_element(
        &mut self,
        elem: &'ast ast::expression::template_literal::Element<Loc>,
    ) -> ast::expression::template_literal::Element<Loc> {
        if elem.value.raw.as_str() == "rename" {
            ast::expression::template_literal::Element {
                loc: elem.loc.dupe(),
                value: ast::expression::template_literal::Value {
                    raw: FlowSmolStr::from("gotRenamed"),
                    cooked: FlowSmolStr::from("gotRenamed"),
                },
                tail: elem.tail,
            }
        } else {
            ast_visitor::map_template_literal_element_default(self, elem)
        }
    }

    fn map_template_literal(
        &mut self,
        _loc: &'ast Loc,
        expr: &'ast ast::expression::TemplateLiteral<Loc, Loc>,
    ) -> ast::expression::TemplateLiteral<Loc, Loc> {
        ast::expression::TemplateLiteral {
            quasis: Arc::from(
                expr.quasis
                    .iter()
                    .map(|elem| self.map_template_literal_element(elem))
                    .collect::<Vec<_>>(),
            ),
            expressions: Arc::from(
                expr.expressions
                    .iter()
                    .map(|expr| self.map_expression(expr))
                    .collect::<Vec<_>>(),
            ),
            comments: self.map_syntax_opt(expr.comments.as_ref()),
        }
    }

    fn map_type_(&mut self, t: &'ast types::Type<Loc, Loc>) -> types::Type<Loc, Loc> {
        let t = ast_visitor::map_type_default(self, t);
        match t.deref() {
            TypeInner::Number { loc, comments } => types::Type::new(TypeInner::String {
                loc: loc.dupe(),
                comments: comments.dupe(),
            }),
            TypeInner::NumberLiteral { loc, .. } => types::Type::new(TypeInner::NumberLiteral {
                loc: loc.dupe(),
                literal: ast::NumberLiteral {
                    value: 4.0,
                    raw: FlowSmolStr::from("4.0"),
                    comments: None,
                },
            }),
            _ => t,
        }
    }

    fn map_jsx_element(
        &mut self,
        loc: &'ast Loc,
        elem: &'ast ast::jsx::Element<Loc, Loc>,
    ) -> ast::jsx::Element<Loc, Loc> {
        let opening_element = self.map_jsx_opening_element(&elem.opening_element);
        let closing_element = if opening_element.self_closing {
            None
        } else if elem.opening_element.self_closing {
            Some(ast::jsx::Closing {
                loc: opening_element.loc.dupe(),
                name: opening_element.name.clone(),
            })
        } else {
            elem.closing_element
                .as_ref()
                .map(|c| self.map_jsx_closing_element(c))
        };
        let children = self.map_jsx_children(&elem.children.0, &elem.children.1);
        let comments = self.map_syntax_opt(elem.comments.as_ref());
        let _ = loc;
        ast::jsx::Element {
            opening_element,
            closing_element,
            children: (elem.children.0.dupe(), children),
            comments,
        }
    }

    fn map_jsx_opening_element(
        &mut self,
        elem: &'ast ast::jsx::Opening<Loc, Loc>,
    ) -> ast::jsx::Opening<Loc, Loc> {
        let mut elem = ast_visitor::map_jsx_opening_element_default(self, elem);
        match &elem.name {
            ast::jsx::Name::Identifier(id) if id.name.as_str() == "selfClosing" => {
                elem.self_closing = true
            }
            ast::jsx::Name::Identifier(id) if id.name.as_str() == "notSelfClosing" => {
                elem.self_closing = false
            }
            _ => {}
        }
        elem
    }

    fn map_jsx_identifier(
        &mut self,
        id: &'ast ast::jsx::Identifier<Loc, Loc>,
    ) -> ast::jsx::Identifier<Loc, Loc> {
        if let Some(name) = rename_name(&id.name) {
            ast::jsx::Identifier {
                loc: id.loc.dupe(),
                name,
                comments: id.comments.dupe(),
            }
        } else {
            ast_visitor::map_jsx_identifier_default(self, id)
        }
    }

    fn map_jsx_child(
        &mut self,
        child: &'ast ast::jsx::Child<Loc, Loc>,
    ) -> ast::jsx::Child<Loc, Loc> {
        match child {
            ast::jsx::Child::Text { loc, inner } if inner.value == "rename" => {
                ast::jsx::Child::Text {
                    loc: loc.dupe(),
                    inner: ast::jsx::Text {
                        value: FlowSmolStr::from("gotRenamed"),
                        raw: FlowSmolStr::from("gotRenamed"),
                    },
                }
            }
            _ => ast_visitor::map_jsx_child_default(self, child),
        }
    }

    fn map_variance(&mut self, variance: &'ast ast::Variance<Loc>) -> ast::Variance<Loc> {
        if variance.kind == ast::VarianceKind::Minus {
            ast::Variance {
                loc: variance.loc.dupe(),
                kind: ast::VarianceKind::Plus,
                comments: variance.comments.dupe(),
            }
        } else {
            ast_visitor::map_variance_default(self, variance)
        }
    }

    fn map_call_type_args(
        &mut self,
        targs: &'ast ast::expression::CallTypeArgs<Loc, Loc>,
    ) -> ast::expression::CallTypeArgs<Loc, Loc> {
        let arguments = targs
            .arguments
            .iter()
            .map(|arg| match arg {
                ast::expression::CallTypeArg::Explicit(t) => {
                    ast::expression::CallTypeArg::Explicit(self.map_type_(t))
                }
                ast::expression::CallTypeArg::Implicit(implicit) => {
                    ast::expression::CallTypeArg::Explicit(any_type(implicit.loc.dupe()))
                }
            })
            .collect::<Vec<_>>();
        ast::expression::CallTypeArgs {
            loc: targs.loc.dupe(),
            arguments: Arc::from(arguments),
            comments: self.map_syntax_opt(targs.comments.as_ref()),
        }
    }

    fn map_update_expression(
        &mut self,
        loc: &'ast Loc,
        expr: &'ast ast::expression::Update<Loc, Loc>,
    ) -> ast::expression::Update<Loc, Loc> {
        let mut expr = ast_visitor::map_update_expression_default(self, loc, expr);
        if expr.operator != ast::expression::UpdateOperator::Increment {
            expr.operator = ast::expression::UpdateOperator::Increment;
        }
        expr
    }

    fn map_enum_defaulted_member(
        &mut self,
        member: &'ast statement::enum_declaration::DefaultedMember<Loc>,
    ) -> statement::enum_declaration::DefaultedMember<Loc> {
        match &member.id {
            statement::enum_declaration::MemberName::Identifier(id) if id.name.as_str() == "On" => {
                statement::enum_declaration::DefaultedMember {
                    loc: member.loc.dupe(),
                    id: statement::enum_declaration::MemberName::Identifier(ast::Identifier::new(
                        ast::IdentifierInner {
                            loc: id.loc.dupe(),
                            name: FlowSmolStr::from("Enabled"),
                            comments: id.comments.dupe(),
                        },
                    )),
                }
            }
            _ => ast_visitor::map_enum_defaulted_member_default(self, member),
        }
    }

    fn map_enum_string_member(
        &mut self,
        member: &'ast statement::enum_declaration::InitializedMember<ast::StringLiteral<Loc>, Loc>,
    ) -> statement::enum_declaration::InitializedMember<ast::StringLiteral<Loc>, Loc> {
        let mut member = ast_visitor::map_enum_string_member_default(self, member);
        if member.init.1.value.as_str() == "on" {
            member.init.1.value = FlowSmolStr::from("enabled");
            member.init.1.raw = FlowSmolStr::from("\"enabled\"");
        }
        member
    }

    fn map_enum_number_member(
        &mut self,
        member: &'ast statement::enum_declaration::InitializedMember<ast::NumberLiteral<Loc>, Loc>,
    ) -> statement::enum_declaration::InitializedMember<ast::NumberLiteral<Loc>, Loc> {
        match &member.init.1 {
            lit if lit.value == 1.0 && lit.comments.is_none() => {
                statement::enum_declaration::InitializedMember {
                    loc: member.loc.dupe(),
                    id: self.map_enum_member_name(&member.id),
                    init: (
                        member.init.0.dupe(),
                        ast_builder::number_literal(
                            Some(comments_with(
                                Vec::new(),
                                vec![ast_builder::comments::line(None, None, " a comment")],
                            )),
                            1.0,
                            member.init.1.raw.as_str(),
                        ),
                    ),
                }
            }
            _ => ast_visitor::map_enum_number_member_default(self, member),
        }
    }

    fn map_match_pattern(
        &mut self,
        pattern: &'ast ast::match_pattern::MatchPattern<Loc, Loc>,
    ) -> ast::match_pattern::MatchPattern<Loc, Loc> {
        match pattern {
            ast::match_pattern::MatchPattern::NullPattern { loc, .. } => {
                ast::match_pattern::MatchPattern::IdentifierPattern {
                    loc: loc.dupe(),
                    inner: Box::new(ident(loc.dupe(), "UpdatedPattern")),
                }
            }
            _ => ast_visitor::map_match_pattern_default(self, pattern),
        }
    }

    fn map_match_object_pattern_property(
        &mut self,
        prop: &'ast ast::match_pattern::object_pattern::Property<Loc, Loc>,
    ) -> ast::match_pattern::object_pattern::Property<Loc, Loc> {
        match prop {
            ast::match_pattern::object_pattern::Property::InvalidShorthand { loc, identifier }
                if identifier.name.as_str() == "changeProp" =>
            {
                ast::match_pattern::object_pattern::Property::Valid {
                    loc: loc.dupe(),
                    property: ast::match_pattern::object_pattern::PropertyStruct {
                        key: ast::match_pattern::object_pattern::Key::Identifier(ident(
                            loc.dupe(),
                            "UpdatedProp",
                        )),
                        pattern: ast::match_pattern::MatchPattern::NullPattern {
                            loc: loc.dupe(),
                            inner: Box::new(None),
                        },
                        shorthand: false,
                        comments: None,
                    },
                }
            }
            _ => ast_visitor::map_match_object_pattern_property_default(self, prop),
        }
    }
}

struct LiteralMapper;

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for LiteralMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_expression(&mut self, expr: &'ast Expression<Loc, Loc>) -> Expression<Loc, Loc> {
        match expr.deref() {
            ExpressionInner::NullLiteral { loc, .. } => {
                Expression::new(ExpressionInner::StringLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::StringLiteral {
                        value: FlowSmolStr::from("wasNull"),
                        raw: FlowSmolStr::from("wasNull"),
                        comments: None,
                    }),
                })
            }
            _ => ast_visitor::map_expression_default(self, expr),
        }
    }

    fn map_string_literal(
        &mut self,
        lit: &'ast ast::StringLiteral<Loc>,
    ) -> ast::StringLiteral<Loc> {
        if lit.value.as_str() == "rename" {
            ast::StringLiteral {
                value: FlowSmolStr::from("gotRenamed"),
                raw: FlowSmolStr::from("gotRenamed"),
                comments: None,
            }
        } else {
            ast_visitor::map_string_literal_default(self, lit)
        }
    }

    fn map_boolean_literal(
        &mut self,
        lit: &'ast ast::BooleanLiteral<Loc>,
    ) -> ast::BooleanLiteral<Loc> {
        if !lit.value {
            ast::BooleanLiteral {
                value: true,
                comments: None,
            }
        } else {
            ast_visitor::map_boolean_literal_default(self, lit)
        }
    }

    fn map_number_literal(
        &mut self,
        lit: &'ast ast::NumberLiteral<Loc>,
    ) -> ast::NumberLiteral<Loc> {
        if lit.value == 4.0 {
            ast::NumberLiteral {
                value: 5.0,
                raw: FlowSmolStr::from("5"),
                comments: None,
            }
        } else {
            ast_visitor::map_number_literal_default(self, lit)
        }
    }
}

struct InsertVarianceMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertVarianceMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        let mut useless = UselessMapper;
        let program = useless.map_program(program);
        ast_visitor::map_program_default(self, &program)
    }

    fn map_type_param(
        &mut self,
        kind: &TypeParamsContext,
        tparam: &'ast types::TypeParam<Loc, Loc>,
    ) -> types::TypeParam<Loc, Loc> {
        let mut tparam = ast_visitor::map_type_param_default(self, kind, tparam);
        if tparam.variance.is_none() {
            tparam.variance = Some(ast::Variance {
                loc: tparam.loc.dupe(),
                kind: ast::VarianceKind::Plus,
                comments: None,
            });
        }
        tparam
    }
}

struct DeleteVarianceMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for DeleteVarianceMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_variance_opt(
        &mut self,
        opt: Option<&'ast ast::Variance<Loc>>,
    ) -> Option<ast::Variance<Loc>> {
        match opt {
            Some(v) if v.kind == ast::VarianceKind::Minus => None,
            _ => ast_visitor::map_variance_opt_default(self, opt),
        }
    }
}

struct InsertEndMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertEndMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        let mut stmts = stmts.iter().cloned().collect::<Vec<_>>();
        let stmt = stmts.last().unwrap().clone();
        stmts.push(stmt);
        Arc::from(stmts)
    }
}

struct InsertBeginMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertBeginMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        let mut out = Vec::new();
        out.push(stmts.last().unwrap().clone());
        out.extend(stmts.iter().cloned());
        Arc::from(out)
    }
}

struct InsertDupMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertDupMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        let mut out = Vec::new();
        for stmt in stmts.iter() {
            out.push(stmt.clone());
            out.push(stmt.clone());
        }
        Arc::from(out)
    }
}

struct FirstLastDupMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for FirstLastDupMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        let mut out = Vec::new();
        out.push(stmts.first().unwrap().clone());
        out.extend(stmts.iter().cloned());
        out.push(stmts.last().unwrap().clone());
        Arc::from(out)
    }
}

fn import_statement(loc: Loc, source: &str) -> Statement<Loc, Loc> {
    Statement::new(StatementInner::ImportDeclaration {
        loc: loc.dupe(),
        inner: Arc::new(statement::ImportDeclaration {
            import_kind: statement::ImportKind::ImportValue,
            source: (
                loc.dupe(),
                ast::StringLiteral {
                    value: FlowSmolStr::from(source),
                    raw: FlowSmolStr::from(format!("\"{}\"", source)),
                    comments: None,
                },
            ),
            default: None,
            specifiers: Some(
                statement::import_declaration::Specifier::ImportNamedSpecifiers(vec![
                    statement::import_declaration::NamedSpecifier {
                        kind: None,
                        kind_loc: None,
                        local: None,
                        remote: ident(loc.dupe(), source),
                        remote_name_def_loc: None,
                    },
                ]),
            ),
            attributes: None,
            comments: None,
        }),
    })
}

fn cjs_require_statement(loc: Loc, source: &str, callee: &str) -> Statement<Loc, Loc> {
    let string_expr = Expression::new(ExpressionInner::StringLiteral {
        loc: loc.dupe(),
        inner: Arc::new(ast::StringLiteral {
            value: FlowSmolStr::from(source),
            raw: FlowSmolStr::from(format!("\"{}\"", source)),
            comments: None,
        }),
    });
    let call = Expression::new(ExpressionInner::Call {
        loc: loc.dupe(),
        inner: Arc::new(ast::expression::Call {
            callee: Expression::new(ExpressionInner::Identifier {
                loc: loc.dupe(),
                inner: ident(loc.dupe(), callee),
            }),
            targs: None,
            arguments: ast::expression::ArgList {
                loc: loc.dupe(),
                arguments: Arc::from(vec![ast::expression::ExpressionOrSpread::Expression(
                    string_expr,
                )]),
                comments: None,
            },
            comments: None,
        }),
    });
    Statement::new(StatementInner::Expression {
        loc: loc.dupe(),
        inner: Arc::new(statement::Expression {
            expression: call,
            directive: None,
            comments: None,
        }),
    })
}

struct InsertImportMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertImportMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        if stmts.is_empty() {
            return ast_visitor::map_statement_list_default(self, stmts);
        }
        let mut useless = UselessMapper;
        let stmts = useless.map_statement_list(stmts);
        let loc = stmts[0].loc().dupe();
        let mut out = vec![import_statement(loc, "baz")];
        out.extend(stmts.iter().cloned());
        Arc::from(out)
    }
}

struct InsertSecondImportMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertSecondImportMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        if stmts.is_empty() {
            return ast_visitor::map_statement_list_default(self, stmts);
        }
        let mut useless = UselessMapper;
        let stmts = useless.map_statement_list(stmts);
        let loc = stmts[0].loc().dupe();
        let mut out = vec![stmts[0].clone(), import_statement(loc, "baz")];
        out.extend(stmts.iter().skip(1).cloned());
        Arc::from(out)
    }
}

struct InsertSecondCjsImportMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertSecondCjsImportMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        if stmts.is_empty() {
            return ast_visitor::map_statement_list_default(self, stmts);
        }
        let mut useless = UselessMapper;
        let stmts = useless.map_statement_list(stmts);
        let loc = stmts[0].loc().dupe();
        let mut out = vec![
            stmts[0].clone(),
            cjs_require_statement(loc, "baz", "require"),
        ];
        out.extend(stmts.iter().skip(1).cloned());
        Arc::from(out)
    }
}

struct AddBodyMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for AddBodyMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        if stmts.is_empty() {
            return ast_visitor::map_statement_list_default(self, stmts);
        }
        let mut useless = UselessMapper;
        let mut stmts = useless
            .map_statement_list(stmts)
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        let loc = stmts.last().unwrap().loc().dupe();
        stmts.push(cjs_require_statement(loc, "baz", "foo"));
        Arc::from(stmts)
    }
}

struct DeleteMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for DeleteMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        Arc::from(stmts.iter().skip(1).cloned().collect::<Vec<_>>())
    }
}

struct DeleteEndMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for DeleteEndMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_statement_list(
        &mut self,
        stmts: &'ast Arc<[Statement<Loc, Loc>]>,
    ) -> Arc<[Statement<Loc, Loc>]> {
        Arc::from(
            stmts
                .iter()
                .take(stmts.len().saturating_sub(1))
                .cloned()
                .collect::<Vec<_>>(),
        )
    }
}

struct DeleteAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for DeleteAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_pattern(
        &mut self,
        kind: Option<ast::VariableKind>,
        pattern: &'ast ast::pattern::Pattern<Loc, Loc>,
    ) -> ast::pattern::Pattern<Loc, Loc> {
        let pattern = ast_visitor::map_pattern_default(self, kind, pattern);
        match pattern {
            ast::pattern::Pattern::Identifier { loc, inner } => ast::pattern::Pattern::Identifier {
                loc,
                inner: Arc::new(ast::pattern::Identifier {
                    name: inner.name.clone(),
                    annot: types::AnnotationOrHint::Missing(Loc::none()),
                    optional: inner.optional,
                }),
            },
            _ => pattern,
        }
    }
    fn map_type_annotation_hint(
        &mut self,
        hint: &'ast types::AnnotationOrHint<Loc, Loc>,
    ) -> types::AnnotationOrHint<Loc, Loc> {
        match ast_visitor::map_type_annotation_hint_default(self, hint) {
            types::AnnotationOrHint::Available(annot) => {
                types::AnnotationOrHint::Missing(annot.loc)
            }
            types::AnnotationOrHint::Missing(loc) => types::AnnotationOrHint::Missing(loc),
        }
    }
    fn map_function_return_annotation(
        &mut self,
        return_: &'ast function::ReturnAnnot<Loc, Loc>,
    ) -> function::ReturnAnnot<Loc, Loc> {
        match ast_visitor::map_function_return_annotation_default(self, return_) {
            function::ReturnAnnot::Available(annot) => function::ReturnAnnot::Missing(annot.loc),
            other => other,
        }
    }
}

struct InsertAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_pattern(
        &mut self,
        kind: Option<ast::VariableKind>,
        pattern: &'ast ast::pattern::Pattern<Loc, Loc>,
    ) -> ast::pattern::Pattern<Loc, Loc> {
        let pattern = ast_visitor::map_pattern_default(self, kind, pattern);
        match pattern {
            ast::pattern::Pattern::Identifier { loc, inner } => ast::pattern::Pattern::Identifier {
                loc: loc.dupe(),
                inner: Arc::new(ast::pattern::Identifier {
                    name: inner.name.clone(),
                    annot: types::AnnotationOrHint::Available(number_annotation(loc)),
                    optional: inner.optional,
                }),
            },
            _ => pattern,
        }
    }
    fn map_type_annotation_hint(
        &mut self,
        hint: &'ast types::AnnotationOrHint<Loc, Loc>,
    ) -> types::AnnotationOrHint<Loc, Loc> {
        match ast_visitor::map_type_annotation_hint_default(self, hint) {
            types::AnnotationOrHint::Available(annot) => types::AnnotationOrHint::Available(annot),
            types::AnnotationOrHint::Missing(loc) => {
                types::AnnotationOrHint::Available(number_annotation(loc))
            }
        }
    }
    fn map_function_return_annotation(
        &mut self,
        return_: &'ast function::ReturnAnnot<Loc, Loc>,
    ) -> function::ReturnAnnot<Loc, Loc> {
        match ast_visitor::map_function_return_annotation_default(self, return_) {
            function::ReturnAnnot::Missing(loc) => {
                function::ReturnAnnot::Available(number_annotation(loc))
            }
            other => other,
        }
    }
}

struct InsertFunctionAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertFunctionAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_type_annotation_hint(
        &mut self,
        hint: &'ast types::AnnotationOrHint<Loc, Loc>,
    ) -> types::AnnotationOrHint<Loc, Loc> {
        match ast_visitor::map_type_annotation_hint_default(self, hint) {
            types::AnnotationOrHint::Available(annot) => types::AnnotationOrHint::Available(annot),
            types::AnnotationOrHint::Missing(loc) => {
                types::AnnotationOrHint::Available(annotation(loc.dupe(), function_type(loc)))
            }
        }
    }
    fn map_function_return_annotation(
        &mut self,
        return_: &'ast function::ReturnAnnot<Loc, Loc>,
    ) -> function::ReturnAnnot<Loc, Loc> {
        match ast_visitor::map_function_return_annotation_default(self, return_) {
            function::ReturnAnnot::Missing(loc) => {
                function::ReturnAnnot::Available(annotation(loc.dupe(), function_type(loc)))
            }
            other => other,
        }
    }
}

struct InsertImportAndAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertImportAndAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_type_annotation_hint(
        &mut self,
        hint: &'ast types::AnnotationOrHint<Loc, Loc>,
    ) -> types::AnnotationOrHint<Loc, Loc> {
        InsertFunctionAnnotMapper.map_type_annotation_hint(hint)
    }

    fn map_function_return_annotation(
        &mut self,
        return_: &'ast function::ReturnAnnot<Loc, Loc>,
    ) -> function::ReturnAnnot<Loc, Loc> {
        InsertFunctionAnnotMapper.map_function_return_annotation(return_)
    }

    fn map_program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        let mut program = ast_visitor::map_program_default(self, program);
        let import = |num: i32| {
            let imp = format!("new_import{}", num);
            Statement::new(StatementInner::ImportDeclaration {
                loc: Loc::none(),
                inner: Arc::new(statement::ImportDeclaration {
                    import_kind: statement::ImportKind::ImportType,
                    source: (
                        Loc::none(),
                        ast::StringLiteral {
                            value: FlowSmolStr::from(imp.as_str()),
                            raw: FlowSmolStr::from(imp),
                            comments: None,
                        },
                    ),
                    default: None,
                    specifiers: Some(
                        statement::import_declaration::Specifier::ImportNamedSpecifiers(vec![
                            statement::import_declaration::NamedSpecifier {
                                kind: None,
                                kind_loc: None,
                                local: Some(ident(Loc::none(), "here")),
                                remote: ident(Loc::none(), "there"),
                                remote_name_def_loc: None,
                            },
                        ]),
                    ),
                    attributes: None,
                    comments: None,
                }),
            })
        };
        let mut statements = Vec::new();
        statements.push(program.statements[0].clone());
        statements.push(import(1));
        statements.push(import(2));
        statements.extend(program.statements.iter().skip(1).cloned());
        program.statements = Arc::from(statements);
        program
    }
}

struct PropAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for PropAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_class_property(
        &mut self,
        prop: &'ast ast::class::Property<Loc, Loc>,
    ) -> ast::class::Property<Loc, Loc> {
        let mut prop = ast_visitor::map_class_property_default(self, prop);
        if matches!(prop.annot, types::AnnotationOrHint::Missing(_)) {
            prop.annot = types::AnnotationOrHint::Available(number_annotation(Loc::none()));
        }
        prop
    }
}

struct FuncReturnAnnotMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for FuncReturnAnnotMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_function_(
        &mut self,
        _loc: &'ast Loc,
        expr: &'ast function::Function<Loc, Loc>,
    ) -> function::Function<Loc, Loc> {
        let mut expr = expr.clone();
        if matches!(expr.return_, function::ReturnAnnot::Missing(_)) {
            expr.return_ = function::ReturnAnnot::Available(number_annotation(Loc::none()));
        }
        expr
    }
}

struct InsertTypecastMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertTypecastMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_expression(&mut self, expression: &'ast Expression<Loc, Loc>) -> Expression<Loc, Loc> {
        let loc = expression.loc().dupe();
        Expression::new(ExpressionInner::TypeCast {
            loc: loc.dupe(),
            inner: Arc::new(ast::expression::TypeCast {
                expression: expression.clone(),
                annot: annotation(loc.dupe(), any_type(loc)),
                comments: None,
            }),
        })
    }
}

struct InsertCallTypeArgsMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for InsertCallTypeArgsMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_call_type_args(
        &mut self,
        targs: &'ast ast::expression::CallTypeArgs<Loc, Loc>,
    ) -> ast::expression::CallTypeArgs<Loc, Loc> {
        let mut arguments = vec![ast::expression::CallTypeArg::Explicit(any_type(
            targs.loc.dupe(),
        ))];
        arguments.extend(targs.arguments.iter().cloned());
        ast::expression::CallTypeArgs {
            loc: targs.loc.dupe(),
            arguments: Arc::from(arguments),
            comments: targs.comments.dupe(),
        }
    }
}

struct AddCommentMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for AddCommentMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> ast::Identifier<Loc, Loc> {
        ast::Identifier::new(ast::IdentifierInner {
            loc: id.loc.dupe(),
            name: id.name.dupe(),
            comments: Some(comments_with(
                vec![ast_builder::comments::block(None, None, "hello")],
                vec![ast_builder::comments::block(None, None, "bye")],
            )),
        })
    }
}

struct TrueToFalseMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for TrueToFalseMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_boolean_literal(
        &mut self,
        lit: &'ast ast::BooleanLiteral<Loc>,
    ) -> ast::BooleanLiteral<Loc> {
        if lit.value {
            ast::BooleanLiteral {
                value: false,
                comments: None,
            }
        } else {
            ast_visitor::map_boolean_literal_default(self, lit)
        }
    }
    fn map_type_annotation(
        &mut self,
        annot: &'ast types::Annotation<Loc, Loc>,
    ) -> types::Annotation<Loc, Loc> {
        match annot.annotation.deref() {
            TypeInner::BooleanLiteral { loc, literal } if literal.value => types::Annotation {
                loc: annot.loc.dupe(),
                annotation: types::Type::new(TypeInner::BooleanLiteral {
                    loc: loc.dupe(),
                    literal: ast::BooleanLiteral {
                        value: false,
                        comments: literal.comments.dupe(),
                    },
                }),
            },
            _ => annot.clone(),
        }
    }
}

struct RemoveAnnotationRestMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for RemoveAnnotationRestMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_type_(&mut self, t: &'ast types::Type<Loc, Loc>) -> types::Type<Loc, Loc> {
        let t = ast_visitor::map_type_default(self, t);
        match t.deref() {
            TypeInner::Intersection { loc, inner } => types::Type::new(TypeInner::Intersection {
                loc: loc.dupe(),
                inner: Arc::new(types::Intersection {
                    types: (inner.types.0.clone(), inner.types.1.clone(), Vec::new()),
                    comments: inner.comments.dupe(),
                }),
            }),
            TypeInner::Union { loc, inner } => types::Type::new(TypeInner::Union {
                loc: loc.dupe(),
                inner: Arc::new(types::Union {
                    types: (inner.types.0.clone(), inner.types.1.clone(), Vec::new()),
                    comments: inner.comments.dupe(),
                }),
            }),
            _ => t,
        }
    }
}

struct DoubleSequenceMapper;
impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for DoubleSequenceMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }
    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
    fn map_sequence(
        &mut self,
        _loc: &'ast Loc,
        expr: &'ast ast::expression::Sequence<Loc, Loc>,
    ) -> ast::expression::Sequence<Loc, Loc> {
        let mut expressions = expr.expressions.iter().cloned().collect::<Vec<_>>();
        expressions.extend(expr.expressions.iter().cloned());
        ast::expression::Sequence {
            expressions: Arc::from(expressions),
            comments: expr.comments.dupe(),
        }
    }
}

fn map_with_kind(kind: MapperKind, ast: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
    match kind {
        MapperKind::Useless => UselessMapper.map_program(ast),
        MapperKind::Literal => LiteralMapper.map_program(ast),
        MapperKind::InsertVariance => InsertVarianceMapper.map_program(ast),
        MapperKind::DeleteVariance => DeleteVarianceMapper.map_program(ast),
        MapperKind::InsertEnd => InsertEndMapper.map_program(ast),
        MapperKind::InsertBegin => InsertBeginMapper.map_program(ast),
        MapperKind::InsertDup => InsertDupMapper.map_program(ast),
        MapperKind::FirstLastDup => FirstLastDupMapper.map_program(ast),
        MapperKind::InsertImport => InsertImportMapper.map_program(ast),
        MapperKind::InsertSecondImport => InsertSecondImportMapper.map_program(ast),
        MapperKind::InsertSecondCjsImport => InsertSecondCjsImportMapper.map_program(ast),
        MapperKind::AddBody => AddBodyMapper.map_program(ast),
        MapperKind::Delete => DeleteMapper.map_program(ast),
        MapperKind::DeleteEnd => DeleteEndMapper.map_program(ast),
        MapperKind::DeleteAnnot => DeleteAnnotMapper.map_program(ast),
        MapperKind::InsertAnnot => InsertAnnotMapper.map_program(ast),
        MapperKind::InsertFunctionAnnot => InsertFunctionAnnotMapper.map_program(ast),
        MapperKind::InsertImportAndAnnot => InsertImportAndAnnotMapper.map_program(ast),
        MapperKind::PropAnnot => PropAnnotMapper.map_program(ast),
        MapperKind::FuncReturnAnnot => FuncReturnAnnotMapper.map_program(ast),
        MapperKind::InsertTypecast => InsertTypecastMapper.map_program(ast),
        MapperKind::InsertCallTypeArgs => InsertCallTypeArgsMapper.map_program(ast),
        MapperKind::AddComment => AddCommentMapper.map_program(ast),
        MapperKind::TrueToFalse => TrueToFalseMapper.map_program(ast),
        MapperKind::RemoveAnnotationRest => RemoveAnnotationRestMapper.map_program(ast),
        MapperKind::DoubleSequence => DoubleSequenceMapper.map_program(ast),
    }
}

fn edits_of_source(source: &str, mapper: MapperKind) -> Vec<((i32, i32), String)> {
    let (ast, _errors) =
        parse_program_without_file(true, None, Some(PERMISSIVE_PARSE_OPTIONS), Ok(source));
    let new_ast = map_with_kind(mapper, &ast);
    let edits = program(&ast, &new_ast);
    ast_diff_printer::edits_of_changes(&js_layout_generator::default_opts(), &edits)
        .into_iter()
        .map(|(loc, text)| ((loc.start.column, loc.end.column), text))
        .collect()
}

fn apply_edits(source: &str, edits: &[((i32, i32), String)]) -> String {
    let mut acc = source.to_string();
    for ((begin, end), text) in edits.iter().rev() {
        let begin = *begin as usize;
        let end = *end as usize;
        acc = format!("{}{}{}", &acc[..begin], text, &acc[end..]);
    }
    acc
}

fn assert_edits_equal(
    name: &str,
    edits: &[Edit],
    source: &str,
    expected: &str,
    mapper: MapperKind,
) {
    let actual = edits_of_source(source, mapper);
    let expected_edits = edits
        .iter()
        .map(|((start, end), text)| ((*start, *end), (*text).to_string()))
        .collect::<Vec<_>>();
    assert_eq!(expected_edits, actual, "Edits for {name}");
    assert_eq!(
        expected,
        apply_edits(source, &actual),
        "Edits applied for {name}"
    );
}

#[test]
fn algo_diff_empty() {
    let source = "";
    let (ast_empty, _errors) =
        parse_program_without_file(true, None, Some(PERMISSIVE_PARSE_OPTIONS), Ok(source));
    let (ast_var, _errors) =
        parse_program_without_file(true, None, Some(PERMISSIVE_PARSE_OPTIONS), Ok("var x = 6;"));
    let edits = program(&ast_empty, &ast_var);
    let edits = ast_diff_printer::edits_of_changes(&js_layout_generator::default_opts(), &edits)
        .into_iter()
        .map(|(loc, text)| ((loc.start.column, loc.end.column), text))
        .collect::<Vec<_>>();
    assert_eq!(vec![((0, 0), "var x = 6;".to_string())], edits);
    assert_eq!("var x = 6;", apply_edits(source, &edits));
}

#[test]
fn literal_number() {
    assert_edits_equal(
        "literal_number",
        &[((0, 1), "5")],
        "4",
        "5",
        MapperKind::Literal,
    );
}

#[test]
fn literal_string() {
    assert_edits_equal(
        "literal_string",
        &[((0, 8), "\"gotRenamed\"")],
        "\"rename\"",
        "\"gotRenamed\"",
        MapperKind::Literal,
    );
}

#[test]
fn literal_bool() {
    assert_edits_equal(
        "literal_bool",
        &[((0, 5), "true")],
        "false",
        "true",
        MapperKind::Literal,
    );
}

#[test]
fn literal_null() {
    assert_edits_equal(
        "literal_null",
        &[((0, 4), "\"wasNull\"")],
        "null",
        "\"wasNull\"",
        MapperKind::Literal,
    );
}

#[test]
fn string_literal_type() {
    assert_edits_equal(
        "string_literal_type",
        &[((6, 16), "\"GotRenamedSL\"")],
        "(foo: \"RenameSL\")",
        "(foo: \"GotRenamedSL\")",
        MapperKind::Useless,
    );
}

#[test]
fn simple() {
    assert_edits_equal(
        "simple",
        &[((26, 27), "5"), ((30, 35), "6 - 5")],
        "function foo() { (5 - 3); 4; (6 + 4); }",
        "function foo() { (5 - 3); 5; (6 - 5); }",
        MapperKind::Useless,
    );
}

#[test]
fn class() {
    assert_edits_equal(
        "class",
        &[((20, 21), "5")],
        "class Foo { bar() { 4; } }",
        "class Foo { bar() { 5; } }",
        MapperKind::Useless,
    );
}

#[test]
fn class2() {
    assert_edits_equal(
        "class2",
        &[((18, 19), "5")],
        "class Foo { bar = 4; }",
        "class Foo { bar = 5; }",
        MapperKind::Useless,
    );
}

#[test]
fn class3() {
    assert_edits_equal(
        "class3",
        &[((12, 24), "#gotRenamed = 5;")],
        "class Foo { #rename = 4; }",
        "class Foo { #gotRenamed = 5; }",
        MapperKind::Useless,
    );
}

#[test]
fn class_prop_annot() {
    assert_edits_equal(
        "class_prop_annot",
        &[((11, 11), ": number")],
        "class A { f = (x: string) => x; }",
        "class A { f: number = (x: string) => x; }",
        MapperKind::PropAnnot,
    );
}

#[test]
fn class_extends() {
    assert_edits_equal(
        "class_extends",
        &[((16, 22), "gotRenamed"), ((23, 29), "gotRenamed")],
        "class A extends rename<rename, dontrename> { }",
        "class A extends gotRenamed<gotRenamed, dontrename> { }",
        MapperKind::Useless,
    );
}

#[test]
fn class_extends_integration() {
    assert_edits_equal(
        "class_extends_integration",
        &[((16, 22), "gotRenamed"), ((31, 32), "5")],
        "class A extends rename { bar = 4 }",
        "class A extends gotRenamed { bar = 5 }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_id() {
    assert_edits_equal(
        "interface_id",
        &[((10, 16), "GotRenamed")],
        "interface Rename { }",
        "interface GotRenamed { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_tparams() {
    assert_edits_equal(
        "interface_tparams",
        &[((14, 20), "GOT_RENAMED")],
        "interface Foo<RENAME> { }",
        "interface Foo<GOT_RENAMED> { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_extends_id() {
    assert_edits_equal(
        "interface_extends_id",
        &[((22, 28), "GotRenamed")],
        "interface Foo extends Rename { }",
        "interface Foo extends GotRenamed { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_extends_targ_simple() {
    assert_edits_equal(
        "interface_extends_targ_simple",
        &[((26, 32), "GOT_RENAMED")],
        "interface Foo extends Bar<RENAME> { }",
        "interface Foo extends Bar<GOT_RENAMED> { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_extends_targs() {
    assert_edits_equal(
        "interface_extends_targs",
        &[((26, 32), "GOT_RENAMED"), ((34, 40), "GOT_RENAMED")],
        "interface Foo extends Bar<RENAME, RENAME> { }",
        "interface Foo extends Bar<GOT_RENAMED, GOT_RENAMED> { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_combo() {
    assert_edits_equal(
        "interface_combo",
        &[
            ((10, 16), "GotRenamed"),
            ((25, 31), "GotRenamed"),
            ((32, 38), "GOT_RENAMED"),
        ],
        "interface Rename extends Rename<RENAME> { }",
        "interface GotRenamed extends GotRenamed<GOT_RENAMED> { }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_body_object_property_key() {
    assert_edits_equal(
        "interface_body_object_property_key",
        &[((16, 22), "gotRenamed")],
        "interface Foo { rename: string }",
        "interface Foo { gotRenamed: string }",
        MapperKind::Useless,
    );
}

#[test]
fn interface_body_object_property_value_init() {
    assert_edits_equal(
        "interface_body_object_property_value_init",
        &[((21, 27), "string")],
        "interface Foo { bar: number }",
        "interface Foo { bar: string }",
        MapperKind::Useless,
    );
}

#[test]
fn obj_prop() {
    assert_edits_equal(
        "obj_prop",
        &[((10, 16), "gotRenamed"), ((19, 20), "5")],
        "let x = { rename : 4 }",
        "let x = { gotRenamed : 5 }",
        MapperKind::Useless,
    );
}

#[test]
fn obj_prop2() {
    assert_edits_equal(
        "obj_prop2",
        &[((18, 24), "gotRenamed")],
        "let x = { bar() { rename; } }",
        "let x = { bar() { gotRenamed; } }",
        MapperKind::Useless,
    );
}

#[test]
fn obj_prop3() {
    assert_edits_equal(
        "obj_prop3",
        &[((10, 11), "5")],
        "let x = { 4 : 3 }",
        "let x = { 5 : 3 }",
        MapperKind::Useless,
    );
}

#[test]
fn obj_spread_prop() {
    assert_edits_equal(
        "obj_spread_prop",
        &[((13, 19), "gotRenamed"), ((25, 26), "5")],
        "let x = { ...rename, x : 4}",
        "let x = { ...gotRenamed, x : 5}",
        MapperKind::Useless,
    );
}

#[test]
fn precedence() {
    assert_edits_equal(
        "precedence",
        &[((4, 9), "(3 + 3)")],
        "5 - 3 * 3",
        "5 - (3 + 3)",
        MapperKind::Useless,
    );
}

#[test]
fn tuple() {
    assert_edits_equal(
        "tuple",
        &[((12, 18), "string"), ((20, 26), "string")],
        "type Foo = [number, number];",
        "type Foo = [string, string];",
        MapperKind::Useless,
    );
}

#[test]
fn identifier() {
    assert_edits_equal(
        "identifier",
        &[((4, 10), "gotRenamed")],
        "5 - rename",
        "5 - gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn interface_type() {
    assert_edits_equal(
        "interface_type",
        &[((23, 29), "gotRenamed")],
        "type Foo = interface { rename() : string }",
        "type Foo = interface { gotRenamed() : string }",
        MapperKind::Useless,
    );
}

#[test]
fn new() {
    assert_edits_equal(
        "new",
        &[((4, 10), "gotRenamed")],
        "new rename()",
        "new gotRenamed()",
        MapperKind::Useless,
    );
}

#[test]
fn typeof_type() {
    assert_edits_equal(
        "typeof_type",
        &[((18, 24), "gotRenamed")],
        "type Foo = typeof rename",
        "type Foo = typeof gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn new_type_param() {
    assert_edits_equal(
        "new_type_param",
        &[((8, 14), "GOT_RENAMED")],
        "new foo<RENAME>()",
        "new foo<GOT_RENAMED>()",
        MapperKind::Useless,
    );
}

#[test]
fn new_type_param_2() {
    assert_edits_equal(
        "new_type_param_2",
        &[
            ((4, 10), "gotRenamed"),
            ((11, 17), "GOT_RENAMED"),
            ((19, 25), "gotRenamed"),
        ],
        "new rename<RENAME>(rename)",
        "new gotRenamed<GOT_RENAMED>(gotRenamed)",
        MapperKind::Useless,
    );
}

#[test]
fn new_type_param_3() {
    assert_edits_equal(
        "new_type_param_3",
        &[((13, 19), "gotRenamed")],
        "new foo<FOO>(rename)",
        "new foo<FOO>(gotRenamed)",
        MapperKind::Useless,
    );
}

#[test]
fn new_type_param_multiple() {
    assert_edits_equal(
        "new_type_param_multiple",
        &[((8, 14), "GOT_RENAMED"), ((16, 22), "GOT_RENAMED")],
        "new foo<RENAME, RENAME>()",
        "new foo<GOT_RENAMED, GOT_RENAMED>()",
        MapperKind::Useless,
    );
}

#[test]
fn new_type_param_insert() {
    assert_edits_equal(
        "new_type_param_insert",
        &[((0, 11), "new foo<any>()")],
        "new foo<>()",
        "new foo<any>()",
        MapperKind::InsertCallTypeArgs,
    );
}

#[test]
fn new_type_param_implicit() {
    assert_edits_equal(
        "new_type_param_implicit",
        &[((0, 12), "new foo<any>()")],
        "new foo<_>()",
        "new foo<any>()",
        MapperKind::Useless,
    );
}

#[test]
fn member() {
    assert_edits_equal(
        "member",
        &[((0, 6), "gotRenamed")],
        "rename.a",
        "gotRenamed.a",
        MapperKind::Useless,
    );
}

#[test]
fn member_identifier() {
    assert_edits_equal(
        "member_identifier",
        &[((0, 6), "gotRenamed"), ((7, 13), "gotRenamed")],
        "rename.rename",
        "gotRenamed.gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn member_expression() {
    assert_edits_equal(
        "member_expression",
        &[((4, 5), "5")],
        "obj[4]",
        "obj[5]",
        MapperKind::Useless,
    );
}

#[test]
fn unary_same_op() {
    assert_edits_equal(
        "unary_same_op",
        &[((1, 7), "gotRenamed")],
        "-rename",
        "-gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn unary_diff_op() {
    assert_edits_equal(
        "unary_diff_op",
        &[((0, 7), "-gotRenamed")],
        "+rename",
        "-gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn block() {
    assert_edits_equal(
        "block",
        &[((5, 6), "5"), ((12, 18), "gotRenamed")],
        "{ 2; 4; 10; rename; }",
        "{ 2; 5; 10; gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn if_nochange() {
    assert_edits_equal(
        "if_nochange",
        &[],
        "if (true) { false; } else { true; }",
        "if (true) { false; } else { true; }",
        MapperKind::Useless,
    );
}

#[test]
fn if_noblock() {
    assert_edits_equal(
        "if_noblock",
        &[((4, 5), "5"), ((7, 13), "gotRenamed")],
        "if (4) rename;",
        "if (5) gotRenamed;",
        MapperKind::Useless,
    );
}

#[test]
fn if_partial() {
    assert_edits_equal(
        "if_partial",
        &[((4, 5), "5"), ((9, 15), "gotRenamed")],
        "if (4) { rename; }",
        "if (5) { gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn if_full() {
    assert_edits_equal(
        "if_full",
        &[((4, 5), "5"), ((9, 10), "5"), ((21, 27), "gotRenamed")],
        "if (4) { 4; } else { rename }",
        "if (5) { 5; } else { gotRenamed }",
        MapperKind::Useless,
    );
}

#[test]
fn conditional_nochange() {
    assert_edits_equal(
        "conditional_nochange",
        &[],
        "1 > 0 ? false : true",
        "1 > 0 ? false : true",
        MapperKind::Useless,
    );
}

#[test]
fn conditional_test() {
    assert_edits_equal(
        "conditional_test",
        &[((0, 6), "gotRenamed")],
        "rename ? false : true",
        "gotRenamed ? false : true",
        MapperKind::Useless,
    );
}

#[test]
fn conditional_consequent() {
    assert_edits_equal(
        "conditional_consequent",
        &[((8, 14), "gotRenamed")],
        "1 > 0 ? rename : true",
        "1 > 0 ? gotRenamed : true",
        MapperKind::Useless,
    );
}

#[test]
fn conditional_alternate() {
    assert_edits_equal(
        "conditional_alternate",
        &[((16, 22), "gotRenamed")],
        "1 > 0 ? false : rename",
        "1 > 0 ? false : gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn conditional_cons_and_alt() {
    assert_edits_equal(
        "conditional_cons_and_alt",
        &[((8, 9), "5"), ((12, 18), "gotRenamed")],
        "1 > 0 ? 4 : rename",
        "1 > 0 ? 5 : gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn with_nochange() {
    assert_edits_equal(
        "with_nochange",
        &[],
        "with (object) { foo = true; }",
        "with (object) { foo = true; }",
        MapperKind::Useless,
    );
}

#[test]
fn with_object() {
    assert_edits_equal(
        "with_object",
        &[((6, 12), "gotRenamed")],
        "with (rename) { foo = true; };",
        "with (gotRenamed) { foo = true; };",
        MapperKind::Useless,
    );
}

#[test]
fn with_body() {
    assert_edits_equal(
        "with_body",
        &[((15, 21), "gotRenamed")],
        "with (objct) { rename; };",
        "with (objct) { gotRenamed; };",
        MapperKind::Useless,
    );
}

#[test]
fn function_expression() {
    assert_edits_equal(
        "function_expression",
        &[((14, 15), "5")],
        "(function() { 4; })",
        "(function() { 5; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_id() {
    assert_edits_equal(
        "function_id",
        &[((10, 16), "gotRenamed")],
        "(function rename() { return; })",
        "(function gotRenamed() { return; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_rest() {
    assert_edits_equal(
        "function_rest",
        &[((13, 19), "gotRenamed")],
        "(function(...rename) { return; })",
        "(function(...gotRenamed) { return; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_param() {
    assert_edits_equal(
        "function_param",
        &[((10, 16), "gotRenamed")],
        "(function(rename, ...dontRename) { return; })",
        "(function(gotRenamed, ...dontRename) { return; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_params() {
    assert_edits_equal(
        "function_params",
        &[((10, 16), "gotRenamed"), ((30, 36), "gotRenamed")],
        "(function(rename, dontRename, rename) { return; })",
        "(function(gotRenamed, dontRename, gotRenamed) { return; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_type_params() {
    assert_edits_equal(
        "function_type_params",
        &[((10, 16), "GOT_RENAMED")],
        "(function<RENAME>() { return; })",
        "(function<GOT_RENAMED>() { return; })",
        MapperKind::Useless,
    );
}

#[test]
fn function_combo() {
    assert_edits_equal(
        "function_combo",
        &[
            ((10, 16), "gotRenamed"),
            ((17, 23), "GOT_RENAMED"),
            ((25, 31), "gotRenamed"),
            ((34, 40), "GotRenamed"),
            ((50, 51), "5"),
        ],
        "(function rename<RENAME>(rename): Rename { return 4; })",
        "(function gotRenamed<GOT_RENAMED>(gotRenamed): GotRenamed { return 5; })",
        MapperKind::Useless,
    );
}

#[test]
fn arrow_function() {
    assert_edits_equal(
        "arrow_function",
        &[((17, 18), "5")],
        "let bar = (x) => 4;",
        "let bar = (x) => 5;",
        MapperKind::Useless,
    );
}

#[test]
fn component_id() {
    assert_edits_equal(
        "component_id",
        &[((10, 16), "gotRenamed")],
        "component rename() { return; }",
        "component gotRenamed() { return; }",
        MapperKind::Useless,
    );
}

#[test]
fn component_rest() {
    assert_edits_equal(
        "component_rest",
        &[((15, 21), "gotRenamed")],
        "component C(...rename) { return; }",
        "component C(...gotRenamed) { return; }",
        MapperKind::Useless,
    );
}

#[test]
fn component_param() {
    assert_edits_equal(
        "component_param",
        &[((12, 18), "gotRenamed")],
        "component C(rename, ...dontRename) { return; }",
        "component C(gotRenamed, ...dontRename) { return; }",
        MapperKind::Useless,
    );
}

#[test]
fn component_params() {
    assert_edits_equal(
        "component_params",
        &[((12, 18), "gotRenamed"), ((48, 54), "gotRenamed")],
        "component C(rename: string, dontRename: string, rename as rename1: string) { return; }",
        "component C(gotRenamed: string, dontRename: string, gotRenamed as rename1: string) { return; }",
        MapperKind::Useless,
    );
}

#[test]
fn component_type_params() {
    assert_edits_equal(
        "component_type_params",
        &[((12, 18), "GOT_RENAMED")],
        "component C<RENAME>() { return; }",
        "component C<GOT_RENAMED>() { return; }",
        MapperKind::Useless,
    );
}

#[test]
fn component_combo() {
    assert_edits_equal(
        "component_combo",
        &[
            ((10, 16), "gotRenamed"),
            ((17, 23), "GOT_RENAMED"),
            ((25, 31), "gotRenamed"),
            ((41, 47), "GotRenamed"),
            ((57, 58), "5"),
        ],
        "component rename<RENAME>(rename) renders Rename { return 4; }",
        "component gotRenamed<GOT_RENAMED>(gotRenamed) renders GotRenamed { return 5; }",
        MapperKind::Useless,
    );
}

#[test]
fn call() {
    assert_edits_equal(
        "call",
        &[((0, 6), "gotRenamed")],
        "rename()",
        "gotRenamed()",
        MapperKind::Useless,
    );
}

#[test]
fn call_type_param() {
    assert_edits_equal(
        "call_type_param",
        &[((0, 6), "gotRenamed"), ((7, 13), "GOT_RENAMED")],
        "rename<RENAME>()",
        "gotRenamed<GOT_RENAMED>()",
        MapperKind::Useless,
    );
}

#[test]
fn variable_declaration_kind() {
    assert_edits_equal(
        "variable_declaration_kind",
        &[((0, 10), "const x = 5;")],
        "var x = 5;",
        "const x = 5;",
        MapperKind::Useless,
    );
}

#[test]
fn variable_declaration_expression() {
    assert_edits_equal(
        "variable_declaration_expression",
        &[((8, 9), "5")],
        "let x = 4;",
        "let x = 5;",
        MapperKind::Useless,
    );
}

#[test]
fn variable_declaration_kind_expression() {
    assert_edits_equal(
        "variable_declaration_kind_expression",
        &[((0, 10), "const x = 5;")],
        "var x = 4;",
        "const x = 5;",
        MapperKind::Useless,
    );
}

#[test]
fn r#for() {
    assert_edits_equal(
        "for",
        &[((16, 22), "gotRenamed")],
        "for (i = 7; i < rename; i++) {}",
        "for (i = 7; i < gotRenamed; i++) {}",
        MapperKind::Useless,
    );
}

#[test]
fn for_init() {
    assert_edits_equal(
        "for_init",
        &[((13, 14), "5")],
        "for (let i = 4; i < 10; i++) {}",
        "for (let i = 5; i < 10; i++) {}",
        MapperKind::Useless,
    );
}

#[test]
fn for_body() {
    assert_edits_equal(
        "for_body",
        &[((28, 34), "gotRenamed")],
        "for (i = 7; i < top; i++) { rename; }",
        "for (i = 7; i < top; i++) { gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn for_in_left() {
    assert_edits_equal(
        "for_in_left",
        &[((0, 31), "for (const x in xs) {\n  continue;\n}")],
        "for (var x in xs) { continue; }",
        "for (const x in xs) {\n  continue;\n}",
        MapperKind::Useless,
    );
}

#[test]
fn for_in_right() {
    assert_edits_equal(
        "for_in_right",
        &[((14, 20), "gotRenamed")],
        "for (let x in rename) { continue; }",
        "for (let x in gotRenamed) { continue; }",
        MapperKind::Useless,
    );
}

#[test]
fn for_in_body() {
    assert_edits_equal(
        "for_in_body",
        &[((20, 26), "gotRenamed")],
        "for (let x in xs) { rename; }",
        "for (let x in xs) { gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn while_test() {
    assert_edits_equal(
        "while_test",
        &[((7, 13), "gotRenamed")],
        "while (rename) { break; };",
        "while (gotRenamed) { break; };",
        MapperKind::Useless,
    );
}

#[test]
fn while_body() {
    assert_edits_equal(
        "while_body",
        &[((15, 21), "gotRenamed")],
        "while (true) { rename; };",
        "while (true) { gotRenamed; };",
        MapperKind::Useless,
    );
}

#[test]
fn for_of_left() {
    assert_edits_equal(
        "for_of_left",
        &[((0, 31), "for (const x of xs) {\n  continue;\n}")],
        "for (var x of xs) { continue; }",
        "for (const x of xs) {\n  continue;\n}",
        MapperKind::Useless,
    );
}

#[test]
fn for_of_right() {
    assert_edits_equal(
        "for_of_right",
        &[((14, 20), "gotRenamed")],
        "for (let x of rename) { continue; }",
        "for (let x of gotRenamed) { continue; }",
        MapperKind::Useless,
    );
}

#[test]
fn for_of_body() {
    assert_edits_equal(
        "for_of_body",
        &[((20, 26), "gotRenamed")],
        "for (let x of xs) { rename; }",
        "for (let x of xs) { gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn do_while_body() {
    assert_edits_equal(
        "do_while_body",
        &[((5, 11), "gotRenamed")],
        "do { rename; } while (true);",
        "do { gotRenamed; } while (true);",
        MapperKind::Useless,
    );
}

#[test]
fn do_while_condition() {
    assert_edits_equal(
        "do_while_condition",
        &[((24, 30), "gotRenamed")],
        "do { continue; } while (rename);",
        "do { continue; } while (gotRenamed);",
        MapperKind::Useless,
    );
}

#[test]
fn try_stmt_body() {
    assert_edits_equal(
        "try_stmt_body",
        &[((6, 12), "gotRenamed")],
        "try { rename; } catch(e) { other; };",
        "try { gotRenamed; } catch(e) { other; };",
        MapperKind::Useless,
    );
}

#[test]
fn try_stmt_catch() {
    assert_edits_equal(
        "try_stmt_catch",
        &[((21, 27), "gotRenamed")],
        "try { thing; } catch(rename) { other; };",
        "try { thing; } catch(gotRenamed) { other; };",
        MapperKind::Useless,
    );
}

#[test]
fn try_stmt_handler() {
    assert_edits_equal(
        "try_stmt_handler",
        &[((26, 32), "gotRenamed")],
        "try { thing; } catch(e) { rename; };",
        "try { thing; } catch(e) { gotRenamed; };",
        MapperKind::Useless,
    );
}

#[test]
fn try_stmt_finalizer() {
    assert_edits_equal(
        "try_stmt_finalizer",
        &[((25, 31), "gotRenamed")],
        "try { thing; } finally { rename; };",
        "try { thing; } finally { gotRenamed; };",
        MapperKind::Useless,
    );
}

#[test]
fn labeled_label() {
    assert_edits_equal(
        "labeled_label",
        &[((0, 6), "gotRenamed")],
        "rename: while (true) { }",
        "gotRenamed: while (true) { }",
        MapperKind::Useless,
    );
}

#[test]
fn labeled_body() {
    assert_edits_equal(
        "labeled_body",
        &[((12, 18), "gotRenamed")],
        "foo: while (rename) { }",
        "foo: while (gotRenamed) { }",
        MapperKind::Useless,
    );
}

#[test]
fn switch_discriminant() {
    assert_edits_equal(
        "switch_discriminant",
        &[((8, 14), "gotRenamed")],
        "switch (rename) { case true: break; }",
        "switch (gotRenamed) { case true: break; }",
        MapperKind::Useless,
    );
}

#[test]
fn switch_case_test() {
    assert_edits_equal(
        "switch_case_test",
        &[((21, 27), "gotRenamed")],
        "switch (true) { case rename: break; }",
        "switch (true) { case gotRenamed: break; }",
        MapperKind::Useless,
    );
}

#[test]
fn switch_case_consequent() {
    assert_edits_equal(
        "switch_case_consequent",
        &[((27, 33), "gotRenamed")],
        "switch (true) { case true: rename; }",
        "switch (true) { case true: gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn algo_diff_end_insert() {
    assert_edits_equal(
        "algo_diff_end_insert",
        &[((21, 21), "var y = 6;")],
        "var x = 5; var y = 6;",
        "var x = 5; var y = 6;var y = 6;",
        MapperKind::InsertEnd,
    );
}

#[test]
fn algo_diff_delete() {
    assert_edits_equal(
        "algo_diff_delete",
        &[((0, 10), "")],
        "var x = 5; var y = 6; var z = 7;",
        " var y = 6; var z = 7;",
        MapperKind::Delete,
    );
}

#[test]
fn algo_diff_begin_insert() {
    assert_edits_equal(
        "algo_diff_begin_insert",
        &[((0, 0), "var y = 6;")],
        "var x = 5; var y = 6;",
        "var y = 6;var x = 5; var y = 6;",
        MapperKind::InsertBegin,
    );
}

#[test]
fn algo_diff_middle_insert() {
    assert_edits_equal(
        "algo_diff_middle_insert",
        &[((10, 10), "var x = 5;"), ((21, 21), "var y = 6;")],
        "var x = 5; var y = 6;",
        "var x = 5;var x = 5; var y = 6;var y = 6;",
        MapperKind::InsertDup,
    );
}

#[test]
fn unnamed_class_expression() {
    assert_edits_equal(
        "unnamed_class_expression",
        &[((20, 26), "gotRenamed")],
        "(class { method() { rename; } })",
        "(class { method() { gotRenamed; } })",
        MapperKind::Useless,
    );
}

#[test]
fn named_class_expression() {
    assert_edits_equal(
        "named_class_expression",
        &[((24, 30), "gotRenamed")],
        "(class Foo { method() { rename; } })",
        "(class Foo { method() { gotRenamed; } })",
        MapperKind::Useless,
    );
}

#[test]
fn return_statement_with_expression() {
    assert_edits_equal(
        "return_statement_with_expression",
        &[((24, 30), "gotRenamed")],
        "function foo() { return rename; }",
        "function foo() { return gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_delete() {
    assert_edits_equal(
        "type_annotation_delete",
        &[((6, 14), "")],
        "let x : number = 3;",
        "let x  = 3;",
        MapperKind::DeleteAnnot,
    );
}

#[test]
fn type_annotation_insert() {
    assert_edits_equal(
        "type_annotation_insert",
        &[((5, 5), ": number")],
        "let x = 3;",
        "let x: number = 3;",
        MapperKind::InsertAnnot,
    );
}

#[test]
fn type_annotation_replace() {
    assert_edits_equal(
        "type_annotation_replace",
        &[((8, 14), "string")],
        "let x : number = 3;",
        "let x : string = 3;",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_type_arg() {
    assert_edits_equal(
        "type_annotation_rename_type_arg",
        &[((10, 16), "gotRenamed")],
        "(foo: bar<rename>);",
        "(foo: bar<gotRenamed>);",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_type() {
    assert_edits_equal(
        "type_annotation_rename_type",
        &[((6, 12), "gotRenamed")],
        "(foo: rename<bar>);",
        "(foo: gotRenamed<bar>);",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_type_and_typearg() {
    assert_edits_equal(
        "type_annotation_rename_type_and_typearg",
        &[((6, 12), "gotRenamed"), ((13, 19), "gotRenamed")],
        "(foo: rename<rename>);",
        "(foo: gotRenamed<gotRenamed>);",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_qualified_type() {
    assert_edits_equal(
        "type_annotation_rename_qualified_type",
        &[((10, 16), "gotRenamed")],
        "(foo: Foo.rename<Bar>);",
        "(foo: Foo.gotRenamed<Bar>);",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_qualified_typearg() {
    assert_edits_equal(
        "type_annotation_rename_qualified_typearg",
        &[((14, 20), "gotRenamed")],
        "(foo: Foo.Bar<rename>);",
        "(foo: Foo.Bar<gotRenamed>);",
        MapperKind::Useless,
    );
}

#[test]
fn type_annotation_rename_qualified_type_and_typearg() {
    assert_edits_equal(
        "type_annotation_rename_qualified_type_and_typearg",
        &[((10, 16), "gotRenamed"), ((17, 23), "gotRenamed")],
        "(foo: Foo.rename<rename>);",
        "(foo: Foo.gotRenamed<gotRenamed>);",
        MapperKind::Useless,
    );
}

#[test]
fn return_type_replace() {
    assert_edits_equal(
        "return_type_replace",
        &[((17, 23), "string")],
        "function foo() : number { return 1; }",
        "function foo() : string { return 1; }",
        MapperKind::Useless,
    );
}

#[test]
fn return_type_delete() {
    assert_edits_equal(
        "return_type_delete",
        &[((15, 23), "")],
        "function foo() : number { return 1; }",
        "function foo()  { return 1; }",
        MapperKind::DeleteAnnot,
    );
}

#[test]
fn return_type_insert() {
    assert_edits_equal(
        "return_type_insert",
        &[((14, 14), ": number")],
        "function foo() { return 1; }",
        "function foo(): number { return 1; }",
        MapperKind::InsertAnnot,
    );
}

#[test]
fn comments() {
    assert_edits_equal(
        "comments",
        &[((40, 41), "5"), ((44, 49), "6 - 5")],
        "function foo() { /* comment */ (5 - 3); 4; (6 + 4); /* comment */}",
        "function foo() { /* comment */ (5 - 3); 5; (6 - 5); /* comment */}",
        MapperKind::Useless,
    );
}

#[test]
fn fn_default_export() {
    assert_edits_equal(
        "fn_default_export",
        &[((40, 46), "gotRenamed")],
        "export default function foo() { let x = rename; }",
        "export default function foo() { let x = gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn fn_export_named() {
    assert_edits_equal(
        "fn_export_named",
        &[((32, 38), "gotRenamed")],
        "export function foo() { let x = rename; }",
        "export function foo() { let x = gotRenamed; }",
        MapperKind::Useless,
    );
}

#[test]
fn assignment_left() {
    assert_edits_equal(
        "assignment_left",
        &[((0, 6), "gotRenamed")],
        "rename = 6;",
        "gotRenamed = 6;",
        MapperKind::Useless,
    );
}

#[test]
fn assignment_right() {
    assert_edits_equal(
        "assignment_right",
        &[((4, 10), "gotRenamed")],
        "x = rename;",
        "x = gotRenamed;",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_identifier() {
    assert_edits_equal(
        "pattern_identifier",
        &[((4, 10), "gotRenamed")],
        "let rename = 0",
        "let gotRenamed = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_array() {
    assert_edits_equal(
        "pattern_array",
        &[((5, 11), "gotRenamed"), ((12, 18), "gotRenamed")],
        "let [rename,rename] = [0]",
        "let [gotRenamed,gotRenamed] = [0]",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_array_nested() {
    assert_edits_equal(
        "pattern_array_nested",
        &[((7, 13), "gotRenamed")],
        "let [[[rename]]] = 0",
        "let [[[gotRenamed]]] = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_array_rest() {
    assert_edits_equal(
        "pattern_array_rest",
        &[((12, 18), "gotRenamed")],
        "let [a,b,...rename] = 0",
        "let [a,b,...gotRenamed] = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_array_annot() {
    assert_edits_equal(
        "pattern_array_annot",
        &[((15, 21), "gotRenamed")],
        "let [foo,bar]: rename = [0]",
        "let [foo,bar]: gotRenamed = [0]",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_object_longhand() {
    assert_edits_equal(
        "pattern_object_longhand",
        &[((5, 11), "gotRenamed"), ((13, 19), "gotRenamed")],
        "let {rename: rename} = 0",
        "let {gotRenamed: gotRenamed} = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_object_rest() {
    assert_edits_equal(
        "pattern_object_rest",
        &[((12, 18), "gotRenamed")],
        "let {a,b,...rename} = 0",
        "let {a,b,...gotRenamed} = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_object_annot() {
    assert_edits_equal(
        "pattern_object_annot",
        &[((16, 22), "gotRenamed")],
        "let {foo: bar}: rename = 0",
        "let {foo: bar}: gotRenamed = 0",
        MapperKind::Useless,
    );
}

#[test]
fn pattern_assignment() {
    assert_edits_equal(
        "pattern_assignment",
        &[((7, 13), "gotRenamed")],
        "let [a=rename] = 0",
        "let [a=gotRenamed] = 0",
        MapperKind::Useless,
    );
}

#[test]
fn type_cast_expr() {
    assert_edits_equal(
        "type_cast_expr",
        &[((1, 7), "gotRenamed")],
        "(rename: string)",
        "(gotRenamed: string)",
        MapperKind::Useless,
    );
}

#[test]
fn type_cast_type() {
    assert_edits_equal(
        "type_cast_type",
        &[((13, 19), "string")],
        "(dontrename: number)",
        "(dontrename: string)",
        MapperKind::Useless,
    );
}

#[test]
fn type_cast_assign() {
    assert_edits_equal(
        "type_cast_assign",
        &[((10, 16), "string"), ((32, 38), "string")],
        "const x : number = (dontrename: number)",
        "const x : string = (dontrename: string)",
        MapperKind::Useless,
    );
}

#[test]
fn type_cast_add() {
    assert_edits_equal(
        "type_cast_add",
        &[((19, 19), "("), ((49, 49), ": any)")],
        "const dontrename = call( /* preserve spaces */  )",
        "const dontrename = (call( /* preserve spaces */  ): any)",
        MapperKind::InsertTypecast,
    );
}

#[test]
fn class_type_param_instantiation() {
    assert_edits_equal(
        "class_type_param_instantiation",
        &[((29, 35), "gotRenamed")],
        "class A extends B<{}> { m(): rename {} }",
        "class A extends B<{}> { m(): gotRenamed {} }",
        MapperKind::Useless,
    );
}

#[test]
fn logical_operator_left() {
    assert_edits_equal(
        "logical_operator_left",
        &[((0, 6), "gotRenamed")],
        "rename && b",
        "gotRenamed && b",
        MapperKind::Useless,
    );
}

#[test]
fn logical_operator_right() {
    assert_edits_equal(
        "logical_operator_right",
        &[((5, 11), "gotRenamed")],
        "a || rename",
        "a || gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn logical_operator_changed() {
    assert_edits_equal(
        "logical_operator_changed",
        &[((0, 6), "a || b")],
        "a ?? b",
        "a || b",
        MapperKind::Useless,
    );
}

#[test]
fn insert_import_split() {
    assert_edits_equal(
        "insert_import_split",
        &[
            ((0, 0), "import { baz } from \"baz\";"),
            ((5, 10), "(2 - 2)"),
        ],
        "5 - (2 + 2)",
        "import { baz } from \"baz\";5 - ((2 - 2))",
        MapperKind::InsertImport,
    );
}

#[test]
fn insert_import_existing_split() {
    assert_edits_equal(
        "insert_import_existing_split",
        &[
            ((0, 0), "import { baz } from \"baz\";"),
            ((10, 15), "(2 - 2)"),
        ],
        "foo; 5 - (2 + 2)",
        "import { baz } from \"baz\";foo; 5 - ((2 - 2))",
        MapperKind::InsertImport,
    );
}

#[test]
fn insert_import_second_split() {
    assert_edits_equal(
        "insert_import_second_split",
        &[
            ((24, 24), "import { baz } from \"baz\";"),
            ((30, 35), "(2 - 2)"),
        ],
        "import bing from 'bing'; 5 - (2 + 2)",
        "import bing from 'bing';import { baz } from \"baz\"; 5 - ((2 - 2))",
        MapperKind::InsertSecondImport,
    );
}

#[test]
fn existing_cjs_import_split() {
    assert_edits_equal(
        "existing_cjs_import_split",
        &[
            ((26, 26), "import { baz } from \"baz\";"),
            ((32, 37), "(2 - 2)"),
        ],
        "const x = require('bing'); 5 - (2 + 2)",
        "const x = require('bing');import { baz } from \"baz\"; 5 - ((2 - 2))",
        MapperKind::InsertSecondImport,
    );
}

#[test]
fn insert_cjs_import_split() {
    assert_edits_equal(
        "insert_cjs_import_split",
        &[((14, 14), "require(\"baz\");"), ((20, 25), "(2 - 2)")],
        "import 'bing'; 5 - (2 + 2)",
        "import 'bing';require(\"baz\"); 5 - ((2 - 2))",
        MapperKind::InsertSecondCjsImport,
    );
}

#[test]
fn pathological_import_split() {
    assert_edits_equal(
        "pathological_import_split",
        &[((0, 0), "5 - (2 + 2);")],
        "import 'baz'; import 'bing'; 5 - (2 + 2);",
        "5 - (2 + 2);import 'baz'; import 'bing'; 5 - (2 + 2);",
        MapperKind::InsertBegin,
    );
}

#[test]
fn remove_import_split() {
    assert_edits_equal(
        "remove_import_split",
        &[((0, 13), "")],
        "import 'baz';5 - (2 + 2);",
        "5 - (2 + 2);",
        MapperKind::Delete,
    );
}

#[test]
fn add_body_split() {
    assert_edits_equal(
        "add_body_split",
        &[((13, 13), "foo(\"baz\");")],
        "import 'baz';",
        "import 'baz';foo(\"baz\");",
        MapperKind::AddBody,
    );
}

#[test]
fn add_to_body_split() {
    assert_edits_equal(
        "add_to_body_split",
        &[((23, 23), "foo(\"baz\");")],
        "import 'baz'; bar(qux);",
        "import 'baz'; bar(qux);foo(\"baz\");",
        MapperKind::AddBody,
    );
}

#[test]
fn remove_body_split() {
    assert_edits_equal(
        "remove_body_split",
        &[((13, 25), "")],
        "import 'baz';5 - (2 + 2);",
        "import 'baz';",
        MapperKind::DeleteEnd,
    );
}

#[test]
fn spread_simple() {
    assert_edits_equal(
        "spread_simple",
        &[((4, 10), "gotRenamed")],
        "[...rename]",
        "[...gotRenamed]",
        MapperKind::Useless,
    );
}

#[test]
fn tagged_template_tag() {
    assert_edits_equal(
        "tagged_template_tag",
        &[((0, 6), "gotRenamed")],
        "rename`dontRename`",
        "gotRenamed`dontRename`",
        MapperKind::Useless,
    );
}

#[test]
fn tagged_template_literal() {
    assert_edits_equal(
        "tagged_template_literal",
        &[((10, 18), "`gotRenamed`")],
        "dontRename`rename`",
        "dontRename`gotRenamed`",
        MapperKind::Useless,
    );
}

#[test]
fn template_literal_simple() {
    assert_edits_equal(
        "template_literal_simple",
        &[((0, 8), "`gotRenamed`")],
        "`rename`",
        "`gotRenamed`",
        MapperKind::Useless,
    );
}

#[test]
fn template_literal_expr() {
    assert_edits_equal(
        "template_literal_expr",
        &[((7, 13), "gotRenamed")],
        "`foo ${rename} bar`",
        "`foo ${gotRenamed} bar`",
        MapperKind::Useless,
    );
}

#[test]
fn template_literal_expr_multiple() {
    assert_edits_equal(
        "template_literal_expr_multiple",
        &[((14, 20), "gotRenamed"), ((35, 41), "gotRenamed")],
        "let test = `${rename} ${foo} bar ${rename}`",
        "let test = `${gotRenamed} ${foo} bar ${gotRenamed}`",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_simple() {
    assert_edits_equal(
        "jsx_element_self_closing_simple",
        &[((1, 7), "gotRenamed")],
        "<rename />",
        "<gotRenamed />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_namespaced_namespace() {
    assert_edits_equal(
        "jsx_element_self_closing_namespaced_namespace",
        &[((1, 7), "GOT_RENAMED")],
        "<RENAME:dontRename />",
        "<GOT_RENAMED:dontRename />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_namespaced_name() {
    assert_edits_equal(
        "jsx_element_self_closing_namespaced_name",
        &[((13, 19), "gotRenamed")],
        "<DONT_RENAME:rename />",
        "<DONT_RENAME:gotRenamed />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_member_expr_object() {
    assert_edits_equal(
        "jsx_element_self_closing_member_expr_object",
        &[((1, 7), "GotRenamed")],
        "<Rename.dontRename />",
        "<GotRenamed.dontRename />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_member_expr_name() {
    assert_edits_equal(
        "jsx_element_self_closing_member_expr_name",
        &[((12, 18), "gotRenamed")],
        "<DontRename.rename />",
        "<DontRename.gotRenamed />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_self_closing_member_expr_nested_object() {
    assert_edits_equal(
        "jsx_element_self_closing_member_expr_nested_object",
        &[((1, 7), "GotRenamed"), ((19, 25), "GotRenamed")],
        "<Rename.DontRename.Rename.dontRename />",
        "<GotRenamed.DontRename.GotRenamed.dontRename />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_simple() {
    assert_edits_equal(
        "jsx_element_simple",
        &[((1, 7), "gotRenamed"), ((10, 16), "gotRenamed")],
        "<rename></rename>",
        "<gotRenamed></gotRenamed>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_member_expr_nested() {
    assert_edits_equal(
        "jsx_element_member_expr_nested",
        &[
            ((1, 7), "GotRenamed"),
            ((19, 25), "gotRenamed"),
            ((28, 34), "GotRenamed"),
            ((46, 52), "gotRenamed"),
        ],
        "<Rename.DontRename.rename></Rename.DontRename.rename>",
        "<GotRenamed.DontRename.gotRenamed></GotRenamed.DontRename.gotRenamed>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_to_self_closing() {
    assert_edits_equal(
        "jsx_element_to_self_closing",
        &[((0, 27), "<selfClosing />")],
        "<selfClosing></selfClosing>",
        "<selfClosing />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_from_self_closing() {
    assert_edits_equal(
        "jsx_element_from_self_closing",
        &[((0, 18), "<notSelfClosing></notSelfClosing>")],
        "<notSelfClosing />",
        "<notSelfClosing></notSelfClosing>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_name() {
    assert_edits_equal(
        "jsx_element_attribute_name",
        &[((11, 17), "gotRenamed")],
        "<Component rename={1} />",
        "<Component gotRenamed={1} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_value_expression_literal() {
    assert_edits_equal(
        "jsx_element_attribute_value_expression_literal",
        &[((21, 22), "5")],
        "<Component someProp={4} />",
        "<Component someProp={5} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_value_expression_binop() {
    assert_edits_equal(
        "jsx_element_attribute_value_expression_binop",
        &[((21, 26), "5 - 5")],
        "<Component someProp={4 + 4} />",
        "<Component someProp={5 - 5} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_name_and_value() {
    assert_edits_equal(
        "jsx_element_attribute_name_and_value",
        &[((11, 17), "gotRenamed"), ((19, 20), "5")],
        "<Component rename={4} />",
        "<Component gotRenamed={5} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_list_name() {
    assert_edits_equal(
        "jsx_element_attribute_list_name",
        &[((26, 32), "gotRenamed")],
        "<Component dontRename={1} rename={2} />",
        "<Component dontRename={1} gotRenamed={2} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_list_expression_literal() {
    assert_edits_equal(
        "jsx_element_attribute_list_expression_literal",
        &[((21, 22), "5"), ((37, 38), "5")],
        "<Component someProp={4} anotherProp={4} />",
        "<Component someProp={5} anotherProp={5} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_spread_attribute() {
    assert_edits_equal(
        "jsx_element_spread_attribute",
        &[((15, 21), "gotRenamed")],
        "<Component {...rename} />",
        "<Component {...gotRenamed} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_spread_attribute_list_mixed() {
    assert_edits_equal(
        "jsx_element_spread_attribute_list_mixed",
        &[
            ((15, 21), "gotRenamed"),
            ((23, 29), "gotRenamed"),
            ((31, 32), "5"),
        ],
        "<Component {...rename} rename={4}/>",
        "<Component {...gotRenamed} gotRenamed={5}/>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_attribute_list_name_and_value() {
    assert_edits_equal(
        "jsx_element_attribute_list_name_and_value",
        &[((11, 17), "gotRenamed"), ((34, 35), "5")],
        "<Component rename={1} dontRename={4} />",
        "<Component gotRenamed={1} dontRename={5} />",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_child_element() {
    assert_edits_equal(
        "jsx_element_child_element",
        &[((6, 12), "gotRenamed")],
        "<div><rename /></div>",
        "<div><gotRenamed /></div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_child_fragment() {
    assert_edits_equal(
        "jsx_element_child_fragment",
        &[((7, 13), "gotRenamed")],
        "<div><>rename</></div>",
        "<div><>gotRenamed</></div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_child_expr() {
    assert_edits_equal(
        "jsx_element_child_expr",
        &[((6, 12), "gotRenamed")],
        "<div>{rename}</div>",
        "<div>{gotRenamed}</div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_child_spread() {
    assert_edits_equal(
        "jsx_element_child_spread",
        &[((9, 15), "gotRenamed")],
        "<div>{...rename}</div>",
        "<div>{...gotRenamed}</div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_child_text() {
    assert_edits_equal(
        "jsx_element_child_text",
        &[((5, 11), "gotRenamed")],
        "<div>rename</div>",
        "<div>gotRenamed</div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_children() {
    assert_edits_equal(
        "jsx_element_children",
        &[((6, 12), "gotRenamed"), ((15, 21), "gotRenamed")],
        "<div>{rename} <rename /></div>",
        "<div>{gotRenamed} <gotRenamed /></div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_element_children_nested() {
    assert_edits_equal(
        "jsx_element_children_nested",
        &[
            ((6, 12), "gotRenamed"),
            ((16, 22), "gotRenamed"),
            ((30, 36), "gotRenamed"),
        ],
        "<div><rename><><rename /></></rename></div>",
        "<div><gotRenamed><><gotRenamed /></></gotRenamed></div>",
        MapperKind::Useless,
    );
}

#[test]
fn jsx_fragment_expr() {
    assert_edits_equal(
        "jsx_fragment_expr",
        &[((3, 9), "gotRenamed")],
        "<>{rename}</>",
        "<>{gotRenamed}</>",
        MapperKind::Useless,
    );
}

#[test]
fn declare_type_alias_id() {
    assert_edits_equal(
        "declare_type_alias_id",
        &[((13, 19), "GotRenamed")],
        "declare type Rename = string",
        "declare type GotRenamed = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_id() {
    assert_edits_equal(
        "type_alias_id",
        &[((5, 11), "GotRenamed")],
        "type Rename = string",
        "type GotRenamed = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_intersection_left() {
    assert_edits_equal(
        "type_alias_intersection_left",
        &[((11, 17), "string")],
        "type foo = number & bar",
        "type foo = string & bar",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_intersection_right() {
    assert_edits_equal(
        "type_alias_intersection_right",
        &[((17, 23), "string")],
        "type foo = bar & number",
        "type foo = bar & string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_intersection_rest() {
    assert_edits_equal(
        "type_alias_intersection_rest",
        &[((23, 29), "string"), ((32, 38), "string")],
        "type foo = bar & baz & number & number",
        "type foo = bar & baz & string & string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_intersection_argument_mismatch() {
    assert_edits_equal(
        "type_alias_intersection_argument_mismatch",
        &[((11, 31), "bar & true")],
        "type foo = bar & true & boolean",
        "type foo = bar & true",
        MapperKind::RemoveAnnotationRest,
    );
}

#[test]
fn type_alias_nullable() {
    assert_edits_equal(
        "type_alias_nullable",
        &[((12, 18), "string")],
        "type foo = ?number",
        "type foo = ?string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_number_literal() {
    assert_edits_equal(
        "type_alias_number_literal",
        &[((11, 14), "4.0")],
        "type foo = 5.0",
        "type foo = 4.0",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_name() {
    assert_edits_equal(
        "type_alias_param_name",
        &[((11, 17), "GOT_RENAMED")],
        "type alias<RENAME> = string",
        "type alias<GOT_RENAMED> = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_bound() {
    assert_edits_equal(
        "type_alias_param_bound",
        &[((14, 20), "string")],
        "type alias<A: number> = string",
        "type alias<A: string> = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_variance() {
    assert_edits_equal(
        "type_alias_param_variance",
        &[((11, 12), "+")],
        "type alias<-A> = string",
        "type alias<+A> = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_insert_variance() {
    assert_edits_equal(
        "type_alias_param_insert_variance",
        &[((11, 12), "+A")],
        "type alias<A> = string",
        "type alias<+A> = string",
        MapperKind::InsertVariance,
    );
}

#[test]
fn type_alias_param_bound_insert_variance() {
    assert_edits_equal(
        "type_alias_param_bound_insert_variance",
        &[((11, 20), "+A: string")],
        "type alias<A: number> = string",
        "type alias<+A: string> = string",
        MapperKind::InsertVariance,
    );
}

#[test]
fn type_alias_param_delete_variance() {
    assert_edits_equal(
        "type_alias_param_delete_variance",
        &[((11, 12), "")],
        "type alias<-A> = string",
        "type alias<A> = string",
        MapperKind::DeleteVariance,
    );
}

#[test]
fn type_alias_param_default() {
    assert_edits_equal(
        "type_alias_param_default",
        &[((15, 21), "string")],
        "type alias<A = number> = string",
        "type alias<A = string> = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_combo() {
    assert_edits_equal(
        "type_alias_param_combo",
        &[
            ((11, 12), "+"),
            ((12, 18), "GOT_RENAMED"),
            ((20, 26), "string"),
            ((29, 35), "string"),
        ],
        "type alias<-RENAME: number = number> = string",
        "type alias<+GOT_RENAMED: string = string> = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_param_list() {
    assert_edits_equal(
        "type_alias_param_list",
        &[
            ((11, 12), "+"),
            ((12, 18), "GOT_RENAMED"),
            ((20, 26), "GOT_RENAMED"),
            ((28, 34), "string"),
        ],
        "type alias<-RENAME, RENAME: number> = string",
        "type alias<+GOT_RENAMED, GOT_RENAMED: string> = string",
        MapperKind::Useless,
    );
}

#[test]
fn declare_type_alias_right() {
    assert_edits_equal(
        "declare_type_alias_right",
        &[((21, 27), "string")],
        "declare type alias = number",
        "declare type alias = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right() {
    assert_edits_equal(
        "type_alias_right",
        &[((13, 19), "string")],
        "type alias = number",
        "type alias = string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_function_type_params() {
    assert_edits_equal(
        "type_alias_right_function_type_params",
        &[
            ((14, 20), "gotRenamed"),
            ((35, 41), "string"),
            ((46, 52), "gotRenamed"),
        ],
        "type alias = (rename: string, bar: number, ...rename: string) => string",
        "type alias = (gotRenamed: string, bar: string, ...gotRenamed: string) => string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_function_type_tparams() {
    assert_edits_equal(
        "type_alias_right_function_type_tparams",
        &[((14, 20), "GOT_RENAMED")],
        "type alias = <RENAME>(param: string) => string",
        "type alias = <GOT_RENAMED>(param: string) => string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_function_type_return() {
    assert_edits_equal(
        "type_alias_right_function_type_return",
        &[((23, 29), "string")],
        "type alias = string => number",
        "type alias = string => string",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_object_type() {
    assert_edits_equal(
        "type_alias_right_object_type",
        &[((15, 21), "gotRenamed")],
        "type alias = { rename: string }",
        "type alias = { gotRenamed: string }",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_object_property_value_get() {
    assert_edits_equal(
        "type_alias_right_object_property_value_get",
        &[((19, 25), "gotRenamed")],
        "type alias = { get rename(): void; }",
        "type alias = { get gotRenamed(): void; }",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_object_property_value_set() {
    assert_edits_equal(
        "type_alias_right_object_property_value_set",
        &[((30, 36), "string")],
        "type alias = { set foo(value: number): void; }",
        "type alias = { set foo(value: string): void; }",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_right_object_variance() {
    assert_edits_equal(
        "type_alias_right_object_variance",
        &[((15, 16), "+")],
        "type alias = { -foo: string }",
        "type alias = { +foo: string }",
        MapperKind::Useless,
    );
}

#[test]
fn opaque_type_id() {
    assert_edits_equal(
        "opaque_type_id",
        &[((12, 18), "GotRenamed")],
        "opaque type Rename = string",
        "opaque type GotRenamed = string",
        MapperKind::Useless,
    );
}

#[test]
fn opaque_type_param() {
    assert_edits_equal(
        "opaque_type_param",
        &[((16, 22), "GOT_RENAMED")],
        "opaque type foo<RENAME> = string",
        "opaque type foo<GOT_RENAMED> = string",
        MapperKind::Useless,
    );
}

#[test]
fn opaque_type_impl() {
    assert_edits_equal(
        "opaque_type_impl",
        &[((21, 27), "string")],
        "opaque type foo<A> = number",
        "opaque type foo<A> = string",
        MapperKind::Useless,
    );
}

#[test]
fn opaque_type_super() {
    assert_edits_equal(
        "opaque_type_super",
        &[((17, 23), "string")],
        "opaque type foo: number = string",
        "opaque type foo: string = string",
        MapperKind::Useless,
    );
}

#[test]
fn opaque_type_combo() {
    assert_edits_equal(
        "opaque_type_combo",
        &[
            ((12, 18), "GotRenamed"),
            ((19, 25), "GOT_RENAMED"),
            ((28, 34), "string"),
            ((37, 43), "string"),
        ],
        "opaque type Rename<RENAME>: number = number",
        "opaque type GotRenamed<GOT_RENAMED>: string = string",
        MapperKind::Useless,
    );
}

#[test]
fn call_insert() {
    assert_edits_equal(
        "call_insert",
        &[((24, 24), ": number")],
        "callFunction(class A { f = (x: string) => x; });",
        "callFunction(class A { f: number = (x: string) => x; });",
        MapperKind::PropAnnot,
    );
}

#[test]
fn new_insert() {
    assert_edits_equal(
        "new_insert",
        &[((23, 23), ": number")],
        "new MyClass(class A { f = (x: string) => x; });",
        "new MyClass(class A { f: number = (x: string) => x; });",
        MapperKind::PropAnnot,
    );
}

#[test]
fn insert_inside_array() {
    assert_edits_equal(
        "insert_inside_array",
        &[((25, 25), ": number")],
        "[{ render() { class A { f = (x: string) => x; } return new A() } }]",
        "[{ render() { class A { f: number = (x: string) => x; } return new A() } }]",
        MapperKind::PropAnnot,
    );
}

#[test]
fn update_same_op() {
    assert_edits_equal(
        "update_same_op",
        &[((2, 8), "gotRenamed")],
        "++rename",
        "++gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn update_diff_op() {
    assert_edits_equal(
        "update_diff_op",
        &[((0, 8), "++gotRenamed")],
        "--rename",
        "++gotRenamed",
        MapperKind::Useless,
    );
}

#[test]
fn update_arrow_function_add_return_annot() {
    assert_edits_equal(
        "update_arrow_function_add_return_annot",
        &[((10, 13), "(bla)"), ((13, 13), ": number")],
        "const x = bla => { return 0; };",
        "const x = (bla): number => { return 0; };",
        MapperKind::FuncReturnAnnot,
    );
}

#[test]
fn update_arrow_function_single_param() {
    assert_edits_equal(
        "update_arrow_function_single_param",
        &[
            ((7, 7), ": number"),
            ((10, 13), "(bla: number)"),
            ((13, 13), ": number"),
        ],
        "const x = bla => { return 0; };",
        "const x: number = (bla: number): number => { return 0; };",
        MapperKind::InsertAnnot,
    );
}

#[test]
fn update_arrow_function_function_return() {
    assert_edits_equal(
        "update_arrow_function_function_return",
        &[
            ((7, 7), ": (() => number)"),
            ((10, 13), "(bla: () => number)"),
            ((13, 13), ": (() => number)"),
        ],
        "const x = bla => { return 0; };",
        "const x: (() => number) = (bla: () => number): (() => number) => { return 0; };",
        MapperKind::InsertFunctionAnnot,
    );
}

#[test]
fn new_imports_after_directive_dont_reprint_the_file() {
    assert_edits_equal(
        "new_imports_after_directive_dont_reprint_the_file",
        &[
            (
                (13, 13),
                "import type { there as here } from \"new_import1\";\nimport type { there as here } from \"new_import2\";",
            ),
            ((20, 20), ": (() => number)"),
            ((23, 26), "(bla: () => number)"),
            ((26, 26), ": (() => number)"),
        ],
        "'use strict';const x = bla => { return 0; };",
        "'use strict';import type { there as here } from \"new_import1\";\nimport type { there as here } from \"new_import2\";const x: (() => number) = (bla: () => number): (() => number) => { return 0; };",
        MapperKind::InsertImportAndAnnot,
    );
}

#[test]
fn import_renamed_simple() {
    assert_edits_equal(
        "import_renamed_simple",
        &[((7, 13), "gotRenamed")],
        "import rename from \"foo\";",
        "import gotRenamed from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_renamed_simple_multiple1() {
    assert_edits_equal(
        "import_renamed_simple_multiple1",
        &[((7, 13), "gotRenamed")],
        "import rename, {bar} from \"foo\";",
        "import gotRenamed, {bar} from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_renamed_simple_multiple2() {
    assert_edits_equal(
        "import_renamed_simple_multiple2",
        &[((13, 19), "gotRenamed")],
        "import bar, {rename} from \"foo\";",
        "import bar, {gotRenamed} from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_renamed_simple1() {
    assert_edits_equal(
        "import_renamed_simple1",
        &[((8, 14), "gotRenamed")],
        "import {rename} from \"foo\";",
        "import {gotRenamed} from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_renamed_multiple() {
    assert_edits_equal(
        "import_renamed_multiple",
        &[((8, 14), "gotRenamed")],
        "import {rename, bar} from \"foo\";",
        "import {gotRenamed, bar} from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_renamed_whole_module() {
    assert_edits_equal(
        "import_renamed_whole_module",
        &[((12, 18), "gotRenamed")],
        "import * as rename from \"foo\";",
        "import * as gotRenamed from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_type1() {
    assert_edits_equal(
        "import_type1",
        &[((12, 18), "gotRenamed")],
        "import type rename from \"bar\";",
        "import type gotRenamed from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_type_and_fn2() {
    assert_edits_equal(
        "import_type_and_fn2",
        &[((7, 13), "gotRenamed")],
        "import rename, {type foo} from \"bar\";",
        "import gotRenamed, {type foo} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_multiple_names1() {
    assert_edits_equal(
        "import_multiple_names1",
        &[((7, 13), "gotRenamed")],
        "import rename, {myBar, myBaz} from \"bar\";",
        "import gotRenamed, {myBar, myBaz} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_multiple_names2() {
    assert_edits_equal(
        "import_multiple_names2",
        &[((15, 21), "gotRenamed")],
        "import myBar, {rename, myBaz} from \"bar\";",
        "import myBar, {gotRenamed, myBaz} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_type2() {
    assert_edits_equal(
        "import_type2",
        &[((13, 19), "gotRenamed")],
        "import type {rename} from \"bar\";",
        "import type {gotRenamed} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_fn_and_rename_module1() {
    assert_edits_equal(
        "import_fn_and_rename_module1",
        &[((7, 13), "gotRenamed")],
        "import rename, * as myModule from \"bar\";",
        "import gotRenamed, * as myModule from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_fn_and_rename_module2() {
    assert_edits_equal(
        "import_fn_and_rename_module2",
        &[((17, 23), "gotRenamed")],
        "import foo, * as rename from \"bar\";",
        "import foo, * as gotRenamed from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_rename() {
    assert_edits_equal(
        "import_rename",
        &[((8, 14), "gotRenamed")],
        "import {rename as bar} from \"foo\";",
        "import {gotRenamed as bar} from \"foo\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_type_and_fn1() {
    assert_edits_equal(
        "import_type_and_fn1",
        &[((18, 24), "gotRenamed")],
        "import foo, {type rename} from \"bar\";",
        "import foo, {type gotRenamed} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_type3() {
    assert_edits_equal(
        "import_type3",
        &[((13, 19), "gotRenamed")],
        "import {type rename} from \"bar\";",
        "import {type gotRenamed} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_typeof1() {
    assert_edits_equal(
        "import_typeof1",
        &[((14, 20), "gotRenamed")],
        "import typeof rename from \"bar\";",
        "import typeof gotRenamed from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn import_typeof2() {
    assert_edits_equal(
        "import_typeof2",
        &[((15, 21), "gotRenamed")],
        "import typeof {rename} from \"bar\";",
        "import typeof {gotRenamed} from \"bar\";",
        MapperKind::Useless,
    );
}

#[test]
fn throw() {
    assert_edits_equal(
        "throw",
        &[((6, 14), "\"gotRenamed\"")],
        "throw \"rename\";",
        "throw \"gotRenamed\";",
        MapperKind::Literal,
    );
}

#[test]
fn bool1() {
    assert_edits_equal(
        "bool1",
        &[((0, 6), "gotRenamed")],
        "rename = true;",
        "gotRenamed = true;",
        MapperKind::Useless,
    );
}

#[test]
fn bool2() {
    assert_edits_equal(
        "bool2",
        &[((6, 12), "gotRenamed"), ((26, 32), "gotRenamed")],
        "const rename = 0; Boolean(rename);",
        "const gotRenamed = 0; Boolean(gotRenamed);",
        MapperKind::Useless,
    );
}

#[test]
fn bool3() {
    assert_edits_equal(
        "bool3",
        &[((6, 12), "gotRenamed"), ((39, 45), "gotRenamed")],
        "const rename = true; Boolean((false || rename));",
        "const gotRenamed = true; Boolean((false || gotRenamed));",
        MapperKind::Useless,
    );
}

#[test]
fn bool_change() {
    assert_edits_equal(
        "bool_change",
        &[((10, 14), "false"), ((24, 28), "false")],
        "const x = true; Boolean(true);",
        "const x = false; Boolean(false);",
        MapperKind::TrueToFalse,
    );
}

#[test]
fn bool_type_change() {
    assert_edits_equal(
        "bool_type_change",
        &[((9, 13), "false")],
        "const x: true = 'garbage';",
        "const x: false = 'garbage';",
        MapperKind::TrueToFalse,
    );
}

#[test]
fn comment_add() {
    assert_edits_equal(
        "comment_add",
        &[((0, 0), "/*hello*/"), ((3, 3), "/*bye*/")],
        "bla",
        "/*hello*/bla/*bye*/",
        MapperKind::AddComment,
    );
}

#[test]
fn comment_modify() {
    assert_edits_equal(
        "comment_modify",
        &[((0, 7), "/*hello*/"), ((10, 19), "/*bye*/")],
        "/*MAL*/bla/*WRONG*/",
        "/*hello*/bla/*bye*/",
        MapperKind::AddComment,
    );
}

#[test]
fn comment_annot_generic_deep() {
    assert_edits_equal(
        "comment_annot_generic_deep",
        &[
            ((6, 6), "/*hello*/"),
            ((7, 7), "/*bye*/"),
            ((9, 9), "/*hello*/"),
            ((12, 12), "/*bye*/"),
            ((13, 13), "/*hello*/"),
            ((16, 16), "/*bye*/"),
        ],
        "const a: Box<Bla> = {}",
        "const /*hello*/a/*bye*/: /*hello*/Box/*bye*/</*hello*/Bla/*bye*/> = {}",
        MapperKind::AddComment,
    );
}

#[test]
fn let_union_first() {
    assert_edits_equal(
        "let_union_first",
        &[((8, 14), "string")],
        "let x : number | void = 42;",
        "let x : string | void = 42;",
        MapperKind::Useless,
    );
}

#[test]
fn let_union_second() {
    assert_edits_equal(
        "let_union_second",
        &[((18, 24), "string")],
        "let x : boolean | number = 42;",
        "let x : boolean | string = 42;",
        MapperKind::Useless,
    );
}

#[test]
fn let_union_rest() {
    assert_edits_equal(
        "let_union_rest",
        &[((25, 31), "string")],
        "let x : boolean | void | number = 42;",
        "let x : boolean | void | string = 42;",
        MapperKind::Useless,
    );
}

#[test]
fn type_alias_union_argument_mismatch() {
    assert_edits_equal(
        "type_alias_union_argument_mismatch",
        &[((11, 31), "bar | true")],
        "type foo = bar | true | boolean",
        "type foo = bar | true",
        MapperKind::RemoveAnnotationRest,
    );
}

#[test]
fn array_type() {
    assert_edits_equal(
        "array_type",
        &[((8, 14), "gotRenamed")],
        "let x : rename[] = []",
        "let x : gotRenamed[] = []",
        MapperKind::Useless,
    );
}

#[test]
fn sequence1() {
    assert_edits_equal(
        "sequence1",
        &[((10, 16), "gotRenamed")],
        "(a, b, c, rename, d)",
        "(a, b, c, gotRenamed, d)",
        MapperKind::Useless,
    );
}

#[test]
fn sequence2() {
    assert_edits_equal(
        "sequence2",
        &[((1, 11), "a, b, c, d, a, b, c, d")],
        "(a, b, c, d)",
        "(a, b, c, d, a, b, c, d)",
        MapperKind::DoubleSequence,
    );
}

#[test]
fn declare_class_id() {
    assert_edits_equal(
        "declare_class_id",
        &[((14, 20), "gotRenamed")],
        "declare class rename { }",
        "declare class gotRenamed { }",
        MapperKind::Useless,
    );
}

#[test]
fn declare_class_tparam() {
    assert_edits_equal(
        "declare_class_tparam",
        &[((16, 22), "gotRenamed")],
        "declare class C<rename> { }",
        "declare class C<gotRenamed> { }",
        MapperKind::Useless,
    );
}

#[test]
fn declare_class_extends() {
    assert_edits_equal(
        "declare_class_extends",
        &[((24, 30), "gotRenamed")],
        "declare class C extends rename { }",
        "declare class C extends gotRenamed { }",
        MapperKind::Useless,
    );
}

#[test]
fn declare_class_body() {
    assert_edits_equal(
        "declare_class_body",
        &[((21, 27), "gotRenamed")],
        "declare class C { f: rename }",
        "declare class C { f: gotRenamed }",
        MapperKind::Useless,
    );
}

#[test]
fn enum_declaration_unchanged() {
    assert_edits_equal(
        "enum_declaration_unchanged",
        &[],
        "enum Status {On, Off}",
        "enum Status {On, Off}",
        MapperKind::Literal,
    );
}

#[test]
fn enum_comments_unchanged() {
    assert_edits_equal(
        "enum_comments_unchanged",
        &[],
        "enum Status {On, Off // internal comment\n}",
        "enum Status {On, Off // internal comment\n}",
        MapperKind::Literal,
    );
}

#[test]
fn enum_declaration_defaulted_string() {
    assert_edits_equal(
        "enum_declaration_defaulted_string",
        &[((13, 15), "Enabled")],
        "enum Status {On, Off}",
        "enum Status {Enabled, Off}",
        MapperKind::Useless,
    );
}

#[test]
fn enum_declaration_string() {
    assert_edits_equal(
        "enum_declaration_string",
        &[((18, 22), "\"enabled\"")],
        "enum Status {On = \"on\", Off = \"off\"}",
        "enum Status {On = \"enabled\", Off = \"off\"}",
        MapperKind::Useless,
    );
}

#[test]
fn enum_add_comment() {
    assert_edits_equal(
        "enum_add_comment",
        &[((19, 19), "// a comment\n")],
        "enum Status {On = 1, Off = 2}",
        "enum Status {On = 1// a comment\n, Off = 2}",
        MapperKind::Useless,
    );
}

#[test]
fn match_expression_arg() {
    assert_edits_equal(
        "match_expression_arg",
        &[((17, 21), "\"wasNull\"")],
        "const e = match (null) {};",
        "const e = match (\"wasNull\") {};",
        MapperKind::Literal,
    );
}

#[test]
fn match_statement_arg() {
    assert_edits_equal(
        "match_statement_arg",
        &[((7, 11), "\"wasNull\"")],
        "match (null) {}",
        "match (\"wasNull\") {}",
        MapperKind::Literal,
    );
}

#[test]
fn match_expression_case_body() {
    assert_edits_equal(
        "match_expression_case_body",
        &[((26, 30), "\"wasNull\"")],
        "const e = match (x) {1 => null};",
        "const e = match (x) {1 => \"wasNull\"};",
        MapperKind::Literal,
    );
}

#[test]
fn match_expression_case_guard() {
    assert_edits_equal(
        "match_expression_case_guard",
        &[((27, 31), "\"wasNull\"")],
        "const e = match (x) {1 if (null) => 0};",
        "const e = match (x) {1 if (\"wasNull\") => 0};",
        MapperKind::Literal,
    );
}

#[test]
fn match_statement_case_guard() {
    assert_edits_equal(
        "match_statement_case_guard",
        &[((17, 21), "\"wasNull\"")],
        "match (x) {1 if (null) => {}}",
        "match (x) {1 if (\"wasNull\") => {}}",
        MapperKind::Literal,
    );
}

#[test]
fn match_null_pattern() {
    assert_edits_equal(
        "match_null_pattern",
        &[((11, 15), "UpdatedPattern")],
        "match (x) {null => {}}",
        "match (x) {UpdatedPattern => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_identifier_pattern() {
    assert_edits_equal(
        "match_identifier_pattern",
        &[((11, 17), "gotRenamed")],
        "match (x) {rename => {}}",
        "match (x) {gotRenamed => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_number_pattern() {
    assert_edits_equal(
        "match_number_pattern",
        &[((11, 12), "5")],
        "match (x) {4 => {}}",
        "match (x) {5 => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_string_pattern() {
    assert_edits_equal(
        "match_string_pattern",
        &[((11, 21), "\"GotRenamedSL\"")],
        "match (x) {\"RenameSL\" => {}}",
        "match (x) {\"GotRenamedSL\" => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_member_pattern() {
    assert_edits_equal(
        "match_member_pattern",
        &[((13, 19), "gotRenamed")],
        "match (x) {O.rename => {}}",
        "match (x) {O.gotRenamed => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_unary_pattern() {
    assert_edits_equal(
        "match_unary_pattern",
        &[((12, 13), "5")],
        "match (x) {-4 => {}}",
        "match (x) {-5 => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_binding_pattern() {
    assert_edits_equal(
        "match_binding_pattern",
        &[((17, 23), "gotRenamed")],
        "match (x) {const rename => {}}",
        "match (x) {const gotRenamed => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_array_pattern() {
    assert_edits_equal(
        "match_array_pattern",
        &[((15, 19), "UpdatedPattern")],
        "match (x) {[1, null, 3] => {}}",
        "match (x) {[1, UpdatedPattern, 3] => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_object_pattern_property_key() {
    assert_edits_equal(
        "match_object_pattern_property_key",
        &[((18, 24), "gotRenamed")],
        "match (x) {{a: 1, rename: 2, c: 3} => {}}",
        "match (x) {{a: 1, gotRenamed: 2, c: 3} => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_object_pattern_property_value() {
    assert_edits_equal(
        "match_object_pattern_property_value",
        &[((21, 25), "UpdatedPattern")],
        "match (x) {{a: 1, b: null, c: 3} => {}}",
        "match (x) {{a: 1, b: UpdatedPattern, c: 3} => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_object_pattern_property_entire() {
    assert_edits_equal(
        "match_object_pattern_property_entire",
        &[((18, 28), "UpdatedProp: null")],
        "match (x) {{a: 1, changeProp, c: 3} => {}}",
        "match (x) {{a: 1, UpdatedProp: null, c: 3} => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_as_pattern() {
    assert_edits_equal(
        "match_as_pattern",
        &[((11, 15), "UpdatedPattern"), ((19, 25), "gotRenamed")],
        "match (x) {null as rename => {}}",
        "match (x) {UpdatedPattern as gotRenamed => {}}",
        MapperKind::Useless,
    );
}

#[test]
fn match_or_pattern() {
    assert_edits_equal(
        "match_or_pattern",
        &[((15, 19), "UpdatedPattern")],
        "match (x) {1 | null | 3 => {}}",
        "match (x) {1 | UpdatedPattern | 3 => {}}",
        MapperKind::Useless,
    );
}

fn mk_insert<'a>(items: Vec<&'a String>, leading_separator: bool) -> Change<&'a String> {
    Change::Insert {
        items,
        separator: None,
        leading_separator,
    }
}

#[test]
fn list_diff_simple() {
    let a = "a".to_string();
    let b = "b".to_string();
    let old_list = vec![a];
    let new_list = vec![b];
    let edits: Vec<DiffResult<&String>> = vec![(0, Change::Replace(&old_list[0], &new_list[0]))];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_simple2() {
    let a = "a".to_string();
    let b = "b".to_string();
    let old_list = vec![a.clone(), a];
    let new_list = vec![b.clone(), b];
    let edits: Vec<DiffResult<&String>> = vec![
        (0, Change::Replace(&old_list[0], &new_list[0])),
        (1, Change::Replace(&old_list[1], &new_list[1])),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_simple3() {
    let a = "a".to_string();
    let b = "b".to_string();
    let old_list = vec![a.clone(), a];
    let new_list = vec![b.clone(), b.clone(), b.clone(), b];
    let edits: Vec<DiffResult<&String>> = vec![
        (0, Change::Replace(&old_list[0], &new_list[0])),
        (1, Change::Replace(&old_list[1], &new_list[1])),
        (1, mk_insert(vec![&new_list[2], &new_list[3]], true)),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_simple4() {
    let a = "a".to_string();
    let b = "b".to_string();
    let old_list = vec![a.clone(), a.clone(), a.clone(), a];
    let new_list = vec![b.clone(), b];
    let edits: Vec<DiffResult<&String>> = vec![
        (0, Change::Replace(&old_list[0], &new_list[0])),
        (1, Change::Replace(&old_list[1], &new_list[1])),
        (2, Change::Delete(&old_list[2])),
        (3, Change::Delete(&old_list[3])),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_paper() {
    let a = "a".to_string();
    let b = "b".to_string();
    let c = "c".to_string();
    let old_list = vec![
        a.clone(),
        b.clone(),
        c.clone(),
        a.clone(),
        b.clone(),
        b.clone(),
        a.clone(),
    ];
    let new_list = vec![c.clone(), b.clone(), a.clone(), b, a, c];
    let edits: Vec<DiffResult<&String>> = vec![
        (0, Change::Delete(&old_list[0])),
        (1, Change::Delete(&old_list[1])),
        (3, Change::Delete(&old_list[3])),
        (4, mk_insert(vec![&new_list[4]], false)),
        (6, mk_insert(vec![&new_list[5]], false)),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_flip() {
    let x = "x".to_string();
    let y = "y".to_string();
    let old_list = vec![x.clone(), x.clone(), x.clone(), y.clone(), y.clone(), y];
    let new_list = vec![
        old_list[3].clone(),
        old_list[4].clone(),
        old_list[5].clone(),
        x.clone(),
        x.clone(),
        x,
    ];
    let edits: Vec<DiffResult<&String>> = vec![
        (0, Change::Delete(&old_list[0])),
        (1, Change::Delete(&old_list[1])),
        (2, Change::Delete(&old_list[2])),
        (
            5,
            mk_insert(vec![&new_list[3], &new_list[4], &new_list[5]], false),
        ),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_sentence() {
    let t_prime = "T".to_string();
    let h = "h".to_string();
    let i = "i".to_string();
    let s = "s".to_string();
    let space = " ".to_string();
    let e = "e".to_string();
    let n = "n".to_string();
    let t = "t".to_string();
    let c = "c".to_string();
    let o = "o".to_string();
    let pd = ".".to_string();
    let d = "d".to_string();
    let old_list = vec![
        t_prime.clone(),
        h.clone(),
        i.clone(),
        s.clone(),
        space.clone(),
        i.clone(),
        s.clone(),
        space.clone(),
        s.clone(),
        e.clone(),
        n.clone(),
        t.clone(),
        e.clone(),
        n.clone(),
        c.clone(),
        e.clone(),
        space.clone(),
        o.clone(),
        n.clone(),
        e.clone(),
        pd.clone(),
    ];
    let new_list = vec![
        t_prime,
        h,
        i,
        s.clone(),
        space.clone(),
        old_list[5].clone(),
        old_list[6].clone(),
        old_list[7].clone(),
        t.clone(),
        old_list[1].clone(),
        e.clone(),
        space.clone(),
        s.clone(),
        e.clone(),
        c.clone(),
        o.clone(),
        n.clone(),
        d.clone(),
        space.clone(),
        s,
        e.clone(),
        n,
        t,
        e.clone(),
        old_list[13].clone(),
        c,
        e,
        pd,
    ];
    let edits: Vec<DiffResult<&String>> = vec![
        (
            7,
            mk_insert(
                vec![&new_list[8], &new_list[9], &new_list[10], &new_list[11]],
                false,
            ),
        ),
        (9, mk_insert(vec![&new_list[14], &new_list[15]], false)),
        (11, Change::Replace(&old_list[11], &new_list[17])),
        (11, mk_insert(vec![&new_list[18], &new_list[19]], true)),
        (14, Change::Replace(&old_list[14], &new_list[22])),
        (16, Change::Delete(&old_list[16])),
        (17, Change::Delete(&old_list[17])),
        (18, mk_insert(vec![&new_list[25]], false)),
    ];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}

#[test]
fn list_diff_simple5() {
    let a = "a".to_string();
    let b = "b".to_string();
    let old_list = vec![a, b.clone()];
    let new_list = vec![b];
    let edits: Vec<DiffResult<&String>> = vec![(0, Change::Delete(&old_list[0]))];
    let script = list_diff(&old_list, &new_list);
    assert_eq!(Some(edits), script);
}
