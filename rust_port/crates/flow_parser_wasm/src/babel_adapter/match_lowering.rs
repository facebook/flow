/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::match_pattern;
use flow_parser::ast::pattern;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_parser::source_location::SourceLocationTable;
use flow_parser_utils::ast_builder;

use super::builders;
use super::error::BabelLoweringError;
use super::gen_id::GenId;

type Expr = expression::Expression<Loc, Loc>;
type Stmt = statement::Statement<Loc, Loc>;
type MatchPattern = match_pattern::MatchPattern<Loc, Loc>;

#[derive(Clone)]
enum Key {
    Identifier(ast::Identifier<Loc, Loc>),
    String(Loc, ast::StringLiteral<Loc>),
    Number(Loc, ast::NumberLiteral<Loc>),
    BigInt(Loc, ast::BigIntLiteral<Loc>),
}

enum Condition {
    Eq {
        key: Vec<Key>,
        arg: Expr,
    },
    IsNan {
        key: Vec<Key>,
    },
    Array {
        key: Vec<Key>,
        length: usize,
        at_least: bool,
    },
    Object {
        key: Vec<Key>,
    },
    InstanceOf {
        key: Vec<Key>,
        constructor: Expr,
    },
    PropExists {
        key: Vec<Key>,
        name: FlowSmolStr,
    },
    Or(Vec<Vec<Condition>>),
}

enum Binding {
    Id {
        key: Vec<Key>,
        kind: ast::VariableKind,
        id: ast::Identifier<Loc, Loc>,
    },
    ArrayRest {
        key: Vec<Key>,
        kind: ast::VariableKind,
        id: ast::Identifier<Loc, Loc>,
        exclude: usize,
    },
    ObjectRest {
        key: Vec<Key>,
        kind: ast::VariableKind,
        id: ast::Identifier<Loc, Loc>,
        exclude: Vec<Key>,
    },
}

struct Analysis<B> {
    conditions: Vec<Condition>,
    bindings: Vec<Binding>,
    guard: Option<Expr>,
    body: B,
}

struct Analyses<B> {
    has_bindings: bool,
    has_wildcard: bool,
    cases: Vec<Analysis<B>>,
}

struct MatchLowerer<'a> {
    locations: SourceLocationTable<'a>,
    gen_id: GenId,
    error: Option<BabelLoweringError>,
}

impl MatchLowerer<'_> {
    fn syntax<T>(&mut self, loc: &Loc, message: impl Into<String>) -> Option<T> {
        if self.error.is_none() {
            self.error = Some(BabelLoweringError::syntax(loc, message));
        }
        None
    }

    fn gen_identifier(&mut self) -> ast::Identifier<Loc, Loc> {
        let name = self.gen_id.id();
        ast::Identifier::new(ast::IdentifierInner {
            loc: builders::generated_loc(),
            name,
            comments: None,
        })
    }

    fn identifier_expression(id: &ast::Identifier<Loc, Loc>) -> Expr {
        expression::Expression::new(ExpressionInner::Identifier {
            loc: id.loc.dupe(),
            inner: id.dupe(),
        })
    }

    fn identifier_with_loc(id: &ast::Identifier<Loc, Loc>, loc: Loc) -> ast::Identifier<Loc, Loc> {
        ast::Identifier::new(ast::IdentifierInner {
            loc,
            name: id.name.dupe(),
            comments: id.comments.dupe(),
        })
    }

    fn serialized_identifier_after(
        &self,
        id: &ast::Identifier<Loc, Loc>,
        loc: &Loc,
    ) -> ast::Identifier<Loc, Loc> {
        Self::identifier_with_loc(id, self.locations.next_token_loc_after(loc))
    }

    fn literal_expression(pattern: &MatchPattern) -> Option<Expr> {
        Some(match pattern {
            MatchPattern::NumberPattern { loc, inner } => {
                expression::Expression::new(ExpressionInner::NumberLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new((**inner).clone()),
                })
            }
            MatchPattern::BigIntPattern { loc, inner } => {
                expression::Expression::new(ExpressionInner::BigIntLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new((**inner).clone()),
                })
            }
            MatchPattern::StringPattern { loc, inner } => {
                expression::Expression::new(ExpressionInner::StringLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new((**inner).clone()),
                })
            }
            MatchPattern::BooleanPattern { loc, inner } => {
                expression::Expression::new(ExpressionInner::BooleanLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new((**inner).clone()),
                })
            }
            MatchPattern::NullPattern { loc, inner } => {
                expression::Expression::new(ExpressionInner::NullLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new((**inner).clone()),
                })
            }
            _ => return None,
        })
    }

    fn key_expression(key: &Key) -> Expr {
        match key {
            Key::Identifier(id) => Self::identifier_expression(id),
            Key::String(loc, literal) => {
                expression::Expression::new(ExpressionInner::StringLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new(literal.clone()),
                })
            }
            Key::Number(loc, literal) => {
                expression::Expression::new(ExpressionInner::NumberLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new(literal.clone()),
                })
            }
            Key::BigInt(loc, literal) => {
                expression::Expression::new(ExpressionInner::BigIntLiteral {
                    loc: loc.dupe(),
                    inner: Arc::new(literal.clone()),
                })
            }
        }
    }

    fn member_pattern_expression(pattern: &match_pattern::MemberPattern<Loc, Loc>) -> Expr {
        let object = match &pattern.base {
            match_pattern::member_pattern::Base::BaseIdentifier(id) => {
                Self::identifier_expression(id)
            }
            match_pattern::member_pattern::Base::BaseMember(member) => {
                Self::member_pattern_expression(member)
            }
        };
        let member = match &pattern.property {
            match_pattern::member_pattern::Property::PropertyIdentifier(id) => {
                ast_builder::expressions::members::identifier(None, id.dupe(), object)
            }
            match_pattern::member_pattern::Property::PropertyString { loc, literal } => {
                ast_builder::expressions::members::expression(
                    None,
                    expression::Expression::new(ExpressionInner::StringLiteral {
                        loc: loc.dupe(),
                        inner: Arc::new(literal.clone()),
                    }),
                    object,
                )
            }
            match_pattern::member_pattern::Property::PropertyNumber { loc, literal } => {
                ast_builder::expressions::members::expression(
                    None,
                    expression::Expression::new(ExpressionInner::NumberLiteral {
                        loc: loc.dupe(),
                        inner: Arc::new(literal.clone()),
                    }),
                    object,
                )
            }
            match_pattern::member_pattern::Property::PropertyBigInt { loc, literal } => {
                ast_builder::expressions::members::expression(
                    None,
                    expression::Expression::new(ExpressionInner::BigIntLiteral {
                        loc: loc.dupe(),
                        inner: Arc::new(literal.clone()),
                    }),
                    object,
                )
            }
        };
        ast_builder::expressions::member(Some(pattern.loc.dupe()), member)
    }

    fn match_object_key(
        &self,
        key: &match_pattern::object_pattern::Key<Loc, Loc>,
        identifier_loc: Option<&Loc>,
    ) -> Key {
        match key {
            match_pattern::object_pattern::Key::Identifier(id) => {
                Key::Identifier(identifier_loc.map_or_else(
                    || id.dupe(),
                    |loc| Self::identifier_with_loc(id, loc.dupe()),
                ))
            }
            match_pattern::object_pattern::Key::StringLiteral((loc, literal)) => {
                Key::String(loc.dupe(), literal.clone())
            }
            match_pattern::object_pattern::Key::NumberLiteral((loc, literal)) => {
                Key::Number(loc.dupe(), literal.clone())
            }
            match_pattern::object_pattern::Key::BigIntLiteral((loc, literal)) => {
                Key::BigInt(loc.dupe(), literal.clone())
            }
        }
    }

    fn key_name(key: &Key) -> FlowSmolStr {
        match key {
            Key::Identifier(id) => id.name.dupe(),
            Key::String(_, literal) => literal.value.dupe(),
            Key::Number(_, literal) => FlowSmolStr::from(literal.value.to_string()),
            Key::BigInt(_, literal) => literal.raw.dupe(),
        }
    }

    fn needs_prop_exists(pattern: &MatchPattern) -> bool {
        match pattern {
            MatchPattern::WildcardPattern { .. }
            | MatchPattern::BindingPattern { .. }
            | MatchPattern::IdentifierPattern { .. }
            | MatchPattern::MemberPattern { .. } => true,
            MatchPattern::AsPattern { inner, .. } => Self::needs_prop_exists(&inner.pattern),
            MatchPattern::OrPattern { inner, .. } => {
                inner.patterns.iter().any(Self::needs_prop_exists)
            }
            _ => false,
        }
    }

    fn check_binding(
        &mut self,
        loc: &Loc,
        seen: &mut HashSet<FlowSmolStr>,
        binding: &match_pattern::BindingPattern<Loc, Loc>,
    ) -> bool {
        if !seen.insert(binding.id.name.dupe()) {
            self.syntax::<()>(
                loc,
                format!(
                    "Duplicate variable name '{}' in match case pattern.",
                    binding.id.name
                ),
            );
            return false;
        }
        if binding.kind == ast::VariableKind::Var {
            self.syntax::<()>(loc, "'var' bindings are not allowed. Use 'const' or 'let'.");
            return false;
        }
        true
    }

    fn analyze_properties(
        &mut self,
        root_key: &[Key],
        seen_bindings: &mut HashSet<FlowSmolStr>,
        object: &match_pattern::ObjectPattern<Loc, Loc>,
    ) -> Option<(Vec<Condition>, Vec<Binding>)> {
        let mut conditions = Vec::new();
        let mut bindings = Vec::new();
        let mut excluded = Vec::new();
        let mut seen_names = HashSet::new();
        for property in object.properties.iter() {
            let match_pattern::object_pattern::Property::Valid { property, .. } = property else {
                return self.syntax(property.loc(), "Invalid shorthand match object property.");
            };
            let shorthand_key_loc = match (&property.key, &property.pattern) {
                (
                    match_pattern::object_pattern::Key::Identifier(_),
                    MatchPattern::BindingPattern { loc, .. },
                ) if property.shorthand => Some(self.locations.next_token_loc_after(loc)),
                _ => None,
            };
            let key = self.match_object_key(&property.key, shorthand_key_loc.as_ref());
            let name = Self::key_name(&key);
            if !seen_names.insert(name.dupe()) {
                return self.syntax(
                    property.pattern.loc(),
                    format!("Duplicate property name '{name}' in match object pattern."),
                );
            }
            excluded.push(key.clone());
            if Self::needs_prop_exists(&property.pattern) {
                conditions.push(Condition::PropExists {
                    key: root_key.to_vec(),
                    name,
                });
            }
            let mut child_key = root_key.to_vec();
            child_key.push(key);
            let (mut child_conditions, mut child_bindings) =
                self.analyze_pattern(&property.pattern, &child_key, seen_bindings)?;
            conditions.append(&mut child_conditions);
            bindings.append(&mut child_bindings);
        }
        if let Some(rest) = &object.rest
            && let Some((loc, binding)) = &rest.argument
        {
            if !self.check_binding(loc, seen_bindings, binding) {
                return None;
            }
            bindings.push(Binding::ObjectRest {
                key: root_key.to_vec(),
                kind: binding.kind,
                id: self.serialized_identifier_after(&binding.id, loc),
                exclude: excluded,
            });
        }
        if self.error.is_some() {
            None
        } else {
            Some((conditions, bindings))
        }
    }

    fn analyze_pattern(
        &mut self,
        pattern: &MatchPattern,
        key: &[Key],
        seen: &mut HashSet<FlowSmolStr>,
    ) -> Option<(Vec<Condition>, Vec<Binding>)> {
        match pattern {
            MatchPattern::WildcardPattern { .. } => Some((vec![], vec![])),
            MatchPattern::UnaryPattern { loc, inner } => {
                if matches!(
                    &inner.argument.1,
                    match_pattern::unary_pattern::Argument::NumberLiteral(literal)
                        if literal.value == 0.0
                ) {
                    return self.syntax(
                        loc,
                        "'+0' and '-0' are not yet supported in match unary patterns.",
                    );
                }
                let argument = match &inner.argument.1 {
                    match_pattern::unary_pattern::Argument::NumberLiteral(literal) => {
                        expression::Expression::new(ExpressionInner::NumberLiteral {
                            loc: inner.argument.0.dupe(),
                            inner: Arc::new(literal.clone()),
                        })
                    }
                    match_pattern::unary_pattern::Argument::BigIntLiteral(literal) => {
                        expression::Expression::new(ExpressionInner::BigIntLiteral {
                            loc: inner.argument.0.dupe(),
                            inner: Arc::new(literal.clone()),
                        })
                    }
                };
                let operator = match inner.operator {
                    match_pattern::unary_pattern::Operator::Plus => expression::UnaryOperator::Plus,
                    match_pattern::unary_pattern::Operator::Minus => {
                        expression::UnaryOperator::Minus
                    }
                };
                Some((
                    vec![Condition::Eq {
                        key: key.to_vec(),
                        arg: ast_builder::expressions::unary(
                            Some(loc.dupe()),
                            None,
                            operator,
                            argument,
                        ),
                    }],
                    vec![],
                ))
            }
            MatchPattern::IdentifierPattern { inner, .. } => {
                if inner.name == "NaN" {
                    Some((vec![Condition::IsNan { key: key.to_vec() }], vec![]))
                } else {
                    Some((
                        vec![Condition::Eq {
                            key: key.to_vec(),
                            arg: Self::identifier_expression(inner),
                        }],
                        vec![],
                    ))
                }
            }
            MatchPattern::MemberPattern { inner, .. } => Some((
                vec![Condition::Eq {
                    key: key.to_vec(),
                    arg: Self::member_pattern_expression(inner),
                }],
                vec![],
            )),
            MatchPattern::BindingPattern { loc, inner } => {
                if !self.check_binding(loc, seen, inner) {
                    return None;
                }
                Some((
                    vec![],
                    vec![Binding::Id {
                        key: key.to_vec(),
                        kind: inner.kind,
                        id: self.serialized_identifier_after(&inner.id, loc),
                    }],
                ))
            }
            MatchPattern::AsPattern { loc, inner } => {
                if matches!(inner.pattern, MatchPattern::BindingPattern { .. }) {
                    return self.syntax(
                        loc,
                        "Match 'as' patterns are not allowed directly on binding patterns.",
                    );
                }
                let (conditions, mut bindings) = self.analyze_pattern(&inner.pattern, key, seen)?;
                let (target_loc, kind, id) = match &inner.target {
                    match_pattern::as_pattern::Target::Identifier(id) => {
                        (&id.loc, ast::VariableKind::Const, id)
                    }
                    match_pattern::as_pattern::Target::Binding { loc, pattern } => {
                        (loc, pattern.kind, &pattern.id)
                    }
                };
                let binding = match_pattern::BindingPattern {
                    kind,
                    id: id.dupe(),
                    comments: None,
                };
                if !self.check_binding(target_loc, seen, &binding) {
                    return None;
                }
                bindings.push(Binding::Id {
                    key: key.to_vec(),
                    kind,
                    id: self.serialized_identifier_after(id, target_loc),
                });
                Some((conditions, bindings))
            }
            MatchPattern::ArrayPattern { inner, .. } => {
                let mut conditions = vec![Condition::Array {
                    key: key.to_vec(),
                    length: inner.elements.len(),
                    at_least: inner.rest.is_some(),
                }];
                let mut bindings = Vec::new();
                for (index, element) in inner.elements.iter().enumerate() {
                    let mut child_key = key.to_vec();
                    child_key.push(Key::Number(
                        builders::generated_loc(),
                        ast_builder::int_literal(None, index as i32),
                    ));
                    let (mut child_conditions, mut child_bindings) =
                        self.analyze_pattern(&element.pattern, &child_key, seen)?;
                    conditions.append(&mut child_conditions);
                    bindings.append(&mut child_bindings);
                }
                if let Some(rest) = &inner.rest
                    && let Some((loc, binding)) = &rest.argument
                {
                    if !self.check_binding(loc, seen, binding) {
                        return None;
                    }
                    bindings.push(Binding::ArrayRest {
                        key: key.to_vec(),
                        kind: binding.kind,
                        id: self.serialized_identifier_after(&binding.id, loc),
                        exclude: inner.elements.len(),
                    });
                }
                Some((conditions, bindings))
            }
            MatchPattern::ObjectPattern { inner, .. } => {
                let (mut conditions, bindings) = self.analyze_properties(key, seen, inner)?;
                conditions.insert(0, Condition::Object { key: key.to_vec() });
                Some((conditions, bindings))
            }
            MatchPattern::InstancePattern { inner, .. } => {
                let constructor = match &inner.constructor {
                    match_pattern::InstancePatternConstructor::IdentifierConstructor(id) => {
                        Self::identifier_expression(id)
                    }
                    match_pattern::InstancePatternConstructor::MemberConstructor(member) => {
                        Self::member_pattern_expression(member)
                    }
                };
                let (mut conditions, bindings) =
                    self.analyze_properties(key, seen, &inner.properties.1)?;
                conditions.insert(
                    0,
                    Condition::InstanceOf {
                        key: key.to_vec(),
                        constructor,
                    },
                );
                Some((conditions, bindings))
            }
            MatchPattern::OrPattern { loc, inner } => {
                let mut has_wildcard = false;
                let mut alternatives = Vec::new();
                for subpattern in inner.patterns.iter() {
                    let (conditions, bindings) = self.analyze_pattern(subpattern, key, seen)?;
                    if !bindings.is_empty() {
                        return self.syntax(
                            loc,
                            "Bindings in match 'or' patterns are not yet supported.",
                        );
                    }
                    has_wildcard |= conditions.is_empty();
                    alternatives.push(conditions);
                }
                if has_wildcard {
                    Some((vec![], vec![]))
                } else {
                    Some((vec![Condition::Or(alternatives)], vec![]))
                }
            }
            MatchPattern::NumberPattern { .. }
            | MatchPattern::BigIntPattern { .. }
            | MatchPattern::StringPattern { .. }
            | MatchPattern::BooleanPattern { .. }
            | MatchPattern::NullPattern { .. } => Self::literal_expression(pattern).map(|arg| {
                (
                    vec![Condition::Eq {
                        key: key.to_vec(),
                        arg,
                    }],
                    vec![],
                )
            }),
        }
    }

    fn analyze_cases<B: Clone>(
        &mut self,
        cases: &[ast::match_::Case<Loc, Loc, B>],
    ) -> Option<Analyses<B>> {
        let mut has_bindings = false;
        let mut has_wildcard = false;
        let mut analyses = Vec::new();
        for case in cases {
            let (conditions, bindings) =
                self.analyze_pattern(&case.pattern, &[], &mut HashSet::new())?;
            has_bindings |= !bindings.is_empty();
            let catches_all = conditions.is_empty() && case.guard.is_none();
            analyses.push(Analysis {
                conditions,
                bindings,
                guard: case.guard.clone(),
                body: case.body.clone(),
            });
            if catches_all {
                has_wildcard = true;
                break;
            }
        }
        Some(Analyses {
            has_bindings,
            has_wildcard,
            cases: analyses,
        })
    }

    fn expression_of_key(root: &Expr, key: &[Key]) -> Expr {
        key.iter()
            .fold(root.clone(), |object, property| match property {
                Key::Identifier(id) => ast_builder::expressions::member(
                    Some(builders::generated_loc()),
                    ast_builder::expressions::members::identifier(None, id.dupe(), object),
                ),
                _ => ast_builder::expressions::member(
                    Some(builders::generated_loc()),
                    ast_builder::expressions::members::expression(
                        None,
                        Self::key_expression(property),
                        object,
                    ),
                ),
            })
    }

    fn conjunction(mut tests: Vec<Expr>) -> Expr {
        let first = tests.remove(0);
        tests
            .into_iter()
            .fold(first, ast_builder::expressions::logical_and)
    }

    fn disjunction(mut tests: Vec<Expr>) -> Expr {
        let first = tests.remove(0);
        tests
            .into_iter()
            .fold(first, ast_builder::expressions::logical_or)
    }

    fn typeof_equals(value: Expr, type_name: &str) -> Expr {
        ast_builder::expressions::binary(
            Some(builders::generated_loc()),
            None,
            expression::BinaryOperator::StrictEqual,
            ast_builder::expressions::unary(
                Some(builders::generated_loc()),
                None,
                expression::UnaryOperator::Typeof,
                value,
            ),
            builders::string_literal(type_name),
        )
    }

    fn tests_of_condition(&self, root: &Expr, condition: &Condition) -> Vec<Expr> {
        match condition {
            Condition::Eq { key, arg } => vec![ast_builder::expressions::binary(
                Some(builders::generated_loc()),
                None,
                expression::BinaryOperator::StrictEqual,
                Self::expression_of_key(root, key),
                arg.clone(),
            )],
            Condition::IsNan { key } => vec![builders::call(
                builders::member(builders::identifier("Number"), "isNaN"),
                vec![Self::expression_of_key(root, key)],
            )],
            Condition::Array {
                key,
                length,
                at_least,
            } => {
                let value = Self::expression_of_key(root, key);
                vec![
                    builders::call(
                        builders::member(builders::identifier("Array"), "isArray"),
                        vec![value.clone()],
                    ),
                    ast_builder::expressions::binary(
                        Some(builders::generated_loc()),
                        None,
                        if *at_least {
                            expression::BinaryOperator::GreaterThanEqual
                        } else {
                            expression::BinaryOperator::StrictEqual
                        },
                        builders::member(value, "length"),
                        ast_builder::int_literal_expression(
                            Some(builders::generated_loc()),
                            None,
                            *length as i32,
                        ),
                    ),
                ]
            }
            Condition::Object { key } => {
                let value = Self::expression_of_key(root, key);
                let object = Self::conjunction(vec![
                    Self::typeof_equals(value.clone(), "object"),
                    ast_builder::expressions::binary(
                        Some(builders::generated_loc()),
                        None,
                        expression::BinaryOperator::StrictNotEqual,
                        value.clone(),
                        expression::Expression::new(ExpressionInner::NullLiteral {
                            loc: builders::generated_loc(),
                            inner: Arc::new(None),
                        }),
                    ),
                ]);
                vec![Self::disjunction(vec![
                    object,
                    Self::typeof_equals(value, "function"),
                ])]
            }
            Condition::InstanceOf { key, constructor } => {
                vec![ast_builder::expressions::binary(
                    Some(builders::generated_loc()),
                    None,
                    expression::BinaryOperator::Instanceof,
                    Self::expression_of_key(root, key),
                    constructor.clone(),
                )]
            }
            Condition::PropExists { key, name } => vec![ast_builder::expressions::binary(
                Some(builders::generated_loc()),
                None,
                expression::BinaryOperator::In,
                builders::string_literal(name.as_str()),
                Self::expression_of_key(root, key),
            )],
            Condition::Or(alternatives) => vec![Self::disjunction(
                alternatives
                    .iter()
                    .map(|conditions| Self::conjunction(self.tests_of_conditions(root, conditions)))
                    .collect(),
            )],
        }
    }

    fn tests_of_conditions(&self, root: &Expr, conditions: &[Condition]) -> Vec<Expr> {
        conditions
            .iter()
            .flat_map(|condition| self.tests_of_condition(root, condition))
            .collect()
    }

    fn variable(kind: ast::VariableKind, id: pattern::Pattern<Loc, Loc>, init: Expr) -> Stmt {
        ast_builder::statements::variable_declaration(
            Some(kind),
            Some(builders::generated_loc()),
            None,
            vec![ast_builder::statements::variable_declarator_generic(
                Some(builders::generated_loc()),
                id,
                Some(init),
            )],
        )
    }

    fn statements_of_bindings(&mut self, root: &Expr, bindings: &[Binding]) -> Vec<Stmt> {
        bindings
            .iter()
            .map(|binding| match binding {
                Binding::Id { key, kind, id } => Self::variable(
                    *kind,
                    builders::identifier_pattern(id),
                    Self::expression_of_key(root, key),
                ),
                Binding::ArrayRest {
                    key,
                    kind,
                    id,
                    exclude,
                } => Self::variable(
                    *kind,
                    builders::identifier_pattern(id),
                    builders::call(
                        builders::member(Self::expression_of_key(root, key), "slice"),
                        vec![ast_builder::int_literal_expression(
                            Some(builders::generated_loc()),
                            None,
                            *exclude as i32,
                        )],
                    ),
                ),
                Binding::ObjectRest {
                    key,
                    kind,
                    id,
                    exclude,
                } => {
                    let mut properties = exclude
                        .iter()
                        .map(|key| {
                            let property_key = match key {
                                Key::Identifier(id) => pattern::object::Key::Identifier(id.dupe()),
                                _ => pattern::object::Key::Computed(ast::ComputedKey {
                                    loc: builders::generated_loc(),
                                    expression: Self::key_expression(key),
                                    comments: None,
                                }),
                            };
                            let temp = self.gen_identifier();
                            pattern::object::Property::NormalProperty(
                                pattern::object::NormalProperty {
                                    loc: builders::generated_loc(),
                                    key: property_key,
                                    pattern: builders::identifier_pattern(&temp),
                                    default: None,
                                    shorthand: false,
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    properties.push(pattern::object::Property::RestElement(
                        pattern::RestElement {
                            loc: builders::generated_loc(),
                            argument: builders::identifier_pattern(id),
                            comments: None,
                        },
                    ));
                    let object_pattern = pattern::Pattern::Object {
                        loc: builders::generated_loc(),
                        inner: Arc::new(pattern::Object {
                            properties: properties.into(),
                            annot: ast::types::AnnotationOrHint::Missing(builders::generated_loc()),
                            optional: false,
                            comments: None,
                        }),
                    };
                    Self::variable(*kind, object_pattern, Self::expression_of_key(root, key))
                }
            })
            .collect()
    }

    fn fallthrough(root: &Expr) -> Stmt {
        let message = ast_builder::expressions::binary(
            Some(builders::generated_loc()),
            None,
            expression::BinaryOperator::Plus,
            builders::string_literal(
                "Match: No case succesfully matched. Make exhaustive or add a wildcard case using '_'. Argument: ",
            ),
            root.clone(),
        );
        statement::Statement::new(StatementInner::Throw {
            loc: builders::generated_loc(),
            inner: Arc::new(statement::Throw {
                argument: builders::call(builders::identifier("Error"), vec![message]),
                comments: None,
            }),
        })
    }

    fn is_simple_argument(expression: &Expr) -> bool {
        match &**expression {
            ExpressionInner::Identifier { .. } | ExpressionInner::Super { .. } => true,
            ExpressionInner::Member { inner, .. } => match &inner.property {
                expression::member::Property::PropertyIdentifier(_)
                | expression::member::Property::PropertyPrivateName(_) => {
                    Self::is_simple_argument(&inner.object)
                }
                expression::member::Property::PropertyExpression(property) => {
                    matches!(
                        &**property,
                        ExpressionInner::StringLiteral { .. }
                            | ExpressionInner::NumberLiteral { .. }
                            | ExpressionInner::BigIntLiteral { .. }
                    ) && Self::is_simple_argument(&inner.object)
                }
            },
            _ => false,
        }
    }

    fn block(statements: Vec<Stmt>) -> Stmt {
        ast_builder::statements::block(None, statements)
    }

    fn iife(
        statements: Vec<Stmt>,
        param: Option<&ast::Identifier<Loc, Loc>>,
        arg: Option<Expr>,
    ) -> Expr {
        let params = param
            .map(|id| {
                vec![ast_builder::functions::param(
                    Some(builders::generated_loc()),
                    None,
                    builders::identifier_pattern(id),
                )]
            })
            .unwrap_or_default();
        let arrow = ast_builder::expressions::arrow_function(
            Some(builders::generated_loc()),
            Some(false),
            Some(ast_builder::functions::params(
                Some(builders::generated_loc()),
                None,
                None,
                None,
                params,
            )),
            Some(ast_builder::functions::body(
                Some(builders::generated_loc()),
                None,
                statements,
            )),
        );
        builders::call(arrow, arg.into_iter().collect())
    }

    fn lower_match_expression(
        &mut self,
        match_: &expression::MatchExpression<Loc, Loc>,
    ) -> Option<Expr> {
        let mut analyses = self.analyze_cases(&match_.cases)?;
        let simple = !analyses.has_bindings && Self::is_simple_argument(&match_.arg);
        let generated_root = (!simple).then(|| self.gen_identifier());
        let root = generated_root
            .as_ref()
            .map(Self::identifier_expression)
            .unwrap_or_else(|| match_.arg.clone());
        if simple {
            let last = if analyses.has_wildcard {
                analyses
                    .cases
                    .pop()
                    .map(|case| case.body)
                    .expect("wildcard analysis exists")
            } else {
                Self::iife(vec![Self::fallthrough(&root)], None, None)
            };
            return Some(
                analyses
                    .cases
                    .into_iter()
                    .rev()
                    .fold(last, |alternate, case| {
                        let mut tests = self.tests_of_conditions(&root, &case.conditions);
                        tests.extend(case.guard);
                        ast_builder::expressions::conditional(
                            Some(builders::generated_loc()),
                            None,
                            Self::conjunction(tests),
                            case.body,
                            alternate,
                        )
                    }),
            );
        }
        let mut statements = Vec::new();
        for case in analyses.cases {
            let return_statement = ast_builder::statements::return_(
                Some(builders::generated_loc()),
                None,
                Some(case.body),
            );
            let body = if let Some(guard) = case.guard {
                ast_builder::statements::if_(
                    Some(builders::generated_loc()),
                    None,
                    guard,
                    return_statement,
                    None,
                )
            } else {
                return_statement
            };
            let mut case_body = self.statements_of_bindings(&root, &case.bindings);
            case_body.push(body);
            if case.conditions.is_empty() {
                statements.push(if case.bindings.is_empty() {
                    case_body.pop().expect("case body has return")
                } else {
                    Self::block(case_body)
                });
            } else {
                statements.push(ast_builder::statements::if_(
                    Some(builders::generated_loc()),
                    None,
                    Self::conjunction(self.tests_of_conditions(&root, &case.conditions)),
                    Self::block(case_body),
                    None,
                ));
            }
        }
        if !analyses.has_wildcard {
            statements.push(Self::fallthrough(&root));
        }
        let argument = generated_root.as_ref().map(|_| match_.arg.clone());
        Some(Self::iife(statements, generated_root.as_ref(), argument))
    }

    fn lower_match_statement(
        &mut self,
        match_: &statement::MatchStatement<Loc, Loc>,
    ) -> Option<Stmt> {
        let analyses = self.analyze_cases(&match_.cases)?;
        let label = self.gen_identifier();
        let simple = !analyses.has_bindings && Self::is_simple_argument(&match_.arg);
        let generated_root = (!simple).then(|| self.gen_identifier());
        let root = generated_root
            .as_ref()
            .map(Self::identifier_expression)
            .unwrap_or_else(|| match_.arg.clone());
        let mut statements = Vec::new();
        if let Some(id) = &generated_root {
            statements.push(Self::variable(
                ast::VariableKind::Const,
                builders::identifier_pattern(id),
                match_.arg.clone(),
            ));
        }
        for case in analyses.cases {
            let StatementInner::Block { inner: body, .. } = &*case.body else {
                return self.syntax(
                    case.body.loc(),
                    "Match statement case body must be a block.",
                );
            };
            let mut body_statements = body.body.to_vec();
            body_statements.push(ast_builder::statements::break_(
                Some(builders::generated_loc()),
                None,
                Some(label.dupe()),
            ));
            let guarded = if let Some(guard) = case.guard {
                vec![ast_builder::statements::if_(
                    Some(builders::generated_loc()),
                    None,
                    guard,
                    Self::block(body_statements),
                    None,
                )]
            } else {
                body_statements
            };
            let mut case_body = self.statements_of_bindings(&root, &case.bindings);
            case_body.extend(guarded);
            statements.push(if case.conditions.is_empty() {
                Self::block(case_body)
            } else {
                ast_builder::statements::if_(
                    Some(builders::generated_loc()),
                    None,
                    Self::conjunction(self.tests_of_conditions(&root, &case.conditions)),
                    Self::block(case_body),
                    None,
                )
            });
        }
        if !analyses.has_wildcard {
            statements.push(Self::fallthrough(&root));
        }
        Some(ast_builder::statements::labeled(
            None,
            label,
            Self::block(statements),
        ))
    }
}

impl<'ast> AstVisitor<'ast, Loc> for MatchLowerer<'_> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_identifier(
        &mut self,
        identifier: &'ast ast::Identifier<Loc, Loc>,
    ) -> ast::Identifier<Loc, Loc> {
        self.gen_id.add_usage(&identifier.name);
        identifier.dupe()
    }

    fn map_expression(&mut self, expression: &'ast Expr) -> Expr {
        if let ExpressionInner::Match { inner, .. } = &**expression
            && let Some(lowered) = self.lower_match_expression(inner)
        {
            return self.map_expression(&lowered);
        }
        ast_visitor::map_expression_default(self, expression)
    }

    fn map_statement(&mut self, statement: &'ast Stmt) -> Stmt {
        if let StatementInner::Match { inner, .. } = &**statement
            && let Some(lowered) = self.lower_match_statement(inner)
        {
            return self.map_statement(&lowered);
        }
        ast_visitor::map_statement_default(self, statement)
    }
}

pub fn lower_program(
    source: &str,
    program: &ast::Program<Loc, Loc>,
) -> Result<ast::Program<Loc, Loc>, BabelLoweringError> {
    let mut lowerer = MatchLowerer {
        locations: SourceLocationTable::new(source),
        gen_id: GenId::new("m"),
        error: None,
    };
    let program = lowerer.map_program(program);
    match lowerer.error {
        Some(error) => Err(error),
        None => Ok(program),
    }
}
