/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::match_;
use flow_parser::ast::match_pattern;
use flow_parser::ast::match_pattern::object_pattern;
use flow_parser::ast::statement;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_match_expression_default;
use flow_parser::ast_visitor::map_match_statement_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

fn get_identifier_from_expr(expr: &expression::Expression<Loc, Loc>) -> Option<FlowSmolStr> {
    match expr.deref() {
        ExpressionInner::Identifier { inner, .. } => Some(inner.name.dupe()),
        _ => None,
    }
}

fn get_property_name(property: &expression::member::Property<Loc, Loc>) -> Option<FlowSmolStr> {
    match property {
        expression::member::Property::PropertyIdentifier(id) => Some(id.name.dupe()),
        _ => None,
    }
}

struct PropertyAccessCollector {
    acc: BTreeSet<FlowSmolStr>,
    base_obj_name: FlowSmolStr,
}

impl<'ast> AstVisitor<'ast, Loc> for PropertyAccessCollector {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn member(
        &mut self,
        _loc: &'ast Loc,
        expr: &'ast expression::Member<Loc, Loc>,
    ) -> Result<(), !> {
        let expression::Member {
            object: _object,
            property,
            comments: _,
        } = expr;
        match (
            get_identifier_from_expr(_object),
            get_property_name(property),
        ) {
            (Some(obj_name), Some(prop_name)) if obj_name == self.base_obj_name => {
                self.acc.insert(prop_name);
            }
            _ => {}
        }
        flow_parser::ast_visitor::member_default(self, _loc, expr)
    }
}

fn is_simple_literal_pattern(pattern: &match_pattern::MatchPattern<Loc, Loc>) -> bool {
    match pattern {
        match_pattern::MatchPattern::StringPattern { .. }
        | match_pattern::MatchPattern::NumberPattern { .. }
        | match_pattern::MatchPattern::BigIntPattern { .. }
        | match_pattern::MatchPattern::BooleanPattern { .. }
        | match_pattern::MatchPattern::UnaryPattern { .. }
        | match_pattern::MatchPattern::IdentifierPattern { .. }
        | match_pattern::MatchPattern::MemberPattern { .. } => true,
        _ => false,
    }
}

fn create_discriminant_property(
    discriminant_name: &FlowSmolStr,
    literal_pattern: &match_pattern::MatchPattern<Loc, Loc>,
) -> object_pattern::Property<Loc, Loc> {
    let lit_loc = literal_pattern.loc().dupe();
    let key = object_pattern::Key::Identifier(ast::Identifier::new(IdentifierInner {
        loc: lit_loc.dupe(),
        name: discriminant_name.dupe(),
        comments: None,
    }));
    let pattern = literal_pattern.clone();
    object_pattern::Property::Valid {
        loc: lit_loc,
        property: object_pattern::PropertyStruct {
            key,
            pattern,
            shorthand: false,
            comments: None,
        },
    }
}

fn create_binding_property(
    prop_name: &FlowSmolStr,
    loc: Loc,
) -> object_pattern::Property<Loc, Loc> {
    let id = ast::Identifier::new(IdentifierInner {
        loc: loc.dupe(),
        name: prop_name.dupe(),
        comments: None,
    });
    let binding_pattern = match_pattern::BindingPattern {
        kind: ast::VariableKind::Const,
        id: id.dupe(),
        comments: None,
    };
    let pattern = match_pattern::MatchPattern::BindingPattern {
        loc: loc.dupe(),
        inner: Arc::new(binding_pattern),
    };
    let key = object_pattern::Key::Identifier(id);
    object_pattern::Property::Valid {
        loc,
        property: object_pattern::PropertyStruct {
            key,
            pattern,
            shorthand: true,
            comments: None,
        },
    }
}

fn create_object_pattern(
    discriminant_name: &FlowSmolStr,
    literal_pattern: &match_pattern::MatchPattern<Loc, Loc>,
    accessed_props: &BTreeSet<FlowSmolStr>,
    loc: Loc,
) -> match_pattern::ObjectPattern<Loc, Loc> {
    let discriminant_prop = create_discriminant_property(discriminant_name, literal_pattern);
    let binding_props: Vec<object_pattern::Property<Loc, Loc>> = accessed_props
        .iter()
        .map(|prop| create_binding_property(prop, loc.dupe()))
        .collect();
    let mut properties = vec![discriminant_prop];
    properties.extend(binding_props);
    match_pattern::ObjectPattern {
        properties: Arc::from(properties),
        rest: Some(match_pattern::RestPattern {
            loc: LOC_NONE,
            argument: None,
            comments: None,
        }),
        comments: None,
    }
}

struct MemberReplacer {
    base_obj_name: FlowSmolStr,
}

impl<'ast> AstVisitor<'ast, Loc> for MemberReplacer {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_expression(
        &mut self,
        expr: &expression::Expression<Loc, Loc>,
    ) -> expression::Expression<Loc, Loc> {
        match expr.deref() {
            ExpressionInner::Member { loc, inner }
                if let expression::member::Property::PropertyIdentifier(prop_id) =
                    &inner.property =>
            {
                let object = &inner.object;
                let name = &prop_id.name;
                match get_identifier_from_expr(object) {
                    Some(obj_name) if obj_name == self.base_obj_name => {
                        expression::Expression::new(ExpressionInner::Identifier {
                            loc: loc.dupe(),
                            inner: ast::Identifier::new(IdentifierInner {
                                loc: loc.dupe(),
                                name: name.dupe(),
                                comments: None,
                            }),
                        })
                    }
                    _ => map_expression_default(self, expr),
                }
            }
            _ => map_expression_default(self, expr),
        }
    }
}

struct Mapper {
    contains: ContainsMapper,
    found: bool,
}

impl Mapper {
    fn transform_case_expr(
        &self,
        base_obj_name: &FlowSmolStr,
        discriminant_name: &FlowSmolStr,
        case: &match_::Case<Loc, Loc, expression::Expression<Loc, Loc>>,
    ) -> match_::Case<Loc, Loc, expression::Expression<Loc, Loc>> {
        let match_::Case {
            loc: case_loc,
            pattern,
            body,
            guard,
            comments,
            invalid_syntax,
            case_match_root_loc,
        } = case;
        if !is_simple_literal_pattern(pattern) {
            return case.clone();
        }
        let mut collector = PropertyAccessCollector {
            acc: BTreeSet::new(),
            base_obj_name: base_obj_name.dupe(),
        };
        let Ok(()) = collector.expression(body);
        let accessed_props = &collector.acc;
        let obj_pattern =
            create_object_pattern(discriminant_name, pattern, accessed_props, case_loc.dupe());
        let new_pattern = match_pattern::MatchPattern::ObjectPattern {
            loc: case_loc.dupe(),
            inner: Arc::new(obj_pattern),
        };
        let as_target =
            match_pattern::as_pattern::Target::Identifier(ast::Identifier::new(IdentifierInner {
                loc: case_loc.dupe(),
                name: base_obj_name.dupe(),
                comments: None,
            }));
        let as_pattern = match_pattern::AsPattern {
            pattern: new_pattern,
            target: as_target,
            comments: None,
        };
        let final_pattern = match_pattern::MatchPattern::AsPattern {
            loc: case_loc.dupe(),
            inner: Arc::new(as_pattern),
        };
        let mut replacer = MemberReplacer {
            base_obj_name: base_obj_name.dupe(),
        };
        let new_body = replacer.map_expression(body);
        match_::Case {
            loc: case_loc.dupe(),
            pattern: final_pattern,
            body: new_body,
            guard: guard.clone(),
            comments: comments.clone(),
            invalid_syntax: invalid_syntax.clone(),
            case_match_root_loc: case_match_root_loc.dupe(),
        }
    }

    fn transform_case_stmt(
        &self,
        base_obj_name: &FlowSmolStr,
        discriminant_name: &FlowSmolStr,
        case: &match_::Case<Loc, Loc, statement::Statement<Loc, Loc>>,
    ) -> match_::Case<Loc, Loc, statement::Statement<Loc, Loc>> {
        let match_::Case {
            loc: case_loc,
            pattern,
            body,
            guard,
            comments,
            invalid_syntax,
            case_match_root_loc,
        } = case;
        if !is_simple_literal_pattern(pattern) {
            return case.clone();
        }
        let mut collector = PropertyAccessCollector {
            acc: BTreeSet::new(),
            base_obj_name: base_obj_name.dupe(),
        };
        let Ok(()) = collector.statement(body);
        let accessed_props = &collector.acc;
        let obj_pattern =
            create_object_pattern(discriminant_name, pattern, accessed_props, case_loc.dupe());
        let new_pattern = match_pattern::MatchPattern::ObjectPattern {
            loc: case_loc.dupe(),
            inner: Arc::new(obj_pattern),
        };
        let as_target =
            match_pattern::as_pattern::Target::Identifier(ast::Identifier::new(IdentifierInner {
                loc: case_loc.dupe(),
                name: base_obj_name.dupe(),
                comments: None,
            }));
        let as_pattern = match_pattern::AsPattern {
            pattern: new_pattern,
            target: as_target,
            comments: None,
        };
        let final_pattern = match_pattern::MatchPattern::AsPattern {
            loc: case_loc.dupe(),
            inner: Arc::new(as_pattern),
        };
        let mut replacer = MemberReplacer {
            base_obj_name: base_obj_name.dupe(),
        };
        let new_body = replacer.map_statement(body);
        match_::Case {
            loc: case_loc.dupe(),
            pattern: final_pattern,
            body: new_body,
            guard: guard.clone(),
            comments: comments.clone(),
            invalid_syntax: invalid_syntax.clone(),
            case_match_root_loc: case_match_root_loc.dupe(),
        }
    }

    fn should_transform<B>(
        &self,
        arg: &expression::Expression<Loc, Loc>,
        cases: &[match_::Case<Loc, Loc, B>],
    ) -> Option<(FlowSmolStr, FlowSmolStr)> {
        match arg.deref() {
            ExpressionInner::Member { inner, .. }
                if let expression::member::Property::PropertyIdentifier(prop_id) =
                    &inner.property =>
            {
                let object = &inner.object;
                let name = &prop_id.name;
                match get_identifier_from_expr(object) {
                    Some(base_name) => {
                        let all_simple = cases
                            .iter()
                            .all(|case| is_simple_literal_pattern(&case.pattern));
                        if all_simple {
                            Some((base_name, name.dupe()))
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            _ => None,
        }
    }
}

impl<'ast> AstVisitor<'ast, Loc> for Mapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_match_expression(
        &mut self,
        loc: &'ast Loc,
        x: &'ast expression::MatchExpression<Loc, Loc>,
    ) -> expression::MatchExpression<Loc, Loc> {
        if self.found {
            return x.clone();
        }
        let match_::Match {
            arg,
            cases,
            match_keyword_loc,
            comments,
        } = x;
        match self.should_transform(arg, cases) {
            Some((base_obj_name, discriminant_name)) if self.contains.target_contained_by(loc) => {
                self.found = true;
                let arg_loc = arg.loc().dupe();
                let new_arg = expression::Expression::new(ExpressionInner::Identifier {
                    loc: arg_loc.dupe(),
                    inner: ast::Identifier::new(IdentifierInner {
                        loc: arg_loc,
                        name: base_obj_name.dupe(),
                        comments: None,
                    }),
                });
                let new_cases: Vec<match_::Case<Loc, Loc, expression::Expression<Loc, Loc>>> =
                    cases
                        .iter()
                        .map(|case| {
                            self.transform_case_expr(&base_obj_name, &discriminant_name, case)
                        })
                        .collect();
                match_::Match {
                    arg: new_arg,
                    cases: Arc::from(new_cases),
                    match_keyword_loc: match_keyword_loc.dupe(),
                    comments: comments.clone(),
                }
            }
            _ => map_match_expression_default(self, loc, x),
        }
    }

    fn map_match_statement(
        &mut self,
        loc: &'ast Loc,
        x: &'ast statement::MatchStatement<Loc, Loc>,
    ) -> statement::MatchStatement<Loc, Loc> {
        if self.found {
            return x.clone();
        }
        let match_::Match {
            arg,
            cases,
            match_keyword_loc,
            comments,
        } = x;
        match self.should_transform(arg, cases) {
            Some((base_obj_name, discriminant_name)) if self.contains.target_contained_by(loc) => {
                self.found = true;
                let arg_loc = arg.loc().dupe();
                let new_arg = expression::Expression::new(ExpressionInner::Identifier {
                    loc: arg_loc.dupe(),
                    inner: ast::Identifier::new(IdentifierInner {
                        loc: arg_loc,
                        name: base_obj_name.dupe(),
                        comments: None,
                    }),
                });
                let new_cases: Vec<match_::Case<Loc, Loc, statement::Statement<Loc, Loc>>> = cases
                    .iter()
                    .map(|case| self.transform_case_stmt(&base_obj_name, &discriminant_name, case))
                    .collect();
                match_::Match {
                    arg: new_arg,
                    cases: Arc::from(new_cases),
                    match_keyword_loc: match_keyword_loc.dupe(),
                    comments: comments.clone(),
                }
            }
            _ => map_match_statement_default(self, loc, x),
        }
    }

    fn map_program(&mut self, program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        if self.contains.should_map_program(program) {
            map_program_default(self, program)
        } else {
            program.clone()
        }
    }

    fn map_statement(
        &mut self,
        stmt: &statement::Statement<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        if self.contains.should_map_statement(stmt) {
            map_statement_default(self, stmt)
        } else {
            stmt.dupe()
        }
    }

    fn map_expression(
        &mut self,
        expr: &expression::Expression<Loc, Loc>,
    ) -> expression::Expression<Loc, Loc> {
        if self.contains.should_map_expression(expr) {
            map_expression_default(self, expr)
        } else {
            expr.dupe()
        }
    }
}

pub fn refactor(ast: &ast::Program<Loc, Loc>, loc: Loc) -> Option<ast::Program<Loc, Loc>> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        found: false,
    };
    let ast_prime = mapper.map_program(ast);
    if !mapper.found || ast_prime == *ast {
        None
    } else {
        Some(ast_prime)
    }
}
