/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::VariableKind;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::match_;
use flow_parser::ast::match_pattern;
use flow_parser::ast::match_pattern::object_pattern;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_match_default;
use flow_parser::ast_visitor::map_match_expression_default;
use flow_parser::ast_visitor::map_match_object_pattern_property_default;
use flow_parser::ast_visitor::map_match_pattern_default;
use flow_parser::ast_visitor::map_match_statement_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::js_id_unicode;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

pub enum Kind {
    ObjShorthandToConst,
    ObjShorthandToReference,
    InvalidMatchStatementBody,
    InvalidBindingKind,
    InvalidWildcardSyntax,
    InvalidCaseSyntax,
    NonExhaustiveObjectPattern {
        add_rest: bool,
        missing_props: Vec<String>,
    },
    NotExhaustive(Vec<match_pattern::MatchPattern<Loc, Loc>>),
    UnusedPattern,
}

fn is_valid_ident_name(name: &str) -> bool {
    js_id_unicode::string_is_valid_identifier_name(name)
}

fn is_invalid_case_syntax(invalid_syntax: &match_::InvalidSyntax<Loc>) -> bool {
    let match_::InvalidSyntax {
        invalid_prefix_case,
        invalid_infix_colon,
        invalid_suffix_semicolon,
    } = invalid_syntax;
    invalid_prefix_case.is_some()
        || invalid_infix_colon.is_some()
        || invalid_suffix_semicolon.is_some()
}

fn empty_invalid_case_syntax() -> match_::InvalidSyntax<Loc> {
    match_::InvalidSyntax {
        invalid_prefix_case: None,
        invalid_infix_colon: None,
        invalid_suffix_semicolon: None,
    }
}

struct Mapper {
    contains: ContainsMapper,
    kind: Kind,
    fix_all_invalid_case_syntax: bool,
}

impl<'ast> AstVisitor<'ast, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_match_object_pattern_property(
        &mut self,
        prop: &object_pattern::Property<Loc, Loc>,
    ) -> object_pattern::Property<Loc, Loc> {
        let loc = prop.loc();
        if !self.contains.is_target(loc) {
            return map_match_object_pattern_property_default(self, prop);
        }
        match prop {
            object_pattern::Property::Valid { .. } => {
                map_match_object_pattern_property_default(self, prop)
            }
            object_pattern::Property::InvalidShorthand {
                loc: _,
                identifier: id,
            } => {
                let key = object_pattern::Key::Identifier(id.dupe());
                match &self.kind {
                    Kind::ObjShorthandToConst => {
                        let pattern = match_pattern::MatchPattern::BindingPattern {
                            loc: LOC_NONE,
                            inner: Arc::new(match_pattern::BindingPattern {
                                kind: VariableKind::Const,
                                id: id.dupe(),
                                comments: None,
                            }),
                        };
                        object_pattern::Property::Valid {
                            loc: LOC_NONE,
                            property: object_pattern::PropertyStruct {
                                key,
                                pattern,
                                shorthand: true,
                                comments: None,
                            },
                        }
                    }
                    Kind::ObjShorthandToReference => {
                        let pattern = match_pattern::MatchPattern::IdentifierPattern {
                            loc: LOC_NONE,
                            inner: id.dupe(),
                        };
                        object_pattern::Property::Valid {
                            loc: LOC_NONE,
                            property: object_pattern::PropertyStruct {
                                key,
                                pattern,
                                shorthand: false,
                                comments: None,
                            },
                        }
                    }
                    _ => map_match_object_pattern_property_default(self, prop),
                }
            }
        }
    }

    fn map_match_statement(
        &mut self,
        loc: &Loc,
        m: &statement::MatchStatement<Loc, Loc>,
    ) -> statement::MatchStatement<Loc, Loc> {
        let match_keyword_loc = &m.match_keyword_loc;
        match &self.kind {
            Kind::InvalidMatchStatementBody if self.contains.target_contained_by(loc) => self
                .map_match_(loc, m, |visitor, stmt| {
                    let stmt_loc = stmt.loc();
                    if !visitor.contains.is_target(stmt_loc) {
                        visitor.map_statement(stmt)
                    } else {
                        let wrapped = statement::Statement::new(StatementInner::Block {
                            loc: LOC_NONE,
                            inner: Arc::new(statement::Block {
                                body: Arc::from([stmt.dupe()]),
                                comments: None,
                            }),
                        });
                        visitor.map_statement(&wrapped)
                    }
                }),
            Kind::NotExhaustive(patterns_to_add) if self.contains.is_target(match_keyword_loc) => {
                let match_::Match {
                    arg,
                    cases,
                    match_keyword_loc,
                    comments,
                } = m;
                let new_cases: Vec<match_::Case<Loc, Loc, statement::Statement<Loc, Loc>>> =
                    patterns_to_add
                        .iter()
                        .map(|pattern| {
                            let body = statement::Statement::new(StatementInner::Block {
                                loc: LOC_NONE,
                                inner: Arc::new(statement::Block {
                                    body: Arc::from([]),
                                    comments: None,
                                }),
                            });
                            match_::Case {
                                loc: LOC_NONE,
                                pattern: pattern.clone(),
                                body,
                                guard: None,
                                comments: None,
                                invalid_syntax: empty_invalid_case_syntax(),
                                case_match_root_loc: LOC_NONE,
                            }
                        })
                        .collect();
                let mut all_cases: Vec<_> = cases.iter().cloned().collect();
                all_cases.extend(new_cases);
                let new_m = match_::Match {
                    arg: arg.dupe(),
                    cases: Arc::from(all_cases),
                    match_keyword_loc: match_keyword_loc.dupe(),
                    comments: comments.clone(),
                };
                map_match_statement_default(self, loc, &new_m)
            }
            _ => map_match_statement_default(self, loc, m),
        }
    }

    fn map_match_expression(
        &mut self,
        loc: &Loc,
        m: &ast::expression::MatchExpression<Loc, Loc>,
    ) -> ast::expression::MatchExpression<Loc, Loc> {
        let match_keyword_loc = &m.match_keyword_loc;
        match &self.kind {
            Kind::NotExhaustive(patterns_to_add) if self.contains.is_target(match_keyword_loc) => {
                let match_::Match {
                    arg,
                    cases,
                    match_keyword_loc,
                    comments,
                } = m;
                let new_cases: Vec<match_::Case<Loc, Loc, ast::expression::Expression<Loc, Loc>>> =
                    patterns_to_add
                        .iter()
                        .map(|pattern| {
                            let body =
                                ast::expression::Expression::new(ExpressionInner::Identifier {
                                    loc: LOC_NONE,
                                    inner: ast::Identifier::new(IdentifierInner {
                                        loc: LOC_NONE,
                                        name: "undefined".into(),
                                        comments: None,
                                    }),
                                });
                            match_::Case {
                                loc: LOC_NONE,
                                pattern: pattern.clone(),
                                body,
                                guard: None,
                                comments: None,
                                invalid_syntax: empty_invalid_case_syntax(),
                                case_match_root_loc: LOC_NONE,
                            }
                        })
                        .collect();
                let mut all_cases: Vec<_> = cases.iter().cloned().collect();
                all_cases.extend(new_cases);
                let new_m = match_::Match {
                    arg: arg.dupe(),
                    cases: Arc::from(all_cases),
                    match_keyword_loc: match_keyword_loc.dupe(),
                    comments: comments.clone(),
                };
                map_match_expression_default(self, loc, &new_m)
            }
            _ => map_match_expression_default(self, loc, m),
        }
    }

    fn map_match_pattern(
        &mut self,
        pattern: &match_pattern::MatchPattern<Loc, Loc>,
    ) -> match_pattern::MatchPattern<Loc, Loc> {
        let pattern = match (pattern, &self.kind) {
            (
                match_pattern::MatchPattern::BindingPattern {
                    loc,
                    inner: binding,
                },
                Kind::InvalidBindingKind,
            ) if self.contains.is_target(loc) => {
                let match_pattern::BindingPattern {
                    kind: _,
                    id,
                    comments,
                } = &**binding;
                match_pattern::MatchPattern::BindingPattern {
                    loc: loc.dupe(),
                    inner: Arc::new(match_pattern::BindingPattern {
                        kind: VariableKind::Const,
                        id: id.dupe(),
                        comments: comments.clone(),
                    }),
                }
            }
            (
                match_pattern::MatchPattern::WildcardPattern {
                    loc,
                    inner: wildcard,
                },
                Kind::InvalidWildcardSyntax,
            ) if self.contains.is_target(loc) => {
                let match_pattern::WildcardPattern {
                    comments,
                    invalid_syntax_default_keyword: _,
                } = wildcard;
                match_pattern::MatchPattern::WildcardPattern {
                    loc: loc.dupe(),
                    inner: match_pattern::WildcardPattern {
                        comments: comments.clone(),
                        invalid_syntax_default_keyword: false,
                    },
                }
            }
            (
                match_pattern::MatchPattern::ObjectPattern { loc, inner: obj },
                Kind::NonExhaustiveObjectPattern {
                    add_rest,
                    missing_props,
                },
            ) if self.contains.is_target(loc) => {
                let match_pattern::ObjectPattern {
                    properties,
                    rest,
                    comments,
                } = &**obj;
                let rest = if *add_rest {
                    Some(match_pattern::RestPattern {
                        loc: LOC_NONE,
                        argument: None,
                        comments: None,
                    })
                } else {
                    rest.clone()
                };
                let properties = if missing_props.is_empty() {
                    properties.to_vec()
                } else {
                    let added_props: Vec<object_pattern::Property<Loc, Loc>> = missing_props
                        .iter()
                        .map(|name| {
                            let key = if is_valid_ident_name(name) {
                                object_pattern::Key::Identifier(ast::Identifier::new(
                                    IdentifierInner {
                                        loc: LOC_NONE,
                                        name: name.as_str().into(),
                                        comments: None,
                                    },
                                ))
                            } else {
                                object_pattern::Key::StringLiteral((
                                    LOC_NONE,
                                    ast::StringLiteral {
                                        value: name.as_str().into(),
                                        raw: "".into(),
                                        comments: None,
                                    },
                                ))
                            };
                            let pattern = match_pattern::MatchPattern::WildcardPattern {
                                loc: LOC_NONE,
                                inner: match_pattern::WildcardPattern {
                                    comments: None,
                                    invalid_syntax_default_keyword: false,
                                },
                            };
                            object_pattern::Property::Valid {
                                loc: LOC_NONE,
                                property: object_pattern::PropertyStruct {
                                    key,
                                    pattern,
                                    shorthand: false,
                                    comments: None,
                                },
                            }
                        })
                        .collect();
                    let mut result = properties.to_vec();
                    result.extend(added_props);
                    result
                };
                match_pattern::MatchPattern::ObjectPattern {
                    loc: loc.dupe(),
                    inner: Arc::new(match_pattern::ObjectPattern {
                        properties: Arc::from(properties),
                        rest,
                        comments: comments.clone(),
                    }),
                }
            }
            (
                match_pattern::MatchPattern::ObjectPattern { loc, inner: obj },
                Kind::UnusedPattern,
            ) if obj
                .rest
                .as_ref()
                .is_some_and(|r| self.contains.is_target(&r.loc)) =>
            {
                let match_pattern::ObjectPattern {
                    properties,
                    rest: _,
                    comments,
                } = &**obj;
                match_pattern::MatchPattern::ObjectPattern {
                    loc: loc.dupe(),
                    inner: Arc::new(match_pattern::ObjectPattern {
                        properties: properties.dupe(),
                        rest: None,
                        comments: comments.clone(),
                    }),
                }
            }
            (
                match_pattern::MatchPattern::ArrayPattern { loc, inner: arr },
                Kind::UnusedPattern,
            ) if arr
                .rest
                .as_ref()
                .is_some_and(|r| self.contains.is_target(&r.loc)) =>
            {
                let match_pattern::ArrayPattern {
                    elements,
                    rest: _,
                    comments,
                } = &**arr;
                match_pattern::MatchPattern::ArrayPattern {
                    loc: loc.dupe(),
                    inner: Arc::new(match_pattern::ArrayPattern {
                        elements: elements.dupe(),
                        rest: None,
                        comments: comments.clone(),
                    }),
                }
            }
            (
                match_pattern::MatchPattern::OrPattern { loc, inner: or_pat },
                Kind::UnusedPattern,
            ) if self.contains.target_contained_by(loc) => {
                let match_pattern::OrPattern { patterns, comments } = &**or_pat;
                let (mut patterns_rev, changed) = patterns.iter().fold(
                    (Vec::new(), false),
                    |(mut patterns_rev, changed), pat| {
                        let pattern_loc = pat.loc();
                        if self.contains.is_target(pattern_loc) {
                            (patterns_rev, true)
                        } else if changed {
                            let mut new_pat = pat.clone();
                            *new_pat.loc_mut() = LOC_NONE;
                            patterns_rev.push(new_pat);
                            (patterns_rev, changed)
                        } else {
                            patterns_rev.push(pat.clone());
                            (patterns_rev, changed)
                        }
                    },
                );
                if changed {
                    patterns_rev.reverse();
                    match patterns_rev.len() {
                        1 => patterns_rev.into_iter().next().unwrap(),
                        _ => match_pattern::MatchPattern::OrPattern {
                            loc: loc.dupe(),
                            inner: Arc::new(match_pattern::OrPattern {
                                patterns: Arc::from(patterns_rev),
                                comments: comments.clone(),
                            }),
                        },
                    }
                } else {
                    pattern.clone()
                }
            }
            _ => pattern.clone(),
        };
        map_match_pattern_default(self, &pattern)
    }

    fn map_match_<B, B2>(
        &mut self,
        loc: &'ast Loc,
        m: &'ast match_::Match<Loc, Loc, B>,
        mut on_case_body: impl FnMut(&mut Self, &'ast B) -> B2,
    ) -> match_::Match<Loc, Loc, B2>
    where
        Loc: Dupe,
    {
        let match_keyword_loc = &m.match_keyword_loc;
        match &self.kind {
            Kind::InvalidCaseSyntax if self.contains.is_target(match_keyword_loc) => {
                self.fix_all_invalid_case_syntax = true;
                let result = map_match_default(self, loc, m, on_case_body);
                self.fix_all_invalid_case_syntax = false;
                result
            }
            Kind::UnusedPattern if self.contains.target_contained_by(loc) => {
                let match_::Match {
                    arg,
                    cases,
                    match_keyword_loc,
                    comments,
                } = m;
                let mut keep_info: Vec<(usize, bool)> = Vec::new(); // (index, reset_loc)
                let mut changed = false;
                for (i, case) in cases.iter().enumerate() {
                    let pattern_loc = case.pattern.loc();
                    if self.contains.is_target(pattern_loc) {
                        changed = true;
                    } else if changed {
                        keep_info.push((i, true));
                    } else {
                        keep_info.push((i, false));
                    }
                }
                if changed {
                    let arg_ = self.map_expression(arg);
                    let cases_: Arc<[match_::Case<Loc, Loc, B2>]> = keep_info
                        .iter()
                        .map(|&(idx, reset_loc)| {
                            let mut case_b2 = self.map_match_case(&cases[idx], &mut on_case_body);
                            if reset_loc {
                                case_b2.loc = LOC_NONE;
                            }
                            case_b2
                        })
                        .collect();
                    let comments_ = self.map_syntax_opt(comments.as_ref());
                    match_::Match {
                        arg: arg_,
                        cases: cases_,
                        match_keyword_loc: match_keyword_loc.dupe(),
                        comments: comments_,
                    }
                } else {
                    map_match_default(self, loc, m, on_case_body)
                }
            }
            _ => map_match_default(self, loc, m, on_case_body),
        }
    }

    fn map_match_case<B, B2>(
        &mut self,
        case: &'ast match_::Case<Loc, Loc, B>,
        on_case_body: &mut impl FnMut(&mut Self, &'ast B) -> B2,
    ) -> match_::Case<Loc, Loc, B2>
    where
        Loc: Dupe,
    {
        let match_::Case {
            loc,
            pattern,
            body,
            guard,
            comments,
            invalid_syntax,
            case_match_root_loc,
        } = case;
        let use_empty_invalid_syntax = matches!(self.kind, Kind::InvalidCaseSyntax)
            && (self.fix_all_invalid_case_syntax || self.contains.target_contained_by(loc))
            && is_invalid_case_syntax(invalid_syntax);
        let pattern_ = self.map_match_pattern(pattern);
        let body_ = on_case_body(self, body);
        let guard_ = guard.as_ref().map(|g| self.map_expression(g));
        let comments_ = self.map_syntax_opt(comments.as_ref());
        match_::Case {
            loc: loc.dupe(),
            pattern: pattern_,
            body: body_,
            guard: guard_,
            comments: comments_,
            invalid_syntax: if use_empty_invalid_syntax {
                empty_invalid_case_syntax()
            } else {
                invalid_syntax.clone()
            },
            case_match_root_loc: case_match_root_loc.dupe(),
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
        stmt: &ast::statement::Statement<Loc, Loc>,
    ) -> ast::statement::Statement<Loc, Loc> {
        if self.contains.should_map_statement(stmt) {
            map_statement_default(self, stmt)
        } else {
            stmt.dupe()
        }
    }
}

pub fn convert_object_shorthand_to_const(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::ObjShorthandToConst,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn convert_object_shorthand_to_reference(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::ObjShorthandToReference,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_invalid_match_statement_body(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::InvalidMatchStatementBody,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_invalid_binding_kind(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::InvalidBindingKind,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_invalid_wildcard_syntax(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::InvalidWildcardSyntax,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_invalid_case_syntax(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::InvalidCaseSyntax,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_non_exhaustive_object_pattern(
    add_rest: bool,
    missing_props: Vec<String>,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::NonExhaustiveObjectPattern {
            add_rest,
            missing_props,
        },
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn fix_not_exhaustive(
    patterns_to_add: Vec<match_pattern::MatchPattern<Loc, Loc>>,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::NotExhaustive(patterns_to_add),
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}

pub fn remove_unused_pattern(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::UnusedPattern,
        fix_all_invalid_case_syntax: false,
    };
    mapper.map_program(ast)
}
