/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;

#[derive(Debug, Clone, Copy)]
pub enum ContextNode {
    Statement,
    ExportDefault,
    Expression,
    ExpressionStatement,
    BindingIdentifier,
    Class,
    Member,
    ObjectKey,
    Type,
    SwitchCase,
    MatchPattern,
}

// TODO: include `of`, `in`, and `instanceof`. We don't currently autocomplete at positions where those are valid.
// true, false, and null are not included here, because we already suggest those when we have type info
pub fn expression_keywords(pattern_matching_enabled: bool) -> Vec<&'static str> {
    let mut result = vec![
        "async", "await", "class", "delete", "function", "import", "new", "typeof", "void", "yield",
    ];
    if pattern_matching_enabled {
        result.push("match");
    }
    result
}

// keywords to suggest in a statement (or expression statement) context, in
// almost-alphabetical order.
//
// the order here only matters amongst items with the same starting letter.
// for example, const is more common than class, which is more common than
// catch or continue; function is more common than finally.
//
// this way, when you type `c` and Quick Suggest triggers, we'll suggest
// [const, class, ...], instead of [case, catch, class, const, continue].
// you can also trigger Quick Suggest without typing anything (cmd+space),
// but that's sorted purely alphabetically.
pub fn statement_keywords(
    component_syntax_enabled: bool,
    pattern_matching_enabled: bool,
    records_enabled: bool,
) -> Vec<&'static str> {
    let mut result = vec![
        "await",
        "async",
        "break",
        "const",
        "class",
        "case",
        "catch",
        "continue",
        "default",
        "debugger",
        "declare",
        "delete",
        "do",
        "else",
        "enum",
        "export",
        "extends",
        "function",
        "for",
        "finally",
        "if",
        "import",
        "implements",
        "interface",
        "let",
        "new",
        "opaque",
        "return",
        "static",
        "switch",
        "throw",
        "try",
        "type",
        "typeof",
        "var",
        "void",
        "while",
        "yield",
    ];
    if component_syntax_enabled {
        result.push("component");
        result.push("hook");
    }
    if pattern_matching_enabled {
        result.push("match");
    }
    if records_enabled {
        result.push("record");
    }
    result
}

// keywords that can appear after export default
pub fn export_default_keywords(component_syntax_enabled: bool) -> Vec<&'static str> {
    let mut result = vec!["async", "class", "function", "enum", "new"];
    if component_syntax_enabled {
        result.push("component");
        result.push("hook");
    }
    result
}

pub const MATCH_PATTERN_KEYWORDS: &[&str] = &["const"];

enum Found {
    Found(Vec<ContextNode>),
}

struct Mapper {
    target: Loc,
    context: Vec<ContextNode>,
}

impl Mapper {
    fn new(target: Loc) -> Self {
        Mapper {
            target,
            context: Vec::new(),
        }
    }

    fn target_contained_by(&self, loc: &Loc) -> bool {
        loc.contains(&self.target)
    }

    fn with_context<F>(&mut self, node: ContextNode, f: F) -> Result<(), Found>
    where
        F: FnOnce(&mut Self) -> Result<(), Found>,
    {
        self.context.push(node);
        let result = f(self);
        if result.is_ok() {
            self.context.pop();
        }
        result
    }
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Found> for Mapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn program(&mut self, program: &'ast ast::Program<Loc, Loc>) -> Result<(), Found> {
        if self.target_contained_by(&program.loc)
            || program
                .all_comments
                .iter()
                .any(|c| self.target_contained_by(&c.loc))
        {
            ast_visitor::program_default(self, program)
        } else {
            Ok(())
        }
    }

    fn expression(
        &mut self,
        expr: &'ast ast::expression::Expression<Loc, Loc>,
    ) -> Result<(), Found> {
        let loc = expr.loc();
        if self.target_contained_by(loc) {
            self.with_context(ContextNode::Expression, |this| {
                ast_visitor::expression_default(this, expr)
            })
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, stmt: &'ast ast::statement::Statement<Loc, Loc>) -> Result<(), Found> {
        let loc = stmt.loc();
        if self.target_contained_by(loc) {
            self.with_context(ContextNode::Statement, |this| {
                ast_visitor::statement_default(this, stmt)
            })
        } else {
            Ok(())
        }
    }

    fn comment(&mut self, _comment: &'ast ast::Comment<Loc>) -> Result<(), Found> {
        Ok(())
    }

    fn export_default_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::statement::ExportDefaultDeclaration<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::ExportDefault, |this| {
            ast_visitor::export_default_declaration_default(this, loc, decl)
        })
    }

    fn expression_statement(
        &mut self,
        loc: &'ast Loc,
        stmt: &'ast ast::statement::Expression<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::ExpressionStatement, |this| {
            ast_visitor::expression_statement_default(this, loc, stmt)
        })
    }

    fn binding_type_identifier(
        &mut self,
        id: &'ast ast::Identifier<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::BindingIdentifier, |this| {
            ast_visitor::binding_type_identifier_default(this, id)
        })
    }

    fn pattern_identifier(
        &mut self,
        kind: Option<ast::VariableKind>,
        id: &'ast ast::Identifier<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::BindingIdentifier, |this| {
            ast_visitor::pattern_identifier_default(this, kind, id)
        })
    }

    fn class_(
        &mut self,
        loc: &'ast Loc,
        cls: &'ast ast::class::Class<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::Class, |this| {
            ast_visitor::class_default(this, loc, cls)
        })
    }

    fn member_property(
        &mut self,
        prop: &'ast ast::expression::member::Property<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::Member, |this| {
            ast_visitor::member_property_default(this, prop)
        })
    }

    fn object_key(
        &mut self,
        key: &'ast ast::expression::object::Key<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::ObjectKey, |this| {
            ast_visitor::object_key_default(this, key)
        })
    }

    fn type_(&mut self, t: &'ast ast::types::Type<Loc, Loc>) -> Result<(), Found> {
        self.with_context(ContextNode::Type, |this| ast_visitor::type_default(this, t))
    }

    fn switch_case(
        &mut self,
        case: &'ast ast::statement::switch::Case<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::SwitchCase, |this| {
            ast_visitor::switch_case_default(this, case)
        })
    }

    fn match_pattern(
        &mut self,
        pattern: &'ast ast::match_pattern::MatchPattern<Loc, Loc>,
    ) -> Result<(), Found> {
        self.with_context(ContextNode::MatchPattern, |this| {
            ast_visitor::match_pattern_default(this, pattern)
        })
    }

    fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), Found> {
        if self.target_contained_by(&id.loc) {
            return Err(Found::Found(self.context.clone()));
        }
        ast_visitor::identifier_default(self, id)
    }
}

pub fn keywords_of_context(
    component_syntax_enabled: bool,
    pattern_matching_enabled: bool,
    records_enabled: bool,
    context: &[ContextNode],
) -> Vec<&'static str> {
    match context {
        [
            ..,
            ContextNode::ExpressionStatement,
            ContextNode::Expression,
        ]
        | [.., ContextNode::Statement] => statement_keywords(
            component_syntax_enabled,
            pattern_matching_enabled,
            records_enabled,
        ),
        [.., ContextNode::ExportDefault]
        | [.., ContextNode::ExportDefault, ContextNode::Expression] => {
            export_default_keywords(component_syntax_enabled)
        }
        [.., ContextNode::Member, ContextNode::Expression]
        | [.., ContextNode::SwitchCase, ContextNode::Expression] => vec![],
        [.., ContextNode::Expression] => expression_keywords(pattern_matching_enabled),
        [.., ContextNode::MatchPattern] if pattern_matching_enabled => {
            MATCH_PATTERN_KEYWORDS.to_vec()
        }
        _ => vec![],
    }
}

// We're looking for an identifier, considering the first character is equivalent.
pub fn keywords_at_loc(
    component_syntax_enabled: bool,
    pattern_matching_enabled: bool,
    records_enabled: bool,
    program: &ast::Program<Loc, Loc>,
    loc: &Loc,
) -> Vec<&'static str> {
    let target = loc.first_char();
    let mut mapper = Mapper::new(target);
    match mapper.program(program) {
        Ok(()) => vec![],
        Err(Found::Found(context)) => keywords_of_context(
            component_syntax_enabled,
            pattern_matching_enabled,
            records_enabled,
            &context,
        ),
    }
}
