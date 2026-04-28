/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(clippy::result_large_err)]

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::flow_import_specifier;
use flow_common::reason;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::statement;
use flow_parser::ast::types;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::TypeParamsContext;
use flow_parser::loc::Loc;
use flow_typing::merge;
use flow_typing::typed_ast_finder;
use flow_typing::typed_ast_finder::EnclosingNode;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_js::type_inference_hooks_js;
use flow_typing_statement::match_pattern;
use flow_typing_statement::statement as typing_statement;
use flow_typing_type::type_;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::Type;
use flow_typing_utils::abnormal::CheckExprError;

use crate::autocomplete_sigil;

thread_local! {
    static AUTOCOMPLETE_CURSOR: RefCell<Option<Loc>> = const { RefCell::new(None) };
}

pub struct BracketSyntax {
    pub include_super: bool,
    pub include_this: bool,
    pub type_: Type,
}

pub enum AutocompleteType {
    AcIgnored,
    AcBinding,
    AcComment {
        text: String,
        loc: ALoc,
    },
    AcId {
        include_super: bool,
        include_this: bool,
        type_: Type,
        enclosing_class_t: Option<Type>,
    },
    AcClassKey {
        enclosing_class_t: Option<Type>,
    },
    AcEnum,
    AcImportSpecifier {
        module_type_opt: Option<ModuleType>,
        used_keys: BTreeSet<String>,
        is_type: bool,
    },
    AcKey {
        obj_type: Type,
        used_keys: BTreeSet<String>,
        spreads: Vec<(Loc, Type)>,
    },
    AcLiteral {
        lit_type: Option<Type>,
    },
    AcModule,
    AcType,
    AcTypeBinding,
    AcQualifiedType(Type),
    AcMember {
        obj_type: Type,
        in_optional_chain: bool,
        bracket_syntax: Option<BracketSyntax>,
        member_loc: Option<Loc>,
        is_type_annotation: bool,
        is_super: bool,
    },
    AcJsxElement {
        type_: Type,
    },
    AcJsxAttribute {
        attribute_name: String,
        used_attr_names: BTreeSet<String>,
        component_t: Type,
        has_value: bool,
    },
    AcJsxText,
    AcRecordField {
        field_name: String,
        used_field_names: BTreeSet<String>,
        record_t: Type,
        has_value: bool,
    },
}

pub struct ProcessLocationResult {
    pub tparams_rev: Vec<String>,
    pub ac_loc: ALoc,
    pub token: String,
    pub autocomplete_type: AutocompleteType,
}

fn type_of_jsx_name(name: &ast::jsx::Name<ALoc, (ALoc, Type)>) -> Type {
    match name {
        ast::jsx::Name::Identifier(id) => id.loc.1.dupe(),
        ast::jsx::Name::NamespacedName(ns) => ns.name.loc.1.dupe(),
        ast::jsx::Name::MemberExpression(me) => me.property.loc.1.dupe(),
    }
}

fn loc_of_qualification(id: &types::generic::Identifier<ALoc, ALoc>) -> ALoc {
    match id {
        types::generic::Identifier::Unqualified(id) => id.loc.dupe(),
        types::generic::Identifier::Qualified(q) => q.id.loc.dupe(),
        types::generic::Identifier::ImportTypeAnnot(i) => i.loc.dupe(),
    }
}

fn loc_of_typeof(target: &types::typeof_::Target<ALoc, ALoc>) -> ALoc {
    match target {
        types::typeof_::Target::Unqualified(id) => id.loc.dupe(),
        types::typeof_::Target::Qualified(q) => q.loc.dupe(),
        types::typeof_::Target::Import(i) => i.loc.dupe(),
    }
}

fn compute_member_loc(expr_loc: &ALoc, obj_loc: &ALoc) -> Loc {
    let expr_loc = expr_loc.to_loc_exn();
    let obj_loc = obj_loc.to_loc_exn();
    Loc {
        source: obj_loc.source.dupe(),
        start: obj_loc.end,
        end: expr_loc.end,
    }
}

fn covers_target(cursor: &Loc, loc: &ALoc) -> bool {
    reason::in_range(cursor, loc.to_loc_exn())
}

fn extract_word(cursor_loc: &Loc, text: &str) -> (String, Loc) {
    match autocomplete_sigil::split_opt(text) {
        None => ("".to_string(), Loc::default()),
        Some((before, after)) => {
            let before_trimmed = before
                .char_indices()
                .rev()
                .find(|(_, c)| c.is_whitespace())
                .map(|(idx, c)| idx + c.len_utf8())
                .map(|idx| before[idx..].to_string())
                .unwrap_or(before);
            let after_trimmed = after
                .char_indices()
                .find(|(_, c)| c.is_whitespace())
                .map(|(idx, _)| after[..idx].to_string())
                .unwrap_or(after);
            let word_loc = Loc {
                source: cursor_loc.source.dupe(),
                start: flow_parser::loc::Position {
                    line: cursor_loc.start.line,
                    column: cursor_loc.start.column - before_trimmed.len() as i32,
                },
                end: flow_parser::loc::Position {
                    line: cursor_loc.end.line,
                    column: cursor_loc.end.column + after_trimmed.len() as i32,
                },
            };
            (format!("{}{}", before_trimmed, after_trimmed), word_loc)
        }
    }
}

enum Found {
    Found(ProcessLocationResult),
    InternalExn(String),
    Canceled(flow_utils_concurrency::worker_cancel::WorkerCanceled),
    TimedOut(flow_utils_concurrency::job_error::CheckTimeout),
    DebugThrow { loc: ALoc },
}

impl From<flow_utils_concurrency::worker_cancel::WorkerCanceled> for Found {
    fn from(e: flow_utils_concurrency::worker_cancel::WorkerCanceled) -> Self {
        Found::Canceled(e)
    }
}

impl From<flow_utils_concurrency::job_error::CheckTimeout> for Found {
    fn from(e: flow_utils_concurrency::job_error::CheckTimeout) -> Self {
        Found::TimedOut(e)
    }
}

impl From<flow_utils_concurrency::job_error::JobError> for Found {
    fn from(e: flow_utils_concurrency::job_error::JobError) -> Self {
        match e {
            flow_utils_concurrency::job_error::JobError::Canceled(c) => Found::Canceled(c),
            flow_utils_concurrency::job_error::JobError::TimedOut(t) => Found::TimedOut(t),
            flow_utils_concurrency::job_error::JobError::DebugThrow { loc } => {
                Found::DebugThrow { loc }
            }
        }
    }
}

struct Inference;

impl Inference {
    fn type_of_component_name_of_jsx_element(
        cx: &Context,
        loc: &ALoc,
        expr: &ast::jsx::Element<ALoc, ALoc>,
    ) -> Result<Type, Found> {
        let typed = typing_statement::expression(
            None,
            None,
            None,
            cx,
            &expression::Expression::new(expression::ExpressionInner::JSXElement {
                loc: loc.dupe(),
                inner: expr.clone().into(),
            }),
        )
        .map_err(|_| Found::InternalExn("typed AST structure mismatch".to_string()))?;
        match typed.deref() {
            expression::ExpressionInner::JSXElement { inner, .. } => {
                Ok(type_of_jsx_name(&inner.opening_element.name))
            }
            _ => Err(Found::InternalExn(
                "typed AST structure mismatch".to_string(),
            )),
        }
    }

    fn type_of_expression(
        cx: &Context,
        expr: &expression::Expression<ALoc, ALoc>,
    ) -> Result<Type, Found> {
        let typed = typing_statement::expression(None, None, None, cx, expr)
            .map_err(|_| Found::InternalExn("typed AST structure mismatch".to_string()))?;
        Ok(typed.loc().1.dupe())
    }

    fn type_of_class_id(
        cx: &Context,
        loc: &ALoc,
        cls: &ast::class::Class<ALoc, ALoc>,
    ) -> Result<Type, Found> {
        let typed = typing_statement::statement(
            cx,
            &statement::Statement::new(statement::StatementInner::ClassDeclaration {
                loc: loc.dupe(),
                inner: cls.clone().into(),
            }),
        )
        .map_err(|_| Found::InternalExn("typed AST structure mismatch".to_string()))?;
        match typed.deref() {
            statement::StatementInner::ClassDeclaration { inner, .. } => match &inner.id {
                Some(id) => Ok(id.loc.1.dupe()),
                None => Err(Found::InternalExn(
                    "typed AST structure mismatch".to_string(),
                )),
            },
            _ => Err(Found::InternalExn(
                "typed AST structure mismatch".to_string(),
            )),
        }
    }

    fn type_of_identifier(
        cx: &Context,
        loc: &ALoc,
        id: &ast::Identifier<ALoc, ALoc>,
    ) -> Result<Type, flow_utils_concurrency::job_error::JobError> {
        typing_statement::identifier(EnclosingContext::NoContext, cx, id, loc.dupe())
    }

    fn type_of_match_member_pattern(
        cx: &Context,
        mem: &ast::match_pattern::MemberPattern<ALoc, ALoc>,
    ) -> Result<Type, Found> {
        match_pattern::type_of_member_pattern(
            cx,
            &|_encl_ctx, cx, id, loc| {
                let id = ast::Identifier(Arc::new(id.clone()));
                typing_statement::identifier(EnclosingContext::NoContext, cx, &id, loc)
            },
            &|cx, expr| typing_statement::expression(None, None, None, cx, expr),
            mem,
        )
        .map_err(|err| match err {
            CheckExprError::Canceled(c) => Found::Canceled(c),
            CheckExprError::TimedOut(t) => Found::TimedOut(t),
            CheckExprError::DebugThrow { loc } => Found::DebugThrow { loc },
            _ => Found::InternalExn("typed AST structure mismatch".to_string()),
        })
    }
}

struct ProcessRequestSearcher<'a, 'cx> {
    cx: &'a Context<'cx>,
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
    from_trigger_character: bool,
    cursor: &'a Loc,
    enclosing_classes: Vec<Type>,
    rev_bound_tparams: Vec<String>,
    enclosing_node_stack: Vec<EnclosingNode<ALoc, ALoc>>,
    class_key_depth: usize,
}

impl<'a, 'cx> ProcessRequestSearcher<'a, 'cx> {
    fn new(
        cx: &'a Context<'cx>,
        file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
        from_trigger_character: bool,
        cursor: &'a Loc,
    ) -> Self {
        Self {
            cx,
            file_sig,
            from_trigger_character,
            cursor,
            enclosing_classes: Vec::new(),
            rev_bound_tparams: Vec::new(),
            enclosing_node_stack: Vec::new(),
            class_key_depth: 0,
        }
    }

    fn covers_target(&self, loc: &ALoc) -> bool {
        covers_target(self.cursor, loc)
    }

    fn make_typeparam(&self, tparam: &types::TypeParam<ALoc, ALoc>) -> String {
        tparam.name.name.to_string()
    }

    fn get_enclosing_class(&self) -> Option<Type> {
        self.enclosing_classes.last().cloned()
    }

    fn check_closest_enclosing_statement(&self) -> Result<(), Found> {
        let stmt = self
            .enclosing_node_stack
            .iter()
            .rev()
            .find_map(|node| match node {
                EnclosingNode::EnclosingStatement(stmt) => Some(stmt),
                _ => None,
            });
        if let Some(stmt) = stmt {
            let typed_stmt = typing_statement::statement(self.cx, stmt)
                .map_err(|_| Found::InternalExn("typed AST structure mismatch".to_string()))?;
            let ast = ast::Program {
                loc: stmt.loc().dupe(),
                statements: vec![stmt.clone()].into(),
                interpreter: None,
                comments: None,
                all_comments: vec![].into(),
            };
            let tast = ast::Program {
                loc: stmt.loc().dupe(),
                statements: vec![typed_stmt].into(),
                interpreter: None,
                comments: None,
                all_comments: vec![].into(),
            };
            merge::post_merge_checks(
                self.cx,
                self.file_sig.dupe(),
                &ast,
                &tast,
                self.cx.metadata(),
            )?;
        }
        Ok(())
    }

    fn type_from_enclosing_node(&self, loc: &ALoc) -> Result<Type, Found> {
        let node = self
            .enclosing_node_stack
            .last()
            .expect("enclosing_node_stack is empty")
            .clone();
        let typed_node = typed_ast_finder::infer_node(self.cx, node).map_err(|e| match e {
            flow_typing_utils::abnormal::CheckExprError::Canceled(c) => Found::Canceled(c),
            flow_typing_utils::abnormal::CheckExprError::TimedOut(t) => Found::TimedOut(t),
            flow_typing_utils::abnormal::CheckExprError::DebugThrow { loc } => {
                Found::DebugThrow { loc }
            }
            flow_typing_utils::abnormal::CheckExprError::Abnormal(_) => {
                Found::InternalExn("typed AST structure mismatch".to_string())
            }
        })?;
        match typed_ast_finder::find_type_annot_in_node(loc.dupe(), &typed_node) {
            Some(t) => Ok(t),
            None => Err(Found::InternalExn("enclosing loc missing".to_string())),
        }
    }

    fn default_ac_id(&self, type_: Type) -> AutocompleteType {
        AutocompleteType::AcId {
            include_super: false,
            include_this: false,
            type_,
            enclosing_class_t: self.get_enclosing_class(),
        }
    }

    fn default_bracket_syntax(&self, type_: Type) -> BracketSyntax {
        BracketSyntax {
            include_super: false,
            include_this: false,
            type_,
        }
    }

    fn annot_with_tparams<F>(&self, f: F) -> Result<(), Found>
    where
        F: FnOnce(Vec<String>) -> Result<(), Found>,
    {
        f(self.rev_bound_tparams.clone())
    }

    fn find(
        &self,
        ac_loc: ALoc,
        token: String,
        autocomplete_type: AutocompleteType,
    ) -> Result<(), Found> {
        let autocomplete_type = match autocomplete_type {
            AutocompleteType::AcId { .. } | AutocompleteType::AcType
                if self.from_trigger_character =>
            {
                AutocompleteType::AcIgnored
            }
            other => other,
        };
        self.annot_with_tparams(|tparams_rev| {
            Err(Found::Found(ProcessLocationResult {
                tparams_rev,
                ac_loc,
                token,
                autocomplete_type,
            }))
        })
    }

    fn with_enclosing_class_t<F>(&mut self, class_t: Type, f: F) -> Result<(), Found>
    where
        F: FnOnce(&mut Self) -> Result<(), Found>,
    {
        self.enclosing_classes.push(class_t);
        let result = f(self);
        self.enclosing_classes.pop();
        result
    }

    fn with_class_key<F>(&mut self, f: F) -> Result<(), Found>
    where
        F: FnOnce(&mut Self) -> Result<(), Found>,
    {
        self.class_key_depth += 1;
        let result = f(self);
        self.class_key_depth -= 1;
        result
    }

    fn member_with_annot(
        &self,
        expr_loc: &ALoc,
        obj: &expression::Expression<ALoc, ALoc>,
        property: &expression::member::Property<ALoc, ALoc>,
        in_optional_chain: bool,
    ) -> Result<Option<(ALoc, String, AutocompleteType)>, Found> {
        let obj_loc = obj.loc().dupe();
        let member_loc = Some(compute_member_loc(expr_loc, &obj_loc));
        let is_super = matches!(obj.deref(), expression::ExpressionInner::Super { .. });
        match property {
            expression::member::Property::PropertyIdentifier(id) if self.covers_target(&id.loc) => {
                let obj_type = Inference::type_of_expression(self.cx, obj)?;
                Ok(Some((
                    id.loc.dupe(),
                    id.name.to_string(),
                    AutocompleteType::AcMember {
                        obj_type,
                        in_optional_chain,
                        bracket_syntax: None,
                        member_loc,
                        is_type_annotation: false,
                        is_super,
                    },
                )))
            }
            expression::member::Property::PropertyExpression(expr)
                if self.covers_target(expr.loc()) =>
            {
                match expr.deref() {
                    expression::ExpressionInner::StringLiteral { loc, inner } => {
                        let index_type = self.type_from_enclosing_node(loc)?;
                        let obj_type = Inference::type_of_expression(self.cx, obj)?;
                        Ok(Some((
                            loc.dupe(),
                            inner.raw.to_string(),
                            AutocompleteType::AcMember {
                                obj_type,
                                in_optional_chain,
                                bracket_syntax: Some(self.default_bracket_syntax(index_type)),
                                member_loc,
                                is_type_annotation: false,
                                is_super,
                            },
                        )))
                    }
                    expression::ExpressionInner::Identifier { loc, inner } => {
                        let index_type = self.type_from_enclosing_node(loc)?;
                        let obj_type = Inference::type_of_expression(self.cx, obj)?;
                        Ok(Some((
                            loc.dupe(),
                            inner.name.to_string(),
                            AutocompleteType::AcMember {
                                obj_type,
                                in_optional_chain,
                                bracket_syntax: Some(self.default_bracket_syntax(index_type)),
                                member_loc,
                                is_type_annotation: false,
                                is_super,
                            },
                        )))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    fn object_spread(
        &self,
        properties: &[expression::object::Property<ALoc, (ALoc, Type)>],
    ) -> (BTreeSet<String>, Vec<(Loc, Type)>) {
        let mut used_keys = BTreeSet::new();
        let mut spreads = Vec::new();
        for prop in properties {
            match prop {
                expression::object::Property::NormalProperty(prop) => match prop {
                    expression::object::NormalProperty::Init { key, .. }
                    | expression::object::NormalProperty::Method { key, .. }
                    | expression::object::NormalProperty::Get { key, .. }
                    | expression::object::NormalProperty::Set { key, .. } => match key {
                        expression::object::Key::Identifier(id) => {
                            used_keys.insert(id.name.to_string());
                        }
                        expression::object::Key::StringLiteral((_loc, lit)) => {
                            used_keys.insert(lit.value.to_string());
                        }
                        _ => {}
                    },
                },
                expression::object::Property::SpreadProperty(prop) => {
                    let (spread_loc, spread_type) = prop.argument.loc();
                    spreads.push((spread_loc.to_loc_exn().clone(), spread_type.dupe()));
                }
            }
        }
        (used_keys, spreads)
    }

    fn ac_key_for_object(
        &self,
        loc: &ALoc,
        obj: &expression::Object<ALoc, ALoc>,
    ) -> Result<AutocompleteType, Found> {
        let typed = typing_statement::expression(
            None,
            None,
            None,
            self.cx,
            &expression::Expression::new(expression::ExpressionInner::Object {
                loc: loc.dupe(),
                inner: obj.clone().into(),
            }),
        )
        .map_err(|_| Found::InternalExn("typed AST structure mismatch".to_string()))?;
        match typed.deref() {
            expression::ExpressionInner::Object {
                loc: (_, obj_type),
                inner,
            } => {
                let (used_keys, spreads) = self.object_spread(inner.properties.as_ref());
                Ok(AutocompleteType::AcKey {
                    obj_type: obj_type.dupe(),
                    used_keys,
                    spreads,
                })
            }
            _ => Ok(AutocompleteType::AcKey {
                used_keys: BTreeSet::new(),
                spreads: Vec::new(),
                obj_type: type_::any_t::at(type_::AnySource::Untyped, loc.dupe()),
            }),
        }
    }

    fn indexed_access_type_with_annot(
        &self,
        expr_loc: &ALoc,
        obj: &types::Type<ALoc, ALoc>,
        index: &types::Type<ALoc, ALoc>,
        in_optional_chain: bool,
    ) -> Result<Option<(ALoc, String, AutocompleteType)>, Found> {
        match index.deref() {
            types::TypeInner::StringLiteral { loc, literal } if self.covers_target(loc) => {
                let index_type = self.type_from_enclosing_node(loc)?;
                let obj_type = self.type_from_enclosing_node(obj.loc())?;
                Ok(Some((
                    loc.dupe(),
                    literal.raw.to_string(),
                    AutocompleteType::AcMember {
                        obj_type,
                        in_optional_chain,
                        bracket_syntax: Some(self.default_bracket_syntax(index_type)),
                        member_loc: Some(compute_member_loc(expr_loc, obj.loc())),
                        is_type_annotation: true,
                        is_super: false,
                    },
                )))
            }
            types::TypeInner::Generic { inner, .. } if self.covers_target(index.loc()) => {
                match &inner.id {
                    types::generic::Identifier::Unqualified(id) => {
                        let index_type = self.type_from_enclosing_node(&id.loc)?;
                        let obj_type = self.type_from_enclosing_node(obj.loc())?;
                        Ok(Some((
                            id.loc.dupe(),
                            id.name.to_string(),
                            AutocompleteType::AcMember {
                                obj_type,
                                in_optional_chain,
                                bracket_syntax: Some(self.default_bracket_syntax(index_type)),
                                member_loc: Some(compute_member_loc(expr_loc, obj.loc())),
                                is_type_annotation: true,
                                is_super: false,
                            },
                        )))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

impl<'ast> AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, Found> for ProcessRequestSearcher<'_, '_> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    fn program(&mut self, prog: &'ast ast::Program<ALoc, ALoc>) -> Result<(), Found> {
        self.enclosing_node_stack
            .push(EnclosingNode::EnclosingProgram(prog.clone()));
        let result = ast_visitor::program_default(self, prog);
        self.enclosing_node_stack.pop();
        result
    }

    fn statement(&mut self, stmt: &'ast statement::Statement<ALoc, ALoc>) -> Result<(), Found> {
        self.enclosing_node_stack
            .push(EnclosingNode::EnclosingStatement(stmt.dupe()));
        let result = match stmt.deref() {
            statement::StatementInner::ClassDeclaration { loc, inner } if inner.id.is_some() => {
                let class_t = Inference::type_of_class_id(self.cx, loc, inner)?;
                self.with_enclosing_class_t(class_t, |this| {
                    ast_visitor::statement_default(this, stmt)
                })
            }
            _ => ast_visitor::statement_default(self, stmt),
        };
        self.enclosing_node_stack.pop();
        result
    }

    fn expression(&mut self, expr: &'ast expression::Expression<ALoc, ALoc>) -> Result<(), Found> {
        self.enclosing_node_stack
            .push(EnclosingNode::EnclosingExpression(expr.dupe()));
        let result = match expr.deref() {
            expression::ExpressionInner::StringLiteral { loc, inner }
                if self.covers_target(loc) =>
            {
                let lit_type = Inference::type_of_expression(self.cx, expr)?;
                self.find(
                    loc.dupe(),
                    inner.raw.to_string(),
                    AutocompleteType::AcLiteral {
                        lit_type: Some(lit_type),
                    },
                )
            }
            _ => ast_visitor::expression_default(self, expr),
        };
        self.enclosing_node_stack.pop();
        result
    }

    fn comment(&mut self, comment: &'ast ast::Comment<ALoc>) -> Result<(), Found> {
        if self.covers_target(&comment.loc) {
            let (token, word_loc) = extract_word(self.cursor, &comment.text);
            self.check_closest_enclosing_statement()?;
            self.find(
                ALoc::of_loc(word_loc),
                token,
                AutocompleteType::AcComment {
                    text: comment.text.to_string(),
                    loc: comment.loc.dupe(),
                },
            )
        } else {
            Ok(())
        }
    }

    fn identifier(&mut self, ident: &'ast ast::Identifier<ALoc, ALoc>) -> Result<(), Found> {
        if self.covers_target(&ident.loc) {
            let t = self.type_from_enclosing_node(&ident.loc)?;
            self.find(
                ident.loc.dupe(),
                ident.name.to_string(),
                self.default_ac_id(t),
            )
        } else {
            ast_visitor::identifier_default(self, ident)
        }
    }

    fn jsx_element_name_identifier(
        &mut self,
        ident: &'ast ast::jsx::Identifier<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if self.covers_target(&ident.loc) {
            let t = self.type_from_enclosing_node(&ident.loc)?;
            self.find(
                ident.loc.dupe(),
                ident.name.to_string(),
                self.default_ac_id(t),
            )
        } else {
            ast_visitor::jsx_element_name_identifier_default(self, ident)
        }
    }

    fn object_property_type(
        &mut self,
        prop: &'ast types::object::NormalProperty<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match &prop.key {
            expression::object::Key::Identifier(id) if self.covers_target(&id.loc) => self.find(
                id.loc.dupe(),
                id.name.to_string(),
                AutocompleteType::AcTypeBinding,
            ),
            _ => ast_visitor::object_property_type_default(self, prop),
        }
    }

    fn member(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast expression::Member<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if let Some((ac_loc, token, autocomplete_type)) =
            self.member_with_annot(loc, &expr.object, &expr.property, false)?
        {
            self.find(ac_loc, token, autocomplete_type)
        } else {
            ast_visitor::member_default(self, loc, expr)
        }
    }

    fn optional_member(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast expression::OptionalMember<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if let Some((ac_loc, token, autocomplete_type)) =
            self.member_with_annot(loc, &expr.member.object, &expr.member.property, true)?
        {
            self.find(ac_loc, token, autocomplete_type)
        } else {
            ast_visitor::optional_member_default(self, loc, expr)
        }
    }

    fn pattern(
        &mut self,
        kind: Option<ast::VariableKind>,
        pat: &'ast ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if let ast::pattern::Pattern::Object { loc, inner } = pat {
            for prop in inner.properties.iter() {
                if let ast::pattern::object::Property::NormalProperty(prop) = prop {
                    match &prop.key {
                        ast::pattern::object::Key::Identifier(id)
                            if self.covers_target(&id.loc) =>
                        {
                            let obj_type = self.type_from_enclosing_node(loc)?;
                            return self.find(
                                id.loc.dupe(),
                                id.name.to_string(),
                                AutocompleteType::AcMember {
                                    obj_type,
                                    in_optional_chain: false,
                                    bracket_syntax: None,
                                    member_loc: None,
                                    is_type_annotation: false,
                                    is_super: false,
                                },
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
        ast_visitor::pattern_default(self, kind, pat)
    }

    fn pattern_identifier(
        &mut self,
        kind: Option<ast::VariableKind>,
        ident: &'ast ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match kind {
            Some(_) if self.covers_target(&ident.loc) => self.find(
                ident.loc.dupe(),
                ident.name.to_string(),
                AutocompleteType::AcBinding,
            ),
            _ => ast_visitor::pattern_identifier_default(self, kind, ident),
        }
    }

    fn jsx_element(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::jsx::Element<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let opening = &expr.opening_element;
        match &opening.name {
            ast::jsx::Name::Identifier(id) if self.covers_target(&id.loc) => {
                let type_ = Inference::type_of_component_name_of_jsx_element(self.cx, loc, expr)?;
                return self.find(
                    id.loc.dupe(),
                    id.name.to_string(),
                    AutocompleteType::AcJsxElement { type_ },
                );
            }
            _ => {}
        }

        let mut used_attr_names = BTreeSet::new();
        let mut found: Option<(ALoc, String, bool)> = None;
        for attr in opening.attributes.iter() {
            if let ast::jsx::OpeningAttribute::Attribute(attr) = attr {
                if let ast::jsx::attribute::Name::Identifier(id) = &attr.name {
                    if found.is_none() && self.covers_target(&id.loc) {
                        found = Some((id.loc.dupe(), id.name.to_string(), attr.value.is_some()));
                    }
                    used_attr_names.insert(id.name.to_string());
                }
            }
        }
        if let Some((ac_loc, attribute_name, has_value)) = found {
            let component_t = Inference::type_of_component_name_of_jsx_element(self.cx, loc, expr)?;
            return self.find(
                ac_loc,
                attribute_name.clone(),
                AutocompleteType::AcJsxAttribute {
                    attribute_name,
                    used_attr_names,
                    component_t,
                    has_value,
                },
            );
        }

        if let Some(closing) = &expr.closing_element {
            if let ast::jsx::Name::Identifier(id) = &closing.name {
                if self.covers_target(&id.loc) {
                    let type_ =
                        Inference::type_of_component_name_of_jsx_element(self.cx, loc, expr)?;
                    return self.find(
                        id.loc.dupe(),
                        id.name.to_string(),
                        AutocompleteType::AcJsxElement { type_ },
                    );
                }
            }
        }

        ast_visitor::jsx_element_default(self, loc, expr)
    }

    fn jsx_attribute_value(
        &mut self,
        value: &'ast ast::jsx::attribute::Value<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match value {
            ast::jsx::attribute::Value::StringLiteral((loc, lit)) if self.covers_target(loc) => {
                let lit_type = self.type_from_enclosing_node(loc)?;
                self.find(
                    loc.dupe(),
                    lit.raw.to_string(),
                    AutocompleteType::AcLiteral {
                        lit_type: Some(lit_type),
                    },
                )
            }
            _ => ast_visitor::jsx_attribute_value_default(self, value),
        }
    }

    fn jsx_child(&mut self, child: &'ast ast::jsx::Child<ALoc, ALoc>) -> Result<(), Found> {
        match child {
            ast::jsx::Child::Text { loc, inner } if self.covers_target(loc) => self.find(
                loc.dupe(),
                inner.raw.to_string(),
                AutocompleteType::AcJsxText,
            ),
            _ => ast_visitor::jsx_child_default(self, child),
        }
    }

    fn pattern_object_property_key(
        &mut self,
        kind: Option<ast::VariableKind>,
        key: &'ast ast::pattern::object::Key<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match key {
            ast::pattern::object::Key::StringLiteral((loc, lit)) if self.covers_target(loc) => self
                .find(
                    loc.dupe(),
                    lit.raw.to_string(),
                    AutocompleteType::AcLiteral {
                        lit_type: Some(type_::any_t::at(type_::AnySource::Untyped, loc.dupe())),
                    },
                ),
            _ => ast_visitor::pattern_object_property_key_default(self, kind, key),
        }
    }

    fn object_key_identifier(
        &mut self,
        id: &'ast ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if self.class_key_depth > 0 && self.covers_target(&id.loc) {
            self.find(
                id.loc.dupe(),
                id.name.to_string(),
                AutocompleteType::AcClassKey {
                    enclosing_class_t: self.get_enclosing_class(),
                },
            )
        } else {
            Ok(())
        }
    }

    fn object_key(&mut self, key: &'ast expression::object::Key<ALoc, ALoc>) -> Result<(), Found> {
        match key {
            expression::object::Key::Identifier(id)
                if self.class_key_depth > 0 && self.covers_target(&id.loc) =>
            {
                self.find(
                    id.loc.dupe(),
                    id.name.to_string(),
                    AutocompleteType::AcClassKey {
                        enclosing_class_t: self.get_enclosing_class(),
                    },
                )
            }
            expression::object::Key::StringLiteral((loc, lit))
                if self.class_key_depth > 0 && self.covers_target(loc) =>
            {
                self.find(
                    loc.dupe(),
                    lit.raw.to_string(),
                    AutocompleteType::AcClassKey {
                        enclosing_class_t: self.get_enclosing_class(),
                    },
                )
            }
            expression::object::Key::PrivateName(name)
                if self.class_key_depth > 0 && self.covers_target(&name.loc) =>
            {
                self.find(
                    name.loc.dupe(),
                    format!("#{}", name.name),
                    AutocompleteType::AcClassKey {
                        enclosing_class_t: self.get_enclosing_class(),
                    },
                )
            }
            expression::object::Key::StringLiteral((loc, lit)) if self.covers_target(loc) => {
                let lit_type = self.type_from_enclosing_node(loc)?;
                self.find(
                    loc.dupe(),
                    lit.raw.to_string(),
                    AutocompleteType::AcLiteral {
                        lit_type: Some(lit_type),
                    },
                )
            }
            _ => ast_visitor::object_key_default(self, key),
        }
    }

    fn enum_member_identifier(
        &mut self,
        ident: &'ast ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if self.covers_target(&ident.loc) {
            self.find(
                ident.loc.dupe(),
                ident.name.to_string(),
                AutocompleteType::AcEnum,
            )
        } else {
            ast_visitor::enum_member_identifier_default(self, ident)
        }
    }

    fn class_body(&mut self, body: &'ast ast::class::Body<ALoc, ALoc>) -> Result<(), Found> {
        match ast_visitor::class_body_default(self, body) {
            Err(Found::Found(mut found)) => {
                match &mut found.autocomplete_type {
                    AutocompleteType::AcId {
                        include_super,
                        include_this,
                        ..
                    } => {
                        *include_super = true;
                        *include_this = true;
                    }
                    AutocompleteType::AcMember {
                        bracket_syntax: Some(bracket_syntax),
                        ..
                    } => {
                        bracket_syntax.include_super = true;
                        bracket_syntax.include_this = true;
                    }
                    _ => {}
                }
                Err(Found::Found(found))
            }
            other => other,
        }
    }

    fn function_expression(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match ast_visitor::function_expression_default(self, loc, expr) {
            Err(Found::Found(mut found)) => {
                match &mut found.autocomplete_type {
                    AutocompleteType::AcId { include_this, .. } => {
                        *include_this = true;
                    }
                    AutocompleteType::AcMember {
                        bracket_syntax: Some(bracket_syntax),
                        ..
                    } => {
                        bracket_syntax.include_this = true;
                    }
                    _ => {}
                }
                Err(Found::Found(found))
            }
            other => other,
        }
    }

    fn function_declaration(
        &mut self,
        loc: &'ast ALoc,
        stmt: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), Found> {
        match ast_visitor::function_declaration_default(self, loc, stmt) {
            Err(Found::Found(mut found)) => {
                match &mut found.autocomplete_type {
                    AutocompleteType::AcId { include_this, .. } => {
                        *include_this = true;
                    }
                    AutocompleteType::AcMember {
                        bracket_syntax: Some(bracket_syntax),
                        ..
                    } => {
                        bracket_syntax.include_this = true;
                    }
                    _ => {}
                }
                Err(Found::Found(found))
            }
            other => other,
        }
    }

    fn object(
        &mut self,
        loc: &'ast ALoc,
        obj: &'ast expression::Object<ALoc, ALoc>,
    ) -> Result<(), Found> {
        for prop in obj.properties.iter() {
            if let expression::object::Property::NormalProperty(prop) = prop {
                let key = match prop {
                    expression::object::NormalProperty::Init { key, .. }
                    | expression::object::NormalProperty::Method { key, .. }
                    | expression::object::NormalProperty::Get { key, .. }
                    | expression::object::NormalProperty::Set { key, .. } => key,
                };
                match key {
                    expression::object::Key::Identifier(id) if self.covers_target(&id.loc) => {
                        let ac_key = self.ac_key_for_object(loc, obj)?;
                        return self.find(id.loc.dupe(), id.name.to_string(), ac_key);
                    }
                    expression::object::Key::StringLiteral((key_loc, lit))
                        if self.covers_target(key_loc) =>
                    {
                        let ac_key = self.ac_key_for_object(loc, obj)?;
                        return self.find(key_loc.dupe(), lit.raw.to_string(), ac_key);
                    }
                    _ => {}
                }
            }
        }
        ast_visitor::object_default(self, loc, obj)
    }

    fn template_literal_element(
        &mut self,
        elem: &'ast expression::template_literal::Element<ALoc>,
    ) -> Result<(), Found> {
        if self.covers_target(&elem.loc) {
            self.find(
                elem.loc.dupe(),
                elem.value.raw.to_string(),
                AutocompleteType::AcLiteral {
                    lit_type: Some(type_::any_t::at(type_::AnySource::Untyped, elem.loc.dupe())),
                },
            )
        } else {
            ast_visitor::template_literal_element_default(self, elem)
        }
    }

    fn import_declaration(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast statement::ImportDeclaration<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let (source_loc, source_literal) = &decl.source;
        if self.covers_target(source_loc) {
            return self.find(
                source_loc.dupe(),
                source_literal.raw.to_string(),
                AutocompleteType::AcModule,
            );
        }
        if self.covers_target(loc) {
            if let Some(statement::import_declaration::Specifier::ImportNamedSpecifiers(
                specifiers,
            )) = &decl.specifiers
            {
                let mut found = None;
                let mut used_keys = BTreeSet::new();
                for specifier in specifiers.iter() {
                    let remote = &specifier.remote;
                    if self.covers_target(&remote.loc) {
                        let is_type = match specifier.kind.unwrap_or(decl.import_kind) {
                            statement::ImportKind::ImportType => true,
                            statement::ImportKind::ImportTypeof => false,
                            statement::ImportKind::ImportValue => false,
                        };
                        found = Some((remote.loc.dupe(), remote.name.to_string(), is_type));
                    }
                    used_keys.insert(remote.name.to_string());
                }
                if let Some((ac_loc, token, is_type)) = found {
                    let module_name =
                        flow_import_specifier::Userland::from_smol_str(source_literal.value.dupe());
                    let module_type_opt =
                        flow_js_utils::import_export_utils::get_module_type_or_any(
                            self.cx,
                            false,
                            None,
                            source_loc.dupe(),
                            module_name,
                        )
                        .ok()
                        .and_then(Result::ok);
                    return self.find(
                        ac_loc,
                        token,
                        AutocompleteType::AcImportSpecifier {
                            module_type_opt,
                            used_keys,
                            is_type,
                        },
                    );
                }
            }
        }
        ast_visitor::import_declaration_default(self, loc, decl)
    }

    fn typeof_type(&mut self, typeof_: &'ast types::Typeof<ALoc, ALoc>) -> Result<(), Found> {
        match &typeof_.argument {
            types::typeof_::Target::Unqualified(id) if self.covers_target(&id.loc) => {
                let t = self.type_from_enclosing_node(&id.loc)?;
                self.find(id.loc.dupe(), id.name.to_string(), self.default_ac_id(t))
            }
            types::typeof_::Target::Qualified(q) if self.covers_target(&q.id.loc) => {
                let obj_type = self.type_from_enclosing_node(&loc_of_typeof(&q.qualification))?;
                self.find(
                    q.id.loc.dupe(),
                    q.id.name.to_string(),
                    AutocompleteType::AcMember {
                        obj_type,
                        in_optional_chain: false,
                        bracket_syntax: None,
                        member_loc: Some(compute_member_loc(
                            &q.loc,
                            &loc_of_typeof(&q.qualification),
                        )),
                        is_type_annotation: false,
                        is_super: false,
                    },
                )
            }
            _ => ast_visitor::typeof_type_default(self, typeof_),
        }
    }

    fn generic_type(&mut self, gt: &'ast types::Generic<ALoc, ALoc>) -> Result<(), Found> {
        match &gt.id {
            types::generic::Identifier::Unqualified(id) if self.covers_target(&id.loc) => {
                self.find(id.loc.dupe(), id.name.to_string(), AutocompleteType::AcType)
            }
            types::generic::Identifier::Qualified(q) if self.covers_target(&q.id.loc) => {
                let qualification_type =
                    self.type_from_enclosing_node(&loc_of_qualification(&q.qualification))?;
                self.find(
                    q.id.loc.dupe(),
                    q.id.name.to_string(),
                    AutocompleteType::AcQualifiedType(qualification_type),
                )
            }
            _ => ast_visitor::generic_type_default(self, gt),
        }
    }

    fn record(
        &mut self,
        _loc: &'ast ALoc,
        expr: &'ast expression::Record<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let record_t = Inference::type_of_expression(self.cx, &expr.constructor)?;
        let props_obj = &expr.properties.1;
        let mut used_field_names = BTreeSet::new();
        let mut found: Option<(ALoc, String, bool)> = None;
        for prop in props_obj.properties.iter() {
            if let expression::object::Property::NormalProperty(prop) = prop {
                let key = match prop {
                    expression::object::NormalProperty::Init { key, .. }
                    | expression::object::NormalProperty::Method { key, .. }
                    | expression::object::NormalProperty::Get { key, .. }
                    | expression::object::NormalProperty::Set { key, .. } => key,
                };
                if let expression::object::Key::Identifier(id) = key {
                    let has_value = match prop {
                        expression::object::NormalProperty::Init { value, .. } => {
                            value.loc() != &id.loc
                        }
                        expression::object::NormalProperty::Method { .. }
                        | expression::object::NormalProperty::Get { .. }
                        | expression::object::NormalProperty::Set { .. } => true,
                    };
                    if found.is_none() && self.covers_target(&id.loc) {
                        found = Some((id.loc.dupe(), id.name.to_string(), has_value));
                    }
                    used_field_names.insert(id.name.to_string());
                }
            }
        }
        if let Some((ac_loc, field_name, has_value)) = found {
            return self.find(
                ac_loc,
                field_name.clone(),
                AutocompleteType::AcRecordField {
                    field_name,
                    used_field_names,
                    record_t,
                    has_value,
                },
            );
        }
        ast_visitor::record_default(self, _loc, expr)
    }

    fn type_(&mut self, t: &'ast types::Type<ALoc, ALoc>) -> Result<(), Found> {
        match t.deref() {
            types::TypeInner::StringLiteral { loc, literal } if self.covers_target(loc) => self
                .find(
                    loc.dupe(),
                    literal.raw.to_string(),
                    AutocompleteType::AcLiteral { lit_type: None },
                ),
            types::TypeInner::IndexedAccess { loc, inner } => {
                if let Some((ac_loc, token, autocomplete_type)) =
                    self.indexed_access_type_with_annot(loc, &inner.object, &inner.index, false)?
                {
                    self.find(ac_loc, token, autocomplete_type)
                } else {
                    ast_visitor::type_default(self, t)
                }
            }
            types::TypeInner::OptionalIndexedAccess { loc, inner } => {
                if let Some((ac_loc, token, autocomplete_type)) = self
                    .indexed_access_type_with_annot(
                        loc,
                        &inner.indexed_access.object,
                        &inner.indexed_access.index,
                        true,
                    )?
                {
                    self.find(ac_loc, token, autocomplete_type)
                } else {
                    ast_visitor::type_default(self, t)
                }
            }
            _ => ast_visitor::type_default(self, t),
        }
    }

    fn binding_type_identifier(
        &mut self,
        id: &'ast ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), Found> {
        if self.covers_target(&id.loc) {
            self.find(
                id.loc.dupe(),
                id.name.to_string(),
                AutocompleteType::AcTypeBinding,
            )
        } else {
            ast_visitor::binding_type_identifier_default(self, id)
        }
    }

    fn match_object_pattern_property_key(
        &mut self,
        key: &'ast ast::match_pattern::object_pattern::Key<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let (loc, token) = match key {
            ast::match_pattern::object_pattern::Key::StringLiteral((loc, lit)) => {
                (loc.dupe(), lit.raw.to_string())
            }
            ast::match_pattern::object_pattern::Key::NumberLiteral((loc, lit)) => {
                (loc.dupe(), lit.raw.to_string())
            }
            ast::match_pattern::object_pattern::Key::BigIntLiteral((loc, lit)) => {
                (loc.dupe(), lit.raw.to_string())
            }
            ast::match_pattern::object_pattern::Key::Identifier(id) => {
                (id.loc.dupe(), id.name.to_string())
            }
        };
        if self.covers_target(&loc) {
            self.find(loc, token, AutocompleteType::AcIgnored)
        } else {
            ast_visitor::match_object_pattern_property_key_default(self, key)
        }
    }

    fn match_member_pattern(
        &mut self,
        member_pattern: &'ast ast::match_pattern::MemberPattern<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let base_loc = match &member_pattern.base {
            ast::match_pattern::member_pattern::Base::BaseIdentifier(id) => id.loc.dupe(),
            ast::match_pattern::member_pattern::Base::BaseMember(mem) => mem.loc.dupe(),
        };
        let member_loc = Some(compute_member_loc(&member_pattern.loc, &base_loc));
        match &member_pattern.property {
            ast::match_pattern::member_pattern::Property::PropertyIdentifier(id)
                if self.covers_target(&id.loc) =>
            {
                let obj_type = match &member_pattern.base {
                    ast::match_pattern::member_pattern::Base::BaseIdentifier(id) => {
                        Inference::type_of_identifier(self.cx, &id.loc, id)?
                    }
                    ast::match_pattern::member_pattern::Base::BaseMember(mem) => {
                        Inference::type_of_match_member_pattern(self.cx, mem)?
                    }
                };
                self.find(
                    id.loc.dupe(),
                    id.name.to_string(),
                    AutocompleteType::AcMember {
                        obj_type,
                        in_optional_chain: false,
                        bracket_syntax: None,
                        member_loc,
                        is_type_annotation: false,
                        is_super: false,
                    },
                )
            }
            ast::match_pattern::member_pattern::Property::PropertyString { loc, literal }
                if self.covers_target(loc) =>
            {
                let obj_type = match &member_pattern.base {
                    ast::match_pattern::member_pattern::Base::BaseIdentifier(id) => {
                        Inference::type_of_identifier(self.cx, &id.loc, id)?
                    }
                    ast::match_pattern::member_pattern::Base::BaseMember(mem) => {
                        Inference::type_of_match_member_pattern(self.cx, mem)?
                    }
                };
                self.find(
                    loc.dupe(),
                    literal.raw.to_string(),
                    AutocompleteType::AcMember {
                        bracket_syntax: Some(self.default_bracket_syntax(obj_type.dupe())),
                        obj_type,
                        in_optional_chain: false,
                        member_loc,
                        is_type_annotation: false,
                        is_super: false,
                    },
                )
            }
            _ => ast_visitor::match_member_pattern_default(self, member_pattern),
        }
    }

    fn type_param(
        &mut self,
        kind: &TypeParamsContext,
        tparam: &'ast types::TypeParam<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let result = ast_visitor::type_param_default(self, kind, tparam);
        self.rev_bound_tparams.push(self.make_typeparam(tparam));
        result
    }

    fn type_params(
        &mut self,
        kind: &TypeParamsContext,
        tparams: &'ast types::TypeParams<ALoc, ALoc>,
    ) -> Result<(), Found> {
        ast_visitor::type_params_default(self, kind, tparams)
    }

    fn function_(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::function_default(self, loc, expr);
        self.rev_bound_tparams = original;
        result
    }

    fn component_declaration(
        &mut self,
        loc: &'ast ALoc,
        component: &'ast statement::ComponentDeclaration<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::component_declaration_default(self, loc, component);
        self.rev_bound_tparams = original;
        result
    }

    fn class_(
        &mut self,
        loc: &'ast ALoc,
        cls: &'ast ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), Found> {
        // Save original, push "this", get class_t and with_enclosing_class_t, restore
        let original = self.rev_bound_tparams.clone();
        self.rev_bound_tparams.push("this".to_string());
        let result = match Inference::type_of_expression(
            self.cx,
            &expression::Expression::new(expression::ExpressionInner::Class {
                loc: loc.dupe(),
                inner: cls.clone().into(),
            }),
        ) {
            Ok(class_t) => self
                .with_enclosing_class_t(class_t, |this| ast_visitor::class_default(this, loc, cls)),
            Err(_) => ast_visitor::class_default(self, loc, cls),
        };
        self.rev_bound_tparams = original;
        result
    }

    fn declare_class(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast statement::DeclareClass<ALoc, ALoc>,
    ) -> Result<(), Found> {
        // Save original, push "this", process declare_class, restore
        let original = self.rev_bound_tparams.clone();
        self.rev_bound_tparams.push("this".to_string());
        let result = ast_visitor::declare_class_default(self, loc, decl);
        self.rev_bound_tparams = original;
        result
    }

    fn declare_component(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast statement::DeclareComponent<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::declare_component_default(self, loc, decl);
        self.rev_bound_tparams = original;
        result
    }

    fn component_type(
        &mut self,
        loc: &'ast ALoc,
        component: &'ast types::Component<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::component_type_default(self, loc, component);
        self.rev_bound_tparams = original;
        result
    }

    fn function_type(&mut self, ft: &'ast types::Function<ALoc, ALoc>) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::function_type_default(self, ft);
        self.rev_bound_tparams = original;
        result
    }

    fn interface(
        &mut self,
        loc: &'ast ALoc,
        interface: &'ast statement::Interface<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::interface_default(self, loc, interface);
        self.rev_bound_tparams = original;
        result
    }

    fn opaque_type(
        &mut self,
        loc: &'ast ALoc,
        otype: &'ast statement::OpaqueType<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::opaque_type_default(self, loc, otype);
        self.rev_bound_tparams = original;
        result
    }

    fn type_alias(
        &mut self,
        loc: &'ast ALoc,
        alias: &'ast statement::TypeAlias<ALoc, ALoc>,
    ) -> Result<(), Found> {
        let original = self.rev_bound_tparams.clone();
        let result = ast_visitor::type_alias_default(self, loc, alias);
        self.rev_bound_tparams = original;
        result
    }

    fn record_declaration(
        &mut self,
        loc: &'ast ALoc,
        record: &'ast statement::RecordDeclaration<ALoc, ALoc>,
    ) -> Result<(), Found> {
        // Save original, push "this", process record_declaration, restore
        let original = self.rev_bound_tparams.clone();
        self.rev_bound_tparams.push("this".to_string());
        let result = ast_visitor::record_declaration_default(self, loc, record);
        self.rev_bound_tparams = original;
        result
    }

    fn class_method(&mut self, method: &'ast ast::class::Method<ALoc, ALoc>) -> Result<(), Found> {
        self.with_class_key(|this| ast_visitor::class_method_default(this, method))
    }

    fn class_property(
        &mut self,
        prop: &'ast ast::class::Property<ALoc, ALoc>,
    ) -> Result<(), Found> {
        self.with_class_key(|this| ast_visitor::class_property_default(this, prop))
    }

    fn class_declare_method(
        &mut self,
        decl: &'ast ast::class::DeclareMethod<ALoc, ALoc>,
    ) -> Result<(), Found> {
        self.with_class_key(|this| ast_visitor::class_declare_method_default(this, decl))
    }

    fn class_abstract_method(
        &mut self,
        abs_meth: &'ast ast::class::AbstractMethod<ALoc, ALoc>,
    ) -> Result<(), Found> {
        self.with_class_key(|this| ast_visitor::class_abstract_method_default(this, abs_meth))
    }

    fn class_abstract_property(
        &mut self,
        abs_prop: &'ast ast::class::AbstractProperty<ALoc, ALoc>,
    ) -> Result<(), Found> {
        self.with_class_key(|this| ast_visitor::class_abstract_property_default(this, abs_prop))
    }
}

pub fn autocomplete_id(cursor: &Loc, _cx: &Context, _ac_name: &str, ac_loc: &ALoc) -> bool {
    covers_target(cursor, ac_loc)
}

pub fn autocomplete_literal(cursor: &Loc, _cx: &Context, ac_loc: &ALoc) -> bool {
    covers_target(cursor, ac_loc)
}

pub fn autocomplete_object_key(cursor: &Loc, _cx: &Context, _ac_name: &str, ac_loc: &ALoc) -> bool {
    covers_target(cursor, ac_loc)
}

pub fn autocomplete_jsx(cursor: &Loc, _cx: &Context, _ac_name: &str, ac_loc: &ALoc) -> bool {
    covers_target(cursor, ac_loc)
}

pub fn process_location(
    cx: &Context,
    file_sig: Arc<flow_parser_utils::file_sig::FileSig>,
    trigger_character: Option<&str>,
    cursor: &Loc,
    aloc_ast: &ast::Program<ALoc, ALoc>,
) -> Result<
    Result<Option<ProcessLocationResult>, String>,
    flow_utils_concurrency::job_error::JobError,
> {
    let from_trigger_character = trigger_character.is_some();
    let mut searcher = ProcessRequestSearcher::new(cx, file_sig, from_trigger_character, cursor);
    match searcher.program(aloc_ast) {
        Err(Found::Found(found)) => Ok(Ok(Some(found))),
        Err(Found::InternalExn(err)) => Ok(Err(err)),
        Err(Found::Canceled(c)) => Err(flow_utils_concurrency::job_error::JobError::Canceled(c)),
        Err(Found::TimedOut(t)) => Err(flow_utils_concurrency::job_error::JobError::TimedOut(t)),
        Err(Found::DebugThrow { loc }) => {
            Err(flow_utils_concurrency::job_error::JobError::DebugThrow { loc })
        }
        Ok(()) => Ok(Ok(None)),
    }
}

fn autocomplete_id_hook(cx: &Context, ac_name: &str, ac_loc: ALoc) -> bool {
    AUTOCOMPLETE_CURSOR.with(|cursor| {
        cursor
            .borrow()
            .as_ref()
            .is_some_and(|cursor| autocomplete_id(cursor, cx, ac_name, &ac_loc))
    })
}

fn autocomplete_literal_hook(cx: &Context, ac_loc: ALoc) -> bool {
    AUTOCOMPLETE_CURSOR.with(|cursor| {
        cursor
            .borrow()
            .as_ref()
            .is_some_and(|cursor| autocomplete_literal(cursor, cx, &ac_loc))
    })
}

fn autocomplete_object_key_hook(cx: &Context, ac_name: &str, ac_loc: ALoc) -> bool {
    AUTOCOMPLETE_CURSOR.with(|cursor| {
        cursor
            .borrow()
            .as_ref()
            .is_some_and(|cursor| autocomplete_object_key(cursor, cx, ac_name, &ac_loc))
    })
}

fn autocomplete_jsx_hook(cx: &Context, ac_name: &str, ac_loc: ALoc) -> bool {
    AUTOCOMPLETE_CURSOR.with(|cursor| {
        cursor
            .borrow()
            .as_ref()
            .is_some_and(|cursor| autocomplete_jsx(cursor, cx, ac_name, &ac_loc))
    })
}

pub fn autocomplete_set_hooks(cursor: &Loc) {
    AUTOCOMPLETE_CURSOR.with(|current| {
        *current.borrow_mut() = Some(cursor.clone());
    });
    type_inference_hooks_js::set_id_hook(autocomplete_id_hook);
    type_inference_hooks_js::set_literal_hook(autocomplete_literal_hook);
    type_inference_hooks_js::set_obj_prop_decl_hook(autocomplete_object_key_hook);
    type_inference_hooks_js::set_jsx_hook(autocomplete_jsx_hook);
}

pub fn autocomplete_unset_hooks() {
    AUTOCOMPLETE_CURSOR.with(|current| {
        *current.borrow_mut() = None;
    });
    type_inference_hooks_js::reset_hooks();
}
