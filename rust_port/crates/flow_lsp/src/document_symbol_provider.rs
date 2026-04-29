/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::Identifier;
use flow_parser::ast::PrivateName;
use flow_parser::ast::class;
use flow_parser::ast::expression;
use flow_parser::ast::expression::object;
use flow_parser::ast::function;
use flow_parser::ast::pattern;
use flow_parser::ast::statement;
use flow_parser::ast::types;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use lsp_types::DocumentSymbol;
use lsp_types::Location;
use lsp_types::Position;
use lsp_types::Range;
use lsp_types::SymbolInformation;
use lsp_types::SymbolKind;
use lsp_types::Url;

fn name_opt(name: &str) -> Option<String> {
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

fn name_and_loc_of_identifier(id: &Identifier<Loc, Loc>) -> Option<(String, Loc)> {
    name_opt(&id.name).map(|name| (name, id.loc.dupe()))
}

fn name_and_loc_of_private_name(pn: &PrivateName<Loc>) -> Option<(String, Loc)> {
    name_opt(&pn.name).map(|name| (format!("#{}", name), pn.loc.dupe()))
}

fn name_and_loc_of_key(key: &object::Key<Loc, Loc>) -> Option<(String, Loc)> {
    match key {
        object::Key::StringLiteral((loc, sl)) => name_opt(&sl.raw).map(|name| (name, loc.dupe())),
        object::Key::NumberLiteral((loc, nl)) => name_opt(&nl.raw).map(|name| (name, loc.dupe())),
        object::Key::BigIntLiteral((loc, bl)) => name_opt(&bl.raw).map(|name| (name, loc.dupe())),
        object::Key::Identifier(id) => name_and_loc_of_identifier(id),
        object::Key::PrivateName(pn) => name_and_loc_of_private_name(pn),
        object::Key::Computed(_) => None,
    }
}

fn name_and_loc_of_pattern(p: &pattern::Pattern<Loc, Loc>) -> Option<(String, Loc)> {
    let (p, _) = flow_parser::ast_utils::unwrap_nonnull_lhs(p);
    match p.as_ref() {
        pattern::Pattern::Identifier { inner, .. } => name_and_loc_of_identifier(&inner.name),
        pattern::Pattern::Object { .. }
        | pattern::Pattern::Array { .. }
        | pattern::Pattern::Expression { .. } => None,
    }
}

fn id_matches_name(name: &str, id: Option<&Identifier<Loc, Loc>>) -> bool {
    match id {
        None => true,
        Some(id) => id.name.as_str() == name,
    }
}

enum SpecialInitializer<'a> {
    Function(&'a Loc, &'a function::Function<Loc, Loc>),
    Class(&'a Loc, &'a class::Class<Loc, Loc>),
    Normal,
}

fn special_initializer<'a>(
    name: &str,
    expr: &'a expression::Expression<Loc, Loc>,
) -> SpecialInitializer<'a> {
    use expression::ExpressionInner;
    match &**expr {
        ExpressionInner::Function {
            loc: func_loc,
            inner: func,
        } if id_matches_name(name, func.id.as_ref()) => {
            SpecialInitializer::Function(func_loc, func.as_ref())
        }
        ExpressionInner::Class {
            loc: class_loc,
            inner: cls,
        } if id_matches_name(name, cls.id.as_ref()) => {
            SpecialInitializer::Class(class_loc, cls.as_ref())
        }
        _ => SpecialInitializer::Normal,
    }
}

fn kind_of_property(is_method: bool) -> SymbolKind {
    if is_method {
        SymbolKind::METHOD
    } else {
        SymbolKind::PROPERTY
    }
}

fn loc_to_lsp_range(loc: &Loc) -> Range {
    Range {
        start: Position {
            line: loc.start.line.saturating_sub(1) as u32,
            character: loc.start.column.max(0) as u32,
        },
        end: Position {
            line: loc.end.line.saturating_sub(1) as u32,
            character: loc.end.column.max(0) as u32,
        },
    }
}

fn mk(
    loc: &Loc,
    selection: &Loc,
    name: String,
    detail: Option<String>,
    kind: SymbolKind,
    children: Option<Vec<DocumentSymbol>>,
) -> DocumentSymbol {
    let range = loc_to_lsp_range(loc);
    let selection_range = loc_to_lsp_range(selection);
    #[allow(deprecated)]
    DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: Some(false),
        range,
        selection_range,
        children,
    }
}

struct Visitor {
    acc: Vec<DocumentSymbol>,
}

impl Visitor {
    fn new() -> Self {
        Visitor { acc: vec![] }
    }

    fn with_children<F, R>(
        &mut self,
        visit_fn: F,
        f: impl FnOnce(Vec<DocumentSymbol>) -> DocumentSymbol,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = std::mem::take(&mut self.acc);
        let result = visit_fn(self);
        let mut children = std::mem::take(&mut self.acc);
        children.reverse();
        let sym = f(children);
        self.acc = prev;
        self.acc.push(sym);
        result
    }

    fn add_with_children<F>(
        &mut self,
        loc: &Loc,
        selection: &Loc,
        name: String,
        detail: Option<String>,
        kind: SymbolKind,
        visit_fn: F,
    ) where
        F: FnOnce(&mut Self),
    {
        let loc = loc.dupe();
        let selection = selection.dupe();
        self.with_children(
            |this| visit_fn(this),
            |children| mk(&loc, &selection, name, detail, kind, Some(children)),
        );
    }

    fn add(
        &mut self,
        loc: &Loc,
        selection: &Loc,
        name: String,
        detail: Option<String>,
        kind: SymbolKind,
    ) {
        let sym = mk(loc, selection, name, detail, kind, None);
        self.acc.push(sym);
    }

    fn visit_special_initializer(
        &mut self,
        loc: &Loc,
        name: String,
        selection: &Loc,
        kind: SymbolKind,
        value: &expression::Expression<Loc, Loc>,
    ) {
        match special_initializer(&name, value) {
            SpecialInitializer::Function(func_loc, func) => {
                let kind = SymbolKind::METHOD;
                self.add_with_children(loc, selection, name, None, kind, |this| {
                    ast_visitor::function_expression_or_method_default(this, func_loc, func)
                        .into_ok();
                });
            }
            SpecialInitializer::Class(class_loc, cls) => {
                let kind = SymbolKind::CLASS;
                self.add_with_children(loc, selection, name, None, kind, |this| {
                    ast_visitor::class_default(this, class_loc, cls).into_ok();
                });
            }
            SpecialInitializer::Normal => {
                self.add_with_children(loc, selection, name, None, kind, |this| {
                    ast_visitor::expression_default(this, value).into_ok();
                });
            }
        }
    }

    fn function_decl_or_expr(
        &mut self,
        default_fn: fn(&mut Self, &Loc, &function::Function<Loc, Loc>) -> Result<(), !>,
        loc: &Loc,
        stmt: &function::Function<Loc, Loc>,
    ) {
        let (name, selection) = stmt
            .id
            .as_ref()
            .and_then(name_and_loc_of_identifier)
            .unwrap_or_else(|| ("<function>".to_string(), loc.dupe()));
        let kind = SymbolKind::FUNCTION;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            default_fn(this, loc, stmt).into_ok();
        });
    }

    fn class_decl_or_expr(
        &mut self,
        default_fn: fn(&mut Self, &Loc, &class::Class<Loc, Loc>) -> Result<(), !>,
        loc: &Loc,
        cls: &class::Class<Loc, Loc>,
    ) {
        let (name, selection) = cls
            .id
            .as_ref()
            .and_then(name_and_loc_of_identifier)
            .unwrap_or_else(|| ("<class>".to_string(), loc.dupe()));
        self.add_with_children(loc, &selection, name, None, SymbolKind::CLASS, |this| {
            default_fn(this, loc, cls).into_ok();
        });
    }
}

impl<'ast> AstVisitor<'ast, Loc> for Visitor {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        decl: &'ast statement::variable::Declarator<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &decl.loc;
        let (name, selection) =
            name_and_loc_of_pattern(&decl.id).unwrap_or_else(|| ("<var>".to_string(), loc.dupe()));
        let k = SymbolKind::VARIABLE;
        match &decl.init {
            Some(init) => {
                self.visit_special_initializer(loc, name, &selection, k, init);
            }
            None => {
                self.add_with_children(loc, &selection, name, None, k, |this| {
                    ast_visitor::variable_declarator_default(this, kind, decl).into_ok();
                });
            }
        }
        Ok(())
    }

    fn object_property(&mut self, prop: &'ast object::NormalProperty<Loc, Loc>) -> Result<(), !> {
        match prop {
            object::NormalProperty::Init {
                loc,
                key,
                value,
                shorthand: _,
            } => {
                if let Some((name, selection)) = name_and_loc_of_key(key) {
                    let kind = SymbolKind::PROPERTY;
                    self.visit_special_initializer(loc, name, &selection, kind, value);
                }
            }
            object::NormalProperty::Method { loc, key, .. } => {
                if let Some((name, selection)) = name_and_loc_of_key(key) {
                    let kind = SymbolKind::METHOD;
                    self.add_with_children(loc, &selection, name, None, kind, |this| {
                        ast_visitor::object_property_default(this, prop).into_ok();
                    });
                }
            }
            object::NormalProperty::Get { loc, key, .. } => {
                if let Some((name, selection)) = name_and_loc_of_key(key) {
                    let kind = SymbolKind::PROPERTY;
                    self.add_with_children(loc, &selection, name, None, kind, |this| {
                        ast_visitor::object_property_default(this, prop).into_ok();
                    });
                }
            }
            object::NormalProperty::Set { loc, key, .. } => {
                if let Some((name, selection)) = name_and_loc_of_key(key) {
                    let kind = SymbolKind::PROPERTY;
                    self.add_with_children(loc, &selection, name, None, kind, |this| {
                        ast_visitor::object_property_default(this, prop).into_ok();
                    });
                }
            }
        }
        Ok(())
    }

    fn component_declaration(
        &mut self,
        loc: &'ast Loc,
        component: &'ast statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&component.id)
            .unwrap_or_else(|| ("<component>".to_string(), loc.dupe()));
        let k = SymbolKind::FUNCTION;
        self.add_with_children(loc, &selection, name, None, k, |this| {
            ast_visitor::component_declaration_default(this, loc, component).into_ok();
        });
        Ok(())
    }

    fn declare_component(
        &mut self,
        loc: &'ast Loc,
        component: &'ast statement::DeclareComponent<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&component.id)
            .unwrap_or_else(|| ("<component>".to_string(), loc.dupe()));
        let k = SymbolKind::FUNCTION;
        self.add_with_children(loc, &selection, name, None, k, |this| {
            ast_visitor::declare_component_default(this, loc, component).into_ok();
        });
        Ok(())
    }

    fn class_declaration(
        &mut self,
        loc: &'ast Loc,
        cls: &'ast class::Class<Loc, Loc>,
    ) -> Result<(), !> {
        self.class_decl_or_expr(
            |this, loc, cls| ast_visitor::class_declaration_default(this, loc, cls),
            loc,
            cls,
        );
        Ok(())
    }

    fn class_expression(
        &mut self,
        loc: &'ast Loc,
        cls: &'ast class::Class<Loc, Loc>,
    ) -> Result<(), !> {
        self.class_decl_or_expr(
            |this, loc, cls| ast_visitor::class_expression_default(this, loc, cls),
            loc,
            cls,
        );
        Ok(())
    }

    fn class_method(&mut self, meth: &'ast class::Method<Loc, Loc>) -> Result<(), !> {
        let loc = &meth.loc;
        let (name, selection) =
            name_and_loc_of_key(&meth.key).unwrap_or_else(|| ("<method>".to_string(), loc.dupe()));
        let (kind, name) = match meth.kind {
            class::MethodKind::Constructor => (SymbolKind::CONSTRUCTOR, name),
            class::MethodKind::Method => (SymbolKind::METHOD, name),
            class::MethodKind::Get => (SymbolKind::PROPERTY, format!("(get) {}", name)),
            class::MethodKind::Set => (SymbolKind::PROPERTY, format!("(set) {}", name)),
        };
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::class_method_default(this, meth).into_ok();
        });
        Ok(())
    }

    fn class_property(&mut self, prop: &'ast class::Property<Loc, Loc>) -> Result<(), !> {
        let loc = &prop.loc;
        let (name, selection) = name_and_loc_of_key(&prop.key)
            .unwrap_or_else(|| ("<property>".to_string(), loc.dupe()));
        let kind = SymbolKind::PROPERTY;
        match &prop.value {
            class::property::Value::Initialized(expr) => {
                self.visit_special_initializer(loc, name, &selection, kind, expr);
            }
            class::property::Value::Declared | class::property::Value::Uninitialized => {
                self.add_with_children(loc, &selection, name, None, kind, |this| {
                    ast_visitor::class_property_default(this, prop).into_ok();
                });
            }
        }
        Ok(())
    }

    fn class_private_field(&mut self, prop: &'ast class::PrivateField<Loc, Loc>) -> Result<(), !> {
        let loc = &prop.loc;
        let (name, selection) = name_and_loc_of_private_name(&prop.key)
            .unwrap_or_else(|| ("<property>".to_string(), loc.dupe()));
        let kind = SymbolKind::PROPERTY;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::class_private_field_default(this, prop).into_ok();
        });
        Ok(())
    }

    fn declare_class(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::DeclareClass<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&decl.id)
            .unwrap_or_else(|| ("<class>".to_string(), loc.dupe()));
        let kind = SymbolKind::CLASS;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::declare_class_default(this, loc, decl).into_ok();
        });
        Ok(())
    }

    fn declare_function(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::DeclareFunction<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = decl
            .id
            .as_ref()
            .and_then(name_and_loc_of_identifier)
            .unwrap_or_else(|| ("<function>".to_string(), loc.dupe()));
        let kind = SymbolKind::FUNCTION;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::declare_function_default(this, loc, decl).into_ok();
        });
        Ok(())
    }

    fn declare_module(
        &mut self,
        loc: &'ast Loc,
        m: &'ast statement::DeclareModule<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = match &m.id {
            statement::declare_module::Id::Identifier(id) => name_and_loc_of_identifier(id)
                .unwrap_or_else(|| ("<module>".to_string(), loc.dupe())),
            statement::declare_module::Id::Literal((lit_loc, sl)) => {
                (format!("{:?}", sl.value.as_str()), lit_loc.dupe())
            }
        };
        let kind = SymbolKind::MODULE;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::declare_module_default(this, loc, m).into_ok();
        });
        Ok(())
    }

    fn declare_variable(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::DeclareVariable<Loc, Loc>,
    ) -> Result<(), !> {
        for d in decl.declarations.iter() {
            let (name, selection) =
                name_and_loc_of_pattern(&d.id).unwrap_or_else(|| ("<var>".to_string(), loc.dupe()));
            let k = SymbolKind::VARIABLE;
            self.add_with_children(loc, &selection, name, None, k, |this| {
                ast_visitor::variable_declarator_default(this, decl.kind, d).into_ok();
            });
        }
        Ok(())
    }

    fn enum_declaration(
        &mut self,
        loc: &'ast Loc,
        enum_: &'ast statement::EnumDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&enum_.id)
            .unwrap_or_else(|| ("<enum>".to_string(), loc.dupe()));
        let kind = SymbolKind::ENUM;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::enum_declaration_default(this, loc, enum_).into_ok();
        });
        Ok(())
    }

    fn enum_member_identifier(&mut self, id: &'ast Identifier<Loc, Loc>) -> Result<(), !> {
        if let Some((name, loc)) = name_and_loc_of_identifier(id) {
            let kind = SymbolKind::ENUM_MEMBER;
            self.add(&loc, &loc, name, None, kind);
        }
        Ok(())
    }

    fn export_default_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::ExportDefaultDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        let name = "default".to_string();
        let kind = SymbolKind::VARIABLE;
        let default = decl.default.dupe();
        self.add_with_children(loc, &default, name, None, kind, |this| {
            ast_visitor::export_default_declaration_default(this, loc, decl).into_ok();
        });
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &'ast Loc,
        stmt: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        self.function_decl_or_expr(
            |this, loc, stmt| ast_visitor::function_declaration_default(this, loc, stmt),
            loc,
            stmt,
        );
        Ok(())
    }

    fn function_expression_or_method(
        &mut self,
        loc: &'ast Loc,
        stmt: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        self.function_decl_or_expr(
            |this, loc, stmt| ast_visitor::function_expression_or_method_default(this, loc, stmt),
            loc,
            stmt,
        );
        Ok(())
    }

    fn interface(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast statement::Interface<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&decl.id)
            .unwrap_or_else(|| ("<interface>".to_string(), loc.dupe()));
        let kind = SymbolKind::INTERFACE;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::interface_default(this, loc, decl).into_ok();
        });
        Ok(())
    }

    fn object_property_type(
        &mut self,
        prop: &'ast types::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &prop.loc;
        match name_and_loc_of_key(&prop.key) {
            Some((name, selection)) => {
                let kind = kind_of_property(prop.method);
                self.add_with_children(loc, &selection, name, None, kind, |this| {
                    ast_visitor::object_property_type_default(this, prop).into_ok();
                });
            }
            None => {}
        }
        Ok(())
    }

    fn object_indexer_property_type(
        &mut self,
        indexer: &'ast types::object::Indexer<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &indexer.loc;
        let (name, selection) = indexer
            .id
            .as_ref()
            .and_then(name_and_loc_of_identifier)
            .unwrap_or_else(|| ("".to_string(), indexer.key.loc().dupe()));
        let name = format!("[{}]", name);
        let kind = SymbolKind::PROPERTY;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::object_indexer_property_type_default(this, indexer).into_ok();
        });
        Ok(())
    }

    fn object_internal_slot_property_type(
        &mut self,
        slot: &'ast types::object::InternalSlot<Loc, Loc>,
    ) -> Result<(), !> {
        let loc = &slot.loc;
        let (name, selection) =
            name_and_loc_of_identifier(&slot.id).unwrap_or_else(|| ("".to_string(), loc.dupe()));
        let name = format!("[[{}]]", name);
        let kind = kind_of_property(slot.method);
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::object_internal_slot_property_type_default(this, slot).into_ok();
        });
        Ok(())
    }

    // TODO

    fn opaque_type(
        &mut self,
        loc: &'ast Loc,
        otype: &'ast statement::OpaqueType<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&otype.id)
            .unwrap_or_else(|| ("<opaque>".to_string(), loc.dupe()));
        let kind = SymbolKind::VARIABLE;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::opaque_type_default(this, loc, otype).into_ok();
        });
        Ok(())
    }

    fn type_alias(
        &mut self,
        loc: &'ast Loc,
        stuff: &'ast statement::TypeAlias<Loc, Loc>,
    ) -> Result<(), !> {
        let (name, selection) = name_and_loc_of_identifier(&stuff.id)
            .unwrap_or_else(|| ("<type>".to_string(), loc.dupe()));
        let kind = SymbolKind::VARIABLE;
        self.add_with_children(loc, &selection, name, None, kind, |this| {
            ast_visitor::type_alias_default(this, loc, stuff).into_ok();
        });
        Ok(())
    }
}

pub fn provide_document_symbols(program: &ast::Program<Loc, Loc>) -> Vec<DocumentSymbol> {
    let mut finder = Visitor::new();
    finder.program(program).into_ok();
    finder.acc
}

pub fn provide_symbol_information(
    uri: &Url,
    program: &ast::Program<Loc, Loc>,
) -> Vec<SymbolInformation> {
    fn flatten(
        uri: &Url,
        container_name: Option<String>,
        acc: &mut Vec<SymbolInformation>,
        items: &[DocumentSymbol],
    ) {
        for next in items {
            let location = Location {
                uri: uri.clone(),
                range: next.range,
            };
            #[allow(deprecated)]
            let info = SymbolInformation {
                name: next.name.clone(),
                kind: next.kind,
                location,
                container_name: container_name.clone(),
                tags: None,
                deprecated: None,
            };
            acc.push(info);
            if let Some(children) = &next.children {
                flatten(uri, Some(next.name.clone()), acc, children);
            }
        }
    }

    let doc_symbols = provide_document_symbols(program);
    let mut acc = Vec::new();
    flatten(uri, None, &mut acc, &doc_symbols);
    acc
}
