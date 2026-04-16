/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast_utils::loc_of_annotation_or_hint;
use flow_parser::ast_utils::loc_of_return_annot;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::jsdoc;
use flow_parser::loc::Loc;

enum FoundJsdoc {
    Found(jsdoc::Jsdoc),
}

struct JsdocDocumentationSearcher<'a> {
    target_loc: &'a Loc,
}

impl<'a> JsdocDocumentationSearcher<'a> {
    fn find_jsdoc<T: Dupe>(
        &self,
        found_loc: &Loc,
        comments: Option<&ast::Syntax<Loc, T>>,
    ) -> Result<(), FoundJsdoc> {
        if *found_loc == *self.target_loc {
            if let Some((_, jsdoc)) = jsdoc::of_comments(comments) {
                return Err(FoundJsdoc::Found(jsdoc));
            }
        }
        Ok(())
    }
}

fn loc_of_object_key(key: &ast::expression::object::Key<Loc, Loc>) -> &Loc {
    match key {
        ast::expression::object::Key::Identifier(id) => &id.loc,
        ast::expression::object::Key::StringLiteral((loc, _))
        | ast::expression::object::Key::NumberLiteral((loc, _))
        | ast::expression::object::Key::BigIntLiteral((loc, _)) => loc,
        ast::expression::object::Key::Computed(computed) => computed.expression.loc(),
        ast::expression::object::Key::PrivateName(private_name) => &private_name.loc,
    }
}

fn comments_of_variance(variance: Option<&ast::Variance<Loc>>) -> Option<&ast::Syntax<Loc, ()>> {
    variance.and_then(|variance| variance.comments.as_ref())
}

fn comments_of_object_key(
    key: &ast::expression::object::Key<Loc, Loc>,
) -> Option<&ast::Syntax<Loc, ()>> {
    match key {
        ast::expression::object::Key::Identifier(id) => id.comments.as_ref(),
        ast::expression::object::Key::StringLiteral((_, lit)) => lit.comments.as_ref(),
        ast::expression::object::Key::NumberLiteral((_, lit)) => lit.comments.as_ref(),
        ast::expression::object::Key::BigIntLiteral((_, lit)) => lit.comments.as_ref(),
        ast::expression::object::Key::Computed(computed) => computed.comments.as_ref(),
        ast::expression::object::Key::PrivateName(_) => None,
    }
}

fn replace_comments_of_statement(
    stmt: &ast::statement::Statement<Loc, Loc>,
    comments: Option<&ast::Syntax<Loc, ()>>,
) -> ast::statement::Statement<Loc, Loc> {
    match stmt.deref() {
        ast::statement::StatementInner::TypeAlias { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::TypeAlias {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::OpaqueType { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::OpaqueType {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::InterfaceDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::InterfaceDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::VariableDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::VariableDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::ComponentDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::ComponentDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::ClassDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::ClassDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::RecordDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::RecordDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::FunctionDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::FunctionDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::EnumDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::EnumDeclaration {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareEnum { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareEnum {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareVariable { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareVariable {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareFunction { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareFunction {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareClass { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareClass {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareComponent { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareComponent {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareTypeAlias { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareTypeAlias {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareOpaqueType { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareOpaqueType {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::DeclareInterface { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareInterface {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::ExportAssignment { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(ast::statement::StatementInner::ExportAssignment {
                loc: loc.dupe(),
                inner: Arc::new(inner),
            })
        }
        ast::statement::StatementInner::NamespaceExportDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(
                ast::statement::StatementInner::NamespaceExportDeclaration {
                    loc: loc.dupe(),
                    inner: Arc::new(inner),
                },
            )
        }
        ast::statement::StatementInner::ImportEqualsDeclaration { loc, inner } => {
            let mut inner = (**inner).clone();
            inner.comments = comments.cloned();
            ast::statement::Statement::new(
                ast::statement::StatementInner::ImportEqualsDeclaration {
                    loc: loc.dupe(),
                    inner: Arc::new(inner),
                },
            )
        }
        _ => stmt.clone(),
    }
}

fn loc_of_type_return_annot(annot: &ast::types::function::ReturnAnnotation<Loc, Loc>) -> &Loc {
    match annot {
        ast::types::function::ReturnAnnotation::Missing(loc) => loc,
        ast::types::function::ReturnAnnotation::Available(annot) => &annot.loc,
        ast::types::function::ReturnAnnotation::TypeGuard(type_guard) => &type_guard.loc,
    }
}

fn enum_member_name_comments<E, F>(
    mut find: F,
    member_loc: &Loc,
    id: &ast::statement::enum_declaration::MemberName<Loc>,
) -> Result<(), E>
where
    F: FnMut(&Loc, Option<&ast::Syntax<Loc, ()>>) -> Result<(), E>,
{
    match id {
        ast::statement::enum_declaration::MemberName::Identifier(id) => {
            find(member_loc, id.comments.as_ref())?;
            find(&id.loc, id.comments.as_ref())
        }
        ast::statement::enum_declaration::MemberName::StringLiteral(loc, lit) => {
            find(member_loc, lit.comments.as_ref())?;
            find(loc, lit.comments.as_ref())
        }
    }
}

macro_rules! impl_jsdoc_documentation_searcher {
    ($name:ident, $err:ty, $find_method:ident) => {
        impl<'ast, 'a> AstVisitor<'ast, Loc, Loc, &'ast Loc, $err> for $name<'a> {
            fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
                loc
            }

            fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
                type_
            }

            fn variable_declaration(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::VariableDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                for declaration in decl.declarations.iter() {
                    if let ast::pattern::Pattern::Identifier { inner, .. } = &declaration.id {
                        self.$find_method(&inner.name.loc, comments)?;
                        self.$find_method(loc_of_annotation_or_hint(&inner.annot), comments)?;
                        if let Some(init) = &declaration.init {
                            self.$find_method(init.loc(), comments)?;
                        }
                    }
                }
                ast_visitor::variable_declaration_default(self, loc, decl)
            }

            fn class_(
                &mut self,
                loc: &'ast Loc,
                class: &'ast ast::class::Class<Loc, Loc>,
            ) -> Result<(), $err> {
                if let Some(id) = &class.id {
                    self.$find_method(&id.loc, class.comments.as_ref())?;
                }
                ast_visitor::class_default(self, loc, class)
            }

            fn function_(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::function::Function<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                self.$find_method(loc, comments)?;
                self.$find_method(&decl.sig_loc, comments)?;
                if let Some(id) = &decl.id {
                    self.$find_method(&id.loc, comments)?;
                }
                ast_visitor::function_default(self, loc, decl)
            }

            fn component_declaration(
                &mut self,
                loc: &'ast Loc,
                component: &'ast ast::statement::ComponentDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = component.comments.as_ref();
                self.$find_method(loc, comments)?;
                self.$find_method(&component.sig_loc, comments)?;
                self.$find_method(&component.id.loc, comments)?;
                ast_visitor::component_declaration_default(self, loc, component)
            }

            fn component_param_name(
                &mut self,
                param_name: &'ast ast::statement::component_params::ParamName<Loc, Loc>,
            ) -> Result<(), $err> {
                match param_name {
                    ast::statement::component_params::ParamName::Identifier(id) => {
                        self.$find_method(&id.loc, id.comments.as_ref())?;
                    }
                    ast::statement::component_params::ParamName::StringLiteral((lit_loc, lit)) => {
                        self.$find_method(lit_loc, lit.comments.as_ref())?;
                    }
                }
                ast_visitor::component_param_name_default(self, param_name)
            }

            fn component_param_pattern(
                &mut self,
                pattern: &'ast ast::pattern::Pattern<Loc, Loc>,
            ) -> Result<(), $err> {
                if let ast::pattern::Pattern::Identifier { inner, .. } = pattern {
                    self.$find_method(&inner.name.loc, inner.name.comments.as_ref())?;
                }
                ast_visitor::component_param_pattern_default(self, pattern)
            }

            fn component_rest_param(
                &mut self,
                expr: &'ast ast::statement::component_params::RestParam<Loc, Loc>,
            ) -> Result<(), $err> {
                if let ast::pattern::Pattern::Identifier { inner, .. } = &expr.argument {
                    self.$find_method(&inner.name.loc, expr.comments.as_ref())?;
                    self.$find_method(&inner.name.loc, inner.name.comments.as_ref())?;
                }
                ast_visitor::component_rest_param_default(self, expr)
            }

            fn declare_variable(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::DeclareVariable<Loc, Loc>,
            ) -> Result<(), $err> {
                if let Some(declaration) = decl.declarations.first() {
                    if let ast::pattern::Pattern::Identifier { inner, .. } = &declaration.id {
                        self.$find_method(&inner.name.loc, decl.comments.as_ref())?;
                    }
                }
                ast_visitor::declare_variable_default(self, loc, decl)
            }

            fn declare_class(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::DeclareClass<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(&decl.id.loc, decl.comments.as_ref())?;
                ast_visitor::declare_class_default(self, loc, decl)
            }

            fn declare_function(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::DeclareFunction<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                if let Some(id) = &decl.id {
                    self.$find_method(&id.loc, comments)?;
                }
                self.$find_method(&decl.annot.loc, comments)?;
                ast_visitor::declare_function_default(self, loc, decl)
            }

            fn declare_export_declaration(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::DeclareExportDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                if let Some(default) = &decl.default {
                    self.$find_method(default, comments)?;
                }
                if let Some(declaration) = &decl.declaration {
                    match declaration {
                        ast::statement::declare_export_declaration::Declaration::Variable {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareVariable {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Function {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareFunction {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Class {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareClass {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Component {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareComponent {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::DefaultType {
                            type_,
                        } => {
                            self.$find_method(type_.loc(), comments)?;
                            self.type_(type_)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::NamedType {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareTypeAlias {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::NamedOpaqueType {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareOpaqueType {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Interface {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareInterface {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Enum {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareEnum {
                                    loc: loc.dupe(),
                                    inner: declaration.clone(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                        ast::statement::declare_export_declaration::Declaration::Namespace {
                            loc,
                            declaration,
                        } => {
                            let stmt = ast::statement::Statement::new(
                                ast::statement::StatementInner::DeclareNamespace {
                                    loc: loc.dupe(),
                                    inner: declaration.clone().into(),
                                },
                            );
                            let stmt = replace_comments_of_statement(&stmt, comments);
                            self.statement(&stmt)?;
                        }
                    }
                }
                ast_visitor::declare_export_declaration_default(self, loc, decl)
            }

            fn object_property_type(
                &mut self,
                prop: &'ast ast::types::object::NormalProperty<Loc, Loc>,
            ) -> Result<(), $err> {
                let key_loc = loc_of_object_key(&prop.key);
                let value_loc = match &prop.value {
                    ast::types::object::PropertyValue::Init(Some(value_loc)) => value_loc.loc(),
                    ast::types::object::PropertyValue::Get(_, func) => {
                        loc_of_type_return_annot(&func.return_)
                    }
                    ast::types::object::PropertyValue::Set(value_loc, _) => value_loc,
                    ast::types::object::PropertyValue::Init(None) => key_loc,
                };
                self.$find_method(key_loc, prop.comments.as_ref())?;
                self.$find_method(value_loc, prop.comments.as_ref())?;
                let variance_comments = comments_of_variance(prop.variance.as_ref());
                self.$find_method(key_loc, variance_comments)?;
                self.$find_method(value_loc, variance_comments)?;
                let key_comments = comments_of_object_key(&prop.key);
                self.$find_method(key_loc, key_comments)?;
                self.$find_method(value_loc, key_comments)?;
                ast_visitor::object_property_type_default(self, prop)
            }

            fn class_method(
                &mut self,
                method: &'ast ast::class::Method<Loc, Loc>,
            ) -> Result<(), $err> {
                let key_loc = loc_of_object_key(&method.key);
                self.$find_method(&method.loc, method.comments.as_ref())?;
                self.$find_method(key_loc, method.comments.as_ref())?;
                self.$find_method(key_loc, comments_of_object_key(&method.key))?;
                self.$find_method(&method.loc, comments_of_object_key(&method.key))?;
                ast_visitor::class_method_default(self, method)
            }

            fn class_property(
                &mut self,
                prop: &'ast ast::class::Property<Loc, Loc>,
            ) -> Result<(), $err> {
                let key_loc = loc_of_object_key(&prop.key);
                self.$find_method(key_loc, prop.comments.as_ref())?;
                self.$find_method(key_loc, comments_of_variance(prop.variance.as_ref()))?;
                self.$find_method(key_loc, comments_of_object_key(&prop.key))?;
                ast_visitor::class_property_default(self, prop)
            }

            fn class_private_field(
                &mut self,
                prop: &'ast ast::class::PrivateField<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(&prop.loc, prop.comments.as_ref())?;
                ast_visitor::class_private_field_default(self, prop)
            }

            fn object_property(
                &mut self,
                prop: &'ast ast::expression::object::NormalProperty<Loc, Loc>,
            ) -> Result<(), $err> {
                match prop {
                    ast::expression::object::NormalProperty::Init { key, value, .. } => {
                        let key_comments = comments_of_object_key(key);
                        match value.deref() {
                            ast::expression::ExpressionInner::Function { loc: value_loc, inner } => {
                                self.$find_method(&inner.sig_loc, key_comments)?;
                                self.$find_method(loc_of_object_key(key), key_comments)?;
                                self.$find_method(value_loc, key_comments)?;
                            }
                            _ => {
                                self.$find_method(loc_of_object_key(key), key_comments)?;
                                self.$find_method(value.loc(), key_comments)?;
                            }
                        }
                    }
                    ast::expression::object::NormalProperty::Method {
                        loc,
                        key,
                        value: (_, func),
                    } => {
                        self.$find_method(&func.sig_loc, comments_of_object_key(key))?;
                        self.$find_method(loc, comments_of_object_key(key))?;
                        self.$find_method(loc_of_object_key(key), comments_of_object_key(key))?;
                        self.$find_method(&func.sig_loc, func.comments.as_ref())?;
                        self.$find_method(loc, func.comments.as_ref())?;
                        self.$find_method(loc_of_object_key(key), func.comments.as_ref())?;
                    }
                    ast::expression::object::NormalProperty::Get {
                        loc,
                        key,
                        value: (_, func),
                        comments,
                    } => {
                        self.$find_method(loc, comments.as_ref())?;
                        self.$find_method(loc_of_object_key(key), comments.as_ref())?;
                        self.$find_method(loc_of_return_annot(&func.return_), comments.as_ref())?;
                    }
                    ast::expression::object::NormalProperty::Set { .. } => {}
                }
                ast_visitor::object_property_default(self, prop)
            }

            fn enum_declaration(
                &mut self,
                loc: &'ast Loc,
                enum_: &'ast ast::statement::EnumDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(loc, enum_.comments.as_ref())?;
                self.$find_method(&enum_.id.loc, enum_.comments.as_ref())?;
                ast_visitor::enum_declaration_default(self, loc, enum_)
            }

            fn enum_defaulted_member(
                &mut self,
                member: &'ast ast::statement::enum_declaration::DefaultedMember<Loc>,
            ) -> Result<(), $err> {
                enum_member_name_comments(
                    |found_loc, comments| self.$find_method(found_loc, comments),
                    &member.loc,
                    &member.id,
                )?;
                ast_visitor::enum_defaulted_member_default(self, member)
            }

            fn enum_boolean_member(
                &mut self,
                member: &'ast ast::statement::enum_declaration::InitializedMember<
                    ast::BooleanLiteral<Loc>,
                    Loc,
                >,
            ) -> Result<(), $err> {
                enum_member_name_comments(
                    |found_loc, comments| self.$find_method(found_loc, comments),
                    &member.loc,
                    &member.id,
                )?;
                ast_visitor::enum_boolean_member_default(self, member)
            }

            fn enum_number_member(
                &mut self,
                member: &'ast ast::statement::enum_declaration::InitializedMember<
                    ast::NumberLiteral<Loc>,
                    Loc,
                >,
            ) -> Result<(), $err> {
                enum_member_name_comments(
                    |found_loc, comments| self.$find_method(found_loc, comments),
                    &member.loc,
                    &member.id,
                )?;
                ast_visitor::enum_number_member_default(self, member)
            }

            fn enum_string_member(
                &mut self,
                member: &'ast ast::statement::enum_declaration::InitializedMember<
                    ast::StringLiteral<Loc>,
                    Loc,
                >,
            ) -> Result<(), $err> {
                enum_member_name_comments(
                    |found_loc, comments| self.$find_method(found_loc, comments),
                    &member.loc,
                    &member.id,
                )?;
                ast_visitor::enum_string_member_default(self, member)
            }

            fn enum_bigint_member(
                &mut self,
                member: &'ast ast::statement::enum_declaration::InitializedMember<
                    ast::BigIntLiteral<Loc>,
                    Loc,
                >,
            ) -> Result<(), $err> {
                enum_member_name_comments(
                    |found_loc, comments| self.$find_method(found_loc, comments),
                    &member.loc,
                    &member.id,
                )?;
                ast_visitor::enum_bigint_member_default(self, member)
            }

            fn export_named_declaration(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::ExportNamedDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                self.$find_method(loc, comments)?;
                if let Some(declaration) = &decl.declaration {
                    let declaration = replace_comments_of_statement(declaration, comments);
                    self.statement(&declaration)?;
                }
                ast_visitor::export_named_declaration_default(self, loc, decl)
            }

            fn export_default_declaration(
                &mut self,
                loc: &'ast Loc,
                decl: &'ast ast::statement::ExportDefaultDeclaration<Loc, Loc>,
            ) -> Result<(), $err> {
                let comments = decl.comments.as_ref();
                self.$find_method(&decl.default, comments)?;
                match &decl.declaration {
                    ast::statement::export_default_declaration::Declaration::Declaration(stmt) => {
                        let stmt = replace_comments_of_statement(stmt, comments);
                        self.statement(&stmt)?;
                    }
                    ast::statement::export_default_declaration::Declaration::Expression(expr) => {
                        match expr.deref() {
                            ast::expression::ExpressionInner::TypeCast { inner, .. } => {
                                self.$find_method(inner.annot.annotation.loc(), comments)?;
                            }
                            ast::expression::ExpressionInner::AsExpression { inner, .. } => {
                                self.$find_method(inner.annot.annotation.loc(), comments)?;
                            }
                            _ => {
                                self.$find_method(expr.loc(), comments)?;
                            }
                        }
                    }
                }
                ast_visitor::export_default_declaration_default(self, loc, decl)
            }

            fn type_alias(
                &mut self,
                loc: &'ast Loc,
                alias: &'ast ast::statement::TypeAlias<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(loc, alias.comments.as_ref())?;
                self.$find_method(&alias.id.loc, alias.comments.as_ref())?;
                self.$find_method(alias.right.loc(), alias.comments.as_ref())?;
                ast_visitor::type_alias_default(self, loc, alias)
            }

            fn opaque_type(
                &mut self,
                loc: &'ast Loc,
                opaque: &'ast ast::statement::OpaqueType<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(&opaque.id.loc, opaque.comments.as_ref())?;
                ast_visitor::opaque_type_default(self, loc, opaque)
            }

            fn interface(
                &mut self,
                loc: &'ast Loc,
                interface: &'ast ast::statement::Interface<Loc, Loc>,
            ) -> Result<(), $err> {
                self.$find_method(loc, interface.comments.as_ref())?;
                self.$find_method(&interface.id.loc, interface.comments.as_ref())?;
                ast_visitor::interface_default(self, loc, interface)
            }
        }
    };
}

impl_jsdoc_documentation_searcher!(JsdocDocumentationSearcher, FoundJsdoc, find_jsdoc);

fn search_jsdoc(def_loc: &Loc, ast: &ast::Program<Loc, Loc>) -> Option<jsdoc::Jsdoc> {
    let mut searcher = JsdocDocumentationSearcher {
        target_loc: def_loc,
    };
    match searcher.program(ast) {
        Ok(()) => None,
        Err(FoundJsdoc::Found(jsdoc)) => Some(jsdoc),
    }
}

fn search_jsdocs(
    target_locs: &BTreeSet<Loc>,
    ast: &ast::Program<Loc, Loc>,
) -> BTreeMap<Loc, jsdoc::Jsdoc> {
    let mut results = BTreeMap::new();
    let mut searcher = JsdocBatchSearcher {
        target_locs,
        results: &mut results,
    };
    match searcher.program(ast) {
        Ok(()) | Err(()) => {}
    }
    results
}

struct JsdocBatchSearcher<'a> {
    target_locs: &'a BTreeSet<Loc>,
    results: &'a mut BTreeMap<Loc, jsdoc::Jsdoc>,
}

impl<'a> JsdocBatchSearcher<'a> {
    fn find_jsdoc_batch<T: Dupe>(
        &mut self,
        found_loc: &Loc,
        comments: Option<&ast::Syntax<Loc, T>>,
    ) -> Result<(), ()> {
        if self.target_locs.contains(found_loc) {
            if let Some((_, jsdoc)) = jsdoc::of_comments(comments) {
                self.results.insert(found_loc.clone(), jsdoc);
            }
        }
        Ok(())
    }
}

impl_jsdoc_documentation_searcher!(JsdocBatchSearcher, (), find_jsdoc_batch);

struct DefLocToCommentLocMapSearcher<'a> {
    map: &'a mut BTreeMap<Loc, Loc>,
}

impl<'a> DefLocToCommentLocMapSearcher<'a> {
    fn add_to_map<T: Dupe>(
        &mut self,
        def_loc: &Loc,
        comments: Option<&ast::Syntax<Loc, T>>,
    ) -> Result<(), !> {
        if let Some(ast::Syntax { leading, .. }) = comments {
            if let Some(comment) = leading.last() {
                self.map
                    .entry(def_loc.clone())
                    .or_insert_with(|| comment.loc.clone());
            }
        }
        Ok(())
    }
}

impl_jsdoc_documentation_searcher!(DefLocToCommentLocMapSearcher, !, add_to_map);

pub fn jsdocs_of_getdef_locs(
    ast: &ast::Program<Loc, Loc>,
    get_ast_from_shared_mem: &dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    def_locs: &[Loc],
) -> BTreeMap<Loc, jsdoc::Jsdoc> {
    let mut by_file: BTreeMap<FileKey, BTreeSet<Loc>> = BTreeMap::new();
    for loc in def_locs {
        match loc.source() {
            None => {}
            Some(source) => {
                by_file
                    .entry(source.dupe())
                    .or_default()
                    .insert(loc.clone());
            }
        }
    }
    let get_file_ast = |source: &FileKey| -> Option<&ast::Program<Loc, Loc>> {
        match ast.loc.source() {
            Some(current_source) if *current_source == *source => Some(ast),
            _ => None,
        }
    };
    let mut results: BTreeMap<Loc, jsdoc::Jsdoc> = BTreeMap::new();
    for (source, locs) in &by_file {
        if let Some(file_ast) = get_file_ast(source) {
            let file_results = search_jsdocs(locs, file_ast);
            for (k, v) in file_results {
                results.entry(k).or_insert(v);
            }
        } else if let Some(file_ast) = get_ast_from_shared_mem(source) {
            let file_results = search_jsdocs(locs, &file_ast);
            for (k, v) in file_results {
                results.entry(k).or_insert(v);
            }
        }
    }
    results
}

pub fn jsdoc_of_getdef_loc(
    ast: &ast::Program<Loc, Loc>,
    get_ast_from_shared_mem: &dyn Fn(
        &flow_parser::file_key::FileKey,
    ) -> Option<ast::Program<Loc, Loc>>,
    def_loc: Loc,
) -> Option<jsdoc::Jsdoc> {
    let source = def_loc.source()?.clone();
    let current_ast_if_should_use = {
        let current_file_source = ast.loc.source()?;
        if *current_file_source == source {
            Some(ast.clone())
        } else {
            None
        }
    };
    let ast = match current_ast_if_should_use {
        Some(ast) => ast,
        None => get_ast_from_shared_mem(&source)?,
    };
    search_jsdoc(&def_loc, &ast)
}

pub fn documentation_of_jsdoc(jsdoc: &jsdoc::Jsdoc) -> Option<String> {
    let documentation_of_unrecognized_tag =
        |(tag_name, tag_description): &(String, Option<String>)| {
            let tag_name_documentation = format!("**@{tag_name}**");
            match tag_description {
                None => tag_name_documentation,
                Some(tag_description) => format!("{tag_name_documentation} {tag_description}"),
            }
        };
    let mut documentation_strings: Vec<String> = jsdoc
        .unrecognized_tags()
        .0
        .iter()
        .map(documentation_of_unrecognized_tag)
        .collect();
    if let Some(description) = jsdoc.description() {
        documentation_strings.insert(0, description.clone());
    }
    if let Some(description) = jsdoc.deprecated() {
        documentation_strings.insert(
            0,
            documentation_of_unrecognized_tag(&(
                "deprecated".to_string(),
                Some(description.clone()),
            )),
        );
    }
    if documentation_strings.is_empty() {
        None
    } else {
        Some(documentation_strings.join("\n\n"))
    }
}

pub fn def_loc_to_comment_loc_map(ast: &ast::Program<Loc, Loc>) -> BTreeMap<Loc, Loc> {
    let mut map = BTreeMap::new();
    let mut searcher = DefLocToCommentLocMapSearcher { map: &mut map };
    let Ok(()) = searcher.program(ast);
    map
}

pub fn module_doc_loc(ast: &ast::Program<Loc, Loc>) -> Option<Loc> {
    ast.comments
        .as_ref()
        .and_then(|comments| comments.leading.first().map(|comment| comment.loc.clone()))
}

enum FoundHardcodedDocumentation {
    Found(String),
}

struct HardcodedDocumentationSearcher<'a> {
    target_loc: &'a Loc,
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, FoundHardcodedDocumentation>
    for HardcodedDocumentationSearcher<'_>
{
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn render_type(
        &mut self,
        renders: &'ast ast::types::Renders<Loc, Loc>,
    ) -> Result<(), FoundHardcodedDocumentation> {
        let operator_loc = &renders.operator_loc;
        let doc = match renders.variant {
            ast::types::RendersVariant::Normal => {
                "`renders A` means that it will eventually render exactly one React element `A`."
            }
            ast::types::RendersVariant::Maybe => {
                "`renders? A` means that it will eventually render zero or one React element `A`."
            }
            ast::types::RendersVariant::Star => {
                "`renders* A` means that it will eventually render any amount of `A`."
            }
        };
        if operator_loc.contains(self.target_loc) {
            return Err(FoundHardcodedDocumentation::Found(doc.to_string()));
        }
        ast_visitor::render_type_default(self, renders)
    }
}

pub fn hardcoded_documentation_at_loc(
    ast: &ast::Program<Loc, Loc>,
    target_loc: Loc,
) -> Option<String> {
    let mut searcher = HardcodedDocumentationSearcher {
        target_loc: &target_loc,
    };
    match searcher.program(ast) {
        Ok(()) => None,
        Err(FoundHardcodedDocumentation::Found(doc)) => Some(doc),
    }
}
