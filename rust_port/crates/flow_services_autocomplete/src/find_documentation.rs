/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
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

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, FoundJsdoc> for JsdocDocumentationSearcher<'_> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn function_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::function::Function<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, decl.comments.as_ref())?;
        ast_visitor::function_declaration_default(self, loc, decl)
    }

    fn declare_function(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::statement::DeclareFunction<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, decl.comments.as_ref())?;
        ast_visitor::declare_function_default(self, loc, decl)
    }

    fn class_declaration(
        &mut self,
        loc: &'ast Loc,
        class: &'ast ast::class::Class<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, class.comments.as_ref())?;
        ast_visitor::class_declaration_default(self, loc, class)
    }

    fn type_alias(
        &mut self,
        loc: &'ast Loc,
        alias: &'ast ast::statement::TypeAlias<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, alias.comments.as_ref())?;
        ast_visitor::type_alias_default(self, loc, alias)
    }

    fn opaque_type(
        &mut self,
        loc: &'ast Loc,
        opaque: &'ast ast::statement::OpaqueType<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, opaque.comments.as_ref())?;
        ast_visitor::opaque_type_default(self, loc, opaque)
    }

    fn interface(
        &mut self,
        loc: &'ast Loc,
        interface: &'ast ast::statement::Interface<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, interface.comments.as_ref())?;
        ast_visitor::interface_default(self, loc, interface)
    }

    fn enum_declaration(
        &mut self,
        loc: &'ast Loc,
        enum_: &'ast ast::statement::EnumDeclaration<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, enum_.comments.as_ref())?;
        ast_visitor::enum_declaration_default(self, loc, enum_)
    }

    fn variable_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::statement::VariableDeclaration<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(loc, decl.comments.as_ref())?;
        ast_visitor::variable_declaration_default(self, loc, decl)
    }

    fn class_method(
        &mut self,
        method: &'ast ast::class::Method<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(&method.loc, method.comments.as_ref())?;
        ast_visitor::class_method_default(self, method)
    }

    fn class_property(
        &mut self,
        prop: &'ast ast::class::Property<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(&prop.loc, prop.comments.as_ref())?;
        ast_visitor::class_property_default(self, prop)
    }

    fn class_private_field(
        &mut self,
        prop: &'ast ast::class::PrivateField<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(&prop.loc, prop.comments.as_ref())?;
        ast_visitor::class_private_field_default(self, prop)
    }

    fn object_property_type(
        &mut self,
        prop: &'ast ast::types::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), FoundJsdoc> {
        self.find_jsdoc(&prop.loc, prop.comments.as_ref())?;
        ast_visitor::object_property_type_default(self, prop)
    }
}

fn search_jsdoc(def_loc: &Loc, ast: &ast::Program<Loc, Loc>) -> Option<jsdoc::Jsdoc> {
    let mut searcher = JsdocDocumentationSearcher {
        target_loc: def_loc,
    };
    match searcher.program(ast) {
        Ok(()) => None,
        Err(FoundJsdoc::Found(jsdoc)) => Some(jsdoc),
    }
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
