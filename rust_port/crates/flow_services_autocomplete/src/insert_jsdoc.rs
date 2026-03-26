/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_common::reason;
use flow_parser::ast;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use crate::jsdoc_stub;

pub enum TargetLoc {
    LocOfTarget(Loc),
    LocOfComment(Loc),
}

struct Mapper {
    use_snippets: bool,
    target_loc: TargetLoc,
    inserted_stub: Option<String>,
}

impl Mapper {
    fn new(use_snippets: bool, target_loc: TargetLoc) -> Self {
        Self {
            use_snippets,
            target_loc,
            inserted_stub: None,
        }
    }

    fn covers_target(&self, loc: &Loc) -> bool {
        match &self.target_loc {
            TargetLoc::LocOfTarget(target_loc) => reason::in_range(target_loc, loc),
            TargetLoc::LocOfComment(_) => false,
        }
    }

    fn contains_comment_target(&self, comments: &[ast::Comment<Loc>]) -> bool {
        match &self.target_loc {
            TargetLoc::LocOfComment(target_loc) => {
                comments.iter().any(|comment| comment.loc == *target_loc)
            }
            TargetLoc::LocOfTarget(_) => false,
        }
    }

    fn string_of_stub(&self, stub: &jsdoc_stub::T) -> String {
        let trailing_space = match self.target_loc {
            TargetLoc::LocOfTarget(_) => true,
            TargetLoc::LocOfComment(_) => false,
        };
        let s = jsdoc_stub::string_of_stub(stub, self.use_snippets);
        if trailing_space { format!("{} ", s) } else { s }
    }

    fn comments_of_stub(&self, stub: &str) -> ast::Syntax<Loc, ()> {
        ast::Syntax {
            leading: Arc::from([ast_builder::comments::block(None, None, stub)]),
            trailing: Arc::from([]),
            internal: (),
        }
    }

    fn function_decl_with_stub(
        &mut self,
        decl: &ast::function::Function<Loc, Loc>,
    ) -> ast::function::Function<Loc, Loc> {
        let stub = self.string_of_stub(&jsdoc_stub::stub_for_function(decl));
        self.inserted_stub = Some(stub.clone());
        let comments = self.comments_of_stub(&stub);
        ast::function::Function {
            comments: Some(comments),
            ..decl.clone()
        }
    }

    fn declare_function_with_stub(
        &mut self,
        loc: &Loc,
        decl: &ast::statement::DeclareFunction<Loc, Loc>,
    ) -> ast::statement::DeclareFunction<Loc, Loc> {
        match &*decl.annot.annotation {
            TypeInner::Function {
                inner: func_type, ..
            } => match ast_builder::functions::of_type(None, None, None, None, None, func_type) {
                Some(func) => {
                    let stub = self.string_of_stub(&jsdoc_stub::stub_for_function(&func));
                    self.inserted_stub = Some(stub.clone());
                    let comments = ast::Syntax {
                        leading: Arc::from([ast_builder::comments::block(None, None, &stub)]),
                        trailing: Arc::from([]),
                        internal: (),
                    };
                    ast::statement::DeclareFunction {
                        comments: Some(comments),
                        ..decl.clone()
                    }
                }
                None => ast_visitor::map_declare_function_default(self, loc, decl),
            },
            _ => ast_visitor::map_declare_function_default(self, loc, decl),
        }
    }
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, !> for Mapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_function_declaration(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::function::Function<Loc, Loc>,
    ) -> ast::function::Function<Loc, Loc> {
        match decl {
            ast::function::Function {
                id: Some(id),
                comments: None,
                ..
            } if self.covers_target(&id.loc) => self.function_decl_with_stub(decl),
            ast::function::Function {
                comments: Some(comments),
                ..
            } if self.contains_comment_target(comments.leading.as_ref()) => {
                self.function_decl_with_stub(decl)
            }
            _ => ast_visitor::map_function_declaration_default(self, loc, decl),
        }
    }

    fn map_declare_function(
        &mut self,
        loc: &'ast Loc,
        decl: &'ast ast::statement::DeclareFunction<Loc, Loc>,
    ) -> ast::statement::DeclareFunction<Loc, Loc> {
        match decl {
            ast::statement::DeclareFunction {
                id: Some(id),
                comments: None,
                ..
            } if self.covers_target(&id.loc) => self.declare_function_with_stub(loc, decl),
            ast::statement::DeclareFunction {
                comments: Some(comments),
                ..
            } if self.contains_comment_target(comments.leading.as_ref()) => {
                self.declare_function_with_stub(loc, decl)
            }
            _ => ast_visitor::map_declare_function_default(self, loc, decl),
        }
    }
}

pub fn insert(
    use_snippets: bool,
    target_loc: TargetLoc,
    ast: &ast::Program<Loc, Loc>,
) -> Option<(ast::Program<Loc, Loc>, String)> {
    let mut mapper = Mapper::new(use_snippets, target_loc);
    let ast_prime = mapper.map_program(ast);
    mapper
        .inserted_stub
        .as_ref()
        .map(|inserted_stub| (ast_prime, inserted_stub.clone()))
}

pub fn insert_stub_for_target(
    use_snippets: bool,
    target_loc: Loc,
    ast: &ast::Program<Loc, Loc>,
) -> Option<(ast::Program<Loc, Loc>, String)> {
    insert(use_snippets, TargetLoc::LocOfTarget(target_loc), ast)
}

pub fn insert_stub_in_comment(
    use_snippets: bool,
    comment_loc: Loc,
    ast: &ast::Program<Loc, Loc>,
) -> Option<(ast::Program<Loc, Loc>, String)> {
    insert(use_snippets, TargetLoc::LocOfComment(comment_loc), ast)
}
