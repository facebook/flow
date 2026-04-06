/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This class maps each node that contains the target until a node is contained
// by the target

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::class;
use flow_parser::ast::expression;
use flow_parser::ast::expression::object;
use flow_parser::ast::function;
use flow_parser::ast::types;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_class_element_default;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

mod super_finder {
    use super::*;

    struct Finder {
        found: bool,
    }

    impl<'ast> AstVisitor<'ast, Loc> for Finder {
        fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
            loc
        }

        fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
            type_
        }

        fn super_expression(
            &mut self,
            _loc: &'ast Loc,
            _expr: &'ast expression::Super<Loc>,
        ) -> Result<(), !> {
            self.found = true;
            Ok(())
        }

        // Any mentions of `this` in these constructs would reference
        // the `this` within those structures, so we ignore them
        fn class_(&mut self, _loc: &'ast Loc, _cls: &'ast class::Class<Loc, Loc>) -> Result<(), !> {
            Ok(())
        }

        fn function_declaration(
            &mut self,
            _loc: &'ast Loc,
            _stmt: &'ast function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn function_expression_or_method(
            &mut self,
            _loc: &'ast Loc,
            _stmt: &'ast function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
    }

    pub fn found_super_in_body(body: &function::Body<Loc, Loc>) -> bool {
        let mut finder = Finder { found: false };
        let Ok(()) = finder.function_body_any(body);
        finder.found
    }
}

mod arguments_finder {
    use super::*;

    struct Finder {
        found: bool,
    }

    impl<'ast> AstVisitor<'ast, Loc> for Finder {
        fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
            loc
        }

        fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
            type_
        }

        fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), !> {
            if id.name == "arguments" {
                self.found = true;
            }
            Ok(())
        }

        // Any mentions of `this` in these constructs would reference
        // the `this` within those structures, so we ignore them
        fn function_declaration(
            &mut self,
            _loc: &'ast Loc,
            _stmt: &'ast function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn function_expression_or_method(
            &mut self,
            _loc: &'ast Loc,
            _stmt: &'ast function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
    }

    pub fn found_arguments_in_body(body: &function::Body<Loc, Loc>) -> bool {
        let mut finder = Finder { found: false };
        let Ok(()) = finder.function_body_any(body);
        finder.found
    }
}

struct Mapper {
    contains: ContainsMapper,
}

impl Mapper {
    fn is_constructor(key: &object::Key<Loc, Loc>) -> bool {
        match key {
            object::Key::Identifier(id) => id.name == "constructor",
            _ => false,
        }
    }
}

impl AstVisitor<'_, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_class_element(
        &mut self,
        elem: &class::BodyElement<Loc, Loc>,
    ) -> class::BodyElement<Loc, Loc> {
        match elem {
            class::BodyElement::Method(method) if self.contains.is_target(&method.loc) => {
                let loc = &method.loc;
                let (mloc, value) = &method.value;
                let key = &method.key;
                let static_ = method.static_;
                let decorators = &method.decorators;
                let comments = &method.comments;
                if Self::is_constructor(key)
                    || super_finder::found_super_in_body(&value.body)
                    || arguments_finder::found_arguments_in_body(&value.body)
                {
                    map_class_element_default(self, elem)
                } else {
                    let prop_value = class::property::Value::Initialized(
                        expression::Expression::new(expression::ExpressionInner::ArrowFunction {
                            loc: mloc.dupe(),
                            inner: Arc::new(value.clone()),
                        }),
                    );
                    class::BodyElement::Property(class::Property {
                        loc: loc.dupe(),
                        key: key.clone(),
                        value: prop_value,
                        static_,
                        override_: false,
                        optional: false,
                        variance: None,
                        ts_accessibility: None,
                        annot: types::AnnotationOrHint::Missing(loc.dupe()),
                        decorators: decorators.clone(),
                        comments: comments.clone(),
                    })
                }
            }
            _ => map_class_element_default(self, elem),
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

    fn map_expression(
        &mut self,
        expr: &ast::expression::Expression<Loc, Loc>,
    ) -> ast::expression::Expression<Loc, Loc> {
        if self.contains.should_map_expression(expr) {
            map_expression_default(self, expr)
        } else {
            expr.dupe()
        }
    }
}

pub fn replace_method_at_target(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
    };
    mapper.map_program(ast)
}
