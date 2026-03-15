/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_common::reason::Name;
use flow_parser::ast;
use flow_parser::ast_visitor::AstVisitor;

pub mod procedure_decider {
    use super::*;

    struct Decider {
        is_procedure: bool,
    }

    impl Decider {
        fn new() -> Self {
            Decider { is_procedure: true }
        }

        fn set_not_procedure(&mut self) {
            self.is_procedure = false;
        }
    }

    impl<Loc: Dupe> AstVisitor<'_, Loc> for Decider {
        fn normalize_loc(loc: &Loc) -> &Loc {
            loc
        }

        fn normalize_type(type_: &Loc) -> &Loc {
            type_
        }

        fn function_(
            &mut self,
            _loc: &Loc,
            _f: &ast::function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn component_declaration(
            &mut self,
            _loc: &Loc,
            _c: &ast::statement::ComponentDeclaration<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn return_(&mut self, _loc: &Loc, ret: &ast::statement::Return<Loc, Loc>) -> Result<(), !> {
            if ret.argument.is_some() {
                self.set_not_procedure();
            }
            Ok(())
        }

        fn function_body_any(&mut self, body: &ast::function::Body<Loc, Loc>) -> Result<(), !> {
            match body {
                ast::function::Body::BodyBlock(block) => {
                    let Ok(()) = self.function_body(block);
                }
                ast::function::Body::BodyExpression(_) => {
                    self.set_not_procedure();
                }
            }
            Ok(())
        }
    }

    pub fn is_procedure<Loc: Dupe>(body: &ast::function::Body<Loc, Loc>) -> bool {
        let mut decider = Decider::new();
        let Ok(()) = decider.function_body_any(body);
        decider.is_procedure
    }
}

pub fn is_munged_property_string(name: &str) -> bool {
    name.len() >= 2 && name.starts_with('_') && !name.starts_with("__")
}

pub fn is_munged_property_name(name: &Name) -> bool {
    // TODO consider adding another name variant for munged property strings
    is_munged_property_string(name.as_str())
}

pub mod this_finder {
    use dupe::Dupe;

    use super::*;

    // OCaml: class ['a] finder
    struct Finder {
        acc: bool,
    }

    impl Finder {
        fn new() -> Self {
            Self { acc: false }
        }

        fn set_acc(&mut self, v: bool) {
            self.acc = v;
        }
    }

    impl<Loc: Dupe> AstVisitor<'_, Loc> for Finder {
        fn normalize_loc(loc: &Loc) -> &Loc {
            loc
        }

        fn normalize_type(type_: &Loc) -> &Loc {
            type_
        }

        fn this_expression(
            &mut self,
            _loc: &Loc,
            _expr: &ast::expression::This<Loc>,
        ) -> Result<(), !> {
            self.set_acc(true);
            Ok(())
        }

        fn type_identifier(&mut self, id: &ast::Identifier<Loc, Loc>) -> Result<(), !> {
            if id.name.as_str() == "this" {
                self.set_acc(true);
            }
            Ok(())
        }

        fn class_(&mut self, _loc: &Loc, _cls: &ast::class::Class<Loc, Loc>) -> Result<(), !> {
            Ok(())
        }

        fn record_declaration(
            &mut self,
            _loc: &Loc,
            _r: &ast::statement::RecordDeclaration<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn function_declaration(
            &mut self,
            _loc: &Loc,
            _f: &ast::function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }

        fn function_expression_or_method(
            &mut self,
            _loc: &Loc,
            _f: &ast::function::Function<Loc, Loc>,
        ) -> Result<(), !> {
            Ok(())
        }
    }

    fn found_this_in_body_or_params<Loc: Dupe>(
        body: &ast::function::Body<Loc, Loc>,
        params: &ast::function::Params<Loc, Loc>,
    ) -> bool {
        let mut finder = Finder::new();
        let Ok(()) = finder.function_body_any(body);
        if finder.acc {
            return true;
        }
        let Ok(()) = finder.function_params(params);
        finder.acc
    }

    pub fn missing_this_annotation<Loc: Dupe>(
        needs_this_param: bool,
        body: &ast::function::Body<Loc, Loc>,
        params: &ast::function::Params<Loc, Loc>,
    ) -> bool {
        needs_this_param && params.this_.is_none() && found_this_in_body_or_params(body, params)
    }
}
