/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast_visitor::AstVisitor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Super,
    This,
}

pub struct Finder<Loc: Dupe + Ord> {
    pub acc: BTreeMap<Loc, Kind>,
}

impl<Loc: Dupe + Ord> Finder<Loc> {
    pub fn new() -> Self {
        Self {
            acc: BTreeMap::new(),
        }
    }
}

impl<Loc: Dupe + Ord> AstVisitor<'_, Loc> for Finder<Loc> {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn this_expression(&mut self, loc: &Loc, _expr: &ast::expression::This<Loc>) -> Result<(), !> {
        self.acc.insert(loc.dupe(), Kind::This);
        Ok(())
    }

    fn super_expression(
        &mut self,
        loc: &Loc,
        _expr: &ast::expression::Super<Loc>,
    ) -> Result<(), !> {
        self.acc.insert(loc.dupe(), Kind::Super);
        Ok(())
    }

    // Any mentions of `this` in these constructs would reference
    // the `this` within those structures, so we ignore them
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

    fn component_declaration(
        &mut self,
        _loc: &Loc,
        _c: &ast::statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }
}
