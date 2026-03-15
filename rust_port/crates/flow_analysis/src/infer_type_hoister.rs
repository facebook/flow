/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;

struct InferTypeHoister<Loc: Dupe> {
    infer_types: Vec<(Loc, ast::types::Infer<Loc, Loc>)>,
}

impl<Loc: Dupe> InferTypeHoister<Loc> {
    pub fn new() -> Self {
        Self {
            infer_types: Vec::new(),
        }
    }

    pub fn into_infer_types(self) -> Vec<(Loc, ast::types::Infer<Loc, Loc>)> {
        self.infer_types
    }
}

impl<Loc: Dupe> AstVisitor<'_, Loc> for InferTypeHoister<Loc> {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn type_(&mut self, t: &ast::types::Type<Loc, Loc>) -> Result<(), !> {
        match t.deref() {
            TypeInner::Infer { loc, inner } => {
                self.infer_types.push((loc.dupe(), inner.as_ref().clone()));
                ast_visitor::type_default(self, t)
            }
            TypeInner::Conditional { loc: _, inner } => {
                // Visiting of extends_type is intentionally skipped,
                // since it should be internal to the nested conditional type.
                let Ok(()) = self.type_(&inner.check_type);
                let Ok(()) = self.type_(&inner.true_type);
                self.type_(&inner.false_type)
            }
            _ => ast_visitor::type_default(self, t),
        }
    }
}

pub fn hoist_infer_types<Loc: Dupe>(
    t: &ast::types::Type<Loc, Loc>,
) -> Vec<(Loc, ast::types::Infer<Loc, Loc>)> {
    let mut hoister = InferTypeHoister::new();
    let Ok(()) = hoister.type_(t);
    hoister.into_infer_types()
}
