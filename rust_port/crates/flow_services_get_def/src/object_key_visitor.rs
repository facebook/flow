/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The default visitor does not provide all of the context we need when visiting an object key. In
//! particular, we need the location of the enclosing object literal.

use std::ops::Deref;

use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::object;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;

pub trait ObjectKeyVisitorCallback {
    fn visit_object_key(&mut self, literal_loc: &Loc, key: &object::Key<Loc, Loc>);
}

pub struct ObjectKeyVisitor<'a, C: ObjectKeyVisitorCallback> {
    callback: &'a mut C,
}

impl<'a, C: ObjectKeyVisitorCallback> ObjectKeyVisitor<'a, C> {
    pub fn new(callback: &'a mut C) -> Self {
        ObjectKeyVisitor { callback }
    }
}

impl<'a, 'ast, C: ObjectKeyVisitorCallback> AstVisitor<'ast, Loc, Loc, &'ast Loc, !>
    for ObjectKeyVisitor<'a, C>
{
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn expression(&mut self, exp: &'ast ast::expression::Expression<Loc, Loc>) -> Result<(), !> {
        match exp.deref() {
            expression::ExpressionInner::Object { loc, inner } => {
                self.visit_object_literal(loc, inner);
            }
            _ => {}
        }
        ast_visitor::expression_default(self, exp)
    }
}

impl<'a, C: ObjectKeyVisitorCallback> ObjectKeyVisitor<'a, C> {
    fn visit_object_literal(&mut self, loc: &Loc, obj: &expression::Object<Loc, Loc>) {
        fn get_prop_key(prop: &object::NormalProperty<Loc, Loc>) -> &object::Key<Loc, Loc> {
            match prop {
                object::NormalProperty::Init { key, .. } => key,
                object::NormalProperty::Method { key, .. } => key,
                object::NormalProperty::Get { key, .. } => key,
                object::NormalProperty::Set { key, .. } => key,
            }
        }
        let expression::Object {
            properties,
            comments: _,
        } = obj;
        for prop in properties.iter() {
            match prop {
                object::Property::SpreadProperty(_) => {}
                object::Property::NormalProperty(prop) => {
                    let key = get_prop_key(prop);
                    self.callback.visit_object_key(loc, key);
                }
            }
        }
    }
}

pub fn visit<C: ObjectKeyVisitorCallback>(callback: &mut C, program: &ast::Program<Loc, Loc>) {
    let mut visitor = ObjectKeyVisitor::new(callback);
    let Ok(()) = visitor.program(program);
}
