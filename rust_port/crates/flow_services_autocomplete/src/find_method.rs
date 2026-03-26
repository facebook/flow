/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::ast;
use flow_parser::ast::class;
use flow_parser::ast::expression::object;
use flow_parser::ast::types;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

pub enum Node {
    Method(class::Method<Loc, Loc>),
    Property(types::object::NormalProperty<Loc, Loc>),
}

enum Found {
    Found(Node),
}

struct Finder {
    target_loc: Loc,
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Found> for Finder {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn class_element(&mut self, elem: &'ast class::BodyElement<Loc, Loc>) -> Result<(), Found> {
        match elem {
            class::BodyElement::Method(method)
                if let object::Key::Identifier(id) = &method.key
                    && id.loc == self.target_loc =>
            {
                return Err(Found::Found(Node::Method(method.clone())));
            }
            _ => {}
        }
        ast_visitor::class_element_default(self, elem)
    }

    fn object_property_type(
        &mut self,
        prop: &'ast types::object::NormalProperty<Loc, Loc>,
    ) -> Result<(), Found> {
        if let object::Key::Identifier(id) = &prop.key {
            if id.loc == self.target_loc {
                return Err(Found::Found(Node::Property(prop.clone())));
            }
        }
        ast_visitor::object_property_type_default(self, prop)
    }
}

pub fn empty_method_of_property_type(
    prop: &types::object::NormalProperty<Loc, Loc>,
) -> Option<class::Method<Loc, Loc>> {
    let key = &prop.key;
    let value = &prop.value;
    let static_ = prop.static_;
    let f = match value {
        types::object::PropertyValue::Init(Some(t)) => match &**t {
            types::TypeInner::Function { inner, .. } => inner.as_ref(),
            _ => return None,
        },
        types::object::PropertyValue::Init(None) => return None,
        types::object::PropertyValue::Get(_, f) => f,
        types::object::PropertyValue::Set(_, f) => f,
    };
    let func = ast_builder::functions::of_type(None, None, None, None, None, f)?;
    Some(class::Method {
        loc: Loc::default(),
        kind: class::MethodKind::Method,
        key: key.clone(),
        value: (Loc::default(), func),
        static_,
        ts_accessibility: None,
        decorators: [].into(),
        comments: None,
    })
}

pub fn find(
    get_ast_from_shared_mem: &dyn Fn(&FileKey) -> Option<ast::Program<Loc, Loc>>,
    target_loc: &Loc,
) -> Option<class::Method<Loc, Loc>> {
    let source = target_loc.source()?;
    let program = get_ast_from_shared_mem(source)?;
    let mut finder = Finder {
        target_loc: target_loc.clone(),
    };
    match finder.program(&program) {
        Ok(()) => None,
        Err(Found::Found(node)) => match node {
            Node::Method(mut m) => {
                m.loc = Loc::default();
                Some(m)
            }
            Node::Property(p) => empty_method_of_property_type(&p),
        },
    }
}
