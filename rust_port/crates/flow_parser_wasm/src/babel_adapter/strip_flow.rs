/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::function;
use flow_parser::ast::pattern;
use flow_parser::ast::statement;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast::types;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;

struct StripFlow;

impl StripFlow {
    fn any(loc: &Loc) -> types::Type<Loc, Loc> {
        types::Type::new(types::TypeInner::Any {
            loc: loc.dupe(),
            comments: None,
        })
    }

    fn simple_generic(loc: &Loc, name: &str) -> types::Type<Loc, Loc> {
        types::Type::new(types::TypeInner::Generic {
            loc: loc.dupe(),
            inner: Arc::new(types::Generic {
                id: types::generic::Identifier::Unqualified(ast::Identifier::new(
                    ast::IdentifierInner {
                        loc: loc.dupe(),
                        name: name.into(),
                        comments: None,
                    },
                )),
                targs: None,
                comments: None,
            }),
        })
    }

    fn declare_variable(
        loc: &Loc,
        id: &ast::Identifier<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        let pattern = pattern::Pattern::Identifier {
            loc: id.loc.dupe(),
            inner: Arc::new(pattern::Identifier {
                name: id.dupe(),
                annot: types::AnnotationOrHint::Available(types::Annotation {
                    loc: loc.dupe(),
                    annotation: Self::any(loc),
                }),
                optional: false,
            }),
        };
        statement::Statement::new(StatementInner::DeclareVariable {
            loc: loc.dupe(),
            inner: Arc::new(statement::DeclareVariable {
                declarations: Arc::from([statement::variable::Declarator {
                    loc: loc.dupe(),
                    id: pattern,
                    init: None,
                }]),
                kind: ast::VariableKind::Const,
                comments: None,
            }),
        })
    }

    fn strip_this_param(
        mut function: function::Function<Loc, Loc>,
    ) -> function::Function<Loc, Loc> {
        function.params.this_ = None;
        function
    }
}

impl<'ast> AstVisitor<'ast, Loc> for StripFlow {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_type_(&mut self, type_: &'ast types::Type<Loc, Loc>) -> types::Type<Loc, Loc> {
        match &**type_ {
            types::TypeInner::Symbol { loc, .. } => Self::simple_generic(loc, "symbol"),
            types::TypeInner::BigInt { loc, .. } => Self::simple_generic(loc, "bigint"),
            types::TypeInner::Object { loc, inner }
                if inner
                    .properties
                    .iter()
                    .any(|property| matches!(property, types::object::Property::MappedType(_))) =>
            {
                Self::any(loc)
            }
            types::TypeInner::IndexedAccess { loc, .. }
            | types::TypeInner::OptionalIndexedAccess { loc, .. }
            | types::TypeInner::Keyof { loc, .. }
            | types::TypeInner::Conditional { loc, .. }
            | types::TypeInner::Infer { loc, .. }
            | types::TypeInner::Component { loc, .. }
            | types::TypeInner::Renders { loc, .. }
            | types::TypeInner::ReadOnly { loc, .. } => Self::any(loc),
            types::TypeInner::Function { loc, inner } if inner.effect == function::Effect::Hook => {
                Self::any(loc)
            }
            _ => ast_visitor::map_type_default(self, type_),
        }
    }

    fn map_tuple_type(
        &mut self,
        loc: &'ast Loc,
        tuple: &'ast types::Tuple<Loc, Loc>,
    ) -> types::Tuple<Loc, Loc> {
        let mut tuple = ast_visitor::map_tuple_type_default(self, loc, tuple);
        tuple.elements = tuple
            .elements
            .iter()
            .map(|element| match element {
                types::tuple::Element::UnlabeledElement { .. } => element.clone(),
                types::tuple::Element::LabeledElement { loc, .. }
                | types::tuple::Element::SpreadElement { loc, .. } => {
                    types::tuple::Element::UnlabeledElement {
                        loc: loc.dupe(),
                        annot: Self::any(loc),
                        optional: false,
                    }
                }
            })
            .collect::<Vec<_>>()
            .into();
        tuple
    }

    fn map_function_return_annotation(
        &mut self,
        return_: &'ast function::ReturnAnnot<Loc, Loc>,
    ) -> function::ReturnAnnot<Loc, Loc> {
        match return_ {
            function::ReturnAnnot::TypeGuard(guard) => {
                function::ReturnAnnot::Available(types::Annotation {
                    loc: guard.loc.dupe(),
                    annotation: Self::any(&guard.loc),
                })
            }
            _ => ast_visitor::map_function_return_annotation_default(self, return_),
        }
    }

    fn map_function_(
        &mut self,
        loc: &'ast Loc,
        function: &'ast function::Function<Loc, Loc>,
    ) -> function::Function<Loc, Loc> {
        Self::strip_this_param(ast_visitor::map_function_default(self, loc, function))
    }

    fn map_statement(
        &mut self,
        statement: &'ast statement::Statement<Loc, Loc>,
    ) -> statement::Statement<Loc, Loc> {
        match &**statement {
            StatementInner::DeclareEnum { loc, inner } => Self::declare_variable(loc, &inner.id),
            StatementInner::DeclareNamespace { loc, inner } => {
                let id = match &inner.id {
                    statement::declare_namespace::Id::Global(id) => id,
                    statement::declare_namespace::Id::Local(id) => id,
                };
                Self::declare_variable(loc, id)
            }
            _ => ast_visitor::map_statement_default(self, statement),
        }
    }
}

pub fn lower_program(program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
    StripFlow.map_program(program)
}
