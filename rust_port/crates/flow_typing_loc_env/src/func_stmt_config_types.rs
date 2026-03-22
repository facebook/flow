/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_aloc::ALoc;
use flow_parser::ast::Comment;
use flow_parser::ast::Syntax;
use flow_parser::ast::class::Property as ClassProperty;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::function::Param as AstFunctionParam;
use flow_parser::ast::function::Params as AstFunctionParams;
use flow_parser::ast::function::RestParam as AstFunctionRestParam;
use flow_parser::ast::function::ThisParam as AstFunctionThisParam;
use flow_parser::ast::pattern::Identifier as PatternIdentifier;
use flow_parser::ast::pattern::array::Element as ArrayElement;
use flow_parser::ast::pattern::object::Property as ObjectProperty;
use flow_parser::ast::types::Annotation;
use flow_parser::ast::types::AnnotationOrHint;
use flow_typing_type::type_::Type;

pub type Ast<T> = AstFunctionParams<ALoc, T>;
pub type ParamAst<T> = AstFunctionParam<ALoc, T>;
pub type RestAst<T> = AstFunctionRestParam<ALoc, T>;
pub type ThisAst<T> = AstFunctionThisParam<ALoc, T>;

#[derive(Debug, Clone)]
pub enum Pattern {
    Id(PatternIdentifier<ALoc, (ALoc, Type)>),
    Object {
        annot: AnnotationOrHint<ALoc, (ALoc, Type)>,
        properties: Vec<ObjectProperty<ALoc, ALoc>>,
        optional: bool,
        comments: Option<Syntax<ALoc, Arc<[Comment<ALoc>]>>>,
    },
    Array {
        annot: AnnotationOrHint<ALoc, (ALoc, Type)>,
        elements: Vec<ArrayElement<ALoc, ALoc>>,
        optional: bool,
        comments: Option<Syntax<ALoc, Arc<[Comment<ALoc>]>>>,
    },
    ParamPropertyPattern(ClassProperty<ALoc, (ALoc, Type)>),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub t: Type,
    pub loc: ALoc,
    pub ploc: ALoc,
    pub pattern: Pattern,
    pub default: Option<Expression<ALoc, ALoc>>,
    pub has_anno: bool,
}

#[derive(Debug, Clone)]
pub struct Rest {
    pub t: Type,
    pub loc: ALoc,
    pub ploc: ALoc,
    pub id: PatternIdentifier<ALoc, (ALoc, Type)>,
    pub has_anno: bool,
}

#[derive(Debug, Clone)]
pub struct ThisParam {
    pub t: Type,
    pub loc: ALoc,
    pub annot: Annotation<ALoc, (ALoc, Type)>,
}
