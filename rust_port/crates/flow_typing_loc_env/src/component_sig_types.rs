/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use flow_aloc::ALoc;
use flow_common::reason::Reason;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::Comment;
use flow_parser::ast::Syntax;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::pattern::Identifier as PatternIdentifier;
use flow_parser::ast::pattern::array::Element as ArrayElement;
use flow_parser::ast::pattern::object::Property as ObjectProperty;
use flow_parser::ast::statement::Block;
use flow_parser::ast::statement::component_params as ast_component_params;
use flow_parser::ast::types::AnnotationOrHint;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParams;

pub mod declaration_body_config {
    use super::*;

    pub type Body<T> = (ALoc, Block<ALoc, T>);
}

pub mod declaration_param_config {
    use std::sync::Arc;

    use super::*;

    pub type Ast<T> = ast_component_params::Params<ALoc, T>;
    pub type ParamAst<T> = ast_component_params::Param<ALoc, T>;
    pub type RestAst<T> = ast_component_params::RestParam<ALoc, T>;

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
    }

    #[derive(Debug, Clone)]
    pub struct Param {
        pub t: Type,
        pub loc: ALoc,
        pub ploc: ALoc,
        pub pattern: Pattern,
        pub default: Option<Expression<ALoc, ALoc>>,
        pub has_anno: bool,
        pub shorthand: bool,
        pub name: ast_component_params::ParamName<ALoc, ALoc>,
    }

    #[derive(Debug, Clone)]
    pub struct Rest {
        pub t: Type,
        pub loc: ALoc,
        pub ploc: ALoc,
        pub pattern: Pattern,
        pub has_anno: bool,
        pub comments: Option<Syntax<ALoc, ()>>,
    }
}

pub mod param_types {
    use super::*;

    pub type Reconstruct = Rc<
        dyn Fn(
            Vec<declaration_param_config::ParamAst<(ALoc, Type)>>,
            Option<declaration_param_config::RestAst<(ALoc, Type)>>,
        ) -> declaration_param_config::Ast<(ALoc, Type)>,
    >;

    #[derive(Clone)]
    pub struct Params {
        pub params: Vec<declaration_param_config::Param>,
        pub rest: Option<declaration_param_config::Rest>,
        pub reconstruct: Reconstruct,
    }
}

pub mod component_sig {
    use flow_parser::ast::statement::ComponentDeclaration;

    use super::*;

    pub type ComponentParams = param_types::Params;

    pub type ComponentParamsTast = declaration_param_config::Ast<(ALoc, Type)>;

    pub type ReconstructComponent = Rc<
        dyn Fn(
            declaration_param_config::Ast<(ALoc, Type)>,
            Option<(ALoc, Block<ALoc, (ALoc, Type)>)>,
            Type,
        ) -> ComponentDeclaration<ALoc, (ALoc, Type)>,
    >;

    #[derive(Clone)]
    pub struct ComponentSig {
        pub id_opt: Option<(ALoc, FlowSmolStr)>,
        pub reason: Reason,
        pub tparams: TypeParams,
        pub cparams: ComponentParams,
        pub body: declaration_body_config::Body<ALoc>,
        pub renders_t: Type,
        pub ret_annot_loc: ALoc,
    }
}
