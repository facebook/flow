/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// =============================================================================
// OCaml: Func_class_sig_types from flow/src/typing/func_class_sig_types.ml
// =============================================================================

use std::collections::BTreeMap;
use std::rc::Rc;

use flow_aloc::ALoc;
use flow_aloc::ALocId;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::function::Body as FunctionBody;
use flow_parser::ast::function::Param as AstFunctionParam;
use flow_parser::ast::function::Params as AstFunctionParams;
use flow_parser::ast::function::RestParam as AstFunctionRestParam;
use flow_parser::ast::function::ThisParam as AstFunctionThisParam;
use flow_typing_type::type_::AnnotatedOrInferred;
use flow_typing_type::type_::ReactEffectType;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeGuard;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::TypeParams;
use flow_typing_type::type_::object::Dict as ObjectDict;
use vec1::Vec1;

use crate::func_stmt_config_types;

pub type Ast<T> = AstFunctionParams<ALoc, T>;
pub type ParamAst<T> = AstFunctionParam<ALoc, T>;
pub type RestAst<T> = AstFunctionRestParam<ALoc, T>;
pub type ThisAst<T> = AstFunctionThisParam<ALoc, T>;

pub trait ConfigTypes {
    type Param: Clone;
    type Rest: Clone;
    type ThisParam: Clone;
    type ParamAst;
    type RestAst;
    type ThisAst;
    type Ast;
}

pub struct StmtConfigTypes;

impl ConfigTypes for StmtConfigTypes {
    type Param = func_stmt_config_types::Param;
    type Rest = func_stmt_config_types::Rest;
    type ThisParam = func_stmt_config_types::ThisParam;
    type ParamAst = ParamAst<(ALoc, Type)>;
    type RestAst = RestAst<(ALoc, Type)>;
    type ThisAst = ThisAst<(ALoc, Type)>;
    type Ast = Ast<(ALoc, Type)>;
}

// module Param
pub mod param {
    use super::*;

    #[allow(type_alias_bounds)]
    pub type Reconstruct<C: ConfigTypes> =
        Rc<dyn Fn(Vec<C::ParamAst>, Option<C::RestAst>, Option<C::ThisAst>) -> Option<C::Ast>>;

    pub struct Param<C: ConfigTypes> {
        pub params: Vec<C::Param>,
        pub rest: Option<C::Rest>,
        pub this_: Option<C::ThisParam>,
        pub reconstruct: Reconstruct<C>,
    }

    impl<C: ConfigTypes> Clone for Param<C> {
        fn clone(&self) -> Self {
            Param {
                params: self.params.clone(),
                rest: self.rest.clone(),
                this_: self.this_.clone(),
                reconstruct: self.reconstruct.clone(),
            }
        }
    }
}

// =============================================================================
// module Func
// =============================================================================

pub mod func {
    use flow_parser::ast;

    use super::*;

    #[derive(Debug, Clone)]
    pub enum Kind {
        Ordinary,
        Async,
        Generator { return_loc: ALoc },
        AsyncGenerator { return_loc: ALoc },
        FieldInit(Expression<ALoc, ALoc>),
        TypeGuard(TypeGuard),
        Ctor,
    }

    pub fn string_of_kind(kind: &Kind) -> &'static str {
        match kind {
            Kind::Ordinary => "ordinary",
            Kind::Async => "async",
            Kind::Generator { .. } => "generator",
            Kind::AsyncGenerator { .. } => "async generator",
            Kind::FieldInit(_) => "field initializer",
            Kind::TypeGuard(_) => "type guard",
            Kind::Ctor => "constructor",
        }
    }

    // Concrete type aliases for the statement path
    pub type FuncParams = param::Param<StmtConfigTypes>;

    pub type FuncParamsTast = Ast<(ALoc, Type)>;

    pub type Reconstruct = Rc<
        dyn Fn(
            ast::function::Params<ALoc, (ALoc, Type)>,
            ast::function::Body<ALoc, (ALoc, Type)>,
            Type,
        ) -> ast::function::Function<ALoc, (ALoc, Type)>,
    >;

    // In OCaml, Func.Make(Config)(Param) is a functor producing a Func.t
    // parameterized by Config. In Rust, we make Func generic over C: ConfigTypes.
    pub struct Func<C: ConfigTypes> {
        pub reason: Reason,
        pub kind: Kind,
        pub tparams: TypeParams,
        pub fparams: param::Param<C>,
        pub body: Option<FunctionBody<ALoc, ALoc>>,
        pub return_t: AnnotatedOrInferred,
        pub effect_: ReactEffectType,
        pub ret_annot_loc: ALoc,
        pub statics: Option<Type>,
    }

    impl<C: ConfigTypes> Clone for Func<C> {
        fn clone(&self) -> Self {
            Func {
                reason: self.reason.clone(),
                kind: self.kind.clone(),
                tparams: self.tparams.clone(),
                fparams: self.fparams.clone(),
                body: self.body.clone(),
                return_t: self.return_t.clone(),
                effect_: self.effect_.clone(),
                ret_annot_loc: self.ret_annot_loc.clone(),
                statics: self.statics.clone(),
            }
        }
    }
}

// =============================================================================
// module Class
// =============================================================================

pub mod class {
    use super::*;

    pub type FuncSig<C> = func::Func<C>;

    #[allow(type_alias_bounds)]
    pub type SetAsts<C: ConfigTypes> = Rc<
        dyn Fn(
            (
                Option<C::Ast>,
                Option<FunctionBody<ALoc, (ALoc, Type)>>,
                Option<Expression<ALoc, (ALoc, Type)>>,
            ),
        ),
    >;

    pub type SetType = Rc<dyn Fn(Type)>;

    pub enum Field<C: ConfigTypes> {
        Annot(Type),
        Infer(FuncSig<C>, SetAsts<C>),
    }

    impl<C: ConfigTypes> Clone for Field<C> {
        fn clone(&self) -> Self {
            match self {
                Field::Annot(t) => Field::Annot(t.clone()),
                Field::Infer(f, s) => Field::Infer(f.clone(), s.clone()),
            }
        }
    }

    pub type FieldPrime<C> = (Option<ALoc>, Polarity, Field<C>);

    pub type TypeApp = (ALoc, Type, Option<Vec<Type>>);

    #[derive(Debug, Clone)]
    pub enum Extends {
        Explicit(TypeApp),
        Implicit { null: bool },
    }

    #[derive(Debug, Clone)]
    pub struct ClassSuper {
        pub extends: Extends,
        // declare class only
        pub mixins: Vec<TypeApp>,
        pub implements: Vec<TypeApp>,
        pub this_tparam: TypeParam,
        pub this_t: Type,
    }

    #[derive(Debug, Clone)]
    pub struct InterfaceSuper {
        pub inline: bool,
        pub extends: Vec<TypeApp>,
        pub callable: bool,
    }

    #[derive(Debug, Clone)]
    pub enum Super {
        Interface(InterfaceSuper),
        Class(ClassSuper),
    }

    pub struct FuncInfo<C: ConfigTypes> {
        pub id_loc: Option<ALoc>,
        pub this_write_loc: Option<ALoc>,
        pub func_sig: FuncSig<C>,
        pub set_asts: SetAsts<C>,
        pub set_type: SetType,
    }

    impl<C: ConfigTypes> Clone for FuncInfo<C> {
        fn clone(&self) -> Self {
            FuncInfo {
                id_loc: self.id_loc.clone(),
                this_write_loc: self.this_write_loc.clone(),
                func_sig: self.func_sig.clone(),
                set_asts: self.set_asts.clone(),
                set_type: self.set_type.clone(),
            }
        }
    }

    pub struct Signature<C: ConfigTypes> {
        pub reason: Reason,
        pub fields: BTreeMap<FlowSmolStr, FieldPrime<C>>,
        pub private_fields: BTreeMap<FlowSmolStr, FieldPrime<C>>,
        pub proto_fields: BTreeMap<FlowSmolStr, FieldPrime<C>>,
        // Multiple function signatures indicates an overloaded method.
        pub methods: BTreeMap<FlowSmolStr, Vec1<FuncInfo<C>>>,
        pub private_methods: BTreeMap<FlowSmolStr, FuncInfo<C>>,
        pub getters: BTreeMap<FlowSmolStr, FuncInfo<C>>,
        pub setters: BTreeMap<FlowSmolStr, FuncInfo<C>>,
        pub calls: Vec<Type>,
        pub dict: ObjectDict,
    }

    impl<C: ConfigTypes> Clone for Signature<C> {
        fn clone(&self) -> Self {
            Signature {
                reason: self.reason.clone(),
                fields: self.fields.clone(),
                private_fields: self.private_fields.clone(),
                proto_fields: self.proto_fields.clone(),
                methods: self.methods.clone(),
                private_methods: self.private_methods.clone(),
                getters: self.getters.clone(),
                setters: self.setters.clone(),
                calls: self.calls.clone(),
                dict: self.dict.clone(),
            }
        }
    }

    pub struct Class<C: ConfigTypes> {
        pub id: ALocId,
        pub class_name: Option<FlowSmolStr>,
        pub class_loc: ALoc,
        pub tparams: TypeParams,
        pub tparams_map: BTreeMap<SubstName, Type>,
        pub super_: Super,
        // Multiple function signatures indicates an overloaded constructor.
        pub constructor: Vec<FuncInfo<C>>,
        pub static_: Signature<C>,
        pub instance: Signature<C>,
    }

    impl<C: ConfigTypes> Clone for Class<C> {
        fn clone(&self) -> Self {
            Class {
                id: self.id.clone(),
                class_name: self.class_name.clone(),
                class_loc: self.class_loc.clone(),
                tparams: self.tparams.clone(),
                tparams_map: self.tparams_map.clone(),
                super_: self.super_.clone(),
                constructor: self.constructor.clone(),
                static_: self.static_.clone(),
                instance: self.instance.clone(),
            }
        }
    }
}
