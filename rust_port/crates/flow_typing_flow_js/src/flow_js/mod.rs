/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! This module describes the subtyping algorithm that forms the core of
//! typechecking. The algorithm (in its basic form) is described in Francois
//! Pottier's thesis. The main data structures maintained by the algorithm are:
//! (1) for every type variable, which type variables form its lower and upper
//! bounds (i.e., flow in and out of the type variable); and (2) for every type
//! variable, which concrete types form its lower and upper bounds. Every new
//! subtyping constraint added to the system is deconstructed into its subparts,
//! until basic flows between type variables and other type variables or concrete
//! types remain; these flows are then viewed as links in a chain, bringing
//! together further concrete types and type variables to participate in
//! subtyping. This process continues till a fixpoint is reached---which itself
//! is guaranteed to exist, and is usually reached in very few steps. *)

use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::ReasonDesc;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::is_literal_object_reason;
use flow_common::subst_name;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_context::SubstCacheErr;
use flow_typing_context::type_app_expansion;
use flow_typing_errors::error_message::EInvariantSubtypingWithUseOpData;
use flow_typing_errors::error_message::EnumErrorKind;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::error_message::UpperKind;
use flow_typing_errors::flow_error;
use flow_typing_errors::intermediate_error_types::ExpectedModulePurpose;
use flow_typing_errors::intermediate_error_types::ExplanationWithLazyParts;
use flow_typing_flow_common::flow_cache;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::flow_js_utils::SpeculativeError;
use flow_typing_flow_common::flow_js_utils::UnionOptimizationGuardResult;
use flow_typing_flow_common::flow_js_utils::callee_recorder;
use flow_typing_flow_common::flow_js_utils::enum_proto;
use flow_typing_flow_common::flow_js_utils::tvar_visitors;
use flow_typing_flow_common::flow_js_utils::use_op_of_lookup_action;
use flow_typing_flow_common::instantiation_utils;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_common::speculation;
use flow_typing_flow_common::type_subst;
use flow_typing_generics::GenericId;
use flow_typing_type::type_::AnyErrorKind;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::CallAction;
use flow_typing_type::type_::CallArg;
use flow_typing_type::type_::CallArgInner;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::ClassBinding;
use flow_typing_type::type_::ConcretizationKind;
use flow_typing_type::type_::ConstructorTData;
use flow_typing_type::type_::Cont;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::DerivedType;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::DroType;
use flow_typing_type::type_::ElemAction;
use flow_typing_type::type_::EnumInfo;
use flow_typing_type::type_::EnumInfoInner;
use flow_typing_type::type_::FieldData;
use flow_typing_type::type_::Flags;
use flow_typing_type::type_::FrameUseOp;
use flow_typing_type::type_::FunParam;
use flow_typing_type::type_::FunRestParam;
use flow_typing_type::type_::FunType;
use flow_typing_type::type_::FuncallType;
use flow_typing_type::type_::GetEnumKind;
use flow_typing_type::type_::GetPrivatePropTData;
use flow_typing_type::type_::GetPropTData;
use flow_typing_type::type_::InstType;
use flow_typing_type::type_::InstTypeInner;
use flow_typing_type::type_::InstanceKind;
use flow_typing_type::type_::InstanceT;
use flow_typing_type::type_::InstanceTInner;
use flow_typing_type::type_::LazyHintT;
use flow_typing_type::type_::Literal;
use flow_typing_type::type_::LookupAction;
use flow_typing_type::type_::LookupKind;
use flow_typing_type::type_::MappedTypeHomomorphicFlag;
use flow_typing_type::type_::MethodAction;
use flow_typing_type::type_::MethodCallType;
use flow_typing_type::type_::MixedFlavor;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::NumberLiteral;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::ObjType;
use flow_typing_type::type_::OptionalIndexedAccessIndex;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::PredicateConcretetizerVariant;
use flow_typing_type::type_::PrivateMethodTData;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::PropertySource;
use flow_typing_type::type_::PropertyType;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ReactDro;
use flow_typing_type::type_::ReactEffectType;
use flow_typing_type::type_::RendersVariant;
use flow_typing_type::type_::ResolveSpreadType;
use flow_typing_type::type_::ResolvedParam;
use flow_typing_type::type_::RootUseOp;
use flow_typing_type::type_::Selector;
use flow_typing_type::type_::SetMode;
use flow_typing_type::type_::SetPrivatePropTData;
use flow_typing_type::type_::SpreadResolve;
use flow_typing_type::type_::StrUtilOp;
use flow_typing_type::type_::Targ;
use flow_typing_type::type_::TupleElement;
use flow_typing_type::type_::TupleView;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeDestructorT;
use flow_typing_type::type_::TypeDestructorTInner;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeMap;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::TypeParamInner;
use flow_typing_type::type_::TypeTKind;
use flow_typing_type::type_::UnifyCause;
use flow_typing_type::type_::UnresolvedParam;
use flow_typing_type::type_::UnsoundnessKind;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualRootUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::WriteCtx;
use flow_typing_type::type_::annot;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::call_of_method_app;
use flow_typing_type::type_::concretize_seen;
use flow_typing_type::type_::constraint;
use flow_typing_type::type_::drop_generic;
use flow_typing_type::type_::dummy_static;
use flow_typing_type::type_::elemt_of_arrtype;
use flow_typing_type::type_::empty_t;
use flow_typing_type::type_::eval;
use flow_typing_type::type_::global_this;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::inter_rep;
use flow_typing_type::type_::mixed_t;
use flow_typing_type::type_::mk_boundfunctiontype;
use flow_typing_type::type_::name_of_propref;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::null;
use flow_typing_type::type_::num_module_t;
use flow_typing_type::type_::object;
use flow_typing_type::type_::open_tvar;
use flow_typing_type::type_::poly;
use flow_typing_type::type_::primitive_promoting_use_t;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::property;
use flow_typing_type::type_::react;
use flow_typing_type::type_::root_of_use_op;
use flow_typing_type::type_::str_module_t;
use flow_typing_type::type_::type_collector;
use flow_typing_type::type_::type_or_type_desc::TypeOrTypeDescT;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_::unsoundness;
use flow_typing_type::type_::void;
use flow_typing_type::type_util;
use flow_typing_type::type_util::loc_of_t;
use flow_typing_type::type_util::mk_named_prop;
use flow_typing_type::type_util::reason_of_propref;
use flow_typing_type::type_util::reason_of_t;
use flow_typing_type::type_util::reason_of_use_t;
use flow_typing_type::type_util::subtype_this_of_function;
use flow_typing_type::type_util::tuple_ts_of_elements;
use flow_typing_type::type_util::use_op_of_use_t;
use flow_utils_union_find::Node;
use vec1::Vec1;

use crate::implicit_instantiation;
use crate::react_kit;
use crate::subtyping_kit;

mod any_helpers;
pub(crate) mod const_fold_expansion;
mod constraint_helpers;
mod dispatch;
mod enum_helpers;
mod eval_helpers;
mod get_prop_helpers;
mod helpers;
mod inheritance_helpers;
mod instantiation_helpers;
mod multi_arg_helpers;
pub mod recursion_check;
mod unification_helpers;

pub struct FlowJs;

impl FlowJs {
    // Base methods
    pub fn flow<'cx>(
        cx: &Context<'cx>,
        t: &Type,
        use_t: &UseT<Context<'cx>>,
    ) -> Result<(), SpeculativeError> {
        helpers::flow(cx, (t, use_t))
    }

    pub fn flow_opt<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        t: &Type,
        use_t: &UseT<Context<'cx>>,
    ) -> Result<(), FlowJsException> {
        helpers::flow_opt(cx, trace, (t, use_t))
    }

    pub fn flow_p<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason1: &Reason,
        reason2: &Reason,
        propref: &PropRef,
        prop1: &PropertyType,
        prop2: &PropertyType,
    ) -> Result<(), FlowJsException> {
        helpers::flow_p(cx, use_op, reason1, reason2, propref, (prop1, prop2))
    }

    pub fn flow_t<'cx>(cx: &Context<'cx>, t1: &Type, t2: &Type) -> Result<(), SpeculativeError> {
        helpers::flow_t(cx, (t1, t2))
    }

    pub fn reposition<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        loc: ALoc,
        desc: Option<&ReasonDesc>,
        annot_loc: Option<ALoc>,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        helpers::reposition(cx, trace, loc, desc, annot_loc, t)
    }

    pub fn rec_flow<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        t: &Type,
        use_t: &UseT<Context<'cx>>,
    ) -> Result<(), FlowJsException> {
        helpers::rec_flow(cx, trace, (t, use_t))
    }

    pub fn rec_flow_t<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        t1: &Type,
        t2: &Type,
    ) -> Result<(), FlowJsException> {
        helpers::rec_flow_t(cx, trace, use_op, (t1, t2))
    }

    pub fn rec_unify<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        unify_cause: UnifyCause,
        unify_any: Option<bool>,
        t1: &Type,
        t2: &Type,
    ) -> Result<(), FlowJsException> {
        helpers::rec_unify(cx, trace, use_op, unify_cause, unify_any, t1, t2)
    }

    pub fn unify<'cx>(
        cx: &Context<'cx>,
        use_op: Option<UseOp>,
        unify_cause: UnifyCause,
        t1: &Type,
        t2: &Type,
    ) -> Result<(), FlowJsException> {
        Ok(helpers::unify(cx, use_op, unify_cause, t1, t2)?)
    }

    pub fn unify_opt<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        use_op: UseOp,
        unify_cause: UnifyCause,
        unify_any: Option<bool>,
        t1: &Type,
        t2: &Type,
    ) -> Result<(), FlowJsException> {
        helpers::unify_opt(cx, trace, use_op, unify_cause, unify_any, t1, t2)
    }

    pub fn filter_optional<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        reason: &Reason,
        t: &Type,
    ) -> Result<u32, FlowJsException> {
        helpers::filter_optional(cx, trace, reason, t)
    }

    pub fn mk_typeapp_instance_annot<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        from_value: bool,
        use_desc: Option<bool>,
        t: &Type,
        targs: Rc<[Type]>,
    ) -> Result<Type, FlowJsException> {
        helpers::mk_typeapp_instance_annot(
            cx,
            trace,
            use_op,
            reason_op,
            reason_tapp,
            from_value,
            use_desc,
            t,
            targs,
        )
    }

    pub fn mk_typeapp_instance<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        from_value: bool,
        t: &Type,
        targs: Rc<[Type]>,
    ) -> Result<Type, FlowJsException> {
        helpers::mk_typeapp_instance(
            cx,
            trace,
            use_op,
            reason_op,
            reason_tapp,
            from_value,
            t,
            targs,
        )
    }

    pub fn flow_use_op<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        use_t: UseT<Context<'cx>>,
    ) -> Result<UseT<Context<'cx>>, FlowJsException> {
        Ok(helpers::flow_use_op(cx, use_op, use_t))
    }

    pub fn mk_react_dro<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        react_dro: ReactDro,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        Ok(helpers::mk_react_dro(cx, use_op, react_dro, t))
    }

    // Builtins methods
    pub fn get_builtin_type<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        reason: &Reason,
        use_desc: Option<bool>,
        name: &str,
    ) -> Result<Type, FlowJsException> {
        helpers::get_builtin_type(cx, trace, reason, use_desc, name)
    }

    pub fn get_builtin_react_type<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        reason: &Reason,
        use_desc: Option<bool>,
        purpose: ExpectedModulePurpose,
    ) -> Result<Type, FlowJsException> {
        helpers::get_builtin_react_type(cx, trace, reason, use_desc, purpose)
    }

    pub fn get_builtin_typeapp<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        use_desc: Option<bool>,
        name: &str,
        targs: Vec<Type>,
    ) -> Type {
        helpers::get_builtin_typeapp(cx, reason, use_desc, name, targs)
    }

    pub fn get_builtin_react_typeapp<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        use_desc: Option<bool>,
        purpose: ExpectedModulePurpose,
        targs: Vec<Type>,
    ) -> Result<Type, FlowJsException> {
        helpers::get_builtin_react_typeapp(cx, reason, use_desc, purpose, targs)
    }

    pub fn perform_read_prop_action<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: VirtualUseOp<ALoc>,
        propref: &PropRef,
        property_type: &PropertyType,
        reason: &Reason,
        dro: Option<(ALoc, DroType)>,
        tvar: &Tvar,
    ) -> Result<(), FlowJsException> {
        let react_dro = dro.map(|(loc, dro_type)| ReactDro(loc, dro_type));
        (flow_js_utils::get_prop_t_kit::perform_read_prop_action::<FlowJs>(
            cx,
            &trace,
            use_op,
            propref,
            property_type.clone(),
            reason,
            &react_dro,
        )?)(cx, tvar.dupe())
    }

    // Subtyping methods
    pub fn speculative_subtyping_succeeds<'cx>(cx: &Context<'cx>, t1: &Type, t2: &Type) -> bool {
        helpers::speculative_subtyping_succeeds(cx, t1, t2)
    }

    pub fn possible_concrete_types_for_optional_chain<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_optional_chain(cx, reason, t)
    }

    pub fn possible_concrete_types_for_inspection<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_inspection(cx, reason, t)
    }

    pub fn possible_concrete_types_for_enum_exhaustive_check<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_enum_exhaustive_check(cx, reason, t)
    }

    pub fn possible_concrete_types_for_imports_exports<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_imports_exports(cx, reason, t)
    }

    pub fn possible_concrete_types_for_operators_checking<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_operators_checking(cx, reason, t)
    }

    pub fn possible_concrete_types_for_object_assign<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_object_assign(cx, reason, t)
    }

    pub fn possible_concrete_types_for_destructuring<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_destructuring(cx, reason, t)
    }

    pub fn possible_concrete_types_for_computed_object_keys<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_computed_object_keys(cx, reason, t)
    }

    pub fn reposition_reason<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        reason: &Reason,
        use_desc: Option<bool>,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        helpers::reposition_reason(cx, trace, reason, use_desc.unwrap_or(false), t)
    }

    pub fn eval_destructor<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
        destructor: &Destructor,
        tvar: &Tvar,
    ) -> Result<(), FlowJsException> {
        eval_helpers::eval_destructor(cx, trace, use_op, reason, t, destructor, tvar)
    }

    pub fn multiflow_subtype<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: VirtualUseOp<ALoc>,
        reason: &Reason,
        call_args: &[CallArg],
        funtype: &FunType,
    ) -> Result<(), FlowJsException> {
        multi_arg_helpers::multiflow_subtype(cx, trace, use_op, reason, call_args, funtype)
    }

    pub fn flow_type_args<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason1: &Reason,
        reason2: &Reason,
        targs1: Rc<[(SubstName, Reason, Type, Polarity)]>,
        targs2: Rc<[(SubstName, Reason, Type, Polarity)]>,
    ) -> Result<(), FlowJsException> {
        inheritance_helpers::flow_type_args(cx, trace, use_op, reason1, reason2, targs1, targs2)
    }

    pub fn instantiate_this_class<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        reason_op: &Reason,
        reason_tapp: &Reason,
        this_t: &Type,
        targs: Option<Rc<[Type]>>,
        t: &Type,
        cont: &Cont<Context<'cx>>,
    ) -> Result<(), FlowJsException> {
        instantiation_helpers::instantiate_this_class(
            cx,
            trace,
            reason_op,
            reason_tapp,
            this_t,
            targs,
            t,
            cont,
        )
    }

    pub fn instantiate_poly_with_targs<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        mut errs_ref: Option<&mut Vec<SubstCacheErr>>,
        unify_bounds: Option<bool>,
        poly_t: (ALoc, Vec1<TypeParam>, Type),
        targs: Vec<Type>,
    ) -> Result<(Type, Vec<(Type, SubstName)>), FlowJsException> {
        let unify_bounds = unify_bounds.unwrap_or(false);
        let (tparams_loc, xs, t) = poly_t;
        let mut errs_opt = errs_ref.as_deref_mut().map(std::mem::take);
        let result = flow_js_utils::instantiation_kit::instantiate_poly_with_targs::<FlowJs>(
            cx,
            trace,
            use_op,
            reason_op,
            reason_tapp,
            &mut errs_opt,
            unify_bounds,
            tparams_loc,
            &xs,
            t,
            targs,
        );
        if let (Some(errs), Some(v)) = (errs_opt, errs_ref) {
            *v = errs;
        }
        result
    }

    pub fn instantiate_poly<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        unify_bounds: Option<bool>,
        poly_t: (ALoc, Vec1<TypeParam>, Type),
    ) -> Result<(Type, Vec<(Type, SubstName)>), FlowJsException> {
        let unify_bounds = unify_bounds.unwrap_or(false);
        let (tparams_loc, xs, t) = poly_t;
        flow_js_utils::instantiation_kit::instantiate_poly::<FlowJs>(
            cx,
            trace,
            use_op,
            reason_op,
            reason_tapp,
            unify_bounds,
            tparams_loc,
            &xs,
            t,
        )
    }

    pub fn instantiate_poly_call_or_new<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        poly_info: (Reason, ALoc, Vec1<TypeParam>, Type),
        call_info: (UseOp, Reason, Option<Rc<[Targ]>>, LazyHintT<Context<'cx>>),
        implicit_check: &dyn Fn() -> flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck,
    ) -> Result<Type, FlowJsException> {
        instantiation_helpers::instantiate_poly_call_or_new(
            cx,
            trace,
            poly_info,
            call_info,
            implicit_check,
        )
    }

    pub fn mk_typeapp_of_poly<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason_op: &Reason,
        reason_tapp: &Reason,
        poly_id: poly::Id,
        loc: ALoc,
        tparams: Vec1<TypeParam>,
        t: &Type,
        targs: Rc<[Type]>,
    ) -> Result<Type, FlowJsException> {
        flow_js_utils::instantiation_kit::mk_typeapp_of_poly::<FlowJs>(
            cx,
            trace,
            use_op,
            reason_op,
            reason_tapp,
            poly_id,
            loc,
            &tparams,
            t.dupe(),
            targs,
        )
    }

    pub fn mk_instance<'cx>(
        cx: &Context<'cx>,
        type_t_kind: Option<TypeTKind>,
        trace: Option<DepthTrace>,
        reason: &Reason,
        use_desc: Option<bool>,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        helpers::mk_instance(cx, type_t_kind, trace, reason, use_desc.unwrap_or(false), t)
    }

    // Eval methods
    pub fn eval_selector<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        annot: bool,
        reason: &Reason,
        t: &Type,
        selector: &Selector,
        tvar: &Tvar,
        index: i32,
    ) -> Result<(), FlowJsException> {
        eval_helpers::eval_selector(cx, trace, annot, reason, t, selector, tvar, index)
    }

    pub fn mk_type_destructor<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
        destructor: &Destructor,
        eval_id: eval::Id,
    ) -> Result<Type, FlowJsException> {
        eval_helpers::mk_type_destructor(cx, trace, use_op, reason, t, destructor, eval_id)
    }

    pub fn mk_possibly_evaluated_destructor_for_annotations<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
        destructor: &Destructor,
        eval_id: eval::Id,
    ) -> Result<Type, FlowJsException> {
        eval_helpers::mk_possibly_evaluated_destructor_for_annotations(
            cx, use_op, reason, t, destructor, eval_id,
        )
    }

    // React methods
    pub fn react_subtype_class_component_render<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        t: &Type,
        reason_op: &Reason,
        render_t: &Type,
    ) -> Result<(), FlowJsException> {
        react_kit::subtype_class_component_render(cx, trace, use_op, t, reason_op, render_t)
    }

    pub fn react_get_config<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        t: &Type,
        use_op: VirtualUseOp<ALoc>,
        reason_op: &Reason,
        tool: react::Tool<Context<'cx>>,
        polarity: Polarity,
        tout: &Type,
    ) -> Result<(), FlowJsException> {
        react_kit::get_config(cx, trace, t, use_op, reason_op, &tool, polarity, tout)
    }

    // ImplicitInstantiationKit methods
    pub fn run_conditional<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        reason: &Reason,
        tparams: &[TypeParam],
        check_t: &Type,
        extends_t: &Type,
        true_t: &Type,
        false_t: &Type,
    ) -> Result<Type, FlowJsException> {
        implicit_instantiation::kit::run_conditional(
            cx, trace, use_op, reason, tparams, check_t, extends_t, true_t, false_t,
        )
    }

    pub fn run_render_extractor<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        implicit_instantiation::kit::run_render_extractor(cx, use_op, reason, t)
    }

    pub fn run_await<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, FlowJsException> {
        implicit_instantiation::kit::run_await(cx, use_op, reason, t)
    }

    // S methods
    pub fn resolve_spread_list<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        reason_op: &Reason,
        unresolved_params: Vec<UnresolvedParam>,
        spread_resolve: SpreadResolve,
    ) -> Result<(), FlowJsException> {
        multi_arg_helpers::resolve_spread_list(
            cx,
            use_op,
            reason_op,
            unresolved_params,
            spread_resolve,
        )
    }

    pub fn possible_concrete_types_for_predicate<'cx>(
        predicate_concretizer_variant: PredicateConcretetizerVariant,
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_predicate(predicate_concretizer_variant, cx, reason, t)
    }

    pub fn possible_concrete_types_for_sentinel_prop_test<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_sentinel_prop_test(cx, reason, t)
    }

    pub fn singleton_concrete_type_for_cjs_extract_named_exports_and_type_exports<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, SpeculativeError> {
        helpers::singleton_concrete_type_for_cjs_extract_named_exports_and_type_exports(
            cx, reason, t,
        )
    }

    pub fn singleton_concretize_type_for_imports_exports<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, SpeculativeError> {
        helpers::singleton_concretize_type_for_imports_exports(cx, reason, t)
    }

    pub fn singleton_concrete_type_for_inspection<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, SpeculativeError> {
        helpers::singleton_concrete_type_for_inspection(cx, reason, t)
    }

    pub fn singleton_concrete_type_for_type_cast<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, SpeculativeError> {
        helpers::singleton_concrete_type_for_type_cast(cx, reason, t)
    }

    pub fn all_possible_concrete_types<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::all_possible_concrete_types(cx, reason, t)
    }

    pub fn singleton_concrete_type_for_match_arg<'cx>(
        cx: &Context<'cx>,
        keep_unions: bool,
        reason: &Reason,
        t: &Type,
    ) -> Result<Type, SpeculativeError> {
        helpers::singleton_concrete_type_for_match_arg(cx, keep_unions, reason, t)
    }

    pub fn possible_concrete_types_for_match_arg<'cx>(
        cx: &Context<'cx>,
        keep_unions: bool,
        reason: &Reason,
        t: &Type,
    ) -> Result<Vec<Type>, SpeculativeError> {
        helpers::possible_concrete_types_for_match_arg(cx, keep_unions, reason, t)
    }
}

// ======================================================================
// Top-level re-exports
// ======================================================================

pub fn flow<'cx>(
    cx: &Context<'cx>,
    (l, u): (&Type, &UseT<Context<'cx>>),
) -> Result<(), SpeculativeError> {
    FlowJs::flow(cx, l, u)
}

pub fn flow_t<'cx>(cx: &Context<'cx>, (t1, t2): (&Type, &Type)) -> Result<(), SpeculativeError> {
    FlowJs::flow_t(cx, t1, t2)
}

pub fn subst<'cx>(
    cx: &Context<'cx>,
    use_op: Option<UseOp>,
    force: Option<bool>,
    purpose: Option<type_subst::Purpose>,
    map: &FlowOrdMap<subst_name::SubstName, Type>,
    t: Type,
) -> Type {
    type_subst::subst(
        cx,
        use_op,
        force.unwrap_or(true),
        false,
        purpose.unwrap_or(type_subst::Purpose::Normal),
        map,
        t,
    )
}

pub fn mk_default<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    d: &flow_typing_default::Default<Type>,
) -> Result<Type, FlowJsException> {
    flow_typing_default::fold(
        d,
        &|t: &Type| -> Result<Type, FlowJsException> { Ok(t.dupe()) },
        &|t1: Result<Type, FlowJsException>,
          t2: Result<Type, FlowJsException>|
         -> Result<Type, FlowJsException> {
            let t1 = t1?;
            let t2 = t2?;
            // Tvar.mk_where cx reason (fun tvar ->
            //     flow_t cx (t1, tvar);
            //     flow_t cx (t2, tvar))
            flow_typing_tvar::mk_where_result(cx, reason.dupe(), |cx, tvar| {
                flow_t(cx, (&t1, tvar))?;
                flow_t(cx, (&t2, tvar))?;
                Ok(())
            })
        },
        &|r: Reason,
          t: Result<Type, FlowJsException>,
          sel: Selector|
         -> Result<Type, FlowJsException> {
            let t = t?;
            flow_typing_tvar::mk_no_wrap_where_result(cx, r.dupe(), |cx, _reason, tvar_id| {
                let tvar = Tvar::new(r.dupe(), tvar_id as u32);
                FlowJs::eval_selector(
                    cx,
                    None,
                    false,
                    &r,
                    &t,
                    &sel,
                    &tvar,
                    flow_common::reason::mk_id() as i32,
                )
            })
        },
    )
}

// Export some functions without the trace parameter

pub fn mk_instance<'cx>(
    cx: &Context<'cx>,
    type_t_kind: Option<TypeTKind>,
    instance_reason: &Reason,
    use_desc: Option<bool>,
    c: &Type,
) -> Result<Type, FlowJsException> {
    FlowJs::mk_instance(cx, type_t_kind, None, instance_reason, use_desc, c)
}

pub fn get_builtin_type<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    x: &str,
) -> Result<Type, FlowJsException> {
    FlowJs::get_builtin_type(cx, None, reason, use_desc, x)
}

pub fn get_builtin_react_type<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    purpose: ExpectedModulePurpose,
) -> Result<Type, FlowJsException> {
    FlowJs::get_builtin_react_type(cx, None, reason, use_desc, purpose)
}

pub fn reposition_reason<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    t: &Type,
) -> Result<Type, FlowJsException> {
    FlowJs::reposition_reason(cx, None, reason, use_desc, t)
}

pub fn filter_optional<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    opt_t: &Type,
) -> Result<u32, FlowJsException> {
    FlowJs::filter_optional(cx, None, reason, opt_t)
}

pub fn unify<'cx>(
    cx: &Context<'cx>,
    use_op: Option<UseOp>,
    t1: &Type,
    t2: &Type,
) -> Result<(), FlowJsException> {
    FlowJs::unify(cx, use_op, UnifyCause::Uncategorized, t1, t2)
}

pub fn reposition<'cx>(cx: &Context<'cx>, loc: ALoc, t: Type) -> Result<Type, FlowJsException> {
    FlowJs::reposition(cx, None, loc, None, None, t)
}

pub fn mk_typeapp_instance_annot<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: &Reason,
    reason_tapp: &Reason,
    from_value: bool,
    c: &Type,
    ts: Rc<[Type]>,
) -> Result<Type, FlowJsException> {
    FlowJs::mk_typeapp_instance_annot(
        cx,
        None,
        use_op,
        reason_op,
        reason_tapp,
        from_value,
        None,
        c,
        ts,
    )
}
pub fn mk_type_destructor<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: &Reason,
    t: &Type,
    d: &Destructor,
    id: eval::Id,
) -> Result<Type, FlowJsException> {
    match FlowJs::mk_type_destructor(
        cx,
        DepthTrace::dummy_trace(),
        use_op,
        reason,
        t,
        d,
        id.dupe(),
    ) {
        Ok(result) => Ok(result),
        Err(FlowJsException::LimitExceeded) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::ERecursionLimit(Box::new((reason.dupe(), reason.dupe()))),
            )?;
            let result = any_t::why(AnySource::AnyError(None), reason.dupe());
            let mut evaluated = cx.evaluated();
            evaluated.insert(id, result.dupe());
            cx.set_evaluated(evaluated);
            Ok(result)
        }
        Err(e) => Err(e),
    }
}

// exporting this for convenience
pub fn add_output<'cx>(cx: &Context<'cx>, msg: ErrorMessage<ALoc>) -> Result<(), FlowJsException> {
    flow_js_utils::add_output(cx, msg)
}

// Non-speculating variants

pub fn flow_non_speculating<'cx>(cx: &Context<'cx>, (l, u): (&Type, &UseT<Context<'cx>>)) {
    if let Err(err) = flow(cx, (l, u)) {
        if !flow_typing_flow_common::speculation::speculating(cx) {
            panic!("Non speculating: {:?}", err);
        }
    }
}

pub fn flow_t_non_speculating<'cx>(cx: &Context<'cx>, (t1, t2): (&Type, &Type)) {
    if let Err(err) = flow_t(cx, (t1, t2)) {
        if !flow_typing_flow_common::speculation::speculating(cx) {
            panic!("Non speculating: {:?}", err);
        }
    }
}

pub fn mk_default_non_speculating<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    d: &flow_typing_default::Default<Type>,
) -> Type {
    mk_default(cx, reason, d).expect("Non speculating")
}

pub fn mk_instance_non_speculating<'cx>(
    cx: &Context<'cx>,
    type_t_kind: Option<TypeTKind>,
    instance_reason: &Reason,
    use_desc: Option<bool>,
    c: &Type,
) -> Type {
    mk_instance(cx, type_t_kind, instance_reason, use_desc, c).expect("Non speculating")
}

pub fn get_builtin_type_non_speculating<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    x: &str,
) -> Type {
    get_builtin_type(cx, reason, use_desc, x).expect("Non speculating")
}

pub fn get_builtin_react_type_non_speculating<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    purpose: ExpectedModulePurpose,
) -> Type {
    get_builtin_react_type(cx, reason, use_desc, purpose).expect("Non speculating")
}

pub fn reposition_reason_non_speculating<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    use_desc: Option<bool>,
    t: &Type,
) -> Type {
    reposition_reason(cx, reason, use_desc, t).expect("Non speculating")
}

pub fn filter_optional_non_speculating<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    opt_t: &Type,
) -> u32 {
    filter_optional(cx, reason, opt_t).expect("Non speculating")
}

pub fn unify_non_speculating<'cx>(cx: &Context<'cx>, use_op: Option<UseOp>, t1: &Type, t2: &Type) {
    if let Err(err) = unify(cx, use_op, t1, t2) {
        if !flow_typing_flow_common::speculation::speculating(cx) {
            panic!("Non speculating: {:?}", err);
        }
    }
}

pub fn reposition_non_speculating<'cx>(cx: &Context<'cx>, loc: ALoc, t: Type) -> Type {
    reposition(cx, loc, t).expect("Non speculating")
}

pub fn mk_typeapp_instance_annot_non_speculating<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: &Reason,
    reason_tapp: &Reason,
    from_value: bool,
    c: &Type,
    ts: Rc<[Type]>,
) -> Type {
    mk_typeapp_instance_annot(cx, use_op, reason_op, reason_tapp, from_value, c, ts)
        .expect("Non speculating")
}

pub fn mk_type_destructor_non_speculating<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason: &Reason,
    t: &Type,
    d: &Destructor,
    id: eval::Id,
) -> Type {
    mk_type_destructor(cx, use_op, reason, t, d, id).expect("Non speculating")
}

pub fn add_output_non_speculating<'cx>(cx: &Context<'cx>, msg: ErrorMessage<ALoc>) {
    if let Err(err) = add_output(cx, msg) {
        if !flow_typing_flow_common::speculation::speculating(cx) {
            panic!("Non speculating: {:?}", err);
        }
    }
}
