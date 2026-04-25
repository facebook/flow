/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! This module contains the traversal functions which set up subtyping
//! constraints for every expression, statement, and declaration form in a
//! JavaScript AST; the subtyping constraints are themselves solved in module
//! Flow_js.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::flow_import_specifier;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::ReasonDescFunction;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::VirtualReasonDesc::*;
use flow_common::reason::func_reason;
use flow_common::reason::locationless_reason;
use flow_common::reason::mk_annot_reason;
use flow_common::reason::mk_expression_reason;
use flow_common::reason::mk_id;
use flow_common::reason::mk_initial_arguments_reason;
use flow_common::reason::mk_obj_lit_reason;
use flow_common::reason::mk_pattern_reason;
use flow_common::reason::mk_reason;
use flow_common::reason::string_of_desc;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_env_builder::env_api::EnvKey;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::statement;
use flow_parser::jsdoc;
use flow_parser::loc_sig::LocSig;
use flow_parser::polymorphic_ast_mapper;
use flow_parser_utils::graphql;
use flow_typing_context::Context;
use flow_typing_errors::error_message::ECallTypeArityData;
use flow_typing_errors::error_message::EComponentThisReferenceData;
use flow_typing_errors::error_message::EDuplicateClassMemberData;
use flow_typing_errors::error_message::EIllegalAssertOperatorData;
use flow_typing_errors::error_message::EIncompatibleWithUseOpData;
use flow_typing_errors::error_message::EInvalidReactCreateElementData;
use flow_typing_errors::error_message::EObjectComputedPropertyPotentialOverwriteData;
use flow_typing_errors::error_message::ETSSyntaxData;
use flow_typing_errors::error_message::ETypeGuardIncompatibleWithFunctionKindData;
use flow_typing_errors::error_message::EnumBigIntMemberNotInitializedData;
use flow_typing_errors::error_message::EnumBooleanMemberNotInitializedData;
use flow_typing_errors::error_message::EnumDuplicateMemberNameData;
use flow_typing_errors::error_message::EnumErrorKind;
use flow_typing_errors::error_message::EnumInconsistentMemberValuesData;
use flow_typing_errors::error_message::EnumInvalidMemberInitializerData;
use flow_typing_errors::error_message::EnumInvalidMemberNameData;
use flow_typing_errors::error_message::EnumMemberDuplicateValueData;
use flow_typing_errors::error_message::EnumNonIdentifierMemberNameData;
use flow_typing_errors::error_message::EnumNumberMemberNotInitializedData;
use flow_typing_errors::error_message::EnumStringMemberInconsistentlyInitializedData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::error_message::MatchErrorKind;
use flow_typing_errors::error_message::MatchInvalidCaseSyntaxData;
use flow_typing_errors::error_message::RecordErrorKind;
use flow_typing_errors::error_message::TSSyntaxKind;
use flow_typing_errors::intermediate_error_types;
use flow_typing_errors::intermediate_error_types::ContextDependentUnsupportedStatement;
use flow_typing_errors::intermediate_error_types::DeclareComponentInvalidParamKind;
use flow_typing_errors::intermediate_error_types::MatchInvalidCaseSyntax;
use flow_typing_errors::intermediate_error_types::RecordDeclarationInvalidSyntax;
use flow_typing_errors::intermediate_error_types::TsLibSyntaxKind;
use flow_typing_errors::intermediate_error_types::UnsupportedSyntax;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_flow_js::natural_inference;
use flow_typing_flow_js::natural_inference::SyntacticFlags;
use flow_typing_flow_js::tvar_resolver;
use flow_typing_flow_js::type_inference_hooks_js;
use flow_typing_loc_env::func_class_sig_types;
use flow_typing_type::type_;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::LazyHintT;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::type_collector::TypeCollector;
use flow_typing_type::type_::*;
use flow_typing_type::type_util::*;
use flow_typing_utils::abnormal::AbnormalControlFlow;
use flow_typing_utils::speculation_flow;
use flow_typing_utils::type_env;
use flow_typing_utils::type_hint;
use flow_typing_utils::type_operation_utils;
use flow_typing_utils::typed_ast_utils;
type LazyBool<'cx> =
    Rc<flow_lazy::Lazy<Context<'cx>, bool, Box<dyn FnOnce(&Context<'cx>) -> bool + 'cx>>>;

use crate::class_sig;
use crate::refinement;
use crate::type_annotation;

// *************
// * Utilities *
// *************

mod optional_chain {
    use flow_common::reason::Reason;
    use flow_typing_context::Context;
    use flow_typing_flow_common::flow_js_utils::FlowJsException;
    use flow_typing_type::type_::DepthTrace;
    use flow_typing_type::type_::Type;
    use flow_typing_type::type_::UseT;
    use flow_typing_type::type_::type_collector::TypeCollector;

    pub fn run<'a>(
        cx: &Context<'a>,
        lhs: &Type,
        reason: &Reason,
        lhs_reason: &Reason,
        upper: &UseT<Context<'a>>,
        voided_out_collector: &Option<TypeCollector>,
    ) -> Result<(), FlowJsException> {
        flow_typing_flow_js::optional_chain_kit::run(
            cx,
            DepthTrace::unit_trace(),
            lhs,
            reason,
            lhs_reason,
            upper,
            voided_out_collector,
        )
    }
}

struct ChainingConf<'a, 'b, A, B> {
    refinement_action: Option<Box<dyn Fn(&Context<'a>, &A, &Type, Type) -> Type + 'b>>,
    refine: Box<dyn Fn(&Context<'a>) -> Option<Type> + 'b>,
    subexpressions: Box<dyn FnOnce(&Context<'a>) -> Result<(A, B), AbnormalControlFlow> + 'b>,
    get_result: Box<dyn Fn(&Context<'a>, &A, Reason, &Type) -> Type + 'b>,
    get_opt_use: Rc<dyn Fn(&Context<'a>, &A, Reason) -> type_::OptUseT<Context<'a>> + 'b>,
    get_reason: Box<dyn Fn(&Type) -> Reason + 'b>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassMemberKind {
    ClassMemberField,
    ClassMemberGetter,
    ClassMemberGetterSetter,
    ClassMemberMethod,
    ClassMemberSetter,
}

struct SeenNames {
    static_names: BTreeMap<FlowSmolStr, ClassMemberKind>,
    instance_names: BTreeMap<FlowSmolStr, ClassMemberKind>,
}

fn empty_seen_names() -> SeenNames {
    SeenNames {
        static_names: BTreeMap::new(),
        instance_names: BTreeMap::new(),
    }
}

pub mod object_expression_acc {

    use flow_typing_utils::speculation_flow;

    use super::*;

    #[derive(Debug, Clone)]
    pub(super) enum Element {
        Spread(Type),
        Slice {
            slice_pmap: properties::PropertiesMap,
            computed_props: Option<DictType>,
        },
    }

    #[derive(Debug, Clone)]
    pub(super) enum ComputedProp {
        Named {
            name: Name,
            prop: type_::Property,
        },
        IgnoredInvalidNonLiteralKey,
        NonLiteralKey {
            key_loc: ALoc,
            key: Type,
            value: (ALoc, Type),
            named_set_opt: Option<BTreeSet<Name>>,
        },
        SpreadEmpty(Reason),
    }

    #[derive(Debug, Clone)]
    pub struct ObjectExpressionAcc {
        pub(super) obj_pmap: properties::PropertiesMap,
        pub(super) computed_props: Option<DictType>,
        pub(super) tail: Vec<Element>,
        pub(super) proto: Option<Type>,
        pub(super) obj_key_autocomplete: bool,
    }

    impl ObjectExpressionAcc {
        pub fn empty() -> Self {
            Self {
                obj_pmap: properties::PropertiesMap::new(),
                computed_props: None,
                tail: vec![],
                proto: None,
                obj_key_autocomplete: false,
            }
        }

        pub(super) fn empty_slice() -> Element {
            Element::Slice {
                slice_pmap: properties::PropertiesMap::new(),
                computed_props: None,
            }
        }

        pub(super) fn head_slice(&self) -> Option<Element> {
            if self.obj_pmap.is_empty() && self.computed_props.is_none() {
                None
            } else {
                Some(Element::Slice {
                    slice_pmap: self.obj_pmap.dupe(),
                    computed_props: self.computed_props.clone(),
                })
            }
        }

        pub fn add_prop(mut self, f: impl FnOnce(&mut properties::PropertiesMap)) -> Self {
            f(&mut self.obj_pmap);
            self
        }

        pub(super) fn add_proto(self, p: Type) -> Self {
            Self {
                proto: Some(p),
                ..self
            }
        }

        pub fn add_spread(self, t: Type) -> Self {
            let mut tail = match self.head_slice() {
                None => self.tail,
                Some(slice) => {
                    let mut new_tail = self.tail;
                    new_tail.push(slice);
                    new_tail
                }
            };
            tail.push(Element::Spread(t));
            Self {
                obj_pmap: properties::PropertiesMap::new(),
                tail,
                computed_props: self.computed_props,
                proto: self.proto,
                obj_key_autocomplete: self.obj_key_autocomplete,
            }
        }

        pub(super) fn add_computed<'a>(self, cx: &Context<'a>, computed: ComputedProp) -> Self {
            match computed {
                ComputedProp::Named { name, prop } => self.add_prop(|pmap| {
                    pmap.insert(name, prop);
                }),
                ComputedProp::IgnoredInvalidNonLiteralKey => self,
                ComputedProp::NonLiteralKey {
                    key_loc,
                    key,
                    value: (value_loc, value),
                    named_set_opt,
                } => {
                    let overlapping_name_map: properties::PropertiesMap = match named_set_opt {
                        None => self
                            .obj_pmap
                            .iter()
                            .filter(|&(prop_name, _)| {
                                let prop_t = Type::new(TypeInner::DefT(
                                    mk_reason(
                                        VirtualReasonDesc::RStringLit(prop_name.dupe()),
                                        key_loc.dupe(),
                                    ),
                                    DefT::new(DefTInner::SingletonStrT {
                                        from_annot: false,
                                        value: prop_name.dupe(),
                                    }),
                                ));
                                speculation_flow::is_subtyping_successful(cx, prop_t, key.dupe())
                                    .unwrap_or(false)
                            })
                            .map(|(n, v)| (n.dupe(), v.dupe()))
                            .collect(),
                        Some(named_set) => self
                            .obj_pmap
                            .iter()
                            .filter(|&(n, _)| !named_set.contains(n))
                            .map(|(n, v)| (n.dupe(), v.dupe()))
                            .collect(),
                    };
                    if overlapping_name_map.is_empty() {
                        match self.computed_props {
                            None => {
                                let value = {
                                    let LazyHintT(has_hint, lazy_hint) =
                                        flow_typing_utils::type_env::get_hint(cx, value_loc);
                                    if !has_hint {
                                        value
                                    } else {
                                        let reason = reason_of_t(&key).dupe();
                                        if let HintEvalResult::HintAvailable(t, _) =
                                            lazy_hint(cx, true, Some(true), reason)
                                            && speculation_flow::is_subtyping_successful(
                                                cx,
                                                value.dupe(),
                                                t.dupe(),
                                            )
                                            .unwrap_or(false)
                                        {
                                            // If a hint on this key exists and is a supertype of the value
                                            // type of the first computed element, then use that as target
                                            // for all subsequent computed elements.
                                            t
                                        } else {
                                            value
                                        }
                                    }
                                };
                                Self {
                                    computed_props: Some(DictType {
                                        dict_name: None,
                                        key,
                                        value,
                                        dict_polarity: Polarity::Neutral,
                                    }),
                                    ..self
                                }
                            }
                            Some(DictType {
                                dict_name: _,
                                key: ref existing_key,
                                value: ref existing_value,
                                ..
                            }) => {
                                let use_op = UseOp::Op(std::sync::Arc::new(
                                    type_::RootUseOp::ObjectAddComputedProperty {
                                        op: mk_reason(VirtualReasonDesc::RProperty(None), key_loc),
                                    },
                                ));
                                if matches!(
                                    &*cx.typing_mode(),
                                    flow_typing_context::TypingMode::CheckingMode
                                ) {
                                    // These checks should not affect synthesis results
                                    flow_js::flow_non_speculating(
                                        cx,
                                        (
                                            &key,
                                            &UseT::new(UseTInner::UseT(
                                                use_op.dupe(),
                                                existing_key.dupe(),
                                            )),
                                        ),
                                    );
                                    flow_js::flow_non_speculating(
                                        cx,
                                        (
                                            &value,
                                            &UseT::new(UseTInner::UseT(
                                                use_op,
                                                existing_value.dupe(),
                                            )),
                                        ),
                                    );
                                }
                                self
                            }
                        }
                    } else {
                        let overwritten_locs: Vec<ALoc> = overlapping_name_map
                            .values()
                            .flat_map(|prop| match property::def_locs(prop) {
                                None => vec![],
                                Some(nel) => nel.to_vec(),
                            })
                            .collect();
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EObjectComputedPropertyPotentialOverwrite(Box::new(
                                EObjectComputedPropertyPotentialOverwriteData {
                                    key_loc,
                                    overwritten_locs,
                                },
                            )),
                        );
                        self
                    }
                }
                ComputedProp::SpreadEmpty(r) => {
                    self.add_spread(Type::new(TypeInner::DefT(r, DefT::new(DefTInner::EmptyT))))
                }
            }
        }

        pub(super) fn set_obj_key_autocomplete(self) -> Self {
            Self {
                obj_key_autocomplete: true,
                ..self
            }
        }

        pub(super) fn elements_rev(mut self) -> (Element, Vec<Element>) {
            match self.head_slice() {
                Some(slice) => {
                    self.tail.reverse();
                    (slice, self.tail)
                }
                None => match self.tail.pop() {
                    None => (Self::empty_slice(), vec![]),
                    Some(x) => {
                        self.tail.reverse();
                        (x, self.tail)
                    }
                },
            }
        }

        pub fn mk_object_from_spread_acc<'a>(
            self,
            cx: &Context<'a>,
            reason: Reason,
            as_const: bool,
            frozen: bool,
            default_proto: Type,
        ) -> Type {
            use object::spread;

            let proto = self.proto.dupe();
            let obj_key_autocomplete_flag = self.obj_key_autocomplete;
            let os = self.elements_rev();
            match os {
                (
                    Element::Slice {
                        slice_pmap,
                        computed_props,
                    },
                    ref rest,
                ) if rest.is_empty() => {
                    let proto = proto.unwrap_or(default_proto);
                    let obj_kind = match computed_props {
                        None => ObjKind::Exact,
                        Some(dict_t) => ObjKind::Indexed(dict_t),
                    };
                    let obj_t = flow_typing_flow_common::obj_type::mk_with_proto(
                        cx,
                        reason.dupe(),
                        obj_kind,
                        None,
                        None,
                        Some(slice_pmap),
                        None,
                        proto,
                    );
                    if obj_key_autocomplete_flag {
                        let obj_t_clone = obj_t.dupe();
                        let reason_clone = reason.dupe();
                        let get_autocomplete_t = move || {
                            flow_typing_flow_js::tvar_resolver::mk_tvar_and_fully_resolve_where(
                                cx,
                                reason_clone,
                                |cx, tvar| {
                                    flow_js::flow_t_non_speculating(cx, (&obj_t_clone, tvar));
                                },
                            )
                        };
                        let LazyHintT(_has_hint, lazy_hint) =
                            flow_typing_utils::type_env::get_hint(cx, reason.loc().dupe());
                        let result = lazy_hint(cx, false, None, reason);
                        flow_typing_utils::type_hint::with_hint_result(
                            |t| t,
                            get_autocomplete_t,
                            result,
                        )
                    } else {
                        obj_t
                    }
                }
                os => {
                    let (t, ts, head_slice) = {
                        let (t, ts) = os;
                        // We don't need to do this recursively because every pair of slices must be separated
                        // by a spread
                        match (t, ts) {
                            (Element::Spread(t), ts) => {
                                let ts: Vec<spread::Operand> = ts
                                    .into_iter()
                                    .map(|elem| match elem {
                                        Element::Spread(t) => spread::Operand::Type(t),
                                        Element::Slice {
                                            slice_pmap,
                                            computed_props,
                                        } => spread::Operand::Slice(spread::OperandSlice::new(
                                            spread::OperandSliceInner {
                                                reason: reason.dupe(),
                                                prop_map: slice_pmap,
                                                dict: computed_props,
                                                generics: flow_typing_generics::spread_empty(),
                                                reachable_targs: vec![].into(),
                                            },
                                        )),
                                    })
                                    .collect();
                                (t, ts, None)
                            }
                            //   | (Slice { slice_pmap = prop_map; computed_props }, Spread t :: ts) ->
                            (
                                Element::Slice {
                                    slice_pmap: prop_map,
                                    computed_props,
                                },
                                ts,
                            ) => {
                                let mut iter = ts.into_iter();
                                match iter.next() {
                                    Some(Element::Spread(t)) => {
                                        let head_slice =
                                            spread::OperandSlice::new(spread::OperandSliceInner {
                                                reason: reason.dupe(),
                                                prop_map,
                                                dict: computed_props,
                                                generics: flow_typing_generics::spread_empty(),
                                                reachable_targs: vec![].into(),
                                            });
                                        let ts: Vec<spread::Operand> = iter
                                            .map(|elem| match elem {
                                                Element::Spread(t) => spread::Operand::Type(t),
                                                Element::Slice {
                                                    slice_pmap,
                                                    computed_props,
                                                } => spread::Operand::Slice(
                                                    spread::OperandSlice::new(
                                                        spread::OperandSliceInner {
                                                            reason: reason.dupe(),
                                                            prop_map: slice_pmap,
                                                            dict: computed_props,
                                                            generics:
                                                                flow_typing_generics::spread_empty(
                                                                ),
                                                            reachable_targs: vec![].into(),
                                                        },
                                                    ),
                                                ),
                                            })
                                            .collect();
                                        (t, ts, Some(head_slice))
                                    }
                                    //   | _ -> failwith "Invariant Violation: spread list has two slices in a row"
                                    _ => panic!(
                                        "Invariant Violation: spread list has two slices in a row"
                                    ),
                                }
                            }
                        }
                    };
                    let target = spread::Target::Value {
                        make_seal: flow_typing_flow_common::obj_type::mk_seal(frozen, as_const),
                    };
                    let tool = object::ResolveTool::Resolve(object::Resolve::Next);
                    let state = spread::State {
                        todo_rev: ts.into(),
                        acc: match head_slice {
                            Some(x) => vec![spread::AccElement::InlineSlice(x)].into(),
                            None => vec![].into(),
                        },
                        spread_id: mk_id() as i32,
                        union_reason: None,
                        curr_resolve_idx: 0,
                    };
                    let tout = flow_typing_flow_js::tvar_resolver::mk_tvar_and_fully_resolve_where(
                        cx,
                        reason.dupe(),
                        |cx, tout| {
                            let use_op =
                                UseOp::Op(std::sync::Arc::new(type_::RootUseOp::ObjectSpread {
                                    op: reason.dupe(),
                                }));
                            flow_js::flow_non_speculating(
                                cx,
                                (
                                    &t,
                                    &UseT::new(UseTInner::ObjKitT(
                                        use_op,
                                        reason.dupe(),
                                        Box::new(tool),
                                        Box::new(object::Tool::Spread(Box::new((target, state)))),
                                        tout.dupe(),
                                    )),
                                ),
                            );
                        },
                    );
                    if obj_key_autocomplete_flag {
                        let LazyHintT(_has_hint, lazy_hint) =
                            flow_typing_utils::type_env::get_hint(cx, reason.loc().dupe());
                        let result = lazy_hint(cx, false, None, reason);
                        flow_typing_utils::type_hint::with_hint_result(|t| t, || tout, result)
                    } else {
                        tout
                    }
                }
            }
        }
    }
}
use object_expression_acc::ObjectExpressionAcc;

fn mk_ident(loc: ALoc, comments: Option<()>, name: FlowSmolStr) -> ast::Identifier<ALoc, ALoc> {
    ast::Identifier::new(ast::IdentifierInner {
        loc,
        name,
        comments: comments.map(|()| ast::Syntax {
            leading: std::sync::Arc::from([]),
            trailing: std::sync::Arc::from([]),
            internal: (),
        }),
    })
}

fn translate_identifier_or_literal_key(
    t: Type,
    key: &expression::object::Key<ALoc, ALoc>,
) -> expression::object::Key<ALoc, (ALoc, Type)> {
    use ast::expression::object::Key;
    match key {
        Key::Identifier(id) => Key::Identifier(ast::Identifier::new(ast::IdentifierInner {
            loc: (id.loc.dupe(), t),
            name: id.name.dupe(),
            comments: id.comments.dupe(),
        })),
        Key::StringLiteral((loc, lit)) => Key::StringLiteral(((loc.dupe(), t), lit.clone())),
        Key::NumberLiteral((loc, lit)) => Key::NumberLiteral(((loc.dupe(), t), lit.clone())),
        Key::BigIntLiteral((loc, lit)) => Key::BigIntLiteral(((loc.dupe(), t), lit.clone())),
        Key::PrivateName(_) | Key::Computed(_) => {
            panic!("precondition not met")
        }
    }
}

fn name_of_identifier_or_literal_key(
    key: &expression::object::Key<ALoc, ALoc>,
) -> Result<FlowSmolStr, Box<ErrorMessage<ALoc>>> {
    use ast::expression::object::Key;
    match key {
        Key::Identifier(id) => Ok(id.name.dupe()),
        Key::StringLiteral((_loc, lit)) => Ok(lit.value.dupe()),
        Key::NumberLiteral((loc, lit)) => {
            if flow_common::js_number::is_float_safe_integer(lit.value) {
                let name = flow_common::js_number::ecma_string_of_float(lit.value);
                Ok(FlowSmolStr::new(name))
            } else {
                Err(Box::new(ErrorMessage::EUnsupportedKeyInObject {
                    loc: loc.dupe(),
                    obj_kind: intermediate_error_types::ObjKind::Literal,
                    key_error_kind: intermediate_error_types::InvalidObjKey::kind_of_num_value(
                        lit.value,
                    ),
                }))
            }
        }
        Key::BigIntLiteral((loc, _))
        | Key::PrivateName(ast::PrivateName { loc, .. })
        | Key::Computed(ast::ComputedKey { loc, .. }) => {
            Err(Box::new(ErrorMessage::EUnsupportedKeyInObject {
                loc: loc.dupe(),
                obj_kind: intermediate_error_types::ObjKind::Literal,
                key_error_kind: intermediate_error_types::InvalidObjKey::Other,
            }))
        }
    }
}

fn convert_call_targs<'a>(
    cx: &Context<'a>,
    tparams_map: &FlowOrdMap<SubstName, Type>,
    call_targs: &expression::CallTypeArgs<ALoc, ALoc>,
) -> (Vec<Targ>, expression::CallTypeArgs<ALoc, (ALoc, Type)>) {
    use ast::expression::CallTypeArg::*;
    let arguments = &call_targs.arguments;
    let comments = &call_targs.comments;
    let mut ts: Vec<Targ> = Vec::new();
    let mut tasts = Vec::new();
    for arg in arguments.iter() {
        match arg {
            Explicit(type_ast) => {
                let tast = type_annotation::convert(cx, tparams_map.dupe(), type_ast);
                let (_, t) = tast.loc();
                let t = t.dupe();
                ts.push(Targ::ExplicitArg(t));
                tasts.push(Explicit(tast));
            }
            Implicit(implicit) => {
                let loc = implicit.loc.dupe();
                let reason = mk_reason(VirtualReasonDesc::RImplicitInstantiation, loc.dupe());
                let id = flow_typing_tvar::mk_no_wrap(cx, &reason);
                ts.push(Targ::ImplicitArg(Tvar::new(reason.dupe(), id as u32)));
                let open_t = Type::new(TypeInner::OpenT(Tvar::new(reason, id as u32)));
                tasts.push(Implicit(expression::CallTypeArgImplicit {
                    loc: (loc, open_t),
                    comments: implicit.comments.dupe(),
                }));
            }
        }
    }
    (
        ts,
        expression::CallTypeArgs {
            loc: (
                call_targs.loc.dupe(),
                any_t::at(AnySource::AnyError(None), call_targs.loc.dupe()),
            ),
            arguments: tasts.into(),
            comments: comments.dupe(),
        },
    )
}

fn convert_call_targs_opt<'a>(
    cx: &Context<'a>,
    targs: Option<&expression::CallTypeArgs<ALoc, ALoc>>,
) -> (
    Option<Vec<Targ>>,
    Option<expression::CallTypeArgs<ALoc, (ALoc, Type)>>,
) {
    match targs {
        None => (None, None),
        Some(args) => {
            let (targts, targs_ast) = convert_call_targs(cx, &FlowOrdMap::new(), args);
            (Some(targts), Some(targs_ast))
        }
    }
}

pub fn convert_call_targs_opt_prime<'a>(
    cx: &Context<'a>,
    targs: Option<&expression::CallTypeArgs<ALoc, ALoc>>,
) -> Option<Vec<Targ>> {
    match targs {
        None => None,
        Some(args) => {
            let (targts, _) = convert_call_targs(cx, &FlowOrdMap::new(), args);
            Some(targts)
        }
    }
}

type ALocThisFinder = flow_parser_utils::this_finder::Finder<ALoc>;

fn error_on_this_uses_in_object_methods<'a>(
    cx: &Context<'a>,
    properties: &[expression::object::Property<ALoc, ALoc>],
) {
    use flow_parser::ast_visitor::AstVisitor;

    for prop in properties {
        match prop {
            expression::object::Property::NormalProperty(normal_prop) => {
                let (prop_loc, key, func) = match normal_prop {
                    expression::object::NormalProperty::Method {
                        loc,
                        key,
                        value: (_, func),
                    } => (loc, key, func),
                    expression::object::NormalProperty::Get {
                        loc,
                        key,
                        value: (_, func),
                        ..
                    } => (loc, key, func),
                    expression::object::NormalProperty::Set {
                        loc,
                        key,
                        value: (_, func),
                        ..
                    } => (loc, key, func),
                    _ => continue,
                };
                let mut finder = ALocThisFinder::new();
                let Ok(()) = finder.function_(prop_loc, func);
                for (loc, kind) in &finder.acc {
                    let reason = match key {
                        expression::object::Key::Identifier(id) => mk_reason(
                            VirtualReasonDesc::RMethod(Some(id.name.dupe())),
                            prop_loc.dupe(),
                        ),
                        expression::object::Key::PrivateName(pn) => mk_reason(
                            VirtualReasonDesc::RMethod(Some(pn.name.dupe())),
                            prop_loc.dupe(),
                        ),
                        expression::object::Key::StringLiteral((_, sl)) => mk_reason(
                            VirtualReasonDesc::RMethod(Some(sl.raw.dupe())),
                            prop_loc.dupe(),
                        ),
                        _ => mk_reason(VirtualReasonDesc::RMethod(None), prop_loc.dupe()),
                    };
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EObjectThisSuperReference(Box::new((
                            loc.dupe(),
                            reason,
                            match kind {
                                flow_parser_utils::this_finder::Kind::This => {
                                    flow_typing_errors::error_message::ThisFinderKind::This
                                }
                                flow_parser_utils::this_finder::Kind::Super => {
                                    flow_typing_errors::error_message::ThisFinderKind::Super
                                }
                            },
                        ))),
                    );
                }
            }
            _ => {}
        }
    }
}

fn error_on_this_uses_in_components<'a>(
    cx: &Context<'a>,
    component: &statement::ComponentDeclaration<ALoc, ALoc>,
) {
    use flow_parser::ast_visitor::AstVisitor;

    let sig_loc = &component.sig_loc;
    let mut finder = ALocThisFinder::new();
    match &component.body {
        None => {}
        Some(body) => {
            let Ok(()) = finder.component_body(body);
        }
    }
    for (this_loc, kind) in &finder.acc {
        match kind {
            flow_parser_utils::this_finder::Kind::Super => {
                panic!("Super expressions in components should be syntax errors")
            }
            flow_parser_utils::this_finder::Kind::This => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EComponentThisReference(Box::new(EComponentThisReferenceData {
                        component_loc: sig_loc.dupe(),
                        this_loc: this_loc.dupe(),
                    })),
                );
            }
        }
    }
}

/// Given the expression of a statement expression, returns a list of child
/// expressions which are _potentially_ unhandled promises. At this point,
/// we don't know if they are actually of type Promise. We will determine that
/// later, but we don't need to even check that if we can tell that the
/// expression is being handled ("used") syntactically here.
fn syntactically_unhandled_promises<'a>(
    expr: &'a expression::Expression<ALoc, (ALoc, Type)>,
) -> Vec<&'a expression::Expression<ALoc, (ALoc, Type)>> {
    use ast::expression::ExpressionInner;
    use ast::expression::member;

    match expr.deref() {
        ExpressionInner::Assignment { .. } => vec![],

        // Call to `catch` or `finally` with one argument
        ExpressionInner::Call { inner, .. }
            if matches!(
                inner.callee.deref(),
                ExpressionInner::Member { inner: m, .. }
                if matches!(
                    &m.property,
                    member::Property::PropertyIdentifier(id)
                    if matches!(id.name.as_str(), "catch" | "finally")
                )
            ) && !inner.arguments.arguments.is_empty() =>
        {
            vec![]
        }

        ExpressionInner::OptionalCall { inner, .. }
            if matches!(
                inner.call.callee.deref(),
                ExpressionInner::OptionalMember { inner: m, .. }
                if matches!(
                    &m.member.property,
                    member::Property::PropertyIdentifier(id)
                    if matches!(id.name.as_str(), "catch" | "finally")
                )
            ) && !inner.call.arguments.arguments.is_empty() =>
        {
            vec![]
        }

        // Call to `then` with two arguments
        ExpressionInner::Call { inner, .. }
            if matches!(
                inner.callee.deref(),
                ExpressionInner::Member { inner: m, .. }
                if matches!(
                    &m.property,
                    member::Property::PropertyIdentifier(id)
                    if id.name.as_str() == "then"
                )
            ) && inner.arguments.arguments.len() >= 2 =>
        {
            vec![]
        }
        ExpressionInner::OptionalCall { inner, .. }
            if matches!(
                inner.call.callee.deref(),
                ExpressionInner::OptionalMember { inner: m, .. }
                if matches!(
                    &m.member.property,
                    member::Property::PropertyIdentifier(id)
                    if id.name.as_str() == "then"
                )
            ) && inner.call.arguments.arguments.len() >= 2 =>
        {
            vec![]
        }

        // Recurse into logical operands for expressions like `condition && somePromise();
        ExpressionInner::Logical { inner, .. } => {
            let mut result = syntactically_unhandled_promises(&inner.left);
            result.extend(syntactically_unhandled_promises(&inner.right));
            result
        }

        // Recurse into conditional operands for expressions like `b ? x : somePromise();`
        ExpressionInner::Conditional { inner, .. } => {
            let mut result = syntactically_unhandled_promises(&inner.consequent);
            result.extend(syntactically_unhandled_promises(&inner.alternate));
            result
        }

        _ => vec![expr],
    }
}

// (* In positions where an annotation may be present or an annotation can be pushed down,
//  * we should prefer the annotation over the pushed-down annotation. *)
// let mk_inference_target_with_annots ~has_hint annot_or_inferred =
//   match (annot_or_inferred, has_hint) with
//   | (Annotated _, _) -> annot_or_inferred
//   | (_, true) -> Annotated (type_t_of_annotated_or_inferred annot_or_inferred)
//   | _ -> annot_or_inferred
// In positions where an annotation may be present or an annotation can be pushed down,
// we should prefer the annotation over the pushed-down annotation.
fn mk_inference_target_with_annots(
    has_hint: bool,
    annot_or_inferred: AnnotatedOrInferred,
) -> AnnotatedOrInferred {
    match (&annot_or_inferred, has_hint) {
        (AnnotatedOrInferred::Annotated(_), _) => annot_or_inferred,
        (_, true) => AnnotatedOrInferred::Annotated(
            type_t_of_annotated_or_inferred(&annot_or_inferred).dupe(),
        ),
        _ => annot_or_inferred,
    }
}

// ******************
// * Constraint gen *
// ******************

// We assume that constructor functions return void
// and constructions return objects.
// TODO: This assumption does not always hold.
// If construction functions return non-void values (e.g., functions),
// then those values are returned by constructions.
fn new_call<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    reason: Reason,
    use_op: UseOp,
    class_: Type,
    targs: Option<Vec<Targ>>,
    args: Vec<CallArg>,
) -> (Type, Type) {
    let specialized_ctor = cx.new_specialized_callee();
    let t = tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason.dupe(), |cx, tout| {
        flow_js::flow_non_speculating(
            cx,
            (
                &class_,
                &UseT::new(UseTInner::ConstructorT(Box::new(ConstructorTData {
                    use_op: use_op.dupe(),
                    reason: reason.dupe(),
                    targs: targs.clone().map(|v| v.into()),
                    args: args.clone().into(),
                    tout: tout.dupe(),
                    return_hint: type_env::get_hint(cx, loc.dupe()),
                    specialized_ctor: Some(specialized_ctor.clone()),
                }))),
            ),
        )
    });
    let ctor_t = flow_js_utils::callee_recorder::type_for_tast_opt(reason, &specialized_ctor)
        .unwrap_or_else(|| class_.dupe());
    (t, ctor_t)
}

// let func_call_opt_use
//     cx loc reason ~use_op ?(call_strict_arity = true) targts argts specialized_callee =
//   let opt_app =
//     mk_opt_functioncalltype reason targts argts call_strict_arity specialized_callee
//   in
//   let return_hint = Type_env.get_hint cx loc in
//   OptCallT { use_op; reason; opt_funcalltype = opt_app; return_hint }
fn func_call_opt_use<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    reason: Reason,
    use_op: UseOp,
    call_strict_arity: bool,
    targts: Option<Vec<Targ>>,
    argts: Vec<CallArg>,
    specialized_callee: Option<type_::SpecializedCallee>,
) -> type_::OptUseT<Context<'a>> {
    let opt_app = type_::mk_opt_functioncalltype(
        reason.dupe(),
        targts.map(|v| v.into()),
        argts.into(),
        call_strict_arity,
        specialized_callee,
    );
    let return_hint = type_env::get_hint(cx, loc);
    type_::OptUseT::OptCallT {
        use_op,
        reason,
        opt_funcalltype: opt_app,
        return_hint,
    }
}

// let func_call cx loc reason ~use_op ?(call_strict_arity = true) func_t targts argts t_callee =
//   let opt_use =
//     func_call_opt_use cx loc reason ~use_op ~call_strict_arity targts argts t_callee
//   in
//   Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
//       Flow.flow cx (func_t, apply_opt_use opt_use t)
//   )
fn func_call<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    reason: Reason,
    use_op: UseOp,
    call_strict_arity: bool,
    func_t: Type,
    targts: Option<Vec<Targ>>,
    argts: Vec<CallArg>,
    t_callee: Option<type_::SpecializedCallee>,
) -> Type {
    let opt_use = func_call_opt_use(
        cx,
        loc,
        reason.dupe(),
        use_op,
        call_strict_arity,
        targts,
        argts,
        t_callee,
    );
    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(cx, reason, |cx, t_reason, t_id| {
        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
        flow_js::flow_non_speculating(cx, (&func_t, &type_::apply_opt_use(opt_use, tvar)));
    })
}

fn method_call_opt_use<'a>(
    cx: &Context<'a>,
    opt_state: OptState,
    voided_out_collector: TypeCollector,
    reason: Reason,
    use_op: UseOp,
    private_: bool,
    call_strict_arity: bool,
    prop_loc: ALoc,
    expr: &expression::Expression<ALoc, ALoc>,
    name: &FlowSmolStr,
    chain_loc: ALoc,
    targts: Option<Vec<Targ>>,
    argts: Vec<CallArg>,
    specialized_callee: Option<type_::SpecializedCallee>,
) -> type_::OptUseT<Context<'a>> {
    let expr_loc = expr.loc().dupe();
    let prop_name = Name::new(name.dupe());
    let reason_prop = mk_reason(
        VirtualReasonDesc::RProperty(Some(prop_name.dupe())),
        prop_loc.dupe(),
    );
    let reason_expr = mk_reason(
        VirtualReasonDesc::RProperty(Some(prop_name.dupe())),
        expr_loc,
    );
    let opt_methodcalltype = type_::mk_opt_methodcalltype(
        None,
        targts.map(|v| v.into()),
        argts.into(),
        call_strict_arity,
    );
    let propref = mk_named_prop(reason_prop, false, prop_name.dupe());
    let action = match opt_state {
        OptState::AssertChain | OptState::NewChain => {
            let (voided_out_collector, exp_reason) = if opt_state == OptState::NewChain {
                (
                    Some(voided_out_collector),
                    mk_reason(VirtualReasonDesc::ROptionalChain, chain_loc.dupe()),
                )
            } else {
                (
                    None,
                    mk_reason(VirtualReasonDesc::RNonnullAssert, chain_loc.dupe()),
                )
            };
            type_::OptMethodAction::OptChainM(Box::new(type_::OptChainMData {
                exp_reason,
                lhs_reason: mk_expression_reason(expr),
                opt_methodcalltype,
                voided_out_collector,
                return_hint: type_::hint_unavailable(),
                specialized_callee,
            }))
        }
        _ => type_::OptMethodAction::OptCallM(Box::new(type_::OptCallMData {
            opt_methodcalltype,
            return_hint: type_env::get_hint(cx, chain_loc),
            specialized_callee,
        })),
    };
    if private_ {
        let class_entries = type_env::get_class_entries(cx);
        type_::OptUseT::OptPrivateMethodT(
            use_op,
            reason,
            reason_expr,
            name.dupe(),
            class_entries.into(),
            false,
            action,
        )
    } else {
        type_::OptUseT::OptMethodT(use_op, reason, reason_expr, propref, action)
    }
}

// returns (type of method itself, type returned from method)
fn method_call<'a>(
    cx: &Context<'a>,
    reason: Reason,
    use_op: UseOp,
    call_strict_arity: bool,
    prop_loc: ALoc,
    expr: &expression::Expression<ALoc, ALoc>,
    obj_t: Type,
    name: &FlowSmolStr,
    targts: Option<Vec<Targ>>,
    argts: Vec<CallArg>,
) -> (Type, Type) {
    let expr_loc = expr.loc().dupe();
    // match Refinement.get ~allow_optional:true cx expr (loc_of_reason reason) with
    match refinement::get(true, cx, expr, reason.loc().dupe()) {
        // | Some f ->
        Some(f) => {
            // Note: the current state of affairs is that we understand
            // member expressions as having refined types, rather than
            // understanding receiver objects as carrying refined properties.
            // generalizing this properly is a todo, and will deliver goodness.
            // meanwhile, here we must hijack the property selection normally
            // performed by the flow algorithm itself.
            let out = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                cx,
                reason.dupe(),
                |cx, t_reason, t_id| {
                    let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                    let app = type_::mk_boundfunctioncalltype(
                        obj_t.dupe(),
                        targts.clone().map(|v| v.into()),
                        argts.clone().into(),
                        call_strict_arity,
                        tvar,
                    );
                    let use_t = UseT::new(UseTInner::CallT(Box::new(CallTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        call_action: Box::new(type_::CallAction::Funcalltype(Box::new(app))),
                        return_hint: type_::hint_unavailable(),
                    })));
                    flow_js::flow_non_speculating(cx, (&f, &use_t));
                },
            );
            (f, out)
        }
        None => {
            let name = Name::new(name.dupe());
            let reason_prop = mk_reason(VirtualReasonDesc::RProperty(Some(name.dupe())), prop_loc);
            let specialized_callee = cx.new_specialized_callee();
            let out = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                cx,
                reason.dupe(),
                |cx, t_reason, t_id| {
                    let reason_expr = mk_reason(
                        VirtualReasonDesc::RProperty(Some(name.dupe())),
                        expr_loc.dupe(),
                    );
                    let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                    let methodcalltype = type_::mk_methodcalltype(
                        targts.clone().map(|v| v.into()),
                        argts.clone().into(),
                        None,
                        call_strict_arity,
                        tvar,
                    );
                    let propref = mk_named_prop(reason_prop.dupe(), false, name.dupe());
                    let use_t = UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        prop_reason: reason_expr,
                        propref: Box::new(propref),
                        method_action: Box::new(type_::MethodAction::CallM(Box::new(
                            type_::CallMData {
                                methodcalltype,
                                return_hint: type_::hint_unavailable(),
                                specialized_callee: Some(specialized_callee.clone()),
                            },
                        ))),
                    })));
                    flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                },
            );
            let prop_t =
                flow_js_utils::callee_recorder::type_for_tast(reason_prop, &specialized_callee);
            (prop_t, out)
        }
    }
}

fn elem_call_opt_use<CX>(
    opt_state: OptState,
    voided_out_collector: TypeCollector,
    use_op: UseOp,
    reason_call: Reason,
    reason_lookup: Reason,
    reason_expr: Reason,
    reason_chain: Reason,
    targts: Option<Vec<Targ>>,
    argts: Vec<CallArg>,
    elem_t: Type,
    specialized_callee: Option<type_::SpecializedCallee>,
) -> type_::OptUseT<CX> {
    let opt_methodcalltype =
        type_::mk_opt_methodcalltype(None, targts.map(|v| v.into()), argts.into(), true);
    let action = match opt_state {
        OptState::AssertChain | OptState::NewChain => {
            let voided_out_collector = if opt_state == OptState::NewChain {
                Some(voided_out_collector)
            } else {
                None
            };
            type_::OptMethodAction::OptChainM(Box::new(type_::OptChainMData {
                exp_reason: reason_chain,
                lhs_reason: reason_expr,
                opt_methodcalltype,
                voided_out_collector,
                return_hint: type_::hint_unavailable(),
                specialized_callee,
            }))
        }
        _ => type_::OptMethodAction::OptCallM(Box::new(type_::OptCallMData {
            opt_methodcalltype,
            return_hint: type_::hint_unavailable(),
            specialized_callee,
        })),
    };
    type_::OptUseT::OptCallElemT(use_op, reason_call, reason_lookup, elem_t, action)
}

// **********
// * Values *
// **********

fn identifier_<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    name: &FlowSmolStr,
    loc: ALoc,
) -> Type {
    let get_checking_mode_type = || {
        let t = type_env::var_ref(
            Some(type_env::LookupMode::ForValue),
            cx,
            None,
            Name::new(name.dupe()),
            loc.dupe(),
        );
        let t = natural_inference::try_generalize(cx, syntactic_flags, &loc, t);
        let desc = desc_of_t(&t);
        let ordinary_name = Name::new(name.dupe());
        // We want to make sure that the reason description for the type we return
        // is always `RIdentifier name`.
        match desc {
            VirtualReasonDesc::RIdentifier(name_prime) if ordinary_name == *name_prime => t,
            _ if matches!(t.deref(), TypeInner::OpenT(_)) => mod_reason_of_t(
                &|r| r.replace_desc_new(VirtualReasonDesc::RIdentifier(Name::new(name.dupe()))),
                &t,
            ),
            _ => {
                let reason = mk_reason(
                    VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                    loc.dupe(),
                );
                tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason, |cx, tout| {
                    flow_js::unify_non_speculating(cx, None, &t, tout);
                })
            }
        }
    };
    if type_inference_hooks_js::dispatch_id_hook(cx, name, loc.dupe()) {
        let reason = mk_reason(VirtualReasonDesc::RAutocompleteToken, loc.dupe());
        let lazy_hint = type_env::get_hint(cx, loc.dupe());
        let result = (lazy_hint.1)(cx, false, None, reason);
        type_hint::with_hint_result(|t| t, || empty_t::at(loc.dupe()), result)
    } else {
        get_checking_mode_type()
    }
}

// let identifier cx syntactic_flags { Ast.Identifier.name; comments = _ } loc =
//   let t = identifier_ cx syntactic_flags name loc in
//   t
fn identifier_inner<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    id: &ast::Identifier<ALoc, ALoc>,
    loc: ALoc,
) -> Type {
    identifier_(cx, syntactic_flags, &id.name, loc)
}

fn string_literal_value<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    value: &FlowSmolStr,
) -> Type {
    // let { Natural_inference.as_const; frozen; _ } = syntactic_flags in
    let SyntacticFlags {
        as_const, frozen, ..
    } = syntactic_flags;
    let as_const = *as_const;
    let frozen = *frozen;
    if type_inference_hooks_js::dispatch_literal_hook(cx, loc.dupe()) {
        let lazy_hint = type_env::get_hint(cx, loc.dupe());
        let reason = mk_reason(VirtualReasonDesc::RString, loc.dupe());
        let hint = (lazy_hint.1)(cx, false, None, reason);
        type_hint::with_hint_result(|t| t, || empty_t::at(loc.dupe()), hint)
    } else if as_const || frozen == FrozenKind::FrozenProp {
        let name = Name::new(value.dupe());
        let reason = mk_annot_reason(VirtualReasonDesc::RStringLit(name.dupe()), loc);
        Type::new(TypeInner::DefT(
            reason,
            DefT::new(DefTInner::SingletonStrT {
                from_annot: true,
                value: name,
            }),
        ))
    } else {
        let precise = || {
            let name = Name::new(value.dupe());
            let reason = mk_reason(VirtualReasonDesc::RStringLit(name.dupe()), loc.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::SingletonStrT {
                    from_annot: false,
                    value: name,
                }),
            ))
        };
        let general = || {
            let reason = mk_annot_reason(VirtualReasonDesc::RString, loc.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::StrGeneralT(Literal::AnyLiteral)),
            ))
        };
        natural_inference::adjust_precision(cx, syntactic_flags, precise, general, &loc)
    }
}

fn string_literal_inner<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    lit: &ast::StringLiteral<ALoc>,
) -> Type {
    string_literal_value(cx, syntactic_flags, loc, &lit.value)
}

fn boolean_literal_inner<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    lit: &ast::BooleanLiteral<ALoc>,
) -> Type {
    // let { Natural_inference.as_const; frozen; _ } = syntactic_flags in
    let SyntacticFlags {
        as_const, frozen, ..
    } = syntactic_flags;
    let as_const = *as_const;
    let frozen = *frozen;
    let value = lit.value;
    if as_const || frozen == FrozenKind::FrozenProp {
        let reason = mk_annot_reason(VirtualReasonDesc::RBooleanLit(value), loc);
        Type::new(TypeInner::DefT(
            reason,
            DefT::new(DefTInner::SingletonBoolT {
                from_annot: true,
                value,
            }),
        ))
    } else {
        let precise = || {
            let reason = mk_reason(VirtualReasonDesc::RBooleanLit(value), loc.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::SingletonBoolT {
                    from_annot: false,
                    value,
                }),
            ))
        };
        let general = || {
            let reason = mk_annot_reason(VirtualReasonDesc::RBoolean, loc.dupe());
            Type::new(TypeInner::DefT(reason, DefT::new(DefTInner::BoolGeneralT)))
        };
        natural_inference::adjust_precision(cx, syntactic_flags, precise, general, &loc)
    }
}

pub fn null_literal(loc: ALoc) -> Type {
    type_::null::at(loc)
}

fn number_literal_inner<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    lit: &ast::NumberLiteral<ALoc>,
) -> Type {
    let value = lit.value;
    let raw = lit.raw.dupe();
    // let { Natural_inference.as_const; frozen; _ } = syntactic_flags in
    let SyntacticFlags {
        as_const, frozen, ..
    } = syntactic_flags;
    let as_const = *as_const;
    let frozen = *frozen;
    if as_const || frozen == FrozenKind::FrozenProp {
        let reason = mk_annot_reason(VirtualReasonDesc::RNumberLit(raw.dupe()), loc);
        Type::new(TypeInner::DefT(
            reason,
            DefT::new(DefTInner::SingletonNumT {
                from_annot: true,
                value: type_::NumberLiteral(value, raw),
            }),
        ))
    } else {
        let raw_c = raw.dupe();
        let loc_c = loc.dupe();
        let precise = move || {
            let reason = mk_reason(VirtualReasonDesc::RNumberLit(raw_c.dupe()), loc_c.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::SingletonNumT {
                    from_annot: false,
                    value: type_::NumberLiteral(value, raw_c.dupe()),
                }),
            ))
        };
        let loc_c2 = loc.dupe();
        let general = move || {
            let reason = mk_annot_reason(VirtualReasonDesc::RNumber, loc_c2.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::NumGeneralT(Literal::AnyLiteral)),
            ))
        };
        natural_inference::adjust_precision(cx, syntactic_flags, precise, general, &loc)
    }
}

fn bigint_literal_inner<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    lit: &ast::BigIntLiteral<ALoc>,
) -> Type {
    let value = lit.value;
    let raw = lit.raw.dupe();
    // let { Natural_inference.as_const; frozen; _ } = syntactic_flags in
    let SyntacticFlags {
        as_const, frozen, ..
    } = syntactic_flags;
    let as_const = *as_const;
    let frozen = *frozen;
    if as_const || frozen == FrozenKind::FrozenProp {
        let reason = mk_annot_reason(VirtualReasonDesc::RBigIntLit(raw.dupe()), loc);
        Type::new(TypeInner::DefT(
            reason,
            DefT::new(DefTInner::SingletonBigIntT {
                from_annot: true,
                value: type_::BigIntLiteral(value, raw),
            }),
        ))
    } else {
        let raw_c = raw.dupe();
        let loc_c = loc.dupe();
        let precise = move || {
            let reason = mk_reason(VirtualReasonDesc::RBigIntLit(raw_c.dupe()), loc_c.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::SingletonBigIntT {
                    from_annot: true,
                    value: type_::BigIntLiteral(value, raw_c.dupe()),
                }),
            ))
        };
        let loc_c2 = loc.dupe();
        let general = move || {
            let reason = mk_annot_reason(VirtualReasonDesc::RBigInt, loc_c2.dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::BigIntGeneralT(Literal::AnyLiteral)),
            ))
        };
        natural_inference::adjust_precision(cx, syntactic_flags, precise, general, &loc)
    }
}

pub fn regexp_literal<'a>(cx: &Context<'a>, loc: ALoc) -> Type {
    let reason = mk_annot_reason(VirtualReasonDesc::RRegExp, loc);
    flow_js::get_builtin_type_non_speculating(cx, &reason, None, "RegExp")
}

pub fn module_ref_literal<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    lit: &ast::ModuleRefLiteral<ALoc>,
) -> (Type, ast::ModuleRefLiteral<ALoc>) {
    let ast::ModuleRefLiteral {
        value, prefix_len, ..
    } = lit;
    let mref_str = FlowSmolStr::new(&value.as_str()[*prefix_len..]);
    let mref = flow_import_specifier::Userland::from_smol_str(mref_str);
    let module_type_or_any = flow_js_utils::import_export_utils::get_module_type_or_any(
        cx,
        false, // perform_platform_validation
        Some(type_::ImportKind::ImportValue),
        loc.dupe(),
        mref.dupe(),
    );
    let module_type_or_any = module_type_or_any.expect("Should not be under speculation");
    let reason = mk_reason(VirtualReasonDesc::RModule(mref.dupe()), loc.dupe());
    let namespace_symbol =
        flow_common::flow_symbol::Symbol::mk_module_symbol(mref.dupe().into_inner(), loc.dupe());
    let (def_loc_opt, require_t) = flow_js_utils::import_export_utils::cjs_require_type(
        cx,
        reason,
        flow_js::reposition_non_speculating,
        namespace_symbol,
        true,
        &module_type_or_any,
    )
    .expect("Should not be under speculation");
    let reason = mk_reason(VirtualReasonDesc::RModuleReference, loc.dupe());
    let t = FlowJs::get_builtin_typeapp(cx, &reason, None, "$Flow$ModuleRef", vec![require_t]);
    (
        t,
        ast::ModuleRefLiteral {
            def_loc_opt,
            ..lit.clone()
        },
    )
}

fn check_const_assertion<'a>(cx: &Context<'a>, expr: &expression::Expression<ALoc, (ALoc, Type)>) {
    use ast::expression::ExpressionInner;

    let should_error = match expr.deref() {
        ExpressionInner::StringLiteral { .. }
        | ExpressionInner::BooleanLiteral { .. }
        | ExpressionInner::NumberLiteral { .. }
        | ExpressionInner::BigIntLiteral { .. }
        | ExpressionInner::RegExpLiteral { .. }
        | ExpressionInner::Array { .. }
        | ExpressionInner::Object { .. }
        | ExpressionInner::TemplateLiteral { .. } => false,
        ExpressionInner::Unary { inner, .. } => {
            if inner.operator == expression::UnaryOperator::Minus {
                match inner.argument.deref() {
                    ExpressionInner::NumberLiteral { .. } => false,
                    _ => true,
                }
            } else {
                true
            }
        }
        ExpressionInner::Identifier { loc, .. } => {
            let (_, t) = loc;
            !natural_inference::is_generalization_candidate(cx, t)
        }
        ExpressionInner::JSXElement { .. } | ExpressionInner::JSXFragment { .. } => {
            match cx.jsx() {
                flow_common::options::JsxMode::JsxReact => false,
                flow_common::options::JsxMode::JsxPragma { .. } => true,
            }
        }
        _ => true,
    };

    if should_error {
        let loc = expr.loc();
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EUnsupportedSyntax(Box::new((
                loc.0.dupe(),
                UnsupportedSyntax::AsConstOnNonLiteral,
            ))),
        );
    }
}

// *********
// * Types *
// *********

pub fn opaque_type<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    otype: &statement::OpaqueType<ALoc, ALoc>,
) -> (Type, statement::OpaqueType<ALoc, (ALoc, Type)>) {
    let name_loc = otype.id.loc.dupe();
    let id = &otype.id;
    let name = &id.name;
    let cache = cx.node_cache();
    match cache.get_opaque(&loc) {
        Some(info) => {
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Opaque type cache hit at {}",
                    loc.debug_to_string(false)
                )]
            });
            info
        }
        None => {
            let r = type_::desc_format::type_reason(Name::new(name.dupe()), name_loc.dupe());
            let (tparams, tparams_map, tparams_ast) = type_annotation::mk_type_param_declarations(
                cx,
                flow_parser::ast_visitor::TypeParamsContext::OpaqueType,
                Some(FlowOrdMap::new()),
                otype.tparams.as_ref(),
            );
            let (underlying_t, impl_type_ast) =
                type_annotation::convert_opt(cx, &tparams_map, otype.impl_type.as_ref());
            if !cx.opaque_type_new_bound_syntax() {
                if let Some(lb) = &otype.lower_bound {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            lb.loc().dupe(),
                            UnsupportedSyntax::OpaqueTypeSuperBound,
                        ))),
                    );
                }
                if let Some(ub) = &otype.upper_bound {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            ub.loc().dupe(),
                            UnsupportedSyntax::OpaqueTypeExtendsBound,
                        ))),
                    );
                }
            }
            let (lower_bound_t, lower_bound_ast) =
                type_annotation::convert_opt(cx, &tparams_map, otype.lower_bound.as_ref());
            let (upper_bound_t, upper_bound_ast) =
                type_annotation::convert_opt(cx, &tparams_map, otype.upper_bound.as_ref());
            let (legacy_upper_bound_t, legacy_upper_bound_ast) =
                type_annotation::convert_opt(cx, &tparams_map, otype.legacy_upper_bound.as_ref());
            let upper_bound_t = upper_bound_t.or(legacy_upper_bound_t);
            if let Some((_tparams_loc, tps)) = &tparams {
                let tparams_map_for_check: BTreeMap<SubstName, TypeParam> =
                    tps.iter().map(|tp| (tp.name.dupe(), tp.dupe())).collect();
                if let Some(ref ut) = underlying_t {
                    cx.add_post_inference_polarity_check(
                        tparams_map_for_check.clone(),
                        Polarity::Positive,
                        ut.dupe(),
                    );
                }
                if let Some(ref lt) = lower_bound_t {
                    cx.add_post_inference_polarity_check(
                        tparams_map_for_check.clone(),
                        Polarity::Positive,
                        lt.dupe(),
                    );
                }
                if let Some(ref ubt) = upper_bound_t {
                    cx.add_post_inference_polarity_check(
                        tparams_map_for_check,
                        Polarity::Positive,
                        ubt.dupe(),
                    );
                }
            }
            let nominal_type_args: Vec<(SubstName, Reason, Type, Polarity)> =
                type_params::to_list(&tparams)
                    .into_iter()
                    .map(|tp| {
                        let t = tparams_map
                            .get(&tp.name)
                            .expect("tparam must be in map")
                            .dupe();
                        (tp.name.dupe(), tp.reason.dupe(), t, tp.polarity)
                    })
                    .collect();
            let nominal_id = nominal::Id::UserDefinedOpaqueTypeId(Box::new(
                nominal::UserDefinedOpaqueTypeIdData(cx.make_aloc_id(&name_loc), name.dupe()),
            ));
            let nominal_type = NominalType::new(NominalTypeInner {
                underlying_t: match &underlying_t {
                    None => nominal::UnderlyingT::FullyOpaque,
                    Some(t) => nominal::UnderlyingT::OpaqueWithLocal { t: t.dupe() },
                },
                lower_t: lower_bound_t.dupe(),
                upper_t: upper_bound_t.dupe(),
                nominal_id,
                nominal_type_args: nominal_type_args.into(),
            });
            let nominal_t = Type::new(TypeInner::NominalT {
                reason: mk_reason(VirtualReasonDesc::ROpaqueType(name.dupe()), name_loc.dupe()),
                nominal_type: Rc::new(nominal_type),
            });
            let type_ = poly_type_of_tparams(
                poly::Id::generate_id(),
                tparams,
                Type::new(TypeInner::DefT(
                    r,
                    DefT::new(DefTInner::TypeT(TypeTKind::OpaqueKind, nominal_t)),
                )),
            );
            if let (Some(l), Some(u)) = (&lower_bound_t, &underlying_t) {
                cx.add_post_inference_subtyping_check(l.dupe(), unknown_use(), u.dupe());
            }
            if let (Some(l), Some(u)) = (&underlying_t, &upper_bound_t) {
                cx.add_post_inference_subtyping_check(l.dupe(), unknown_use(), u.dupe());
            }
            if let (Some(l), Some(u)) = (&lower_bound_t, &upper_bound_t) {
                cx.add_post_inference_subtyping_check(l.dupe(), unknown_use(), u.dupe());
            }
            let opaque_type_ast = statement::OpaqueType {
                id: ast::Identifier::new(ast::IdentifierInner {
                    loc: (name_loc.dupe(), type_.dupe()),
                    name: id.name.dupe(),
                    comments: id.comments.dupe(),
                }),
                tparams: tparams_ast,
                impl_type: impl_type_ast,
                lower_bound: lower_bound_ast,
                upper_bound: upper_bound_ast,
                legacy_upper_bound: legacy_upper_bound_ast,
                comments: otype.comments.dupe(),
            };
            (type_, opaque_type_ast)
        }
    }
}

// *****************
// * Import/Export *
// *****************

fn export_specifiers<'a>(
    cx: &Context<'a>,
    source: Option<(
        &Result<type_::ModuleType, Type>,
        ALoc,
        &ast::StringLiteral<ALoc>,
    )>,
    export_kind: statement::ExportKind,
    specifier: &statement::export_named_declaration::Specifier<ALoc, ALoc>,
) -> statement::export_named_declaration::Specifier<ALoc, (ALoc, Type)> {
    let effective_kind = |specifier_export_kind: statement::ExportKind| -> statement::ExportKind {
        flow_parser::ast_utils::effective_export_kind(export_kind, specifier_export_kind)
    };
    // [declare] export [type] {[type] foo [as bar]};
    let export_ref =
        |kind: statement::ExportKind, loc: ALoc, local_name: &Name| -> (Option<ALoc>, Type) {
            match kind {
                statement::ExportKind::ExportType => {
                    let t = type_env::var_ref(
                        Some(type_env::LookupMode::ForType),
                        cx,
                        None,
                        local_name.dupe(),
                        loc,
                    );
                    (
                        None,
                        type_operation_utils::type_assertions::assert_export_is_type(
                            cx, local_name, &t,
                        ),
                    )
                }
                statement::ExportKind::ExportValue if flow_common::files::has_ts_ext(cx.file()) => {
                    // In .ts files, check if this is a local type binding or an imported type-only
                    let binding_info = type_env::local_export_binding_at_loc(cx, loc.dupe());
                    match binding_info {
                        Some(type_env::LocalExportBinding {
                            val_kind: flow_env_builder::env_api::ValKind::Type { .. },
                            ..
                        }) => {
                            // Local type binding: use ForType lookup and route as type export
                            let t = type_env::var_ref(
                                Some(type_env::LookupMode::ForType),
                                cx,
                                None,
                                local_name.dupe(),
                                loc,
                            );
                            (
                                None,
                                type_operation_utils::type_assertions::assert_export_is_type(
                                    cx, local_name, &t,
                                ),
                            )
                        }
                        Some(type_env::LocalExportBinding {
                            def_loc: Some(def_loc),
                            val_kind: flow_env_builder::env_api::ValKind::TsImport,
                        }) => {
                            if flow_js_utils::import_export_utils::is_ts_import_type_only(
                                cx, &def_loc,
                            ) {
                                let t = type_env::var_ref(
                                    Some(type_env::LookupMode::ForType),
                                    cx,
                                    None,
                                    local_name.dupe(),
                                    loc,
                                );
                                (
                                    None,
                                    type_operation_utils::type_assertions::assert_export_is_type(
                                        cx, local_name, &t,
                                    ),
                                )
                            } else {
                                let t = type_env::var_ref(
                                    Some(type_env::LookupMode::ForValue),
                                    cx,
                                    None,
                                    local_name.dupe(),
                                    loc,
                                );
                                (None, t)
                            }
                        }
                        _ => {
                            let t = type_env::var_ref(
                                Some(type_env::LookupMode::ForValue),
                                cx,
                                None,
                                local_name.dupe(),
                                loc,
                            );
                            (None, t)
                        }
                    }
                }
                statement::ExportKind::ExportValue => {
                    let t = type_env::var_ref(
                        Some(type_env::LookupMode::ForValue),
                        cx,
                        None,
                        local_name.dupe(),
                        loc,
                    );
                    (None, t)
                }
            }
        };
    // [declare] export [type] {[type] foo [as bar]} from 'module'
    let export_from = |kind: statement::ExportKind,
                       module_name: flow_import_specifier::Userland,
                       source_module: &Result<type_::ModuleType, Type>,
                       loc: ALoc,
                       local_name: &Name|
     -> (Option<ALoc>, Type) {
        let reason = mk_reason(VirtualReasonDesc::RIdentifier(local_name.dupe()), loc);
        let import_kind = match kind {
            statement::ExportKind::ExportType => statement::ImportKind::ImportType,
            statement::ExportKind::ExportValue => statement::ImportKind::ImportValue,
        };
        flow_js_utils::import_export_utils::import_named_specifier_type(
            cx,
            reason,
            &|cx, reason, t| {
                FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                    .map_err(Into::into)
            },
            &import_kind,
            module_name.dupe(),
            source_module,
            local_name.as_smol_str(),
            local_name.as_smol_str(),
        )
        .expect("Should not be under speculation")
    };
    let export_specifier =
        |make_export: &dyn Fn(statement::ExportKind, ALoc, &Name) -> (Option<ALoc>, Type),
         spec: &statement::export_named_declaration::ExportSpecifier<ALoc, ALoc>|
         -> statement::export_named_declaration::ExportSpecifier<ALoc, (ALoc, Type)> {
            let specifier_export_kind = spec.export_kind;
            let kind = effective_kind(specifier_export_kind);
            let local = &spec.local;
            let local_loc = local.loc.dupe();
            let local_name = Name::new(local.name.dupe());
            let reconstruct_remote = |t: &Type| {
                spec.exported.as_ref().map(|remote| {
                    ast::Identifier::new(ast::IdentifierInner {
                        loc: (remote.loc.dupe(), t.dupe()),
                        name: remote.name.dupe(),
                        comments: remote.comments.dupe(),
                    })
                })
            };
            let (imported_name_def_loc, t) = make_export(kind, local_loc.dupe(), &local_name);
            statement::export_named_declaration::ExportSpecifier {
                loc: spec.loc.dupe(),
                local: ast::Identifier::new(ast::IdentifierInner {
                    loc: (local_loc, t.dupe()),
                    name: local.name.dupe(),
                    comments: local.comments.dupe(),
                }),
                exported: reconstruct_remote(&t),
                export_kind: specifier_export_kind,
                from_remote: spec.from_remote,
                imported_name_def_loc,
            }
        };
    // function
    match specifier {
        // [declare] export [type] {[type] foo [as bar]} [from ...];
        statement::export_named_declaration::Specifier::ExportSpecifiers(specifiers) => {
            let specifiers: Vec<_> = match source {
                Some((source_module, _, source_literal)) => {
                    let module_name =
                        flow_import_specifier::Userland::from_smol_str(source_literal.value.dupe());
                    specifiers
                        .iter()
                        .map(|spec| {
                            export_specifier(
                                &|kind, loc, local_name| {
                                    export_from(
                                        kind,
                                        module_name.dupe(),
                                        source_module,
                                        loc,
                                        local_name,
                                    )
                                },
                                spec,
                            )
                        })
                        .collect()
                }
                None => specifiers
                    .iter()
                    .map(|spec| export_specifier(&export_ref, spec))
                    .collect(),
            };
            statement::export_named_declaration::Specifier::ExportSpecifiers(specifiers)
        }
        statement::export_named_declaration::Specifier::ExportBatchSpecifier(batch) => {
            match &batch.specifier {
                // [declare] export [type] * as id from "source";
                Some(id) => {
                    let (source_module, _, _) =
                        source.expect("source must be present for ExportBatchSpecifier with id");
                    let id_loc = id.loc.dupe();
                    let name = &id.name;
                    //   let reason = mk_reason (RIdentifier (OrdinaryName name)) id_loc in
                    let reason = mk_reason(
                        VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                        id_loc.dupe(),
                    );
                    let ns_t = flow_js_utils::import_export_utils::get_module_namespace_type(
                        cx,
                        reason,
                        flow_common::flow_symbol::Symbol::mk_constant_symbol(
                            name.dupe(),
                            id_loc.dupe(),
                        ),
                        source_module,
                    )
                    .expect("Should not be under speculation");
                    statement::export_named_declaration::Specifier::ExportBatchSpecifier(
                        statement::export_named_declaration::ExportBatchSpecifier {
                            loc: batch.loc.dupe(),
                            specifier: Some(ast::Identifier::new(ast::IdentifierInner {
                                loc: (id_loc, ns_t),
                                name: name.dupe(),
                                comments: id.comments.dupe(),
                            })),
                        },
                    )
                }
                // [declare] export [type] * from "source";
                None => statement::export_named_declaration::Specifier::ExportBatchSpecifier(
                    statement::export_named_declaration::ExportBatchSpecifier {
                        loc: batch.loc.dupe(),
                        specifier: None,
                    },
                ),
            }
        }
    }
}

fn hook_check<'a>(
    cx: &Context<'a>,
    effect: ast::function::Effect,
    id: &ast::Identifier<ALoc, ALoc>,
) {
    if effect == ast::function::Effect::Hook && !flow_parser::ast_utils::hook_name(&id.name) {
        flow_js::add_output_non_speculating(cx, ErrorMessage::EHookNaming(id.loc.dupe()));
    }
}

// ************
// * Visitors *
// ************

// ***************************************************************
// * local inference pass: visit AST statement list, calling
// * flow to check types/create graphs for merge-time checking
// ***************************************************************

pub fn statement<'a>(
    cx: &Context<'a>,
    stmt: &statement::Statement<ALoc, ALoc>,
) -> Result<statement::Statement<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let node_cache = cx.node_cache();
    let loc = stmt.loc();
    match node_cache.get_statement(loc) {
        Some(node) => {
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Statement cache hit at {}",
                    loc.debug_to_string(false)
                )]
            });
            Ok(node)
        }
        None => statement_(cx, stmt),
    }
}

#[rustfmt::skip]
fn statement_<'a>(
    cx: &Context<'a>,
    stmt: &statement::Statement<ALoc, ALoc>,
) -> Result<statement::Statement<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    use ast::statement::StatementInner;

    let variables = |cx: &Context<'a>,
                     decls: &statement::VariableDeclaration<ALoc, ALoc>|
     -> Result<
        statement::VariableDeclaration<ALoc, (ALoc, Type)>,
        AbnormalControlFlow,
    > {
        let kind = decls.kind;
        let declarations: Vec<_> = decls
            .declarations
            .iter()
            .map(|decl| {
                if kind == ast::VariableKind::Const && decl.init.is_none() && !cx.tslib_syntax() {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            decl.loc.dupe(),
                            UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::DeclarationWithoutDeclare,
                            ),
                        ))),
                    );
                }
                let (id, init) = variable(cx, kind, None, &decl.id, decl.init.as_ref())?;
                Ok(statement::variable::Declarator {
                    loc: decl.loc.dupe(),
                    id,
                    init,
                })
            })
            .collect::<Result<Vec<_>, AbnormalControlFlow>>()?;
        Ok(statement::VariableDeclaration {
            declarations: declarations.into(),
            kind,
            comments: decls.comments.dupe(),
        })
    };

    let catch_clause = |cx: &Context<'a>,
                        cc: &statement::try_::CatchClause<ALoc, ALoc>|
     -> statement::try_::CatchClause<ALoc, (ALoc, Type)> {
        let statement::try_::CatchClause {
            loc: _cc_loc,
            param,
            body,
            comments: cc_comments,
        } = cc;
        let (b_loc, b) = body;
        match param {
            Some(p) => {
                match p {
                    ast::pattern::Pattern::Identifier { loc, inner } => {
                        let loc = loc.dupe();
                        let name_loc = inner.name.loc.dupe();
                        let id_ident = &inner.name;
                        let optional = inner.optional;
                        let (t, ast_annot) = match &inner.annot {
                            ast::types::AnnotationOrHint::Missing(mloc) => {
                                let t = if cx.use_unknown_in_catch_variables() {
                                    mixed_t::at(loc.dupe())
                                } else {
                                    any_t::why(
                                        type_::AnySource::CatchAny,
                                        mk_reason(
                                            VirtualReasonDesc::RAnyImplicit,
                                            loc.dupe(),
                                        ),
                                    )
                                };
                                (
                                    t.dupe(),
                                    ast::types::AnnotationOrHint::Missing((mloc.dupe(), t)),
                                )
                            }
                            ast::types::AnnotationOrHint::Available(annot) => {
                                // Check if it's Any, Mixed, or Unknown type annotation
                                let is_valid_catch_annot = match &*annot.annotation {
                                    ast::types::TypeInner::Any { .. }
                                    | ast::types::TypeInner::Mixed { .. }
                                    | ast::types::TypeInner::Unknown { .. } => true,
                                    _ => false,
                                };
                                if is_valid_catch_annot {
                                    // Not relevant with our limited accepted annotations.
                                    let tparams_map = FlowOrdMap::new();
                                    let (t, ast_annot) =
                                        type_annotation::mk_type_available_annotation(
                                            cx,
                                            tparams_map,
                                            annot,
                                        );
                                    (t, ast::types::AnnotationOrHint::Available(ast_annot))
                                } else {
                                    let annot_loc = annot.annotation.loc().dupe();
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EInvalidCatchParameterAnnotation {
                                            loc: annot_loc.dupe(),
                                            ts_utility_syntax: cx.ts_utility_syntax(),
                                        },
                                    );
                                    let t = any_t::why(
                                        type_::AnySource::CatchAny,
                                        mk_reason(
                                            VirtualReasonDesc::RAnyImplicit,
                                            annot_loc,
                                        ),
                                    );
                                    let Ok(ast_annot) =
                                        polymorphic_ast_mapper::type_annotation_hint(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &inner.annot,
                                        );
                                    (t, ast_annot)
                                }
                            }
                        };
                        let body = statement_list(cx, &b.body);
                        statement::try_::CatchClause {
                            loc: cc.loc.dupe(),
                            param: Some(ast::pattern::Pattern::Identifier {
                                loc: (loc.dupe(), t.dupe()),
                                inner: (ast::pattern::Identifier {
                                    name: ast::Identifier::new(ast::IdentifierInner {
                                        loc: (name_loc, t),
                                        name: id_ident.name.dupe(),
                                        comments: id_ident.comments.dupe(),
                                    }),
                                    annot: ast_annot,
                                    optional,
                                })
                                .into(),
                            }),
                            body: (
                                b_loc.dupe(),
                                statement::Block {
                                    body: body.into(),
                                    comments: b.comments.dupe(),
                                },
                            ),
                            comments: cc_comments.clone(),
                        }
                    }
                    _ => {
                        let loc = p.loc();
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                loc.dupe(),
                                UnsupportedSyntax::CatchParameterDeclaration,
                            ))),
                        );
                        let mut mapper = typed_ast_utils::ErrorMapper;
                        statement::try_::CatchClause {
                            loc: cc.loc.dupe(),
                            param: cc.param.as_ref().map(|p| {
                                let Ok(v) =
                                    polymorphic_ast_mapper::assignment_pattern(&mut mapper, p);
                                v
                            }),
                            body: {
                                let (bloc, blk) = &cc.body;
                                let Ok(blk) = polymorphic_ast_mapper::block(&mut mapper, blk);
                                (bloc.dupe(), blk)
                            },
                            comments: {
                                let Ok(v) = polymorphic_ast_mapper::syntax_opt(
                                    &mut mapper,
                                    cc.comments.as_ref(),
                                );
                                v
                            },
                        }
                    }
                }
            }
            None => {
                let body = statement_list(cx, &b.body);
                statement::try_::CatchClause {
                    loc: cc.loc.dupe(),
                    param: None,
                    body: (
                        b_loc.dupe(),
                        statement::Block {
                            body: body.into(),
                            comments: b.comments.dupe(),
                        },
                    ),
                    comments: cc_comments.clone(),
                }
            }
        }
    };

    let function_ = |cx: &Context<'a>,
                     is_declared_function: bool,
                     loc: ALoc,
                     func: &ast::function::Function<ALoc, ALoc>|
     -> Result<
        (
            Type,
            ast::Identifier<ALoc, ALoc>,
            statement::Statement<ALoc, (ALoc, Type)>,
        ),
        AbnormalControlFlow,
    > {
        let id = func
            .id
            .as_ref()
            .expect("unexpected anonymous function statement");
        let sig_loc = func.sig_loc.dupe();
        let async_ = func.async_;
        let generator = func.generator;
        let ast::IdentifierInner {
            loc: name_loc,
            name,
            ..
        } = &**id;
        let reason = func_reason(async_, generator, sig_loc);
        let tast_fun_type = type_env::get_var_declared_type(
            Some(type_env::LookupMode::ForValue),
            Some(is_declared_function),
            cx,
            Name::new(name.dupe()),
            name_loc.dupe(),
        );
        let (fn_type, func_ast) =
            mk_function_declaration(cx, Some(tast_fun_type), reason, loc.dupe(), func)?;
        Ok((
            fn_type,
            id.dupe(),
            statement::Statement::new(StatementInner::FunctionDeclaration {
                loc,
                inner: func_ast.into(),
            }),
        ))
    };

    let declare_function_inner = |cx: &Context<'a>,
                                  loc: ALoc,
                                  f: &statement::DeclareFunction<ALoc, ALoc>|
     -> statement::DeclareFunction<ALoc, (ALoc, Type)> {
        // let { id = (id_loc, id_name); annot; predicate; comments; implicit_declare } = f in
        let statement::DeclareFunction {
            id: id_name,
            annot,
            predicate,
            comments: _decl_comments,
            implicit_declare,
        } = f;
        let effect_ = match &*annot.annotation {
            ast::types::TypeInner::Function { inner, .. } => inner.effect,
            _ => ast::function::Effect::Arbitrary,
        };
        let (_, annot_ast) =
            type_annotation::mk_type_available_annotation(cx, FlowOrdMap::new(), annot);
        let predicate = predicate.as_ref().map(|p| {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    p.loc.dupe(),
                    UnsupportedSyntax::PredicateFunction,
                ))),
            );
            let Ok(v) = polymorphic_ast_mapper::predicate(&mut typed_ast_utils::ErrorMapper, p);
            v
        });
        if *implicit_declare && !cx.tslib_syntax() {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::DeclarationWithoutDeclare),
                ))),
            );
        }
        match id_name {
            Some(id_name) => {
                hook_check(cx, effect_, id_name);
                let t = type_env::get_var_declared_type(
                    Some(type_env::LookupMode::ForValue),
                    Some(true),
                    cx,
                    Name::new(id_name.name.dupe()),
                    id_name.loc.dupe(),
                );
                statement::DeclareFunction {
                    id: Some(ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_name.loc.dupe(), t),
                        name: id_name.name.dupe(),
                        comments: id_name.comments.dupe(),
                    })),
                    annot: annot_ast,
                    predicate,
                    comments: _decl_comments.clone(),
                    implicit_declare: *implicit_declare,
                }
            }
            None => statement::DeclareFunction {
                id: None,
                annot: annot_ast,
                predicate,
                comments: _decl_comments.clone(),
                implicit_declare: *implicit_declare,
            },
        }
    };

    let loc = stmt.loc().dupe();
    Ok(match stmt.deref() {
        StatementInner::Empty { loc: eloc, inner } => {
            statement::Statement::new(StatementInner::Empty {
                loc: eloc.dupe(),
                inner: inner.clone(),
            })
        }
        StatementInner::Block { inner, .. } => {
            let body = statement_list(cx, &inner.body);
            statement::Statement::new(StatementInner::Block {
                loc,
                inner: (statement::Block {
                    body: body.into(),
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::Expression { inner, .. } => {
            let expr = expression(None, None, None, cx, &inner.expression)?;
            for unhandled in syntactically_unhandled_promises(&expr) {
                let (_, expr_t) = unhandled.loc();
                cx.mark_maybe_unused_promise(
                    loc.dupe(),
                    expr_t.dupe(),
                    type_env::in_async_scope(cx),
                );
            }
            statement::Statement::new(StatementInner::Expression {
                loc,
                inner: (statement::Expression {
                    expression: expr,
                    directive: inner.directive.clone(),
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::If { inner, .. } => {
            let test_ast = condition(
                cx,
                EnclosingContext::OtherTestContext,
                None,
                None,
                &inner.test,
            )?;
            let then_ast = statement(cx, &inner.consequent)?;
            let else_ast = inner
                .alternate
                .as_ref()
                .map(|alt| {
                    Ok(statement::if_::Alternate {
                        loc: alt.loc.dupe(),
                        body: statement(cx, &alt.body)?,
                        comments: alt.comments.dupe(),
                    })
                })
                .transpose()?;
            statement::Statement::new(StatementInner::If {
                loc,
                inner: (statement::If {
                    test: test_ast,
                    consequent: then_ast,
                    alternate: else_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::Labeled { inner, .. } => {
            statement::Statement::new(StatementInner::Labeled {
                loc,
                inner: (statement::Labeled {
                    label: inner.label.dupe(),
                    body: statement(cx, &inner.body)?,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::Break { inner, .. } => statement::Statement::new(StatementInner::Break {
            loc,
            inner: inner.clone(),
        }),
        StatementInner::Continue { inner, .. } => {
            statement::Statement::new(StatementInner::Continue {
                loc,
                inner: inner.clone(),
            })
        }
        StatementInner::With { .. } => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((loc.dupe(), UnsupportedSyntax::WithStatement))),
            );
            let Ok(v) = polymorphic_ast_mapper::statement(&mut typed_ast_utils::ErrorMapper, stmt);
            v
        }
        StatementInner::DeclareTypeAlias { inner, .. } => {
            let (_, type_alias_ast) = type_alias(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareTypeAlias {
                loc,
                inner: type_alias_ast.into(),
            })
        }
        StatementInner::TypeAlias { inner, .. } => {
            let (_, type_alias_ast) = type_alias(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::TypeAlias {
                loc,
                inner: type_alias_ast.into(),
            })
        }
        StatementInner::DeclareOpaqueType { inner, .. } => {
            let (_, opaque_type_ast) = opaque_type(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareOpaqueType {
                loc,
                inner: opaque_type_ast.into(),
            })
        }
        StatementInner::OpaqueType { inner, .. } => {
            let (_, opaque_type_ast) = opaque_type(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::OpaqueType {
                loc,
                inner: opaque_type_ast.into(),
            })
        }
        StatementInner::Match { inner, .. } => {
            if !cx.enable_pattern_matching() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((loc.dupe(), UnsupportedSyntax::MatchStatement))),
                );
                return Ok({
                    let Ok(v) =
                        polymorphic_ast_mapper::statement(&mut typed_ast_utils::ErrorMapper, stmt);
                    v
                });
            }
            let arg = expression(None, None, Some(true), cx, &inner.arg)?;
            let (_, arg_t) = arg.loc();
            type_env::init_const(
                cx,
                &type_::unknown_use(),
                arg_t,
                inner.match_keyword_loc.dupe(),
            );
            let mut cases = Vec::new();
            let mut invalid_syntax_list = Vec::new();
            for case in inner.cases.iter() {
                let pattern = match_pattern(
                    cx,
                    case.case_match_root_loc.dupe(),
                    case.guard.is_some(),
                    &case.pattern,
                )?;
                let guard = case
                    .guard
                    .as_ref()
                    .map(|g| expression(None, None, None, cx, g))
                    .transpose()?;
                if !matches!(&*case.body, StatementInner::Block { .. }) {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EMatchError(MatchErrorKind::MatchStatementInvalidBody {
                            loc: case.body.loc().dupe(),
                        }),
                    );
                }
                let body = statement(cx, &case.body)?;
                cases.push(ast::match_::Case {
                    loc: case.loc.dupe(),
                    pattern,
                    body,
                    guard,
                    comments: case.comments.dupe(),
                    invalid_syntax: case.invalid_syntax.clone(),
                    case_match_root_loc: case.case_match_root_loc.dupe(),
                });
                invalid_syntax_list.push(case.invalid_syntax.clone());
            }
            error_on_match_case_invalid_syntax(
                cx,
                inner.match_keyword_loc.dupe(),
                &invalid_syntax_list,
            );
            type_env::var_ref(
                Some(type_env::LookupMode::ForValue),
                cx,
                None,
                Name::new(flow_parser::ast_utils::MATCH_ROOT_NAME),
                inner.match_keyword_loc.dupe(),
            );
            statement::Statement::new(StatementInner::Match {
                loc,
                inner: (statement::MatchStatement {
                    arg,
                    cases: cases.into(),
                    match_keyword_loc: inner.match_keyword_loc.dupe(),
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::Switch { inner, .. } => {
            let discriminant_ast = expression(None, None, None, cx, &inner.discriminant)?;
            let exhaustive_check_incomplete_out = flow_typing_tvar::mk(
                cx,
                mk_reason(
                    VirtualReasonDesc::RCustom(
                        "exhaustive check incomplete out".into(),
                    ),
                    loc.dupe(),
                ),
            );
            if cx.enable_pattern_matching()
                && crate::switch_to_match::convert_switch(&ALoc::none(), loc.dupe(), inner)
                    .is_some()
            {
                cx.add_switch_to_match_eligible_location(loc.dupe());
            }
            let mut cases_ast = Vec::new();
            let mut has_default = false;
            for case in inner.cases.iter() {
                let test_ast = match (&case.test, &case.case_test_loc) {
                    (_, None) | (None, _) => None,
                    (Some(expr), Some(case_test_loc)) => {
                        let fake_discriminant = match &*inner.discriminant {
                            expression::ExpressionInner::Member {
                                loc: mem_loc,
                                inner: mem,
                            } if refinement::keys::key(true, &mem.object).is_some() => {
                                // _object = (_, x) => replace loc with case loc: (loc, x)
                                let mut new_object_inner = (*mem.object).clone();
                                *new_object_inner.loc_mut() = case.loc.dupe();
                                let new_object = expression::Expression::new(new_object_inner);
                                expression::Expression::new(expression::ExpressionInner::Member {
                                    loc: mem_loc.dupe(),
                                    inner: std::sync::Arc::new(expression::Member {
                                        object: new_object,
                                        property: mem.property.clone(),
                                        comments: mem.comments.dupe(),
                                    }),
                                })
                            }
                            _ => inner.discriminant.dupe(),
                        };
                        let fake =
                            expression::Expression::new(expression::ExpressionInner::Binary {
                                loc: case_test_loc.dupe(),
                                inner: std::sync::Arc::new(expression::Binary {
                                    operator: expression::BinaryOperator::StrictEqual,
                                    left: fake_discriminant,
                                    right: expr.dupe(),
                                    comments: None,
                                }),
                            });
                        let fake_ast = condition(
                            cx,
                            EnclosingContext::SwitchTestContext {
                                case_test_loc: expr.loc().dupe(),
                                switch_discriminant_loc: inner.discriminant.loc().dupe(),
                            },
                            None,
                            None,
                            &fake,
                        )?;
                        let expr_ast = match &*fake_ast {
                            expression::ExpressionInner::Binary { inner: bin, .. } => {
                                bin.right.dupe()
                            }
                            _ => panic!("expected Binary expression from condition"),
                        };
                        Some(expr_ast)
                    }
                };
                let consequent_ast = statement_list(cx, &case.consequent);
                has_default = has_default || case.test.is_none();
                cases_ast.push(statement::switch::Case {
                    loc: case.loc.dupe(),
                    test: test_ast,
                    case_test_loc: case.case_test_loc.clone(),
                    consequent: consequent_ast.into(),
                    comments: case.comments.dupe(),
                });
            }
            let enum_exhaustive_check = enum_exhaustive_check_of_switch_cases(&cases_ast);
            let (_, discriminant_t) = discriminant_ast.loc();
            let discriminant_after_check = if !has_default {
                let refinement_key = refinement::keys::key(true, &inner.discriminant);
                type_env::discriminant_after_negated_cases(cx, loc.dupe(), refinement_key.as_ref())
            } else {
                None
            };
            let check_reason = reason_of_t(discriminant_t);
            match enum_exhaustive_check {
                EnumPossibleExhaustiveCheckT::EnumExhaustiveCheckPossiblyValid(box EnumExhaustiveCheckPossiblyValidData {
                    possible_checks,
                    checks,
                    default_case_loc,
                }) => {
                    check_possible_enum_exhaustive_check(
                        cx,
                        check_reason,
                        &possible_checks,
                        &checks,
                        default_case_loc,
                        &exhaustive_check_incomplete_out,
                        discriminant_after_check.as_ref(),
                        discriminant_t,
                    )
                    .unwrap();
                }
                EnumPossibleExhaustiveCheckT::EnumExhaustiveCheckInvalid(reasons) => {
                    check_invalid_enum_exhaustive_check(
                        cx,
                        check_reason,
                        &reasons,
                        &exhaustive_check_incomplete_out,
                        discriminant_after_check.as_ref(),
                        discriminant_t,
                    )
                    .unwrap();
                }
            };
            // We need to fully resolve all types attached to AST,
            // because the post inference pass might inspect them.
            tvar_resolver::resolve(
                cx,
                tvar_resolver::default_no_lowers,
                true,
                &exhaustive_check_incomplete_out,
            );
            statement::Statement::new(StatementInner::Switch {
                loc,
                inner: statement::Switch {
                    discriminant: discriminant_ast,
                    cases: cases_ast.into(),
                    comments: inner.comments.dupe(),
                    exhaustive_out: (inner.exhaustive_out.dupe(), exhaustive_check_incomplete_out),
                }
                .into(),
            })
        }
        StatementInner::Return { inner, .. } => {
            let (t, argument_ast) = match &inner.argument {
                None => (type_::void::at(loc.dupe()), None),
                Some(expr) => {
                    let ast = expression(None, None, None, cx, expr)?;
                    let (_, t) = ast.loc();
                    (t.dupe(), Some(ast))
                }
            };
            statement::Statement::new(StatementInner::Return {
                loc,
                inner: statement::Return {
                    argument: argument_ast,
                    comments: inner.comments.dupe(),
                    return_out: (inner.return_out.dupe(), t),
                }
                .into(),
            })
        }
        StatementInner::Throw { inner, .. } => statement::Statement::new(StatementInner::Throw {
            loc,
            inner: (statement::Throw {
                argument: expression(None, None, None, cx, &inner.argument)?,
                comments: inner.comments.dupe(),
            })
            .into(),
        }),
        StatementInner::Try { inner, .. } => {
            let (b_loc, b) = &inner.block;
            let try_block_ast = statement_list(cx, &b.body);
            let catch_ast = inner.handler.as_ref().map(|h| catch_clause(cx, h));
            let finally_ast = inner.finalizer.as_ref().map(|(f_loc, f)| {
                (
                    f_loc.dupe(),
                    statement::Block {
                        body: statement_list(cx, &f.body).into(),
                        comments: f.comments.dupe(),
                    },
                )
            });
            statement::Statement::new(StatementInner::Try {
                loc,
                inner: statement::Try {
                    block: (
                        b_loc.dupe(),
                        statement::Block {
                            body: try_block_ast.into(),
                            comments: b.comments.dupe(),
                        },
                    ),
                    handler: catch_ast,
                    finalizer: finally_ast,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        StatementInner::While { inner, .. } => {
            let test_ast = condition(
                cx,
                EnclosingContext::OtherTestContext,
                None,
                None,
                &inner.test,
            )?;
            let body_ast = statement(cx, &inner.body)?;
            statement::Statement::new(StatementInner::While {
                loc,
                inner: (statement::While {
                    test: test_ast,
                    body: body_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::DoWhile { inner, .. } => {
            let body_ast = statement(cx, &inner.body)?;
            let test_ast = condition(
                cx,
                EnclosingContext::OtherTestContext,
                None,
                None,
                &inner.test,
            )?;
            statement::Statement::new(StatementInner::DoWhile {
                loc,
                inner: (statement::DoWhile {
                    body: body_ast,
                    test: test_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::For { inner, .. } => {
            let init_ast = inner
                .init
                .as_ref()
                .map(|init| {
                    Ok(match init {
                        statement::for_::Init::InitDeclaration((decl_loc, decl)) => {
                            statement::for_::Init::InitDeclaration((
                                decl_loc.dupe(),
                                variables(cx, decl)?,
                            ))
                        }
                        statement::for_::Init::InitExpression(expr) => {
                            statement::for_::Init::InitExpression(expression(
                                None, None, None, cx, expr,
                            )?)
                        }
                    })
                })
                .transpose()?;
            let test_ast = inner
                .test
                .as_ref()
                .map(|expr| condition(cx, EnclosingContext::OtherTestContext, None, None, expr))
                .transpose()?;
            let body_ast = statement(cx, &inner.body)?;
            let update_ast = inner
                .update
                .as_ref()
                .map(|expr| expression(None, None, None, cx, expr))
                .transpose()?;
            statement::Statement::new(StatementInner::For {
                loc,
                inner: statement::For {
                    init: init_ast,
                    test: test_ast,
                    update: update_ast,
                    body: body_ast,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        StatementInner::ForIn { inner, .. } => {
            let eval_right = || expression(None, None, None, cx, &inner.right);
            let (left_ast, right_ast) = match &inner.left {
                statement::for_in::Left::LeftDeclaration((decl_loc, decl))
                    if decl.declarations.len() == 1 && decl.declarations[0].init.is_none() =>
                {
                    let right_ast = eval_right()?;
                    let vdecl_loc = decl.declarations[0].loc.dupe();
                    let id = &decl.declarations[0].id;
                    let (id_ast, _) = variable(
                        cx,
                        decl.kind,
                        Some(&(type_::str_module_t::at as fn(ALoc) -> Type)),
                        id,
                        None,
                    )?;
                    (
                        statement::for_in::Left::LeftDeclaration((
                            decl_loc.dupe(),
                            statement::VariableDeclaration {
                                kind: decl.kind,
                                declarations: vec![ast::statement::variable::Declarator {
                                    loc: vdecl_loc,
                                    id: id_ast,
                                    init: None,
                                }]
                                .into(),
                                comments: decl.comments.dupe(),
                            },
                        )),
                        right_ast,
                    )
                }
                statement::for_in::Left::LeftPattern(ast::pattern::Pattern::Identifier {
                    loc: pat_loc,
                    inner: id_inner,
                }) => {
                    let right_ast = eval_right()?;
                    let pat_loc = pat_loc.dupe();
                    let name_loc = id_inner.name.loc.dupe();
                    let name_str = &id_inner.name.name;
                    let optional = id_inner.optional;
                    let t = type_::str_module_t::at(pat_loc.dupe());
                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                        var: Some(mk_reason(
                            VirtualReasonDesc::RIdentifier(Name::new(
                                name_str.dupe(),
                            )),
                            pat_loc.dupe(),
                        )),
                        init: reason_of_t(&t).dupe(),
                    }));
                    type_env::set_var(cx, &use_op, name_str, &t, pat_loc.dupe());
                    (
                        statement::for_in::Left::LeftPattern(ast::pattern::Pattern::Identifier {
                            loc: (pat_loc, t.dupe()),
                            inner: (ast::pattern::Identifier {
                                name: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (name_loc, t.dupe()),
                                    name: id_inner.name.name.dupe(),
                                    comments: id_inner.name.comments.dupe(),
                                }),
                                annot: match &id_inner.annot {
                                    ast::types::AnnotationOrHint::Available(_) => {
                                        let Ok(v) = polymorphic_ast_mapper::type_annotation_hint(
                                            &mut typed_ast_utils::UncheckedMapper,
                                            &id_inner.annot,
                                        );
                                        v
                                    }
                                    ast::types::AnnotationOrHint::Missing(mloc) => {
                                        ast::types::AnnotationOrHint::Missing((mloc.dupe(), t))
                                    }
                                },
                                optional,
                            })
                            .into(),
                        }),
                        right_ast,
                    )
                }
                _ => {
                    let right_ast = eval_right()?;
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInternal(Box::new((loc.dupe(), InternalError::ForInLHS))),
                    );
                    {
                        let Ok(v) = polymorphic_ast_mapper::for_in_statement_lhs(
                            &mut typed_ast_utils::ErrorMapper,
                            &inner.left,
                        );
                        (v, right_ast)
                    }
                }
            };
            let (_, right_t) = right_ast.loc();
            type_operation_utils::type_assertions::assert_for_in_rhs(cx, right_t);
            let body_ast = statement(cx, &inner.body)?;
            statement::Statement::new(StatementInner::ForIn {
                loc,
                inner: (statement::ForIn {
                    left: left_ast,
                    right: right_ast,
                    body: body_ast,
                    each: inner.each,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::ForOf { inner, .. } => {
            let reason_desc = match &inner.left {
                statement::for_of::Left::LeftDeclaration((_, decl)) => {
                    if let Some(first_decl) = decl.declarations.first()
                        && let ast::pattern::Pattern::Identifier {
                            inner: id_inner, ..
                        } = &first_decl.id
                    {
                        VirtualReasonDesc::RIdentifier(Name::new(
                            id_inner.name.name.dupe(),
                        ))
                    } else {
                        VirtualReasonDesc::RForOfElement
                    }
                }
                statement::for_of::Left::LeftPattern(patt) => {
                    if let ast::pattern::Pattern::Identifier {
                        inner: id_inner, ..
                    } = patt
                    {
                        VirtualReasonDesc::RIdentifier(Name::new(
                            id_inner.name.name.dupe(),
                        ))
                    } else {
                        VirtualReasonDesc::RForOfElement
                    }
                }
            };
            let reason = mk_reason(reason_desc, loc.dupe());
            let eval_right = || {
                let right_ast = expression(None, None, None, cx, &inner.right)?;
                let (_, t) = right_ast.loc();
                let elem_t = for_of_elemt(cx, t.dupe(), reason.dupe(), inner.await_);
                // null/undefined are NOT allowed
                let elem_t = flow_js::reposition_non_speculating(cx, loc_of_t(t).dupe(), elem_t);
                Ok((elem_t, right_ast))
            };
            let (left_ast, right_ast) = match &inner.left {
                statement::for_of::Left::LeftDeclaration((decl_loc, decl))
                    if decl.declarations.len() == 1 && decl.declarations[0].init.is_none() =>
                {
                    let (elem_t, right_ast) = eval_right()?;
                    let vdecl_loc = decl.declarations[0].loc.dupe();
                    let id = &decl.declarations[0].id;
                    let elem_t_clone = elem_t.dupe();
                    let (id_ast, _) = variable(
                        cx,
                        decl.kind,
                        Some(&move |_: ALoc| elem_t_clone.dupe()),
                        id,
                        None,
                    )?;
                    (
                        statement::for_of::Left::LeftDeclaration((
                            decl_loc.dupe(),
                            statement::VariableDeclaration {
                                kind: decl.kind,
                                declarations: vec![ast::statement::variable::Declarator {
                                    loc: vdecl_loc,
                                    id: id_ast,
                                    init: None,
                                }]
                                .into(),
                                comments: decl.comments.dupe(),
                            },
                        )),
                        right_ast,
                    )
                }
                statement::for_of::Left::LeftPattern(ast::pattern::Pattern::Identifier {
                    loc: pat_loc,
                    inner: id_inner,
                }) => {
                    let (elem_t, right_ast) = eval_right()?;
                    let pat_loc = pat_loc.dupe();
                    let name_loc = id_inner.name.loc.dupe();
                    let name_str = &id_inner.name.name;
                    let optional = id_inner.optional;
                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                        var: Some(mk_reason(
                            VirtualReasonDesc::RIdentifier(Name::new(
                                name_str.dupe(),
                            )),
                            pat_loc.dupe(),
                        )),
                        init: reason_of_t(&elem_t).dupe(),
                    }));
                    type_env::set_var(cx, &use_op, name_str, &elem_t, pat_loc.dupe());
                    (
                        statement::for_of::Left::LeftPattern(ast::pattern::Pattern::Identifier {
                            loc: (pat_loc, elem_t.dupe()),
                            inner: ast::pattern::Identifier {
                                name: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (name_loc, elem_t.dupe()),
                                    name: id_inner.name.name.dupe(),
                                    comments: id_inner.name.comments.dupe(),
                                }),
                                annot: match &id_inner.annot {
                                    ast::types::AnnotationOrHint::Available(annot) => {
                                        let Ok(v) = polymorphic_ast_mapper::type_annotation(
                                            &mut typed_ast_utils::ErrorMapper,
                                            annot,
                                        );
                                        ast::types::AnnotationOrHint::Available(v)
                                    }
                                    ast::types::AnnotationOrHint::Missing(mloc) => {
                                        ast::types::AnnotationOrHint::Missing((mloc.dupe(), elem_t))
                                    }
                                },
                                optional,
                            }
                            .into(),
                        }),
                        right_ast,
                    )
                }
                _ => {
                    let (_, right_ast) = eval_right()?;
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInternal(Box::new((loc.dupe(), InternalError::ForOfLHS))),
                    );
                    let left_ast = match &inner.left {
                        statement::for_of::Left::LeftDeclaration((dloc, decl)) => {
                            let mut mapper = typed_ast_utils::ErrorMapper;
                            {
                                let Ok(v) =
                                    polymorphic_ast_mapper::variable_declaration(&mut mapper, decl);
                                statement::for_of::Left::LeftDeclaration((dloc.dupe(), v))
                            }
                        }
                        statement::for_of::Left::LeftPattern(patt) => {
                            let mut mapper = typed_ast_utils::ErrorMapper;
                            {
                                let Ok(v) =
                                    polymorphic_ast_mapper::assignment_pattern(&mut mapper, patt);
                                statement::for_of::Left::LeftPattern(v)
                            }
                        }
                    };
                    (left_ast, right_ast)
                }
            };
            let body_ast = statement(cx, &inner.body)?;
            statement::Statement::new(StatementInner::ForOf {
                loc,
                inner: (statement::ForOf {
                    left: left_ast,
                    right: right_ast,
                    body: body_ast,
                    await_: inner.await_,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::Debugger { loc: dloc, inner } => {
            statement::Statement::new(StatementInner::Debugger {
                loc: dloc.dupe(),
                inner: inner.clone(),
            })
        }
        StatementInner::FunctionDeclaration { inner, .. } => {
            let (_, _, node) = function_(cx, false, loc, inner)?;
            node
        }
        StatementInner::ComponentDeclaration { inner, .. } => {
            if cx.component_syntax() {
                error_on_this_uses_in_components(cx, inner);
                let name_loc = inner.id.loc.dupe();
                let name = &inner.id.name;
                let first_char = name.chars().next();
                if let Some(c) = first_char {
                    if c.is_ascii_lowercase() {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EComponentCase(name_loc.dupe()),
                        );
                    }
                }
                let reason = mk_reason(
                    VirtualReasonDesc::RComponent(Name::new(name.dupe())),
                    loc.dupe(),
                );
                match (&inner.body, cx.under_declaration_context()) {
                    (None, false) => {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EComponentMissingBody(loc.dupe()),
                        );
                    }
                    (Some(_), true) => {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EComponentBodyInAmbientContext(loc.dupe()),
                        );
                    }
                    (None, true) => {
                        if !cx.tslib_syntax() {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    loc.dupe(),
                                    UnsupportedSyntax::TSLibSyntax(
                                        TsLibSyntaxKind::DeclarationWithoutDeclare,
                                    ),
                                ))),
                            );
                        }
                    }
                    (Some(_), false) => {}
                }
                let tparams_map = BTreeMap::new();
                let (component_sig, reconstruct_component) =
                    mk_component_sig(cx, &tparams_map, reason.dupe(), inner);
                let general = type_env::read_declared_type(cx, reason, name_loc);
                let (params_ast, body_ast_opt) = match &inner.body {
                    None => {
                        // For ambient components, only evaluate params - skip body evaluation
                        let params_ast = crate::component_params::eval::<
                            crate::component_params::DeclarationConfig,
                            _,
                        >(
                            cx,
                            &component_sig.cparams.params,
                            component_sig.cparams.rest.as_ref(),
                            &*component_sig.cparams.reconstruct,
                        )?;
                        (params_ast, None)
                    }
                    Some(_) => {
                        let (params_ast, body_ast) =
                            crate::component_sig::toplevels(cx, &component_sig)?;
                        (params_ast, Some(body_ast))
                    }
                };
                statement::Statement::new(StatementInner::ComponentDeclaration {
                    loc,
                    inner: (reconstruct_component)(params_ast, body_ast_opt, general).into(),
                })
            } else {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::ComponentSyntax,
                    ))),
                );
                statement::Statement::new(StatementInner::ComponentDeclaration {
                    loc,
                    inner: {
                        let Ok(v) = polymorphic_ast_mapper::component_declaration(
                            &mut typed_ast_utils::ErrorMapper,
                            inner,
                        );
                        v
                    }
                    .into(),
                })
            }
        }
        StatementInner::EnumDeclaration { inner, .. } => {
            let enum_ast = enum_declaration(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::EnumDeclaration {
                loc,
                inner: enum_ast.into(),
            })
        }
        StatementInner::DeclareVariable { inner, .. } => {
            let decl_ast = declare_variable(cx, loc.dupe(), inner)?;
            statement::Statement::new(StatementInner::DeclareVariable {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareFunction { inner, .. } if inner.id.is_none() => {
            panic!("unexpected anonymous declare function")
        }
        StatementInner::DeclareFunction { inner, .. } => {
            let decl_ast = declare_function_inner(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareFunction {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::VariableDeclaration { inner, .. } => {
            statement::Statement::new(StatementInner::VariableDeclaration {
                loc,
                inner: variables(cx, inner)?.into(),
            })
        }
        StatementInner::ClassDeclaration { inner, .. } => {
            let id = inner
                .id
                .as_ref()
                .expect("unexpected anonymous class declaration");
            let name_loc = id.loc.dupe();
            let name = Name::new(id.name.dupe());
            let reason = type_::desc_format::instance_reason(name.dupe(), name_loc.dupe());
            let tast_class_type = type_env::read_declared_type(cx, reason.dupe(), name_loc.dupe());
            let (class_t, c_ast) = mk_class(
                cx,
                loc.dupe(),
                name_loc.dupe(),
                Some(tast_class_type),
                reason,
                inner,
            )?;
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                var: Some(mk_reason(
                    VirtualReasonDesc::RIdentifier(name),
                    name_loc.dupe(),
                )),
                init: reason_of_t(&class_t).dupe(),
            }));
            type_env::init_implicit_let(cx, &use_op, &class_t, name_loc);
            statement::Statement::new(StatementInner::ClassDeclaration {
                loc,
                inner: c_ast.into(),
            })
        }
        StatementInner::DeclareClass { inner, .. } => {
            let name = &inner.id.name;
            let name_loc = inner.id.loc.dupe();
            let (t, decl_ast) = declare_class(cx, loc.dupe(), inner);
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                var: Some(mk_reason(
                    VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                    loc.dupe(),
                )),
                init: reason_of_t(&t).dupe(),
            }));
            type_env::init_var(cx, &use_op, &t, name_loc);
            statement::Statement::new(StatementInner::DeclareClass {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::RecordDeclaration { inner, .. } => {
            if !cx.enable_records() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((loc.dupe(), UnsupportedSyntax::Records))),
                );
                statement::Statement::new(StatementInner::RecordDeclaration {
                    loc,
                    inner: {
                        let Ok(v) = polymorphic_ast_mapper::record_declaration(
                            &mut typed_ast_utils::ErrorMapper,
                            inner,
                        );
                        v
                    }
                    .into(),
                })
            } else {
                let name_loc = inner.id.loc.dupe();
                let name_str = &inner.id.name;
                let invalid_properties_syntax: Vec<
                    &statement::record_declaration::InvalidPropertySyntax<ALoc>,
                > = inner
                    .body
                    .body
                    .iter()
                    .filter_map(|element| match element {
                        statement::record_declaration::BodyElement::Method(_) => None,
                        statement::record_declaration::BodyElement::Property(prop) => {
                            prop.invalid_syntax.as_ref()
                        }
                        statement::record_declaration::BodyElement::StaticProperty(prop) => {
                            prop.invalid_syntax.as_ref()
                        }
                    })
                    .collect();
                error_on_record_declaration_invalid_syntax(
                    cx,
                    name_loc.dupe(),
                    inner.invalid_syntax.as_ref(),
                    &invalid_properties_syntax,
                );
                let is_a_to_z = |c: char| c >= 'a' && c <= 'z';
                if !name_str.is_empty() && is_a_to_z(name_str.chars().next().unwrap()) {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::ERecordError(RecordErrorKind::RecordInvalidName {
                            loc: name_loc.dupe(),
                            name: name_str.dupe(),
                        }),
                    );
                }
                let name = Name::new(name_str.dupe());
                let reason = type_::desc_format::instance_reason(name.dupe(), name_loc.dupe());
                let tast_record_type =
                    type_env::read_declared_type(cx, reason.dupe(), name_loc.dupe());
                let (record_t, ast) = mk_record(
                    cx,
                    loc.dupe(),
                    name_loc.dupe(),
                    Some(tast_record_type),
                    reason,
                    inner,
                )?;
                let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                    var: Some(mk_reason(
                        VirtualReasonDesc::RIdentifier(name),
                        name_loc.dupe(),
                    )),
                    init: reason_of_t(&record_t).dupe(),
                }));
                type_env::init_implicit_const(cx, &use_op, &record_t, name_loc);
                statement::Statement::new(StatementInner::RecordDeclaration {
                    loc,
                    inner: ast.into(),
                })
            }
        }
        StatementInner::DeclareComponent { inner, .. } => {
            let name = &inner.id.name;
            let name_loc = inner.id.loc.dupe();
            let first_char = name.chars().next();
            if let Some(c) = first_char {
                if c.is_ascii_lowercase() {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EComponentCase(name_loc.dupe()),
                    );
                }
            }
            validate_declare_component_params(cx, &inner.params);
            let (t, decl_ast) = declare_component(cx, loc.dupe(), inner);
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                var: Some(mk_reason(
                    VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                    loc.dupe(),
                )),
                init: reason_of_t(&t).dupe(),
            }));
            type_env::init_var(cx, &use_op, &t, name_loc);
            statement::Statement::new(StatementInner::DeclareComponent {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareEnum { inner, .. } => {
            let decl_ast = enum_declaration(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareEnum {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareInterface { inner, .. } => {
            let (_, decl_ast) = interface(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareInterface {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::InterfaceDeclaration { inner, .. } => {
            let (_, decl_ast) = interface(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::InterfaceDeclaration {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareModule { inner, .. } => {
            let decl_ast = declare_module(cx, inner);
            statement::Statement::new(StatementInner::DeclareModule {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareNamespace { inner, .. } => {
            let (_, decl_ast) = declare_namespace(cx, loc.dupe(), inner);
            statement::Statement::new(StatementInner::DeclareNamespace {
                loc,
                inner: decl_ast.into(),
            })
        }
        StatementInner::DeclareExportDeclaration { inner, .. } => {
            let declaration = inner
                .declaration
                .as_ref()
                .map(|decl| {
                    use ast::statement::declare_export_declaration::Declaration as D;
                    Ok(match decl {
                        D::Variable {
                            loc: d_loc,
                            declaration: v,
                        } => D::Variable {
                            loc: d_loc.dupe(),
                            declaration: declare_variable(cx, d_loc.dupe(), v)?.into(),
                        },
                        D::Function {
                            loc: d_loc,
                            declaration: f,
                        } => {
                            if f.id.is_none() && !cx.tslib_syntax() {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        d_loc.dupe(),
                                        UnsupportedSyntax::TSLibSyntax(
                                            TsLibSyntaxKind::AnonymousDefaultExportFunction,
                                        ),
                                    ))),
                                );
                                D::Function {
                                    loc: d_loc.dupe(),
                                    declaration: {
                                        let Ok(v) = polymorphic_ast_mapper::declare_function(
                                            &mut typed_ast_utils::ErrorMapper,
                                            f,
                                        );
                                        v.into()
                                    },
                                }
                            } else {
                                D::Function {
                                    loc: d_loc.dupe(),
                                    declaration: declare_function_inner(cx, d_loc.dupe(), f).into(),
                                }
                            }
                        }
                        D::Class {
                            loc: d_loc,
                            declaration: c,
                        } => {
                            let (_, c_ast) = declare_class(cx, d_loc.dupe(), c);
                            D::Class {
                                loc: d_loc.dupe(),
                                declaration: c_ast.into(),
                            }
                        }
                        D::Component {
                            loc: d_loc,
                            declaration: c,
                        } => {
                            let (_, c_ast) = declare_component(cx, d_loc.dupe(), c);
                            D::Component {
                                loc: d_loc.dupe(),
                                declaration: c_ast.into(),
                            }
                        }
                        D::DefaultType { type_ } => {
                            let t_ast = type_annotation::convert(cx, Default::default(), type_);
                            D::DefaultType {
                                type_: t_ast.into(),
                            }
                        }
                        D::NamedType {
                            loc: d_loc,
                            declaration: t,
                        } => {
                            let (_, type_alias_ast) = type_alias(cx, d_loc.dupe(), t);
                            if type_env::in_toplevel_scope(cx) {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnnecessaryDeclareTypeOnlyExport(d_loc.dupe()),
                                );
                            }
                            D::NamedType {
                                loc: d_loc.dupe(),
                                declaration: type_alias_ast.into(),
                            }
                        }
                        D::NamedOpaqueType {
                            loc: d_loc,
                            declaration: t,
                        } => {
                            let (_, opaque_type_ast) = opaque_type(cx, d_loc.dupe(), t);
                            D::NamedOpaqueType {
                                loc: d_loc.dupe(),
                                declaration: opaque_type_ast.into(),
                            }
                        }
                        D::Interface {
                            loc: d_loc,
                            declaration: i,
                        } => {
                            let (_, i_ast) = interface(cx, d_loc.dupe(), i);
                            if type_env::in_toplevel_scope(cx) {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnnecessaryDeclareTypeOnlyExport(d_loc.dupe()),
                                );
                            }
                            D::Interface {
                                loc: d_loc.dupe(),
                                declaration: i_ast.into(),
                            }
                        }
                        D::Enum {
                            loc: d_loc,
                            declaration: e,
                        } => {
                            let enum_ast = enum_declaration(cx, d_loc.dupe(), e);
                            D::Enum {
                                loc: d_loc.dupe(),
                                declaration: enum_ast.into(),
                            }
                        }
                        D::Namespace {
                            loc: d_loc,
                            declaration: ns,
                        } => {
                            if !cx.tslib_syntax() {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        d_loc.dupe(),
                                        UnsupportedSyntax::TSLibSyntax(
                                            TsLibSyntaxKind::DeclareExportNamespace,
                                        ),
                                    ))),
                                );
                            }
                            let (_, ns_ast) = declare_namespace(cx, d_loc.dupe(), ns);
                            D::Namespace {
                                loc: d_loc.dupe(),
                                declaration: Box::new(ns_ast),
                            }
                        }
                    })
                })
                .transpose()?;
            let source = inner.source.as_ref().map(|(source_loc, source_literal)| {
                let module_name = &source_literal.value;
                let source_module_rc = flow_js_utils::import_export_utils::get_module_type_or_any(
                    cx,
                    false,
                    Some(type_::ImportKind::ImportValue),
                    source_loc.dupe(),
                    flow_import_specifier::Userland::from_smol_str(module_name.dupe()),
                )
                .expect("Should not be under speculation");
                let source_module: Result<type_::ModuleType, Type> = source_module_rc;
                (source_module, source_loc.dupe(), source_literal)
            });
            let specifiers = inner.specifiers.as_ref().map(|spec| {
                let export_kind = statement::ExportKind::ExportValue;
                let source_ref = source
                    .as_ref()
                    .map(|(sm, sl, slit)| (sm, sl.dupe(), slit.clone()));
                export_specifiers(cx, source_ref, export_kind, spec)
            });
            let source = source.map(|(_, source_loc, source_literal)| {
                (
                    (
                        source_loc.dupe(),
                        type_::str_module_t::at(source_loc.dupe()),
                    ),
                    source_literal.clone(),
                )
            });
            statement::Statement::new(StatementInner::DeclareExportDeclaration {
                loc,
                inner: (statement::DeclareExportDeclaration {
                    default: inner.default.clone(),
                    declaration,
                    specifiers,
                    source,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::DeclareModuleExports { inner, .. } => {
            let t_loc = inner.annot.loc.dupe();
            let t_ast = type_annotation::convert(cx, Default::default(), &inner.annot.annotation);
            let (_, t) = t_ast.loc();
            let reason = {
                let filename = cx.file();
                mk_reason(
                    VirtualReasonDesc::RModule(
                        flow_import_specifier::Userland::from_smol_str(filename.as_str().into()),
                    ),
                    flow_aloc::ALoc::of_loc(flow_parser::loc::Loc {
                        source: Some(filename.dupe()),
                        ..flow_parser::loc::Loc::none()
                    }),
                )
            };
            flow_js::flow_t_non_speculating(cx, (t, &type_::unsoundness::exports_any(reason)));
            statement::Statement::new(StatementInner::DeclareModuleExports {
                loc,
                inner: (statement::DeclareModuleExports {
                    annot: ast::types::Annotation {
                        loc: t_loc,
                        annotation: t_ast,
                    },
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::ExportAssignment { inner, .. } => {
            if !cx.tslib_syntax() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::ExportAssignment),
                    ))),
                );
                {
                    let Ok(v) =
                        polymorphic_ast_mapper::statement(&mut typed_ast_utils::ErrorMapper, stmt);
                    v
                }
            } else {
                let rhs = match &inner.rhs {
                    statement::ExportAssignmentRhs::Expression(e) => {
                        statement::ExportAssignmentRhs::Expression(
                            expression(None, None, None, cx, e)?,
                        )
                    }
                    statement::ExportAssignmentRhs::DeclareFunction(fn_loc, decl) => {
                        let (_, annot_ast) =
                            type_annotation::mk_type_available_annotation(cx, FlowOrdMap::new(), &decl.annot);
                        let (_, t) = annot_ast.annotation.loc();
                        let id = decl.id.as_ref().map(|id_name| {
                            ast::Identifier::new(ast::IdentifierInner {
                                loc: (id_name.loc.dupe(), t.dupe()),
                                name: id_name.name.dupe(),
                                comments: id_name.comments.dupe(),
                            })
                        });
                        let predicate = decl.predicate.as_ref().map(|p| {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    p.loc.dupe(),
                                    UnsupportedSyntax::PredicateFunction,
                                ))),
                            );
                            let Ok(v) =
                                polymorphic_ast_mapper::predicate(&mut typed_ast_utils::ErrorMapper, p);
                            v
                        });
                        statement::ExportAssignmentRhs::DeclareFunction(
                            fn_loc.dupe(),
                            statement::DeclareFunction {
                                id,
                                annot: annot_ast,
                                predicate,
                                comments: decl.comments.dupe(),
                                implicit_declare: decl.implicit_declare,
                            },
                        )
                    }
                };
                statement::Statement::new(StatementInner::ExportAssignment {
                    loc,
                    inner: (statement::ExportAssignment {
                        rhs,
                        comments: inner.comments.dupe(),
                    })
                    .into(),
                })
            }
        }
        StatementInner::NamespaceExportDeclaration { .. } => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::NamespaceExportDeclaration),
                ))),
            );
            {
                let Ok(v) =
                    polymorphic_ast_mapper::statement(&mut typed_ast_utils::ErrorMapper, stmt);
                v
            }
        }
        StatementInner::ExportNamedDeclaration { inner, .. } => {
            let has_type_specifier = match &inner.specifiers {
                Some(ast::statement::export_named_declaration::Specifier::ExportSpecifiers(
                    specs,
                )) => specs
                    .iter()
                    .any(|spec| spec.export_kind == statement::ExportKind::ExportType),
                _ => false,
            };
            if has_type_specifier && !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::ExportTypeSpecifier),
                    ))),
                );
            }
            if has_type_specifier && inner.export_kind == statement::ExportKind::ExportType {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::ExportTypeSpecifierInExportType,
                    ))),
                );
            }
            let declaration = inner
                .declaration
                .as_ref()
                .map(|decl_stmt| statement(cx, decl_stmt))
                .transpose()?;
            let source = inner.source.as_ref().map(|(source_loc, source_literal)| {
                let (perform_platform_validation, import_kind_for_untyped_import_validation) =
                    match inner.export_kind {
                        statement::ExportKind::ExportType => {
                            (false, Some(type_::ImportKind::ImportType))
                        }
                        statement::ExportKind::ExportValue => {
                            (true, Some(type_::ImportKind::ImportValue))
                        }
                    };
                let module_name = &source_literal.value;
                let source_module = flow_js_utils::import_export_utils::get_module_type_or_any(
                    cx,
                    perform_platform_validation,
                    import_kind_for_untyped_import_validation,
                    source_loc.dupe(),
                    flow_import_specifier::Userland::from_smol_str(module_name.dupe()),
                )
                .expect("Should not be under speculation");
                let source_module_deref: Result<type_::ModuleType, Type> = source_module;
                (source_module_deref, source_loc.dupe(), source_literal)
            });
            let specifiers = inner.specifiers.as_ref().map(|spec| {
                let source_ref = source
                    .as_ref()
                    .map(|(sm, sl, slit)| (sm, sl.dupe(), slit.clone()));
                export_specifiers(cx, source_ref, inner.export_kind, spec)
            });
            let source = source.map(|(_, source_loc, source_literal)| {
                (
                    (
                        source_loc.dupe(),
                        type_::str_module_t::at(source_loc.dupe()),
                    ),
                    source_literal.clone(),
                )
            });
            statement::Statement::new(StatementInner::ExportNamedDeclaration {
                loc,
                inner: (statement::ExportNamedDeclaration {
                    declaration,
                    specifiers,
                    source,
                    export_kind: inner.export_kind,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::ExportDefaultDeclaration { inner, .. } => {
            let (t, declaration) = match &inner.declaration {
                statement::export_default_declaration::Declaration::Declaration(decl_stmt) => {
                    match &**decl_stmt {
                        StatementInner::FunctionDeclaration {
                            loc: fn_loc,
                            inner: func,
                        } if func.id.is_none() => {
                            let sig_loc = func.sig_loc.dupe();
                            let reason = func_reason(func.async_, func.generator, sig_loc);
                            let (t, fn_ast) =
                                mk_function_declaration(cx, None, reason, fn_loc.dupe(), func)?;
                            (
                                t,
                                statement::export_default_declaration::Declaration::Declaration(
                                    statement::Statement::new(
                                        StatementInner::FunctionDeclaration {
                                            loc: fn_loc.dupe(),
                                            inner: fn_ast.into(),
                                        },
                                    ),
                                ),
                            )
                        }
                        StatementInner::ClassDeclaration {
                            loc: class_loc,
                            inner: c,
                        } if c.id.is_none() => {
                            let reason = type_::desc_format::instance_reason(
                                Name::new("<<anonymous class>>"),
                                class_loc.dupe(),
                            );
                            let (t, c_ast) =
                                mk_class(cx, class_loc.dupe(), class_loc.dupe(), None, reason, c)?;
                            (
                                t,
                                statement::export_default_declaration::Declaration::Declaration(
                                    statement::Statement::new(StatementInner::ClassDeclaration {
                                        loc: class_loc.dupe(),
                                        inner: c_ast.into(),
                                    }),
                                ),
                            )
                        }
                        _ => {
                            let stmt_ast = statement(cx, decl_stmt)?;
                            let id = match &**decl_stmt {
                                StatementInner::FunctionDeclaration { inner: func, .. } => {
                                    func.id.as_ref()
                                }
                                StatementInner::ClassDeclaration { inner: c, .. } => c.id.as_ref(),
                                StatementInner::EnumDeclaration { inner: e, .. } => Some(&e.id),
                                StatementInner::ComponentDeclaration { inner: comp, .. } => {
                                    Some(&comp.id)
                                }
                                StatementInner::RecordDeclaration { inner: r, .. } => Some(&r.id),
                                _ => panic!("unexpected default export declaration"),
                            };
                            let id = id.expect("unexpected default export declaration");
                            let t = type_env::get_var_declared_type(
                                Some(type_env::LookupMode::ForValue),
                                None,
                                cx,
                                Name::new(id.name.dupe()),
                                id.loc.dupe(),
                            );
                            (
                                t,
                                statement::export_default_declaration::Declaration::Declaration(
                                    stmt_ast,
                                ),
                            )
                        }
                    }
                }
                statement::export_default_declaration::Declaration::Expression(expr) => {
                    let expr_ast = expression(None, None, None, cx, expr)?;
                    let (_, t) = expr_ast.loc();
                    (
                        t.dupe(),
                        statement::export_default_declaration::Declaration::Expression(expr_ast),
                    )
                }
            };
            statement::Statement::new(StatementInner::ExportDefaultDeclaration {
                loc,
                inner: statement::ExportDefaultDeclaration {
                    default: (inner.default.dupe(), t),
                    declaration,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        StatementInner::ImportDeclaration { inner, .. } => {
            let (ref source_loc, ref source_literal) = inner.source;
            let module_name_str = &source_literal.value;
            let (perform_platform_validation, import_kind_for_untyped_import_validation) =
                match inner.import_kind {
                    statement::ImportKind::ImportType => {
                        (false, Some(type_::ImportKind::ImportType))
                    }
                    statement::ImportKind::ImportTypeof => {
                        (false, Some(type_::ImportKind::ImportTypeof))
                    }
                    statement::ImportKind::ImportValue => {
                        (true, Some(type_::ImportKind::ImportValue))
                    }
                };
            let module_name =
                flow_import_specifier::Userland::from_smol_str(module_name_str.dupe());
            let source_module_rc = flow_js_utils::import_export_utils::get_module_type_or_any(
                cx,
                perform_platform_validation,
                import_kind_for_untyped_import_validation,
                source_loc.dupe(),
                module_name.dupe(),
            )
            .expect("Should not be under speculation");
            let source_module: Result<type_::ModuleType, Type> = source_module_rc;
            let source_ast = (
                (
                    source_loc.dupe(),
                    type_::str_module_t::at(source_loc.dupe()),
                ),
                source_literal.clone(),
            );
            let specifiers_ast = inner.specifiers.as_ref().map(|specifiers| match specifiers {
                statement::import_declaration::Specifier::ImportNamedSpecifiers(
                    named_specifiers,
                ) => {
                    let named_specifiers_ast: Vec<_> = named_specifiers
                        .iter()
                        .map(|spec| {
                            let remote = &spec.remote;
                            let remote_name_loc = remote.loc.dupe();
                            let remote_name = &remote.name;
                            let local_name = match &spec.local {
                                Some(local_id) => &local_id.name,
                                None => remote_name,
                            };
                            let import_reason = mk_reason(
                                
                                    VirtualReasonDesc::RNamedImportedType(
                                        module_name.dupe(),
                                        local_name.dupe(),
                                    ),
                                remote_name_loc.dupe(),
                            );
                            let spec_import_kind = spec.kind.unwrap_or(inner.import_kind);
                            let (remote_name_def_loc, imported_t) =
                                flow_js_utils::import_export_utils::import_named_specifier_type(
                                    cx,
                                    import_reason,
                                    &|cx, reason, t| {
                                        FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                                            .map_err(Into::into)
                                    },
                                    &spec_import_kind,
                                    module_name.dupe(),
                                    &source_module,
                                    remote_name,
                                    local_name,
                                )
                                .expect("Should not be under speculation");
                            if flow_common::files::has_ts_ext(cx.file())
                                && spec_import_kind == statement::ImportKind::ImportValue
                            {
                                let binding_def_loc = match &spec.local {
                                    Some(local_id) => local_id.loc.dupe(),
                                    None => remote_name_loc.dupe(),
                                };
                                cx.add_ts_import_provenance(
                                    binding_def_loc,
                                    flow_common::flow_import_specifier::FlowImportSpecifier::Userland(module_name.dupe()),
                                    remote_name.dupe(),
                                );
                            }
                            let remote_ast = ast::Identifier::new(ast::IdentifierInner {
                                loc: (remote_name_loc, imported_t.dupe()),
                                name: remote_name.dupe(),
                                comments: remote.comments.dupe(),
                            });
                            let local_ast = spec.local.as_ref().map(|local_id| {
                                ast::Identifier::new(ast::IdentifierInner {
                                    loc: (local_id.loc.dupe(), imported_t.dupe()),
                                    name: local_id.name.dupe(),
                                    comments: local_id.comments.dupe(),
                                })
                            });
                            statement::import_declaration::NamedSpecifier {
                                local: local_ast,
                                remote: remote_ast,
                                remote_name_def_loc,
                                kind: spec.kind,
                            }
                        })
                        .collect();
                    statement::import_declaration::Specifier::ImportNamedSpecifiers(
                        named_specifiers_ast,
                    )
                }
                statement::import_declaration::Specifier::ImportNamespaceSpecifier((
                    loc_with_star,
                    local_id,
                )) => {
                    let local_loc = local_id.loc.dupe();
                    let local_name = &local_id.name;
                    let import_reason_desc = match inner.import_kind {
                        statement::ImportKind::ImportType => {
                            VirtualReasonDesc::RImportStarType(
                                local_name.dupe(),
                            )
                        }
                        statement::ImportKind::ImportTypeof => {
                            VirtualReasonDesc::RImportStarTypeOf(
                                local_name.dupe(),
                            )
                        }
                        statement::ImportKind::ImportValue => {
                            VirtualReasonDesc::RImportStar(
                                local_name.dupe(),
                            )
                        }
                    };
                    let import_reason = mk_reason(
                        import_reason_desc,
                        local_loc.dupe(),
                    );
                    let t = flow_js_utils::import_export_utils::import_namespace_specifier_type(
                        cx,
                        import_reason,
                        &inner.import_kind,
                        module_name.dupe(),
                        flow_common::flow_symbol::Symbol::mk_namespace_symbol(
                            local_name.dupe(),
                            local_loc.dupe(),
                        ),
                        &source_module,
                        local_loc.dupe(),
                    ).expect("Should not be under speculation");
                    let namespace_specifier_ast = ast::Identifier::new(ast::IdentifierInner {
                        loc: (local_loc, t),
                        name: local_name.dupe(),
                        comments: local_id.comments.dupe(),
                    });
                    statement::import_declaration::Specifier::ImportNamespaceSpecifier(
                        (loc_with_star.dupe(), namespace_specifier_ast),
                    )
                }
            });
            let default_ast = inner.default.as_ref().map(|default| {
                let id = &default.identifier;
                let id_loc = id.loc.dupe();
                let local_name = &id.name;
                let import_reason = mk_reason(
                    VirtualReasonDesc::RDefaultImportedType(
                        local_name.dupe(),
                        module_name.dupe(),
                    ),
                    id_loc.dupe(),
                );
                let (remote_default_name_def_loc, imported_t) =
                    flow_js_utils::import_export_utils::import_default_specifier_type(
                        cx,
                        import_reason,
                        &|cx, reason, t| {
                            FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                                .map_err(Into::into)
                        },
                        &inner.import_kind,
                        module_name.dupe(),
                        &source_module,
                        local_name,
                    )
                    .expect("Should not be under speculation");
                statement::import_declaration::DefaultIdentifier {
                    identifier: ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_loc, imported_t),
                        name: local_name.dupe(),
                        comments: id.comments.dupe(),
                    }),
                    remote_default_name_def_loc,
                }
            });
            statement::Statement::new(StatementInner::ImportDeclaration {
                loc,
                inner: (statement::ImportDeclaration {
                    source: source_ast,
                    specifiers: specifiers_ast,
                    default: default_ast,
                    import_kind: inner.import_kind,
                    attributes: None,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        StatementInner::ImportEqualsDeclaration { inner, .. } => {
            if !cx.tslib_syntax() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::ImportEqualsDeclaration),
                    ))),
                );
                {
                    let Ok(v) =
                        polymorphic_ast_mapper::statement(&mut typed_ast_utils::ErrorMapper, stmt);
                    v
                }
            } else {
                let id_loc = inner.id.loc.dupe();
                let id_ident = &inner.id;
                match &inner.module_reference {
                    statement::import_equals_declaration::ModuleReference::ExternalModuleReference(
                        source_loc,
                        source_literal,
                    ) => {
                        let module_name_str = &source_literal.value;
                        let (perform_platform_validation, import_kind_for_untyped_import_validation) =
                            match inner.import_kind {
                                statement::ImportKind::ImportType => {
                                    (false, Some(type_::ImportKind::ImportType))
                                }
                                statement::ImportKind::ImportTypeof => {
                                    (false, Some(type_::ImportKind::ImportTypeof))
                                }
                                statement::ImportKind::ImportValue => {
                                    (true, Some(type_::ImportKind::ImportValue))
                                }
                            };
                        let module_name =
                            flow_import_specifier::Userland::from_smol_str(
                                module_name_str.dupe(),
                            );
                        let source_module =
                            flow_js_utils::import_export_utils::get_module_type_or_any(
                                cx,
                                perform_platform_validation,
                                import_kind_for_untyped_import_validation,
                                source_loc.dupe(),
                                module_name.dupe(),
                            )
                            .expect("Should not be under speculation");
                        let source_module: Result<type_::ModuleType, Type> =
                            source_module;
                        let local_name = &id_ident.name;
                        let import_reason = mk_reason(
                            
                                VirtualReasonDesc::RDefaultImportedType(
                                    local_name.dupe(),
                                    module_name.dupe(),
                                ),
                            id_loc.dupe(),
                        );
                        let (_remote_default_name_def_loc, imported_t) =
                            flow_js_utils::import_export_utils::import_default_specifier_type(
                                cx,
                                import_reason,
                                &|cx, reason, t| {
                                    FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                                        .map_err(Into::into)
                                },
                                &inner.import_kind,
                                module_name,
                                &source_module,
                                local_name,
                            )
                            .expect("Should not be under speculation");
                        statement::Statement::new(StatementInner::ImportEqualsDeclaration {
                            loc,
                            inner: (statement::ImportEqualsDeclaration {
                                id: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc, imported_t),
                                    name: id_ident.name.dupe(),
                                    comments: id_ident.comments.dupe(),
                                }),
                                module_reference:
                                    statement::import_equals_declaration::ModuleReference::ExternalModuleReference(
                                        (
                                            source_loc.dupe(),
                                            type_::str_module_t::at(source_loc.dupe()),
                                        ),
                                        source_literal.clone(),
                                    ),
                                import_kind: inner.import_kind,
                                is_export: inner.is_export,
                                comments: inner.comments.dupe(),
                            }).into(),
                        })
                    }
                    statement::import_equals_declaration::ModuleReference::Identifier(_) => {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                loc.dupe(),
                                UnsupportedSyntax::TSLibSyntax(
                                    TsLibSyntaxKind::ImportEqualsQualifiedName,
                                ),
                            ))),
                        );
                        {
                            let Ok(v) = polymorphic_ast_mapper::statement(
                                &mut typed_ast_utils::ErrorMapper,
                                stmt,
                            );
                            v
                        }
                    }
                }
            }
        }
    })
}

pub fn statement_list<'a>(
    cx: &Context<'a>,
    stmts: &[statement::Statement<ALoc, ALoc>],
) -> Vec<statement::Statement<ALoc, (ALoc, Type)>> {
    stmts
        .iter()
        .map(|stmt| {
            let (stmt, _abnormal) =
                flow_typing_utils::abnormal::catch_stmt_control_flow_exception(|| {
                    statement(cx, stmt)
                });
            stmt
        })
        .collect()
}

pub fn for_of_elemt<'a>(cx: &Context<'a>, right_t: Type, reason: Reason, await_: bool) -> Type {
    tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason.dupe(), move |cx, elem_t| {
        let loc = reason.loc().dupe();
        // Second and third args here are never relevant to the loop, but they should be as
        // general as possible to allow iterating over arbitrary generators
        let targs = vec![
            elem_t.dupe(),
            mixed_t::why(reason.dupe()),
            empty_t::why(reason.dupe()),
        ];
        type_operation_utils::type_assertions::assert_iterable(
            cx,
            loc,
            await_,
            &type_::unknown_use(),
            &right_t,
            &targs,
        );
    })
}

pub fn type_alias<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    alias: &statement::TypeAlias<ALoc, ALoc>,
) -> (Type, statement::TypeAlias<ALoc, (ALoc, Type)>) {
    let name_loc = alias.id.loc.dupe();
    let id = &alias.id;
    let name = &id.name;
    let cache = cx.node_cache();
    // | Some info ->
    if let Some(info) = cache.get_alias(&loc) {
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec![format!("Alias cache hit at {}", loc.debug_to_string(false))]
        });
        return info;
    }
    let r = type_::desc_format::type_reason(Name::new(name.dupe()), name_loc.dupe());
    let (tparams, tparams_map, tparams_ast) = type_annotation::mk_type_param_declarations(
        cx,
        flow_parser::ast_visitor::TypeParamsContext::TypeAlias,
        Some(FlowOrdMap::new()),
        alias.tparams.as_ref(),
    );
    let tparams_map_for_custom_error = tparams_map.dupe();
    let right_ast = type_annotation::convert(cx, tparams_map, &alias.right);
    let t = right_ast.loc().1.dupe();
    let name_clone = name.dupe();
    let name_loc_clone = name_loc.dupe();
    let normal_type_alias = || {
        let alias_t = mod_reason_of_t(
            &|reason: Reason| {
                reason.update_desc(|desc| {
                    VirtualReasonDesc::RTypeAlias(Box::new((
                        name_clone.dupe(),
                        Some(name_loc_clone.dupe()),
                        Arc::new(desc),
                    )))
                })
            },
            &t,
        );
        poly_type_of_tparams(
            type_::poly::Id::generate_id(),
            tparams.clone(),
            Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::TypeT(type_::TypeTKind::TypeAliasKind, alias_t)),
            )),
        )
    };
    let type_ = if cx.enable_custom_error() {
        match jsdoc::of_comments(alias.comments.as_ref()) {
            Some((custom_error_loc, ref jsdoc_val))
                if jsdoc_val
                    .unrecognized_tags()
                    .0
                    .iter()
                    .any(|(tag_name, _)| tag_name == "flowCustomError") =>
            {
                // Convert to opaque type with custom error variant
                let nominal_type_args: Vec<(SubstName, Reason, Type, Polarity)> = match &tparams {
                    Some((_, tps)) => tps
                        .iter()
                        .map(|tp| {
                            let tp_t = tparams_map_for_custom_error.get(&tp.name).unwrap().dupe();
                            (tp.name.dupe(), tp.reason.dupe(), tp_t, tp.polarity)
                        })
                        .collect(),
                    None => vec![],
                };
                let nominal_id = type_::nominal::Id::UserDefinedOpaqueTypeId(Box::new(
                    type_::nominal::UserDefinedOpaqueTypeIdData(
                        cx.make_aloc_id(&name_loc),
                        name.dupe(),
                    ),
                ));
                let nominal_type = type_::NominalType::new(type_::NominalTypeInner {
                    underlying_t: type_::nominal::UnderlyingT::CustomError(Box::new(
                        nominal::CustomErrorData {
                            custom_error_loc,
                            t: t.dupe(),
                        },
                    )),
                    lower_t: None,
                    upper_t: None,
                    nominal_id,
                    nominal_type_args: nominal_type_args.into(),
                });
                let nominal_t = Type::new(TypeInner::NominalT {
                    reason: r.dupe().update_desc(|desc| {
                        VirtualReasonDesc::RTypeAlias(Box::new((
                            name.dupe(),
                            Some(name_loc.dupe()),
                            Arc::new(desc),
                        )))
                    }),
                    nominal_type: Rc::new(nominal_type),
                });
                poly_type_of_tparams(
                    type_::poly::Id::generate_id(),
                    tparams.clone(),
                    Type::new(TypeInner::DefT(
                        r.dupe(),
                        DefT::new(DefTInner::TypeT(type_::TypeTKind::TypeAliasKind, nominal_t)),
                    )),
                )
            }
            _ => normal_type_alias(),
        }
    } else {
        normal_type_alias()
    };
    if let Some((_, tps)) = &tparams {
        // TODO: use tparams_map
        let tparams_map: std::collections::BTreeMap<
            flow_common::subst_name::SubstName,
            type_::TypeParam,
        > = tps
            .iter()
            .fold(std::collections::BTreeMap::new(), |mut acc, tp| {
                acc.insert(tp.name.dupe(), tp.dupe());
                acc
            });
        cx.add_post_inference_polarity_check(tparams_map, Polarity::Positive, t);
    }
    let type_alias_ast = statement::TypeAlias {
        id: ast::Identifier::new(ast::IdentifierInner {
            loc: (name_loc.dupe(), type_.dupe()),
            name: id.name.dupe(),
            comments: id.comments.dupe(),
        }),
        tparams: tparams_ast,
        right: right_ast,
        comments: alias.comments.dupe(),
    };
    (type_, type_alias_ast)
}

pub fn interface<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    decl: &statement::Interface<ALoc, ALoc>,
) -> (Type, statement::Interface<ALoc, (ALoc, Type)>) {
    let node_cache = cx.node_cache();
    if let Some(node) = node_cache.get_interface(&loc) {
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec![format!(
                "Interface cache hit at {}",
                loc.debug_to_string(false)
            )]
        });
        return node;
    }
    let name_loc = decl.id.loc.dupe();
    let id = &decl.id;
    let name = &id.name;
    let desc = VirtualReasonDesc::RType(Name::new(name.dupe()));
    let reason = mk_reason(desc.clone(), name_loc.dupe());
    let (t, iface_sig, decl_ast) = type_annotation::mk_interface_sig(cx, loc.dupe(), reason, decl);
    class_sig::check_signature_compatibility(cx, mk_reason(desc, loc), &iface_sig);
    (t, decl_ast)
}

fn declare_variable<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    decl: &statement::DeclareVariable<ALoc, ALoc>,
) -> Result<statement::DeclareVariable<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let statement::DeclareVariable {
        declarations,
        kind,
        comments,
    } = decl;
    // TSLib gates
    if !cx.tslib_syntax() {
        if declarations.len() > 1 {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    UnsupportedSyntax::TSLibSyntax(
                        TsLibSyntaxKind::DeclareVariableMultipleDeclarators,
                    ),
                ))),
            );
        }
        for d in declarations.iter() {
            match &d.init {
                Some(init_expr) => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            init_expr.loc().dupe(),
                            UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::DeclareVariableLiteralInit,
                            ),
                        ))),
                    );
                }
                None => {}
            }
        }
    }
    let declarations: Vec<_> = declarations
        .iter()
        .map(|d| -> Result<_, AbnormalControlFlow> {
            let decl_loc = d.loc.dupe();
            let id = &d.id;
            let init = &d.init;
            match id {
                ast::pattern::Pattern::Identifier {
                    loc: pat_loc,
                    inner: pat_id,
                } => {
                    let id_loc = &pat_id.name.loc;
                    let id_name = &pat_id.name;
                    let annot = &pat_id.annot;
                    match (annot, init) {
                        (ast::types::AnnotationOrHint::Available(annot), None) => {
                            let (t, annot_ast) = type_annotation::mk_type_available_annotation(
                                cx,
                                Default::default(),
                                annot,
                            );
                            let typed_id = ast::pattern::Pattern::Identifier {
                                loc: (pat_loc.dupe(), t.dupe()),
                                inner: ast::pattern::Identifier {
                                    name: ast::Identifier::new(ast::IdentifierInner {
                                        loc: (id_loc.dupe(), t.dupe()),
                                        name: id_name.name.dupe(),
                                        comments: id_name.comments.clone(),
                                    }),
                                    annot: ast::types::AnnotationOrHint::Available(annot_ast),
                                    optional: false,
                                }
                                .into(),
                            };
                            Ok(ast::statement::variable::Declarator {
                                loc: decl_loc,
                                id: typed_id,
                                init: None,
                            })
                        }
                        (ast::types::AnnotationOrHint::Missing(missing_loc), Some(init_expr)) => {
                            match &**init_expr {
                                ast::expression::ExpressionInner::StringLiteral { .. }
                                | ast::expression::ExpressionInner::NumberLiteral { .. }
                                | ast::expression::ExpressionInner::BigIntLiteral { .. }
                                | ast::expression::ExpressionInner::BooleanLiteral { .. } => {
                                    let init_ast =
                                        expression(None, None, Some(true), cx, init_expr)?;
                                    let t = init_ast.loc().1.dupe();
                                    let typed_id = ast::pattern::Pattern::Identifier {
                                        loc: (pat_loc.dupe(), t.dupe()),
                                        inner: ast::pattern::Identifier {
                                            name: ast::Identifier::new(ast::IdentifierInner {
                                                loc: (id_loc.dupe(), t.dupe()),
                                                name: id_name.name.dupe(),
                                                comments: id_name.comments.clone(),
                                            }),
                                            annot: ast::types::AnnotationOrHint::Missing((
                                                missing_loc.dupe(),
                                                t.dupe(),
                                            )),
                                            optional: false,
                                        }
                                        .into(),
                                    };
                                    Ok(ast::statement::variable::Declarator {
                                        loc: decl_loc,
                                        id: typed_id,
                                        init: Some(init_ast),
                                    })
                                }
                                _ => {
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            init_expr.loc().dupe(),
                                            UnsupportedSyntax::DeclareVariableNonLiteralInit,
                                        ))),
                                    );
                                    let Ok(v) = polymorphic_ast_mapper::declare_variable_declarator(
                                        &mut typed_ast_utils::ErrorMapper,
                                        *kind,
                                        d,
                                    );
                                    Ok(v)
                                }
                            }
                        }
                        (ast::types::AnnotationOrHint::Available(_), Some(init_expr)) => {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    init_expr.loc().dupe(),
                                    UnsupportedSyntax::DeclareVariableAnnotationAndInit,
                                ))),
                            );
                            let Ok(v) = polymorphic_ast_mapper::declare_variable_declarator(
                                &mut typed_ast_utils::ErrorMapper,
                                *kind,
                                d,
                            );
                            Ok(v)
                        }
                        (ast::types::AnnotationOrHint::Missing(_), None) => {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    id_loc.dupe(),
                                    UnsupportedSyntax::DeclareVariableMissingAnnotationOrInit,
                                ))),
                            );
                            let Ok(v) = polymorphic_ast_mapper::declare_variable_declarator(
                                &mut typed_ast_utils::ErrorMapper,
                                *kind,
                                d,
                            );
                            Ok(v)
                        }
                    }
                }
                _ => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            id.loc().dupe(),
                            UnsupportedSyntax::DeclareVariableDestructuring,
                        ))),
                    );
                    let Ok(v) = polymorphic_ast_mapper::declare_variable_declarator(
                        &mut typed_ast_utils::ErrorMapper,
                        *kind,
                        d,
                    );
                    Ok(v)
                }
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(statement::DeclareVariable {
        declarations: declarations.into(),
        kind: *kind,
        comments: comments.clone(),
    })
}

pub fn declare_class<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    decl: &statement::DeclareClass<ALoc, ALoc>,
) -> (Type, statement::DeclareClass<ALoc, (ALoc, Type)>) {
    let node_cache = cx.node_cache();
    if let Some(node) = node_cache.get_declared_class(&loc) {
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec![format!(
                "Declared class cache hit at {}",
                loc.debug_to_string(false)
            )]
        });
        return node;
    }
    let name_loc = decl.id.loc.dupe();
    let id = &decl.id;
    let name = &id.name;
    let abstract_ = decl.abstract_;
    if abstract_ {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                kind: TSSyntaxKind::AbstractClass,
                loc: loc.dupe(),
            })),
        );
    }
    let desc = VirtualReasonDesc::RType(Name::new(name.dupe()));
    let reason = mk_reason(desc.clone(), name_loc.dupe());
    let (t, class_sig, decl_ast) =
        type_annotation::mk_declare_class_sig(cx, loc.dupe(), name, reason, decl);
    class_sig::check_signature_compatibility(cx, mk_reason(desc, loc), &class_sig);
    (t, decl_ast)
}

fn validate_declare_component_params<'a>(
    cx: &Context<'a>,
    params: &statement::component_params::Params<ALoc, ALoc>,
) {
    let param_list = &params.params;
    let rest = &params.rest;
    // Validate each param
    for param in param_list.iter() {
        let loc = &param.loc;
        let name = &param.name;
        let local = &param.local;
        let default = &param.default;
        let shorthand = param.shorthand;
        // Check for default values - not allowed in declare component
        if default.is_some() {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EDeclareComponentInvalidParam {
                    loc: loc.dupe(),
                    kind: DeclareComponentInvalidParamKind::DeclareComponentParamDefaultValue,
                },
            );
        }
        // Check for as bindings and string literal params *)
        // - Identifier params with as bindings are not allowed *)
        // - String literal params without as bindings are required to have one
        match name {
            statement::component_params::ParamName::Identifier(_) if !shorthand => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDeclareComponentInvalidParam {
                        loc: loc.dupe(),
                        kind: DeclareComponentInvalidParamKind::DeclareComponentParamAsBinding,
                    },
                );
            }
            statement::component_params::ParamName::StringLiteral((name_loc, _)) if shorthand => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDeclareComponentInvalidParam {
                        loc: name_loc.dupe(),
                        kind: DeclareComponentInvalidParamKind::DeclareComponentParamStringLiteralWithoutAs,
                    },
                );
            }
            _ => {}
        }
        // Check for missing annotation
        if let ast::pattern::Pattern::Identifier { inner, .. } = local {
            if matches!(inner.annot, ast::types::AnnotationOrHint::Missing(_)) {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDeclareComponentInvalidParam {
                        loc: loc.dupe(),
                        kind:
                            DeclareComponentInvalidParamKind::DeclareComponentParamMissingAnnotation,
                    },
                );
            }
        }
    }
    // Validate rest param
    if let Some(rest_param) = rest {
        let loc = &rest_param.loc;
        if let ast::pattern::Pattern::Identifier { inner, .. } = &rest_param.argument {
            if matches!(inner.annot, ast::types::AnnotationOrHint::Missing(_)) {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDeclareComponentInvalidParam {
                        loc: loc.dupe(),
                        kind:
                            DeclareComponentInvalidParamKind::DeclareComponentParamMissingAnnotation,
                    },
                );
            }
        }
    }
}

pub fn declare_component<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    decl: &statement::DeclareComponent<ALoc, ALoc>,
) -> (Type, statement::DeclareComponent<ALoc, (ALoc, Type)>) {
    let node_cache = cx.node_cache();
    if let Some(node) = node_cache.get_declared_component(&loc) {
        flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
            vec![format!(
                "Declared component cache hit at {}",
                loc.debug_to_string(false)
            )]
        });
        return node;
    }
    let (t, decl_ast) = type_annotation::mk_declare_component_sig(cx, loc, decl);
    (t, decl_ast)
}

fn declare_module<'a>(
    cx: &Context<'a>,
    decl: &statement::DeclareModule<ALoc, ALoc>,
) -> statement::DeclareModule<ALoc, (ALoc, Type)> {
    let id_loc = match &decl.id {
        statement::declare_module::Id::Identifier(id) => id.loc.dupe(),
        statement::declare_module::Id::Literal((loc, _)) => loc.dupe(),
    };
    if !(cx.file().is_lib_file() && type_env::in_global_scope(cx)) {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EUnsupportedSyntax(Box::new((
                id_loc.dupe(),
                UnsupportedSyntax::ContextDependentUnsupportedStatement(
                    ContextDependentUnsupportedStatement::NonLibdefToplevelDeclareModule,
                ),
            ))),
        );
    }
    let (body_loc, body_block) = &decl.body;
    let elements = &body_block.body;
    let elements_comments = &body_block.comments;
    let prev_scope_kind = type_env::set_scope_kind(
        cx,
        flow_env_builder::name_def_types::ScopeKind::DeclareModule,
    );
    //   let elements_ast = statement_list cx elements in
    let elements_ast = statement_list(cx, elements);
    for stmt in elements_ast.iter() {
        let loc = stmt.loc();
        match flow_parser::ast_utils::acceptable_statement_in_declaration_context(false, stmt) {
            Ok(()) => {}
            Err(kind) => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::ContextDependentUnsupportedStatement(
                            ContextDependentUnsupportedStatement::UnsupportedStatementInDeclareModule(
                                FlowSmolStr::new(kind),
                            ),
                        ),
                    ))),
                );
            }
        }
    }
    let id = match &decl.id {
        statement::declare_module::Id::Identifier(ident) => {
            let id_loc = ident.loc.dupe();
            statement::declare_module::Id::Identifier(ast::Identifier::new(ast::IdentifierInner {
                loc: (id_loc.dupe(), mixed_t::at(id_loc)),
                name: ident.name.dupe(),
                comments: ident.comments.dupe(),
            }))
        }
        statement::declare_module::Id::Literal((lit_loc, lit)) => {
            statement::declare_module::Id::Literal((
                (lit_loc.dupe(), type_::str_module_t::at(lit_loc.dupe())),
                lit.clone(),
            ))
        }
    };
    let ast = statement::DeclareModule {
        id,
        body: (
            body_loc.dupe(),
            statement::Block {
                body: elements_ast.into(),
                comments: elements_comments.clone(),
            },
        ),
        comments: decl.comments.dupe(),
    };
    type_env::set_scope_kind(cx, prev_scope_kind);
    ast
}

pub fn declare_namespace<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    decl: &statement::DeclareNamespace<ALoc, ALoc>,
) -> (Type, statement::DeclareNamespace<ALoc, (ALoc, Type)>) {
    let statement::DeclareNamespace {
        id,
        body,
        comments,
        implicit_declare,
        keyword,
    } = decl;
    let (body_loc, body_block) = body;
    if *implicit_declare && !cx.tslib_syntax() {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EUnsupportedSyntax(Box::new((
                loc.dupe(),
                UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::DeclarationWithoutDeclare),
            ))),
        );
    }
    let node_cache = cx.node_cache();
    if let Some(x) = node_cache.get_declared_namespace(&loc) {
        return x;
    }
    let prev_scope_kind = type_env::set_scope_kind(
        cx,
        flow_env_builder::name_def_types::ScopeKind::DeclareModule,
    );
    let body_statements = statement_list(cx, &body_block.body);
    let body_comments = &body_block.comments;
    type_env::set_scope_kind(cx, prev_scope_kind);
    let body = (
        body_loc.dupe(),
        statement::Block {
            body: body_statements.clone().into(),
            comments: body_comments.clone(),
        },
    );
    let (t, id) = match id {
        statement::declare_namespace::Id::Global(ident) => {
            let ast::IdentifierInner {
                loc: name_loc,
                name,
                ..
            } = &**ident;
            let reason = mk_reason(VirtualReasonDesc::RNamespace(name.dupe()), name_loc.dupe());
            let namespace_symbol =
                flow_common::flow_symbol::Symbol::mk_namespace_symbol(name.dupe(), name_loc.dupe());
            let t = crate::module_info_analyzer::analyze_declare_namespace(
                cx,
                namespace_symbol,
                reason,
                &body_statements,
            );
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    name_loc.dupe(),
                    UnsupportedSyntax::DeclareGlobal,
                ))),
            );
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUndocumentedFeature {
                    loc: name_loc.dupe(),
                },
            );
            (t, statement::declare_namespace::Id::Global(ident.clone()))
        }
        statement::declare_namespace::Id::Local(ident) => {
            let ast::IdentifierInner {
                loc: name_loc,
                name,
                comments: id_comments,
            } = &**ident;
            let reason = mk_reason(VirtualReasonDesc::RNamespace(name.dupe()), name_loc.dupe());
            let namespace_symbol =
                flow_common::flow_symbol::Symbol::mk_namespace_symbol(name.dupe(), name_loc.dupe());
            let t = crate::module_info_analyzer::analyze_declare_namespace(
                cx,
                namespace_symbol,
                reason,
                &body_statements,
            );
            (
                t.dupe(),
                statement::declare_namespace::Id::Local(ast::Identifier::new(
                    ast::IdentifierInner {
                        loc: (name_loc.dupe(), t),
                        name: name.dupe(),
                        comments: id_comments.clone(),
                    },
                )),
            )
        }
    };
    (
        t,
        statement::DeclareNamespace {
            id,
            body,
            comments: comments.clone(),
            implicit_declare: *implicit_declare,
            keyword: *keyword,
        },
    )
}

fn object_prop<'a>(
    cx: &Context<'a>,
    as_const: bool,
    frozen: bool,
    has_hint: LazyBool<'a>,
    acc: ObjectExpressionAcc,
    prop: &expression::object::Property<ALoc, ALoc>,
) -> Result<
    (
        ObjectExpressionAcc,
        expression::object::Property<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    use ast::expression::object::Key;
    use ast::expression::object::NormalProperty;
    use ast::expression::object::Property;

    Ok(match prop {
        // named prop
        Property::NormalProperty(NormalProperty::Init {
            loc: prop_loc,
            key,
            value: v,
            shorthand,
        }) => {
            let loc = match key {
                Key::Identifier(id) => id.loc.dupe(),
                Key::StringLiteral((loc, _))
                | Key::NumberLiteral((loc, _))
                | Key::BigIntLiteral((loc, _)) => loc.dupe(),
                Key::Computed(_) => {
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    return Ok((acc, mapped));
                }
                Key::PrivateName(_) => {
                    panic!("Internal Error: Non-private field with private name")
                }
            };
            match name_of_identifier_or_literal_key(key) {
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    (acc, mapped)
                }
                Ok(name) => {
                    let (acc, key, value) = {
                        let frozen = if frozen {
                            FrozenKind::FrozenProp
                        } else {
                            FrozenKind::NotFrozen
                        };
                        if type_inference_hooks_js::dispatch_obj_prop_decl_hook(
                            cx,
                            &name,
                            loc.dupe(),
                        ) {
                            let t = type_::unsoundness::at(
                                type_::UnsoundnessKind::InferenceHooks,
                                loc.dupe(),
                            );
                            let typed_key = translate_identifier_or_literal_key(t, key);
                            // don't add `name` to `acc` because `name` is the autocomplete token
                            let acc = acc.set_obj_key_autocomplete();
                            let value = expression_inner(
                                None,
                                None,
                                Some(as_const),
                                frozen,
                                Some(has_hint.clone()),
                                cx,
                                v,
                            )?;
                            (acc, typed_key, value)
                        } else {
                            let value = expression_inner(
                                None,
                                None,
                                Some(as_const),
                                frozen,
                                Some(has_hint.clone()),
                                cx,
                                v,
                            )?;
                            let (_, t) = value.loc();
                            let typed_key = translate_identifier_or_literal_key(t.dupe(), key);
                            let polarity = Polarity::object_literal_polarity(
                                as_const || frozen == FrozenKind::FrozenProp,
                            );
                            let t_clone = t.dupe();
                            let acc = acc.add_prop(|pmap| {
                                pmap.insert(
                                    Name::new(name.dupe()),
                                    type_::Property::new(type_::PropertyInner::Field(Box::new(
                                        FieldData {
                                            preferred_def_locs: None,
                                            key_loc: Some(loc.dupe()),
                                            type_: t_clone,
                                            polarity,
                                        },
                                    ))),
                                );
                            });
                            (acc, typed_key, value)
                        }
                    };
                    (
                        acc,
                        Property::NormalProperty(NormalProperty::Init {
                            loc: prop_loc.dupe(),
                            key,
                            value,
                            shorthand: *shorthand,
                        }),
                    )
                }
            }
        }
        // named method
        Property::NormalProperty(NormalProperty::Method {
            loc: prop_loc,
            key,
            value: (fn_loc, func),
        }) => {
            let loc = match key {
                Key::Identifier(id) => id.loc.dupe(),
                Key::StringLiteral((loc, _))
                | Key::NumberLiteral((loc, _))
                | Key::BigIntLiteral((loc, _)) => loc.dupe(),
                // computed LHS silently ignored for now
                Key::Computed(_) => {
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    return Ok((acc, mapped));
                }
                Key::PrivateName(_) => {
                    panic!("Internal Error: Non-private field with private name")
                }
            };
            match name_of_identifier_or_literal_key(key) {
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    (acc, mapped)
                }
                Ok(name) => {
                    let reason = func_reason(false, false, prop_loc.dupe());
                    let (t, typed_func) =
                        mk_function_expression(cx, false, reason, fn_loc.dupe(), func)?;
                    let t_clone = t.dupe();
                    let acc = acc.add_prop(|pmap| {
                        pmap.insert(
                            Name::new(name.dupe()),
                            type_::Property::new(type_::PropertyInner::Method {
                                key_loc: Some(loc.dupe()),
                                type_: t_clone,
                            }),
                        );
                    });
                    (
                        acc,
                        Property::NormalProperty(NormalProperty::Method {
                            loc: prop_loc.dupe(),
                            key: translate_identifier_or_literal_key(t, key),
                            value: (fn_loc.dupe(), typed_func),
                        }),
                    )
                }
            }
        }
        // We enable some unsafe support for getters and setters. The main unsafe bit
        // is that we don't properly havok refinements when getter and setter methods
        // are called.
        // unsafe getter property
        Property::NormalProperty(NormalProperty::Get {
            loc: prop_loc,
            key,
            value: (vloc, func),
            comments,
        }) => {
            let id_loc = match key {
                Key::Identifier(id) => id.loc.dupe(),
                Key::StringLiteral((loc, _))
                | Key::NumberLiteral((loc, _))
                | Key::BigIntLiteral((loc, _)) => loc.dupe(),
                // computed getters and setters aren't supported yet
                Key::Computed(_) => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            prop_loc.dupe(),
                            UnsupportedSyntax::ObjectPropertyComputedGetSet,
                        ))),
                    );
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    return Ok((acc, mapped));
                }
                Key::PrivateName(_) => {
                    panic!("Internal Error: Non-private field with private name")
                }
            };
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsafeGettersSetters(prop_loc.dupe()),
            );
            match name_of_identifier_or_literal_key(key) {
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    (acc, mapped)
                }
                Ok(name) => {
                    let reason = func_reason(false, false, vloc.dupe());
                    let (function_type, typed_func) =
                        mk_function_expression(cx, false, reason, vloc.dupe(), func)?;
                    let return_t = type_::extract_getter_type(&function_type);
                    let return_t_clone = return_t.dupe();
                    let acc = acc.add_prop(|pmap| {
                        let prop_name = Name::new(name.dupe());
                        let new_prop = match pmap.get(&prop_name).map(|p| p.deref()) {
                            Some(type_::PropertyInner::Set {
                                key_loc: set_key_loc,
                                type_: set_type,
                            }) => type_::Property::new(type_::PropertyInner::GetSet(Box::new(
                                GetSetData {
                                    get_key_loc: Some(id_loc.dupe()),
                                    get_type: return_t_clone,
                                    set_key_loc: set_key_loc.dupe(),
                                    set_type: set_type.dupe(),
                                },
                            ))),
                            _ => type_::Property::new(type_::PropertyInner::Get {
                                key_loc: Some(id_loc.dupe()),
                                type_: return_t_clone,
                            }),
                        };
                        pmap.insert(prop_name, new_prop);
                    });
                    (
                        acc,
                        Property::NormalProperty(NormalProperty::Get {
                            loc: prop_loc.dupe(),
                            key: translate_identifier_or_literal_key(return_t, key),
                            value: (vloc.dupe(), typed_func),
                            comments: comments.clone(),
                        }),
                    )
                }
            }
        }
        // unsafe setter property
        Property::NormalProperty(NormalProperty::Set {
            loc: prop_loc,
            key,
            value: (vloc, func),
            comments,
        }) => {
            let id_loc = match key {
                Key::Identifier(id) => id.loc.dupe(),
                Key::StringLiteral((loc, _))
                | Key::NumberLiteral((loc, _))
                | Key::BigIntLiteral((loc, _)) => loc.dupe(),
                // computed getters and setters aren't supported yet
                Key::Computed(_) => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            prop_loc.dupe(),
                            UnsupportedSyntax::ObjectPropertyComputedGetSet,
                        ))),
                    );
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    return Ok((acc, mapped));
                }
                Key::PrivateName(_) => {
                    panic!("Internal Error: Non-private field with private name")
                }
            };
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsafeGettersSetters(prop_loc.dupe()),
            );
            match name_of_identifier_or_literal_key(key) {
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                    let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    (acc, mapped)
                }
                Ok(name) => {
                    let reason = func_reason(false, false, vloc.dupe());
                    let (function_type, typed_func) =
                        mk_function_expression(cx, false, reason, vloc.dupe(), func)?;
                    let param_t = type_::extract_setter_type(&function_type);
                    let param_t_clone = param_t.dupe();
                    let acc = acc.add_prop(|pmap| {
                        let prop_name = Name::new(name.dupe());
                        let new_prop = match pmap.get(&prop_name).map(|p| p.deref()) {
                            Some(type_::PropertyInner::Get {
                                key_loc: get_key_loc,
                                type_: get_type,
                            }) => type_::Property::new(type_::PropertyInner::GetSet(Box::new(
                                GetSetData {
                                    get_key_loc: get_key_loc.dupe(),
                                    get_type: get_type.dupe(),
                                    set_key_loc: Some(id_loc.dupe()),
                                    set_type: param_t_clone,
                                },
                            ))),
                            _ => type_::Property::new(type_::PropertyInner::Set {
                                key_loc: Some(id_loc.dupe()),
                                type_: param_t_clone,
                            }),
                        };
                        pmap.insert(prop_name, new_prop);
                    });
                    (
                        acc,
                        Property::NormalProperty(NormalProperty::Set {
                            loc: prop_loc.dupe(),
                            key: translate_identifier_or_literal_key(param_t, key),
                            value: (vloc.dupe(), typed_func),
                            comments: comments.clone(),
                        }),
                    )
                }
            }
        }
        // spread prop
        Property::SpreadProperty(_) => {
            let Ok(mapped) = polymorphic_ast_mapper::object_property_or_spread_property(
                &mut typed_ast_utils::ErrorMapper,
                prop,
            );
            (acc, mapped)
        }
    })
}

fn prop_map_of_object<'a>(
    cx: &Context<'a>,
    props: &[expression::object::Property<ALoc, ALoc>],
) -> Result<
    (
        properties::PropertiesMap,
        Vec<expression::object::Property<ALoc, (ALoc, Type)>>,
    ),
    AbnormalControlFlow,
> {
    let (acc, prop_asts) = props.iter().try_fold(
        (ObjectExpressionAcc::empty(), vec![]),
        |(map, mut prop_asts), prop| {
            let (map, typed_prop) = object_prop(
                cx,
                false,
                false,
                Rc::new(flow_lazy::Lazy::new_forced(false)),
                map,
                prop,
            )?;
            prop_asts.push(typed_prop);
            Ok((map, prop_asts))
        },
    )?;
    Ok((acc.obj_pmap, prop_asts))
}

fn create_computed_prop<'a>(
    cx: &Context<'a>,
    key_loc: ALoc,
    unconcretized_key: Type,
    concretized_keys: Vec<Type>,
    reason: Reason,
    reason_key: Reason,
    reason_obj: Reason,
    as_const: bool,
    frozen: bool,
    value: (ALoc, Type),
) -> object_expression_acc::ComputedProp {
    let valid_computed_key = |key: &Type| -> bool {
        match key.deref() {
            TypeInner::DefT(_, def) => matches!(
                def.deref(),
                DefTInner::StrGeneralT(_) | DefTInner::NumGeneralT(_) | DefTInner::EnumValueT(_)
            ),
            TypeInner::StrUtilT { .. } => true,
            TypeInner::AnyT(_, _) => true,
            _ => false,
        }
    };
    let single_key = |key: Type| -> object_expression_acc::ComputedProp {
        match flow_js_utils::propref_for_elem_t(cx, &key) {
            PropRef::Computed(key) => {
                if valid_computed_key(&key) {
                    object_expression_acc::ComputedProp::NonLiteralKey {
                        key_loc: key_loc.dupe(),
                        key,
                        value: value.clone(),
                        named_set_opt: None,
                    }
                } else {
                    match key.deref() {
                        TypeInner::NominalT {
                            reason: nom_reason,
                            nominal_type,
                        } if let nominal::UnderlyingT::OpaqueWithLocal { t: inner_key, .. } =
                            &nominal_type.underlying_t
                            && matches!(
                                &nominal_type.nominal_id,
                                nominal::Id::UserDefinedOpaqueTypeId(_)
                            )
                            && nom_reason.loc().source() == nom_reason.def_loc().source() =>
                        {
                            if valid_computed_key(inner_key) {
                                return object_expression_acc::ComputedProp::NonLiteralKey {
                                    key_loc: key_loc.dupe(),
                                    key,
                                    value: value.clone(),
                                    named_set_opt: None,
                                };
                            }
                            // Fallthrough to error case
                            let r = reason_of_t(&key).dupe();
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EObjectComputedPropertyAssign(Box::new((
                                    r,
                                    Some(reason_key.dupe()),
                                    intermediate_error_types::InvalidObjKey::Other,
                                ))),
                            );
                            object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey
                        }
                        TypeInner::NominalT { nominal_type, .. }
                            if let nominal::UnderlyingT::CustomError(
                                box nominal::CustomErrorData { t: inner_key, .. },
                            ) = &nominal_type.underlying_t
                                && matches!(
                                    &nominal_type.nominal_id,
                                    nominal::Id::UserDefinedOpaqueTypeId(_)
                                ) =>
                        {
                            if valid_computed_key(inner_key) {
                                return object_expression_acc::ComputedProp::NonLiteralKey {
                                    key_loc: key_loc.dupe(),
                                    key,
                                    value: value.clone(),
                                    named_set_opt: None,
                                };
                            }
                            let r = reason_of_t(&key).dupe();
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EObjectComputedPropertyAssign(Box::new((
                                    r,
                                    Some(reason_key.dupe()),
                                    intermediate_error_types::InvalidObjKey::Other,
                                ))),
                            );
                            object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey
                        }
                        TypeInner::NominalT { nominal_type, .. }
                            if let Some(upper_t) = &nominal_type.upper_t
                                && matches!(
                                    &nominal_type.nominal_id,
                                    nominal::Id::UserDefinedOpaqueTypeId(_)
                                )
                                && valid_computed_key(upper_t) =>
                        {
                            object_expression_acc::ComputedProp::NonLiteralKey {
                                key_loc: key_loc.dupe(),
                                key,
                                value: value.clone(),
                                named_set_opt: None,
                            }
                        }
                        TypeInner::DefT(singleton_reason, def)
                            if let DefTInner::SingletonNumT { value: num_lit, .. } =
                                def.deref() =>
                        {
                            let kind = intermediate_error_types::InvalidObjKey::kind_of_num_value(
                                num_lit.0,
                            );
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EObjectComputedPropertyAssign(Box::new((
                                    reason.dupe(),
                                    Some(singleton_reason.dupe()),
                                    kind,
                                ))),
                            );
                            object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey
                        }
                        _ => {
                            let r = reason_of_t(&key).dupe();
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EObjectComputedPropertyAssign(Box::new((
                                    r,
                                    Some(reason_key.dupe()),
                                    intermediate_error_types::InvalidObjKey::Other,
                                ))),
                            );
                            object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey
                        }
                    }
                }
            }
            PropRef::Named { name, .. } => {
                let prop = Property::new(PropertyInner::Field(Box::new(FieldData {
                    preferred_def_locs: None,
                    key_loc: Some(key_loc.dupe()),
                    type_: value.1.clone(),
                    polarity: Polarity::object_literal_polarity(as_const || frozen),
                })));
                object_expression_acc::ComputedProp::Named { name, prop }
            }
        }
    };
    match concretized_keys.len() {
        0 => object_expression_acc::ComputedProp::SpreadEmpty(reason_obj),
        1 => {
            let key = concretized_keys.into_iter().next().unwrap();
            single_key(key)
        }
        _ => {
            let mut computed_key_status: Result<Option<BTreeSet<Name>>, ()> =
                Ok(Some(BTreeSet::new()));
            for k in concretized_keys {
                match &mut computed_key_status {
                    Err(()) => break,
                    Ok(acc) => {
                        let result = single_key(k);
                        match result {
                            object_expression_acc::ComputedProp::Named { name, .. } => match acc {
                                None => {
                                    computed_key_status = Ok(None);
                                }
                                Some(named) => {
                                    named.insert(name);
                                }
                            },
                            object_expression_acc::ComputedProp::NonLiteralKey { .. }
                            | object_expression_acc::ComputedProp::SpreadEmpty(_) => {
                                computed_key_status = Ok(None);
                            }
                            object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey => {
                                computed_key_status = Err(());
                            }
                        }
                    }
                }
            }
            match computed_key_status {
                Err(()) => object_expression_acc::ComputedProp::IgnoredInvalidNonLiteralKey,
                Ok(named_set_opt) => object_expression_acc::ComputedProp::NonLiteralKey {
                    key_loc,
                    key: unconcretized_key,
                    value,
                    named_set_opt,
                },
            }
        }
    }
}

fn object_<'a>(
    cx: &Context<'a>,
    frozen: bool,
    as_const: bool,
    has_hint: LazyBool<'a>,
    loc: ALoc,
    props: &[expression::object::Property<ALoc, ALoc>],
) -> Result<(Type, Vec<expression::object::Property<ALoc, (ALoc, Type)>>), AbnormalControlFlow> {
    use ast::expression::object::Key;
    use ast::expression::object::NormalProperty;
    use ast::expression::object::Property;
    use ast::expression::object::SpreadProperty;

    error_on_this_uses_in_object_methods(cx, props);
    let has_hint: LazyBool<'a> = {
        let has_hint = has_hint.dupe();
        let loc = loc.dupe();
        Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
            *has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &loc)
        })))
    };
    let reason = mk_obj_lit_reason(as_const, frozen, || *has_hint.get_forced(cx), loc.dupe());
    // Use the same reason for proto and the ObjT so we can walk the proto chain
    // and use the root proto reason to build an error.
    let obj_proto = Type::new(TypeInner::ObjProtoT(reason.dupe()));
    let mk_computed =
        |k_expr: &expression::Expression<ALoc, (ALoc, Type)>, key: Type, value: (ALoc, Type)| {
            let key_loc = k_expr.loc().0.dupe();
            let concretized_keys =
                FlowJs::possible_concrete_types_for_computed_object_keys(cx, &reason, &key)
                    .expect("Should not be under speculation");
            let key_reason = reason_of_t(&key).dupe();
            let reason_key = mk_expression_reason(k_expr).map_locs(|loc| loc.0.dupe());
            let reason_obj = key_reason.dupe();
            create_computed_prop(
                cx,
                key_loc,
                key,
                concretized_keys,
                key_reason,
                reason_key,
                reason_obj,
                as_const,
                frozen,
                value,
            )
        };
    let (acc, prop_asts) = props.iter().try_fold(
        (ObjectExpressionAcc::empty(), vec![]),
        |(acc, mut prop_asts), prop| -> Result<_, AbnormalControlFlow> {
            match prop {
                Property::SpreadProperty(SpreadProperty {
                    loc: prop_loc,
                    argument,
                    comments,
                }) => {
                    let frozen = if frozen {
                        FrozenKind::FrozenDirect
                    } else {
                        FrozenKind::NotFrozen
                    };
                    let typed_argument =
                        expression_inner(None, None, Some(as_const), frozen, None, cx, argument)?;
                    let (_, spread) = typed_argument.loc();
                    let acc = acc.add_spread(spread.clone());
                    prop_asts.push(Property::SpreadProperty(SpreadProperty {
                        loc: prop_loc.dupe(),
                        argument: typed_argument,
                        comments: comments.clone(),
                    }));
                    Ok((acc, prop_asts))
                }
                Property::NormalProperty(NormalProperty::Init {
                    loc: prop_loc,
                    key: Key::Computed(ck),
                    value: v,
                    shorthand,
                }) => {
                    let k_typed = expression(
                        Some(EnclosingContext::IndexContext),
                        None,
                        None,
                        cx,
                        &ck.expression,
                    )?;
                    let (_, kt) = k_typed.loc();
                    let v_typed = expression_inner(
                        None,
                        None,
                        Some(as_const),
                        FrozenKind::NotFrozen,
                        Some(has_hint.clone()),
                        cx,
                        v,
                    )?;
                    let vt = v_typed.loc().clone();
                    let computed = mk_computed(&k_typed, kt.dupe(), vt);
                    let acc = acc.add_computed(cx, computed);
                    prop_asts.push(Property::NormalProperty(NormalProperty::Init {
                        loc: prop_loc.dupe(),
                        key: Key::Computed(ast::ComputedKey {
                            loc: ck.loc.dupe(),
                            expression: k_typed,
                            comments: ck.comments.dupe(),
                        }),
                        value: v_typed,
                        shorthand: *shorthand,
                    }));
                    Ok((acc, prop_asts))
                }
                Property::NormalProperty(NormalProperty::Method {
                    loc: prop_loc,
                    key: Key::Computed(ck),
                    value: (fn_loc, func),
                }) => {
                    let k_typed = expression(
                        Some(EnclosingContext::IndexContext),
                        None,
                        None,
                        cx,
                        &ck.expression,
                    )?;
                    let (_, kt) = k_typed.loc();
                    let tmp_expr =
                        expression::Expression::new(expression::ExpressionInner::Function {
                            loc: fn_loc.dupe(),
                            inner: std::sync::Arc::new(func.clone()),
                        });
                    let typed_tmp = expression(None, None, None, cx, &tmp_expr)?;
                    let vt = typed_tmp.loc().clone();
                    let typed_func = match &*typed_tmp {
                        expression::ExpressionInner::Function { inner, .. } => (**inner).clone(),
                        _ => panic!("Expected Function expression"),
                    };
                    let computed = mk_computed(&k_typed, kt.dupe(), vt);
                    let acc = acc.add_computed(cx, computed);
                    prop_asts.push(Property::NormalProperty(NormalProperty::Method {
                        loc: prop_loc.dupe(),
                        key: Key::Computed(ast::ComputedKey {
                            loc: ck.loc.dupe(),
                            expression: k_typed,
                            comments: ck.comments.dupe(),
                        }),
                        value: (fn_loc.dupe(), typed_func),
                    }));
                    Ok((acc, prop_asts))
                }
                Property::NormalProperty(NormalProperty::Init {
                    loc: prop_loc,
                    key,
                    value: v,
                    shorthand: false,
                }) if matches!(
                    key,
                    Key::Identifier(id) if id.name.as_str() == "__proto__"
                ) || matches!(
                    key,
                    Key::StringLiteral((_, lit)) if lit.value.as_str() == "__proto__"
                ) =>
                {
                    let v_loc = v.loc().dupe();
                    let reason = mk_reason(VirtualReasonDesc::RPrototype, v_loc);
                    let v_typed = expression(None, None, Some(as_const), cx, v)?;
                    let (_, vt) = v_typed.loc();
                    let vt_clone = vt.dupe();
                    let reason_clone = reason.dupe();
                    let t = tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason, |cx, t| {
                        flow_js::flow_non_speculating(
                            cx,
                            (
                                &vt_clone,
                                &UseT::new(UseTInner::ObjTestProtoT(reason_clone.dupe(), t.dupe())),
                            ),
                        );
                    });
                    let acc = acc.add_proto(t);
                    prop_asts.push(Property::NormalProperty(NormalProperty::Init {
                        loc: prop_loc.dupe(),
                        key: translate_identifier_or_literal_key(vt.dupe(), key),
                        value: v_typed,
                        shorthand: false,
                    }));
                    Ok((acc, prop_asts))
                }
                prop => {
                    let (acc, typed_prop) =
                        object_prop(cx, as_const, frozen, has_hint.dupe(), acc, prop)?;
                    prop_asts.push(typed_prop);
                    Ok((acc, prop_asts))
                }
            }
        },
    )?;
    let t = acc.mk_object_from_spread_acc(cx, reason, as_const, frozen, obj_proto);
    Ok((t, prop_asts))
}

fn init_var(kind: ast::VariableKind) -> for<'cx> fn(&Context<'cx>, &UseOp, &Type, ALoc) {
    match kind {
        ast::VariableKind::Const => type_env::init_const,
        ast::VariableKind::Let => type_env::init_let,
        ast::VariableKind::Var => type_env::init_var,
    }
}

fn variable<'a>(
    cx: &Context<'a>,
    kind: ast::VariableKind,
    if_uninitialized: Option<&dyn Fn(ALoc) -> Type>,
    id: &ast::pattern::Pattern<ALoc, ALoc>,
    init: Option<&expression::Expression<ALoc, ALoc>>,
) -> Result<
    (
        ast::pattern::Pattern<ALoc, (ALoc, Type)>,
        Option<expression::Expression<ALoc, (ALoc, Type)>>,
    ),
    AbnormalControlFlow,
> {
    let init_var = init_var(kind);
    let annot = crate::destructuring::type_of_pattern(id);
    let has_anno = match &annot {
        ast::types::AnnotationOrHint::Missing(_) => false,
        ast::types::AnnotationOrHint::Available(_) => true,
    };
    let id_reason = match id {
        ast::pattern::Pattern::Identifier { inner, .. } => {
            let id_loc = inner.name.loc.dupe();
            let name = &inner.name.name;
            mk_reason(
                VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                id_loc,
            )
        }
        _ => {
            let ploc = id.loc().dupe();
            mk_reason(VirtualReasonDesc::RDestructuring, ploc)
        }
    };
    // Identifiers do not need to be initialized at the declaration site as long
    // as they are definitely initialized before use. Destructuring patterns must
    // be initialized, since their declaration involves some operation on the
    // right hand side, like a property access.
    let (init_opt, init_ast): (
        Option<(Type, Reason)>,
        Option<expression::Expression<ALoc, (ALoc, Type)>>,
    ) = match (id, init, &if_uninitialized) {
        (ast::pattern::Pattern::Identifier { .. }, None, None) => (None, None),
        (_, Some(expr), _) => {
            let init_ast = expression(None, None, None, cx, expr)?;
            let t = init_ast.loc().1.dupe();
            let r = mk_expression_reason(expr);
            (Some((t, r)), Some(init_ast))
        }
        (_, None, Some(f)) => {
            let ploc = id.loc().dupe();
            let t = f(ploc);
            let r = reason_of_t(&t).dupe();
            (Some((t, r)), None)
        }
        (_, None, None) => {
            let ploc = id.loc().dupe();
            let t = type_::void::at(ploc);
            let r = reason_of_t(&t).dupe();
            (Some((t, r)), None)
        }
    };

    let id_ast = match id {
        ast::pattern::Pattern::Identifier { loc: ploc, inner } => {
            let id_loc = inner.name.loc.dupe();
            let name = &inner.name.name;
            let comments = &inner.name.comments;
            let optional = inner.optional;
            let (annot_t, annot_ast) = match &annot {
                ast::types::AnnotationOrHint::Missing(loc) => {
                    // When there is no annotation, we still need to populate `annot_t` when we can.
                    // In this case, we unify it with the type of the env entry binding, if there is one
                    let t = if init_ast.is_some() || if_uninitialized.is_some() {
                        type_env::read_declared_type(cx, id_reason.dupe(), id_loc.dupe())
                    } else {
                        empty_t::at(id_loc.dupe())
                    };
                    (
                        t.dupe(),
                        ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                    )
                }
                ast::types::AnnotationOrHint::Available(avail_annot) => {
                    let (t, ast_annot) = type_annotation::mk_type_available_annotation(
                        cx,
                        Default::default(),
                        avail_annot,
                    );
                    (t, ast::types::AnnotationOrHint::Available(ast_annot))
                }
            };
            if let Some((ref init_t, ref init_reason)) = init_opt {
                let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                    var: Some(id_reason.dupe()),
                    init: init_reason.dupe(),
                }));
                init_var(cx, &use_op, init_t, id_loc.dupe());
            }
            let ast_t = type_env::constraining_type(annot_t, cx, name, id_loc.dupe());
            ast::pattern::Pattern::Identifier {
                loc: (ploc.dupe(), ast_t.dupe()),
                inner: (ast::pattern::Identifier {
                    name: ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_loc, ast_t),
                        name: name.dupe(),
                        comments: comments.clone(),
                    }),
                    annot: annot_ast,
                    optional,
                })
                .into(),
            }
        }
        _ => {
            match &annot {
                ast::types::AnnotationOrHint::Missing(_) => {}
                ast::types::AnnotationOrHint::Available(avail_annot) => {
                    let (annot_t, _) = type_annotation::mk_type_available_annotation(
                        cx,
                        Default::default(),
                        avail_annot,
                    );
                    if let Some((ref init_t, ref init_reason)) = init_opt {
                        let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                            var: Some(id_reason.dupe()),
                            init: init_reason.dupe(),
                        }));
                        flow_js::flow_non_speculating(
                            cx,
                            (init_t, &UseT::new(UseTInner::UseT(use_op, annot_t))),
                        );
                    }
                }
            }
            let mut dest_init = crate::destructuring::empty(init.cloned(), None);
            crate::destructuring::pattern(
                cx,
                &|use_op: &UseOp,
                  name_loc: ALoc,
                  name: &FlowSmolStr,
                  default: Option<&flow_typing_default::Default<Type>>,
                  t: Type| {
                    let reason = mk_reason(
                        VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                        name_loc.dupe(),
                    );
                    // If this is a variable declaration without a type annotation
                    // constraining writes, we need the type of the identifier to be the
                    // general type of the variable in order to detect if a generic escapes
                    // into it.
                    //
                    // If there is an annotation, the specific and the general will be
                    // unified.
                    let id_node_type = if has_anno {
                        t.dupe()
                    } else {
                        init_var(cx, use_op, &t, name_loc.dupe());
                        type_env::constraining_type(
                            type_env::get_var_declared_type(
                                None,
                                None,
                                cx,
                                Name::new(name.dupe()),
                                name_loc.dupe(),
                            ),
                            cx,
                            name,
                            name_loc.dupe(),
                        )
                    };
                    if let Some(d) = default {
                        let default_t = flow_js::mk_default_non_speculating(cx, &reason, d);
                        flow_js::flow_non_speculating(
                            cx,
                            (&default_t, &UseT::new(UseTInner::UseT(use_op.dupe(), t))),
                        );
                    }
                    id_node_type
                },
                &mut dest_init,
                id,
            )?
        }
    };
    Ok((id_ast, init_ast))
}

pub fn expression_or_spread<'a>(
    cx: &Context<'a>,
    expr: &expression::ExpressionOrSpread<ALoc, ALoc>,
) -> Result<(CallArg, expression::ExpressionOrSpread<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    use ast::expression::ExpressionOrSpread;
    Ok(match expr {
        ExpressionOrSpread::Expression(e) => {
            let e_prime = expression(None, None, None, cx, e)?;
            let (_, t) = e_prime.loc();
            let t = t.dupe();
            (CallArg::arg(t), ExpressionOrSpread::Expression(e_prime))
        }
        ExpressionOrSpread::Spread(expression::SpreadElement {
            loc,
            argument,
            comments,
        }) => {
            let e_prime = expression(None, None, None, cx, argument)?;
            let (_, t) = e_prime.loc();
            let t = t.dupe();
            (
                CallArg::spread_arg(t),
                ExpressionOrSpread::Spread(expression::SpreadElement {
                    loc: loc.dupe(),
                    argument: e_prime,
                    comments: comments.clone(),
                }),
            )
        }
    })
}

fn array_elements<'a>(
    cx: &Context<'a>,
    as_const: bool,
    has_hint: LazyBool<'a>,
    elements: &[expression::ArrayElement<ALoc, ALoc>],
) -> Result<
    (
        Vec<type_::UnresolvedParam>,
        Vec<expression::ArrayElement<ALoc, (ALoc, Type)>>,
    ),
    AbnormalControlFlow,
> {
    use ast::expression::ArrayElement;
    let results: Vec<(type_::UnresolvedParam, ArrayElement<ALoc, (ALoc, Type)>)> = elements
        .iter()
        .map(|e| match e {
            ArrayElement::Expression(e) => {
                let e_prime = expression_inner(
                    None,
                    None,
                    Some(as_const),
                    FrozenKind::NotFrozen,
                    Some(has_hint.clone()),
                    cx,
                    e,
                )?;
                let (loc, t) = e_prime.loc();
                let loc = loc.dupe();
                let t = t.dupe();
                let reason = mk_reason(VirtualReasonDesc::RArrayElement, loc);
                let elem = mk_tuple_element(reason, t.dupe(), None, false, Polarity::Neutral);
                Ok((
                    type_::UnresolvedParam::UnresolvedArg(Box::new(type_::UnresolvedArgData(
                        elem, None,
                    ))),
                    ArrayElement::Expression(e_prime),
                ))
            }
            ArrayElement::Hole(loc) => {
                let t = type_::void::make(mk_reason(VirtualReasonDesc::RArrayHole, loc.dupe()));
                let reason = mk_reason(VirtualReasonDesc::RArrayElement, loc.dupe());
                let elem = mk_tuple_element(reason, t, None, false, Polarity::Neutral);
                Ok((
                    type_::UnresolvedParam::UnresolvedArg(Box::new(type_::UnresolvedArgData(
                        elem, None,
                    ))),
                    ArrayElement::Hole(loc.dupe()),
                ))
            }
            ArrayElement::Spread(expression::SpreadElement {
                loc: spread_loc,
                argument: spread_argument,
                comments,
            }) => {
                let argument = expression_inner(
                    None,
                    None,
                    Some(as_const),
                    FrozenKind::NotFrozen,
                    Some(has_hint.clone()),
                    cx,
                    spread_argument,
                )?;
                let (_, t) = argument.loc();
                let t = t.dupe();
                Ok((
                    type_::UnresolvedParam::UnresolvedSpreadArg(t),
                    ArrayElement::Spread(expression::SpreadElement {
                        loc: spread_loc.dupe(),
                        argument,
                        comments: comments.clone(),
                    }),
                ))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(results.into_iter().unzip())
}

pub fn empty_array<'a>(cx: &Context<'a>, loc: ALoc) -> (Reason, Type) {
    let reason = mk_reason(VirtualReasonDesc::REmptyArrayLit, loc.dupe());
    let element_reason = mk_reason(VirtualReasonDesc::REmptyArrayElement, loc.dupe());
    let type_::LazyHintT(has_hint, lazy_hint) = type_env::get_hint(cx, loc.dupe());
    // empty array, analogous to object with implicit properties
    let elemt = if !has_hint {
        empty_t::make(mk_reason(VirtualReasonDesc::REmptyArrayElement, loc.dupe()))
    } else {
        match lazy_hint(cx, false, None, element_reason) {
            type_::HintEvalResult::HintAvailable(hint, _) => hint,
            type_::HintEvalResult::DecompositionError => {
                // A hint is available, but cannot be used to provide a type for this
                // array element. In this case, the element type of the array is likely
                // useless (does not escape the annotation), so we can use `empty` as
                // its type.
                empty_t::make(mk_reason(VirtualReasonDesc::REmptyArrayElement, loc.dupe()))
            }
            type_::HintEvalResult::NoHint | type_::HintEvalResult::EncounteredPlaceholder => {
                // If there is no hint then raise an error. The EncounteredPlaceholder case
                // corresponds to code like `const set = new Set([]);`. This case will
                // raise a [missing-empty-array-annot] error on `[]`.
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EEmptyArrayNoProvider { loc: loc.dupe() },
                );
                any_t::at(type_::AnySource::Untyped, loc.dupe())
            }
        }
    };
    (reason, elemt)
}

/// can raise Abnormal.(Exn (_, _))
/// annot should become a Type.t option when we have the ability to
/// inspect annotations and recurse into them
fn expression_inner<'a>(
    encl_ctx: Option<EnclosingContext>,
    decl: Option<ast::VariableKind>,
    as_const: Option<bool>,
    frozen: FrozenKind,
    has_hint: Option<LazyBool<'a>>,
    cx: &Context<'a>,
    expr: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let loc = expr.loc().dupe();
    let threshold = cx
        .slow_to_check_logging()
        .slow_expressions_logging_threshold;
    let start_time = threshold.map(|_| std::time::Instant::now());
    let node_cache = cx.node_cache();
    let res = match node_cache.get_expression(&loc) {
        Some(node) => {
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Expression cache hit at {}",
                    loc.debug_to_string(false)
                )]
            });
            node
        }
        None => {
            let syntactic_flags = natural_inference::mk_syntactic_flags(
                encl_ctx,
                decl,
                as_const,
                Some(frozen),
                has_hint,
            );
            let res = expression_(cx, syntactic_flags, loc.dupe(), expr)?;
            if matches!(
                &*cx.typing_mode(),
                flow_typing_context::TypingMode::CheckingMode
            ) {
                cx.constraint_cache_mut().clear();
                cx.eval_repos_cache_mut().clear();
                node_cache.set_expression(res.dupe());
            }
            res
        }
    };
    // We need to fully resolve all types attached to AST,
    // because the post inference pass might inspect them.
    let (_, t) = res.loc();
    let t = t.dupe();
    match &*res {
        expression::ExpressionInner::OptionalCall { inner, .. } => {
            let (_, extra_t) = &inner.filtered_out;
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, extra_t);
        }
        expression::ExpressionInner::OptionalMember { inner, .. } => {
            let (_, extra_t) = &inner.filtered_out;
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, extra_t);
        }
        expression::ExpressionInner::Yield { inner, .. } => {
            let (_, extra_t) = &inner.result_out;
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, extra_t);
        }
        _ => {}
    }
    tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &t);
    if let (Some(threshold), Some(start_time)) = (threshold, start_time) {
        let run_time = start_time.elapsed().as_secs_f64();
        if run_time > threshold {
            eprintln!(
                "[{}] Slow CHECK expression at {} ({} seconds)",
                std::process::id(),
                loc.debug_to_string(true),
                run_time,
            );
        }
    }
    Ok(res)
}

fn this_<'a>(cx: &Context<'a>, loc: ALoc, this: &expression::This<ALoc>) -> Type {
    let expr = expression::Expression::new(expression::ExpressionInner::This {
        loc: loc.dupe(),
        inner: std::sync::Arc::new(this.clone()),
    });
    match refinement::get(true, cx, &expr, loc.dupe()) {
        Some(t) => t,
        None => type_env::var_ref(None, cx, None, Name::new("this"), loc),
    }
}

fn super_<'a>(cx: &Context<'a>, loc: ALoc) -> Type {
    type_env::var_ref(None, cx, None, Name::new("super"), loc)
}

fn expression_<'a>(
    cx: &Context<'a>,
    syntactic_flags: SyntacticFlags<'a>,
    loc: ALoc,
    e: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let SyntacticFlags {
        encl_ctx,
        decl,
        as_const,
        frozen,
        has_hint,
    } = &syntactic_flags;
    let encl_ctx = encl_ctx.dupe();
    let ex = e.dupe();
    use ast::expression::ExpressionInner;
    Ok(match e.deref() {
        ExpressionInner::StringLiteral { inner, .. } => {
            let t = string_literal_inner(cx, &syntactic_flags, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::StringLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::BooleanLiteral { inner, .. } => {
            let t = boolean_literal_inner(cx, &syntactic_flags, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::BooleanLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::NullLiteral { inner, .. } => {
            let t = null_literal(loc.dupe());
            expression::Expression::new(ExpressionInner::NullLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::NumberLiteral { inner, .. } => {
            let t = number_literal_inner(cx, &syntactic_flags, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::NumberLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::BigIntLiteral { inner, .. } => {
            let t = bigint_literal_inner(cx, &syntactic_flags, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::BigIntLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::RegExpLiteral { inner, .. } => {
            let t = regexp_literal(cx, loc.dupe());
            expression::Expression::new(ExpressionInner::RegExpLiteral {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::ModuleRefLiteral { inner, .. } => {
            let (t, lit) = module_ref_literal(cx, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::ModuleRefLiteral {
                loc: (loc, t),
                inner: std::sync::Arc::new(lit),
            })
        }
        // Treat the identifier `undefined` as an annotation for error reporting
        // purposes. Like we do with other literals. Otherwise we end up pointing to
        // `void` in `core.js`. While possible to re-declare `undefined`, it is
        // unlikely. The tradeoff is worth it.
        ExpressionInner::Identifier { inner, .. }
            if inner.name.as_str() == "undefined"
                && type_env::is_global_var(cx, inner.loc.dupe()) =>
        {
            let t = type_::void::make(mk_reason(VirtualReasonDesc::RVoid, loc.dupe()));
            expression::Expression::new(ExpressionInner::Identifier {
                loc: (loc, t.dupe()),
                inner: ast::Identifier::new(ast::IdentifierInner {
                    loc: (inner.loc.dupe(), t),
                    name: inner.name.dupe(),
                    comments: inner.comments.dupe(),
                }),
            })
        }
        ExpressionInner::Identifier { inner, .. } => {
            let t = identifier_(cx, &syntactic_flags, &inner.name, loc.dupe());
            expression::Expression::new(ExpressionInner::Identifier {
                loc: (loc, t.dupe()),
                inner: ast::Identifier::new(ast::IdentifierInner {
                    loc: (inner.loc.dupe(), t),
                    name: inner.name.dupe(),
                    comments: inner.comments.dupe(),
                }),
            })
        }
        ExpressionInner::This { inner, .. } => {
            let t = this_(cx, loc.dupe(), inner);
            expression::Expression::new(ExpressionInner::This {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::Super { inner, .. } => {
            let t = identifier_(
                cx,
                &natural_inference::empty_syntactic_flags(),
                &FlowSmolStr::new("super"),
                loc.dupe(),
            );
            expression::Expression::new(ExpressionInner::Super {
                loc: (loc, t),
                inner: inner.clone(),
            })
        }
        ExpressionInner::Unary { inner, .. } => {
            let (t, u) = unary(cx, &syntactic_flags, loc.dupe(), inner)?;
            expression::Expression::new(ExpressionInner::Unary {
                loc: (loc, t),
                inner: u.into(),
            })
        }
        ExpressionInner::Update { inner, .. } => {
            let (t, u) = update(cx, loc.dupe(), inner)?;
            expression::Expression::new(ExpressionInner::Update {
                loc: (loc, t),
                inner: u.into(),
            })
        }
        ExpressionInner::Binary { inner, .. } => {
            let (t, b) = binary(cx, loc.dupe(), encl_ctx, inner)?;
            expression::Expression::new(ExpressionInner::Binary {
                loc: (loc, t),
                inner: b.into(),
            })
        }
        ExpressionInner::Logical { inner, .. } => {
            let (t, l) = logical(cx, &syntactic_flags, loc.dupe(), inner)?;
            expression::Expression::new(ExpressionInner::Logical {
                loc: (loc, t),
                inner: l.into(),
            })
        }
        ExpressionInner::TypeCast { inner, .. } => {
            let casting_syntax = cx.casting_syntax();
            match &casting_syntax {
                flow_common::options::CastingSyntax::Both => {}
                flow_common::options::CastingSyntax::As => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInvalidTypeCastSyntax {
                            loc: loc.dupe(),
                            enabled_casting_syntax: casting_syntax.clone(),
                        },
                    );
                }
            }
            let (t, annot_prime) =
                type_annotation::mk_type_available_annotation(cx, Default::default(), &inner.annot);
            let e_prime = expression(None, None, None, cx, &inner.expression)?;
            let infer_t = e_prime.loc().1.dupe();
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::Cast {
                lower: mk_expression_reason(&inner.expression),
                upper: reason_of_t(&t).dupe(),
            }));
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &infer_t);
            type_operation_utils::perform_type_cast(cx, use_op, &infer_t, &t).unwrap();
            expression::Expression::new(ExpressionInner::TypeCast {
                loc: (loc, t),
                inner: (expression::TypeCast {
                    expression: e_prime,
                    annot: annot_prime,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        ExpressionInner::AsExpression { inner, .. } => {
            let (t, annot_prime) =
                type_annotation::mk_type_available_annotation(cx, Default::default(), &inner.annot);
            let e_prime = expression(None, None, None, cx, &inner.expression)?;
            let infer_t = e_prime.loc().1.dupe();
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::Cast {
                lower: mk_expression_reason(&inner.expression),
                upper: reason_of_t(&t).dupe(),
            }));
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &infer_t);
            type_operation_utils::perform_type_cast(cx, use_op, &infer_t, &t).unwrap();
            expression::Expression::new(ExpressionInner::AsExpression {
                loc: (loc, t),
                inner: (expression::AsExpression {
                    expression: e_prime,
                    annot: annot_prime,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        ExpressionInner::AsConstExpression { inner, .. } => {
            let e = expression(None, None, Some(true), cx, &inner.expression)?;
            check_const_assertion(cx, &e);
            let t = e.loc().1.dupe();
            expression::Expression::new(ExpressionInner::AsConstExpression {
                loc: (loc, t),
                inner: (expression::AsConstExpression {
                    expression: e,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        ExpressionInner::TSSatisfies { inner, .. } => {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                    kind: TSSyntaxKind::TSSatisfiesType(cx.casting_syntax()),
                    loc: loc.dupe(),
                })),
            );
            let t = type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe());
            let Ok(mapped) =
                polymorphic_ast_mapper::ts_satisfies(&mut typed_ast_utils::ErrorMapper, inner);
            expression::Expression::new(ExpressionInner::TSSatisfies {
                loc: (loc, t),
                inner: mapped.into(),
            })
        }
        ExpressionInner::Match { inner, .. } => {
            if !cx.enable_pattern_matching() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::MatchExpression,
                    ))),
                );
                {
                    let Ok(v) =
                        polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, &ex);
                    v
                }
            } else {
                let reason = mk_reason(VirtualReasonDesc::RMatch, loc.dupe());
                let arg = expression(None, None, Some(true), cx, &inner.arg)?;
                let has_hint: LazyBool<'a> = {
                    let old_has_hint = has_hint.dupe();
                    let loc = loc.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                        *old_has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &loc)
                    })))
                };
                let arg_t = arg.loc().1.dupe();
                type_env::init_const(
                    cx,
                    &type_::unknown_use(),
                    &arg_t,
                    inner.match_keyword_loc.dupe(),
                );
                let mut cases = Vec::new();
                let mut ts = Vec::new();
                let mut all_throws = true;
                let mut invalid_syntax_list = Vec::new();
                for case in inner.cases.iter() {
                    let case_loc = &case.loc;
                    let pattern = match_pattern(
                        cx,
                        case.case_match_root_loc.dupe(),
                        case.guard.is_some(),
                        &case.pattern,
                    )?;
                    let (guard, guard_throws) = match &case.guard {
                        Some(guard_expr) => {
                            let (guard, throws) =
                                flow_typing_utils::abnormal::catch_expr_control_flow_exception(
                                    || expression(None, None, None, cx, guard_expr),
                                );
                            (Some(guard), throws)
                        }
                        None => (None, false),
                    };
                    let (body, body_throws) =
                        flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                            expression_inner(
                                Some(encl_ctx.dupe()),
                                *decl,
                                None,
                                FrozenKind::NotFrozen,
                                Some(has_hint.dupe()),
                                cx,
                                &case.body,
                            )
                        });
                    let t = body.loc().1.dupe();
                    let case_ast = ast::match_::Case {
                        loc: case_loc.dupe(),
                        pattern,
                        body,
                        guard,
                        comments: case.comments.dupe(),
                        invalid_syntax: case.invalid_syntax.clone(),
                        case_match_root_loc: case.case_match_root_loc.dupe(),
                    };
                    let throws = guard_throws || body_throws;
                    all_throws = all_throws && throws;
                    if !throws {
                        ts.push(t);
                    }
                    cases.push(case_ast);
                    invalid_syntax_list.push(case.invalid_syntax.clone());
                }
                error_on_match_case_invalid_syntax(
                    cx,
                    inner.match_keyword_loc.dupe(),
                    &invalid_syntax_list,
                );
                type_env::var_ref(
                    Some(type_env::LookupMode::ForValue),
                    cx,
                    None,
                    Name::new(flow_parser::ast_utils::MATCH_ROOT_NAME),
                    inner.match_keyword_loc.dupe(),
                );
                let match_t = union_of_ts(reason, ts, None);
                let ast = expression::Expression::new(ExpressionInner::Match {
                    loc: (loc.dupe(), match_t),
                    inner: (expression::MatchExpression {
                        arg,
                        cases: cases.into(),
                        match_keyword_loc: inner.match_keyword_loc.dupe(),
                        comments: inner.comments.dupe(),
                    })
                    .into(),
                });
                if !inner.cases.is_empty() && all_throws {
                    return Err(AbnormalControlFlow(loc, ast));
                }
                ast
            }
        }
        ExpressionInner::Member { .. } | ExpressionInner::OptionalMember { .. } => {
            subscript(encl_ctx, cx, &ex)?
        }
        ExpressionInner::Object { inner, .. } => {
            let as_const = *as_const;
            let frozen = *frozen == FrozenKind::FrozenDirect;
            let (t, properties) = object_(
                cx,
                frozen,
                as_const,
                has_hint.dupe(),
                loc.dupe(),
                &inner.properties,
            )?;
            expression::Expression::new(ExpressionInner::Object {
                loc: (loc, t),
                inner: expression::Object {
                    properties: properties.into(),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::Record { inner, .. } => {
            if !cx.enable_records() {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::Records,
                    ))),
                );
                {
                    let Ok(v) =
                        polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, &ex);
                    v
                }
            } else {
                let constructor_ast = expression(None, None, None, cx, &inner.constructor)?;
                let constructor_t = constructor_ast.loc().1.dupe();
                let (targts, targs_ast) = convert_call_targs_opt(cx, inner.targs.as_ref());
                let (props_loc, props_obj) = &inner.properties;
                let (props_t, props_ast) = object_(
                    cx,
                    *frozen == FrozenKind::FrozenDirect,
                    *as_const,
                    has_hint.dupe(),
                    props_loc.dupe(),
                    &props_obj.properties,
                )?;
                let props_t = mod_reason_of_t(
                    &|r: Reason| r.replace_desc(VirtualReasonDesc::RRecordProperties),
                    &props_t,
                );
                let use_op = VirtualUseOp::Op(Arc::new(VirtualRootUseOp::RecordCreate(Box::new(
                    RecordCreateData {
                        op: mk_expression_reason(&ex),
                        constructor: mk_expression_reason(&inner.constructor),
                        properties: props_loc.dupe(),
                    },
                ))));
                let reason = reason_of_t(&constructor_t).dupe();
                let (t, ctor_t) = new_call(
                    cx,
                    loc.dupe(),
                    reason,
                    use_op,
                    constructor_t,
                    targts,
                    vec![CallArg::arg(props_t)],
                );
                cx.set_ctor_callee(loc.dupe(), ctor_t);
                expression::Expression::new(ExpressionInner::Record {
                    loc: (loc, t),
                    inner: (expression::Record {
                        constructor: constructor_ast,
                        targs: targs_ast,
                        properties: (
                            props_loc.dupe(),
                            expression::Object {
                                properties: props_ast.into(),
                                comments: props_obj.comments.dupe(),
                            },
                        ),
                        comments: inner.comments.dupe(),
                    })
                    .into(),
                })
            }
        }
        ExpressionInner::Array { inner, .. } => {
            let as_const = *as_const;
            match inner.elements.as_ref() {
                [] if as_const => {
                    let reason = mk_reason(VirtualReasonDesc::RConstArrayLit, loc.dupe());
                    let elem_t =
                        empty_t::make(mk_reason(VirtualReasonDesc::REmptyArrayElement, loc.dupe()));
                    let arrtype = type_::ArrType::TupleAT(Box::new(TupleATData {
                        elem_t,
                        elements: vec![].into(),
                        react_dro: None,
                        arity: (0, 0),
                        inexact: false,
                    }));
                    let t = Type::new(TypeInner::DefT(
                        reason,
                        DefT::new(DefTInner::ArrT(Rc::new(arrtype))),
                    ));
                    expression::Expression::new(ExpressionInner::Array {
                        loc: (loc, t),
                        inner: (expression::Array {
                            elements: vec![].into(),
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                [] if !matches!(
                    &*cx.typing_mode(),
                    flow_typing_context::TypingMode::CheckingMode
                ) =>
                {
                    let reason = mk_reason(VirtualReasonDesc::REmptyArrayLit, loc.dupe());
                    let element_reason =
                        mk_reason(VirtualReasonDesc::REmptyArrayElement, loc.dupe());
                    let elem_t = cx.mk_placeholder(element_reason);
                    let arrtype = type_::ArrType::ArrayAT(Box::new(ArrayATData {
                        elem_t,
                        tuple_view: Some(type_::empty_tuple_view()),
                        react_dro: None,
                    }));
                    let t = Type::new(TypeInner::DefT(
                        reason,
                        DefT::new(DefTInner::ArrT(Rc::new(arrtype))),
                    ));
                    expression::Expression::new(ExpressionInner::Array {
                        loc: (loc, t),
                        inner: (expression::Array {
                            elements: vec![].into(),
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                [] => {
                    let (reason, elem_t) = empty_array(cx, loc.dupe());
                    let arrtype = type_::ArrType::ArrayAT(Box::new(ArrayATData {
                        elem_t,
                        tuple_view: Some(type_::empty_tuple_view()),
                        react_dro: None,
                    }));
                    let t = Type::new(TypeInner::DefT(
                        reason,
                        DefT::new(DefTInner::ArrT(Rc::new(arrtype))),
                    ));
                    expression::Expression::new(ExpressionInner::Array {
                        loc: (loc, t),
                        inner: (expression::Array {
                            elements: vec![].into(),
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                _elems => {
                    let has_hint: LazyBool<'a> = {
                        let old_has_hint = has_hint.dupe();
                        let loc = loc.dupe();
                        Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                            *old_has_hint.get_forced(cx)
                                || natural_inference::loc_has_hint(cx, &loc)
                        })))
                    };
                    let reason = if as_const {
                        mk_reason(VirtualReasonDesc::RConstArrayLit, loc.dupe())
                    } else if *has_hint.get_forced(cx) {
                        mk_reason(VirtualReasonDesc::RArrayLitUnsound, loc.dupe())
                    } else {
                        mk_reason(VirtualReasonDesc::RArrayLit, loc.dupe())
                    };
                    let (elem_spread_list, elements) =
                        array_elements(cx, as_const, has_hint, &inner.elements)?;
                    let reason_clone = reason.dupe();
                    let t = tvar_resolver::mk_tvar_and_fully_resolve_where(
                        cx,
                        reason.dupe(),
                        |cx, tout| {
                            let reason_op = reason_clone.dupe();
                            let element_reason = mk_reason(
                                VirtualReasonDesc::RInferredUnionElemArray {
                                    instantiable: false,
                                    is_empty: false,
                                },
                                reason_op.loc().dupe(),
                            );
                            let elem_t = flow_typing_tvar::mk(cx, element_reason);
                            let resolve_to = type_::SpreadResolve::ResolveSpreadsToArrayLiteral {
                                id: mk_id() as i32,
                                as_const,
                                elem_t,
                                tout: tout.dupe(),
                            };
                            FlowJs::resolve_spread_list(
                                cx,
                                type_::unknown_use(),
                                &reason_op,
                                elem_spread_list.clone(),
                                resolve_to,
                            )
                            .expect("should not fail outside speculation");
                        },
                    );
                    expression::Expression::new(ExpressionInner::Array {
                        loc: (loc, t),
                        inner: (expression::Array {
                            elements: elements.into(),
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
            }
        }
        //   | New
        //       {
        //         New.callee =
        //           ( callee_loc,
        //             Identifier (id_loc, ({ Ast.Identifier.name = "Function"; comments = _ } as name))
        //           );
        //         targs;
        //         arguments;
        //         comments;
        //       } ->
        ExpressionInner::New { inner, .. }
            if let ExpressionInner::Identifier {
                loc: callee_loc,
                inner: name,
            } = inner.callee.deref()
                && name.name.as_str() == "Function" =>
        {
            let id_loc = name.loc.dupe();
            let targts_opt = inner
                .targs
                .as_ref()
                .map(|args| convert_call_targs(cx, &FlowOrdMap::new(), args));
            let (argts, arges) = match &inner.arguments {
                Some(arguments) => {
                    let (argts, arges) = arg_list(cx, arguments)?;
                    (argts, Some(arges))
                }
                None => (vec![], None),
            };
            let id_t = identifier_inner(
                cx,
                &natural_inference::empty_syntactic_flags(),
                name,
                callee_loc.dupe(),
            );
            let callee_annot = (callee_loc.dupe(), id_t.dupe());
            match targts_opt {
                None => {
                    for argt in &argts {
                        let t = match argt.deref() {
                            CallArgInner::Arg(t) | CallArgInner::SpreadArg(t) => t,
                        };
                        flow_js::flow_t_non_speculating(cx, (t, &str_module_t::at(loc.dupe())));
                    }
                    let reason = mk_reason(VirtualReasonDesc::RNewFunction, loc.dupe());
                    let proto = obj_proto::make(reason.dupe());
                    let t = Type::new(TypeInner::DefT(
                        reason.dupe(),
                        DefT::new(DefTInner::FunT(
                            dummy_static(reason.dupe()),
                            Rc::new(mk_functiontype(
                                reason.dupe(),
                                None,
                                None,
                                vec![],
                                None,
                                reason.dupe(),
                                Some(vec![]),
                                None,
                                proto,
                            )),
                        )),
                    ));
                    expression::Expression::new(ExpressionInner::New {
                        loc: (loc, t),
                        inner: (expression::New {
                            callee: expression::Expression::new(ExpressionInner::Identifier {
                                loc: callee_annot,
                                inner: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc, id_t),
                                    name: name.name.dupe(),
                                    comments: name.comments.dupe(),
                                }),
                            }),
                            targs: None,
                            arguments: arges,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                Some((_, targs_ast)) => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                            call_loc: loc.dupe(),
                            is_new: true,
                            reason_arity: locationless_reason(VirtualReasonDesc::RType(Name::new(
                                FlowSmolStr::new("Function"),
                            ))),
                            expected_arity: 0,
                        })),
                    );
                    let t = any_t::at(type_::AnySource::AnyError(None), loc.dupe());
                    expression::Expression::new(ExpressionInner::New {
                        loc: (loc, t),
                        inner: (expression::New {
                            callee: expression::Expression::new(ExpressionInner::Identifier {
                                loc: callee_annot,
                                inner: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc, id_t),
                                    name: name.name.dupe(),
                                    comments: name.comments.dupe(),
                                }),
                            }),
                            targs: Some(targs_ast),
                            arguments: arges,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
            }
        }
        //   | New
        //       {
        //         New.callee =
        //           ( callee_loc,
        //             Identifier (id_loc, ({ Ast.Identifier.name = "Array" as n; comments = _ } as name))
        //           );
        //         targs;
        //         arguments;
        //         comments;
        //       } ->
        ExpressionInner::New { inner, .. }
            if let ExpressionInner::Identifier {
                loc: callee_loc,
                inner: name,
            } = inner.callee.deref()
                && name.name.as_str() == "Array" =>
        {
            let id_loc = name.loc.dupe();
            let n = name.name.dupe();
            let targts = inner
                .targs
                .as_ref()
                .map(|args| convert_call_targs(cx, &FlowOrdMap::new(), args));
            let (argts, args) = match &inner.arguments {
                Some(arguments) => {
                    let (argts, args) = arg_list(cx, arguments)?;
                    (argts, Some(args))
                }
                None => (vec![], None),
            };
            let result: Result<
                (
                    Option<(expression::CallTypeArgs<ALoc, (ALoc, Type)>, Targ)>,
                    Type,
                ),
                ErrorMessage<ALoc>,
            > = match (targts, &argts[..]) {
                (Some((targs_vec, call_targs)), [argt])
                    if targs_vec.len() == 1
                        && call_targs.arguments.len() == 1
                        && let CallArgInner::Arg(t) = argt.deref() =>
                {
                    let targ = targs_vec.into_iter().next().unwrap();
                    Ok((Some((call_targs, targ)), t.dupe()))
                }
                (None, [argt]) if let CallArgInner::Arg(t) = argt.deref() => Ok((None, t.dupe())),
                (None, _) => Err(ErrorMessage::EUseArrayLiteral(loc.dupe())),
                (Some(_), _) => Err(ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                    call_loc: loc.dupe(),
                    is_new: true,
                    reason_arity: locationless_reason(VirtualReasonDesc::RType(Name::new(
                        n.dupe(),
                    ))),
                    expected_arity: 1,
                }))),
            };
            match result {
                Ok((targ_t, arg_t)) => {
                    let reason = mk_reason(VirtualReasonDesc::RNewArray, loc.dupe());
                    let length_reason = reason.dupe().replace_desc(VirtualReasonDesc::RArrayLength);
                    flow_js::flow_t_non_speculating(
                        cx,
                        (
                            &arg_t,
                            &Type::new(TypeInner::DefT(
                                length_reason,
                                DefT::new(DefTInner::NumGeneralT(Literal::AnyLiteral)),
                            )),
                        ),
                    );
                    let (targ_ts, targs_ast) = match targ_t {
                        Some((ast, Targ::ExplicitArg(t))) => {
                            (Some(vec![Targ::ExplicitArg(t)]), Some(ast))
                        }
                        Some((_, Targ::ImplicitArg(_))) | None => (None, None),
                    };
                    let id_t = identifier_inner(
                        cx,
                        &natural_inference::empty_syntactic_flags(),
                        name,
                        callee_loc.dupe(),
                    );
                    let reason_call = mk_reason(
                        VirtualReasonDesc::RConstructorCall(Arc::new(desc_of_t(&id_t).clone())),
                        loc.dupe(),
                    );
                    let use_op =
                        UseOp::Op(Arc::new(type_::RootUseOp::FunCall(Box::new(FunCallData {
                            op: reason.dupe(),
                            fn_: reason_of_t(&id_t).dupe(),
                            args: vec![reason_of_t(&arg_t).dupe()].into(),
                            local: true,
                        }))));
                    let (t, ctor_t) = new_call(
                        cx,
                        loc.dupe(),
                        reason_call,
                        use_op,
                        id_t.dupe(),
                        targ_ts,
                        vec![CallArg::arg(arg_t)],
                    );
                    cx.set_ctor_callee(loc.dupe(), ctor_t);
                    expression::Expression::new(ExpressionInner::New {
                        loc: (loc, t),
                        inner: (expression::New {
                            callee: expression::Expression::new(ExpressionInner::Identifier {
                                loc: (callee_loc.dupe(), id_t.dupe()),
                                inner: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc.dupe(), id_t),
                                    name: name.name.dupe(),
                                    comments: name.comments.dupe(),
                                }),
                            }),
                            targs: targs_ast,
                            arguments: args,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, err);
                    {
                        let Ok(v) = polymorphic_ast_mapper::expression(
                            &mut typed_ast_utils::ErrorMapper,
                            &ex,
                        );
                        v
                    }
                }
            }
        }
        ExpressionInner::New { inner, .. } => {
            let callee_ast = expression(None, None, None, cx, &inner.callee)?;
            let class_ = callee_ast.loc().1.dupe();
            let (targts, targs_ast) = convert_call_targs_opt(cx, inner.targs.as_ref());
            let (argts, args_reasons, arguments_ast) = match &inner.arguments {
                Some(arguments) => {
                    let (argst, arguments_ast) = arg_list(cx, arguments)?;
                    let args_reasons = mk_initial_arguments_reason(arguments);
                    (argst, args_reasons, Some(arguments_ast))
                }
                None => (vec![], vec![], None),
            };
            let reason = mk_reason(
                VirtualReasonDesc::RConstructorCall(Arc::new(desc_of_t(&class_).clone())),
                loc.dupe(),
            );
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCall(Box::new(FunCallData {
                op: mk_expression_reason(&ex),
                fn_: mk_expression_reason(&inner.callee),
                args: args_reasons.into(),
                local: true,
            }))));
            let (t, ctor_t) = new_call(cx, loc.dupe(), reason, use_op, class_, targts, argts);
            cx.set_ctor_callee(loc.dupe(), ctor_t);
            // Check if using new Record({...}) and emit error suggesting record expression syntax
            if let Some(arguments) = &inner.arguments {
                if arguments.arguments.len() == 1 {
                    if let Some(arg_expr) = arguments.arguments.first()
                        && let expression::ExpressionOrSpread::Expression(arg_e) = arg_expr
                        && matches!(arg_e.deref(), ExpressionInner::Object { .. })
                    {
                        let record_name_opt = match FlowJs::singleton_concrete_type_for_inspection(
                            cx,
                            reason_of_t(&t),
                            &t,
                        ) {
                            Ok(concrete_t) => match concrete_t.deref() {
                                TypeInner::DefT(_, def_t) => match def_t.deref() {
                                    DefTInner::InstanceT(instance_t) => {
                                        if matches!(
                                            &instance_t.inst.inst_kind,
                                            InstanceKind::RecordKind { .. }
                                        ) {
                                            instance_t.inst.class_name.as_ref().duped()
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                },
                                TypeInner::ThisInstanceT(box ThisInstanceTData {
                                    instance,
                                    ..
                                }) => {
                                    if matches!(
                                        &instance.inst.inst_kind,
                                        InstanceKind::RecordKind { .. }
                                    ) {
                                        instance.inst.class_name.as_ref().duped()
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            },
                            Err(_) => None,
                        };
                        if let Some(record_name) = record_name_opt {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::ERecordError(RecordErrorKind::RecordInvalidNew {
                                    loc: loc.dupe(),
                                    record_name,
                                }),
                            );
                        }
                    }
                }
            }
            expression::Expression::new(ExpressionInner::New {
                loc: (loc, t),
                inner: expression::New {
                    callee: callee_ast,
                    targs: targs_ast,
                    arguments: arguments_ast,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::Call { .. } => {
            cx.set_enclosing_context_for_call(loc.dupe(), encl_ctx.dupe());
            subscript(encl_ctx, cx, &ex)?
        }
        ExpressionInner::OptionalCall { .. } => subscript(encl_ctx, cx, &ex)?,
        ExpressionInner::Conditional { inner, .. } => {
            let has_hint: LazyBool<'a> = {
                let old_has_hint = has_hint.dupe();
                let loc = loc.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                    *old_has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &loc)
                })))
            };
            let decl = *decl;
            let reason = mk_reason(VirtualReasonDesc::RConditional, loc.dupe());
            let test = condition(
                cx,
                EnclosingContext::OtherTestContext,
                None,
                None,
                &inner.test,
            )?;
            let (consequent, then_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    expression_inner(
                        Some(encl_ctx.dupe()),
                        decl,
                        None,
                        FrozenKind::NotFrozen,
                        Some(has_hint.dupe()),
                        cx,
                        &inner.consequent,
                    )
                });
            let t1 = consequent.loc().1.dupe();
            let (alternate, else_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    expression_inner(
                        Some(encl_ctx.dupe()),
                        decl,
                        None,
                        FrozenKind::NotFrozen,
                        Some(has_hint.dupe()),
                        cx,
                        &inner.alternate,
                    )
                });
            let t2 = alternate.loc().1.dupe();
            let combined_type = match (then_throws, else_throws) {
                (true, false) => t2,
                (false, true) => t1,
                (true, true) => empty_t::at(loc.dupe()),
                // In general it is dangerous to express the least upper bound of
                // some types as a union: it might pin down the least upper bound
                // prematurely (before all the types have been inferred), and when the
                // union appears as an upper bound, it might lead to speculative matching.
                //
                // However, here a union is safe, because this union is guaranteed to only
                // appear as a lower bound.
                //
                // In such "covariant" positions, avoiding unnecessary indirection via
                // tvars is a good thing, because it improves precision. In particular, it
                // enables more types to be fully resolvable, which improves results of
                // speculative matching.
                //
                // It should be possible to do this more broadly and systematically. For
                // example, results of operations on annotations (like property gets on
                // objects, calls on functions) are often represented as unresolved tvars,
                // where they could be pinned down to resolved types.
                (false, false) => Type::new(TypeInner::UnionT(
                    reason.dupe(),
                    type_::union_rep::make(
                        None,
                        type_::union_rep::UnionKind::ConditionalKind,
                        t1,
                        t2,
                        vec![].into(),
                    ),
                )),
            };
            // TODO call loc_of_predicate on some pred?
            let ast = expression::Expression::new(ExpressionInner::Conditional {
                loc: (loc.dupe(), combined_type),
                inner: (expression::Conditional {
                    test,
                    consequent,
                    alternate,
                    comments: inner.comments.dupe(),
                })
                .into(),
            });
            // handle control flow in cases where we've thrown from both sides
            if then_throws && else_throws {
                return Err(AbnormalControlFlow(loc, ast));
            }
            ast
        }
        ExpressionInner::Assignment { inner, .. } => {
            let (t, left, right) =
                assignment(cx, loc.dupe(), &inner.left, inner.operator, &inner.right)?;
            expression::Expression::new(ExpressionInner::Assignment {
                loc: (loc, t),
                inner: (expression::Assignment {
                    operator: inner.operator,
                    left,
                    right,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        ExpressionInner::Sequence { inner, .. } => {
            let expressions: Vec<_> = inner
                .expressions
                .iter()
                .map(|e| expression(None, None, None, cx, e))
                .collect::<Result<_, _>>()?;
            let t = expressions
                .last()
                .map(|e| e.loc().1.dupe())
                .unwrap_or_else(|| any_t::at(type_::AnySource::Untyped, loc.dupe()));
            expression::Expression::new(ExpressionInner::Sequence {
                loc: (loc, t),
                inner: (expression::Sequence {
                    expressions: expressions.into(),
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        ExpressionInner::Function { inner, .. } => {
            let reason = func_reason(inner.async_, inner.generator, inner.sig_loc.dupe());
            let (t, func) = match &inner.id {
                None => mk_function_expression(cx, true, reason, loc.dupe(), inner)?,
                Some(_) => {
                    let prev_scope_kind = type_env::set_scope_kind(
                        cx,
                        flow_env_builder::name_def_types::ScopeKind::Ordinary,
                    );
                    let (t, func) = mk_function_expression(cx, true, reason, loc.dupe(), inner)?;
                    type_env::set_scope_kind(cx, prev_scope_kind);
                    (t, func)
                }
            };
            expression::Expression::new(ExpressionInner::Function {
                loc: (loc, t),
                inner: func.into(),
            })
        }
        ExpressionInner::ArrowFunction { inner, .. } => {
            let reason = func_reason(inner.async_, inner.generator, loc.dupe());
            let (t, f) = mk_arrow(cx, &BTreeMap::new(), reason, inner)?;
            expression::Expression::new(ExpressionInner::ArrowFunction {
                loc: (loc, t),
                inner: f.into(),
            })
        }
        ExpressionInner::TaggedTemplate { inner, .. }
            if let ExpressionInner::Identifier { inner: tag_id, .. } = inner.tag.deref()
                && tag_id.name.as_str() == "graphql"
                && cx.enable_relay_integration() =>
        {
            let module_prefix = cx.relay_integration_module_prefix();
            let t = match graphql::extract_module_name(&inner.quasi.1, module_prefix.as_deref()) {
                //       | Ok module_name ->
                Ok(module_name) => {
                    let module_name_smol = FlowSmolStr::from(module_name.as_str());
                    let module_name_userland =
                        flow_import_specifier::Userland::from_smol_str(module_name_smol.dupe());
                    let source_module_rc =
                        flow_js_utils::import_export_utils::get_module_type_or_any(
                            cx,
                            false,
                            Some(type_::ImportKind::ImportValue),
                            loc.dupe(),
                            module_name_userland.dupe(),
                        )
                        .expect("Should not be under speculation");
                    let source_module = source_module_rc;
                    if cx.relay_integration_esmodules() {
                        let import_reason = mk_reason(
                            VirtualReasonDesc::RDefaultImportedType(
                                module_name_smol.dupe(),
                                module_name_userland.dupe(),
                            ),
                            loc.dupe(),
                        );
                        let (_def_loc, t) =
                            flow_js_utils::import_export_utils::import_default_specifier_type(
                                cx,
                                import_reason,
                                &|cx, reason, t| {
                                    FlowJs::singleton_concretize_type_for_imports_exports(
                                        cx, &reason, &t,
                                    )
                                    .map_err(Into::into)
                                },
                                &statement::ImportKind::ImportValue,
                                module_name_userland.dupe(),
                                &source_module,
                                &module_name_smol,
                            )
                            .expect("Should not be under speculation");
                        t
                    } else {
                        let reason = mk_reason(
                            VirtualReasonDesc::RModule(module_name_userland.dupe()),
                            loc.dupe(),
                        );
                        let namespace_symbol = flow_common::flow_symbol::Symbol::mk_module_symbol(
                            module_name_userland.dupe().into_inner(),
                            loc.dupe(),
                        );
                        let (_def_loc, t) = flow_js_utils::import_export_utils::cjs_require_type(
                            cx,
                            reason,
                            flow_js::reposition_non_speculating,
                            namespace_symbol,
                            false,
                            &source_module,
                        )
                        .expect("Should not be under speculation");
                        t
                    }
                }
                Err(err) => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInvalidGraphQL(Box::new((loc.dupe(), err))),
                    );
                    let reason = mk_reason(VirtualReasonDesc::RAnyImplicit, loc.dupe());
                    any_t::error(reason)
                }
            };
            let tag_ast = expression(None, None, None, cx, &inner.tag)?;
            let (_, targs_ast) = convert_call_targs_opt(cx, inner.targs.as_ref());
            let (quasi_loc, quasi) = &inner.quasi;
            let expressions: Vec<_> = quasi
                .expressions
                .iter()
                .map(|e| expression(None, None, None, cx, e))
                .collect::<Result<_, _>>()?;
            expression::Expression::new(ExpressionInner::TaggedTemplate {
                loc: (loc, t),
                inner: expression::TaggedTemplate {
                    tag: tag_ast,
                    targs: targs_ast,
                    quasi: (
                        quasi_loc.dupe(),
                        expression::TemplateLiteral {
                            quasis: quasi.quasis.clone(),
                            expressions: expressions.into(),
                            comments: quasi.comments.dupe(),
                        },
                    ),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::TaggedTemplate { inner, .. }
            if inner.targs.is_some() && !cx.tslib_syntax() =>
        {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    UnsupportedSyntax::TSLibSyntax(TsLibSyntaxKind::GenericTaggedTemplate),
                ))),
            );
            {
                let Ok(v) =
                    polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, &ex);
                v
            }
        }
        ExpressionInner::TaggedTemplate { inner, .. } => {
            let (quasi_loc, quasi) = &inner.quasi;
            let expressions: Vec<_> = quasi
                .expressions
                .iter()
                .map(|e| expression(None, None, None, cx, e))
                .collect::<Result<_, _>>()?;
            let tag_ast = expression(None, None, None, cx, &inner.tag)?;
            let (targts, targs_ast) = convert_call_targs_opt(cx, inner.targs.as_ref());
            let t = tag_ast.loc().1.dupe();
            let reason = mk_reason(VirtualReasonDesc::RCustom("encaps tag".into()), loc.dupe());
            let reason_array = mk_reason(VirtualReasonDesc::RArray, reason.loc().dupe());
            let ret_id = flow_typing_tvar::mk_no_wrap(cx, &reason);
            let ret_tvar = Tvar::new(reason.dupe(), ret_id as u32);
            let quasi_t = flow_js::get_builtin_type_non_speculating(
                cx,
                &reason_array,
                None,
                "TaggedTemplateLiteralArray",
            );
            let mut args: Vec<CallArg> = vec![CallArg::arg(quasi_t)];
            for e in &expressions {
                args.push(CallArg::arg(e.loc().1.dupe()));
            }
            let ft = type_::mk_functioncalltype(
                reason.dupe(),
                targts.map(|v| v.into()),
                args.into(),
                true,
                ret_tvar.dupe(),
            );
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCall(Box::new(FunCallData {
                op: mk_expression_reason(&ex),
                fn_: mk_expression_reason(&inner.tag),
                args: vec![].into(),
                local: true,
            }))));
            // tag`a${b}c${d}` -> tag(['a', 'c'], b, d)
            let call_t = UseT::new(UseTInner::CallT(Box::new(CallTData {
                use_op,
                reason: reason.dupe(),
                call_action: Box::new(type_::CallAction::Funcalltype(Box::new(ft))),
                return_hint: type_env::get_hint(cx, loc.dupe()),
            })));
            flow_js::flow_non_speculating(cx, (&t, &call_t));
            let result_t = Type::new(TypeInner::OpenT(ret_tvar));
            expression::Expression::new(ExpressionInner::TaggedTemplate {
                loc: (loc.dupe(), result_t),
                inner: expression::TaggedTemplate {
                    tag: tag_ast,
                    targs: targs_ast,
                    quasi: (
                        quasi_loc.dupe(),
                        expression::TemplateLiteral {
                            quasis: quasi.quasis.clone(),
                            expressions: expressions.into(),
                            comments: quasi.comments.dupe(),
                        },
                    ),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::TemplateLiteral { inner, .. } => {
            let (t, expressions) = match inner.quasis.as_ref() {
                [head] => {
                    let expression::template_literal::Element {
                        loc: elem_loc,
                        value: expression::template_literal::Value { cooked, .. },
                        ..
                    } = head;
                    let t = string_literal_value(cx, &syntactic_flags, elem_loc.dupe(), cooked);
                    (t, vec![])
                }
                _ => {
                    let t_out = type_::str_module_t::at(loc.dupe());
                    let expressions: Vec<_> = inner
                        .expressions
                        .iter()
                        .map(|expr| {
                            let e = expression(None, None, None, cx, expr)?;
                            let t = e.loc().1.dupe();
                            let reason = reason_of_t(&t).dupe();
                            let concrete_types =
                                FlowJs::possible_concrete_types_for_inspection(cx, &reason, &t)
                                    .expect("should not fail outside speculation");
                            let expanded: Vec<_> = concrete_types
                                .iter()
                                .flat_map(|ct| {
                                    if let TypeInner::GenericT(box GenericTData { bound, .. }) =
                                        ct.deref()
                                    {
                                        FlowJs::possible_concrete_types_for_inspection(
                                            cx, &reason, bound,
                                        )
                                        .unwrap_or_else(|_| vec![ct.dupe()])
                                    } else {
                                        vec![ct.dupe()]
                                    }
                                })
                                .collect();
                            for ct in &expanded {
                                let is_numeric = matches!(
                                    ct.deref(),
                                    TypeInner::DefT(_, def)
                                        if matches!(
                                            def.deref(),
                                            DefTInner::NumGeneralT(_)
                                            | DefTInner::SingletonNumT { .. }
                                            | DefTInner::BigIntGeneralT(_)
                                            | DefTInner::SingletonBigIntT { .. }
                                        )
                                );
                                if !is_numeric {
                                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::Coercion {
                                        from: mk_expression_reason(expr),
                                        target: reason_of_t(&t_out).dupe(),
                                    }));
                                    flow_js::flow_non_speculating(
                                        cx,
                                        (ct, &UseT::new(UseTInner::UseT(use_op, t_out.dupe()))),
                                    );
                                }
                            }
                            Ok(e)
                        })
                        .collect::<Result<_, AbnormalControlFlow>>()?;
                    (t_out, expressions)
                }
            };
            expression::Expression::new(ExpressionInner::TemplateLiteral {
                loc: (loc, t),
                inner: expression::TemplateLiteral {
                    quasis: inner.quasis.clone(),
                    expressions: expressions.into(),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::JSXElement { inner, .. } => {
            let as_const = *as_const;
            let should_generalize: LazyBool<'a> = {
                let has_hint = has_hint.dupe();
                let loc = loc.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                    should_generalize_jsx(cx, &has_hint, as_const, loc)
                })))
            };
            let (t, e) = jsx(cx, should_generalize, loc.dupe(), inner)?;
            expression::Expression::new(ExpressionInner::JSXElement {
                loc: (loc, t),
                inner: e.into(),
            })
        }
        ExpressionInner::JSXFragment { inner, .. } => {
            let as_const = *as_const;
            let should_generalize: LazyBool<'a> = {
                let has_hint = has_hint.dupe();
                let loc = loc.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                    should_generalize_jsx(cx, &has_hint, as_const, loc)
                })))
            };
            let (t, f) = jsx_fragment(cx, should_generalize, loc.dupe(), inner)?;
            expression::Expression::new(ExpressionInner::JSXFragment {
                loc: (loc, t),
                inner: f.into(),
            })
        }
        ExpressionInner::Class { inner, .. } => {
            let class_loc = loc.dupe();
            let (name_loc, name) = match &inner.id {
                Some(id) => (id.loc.dupe(), id.name.dupe()),
                None => (class_loc.dupe(), FlowSmolStr::from("<<anonymous class>>")),
            };
            let reason = mk_reason(
                VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                name_loc.dupe(),
            );
            match &inner.id {
                Some(_) => {
                    let (class_t, c_typed) = mk_class(
                        cx,
                        class_loc.dupe(),
                        name_loc.dupe(),
                        None,
                        reason.dupe(),
                        inner,
                    )?;
                    // mk_class above ensures that the function name in the inline declaration
                    // has the same type as its references inside the class.
                    // However, in the new env, we need to perform a bind of the class declaration type to the
                    // name to ensure that the environment knows the type of both the declaration and usages.
                    let ord_name = Name::new(name.dupe());
                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                        var: Some(mk_reason(
                            VirtualReasonDesc::RIdentifier(ord_name),
                            name_loc.dupe(),
                        )),
                        init: reason_of_t(&class_t).dupe(),
                    }));
                    type_env::init_implicit_let(cx, &use_op, &class_t, name_loc);
                    expression::Expression::new(ExpressionInner::Class {
                        loc: (class_loc, class_t),
                        inner: c_typed.into(),
                    })
                }
                None => {
                    let (class_t, c_typed) =
                        mk_class(cx, class_loc.dupe(), name_loc, None, reason, inner)?;
                    expression::Expression::new(ExpressionInner::Class {
                        loc: (class_loc, class_t),
                        inner: c_typed.into(),
                    })
                }
            }
        }
        ExpressionInner::Yield { inner, .. } if !inner.delegate => {
            let (t, argument_ast) = match &inner.argument {
                Some(expr) => {
                    let expr_typed = expression(None, None, None, cx, expr)?;
                    let t = expr_typed.loc().1.dupe();
                    (t, Some(expr_typed))
                }
                None => (type_::void::at(loc.dupe()), None),
            };
            let next_t = type_env::get_next(cx, loc.dupe());
            expression::Expression::new(ExpressionInner::Yield {
                loc: (loc.dupe(), next_t),
                inner: expression::Yield {
                    argument: argument_ast,
                    delegate: false,
                    comments: inner.comments.dupe(),
                    result_out: (inner.result_out.dupe(), t),
                }
                .into(),
            })
        }
        ExpressionInner::Yield { inner, .. } => {
            let reason = mk_reason(
                VirtualReasonDesc::RCustom("yield* delegate".into()),
                loc.dupe(),
            );
            let next = type_env::get_next(cx, loc.dupe());
            let (t, argument_ast) = match &inner.argument {
                Some(expr) => {
                    let expr_typed = expression(None, None, None, cx, expr)?;
                    let t = expr_typed.loc().1.dupe();
                    (t, Some(expr_typed))
                }
                None => panic!("delegate yield without argument"),
            };
            let ret_reason = mk_reason(
                VirtualReasonDesc::RCustom("return of child generator in yield* delegate".into()),
                reason.loc().dupe(),
            );
            let ret = flow_typing_tvar::mk(cx, ret_reason);
            let yield_t = flow_typing_tvar::mk(cx, reason.dupe());
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GeneratorYield {
                value: match &inner.argument {
                    Some(expr) => mk_expression_reason(expr),
                    None => reason_of_t(&t).dupe(),
                },
            }));
            // widen yield with the element type of the delegated-to iterable
            let targs = vec![yield_t.dupe(), ret.dupe(), next];
            type_operation_utils::type_assertions::assert_iterable(
                cx,
                loc.dupe(),
                type_env::in_async_scope(cx),
                &use_op,
                &t,
                &targs,
            );
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &ret);
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &yield_t);
            expression::Expression::new(ExpressionInner::Yield {
                loc: (loc.dupe(), ret),
                inner: expression::Yield {
                    argument: argument_ast,
                    delegate: true,
                    comments: inner.comments.dupe(),
                    result_out: (inner.result_out.dupe(), yield_t),
                }
                .into(),
            })
        }
        ExpressionInner::MetaProperty { inner, .. }
            if inner.meta.name.as_str() == "new" && inner.property.name.as_str() == "target" =>
        {
            let t = mixed_t::at(loc.dupe());
            expression::Expression::new(ExpressionInner::MetaProperty {
                loc: (loc.dupe(), t.dupe()),
                inner: expression::MetaProperty {
                    meta: inner.meta.clone(),
                    property: inner.property.clone(),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::MetaProperty { inner, .. }
            if inner.meta.name.as_str() == "import" && inner.property.name.as_str() == "meta" =>
        {
            let reason = mk_reason(VirtualReasonDesc::RImportMeta, loc.dupe());
            let t = flow_js::get_builtin_type_non_speculating(cx, &reason, None, "Import$Meta");
            expression::Expression::new(ExpressionInner::MetaProperty {
                loc: (loc.dupe(), t.dupe()),
                inner: expression::MetaProperty {
                    meta: inner.meta.clone(),
                    property: inner.property.clone(),
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        ExpressionInner::MetaProperty { .. } => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    UnsupportedSyntax::MetaPropertyExpression,
                ))),
            );
            {
                let Ok(v) =
                    polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, &ex);
                v
            }
        }
        ExpressionInner::Import { inner, .. } => {
            let source_loc = inner.argument.loc().dupe();
            let argument = &*inner.argument;
            let t = |module_name: flow_import_specifier::Userland| -> Type {
                let ns_t = {
                    let reason =
                        mk_reason(VirtualReasonDesc::RModule(module_name.dupe()), loc.dupe());
                    let source_module_rc =
                        flow_js_utils::import_export_utils::get_module_type_or_any(
                            cx,
                            true,
                            Some(type_::ImportKind::ImportValue),
                            source_loc.dupe(),
                            module_name.dupe(),
                        )
                        .expect("Should not be under speculation");
                    let source_module: Result<type_::ModuleType, Type> = source_module_rc;
                    let namespace_symbol = flow_common::flow_symbol::Symbol::mk_module_symbol(
                        module_name.into_inner(),
                        loc.dupe(),
                    );
                    flow_js_utils::import_export_utils::get_module_namespace_type(
                        cx,
                        reason,
                        namespace_symbol,
                        &source_module,
                    )
                    .expect("Should not be under speculation")
                };
                let reason = mk_annot_reason(VirtualReasonDesc::RAsyncImport, loc.dupe());
                FlowJs::get_builtin_typeapp(cx, &reason, None, "Promise", vec![ns_t])
            };
            let opts_prime = inner
                .options
                .as_ref()
                .map(|o| expression(None, None, None, cx, o))
                .transpose()?;
            match argument {
                ExpressionInner::StringLiteral { inner: lit, .. } => {
                    let module_name =
                        flow_import_specifier::Userland::from_smol_str(lit.value.dupe());
                    let t = t(module_name);
                    expression::Expression::new(ExpressionInner::Import {
                        loc: (loc, t.dupe()),
                        inner: expression::Import {
                            argument: expression::Expression::new(ExpressionInner::StringLiteral {
                                loc: (source_loc, t),
                                inner: lit.clone(),
                            }),
                            options: opts_prime,
                            comments: inner.comments.dupe(),
                        }
                        .into(),
                    })
                }
                ExpressionInner::TemplateLiteral { inner: lit, .. }
                    if lit.quasis.len() == 1 && lit.expressions.is_empty() =>
                {
                    let quasi = &lit.quasis[0];
                    let module_name = &quasi.value.cooked;
                    let module_name_userland =
                        flow_import_specifier::Userland::from_smol_str(module_name.dupe());
                    let t = t(module_name_userland);
                    expression::Expression::new(ExpressionInner::Import {
                        loc: (loc, t.dupe()),
                        inner: (expression::Import {
                            argument: expression::Expression::new(
                                ExpressionInner::TemplateLiteral {
                                    loc: (source_loc, t),
                                    inner: (expression::TemplateLiteral {
                                        quasis: lit.quasis.clone(),
                                        expressions: std::sync::Arc::from([]),
                                        comments: lit.comments.dupe(),
                                    })
                                    .into(),
                                },
                            ),
                            options: opts_prime,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                _ => {
                    let ignore_non_literals = cx.should_ignore_non_literal_requires();
                    if !ignore_non_literals {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                loc.dupe(),
                                UnsupportedSyntax::ImportDynamicArgument,
                            ))),
                        );
                        {
                            let Ok(v) = polymorphic_ast_mapper::expression(
                                &mut typed_ast_utils::ErrorMapper,
                                &ex,
                            );
                            v
                        }
                    } else {
                        {
                            let Ok(v) = polymorphic_ast_mapper::expression(
                                &mut typed_ast_utils::UncheckedMapper,
                                &ex,
                            );
                            v
                        }
                    }
                }
            }
        }
    })
}

/// Handles operations that may traverse optional chains
///
/// Returns a tuple:
/// * type of expression if no optional chains short-circuited,
/// * a list of void types of all possible short-circuitings,
/// * typed AST of expression, where the type is the combination of
///   short-circuiting and non short-circuiting (i.e. representing the actual
///   range of possible types of the expression)
pub fn optional_chain<'a>(
    encl_ctx: EnclosingContext,
    cx: &Context<'a>,
    expr: &expression::Expression<ALoc, ALoc>,
) -> Result<(Type, Vec<Type>, expression::Expression<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    use ast::expression::ExpressionInner;
    let loc = expr.loc().dupe();
    let e: &ExpressionInner<ALoc, ALoc> = expr.deref();
    let ex = expr;

    let normalize_voided_out = |t: Type| -> Vec<Type> {
        let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason_of_t(&t), &t)
            .expect("should not fail outside speculation");
        for t in &ts {
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, t);
        }
        ts
    };

    let factor_out_optional = |factored_loc: ALoc,
                               e: &ExpressionInner<ALoc, ALoc>|
     -> (
        ExpressionInner<ALoc, ALoc>,
        OptState,
        Box<
            dyn Fn(
                    Type,
                    Type,
                    expression::Call<ALoc, (ALoc, Type)>,
                ) -> ExpressionInner<ALoc, (ALoc, Type)>
                + '_,
        >,
        Box<
            dyn Fn(
                    expression::Member<ALoc, (ALoc, Type)>,
                    Type,
                ) -> ExpressionInner<ALoc, (ALoc, Type)>
                + '_,
        >,
    ) {
        let (opt_state, filtered_out_loc, e_prime) = match e {
            ExpressionInner::OptionalCall { inner, .. } => {
                let call = &inner.call;
                let optional = &inner.optional;
                let filtered_out = &inner.filtered_out;
                let opt_state = match optional {
                    expression::OptionalCallKind::Optional => OptState::NewChain,
                    expression::OptionalCallKind::NonOptional => OptState::ContinueChain,
                    expression::OptionalCallKind::AssertNonnull => {
                        if !cx.assert_operator_enabled() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    factored_loc.dupe(),
                                    UnsupportedSyntax::NonnullAssertion,
                                ))),
                            );
                        }
                        OptState::AssertChain
                    }
                };
                (
                    opt_state,
                    filtered_out.dupe(),
                    ExpressionInner::Call {
                        loc: call.callee.loc().dupe(),
                        inner: std::sync::Arc::new(call.clone()),
                    },
                )
            }
            ExpressionInner::OptionalMember { inner, .. } => {
                let member = &inner.member;
                let optional = &inner.optional;
                let filtered_out = &inner.filtered_out;
                let opt_state = match optional {
                    expression::OptionalMemberKind::Optional => OptState::NewChain,
                    expression::OptionalMemberKind::NonOptional => OptState::ContinueChain,
                    expression::OptionalMemberKind::AssertNonnull => {
                        if !cx.assert_operator_enabled() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    factored_loc.dupe(),
                                    UnsupportedSyntax::NonnullAssertion,
                                ))),
                            );
                        }
                        OptState::AssertChain
                    }
                };
                (
                    opt_state,
                    filtered_out.dupe(),
                    ExpressionInner::Member {
                        loc: member.object.loc().dupe(),
                        inner: std::sync::Arc::new(member.clone()),
                    },
                )
            }
            _ => (OptState::NonOptional, loc.dupe(), e.clone()),
        };
        let call_ast = {
            let filtered_out_loc = filtered_out_loc.dupe();
            let opt_state = opt_state.clone();
            Box::new(
                move |ty: Type,
                      sig_help: Type,
                      call: expression::Call<ALoc, (ALoc, Type)>|
                      -> ExpressionInner<ALoc, (ALoc, Type)> {
                    let loc = call.callee.loc().0.dupe();
                    cx.set_signature_help_callee(loc.dupe(), sig_help);
                    match opt_state {
                        OptState::NewChain => ExpressionInner::OptionalCall {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalCall {
                                call,
                                optional: expression::OptionalCallKind::Optional,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::ContinueChain => ExpressionInner::OptionalCall {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalCall {
                                call,
                                optional: expression::OptionalCallKind::NonOptional,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::AssertChain => ExpressionInner::OptionalCall {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalCall {
                                call,
                                optional: expression::OptionalCallKind::AssertNonnull,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::NonOptional => ExpressionInner::Call {
                            loc: (loc.dupe(), ty),
                            inner: call.into(),
                        },
                    }
                },
            )
        };
        let member_ast = {
            let loc = loc.dupe();
            let filtered_out_loc = filtered_out_loc.dupe();
            let opt_state = opt_state.clone();
            Box::new(
                move |member: expression::Member<ALoc, (ALoc, Type)>,
                      ty: Type|
                      -> ExpressionInner<ALoc, (ALoc, Type)> {
                    match opt_state {
                        OptState::NewChain => ExpressionInner::OptionalMember {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalMember {
                                member,
                                optional: expression::OptionalMemberKind::Optional,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::ContinueChain => ExpressionInner::OptionalMember {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalMember {
                                member,
                                optional: expression::OptionalMemberKind::NonOptional,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::AssertChain => ExpressionInner::OptionalMember {
                            loc: (loc.dupe(), ty.dupe()),
                            inner: (expression::OptionalMember {
                                member,
                                optional: expression::OptionalMemberKind::AssertNonnull,
                                filtered_out: (filtered_out_loc.dupe(), ty),
                            })
                            .into(),
                        },
                        OptState::NonOptional => ExpressionInner::Member {
                            loc: (loc.dupe(), ty),
                            inner: member.into(),
                        },
                    }
                },
            )
        };
        (e_prime, opt_state, call_ast, member_ast)
    };

    let try_non_chain = |cx: &Context<'a>,
                         loc: ALoc,
                         e: &ExpressionInner<ALoc, ALoc>,
                         call_ast: &dyn Fn(
        Type,
        Type,
        expression::Call<ALoc, (ALoc, Type)>,
    ) -> ExpressionInner<ALoc, (ALoc, Type)>,
                         member_ast: &dyn Fn(
        expression::Member<ALoc, (ALoc, Type)>,
        Type,
    ) -> ExpressionInner<ALoc, (ALoc, Type)>|
     -> Result<
        Option<((ALoc, Type), ExpressionInner<ALoc, (ALoc, Type)>)>,
        AbnormalControlFlow,
    > {
        // Special cases where optional chaining doesn't occur
        Ok(match e {
            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Identifier {
                    loc: id_loc,
                    inner: callee_id,
                } = inner.callee.deref()
                    && callee_id.name.as_str() == "require"
                    && !type_env::local_scope_entry_exists(cx, id_loc.dupe()) =>
            {
                let callee_loc = inner.callee.loc().dupe();
                let targs = inner.targs.as_ref().map(|targs| {
                    let (_, tast) = convert_call_targs(cx, &FlowOrdMap::default(), targs);
                    tast
                });
                let (lhs_t, arguments) = match (targs.as_ref(), &inner.arguments) {
                    (None, args)
                        if args.arguments.len() == 1
                            && let Some(expression::ExpressionOrSpread::Expression(lit_exp)) =
                                args.arguments.first()
                            && let Some(module_name_str) = match lit_exp.deref() {
                                ExpressionInner::StringLiteral { inner, .. } => {
                                    Some(inner.value.dupe())
                                }
                                ExpressionInner::TemplateLiteral { inner, .. }
                                    if inner.quasis.len() == 1 && inner.expressions.is_empty() =>
                                {
                                    Some(inner.quasis[0].value.cooked.dupe())
                                }
                                _ => None,
                            } =>
                    {
                        let source_loc = lit_exp.loc().dupe();
                        let module_name =
                            flow_import_specifier::Userland::from_smol_str(module_name_str);
                        let source_module_rc =
                            flow_js_utils::import_export_utils::get_module_type_or_any(
                                cx,
                                true,
                                Some(type_::ImportKind::ImportValue),
                                source_loc.dupe(),
                                module_name.dupe(),
                            )
                            .expect("Should not be under speculation");
                        let source_module: Result<type_::ModuleType, Type> = source_module_rc;
                        let (def_loc_opt, require_t) =
                            flow_js_utils::import_export_utils::cjs_require_type(
                                cx,
                                mk_reason(
                                    VirtualReasonDesc::RModule(module_name.dupe()),
                                    loc.dupe(),
                                ),
                                flow_js::reposition_non_speculating,
                                flow_common::flow_symbol::Symbol::mk_module_symbol(
                                    module_name.dupe().into_inner(),
                                    loc.dupe(),
                                ),
                                false,
                                &source_module,
                            )
                            .expect("Should not be under speculation");
                        let lit_exp_ast = expression(None, None, None, cx, lit_exp)?;
                        let lit_exp_l = lit_exp_ast.loc().0.dupe();
                        let lit_exp_t = mod_reason_of_t(
                            &|_| match &def_loc_opt {
                                Some(def_loc) => mk_reason(
                                    VirtualReasonDesc::RModule(module_name.dupe()),
                                    def_loc.dupe(),
                                )
                                .reposition(loc.dupe()),
                                None => reason_of_t(&require_t).dupe().reposition(loc.dupe()),
                            },
                            &lit_exp_ast.loc().1,
                        );
                        let mut lit_exp_inner = (*lit_exp_ast.0).clone();
                        *lit_exp_inner.loc_mut() = (lit_exp_l, lit_exp_t);
                        let lit_exp_typed = expression::Expression::new(lit_exp_inner);
                        (
                            require_t,
                            expression::ArgList {
                                loc: args.loc.dupe(),
                                arguments: vec![ast::expression::ExpressionOrSpread::Expression(
                                    lit_exp_typed,
                                )]
                                .into(),
                                comments: args.comments.dupe(),
                            },
                        )
                    }
                    (Some(_), _) => {
                        arg_list(cx, &inner.arguments)?;
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                                call_loc: loc.dupe(),
                                is_new: false,
                                reason_arity: locationless_reason(VirtualReasonDesc::RFunction(
                                    ReasonDescFunction::RNormal,
                                )),
                                expected_arity: 0,
                            })),
                        );
                        let t = any_t::at(AnySource::AnyError(None), loc.dupe());
                        (t, {
                            let Ok(v) = polymorphic_ast_mapper::arg_list(
                                &mut typed_ast_utils::ErrorMapper,
                                &inner.arguments,
                            );
                            v
                        })
                    }
                    (None, _) => {
                        arg_list(cx, &inner.arguments)?;
                        let ignore_non_literals = cx.should_ignore_non_literal_requires();
                        if !ignore_non_literals {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    loc.dupe(),
                                    UnsupportedSyntax::RequireDynamicArgument,
                                ))),
                            );
                        }
                        let t = any_t::at(AnySource::AnyError(None), loc.dupe());
                        (t, {
                            let Ok(v) = polymorphic_ast_mapper::arg_list(
                                &mut typed_ast_utils::ErrorMapper,
                                &inner.arguments,
                            );
                            v
                        })
                    }
                };
                let id_t = mixed_t::at(callee_loc.dupe());
                Some((
                    (loc.dupe(), lhs_t.dupe()),
                    call_ast(
                        lhs_t,
                        id_t.dupe(),
                        expression::Call {
                            callee: expression::Expression::new(ExpressionInner::Identifier {
                                loc: (callee_loc, id_t.dupe()),
                                inner: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc.dupe(), id_t),
                                    name: callee_id.name.dupe(),
                                    comments: callee_id.comments.dupe(),
                                }),
                            }),
                            targs,
                            arguments,
                            comments: inner.comments.dupe(),
                        },
                    ),
                ))
            }

            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Member {
                    inner: member,
                    loc: callee_loc_pair,
                    ..
                } = inner.callee.deref()
                    && let ExpressionInner::Identifier { inner: obj_id, .. } =
                        member.object.deref()
                    && obj_id.name.as_str() == "Object"
                    && let expression::member::Property::PropertyIdentifier(prop_id) =
                        &member.property =>
            {
                let callee_loc = callee_loc_pair.dupe();
                let prop_loc = prop_id.loc.dupe();
                let name = &prop_id.name;
                let member_comments = &member.comments;
                let obj_ast = expression(None, None, None, cx, &member.object)?;
                let obj_t = obj_ast.loc().1.dupe();
                let (lhs_t, targs, arguments) = static_method_call_object(
                    cx,
                    loc.dupe(),
                    callee_loc.dupe(),
                    prop_loc.dupe(),
                    &inner.callee,
                    obj_t,
                    name,
                    inner.targs.as_ref(),
                    &inner.arguments,
                )?;
                let t = mixed_t::at(callee_loc.dupe());
                Some((
                    (loc.dupe(), lhs_t.dupe()),
                    call_ast(
                        lhs_t,
                        t.dupe(),
                        expression::Call {
                            // TODO(vijayramamurthy): what is the type of `Object.name`
                            callee: expression::Expression::new(ExpressionInner::Member {
                                loc: (callee_loc, t.dupe()),
                                inner: (expression::Member {
                                    object: obj_ast,
                                    property: expression::member::Property::PropertyIdentifier(
                                        ast::Identifier::new(ast::IdentifierInner {
                                            loc: (prop_loc, t),
                                            name: name.dupe(),
                                            comments: prop_id.comments.dupe(),
                                        }),
                                    ),
                                    comments: member_comments.clone(),
                                })
                                .into(),
                            }),
                            targs,
                            arguments,
                            comments: inner.comments.dupe(),
                        },
                    ),
                ))
            }
            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Member {
                    inner: member,
                    loc: callee_loc_pair,
                    ..
                } = inner.callee.deref()
                    && let ExpressionInner::Super {
                        loc: super_loc,
                        inner: super_inner,
                    } = member.object.deref()
                    && let expression::member::Property::PropertyIdentifier(id) =
                        &member.property =>
            {
                let callee_loc = callee_loc_pair.dupe();
                let ploc = id.loc.dupe();
                let name = &id.name;
                let member_comments = &member.comments;
                let reason = mk_reason(
                    VirtualReasonDesc::RMethodCall(Some(name.dupe())),
                    loc.dupe(),
                );
                let name = Name::new(name.dupe());
                let reason_lookup = mk_reason(RProperty(Some(name.dupe())), callee_loc.dupe());
                let reason_prop = mk_reason(RProperty(Some(name.dupe())), ploc.dupe());
                let super_t = super_(cx, super_loc.dupe());
                let meth_generic_this = flow_typing_tvar::mk(cx, reason.dupe());
                let (targts, targs) = convert_call_targs_opt(cx, inner.targs.as_ref());
                let (argts, arguments_ast) = arg_list(cx, &inner.arguments)?;
                let specialized_callee = cx.new_specialized_callee();
                let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                    cx,
                    reason.dupe(),
                    |cx, t_reason, t_id| {
                        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                        let methodcalltype = type_::mk_methodcalltype(
                            targts.map(|v| v.into()),
                            argts.into(),
                            Some(meth_generic_this.dupe()),
                            true,
                            tvar,
                        );
                        let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCallMethod(
                            Box::new(FunCallMethodData {
                                op: mk_expression_reason(ex),
                                fn_: mk_expression_reason(&inner.callee),
                                prop: reason_prop.dupe(),
                                args: mk_initial_arguments_reason(&inner.arguments).into(),
                                local: true,
                            }),
                        )));
                        let propref = mk_named_prop(reason_prop.dupe(), false, name.dupe());
                        let use_t = UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                            use_op,
                            reason: reason.dupe(),
                            prop_reason: reason_lookup.dupe(),
                            propref: Box::new(propref),
                            method_action: Box::new(type_::MethodAction::CallM(Box::new(
                                type_::CallMData {
                                    methodcalltype,
                                    return_hint: type_env::get_hint(cx, loc.dupe()),
                                    specialized_callee: Some(specialized_callee.clone()),
                                },
                            ))),
                        })));
                        flow_js::flow_non_speculating(cx, (&super_t, &use_t));
                    },
                );
                let prop_t = flow_js_utils::callee_recorder::type_for_tast(
                    reason_lookup.dupe(),
                    &specialized_callee,
                );
                Some((
                    (loc.dupe(), lhs_t.dupe()),
                    call_ast(
                        lhs_t,
                        prop_t.dupe(),
                        expression::Call {
                            callee: expression::Expression::new(ExpressionInner::Member {
                                loc: (callee_loc, prop_t.dupe()),
                                inner: (expression::Member {
                                    object: expression::Expression::new(ExpressionInner::Super {
                                        loc: (super_loc.dupe(), super_t),
                                        inner: super_inner.clone(),
                                    }),
                                    property: expression::member::Property::PropertyIdentifier(
                                        ast::Identifier::new(ast::IdentifierInner {
                                            loc: (ploc, prop_t),
                                            name: id.name.dupe(),
                                            comments: id.comments.dupe(),
                                        }),
                                    ),
                                    comments: member_comments.clone(),
                                })
                                .into(),
                            }),
                            targs,
                            arguments: arguments_ast,
                            comments: inner.comments.dupe(),
                        },
                    ),
                ))
            }
            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Super {
                    loc: super_loc,
                    inner: super_inner,
                } = inner.callee.deref() =>
            {
                let (targts, targs) = convert_call_targs_opt(cx, inner.targs.as_ref());
                let reason = mk_reason(
                    VirtualReasonDesc::RFunctionCall(VirtualReasonDesc::RSuper.into()),
                    loc.dupe(),
                );
                let super_t = super_(cx, super_loc.dupe());
                let (argts, arguments_ast) = arg_list(cx, &inner.arguments)?;
                let super_reason = reason_of_t(&super_t).dupe();
                let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                    cx,
                    reason.dupe(),
                    |cx, t_reason, t_id| {
                        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                        let methodcalltype = type_::mk_methodcalltype(
                            targts.map(|v| v.into()),
                            argts.into(),
                            None,
                            true,
                            tvar,
                        );
                        let propref = mk_named_prop(
                            super_reason.dupe(),
                            false,
                            Name::new(FlowSmolStr::new_inline("constructor")),
                        );
                        let use_op =
                            UseOp::Op(Arc::new(type_::RootUseOp::FunCall(Box::new(FunCallData {
                                op: mk_expression_reason(ex),
                                fn_: mk_expression_reason(&inner.callee),
                                args: mk_initial_arguments_reason(&inner.arguments).into(),
                                local: true,
                            }))));
                        let use_t = UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                            use_op,
                            reason: reason.dupe(),
                            prop_reason: super_reason.dupe(),
                            propref: Box::new(propref),
                            method_action: Box::new(type_::MethodAction::CallM(Box::new(
                                type_::CallMData {
                                    methodcalltype,
                                    return_hint: type_::hint_unavailable(),
                                    specialized_callee: None,
                                },
                            ))),
                        })));
                        flow_js::flow_non_speculating(cx, (&super_t, &use_t));
                    },
                );
                Some((
                    (loc.dupe(), lhs_t.dupe()),
                    call_ast(
                        lhs_t,
                        super_t.dupe(),
                        expression::Call {
                            callee: expression::Expression::new(ExpressionInner::Super {
                                loc: (super_loc.dupe(), super_t),
                                inner: super_inner.clone(),
                            }),
                            targs,
                            arguments: arguments_ast,
                            comments: inner.comments.dupe(),
                        },
                    ),
                ))
            }
            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Identifier {
                    loc: callee_id_loc,
                    inner: callee_id,
                } = inner.callee.deref()
                    && callee_id.name.as_str()
                        == "$Flow$DebugSleep$DO_NOT_USE_IN_PRODUCTION_CODE_OR_YOU_WILL_BE_FIRED"
                    && inner.targs.is_none()
                    && inner.arguments.arguments.len() == 1
                    && let Some(expression::ExpressionOrSpread::Expression(lit_exp)) =
                        inner.arguments.arguments.first()
                    && let ExpressionInner::NumberLiteral {
                        loc: lit_exp_loc,
                        inner: num_lit,
                    } = lit_exp.deref()
                    && !type_env::local_scope_entry_exists(cx, callee_id_loc.dupe()) =>
            {
                let callee_loc = inner.callee.loc().dupe();
                let args_loc = inner.arguments.loc.dupe();
                let args_comments = &inner.arguments.comments;
                let value = num_lit.value;
                fn any_t_with_loc(l: ALoc) -> (ALoc, Type) {
                    (
                        l.dupe(),
                        any_t::untyped(mk_reason(VirtualReasonDesc::RAnyImplicit, l)),
                    )
                }
                let mut n = value;
                while n > 0.0 {
                    flow_utils_concurrency::worker_cancel::check_should_cancel();
                    std::thread::sleep(std::time::Duration::from_secs_f64(n.min(1.0)));
                    n -= 1.0;
                }
                Some((
                    any_t_with_loc(loc.dupe()),
                    ExpressionInner::Call {
                        loc: any_t_with_loc(loc.dupe()),
                        inner: (expression::Call {
                            callee: expression::Expression::new(ExpressionInner::Identifier {
                                loc: any_t_with_loc(callee_loc.dupe()),
                                inner: ast::Identifier::new(ast::IdentifierInner {
                                    loc: any_t_with_loc(callee_id_loc.dupe()),
                                    name: callee_id.name.dupe(),
                                    comments: callee_id.comments.dupe(),
                                }),
                            }),
                            targs: None,
                            arguments: expression::ArgList {
                                loc: args_loc,
                                arguments: vec![ast::expression::ExpressionOrSpread::Expression(
                                    ast::expression::Expression::new(
                                        ExpressionInner::NumberLiteral {
                                            loc: any_t_with_loc(lit_exp_loc.dupe()),
                                            inner: num_lit.dupe(),
                                        },
                                    ),
                                )]
                                .into(),
                                comments: args_comments.dupe(),
                            },
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    },
                ))
            }
            ExpressionInner::Call { inner, .. }
                if let ExpressionInner::Identifier {
                    loc: callee_id_loc,
                    inner: callee_id,
                } = inner.callee.deref()
                    && callee_id.name.as_str()
                        == "$Flow$DebugThrow$DO_NOT_USE_IN_PRODUCTION_CODE_OR_YOU_WILL_BE_FIRED"
                    && inner.targs.is_none()
                    && !type_env::local_scope_entry_exists(cx, callee_id_loc.dupe()) =>
            {
                panic!("EDebugThrow:{:?}", loc);
            }
            // See ~/www/static_upstream/core/
            ExpressionInner::Call { inner, .. }
                if flow_parser::ast_utils::is_call_to_invariant(&inner.callee) =>
            {
                // TODO: require
                let callee_ast = expression(None, None, None, cx, &inner.callee)?;
                let callee_t = callee_ast.loc().1.dupe();
                let targs = inner.targs.as_ref().map(|targs| {
                    let (_, tast) = convert_call_targs(cx, &FlowOrdMap::default(), targs);
                    tast
                });
                // NOTE: if an invariant expression throws abnormal control flow, the
                // entire statement it was in is reconstructed in the typed AST as an
                // expression statement containing just the invariant call. This should
                // be ok for the most part since this is the most common way to call
                // invariant. It's worth experimenting with whether people use invariant
                // in other ways, and if not, restricting it to this pattern.
                let args = &inner.arguments;
                let (t, arguments) = match (inner.targs.as_ref(), args.arguments.first()) {
                    (None, None) => {
                        let t = empty_t::at(loc.dupe());
                        // invariant() is treated like a throw
                        let ast_expr = expression::Expression::new(ExpressionInner::Call {
                            loc: (loc.dupe(), t.dupe()),
                            inner: (expression::Call {
                                callee: callee_ast.dupe(),
                                targs: targs.clone(),
                                arguments: expression::ArgList {
                                    loc: args.loc.dupe(),
                                    arguments: std::sync::Arc::from([]),
                                    comments: args.comments.dupe(),
                                },
                                comments: inner.comments.dupe(),
                            })
                            .into(),
                        });
                        return Err(AbnormalControlFlow(loc.dupe(), ast_expr));
                    }
                    (None, Some(expression::ExpressionOrSpread::Expression(lit_exp_src)))
                        if matches!(
                            lit_exp_src.deref(),
                            ExpressionInner::BooleanLiteral { inner, .. }
                                if !inner.value
                        ) =>
                    {
                        // invariant(false, ...) is treated like a throw
                        let rest_arguments: Vec<_> = args
                            .arguments
                            .iter()
                            .skip(1)
                            .map(|a| Ok(expression_or_spread(cx, a)?.1))
                            .collect::<Result<_, AbnormalControlFlow>>()?;
                        let lit_exp = expression(None, None, None, cx, lit_exp_src)?;
                        let t = empty_t::at(loc.dupe());
                        let mut all_arguments = Vec::with_capacity(1 + rest_arguments.len());
                        all_arguments.push(expression::ExpressionOrSpread::Expression(lit_exp));
                        all_arguments.extend(rest_arguments);
                        let ast_expr = expression::Expression::new(ExpressionInner::Call {
                            loc: (loc.dupe(), t.dupe()),
                            inner: (expression::Call {
                                callee: callee_ast.dupe(),
                                targs: targs.clone(),
                                arguments: expression::ArgList {
                                    loc: args.loc.dupe(),
                                    arguments: all_arguments.into(),
                                    comments: args.comments.dupe(),
                                },
                                comments: inner.comments.dupe(),
                            })
                            .into(),
                        });
                        return Err(AbnormalControlFlow(loc.dupe(), ast_expr));
                    }
                    (None, Some(expression::ExpressionOrSpread::Expression(cond_src))) => {
                        let rest_arguments: Vec<_> = args
                            .arguments
                            .iter()
                            .skip(1)
                            .map(|a| Ok(expression_or_spread(cx, a)?.1))
                            .collect::<Result<_, AbnormalControlFlow>>()?;
                        let cond = condition(
                            cx,
                            EnclosingContext::OtherTestContext,
                            None,
                            None,
                            cond_src,
                        )?;
                        let cond_t = cond.loc().1.dupe();
                        let concretized_cond_t = FlowJs::singleton_concrete_type_for_inspection(
                            cx,
                            reason_of_t(&cond_t),
                            &cond_t,
                        )
                        .expect("Should not be under speculation");
                        match concretized_cond_t.deref() {
                            // If condition is empty, it means that the branch is unreachable. The invariant is
                            // still useless, but it's not useless because it's always truthy, which is what the
                            // code below tries to report.
                            TypeInner::DefT(_, def_t)
                                if matches!(def_t.deref(), DefTInner::EmptyT) => {}
                            // any will fail the test below, but it's not always truthy.
                            TypeInner::AnyT(_, _) => {}
                            _ => {
                                let filter_result = flow_typing_utils::type_filter::not_truthy(
                                    cx,
                                    concretized_cond_t.dupe(),
                                );
                                match filter_result.type_.deref() {
                                    TypeInner::DefT(_, def_t)
                                        if matches!(def_t.deref(), DefTInner::EmptyT) =>
                                    {
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnnecessaryInvariant(Box::new((
                                                loc.dupe(),
                                                reason_of_t(&concretized_cond_t).dupe(),
                                            ))),
                                        );
                                    }
                                    _ => {}
                                }
                            }
                        }
                        let t = type_::void::at(loc.dupe());
                        let mut all_arguments = Vec::with_capacity(1 + rest_arguments.len());
                        all_arguments.push(expression::ExpressionOrSpread::Expression(cond));
                        all_arguments.extend(rest_arguments);
                        (
                            t,
                            expression::ArgList {
                                loc: args.loc.dupe(),
                                arguments: all_arguments.into(),
                                comments: args.comments.dupe(),
                            },
                        )
                    }
                    (_, Some(expression::ExpressionOrSpread::Spread(_))) => {
                        arg_list(cx, &inner.arguments)?;
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                loc.dupe(),
                                UnsupportedSyntax::InvariantSpreadArgument,
                            ))),
                        );
                        let t = any_t::at(AnySource::AnyError(None), loc.dupe());
                        (t, {
                            let Ok(v) = polymorphic_ast_mapper::arg_list(
                                &mut typed_ast_utils::ErrorMapper,
                                &inner.arguments,
                            );
                            v
                        })
                    }
                    (Some(_), _) => {
                        arg_list(cx, &inner.arguments)?;
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                                call_loc: loc.dupe(),
                                is_new: false,
                                reason_arity: locationless_reason(VirtualReasonDesc::RFunction(
                                    ReasonDescFunction::RNormal,
                                )),
                                expected_arity: 0,
                            })),
                        );
                        let t = any_t::at(AnySource::AnyError(None), loc.dupe());
                        (t, {
                            let Ok(v) = polymorphic_ast_mapper::arg_list(
                                &mut typed_ast_utils::ErrorMapper,
                                &inner.arguments,
                            );
                            v
                        })
                    }
                };
                Some((
                    (loc.dupe(), t.dupe()),
                    call_ast(
                        t.dupe(),
                        callee_t,
                        expression::Call {
                            callee: callee_ast,
                            targs,
                            arguments,
                            comments: inner.comments.dupe(),
                        },
                    ),
                ))
            }
            ExpressionInner::Member { inner, .. }
                if let ExpressionInner::Super {
                    loc: super_loc,
                    inner: super_inner,
                } = inner.object.deref()
                    && let expression::member::Property::PropertyIdentifier(id) =
                        &inner.property =>
            {
                let member = inner.as_ref();
                let ploc = &id.loc;
                let name = &id.name;
                let comments = &member.comments;
                let super_t = super_(cx, super_loc.dupe());
                let prop_name = Name::new(name.dupe());
                let expr_reason = mk_reason(RProperty(Some(prop_name.dupe())), loc.dupe());
                let prop_reason = mk_reason(RProperty(Some(prop_name.dupe())), ploc.dupe());
                let lhs_t = match refinement::get(true, cx, ex, loc.dupe()) {
                    Some(t) => t,
                    None => {
                        let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(
                            mk_expression_reason(ex),
                        )));
                        get_prop(
                            encl_ctx.dupe(),
                            cx,
                            expr_reason.dupe(),
                            use_op,
                            type_env::get_hint(cx, loc.dupe()),
                            super_t.dupe(),
                            prop_reason,
                            name,
                        )
                    }
                };
                //     let property = Member.PropertyIdentifier ((ploc, lhs_t), id) in
                let property = expression::member::Property::PropertyIdentifier(
                    ast::Identifier::new(ast::IdentifierInner {
                        loc: (ploc.dupe(), lhs_t.dupe()),
                        name: id.name.dupe(),
                        comments: id.comments.dupe(),
                    }),
                );
                let ast_inner = member_ast(
                    expression::Member {
                        object: expression::Expression::new(ExpressionInner::Super {
                            loc: (super_loc.dupe(), super_t),
                            inner: super_inner.clone(),
                        }),
                        property,
                        comments: comments.clone(),
                    },
                    lhs_t.dupe(),
                );
                Some(((loc.dupe(), lhs_t), ast_inner))
            }
            ExpressionInner::Call { inner, .. } if cx.enable_jest_integration() => {
                if let Some((jest_loc, source_loc, module_name)) =
                    flow_parser::ast_utils::get_call_to_jest_module_mocking_fn(
                        &inner.callee,
                        &inner.arguments,
                    )
                {
                    if !type_env::local_scope_entry_exists(cx, jest_loc.dupe()) {
                        match flow_js_utils::import_export_utils::get_module_type_or_any(
                            cx,
                            false,
                            None,
                            source_loc.dupe(),
                            flow_import_specifier::Userland::from_smol_str(module_name.dupe()),
                        )
                        .expect("Should not be under speculation")
                        {
                            Ok(_) | Err(_) => {}
                        }
                    }
                }
                None
            }
            _ => None,
        })
    };

    let (e_prime, opt_state, call_ast, member_ast) = factor_out_optional(loc.dupe(), e);

    // When traversing an optional chain, we need to track the "successful" types
    // (if all optional chain operators in the sequence filtered out null/void),
    // the nullish results if any, from the possibility of the optional chain
    // short-circuiting (there may be multiple sources of null, from multiple
    // chain operators in the chain) and the "actual"/final type of the overall
    // expression, which can be seen as a union of the successful type and all
    // possible nullish failure types.
    //
    // The optional_chain function therefore returns a 5-tuple:
    //   * T1: the type of the expression modulo optional chaining--i.e., the
    //     type in the case where any optional chain tests succeed,
    //   * T2: a list of types representing the union of all optional chain
    //     *failures*, if they may exist
    //   * exp: the typed AST expression, where the type of the node is the
    //     "actual" type of the expression, including both chain failures and
    //     chain successes.
    //
    // So, if `a: ?{b?: {c: number}}`, and the checked expression is `a?.b?.c`,
    //   then the output would be (T1, T2, T3, exp), where:
    //   * T1 = number
    //   * T2 = void, both from `a: ?{...}` and from `a: {b? : {...}}`
    //   * exp = ast for `a?.b?.c` with type T1 U T2
    //
    // Below are several helper functions for setting up this tuple in the
    // presence of chaining.
    let join_optional_branches = |voided: &[Type], filtered: &Type| -> Type {
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason_of_t(filtered).dupe(), |cx, t| {
            flow_js::flow_t_non_speculating(cx, (filtered, t));
            for void_t in voided {
                flow_js::flow_t_non_speculating(cx, (void_t, t));
            }
        })
    };

    fn noop() -> Option<Type> {
        None
    }

    fn handle_new_chain<'a, A, B>(
        cx: &Context<'a>,
        assertion: bool,
        conf: ChainingConf<'a, '_, A, B>,
        lhs_reason: Reason,
        loc: ALoc,
        chain_t: Type,
        voided_t: Vec<Type>,
        object_ast: expression::Expression<ALoc, (ALoc, Type)>,
    ) -> Result<
        (
            Type,
            Vec<Type>,
            Type,
            Type,
            expression::Expression<ALoc, (ALoc, Type)>,
            B,
        ),
        AbnormalControlFlow,
    > {
        let ChainingConf {
            subexpressions,
            get_reason,
            get_opt_use,
            ..
        } = conf;
        // We've encountered an optional chaining operator.
        // We need to flow the "success" type of obj_ into a OptionalChain.run,
        // which will "filter out" VoidT and NullT from the type of
        // obj_ and flow them into `voided_out`, and then flow any non-void
        // type into a use_t created by applying an opt_use_t (representing the
        // operation that will occur on the upper bound) to a new "output" tvar.
        //
        // This might not be the first optional chain operator in the chain, so
        // we need to take chain_t, which is equivalent to T1 above and
        // represents the result if any previous operator succeeded--this is the
        // type that we want to flow into the OptionalChain.run, because if the
        // previous operator failed we wouldn't reach this point in the chain in
        // the first place. We also take voided_t, equivalent to T2 above and
        // representing any previous chain short-circuits, and it will
        // contribute to the failure/short-circuit output of this function,
        // `voided_out`.
        //
        // Method calls need a little bit of extra support, because MethodT
        // acts as both a lookup and a call. Suppose `a: ?{b?: () => number}`
        // and `a?.b?.().` We need to generate a funcalltype for the call to
        // () => number, and funcalltypes include the receiver ("this") of the
        // call. However, we don't want the receiver to be the type of `a`,
        // ?{b?: () => number}, because before calling the method, we've
        // already filtered out the nullish case on `a`. The receiver instead
        // should be {b?: () => number} (not optional). The bind_t parameter is
        // (if present) the receiver of the method call, and is included in the
        // OptionalChain.run; see the rules in flow_js for how it's used, but
        // essentially the successfully filtered receiver of the function call
        // is flowed into it, and it is used as the `this`-parameter of the
        // calltype that the method call will flow into.
        let (subexpression_types, subexpression_asts) = subexpressions(cx)?;
        let reason = get_reason(&chain_t);
        let chain_reason = mk_reason(ROptionalChain, loc.dupe());
        let mem_tvar_id = flow_typing_tvar::mk_no_wrap(cx, &reason);
        let mem_tvar = Tvar::new(reason.dupe(), mem_tvar_id as u32);
        let voided_out_collector = TypeCollector::create();
        for v in &voided_t {
            voided_out_collector.add(v.dupe());
        }
        let opt_use = get_opt_use(cx, &subexpression_types, reason.dupe());
        let chain_voided_out_collector = if assertion {
            None
        } else {
            Some(voided_out_collector.dupe())
        };
        let upper = apply_opt_use(opt_use, mem_tvar.dupe());
        optional_chain::run(
            cx,
            &chain_t,
            &chain_reason,
            &lhs_reason,
            &upper,
            &chain_voided_out_collector,
        )
        .expect("should not fail outside speculation");
        let mem_t = Type::new(TypeInner::OpenT(mem_tvar));
        let voided_out_ts: Vec<Type> = voided_out_collector.collect_to_vec();
        let voided_out = {
            let t = union_of_ts(reason.dupe(), voided_out_ts, None);
            let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason_of_t(&t), &t)
                .expect("should not fail outside speculation");
            for t in &ts {
                tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, t);
            }
            ts
        };
        let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_where(cx, reason.dupe(), |cx, t| {
            flow_js::flow_t_non_speculating(cx, (&mem_t, t));
            for out in &voided_out {
                flow_js::flow_t_non_speculating(cx, (out, t));
            }
        });
        tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &mem_t);
        Ok((
            mem_t,
            voided_out,
            lhs_t,
            chain_t,
            object_ast,
            subexpression_asts,
        ))
    }
    fn handle_continue_chain<'a, A, B>(
        cx: &Context<'a>,
        conf: ChainingConf<'a, '_, A, B>,
        chain_t: Type,
        voided_t: Vec<Type>,
        object_ast: expression::Expression<ALoc, (ALoc, Type)>,
    ) -> Result<
        (
            Type,
            Vec<Type>,
            Type,
            Type,
            expression::Expression<ALoc, (ALoc, Type)>,
            B,
        ),
        AbnormalControlFlow,
    > {
        let ChainingConf {
            refine,
            refinement_action,
            subexpressions,
            get_result,
            get_reason,
            ..
        } = conf;
        // We're looking at a non-optional call or member access, but one where
        // deeper in the chain there was an optional chaining operator. We don't
        // need to do anything special locally, but we do need to remember that
        // we might have short-circuited before getting here--that's the
        // voided_t parameter. We'll flow that type into the type of the overall
        // expression to account for that possibility.
        let (subexpression_types, subexpression_asts) = subexpressions(cx)?;
        let reason = get_reason(&chain_t);
        let res_t = match refine(cx) {
            Some(refi) => match &refinement_action {
                Some(ra) => ra(cx, &subexpression_types, &chain_t, refi),
                None => refi,
            },
            None => get_result(cx, &subexpression_types, reason, &chain_t),
        };
        let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_where(
            cx,
            reason_of_t(&res_t).dupe(),
            |cx, t| {
                flow_js::flow_t_non_speculating(cx, (&res_t, t));
                for void_t in &voided_t {
                    flow_js::flow_t_non_speculating(cx, (void_t, t));
                }
            },
        );
        Ok((
            res_t,
            voided_t,
            lhs_t,
            chain_t,
            object_ast,
            subexpression_asts,
        ))
    }

    fn handle_chaining<'a, A, B>(
        cx: &Context<'a>,
        conf: ChainingConf<'a, '_, A, B>,
        opt: OptState,
        obj_: &expression::Expression<ALoc, ALoc>,
        loc: ALoc,
    ) -> Result<
        (
            Type,
            Vec<Type>,
            Type,
            Type,
            expression::Expression<ALoc, (ALoc, Type)>,
            B,
        ),
        AbnormalControlFlow,
    > {
        match opt {
            //   | NonOptional ->
            OptState::NonOptional => {
                let ChainingConf {
                    refinement_action,
                    refine,
                    subexpressions,
                    get_result,
                    get_reason,
                    ..
                } = conf;
                // Proceeding as normal: no need to worry about optionality, so T2 from
                // above is None. We don't need to consider optional short-circuiting, so
                // we can call expression_ rather than optional_chain. *)
                let object_ast =
                    expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, obj_)?;
                let obj_t = object_ast.loc().1.dupe();
                let (subexpression_types, subexpression_asts) = subexpressions(cx)?;
                let reason = get_reason(&obj_t);
                let lhs_t = match refine(cx) {
                    Some(refi) => match &refinement_action {
                        Some(ra) => ra(cx, &subexpression_types, &obj_t, refi),
                        None => refi,
                    },
                    None => get_result(cx, &subexpression_types, reason, &obj_t),
                };
                Ok((
                    lhs_t.dupe(),
                    vec![],
                    lhs_t,
                    obj_t,
                    object_ast,
                    subexpression_asts,
                ))
            }
            OptState::AssertChain | OptState::NewChain => {
                let lhs_reason = mk_expression_reason(obj_);
                let (filtered_t, voided_t, object_ast) =
                    optional_chain(EnclosingContext::NoContext, cx, obj_)?;
                match (conf.refine)(cx) {
                    Some(t) => {
                        let ChainingConf {
                            refinement_action,
                            subexpressions,
                            ..
                        } = conf;
                        cx.mark_optional_chain(loc.dupe(), lhs_reason, false);
                        let (subexpression_types, subexpression_asts) = subexpressions(cx)?;
                        let tout = match &refinement_action {
                            Some(ra) => ra(cx, &subexpression_types, &filtered_t, t),
                            None => t,
                        };
                        let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_where(
                            cx,
                            reason_of_t(&tout).dupe(),
                            |cx, t| {
                                flow_js::flow_t_non_speculating(cx, (&tout, t));
                                for void_t in &voided_t {
                                    flow_js::flow_t_non_speculating(cx, (void_t, t));
                                }
                            },
                        );
                        Ok((
                            tout,
                            voided_t,
                            lhs_t,
                            filtered_t,
                            object_ast,
                            subexpression_asts,
                        ))
                    }
                    None => handle_new_chain(
                        cx,
                        opt == OptState::AssertChain,
                        conf,
                        lhs_reason,
                        loc,
                        filtered_t,
                        voided_t,
                        object_ast,
                    ),
                }
            }
            OptState::ContinueChain => {
                let (chain_t, voided_t, object_ast) =
                    optional_chain(EnclosingContext::NoContext, cx, obj_)?;
                handle_continue_chain(cx, conf, chain_t, voided_t, object_ast)
            }
        }
    }

    fn specialize_callee(
        callee: expression::Expression<ALoc, (ALoc, Type)>,
        specialized_callee: &type_::SpecializedCallee,
    ) -> expression::Expression<ALoc, (ALoc, Type)> {
        let finalized = specialized_callee.finalized.borrow();
        if finalized.is_empty() {
            callee
        } else {
            // If the type of the callee has been specialized (due to implicit
            // instantiation or overload resolution) then use that type.
            let _callee_aloc = callee.loc().0.dupe();
            let t_init = callee.loc().1.dupe();
            let t = union_of_ts(
                reason_of_t(&t_init).dupe(),
                finalized.iter().cloned().collect(),
                None,
            );
            flow_parser::ast_utils::push_toplevel_type(t, callee)
        }
    }

    if let Some(((res_loc, ref lhs_t), ref res_inner)) =
        try_non_chain(cx, loc.dupe(), &e_prime, &*call_ast, &*member_ast)?
    {
        // Nothing to do with respect to optional chaining, because we're in a
        // case where chaining isn't allowed. *)
        let mut res_inner = res_inner.clone();
        *res_inner.loc_mut() = (res_loc, lhs_t.dupe());
        return Ok((lhs_t.dupe(), vec![], expression::Expression::new(res_inner)));
    }

    type ReceiverAstFn = Box<
        dyn Fn(expression::Member<ALoc, (ALoc, Type)>, Type) -> ExpressionInner<ALoc, (ALoc, Type)>,
    >;
    struct MethodReceiverAndState {
        member_opt: OptState,
        member: expression::Member<ALoc, ALoc>,
        receiver_ast: ReceiverAstFn,
        orig_receiver: expression::Expression<ALoc, ALoc>,
    }

    // If we're looking at a call, look "one level deeper" to see if the
    // next element of the chain is an member access, in which case we're
    // looking at an optional method call and we need to process both
    //  "levels" at once.  Similar to the call to factor_out_optional above,
    // we then factor out the optionality of the member lookup component of
    // the method call. However, we can skip this if the callee is optional
    // and the call is non-optional--this means that the callee is in
    // parentheses, so we can treat it as a regular GetProp followed by a
    // regular Call instead of using the special method call machinery. Such
    // a case would look like this:
    //
    //  callee
    //  vvvvvvvvv
    // (obj?.meth)()
    //  ^^^
    // member._object
    let (e_prime, method_receiver_and_state): (
        ExpressionInner<ALoc, ALoc>,
        Option<MethodReceiverAndState>,
    ) = match (&e_prime, &opt_state) {
        (
            ExpressionInner::Call { inner, .. },
            OptState::NewChain | OptState::ContinueChain | OptState::AssertChain,
        ) if let ExpressionInner::OptionalMember { inner: om, .. } = inner.callee.deref() => {
            let callee_loc = inner.callee.loc().dupe();
            let call = inner.as_ref().clone();
            let orig_receiver = inner.callee.clone();
            let (optional, member, filtered_out) =
                (om.optional, om.member.clone(), om.filtered_out.dupe());
            let receiver_ast: ReceiverAstFn = {
                let optional = optional.clone();
                let filtered_out = filtered_out.dupe();
                Box::new(
                    move |member: expression::Member<ALoc, (ALoc, Type)>, ty: Type| {
                        ExpressionInner::OptionalMember {
                            loc: (filtered_out.dupe(), ty.dupe()),
                            inner: (expression::OptionalMember {
                                member,
                                optional: optional.clone(),
                                filtered_out: (filtered_out.dupe(), ty),
                            })
                            .into(),
                        }
                    },
                )
            };
            let member_opt = match &optional {
                // In this case:
                //
                //    callee
                //   vvvvvvvvv
                //   obj?.meth() (or obj?.meth?.())
                //   ^^^
                //   member._object
                //
                // There may or may not be other links in the chain earlier than obj, and the call
                // to meth() may be optional itself (e.g. obj?.meth?.()) -- this has already been
                // factored out.
                expression::OptionalMemberKind::Optional => OptState::NewChain,
                expression::OptionalMemberKind::AssertNonnull => {
                    if !cx.assert_operator_enabled() {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                callee_loc.dupe(),
                                UnsupportedSyntax::NonnullAssertion,
                            ))),
                        );
                    }
                    OptState::AssertChain
                }
                // In this case:
                //
                //             callee
                //            vvvvvvvv
                // other_obj?.obj.meth() (or other_obj?.obj.meth?.())
                //            ^^^
                //            member._object
                expression::OptionalMemberKind::NonOptional => OptState::ContinueChain,
            };
            let new_callee = expression::Expression::new(ExpressionInner::Member {
                loc: callee_loc,
                inner: member.clone().into(),
            });
            let new_e_prime = ExpressionInner::Call {
                loc: e_prime.loc().dupe(),
                inner: expression::Call {
                    callee: new_callee,
                    targs: call.targs.clone(),
                    arguments: call.arguments.clone(),
                    comments: call.comments.dupe(),
                }
                .into(),
            };
            (
                new_e_prime,
                Some(MethodReceiverAndState {
                    member_opt,
                    member,
                    receiver_ast,
                    orig_receiver,
                }),
            )
        }
        (ExpressionInner::Call { inner, .. }, _)
            if let ExpressionInner::Member { inner: m, .. } = inner.callee.deref() =>
        {
            let orig_receiver = inner.callee.clone();
            let member = m.as_ref().clone();
            let receiver_ast: ReceiverAstFn = Box::new(
                |member: expression::Member<ALoc, (ALoc, Type)>, _ty: Type| {
                    ExpressionInner::Member {
                        loc: member.object.loc().clone(),
                        inner: member.into(),
                    }
                },
            );
            (
                e_prime,
                Some(MethodReceiverAndState {
                    member_opt: OptState::NonOptional,
                    member,
                    receiver_ast,
                    orig_receiver,
                }),
            )
        }
        _ => (e_prime, None),
    };

    Ok(match (&e_prime, &method_receiver_and_state) {
        // e1[e2]
        (ExpressionInner::Member { inner, .. }, _)
            if let expression::member::Property::PropertyExpression(index) = &inner.property =>
        {
            let _object = &inner.object;
            let comments = &inner.comments;
            let reason = mk_reason(RProperty(None), loc.dupe());
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(
                mk_expression_reason(ex),
            )));
            // Only create an id if the property expression is a literal, which are
            // treated like named props.
            let id = match index.deref() {
                ExpressionInner::StringLiteral { .. } | ExpressionInner::NumberLiteral { .. } => {
                    Some(mk_id() as i32)
                }
                _ => None,
            };
            let get_opt_use: Rc<
                dyn Fn(&Context<'a>, &Type, Reason) -> type_::OptUseT<Context<'a>>,
            > = {
                let use_op = use_op.dupe();
                let reason = reason.dupe();
                Rc::new(move |_cx: &Context<'a>, tind: &Type, _: Reason| {
                    type_::OptUseT::OptGetElemT(
                        use_op.dupe(),
                        reason.dupe(),
                        id,
                        false, // annot
                        tind.dupe(),
                    )
                })
            };
            let get_mem_t: Box<dyn Fn(&Context<'a>, &Type, Reason, &Type) -> Type> = {
                let get_opt_use = get_opt_use.dupe();
                Box::new(
                    move |cx: &Context<'a>, tind: &Type, reason: Reason, obj_t: &Type| {
                        let opt_use = get_opt_use(cx, tind, reason.dupe());
                        let obj_t = obj_t.dupe();
                        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                            cx,
                            reason,
                            |cx, t_reason, t_id| {
                                let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                                let use_t = apply_opt_use(opt_use.clone(), tvar);
                                flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                            },
                        )
                    },
                )
            };
            let eval_index: Box<
                dyn FnOnce(
                    &Context<'a>,
                ) -> Result<
                    (Type, expression::Expression<ALoc, (ALoc, Type)>),
                    AbnormalControlFlow,
                >,
            > = {
                Box::new(move |cx: &Context<'a>| {
                    let typed_index = expression_inner(
                        Some(EnclosingContext::IndexContext),
                        None,
                        None,
                        FrozenKind::NotFrozen,
                        None,
                        cx,
                        index,
                    )?;
                    let tind = typed_index.loc().1.dupe();
                    Ok((tind, typed_index))
                })
            };
            let conf = ChainingConf {
                refinement_action: None,
                refine: {
                    let loc = loc.dupe();
                    let ex = ex.dupe();
                    Box::new(move |cx: &Context<'a>| refinement::get(true, cx, &ex, loc.dupe()))
                },
                subexpressions: eval_index,
                get_result: get_mem_t,
                get_opt_use,
                get_reason: {
                    let reason = reason.dupe();
                    Box::new(move |_: &Type| reason.dupe())
                },
            };
            let (filtered_out, voided_out, lhs_t, _, object_ast, typed_index) =
                handle_chaining(cx, conf, opt_state.clone(), _object, loc.dupe())?;
            let property = expression::member::Property::PropertyExpression(typed_index);
            let ast_inner = member_ast(
                expression::Member {
                    object: object_ast,
                    property,
                    comments: comments.clone(),
                },
                filtered_out.dupe(),
            );
            (filtered_out, voided_out, {
                let mut ast_inner = ast_inner;
                *ast_inner.loc_mut() = (loc, lhs_t);
                expression::Expression::new(ast_inner)
            })
        }
        // e.l
        (ExpressionInner::Member { inner, .. }, _)
            if let expression::member::Property::PropertyIdentifier(id) = &inner.property =>
        {
            let _object = &inner.object;
            let comments = &inner.comments;
            let ploc = &id.loc;
            let name = &id.name;
            let expr_reason = mk_expression_reason(ex);
            let prop_name = Name::new(name.dupe());
            let prop_reason = mk_reason(RProperty(Some(prop_name.dupe())), ploc.dupe());
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(expr_reason.dupe())));
            let opt_use = get_prop_opt_use(
                encl_ctx,
                expr_reason.dupe(),
                use_op,
                type_env::get_hint(cx, loc.dupe()),
                prop_reason,
                name,
            );
            let get_mem_t: Box<dyn Fn(&Context<'a>, &(), Reason, &Type) -> Type> = {
                let expr_reason = expr_reason.dupe();
                let opt_use = opt_use.clone();
                Box::new(move |cx: &Context<'a>, _: &(), _: Reason, obj_t: &Type| {
                    let opt_use = opt_use.clone();
                    let obj_t = obj_t.dupe();
                    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                        cx,
                        expr_reason.dupe(),
                        |cx, t_reason, t_id| {
                            let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                            let use_t = apply_opt_use(opt_use.clone(), tvar);
                            flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                        },
                    )
                })
            };
            let conf = ChainingConf {
                refinement_action: None,
                subexpressions: Box::new(|_cx| Ok(((), ()))),
                get_result: get_mem_t,
                refine: {
                    let loc = loc.dupe();
                    let ex = ex.dupe();
                    Box::new(move |cx: &Context<'a>| refinement::get(true, cx, &ex, loc.dupe()))
                },
                get_opt_use: {
                    let opt_use = opt_use.clone();
                    Rc::new(move |_cx: &Context<'a>, _: &(), _: Reason| opt_use.clone())
                },
                get_reason: {
                    let expr_reason = expr_reason.dupe();
                    Box::new(move |_: &Type| expr_reason.dupe())
                },
            };
            let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
                handle_chaining(cx, conf, opt_state.clone(), _object, loc.dupe())?;
            let property = expression::member::Property::PropertyIdentifier(ast::Identifier::new(
                ast::IdentifierInner {
                    loc: (ploc.dupe(), lhs_t.dupe()),
                    name: id.name.dupe(),
                    comments: id.comments.dupe(),
                },
            ));
            let ast_inner = member_ast(
                expression::Member {
                    object: object_ast,
                    property,
                    comments: comments.clone(),
                },
                filtered_out.dupe(),
            );
            (filtered_out, voided_out, {
                let mut ast_inner = ast_inner;
                *ast_inner.loc_mut() = (loc, lhs_t);
                expression::Expression::new(ast_inner)
            })
        }

        // e.#l
        (ExpressionInner::Member { inner, .. }, _)
            if let expression::member::Property::PropertyPrivateName(pn) = &inner.property =>
        {
            let _object = &inner.object;
            let comments = &inner.comments;
            let name = &pn.name;
            let expr_reason = mk_reason(RPrivateProperty(name.dupe()), loc.dupe());
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(
                mk_expression_reason(ex),
            )));
            let opt_use = get_private_field_opt_use(cx, expr_reason.dupe(), use_op, name);
            let get_mem_t: Box<dyn Fn(&Context<'a>, &(), Reason, &Type) -> Type> = {
                let expr_reason = expr_reason.dupe();
                let opt_use = opt_use.clone();
                Box::new(move |cx: &Context<'a>, _: &(), _: Reason, obj_t: &Type| {
                    let opt_use = opt_use.clone();
                    let obj_t = obj_t.dupe();
                    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                        cx,
                        expr_reason.dupe(),
                        |cx, t_reason, t_id| {
                            let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                            let use_t = apply_opt_use(opt_use.clone(), tvar);
                            flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                        },
                    )
                })
            };
            let conf = ChainingConf {
                refinement_action: None,
                subexpressions: Box::new(|_cx| Ok(((), ()))),
                get_result: get_mem_t,
                refine: {
                    let loc = loc.dupe();
                    let ex = ex.dupe();
                    Box::new(move |cx: &Context<'a>| refinement::get(true, cx, &ex, loc.dupe()))
                },
                get_opt_use: {
                    let opt_use = opt_use.clone();
                    Rc::new(move |_cx: &Context<'a>, _: &(), _: Reason| opt_use.clone())
                },
                get_reason: {
                    let expr_reason = expr_reason.dupe();
                    Box::new(move |_: &Type| expr_reason.dupe())
                },
            };
            let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
                handle_chaining(cx, conf, opt_state.clone(), _object, loc.dupe())?;
            let property = expression::member::Property::PropertyPrivateName(ast::PrivateName {
                loc: pn.loc.dupe(),
                name: pn.name.dupe(),
                comments: pn.comments.dupe(),
            });
            let ast_inner = member_ast(
                expression::Member {
                    object: object_ast,
                    property,
                    comments: comments.clone(),
                },
                filtered_out.dupe(),
            );
            (filtered_out, voided_out, {
                let mut ast_inner = ast_inner;
                *ast_inner.loc_mut() = (loc, lhs_t);
                expression::Expression::new(ast_inner)
            })
        }
        // Method calls: e.l(), e.#l(), and e1[e2]()
        (ExpressionInner::Call { inner, .. }, Some(mrs)) => {
            let callee = &inner.callee;
            let targs = &inner.targs;
            let arguments = &inner.arguments;
            let comments = &inner.comments;
            let member_opt = mrs.member_opt.clone();
            let _object = &mrs.member.object;
            let property = &mrs.member.property;
            let member_comments = &mrs.member.comments;
            let receiver_ast = &mrs.receiver_ast;
            let orig_receiver = &mrs.orig_receiver;
            let lookup_loc = callee.loc().dupe();

            let (targts, typed_targs) = convert_call_targs_opt(cx, targs.as_ref());
            let expr_reason = mk_expression_reason(ex);
            let specialized_callee = cx.new_specialized_callee();

            let (
                filtered_out,
                lookup_voided_out,
                call_voided_out_collector,
                member_lhs_t,
                prop_t,
                obj_filtered_out,
                object_ast,
                property_ast,
                argument_asts,
                reason_lookup,
            ) = match property {
                expression::member::Property::PropertyPrivateName(_)
                | expression::member::Property::PropertyIdentifier(_) => {
                    let (prop_loc, name_str, private_) = match property {
                        expression::member::Property::PropertyPrivateName(pn) => {
                            (&pn.loc, pn.name.dupe(), true)
                        }
                        expression::member::Property::PropertyIdentifier(pid) => {
                            (&pid.loc, pid.name.dupe(), false)
                        }
                        expression::member::Property::PropertyExpression(_) => {
                            panic!("unexpected property expression")
                        }
                    };
                    let reason_call = mk_reason(RMethodCall(Some(name_str.dupe())), loc.dupe());
                    let prop_name = Name::new(name_str.dupe());
                    let reason_prop = mk_reason(RProperty(Some(prop_name.dupe())), prop_loc.dupe());
                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCallMethod(Box::new(
                        FunCallMethodData {
                            op: expr_reason.dupe(),
                            fn_: mk_expression_reason(orig_receiver),
                            prop: reason_prop.dupe(),
                            args: mk_initial_arguments_reason(arguments).into(),
                            local: true,
                        },
                    ))));
                    let prop_t = flow_typing_tvar::mk(cx, reason_prop.dupe());
                    let call_voided_out_collector = TypeCollector::create();
                    let get_opt_use: Rc<
                        dyn Fn(&Context<'a>, &Vec<CallArg>, Reason) -> type_::OptUseT<Context<'a>>,
                    > = {
                        let opt_state = opt_state.clone();
                        let call_voided_out_collector = call_voided_out_collector.dupe();
                        let reason_call = reason_call.dupe();
                        let use_op = use_op.dupe();
                        let prop_loc = prop_loc.dupe();
                        let loc = loc.dupe();
                        let targts = targts.clone();
                        let specialized_callee = specialized_callee.clone();
                        let name_str = name_str.dupe();
                        Rc::new(
                            move |cx: &Context<'a>, argts: &Vec<CallArg>, _reason: Reason| {
                                method_call_opt_use(
                                    cx,
                                    opt_state.clone(),
                                    call_voided_out_collector.dupe(),
                                    reason_call.dupe(),
                                    use_op.dupe(),
                                    private_,
                                    true,
                                    prop_loc.dupe(),
                                    callee,
                                    &name_str,
                                    loc.dupe(),
                                    targts.clone(),
                                    argts.clone(),
                                    Some(specialized_callee.clone()),
                                )
                            },
                        )
                    };
                    let handle_refined_callee = {
                        let reason_call = reason_call.dupe();
                        let targts = targts.clone();
                        let prop_t = prop_t.dupe();
                        let use_op = use_op.dupe();
                        let loc = loc.dupe();
                        let opt_state = opt_state.clone();
                        move |cx: &Context<'a>, argts: &Vec<CallArg>, obj_t: &Type, f: Type| {
                            let reason_call = reason_call.dupe();
                            let targts = targts.clone();
                            let prop_t = prop_t.dupe();
                            let use_op = use_op.dupe();
                            let loc = loc.dupe();
                            let opt_state = opt_state.clone();
                            let obj_t = obj_t.dupe();
                            let argts = argts.clone();
                            tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                                cx,
                                reason_call.dupe(),
                                |cx, t_reason, t_id| {
                                    let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                                    let inner_voided_out_collector = TypeCollector::create();
                                    let app = mk_boundfunctioncalltype(
                                        obj_t.dupe(),
                                        targts.clone().map(|v: Vec<type_::Targ>| v.into()),
                                        argts.clone().into(),
                                        true,
                                        tvar.dupe(),
                                    );
                                    flow_js::unify_non_speculating(cx, None, &f, &prop_t);
                                    let call_use =
                                        UseT::new(UseTInner::CallT(Box::new(CallTData {
                                            use_op: use_op.dupe(),
                                            reason: reason_call.dupe(),
                                            call_action: Box::new(CallAction::Funcalltype(
                                                Box::new(app),
                                            )),
                                            return_hint: type_env::get_hint(cx, loc.dupe()),
                                        })));
                                    match opt_state {
                                        OptState::AssertChain | OptState::NewChain => {
                                            let (chain_reason, voided_out_collector_opt) =
                                                if opt_state == OptState::NewChain {
                                                    (
                                                        mk_reason(ROptionalChain, loc.dupe()),
                                                        Some(inner_voided_out_collector.dupe()),
                                                    )
                                                } else {
                                                    (mk_reason(RNonnullAssert, loc.dupe()), None)
                                                };
                                            let lhs_reason = mk_expression_reason(callee);
                                            optional_chain::run(
                                                cx,
                                                &f,
                                                &chain_reason,
                                                &lhs_reason,
                                                &call_use,
                                                &voided_out_collector_opt,
                                            )
                                            .expect("should not fail outside speculation");
                                        }
                                        _ => {
                                            flow_js::flow_non_speculating(cx, (&f, &call_use));
                                        }
                                    }
                                    inner_voided_out_collector.iter(|void_t| {
                                        let open_t = Type::new(TypeInner::OpenT(tvar.dupe()));
                                        flow_js::flow_t_non_speculating(cx, (void_t, &open_t));
                                    });
                                },
                            )
                        }
                    };
                    let refinement_action: Option<
                        Box<dyn Fn(&Context<'a>, &Vec<CallArg>, &Type, Type) -> Type>,
                    > = Some(Box::new(handle_refined_callee));
                    let get_mem_t: Box<dyn Fn(&Context<'a>, &Vec<CallArg>, Reason, &Type) -> Type> = {
                        let reason_call = reason_call.dupe();
                        let get_opt_use = get_opt_use.dupe();
                        Box::new(
                            move |cx: &Context<'a>,
                                  argts: &Vec<CallArg>,
                                  reason: Reason,
                                  obj_t: &Type| {
                                let opt_use = get_opt_use(cx, argts, reason);
                                let obj_t = obj_t.dupe();
                                tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                                    cx,
                                    reason_call.dupe(),
                                    |cx, t_reason, t_id| {
                                        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                                        let use_t = apply_opt_use(opt_use.clone(), tvar);
                                        flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                                    },
                                )
                            },
                        )
                    };
                    let eval_args: Box<
                        dyn FnOnce(
                            &Context<'a>,
                        ) -> Result<
                            (Vec<CallArg>, expression::ArgList<ALoc, (ALoc, Type)>),
                            AbnormalControlFlow,
                        >,
                    > = { Box::new(move |cx: &Context<'a>| arg_list(cx, arguments)) };
                    let conf = ChainingConf {
                        subexpressions: eval_args,
                        get_result: get_mem_t,
                        get_opt_use,
                        refine: {
                            let lookup_loc = lookup_loc.dupe();
                            Box::new(move |cx: &Context<'a>| {
                                refinement::get(true, cx, callee, lookup_loc.dupe())
                            })
                        },
                        refinement_action,
                        get_reason: {
                            let expr_reason = expr_reason.dupe();
                            Box::new(move |_: &Type| expr_reason.dupe())
                        },
                    };
                    let (
                        filtered_out,
                        lookup_voided_out,
                        member_lhs_t,
                        obj_filtered_out,
                        object_ast,
                        argument_asts,
                    ) = handle_chaining(cx, conf, member_opt, _object, lookup_loc.dupe())?;
                    let prop_ast = match property {
                        expression::member::Property::PropertyExpression(_) => {
                            panic!("unexpected property expression")
                        }
                        expression::member::Property::PropertyPrivateName(pn) => {
                            expression::member::Property::PropertyPrivateName(ast::PrivateName {
                                loc: pn.loc.dupe(),
                                name: pn.name.dupe(),
                                comments: pn.comments.dupe(),
                            })
                        }
                        expression::member::Property::PropertyIdentifier(pid) => {
                            expression::member::Property::PropertyIdentifier(ast::Identifier::new(
                                ast::IdentifierInner {
                                    loc: (prop_loc.dupe(), prop_t.dupe()),
                                    name: pid.name.dupe(),
                                    comments: pid.comments.dupe(),
                                },
                            ))
                        }
                    };
                    (
                        filtered_out,
                        lookup_voided_out,
                        call_voided_out_collector,
                        member_lhs_t,
                        prop_t,
                        obj_filtered_out,
                        object_ast,
                        prop_ast,
                        argument_asts,
                        reason_prop,
                    )
                }
                expression::member::Property::PropertyExpression(prop_expr) => {
                    let reason_call = mk_reason(RMethodCall(None), loc.dupe());
                    let reason_lookup = mk_reason(RProperty(None), lookup_loc.dupe());
                    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCallMethod(Box::new(
                        FunCallMethodData {
                            op: expr_reason.dupe(),
                            fn_: mk_expression_reason(orig_receiver),
                            prop: mk_expression_reason(prop_expr),
                            args: mk_initial_arguments_reason(arguments).into(),
                            local: true,
                        },
                    ))));
                    let call_voided_out_collector = TypeCollector::create();
                    let prop_t = flow_typing_tvar::mk(cx, reason_lookup.dupe());
                    let get_opt_use: Rc<
                        dyn Fn(
                            &Context<'a>,
                            &(Vec<CallArg>, Type),
                            Reason,
                        ) -> type_::OptUseT<Context<'a>>,
                    > = {
                        let opt_state = opt_state.clone();
                        let call_voided_out_collector = call_voided_out_collector.dupe();
                        let use_op = use_op.dupe();
                        let reason_call = reason_call.dupe();
                        let reason_lookup = reason_lookup.dupe();
                        let expr_reason = expr_reason.dupe();
                        let loc = loc.dupe();
                        let targts = targts.clone();
                        let specialized_callee = specialized_callee.clone();
                        Rc::new(
                            move |_cx: &Context<'a>,
                                  arg_and_elem: &(Vec<CallArg>, Type),
                                  _reason: Reason| {
                                let (argts, elem_t) = arg_and_elem;
                                elem_call_opt_use(
                                    opt_state.clone(),
                                    call_voided_out_collector.dupe(),
                                    use_op.dupe(),
                                    reason_call.dupe(),
                                    reason_lookup.dupe(),
                                    expr_reason.dupe(),
                                    mk_reason(ROptionalChain, loc.dupe()),
                                    targts.clone(),
                                    argts.clone(),
                                    elem_t.dupe(),
                                    Some(specialized_callee.clone()),
                                )
                            },
                        )
                    };
                    let get_mem_t: Box<
                        dyn Fn(&Context<'a>, &(Vec<CallArg>, Type), Reason, &Type) -> Type,
                    > = {
                        let reason_call = reason_call.dupe();
                        let get_opt_use = get_opt_use.dupe();
                        Box::new(
                            move |cx: &Context<'a>,
                                  arg_and_elem: &(Vec<CallArg>, Type),
                                  reason: Reason,
                                  obj_t: &Type| {
                                let opt_use = get_opt_use(cx, arg_and_elem, reason);
                                let obj_t = obj_t.dupe();
                                tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                                    cx,
                                    reason_call.dupe(),
                                    |cx, t_reason, t_id| {
                                        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                                        let use_t = apply_opt_use(opt_use.clone(), tvar);
                                        flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                                    },
                                )
                            },
                        )
                    };
                    let eval_args_and_expr: Box<
                        dyn FnOnce(
                                &Context<'a>,
                            ) -> Result<
                                (
                                    (Vec<CallArg>, Type),
                                    (
                                        expression::ArgList<ALoc, (ALoc, Type)>,
                                        expression::Expression<ALoc, (ALoc, Type)>,
                                    ),
                                ),
                                AbnormalControlFlow,
                            > + '_,
                    > = {
                        Box::new(move |cx: &Context<'a>| {
                            let typed_prop_expr = expression_inner(
                                Some(EnclosingContext::IndexContext),
                                None,
                                None,
                                FrozenKind::NotFrozen,
                                None,
                                cx,
                                prop_expr,
                            )?;
                            let elem_t = typed_prop_expr.loc().1.dupe();
                            let (argts, arguments_ast) = arg_list(cx, arguments)?;
                            Ok(((argts, elem_t), (arguments_ast, typed_prop_expr)))
                        })
                    };
                    let conf = ChainingConf {
                        refinement_action: None,
                        subexpressions: eval_args_and_expr,
                        get_result: get_mem_t,
                        get_opt_use,
                        refine: Box::new(|_cx: &Context<'a>| noop()),
                        get_reason: {
                            let expr_reason = expr_reason.dupe();
                            Box::new(move |_: &Type| expr_reason.dupe())
                        },
                    };
                    let (
                        filtered_out,
                        lookup_voided_out,
                        member_lhs_t,
                        obj_filtered_out,
                        object_ast,
                        (argument_asts, expr_ast),
                    ) = handle_chaining(cx, conf, member_opt, _object, lookup_loc.dupe())?;
                    (
                        filtered_out,
                        lookup_voided_out,
                        call_voided_out_collector,
                        member_lhs_t,
                        prop_t,
                        obj_filtered_out,
                        object_ast,
                        expression::member::Property::PropertyExpression(expr_ast),
                        argument_asts,
                        reason_lookup,
                    )
                }
            };
            let call_voided_ts: Vec<Type> = call_voided_out_collector.collect_to_vec();
            let call_voided_union = union_of_ts(expr_reason.dupe(), call_voided_ts, None);
            let joined = join_optional_branches(&lookup_voided_out, &call_voided_union);
            let voided_out = normalize_voided_out(joined);
            let lhs_t = tvar_resolver::mk_tvar_and_fully_resolve_where(
                cx,
                reason_of_t(&member_lhs_t).dupe(),
                |cx, t| {
                    flow_js::flow_t_non_speculating(cx, (&member_lhs_t, t));
                    for out in &voided_out {
                        flow_js::flow_t_non_speculating(cx, (out, t));
                    }
                },
            );
            let member_for_callee = expression::Member {
                object: object_ast,
                property: property_ast,
                comments: member_comments.clone(),
            };
            let mut receiver_inner = receiver_ast(member_for_callee, obj_filtered_out.dupe());
            *receiver_inner.loc_mut() = (lookup_loc.dupe(), prop_t.dupe());
            let callee_expr_ast = expression::Expression::new(receiver_inner);
            let callee_expr_ast = specialize_callee(callee_expr_ast, &specialized_callee);
            let sig_help = flow_js_utils::callee_recorder::type_for_sig_help(
                reason_lookup.dupe(),
                &specialized_callee,
            );
            let ast_inner = call_ast(
                filtered_out.dupe(),
                sig_help,
                expression::Call {
                    callee: callee_expr_ast,
                    targs: typed_targs,
                    arguments: argument_asts,
                    comments: comments.clone(),
                },
            );
            (filtered_out, voided_out, {
                let mut ast_inner = ast_inner;
                *ast_inner.loc_mut() = (loc, lhs_t);
                expression::Expression::new(ast_inner)
            })
        }
        // e1(e2...)
        (ExpressionInner::Call { inner, .. }, None) => {
            let callee = &inner.callee;
            let targs = &inner.targs;
            let arguments = &inner.arguments;
            let comments = &inner.comments;
            let (targts, typed_targs) = convert_call_targs_opt(cx, targs.as_ref());
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCall(Box::new(FunCallData {
                op: mk_expression_reason(ex),
                fn_: mk_expression_reason(callee),
                args: mk_initial_arguments_reason(arguments).into(),
                local: true,
            }))));
            let spec_callee = cx.new_specialized_callee();
            let get_opt_use: Rc<
                dyn Fn(&Context<'a>, &Vec<CallArg>, Reason) -> type_::OptUseT<Context<'a>>,
            > = {
                let loc = loc.dupe();
                let use_op = use_op.dupe();
                let targts = targts.clone();
                let spec_callee = spec_callee.clone();
                Rc::new(
                    move |cx: &Context<'a>, argts: &Vec<CallArg>, reason: Reason| {
                        func_call_opt_use(
                            cx,
                            loc.dupe(),
                            reason,
                            use_op.dupe(),
                            true,
                            targts.clone(),
                            argts.clone(),
                            Some(spec_callee.clone()),
                        )
                    },
                )
            };
            let get_reason: Box<dyn Fn(&Type) -> Reason + '_> = {
                let loc = loc.dupe();
                Box::new(move |lhs_t: &Type| {
                    mk_reason(
                        RFunctionCall(Arc::new(desc_of_t(lhs_t).clone())),
                        loc.dupe(),
                    )
                })
            };
            let get_result: Box<dyn Fn(&Context<'a>, &Vec<CallArg>, Reason, &Type) -> Type> = {
                let get_opt_use = get_opt_use.dupe();
                Box::new(
                    move |cx: &Context<'a>, argts: &Vec<CallArg>, reason: Reason, obj_t: &Type| {
                        let opt_use = get_opt_use(cx, argts, reason.dupe());
                        let obj_t = obj_t.dupe();
                        tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                            cx,
                            reason,
                            |cx, t_reason, t_id| {
                                let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
                                let use_t = apply_opt_use(opt_use.clone(), tvar);
                                flow_js::flow_non_speculating(cx, (&obj_t, &use_t));
                            },
                        )
                    },
                )
            };
            let eval_args: Box<
                dyn FnOnce(
                    &Context<'a>,
                ) -> Result<
                    (Vec<CallArg>, expression::ArgList<ALoc, (ALoc, Type)>),
                    AbnormalControlFlow,
                >,
            > = { Box::new(move |cx: &Context<'a>| arg_list(cx, arguments)) };
            let conf = ChainingConf {
                refinement_action: None,
                subexpressions: eval_args,
                refine: Box::new(|_cx: &Context<'a>| noop()),
                get_result,
                get_opt_use,
                get_reason,
            };
            let (filtered_out, voided_out, lhs_t, _, object_ast, argument_asts) =
                handle_chaining(cx, conf, opt_state.clone(), callee, loc.dupe())?;
            let reason_callee = mk_expression_reason(callee);
            let sig_help =
                flow_js_utils::callee_recorder::type_for_sig_help(reason_callee, &spec_callee);
            let exp =
                    |callee_arg: expression::Expression<ALoc, (ALoc, Type)>|
                     -> ExpressionInner<ALoc, (ALoc, Type)> {
                        let callee_arg = specialize_callee(callee_arg, &spec_callee);
                        call_ast(
                            filtered_out.dupe(),
                            sig_help,
                            expression::Call {
                                callee: callee_arg,
                                targs: typed_targs,
                                arguments: argument_asts,
                                comments: comments.clone(),
                            },
                        )
                    };
            let ast_inner = exp(object_ast);
            (filtered_out, voided_out, {
                let mut ast_inner = ast_inner;
                *ast_inner.loc_mut() = (loc, lhs_t);
                expression::Expression::new(ast_inner)
            })
        }
        _ => {
            let res = expression_inner(
                Some(encl_ctx),
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                ex,
            )?;
            let t = res.loc().1.dupe();
            (t, vec![], res)
        }
    })
}

fn arg_list<'a>(
    cx: &Context<'a>,
    args: &expression::ArgList<ALoc, ALoc>,
) -> Result<(Vec<CallArg>, expression::ArgList<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let arg_list = args;
    let (argts, arg_asts): (
        Vec<CallArg>,
        Vec<expression::ExpressionOrSpread<ALoc, (ALoc, Type)>>,
    ) = arg_list
        .arguments
        .iter()
        .map(|arg| expression_or_spread(cx, arg))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .unzip();
    Ok((
        argts,
        expression::ArgList {
            loc: arg_list.loc.dupe(),
            arguments: arg_asts.into(),
            comments: arg_list.comments.dupe(),
        },
    ))
}

fn subscript<'a>(
    encl_ctx: EnclosingContext,
    cx: &Context<'a>,
    ex: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let (_, _, ast) = optional_chain(encl_ctx, cx, ex)?;
    Ok(ast)
}

fn unary<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    expr: &expression::Unary<ALoc, ALoc>,
) -> Result<(Type, expression::Unary<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    use ast::expression::UnaryOperator;

    let SyntacticFlags {
        encl_ctx,
        decl,
        as_const,
        frozen,
        has_hint,
    } = syntactic_flags;
    Ok(match expr.operator {
        UnaryOperator::Not => {
            let argument = expression_inner(
                Some(EnclosingContext::OtherTestContext),
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.argument,
            )?;
            let arg = argument.loc().1.dupe();
            let tout = if encl_ctx.is_conditional_test_context() {
                type_::bool_module_t::at(loc.dupe())
            } else {
                let reason = mk_reason(
                    RUnaryOperator(FlowSmolStr::from("not"), Arc::new(desc_of_t(&arg).clone())),
                    loc.dupe(),
                );
                type_operation_utils::operators::unary_not(cx, &reason, &arg)
            };
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::Not,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Plus => {
            let argument = expression_inner(
                Some(encl_ctx.dupe()),
                *decl,
                Some(*as_const),
                *frozen,
                Some(has_hint.dupe()),
                cx,
                &expr.argument,
            )?;
            let argt = argument.loc().1.dupe();
            let reason = mk_reason(desc_of_t(&argt).clone(), loc.dupe());
            let tout = type_operation_utils::operators::unary_arith(
                cx,
                &reason,
                &type_::UnaryArithKind::Plus,
                &argt,
            );
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::Plus,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Minus => {
            let has_hint: LazyBool<'a> = {
                let old_has_hint = has_hint.dupe();
                let loc = loc.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
                    *old_has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &loc)
                })))
            };
            let argument = expression_inner(
                Some(encl_ctx.dupe()),
                *decl,
                Some(*as_const),
                *frozen,
                Some(has_hint),
                cx,
                &expr.argument,
            )?;
            let argt = argument.loc().1.dupe();
            let reason = mk_reason(desc_of_t(&argt).clone(), loc.dupe());
            let tout = type_operation_utils::operators::unary_arith(
                cx,
                &reason,
                &type_::UnaryArithKind::Minus,
                &argt,
            );
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::Minus,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::BitNot => {
            let argument = expression_inner(
                Some(encl_ctx.dupe()),
                *decl,
                Some(*as_const),
                *frozen,
                Some(has_hint.dupe()),
                cx,
                &expr.argument,
            )?;
            let argt = argument.loc().1.dupe();
            let reason = mk_reason(desc_of_t(&argt).clone(), loc.dupe());
            let tout = type_operation_utils::operators::unary_arith(
                cx,
                &reason,
                &type_::UnaryArithKind::BitNot,
                &argt,
            );
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::BitNot,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Typeof => {
            let argument = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.argument,
            )?;
            (
                type_::str_module_t::at(loc.dupe()),
                expression::Unary {
                    operator: UnaryOperator::Typeof,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Void => {
            let argument = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.argument,
            )?;
            (
                type_::void::at(loc.dupe()),
                expression::Unary {
                    operator: UnaryOperator::Void,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Delete => {
            let argument = delete(cx, loc.dupe(), &expr.argument)?;
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Unary {
                    operator: UnaryOperator::Delete,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Await => {
            let reason = mk_reason(RAwait, loc.dupe());
            let argument_ast = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.argument,
            )?;
            let arg = argument_ast.loc().1.dupe();
            let tout = type_operation_utils::promise::await_(cx, &reason, &arg);
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::Await,
                    argument: argument_ast,
                    comments: expr.comments.dupe(),
                },
            )
        }
        UnaryOperator::Nonnull => {
            if !cx.assert_operator_enabled() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        UnsupportedSyntax::NonnullAssertion,
                    ))),
                );
            }
            let argument = expression_inner(
                Some(encl_ctx.dupe()),
                *decl,
                Some(*as_const),
                *frozen,
                Some(has_hint.dupe()),
                cx,
                &expr.argument,
            )?;
            let argt = argument.loc().1.dupe();
            let reason = mk_reason(RNonnullAssert, loc.dupe());
            let tout = type_operation_utils::operators::non_maybe(cx, &reason, &argt);
            (
                tout,
                expression::Unary {
                    operator: UnaryOperator::Nonnull,
                    argument,
                    comments: expr.comments.dupe(),
                },
            )
        }
    })
}

// numeric pre/post inc/dec
fn update<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    expr: &expression::Update<ALoc, ALoc>,
) -> Result<(Type, expression::Update<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let reason = mk_reason(RUpdate, loc.dupe());
    let arg_ast = expression_inner(
        None,
        None,
        None,
        FrozenKind::NotFrozen,
        None,
        cx,
        &expr.argument,
    )?;
    let arg_t = arg_ast.loc().1.dupe();
    let result_t = type_operation_utils::operators::unary_arith(
        cx,
        &reason,
        &type_::UnaryArithKind::Update,
        &arg_t,
    );
    let arg_ast = match expr.argument.deref() {
        expression::ExpressionInner::Identifier { inner, .. } => {
            let id_loc = inner.loc.dupe();
            let name = &inner.name;
            // enforce state-based guards for binding update, e.g., const
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                var: Some(mk_reason(
                    VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                    id_loc.dupe(),
                )),
                init: reason_of_t(&result_t).dupe(),
            }));
            type_env::set_var(cx, &use_op, name, &result_t, id_loc);
            arg_ast
        }
        // Updating involves both reading and writing. We need to model both of these, and ensuring
        // an arithmetic operand should use the read type, which is affected by refinements.
        expression::ExpressionInner::Member {
            loc: lhs_loc,
            inner,
        } => {
            let make_op = |lhs_r: Reason, prop: Reason| -> UseOp {
                UseOp::Op(Arc::new(type_::RootUseOp::UpdateProperty {
                    lhs: lhs_r,
                    prop,
                }))
            };
            let lhs_prop_reason = flow_common::reason::mk_expression_reason(&expr.argument);
            let lhs_loc_clone = lhs_loc.dupe();
            let reconstruct_ast = |mem: expression::Member<ALoc, (ALoc, Type)>,
                                   t: Type|
             -> expression::ExpressionInner<ALoc, (ALoc, Type)> {
                expression::ExpressionInner::Member {
                    loc: (lhs_loc_clone.dupe(), t),
                    inner: mem.into(),
                }
            };
            let ((_loc_t, _), inner) = assign_member(
                cx,
                OptState::NonOptional,
                &make_op,
                result_t.dupe(),
                lhs_loc.dupe(),
                lhs_prop_reason,
                &reconstruct_ast,
                SetMode::Assign,
                inner,
            )?;
            expression::Expression::new(inner)
        }
        _ => arg_ast,
    };
    Ok((
        result_t,
        expression::Update {
            operator: expr.operator.clone(),
            argument: arg_ast,
            prefix: expr.prefix,
            comments: expr.comments.dupe(),
        },
    ))
}

// Returns a function that type check LHS or RHS of eq_test under correct conditional context.
fn visit_eq_test<'a>(
    encl_ctx: EnclosingContext,
    loc: ALoc,
    left: &expression::Expression<ALoc, ALoc>,
    right: &expression::Expression<ALoc, ALoc>,
) -> Box<
    dyn Fn(
        &Context<'a>,
        &expression::Expression<ALoc, ALoc>,
    ) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow>,
> {
    if encl_ctx.is_conditional_test_context() {
        let mk = move |encl: EnclosingContext| -> Box<
            dyn Fn(
                &Context<'a>,
                &expression::Expression<ALoc, ALoc>,
            )
                -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow>,
        > {
            Box::new(move |cx, e| {
                expression_inner(
                    Some(encl.dupe()),
                    None,
                    None,
                    FrozenKind::NotFrozen,
                    None,
                    cx,
                    e,
                )
            })
        };
        let is_switch = matches!(encl_ctx, EnclosingContext::SwitchTestContext { .. });
        flow_env_builder::eq_test::visit_eq_test(
            |_loc, _expr, _value, _str, _b| mk(EnclosingContext::OtherTestContext),
            |_strict, _sense, _loc, _expr, _kind, _value| mk(EnclosingContext::OtherTestContext),
            |_sense, _strict, _loc, _expr, _value| mk(EnclosingContext::OtherTestContext),
            |_sense, _strict, _check_bound, _loc, _expr, _value| {
                mk(EnclosingContext::OtherTestContext)
            },
            |_expr, _value| mk(EnclosingContext::OtherTestContext),
            |_value, _expr| mk(EnclosingContext::OtherTestContext),
            is_switch,
            |_, _| mk(EnclosingContext::NoContext),
            false,
            false,
            loc.dupe(),
            left,
            right,
        )
    } else {
        let encl_ctx_clone = encl_ctx.dupe();
        Box::new(move |cx, e: &expression::Expression<ALoc, ALoc>| {
            expression_inner(
                Some(encl_ctx_clone.dupe()),
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                e,
            )
        })
    }
}

// traverse a binary expression, return result type
fn binary<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    encl_ctx: EnclosingContext,
    expr: &expression::Binary<ALoc, ALoc>,
) -> Result<(Type, expression::Binary<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    use ast::expression::BinaryOperator;
    Ok(match expr.operator {
        BinaryOperator::Equal | BinaryOperator::NotEqual => {
            let reconstruct_ast = visit_eq_test(encl_ctx, loc.dupe(), &expr.left, &expr.right);
            let left = reconstruct_ast(cx, &expr.left)?;
            let t1 = left.loc().1.dupe();
            let right = reconstruct_ast(cx, &expr.right)?;
            let t2 = right.loc().1.dupe();
            type_operation_utils::operators::check_eq(cx, (&t1, &t2));
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Binary {
                    operator: expr.operator.clone(),
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        BinaryOperator::In => {
            let left = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.left,
            )?;
            let t1 = left.loc().1.dupe();
            let right = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.right,
            )?;
            let t2 = right.loc().1.dupe();
            type_operation_utils::type_assertions::assert_binary_in_lhs(cx, &t1);
            type_operation_utils::type_assertions::assert_binary_in_rhs(cx, &t2);
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Binary {
                    operator: expr.operator.clone(),
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        BinaryOperator::StrictEqual | BinaryOperator::StrictNotEqual => {
            let eq_encl_ctx = match encl_ctx.dupe() {
                EnclosingContext::NoContext => EnclosingContext::StrictComparison,
                other => other,
            };
            let reconstruct_ast = visit_eq_test(eq_encl_ctx, loc.dupe(), &expr.left, &expr.right);
            let left = reconstruct_ast(cx, &expr.left)?;
            let t1 = left.loc().1.dupe();
            let right = reconstruct_ast(cx, &expr.right)?;
            let t2 = right.loc().1.dupe();
            type_operation_utils::operators::check_strict_eq(&encl_ctx, cx, (&t1, &t2));
            cx.add_strict_comparison((loc.dupe(), (left.clone(), right.clone())));
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Binary {
                    operator: expr.operator.clone(),
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        BinaryOperator::Instanceof => {
            let left = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.left,
            )?;
            let right = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.right,
            )?;
            let right_t = right.loc().1.dupe();
            type_operation_utils::type_assertions::assert_instanceof_rhs(cx, &right_t);
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Binary {
                    operator: expr.operator.clone(),
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        BinaryOperator::LessThan
        | BinaryOperator::LessThanEqual
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanEqual => {
            let left = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.left,
            )?;
            let t1 = left.loc().1.dupe();
            let right = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.right,
            )?;
            let t2 = right.loc().1.dupe();
            type_operation_utils::operators::check_comparator(cx, &t1, &t2);
            (
                type_::bool_module_t::at(loc.dupe()),
                expression::Binary {
                    operator: expr.operator.clone(),
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        BinaryOperator::Plus
        | BinaryOperator::LShift
        | BinaryOperator::RShift
        | BinaryOperator::RShift3
        | BinaryOperator::Minus
        | BinaryOperator::Mult
        | BinaryOperator::Exp
        | BinaryOperator::Div
        | BinaryOperator::Mod
        | BinaryOperator::BitOr
        | BinaryOperator::Xor
        | BinaryOperator::BitAnd => {
            let left_ast = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.left,
            )?;
            let t1 = left_ast.loc().1.dupe();
            let right_ast = expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                &expr.right,
            )?;
            let t2 = right_ast.loc().1.dupe();
            let desc = RBinaryOperator(Box::new((
                FlowSmolStr::from("arithmetic operation"),
                Arc::new(reason_of_t(&t1).desc(true).clone()),
                Arc::new(reason_of_t(&t2).desc(true).clone()),
            )));
            let reason = mk_reason(desc, loc.dupe());
            let arith_kind = type_::arith_kind::ArithKind::of_binary_operator(expr.operator);
            let tout = type_operation_utils::operators::arith(cx, &reason, &arith_kind, &t1, &t2);
            (
                tout,
                expression::Binary {
                    operator: expr.operator.clone(),
                    left: left_ast,
                    right: right_ast,
                    comments: expr.comments.dupe(),
                },
            )
        }
    })
}

fn logical<'a>(
    cx: &Context<'a>,
    syntactic_flags: &SyntacticFlags<'a>,
    loc: ALoc,
    expr: &expression::Logical<ALoc, ALoc>,
) -> Result<(Type, expression::Logical<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    use ast::expression::LogicalOperator;

    let SyntacticFlags {
        encl_ctx,
        has_hint,
        decl,
        ..
    } = syntactic_flags;
    let has_hint: LazyBool<'a> = {
        let old_has_hint = has_hint.dupe();
        let loc = loc.dupe();
        Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
            *old_has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &loc)
        })))
    };
    // With logical operators the LHS is always evaluated. So if the LHS throws, the whole
    // expression throws. To model this we do not catch abnormal exceptions on the LHS.
    // As such, we only analyze the RHS expression if the LHS does not throw.
    // If the LHS does not throw, and the RHS does throw, then we cannot say that the
    // entire expression throws, because we only evaluate the RHS depending on the value of the LHS.
    // Thus, we catch abnormal control flow exceptions on the RHS and do not rethrow them.
    //   let (t, op) = match operator with
    let (t, op) = match expr.operator {
        // | Or ->
        LogicalOperator::Or => {
            check_default_pattern(cx, expr.left.loc().dupe(), &expr.right);
            let left = condition(
                cx,
                EnclosingContext::OtherTestContext,
                *decl,
                Some(has_hint.dupe()),
                &expr.left,
            )?;
            let t1 = left.loc().1.dupe();
            let (right, right_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    expression_inner(
                        Some(encl_ctx.dupe()),
                        *decl,
                        None,
                        FrozenKind::NotFrozen,
                        Some(has_hint.dupe()),
                        cx,
                        &expr.right,
                    )
                });
            let t2_raw = right.loc().1.dupe();
            let t2 = if right_throws {
                empty_t::at(loc.dupe())
            } else {
                t2_raw
            };
            let reason = mk_reason(
                RLogical(Box::new((
                    FlowSmolStr::from("||"),
                    Arc::new(desc_of_t(&t1).clone()),
                    Arc::new(desc_of_t(&t2).clone()),
                ))),
                loc.dupe(),
            );
            let tout = type_operation_utils::operators::logical_or(cx, &reason, &t1, &t2);
            (
                tout,
                expression::Logical {
                    operator: LogicalOperator::Or,
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        LogicalOperator::And => {
            let left = condition(
                cx,
                EnclosingContext::OtherTestContext,
                *decl,
                Some(has_hint.dupe()),
                &expr.left,
            )?;
            let t1 = left.loc().1.dupe();
            let (right, right_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    expression_inner(
                        Some(encl_ctx.dupe()),
                        *decl,
                        None,
                        FrozenKind::NotFrozen,
                        Some(has_hint.dupe()),
                        cx,
                        &expr.right,
                    )
                });
            let t2_raw = right.loc().1.dupe();
            let t2 = if right_throws {
                empty_t::at(loc.dupe())
            } else {
                t2_raw
            };
            let reason = mk_reason(
                RLogical(Box::new((
                    FlowSmolStr::from("&&"),
                    Arc::new(desc_of_t(&t1).clone()),
                    Arc::new(desc_of_t(&t2).clone()),
                ))),
                loc.dupe(),
            );
            let tout = type_operation_utils::operators::logical_and(cx, &reason, &t1, &t2);
            (
                tout,
                expression::Logical {
                    operator: LogicalOperator::And,
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
        LogicalOperator::NullishCoalesce => {
            let left = condition(
                cx,
                encl_ctx.dupe(),
                *decl,
                Some(has_hint.dupe()),
                &expr.left,
            )?;
            let t1 = left.loc().1.dupe();
            let (right, right_throws) =
                flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                    expression_inner(
                        Some(encl_ctx.dupe()),
                        *decl,
                        None,
                        FrozenKind::NotFrozen,
                        Some(has_hint.dupe()),
                        cx,
                        &expr.right,
                    )
                });
            let t2_raw = right.loc().1.dupe();
            let t2 = if right_throws {
                empty_t::at(loc.dupe())
            } else {
                t2_raw
            };
            let reason = mk_reason(
                RLogical(Box::new((
                    FlowSmolStr::from("??"),
                    Arc::new(desc_of_t(&t1).clone()),
                    Arc::new(desc_of_t(&t2).clone()),
                ))),
                loc.dupe(),
            );
            let tout =
                type_operation_utils::operators::logical_nullish_coalesce(cx, &reason, &t1, &t2);
            (
                tout,
                expression::Logical {
                    operator: LogicalOperator::NullishCoalesce,
                    left,
                    right,
                    comments: expr.comments.dupe(),
                },
            )
        }
    };
    let t = natural_inference::try_generalize(cx, syntactic_flags, &loc, t);
    Ok((t, op))
}

pub fn assignment_lhs<'a>(
    cx: &Context<'a>,
    patt: &ast::pattern::Pattern<ALoc, ALoc>,
) -> Result<ast::pattern::Pattern<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    match patt {
        ast::pattern::Pattern::Identifier {
            loc: pat_loc,
            inner,
        } => {
            let ast::pattern::Identifier {
                name,
                annot,
                optional,
            } = &**inner;
            let id_loc = name.loc.dupe();
            let t = identifier_inner(
                cx,
                &natural_inference::empty_syntactic_flags(),
                name,
                id_loc.dupe(),
            );
            let typed_annot = match annot {
                ast::types::AnnotationOrHint::Available(annot) => {
                    let Ok(v) = polymorphic_ast_mapper::type_annotation(
                        &mut typed_ast_utils::ErrorMapper,
                        annot,
                    );
                    ast::types::AnnotationOrHint::Available(v)
                }
                ast::types::AnnotationOrHint::Missing(hint) => {
                    ast::types::AnnotationOrHint::Missing((
                        hint.dupe(),
                        any_t::locationless(type_::AnySource::Untyped),
                    ))
                }
            };
            Ok(ast::pattern::Pattern::Identifier {
                loc: (pat_loc.dupe(), t.dupe()),
                inner: (ast::pattern::Identifier {
                    name: ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_loc.dupe(), t.dupe()),
                        name: name.name.dupe(),
                        comments: name.comments.dupe(),
                    }),
                    annot: typed_annot,
                    optional: *optional,
                })
                .into(),
            })
        }
        ast::pattern::Pattern::Expression {
            loc: pat_loc,
            inner,
        } => {
            let (expr, assertion, reconstruct_ast) =
                flow_parser::ast_utils::unwrap_nonnull_lhs_expr(inner);
            if assertion && !cx.assert_operator_enabled() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        pat_loc.dupe(),
                        UnsupportedSyntax::NonnullAssertion,
                    ))),
                );
            }
            match expr.deref() {
                expression::ExpressionInner::Identifier {
                    loc: id_exp_loc,
                    inner: name,
                } => {
                    let id_loc = name.loc.dupe();
                    let t = identifier_inner(
                        cx,
                        &natural_inference::empty_syntactic_flags(),
                        name,
                        id_loc.dupe(),
                    );
                    let typed_expr = reconstruct_ast.call(
                        expression::Expression::new(expression::ExpressionInner::Identifier {
                            loc: (id_exp_loc.dupe(), t.dupe()),
                            inner: ast::Identifier::new(ast::IdentifierInner {
                                loc: (id_loc.dupe(), t.dupe()),
                                name: name.name.dupe(),
                                comments: name.comments.dupe(),
                            }),
                        }),
                        &|loc: ALoc| -> (ALoc, Type) {
                            let reason = mk_reason(RNonnullAssert, loc.dupe());
                            let t = type_operation_utils::operators::non_maybe(cx, &reason, &t);
                            (loc, t)
                        },
                    );
                    let t = typed_expr.loc().1.dupe();
                    Ok(ast::pattern::Pattern::Expression {
                        loc: (pat_loc.dupe(), t),
                        inner: typed_expr.into(),
                    })
                }
                expression::ExpressionInner::Member { .. } => {
                    let typed_expr =
                        expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, expr)?;
                    let t = typed_expr.loc().1.dupe();
                    let typed_expr =
                        reconstruct_ast.call(typed_expr, &|loc: ALoc| -> (ALoc, Type) {
                            let reason = mk_reason(RNonnullAssert, loc.dupe());
                            let t = type_operation_utils::operators::non_maybe(cx, &reason, &t);
                            (loc, t)
                        });
                    let t = typed_expr.loc().1.dupe();
                    Ok(ast::pattern::Pattern::Expression {
                        loc: (pat_loc.dupe(), t),
                        inner: typed_expr.into(),
                    })
                }
                _ => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInvalidLHSInAssignment(pat_loc.dupe()),
                    );
                    let mut mapper = typed_ast_utils::ErrorMapper;
                    Ok({
                        let Ok(v) = polymorphic_ast_mapper::pattern(&mut mapper, None, patt);
                        v
                    })
                }
            }
        }
        // TODO: object, array and non-member expression patterns are invalid
        // (should be a parse error but isn't yet)
        ast::pattern::Pattern::Object { .. } | ast::pattern::Pattern::Array { .. } => {
            let lhs_loc = patt.loc().dupe();
            flow_js::add_output_non_speculating(cx, ErrorMessage::EInvalidLHSInAssignment(lhs_loc));
            let mut mapper = typed_ast_utils::ErrorMapper;
            Ok({
                let Ok(v) = polymorphic_ast_mapper::pattern(&mut mapper, None, patt);
                v
            })
        }
    }
}

/// write a type t into a member.
// - the `optional` parameter should be set to NewChain when the member access
//   is optional (a?.b) and should be ContinueChain when it is not itself
//   optional but is part of an optional chain (a?.b.c).
fn assign_member<'a>(
    cx: &Context<'a>,
    optional: OptState,
    make_op: &dyn Fn(/* lhs */ Reason, /* prop */ Reason) -> UseOp,
    t: Type,
    lhs_loc: ALoc,
    lhs_prop_reason: Reason,
    reconstruct_ast: &dyn Fn(
        expression::Member<ALoc, (ALoc, Type)>,
        Type,
    ) -> expression::ExpressionInner<ALoc, (ALoc, Type)>,
    mode: SetMode,
    lhs: &expression::Member<ALoc, ALoc>,
) -> Result<
    (
        (ALoc, Type),
        expression::ExpressionInner<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    let run_maybe_optional_chain =
        |lhs_type: &Type, lhs_reason: &Reason, use_t: UseT<Context<'a>>| {
            match (&optional, &mode) {
                (OptState::NewChain | OptState::AssertChain, SetMode::Delete) => {
                    let reason = if optional == OptState::NewChain {
                        mk_reason(ROptionalChain, lhs_loc.dupe())
                    } else {
                        mk_reason(RNonnullAssert, lhs_loc.dupe())
                    };

                    // When deleting an optional chain, we only really care about the case
                    // where the object type is non-nullable. The specification is:
                    //
                    //   delete a?.b
                    //   is equivalent to
                    //   a == null ? true : delete a.b
                    //
                    // So if a is null, no work has to be done. Hence, we don't collect
                    // the nullable output for the optional chain.
                    optional_chain::run(cx, lhs_type, &reason, lhs_reason, &use_t, &None).unwrap();
                }
                //     | _ -> Flow.flow cx (lhs, use_t)
                _ => {
                    flow_js::flow_non_speculating(cx, (lhs_type, &use_t));
                }
            }
        };
    let typecheck_object = |obj: &expression::Expression<ALoc, ALoc>| -> Result<
        (Type, expression::Expression<ALoc, (ALoc, Type)>),
        AbnormalControlFlow,
    > {
        // If we're deleting a member expression, it's allowed to be an optional chain, and we
        // need to respect short-circuiting, which means the type that's flowed into the
        // SetPropT (or similar) upper bound must be the "filtered," non-nullish type.
        // However, syntactically `a?.x = e` is banned, so if this is an assignment expression,
        // we should just use `expression` to evaluate the object. It still might contain
        // an optional chain, but if so the chain is in parentheses (like `(a?.b).x = e`),
        // which means that the type that flows into SetPropT should include the nullish case.
        Ok(match (&optional, &mode) {
            (
                OptState::NewChain | OptState::ContinueChain | OptState::AssertChain,
                SetMode::Delete,
            ) => {
                let (o, _, _object) = optional_chain(EnclosingContext::NoContext, cx, obj)?;
                (o, _object)
            }
            _ => {
                let _object =
                    expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, obj)?;
                let o = _object.loc().1.dupe();
                (o, _object)
            }
        })
    };
    Ok(match &lhs.property {
        // super.name = e
        expression::member::Property::PropertyIdentifier(id)
            if let expression::ExpressionInner::Super {
                loc: super_loc,
                inner: super_inner,
            } = lhs.object.deref() =>
        {
            let prop_loc: &ALoc = &id.loc;
            let name: &FlowSmolStr = &id.name;
            let comments = &lhs.comments;
            let reason = mk_reason(RPropertyAssignment(Some(name.dupe())), lhs_loc.dupe());
            let prop_name = Name::new(name.dupe());
            let prop_reason = mk_reason(RProperty(Some(prop_name.dupe())), prop_loc.dupe());
            let super_t = super_(cx, super_loc.dupe());
            let prop_t = tvar_resolver::mk_tvar_and_fully_resolve_where(
                cx,
                prop_reason.dupe(),
                |cx, prop_t| {
                    let use_op = make_op(
                        reason.dupe(),
                        mk_reason(lhs_prop_reason.desc(true).clone(), prop_loc.dupe()),
                    );
                    flow_js::flow_non_speculating(
                        cx,
                        (
                            &super_t,
                            &UseT::new(UseTInner::SetPropT(
                                use_op,
                                reason.dupe(),
                                Box::new(mk_named_prop(
                                    prop_reason.dupe(),
                                    false,
                                    prop_name.dupe(),
                                )),
                                mode,
                                WriteCtx::Normal,
                                t.dupe(),
                                Some(prop_t.dupe()),
                            )),
                        ),
                    );
                },
            );
            let property = expression::member::Property::PropertyIdentifier(ast::Identifier::new(
                ast::IdentifierInner {
                    loc: (prop_loc.dupe(), prop_t.dupe()),
                    name: id.name.dupe(),
                    comments: id.comments.dupe(),
                },
            ));
            (
                (lhs_loc, prop_t.dupe()),
                reconstruct_ast(
                    expression::Member {
                        object: expression::Expression::new(expression::ExpressionInner::Super {
                            loc: (super_loc.dupe(), super_t),
                            inner: super_inner.clone(),
                        }),
                        property,
                        comments: comments.clone(),
                    },
                    prop_t,
                ),
            )
        }
        // _object.#name = e
        expression::member::Property::PropertyPrivateName(pn) => {
            let prop_loc: &ALoc = &pn.loc;
            let name: &FlowSmolStr = &pn.name;
            let comments = &lhs.comments;
            let lhs_reason = mk_expression_reason(&lhs.object);
            let (o, _object) = typecheck_object(&lhs.object)?;
            let wr_ctx = match (_object.deref(), type_env::var_scope_kind(cx)) {
                (
                    expression::ExpressionInner::This { .. },
                    flow_env_builder::name_def_types::ScopeKind::Ctor,
                ) => WriteCtx::ThisInCtor,
                _ => WriteCtx::Normal,
            };
            let prop_t = {
                let reason = mk_reason(RPropertyAssignment(Some(name.dupe())), lhs_loc.dupe());
                // flow type to object property itself
                let class_entries = type_env::get_class_entries(cx);
                let prop_reason = mk_reason(RPrivateProperty(name.dupe()), prop_loc.dupe());
                tvar_resolver::mk_tvar_and_fully_resolve_where(cx, prop_reason, |_cx, prop_t| {
                    let use_op = make_op(
                        reason.dupe(),
                        mk_reason(lhs_prop_reason.desc(true).clone(), prop_loc.dupe()),
                    );
                    run_maybe_optional_chain(
                        &o,
                        &lhs_reason,
                        UseT::new(UseTInner::SetPrivatePropT(Box::new(SetPrivatePropTData {
                            use_op,
                            reason: reason.dupe(),
                            name: name.dupe(),
                            set_mode: mode,
                            class_bindings: class_entries.clone().into(),
                            static_: false,
                            write_ctx: wr_ctx,
                            tin: t.dupe(),
                            tout: Some(prop_t.dupe()),
                        }))),
                    );
                })
            };
            (
                (lhs_loc, prop_t.dupe()),
                reconstruct_ast(
                    expression::Member {
                        object: _object,
                        property: expression::member::Property::PropertyPrivateName(
                            ast::PrivateName {
                                loc: pn.loc.dupe(),
                                name: pn.name.dupe(),
                                comments: pn.comments.dupe(),
                            },
                        ),
                        comments: comments.clone(),
                    },
                    prop_t,
                ),
            )
        }
        // _object.name = e
        expression::member::Property::PropertyIdentifier(id) => {
            let prop_loc: &ALoc = &id.loc;
            let name: &FlowSmolStr = &id.name;
            let comments = &lhs.comments;
            let wr_ctx = match (lhs.object.deref(), type_env::var_scope_kind(cx)) {
                (
                    expression::ExpressionInner::This { .. },
                    flow_env_builder::name_def_types::ScopeKind::Ctor,
                ) => WriteCtx::ThisInCtor,
                _ => WriteCtx::Normal,
            };
            let lhs_reason = mk_expression_reason(&lhs.object);
            let (o, _object) = typecheck_object(&lhs.object)?;
            let prop_t = {
                let reason = mk_reason(RPropertyAssignment(Some(name.dupe())), lhs_loc.dupe());
                let prop_name = Name::new(name.dupe());
                let prop_reason = mk_reason(RProperty(Some(prop_name.dupe())), prop_loc.dupe());
                // flow type to object property itself
                tvar_resolver::mk_tvar_and_fully_resolve_where(
                    cx,
                    prop_reason.dupe(),
                    |_cx, prop_t| {
                        let use_op = make_op(
                            reason.dupe(),
                            mk_reason(lhs_prop_reason.desc(true).clone(), prop_loc.dupe()),
                        );
                        run_maybe_optional_chain(
                            &o,
                            &lhs_reason,
                            UseT::new(UseTInner::SetPropT(
                                use_op,
                                reason.dupe(),
                                Box::new(mk_named_prop(
                                    prop_reason.dupe(),
                                    false,
                                    prop_name.dupe(),
                                )),
                                mode,
                                wr_ctx,
                                t.dupe(),
                                Some(prop_t.dupe()),
                            )),
                        );
                    },
                )
            };
            let lhs_t = match (_object.deref(), name.as_str()) {
                (
                    expression::ExpressionInner::Identifier {
                        loc: _,
                        inner: id_inner,
                    },
                    "exports",
                ) if id_inner.name.as_str() == "module"
                    && !type_env::local_scope_entry_exists(cx, id_inner.loc.0.dupe()) =>
                {
                    // module.exports has type `any` in theory, but shouldnt be treated as uncovered
                    t.dupe()
                }
                _ => prop_t.dupe(),
            };
            let member_t = match (_object.deref(), name.as_str()) {
                (
                    expression::ExpressionInner::Identifier {
                        loc: _,
                        inner: id_inner,
                    },
                    "exports",
                ) if id_inner.name.as_str() == "module"
                    && !type_env::local_scope_entry_exists(cx, id_inner.loc.0.dupe()) =>
                {
                    t.dupe()
                }
                _ => prop_t.dupe(),
            };
            let property = expression::member::Property::PropertyIdentifier(ast::Identifier::new(
                ast::IdentifierInner {
                    loc: (prop_loc.dupe(), lhs_t.dupe()),
                    name: id.name.dupe(),
                    comments: id.comments.dupe(),
                },
            ));
            (
                (lhs_loc, lhs_t),
                reconstruct_ast(
                    expression::Member {
                        object: _object,
                        property,
                        comments: comments.clone(),
                    },
                    member_t,
                ),
            )
        }
        // _object[index] = e
        expression::member::Property::PropertyExpression(index) => {
            let iloc: &ALoc = index.loc();
            let comments = &lhs.comments;
            let reason = mk_reason(RPropertyAssignment(None), lhs_loc.dupe());
            let lhs_reason = mk_expression_reason(&lhs.object);
            let (o, _object) = typecheck_object(&lhs.object)?;
            let typed_index = expression_inner(
                Some(EnclosingContext::IndexContext),
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                index,
            )?;
            let i = typed_index.loc().1.dupe();
            let use_op = make_op(
                reason.dupe(),
                mk_reason(lhs_prop_reason.desc(true).clone(), iloc.dupe()),
            );
            run_maybe_optional_chain(
                &o,
                &lhs_reason,
                UseT::new(UseTInner::SetElemT(Box::new(SetElemTData {
                    use_op,
                    reason,
                    key_t: i,
                    set_mode: mode,
                    tin: t.dupe(),
                    tout: None,
                }))),
            );
            // types involved in the assignment itself are computed
            // in pre-havoc environment. it's the assignment itself
            // which clears refis
            (
                (lhs_loc, t.dupe()),
                reconstruct_ast(
                    expression::Member {
                        object: _object,
                        property: expression::member::Property::PropertyExpression(typed_index),
                        comments: comments.clone(),
                    },
                    t,
                ),
            )
        }
    })
}

// traverse simple assignment expressions (`lhs = rhs`)
fn simple_assignment<'a>(
    cx: &Context<'a>,
    _loc: ALoc,
    lhs: &ast::pattern::Pattern<ALoc, ALoc>,
    rhs: &expression::Expression<ALoc, ALoc>,
) -> Result<
    (
        Type,
        ast::pattern::Pattern<ALoc, (ALoc, Type)>,
        expression::Expression<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    let typed_rhs = expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, rhs)?;
    let t = typed_rhs.loc().1.dupe();
    let typed_lhs = match lhs {
        ast::pattern::Pattern::Expression {
            loc: lhs_loc,
            inner,
        } => {
            let expr_loc = inner.loc().dupe();
            let (unwrapped_expr, assertion, reconstruct_ast) =
                flow_parser::ast_utils::unwrap_nonnull_lhs_expr(inner);
            let pat_loc = unwrapped_expr.loc().dupe();
            if assertion {
                if !cx.assert_operator_enabled() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            expr_loc.dupe(),
                            UnsupportedSyntax::NonnullAssertion,
                        ))),
                    );
                }
                if cx.assert_operator_specialized() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EIllegalAssertOperator(Box::new(
                            EIllegalAssertOperatorData {
                                op: mk_reason(RNonnullAssert, lhs_loc.dupe()),
                                obj: mk_expression_reason(unwrapped_expr),
                                specialized: true,
                            },
                        )),
                    );
                }
            }
            match unwrapped_expr.deref() {
                expression::ExpressionInner::Member { inner: mem, .. } => {
                    let lhs_prop_reason = mk_pattern_reason(lhs);
                    let make_op = |lhs_r: Reason, prop: Reason| -> UseOp {
                        UseOp::Op(Arc::new(type_::RootUseOp::SetProperty(Box::new(
                            type_::SetPropertyData {
                                lhs: lhs_r,
                                prop,
                                value: mk_expression_reason(rhs),
                            },
                        ))))
                    };
                    let pat_loc_c = pat_loc.dupe();
                    let reconstruct_ast_internal = |mem: expression::Member<ALoc, (ALoc, Type)>,
                                                    _t: Type|
                     -> expression::ExpressionInner<
                        ALoc,
                        (ALoc, Type),
                    > {
                        expression::ExpressionInner::Member {
                            loc: (pat_loc_c.dupe(), _t),
                            inner: mem.into(),
                        }
                    };
                    let ((assign_loc, assign_t), lhs_inner) = assign_member(
                        cx,
                        OptState::NonOptional,
                        &make_op,
                        t.dupe(),
                        lhs_loc.dupe(),
                        lhs_prop_reason,
                        &reconstruct_ast_internal,
                        SetMode::Assign,
                        mem,
                    )?;
                    let t = assign_t.dupe();
                    let typed_expr = reconstruct_ast.call(
                        expression::Expression::new(lhs_inner),
                        &|loc: ALoc| -> (ALoc, Type) { (loc, t.dupe()) },
                    );
                    let t = assign_t;
                    ast::pattern::Pattern::Expression {
                        loc: (assign_loc.dupe(), t),
                        inner: typed_expr.into(),
                    }
                }
                expression::ExpressionInner::Identifier { inner: name, .. } => {
                    let pat = ast::pattern::Pattern::Identifier {
                        loc: lhs_loc.dupe(),
                        inner: (ast::pattern::Identifier {
                            name: name.dupe(),
                            optional: false,
                            annot: ast::types::AnnotationOrHint::Missing(name.loc.dupe()),
                        })
                        .into(),
                    };
                    let typed_pat = crate::destructuring::assignment(cx, rhs.clone(), &pat)?;
                    let t = typed_pat.loc().1.dupe();
                    let ast::pattern::Pattern::Identifier {
                        inner: typed_id, ..
                    } = &typed_pat
                    else {
                        panic!("Result of destructuring an identifier should be an identifier")
                    };
                    let typed_name = typed_id.name.dupe();
                    let typed_expr = reconstruct_ast.call(
                        expression::Expression::new(expression::ExpressionInner::Identifier {
                            loc: (pat_loc, t.dupe()),
                            inner: typed_name,
                        }),
                        &|loc: ALoc| -> (ALoc, Type) { (loc, t.dupe()) },
                    );
                    let t = typed_expr.loc().1.dupe();
                    ast::pattern::Pattern::Expression {
                        loc: (lhs_loc.dupe(), t),
                        inner: typed_expr.into(),
                    }
                }
                _ => crate::destructuring::assignment(cx, rhs.dupe(), lhs)?,
            }
        }
        _ => crate::destructuring::assignment(cx, rhs.dupe(), lhs)?,
    };
    Ok((t, typed_lhs, typed_rhs))
}

// traverse assignment expressions with operators (`lhs += rhs`, `lhs *= rhs`, etc)
fn op_assignment<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    lhs: &ast::pattern::Pattern<ALoc, ALoc>,
    op: expression::AssignmentOperator,
    rhs: &expression::Expression<ALoc, ALoc>,
) -> Result<
    (
        Type,
        ast::pattern::Pattern<ALoc, (ALoc, Type)>,
        expression::Expression<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    let reason = mk_reason(
        VirtualReasonDesc::RCustom(FlowSmolStr::from(op.as_str())),
        loc.dupe(),
    );
    let rhs_reason = mk_expression_reason(rhs);
    let update_env = |result_t: &Type| -> Result<(), AbnormalControlFlow> {
        let (lhs, _) = flow_parser::ast_utils::unwrap_nonnull_lhs(lhs);
        match lhs.as_ref() {
            ast::pattern::Pattern::Identifier { inner, .. } => {
                let id_loc = inner.name.loc.dupe();
                let name = &inner.name;
                let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
                    var: Some(mk_reason(
                        VirtualReasonDesc::RIdentifier(Name::new(name.name.dupe())),
                        id_loc.dupe(),
                    )),
                    init: rhs_reason.dupe(),
                }));
                type_env::set_var(cx, &use_op, &name.name, result_t, id_loc);
            }
            ast::pattern::Pattern::Expression {
                loc: lhs_loc,
                inner: pat_expr,
            } => match pat_expr.deref().deref() {
                expression::ExpressionInner::Member { inner: mem, .. } => {
                    let lhs_prop_reason = mk_pattern_reason(lhs.as_ref());
                    let make_op = |lhs_r: Reason, prop: Reason| -> UseOp {
                        UseOp::Op(Arc::new(type_::RootUseOp::UpdateProperty {
                            lhs: lhs_r,
                            prop,
                        }))
                    };
                    let reconstruct_ast = |mem: expression::Member<ALoc, (ALoc, Type)>,
                                           t: Type|
                     -> expression::ExpressionInner<
                        ALoc,
                        (ALoc, Type),
                    > {
                        expression::ExpressionInner::Member {
                            loc: (lhs_loc.dupe(), t),
                            inner: mem.into(),
                        }
                    };
                    assign_member(
                        cx,
                        OptState::NonOptional,
                        &make_op,
                        result_t.dupe(),
                        lhs_loc.dupe(),
                        lhs_prop_reason,
                        &reconstruct_ast,
                        SetMode::Assign,
                        mem,
                    )?;
                }
                _ => {}
            },
            _ => {}
        }
        Ok(())
    };
    Ok(match op {
        expression::AssignmentOperator::PlusAssign
        | expression::AssignmentOperator::MinusAssign
        | expression::AssignmentOperator::MultAssign
        | expression::AssignmentOperator::ExpAssign
        | expression::AssignmentOperator::DivAssign
        | expression::AssignmentOperator::ModAssign
        | expression::AssignmentOperator::LShiftAssign
        | expression::AssignmentOperator::RShiftAssign
        | expression::AssignmentOperator::RShift3Assign
        | expression::AssignmentOperator::BitOrAssign
        | expression::AssignmentOperator::BitXorAssign
        | expression::AssignmentOperator::BitAndAssign => {
            // lhs (op)= rhs
            let lhs_ast = assignment_lhs(cx, lhs)?;
            let lhs_t = lhs_ast.loc().1.dupe();
            let rhs_ast = expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, rhs)?;
            let rhs_t = rhs_ast.loc().1.dupe();
            let arith_kind = type_::arith_kind::ArithKind::of_assignment_operator(op);
            let result_t =
                type_operation_utils::operators::arith(cx, &reason, &arith_kind, &lhs_t, &rhs_t);
            // enforce state-based guards for binding update, e.g., const
            update_env(&result_t)?;
            (lhs_t, lhs_ast, rhs_ast)
        }
        expression::AssignmentOperator::NullishAssign
        | expression::AssignmentOperator::AndAssign
        | expression::AssignmentOperator::OrAssign => {
            let lhs_pattern_ast = assignment_lhs(cx, lhs)?;
            let lhs_t = lhs_pattern_ast.loc().1.dupe();
            let left_expr: Option<expression::Expression<ALoc, ALoc>> = match lhs {
                ast::pattern::Pattern::Identifier {
                    loc: lhs_loc,
                    inner,
                } => Some(expression::Expression::new(
                    expression::ExpressionInner::Identifier {
                        loc: lhs_loc.dupe(),
                        inner: inner.name.dupe(),
                    },
                )),
                ast::pattern::Pattern::Expression {
                    loc: lhs_loc,
                    inner: pat_expr,
                } => match pat_expr.deref().deref() {
                    expression::ExpressionInner::Member { inner: mem, .. } => Some(
                        expression::Expression::new(expression::ExpressionInner::Member {
                            loc: lhs_loc.dupe(),
                            inner: mem.clone(),
                        }),
                    ),
                    expression::ExpressionInner::Unary {
                        inner: unary_op, ..
                    } if matches!(unary_op.operator, ast::expression::UnaryOperator::Nonnull) => {
                        Some(expression::Expression::new(
                            expression::ExpressionInner::Unary {
                                loc: lhs_loc.dupe(),
                                inner: unary_op.clone(),
                            },
                        ))
                    }
                    _ => None,
                },
                _ => None,
            };
            match left_expr {
                None => {
                    let (rhs_ast, _right_throws) =
                        flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                            expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, rhs)
                        });
                    (any_t::error(reason.dupe()), lhs_pattern_ast, rhs_ast)
                }
                Some(left_expr) => match op {
                    expression::AssignmentOperator::NullishAssign => {
                        let (rhs_ast, right_throws) =
                            flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                                expression_inner(
                                    None,
                                    None,
                                    None,
                                    FrozenKind::NotFrozen,
                                    None,
                                    cx,
                                    rhs,
                                )
                            });
                        let rhs_t = if right_throws {
                            empty_t::at(loc.dupe())
                        } else {
                            rhs_ast.loc().1.dupe()
                        };
                        let result_t = type_operation_utils::operators::logical_nullish_coalesce(
                            cx, &reason, &lhs_t, &rhs_t,
                        );
                        update_env(&result_t)?;
                        (lhs_t, lhs_pattern_ast, rhs_ast)
                    }
                    expression::AssignmentOperator::AndAssign => {
                        let cond_result = condition(
                            cx,
                            EnclosingContext::OtherTestContext,
                            None,
                            None,
                            &left_expr,
                        )?;
                        let lhs_t = cond_result.loc().1.dupe();
                        let (rhs_ast, right_throws) =
                            flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                                expression_inner(
                                    None,
                                    None,
                                    None,
                                    FrozenKind::NotFrozen,
                                    None,
                                    cx,
                                    rhs,
                                )
                            });
                        let rhs_t = if right_throws {
                            empty_t::at(loc.dupe())
                        } else {
                            rhs_ast.loc().1.dupe()
                        };
                        let result_t = type_operation_utils::operators::logical_and(
                            cx, &reason, &lhs_t, &rhs_t,
                        );
                        update_env(&result_t)?;
                        (lhs_t, lhs_pattern_ast, rhs_ast)
                    }
                    expression::AssignmentOperator::OrAssign => {
                        check_default_pattern(cx, left_expr.loc().dupe(), rhs);
                        let cond_result = condition(
                            cx,
                            EnclosingContext::OtherTestContext,
                            None,
                            None,
                            &left_expr,
                        )?;
                        let lhs_t = cond_result.loc().1.dupe();
                        let (rhs_ast, right_throws) =
                            flow_typing_utils::abnormal::catch_expr_control_flow_exception(|| {
                                expression_inner(
                                    None,
                                    None,
                                    None,
                                    FrozenKind::NotFrozen,
                                    None,
                                    cx,
                                    rhs,
                                )
                            });
                        let rhs_t = if right_throws {
                            empty_t::at(loc.dupe())
                        } else {
                            rhs_ast.loc().1.dupe()
                        };
                        let result_t = type_operation_utils::operators::logical_or(
                            cx, &reason, &lhs_t, &rhs_t,
                        );
                        update_env(&result_t)?;
                        (lhs_t, lhs_pattern_ast, rhs_ast)
                    }
                    //   | _ -> assert_false "Unexpected operator"
                    _ => panic!("Unexpected operator"),
                },
            }
        }
    })
}

// traverse assignment expressions
fn assignment<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    lhs: &ast::pattern::Pattern<ALoc, ALoc>,
    op: Option<expression::AssignmentOperator>,
    rhs: &expression::Expression<ALoc, ALoc>,
) -> Result<
    (
        Type,
        ast::pattern::Pattern<ALoc, (ALoc, Type)>,
        expression::Expression<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    match op {
        None => simple_assignment(cx, loc, lhs, rhs),
        Some(op) => op_assignment(cx, loc, lhs, op, rhs),
    }
}

// delete variables and properties
fn delete<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    target: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let void = type_::void::at(loc.dupe());
    match target.deref() {
        expression::ExpressionInner::Member {
            loc: lhs_loc,
            inner: mem,
        } => {
            let lhs_prop_reason = mk_expression_reason(target);
            let make_op = |lhs: Reason, prop: Reason| -> UseOp {
                UseOp::Op(Arc::new(type_::RootUseOp::DeleteProperty { lhs, prop }))
            };
            let lhs_loc_clone = lhs_loc.dupe();
            let reconstruct_ast =
                move |mem: expression::Member<ALoc, (ALoc, Type)>,
                      ty: Type|
                      -> expression::ExpressionInner<ALoc, (ALoc, Type)> {
                    expression::ExpressionInner::Member {
                        loc: (lhs_loc_clone.dupe(), ty),
                        inner: mem.into(),
                    }
                };
            let ((_loc_t, _), inner) = assign_member(
                cx,
                OptState::NonOptional,
                &make_op,
                void,
                lhs_loc.dupe(),
                lhs_prop_reason,
                &reconstruct_ast,
                SetMode::Delete,
                mem,
            )?;
            Ok(expression::Expression::new(inner))
        }
        expression::ExpressionInner::OptionalMember {
            loc: lhs_loc,
            inner,
        } => {
            let expression::OptionalMember {
                member: mem,
                optional,
                filtered_out,
            } = &**inner;
            let lhs_prop_reason = mk_expression_reason(target);
            let make_op = |lhs: Reason, prop: Reason| -> UseOp {
                UseOp::Op(Arc::new(type_::RootUseOp::DeleteProperty { lhs, prop }))
            };
            let optional_clone = optional.clone();
            let filtered_out_loc = filtered_out.dupe();
            let lhs_loc_clone = lhs_loc.dupe();
            let reconstruct_ast = move |mem: expression::Member<ALoc, (ALoc, Type)>, ty: Type| {
                expression::ExpressionInner::OptionalMember {
                    loc: (lhs_loc_clone.dupe(), ty.dupe()),
                    inner: (expression::OptionalMember {
                        member: mem,
                        optional: optional_clone,
                        filtered_out: (filtered_out_loc.dupe(), ty),
                    })
                    .into(),
                }
            };
            let opt_state = match optional {
                expression::OptionalMemberKind::Optional => OptState::NewChain,
                expression::OptionalMemberKind::AssertNonnull => {
                    if !cx.assert_operator_enabled() {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                lhs_loc.dupe(),
                                UnsupportedSyntax::NonnullAssertion,
                            ))),
                        );
                    }
                    OptState::AssertChain
                }
                expression::OptionalMemberKind::NonOptional => OptState::ContinueChain,
            };
            let ((_loc_t, _), inner) = assign_member(
                cx,
                opt_state,
                &make_op,
                void,
                lhs_loc.dupe(),
                lhs_prop_reason,
                &reconstruct_ast,
                SetMode::Delete,
                mem,
            )?;
            Ok(expression::Expression::new(inner))
        }
        expression::ExpressionInner::Identifier { loc: id_loc, inner } => {
            let name = &inner.name;
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::DeleteVar {
                var: mk_expression_reason(target),
            }));
            type_env::set_var(cx, &use_op, name, &void, id_loc.dupe());
            Ok(expression_inner(
                None,
                None,
                None,
                FrozenKind::NotFrozen,
                None,
                cx,
                target,
            )?)
        }
        _ => {
            let target_ast =
                expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, target)?;
            let t = target_ast.loc().1.dupe();
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ECannotDelete(Box::new((loc.dupe(), reason_of_t(&t).dupe()))),
            );
            Ok(target_ast)
        }
    }
}

pub fn collapse_children<'a>(
    cx: &Context<'a>,
    children: &(ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
) -> Result<
    (
        Vec<type_::UnresolvedParam>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    ),
    AbnormalControlFlow,
> {
    let (children_loc, children_list) = children;
    let cache = cx.node_cache();
    if let Some(result) = cache.get_jsx_children(children_loc) {
        Ok(result)
    } else {
        let mut unres_params: Vec<type_::UnresolvedParam> = Vec::new();
        let mut children_acc: Vec<ast::jsx::Child<ALoc, (ALoc, Type)>> = Vec::new();
        for child in children_list {
            let (unres_param_opt, typed_child) = jsx_body(cx, child)?;
            if let Some(x) = unres_param_opt {
                unres_params.push(x);
            }
            children_acc.push(typed_child);
        }
        Ok((unres_params, (children_loc.dupe(), children_acc)))
    }
}

fn should_generalize_jsx<'a>(
    cx: &Context<'a>,
    has_hint: &LazyBool<'a>,
    as_const: bool,
    jsx_loc: ALoc,
) -> bool {
    if as_const {
        false
    } else {
        !(*has_hint.get_forced(cx) || natural_inference::loc_has_hint(cx, &jsx_loc))
    }
}

fn jsx<'a>(
    cx: &Context<'a>,
    should_generalize: LazyBool<'a>,
    expr_loc: ALoc,
    e: &ast::jsx::Element<ALoc, ALoc>,
) -> Result<(Type, ast::jsx::Element<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let children = &e.children;
    let closing_element = e.closing_element.as_ref();
    let comments = e.comments.dupe();
    let children_loc = children.0.dupe();
    let open_ = e.opening_element.loc.dupe();
    let locs = match closing_element {
        Some(_) => (expr_loc, open_.dupe(), children_loc),
        _ => (open_.dupe(), open_.dupe(), open_.dupe()),
    };
    let (t, typed_opening, typed_children, typed_closing) = jsx_title(
        cx,
        should_generalize,
        &e.opening_element,
        children,
        closing_element,
        locs,
    )?;
    tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &t);
    Ok((
        t,
        ast::jsx::Element {
            opening_element: typed_opening,
            children: typed_children,
            closing_element: typed_closing,
            comments,
        },
    ))
}

fn jsx_fragment<'a>(
    cx: &Context<'a>,
    should_generalize: LazyBool<'a>,
    expr_loc: ALoc,
    fragment: &ast::jsx::Fragment<ALoc, ALoc>,
) -> Result<(Type, ast::jsx::Fragment<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let frag_opening_element = fragment.frag_opening_element.dupe();
    let frag_closing_element = fragment.frag_closing_element.dupe();
    let frag_comments = fragment.frag_comments.clone();
    let loc_children = fragment.frag_children.0.dupe();
    // TODO: we could make it configurable like the jsx pragma, with the @jsxFrag directive.
    // See https://babeljs.io/docs/babel-plugin-transform-react-jsx#fragments
    let fragment_t = match cx.react_runtime() {
        flow_common::options::ReactRuntime::Automatic => {
            flow_js_utils::import_export_utils::get_implicitly_imported_react_type(
                cx,
                expr_loc.dupe(),
                &|cx, reason, t| {
                    FlowJs::singleton_concretize_type_for_imports_exports(cx, &reason, &t)
                        .map_err(Into::into)
                },
                intermediate_error_types::ExpectedModulePurpose::ReactModuleForJSXFragment,
            )
            .expect("Should not be under speculation")
        }
        flow_common::options::ReactRuntime::Classic => {
            let reason = mk_reason(
                VirtualReasonDesc::RIdentifier(Name::new(FlowSmolStr::from("React.Fragment"))),
                expr_loc.dupe(),
            );
            let react = type_env::var_ref(
                Some(type_env::LookupMode::ForValue),
                cx,
                None,
                Name::new(FlowSmolStr::from("React")),
                expr_loc.dupe(),
            );
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(reason.dupe())));
            get_prop(
                EnclosingContext::NoContext,
                cx,
                reason.dupe(),
                use_op,
                type_::hint_unavailable(),
                react,
                reason,
                &FlowSmolStr::new("Fragment"),
            )
        }
    };
    let (unresolved_params, typed_frag_children) = collapse_children(cx, &fragment.frag_children)?;
    let props = if cx.react_custom_jsx_typing() {
        type_::null::at(expr_loc.dupe())
    } else {
        match react_jsx_normalize_children_prop(cx, loc_children.dupe(), unresolved_params.clone())
        {
            None => type_::null::at(expr_loc.dupe()),
            Some(fragment_children_prop) => {
                let reason_props = mk_reason(VirtualReasonDesc::RReactProps, loc_children.dupe());
                let props_map = properties::PropertiesMap::from_btree_map(BTreeMap::from([(
                    Name::new(FlowSmolStr::from("children")),
                    type_::Property::new(type_::PropertyInner::Field(Box::new(FieldData {
                        preferred_def_locs: None,
                        key_loc: None,
                        type_: fragment_children_prop,
                        polarity: Polarity::Neutral,
                    }))),
                )]));
                let proto = type_::obj_proto::make(reason_props.dupe());
                obj_type::mk_with_proto(
                    cx,
                    reason_props,
                    type_::ObjKind::Exact,
                    None,
                    None,
                    Some(props_map),
                    None,
                    proto,
                )
            }
        }
    };
    let (t, _) = react_jsx_desugar(
        cx,
        FlowSmolStr::new("React.Fragment"),
        should_generalize,
        expr_loc.dupe(),
        loc_children.dupe(),
        fragment_t,
        None,
        props,
        unresolved_params,
    );
    tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &t);
    Ok((
        t,
        ast::jsx::Fragment {
            frag_opening_element,
            frag_children: typed_frag_children,
            frag_closing_element,
            frag_comments,
        },
    ))
}

fn jsx_title<'a>(
    cx: &Context<'a>,
    should_generalize: LazyBool<'a>,
    opening_element: &ast::jsx::Opening<ALoc, ALoc>,
    children: &(ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
    closing_element: Option<&ast::jsx::Closing<ALoc, ALoc>>,
    locs: (ALoc, ALoc, ALoc),
) -> Result<
    (
        Type,
        ast::jsx::Opening<ALoc, (ALoc, Type)>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
        Option<ast::jsx::Closing<ALoc, (ALoc, Type)>>,
    ),
    AbnormalControlFlow,
> {
    let (loc_element, loc_opening, loc_children) = locs;
    //   let (loc, { Opening.name; targs; attributes; self_closing }) = opening_element in
    let ast::jsx::Opening {
        loc,
        name,
        targs,
        attributes,
        self_closing,
    } = opening_element;
    let loc = loc.dupe();
    let targs = targs.as_ref();
    let targs_with_tast_opt: Option<(Vec<Targ>, expression::CallTypeArgs<ALoc, (ALoc, Type)>)> =
        targs.map(|targs_ast| {
            let (targs_vec, targs_tast) = convert_call_targs(cx, &FlowOrdMap::new(), targs_ast);
            (targs_vec, targs_tast)
        });
    let targs_opt = targs_with_tast_opt.as_ref().map(|(t, _)| t.clone());
    let targs_tast_opt = targs_with_tast_opt.map(|(_, tast)| tast);
    let facebook_fbs = cx.facebook_fbs();
    let facebook_fbt = cx.facebook_fbt();
    let jsx_mode = cx.jsx();

    let (t, typed_name, typed_attributes, typed_children): (
        Type,
        ast::jsx::Name<ALoc, (ALoc, Type)>,
        Vec<ast::jsx::OpeningAttribute<ALoc, (ALoc, Type)>>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    ) = match (name, &jsx_mode, (&facebook_fbs, &facebook_fbt)) {
        (ast::jsx::Name::Identifier(id), _, _)
            if (id.name == "fbs" && facebook_fbs.is_some())
                || (id.name == "fbt" && facebook_fbt.is_some()) =>
        {
            let loc_id = id.loc.dupe();
            let aname = id.name.dupe();
            let id_inner = id.clone();
            let custom_jsx_type = if aname == "fbs" {
                facebook_fbs.unwrap()
            } else {
                facebook_fbt.unwrap()
            };
            let fbt_reason = mk_reason(VirtualReasonDesc::RFbt, loc_element.dupe());
            let t =
                flow_js::get_builtin_type_non_speculating(cx, &fbt_reason, None, custom_jsx_type);
            // TODO check attribute types against an fbt API
            let (_, typed_attrs, _, typed_children) = jsx_mk_props(
                cx,
                fbt_reason,
                &|has_hint, cx, e| {
                    expression_inner(None, None, None, FrozenKind::NotFrozen, has_hint, cx, e)
                },
                &collapse_children,
                &aname,
                attributes,
                children,
            )?;
            let typed_name = ast::jsx::Name::Identifier(ast::jsx::Identifier {
                loc: (loc_id, t.dupe()),
                name: id_inner.name,
                comments: id_inner.comments,
            });
            (t, typed_name, typed_attrs, typed_children)
        }
        (ast::jsx::Name::Identifier(id), _, _) => {
            let id_loc = id.loc.dupe();
            let id_name = id.name.dupe();
            let id_name_str: &str = &id_name;
            let id_comments = id.comments.dupe();
            if type_inference_hooks_js::dispatch_id_hook(cx, id_name_str, id_loc.dupe()) {
                let t = type_::unsoundness::at(
                    type_::UnsoundnessKind::InferenceHooks,
                    loc_element.dupe(),
                );
                let typed_name = ast::jsx::Name::Identifier(ast::jsx::Identifier {
                    loc: (id_loc, t.dupe()),
                    name: id_name,
                    comments: id_comments,
                });
                let typed_attrs: Vec<ast::jsx::OpeningAttribute<ALoc, (ALoc, Type)>> = attributes
                    .iter()
                    .map(|attr| {
                        let Ok(v) = polymorphic_ast_mapper::jsx_opening_attribute(
                            &mut typed_ast_utils::ErrorMapper,
                            attr,
                        );
                        v
                    })
                    .collect();
                let (_, typed_children) = collapse_children(cx, children)?;
                (t, typed_name, typed_attrs, typed_children)
            } else {
                let reason = match jsx_mode {
                    flow_common::options::JsxMode::JsxReact => mk_reason(
                        VirtualReasonDesc::RReactElement {
                            name_opt: Some(Name::new(id_name.dupe())),
                            from_component_syntax: false,
                        },
                        loc_element.dupe(),
                    ),
                    flow_common::options::JsxMode::JsxPragma(_, _) => mk_reason(
                        VirtualReasonDesc::RJSXElement(Some(id_name.dupe())),
                        loc_element.dupe(),
                    ),
                };
                let c = if id_name_str
                    .chars()
                    .next()
                    .is_some_and(|ch| !ch.is_ascii_lowercase())
                {
                    let syntactic_flags = natural_inference::mk_syntactic_flags(
                        Some(EnclosingContext::JsxTitleNameContext),
                        None,
                        None,
                        None,
                        None,
                    );
                    let ident = mk_ident(id_loc.dupe(), None, id_name.dupe());
                    identifier_inner(cx, &syntactic_flags, &ident, id_loc.dupe())
                } else {
                    if let Some((ref_t, def_loc)) =
                        type_env::intrinsic_ref(cx, None, Name::new(id_name.dupe()), id_loc.dupe())
                    {
                        let ref_reason = mk_reason(
                            VirtualReasonDesc::RIdentifier(Name::new(id_name.dupe())),
                            id_loc.dupe(),
                        );
                        type_operation_utils::type_assertions::assert_non_component_like_base(
                            cx,
                            def_loc,
                            &ref_reason,
                            &ref_t,
                        );
                    }
                    let ident_reason = mk_reason(
                        VirtualReasonDesc::RIdentifier(Name::new(id_name.dupe())),
                        id_loc.dupe(),
                    );
                    Type::new(TypeInner::DefT(
                        ident_reason,
                        DefT::new(DefTInner::SingletonStrT {
                            from_annot: true,
                            value: Name::new(id_name.dupe()),
                        }),
                    ))
                };
                let (o, typed_attrs, unresolved_params, typed_children) = jsx_mk_props(
                    cx,
                    reason.dupe(),
                    &|has_hint, cx, e| {
                        expression_inner(None, None, None, FrozenKind::NotFrozen, has_hint, cx, e)
                    },
                    &collapse_children,
                    id_name_str,
                    attributes,
                    children,
                )?;
                let (t, c_opt) = match cx.jsx() {
                    flow_common::options::JsxMode::JsxReact => react_jsx_desugar(
                        cx,
                        id_name.dupe(),
                        should_generalize,
                        loc_element.dupe(),
                        loc_children.dupe(),
                        c.dupe(),
                        targs_opt,
                        o,
                        unresolved_params,
                    ),
                    flow_common::options::JsxMode::JsxPragma(raw_jsx_expr, jsx_expr) => {
                        non_react_jsx_desugar(
                            cx,
                            raw_jsx_expr,
                            jsx_expr,
                            loc_element.dupe(),
                            loc_opening.dupe(),
                            c.dupe(),
                            targs_opt,
                            o,
                            attributes,
                            unresolved_params,
                        )?
                    }
                };
                let c = c_opt.unwrap_or(c);
                let typed_name = ast::jsx::Name::Identifier(ast::jsx::Identifier {
                    loc: (id_loc, c),
                    name: id_name,
                    comments: id_comments,
                });
                (t, typed_name, typed_attrs, typed_children)
            }
        }
        (ast::jsx::Name::MemberExpression(member), flow_common::options::JsxMode::JsxReact, _) => {
            let el_name = jsx_title_member_to_string(member);
            let reason = mk_reason(
                VirtualReasonDesc::RReactElement {
                    name_opt: Some(Name::new(FlowSmolStr::from(el_name.as_str()))),
                    from_component_syntax: false,
                },
                loc_element.dupe(),
            );
            let m_expr = jsx_title_member_to_expression(member);
            let typed_m_expr =
                expression_inner(None, None, None, FrozenKind::NotFrozen, None, cx, &m_expr)?;
            let (m_loc, m_t) = typed_m_expr.loc();
            let m_loc = m_loc.dupe();
            let m_t = m_t.dupe();
            let c = mod_reason_of_t(
                &|r: Reason| {
                    r.replace_desc(VirtualReasonDesc::RIdentifier(Name::new(
                        FlowSmolStr::from(el_name.as_str()),
                    )))
                },
                &m_t,
            );
            let (o, typed_attrs, unresolved_params, typed_children) = jsx_mk_props(
                cx,
                reason,
                &|has_hint, cx, e| {
                    expression_inner(None, None, None, FrozenKind::NotFrozen, has_hint, cx, e)
                },
                &collapse_children,
                &el_name,
                attributes,
                children,
            )?;
            let (t, _) = react_jsx_desugar(
                cx,
                FlowSmolStr::new(el_name),
                should_generalize,
                loc_element.dupe(),
                loc_children.dupe(),
                c,
                targs_opt,
                o,
                unresolved_params,
            );
            let typed_member = match expression_to_jsx_title_member(m_loc, &typed_m_expr) {
                Some(m) => m,
                None => {
                    let Ok(v) = polymorphic_ast_mapper::jsx_member_expression(
                        &mut typed_ast_utils::ErrorMapper,
                        member,
                    );
                    v
                }
            };
            (
                t,
                ast::jsx::Name::MemberExpression(typed_member),
                typed_attrs,
                typed_children,
            )
        }
        (
            ast::jsx::Name::MemberExpression(member),
            flow_common::options::JsxMode::JsxPragma(_, _),
            _,
        ) => {
            let t =
                type_::unsoundness::at(type_::UnsoundnessKind::InferenceHooks, loc_element.dupe());
            let Ok(typed_name) =
                polymorphic_ast_mapper::jsx_element_name(&mut typed_ast_utils::ErrorMapper, name);
            let el_name = jsx_title_member_to_string(member);
            let reason = mk_reason(
                VirtualReasonDesc::RJSXElement(Some(FlowSmolStr::from(el_name.as_str()))),
                loc_element.dupe(),
            );
            let (_o, typed_attrs, _, typed_children) = jsx_mk_props(
                cx,
                reason,
                &|has_hint, cx, e| {
                    expression_inner(None, None, None, FrozenKind::NotFrozen, has_hint, cx, e)
                },
                &collapse_children,
                &el_name,
                attributes,
                children,
            )?;
            (t, typed_name, typed_attrs, typed_children)
        }
        // TODO? covers namespaced names as element names
        (ast::jsx::Name::NamespacedName(namespace), _, _) => {
            let t =
                type_::unsoundness::at(type_::UnsoundnessKind::InferenceHooks, loc_element.dupe());
            let Ok(typed_name) =
                polymorphic_ast_mapper::jsx_element_name(&mut typed_ast_utils::ErrorMapper, name);
            let el_name = jsx_title_namespaced_name_to_string(namespace);
            let reason = mk_reason(
                VirtualReasonDesc::RJSXElement(Some(FlowSmolStr::from(el_name.as_str()))),
                loc_element.dupe(),
            );
            let (_o, typed_attrs, _, typed_children) = jsx_mk_props(
                cx,
                reason,
                &|has_hint, cx, e| {
                    expression_inner(None, None, None, FrozenKind::NotFrozen, has_hint, cx, e)
                },
                &collapse_children,
                &el_name,
                attributes,
                children,
            )?;
            (t, typed_name, typed_attrs, typed_children)
        }
    };
    let typed_closing = closing_element.map(|closing| ast::jsx::Closing {
        loc: closing.loc.dupe(),
        name: jsx_match_closing_element(&typed_name, &closing.name),
    });
    let typed_opening = ast::jsx::Opening {
        loc: loc.dupe(),
        name: typed_name,
        targs: targs_tast_opt,
        self_closing: *self_closing,
        attributes: typed_attributes.into(),
    };
    Ok((t, typed_opening, typed_children, typed_closing))
}

fn jsx_match_closing_element(
    opening: &ast::jsx::Name<ALoc, (ALoc, Type)>,
    closing: &ast::jsx::Name<ALoc, ALoc>,
) -> ast::jsx::Name<ALoc, (ALoc, Type)> {
    fn match_identifiers(
        o_id: &ast::jsx::Identifier<ALoc, (ALoc, Type)>,
        c_id: &ast::jsx::Identifier<ALoc, ALoc>,
    ) -> ast::jsx::Identifier<ALoc, (ALoc, Type)> {
        let t = o_id.loc.1.dupe();
        ast::jsx::Identifier {
            loc: (c_id.loc.dupe(), t),
            name: c_id.name.dupe(),
            comments: c_id.comments.dupe(),
        }
    }

    fn match_member_expressions(
        o_mexp: &ast::jsx::MemberExpression<ALoc, (ALoc, Type)>,
        c_mexp: &ast::jsx::MemberExpression<ALoc, ALoc>,
    ) -> ast::jsx::MemberExpression<ALoc, (ALoc, Type)> {
        let object = match_objects(&o_mexp.object, &c_mexp.object);
        let property = match_identifiers(&o_mexp.property, &c_mexp.property);
        ast::jsx::MemberExpression {
            loc: c_mexp.loc.dupe(),
            object,
            property,
        }
    }

    fn match_objects(
        o_obj: &ast::jsx::member_expression::Object<ALoc, (ALoc, Type)>,
        c_obj: &ast::jsx::member_expression::Object<ALoc, ALoc>,
    ) -> ast::jsx::member_expression::Object<ALoc, (ALoc, Type)> {
        use ast::jsx::member_expression::Object;
        match (o_obj, c_obj) {
            (Object::Identifier(o_id), Object::Identifier(c_id)) => {
                Object::Identifier(match_identifiers(o_id, c_id))
            }
            (Object::MemberExpression(o_exp), Object::MemberExpression(c_exp)) => {
                Object::MemberExpression(match_member_expressions(o_exp, c_exp).into())
            }
            (_, _) => {
                let Ok(v) = polymorphic_ast_mapper::jsx_member_expression_object(
                    &mut typed_ast_utils::ErrorMapper,
                    c_obj,
                );
                v
            }
        }
    }

    fn match_namespaced_names(
        o_id: &ast::jsx::NamespacedName<ALoc, (ALoc, Type)>,
        c_id: &ast::jsx::NamespacedName<ALoc, ALoc>,
    ) -> ast::jsx::NamespacedName<ALoc, (ALoc, Type)> {
        let namespace = match_identifiers(&o_id.namespace, &c_id.namespace);
        let name = match_identifiers(&o_id.name, &c_id.name);
        ast::jsx::NamespacedName {
            loc: c_id.loc.dupe(),
            namespace,
            name,
        }
    }

    // Transfer open types to close types
    use ast::jsx::Name;
    match (opening, closing) {
        (Name::Identifier(o_id), Name::Identifier(c_id)) => {
            Name::Identifier(match_identifiers(o_id, c_id))
        }
        (Name::NamespacedName(o_nname), Name::NamespacedName(c_nname)) => {
            Name::NamespacedName(match_namespaced_names(o_nname, c_nname))
        }
        (Name::MemberExpression(o_mexp), Name::MemberExpression(c_mexp)) => {
            Name::MemberExpression(match_member_expressions(o_mexp, c_mexp))
        }
        (_, _) => {
            let Ok(v) = polymorphic_ast_mapper::jsx_element_name(
                &mut typed_ast_utils::ErrorMapper,
                closing,
            );
            v
        }
    }
}

pub fn jsx_mk_props<'a>(
    cx: &Context<'a>,
    reason: Reason,
    check_expression: &dyn Fn(
        Option<LazyBool<'a>>,
        &Context<'a>,
        &expression::Expression<ALoc, ALoc>,
    ) -> Result<
        expression::Expression<ALoc, (ALoc, Type)>,
        AbnormalControlFlow,
    >,
    collapse_children: &dyn Fn(
        &Context<'a>,
        &(ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
    ) -> Result<
        (
            Vec<type_::UnresolvedParam>,
            (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
        ),
        AbnormalControlFlow,
    >,
    name: &str,
    attributes: &[ast::jsx::OpeningAttribute<ALoc, ALoc>],
    children: &(ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
) -> Result<
    (
        Type,
        Vec<ast::jsx::OpeningAttribute<ALoc, (ALoc, Type)>>,
        Vec<type_::UnresolvedParam>,
        (ALoc, Vec<ast::jsx::Child<ALoc, (ALoc, Type)>>),
    ),
    AbnormalControlFlow,
> {
    let is_builtin_react = matches!(cx.jsx(), flow_common::options::JsxMode::JsxReact)
        && !cx.react_custom_jsx_typing();
    let reason_props = reason.dupe().replace_desc(if is_builtin_react {
        VirtualReasonDesc::RReactProps
    } else {
        VirtualReasonDesc::RJSXElementProps(FlowSmolStr::from(name))
    });
    // Use the same reason for proto and the ObjT so we can walk the proto chain
    // and use the root proto reason to build an error.
    let proto = type_::obj_proto::make(reason_props.dupe());
    let is_lowercase_element = {
        let mut chars = name.chars();
        match chars.next() {
            Some(c) => c.is_ascii_lowercase(),
            None => false,
        }
    };
    let shorthand_prop_name = if is_lowercase_element {
        cx.stylex_shorthand_prop().map(|s| s.to_string())
    } else {
        None
    };
    // Helper to type an attribute's value expression
    let jsx_attr_value_type = |attr_loc: ALoc,
                               value: &Option<ast::jsx::attribute::Value<ALoc, ALoc>>|
     -> Result<
        (Type, Option<ast::jsx::attribute::Value<ALoc, (ALoc, Type)>>),
        AbnormalControlFlow,
    > {
        match value {
            // <element name="literal" />
            Some(ast::jsx::attribute::Value::StringLiteral((loc, lit))) => {
                let syntactic_flags = natural_inference::mk_syntactic_flags(
                    Some(EnclosingContext::JsxAttrOrChildrenContext),
                    None,
                    None,
                    None,
                    None,
                );
                let t = string_literal_inner(cx, &syntactic_flags, loc.dupe(), lit);
                Ok((
                    t.dupe(),
                    Some(ast::jsx::attribute::Value::StringLiteral((
                        (loc.dupe(), t),
                        lit.clone(),
                    ))),
                ))
            }
            // <element name={expression} />
            Some(ast::jsx::attribute::Value::ExpressionContainer((ec_loc, ec)))
                if let ast::jsx::expression_container::Expression::Expression(expr) =
                    &ec.expression =>
            {
                let ec_comments = ec.comments.dupe();
                let typed_e =
                    check_expression(Some(Rc::new(flow_lazy::Lazy::new_forced(true))), cx, expr)?;
                let (_, t) = typed_e.loc();
                let t = t.dupe();
                Ok((
                    t.dupe(),
                    Some(ast::jsx::attribute::Value::ExpressionContainer((
                        (ec_loc.dupe(), t),
                        ast::jsx::ExpressionContainer {
                            expression: ast::jsx::expression_container::Expression::Expression(
                                typed_e,
                            ),
                            comments: ec_comments,
                        },
                    ))),
                ))
            }
            // <element name={} />
            Some(ec @ ast::jsx::attribute::Value::ExpressionContainer(_)) => {
                let t = empty_t::at(attr_loc.dupe());
                Ok((
                    t,
                    Some({
                        let Ok(v) = polymorphic_ast_mapper::jsx_attribute_value(
                            &mut typed_ast_utils::UncheckedMapper,
                            ec,
                        );
                        v
                    }),
                ))
            }
            // <element name />
            None => {
                let reason = mk_reason(VirtualReasonDesc::RBoolean, attr_loc.dupe());
                Ok((
                    Type::new(TypeInner::DefT(
                        reason,
                        DefT::new(DefTInner::SingletonBoolT {
                            from_annot: false,
                            value: true,
                        }),
                    )),
                    None,
                ))
            }
        }
    };
    // Desugar the configured prop (e.g. sx={...}) into a stylex.props() call.
    // Returns the spread type to add to acc and the typed attribute node.
    // Instead of calling stylex.props(...sx_value) with SpreadArg (which produces
    // confusing errors about "$Iterable" and "spread" when sx is not an array),
    // we first check that the sx value is an array, extracting the element type,
    // then call stylex.props(elem) with a regular Arg.
    let desugar_shorthand_prop = |prop_name: &FlowSmolStr,
                                  attr_loc: ALoc,
                                  id_loc: ALoc,
                                  acomments: Option<ast::Syntax<ALoc, ()>>,
                                  value: &Option<ast::jsx::attribute::Value<ALoc, ALoc>>|
     -> Result<
        (Type, ast::jsx::OpeningAttribute<ALoc, (ALoc, Type)>),
        AbnormalControlFlow,
    > {
        let (atype, value) = jsx_attr_value_type(attr_loc.dupe(), value)?;
        // Look up stylex from the environment
        let stylex_t = type_env::var_ref(
            Some(type_env::LookupMode::ForValue),
            cx,
            None,
            Name::new(FlowSmolStr::new("stylex")),
            id_loc.dupe(),
        );
        // Get stylex.props
        let props_reason = mk_reason(
            VirtualReasonDesc::RProperty(Some(Name::new(FlowSmolStr::new("props")))),
            id_loc.dupe(),
        );
        let stylex_props_t = get_prop(
            EnclosingContext::NoContext,
            cx,
            props_reason.dupe(),
            UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(props_reason.dupe()))),
            type_::hint_unavailable(),
            stylex_t,
            props_reason,
            &FlowSmolStr::new("props"),
        );
        // Check that the prop value is an array and extract element type.
        // This produces errors like "Cannot create `div` element because ..."
        // instead of confusing messages about spread and $Iterable.
        let shorthand_arr_reason = mk_reason(
            VirtualReasonDesc::RProperty(Some(Name::new(prop_name.dupe()))),
            attr_loc.dupe(),
        );
        let component = mk_reason(
            VirtualReasonDesc::RIdentifier(Name::new(FlowSmolStr::new(name))),
            reason.loc().dupe(),
        );
        let use_op = UseOp::Op(Arc::new(type_::RootUseOp::JSXCreateElement {
            op: reason.dupe(),
            component,
        }));
        let elem_reason = mk_reason(VirtualReasonDesc::RArrayElement, attr_loc.dupe());
        let atype_clone = atype.dupe();
        let use_op_clone = use_op.dupe();
        let shorthand_arr_reason_clone = shorthand_arr_reason;
        let elem_t = flow_typing_flow_js::tvar_resolver::mk_tvar_and_fully_resolve_where(
            cx,
            elem_reason,
            |cx, elem_t| {
                let arr_t = Type::new(TypeInner::DefT(
                    shorthand_arr_reason_clone,
                    DefT::new(DefTInner::ArrT(Rc::new(type_::ArrType::ROArrayAT(
                        Box::new((elem_t.dupe(), None)),
                    )))),
                ));
                flow_js::flow_non_speculating(
                    cx,
                    (
                        &atype_clone,
                        &UseT::new(UseTInner::UseT(use_op_clone, arr_t)),
                    ),
                );
            },
        );
        // Call stylex.props(elem) to check element types and get the return type
        let call_reason = mk_reason(
            VirtualReasonDesc::RProperty(Some(Name::new(FlowSmolStr::new("props")))),
            id_loc.dupe(),
        );
        let stylex_props_t_clone = stylex_props_t;
        let use_op_clone2 = use_op;
        let call_reason_clone = call_reason.dupe();
        let elem_t_clone = elem_t;
        let tout = flow_typing_flow_js::tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
            cx,
            call_reason.dupe(),
            |cx, tout_reason, tout_id| {
                let tout_tvar = Tvar::new(tout_reason.dupe(), tout_id as u32);
                let app = type_::mk_functioncalltype(
                    call_reason_clone.dupe(),
                    None,
                    vec![CallArg::arg(elem_t_clone.dupe())].into(),
                    true,
                    tout_tvar,
                );
                let use_t = UseT::new(UseTInner::CallT(Box::new(CallTData {
                    use_op: use_op_clone2.dupe(),
                    reason: call_reason_clone.dupe(),
                    call_action: Box::new(type_::CallAction::Funcalltype(Box::new(app))),
                    return_hint: type_::hint_unavailable(),
                })));
                flow_js::flow_non_speculating(cx, (&stylex_props_t_clone, &use_t));
            },
        );
        let att = ast::jsx::OpeningAttribute::Attribute(ast::jsx::Attribute {
            loc: attr_loc,
            name: ast::jsx::attribute::Name::Identifier(ast::jsx::Identifier {
                loc: (id_loc, atype),
                name: prop_name.dupe(),
                comments: acomments,
            }),
            value,
        });
        Ok((tout, att))
    };
    let (acc, atts) = attributes.iter().try_fold(
        (ObjectExpressionAcc::empty(), vec![]),
        |(acc, mut atts), att| -> Result<_, AbnormalControlFlow> {
            match att {
                // The configured prop (e.g. sx=) on lowercase (intrinsic) JSX elements
                // desugars to {...stylex.props(styles.foo, styles.bar)}
                // Only when the option is set and stylex is imported (phantom read exists).
                ast::jsx::OpeningAttribute::Attribute(attr)
                    if let ast::jsx::attribute::Name::Identifier(id) = &attr.name
                        && shorthand_prop_name.as_deref() == Some(id.name.as_str())
                        && type_env::has_var_read(cx, &id.loc) =>
                {
                    let (id_loc, aname, acomments) =
                        (id.loc.dupe(), id.name.dupe(), id.comments.dupe());
                    let attr_loc = attr.loc.dupe();
                    let value = &attr.value;
                    let (spread_t, att) =
                        desugar_shorthand_prop(&aname, attr_loc, id_loc, acomments, value)?;
                    let acc = acc.add_spread(spread_t);
                    atts.push(att);
                    Ok((acc, atts))
                }
                // All attributes with a non-namespaced name that are not a react ignored attribute.
                ast::jsx::OpeningAttribute::Attribute(attr)
                    if let ast::jsx::attribute::Name::Identifier(id) = &attr.name =>
                {
                    let (id_loc, aname, acomments) =
                        (id.loc.dupe(), id.name.dupe(), id.comments.dupe());
                    let attr_loc = attr.loc.dupe();
                    let value = &attr.value;
                    let (atype, typed_value) = jsx_attr_value_type(attr_loc.dupe(), value)?;
                    let acc =
                        if type_inference_hooks_js::dispatch_jsx_hook(cx, &aname, id_loc.dupe()) {
                            // don't add `aname` to the prop map because it is the autocomplete token
                            acc
                        } else {
                            let atype_clone = atype.dupe();
                            let id_loc_clone = id_loc.dupe();
                            let aname_clone = aname.dupe();
                            acc.add_prop(|pmap| {
                                pmap.insert(
                                    Name::new(aname_clone),
                                    type_::Property::new(type_::PropertyInner::Field(Box::new(
                                        FieldData {
                                            preferred_def_locs: None,
                                            key_loc: Some(id_loc_clone),
                                            type_: atype_clone,
                                            polarity: Polarity::Neutral,
                                        },
                                    ))),
                                );
                            })
                        };
                    let typed_att = ast::jsx::OpeningAttribute::Attribute(ast::jsx::Attribute {
                        loc: attr_loc,
                        name: ast::jsx::attribute::Name::Identifier(ast::jsx::Identifier {
                            loc: (id_loc, atype),
                            name: aname,
                            comments: acomments,
                        }),
                        value: typed_value,
                    });
                    atts.push(typed_att);
                    Ok((acc, atts))
                }
                // Do nothing for namespaced attributes or ignored React attributes.
                ast::jsx::OpeningAttribute::Attribute(_) => {
                    // TODO: attributes with namespaced names
                    Ok((acc, atts))
                }
                // <element {...spread} />
                ast::jsx::OpeningAttribute::SpreadAttribute(spread) => {
                    let spread_loc = spread.loc.dupe();
                    let spread_comments = spread.comments.dupe();
                    let typed_argument = check_expression(
                        Some(Rc::new(flow_lazy::Lazy::new_forced(true))),
                        cx,
                        &spread.argument,
                    )?;
                    let (_, spread_t) = typed_argument.loc();
                    let spread_t = spread_t.dupe();
                    let acc = acc.add_spread(spread_t);
                    let typed_att =
                        ast::jsx::OpeningAttribute::SpreadAttribute(ast::jsx::SpreadAttribute {
                            loc: spread_loc,
                            argument: typed_argument,
                            comments: spread_comments,
                        });
                    atts.push(typed_att);
                    Ok((acc, atts))
                }
            }
        },
    )?;
    let attributes = atts;
    //   let (unresolved_params, ((loc_children, _) as children)) = collapse_children cx children in
    let (unresolved_params, children) = collapse_children(cx, children)?;
    let loc_children = children.0.dupe();
    //   let acc =
    //     match unresolved_params with
    let acc = match unresolved_params.as_slice() {
        [] => acc,
        // We add children to the React.createElement() call for React. Not to the
        // props as other JSX users may support.
        _ if is_builtin_react => {
            match react_jsx_normalize_children_prop(cx, loc_children, unresolved_params.clone()) {
                None => acc,
                Some(children_prop) => acc.add_prop(|pmap| {
                    pmap.insert(
                        Name::new(FlowSmolStr::from("children")),
                        type_::Property::new(type_::PropertyInner::Field(Box::new(FieldData {
                            preferred_def_locs: None,
                            key_loc: None,
                            type_: children_prop,
                            polarity: Polarity::Neutral,
                        }))),
                    );
                }),
            }
        }
        _ => acc,
    };
    let t = acc.mk_object_from_spread_acc(cx, reason_props, false, false, proto);
    Ok((t, attributes, unresolved_params, children))
}

fn jsx_normalize_children_prop<'a>(
    cx: &Context<'a>,
    loc_children: ALoc,
    children: Vec<type_::UnresolvedParam>,
) -> Vec<Type> {
    children
        .into_iter()
        .map(|child| match child {
            type_::UnresolvedParam::UnresolvedArg(box type_::UnresolvedArgData(elem, _)) => elem.t,
            type_::UnresolvedParam::UnresolvedSpreadArg(a) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc_children.dupe(),
                        UnsupportedSyntax::SpreadArgument,
                    ))),
                );
                any_t::error(reason_of_t(&a).dupe())
            }
        })
        .collect()
}

fn react_jsx_normalize_children_prop<'a>(
    cx: &Context<'a>,
    loc_children: ALoc,
    children: Vec<type_::UnresolvedParam>,
) -> Option<Type> {
    normalize_jsx_children_prop(
        loc_children.dupe(),
        jsx_normalize_children_prop(cx, loc_children, children),
    )
}

fn react_jsx_desugar<'a>(
    cx: &Context<'a>,
    name: FlowSmolStr,
    should_generalize: LazyBool<'a>,
    loc_element: ALoc,
    loc_children: ALoc,
    component_t: Type,
    targs_opt: Option<Vec<Targ>>,
    props: Type,
    children: Vec<type_::UnresolvedParam>,
) -> (Type, Option<Type>) {
    let return_hint = type_env::get_hint(cx, loc_element.dupe());
    let reason = mk_reason(
        VirtualReasonDesc::RReactElement {
            name_opt: Some(Name::new(name)),
            from_component_syntax: false,
        },
        loc_element.dupe(),
    );
    let reason_jsx = mk_reason(
        VirtualReasonDesc::RFunction(ReasonDescFunction::RNormal),
        loc_element.dupe(),
    );
    let get_custom_jsx_factory_type = || {
        flow_js::get_builtin_type_non_speculating(
            cx,
            &reason_jsx,
            Some(false),
            "React$CustomJSXFactory",
        )
    };
    let reason_c = reason_of_t(&component_t).dupe();
    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::ReactCreateElementCall(
        Box::new(ReactCreateElementCallData {
            op: reason_jsx.dupe(),
            component: reason_c.dupe(),
            children: loc_children.dupe(),
        }),
    )));
    let (tout, instantiated_component, use_op) = if cx.react_custom_jsx_typing() {
        let tout_id = flow_typing_tvar::mk_no_wrap(cx, &reason);
        let tout_tvar = Tvar::new(reason.dupe(), tout_id as u32);
        let tout = Type::new(TypeInner::OpenT(tout_tvar.dupe()));
        let mut argts: Vec<CallArg> =
            vec![CallArg::arg(component_t.dupe()), CallArg::arg(props.dupe())];
        for child in &children {
            match child {
                type_::UnresolvedParam::UnresolvedArg(box type_::UnresolvedArgData(elem, _)) => {
                    argts.push(CallArg::arg(elem.t.dupe()));
                }
                type_::UnresolvedParam::UnresolvedSpreadArg(t) => {
                    argts.push(CallArg::spread_arg(t.dupe()));
                }
            }
        }
        let funcalltype = type_::mk_functioncalltype(
            reason.dupe(),
            targs_opt.clone().map(|v| v.into()),
            argts.into(),
            true,
            tout_tvar,
        );
        let custom_jsx_factory_type = get_custom_jsx_factory_type();
        flow_js::flow_non_speculating(
            cx,
            (
                &custom_jsx_factory_type,
                &UseT::new(UseTInner::CallT(Box::new(CallTData {
                    use_op: use_op.dupe(),
                    reason: reason.dupe(),
                    call_action: Box::new(type_::CallAction::Funcalltype(Box::new(funcalltype))),
                    return_hint: return_hint.clone(),
                }))),
            ),
        );
        (tout, None, use_op)
    } else {
        let tout_id = flow_typing_tvar::mk_no_wrap(cx, &reason);
        let tout_tvar = Tvar::new(reason.dupe(), tout_id as u32);
        let tout = Type::new(TypeInner::OpenT(tout_tvar.dupe()));
        let specialized_component = cx.new_specialized_callee();
        flow_js::flow_non_speculating(
            cx,
            (
                &component_t,
                &UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                    use_op: use_op.dupe(),
                    reason: reason.dupe(),
                    tool: Box::new(type_::react::Tool::CreateElement(Box::new(
                        type_::react::CreateElementData {
                            component: component_t.dupe(),
                            jsx_props: props.dupe(),
                            tout: tout_tvar,
                            targs: targs_opt.clone().map(|v| v.into()),
                            should_generalize: *should_generalize.get_forced(cx),
                            return_hint: return_hint.clone(),
                            record_monomorphized_result: false,
                            inferred_targs: None,
                            specialized_component: Some(specialized_component.clone()),
                        },
                    ))),
                }))),
            ),
        );
        let specialized_component_t =
            flow_js_utils::callee_recorder::type_for_tast_opt(reason_c, &specialized_component);
        (tout, specialized_component_t, use_op)
    };
    match cx.react_runtime() {
        flow_common::options::ReactRuntime::Automatic => {
            // TODO(jmbrown): Model jsx more faithfully. children are now passed in as part of the props
            // object. See https://github.com/reactjs/rfcs/blob/createlement-rfc/text/0000-create-element-changes.md
            // for more details.
        }
        flow_common::options::ReactRuntime::Classic => {
            // Under classic jsx, we trust but verify:
            // - We first unconditionally call the right createElement (already done above)
            // - Then we validate that we are calling the right one. By modeling React$CreateElement
            //   as an opaque type bounded by the real definition, we can reliable check it.
            // Validate that we are actually calling the right React.createElement
            let react_t = type_env::var_ref(
                Some(type_env::LookupMode::ForValue),
                cx,
                None,
                Name::new(FlowSmolStr::from("React")),
                loc_element.dupe(),
            );
            let create_element_t = get_prop(
                EnclosingContext::NoContext,
                cx,
                reason.dupe(),
                use_op.dupe(),
                type_::hint_unavailable(),
                react_t.dupe(),
                mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(FlowSmolStr::from(
                        "createElement",
                    )))),
                    loc_element.dupe(),
                ),
                &FlowSmolStr::new("createElement"),
            );
            let expected = if cx.react_custom_jsx_typing() {
                get_custom_jsx_factory_type()
            } else {
                flow_js::get_builtin_type_non_speculating(
                    cx,
                    &reason,
                    Some(false),
                    "React$CreateElement",
                )
            };
            if !flow_typing_utils::speculation_flow::is_flow_successful(
                cx,
                create_element_t,
                UseT::new(UseTInner::UseT(type_::unknown_use(), expected)),
            )
            .unwrap_or(false)
            {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInvalidReactCreateElement(Box::new(
                        EInvalidReactCreateElementData {
                            create_element_loc: loc_element.dupe(),
                            invalid_react: reason_of_t(&react_t).dupe(),
                        },
                    )),
                );
            }
        }
    }
    (tout, instantiated_component)
}

fn non_react_jsx_desugar<'a>(
    cx: &Context<'a>,
    raw_jsx_expr: &str,
    jsx_expr: &expression::Expression<ALoc, ALoc>,
    loc_element: ALoc,
    loc_opening: ALoc,
    component_t: Type,
    targs_opt: Option<Vec<Targ>>,
    props: Type,
    attributes: &[ast::jsx::OpeningAttribute<ALoc, ALoc>],
    children: Vec<type_::UnresolvedParam>,
) -> Result<(Type, Option<Type>), AbnormalControlFlow> {
    let reason = mk_reason(
        VirtualReasonDesc::RJSXFunctionCall(FlowSmolStr::from(raw_jsx_expr)),
        loc_element.dupe(),
    );
    // A JSX element with no attributes should pass in null as the second arg
    let props = if attributes.is_empty() {
        type_::null::at(loc_opening.dupe())
    } else {
        props
    };
    let mut argts: Vec<CallArg> = vec![CallArg::arg(component_t.dupe()), CallArg::arg(props)];
    for child in &children {
        match child {
            type_::UnresolvedParam::UnresolvedArg(box type_::UnresolvedArgData(elem, _)) => {
                argts.push(CallArg::arg(elem.t.dupe()));
            }
            type_::UnresolvedParam::UnresolvedSpreadArg(t) => {
                argts.push(CallArg::spread_arg(t.dupe()));
            }
        }
    }
    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::JSXCreateElement {
        op: reason.dupe(),
        component: reason_of_t(&component_t).dupe(),
    }));
    let t = match jsx_expr.deref() {
        expression::ExpressionInner::Member { inner: member, .. }
            if let expression::member::Property::PropertyIdentifier(id) = &member.property =>
        {
            let (prop_loc, name) = (id.loc.dupe(), id.name.dupe());
            let object = &member.object;
            let ot = jsx_pragma_expression(cx, raw_jsx_expr, object.loc().dupe(), object)?;
            method_call(
                cx, reason, use_op, false, prop_loc, jsx_expr, ot, &name, targs_opt, argts,
            )
            .1
        }
        _ => {
            let f = jsx_pragma_expression(cx, raw_jsx_expr, loc_element.dupe(), jsx_expr)?;
            func_call(
                cx,
                loc_element,
                reason,
                use_op,
                false,
                f,
                targs_opt,
                argts,
                None,
            )
        }
    };
    Ok((t, None))
}

// The @jsx pragma specifies a left hand side expression EXPR such that
//
// <Foo />
//
// is transformed into
//
// EXPR(Foo, props, child1, child2, etc)
//
// This means we need to process EXPR. However, EXPR is not inline in the code,
// it's up in a comment at the top of the file. This means if we run into an
// error, we're going to point at the comment at the top.
//
// We can cover almost all the cases by just explicitly handling identifiers,
// since the common error is that the identifier is not in scope.
fn jsx_pragma_expression<'a>(
    cx: &Context<'a>,
    raw_jsx_expr: &str,
    loc: ALoc,
    expr: &expression::Expression<ALoc, ALoc>,
) -> Result<Type, AbnormalControlFlow> {
    Ok(match expr.deref() {
        expression::ExpressionInner::Identifier { inner: id, .. } => {
            let name = &id.name;
            let desc =
                VirtualReasonDesc::RJSXIdentifier(FlowSmolStr::from(raw_jsx_expr), name.dupe());
            type_env::var_ref(
                Some(type_env::LookupMode::ForValue),
                cx,
                Some(desc),
                Name::new(name.dupe()),
                loc,
            )
        }
        _ => {
            // Oh well, we tried
            let typed_expr = expression(None, None, None, cx, expr)?;
            typed_expr.loc().1.dupe()
        }
    })
}

fn jsx_body<'a>(
    _cx: &Context<'a>,
    _child: &ast::jsx::Child<ALoc, ALoc>,
) -> Result<
    (
        Option<type_::UnresolvedParam>,
        ast::jsx::Child<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    let loc = _child.loc().dupe();
    Ok(match _child {
        ast::jsx::Child::Element { inner: e, .. } => {
            let (t, typed_e) = jsx(
                _cx,
                Rc::new(flow_lazy::Lazy::new_forced(false)),
                loc.dupe(),
                e,
            )?;
            let reason = mk_reason(VirtualReasonDesc::RJSXChild, loc.dupe());
            (
                Some(type_::UnresolvedParam::UnresolvedArg(Box::new(
                    type_::UnresolvedArgData(
                        mk_tuple_element(reason, t.dupe(), None, false, Polarity::Neutral),
                        None,
                    ),
                ))),
                ast::jsx::Child::Element {
                    loc: (loc, t),
                    inner: typed_e,
                },
            )
        }
        ast::jsx::Child::Fragment { inner: f, .. } => {
            let (t, typed_f) = jsx_fragment(
                _cx,
                Rc::new(flow_lazy::Lazy::new_forced(false)),
                loc.dupe(),
                f,
            )?;
            let reason = mk_reason(VirtualReasonDesc::RJSXChild, loc.dupe());
            (
                Some(type_::UnresolvedParam::UnresolvedArg(Box::new(
                    type_::UnresolvedArgData(
                        mk_tuple_element(reason, t.dupe(), None, false, Polarity::Neutral),
                        None,
                    ),
                ))),
                ast::jsx::Child::Fragment {
                    loc: (loc, t),
                    inner: typed_f,
                },
            )
        }
        ast::jsx::Child::ExpressionContainer { inner: ec, .. } => {
            let comments = ec.comments.dupe();
            let (unresolved_param, typed_ex, t) = match &ec.expression {
                ast::jsx::expression_container::Expression::Expression(e) => {
                    let typed_e = expression_inner(
                        Some(EnclosingContext::JsxAttrOrChildrenContext),
                        None,
                        None,
                        FrozenKind::NotFrozen,
                        None,
                        _cx,
                        e,
                    )?;
                    let e_loc = typed_e.loc().0.dupe();
                    let e_t = typed_e.loc().1.dupe();
                    let reason = mk_reason(VirtualReasonDesc::RJSXChild, e_loc);
                    (
                        Some(type_::UnresolvedParam::UnresolvedArg(Box::new(
                            type_::UnresolvedArgData(
                                mk_tuple_element(
                                    reason,
                                    e_t.dupe(),
                                    None,
                                    false,
                                    Polarity::Neutral,
                                ),
                                None,
                            ),
                        ))),
                        ast::jsx::expression_container::Expression::Expression(typed_e),
                        e_t,
                    )
                }
                ast::jsx::expression_container::Expression::EmptyExpression => {
                    let any_t = any_t::at(type_::AnySource::Untyped, loc.dupe());
                    (
                        None,
                        ast::jsx::expression_container::Expression::EmptyExpression,
                        any_t,
                    )
                }
            };
            (
                unresolved_param,
                ast::jsx::Child::ExpressionContainer {
                    loc: (loc, t),
                    inner: ast::jsx::ExpressionContainer {
                        expression: typed_ex,
                        comments,
                    },
                },
            )
        }
        ast::jsx::Child::SpreadChild { inner: sc, .. } => {
            let typed_e = expression(None, None, None, _cx, &sc.expression)?;
            let (_, t) = typed_e.loc();
            let t = t.dupe();
            (
                Some(type_::UnresolvedParam::UnresolvedSpreadArg(t.dupe())),
                ast::jsx::Child::SpreadChild {
                    loc: (loc, t.dupe()),
                    inner: ast::jsx::SpreadChild {
                        expression: typed_e,
                        comments: sc.comments.dupe(),
                    },
                },
            )
        }
        ast::jsx::Child::Text { inner: text, .. } => {
            let unresolved_param_opt = match jsx_trim_text(loc.dupe(), &text.value) {
                Some(c) => {
                    let reason = mk_reason(VirtualReasonDesc::RJSXChild, loc.dupe());
                    Some(type_::UnresolvedParam::UnresolvedArg(Box::new(
                        type_::UnresolvedArgData(
                            mk_tuple_element(reason, c, None, false, Polarity::Neutral),
                            None,
                        ),
                    )))
                }
                None => None,
            };
            let any_t = any_t::at(type_::AnySource::Untyped, loc.dupe());
            (
                unresolved_param_opt,
                ast::jsx::Child::Text {
                    loc: (loc, any_t),
                    inner: text.clone(),
                },
            )
        }
    })
}

fn jsx_trim_text(loc: ALoc, value: &str) -> Option<Type> {
    flow_common_utils::utils_jsx::trim_jsx_text(loc.to_loc_exn().dupe(), value).map(
        |(trimmed_loc, trimmed)| {
            Type::new(TypeInner::DefT(
                mk_reason(VirtualReasonDesc::RJSXText, ALoc::of_loc(trimmed_loc)),
                DefT::new(DefTInner::SingletonStrT {
                    from_annot: false,
                    value: Name::new(FlowSmolStr::from(trimmed.as_str())),
                }),
            ))
        },
    )
}

fn jsx_title_member_to_string(member: &ast::jsx::MemberExpression<ALoc, ALoc>) -> String {
    let name = &member.property.name;
    match &member.object {
        ast::jsx::member_expression::Object::MemberExpression(inner_member) => {
            jsx_title_member_to_string(inner_member) + "." + name
        }
        ast::jsx::member_expression::Object::Identifier(obj_id) => {
            format!("{}.{}", obj_id.name, name)
        }
    }
}

fn jsx_title_namespaced_name_to_string(
    namespaced_name: &ast::jsx::NamespacedName<ALoc, ALoc>,
) -> String {
    let namespace = &namespaced_name.namespace;
    let name = &namespaced_name.name;
    format!("{}{}", namespace.name, name.name)
}

fn jsx_title_member_to_expression(
    member: &ast::jsx::MemberExpression<ALoc, ALoc>,
) -> expression::Expression<ALoc, ALoc> {
    let mloc = member.loc.dupe();
    let object = match &member.object {
        ast::jsx::member_expression::Object::MemberExpression(inner_member) => {
            jsx_title_member_to_expression(inner_member)
        }
        ast::jsx::member_expression::Object::Identifier(id) if id.name == "this" => {
            expression::Expression::new(expression::ExpressionInner::This {
                loc: id.loc.dupe(),
                inner: std::sync::Arc::new(expression::This {
                    comments: id.comments.dupe(),
                }),
            })
        }
        ast::jsx::member_expression::Object::Identifier(id) => {
            let ident = ast::Identifier::new(ast::IdentifierInner {
                loc: id.loc.dupe(),
                name: id.name.dupe(),
                comments: id.comments.dupe(),
            });
            expression::Expression::new(expression::ExpressionInner::Identifier {
                loc: id.loc.dupe(),
                inner: ident,
            })
        }
    };
    let prop_id = &member.property;
    let property = ast::Identifier::new(ast::IdentifierInner {
        loc: prop_id.loc.dupe(),
        name: prop_id.name.dupe(),
        comments: prop_id.comments.dupe(),
    });
    expression::Expression::new(expression::ExpressionInner::Member {
        loc: mloc,
        inner: std::sync::Arc::new(expression::Member {
            object,
            property: expression::member::Property::PropertyIdentifier(property),
            comments: None,
        }),
    })
}

// reverses jsx_title_member_to_expression
fn expression_to_jsx_title_member(
    loc: ALoc,
    member: &expression::Expression<ALoc, (ALoc, Type)>,
) -> Option<ast::jsx::MemberExpression<ALoc, (ALoc, Type)>> {
    match member.deref() {
        expression::ExpressionInner::Member { loc: _, inner } => {
            let member_inner = inner.as_ref();
            if let expression::member::Property::PropertyIdentifier(prop_ident) =
                &member_inner.property
            {
                let name = prop_ident.name.dupe();
                let comments = prop_ident.comments.dupe();
                let pannot = prop_ident.loc.dupe();
                let obj_expr = &member_inner.object;
                let obj_loc = obj_expr.loc().dupe();
                let jsx_object = match obj_expr.deref() {
                    expression::ExpressionInner::This {
                        loc: this_loc,
                        inner: this_inner,
                    } => Some(ast::jsx::member_expression::Object::Identifier(
                        ast::jsx::Identifier {
                            loc: this_loc.dupe(),
                            name: FlowSmolStr::from("this"),
                            comments: this_inner.comments.dupe(),
                        },
                    )),
                    expression::ExpressionInner::Identifier {
                        loc: id_loc,
                        inner: id_inner,
                    } => Some(ast::jsx::member_expression::Object::Identifier(
                        ast::jsx::Identifier {
                            loc: id_loc.dupe(),
                            name: id_inner.name.dupe(),
                            comments: id_inner.comments.dupe(),
                        },
                    )),
                    _ => {
                        let mloc = obj_loc.0.dupe();
                        expression_to_jsx_title_member(mloc, obj_expr).map(|e| {
                            ast::jsx::member_expression::Object::MemberExpression(e.into())
                        })
                    }
                };
                let property = ast::jsx::Identifier {
                    loc: pannot,
                    name,
                    comments,
                };
                jsx_object.map(|obj| ast::jsx::MemberExpression {
                    loc,
                    object: obj,
                    property,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

// Conditional expressions are checked like expressions, except that property
// accesses are provisionally allowed even when such properties do not exist.
// This accommodates the common JavaScript idiom of testing for the existence
// of a property before using that property.
fn condition<'a>(
    cx: &Context<'a>,
    encl_ctx: EnclosingContext,
    decl: Option<ast::VariableKind>,
    has_hint: Option<LazyBool<'a>>,
    e: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let result = expression_inner(
        Some(encl_ctx),
        decl,
        None,
        FrozenKind::NotFrozen,
        has_hint,
        cx,
        e,
    )?;
    cx.add_condition(result.clone());
    Ok(result)
}

fn get_private_field_opt_use<'a>(
    cx: &Context<'a>,
    reason: Reason,
    use_op: UseOp,
    name: &FlowSmolStr,
) -> type_::OptUseT<Context<'a>> {
    let class_entries = type_env::get_class_entries(cx);
    type_::OptUseT::OptGetPrivatePropT(use_op, reason, name.dupe(), class_entries.into(), false)
}

// Property lookups become non-strict when processing conditional expressions
// (see above).
//
// TODO: It should be possible to factor the processing of LHS / reference
// expressions out of `expression`, somewhat like what assignment_lhs does. That
// would make everything involving Refinement be in the same place.
fn get_prop_opt_use<CX>(
    encl_ctx: EnclosingContext,
    reason: Reason,
    use_op: UseOp,
    hint: type_::LazyHintT<CX>,
    prop_reason: Reason,
    name: &FlowSmolStr,
) -> type_::OptUseT<CX> {
    let id = mk_id();
    let prop_name = Name::new(name.dupe());
    if encl_ctx.is_conditional_test_context() {
        type_::OptUseT::OptTestPropT(
            use_op,
            reason,
            id as i32,
            mk_named_prop(prop_reason, false, prop_name),
            hint,
        )
    } else {
        type_::OptUseT::OptGetPropT {
            use_op,
            reason,
            id: Some(id as i32),
            propref: mk_named_prop(prop_reason, false, prop_name),
            hint,
        }
    }
}

pub fn get_prop<'a>(
    encl_ctx: EnclosingContext,
    cx: &Context<'a>,
    reason: Reason,
    use_op: UseOp,
    hint: type_::LazyHintT<Context<'a>>,
    tobj: Type,
    prop_reason: Reason,
    name: &FlowSmolStr,
) -> Type {
    let opt_use = get_prop_opt_use(encl_ctx, reason.dupe(), use_op, hint, prop_reason, name);
    tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(cx, reason, |cx, t_reason, t_id| {
        let tvar = Tvar::new(t_reason.dupe(), t_id as u32);
        let get_prop_u = type_::apply_opt_use(opt_use.clone(), tvar);
        flow_js::flow_non_speculating(cx, (&tobj, &get_prop_u));
    })
}

fn static_method_call_object<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    callee_loc: ALoc,
    prop_loc: ALoc,
    expr: &expression::Expression<ALoc, ALoc>,
    obj_t: Type,
    m: &FlowSmolStr,
    targs: Option<&expression::CallTypeArgs<ALoc, ALoc>>,
    args: &expression::ArgList<ALoc, ALoc>,
) -> Result<
    (
        Type,
        Option<expression::CallTypeArgs<ALoc, (ALoc, Type)>>,
        expression::ArgList<ALoc, (ALoc, Type)>,
    ),
    AbnormalControlFlow,
> {
    use ast::expression::ExpressionOrSpread;
    let reason = mk_reason(RCustom(format!("`Object.{}`", m).into()), loc.dupe());
    let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCallMethod(Box::new(
        FunCallMethodData {
            op: reason.dupe(),
            fn_: mk_reason(RMethod(Some(m.dupe())), callee_loc.dupe()),
            prop: mk_reason(RProperty(Some(Name::new(m.dupe()))), prop_loc.dupe()),
            args: mk_initial_arguments_reason(args).into(),
            local: true,
        },
    ))));
    let get_keys = |arr_reason: Reason, obj_t: &Type| -> Type {
        let reason_clone = reason.dupe();
        let use_op_clone = use_op.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, arr_reason, |cx, tvar| {
            let keys_reason = reason_clone.dupe().update_desc_new(|desc| {
                RCustom(format!("element of {}", string_of_desc(&desc)).into())
            });
            flow_js::flow_non_speculating(
                cx,
                (
                    obj_t,
                    &UseT::new(UseTInner::GetKeysT(
                        keys_reason,
                        Box::new(UseT::new(UseTInner::UseT(use_op_clone.dupe(), tvar.dupe()))),
                    )),
                ),
            );
        })
    };
    let get_values = |arr_reason: Reason, obj_t: &Type| -> Type {
        let reason_clone = reason.dupe();
        let use_op_clone = use_op.dupe();
        tvar_resolver::mk_tvar_and_fully_resolve_where(cx, arr_reason, |cx, tvar| {
            flow_js::flow_non_speculating(
                cx,
                (
                    obj_t,
                    &UseT::new(UseTInner::GetDictValuesT(
                        reason_clone.dupe(),
                        Box::new(UseT::new(UseTInner::UseT(use_op_clone.dupe(), tvar.dupe()))),
                    )),
                ),
            );
        })
    };
    let args_arguments = &args.arguments;
    let args_comments = &args.comments;
    let args_loc = args.loc.dupe();
    Ok(match (m.as_str(), targs, &**args_arguments) {
        ("assign", None, []) => {
            let use_op = UseOp::Frame(
                Arc::new(FrameUseOp::FunMissingArg(Box::new(FunMissingArgData {
                    def: reason.dupe(),
                    op: reason.dupe(),
                    n: 0,
                }))),
                Arc::new(use_op),
            );
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower: reason.dupe(),
                    reason_upper: reason.dupe(),
                    use_op,
                    explanation: None,
                })),
            );
            let t = any_t::error(reason);
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        ("assign", None, [first_arg, rest_args @ ..]) => {
            let (first_arg_t, first_arg_typed) = expression_or_spread(cx, first_arg)?;
            let rest_args_with_ts: Vec<_> = rest_args
                .iter()
                .map(|a| expression_or_spread(cx, a))
                .collect::<Result<Vec<_>, _>>()?;
            let (rest_arg_ts, rest_args): (Vec<_>, Vec<_>) = rest_args_with_ts.into_iter().unzip();
            let target_t = match &*first_arg_t {
                CallArgInner::Arg(t) => t.dupe(),
                CallArgInner::SpreadArg(arr) => {
                    let reason = reason_of_t(arr).dupe();
                    let loc = loc_of_t(arr).dupe();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            loc,
                            UnsupportedSyntax::SpreadArgument,
                        ))),
                    );
                    any_t::error(reason)
                }
            };
            let mut all_args = vec![first_arg_typed];
            all_args.extend(rest_args);
            let typed_args = expression::ArgList {
                loc: args_loc,
                arguments: all_args.into(),
                comments: args_comments.clone(),
            };
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsafeObjectAssign(loc.dupe()),
            );
            let t = type_operation_utils::special_cased_functions::object_assign(
                cx,
                &use_op,
                &reason,
                &target_t,
                &rest_arg_ts,
            );
            (t, None, typed_args)
        }
        ("getPrototypeOf", None, [ExpressionOrSpread::Expression(e)]) => {
            let e_ast = expression(None, None, None, cx, e)?;
            let e_t = e_ast.loc().1.dupe();
            let proto_reason = mk_reason(RPrototype, e.loc().dupe());
            let t = tvar_resolver::mk_tvar_and_fully_resolve_no_wrap_where(
                cx,
                proto_reason.dupe(),
                |cx, tout_reason, tout_id| {
                    let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                    flow_js::flow_non_speculating(
                        cx,
                        (
                            &e_t,
                            &UseT::new(UseTInner::GetProtoT(proto_reason.dupe(), Box::new(tout))),
                        ),
                    );
                },
            );
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        ("create", None, [ExpressionOrSpread::Expression(e)]) => {
            let e_ast = expression(None, None, None, cx, e)?;
            let e_t = e_ast.loc().1.dupe();
            let proto_reason = mk_reason(RPrototype, e.loc().dupe());
            let proto =
                tvar_resolver::mk_tvar_and_fully_resolve_where(cx, proto_reason.dupe(), |cx, t| {
                    flow_js::flow_non_speculating(
                        cx,
                        (
                            &e_t,
                            &UseT::new(UseTInner::ObjTestProtoT(proto_reason.dupe(), t.dupe())),
                        ),
                    );
                });
            let t = obj_type::mk_with_proto(
                cx,
                reason.dupe(),
                ObjKind::Exact,
                None,
                None,
                None,
                None,
                proto,
            );
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        (
            "create",
            None,
            [
                ExpressionOrSpread::Expression(e),
                ExpressionOrSpread::Expression(obj_expr),
            ],
        ) if let expression::ExpressionInner::Object { loc, inner } = &**obj_expr => {
            let (obj_loc, obj_properties, obj_comments) =
                (loc.dupe(), &inner.properties, &inner.comments);
            error_on_this_uses_in_object_methods(cx, obj_properties);
            let e_ast = expression(None, None, None, cx, e)?;
            let e_t = e_ast.loc().1.dupe();
            let proto_reason = mk_reason(RPrototype, e.loc().dupe());
            let proto =
                tvar_resolver::mk_tvar_and_fully_resolve_where(cx, proto_reason.dupe(), |cx, t| {
                    flow_js::flow_non_speculating(
                        cx,
                        (
                            &e_t,
                            &UseT::new(UseTInner::ObjTestProtoT(proto_reason.dupe(), t.dupe())),
                        ),
                    );
                });
            let (pmap, properties_typed) = prop_map_of_object(cx, obj_properties)?;
            let propdesc_type =
                flow_js_utils::lookup_builtin_type(cx, "PropertyDescriptor", reason.dupe());
            let mut props_entries: BTreeMap<Name, Property> = BTreeMap::new();
            for (x, p) in pmap.iter() {
                let key_loc = property::read_loc(p);
                match property::read_t(p) {
                    None => {
                        // Since the properties object must be a literal, and literal objects
                        // can only ever contain neutral fields, this should not happen.
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EInternal(Box::new((
                                prop_loc.dupe(),
                                InternalError::PropertyDescriptorPropertyCannotBeRead,
                            ))),
                        );
                    }
                    Some(spec) => {
                        let prop_reason = reason.dupe().update_desc_new(|desc| {
                            RCustom(format!(".{} of {}", x.as_str(), string_of_desc(&desc)).into())
                        });
                        let propdesc_type_clone = propdesc_type.dupe();
                        let use_op_clone = use_op.dupe();
                        let t = tvar_resolver::mk_tvar_and_fully_resolve_where(
                            cx,
                            prop_reason.dupe(),
                            |cx, tvar| {
                                let annot_loc = prop_reason.loc().dupe();
                                let propdesc = implicit_typeapp(
                                    propdesc_type_clone.dupe(),
                                    vec![tvar.dupe()],
                                    Some(annot_loc),
                                );
                                flow_js::flow_non_speculating(
                                    cx,
                                    (
                                        &spec,
                                        &UseT::new(UseTInner::UseT(use_op_clone.dupe(), propdesc)),
                                    ),
                                );
                            },
                        );
                        let prop = Property::new(PropertyInner::Field(Box::new(FieldData {
                            preferred_def_locs: None,
                            key_loc,
                            type_: t,
                            polarity: Polarity::Neutral,
                        })));
                        props_entries.insert(x.dupe(), prop);
                    }
                }
            }
            let props = properties::PropertiesMap::from_btree_map(props_entries);
            let t = obj_type::mk_with_proto(
                cx,
                reason.dupe(),
                ObjKind::Exact,
                None,
                None,
                Some(props),
                None,
                proto,
            );
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![
                        ExpressionOrSpread::Expression(e_ast),
                        // TODO(vijayramamurthy) construct object type
                        ExpressionOrSpread::Expression(ast::expression::Expression::new(
                            ast::expression::ExpressionInner::Object {
                                loc: (obj_loc.dupe(), any_t::at(AnySource::Untyped, obj_loc)),
                                inner: (ast::expression::Object {
                                    properties: properties_typed.into(),
                                    comments: obj_comments.clone(),
                                })
                                .into(),
                            },
                        )),
                    ]
                    .into(),
                    comments: args_comments.clone(),
                },
            )
        }
        ("getOwnPropertyNames" | "keys", None, [ExpressionOrSpread::Expression(e)]) => {
            let arr_reason = mk_reason(RArrayType, loc.dupe());
            let e_ast = expression(None, None, None, cx, e)?;
            let o = e_ast.loc().1.dupe();
            let keys_t = get_keys(arr_reason.dupe(), &o);
            let t = Type::new(TypeInner::DefT(
                arr_reason,
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                    ArrayATData {
                        elem_t: keys_t,
                        tuple_view: None,
                        react_dro: None,
                    },
                ))))),
            ));
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        ("values", None, [ExpressionOrSpread::Expression(e)]) => {
            let arr_reason = mk_reason(RArrayType, loc.dupe());
            let e_ast = expression(None, None, None, cx, e)?;
            let o = e_ast.loc().1.dupe();
            let values_t = get_values(arr_reason.dupe(), &o);
            let t = Type::new(TypeInner::DefT(
                arr_reason,
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                    ArrayATData {
                        elem_t: values_t,
                        tuple_view: None,
                        react_dro: None,
                    },
                ))))),
            ));
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        ("entries", None, [ExpressionOrSpread::Expression(e)]) => {
            let arr_reason = mk_reason(RArrayType, loc.dupe());
            let e_ast = expression(None, None, None, cx, e)?;
            let o = e_ast.loc().1.dupe();
            let keys_t = get_keys(arr_reason.dupe(), &o);
            let values_t = get_values(arr_reason.dupe(), &o);
            let elem_reason = mk_reason(RTupleElement { name: None }, loc.dupe());
            let elem_t = Type::new(TypeInner::UnionT(
                elem_reason.dupe(),
                union_rep::make(
                    None,
                    union_rep::UnionKind::UnknownKind,
                    keys_t.dupe(),
                    values_t.dupe(),
                    vec![].into(),
                ),
            ));
            let entry_t = Type::new(TypeInner::DefT(
                mk_reason(RTupleType, loc.dupe()),
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT(Box::new(
                    TupleATData {
                        elem_t,
                        react_dro: None,
                        elements: vec![
                            mk_tuple_element(
                                elem_reason.dupe(),
                                keys_t,
                                Some(FlowSmolStr::new("key")),
                                false,
                                Polarity::Neutral,
                            ),
                            mk_tuple_element(
                                elem_reason,
                                values_t,
                                Some(FlowSmolStr::new("value")),
                                false,
                                Polarity::Neutral,
                            ),
                        ]
                        .into(),
                        arity: (2, 2),
                        inexact: false,
                    },
                ))))),
            ));
            let t = Type::new(TypeInner::DefT(
                arr_reason,
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                    ArrayATData {
                        elem_t: entry_t,
                        tuple_view: None,
                        react_dro: None,
                    },
                ))))),
            ));
            (
                t,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        (
            "defineProperty",
            targs_val,
            [
                ExpressionOrSpread::Expression(e),
                ExpressionOrSpread::Expression(key_expr),
                ExpressionOrSpread::Expression(config),
            ],
        ) if let expression::ExpressionInner::StringLiteral { loc, inner } = &**key_expr
            && (targs_val.is_none()
                || matches!(
                    targs_val.unwrap().arguments.as_ref(),
                    [ast::expression::CallTypeArg::Explicit(_)]
                )) =>
        {
            let (ploc, x) = (loc.dupe(), &inner.value);
            let (ty, typed_targs): (Type, Option<expression::CallTypeArgs<ALoc, (ALoc, Type)>>) =
                match targs_val {
                    None => (flow_typing_tvar::mk(cx, reason.dupe()), None),
                    Some(call_targs) => {
                        let [expression::CallTypeArg::Explicit(targ)] = &*call_targs.arguments
                        else {
                            //   | _ -> assert_false "unexpected type argument to Object.defineProperty, match guard failed"
                            panic!(
                                "unexpected type argument to Object.defineProperty, match guard failed"
                            )
                        };
                        let targ_typed = type_annotation::convert(cx, FlowOrdMap::new(), targ);
                        let ty = targ_typed.loc().1.dupe();
                        (
                            ty.dupe(),
                            Some(expression::CallTypeArgs {
                                loc: (call_targs.loc.dupe(), ty.dupe()),
                                arguments: vec![ast::expression::CallTypeArg::Explicit(targ_typed)]
                                    .into(),
                                comments: call_targs.comments.dupe(),
                            }),
                        )
                    }
                };
            let reason_loc = reason.loc().dupe();
            let propdesc_type =
                flow_js_utils::lookup_builtin_type(cx, "PropertyDescriptor", reason.dupe());
            let propdesc = implicit_typeapp(propdesc_type, vec![ty.dupe()], Some(reason_loc));
            let e_ast = expression(None, None, None, cx, e)?;
            let o = e_ast.loc().1.dupe();
            let key_ast = expression(None, None, None, cx, key_expr)?;
            let config_ast = expression(None, None, None, cx, config)?;
            let spec = config_ast.loc().1.dupe();
            let prop_name = Name::new(x.dupe());
            let prop_reason = mk_reason(RProperty(Some(prop_name.dupe())), ploc.dupe());
            flow_js::flow_non_speculating(
                cx,
                (&spec, &UseT::new(UseTInner::UseT(use_op.dupe(), propdesc))),
            );
            let prop_t = flow_typing_tvar::mk(cx, prop_reason.dupe());
            flow_js::flow_non_speculating(
                cx,
                (
                    &o,
                    &UseT::new(UseTInner::SetPropT(
                        use_op.dupe(),
                        reason,
                        Box::new(mk_named_prop(prop_reason, false, prop_name)),
                        SetMode::Assign,
                        WriteCtx::Normal,
                        ty,
                        Some(prop_t),
                    )),
                ),
            );
            (
                o,
                typed_targs,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![
                        ExpressionOrSpread::Expression(e_ast),
                        ExpressionOrSpread::Expression(key_ast),
                        ExpressionOrSpread::Expression(config_ast),
                    ]
                    .into(),
                    comments: args_comments.clone(),
                },
            )
        }
        (
            "defineProperties",
            None,
            [
                ExpressionOrSpread::Expression(e),
                ExpressionOrSpread::Expression(obj_expr),
            ],
        ) if let expression::ExpressionInner::Object { loc, inner } = &**obj_expr => {
            let (obj_loc, obj_properties, obj_comments) =
                (loc.dupe(), &inner.properties, &inner.comments);
            error_on_this_uses_in_object_methods(cx, obj_properties);
            let e_ast = expression(None, None, None, cx, e)?;
            let o = e_ast.loc().1.dupe();
            let (pmap, properties_typed) = prop_map_of_object(cx, obj_properties)?;
            let propdesc_type =
                flow_js_utils::lookup_builtin_type(cx, "PropertyDescriptor", reason.dupe());
            for (x, p) in pmap.iter() {
                match property::read_t(p) {
                    None => {
                        // Since the properties object must be a literal, and literal objects
                        // can only ever contain neutral fields, this should not happen.
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EInternal(Box::new((
                                prop_loc.dupe(),
                                InternalError::PropertyDescriptorPropertyCannotBeRead,
                            ))),
                        );
                    }
                    Some(spec) => {
                        let prop_reason = reason.dupe().update_desc_new(|desc| {
                            RCustom(format!(".{} of {}", x.as_str(), string_of_desc(&desc)).into())
                        });
                        let tvar = flow_typing_tvar::mk(cx, prop_reason.dupe());
                        let annot_loc = prop_reason.loc().dupe();
                        let propdesc = implicit_typeapp(
                            propdesc_type.dupe(),
                            vec![tvar.dupe()],
                            Some(annot_loc),
                        );
                        flow_js::flow_non_speculating(
                            cx,
                            (&spec, &UseT::new(UseTInner::UseT(use_op.dupe(), propdesc))),
                        );
                        flow_js::flow_non_speculating(
                            cx,
                            (
                                &o,
                                &UseT::new(UseTInner::SetPropT(
                                    use_op.dupe(),
                                    prop_reason.dupe(),
                                    Box::new(mk_named_prop(prop_reason, false, x.dupe())),
                                    SetMode::Assign,
                                    WriteCtx::Normal,
                                    tvar,
                                    None,
                                )),
                            ),
                        );
                    }
                }
            }
            (
                o,
                None,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![
                        ExpressionOrSpread::Expression(e_ast),
                        // TODO(vijayramamurthy) construct object type
                        ExpressionOrSpread::Expression(ast::expression::Expression::new(
                            ast::expression::ExpressionInner::Object {
                                loc: (obj_loc.dupe(), any_t::at(AnySource::Untyped, obj_loc)),
                                inner: (ast::expression::Object {
                                    properties: properties_typed.into(),
                                    comments: obj_comments.clone(),
                                })
                                .into(),
                            },
                        )),
                    ]
                    .into(),
                    comments: args_comments.clone(),
                },
            )
        }
        // Freezing an object literal is supported since there's no way it could
        // have been mutated elsewhere
        ("freeze", targs_val, [ExpressionOrSpread::Expression(arg_expr)])
            if let expression::ExpressionInner::Object { inner, .. } = &**arg_expr
                && (targs_val.is_none() || targs_val.unwrap().arguments.len() == 1) =>
        {
            let arg_loc = arg_expr.loc().dupe();
            let o = inner;
            let typed_targs: Option<(Vec<Targ>, expression::CallTypeArgs<ALoc, (ALoc, Type)>)> =
                targs_val.map(|call_targs| {
                    let (targts, targs_ast) =
                        convert_call_targs(cx, &FlowOrdMap::new(), call_targs);
                    (targts, targs_ast)
                });
            let (obj_t_inner, properties_typed) = object_(
                cx,
                true,
                false,
                Rc::new(flow_lazy::Lazy::new_forced(false)),
                arg_loc.dupe(),
                &o.properties,
            )?;
            let e_ast = expression::Expression::new(expression::ExpressionInner::Object {
                loc: (arg_loc, obj_t_inner.dupe()),
                inner: (expression::Object {
                    properties: properties_typed.into(),
                    comments: o.comments.dupe(),
                })
                .into(),
            });
            let arg_t = obj_t_inner;
            let method_reason = mk_reason(RMethodCall(Some(m.dupe())), loc.dupe());
            let method_targts = typed_targs.as_ref().map(|(targts, _)| targts.clone());
            let (_, result_t) = method_call(
                cx,
                method_reason,
                use_op,
                true,
                prop_loc,
                expr,
                obj_t,
                m,
                method_targts,
                vec![CallArg::arg(arg_t)],
            );
            let result_targs = typed_targs.map(|(_, targs_ast)| targs_ast);
            (
                result_t,
                result_targs,
                expression::ArgList {
                    loc: args_loc,
                    arguments: vec![ExpressionOrSpread::Expression(e_ast)].into(),
                    comments: args_comments.clone(),
                },
            )
        }
        (
            "assign"
            | "create"
            | "getOwnPropertyNames"
            | "getPrototypeOf"
            | "keys"
            | "defineProperty"
            | "defineProperties"
            | "freeze",
            Some(call_targs),
            _,
        ) => {
            let (_, targs_ast) = convert_call_targs(cx, &FlowOrdMap::new(), call_targs);
            let (_argts, typed_args) = arg_list(cx, args)?;
            let arity = if m == "freeze" || m == "defineProperty" {
                1
            } else {
                0
            };
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                    call_loc: loc.dupe(),
                    is_new: false,
                    reason_arity: locationless_reason(RFunction(ReasonDescFunction::RNormal)),
                    expected_arity: arity,
                })),
            );
            (
                any_t::at(AnySource::AnyError(None), loc),
                Some(targs_ast),
                typed_args,
            )
        }
        // TODO
        _ => {
            let (targts, targ_asts) = convert_call_targs_opt(cx, targs);
            let (argts, arg_asts) = arg_list(cx, args)?;
            let method_reason = mk_reason(RMethodCall(Some(m.dupe())), loc.dupe());
            let default_use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunCallMethod(Box::new(
                FunCallMethodData {
                    op: method_reason.dupe(),
                    fn_: mk_reason(RMethod(Some(m.dupe())), callee_loc.dupe()),
                    prop: mk_reason(RProperty(Some(Name::new(m.dupe()))), prop_loc.dupe()),
                    args: mk_initial_arguments_reason(args).into(),
                    local: true,
                },
            ))));
            let (_, result_t) = method_call(
                cx,
                method_reason,
                default_use_op,
                true,
                prop_loc,
                expr,
                obj_t,
                m,
                targts,
                argts,
            );
            (result_t, targ_asts, arg_asts)
        }
    })
}

fn mk_class<'a>(
    cx: &Context<'a>,
    class_loc: ALoc,
    name_loc: ALoc,
    tast_class_type: Option<Type>,
    reason: Reason,
    c: &ast::class::Class<ALoc, ALoc>,
) -> Result<(Type, ast::class::Class<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let def_reason = reason.dupe().reposition(class_loc.dupe());
    let (class_t, _, class_sig, class_ast_f) =
        mk_class_sig(cx, name_loc, class_loc, InstanceKind::ClassKind, reason, c)?;
    let public_property_map =
        class_sig::fields_to_prop_map(cx, class_sig::public_fields_of_signature(false, &class_sig));
    let private_property_map = class_sig::fields_to_prop_map(
        cx,
        class_sig::private_fields_of_signature(false, &class_sig),
    );
    class_sig::check_signature_compatibility(cx, def_reason, &class_sig);
    class_sig::toplevels(cx, &class_sig)?;
    let class_body = &c.body.body;
    cx.add_voidable_check(flow_typing_context::VoidableCheck {
        public_property_map,
        private_property_map,
        errors: flow_analysis::property_assignment::eval_property_assignment(class_body),
    });
    let tast_class_type = tast_class_type.unwrap_or_else(|| class_t.dupe());
    Ok((class_t, class_ast_f(cx, tast_class_type)))
}

fn check_duplicate_class_member<'a>(
    cx: &Context<'a>,
    seen_names: &mut SeenNames,
    static_: bool,
    private_: bool,
    name: &FlowSmolStr,
    kind: ClassMemberKind,
    loc: ALoc,
    class_kind: intermediate_error_types::ClassKind,
) {
    if private_ {
        // duplicate private names are a parser error - so we don't need to check them
        return;
    }
    let name_key = name.dupe();
    let names_map = if static_ {
        &mut seen_names.static_names
    } else {
        &mut seen_names.instance_names
    };
    match names_map.get(&name_key).copied() {
        Some(seen) => match (kind, seen) {
            (ClassMemberKind::ClassMemberGetter, ClassMemberKind::ClassMemberSetter)
            | (ClassMemberKind::ClassMemberSetter, ClassMemberKind::ClassMemberGetter) => {
                // One getter and one setter are allowed as long as it's not used as a field
                // We use the special type here to indicate we've seen both a getter and a
                // setter for the name so that future getters/setters can have an error raised.
                names_map.insert(name_key, ClassMemberKind::ClassMemberGetterSetter);
            }
            _ => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDuplicateClassMember(Box::new(EDuplicateClassMemberData {
                        loc,
                        name: name_key,
                        is_static: static_,
                        class_kind,
                    })),
                );
            }
        },
        None => {
            names_map.insert(name_key, kind);
        }
    }
}

/// Process a class definition, returning a (polymorphic) class type. A class
/// type is a wrapper around an instance type, which contains types of instance
/// members, a pointer to the super instance type, and a container for types of
/// static members. The static members can be thought of as instance members of a
/// "metaclass": thus, the static type is itself implemented as an instance type.
pub fn mk_class_sig<'a>(
    cx: &Context<'a>,
    name_loc: ALoc,
    class_loc: ALoc,
    inst_kind: InstanceKind,
    reason: Reason,
    cls: &ast::class::Class<ALoc, ALoc>,
) -> Result<
    (
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<dyn Fn(&Context<'a>, Type) -> ast::class::Class<ALoc, (ALoc, Type)> + 'a>,
    ),
    AbnormalControlFlow,
> {
    use std::cell::RefCell;
    use std::rc::Rc;

    use flow_typing_loc_env::func_class_sig_types::StmtConfigTypes;
    use flow_typing_loc_env::func_class_sig_types::class as class_types;

    // Given information about a field, returns:
    // - Class_sig.field representation of this field
    // - typed AST of the field's type annotation
    // - a function which will return a typed AST of the field's initializer expression.
    //   Function should only be called after Class_sig.toplevels has been called on a
    //   Class_sig.t containing this field, as that is when the initializer expression
    //   gets checked.
    let mk_field = |cx: &Context<'a>,
                    tparams_map: &FlowOrdMap<SubstName, Type>,
                    reason: Reason,
                    annot: &ast::types::AnnotationOrHint<ALoc, ALoc>,
                    init: &ast::class::property::Value<ALoc, ALoc>|
     -> Result<
        (
            class_types::Field<StmtConfigTypes>,
            Type,
            ast::types::AnnotationOrHint<ALoc, (ALoc, Type)>,
            Box<dyn FnOnce() -> ast::class::property::Value<ALoc, (ALoc, Type)>>,
        ),
        AbnormalControlFlow,
    > {
        let unconditionally_required_annot = |cx: &Context<'a>,
                                              tparams_map: &FlowOrdMap<SubstName, Type>,
                                              reason: &Reason,
                                              annot: &ast::types::AnnotationOrHint<ALoc, ALoc>|
         -> (
            Type,
            ast::types::AnnotationOrHint<ALoc, (ALoc, Type)>,
        ) {
            match annot {
                ast::types::AnnotationOrHint::Missing(loc) => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EMissingLocalAnnotation {
                            reason: reason.dupe(),
                            hint_available: false,
                            from_generic_function: false,
                        },
                    );
                    let t = any_t::make(
                        type_::AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                        reason.dupe(),
                    );
                    (
                        t.dupe(),
                        ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                    )
                }
                ast::types::AnnotationOrHint::Available(annot) => {
                    let (t, ast_annot) = type_annotation::mk_type_available_annotation(
                        cx,
                        tparams_map
                            .iter()
                            .map(|(k, v)| (k.dupe(), v.dupe()))
                            .collect(),
                        annot,
                    );
                    (t, ast::types::AnnotationOrHint::Available(ast_annot))
                }
            }
        };
        match init {
            ast::class::property::Value::Declared => {
                let (annot_t, ast_annot) =
                    unconditionally_required_annot(cx, tparams_map, &reason, annot);
                Ok((
                    class_types::Field::Annot(annot_t.dupe()),
                    annot_t,
                    ast_annot,
                    Box::new(|| ast::class::property::Value::Declared),
                ))
            }
            ast::class::property::Value::Uninitialized => {
                let (annot_t, ast_annot) =
                    unconditionally_required_annot(cx, tparams_map, &reason, annot);
                Ok((
                    class_types::Field::Annot(annot_t.dupe()),
                    annot_t,
                    ast_annot,
                    Box::new(|| ast::class::property::Value::Uninitialized),
                ))
            }
            ast::class::property::Value::Initialized(expr) => {
                // TODO(pvekris) expr could be an `as const`
                let (annot_t, annot_ast) = match (expr.deref(), annot) {
                    (
                        expression::ExpressionInner::StringLiteral { loc, inner },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let t = string_literal_inner(
                            cx,
                            &natural_inference::empty_syntactic_flags(),
                            loc.dupe(),
                            inner,
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (
                        expression::ExpressionInner::BooleanLiteral { loc, inner },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let t = boolean_literal_inner(
                            cx,
                            &natural_inference::empty_syntactic_flags(),
                            loc.dupe(),
                            inner,
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (
                        expression::ExpressionInner::NumberLiteral { loc, inner },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let t = number_literal_inner(
                            cx,
                            &natural_inference::empty_syntactic_flags(),
                            loc.dupe(),
                            inner,
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (
                        expression::ExpressionInner::BigIntLiteral { loc, inner },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let t = bigint_literal_inner(
                            cx,
                            &natural_inference::empty_syntactic_flags(),
                            loc.dupe(),
                            inner,
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (
                        expression::ExpressionInner::RegExpLiteral { loc, .. },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let t = regexp_literal(cx, loc.dupe());
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (
                        expression::ExpressionInner::ArrowFunction {
                            loc,
                            inner: function_,
                        },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    )
                    | (
                        expression::ExpressionInner::Function {
                            loc,
                            inner: function_,
                        },
                        ast::types::AnnotationOrHint::Missing(annot_loc),
                    ) => {
                        let sig_loc = function_.sig_loc.dupe();
                        let cache = cx.node_cache();
                        let (this_t, arrow, function_loc_opt) = match expr.deref() {
                            expression::ExpressionInner::ArrowFunction { .. } => {
                                let r_loc = reason.loc().dupe();
                                (type_::dummy_this(r_loc), true, None)
                            }
                            _ => {
                                let this_t = flow_js_utils::default_this_type(cx, true, function_)
                                    .expect("Should not be under speculation");
                                (this_t, false, Some(loc.dupe()))
                            }
                        };
                        let (func_sig_data, reconstruct_data, deferred_tg_check) = mk_func_sig(
                            cx,
                            true,
                            false,
                            false,
                            &BTreeMap::new(),
                            tparams_map,
                            reason.dupe(),
                            function_,
                        )?;
                        let func_sig = func_sig_data.clone();
                        if matches!(
                            &*cx.typing_mode(),
                            flow_typing_context::TypingMode::CheckingMode
                        ) {
                            cache.set_function_sig(
                                sig_loc,
                                (func_sig_data, reconstruct_data, deferred_tg_check),
                            );
                        }
                        let t = crate::func_sig::functiontype(
                            cx,
                            arrow,
                            function_loc_opt,
                            this_t,
                            &func_sig,
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (_, ast::types::AnnotationOrHint::Missing(annot_loc)) => {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EMissingLocalAnnotation {
                                reason: reason.dupe().reposition(annot_loc.dupe()),
                                hint_available: false,
                                from_generic_function: false,
                            },
                        );
                        let t = any_t::make(
                            type_::AnySource::AnyError(Some(
                                type_::AnyErrorKind::MissingAnnotation,
                            )),
                            reason.dupe(),
                        );
                        (
                            t.dupe(),
                            ast::types::AnnotationOrHint::Missing((annot_loc.dupe(), t)),
                        )
                    }
                    (_, ast::types::AnnotationOrHint::Available(annot)) => {
                        let (t, ast_annot) = type_annotation::mk_type_available_annotation(
                            cx,
                            tparams_map
                                .iter()
                                .map(|(k, v)| (k.dupe(), v.dupe()))
                                .collect(),
                            annot,
                        );
                        (t, ast::types::AnnotationOrHint::Available(ast_annot))
                    }
                };
                let value_ref: Rc<RefCell<Option<expression::Expression<ALoc, (ALoc, Type)>>>> =
                    Rc::new(RefCell::new(None));
                let (annot_loc, annot_or_inferred) = match annot {
                    ast::types::AnnotationOrHint::Missing(loc) => {
                        (loc.dupe(), AnnotatedOrInferred::Inferred(annot_t.dupe()))
                    }
                    ast::types::AnnotationOrHint::Available(annot) => (
                        annot.loc.dupe(),
                        AnnotatedOrInferred::Annotated(annot_t.dupe()),
                    ),
                };
                let field_init_sig = crate::func_sig::field_initializer(
                    reason.dupe(),
                    expr.dupe(),
                    annot_loc,
                    annot_or_inferred,
                );
                let value_ref_c = value_ref.dupe();
                let set_asts: class_types::SetAsts<StmtConfigTypes> = Rc::new(move |args| {
                    let (_, _, value_opt) = args;
                    *value_ref_c.borrow_mut() = Some(value_opt.unwrap());
                });
                let value_ref_c2 = value_ref.dupe();
                let expr_c = expr.clone();
                Ok((
                    class_types::Field::Infer(field_init_sig, set_asts),
                    annot_t,
                    annot_ast,
                    Box::new(move || {
                        let val = value_ref_c2.borrow().clone();
                        ast::class::property::Value::Initialized(val.unwrap_or_else(|| {
                            let Ok(v) = polymorphic_ast_mapper::expression(
                                &mut typed_ast_utils::ErrorMapper,
                                &expr_c,
                            );
                            v
                        }))
                    }),
                ))
            }
        }
    };

    fn mk_method<'a>(
        cx: &Context<'a>,
        constructor: bool,
        getset: bool,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        reason: Reason,
        func: &ast::function::Function<ALoc, ALoc>,
    ) -> Result<
        (
            func_class_sig_types::func::Func<func_class_sig_types::StmtConfigTypes>,
            func_class_sig_types::func::Reconstruct,
            DeferredTypeGuardCheck<'a>,
        ),
        AbnormalControlFlow,
    > {
        mk_func_sig(
            cx,
            !constructor,
            constructor,
            getset,
            &BTreeMap::new(),
            tparams_map,
            reason,
            func,
        )
    }

    let mk_extends = move |cx: &Context<'a>,
                           tparams_map: &FlowOrdMap<SubstName, Type>,
                           extends: &Option<ast::class::Extends<ALoc, ALoc>>|
          -> (
        class_types::Extends,
        Box<
            dyn FnOnce(
                    &Context<'a>,
                ) -> Result<
                    Option<ast::class::Extends<ALoc, (ALoc, Type)>>,
                    AbnormalControlFlow,
                > + 'a,
        >,
    ) {
        match extends {
            None => (
                class_types::Extends::Implicit { null: false },
                Box::new(|_cx| Ok(None)),
            ),
            Some(ext) => {
                let loc = ext.loc.dupe();
                let expr = &ext.expr;
                let targs = &ext.targs;
                let comments = ext.comments.dupe();

                use ast::expression::ExpressionInner;
                fn super_expr<'a>(
                    cx: &Context<'a>,
                    expr: &expression::Expression<ALoc, ALoc>,
                ) -> (
                    Type,
                    Box<
                        dyn FnOnce(
                                &Context<'a>,
                            ) -> Result<
                                expression::Expression<ALoc, (ALoc, Type)>,
                                AbnormalControlFlow,
                            > + 'a,
                    >,
                ) {
                    let loc = expr.loc().dupe();
                    match expr.deref() {
                        ExpressionInner::Identifier { inner, .. } => {
                            let id_loc = inner.loc.dupe();
                            let id = inner.clone();
                            let t = type_env::sig_var_ref(
                                None,
                                cx,
                                None,
                                Name::new(inner.name.dupe()),
                                id_loc.dupe(),
                            );
                            let t_c = t.dupe();
                            (
                                t,
                                Box::new(move |_cx| {
                                    Ok(expression::Expression::new(ExpressionInner::Identifier {
                                        loc: (loc, t_c.dupe()),
                                        inner: ast::Identifier::new(ast::IdentifierInner {
                                            loc: (id_loc, t_c),
                                            name: id.name.dupe(),
                                            comments: id.comments.dupe(),
                                        }),
                                    }))
                                }),
                            )
                        }
                        ExpressionInner::Member { inner, .. }
                            if let expression::member::Property::PropertyIdentifier(ref pid) =
                                inner.property =>
                        {
                            let member = inner.as_ref();
                            let member_comments = member.comments.dupe();
                            let ploc = pid.loc.dupe();
                            let id = pid.clone();
                            let (t, _object_f) = super_expr(cx, &member.object);
                            let expr_reason = mk_expression_reason(expr);
                            let prop_name = Name::new(pid.name.dupe());
                            let prop_reason = mk_reason(
                                VirtualReasonDesc::RProperty(Some(prop_name.dupe())),
                                ploc.dupe(),
                            );
                            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(
                                expr_reason.dupe(),
                            )));
                            let tout = crate::type_annotation_cons_gen::get_prop(
                                cx,
                                use_op,
                                prop_reason,
                                Some(expr_reason),
                                prop_name,
                                t,
                            );
                            let tout_c = tout.dupe();
                            (
                                tout,
                                Box::new(move |cx| {
                                    let property = expression::member::Property::PropertyIdentifier(
                                        ast::Identifier::new(ast::IdentifierInner {
                                            loc: (ploc, tout_c.dupe()),
                                            name: id.name.dupe(),
                                            comments: id.comments.dupe(),
                                        }),
                                    );
                                    Ok(expression::Expression::new(ExpressionInner::Member {
                                        loc: (loc, tout_c),
                                        inner: expression::Member {
                                            object: _object_f(cx)?,
                                            property,
                                            comments: member_comments,
                                        }
                                        .into(),
                                    }))
                                }),
                            )
                        }
                        ExpressionInner::AsExpression { inner, .. } => {
                            let as_expr = inner.as_ref();
                            let as_comments = as_expr.comments.dupe();
                            match cx.casting_syntax() {
                                flow_common::options::CastingSyntax::As
                                | flow_common::options::CastingSyntax::Both => {
                                    let (t, annot_ast) =
                                        type_annotation::mk_type_available_annotation(
                                            cx,
                                            FlowOrdMap::new(),
                                            &as_expr.annot,
                                        );
                                    let t_c = t.dupe();
                                    let sub_expr_c = as_expr.expression.clone();
                                    (
                                        t,
                                        Box::new(move |cx| {
                                            let e_prime =
                                                expression(None, None, None, cx, &sub_expr_c)?;
                                            let infer_t = e_prime.loc().1.dupe();
                                            let use_op =
                                                UseOp::Op(Arc::new(type_::RootUseOp::Cast {
                                                    lower: mk_expression_reason(&sub_expr_c),
                                                    upper: reason_of_t(&t_c).dupe(),
                                                }));
                                            tvar_resolver::resolve(
                                                cx,
                                                tvar_resolver::default_no_lowers,
                                                true,
                                                &infer_t,
                                            );
                                            type_operation_utils::perform_type_cast(
                                                cx, use_op, &infer_t, &t_c,
                                            )
                                            .unwrap();
                                            Ok(expression::Expression::new(
                                                ExpressionInner::AsExpression {
                                                    loc: (loc, t_c),
                                                    inner: expression::AsExpression {
                                                        expression: e_prime,
                                                        annot: annot_ast,
                                                        comments: as_comments,
                                                    }
                                                    .into(),
                                                },
                                            ))
                                        }),
                                    )
                                }
                            }
                        }
                        ExpressionInner::TypeCast { inner, .. } => {
                            let cast = inner.as_ref();
                            let tc_comments = cast.comments.dupe();
                            match cx.casting_syntax() {
                                flow_common::options::CastingSyntax::Both => {
                                    let (t, annot_ast) =
                                        type_annotation::mk_type_available_annotation(
                                            cx,
                                            FlowOrdMap::new(),
                                            &cast.annot,
                                        );
                                    let t_c = t.dupe();
                                    let sub_expr_c = cast.expression.clone();
                                    (
                                        t,
                                        Box::new(move |cx| {
                                            let e_prime =
                                                expression(None, None, None, cx, &sub_expr_c)?;
                                            let infer_t = e_prime.loc().1.dupe();
                                            let use_op =
                                                UseOp::Op(Arc::new(type_::RootUseOp::Cast {
                                                    lower: mk_expression_reason(&sub_expr_c),
                                                    upper: reason_of_t(&t_c).dupe(),
                                                }));
                                            tvar_resolver::resolve(
                                                cx,
                                                tvar_resolver::default_no_lowers,
                                                true,
                                                &infer_t,
                                            );
                                            type_operation_utils::perform_type_cast(
                                                cx, use_op, &infer_t, &t_c,
                                            )
                                            .unwrap();
                                            Ok(expression::Expression::new(
                                                ExpressionInner::TypeCast {
                                                    loc: (loc, t_c),
                                                    inner: expression::TypeCast {
                                                        expression: e_prime,
                                                        annot: annot_ast,
                                                        comments: tc_comments,
                                                    }
                                                    .into(),
                                                },
                                            ))
                                        }),
                                    )
                                }
                                flow_common::options::CastingSyntax::As => {
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EInvalidTypeCastSyntax {
                                            loc: loc.dupe(),
                                            enabled_casting_syntax: cx.casting_syntax(),
                                        },
                                    );
                                    let t = any_t::at(type_::AnySource::AnyError(None), loc.dupe());
                                    let t_c = t.dupe();
                                    let cast_c = cast.clone();
                                    (
                                        t,
                                        Box::new(move |_cx| {
                                            let Ok(mapped_cast) = polymorphic_ast_mapper::type_cast(
                                                &mut typed_ast_utils::ErrorMapper,
                                                &cast_c,
                                            );
                                            Ok(expression::Expression::new(
                                                ExpressionInner::TypeCast {
                                                    loc: (loc, t_c),
                                                    inner: mapped_cast.into(),
                                                },
                                            ))
                                        }),
                                    )
                                }
                            }
                        }
                        _ => {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInvalidExtends(mk_expression_reason(expr)),
                            );
                            let t = any_t::at(type_::AnySource::AnyError(None), loc.dupe());
                            let expr_c = expr.clone();
                            (
                                t,
                                Box::new(move |cx| expression(None, None, None, cx, &expr_c)),
                            )
                        }
                    }
                }
                let (c, expr_f) = super_expr(cx, expr);
                let (t, targs_ast) = type_annotation::mk_super(
                    cx,
                    tparams_map
                        .iter()
                        .map(|(k, v)| (k.dupe(), v.dupe()))
                        .collect(),
                    loc.dupe(),
                    c,
                    targs.as_ref(),
                );
                (
                    class_types::Extends::Explicit(t),
                    Box::new(move |cx| {
                        Ok(Some(ast::class::Extends {
                            loc,
                            expr: expr_f(cx)?,
                            targs: targs_ast,
                            comments,
                        }))
                    }),
                )
            }
        }
    };

    let mk_class_sig_with_self = move |cx: &Context<'a>,
                                       name_loc: ALoc,
                                       class_loc: ALoc,
                                       inst_kind: &InstanceKind,
                                       reason: Reason,
                                       self_: Type,
                                       cls: &ast::class::Class<ALoc, ALoc>|
          -> SigResult<'a> {
        let node_cache = cx.node_cache();
        match node_cache.get_class_sig(&class_loc) {
            Some((class_t, class_t_internal, class_sig, class_ast_f)) => {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![format!(
                        "Class sig cache hit at {}",
                        reason.loc().debug_to_string(false)
                    )]
                });
                Ok((
                    class_t,
                    class_t_internal,
                    class_sig,
                    Rc::new(move |cx, t: Type| class_ast_f(cx, t)),
                ))
            }
            None => {
                let id = &cls.id;
                let body = &cls.body;
                let body_loc = &body.loc;
                let elements = &body.body;
                let body_comments = &body.comments;
                let tparams = &cls.tparams;
                let extends = &cls.extends;
                let implements = &cls.implements;
                let class_decorators = &cls.class_decorators;
                let comments = &cls.comments;
                let abstract_ = cls.abstract_;

                let class_decorators_ast: Vec<_> = class_decorators
                    .iter()
                    .map(|d| {
                        let Ok(v) = polymorphic_ast_mapper::class_decorator(
                            &mut typed_ast_utils::ErrorMapper,
                            d,
                        );
                        v
                    })
                    .collect();
                let (tparams_val, tparams_map, tparams_ast) =
                    type_annotation::mk_type_param_declarations(
                        cx,
                        flow_parser::ast_visitor::TypeParamsContext::Class,
                        None,
                        tparams.as_ref(),
                    );
                let (this_tparam, this_t) = class_sig::mk_this(self_, cx, reason.dupe());
                let mut tparams_map_with_this = tparams_map.dupe();
                tparams_map_with_this
                    .insert(SubstName::name(FlowSmolStr::new("this")), this_t.dupe());
                let class_name: Option<FlowSmolStr> = id.as_ref().map(|ident| ident.name.dupe());
                let (mut class_sig, extends_ast_f, implements_ast) = {
                    let id_val = cx.make_aloc_id(&name_loc);
                    let (extends_val, extends_ast_f) = mk_extends(cx, &tparams_map, extends);
                    let (implements_val, implements_ast) = match implements {
                        None => (vec![], None),
                        Some(impl_) => {
                            let implements_loc = &impl_.loc;
                            let interfaces = &impl_.interfaces;
                            let impl_comments = &impl_.comments;
                            let (implements_list, interfaces_ast): (Vec<_>, Vec<_>) = interfaces
                                .iter()
                                .map(|i| {
                                    let loc = &i.loc;
                                    let id = &i.id;
                                    let targs = &i.targs;
                                    match id {
                                        ast::types::generic::Identifier::Qualified(_)
                                        | ast::types::generic::Identifier::ImportTypeAnnot(_)
                                            if !cx.tslib_syntax() =>
                                        {
                                            flow_js::add_output_non_speculating(
                                                cx,
                                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                                    loc.dupe(),
                                                    UnsupportedSyntax::TSLibSyntax(
                                                        TsLibSyntaxKind::ImplementsDottedPath,
                                                    ),
                                                ))),
                                            );
                                        }
                                        _ => {}
                                    }
                                    let (c, id) = type_annotation::convert_qualification(
                                        cx,
                                        "implements",
                                        id,
                                    );
                                    let (typeapp, targs_ast) = match targs {
                                        None => ((loc.dupe(), c.dupe(), None), None),
                                        Some(type_args) => {
                                            let targs_loc = &type_args.loc;
                                            let (ts, targs_ast_inner) =
                                                type_annotation::convert_list(
                                                    cx,
                                                    tparams_map.clone(),
                                                    &type_args.arguments,
                                                );
                                            (
                                                (loc.dupe(), c.dupe(), Some(ts)),
                                                Some(ast::types::TypeArgs::<ALoc, (ALoc, Type)> {
                                                    loc: targs_loc.dupe(),
                                                    arguments: targs_ast_inner.into(),
                                                    comments: type_args.comments.dupe(),
                                                }),
                                            )
                                        }
                                    };
                                    (
                                        typeapp,
                                        ast::class::implements::Interface {
                                            loc: loc.dupe(),
                                            id,
                                            targs: targs_ast,
                                        },
                                    )
                                })
                                .unzip();
                            (
                                implements_list,
                                Some(ast::class::Implements {
                                    loc: implements_loc.dupe(),
                                    interfaces: interfaces_ast.into(),
                                    comments: impl_comments.clone(),
                                }),
                            )
                        }
                    };
                    let super_ = class_types::Super::Class(class_types::ClassSuper {
                        extends: extends_val,
                        mixins: vec![],
                        implements: implements_val,
                        this_t: this_t.dupe(),
                        this_tparam,
                    });
                    (
                        class_sig::empty(
                            id_val,
                            class_name.dupe(),
                            class_loc.dupe(),
                            reason.dupe(),
                            tparams_val,
                            tparams_map
                                .iter()
                                .map(|(k, v)| (k.dupe(), v.dupe()))
                                .collect(),
                            super_,
                        ),
                        extends_ast_f,
                        implements_ast,
                    )
                };

                let mk_record_constructor =
                    |cx,
                     tparams_map_with_this: &FlowOrdMap<SubstName, Type>,
                     elements: &[ast::class::BodyElement<ALoc, ALoc>],
                     defaulted_props: &flow_data_structure_wrapper::ord_set::FlowOrdSet<
                        FlowSmolStr,
                    >,
                     record_name: &FlowSmolStr,
                     name_loc: ALoc|
                     -> func_class_sig_types::func::Func<
                        func_class_sig_types::StmtConfigTypes,
                    > {
                        // Build an object type with properties for each instance field.
                        let props: properties::PropertiesMap = elements
                            .iter()
                            .filter_map(|elem| {
                                use ast::class::BodyElement;
                                use ast::class::Property;
                                match elem {
                                    BodyElement::Property(Property {
                                        key: expression::object::Key::Identifier(id_pair),
                                        annot,
                                        static_: false,
                                        ..
                                    }) => {
                                        let id_loc = &id_pair.loc;
                                        let name = &id_pair.name;
                                        let field_t = match annot {
                                            ast::types::AnnotationOrHint::Available(annot) => {
                                                let (t, _) =
                                                    type_annotation::mk_type_available_annotation(
                                                        cx,
                                                        tparams_map_with_this
                                                            .iter()
                                                            .map(|(k, v)| (k.dupe(), v.dupe()))
                                                            .collect(),
                                                        annot,
                                                    );
                                                t
                                            }
                                            ast::types::AnnotationOrHint::Missing(loc) => {
                                                any_t::at(
                                                    type_::AnySource::AnyError(None),
                                                    loc.dupe(),
                                                )
                                            }
                                        };
                                        // Create an optional property if it has an initializer (i.e. has a default value).
                                        let prop_t = if defaulted_props.contains(name) {
                                            let field_reason = mk_reason(
                                                VirtualReasonDesc::RProperty(
                                                    Some(Name::new(name.dupe())),
                                                ),
                                                id_loc.dupe(),
                                            );
                                            Type::new(TypeInner::OptionalT {
                                                reason: mk_reason(
                                                    VirtualReasonDesc::ROptional(
                                                        Arc::new(
                                                            field_reason.desc(true).clone(),
                                                        ),
                                                    ),
                                                    id_loc.dupe(),
                                                ),
                                                type_: field_t,
                                                use_desc: false,
                                            })
                                        } else {
                                            field_t
                                        };
                                        Some((
                                            Name::new(name.dupe()),
                                            type_::Property::new(type_::PropertyInner::Field(Box::new(FieldData {
                                                preferred_def_locs: None,
                                                key_loc: Some(id_loc.dupe()),
                                                type_: prop_t,
                                                polarity: Polarity::Positive,
                                            }))),
                                        ))
                                    }
                                    _ => None,
                                }
                            })
                            .collect();
                        let record_reason = mk_reason(
                            VirtualReasonDesc::RRecordType(
                                record_name.dupe(),
                            ),
                            name_loc.dupe(),
                        );
                        let null_proto = type_::null_proto::make(record_reason.dupe());
                        let obj_t = obj_type::mk_with_proto(
                            cx,
                            record_reason.dupe(),
                            type_::ObjKind::Exact,
                            None,
                            None,
                            Some(props),
                            None,
                            null_proto,
                        );
                        let param = flow_typing_loc_env::func_stmt_config_types::Param {
                            t: obj_t.dupe(),
                            loc: name_loc.dupe(),
                            ploc: name_loc.dupe(),
                            pattern: flow_typing_loc_env::func_stmt_config_types::Pattern::Object {
                                annot: ast::types::AnnotationOrHint::Missing((
                                    name_loc.dupe(),
                                    obj_t,
                                )),
                                properties: vec![],
                                optional: false,
                                comments: None,
                            },
                            default: None,
                            has_anno: false,
                        };
                        func_class_sig_types::func::Func {
                            reason: mk_reason(
                                VirtualReasonDesc::RConstructor,
                                name_loc.dupe(),
                            ),
                            kind: func_class_sig_types::func::Kind::Ctor,
                            tparams: None,
                            fparams: func_class_sig_types::param::Param {
                                params: vec![param],
                                rest: None,
                                this_: None,
                                reconstruct: Rc::new(|_, _, _| None),
                            },
                            body: None,
                            return_t: AnnotatedOrInferred::Inferred(type_::void::at(
                                name_loc.dupe(),
                            )),
                            effect_: type_::ReactEffectType::ArbitraryEffect,
                            ret_annot_loc: name_loc,
                            statics: None,
                        }
                    };

                // In case there is no constructor, pick up a default one.
                if extends.is_none() {
                    match inst_kind {
                        InstanceKind::RecordKind { defaulted_props } => {
                            let record_name = class_name
                                .as_ref()
                                .expect("RecordKind should have class_name");
                            let func_sig = mk_record_constructor(
                                cx,
                                &tparams_map_with_this,
                                elements,
                                defaulted_props,
                                record_name,
                                name_loc.dupe(),
                            );
                            class_sig::add_constructor(None, func_sig, None, None, &mut class_sig);
                        }
                        _ => {
                            let default_reason = reason
                                .dupe()
                                .replace_desc(VirtualReasonDesc::RDefaultConstructor);
                            class_sig::add_default_constructor(default_reason, &mut class_sig);
                        }
                    }
                }

                // All classes have a static "name" property.
                class_sig::add_name_field(&mut class_sig);

                // class_kind derived from inst_kind
                let class_kind = match inst_kind {
                    InstanceKind::ClassKind | InstanceKind::InterfaceKind { .. } => {
                        intermediate_error_types::ClassKind::Class
                    }
                    InstanceKind::RecordKind { .. } => intermediate_error_types::ClassKind::Record,
                };

                // let check_duplicate_name public_seen_names member_loc name ~static ~private_ kind =
                //   check_duplicate_class_member cx public_seen_names member_loc name ~static ~private_ kind class_kind
                // in
                let check_duplicate_name =
                    |public_seen_names: &mut SeenNames,
                     member_loc: ALoc,
                     name: &FlowSmolStr,
                     static_: bool,
                     private_: bool,
                     kind: ClassMemberKind| {
                        check_duplicate_class_member(
                            cx,
                            public_seen_names,
                            static_,
                            private_,
                            name,
                            kind,
                            member_loc,
                            class_kind,
                        );
                    };

                let check_ts_accessibility = |cx: &Context<'a>,
                                              ts_accessibility: &Option<
                    ast::class::ts_accessibility::TSAccessibility<ALoc>,
                >| {
                    if !cx.ts_syntax() {
                        if let Some(ts_acc) = ts_accessibility {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                                    kind: TSSyntaxKind::TSClassAccessibility(ts_acc.kind),
                                    loc: ts_acc.loc.dupe(),
                                })),
                            );
                        }
                    }
                };

                if abstract_ {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                            kind: TSSyntaxKind::AbstractClass,
                            loc: class_loc.dupe(),
                        })),
                    );
                }

                // NOTE: We used to mine field declarations from field assignments in a
                // constructor as a convenience, but it was not worth it: often, all that did
                // was exchange a complaint about a missing field for a complaint about a
                // missing annotation. Moreover, it caused fields declared in the super class
                // to be redeclared if they were assigned in the constructor. So we don't do
                // it. In the future, we could do it again, but only for private fields.

                // NOTE: field initializer expressions and method bodies don't get checked
                // until Class_sig.toplevels is called on class_sig. For this reason rather
                // than returning a typed AST, we'll return a function which returns a typed
                // AST, and this function shouldn't be called until after Class_sig.toplevels
                // has been called.

                // If a field/method ever gets shadowed later in the class, then its
                // initializer/body (respectively) will not get checked, and the corresponding
                // nodes of the typed AST will be filled in with error nodes.
                let mut public_seen_names = empty_seen_names();
                let mut rev_elements: Vec<
                    Box<
                        dyn FnOnce(&Context<'a>) -> ast::class::BodyElement<ALoc, (ALoc, Type)>
                            + 'a,
                    >,
                > = Vec::new();

                #[allow(clippy::too_many_arguments)]
                fn add_method_sig_and_element<'a>(
                    cx: &Context<'a>,
                    tparams_map_with_this: &FlowOrdMap<SubstName, Type>,
                    c: &mut class_types::Class<StmtConfigTypes>,
                    rev_elements: &mut Vec<
                        Box<
                            dyn FnOnce(&Context<'a>) -> ast::class::BodyElement<ALoc, (ALoc, Type)>
                                + 'a,
                        >,
                    >,
                    public_seen_names: &mut SeenNames,
                    class_kind: intermediate_error_types::ClassKind,
                    method_loc: ALoc,
                    name: FlowSmolStr,
                    id_loc: ALoc,
                    func_loc: ALoc,
                    func: &ast::function::Function<ALoc, ALoc>,
                    kind: ast::class::MethodKind,
                    private_: bool,
                    static_: bool,
                    override_: bool,
                    ts_accessibility: Option<ast::class::ts_accessibility::TSAccessibility<ALoc>>,
                    decorators: &std::sync::Arc<[ast::class::Decorator<ALoc, ALoc>]>,
                    comments: Option<ast::Syntax<ALoc, ()>>,
                    get_typed_method_key: Box<
                        dyn FnOnce(
                                &Context<'a>,
                                Type,
                            )
                                -> ast::expression::object::Key<ALoc, (ALoc, Type)>
                            + 'a,
                    >,
                ) -> Result<(), AbnormalControlFlow> {
                    use std::cell::RefCell;
                    use std::rc::Rc;

                    use flow_typing_loc_env::func_class_sig_types::StmtConfigTypes;
                    use flow_typing_loc_env::func_class_sig_types::class as class_types;

                    let decorators_ast: Vec<_> = decorators
                        .iter()
                        .map(|d| {
                            let Ok(v) = polymorphic_ast_mapper::class_decorator(
                                &mut typed_ast_utils::ErrorMapper,
                                d,
                            );
                            v
                        })
                        .collect();
                    match kind {
                        ast::class::MethodKind::Get | ast::class::MethodKind::Set => {
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsafeGettersSetters(method_loc.dupe()),
                            );
                        }
                        _ => {}
                    }
                    let method_reason = func_reason(func.async_, func.generator, method_loc.dupe());
                    let (method_sig, reconstruct_func, deferred_tg_check) = mk_method(
                        cx,
                        kind == ast::class::MethodKind::Constructor,
                        kind == ast::class::MethodKind::Get || kind == ast::class::MethodKind::Set,
                        tparams_map_with_this,
                        method_reason,
                        func,
                    )?;
                    // The body of a class method doesn't get checked until Class_sig.toplevels
                    // is called on the class sig (in this case c). The order of how the methods
                    // were arranged in the class is lost by the time this happens, so rather
                    // than attempting to return a list of method bodies from the Class_sig.toplevels
                    // function, we have it place the function bodies into a list via side effects.
                    // We use a similar approach for method types
                    let params_ref = Rc::new(RefCell::new(None));
                    let body_ref = Rc::new(RefCell::new(None));
                    let params_ref_c = params_ref.dupe();
                    let body_ref_c = body_ref.dupe();
                    let set_asts: class_types::SetAsts<StmtConfigTypes> = Rc::new(move |args| {
                        let (params_opt, body_opt, _) = args;
                        *params_ref_c.borrow_mut() = Some(params_opt.unwrap());
                        *body_ref_c.borrow_mut() = Some(body_opt.unwrap());
                    });
                    let func_t_ref = Rc::new(RefCell::new(None));
                    let func_t_ref_c = func_t_ref.dupe();
                    let set_type: class_types::SetType = Rc::new(move |t| {
                        *func_t_ref_c.borrow_mut() = Some(t);
                    });
                    let params_ref_c2 = params_ref.dupe();
                    let body_ref_c2 = body_ref.dupe();
                    let func_t_ref_c2 = func_t_ref.dupe();
                    let method_loc_c = method_loc.dupe();
                    let func_loc_c = func_loc.dupe();
                    let id_loc_c = id_loc.dupe();
                    let kind_c = kind;
                    let static_c = static_;
                    let override_c = override_;
                    let ts_accessibility_c = ts_accessibility.clone();
                    let decorators_ast_c = decorators_ast.clone();
                    let comments_c = comments.clone();
                    let func_params_c = func.params.clone();
                    let func_body_c = func.body.clone();
                    let reconstruct_func_c = reconstruct_func.dupe();
                    let get_element = Box::new(move |cx: &Context<'a>| {
                        // Invoke deferred type guard check (must run after toplevels)
                        if let Some(check) = deferred_tg_check {
                            check(cx);
                        }
                        let params = params_ref_c2.borrow().clone().unwrap_or_else(|| {
                            let Ok(v) = polymorphic_ast_mapper::function_params(
                                &mut typed_ast_utils::ErrorMapper,
                                &func_params_c,
                            );
                            v
                        });
                        let body = body_ref_c2.borrow().clone().unwrap_or_else(|| {
                            let Ok(v) = polymorphic_ast_mapper::function_body_any(
                                &mut typed_ast_utils::ErrorMapper,
                                &func_body_c,
                            );
                            v
                        });
                        let func_t = func_t_ref_c2
                            .borrow()
                            .clone()
                            .unwrap_or_else(|| empty_t::at(id_loc_c.dupe()));
                        let typed_func = reconstruct_func_c(params, body, func_t.dupe());
                        ast::class::BodyElement::Method(ast::class::Method {
                            loc: (method_loc_c, func_t.dupe()),
                            key: get_typed_method_key(cx, func_t),
                            value: (func_loc_c, typed_func),
                            kind: kind_c,
                            static_: static_c,
                            override_: override_c,
                            ts_accessibility: ts_accessibility_c,
                            decorators: decorators_ast_c.into(),
                            comments: comments_c,
                        })
                    });
                    match kind {
                        ast::class::MethodKind::Constructor => {
                            class_sig::add_constructor(
                                Some(id_loc.dupe()),
                                method_sig,
                                Some(set_asts),
                                Some(set_type),
                                c,
                            );
                        }
                        ast::class::MethodKind::Method => {
                            if private_ {
                                class_sig::add_private_method(
                                    static_,
                                    name.dupe(),
                                    id_loc.dupe(),
                                    Some(func_loc.dupe()),
                                    method_sig,
                                    set_asts,
                                    set_type,
                                    c,
                                );
                            } else {
                                class_sig::add_method(
                                    static_,
                                    name.dupe(),
                                    id_loc.dupe(),
                                    Some(func_loc.dupe()),
                                    method_sig,
                                    Some(set_asts),
                                    Some(set_type),
                                    c,
                                );
                            }
                            check_duplicate_class_member(
                                cx,
                                public_seen_names,
                                static_,
                                private_,
                                &name,
                                ClassMemberKind::ClassMemberMethod,
                                id_loc.dupe(),
                                class_kind,
                            );
                        }
                        ast::class::MethodKind::Get => {
                            class_sig::add_getter(
                                static_,
                                name.dupe(),
                                id_loc.dupe(),
                                Some(func_loc.dupe()),
                                method_sig,
                                Some(set_asts),
                                Some(set_type),
                                c,
                            );
                            check_duplicate_class_member(
                                cx,
                                public_seen_names,
                                static_,
                                private_,
                                &name,
                                ClassMemberKind::ClassMemberGetter,
                                id_loc.dupe(),
                                class_kind,
                            );
                        }
                        ast::class::MethodKind::Set => {
                            class_sig::add_setter(
                                static_,
                                name.dupe(),
                                id_loc.dupe(),
                                Some(func_loc.dupe()),
                                method_sig,
                                Some(set_asts),
                                Some(set_type),
                                c,
                            );
                            check_duplicate_class_member(
                                cx,
                                public_seen_names,
                                static_,
                                private_,
                                &name,
                                ClassMemberKind::ClassMemberSetter,
                                id_loc.dupe(),
                                class_kind,
                            );
                        }
                    }
                    rev_elements.push(get_element);
                    Ok(())
                }

                for elem in elements.iter() {
                    use ast::class::BodyElement;
                    use ast::class::PrivateField;
                    use ast::class::Property;
                    use ast::expression::object::Key;
                    match elem {
                        BodyElement::Method(method) => {
                            match &method.key {
                                Key::PrivateName(private_name) => {
                                    let method_loc = &method.loc;
                                    let id_loc = &private_name.loc;
                                    let name = &private_name.name;
                                    let (func_loc, func) = &method.value;
                                    let kind = method.kind;
                                    let static_ = method.static_;
                                    let override_ = method.override_;
                                    let ts_accessibility = &method.ts_accessibility;
                                    let decorators = &method.decorators;
                                    let method_comments = &method.comments;

                                    check_ts_accessibility(cx, ts_accessibility);
                                    if override_ {
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                                method_loc.dupe(),
                                                UnsupportedSyntax::TSLibSyntax(
                                                    TsLibSyntaxKind::OverrideModifier,
                                                ),
                                            ))),
                                        );
                                    }
                                    let id_loc_c = id_loc.dupe();
                                    let pn_c = private_name.clone();
                                    add_method_sig_and_element(
                                        cx,
                                        &tparams_map_with_this,
                                        &mut class_sig,
                                        &mut rev_elements,
                                        &mut public_seen_names,
                                        class_kind,
                                        method_loc.dupe(),
                                        name.dupe(),
                                        id_loc.dupe(),
                                        func_loc.dupe(),
                                        func,
                                        kind,
                                        true,
                                        static_,
                                        override_,
                                        ts_accessibility.clone(),
                                        decorators,
                                        method_comments.clone(),
                                        Box::new(move |_cx, _func_t| {
                                            expression::object::Key::PrivateName(ast::PrivateName {
                                                loc: id_loc_c,
                                                name: pn_c.name.dupe(),
                                                comments: pn_c.comments.dupe(),
                                            })
                                        }),
                                    )?;
                                }
                                Key::Identifier(id_pair) => {
                                    let method_loc = &method.loc;
                                    let id_loc = &id_pair.loc;
                                    let name = &id_pair.name;
                                    let (func_loc, func) = &method.value;
                                    let kind = method.kind;
                                    let static_ = method.static_;
                                    let override_ = method.override_;
                                    let ts_accessibility = &method.ts_accessibility;
                                    let decorators = &method.decorators;
                                    let method_comments = &method.comments;

                                    check_ts_accessibility(cx, ts_accessibility);
                                    if override_ {
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                                method_loc.dupe(),
                                                UnsupportedSyntax::TSLibSyntax(
                                                    TsLibSyntaxKind::OverrideModifier,
                                                ),
                                            ))),
                                        );
                                    }

                                    let id_loc_c = id_loc.dupe();
                                    let id_pair_c = id_pair.clone();
                                    add_method_sig_and_element(
                                        cx,
                                        &tparams_map_with_this,
                                        &mut class_sig,
                                        &mut rev_elements,
                                        &mut public_seen_names,
                                        class_kind,
                                        method_loc.dupe(),
                                        name.dupe(),
                                        id_loc.dupe(),
                                        func_loc.dupe(),
                                        func,
                                        kind,
                                        false,
                                        static_,
                                        override_,
                                        ts_accessibility.clone(),
                                        decorators,
                                        method_comments.clone(),
                                        Box::new(move |_cx, func_t| {
                                            expression::object::Key::Identifier(
                                                ast::Identifier::new(ast::IdentifierInner {
                                                    loc: (id_loc_c, func_t),
                                                    name: id_pair_c.name.dupe(),
                                                    comments: id_pair_c.comments.dupe(),
                                                }),
                                            )
                                        }),
                                    )?;
                                }
                                Key::StringLiteral(_)
                                | Key::NumberLiteral(_)
                                | Key::BigIntLiteral(_) => {
                                    let loc = &method.loc;
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            loc.dupe(),
                                            UnsupportedSyntax::ClassPropertyLiteral,
                                        ))),
                                    );
                                    let elem_c = elem.clone();
                                    rev_elements.push(Box::new(move |_cx| {
                                        let Ok(v) = polymorphic_ast_mapper::class_element(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &elem_c,
                                        );
                                        v
                                    }));
                                }
                                Key::Computed(computed_key) => {
                                    if let Some(name) =
                                        flow_parser::ast_utils::well_known_symbol_name(
                                            &computed_key.expression,
                                        )
                                    {
                                        let method_loc = &method.loc;
                                        let id_loc = computed_key.expression.loc().dupe();
                                        let (func_loc, func) = &method.value;
                                        let kind = method.kind;
                                        let static_ = method.static_;
                                        let override_ = method.override_;
                                        let ts_accessibility = &method.ts_accessibility;
                                        let decorators = &method.decorators;
                                        let method_comments = &method.comments;
                                        let computed_loc = &computed_key.loc;
                                        let computed_comments = &computed_key.comments;

                                        check_ts_accessibility(cx, ts_accessibility);
                                        if override_ {
                                            flow_js::add_output_non_speculating(
                                                cx,
                                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                                    method_loc.dupe(),
                                                    UnsupportedSyntax::TSLibSyntax(
                                                        TsLibSyntaxKind::OverrideModifier,
                                                    ),
                                                ))),
                                            );
                                        }

                                        let computed_loc_c = computed_loc.dupe();
                                        let computed_expr_c = computed_key.expression.clone();
                                        let computed_comments_c = computed_comments.clone();
                                        add_method_sig_and_element(
                                            cx,
                                            &tparams_map_with_this,
                                            &mut class_sig,
                                            &mut rev_elements,
                                            &mut public_seen_names,
                                            class_kind,
                                            method_loc.dupe(),
                                            FlowSmolStr::from(name),
                                            id_loc.dupe(),
                                            func_loc.dupe(),
                                            func,
                                            kind,
                                            false,
                                            static_,
                                            override_,
                                            ts_accessibility.clone(),
                                            decorators,
                                            method_comments.clone(),
                                            Box::new(move |cx, _func_t| {
                                                let typed_expr = crate::statement::expression(
                                                    None,
                                                    None,
                                                    None,
                                                    cx,
                                                    &computed_expr_c,
                                                )
                                                .unwrap();
                                                expression::object::Key::Computed(
                                                    ast::ComputedKey {
                                                        loc: computed_loc_c,
                                                        expression: typed_expr,
                                                        comments: computed_comments_c,
                                                    },
                                                )
                                            }),
                                        )?;
                                    } else {
                                        // computed LHS
                                        let loc = &method.loc;
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                                loc.dupe(),
                                                UnsupportedSyntax::ClassPropertyComputed,
                                            ))),
                                        );
                                        let elem_c = elem.clone();
                                        rev_elements.push(Box::new(move |_cx| {
                                            let Ok(v) = polymorphic_ast_mapper::class_element(
                                                &mut typed_ast_utils::ErrorMapper,
                                                &elem_c,
                                            );
                                            v
                                        }));
                                    }
                                }
                            }
                        }
                        BodyElement::Property(prop) => {
                            match &prop.key {
                                Key::PrivateName(_) => {
                                    panic!(
                                        "Internal Error: Found non-private field with private name"
                                    );
                                }
                                Key::Identifier(id_pair) => {
                                    let loc = &prop.loc;
                                    let id_loc = &id_pair.loc;
                                    let name = &id_pair.name;
                                    let annot = &prop.annot;
                                    let value = &prop.value;
                                    let static_ = prop.static_;
                                    let override_ = prop.override_;
                                    let optional = prop.optional;
                                    let variance = &prop.variance;
                                    let ts_accessibility = &prop.ts_accessibility;
                                    let decorators = &prop.decorators;
                                    let prop_comments = &prop.comments;

                                    check_ts_accessibility(cx, ts_accessibility);
                                    if override_ {
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                                loc.dupe(),
                                                UnsupportedSyntax::TSLibSyntax(
                                                    TsLibSyntaxKind::OverrideModifier,
                                                ),
                                            ))),
                                        );
                                    }
                                    if optional && !cx.tslib_syntax() {
                                        flow_js::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                                loc.dupe(),
                                                UnsupportedSyntax::TSLibSyntax(
                                                    TsLibSyntaxKind::OptionalClassProperty,
                                                ),
                                            ))),
                                        );
                                    }
                                    let field_reason = mk_reason(
                                        VirtualReasonDesc::RProperty(Some(Name::new(name.dupe()))),
                                        loc.dupe(),
                                    );
                                    let polarity = type_annotation::polarity(cx, variance.as_ref());
                                    //   let decorators = ...
                                    let decorators_ast: Vec<_> = decorators
                                        .iter()
                                        .map(|d| {
                                            let Ok(v) = polymorphic_ast_mapper::class_decorator(
                                                &mut typed_ast_utils::ErrorMapper,
                                                d,
                                            );
                                            v
                                        })
                                        .collect();
                                    let (field, annot_t, annot_ast, get_value) = mk_field(
                                        cx,
                                        &tparams_map_with_this,
                                        field_reason,
                                        annot,
                                        value,
                                    )?;
                                    let loc_c = loc.dupe();
                                    let annot_t_c = annot_t.dupe();
                                    let id_loc_c = id_loc.dupe();
                                    let id_pair_c = id_pair.clone();
                                    let static_c = static_;
                                    let override_c = override_;
                                    let variance_c = variance.clone();
                                    let ts_accessibility_c = ts_accessibility.clone();
                                    let prop_comments_c = prop_comments.clone();
                                    let get_element =
                                        Box::new(
                                            move |_cx: &Context<'_>| -> ast::class::BodyElement<
                                                ALoc,
                                                (ALoc, Type),
                                            > {
                                                BodyElement::Property(Property {
                                                    loc: (loc_c, annot_t_c.dupe()),
                                                    key: expression::object::Key::Identifier(
                                                        ast::Identifier::new(
                                                            ast::IdentifierInner {
                                                                loc: (id_loc_c, annot_t_c),
                                                                name: id_pair_c.name.dupe(),
                                                                comments: id_pair_c.comments.dupe(),
                                                            },
                                                        ),
                                                    ),
                                                    annot: annot_ast,
                                                    value: get_value(),
                                                    static_: static_c,
                                                    override_: override_c,
                                                    optional,
                                                    variance: variance_c,
                                                    ts_accessibility: ts_accessibility_c,
                                                    decorators: decorators_ast.into(),
                                                    comments: prop_comments_c,
                                                })
                                            },
                                        );
                                    check_duplicate_name(
                                        &mut public_seen_names,
                                        id_loc.dupe(),
                                        name,
                                        static_,
                                        false,
                                        ClassMemberKind::ClassMemberField,
                                    );
                                    class_sig::add_field(
                                        static_,
                                        name.dupe(),
                                        id_loc.dupe(),
                                        polarity,
                                        field,
                                        &mut class_sig,
                                    );
                                    rev_elements.push(get_element);
                                }
                                Key::StringLiteral(_)
                                | Key::NumberLiteral(_)
                                | Key::BigIntLiteral(_) => {
                                    let loc = &prop.loc;
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            loc.dupe(),
                                            UnsupportedSyntax::ClassPropertyLiteral,
                                        ))),
                                    );
                                    let elem_c = elem.clone();
                                    rev_elements.push(Box::new(move |_cx| {
                                        let Ok(v) = polymorphic_ast_mapper::class_element(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &elem_c,
                                        );
                                        v
                                    }));
                                }
                                Key::Computed(_) => {
                                    let loc = &prop.loc;
                                    flow_js::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            loc.dupe(),
                                            UnsupportedSyntax::ClassPropertyComputed,
                                        ))),
                                    );
                                    let elem_c = elem.clone();
                                    rev_elements.push(Box::new(move |_cx| {
                                        let Ok(v) = polymorphic_ast_mapper::class_element(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &elem_c,
                                        );
                                        v
                                    }));
                                }
                            }
                        }

                        // fields
                        BodyElement::PrivateField(pf) => {
                            let loc = &pf.loc;
                            let id_loc = &pf.key.loc;
                            let name = &pf.key.name;
                            let annot = &pf.annot;
                            let value = &pf.value;
                            let static_ = pf.static_;
                            let override_ = pf.override_;
                            let optional = pf.optional;
                            let variance = &pf.variance;
                            let ts_accessibility = &pf.ts_accessibility;
                            let decorators = &pf.decorators;
                            let field_comments = &pf.comments;

                            check_ts_accessibility(cx, ts_accessibility);
                            if override_ {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        loc.dupe(),
                                        UnsupportedSyntax::TSLibSyntax(
                                            TsLibSyntaxKind::OverrideModifier,
                                        ),
                                    ))),
                                );
                            }
                            if optional && !cx.tslib_syntax() {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        loc.dupe(),
                                        UnsupportedSyntax::TSLibSyntax(
                                            TsLibSyntaxKind::OptionalClassProperty,
                                        ),
                                    ))),
                                );
                            }
                            let field_reason = mk_reason(
                                VirtualReasonDesc::RPrivateProperty(name.dupe()),
                                loc.dupe(),
                            );
                            let polarity = type_annotation::polarity(cx, variance.as_ref());
                            let decorators_ast: Vec<_> = decorators
                                .iter()
                                .map(|d| {
                                    let Ok(v) = polymorphic_ast_mapper::class_decorator(
                                        &mut typed_ast_utils::ErrorMapper,
                                        d,
                                    );
                                    v
                                })
                                .collect();
                            let (field, annot_t, annot_ast, get_value) =
                                mk_field(cx, &tparams_map_with_this, field_reason, annot, value)?;
                            let loc_c = loc.dupe();
                            let annot_t_c = annot_t.dupe();
                            let key_c = pf.key.clone();
                            let static_c = static_;
                            let override_c = override_;
                            let variance_c = variance.clone();
                            let ts_accessibility_c = ts_accessibility.clone();
                            let field_comments_c = field_comments.clone();
                            let get_element_inner =
                                move || -> ast::class::BodyElement<ALoc, (ALoc, Type)> {
                                    BodyElement::PrivateField(PrivateField {
                                        loc: (loc_c, annot_t_c),
                                        key: key_c,
                                        annot: annot_ast,
                                        value: get_value(),
                                        static_: static_c,
                                        override_: override_c,
                                        optional,
                                        variance: variance_c,
                                        ts_accessibility: ts_accessibility_c,
                                        decorators: decorators_ast.into(),
                                        comments: field_comments_c,
                                    })
                                };
                            let get_element: Box<
                                dyn FnOnce(
                                        &Context<'a>,
                                    )
                                        -> ast::class::BodyElement<ALoc, (ALoc, Type)>
                                    + 'a,
                            > = Box::new(move |_cx| get_element_inner());
                            check_duplicate_name(
                                &mut public_seen_names,
                                id_loc.dupe(),
                                name,
                                static_,
                                true,
                                ClassMemberKind::ClassMemberField,
                            );
                            class_sig::add_private_field(
                                name.dupe(),
                                id_loc.dupe(),
                                polarity,
                                field,
                                static_,
                                &mut class_sig,
                            );
                            rev_elements.push(get_element);
                        }
                        BodyElement::StaticBlock(_sb) => {
                            let loc = &_sb.loc;
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    loc.dupe(),
                                    UnsupportedSyntax::ClassStaticBlock,
                                ))),
                            );
                            let elem_c = elem.clone();
                            rev_elements.push(Box::new(move |_cx| {
                                let Ok(v) = polymorphic_ast_mapper::class_element(
                                    &mut typed_ast_utils::ErrorMapper,
                                    &elem_c,
                                );
                                v
                            }));
                        }
                        BodyElement::DeclareMethod(_dm) => {
                            let loc = &_dm.loc;
                            flow_js::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    loc.dupe(),
                                    UnsupportedSyntax::ClassDeclareMethod,
                                ))),
                            );
                            let elem_c = elem.clone();
                            rev_elements.push(Box::new(move |_cx| {
                                let Ok(v) = polymorphic_ast_mapper::class_element(
                                    &mut typed_ast_utils::ErrorMapper,
                                    &elem_c,
                                );
                                v
                            }));
                        }
                        BodyElement::AbstractMethod(_am) => {
                            let loc = &_am.loc;
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                                    kind: TSSyntaxKind::AbstractMethod,
                                    loc: loc.dupe(),
                                })),
                            );
                            let elem_c = elem.clone();
                            rev_elements.push(Box::new(move |_cx| {
                                let Ok(v) = polymorphic_ast_mapper::class_element(
                                    &mut typed_ast_utils::ErrorMapper,
                                    &elem_c,
                                );
                                v
                            }));
                        }
                        BodyElement::AbstractProperty(_ap) => {
                            let loc = &_ap.loc;
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                                    kind: TSSyntaxKind::AbstractMethod,
                                    loc: loc.dupe(),
                                })),
                            );
                            let elem_c = elem.clone();
                            rev_elements.push(Box::new(move |_cx| {
                                let Ok(v) = polymorphic_ast_mapper::class_element(
                                    &mut typed_ast_utils::ErrorMapper,
                                    &elem_c,
                                );
                                v
                            }));
                        }
                        BodyElement::IndexSignature(indexer) => {
                            let loc = &indexer.loc;
                            if !(cx.tslib_syntax() && cx.under_declaration_context()) {
                                flow_js::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        loc.dupe(),
                                        UnsupportedSyntax::ClassIndexSignature,
                                    ))),
                                );
                                let elem_c = elem.clone();
                                rev_elements.push(Box::new(move |_cx| {
                                    let Ok(v) = polymorphic_ast_mapper::class_element(
                                        &mut typed_ast_utils::ErrorMapper,
                                        &elem_c,
                                    );
                                    v
                                }));
                            } else {
                                let static_ = indexer.static_;
                                if class_sig::has_indexer(static_, &class_sig) {
                                    flow_js_utils::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            loc.dupe(),
                                            UnsupportedSyntax::MultipleIndexers,
                                        ))),
                                    );
                                    let elem_c = elem.clone();
                                    rev_elements.push(Box::new(move |_cx| {
                                        let Ok(v) = polymorphic_ast_mapper::class_element(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &elem_c,
                                        );
                                        v
                                    }));
                                } else {
                                    let (dict, indexer_ast) = type_annotation::convert_indexer(
                                        cx,
                                        &tparams_map_with_this,
                                        indexer,
                                    );
                                    class_sig::add_indexer(static_, dict, &mut class_sig);
                                    rev_elements.push(Box::new(move |_cx| {
                                        BodyElement::IndexSignature(indexer_ast)
                                    }));
                                }
                            }
                        }
                    }
                }
                {
                    let binding = class_sig::mk_class_binding(cx, &class_sig);
                    cx.environment_mut().class_bindings.insert(
                        EnvKey::new(
                            flow_env_builder::env_api::DefLocType::OrdinaryNameLoc,
                            class_loc.dupe(),
                        ),
                        binding,
                    );
                }
                let (instance_this_default, static_this_default, super_t, static_super) =
                    type_env::in_class_scope(cx, class_loc.dupe(), || {
                        class_sig::make_thises(cx, &class_sig)
                    });
                type_env::bind_class_instance_this(cx, instance_this_default, class_loc.dupe());
                type_env::bind_class_static_this(cx, static_this_default, class_loc.dupe());
                type_env::bind_class_instance_super(cx, super_t, class_loc.dupe());
                type_env::bind_class_static_super(cx, static_super, class_loc.dupe());
                let (class_t_internal, class_t, _) =
                    class_sig::classtype(cx, true, inst_kind.clone(), &class_sig);

                let id_c = id.clone();
                let body_loc_c = body_loc.dupe();
                let body_comments_c = body_comments.clone();
                let comments_c = comments.clone();
                let tparams_ast_c = tparams_ast.clone();
                let implements_ast_c = implements_ast.clone();
                let class_decorators_ast_c = class_decorators_ast.clone();
                let elements_cell: Rc<
                    RefCell<
                        Option<
                            Vec<
                                Box<
                                    dyn FnOnce(
                                            &Context<'a>,
                                        )
                                            -> ast::class::BodyElement<ALoc, (ALoc, Type)>
                                        + 'a,
                                >,
                            >,
                        >,
                    >,
                > = Rc::new(RefCell::new(Some(rev_elements)));
                let extends_ast_f_cell: Rc<
                    RefCell<
                        Option<
                            Box<
                                dyn FnOnce(
                                        &Context<'a>,
                                    ) -> Result<
                                        Option<ast::class::Extends<ALoc, (ALoc, Type)>>,
                                        AbnormalControlFlow,
                                    > + 'a,
                            >,
                        >,
                    >,
                > = Rc::new(RefCell::new(Some(extends_ast_f)));
                // In OCaml, closures are reusable (Fn), so the reconstruct
                // function can be called multiple times and returns the same
                // result. In Rust, the element closures are FnOnce. We cache
                // the result of the first call so subsequent calls return the
                // same value, preserving OCaml semantics.
                let cached_class_ast: Rc<RefCell<Option<ast::class::Class<ALoc, (ALoc, Type)>>>> =
                    Rc::new(RefCell::new(None));
                let reconstruct: Rc<
                    dyn Fn(&Context<'a>, Type) -> ast::class::Class<ALoc, (ALoc, Type)> + 'a,
                > = Rc::new(move |cx, class_t: Type| {
                    if let Some(ref cached) = *cached_class_ast.borrow() {
                        return cached.clone();
                    }
                    let elements = elements_cell
                        .borrow_mut()
                        .take()
                        .expect("class reconstruct: elements missing on first call");
                    let body: Vec<ast::class::BodyElement<ALoc, (ALoc, Type)>> =
                        elements.into_iter().map(|f| f(cx)).collect();
                    //     extends = extends_ast_f ();
                    let extends_fn = extends_ast_f_cell
                        .borrow_mut()
                        .take()
                        .expect("class reconstruct: extends_ast_f missing on first call");
                    let extends = extends_fn(cx).unwrap_or(None);
                    let result = ast::class::Class {
                        id: id_c.as_ref().map(|ident| {
                            ast::Identifier::new(ast::IdentifierInner {
                                loc: (ident.loc.dupe(), class_t.dupe()),
                                name: ident.name.dupe(),
                                comments: ident.comments.dupe(),
                            })
                        }),
                        body: ast::class::Body {
                            loc: body_loc_c.dupe(),
                            body: body.into(),
                            comments: body_comments_c.clone(),
                        },
                        tparams: tparams_ast_c.clone(),
                        extends,
                        implements: implements_ast_c.clone(),
                        class_decorators: class_decorators_ast_c.clone().into(),
                        comments: comments_c.clone(),
                        abstract_,
                    };
                    *cached_class_ast.borrow_mut() = Some(result.clone());
                    result
                });

                Ok((class_t, class_t_internal, class_sig, reconstruct))
            }
        }
    };

    // Note: SigTuple/SigResult inlined because they contain lifetime-dependent types.
    // The reconstruct closure only works for the specific 'a lifetime, not for<'b>.

    type SigTuple<'a> = (
        Type,
        Type,
        func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
        Rc<dyn Fn(&Context<'a>, Type) -> ast::class::Class<ALoc, (ALoc, Type)> + 'a>,
    );

    type SigResult<'a> = Result<SigTuple<'a>, AbnormalControlFlow>;

    let lazy_cell: Rc<
        RefCell<
            Option<
                Rc<
                    flow_lazy::Lazy<
                        Context<'a>,
                        SigResult<'a>,
                        Box<dyn FnOnce(&Context<'a>) -> SigResult<'a> + 'a>,
                    >,
                >,
            >,
        >,
    > = Rc::new(RefCell::new(None));
    let lazy_cell_c = lazy_cell.dupe();

    let name_loc_c = name_loc.dupe();
    let class_loc_c = class_loc.dupe();
    let inst_kind_c = inst_kind.clone();
    let reason_c = reason.dupe();
    let cls_clone = cls.clone();

    let lazy_val: Rc<
        flow_lazy::Lazy<
            Context<'a>,
            SigResult<'a>,
            Box<dyn FnOnce(&Context<'a>) -> SigResult<'a> + 'a>,
        >,
    > = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
        let lazy_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
        let self_ = flow_typing_tvar::mk_fully_resolved_lazy(
            cx,
            reason_c.dupe(),
            true,
            Box::new(move |cx| {
                let val = lazy_ref.get_forced(cx).as_ref().unwrap();
                val.1.dupe()
            }),
        );

        mk_class_sig_with_self(
            cx,
            name_loc_c,
            class_loc_c,
            &inst_kind_c,
            reason_c,
            self_,
            &cls_clone,
        )
    })));

    *lazy_cell.borrow_mut() = Some(lazy_val.dupe());

    let result = lazy_val.get_forced(cx);
    let result_ref = result.as_ref().map_err(|e| e.clone())?;

    Ok((
        result_ref.0.dupe(),
        result_ref.1.dupe(),
        result_ref.2.clone(),
        result_ref.3.dupe(),
    ))
}

type RecordSigTuple<'a> = (
    Type,
    Type,
    func_class_sig_types::class::Class<func_class_sig_types::StmtConfigTypes>,
    Rc<dyn Fn(&Context<'a>, Type) -> statement::RecordDeclaration<ALoc, (ALoc, Type)> + 'a>,
);
type RecordSigResult<'a> = Result<RecordSigTuple<'a>, AbnormalControlFlow>;

pub fn mk_record_sig<'a>(
    cx: &Context<'a>,
    name_loc: ALoc,
    record_loc: ALoc,
    defaulted_props: &BTreeSet<FlowSmolStr>,
    reason: Reason,
    record: &statement::RecordDeclaration<ALoc, ALoc>,
) -> RecordSigResult<'a> {
    use std::cell::RefCell;
    use std::rc::Rc;

    use flow_env_builder::env_api::DefLocType;
    use flow_env_builder::env_api::EnvKey;
    use flow_typing_loc_env::func_class_sig_types::StmtConfigTypes;
    use flow_typing_loc_env::func_class_sig_types::class as class_types;
    use flow_typing_loc_env::func_class_sig_types::func;

    let mk_record_field = |cx: &Context<'a>,
                           tparams_map: &FlowOrdMap<SubstName, Type>,
                           reason: Reason,
                           annot: &ast::types::Annotation<ALoc, ALoc>,
                           default_value: Option<&expression::Expression<ALoc, ALoc>>|
     -> (
        class_types::Field<StmtConfigTypes>,
        Type,
        ast::types::Annotation<ALoc, (ALoc, Type)>,
        Box<dyn FnOnce() -> Option<expression::Expression<ALoc, (ALoc, Type)>>>,
    ) {
        let (annot_t, annot_ast) = type_annotation::mk_type_available_annotation(
            cx,
            tparams_map
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect(),
            annot,
        );
        match default_value {
            None => (
                class_types::Field::Annot(annot_t.dupe()),
                annot_t,
                annot_ast,
                Box::new(|| None),
            ),
            Some(expr) => {
                let value_ref: Rc<RefCell<Option<expression::Expression<ALoc, (ALoc, Type)>>>> =
                    Rc::new(RefCell::new(None));
                let annot_loc = annot.loc.dupe();
                let value_ref_c = value_ref.dupe();
                let set_asts: class_types::SetAsts<StmtConfigTypes> =
                    Rc::new(move |(_, _, value_opt)| {
                        *value_ref_c.borrow_mut() = Some(value_opt.unwrap());
                    });
                let field = class_types::Field::Infer(
                    crate::func_sig::field_initializer(
                        reason,
                        expr.clone(),
                        annot_loc,
                        AnnotatedOrInferred::Annotated(annot_t.dupe()),
                    ),
                    set_asts,
                );
                let value_ref_c2 = value_ref.dupe();
                (
                    field,
                    annot_t,
                    annot_ast,
                    Box::new(move || value_ref_c2.borrow().clone()),
                )
            }
        }
    };

    let mk_record_static_field = |cx: &Context<'a>,
                                  tparams_map: &FlowOrdMap<SubstName, Type>,
                                  reason: Reason,
                                  annot: &ast::types::Annotation<ALoc, ALoc>,
                                  value_expr: &expression::Expression<ALoc, ALoc>|
     -> (
        class_types::Field<StmtConfigTypes>,
        Type,
        ast::types::Annotation<ALoc, (ALoc, Type)>,
        Box<dyn FnOnce() -> expression::Expression<ALoc, (ALoc, Type)>>,
    ) {
        let (annot_t, annot_ast) = type_annotation::mk_type_available_annotation(
            cx,
            tparams_map
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect(),
            annot,
        );
        let value_ref: Rc<RefCell<Option<expression::Expression<ALoc, (ALoc, Type)>>>> =
            Rc::new(RefCell::new(None));
        let annot_loc = annot.loc.dupe();
        let value_ref_c = value_ref.dupe();
        let set_asts: class_types::SetAsts<StmtConfigTypes> = Rc::new(move |(_, _, value_opt)| {
            *value_ref_c.borrow_mut() = Some(value_opt.unwrap());
        });
        let field = class_types::Field::Infer(
            crate::func_sig::field_initializer(
                reason,
                value_expr.clone(),
                annot_loc,
                AnnotatedOrInferred::Annotated(annot_t.dupe()),
            ),
            set_asts,
        );
        let value_ref_c2 = value_ref.dupe();
        (
            field,
            annot_t,
            annot_ast,
            Box::new(move || value_ref_c2.borrow().clone().unwrap()),
        )
    };

    let mk_record_sig_with_self = move |cx: &Context<'a>,
                                        name_loc: ALoc,
                                        record_loc: ALoc,
                                        defaulted_props: &BTreeSet<FlowSmolStr>,
                                        reason: Reason,
                                        self_: Type,
                                        record: &statement::RecordDeclaration<ALoc, ALoc>|
          -> RecordSigResult<'a> {
        let node_cache = cx.node_cache();
        match node_cache.get_record_sig(&record_loc) {
            Some((class_t, class_t_internal, class_sig, record_ast_f)) => {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![format!(
                        "Record sig cache hit at {}",
                        reason.loc().debug_to_string(false)
                    )]
                });
                Ok((
                    class_t,
                    class_t_internal,
                    class_sig,
                    Rc::new(move |cx, t: Type| record_ast_f(cx, t)),
                ))
            }
            None => {
                let id_loc = record.id.loc.dupe();
                let id_name = &record.id;
                let record_name_str = &id_name.name;
                let body_loc = record.body.loc.dupe();
                let elements = &record.body.body;
                let body_comments = &record.body.comments;

                let (tparams, tparams_map, tparams_ast) =
                    type_annotation::mk_type_param_declarations(
                        cx,
                        flow_parser::ast_visitor::TypeParamsContext::Class,
                        None,
                        record.tparams.as_ref(),
                    );
                let (this_tparam, this_t) = class_sig::mk_this(self_, cx, reason.dupe());
                let mut tparams_map_with_this = tparams_map.dupe();
                tparams_map_with_this
                    .insert(SubstName::name(FlowSmolStr::new("this")), this_t.dupe());

                let record_name = Some(record_name_str.dupe());

                let (mut class_sig, implements_ast) = {
                    let aloc_id = cx.make_aloc_id(&name_loc);
                    let (implements, implements_ast) = {
                        match &record.implements {
                            None => (vec![], None),
                            Some(impl_node) => {
                                let implements_loc = impl_node.loc.dupe();
                                let interfaces = &impl_node.interfaces;
                                let impl_comments = &impl_node.comments;
                                let (implements, interfaces_ast): (Vec<_>, Vec<_>) = interfaces
                                    .iter()
                                    .map(|i| {
                                        let loc = i.loc.dupe();
                                        let id = &i.id;
                                        match id {
                                            ast::types::generic::Identifier::Qualified(_)
                                            | ast::types::generic::Identifier::ImportTypeAnnot(_)
                                                if !cx.tslib_syntax() =>
                                            {
                                                flow_js::add_output_non_speculating(
                                                    cx,
                                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                                        loc.dupe(),
                                                        UnsupportedSyntax::TSLibSyntax(
                                                            TsLibSyntaxKind::ImplementsDottedPath,
                                                        ),
                                                    ))),
                                                );
                                            }
                                            _ => {}
                                        }
                                        let (c, id) = type_annotation::convert_qualification(
                                            cx,
                                            "implements",
                                            id,
                                        );
                                        let (typeapp, targs_ast) = match &i.targs {
                                            None => ((loc.dupe(), c.dupe(), None), None),
                                            Some(targs_node) => {
                                                let targs_loc = targs_node.loc.dupe();
                                                let targs_arguments = &targs_node.arguments;
                                                let targs_comments = &targs_node.comments;
                                                let (ts, targs_ast) = type_annotation::convert_list(
                                                    cx,
                                                    tparams_map
                                                        .iter()
                                                        .map(|(k, v)| (k.dupe(), v.dupe()))
                                                        .collect(),
                                                    targs_arguments,
                                                );
                                                (
                                                    (loc.dupe(), c.dupe(), Some(ts)),
                                                    Some(ast::types::TypeArgs::<
                                                        ALoc,
                                                        (ALoc, Type),
                                                    > {
                                                        loc: targs_loc,
                                                        arguments: targs_ast.into(),
                                                        comments: targs_comments.clone(),
                                                    }),
                                                )
                                            }
                                        };
                                        (
                                            typeapp,
                                            ast::class::implements::Interface {
                                                loc: loc.dupe(),
                                                id,
                                                targs: targs_ast,
                                            },
                                        )
                                    })
                                    .unzip();
                                (
                                    implements,
                                    Some(ast::class::Implements {
                                        loc: implements_loc,
                                        interfaces: interfaces_ast.into(),
                                        comments: impl_comments.clone(),
                                    }),
                                )
                            }
                        }
                    };
                    // Records have no extends clause
                    let super_ = class_types::Super::Class(class_types::ClassSuper {
                        extends: class_types::Extends::Implicit { null: false },
                        mixins: vec![],
                        implements,
                        this_t: this_t.dupe(),
                        this_tparam,
                    });
                    let class_sig = class_sig::empty(
                        aloc_id,
                        record_name.dupe(),
                        record_loc.dupe(),
                        reason.dupe(),
                        tparams,
                        tparams_map
                            .iter()
                            .map(|(k, v)| (k.dupe(), v.dupe()))
                            .collect(),
                        super_,
                    );
                    (class_sig, implements_ast)
                };

                let mk_record_constructor =
                    |cx,
                     tparams_map_with_this: &FlowOrdMap<SubstName, Type>,
                     elements: &[statement::record_declaration::BodyElement<ALoc, ALoc>],
                     defaulted_props: &BTreeSet<FlowSmolStr>,
                     record_name_str: &FlowSmolStr,
                     name_loc: ALoc|
                     -> func::Func<StmtConfigTypes> {
                        let props: properties::PropertiesMap = elements
                            .iter()
                            .filter_map(|elem| {
                                match elem {
                                    statement::record_declaration::BodyElement::Property(prop) => {
                                        let key = &prop.key;
                                        let annot = &prop.annot;
                                        let (annot_t, _) =
                                            type_annotation::mk_type_available_annotation(
                                                cx,
                                                tparams_map_with_this
                                                    .iter()
                                                    .map(|(k, v)| (k.dupe(), v.dupe()))
                                                    .collect(),
                                                annot,
                                            );
                                        let (key_loc, name) =
                                            flow_parser_utils::record_utils::loc_and_string_of_property_key(key)
                                                .expect("Record property must have a valid key");
                                        let prop_t = if defaulted_props.contains(&name) {
                                            let field_reason = mk_reason(
                                                VirtualReasonDesc::RProperty(Some(Name::new(
                                                    name.dupe(),
                                                ))),
                                                key_loc.dupe(),
                                            );
                                            Type::new(TypeInner::OptionalT {
                                                reason: mk_reason(
                                                    VirtualReasonDesc::ROptional(Arc::new(
                                                        field_reason.desc(true).clone(),
                                                    )),
                                                    key_loc.dupe(),
                                                ),
                                                type_: annot_t,
                                                use_desc: false,
                                            })
                                        } else {
                                            annot_t
                                        };
                                        Some((
                                            Name::new(name),
                                            type_::Property::new(type_::PropertyInner::Field(Box::new(FieldData {
                                                preferred_def_locs: None,
                                                key_loc: Some(key_loc),
                                                type_: prop_t,
                                                polarity: Polarity::Positive,
                                            }))),
                                        ))
                                    }
                                    _ => None,
                                }
                            })
                            .collect();
                        let record_reason = mk_reason(
                            VirtualReasonDesc::RRecordType(record_name_str.dupe()),
                            name_loc.dupe(),
                        );
                        let null_proto = type_::null_proto::make(record_reason.dupe());
                        let obj_t = obj_type::mk_with_proto(
                            cx,
                            record_reason.dupe(),
                            type_::ObjKind::Exact,
                            None,
                            None,
                            Some(props),
                            None,
                            null_proto,
                        );
                        let param = flow_typing_loc_env::func_stmt_config_types::Param {
                            t: obj_t.dupe(),
                            loc: name_loc.dupe(),
                            ploc: name_loc.dupe(),
                            pattern: flow_typing_loc_env::func_stmt_config_types::Pattern::Object {
                                annot: ast::types::AnnotationOrHint::Missing((
                                    name_loc.dupe(),
                                    obj_t,
                                )),
                                properties: vec![],
                                optional: false,
                                comments: None,
                            },
                            default: None,
                            has_anno: false,
                        };
                        func::Func {
                            reason: mk_reason(VirtualReasonDesc::RConstructor, name_loc.dupe()),
                            kind: func::Kind::Ctor,
                            tparams: None,
                            fparams: func_class_sig_types::param::Param {
                                params: vec![param],
                                rest: None,
                                this_: None,
                                reconstruct: Rc::new(|_, _, _| None),
                            },
                            body: None,
                            return_t: AnnotatedOrInferred::Inferred(type_::void::at(
                                name_loc.dupe(),
                            )),
                            effect_: type_::ReactEffectType::ArbitraryEffect,
                            ret_annot_loc: name_loc,
                            statics: None,
                        }
                    };

                let func_sig = mk_record_constructor(
                    cx,
                    &tparams_map_with_this,
                    elements,
                    defaulted_props,
                    record_name_str,
                    name_loc.dupe(),
                );
                class_sig::add_constructor(None, func_sig, None, None, &mut class_sig);

                class_sig::add_name_field(&mut class_sig);

                let check_duplicate_name =
                    |public_seen_names: &mut SeenNames,
                     member_loc: ALoc,
                     name: &FlowSmolStr,
                     static_: bool,
                     private_: bool,
                     kind: ClassMemberKind| {
                        check_duplicate_class_member(
                            cx,
                            public_seen_names,
                            static_,
                            private_,
                            name,
                            kind,
                            member_loc,
                            intermediate_error_types::ClassKind::Record,
                        );
                    };

                let mut public_seen_names = empty_seen_names();
                let mut rev_elements: Vec<
                    Box<
                        dyn FnOnce(
                                &Context<'a>,
                            )
                                -> statement::record_declaration::BodyElement<ALoc, (ALoc, Type)>
                            + 'a,
                    >,
                > = Vec::new();

                for element in elements.iter() {
                    match element {
                        statement::record_declaration::BodyElement::Method(method) => match &method
                            .key
                        {
                            expression::object::Key::Identifier(method_id_node) => {
                                let method_loc = method.loc.dupe();
                                let method_id_loc = method_id_node.loc.dupe();
                                let name = method_id_node.name.dupe();
                                let method_id = method_id_node.clone();
                                let func_loc = method.value.0.dupe();
                                let func = &method.value.1;
                                let kind = method.kind;
                                let static_ = method.static_;
                                let method_comments = method.comments.dupe();

                                let reason =
                                    func_reason(func.async_, func.generator, method_loc.dupe());
                                let getset = kind == ast::class::MethodKind::Get
                                    || kind == ast::class::MethodKind::Set;
                                let (method_sig, reconstruct_func, deferred_tg_check) =
                                    mk_func_sig(
                                        cx,
                                        true,
                                        false,
                                        getset,
                                        &BTreeMap::new(),
                                        &tparams_map_with_this,
                                        reason.dupe(),
                                        func,
                                    )?;
                                let params_ref: Rc<
                                    RefCell<Option<ast::function::Params<ALoc, (ALoc, Type)>>>,
                                > = Rc::new(RefCell::new(None));
                                let body_ref: Rc<
                                    RefCell<Option<ast::function::Body<ALoc, (ALoc, Type)>>>,
                                > = Rc::new(RefCell::new(None));
                                let params_ref_c = params_ref.dupe();
                                let body_ref_c = body_ref.dupe();
                                let set_asts: class_types::SetAsts<StmtConfigTypes> =
                                    Rc::new(move |(params_opt, body_opt, _)| {
                                        *params_ref_c.borrow_mut() = Some(params_opt.unwrap());
                                        *body_ref_c.borrow_mut() = Some(body_opt.unwrap());
                                    });
                                let func_t_ref: Rc<RefCell<Option<Type>>> =
                                    Rc::new(RefCell::new(None));
                                let func_t_ref_c = func_t_ref.dupe();
                                let set_type: class_types::SetType = Rc::new(move |t| {
                                    *func_t_ref_c.borrow_mut() = Some(t);
                                });

                                let params_ref_c2 = params_ref.dupe();
                                let body_ref_c2 = body_ref.dupe();
                                let func_t_ref_c2 = func_t_ref.dupe();
                                let reconstruct_func = reconstruct_func.dupe();
                                let func_clone = func.clone();
                                let method_id_clone = method_id.clone();
                                let method_id_loc_c = method_id_loc.dupe();
                                let method_loc_c = method_loc.dupe();
                                let func_loc_c = func_loc.dupe();
                                let method_comments_c = method_comments.clone();
                                let kind_c = kind;
                                let static_c = static_;
                                let get_element = move |cx: &Context<'a>| {
                                    if let Some(check) = deferred_tg_check {
                                        check(cx);
                                    }
                                    let params =
                                        params_ref_c2.borrow().clone().unwrap_or_else(|| {
                                            let Ok(v) = polymorphic_ast_mapper::function_params(
                                                &mut typed_ast_utils::ErrorMapper,
                                                &func_clone.params,
                                            );
                                            v
                                        });
                                    let body = body_ref_c2.borrow().clone().unwrap_or_else(|| {
                                        let Ok(v) = polymorphic_ast_mapper::function_body_any(
                                            &mut typed_ast_utils::ErrorMapper,
                                            &func_clone.body,
                                        );
                                        v
                                    });
                                    let func_t = func_t_ref_c2
                                        .borrow()
                                        .clone()
                                        .unwrap_or_else(|| empty_t::at(method_id_loc_c.dupe()));
                                    let typed_func = reconstruct_func(params, body, func_t.dupe());
                                    statement::record_declaration::BodyElement::Method(
                                        ast::class::Method {
                                            loc: (method_loc_c, func_t.dupe()),
                                            key: expression::object::Key::Identifier(
                                                ast::Identifier::new(ast::IdentifierInner {
                                                    loc: (method_id_loc_c, func_t),
                                                    name: method_id_clone.name.dupe(),
                                                    comments: method_id_clone.comments.dupe(),
                                                }),
                                            ),
                                            value: (func_loc_c, typed_func),
                                            kind: kind_c,
                                            static_: static_c,
                                            override_: false,
                                            ts_accessibility: None,
                                            decorators: vec![].into(),
                                            comments: method_comments_c,
                                        },
                                    )
                                };

                                match kind {
                                    ast::class::MethodKind::Constructor => {
                                        panic!("Records cannot have constructors")
                                    }
                                    ast::class::MethodKind::Get => {
                                        panic!("Records cannot have getters")
                                    }
                                    ast::class::MethodKind::Set => {
                                        panic!("Records cannot have setters")
                                    }
                                    ast::class::MethodKind::Method => {
                                        check_duplicate_name(
                                            &mut public_seen_names,
                                            method_id_loc.dupe(),
                                            &name,
                                            static_,
                                            false,
                                            ClassMemberKind::ClassMemberMethod,
                                        );
                                        class_sig::add_method(
                                            static_,
                                            name,
                                            method_id_loc,
                                            Some(func_loc),
                                            method_sig,
                                            Some(set_asts),
                                            Some(set_type),
                                            &mut class_sig,
                                        );
                                        rev_elements.push(Box::new(get_element));
                                    }
                                }
                            }
                            expression::object::Key::PrivateName(_) => {
                                panic!("Records cannot have private methods")
                            }
                            expression::object::Key::StringLiteral(_)
                            | expression::object::Key::NumberLiteral(_)
                            | expression::object::Key::BigIntLiteral(_)
                            | expression::object::Key::Computed(_) => {
                                panic!("Records can only have identifier method keys")
                            }
                        },

                        statement::record_declaration::BodyElement::Property(prop) => {
                            let statement::record_declaration::Property {
                                loc: prop_loc,
                                key,
                                annot,
                                default_value,
                                comments: prop_comments,
                                invalid_syntax: _,
                            } = prop;
                            let prop_loc = prop_loc.dupe();
                            let default_value = default_value.as_ref();
                            let prop_comments = prop_comments.clone();
                            let (key_loc, name) =
                                flow_parser_utils::record_utils::loc_and_string_of_property_key(
                                    key,
                                )
                                .expect("Record property must have a valid key");
                            let reason = mk_reason(
                                VirtualReasonDesc::RProperty(Some(Name::new(name.dupe()))),
                                prop_loc.dupe(),
                            );
                            let (field, annot_t, annot_ast, get_value) = mk_record_field(
                                cx,
                                &tparams_map_with_this,
                                reason,
                                annot,
                                default_value,
                            );
                            let key_clone = key.clone();
                            let get_element = move |_cx: &Context<'a>| {
                                statement::record_declaration::BodyElement::Property(
                                    statement::record_declaration::Property {
                                        loc: (prop_loc, annot_t.dupe()),
                                        key: translate_identifier_or_literal_key(
                                            annot_t, &key_clone,
                                        ),
                                        annot: annot_ast,
                                        default_value: get_value(),
                                        comments: prop_comments,
                                        invalid_syntax: None,
                                    },
                                )
                            };
                            check_duplicate_name(
                                &mut public_seen_names,
                                key_loc.dupe(),
                                &name,
                                false,
                                false,
                                ClassMemberKind::ClassMemberField,
                            );
                            class_sig::add_field(
                                false,
                                name,
                                key_loc,
                                Polarity::Positive,
                                field,
                                &mut class_sig,
                            );
                            rev_elements.push(Box::new(get_element));
                        }
                        statement::record_declaration::BodyElement::StaticProperty(static_prop) => {
                            let statement::record_declaration::StaticProperty {
                                loc: prop_loc,
                                key,
                                annot,
                                value: value_expr,
                                comments: prop_comments,
                                invalid_syntax: _,
                            } = static_prop;
                            let prop_loc = prop_loc.dupe();
                            let prop_comments = prop_comments.clone();
                            let (key_loc, name) =
                                flow_parser_utils::record_utils::loc_and_string_of_property_key(
                                    key,
                                )
                                .expect("Record static property must have a valid key");
                            let reason = mk_reason(
                                VirtualReasonDesc::RProperty(Some(Name::new(name.dupe()))),
                                prop_loc.dupe(),
                            );
                            let (field, annot_t, annot_ast, get_value) = mk_record_static_field(
                                cx,
                                &tparams_map_with_this,
                                reason,
                                annot,
                                value_expr,
                            );
                            let key_clone = key.clone();
                            let get_element = move |_cx: &Context<'a>| {
                                statement::record_declaration::BodyElement::StaticProperty(
                                    statement::record_declaration::StaticProperty {
                                        loc: (prop_loc, annot_t.dupe()),
                                        key: translate_identifier_or_literal_key(
                                            annot_t, &key_clone,
                                        ),
                                        annot: annot_ast,
                                        value: get_value(),
                                        comments: prop_comments,
                                        invalid_syntax: None,
                                    },
                                )
                            };
                            check_duplicate_name(
                                &mut public_seen_names,
                                key_loc.dupe(),
                                &name,
                                true,
                                false,
                                ClassMemberKind::ClassMemberField,
                            );
                            class_sig::add_field(
                                true,
                                name,
                                key_loc,
                                Polarity::Positive,
                                field,
                                &mut class_sig,
                            );
                            rev_elements.push(Box::new(get_element));
                        }
                    }
                }
                let binding = class_sig::mk_class_binding(cx, &class_sig);
                {
                    cx.environment_mut().class_bindings.insert(
                        EnvKey::new(DefLocType::OrdinaryNameLoc, record_loc.dupe()),
                        binding,
                    );
                }

                let (instance_this_default, static_this_default, super_t, static_super) =
                    type_env::in_class_scope(cx, record_loc.dupe(), || {
                        class_sig::make_thises(cx, &class_sig)
                    });
                type_env::bind_class_instance_this(cx, instance_this_default, record_loc.dupe());
                type_env::bind_class_static_this(cx, static_this_default, record_loc.dupe());
                type_env::bind_class_instance_super(cx, super_t, record_loc.dupe());
                type_env::bind_class_static_super(cx, static_super, record_loc.dupe());

                let inst_kind = InstanceKind::RecordKind {
                    defaulted_props: defaulted_props.iter().duped().collect(),
                };
                let (class_t_internal, class_t, _) =
                    class_sig::classtype(cx, true, inst_kind, &class_sig);

                let id_name_c = id_name.clone();
                let id_loc_c = id_loc.dupe();
                let tparams_ast_c = tparams_ast;
                let implements_ast_c = implements_ast;
                let record_comments = record.comments.dupe();
                let record_invalid_syntax = record.invalid_syntax.clone();
                let body_comments_c = body_comments.clone();
                Ok((class_t, class_t_internal, class_sig, {
                    let inner = RefCell::new(Some(move |cx: &Context<'a>, record_t: Type| {
                        statement::RecordDeclaration {
                            id: ast::Identifier::new(ast::IdentifierInner {
                                loc: (id_loc_c, record_t),
                                name: id_name_c.name.dupe(),
                                comments: id_name_c.comments.dupe(),
                            }),
                            body: statement::record_declaration::Body {
                                loc: body_loc,
                                body: rev_elements.into_iter().map(|f| f(cx)).collect(),
                                comments: body_comments_c,
                            },
                            tparams: tparams_ast_c,
                            implements: implements_ast_c,
                            comments: record_comments,
                            invalid_syntax: record_invalid_syntax,
                        }
                    }));
                    Rc::new(move |cx, t: Type| {
                        inner
                            .borrow_mut()
                            .take()
                            .expect("record reconstruct called more than once")(
                            cx, t
                        )
                    })
                }))
            }
        }
    };

    let lazy_cell: Rc<
        RefCell<
            Option<
                Rc<
                    flow_lazy::Lazy<
                        Context<'a>,
                        RecordSigResult<'a>,
                        Box<dyn FnOnce(&Context<'a>) -> RecordSigResult<'a> + 'a>,
                    >,
                >,
            >,
        >,
    > = Rc::new(RefCell::new(None));
    let lazy_cell_c = lazy_cell.dupe();

    let name_loc_c = name_loc.dupe();
    let record_loc_c = record_loc.dupe();
    let defaulted_props_c = defaulted_props.clone();
    let reason_c = reason.dupe();
    let record_clone = record.clone();

    let lazy_val: Rc<
        flow_lazy::Lazy<
            Context<'a>,
            RecordSigResult<'a>,
            Box<dyn FnOnce(&Context<'a>) -> RecordSigResult<'a> + 'a>,
        >,
    > = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx| {
        let lazy_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
        let self_ = flow_typing_tvar::mk_fully_resolved_lazy(
            cx,
            reason_c.dupe(),
            true,
            Box::new(move |cx| {
                let val = lazy_ref.get_forced(cx).as_ref().unwrap();
                val.1.dupe()
            }),
        );

        mk_record_sig_with_self(
            cx,
            name_loc_c,
            record_loc_c,
            &defaulted_props_c,
            reason_c,
            self_,
            &record_clone,
        )
    })));

    *lazy_cell.borrow_mut() = Some(lazy_val.dupe());

    let result = lazy_val.get_forced(cx);
    let result_ref = result.as_ref().map_err(|e| e.clone())?;
    Ok((
        result_ref.0.dupe(),
        result_ref.1.dupe(),
        result_ref.2.clone(),
        result_ref.3.dupe(),
    ))
}

fn mk_record<'a>(
    cx: &Context<'a>,
    record_loc: ALoc,
    name_loc: ALoc,
    tast_record_type: Option<Type>,
    reason: Reason,
    record: &statement::RecordDeclaration<ALoc, ALoc>,
) -> Result<(Type, statement::RecordDeclaration<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let def_reason = reason.dupe().reposition(record_loc.dupe());
    let defaulted_props = flow_parser_utils::record_utils::defaulted_props_of_record(record);
    let (t, _, class_sig, record_ast_f) = mk_record_sig(
        cx,
        name_loc,
        record_loc.dupe(),
        &defaulted_props,
        reason,
        record,
    )?;
    class_sig::check_signature_compatibility(cx, def_reason, &class_sig);
    class_sig::toplevels(cx, &class_sig)?;
    let tast_type = tast_record_type.unwrap_or_else(|| t.dupe());
    Ok((t, record_ast_f(cx, tast_type)))
}

pub fn mk_component_sig<'a>(
    cx: &Context<'a>,
    tparams_map: &BTreeMap<SubstName, Type>,
    reason: Reason,
    component: &statement::ComponentDeclaration<ALoc, ALoc>,
) -> (
    flow_typing_loc_env::component_sig_types::component_sig::ComponentSig,
    flow_typing_loc_env::component_sig_types::component_sig::ReconstructComponent,
) {
    use flow_typing_loc_env::component_sig_types::declaration_param_config;
    use flow_typing_loc_env::component_sig_types::param_types;

    fn mk_param_annot<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        reason: Reason,
        annot: &ast::types::AnnotationOrHint<ALoc, ALoc>,
    ) -> (Type, ast::types::AnnotationOrHint<ALoc, (ALoc, Type)>) {
        match annot {
            ast::types::AnnotationOrHint::Missing(loc)
                if !matches!(
                    &*cx.typing_mode(),
                    flow_typing_context::TypingMode::CheckingMode
                ) =>
            {
                let t = cx.mk_placeholder(reason);
                (
                    t.dupe(),
                    ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                )
            }
            ast::types::AnnotationOrHint::Missing(loc) => {
                let t = type_env::find_write(
                    cx,
                    flow_env_builder::env_api::DefLocType::FunctionParamLoc,
                    reason,
                );
                (
                    t.dupe(),
                    ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                )
            }
            ast::types::AnnotationOrHint::Available(annot) => {
                let (t, ast_annot) =
                    type_annotation::mk_type_available_annotation(cx, tparams_map.dupe(), annot);
                (t, ast::types::AnnotationOrHint::Available(ast_annot))
            }
        }
    }

    fn id_param<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        id: &ast::pattern::Identifier<ALoc, ALoc>,
        mk_reason_fn: impl FnOnce(FlowSmolStr) -> Reason,
    ) -> (Type, ast::pattern::Identifier<ALoc, (ALoc, Type)>) {
        let ast::pattern::Identifier {
            name,
            annot,
            optional,
        } = id;
        let id_loc = name.loc.dupe();
        let name_str = name.name.dupe();
        let reason = mk_reason_fn(name_str);
        let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, annot);
        let typed_name = ast::Identifier::new(ast::IdentifierInner {
            loc: (id_loc, t.dupe()),
            name: name.name.dupe(),
            comments: name.comments.dupe(),
        });
        (
            t,
            ast::pattern::Identifier {
                name: typed_name,
                annot: typed_annot,
                optional: *optional,
            },
        )
    }

    fn mk_param<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        param: &statement::component_params::Param<ALoc, ALoc>,
    ) -> declaration_param_config::Param {
        let statement::component_params::Param {
            loc,
            local,
            default,
            name,
            shorthand,
        } = param;
        let loc = loc.dupe();
        let ploc = local.loc().dupe();
        let has_param_anno = match crate::destructuring::type_of_pattern(local) {
            ast::types::AnnotationOrHint::Missing(_) => false,
            ast::types::AnnotationOrHint::Available(_) => true,
        };
        let (t, pattern) = match local {
            ast::pattern::Pattern::Identifier { inner: id, .. } => {
                let ploc_clone = ploc.dupe();
                let (t, typed_id) = id_param(cx, tparams_map, id, move |name| {
                    mk_reason(VirtualReasonDesc::RParameter(Some(name)), ploc_clone)
                });
                (t, declaration_param_config::Pattern::Id(typed_id))
            }
            ast::pattern::Pattern::Object { inner, .. } => {
                let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, &inner.annot);
                (
                    t,
                    declaration_param_config::Pattern::Object {
                        annot: typed_annot,
                        properties: inner.properties.to_vec(),
                        optional: inner.optional,
                        comments: inner.comments.dupe(),
                    },
                )
            }
            ast::pattern::Pattern::Array { inner, .. } => {
                let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, &inner.annot);
                (
                    t,
                    declaration_param_config::Pattern::Array {
                        annot: typed_annot,
                        elements: inner.elements.to_vec(),
                        optional: inner.optional,
                        comments: inner.comments.dupe(),
                    },
                )
            }
            ast::pattern::Pattern::Expression { .. } => {
                panic!("unexpected expression pattern in param")
            }
        };
        declaration_param_config::Param {
            t,
            loc,
            ploc,
            pattern,
            default: default.clone(),
            has_anno: has_param_anno,
            name: name.clone(),
            shorthand: *shorthand,
        }
    }

    fn mk_rest<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        rest: &statement::component_params::RestParam<ALoc, ALoc>,
    ) -> Result<declaration_param_config::Rest, Box<ErrorMessage<ALoc>>> {
        let statement::component_params::RestParam {
            loc,
            argument,
            comments: rest_comments,
        } = rest;
        let loc = loc.dupe();
        let ploc = argument.loc().dupe();
        let has_param_anno = match crate::destructuring::type_of_pattern(argument) {
            ast::types::AnnotationOrHint::Missing(_) => false,
            ast::types::AnnotationOrHint::Available(_) => true,
        };
        match argument {
            ast::pattern::Pattern::Identifier { inner: id, .. } => {
                let ploc_clone = ploc.dupe();
                let (t, typed_id) = id_param(cx, tparams_map, id, move |name| {
                    mk_reason(VirtualReasonDesc::RParameter(Some(name)), ploc_clone)
                });
                Ok(declaration_param_config::Rest {
                    t,
                    loc,
                    ploc,
                    pattern: declaration_param_config::Pattern::Id(typed_id),
                    has_anno: has_param_anno,
                    comments: rest_comments.clone(),
                })
            }
            ast::pattern::Pattern::Object { inner, .. } => {
                let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, &inner.annot);
                let pattern = declaration_param_config::Pattern::Object {
                    annot: typed_annot,
                    properties: inner.properties.to_vec(),
                    optional: inner.optional,
                    comments: inner.comments.dupe(),
                };
                Ok(declaration_param_config::Rest {
                    t,
                    loc,
                    ploc,
                    pattern,
                    has_anno: has_param_anno,
                    comments: rest_comments.clone(),
                })
            }
            ast::pattern::Pattern::Array { inner, .. } => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInvalidComponentRestParam(ploc.dupe()),
                );
                let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, &inner.annot);
                let pattern = declaration_param_config::Pattern::Array {
                    annot: typed_annot,
                    elements: inner.elements.to_vec(),
                    optional: inner.optional,
                    comments: inner.comments.dupe(),
                };
                Ok(declaration_param_config::Rest {
                    t,
                    loc,
                    ploc,
                    pattern,
                    has_anno: has_param_anno,
                    comments: rest_comments.clone(),
                })
            }
            ast::pattern::Pattern::Expression { .. } => {
                Err(Box::new(ErrorMessage::EInvalidComponentRestParam(ploc)))
            }
        }
    }

    fn mk_params<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        params: &statement::component_params::Params<ALoc, ALoc>,
    ) -> param_types::Params {
        let statement::component_params::Params {
            loc: params_loc,
            params: param_list,
            rest,
            comments,
        } = params;
        let params_loc = params_loc.dupe();
        let comments = comments.clone();
        let comments_clone = comments.clone();
        let mut cparams =
            crate::component_params::empty(Rc::new(move |typed_params, typed_rest| {
                statement::component_params::Params {
                    loc: (
                        params_loc.dupe(),
                        any_t::at(type_::AnySource::Untyped, params_loc.dupe()),
                    ),
                    params: typed_params.into(),
                    rest: typed_rest,
                    comments: comments_clone.clone(),
                }
            }));
        for param in param_list.iter() {
            let p = mk_param(cx, tparams_map, param);
            crate::component_params::add_param(p, &mut cparams);
        }
        if let Some(rest) = rest {
            match mk_rest(cx, tparams_map, rest) {
                Ok(rest) => {
                    crate::component_params::add_rest(rest, &mut cparams);
                }
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                }
            }
        }
        cparams
    }

    let cache = cx.node_cache();
    let statement::ComponentDeclaration {
        tparams: tparams_ast,
        renders,
        body,
        params,
        id,
        sig_loc,
        comments: _,
        async_: _,
    } = component;
    let sig_loc = sig_loc.dupe();
    if let Some(x) = cache.get_component_sig(&sig_loc) {
        return x;
    }
    let (tparams, tparams_map, tparams_ast_typed) = type_annotation::mk_type_param_declarations(
        cx,
        flow_parser::ast_visitor::TypeParamsContext::ComponentDeclaration,
        Some(
            tparams_map
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect(),
        ),
        tparams_ast.as_ref(),
    );
    let cparams = mk_params(cx, &tparams_map, params);
    let (ret_loc, renders_t, renders_ast) = match renders {
        ast::types::ComponentRendersAnnotation::AvailableRenders(loc, annot) => {
            let (t, renders_ast_typed) =
                type_annotation::convert_render_type(cx, tparams_map.dupe(), loc.dupe(), annot);
            (
                loc.dupe(),
                t,
                ast::types::ComponentRendersAnnotation::AvailableRenders(
                    loc.dupe(),
                    renders_ast_typed,
                ),
            )
        }
        ast::types::ComponentRendersAnnotation::MissingRenders(loc) => {
            let reason = mk_annot_reason(
                VirtualReasonDesc::RRenderType(Arc::new(VirtualReasonDesc::RType(Name::new(
                    FlowSmolStr::new("React.Node"),
                )))),
                loc.dupe(),
            );
            let renders_t = Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::RendersT(Rc::new(
                    type_::CanonicalRendersForm::DefaultRenders,
                ))),
            ));
            (
                loc.dupe(),
                renders_t.dupe(),
                ast::types::ComponentRendersAnnotation::MissingRenders((loc.dupe(), renders_t)),
            )
        }
    };
    let ast::IdentifierInner {
        loc: ref id_loc,
        ref name,
        ref comments,
    } = **id;
    let id_loc = id_loc.dupe();
    let name = name.dupe();
    let name_ast_comments = comments.clone();
    let body_value = match body {
        None => (
            params.loc.dupe(),
            statement::Block {
                body: vec![].into(),
                comments: None,
            },
        ),
        Some(body) => body.clone(),
    };
    let component_sig = flow_typing_loc_env::component_sig_types::component_sig::ComponentSig {
        reason,
        tparams,
        cparams,
        body: body_value,
        renders_t,
        ret_annot_loc: ret_loc,
        id_opt: Some((id_loc.dupe(), name.dupe())),
    };
    let component_clone = component.clone();
    let reconstruct: flow_typing_loc_env::component_sig_types::component_sig::ReconstructComponent =
        Rc::new(
            move |params_ast, body_ast, component_type| statement::ComponentDeclaration {
                id: ast::Identifier::new(ast::IdentifierInner {
                    loc: (id_loc.dupe(), component_type),
                    name: name.dupe(),
                    comments: name_ast_comments.clone(),
                }),
                params: params_ast,
                body: body_ast,
                renders: renders_ast.clone(),
                tparams: tparams_ast_typed.clone(),
                sig_loc: component_clone.sig_loc.dupe(),
                comments: component_clone.comments.dupe(),
                async_: component_clone.async_,
            },
        );
    cache.set_component_sig(sig_loc, (component_sig.clone(), reconstruct.dupe()));
    (component_sig, reconstruct)
}

/// A deferred type guard check that must be invoked after `toplevels` has
/// checked the function body. In OCaml this runs inside `reconstruct_ast`, but
/// in Rust `Reconstruct` is `'static` and cannot capture `Context<'a>`.
pub type DeferredTypeGuardCheck<'a> =
    flow_typing_loc_env::node_cache::DeferredTypeGuardCheck<'a, Context<'a>>;

pub fn mk_func_sig<'a>(
    cx: &Context<'a>,
    require_return_annot: bool,
    constructor: bool,
    getset: bool,
    statics: &BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
    tparams_map: &FlowOrdMap<SubstName, Type>,
    reason: Reason,
    func: &ast::function::Function<ALoc, ALoc>,
) -> Result<
    (
        func_class_sig_types::func::Func<func_class_sig_types::StmtConfigTypes>,
        func_class_sig_types::func::Reconstruct,
        DeferredTypeGuardCheck<'a>,
    ),
    AbnormalControlFlow,
> {
    use flow_typing_loc_env::func_class_sig_types::func;
    use flow_typing_loc_env::func_stmt_config_types;

    fn function_kind(
        constructor: bool,
        async_: bool,
        generator: bool,
        ret_loc: ALoc,
    ) -> func::Kind {
        match (constructor, async_, generator) {
            (true, _, _) => func::Kind::Ctor,
            (false, true, true) => func::Kind::AsyncGenerator {
                return_loc: ret_loc,
            },
            (false, true, false) => func::Kind::Async,
            (false, false, true) => func::Kind::Generator {
                return_loc: ret_loc,
            },
            (false, false, false) => func::Kind::Ordinary,
        }
    }

    fn mk_param_annot<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        reason: Reason,
        annot: &ast::types::AnnotationOrHint<ALoc, ALoc>,
    ) -> (Type, ast::types::AnnotationOrHint<ALoc, (ALoc, Type)>) {
        match annot {
            ast::types::AnnotationOrHint::Missing(loc)
                if !matches!(
                    &*cx.typing_mode(),
                    flow_typing_context::TypingMode::CheckingMode
                ) =>
            {
                let t = cx.mk_placeholder(reason);
                (
                    t.dupe(),
                    ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                )
            }
            ast::types::AnnotationOrHint::Missing(loc) => {
                let t = type_env::find_write(
                    cx,
                    flow_env_builder::env_api::DefLocType::FunctionParamLoc,
                    reason,
                );
                (
                    t.dupe(),
                    ast::types::AnnotationOrHint::Missing((loc.dupe(), t)),
                )
            }
            ast::types::AnnotationOrHint::Available(annot) => {
                let (t, ast_annot) =
                    type_annotation::mk_type_available_annotation(cx, tparams_map.dupe(), annot);
                (t, ast::types::AnnotationOrHint::Available(ast_annot))
            }
        }
    }

    fn id_param<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        id: &ast::pattern::Identifier<ALoc, ALoc>,
        mk_reason_fn: impl FnOnce(FlowSmolStr) -> Reason,
    ) -> (Type, ast::pattern::Identifier<ALoc, (ALoc, Type)>) {
        let ast::pattern::Identifier {
            name,
            annot,
            optional,
        } = id;
        let id_loc = name.loc.dupe();
        let name_str = name.name.dupe();
        let reason = mk_reason_fn(name_str);
        let (t, typed_annot) = mk_param_annot(cx, tparams_map, reason, annot);
        let typed_name = ast::Identifier::new(ast::IdentifierInner {
            loc: (id_loc, t.dupe()),
            name: name.name.dupe(),
            comments: name.comments.dupe(),
        });
        (
            t,
            ast::pattern::Identifier {
                name: typed_name,
                annot: typed_annot,
                optional: *optional,
            },
        )
    }

    fn mk_param<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        param: &ast::function::Param<ALoc, ALoc>,
    ) -> func_stmt_config_types::Param {
        match param {
            ast::function::Param::ParamProperty {
                loc,
                property: prop,
            } => {
                let loc = loc.dupe();
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: TSSyntaxKind::TSParameterProperty,
                        loc: loc.dupe(),
                    })),
                );
                func_stmt_config_types::Param {
                    t: any_t::make(
                        type_::AnySource::AnyError(None),
                        mk_reason(VirtualReasonDesc::RParameter(None), loc.dupe()),
                    ),
                    loc: loc.dupe(),
                    ploc: loc,
                    pattern: func_stmt_config_types::Pattern::ParamPropertyPattern({
                        let Ok(v) = polymorphic_ast_mapper::class_property(
                            &mut typed_ast_utils::ErrorMapper,
                            prop,
                        );
                        v
                    }),
                    default: None,
                    has_anno: true,
                }
            }
            ast::function::Param::RegularParam {
                loc,
                argument,
                default,
            } => {
                let loc = loc.dupe();
                let ploc = argument.loc().dupe();
                let has_param_anno = match crate::destructuring::type_of_pattern(argument) {
                    ast::types::AnnotationOrHint::Missing(_) => false,
                    ast::types::AnnotationOrHint::Available(_) => true,
                };
                let (t, pattern) = match argument {
                    ast::pattern::Pattern::Identifier { inner: id, .. } => {
                        let ploc_clone = ploc.dupe();
                        let (t, typed_id) = id_param(cx, tparams_map, id, move |name| {
                            mk_reason(VirtualReasonDesc::RParameter(Some(name)), ploc_clone)
                        });
                        (t, func_stmt_config_types::Pattern::Id(typed_id))
                    }
                    ast::pattern::Pattern::Object { inner, .. } => {
                        let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                        let (t, typed_annot) =
                            mk_param_annot(cx, tparams_map, reason, &inner.annot);
                        (
                            t,
                            func_stmt_config_types::Pattern::Object {
                                annot: typed_annot,
                                properties: inner.properties.to_vec(),
                                optional: inner.optional,
                                comments: inner.comments.dupe(),
                            },
                        )
                    }
                    ast::pattern::Pattern::Array { inner, .. } => {
                        let reason = mk_reason(VirtualReasonDesc::RDestructuring, ploc.dupe());
                        let (t, typed_annot) =
                            mk_param_annot(cx, tparams_map, reason, &inner.annot);
                        (
                            t,
                            func_stmt_config_types::Pattern::Array {
                                annot: typed_annot,
                                elements: inner.elements.to_vec(),
                                optional: inner.optional,
                                comments: inner.comments.dupe(),
                            },
                        )
                    }
                    ast::pattern::Pattern::Expression { .. } => {
                        panic!("unexpected expression pattern in param")
                    }
                };
                func_stmt_config_types::Param {
                    t,
                    loc,
                    ploc,
                    pattern,
                    default: default.clone(),
                    has_anno: has_param_anno,
                }
            }
        }
    }

    fn mk_rest<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        rest: &ast::function::RestParam<ALoc, ALoc>,
    ) -> Result<func_stmt_config_types::Rest, Box<ErrorMessage<ALoc>>> {
        let ast::function::RestParam {
            loc,
            argument,
            comments: _,
        } = rest;
        let loc = loc.dupe();
        let ploc = argument.loc().dupe();
        let has_param_anno = match crate::destructuring::type_of_pattern(argument) {
            ast::types::AnnotationOrHint::Missing(_) => false,
            ast::types::AnnotationOrHint::Available(_) => true,
        };
        match argument {
            ast::pattern::Pattern::Identifier { inner: id, .. } => {
                let ploc_clone = ploc.dupe();
                let (t, typed_id) = id_param(cx, tparams_map, id, move |name| {
                    mk_reason(VirtualReasonDesc::RRestParameter(Some(name)), ploc_clone)
                });
                Ok(func_stmt_config_types::Rest {
                    t,
                    loc,
                    ploc,
                    id: typed_id,
                    has_anno: has_param_anno,
                })
            }
            ast::pattern::Pattern::Object { .. }
            | ast::pattern::Pattern::Array { .. }
            | ast::pattern::Pattern::Expression { .. } => {
                // this should be a parse error, unrepresentable AST
                Err(Box::new(ErrorMessage::EInternal(Box::new((
                    ploc,
                    InternalError::RestParameterNotIdentifierPattern,
                )))))
            }
        }
    }

    fn mk_this<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        this: &ast::function::ThisParam<ALoc, ALoc>,
    ) -> func_stmt_config_types::ThisParam {
        let ast::function::ThisParam {
            loc,
            annot,
            comments: _,
        } = this;
        let loc = loc.dupe();
        let annot_loc = annot.loc.dupe();
        let annot_type = &annot.annotation;
        let typed_annot = type_annotation::convert(cx, tparams_map.dupe(), annot_type);
        let t = typed_annot.loc().1.dupe();
        func_stmt_config_types::ThisParam {
            t,
            loc,
            annot: ast::types::Annotation {
                loc: annot_loc,
                annotation: typed_annot,
            },
        }
    }

    fn mk_params<'a>(
        cx: &Context<'a>,
        tparams_map: &FlowOrdMap<SubstName, Type>,
        params: &ast::function::Params<ALoc, ALoc>,
    ) -> func_class_sig_types::func::FuncParams {
        let ast::function::Params {
            loc,
            params: param_list,
            rest,
            this_,
            comments,
        } = params;
        let loc = loc.dupe();
        let comments = comments.clone();
        let comments_clone = comments.clone();
        let loc_clone = loc.dupe();
        let mut fparams =
            crate::func_params::empty(Rc::new(move |typed_params, typed_rest, typed_this| {
                Some(ast::function::Params {
                    loc: loc_clone.dupe(),
                    params: typed_params.into(),
                    rest: typed_rest,
                    this_: typed_this,
                    comments: comments_clone.clone(),
                })
            }));
        for param in param_list.iter() {
            let p = mk_param(cx, tparams_map, param);
            crate::func_params::add_param(p, &mut fparams);
        }
        if let Some(rest) = rest {
            match mk_rest(cx, tparams_map, rest) {
                Ok(rest) => crate::func_params::add_rest(rest, &mut fparams),
                Err(err) => {
                    flow_js::add_output_non_speculating(cx, *err);
                }
            }
        }
        if let Some(this) = this_ {
            let this_param = mk_this(cx, tparams_map, this);
            crate::func_params::add_this(this_param, &mut fparams);
        }
        fparams
    }

    let sig_loc = func.sig_loc.dupe();
    let cache = cx.node_cache();
    if let Some(x) = cache.get_function_sig(&sig_loc) {
        return Ok((x.0, x.1, x.2));
    }
    let ast::function::Function {
        tparams: func_tparams,
        return_,
        body,
        predicate,
        params,
        id,
        async_,
        generator,
        effect_,
        sig_loc: _,
        comments: _,
    } = func;
    let loc = reason.loc().dupe();
    let ret_loc = match return_ {
        ast::function::ReturnAnnot::Available(annot) => annot.loc.dupe(),
        ast::function::ReturnAnnot::TypeGuard(guard) => guard.loc.dupe(),
        ast::function::ReturnAnnot::Missing(loc) => loc.dupe(),
    };
    let kind = function_kind(constructor, *async_, *generator, ret_loc.dupe());
    type_annotation::error_on_unsupported_variance_annotation(
        cx,
        "function",
        func_tparams.as_ref(),
    );
    let (tparams, tparams_map, tparams_ast) = type_annotation::mk_type_param_declarations(
        cx,
        flow_parser::ast_visitor::TypeParamsContext::Function,
        Some(
            tparams_map
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect(),
        ),
        func_tparams.as_ref(),
    );
    let fparams = mk_params(cx, &tparams_map, params);
    let ret_reason = mk_reason(
        VirtualReasonDesc::RReturn,
        crate::func_sig::return_loc(func),
    );
    let has_nonvoid_return =
        flow_env_builder::nonvoid_return::might_have_nonvoid_return(&loc, func)
            || (!matches!(
                kind,
                func::Kind::Ordinary | func::Kind::Async | func::Kind::Ctor
            ));
    let type_guard_incompatible = match &kind {
        func::Kind::Async
        | func::Kind::Generator { .. }
        | func::Kind::AsyncGenerator { .. }
        | func::Kind::FieldInit(_)
        | func::Kind::Ctor => Some(func::string_of_kind(&kind)),
        _ if getset => Some("getter/setter"),
        _ => None,
    };
    let from_generic_function = tparams.is_some();
    let require_return_annot = require_return_annot || from_generic_function;
    if let Some(id) = id {
        hook_check(cx, *effect_, id);
    }
    let (return_t, return_ast, type_guard_opt) = match (return_, type_guard_incompatible) {
        (ast::function::ReturnAnnot::TypeGuard(guard_annot), Some(incompat_kind)) => {
            let guard_loc = guard_annot.guard.loc.dupe();
            let return_t = type_::bool_module_t::at(guard_loc.dupe());
            let Ok(return_ast) = polymorphic_ast_mapper::function_return_annotation(
                &mut typed_ast_utils::ErrorMapper,
                return_,
            );
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::ETypeGuardIncompatibleWithFunctionKind(Box::new(
                    ETypeGuardIncompatibleWithFunctionKindData {
                        loc: guard_loc,
                        kind: FlowSmolStr::new(incompat_kind),
                    },
                )),
            );
            (AnnotatedOrInferred::Annotated(return_t), return_ast, None)
        }
        (ast::function::ReturnAnnot::Missing(loc), _) if !has_nonvoid_return => {
            let void_t = type_::void::why(ret_reason.dupe());
            let t = if matches!(kind, func::Kind::Async) {
                let promise_reason = mk_annot_reason(
                    VirtualReasonDesc::RType(Name::new(FlowSmolStr::new("Promise"))),
                    ret_reason.loc().dupe(),
                );
                { FlowJs::get_builtin_typeapp(cx, &promise_reason, None, "Promise", vec![void_t]) }
            } else {
                void_t
            };
            (
                AnnotatedOrInferred::Inferred(t.dupe()),
                ast::function::ReturnAnnot::Missing((loc.dupe(), t)),
                None,
            )
        }
        (ast::function::ReturnAnnot::Missing(loc), _)
            if has_nonvoid_return && require_return_annot =>
        {
            let reason = ret_reason.dupe().reposition(ret_loc.dupe());
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMissingLocalAnnotation {
                    reason: reason.dupe(),
                    hint_available: false,
                    from_generic_function,
                },
            );
            let t = any_t::make(
                type_::AnySource::AnyError(Some(type_::AnyErrorKind::MissingAnnotation)),
                reason,
            );
            (
                AnnotatedOrInferred::Inferred(t.dupe()),
                ast::function::ReturnAnnot::Missing((loc.dupe(), t)),
                None,
            )
        }
        (ast::function::ReturnAnnot::Missing(loc), _) if matches!(kind, func::Kind::Ctor) => {
            let t = type_::void::make(mk_reason(
                VirtualReasonDesc::RConstructorVoidReturn,
                ret_loc.dupe(),
            ));
            (
                AnnotatedOrInferred::Inferred(t.dupe()),
                ast::function::ReturnAnnot::Missing((loc.dupe(), t)),
                None,
            )
        }
        // TODO we could probably take the same shortcut for functions with an explicit `void` annotation
        // and no explicit returns
        (ast::function::ReturnAnnot::Missing(loc), _) => {
            let t = if !matches!(
                &*cx.typing_mode(),
                flow_typing_context::TypingMode::CheckingMode
            ) {
                cx.mk_placeholder(ret_reason.dupe())
            } else {
                flow_typing_tvar::mk(cx, ret_reason.dupe())
            };
            let type_guard_opt = flow_typing_utils::type_guard::infer_type_guard(
                cx,
                &|cx, expr: &expression::Expression<ALoc, ALoc>| {
                    expression(None, None, None, cx, expr)
                },
                params,
            )?;
            (
                AnnotatedOrInferred::Inferred(t.dupe()),
                ast::function::ReturnAnnot::Missing((loc.dupe(), t)),
                type_guard_opt,
            )
        }
        (ast::function::ReturnAnnot::Available(annot), _) => {
            let (t, ast_annot) =
                type_annotation::mk_type_available_annotation(cx, tparams_map.dupe(), annot);
            (
                AnnotatedOrInferred::Annotated(t),
                ast::function::ReturnAnnot::Available(ast_annot),
                None,
            )
        }
        (ast::function::ReturnAnnot::TypeGuard(guard_annot), None) => {
            let gloc = guard_annot.guard.loc.dupe();
            let guard_kind = guard_annot.guard.kind;
            let (ref id_name, ref guard_type_opt) = guard_annot.guard.guard;
            let guard_comments = guard_annot.guard.comments.as_ref();
            match (guard_kind, guard_type_opt) {
                (kind, Some(t))
                    if kind == ast::types::TypeGuardKind::Default
                        || kind == ast::types::TypeGuardKind::Implies =>
                {
                    let fparams_value = crate::func_params::value::<
                        crate::func_params::FuncStmtConfig,
                    >(&fparams.params);
                    let (bool_t, guard_ast, predicate_opt) = type_annotation::convert_type_guard(
                        cx,
                        tparams_map.dupe(),
                        &fparams_value,
                        gloc,
                        kind,
                        id_name,
                        t,
                        guard_comments,
                    );
                    (
                        AnnotatedOrInferred::Annotated(bool_t),
                        ast::function::ReturnAnnot::TypeGuard(ast::types::TypeGuardAnnotation {
                            loc: guard_annot.loc.dupe(),
                            guard: guard_ast,
                        }),
                        predicate_opt,
                    )
                }
                _ => {
                    let Ok(t_ast) = polymorphic_ast_mapper::type_guard_annotation(
                        &mut typed_ast_utils::ErrorMapper,
                        guard_annot,
                    );
                    let error_loc = t_ast.guard.loc.dupe();
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            error_loc.dupe(),
                            UnsupportedSyntax::UserDefinedTypeGuards { kind: guard_kind },
                        ))),
                    );
                    (
                        AnnotatedOrInferred::Annotated(any_t::at(
                            type_::AnySource::AnyError(None),
                            error_loc,
                        )),
                        ast::function::ReturnAnnot::TypeGuard(t_ast),
                        None,
                    )
                }
            }
        }
    };
    // Now that we've seen the return annotation we might need to update `kind`.
    let kind = match type_guard_opt {
        Some(p) => func::Kind::TypeGuard(p),
        None => kind,
    };
    let (return_t, predicate) = match predicate {
        Some(pred) => {
            let pred_loc = pred.loc.dupe();
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    pred_loc,
                    UnsupportedSyntax::PredicateFunction,
                ))),
            );
            let Ok(mapped_pred) =
                polymorphic_ast_mapper::predicate(&mut typed_ast_utils::ErrorMapper, pred);
            (return_t, Some(mapped_pred))
        }
        None => (return_t, None),
    };
    match (&return_ast, &kind) {
        (ast::function::ReturnAnnot::Missing(_), func::Kind::Ctor) => {}
        (_, func::Kind::Ctor) => {
            let return_type = type_t_of_annotated_or_inferred(&return_t);
            let use_op = UseOp::Op(Arc::new(type_::RootUseOp::FunReturnStatement {
                value: reason_of_t(return_type).dupe(),
            }));
            cx.add_post_inference_subtyping_check(
                return_type.dupe(),
                use_op,
                type_::void::make(mk_reason(
                    VirtualReasonDesc::RConstructorVoidReturn,
                    ret_loc.dupe(),
                )),
            );
        }
        _ => {}
    }
    let return_t =
        mk_inference_target_with_annots(type_env::has_hint(cx, ret_loc.dupe()), return_t);
    let statics_t = {
        let props: properties::PropertiesMap = statics
            .iter()
            .map(|(name, env_key)| {
                let expr_t = type_env::find_write(
                    cx,
                    env_key.def_loc_type,
                    mk_reason(
                        VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                        env_key.loc.dupe(),
                    ),
                );
                let field =
                    type_::Property::new(type_::PropertyInner::Field(Box::new(FieldData {
                        preferred_def_locs: None,
                        key_loc: Some(env_key.loc.dupe()),
                        type_: expr_t,
                        polarity: Polarity::Neutral,
                    })));
                (Name::new(name.dupe()), field)
            })
            .collect();
        obj_type::mk_with_proto(
            cx,
            reason.dupe(),
            type_::ObjKind::Inexact,
            None,
            None,
            Some(props),
            None,
            Type::new(TypeInner::FunProtoT(reason.dupe())),
        )
    };
    let effect = match effect_ {
        ast::function::Effect::Hook => type_::ReactEffectType::HookDecl(cx.make_aloc_id(&loc)),
        ast::function::Effect::Arbitrary => type_::ReactEffectType::ArbitraryEffect,
    };
    let func_stmt_sig = func::Func {
        reason: reason.dupe(),
        kind: kind.clone(),
        tparams,
        fparams: fparams.clone(),
        body: Some(body.clone()),
        return_t,
        ret_annot_loc: ret_loc.dupe(),
        statics: Some(statics_t),
        effect_: effect,
    };
    let id_clone = id.clone();
    let predicate_clone = predicate.clone();
    let return_ast_clone = return_ast;
    let tparams_ast_clone = tparams_ast;
    let func_clone = func.clone();
    // check_type_guard needs cx which has lifetime 'a, but Reconstruct is 'static
    // (Rc<dyn Fn(...)>). We extract the check into a DeferredTypeGuardCheck that
    // callers invoke after toplevels, preserving the OCaml ordering where
    // check_type_guard runs inside reconstruct_ast (which is called after toplevels).
    let cx_typing_mode_is_checking = matches!(
        &*cx.typing_mode(),
        flow_typing_context::TypingMode::CheckingMode
    );
    let deferred_type_guard_check: DeferredTypeGuardCheck<'a> = if cx_typing_mode_is_checking {
        match &kind {
            func::Kind::TypeGuard(tg) => {
                let params_clone = params.clone();
                let tg_clone = tg.clone();
                Some(Rc::new(move |cx: &Context<'a>| {
                    flow_typing_utils::type_guard::check_type_guard(cx, &params_clone, &tg_clone);
                }))
            }
            _ => None,
        }
    } else {
        None
    };
    let reconstruct_ast: func_class_sig_types::func::Reconstruct = Rc::new(
        move |params_tast, body_tast, fun_type| ast::function::Function {
            id: id_clone.as_ref().map(|id| {
                ast::Identifier::new(ast::IdentifierInner {
                    loc: (id.loc.dupe(), fun_type.dupe()),
                    name: id.name.dupe(),
                    comments: id.comments.dupe(),
                })
            }),
            params: params_tast,
            body: body_tast,
            predicate: predicate_clone.clone(),
            return_: return_ast_clone.clone(),
            tparams: tparams_ast_clone.clone(),
            async_: func_clone.async_,
            generator: func_clone.generator,
            effect_: func_clone.effect_,
            sig_loc: func_clone.sig_loc.dupe(),
            comments: func_clone.comments.dupe(),
        },
    );
    Ok((func_stmt_sig, reconstruct_ast, deferred_type_guard_check))
}

/// Given a function declaration and types for `this` and `super`, extract a
/// signature consisting of type parameters, parameter types, parameter names,
/// and return type, check the body against that signature by adding `this`
/// and super` to the environment, and return the signature.
fn function_decl<'a>(
    cx: &Context<'a>,
    fun_loc: Option<ALoc>,
    arrow: bool,
    statics: &BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
    reason: Reason,
    func: &ast::function::Function<ALoc, ALoc>,
    default_this: Option<Type>,
) -> Result<
    (
        Type,
        Box<dyn FnOnce(Type) -> ast::function::Function<ALoc, (ALoc, Type)>>,
    ),
    AbnormalControlFlow,
> {
    let (func_sig, reconstruct_func, deferred_tg_check) = mk_func_sig(
        cx,
        false,
        false,
        false,
        statics,
        &FlowOrdMap::new(),
        reason.dupe(),
        func,
    )?;
    let default_this = match default_this {
        Some(t) => t,
        None => type_::dummy_this(reason.loc().dupe()),
    };
    let fun_type = crate::func_sig::functiontype(cx, arrow, fun_loc, default_this, &func_sig);
    if !matches!(
        &*cx.typing_mode(),
        flow_typing_context::TypingMode::CheckingMode
    ) {
        let mut mapper = typed_ast_utils::PlaceholderMapper::new(cx);
        let Ok(params_ast) = polymorphic_ast_mapper::function_params(&mut mapper, &func.params);
        let Ok(body_ast) = polymorphic_ast_mapper::function_body_any(&mut mapper, &func.body);
        Ok((
            fun_type,
            Box::new(move |fun_type_arg: Type| {
                reconstruct_func(params_ast, body_ast, fun_type_arg)
            }),
        ))
    } else {
        let (params_ast, body_ast, _) = crate::func_sig::toplevels(cx, &func_sig)?;
        // Invoke deferred type guard check after toplevels (same timing as OCaml)
        if let Some(check) = deferred_tg_check {
            check(cx);
        }
        Ok((
            fun_type,
            Box::new(move |fun_type_arg: Type| {
                reconstruct_func(
                    params_ast.expect("params_ast should be Some"),
                    body_ast.expect("body_ast should be Some"),
                    fun_type_arg,
                )
            }),
        ))
    }
}

/// Process a function declaration, returning a (polymorphic) function type
fn mk_function_declaration<'a>(
    cx: &Context<'a>,
    tast_fun_type: Option<Type>,
    reason: Reason,
    fun_loc: ALoc,
    func: &ast::function::Function<ALoc, ALoc>,
) -> Result<(Type, ast::function::Function<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    mk_function(
        cx,
        true,
        tast_fun_type,
        &BTreeMap::new(),
        reason,
        fun_loc,
        func,
    )
}

/// Process a function expression, returning a (polymorphic) function type.
fn mk_function_expression<'a>(
    cx: &Context<'a>,
    needs_this_param: bool,
    reason: Reason,
    fun_loc: ALoc,
    func: &ast::function::Function<ALoc, ALoc>,
) -> Result<(Type, ast::function::Function<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    mk_function(
        cx,
        needs_this_param,
        None,
        &BTreeMap::new(),
        reason,
        fun_loc,
        func,
    )
}

/// Internal helper function. Use `mk_function_declaration` and `mk_function_expression` instead.
pub fn mk_function<'a>(
    cx: &Context<'a>,
    needs_this_param: bool,
    tast_fun_type: Option<Type>,
    statics: &BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
    reason: Reason,
    fun_loc: ALoc,
    func: &ast::function::Function<ALoc, ALoc>,
) -> Result<(Type, ast::function::Function<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    let node_cache = cx.node_cache();
    let cache_loc = func
        .id
        .as_ref()
        .map(|id| id.loc.dupe())
        .unwrap_or_else(|| fun_loc.dupe());
    match node_cache.get_function(&cache_loc) {
        Some(cached) => {
            flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                vec![format!(
                    "Function cache hit at {}",
                    reason.loc().debug_to_string(false)
                )]
            });
            Ok(cached)
        }
        None => {
            let default_this = flow_js_utils::default_this_type(cx, needs_this_param, func)
                .expect("Should not be under speculation");
            let fun_loc_arg = if needs_this_param {
                Some(fun_loc)
            } else {
                None
            };
            let default_this_arg = if needs_this_param {
                Some(default_this)
            } else {
                None
            };
            let (fun_type, reconstruct_ast) = function_decl(
                cx,
                fun_loc_arg,
                false,
                statics,
                reason,
                func,
                default_this_arg,
            )?;
            let tast_fun_type = tast_fun_type.unwrap_or_else(|| fun_type.dupe());
            Ok((fun_type, reconstruct_ast(tast_fun_type)))
        }
    }
}

/// Process an arrow function, returning a (polymorphic) function type.
pub fn mk_arrow<'a>(
    cx: &Context<'a>,
    statics: &BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
    reason: Reason,
    func: &ast::function::Function<ALoc, ALoc>,
) -> Result<(Type, ast::function::Function<ALoc, (ALoc, Type)>), AbnormalControlFlow> {
    // Do not expose the type of `this` in the function's type. This call to
    // function_decl has already done the necessary checking of `this` in
    // the body of the function. Now we want to avoid re-binding `this` to
    // objects through which the function may be called.
    let default_this = None;
    let (fun_type, reconstruct_ast) =
        function_decl(cx, None, true, statics, reason, func, default_this)?;
    Ok((fun_type.dupe(), reconstruct_ast(fun_type)))
}

fn check_default_pattern<'a>(
    cx: &Context<'a>,
    left_loc: ALoc,
    right: &expression::Expression<ALoc, ALoc>,
) {
    let right_loc = right.loc().dupe();
    let update_excuses =
        |update_fun: Box<dyn FnOnce(&mut flow_typing_exists_check::ExistsCheck)>| {
            let mut exists_excuses = cx.exists_excuses_mut();
            let exists_excuse = exists_excuses
                .entry(left_loc.dupe())
                .or_insert_with(Default::default);
            update_fun(exists_excuse);
        };
    match right.deref() {
        expression::ExpressionInner::StringLiteral { inner, .. } if inner.value.is_empty() => {
            update_excuses(Box::new(|excuse| {
                excuse.string_loc = Some(right_loc.dupe());
            }));
        }
        expression::ExpressionInner::BooleanLiteral { inner, .. } if !inner.value => {
            update_excuses(Box::new(|excuse| {
                excuse.bool_loc = Some(right_loc.dupe());
            }));
        }
        expression::ExpressionInner::NumberLiteral { inner, .. } if inner.value == 0.0 => {
            update_excuses(Box::new(|excuse| {
                excuse.number_loc = Some(right_loc.dupe());
            }));
        }
        expression::ExpressionInner::BigIntLiteral { inner, .. } if inner.value == Some(0i64) => {
            update_excuses(Box::new(|excuse| {
                excuse.bigint_loc = Some(right_loc.dupe());
            }));
        }
        // There's no valid default value for mixed to create an excuse.
        _ => {}
    }
}

fn is_valid_enum_member_name(name: &str) -> bool {
    !name.is_empty() && !name.as_bytes()[0].is_ascii_lowercase()
}

fn flow_enum_exhaustive_check_incomplete<'cx>(
    cx: &Context<'cx>,
    check_reason: &Reason,
    trigger: Option<&Type>,
    exhaustive_check_incomplete_out: &Type,
) -> Result<(), FlowJsException> {
    let default_trigger = void::why(check_reason.dupe());
    let trigger = trigger.unwrap_or(&default_trigger);
    FlowJs::flow(
        cx,
        trigger,
        &UseT::new(UseTInner::UseT(
            unknown_use(),
            exhaustive_check_incomplete_out.dupe(),
        )),
    )?;
    Ok(())
}

fn check_possible_enum_exhaustive_check<'cx>(
    cx: &Context<'cx>,
    check_reason: &Reason,
    possible_checks: &std::collections::VecDeque<(Type, EnumCheck)>,
    checks: &Rc<[EnumCheck]>,
    default_case_loc: Option<ALoc>,
    incomplete_out: &Type,
    discriminant_after_check: Option<&Type>,
    discriminant_t: &Type,
) -> Result<(), FlowJsException> {
    let concrete_discriminants = FlowJs::possible_concrete_types_for_enum_exhaustive_check(
        cx,
        check_reason,
        discriminant_t,
    )?;
    for concrete in &concrete_discriminants {
        match concrete.deref() {
            TypeInner::IntersectionT(_, rep) => {
                let cases: Vec<_> = rep
                    .members_iter()
                    .map(|member| {
                        let member = member.dupe();
                        let check_reason = check_reason.dupe();
                        let possible_checks = possible_checks.clone();
                        let checks = checks.clone();
                        let default_case_loc = default_case_loc.dupe();
                        let incomplete_out = incomplete_out.dupe();
                        let discriminant_after_check = discriminant_after_check.map(|t| t.dupe());
                        Box::new(move |cx: &Context<'cx>| {
                            check_possible_enum_exhaustive_check(
                                cx,
                                &check_reason,
                                &possible_checks,
                                &checks,
                                default_case_loc,
                                &incomplete_out,
                                discriminant_after_check.as_ref(),
                                &member,
                            )
                        })
                            as Box<dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + '_>
                    })
                    .collect();
                speculation_flow::try_custom(
                    cx,
                    None,
                    None,
                    None,
                    check_reason.loc().dupe(),
                    cases,
                )?;
            }
            TypeInner::DefT(enum_reason, def_t)
                if let DefTInner::EnumValueT(info) = def_t.deref()
                    && let EnumInfoInner::ConcreteEnum(enum_info) = EnumInfo::deref(info) =>
            {
                perform_enum_exhaustive_check(
                    cx,
                    check_reason,
                    enum_reason,
                    enum_info,
                    possible_checks,
                    checks,
                    default_case_loc.dupe(),
                    incomplete_out,
                )?;
            }
            TypeInner::DefT(enum_reason, def_t) if matches!(def_t.deref(), DefTInner::EnumValueT(info) if matches!(EnumInfo::deref(info), EnumInfoInner::AbstractEnum { .. })) =>
            {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidAbstractUse(Box::new(
                        flow_typing_errors::error_message::EnumInvalidAbstractUseData {
                            reason: check_reason.dupe(),
                            enum_reason: enum_reason.dupe(),
                        },
                    ))),
                )?;
            }
            TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::EmptyT) => {}
            _ => {
                flow_enum_exhaustive_check_incomplete(
                    cx,
                    check_reason,
                    discriminant_after_check,
                    incomplete_out,
                )?;
            }
        }
    }
    Ok(())
}

fn check_invalid_enum_exhaustive_check<'cx>(
    cx: &Context<'cx>,
    check_reason: &Reason,
    reasons: &Rc<[ALoc]>,
    incomplete_out: &Type,
    discriminant_after_check: Option<&Type>,
    discriminant_t: &Type,
) -> Result<(), FlowJsException> {
    let concrete_discriminants = FlowJs::possible_concrete_types_for_enum_exhaustive_check(
        cx,
        check_reason,
        discriminant_t,
    )?;
    for concrete in &concrete_discriminants {
        match concrete.deref() {
            TypeInner::IntersectionT(_, rep) => {
                let cases: Vec<_> = rep
                    .members_iter()
                    .map(|member| {
                        let member = member.dupe();
                        let check_reason = check_reason.dupe();
                        let reasons = reasons.clone();
                        let incomplete_out = incomplete_out.dupe();
                        let discriminant_after_check = discriminant_after_check.map(|t| t.dupe());
                        Box::new(move |cx: &Context<'cx>| {
                            check_invalid_enum_exhaustive_check(
                                cx,
                                &check_reason,
                                &reasons,
                                &incomplete_out,
                                discriminant_after_check.as_ref(),
                                &member,
                            )
                        })
                            as Box<dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + '_>
                    })
                    .collect();
                speculation_flow::try_custom(
                    cx,
                    None,
                    None,
                    None,
                    check_reason.loc().dupe(),
                    cases,
                )?;
            }
            TypeInner::DefT(enum_reason, def_t)
                if let DefTInner::EnumValueT(enum_info) = def_t.deref() =>
            {
                let example_member = match EnumInfo::deref(enum_info) {
                    EnumInfoInner::ConcreteEnum(concrete) => {
                        concrete.members.keys().next().map(|k| k.dupe())
                    }
                    EnumInfoInner::AbstractEnum { .. } => None,
                };
                for loc in reasons.iter() {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidCheck(Box::new(
                            flow_typing_errors::error_message::EnumInvalidCheckData {
                                loc: loc.dupe(),
                                enum_reason: enum_reason.dupe(),
                                example_member: example_member.dupe(),
                                from_match: false,
                            },
                        ))),
                    )?;
                }
                flow_enum_exhaustive_check_incomplete(cx, check_reason, None, incomplete_out)?;
            }
            TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::EmptyT) => {}
            _ => {
                flow_enum_exhaustive_check_incomplete(
                    cx,
                    check_reason,
                    discriminant_after_check,
                    incomplete_out,
                )?;
            }
        }
    }
    Ok(())
}

fn enum_case_test_matches_discriminant<'cx>(
    cx: &Context<'cx>,
    enum_id_discriminant: &flow_aloc::ALocId,
    members: &FlowOrdMap<FlowSmolStr, ALoc>,
    member_name: &FlowSmolStr,
    obj_t: &Type,
) -> bool {
    fn matches_single<'cx>(
        cx: &Context<'cx>,
        enum_id_discriminant: &flow_aloc::ALocId,
        members: &FlowOrdMap<FlowSmolStr, ALoc>,
        member_name: &FlowSmolStr,
        t: &Type,
    ) -> bool {
        match t.deref() {
            TypeInner::DefT(_, inner_def)
                if let DefTInner::EnumObjectT { enum_info: ei, .. } = inner_def.deref()
                    && let EnumInfoInner::ConcreteEnum(concrete) = EnumInfo::deref(ei)
                    && concrete.enum_id == *enum_id_discriminant
                    && members.contains_key(member_name) =>
            {
                true
            }
            TypeInner::IntersectionT(_, rep) => rep
                .members_iter()
                .any(|member| matches(cx, enum_id_discriminant, members, member_name, member)),
            _ => false,
        }
    }
    fn matches<'cx>(
        cx: &Context<'cx>,
        enum_id_discriminant: &flow_aloc::ALocId,
        members: &FlowOrdMap<FlowSmolStr, ALoc>,
        member_name: &FlowSmolStr,
        t: &Type,
    ) -> bool {
        match FlowJs::possible_concrete_types_for_enum_exhaustive_check(
            cx,
            &reason_of_t(t).dupe(),
            t,
        ) {
            Ok(ts) if ts.len() == 1 => {
                matches_single(cx, enum_id_discriminant, members, member_name, &ts[0])
            }
            _ => false,
        }
    }
    matches(cx, enum_id_discriminant, members, member_name, obj_t)
}

fn perform_enum_exhaustive_check<'cx>(
    cx: &Context<'cx>,
    check_reason: &Reason,
    enum_reason: &Reason,
    enum_info: &EnumConcreteInfo,
    possible_checks: &std::collections::VecDeque<(Type, EnumCheck)>,
    checks: &[EnumCheck],
    default_case_loc: Option<ALoc>,
    incomplete_out: &Type,
) -> Result<(), FlowJsException> {
    let mut resolved_checks: Vec<EnumCheck> = possible_checks
        .iter()
        .filter_map(|(obj_t, check)| {
            let member_name = &check.member_name;
            let enum_id_discriminant = &enum_info.enum_id;
            let members = &enum_info.members;
            let is_valid = enum_case_test_matches_discriminant(
                cx,
                enum_id_discriminant,
                members,
                member_name,
                obj_t,
            );
            if is_valid { Some(check.clone()) } else { None }
        })
        .collect();
    // OCaml conses onto the front during the fold and ends up with the original
    // switch-case order after reversing the reversed possible_checks list once.
    resolved_checks.reverse();
    resolved_checks.extend_from_slice(checks);
    let members = &enum_info.members;
    let has_unknown_members = enum_info.has_unknown_members;
    let mut members_remaining: FlowOrdMap<FlowSmolStr, ALoc> = members.dupe();
    let mut seen: FlowOrdMap<FlowSmolStr, ALoc> = FlowOrdMap::new();
    for check in &resolved_checks {
        let EnumCheck {
            case_test_loc,
            member_name,
        } = check;
        if !members_remaining.contains_key(member_name) {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked(Box::new(
                    flow_typing_errors::error_message::EnumMemberAlreadyCheckedData {
                        case_test_loc: case_test_loc.dupe(),
                        prev_check_loc: seen[member_name].dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.dupe(),
                    },
                ))),
            )?;
        }
        members_remaining.remove(member_name);
        seen.insert(member_name.dupe(), case_test_loc.dupe());
    }
    let left_over = members_remaining;
    match (left_over.is_empty(), default_case_loc, has_unknown_members) {
        (false, default_case_loc, _) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked(Box::new(
                    flow_typing_errors::error_message::EnumNotAllCheckedData {
                        reason: check_reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                        left_to_check: left_over.keys().duped().collect(),
                        default_case_loc,
                    },
                ))),
            )?;
            flow_enum_exhaustive_check_incomplete(cx, check_reason, None, incomplete_out)?;
        }
        (true, None, true) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked(Box::new(
                    flow_typing_errors::error_message::EnumUnknownNotCheckedData {
                        reason: check_reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            flow_enum_exhaustive_check_incomplete(cx, check_reason, None, incomplete_out)?;
        }
        (true, Some(_), true) => {}
        (true, Some(default_case_loc), false) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked(Box::new(
                    flow_typing_errors::error_message::EnumAllMembersAlreadyCheckedData {
                        loc: default_case_loc,
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
        }
        _ => {}
    }
    Ok(())
}

fn enum_exhaustive_check_of_switch_cases(
    cases_ast: &[statement::switch::Case<ALoc, (ALoc, Type)>],
) -> EnumPossibleExhaustiveCheckT {
    use std::collections::VecDeque;
    enum EnumPossibleExhaustiveCheckAcc {
        EnumExhaustiveCheckPossiblyValid(Box<EnumExhaustiveCheckPossiblyValidData>),
        EnumExhaustiveCheckInvalid(Vec<ALoc>),
    }
    let mut exhaustive_check = EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(
        Box::new(EnumExhaustiveCheckPossiblyValidData {
            possible_checks: VecDeque::new(),
            checks: Vec::new().into(),
            default_case_loc: None,
        }),
    );
    for case in cases_ast {
        exhaustive_check = match case {
            statement::switch::Case {
                test: Some(test_expr),
                ..
            } if let expression::ExpressionInner::Member { inner, .. } = test_expr.deref()
                && let expression::member::Property::PropertyIdentifier(prop_id) =
                    &inner.property
                && is_valid_enum_member_name(&prop_id.name) =>
            {
                let (case_test_loc, _) = test_expr.loc();
                let (_, obj_t) = inner.object.loc();
                match exhaustive_check {
                    EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(_) => {
                        exhaustive_check
                    }
                    EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(
                        box EnumExhaustiveCheckPossiblyValidData {
                            mut possible_checks,
                            checks,
                            default_case_loc,
                        },
                    ) => {
                        let possible_check = (
                            obj_t.dupe(),
                            EnumCheck {
                                case_test_loc: case_test_loc.dupe(),
                                member_name: prop_id.name.dupe(),
                            },
                        );
                        possible_checks.push_front(possible_check);
                        EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(Box::new(
                            EnumExhaustiveCheckPossiblyValidData {
                                possible_checks,
                                checks,
                                default_case_loc,
                            },
                        ))
                    }
                }
            }
            statement::switch::Case {
                loc: default_case_loc,
                test: None,
                ..
            } => match exhaustive_check {
                EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(_) => exhaustive_check,
                EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(
                    box EnumExhaustiveCheckPossiblyValidData {
                        possible_checks,
                        checks,
                        ..
                    },
                ) => EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(Box::new(
                    EnumExhaustiveCheckPossiblyValidData {
                        possible_checks,
                        checks,
                        default_case_loc: Some(default_case_loc.dupe()),
                    },
                )),
            },
            statement::switch::Case {
                test: Some(test_expr),
                ..
            } => {
                let (case_test_loc, _) = test_expr.loc();
                match exhaustive_check {
                    EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(
                        mut invalid_checks,
                    ) => {
                        invalid_checks.push(case_test_loc.dupe());
                        EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(invalid_checks)
                    }
                    EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(..) => {
                        EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(vec![
                            case_test_loc.dupe(),
                        ])
                    }
                }
            }
        };
    }
    match exhaustive_check {
        EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckInvalid(invalid_checks) => {
            EnumPossibleExhaustiveCheckT::EnumExhaustiveCheckInvalid(invalid_checks.into())
        }
        EnumPossibleExhaustiveCheckAcc::EnumExhaustiveCheckPossiblyValid(data) => {
            EnumPossibleExhaustiveCheckT::EnumExhaustiveCheckPossiblyValid(data)
        }
    }
}

fn enum_declaration<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    enum_decl: &statement::EnumDeclaration<ALoc, ALoc>,
) -> statement::EnumDeclaration<ALoc, (ALoc, Type)> {
    let statement::EnumDeclaration {
        id: ref ident,
        ref body,
        const_,
        ref comments,
    } = *enum_decl;
    let name_loc = ident.loc.dupe();
    let name = &ident.name;
    if const_ {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::EEnumError(EnumErrorKind::EnumConstNotSupported(loc.dupe())),
        );
    }
    let reason = mk_reason(
        VirtualReasonDesc::REnum {
            name: Some(name.dupe()),
        },
        name_loc.dupe(),
    );
    let t = if cx.enable_enums() {
        let concrete_info = mk_enum(cx, reason.dupe(), name_loc.dupe(), name.as_str(), body);
        let enum_info = Rc::new(type_::EnumInfo::new(type_::EnumInfoInner::ConcreteEnum(
            type_::EnumConcreteInfo::new(concrete_info),
        )));
        let t = type_::mk_enum_object_type(reason.dupe(), enum_info);
        let use_op = UseOp::Op(Arc::new(type_::RootUseOp::AssignVar {
            var: Some(mk_reason(
                VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                name_loc.dupe(),
            )),
            init: reason.dupe(),
        }));
        type_env::init_implicit_const(cx, &use_op, &t, name_loc.dupe());
        t
    } else {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::EEnumError(EnumErrorKind::EnumsNotEnabled(loc.dupe())),
        );
        any_t::error(reason)
    };
    statement::EnumDeclaration {
        id: ast::Identifier::new(ast::IdentifierInner {
            loc: (name_loc.dupe(), t),
            name: ident.name.dupe(),
            comments: ident.comments.dupe(),
        }),
        body: body.clone(),
        const_,
        comments: comments.clone(),
    }
}

pub fn mk_enum<'a>(
    cx: &Context<'a>,
    enum_reason: Reason,
    name_loc: ALoc,
    enum_name: &str,
    body: &statement::enum_declaration::Body<ALoc>,
) -> type_::EnumConcreteInfoInner {
    use flow_parser_utils::enum_validate;
    let result = enum_validate::classify_enum_body(body, &body.loc);
    // Report validation errors
    for err in &result.errors {
        let error = match err {
            enum_validate::ValidationError::DuplicateMemberName {
                loc,
                prev_use_loc,
                member_name,
            } => EnumErrorKind::EnumDuplicateMemberName(Box::new(EnumDuplicateMemberNameData {
                loc: loc.dupe(),
                prev_use_loc: prev_use_loc.dupe(),
                enum_reason: enum_reason.dupe(),
                member_name: member_name.clone(),
            })),
            enum_validate::ValidationError::InconsistentMemberValues { loc } => {
                EnumErrorKind::EnumInconsistentMemberValues(Box::new(
                    EnumInconsistentMemberValuesData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))
            }
            enum_validate::ValidationError::InvalidMemberInitializer {
                loc,
                explicit_type,
                member_name,
            } => EnumErrorKind::EnumInvalidMemberInitializer(Box::new(
                EnumInvalidMemberInitializerData {
                    loc: loc.dupe(),
                    enum_reason: enum_reason.dupe(),
                    explicit_type: *explicit_type,
                    member_name: member_name.clone(),
                },
            )),
            enum_validate::ValidationError::BooleanMemberNotInitialized { loc, member_name } => {
                EnumErrorKind::EnumBooleanMemberNotInitialized(Box::new(
                    EnumBooleanMemberNotInitializedData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.clone(),
                    },
                ))
            }
            enum_validate::ValidationError::NumberMemberNotInitialized { loc, member_name } => {
                EnumErrorKind::EnumNumberMemberNotInitialized(Box::new(
                    EnumNumberMemberNotInitializedData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.clone(),
                    },
                ))
            }
            enum_validate::ValidationError::BigIntMemberNotInitialized { loc, member_name } => {
                EnumErrorKind::EnumBigIntMemberNotInitialized(Box::new(
                    EnumBigIntMemberNotInitializedData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.clone(),
                    },
                ))
            }
            enum_validate::ValidationError::StringMemberInconsistentlyInitialized { loc } => {
                EnumErrorKind::EnumStringMemberInconsistentlyInitialized(Box::new(
                    EnumStringMemberInconsistentlyInitializedData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))
            }
            enum_validate::ValidationError::SymbolMemberWithInitializer { loc, member_name } => {
                EnumErrorKind::EnumInvalidMemberInitializer(Box::new(
                    EnumInvalidMemberInitializerData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                        explicit_type: Some(
                            flow_parser::ast::statement::enum_declaration::ExplicitType::Symbol,
                        ),
                        member_name: member_name.clone(),
                    },
                ))
            }
            enum_validate::ValidationError::DuplicateMemberValue { loc, prev_use_loc } => {
                EnumErrorKind::EnumMemberDuplicateValue(Box::new(EnumMemberDuplicateValueData {
                    loc: loc.dupe(),
                    prev_use_loc: prev_use_loc.dupe(),
                    enum_reason: enum_reason.dupe(),
                }))
            }
            enum_validate::ValidationError::InvalidMemberName { loc, member_name } => {
                EnumErrorKind::EnumInvalidMemberName(Box::new(EnumInvalidMemberNameData {
                    loc: loc.dupe(),
                    enum_reason: enum_reason.dupe(),
                    member_name: member_name.clone(),
                }))
            }
            enum_validate::ValidationError::NonIdentifierMemberName { loc, member_name } => {
                EnumErrorKind::EnumNonIdentifierMemberName(Box::new(
                    EnumNonIdentifierMemberNameData {
                        loc: loc.dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.clone(),
                    },
                ))
            }
        };
        flow_js_utils::add_output_non_speculating(cx, ErrorMessage::EEnumError(error));
    }
    let members = result
        .members
        .into_iter()
        .fold(FlowOrdMap::new(), |mut acc, (name, loc)| {
            acc.insert(FlowSmolStr::from(name), loc);
            acc
        });
    let enum_id = cx.make_aloc_id(&name_loc);
    let representation_t = match result.rep {
        Some(enum_validate::EnumRep::BoolRep(lit)) => {
            let reason = mk_reason(VirtualReasonDesc::RBoolean, enum_reason.loc().dupe());
            let boolt = match lit {
                Some(b) => DefTInner::SingletonBoolT {
                    value: b,
                    from_annot: false,
                },
                None => DefTInner::BoolGeneralT,
            };
            Type::new(TypeInner::DefT(reason, DefT::new(boolt)))
        }
        Some(enum_validate::EnumRep::NumberRep { truthy }) => {
            let reason = mk_reason(VirtualReasonDesc::RNumber, enum_reason.loc().dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::NumGeneralT(if truthy {
                    Literal::Truthy
                } else {
                    Literal::AnyLiteral
                })),
            ))
        }
        Some(enum_validate::EnumRep::StringRep { truthy }) => {
            let reason = mk_reason(VirtualReasonDesc::RString, enum_reason.loc().dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::StrGeneralT(if truthy {
                    Literal::Truthy
                } else {
                    Literal::AnyLiteral
                })),
            ))
        }
        Some(enum_validate::EnumRep::SymbolRep) => {
            let reason = mk_reason(VirtualReasonDesc::RSymbol, enum_reason.loc().dupe());
            Type::new(TypeInner::DefT(reason, DefT::new(DefTInner::SymbolT)))
        }
        Some(enum_validate::EnumRep::BigIntRep { truthy }) => {
            let reason = mk_reason(VirtualReasonDesc::RBigInt, enum_reason.loc().dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::BigIntGeneralT(if truthy {
                    Literal::Truthy
                } else {
                    Literal::AnyLiteral
                })),
            ))
        }
        None => {
            // Fallback for invalid enums
            let reason = mk_reason(VirtualReasonDesc::RString, enum_reason.loc().dupe());
            Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::StrGeneralT(Literal::Truthy)),
            ))
        }
    };
    let has_unknown_members = result.has_unknown_members.is_some();
    type_::EnumConcreteInfoInner {
        enum_name: FlowSmolStr::from(enum_name),
        enum_id,
        members,
        representation_t,
        has_unknown_members,
    }
}

pub fn match_pattern<'a>(
    cx: &Context<'a>,
    case_match_root_loc: ALoc,
    has_guard: bool,
    pattern: &ast::match_pattern::MatchPattern<ALoc, ALoc>,
) -> Result<ast::match_pattern::MatchPattern<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    let node_cache = cx.node_cache();
    match node_cache.get_match_pattern(pattern.loc()) {
        Some((p, _)) => Ok(p),
        None => {
            let ident = flow_parser::ast_utils::match_root_ident(case_match_root_loc.dupe());
            let acc = expression::Expression::new(expression::ExpressionInner::Identifier {
                loc: ident.loc.dupe(),
                inner: ident,
            });
            fn on_identifier_fn<'a>(
                encl_ctx: EnclosingContext,
                cx: &Context<'a>,
                id: &ast::IdentifierInner<ALoc, ALoc>,
                loc: ALoc,
            ) -> Type {
                let mut flags = natural_inference::empty_syntactic_flags();
                flags.encl_ctx = encl_ctx;
                identifier_inner(cx, &flags, &ast::Identifier::new(id.clone()), loc)
            }
            let on_identifier = on_identifier_fn;
            let on_expression = |cx: &Context<'a>,
                                 expr: &expression::Expression<ALoc, ALoc>|
             -> Result<
                expression::Expression<ALoc, (ALoc, Type)>,
                AbnormalControlFlow,
            > { expression(None, None, None, cx, expr) };
            let on_binding = |use_op: &UseOp,
                              name_loc: ALoc,
                              kind: ast::VariableKind,
                              name: &FlowSmolStr,
                              t: Type|
             -> Type {
                init_var(kind)(cx, use_op, &t, name_loc.dupe());
                let default = type_env::get_var_declared_type(
                    None,
                    None,
                    cx,
                    Name::new(name.dupe()),
                    name_loc.dupe(),
                );
                type_env::constraining_type(default, cx, name, name_loc)
            };
            let p = crate::match_pattern::pattern(
                cx,
                &on_identifier,
                &on_expression,
                &on_binding,
                &acc,
                pattern,
            )?;
            node_cache.set_match_pattern(pattern.loc().dupe(), (p.clone(), has_guard));
            Ok(p)
        }
    }
}

fn error_on_match_case_invalid_syntax<'a>(
    cx: &Context<'a>,
    match_keyword_loc: ALoc,
    invalid_syntax_list: &[ast::match_::InvalidSyntax<ALoc>],
) {
    let mut prefix_acc: Vec<ALoc> = Vec::new();
    let mut infix_acc: Vec<ALoc> = Vec::new();
    let mut suffix_acc: Vec<ALoc> = Vec::new();
    for invalid_syntax in invalid_syntax_list {
        if let Some(ref loc) = invalid_syntax.invalid_prefix_case {
            prefix_acc.push(loc.dupe());
        }
        if let Some(ref loc) = invalid_syntax.invalid_infix_colon {
            infix_acc.push(loc.dupe());
        }
        if let Some(ref loc) = invalid_syntax.invalid_suffix_semicolon {
            suffix_acc.push(loc.dupe());
        }
    }
    match (
        prefix_acc.as_slice(),
        infix_acc.as_slice(),
        suffix_acc.as_slice(),
    ) {
        ([], [], []) => {}
        ([loc], [], []) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax(Box::new(
                    MatchInvalidCaseSyntaxData {
                        loc: loc.dupe(),
                        kind: MatchInvalidCaseSyntax::InvalidMatchCasePrefixCase,
                    },
                ))),
            );
        }
        ([], [loc], []) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax(Box::new(
                    MatchInvalidCaseSyntaxData {
                        loc: loc.dupe(),
                        kind: MatchInvalidCaseSyntax::InvalidMatchCaseInfixColon,
                    },
                ))),
            );
        }
        ([], [], [loc]) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax(Box::new(
                    MatchInvalidCaseSyntaxData {
                        loc: loc.dupe(),
                        kind: MatchInvalidCaseSyntax::InvalidMatchCaseSuffixSemicolon,
                    },
                ))),
            );
        }
        _ => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax(Box::new(
                    MatchInvalidCaseSyntaxData {
                        loc: match_keyword_loc,
                        kind: MatchInvalidCaseSyntax::InvalidMatchCaseMultiple {
                            invalid_prefix_case_locs: prefix_acc,
                            invalid_infix_colon_locs: infix_acc,
                            invalid_suffix_semicolon_locs: suffix_acc,
                        },
                    },
                ))),
            );
        }
    }
}

fn error_on_record_declaration_invalid_syntax<'a>(
    cx: &Context<'a>,
    multiple_error_loc: ALoc,
    invalid_syntax: Option<&statement::record_declaration::InvalidSyntax<ALoc>>,
    invalid_properties_syntax: &[&statement::record_declaration::InvalidPropertySyntax<ALoc>],
) {
    let mut variance_acc: Vec<ALoc> = Vec::new();
    let mut optional_acc: Vec<ALoc> = Vec::new();
    let mut suffix_acc: Vec<ALoc> = Vec::new();
    for inv in invalid_properties_syntax {
        if let Some(ref var) = inv.invalid_variance {
            variance_acc.push(var.loc.dupe());
        }
        if let Some(loc) = &inv.invalid_optional {
            optional_acc.push(loc.dupe());
        }
        if let Some(loc) = &inv.invalid_suffix_semicolon {
            suffix_acc.push(loc.dupe());
        }
    }
    let invalid_infix_equals =
        invalid_syntax.and_then(|s| s.invalid_infix_equals.as_ref().map(|l| l.dupe()));
    // If we only have one error in this `record`, error there, otherwise coalesce into a single error.
    match (
        &invalid_infix_equals,
        (
            variance_acc.as_slice(),
            optional_acc.as_slice(),
            suffix_acc.as_slice(),
        ),
    ) {
        (None, ([], [], [])) => {}
        (Some(loc), ([], [], [])) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ERecordError(RecordErrorKind::RecordDeclarationInvalidSyntax {
                    loc: loc.dupe(),
                    kind: RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxInfixEquals,
                }),
            );
        }
        (None, ([loc], [], [])) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ERecordError(RecordErrorKind::RecordDeclarationInvalidSyntax {
                    loc: loc.dupe(),
                    kind: RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxVariance,
                }),
            );
        }
        (None, ([], [loc], [])) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ERecordError(RecordErrorKind::RecordDeclarationInvalidSyntax {
                    loc: loc.dupe(),
                    kind: RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxOptional,
                }),
            );
        }
        (None, ([], [], [loc])) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ERecordError(RecordErrorKind::RecordDeclarationInvalidSyntax {
                    loc: loc.dupe(),
                    kind: RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxSuffixSemicolon,
                }),
            );
        }
        _ => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::ERecordError(RecordErrorKind::RecordDeclarationInvalidSyntax {
                    loc: multiple_error_loc,
                    kind: RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxMultiple {
                        invalid_infix_equals_loc: invalid_infix_equals,
                        invalid_variance_locs: variance_acc,
                        invalid_optional_locs: optional_acc,
                        invalid_suffix_semicolon_locs: suffix_acc,
                    },
                }),
            );
        }
    }
}

// **** Public API wrappers (at the end of the file) ****

pub fn expression<'a>(
    encl_ctx: Option<EnclosingContext>,
    decl: Option<ast::VariableKind>,
    as_const: Option<bool>,
    cx: &Context<'a>,
    expr: &expression::Expression<ALoc, ALoc>,
) -> Result<expression::Expression<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    expression_inner(
        encl_ctx,
        decl,
        as_const,
        FrozenKind::NotFrozen,
        None,
        cx,
        expr,
    )
}

pub fn identifier<'a>(
    encl_ctx: EnclosingContext,
    cx: &Context<'a>,
    id: &ast::Identifier<ALoc, ALoc>,
    loc: ALoc,
) -> Type {
    let mut syntactic_flags = natural_inference::empty_syntactic_flags();
    syntactic_flags.encl_ctx = encl_ctx;
    identifier_inner(cx, &syntactic_flags, id, loc)
}

pub fn string_literal<'a>(
    cx: &Context<'a>,
    encl_ctx: EnclosingContext,
    loc: ALoc,
    v: &ast::StringLiteral<ALoc>,
) -> Type {
    let mut syntactic_flags = natural_inference::empty_syntactic_flags();
    syntactic_flags.encl_ctx = encl_ctx;
    string_literal_inner(cx, &syntactic_flags, loc, v)
}

pub fn number_literal<'a>(
    cx: &Context<'a>,
    encl_ctx: EnclosingContext,
    loc: ALoc,
    v: &ast::NumberLiteral<ALoc>,
) -> Type {
    let mut syntactic_flags = natural_inference::empty_syntactic_flags();
    syntactic_flags.encl_ctx = encl_ctx;
    number_literal_inner(cx, &syntactic_flags, loc, v)
}

pub fn boolean_literal<'a>(
    cx: &Context<'a>,
    encl_ctx: EnclosingContext,
    loc: ALoc,
    v: &ast::BooleanLiteral<ALoc>,
) -> Type {
    let mut syntactic_flags = natural_inference::empty_syntactic_flags();
    syntactic_flags.encl_ctx = encl_ctx;
    boolean_literal_inner(cx, &syntactic_flags, loc, v)
}

pub fn bigint_literal<'a>(
    cx: &Context<'a>,
    encl_ctx: EnclosingContext,
    loc: ALoc,
    v: &ast::BigIntLiteral<ALoc>,
) -> Type {
    let mut syntactic_flags = natural_inference::empty_syntactic_flags();
    syntactic_flags.encl_ctx = encl_ctx;
    bigint_literal_inner(cx, &syntactic_flags, loc, v)
}
