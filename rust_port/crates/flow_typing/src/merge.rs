/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_aloc::ALocTable;
use flow_analysis::property_assignment;
use flow_common::files;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_projects::FlowProjects;
use flow_common::platform_set;
use flow_common::platform_set::PlatformSet;
use flow_common::polarity::Polarity;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_lint_settings::lint_settings::LintSettings;
use flow_lint_settings::lints::SketchyNullKind;
use flow_lint_settings::severity::Severity;
use flow_lint_settings::strict_mode_settings::StrictModeSettings;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::file_sig::FileSig;
use flow_type_sig::compact_table;
use flow_type_sig::type_sig;
use flow_type_sig::type_sig_options::TypeSigOptions;
use flow_type_sig::type_sig_utils;
use flow_typing_builtins::Builtins;
use flow_typing_context::BuiltinsGroup;
use flow_typing_context::Context;
use flow_typing_context::MasterContext;
use flow_typing_context::Metadata;
use flow_typing_context::ResolvedRequire;
use flow_typing_errors::error_message::EComparisonData;
use flow_typing_errors::error_message::EConstantConditionData;
use flow_typing_errors::error_message::EDevOnlyInvalidatedRefinementInfoData;
use flow_typing_errors::error_message::EDevOnlyRefinedLocInfoData;
use flow_typing_errors::error_message::EDuplicateModuleProviderData;
use flow_typing_errors::error_message::EIllegalAssertOperatorData;
use flow_typing_errors::error_message::EKeySpreadPropData;
use flow_typing_errors::error_message::EMissingPlatformSupportData;
use flow_typing_errors::error_message::EPlatformSpecificImplementationModuleLookupFailedData;
use flow_typing_errors::error_message::EPropNotFoundInLookupData;
use flow_typing_errors::error_message::ESketchyNullLintData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::flow_error;
use flow_typing_errors::flow_error::FlowError;
use flow_typing_errors::intermediate_error_types::ConstantConditionKind;
use flow_typing_errors::intermediate_error_types::EmptySide;
use flow_typing_errors::intermediate_error_types::NullSide;
use flow_typing_errors::intermediate_error_types::StrictComparisonInfo;
use flow_typing_errors::intermediate_error_types::StrictComparisonKind;
use flow_typing_exists_check::ExistsCheck;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_flow_js::tvar_resolver;
use flow_typing_statement::exists_marker;
use flow_typing_statement::module_info_analyzer;
use flow_typing_statement::react_rules;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::BigIntLiteral;
use flow_typing_type::type_::ConformToCommonInterfaceData;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::EnumInfo;
use flow_typing_type::type_::EnumInfoInner;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::Literal;
use flow_typing_type::type_::MixedFlavor;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::NumberLiteral;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::RootUseOp;
use flow_typing_type::type_::TvarSeenSet;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::constraint::Constraints;
use flow_typing_type::type_::drop_generic;
use flow_typing_type::type_::eval;
use flow_typing_type::type_::exports;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::null;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::property;
use flow_typing_type::type_::type_or_type_desc;
use flow_typing_type::type_::void;
use flow_typing_type::type_util;
use flow_typing_type::type_util::reason_of_t;
use flow_typing_utils::check_polarity;
use flow_typing_utils::exhaustive;
use flow_typing_utils::module_exports_checker;
use flow_typing_utils::strict_es6_import_export;
use flow_typing_utils::type_filter;
use flow_typing_utils::type_operation_utils::type_assertions;
use flow_typing_utils::type_sig_merge;
use flow_typing_visitors::type_visitor;
use flow_typing_visitors::type_visitor::TypeVisitor;
use flow_utils_union_find::Node;

fn force_lazy_tvars<'cx>(cx: &Context<'cx>) {
    for s in cx.post_component_tvar_forcing_states().iter() {
        cx.force_fully_resolved_tvar(s);
    }
}

fn detect_sketchy_null_checks<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    exists_marker::mark(cx, tast);

    let add_error = |loc: &ALoc, null_loc: &ALoc, kind: SketchyNullKind, falsy_loc: &ALoc| {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::ESketchyNullLint(Box::new(ESketchyNullLintData {
                kind,
                loc: loc.dupe(),
                null_loc: null_loc.dupe(),
                falsy_loc: falsy_loc.dupe(),
            })),
        );
    };

    let detect_function =
        |exists_excuses: &ALocMap<ExistsCheck>, loc: &ALoc, exists_check: &ExistsCheck| {
            let exists_excuse = exists_excuses
                .get(loc)
                .cloned()
                .unwrap_or_else(ExistsCheck::empty);

            match &exists_check.null_loc {
                None => {}
                Some(null_loc) => {
                    if exists_excuse.bool_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.bool_loc {
                            add_error(loc, null_loc, SketchyNullKind::Bool, falsy_loc);
                        }
                    }
                    if exists_excuse.number_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.number_loc {
                            add_error(loc, null_loc, SketchyNullKind::Number, falsy_loc);
                        }
                    }
                    if exists_excuse.bigint_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.bigint_loc {
                            add_error(loc, null_loc, SketchyNullKind::BigInt, falsy_loc);
                        }
                    }
                    if exists_excuse.string_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.string_loc {
                            add_error(loc, null_loc, SketchyNullKind::String, falsy_loc);
                        }
                    }
                    if exists_excuse.mixed_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.mixed_loc {
                            add_error(loc, null_loc, SketchyNullKind::Mixed, falsy_loc);
                        }
                    }
                    if exists_excuse.enum_bool_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.enum_bool_loc {
                            add_error(loc, null_loc, SketchyNullKind::EnumBool, falsy_loc);
                        }
                    }
                    if exists_excuse.enum_number_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.enum_number_loc {
                            add_error(loc, null_loc, SketchyNullKind::EnumNumber, falsy_loc);
                        }
                    }
                    if exists_excuse.enum_bigint_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.enum_bigint_loc {
                            add_error(loc, null_loc, SketchyNullKind::EnumBigInt, falsy_loc);
                        }
                    }
                    if exists_excuse.enum_string_loc.is_none() {
                        if let Some(ref falsy_loc) = exists_check.enum_string_loc {
                            add_error(loc, null_loc, SketchyNullKind::EnumString, falsy_loc);
                        }
                    }
                }
            }
        };

    let exists_checks = {
        let checks = cx.exists_checks();
        if !checks.is_empty() {
            fn make_checks<'cx>(
                cx: &Context<'cx>,
                seen: &mut TvarSeenSet<i32>,
                cur_checks: ALocMap<ExistsCheck>,
                loc: &ALoc,
                t: &Type,
            ) -> ALocMap<ExistsCheck> {
                match t.deref() {
                    TypeInner::AnnotT(_, t, _) => make_checks(cx, seen, cur_checks, loc, t),
                    TypeInner::OpenT(tvar) if seen.contains(&(tvar.id() as i32)) => cur_checks,
                    TypeInner::OpenT(tvar) => {
                        let id = tvar.id() as i32;
                        match cx.find_resolved(t) {
                            Some(resolved) => seen.with_added(id, |seen| {
                                make_checks(cx, seen, cur_checks, loc, &resolved)
                            }),
                            None => cur_checks,
                        }
                    }
                    // Ignore AnyTs for sketchy null checks; otherwise they'd always trigger the lint.
                    TypeInner::AnyT(_, _) => cur_checks,
                    TypeInner::GenericT(box GenericTData { bound, .. }) => {
                        make_checks(cx, seen, cur_checks, loc, bound)
                    }
                    TypeInner::NominalT { nominal_type, .. }
                        if let Some(t) = match &nominal_type.underlying_t {
                            nominal::UnderlyingT::OpaqueWithLocal { t }
                            | nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                                t,
                                ..
                            }) => Some(t),
                            _ => None,
                        } =>
                    {
                        make_checks(cx, seen, cur_checks, loc, t)
                    }
                    TypeInner::NominalT { nominal_type, .. }
                        if matches!(
                            &nominal_type.underlying_t,
                            nominal::UnderlyingT::FullyOpaque
                        ) && let Some(upper_t) = nominal_type.upper_t.as_ref() =>
                    {
                        make_checks(cx, seen, cur_checks, loc, upper_t)
                    }
                    TypeInner::MaybeT(r, t) => {
                        let acc = make_checks(cx, seen, cur_checks, loc, t);
                        let null_t = null::why(r.dupe());
                        let acc = make_checks(cx, seen, acc, loc, &null_t);
                        let void_t = void::why(r.dupe());
                        make_checks(cx, seen, acc, loc, &void_t)
                    }
                    TypeInner::OptionalT { reason, type_, .. } => {
                        let acc = make_checks(cx, seen, cur_checks, loc, type_);
                        let void_t = void::why(reason.dupe());
                        make_checks(cx, seen, acc, loc, &void_t)
                    }
                    TypeInner::UnionT(_, rep) => {
                        let mut acc = cur_checks;
                        for t in rep.members_iter() {
                            acc = make_checks(cx, seen, acc, loc, t);
                        }
                        acc
                    }
                    _ => {
                        let t_loc = {
                            let reason = reason_of_t(t);
                            match reason.annot_loc() {
                                Some(loc) => Some(loc.dupe()),
                                None => Some(reason.def_loc().dupe()),
                            }
                        };
                        let mut exists_check = cur_checks
                            .get(loc)
                            .cloned()
                            .unwrap_or_else(ExistsCheck::empty);
                        let filter_result = type_filter::maybe(cx, t.dupe());
                        match filter_result.type_.deref() {
                            TypeInner::DefT(_, def_t) if matches!(&**def_t, DefTInner::EmptyT) => {}
                            _ => {
                                exists_check.null_loc = t_loc.dupe();
                            }
                        }

                        let filtered = {
                            let r1 = type_filter::not_truthy(cx, t.dupe());
                            let r2 = type_filter::not_maybe(cx, r1.type_);
                            r2.type_
                        };

                        fn enum_representation_t(ev: &Rc<EnumInfo>) -> &Type {
                            match &***ev {
                                EnumInfoInner::ConcreteEnum(concrete) => &concrete.representation_t,
                                EnumInfoInner::AbstractEnum { representation_t } => {
                                    representation_t
                                }
                            }
                        }

                        match filtered.deref() {
                            TypeInner::DefT(_, def_t)
                                if matches!(
                                    &**def_t,
                                    DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. }
                                ) =>
                            {
                                exists_check.bool_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if matches!(
                                    &**def_t,
                                    DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
                                ) =>
                            {
                                exists_check.string_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if matches!(
                                    &**def_t,
                                    DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
                                ) =>
                            {
                                exists_check.number_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if matches!(&**def_t, DefTInner::BigIntGeneralT(_)) =>
                            {
                                exists_check.bigint_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if matches!(&**def_t, DefTInner::MixedT(_)) =>
                            {
                                exists_check.mixed_loc = t_loc.dupe();
                            }
                            // (*           | DefT *)
                            // (*               ( _, *)
                            // (*                 EnumValueT *)
                            // (*                   (ConcreteEnum *)
                            // (*                     { representation_t = DefT (_, (BoolGeneralT | SingletonBoolT _)); _ } *)
                            // (*                     ) *)
                            // (*               ) *)
                            // (*           | DefT *)
                            // (*               ( _, *)
                            // (*                 EnumValueT *)
                            // (*                   (AbstractEnum *)
                            // (*                     { representation_t = DefT (_, (BoolGeneralT | SingletonBoolT _)); _ } *)
                            // (*                     ) *)
                            // (*               ) -> *)
                            // (*             { exists_check with enum_bool_loc = t_loc } *)
                            TypeInner::DefT(_, def_t)
                                if match def_t.deref() {
                                    DefTInner::EnumValueT(ev) => {
                                        match enum_representation_t(ev).deref() {
                                            TypeInner::DefT(_, inner) => {
                                                matches!(
                                                    inner.deref(),
                                                    DefTInner::BoolGeneralT
                                                        | DefTInner::SingletonBoolT { .. }
                                                )
                                            }
                                            _ => false,
                                        }
                                    }
                                    _ => false,
                                } =>
                            {
                                exists_check.enum_bool_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if match def_t.deref() {
                                    DefTInner::EnumValueT(ev) => {
                                        match enum_representation_t(ev).deref() {
                                            TypeInner::DefT(_, inner) => {
                                                matches!(
                                                    inner.deref(),
                                                    DefTInner::StrGeneralT(_)
                                                        | DefTInner::SingletonStrT { .. }
                                                )
                                            }
                                            _ => false,
                                        }
                                    }
                                    _ => false,
                                } =>
                            {
                                exists_check.enum_string_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if match def_t.deref() {
                                    DefTInner::EnumValueT(ev) => {
                                        match enum_representation_t(ev).deref() {
                                            TypeInner::DefT(_, inner) => {
                                                matches!(
                                                    inner.deref(),
                                                    DefTInner::NumGeneralT(_)
                                                        | DefTInner::SingletonNumT { .. }
                                                )
                                            }
                                            _ => false,
                                        }
                                    }
                                    _ => false,
                                } =>
                            {
                                exists_check.enum_number_loc = t_loc.dupe();
                            }
                            TypeInner::DefT(_, def_t)
                                if match def_t.deref() {
                                    DefTInner::EnumValueT(ev) => match enum_representation_t(ev)
                                        .deref()
                                    {
                                        TypeInner::DefT(_, inner) => {
                                            matches!(inner.deref(), DefTInner::BigIntGeneralT(_))
                                        }
                                        _ => false,
                                    },
                                    _ => false,
                                } =>
                            {
                                exists_check.enum_bigint_loc = t_loc.dupe();
                            }
                            _ => {}
                        }
                        if exists_check == ExistsCheck::empty() {
                            cur_checks
                        } else {
                            let mut result = cur_checks;
                            result.insert(loc.dupe(), exists_check);
                            result
                        }
                    }
                }
            }

            let mut result: ALocMap<ExistsCheck> = ALocMap::new();
            for (loc, tset) in checks.iter() {
                for t in tset.iter() {
                    result = make_checks(cx, &mut TvarSeenSet::new(), result, loc, t);
                }
            }
            result
        } else {
            ALocMap::new()
        }
    };

    let exists_excuses = cx.exists_excuses();
    for (loc, exists_check) in exists_checks.iter() {
        detect_function(&exists_excuses, loc, exists_check);
    }
}

fn detect_test_prop_misses<'cx>(cx: &Context<'cx>) {
    let misses = cx.test_prop_get_never_hit();
    for (prop_name, (reason_prop, reason_obj), use_op, suggestion) in misses.iter() {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                prop_name: prop_name.dupe(),
                reason_prop: reason_prop.dupe(),
                reason_obj: reason_obj.dupe(),
                use_op: use_op.dupe(),
                suggestion: suggestion.dupe(),
            })),
        );
    }
}

enum TruthynessResult {
    ConstCondTruthy {
        constant_condition_kind: ConstantConditionKind,
        reason: Option<Reason>,
    },
    ConstCondFalsy {
        constant_condition_kind: ConstantConditionKind,
        reason: Option<Reason>,
    },
    ConstCondUnknown,
    ConstCondUnconcretized,
}

fn try_eval_concrete_type_truthyness<'cx>(cx: &Context<'cx>, t: &Type) -> TruthynessResult {
    match t.deref() {
        TypeInner::OpenT(_)
        | TypeInner::EvalT { .. }
        | TypeInner::TypeAppT(..)
        | TypeInner::GenericT(..)
        | TypeInner::AnnotT(_, _, _)
        | TypeInner::MaybeT(_, _)
        | TypeInner::OptionalT { .. }
        | TypeInner::UnionT(_, _) => TruthynessResult::ConstCondUnconcretized,
        TypeInner::DefT(reason, def_t) => match def_t.deref() {
            DefTInner::NumGeneralT(literal) => match literal {
                Literal::Truthy => TruthynessResult::ConstCondTruthy {
                    constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                    reason: Some(reason.dupe()),
                },
                // might be null if it's from member access due to flow soundness hole
                // or internal flow bugs
                Literal::AnyLiteral => TruthynessResult::ConstCondUnknown,
            },
            DefTInner::StrGeneralT(literal) => match literal {
                Literal::Truthy => TruthynessResult::ConstCondTruthy {
                    constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                    reason: Some(reason.dupe()),
                },
                // we don't know the exact string literal but we might know the truthyness from refinement
                // might be null if it's from member access due to flow soundness hole
                // or internal flow bugs
                Literal::AnyLiteral => TruthynessResult::ConstCondUnknown,
            },
            DefTInner::BoolGeneralT => TruthynessResult::ConstCondUnknown,
            DefTInner::BigIntGeneralT(literal) => match literal {
                Literal::Truthy => TruthynessResult::ConstCondTruthy {
                    constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                    reason: Some(reason.dupe()),
                },
                // might be null if it's from member access due to flow soundness hole
                // or internal flow bugs
                Literal::AnyLiteral => TruthynessResult::ConstCondUnknown,
            },
            DefTInner::EmptyT => TruthynessResult::ConstCondUnknown,
            DefTInner::MixedT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::NullT | DefTInner::VoidT => TruthynessResult::ConstCondFalsy {
                constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                reason: Some(reason.dupe()),
            },
            DefTInner::SymbolT | DefTInner::UniqueSymbolT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::FunT(..) => TruthynessResult::ConstCondTruthy {
                constant_condition_kind: ConstantConditionKind::UncalledFunction,
                reason: Some(reason.dupe()),
            },
            DefTInner::ObjT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::ArrT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::ClassT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::InstanceT(instance_t) => {
                let class_id = instance_t.inst.class_id.dupe();
                if flow_js_utils::is_builtin_class_id("Promise", class_id, cx) {
                    TruthynessResult::ConstCondTruthy {
                        constant_condition_kind: ConstantConditionKind::UnawaitedPromise,
                        reason: Some(reason.dupe()),
                    }
                } else {
                    TruthynessResult::ConstCondUnknown
                }
            }
            DefTInner::SingletonStrT { value, .. } => {
                if value.as_str() == "" {
                    TruthynessResult::ConstCondFalsy {
                        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                        reason: Some(reason.dupe()),
                    }
                } else {
                    TruthynessResult::ConstCondUnknown
                }
            }
            DefTInner::NumericStrKeyT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::SingletonNumT { value, .. } => {
                let NumberLiteral(number_value, _) = value;
                if *number_value == 0.0 {
                    TruthynessResult::ConstCondFalsy {
                        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                        reason: Some(reason.dupe()),
                    }
                } else {
                    TruthynessResult::ConstCondUnknown
                }
            }
            DefTInner::SingletonBoolT { value, .. } => {
                if *value {
                    TruthynessResult::ConstCondTruthy {
                        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                        reason: Some(reason.dupe()),
                    }
                } else {
                    TruthynessResult::ConstCondFalsy {
                        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                        reason: Some(reason.dupe()),
                    }
                }
            }
            DefTInner::SingletonBigIntT { value, .. } => {
                let BigIntLiteral(bigint_value, _) = value;
                match bigint_value {
                    Some(0) => TruthynessResult::ConstCondFalsy {
                        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                        reason: Some(reason.dupe()),
                    },
                    _ => TruthynessResult::ConstCondUnknown,
                }
            }
            DefTInner::TypeT(..) => TruthynessResult::ConstCondUnknown,
            DefTInner::PolyT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::ReactAbstractComponentT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::RendersT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::EnumValueT(_) => TruthynessResult::ConstCondUnknown,
            DefTInner::EnumObjectT { .. } => TruthynessResult::ConstCondUnknown,
        },
        TypeInner::ThisInstanceT(..) => TruthynessResult::ConstCondUnknown,
        TypeInner::ThisTypeAppT(..) => TruthynessResult::ConstCondUnknown,
        TypeInner::ObjProtoT(_) => TruthynessResult::ConstCondUnknown,
        TypeInner::FunProtoT(_) => TruthynessResult::ConstCondUnknown,
        TypeInner::NullProtoT(_) => TruthynessResult::ConstCondUnknown,
        TypeInner::FunProtoBindT(_) => TruthynessResult::ConstCondUnknown,
        TypeInner::IntersectionT(_, _) => TruthynessResult::ConstCondUnknown,
        TypeInner::KeysT(_, _) => TruthynessResult::ConstCondUnknown,
        TypeInner::StrUtilT { .. } => TruthynessResult::ConstCondUnknown,
        TypeInner::NominalT { .. } => TruthynessResult::ConstCondUnknown,
        TypeInner::NamespaceT(_) => TruthynessResult::ConstCondUnknown,
        TypeInner::AnyT(_, _) => TruthynessResult::ConstCondUnknown,
    }
}

fn try_eval_type_truthyness<'cx>(cx: &Context<'cx>, t: &Type) -> TruthynessResult {
    let reason = reason_of_t(t);
    let concrete_types = FlowJs::all_possible_concrete_types(cx, reason, t).unwrap();
    let results: Vec<_> = concrete_types
        .iter()
        .map(|t| try_eval_concrete_type_truthyness(cx, t))
        .collect();
    match results.len() {
        0 => TruthynessResult::ConstCondUnknown,
        1 => results.into_iter().next().unwrap(),
        _ => {
            if results
                .iter()
                .all(|r| matches!(r, TruthynessResult::ConstCondTruthy { .. }))
            {
                // we don't pass on the error kind when there's multiple types
                TruthynessResult::ConstCondTruthy {
                    constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                    reason: None,
                }
            } else if results
                .iter()
                .all(|r| matches!(r, TruthynessResult::ConstCondFalsy { .. }))
            {
                // we don't pass on the error kind when there's multiple types
                TruthynessResult::ConstCondFalsy {
                    constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
                    reason: None,
                }
            } else {
                TruthynessResult::ConstCondUnknown
            }
        }
    }
}

#[derive(Clone, Dupe)]
enum CheckConditionResult {
    ConditionAllowed,
    ConditionBanned {
        is_truthy: bool,
        show_warning: bool,
        constant_condition_kind: ConstantConditionKind,
        reason: Option<Reason>,
    },
}

fn condition_banned_and_truthy() -> CheckConditionResult {
    CheckConditionResult::ConditionBanned {
        is_truthy: true,
        show_warning: false,
        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
        reason: None,
    }
}

fn condition_banned_and_falsy() -> CheckConditionResult {
    CheckConditionResult::ConditionBanned {
        is_truthy: false,
        show_warning: false,
        constant_condition_kind: ConstantConditionKind::ConstCondGeneral,
        reason: None,
    }
}

fn condition_allowed() -> CheckConditionResult {
    CheckConditionResult::ConditionAllowed
}

fn use_type_to_check_conditional<'cx>(cx: &Context<'cx>, ttype: &Type) -> CheckConditionResult {
    // We always show warning for errors generated from type inferrence
    match try_eval_type_truthyness(cx, ttype) {
        TruthynessResult::ConstCondTruthy {
            constant_condition_kind,
            reason,
        } => CheckConditionResult::ConditionBanned {
            is_truthy: true,
            show_warning: true,
            constant_condition_kind,
            reason,
        },
        TruthynessResult::ConstCondFalsy {
            constant_condition_kind,
            reason,
        } => CheckConditionResult::ConditionBanned {
            is_truthy: false,
            show_warning: true,
            constant_condition_kind,
            reason,
        },
        TruthynessResult::ConstCondUnknown => CheckConditionResult::ConditionAllowed,
        // ConstCondUnconcretized shouldn't happen because we call
        // `all_possible_concrete_types` to concretize all possible types
        TruthynessResult::ConstCondUnconcretized => CheckConditionResult::ConditionAllowed,
    }
}

fn check_conditional<'cx>(
    should_report_error: bool,
    cx: &Context<'cx>,
    e: &ast::expression::Expression<ALoc, (ALoc, Type)>,
    cached_results: &mut ALocMap<CheckConditionResult>,
) -> CheckConditionResult {
    let mut should_report_error_ref = should_report_error;
    let (loc, ttype) = e.loc();
    let exp = e.deref();
    if let Some(result) = cached_results.get(loc) {
        return result.dupe();
    }
    let check_condition_result = match exp {
        ast::expression::ExpressionInner::StringLiteral { inner, .. } => {
            if inner.value.as_str() == "" {
                condition_banned_and_falsy()
            } else {
                condition_banned_and_truthy()
            }
        }
        // true/false is allowed because it's likely intentional
        // and could be useful for debugging / iterating
        ast::expression::ExpressionInner::BooleanLiteral { .. } => condition_allowed(),
        ast::expression::ExpressionInner::NullLiteral { .. } => condition_banned_and_falsy(),
        ast::expression::ExpressionInner::NumberLiteral { inner, .. } => {
            if inner.value != 0.0 && inner.value != 1.0 {
                condition_banned_and_truthy()
            } else {
                condition_allowed()
            }
        }
        ast::expression::ExpressionInner::BigIntLiteral { inner, .. } => match inner.value {
            Some(0) | Some(1) => condition_allowed(),
            _ => condition_banned_and_truthy(),
        },
        ast::expression::ExpressionInner::RegExpLiteral { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::ModuleRefLiteral { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Super { .. } => condition_banned_and_truthy(),
        // we don't check `++` and `--` because their value could change
        ast::expression::ExpressionInner::Update { .. } => condition_allowed(),
        ast::expression::ExpressionInner::Logical { inner, .. } => match inner.operator {
            ast::expression::LogicalOperator::Or | ast::expression::LogicalOperator::And => {
                let left_result = check_conditional(false, cx, &inner.left, cached_results);
                let right_result = check_conditional(false, cx, &inner.right, cached_results);
                match (&left_result, &right_result) {
                    (
                        CheckConditionResult::ConditionBanned {
                            is_truthy: left_truthy,
                            show_warning: left_show_warning,
                            constant_condition_kind: left_kind,
                            ..
                        },
                        CheckConditionResult::ConditionBanned {
                            is_truthy: right_truthy,
                            show_warning: right_show_warning,
                            constant_condition_kind: right_kind,
                            ..
                        },
                    ) if left_truthy == right_truthy => CheckConditionResult::ConditionBanned {
                        is_truthy: *left_truthy,
                        show_warning: *left_show_warning || *right_show_warning,
                        constant_condition_kind: if left_kind == right_kind {
                            *left_kind
                        } else {
                            ConstantConditionKind::ConstCondGeneral
                        },
                        reason: None,
                    },
                    _ => condition_allowed(),
                }
            }
            // NullishCoalesce is allowed here because the `lhs` is stored in `conditions`
            // and we will check `lhs` in a separate call of check_conditional, so we don't need to
            // recur to the lhs here.
            ast::expression::LogicalOperator::NullishCoalesce => condition_allowed(),
        },
        ast::expression::ExpressionInner::TypeCast { inner, .. } => {
            should_report_error_ref = false;
            check_conditional(should_report_error, cx, &inner.expression, cached_results)
        }
        ast::expression::ExpressionInner::AsConstExpression { inner, .. } => {
            should_report_error_ref = false;
            check_conditional(should_report_error, cx, &inner.expression, cached_results)
        }
        ast::expression::ExpressionInner::TSSatisfies { inner, .. } => {
            should_report_error_ref = false;
            check_conditional(should_report_error, cx, &inner.expression, cached_results)
        }
        ast::expression::ExpressionInner::Member { .. } => condition_allowed(),
        ast::expression::ExpressionInner::OptionalMember { .. } => condition_allowed(),
        ast::expression::ExpressionInner::Object { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Array { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::New { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Conditional { inner, .. } => {
            // we don't need to check or unnest `test` here because they are unnested and checked
            // in `expression` function
            let left_result = check_conditional(false, cx, &inner.consequent, cached_results);
            let right_result = check_conditional(false, cx, &inner.alternate, cached_results);
            match (&left_result, &right_result) {
                (
                    CheckConditionResult::ConditionBanned {
                        is_truthy: left_truthy,
                        show_warning: left_show_warning,
                        constant_condition_kind: left_kind,
                        ..
                    },
                    CheckConditionResult::ConditionBanned {
                        is_truthy: right_truthy,
                        show_warning: right_show_warning,
                        constant_condition_kind: right_kind,
                        ..
                    },
                ) if left_truthy == right_truthy => CheckConditionResult::ConditionBanned {
                    is_truthy: *left_truthy,
                    show_warning: *left_show_warning || *right_show_warning,
                    constant_condition_kind: if left_kind == right_kind {
                        *left_kind
                    } else {
                        ConstantConditionKind::ConstCondGeneral
                    },
                    reason: None,
                },
                _ => condition_allowed(),
            }
        }
        ast::expression::ExpressionInner::Sequence { inner, .. } => {
            let last = inner.expressions.last().unwrap();
            should_report_error_ref = false;
            check_conditional(should_report_error, cx, last, cached_results)
        }
        ast::expression::ExpressionInner::Function { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::ArrowFunction { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::TaggedTemplate { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::TemplateLiteral { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::JSXElement { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::JSXFragment { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Class { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Yield { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Import { .. } => condition_banned_and_truthy(),
        ast::expression::ExpressionInner::Unary { inner, .. } => match inner.operator {
            ast::expression::UnaryOperator::Not => {
                let arg_eval_result = check_conditional(false, cx, &inner.argument, cached_results);
                match arg_eval_result {
                    CheckConditionResult::ConditionBanned {
                        is_truthy,
                        show_warning,
                        constant_condition_kind,
                        reason,
                    } => CheckConditionResult::ConditionBanned {
                        is_truthy: !is_truthy,
                        show_warning,
                        constant_condition_kind,
                        reason,
                    },
                    CheckConditionResult::ConditionAllowed => {
                        CheckConditionResult::ConditionAllowed
                    }
                }
            }
            ast::expression::UnaryOperator::Minus
            | ast::expression::UnaryOperator::Plus
            | ast::expression::UnaryOperator::BitNot
            | ast::expression::UnaryOperator::Typeof => {
                check_conditional(false, cx, &inner.argument, cached_results)
            }
            ast::expression::UnaryOperator::Void => condition_banned_and_falsy(),
            ast::expression::UnaryOperator::Delete | ast::expression::UnaryOperator::Nonnull => {
                use_type_to_check_conditional(cx, ttype)
            }
            ast::expression::UnaryOperator::Await => condition_allowed(),
        },
        ast::expression::ExpressionInner::Assignment { inner, .. } => {
            match inner.operator {
                // vanilla assignment (e.g. `a='test_str'`) is represented by `None`
                None => {
                    should_report_error_ref = false;
                    check_conditional(should_report_error, cx, &inner.right, cached_results)
                }
                // we don't check compound assignment (e.g. `++`, `--`, `+=`, ...)
                // because their value could change
                Some(_) => condition_allowed(),
            }
        }
        ast::expression::ExpressionInner::AsExpression { .. }
        | ast::expression::ExpressionInner::MetaProperty { .. }
        | ast::expression::ExpressionInner::Call { .. }
        | ast::expression::ExpressionInner::OptionalCall { .. }
        | ast::expression::ExpressionInner::Match { .. }
        | ast::expression::ExpressionInner::Binary { .. }
        | ast::expression::ExpressionInner::Identifier { .. }
        | ast::expression::ExpressionInner::Record { .. }
        | ast::expression::ExpressionInner::This { .. } => use_type_to_check_conditional(cx, ttype),
    };
    if should_report_error_ref {
        cached_results.insert(loc.dupe(), check_condition_result.dupe());
    }
    check_condition_result
}

fn detect_constant_conditions<'cx>(cx: &Context<'cx>) {
    let all_conditions = cx.get_all_conditions();
    let mut all_condition_results: ALocMap<CheckConditionResult> = ALocMap::new();
    for condition in all_conditions.iter() {
        check_conditional(true, cx, condition, &mut all_condition_results);
    }
    let mut banned_conditions: Vec<(ALoc, bool, bool, ConstantConditionKind, Option<Reason>)> =
        Vec::new();
    for (loc, check_condition_result) in all_condition_results.iter() {
        match check_condition_result {
            CheckConditionResult::ConditionAllowed => {}
            CheckConditionResult::ConditionBanned {
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            } => {
                banned_conditions.push((
                    loc.dupe(),
                    *is_truthy,
                    *show_warning,
                    *constant_condition_kind,
                    reason.dupe(),
                ));
            }
        }
    }
    for (loc, is_truthy, show_warning, constant_condition_kind, reason) in banned_conditions {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::EConstantCondition(Box::new(EConstantConditionData {
                loc,
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            })),
        );
    }
}

struct StrictComparisonResult {
    l_reason: Reason,
    r_reason: Reason,
    l_singleton_reason: Reason,
    r_singleton_reason: Reason,
    primary_loc: ALoc,
    kind: StrictComparisonKind,
}

fn check_strict_comparison<'cx>(
    cx: &Context<'cx>,
    all_strict_comparisons: &[(
        ALoc,
        (
            ast::expression::Expression<ALoc, (ALoc, Type)>,
            ast::expression::Expression<ALoc, (ALoc, Type)>,
        ),
    )],
) -> Vec<StrictComparisonResult> {
    all_strict_comparisons
        .iter()
        .filter_map(|(loc, (left_ast, right_ast))| {
            let (_, left_t) = left_ast.loc();
            let (_, right_t) = right_ast.loc();
            let l_reason = reason_of_t(left_t);
            let r_reason = reason_of_t(right_t);
            let left_conc_t =
                FlowJs::singleton_concrete_type_for_inspection(cx, l_reason, left_t).ok()?;
            let right_conc_t =
                FlowJs::singleton_concrete_type_for_inspection(cx, r_reason, right_t).ok()?;
            // the reason after concretization will contain more information.
            let l_singleton_reason = reason_of_t(&left_conc_t);
            let r_singleton_reason = reason_of_t(&right_conc_t);

            fn has_null_type<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
                match type_filter::not_null(cx, t.dupe()) {
                    type_filter::FilterResult { changed: true, .. } => true,
                    _ => false,
                }
            }
            // not_maybe will return emptyT if null or undefined is passed in.
            // emptyT is a subtype of any type, so we don't need to check the AST node
            // of `left` or `right`.
            fn filter_maybe_and_check_is_subtyping<'cx>(
                cx: &Context<'cx>,
                left: &Type,
                right: &Type,
            ) -> bool {
                let left_filtered = type_filter::not_maybe(cx, left.dupe()).type_;
                let right_filtered = type_filter::not_maybe(cx, right.dupe()).type_;
                let left_expanded = drop_generic(left.dupe());
                let right_expanded = drop_generic(right.dupe());
                FlowJs::speculative_subtyping_succeeds(cx, &right_filtered, &left_expanded)
                    || FlowJs::speculative_subtyping_succeeds(cx, &left_filtered, &right_expanded)
            }
            let banned = StrictComparisonResult {
                l_reason: l_reason.dupe(),
                r_reason: r_reason.dupe(),
                l_singleton_reason: l_singleton_reason.dupe(),
                r_singleton_reason: r_singleton_reason.dupe(),
                primary_loc: loc.dupe(),
                kind: StrictComparisonKind::StrictComparisonGeneral,
            };
            match (left_conc_t.deref(), right_conc_t.deref()) {
                (TypeInner::AnyT(_, _), _) | (_, TypeInner::AnyT(_, _)) => None,
                (TypeInner::DefT(_, d), _) if matches!(d.deref(), DefTInner::VoidT) => None,
                (_, TypeInner::DefT(_, d)) if matches!(d.deref(), DefTInner::VoidT) => None,
                (TypeInner::DefT(_, d1), TypeInner::DefT(_, d2))
                    if matches!(d1.deref(), DefTInner::EmptyT)
                        && matches!(d2.deref(), DefTInner::EmptyT) =>
                {
                    None
                }
                (TypeInner::DefT(_, d), _) if matches!(d.deref(), DefTInner::EmptyT) => {
                    Some(StrictComparisonResult {
                        kind: StrictComparisonKind::StrictComparisonEmpty {
                            empty_side: EmptySide::Left,
                        },
                        ..banned
                    })
                }
                (_, TypeInner::DefT(_, d)) if matches!(d.deref(), DefTInner::EmptyT) => {
                    Some(StrictComparisonResult {
                        kind: StrictComparisonKind::StrictComparisonEmpty {
                            empty_side: EmptySide::Right,
                        },
                        ..banned
                    })
                }
                (_, TypeInner::DefT(_, d))
                    if matches!(d.deref(), DefTInner::NullT)
                        && !has_null_type(cx, &left_conc_t) =>
                {
                    Some(StrictComparisonResult {
                        kind: StrictComparisonKind::StrictComparisonNull {
                            null_side: NullSide::Right,
                        },
                        ..banned
                    })
                }
                (TypeInner::DefT(_, d), _)
                    if matches!(d.deref(), DefTInner::NullT)
                        && !has_null_type(cx, &right_conc_t) =>
                {
                    Some(StrictComparisonResult {
                        kind: StrictComparisonKind::StrictComparisonNull {
                            null_side: NullSide::Left,
                        },
                        ..banned
                    })
                }
                _ => {
                    if filter_maybe_and_check_is_subtyping(cx, &left_conc_t, &right_conc_t) {
                        None
                    } else {
                        Some(banned)
                    }
                }
            }
        })
        .collect()
}

fn detect_invalid_strict_comparison<'cx>(cx: &Context<'cx>) {
    let all_strict_comparisons = cx.get_all_strict_comparisons();
    let all_strict_comparisons: Vec<_> = all_strict_comparisons.iter().cloned().collect();
    for result in check_strict_comparison(cx, &all_strict_comparisons) {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EComparison(Box::new(EComparisonData {
                r1: result.l_reason,
                r2: result.r_reason,
                loc_opt: Some(result.primary_loc),
                strict_comparison_opt: Some(StrictComparisonInfo {
                    left_precise_reason: result.l_singleton_reason,
                    right_precise_reason: result.r_singleton_reason,
                    strict_comparison_kind: result.kind,
                }),
            })),
        );
    }
}

fn detect_unnecessary_optional_chains<'cx>(cx: &Context<'cx>) {
    for (loc, lhs_reason) in cx.unnecessary_optional_chains().iter() {
        flow_js::add_output_non_speculating(
            cx,
            ErrorMessage::EUnnecessaryOptionalChain(Box::new((loc.dupe(), lhs_reason.dupe()))),
        );
    }
}

fn detect_unused_promises<'cx>(cx: &Context<'cx>) {
    for (loc, t, async_) in cx.maybe_unused_promises().iter() {
        let t = tvar_resolver::resolved_t(
            |r| any_t::make(AnySource::Untyped, r.dupe()),
            false,
            cx,
            t.dupe(),
        );
        let reason = flow_common::reason::mk_reason(
            VirtualReasonDesc::RCustom("unused promise lint".into()),
            loc.dupe(),
        );
        flow_js::flow_non_speculating(
            cx,
            (
                &t,
                &UseT::new(UseTInner::CheckUnusedPromiseT {
                    reason,
                    async_: *async_,
                }),
            ),
        );
    }
}

fn enforce_optimize<'cx>(cx: &Context<'cx>, loc: &ALoc, t: &Type) {
    let reason = flow_common::reason::mk_reason(
        VirtualReasonDesc::RTypeApp(Arc::new(VirtualReasonDesc::RType(Name::new(
            FlowSmolStr::new_inline("$Flow$EnforceOptimized"),
        )))),
        loc.dupe(),
    );
    let internal_t = Type::new(TypeInner::NominalT {
        reason: reason.dupe(),
        nominal_type: Rc::new(NominalType::new(NominalTypeInner {
            nominal_id: nominal::Id::InternalEnforceUnionOptimized,
            underlying_t: nominal::UnderlyingT::FullyOpaque,
            lower_t: None,
            upper_t: None,
            nominal_type_args: Rc::from(Vec::<(
                flow_common::subst_name::SubstName,
                Reason,
                Type,
                Polarity,
            )>::new()),
        })),
    });
    flow_js::flow_t_non_speculating(cx, (&internal_t, t));
}

fn check_union_opt<'cx>(cx: &Context<'cx>) {
    cx.iter_union_opt(|loc, t| enforce_optimize(cx, loc, t));
}

fn detect_import_export_errors<'cx>(
    cx: &Context<'cx>,
    program: &ast::Program<ALoc, ALoc>,
    metadata: &Metadata,
) {
    strict_es6_import_export::detect_errors(cx, program, metadata);
    for msg in module_exports_checker::check_program(program) {
        flow_js_utils::add_output_non_speculating(cx, msg);
    }
}

fn detect_non_voidable_properties<'cx>(cx: &Context<'cx>) {
    // This function approximately checks whether VoidT can flow to the provided
    // type without actually creating the flow so as not to disturb type inference.
    // Even though this is happening post-merge, it is possible to encounter an
    // unresolved tvar, in which case it conservatively returns false.
    fn is_voidable<'cx>(cx: &Context<'cx>, seen_ids: &mut TvarSeenSet<i32>, t: &Type) -> bool {
        match t.deref() {
            TypeInner::OpenT(tvar) => {
                let id = tvar.id() as i32;
                // tvar is recursive: conservatively assume it is non-voidable
                if seen_ids.contains(&id) {
                    false
                } else {
                    match flow_js_utils::possible_types(cx, id).as_slice() {
                        // tvar has no lower bounds: we conservatively assume it's non-voidable
                        // except in the special case when it also has no upper bounds
                        [] => flow_js_utils::possible_uses(cx, id).is_empty(),
                        // tvar is resolved: look at voidability of the resolved type
                        [t] => seen_ids.with_added(id, |seen_ids| is_voidable(cx, seen_ids, t)),
                        // tvar is unresolved: conservatively assume it is non-voidable
                        _ => false,
                    }
                }
            }
            // a union is voidable if any of its members are voidable
            TypeInner::UnionT(_, rep) => rep.members_iter().any(|t| is_voidable(cx, seen_ids, t)),
            // an intersection is voidable if all of its members are voidable
            TypeInner::IntersectionT(_, rep) => {
                rep.members_iter().all(|t| is_voidable(cx, seen_ids, t))
            }
            // trivially voidable
            TypeInner::MaybeT(_, _) | TypeInner::OptionalT { .. } | TypeInner::AnyT(_, _) => true,
            TypeInner::DefT(_, def_t) => matches!(
                def_t.deref(),
                DefTInner::VoidT
                    | DefTInner::MixedT(MixedFlavor::MixedEverything | MixedFlavor::MixedNonNull)
            ),
            // conservatively assume all other types are non-voidable
            _ => false,
        }
    }

    fn check_properties<'cx>(
        cx: &Context<'cx>,
        property_map_id: properties::Id,
        errors: &BTreeMap<FlowSmolStr, Vec<property_assignment::Error<ALoc>>>,
    ) {
        let pmap = cx.find_props(property_map_id);
        for (name, errs) in errors.iter() {
            let should_error = match pmap.get(&Name::new(name.dupe())) {
                Some(prop) => match &**prop {
                    PropertyInner::Field(fd) => {
                        !is_voidable(cx, &mut TvarSeenSet::new(), &fd.type_)
                    }
                    _ => true,
                },
                _ => true,
            };
            if should_error {
                for err in errs {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUninitializedInstanceProperty(err.loc.dupe(), err.desc),
                    );
                }
            }
        }
    }

    for check in cx.voidable_checks().iter() {
        check_properties(
            cx,
            check.public_property_map.dupe(),
            &check.errors.public_property_errors,
        );
        check_properties(
            cx,
            check.private_property_map.dupe(),
            &check.errors.private_property_errors,
        );
    }
}

fn check_polarity_fn<'cx>(cx: &Context<'cx>) {
    for (tparams, polarity, t) in cx.post_inference_polarity_checks().iter() {
        check_polarity::check_polarity(cx, tparams, *polarity, t).expect("Non speculating");
    }
}

fn check_general_post_inference_validations<'cx>(cx: &Context<'cx>) {
    for (t, use_t) in cx.post_inference_validation_flows().iter() {
        flow_js::flow_non_speculating(cx, (t, use_t));
    }
}

fn check_interface_merge_prop_conflicts<'cx>(cx: &Context<'cx>) {
    for (use_op, bad_t, good_t) in cx.interface_merge_unify_tasks().iter() {
        flow_js::unify_non_speculating(cx, Some(use_op.dupe()), bad_t, good_t);
    }
}

fn check_react_rules_fn<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    react_rules::check_react_rules(cx, tast);
}

fn check_haste_provider_conflict<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    let file_options = cx.file_options();
    let filename = cx.file();
    let Some(haste_name) = files::haste_name_opt(&file_options, filename) else {
        return;
    };
    let haste_name = FlowSmolStr::new(haste_name);
    let opts = cx.projects_options();
    let Some(projects) = FlowProjects::from_path(opts, filename.as_str())
        .and_then(|fp| opts.individual_projects_bitsets_from_common_project_bitset(fp))
    else {
        return;
    };
    let pos = flow_parser::loc::Position { line: 1, column: 0 };
    let loc_of_file = |f: &FileKey| -> ALoc {
        ALoc::of_loc(Loc {
            source: Some(f.dupe()),
            start: pos,
            end: pos,
        })
    };
    let add_duplicate_provider_error = |platform_specific_provider_file: &FileKey| {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::EDuplicateModuleProvider(Box::new(EDuplicateModuleProviderData {
                module_name: haste_name.dupe(),
                conflict: loc_of_file(filename),
                provider: loc_of_file(platform_specific_provider_file),
            })),
        );
    };
    if files::has_declaration_ext(filename) {
        // Suppose we have the setup of web project, native project, and web+native common code project.
        // We want to emit the same kinds of Haste module provider conflict error as if the same set of
        // code is covered by two flowconfigs.
        // (one include web only + common, one include native only + common)
        //
        // We have Foo.js.flow in common code.
        //
        // 1. If we have also have Foo.js in common code:
        //   a. We have Foo.js.flow or Foo.js in web code.
        //      This is not good, but we will complain anyways from Foo.js in common code.
        //   b. Nothing in web code. We are good
        // 2. If we don't have Foo.js in common code:
        //   a. We have Foo.js.flow or Foo.js in web code.
        //      This can be tolerated, because the Foo.js.flow file can act as common interface file.
        //   b. Nothing in web code. We are good.
        //
        // Therefore, in all possible cases, we don't have to emit an error.
        // .js.flow case - check for conformance
        for project in &projects {
            let specifier = FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                namespace: FlowProjects::to_bitset(*project),
                name: haste_name.dupe(),
                allow_implicit_platform_specific_import: false,
            };
            match cx.find_require(&specifier) {
                // There is no corresponding implementation file. This is allowed.
                ResolvedRequire::MissingModule => {}
                ResolvedRequire::UncheckedModule(platform_specific_provider_module_loc) => {
                    // If the corresponding platform specific file is untyped, then we assume it satisfies
                    // the common interface. However, we do need to make sure that it's not another
                    // .js.flow file
                    let platform_specific_provider_file =
                        ALoc::source(&platform_specific_provider_module_loc).unwrap();
                    if files::has_declaration_ext(platform_specific_provider_file) {
                        add_duplicate_provider_error(platform_specific_provider_file);
                    }
                }
                ResolvedRequire::TypedModule(f) => match f(cx, cx) {
                    Err(t) => {
                        // Similar to the case above,
                        // but in this case the module is any typed instead of untyped.
                        let platform_specific_provider_file =
                            ALoc::source(type_util::loc_of_t(&t)).unwrap();
                        if files::has_declaration_ext(platform_specific_provider_file) {
                            add_duplicate_provider_error(platform_specific_provider_file);
                        }
                    }
                    Ok(platform_specific_module_type) => {
                        let get_exports_t =
                            |is_common_interface_module: bool, reason: Reason, m: &ModuleType| {
                                // For conformance test, we only care about the value part
                                let (values_t, _) =
                                    flow_js_utils::import_module_ns_t_kit::on_module_t(
                                        cx,
                                        is_common_interface_module,
                                        reason,
                                        false,
                                        m,
                                    )
                                    .expect("on_module_t should not fail outside speculation");
                                values_t
                            };
                        let (self_sig_loc, self_module_type) =
                            module_info_analyzer::analyze_program(cx, tast);
                        let prog_aloc = tast.loc.dupe();
                        let interface_t = {
                            let reason = flow_common::reason::mk_reason(
                                VirtualReasonDesc::RCommonInterface,
                                tast.loc.dupe(),
                            );
                            get_exports_t(true, reason, &self_module_type)
                        };
                        let platform_specific_t = get_exports_t(
                            false,
                            platform_specific_module_type.module_reason.dupe(),
                            &platform_specific_module_type,
                        );
                        // We need to fully resolve the type to prevent tvar widening.
                        tvar_resolver::resolve(
                            cx,
                            tvar_resolver::default_no_lowers,
                            true,
                            &interface_t,
                        );
                        tvar_resolver::resolve(
                            cx,
                            tvar_resolver::default_no_lowers,
                            true,
                            &platform_specific_t,
                        );
                        let use_op = UseOp::Op(Arc::new(RootUseOp::ConformToCommonInterface(
                            Box::new(ConformToCommonInterfaceData {
                                self_sig_loc,
                                self_module_loc: prog_aloc,
                                originate_from_import: false,
                            }),
                        )));
                        flow_js::flow_non_speculating(
                            cx,
                            (
                                &platform_specific_t,
                                &UseT::new(UseTInner::UseT(use_op, interface_t)),
                            ),
                        );
                    }
                },
            }
        }
    } else {
        // We have Foo.js in common code.
        // We should error if we want Foo.js or Foo.js.flow in web only code.
        // non-.js.flow case - error if platform-specific file exists
        for project in &projects {
            let platform_specific_provider_module_loc = {
                let specifier = FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                    namespace: FlowProjects::to_bitset(*project),
                    name: haste_name.dupe(),
                    allow_implicit_platform_specific_import: false,
                };
                match cx.find_require(&specifier) {
                    ResolvedRequire::MissingModule => None,
                    ResolvedRequire::UncheckedModule(loc) => Some(loc),
                    ResolvedRequire::TypedModule(f) => match f(cx, cx) {
                        Ok(m) => Some(m.module_reason.loc().dupe()),
                        Err(t) => Some(type_util::loc_of_t(&t).dupe()),
                    },
                }
            };
            if let Some(platform_specific_provider_module_loc) =
                platform_specific_provider_module_loc
            {
                let platform_specific_provider_file =
                    ALoc::source(&platform_specific_provider_module_loc).unwrap();
                add_duplicate_provider_error(platform_specific_provider_file);
            }
        }
    }
}

fn validate_strict_boundary_import_pattern_opt_outs<'cx>(cx: &Context<'cx>) {
    let validate = |error_loc: &ALoc, import_specifier: &str, projects: &[FlowProjects]| {
        let get_exports_t = |specifier: &FlowImportSpecifier| -> Option<Type> {
            match cx.find_require(specifier) {
                ResolvedRequire::MissingModule => None,
                ResolvedRequire::UncheckedModule(_) => None,
                ResolvedRequire::TypedModule(f) => match f(cx, cx) {
                    Err(_) => None,
                    Ok(m) => {
                        if m.module_export_types.has_every_named_export {
                            Some(any_t::at(AnySource::Untyped, error_loc.dupe()))
                        } else {
                            // For conformance test, we only care about the value part
                            let (values_t, _) = flow_js_utils::import_module_ns_t_kit::on_module_t(
                                cx,
                                false,
                                m.module_reason.dupe(),
                                false,
                                &m,
                            )
                            .expect("on_module_t should not fail outside speculation");
                            Some(values_t)
                        }
                    }
                },
            }
        };
        match get_exports_t(&FlowImportSpecifier::userland(FlowSmolStr::new(
            import_specifier,
        ))) {
            // If we cannot resolve the actual import, we will already error.
            // Therefore, we don't have to error again.
            None => {}
            Some(acting_common_interface_module_t) => {
                let _file_options = cx.file_options();
                let _projects_options = cx.projects_options();
                let missing_platforms: Vec<BTreeSet<FlowSmolStr>> = projects
                    .iter()
                    .filter_map(|project| {
                        let specifier = FlowImportSpecifier::HasteImportWithSpecifiedNamespace {
                            namespace: FlowProjects::to_bitset(*project),
                            name: FlowSmolStr::new(import_specifier),
                            allow_implicit_platform_specific_import: true,
                        };
                        match get_exports_t(&specifier) {
                            // If we cannot find the counterparts in other Haste namespaces, we need to error.
                            // Consider the following example:
                            //
                            // ```
                            // // file:common.js
                            // import 'foo';
                            //
                            // // file: web/foo.js
                            // // missing file: native/foo.js
                            // ```
                            //
                            // The above example will cause us unable to find `foo` from `native` namespace.
                            // We should error, because it will cause missing-module error in a flowconfig that
                            // includes all of common code + `native/`.
                            None => platform_set::available_platforms(
                                &_file_options,
                                _projects_options,
                                cx.file().as_str(),
                                _projects_options
                                    .multi_platform_ambient_supports_platform_for_project(*project)
                                    .as_deref(),
                            )
                            .map(|ps| ps.to_platform_string_set(&_file_options)),
                            Some(alternative_module_t) => {
                                tvar_resolver::resolve(
                                    cx,
                                    tvar_resolver::default_no_lowers,
                                    true,
                                    &acting_common_interface_module_t,
                                );
                                tvar_resolver::resolve(
                                    cx,
                                    tvar_resolver::default_no_lowers,
                                    true,
                                    &alternative_module_t,
                                );
                                let use_op =
                                    UseOp::Op(Arc::new(RootUseOp::ConformToCommonInterface(
                                        Box::new(ConformToCommonInterfaceData {
                                            self_sig_loc: error_loc.dupe(),
                                            self_module_loc: error_loc.dupe(),
                                            originate_from_import: true,
                                        }),
                                    )));
                                flow_js::flow_non_speculating(
                                    cx,
                                    (
                                        &alternative_module_t,
                                        &UseT::new(UseTInner::UseT(
                                            use_op,
                                            acting_common_interface_module_t.dupe(),
                                        )),
                                    ),
                                );
                                None
                            }
                        }
                    })
                    .collect();
                let mut all_missing = BTreeSet::new();
                for ps in &missing_platforms {
                    all_missing.extend(ps.iter().duped());
                }
                if !all_missing.is_empty() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EMissingPlatformSupport(Box::new(
                            EMissingPlatformSupportData {
                                loc: error_loc.dupe(),
                                missing_platforms: all_missing,
                            },
                        )),
                    );
                }
            }
        }
    };
    for (error_loc, import_specifier, projects) in cx
        .post_inference_projects_strict_boundary_import_pattern_opt_outs_validations()
        .iter()
    {
        validate(error_loc, import_specifier, projects);
    }
}

fn check_multiplatform_conformance<'cx>(
    cx: &Context<'cx>,
    ast: &ast::Program<ALoc, ALoc>,
    tast: &ast::Program<ALoc, (ALoc, Type)>,
) {
    let prog_aloc = &ast.loc;
    let filename = cx.file();
    let file_options = cx.file_options();
    let file_loc = ALoc::of_loc(Loc {
        source: Some(filename.dupe()),
        start: flow_parser::loc::Position { line: 0, column: 0 },
        end: flow_parser::loc::Position { line: 0, column: 0 },
    });
    match files::relative_interface_mref_of_possibly_platform_specific_file(&file_options, filename)
    {
        Some(imported_interface_module_name) => {
            let specifier =
                FlowImportSpecifier::userland(FlowSmolStr::new(&imported_interface_module_name));
            match cx.find_require(&specifier) {
                // It's ok if a platform speicific implementation file doesn't have an interface.
                // It just makes the module non-importable without platform extension.
                ResolvedRequire::MissingModule | ResolvedRequire::UncheckedModule(_) => {}
                ResolvedRequire::TypedModule(interface_module_f) => {
                    let get_exports_t = |is_common_interface_module: bool,
                                         reason: Reason,
                                         m_opt: Option<&ModuleType>|
                     -> Type {
                        match m_opt {
                            Some(m) => {
                                if is_common_interface_module {
                                    if let (Some(avail), Some(mod_avail)) =
                                        (cx.available_platforms(), &m.module_available_platforms)
                                    {
                                        if PlatformSet::no_overlap(avail, mod_avail) {
                                            // If the current module's platform has no overlap with the common interface
                                            // file's platforms, then the common interface file is irrelevant. We give it an
                                            // any type to make conformance check always passing.
                                            return any_t::make(AnySource::Untyped, reason);
                                        }
                                    }
                                }
                                // For conformance test, we only care about the value part
                                let (values_t, _) =
                                    flow_js_utils::import_module_ns_t_kit::on_module_t(
                                        cx,
                                        is_common_interface_module,
                                        reason.dupe(),
                                        false,
                                        m,
                                    )
                                    .unwrap();
                                values_t
                            }
                            None => any_t::make(AnySource::Untyped, reason),
                        }
                    };
                    let interface_t = {
                        let reason = flow_common::reason::mk_reason(
                            VirtualReasonDesc::RCommonInterface,
                            prog_aloc.dupe(),
                        );
                        let m_result = interface_module_f(cx, cx);
                        get_exports_t(true, reason, m_result.ok().as_ref())
                    };
                    let (self_sig_loc, self_module_type) =
                        module_info_analyzer::analyze_program(cx, tast);
                    let self_t = get_exports_t(
                        false,
                        self_module_type.module_reason.dupe(),
                        Some(&self_module_type),
                    );
                    // We need to fully resolve the type to prevent tvar widening.
                    tvar_resolver::resolve(
                        cx,
                        tvar_resolver::default_no_lowers,
                        true,
                        &interface_t,
                    );
                    tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &self_t);
                    let use_op = UseOp::Op(Arc::new(RootUseOp::ConformToCommonInterface(
                        Box::new(ConformToCommonInterfaceData {
                            self_sig_loc,
                            self_module_loc: prog_aloc.dupe(),
                            originate_from_import: false,
                        }),
                    )));
                    flow_js::flow_non_speculating(
                        cx,
                        (&self_t, &UseT::new(UseTInner::UseT(use_op, interface_t))),
                    );
                }
            }
        }
        None => {
            match platform_set::platform_specific_implementation_mrefs_of_possibly_interface_file(
                &file_options,
                cx.available_platforms(),
                filename,
            ) {
                None => {}
                Some((
                    unconditional_extensions,
                    grouped_extensions_with_conditional_extensions,
                )) => {
                    let module_exists = |mref: &str| -> bool {
                        match cx
                            .find_require(&FlowImportSpecifier::userland(FlowSmolStr::new(mref)))
                        {
                            ResolvedRequire::TypedModule(_)
                            | ResolvedRequire::UncheckedModule(_) => true,
                            ResolvedRequire::MissingModule => false,
                        }
                    };
                    if !cx.has_explicit_supports_platform()
                        && unconditional_extensions.iter().all(|m| !module_exists(m))
                        && grouped_extensions_with_conditional_extensions.iter().all(
                            |(grouped, conditional)| {
                                !module_exists(grouped)
                                    && conditional.iter().all(|m| !module_exists(m))
                            },
                        )
                    {
                        // We are fine if no implementation file exist.
                        // The .js.flow file might be declaring a builtin module.
                    } else {
                        for name in &unconditional_extensions {
                            if !module_exists(name) {
                                flow_js_utils::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed(
                                        Box::new(
                                            EPlatformSpecificImplementationModuleLookupFailedData {
                                                loc: file_loc.dupe(),
                                                name: FlowSmolStr::new(name),
                                            },
                                        ),
                                    ),
                                );
                            }
                        }
                        for (grouped, conditional) in
                            &grouped_extensions_with_conditional_extensions
                        {
                            if !module_exists(grouped) {
                                for name in conditional {
                                    if !module_exists(name) {
                                        flow_js_utils::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed(Box::new(EPlatformSpecificImplementationModuleLookupFailedData {
                                                loc: file_loc.dupe(), name:  FlowSmolStr::new(name),
                                            }))
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn check_spread_prop_keys<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    if !cx.ban_spread_key_props() {
        return;
    }
    fn find_key_prop<'cx>(cx: &Context<'cx>, seen: &mut TvarSeenSet<i32>, spread: &ALoc, t: &Type) {
        match t.deref() {
            TypeInner::OpenT(tvar) if seen.contains(&(tvar.id() as i32)) => {}
            TypeInner::OpenT(tvar) => {
                let id = tvar.id() as i32;
                seen.with_added(id, |seen| {
                    for t in flow_js_utils::possible_types(cx, id) {
                        find_key_prop(cx, seen, spread, &t);
                    }
                });
            }
            TypeInner::AnnotT(_, t, _) => find_key_prop(cx, seen, spread, t),
            TypeInner::UnionT(_, rep) => {
                for t in rep.members_iter() {
                    find_key_prop(cx, seen, spread, t);
                }
            }
            TypeInner::IntersectionT(_, rep) => {
                for t in rep.members_iter() {
                    find_key_prop(cx, seen, spread, t);
                }
            }
            TypeInner::OptionalT { type_, .. } => find_key_prop(cx, seen, spread, type_),
            TypeInner::MaybeT(_, ty) => find_key_prop(cx, seen, spread, ty),
            TypeInner::DefT(r, def_t) => {
                if let DefTInner::ObjT(obj_t) = def_t.deref() {
                    let prop = cx.get_prop(
                        obj_t.props_tmap.dupe(),
                        &Name::new(FlowSmolStr::new_inline("key")),
                    );
                    if let Some(prop) = prop {
                        let loc = property::first_loc(&prop).unwrap_or_else(|| r.loc().dupe());
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EKeySpreadProp(Box::new(EKeySpreadPropData {
                                spread: spread.dupe(),
                                loc,
                            })),
                        );
                    }
                }
            }
            _ => {}
        }
    }
    struct SpreadPropKeyChecker<'a, 'cx> {
        cx: &'a Context<'cx>,
    }
    impl<'ast> ast_visitor::AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, !>
        for SpreadPropKeyChecker<'_, '_>
    {
        fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
            loc
        }
        fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
            &type_.0
        }
        fn jsx_spread_attribute(
            &mut self,
            attr: &'ast ast::jsx::SpreadAttribute<ALoc, (ALoc, Type)>,
        ) -> Result<(), !> {
            let (spread, ty) = attr.argument.loc();
            find_key_prop(self.cx, &mut TvarSeenSet::new(), spread, ty);
            ast_visitor::jsx_spread_attribute_default(self, attr)
        }
    }
    let mut checker = SpreadPropKeyChecker { cx };
    let Ok(()) = checker.program(tast);
}

fn check_match_exhaustiveness<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    if !cx.enable_pattern_matching() {
        return;
    }
    struct MatchExhaustivenessChecker<'a, 'cx> {
        cx: &'a Context<'cx>,
    }
    impl<'ast> ast_visitor::AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, !>
        for MatchExhaustivenessChecker<'_, '_>
    {
        fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
            loc
        }
        fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
            &type_.0
        }
        fn match_<B>(
            &mut self,
            loc: &'ast ALoc,
            m: &'ast ast::match_::Match<ALoc, (ALoc, Type), B>,
            on_case_body: impl FnMut(&mut Self, &'ast B) -> Result<(), !>,
        ) -> Result<(), !> {
            let match_loc = &m.match_keyword_loc;
            let (_, arg_t) = m.arg.loc();
            let patterns: Vec<_> = m
                .cases
                .iter()
                .map(|case| (case.pattern.clone(), case.guard.is_some()))
                .collect();
            exhaustive::analyze(self.cx, match_loc.dupe(), &patterns, arg_t);
            ast_visitor::match_default(self, loc, m, on_case_body)
        }
    }
    let mut checker = MatchExhaustivenessChecker { cx };
    let Ok(()) = checker.program(tast);
}

fn emit_refinement_information_as_errors<'cx>(cx: &Context<'cx>) {
    fn emit_refined_locations_info<'cx>(cx: &Context<'cx>) {
        for (refined_loc, refining_locs) in cx.refined_locations().iter() {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EDevOnlyRefinedLocInfo(Box::new(EDevOnlyRefinedLocInfoData {
                    refined_loc: refined_loc.dupe(),
                    refining_locs: refining_locs.iter().map(|l| l.dupe()).collect(),
                })),
            );
        }
    }
    fn emit_invalidated_locations_info<'cx>(cx: &Context<'cx>) {
        for (read_loc, invalidation_info) in cx.aggressively_invalidated_locations().iter() {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EDevOnlyInvalidatedRefinementInfo(Box::new(
                    EDevOnlyInvalidatedRefinementInfoData {
                        read_loc: read_loc.dupe(),
                        invalidation_info: invalidation_info
                            .iter()
                            .map(|(k, v)| (k.dupe(), *v))
                            .collect(),
                    },
                )),
            );
        }
    }
    if cx.dev_only_refinement_info_as_errors() {
        emit_refined_locations_info(cx);
        emit_invalidated_locations_info(cx);
    }
}

// (* let check_assert_operator cx tast = *)
fn check_assert_operator<'cx>(cx: &Context<'cx>, tast: &ast::Program<ALoc, (ALoc, Type)>) {
    // (* if Context.assert_operator_enabled cx then ignore (checker#program tast ...) *)
    if !cx.assert_operator_enabled() {
        return;
    }
    // (* let check_specialized_assert_operator ~op_reason expr = *)
    fn check_specialized_assert_operator<'cx>(
        cx: &Context<'cx>,
        op_reason: &Reason,
        expr: &ast::expression::Expression<ALoc, (ALoc, Type)>,
    ) {
        let obj_reason = reason::mk_typed_expression_reason(expr);
        match expr.deref() {
            ast::expression::ExpressionInner::Member { loc: _, inner }
                if let ast::expression::member::Property::PropertyIdentifier(id) =
                    &inner.property =>
            {
                let (_, obj_t) = inner.object.loc();
                let name = &id.name;
                type_assertions::check_specialized_assert_operator_property(
                    cx,
                    op_reason,
                    &obj_reason,
                    obj_t,
                    name,
                );
            }
            ast::expression::ExpressionInner::OptionalMember { loc: _, inner }
                if let ast::expression::member::Property::PropertyIdentifier(id) =
                    &inner.member.property =>
            {
                let (_, obj_t) = inner.member.object.loc();
                let name = &id.name;
                type_assertions::check_specialized_assert_operator_property(
                    cx,
                    op_reason,
                    &obj_reason,
                    obj_t,
                    name,
                );
            }
            ast::expression::ExpressionInner::Member { loc: _, inner }
                if let ast::expression::member::Property::PropertyExpression(prop_expr) =
                    &inner.property =>
            {
                let (_, obj_t) = inner.object.loc();
                let (_, prop_t) = prop_expr.loc();
                type_assertions::check_specialized_assert_operator_lookup(
                    cx,
                    op_reason,
                    &obj_reason,
                    obj_t,
                    prop_t,
                );
            }
            ast::expression::ExpressionInner::OptionalMember { loc: _, inner }
                if let ast::expression::member::Property::PropertyExpression(prop_expr) =
                    &inner.member.property =>
            {
                let (_, obj_t) = inner.member.object.loc();

                let (_, prop_t) = prop_expr.loc();
                type_assertions::check_specialized_assert_operator_lookup(
                    cx,
                    op_reason,
                    &obj_reason,
                    obj_t,
                    prop_t,
                );
            }
            _ => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EIllegalAssertOperator(Box::new(EIllegalAssertOperatorData {
                        op: op_reason.dupe(),
                        obj: obj_reason,
                        specialized: true,
                    })),
                );
            }
        }
    }
    fn check_assert_operator_useful<'cx>(
        cx: &Context<'cx>,
        op_reason: &Reason,
        expr: &ast::expression::Expression<ALoc, (ALoc, Type)>,
    ) {
        let obj_reason = reason::mk_typed_expression_reason(expr);
        let (_, t) = expr.loc();
        let legal = type_assertions::check_assert_operator_nullable(cx, t)
            || match expr.deref() {
                ast::expression::ExpressionInner::Member { loc: _, inner } => {
                    let (_, obj_t) = inner.object.loc();
                    type_assertions::check_assert_operator_implicitly_nullable(cx, obj_t)
                }
                ast::expression::ExpressionInner::OptionalMember { loc: _, inner } => {
                    let (_, obj_t) = inner.member.object.loc();
                    type_assertions::check_assert_operator_implicitly_nullable(cx, obj_t)
                }
                _ => false,
            };
        if !legal {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EIllegalAssertOperator(Box::new(EIllegalAssertOperatorData {
                    op: op_reason.dupe(),
                    obj: obj_reason,
                    specialized: false,
                })),
            );
        }
    }
    fn check<'cx>(
        cx: &Context<'cx>,
        op_reason: &Reason,
        expr: &ast::expression::Expression<ALoc, (ALoc, Type)>,
    ) {
        check_assert_operator_useful(cx, op_reason, expr);
        if cx.assert_operator_specialized() {
            check_specialized_assert_operator(cx, op_reason, expr);
        }
    }

    struct AssertOperatorChecker<'a, 'cx> {
        cx: &'a Context<'cx>,
    }
    impl<'ast> ast_visitor::AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, !>
        for AssertOperatorChecker<'_, '_>
    {
        fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
            loc
        }
        fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
            &type_.0
        }
        fn expression(
            &mut self,
            expr: &'ast ast::expression::Expression<ALoc, (ALoc, Type)>,
        ) -> Result<(), !> {
            match expr.deref() {
                ast::expression::ExpressionInner::Unary { loc: _, inner }
                    if inner.operator == ast::expression::UnaryOperator::Nonnull =>
                {
                    let op_reason = reason::mk_typed_expression_reason(expr);
                    check(self.cx, &op_reason, &inner.argument);
                }
                ast::expression::ExpressionInner::OptionalCall { loc: _, inner }
                    if inner.optional == ast::expression::OptionalCallKind::AssertNonnull =>
                {
                    let target = &inner.call.callee;
                    let op_reason = reason::mk_typed_expression_reason(target);
                    check(self.cx, &op_reason, target);
                }
                ast::expression::ExpressionInner::OptionalMember { loc: _, inner }
                    if inner.optional == ast::expression::OptionalMemberKind::AssertNonnull =>
                {
                    let target = &inner.member.object;
                    let op_reason = reason::mk_typed_expression_reason(target);
                    check(self.cx, &op_reason, target);
                }
                _ => {}
            }
            ast_visitor::expression_default(self, expr)
        }
    }

    let mut checker = AssertOperatorChecker { cx };
    let Ok(()) = checker.program(tast);
}

fn convert_type_to_type_desc_in_errors<'cx>(
    cx: &Context<'cx>,
    file_sig: Arc<FileSig>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
) {
    let genv = crate::ty_normalizer_flow::mk_genv(
        flow_typing_ty_normalizer::env::Options {
            optimize_types: false,
            ..flow_typing_ty_normalizer::env::Options::default()
        },
        cx,
        Some(typed_ast),
        file_sig,
    );
    type TypeOrTypeDesc = type_or_type_desc::TypeOrTypeDescT<ALoc>;
    let convert = |tod: TypeOrTypeDesc| -> TypeOrTypeDesc {
        match &tod {
            TypeOrTypeDesc::Type(t) => TypeOrTypeDesc::TypeDesc(
                flow_typing_ty_normalizer::no_flow::type_to_desc_for_errors(&genv, t),
            ),
            TypeOrTypeDesc::TypeDesc(d) => TypeOrTypeDesc::TypeDesc(d.clone()),
        }
    };
    let errors = cx.errors().dupe();
    let converted = errors.map(|err| FlowError::convert_type_to_type_desc(&convert, err));
    cx.reset_errors(converted);
}

pub fn get_lint_severities(
    metadata: &Metadata,
    strict_mode: &StrictModeSettings,
    lint_severities: LintSettings<Severity>,
) -> LintSettings<Severity> {
    if metadata.overridable.strict || metadata.overridable.strict_local {
        strict_mode.fold(lint_severities, |lint_kind, mut settings| {
            settings.set_value(lint_kind, (Severity::Err, None));
            settings
        })
    } else {
        lint_severities
    }
}

// Post-merge errors.
//
// At this point, all dependencies have been merged and the component has been
// linked together. Any constraints should have already been evaluated, which
// means we can complain about things that either haven't happened yet, or
// which require complete knowledge of tvar bounds.

pub fn post_merge_checks<'cx>(
    cx: &Context<'cx>,
    file_sig: Arc<FileSig>,
    ast: &ast::Program<ALoc, ALoc>,
    tast: &ast::Program<ALoc, (ALoc, Type)>,
    metadata: &Metadata,
) {
    force_lazy_tvars(cx);
    check_react_rules_fn(cx, tast);
    if !cx.is_lib_file() {
        check_haste_provider_conflict(cx, tast);
        check_multiplatform_conformance(cx, ast, tast);
        validate_strict_boundary_import_pattern_opt_outs(cx);
    }
    check_polarity_fn(cx);
    check_general_post_inference_validations(cx);
    check_interface_merge_prop_conflicts(cx);
    detect_sketchy_null_checks(cx, tast);
    detect_non_voidable_properties(cx);
    detect_test_prop_misses(cx);
    detect_unnecessary_optional_chains(cx);
    detect_constant_conditions(cx);
    detect_import_export_errors(cx, ast, metadata);
    detect_invalid_strict_comparison(cx);
    detect_unused_promises(cx);
    check_union_opt(cx);
    check_spread_prop_keys(cx, tast);
    check_match_exhaustiveness(cx, tast);
    check_assert_operator(cx, tast);
    emit_refinement_information_as_errors(cx);
    convert_type_to_type_desc_in_errors(cx, file_sig, tast);
}

// Check will lazily create types for the checked file's dependencies. These
// types are created in the dependency's context and need to be copied into the
// checked file's context.
//
// This visitor walks a type in the dependency's context (src_cx) and copies
// any tvars, property maps, evaluated types, etc. into the check file's context
// (dst_cx).
//
// When calculating a direct dependency's types, we might also need to construct
// types for a transitive dependency. These types are similarly created in the
// transitive dependency's context, then copied into the dependency's context,
// and so on.
//
// Finally, due to cycles, it's possile that src_cx and dst_cx share the same
// component cx, and thus have the same tvar graph, property maps, etc. Happily,
// this does not complicate the implementation, as the mem checks and early
// returns on each method override are sufficient.
//
// Crucially, this copying process is shallow. We only copy what is necessary to
// interpret a given type.

// (* let copier = *)
// (*   let open Type in *)
// (*   let open Constraint in *)
// (*   object (self) *)
// (*     inherit [Context.t] Type_visitor.t as super *)
struct CopierVisitor<'b, 'a> {
    src_cx: &'b Context<'a>,
    dst_cx: &'b Context<'a>,
}

impl<'b, 'a> CopierVisitor<'b, 'a> {
    fn new(src_cx: &'b Context<'a>, dst_cx: &'b Context<'a>) -> Self {
        CopierVisitor { src_cx, dst_cx }
    }
}

impl<'b, 'a> type_visitor::TypeVisitor<()> for CopierVisitor<'b, 'a> {
    // Copying a tvar produces a FullyResolved tvar in the dst cx, which
    // contains an unevaluated thunk. The laziness here makes the copying
    // shallow. Note that the visitor stops at root tvars here and only resumes
    // if the thunk is forced.
    fn tvar<'cx>(&mut self, _cx: &Context<'cx>, pole: Polarity, _acc: (), _r: &Reason, id: u32) {
        let id = id as i32;
        let src_cx = self.src_cx;
        let dst_cx = self.dst_cx;
        if dst_cx.graph().borrow().get(&id).is_some() {
            return;
        }
        // (* let (root_id, constraints) = Context.find_constraints src_cx id in *)
        let (root_id, constraints) = src_cx.find_constraints(id);
        // (* if id == root_id then *)
        if id == root_id {
            let state = match constraints {
                Constraints::Unresolved(_) | Constraints::Resolved(_) => {
                    panic!("unexpected unresolved constraint")
                }
                Constraints::FullyResolved(s) => s.copy(
                    src_cx,
                    move |src_cx, r| src_cx.on_cyclic_tvar_error(r.dupe()),
                    move |src_cx, dst_cx, t| {
                        let mut visitor = CopierVisitor::new(src_cx, dst_cx);
                        visitor.type_(dst_cx, pole, (), t);
                    },
                ),
            };
            dst_cx
                .graph()
                .borrow_mut()
                .insert(id, Node::create_root(Constraints::FullyResolved(state)));
        } else {
            dst_cx
                .graph()
                .borrow_mut()
                .insert(id, Node::create_goto(root_id));
            self.tvar(_cx, pole, (), _r, root_id as u32);
        }
    }

    fn props<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, _acc: (), id: properties::Id) {
        let dst_cx = self.dst_cx;
        if dst_cx.has_property_map(&id) {
            return;
        }
        let props = self.src_cx.find_props(id.dupe());
        dst_cx.add_property_map(id.dupe(), props.dupe());
        type_visitor::props_default(self, cx, pole, (), id);
    }

    fn call_prop<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, _acc: (), id: i32) {
        let dst_cx = self.dst_cx;
        if dst_cx.has_call_prop(&id) {
            return;
        }
        let t = self.src_cx.find_call(id);
        dst_cx.add_call_prop(id, t.dupe());
        type_visitor::call_prop_default(self, cx, pole, (), id);
    }

    fn exports<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, _acc: (), id: exports::Id) {
        let dst_cx = self.dst_cx;
        if dst_cx.has_export_map(&id) {
            return;
        }
        let map = self.src_cx.find_exports(id);
        dst_cx.add_export_map(id.dupe(), map.dupe());
        type_visitor::exports_default(self, cx, pole, (), id);
    }

    fn eval_id<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, _acc: (), id: eval::Id) {
        let src_cx = self.src_cx;
        let dst_cx = self.dst_cx;
        match src_cx.evaluated().get(&id).duped() {
            None => {}
            Some(t) => {
                if dst_cx.evaluated().contains_key(&id) {
                    return;
                }
                let mut evaluated = dst_cx.evaluated();
                evaluated.insert(id.dupe(), t.dupe());
                dst_cx.set_evaluated(evaluated);
                type_visitor::eval_id_default(self, cx, pole, (), id);
            }
        }
    }
}

pub fn copy_into<'cx>(
    src_cx: &Context<'cx>,
    dst_cx: &Context<'cx>,
    f: &dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type>,
) -> Result<ModuleType, Type> {
    // Chain the merge destination context: if dst_cx already has a
    // merge_dst_cx set (from an outer copy_into), use that as the ultimate
    // destination for error reporting. Otherwise use dst_cx itself.
    // This chaining preserves the original importing file's context through
    // nested star exports.
    let effective_dst = dst_cx.merge_dst_cx().unwrap_or_else(|| dst_cx.dupe());
    src_cx.set_merge_dst_cx(&effective_dst);
    // Pass effective_dst to f for error reporting. CopierVisitor below still
    // uses dst_cx for type copying (the immediate destination context).
    let m = f(src_cx, &effective_dst);
    // Note: we do NOT clear merge_dst_cx after copy_into. Lazy closures
    // created during f() may be forced later (during the check phase) and
    // still need merge_dst_cx to route errors to the correct destination.
    match m {
        Ok(m) => {
            let mut copier = CopierVisitor::new(src_cx, dst_cx);
            copier.export_types(src_cx, Polarity::Positive, (), &m.module_export_types);
            Ok(m)
        }
        Err(e) => Err(copied(dst_cx, src_cx, &e)),
    }
}

pub fn copied<'cx>(dst_cx: &Context<'cx>, src_cx: &Context<'cx>, t: &Type) -> Type {
    let mut copier = CopierVisitor::new(src_cx, dst_cx);
    copier.type_(src_cx, Polarity::Positive, (), t);
    t.dupe()
}

pub fn module_type_copied<'cx>(
    dst_cx: &Context<'cx>,
    src_cx: &Context<'cx>,
    m: &ModuleType,
) -> ModuleType {
    let mut copier = CopierVisitor::new(src_cx, dst_cx);
    copier.export_types(src_cx, Polarity::Positive, (), &m.module_export_types);
    m.dupe()
}

fn merge_libs_from_ordered_asts(
    sig_opts: &TypeSigOptions,
    ordered_asts: &[&ast::Program<Loc, Loc>],
) -> (
    flow_error::ErrorSet,
    compact_table::Table<Loc>,
    flow_type_sig::packed_type_sig::Builtins<Loc>,
) {
    let arenas = bumpalo::Bump::new();
    let (builtin_errors, builtin_locs, builtins) =
        type_sig_utils::parse_and_pack_builtins(sig_opts, &arenas, ordered_asts);
    let builtin_errors: Vec<FlowError<ALoc>> = builtin_errors
        .into_iter()
        .filter_map(|err| match err {
            type_sig::Errno::SigError(e) => {
                let e = e.map(&mut (), |_, l| ALoc::of_loc(builtin_locs.get(*l).dupe()));
                let msg = ErrorMessage::ESignatureVerification(e);
                let loc_of_msg = msg.loc_of_msg();
                let source_file = loc_of_msg
                    .as_ref()
                    .and_then(|loc| ALoc::source(loc))
                    .unwrap();
                Some(flow_error::error_of_msg(source_file.dupe(), msg))
            }
            type_sig::Errno::BindingValidationError(e) => {
                let e = e.map(&mut (), |_, l| ALoc::of_loc(builtin_locs.get(*l).dupe()));
                let msg = ErrorMessage::ESignatureBindingValidation(e);
                let loc_of_msg = msg.loc_of_msg();
                let source_file = loc_of_msg
                    .as_ref()
                    .and_then(|loc| ALoc::source(loc))
                    .unwrap();
                Some(flow_error::error_of_msg(source_file.dupe(), msg))
            }
            type_sig::Errno::CheckError => None,
        })
        .collect();
    let builtin_error_set = flow_error::ErrorSet::from_iter(builtin_errors);
    (builtin_error_set, builtin_locs, builtins)
}

pub fn merge_lib_files(
    project_opts: &flow_common::flow_projects::ProjectsOptions,
    sig_opts: &TypeSigOptions,
    ordered_asts_with_scoped_projects: &[(Option<String>, Arc<ast::Program<Loc, Loc>>)],
) -> (flow_error::ErrorSet, MasterContext) {
    let builtin_leader_file_key = ordered_asts_with_scoped_projects
        .first()
        .and_then(|(_, ast)| ast.loc.source.dupe());

    let mut grouped: Vec<(Option<FlowProjects>, Vec<&ast::Program<Loc, Loc>>)> = Vec::new();
    for (scoped_project_key, ast) in ordered_asts_with_scoped_projects.iter() {
        let scoped_key = scoped_project_key
            .as_ref()
            .map(|k| FlowProjects::from_project_str(project_opts, k));
        let found = grouped.iter_mut().find(|(k, _)| k == &scoped_key);
        match found {
            None => grouped.push((scoped_key, vec![ast.as_ref()])),
            Some((_, list)) => list.push(ast.as_ref()),
        }
    }
    let non_scoped_asts: Option<Vec<_>> = grouped
        .iter()
        .find(|(k, _)| k.is_none())
        .map(|(_, asts)| asts.to_vec());
    // Make every scoped asts include all of non-scoped asts at the end
    let grouped: Vec<_> = grouped
        .into_iter()
        .map(|(key, mut asts)| {
            if key.is_some() {
                if let Some(non_scoped) = &non_scoped_asts {
                    asts.extend(non_scoped.iter().copied());
                }
            }
            (key, asts)
        })
        .collect();
    let mut all_errors = flow_error::ErrorSet::default();
    let scoped_builtins: Vec<_> = grouped
        .iter()
        .map(|(key, asts)| {
            let (builtin_errors, builtin_locs, builtins) =
                merge_libs_from_ordered_asts(sig_opts, asts);
            all_errors = flow_error::ErrorSet::union(&all_errors, &builtin_errors);
            (
                *key,
                BuiltinsGroup {
                    builtin_locs: Arc::new(builtin_locs),
                    builtins: Arc::new(builtins),
                },
            )
        })
        .collect();
    let mut unscoped_list = Vec::new();
    let mut scoped_list = Vec::new();
    for (key, bg) in scoped_builtins {
        match key {
            None => unscoped_list.push(bg),
            Some(k) => scoped_list.push((k, bg)),
        }
    }
    match builtin_leader_file_key {
        None => (all_errors, MasterContext::EmptyMasterContext),
        Some(builtin_leader_file_key) => {
            let unscoped_builtins = match unscoped_list.len() {
                1 => unscoped_list.into_iter().next().unwrap(),
                _ => panic!("There can be only one group of unscoped builtins"),
            };
            (
                all_errors,
                MasterContext::NonEmptyMasterContext {
                    builtin_leader_file_key,
                    unscoped_builtins,
                    scoped_builtins: scoped_list,
                },
            )
        }
    }
}

pub fn mk_builtins<'cx>(
    metadata: &Metadata,
    master_cx: &MasterContext,
) -> Rc<dyn Fn(&Context<'cx>) -> Builtins<'cx, Context<'cx>> + 'cx> {
    match master_cx {
        MasterContext::EmptyMasterContext => Rc::new(|_| Builtins::empty()),
        MasterContext::NonEmptyMasterContext {
            builtin_leader_file_key,
            unscoped_builtins,
            scoped_builtins,
        } => {
            let builtin_leader_file_key = builtin_leader_file_key.dupe();
            let create_mapped_builtins = {
                let builtin_leader_file_key = builtin_leader_file_key.dupe();
                let metadata = metadata.clone();
                move |bg: &BuiltinsGroup|
                     -> Rc<dyn Fn(&Context<'cx>) -> Builtins<'cx, Context<'cx>> + 'cx> {
                    use std::cell::RefCell;

                    let builtin_leader_file_key = builtin_leader_file_key.dupe();
                    let metadata = metadata.clone();
                    let bg = bg.clone();
                    let builtins_ref: Rc<RefCell<Builtins<'cx, Context<'cx>>>> =
                        Rc::new(RefCell::new(Builtins::empty()));
                    let builtins_ref_clone = builtins_ref.dupe();
                    let cx = Context::make(
                        Rc::new(flow_typing_context::make_ccx()),
                        {
                            let mut m = metadata.clone();
                            m.overridable.checked = false;
                            m
                        },
                        builtin_leader_file_key.dupe(),
                        {
                            let builtin_leader_file_key = builtin_leader_file_key.dupe();
                            Rc::new(LazyCell::new(Box::new(move || {
                                Rc::new(ALocTable::empty(builtin_leader_file_key))
                            })
                                as Box<dyn FnOnce() -> Rc<ALocTable>>))
                        },
                        Rc::new(move |_cx: &Context, _| ResolvedRequire::MissingModule),
                        Rc::new(move |_cx: &Context| -> Builtins<'_, Context<'_>> {
                            builtins_ref_clone.replace(Builtins::empty())
                        }),
                    );
                    let (values, types, modules) = type_sig_merge::merge_builtins(
                        &cx,
                        builtin_leader_file_key.dupe(),
                        bg.builtin_locs.dupe(),
                        bg.builtins.dupe(),
                    );
                    let values: FlowOrdMap<_, _> = values.into_iter().collect();
                    let types: FlowOrdMap<_, _> = types.into_iter().collect();
                    let modules: FlowOrdMap<_, _> = modules.into_iter().collect();
                    let original_builtins = Builtins::of_name_map(
                        Rc::new(|_src_cx: &Context, _dst_cx: &Context, t: Type| t),
                        Rc::new(|_src_cx: &Context, _dst_cx: &Context, m: &ModuleType| {
                            m.dupe()
                        }),
                        values.dupe(),
                        types.dupe(),
                        modules.dupe(),
                    );
                    *builtins_ref.borrow_mut() = original_builtins;
                    let source_cx = cx.dupe();
                    Rc::new(move |_dst_cx: &Context| {
                        Builtins::of_name_map_with_source_cx(
                            source_cx.dupe(),
                            Rc::new(move |src_cx: &Context, dst_cx: &Context, t: Type| {
                                copied(dst_cx, src_cx, &t)
                            }),
                            Rc::new(move |src_cx: &Context, dst_cx: &Context, m: &ModuleType| {
                                module_type_copied(dst_cx, src_cx, m)
                            }),
                            values.dupe(),
                            types.dupe(),
                            modules.dupe(),
                        )
                    })
                }
            };
            let mapped_unscoped_builtins = create_mapped_builtins(unscoped_builtins);
            let mapped_scoped_builtins: Vec<_> = scoped_builtins
                .iter()
                .map(|(key, bg)| (*key, create_mapped_builtins(bg)))
                .collect();
            let metadata = metadata.clone();
            Rc::new(move |dst_cx: &Context| {
                let project = FlowProjects::from_path(
                    &metadata.frozen.projects_options,
                    dst_cx.file().as_str(),
                );
                // With the scoped libdef feature,
                // the set of libdefs active for a given file might be different.
                // The correct set of builtins is chosen here.
                match mapped_scoped_builtins
                    .iter()
                    .find_map(|(scoped_project, mapped_builtins)| {
                        if project.as_ref() == Some(scoped_project) {
                            Some(mapped_builtins(dst_cx))
                        } else {
                            None
                        }
                    }) {
                    None => mapped_unscoped_builtins(dst_cx),
                    Some(builtins) => builtins,
                }
            })
        }
    }
}
