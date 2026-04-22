/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_common::flow_import_specifier::Userland;
use flow_common::flow_symbol::Symbol;
use flow_common::polarity::Polarity;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::polymorphic_ast_mapper;
use flow_parser::polymorphic_ast_mapper::LocMapper;
use flow_typing_context::Context;
use flow_typing_errors::error_message::EIncorrectTypeWithReplacementData;
use flow_typing_errors::error_message::ETSSyntaxData;
use flow_typing_errors::error_message::ETooManyTypeArgsData;
use flow_typing_errors::error_message::ETypeGuardIncompatibleWithFunctionKindData;
use flow_typing_errors::error_message::ETypeGuardInvalidParameterData;
use flow_typing_errors::error_message::EVarianceKeywordData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::intermediate_error_types;
use flow_typing_errors::intermediate_error_types::InternalType;
use flow_typing_errors::intermediate_error_types::InvalidObjKey;
use flow_typing_errors::intermediate_error_types::TsLibSyntaxKind;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::type_subst;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_loc_env::func_class_sig_types;
use flow_typing_type::type_;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_util;
use flow_typing_utils::type_env;
use flow_typing_utils::typed_ast_utils;

use crate::class_sig;

type FuncTypeParam = (Type, ast::types::function::Param<ALoc, (ALoc, Type)>);
type FuncTypeRest = (Type, ast::types::function::RestParam<ALoc, (ALoc, Type)>);
type FuncTypeThisParam = (Type, ast::types::function::ThisParam<ALoc, (ALoc, Type)>);

fn func_type_id_name(id: &ast::Identifier<ALoc, (ALoc, Type)>) -> FlowSmolStr {
    id.name.dupe()
}

pub struct FuncTypeParamsConfig;

impl flow_typing_loc_env::func_class_sig_types::ConfigTypes for FuncTypeParamsConfig {
    type Param = FuncTypeParam;
    type Rest = FuncTypeRest;
    type ThisParam = FuncTypeThisParam;
    type ParamAst = ast::types::function::Param<ALoc, (ALoc, Type)>;
    type RestAst = ast::types::function::RestParam<ALoc, (ALoc, Type)>;
    type ThisAst = ast::types::function::ThisParam<ALoc, (ALoc, Type)>;
    type Ast = ast::types::function::Params<ALoc, (ALoc, Type)>;
}

impl crate::func_params_intf::Config for FuncTypeParamsConfig {
    fn param_type(param: &Self::Param) -> type_::FunParam {
        let (t, param_ast) = param;
        let (name, optional) = match &param_ast.param {
            ast::types::function::ParamKind::Anonymous(_) => (None, false),
            ast::types::function::ParamKind::Labeled { name, optional, .. } => {
                (Some(func_type_id_name(name)), *optional)
            }
            ast::types::function::ParamKind::Destructuring(pattern) => {
                (None, flow_parser::ast_utils::pattern_optional(pattern))
            }
        };
        let t = if optional {
            type_util::optional(t.dupe(), None, false)
        } else {
            t.dupe()
        };
        type_::FunParam(name, t)
    }

    fn rest_type(rest: &Self::Rest) -> type_::FunRestParam {
        let (t, rest_ast) = rest;
        let name = match &rest_ast.argument.param {
            ast::types::function::ParamKind::Labeled { name, .. } => Some(func_type_id_name(name)),
            _ => None,
        };
        type_::FunRestParam(name, rest_ast.loc.dupe(), t.dupe())
    }

    fn this_type(this: &Self::ThisParam) -> Type {
        this.0.dupe()
    }

    fn is_param_type_annotated(_param: &Self::Param) -> bool {
        true
    }

    fn is_rest_type_annotated(_rest: &Self::Rest) -> bool {
        true
    }

    fn subst_param<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        param: &Self::Param,
    ) -> Self::Param {
        let t = flow_js::subst(cx, None, None, None, map, param.0.dupe());
        (t, param.1.clone())
    }

    fn subst_rest<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        rest: &Self::Rest,
    ) -> Self::Rest {
        let t = flow_js::subst(cx, None, None, None, map, rest.0.dupe());
        (t, rest.1.clone())
    }

    fn subst_this<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        this: &Self::ThisParam,
    ) -> Self::ThisParam {
        let t = flow_js::subst(cx, None, None, None, map, this.0.dupe());
        (t, this.1.clone())
    }

    fn eval_param<'a>(
        _cx: &Context<'a>,
        param: &Self::Param,
    ) -> Result<Self::ParamAst, flow_typing_utils::abnormal::AbnormalControlFlow> {
        Ok(param.1.clone())
    }

    fn eval_rest<'a>(_cx: &Context<'a>, rest: &Self::Rest) -> Self::RestAst {
        rest.1.clone()
    }

    fn eval_this<'a>(_cx: &Context<'a>, this: &Self::ThisParam) -> Self::ThisAst {
        this.1.clone()
    }
}

type ComponentTypeParam = (
    Type,
    ast::types::component_params::Param<ALoc, (ALoc, Type)>,
);

type ComponentTypeRest = (
    Type,
    ast::types::component_params::RestParam<ALoc, (ALoc, Type)>,
);

struct TypeAnnotationConfig;

impl crate::component_params_intf::Config for TypeAnnotationConfig {
    type Param = ComponentTypeParam;
    type Rest = ComponentTypeRest;
    type ParamAst = ast::types::component_params::Param<ALoc, (ALoc, Type)>;
    type RestAst = ast::types::component_params::RestParam<ALoc, (ALoc, Type)>;

    fn read_react<'a>(_cx: &Context<'a>, _loc: ALoc) {}

    fn param_type_with_name(param: &Self::Param) -> (ALoc, FlowSmolStr, Type) {
        let (t, param_ast) = param;
        let param_t = if param_ast.optional {
            type_util::optional(t.dupe(), None, false)
        } else {
            t.dupe()
        };
        use flow_parser::ast::statement::component_params::ParamName;
        match &param_ast.name {
            ParamName::Identifier(id) => {
                let (ref loc, _) = id.loc;
                (loc.dupe(), id.name.dupe(), param_t)
            }
            ParamName::StringLiteral((loc, lit)) => {
                (loc.dupe(), FlowSmolStr::new(&lit.value), param_t)
            }
        }
    }

    fn rest_type(rest: &Self::Rest) -> Type {
        rest.0.dupe()
    }

    fn eval_param<'a>(
        _cx: &Context<'a>,
        param: &Self::Param,
    ) -> Result<Self::ParamAst, flow_typing_utils::abnormal::AbnormalControlFlow> {
        Ok(param.1.clone())
    }

    fn eval_rest<'a>(
        _cx: &Context<'a>,
        rest: &Self::Rest,
    ) -> Result<Self::RestAst, flow_typing_utils::abnormal::AbnormalControlFlow> {
        Ok(rest.1.clone())
    }
}

// =========================================================================
// AST helpers
// =========================================================================

fn qualified_name(mut id: &ast::types::generic::Identifier<ALoc, ALoc>) -> String {
    let mut acc = Vec::new();
    loop {
        use flow_parser::ast::types::generic::Identifier;
        match id {
            Identifier::Unqualified(ident) => {
                acc.push(ident.name.to_string());
                acc.reverse();
                return acc.join(".");
            }
            Identifier::Qualified(q) => {
                acc.push(q.id.name.to_string());
                id = &q.qualification;
            }
            Identifier::ImportTypeAnnot(import) => {
                let value = &import.argument.1.value;
                acc.push(format!("import(\"{}\")", value));
                acc.reverse();
                return acc.join(".");
            }
        }
    }
}

fn typeof_name(mut target: &ast::types::typeof_::Target<ALoc, ALoc>) -> String {
    use flow_parser::ast::types::typeof_::Target;

    let mut acc = Vec::new();
    loop {
        match target {
            Target::Unqualified(ident) => {
                acc.push(ident.name.as_str());
                acc.reverse();
                return acc.join(".");
            }
            Target::Qualified(q) => {
                acc.push(q.id.name.as_str());
                target = &q.qualification;
            }
            Target::Import(it) => {
                let value = &it.argument.1.value;
                let import_str = format!("import(\"{}\")", value);
                acc.reverse();
                if acc.is_empty() {
                    return import_str;
                } else {
                    return format!("{}.{}", import_str, acc.join("."));
                }
            }
        }
    }
}

fn ident_name(id: &ast::Identifier<ALoc, ALoc>) -> FlowSmolStr {
    id.name.dupe()
}

pub fn error_type<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    msg: ErrorMessage<ALoc>,
    t_in: &ast::types::Type<ALoc, ALoc>,
) -> ast::types::Type<ALoc, (ALoc, Type)> {
    flow_js_utils::add_output_non_speculating(cx, msg);
    let Ok(mapped) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t_in);
    let any_t = type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe());
    let mut inner = (*mapped).clone();
    *inner.loc_mut() = (loc, any_t);
    ast::types::Type::new(inner)
}

fn check_type_arg_arity<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    t_ast: &ast::types::Type<ALoc, ALoc>,
    params: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
    n: i32,
    f: impl FnOnce() -> ast::types::Type<ALoc, (ALoc, Type)>,
) -> ast::types::Type<ALoc, (ALoc, Type)> {
    match params {
        None => {
            if n == 0 {
                f()
            } else {
                error_type(cx, loc.dupe(), ErrorMessage::ETypeParamArity(loc, n), t_ast)
            }
        }
        Some(targs) => {
            let l_len = targs.arguments.len() as i32;
            if n == l_len && n != 0 {
                f()
            } else {
                error_type(cx, loc.dupe(), ErrorMessage::ETypeParamArity(loc, n), t_ast)
            }
        }
    }
}

fn mk_eval_id<'a>(cx: &Context<'a>, loc: ALoc) -> type_::eval::Id {
    if type_env::in_toplevel_scope(cx) {
        type_::eval::Id::of_aloc_id(false, cx.make_aloc_id(&loc))
    } else {
        type_::eval::Id::generate_id()
    }
}

fn add_unclear_type_error_if_not_lib_file<'a>(cx: &Context<'a>, loc: ALoc) {
    match loc.source() {
        Some(file) if !file.is_lib_file() => {
            flow_js_utils::add_output_non_speculating(cx, ErrorMessage::EUnclearType(loc));
        }
        _ => {}
    }
}

pub fn polarity<'a>(cx: &Context<'a>, variance: Option<&ast::Variance<ALoc>>) -> Polarity {
    if !cx.ts_syntax() {
        match variance {
            Some(ast::Variance {
                loc,
                kind: ast::VarianceKind::Readonly,
                ..
            }) if !(cx.allow_readonly_variance() || cx.allow_variance_keywords()) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSReadonlyVariance,
                        loc: loc.dupe(),
                    })),
                );
            }
            Some(ast::Variance {
                loc,
                kind: ast::VarianceKind::In,
                ..
            }) if !cx.allow_variance_keywords() => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSInOutVariance(
                            flow_typing_errors::error_message::InOutVariance::In,
                        ),
                        loc: loc.dupe(),
                    })),
                );
            }
            Some(ast::Variance {
                loc,
                kind: ast::VarianceKind::Out,
                ..
            }) if !cx.allow_variance_keywords() => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSInOutVariance(
                            flow_typing_errors::error_message::InOutVariance::Out,
                        ),
                        loc: loc.dupe(),
                    })),
                );
            }
            Some(ast::Variance {
                loc,
                kind: ast::VarianceKind::InOut,
                ..
            }) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSInOutVariance(
                            flow_typing_errors::error_message::InOutVariance::InOut,
                        ),
                        loc: loc.dupe(),
                    })),
                );
            }
            _ => {}
        }
    }
    if let Some(ast::Variance {
        loc,
        kind: ast::VarianceKind::Writeonly,
        ..
    }) = variance
    {
        if !cx.allow_variance_keywords() {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EVarianceKeyword(Box::new(EVarianceKeywordData {
                    kind: flow_typing_errors::error_message::VarianceKeywordKind::Writeonly,
                    loc: loc.dupe(),
                })),
            );
        }
    }
    typed_ast_utils::polarity(variance)
}

// =========================================================================
// Distributive tparam name helpers
// =========================================================================

fn use_distributive_tparam_name<'a>(
    cx: &Context<'a>,
    name: &FlowSmolStr,
    name_loc: ALoc,
    tparams_map: &mut FlowOrdMap<SubstName, Type>,
) -> Option<SubstName> {
    let subst_name = SubstName::name(name.dupe());
    match tparams_map.get(&subst_name) {
        Some(bound) => {
            let bound = bound.dupe();
            let distributive_tparam = TypeParam::new(type_::TypeParamInner {
                reason: reason::mk_annot_reason(
                    reason::VirtualReasonDesc::RType(Name::new(name.dupe())),
                    name_loc,
                ),
                name: subst_name.dupe(),
                bound,
                polarity: Polarity::Neutral,
                default: None,
                is_this: false,
                is_const: false,
            });
            let generic =
                flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), &distributive_tparam);
            tparams_map.insert(subst_name.dupe(), generic);
            Some(subst_name)
        }
        None => None,
    }
}

fn use_distributive_tparam_name_from_ast<'a>(
    cx: &Context<'a>,
    ast_type: &ast::types::Type<ALoc, ALoc>,
    tparams_map: &mut FlowOrdMap<SubstName, Type>,
) -> Option<SubstName> {
    use flow_parser::ast::types::TypeInner;
    use flow_parser::ast::types::generic::Identifier as GenericId;
    match ast_type.deref() {
        TypeInner::Generic { inner, .. }
            if let GenericId::Unqualified(ref ident) = inner.id
                && inner.targs.is_none() =>
        {
            use_distributive_tparam_name(cx, &ident.name, ident.loc.dupe(), tparams_map)
        }
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodKind {
    FunctionKind,
    MethodKind { is_static: bool },
    ConstructorKind,
    GetterKind,
    SetterKind,
}

fn allows_type_guards(kind: MethodKind) -> bool {
    match kind {
        MethodKind::FunctionKind | MethodKind::MethodKind { .. } => true,
        MethodKind::ConstructorKind | MethodKind::GetterKind | MethodKind::SetterKind => false,
    }
}

fn allows_this_type_guards(kind: MethodKind) -> bool {
    match kind {
        MethodKind::MethodKind { is_static } => !is_static,
        MethodKind::FunctionKind
        | MethodKind::ConstructorKind
        | MethodKind::GetterKind
        | MethodKind::SetterKind => false,
    }
}

fn method_kind_to_string(kind: MethodKind) -> &'static str {
    match kind {
        MethodKind::FunctionKind => "function",
        MethodKind::MethodKind { .. } => "method",
        MethodKind::ConstructorKind => "constructor",
        MethodKind::GetterKind => "getter",
        MethodKind::SetterKind => "setter",
    }
}

// =========================================================================
// Transform annotations to types
// =========================================================================

pub fn error_on_unsupported_variance_annotation<'a>(
    cx: &Context<'a>,
    kind: &str,
    tparams: Option<&ast::types::TypeParams<ALoc, ALoc>>,
) {
    if let Some(tparams) = tparams {
        for tparam in tparams.params.iter() {
            if let Some(ref variance) = tparam.variance {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedVarianceAnnotation(Box::new((
                        variance.loc.dupe(),
                        FlowSmolStr::new(kind),
                    ))),
                );
            }
        }
    }
}

#[derive(Clone)]
struct ConvertEnv {
    tparams_map: FlowOrdMap<SubstName, Type>,
    infer_tparams_map: Rc<ALocMap<(ast::types::TypeParam<ALoc, (ALoc, Type)>, Type)>>,
    in_no_infer: bool,
    in_renders_arg: bool,
}

impl ConvertEnv {
    fn new(
        infer_tparams_map: Option<ALocMap<(ast::types::TypeParam<ALoc, (ALoc, Type)>, Type)>>,
        in_no_infer: Option<bool>,
        in_renders_arg: Option<bool>,
        tparams_map: FlowOrdMap<SubstName, Type>,
    ) -> Self {
        Self {
            tparams_map,
            infer_tparams_map: Rc::new(infer_tparams_map.unwrap_or_default()),
            in_no_infer: in_no_infer.unwrap_or(false),
            in_renders_arg: in_renders_arg.unwrap_or(false),
        }
    }
}

fn resolve_computed_key_name(
    cx: &Context,
    expr: &ast::expression::Expression<ALoc, ALoc>,
) -> (
    ast::expression::Expression<ALoc, (ALoc, Type)>,
    Option<Name>,
) {
    if let Some(name) = flow_parser::ast_utils::well_known_symbol_name(expr) {
        let typed_expr = crate::statement::expression(None, None, None, cx, expr).unwrap();
        (typed_expr, Some(Name::new(name)))
    } else {
        let typed_expr = crate::statement::expression(None, None, Some(true), cx, expr).unwrap();
        let key_t = typed_expr.loc().1.dupe();
        let reason = type_util::reason_of_t(&key_t);
        let concrete_keys =
            FlowJs::possible_concrete_types_for_computed_object_keys(cx, reason, &key_t)
                .expect("Should not be under speculation");
        let resolved_name = match concrete_keys.as_slice() {
            [key] => match flow_js_utils::propref_for_elem_t(cx, key) {
                type_::PropRef::Named { name, .. } => Some(name),
                type_::PropRef::Computed(_) => None,
            },
            _ => None,
        };
        (typed_expr, resolved_name)
    }
}

// =========================================================================
// converter
// =========================================================================

fn convert_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    t: &ast::types::Type<ALoc, ALoc>,
) -> ast::types::Type<ALoc, (ALoc, Type)> {
    use std::ops::Deref;
    use std::sync::Arc;

    use flow_parser::ast::types::TypeInner;
    use flow_typing_type::type_::*;

    match t.deref() {
        TypeInner::Any { loc, comments } => {
            add_unclear_type_error_if_not_lib_file(cx, loc.dupe());
            let rt = any_t::at(AnySource::AnnotatedAny, loc.dupe());
            ast::types::Type::new(TypeInner::Any {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Mixed { loc, comments } => {
            if cx.is_utility_type_deprecated("mixed") && cx.ts_utility_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                        EIncorrectTypeWithReplacementData {
                            loc: loc.dupe(),
                            kind: intermediate_error_types::IncorrectType::Mixed,
                        },
                    )),
                );
            }
            let rt = mixed_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Mixed {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Empty { loc, comments } => {
            let rt = empty_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Empty {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Void { loc, comments } => {
            let rt = void::at(loc.dupe());
            ast::types::Type::new(TypeInner::Void {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Null { loc, comments } => {
            let rt = null::at(loc.dupe());
            ast::types::Type::new(TypeInner::Null {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Symbol { loc, comments } => {
            let rt = symbol_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Symbol {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Number { loc, comments } => {
            let rt = num_module_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Number {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::BigInt { loc, comments } => {
            let rt = bigint_module_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::BigInt {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::String { loc, comments } => {
            let rt = str_module_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::String {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Boolean { loc, raw, comments } => {
            if *raw == ast::types::BooleanRaw::Bool {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EDeprecatedBool(loc.dupe()),
                );
            }
            let rt = bool_module_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Boolean {
                loc: (loc.dupe(), rt),
                raw: raw.clone(),
                comments: comments.clone(),
            })
        }
        TypeInner::Unknown { loc, comments } => {
            if !(cx.ts_syntax() || cx.ts_utility_syntax()) {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSUnknown,
                        loc: loc.dupe(),
                    })),
                );
            }
            let rt = mixed_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Unknown {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Never { loc, comments } => {
            if !cx.ts_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSNever,
                        loc: loc.dupe(),
                    })),
                );
            }
            let rt = empty_t::at(loc.dupe());
            ast::types::Type::new(TypeInner::Never {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::Undefined { loc, comments } => {
            if !cx.ts_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSUndefined,
                        loc: loc.dupe(),
                    })),
                );
            }
            let rt = void::at(loc.dupe());
            ast::types::Type::new(TypeInner::Undefined {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
        TypeInner::UniqueSymbol { loc, comments } => {
            if !cx.tslib_syntax() {
                error_type(
                    cx,
                    loc.dupe(),
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::UniqueSymbolType,
                        ),
                    ))),
                    t,
                )
            } else {
                let rt = unique_symbol_t::at(cx.make_aloc_id(loc), loc.dupe());
                ast::types::Type::new(TypeInner::UniqueSymbol {
                    loc: (loc.dupe(), rt),
                    comments: comments.clone(),
                })
            }
        }
        TypeInner::Nullable { loc, inner } => {
            let t_ast = convert_inner(cx, env, &inner.argument);
            let inner_t = t_ast.loc().1.dupe();
            let reason = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RMaybe(Arc::new(type_util::desc_of_t(&inner_t).clone())),
                loc.dupe(),
            );
            let rt = Type::new(type_::TypeInner::MaybeT(reason, inner_t));
            ast::types::Type::new(TypeInner::Nullable {
                loc: (loc.dupe(), rt),
                inner: (ast::types::Nullable {
                    argument: t_ast,
                    comments: inner.comments.clone(),
                })
                .into(),
            })
        }
        TypeInner::Union { loc, inner } => {
            let t0_ast = convert_inner(cx, env, &inner.types.0);
            let t0 = t0_ast.loc().1.dupe();
            let t1_ast = convert_inner(cx, env, &inner.types.1);
            let t1 = t1_ast.loc().1.dupe();
            let (ts, ts_ast) = convert_list_inner(cx, env, &inner.types.2);
            let rep = type_::union_rep::make(
                Some(cx.make_aloc_id(loc)),
                type_::union_rep::UnionKind::UnknownKind,
                t0,
                t1,
                ts.into(),
            );
            let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RUnionType, loc.dupe());
            let rt = Type::new(type_::TypeInner::UnionT(reason, rep));
            ast::types::Type::new(TypeInner::Union {
                loc: (loc.dupe(), rt),
                inner: (ast::types::Union {
                    types: (t0_ast, t1_ast, ts_ast),
                    comments: inner.comments.clone(),
                })
                .into(),
            })
        }
        TypeInner::Intersection { loc, inner } => {
            let t0_ast = convert_inner(cx, env, &inner.types.0);
            let t0 = t0_ast.loc().1.dupe();
            let t1_ast = convert_inner(cx, env, &inner.types.1);
            let t1 = t1_ast.loc().1.dupe();
            let (ts, ts_ast) = convert_list_inner(cx, env, &inner.types.2);
            let rep = type_::inter_rep::make(t0, t1, ts.into());
            let reason =
                reason::mk_annot_reason(reason::VirtualReasonDesc::RIntersectionType, loc.dupe());
            let rt = Type::new(type_::TypeInner::IntersectionT(reason, rep));
            ast::types::Type::new(TypeInner::Intersection {
                loc: (loc.dupe(), rt),
                inner: (ast::types::Intersection {
                    types: (t0_ast, t1_ast, ts_ast),
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Typeof { loc, inner } => {
            let (valtype, qualification_ast) =
                convert_typeof(cx, "typeof-annotation", &inner.argument);
            let desc =
                reason::VirtualReasonDesc::RTypeof(FlowSmolStr::new(typeof_name(&inner.argument)));
            let reason = reason::mk_reason(desc, loc.dupe());
            let (targs, targs_ast) = match &inner.targs {
                None => (None, None),
                Some(type_args) => {
                    let (targs_vec, targs_ast_vec) =
                        convert_list_inner(cx, env, &type_args.arguments);
                    (
                        Some(targs_vec),
                        Some(ast::types::TypeArgs {
                            loc: {
                                let Ok(v) =
                                    typed_ast_utils::ErrorMapper.on_type_annot(&type_args.loc);
                                v
                            },
                            arguments: Arc::from(targs_ast_vec),
                            comments: type_args.comments.dupe(),
                        }),
                    )
                }
            };
            let rt = type_util::typeof_annotation(reason, valtype, targs);
            ast::types::Type::new(TypeInner::Typeof {
                loc: (loc.dupe(), rt),
                inner: (ast::types::Typeof {
                    argument: qualification_ast,
                    targs: targs_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Keyof { loc, inner } => {
            let argument_ast = convert_inner(cx, env, &inner.argument);
            let arg_t = argument_ast.loc().1.dupe();
            let t = Type::new(type_::TypeInner::KeysT(
                reason::mk_reason(reason::VirtualReasonDesc::RKeySet, loc.dupe()),
                arg_t,
            ));
            if !(cx.ts_syntax() || cx.ts_utility_syntax()) {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSKeyof,
                        loc: loc.dupe(),
                    })),
                );
            }
            ast::types::Type::new(TypeInner::Keyof {
                loc: (loc.dupe(), t),
                inner: ast::types::Keyof {
                    argument: argument_ast,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        TypeInner::Renders { loc, inner } => {
            let (t, renders_ast) = convert_render_type_inner(cx, env, loc.dupe(), inner);
            ast::types::Type::new(TypeInner::Renders {
                loc: (loc.dupe(), t),
                inner: renders_ast.into(),
            })
        }
        TypeInner::ReadOnly { loc, inner } => match inner.argument.deref() {
            TypeInner::Tuple { .. } => {
                if !cx.ts_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                            kind: flow_typing_errors::error_message::TSSyntaxKind::TSReadonlyType(
                                Some(flow_typing_errors::error_message::ReadonlyTypeKind::Tuple),
                            ),
                            loc: loc.dupe(),
                        })),
                    );
                }
                let argument_ast = convert_inner(cx, env, &inner.argument);
                let arg_t = argument_ast.loc().1.dupe();
                let reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RReadOnlyType, loc.dupe());
                let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                    type_: reason.dupe(),
                }));
                let t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                    cx,
                    use_op,
                    &reason,
                    &arg_t,
                    &type_::Destructor::ReadOnlyType,
                    mk_eval_id(cx, loc.dupe()),
                )
                .expect("Should not be under speculation");
                ast::types::Type::new(TypeInner::ReadOnly {
                    loc: (loc.dupe(), t),
                    inner: ast::types::ReadOnly {
                        argument: argument_ast,
                        comments: inner.comments.dupe(),
                    }
                    .into(),
                })
            }
            TypeInner::Array {
                loc: arr_loc,
                inner: arr_inner,
            } => {
                if !cx.ts_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                            kind: flow_typing_errors::error_message::TSSyntaxKind::TSReadonlyType(
                                Some(flow_typing_errors::error_message::ReadonlyTypeKind::Array),
                            ),
                            loc: loc.dupe(),
                        })),
                    );
                }
                let argument_ast = convert_inner(cx, env, &arr_inner.argument);
                let elem_t = argument_ast.loc().1.dupe();
                let arr_t = Type::new(type_::TypeInner::DefT(
                    reason::mk_annot_reason(reason::VirtualReasonDesc::RROArrayType, loc.dupe()),
                    type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT(
                        Box::new(ArrayATData {
                            react_dro: None,
                            elem_t: elem_t.dupe(),
                            tuple_view: None,
                        }),
                    )))),
                ));
                let ro_t = Type::new(type_::TypeInner::DefT(
                    reason::mk_annot_reason(reason::VirtualReasonDesc::RROArrayType, loc.dupe()),
                    type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ROArrayAT(
                        Box::new((elem_t.dupe(), None)),
                    )))),
                ));
                ast::types::Type::new(TypeInner::ReadOnly {
                    loc: (loc.dupe(), ro_t),
                    inner: (ast::types::ReadOnly {
                        argument: ast::types::Type::new(TypeInner::Array {
                            loc: (arr_loc.dupe(), arr_t),
                            inner: (ast::types::Array {
                                argument: argument_ast,
                                comments: arr_inner.comments.dupe(),
                            })
                            .into(),
                        }),
                        comments: inner.comments.dupe(),
                    })
                    .into(),
                })
            }
            _ => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                        kind: flow_typing_errors::error_message::TSSyntaxKind::TSReadonlyType(None),
                        loc: loc.dupe(),
                    })),
                );
                let t = type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe());
                let mapped_ro = {
                    let Ok(v) = polymorphic_ast_mapper::readonly_type(
                        &mut typed_ast_utils::ErrorMapper,
                        inner,
                    );
                    v
                };
                ast::types::Type::new(TypeInner::ReadOnly {
                    loc: (loc.dupe(), t),
                    inner: mapped_ro.into(),
                })
            }
        },
        TypeInner::Tuple { loc, inner } => {
            let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RTupleType, loc.dupe());
            let mut unresolved = Vec::new();
            let mut els_asts = Vec::new();
            for element in inner.elements.iter() {
                let (el, el_ast) = convert_tuple_element(cx, env, element);
                unresolved.push(el);
                els_asts.push(el_ast);
            }
            let id = mk_eval_id(cx, loc.dupe());
            let t = flow_js_utils::mk_tuple_type(
                cx,
                id,
                |cx, use_op, reason, t, destructor, eval_id| {
                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                        cx,
                        use_op,
                        &reason,
                        &t,
                        &destructor,
                        eval_id,
                    )
                },
                inner.inexact,
                reason,
                unresolved,
            )
            .expect("Should not be under speculation");
            ast::types::Type::new(TypeInner::Tuple {
                loc: (loc.dupe(), t),
                inner: (ast::types::Tuple {
                    elements: els_asts.into(),
                    inexact: inner.inexact,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Array { loc, inner } => {
            let r = reason::mk_annot_reason(reason::VirtualReasonDesc::RArrayType, loc.dupe());
            let t_ast = convert_inner(cx, env, &inner.argument);
            let elem_t = t_ast.loc().1.dupe();
            let rt = Type::new(type_::TypeInner::DefT(
                r,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT(
                    Box::new(ArrayATData {
                        elem_t,
                        tuple_view: None,
                        react_dro: None,
                    }),
                )))),
            ));
            ast::types::Type::new(TypeInner::Array {
                loc: (loc.dupe(), rt),
                inner: (ast::types::Array {
                    argument: t_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Conditional { loc, inner } => {
            let distributive_tparam_name =
                use_distributive_tparam_name_from_ast(cx, &inner.check_type, &mut env.tparams_map);
            let check_type_ast = convert_inner(cx, env, &inner.check_type);
            let check_t = check_type_ast.loc().1.dupe();
            let hoisted_infer_types =
                flow_analysis::infer_type_hoister::hoist_infer_types(&inner.extends_type);
            let mut tparams: Vec<type_::TypeParam> = Vec::new();
            let mut additional_true_type_tparams_map: FlowOrdMap<SubstName, Type> =
                FlowOrdMap::default();
            let mut extends_infer_tparams_map = ALocMap::new();
            let mut infer_bounds_map: FlowOrdMap<SubstName, Type> = FlowOrdMap::default();
            for (_infer_loc, infer) in hoisted_infer_types.iter() {
                let subst_name = SubstName::name(infer.tparam.name.name.dupe());
                let old_infer_tparams_map =
                    std::mem::replace(&mut env.infer_tparams_map, Rc::new(ALocMap::new()));
                let (tparam_tast, tparam, t) = mk_type_param_inner(
                    cx,
                    env,
                    flow_parser::ast_visitor::TypeParamsContext::Infer,
                    &infer.tparam,
                );
                env.infer_tparams_map = old_infer_tparams_map;
                match infer_bounds_map.get(&subst_name) {
                    Some(existing_bound) => {
                        let use_op = type_::UseOp::Op(Arc::new(
                            type_::RootUseOp::InferBoundCompatibilityCheck {
                                bound: type_util::reason_of_t(&tparam.bound).dupe(),
                                infer: type_util::reason_of_t(&t).dupe(),
                            },
                        ));
                        flow_js::unify(cx, Some(use_op), &tparam.bound, existing_bound)
                            .expect("Should not be under speculation");
                        let t = additional_true_type_tparams_map
                            .get(&subst_name)
                            .expect("subst_name should exist in additional_true_type_tparams_map")
                            .dupe();
                        extends_infer_tparams_map
                            .insert(tparam_tast.loc.0.dupe(), (tparam_tast, t));
                    }
                    None => {
                        tparams.push(tparam.dupe());
                        additional_true_type_tparams_map.insert(subst_name.dupe(), t.dupe());
                        extends_infer_tparams_map
                            .insert(tparam_tast.loc.0.dupe(), (tparam_tast, t));
                        infer_bounds_map.insert(subst_name, tparam.bound.dupe());
                    }
                }
            }
            let infer_tparams = tparams;
            let old_infer_tparams_map = std::mem::replace(
                &mut env.infer_tparams_map,
                Rc::new(extends_infer_tparams_map),
            );
            let extends_type_ast = convert_inner(cx, env, &inner.extends_type);
            let extends_t = extends_type_ast.loc().1.dupe();
            env.infer_tparams_map = old_infer_tparams_map;
            let old_tparams_map = env.tparams_map.dupe();
            for (k, v) in additional_true_type_tparams_map.iter() {
                env.tparams_map.insert(k.dupe(), v.dupe());
            }
            let true_type_ast = convert_inner(cx, env, &inner.true_type);
            let true_t = true_type_ast.loc().1.dupe();
            env.tparams_map = old_tparams_map;
            let false_type_ast = convert_inner(cx, env, &inner.false_type);
            let false_t = false_type_ast.loc().1.dupe();
            let reason = reason::mk_reason(reason::VirtualReasonDesc::RConditionalType, loc.dupe());
            let destructor =
                type_::Destructor::ConditionalType(Box::new(DestructorConditionalTypeData {
                    distributive_tparam_name,
                    infer_tparams: infer_tparams.into(),
                    extends_t,
                    true_t,
                    false_t,
                }));
            let t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                cx,
                type_::unknown_use(),
                &reason,
                &check_t,
                &destructor,
                mk_eval_id(cx, loc.dupe()),
            )
            .expect("Should not be under speculation");
            ast::types::Type::new(TypeInner::Conditional {
                loc: (loc.dupe(), t),
                inner: (ast::types::Conditional {
                    check_type: check_type_ast,
                    extends_type: extends_type_ast,
                    true_type: true_type_ast,
                    false_type: false_type_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Infer { loc, inner } => {
            let tparam_loc = &inner.tparam.loc;
            let name_loc = &inner.tparam.name.loc;
            let environment = cx.environment();
            let var_info = &environment.var_info;
            match var_info.env_entries.get_ordinary(name_loc) {
                Some(flow_env_builder::env_api::EnvEntry::NonAssigningWrite) => {
                    let Ok(tparam_tast) = polymorphic_ast_mapper::type_param(
                        &mut typed_ast_utils::ErrorMapper,
                        &inner.tparam,
                    );
                    let t = type_::any_t::error(reason::mk_reason(
                        reason::VirtualReasonDesc::RAnyImplicit,
                        loc.dupe(),
                    ));
                    ast::types::Type::new(TypeInner::Infer {
                        loc: (loc.dupe(), t),
                        inner: (ast::types::Infer {
                            tparam: tparam_tast,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
                _ => {
                    drop(environment);
                    let (tparam_tast, t) = env
                        .infer_tparams_map
                        .get(tparam_loc)
                        .expect("tparam_loc should exist in infer_tparams_map")
                        .clone();
                    ast::types::Type::new(TypeInner::Infer {
                        loc: (loc.dupe(), t),
                        inner: (ast::types::Infer {
                            tparam: tparam_tast,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                }
            }
        }
        TypeInner::StringLiteral { loc, literal } => {
            let rt = mk_singleton_string(loc.dupe(), &literal.value);
            ast::types::Type::new(TypeInner::StringLiteral {
                loc: (loc.dupe(), rt),
                literal: literal.clone(),
            })
        }
        TypeInner::NumberLiteral { loc, literal } => {
            let rt = mk_singleton_number(loc.dupe(), literal.value, &literal.raw);
            ast::types::Type::new(TypeInner::NumberLiteral {
                loc: (loc.dupe(), rt),
                literal: literal.clone(),
            })
        }
        TypeInner::BigIntLiteral { loc, literal } => {
            let rt = mk_singleton_bigint(loc.dupe(), literal.value, &literal.raw);
            ast::types::Type::new(TypeInner::BigIntLiteral {
                loc: (loc.dupe(), rt),
                literal: literal.clone(),
            })
        }
        TypeInner::BooleanLiteral { loc, literal } => {
            let rt = mk_singleton_boolean(loc.dupe(), literal.value);
            ast::types::Type::new(TypeInner::BooleanLiteral {
                loc: (loc.dupe(), rt),
                literal: literal.clone(),
            })
        }
        TypeInner::IndexedAccess { loc, inner } => {
            let reason = reason::mk_reason(
                reason::VirtualReasonDesc::RIndexedAccess { optional: false },
                loc.dupe(),
            );
            let object_ast = convert_inner(cx, env, &inner.object);
            let object_type = object_ast.loc().1.dupe();
            let index_ast = convert_inner(cx, env, &inner.index);
            let index_type = index_ast.loc().1.dupe();
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::IndexedTypeAccess {
                object: type_util::reason_of_t(&object_type).dupe(),
                index: type_util::reason_of_t(&index_type).dupe(),
            }));
            let destructor = match index_ast.deref() {
                TypeInner::StringLiteral { literal, .. } => type_::Destructor::PropertyType {
                    name: Name::new(literal.value.dupe()),
                },
                _ => type_::Destructor::ElementType {
                    index_type: index_type.dupe(),
                },
            };
            let t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                cx,
                use_op,
                &reason,
                &object_type,
                &destructor,
                mk_eval_id(cx, loc.dupe()),
            )
            .expect("Should not be under speculation");
            ast::types::Type::new(TypeInner::IndexedAccess {
                loc: (loc.dupe(), t),
                inner: ast::types::IndexedAccess {
                    object: object_ast,
                    index: index_ast,
                    comments: inner.comments.dupe(),
                }
                .into(),
            })
        }
        TypeInner::OptionalIndexedAccess { loc: _, inner } => {
            let loc = t.loc().dupe();
            let (_, ast) = optional_indexed_access(cx, env, loc, inner);
            ast
        }
        TypeInner::Generic { loc, inner } => match &inner.id {
            ast::types::generic::Identifier::Qualified(q) => {
                let qid = &inner.id;
                let qid_loc = &q.loc;
                let targs = &inner.targs;
                let comments = &inner.comments;

                let (m, qualification_ast) =
                    convert_qualification(cx, "type-annotation", &q.qualification);
                let id_loc = q.id.loc.dupe();
                let id_name = &q.id;
                let name = &id_name.name;
                let reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RType(Name::new(name.dupe())),
                    loc.dupe(),
                );
                let id_reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RType(Name::new(name.dupe())),
                    id_loc.dupe(),
                );
                let qid_name = qualified_name(qid);
                let qid_reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new(&qid_name))),
                    qid_loc.dupe(),
                );
                let use_op =
                    type_::UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(qid_reason.dupe())));
                let t_unapplied = crate::type_annotation_cons_gen::qualify_type(
                    cx,
                    use_op,
                    id_reason,
                    qid_reason,
                    Name::new(name.dupe()),
                    m,
                );
                let (t, targs_ast) =
                    mk_nominal_type_inner(cx, env, reason, t_unapplied.dupe(), targs.as_ref());
                ast::types::Type::new(TypeInner::Generic {
                    loc: (loc.dupe(), t),
                    inner: (ast::types::Generic {
                        id: ast::types::generic::Identifier::Qualified(
                            (ast::types::generic::Qualified {
                                loc: qid_loc.dupe(),
                                qualification: qualification_ast,
                                id: ast::Identifier::new(ast::IdentifierInner {
                                    loc: (id_loc, t_unapplied),
                                    name: name.dupe(),
                                    comments: id_name.comments.dupe(),
                                }),
                            })
                            .into(),
                        ),
                        targs: targs_ast,
                        comments: comments.clone(),
                    })
                    .into(),
                })
            }
            // import("module") type syntax
            ast::types::generic::Identifier::ImportTypeAnnot(import) => {
                let import_id = &inner.id;
                let import_loc = &import.loc;
                let argument = &import.argument;
                let import_comments = &import.comments;
                let targs = &inner.targs;
                let comments = &inner.comments;

                let (m, import_ast) = convert_qualification(cx, "type-annotation", import_id);
                let (t, targs_ast) = {
                    let import_id_name = qualified_name(import_id);
                    let reason = reason::mk_reason(
                        reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new(
                            &import_id_name,
                        ))),
                        loc.dupe(),
                    );
                    mk_nominal_type_inner(cx, env, reason, m.dupe(), targs.as_ref())
                };
                let id_ast = match import_ast {
                    ast::types::generic::Identifier::ImportTypeAnnot(import_type_ast) => {
                        ast::types::generic::Identifier::ImportTypeAnnot(import_type_ast)
                    }
                    _ => ast::types::generic::Identifier::ImportTypeAnnot(
                        (ast::types::generic::ImportType {
                            loc: (import_loc.dupe(), m),
                            argument: argument.clone(),
                            comments: import_comments.clone(),
                        })
                        .into(),
                    ),
                };
                ast::types::Type::new(TypeInner::Generic {
                    loc: (loc.dupe(), t),
                    inner: (ast::types::Generic {
                        id: id_ast,
                        targs: targs_ast,
                        comments: comments.clone(),
                    })
                    .into(),
                })
            }
            // type applications: name < params >
            ast::types::generic::Identifier::Unqualified(ident) => {
                let name_loc = ident.loc.dupe();
                let id_name = ident;
                let name = &ident.name;
                // Comments are innecessary, so they can be stripped to meet the generic requirements
                let convert_type_params = |cx,
                                           env: &mut ConvertEnv,
                                           targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>|
                 -> (
                    Vec<Type>,
                    Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>,
                ) {
                    match targs {
                        None => (vec![], None),
                        Some(targs_node) => {
                            let (elemts, targs_ast) =
                                convert_list_inner(cx, env, &targs_node.arguments);
                            (
                                elemts,
                                Some(ast::types::TypeArgs {
                                    loc: {
                                        let Ok(v) = typed_ast_utils::ErrorMapper
                                            .on_type_annot(&targs_node.loc);
                                        v
                                    },
                                    arguments: Arc::from(targs_ast),
                                    comments: targs_node.comments.dupe(),
                                }),
                            )
                        }
                    }
                };
                let reconstruct_ast = |t: Type,
                                       id_t: Option<Type>,
                                       targs: Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>|
                 -> ast::types::Type<ALoc, (ALoc, Type)> {
                    let id_t_val = id_t.unwrap_or_else(|| t.dupe());
                    ast::types::Type::new(TypeInner::Generic {
                        loc: (loc.dupe(), t),
                        inner: (ast::types::Generic {
                            id: ast::types::generic::Identifier::Unqualified(ast::Identifier::new(
                                ast::IdentifierInner {
                                    loc: (name_loc.dupe(), id_t_val),
                                    name: id_name.name.dupe(),
                                    comments: id_name.comments.dupe(),
                                },
                            )),
                            targs,
                            comments: inner.comments.dupe(),
                        })
                        .into(),
                    })
                };

                let use_op = |reason: &Reason| -> type_::UseOp {
                    type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                        type_: reason.dupe(),
                    }))
                };
                let local_generic_type =
                    |cx,
                     env: &mut ConvertEnv,
                     name: &FlowSmolStr,
                     name_loc: ALoc,
                     loc: ALoc,
                     targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
                     reconstruct_ast: &dyn Fn(
                        Type,
                        Option<Type>,
                        Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>,
                    )
                        -> ast::types::Type<ALoc, (ALoc, Type)>|
                     -> ast::types::Type<ALoc, (ALoc, Type)> {
                        let reason = reason::mk_reason(
                            reason::VirtualReasonDesc::RType(Name::new(name.dupe())),
                            loc.dupe(),
                        );
                        let c = type_identifier(cx, name, name_loc);
                        let (t, targs_ast) =
                            mk_nominal_type_inner(cx, env, reason, c.dupe(), targs);
                        reconstruct_ast(t, Some(c), targs_ast)
                    };
                let mod_tparam_t_annot_loc = |annot_loc: ALoc, tp: &Type| -> Type {
                    type_util::mod_reason_of_t(
                        &|r: Reason| {
                            r.reposition(loc.dupe())
                                .opt_annotate(Some(annot_loc.dupe()))
                        },
                        tp,
                    )
                };
                match name.as_str() {
                    "this" => {
                        if env
                            .tparams_map
                            .contains_key(&SubstName::name(FlowSmolStr::new_inline("this")))
                        {
                            // We model a this type like a type parameter. The bound on a this
                            // type reflects the interface of `this` exposed in the current
                            // environment. Currently, we only support this types in a class
                            // environment: a this type in class C is bounded by C.
                            check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 0, || {
                                let tparam_t = env
                                    .tparams_map
                                    .get(&SubstName::name(FlowSmolStr::new_inline("this")))
                                    .unwrap()
                                    .dupe();
                                let tp = mod_tparam_t_annot_loc(loc.dupe(), &tparam_t);
                                reconstruct_ast(tp, None, None)
                            })
                        } else {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnexpectedThisType(loc.dupe()),
                            );
                            {
                                let Ok(v) = polymorphic_ast_mapper::type_(
                                    &mut typed_ast_utils::ErrorMapper,
                                    t,
                                );
                                v
                            }
                        }
                    }
                    // in-scope type vars
                    _ if env.tparams_map.contains_key(&SubstName::name(name.dupe())) => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 0, || {
                            let mut tp = env
                                .tparams_map
                                .get(&SubstName::name(name.dupe()))
                                .unwrap()
                                .dupe();
                            if env.in_no_infer {
                                if let type_::TypeInner::GenericT(box GenericTData {
                                    reason: gr,
                                    name: gn,
                                    bound: gb,
                                    no_infer: _,
                                    id: gid,
                                }) = tp.deref()
                                {
                                    tp = Type::new(type_::TypeInner::GenericT(Box::new(
                                        GenericTData {
                                            reason: gr.dupe(),
                                            name: gn.dupe(),
                                            bound: gb.dupe(),
                                            no_infer: true,
                                            id: gid.clone(),
                                        },
                                    )));
                                }
                            }
                            let tp = mod_tparam_t_annot_loc(loc.dupe(), &tp);
                            reconstruct_ast(tp, None, None)
                        })
                    }
                    _ if type_env::local_scope_entry_exists(cx, name_loc.dupe()) => {
                        local_generic_type(
                            cx,
                            env,
                            name,
                            name_loc.dupe(),
                            loc.dupe(),
                            inner.targs.as_ref(),
                            &reconstruct_ast,
                        )
                    }
                    // NoInfer intrinsic that makes every GenericT inside it no_infer
                    "NoInfer" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let mut env_no_infer = env.clone();
                            env_no_infer.in_no_infer = true;
                            let (elemts, targs_ast) =
                                convert_type_params(cx, &mut env_no_infer, inner.targs.as_ref());
                            let elem_t = elemts.into_iter().next().unwrap();
                            reconstruct_ast(elem_t, None, targs_ast)
                        })
                    }
                    "StringPrefix" => {
                        let (ts, targs_ast) = convert_type_params(cx, env, inner.targs.as_ref());
                        let create_string_prefix_type =
                            |prefix: &Type,
                             remainder: Option<Type>,
                             targs_ast: Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>|
                             -> ast::types::Type<ALoc, (ALoc, Type)> {
                                if let type_::TypeInner::DefT(_, ref def) = *prefix.deref() {
                                    if let type_::DefTInner::SingletonStrT {
                                        value: ref sst_value,
                                        ..
                                    } = *def.deref()
                                    {
                                        let prefix_str = sst_value.as_smol_str();
                                        let reason = reason::mk_reason(
                                            reason::VirtualReasonDesc::RStringPrefix {
                                                prefix: prefix_str.dupe(),
                                            },
                                            loc.dupe(),
                                        );
                                        return reconstruct_ast(
                                            Type::new(type_::TypeInner::StrUtilT {
                                                reason,
                                                op: type_::StrUtilOp::StrPrefix(prefix_str.dupe()),
                                                remainder,
                                            }),
                                            None,
                                            targs_ast,
                                        );
                                    }
                                }
                                error_type(
                                    cx,
                                    loc.dupe(),
                                    ErrorMessage::EStrUtilTypeNonLiteralArg(loc.dupe()),
                                    t,
                                )
                            };
                        let reason = reason::mk_reason(
                            reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new_inline(
                                "StringPrefix",
                            ))),
                            loc.dupe(),
                        );
                        match ts.len() {
                            0 => error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::ETypeParamMinArity(loc.dupe(), 1),
                                t,
                            ),
                            1 => create_string_prefix_type(&ts[0], None, targs_ast),
                            2 => {
                                let uo = use_op(&reason);
                                cx.add_post_inference_subtyping_check(
                                    ts[1].dupe(),
                                    uo,
                                    type_::str_module_t::at(loc.dupe()),
                                );
                                create_string_prefix_type(&ts[0], Some(ts[1].dupe()), targs_ast)
                            }
                            _ => error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::ETooManyTypeArgs(Box::new(ETooManyTypeArgsData {
                                    reason_tapp: reason,
                                    arity_loc: loc.dupe(),
                                    maximum_arity: 2,
                                })),
                                t,
                            ),
                        }
                    }
                    "StringSuffix" => {
                        let (ts, targs_ast) = convert_type_params(cx, env, inner.targs.as_ref());
                        let create_string_suffix_type =
                            |suffix: &Type,
                             remainder: Option<Type>,
                             targs_ast: Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>|
                             -> ast::types::Type<ALoc, (ALoc, Type)> {
                                if let type_::TypeInner::DefT(_, ref def) = *suffix.deref() {
                                    if let type_::DefTInner::SingletonStrT {
                                        value: ref sst_value,
                                        ..
                                    } = *def.deref()
                                    {
                                        let suffix_str = sst_value.as_smol_str();
                                        let reason = reason::mk_reason(
                                            reason::VirtualReasonDesc::RStringSuffix {
                                                suffix: suffix_str.dupe(),
                                            },
                                            loc.dupe(),
                                        );
                                        return reconstruct_ast(
                                            Type::new(type_::TypeInner::StrUtilT {
                                                reason,
                                                op: type_::StrUtilOp::StrSuffix(suffix_str.dupe()),
                                                remainder,
                                            }),
                                            None,
                                            targs_ast,
                                        );
                                    }
                                }
                                error_type(
                                    cx,
                                    loc.dupe(),
                                    ErrorMessage::EStrUtilTypeNonLiteralArg(loc.dupe()),
                                    t,
                                )
                            };
                        let reason = reason::mk_reason(
                            reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new_inline(
                                "StringSuffix",
                            ))),
                            loc.dupe(),
                        );
                        match ts.len() {
                            0 => error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::ETypeParamMinArity(loc.dupe(), 1),
                                t,
                            ),
                            1 => create_string_suffix_type(&ts[0], None, targs_ast),
                            2 => {
                                let uo = use_op(&reason);
                                cx.add_post_inference_subtyping_check(
                                    ts[1].dupe(),
                                    uo,
                                    type_::str_module_t::at(loc.dupe()),
                                );
                                create_string_suffix_type(&ts[0], Some(ts[1].dupe()), targs_ast)
                            }
                            _ => error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::ETooManyTypeArgs(Box::new(ETooManyTypeArgsData {
                                    reason_tapp: reason,
                                    arity_loc: loc.dupe(),
                                    maximum_arity: 2,
                                })),
                                t,
                            ),
                        }
                    }
                    // Array<T>
                    "Array" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (elemts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let elem_t = elemts.into_iter().next().unwrap();
                            reconstruct_ast(
                                Type::new(type_::TypeInner::DefT(
                                    reason::mk_annot_reason(
                                        reason::VirtualReasonDesc::RArrayType,
                                        loc.dupe(),
                                    ),
                                    type_::DefT::new(type_::DefTInner::ArrT(Rc::new(
                                        type_::ArrType::ArrayAT(Box::new(ArrayATData {
                                            react_dro: None,
                                            elem_t,
                                            tuple_view: None,
                                        })),
                                    ))),
                                )),
                                None,
                                targs_ast,
                            )
                        })
                    }
                    // $ReadOnlyArray<T> is the supertype of all tuples and all arrays
                    "$ReadOnlyArray" => {
                        if cx.is_utility_type_deprecated("$ReadOnlyArray") {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(EIncorrectTypeWithReplacementData {
                                    loc: loc.dupe(),
                                    kind:
                                        intermediate_error_types::IncorrectType::DollarReadOnlyArray,
                                })),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (elemts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let elemt = elemts.into_iter().next().unwrap();
                            reconstruct_ast(
                                Type::new(type_::TypeInner::DefT(
                                    reason::mk_annot_reason(
                                        reason::VirtualReasonDesc::RROArrayType,
                                        loc.dupe(),
                                    ),
                                    type_::DefT::new(type_::DefTInner::ArrT(Rc::new(
                                        type_::ArrType::ROArrayAT(Box::new((elemt, None))),
                                    ))),
                                )),
                                None,
                                targs_ast,
                            )
                        })
                    }
                    // $NonMaybeType<T> acts as the type T without null and void
                    "$NonMaybeType" => {
                        if cx.is_utility_type_deprecated("$NonMaybeType") {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(EIncorrectTypeWithReplacementData {
                                    loc: loc.dupe(),
                                    kind:
                                        intermediate_error_types::IncorrectType::DollarNonMaybeType,
                                })),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RType(Name::new(
                                    FlowSmolStr::new_inline("$NonMaybeType"),
                                )),
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::NonMaybeType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // Deprecated former alias of `Partial`
                    "$Partial" => error_type(
                        cx,
                        loc.dupe(),
                        ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                            EIncorrectTypeWithReplacementData {
                                loc: loc.dupe(),
                                kind: intermediate_error_types::IncorrectType::Partial,
                            },
                        )),
                        t,
                    ),
                    // Partial<T> makes all of `T`'s properties optional
                    "Partial" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RPartialOf(Arc::new(
                                    type_util::desc_of_t(&tp).clone(),
                                )),
                                type_util::loc_of_t(&tp).dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::PartialType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // Required<T> makes all of `T`'s optional properties required.
                    "Required" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RRequiredOf(Arc::new(
                                    type_util::desc_of_t(&tp).clone(),
                                )),
                                type_util::loc_of_t(&tp).dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::RequiredType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // `$Shape` is deprecated in favor of `Partial`
                    "$Shape" => error_type(
                        cx,
                        loc.dupe(),
                        ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                            EIncorrectTypeWithReplacementData {
                                loc: loc.dupe(),
                                kind: intermediate_error_types::IncorrectType::Shape,
                            },
                        )),
                        t,
                    ),
                    // Omit<T, K> removes keys K from T.
                    "$Omit" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::DollarUtilityTypeWithNonDollarAliases(
                                        FlowSmolStr::new_inline("Omit"),
                                    ),
                                ),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 2, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let mut ts_iter = ts.into_iter();
                            let t1 = ts_iter.next().unwrap();
                            let t2 = ts_iter.next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RType(Name::new(
                                    FlowSmolStr::new_inline("Omit"),
                                )),
                                loc.dupe(),
                            );
                            let mapped_use_op =
                                type_::UseOp::Op(Arc::new(type_::RootUseOp::EvalMappedType {
                                    mapped_type: reason.dupe(),
                                }));
                            let t2_mapped =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    mapped_use_op,
                                    &reason,
                                    &t2,
                                    &type_::Destructor::MappedType(Box::new(
                                        DestructorMappedTypeData {
                                            homomorphic:
                                                type_::MappedTypeHomomorphicFlag::Unspecialized,
                                            property_type: type_::mixed_t::make(reason.dupe()),
                                            mapped_type_flags: type_::MappedTypeFlags {
                                                optional:
                                                    type_::MappedTypeOptionality::KeepOptionality,
                                                variance: type_::MappedTypeVariance::KeepVariance,
                                            },
                                            distributive_tparam_name: None,
                                        },
                                    )),
                                    type_::eval::Id::generate_id(),
                                )
                                .expect("mk_type_destructor should not fail");
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &t1,
                                    &type_::Destructor::RestType(
                                        type_::object::rest::MergeMode::Omit,
                                        t2_mapped,
                                    ),
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // $ReadOnly<T>
                    "$ReadOnly" => {
                        if cx.is_utility_type_deprecated("$ReadOnly") {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind:
                                            intermediate_error_types::IncorrectType::DollarReadOnly,
                                    },
                                )),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RReadOnlyType,
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::ReadOnlyType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // $ReactDeepReadOnly<T>
                    "$ReactDeepReadOnly" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::DollarReactDeepReadOnly,
                                ),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RReadOnlyType,
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::ReactDRO(Box::new(type_::ReactDro(
                                        loc.dupe(),
                                        type_::DroType::DebugAnnot,
                                    ))),
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // $Keys<T> is the set of keys of T
                    "$Keys" => {
                        if cx.is_utility_type_deprecated("$Keys") {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind: intermediate_error_types::IncorrectType::DollarKeys,
                                    },
                                )),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            reconstruct_ast(
                                Type::new(type_::TypeInner::KeysT(
                                    reason::mk_reason(
                                        reason::VirtualReasonDesc::RKeySet,
                                        loc.dupe(),
                                    ),
                                    tp,
                                )),
                                None,
                                targs_ast,
                            )
                        })
                    }
                    // $Values<T> is a union of all the own enumerable value types of T
                    "$Values" => {
                        if cx.is_utility_type_deprecated("$Values") {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind: intermediate_error_types::IncorrectType::DollarValues,
                                    },
                                )),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RType(Name::new(
                                    FlowSmolStr::new_inline("$Values"),
                                )),
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::ValuesType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // Values<T> replaces $Values<T>
                    "Values" => {
                        if cx.ts_syntax() || cx.ts_utility_syntax() {
                            check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                                let (ts, targs_ast) =
                                    convert_type_params(cx, env, inner.targs.as_ref());
                                let tp = ts.into_iter().next().unwrap();
                                let reason = reason::mk_reason(
                                    reason::VirtualReasonDesc::RType(Name::new(
                                        FlowSmolStr::new_inline("Values"),
                                    )),
                                    loc.dupe(),
                                );
                                let result_t =
                                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                        cx,
                                        use_op(&reason),
                                        &reason,
                                        &tp,
                                        &type_::Destructor::ValuesType,
                                        mk_eval_id(cx, loc.dupe()),
                                    )
                                    .expect("mk_type_destructor should not fail");
                                reconstruct_ast(result_t, None, targs_ast)
                            })
                        } else {
                            error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind: intermediate_error_types::IncorrectType::Values,
                                    },
                                )),
                                t,
                            )
                        }
                    }
                    "$Exact" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let desc = reason::VirtualReasonDesc::RExactType(Arc::new(
                                type_util::desc_of_t(&tp).clone(),
                            ));
                            let reason = reason::mk_annot_reason(desc, loc.dupe());
                            let tp = type_util::push_type_alias_reason(&reason, tp);
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::ExactType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // Exports<'M'> is the type of the exports of module 'M'
                    "$Exports" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            match &inner.targs {
                                Some(targs_node)
                                    if !targs_node.arguments.is_empty()
                                        && let TypeInner::StringLiteral {
                                            loc: str_loc,
                                            literal: str_lit,
                                        } = targs_node.arguments[0].deref() =>
                                {
                                    let value = &str_lit.value;
                                    let import_specifier =
                                        Userland::from_smol_str(FlowSmolStr::new(value.as_str()));
                                    let remote_module: Result<type_::ModuleType, Type> = match cx
                                        .builtin_module_opt(&import_specifier)
                                    {
                                        Some((_, m)) => Ok(m.get_forced(cx).dupe()),
                                        None => Err(flow_js_utils::lookup_builtin_module_error(
                                            cx,
                                            value,
                                            loc.dupe(),
                                        )
                                        .expect("Should not be under speculation")),
                                    };
                                    let str_t = mk_singleton_string(str_loc.dupe(), value.as_str());
                                    let module_reason = reason::mk_annot_reason(
                                        reason::VirtualReasonDesc::RModule(import_specifier.dupe()),
                                        loc.dupe(),
                                    );
                                    let namespace_symbol =
                                        Symbol::mk_module_symbol(value.dupe(), loc.dupe());
                                    let (_def_loc_opt, require_t) =
                                        flow_js_utils::import_export_utils::cjs_require_type(
                                            cx,
                                            module_reason,
                                            |cx, loc, t| {
                                                flow_js::reposition(cx, loc, t)
                                                    .expect("Should not be under speculation")
                                            },
                                            namespace_symbol,
                                            false,
                                            &remote_module,
                                        )
                                        .expect("Should not be under speculation");
                                    let str_ast = ast::types::Type::new(TypeInner::StringLiteral {
                                        loc: (str_loc.dupe(), str_t),
                                        literal: str_lit.clone(),
                                    });
                                    reconstruct_ast(
                                        require_t.dupe(),
                                        None,
                                        Some(ast::types::TypeArgs {
                                            loc: {
                                                let Ok(v) = typed_ast_utils::ErrorMapper
                                                    .on_type_annot(&targs_node.loc);
                                                v
                                            },
                                            arguments: Arc::from(vec![str_ast]),
                                            comments: targs_node.comments.dupe(),
                                        }),
                                    )
                                }
                                _ => error_type(
                                    cx,
                                    loc.dupe(),
                                    ErrorMessage::EExportsAnnot(loc.dupe()),
                                    t,
                                ),
                            }
                        })
                    }
                    "$KeyMirror" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let t1 = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RObjectKeyMirror,
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &t1,
                                    &type_::Destructor::TypeMap(type_::TypeMap::ObjectKeyMirror),
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    // Class<T> is the type of the class whose instances are of type T
                    "Class" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RStatics(Arc::new(
                                    type_util::desc_of_t(&tp).clone(),
                                )),
                                loc.dupe(),
                            );
                            reconstruct_ast(
                                Type::new(type_::TypeInner::DefT(
                                    reason,
                                    type_::DefT::new(type_::DefTInner::ClassT(tp)),
                                )),
                                None,
                                targs_ast,
                            )
                        })
                    }
                    "Function" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 0, || {
                            add_unclear_type_error_if_not_lib_file(cx, loc.dupe());
                            let reason = reason::mk_annot_reason(
                                reason::VirtualReasonDesc::RFunctionType,
                                loc.dupe(),
                            );
                            reconstruct_ast(
                                type_::any_t::make(type_::AnySource::AnnotatedAny, reason),
                                None,
                                None,
                            )
                        })
                    }
                    "Object" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 0, || {
                            add_unclear_type_error_if_not_lib_file(cx, loc.dupe());
                            let reason = reason::mk_annot_reason(
                                reason::VirtualReasonDesc::RObjectType,
                                loc.dupe(),
                            );
                            reconstruct_ast(
                                type_::any_t::make(type_::AnySource::AnnotatedAny, reason),
                                None,
                                None,
                            )
                        })
                    }
                    "$EnumValue" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::DollarUtilityTypeWithNonDollarAliases(
                                        FlowSmolStr::new_inline("EnumValue"),
                                    ),
                                ),
                            );
                        }
                        let reason = reason::mk_annot_reason(
                            reason::VirtualReasonDesc::REnum { name: None },
                            loc.dupe(),
                        );
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let representation_t = ts.into_iter().next().unwrap();
                            reconstruct_ast(
                                Type::new(type_::TypeInner::DefT(
                                    reason,
                                    type_::DefT::new(type_::DefTInner::EnumValueT(Rc::new(
                                        type_::EnumInfo::new(type_::EnumInfoInner::AbstractEnum {
                                            representation_t,
                                        }),
                                    ))),
                                )),
                                None,
                                targs_ast,
                            )
                        })
                    }
                    "$Enum" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::DollarUtilityTypeWithNonDollarAliases(
                                        FlowSmolStr::new_inline("Enum"),
                                    ),
                                ),
                            );
                        }
                        let reason = reason::mk_annot_reason(
                            reason::VirtualReasonDesc::REnum { name: None },
                            loc.dupe(),
                        );
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::EnumType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    "Function$Prototype$Bind" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 0, || {
                            let reason = reason::mk_annot_reason(
                                reason::VirtualReasonDesc::RFunctionPrototype,
                                loc.dupe(),
                            );
                            reconstruct_ast(
                                Type::new(type_::TypeInner::FunProtoBindT(reason)),
                                None,
                                None,
                            )
                        })
                    }
                    "React$ElementConfig" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::ReactDollarUtilityTypesWithNonDollarAliases(
                                        FlowSmolStr::new_inline("ElementConfig"),
                                    ),
                                ),
                            );
                        }
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            let reason = reason::mk_reason(
                                reason::VirtualReasonDesc::RType(Name::new(
                                    FlowSmolStr::new_inline("React$ElementConfig"),
                                )),
                                loc.dupe(),
                            );
                            let result_t =
                                FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                    cx,
                                    use_op(&reason),
                                    &reason,
                                    &tp,
                                    &type_::Destructor::ReactElementConfigType,
                                    mk_eval_id(cx, loc.dupe()),
                                )
                                .expect("mk_type_destructor should not fail");
                            reconstruct_ast(result_t, None, targs_ast)
                        })
                    }
                    "$Flow$EnforceOptimized" => {
                        check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                            let (ts, targs_ast) =
                                convert_type_params(cx, env, inner.targs.as_ref());
                            let tp = ts.into_iter().next().unwrap();
                            cx.set_union_opt(loc.dupe(), tp.dupe());
                            reconstruct_ast(tp, None, targs_ast)
                        })
                    }
                    // TS Types
                    "Readonly" => {
                        if cx.ts_syntax() || cx.ts_utility_syntax() {
                            check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                                let (ts, targs_ast) =
                                    convert_type_params(cx, env, inner.targs.as_ref());
                                let tp = ts.into_iter().next().unwrap();
                                let reason = reason::mk_reason(
                                    reason::VirtualReasonDesc::RReadOnlyType,
                                    loc.dupe(),
                                );
                                let result_t =
                                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                        cx,
                                        use_op(&reason),
                                        &reason,
                                        &tp,
                                        &type_::Destructor::ReadOnlyType,
                                        mk_eval_id(cx, loc.dupe()),
                                    )
                                    .expect("mk_type_destructor should not fail");
                                reconstruct_ast(result_t, None, targs_ast)
                            })
                        } else {
                            error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind: intermediate_error_types::IncorrectType::TSReadonly,
                                    },
                                )),
                                t,
                            )
                        }
                    }
                    "ReadonlyArray" => {
                        if cx.ts_syntax() || cx.ts_utility_syntax() {
                            check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                                let (elemts, targs_ast) =
                                    convert_type_params(cx, env, inner.targs.as_ref());
                                let elemt = elemts.into_iter().next().unwrap();
                                reconstruct_ast(
                                    Type::new(type_::TypeInner::DefT(
                                        reason::mk_annot_reason(
                                            reason::VirtualReasonDesc::RROArrayType,
                                            loc.dupe(),
                                        ),
                                        type_::DefT::new(type_::DefTInner::ArrT(Rc::new(
                                            type_::ArrType::ROArrayAT(Box::new((elemt, None))),
                                        ))),
                                    )),
                                    None,
                                    targs_ast,
                                )
                            })
                        } else {
                            error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind:
                                            intermediate_error_types::IncorrectType::TSReadonlyArray,
                                    },
                                )),
                                t,
                            )
                        }
                    }
                    "ReadonlyMap" if !(cx.ts_syntax() || cx.ts_utility_syntax()) => error_type(
                        cx,
                        loc.dupe(),
                        ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                            EIncorrectTypeWithReplacementData {
                                loc: loc.dupe(),
                                kind: intermediate_error_types::IncorrectType::TSReadonlyMap,
                            },
                        )),
                        t,
                    ),
                    "ReadonlySet" if !(cx.ts_syntax() || cx.ts_utility_syntax()) => error_type(
                        cx,
                        loc.dupe(),
                        ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                            EIncorrectTypeWithReplacementData {
                                loc: loc.dupe(),
                                kind: intermediate_error_types::IncorrectType::TSReadonlySet,
                            },
                        )),
                        t,
                    ),
                    "NonNullable" => {
                        if cx.ts_syntax() || cx.ts_utility_syntax() {
                            check_type_arg_arity(cx, loc.dupe(), t, inner.targs.as_ref(), 1, || {
                                let (ts, targs_ast) =
                                    convert_type_params(cx, env, inner.targs.as_ref());
                                let tp = ts.into_iter().next().unwrap();
                                let reason = reason::mk_reason(
                                    reason::VirtualReasonDesc::RType(Name::new(
                                        FlowSmolStr::new_inline("NonNullable"),
                                    )),
                                    loc.dupe(),
                                );
                                let result_t =
                                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                                        cx,
                                        use_op(&reason),
                                        &reason,
                                        &tp,
                                        &type_::Destructor::NonMaybeType,
                                        mk_eval_id(cx, loc.dupe()),
                                    )
                                    .expect("mk_type_destructor should not fail");
                                reconstruct_ast(result_t, None, targs_ast)
                            })
                        } else {
                            error_type(
                                cx,
                                loc.dupe(),
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(
                                    EIncorrectTypeWithReplacementData {
                                        loc: loc.dupe(),
                                        kind:
                                            intermediate_error_types::IncorrectType::TSNonNullable,
                                    },
                                )),
                                t,
                            )
                        }
                    }
                    "React$Node" => {
                        if !cx.is_lib_file() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EInternalType(
                                    loc.dupe(),
                                    InternalType::ReactDollarUtilityTypesWithNonDollarAliases(
                                        FlowSmolStr::new_inline("Node"),
                                    ),
                                ),
                            );
                        }
                        local_generic_type(
                            cx,
                            env,
                            name,
                            name_loc.dupe(),
                            loc.dupe(),
                            inner.targs.as_ref(),
                            &reconstruct_ast,
                        )
                    }
                    "$ReadOnlyMap" => {
                        if cx.is_utility_type_deprecated("$ReadOnlyMap") && cx.ts_utility_syntax() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(EIncorrectTypeWithReplacementData {
                                    loc: loc.dupe(),
                                    kind:
                                        intermediate_error_types::IncorrectType::DollarReadOnlyMap,
                                })),
                            );
                        }
                        local_generic_type(
                            cx,
                            env,
                            name,
                            name_loc.dupe(),
                            loc.dupe(),
                            inner.targs.as_ref(),
                            &reconstruct_ast,
                        )
                    }
                    "$ReadOnlySet" => {
                        if cx.is_utility_type_deprecated("$ReadOnlySet") && cx.ts_utility_syntax() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EIncorrectTypeWithReplacement(Box::new(EIncorrectTypeWithReplacementData {
                                    loc: loc.dupe(),
                                    kind:
                                        intermediate_error_types::IncorrectType::DollarReadOnlySet,
                                })),
                            );
                        }
                        local_generic_type(
                            cx,
                            env,
                            name,
                            name_loc.dupe(),
                            loc.dupe(),
                            inner.targs.as_ref(),
                            &reconstruct_ast,
                        )
                    }
                    // other applications with id as head expr
                    _ => local_generic_type(
                        cx,
                        env,
                        name,
                        name_loc.dupe(),
                        loc.dupe(),
                        inner.targs.as_ref(),
                        &reconstruct_ast,
                    ),
                }
            }
        },
        TypeInner::Function { loc, inner } => {
            let params_node = &inner.params;
            let params_loc = &params_node.loc;
            let ps = &params_node.params;
            let rest = &params_node.rest;
            let this_ = &params_node.this;
            let params_comments = &params_node.comments;
            let func_comments = &inner.comments;
            let effect_ = &inner.effect;
            error_on_unsupported_variance_annotation(cx, "function type", inner.tparams.as_ref());
            let saved_tparams_map = env.tparams_map.dupe();
            let (tparams, tparams_ast) = mk_type_param_declarations_inner(
                cx,
                env,
                flow_parser::ast_visitor::TypeParamsContext::FunctionType,
                inner.tparams.as_ref(),
            );
            let mut params = Vec::new();
            let mut param_asts = Vec::new();
            for param in ps.iter() {
                let param_loc = &param.loc;
                let (name, annot, optional) =
                    flow_parser::ast_utils::function_type_param_parts(&param.param);
                let annot_ast = convert_inner(cx, env, annot);
                let (_, t) = annot_ast.loc();
                let t = t.dupe();
                let t = if optional {
                    type_util::optional(t, None, false)
                } else {
                    t
                };
                let fun_param_name = name.map(ident_name);
                params.push(type_::FunParam(fun_param_name, t.dupe()));
                let param_ast = typed_function_param_ast(&param.param, t, annot_ast);
                param_asts.push(ast::types::function::Param {
                    loc: param_loc.dupe(),
                    param: param_ast,
                });
            }
            let (this_t, this_param_ast) = match this_ {
                None => (type_::bound_function_dummy_this(params_loc.dupe()), None),
                Some(tp) => {
                    let this_loc = &tp.loc;
                    let annot = &tp.annot;
                    let comments = &tp.comments;
                    let annot_ast = convert_inner(cx, env, &annot.annotation);
                    let (_, this_t) = annot_ast.loc();
                    let this_t = this_t.dupe();
                    (
                        this_t,
                        Some(ast::types::function::ThisParam {
                            loc: this_loc.dupe(),
                            annot: ast::types::Annotation {
                                loc: annot.loc.dupe(),
                                annotation: annot_ast,
                            },
                            comments: comments.clone(),
                        }),
                    )
                }
            };
            let reason =
                reason::mk_annot_reason(reason::VirtualReasonDesc::RFunctionType, loc.dupe());
            let (rest_param, rest_param_ast) = match rest {
                Some(rp) => {
                    let rest_loc = &rp.loc;
                    let param = &rp.argument;
                    let param_loc = &param.loc;
                    let (name, annot, _optional) =
                        flow_parser::ast_utils::function_type_param_parts(&param.param);
                    let annot_ast = convert_inner(cx, env, annot);
                    let (_, rest_t) = annot_ast.loc();
                    let rest_t = rest_t.dupe();
                    let rest_name = name.map(ident_name);
                    let rest_param = Some(type_::FunRestParam(
                        rest_name,
                        type_util::loc_of_t(&rest_t).dupe(),
                        rest_t.dupe(),
                    ));
                    let rest_param_ast_inner =
                        typed_function_param_ast(&param.param, rest_t, annot_ast);
                    let rest_ast = Some(ast::types::function::RestParam {
                        loc: rest_loc.dupe(),
                        argument: ast::types::function::Param {
                            loc: param_loc.dupe(),
                            param: rest_param_ast_inner,
                        },
                        comments: rp.comments.dupe(),
                    });
                    (rest_param, rest_ast)
                }
                None => (None, None),
            };
            let params_ast_node = ast::types::function::Params {
                loc: params_loc.dupe(),
                params: param_asts.into(),
                rest: rest_param_ast,
                this: this_param_ast,
                comments: params_comments.clone(),
            };
            let fparams = params;
            let (return_t, return_ast, type_guard) = convert_return_annotation(
                cx,
                env,
                MethodKind::FunctionKind,
                &inner.params,
                &fparams,
                &inner.return_,
            );
            env.tparams_map = saved_tparams_map;
            let statics_reason = reason
                .dupe()
                .update_desc(|d| reason::VirtualReasonDesc::RStatics(Arc::new(d)));
            let fun_proto_t = Type::new(type_::TypeInner::FunProtoT(statics_reason.dupe()));
            let statics_t = flow_typing_flow_common::obj_type::mk_with_proto(
                cx,
                statics_reason,
                type_::ObjKind::Inexact,
                None,
                None,
                None,
                None,
                fun_proto_t,
            );
            let effect_flag = match effect_ {
                ast::function::Effect::Hook => type_::ReactEffectType::HookAnnot,
                ast::function::Effect::Arbitrary => type_::ReactEffectType::ArbitraryEffect,
            };
            let ft = Type::new(type_::TypeInner::DefT(
                reason.dupe(),
                type_::DefT::new(type_::DefTInner::FunT(
                    statics_t,
                    Rc::new(type_::FunType {
                        this_t: (this_t, type_::ThisStatus::ThisFunction),
                        params: fparams.into(),
                        rest_param,
                        return_t,
                        type_guard,
                        def_reason: reason.dupe(),
                        effect_: effect_flag,
                    }),
                )),
            ));
            let t = match tparams {
                None => ft,
                Some((ref tparams_loc, ref tparams_nel)) => {
                    let id = cx.make_source_poly_id(false, tparams_loc);
                    type_util::poly_type(id, tparams_loc.dupe(), tparams_nel.clone(), ft)
                }
            };
            ast::types::Type::new(TypeInner::Function {
                loc: (loc.dupe(), t),
                inner: ast::types::Function {
                    tparams: tparams_ast,
                    params: params_ast_node,
                    return_: return_ast,
                    comments: func_comments.clone(),
                    effect: effect_.clone(),
                }
                .into(),
            })
        }
        TypeInner::Component { loc, inner } => {
            let reason = reason::mk_reason(reason::VirtualReasonDesc::RComponentType, loc.dupe());
            let (t, tparams_ast, params_ast, renders_ast) = mk_component(
                cx,
                env,
                reason,
                None,
                inner.tparams.as_ref(),
                &inner.params,
                &inner.renders,
            );
            ast::types::Type::new(TypeInner::Component {
                loc: (loc.dupe(), t),
                inner: (ast::types::Component {
                    tparams: tparams_ast,
                    params: params_ast,
                    renders: renders_ast,
                    comments: inner.comments.dupe(),
                })
                .into(),
            })
        }
        TypeInner::Object {
            loc: obj_loc,
            inner,
        } if inner.properties.len() == 1
            && let ast::types::object::Property::MappedType(mt) = &inner.properties[0] =>
        {
            let exact = inner.exact;
            let inexact = inner.inexact;
            let comments = &inner.comments;
            let mapped_type_loc = &mt.loc;
            let key_tparam = &mt.key_tparam;
            let prop_type = &mt.prop_type;
            let source_type = &mt.source_type;
            let name_type = &mt.name_type;
            let variance = &mt.variance;
            let variance_op = &mt.variance_op;
            let optional = mt.optional;
            let mapped_type_comments = &mt.comments;
            if exact || inexact {
                // Mapped types are implemented with the following limitations:
                // 1. Mapped types cannot be declared with additional properties
                // 2. Mapped types do not support explicit exact or inexact modifiers
                // 3. Mapped types do not yet support optional property removal via -?
                // 4. Mapped types must use an inline keyof
                // All of these conditions are checked in this case, and the extra properties
                // case is additionally checked in the normal object type case. If any of these
                // conditions are violated then the result is Any
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInvalidMappedType {
                        loc: obj_loc.dupe(),
                        kind: flow_typing_errors::error_message::InvalidMappedTypeErrorKind::ExplicitExactOrInexact,
                    },
                );
                {
                    let Ok(v) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                    v
                }
            } else if name_type.is_some() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        obj_loc.dupe(),
                        flow_typing_errors::intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            flow_typing_errors::intermediate_error_types::TsLibSyntaxKind::MappedTypeKeyRemapping,
                        ),
                    ))),
                );
                {
                    let Ok(v) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                    v
                }
            } else if variance_op.is_some() && !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        mapped_type_loc.dupe(),
                        flow_typing_errors::intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            flow_typing_errors::intermediate_error_types::TsLibSyntaxKind::ReadonlyMappedTypeVarianceOp,
                        ),
                    ))),
                );
                {
                    let Ok(v) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                    v
                }
            } else {
                use flow_parser::ast::types::object::MappedTypeOptionalFlag;
                let mapped_type_optionality = match optional {
                    MappedTypeOptionalFlag::PlusOptional | MappedTypeOptionalFlag::Optional => {
                        type_::MappedTypeOptionality::MakeOptional
                    }
                    MappedTypeOptionalFlag::MinusOptional => {
                        type_::MappedTypeOptionality::RemoveOptional
                    }
                    MappedTypeOptionalFlag::NoOptionalFlag => {
                        type_::MappedTypeOptionality::KeepOptionality
                    }
                };
                let (homomorphic, source_type_t, source_ast, distributive_tparam_name) = {
                    use flow_parser::ast::types::TypeInner;
                    use flow_parser::ast::types::generic::Identifier as GenericId;
                    match source_type.deref() {
                        TypeInner::Generic {
                            inner: gen_inner, ..
                        } if let GenericId::Unqualified(ident) = &gen_inner.id => {
                            let gen_name = &ident.name;
                            let subst_name = SubstName::name(gen_name.dupe());
                            use flow_typing_type::type_::TypeInner as TI;

                            match env.tparams_map.get(&subst_name).map(|v| v.dupe()) {
                                Some(ref tparam_type)
                                    if let TI::GenericT(box GenericTData { bound, .. }) =
                                        tparam_type.deref()
                                        && let TI::KeysT(_, obj_t) = bound.deref() =>
                                {
                                    let source_ast = convert_inner(cx, env, source_type);
                                    let (_, selected_keys) = source_ast.loc();
                                    let selected_keys = selected_keys.dupe();
                                    let distributive_tparam_name = match obj_t.deref() {
                                        TI::GenericT(box GenericTData {
                                            name: obj_name,
                                            reason: obj_reason,
                                            ..
                                        }) => use_distributive_tparam_name(
                                            cx,
                                            obj_name.string_of_subst_name(),
                                            obj_reason.def_loc().dupe(),
                                            &mut env.tparams_map,
                                        ),
                                        _ => None,
                                    };
                                    (
                                        type_::MappedTypeHomomorphicFlag::SemiHomomorphic(
                                            selected_keys,
                                        ),
                                        obj_t.dupe(),
                                        source_ast,
                                        distributive_tparam_name,
                                    )
                                }
                                _ => {
                                    let source_ast = convert_inner(cx, env, source_type);
                                    let (_, source_type_t) = source_ast.loc();
                                    let source_type_t = source_type_t.dupe();
                                    (
                                        type_::MappedTypeHomomorphicFlag::Unspecialized,
                                        source_type_t,
                                        source_ast,
                                        None,
                                    )
                                }
                            }
                        }
                        TypeInner::Keyof {
                            loc: keyof_loc,
                            inner: keyof_inner,
                        } => {
                            let argument = &keyof_inner.argument;
                            let keyof_comments = &keyof_inner.comments;
                            let arg_ast = convert_inner(cx, env, argument);
                            let (_, source_type_t) = arg_ast.loc();
                            let source_type_t = source_type_t.dupe();
                            let source_ast = ast::types::Type::new(TypeInner::Keyof {
                                loc: (keyof_loc.dupe(), source_type_t.dupe()),
                                inner: (ast::types::Keyof {
                                    argument: arg_ast,
                                    comments: keyof_comments.clone(),
                                })
                                .into(),
                            });
                            let distributive_tparam_name = use_distributive_tparam_name_from_ast(
                                cx,
                                argument,
                                &mut env.tparams_map,
                            );
                            (
                                type_::MappedTypeHomomorphicFlag::Homomorphic,
                                source_type_t,
                                source_ast,
                                distributive_tparam_name,
                            )
                        }
                        _ => {
                            let source_ast = convert_inner(cx, env, source_type);
                            let (_, source_type_t) = source_ast.loc();
                            let source_type_t = source_type_t.dupe();
                            (
                                type_::MappedTypeHomomorphicFlag::Unspecialized,
                                source_type_t,
                                source_ast,
                                None,
                            )
                        }
                    }
                };
                match mapped_type_optionality {
                    type_::MappedTypeOptionality::MakeOptional
                    | type_::MappedTypeOptionality::KeepOptionality => {
                        let (tparam_ast, tparam, tparam_t) = mk_type_param_inner(
                            cx,
                            env,
                            flow_parser::ast_visitor::TypeParamsContext::ObjectMappedType,
                            key_tparam,
                        );
                        let tparam_name = tparam.name.dupe();
                        env.tparams_map.insert(tparam_name.dupe(), tparam_t);
                        let prop_type_ast = convert_inner(cx, env, prop_type);
                        let (prop_loc, prop_type_t) = prop_type_ast.loc();
                        let prop_loc = prop_loc.dupe();
                        let prop_type_t = prop_type_t.dupe();
                        let poly_prop_type = type_util::poly_type_of_tparams(
                            cx.make_source_poly_id(false, &prop_loc),
                            Some((key_tparam.loc.dupe(), vec1::Vec1::new(tparam))),
                            prop_type_t,
                        );
                        let reason = reason::mk_reason(
                            reason::VirtualReasonDesc::RMappedType,
                            obj_loc.dupe(),
                        );
                        let use_op = type_::VirtualUseOp::Op(Arc::new(
                            type_::VirtualRootUseOp::EvalMappedType {
                                mapped_type: reason.dupe(),
                            },
                        ));
                        let eval_t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                            cx,
                            use_op,
                            &reason,
                            &source_type_t,
                            &type_::Destructor::MappedType(Box::new(DestructorMappedTypeData {
                                homomorphic,
                                property_type: poly_prop_type.dupe(),
                                mapped_type_flags: type_::MappedTypeFlags {
                                    optional: mapped_type_optionality,
                                    variance: {
                                        use flow_parser::ast::types::object::MappedTypeVarianceOp;
                                        match (variance, variance_op) {
                                            (Some(v), Some(MappedTypeVarianceOp::Add))
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Readonly =>
                                            {
                                                type_::MappedTypeVariance::OverrideVariance(
                                                    Polarity::Positive,
                                                )
                                            }
                                            (Some(v), None)
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Readonly =>
                                            {
                                                type_::MappedTypeVariance::OverrideVariance(
                                                    Polarity::Positive,
                                                )
                                            }
                                            (Some(v), Some(MappedTypeVarianceOp::Remove))
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Readonly =>
                                            {
                                                type_::MappedTypeVariance::RemoveVariance(
                                                    Polarity::Positive,
                                                )
                                            }
                                            (Some(v), Some(MappedTypeVarianceOp::Add))
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Writeonly =>
                                            {
                                                type_::MappedTypeVariance::OverrideVariance(
                                                    Polarity::Negative,
                                                )
                                            }
                                            (Some(v), None)
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Writeonly =>
                                            {
                                                type_::MappedTypeVariance::OverrideVariance(
                                                    Polarity::Negative,
                                                )
                                            }
                                            (Some(v), Some(MappedTypeVarianceOp::Remove))
                                                if v.kind
                                                    == flow_parser::ast::VarianceKind::Writeonly =>
                                            {
                                                type_::MappedTypeVariance::RemoveVariance(
                                                    Polarity::Negative,
                                                )
                                            }
                                            _ => {
                                                let pol = polarity(cx, variance.as_ref());
                                                if pol == Polarity::Neutral {
                                                    type_::MappedTypeVariance::KeepVariance
                                                } else {
                                                    type_::MappedTypeVariance::OverrideVariance(pol)
                                                }
                                            }
                                        }
                                    },
                                },
                                distributive_tparam_name,
                            })),
                            type_::eval::Id::generate_id(),
                        )
                        .expect("mk_type_destructor not in speculation");
                        let prop_ast = ast::types::object::Property::MappedType(
                            ast::types::object::MappedType {
                                loc: mapped_type_loc.dupe(),
                                source_type: source_ast,
                                prop_type: prop_type_ast,
                                key_tparam: tparam_ast,
                                name_type: None,
                                variance: variance.clone(),
                                variance_op: *variance_op,
                                optional,
                                comments: mapped_type_comments.clone(),
                            },
                        );
                        ast::types::Type::new(TypeInner::Object {
                            loc: (obj_loc.dupe(), eval_t),
                            inner: (ast::types::Object {
                                exact,
                                inexact,
                                properties: vec![prop_ast].into(),
                                comments: comments.clone(),
                            })
                            .into(),
                        })
                    }
                    type_::MappedTypeOptionality::RemoveOptional => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EInvalidMappedType {
                                loc: mapped_type_loc.dupe(),
                                kind: flow_typing_errors::error_message::InvalidMappedTypeErrorKind::RemoveOptionality,
                            },
                        );
                        {
                            let Ok(v) =
                                polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                            v
                        }
                    }
                }
            }
        }
        TypeInner::Object { loc, inner } => {
            let exact_by_default = cx.exact_by_default();
            let exact = inner.exact;
            let inexact = inner.inexact;
            let properties = &inner.properties;
            let comments = &inner.comments;
            let exact_type = exact || (!inexact && exact_by_default);
            let mut has_indexer = false;
            let mut mapped_type_loc: Option<ALoc> = None;
            for property in properties.iter() {
                match property {
                    ast::types::object::Property::Indexer(_) => {
                        has_indexer = true;
                    }
                    ast::types::object::Property::MappedType(mt) => {
                        mapped_type_loc = Some(mt.loc.dupe());
                    }
                    _ => {}
                }
            }
            match mapped_type_loc {
                Some(_) => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInvalidMappedType {
                            loc: loc.dupe(),
                            kind: flow_typing_errors::error_message::InvalidMappedTypeErrorKind::ExtraProperties,
                        },
                    );
                    {
                        let Ok(v) =
                            polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                        v
                    }
                }
                None => {
                    let (obj_t, properties_ast) =
                        convert_object(cx, env, loc.dupe(), exact_type, properties);
                    if !exact && !inexact && !has_indexer {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EAmbiguousObjectType(loc.dupe()),
                        );
                        if !exact_by_default {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EImplicitInexactObject(loc.dupe()),
                            );
                        }
                    }
                    ast::types::Type::new(TypeInner::Object {
                        loc: (loc.dupe(), obj_t),
                        inner: (ast::types::Object {
                            exact,
                            inexact,
                            properties: properties_ast.into(),
                            comments: comments.clone(),
                        })
                        .into(),
                    })
                }
            }
        }
        TypeInner::Interface { loc, inner } => {
            let (ref body_loc, ref body) = inner.body;
            let properties = &body.properties;
            let body_exact = body.exact;
            let object_comments = &body.comments;
            let extends = &inner.extends;
            let iface_comments = &inner.comments;
            let reason =
                reason::mk_annot_reason(reason::VirtualReasonDesc::RInterfaceType, loc.dupe());
            let id = cx.make_aloc_id(loc);
            let mut extends_types = Vec::new();
            let mut extend_asts = Vec::new();
            for ext in extends.iter() {
                let (typeapp, ext_ast) = mk_interface_super(cx, env, ext);
                extends_types.push(typeapp);
                extend_asts.push(ext_ast);
            }
            let callable = properties.iter().any(|prop| match prop {
                ast::types::object::Property::CallProperty(cp) => !cp.static_,
                _ => false,
            });
            let super_ = func_class_sig_types::class::Super::Interface(
                func_class_sig_types::class::InterfaceSuper {
                    inline: true,
                    extends: extends_types,
                    callable,
                },
            );
            let iface_sig = class_sig::empty(
                id,
                None,
                loc.dupe(),
                reason.dupe(),
                None,
                env.tparams_map
                    .iter()
                    .map(|(k, v)| (k.dupe(), v.dupe()))
                    .collect(),
                super_,
            );
            let this = type_::implicit_mixed_this(reason.dupe());
            let (iface_sig, property_asts) = add_interface_properties(
                cx,
                env,
                None,
                intermediate_error_types::ObjKind::Interface,
                this,
                properties,
                iface_sig,
            );
            class_sig::check_signature_compatibility(cx, reason.dupe(), &iface_sig);
            let iface_t = class_sig::thistype(cx, &iface_sig);
            ast::types::Type::new(TypeInner::Interface {
                loc: (loc.dupe(), iface_t),
                inner: ast::types::Interface {
                    body: (
                        body_loc.dupe(),
                        ast::types::Object {
                            exact: body_exact,
                            inexact: false,
                            properties: property_asts.into(),
                            comments: object_comments.clone(),
                        },
                    ),
                    extends: extend_asts.into(),
                    comments: iface_comments.clone(),
                }
                .into(),
            })
        }
        TypeInner::TemplateLiteral { loc, .. } => {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                        TsLibSyntaxKind::TemplateLiteralType,
                    ),
                ))),
            );
            let Ok(v) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
            v
        }
        TypeInner::ConstructorType {
            loc,
            abstract_,
            inner: func,
        } => {
            if !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::ConstructorType,
                        ),
                    ))),
                );
                let Ok(v) = polymorphic_ast_mapper::type_(&mut typed_ast_utils::ErrorMapper, t);
                v
            } else {
                // Desugar `new (params) => T` to an inline interface: `interface { new(params): T }`
                let reason =
                    reason::mk_annot_reason(reason::VirtualReasonDesc::RInterfaceType, loc.dupe());
                let id = cx.make_aloc_id(loc);
                let super_ = func_class_sig_types::class::Super::Interface(
                    func_class_sig_types::class::InterfaceSuper {
                        inline: true,
                        extends: vec![],
                        callable: false,
                    },
                );
                let mut iface_sig = class_sig::empty(
                    id,
                    None,
                    loc.dupe(),
                    reason.dupe(),
                    None,
                    env.tparams_map
                        .iter()
                        .map(|(k, v)| (k.dupe(), v.dupe()))
                        .collect(),
                    super_,
                );
                let (fsig, func_ast) = mk_method_func_sig(
                    cx,
                    env,
                    MethodKind::MethodKind { is_static: false },
                    loc.dupe(),
                    func,
                );
                class_sig::append_method(
                    false,
                    "new".into(),
                    loc.dupe(),
                    None,
                    fsig,
                    None,
                    None,
                    &mut iface_sig,
                );
                class_sig::check_signature_compatibility(cx, reason.dupe(), &iface_sig);
                let iface_t = class_sig::thistype(cx, &iface_sig);
                ast::types::Type::new(TypeInner::ConstructorType {
                    loc: (loc.dupe(), iface_t),
                    abstract_: *abstract_,
                    inner: func_ast.into(),
                })
            }
        }
        TypeInner::Exists { loc, comments } => {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    intermediate_error_types::UnsupportedSyntax::ExistsType,
                ))),
            );
            let rt = any_t::at(AnySource::AnnotatedAny, loc.dupe());
            ast::types::Type::new(TypeInner::Exists {
                loc: (loc.dupe(), rt),
                comments: comments.clone(),
            })
        }
    }
}

fn convert_list_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    asts: &[ast::types::Type<ALoc, ALoc>],
) -> (Vec<Type>, Vec<ast::types::Type<ALoc, (ALoc, Type)>>) {
    let mut ts = Vec::new();
    let mut tasts = Vec::new();
    for ast in asts {
        let tast = convert_inner(cx, env, ast);
        let (_, t) = tast.loc();
        ts.push(t.dupe());
        tasts.push(tast);
    }
    (ts, tasts)
}

pub fn convert_opt<'a>(
    cx: &Context<'a>,
    tparams_map: &FlowOrdMap<SubstName, Type>,
    ast_opt: Option<&ast::types::Type<ALoc, ALoc>>,
) -> (Option<Type>, Option<ast::types::Type<ALoc, (ALoc, Type)>>) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map.dupe());
    let tast_opt = ast_opt.map(|ast| convert_inner(cx, &mut env, ast));
    let t_opt = tast_opt.as_ref().map(|tast| {
        let (_, t) = tast.loc();
        t.dupe()
    });
    (t_opt, tast_opt)
}

pub fn convert_qualification<'a>(
    cx: &Context<'a>,
    reason_prefix: &str,
    id: &ast::types::generic::Identifier<ALoc, ALoc>,
) -> (Type, ast::types::generic::Identifier<ALoc, (ALoc, Type)>) {
    convert_qualification_with_lookup_mode(cx, None, reason_prefix, id)
}

fn convert_qualification_with_lookup_mode<'a>(
    cx: &Context<'a>,
    lookup_mode: Option<type_env::LookupMode>,
    reason_prefix: &str,
    id: &ast::types::generic::Identifier<ALoc, ALoc>,
) -> (Type, ast::types::generic::Identifier<ALoc, (ALoc, Type)>) {
    use flow_parser::ast::types::generic::Identifier;
    let lookup_mode = lookup_mode.unwrap_or(type_env::LookupMode::ForType);
    match id {
        Identifier::Qualified(q) => {
            let (m, qualification_ast) = convert_qualification_with_lookup_mode(
                cx,
                Some(lookup_mode),
                reason_prefix,
                &q.qualification,
            );
            let id_loc = q.id.loc.dupe();
            let id_name = &q.id;
            let name = &id_name.name;
            let qname = qualified_name(id);
            let desc = reason::VirtualReasonDesc::RCustom(FlowSmolStr::new(format!(
                "{} `{}`",
                reason_prefix, qname
            )));
            let id_reason = reason::mk_reason(desc, id_loc.dupe());
            let op_reason = reason::mk_reason(
                reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new(&qname))),
                q.loc.dupe(),
            );
            let use_op =
                type_::UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(op_reason.dupe())));
            let t = if lookup_mode == type_env::LookupMode::ForType {
                crate::type_annotation_cons_gen::qualify_type(
                    cx,
                    use_op,
                    id_reason,
                    op_reason,
                    Name::new(name.dupe()),
                    m,
                )
            } else {
                crate::type_annotation_cons_gen::get_prop(
                    cx,
                    use_op,
                    id_reason,
                    None,
                    Name::new(name.dupe()),
                    m,
                )
            };
            let result_ast = Identifier::Qualified(
                ast::types::generic::Qualified {
                    loc: q.loc.dupe(),
                    qualification: qualification_ast,
                    id: ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_loc, t.dupe()),
                        name: name.dupe(),
                        comments: id_name.comments.dupe(),
                    }),
                }
                .into(),
            );
            (t, result_ast)
        }
        Identifier::Unqualified(ident) => {
            let t = type_env::get_var(Some(lookup_mode), cx, &ident.name, ident.loc.dupe());
            (
                t.dupe(),
                Identifier::Unqualified(ast::Identifier::new(ast::IdentifierInner {
                    loc: (ident.loc.dupe(), t),
                    name: ident.name.dupe(),
                    comments: ident.comments.dupe(),
                })),
            )
        }
        Identifier::ImportTypeAnnot(import) => {
            if !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        import.loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::ImportTypeAnnotation,
                        ),
                    ))),
                );
                let t = type_::any_t::at(type_::AnySource::AnyError(None), import.loc.dupe());
                (
                    t.dupe(),
                    Identifier::ImportTypeAnnot(
                        (ast::types::generic::ImportType {
                            loc: (import.loc.dupe(), t),
                            argument: import.argument.clone(),
                            comments: import.comments.dupe(),
                        })
                        .into(),
                    ),
                )
            } else {
                // Resolve the module
                let module_name = &import.argument.1.value;
                let mref = Userland::from_smol_str(module_name.dupe());
                let source_module = flow_js_utils::import_export_utils::get_module_type_or_any(
                    cx,
                    false,
                    Some(type_::ImportKind::ImportType),
                    import.argument.0.dupe(),
                    mref.dupe(),
                )
                .expect("Should not be under speculation");
                let reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RModule(mref.dupe()),
                    import.loc.dupe(),
                );
                let namespace_symbol =
                    Symbol::mk_module_symbol(FlowSmolStr::new(mref.as_str()), import.loc.dupe());
                let t = flow_js_utils::import_export_utils::get_module_namespace_type(
                    cx,
                    reason,
                    namespace_symbol,
                    &source_module,
                )
                .expect("Should not be under speculation");
                (
                    t.dupe(),
                    Identifier::ImportTypeAnnot(
                        ast::types::generic::ImportType {
                            loc: (import.loc.dupe(), t),
                            argument: import.argument.clone(),
                            comments: import.comments.dupe(),
                        }
                        .into(),
                    ),
                )
            }
        }
    }
}

fn convert_typeof<'a>(
    cx: &Context<'a>,
    reason_prefix: &str,
    target: &ast::types::typeof_::Target<ALoc, ALoc>,
) -> (Type, ast::types::typeof_::Target<ALoc, (ALoc, Type)>) {
    use flow_parser::ast::types::typeof_::Target;
    match target {
        Target::Qualified(q) => {
            let (m, qualification_ast) = convert_typeof(cx, reason_prefix, &q.qualification);
            let id_loc = q.id.loc.dupe();
            let id_name = &q.id;
            let name = &id_name.name;
            let tname = typeof_name(target);
            let desc = reason::VirtualReasonDesc::RCustom(FlowSmolStr::new(format!(
                "{} `{}`",
                reason_prefix, tname
            )));
            let id_reason = reason::mk_reason(desc, id_loc.dupe());
            let use_op =
                type_::UseOp::Op(Arc::new(type_::RootUseOp::GetProperty(reason::mk_reason(
                    reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new(&tname))),
                    q.loc.dupe(),
                ))));
            let t = crate::type_annotation_cons_gen::get_prop(
                cx,
                use_op,
                id_reason,
                None,
                Name::new(name.dupe()),
                m,
            );
            let final_ast = Target::Qualified(
                ast::types::typeof_::Qualified {
                    loc: (q.loc.dupe(), t.dupe()),
                    qualification: qualification_ast,
                    id: ast::Identifier::new(ast::IdentifierInner {
                        loc: (id_loc, t.dupe()),
                        name: name.dupe(),
                        comments: id_name.comments.dupe(),
                    }),
                }
                .into(),
            );
            (t, final_ast)
        }
        Target::Unqualified(ident) => {
            if ident.name == "this" && !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        ident.loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::TypeofThis,
                        ),
                    ))),
                );
                let t = type_::any_t::at(type_::AnySource::AnyError(None), ident.loc.dupe());
                let Ok(mapped) = polymorphic_ast_mapper::typeof_expression(
                    &mut typed_ast_utils::ErrorMapper,
                    target,
                );
                (t, mapped)
            } else {
                let t = if flow_typing_flow_js::type_inference_hooks_js::dispatch_id_hook(
                    cx,
                    &ident.name,
                    ident.loc.dupe(),
                ) {
                    type_::unsoundness::at(type_::UnsoundnessKind::InferenceHooks, ident.loc.dupe())
                } else {
                    type_env::get_var(
                        Some(type_env::LookupMode::ForTypeof),
                        cx,
                        &ident.name,
                        ident.loc.dupe(),
                    )
                };
                (
                    t.dupe(),
                    Target::Unqualified(ast::Identifier::new(ast::IdentifierInner {
                        loc: (ident.loc.dupe(), t),
                        name: ident.name.dupe(),
                        comments: ident.comments.dupe(),
                    })),
                )
            }
        }
        Target::Import(import) => {
            if !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        import.loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::TypeofImport,
                        ),
                    ))),
                );
                let t = type_::any_t::at(type_::AnySource::AnyError(None), import.loc.dupe());
                let Ok(mapped) = polymorphic_ast_mapper::typeof_expression(
                    &mut typed_ast_utils::ErrorMapper,
                    target,
                );
                (t, mapped)
            } else {
                // Resolve the module
                let module_name = &import.argument.1.value;
                let mref = Userland::from_smol_str(module_name.dupe());
                let source_module = flow_js_utils::import_export_utils::get_module_type_or_any(
                    cx,
                    false,
                    Some(type_::ImportKind::ImportValue),
                    import.argument.0.dupe(),
                    mref.dupe(),
                )
                .expect("Should not be under speculation");
                let reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RModule(mref.dupe()),
                    import.loc.dupe(),
                );
                let namespace_symbol =
                    Symbol::mk_module_symbol(FlowSmolStr::new(mref.as_str()), import.loc.dupe());
                let t = flow_js_utils::import_export_utils::get_module_namespace_type(
                    cx,
                    reason,
                    namespace_symbol,
                    &source_module,
                )
                .expect("Should not be under speculation");
                (
                    t.dupe(),
                    Target::Import(
                        ast::types::generic::ImportType {
                            loc: (import.loc.dupe(), t),
                            argument: import.argument.clone(),
                            comments: import.comments.dupe(),
                        }
                        .into(),
                    ),
                )
            }
        }
    }
}

fn convert_render_type_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    loc: ALoc,
    renders: &ast::types::Renders<ALoc, ALoc>,
) -> (Type, ast::types::Renders<ALoc, (ALoc, Type)>) {
    let old_in_renders_arg = env.in_renders_arg;
    env.in_renders_arg = true;
    let t_ast = convert_inner(cx, env, &renders.argument);
    env.in_renders_arg = old_in_renders_arg;
    let (_, t) = t_ast.loc();
    let t = t.dupe();
    let arg_desc = Arc::new(type_util::reason_of_t(&t).desc(true).clone());
    let reason_desc = match renders.variant {
        ast::types::RendersVariant::Normal => reason::VirtualReasonDesc::RRenderType(arg_desc),
        ast::types::RendersVariant::Maybe => reason::VirtualReasonDesc::RRenderMaybeType(arg_desc),
        ast::types::RendersVariant::Star => reason::VirtualReasonDesc::RRenderStarType(arg_desc),
    };
    let reason = reason::mk_reason(reason_desc.clone(), loc.dupe());
    match type_util::mk_possibly_generic_render_type(
        renders.variant.clone(),
        reason.dupe(),
        t.dupe(),
    ) {
        Some(t_prime) => {
            let node = flow_js::get_builtin_react_type_non_speculating(
                cx,
                &reason,
                None,
                intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactNodeType,
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::RenderTypeInstantiation {
                render_type: reason.dupe(),
            }));
            cx.add_post_inference_subtyping_check(t, use_op, node);
            (
                t_prime,
                ast::types::Renders {
                    operator_loc: renders.operator_loc.dupe(),
                    comments: renders.comments.dupe(),
                    argument: t_ast,
                    variant: renders.variant.clone(),
                },
            )
        }
        None => {
            let reason = reason::mk_reason(reason_desc, loc.dupe());
            let renders_variant = match renders.variant {
                ast::types::RendersVariant::Normal => type_::RendersVariant::RendersNormal,
                ast::types::RendersVariant::Maybe => type_::RendersVariant::RendersMaybe,
                ast::types::RendersVariant::Star => type_::RendersVariant::RendersStar,
            };
            fn concretize_fn<'cx>(cx_arg: &Context<'cx>, t_arg: &Type) -> Vec<Type> {
                let r = type_util::reason_of_t(t_arg).dupe();
                FlowJs::possible_concrete_types_for_inspection(cx_arg, &r, t_arg)
                    .expect("Should not be under speculation")
            }
            fn is_iterable_fn<'cx>(cx_arg: &Context<'cx>, t_arg: &Type) -> bool {
                let r = type_util::reason_of_t(t_arg).dupe();
                let error_r = type_::any_t::error(r.dupe());
                let iterable_t = FlowJs::get_builtin_typeapp(
                    cx_arg,
                    &r,
                    None,
                    "$Iterable",
                    vec![error_r.dupe(), error_r.dupe(), error_r],
                );
                FlowJs::speculative_subtyping_succeeds(cx_arg, t_arg, &iterable_t)
            }
            let concretize = concretize_fn;
            let is_iterable_for_better_error = is_iterable_fn;
            let renders_t = flow_js_utils::render_types::mk_non_generic_render_type(
                cx,
                reason,
                renders_variant,
                true,
                concretize,
                is_iterable_for_better_error,
                t,
            );
            (
                renders_t,
                ast::types::Renders {
                    operator_loc: renders.operator_loc.dupe(),
                    comments: renders.comments.dupe(),
                    argument: t_ast,
                    variant: renders.variant.clone(),
                },
            )
        }
    }
}

fn convert_object<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    loc: ALoc,
    exact: bool,
    properties: &[ast::types::object::Property<ALoc, ALoc>],
) -> (Type, Vec<ast::types::object::Property<ALoc, (ALoc, Type)>>) {
    use flow_parser::ast::types::object::Property;
    use flow_typing_type::type_::*;

    let obj_proto_t = obj_proto::make(reason::locationless_reason(
        reason::VirtualReasonDesc::RObjectPrototype,
    ));
    let fun_proto_t = Type::new(type_::TypeInner::FunProtoT(reason::locationless_reason(
        reason::VirtualReasonDesc::RFunctionPrototype,
    )));

    #[derive(Clone)]
    enum AccElement {
        Spread(Type),
        Slice {
            dict: Option<DictType>,
            pmap: type_::properties::PropertiesMap,
        },
    }

    #[derive(Clone)]
    struct Acc {
        dict: Option<DictType>,
        pmap: type_::properties::PropertiesMap,
        tail: Vec<AccElement>,
        proto: Option<Type>,
        calls: Vec<Type>,
    }

    impl Acc {
        fn empty() -> Self {
            Self {
                dict: None,
                pmap: type_::properties::PropertiesMap::new(),
                tail: Vec::new(),
                proto: None,
                calls: Vec::new(),
            }
        }

        fn head_slice(&self) -> Option<AccElement> {
            if self.dict.is_none() && self.pmap.is_empty() {
                None
            } else {
                Some(AccElement::Slice {
                    dict: self.dict.clone(),
                    pmap: self.pmap.dupe(),
                })
            }
        }

        fn add_call(&mut self, c: Type) -> Result<(), intermediate_error_types::UnsupportedSyntax> {
            if self.proto.is_some() {
                return Err(intermediate_error_types::UnsupportedSyntax::ExplicitCallAfterProto);
            }
            self.calls.push(c);
            Ok(())
        }

        fn add_dict(
            &mut self,
            d: DictType,
        ) -> Result<(), intermediate_error_types::UnsupportedSyntax> {
            if self.dict.is_some() {
                return Err(intermediate_error_types::UnsupportedSyntax::MultipleIndexers);
            }
            self.dict = Some(d);
            Ok(())
        }

        fn add_prop(&mut self, f: impl FnOnce(&mut type_::properties::PropertiesMap)) {
            f(&mut self.pmap);
        }

        fn add_proto(
            &mut self,
            p: Type,
        ) -> Result<(), intermediate_error_types::UnsupportedSyntax> {
            if self.proto.is_some() {
                return Err(intermediate_error_types::UnsupportedSyntax::MultipleProtos);
            }
            if !self.calls.is_empty() {
                return Err(intermediate_error_types::UnsupportedSyntax::ExplicitProtoAfterCall);
            }
            self.proto = Some(p);
            Ok(())
        }

        fn add_spread(&mut self, t: Type) {
            let mut tail = std::mem::take(&mut self.tail);
            if let Some(slice) = self.head_slice() {
                tail.push(slice);
            }
            tail.push(AccElement::Spread(t));
            self.dict = None;
            self.pmap = type_::properties::PropertiesMap::new();
            self.tail = tail;
        }

        fn elements_rev(self) -> (Vec<AccElement>, AccElement) {
            let head = self.head_slice();
            let mut tail = self.tail;
            tail.reverse();
            match head {
                Some(slice) => (tail, slice),
                None => match tail.len() {
                    0 => (
                        Vec::new(),
                        AccElement::Slice {
                            dict: None,
                            pmap: type_::properties::PropertiesMap::new(),
                        },
                    ),
                    _ => {
                        let first = tail.remove(0);
                        (tail, first)
                    }
                },
            }
        }

        fn get_proto(&self, obj_proto_t: &Type, fun_proto_t: &Type) -> Type {
            if let Some(ref t) = self.proto {
                t.dupe()
            } else if !self.calls.is_empty() {
                fun_proto_t.dupe()
            } else {
                obj_proto_t.dupe()
            }
        }
    }

    fn mk_object<'a>(
        cx: &Context<'a>,
        loc: ALoc,
        src_loc: bool,
        exact: bool,
        call: Option<Type>,
        dict: Option<DictType>,
        pmap: type_::properties::PropertiesMap,
        proto: Type,
    ) -> (Reason, ObjType) {
        let pmap_props = pmap;
        let pmap_id = if src_loc && type_env::in_toplevel_scope(cx) {
            cx.make_source_property_map(pmap_props, false, &loc)
        } else {
            cx.generate_property_map(pmap_props)
        };
        let call_id = call.map(|c| cx.make_call_prop(c));
        let obj_kind = match dict {
            Some(d) => ObjKind::Indexed(d),
            None => {
                if exact {
                    ObjKind::Exact
                } else {
                    ObjKind::Inexact
                }
            }
        };
        let flags = Flags {
            obj_kind,
            react_dro: None,
        };
        let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RObjectType, loc);
        let obj_t = mk_objecttype(Some(flags), None, call_id, pmap_id, proto);
        (reason, obj_t)
    }

    fn mk_object_annot<'a>(
        cx: &Context<'a>,
        loc: ALoc,
        exact: bool,
        call: Option<Type>,
        dict: &Option<DictType>,
        pmap: &type_::properties::PropertiesMap,
        proto: &Type,
    ) -> Type {
        let exact = exact && dict.is_none();
        let (reason_obj, obj_t) = mk_object(
            cx,
            loc.dupe(),
            true,
            exact,
            call,
            dict.clone(),
            pmap.clone(),
            proto.dupe(),
        );
        if exact {
            let reason_op = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RExactType(Arc::new(
                    reason::VirtualReasonDesc::RObjectType,
                )),
                loc,
            );
            type_util::make_exact_object(reason_obj, Rc::new(obj_t), &reason_op)
        } else {
            Type::new(type_::TypeInner::DefT(
                reason_obj,
                DefT::new(DefTInner::ObjT(Rc::new(obj_t))),
            ))
        }
    }

    fn named_property<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        acc: &mut Acc,
        prop: &ast::types::object::NormalProperty<ALoc, ALoc>,
    ) -> ast::types::object::NormalProperty<ALoc, (ALoc, Type)> {
        use flow_parser::ast::expression::object::Key;
        use flow_typing_errors::error_message::ETSSyntaxData;
        use flow_typing_errors::error_message::ErrorMessage;
        use flow_typing_type::type_::*;
        match &prop.value {
            ast::types::object::PropertyValue::Init(Some(value)) => {
                let key = &prop.key;
                let optional = prop.optional;
                let variance = &prop.variance;
                let method = prop.method;
                let abstract_ = prop.abstract_;
                if abstract_ && !cx.metadata().frozen.abstract_classes {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                            kind: flow_typing_errors::error_message::TSSyntaxKind::AbstractMethod,
                            loc: prop.loc.dupe(),
                        })),
                    );
                }
                if optional && method && !cx.tslib_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            prop.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::OptionalShorthandMethod,
                            ),
                        ))),
                    );
                }
                let prop_of_name = |cx,
                                    env: &mut ConvertEnv,
                                    acc: &mut Acc,
                                    key_loc: ALoc,
                                    name: FlowSmolStr| {
                    let value_ast = convert_inner(cx, env, value);
                    let t = value_ast.loc().1.dupe();
                    let make_prop_ast = |t: Type| {
                        let key_ast = match key {
                            Key::StringLiteral((_, lit)) => {
                                Key::StringLiteral(((key_loc.dupe(), t.dupe()), lit.clone()))
                            }
                            Key::NumberLiteral((_, lit)) => {
                                Key::NumberLiteral(((key_loc.dupe(), t.dupe()), lit.clone()))
                            }
                            Key::Identifier(ident) => {
                                Key::Identifier(ast::Identifier::new(ast::IdentifierInner {
                                    loc: (key_loc.dupe(), t.dupe()),
                                    name: name.dupe(),
                                    comments: ident.comments.dupe(),
                                }))
                            }
                            _ => panic!("branch invariant"),
                        };
                        ast::types::object::NormalProperty {
                            loc: prop.loc.dupe(),
                            key: key_ast,
                            value: ast::types::object::PropertyValue::Init(Some(value_ast.dupe())),
                            optional,
                            static_: prop.static_,
                            proto: prop.proto,
                            method,
                            abstract_: prop.abstract_,
                            override_: prop.override_,
                            variance: prop.variance.clone(),
                            ts_accessibility: prop.ts_accessibility.as_ref().map(|tsa| {
                                ast::class::ts_accessibility::TSAccessibility {
                                    loc: tsa.loc.dupe(),
                                    kind: tsa.kind,
                                    comments: tsa.comments.dupe(),
                                }
                            }),
                            init: None,
                            comments: prop.comments.dupe(),
                        }
                    };
                    if name.as_str() == "__proto__" && !(method || optional) && variance.is_none() {
                        let reason = reason::mk_reason(
                            reason::VirtualReasonDesc::RPrototype,
                            value.loc().dupe(),
                        );
                        let proto =
                            crate::type_annotation_cons_gen::obj_test_proto(cx, reason.dupe(), t);
                        let typeof_annot = type_util::typeof_annotation(reason, proto.dupe(), None);
                        if let Err(err) = acc.add_proto(typeof_annot) {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((key_loc.dupe(), err))),
                            );
                        }
                        make_prop_ast(proto)
                    } else {
                        let t = if optional {
                            type_util::optional(t, None, false)
                        } else {
                            t
                        };
                        let prop_name = Name::new(name.dupe());
                        if method {
                            let key_loc_clone = key_loc.dupe();
                            let t_clone = t.dupe();
                            acc.add_prop(move |pmap| {
                                pmap.insert(
                                    prop_name,
                                    type_::Property::new(type_::PropertyInner::Method {
                                        key_loc: Some(key_loc_clone),
                                        type_: t_clone,
                                    }),
                                );
                            });
                        } else {
                            let pol = polarity(cx, variance.as_ref());
                            let key_loc_clone = key_loc.dupe();
                            let t_clone = t.dupe();
                            acc.add_prop(move |pmap| {
                                pmap.insert(
                                    prop_name,
                                    type_::Property::new(type_::PropertyInner::Field(Box::new(
                                        FieldData {
                                            preferred_def_locs: None,
                                            key_loc: Some(key_loc_clone),
                                            type_: t_clone,
                                            polarity: pol,
                                        },
                                    ))),
                                );
                            });
                        }
                        make_prop_ast(t)
                    }
                };
                match key {
                    Key::StringLiteral((key_loc, lit)) => {
                        prop_of_name(cx, env, acc, key_loc.dupe(), lit.value.dupe())
                    }
                    Key::Identifier(ident) => {
                        prop_of_name(cx, env, acc, ident.loc.dupe(), ident.name.dupe())
                    }
                    Key::NumberLiteral((key_loc, lit)) => match variance {
                        Some(ast::Variance {
                            kind: ast::VarianceKind::Plus,
                            ..
                        })
                        | Some(ast::Variance {
                            kind: ast::VarianceKind::Minus,
                            ..
                        }) => {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EAmbiguousNumericKeyWithVariance(key_loc.dupe()),
                            );
                            {
                                let Ok(v) = polymorphic_ast_mapper::object_property_type(
                                    &mut typed_ast_utils::ErrorMapper,
                                    prop,
                                );
                                v
                            }
                        }
                        _ => {
                            if flow_common::js_number::is_float_safe_integer(lit.value) {
                                let name_str =
                                    flow_common::js_number::ecma_string_of_float(lit.value);
                                prop_of_name(
                                    cx,
                                    env,
                                    acc,
                                    key_loc.dupe(),
                                    FlowSmolStr::new(&name_str),
                                )
                            } else {
                                flow_js_utils::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedKeyInObject {
                                        loc: key_loc.dupe(),
                                        obj_kind: intermediate_error_types::ObjKind::Type,
                                        key_error_kind: InvalidObjKey::kind_of_num_value(lit.value),
                                    },
                                );
                                {
                                    let Ok(v) = polymorphic_ast_mapper::object_property_type(
                                        &mut typed_ast_utils::ErrorMapper,
                                        prop,
                                    );
                                    v
                                }
                            }
                        }
                    },
                    Key::BigIntLiteral((key_loc, _)) => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedKeyInObject {
                                loc: key_loc.dupe(),
                                obj_kind: intermediate_error_types::ObjKind::Type,
                                key_error_kind: InvalidObjKey::Other,
                            },
                        );
                        {
                            let Ok(v) = polymorphic_ast_mapper::object_property_type(
                                &mut typed_ast_utils::ErrorMapper,
                                prop,
                            );
                            v
                        }
                    }
                    Key::PrivateName(pn) => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedKeyInObject {
                                loc: pn.loc.dupe(),
                                obj_kind: intermediate_error_types::ObjKind::Type,
                                key_error_kind: InvalidObjKey::Other,
                            },
                        );
                        {
                            let Ok(v) = polymorphic_ast_mapper::object_property_type(
                                &mut typed_ast_utils::ErrorMapper,
                                prop,
                            );
                            v
                        }
                    }
                    Key::Computed(comp) => {
                        let (typed_expr, resolved_name) =
                            resolve_computed_key_name(cx, &comp.expression);
                        match resolved_name {
                            Some(name) => {
                                let value_ast = convert_inner(cx, env, value);
                                let t = value_ast.loc().1.dupe();
                                let t = if optional {
                                    type_util::optional(t, None, false)
                                } else {
                                    t
                                };
                                let id_loc = comp.expression.loc().dupe();
                                if method {
                                    let id_loc_clone = id_loc.dupe();
                                    let t_clone = t.dupe();
                                    acc.add_prop(move |pmap| {
                                        pmap.insert(
                                            name,
                                            type_::Property::new(type_::PropertyInner::Method {
                                                key_loc: Some(id_loc_clone),
                                                type_: t_clone,
                                            }),
                                        );
                                    });
                                } else {
                                    let pol = polarity(cx, variance.as_ref());
                                    let id_loc_clone = id_loc.dupe();
                                    let t_clone = t.dupe();
                                    acc.add_prop(move |pmap| {
                                        pmap.insert(
                                            name,
                                            type_::Property::new(type_::PropertyInner::Field(
                                                Box::new(FieldData {
                                                    preferred_def_locs: None,
                                                    key_loc: Some(id_loc_clone),
                                                    type_: t_clone,
                                                    polarity: pol,
                                                }),
                                            )),
                                        );
                                    });
                                }
                                ast::types::object::NormalProperty {
                                    loc: prop.loc.dupe(),
                                    key: Key::Computed(ast::ComputedKey {
                                        loc: comp.loc.dupe(),
                                        expression: typed_expr,
                                        comments: comp.comments.dupe(),
                                    }),
                                    value: ast::types::object::PropertyValue::Init(Some(value_ast)),
                                    optional,
                                    static_: false,
                                    proto: false,
                                    method,
                                    abstract_,
                                    override_: prop.override_,
                                    variance: prop.variance.clone(),
                                    ts_accessibility: None,
                                    init: None,
                                    comments: None,
                                }
                            }
                            None => {
                                flow_js_utils::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedKeyInObject {
                                        loc: comp.loc.dupe(),
                                        obj_kind: intermediate_error_types::ObjKind::Type,
                                        key_error_kind: InvalidObjKey::Other,
                                    },
                                );
                                let Ok(v) = polymorphic_ast_mapper::object_property_type(
                                    &mut typed_ast_utils::ErrorMapper,
                                    prop,
                                );
                                v
                            }
                        }
                    }
                }
            }
            // unsafe getter property
            ast::types::object::PropertyValue::Get(getter_loc, getter_fn) => match &prop.key {
                Key::Identifier(id_name) => {
                    let id_loc = &id_name.loc;
                    let name = &id_name.name;
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsafeGettersSetters(getter_loc.dupe()),
                    );
                    let (function_type, getter_ast) = mk_function_type_annotation_inner(
                        cx,
                        env,
                        &(getter_loc.dupe(), getter_fn.clone()),
                    );
                    let return_t = extract_getter_type(&function_type);
                    let prop_name = Name::new(name.dupe());
                    let id_loc_clone = id_loc.dupe();
                    let return_t_clone = return_t.dupe();
                    acc.add_prop(move |pmap| {
                        let new_prop = match pmap.get(&prop_name).map(|p| p.deref()) {
                            Some(type_::PropertyInner::Set {
                                key_loc: set_key_loc,
                                type_: set_type,
                            }) => type_::Property::new(type_::PropertyInner::GetSet(Box::new(
                                GetSetData {
                                    get_key_loc: Some(id_loc_clone),
                                    get_type: return_t_clone,
                                    set_key_loc: set_key_loc.dupe(),
                                    set_type: set_type.dupe(),
                                },
                            ))),
                            _ => type_::Property::new(type_::PropertyInner::Get {
                                key_loc: Some(id_loc_clone),
                                type_: return_t_clone,
                            }),
                        };
                        pmap.insert(prop_name, new_prop);
                    });
                    ast::types::object::NormalProperty {
                        loc: prop.loc.dupe(),
                        key: Key::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (id_loc.dupe(), return_t),
                            name: name.dupe(),
                            comments: id_name.comments.dupe(),
                        })),
                        value: ast::types::object::PropertyValue::Get(getter_ast.0, getter_ast.1),
                        optional: prop.optional,
                        static_: prop.static_,
                        proto: prop.proto,
                        method: prop.method,
                        abstract_: prop.abstract_,
                        override_: prop.override_,
                        variance: prop.variance.clone(),
                        ts_accessibility: prop.ts_accessibility.as_ref().map(|tsa| {
                            ast::class::ts_accessibility::TSAccessibility {
                                loc: tsa.loc.dupe(),
                                kind: tsa.kind,
                                comments: tsa.comments.dupe(),
                            }
                        }),
                        init: None,
                        comments: prop.comments.dupe(),
                    }
                }
                _ => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            prop.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::ObjectPropertyGetSet,
                        ))),
                    );
                    {
                        let Ok(v) = polymorphic_ast_mapper::object_property_type(
                            &mut typed_ast_utils::ErrorMapper,
                            prop,
                        );
                        v
                    }
                }
            },
            // unsafe setter property
            ast::types::object::PropertyValue::Set(setter_loc, setter_fn) => match &prop.key {
                Key::Identifier(id_name) => {
                    let id_loc = &id_name.loc;
                    let name = &id_name.name;
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsafeGettersSetters(setter_loc.dupe()),
                    );
                    let (function_type, setter_ast) = mk_function_type_annotation_inner(
                        cx,
                        env,
                        &(setter_loc.dupe(), setter_fn.clone()),
                    );
                    let param_t = extract_setter_type(&function_type);
                    let prop_name = Name::new(name.dupe());
                    let id_loc_clone = id_loc.dupe();
                    let param_t_clone = param_t.dupe();
                    acc.add_prop(move |pmap| {
                        let new_prop = match pmap.get(&prop_name).map(|p| p.deref()) {
                            Some(type_::PropertyInner::Get {
                                key_loc: get_key_loc,
                                type_: get_type,
                            }) => type_::Property::new(type_::PropertyInner::GetSet(Box::new(
                                GetSetData {
                                    get_key_loc: get_key_loc.dupe(),
                                    get_type: get_type.dupe(),
                                    set_key_loc: Some(id_loc_clone),
                                    set_type: param_t_clone,
                                },
                            ))),
                            _ => type_::Property::new(type_::PropertyInner::Set {
                                key_loc: Some(id_loc_clone),
                                type_: param_t_clone,
                            }),
                        };
                        pmap.insert(prop_name, new_prop);
                    });
                    ast::types::object::NormalProperty {
                        loc: prop.loc.dupe(),
                        key: Key::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (id_loc.dupe(), param_t),
                            name: name.dupe(),
                            comments: id_name.comments.dupe(),
                        })),
                        value: ast::types::object::PropertyValue::Set(setter_ast.0, setter_ast.1),
                        optional: prop.optional,
                        static_: prop.static_,
                        proto: prop.proto,
                        method: prop.method,
                        abstract_: prop.abstract_,
                        override_: prop.override_,
                        variance: prop.variance.clone(),
                        ts_accessibility: prop.ts_accessibility.as_ref().map(|tsa| {
                            ast::class::ts_accessibility::TSAccessibility {
                                loc: tsa.loc.dupe(),
                                kind: tsa.kind,
                                comments: tsa.comments.dupe(),
                            }
                        }),
                        init: None,
                        comments: prop.comments.dupe(),
                    }
                }
                _ => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            prop.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::ObjectPropertyGetSet,
                        ))),
                    );
                    {
                        let Ok(v) = polymorphic_ast_mapper::object_property_type(
                            &mut typed_ast_utils::ErrorMapper,
                            prop,
                        );
                        v
                    }
                }
            },
            ast::types::object::PropertyValue::Init(None) => {
                let Ok(v) = polymorphic_ast_mapper::object_property_type(
                    &mut typed_ast_utils::ErrorMapper,
                    prop,
                );
                v
            }
        }
    }

    // ========================================
    // Process properties
    // ========================================
    let mut acc = Acc::empty();
    let mut prop_asts: Vec<Property<ALoc, (ALoc, Type)>> = Vec::new();

    for prop in properties {
        match prop {
            Property::CallProperty(cp) => {
                let cp_loc = &cp.value.0;
                let (t, (_, fn_ast)) = mk_function_type_annotation_inner(
                    cx,
                    env,
                    &(cp_loc.dupe(), cp.value.1.clone()),
                );
                let call_ast = ast::types::object::CallProperty {
                    loc: cp.loc.dupe(),
                    value: (cp.value.0.dupe(), fn_ast),
                    static_: cp.static_,
                    comments: cp.comments.dupe(),
                };
                if let Err(err) = acc.add_call(t) {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((cp_loc.dupe(), err))),
                    );
                }
                prop_asts.push(Property::CallProperty(call_ast));
            }
            Property::Indexer(indexer) => {
                if indexer.optional && !cx.tslib_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            indexer.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                intermediate_error_types::TsLibSyntaxKind::OptionalIndexer,
                            ),
                        ))),
                    );
                    let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    prop_asts.push(error_prop);
                } else {
                    let key_ast = convert_inner(cx, env, &indexer.key);
                    let key_t = key_ast.loc().1.dupe();
                    let value_ast = convert_inner(cx, env, &indexer.value);
                    let annot_loc = value_ast.loc().0.dupe();
                    let value_t = value_ast.loc().1.dupe();
                    let value_t = if indexer.optional {
                        type_util::optional(value_t, Some(annot_loc), false)
                    } else {
                        value_t
                    };
                    let d = DictType {
                        dict_name: indexer.id.as_ref().map(ident_name),
                        key: key_t,
                        value: value_t,
                        dict_polarity: polarity(cx, indexer.variance.as_ref()),
                    };
                    let indexer_ast = ast::types::object::Indexer {
                        loc: indexer.loc.dupe(),
                        id: indexer.id.as_ref().map(|id| {
                            let Ok(v) = polymorphic_ast_mapper::t_identifier(
                                &mut typed_ast_utils::ErrorMapper,
                                id,
                            );
                            v
                        }),
                        key: key_ast,
                        value: value_ast,
                        static_: indexer.static_,
                        variance: indexer.variance.clone(),
                        optional: indexer.optional,
                        comments: indexer.comments.dupe(),
                    };
                    if let Err(err) = acc.add_dict(d) {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((indexer.loc.dupe(), err))),
                        );
                    }
                    prop_asts.push(Property::Indexer(indexer_ast));
                }
            }
            Property::NormalProperty(obj_prop) => {
                let prop_ast = named_property(cx, env, &mut acc, obj_prop);
                prop_asts.push(Property::NormalProperty(prop_ast));
            }
            Property::InternalSlot(slot) => {
                let slot_name = &slot.id.name;
                if slot_name.as_str() == "call" {
                    let value_ast = convert_inner(cx, env, &slot.value);
                    let mut t = value_ast.loc().1.dupe();
                    if slot.optional {
                        t = type_util::optional(t, None, false);
                    }
                    if let Err(err) = acc.add_call(t) {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((slot.loc.dupe(), err))),
                        );
                    }
                    let slot_ast = ast::types::object::InternalSlot {
                        loc: slot.loc.dupe(),
                        id: ast::Identifier::new(ast::IdentifierInner {
                            loc: slot.id.loc.dupe(),
                            name: slot.id.name.dupe(),
                            comments: slot.id.comments.dupe(),
                        }),
                        value: value_ast,
                        optional: slot.optional,
                        static_: slot.static_,
                        method: slot.method,
                        comments: slot.comments.dupe(),
                    };
                    prop_asts.push(Property::InternalSlot(slot_ast));
                } else {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            slot.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::UnsupportedInternalSlot {
                                name: slot_name.dupe(),
                                static_: false,
                            },
                        ))),
                    );
                    let Ok(mapped) = polymorphic_ast_mapper::object_type_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    prop_asts.push(mapped);
                }
            }
            Property::SpreadProperty(sp) => {
                let argument_ast = convert_inner(cx, env, &sp.argument);
                let t = argument_ast.loc().1.dupe();
                acc.add_spread(t);
                prop_asts.push(Property::SpreadProperty(
                    ast::types::object::SpreadProperty {
                        loc: sp.loc.dupe(),
                        argument: argument_ast,
                        comments: sp.comments.dupe(),
                    },
                ));
            }
            Property::MappedType(_) => {
                panic!("Unreachable until we support mapped types with additional properties");
            }
            Property::PrivateField(_) => {
                panic!("Unreachable: private fields only appear in declare class bodies");
            }
        }
    }

    // ========================================
    // Final assembly
    // ========================================
    let proto = acc.get_proto(&obj_proto_t, &fun_proto_t);
    let calls: Vec<Type> = acc.calls.clone();

    let (tail, head) = acc.elements_rev();
    let t = match (&head, tail.len()) {
        (AccElement::Slice { dict, pmap }, 0) => {
            let ts: Vec<Type> = calls
                .into_iter()
                .map(|call| mk_object_annot(cx, loc.dupe(), exact, Some(call), dict, pmap, &proto))
                .collect();
            match ts.len() {
                0 => mk_object_annot(cx, loc.dupe(), exact, None, dict, pmap, &proto),
                1 => ts.into_iter().next().unwrap(),
                _ => {
                    let mut iter = ts.into_iter();
                    let t0 = iter.next().unwrap();
                    let t1 = iter.next().unwrap();
                    let rest: Vec<Type> = iter.collect();
                    let callable_reason = reason::mk_annot_reason(
                        reason::VirtualReasonDesc::RCallableObjectType,
                        loc.dupe(),
                    );
                    let rep = type_::inter_rep::make(t0, t1, rest.into());
                    Type::new(type_::TypeInner::IntersectionT(callable_reason, rep))
                }
            }
        }
        _ => {
            let reason = reason::mk_reason(reason::VirtualReasonDesc::RObjectType, loc.dupe());
            let target = object::spread::Target::Annot { make_exact: exact };

            fn acc_element_to_operand(el: &AccElement, reason: &Reason) -> object::spread::Operand {
                match el {
                    AccElement::Spread(t) => object::spread::Operand::Type(t.dupe()),
                    AccElement::Slice { dict, pmap } => object::spread::Operand::Slice(
                        object::spread::OperandSlice::new(object::spread::OperandSliceInner {
                            reason: reason.dupe(),
                            prop_map: pmap.dupe(),
                            dict: dict.clone(),
                            generics: flow_typing_generics::spread_empty(),
                            reachable_targs: Rc::from([]),
                        }),
                    ),
                }
            }

            match (&head, &tail[..]) {
                (AccElement::Spread(t), ts) => {
                    let operands: Vec<object::spread::Operand> = ts
                        .iter()
                        .map(|el| acc_element_to_operand(el, &reason))
                        .collect();
                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                        cx,
                        type_::unknown_use(),
                        &reason,
                        &t.dupe(),
                        &Destructor::SpreadType(Box::new(DestructorSpreadTypeData(
                            target,
                            operands.into(),
                            None,
                        ))),
                        eval::Id::generate_id(),
                    )
                    .expect("mk_type_destructor should not fail")
                }
                (
                    AccElement::Slice {
                        dict: head_dict,
                        pmap: head_pmap,
                    },
                    rest,
                ) if !rest.is_empty()
                    && let AccElement::Spread(spread_t_ref) = &rest[0] =>
                {
                    let spread_t = spread_t_ref.dupe();
                    let head_slice_val =
                        object::spread::OperandSlice::new(object::spread::OperandSliceInner {
                            reason: reason.dupe(),
                            prop_map: head_pmap.dupe(),
                            dict: head_dict.clone(),
                            generics: flow_typing_generics::spread_empty(),
                            reachable_targs: Rc::from([]),
                        });
                    let operands: Vec<object::spread::Operand> = rest[1..]
                        .iter()
                        .map(|el| acc_element_to_operand(el, &reason))
                        .collect();
                    FlowJs::mk_possibly_evaluated_destructor_for_annotations(
                        cx,
                        type_::unknown_use(),
                        &reason,
                        &spread_t,
                        &Destructor::SpreadType(Box::new(DestructorSpreadTypeData(
                            target,
                            operands.into(),
                            Some(head_slice_val),
                        ))),
                        eval::Id::generate_id(),
                    )
                    .expect("mk_type_destructor should not fail")
                }
                _ => {
                    panic!("Invariant Violation: spread list has two slices in a row");
                }
            }
        }
    };

    (t, prop_asts)
}

fn convert_tuple_element<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    element: &ast::types::tuple::Element<ALoc, ALoc>,
) -> (
    type_::UnresolvedParam,
    ast::types::tuple::Element<ALoc, (ALoc, Type)>,
) {
    use flow_parser::ast::types::tuple::Element;
    let loc = element.loc().dupe();
    match element {
        Element::UnlabeledElement {
            annot, optional, ..
        } => {
            if *optional && !cx.tslib_syntax() {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                            TsLibSyntaxKind::OptionalUnlabeledTupleElement,
                        ),
                    ))),
                );
                // let element_ast = Tast_utils.error_mapper#tuple_element (loc, el) in
                let Ok(element_ast) = polymorphic_ast_mapper::tuple_element(
                    &mut typed_ast_utils::ErrorMapper,
                    element,
                );
                let reason = reason::mk_reason(
                    reason::VirtualReasonDesc::RTupleElement { name: None },
                    loc.dupe(),
                );
                let te = type_util::mk_tuple_element(
                    reason,
                    type_::any_t::at(type_::AnySource::AnyError(None), loc),
                    None,
                    *optional,
                    flow_common::polarity::Polarity::Neutral,
                );
                (
                    type_::UnresolvedParam::UnresolvedArg(Box::new(type_::UnresolvedArgData(
                        te, None,
                    ))),
                    element_ast,
                )
            } else {
                let annot_ast = convert_inner(cx, env, annot);
                let (_, annot_t) = annot_ast.loc();
                let annot_t = annot_t.dupe();
                let t = if *optional {
                    type_util::optional(annot_t, None, false)
                } else {
                    annot_t
                };
                let element_ast = Element::UnlabeledElement {
                    loc: loc.dupe(),
                    annot: annot_ast,
                    optional: *optional,
                };
                let reason =
                    reason::mk_reason(reason::VirtualReasonDesc::RTupleElement { name: None }, loc);
                let te = type_util::mk_tuple_element(
                    reason,
                    t,
                    None,
                    *optional,
                    flow_common::polarity::Polarity::Neutral,
                );
                (
                    type_::UnresolvedParam::UnresolvedArg(Box::new(type_::UnresolvedArgData(
                        te, None,
                    ))),
                    element_ast,
                )
            }
        }
        Element::LabeledElement {
            element:
                ast::types::tuple::LabeledElement {
                    name: label_name,
                    annot,
                    variance,
                    optional,
                },
            ..
        } => {
            let annot_ast = convert_inner(cx, env, annot);
            let (_, annot_t) = annot_ast.loc();
            let annot_t = annot_t.dupe();
            let t = if *optional {
                type_util::optional(annot_t.dupe(), None, false)
            } else {
                annot_t.dupe()
            };
            let name_loc = label_name.loc.dupe();
            let str_name = &label_name.name;
            let id_name = ast::Identifier::new(ast::IdentifierInner {
                loc: (name_loc, t.dupe()),
                name: str_name.dupe(),
                comments: label_name.comments.dupe(),
            });
            let element_ast = Element::LabeledElement {
                loc: loc.dupe(),
                element: ast::types::tuple::LabeledElement {
                    name: id_name,
                    annot: annot_ast,
                    variance: variance.clone(),
                    optional: *optional,
                },
            };
            let name_opt = Some(str_name.dupe());
            let reason = reason::mk_reason(
                reason::VirtualReasonDesc::RTupleElement {
                    name: name_opt.dupe(),
                },
                loc,
            );
            let te = type_::TupleElement {
                name: name_opt,
                t,
                polarity: polarity(cx, variance.as_ref()),
                optional: *optional,
                reason,
            };
            (
                type_::UnresolvedParam::UnresolvedArg(Box::new(type_::UnresolvedArgData(te, None))),
                element_ast,
            )
        }
        Element::SpreadElement {
            element:
                ast::types::tuple::SpreadElement {
                    name: spread_name,
                    annot,
                },
            ..
        } => {
            let annot_ast = convert_inner(cx, env, annot);
            let (_, t) = annot_ast.loc();
            let t = t.dupe();
            let name_typed: Option<ast::Identifier<ALoc, (ALoc, Type)>> =
                spread_name.as_ref().map(|n| {
                    ast::Identifier::new(ast::IdentifierInner {
                        loc: (n.loc.dupe(), t.dupe()),
                        name: n.name.dupe(),
                        comments: n.comments.dupe(),
                    })
                });
            let element_ast = Element::SpreadElement {
                loc: loc.dupe(),
                element: ast::types::tuple::SpreadElement {
                    name: name_typed,
                    annot: annot_ast,
                },
            };
            (type_::UnresolvedParam::UnresolvedSpreadArg(t), element_ast)
        }
    }
}

fn check_guard_type<'a>(
    cx: &Context<'a>,
    fparams: &[type_::FunParam],
    guard_name: &FlowSmolStr,
    guard_t: &Type,
) {
    let param_t = fparams
        .iter()
        .find_map(|type_::FunParam(name, t)| match name {
            Some(n) if n == guard_name => Some(t),
            _ => None,
        });
    if let Some(param_t) = param_t {
        let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeGuardIncompatibility {
            guard_type: type_util::reason_of_t(guard_t).dupe(),
            param_name: guard_name.dupe(),
        }));
        cx.add_post_inference_subtyping_check(guard_t.dupe(), use_op, param_t.dupe());
    }
}

fn convert_type_guard_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    fparams: &[type_::FunParam],
    gloc: ALoc,
    kind: ast::types::TypeGuardKind,
    id_name: &ast::Identifier<ALoc, ALoc>,
    t: &ast::types::Type<ALoc, ALoc>,
    comments: Option<&ast::Syntax<ALoc, std::sync::Arc<[ast::Comment<ALoc>]>>>,
) -> (
    Type,
    ast::types::TypeGuard<ALoc, (ALoc, Type)>,
    Option<type_::TypeGuard>,
) {
    let name_loc = id_name.loc.dupe();
    let name = &id_name.name;
    let t_prime = convert_inner(cx, env, t);
    let (_, type_guard_t) = t_prime.loc();
    let type_guard_t = type_guard_t.dupe();
    let bool_t = type_::bool_module_t::at(gloc.dupe());
    let id_name_typed: ast::Identifier<ALoc, ALoc> = ast::Identifier::new(ast::IdentifierInner {
        loc: id_name.loc.dupe(),
        name: id_name.name.dupe(),
        comments: id_name.comments.dupe(),
    });
    let guard_prime = ast::types::TypeGuard {
        loc: gloc.dupe(),
        kind,
        guard: (id_name_typed, Some(t_prime)),
        comments: comments.cloned(),
    };
    let one_sided = kind == ast::types::TypeGuardKind::Implies;
    let reason = reason::mk_reason(reason::VirtualReasonDesc::RTypeGuard, gloc);
    check_guard_type(cx, fparams, name, &type_guard_t);
    let type_guard = Some(type_::TypeGuard::new(type_::TypeGuardInner {
        reason,
        one_sided,
        inferred: false,
        param_name: (name_loc, name.dupe()),
        type_guard: type_guard_t,
    }));
    (bool_t, guard_prime, type_guard)
}

fn error_type_guard<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    loc: ALoc,
    x: &ast::Identifier<ALoc, ALoc>,
    t: &ast::types::Type<ALoc, ALoc>,
    kind: ast::types::TypeGuardKind,
    comments: Option<&ast::Syntax<ALoc, std::sync::Arc<[ast::Comment<ALoc>]>>>,
    msg: ErrorMessage<ALoc>,
) -> (
    Type,
    ast::types::TypeGuard<ALoc, (ALoc, Type)>,
    Option<type_::TypeGuard>,
) {
    flow_js_utils::add_output_non_speculating(cx, msg);
    let t_ast = convert_inner(cx, env, t);
    let x_typed = ast::Identifier::new(ast::IdentifierInner {
        loc: x.loc.dupe(),
        name: x.name.dupe(),
        comments: x.comments.dupe(),
    });
    let guard_prime = ast::types::TypeGuard {
        loc: loc.dupe(),
        kind,
        guard: (x_typed, Some(t_ast)),
        comments: comments.cloned(),
    };
    let bool_t = type_::bool_module_t::at(loc);
    (bool_t, guard_prime, None)
}

fn convert_return_annotation<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    meth_kind: MethodKind,
    params: &ast::types::function::Params<ALoc, ALoc>,
    fparams: &[type_::FunParam],
    return_annot: &ast::types::function::ReturnAnnotation<ALoc, ALoc>,
) -> (
    Type,
    ast::types::function::ReturnAnnotation<ALoc, (ALoc, Type)>,
    Option<type_::TypeGuard>,
) {
    use flow_parser::ast::types::function::ReturnAnnotation;
    match return_annot {
        ReturnAnnotation::Available(t_ast) => {
            let t_ast_prime = convert_inner(cx, env, &t_ast.annotation);
            let (_, t_prime) = t_ast_prime.loc();
            let t_prime = t_prime.dupe();
            (
                t_prime,
                ReturnAnnotation::Available(ast::types::Annotation {
                    loc: t_ast.loc.dupe(),
                    annotation: t_ast_prime,
                }),
                None,
            )
        }
        ReturnAnnotation::TypeGuard(guard)
            if guard.guard.0.name.as_str() == "this" && guard.guard.1.is_some() =>
        {
            let x = &guard.guard.0;
            let t = guard.guard.1.as_ref().unwrap();
            let gloc = guard.loc.dupe();
            let kind = guard.kind;
            let comments = guard.comments.as_ref();
            if allows_this_type_guards(meth_kind) {
                let (bool_t, guard_prime, predicate) =
                    convert_type_guard_inner(cx, env, fparams, gloc, kind, x, t, comments);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            } else {
                let name_loc = x.loc.dupe();
                let msg = ErrorMessage::ETypeGuardThisParam(reason::mk_reason(
                    reason::VirtualReasonDesc::RThis,
                    name_loc,
                ));
                let (bool_t, guard_prime, predicate) =
                    error_type_guard(cx, env, gloc, x, t, kind, comments, msg);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            }
        }
        ReturnAnnotation::TypeGuard(guard) if guard.guard.1.is_some() => {
            let x = &guard.guard.0;
            let t = guard.guard.1.as_ref().unwrap();
            let gloc = guard.loc.dupe();
            let kind = guard.kind;
            let comments = guard.comments.as_ref();
            let name_loc = x.loc.dupe();
            let name = &x.name;
            // Check that type guard variable is not a rest param
            let is_rest_param_conflict = params.rest.as_ref().and_then(|rest| {
                let arg = &rest.argument;
                match &arg.param {
                    ast::types::function::ParamKind::Labeled {
                        name: param_name, ..
                    } => {
                        if param_name.name.as_str() == name.as_str() {
                            Some(param_name.loc.dupe())
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            });
            if let Some(rloc) = is_rest_param_conflict {
                let msg = ErrorMessage::ETypeGuardInvalidParameter(Box::new(
                    ETypeGuardInvalidParameterData {
                        type_guard_reason: reason::mk_reason(
                            reason::VirtualReasonDesc::RTypeGuardParam(name.dupe()),
                            name_loc,
                        ),
                        binding_reason: reason::mk_reason(
                            reason::VirtualReasonDesc::RRestParameter(Some(name.dupe())),
                            rloc,
                        ),
                    },
                ));
                let (bool_t, guard_prime, predicate) =
                    error_type_guard(cx, env, gloc, x, t, kind, comments, msg);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            } else if !allows_type_guards(meth_kind) {
                // Check that type guard variable appears in parameter list
                let msg = ErrorMessage::ETypeGuardIncompatibleWithFunctionKind(Box::new(
                    ETypeGuardIncompatibleWithFunctionKindData {
                        loc: gloc.dupe(),
                        kind: FlowSmolStr::new(method_kind_to_string(meth_kind)),
                    },
                ));
                let (bool_t, guard_prime, predicate) =
                    error_type_guard(cx, env, gloc, x, t, kind, comments, msg);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            } else if params.params.iter().all(|p| match &p.param {
                ast::types::function::ParamKind::Labeled { name: pname, .. } => {
                    pname.name.as_str() != name.as_str()
                }
                _ => true,
            }) {
                let msg = ErrorMessage::ETypeGuardParamUnbound(reason::mk_reason(
                    reason::VirtualReasonDesc::RTypeGuardParam(name.dupe()),
                    name_loc,
                ));
                let (bool_t, guard_prime, predicate) =
                    error_type_guard(cx, env, gloc, x, t, kind, comments, msg);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            } else {
                let (bool_t, guard_prime, predicate) =
                    convert_type_guard_inner(cx, env, fparams, gloc, kind, x, t, comments);
                (bool_t, ReturnAnnotation::TypeGuard(guard_prime), predicate)
            }
        }
        ReturnAnnotation::TypeGuard(guard) => {
            let loc = guard.loc.dupe();
            let kind = guard.kind;
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::EUnsupportedSyntax(Box::new((
                    loc.dupe(),
                    intermediate_error_types::UnsupportedSyntax::UserDefinedTypeGuards { kind },
                ))),
            );
            let any_t = type_::any_t::at(type_::AnySource::AnyError(None), loc);
            let Ok(mapped_ret) = polymorphic_ast_mapper::function_type_return_annotation(
                &mut typed_ast_utils::ErrorMapper,
                return_annot,
            );
            (any_t, mapped_ret, None)
        }
        ReturnAnnotation::Missing(loc) => match meth_kind {
            MethodKind::ConstructorKind => {
                if !cx.tslib_syntax() {
                    flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                loc.dupe(),
                                intermediate_error_types::UnsupportedSyntax::DeclareClassMethodMissingReturnType,
                            ))),
                        );
                }
                let void_t = type_::void::at(loc.dupe());
                (void_t, ReturnAnnotation::Missing(loc.dupe()), None)
            }
            _ => {
                flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::DeclareClassMethodMissingReturnType,
                        ))),
                    );
                let any_t = type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe());
                (any_t, ReturnAnnotation::Missing(loc.dupe()), None)
            }
        },
    }
}

/// Build typed AST for a function type parameter after type-checking.
fn typed_function_param_ast(
    param: &ast::types::function::ParamKind<ALoc, ALoc>,
    t: Type,
    annot_ast: ast::types::Type<ALoc, (ALoc, Type)>,
) -> ast::types::function::ParamKind<ALoc, (ALoc, Type)> {
    match param {
        ast::types::function::ParamKind::Labeled { name, optional, .. } => {
            let typed_name = ast::Identifier::new(ast::IdentifierInner {
                loc: (name.loc.dupe(), t),
                name: name.name.dupe(),
                comments: name.comments.dupe(),
            });
            ast::types::function::ParamKind::Labeled {
                name: typed_name,
                annot: annot_ast,
                optional: *optional,
            }
        }
        ast::types::function::ParamKind::Anonymous(_) => {
            ast::types::function::ParamKind::Anonymous(annot_ast)
        }
        ast::types::function::ParamKind::Destructuring(pattern) => {
            // Destructuring patterns in function type params are not runtime bindings —
            // they exist purely for documentation in .d.ts files. Map them with
            // unimplemented_mapper to assign placeholder types to the pattern nodes.
            let Ok(mapped) = polymorphic_ast_mapper::pattern(
                &mut typed_ast_utils::UnimplementedMapper,
                None,
                pattern,
            );
            ast::types::function::ParamKind::Destructuring(mapped)
        }
    }
}

fn mk_method_func_sig<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    meth_kind: MethodKind,
    loc: ALoc,
    func: &ast::types::Function<ALoc, ALoc>,
) -> (
    func_class_sig_types::func::Func<FuncTypeParamsConfig>,
    ast::types::Function<ALoc, (ALoc, Type)>,
) {
    fn add_param<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        x: &mut func_class_sig_types::param::Param<FuncTypeParamsConfig>,
        param: &ast::types::function::Param<ALoc, ALoc>,
    ) {
        let param_loc = param.loc.dupe();
        let (_name, annot, _optional) =
            flow_parser::ast_utils::function_type_param_parts(&param.param);
        let annot_ast = convert_inner(cx, env, annot);
        let (_, t) = annot_ast.loc();
        let t = t.dupe();
        let param_ast = typed_function_param_ast(&param.param, t.dupe(), annot_ast);
        let typed_param = ast::types::function::Param {
            loc: param_loc,
            param: param_ast,
        };
        crate::func_params::add_param::<FuncTypeParamsConfig>((t, typed_param), x);
    }

    fn add_rest<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        x: &mut func_class_sig_types::param::Param<FuncTypeParamsConfig>,
        rest_param: &ast::types::function::RestParam<ALoc, ALoc>,
    ) {
        let rest_loc = rest_param.loc.dupe();
        let argument = &rest_param.argument;
        let arg_loc = argument.loc.dupe();
        let (_name, annot, _optional) =
            flow_parser::ast_utils::function_type_param_parts(&argument.param);
        let annot_ast = convert_inner(cx, env, annot);
        let (_, t) = annot_ast.loc();
        let t = t.dupe();
        let rest_param_ast_inner = typed_function_param_ast(&argument.param, t.dupe(), annot_ast);
        let typed_rest = ast::types::function::RestParam {
            loc: rest_loc,
            argument: ast::types::function::Param {
                loc: arg_loc,
                param: rest_param_ast_inner,
            },
            comments: rest_param.comments.dupe(),
        };
        crate::func_params::add_rest::<FuncTypeParamsConfig>((t, typed_rest), x);
    }

    fn add_this<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        x: &mut func_class_sig_types::param::Param<FuncTypeParamsConfig>,
        this_param: &ast::types::function::ThisParam<ALoc, ALoc>,
    ) {
        let this_loc = this_param.loc.dupe();
        let annot = &this_param.annot;
        let annot_ast = convert_inner(cx, env, &annot.annotation);
        let (_, t) = annot_ast.loc();
        let t = t.dupe();
        let typed_this = ast::types::function::ThisParam {
            loc: this_loc,
            annot: ast::types::Annotation {
                loc: annot.loc.dupe(),
                annotation: annot_ast,
            },
            comments: this_param.comments.dupe(),
        };
        crate::func_params::add_this::<FuncTypeParamsConfig>((t, typed_this), x);
    }

    fn convert_params<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        params_node: &ast::types::function::Params<ALoc, ALoc>,
    ) -> (
        func_class_sig_types::param::Param<FuncTypeParamsConfig>,
        ast::types::function::Params<ALoc, (ALoc, Type)>,
    ) {
        let params_loc = params_node.loc.dupe();
        let comments = params_node.comments.dupe();
        let mut fparams = crate::func_params::empty::<FuncTypeParamsConfig>(Rc::new(
            move |params, rest, this_| {
                Some(ast::types::function::Params {
                    loc: params_loc.dupe(),
                    params: Arc::from(params),
                    rest,
                    this: this_,
                    comments: comments.clone(),
                })
            },
        ));
        for param in params_node.params.iter() {
            add_param(cx, env, &mut fparams, param);
        }
        if let Some(rest) = &params_node.rest {
            add_rest(cx, env, &mut fparams, rest);
        }
        if let Some(this) = &params_node.this {
            add_this(cx, env, &mut fparams, this);
        }
        let params_ast = crate::func_params::eval::<FuncTypeParamsConfig, _>(
            cx,
            &fparams.params,
            fparams.rest.as_ref(),
            fparams.this_.as_ref(),
            &*fparams.reconstruct,
        )
        .expect("eval should not fail for type annotation params");
        (
            fparams,
            params_ast.expect("reconstruct always returns Some"),
        )
    }

    let saved_tparams_map = env.tparams_map.dupe();
    let (tparams, tparams_ast) = mk_type_param_declarations_inner(
        cx,
        env,
        flow_parser::ast_visitor::TypeParamsContext::FunctionType,
        func.tparams.as_ref(),
    );
    let (fparams, params_ast) = convert_params(cx, env, &func.params);
    let fparams_value = crate::func_params::value::<FuncTypeParamsConfig>(&fparams.params);
    let (return_t, return_ast, type_guard) = convert_return_annotation(
        cx,
        env,
        meth_kind,
        &func.params,
        &fparams_value,
        &func.return_,
    );
    env.tparams_map = saved_tparams_map;
    let kind = match type_guard {
        None => func_class_sig_types::func::Kind::Ordinary,
        Some(g) => func_class_sig_types::func::Kind::TypeGuard(g),
    };
    let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RFunctionType, loc.dupe());
    // Methods can't be hooks
    let effect_flag = type_::ReactEffectType::ArbitraryEffect;
    let ret_annot_loc = type_util::loc_of_t(&return_t).dupe();
    let func_sig = func_class_sig_types::func::Func {
        reason,
        kind,
        tparams,
        fparams,
        body: None,
        return_t: type_::AnnotatedOrInferred::Annotated(return_t),
        ret_annot_loc,
        statics: None,
        effect_: effect_flag,
    };
    let func_ast = ast::types::Function {
        tparams: tparams_ast,
        params: params_ast,
        return_: return_ast,
        effect: func.effect.clone(),
        comments: None,
    };
    (func_sig, func_ast)
}

fn mk_type_available_annotation_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    annotation: &ast::types::Annotation<ALoc, ALoc>,
) -> (Type, ast::types::Annotation<ALoc, (ALoc, Type)>) {
    let node_cache = cx.node_cache();
    let loc = &annotation.loc;
    let annot = &annotation.annotation;
    let is_tparam_generic = match annot.deref() {
        ast::types::TypeInner::Generic { inner, .. } => match &inner.id {
            ast::types::generic::Identifier::Unqualified(id) => env
                .tparams_map
                .contains_key(&SubstName::name(id.name.dupe())),
            _ => false,
        },
        _ => false,
    };
    let annot_ast = if is_tparam_generic {
        // If the type we're converting is in the tparams map, we prefer that over
        // the node cache
        convert_inner(cx, env, annot)
    } else {
        match node_cache.get_annotation(loc) {
            Some(cached) => {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    use flow_parser::loc_sig::LocSig;
                    vec![format!(
                        "Annotation cache hit at {}",
                        loc.debug_to_string(true)
                    )]
                });
                // node
                cached.annotation
            }
            None => convert_inner(cx, env, annot),
        }
    };
    let (_, t) = annot_ast.loc();
    let t = t.dupe();
    (
        t,
        ast::types::Annotation {
            loc: loc.dupe(),
            annotation: annot_ast,
        },
    )
}

fn mk_function_type_annotation_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    func: &(ALoc, ast::types::Function<ALoc, ALoc>),
) -> (Type, (ALoc, ast::types::Function<ALoc, (ALoc, Type)>)) {
    let (ref loc, ref f) = *func;
    let ast_type = ast::types::Type::new(ast::types::TypeInner::Function {
        loc: loc.dupe(),
        inner: std::sync::Arc::new(f.clone()),
    });
    let result = convert_inner(cx, env, &ast_type);
    match result.deref() {
        ast::types::TypeInner::Function {
            loc: (_, function_type),
            inner: f_ast,
        } => (function_type.dupe(), (loc.dupe(), f_ast.as_ref().clone())),
        _ => panic!("convert of Function should return Function"),
    }
}

fn mk_singleton_string(loc: ALoc, key: &str) -> Type {
    let name = Name::new(FlowSmolStr::new(key));
    let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RStringLit(name.dupe()), loc);
    Type::new(type_::TypeInner::DefT(
        reason,
        type_::DefT::new(type_::DefTInner::SingletonStrT {
            from_annot: true,
            value: name,
        }),
    ))
}

fn mk_singleton_number(loc: ALoc, num: f64, raw: &str) -> Type {
    let raw_str = FlowSmolStr::new(raw);
    let reason =
        reason::mk_annot_reason(reason::VirtualReasonDesc::RNumberLit(raw_str.dupe()), loc);
    Type::new(type_::TypeInner::DefT(
        reason,
        type_::DefT::new(type_::DefTInner::SingletonNumT {
            from_annot: true,
            value: type_::NumberLiteral(num, raw_str),
        }),
    ))
}

fn mk_singleton_boolean(loc: ALoc, b: bool) -> Type {
    let reason = reason::mk_annot_reason(reason::VirtualReasonDesc::RBooleanLit(b), loc);
    Type::new(type_::TypeInner::DefT(
        reason,
        type_::DefT::new(type_::DefTInner::SingletonBoolT {
            from_annot: true,
            value: b,
        }),
    ))
}

fn mk_singleton_bigint(loc: ALoc, num: Option<i64>, raw: &str) -> Type {
    let raw_str = FlowSmolStr::new(raw);
    let reason =
        reason::mk_annot_reason(reason::VirtualReasonDesc::RBigIntLit(raw_str.dupe()), loc);
    Type::new(type_::TypeInner::DefT(
        reason,
        type_::DefT::new(type_::DefTInner::SingletonBigIntT {
            from_annot: true,
            value: type_::BigIntLiteral(num, raw_str),
        }),
    ))
}

/// Given the type of expression C and type arguments T1...Tn, return the type of
/// values described by C<T1,...,Tn>, or C when there are no type arguments.
fn mk_nominal_type_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    reason: Reason,
    c: Type,
    targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
) -> (Type, Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>) {
    let annot_loc = reason.loc().dupe();
    match targs {
        None => {
            let reason = reason.annotate(annot_loc);
            let type_t_kind = if env.in_renders_arg {
                type_::TypeTKind::RenderTypeKind
            } else {
                type_::TypeTKind::InstanceKind
            };
            let t = crate::type_annotation_cons_gen::mk_instance(
                cx,
                reason,
                c,
                Some(type_t_kind),
                None,
            );
            (t, None)
        }
        Some(targs_node) => {
            let (targs_ts, targs_ast) = convert_list_inner(cx, env, &targs_node.arguments);
            let t = type_util::typeapp_annot(false, false, annot_loc, c, targs_ts);
            if type_subst::free_var_finder(cx, None, &t).is_empty() {
                match t.deref() {
                    type_::TypeInner::TypeAppT(box TypeAppTData {
                        reason: app_reason,
                        use_op,
                        type_: type_t,
                        targs: app_targs,
                        ..
                    }) => {
                        let tvar_t = flow_typing_tvar::mk(cx, app_reason.dupe());
                        cx.add_post_inference_validation_flow(
                            type_t.dupe(),
                            type_::UseT::new(type_::UseTInner::SpecializeT(Box::new(
                                type_::SpecializeTData {
                                    use_op: use_op.dupe(),
                                    reason: app_reason.dupe(),
                                    reason2: app_reason.dupe(),
                                    targs: Some(app_targs.clone()),
                                    tvar: tvar_t,
                                },
                            ))),
                        );
                    }
                    _ => panic!("typeapp_annot should create a TypeAppT"),
                }
            }
            (
                t,
                Some(ast::types::TypeArgs {
                    loc: {
                        let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(&targs_node.loc);
                        v
                    },
                    arguments: targs_ast.into(),
                    comments: targs_node.comments.dupe(),
                }),
            )
        }
    }
}

fn mk_type_param_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    kind: flow_parser::ast_visitor::TypeParamsContext,
    type_param: &ast::types::TypeParam<ALoc, ALoc>,
) -> (ast::types::TypeParam<ALoc, (ALoc, Type)>, TypeParam, Type) {
    use flow_parser::ast_visitor::TypeParamsContext;
    let node_cache = cx.node_cache();
    let loc = &type_param.loc;
    if let Some(cached) = node_cache.get_tparam(loc) {
        return cached;
    }

    let id = &type_param.name;
    let name_loc = id.loc.dupe();
    let name = &id.name;
    let bound = &type_param.bound;
    let bound_kind = &type_param.bound_kind;
    let variance = type_param.variance.as_ref();
    let default = &type_param.default;
    let const_mod = &type_param.const_;

    let reason = reason::mk_annot_reason(
        reason::VirtualReasonDesc::RType(Name::new(name.dupe())),
        name_loc.dupe(),
    );
    let pol = polarity(cx, variance);
    let is_const = match (const_mod, kind) {
        (Some(_), TypeParamsContext::Class)
        | (Some(_), TypeParamsContext::Function)
        | (Some(_), TypeParamsContext::DeclareFunction)
        | (Some(_), TypeParamsContext::DeclareClass)
        | (Some(_), TypeParamsContext::DeclareComponent)
        | (Some(_), TypeParamsContext::ComponentDeclaration)
        | (Some(_), TypeParamsContext::ComponentType)
        | (Some(_), TypeParamsContext::FunctionType)
        | (Some(_), TypeParamsContext::Record) => true,
        (Some(_), _) => {
            flow_js_utils::add_output_non_speculating(
                cx,
                ErrorMessage::ETypeParamConstInvalidPosition(reason.dupe()),
            );
            false
        }
        (None, _) => false,
    };
    // (match (bound_kind, bound) with
    // | (Ast.Type.TypeParam.Colon, Ast.Type.Available _)
    //   when (not (kind = Flow_ast_mapper.InferTP)) && Context.is_colon_extends_deprecated env.cx ->
    if *bound_kind == ast::types::type_param::BoundKind::Colon
        && matches!(bound, ast::types::AnnotationOrHint::Available(_))
        && !matches!(kind, TypeParamsContext::Infer)
        && cx.is_colon_extends_deprecated()
    {
        flow_js_utils::add_output_non_speculating(
            cx,
            ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                kind: flow_typing_errors::error_message::TSSyntaxKind::DeprecatedTypeParamColon,
                loc: loc.dupe(),
            })),
        );
    }
    let (bound_t, bound_ast) = match bound {
        ast::types::AnnotationOrHint::Missing(missing_loc) => {
            let t = type_::Type::new(type_::TypeInner::DefT(
                reason
                    .dupe()
                    .replace_desc(reason::VirtualReasonDesc::RMixed),
                type_::DefT::new(type_::DefTInner::MixedT(
                    type_::MixedFlavor::MixedEverything,
                )),
            ));
            (
                t.dupe(),
                ast::types::AnnotationOrHint::Missing((missing_loc.dupe(), t)),
            )
        }
        ast::types::AnnotationOrHint::Available(annot) => {
            let bound_loc = &annot.loc;
            let u = &annot.annotation;
            let annot_ast = convert_inner(cx, env, u);
            let (_, bound_val) = annot_ast.loc();
            let bound_val = bound_val.dupe();
            // Available (bound_loc, ast)
            let bound_ast = ast::types::AnnotationOrHint::Available(ast::types::Annotation {
                loc: bound_loc.dupe(),
                annotation: annot_ast,
            });
            (bound_val, bound_ast)
        }
    };
    let (default_t, default_ast) = match default {
        None => (None, None),
        Some(default_annot) => {
            let annot_ast = convert_inner(cx, env, default_annot);
            let (_, t) = annot_ast.loc();
            let t = t.dupe();
            cx.add_post_inference_subtyping_check(t.dupe(), type_::unknown_use(), bound_t.dupe());
            (Some(t), Some(annot_ast))
        }
    };
    let subst_name = if matches!(kind, TypeParamsContext::Infer)
        && env.tparams_map.contains_key(&SubstName::name(name.dupe()))
    {
        let keys: FlowOrdSet<SubstName> = env.tparams_map.keys().cloned().collect();
        type_subst::new_name(&SubstName::name(name.dupe()), &keys)
    } else {
        SubstName::name(name.dupe())
    };
    let tparam = TypeParam::new(type_::TypeParamInner {
        reason: reason.dupe(),
        name: subst_name,
        bound: bound_t,
        polarity: pol,
        default: default_t,
        is_this: false,
        is_const,
    });
    let t = flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), &tparam);
    let name_ast = ast::Identifier::new(ast::IdentifierInner {
        loc: (name_loc, t.dupe()),
        name: id.name.dupe(),
        comments: id.comments.dupe(),
    });
    let ast = ast::types::TypeParam {
        loc: (loc.dupe(), t.dupe()),
        name: name_ast,
        bound: bound_ast,
        bound_kind: bound_kind.clone(),
        variance: variance.cloned(),
        default: default_ast,
        const_: const_mod
            .as_ref()
            .map(|cm| ast::types::type_param::ConstModifier {
                loc: {
                    let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(&cm.loc);
                    v
                },
                comments: cm.comments.dupe(),
            }),
    };
    (ast, tparam, t)
}

/// take a list of AST type param declarations,
/// do semantic checking and create types for them.
fn mk_type_param_declarations_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    kind: flow_parser::ast_visitor::TypeParamsContext,
    tparams: Option<&ast::types::TypeParams<ALoc, ALoc>>,
) -> (
    type_::TypeParams,
    Option<ast::types::TypeParams<ALoc, (ALoc, Type)>>,
) {
    match tparams {
        None => (None, None),
        Some(tparams_node) => {
            let tparams_loc = &tparams_node.loc;
            let comments = &tparams_node.comments;
            let mut tparams: Vec<TypeParam> = Vec::new();
            let mut bounds_map: FlowOrdMap<SubstName, Type> = FlowOrdMap::default();
            let mut asts: Vec<ast::types::TypeParam<ALoc, (ALoc, Type)>> = Vec::new();
            for tp in tparams_node.params.iter() {
                let (ast, tparam, t) = mk_type_param_inner(cx, env, kind, tp);
                let name = tparam.name.dupe();
                let bound = tparam.bound.dupe();
                tparams.push(tparam);
                env.tparams_map.insert(name.dupe(), t);
                let subst_bound = type_subst::subst(
                    cx,
                    None,
                    true,
                    false,
                    type_subst::Purpose::Normal,
                    &bounds_map,
                    bound,
                );
                bounds_map.insert(name, subst_bound);
                asts.push(ast);
            }
            let tparams_ast = Some(ast::types::TypeParams {
                loc: {
                    let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(tparams_loc);
                    v
                },
                params: asts.into(),
                comments: comments.clone(),
            });
            let tparams = if tparams.is_empty() {
                None
            } else {
                let mut iter = tparams.into_iter();
                let hd = iter.next().unwrap();
                let mut nel = vec1::Vec1::new(hd);
                nel.extend(iter);
                Some((tparams_loc.dupe(), nel))
            };
            (tparams, tparams_ast)
        }
    }
}

fn type_identifier<'a>(cx: &Context<'a>, name: &FlowSmolStr, loc: ALoc) -> Type {
    let t = type_env::query_var(
        Some(type_env::LookupMode::ForType),
        cx,
        Name::new(name.dupe()),
        None,
        loc.dupe(),
    );
    type_util::mod_reason_of_t(&|r: Reason| r.reposition(loc.dupe()), &t)
}

fn mk_interface_super<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    generic_with_loc: &(ALoc, ast::types::Generic<ALoc, ALoc>),
) -> (
    (ALoc, Type, Option<Vec<Type>>),
    (ALoc, ast::types::Generic<ALoc, (ALoc, Type)>),
) {
    let (loc, generic) = generic_with_loc;
    let id = &generic.id;
    let targs = &generic.targs;
    let comments = &generic.comments;
    let (c, id_typed) = convert_qualification(cx, "extends", id);
    let (typeapp, targs_typed) = match targs {
        None => ((loc.dupe(), c, None), None),
        Some(targs_node) => {
            let (ts, targs_ast) = convert_list_inner(cx, env, &targs_node.arguments);
            (
                (loc.dupe(), c, Some(ts)),
                Some(ast::types::TypeArgs {
                    loc: {
                        let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(&targs_node.loc);
                        v
                    },
                    arguments: targs_ast.into(),
                    comments: targs_node.comments.dupe(),
                }),
            )
        }
    };
    (
        typeapp,
        (
            loc.dupe(),
            ast::types::Generic {
                id: id_typed,
                targs: targs_typed,
                comments: comments.dupe(),
            },
        ),
    )
}

fn convert_indexer_internal(
    cx: &Context,
    env: &mut ConvertEnv,
    indexer: &ast::types::object::Indexer<ALoc, ALoc>,
) -> (
    type_::DictType,
    ast::types::object::Indexer<ALoc, (ALoc, Type)>,
) {
    let key_ast = convert_inner(cx, env, &indexer.key);
    let (_, k) = key_ast.loc();
    let k = k.dupe();
    let value_ast = convert_inner(cx, env, &indexer.value);
    let (annot_loc, v) = value_ast.loc();
    let annot_loc = annot_loc.dupe();
    let v = v.dupe();
    let p = polarity(cx, indexer.variance.as_ref());
    let v = if indexer.optional {
        type_util::optional(v, Some(annot_loc), false)
    } else {
        v
    };
    let dict = type_::DictType {
        dict_name: indexer.id.as_ref().map(|id| id.name.dupe()),
        key: k,
        value: v.dupe(),
        dict_polarity: p,
    };
    let indexer_ast = ast::types::object::Indexer {
        loc: indexer.loc.dupe(),
        id: indexer.id.as_ref().map(|id| {
            ast::Identifier::new(ast::IdentifierInner {
                loc: (id.loc.dupe(), v.dupe()),
                name: id.name.dupe(),
                comments: id.comments.dupe(),
            })
        }),
        key: key_ast,
        value: value_ast,
        static_: indexer.static_,
        variance: indexer.variance.clone(),
        optional: indexer.optional,
        comments: indexer.comments.dupe(),
    };
    (dict, indexer_ast)
}

fn add_interface_properties<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    record_for_interface: Option<ALoc>,
    obj_kind: intermediate_error_types::ObjKind,
    this: Type,
    properties: &[ast::types::object::Property<ALoc, ALoc>],
    mut s: func_class_sig_types::class::Class<FuncTypeParamsConfig>,
) -> (
    func_class_sig_types::class::Class<FuncTypeParamsConfig>,
    Vec<ast::types::object::Property<ALoc, (ALoc, Type)>>,
) {
    let record_field = |name: &FlowSmolStr, t: &Type| {
        if let Some(id_loc) = &record_for_interface {
            cx.record_interface_field(id_loc.dupe(), Name::new(name.dupe()), t.dupe());
        }
    };
    use flow_parser::ast::types::object::Property;

    let mut prop_asts: Vec<Property<ALoc, (ALoc, Type)>> = Vec::new();
    for prop in properties {
        match prop {
            Property::CallProperty(cp) => {
                let (t, value_ast) = mk_function_type_annotation_inner(
                    cx,
                    env,
                    &(cp.value.0.dupe(), cp.value.1.clone()),
                );
                class_sig::append_call(cp.static_, t, &mut s);
                prop_asts.push(Property::CallProperty(ast::types::object::CallProperty {
                    loc: cp.loc.dupe(),
                    value: value_ast,
                    static_: cp.static_,
                    comments: cp.comments.dupe(),
                }));
            }
            Property::Indexer(idx) if class_sig::has_indexer(idx.static_, &s) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EUnsupportedSyntax(Box::new((
                        idx.loc.dupe(),
                        intermediate_error_types::UnsupportedSyntax::MultipleIndexers,
                    ))),
                );
                let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                    &mut typed_ast_utils::ErrorMapper,
                    prop,
                );
                prop_asts.push(error_prop);
            }
            Property::Indexer(idx) => {
                if idx.optional && !cx.tslib_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            idx.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                intermediate_error_types::TsLibSyntaxKind::OptionalIndexer,
                            ),
                        ))),
                    );
                    let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    prop_asts.push(error_prop);
                } else {
                    let (dict, indexer_ast) = convert_indexer_internal(cx, env, idx);
                    class_sig::add_indexer(idx.static_, dict, &mut s);
                    prop_asts.push(Property::Indexer(indexer_ast));
                }
            }
            Property::MappedType(mt) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInvalidMappedType {
                        loc: mt.loc.dupe(),
                        kind: flow_typing_errors::error_message::InvalidMappedTypeErrorKind::InterfaceOrDeclaredClass,
                    },
                );
                let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                    &mut typed_ast_utils::ErrorMapper,
                    prop,
                );
                prop_asts.push(error_prop);
            }
            Property::NormalProperty(np) => {
                if np.override_ {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            np.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::OverrideModifier,
                            ),
                        ))),
                    );
                }
                let init_ = &np.init;
                let is_ts_private = match &np.ts_accessibility {
                    Some(acc) => {
                        if !cx.ts_syntax() {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                                    kind: flow_typing_errors::error_message::TSSyntaxKind::TSClassAccessibility(acc.kind),
                                    loc: acc.loc.dupe(),
                                })),
                            );
                        }
                        matches!(
                            acc.kind,
                            flow_parser::ast::class::ts_accessibility::Kind::Private
                        )
                    }
                    None => false,
                };
                let is_literal_init = |expr: &ast::expression::Expression<ALoc, ALoc>| -> bool {
                    match &*expr.0 {
                        ast::expression::ExpressionInner::StringLiteral { .. }
                        | ast::expression::ExpressionInner::NumberLiteral { .. }
                        | ast::expression::ExpressionInner::BigIntLiteral { .. }
                        | ast::expression::ExpressionInner::BooleanLiteral { .. } => true,
                        ast::expression::ExpressionInner::Unary { inner, .. }
                            if inner.operator == ast::expression::UnaryOperator::Minus =>
                        {
                            matches!(
                                &*inner.argument.0,
                                ast::expression::ExpressionInner::NumberLiteral { .. }
                                    | ast::expression::ExpressionInner::BigIntLiteral { .. }
                            )
                        }
                        _ => false,
                    }
                };
                match (init_, &np.value) {
                    (Some(_), _) if !cx.tslib_syntax() => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                np.loc.dupe(),
                                intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                    TsLibSyntaxKind::PropertyValueInitializer,
                                ),
                            ))),
                        );
                    }
                    (Some(init_expr), ast::types::object::PropertyValue::Init(Some(_))) => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                init_expr.loc().dupe(),
                                intermediate_error_types::UnsupportedSyntax::DeclareClassProperty(
                                    intermediate_error_types::DeclareClassPropKind::AnnotationAndInit,
                                ),
                            ))),
                        );
                    }
                    (Some(init_expr), ast::types::object::PropertyValue::Init(None)) => {
                        let is_readonly = matches!(
                            &np.variance,
                            Some(ast::Variance {
                                kind: ast::VarianceKind::Readonly | ast::VarianceKind::Plus,
                                ..
                            })
                        );
                        if !is_readonly {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    init_expr.loc().dupe(),
                                    intermediate_error_types::UnsupportedSyntax::DeclareClassProperty(
                                        intermediate_error_types::DeclareClassPropKind::InitWithoutReadonly,
                                    ),
                                ))),
                            );
                        }
                        if !is_literal_init(init_expr) {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    init_expr.loc().dupe(),
                                    intermediate_error_types::UnsupportedSyntax::DeclareClassProperty(
                                        intermediate_error_types::DeclareClassPropKind::NonLiteralInit,
                                    ),
                                ))),
                            );
                        }
                    }
                    (None, ast::types::object::PropertyValue::Init(None)) if !is_ts_private => {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                np.loc.dupe(),
                                intermediate_error_types::UnsupportedSyntax::DeclareClassProperty(
                                    intermediate_error_types::DeclareClassPropKind::MissingAnnotationOrInit,
                                ),
                            ))),
                        );
                    }
                    _ => {}
                }
                if is_ts_private {
                    // Private members are excluded from the declare class's public
                    // interface, so we skip adding them to the class signature.
                    let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                        &mut typed_ast_utils::UncheckedMapper,
                        prop,
                    );
                    prop_asts.push(error_prop);
                } else {
                    if np.abstract_ && !cx.metadata().frozen.abstract_classes {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::ETSSyntax(Box::new(ETSSyntaxData {
                                kind:
                                    flow_typing_errors::error_message::TSSyntaxKind::AbstractMethod,
                                loc: np.loc.dupe(),
                            })),
                        );
                    }
                    if np.optional && np.method && !cx.tslib_syntax() {
                        flow_js_utils::add_output_non_speculating(
                            cx,
                            ErrorMessage::EUnsupportedSyntax(Box::new((
                                np.loc.dupe(),
                                intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                    TsLibSyntaxKind::OptionalShorthandMethod,
                                ),
                            ))),
                        );
                        let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                            &mut typed_ast_utils::ErrorMapper,
                            prop,
                        );
                        prop_asts.push(error_prop);
                        continue;
                    }
                    let polarity = polarity(cx, np.variance.as_ref());
                    let mut handle_init_only_property =
                        |name: FlowSmolStr,
                         id_loc: ALoc,
                         rebuild_key: &dyn Fn(Type) -> ast::expression::object::Key<ALoc, (ALoc, Type)>|
                         -> Option<ast::types::object::Property<ALoc, (ALoc, Type)>> {
                            match init_ {
                                Some(init_expr)
                                    if cx.tslib_syntax() && is_literal_init(init_expr) =>
                                {
                                    let init_ast = crate::statement::expression(
                                        None,
                                        None,
                                        Some(true),
                                        cx,
                                        init_expr,
                                    )
                                    .unwrap();
                                    let t = init_ast.loc().1.dupe();
                                    let t = if np.optional {
                                        type_util::optional(t, None, false)
                                    } else {
                                        t
                                    };
                                    if np.proto {
                                        class_sig::add_proto_field(
                                            name.dupe(),
                                            id_loc.dupe(),
                                            polarity,
                                            func_class_sig_types::class::Field::Annot(t.dupe()),
                                            &mut s,
                                        );
                                    } else {
                                        record_field(&name, &t);
                                        class_sig::add_field(
                                            np.static_,
                                            name.dupe(),
                                            id_loc.dupe(),
                                            polarity,
                                            func_class_sig_types::class::Field::Annot(t.dupe()),
                                            &mut s,
                                        );
                                    }
                                    Some(Property::NormalProperty(
                                        ast::types::object::NormalProperty {
                                            loc: np.loc.dupe(),
                                            key: rebuild_key(t),
                                            value: ast::types::object::PropertyValue::Init(None),
                                            optional: np.optional,
                                            static_: np.static_,
                                            proto: np.proto,
                                            method: np.method,
                                            abstract_: np.abstract_,
                                            override_: np.override_,
                                            variance: np.variance.clone(),
                                            ts_accessibility: np.ts_accessibility.clone(),
                                            init: Some(init_ast),
                                            comments: np.comments.dupe(),
                                        },
                                    ))
                                }
                                _ => None,
                            }
                        };
                    use flow_parser::ast::expression::object::Key;
                    match &np.key {
                        Key::BigIntLiteral((loc, _)) => {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    loc.dupe(),
                                    intermediate_error_types::UnsupportedSyntax::IllegalName,
                                ))),
                            );
                            let Ok(error_prop) = polymorphic_ast_mapper::object_property_type(
                                &mut typed_ast_utils::ErrorMapper,
                                np,
                            );
                            prop_asts.push(Property::NormalProperty(error_prop));
                            continue;
                        }
                        Key::PrivateName(pn) => {
                            flow_js_utils::add_output_non_speculating(
                                cx,
                                ErrorMessage::EUnsupportedSyntax(Box::new((
                                    pn.loc.dupe(),
                                    intermediate_error_types::UnsupportedSyntax::IllegalName,
                                ))),
                            );
                            let Ok(error_prop) = polymorphic_ast_mapper::object_property_type(
                                &mut typed_ast_utils::ErrorMapper,
                                np,
                            );
                            prop_asts.push(Property::NormalProperty(error_prop));
                            continue;
                        }
                        Key::Computed(ck) => {
                            let (typed_expr, resolved_name) =
                                resolve_computed_key_name(cx, &ck.expression);
                            let resolved_name = resolved_name.map(|n| n.into_smol_str());
                            match resolved_name {
                                Some(name) => {
                                    let id_loc = ck.expression.loc().dupe();
                                    if np.method {
                                        match &np.value {
                                            ast::types::object::PropertyValue::Init(Some(
                                                value,
                                            )) if let ast::types::TypeInner::Function {
                                                loc: func_loc,
                                                inner: func,
                                            } = &**value =>
                                            {
                                                let meth_kind = MethodKind::MethodKind {
                                                    is_static: np.static_,
                                                };
                                                let (fsig, func_ast) = mk_method_func_sig(
                                                    cx,
                                                    env,
                                                    meth_kind,
                                                    np.loc.dupe(),
                                                    func,
                                                );
                                                let this_write_loc = None;
                                                let ft = crate::func_sig::methodtype(
                                                    cx,
                                                    this_write_loc,
                                                    this.dupe(),
                                                    &fsig,
                                                );
                                                class_sig::append_method(
                                                    np.static_,
                                                    name.dupe(),
                                                    id_loc.dupe(),
                                                    None,
                                                    fsig,
                                                    None,
                                                    None,
                                                    &mut s,
                                                );
                                                prop_asts.push(Property::NormalProperty(
                                                    ast::types::object::NormalProperty {
                                                        loc: np.loc.dupe(),
                                                        key: Key::Computed(ast::ComputedKey {
                                                            loc: ck.loc.dupe(),
                                                            expression: typed_expr,
                                                            comments: ck.comments.dupe(),
                                                        }),
                                                        value:
                                                            ast::types::object::PropertyValue::Init(
                                                                Some(ast::types::Type::new(
                                                                    ast::types::TypeInner::Function {
                                                                        loc: (func_loc.dupe(), ft),
                                                                        inner: func_ast.into(),
                                                                    },
                                                                )),
                                                            ),
                                                        optional: np.optional,
                                                        static_: np.static_,
                                                        proto: np.proto,
                                                        method: np.method,
                                                        abstract_: np.abstract_,
                                                        override_: np.override_,
                                                        variance: np.variance.clone(),
                                                        ts_accessibility: np.ts_accessibility.clone(),
                                                        init: None,
                                                        comments: np.comments.dupe(),
                                                    },
                                                ));
                                            }
                                            _ => {
                                                flow_js_utils::add_output_non_speculating(
                                                    cx,
                                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                                        ck.loc.dupe(),
                                                        intermediate_error_types::UnsupportedSyntax::IllegalName,
                                                    ))),
                                                );
                                                let Ok(error_prop) =
                                                    polymorphic_ast_mapper::object_property_type(
                                                        &mut typed_ast_utils::ErrorMapper,
                                                        np,
                                                    );
                                                prop_asts
                                                    .push(Property::NormalProperty(error_prop));
                                            }
                                        }
                                    } else {
                                        match &np.value {
                                            ast::types::object::PropertyValue::Init(Some(
                                                value,
                                            )) => {
                                                let value_ast = convert_inner(cx, env, value);
                                                let (_, t) = value_ast.loc();
                                                let t = t.dupe();
                                                let t_with_optional = if np.optional {
                                                    type_util::optional(t.dupe(), None, false)
                                                } else {
                                                    t.dupe()
                                                };
                                                if np.proto {
                                                    class_sig::add_proto_field(
                                                        name.dupe(),
                                                        id_loc.dupe(),
                                                        polarity,
                                                        func_class_sig_types::class::Field::Annot(
                                                            t_with_optional.dupe(),
                                                        ),
                                                        &mut s,
                                                    );
                                                } else {
                                                    record_field(&name, &t_with_optional);
                                                    class_sig::add_field(
                                                        np.static_,
                                                        name.dupe(),
                                                        id_loc.dupe(),
                                                        polarity,
                                                        func_class_sig_types::class::Field::Annot(
                                                            t_with_optional.dupe(),
                                                        ),
                                                        &mut s,
                                                    );
                                                }
                                                let init_ast = init_.as_ref().map(|init_expr| {
                                                    crate::statement::expression(
                                                        None,
                                                        None,
                                                        Some(true),
                                                        cx,
                                                        init_expr,
                                                    )
                                                    .unwrap()
                                                });
                                                prop_asts.push(Property::NormalProperty(
                                                    ast::types::object::NormalProperty {
                                                        loc: np.loc.dupe(),
                                                        key: Key::Computed(ast::ComputedKey {
                                                            loc: ck.loc.dupe(),
                                                            expression: typed_expr,
                                                            comments: ck.comments.dupe(),
                                                        }),
                                                        value:
                                                            ast::types::object::PropertyValue::Init(
                                                                Some(value_ast),
                                                            ),
                                                        optional: np.optional,
                                                        static_: np.static_,
                                                        proto: np.proto,
                                                        method: np.method,
                                                        abstract_: np.abstract_,
                                                        override_: np.override_,
                                                        variance: np.variance.clone(),
                                                        ts_accessibility: np
                                                            .ts_accessibility
                                                            .clone(),
                                                        init: init_ast,
                                                        comments: np.comments.dupe(),
                                                    },
                                                ));
                                            }
                                            ast::types::object::PropertyValue::Init(None) => {
                                                let typed_expr_clone = typed_expr.clone();
                                                let ck_loc = ck.loc.dupe();
                                                let ck_comments = ck.comments.dupe();
                                                if let Some(prop_ast) = handle_init_only_property(
                                                    name.dupe(),
                                                    id_loc.dupe(),
                                                    &|_t| {
                                                        Key::Computed(ast::ComputedKey {
                                                            loc: ck_loc.dupe(),
                                                            expression: typed_expr_clone.clone(),
                                                            comments: ck_comments.dupe(),
                                                        })
                                                    },
                                                ) {
                                                    prop_asts.push(prop_ast);
                                                } else {
                                                    let Ok(error_prop) =
                                                        polymorphic_ast_mapper::object_property_type(
                                                            &mut typed_ast_utils::ErrorMapper,
                                                            np,
                                                        );
                                                    prop_asts
                                                        .push(Property::NormalProperty(error_prop));
                                                }
                                            }
                                            _ => {
                                                flow_js_utils::add_output_non_speculating(
                                                    cx,
                                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                                        ck.loc.dupe(),
                                                        intermediate_error_types::UnsupportedSyntax::IllegalName,
                                                    ))),
                                                );
                                                let Ok(error_prop) =
                                                    polymorphic_ast_mapper::object_property_type(
                                                        &mut typed_ast_utils::ErrorMapper,
                                                        np,
                                                    );
                                                prop_asts
                                                    .push(Property::NormalProperty(error_prop));
                                            }
                                        }
                                    }
                                }
                                None => {
                                    flow_js_utils::add_output_non_speculating(
                                        cx,
                                        ErrorMessage::EUnsupportedSyntax(Box::new((
                                            ck.loc.dupe(),
                                            intermediate_error_types::UnsupportedSyntax::IllegalName,
                                        ))),
                                    );
                                    let Ok(error_prop) =
                                        polymorphic_ast_mapper::object_property_type(
                                            &mut typed_ast_utils::ErrorMapper,
                                            np,
                                        );
                                    prop_asts.push(Property::NormalProperty(error_prop));
                                }
                            }
                            continue;
                        }
                        _ => {}
                    };
                    type RebuildKey = Box<dyn Fn(Type) -> Key<ALoc, (ALoc, Type)>>;
                    let resolve_named_key =
                        |check_variance: bool| -> Option<(FlowSmolStr, ALoc, RebuildKey)> {
                            let check_num =
                                |key_loc: ALoc,
                                 num_value: f64,
                                 num_lit: ast::NumberLiteral<ALoc>|
                                 -> Option<(FlowSmolStr, ALoc, RebuildKey)> {
                                    if flow_common::js_number::is_float_safe_integer(num_value) {
                                        let name = flow_common::js_number::ecma_string_of_float(
                                            num_value,
                                        );
                                        let key_loc_clone = key_loc.dupe();
                                        Some((
                                            FlowSmolStr::new(&name),
                                            key_loc,
                                            Box::new(move |t| {
                                                Key::NumberLiteral((
                                                    (key_loc_clone.dupe(), t),
                                                    num_lit.clone(),
                                                ))
                                            }),
                                        ))
                                    } else {
                                        flow_js_utils::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsupportedKeyInObject {
                                                loc: key_loc,
                                                obj_kind: obj_kind.clone(),
                                                key_error_kind:
                                                    InvalidObjKey::kind_of_num_value(num_value),
                                            },
                                        );
                                        None
                                    }
                                };
                            match &np.key {
                                Key::Identifier(id_name) => {
                                    let id_loc = id_name.loc.dupe();
                                    let id_name_clone = id_name.clone();
                                    Some((
                                        id_name.name.dupe(),
                                        id_loc.dupe(),
                                        Box::new(move |t| {
                                            Key::Identifier(ast::Identifier::new(
                                                ast::IdentifierInner {
                                                    loc: (id_loc.dupe(), t),
                                                    name: id_name_clone.name.dupe(),
                                                    comments: id_name_clone.comments.dupe(),
                                                },
                                            ))
                                        }),
                                    ))
                                }
                                Key::StringLiteral((key_loc, str_lit)) => {
                                    let name = str_lit.value.dupe();
                                    let key_loc = key_loc.dupe();
                                    let str_lit_clone = str_lit.clone();
                                    Some((
                                        name,
                                        key_loc.dupe(),
                                        Box::new(move |t| {
                                            Key::StringLiteral((
                                                (key_loc.dupe(), t),
                                                str_lit_clone.clone(),
                                            ))
                                        }),
                                    ))
                                }
                                Key::NumberLiteral((key_loc, num_lit)) => {
                                    if check_variance {
                                        match &np.variance {
                                            Some(ast::Variance {
                                                kind:
                                                    ast::VarianceKind::Plus | ast::VarianceKind::Minus,
                                                ..
                                            }) => {
                                                flow_js_utils::add_output_non_speculating(
                                                    cx,
                                                    ErrorMessage::EAmbiguousNumericKeyWithVariance(
                                                        key_loc.dupe(),
                                                    ),
                                                );
                                                None
                                            }
                                            _ => check_num(
                                                key_loc.dupe(),
                                                num_lit.value,
                                                num_lit.clone(),
                                            ),
                                        }
                                    } else {
                                        check_num(key_loc.dupe(), num_lit.value, num_lit.clone())
                                    }
                                }
                                _ => None,
                            }
                        };
                    // method
                    if np.method {
                        match &np.value {
                            ast::types::object::PropertyValue::Init(Some(value))
                                if let ast::types::TypeInner::Function {
                                    loc: func_loc,
                                    inner: func,
                                } = &**value =>
                            {
                                match resolve_named_key(false) {
                                    None => {
                                        let Ok(error_prop) =
                                            polymorphic_ast_mapper::object_property_type(
                                                &mut typed_ast_utils::ErrorMapper,
                                                np,
                                            );
                                        prop_asts.push(Property::NormalProperty(error_prop));
                                    }
                                    Some((name, key_loc, rebuild_key)) => {
                                        let meth_kind = match name.as_str() {
                                            "constructor" => MethodKind::ConstructorKind,
                                            _ => MethodKind::MethodKind {
                                                is_static: np.static_,
                                            },
                                        };
                                        let (fsig, func_ast) = mk_method_func_sig(
                                            cx,
                                            env,
                                            meth_kind,
                                            np.loc.dupe(),
                                            func,
                                        );
                                        let this_write_loc = None;
                                        let ft = crate::func_sig::methodtype(
                                            cx,
                                            this_write_loc,
                                            this.dupe(),
                                            &fsig,
                                        );
                                        match (np.static_, &meth_kind) {
                                            (false, MethodKind::ConstructorKind) => {
                                                class_sig::append_constructor(
                                                    Some(key_loc.dupe()),
                                                    fsig,
                                                    None,
                                                    None,
                                                    &mut s,
                                                );
                                            }
                                            _ => {
                                                class_sig::append_method(
                                                    np.static_,
                                                    name.dupe(),
                                                    key_loc.dupe(),
                                                    None,
                                                    fsig,
                                                    None,
                                                    None,
                                                    &mut s,
                                                );
                                            }
                                        }
                                        prop_asts.push(Property::NormalProperty(
                                            ast::types::object::NormalProperty {
                                                loc: np.loc.dupe(),
                                                key: rebuild_key(ft.dupe()),
                                                value: ast::types::object::PropertyValue::Init(
                                                    Some(ast::types::Type::new(
                                                        ast::types::TypeInner::Function {
                                                            loc: (func_loc.dupe(), ft),
                                                            inner: func_ast.into(),
                                                        },
                                                    )),
                                                ),
                                                optional: np.optional,
                                                static_: np.static_,
                                                proto: np.proto,
                                                method: np.method,
                                                abstract_: np.abstract_,
                                                override_: np.override_,
                                                variance: np.variance.clone(),
                                                ts_accessibility: np.ts_accessibility.clone(),
                                                init: None,
                                                comments: np.comments.dupe(),
                                            },
                                        ));
                                    }
                                }
                            }
                            // method not a function
                            _ => {
                                flow_js_utils::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EInternal(Box::new((
                                        np.loc.dupe(),
                                        InternalError::MethodNotAFunction,
                                    ))),
                                );
                                let Ok(error_prop) = polymorphic_ast_mapper::object_property_type(
                                    &mut typed_ast_utils::ErrorMapper,
                                    np,
                                );
                                prop_asts.push(Property::NormalProperty(error_prop));
                            }
                        }
                    } else {
                        match &np.value {
                            // field with annotation
                            ast::types::object::PropertyValue::Init(Some(value)) => {
                                match resolve_named_key(true) {
                                    None => {
                                        let Ok(error_prop) =
                                            polymorphic_ast_mapper::object_property_type(
                                                &mut typed_ast_utils::ErrorMapper,
                                                np,
                                            );
                                        prop_asts.push(Property::NormalProperty(error_prop));
                                    }
                                    Some((name, key_loc, rebuild_key)) => {
                                        let value_ast = convert_inner(cx, env, value);
                                        let (_, t) = value_ast.loc();
                                        let t = t.dupe();
                                        let t_with_optional = if np.optional {
                                            type_util::optional(t.dupe(), None, false)
                                        } else {
                                            t.dupe()
                                        };
                                        if np.proto {
                                            class_sig::add_proto_field(
                                                name.dupe(),
                                                key_loc.dupe(),
                                                polarity,
                                                func_class_sig_types::class::Field::Annot(
                                                    t_with_optional.dupe(),
                                                ),
                                                &mut s,
                                            );
                                        } else {
                                            record_field(&name, &t_with_optional);
                                            class_sig::add_field(
                                                np.static_,
                                                name.dupe(),
                                                key_loc.dupe(),
                                                polarity,
                                                func_class_sig_types::class::Field::Annot(
                                                    t_with_optional.dupe(),
                                                ),
                                                &mut s,
                                            );
                                        }
                                        let init_ast = init_.as_ref().map(|init_expr| {
                                            crate::statement::expression(
                                                None,
                                                None,
                                                Some(true),
                                                cx,
                                                init_expr,
                                            )
                                            .unwrap()
                                        });
                                        prop_asts.push(Property::NormalProperty(
                                            ast::types::object::NormalProperty {
                                                loc: np.loc.dupe(),
                                                key: rebuild_key(t_with_optional),
                                                value: ast::types::object::PropertyValue::Init(
                                                    Some(value_ast),
                                                ),
                                                optional: np.optional,
                                                static_: np.static_,
                                                proto: np.proto,
                                                method: np.method,
                                                abstract_: np.abstract_,
                                                override_: np.override_,
                                                variance: np.variance.clone(),
                                                ts_accessibility: np.ts_accessibility.clone(),
                                                init: init_ast,
                                                comments: np.comments.dupe(),
                                            },
                                        ));
                                    }
                                }
                            }
                            // field without annotation
                            ast::types::object::PropertyValue::Init(None) => {
                                match resolve_named_key(true) {
                                    None => {
                                        let Ok(error_prop) =
                                            polymorphic_ast_mapper::object_property_type(
                                                &mut typed_ast_utils::ErrorMapper,
                                                np,
                                            );
                                        prop_asts.push(Property::NormalProperty(error_prop));
                                    }
                                    Some((name, key_loc, rebuild_key)) => {
                                        if let Some(prop_ast) =
                                            handle_init_only_property(name, key_loc, &*rebuild_key)
                                        {
                                            prop_asts.push(prop_ast);
                                        } else {
                                            let Ok(error_prop) =
                                                polymorphic_ast_mapper::object_property_type(
                                                    &mut typed_ast_utils::ErrorMapper,
                                                    np,
                                                );
                                            prop_asts.push(Property::NormalProperty(error_prop));
                                        }
                                    }
                                }
                            }
                            // getter
                            ast::types::object::PropertyValue::Get(get_loc, func) => {
                                match resolve_named_key(false) {
                                    None => {
                                        let Ok(error_prop) =
                                            polymorphic_ast_mapper::object_property_type(
                                                &mut typed_ast_utils::ErrorMapper,
                                                np,
                                            );
                                        prop_asts.push(Property::NormalProperty(error_prop));
                                    }
                                    Some((name, key_loc, rebuild_key)) => {
                                        flow_js_utils::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsafeGettersSetters(np.loc.dupe()),
                                        );
                                        let (fsig, func_ast) = mk_method_func_sig(
                                            cx,
                                            env,
                                            MethodKind::GetterKind,
                                            np.loc.dupe(),
                                            func,
                                        );
                                        let prop_t = type_util::type_t_of_annotated_or_inferred(
                                            &fsig.return_t,
                                        )
                                        .dupe();
                                        class_sig::add_getter(
                                            np.static_,
                                            name.dupe(),
                                            key_loc.dupe(),
                                            None,
                                            fsig,
                                            None,
                                            None,
                                            &mut s,
                                        );
                                        prop_asts.push(Property::NormalProperty(
                                            ast::types::object::NormalProperty {
                                                loc: np.loc.dupe(),
                                                key: rebuild_key(prop_t),
                                                value: ast::types::object::PropertyValue::Get(
                                                    get_loc.dupe(),
                                                    func_ast,
                                                ),
                                                optional: np.optional,
                                                static_: np.static_,
                                                proto: np.proto,
                                                method: np.method,
                                                abstract_: np.abstract_,
                                                override_: np.override_,
                                                variance: np.variance.clone(),
                                                ts_accessibility: np.ts_accessibility.clone(),
                                                init: None,
                                                comments: np.comments.dupe(),
                                            },
                                        ));
                                    }
                                }
                            }
                            // setter
                            ast::types::object::PropertyValue::Set(set_loc, func) => {
                                match resolve_named_key(false) {
                                    None => {
                                        let Ok(error_prop) =
                                            polymorphic_ast_mapper::object_property_type(
                                                &mut typed_ast_utils::ErrorMapper,
                                                np,
                                            );
                                        prop_asts.push(Property::NormalProperty(error_prop));
                                    }
                                    Some((name, key_loc, rebuild_key)) => {
                                        flow_js_utils::add_output_non_speculating(
                                            cx,
                                            ErrorMessage::EUnsafeGettersSetters(np.loc.dupe()),
                                        );
                                        let (fsig, func_ast) = mk_method_func_sig(
                                            cx,
                                            env,
                                            MethodKind::SetterKind,
                                            np.loc.dupe(),
                                            func,
                                        );
                                        let prop_t = if fsig.tparams.is_none() {
                                            let params =
                                                crate::func_params::value::<FuncTypeParamsConfig>(
                                                    &fsig.fparams.params,
                                                );
                                            if params.len() == 1 {
                                                params.into_iter().next().unwrap().1
                                            } else {
                                                type_::any_t::at(
                                                    type_::AnySource::AnyError(None),
                                                    key_loc.dupe(),
                                                )
                                            }
                                        } else {
                                            type_::any_t::at(
                                                type_::AnySource::AnyError(None),
                                                key_loc.dupe(),
                                            )
                                        };
                                        class_sig::add_setter(
                                            np.static_,
                                            name.dupe(),
                                            key_loc.dupe(),
                                            None,
                                            fsig,
                                            None,
                                            None,
                                            &mut s,
                                        );
                                        prop_asts.push(Property::NormalProperty(
                                            ast::types::object::NormalProperty {
                                                loc: np.loc.dupe(),
                                                key: rebuild_key(prop_t),
                                                value: ast::types::object::PropertyValue::Set(
                                                    set_loc.dupe(),
                                                    func_ast,
                                                ),
                                                optional: np.optional,
                                                static_: np.static_,
                                                proto: np.proto,
                                                method: np.method,
                                                abstract_: np.abstract_,
                                                override_: np.override_,
                                                variance: np.variance.clone(),
                                                ts_accessibility: np.ts_accessibility.clone(),
                                                init: None,
                                                comments: np.comments.dupe(),
                                            },
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Property::InternalSlot(is) => {
                let name = &is.id.name;
                if name.as_str() == "call" {
                    let value_ast = convert_inner(cx, env, &is.value);
                    let (_, t) = value_ast.loc();
                    let t = t.dupe();
                    let t = if is.optional {
                        type_util::optional(t, None, false)
                    } else {
                        t
                    };
                    class_sig::append_call(is.static_, t, &mut s);
                    prop_asts.push(Property::InternalSlot(ast::types::object::InternalSlot {
                        loc: is.loc.dupe(),
                        id: ast::Identifier::new(ast::IdentifierInner {
                            loc: is.id.loc.dupe(),
                            name: is.id.name.dupe(),
                            comments: is.id.comments.dupe(),
                        }),
                        value: value_ast,
                        optional: is.optional,
                        static_: is.static_,
                        method: is.method,
                        comments: is.comments.dupe(),
                    }));
                } else {
                    // Unsupported internal slot
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            is.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::UnsupportedInternalSlot {
                                name: name.dupe(),
                                static_: is.static_,
                            },
                        ))),
                    );
                    let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                        &mut typed_ast_utils::ErrorMapper,
                        prop,
                    );
                    prop_asts.push(error_prop);
                }
            }
            Property::SpreadProperty(sp) => {
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInternal(Box::new((
                        sp.loc.dupe(),
                        InternalError::InterfaceTypeSpread,
                    ))),
                );
                let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                    &mut typed_ast_utils::ErrorMapper,
                    prop,
                );
                prop_asts.push(error_prop);
            }
            Property::PrivateField(pf) => {
                if !cx.tslib_syntax() {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            pf.loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::PrivateClassField,
                            ),
                        ))),
                    );
                }
                // Private fields are intentionally ignored — they are not part of the
                // public interface, so skipping them is correct behavior.
                let Ok(error_prop) = polymorphic_ast_mapper::object_type_property(
                    &mut typed_ast_utils::UncheckedMapper,
                    prop,
                );
                prop_asts.push(error_prop);
            }
        }
    }
    (s, prop_asts)
}

fn optional_indexed_access<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    loc: ALoc,
    ia: &ast::types::OptionalIndexedAccess<ALoc, ALoc>,
) -> (Type, ast::types::Type<ALoc, (ALoc, Type)>) {
    let reason = reason::mk_reason(
        reason::VirtualReasonDesc::RIndexedAccess {
            optional: ia.optional,
        },
        loc.dupe(),
    );
    let _object = &ia.indexed_access.object;
    let index = &ia.indexed_access.index;
    let comments = &ia.indexed_access.comments;
    let index_ast = convert_inner(cx, env, index);
    let (_, index_type) = index_ast.loc();
    let index_type = index_type.dupe();
    let index_reason = type_util::reason_of_t(&index_type).dupe();
    let (object_t, object_ast) = match _object.deref() {
        ast::types::TypeInner::OptionalIndexedAccess {
            loc: obj_loc,
            inner,
        } => optional_indexed_access(cx, env, obj_loc.dupe(), inner),
        _ => {
            let object_ast = convert_inner(cx, env, _object);
            let (_, object_t) = object_ast.loc();
            let object_t = object_t.dupe();
            (object_t, object_ast)
        }
    };
    let lhs_reason = type_util::reason_of_t(&object_t).dupe();
    let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::IndexedTypeAccess {
        object: lhs_reason.dupe(),
        index: index_reason,
    }));
    let non_maybe_destructor = match index_ast.deref() {
        ast::types::TypeInner::StringLiteral { literal, .. } => {
            let name = Name::new(literal.value.dupe());
            if ia.optional {
                type_::Destructor::OptionalIndexedAccessNonMaybeType {
                    index: type_::OptionalIndexedAccessIndex::OptionalIndexedAccessStrLitIndex(
                        name,
                    ),
                }
            } else {
                type_::Destructor::PropertyType { name }
            }
        }
        _ => {
            if ia.optional {
                type_::Destructor::OptionalIndexedAccessNonMaybeType {
                    index: type_::OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(
                        index_type.dupe(),
                    ),
                }
            } else {
                type_::Destructor::ElementType {
                    index_type: index_type.dupe(),
                }
            }
        }
    };
    let non_maybe_result_t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
        cx,
        use_op.dupe(),
        &reason,
        &object_t,
        &non_maybe_destructor,
        mk_eval_id(cx, loc.dupe()),
    )
    .expect("Should not be under speculation");
    let void_reason = lhs_reason.replace_desc(reason::VirtualReasonDesc::RVoid);
    let result_t = FlowJs::mk_possibly_evaluated_destructor_for_annotations(
        cx,
        type_::unknown_use(),
        &reason,
        &non_maybe_result_t,
        &type_::Destructor::OptionalIndexedAccessResultType { void_reason },
        type_::eval::Id::generate_id(),
    )
    .expect("Should not be under speculation");
    (
        non_maybe_result_t,
        ast::types::Type::new(ast::types::TypeInner::OptionalIndexedAccess {
            loc: (loc, result_t),
            inner: (ast::types::OptionalIndexedAccess {
                indexed_access: ast::types::IndexedAccess {
                    object: object_ast,
                    index: index_ast,
                    comments: comments.clone(),
                },
                optional: ia.optional,
            })
            .into(),
        }),
    )
}

fn mk_component<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    reason: Reason,
    id_opt: Option<&ast::Identifier<ALoc, ALoc>>,
    tparams_node: Option<&ast::types::TypeParams<ALoc, ALoc>>,
    params: &ast::types::component_params::Params<ALoc, ALoc>,
    renders: &ast::types::ComponentRendersAnnotation<ALoc, ALoc>,
) -> (
    Type,
    Option<ast::types::TypeParams<ALoc, (ALoc, Type)>>,
    ast::types::component_params::Params<ALoc, (ALoc, Type)>,
    ast::types::ComponentRendersAnnotation<ALoc, (ALoc, Type)>,
) {
    let saved_tparams_map = env.tparams_map.dupe();
    let (tparams, tparam_asts) = mk_type_param_declarations_inner(
        cx,
        env,
        flow_parser::ast_visitor::TypeParamsContext::ComponentDeclaration,
        tparams_node,
    );
    let mk_param = |cx_ref,
                    env_ref: &mut ConvertEnv,
                    param: &ast::types::component_params::Param<ALoc, ALoc>|
     -> (
        Type,
        ast::types::component_params::Param<ALoc, (ALoc, Type)>,
    ) {
        let (t, annot_ast) = mk_type_available_annotation_inner(cx_ref, env_ref, &param.annot);
        use flow_parser::ast::statement::component_params::ParamName;
        let name_ast = match &param.name {
            ParamName::StringLiteral((l, n)) => ParamName::StringLiteral((l.dupe(), n.clone())),
            ParamName::Identifier(ident) => {
                ParamName::Identifier(ast::Identifier::new(ast::IdentifierInner {
                    loc: (ident.loc.dupe(), t.dupe()),
                    name: ident.name.dupe(),
                    comments: ident.comments.dupe(),
                }))
            }
        };
        (
            t.dupe(),
            ast::types::component_params::Param {
                loc: (param.loc.dupe(), t),
                name: name_ast,
                annot: annot_ast,
                optional: param.optional,
            },
        )
    };
    let mk_rest = |cx_ref,
                   env_ref: &mut ConvertEnv,
                   rest: &ast::types::component_params::RestParam<ALoc, ALoc>|
     -> (
        Type,
        ast::types::component_params::RestParam<ALoc, (ALoc, Type)>,
    ) {
        let annot_ast = convert_inner(cx_ref, env_ref, &rest.annot);
        let (_, t) = annot_ast.loc();
        let t = t.dupe();
        let argument_ast = rest.argument.as_ref().map(|arg| {
            ast::Identifier::new(ast::IdentifierInner {
                loc: (arg.loc.dupe(), t.dupe()),
                name: arg.name.dupe(),
                comments: arg.comments.dupe(),
            })
        });
        (
            t.dupe(),
            ast::types::component_params::RestParam {
                loc: (rest.loc.dupe(), t),
                argument: argument_ast,
                annot: annot_ast,
                optional: rest.optional,
                comments: rest.comments.dupe(),
            },
        )
    };
    let mut typed_params = Vec::new();
    let mut param_types: Vec<(
        Type,
        ast::types::component_params::Param<ALoc, (ALoc, Type)>,
    )> = Vec::new();
    for p in params.params.iter() {
        let (t, param_ast) = mk_param(cx, env, p);
        param_types.push((t, param_ast.clone()));
        typed_params.push(param_ast);
    }
    let mut typed_rest = None;
    let mut rest_type_opt: Option<(
        Type,
        ast::types::component_params::RestParam<ALoc, (ALoc, Type)>,
    )> = None;
    if let Some(ref r) = params.rest {
        let (t, rest_ast) = mk_rest(cx, env, r);
        rest_type_opt = Some((t, rest_ast.clone()));
        typed_rest = Some(rest_ast);
    }
    let params_ast = ast::types::component_params::Params {
        loc: {
            let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(&params.loc);
            v
        },
        params: typed_params.into(),
        rest: typed_rest,
        comments: params.comments.dupe(),
    };
    let (_ren_loc, renders_t, renders_ast) = match renders {
        ast::types::ComponentRendersAnnotation::AvailableRenders(loc, annot) => {
            let (t, renders_ast) = convert_render_type_inner(cx, env, loc.dupe(), annot);
            (
                loc.dupe(),
                t,
                ast::types::ComponentRendersAnnotation::AvailableRenders(loc.dupe(), renders_ast),
            )
        }
        ast::types::ComponentRendersAnnotation::MissingRenders(loc) => {
            let reason = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RRenderType(Arc::new(reason::VirtualReasonDesc::RType(
                    Name::new(FlowSmolStr::new("React.Node")),
                ))),
                loc.dupe(),
            );
            let renders_t = Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::RendersT(Rc::new(
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
    env.tparams_map = saved_tparams_map;
    let t = crate::component_sig::component_type::<TypeAnnotationConfig>(
        cx,
        id_opt.is_none(),
        &reason,
        tparams,
        &param_types,
        rest_type_opt.as_ref(),
        renders_t,
        id_opt.map(|id| (&id.loc, &id.name)),
    );
    (t, tparam_asts, params_ast, renders_ast)
}

fn mk_super_inner<'a>(
    cx: &Context<'a>,
    env: &mut ConvertEnv,
    loc: ALoc,
    c: Type,
    targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
) -> (
    (ALoc, Type, Option<Vec<Type>>),
    Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>,
) {
    match targs {
        None => ((loc, c, None), None),
        Some(targs_node) => {
            let (ts, targs_ast) = convert_list_inner(cx, env, &targs_node.arguments);
            (
                (loc, c, Some(ts)),
                Some(ast::types::TypeArgs {
                    loc: {
                        let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(&targs_node.loc);
                        v
                    },
                    arguments: targs_ast.into(),
                    comments: targs_node.comments.dupe(),
                }),
            )
        }
    }
}

pub fn mk_interface_sig<'a>(
    cx: &Context<'a>,
    intf_loc: ALoc,
    reason: Reason,
    decl: &ast::statement::Interface<ALoc, ALoc>,
) -> (
    Type,
    func_class_sig_types::class::Class<FuncTypeParamsConfig>,
    ast::statement::Interface<ALoc, (ALoc, Type)>,
) {
    let id_loc = &decl.id.loc;
    let id_name = &*decl.id;
    let (ref body_loc, ref body) = decl.body;
    let mut env = ConvertEnv::new(None, None, None, FlowOrdMap::new());
    let (tparams, tparams_ast) = mk_type_param_declarations_inner(
        cx,
        &mut env,
        flow_parser::ast_visitor::TypeParamsContext::Interface,
        decl.tparams.as_ref(),
    );
    let class_name = &id_name.name;
    let id = cx.make_aloc_id(id_loc);
    let mut extends = Vec::new();
    let mut extends_ast = Vec::new();
    for ext in decl.extends.iter() {
        let (typeapp, ext_ast) = mk_interface_super(cx, &mut env, ext);
        extends.push(typeapp);
        extends_ast.push(ext_ast);
    }
    let callable = body.properties.iter().any(|prop| match prop {
        ast::types::object::Property::CallProperty(cp) => !cp.static_,
        _ => false,
    });
    let super_ = func_class_sig_types::class::Super::Interface(
        func_class_sig_types::class::InterfaceSuper {
            inline: false,
            extends,
            callable,
        },
    );
    let mut iface_sig = class_sig::empty(
        id,
        Some(class_name.dupe()),
        intf_loc.dupe(),
        reason.dupe(),
        tparams,
        env.tparams_map
            .iter()
            .map(|(k, v)| (k.dupe(), v.dupe()))
            .collect(),
        super_,
    );
    // TODO: interfaces don't have a name field, or even statics
    class_sig::add_name_field(&mut iface_sig);
    let this = type_::implicit_mixed_this(reason.dupe());
    let (iface_sig, properties_ast) = add_interface_properties(
        cx,
        &mut env,
        Some(id_loc.dupe()),
        intermediate_error_types::ObjKind::Interface,
        this,
        &body.properties,
        iface_sig,
    );
    let (_t_internal, t, (own_props, proto_props)) = class_sig::classtype(
        cx,
        true,
        type_::InstanceKind::InterfaceKind { inline: false },
        &iface_sig,
    );
    cx.add_interface_prop_ids(id_loc.dupe(), own_props, proto_props);
    (
        t.dupe(),
        iface_sig,
        ast::statement::Interface {
            id: ast::Identifier::new(ast::IdentifierInner {
                loc: (id_loc.dupe(), t),
                name: id_name.name.dupe(),
                comments: id_name.comments.dupe(),
            }),
            tparams: tparams_ast,
            extends: extends_ast.into(),
            body: (
                body_loc.dupe(),
                ast::types::Object {
                    exact: body.exact,
                    inexact: false,
                    properties: properties_ast.into(),
                    comments: body.comments.dupe(),
                },
            ),
            comments: decl.comments.dupe(),
        },
    )
}

pub fn mk_declare_component_sig<'a>(
    cx: &Context<'a>,
    loc: ALoc,
    component: &ast::statement::DeclareComponent<ALoc, ALoc>,
) -> (Type, ast::statement::DeclareComponent<ALoc, (ALoc, Type)>) {
    let id_loc = &component.id.loc;
    let id_name = &*component.id;
    let name = &id_name.name;
    let reason = reason::mk_reason(
        reason::VirtualReasonDesc::RComponent(Name::new(name.dupe())),
        loc.dupe(),
    );
    let mut env = ConvertEnv::new(None, None, None, FlowOrdMap::new());
    let (tparams, tparam_asts) = mk_type_param_declarations_inner(
        cx,
        &mut env,
        flow_parser::ast_visitor::TypeParamsContext::DeclareComponent,
        component.tparams.as_ref(),
    );
    let params_loc = &component.params.loc;
    let params_comments = &component.params.comments;

    // Helper to convert annotation_or_hint to annotation
    fn convert_annot_or_hint<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        _loc: &ALoc,
        annot_or_hint: &ast::types::AnnotationOrHint<ALoc, ALoc>,
    ) -> (Type, ast::types::AnnotationOrHint<ALoc, (ALoc, Type)>) {
        match annot_or_hint {
            ast::types::AnnotationOrHint::Available(annot) => {
                let (t, annot_ast) = mk_type_available_annotation_inner(cx, env, annot);
                (t, ast::types::AnnotationOrHint::Available(annot_ast))
            }
            ast::types::AnnotationOrHint::Missing(hint_loc) => {
                let t = type_::any_t::error(reason::mk_reason(
                    reason::VirtualReasonDesc::RAnyImplicit,
                    hint_loc.dupe(),
                ));
                (
                    t.dupe(),
                    ast::types::AnnotationOrHint::Missing((hint_loc.dupe(), t)),
                )
            }
        }
    }
    // Process each param: convert statement param to type param for Component_type_params,
    // and build typed statement param for output
    fn process_param<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        param: &ast::statement::component_params::Param<ALoc, ALoc>,
    ) -> (
        Type,
        ast::types::component_params::Param<ALoc, (ALoc, Type)>,
        ast::statement::component_params::Param<ALoc, (ALoc, Type)>,
    ) {
        use flow_parser::ast::statement::component_params::ParamName;

        let p_loc = &param.loc;
        let param_name = &param.name;
        let local = &param.local;
        let default = &param.default;
        let shorthand = param.shorthand;

        match local {
            ast::pattern::Pattern::Identifier {
                inner: id_inner,
                loc: local_loc,
                ..
            } => {
                let local_id = &id_inner.name;
                let local_id_loc = &local_id.loc;
                let annot = &id_inner.annot;
                let optional = id_inner.optional;
                let (t, typed_annot) = convert_annot_or_hint(cx, env, p_loc, annot);
                // Build typed param_name for statement params
                let typed_stmt_name = match param_name {
                    ParamName::StringLiteral(sl) => ParamName::StringLiteral(sl.clone()),
                    ParamName::Identifier(ident) => {
                        ParamName::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (ident.loc.dupe(), t.dupe()),
                            name: ident.name.dupe(),
                            comments: ident.comments.dupe(),
                        }))
                    }
                };
                // Build typed statement param
                let typed_local = ast::pattern::Pattern::Identifier {
                    loc: (local_loc.dupe(), t.dupe()),
                    inner: (ast::pattern::Identifier {
                        name: ast::Identifier::new(ast::IdentifierInner {
                            loc: (local_id_loc.dupe(), t.dupe()),
                            name: local_id.name.dupe(),
                            comments: local_id.comments.dupe(),
                        }),
                        annot: typed_annot.clone(),
                        optional,
                    })
                    .into(),
                };
                let typed_default = default.as_ref().map(|d| {
                    let Ok(v) =
                        polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, d);
                    v
                });
                let typed_param = ast::statement::component_params::Param {
                    loc: (p_loc.dupe(), t.dupe()),
                    name: typed_stmt_name,
                    local: typed_local,
                    default: typed_default,
                    shorthand,
                };
                // Build type component param for Component_type_params
                let typed_tc_name = match param_name {
                    ParamName::StringLiteral(sl) => ParamName::StringLiteral(sl.clone()),
                    ParamName::Identifier(ident) => {
                        ParamName::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (ident.loc.dupe(), t.dupe()),
                            name: ident.name.dupe(),
                            comments: ident.comments.dupe(),
                        }))
                    }
                };
                let type_param_annot = match &typed_annot {
                    ast::types::AnnotationOrHint::Available(annot) => annot.clone(),
                    ast::types::AnnotationOrHint::Missing((hint_loc, hint_t)) => {
                        ast::types::Annotation {
                            loc: hint_loc.dupe(),
                            annotation: ast::types::Type::new(ast::types::TypeInner::Any {
                                loc: (hint_loc.dupe(), hint_t.dupe()),
                                comments: None,
                            }),
                        }
                    }
                };
                let type_param = ast::types::component_params::Param {
                    loc: (p_loc.dupe(), t.dupe()),
                    name: typed_tc_name,
                    annot: type_param_annot,
                    optional,
                };
                (t, type_param, typed_param)
            }
            _ => {
                // Non-identifier patterns (e.g., object/array destructuring via `as`) are errors,
                // but we still need to produce something for the typed AST
                let t = type_::any_t::error(reason::mk_reason(
                    reason::VirtualReasonDesc::RAnyImplicit,
                    p_loc.dupe(),
                ));
                let typed_local = {
                    let Ok(v) = polymorphic_ast_mapper::pattern(
                        &mut typed_ast_utils::ErrorMapper,
                        None,
                        local,
                    );
                    v
                };
                let typed_default = default.as_ref().map(|d| {
                    let Ok(v) =
                        polymorphic_ast_mapper::expression(&mut typed_ast_utils::ErrorMapper, d);
                    v
                });
                // Build typed param_name for statement params
                let typed_stmt_name = match param_name {
                    ParamName::StringLiteral(sl) => ParamName::StringLiteral(sl.clone()),
                    ParamName::Identifier(ident) => {
                        ParamName::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (ident.loc.dupe(), t.dupe()),
                            name: ident.name.dupe(),
                            comments: ident.comments.dupe(),
                        }))
                    }
                };
                let typed_param = ast::statement::component_params::Param {
                    loc: (p_loc.dupe(), t.dupe()),
                    name: typed_stmt_name,
                    local: typed_local,
                    default: typed_default,
                    shorthand,
                };
                // Build type component param for Component_type_params
                let typed_tc_name = match param_name {
                    ParamName::StringLiteral(sl) => ParamName::StringLiteral(sl.clone()),
                    ParamName::Identifier(ident) => {
                        ParamName::Identifier(ast::Identifier::new(ast::IdentifierInner {
                            loc: (ident.loc.dupe(), t.dupe()),
                            name: ident.name.dupe(),
                            comments: ident.comments.dupe(),
                        }))
                    }
                };
                let type_param = ast::types::component_params::Param {
                    loc: (p_loc.dupe(), t.dupe()),
                    name: typed_tc_name,
                    annot: ast::types::Annotation {
                        loc: p_loc.dupe(),
                        annotation: ast::types::Type::new(ast::types::TypeInner::Any {
                            loc: (p_loc.dupe(), t.dupe()),
                            comments: None,
                        }),
                    },
                    optional: false,
                };
                (t, type_param, typed_param)
            }
        }
    }

    // Process rest param
    fn process_rest<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        rest_param: &ast::statement::component_params::RestParam<ALoc, ALoc>,
    ) -> (
        Type,
        ast::types::component_params::RestParam<ALoc, (ALoc, Type)>,
        ast::statement::component_params::RestParam<ALoc, (ALoc, Type)>,
    ) {
        let r_loc = &rest_param.loc;
        let argument = &rest_param.argument;
        let rest_comments = &rest_param.comments;
        match argument {
            ast::pattern::Pattern::Identifier {
                inner: id_inner,
                loc: arg_loc,
                ..
            } => {
                let arg_id = &id_inner.name;
                let arg_id_loc = &arg_id.loc;
                let annot = &id_inner.annot;
                let optional = id_inner.optional;
                let (t, typed_annot) = convert_annot_or_hint(cx, env, r_loc, annot);
                // Build typed statement rest param
                let typed_argument = ast::pattern::Pattern::Identifier {
                    loc: (arg_loc.dupe(), t.dupe()),
                    inner: ast::pattern::Identifier {
                        name: ast::Identifier::new(ast::IdentifierInner {
                            loc: (arg_id_loc.dupe(), t.dupe()),
                            name: arg_id.name.dupe(),
                            comments: arg_id.comments.dupe(),
                        }),
                        annot: typed_annot.clone(),
                        optional,
                    }
                    .into(),
                };
                let typed_rest = ast::statement::component_params::RestParam {
                    loc: (r_loc.dupe(), t.dupe()),
                    argument: typed_argument,
                    comments: rest_comments.dupe(),
                };
                // Build type component rest param for Component_type_params
                let type_rest_annot = match &typed_annot {
                    ast::types::AnnotationOrHint::Available(annot) => annot.annotation.clone(),
                    ast::types::AnnotationOrHint::Missing((hint_loc, hint_t)) => {
                        ast::types::Type::new(ast::types::TypeInner::Any {
                            loc: (hint_loc.dupe(), hint_t.dupe()),
                            comments: None,
                        })
                    }
                };
                let type_rest = ast::types::component_params::RestParam {
                    loc: (r_loc.dupe(), t.dupe()),
                    argument: Some(ast::Identifier::new(ast::IdentifierInner {
                        loc: (arg_id_loc.dupe(), t.dupe()),
                        name: arg_id.name.dupe(),
                        comments: arg_id.comments.dupe(),
                    })),
                    annot: type_rest_annot,
                    optional,
                    comments: rest_comments.clone(),
                };
                (t, type_rest, typed_rest)
            }
            _ => {
                // Non-identifier patterns are errors, produce error typed AST
                let t = type_::any_t::error(reason::mk_reason(
                    reason::VirtualReasonDesc::RAnyImplicit,
                    r_loc.dupe(),
                ));
                let Ok(typed_argument) = polymorphic_ast_mapper::pattern(
                    &mut typed_ast_utils::ErrorMapper,
                    None,
                    argument,
                );
                let typed_rest = ast::statement::component_params::RestParam {
                    loc: (r_loc.dupe(), t.dupe()),
                    argument: typed_argument,
                    comments: rest_comments.clone(),
                };
                let type_rest = ast::types::component_params::RestParam {
                    loc: (r_loc.dupe(), t.dupe()),
                    argument: None,
                    annot: ast::types::Type::new(ast::types::TypeInner::Any {
                        loc: (r_loc.dupe(), t.dupe()),
                        comments: None,
                    }),
                    optional: false,
                    comments: rest_comments.clone(),
                };
                (t, type_rest, typed_rest)
            }
        }
    }
    // Process all params
    let processed_params: Vec<_> = component
        .params
        .params
        .iter()
        .map(|param| process_param(cx, &mut env, param))
        .collect();
    let processed_rest = component
        .params
        .rest
        .as_ref()
        .map(|rest| process_rest(cx, &mut env, rest));
    // Build Component_type_params
    let type_params: Vec<_> = processed_params
        .iter()
        .map(|(t, tp, _)| (t.dupe(), tp.clone()))
        .collect();
    let type_rest_opt = processed_rest
        .as_ref()
        .map(|(t, tr, _)| (t.dupe(), tr.clone()));
    let typed_params = {
        let stmt_params: Vec<_> = processed_params.into_iter().map(|(_, _, sp)| sp).collect();
        let stmt_rest = processed_rest.map(|(_, _, sr)| sr);
        ast::statement::component_params::Params {
            loc: {
                let Ok(v) = typed_ast_utils::ErrorMapper.on_type_annot(params_loc);
                v
            },
            params: stmt_params.into(),
            rest: stmt_rest,
            comments: params_comments.clone(),
        }
    };
    // Process renders
    let (_ren_loc, renders_t, renders_ast) = match &component.renders {
        ast::types::ComponentRendersAnnotation::AvailableRenders(r_loc, annot) => {
            let (t, r_ast) = convert_render_type_inner(cx, &mut env, r_loc.dupe(), annot);
            (
                r_loc.dupe(),
                t,
                ast::types::ComponentRendersAnnotation::AvailableRenders(r_loc.dupe(), r_ast),
            )
        }
        ast::types::ComponentRendersAnnotation::MissingRenders(r_loc) => {
            let r = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RRenderType(Arc::new(reason::VirtualReasonDesc::RType(
                    Name::new(FlowSmolStr::new("React.Node")),
                ))),
                r_loc.dupe(),
            );
            let renders_t = Type::new(type_::TypeInner::DefT(
                r,
                type_::DefT::new(type_::DefTInner::RendersT(Rc::new(
                    type_::CanonicalRendersForm::DefaultRenders,
                ))),
            ));
            (
                r_loc.dupe(),
                renders_t.dupe(),
                ast::types::ComponentRendersAnnotation::MissingRenders((r_loc.dupe(), renders_t)),
            )
        }
    };
    let t = crate::component_sig::component_type::<TypeAnnotationConfig>(
        cx,
        false,
        &reason,
        tparams,
        &type_params,
        type_rest_opt.as_ref(),
        renders_t,
        Some((id_loc, name)),
    );
    (
        t.dupe(),
        ast::statement::DeclareComponent {
            tparams: tparam_asts,
            params: typed_params,
            id: ast::Identifier::new(ast::IdentifierInner {
                loc: (id_loc.dupe(), t),
                name: id_name.name.dupe(),
                comments: id_name.comments.dupe(),
            }),
            renders: renders_ast,
            comments: component.comments.dupe(),
        },
    )
}

pub fn mk_declare_class_sig<'a>(
    cx: &Context<'a>,
    class_loc: ALoc,
    class_name: &FlowSmolStr,
    reason: Reason,
    decl: &ast::statement::DeclareClass<ALoc, ALoc>,
) -> (
    Type,
    func_class_sig_types::class::Class<FuncTypeParamsConfig>,
    ast::statement::DeclareClass<ALoc, (ALoc, Type)>,
) {
    use flow_typing_type::type_::*;

    fn mk_mixins<'a>(
        cx: &Context<'a>,
        env: &mut ConvertEnv,
        generic_with_loc: &(ALoc, ast::types::Generic<ALoc, ALoc>),
    ) -> (
        (ALoc, Type, Option<Vec<Type>>),
        (ALoc, ast::types::Generic<ALoc, (ALoc, Type)>),
    ) {
        let (loc, generic) = generic_with_loc;
        let loc = loc.dupe();
        let name = qualified_name(&generic.id);
        let r = reason::mk_annot_reason(
            reason::VirtualReasonDesc::RType(Name::new(FlowSmolStr::new(&name))),
            loc.dupe(),
        );
        let (i, id_ast) = convert_qualification_with_lookup_mode(
            cx,
            Some(type_env::LookupMode::ForValue),
            "mixins",
            &generic.id,
        );
        let props_bag = crate::type_annotation_cons_gen::mixin(cx, r, i);
        let (t, targs_ast) = mk_super_inner(cx, env, loc.dupe(), props_bag, generic.targs.as_ref());
        (
            t,
            (
                loc.dupe(),
                ast::types::Generic {
                    id: id_ast,
                    targs: targs_ast,
                    comments: generic.comments.dupe(),
                },
            ),
        )
    }

    fn is_object_builtin_libdef(loc: &ALoc, name: &str) -> bool {
        name == "Object"
            && match loc.source() {
                None => false,
                Some(source) => source.is_lib_file(),
            }
    }

    let reason_c = reason.dupe();
    let class_loc_c = class_loc.dupe();
    let class_name_owned = class_name.dupe();
    let decl_clone = decl.clone();

    use std::cell::RefCell;
    use std::rc::Rc;

    type LazyResult = (
        Type,
        Type,
        func_class_sig_types::class::Class<FuncTypeParamsConfig>,
        ast::statement::DeclareClass<ALoc, (ALoc, Type)>,
    );
    let lazy_cell: Rc<
        RefCell<
            Option<
                Rc<
                    flow_lazy::Lazy<
                        Context<'a>,
                        LazyResult,
                        Box<dyn FnOnce(&Context<'a>) -> LazyResult + 'a>,
                    >,
                >,
            >,
        >,
    > = Rc::new(RefCell::new(None));
    let lazy_cell_c = lazy_cell.dupe();

    let lazy_val: Rc<
        flow_lazy::Lazy<Context<'a>, LazyResult, Box<dyn FnOnce(&Context<'a>) -> LazyResult + 'a>>,
    > = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'_>| {
        let reason = reason_c;
        let class_loc = class_loc_c;
        let class_name = &class_name_owned;
        let decl = &decl_clone;

        // Build self type via lazy reference
        let lazy_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
        let self_ = flow_typing_tvar::mk_fully_resolved_lazy(
            cx,
            reason.dupe(),
            true,
            Box::new(move |cx: &Context<'_>| {
                let val = lazy_ref.get_forced(cx);
                val.0.dupe()
            }),
        );

        // Now implement f:
        let id_loc = &decl.id.loc;
        let id_name = &*decl.id;
        let mut env = ConvertEnv::new(None, None, None, FlowOrdMap::new());
        let (tparams, tparam_asts) = mk_type_param_declarations_inner(
            cx,
            &mut env,
            flow_parser::ast_visitor::TypeParamsContext::DeclareClass,
            decl.tparams.as_ref(),
        );
        let (this_tparam, this_t) = class_sig::mk_this(self_, cx, reason.dupe());
        let id = cx.make_aloc_id(id_loc);
        let (extends, extends_ast) = {
            use ast::statement::DeclareClassExtends;
            match &decl.extends {
                Some((loc, DeclareClassExtends::ExtendsIdent(generic))) => {
                    let (i, id_ast) = convert_qualification_with_lookup_mode(
                        cx,
                        Some(type_env::LookupMode::ForValue),
                        "mixins",
                        &generic.id,
                    );
                    let (t, targs_ast) =
                        mk_super_inner(cx, &mut env, loc.dupe(), i, generic.targs.as_ref());
                    (
                        Some(t),
                        Some((
                            loc.dupe(),
                            DeclareClassExtends::ExtendsIdent(ast::types::Generic {
                                id: id_ast,
                                targs: targs_ast,
                                comments: generic.comments.dupe(),
                            }),
                        )),
                    )
                }
                Some((loc, ext @ DeclareClassExtends::ExtendsCall { .. })) => {
                    flow_js_utils::add_output_non_speculating(
                        cx,
                        ErrorMessage::EUnsupportedSyntax(Box::new((
                            loc.dupe(),
                            intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                TsLibSyntaxKind::ClassExtendsCall,
                            ),
                        ))),
                    );
                    let Ok(ext_ast) = polymorphic_ast_mapper::declare_class_extends(
                        &mut typed_ast_utils::ErrorMapper,
                        ext,
                    );
                    (None, Some((loc.dupe(), ext_ast)))
                }
                None => (None, None),
            }
        };
        let (mixins_list, mixins_ast_list): (Vec<_>, Vec<_>) = decl
            .mixins
            .iter()
            .map(|m| mk_mixins(cx, &mut env, m))
            .unzip();
        let (implements_list, implements_ast) = match &decl.implements {
            None => (Vec::new(), None),
            Some(impls) => {
                let (impl_list, ifaces_ast): (Vec<_>, Vec<_>) = impls
                    .interfaces
                    .iter()
                    .map(|iface| {
                        let id = &iface.id;
                        match id {
                            ast::types::generic::Identifier::Qualified(_)
                            | ast::types::generic::Identifier::ImportTypeAnnot(_)
                                if !cx.tslib_syntax() =>
                            {
                                flow_js_utils::add_output_non_speculating(
                                    cx,
                                    ErrorMessage::EUnsupportedSyntax(Box::new((
                                        iface.loc.dupe(),
                                        intermediate_error_types::UnsupportedSyntax::TSLibSyntax(
                                            TsLibSyntaxKind::ImplementsDottedPath,
                                        ),
                                    ))),
                                );
                            }
                            _ => {}
                        }
                        let (c, id) = convert_qualification(cx, "implements", id);
                        let (typeapp, targs_ast) = match &iface.targs {
                            None => ((iface.loc.dupe(), c.dupe(), None), None),
                            Some(targs) => {
                                let (ts, targs_ast) =
                                    convert_list_inner(cx, &mut env, &targs.arguments);
                                (
                                    (iface.loc.dupe(), c.dupe(), Some(ts)),
                                    Some(ast::types::TypeArgs {
                                        loc: {
                                            let Ok(v) = typed_ast_utils::ErrorMapper
                                                .on_type_annot(&targs.loc);
                                            v
                                        },
                                        arguments: targs_ast.into(),
                                        comments: targs.comments.dupe(),
                                    }),
                                )
                            }
                        };
                        (
                            typeapp,
                            ast::class::implements::Interface {
                                loc: iface.loc.dupe(),
                                id,
                                targs: targs_ast,
                            },
                        )
                    })
                    .unzip();
                (
                    impl_list,
                    Some(ast::class::Implements {
                        loc: impls.loc.dupe(),
                        interfaces: ifaces_ast.into(),
                        comments: impls.comments.dupe(),
                    }),
                )
            }
        };
        let super_kind = {
            let extends_kind = match extends {
                None => func_class_sig_types::class::Extends::Implicit {
                    null: is_object_builtin_libdef(id_loc, &id_name.name),
                },
                Some(e) => func_class_sig_types::class::Extends::Explicit(e),
            };
            func_class_sig_types::class::Super::Class(func_class_sig_types::class::ClassSuper {
                extends: extends_kind,
                mixins: mixins_list,
                implements: implements_list,
                this_t: this_t.dupe(),
                this_tparam,
            })
        };
        let mut iface_sig = class_sig::empty(
            id,
            Some(class_name.dupe()),
            class_loc.dupe(),
            reason.dupe(),
            tparams,
            env.tparams_map
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect(),
            super_kind,
        );
        // All classes have a static "name" property.
        class_sig::add_name_field(&mut iface_sig);
        let tparams_map_with_this = {
            let mut m = env.tparams_map.dupe();
            m.insert(SubstName::name(FlowSmolStr::new("this")), this_t.dupe());
            m
        };
        let mut env_with_this = env.clone();
        env_with_this.tparams_map = tparams_map_with_this;
        let this_type = type_::implicit_mixed_this(type_util::reason_of_t(&this_t).dupe());
        let (ref body_loc, ref body) = decl.body;
        let (updated_sig, properties_typed) = add_interface_properties(
            cx,
            &mut env_with_this,
            None,
            intermediate_error_types::ObjKind::DeclareClass,
            this_type,
            &body.properties,
            iface_sig,
        );
        iface_sig = updated_sig;
        let body_props = properties_typed;

        // Add a default ctor if we don't have a ctor and won't inherit one from a super
        if !class_sig::mem_constructor(&iface_sig)
            && decl.extends.is_none()
            && decl.mixins.is_empty()
        {
            let ctor_reason = reason.replace_desc(reason::VirtualReasonDesc::RDefaultConstructor);
            class_sig::add_default_constructor(ctor_reason, &mut iface_sig);
        }

        let (t_internal, t, _) =
            class_sig::classtype(cx, true, type_::InstanceKind::ClassKind, &iface_sig);

        let tast = ast::statement::DeclareClass {
            id: ast::Identifier::new(ast::IdentifierInner {
                loc: (id_loc.dupe(), t.dupe()),
                name: id_name.name.dupe(),
                comments: id_name.comments.dupe(),
            }),
            tparams: tparam_asts,
            body: (
                body_loc.dupe(),
                ast::types::Object {
                    exact: body.exact,
                    inexact: false,
                    properties: body_props.into(),
                    comments: body.comments.dupe(),
                },
            ),
            extends: extends_ast,
            mixins: mixins_ast_list.into(),
            implements: implements_ast,
            abstract_: decl.abstract_,
            comments: decl.comments.dupe(),
        };

        (t_internal, t, iface_sig, tast)
    })));

    *lazy_cell.borrow_mut() = Some(lazy_val.dupe());

    let result = lazy_val.get_forced(cx);
    (result.1.dupe(), result.2.clone(), result.3.clone())
}

// =========================================================================
// Public wrapper functions that create env and delegate
// =========================================================================

pub fn convert<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    t: &ast::types::Type<ALoc, ALoc>,
) -> ast::types::Type<ALoc, (ALoc, Type)> {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    convert_inner(cx, &mut env, t)
}

pub fn convert_list<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    asts: &[ast::types::Type<ALoc, ALoc>],
) -> (Vec<Type>, Vec<ast::types::Type<ALoc, (ALoc, Type)>>) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    convert_list_inner(cx, &mut env, asts)
}

pub fn convert_render_type<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    loc: ALoc,
    renders: &ast::types::Renders<ALoc, ALoc>,
) -> (Type, ast::types::Renders<ALoc, (ALoc, Type)>) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    convert_render_type_inner(cx, &mut env, loc, renders)
}

pub fn convert_type_guard<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    fparams: &[type_::FunParam],
    gloc: ALoc,
    kind: ast::types::TypeGuardKind,
    id_name: &ast::Identifier<ALoc, ALoc>,
    t: &ast::types::Type<ALoc, ALoc>,
    comments: Option<&ast::Syntax<ALoc, std::sync::Arc<[ast::Comment<ALoc>]>>>,
) -> (
    Type,
    ast::types::TypeGuard<ALoc, (ALoc, Type)>,
    Option<type_::TypeGuard>,
) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    convert_type_guard_inner(cx, &mut env, fparams, gloc, kind, id_name, t, comments)
}

pub fn convert_indexer(
    cx: &Context,
    tparams_map: &FlowOrdMap<SubstName, Type>,
    indexer: &ast::types::object::Indexer<ALoc, ALoc>,
) -> (
    type_::DictType,
    ast::types::object::Indexer<ALoc, (ALoc, Type)>,
) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map.dupe());
    convert_indexer_internal(cx, &mut env, indexer)
}

pub fn mk_empty_interface_type<'a>(cx: &Context<'a>, loc: ALoc) -> Type {
    let interface_type = ast::types::Type::new(ast::types::TypeInner::Interface {
        loc: loc.dupe(),
        inner: std::sync::Arc::new(ast::types::Interface {
            body: (
                loc.dupe(),
                ast::types::Object {
                    exact: false,
                    inexact: true,
                    properties: Vec::new().into(),
                    comments: None,
                },
            ),
            extends: Vec::new().into(),
            comments: None,
        }),
    });
    let mut env = ConvertEnv::new(None, None, None, FlowOrdMap::default());
    let result = convert_inner(cx, &mut env, &interface_type);
    let (_, t) = result.loc();
    t.dupe()
}

pub fn mk_super<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    loc: ALoc,
    c: Type,
    targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
) -> (
    (ALoc, Type, Option<Vec<Type>>),
    Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>,
) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    mk_super_inner(cx, &mut env, loc, c, targs)
}

pub fn mk_type_available_annotation<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    annotation: &ast::types::Annotation<ALoc, ALoc>,
) -> (Type, ast::types::Annotation<ALoc, (ALoc, Type)>) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    mk_type_available_annotation_inner(cx, &mut env, annotation)
}

pub fn mk_function_type_annotation<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    func: &(ALoc, ast::types::Function<ALoc, ALoc>),
) -> (Type, (ALoc, ast::types::Function<ALoc, (ALoc, Type)>)) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    mk_function_type_annotation_inner(cx, &mut env, func)
}

pub fn mk_nominal_type<'a>(
    cx: &Context<'a>,
    reason: Reason,
    tparams_map: FlowOrdMap<SubstName, Type>,
    c: Type,
    targs: Option<&ast::types::TypeArgs<ALoc, ALoc>>,
) -> (Type, Option<ast::types::TypeArgs<ALoc, (ALoc, Type)>>) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    mk_nominal_type_inner(cx, &mut env, reason, c, targs)
}

pub fn mk_type_param<'a>(
    cx: &Context<'a>,
    tparams_map: FlowOrdMap<SubstName, Type>,
    kind: flow_parser::ast_visitor::TypeParamsContext,
    type_param: &ast::types::TypeParam<ALoc, ALoc>,
) -> (ast::types::TypeParam<ALoc, (ALoc, Type)>, TypeParam, Type) {
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    mk_type_param_inner(cx, &mut env, kind, type_param)
}

pub fn mk_type_param_declarations<'a>(
    cx: &Context<'a>,
    kind: flow_parser::ast_visitor::TypeParamsContext,
    tparams_map: Option<FlowOrdMap<SubstName, Type>>,
    tparams: Option<&ast::types::TypeParams<ALoc, ALoc>>,
) -> (
    type_::TypeParams,
    FlowOrdMap<SubstName, Type>,
    Option<ast::types::TypeParams<ALoc, (ALoc, Type)>>,
) {
    let tparams_map = tparams_map.unwrap_or_default();
    let mut env = ConvertEnv::new(None, None, None, tparams_map);
    let (type_params, tast) = mk_type_param_declarations_inner(cx, &mut env, kind, tparams);
    (type_params, env.tparams_map, tast)
}
