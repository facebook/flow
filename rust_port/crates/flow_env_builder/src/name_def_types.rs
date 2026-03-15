/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::hint::Hint;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReason;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_parser::ast;
use flow_parser::ast_visitor::TypeParamsContext;
use vec1::Vec1;

use crate::env_api::DefLocType;
use crate::env_api::EnvKey;
use crate::env_api::EnvMap;
use crate::env_api::EnvSet;
use crate::selector::Selector;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Ordinary,                 // function or module
    Async,                    // async function
    Generator,                // generator function
    AsyncGenerator,           // async generator function
    Module,                   // module scope
    DeclareModule,            // module scope
    DeclareNamespace,         // namespace scope
    Global,                   // global scope
    Ctor,                     // constructor function
    ComponentOrHookBody,      // component or hook syntax
    AsyncComponentOrHookBody, // async component or hook syntax
}

pub type ClassStack = FlowVector<ALoc>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForKind {
    In,
    Of { await_: bool },
}

/// A map from location of tparam to name.
pub type TparamsMap = FlowOrdMap<ALoc, FlowSmolStr>;

#[derive(Debug, Clone)]
pub enum HintNode {
    AnnotationHint(TparamsMap, ast::types::Annotation<ALoc, ALoc>),
    ValueHint(EnclosingContext, ast::expression::Expression<ALoc, ALoc>),
    ProvidersHint(Vec1<ALoc>),
    WriteLocHint(DefLocType, ALoc),
    StringLiteralType(FlowSmolStr),
    ReactFragmentType,
    ReactNodeType,
    AnyErrorHint(Reason),
    ComposedArrayPatternHint(ALoc, Vec<ArrayElementPatternHint>),
    ComposedObjectPatternHint(ALoc, Vec<ObjectPropPatternHint>),
}

#[derive(Debug, Clone)]
pub enum ArrayElementPatternHint {
    ArrayElementPatternHint(HintNode),
    ArrayRestElementPatternHint(HintNode),
}

#[derive(Debug, Clone)]
pub enum ObjectPropPatternHint {
    ObjectPropPatternHint(FlowSmolStr, ALoc, HintNode),
    ObjectSpreadPropPatternHint(HintNode),
}

pub type AstHint = Hint<
    HintNode,
    Option<ast::expression::CallTypeArgs<ALoc, ALoc>>,
    ast::expression::ArgList<ALoc, ALoc>,
    (
        std::sync::Arc<[ast::jsx::OpeningAttribute<ALoc, ALoc>]>,
        (ALoc, Vec<ast::jsx::Child<ALoc, ALoc>>),
    ),
>;

pub type AstHints = Vec<AstHint>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionSynthKind {
    FunctionSynthesizable,
    MissingReturn(ALoc),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectMissingAnnot {
    FuncMissingAnnot(ALoc),
    OtherMissingAnnot(ALoc),
}

#[derive(Debug, Clone)]
pub enum ObjectSynthKind {
    ObjectSynthesizable {
        /// A set of this write locations that can be resolved by resolving the object.
        this_write_locs: EnvSet<ALoc>,
    },
    MissingMemberAnnots {
        locs: Vec1<ObjectMissingAnnot>,
    },
    Unsynthesizable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DroAnnot {
    Hook,
    Comp,
}

#[derive(Clone)]
pub struct Value {
    pub hints: AstHints,
    pub expr: ast::expression::Expression<ALoc, ALoc>,
    pub decl_kind: Option<ast::VariableKind>,
    pub as_const: bool,
}

#[derive(Clone)]
pub enum Root {
    Annotation {
        tparams_map: TparamsMap,
        optional: bool,
        has_default_expression: bool,
        react_deep_read_only: Option<DroAnnot>,
        param_loc: Option<ALoc>,
        annot: (ALoc, ast::types::Type<ALoc, ALoc>),
        concrete: Option<Box<Root>>,
    },
    Value(Value),
    MatchCaseRoot {
        case_match_root_loc: ALoc,
        root_pattern_loc: ALoc,
        prev_pattern_loc: Option<ALoc>,
    },
    FunctionValue {
        hints: AstHints,
        synthesizable_from_annotation: FunctionSynthKind,
        function_loc: ALoc,
        function_: ast::function::Function<ALoc, ALoc>,
        statics: BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
        arrow: bool,
        tparams_map: TparamsMap,
    },
    ObjectValue {
        synthesizable: ObjectSynthKind,
        obj_loc: ALoc,
        obj: ast::expression::Object<ALoc, ALoc>,
    },
    EmptyArray {
        array_providers: FlowOrdSet<ALoc>,
        arr_loc: ALoc,
    },
    Contextual {
        reason: Reason,
        hints: AstHints,
        optional: bool,
        default_expression: Option<(ALoc, ast::expression::Expression<ALoc, ALoc>)>,
    },
    CatchUnannotated,
    DeclareVariableMissingAnnotationAndInit,
    UnannotatedParameter(Reason),
    For(ForKind, (ALoc, ast::expression::Expression<ALoc, ALoc>)),
}

pub fn mk_value(
    hints: Option<AstHints>,
    decl_kind: Option<ast::VariableKind>,
    as_const: bool,
    expr: ast::expression::Expression<ALoc, ALoc>,
) -> Root {
    Root::Value(Value {
        hints: hints.unwrap_or_default(),
        expr,
        decl_kind,
        as_const,
    })
}

#[derive(Clone)]
pub enum Binding {
    Root(Root),
    Hooklike(Box<Binding>),
    Select {
        selector: Selector<ALoc, ALoc>,
        parent: (ALoc, Box<Binding>),
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Import {
    Named {
        kind: Option<ast::statement::ImportKind>,
        remote: FlowSmolStr,
        local: FlowSmolStr,
    },
    Namespace(FlowSmolStr),
    Default(FlowSmolStr),
}

#[derive(Debug, Clone)]
pub struct GeneratorAnnot {
    pub tparams_map: TparamsMap,
    pub return_annot: (ALoc, ast::types::Type<ALoc, ALoc>),
    pub async_: bool,
}

#[derive(Clone)]
pub struct ExpressionDef {
    pub cond_context: EnclosingContext,
    pub chain: bool,
    pub expr: ast::expression::Expression<ALoc, ALoc>,
    pub hints: AstHints,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassKind {
    Class,
    Record {
        defaulted_props: BTreeSet<FlowSmolStr>,
    },
}

#[derive(Clone)]
pub enum Def {
    Binding(Binding),
    MatchCasePattern {
        case_match_root_loc: ALoc,
        has_guard: bool,
        pattern: (ALoc, ast::match_pattern::MatchPattern<ALoc, ALoc>),
        prev_pattern_loc: Option<ALoc>,
    },
    ExpressionDef(ExpressionDef),
    MemberAssign {
        member_loc: ALoc,
        member: ast::expression::Member<ALoc, ALoc>,
        rhs: (ALoc, ast::expression::Expression<ALoc, ALoc>),
    },
    OpAssign {
        exp_loc: ALoc,
        lhs: (ALoc, ast::pattern::Pattern<ALoc, ALoc>),
        op: ast::expression::AssignmentOperator,
        rhs: (ALoc, ast::expression::Expression<ALoc, ALoc>),
        assertion: bool,
    },
    Update {
        exp_loc: ALoc,
        op: ast::expression::UpdateOperator,
    },
    Function {
        hints: AstHints,
        synthesizable_from_annotation: FunctionSynthKind,
        arrow: bool,
        has_this_def: bool,
        function_loc: ALoc,
        function_: ast::function::Function<ALoc, ALoc>,
        statics: BTreeMap<FlowSmolStr, EnvKey<ALoc>>,
        tparams_map: TparamsMap,
    },
    Component {
        tparams_map: TparamsMap,
        component_loc: ALoc,
        component: ast::statement::ComponentDeclaration<ALoc, ALoc>,
    },
    Class {
        class_: ast::class::Class<ALoc, ALoc>,
        class_loc: ALoc,
        /// A set of this and super write locations that can be resolved by resolving the class.
        this_super_write_locs: EnvSet<ALoc>,
        kind: ClassKind,
    },
    Record {
        record: ast::statement::RecordDeclaration<ALoc, ALoc>,
        record_loc: ALoc,
        /// A set of this and super write locations that can be resolved by resolving the record.
        this_super_write_locs: EnvSet<ALoc>,
        defaulted_props: BTreeSet<FlowSmolStr>,
    },
    DeclaredClass(ALoc, ast::statement::DeclareClass<ALoc, ALoc>),
    DeclaredComponent(ALoc, ast::statement::DeclareComponent<ALoc, ALoc>),
    TypeAlias(ALoc, ast::statement::TypeAlias<ALoc, ALoc>),
    OpaqueType(ALoc, ast::statement::OpaqueType<ALoc, ALoc>),
    TypeParam {
        tparams_map: TparamsMap,
        kind: TypeParamsContext,
        tparam: (ALoc, ast::types::TypeParam<ALoc, ALoc>),
    },
    Interface(ALoc, ast::statement::Interface<ALoc, ALoc>),
    Enum(
        ALoc,
        FlowSmolStr,
        ast::statement::enum_declaration::Body<ALoc>,
    ),
    Import {
        import_kind: ast::statement::ImportKind,
        import: Import,
        source: FlowSmolStr,
        source_loc: ALoc,
    },
    GeneratorNext(Option<GeneratorAnnot>),
    DeclaredNamespace(ALoc, ast::statement::DeclareNamespace<ALoc, ALoc>),
    MissingThisAnnot,
}

pub mod print {
    use std::fmt::Write;

    use flow_aloc::ALoc;
    use flow_parser::loc_sig::LocSig;

    use super::*;

    pub fn string_of_root(root: &Root) -> String {
        match root {
            Root::Contextual { .. } => "contextual".to_string(),
            Root::EmptyArray { .. } => "[]".to_string(),
            Root::CatchUnannotated => "unannotated catch param".to_string(),
            Root::DeclareVariableMissingAnnotationAndInit => {
                "declare var missing annotation or init".to_string()
            }
            Root::UnannotatedParameter(r) => format!("{:?}", r),
            Root::Annotation {
                annot: (loc, _), ..
            } => format!("annot {}", loc.debug_to_string(false)),
            Root::Value(Value { expr, .. }) => {
                format!("val {}", expr.loc().debug_to_string(false))
            }
            Root::MatchCaseRoot {
                case_match_root_loc,
                ..
            } => {
                format!(
                    "match root for case at {}",
                    case_match_root_loc.debug_to_string(false)
                )
            }
            Root::FunctionValue { function_loc, .. } => {
                format!("function val {}", function_loc.debug_to_string(false))
            }
            Root::ObjectValue { .. } => "object".to_string(),
            Root::For(ForKind::In, (loc, _)) => format!("for in {}", loc.debug_to_string(false)),
            Root::For(ForKind::Of { .. }, (loc, _)) => {
                format!("for of {}", loc.debug_to_string(false))
            }
        }
    }

    pub fn string_of_selector(sel: &Selector<ALoc, ALoc>) -> String {
        sel.to_string()
    }

    pub fn string_of_binding(binding: &Binding) -> String {
        match binding {
            Binding::Root(r) => string_of_root(r),
            Binding::Hooklike(b) => format!("({})<as hooklike>", string_of_binding(b)),
            Binding::Select {
                selector,
                parent: (_, binding),
                ..
            } => {
                format!(
                    "({}){}",
                    string_of_binding(binding),
                    string_of_selector(selector)
                )
            }
        }
    }

    pub fn string_of_import_kind(kind: &ast::statement::ImportKind) -> &'static str {
        use ast::statement::ImportKind;
        match kind {
            ImportKind::ImportTypeof => "typeof ",
            ImportKind::ImportType => "type ",
            ImportKind::ImportValue => "",
        }
    }

    pub fn string_of_import(import: &Import) -> String {
        match import {
            Import::Named { kind, remote, .. } => {
                let k = kind.as_ref().map(string_of_import_kind).unwrap_or("");
                format!("{}{}", k, remote)
            }
            Import::Namespace(_) => "namespace".to_string(),
            Import::Default(_) => "default".to_string(),
        }
    }

    pub fn on_hint(hint: &HintNode) -> String {
        match hint {
            HintNode::AnnotationHint(_, _) => "annot hint".to_string(),
            HintNode::ValueHint(_, _) => "value hint".to_string(),
            HintNode::ProvidersHint(_) => "providers hint".to_string(),
            HintNode::WriteLocHint(_, _) => "write loc hint".to_string(),
            HintNode::StringLiteralType(s) => format!("string literal hint: {}", s),
            HintNode::ReactFragmentType => "react fragment hint".to_string(),
            HintNode::ReactNodeType => "react node hint".to_string(),
            HintNode::AnyErrorHint(_) => "any type hint".to_string(),
            HintNode::ComposedArrayPatternHint(_, elements) => {
                let mut result = String::from("[");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    match elem {
                        ArrayElementPatternHint::ArrayElementPatternHint(h) => {
                            write!(result, "({})", on_hint(h)).unwrap();
                        }
                        ArrayElementPatternHint::ArrayRestElementPatternHint(h) => {
                            write!(result, "...({})", on_hint(h)).unwrap();
                        }
                    }
                }
                result.push(']');
                result
            }
            HintNode::ComposedObjectPatternHint(_, props) => {
                let mut result = String::from("{");
                for (i, prop) in props.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    match prop {
                        ObjectPropPatternHint::ObjectPropPatternHint(n, _, h) => {
                            write!(result, "{}: {}", n, on_hint(h)).unwrap();
                        }
                        ObjectPropPatternHint::ObjectSpreadPropPatternHint(h) => {
                            write!(result, "...({})", on_hint(h)).unwrap();
                        }
                    }
                }
                result.push('}');
                result
            }
        }
    }

    pub fn string_of_source(def: &Def) -> String {
        match def {
            Def::Binding(b) => string_of_binding(b),
            Def::MatchCasePattern {
                pattern: (loc, _), ..
            } => {
                format!("match pattern {}", loc.debug_to_string(false))
            }
            Def::ExpressionDef(ExpressionDef { expr, hints, .. }) => {
                format!(
                    "exp {} (hint = {})",
                    expr.loc().debug_to_string(false),
                    string_of_hints(hints)
                )
            }
            Def::Update { .. } => "[in/de]crement".to_string(),
            Def::MemberAssign { .. } => "member_assign".to_string(),
            Def::OpAssign { .. } => "opassign".to_string(),
            Def::Function {
                function_: ast::function::Function { id, .. },
                ..
            } => {
                let name = id
                    .as_ref()
                    .map(|id| id.name.as_str())
                    .unwrap_or("<anonymous>");
                format!("fun {}", name)
            }
            Def::Component { component, .. } => {
                format!("component {}", component.id.name)
            }
            Def::DeclaredClass(_, decl) => {
                format!("declared class {}", decl.id.name)
            }
            Def::DeclaredComponent(_, decl) => {
                format!("declared component {}", decl.id.name)
            }
            Def::Class { class_, kind, .. } => {
                let kind_str = match kind {
                    ClassKind::Class => "class",
                    ClassKind::Record { .. } => "record",
                };
                let name = class_
                    .id
                    .as_ref()
                    .map(|id| id.name.as_str())
                    .unwrap_or("<anonymous>");
                format!("{} {}", kind_str, name)
            }
            Def::Record { record, .. } => {
                format!("record {}", record.id.name)
            }
            Def::TypeAlias(_, alias) => {
                format!("alias {}", alias.right.loc().debug_to_string(false))
            }
            Def::OpaqueType(_, opaque) => {
                format!("opaque {}", opaque.id.loc.debug_to_string(false))
            }
            Def::TypeParam {
                tparam: (loc, _), ..
            } => {
                format!("tparam {}", loc.debug_to_string(false))
            }
            Def::Enum(loc, name, _) => {
                format!("enum {} {}", name, loc.debug_to_string(false))
            }
            Def::Interface(_, _) => "interface".to_string(),
            Def::GeneratorNext(_) => "next".to_string(),
            Def::DeclaredNamespace(_, ns) => {
                use ast::statement::declare_namespace::Id;
                match &ns.id {
                    Id::Local(id) => {
                        format!(
                            "declare namespace {} {}",
                            id.name,
                            id.loc.debug_to_string(false)
                        )
                    }
                    Id::Global(_) => "declare global".to_string(),
                }
            }
            Def::Import {
                import_kind,
                source,
                import,
                ..
            } => {
                format!(
                    "import {}{} from {}",
                    string_of_import_kind(import_kind),
                    string_of_import(import),
                    source
                )
            }
            Def::MissingThisAnnot => "this (missing)".to_string(),
        }
    }

    fn string_of_hints(hints: &AstHints) -> String {
        use flow_common::hint::Hint;
        let strs: Vec<String> = hints
            .iter()
            .map(|hint| match hint {
                Hint::HintT(t, _) => format!("Hint_t ({})", on_hint(t)),
                Hint::HintDecomp(_, t, _) => format!("Hint_Decomp ({})", on_hint(t)),
                Hint::HintPlaceholder => "Hint_Placeholder".to_string(),
            })
            .collect();
        format!("[{}]", strs.join(", "))
    }
}

pub type EnvEntriesMap = EnvMap<ALoc, (Def, ScopeKind, ClassStack, VirtualReason<ALoc>)>;

pub type HintMap = ALocMap<AstHints>;
