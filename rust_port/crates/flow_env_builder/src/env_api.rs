/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::hash::Hash;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use dupe::Dupe;
use flow_analysis::scope_api::ScopeInfo;
use flow_common::reason::VirtualReason;
use flow_common::refinement_invalidation::RefinementInvalidation;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_parser::ast::expression::ArgList;
use flow_parser::ast::expression::CallTypeArgs;
use flow_parser::ast::expression::Expression;
use flow_parser::loc_sig::LocSig;

use crate::provider_api;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum EnvInvariantFailure<L> {
    NameDefOrderingFailure {
        all: Vec<L>,
        roots: Vec<L>,
        missing_roots: Vec<L>,
    },
    Impossible(FlowSmolStr),
    ASTStructureOverride(FlowSmolStr),
    NameDefGraphMismatch,
    MissingEnvEntry(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvInvariant<L> {
    pub loc: Option<L>,
    pub failure: EnvInvariantFailure<L>,
}

impl<L> EnvInvariant<L> {
    pub fn new(loc: Option<L>, failure: EnvInvariantFailure<L>) -> Self {
        Self { loc, failure }
    }
}

#[derive(Debug, Clone)]
pub enum CacheableEnvError<L> {
    ReferencedBeforeDeclaration { def_loc: L, name: FlowSmolStr },
    BuiltinNameLookupFailed(FlowSmolStr),
}

#[derive(Debug, Clone)]
pub enum LiteralCheck<L> {
    SingletonNum(L, f64, FlowSmolStr),
    SingletonBool(L, bool),
    SingletonStr(L, FlowSmolStr),
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DefLocType {
    OrdinaryNameLoc,
    FunctionParamLoc,
    PatternLoc,
    MatchCasePatternLoc,
    ExpressionLoc,
    ArrayProviderLoc,
    FunctionThisLoc,
    ClassSelfLoc,
    ClassInstanceThisLoc,
    ClassStaticThisLoc,
    ClassInstanceSuperLoc,
    ClassStaticSuperLoc,
}

impl DefLocType {
    pub fn show(self) -> &'static str {
        match self {
            Self::OrdinaryNameLoc => "OrdinaryNameLoc",
            Self::FunctionParamLoc => "FunctionParamLoc",
            Self::PatternLoc => "PatternLoc",
            Self::MatchCasePatternLoc => "MatchCasePatternLoc",
            Self::ExpressionLoc => "ExpressionLoc",
            Self::ArrayProviderLoc => "ArrayProviderLoc",
            Self::FunctionThisLoc => "FunctionThisLoc",
            Self::ClassSelfLoc => "ClassSelfLoc",
            Self::ClassInstanceThisLoc => "ClassInstanceThisLoc",
            Self::ClassStaticThisLoc => "ClassStaticThisLoc",
            Self::ClassInstanceSuperLoc => "ClassInstanceSuperLoc",
            Self::ClassStaticSuperLoc => "ClassStaticSuperLoc",
        }
    }
}

pub struct AutocompleteHooks<'a, L> {
    pub id_hook: Box<dyn Fn(&str, &L) -> bool + 'a>,
    pub literal_hook: Box<dyn Fn(&L) -> bool + 'a>,
    pub obj_prop_decl_hook: Box<dyn Fn(&str, &L) -> bool + 'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnvKey<L: Dupe + Eq + Ord + Hash> {
    pub def_loc_type: DefLocType,
    pub loc: L,
}

impl<L: Dupe + Eq + Ord + Hash> Dupe for EnvKey<L> {}

impl<L: Dupe + Eq + Ord + Hash> EnvKey<L> {
    pub fn new(def_loc_type: DefLocType, loc: L) -> Self {
        Self { def_loc_type, loc }
    }

    pub fn ordinary(loc: L) -> Self {
        Self::new(DefLocType::OrdinaryNameLoc, loc)
    }
}

#[derive(Debug, Clone, Default)]
pub struct EnvSet<L: Dupe + Eq + Ord + Hash> {
    inner: FlowOrdSet<EnvKey<L>>,
}

impl<L: Dupe + Eq + Ord + Hash> Dupe for EnvSet<L> {}

impl<L: Dupe + Eq + Ord + Hash> EnvSet<L> {
    pub fn empty() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<L: Dupe + Eq + Ord + Hash> Deref for EnvSet<L> {
    type Target = FlowOrdSet<EnvKey<L>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<L: Dupe + Eq + Ord + Hash> DerefMut for EnvSet<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone, Default)]
pub struct EnvMap<L: Dupe + Eq + Ord + Hash, V: Clone> {
    inner: FlowOrdMap<EnvKey<L>, V>,
}

impl<L: Dupe + Eq + Ord + Hash, V: Clone> Dupe for EnvMap<L, V> {}

impl<L: Dupe + Eq + Ord + Hash, V: Clone> EnvMap<L, V> {
    pub fn empty() -> Self {
        Self {
            inner: Default::default(),
        }
    }

    pub fn get_ordinary(&self, loc: &L) -> Option<&V>
    where
        L: Clone,
    {
        self.inner.get(&EnvKey::ordinary(loc.clone()))
    }

    pub fn update_ordinary<F>(&mut self, loc: L, f: F)
    where
        F: FnOnce(Option<V>) -> Option<V>,
        L: Clone,
    {
        let key = EnvKey::ordinary(loc);
        let old_value = self.inner.remove(&key);
        if let Some(new_value) = f(old_value) {
            self.inner.insert(key, new_value);
        }
    }
}

impl<L: Dupe + Eq + Ord + Hash, V: Clone> Deref for EnvMap<L, V> {
    type Target = FlowOrdMap<EnvKey<L>, V>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<L: Dupe + Eq + Ord + Hash, V: Clone> DerefMut for EnvMap<L, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReadLoc<L>(pub L);

impl<L: Dupe> Dupe for ReadLoc<L> {}

impl<L> Deref for ReadLoc<L> {
    type Target = L;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<L> DerefMut for ReadLoc<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone)]
pub enum WriteLoc<L: Dupe + Eq + Ord + Hash> {
    Write(VirtualReason<L>),
    EmptyArray {
        reason: VirtualReason<L>,
        arr_providers: FlowOrdSet<L>,
    },
    IllegalWrite(VirtualReason<L>),
    Uninitialized(VirtualReason<L>),
    Undeclared(FlowSmolStr, L),
    Refinement {
        refinement_id: i32,
        writes: Rc<Vec<WriteLoc<L>>>,
        write_id: Option<i32>,
    },
    FunctionThis(VirtualReason<L>),
    GlobalThis(VirtualReason<L>),
    IllegalThis(VirtualReason<L>),
    ClassInstanceThis(VirtualReason<L>),
    ClassStaticThis(VirtualReason<L>),
    ClassInstanceSuper(VirtualReason<L>),
    ClassStaticSuper(VirtualReason<L>),
    ModuleScoped(FlowSmolStr),
    Global(FlowSmolStr),
    Projection(L),
    Unreachable(L),
    Undefined(VirtualReason<L>),
    Number(VirtualReason<L>),
    DeclaredFunction(L),
}

pub mod refi {
    use std::hash::Hash;
    use std::rc::Rc;

    use dupe::Dupe;
    use flow_data_structure_wrapper::ord_set::FlowOrdSet;
    use flow_data_structure_wrapper::smol_str::FlowSmolStr;
    use flow_parser::ast::expression;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Dupe)]
    pub enum ArrayLengthOp {
        ArrLenEqual,
        ArrLenGreaterThanEqual,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Dupe)]
    pub enum InstanceofContext {
        InstanceOfExpr,
        MatchInstancePattern,
    }

    #[derive(Debug, Clone, Dupe)]
    pub enum RefinementKind<L: Dupe> {
        AndR(Rc<RefinementKind<L>>, Rc<RefinementKind<L>>),
        OrR(Rc<RefinementKind<L>>, Rc<RefinementKind<L>>),
        NotR(Rc<RefinementKind<L>>),
        TruthyR,
        NullR,
        UndefinedR,
        MaybeR,
        InstanceOfR {
            expr: Rc<expression::Expression<L, L>>,
            context: InstanceofContext,
        },
        IsArrayR,
        ArrLenR {
            op: ArrayLengthOp,
            n: i32,
        },
        BoolR(L),
        FunctionR,
        NumberR(L),
        BigIntR(L),
        ObjectR,
        StringR(L),
        SymbolR(L),
        SingletonBoolR {
            loc: L,
            sense: bool,
            lit: bool,
        },
        SingletonStrR {
            loc: L,
            sense: bool,
            lit: FlowSmolStr,
        },
        SingletonNumR {
            loc: L,
            sense: bool,
            lit: (f64, FlowSmolStr),
        },
        SingletonBigIntR {
            loc: L,
            sense: bool,
            lit: (Option<i64>, FlowSmolStr),
        },
        SentinelR {
            prop: FlowSmolStr,
            other_loc: L,
        },
        PropExistsR {
            propname: FlowSmolStr,
            loc: L,
        },
        PropNullishR {
            propname: FlowSmolStr,
            loc: L,
        },
        PropIsExactlyNullR {
            propname: FlowSmolStr,
            loc: L,
        },
        PropNonVoidR {
            propname: FlowSmolStr,
            loc: L,
        },
        LatentR {
            func: Rc<expression::Expression<L, L>>,
            targs: Option<Rc<expression::CallTypeArgs<L, L>>>,
            arguments: Rc<expression::ArgList<L, L>>,
            index: Rc<[i32]>,
        },
        LatentThisR {
            func: Rc<expression::Expression<L, L>>,
            targs: Option<Rc<expression::CallTypeArgs<L, L>>>,
            arguments: Rc<expression::ArgList<L, L>>,
        },
        PropTruthyR {
            propname: FlowSmolStr,
            loc: L,
        },
        EqR(L),
        ImpossibleR,
    }

    #[derive(Debug, Clone, Dupe)]
    pub struct Refinement<L: Dupe + Eq + Ord + Hash> {
        /// Locations that we can point to the user where the refinement happens.
        pub refining_locs: FlowOrdSet<L>,
        pub kind: RefinementKind<L>,
    }
}

pub use refi::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValKind {
    Type {
        imported: bool,
        type_only_namespace: bool,
    },
    TsImport,
    Value,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Read<L: Dupe + Eq + Ord + Hash> {
    pub def_loc: Option<L>,
    pub write_locs: Rc<Vec<WriteLoc<L>>>,
    pub val_kind: ValKind,
    pub name: Option<FlowSmolStr>,
    pub id: Option<i32>,
}

#[derive(Debug, Clone, Default)]
pub struct Values<L: Dupe + Eq + Ord + Hash> {
    inner: FlowOrdMap<L, Read<L>>,
}

impl<L: Dupe + Eq + Ord + Hash> Dupe for Values<L> {}

impl<L: Dupe + Eq + Ord + Hash> Values<L> {
    pub fn new() -> Self {
        Self {
            inner: FlowOrdMap::new(),
        }
    }
}

impl<L: Dupe + Eq + Ord + Hash> Deref for Values<L> {
    type Target = FlowOrdMap<L, Read<L>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<L: Dupe + Eq + Ord + Hash> DerefMut for Values<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone, Dupe)]
pub enum EnvEntry<L: Dupe> {
    /// Not every assignment actually produces a write. For example, when a
    /// const is reassigned it does not actually update the contents of the const
    /// and crashes instead. In these types of writes where nothing can actually
    /// be written, we don't want to produce extra error messages related to the type
    /// incompatibility of the write that is never performed. When the new_env finds a
    /// NonAssigningWrite it will not subtype the given type against the providers.
    AssigningWrite(VirtualReason<L>),
    GlobalWrite(VirtualReason<L>),
    NonAssigningWrite,
}

#[derive(Debug, Clone)]
pub struct PredicateRefinement<L: Dupe + Eq + Ord + Hash, Binding>(pub Read<L>, pub L, pub Binding);

/// Entry for type guard consistency checking.
/// (expression, reason, read before, read after)
#[derive(Debug, Clone)]
pub struct TypeGuardConsistencyEntry<L: Dupe + Eq + Ord + Hash>(
    pub Expression<L, L>,
    pub VirtualReason<L>,
    pub Read<L>,
    pub Read<L>,
);

#[derive(Debug, Clone, Default)]
pub struct TypeGuardConsistencyMaps<L: Dupe + Eq + Ord + Hash> {
    inner: FlowOrdMap<L, (Option<FlowOrdSet<L>>, Vec<TypeGuardConsistencyEntry<L>>)>,
}

impl<L: Dupe + Eq + Ord + Hash> Dupe for TypeGuardConsistencyMaps<L> {}

impl<L: Dupe + Eq + Ord + Hash> TypeGuardConsistencyMaps<L> {
    pub fn new() -> Self {
        Self {
            inner: FlowOrdMap::new(),
        }
    }
}

impl<L: Dupe + Eq + Ord + Hash> Deref for TypeGuardConsistencyMaps<L> {
    type Target = FlowOrdMap<L, (Option<FlowOrdSet<L>>, Vec<TypeGuardConsistencyEntry<L>>)>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<L: Dupe + Eq + Ord + Hash> DerefMut for TypeGuardConsistencyMaps<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone)]
pub struct PredFuncInfo<L: Dupe> {
    pub class_stack: FlowVector<L>,
    pub call_expr: Expression<L, L>,
    pub callee: Expression<L, L>,
    pub targs: Option<CallTypeArgs<L, L>>,
    pub arguments: ArgList<L, L>,
}

pub struct EnvInfo<L>
where
    L: Dupe + Eq + Ord + Hash + LocSig,
{
    pub scopes: ScopeInfo<L>,
    pub env_values: Values<L>,
    pub env_refinement_invalidation_info: FlowOrdMap<L, RefinementInvalidation>,
    pub env_entries: EnvMap<L, EnvEntry<L>>,
    pub type_guard_consistency_maps: TypeGuardConsistencyMaps<L>,
    pub providers: Rc<provider_api::Info<L>>,
    pub refinement_of_id: Box<dyn Fn(i32) -> Refinement<L>>,
    pub pred_func_map: FlowOrdMap<L, PredFuncInfo<L>>,
    pub interface_merge_conflicts: FlowOrdMap<L, Vec<L>>,
    pub declare_class_interface_merge_conflicts: FlowOrdMap<L, Vec<L>>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum AnnotLoc<L: Dupe> {
    Loc(L),
    Object { loc: L, props: Vec<L> },
}

impl<L: Dupe> AnnotLoc<L> {
    pub fn map<F, M: Dupe>(&self, f: F) -> AnnotLoc<M>
    where
        F: Fn(&L) -> M,
    {
        match self {
            AnnotLoc::Loc(l) => AnnotLoc::Loc(f(l)),
            AnnotLoc::Object { loc, props } => AnnotLoc::Object {
                loc: f(loc),
                props: props.iter().map(f).collect(),
            },
        }
    }
}

impl<L> EnvInfo<L>
where
    L: Dupe + Eq + Ord + Hash + LocSig,
{
    pub fn empty() -> Self {
        EnvInfo {
            scopes: ScopeInfo::empty(),
            env_values: Values::new(),
            env_refinement_invalidation_info: FlowOrdMap::new(),
            env_entries: EnvMap::empty(),
            type_guard_consistency_maps: TypeGuardConsistencyMaps::new(),
            providers: Rc::new(provider_api::Info::default()),
            refinement_of_id: Box::new(|_| {
                panic!("Empty env info: refinement_of_id called on empty EnvInfo")
            }),
            pred_func_map: FlowOrdMap::new(),
            interface_merge_conflicts: FlowOrdMap::new(),
            declare_class_interface_merge_conflicts: FlowOrdMap::new(),
        }
    }
}

pub fn has_assigning_write<L>(key: EnvKey<L>, env_entries: &EnvMap<L, EnvEntry<L>>) -> bool
where
    L: Dupe + Eq + Ord + Hash,
{
    match env_entries.get(&key) {
        Some(EnvEntry::AssigningWrite(_)) | Some(EnvEntry::GlobalWrite(_)) => true,
        Some(EnvEntry::NonAssigningWrite) | None => false,
    }
}

/// Like has_assigning_write, but excludes GlobalWrite. GlobalWrite occurs
/// for all declarations in lib/builtin files. Deferring those declarations
/// changes their processing order and causes spurious type errors.
pub fn has_non_global_assigning_write<L>(
    key: EnvKey<L>,
    env_entries: &EnvMap<L, EnvEntry<L>>,
) -> bool
where
    L: Dupe + Eq + Ord + Hash,
{
    match env_entries.get(&key) {
        Some(EnvEntry::AssigningWrite(_)) => true,
        _ => false,
    }
}

pub fn is_global_var<L>(read: &Read<L>) -> bool
where
    L: Dupe + Eq + Ord + Hash,
{
    fn no_local_defs<L>(states: &[WriteLoc<L>]) -> bool
    where
        L: Dupe + Eq + Ord + Hash,
    {
        states.iter().all(|state| match state {
            WriteLoc::Global(_) => true,
            WriteLoc::Refinement { writes, .. } => no_local_defs(writes.as_slice()),
            WriteLoc::Undefined(_)
            | WriteLoc::Number(_)
            | WriteLoc::DeclaredFunction(_)
            | WriteLoc::Uninitialized(_)
            | WriteLoc::EmptyArray { .. }
            | WriteLoc::Write(_)
            | WriteLoc::IllegalWrite(_)
            | WriteLoc::Unreachable(_)
            | WriteLoc::Undeclared(_, _)
            | WriteLoc::Projection(_)
            | WriteLoc::GlobalThis(_)
            | WriteLoc::IllegalThis(_)
            | WriteLoc::FunctionThis(_)
            | WriteLoc::ClassInstanceThis(_)
            | WriteLoc::ClassStaticThis(_)
            | WriteLoc::ClassInstanceSuper(_)
            | WriteLoc::ClassStaticSuper(_)
            | WriteLoc::ModuleScoped(_) => false,
        })
    }
    no_local_defs(&read.write_locs)
}

pub fn write_locs_of_read_loc<'a, L>(values: &'a Values<L>, read_loc: &L) -> &'a [WriteLoc<L>]
where
    L: Dupe + Eq + Ord + Hash,
{
    let read = values
        .get(read_loc)
        .expect("write_locs_of_read_loc: read_loc not found in values");
    read.write_locs.as_slice()
}

pub fn writes_of_write_loc<L: LocSig>(
    for_type: bool,
    providers: &provider_api::Info<L>,
    write_loc: &WriteLoc<L>,
) -> Vec<EnvKey<L>> {
    fn collect<L: LocSig>(
        for_type: bool,
        providers: &provider_api::Info<L>,
        write_loc: &WriteLoc<L>,
        acc: &mut Vec<EnvKey<L>>,
    ) {
        match write_loc {
            WriteLoc::Refinement { writes, .. } => {
                for w in writes.iter() {
                    collect(for_type, providers, w, acc);
                }
            }
            WriteLoc::Write(r) => {
                acc.push(EnvKey::new(DefLocType::OrdinaryNameLoc, r.loc().dupe()));
            }
            WriteLoc::EmptyArray {
                reason,
                arr_providers,
            } => {
                acc.push(EnvKey::new(
                    DefLocType::OrdinaryNameLoc,
                    reason.loc().dupe(),
                ));
                for l in arr_providers {
                    acc.push(EnvKey::new(DefLocType::ArrayProviderLoc, l.dupe()));
                }
            }
            WriteLoc::IllegalWrite(_) => {}
            WriteLoc::Uninitialized(_) => {}
            WriteLoc::Undeclared(_, loc) => {
                if for_type {
                    acc.push(EnvKey::new(DefLocType::OrdinaryNameLoc, loc.dupe()));
                }
            }
            WriteLoc::FunctionThis(r) => {
                acc.push(EnvKey::new(DefLocType::FunctionThisLoc, r.loc().dupe()));
            }
            WriteLoc::GlobalThis(_) => {}
            WriteLoc::IllegalThis(_) => {}
            WriteLoc::ClassInstanceThis(r) => {
                acc.push(EnvKey::new(
                    DefLocType::ClassInstanceThisLoc,
                    r.loc().dupe(),
                ));
            }
            WriteLoc::ClassStaticThis(r) => {
                acc.push(EnvKey::new(DefLocType::ClassStaticThisLoc, r.loc().dupe()));
            }
            WriteLoc::ClassInstanceSuper(r) => {
                acc.push(EnvKey::new(
                    DefLocType::ClassInstanceSuperLoc,
                    r.loc().dupe(),
                ));
            }
            WriteLoc::ClassStaticSuper(r) => {
                acc.push(EnvKey::new(DefLocType::ClassStaticSuperLoc, r.loc().dupe()));
            }
            WriteLoc::ModuleScoped(_) => {}
            WriteLoc::Global(_) => {}
            WriteLoc::Projection(l) => {
                acc.push(EnvKey::new(DefLocType::OrdinaryNameLoc, l.dupe()));
            }
            WriteLoc::Unreachable(_) => {}
            WriteLoc::Undefined(_) => {}
            WriteLoc::Number(_) => {}
            WriteLoc::DeclaredFunction(l) => {
                if let Some(def_providers) = providers.providers_of_def(l) {
                    for provider in &def_providers.providers {
                        acc.push(EnvKey::new(
                            DefLocType::OrdinaryNameLoc,
                            provider.reason.loc().dupe(),
                        ));
                    }
                }
            }
        }
    }

    let mut result = Vec::new();
    collect(for_type, providers, write_loc, &mut result);
    result
}

pub fn refinements_of_write_loc<L, F>(
    refinement_of_id: F,
    write_loc: &WriteLoc<L>,
) -> Vec<RefinementKind<L>>
where
    L: Dupe + Eq + Ord + Hash,
    F: Fn(i32) -> Refinement<L> + Copy,
{
    fn collect<L, F>(refinement_of_id: F, write_loc: &WriteLoc<L>, acc: &mut Vec<RefinementKind<L>>)
    where
        L: Dupe + Eq + Ord + Hash,
        F: Fn(i32) -> Refinement<L> + Copy,
    {
        match write_loc {
            WriteLoc::Refinement {
                refinement_id,
                writes,
                ..
            } => {
                let refinement = refinement_of_id(*refinement_id);
                acc.push(refinement.kind);
                for w in writes.iter() {
                    collect(refinement_of_id, w, acc);
                }
            }
            _ => {}
        }
    }

    let mut result = Vec::new();
    collect(refinement_of_id, write_loc, &mut result);
    result
}

pub fn show_refinement_kind_without_locs<L: Dupe>(kind: &RefinementKind<L>) -> String {
    match kind {
        RefinementKind::AndR(l, r) => format!(
            "And ({}, {})",
            show_refinement_kind_without_locs(l),
            show_refinement_kind_without_locs(r)
        ),
        RefinementKind::OrR(l, r) => format!(
            "Or ({}, {})",
            show_refinement_kind_without_locs(l),
            show_refinement_kind_without_locs(r)
        ),
        RefinementKind::NotR(r) => format!("Not ({})", show_refinement_kind_without_locs(r)),
        RefinementKind::TruthyR => "Truthy".to_string(),
        RefinementKind::NullR => "Null".to_string(),
        RefinementKind::UndefinedR => "Undefined".to_string(),
        RefinementKind::MaybeR => "Maybe".to_string(),
        RefinementKind::InstanceOfR { .. } => "instanceof".to_string(),
        RefinementKind::IsArrayR => "isArray".to_string(),
        RefinementKind::ArrLenR { op, n } => {
            let op_str = match op {
                ArrayLengthOp::ArrLenEqual => "===",
                ArrayLengthOp::ArrLenGreaterThanEqual => ">=",
            };
            format!("array length {} {}", op_str, n)
        }
        RefinementKind::BoolR(_) => "bool".to_string(),
        RefinementKind::FunctionR => "function".to_string(),
        RefinementKind::NumberR(_) => "number".to_string(),
        RefinementKind::BigIntR(_) => "bigint".to_string(),
        RefinementKind::ObjectR => "object".to_string(),
        RefinementKind::StringR(_) => "string".to_string(),
        RefinementKind::SymbolR(_) => "symbol".to_string(),
        RefinementKind::SingletonBoolR { lit, sense, .. } => (lit == sense).to_string(),
        RefinementKind::SingletonStrR { lit, sense, .. } => {
            if !sense {
                format!("Not ({})", lit)
            } else {
                lit.to_string()
            }
        }
        RefinementKind::SingletonNumR { lit, sense, .. } => {
            if !sense {
                format!("Not ({})", lit.1)
            } else {
                lit.1.to_string()
            }
        }
        RefinementKind::SingletonBigIntR { lit, sense, .. } => {
            if !sense {
                format!("Not ({})", lit.1)
            } else {
                lit.1.to_string()
            }
        }
        RefinementKind::SentinelR { prop, .. } => format!("SentinelR {}", prop),
        RefinementKind::PropExistsR { propname, .. } => format!("PropExistsR ({})", propname),
        RefinementKind::PropNullishR { propname, .. } => format!("PropNullishR {}", propname),
        RefinementKind::PropIsExactlyNullR { propname, .. } => {
            format!("PropIsExactlyNullR {}", propname)
        }
        RefinementKind::PropNonVoidR { propname, .. } => format!("PropNonVoidR {}", propname),
        RefinementKind::LatentR { index, .. } => {
            let indices: Vec<String> = index.iter().map(|i| i.to_string()).collect();
            format!("LatentR (index = {})", indices.join(", "))
        }
        RefinementKind::LatentThisR { .. } => "LatentThisR".to_string(),
        RefinementKind::PropTruthyR { propname, .. } => format!("PropTruthyR ({})", propname),
        RefinementKind::EqR(_) => "EqR".to_string(),
        RefinementKind::ImpossibleR => "ImpossibleR".to_string(),
    }
}
