/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(never_type)]

use std::cell::LazyCell;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::rc::Rc;
use std::rc::Weak;
use std::sync::Arc;

use dupe::Dupe;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_aloc::ALocFuzzy;
use flow_aloc::ALocFuzzyMap;
use flow_aloc::ALocId;
use flow_aloc::ALocMap;
use flow_aloc::ALocSet;
use flow_aloc::LazyALocTable;
use flow_aloc::LocToALocMapper;
use flow_analysis::property_assignment::Errors as PropertyAssignmentErrors;
use flow_common::docblock::Docblock;
use flow_common::docblock::FlowMode;
use flow_common::docblock::JsxRuntimePragma;
use flow_common::enclosing_context::EnclosingContext;
use flow_common::files;
use flow_common::files::FileOptions;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common::flow_projects::FlowProjects;
use flow_common::flow_projects::ProjectsOptions;
use flow_common::options::AssertOperator;
use flow_common::options::CastingSyntax;
use flow_common::options::JsxMode;
use flow_common::options::Options;
use flow_common::options::ReactRefAsProp;
use flow_common::options::ReactRule;
use flow_common::options::ReactRuntime;
use flow_common::platform_set::PlatformSet;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::string_of_aloc;
use flow_common::refinement_invalidation::RefinementInvalidation;
use flow_common::slow_to_check_logging::SlowToCheckLogging;
use flow_common::subst_name::SubstName;
use flow_common::verbose::Verbose;
use flow_data_structure_wrapper::int_map::IntHashMap;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_env_builder::env_api::CacheableEnvError;
use flow_env_builder::name_def_types::ScopeKind;
use flow_lint_settings::lint_severity_cover::LintSeverityCover;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper;
use flow_type_sig::compact_table;
use flow_typing_builtins::Builtins;
use flow_typing_builtins::LazyModuleType;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_suppressions::ErrorSuppressions;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::flow_error::FlowError;
use flow_typing_errors::flow_error::error_of_msg;
use flow_typing_exists_check::ExistsCheck;
use flow_typing_generics::GenericId;
use flow_typing_loc_env::loc_env::LocEnv;
use flow_typing_loc_env::node_cache::NodeCache;
use flow_typing_speculation_state::SpeculationState;
use flow_typing_spread_cache::SpreadCache;
use flow_typing_type::type_;
use flow_typing_type::type_::ConstFoldMap;
use flow_typing_type::type_::EvalIdCacheMap;
use flow_typing_type::type_::EvalReposCacheMap;
use flow_typing_type::type_::FixCacheMap;
use flow_typing_type::type_::IdCacheMap;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::RootUseOp;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeContext;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::aconstraint::AConstraint;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::constraint::forcing_state::ForcingState;
use flow_utils_concurrency::check_budget::CheckBudget;
use flow_utils_concurrency::job_error::JobError;
use flow_utils_union_find::TvarNotFound;
use regex::Regex;
use vec1::Vec1;

#[derive(Debug, Clone, Dupe)]
pub struct PropsNotFound(pub type_::properties::Id);

#[derive(Debug, Clone, Copy, Dupe)]
pub struct CallNotFound(pub i32);

#[derive(Debug, Clone, Dupe)]
pub struct ExportsNotFound(pub type_::exports::Id);

#[derive(Debug, Clone, Dupe)]
pub struct RequireNotFound(pub FlowImportSpecifier);

#[derive(Debug, Clone, Dupe)]
pub struct ModuleNotFound(pub FlowSmolStr);

/// Fields that change per-file (via `docblock_overrides` or by setting `checked = false`).
/// Small struct, cheap to clone directly.
#[derive(Debug, Clone)]
pub struct OverridableMetadata {
    pub available_platforms: Option<PlatformSet>,
    pub checked: bool,
    pub has_explicit_supports_platform: bool,
    pub jsx: JsxMode,
    pub munge_underscores: bool,
    pub react_runtime: ReactRuntime,
    pub strict: bool,
    pub strict_local: bool,
}

impl Default for OverridableMetadata {
    fn default() -> Self {
        Self {
            available_platforms: None,
            checked: true,
            has_explicit_supports_platform: false,
            jsx: JsxMode::JsxReact,
            munge_underscores: false,
            react_runtime: ReactRuntime::Classic,
            strict: false,
            strict_local: false,
        }
    }
}

/// Fields set once from `Options`, never modified per-file. Stored in `Rc<FrozenMetadata>` for
/// cheap cloning.
#[derive(Clone, Debug)]
pub struct FrozenMetadata {
    pub include_suppressions: bool,
    pub slow_to_check_logging: SlowToCheckLogging,
    pub verbose: Option<Arc<Verbose>>,

    pub abstract_classes: bool,
    pub assert_operator: AssertOperator,
    pub automatic_require_default: bool,
    pub babel_loose_array_spread: bool,
    pub ban_spread_key_props: bool,
    pub casting_syntax: CastingSyntax,
    pub casting_syntax_only_support_as_excludes: Arc<[Regex]>,
    pub component_syntax: bool,
    pub async_component_syntax: bool,
    pub async_component_syntax_includes: Arc<[Regex]>,
    pub deprecated_utilities: Arc<BTreeMap<String, Vec<String>>>,
    pub deprecated_utilities_excludes: Arc<[Regex]>,
    pub dev_only_refinement_info_as_errors: bool,
    pub enable_const_params: bool,
    pub enable_custom_error: bool,
    pub enable_enums: bool,
    pub enable_jest_integration: bool,
    pub enable_pattern_matching: bool,
    pub enable_pattern_matching_instance_patterns: bool,
    pub enable_records: bool,
    pub enable_relay_integration: bool,
    pub exact_by_default: bool,
    pub facebook_fbs: Option<FlowSmolStr>,
    pub facebook_fbt: Option<FlowSmolStr>,
    pub facebook_module_interop: bool,
    pub file_options: Arc<FileOptions>,
    pub hook_compatibility: bool,
    pub hook_compatibility_excludes: Arc<[Regex]>,
    pub hook_compatibility_includes: Arc<[Regex]>,
    pub ignore_non_literal_requires: bool,
    pub records_includes: Arc<[Regex]>,
    pub instance_t_objkit_fix: bool,
    pub max_workers: i32,
    pub missing_module_generators: Arc<[(Regex, String)]>,
    pub no_unchecked_indexed_access: bool,
    pub opaque_type_new_bound_syntax: bool,
    pub projects_options: Arc<ProjectsOptions>,
    pub react_custom_jsx_typing: bool,
    pub react_ref_as_prop: ReactRefAsProp,
    pub react_rules: Arc<[ReactRule]>,
    pub recursion_limit: i32,
    pub relay_integration_esmodules: bool,
    pub relay_integration_excludes: Arc<[Regex]>,
    pub relay_integration_module_prefix: Option<FlowSmolStr>,
    pub relay_integration_module_prefix_includes: Arc<[Regex]>,
    pub root: Arc<PathBuf>,
    pub strict_es6_import_export: bool,
    pub strip_root: bool,
    pub stylex_shorthand_prop: Option<String>,
    pub ts_syntax: bool,
    pub allow_readonly_variance: bool,
    pub allow_variance_keywords: bool,
    pub deprecated_colon_extends: Arc<[String]>,
    pub deprecated_colon_extends_excludes: Arc<[Regex]>,
    pub ts_utility_syntax: bool,
    pub tslib_syntax: bool,
    pub typescript_library_definition_support: bool,
    pub type_expansion_recursion_limit: i32,
    pub use_unknown_in_catch_variables: bool,
}

impl Default for FrozenMetadata {
    fn default() -> Self {
        Self {
            include_suppressions: false,
            slow_to_check_logging: SlowToCheckLogging::default(),
            verbose: None,
            abstract_classes: false,
            assert_operator: AssertOperator::Disabled,
            automatic_require_default: false,
            babel_loose_array_spread: false,
            ban_spread_key_props: false,
            casting_syntax: CastingSyntax::Both,
            casting_syntax_only_support_as_excludes: Arc::from([]),
            component_syntax: true,
            async_component_syntax: false,
            async_component_syntax_includes: Arc::from([]),
            deprecated_utilities: Arc::new(BTreeMap::new()),
            deprecated_utilities_excludes: Arc::from([]),
            dev_only_refinement_info_as_errors: false,
            enable_const_params: false,
            enable_custom_error: false,
            enable_enums: true,
            enable_jest_integration: false,
            enable_pattern_matching: false,
            enable_pattern_matching_instance_patterns: false,
            enable_records: false,
            enable_relay_integration: false,
            exact_by_default: true,
            facebook_fbs: None,
            facebook_fbt: None,
            facebook_module_interop: false,
            file_options: Arc::new(FileOptions::default()),
            hook_compatibility: false,
            hook_compatibility_excludes: Arc::from([]),
            hook_compatibility_includes: Arc::from([]),
            ignore_non_literal_requires: false,
            records_includes: Arc::from([]),
            instance_t_objkit_fix: false,
            max_workers: 0,
            missing_module_generators: Arc::from([]),
            no_unchecked_indexed_access: false,
            opaque_type_new_bound_syntax: false,
            projects_options: Arc::new(ProjectsOptions::default()),
            react_custom_jsx_typing: false,
            react_ref_as_prop: ReactRefAsProp::Legacy,
            react_rules: Arc::from([]),
            recursion_limit: 10000,
            relay_integration_esmodules: false,
            relay_integration_excludes: Arc::from([]),
            relay_integration_module_prefix: None,
            relay_integration_module_prefix_includes: Arc::from([]),
            root: Arc::new(PathBuf::new()),
            strict_es6_import_export: false,
            strip_root: true,
            stylex_shorthand_prop: None,
            ts_syntax: false,
            allow_readonly_variance: false,
            allow_variance_keywords: false,
            deprecated_colon_extends: Arc::from([]),
            deprecated_colon_extends_excludes: Arc::from([]),
            ts_utility_syntax: false,
            tslib_syntax: false,
            typescript_library_definition_support: false,
            type_expansion_recursion_limit: 3,
            use_unknown_in_catch_variables: false,
        }
    }
}

/// Metadata for a file. Split into frozen (shared via `Rc`) and overridable parts for cheap
/// cloning.
#[derive(Debug, Clone)]
pub struct Metadata {
    pub frozen: Rc<FrozenMetadata>,
    pub overridable: OverridableMetadata,
}

impl Default for Metadata {
    fn default() -> Self {
        Self {
            frozen: Rc::new(FrozenMetadata::default()),
            overridable: OverridableMetadata::default(),
        }
    }
}

#[derive(Debug, Clone)]
enum TestPropHitOrMiss {
    Hit,
    Miss {
        name: Option<Name>,
        reasons: (Reason, Reason),
        use_op: UseOp,
        suggestion: Option<FlowSmolStr>,
    },
}

#[derive(Debug, Clone)]
pub struct VoidableCheck {
    pub public_property_map: type_::properties::Id,
    pub private_property_map: type_::properties::Id,
    pub errors: PropertyAssignmentErrors<ALoc>,
}

#[derive(Debug, Clone, Dupe)]
pub enum SubstCacheErr {
    ETooFewTypeArgs(ALoc, i32),
    ETooManyTypeArgs(ALoc, i32),
}

pub mod type_app_expansion {
    use super::*;

    // Array types function like type applications but are not implemented as such. Unless
    // we decide to unify their implementation with regular typeapps, they need special
    // handling here
    #[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Root {
        Type(flow_typing_type::type_::Type),
        Array(flow_common::reason::Reason),
        ROArray(flow_common::reason::Reason),
        Tuple(flow_common::reason::Reason, usize),
    }

    pub type RootSet = BTreeSet<Root>;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Bound {
        Lower,
        Upper,
    }

    #[derive(Debug, Clone)]
    pub struct Entry(
        pub flow_typing_type::type_::Type,
        pub Vec<RootSet>,
        pub Bound,
    );
}

#[derive(Debug, Clone)]
pub struct PossiblyRefinedWriteState {
    pub t: Type,
    pub errors: Vec<CacheableEnvError<ALoc>>,
    pub actually_refined_refining_locs: Option<FlowOrdSet<ALoc>>,
}

#[derive(Debug, Clone)]
pub struct BuiltinsGroup {
    pub builtin_locs: Arc<compact_table::Table<Loc>>,
    pub builtins: Arc<flow_type_sig::packed_type_sig::Builtins<Loc>>,
}

#[derive(Debug)]
pub enum MasterContext {
    EmptyMasterContext,
    NonEmptyMasterContext {
        builtin_leader_file_key: FileKey,
        unscoped_builtins: BuiltinsGroup,
        scoped_builtins: Vec<(FlowProjects, BuiltinsGroup)>,
    },
}

pub struct ComponentT<'cx> {
    sig_cx: RefCell<TypeContext<'cx, Context<'cx>>>,
    // mapping from keyed alocs to concrete locations
    aloc_tables: RefCell<HashMap<FileKey, LazyALocTable>>,
    synthesis_produced_uncacheable_result: RefCell<bool>,
    errors: RefCell<ErrorSet>,
    error_suppressions: RefCell<ErrorSuppressions>,
    severity_cover: RefCell<FlowOrdMap<FileKey, LintSeverityCover>>,

    // map from exists proposition locations to the types of values running through them
    exists_checks: RefCell<ALocMap<BTreeSet<Type>>>,
    // map from exists proposition locations to the types of excuses for them *)
    // If a variable appears in something like `x || ''`, the existence check
    // is excused and not considered sketchy. (The program behaves identically to how it would
    // if the null check was made explicit (`x == null ? '' : x`), and this is a fairly
    // common pattern. Excusing it eliminates a lot of noise from the lint rule. *)
    // The above example assumes that x is a string. If it were a different type
    // it wouldn't be excused.
    exists_excuses: RefCell<ALocMap<ExistsCheck>>,
    // For the definite instance property assignment analysis, we should only
    // emit errors for a given property if VoidT flows to the type of that
    // property. Ideally, we would create a VoidT ~> property type flow when we
    // perform the analysis. The problem is that doing that causes the type
    // inference behavior to depend on lint settings which can lead to some weird
    // behavior, such as extra errors even when the lint is off. The solution is
    // to collect all of potential errors that we would have created a flow for
    // in the context and deal with them post-merge. At this point, the tvars of
    // nearly all properties will have a concrete type that we can safely pattern
    // match on without affecting other constraints. For the unresolved tvars, we
    // conservatively emit errors.
    voidable_checks: RefCell<Vec<VoidableCheck>>,
    test_prop_hits_and_misses: RefCell<IntHashMap<i32, TestPropHitOrMiss>>,
    optional_chains_useful: RefCell<ALocMap<(Reason, bool)>>,
    conditions: RefCell<FlowVector<flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>>>,
    strict_comparisons: RefCell<
        FlowVector<(
            ALoc,
            (
                flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
                flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
            ),
        )>,
    >,
    maybe_unused_promises: RefCell<FlowVector<(ALoc, Type, bool)>>,

    constraint_cache: RefCell<type_::FlowSet<Context<'cx>>>,
    subst_cache: RefCell<type_::SubstCacheMap<(Vec<SubstCacheErr>, Type)>>,
    eval_id_cache: RefCell<EvalIdCacheMap>,
    id_cache: RefCell<IdCacheMap>,
    eval_repos_cache: RefCell<EvalReposCacheMap>,
    fix_cache: RefCell<FixCacheMap>,
    spread_cache: RefCell<SpreadCache>,
    const_fold_cache: RefCell<ConstFoldMap>,
    speculation_state: RefCell<SpeculationState>,
    instantiation_stack: RefCell<FlowVector<type_app_expansion::Entry>>,

    // Post-inference checks
    delayed_forcing_tvars: RefCell<FlowOrdSet<i32>>,
    post_component_tvar_forcing_states: RefCell<FlowVector<ForcingState<'cx, Context<'cx>>>>,
    post_inference_polarity_checks: RefCell<
        Vec<(
            BTreeMap<SubstName, type_::TypeParam>,
            flow_common::polarity::Polarity,
            Type,
        )>,
    >,
    post_inference_validation_flows: RefCell<VecDeque<(Type, type_::UseT<Context<'cx>>)>>,
    post_inference_projects_strict_boundary_import_pattern_opt_outs_validations:
        RefCell<Vec<(ALoc, String, Vec<FlowProjects>)>>,
    /// Supports interface declaration merging. When two or more
    /// `interface Foo { ... }` declarations share a name in one scope,
    /// TS-style merging folds them into a single Foo. A consequence is that
    /// any field name appearing in more than one declaration must have
    /// agreeing types across declarations — otherwise the merged interface
    /// would be contradictory. This table is what lets us check that
    /// agreement: for each merging interface (keyed by its name_loc), we
    /// remember the field types we saw in source. After inference is done,
    /// the post-inference pass walks these and unifies any field types that
    /// appear on both sides of a merge. Interfaces not involved in any merge
    /// are absent from this table — their loc never gets a slot, so
    /// recording is a no-op for them.
    merging_interface_field_types: RefCell<HashMap<ALoc, HashMap<Name, Type>>>,
    /// Also supports interface declaration merging, but for a different step:
    /// actually combining the property maps. Each interface InstanceT stores
    /// its own/proto properties in two heap-allocated maps identified by
    /// `Properties::Id`. To fold N decls of `Foo` into one usable type, the
    /// maps of the later decls must be merged into the maps of the first.
    /// This table records the (own_props, proto_props) id pair of every
    /// interface decl in the file so the merger (env_resolution) can look up
    /// the right maps for each side of a merge. Populated for all interfaces,
    /// not only the merging ones, since at registration time we don't gate on
    /// participation.
    interface_prop_ids: RefCell<BTreeMap<ALoc, (type_::properties::Id, type_::properties::Id)>>,
    env_value_cache: RefCell<IntHashMap<i32, PossiblyRefinedWriteState>>,
    env_type_cache: RefCell<IntHashMap<i32, PossiblyRefinedWriteState>>,
    // map from annot tvar ids to nodes used during annotation processing
    annot_graph: RefCell<IntHashMap<i32, AConstraint<'cx>>>,
    // Used to power an autofix that takes the lower bounds of types where we emit missing-local-annot
    // and turn them into annotations. This has to exist outside of the tvar graph because we will
    // eventually not use unresolved tvars to represent unannotated parameters. We use an ALocFuzzyMap
    // because we may compare keyed and concrete locations
    missing_local_annot_lower_bounds: RefCell<ALocFuzzyMap<Vec<Type>>>,
    // Used to power an autofix that takes the upper bounds of array & object literal declarations,
    // and turn them into annotations.
    array_or_object_literal_declaration_upper_bounds: RefCell<IntHashMap<i32, (ALoc, Vec<Type>)>>,
    array_or_object_literal_declaration_reposition_tracking: RefCell<IntHashMap<i32, i32>>,
    // Used to power a code action to automatically insert appropriate render type.
    // It is key-ed by the body_loc of components.
    inferred_component_return: RefCell<ALocFuzzyMap<Vec1<Type>>>,
    exhaustive_checks: RefCell<ALocMap<(Vec<ALoc>, bool)>>,
    in_implicit_instantiation: RefCell<bool>,
    // React$Element does not store the monomorphized version of a component to support
    // cloning polymorphic elements. We need to know the monomorphized version of a component
    // to determine the render type of an element of a polymorphic component, so we keep track
    // of the monomorphized version here by mapping the Element's object id to the monomorphized
    // component
    monomorphized_components: RefCell<HashMap<type_::properties::Id, Type>>,
    // Signature help
    signature_help_callee: RefCell<ALocMap<Type>>,
    // Hover type
    ctor_callee: RefCell<ALocMap<Type>>,
    // Union optimization checks
    union_opt: RefCell<ALocMap<Type>>,
    // Natural inference (records checks on primitive literal types during
    // implicit instantiation)
    primitive_literal_checks: RefCell<ALocSet>,
    enclosing_context_for_call: RefCell<ALocMap<EnclosingContext>>,
}

#[derive(Debug, Clone, Dupe)]
pub enum TypingMode {
    CheckingMode,
    SynthesisMode { target_loc: Option<ALoc> },
    HintEvaluationMode,
}

impl TypingMode {
    fn show_frame(&self) -> String {
        match self {
            TypingMode::CheckingMode => "CheckingMode".to_string(),
            TypingMode::SynthesisMode {
                target_loc: Some(loc),
            } => format!("SynthesisMode (target={})", string_of_aloc(None, loc)),
            TypingMode::SynthesisMode { target_loc: None } => {
                "SynthesisMode (no target)".to_string()
            }
            TypingMode::HintEvaluationMode => "HintEvaluationMode".to_string(),
        }
    }

    pub fn show(frames: &[Self]) -> String {
        frames
            .iter()
            .map(Self::show_frame)
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[derive(Clone, Dupe)]
pub enum ResolvedRequire<'cx> {
    /// The closure takes (cx, dst_cx) where cx is the source context and
    /// dst_cx is the destination context for error reporting.
    TypedModule(Rc<dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type> + 'cx>),
    UncheckedModule(ALoc),
    MissingModule,
}

struct ContextInner<'cx> {
    ccx: Rc<ComponentT<'cx>>,
    file: FileKey,
    aloc_table: LazyALocTable,
    metadata: Metadata,
    budget: CheckBudget,
    /// During cross-file merge (copy_into), annotation inference errors must be
    /// reported on the destination context, not the source. This field stores a
    /// weak reference to the destination context, set only during merge and None
    /// otherwise. When None, callers should use cx itself as dst_cx.
    merge_dst_cx: RefCell<Option<Weak<ContextInner<'cx>>>>,
    resolve_require: RefCell<Option<ResolveRequire<'cx>>>,
    builtins: flow_lazy::Lazy<
        Context<'cx>,
        Builtins<'cx, Context<'cx>>,
        Box<dyn FnOnce(&Context<'cx>) -> Builtins<'cx, Context<'cx>> + 'cx>,
    >,
    hint_map_arglist_cache: RefCell<HashMap<(ALoc, Option<ALoc>), Vec<(ALoc, type_::CallArg)>>>,
    hint_map_jsx_cache: RefCell<
        HashMap<
            (Reason, String, Vec<ALoc>, ALoc),
            Rc<flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>>,
        >,
    >,
    hint_eval_cache: RefCell<IntHashMap<i32, Option<Type>>>,
    environment: RefCell<LocEnv<'cx, Context<'cx>>>,
    typing_mode: RefCell<TypingMode>,
    //  A subset of all transitive dependencies of the current file as determined by import/require.
    // This set will only be populated with type sig files that are actually forced.
    reachable_deps: RefCell<BTreeSet<FileKey>>,
    refined_locations: RefCell<ALocMap<FlowOrdSet<ALoc>>>,
    aggressively_invalidated_locations: RefCell<ALocMap<RefinementInvalidation>>,
    switch_to_match_eligible_locations: RefCell<ALocSet>,
    node_cache: NodeCache<'cx, Context<'cx>>,
    ts_import_provenance: RefCell<ALocMap<(FlowImportSpecifier, FlowSmolStr)>>,
}

#[derive(Clone, Dupe)]
pub struct Context<'cx>(Rc<ContextInner<'cx>>);

pub type ResolveRequire<'cx> =
    Rc<dyn Fn(&Context<'cx>, &FlowImportSpecifier) -> ResolvedRequire<'cx> + 'cx>;

pub fn metadata_of_options(options: &Options) -> Metadata {
    Metadata {
        overridable: OverridableMetadata {
            checked: options.all,
            jsx: JsxMode::JsxReact,
            munge_underscores: options.munge_underscores,
            strict: false,
            strict_local: false,
            available_platforms: None,
            has_explicit_supports_platform: false,
            react_runtime: options.react_runtime,
        },
        frozen: Rc::new(FrozenMetadata {
            include_suppressions: options.include_suppressions,
            verbose: options.verbose.dupe(),
            slow_to_check_logging: options.slow_to_check_logging,

            abstract_classes: options.abstract_classes,
            automatic_require_default: options.automatic_require_default,
            babel_loose_array_spread: options.babel_loose_array_spread,
            casting_syntax: options.casting_syntax,
            casting_syntax_only_support_as_excludes: options
                .casting_syntax_only_support_as_excludes
                .dupe(),
            component_syntax: options.component_syntax,
            async_component_syntax: options.async_component_syntax,
            async_component_syntax_includes: options.async_component_syntax_includes.dupe(),
            deprecated_utilities_excludes: options.deprecated_utilities_excludes.dupe(),
            hook_compatibility_excludes: options.hook_compatibility_excludes.dupe(),
            hook_compatibility_includes: options.hook_compatibility_includes.dupe(),
            hook_compatibility: options.hook_compatibility,
            instance_t_objkit_fix: options.instance_t_objkit_fix,
            records_includes: options
                .records_includes
                .iter()
                .filter_map(|s| Regex::new(s).ok())
                .collect::<Vec<_>>()
                .into(),
            dev_only_refinement_info_as_errors: options.dev_only_refinement_info_as_errors,
            enable_const_params: options.enable_const_params,
            enable_custom_error: options.enable_custom_error,
            enable_enums: options.enums,
            enable_jest_integration: options.enable_jest_integration,
            enable_pattern_matching: options.enable_pattern_matching,
            enable_pattern_matching_instance_patterns: options
                .enable_pattern_matching_instance_patterns,
            enable_records: options.enable_records,
            enable_relay_integration: options.enable_relay_integration,
            exact_by_default: options.exact_by_default,
            facebook_fbs: options.facebook_fbs.dupe(),
            facebook_fbt: options.facebook_fbt.dupe(),
            facebook_module_interop: options.facebook_module_interop,
            file_options: options.file_options.dupe(),
            ignore_non_literal_requires: options.ignore_non_literal_requires,
            max_workers: options.max_workers,
            missing_module_generators: options.missing_module_generators.dupe(),
            no_unchecked_indexed_access: options.no_unchecked_indexed_access,
            opaque_type_new_bound_syntax: options.opaque_type_new_bound_syntax,
            projects_options: options.projects_options.dupe(),
            react_custom_jsx_typing: options.react_custom_jsx_typing,
            react_ref_as_prop: options.react_ref_as_prop,
            react_rules: options.react_rules.dupe(),
            recursion_limit: options.recursion_limit,
            relay_integration_esmodules: options.relay_integration_esmodules,
            relay_integration_excludes: options.relay_integration_excludes.dupe(),
            relay_integration_module_prefix: options.relay_integration_module_prefix.dupe(),
            relay_integration_module_prefix_includes: options
                .relay_integration_module_prefix_includes
                .dupe(),
            root: options.root.dupe(),
            strict_es6_import_export: options.strict_es6_import_export,
            strip_root: options.strip_root,
            stylex_shorthand_prop: options.stylex_shorthand_prop.clone(),
            ts_syntax: options.ts_syntax,
            allow_readonly_variance: options.allow_readonly_variance,
            allow_variance_keywords: options.allow_variance_keywords,
            deprecated_colon_extends: options.deprecated_colon_extends.dupe(),
            deprecated_colon_extends_excludes: options.deprecated_colon_extends_excludes.dupe(),
            ts_utility_syntax: options.ts_utility_syntax,
            tslib_syntax: options.tslib_syntax,
            typescript_library_definition_support: options.typescript_library_definition_support,
            deprecated_utilities: options.deprecated_utilities.dupe(),
            assert_operator: options.assert_operator,
            type_expansion_recursion_limit: options.type_expansion_recursion_limit,
            use_unknown_in_catch_variables: options.use_unknown_in_catch_variables,
            ban_spread_key_props: options.ban_spread_key_props,
        }),
    }
}

pub fn docblock_overrides(
    docblock_info: &Docblock,
    file_key: &FileKey,
    mut metadata: Metadata,
) -> Metadata {
    let jsx = match docblock_info.jsx() {
        Some(jsx_pragma) => {
            let mut mapper = LocToALocMapper;
            let Ok(jsx_expr) =
                polymorphic_ast_mapper::expression(&mut mapper, &jsx_pragma.expression);
            JsxMode::JsxPragma(jsx_pragma.raw.clone(), jsx_expr)
        }
        None => JsxMode::JsxReact,
    };
    metadata.overridable.jsx = jsx;

    match docblock_info.jsx_runtime() {
        Some(JsxRuntimePragma::Classic) => {
            metadata.overridable.react_runtime = ReactRuntime::Classic
        }
        Some(JsxRuntimePragma::Automatic) => {
            metadata.overridable.react_runtime = ReactRuntime::Automatic
        }
        None => {}
    }

    match docblock_info.flow() {
        None => {}
        Some(FlowMode::OptIn) => {
            metadata.overridable.checked = true;
        }
        Some(FlowMode::OptInStrict) => {
            metadata.overridable.checked = true;
            metadata.overridable.strict = true;
        }
        Some(FlowMode::OptInStrictLocal) => {
            metadata.overridable.checked = true;
            metadata.overridable.strict_local = true;
        }
        // --all (which sets metadata.checked = true) overrides @noflow, so there are
        // currently no scenarios where we'd change checked = true to false. in the
        // future, there may be a case where checked defaults to true (but is not
        // forced to be true ala --all), but for now we do *not* want to force
        // checked = false here.
        Some(FlowMode::OptOut) => {}
    }

    if docblock_info.prevent_munge() {
        metadata.overridable.munge_underscores = false;
    }

    let explicit_available_platforms = docblock_info.supports_platform();
    let available_platforms = flow_common::platform_set::available_platforms(
        &metadata.frozen.file_options,
        &metadata.frozen.projects_options,
        file_key.as_str(),
        explicit_available_platforms,
    );
    metadata.overridable.available_platforms = available_platforms;
    metadata.overridable.has_explicit_supports_platform = explicit_available_platforms.is_some();

    if flow_common::files::has_ts_ext(file_key) {
        // .ts files are always checked, consistent with `types_checked` in `parsing_service_js`
        metadata.overridable.checked = true;
        let frozen = Rc::make_mut(&mut metadata.frozen);
        frozen.ts_syntax = true;
        frozen.tslib_syntax = true;
        frozen.abstract_classes = true;
        frozen.ts_utility_syntax = true;
    }

    metadata
}

pub fn empty_sig_cx<'cx>() -> TypeContext<'cx, Context<'cx>> {
    thread_local! {
        static CACHED_EVALUATED: type_::eval::Map<Type> = Default::default();
    }
    TypeContext {
        graph: Rc::new(RefCell::new(flow_utils_union_find::Graph::with_capacity(
            1024,
        ))),
        property_maps: BTreeMap::new(),
        call_props: IntHashMap::with_capacity_and_hasher(128, Default::default()),
        export_maps: HashMap::with_capacity(4),
        evaluated: CACHED_EVALUATED.with(|c| c.clone()),
    }
}

pub fn make_ccx<'cx>() -> ComponentT<'cx> {
    thread_local! {
        static CACHED_ORD_SET: FlowOrdSet<i32> = FlowOrdSet::new();
        static CACHED_SEVERITY_COVER: FlowOrdMap<FileKey, LintSeverityCover> = FlowOrdMap::new();
    }
    ComponentT {
        sig_cx: RefCell::new(empty_sig_cx()),
        aloc_tables: RefCell::new(HashMap::new()),
        synthesis_produced_uncacheable_result: RefCell::new(false),
        delayed_forcing_tvars: RefCell::new(CACHED_ORD_SET.with(|c| c.clone())),
        post_component_tvar_forcing_states: RefCell::new(FlowVector::new()),
        post_inference_polarity_checks: RefCell::new(Vec::new()),
        post_inference_validation_flows: RefCell::new(VecDeque::new()),
        post_inference_projects_strict_boundary_import_pattern_opt_outs_validations: RefCell::new(
            Vec::new(),
        ),
        merging_interface_field_types: RefCell::new(HashMap::new()),
        interface_prop_ids: RefCell::new(BTreeMap::new()),
        env_value_cache: RefCell::new(IntHashMap::default()),
        env_type_cache: RefCell::new(IntHashMap::default()),
        missing_local_annot_lower_bounds: RefCell::new(BTreeMap::new()),
        array_or_object_literal_declaration_upper_bounds: RefCell::new(IntHashMap::default()),
        array_or_object_literal_declaration_reposition_tracking: RefCell::new(IntHashMap::default()),
        inferred_component_return: RefCell::new(BTreeMap::new()),
        errors: RefCell::new(ErrorSet::empty()),
        error_suppressions: RefCell::new(ErrorSuppressions::empty()),
        severity_cover: RefCell::new(CACHED_SEVERITY_COVER.with(|c| c.clone())),
        exists_checks: RefCell::new(ALocMap::new()),
        exists_excuses: RefCell::new(ALocMap::new()),
        voidable_checks: RefCell::new(Vec::new()),
        test_prop_hits_and_misses: RefCell::new(IntHashMap::default()),
        optional_chains_useful: RefCell::new(ALocMap::new()),
        conditions: RefCell::new(FlowVector::new()),
        strict_comparisons: RefCell::new(FlowVector::new()),
        maybe_unused_promises: RefCell::new(FlowVector::new()),
        constraint_cache: RefCell::new(type_::FlowSet::default()),
        subst_cache: RefCell::new(Default::default()),
        eval_id_cache: RefCell::new(Default::default()),
        id_cache: RefCell::new(Default::default()),
        eval_repos_cache: RefCell::new(Default::default()),
        fix_cache: RefCell::new(Default::default()),
        spread_cache: RefCell::new(SpreadCache::new()),
        instantiation_stack: RefCell::new(FlowVector::new()),
        const_fold_cache: RefCell::new(Default::default()),
        speculation_state: RefCell::new(SpeculationState(Vec::new())),
        annot_graph: RefCell::new(IntHashMap::default()),
        exhaustive_checks: RefCell::new(ALocMap::new()),
        in_implicit_instantiation: RefCell::new(false),
        monomorphized_components: RefCell::new(HashMap::new()),
        signature_help_callee: RefCell::new(ALocMap::new()),
        ctor_callee: RefCell::new(ALocMap::new()),
        union_opt: RefCell::new(ALocMap::new()),
        primitive_literal_checks: RefCell::new(ALocSet::new()),
        enclosing_context_for_call: RefCell::new(ALocMap::new()),
    }
}

impl<'cx> Context<'cx> {
    pub fn make(
        ccx: Rc<ComponentT<'cx>>,
        metadata: Metadata,
        file: FileKey,
        aloc_table: LazyALocTable,
        resolve_require: ResolveRequire<'cx>,
        mk_builtins: Rc<dyn Fn(&Context<'cx>) -> Builtins<'cx, Context<'cx>> + 'cx>,
        budget: CheckBudget,
    ) -> Self {
        ccx.aloc_tables
            .borrow_mut()
            .insert(file.dupe(), aloc_table.dupe());
        let inner = Rc::new(ContextInner {
            ccx,
            file,
            aloc_table,
            metadata,
            budget,
            merge_dst_cx: RefCell::new(None),
            resolve_require: RefCell::new(Some(resolve_require)),
            builtins: flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| mk_builtins(cx))),
            hint_map_arglist_cache: RefCell::new(HashMap::new()),
            hint_map_jsx_cache: RefCell::new(HashMap::new()),
            hint_eval_cache: RefCell::new(IntHashMap::default()),
            environment: RefCell::new(LocEnv::empty(ScopeKind::Global)),
            typing_mode: RefCell::new(TypingMode::CheckingMode),
            reachable_deps: RefCell::new(BTreeSet::new()),
            node_cache: NodeCache::mk_empty(),
            refined_locations: RefCell::new(ALocMap::new()),
            aggressively_invalidated_locations: RefCell::new(ALocMap::new()),
            switch_to_match_eligible_locations: RefCell::new(ALocSet::new()),
            ts_import_provenance: RefCell::new(ALocMap::new()),
        });
        Self(inner)
    }

    /// Returns the merge-phase destination context if set, otherwise None.
    /// During cross-file merge, errors should be reported on the destination
    /// context. Outside of merge, callers should use cx itself as dst_cx.
    pub fn merge_dst_cx(&self) -> Option<Context<'cx>> {
        self.0
            .merge_dst_cx
            .borrow()
            .as_ref()
            .and_then(|w| w.upgrade().map(Context))
    }

    /// Set the merge-phase destination context.
    pub fn set_merge_dst_cx(&self, dst_cx: &Context<'cx>) {
        *self.0.merge_dst_cx.borrow_mut() = Some(Rc::downgrade(&dst_cx.0));
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.0)
    }

    /// Break per-file Rc reference cycles in this Context.
    /// Safe to call even when other files sharing the same ComponentT are still
    /// in use, because this only clears per-file data (not shared component data).
    pub fn post_inference_cleanup_per_file(&self) {
        // resolve_require closures capture cx
        *self.0.resolve_require.borrow_mut() = None;

        // TypeEntry.state lazy closures in environment capture cx.
        // Replace each Lazy (which may hold a closure capturing cx) with one
        // that is already forced to entry.t, releasing the closure.
        {
            let entries = {
                let env = self.0.environment.borrow();
                env.types
                    .iter()
                    .map(|(_, entry)| (entry.state.dupe(), entry.t.dupe()))
                    .collect::<Vec<_>>()
            };
            for (state, t) in entries {
                *state.borrow_mut() = flow_lazy::Lazy::new_forced(t);
            }
        }

        // hint_map LazyHintCompute closures (Rc<dyn Fn>),
        // pred_func_map Lazy closures, and hint_map_jsx_cache — all capture cx
        {
            let mut env = self.0.environment.borrow_mut();
            env.hint_map = flow_env_builder::env_api::EnvMap::empty();
            env.pred_func_map = Default::default();
        }
        self.0.hint_map_jsx_cache.borrow_mut().clear();

        // node_cache contains Rc<dyn Fn> closures (function_sigs Reconstruct,
        // class_sigs, record_sigs, component_sigs) that may capture cx.dupe(),
        // creating cycles: Context → node_cache → Rc<dyn Fn> → cx → Context
        self.0.node_cache.clear();
    }

    /// Break all Rc reference cycles in this Context, including shared
    /// component data. Only call when the Context is being fully discarded
    /// (not when other files sharing the same ComponentT may still be in use).
    pub fn post_inference_cleanup(&self) {
        self.post_inference_cleanup_per_file();

        // ForcingState closures in the shared type graph capture cx.
        // Also clears Unresolved bounds that may contain UseT values with
        // LazyHintT closures capturing cx.
        self.0.ccx.sig_cx.borrow().drop_lazy_forcing_states();

        // post_component_tvar_forcing_states closures capture cx
        self.0
            .ccx
            .post_component_tvar_forcing_states
            .borrow_mut()
            .clear();

        // constraint_cache contains (Type, UseT) pairs. UseT variants like
        // CallT, GetPropT, ConstructorT embed LazyHintT(Rc<dyn Fn>) closures
        // that capture cx, forming:
        // Context → ccx → constraint_cache → UseT → LazyHintT → cx → Context
        *self.0.ccx.constraint_cache.borrow_mut() = Default::default();

        // post_inference_validation_flows also contains UseT values with
        // LazyHintT closures that capture cx.
        self.0
            .ccx
            .post_inference_validation_flows
            .borrow_mut()
            .clear();
    }

    pub fn sig_cx(&self) -> std::cell::Ref<'_, TypeContext<'cx, Context<'cx>>> {
        self.0.ccx.sig_cx.borrow()
    }

    // modules

    pub fn in_declare_module(&self) -> bool {
        self.0.environment.borrow().scope_kind == ScopeKind::DeclareModule
    }

    pub fn in_declare_namespace(&self) -> bool {
        self.0.environment.borrow().scope_kind == ScopeKind::DeclareNamespace
    }

    // accessors

    pub fn metadata(&self) -> &Metadata {
        &self.0.metadata
    }

    pub fn babel_loose_array_spread(&self) -> bool {
        self.0.metadata.frozen.babel_loose_array_spread
    }

    pub fn file(&self) -> &FileKey {
        &self.0.file
    }

    pub fn check_budget(&self) -> Result<(), JobError> {
        self.0.budget.check()
    }

    /// Returns the per-file budget so dependency contexts (e.g., those built
    /// inside `dep_file`) can inherit the parent file's cancel + timeout
    /// budget rather than starting a fresh one.
    pub fn budget(&self) -> CheckBudget {
        self.0.budget
    }

    pub fn is_lib_file(&self) -> bool {
        self.0.file.is_lib_file()
    }

    pub fn under_declaration_context(&self) -> bool {
        self.0.file.is_lib_file()
            || self.0.file.check_suffix(".flow")
            || files::has_dts_ext(&self.0.file)
            || self.in_declare_module()
            || self.in_declare_namespace()
    }

    fn in_dirlist(&self, dirs: &[Regex]) -> bool {
        match dirs {
            [] => false,
            _ => {
                let filename = self.0.file.to_absolute();
                let normalized_filename =
                    flow_common::sys_utils::normalize_filename_dir_sep(&filename);
                dirs.iter().any(|r| r.is_match(&normalized_filename))
            }
        }
    }

    pub fn builtins(&self) -> &Builtins<'cx, Context<'cx>> {
        self.0.builtins.get_forced(self)
    }

    pub fn builtin_value_opt(&self, name: &str) -> Option<(ALoc, Type)> {
        self.builtins().get_builtin_value_opt(self, name)
    }

    pub fn builtin_type_opt(&self, name: &str) -> Option<(ALoc, Type)> {
        self.builtins().get_builtin_type_opt(self, name)
    }

    pub fn builtin_module_opt(
        &self,
        name: &Userland,
    ) -> Option<(Reason, LazyModuleType<'cx, Context<'cx>>)> {
        self.builtins().get_builtin_module_opt(self, name)
    }

    pub fn casting_syntax(&self) -> CastingSyntax {
        match self.0.metadata.frozen.casting_syntax {
            CastingSyntax::Both => CastingSyntax::Both,
            CastingSyntax::As => {
                if self.in_dirlist(
                    &self
                        .0
                        .metadata
                        .frozen
                        .casting_syntax_only_support_as_excludes,
                ) {
                    CastingSyntax::Both
                } else {
                    CastingSyntax::As
                }
            }
        }
    }

    pub fn component_syntax(&self) -> bool {
        self.0.metadata.frozen.component_syntax || self.0.file.is_lib_file()
    }

    pub fn async_component_syntax(&self) -> bool {
        self.in_dirlist(&self.0.metadata.frozen.async_component_syntax_includes)
            || self.0.metadata.frozen.async_component_syntax
    }

    pub fn hook_compatibility(&self) -> bool {
        self.in_dirlist(&self.0.metadata.frozen.hook_compatibility_includes)
            || (self.0.metadata.frozen.hook_compatibility
                && !self.in_dirlist(&self.0.metadata.frozen.hook_compatibility_excludes))
    }

    pub fn react_rule_enabled(&self, rule: ReactRule) -> bool {
        self.0.metadata.frozen.react_rules.contains(&rule)
    }

    pub fn dev_only_refinement_info_as_errors(&self) -> bool {
        self.0.metadata.frozen.dev_only_refinement_info_as_errors
    }

    pub fn enable_const_params(&self) -> bool {
        self.0.metadata.frozen.enable_const_params
            || self.0.metadata.overridable.strict
            || self.0.metadata.overridable.strict_local
    }

    pub fn enable_custom_error(&self) -> bool {
        self.0.metadata.frozen.enable_custom_error
    }

    pub fn enable_enums(&self) -> bool {
        self.0.metadata.frozen.enable_enums
    }

    pub fn enable_jest_integration(&self) -> bool {
        self.0.metadata.frozen.enable_jest_integration
    }

    pub fn enable_pattern_matching(&self) -> bool {
        self.0.metadata.frozen.enable_pattern_matching
    }

    pub fn enable_pattern_matching_instance_patterns(&self) -> bool {
        self.0
            .metadata
            .frozen
            .enable_pattern_matching_instance_patterns
            && match &self.0.metadata.frozen.records_includes[..] {
                [] => true,
                dirs => self.in_dirlist(dirs),
            }
    }

    pub fn enable_records(&self) -> bool {
        self.0.metadata.frozen.enable_records
            && match &self.0.metadata.frozen.records_includes[..] {
                [] => true,
                dirs => self.in_dirlist(dirs),
            }
    }

    pub fn is_utility_type_deprecated(&self, t: &str) -> bool {
        if self.is_lib_file() {
            return false;
        }
        match self.0.metadata.frozen.deprecated_utilities.get(t) {
            None => false,
            Some(dirs) => {
                let filename = self.0.file.to_absolute();
                let normalized_filename =
                    flow_common::sys_utils::normalize_filename_dir_sep(&filename);
                let is_excluded = self
                    .0
                    .metadata
                    .frozen
                    .deprecated_utilities_excludes
                    .iter()
                    .any(|r| r.is_match(&normalized_filename));
                !is_excluded
                    && dirs
                        .iter()
                        .any(|prefix| normalized_filename.starts_with(prefix))
            }
        }
    }

    pub fn enable_relay_integration(&self) -> bool {
        self.0.metadata.frozen.enable_relay_integration
            && flow_common::relay_options::enabled_for_file(
                &self.0.metadata.frozen.relay_integration_excludes,
                &self.0.file,
            )
    }

    pub fn relay_integration_esmodules(&self) -> bool {
        self.0.metadata.frozen.relay_integration_esmodules
    }

    pub fn relay_integration_module_prefix(&self) -> Option<String> {
        flow_common::relay_options::module_prefix_for_file(
            &self
                .0
                .metadata
                .frozen
                .relay_integration_module_prefix_includes,
            &self.0.file,
            self.0
                .metadata
                .frozen
                .relay_integration_module_prefix
                .as_deref(),
        )
    }

    pub fn errors(&self) -> ErrorSet {
        self.0.ccx.errors.borrow().dupe()
    }

    pub fn error_suppressions(&self) -> std::cell::Ref<'_, ErrorSuppressions> {
        self.0.ccx.error_suppressions.borrow()
    }

    pub fn take_error_suppressions(&self) -> ErrorSuppressions {
        std::mem::take(&mut *self.0.ccx.error_suppressions.borrow_mut())
    }

    pub fn evaluated(&self) -> type_::eval::Map<Type> {
        self.0.ccx.sig_cx.borrow().evaluated.dupe()
    }

    pub fn exact_by_default(&self) -> bool {
        self.0.metadata.frozen.exact_by_default
    }

    pub fn file_options(&self) -> Arc<FileOptions> {
        self.0.metadata.frozen.file_options.dupe()
    }

    pub fn aloc_tables(&self) -> std::cell::Ref<'_, HashMap<FileKey, LazyALocTable>> {
        self.0.ccx.aloc_tables.borrow()
    }

    pub fn find_props(&self, id: type_::properties::Id) -> type_::properties::PropertiesMap {
        self.0
            .ccx
            .sig_cx
            .borrow()
            .property_maps
            .get(&id)
            .cloned()
            .ok_or(PropsNotFound(id))
            .unwrap()
    }

    pub fn find_props_opt(
        &self,
        id: type_::properties::Id,
    ) -> Option<type_::properties::PropertiesMap> {
        self.0.ccx.sig_cx.borrow().property_maps.get(&id).cloned()
    }

    pub fn find_call(&self, id: i32) -> Type {
        self.0
            .ccx
            .sig_cx
            .borrow()
            .call_props
            .get(&id)
            .cloned()
            .ok_or(CallNotFound(id))
            .unwrap()
    }

    pub fn find_exports(&self, id: type_::exports::Id) -> type_::exports::T {
        self.0
            .ccx
            .sig_cx
            .borrow()
            .export_maps
            .get(&id)
            .cloned()
            .ok_or(ExportsNotFound(id))
            .unwrap()
    }

    pub fn find_require(&self, mref: &FlowImportSpecifier) -> ResolvedRequire<'cx> {
        let rr = self
            .0
            .resolve_require
            .borrow()
            .as_ref()
            .expect("resolve_require already cleaned up")
            .dupe();
        (rr)(self, mref)
    }

    pub fn find_tvar(
        &self,
        id: i32,
    ) -> flow_utils_union_find::Node<type_::constraint::Constraints<'cx, Context<'cx>>> {
        self.0
            .ccx
            .sig_cx
            .borrow()
            .graph
            .borrow()
            .get(&id)
            .cloned()
            .ok_or(TvarNotFound(id))
            .unwrap()
    }

    pub fn graph(
        &self,
    ) -> Rc<RefCell<flow_utils_union_find::Graph<type_::constraint::Constraints<'cx, Context<'cx>>>>>
    {
        self.0.ccx.sig_cx.borrow().graph.dupe()
    }

    pub fn in_implicit_instantiation(&self) -> bool {
        *self.0.ccx.in_implicit_instantiation.borrow()
    }

    pub fn is_checked(&self) -> bool {
        self.0.metadata.overridable.checked
    }

    pub fn is_projects_strict_boundary_import_pattern_opt_outs(
        &self,
        import_specifier: &Userland,
    ) -> bool {
        if flow_common::files::haste_name_opt(&self.0.metadata.frozen.file_options, &self.0.file)
            .is_some()
        {
            let projects_options = &self.0.metadata.frozen.projects_options;
            let file = self.0.file.as_str();
            let import_specifier_str = import_specifier.as_str();
            projects_options.is_common_code_path(file)
                && projects_options
                    .is_import_specifier_that_opt_out_of_strict_boundary(import_specifier_str)
        } else {
            false
        }
    }

    pub fn is_verbose(&self) -> bool {
        match &self.0.metadata.frozen.verbose {
            None => false,
            Some(v) if v.focused_files.is_none() => v.enabled_during_flowlib || !self.is_lib_file(),
            Some(v) => {
                let file = &self.0.file;
                if file.is_lib_file() {
                    v.enabled_during_flowlib
                } else {
                    v.focused_files.as_ref().is_some_and(|files| {
                        let abs = file.to_absolute();
                        files.iter().any(|f| f == &abs)
                    })
                }
            }
        }
    }

    pub fn is_strict(&self) -> bool {
        self.in_declare_module() || self.0.metadata.overridable.strict
    }

    pub fn is_strict_local(&self) -> bool {
        self.0.metadata.overridable.strict_local
    }

    pub fn available_platforms(&self) -> Option<&PlatformSet> {
        self.0.metadata.overridable.available_platforms.as_ref()
    }

    pub fn has_explicit_supports_platform(&self) -> bool {
        self.0.metadata.overridable.has_explicit_supports_platform
    }

    pub fn include_suppressions(&self) -> bool {
        self.0.metadata.frozen.include_suppressions
    }

    pub fn severity_cover(&self) -> std::cell::Ref<'_, FlowOrdMap<FileKey, LintSeverityCover>> {
        self.0.ccx.severity_cover.borrow()
    }

    pub fn has_property_map(&self, id: &type_::properties::Id) -> bool {
        self.0.ccx.sig_cx.borrow().property_maps.contains_key(id)
    }

    pub fn has_call_prop(&self, id: &i32) -> bool {
        self.0.ccx.sig_cx.borrow().call_props.contains_key(id)
    }

    pub fn has_export_map(&self, id: &type_::exports::Id) -> bool {
        self.0.ccx.sig_cx.borrow().export_maps.contains_key(id)
    }

    pub fn projects_options(&self) -> &ProjectsOptions {
        &self.0.metadata.frozen.projects_options
    }

    pub fn react_custom_jsx_typing(&self) -> bool {
        self.0.metadata.frozen.react_custom_jsx_typing
    }

    pub fn react_ref_as_prop(&self) -> ReactRefAsProp {
        self.0.metadata.frozen.react_ref_as_prop
    }

    pub fn react_runtime(&self) -> ReactRuntime {
        self.0.metadata.overridable.react_runtime
    }

    pub fn recursion_limit(&self) -> i32 {
        self.0.metadata.frozen.recursion_limit
    }

    pub fn root(&self) -> &PathBuf {
        &self.0.metadata.frozen.root
    }

    pub fn facebook_fbs(&self) -> Option<&str> {
        self.0.metadata.frozen.facebook_fbs.as_deref()
    }

    pub fn facebook_fbt(&self) -> Option<&str> {
        self.0.metadata.frozen.facebook_fbt.as_deref()
    }

    pub fn facebook_module_interop(&self) -> bool {
        self.0.metadata.frozen.facebook_module_interop
    }

    pub fn should_ignore_non_literal_requires(&self) -> bool {
        self.0.metadata.frozen.ignore_non_literal_requires
    }

    pub fn should_munge_underscores(&self) -> bool {
        self.0.metadata.overridable.munge_underscores
    }

    pub fn should_strip_root(&self) -> bool {
        self.0.metadata.frozen.strip_root
    }

    pub fn stylex_shorthand_prop(&self) -> Option<&str> {
        self.0.metadata.frozen.stylex_shorthand_prop.as_deref()
    }

    pub fn ts_syntax(&self) -> bool {
        self.0.metadata.frozen.ts_syntax
    }

    pub fn allow_readonly_variance(&self) -> bool {
        self.0.metadata.frozen.allow_readonly_variance
    }

    pub fn allow_variance_keywords(&self) -> bool {
        self.0.metadata.frozen.allow_variance_keywords
    }

    pub fn is_colon_extends_deprecated(&self) -> bool {
        if self.is_lib_file() {
            return false;
        }
        match &*self.0.metadata.frozen.deprecated_colon_extends {
            [] => false,
            dirs => {
                let filename = self.0.file.to_absolute();
                let normalized_filename =
                    flow_common::sys_utils::normalize_filename_dir_sep(&filename);
                let is_excluded = self
                    .0
                    .metadata
                    .frozen
                    .deprecated_colon_extends_excludes
                    .iter()
                    .any(|r: &Regex| r.is_match(&normalized_filename));
                !is_excluded
                    && dirs
                        .iter()
                        .any(|prefix| normalized_filename.starts_with(prefix.as_str()))
            }
        }
    }

    pub fn ts_utility_syntax(&self) -> bool {
        self.0.metadata.frozen.ts_utility_syntax
    }

    pub fn tslib_syntax(&self) -> bool {
        self.0.metadata.frozen.tslib_syntax
    }

    pub fn typescript_library_definition_support(&self) -> bool {
        self.0.metadata.frozen.typescript_library_definition_support
    }

    pub fn assert_operator_enabled(&self) -> bool {
        self.0.metadata.frozen.assert_operator.usable()
    }

    pub fn assert_operator_specialized(&self) -> bool {
        self.0.metadata.frozen.assert_operator.specialized()
    }

    pub fn opaque_type_new_bound_syntax(&self) -> bool {
        self.0.metadata.frozen.opaque_type_new_bound_syntax
    }

    pub fn type_expansion_recursion_limit(&self) -> i32 {
        self.0.metadata.frozen.type_expansion_recursion_limit
    }

    pub fn delayed_forcing_tvars(&self) -> std::cell::Ref<'_, FlowOrdSet<i32>> {
        self.0.ccx.delayed_forcing_tvars.borrow()
    }

    pub fn post_component_tvar_forcing_states(
        &self,
    ) -> FlowVector<ForcingState<'cx, Context<'cx>>> {
        self.0
            .ccx
            .post_component_tvar_forcing_states
            .replace(FlowVector::new())
    }

    pub fn post_inference_polarity_checks(
        &self,
    ) -> std::cell::Ref<
        '_,
        Vec<(
            BTreeMap<SubstName, type_::TypeParam>,
            flow_common::polarity::Polarity,
            Type,
        )>,
    > {
        self.0.ccx.post_inference_polarity_checks.borrow()
    }

    pub fn post_inference_validation_flows(
        &self,
    ) -> std::cell::Ref<'_, VecDeque<(Type, type_::UseT<Context<'cx>>)>> {
        self.0.ccx.post_inference_validation_flows.borrow()
    }

    pub fn post_inference_projects_strict_boundary_import_pattern_opt_outs_validations(
        &self,
    ) -> std::cell::Ref<'_, Vec<(ALoc, String, Vec<FlowProjects>)>> {
        self.0
            .ccx
            .post_inference_projects_strict_boundary_import_pattern_opt_outs_validations
            .borrow()
    }

    /// Reserve a slot in `merging_interface_field_types` for every interface
    /// that participates in a merge, so later `record_interface_field` calls
    /// know who to record for in O(1). The set of participating interfaces is
    /// whatever env_builder reported in `interface_merge_conflicts`; we don't
    /// compute it ourselves.
    pub fn init_interface_merge_field_index(&self) {
        let mut table = self.0.ccx.merging_interface_field_types.borrow_mut();
        table.clear();
        let mut install = |good_loc: &ALoc, bad_locs: &Vec<ALoc>| {
            table.entry(good_loc.dupe()).or_insert_with(HashMap::new);
            for bad_loc in bad_locs {
                table.entry(bad_loc.dupe()).or_insert_with(HashMap::new);
            }
        };
        let interface_merge_conflicts =
            self.environment().var_info.interface_merge_conflicts.dupe();
        for (good_loc, bad_locs) in interface_merge_conflicts.iter() {
            install(good_loc, bad_locs);
        }
        let declare_class_interface_merge_conflicts = self
            .environment()
            .var_info
            .declare_class_interface_merge_conflicts
            .dupe();
        for (good_loc, bad_locs) in declare_class_interface_merge_conflicts.iter() {
            install(good_loc, bad_locs);
        }
    }

    /// Remember that interface `id_loc` declared a Field named `name` of type
    /// `t`. Only matters for interfaces that participate in declaration
    /// merging — for anyone else, the absence of a slot in the table makes
    /// this a no-op. If the same name shows up twice in one declaration (a
    /// syntax-level oddity that the rest of the diff treats as "first
    /// wins"), we keep the first type so the post-inference check sees the
    /// same type the merger sees.
    pub fn record_interface_field(&self, id_loc: ALoc, name: Name, t: Type) {
        let mut table = self.0.ccx.merging_interface_field_types.borrow_mut();
        if let Some(inner) = table.get_mut(&id_loc) {
            inner.entry(name).or_insert(t);
        }
    }

    /// The list of "these two types should be the same, and here's why"
    /// obligations that interface declaration merging produces. Each tuple
    /// is one shared field between two merging declarations: post-inference
    /// will `unify` them, and anyone who wrote disagreeing types gets a
    /// `MergedDeclaration` error pointing at both decls.
    pub fn interface_merge_unify_tasks(&self) -> Vec<(UseOp, Type, Type)> {
        let interface_conflicts = self.environment().var_info.interface_merge_conflicts.dupe();
        let dc_conflicts = self
            .environment()
            .var_info
            .declare_class_interface_merge_conflicts
            .dupe();
        let fields = self.0.ccx.merging_interface_field_types.borrow();
        let walk = |good_reason: &dyn Fn(ALoc) -> VirtualReason<ALoc>,
                    bad_reason: &dyn Fn(ALoc) -> VirtualReason<ALoc>,
                    conflicts: &FlowOrdMap<ALoc, Vec<ALoc>>,
                    acc: &mut Vec<(UseOp, Type, Type)>| {
            for (good_loc, bad_locs) in conflicts.iter() {
                let Some(good_fields) = fields.get(good_loc) else {
                    continue;
                };
                let first_decl = good_reason(good_loc.dupe());
                for bad_loc in bad_locs {
                    let Some(bad_fields) = fields.get(bad_loc) else {
                        continue;
                    };
                    let current_decl = bad_reason(bad_loc.dupe());
                    let use_op = UseOp::Op(Arc::new(RootUseOp::MergedDeclaration {
                        first_decl: first_decl.dupe(),
                        current_decl,
                    }));
                    for (name, bad_t) in bad_fields.iter() {
                        if let Some(good_t) = good_fields.get(name) {
                            acc.push((use_op.dupe(), bad_t.dupe(), good_t.dupe()));
                        }
                    }
                }
            }
        };
        let mut acc = Vec::new();
        walk(
            &|loc| VirtualReason::new(VirtualReasonDesc::RInterfaceType, loc),
            &|loc| VirtualReason::new(VirtualReasonDesc::RInterfaceType, loc),
            &interface_conflicts,
            &mut acc,
        );
        walk(
            &|loc| {
                VirtualReason::new(
                    VirtualReasonDesc::RClass(Arc::new(VirtualReasonDesc::RInterfaceType)),
                    loc,
                )
            },
            &|loc| VirtualReason::new(VirtualReasonDesc::RInterfaceType, loc),
            &dc_conflicts,
            &mut acc,
        );
        acc
    }

    pub fn interface_prop_ids(
        &self,
    ) -> std::cell::Ref<'_, BTreeMap<ALoc, (type_::properties::Id, type_::properties::Id)>> {
        self.0.ccx.interface_prop_ids.borrow()
    }

    /// Hand the merger a way to find this declaration's property maps later.
    /// The ids point at the heap-allocated maps that back its InstanceT; the
    /// merger takes those ids and copies properties between them when
    /// several decls of the same interface need to be folded together.
    pub fn add_interface_prop_ids(
        &self,
        loc: ALoc,
        own_props: type_::properties::Id,
        proto_props: type_::properties::Id,
    ) {
        self.0
            .ccx
            .interface_prop_ids
            .borrow_mut()
            .insert(loc, (own_props, proto_props));
    }

    pub fn env_cache_find_opt(
        &self,
        for_value: bool,
        id: i32,
    ) -> Option<PossiblyRefinedWriteState> {
        let cache = if for_value {
            self.0.ccx.env_value_cache.borrow()
        } else {
            self.0.ccx.env_type_cache.borrow()
        };
        cache.get(&id).cloned()
    }

    pub fn missing_local_annot_lower_bounds(&self) -> std::cell::Ref<'_, ALocFuzzyMap<Vec<Type>>> {
        self.0.ccx.missing_local_annot_lower_bounds.borrow()
    }

    pub fn array_or_object_literal_declaration_upper_bounds(&self) -> Vec<(ALoc, Vec<Type>)> {
        self.0
            .ccx
            .array_or_object_literal_declaration_upper_bounds
            .borrow()
            .values()
            .cloned()
            .collect()
    }

    pub fn inferred_component_return(&self) -> std::cell::Ref<'_, ALocFuzzyMap<Vec1<Type>>> {
        self.0.ccx.inferred_component_return.borrow()
    }

    pub fn use_unknown_in_catch_variables(&self) -> bool {
        self.0.metadata.frozen.use_unknown_in_catch_variables
    }

    pub fn ban_spread_key_props(&self) -> bool {
        self.0.metadata.frozen.ban_spread_key_props
    }

    pub fn verbose(&self) -> Option<&Verbose> {
        self.0.metadata.frozen.verbose.as_deref()
    }

    pub fn slow_to_check_logging(&self) -> &SlowToCheckLogging {
        &self.0.metadata.frozen.slow_to_check_logging
    }

    pub fn max_workers(&self) -> i32 {
        self.0.metadata.frozen.max_workers
    }

    pub fn missing_module_generators(&self) -> &[(Regex, String)] {
        &self.0.metadata.frozen.missing_module_generators
    }

    pub fn no_unchecked_indexed_access(&self) -> bool {
        self.0.metadata.frozen.no_unchecked_indexed_access
    }

    pub fn jsx(&self) -> &JsxMode {
        &self.0.metadata.overridable.jsx
    }

    pub fn exists_checks(&self) -> std::cell::Ref<'_, ALocMap<BTreeSet<Type>>> {
        self.0.ccx.exists_checks.borrow()
    }

    pub fn exists_excuses(&self) -> std::cell::Ref<'_, ALocMap<ExistsCheck>> {
        self.0.ccx.exists_excuses.borrow()
    }

    pub fn exists_excuses_mut(&self) -> std::cell::RefMut<'_, ALocMap<ExistsCheck>> {
        self.0.ccx.exists_excuses.borrow_mut()
    }

    pub fn voidable_checks(&self) -> std::cell::Ref<'_, Vec<VoidableCheck>> {
        self.0.ccx.voidable_checks.borrow()
    }

    pub fn reachable_deps(&self) -> std::cell::Ref<'_, BTreeSet<FileKey>> {
        self.0.reachable_deps.borrow()
    }

    pub fn environment(&self) -> std::cell::Ref<'_, LocEnv<'cx, Context<'cx>>> {
        self.0.environment.borrow()
    }

    pub fn environment_mut(&self) -> std::cell::RefMut<'_, LocEnv<'cx, Context<'cx>>> {
        self.0.environment.borrow_mut()
    }

    pub fn typing_mode(&self) -> std::cell::Ref<'_, TypingMode> {
        self.0.typing_mode.borrow()
    }

    pub fn node_cache(&self) -> &NodeCache<'cx, Context<'cx>> {
        &self.0.node_cache
    }

    pub fn refined_locations(&self) -> std::cell::Ref<'_, ALocMap<FlowOrdSet<ALoc>>> {
        self.0.refined_locations.borrow()
    }

    pub fn aggressively_invalidated_locations(
        &self,
    ) -> std::cell::Ref<'_, ALocMap<RefinementInvalidation>> {
        self.0.aggressively_invalidated_locations.borrow()
    }

    pub fn switch_to_match_eligible_locations(&self) -> std::cell::Ref<'_, ALocSet> {
        self.0.switch_to_match_eligible_locations.borrow()
    }

    pub fn add_ts_import_provenance(
        &self,
        def_loc: ALoc,
        source: FlowImportSpecifier,
        remote_name: FlowSmolStr,
    ) {
        self.0
            .ts_import_provenance
            .borrow_mut()
            .insert(def_loc, (source, remote_name));
    }

    pub fn find_ts_import_provenance(
        &self,
        def_loc: &ALoc,
    ) -> Option<(FlowImportSpecifier, FlowSmolStr)> {
        self.0
            .ts_import_provenance
            .borrow()
            .get(def_loc)
            .map(|(s, r)| (s.dupe(), r.dupe()))
    }

    pub fn hint_map_arglist_cache(
        &self,
    ) -> std::cell::Ref<'_, HashMap<(ALoc, Option<ALoc>), Vec<(ALoc, type_::CallArg)>>> {
        self.0.hint_map_arglist_cache.borrow()
    }

    pub fn hint_map_jsx_cache(
        &self,
    ) -> std::cell::Ref<
        '_,
        HashMap<
            (Reason, String, Vec<ALoc>, ALoc),
            Rc<flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>>,
        >,
    > {
        self.0.hint_map_jsx_cache.borrow()
    }

    pub fn hint_map_jsx_cache_mut(
        &self,
    ) -> std::cell::RefMut<
        '_,
        HashMap<
            (Reason, String, Vec<ALoc>, ALoc),
            Rc<flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>>,
        >,
    > {
        self.0.hint_map_jsx_cache.borrow_mut()
    }

    pub fn hint_map_arglist_cache_mut(
        &self,
    ) -> std::cell::RefMut<'_, HashMap<(ALoc, Option<ALoc>), Vec<(ALoc, type_::CallArg)>>> {
        self.0.hint_map_arglist_cache.borrow_mut()
    }

    pub fn hint_eval_cache_find_opt(&self, id: i32) -> Option<Option<Type>> {
        self.0.hint_eval_cache.borrow().get(&id).duped()
    }

    pub fn automatic_require_default(&self) -> bool {
        self.0.metadata.frozen.automatic_require_default
    }

    pub fn pid_prefix(&self) -> String {
        if self.max_workers() > 0 {
            format!("[{}] ", std::process::id())
        } else {
            String::new()
        }
    }

    // Mutators

    pub fn add_exhaustive_check(&self, loc: ALoc, x: (Vec<ALoc>, bool)) {
        self.0.ccx.exhaustive_checks.borrow_mut().insert(loc, x);
    }

    pub fn add_error(&self, error: FlowError<ALoc>) {
        self.0.ccx.errors.borrow_mut().add(error);
    }

    pub fn reset_errors(&self, errors: ErrorSet) {
        *self.0.ccx.errors.borrow_mut() = errors;
    }

    pub fn add_error_suppressions(&self, suppressions: ErrorSuppressions) {
        self.0
            .ccx
            .error_suppressions
            .borrow_mut()
            .union(suppressions);
    }

    pub fn add_severity_covers(&self, severity_covers: FlowOrdMap<FileKey, LintSeverityCover>) {
        self.0
            .ccx
            .severity_cover
            .borrow_mut()
            .extend(severity_covers);
    }

    pub fn add_property_map(
        &self,
        id: type_::properties::Id,
        pmap: type_::properties::PropertiesMap,
    ) {
        self.0
            .ccx
            .sig_cx
            .borrow_mut()
            .property_maps
            .insert(id, pmap);
    }

    pub fn add_call_prop(&self, id: i32, t: Type) {
        self.0.ccx.sig_cx.borrow_mut().call_props.insert(id, t);
    }

    pub fn add_export_map(&self, id: type_::exports::Id, tmap: type_::exports::T) {
        self.0.ccx.sig_cx.borrow_mut().export_maps.insert(id, tmap);
    }

    pub fn add_tvar(
        &self,
        id: i32,
        bounds: flow_utils_union_find::Node<type_::constraint::Constraints<'cx, Context<'cx>>>,
    ) {
        self.0
            .ccx
            .sig_cx
            .borrow()
            .graph
            .borrow_mut()
            .insert(id, bounds);
    }

    pub fn set_synthesis_produced_uncacheable_result(&self) {
        match *self.0.typing_mode.borrow() {
            TypingMode::SynthesisMode { .. } => {
                *self
                    .0
                    .ccx
                    .synthesis_produced_uncacheable_result
                    .borrow_mut() = true;
            }
            _ => {}
        }
    }

    pub fn mk_placeholder(&self, reason: Reason) -> Type {
        self.set_synthesis_produced_uncacheable_result();
        any_t::placeholder(reason)
    }

    pub fn add_post_component_tvar_forcing_state(
        &self,
        id: i32,
        state: ForcingState<'cx, Context<'cx>>,
    ) {
        self.0.ccx.delayed_forcing_tvars.borrow_mut().insert(id);
        self.0
            .ccx
            .post_component_tvar_forcing_states
            .borrow_mut()
            .push_front(state);
    }

    pub fn add_post_inference_polarity_check(
        &self,
        tparams: BTreeMap<SubstName, type_::TypeParam>,
        polarity: flow_common::polarity::Polarity,
        t: Type,
    ) {
        self.0
            .ccx
            .post_inference_polarity_checks
            .borrow_mut()
            .push((tparams, polarity, t));
    }

    pub fn add_post_inference_validation_flow(&self, t: Type, use_t: type_::UseT<Context<'cx>>) {
        self.0
            .ccx
            .post_inference_validation_flows
            .borrow_mut()
            .push_front((t, use_t));
    }

    pub fn add_post_inference_subtyping_check(&self, l: Type, use_op: UseOp, u: Type) {
        self.add_post_inference_validation_flow(
            l,
            type_::UseT::new(type_::UseTInner::UseT(use_op, u)),
        );
    }

    pub fn add_post_inference_projects_strict_boundary_import_pattern_opt_outs_validation(
        &self,
        l: ALoc,
        import_specifier: String,
        projects: Vec<FlowProjects>,
    ) {
        self.0
            .ccx
            .post_inference_projects_strict_boundary_import_pattern_opt_outs_validations
            .borrow_mut()
            .push((l, import_specifier, projects));
    }

    pub fn add_env_cache_entry(&self, for_value: bool, id: i32, t: PossiblyRefinedWriteState) {
        if for_value {
            self.0.ccx.env_value_cache.borrow_mut().insert(id, t);
        } else {
            self.0.ccx.env_type_cache.borrow_mut().insert(id, t);
        }
    }

    pub fn add_voidable_check(&self, voidable_check: VoidableCheck) {
        self.0.ccx.voidable_checks.borrow_mut().push(voidable_check);
    }

    pub fn add_monomorphized_component(&self, id: type_::properties::Id, t: Type) {
        self.0
            .ccx
            .monomorphized_components
            .borrow_mut()
            .insert(id, t);
    }

    pub fn add_reachable_dep(&self, file_key: FileKey) {
        self.0.reachable_deps.borrow_mut().insert(file_key);
    }

    pub fn add_refined_location(&self, read_loc: ALoc, refining_locs: FlowOrdSet<ALoc>) {
        self.0
            .refined_locations
            .borrow_mut()
            .insert(read_loc, refining_locs);
    }

    pub fn add_aggressively_invalidated_location(&self, loc: ALoc, info: RefinementInvalidation) {
        self.0
            .aggressively_invalidated_locations
            .borrow_mut()
            .insert(loc, info);
    }

    pub fn add_switch_to_match_eligible_location(&self, loc: ALoc) {
        self.0
            .switch_to_match_eligible_locations
            .borrow_mut()
            .insert(loc);
    }

    pub fn add_missing_local_annot_lower_bound(&self, loc: ALoc, t: Type) {
        let mut bounds_map = self.0.ccx.missing_local_annot_lower_bounds.borrow_mut();
        bounds_map
            .entry(ALocFuzzy(loc))
            .or_insert_with(Vec::new)
            .push(t);
    }

    pub fn add_array_or_object_literal_declaration_tracking(&self, tvar_id: i32, loc: ALoc) {
        self.0
            .ccx
            .array_or_object_literal_declaration_upper_bounds
            .borrow_mut()
            .insert(tvar_id, (loc, Vec::new()));
    }

    pub fn report_array_or_object_literal_declaration_reposition(
        &self,
        repositioned_tvar_id: i32,
        tvar_id: i32,
    ) {
        if self
            .0
            .ccx
            .array_or_object_literal_declaration_upper_bounds
            .borrow()
            .contains_key(&tvar_id)
        {
            self.0
                .ccx
                .array_or_object_literal_declaration_reposition_tracking
                .borrow_mut()
                .insert(repositioned_tvar_id, tvar_id);
        }
    }

    pub fn add_array_or_object_literal_declaration_upper_bound(&self, tvar_id: i32, t: Type) {
        let tvar_id = self
            .0
            .ccx
            .array_or_object_literal_declaration_reposition_tracking
            .borrow()
            .get(&tvar_id)
            .copied()
            .unwrap_or(tvar_id);
        let mut bounds_map = self
            .0
            .ccx
            .array_or_object_literal_declaration_upper_bounds
            .borrow_mut();
        if let Some((_, bounds)) = bounds_map.get_mut(&tvar_id) {
            bounds.push(t);
        }
    }

    pub fn add_inferred_component_return(&self, loc: ALoc, t: Type) {
        let aloc_fuzzy = ALocFuzzy(loc);
        let mut return_map = self.0.ccx.inferred_component_return.borrow_mut();
        match return_map.get_mut(&aloc_fuzzy) {
            None => {
                return_map.insert(aloc_fuzzy, Vec1::new(t));
            }
            Some(bounds) => {
                bounds.push(t);
            }
        }
    }

    pub fn set_evaluated(&self, evaluated: type_::eval::Map<Type>) {
        self.0.ccx.sig_cx.borrow_mut().evaluated = evaluated;
    }

    pub fn run_in_implicit_instantiation_mode<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let saved = *self.0.ccx.in_implicit_instantiation.borrow();
        *self.0.ccx.in_implicit_instantiation.borrow_mut() = true;
        let result = f();
        *self.0.ccx.in_implicit_instantiation.borrow_mut() = saved;
        result
    }

    pub fn set_exists_checks(&self, exists_checks: ALocMap<BTreeSet<Type>>) {
        *self.0.ccx.exists_checks.borrow_mut() = exists_checks;
    }

    pub fn set_signature_help_callee(&self, loc: ALoc, t: Type) {
        self.0.ccx.signature_help_callee.borrow_mut().insert(loc, t);
    }

    pub fn get_signature_help_callee(&self, loc: &ALoc) -> Option<Type> {
        self.0.ccx.signature_help_callee.borrow().get(loc).duped()
    }

    pub fn set_ctor_callee(&self, loc: ALoc, t: Type) {
        self.0.ccx.ctor_callee.borrow_mut().insert(loc, t);
    }

    pub fn get_ctor_callee(&self, loc: &ALoc) -> Option<Type> {
        self.0.ccx.ctor_callee.borrow().get(loc).duped()
    }

    pub fn record_primitive_literal_check(&self, loc: ALoc) {
        self.0.ccx.primitive_literal_checks.borrow_mut().insert(loc);
    }

    pub fn is_primitive_literal_checked(&self, loc: &ALoc) -> bool {
        self.0.ccx.primitive_literal_checks.borrow().contains(loc)
    }

    pub fn set_enclosing_context_for_call(&self, loc: ALoc, t: EnclosingContext) {
        self.0
            .ccx
            .enclosing_context_for_call
            .borrow_mut()
            .insert(loc, t);
    }

    pub fn get_enclosing_context_for_call(&self, loc: &ALoc) -> Option<EnclosingContext> {
        self.0
            .ccx
            .enclosing_context_for_call
            .borrow()
            .get(loc)
            .cloned()
    }

    pub fn set_union_opt(&self, loc: ALoc, t: Type) {
        self.0.ccx.union_opt.borrow_mut().insert(loc, t);
    }

    pub fn iter_union_opt<F>(&self, f: F)
    where
        F: FnMut(&ALoc, &Type),
    {
        let union_opt = self.0.ccx.union_opt.borrow();
        let mut f = f;
        for (loc, t) in union_opt.iter() {
            f(loc, t);
        }
    }

    pub fn add_exists_check(&self, loc: ALoc, t: Type) {
        let mut checks = self.0.ccx.exists_checks.borrow_mut();
        match checks.get_mut(&loc) {
            Some(tset) => {
                tset.insert(t);
            }
            None => {
                let mut tset = BTreeSet::default();
                tset.insert(t);
                checks.insert(loc, tset);
            }
        }
    }

    pub fn set_exists_excuses(&self, exists_excuses: ALocMap<ExistsCheck>) {
        *self.0.ccx.exists_excuses.borrow_mut() = exists_excuses;
    }
}

pub struct CacheSnapshot {
    constraint_cache_level_count: usize,
    // eval_id_cache, id_cache, eval_repos_cache, fix_cache,
    // const_fold_cache use MultiLevelMap — their state is tracked internally
    // via push_level/pop_level, not stored here.
    // subst_cache: snapshot_subst_cache = !(cx.ccx.subst_cache);
    snapshot_subst_cache: type_::SubstCacheMap<(Vec<SubstCacheErr>, Type)>,
    snapshot_spread_cache: SpreadCache,
    snapshot_evaluated: type_::eval::Map<Type>,
    snapshot_instantiation_stack: FlowVector<type_app_expansion::Entry>,
}

impl<'cx> Context<'cx> {
    pub fn take_cache_snapshot(&self) -> CacheSnapshot {
        // snapshot_subst_cache = !(cx.ccx.subst_cache);
        let snapshot_subst_cache = self.0.ccx.subst_cache.borrow().dupe();
        self.0.ccx.eval_id_cache.borrow_mut().push_level();
        self.0.ccx.id_cache.borrow_mut().push_level();
        self.0.ccx.eval_repos_cache.borrow_mut().push_level();
        self.0.ccx.fix_cache.borrow_mut().push_level();
        self.0.ccx.const_fold_cache.borrow_mut().push_level();
        let mut cc = self.0.ccx.constraint_cache.borrow_mut();
        let constraint_cache_level_count = cc.level_count();
        cc.push_level();
        drop(cc);
        CacheSnapshot {
            constraint_cache_level_count,
            snapshot_subst_cache,
            snapshot_spread_cache: self.0.ccx.spread_cache.borrow().dupe(),
            snapshot_evaluated: self.0.ccx.sig_cx.borrow().evaluated.dupe(),
            snapshot_instantiation_stack: self.0.ccx.instantiation_stack.borrow().dupe(),
        }
    }

    pub fn restore_cache_snapshot(&self, snapshot: CacheSnapshot) {
        let CacheSnapshot {
            constraint_cache_level_count,
            snapshot_subst_cache,
            snapshot_spread_cache,
            snapshot_evaluated,
            snapshot_instantiation_stack,
        } = snapshot;
        self.0
            .ccx
            .constraint_cache
            .borrow_mut()
            .truncate_to(constraint_cache_level_count);
        // cx.ccx.subst_cache := snapshot_subst_cache;
        *self.0.ccx.subst_cache.borrow_mut() = snapshot_subst_cache;
        self.0.ccx.eval_id_cache.borrow_mut().pop_level();
        self.0.ccx.id_cache.borrow_mut().pop_level();
        self.0.ccx.eval_repos_cache.borrow_mut().pop_level();
        self.0.ccx.fix_cache.borrow_mut().pop_level();
        self.0.ccx.const_fold_cache.borrow_mut().pop_level();
        *self.0.ccx.spread_cache.borrow_mut() = snapshot_spread_cache;
        self.set_evaluated(snapshot_evaluated);
        *self.0.ccx.instantiation_stack.borrow_mut() = snapshot_instantiation_stack;
    }

    pub fn run_and_rolled_back_cache<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let cache_snapshot = self.take_cache_snapshot();
        *self.0.ccx.instantiation_stack.borrow_mut() = FlowVector::new();
        let result = f();
        self.restore_cache_snapshot(cache_snapshot);
        result
    }

    pub fn run_in_synthesis_mode<F, R>(
        &self,
        reset_forcing_state: bool,
        target_loc: Option<ALoc>,
        f: F,
    ) -> (bool, R)
    where
        F: FnOnce() -> R,
    {
        let old_typing_mode = self.0.typing_mode.borrow().dupe();
        let old_synthesis_produced_uncacheable_result =
            *self.0.ccx.synthesis_produced_uncacheable_result.borrow();
        let old_delayed_forcing_tvars = self.0.ccx.delayed_forcing_tvars.borrow().dupe();
        let old_post_component_tvar_forcing_states = self
            .0
            .ccx
            .post_component_tvar_forcing_states
            .borrow()
            .dupe();
        *self
            .0
            .ccx
            .synthesis_produced_uncacheable_result
            .borrow_mut() = false;
        *self.0.typing_mode.borrow_mut() = TypingMode::SynthesisMode { target_loc };
        let cache_snapshot = self.take_cache_snapshot();
        *self.0.ccx.instantiation_stack.borrow_mut() = FlowVector::new();

        let result = f();

        let synthesis_produced_uncacheable_result =
            *self.0.ccx.synthesis_produced_uncacheable_result.borrow();

        self.restore_cache_snapshot(cache_snapshot);
        *self.0.typing_mode.borrow_mut() = old_typing_mode;
        *self
            .0
            .ccx
            .synthesis_produced_uncacheable_result
            .borrow_mut() = old_synthesis_produced_uncacheable_result;
        if reset_forcing_state {
            *self.0.ccx.delayed_forcing_tvars.borrow_mut() = old_delayed_forcing_tvars;
            *self.0.ccx.post_component_tvar_forcing_states.borrow_mut() =
                old_post_component_tvar_forcing_states;
        }

        (synthesis_produced_uncacheable_result, result)
    }

    pub fn run_in_synthesis_mode_with_errors<F, R>(
        &self,
        target_loc: Option<ALoc>,
        f: F,
    ) -> (bool, (R, ErrorSet))
    where
        F: FnOnce() -> R,
    {
        // Since this mode resets errors, we make sure to reset_forcing_state so that
        // we don't raise errors in a post-inference pass.
        self.run_in_synthesis_mode(true, target_loc, || {
            let original_errors = self.0.ccx.errors.borrow().dupe();
            self.reset_errors(ErrorSet::new());
            let t = f();
            let new_errors = self.0.ccx.errors.borrow().dupe();
            self.reset_errors(original_errors);
            (t, new_errors)
        })
    }

    pub fn run_in_signature_tvar_env<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let saved_speculation_state =
            std::mem::take(&mut self.0.ccx.speculation_state.borrow_mut().0);
        let saved_typing_mode = self.0.typing_mode.borrow().dupe();
        let saved_instantiation_stack = self.0.ccx.instantiation_stack.borrow().dupe();
        *self.0.ccx.instantiation_stack.borrow_mut() = FlowVector::new();
        *self.0.typing_mode.borrow_mut() = TypingMode::CheckingMode;

        let result = f();

        *self.0.typing_mode.borrow_mut() = saved_typing_mode;
        *self.0.ccx.speculation_state.borrow_mut() = SpeculationState(saved_speculation_state);
        *self.0.ccx.instantiation_stack.borrow_mut() = saved_instantiation_stack;

        result
    }

    pub fn run_in_hint_eval_mode<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let old_typing_mode = self.0.typing_mode.borrow().dupe();
        // We need to run type hint eval in an empty speculation state, since the hint eval is an
        // independent unit of type evaluation that's separate from an ongoing speculation.
        let saved_speculation_state =
            std::mem::take(&mut self.0.ccx.speculation_state.borrow_mut().0);
        *self.0.typing_mode.borrow_mut() = TypingMode::HintEvaluationMode;
        let cache_snapshot = self.take_cache_snapshot();
        *self.0.ccx.instantiation_stack.borrow_mut() = FlowVector::new();

        let result = f();

        self.restore_cache_snapshot(cache_snapshot);
        *self.0.ccx.speculation_state.borrow_mut() = SpeculationState(saved_speculation_state);
        *self.0.typing_mode.borrow_mut() = old_typing_mode;

        result
    }

    pub fn test_prop_hit(&self, id: i32) {
        self.0
            .ccx
            .test_prop_hits_and_misses
            .borrow_mut()
            .insert(id, TestPropHitOrMiss::Hit);
    }

    pub fn test_prop_miss(
        &self,
        id: i32,
        name: Option<Name>,
        reasons: (Reason, Reason),
        use_op: UseOp,
        suggestion: Option<FlowSmolStr>,
    ) {
        if !self
            .0
            .ccx
            .test_prop_hits_and_misses
            .borrow()
            .contains_key(&id)
        {
            self.0.ccx.test_prop_hits_and_misses.borrow_mut().insert(
                id,
                TestPropHitOrMiss::Miss {
                    name,
                    reasons,
                    use_op,
                    suggestion,
                },
            );
        }
    }

    pub fn test_prop_get_never_hit(
        &self,
    ) -> Vec<(Option<Name>, (Reason, Reason), UseOp, Option<FlowSmolStr>)> {
        self.0
            .ccx
            .test_prop_hits_and_misses
            .borrow()
            .iter()
            .filter_map(|(_, hit_or_miss)| match hit_or_miss {
                TestPropHitOrMiss::Hit => None,
                TestPropHitOrMiss::Miss {
                    name,
                    reasons,
                    use_op,
                    suggestion,
                } => Some((
                    name.dupe(),
                    reasons.dupe(),
                    use_op.dupe(),
                    suggestion.dupe(),
                )),
            })
            .collect()
    }

    pub fn add_condition(&self, e: flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>) {
        self.0.ccx.conditions.borrow_mut().push(e);
    }

    pub fn get_all_conditions(
        &self,
    ) -> FlowVector<flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>> {
        self.0.ccx.conditions.borrow().dupe()
    }

    pub fn add_strict_comparison(
        &self,
        comp: (
            ALoc,
            (
                flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
                flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
            ),
        ),
    ) {
        self.0.ccx.strict_comparisons.borrow_mut().push(comp);
    }

    pub fn get_all_strict_comparisons(
        &self,
    ) -> FlowVector<(
        ALoc,
        (
            flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
            flow_parser::ast::expression::Expression<ALoc, (ALoc, Type)>,
        ),
    )> {
        self.0.ccx.strict_comparisons.borrow().dupe()
    }

    pub fn mark_optional_chain(&self, loc: ALoc, lhs_reason: Reason, useful: bool) {
        let mut chains = self.0.ccx.optional_chains_useful.borrow_mut();
        chains
            .entry(loc)
            .and_modify(|(_r, u)| {
                *u = *u || useful;
            })
            .or_insert((lhs_reason, useful));
    }

    pub fn unnecessary_optional_chains(&self) -> Vec<(ALoc, Reason)> {
        let chains = self.0.ccx.optional_chains_useful.borrow();
        let mut result = Vec::new();
        for (loc, (r, useful)) in chains.iter() {
            if !useful {
                result.push((loc.dupe(), r.dupe()));
            }
        }
        result
    }

    pub fn mark_maybe_unused_promise(&self, loc: ALoc, t: Type, async_: bool) {
        self.0
            .ccx
            .maybe_unused_promises
            .borrow_mut()
            .push((loc, t, async_));
    }

    pub fn maybe_unused_promises(&self) -> FlowVector<(ALoc, Type, bool)> {
        self.0.ccx.maybe_unused_promises.borrow().dupe()
    }

    pub fn add_hint_eval_cache_entry(&self, id: i32, result: Option<Type>) {
        self.0.hint_eval_cache.borrow_mut().insert(id, result);
    }

    // utils

    pub fn iter_props<F>(&self, id: type_::properties::Id, f: F)
    where
        F: Fn(&flow_common::reason::Name, &type_::Property),
    {
        self.find_props(id).iter().for_each(|(k, v)| f(k, v))
    }

    pub fn try_iter_props<F, E>(&self, id: type_::properties::Id, f: F) -> Result<(), E>
    where
        F: Fn(&flow_common::reason::Name, &type_::Property) -> Result<(), E>,
    {
        for (k, v) in self.find_props(id).iter() {
            f(k, v)?;
        }
        Ok(())
    }

    pub fn fold_props<F, A>(&self, id: type_::properties::Id, f: F, init: A) -> A
    where
        F: Fn(&flow_common::reason::Name, &type_::Property, A) -> A,
    {
        self.find_props(id)
            .iter()
            .fold(init, |acc, (k, v)| f(k, v, acc))
    }

    pub fn has_prop(&self, id: type_::properties::Id, x: &flow_common::reason::Name) -> bool {
        self.find_props(id).contains_key(x)
    }

    pub fn get_prop(
        &self,
        id: type_::properties::Id,
        x: &flow_common::reason::Name,
    ) -> Option<type_::Property> {
        self.find_props(id).get(x).duped()
    }

    // constructors

    pub fn make_aloc_id(&self, aloc: &ALoc) -> ALocId {
        let table = self.0.aloc_table.dupe();
        ALocId::of_aloc(aloc, move || LazyCell::force(&*table).dupe())
    }

    pub fn make_generic_id(&self, name: SubstName, loc: &ALoc) -> GenericId {
        GenericId::make_bound_id(self.make_aloc_id(loc), name)
    }

    pub fn generate_property_map(
        &self,
        pmap: type_::properties::PropertiesMap,
    ) -> type_::properties::Id {
        let id = type_::properties::Id::generate_id();
        self.add_property_map(id.dupe(), pmap);
        id
    }

    pub fn make_source_property_map(
        &self,
        pmap: type_::properties::PropertiesMap,
        type_sig: bool,
        aloc: &ALoc,
    ) -> type_::properties::Id {
        // To prevent cases where we might compare a concrete and an abstract
        // aloc (like in a cycle) we abstractify all incoming alocs before adding
        // them to the map. The only exception is for library files, which have only
        // concrete definitions and by definition cannot appear in cycles. *)
        let aloc_id = self.make_aloc_id(aloc);
        let id = type_::properties::Id::of_aloc_id(type_sig, aloc_id);
        self.add_property_map(id.dupe(), pmap);
        id
    }

    pub fn make_call_prop(&self, t: Type) -> i32 {
        let id = flow_common::reason::mk_id() as i32;
        self.add_call_prop(id, t);
        id
    }

    pub fn make_export_map(&self, tmap: type_::exports::T) -> type_::exports::Id {
        let id = type_::exports::Id::mk_id();
        self.add_export_map(id.dupe(), tmap);
        id
    }

    pub fn make_source_poly_id(&self, type_sig: bool, aloc: &ALoc) -> type_::poly::Id {
        let aloc_id = self.make_aloc_id(aloc);
        type_::poly::Id::of_aloc_id(type_sig, aloc_id)
    }

    pub fn find_graph(&self, id: i32) -> type_::constraint::Constraints<'cx, Context<'cx>> {
        let graph = self.graph();
        graph.borrow_mut().find_graph(id).unwrap().clone()
    }

    pub fn find_constraints(
        &self,
        id: i32,
    ) -> (i32, type_::constraint::Constraints<'cx, Context<'cx>>) {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let file = self.file();
        let (id, c) = graph.find_constraints(id).unwrap_or_else(|e| {
            panic!("find_constraints: TvarNotFound({}) in file {:?}", e.0, file,)
        });
        (id, c.clone())
    }

    /// Mutate the constraints of a tvar's root in-place, avoiding the
    /// clone-modify-writeback overhead of find_constraints + set_root_constraints.
    /// The closure receives (root_id, &mut Constraints).
    pub fn modify_constraints<R>(
        &self,
        id: i32,
        f: impl FnOnce(i32, &mut type_::constraint::Constraints<'cx, Context<'cx>>) -> R,
    ) -> R {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let (root_id, c) = graph.find_constraints(id).unwrap_or_else(|e| {
            panic!(
                "modify_constraints: TvarNotFound({}) in file {:?}",
                e.0,
                self.file(),
            )
        });
        f(root_id, c)
    }

    /// Read-only inspection of constraints without cloning.
    /// The closure receives (root_id, &Constraints) and must not call
    /// other graph operations (the graph borrow is held).
    pub fn inspect_constraints<R>(
        &self,
        id: i32,
        f: impl FnOnce(i32, &type_::constraint::Constraints<'cx, Context<'cx>>) -> R,
    ) -> R {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let (root_id, c) = graph.find_constraints(id).unwrap_or_else(|e| {
            panic!(
                "inspect_constraints: TvarNotFound({}) in file {:?}",
                e.0,
                self.file(),
            )
        });
        f(root_id, c)
    }

    pub fn find_root(
        &self,
        id: i32,
    ) -> (
        i32,
        flow_utils_union_find::Root<type_::constraint::Constraints<'cx, Context<'cx>>>,
    ) {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let file = self.file();
        let (id, root) = graph
            .find_root(id)
            .unwrap_or_else(|e| panic!("find_root: TvarNotFound({}) in file {:?}", e.0, file,));
        (id, root.clone())
    }

    pub fn set_root_constraints(
        &self,
        id: i32,
        constraints: type_::constraint::Constraints<'cx, Context<'cx>>,
    ) {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let (_root_id, root) = graph.find_root(id).unwrap();
        root.constraints = constraints;
    }

    pub fn set_root_rank(&self, id: i32, rank: i32) {
        let graph = self.graph();
        let mut graph = graph.borrow_mut();
        let (_root_id, root) = graph.find_root(id).unwrap();
        root.rank = rank;
    }

    pub fn find_root_id(&self, id: i32) -> i32 {
        self.graph()
            .borrow_mut()
            .find_root_id(id)
            .unwrap_or_else(|e| {
                panic!(
                    "find_root_id: TvarNotFound({}) in file {:?}",
                    e.0,
                    self.file(),
                )
            })
    }

    pub fn on_cyclic_tvar_error(&self, reason: Reason) -> Type {
        let msg = ErrorMessage::ETrivialRecursiveDefinition(Box::new((
            reason.loc().dupe(),
            reason.dupe(),
        )));
        let error = error_of_msg(self.file().dupe(), msg);
        if self.is_verbose() {
            eprintln!("\nCyclic type: {:?}", reason);
        }
        self.add_error(error);
        any_t::error(reason)
    }

    pub fn force_fully_resolved_tvar(&self, s: &ForcingState<'cx, Context<'cx>>) -> Type {
        s.force(self, |reason| self.on_cyclic_tvar_error(reason.dupe()))
    }

    pub fn find_resolved(&self, t_in: &Type) -> Option<Type> {
        fn find_resolved_inner<'a>(
            cx: &Context<'a>,
            seen: &mut BTreeSet<u32>,
            t_in: &Type,
        ) -> Option<Type> {
            match &**t_in {
                TypeInner::OpenT(tvar) => {
                    let id = tvar.id();
                    if seen.contains(&id) {
                        Some(t_in.dupe())
                    } else {
                        use flow_typing_type::type_::constraint::Constraints;
                        match cx.find_graph(id as i32) {
                            Constraints::Resolved(t) => {
                                seen.insert(id);
                                find_resolved_inner(cx, seen, &t)
                            }
                            Constraints::FullyResolved(s) => {
                                seen.insert(id);
                                let forced = cx.force_fully_resolved_tvar(&s);
                                find_resolved_inner(cx, seen, &forced)
                            }
                            Constraints::Unresolved(_) => None,
                        }
                    }
                }
                TypeInner::AnnotT(_, t, _) => find_resolved_inner(cx, seen, t),
                _ => Some(t_in.dupe()),
            }
        }

        let mut seen = BTreeSet::new();
        find_resolved_inner(self, &mut seen, t_in)
    }

    pub fn constraint_cache(&self) -> std::cell::Ref<'_, type_::FlowSet<Context<'cx>>> {
        self.0.ccx.constraint_cache.borrow()
    }

    pub fn constraint_cache_mut(&self) -> std::cell::RefMut<'_, type_::FlowSet<Context<'cx>>> {
        self.0.ccx.constraint_cache.borrow_mut()
    }

    pub fn subst_cache(
        &self,
    ) -> std::cell::Ref<'_, type_::SubstCacheMap<(Vec<SubstCacheErr>, Type)>> {
        self.0.ccx.subst_cache.borrow()
    }

    pub fn subst_cache_mut(
        &self,
    ) -> std::cell::RefMut<'_, type_::SubstCacheMap<(Vec<SubstCacheErr>, Type)>> {
        self.0.ccx.subst_cache.borrow_mut()
    }

    pub fn eval_id_cache(&self) -> std::cell::Ref<'_, EvalIdCacheMap> {
        self.0.ccx.eval_id_cache.borrow()
    }

    pub fn eval_id_cache_mut(&self) -> std::cell::RefMut<'_, EvalIdCacheMap> {
        self.0.ccx.eval_id_cache.borrow_mut()
    }

    pub fn id_cache(&self) -> std::cell::Ref<'_, IdCacheMap> {
        self.0.ccx.id_cache.borrow()
    }

    pub fn id_cache_mut(&self) -> std::cell::RefMut<'_, IdCacheMap> {
        self.0.ccx.id_cache.borrow_mut()
    }

    pub fn eval_repos_cache(&self) -> std::cell::Ref<'_, EvalReposCacheMap> {
        self.0.ccx.eval_repos_cache.borrow()
    }

    pub fn eval_repos_cache_mut(&self) -> std::cell::RefMut<'_, EvalReposCacheMap> {
        self.0.ccx.eval_repos_cache.borrow_mut()
    }

    pub fn fix_cache(&self) -> std::cell::Ref<'_, FixCacheMap> {
        self.0.ccx.fix_cache.borrow()
    }

    pub fn fix_cache_mut(&self) -> std::cell::RefMut<'_, FixCacheMap> {
        self.0.ccx.fix_cache.borrow_mut()
    }

    pub fn spread_cache(&self) -> std::cell::Ref<'_, SpreadCache> {
        self.0.ccx.spread_cache.borrow()
    }

    pub fn spread_cache_cell(&self) -> &std::cell::RefCell<SpreadCache> {
        &self.0.ccx.spread_cache
    }

    pub fn instantiation_stack(&self) -> std::cell::Ref<'_, FlowVector<type_app_expansion::Entry>> {
        self.0.ccx.instantiation_stack.borrow()
    }

    pub fn instantiation_stack_mut(
        &self,
    ) -> std::cell::RefMut<'_, FlowVector<type_app_expansion::Entry>> {
        self.0.ccx.instantiation_stack.borrow_mut()
    }

    pub fn const_fold_cache(&self) -> std::cell::Ref<'_, ConstFoldMap> {
        self.0.ccx.const_fold_cache.borrow()
    }

    pub fn const_fold_cache_mut(&self) -> std::cell::RefMut<'_, ConstFoldMap> {
        self.0.ccx.const_fold_cache.borrow_mut()
    }

    pub fn exhaustive_check(&self, loc: &ALoc) -> Option<(Vec<ALoc>, bool)> {
        self.0.ccx.exhaustive_checks.borrow().get(loc).cloned()
    }

    pub fn speculation_state(&self) -> std::cell::Ref<'_, SpeculationState> {
        self.0.ccx.speculation_state.borrow()
    }

    pub fn speculation_state_mut(&self) -> std::cell::RefMut<'_, SpeculationState> {
        self.0.ccx.speculation_state.borrow_mut()
    }

    pub fn speculation_id(&self) -> Option<type_::SpecState> {
        let state = self.0.ccx.speculation_state.borrow();
        state.0.last().map(|frame| type_::SpecState {
            speculation_id: frame.speculation_id,
            case_id: frame.case.case_id,
        })
    }

    pub fn add_avar(&self, id: i32, constraint_: AConstraint<'cx>) {
        self.0.ccx.annot_graph.borrow_mut().insert(id, constraint_);
    }

    pub fn find_avar(&self, id: i32) -> AConstraint<'cx> {
        self.0
            .ccx
            .annot_graph
            .borrow()
            .get(&id)
            .expect("find_avar: id not found in annot_graph")
            .dupe()
    }

    pub fn find_avar_opt(&self, id: i32) -> Option<AConstraint<'cx>> {
        self.0.ccx.annot_graph.borrow().get(&id).duped()
    }

    pub fn find_monomorphized_component(&self, id: type_::properties::Id) -> Option<Type> {
        self.0
            .ccx
            .monomorphized_components
            .borrow()
            .get(&id)
            .duped()
    }

    pub fn remove_avar(&self, id: i32) {
        self.0.ccx.annot_graph.borrow_mut().remove(&id);
    }

    pub fn iter_annot_dependent_set<F>(&self, mut f: F, set: &FlowOrdSet<i32>)
    where
        F: FnMut(i32, &type_::aconstraint::Op<'cx>),
    {
        for &i in set {
            let constraints = self.find_avar(i);
            let op = constraints.to_annot_op_exn();
            f(i, op);
        }
    }

    pub fn new_specialized_callee(&self) -> type_::SpecializedCallee {
        type_::SpecializedCallee {
            init_speculation_state: self.speculation_id(),
            finalized: std::rc::Rc::new(std::cell::RefCell::new(std::collections::VecDeque::new())),
            speculative_candidates: std::rc::Rc::new(std::cell::RefCell::new(
                std::collections::VecDeque::new(),
            )),
            sig_help: std::rc::Rc::new(std::cell::RefCell::new(std::collections::VecDeque::new())),
        }
    }
}
