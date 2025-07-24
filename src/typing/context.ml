(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type.TypeContext
module ALocMap = Loc_collections.ALocMap
module ALocSet = Loc_collections.ALocSet
module ALocFuzzyMap = Loc_collections.ALocFuzzyMap

exception Props_not_found of Type.Properties.id

exception Call_not_found of int

exception Exports_not_found of Type.Exports.id

exception Require_not_found of Flow_import_specifier.t

exception Module_not_found of string

type metadata = {
  (* local *)
  checked: bool;
  include_suppressions: bool;
  jsx: Options.jsx_mode;
  munge_underscores: bool;
  strict: bool;
  strict_local: bool;
  available_platforms: Platform_set.t option;
  has_explicit_supports_platform: bool;
  verbose: Verbose.t option;
  slow_to_check_logging: Slow_to_check_logging.t;
  (* global *)
  automatic_require_default: bool;
  babel_loose_array_spread: bool;
  casting_syntax: Options.CastingSyntax.t;
  component_syntax: bool;
  hook_compatibility_excludes: Str.regexp list;
  hook_compatibility_includes: Str.regexp list;
  hook_compatibility: bool;
  react_rules: Options.react_rules list;
  dev_only_refinement_info_as_errors: bool;
  enable_const_params: bool;
  enable_enums: bool;
  enable_jest_integration: bool;
  enable_pattern_matching: bool;
  pattern_matching_includes: string list;
  constant_condition: bool;
  constant_condition_boolean_literal_includes: string list;
  constant_condition_null_void_includes: string list;
  constant_condition_function_includes: string list;
  invalid_comparison_general_includes: string list;
  invalid_comparison_null_check_includes: string list;
  enable_relay_integration: bool;
  exact_by_default: bool;
  facebook_fbs: string option;
  facebook_fbt: string option;
  facebook_module_interop: bool;
  file_options: Files.options;
  ignore_non_literal_requires: bool;
  max_literal_length: int;
  max_workers: int;
  missing_module_generators: (Str.regexp * string) list;
  no_unchecked_indexed_access: bool;
  opaque_type_new_bound_syntax: bool;
  projects_options: Flow_projects.options;
  react_custom_jsx_typing: bool;
  react_ref_as_prop: Options.ReactRefAsProp.t;
  react_runtime: Options.react_runtime;
  recursion_limit: int;
  relay_integration_esmodules: bool;
  relay_integration_excludes: Str.regexp list;
  relay_integration_module_prefix: string option;
  relay_integration_module_prefix_includes: Str.regexp list;
  root: File_path.t;
  strict_es6_import_export: bool;
  strip_root: bool;
  suppress_types: SSet.t;
  ts_syntax: bool;
  assert_operator: Options.AssertOperator.t;
  type_expansion_recursion_limit: int;
  use_mixed_in_catch_variables: bool;
  ban_spread_key_props: bool;
}

type test_prop_hit_or_miss =
  | Hit
  | Miss of Reason.name option * (Reason.t * Reason.t) * Type.use_op * string option

type voidable_check = {
  public_property_map: Type.Properties.id;
  private_property_map: Type.Properties.id;
  errors: ALoc.t Property_assignment.errors;
}

type subst_cache_err =
  | ETooFewTypeArgs of ALoc.t * int
  | ETooManyTypeArgs of ALoc.t * int

module TypeAppExpansion = struct
  (* Array types function like type applications but are not implemented as such. Unless
     we decide to unify their implementation with regular typeapps, they need special
     handling here *)
  type root =
    | Type of Type.t
    | Array of Reason.t
    | ROArray of Reason.t
    | Tuple of Reason.t * int

  (* arity *)

  module RootSet : Flow_set.S with type elt = root = Flow_set.Make (struct
    type elt = root

    type t = elt

    let compare = Stdlib.compare
  end)

  type entry = Type.t * RootSet.t list * [ `Lower | `Upper ]
end

type sig_t = Type.TypeContext.t

type possibly_refined_write_state =
  | PossiblyRefinedWriteState of {
      t: Type.t;
      errors: Env_api.cacheable_env_error list;
      actually_refined_refining_locs: ALocSet.t option;
    }

type builtins_group =
  | BuiltinGroup of {
      builtin_locs: Loc.t Type_sig_collections.Locs.t;
      builtins: Type_sig_collections.Locs.index Packed_type_sig.Builtins.t;
    }

type master_context =
  | EmptyMasterContext
  | NonEmptyMasterContext of {
      builtin_leader_file_key: File_key.t;
      unscoped_builtins: builtins_group;
      scoped_builtins: (Flow_projects.t * builtins_group) list;
    }

type component_t = {
  mutable sig_cx: sig_t;
  (* mapping from keyed alocs to concrete locations *)
  mutable aloc_tables: ALoc.table Lazy.t Utils_js.FilenameMap.t;
  mutable synthesis_produced_uncacheable_result: bool;
  mutable errors: Flow_error.ErrorSet.t;
  mutable error_suppressions: Error_suppressions.t;
  mutable severity_cover: ExactCover.lint_severity_cover Utils_js.FilenameMap.t;
  (* map from exists proposition locations to the types of values running through them *)
  mutable exists_checks: Type.TypeSet.t ALocMap.t;
  (* map from exists proposition locations to the types of excuses for them *)
  (* If a variable appears in something like `x || ''`, the existence check
   * is excused and not considered sketchy. (The program behaves identically to how it would
   * if the null check was made explicit (`x == null ? '' : x`), and this is a fairly
   * common pattern. Excusing it eliminates a lot of noise from the lint rule. *)
  (* The above example assumes that x is a string. If it were a different type
   * it wouldn't be excused. *)
  mutable exists_excuses: ExistsCheck.t ALocMap.t;
  (* For the definite instance property assignment analysis, we should only
   * emit errors for a given property if VoidT flows to the type of that
   * property. Ideally, we would create a VoidT ~> property type flow when we
   * perform the analysis. The problem is that doing that causes the type
   * inference behavior to depend on lint settings which can lead to some weird
   * behavior, such as extra errors even when the lint is off. The solution is
   * to collect all of potential errors that we would have created a flow for
   * in the context and deal with them post-merge. At this point, the tvars of
   * nearly all properties will have a concrete type that we can safely pattern
   * match on without affecting other constraints. For the unresolved tvars, we
   * conservatively emit errors.
   *)
  mutable voidable_checks: voidable_check list;
  mutable test_prop_hits_and_misses: test_prop_hit_or_miss IMap.t;
  mutable optional_chains_useful: (Reason.t * bool) ALocMap.t;
  mutable conditions: (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t list;
  mutable strict_comparisons:
    ( (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t
    )
    list;
  mutable invariants_useful: (Reason.t * bool) ALocMap.t;
  mutable maybe_unused_promises: (ALoc.t * Type.t * bool) list;
  constraint_cache: Type.FlowSet.t ref;
  subst_cache: (subst_cache_err list * Type.t) Type.SubstCacheMap.t ref;
  eval_id_cache: Type.t Type.EvalIdCacheMap.t ref * Type.Eval.id Type.IdCacheMap.t ref;
  eval_repos_cache: Type.t Type.EvalReposCacheMap.t ref;
  fix_cache: Type.t Type.FixCacheMap.t ref;
  spread_cache: Spread_cache.t ref;
  const_fold_cache: int Type.ConstFoldMap.t IMap.t ref;
  speculation_state: Speculation_state.t;
  instantiation_stack: TypeAppExpansion.entry list ref;
  (* Post-inference checks *)
  mutable literal_subtypes: (ALoc.t * Env_api.literal_check) list;
  mutable matching_props: (string * Type.t * Type.t) list;
  mutable delayed_forcing_tvars: ISet.t;
  mutable post_component_tvar_forcing_states: Type.Constraint.ForcingState.t list;
  mutable post_inference_polarity_checks:
    (Type.typeparam Subst_name.Map.t * Polarity.t * Type.t) list;
  mutable post_inference_validation_flows: (Type.t * Type.use_t) list;
  mutable post_inference_projects_strict_boundary_import_pattern_opt_outs_validations:
    (ALoc.t * string * Flow_projects.t list) list;
  mutable env_value_cache: possibly_refined_write_state IMap.t;
  mutable env_type_cache: possibly_refined_write_state IMap.t;
  (* map from annot tvar ids to nodes used during annotation processing *)
  mutable annot_graph: Type.AConstraint.t IMap.t;
  (* Used to power an autofix that takes the lower bounds of types where we emit missing-local-annot
   * and turn them into annotations. This has to exist outside of the tvar graph because we will
   * eventually not use unresolved tvars to represent unannotated parameters. We use an ALocFuzzyMap
   * because we may compare keyed and concrete locations *)
  mutable missing_local_annot_lower_bounds: Type.t Nel.t ALocFuzzyMap.t;
  (* Used to power an autofix that takes the upper bounds of array & object literal declarations,
   * and turn them into annotations. *)
  mutable array_or_object_literal_declaration_upper_bounds: (ALoc.t * Type.t list) IMap.t;
  mutable array_or_object_literal_declaration_reposition_tracking: int IMap.t;
  (* Used to power a code action to automatically insert appropriate render type.
   * It is key-ed by the body_loc of components. *)
  mutable inferred_component_return: Type.t Nel.t ALocFuzzyMap.t;
  mutable exhaustive_checks: (ALoc.t list * bool) ALocMap.t;
  mutable in_implicit_instantiation: bool;
  (* React$Element does not store the monomorphized version of a component to support
   * cloning polymorphic elements. We need to know the monomorphized version of a component
   * to determine the render type of an element of a polymorphic component, so we keep track
   * of the monomorphized version here by mapping the Element's object id to the monomorphized
   * component *)
  mutable monomorphized_components: Type.t Type.Properties.Map.t;
  (* Signature help *)
  mutable signature_help_callee: Type.t ALocMap.t;
  (* Hover type *)
  mutable ctor_callee: Type.t ALocMap.t;
  (* Union optimization checks *)
  mutable union_opt: Type.t ALocMap.t;
  (* Natural inference (records checks on primitive literal types during
   * implicit instantiation) *)
  mutable primitive_literal_checks: ALocSet.t;
  mutable enclosing_context_for_call: Enclosing_context.enclosing_context ALocMap.t;
}
[@@warning "-69"]

type typing_mode =
  | CheckingMode
  | SynthesisMode of { target_loc: ALoc.t option }
  | HintEvaluationMode

type resolved_require =
  | TypedModule of (unit -> (Type.moduletype, Type.t) result)
  | UncheckedModule of ALoc.t
  | MissingModule

type t = {
  ccx: component_t;
  file: File_key.t;
  aloc_table: ALoc.table Lazy.t;
  metadata: metadata;
  resolve_require: resolve_require;
  builtins: Builtins.t lazy_t;
  hint_map_arglist_cache: (ALoc.t * ALoc.t option, (ALoc.t * Type.call_arg) list) Hashtbl.t;
  hint_map_jsx_cache: (Reason.t * string * ALoc.t list * ALoc.t, Type.t Lazy.t) Hashtbl.t;
  mutable hint_eval_cache: Type.t option IMap.t;
  mutable environment: Loc_env.t;
  mutable typing_mode: typing_mode;
  (* A subset of all transitive dependencies of the current file as determined by import/require.
   * This set will only be populated with type sig files that are actually forced. *)
  mutable reachable_deps: Utils_js.FilenameSet.t;
  mutable refined_locations: ALocSet.t ALocMap.t;
  mutable aggressively_invalidated_locations: Refinement_invalidation.t ALocMap.t;
  node_cache: Node_cache.t;
}

and resolve_require = Flow_import_specifier.t -> resolved_require

let metadata_of_options options =
  {
    (* local *)
    checked = Options.all options;
    include_suppressions = Options.include_suppressions options;
    jsx = Options.Jsx_react;
    munge_underscores = Options.should_munge_underscores options;
    strict = false;
    strict_local = false;
    available_platforms = None;
    has_explicit_supports_platform = false;
    verbose = Options.verbose options;
    slow_to_check_logging = Options.slow_to_check_logging options;
    (* global *)
    automatic_require_default = Options.automatic_require_default options;
    babel_loose_array_spread = Options.babel_loose_array_spread options;
    casting_syntax = Options.casting_syntax options;
    component_syntax = Options.component_syntax options;
    hook_compatibility_excludes = Options.hook_compatibility_excludes options;
    hook_compatibility_includes = Options.hook_compatibility_includes options;
    hook_compatibility = Options.hook_compatibility options;
    react_rules = Options.react_rules options;
    dev_only_refinement_info_as_errors = Options.dev_only_refinement_info_as_errors options;
    enable_const_params = Options.enable_const_params options;
    enable_enums = Options.enums options;
    enable_jest_integration = Options.enable_jest_integration options;
    enable_pattern_matching = Options.enable_pattern_matching options;
    pattern_matching_includes = Options.pattern_matching_includes options;
    constant_condition = Options.constant_condition options;
    constant_condition_boolean_literal_includes =
      Options.constant_condition_boolean_literal_includes options;
    constant_condition_null_void_includes = Options.constant_condition_null_void_includes options;
    constant_condition_function_includes = Options.constant_condition_function_includes options;
    invalid_comparison_general_includes = Options.invalid_comparison_general_includes options;
    invalid_comparison_null_check_includes = Options.invalid_comparison_null_check_includes options;
    enable_relay_integration = Options.enable_relay_integration options;
    exact_by_default = Options.exact_by_default options;
    facebook_fbs = Options.facebook_fbs options;
    facebook_fbt = Options.facebook_fbt options;
    facebook_module_interop = Options.facebook_module_interop options;
    file_options = Options.file_options options;
    ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
    max_literal_length = Options.max_literal_length options;
    max_workers = Options.max_workers options;
    missing_module_generators = Options.missing_module_generators options;
    no_unchecked_indexed_access = Options.no_unchecked_indexed_access options;
    opaque_type_new_bound_syntax = Options.opaque_type_new_bound_syntax options;
    projects_options = Options.projects_options options;
    react_custom_jsx_typing = Options.react_custom_jsx_typing options;
    react_ref_as_prop = Options.react_ref_as_prop options;
    react_runtime = Options.react_runtime options;
    recursion_limit = Options.recursion_limit options;
    relay_integration_esmodules = Options.relay_integration_esmodules options;
    relay_integration_excludes = Options.relay_integration_excludes options;
    relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    relay_integration_module_prefix_includes =
      Options.relay_integration_module_prefix_includes options;
    root = Options.root options;
    strict_es6_import_export = Options.strict_es6_import_export options;
    strip_root = Options.should_strip_root options;
    suppress_types = Options.suppress_types options;
    ts_syntax = Options.ts_syntax options;
    assert_operator = Options.assert_operator options;
    type_expansion_recursion_limit = Options.type_expansion_recursion_limit options;
    use_mixed_in_catch_variables = Options.use_mixed_in_catch_variables options;
    ban_spread_key_props = Options.ban_spread_key_props options;
  }

let docblock_overrides docblock_info file_key metadata =
  let metadata =
    let jsx =
      match Docblock.jsx docblock_info with
      | Some (expr, jsx_expr) ->
        let jsx_expr = Ast_loc_utils.loc_to_aloc_mapper#expression jsx_expr in
        Options.Jsx_pragma (expr, jsx_expr)
      | None -> Options.Jsx_react
    in
    { metadata with jsx }
  in
  let metadata =
    let react_runtime =
      match Docblock.jsx_runtime docblock_info with
      | Some Docblock.JsxRuntimePragmaClassic -> Options.ReactRuntimeClassic
      | Some Docblock.JsxRuntimePragmaAutomatic -> Options.ReactRuntimeAutomatic
      | None -> metadata.react_runtime
    in
    { metadata with react_runtime }
  in
  let metadata =
    match Docblock.flow docblock_info with
    | None -> metadata
    | Some Docblock.OptIn -> { metadata with checked = true }
    | Some Docblock.OptInStrict -> { metadata with checked = true; strict = true }
    | Some Docblock.OptInStrictLocal -> { metadata with checked = true; strict_local = true }
    (* --all (which sets metadata.checked = true) overrides @noflow, so there are
       currently no scenarios where we'd change checked = true to false. in the
       future, there may be a case where checked defaults to true (but is not
       forced to be true ala --all), but for now we do *not* want to force
       checked = false here. *)
    | Some Docblock.OptOut -> metadata
  in
  let metadata =
    if Docblock.preventMunge docblock_info then
      { metadata with munge_underscores = false }
    else
      metadata
  in
  let metadata =
    let file_options = metadata.file_options in
    let projects_options = metadata.projects_options in
    let explicit_available_platforms = Docblock.supportsPlatform docblock_info in
    let available_platforms =
      Platform_set.available_platforms
        ~file_options
        ~projects_options
        ~filename:(File_key.to_string file_key)
        ~explicit_available_platforms
    in
    {
      metadata with
      available_platforms;
      has_explicit_supports_platform = Option.is_some explicit_available_platforms;
    }
  in
  metadata

let empty_sig_cx =
  {
    graph = IMap.empty;
    property_maps = Type.Properties.Map.empty;
    call_props = IMap.empty;
    export_maps = Type.Exports.Map.empty;
    evaluated = Type.Eval.Map.empty;
  }

let make_ccx () =
  {
    sig_cx = empty_sig_cx;
    aloc_tables = Utils_js.FilenameMap.empty;
    synthesis_produced_uncacheable_result = false;
    matching_props = [];
    literal_subtypes = [];
    delayed_forcing_tvars = ISet.empty;
    post_component_tvar_forcing_states = [];
    post_inference_polarity_checks = [];
    post_inference_validation_flows = [];
    post_inference_projects_strict_boundary_import_pattern_opt_outs_validations = [];
    env_value_cache = IMap.empty;
    env_type_cache = IMap.empty;
    missing_local_annot_lower_bounds = ALocFuzzyMap.empty;
    array_or_object_literal_declaration_upper_bounds = IMap.empty;
    array_or_object_literal_declaration_reposition_tracking = IMap.empty;
    inferred_component_return = ALocFuzzyMap.empty;
    errors = Flow_error.ErrorSet.empty;
    error_suppressions = Error_suppressions.empty;
    severity_cover = Utils_js.FilenameMap.empty;
    exists_checks = ALocMap.empty;
    exists_excuses = ALocMap.empty;
    voidable_checks = [];
    test_prop_hits_and_misses = IMap.empty;
    optional_chains_useful = ALocMap.empty;
    conditions = [];
    strict_comparisons = [];
    invariants_useful = ALocMap.empty;
    maybe_unused_promises = [];
    constraint_cache = ref Type.FlowSet.empty;
    subst_cache = ref Type.SubstCacheMap.empty;
    eval_id_cache = (ref Type.EvalIdCacheMap.empty, ref Type.IdCacheMap.empty);
    eval_repos_cache = ref Type.EvalReposCacheMap.empty;
    fix_cache = ref Type.FixCacheMap.empty;
    spread_cache = ref IMap.empty;
    instantiation_stack = ref [];
    const_fold_cache = ref IMap.empty;
    speculation_state = ref [];
    annot_graph = IMap.empty;
    exhaustive_checks = ALocMap.empty;
    in_implicit_instantiation = false;
    monomorphized_components = Type.Properties.Map.empty;
    signature_help_callee = ALocMap.empty;
    ctor_callee = ALocMap.empty;
    union_opt = ALocMap.empty;
    primitive_literal_checks = ALocSet.empty;
    enclosing_context_for_call = ALocMap.empty;
  }

let make ccx metadata file aloc_table resolve_require mk_builtins =
  ccx.aloc_tables <- Utils_js.FilenameMap.add file aloc_table ccx.aloc_tables;
  let rec cx_lazy =
    lazy
      {
        ccx;
        builtins = lazy (mk_builtins (Lazy.force cx_lazy));
        file;
        aloc_table;
        metadata;
        resolve_require;
        hint_map_arglist_cache = Hashtbl.create 0;
        hint_map_jsx_cache = Hashtbl.create 0;
        hint_eval_cache = IMap.empty;
        environment = Loc_env.empty Name_def.Global;
        typing_mode = CheckingMode;
        reachable_deps = Utils_js.FilenameSet.empty;
        node_cache = Node_cache.mk_empty ();
        refined_locations = ALocMap.empty;
        aggressively_invalidated_locations = ALocMap.empty;
      }
  in
  Lazy.force cx_lazy

let sig_cx cx = cx.ccx.sig_cx

(* modules *)

let in_declare_module cx = cx.environment.Loc_env.scope_kind = Name_def.DeclareModule

let in_declare_namespace cx = cx.environment.Loc_env.scope_kind = Name_def.DeclareNamespace

(* accessors *)

let metadata cx = cx.metadata

let max_literal_length cx = cx.metadata.max_literal_length

let babel_loose_array_spread cx = cx.metadata.babel_loose_array_spread

let file cx = cx.file

let is_lib_file cx = File_key.is_lib_file cx.file

let in_dirlist cx dirs =
  match dirs with
  | [] -> false
  | _ :: _ ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun r -> Str.string_match r normalized_filename 0) dirs

let builtins cx = Lazy.force cx.builtins

let builtin_value_opt cx = Builtins.get_builtin_value_opt (builtins cx)

let builtin_type_opt cx n = Builtins.get_builtin_type_opt (builtins cx) n

let builtin_module_opt cx = Builtins.get_builtin_module_opt (builtins cx)

let casting_syntax cx = cx.metadata.casting_syntax

let component_syntax cx = cx.metadata.component_syntax || File_key.is_lib_file cx.file

let hook_compatibility cx =
  in_dirlist cx cx.metadata.hook_compatibility_includes
  || (cx.metadata.hook_compatibility && not (in_dirlist cx cx.metadata.hook_compatibility_excludes))

let react_rule_enabled cx rule = List.mem rule cx.metadata.react_rules

let dev_only_refinement_info_as_errors cx = cx.metadata.dev_only_refinement_info_as_errors

let enable_const_params cx =
  cx.metadata.enable_const_params || cx.metadata.strict || cx.metadata.strict_local

let enable_enums cx = cx.metadata.enable_enums

let enable_jest_integration cx = cx.metadata.enable_jest_integration

let enable_pattern_matching cx =
  cx.metadata.enable_pattern_matching
  &&
  match cx.metadata.pattern_matching_includes with
  | [] -> true
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_constant_condition_boolean_literal cx =
  cx.metadata.constant_condition
  &&
  match cx.metadata.constant_condition_boolean_literal_includes with
  | [] -> false
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_constant_condition_null_void cx =
  cx.metadata.constant_condition
  &&
  match cx.metadata.constant_condition_null_void_includes with
  | [] -> false
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_constant_condition_function cx =
  cx.metadata.constant_condition
  &&
  match cx.metadata.constant_condition_function_includes with
  | [] -> false
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_invalid_comparison_general cx =
  cx.metadata.constant_condition
  &&
  match cx.metadata.invalid_comparison_general_includes with
  | [] -> false
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_invalid_comparison_null_check cx =
  cx.metadata.constant_condition
  &&
  match cx.metadata.invalid_comparison_null_check_includes with
  | [] -> false
  | dirs ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun prefix -> Base.String.is_prefix ~prefix normalized_filename) dirs

let enable_relay_integration cx =
  cx.metadata.enable_relay_integration
  && Relay_options.enabled_for_file cx.metadata.relay_integration_excludes (file cx)

let relay_integration_esmodules cx = cx.metadata.relay_integration_esmodules

let relay_integration_module_prefix cx =
  Relay_options.module_prefix_for_file
    cx.metadata.relay_integration_module_prefix_includes
    (file cx)
    cx.metadata.relay_integration_module_prefix

let errors cx = cx.ccx.errors

let error_suppressions cx = cx.ccx.error_suppressions

let evaluated cx = cx.ccx.sig_cx.evaluated

let exact_by_default cx = cx.metadata.exact_by_default

let file_options cx = cx.metadata.file_options

let aloc_tables cx = cx.ccx.aloc_tables

let find_props cx id =
  try Type.Properties.Map.find id cx.ccx.sig_cx.property_maps with
  | Not_found -> raise (Props_not_found id)

let find_call cx id =
  try IMap.find id cx.ccx.sig_cx.call_props with
  | Not_found -> raise (Call_not_found id)

let find_exports cx id =
  try Type.Exports.Map.find id cx.ccx.sig_cx.export_maps with
  | Not_found -> raise (Exports_not_found id)

let find_require cx mref =
  try cx.resolve_require mref with
  | Not_found -> raise (Require_not_found mref)

let find_tvar cx id =
  try IMap.find id cx.ccx.sig_cx.graph with
  | Not_found -> raise (Union_find.Tvar_not_found id)

let graph cx = cx.ccx.sig_cx.graph

let in_implicit_instantiation cx = cx.ccx.in_implicit_instantiation

let is_checked cx = cx.metadata.checked

let is_projects_strict_boundary_import_pattern_opt_outs cx import_specifier =
  if Base.Option.is_some (Files.haste_name_opt ~options:cx.metadata.file_options (file cx)) then
    let projects_options = cx.metadata.projects_options in
    let file = File_key.to_string (file cx) in
    let import_specifier = Flow_import_specifier.unwrap_userland import_specifier in
    Flow_projects.is_common_code_path ~opts:projects_options file
    && Flow_projects.is_import_specifier_that_opt_out_of_strict_boundary
         ~opts:projects_options
         ~import_specifier
  else
    false

let is_verbose cx =
  match cx.metadata.verbose with
  | None -> false
  | Some { Verbose.focused_files = None; enabled_during_flowlib; _ } ->
    enabled_during_flowlib || not (is_lib_file cx)
  | Some { Verbose.focused_files = Some files; enabled_during_flowlib; _ } ->
    let file = file cx in
    if File_key.is_lib_file file then
      enabled_during_flowlib
    else
      Base.List.mem files (File_key.to_string file) ~equal:String.equal

let is_strict cx = in_declare_module cx || cx.metadata.strict

let is_strict_local cx = cx.metadata.strict_local

let available_platforms cx = cx.metadata.available_platforms

let has_explicit_supports_platform cx = cx.metadata.has_explicit_supports_platform

let include_suppressions cx = cx.metadata.include_suppressions

let severity_cover cx = cx.ccx.severity_cover

let property_maps cx = cx.ccx.sig_cx.property_maps

let call_props cx = cx.ccx.sig_cx.call_props

let export_maps cx = cx.ccx.sig_cx.export_maps

let projects_options cx = cx.metadata.projects_options

let react_custom_jsx_typing cx = cx.metadata.react_custom_jsx_typing

let react_ref_as_prop cx = cx.metadata.react_ref_as_prop

let react_runtime cx = cx.metadata.react_runtime

let recursion_limit cx = cx.metadata.recursion_limit

let root cx = cx.metadata.root

let facebook_fbs cx = cx.metadata.facebook_fbs

let facebook_fbt cx = cx.metadata.facebook_fbt

let facebook_module_interop cx = cx.metadata.facebook_module_interop

let should_ignore_non_literal_requires cx = cx.metadata.ignore_non_literal_requires

let should_munge_underscores cx = cx.metadata.munge_underscores

let should_strip_root cx = cx.metadata.strip_root

let suppress_types cx = cx.metadata.suppress_types

let ts_syntax cx = cx.metadata.ts_syntax

let assert_operator_enabled cx = Options.AssertOperator.usable cx.metadata.assert_operator

let assert_operator_specialized cx = Options.AssertOperator.specialized cx.metadata.assert_operator

let opaque_type_new_bound_syntax cx = cx.metadata.opaque_type_new_bound_syntax

let type_expansion_recursion_limit cx = cx.metadata.type_expansion_recursion_limit

let literal_subtypes cx = cx.ccx.literal_subtypes

let delayed_forcing_tvars cx = cx.ccx.delayed_forcing_tvars

let post_component_tvar_forcing_states cx =
  let states = cx.ccx.post_component_tvar_forcing_states in
  cx.ccx.post_component_tvar_forcing_states <- [];
  states

let post_inference_polarity_checks cx = cx.ccx.post_inference_polarity_checks

let post_inference_validation_flows cx = cx.ccx.post_inference_validation_flows

let post_inference_projects_strict_boundary_import_pattern_opt_outs_validations cx =
  cx.ccx.post_inference_projects_strict_boundary_import_pattern_opt_outs_validations

let env_cache_find_opt cx ~for_value id =
  let cache =
    if for_value then
      cx.ccx.env_value_cache
    else
      cx.ccx.env_type_cache
  in
  IMap.find_opt id cache

let missing_local_annot_lower_bounds cx = cx.ccx.missing_local_annot_lower_bounds

let array_or_object_literal_declaration_upper_bounds cx =
  IMap.values cx.ccx.array_or_object_literal_declaration_upper_bounds

let inferred_component_return cx = cx.ccx.inferred_component_return

let matching_props cx = cx.ccx.matching_props

let use_mixed_in_catch_variables cx = cx.metadata.use_mixed_in_catch_variables

let ban_spread_key_props cx = cx.metadata.ban_spread_key_props

let verbose cx = cx.metadata.verbose

let slow_to_check_logging cx = cx.metadata.slow_to_check_logging

let max_workers cx = cx.metadata.max_workers

let missing_module_generators cx = cx.metadata.missing_module_generators

let no_unchecked_indexed_access cx = cx.metadata.no_unchecked_indexed_access

let jsx cx = cx.metadata.jsx

let exists_checks cx = cx.ccx.exists_checks

let exists_excuses cx = cx.ccx.exists_excuses

let voidable_checks cx = cx.ccx.voidable_checks

let reachable_deps cx = cx.reachable_deps

let environment cx = cx.environment

let typing_mode cx = cx.typing_mode

let show_typing_mode_frame = function
  | CheckingMode -> "CheckingMode"
  | SynthesisMode { target_loc = Some loc } ->
    Utils_js.spf "SynthesisMode (target=%s)" (Reason.string_of_aloc loc)
  | SynthesisMode { target_loc = None } -> "SynthesisMode (no target)"
  | HintEvaluationMode -> "HintEvaluationMode"

let show_typing_mode frames =
  frames |> Nel.to_list |> Base.List.map ~f:show_typing_mode_frame |> String.concat ", "

let node_cache cx = cx.node_cache

let refined_locations cx = cx.refined_locations

let aggressively_invalidated_locations cx = cx.aggressively_invalidated_locations

let hint_map_arglist_cache cx = cx.hint_map_arglist_cache

let hint_map_jsx_cache cx = cx.hint_map_jsx_cache

let hint_eval_cache_find_opt cx id = IMap.find_opt id cx.hint_eval_cache

let automatic_require_default cx = cx.metadata.automatic_require_default

let pid_prefix =
  let pid = lazy (Sys_utils.get_pretty_pid ()) in
  fun cx ->
    if max_workers cx > 0 then
      Printf.sprintf "[%d] " (Lazy.force pid)
    else
      ""

(* Create a shallow copy of this context, so that mutations to the sig_cx's
 * fields will not affect the copy. *)
let copy_of_context cx = { cx with ccx = { cx.ccx with sig_cx = cx.ccx.sig_cx } }

(* mutators *)

let add_exhaustive_check cx loc x =
  cx.ccx.exhaustive_checks <- ALocMap.add loc x cx.ccx.exhaustive_checks

let add_error cx error = cx.ccx.errors <- Flow_error.ErrorSet.add error cx.ccx.errors

let reset_errors cx errors = cx.ccx.errors <- errors

let add_error_suppressions cx suppressions =
  cx.ccx.error_suppressions <- Error_suppressions.union suppressions cx.ccx.error_suppressions

let add_severity_covers cx severity_covers =
  cx.ccx.severity_cover <- Utils_js.FilenameMap.union severity_covers cx.ccx.severity_cover

let add_property_map cx id pmap =
  let property_maps = Type.Properties.Map.add id pmap cx.ccx.sig_cx.property_maps in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with property_maps }

let add_call_prop cx id t =
  let call_props = IMap.add id t cx.ccx.sig_cx.call_props in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with call_props }

let add_export_map cx id tmap =
  let export_maps = Type.Exports.Map.add id tmap cx.ccx.sig_cx.export_maps in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with export_maps }

let add_tvar cx id bounds =
  let graph = IMap.add id bounds cx.ccx.sig_cx.graph in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph }

let set_synthesis_produced_uncacheable_result cx =
  match cx.typing_mode with
  | SynthesisMode { target_loc = _ } -> cx.ccx.synthesis_produced_uncacheable_result <- true
  | _ -> ()

let mk_placeholder cx reason =
  set_synthesis_produced_uncacheable_result cx;
  Type.AnyT.placeholder reason

let add_matching_props cx c = cx.ccx.matching_props <- c :: cx.ccx.matching_props

let add_literal_subtypes cx c = cx.ccx.literal_subtypes <- c :: cx.ccx.literal_subtypes

let add_post_component_tvar_forcing_state cx id state =
  cx.ccx.delayed_forcing_tvars <- ISet.add id cx.ccx.delayed_forcing_tvars;
  cx.ccx.post_component_tvar_forcing_states <- state :: cx.ccx.post_component_tvar_forcing_states

let add_post_inference_polarity_check cx tparams polarity t =
  cx.ccx.post_inference_polarity_checks <-
    (tparams, polarity, t) :: cx.ccx.post_inference_polarity_checks

let add_post_inference_validation_flow cx t use_t =
  cx.ccx.post_inference_validation_flows <- (t, use_t) :: cx.ccx.post_inference_validation_flows

let add_post_inference_subtyping_check cx l use_op u =
  add_post_inference_validation_flow cx l (Type.UseT (use_op, u))

let add_post_inference_projects_strict_boundary_import_pattern_opt_outs_validation
    cx l import_specifier projects =
  cx.ccx.post_inference_projects_strict_boundary_import_pattern_opt_outs_validations <-
    (l, import_specifier, projects)
    :: cx.ccx.post_inference_projects_strict_boundary_import_pattern_opt_outs_validations

let add_env_cache_entry cx ~for_value id t =
  if for_value then
    cx.ccx.env_value_cache <- IMap.add id t cx.ccx.env_value_cache
  else
    cx.ccx.env_type_cache <- IMap.add id t cx.ccx.env_type_cache

let add_voidable_check cx voidable_check =
  cx.ccx.voidable_checks <- voidable_check :: cx.ccx.voidable_checks

let add_monomorphized_component cx id t =
  cx.ccx.monomorphized_components <- Type.Properties.Map.add id t cx.ccx.monomorphized_components

let add_reachable_dep cx file_key =
  cx.reachable_deps <- Utils_js.FilenameSet.add file_key cx.reachable_deps

let add_refined_location cx read_loc refining_locs =
  cx.refined_locations <- ALocMap.add read_loc refining_locs cx.refined_locations

let add_aggressively_invalidated_location cx loc info =
  cx.aggressively_invalidated_locations <-
    ALocMap.add loc info cx.aggressively_invalidated_locations

let add_missing_local_annot_lower_bound cx loc t =
  let missing_local_annot_lower_bounds = cx.ccx.missing_local_annot_lower_bounds in
  let bounds =
    match ALocFuzzyMap.find_opt loc missing_local_annot_lower_bounds with
    | None -> Nel.one t
    | Some bounds -> Nel.cons t bounds
  in
  cx.ccx.missing_local_annot_lower_bounds <-
    ALocFuzzyMap.add loc bounds missing_local_annot_lower_bounds

let add_array_or_object_literal_declaration_tracking cx tvar_id loc =
  cx.ccx.array_or_object_literal_declaration_upper_bounds <-
    IMap.add tvar_id (loc, []) cx.ccx.array_or_object_literal_declaration_upper_bounds

let report_array_or_object_literal_declaration_reposition cx respositioned_tvar_id tvar_id =
  if IMap.mem tvar_id cx.ccx.array_or_object_literal_declaration_upper_bounds then
    cx.ccx.array_or_object_literal_declaration_reposition_tracking <-
      IMap.add
        respositioned_tvar_id
        tvar_id
        cx.ccx.array_or_object_literal_declaration_reposition_tracking

let add_array_or_object_literal_declaration_upper_bound cx tvar_id t =
  let array_or_object_literal_declaration_upper_bounds =
    cx.ccx.array_or_object_literal_declaration_upper_bounds
  in
  let tvar_id =
    IMap.find_opt tvar_id cx.ccx.array_or_object_literal_declaration_reposition_tracking
    |> Base.Option.value ~default:tvar_id
  in
  match IMap.find_opt tvar_id array_or_object_literal_declaration_upper_bounds with
  | None -> ()
  | Some (loc, bounds) ->
    cx.ccx.array_or_object_literal_declaration_upper_bounds <-
      IMap.add tvar_id (loc, t :: bounds) array_or_object_literal_declaration_upper_bounds

let add_inferred_component_return cx loc t =
  let inferred_component_return = cx.ccx.inferred_component_return in
  let bounds =
    match ALocFuzzyMap.find_opt loc inferred_component_return with
    | None -> Nel.one t
    | Some bounds -> Nel.cons t bounds
  in
  cx.ccx.inferred_component_return <- ALocFuzzyMap.add loc bounds inferred_component_return

let set_evaluated cx evaluated = cx.ccx.sig_cx <- { cx.ccx.sig_cx with evaluated }

let set_graph cx graph = cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph }

let run_in_implicit_instantiation_mode cx f =
  let saved = in_implicit_instantiation cx in
  cx.ccx.in_implicit_instantiation <- true;
  Exception.protect ~f ~finally:(fun () -> cx.ccx.in_implicit_instantiation <- saved)

let set_property_maps cx property_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with property_maps }

let set_call_props cx call_props = cx.ccx.sig_cx <- { cx.ccx.sig_cx with call_props }

let set_export_maps cx export_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with export_maps }

let set_exists_checks cx exists_checks = cx.ccx.exists_checks <- exists_checks

let set_signature_help_callee cx loc t =
  cx.ccx.signature_help_callee <- ALocMap.add loc t cx.ccx.signature_help_callee

let get_signature_help_callee cx loc = ALocMap.find_opt loc cx.ccx.signature_help_callee

let set_ctor_callee cx loc t = cx.ccx.ctor_callee <- ALocMap.add loc t cx.ccx.ctor_callee

let get_ctor_callee cx loc = ALocMap.find_opt loc cx.ccx.ctor_callee

let record_primitive_literal_check cx loc =
  cx.ccx.primitive_literal_checks <- ALocSet.add loc cx.ccx.primitive_literal_checks

let is_primitive_literal_checked cx loc = ALocSet.mem loc cx.ccx.primitive_literal_checks

let set_enclosing_context_for_call cx loc t =
  cx.ccx.enclosing_context_for_call <- ALocMap.add loc t cx.ccx.enclosing_context_for_call

let get_enclosing_context_for_call cx loc = ALocMap.find_opt loc cx.ccx.enclosing_context_for_call

let set_union_opt cx loc t = cx.ccx.union_opt <- ALocMap.add loc t cx.ccx.union_opt

let iter_union_opt cx ~f = ALocMap.iter f cx.ccx.union_opt

let add_exists_check cx loc t =
  let tset =
    match ALocMap.find_opt loc cx.ccx.exists_checks with
    | Some tset -> Type.TypeSet.add t tset
    | None -> Type.TypeSet.singleton t
  in
  set_exists_checks cx (ALocMap.add loc tset cx.ccx.exists_checks)

let set_exists_excuses cx exists_excuses = cx.ccx.exists_excuses <- exists_excuses

let set_environment cx env = cx.environment <- env

type cache_snapshot = {
  snapshot_constraint_cache: Type.FlowSet.t;
  snapshot_subst_cache: (subst_cache_err list * Type.t) Type.SubstCacheMap.t;
  snapshot_eval_id_cache: Type.t Type.EvalIdCacheMap.t * Type.Eval.id Type.IdCacheMap.t;
  snapshot_eval_repos_cache: Type.t Type.EvalReposCacheMap.t;
  snapshot_fix_cache: Type.t Type.FixCacheMap.t;
  snapshot_spread_cache: Spread_cache.t;
  snapshot_const_fold_cache: int Type.ConstFoldMap.t IMap.t;
  snapshot_evaluated: Type.t Type.Eval.Map.t;
  snapshot_instantiation_stack: TypeAppExpansion.entry list;
}

let take_cache_snapshot cx =
  let (eval_id_cache, id_cache) = cx.ccx.eval_id_cache in
  {
    snapshot_constraint_cache = !(cx.ccx.constraint_cache);
    snapshot_subst_cache = !(cx.ccx.subst_cache);
    snapshot_eval_id_cache = (!eval_id_cache, !id_cache);
    snapshot_eval_repos_cache = !(cx.ccx.eval_repos_cache);
    snapshot_fix_cache = !(cx.ccx.fix_cache);
    snapshot_spread_cache = !(cx.ccx.spread_cache);
    snapshot_const_fold_cache = !(cx.ccx.const_fold_cache);
    snapshot_evaluated = cx.ccx.sig_cx.evaluated;
    snapshot_instantiation_stack = !(cx.ccx.instantiation_stack);
  }

let restore_cache_snapshot cx snapshot =
  let {
    snapshot_constraint_cache;
    snapshot_subst_cache;
    snapshot_eval_id_cache = (snapshot_eval_id_cache, snapshot_id_cache);
    snapshot_eval_repos_cache;
    snapshot_fix_cache;
    snapshot_spread_cache;
    snapshot_const_fold_cache;
    snapshot_evaluated;
    snapshot_instantiation_stack;
  } =
    snapshot
  in
  let (eval_id_cache, id_cache) = cx.ccx.eval_id_cache in
  cx.ccx.constraint_cache := snapshot_constraint_cache;
  cx.ccx.subst_cache := snapshot_subst_cache;
  eval_id_cache := snapshot_eval_id_cache;
  id_cache := snapshot_id_cache;
  cx.ccx.eval_repos_cache := snapshot_eval_repos_cache;
  cx.ccx.fix_cache := snapshot_fix_cache;
  cx.ccx.spread_cache := snapshot_spread_cache;
  cx.ccx.const_fold_cache := snapshot_const_fold_cache;
  set_evaluated cx snapshot_evaluated;
  cx.ccx.instantiation_stack := snapshot_instantiation_stack

let run_and_rolled_back_cache cx f =
  let cache_snapshot = take_cache_snapshot cx in
  cx.ccx.instantiation_stack := [];
  Exception.protect ~f ~finally:(fun () -> restore_cache_snapshot cx cache_snapshot)

let run_in_synthesis_mode cx ?(reset_forcing_state = false) ~target_loc f =
  let old_typing_mode = cx.typing_mode in
  let old_synthesis_produced_uncacheable_result = cx.ccx.synthesis_produced_uncacheable_result in
  let old_delayed_forcing_tvars = cx.ccx.delayed_forcing_tvars in
  let old_post_component_tvar_forcing_states = cx.ccx.post_component_tvar_forcing_states in
  cx.ccx.synthesis_produced_uncacheable_result <- false;
  cx.typing_mode <- SynthesisMode { target_loc };
  let cache_snapshot = take_cache_snapshot cx in
  cx.ccx.instantiation_stack := [];
  let synthesis_produced_uncacheable_result = ref false in
  let result =
    Exception.protect ~f ~finally:(fun () ->
        restore_cache_snapshot cx cache_snapshot;
        cx.typing_mode <- old_typing_mode;
        synthesis_produced_uncacheable_result := cx.ccx.synthesis_produced_uncacheable_result;
        cx.ccx.synthesis_produced_uncacheable_result <- old_synthesis_produced_uncacheable_result;
        if reset_forcing_state then (
          cx.ccx.delayed_forcing_tvars <- old_delayed_forcing_tvars;
          cx.ccx.post_component_tvar_forcing_states <- old_post_component_tvar_forcing_states
        )
    )
  in
  (!synthesis_produced_uncacheable_result, result)

let run_in_synthesis_mode_with_errors cx ~target_loc ~f =
  (* Since this mode resets errors, we make sure to reset_forcing_state so that
   * we don't raise errors in a post-inference pass. *)
  run_in_synthesis_mode cx ~reset_forcing_state:true ~target_loc (fun () ->
      let original_errors = errors cx in
      reset_errors cx Flow_error.ErrorSet.empty;
      match f () with
      | exception exn ->
        let exn = Exception.wrap exn in
        reset_errors cx original_errors;
        Exception.reraise exn
      | t ->
        let new_errors = errors cx in
        reset_errors cx original_errors;
        (t, new_errors)
  )

let run_in_signature_tvar_env cx f =
  let saved_speculation_state = !(cx.ccx.speculation_state) in
  let saved_typing_mode = cx.typing_mode in
  let saved_instantiation_stack = !(cx.ccx.instantiation_stack) in
  cx.ccx.speculation_state := [];
  cx.ccx.instantiation_stack := [];
  cx.typing_mode <- CheckingMode;
  Exception.protect ~f ~finally:(fun () ->
      cx.typing_mode <- saved_typing_mode;
      cx.ccx.speculation_state := saved_speculation_state;
      cx.ccx.instantiation_stack := saved_instantiation_stack
  )

let run_in_hint_eval_mode cx f =
  let old_typing_mode = cx.typing_mode in
  (* We need to run type hint eval in an empty speculation state, since the hint eval is an
   * independent unit of type evaluation that's separate from an ongoing speculation. *)
  let saved_speculation_state = !(cx.ccx.speculation_state) in
  cx.ccx.speculation_state := [];
  cx.typing_mode <- HintEvaluationMode;
  let cache_snapshot = take_cache_snapshot cx in
  cx.ccx.instantiation_stack := [];
  Exception.protect ~f ~finally:(fun () ->
      restore_cache_snapshot cx cache_snapshot;
      cx.ccx.speculation_state := saved_speculation_state;
      cx.typing_mode <- old_typing_mode
  )

let test_prop_hit cx id =
  cx.ccx.test_prop_hits_and_misses <- IMap.add id Hit cx.ccx.test_prop_hits_and_misses

let test_prop_miss cx id name reasons use suggestion =
  if not (IMap.mem id cx.ccx.test_prop_hits_and_misses) then
    cx.ccx.test_prop_hits_and_misses <-
      IMap.add id (Miss (name, reasons, use, suggestion)) cx.ccx.test_prop_hits_and_misses

let test_prop_get_never_hit cx =
  List.fold_left
    (fun acc (_, hit_or_miss) ->
      match hit_or_miss with
      | Hit -> acc
      | Miss (name, reasons, use_op, suggestion) -> (name, reasons, use_op, suggestion) :: acc)
    []
    (IMap.bindings cx.ccx.test_prop_hits_and_misses)

let add_condition cx e = cx.ccx.conditions <- e :: cx.ccx.conditions

let get_all_conditions cx = List.rev cx.ccx.conditions

let add_strict_comparison cx comp = cx.ccx.strict_comparisons <- comp :: cx.ccx.strict_comparisons

let get_all_strict_comparisons cx = List.rev cx.ccx.strict_comparisons

let mark_optional_chain cx loc lhs_reason ~useful =
  cx.ccx.optional_chains_useful <-
    ALocMap.add
      loc
      (lhs_reason, useful)
      ~combine:(fun (r, u) (_, u') -> (r, u || u'))
      cx.ccx.optional_chains_useful

let unnecessary_optional_chains cx =
  ALocMap.fold
    (fun loc (r, useful) acc ->
      if useful then
        acc
      else
        (loc, r) :: acc)
    cx.ccx.optional_chains_useful
    []

let mark_maybe_unused_promise cx loc t ~async =
  cx.ccx.maybe_unused_promises <- (loc, t, async) :: cx.ccx.maybe_unused_promises

let maybe_unused_promises cx = cx.ccx.maybe_unused_promises

let add_hint_eval_cache_entry cx id result =
  cx.hint_eval_cache <- IMap.add id result cx.hint_eval_cache

(* utils *)
let find_real_props cx id =
  find_props cx id |> NameUtils.Map.filter (fun x _ -> not (Reason.is_internal_name x))

let iter_props cx id f = find_props cx id |> NameUtils.Map.iter f

let iter_real_props cx id f = find_real_props cx id |> NameUtils.Map.iter f

let fold_real_props cx id f = find_real_props cx id |> NameUtils.Map.fold f

let has_prop cx id x = find_props cx id |> NameUtils.Map.mem x

let get_prop cx id x = find_props cx id |> NameUtils.Map.find_opt x

(* constructors *)
let make_aloc_id cx aloc = ALoc.id_of_aloc cx.aloc_table aloc

let make_generic_id cx name loc = Generic.make_bound_id (make_aloc_id cx loc) name

let generate_property_map cx pmap =
  let id = Type.Properties.generate_id () in
  add_property_map cx id pmap;
  id

let make_source_property_map cx pmap ~type_sig aloc =
  (* To prevent cases where we might compare a concrete and an abstract
     aloc (like in a cycle) we abstractify all incoming alocs before adding
     them to the map. The only exception is for library files, which have only
     concrete definitions and by definition cannot appear in cycles. *)
  let id = make_aloc_id cx aloc |> Type.Properties.id_of_aloc_id ~type_sig in
  add_property_map cx id pmap;
  id

let make_call_prop cx t =
  let id = Reason.mk_id () in
  add_call_prop cx id t;
  id

let make_export_map cx tmap =
  let id = Type.Exports.mk_id () in
  add_export_map cx id tmap;
  id

let make_source_poly_id cx ~type_sig aloc =
  make_aloc_id cx aloc |> Type.Poly.id_of_aloc_id ~type_sig

let find_graph cx id = Type.Constraint.find_graph cx.ccx.sig_cx.graph id

let find_constraints cx id = Type.Constraint.find_constraints cx.ccx.sig_cx.graph id

let find_root cx id = Type.Constraint.find_root cx.ccx.sig_cx.graph id

let find_root_id cx id = Type.Constraint.find_root_id cx.ccx.sig_cx.graph id

let on_cyclic_tvar_error cx reason =
  let msg = Error_message.(ETrivialRecursiveDefinition (Reason.loc_of_reason reason, reason)) in
  let error = Flow_error.error_of_msg ~source_file:cx.file msg in
  if is_verbose cx then
    Utils_js.prerr_endlinef
      "\nCyclic type: %s"
      (Reason.dump_reason
         ~strip_root:
           ( if should_strip_root cx then
             Some (root cx)
           else
             None
           )
         reason
      );
  add_error cx error;
  Type.AnyT.error reason

let force_fully_resolved_tvar cx =
  Type.Constraint.ForcingState.force ~on_error:(on_cyclic_tvar_error cx)

let find_resolved =
  let rec loop cx seen t_in =
    match t_in with
    | Type.OpenT (_, id) ->
      if ISet.mem id seen then
        Some t_in
      else begin
        match find_graph cx id with
        | Type.Constraint.Resolved t -> loop cx (ISet.add id seen) t
        | Type.Constraint.FullyResolved s ->
          loop cx (ISet.add id seen) (force_fully_resolved_tvar cx s)
        | Type.Constraint.Unresolved _ -> None
      end
    | Type.AnnotT (_, t, _) -> loop cx seen t
    | t -> Some t
  in
  (fun cx t_in -> loop cx ISet.empty t_in)

let constraint_cache cx = cx.ccx.constraint_cache

let subst_cache cx = cx.ccx.subst_cache

let eval_id_cache cx = cx.ccx.eval_id_cache

let eval_repos_cache cx = cx.ccx.eval_repos_cache

let fix_cache cx = cx.ccx.fix_cache

let spread_cache cx = cx.ccx.spread_cache

let instantiation_stack cx = cx.ccx.instantiation_stack

let const_fold_cache cx = cx.ccx.const_fold_cache

let exhaustive_check cx loc = ALocMap.find loc cx.ccx.exhaustive_checks

let speculation_state cx = cx.ccx.speculation_state

let speculation_id cx =
  let open Speculation_state in
  match !(speculation_state cx) with
  | [] -> None
  | { speculation_id; case = { case_id; _ }; _ } :: _ -> Some (speculation_id, case_id)

let add_avar cx id node = cx.ccx.annot_graph <- IMap.add id node cx.ccx.annot_graph

let find_avar cx id = IMap.find id cx.ccx.annot_graph

let find_avar_opt cx id = IMap.find_opt id cx.ccx.annot_graph

let find_monomorphized_component cx id =
  Type.Properties.Map.find_opt id cx.ccx.monomorphized_components

let remove_avar cx id = cx.ccx.annot_graph <- IMap.remove id cx.ccx.annot_graph

let iter_annot_dependent_set cx f set =
  ISet.iter
    (fun i ->
      let constraints = find_avar cx i in
      let op = Type.AConstraint.to_annot_op_exn constraints in
      f i op)
    set

let new_specialized_callee cx =
  let open Type in
  Specialized_callee
    {
      init_speculation_state = speculation_id cx;
      finalized = [];
      speculative_candidates = [];
      sig_help = [];
    }

(* External uses shouldn't need to reset forcing state. *)
let run_in_synthesis_mode = run_in_synthesis_mode ~reset_forcing_state:false
