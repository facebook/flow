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

exception Require_not_found of string

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
  any_propagation: bool;
  automatic_require_default: bool;
  babel_loose_array_spread: bool;
  casting_syntax: Options.CastingSyntax.t;
  component_syntax: bool;
  hooklike_functions_includes: string list;
  hooklike_functions: bool;
  react_rules: Options.react_rules list;
  react_rules_always: bool;
  enable_as_const: bool;
  enable_const_params: bool;
  enable_enums: bool;
  enable_relay_integration: bool;
  exact_by_default: bool;
  facebook_fbs: string option;
  facebook_fbt: string option;
  facebook_module_interop: bool;
  file_options: Files.options;
  ignore_non_literal_requires: bool;
  max_literal_length: int;
  max_trace_depth: int;
  max_workers: int;
  missing_module_generators: (Str.regexp * string) list;
  namespaces: bool;
  react_runtime: Options.react_runtime;
  recursion_limit: int;
  relay_integration_esmodules: bool;
  relay_integration_excludes: Str.regexp list;
  relay_integration_module_prefix: string option;
  relay_integration_module_prefix_includes: Str.regexp list;
  root: File_path.t;
  strict_es6_import_export: bool;
  strict_es6_import_export_excludes: string list;
  strip_root: bool;
  suppress_types: SSet.t;
  ts_syntax: bool;
  use_mixed_in_catch_variables: bool;
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
  | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
  | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

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

  type entry = Type.t * RootSet.t list
end

type sig_t = Type.TypeContext.t

type master_context =
  | EmptyMasterContext
  | NonEmptyMasterContext of {
      builtin_leader_file_key: File_key.t;
      builtin_locs: Loc.t Type_sig_collections.Locs.t;
      builtins: Type_sig_collections.Locs.index Packed_type_sig.Builtins.t;
    }

type component_t = {
  mutable sig_cx: sig_t;
  mutable builtins: Builtins.t lazy_t;
  (* mapping from keyed alocs to concrete locations *)
  mutable aloc_tables: ALoc.table Lazy.t Utils_js.FilenameMap.t;
  (* map from goal ids to types *)
  mutable goal_map: Type.t IMap.t;
  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;
  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: ISet.t IMap.t;
  mutable synthesis_produced_placeholders: bool;
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
  mutable invariants_useful: (Reason.t * bool) ALocMap.t;
  mutable maybe_unused_promises: (ALoc.t * Type.t * bool) list;
  constraint_cache: Type.FlowSet.t ref;
  subst_cache: (subst_cache_err list * Type.t) Type.SubstCacheMap.t ref;
  repos_cache: Repos_cache.t ref;
  eval_id_cache: Type.t Type.EvalIdCacheMap.t ref * Type.Eval.id Type.IdCacheMap.t ref;
  eval_repos_cache: Type.t Type.EvalReposCacheMap.t ref;
  fix_cache: Type.t Type.FixCacheMap.t ref;
  spread_cache: Spread_cache.t ref;
  const_fold_cache: int Type.ConstFoldMap.t IMap.t ref;
  speculation_state: Speculation_state.t;
  instantiation_stack: TypeAppExpansion.entry list ref;
  (* Post-inference checks *)
  mutable literal_subtypes: (ALoc.t * Env_api.literal_check) list;
  mutable matching_props: (string * ALoc.t * ALoc.t) list;
  mutable post_inference_polarity_checks:
    (Type.typeparam Subst_name.Map.t * Polarity.t * Type.t) list;
  mutable post_inference_validation_flows: (Type.t * Type.use_t) list;
  mutable renders_type_argument_validations:
    (ALoc.t * Flow_ast.Type.Renders.variant * bool * Type.t) list;
  mutable env_value_cache: (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result IMap.t;
  mutable env_type_cache: (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result IMap.t;
  (* map from annot tvar ids to nodes used during annotation processing *)
  mutable annot_graph: Type.AConstraint.t IMap.t;
  (* Used to power an autofix that takes the lower bounds of types where we emit missing-local-annot
   * and turn them into annotations. This has to exist outside of the tvar graph because we will
   * eventually not use unresolved tvars to represent unannotated parameters. We use an ALocFuzzyMap
   * because we may compare keyed and concrete locations *)
  mutable missing_local_annot_lower_bounds: Type.t Nel.t ALocFuzzyMap.t;
  mutable exhaustive_checks: (ALoc.t list * bool) ALocMap.t;
  mutable in_implicit_instantiation: bool;
  (* Temporarily allow method unbinding in the following locs *)
  mutable allow_method_unbinding: ALocSet.t;
  (* React.Element does not store the monomorphized version of a component to support
   * cloning polymorphic elements. We need to know the monomorphized version of a component
   * to determine the render type of an element of a polymorphic component, so we keep track
   * of the monomorphized version here by mapping the Element's object id to the monomorphized
   * component *)
  mutable monomorphized_components: Type.t Type.Properties.Map.t;
  (* Signature help *)
  mutable signature_help_callee: Type.t ALocMap.t;
  (* Union optimization checks *)
  mutable union_opt: Type.t ALocMap.t;
}
[@@warning "-69"]

type typing_mode =
  | CheckingMode
  | SynthesisMode
  | HintEvaluationMode

type resolved_require = (Type.t, Reason.name) result

type t = {
  ccx: component_t;
  file: File_key.t;
  aloc_table: ALoc.table Lazy.t;
  metadata: metadata;
  resolve_require: resolve_require;
  hint_map_arglist_cache: (ALoc.t * Type.call_arg) list ALocMap.t ref;
  hint_map_jsx_cache:
    ( Reason.t * string * ALoc.t list * ALoc.t,
      Type.t Lazy.t * (Type.t list * Type.t option) Lazy.t
    )
    Hashtbl.t;
  mutable hint_eval_cache: Type.t option IMap.t;
  mutable environment: Loc_env.t;
  mutable typing_mode: typing_mode;
  (* A subset of all transitive dependencies of the current file as determined by import/require.
   * This set will only be populated with type sig files that are actually forced. *)
  mutable reachable_deps: Utils_js.FilenameSet.t;
  node_cache: Node_cache.t;
}

and resolve_require = string -> resolved_require

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
    any_propagation = Options.any_propagation options;
    automatic_require_default = Options.automatic_require_default options;
    babel_loose_array_spread = Options.babel_loose_array_spread options;
    casting_syntax = Options.casting_syntax options;
    component_syntax = Options.component_syntax options;
    hooklike_functions_includes = Options.hooklike_functions_includes options;
    hooklike_functions = Options.hooklike_functions options;
    react_rules = Options.react_rules options;
    react_rules_always = false;
    enable_as_const = Options.as_const options;
    enable_const_params = Options.enable_const_params options;
    enable_enums = Options.enums options;
    enable_relay_integration = Options.enable_relay_integration options;
    exact_by_default = Options.exact_by_default options;
    facebook_fbs = Options.facebook_fbs options;
    facebook_fbt = Options.facebook_fbt options;
    facebook_module_interop = Options.facebook_module_interop options;
    file_options = Options.file_options options;
    ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
    max_literal_length = Options.max_literal_length options;
    max_trace_depth = Options.max_trace_depth options;
    max_workers = Options.max_workers options;
    missing_module_generators = Options.missing_module_generators options;
    namespaces = Options.namespaces options;
    react_runtime = Options.react_runtime options;
    recursion_limit = Options.recursion_limit options;
    relay_integration_esmodules = Options.relay_integration_esmodules options;
    relay_integration_excludes = Options.relay_integration_excludes options;
    relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    relay_integration_module_prefix_includes =
      Options.relay_integration_module_prefix_includes options;
    root = Options.root options;
    strict_es6_import_export = Options.strict_es6_import_export options;
    strict_es6_import_export_excludes = Options.strict_es6_import_export_excludes options;
    strip_root = Options.should_strip_root options;
    suppress_types = Options.suppress_types options;
    ts_syntax = Options.ts_syntax options;
    use_mixed_in_catch_variables = Options.use_mixed_in_catch_variables options;
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
    let explicit_available_platforms = Docblock.supportsPlatform docblock_info in
    let available_platforms =
      Platform_set.available_platforms
        ~file_options
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
    builtins = lazy (Builtins.empty ());
    aloc_tables = Utils_js.FilenameMap.empty;
    goal_map = IMap.empty;
    type_graph = Graph_explorer.new_graph ();
    all_unresolved = IMap.empty;
    synthesis_produced_placeholders = false;
    matching_props = [];
    literal_subtypes = [];
    post_inference_polarity_checks = [];
    post_inference_validation_flows = [];
    renders_type_argument_validations = [];
    env_value_cache = IMap.empty;
    env_type_cache = IMap.empty;
    missing_local_annot_lower_bounds = ALocFuzzyMap.empty;
    errors = Flow_error.ErrorSet.empty;
    error_suppressions = Error_suppressions.empty;
    severity_cover = Utils_js.FilenameMap.empty;
    exists_checks = ALocMap.empty;
    exists_excuses = ALocMap.empty;
    voidable_checks = [];
    test_prop_hits_and_misses = IMap.empty;
    optional_chains_useful = ALocMap.empty;
    invariants_useful = ALocMap.empty;
    maybe_unused_promises = [];
    constraint_cache = ref Type.FlowSet.empty;
    subst_cache = ref Type.SubstCacheMap.empty;
    repos_cache = ref Repos_cache.empty;
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
    allow_method_unbinding = ALocSet.empty;
    monomorphized_components = Type.Properties.Map.empty;
    signature_help_callee = ALocMap.empty;
    union_opt = ALocMap.empty;
  }

let make ccx metadata file aloc_table resolve_require mk_builtins =
  ccx.aloc_tables <- Utils_js.FilenameMap.add file aloc_table ccx.aloc_tables;
  let cx =
    {
      ccx;
      file;
      aloc_table;
      metadata;
      resolve_require;
      hint_map_arglist_cache = ref ALocMap.empty;
      hint_map_jsx_cache = Hashtbl.create 0;
      hint_eval_cache = IMap.empty;
      environment = Loc_env.empty Name_def.Global;
      typing_mode = CheckingMode;
      reachable_deps = Utils_js.FilenameSet.empty;
      node_cache = Node_cache.mk_empty ();
    }
  in
  ccx.builtins <- lazy (mk_builtins cx);
  cx

let sig_cx cx = cx.ccx.sig_cx

(* modules *)

let in_declare_module cx = cx.environment.Loc_env.scope_kind = Name_def.DeclareModule

let in_declare_namespace cx = cx.environment.Loc_env.scope_kind = Name_def.DeclareNamespace

(* accessors *)

let all_unresolved cx = cx.ccx.all_unresolved

let metadata cx = cx.metadata

let max_literal_length cx = cx.metadata.max_literal_length

let babel_loose_array_spread cx = cx.metadata.babel_loose_array_spread

let builtins cx = Lazy.force cx.ccx.builtins

let file cx = cx.file

let in_dirlist cx dirs =
  match dirs with
  | [] -> false
  | _ :: _ ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun str -> Base.String.is_prefix ~prefix:str normalized_filename) dirs

let casting_syntax cx = cx.metadata.casting_syntax

let component_syntax cx = cx.metadata.component_syntax || File_key.is_lib_file cx.file

let hooklike_functions cx =
  cx.metadata.hooklike_functions || in_dirlist cx cx.metadata.hooklike_functions_includes

let react_rules_always cx = cx.metadata.react_rules_always

let react_rule_enabled cx rule = List.mem rule cx.metadata.react_rules

let enable_as_const cx = cx.metadata.enable_as_const

let enable_const_params cx =
  cx.metadata.enable_const_params || cx.metadata.strict || cx.metadata.strict_local

let enable_enums cx = cx.metadata.enable_enums

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

let goals cx = cx.ccx.goal_map

let exact_by_default cx = cx.metadata.exact_by_default

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

let is_verbose cx =
  match cx.metadata.verbose with
  | None -> false
  | Some { Verbose.focused_files = None; enabled_during_flowlib; _ } ->
    enabled_during_flowlib || not (File_key.is_lib_file (file cx))
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

let max_trace_depth cx = cx.metadata.max_trace_depth

let property_maps cx = cx.ccx.sig_cx.property_maps

let call_props cx = cx.ccx.sig_cx.call_props

let export_maps cx = cx.ccx.sig_cx.export_maps

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

let literal_subtypes cx = cx.ccx.literal_subtypes

let post_inference_polarity_checks cx = cx.ccx.post_inference_polarity_checks

let post_inference_validation_flows cx = cx.ccx.post_inference_validation_flows

let renders_type_argument_validations cx = cx.ccx.renders_type_argument_validations

let env_cache_find_opt cx ~for_value id =
  let cache =
    if for_value then
      cx.ccx.env_value_cache
    else
      cx.ccx.env_type_cache
  in
  IMap.find_opt id cache

let missing_local_annot_lower_bounds cx = cx.ccx.missing_local_annot_lower_bounds

let type_graph cx = cx.ccx.type_graph

let matching_props cx = cx.ccx.matching_props

let use_mixed_in_catch_variables cx = cx.metadata.use_mixed_in_catch_variables

let verbose cx = cx.metadata.verbose

let slow_to_check_logging cx = cx.metadata.slow_to_check_logging

let max_workers cx = cx.metadata.max_workers

let missing_module_generators cx = cx.metadata.missing_module_generators

let namespaces cx = cx.metadata.namespaces

let jsx cx = cx.metadata.jsx

let exists_checks cx = cx.ccx.exists_checks

let exists_excuses cx = cx.ccx.exists_excuses

let voidable_checks cx = cx.ccx.voidable_checks

let reachable_deps cx = cx.reachable_deps

let environment cx = cx.environment

let typing_mode cx = cx.typing_mode

let any_propagation cx = cx.metadata.any_propagation

let node_cache cx = cx.node_cache

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

let mk_placeholder cx reason =
  if cx.typing_mode = SynthesisMode then cx.ccx.synthesis_produced_placeholders <- true;
  Type.AnyT.placeholder reason

let add_matching_props cx c = cx.ccx.matching_props <- c :: cx.ccx.matching_props

let add_literal_subtypes cx c = cx.ccx.literal_subtypes <- c :: cx.ccx.literal_subtypes

let add_post_inference_polarity_check cx tparams polarity t =
  cx.ccx.post_inference_polarity_checks <-
    (tparams, polarity, t) :: cx.ccx.post_inference_polarity_checks

let add_post_inference_validation_flow cx t use_t =
  cx.ccx.post_inference_validation_flows <- (t, use_t) :: cx.ccx.post_inference_validation_flows

let add_post_inference_subtyping_check cx l use_op u =
  add_post_inference_validation_flow cx l (Type.UseT (use_op, u))

let add_renders_type_argument_validation cx ~allow_generic_t loc variant t =
  cx.ccx.renders_type_argument_validations <-
    (loc, variant, allow_generic_t, t) :: cx.ccx.renders_type_argument_validations

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

let add_missing_local_annot_lower_bound cx loc t =
  let missing_local_annot_lower_bounds = cx.ccx.missing_local_annot_lower_bounds in
  let bounds =
    match ALocFuzzyMap.find_opt loc missing_local_annot_lower_bounds with
    | None -> Nel.one t
    | Some bounds -> Nel.cons t bounds
  in
  cx.ccx.missing_local_annot_lower_bounds <-
    ALocFuzzyMap.add loc bounds missing_local_annot_lower_bounds

let set_all_unresolved cx all_unresolved = cx.ccx.all_unresolved <- all_unresolved

let set_evaluated cx evaluated = cx.ccx.sig_cx <- { cx.ccx.sig_cx with evaluated }

let set_goals cx goals = cx.ccx.goal_map <- goals

let set_graph cx graph = cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph }

let run_in_implicit_instantiation_mode cx f =
  let saved = in_implicit_instantiation cx in
  cx.ccx.in_implicit_instantiation <- true;
  Exception.protect ~f ~finally:(fun () -> cx.ccx.in_implicit_instantiation <- saved)

let set_property_maps cx property_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with property_maps }

let set_call_props cx call_props = cx.ccx.sig_cx <- { cx.ccx.sig_cx with call_props }

let set_export_maps cx export_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with export_maps }

let set_type_graph cx type_graph = cx.ccx.type_graph <- type_graph

let set_exists_checks cx exists_checks = cx.ccx.exists_checks <- exists_checks

let set_signature_help_callee cx loc t =
  cx.ccx.signature_help_callee <- ALocMap.add loc t cx.ccx.signature_help_callee

let get_signature_help_callee cx loc = ALocMap.find_opt loc cx.ccx.signature_help_callee

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
  snapshot_repos_cache: Repos_cache.t;
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
    snapshot_repos_cache = !(cx.ccx.repos_cache);
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
    snapshot_repos_cache;
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
  cx.ccx.repos_cache := snapshot_repos_cache;
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

let run_in_synthesis_mode cx f =
  let old_typing_mode = cx.typing_mode in
  let old_produced_placeholders = cx.ccx.synthesis_produced_placeholders in
  cx.ccx.synthesis_produced_placeholders <- false;
  cx.typing_mode <- SynthesisMode;
  let cache_snapshot = take_cache_snapshot cx in
  cx.ccx.instantiation_stack := [];
  let produced_placeholders = ref false in
  let result =
    Exception.protect ~f ~finally:(fun () ->
        restore_cache_snapshot cx cache_snapshot;
        cx.typing_mode <- old_typing_mode;
        produced_placeholders := cx.ccx.synthesis_produced_placeholders;
        cx.ccx.synthesis_produced_placeholders <- old_produced_placeholders
    )
  in
  (!produced_placeholders, result)

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

let with_allowed_method_unbinding cx loc f =
  let old_set = cx.ccx.allow_method_unbinding in
  cx.ccx.allow_method_unbinding <- ALocSet.add loc old_set;
  Exception.protect ~f ~finally:(fun () -> cx.ccx.allow_method_unbinding <- old_set)

let allowed_method_unbinding cx loc = ALocSet.mem loc cx.ccx.allow_method_unbinding

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

let mark_invariant cx loc reason ~useful =
  cx.ccx.invariants_useful <-
    ALocMap.add
      loc
      (reason, useful)
      ~combine:(fun (r, u) (_, u') -> (r, u || u'))
      cx.ccx.invariants_useful

let unnecessary_invariants cx =
  ALocMap.fold
    (fun loc (r, useful) acc ->
      if useful then
        acc
      else
        (loc, r) :: acc)
    cx.ccx.invariants_useful
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

let find_resolved =
  let rec loop cx seen t_in =
    match t_in with
    | Type.OpenT (_, id) ->
      if ISet.mem id seen then
        Some t_in
      else begin
        match find_graph cx id with
        | Type.Constraint.Resolved t
        | Type.Constraint.FullyResolved (lazy t) ->
          loop cx (ISet.add id seen) t
        | Type.Constraint.Unresolved _ -> None
      end
    | Type.AnnotT (_, t, _) -> loop cx seen t
    | t -> Some t
  in
  (fun cx t_in -> loop cx ISet.empty t_in)

let constraint_cache cx = cx.ccx.constraint_cache

let subst_cache cx = cx.ccx.subst_cache

let repos_cache cx = cx.ccx.repos_cache

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
