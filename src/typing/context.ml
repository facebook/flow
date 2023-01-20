(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type.TypeContext
module ALocMap = Loc_collections.ALocMap
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
  verbose: Verbose.t option;
  (* global *)
  any_propagation: bool;
  automatic_require_default: bool;
  babel_loose_array_spread: bool;
  conditional_type: bool;
  cycle_errors: bool;
  cycle_errors_includes: string list;
  enable_const_params: bool;
  enable_enums: bool;
  enable_relay_integration: bool;
  enforce_strict_call_arity: bool;
  inference_mode: Options.inference_mode;
  inference_mode_lti_includes: string list;
  exact_by_default: bool;
  facebook_fbs: string option;
  facebook_fbt: string option;
  facebook_module_interop: bool;
  haste_module_ref_prefix: string option;
  ignore_non_literal_requires: bool;
  array_literal_providers: bool;
  array_literal_providers_includes: string list;
  max_literal_length: int;
  max_trace_depth: int;
  max_workers: int;
  missing_module_generators: (Str.regexp * string) list;
  react_runtime: Options.react_runtime;
  react_server_component_exts: SSet.t;
  recursion_limit: int;
  relay_integration_excludes: Str.regexp list;
  relay_integration_module_prefix: string option;
  relay_integration_module_prefix_includes: Str.regexp list;
  root: Path.t;
  run_post_inference_implicit_instantiation: bool;
  enable_post_inference_targ_widened_check: bool;
  (* save_implicit_instantiation_results is used for the implicit instantiation
   * annotation codemod *)
  save_implicit_instantiation_results: bool;
  strict_es6_import_export: bool;
  strict_es6_import_export_excludes: string list;
  strip_root: bool;
  suppress_types: SSet.t;
  trust_mode: Options.trust_mode;
}

type test_prop_hit_or_miss =
  | Hit
  | Miss of Reason.name option * (Reason.t * Reason.t) * Type.use_op * string option

type voidable_check = {
  public_property_map: Type.Properties.id;
  private_property_map: Type.Properties.id;
  errors: ALoc.t Property_assignment.errors;
}

(* Equivalently, we could use a Reason.t option, but this is more self-documenting. *)
type computed_property_state =
  | ResolvedOnce of Reason.t
  | ResolvedMultipleTimes

type subst_cache_err =
  | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
  | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

type sig_t = Type.TypeContext.t

type master_context = {
  master_sig_cx: sig_t;
  builtins: Builtins.t;
}

type component_t = {
  mutable sig_cx: sig_t;
  mutable builtins: Builtins.t;
  (* mapping from keyed alocs to concrete locations *)
  mutable aloc_tables: ALoc.table Lazy.t Utils_js.FilenameMap.t;
  (* map from goal ids to types *)
  mutable goal_map: Type.t IMap.t;
  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;
  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: ISet.t IMap.t;
  mutable produced_placeholders: bool;
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
  (* A map from syntactic computed properties to the reasons of the first lower bound they receive.
   * If multiple lower bounds are received, we instead store ResolvedMultipleTimes so that we don't
   * emit multiple errors for a single syntactic computed property.
   *)
  mutable computed_property_states: computed_property_state IMap.t;
  mutable spread_widened_types: Type.Object.slice IMap.t;
  mutable optional_chains_useful: (Reason.t * bool) ALocMap.t;
  mutable invariants_useful: (Reason.t * bool) ALocMap.t;
  constraint_cache: Type.FlowSet.t ref;
  subst_cache: (subst_cache_err list * Type.t) Type.SubstCacheMap.t ref;
  instantiation_cache: Type.t Reason.ImplicitInstantiationReasonMap.t ref;
  repos_cache: Repos_cache.t ref;
  eval_id_cache: Type.t Type.EvalIdCacheMap.t ref * Type.Eval.id Type.IdCacheMap.t ref;
  eval_repos_cache: Type.t Type.EvalReposCacheMap.t ref;
  fix_cache: Type.t Type.FixCacheMap.t ref;
  spread_cache: Spread_cache.t;
  speculation_state: Speculation_state.t;
  (* Post-inference checks *)
  mutable literal_subtypes: (ALoc.t * Env_api.literal_check) list;
  mutable matching_props: (string * ALoc.t * ALoc.t) list;
  mutable implicit_instantiation_checks: Implicit_instantiation_check.t list;
  mutable implicit_instantiation_results: (Type.t * Subst_name.t) list ALocFuzzyMap.t;
  (* This are only used for the implicit instantiation codemod. Right before we run the post-
   * inference pass to get the error locations we change the implicit_instantiation_results map
   * into tys and set this field. We do this in order to avoid accidentally resolving types in the
   * results via the tvar resolver. We really want to get these types fully free from any pollution
   * that might happen during the post-pass. *)
  mutable implicit_instantiation_ty_results: (Ty.t option * Subst_name.t) list ALocFuzzyMap.t;
  mutable constrained_writes: (Type.t * Type.use_op * Type.t) list;
  mutable global_value_cache:
    (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result NameUtils.Map.t;
  mutable env_value_cache: (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result IMap.t;
  mutable env_type_cache: (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result IMap.t;
  (* map from annot tvar ids to nodes used during annotation processing *)
  mutable annot_graph: Type.AConstraint.node IMap.t;
  (* Used to power an autofix that takes the lower bounds of a parameter and turn them into an
   * annotation. This has to exist outside of the tvar graph because we will eventually not use
   * unresolved tvars to represent unannotated parameters. We use an ALocFuzzyMap because we may
   * compare keyed and concrete locations *)
  mutable call_arg_lower_bounds: Type.t Nel.t ALocFuzzyMap.t;
  mutable exhaustive_checks: (ALoc.t list * bool) ALocMap.t;
  mutable in_implicit_instantiation: bool;
}
[@@warning "-69"]

type phase =
  | InitLib
  | Checking
  | Merging
  | PostInference

let string_of_phase = function
  | InitLib -> "InitLib"
  | Checking -> "Checking"
  | Merging -> "Merging"
  | PostInference -> "PostInference"

type t = {
  ccx: component_t;
  file: File_key.t;
  mutable phase: phase;
  aloc_table: ALoc.table Lazy.t;
  metadata: metadata;
  module_info: Module_info.t;
  mutable require_map: Type.tvar ALocMap.t;
  trust_constructor: unit -> Trust.trust_rep;
  hint_map_arglist_cache: (ALoc.t * Type.call_arg) list ALocMap.t ref;
  hint_map_jsx_cache:
    (Reason.t * string * ALoc.t list * ALoc.t, Type.t * (Type.t list * Type.t option)) Hashtbl.t;
  mutable hint_eval_cache: Type.t option IMap.t;
  mutable declare_module_ref: Module_info.t option;
  mutable environment: Loc_env.t;
  mutable in_synthesis_mode: bool;
  node_cache: Node_cache.t;
}

let metadata_of_options options =
  {
    (* local *)
    checked = Options.all options;
    include_suppressions = Options.include_suppressions options;
    jsx = Options.Jsx_react;
    munge_underscores = Options.should_munge_underscores options;
    strict = false;
    strict_local = false;
    verbose = Options.verbose options;
    (* global *)
    any_propagation = Options.any_propagation options;
    automatic_require_default = Options.automatic_require_default options;
    babel_loose_array_spread = Options.babel_loose_array_spread options;
    conditional_type = Options.conditional_type options;
    cycle_errors = Options.cycle_errors options;
    cycle_errors_includes = Options.cycle_errors_includes options;
    enable_const_params = Options.enable_const_params options;
    enable_enums = Options.enums options;
    enable_relay_integration = Options.enable_relay_integration options;
    enforce_strict_call_arity = Options.enforce_strict_call_arity options;
    inference_mode = Options.inference_mode options;
    inference_mode_lti_includes = Options.inference_mode_lti_includes options;
    exact_by_default = Options.exact_by_default options;
    facebook_fbs = Options.facebook_fbs options;
    facebook_fbt = Options.facebook_fbt options;
    facebook_module_interop = Options.facebook_module_interop options;
    haste_module_ref_prefix = Options.haste_module_ref_prefix options;
    ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
    max_literal_length = Options.max_literal_length options;
    max_trace_depth = Options.max_trace_depth options;
    max_workers = Options.max_workers options;
    missing_module_generators = Options.missing_module_generators options;
    array_literal_providers = Options.array_literal_providers options;
    array_literal_providers_includes = Options.array_literal_providers_includes options;
    react_runtime = Options.react_runtime options;
    react_server_component_exts = Options.react_server_component_exts options;
    recursion_limit = Options.recursion_limit options;
    relay_integration_excludes = Options.relay_integration_excludes options;
    relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    relay_integration_module_prefix_includes =
      Options.relay_integration_module_prefix_includes options;
    root = Options.root options;
    run_post_inference_implicit_instantiation =
      Options.run_post_inference_implicit_instantiation options;
    enable_post_inference_targ_widened_check =
      Options.enable_post_inference_targ_widened_check options;
    save_implicit_instantiation_results = Options.save_implicit_instantiation_results options;
    strict_es6_import_export = Options.strict_es6_import_export options;
    strict_es6_import_export_excludes = Options.strict_es6_import_export_excludes options;
    strip_root = Options.should_strip_root options;
    suppress_types = Options.suppress_types options;
    trust_mode = Options.trust_mode options;
  }

let docblock_overrides docblock_info metadata =
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
    if Docblock.lti docblock_info then
      { metadata with inference_mode = Options.LTI }
    else
      metadata
  in
  metadata

let empty_sig_cx =
  {
    graph = IMap.empty;
    trust_graph = IMap.empty;
    property_maps = Type.Properties.Map.empty;
    call_props = IMap.empty;
    export_maps = Type.Exports.Map.empty;
    evaluated = Type.Eval.Map.empty;
  }

let empty_master_cx () = { master_sig_cx = empty_sig_cx; builtins = Builtins.empty () }

let make_ccx master_cx =
  {
    sig_cx = master_cx.master_sig_cx;
    builtins = master_cx.builtins;
    aloc_tables = Utils_js.FilenameMap.empty;
    goal_map = IMap.empty;
    type_graph = Graph_explorer.new_graph ();
    all_unresolved = IMap.empty;
    produced_placeholders = false;
    matching_props = [];
    literal_subtypes = [];
    constrained_writes = [];
    global_value_cache = NameUtils.Map.empty;
    env_value_cache = IMap.empty;
    env_type_cache = IMap.empty;
    call_arg_lower_bounds = ALocFuzzyMap.empty;
    errors = Flow_error.ErrorSet.empty;
    error_suppressions = Error_suppressions.empty;
    severity_cover = Utils_js.FilenameMap.empty;
    exists_checks = ALocMap.empty;
    exists_excuses = ALocMap.empty;
    voidable_checks = [];
    implicit_instantiation_checks = [];
    implicit_instantiation_results = ALocFuzzyMap.empty;
    implicit_instantiation_ty_results = ALocFuzzyMap.empty;
    test_prop_hits_and_misses = IMap.empty;
    computed_property_states = IMap.empty;
    spread_widened_types = IMap.empty;
    optional_chains_useful = ALocMap.empty;
    invariants_useful = ALocMap.empty;
    constraint_cache = ref Type.FlowSet.empty;
    subst_cache = ref Type.SubstCacheMap.empty;
    instantiation_cache = ref Reason.ImplicitInstantiationReasonMap.empty;
    repos_cache = ref Repos_cache.empty;
    eval_id_cache = (ref Type.EvalIdCacheMap.empty, ref Type.IdCacheMap.empty);
    eval_repos_cache = ref Type.EvalReposCacheMap.empty;
    fix_cache = ref Type.FixCacheMap.empty;
    spread_cache = ref IMap.empty;
    speculation_state = ref [];
    annot_graph = IMap.empty;
    exhaustive_checks = ALocMap.empty;
    in_implicit_instantiation = false;
  }

let make ccx metadata file aloc_table phase =
  ccx.aloc_tables <- Utils_js.FilenameMap.add file aloc_table ccx.aloc_tables;
  {
    ccx;
    file;
    phase;
    aloc_table;
    metadata;
    module_info = Module_info.empty_cjs_module ();
    require_map = ALocMap.empty;
    trust_constructor = Trust.literal_trust;
    hint_map_arglist_cache = ref ALocMap.empty;
    hint_map_jsx_cache = Hashtbl.create 0;
    hint_eval_cache = IMap.empty;
    declare_module_ref = None;
    environment = Loc_env.empty Name_def.Global;
    in_synthesis_mode = false;
    node_cache = Node_cache.mk_empty ();
  }

let sig_cx cx = cx.ccx.sig_cx

let trust_graph_sig sig_cx = sig_cx.trust_graph

(* modules *)

let push_declare_module cx info =
  match cx.declare_module_ref with
  | Some _ -> failwith "declare module must be one level deep"
  | None -> cx.declare_module_ref <- Some info

let pop_declare_module cx =
  match cx.declare_module_ref with
  | None -> failwith "pop empty declare module"
  | Some _ -> cx.declare_module_ref <- None

let module_info cx =
  match cx.declare_module_ref with
  | Some info -> info
  | None -> cx.module_info

let module_kind cx =
  let info = module_info cx in
  info.Module_info.kind

(* accessors *)
let current_phase cx = cx.phase

let all_unresolved cx = cx.ccx.all_unresolved

let trust_constructor cx = cx.trust_constructor

let cx_with_trust cx trust = { cx with trust_constructor = trust }

let metadata cx = cx.metadata

let max_literal_length cx = cx.metadata.max_literal_length

let babel_loose_array_spread cx = cx.metadata.babel_loose_array_spread

let builtins cx = cx.ccx.builtins

let enable_const_params cx =
  cx.metadata.enable_const_params || cx.metadata.strict || cx.metadata.strict_local

let enable_enums cx = cx.metadata.enable_enums

let file cx = cx.file

let enable_relay_integration cx =
  cx.metadata.enable_relay_integration
  && Relay_options.enabled_for_file cx.metadata.relay_integration_excludes (file cx)

let relay_integration_module_prefix cx =
  Relay_options.module_prefix_for_file
    cx.metadata.relay_integration_module_prefix_includes
    (file cx)
    cx.metadata.relay_integration_module_prefix

let in_dirlist cx dirs =
  match dirs with
  | [] -> false
  | _ :: _ ->
    let filename = File_key.to_string (file cx) in
    let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
    List.exists (fun str -> Base.String.is_prefix ~prefix:str normalized_filename) dirs

let lti cx =
  cx.metadata.inference_mode = Options.LTI || in_dirlist cx cx.metadata.inference_mode_lti_includes

let enforce_strict_call_arity cx = cx.metadata.enforce_strict_call_arity

let errors cx = cx.ccx.errors

let error_suppressions cx = cx.ccx.error_suppressions

let evaluated cx = cx.ccx.sig_cx.evaluated

let goals cx = cx.ccx.goal_map

let exact_by_default cx = cx.metadata.exact_by_default

let conditional_type cx = cx.metadata.conditional_type

let cycle_errors cx =
  cx.metadata.cycle_errors || lti cx || in_dirlist cx cx.metadata.cycle_errors_includes

let run_post_inference_implicit_instantiation cx =
  cx.metadata.run_post_inference_implicit_instantiation && not (lti cx)

let enable_post_inference_targ_widened_check cx =
  cx.metadata.enable_post_inference_targ_widened_check

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

let find_require cx loc =
  try ALocMap.find loc cx.require_map with
  | Not_found -> raise (Require_not_found (ALoc.debug_to_string ~include_source:true loc))

let find_tvar cx id =
  try IMap.find id cx.ccx.sig_cx.graph with
  | Not_found -> raise (Union_find.Tvar_not_found id)

let graph cx = cx.ccx.sig_cx.graph

let trust_graph cx = trust_graph_sig cx.ccx.sig_cx

let in_implicit_instantiation cx = cx.ccx.in_implicit_instantiation

let in_lti_implicit_instantiation cx =
  match (cx.phase, lti cx) with
  | (_, true) -> in_implicit_instantiation cx
  | (PostInference, _) -> in_implicit_instantiation cx
  | _ -> false

let is_checked cx = cx.metadata.checked

let is_verbose cx =
  match cx.metadata.verbose with
  | None -> false
  | Some { Verbose.focused_files = None; _ } -> true
  | Some { Verbose.focused_files = Some files; _ } ->
    let file = file cx in
    Base.List.mem files (File_key.to_string file) ~equal:String.equal

let is_strict cx = Base.Option.is_some cx.declare_module_ref || cx.metadata.strict

let is_strict_local cx = cx.metadata.strict_local

let include_suppressions cx = cx.metadata.include_suppressions

let severity_cover cx = cx.ccx.severity_cover

let max_trace_depth cx = cx.metadata.max_trace_depth

let require_map cx = cx.require_map

let property_maps cx = cx.ccx.sig_cx.property_maps

let call_props cx = cx.ccx.sig_cx.call_props

let export_maps cx = cx.ccx.sig_cx.export_maps

let react_runtime cx = cx.metadata.react_runtime

let in_react_server_component_file cx =
  let file = file cx in
  let exts = cx.metadata.react_server_component_exts in
  SSet.exists (File_key.check_suffix file) exts

let recursion_limit cx = cx.metadata.recursion_limit

let root cx = cx.metadata.root

let facebook_fbs cx = cx.metadata.facebook_fbs

let facebook_fbt cx = cx.metadata.facebook_fbt

let facebook_module_interop cx = cx.metadata.facebook_module_interop

let haste_module_ref_prefix cx = cx.metadata.haste_module_ref_prefix

let should_ignore_non_literal_requires cx = cx.metadata.ignore_non_literal_requires

let should_munge_underscores cx = cx.metadata.munge_underscores

let should_strip_root cx = cx.metadata.strip_root

let suppress_types cx = cx.metadata.suppress_types

let literal_subtypes cx = cx.ccx.literal_subtypes

let constrained_writes cx = cx.ccx.constrained_writes

let global_value_cache_find_opt cx name = NameUtils.Map.find_opt name cx.ccx.global_value_cache

let env_cache_find_opt cx ~for_value id =
  let cache =
    if for_value then
      cx.ccx.env_value_cache
    else
      cx.ccx.env_type_cache
  in
  IMap.find_opt id cache

let call_arg_lower_bounds cx = cx.ccx.call_arg_lower_bounds

let type_graph cx = cx.ccx.type_graph

let matching_props cx = cx.ccx.matching_props

let trust_mode cx = cx.metadata.trust_mode

let verbose cx = cx.metadata.verbose

let max_workers cx = cx.metadata.max_workers

let missing_module_generators cx = cx.metadata.missing_module_generators

let array_literal_providers cx =
  cx.metadata.array_literal_providers
  || lti cx
  || in_dirlist cx cx.metadata.array_literal_providers_includes

let jsx cx = cx.metadata.jsx

let exists_checks cx = cx.ccx.exists_checks

let exists_excuses cx = cx.ccx.exists_excuses

let voidable_checks cx = cx.ccx.voidable_checks

let implicit_instantiation_checks cx = cx.ccx.implicit_instantiation_checks

let implicit_instantiation_results cx = cx.ccx.implicit_instantiation_results

let implicit_instantiation_ty_results cx = cx.ccx.implicit_instantiation_ty_results

let environment cx = cx.environment

let in_synthesis_mode cx = cx.in_synthesis_mode

let any_propagation cx = cx.metadata.any_propagation

let node_cache cx = cx.node_cache

let hint_map_arglist_cache cx = cx.hint_map_arglist_cache

let hint_map_jsx_cache cx = cx.hint_map_jsx_cache

let hint_eval_cache_find_opt cx id = IMap.find_opt id cx.hint_eval_cache

let automatic_require_default cx = cx.metadata.automatic_require_default

let trust_tracking cx =
  match cx.metadata.trust_mode with
  | Options.CheckTrust
  | Options.SilentTrust ->
    true
  | Options.NoTrust -> false

let trust_errors cx =
  match cx.metadata.trust_mode with
  | Options.CheckTrust -> true
  | Options.SilentTrust
  | Options.NoTrust ->
    false

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

let add_error_suppression cx loc codes =
  cx.ccx.error_suppressions <- Error_suppressions.add loc codes cx.ccx.error_suppressions

let add_severity_cover cx filekey severity_cover =
  cx.ccx.severity_cover <- Utils_js.FilenameMap.add filekey severity_cover cx.ccx.severity_cover

let add_lint_suppressions cx suppressions =
  cx.ccx.error_suppressions <-
    Error_suppressions.add_lint_suppressions suppressions cx.ccx.error_suppressions

let add_require cx loc tvar = cx.require_map <- ALocMap.add loc tvar cx.require_map

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

let add_trust_var cx id bounds =
  let trust_graph = IMap.add id bounds cx.ccx.sig_cx.trust_graph in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with trust_graph }

let mk_placeholder cx reason =
  cx.ccx.produced_placeholders <- true;
  Type.AnyT.placeholder reason

let add_matching_props cx c = cx.ccx.matching_props <- c :: cx.ccx.matching_props

let add_literal_subtypes cx c = cx.ccx.literal_subtypes <- c :: cx.ccx.literal_subtypes

let add_constrained_write cx c = cx.ccx.constrained_writes <- c :: cx.ccx.constrained_writes

let add_global_value_cache_entry cx name t =
  cx.ccx.global_value_cache <- NameUtils.Map.add name t cx.ccx.global_value_cache

let add_env_cache_entry cx ~for_value id t =
  if for_value then
    cx.ccx.env_value_cache <- IMap.add id t cx.ccx.env_value_cache
  else
    cx.ccx.env_type_cache <- IMap.add id t cx.ccx.env_type_cache

let add_voidable_check cx voidable_check =
  cx.ccx.voidable_checks <- voidable_check :: cx.ccx.voidable_checks

let set_implicit_instantiation_ty_results cx results =
  cx.ccx.implicit_instantiation_ty_results <- results

let add_implicit_instantiation_result cx loc result =
  if cx.phase <> PostInference then
    cx.ccx.implicit_instantiation_results <-
      ALocFuzzyMap.add loc result cx.ccx.implicit_instantiation_results

let add_implicit_instantiation_check cx check =
  cx.ccx.implicit_instantiation_checks <- check :: cx.ccx.implicit_instantiation_checks

let add_possibly_speculating_implicit_instantiation_result cx loc result =
  let speculation_state = cx.ccx.speculation_state in
  match !speculation_state with
  | [] when cx.metadata.save_implicit_instantiation_results ->
    add_implicit_instantiation_result cx loc result
  | Speculation_state.{ case; _ } :: _ when cx.metadata.save_implicit_instantiation_results ->
    case.Speculation_state.implicit_instantiation_results <-
      ALocFuzzyMap.add loc result case.Speculation_state.implicit_instantiation_results
  | _ -> ()
(*TODO: Speculative implicit instantiations *)

let add_possibly_speculating_implicit_instantiation_check cx check =
  let open Speculation_state in
  let speculation_state = cx.ccx.speculation_state in
  match !speculation_state with
  | [] -> add_implicit_instantiation_check cx check
  | { case; _ } :: _ ->
    case.implicit_instantiation_post_inference_checks <-
      check :: case.implicit_instantiation_post_inference_checks

let add_implicit_instantiation_call cx lhs poly_t use_op reason funcalltype =
  if cx.metadata.run_post_inference_implicit_instantiation then
    let check = Implicit_instantiation_check.of_call lhs poly_t use_op reason funcalltype in
    add_possibly_speculating_implicit_instantiation_check cx check

let add_implicit_instantiation_ctor cx lhs poly_t use_op reason_op targs args =
  if cx.metadata.run_post_inference_implicit_instantiation then
    let check = Implicit_instantiation_check.of_ctor lhs poly_t use_op reason_op targs args in
    add_possibly_speculating_implicit_instantiation_check cx check

let add_implicit_instantiation_jsx
    cx lhs poly_t use_op reason_op clone ~component ~config ~targs children =
  if cx.metadata.run_post_inference_implicit_instantiation then
    let check =
      Implicit_instantiation_check.of_jsx
        lhs
        poly_t
        use_op
        reason_op
        clone
        ~component
        ~config
        ~targs
        children
    in
    add_possibly_speculating_implicit_instantiation_check cx check

let add_call_arg_lower_bound cx loc t =
  let call_arg_lower_bounds = cx.ccx.call_arg_lower_bounds in
  let call_args =
    match ALocFuzzyMap.find_opt loc call_arg_lower_bounds with
    | None -> Nel.one t
    | Some arg_ts -> Nel.cons t arg_ts
  in
  cx.ccx.call_arg_lower_bounds <- ALocFuzzyMap.add loc call_args call_arg_lower_bounds

let set_all_unresolved cx all_unresolved = cx.ccx.all_unresolved <- all_unresolved

let set_evaluated cx evaluated = cx.ccx.sig_cx <- { cx.ccx.sig_cx with evaluated }

let set_goals cx goals = cx.ccx.goal_map <- goals

let set_graph cx graph = cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph }

let run_in_implicit_instantiation_mode cx f =
  let saved = in_implicit_instantiation cx in
  cx.ccx.in_implicit_instantiation <- true;
  let result = Base.Result.try_with f in
  cx.ccx.in_implicit_instantiation <- saved;
  Base.Result.ok_exn result

let run_in_post_inference_mode cx f =
  let saved = cx.phase in
  cx.phase <- PostInference;
  let result = Base.Result.try_with f in
  cx.phase <- saved;
  Base.Result.ok_exn result

let set_trust_graph cx trust_graph = cx.ccx.sig_cx <- { cx.ccx.sig_cx with trust_graph }

let set_property_maps cx property_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with property_maps }

let set_call_props cx call_props = cx.ccx.sig_cx <- { cx.ccx.sig_cx with call_props }

let set_export_maps cx export_maps = cx.ccx.sig_cx <- { cx.ccx.sig_cx with export_maps }

let set_type_graph cx type_graph = cx.ccx.type_graph <- type_graph

let set_exists_checks cx exists_checks = cx.ccx.exists_checks <- exists_checks

let add_exists_check cx loc t =
  let tset =
    match ALocMap.find_opt loc cx.ccx.exists_checks with
    | Some tset -> Type.TypeSet.add t tset
    | None -> Type.TypeSet.singleton t
  in
  set_exists_checks cx (ALocMap.add loc tset cx.ccx.exists_checks)

let set_exists_excuses cx exists_excuses = cx.ccx.exists_excuses <- exists_excuses

let set_environment cx env = cx.environment <- env

let run_and_rolled_back_cache cx f =
  let saved_constraint_cache = !(cx.ccx.constraint_cache) in
  let saved_subst_cache = !(cx.ccx.subst_cache) in
  let saved_repos_cache = !(cx.ccx.repos_cache) in
  let (eval_id_cache, id_cache) = cx.ccx.eval_id_cache in
  let (saved_eval_id_cache, saved_id_cache) = (!eval_id_cache, !id_cache) in
  let saved_eval_repos_cache = !(cx.ccx.eval_repos_cache) in
  let saved_fix_cache = !(cx.ccx.fix_cache) in
  let saved_spread_cache = !(cx.ccx.spread_cache) in
  let result = Base.Result.try_with f in
  cx.ccx.constraint_cache := saved_constraint_cache;
  cx.ccx.subst_cache := saved_subst_cache;
  cx.ccx.repos_cache := saved_repos_cache;
  eval_id_cache := saved_eval_id_cache;
  id_cache := saved_id_cache;
  cx.ccx.eval_repos_cache := saved_eval_repos_cache;
  cx.ccx.fix_cache := saved_fix_cache;
  cx.ccx.spread_cache := saved_spread_cache;
  Base.Result.ok_exn result

let run_in_synthesis_mode cx f =
  let old_synthesis_mode = cx.in_synthesis_mode in
  let old_produced_placeholders = cx.ccx.produced_placeholders in
  cx.ccx.produced_placeholders <- false;
  cx.in_synthesis_mode <- true;
  let result = Base.Result.try_with f in
  cx.in_synthesis_mode <- old_synthesis_mode;
  let produced_placeholders = cx.ccx.produced_placeholders in
  cx.ccx.produced_placeholders <- old_produced_placeholders;
  (produced_placeholders, Base.Result.ok_exn result)

(* Given a sig context, it makes sense to clear the parts that are shared with
   the master sig context. Why? The master sig context, which contains global
   declarations, is an implicit dependency for every file, and so will be
   "merged in" anyway, thus making those shared parts redundant to carry around
   in other sig contexts. This saves a lot of shared memory as well as
   deserialization time. *)
let clear_master_shared cx master_cx =
  let { master_sig_cx = master_cx; _ } = master_cx in
  let module PMap = Type.Properties.Map in
  let module EMap = Type.Exports.Map in
  let sig_cx = cx.ccx.sig_cx in
  cx.ccx.sig_cx <-
    {
      graph = IMap.filter (fun id _ -> not (IMap.mem id master_cx.graph)) sig_cx.graph;
      trust_graph =
        IMap.filter (fun id _ -> not (IMap.mem id master_cx.trust_graph)) sig_cx.trust_graph;
      property_maps =
        PMap.filter (fun id _ -> not (PMap.mem id master_cx.property_maps)) sig_cx.property_maps;
      call_props =
        IMap.filter (fun id _ -> not (IMap.mem id master_cx.call_props)) sig_cx.call_props;
      evaluated =
        Type.Eval.Map.filter
          (fun id _ -> not (Type.Eval.Map.mem id master_cx.evaluated))
          sig_cx.evaluated;
      export_maps =
        EMap.filter (fun id _ -> not (EMap.mem id master_cx.export_maps)) sig_cx.export_maps;
    }

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

let computed_property_state_for_id cx id = IMap.find_opt id cx.ccx.computed_property_states

let computed_property_add_lower_bound cx id r =
  cx.ccx.computed_property_states <- IMap.add id (ResolvedOnce r) cx.ccx.computed_property_states

let computed_property_add_multiple_lower_bounds cx id =
  cx.ccx.computed_property_states <-
    IMap.add id ResolvedMultipleTimes cx.ccx.computed_property_states

let spread_widened_types_get_widest cx id = IMap.find_opt id cx.ccx.spread_widened_types

let spread_widened_types_add_widest cx id objtype =
  cx.ccx.spread_widened_types <- IMap.add id objtype cx.ccx.spread_widened_types

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

let set_prop cx id x p = find_props cx id |> NameUtils.Map.add x p |> add_property_map cx id

let has_export cx id name = find_exports cx id |> NameUtils.Map.mem name

let set_export cx id name t = find_exports cx id |> NameUtils.Map.add name t |> add_export_map cx id

(* constructors *)
let make_aloc_id cx aloc = ALoc.id_of_aloc cx.aloc_table aloc

let make_generic_id cx name loc = Generic.make_bound_id (make_aloc_id cx loc) name

let generate_property_map cx pmap =
  let id = Type.Properties.generate_id () in
  add_property_map cx id pmap;
  id

let make_source_property_map cx pmap aloc =
  (* To prevent cases where we might compare a concrete and an abstract
     aloc (like in a cycle) we abstractify all incoming alocs before adding
     them to the map. The only exception is for library files, which have only
     concrete definitions and by definition cannot appear in cycles. *)
  let id = make_aloc_id cx aloc |> Type.Properties.id_of_aloc_id in
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

let make_source_poly_id cx aloc = make_aloc_id cx aloc |> Type.Poly.id_of_aloc_id

(* Copy context from cx_other to cx *)
let merge_into ccx sig_cx_other =
  let sig_cx = ccx.sig_cx in
  ccx.sig_cx <-
    {
      property_maps = Type.Properties.Map.union sig_cx_other.property_maps sig_cx.property_maps;
      call_props = IMap.union sig_cx_other.call_props sig_cx.call_props;
      export_maps = Type.Exports.Map.union sig_cx_other.export_maps sig_cx.export_maps;
      evaluated = Type.Eval.Map.union sig_cx_other.evaluated sig_cx.evaluated;
      graph = IMap.union sig_cx_other.graph sig_cx.graph;
      trust_graph = IMap.union sig_cx_other.trust_graph sig_cx.trust_graph;
    }

let find_graph cx id =
  let (graph', constraints) = Type.Constraint.find_graph cx.ccx.sig_cx.graph id in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph = graph' };
  constraints

let find_constraints cx id =
  let (graph', root_id, constraints) = Type.Constraint.find_constraints cx.ccx.sig_cx.graph id in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph = graph' };
  (root_id, constraints)

let find_root cx id =
  let (graph', root_id, constraints) = Type.Constraint.find_root cx.ccx.sig_cx.graph id in
  cx.ccx.sig_cx <- { cx.ccx.sig_cx with graph = graph' };
  (root_id, constraints)

let find_resolved =
  let rec loop cx seen t_in =
    match t_in with
    | Type.OpenT (_, id) ->
      if ISet.mem id seen then
        Some t_in
      else begin
        match find_graph cx id with
        | Type.Constraint.Resolved (_, t)
        | Type.Constraint.FullyResolved (_, (lazy t)) ->
          loop cx (ISet.add id seen) t
        | Type.Constraint.Unresolved _ -> None
      end
    | Type.AnnotT (_, t, _) -> loop cx seen t
    | t -> Some t
  in
  (fun cx t_in -> loop cx ISet.empty t_in)

let rec find_trust_root cx (id : Trust_constraint.ident) =
  Trust_constraint.(
    match IMap.find_opt id (trust_graph cx) with
    | Some (TrustGoto next_id) ->
      let (root_id, root) = find_trust_root cx next_id in
      if root_id != next_id then Trust_constraint.new_goto root_id |> add_trust_var cx id;
      (root_id, root)
    | Some (TrustRoot root) -> (id, root)
    | None ->
      let msg =
        Utils_js.spf
          "find_trust_root: trust var %d not found in file %s"
          id
          (File_key.to_string @@ file cx)
      in
      Utils_js.assert_false msg
  )

let find_trust_constraints cx id =
  let (root_id, root) = find_trust_root cx id in
  (root_id, Trust_constraint.get_constraints root)

let find_trust_graph cx id =
  let (_, constraints) = find_trust_constraints cx id in
  constraints

let constraint_cache cx = cx.ccx.constraint_cache

let subst_cache cx = cx.ccx.subst_cache

let instantiation_cache cx = cx.ccx.instantiation_cache

let repos_cache cx = cx.ccx.repos_cache

let eval_id_cache cx = cx.ccx.eval_id_cache

let eval_repos_cache cx = cx.ccx.eval_repos_cache

let fix_cache cx = cx.ccx.fix_cache

let spread_cache cx = cx.ccx.spread_cache

let exhaustive_check cx loc = ALocMap.find loc cx.ccx.exhaustive_checks

let speculation_state cx = cx.ccx.speculation_state

let speculation_id cx =
  let open Speculation_state in
  match !(speculation_state cx) with
  | [] -> None
  | { speculation_id; case = { case_id; _ }; _ } :: _ -> Some (speculation_id, case_id)

let add_avar cx id node = cx.ccx.annot_graph <- IMap.add id node cx.ccx.annot_graph

let find_avar_exn cx id =
  let (annot_graph', root_id, constraints) = Type.AConstraint.find_root cx.ccx.annot_graph id in
  cx.ccx.annot_graph <- annot_graph';
  (root_id, constraints)

let find_avar cx id =
  try find_avar_exn cx id with
  | Union_find.Tvar_not_found root_id ->
    (* When an id is missing in the annot graph, then it _must_ be resolved and
     * part of the type graph. *)
    add_avar cx root_id Type.AConstraint.fully_resolved_node;
    (root_id, Type.AConstraint.fully_resolved_root)

let iter_annot_dependent_set cx f set =
  let (_ : ISet.t) =
    ISet.fold
      (fun i seen ->
        let (root_id, root) = find_avar cx i in
        if ISet.mem root_id seen then
          seen
        else
          let constraints = root.Type.AConstraint.constraints in
          let op = Type.AConstraint.to_annot_op_exn constraints in
          let () = f root_id op in
          ISet.add root_id seen)
      set
      ISet.empty
  in
  ()
