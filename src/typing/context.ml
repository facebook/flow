(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocMap = Loc_collections.ALocMap
module Scope_api = Scope_api.With_ALoc

exception Props_not_found of Type.Properties.id

exception Call_not_found of int

exception Exports_not_found of Type.Exports.id

exception Require_not_found of string

exception Module_not_found of string

exception Tvar_not_found of Constraint.ident

type env = Scope.t list

type metadata = {
  (* local *)
  checked: bool;
  munge_underscores: bool;
  verbose: Verbose.t option;
  weak: bool;
  include_suppressions: bool;
  jsx: Options.jsx_mode;
  strict: bool;
  strict_local: bool;
  (* global *)
  automatic_require_default: bool;
  babel_loose_array_spread: bool;
  max_literal_length: int;
  enable_const_params: bool;
  enable_enums: bool;
  enforce_strict_call_arity: bool;
  esproposal_class_static_fields: Options.esproposal_feature_mode;
  esproposal_class_instance_fields: Options.esproposal_feature_mode;
  esproposal_decorators: Options.esproposal_feature_mode;
  esproposal_export_star_as: Options.esproposal_feature_mode;
  esproposal_optional_chaining: Options.esproposal_feature_mode;
  esproposal_nullish_coalescing: Options.esproposal_feature_mode;
  exact_by_default: bool;
  facebook_fbs: string option;
  facebook_fbt: string option;
  facebook_module_interop: bool;
  haste_module_ref_prefix: string option;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  react_runtime: Options.react_runtime;
  recursion_limit: int;
  root: Path.t;
  strict_es6_import_export: bool;
  strict_es6_import_export_excludes: string list;
  strip_root: bool;
  suppress_types: SSet.t;
  max_workers: int;
  default_lib_dir: Path.t option;
  trust_mode: Options.trust_mode;
  type_asserts: bool;
}

type test_prop_hit_or_miss =
  | Hit
  | Miss of string option * (Reason.t * Reason.t) * Type.use_op

type type_assert_kind =
  | Is
  | Throws
  | Wraps

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

type sig_t = {
  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint.node IMap.t;
  (* map from tvar ids to trust nodes *)
  mutable trust_graph: Trust_constraint.node IMap.t;
  (* obj types point to mutable property maps *)
  mutable property_maps: Type.Properties.map;
  (* indirection to support context opt *)
  mutable call_props: Type.t IMap.t;
  (* modules point to mutable export maps *)
  mutable export_maps: Type.Exports.map;
  (* map from evaluation ids to types *)
  mutable evaluated: Type.t Type.Eval.Map.t;
  (* map from module names to their types *)
  mutable module_map: Type.t SMap.t;
}

type component_t = {
  sig_cx: sig_t;
  (* mapping from keyed alocs to concrete locations *)
  aloc_tables: ALoc.table Lazy.t Utils_js.FilenameMap.t;
  (* map from goal ids to types *)
  mutable goal_map: Type.t IMap.t;
  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;
  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: ISet.t IMap.t;
  (* map from frame ids to env snapshots *)
  mutable envs: env IMap.t;
  (* We track nominal ids in the context to help decide when the types exported by a module have
     meaningfully changed: see Merge_js.ContextOptimizer. We care about two different types of
     nominal ids, one for object properties and one for polymorphic types. **)
  mutable nominal_prop_ids: ISet.t;
  mutable nominal_poly_ids: Type.Poly.Set.t;
  (* map from TypeAssert assertion locations to the type being asserted *)
  mutable type_asserts_map: (type_assert_kind * ALoc.t) ALocMap.t;
  mutable errors: Flow_error.ErrorSet.t;
  mutable error_suppressions: Error_suppressions.t;
  mutable severity_cover: ExactCover.lint_severity_cover Utils_js.FilenameMap.t;
  (* map from exists proposition locations to the types of values running through them *)
  mutable exists_checks: ExistsCheck.t ALocMap.t;
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
  subst_cache: (Type.Poly.id * Type.t list, subst_cache_err list * Type.t) Hashtbl.t;
  instantiation_cache: (Reason.t * Reason.t * Reason.t Nel.t, Type.t) Hashtbl.t;
  repos_cache: Repos_cache.t ref;
  eval_id_cache:
    (Type.Eval.id, Type.t) Hashtbl.t * (Type.t * Type.defer_use_t, Type.Eval.id) Hashtbl.t;
  eval_repos_cache: (Type.t * Type.defer_use_t * Type.Eval.id, Type.t) Hashtbl.t;
  fix_cache: (Reason.t * Type.t, Type.t) Hashtbl.t;
  spread_cache: Spread_cache.t;
  speculation_state: Speculation_state.t;
  (* Post-inference checks *)
  mutable literal_subtypes: (Type.t * Type.use_t) list;
  mutable matching_props: (Reason.reason * string * Type.t * Type.t) list;
}

type phase =
  | Checking
  | Merging
  | Normalizing

type t = {
  ccx: component_t;
  file: File_key.t;
  phase: phase;
  rev_aloc_table: ALoc.reverse_table Lazy.t;
  metadata: metadata;
  module_info: Module_info.t;
  mutable require_map: Type.t ALocMap.t;
  trust_constructor: unit -> Trust.trust_rep;
  mutable declare_module_ref: Module_info.t option;
  mutable use_def: Scope_api.info * Ssa_api.With_ALoc.values;
  mutable exported_locals: Loc_collections.ALocSet.t SMap.t option;
}

let metadata_of_options options =
  {
    (* local *)
    checked = Options.all options;
    munge_underscores = Options.should_munge_underscores options;
    verbose = Options.verbose options;
    weak = Options.weak_by_default options;
    include_suppressions = Options.include_suppressions options;
    jsx = Options.Jsx_react;
    strict = false;
    strict_local = false;
    (* global *)
    automatic_require_default = Options.automatic_require_default options;
    babel_loose_array_spread = Options.babel_loose_array_spread options;
    max_literal_length = Options.max_literal_length options;
    enable_const_params = Options.enable_const_params options;
    enable_enums = Options.enums options;
    enforce_strict_call_arity = Options.enforce_strict_call_arity options;
    esproposal_class_instance_fields = Options.esproposal_class_instance_fields options;
    esproposal_class_static_fields = Options.esproposal_class_static_fields options;
    esproposal_decorators = Options.esproposal_decorators options;
    esproposal_export_star_as = Options.esproposal_export_star_as options;
    esproposal_optional_chaining = Options.esproposal_optional_chaining options;
    esproposal_nullish_coalescing = Options.esproposal_nullish_coalescing options;
    exact_by_default = Options.exact_by_default options;
    facebook_fbs = Options.facebook_fbs options;
    facebook_fbt = Options.facebook_fbt options;
    facebook_module_interop = Options.facebook_module_interop options;
    haste_module_ref_prefix = Options.haste_module_ref_prefix options;
    ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
    max_trace_depth = Options.max_trace_depth options;
    max_workers = Options.max_workers options;
    react_runtime = Options.react_runtime options;
    recursion_limit = Options.recursion_limit options;
    root = Options.root options;
    strict_es6_import_export = Options.strict_es6_import_export options;
    strict_es6_import_export_excludes = Options.strict_es6_import_export_excludes options;
    strip_root = Options.should_strip_root options;
    suppress_types = Options.suppress_types options;
    default_lib_dir = (Options.file_options options).Files.default_lib_dir;
    trust_mode = Options.trust_mode options;
    type_asserts = Options.type_asserts options;
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
    match Docblock.flow docblock_info with
    | None -> metadata
    | Some Docblock.OptIn -> { metadata with checked = true }
    | Some Docblock.OptInStrict -> { metadata with checked = true; strict = true }
    | Some Docblock.OptInStrictLocal -> { metadata with checked = true; strict_local = true }
    | Some Docblock.OptInWeak -> { metadata with checked = true; weak = true }
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
  metadata

let empty_use_def = (Scope_api.{ max_distinct = 0; scopes = IMap.empty }, ALocMap.empty)

let make_sig () =
  {
    graph = IMap.empty;
    trust_graph = IMap.empty;
    property_maps = Type.Properties.Map.empty;
    call_props = IMap.empty;
    export_maps = Type.Exports.Map.empty;
    evaluated = Type.Eval.Map.empty;
    module_map = SMap.empty;
  }

let make_ccx sig_cx aloc_tables =
  {
    sig_cx;
    aloc_tables;
    goal_map = IMap.empty;
    type_graph = Graph_explorer.new_graph ();
    all_unresolved = IMap.empty;
    envs = IMap.empty;
    nominal_poly_ids = Type.Poly.Set.empty;
    nominal_prop_ids = ISet.empty;
    type_asserts_map = ALocMap.empty;
    matching_props = [];
    literal_subtypes = [];
    errors = Flow_error.ErrorSet.empty;
    error_suppressions = Error_suppressions.empty;
    severity_cover = Utils_js.FilenameMap.empty;
    exists_checks = ALocMap.empty;
    exists_excuses = ALocMap.empty;
    voidable_checks = [];
    test_prop_hits_and_misses = IMap.empty;
    computed_property_states = IMap.empty;
    spread_widened_types = IMap.empty;
    optional_chains_useful = ALocMap.empty;
    invariants_useful = ALocMap.empty;
    constraint_cache = ref Type.FlowSet.empty;
    subst_cache = Hashtbl.create 0;
    instantiation_cache = Hashtbl.create 0;
    repos_cache = ref Repos_cache.empty;
    eval_id_cache = (Hashtbl.create 0, Hashtbl.create 0);
    eval_repos_cache = Hashtbl.create 0;
    fix_cache = Hashtbl.create 0;
    spread_cache = Hashtbl.create 0;
    speculation_state = ref [];
  }

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let make ccx metadata file rev_aloc_table module_ref phase =
  {
    ccx;
    file;
    phase;
    rev_aloc_table;
    metadata;
    module_info = Module_info.empty_cjs_module module_ref;
    require_map = ALocMap.empty;
    trust_constructor = Trust.literal_trust;
    declare_module_ref = None;
    use_def = empty_use_def;
    exported_locals = None;
  }

let sig_cx cx = cx.ccx.sig_cx

let graph_sig sig_cx = sig_cx.graph

let trust_graph_sig sig_cx = sig_cx.trust_graph

let find_module_sig sig_cx m =
  (try SMap.find m sig_cx.module_map with Not_found -> raise (Module_not_found m))

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

let module_ref cx =
  let info = module_info cx in
  info.Module_info.ref

(* accessors *)
let current_phase cx = cx.phase

let all_unresolved cx = cx.ccx.all_unresolved

let envs cx = cx.ccx.envs

let trust_constructor cx = cx.trust_constructor

let cx_with_trust cx trust = { cx with trust_constructor = trust }

let metadata cx = cx.metadata

let max_literal_length cx = cx.metadata.max_literal_length

let babel_loose_array_spread cx = cx.metadata.babel_loose_array_spread

let enable_const_params cx =
  cx.metadata.enable_const_params || cx.metadata.strict || cx.metadata.strict_local

let enable_enums cx = cx.metadata.enable_enums

let enforce_strict_call_arity cx = cx.metadata.enforce_strict_call_arity

let errors cx = cx.ccx.errors

let error_suppressions cx = cx.ccx.error_suppressions

let esproposal_class_static_fields cx = cx.metadata.esproposal_class_static_fields

let esproposal_class_instance_fields cx = cx.metadata.esproposal_class_instance_fields

let esproposal_decorators cx = cx.metadata.esproposal_decorators

let esproposal_export_star_as cx = cx.metadata.esproposal_export_star_as

let esproposal_optional_chaining cx = cx.metadata.esproposal_optional_chaining

let esproposal_nullish_coalescing cx = cx.metadata.esproposal_nullish_coalescing

let evaluated cx = cx.ccx.sig_cx.evaluated

let goals cx = cx.ccx.goal_map

let exact_by_default cx = cx.metadata.exact_by_default

let file cx = cx.file

let aloc_tables cx = cx.ccx.aloc_tables

let find_props cx id =
  try Type.Properties.Map.find id cx.ccx.sig_cx.property_maps
  with Not_found -> raise (Props_not_found id)

let find_call cx id =
  (try IMap.find id cx.ccx.sig_cx.call_props with Not_found -> raise (Call_not_found id))

let find_exports cx id =
  try Type.Exports.Map.find id cx.ccx.sig_cx.export_maps
  with Not_found -> raise (Exports_not_found id)

let find_require cx loc =
  try ALocMap.find loc cx.require_map
  with Not_found -> raise (Require_not_found (ALoc.debug_to_string ~include_source:true loc))

let find_module cx m = find_module_sig (sig_cx cx) m

let find_tvar cx id =
  (try IMap.find id cx.ccx.sig_cx.graph with Not_found -> raise (Tvar_not_found id))

let mem_nominal_poly_id cx id = Type.Poly.Set.mem id cx.ccx.nominal_poly_ids

let mem_nominal_prop_id cx id = ISet.mem id cx.ccx.nominal_prop_ids

let graph cx = graph_sig cx.ccx.sig_cx

let trust_graph cx = trust_graph_sig cx.ccx.sig_cx

let is_checked cx = cx.metadata.checked

let is_verbose cx = cx.metadata.verbose <> None

let is_weak cx = cx.metadata.weak

let is_strict cx = Base.Option.is_some cx.declare_module_ref || cx.metadata.strict

let is_strict_local cx = cx.metadata.strict_local

let include_suppressions cx = cx.metadata.include_suppressions

let severity_cover cx = cx.ccx.severity_cover

let max_trace_depth cx = cx.metadata.max_trace_depth

let require_map cx = cx.require_map

let module_map cx = cx.ccx.sig_cx.module_map

let property_maps cx = cx.ccx.sig_cx.property_maps

let call_props cx = cx.ccx.sig_cx.call_props

let export_maps cx = cx.ccx.sig_cx.export_maps

let react_runtime cx = cx.metadata.react_runtime

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

let default_lib_dir cx = cx.metadata.default_lib_dir

let type_asserts_map cx = cx.ccx.type_asserts_map

let literal_subtypes cx = cx.ccx.literal_subtypes

let type_graph cx = cx.ccx.type_graph

let matching_props cx = cx.ccx.matching_props

let trust_mode cx = cx.metadata.trust_mode

let type_asserts cx = cx.metadata.type_asserts

let verbose cx = cx.metadata.verbose

let max_workers cx = cx.metadata.max_workers

let jsx cx = cx.metadata.jsx

let exists_checks cx = cx.ccx.exists_checks

let exists_excuses cx = cx.ccx.exists_excuses

let voidable_checks cx = cx.ccx.voidable_checks

let use_def cx = cx.use_def

let exported_locals cx = cx.exported_locals

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

let pid_prefix (cx : t) =
  if max_workers cx > 0 then
    Printf.sprintf "[%d] " (Unix.getpid ())
  else
    ""

(* Create a shallow copy of this context, so that mutations to the sig_cx's
 * fields will not affect the copy. *)
let copy_of_context cx =
  {
    cx with
    ccx =
      {
        cx.ccx with
        sig_cx =
          {
            cx.ccx.sig_cx with
            graph = cx.ccx.sig_cx.graph;
            trust_graph = cx.ccx.sig_cx.trust_graph;
          };
      };
  }

(* mutators *)
let add_env cx frame env = cx.ccx.envs <- IMap.add frame env cx.ccx.envs

let add_error cx error = cx.ccx.errors <- Flow_error.ErrorSet.add error cx.ccx.errors

let add_error_suppression cx loc codes =
  cx.ccx.error_suppressions <- Error_suppressions.add loc codes cx.ccx.error_suppressions

let add_severity_cover cx filekey severity_cover =
  cx.ccx.severity_cover <- Utils_js.FilenameMap.add filekey severity_cover cx.ccx.severity_cover

let add_lint_suppressions cx suppressions =
  cx.ccx.error_suppressions <-
    Error_suppressions.add_lint_suppressions suppressions cx.ccx.error_suppressions

let add_require cx loc tvar = cx.require_map <- ALocMap.add loc tvar cx.require_map

let add_module cx name tvar =
  cx.ccx.sig_cx.module_map <- SMap.add name tvar cx.ccx.sig_cx.module_map

let add_property_map cx id pmap =
  cx.ccx.sig_cx.property_maps <- Type.Properties.Map.add id pmap cx.ccx.sig_cx.property_maps

let add_call_prop cx id t = cx.ccx.sig_cx.call_props <- IMap.add id t cx.ccx.sig_cx.call_props

let add_export_map cx id tmap =
  cx.ccx.sig_cx.export_maps <- Type.Exports.Map.add id tmap cx.ccx.sig_cx.export_maps

let add_tvar cx id bounds = cx.ccx.sig_cx.graph <- IMap.add id bounds cx.ccx.sig_cx.graph

let add_trust_var cx id bounds =
  cx.ccx.sig_cx.trust_graph <- IMap.add id bounds cx.ccx.sig_cx.trust_graph

let add_nominal_prop_id cx id = cx.ccx.nominal_prop_ids <- ISet.add id cx.ccx.nominal_prop_ids

let add_nominal_poly_id cx id =
  cx.ccx.nominal_poly_ids <- Type.Poly.Set.add id cx.ccx.nominal_poly_ids

let add_type_assert cx k v = cx.ccx.type_asserts_map <- ALocMap.add k v cx.ccx.type_asserts_map

let add_matching_props cx c = cx.ccx.matching_props <- c :: cx.ccx.matching_props

let add_literal_subtypes cx c = cx.ccx.literal_subtypes <- c :: cx.ccx.literal_subtypes

let add_voidable_check cx voidable_check =
  cx.ccx.voidable_checks <- voidable_check :: cx.ccx.voidable_checks

let remove_tvar cx id = cx.ccx.sig_cx.graph <- IMap.remove id cx.ccx.sig_cx.graph

let set_all_unresolved cx all_unresolved = cx.ccx.all_unresolved <- all_unresolved

let set_envs cx envs = cx.ccx.envs <- envs

let set_evaluated cx evaluated = cx.ccx.sig_cx.evaluated <- evaluated

let set_goals cx goals = cx.ccx.goal_map <- goals

let set_graph cx graph = cx.ccx.sig_cx.graph <- graph

let set_trust_graph cx trust_graph = cx.ccx.sig_cx.trust_graph <- trust_graph

let set_property_maps cx property_maps = cx.ccx.sig_cx.property_maps <- property_maps

let set_call_props cx call_props = cx.ccx.sig_cx.call_props <- call_props

let set_export_maps cx export_maps = cx.ccx.sig_cx.export_maps <- export_maps

let set_type_graph cx type_graph = cx.ccx.type_graph <- type_graph

let set_exists_checks cx exists_checks = cx.ccx.exists_checks <- exists_checks

let set_exists_excuses cx exists_excuses = cx.ccx.exists_excuses <- exists_excuses

let set_use_def cx use_def = cx.use_def <- use_def

let set_local_env cx exported_locals = cx.exported_locals <- exported_locals

let set_module_map cx module_map = cx.ccx.sig_cx.module_map <- module_map

(* Given a sig context, it makes sense to clear the parts that are shared with
   the master sig context. Why? The master sig context, which contains global
   declarations, is an implicit dependency for every file, and so will be
   "merged in" anyway, thus making those shared parts redundant to carry around
   in other sig contexts. This saves a lot of shared memory as well as
   deserialization time. *)
let clear_master_shared cx master_cx =
  let module PMap = Type.Properties.Map in
  let module EMap = Type.Exports.Map in
  let sig_cx = cx.ccx.sig_cx in
  sig_cx.graph <- IMap.filter (fun id _ -> not (IMap.mem id master_cx.graph)) sig_cx.graph;
  sig_cx.trust_graph <-
    IMap.filter (fun id _ -> not (IMap.mem id master_cx.trust_graph)) sig_cx.trust_graph;
  sig_cx.property_maps <-
    PMap.filter (fun id _ -> not (PMap.mem id master_cx.property_maps)) sig_cx.property_maps;
  sig_cx.call_props <-
    IMap.filter (fun id _ -> not (IMap.mem id master_cx.call_props)) sig_cx.call_props;
  sig_cx.evaluated <-
    Type.Eval.Map.filter
      (fun id _ -> not (Type.Eval.Map.mem id master_cx.evaluated))
      sig_cx.evaluated;
  sig_cx.export_maps <-
    EMap.filter (fun id _ -> not (EMap.mem id master_cx.export_maps)) sig_cx.export_maps;
  ()

let test_prop_hit cx id =
  cx.ccx.test_prop_hits_and_misses <- IMap.add id Hit cx.ccx.test_prop_hits_and_misses

let test_prop_miss cx id name reasons use =
  if not (IMap.mem id cx.ccx.test_prop_hits_and_misses) then
    cx.ccx.test_prop_hits_and_misses <-
      IMap.add id (Miss (name, reasons, use)) cx.ccx.test_prop_hits_and_misses

let test_prop_get_never_hit cx =
  List.fold_left
    (fun acc (_, hit_or_miss) ->
      match hit_or_miss with
      | Hit -> acc
      | Miss (name, reasons, use_op) -> (name, reasons, use_op) :: acc)
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

(* utils *)
let find_real_props cx id =
  find_props cx id |> SMap.filter (fun x _ -> not (Reason.is_internal_name x))

let iter_props cx id f = find_props cx id |> SMap.iter f

let iter_real_props cx id f = find_real_props cx id |> SMap.iter f

let fold_real_props cx id f = find_real_props cx id |> SMap.fold f

let has_prop cx id x = find_props cx id |> SMap.mem x

let get_prop cx id x = find_props cx id |> SMap.find_opt x

let set_prop cx id x p = find_props cx id |> SMap.add x p |> add_property_map cx id

let has_export cx id name = find_exports cx id |> SMap.mem name

let set_export cx id name t = find_exports cx id |> SMap.add name t |> add_export_map cx id

(* constructors *)
let make_aloc_id cx aloc = ALoc.id_of_aloc cx.rev_aloc_table aloc

let generate_property_map cx pmap =
  let id = Reason.mk_id () in
  add_nominal_prop_id cx (id :> int);
  let id = Type.Properties.id_of_int id in
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

let generate_poly_id cx =
  let nominal = Type.Poly.generate_id () in
  add_nominal_poly_id cx nominal;
  nominal

let make_source_poly_id cx aloc = make_aloc_id cx aloc |> Type.Poly.id_of_aloc_id

(* Copy context from cx_other to cx *)
let merge_into sig_cx sig_cx_other =
  sig_cx.property_maps <- Type.Properties.Map.union sig_cx_other.property_maps sig_cx.property_maps;
  sig_cx.call_props <- IMap.union sig_cx_other.call_props sig_cx.call_props;
  sig_cx.export_maps <- Type.Exports.Map.union sig_cx_other.export_maps sig_cx.export_maps;
  sig_cx.evaluated <- Type.Eval.Map.union sig_cx_other.evaluated sig_cx.evaluated;
  sig_cx.graph <- IMap.union sig_cx_other.graph sig_cx.graph;
  sig_cx.trust_graph <- IMap.union sig_cx_other.trust_graph sig_cx.trust_graph;
  ()

(* Find the constraints of a type variable in the graph.

   Recall that type variables are either roots or goto nodes. (See
   Constraint for details.) If the type variable is a root, the
   constraints are stored with the type variable. Otherwise, the type variable
   is a goto node, and it points to another type variable: a linked list of such
   type variables must be traversed until a root is reached. *)
let rec find_graph cx id =
  let (_, constraints) = find_constraints cx id in
  constraints

and find_constraints cx id =
  let (root_id, root) = find_root cx id in
  (root_id, root.Constraint.constraints)

(* Find the root of a type variable, potentially traversing a chain of type
   variables, while short-circuiting all the type variables in the chain to the
   root during traversal to speed up future traversals. *)
and find_root cx id =
  Constraint.(
    match IMap.find_opt id (graph cx) with
    | Some (Goto next_id) ->
      let (root_id, root) = find_root cx next_id in
      if root_id != next_id then
        add_tvar cx id (Goto root_id)
      else
        ();
      (root_id, root)
    | Some (Root root) -> (id, root)
    | None ->
      let msg =
        Utils_js.spf "find_root: tvar %d not found in file %s" id (File_key.to_string @@ file cx)
      in
      Utils_js.assert_false msg)

let rec find_resolved cx = function
  | Type.OpenT (_, id) ->
    Constraint.(
      begin
        match find_graph cx id with
        | Resolved (_, t)
        | FullyResolved (_, t) ->
          Some t
        | Unresolved _ -> None
      end)
  | Type.AnnotT (_, t, _) -> find_resolved cx t
  | t -> Some t

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
      Utils_js.assert_false msg)

let find_trust_constraints cx id =
  let (root_id, root) = find_trust_root cx id in
  (root_id, Trust_constraint.get_constraints root)

let find_trust_graph cx id =
  let (_, constraints) = find_trust_constraints cx id in
  constraints

let with_normalizer_mode cx f = f { cx with phase = Normalizing }

let in_normalizer_mode cx = cx.phase = Normalizing

let constraint_cache cx = cx.ccx.constraint_cache

let subst_cache cx = cx.ccx.subst_cache

let instantiation_cache cx = cx.ccx.instantiation_cache

let repos_cache cx = cx.ccx.repos_cache

let eval_id_cache cx = cx.ccx.eval_id_cache

let eval_repos_cache cx = cx.ccx.eval_repos_cache

let fix_cache cx = cx.ccx.fix_cache

let spread_cache cx = cx.ccx.spread_cache

let speculation_state cx = cx.ccx.speculation_state
