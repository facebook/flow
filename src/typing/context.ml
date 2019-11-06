(**
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
  haste_module_ref_prefix: string option;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  recursion_limit: int;
  root: Path.t;
  strip_root: bool;
  suppress_comments: Str.regexp list;
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
  mutable evaluated: Type.t IMap.t;
  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;
  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: ISet.t IMap.t;
  (* map from frame ids to env snapshots *)
  mutable envs: env IMap.t;
  (* map from module names to their types *)
  mutable module_map: Type.t SMap.t;
  (* We track nominal ids in the context to help decide when the types exported by a module have
     meaningfully changed: see Merge_js.ContextOptimizer. **)
  mutable nominal_ids: ISet.t;
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
}

type phase =
  | Checking
  | Merging

type t = {
  sig_cx: sig_t;
  file: File_key.t;
  phase: phase;
  (* Tables for the current component (cycle) *)
  aloc_tables: ALoc.table Lazy.t Utils_js.FilenameMap.t;
  (* Reverse lookup table for the current file. Unlike the aloc_tables, we only
     store the rev_table for the leader file, rather than the whole component.
     We only need this table during the check phase, when we are checking single
     files, so storing the rev table for the whole component would be a waste
     of space/ *)
  rev_aloc_table: ALoc.reverse_table Lazy.t;
  metadata: metadata;
  module_info: Module_info.t;
  mutable import_stmts: (ALoc.t, ALoc.t) Flow_ast.Statement.ImportDeclaration.t list;
  mutable imported_ts: Type.t SMap.t;
  mutable require_map: Type.t ALocMap.t;
  trust_constructor: unit -> Trust.trust_rep;
  mutable declare_module_ref: Module_info.t option;
  mutable use_def: Scope_api.info * Ssa_api.With_ALoc.values;
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
    haste_module_ref_prefix = Options.haste_module_ref_prefix options;
    ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
    max_trace_depth = Options.max_trace_depth options;
    max_workers = Options.max_workers options;
    recursion_limit = Options.recursion_limit options;
    root = Options.root options;
    strip_root = Options.should_strip_root options;
    suppress_comments = Options.suppress_comments options;
    suppress_types = Options.suppress_types options;
    default_lib_dir = (Options.file_options options).Files.default_lib_dir;
    trust_mode = Options.trust_mode options;
    type_asserts = Options.type_asserts options;
  }

let empty_use_def = (Scope_api.{ max_distinct = 0; scopes = IMap.empty }, ALocMap.empty)

let make_sig () =
  {
    graph = IMap.empty;
    trust_graph = IMap.empty;
    property_maps = Type.Properties.Map.empty;
    call_props = IMap.empty;
    export_maps = Type.Exports.Map.empty;
    evaluated = IMap.empty;
    type_graph = Graph_explorer.new_graph ();
    all_unresolved = IMap.empty;
    envs = IMap.empty;
    module_map = SMap.empty;
    nominal_ids = ISet.empty;
    type_asserts_map = ALocMap.empty;
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
  }

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let make sig_cx metadata file aloc_tables rev_aloc_table module_ref phase =
  {
    sig_cx;
    file;
    phase;
    aloc_tables;
    rev_aloc_table;
    metadata;
    module_info = Module_info.empty_cjs_module module_ref;
    import_stmts = [];
    imported_ts = SMap.empty;
    require_map = ALocMap.empty;
    trust_constructor = Trust.literal_trust;
    declare_module_ref = None;
    use_def = empty_use_def;
  }

let sig_cx cx = cx.sig_cx

let graph_sig sig_cx = sig_cx.graph

let trust_graph_sig sig_cx = sig_cx.trust_graph

let find_module_sig sig_cx m =
  (try SMap.find_unsafe m sig_cx.module_map with Not_found -> raise (Module_not_found m))

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

let all_unresolved cx = cx.sig_cx.all_unresolved

let envs cx = cx.sig_cx.envs

let trust_constructor cx = cx.trust_constructor

let cx_with_trust cx trust = { cx with trust_constructor = trust }

let metadata cx = cx.metadata

let max_literal_length cx = cx.metadata.max_literal_length

let enable_const_params cx =
  cx.metadata.enable_const_params || cx.metadata.strict || cx.metadata.strict_local

let enable_enums cx = cx.metadata.enable_enums

let enforce_strict_call_arity cx = cx.metadata.enforce_strict_call_arity

let errors cx = cx.sig_cx.errors

let error_suppressions cx = cx.sig_cx.error_suppressions

let esproposal_class_static_fields cx = cx.metadata.esproposal_class_static_fields

let esproposal_class_instance_fields cx = cx.metadata.esproposal_class_instance_fields

let esproposal_decorators cx = cx.metadata.esproposal_decorators

let esproposal_export_star_as cx = cx.metadata.esproposal_export_star_as

let esproposal_optional_chaining cx = cx.metadata.esproposal_optional_chaining

let esproposal_nullish_coalescing cx = cx.metadata.esproposal_nullish_coalescing

let evaluated cx = cx.sig_cx.evaluated

let exact_by_default cx = cx.metadata.exact_by_default

let file cx = cx.file

let aloc_tables cx = cx.aloc_tables

let find_props cx id =
  try Type.Properties.Map.find_unsafe id cx.sig_cx.property_maps
  with Not_found -> raise (Props_not_found id)

let find_call cx id =
  (try IMap.find_unsafe id cx.sig_cx.call_props with Not_found -> raise (Call_not_found id))

let find_exports cx id =
  try Type.Exports.Map.find_unsafe id cx.sig_cx.export_maps
  with Not_found -> raise (Exports_not_found id)

let find_require cx loc =
  try ALocMap.find_unsafe loc cx.require_map
  with Not_found -> raise (Require_not_found (ALoc.debug_to_string ~include_source:true loc))

let find_module cx m = find_module_sig (sig_cx cx) m

let find_tvar cx id =
  (try IMap.find_unsafe id cx.sig_cx.graph with Not_found -> raise (Tvar_not_found id))

let mem_nominal_id cx id = ISet.mem id cx.sig_cx.nominal_ids

let graph cx = graph_sig cx.sig_cx

let trust_graph cx = trust_graph_sig cx.sig_cx

let import_stmts cx = cx.import_stmts

let imported_ts cx = cx.imported_ts

let is_checked cx = cx.metadata.checked

let is_verbose cx = cx.metadata.verbose <> None

let is_weak cx = cx.metadata.weak

let is_strict cx = Option.is_some cx.declare_module_ref || cx.metadata.strict

let is_strict_local cx = cx.metadata.strict_local

let include_suppressions cx = cx.metadata.include_suppressions

let severity_cover cx = cx.sig_cx.severity_cover

let max_trace_depth cx = cx.metadata.max_trace_depth

let require_map cx = cx.require_map

let module_map cx = cx.sig_cx.module_map

let property_maps cx = cx.sig_cx.property_maps

let call_props cx = cx.sig_cx.call_props

let export_maps cx = cx.sig_cx.export_maps

let recursion_limit cx = cx.metadata.recursion_limit

let root cx = cx.metadata.root

let facebook_fbs cx = cx.metadata.facebook_fbs

let facebook_fbt cx = cx.metadata.facebook_fbt

let haste_module_ref_prefix cx = cx.metadata.haste_module_ref_prefix

let should_ignore_non_literal_requires cx = cx.metadata.ignore_non_literal_requires

let should_munge_underscores cx = cx.metadata.munge_underscores

let should_strip_root cx = cx.metadata.strip_root

let suppress_comments cx = cx.metadata.suppress_comments

let suppress_types cx = cx.metadata.suppress_types

let default_lib_dir cx = cx.metadata.default_lib_dir

let type_asserts_map cx = cx.sig_cx.type_asserts_map

let type_graph cx = cx.sig_cx.type_graph

let trust_mode cx = cx.metadata.trust_mode

let type_asserts cx = cx.metadata.type_asserts

let verbose cx = cx.metadata.verbose

let max_workers cx = cx.metadata.max_workers

let jsx cx = cx.metadata.jsx

let exists_checks cx = cx.sig_cx.exists_checks

let exists_excuses cx = cx.sig_cx.exists_excuses

let voidable_checks cx = cx.sig_cx.voidable_checks

let use_def cx = cx.use_def

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
    sig_cx = { cx.sig_cx with graph = cx.sig_cx.graph; trust_graph = cx.sig_cx.trust_graph };
  }

(* mutators *)
let add_env cx frame env = cx.sig_cx.envs <- IMap.add frame env cx.sig_cx.envs

let add_error cx error = cx.sig_cx.errors <- Flow_error.ErrorSet.add error cx.sig_cx.errors

let add_error_suppression cx loc =
  cx.sig_cx.error_suppressions <- Error_suppressions.add loc cx.sig_cx.error_suppressions

let add_severity_cover cx filekey severity_cover =
  cx.sig_cx.severity_cover <-
    Utils_js.FilenameMap.add filekey severity_cover cx.sig_cx.severity_cover

let add_lint_suppressions cx suppressions =
  cx.sig_cx.error_suppressions <-
    Error_suppressions.add_lint_suppressions suppressions cx.sig_cx.error_suppressions

let add_import_stmt cx stmt = cx.import_stmts <- stmt :: cx.import_stmts

let add_imported_t cx name t = cx.imported_ts <- SMap.add name t cx.imported_ts

let add_require cx loc tvar = cx.require_map <- ALocMap.add loc tvar cx.require_map

let add_module cx name tvar = cx.sig_cx.module_map <- SMap.add name tvar cx.sig_cx.module_map

let add_property_map cx id pmap =
  cx.sig_cx.property_maps <- Type.Properties.Map.add id pmap cx.sig_cx.property_maps

let add_call_prop cx id t = cx.sig_cx.call_props <- IMap.add id t cx.sig_cx.call_props

let add_export_map cx id tmap =
  cx.sig_cx.export_maps <- Type.Exports.Map.add id tmap cx.sig_cx.export_maps

let add_tvar cx id bounds = cx.sig_cx.graph <- IMap.add id bounds cx.sig_cx.graph

let add_trust_var cx id bounds = cx.sig_cx.trust_graph <- IMap.add id bounds cx.sig_cx.trust_graph

let add_nominal_id cx id = cx.sig_cx.nominal_ids <- ISet.add id cx.sig_cx.nominal_ids

let add_type_assert cx k v =
  cx.sig_cx.type_asserts_map <- ALocMap.add k v cx.sig_cx.type_asserts_map

let add_voidable_check cx voidable_check =
  cx.sig_cx.voidable_checks <- voidable_check :: cx.sig_cx.voidable_checks

let remove_all_errors cx = cx.sig_cx.errors <- Flow_error.ErrorSet.empty

let remove_all_error_suppressions cx = cx.sig_cx.error_suppressions <- Error_suppressions.empty

let remove_all_lint_severities cx = cx.sig_cx.severity_cover <- Utils_js.FilenameMap.empty

let remove_tvar cx id = cx.sig_cx.graph <- IMap.remove id cx.sig_cx.graph

let set_all_unresolved cx all_unresolved = cx.sig_cx.all_unresolved <- all_unresolved

let set_envs cx envs = cx.sig_cx.envs <- envs

let set_evaluated cx evaluated = cx.sig_cx.evaluated <- evaluated

let set_graph cx graph = cx.sig_cx.graph <- graph

let set_trust_graph cx trust_graph = cx.sig_cx.trust_graph <- trust_graph

let set_property_maps cx property_maps = cx.sig_cx.property_maps <- property_maps

let set_call_props cx call_props = cx.sig_cx.call_props <- call_props

let set_export_maps cx export_maps = cx.sig_cx.export_maps <- export_maps

let set_type_graph cx type_graph = cx.sig_cx.type_graph <- type_graph

let set_exists_checks cx exists_checks = cx.sig_cx.exists_checks <- exists_checks

let set_exists_excuses cx exists_excuses = cx.sig_cx.exists_excuses <- exists_excuses

let set_use_def cx use_def = cx.use_def <- use_def

let set_module_map cx module_map = cx.sig_cx.module_map <- module_map

let clear_intermediates cx =
  cx.sig_cx.envs <- IMap.empty;
  cx.sig_cx.all_unresolved <- IMap.empty;
  cx.sig_cx.nominal_ids <- ISet.empty;

  (* still 176 bytes :/ *)
  cx.sig_cx.type_graph <- Graph_explorer.Tbl.create 0;
  cx.sig_cx.exists_checks <- ALocMap.empty;
  cx.sig_cx.exists_excuses <- ALocMap.empty;
  cx.sig_cx.voidable_checks <- [];
  cx.sig_cx.test_prop_hits_and_misses <- IMap.empty;
  cx.sig_cx.computed_property_states <- IMap.empty;
  cx.sig_cx.optional_chains_useful <- ALocMap.empty;
  cx.sig_cx.invariants_useful <- ALocMap.empty;
  cx.sig_cx.type_asserts_map <- ALocMap.empty;
  ()

(* Given a sig context, it makes sense to clear the parts that are shared with
   the master sig context. Why? The master sig context, which contains global
   declarations, is an implicit dependency for every file, and so will be
   "merged in" anyway, thus making those shared parts redundant to carry around
   in other sig contexts. This saves a lot of shared memory as well as
   deserialization time. *)
let clear_master_shared cx master_cx =
  let module PMap = Type.Properties.Map in
  let module EMap = Type.Exports.Map in
  cx.sig_cx.graph <- IMap.filter (fun id _ -> not (IMap.mem id master_cx.graph)) cx.sig_cx.graph;
  cx.sig_cx.trust_graph <-
    IMap.filter (fun id _ -> not (IMap.mem id master_cx.trust_graph)) cx.sig_cx.trust_graph;
  cx.sig_cx.property_maps <-
    PMap.filter (fun id _ -> not (PMap.mem id master_cx.property_maps)) cx.sig_cx.property_maps;
  cx.sig_cx.call_props <-
    IMap.filter (fun id _ -> not (IMap.mem id master_cx.call_props)) cx.sig_cx.call_props;
  cx.sig_cx.evaluated <-
    IMap.filter (fun id _ -> not (IMap.mem id master_cx.evaluated)) cx.sig_cx.evaluated;
  cx.sig_cx.export_maps <-
    EMap.filter (fun id _ -> not (EMap.mem id master_cx.export_maps)) cx.sig_cx.export_maps;
  ()

let test_prop_hit cx id =
  cx.sig_cx.test_prop_hits_and_misses <- IMap.add id Hit cx.sig_cx.test_prop_hits_and_misses

let test_prop_miss cx id name reasons use =
  if not (IMap.mem id cx.sig_cx.test_prop_hits_and_misses) then
    cx.sig_cx.test_prop_hits_and_misses <-
      IMap.add id (Miss (name, reasons, use)) cx.sig_cx.test_prop_hits_and_misses

let test_prop_get_never_hit cx =
  List.fold_left
    (fun acc (_, hit_or_miss) ->
      match hit_or_miss with
      | Hit -> acc
      | Miss (name, reasons, use_op) -> (name, reasons, use_op) :: acc)
    []
    (IMap.bindings cx.sig_cx.test_prop_hits_and_misses)

let computed_property_state_for_id cx id = IMap.find_opt id cx.sig_cx.computed_property_states

let computed_property_add_lower_bound cx id r =
  cx.sig_cx.computed_property_states <-
    IMap.add id (ResolvedOnce r) cx.sig_cx.computed_property_states

let computed_property_add_multiple_lower_bounds cx id =
  cx.sig_cx.computed_property_states <-
    IMap.add id ResolvedMultipleTimes cx.sig_cx.computed_property_states

let spread_widened_types_get_widest cx id = IMap.find_opt id cx.sig_cx.spread_widened_types

let spread_widened_types_add_widest cx id objtype =
  cx.sig_cx.spread_widened_types <- IMap.add id objtype cx.sig_cx.spread_widened_types

let mark_optional_chain cx loc lhs_reason ~useful =
  cx.sig_cx.optional_chains_useful <-
    ALocMap.add
      loc
      (lhs_reason, useful)
      ~combine:(fun (r, u) (_, u') -> (r, u || u'))
      cx.sig_cx.optional_chains_useful

let unnecessary_optional_chains cx =
  ALocMap.fold
    (fun loc (r, useful) acc ->
      if useful then
        acc
      else
        (loc, r) :: acc)
    cx.sig_cx.optional_chains_useful
    []

let mark_invariant cx loc reason ~useful =
  cx.sig_cx.invariants_useful <-
    ALocMap.add
      loc
      (reason, useful)
      ~combine:(fun (r, u) (_, u') -> (r, u || u'))
      cx.sig_cx.invariants_useful

let unnecessary_invariants cx =
  ALocMap.fold
    (fun loc (r, useful) acc ->
      if useful then
        acc
      else
        (loc, r) :: acc)
    cx.sig_cx.invariants_useful
    []

(* utils *)
let find_real_props cx id =
  find_props cx id |> SMap.filter (fun x _ -> not (Reason.is_internal_name x))

let iter_props cx id f = find_props cx id |> SMap.iter f

let iter_real_props cx id f = find_real_props cx id |> SMap.iter f

let fold_real_props cx id f = find_real_props cx id |> SMap.fold f

let has_prop cx id x = find_props cx id |> SMap.mem x

let get_prop cx id x = find_props cx id |> SMap.get x

let set_prop cx id x p = find_props cx id |> SMap.add x p |> add_property_map cx id

let has_export cx id name = find_exports cx id |> SMap.mem name

let set_export cx id name t = find_exports cx id |> SMap.add name t |> add_export_map cx id

(* constructors *)
let generate_property_map cx pmap =
  let id = Reason.mk_id () in
  add_nominal_id cx (id :> int);
  let id = Type.Properties.id_of_int id in
  add_property_map cx id pmap;
  id

let make_source_property_map cx pmap loc =
  (* To prevent cases where we might compare a concrete and an abstract
     aloc (like in a cycle) we abstractify all incoming alocs before adding
     them to the map. The only exception is for library files, which have only
     concrete definitions and by definition cannot appear in cycles. *)
  let loc = ALoc.lookup_key_if_possible cx.rev_aloc_table loc in
  let id = Type.Properties.id_of_aloc loc in
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

let make_nominal cx =
  let nominal = Reason.mk_id () in
  add_nominal_id cx nominal;
  nominal

(* Copy context from cx_other to cx *)
let merge_into sig_cx sig_cx_other =
  sig_cx.property_maps <- Type.Properties.Map.union sig_cx_other.property_maps sig_cx.property_maps;
  sig_cx.call_props <- IMap.union sig_cx_other.call_props sig_cx.call_props;
  sig_cx.export_maps <- Type.Exports.Map.union sig_cx_other.export_maps sig_cx.export_maps;
  sig_cx.evaluated <- IMap.union sig_cx_other.evaluated sig_cx.evaluated;
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
    match IMap.get id (graph cx) with
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
    match IMap.get id (trust_graph cx) with
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
