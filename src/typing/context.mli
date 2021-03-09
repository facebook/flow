(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

exception Props_not_found of Type.Properties.id

exception Exports_not_found of Type.Exports.id

exception Require_not_found of string

exception Module_not_found of string

exception Tvar_not_found of Type.ident

(* The Context module defines types for data which is passed around during type
 * checking, providing access to commonly needed state. The data is layered
 * according to their lifetimes and falls into three categories: *)

(* 1. Per-file information is needed during the AST traversal, to answer
 * questions like "what options are enabled" where options can be set on a
 * perf-file bases, like the ability to munge underscores. *)
type 'phase t_

type t = Type.Constraint.infer_phase t_

(* 2. Per-component information is needed during constraint solving, which
 * happens outside the context of any specific file -- specifically when dealing
 * with constraints between files in a cycle. *)
type 'phase component_t_

type component_t

(* 3. Inter-component information, i.e., stuff that we might want to know about
 * dependencies, like what modules they export and what types correspond to what
 * resolved tvars. *)
type 'phase sig_t_ = 'phase Type.TypeContext.t

type sig_t = Type.Constraint.infer_phase sig_t_

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
  enable_enums_with_unknown_members: bool;
  enable_indexed_access: bool;
  enable_this_annot: bool;
  enforce_strict_call_arity: bool;
  enforce_local_inference_annotations: bool;
  exact_by_default: bool;
  facebook_fbs: string option;
  facebook_fbt: string option;
  facebook_module_interop: bool;
  haste_module_ref_prefix: string option;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  react_runtime: Options.react_runtime;
  react_server_component_exts: SSet.t;
  recursion_limit: int;
  root: Path.t;
  run_post_inference_implicit_instantiation: bool;
  strict_es6_import_export: bool;
  strict_es6_import_export_excludes: string list;
  strip_root: bool;
  suppress_types: SSet.t;
  max_workers: int;
  default_lib_dir: Path.t option;
  trust_mode: Options.trust_mode;
  type_asserts: bool;
}

type phase =
  | InitLib
  | Checking
  | Merging

val string_of_phase : phase -> string

type type_assert_kind =
  | Is
  | Throws
  | Wraps

type voidable_check = {
  public_property_map: Type.Properties.id;
  private_property_map: Type.Properties.id;
  errors: ALoc.t Property_assignment.errors;
}

type implicit_instantiation_check = {
  fun_or_class: Type.t;
  call_or_constructor: Type.use_t;
}

type computed_property_state =
  | ResolvedOnce of Reason.t
  | ResolvedMultipleTimes

type subst_cache_err =
  | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
  | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

val make_ccx : unit -> 'phase component_t_

val make :
  'phase component_t_ ->
  metadata ->
  File_key.t ->
  ALoc.table Lazy.t ->
  Reason.name ->
  phase ->
  'phase t_

val metadata_of_options : Options.t -> metadata

val docblock_overrides : Docblock.t -> metadata -> metadata

val trust_constructor : 'phase t_ -> unit -> Trust.trust_rep

val cx_with_trust : 'phase t_ -> (unit -> Trust.trust_rep) -> 'phase t_

val sig_cx : 'phase t_ -> 'phase sig_t_

val graph_sig : 'phase sig_t_ -> 'phase Type.Constraint.node IMap.t

val find_module_sig : 'phase sig_t_ -> string -> Type.t

(* accessors *)
val current_phase : 'phase t_ -> phase

val all_unresolved : 'phase t_ -> ISet.t IMap.t

val metadata : 'phase t_ -> metadata

val max_literal_length : 'phase t_ -> int

val babel_loose_array_spread : 'phase t_ -> bool

val enable_const_params : 'phase t_ -> bool

val enable_enums : 'phase t_ -> bool

val enable_enums_with_unknown_members : 'phase t_ -> bool

val enable_indexed_access : 'phase t_ -> bool

val enable_this_annot : 'phase t_ -> bool

val enforce_strict_call_arity : 'phase t_ -> bool

val errors : 'phase t_ -> Flow_error.ErrorSet.t

val error_suppressions : 'phase t_ -> Error_suppressions.t

val evaluated : 'phase t_ -> Type.t Type.Eval.Map.t

val goals : 'phase t_ -> Type.t IMap.t

val exact_by_default : 'phase t_ -> bool

val enforce_local_inference_annotations : 'phase t_ -> bool

val run_post_inference_implicit_instantiation : 'phase t_ -> bool

val file : 'phase t_ -> File_key.t

val aloc_tables : 'phase t_ -> ALoc.table Lazy.t Utils_js.FilenameMap.t

val find_props : 'phase t_ -> Type.Properties.id -> Type.Properties.t

val find_real_props : 'phase t_ -> Type.Properties.id -> Type.Properties.t

val find_call : 'phase t_ -> int -> Type.t

val find_exports : 'phase t_ -> Type.Exports.id -> Type.Exports.t

val find_require : 'phase t_ -> ALoc.t -> Type.t

val find_module : 'phase t_ -> string -> Type.t

val find_tvar : 'phase t_ -> Type.ident -> 'phase Type.Constraint.node

val mem_nominal_prop_id : 'phase t_ -> Type.ident -> bool

val mem_nominal_poly_id : 'phase t_ -> Type.Poly.id -> bool

val graph : 'phase t_ -> 'phase Type.Constraint.node IMap.t

val trust_graph : 'phase t_ -> Trust_constraint.node IMap.t

val is_checked : 'phase t_ -> bool

val is_verbose : 'phase t_ -> bool

val is_weak : 'phase t_ -> bool

val is_strict : 'phase t_ -> bool

val is_strict_local : 'phase t_ -> bool

val include_suppressions : 'phase t_ -> bool

val severity_cover : 'phase t_ -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t

val max_trace_depth : 'phase t_ -> int

val module_kind : 'phase t_ -> Module_info.kind

val require_map : 'phase t_ -> Type.t ALocMap.t

val module_map : 'phase t_ -> Type.t NameUtils.Map.t

val exported_locals : 'phase t_ -> ALocSet.t SMap.t option

val module_ref : 'phase t_ -> Reason.name

val property_maps : 'phase t_ -> Type.Properties.map

val call_props : 'phase t_ -> Type.t IMap.t

val export_maps : 'phase t_ -> Type.Exports.map

val react_runtime : 'phase t_ -> Options.react_runtime

val in_react_server_component_file : 'phase t_ -> bool

val recursion_limit : 'phase t_ -> int

val root : 'phase t_ -> Path.t

val facebook_fbs : 'phase t_ -> string option

val facebook_fbt : 'phase t_ -> string option

val facebook_module_interop : 'phase t_ -> bool

val haste_module_ref_prefix : 'phase t_ -> string option

val should_ignore_non_literal_requires : 'phase t_ -> bool

val should_munge_underscores : 'phase t_ -> bool

val should_strip_root : 'phase t_ -> bool

val suppress_types : 'phase t_ -> SSet.t

val default_lib_dir : 'phase t_ -> Path.t option

val trust_mode : 'phase t_ -> Options.trust_mode

val trust_tracking : 'phase t_ -> bool

val trust_errors : 'phase t_ -> bool

val type_asserts : 'phase t_ -> bool

val type_graph : 'phase t_ -> Graph_explorer.graph

val type_asserts_map : 'phase t_ -> (type_assert_kind * ALoc.t) ALocMap.t

val matching_props : 'phase t_ -> (Reason.reason * string * Type.t * Type.t) list

val literal_subtypes : 'phase t_ -> (Type.t * Type.use_t) list

val verbose : 'phase t_ -> Verbose.t option

val max_workers : 'phase t_ -> int

val jsx : 'phase t_ -> Options.jsx_mode

val exists_checks : 'phase t_ -> ExistsCheck.t ALocMap.t

val exists_excuses : 'phase t_ -> ExistsCheck.t ALocMap.t

val voidable_checks : 'phase t_ -> voidable_check list

val implicit_instantiation_checks : 'phase t_ -> implicit_instantiation_check list

val use_def : 'phase t_ -> Scope_api.With_ALoc.info * Ssa_api.With_ALoc.values

val pid_prefix : 'phase t_ -> string

val copy_of_context : 'phase t_ -> 'phase t_

val merge_into : 'phase component_t_ -> 'phase sig_t_ -> unit

val automatic_require_default : 'phase t_ -> bool

(* modules *)
val push_declare_module : 'phase t_ -> Module_info.t -> unit

val pop_declare_module : 'phase t_ -> unit

val module_info : 'phase t_ -> Module_info.t

(* mutators *)
val add_error : 'phase t_ -> ALoc.t Flow_error.t -> unit

val add_error_suppression : 'phase t_ -> Loc.t -> Suppression_comments.applicable_codes -> unit

val add_severity_cover : 'phase t_ -> File_key.t -> ExactCover.lint_severity_cover -> unit

val add_lint_suppressions : 'phase t_ -> LocSet.t -> unit

val add_require : 'phase t_ -> ALoc.t -> Type.t -> unit

val add_module : 'phase t_ -> Reason.name -> Type.t -> unit

val add_property_map : 'phase t_ -> Type.Properties.id -> Type.Properties.t -> unit

val add_call_prop : 'phase t_ -> int -> Type.t -> unit

val add_export_map : 'phase t_ -> Type.Exports.id -> Type.Exports.t -> unit

val add_tvar : 'phase t_ -> Type.ident -> 'phase Type.Constraint.node -> unit

val add_trust_var : 'phase t_ -> Trust_constraint.ident -> Trust_constraint.node -> unit

val add_type_assert : 'phase t_ -> ALoc.t -> type_assert_kind * ALoc.t -> unit

val add_matching_props : 'phase t_ -> Reason.reason * string * Type.t * Type.t -> unit

val add_literal_subtypes : 'phase t_ -> Type.t * Type.use_t -> unit

val add_voidable_check : 'phase t_ -> voidable_check -> unit

val add_implicit_instantiation_check : 'phase t_ -> Type.t -> Type.use_t -> unit

val remove_tvar : 'phase t_ -> Type.ident -> unit

val set_evaluated : 'phase t_ -> Type.t Type.Eval.Map.t -> unit

val set_goals : 'phase t_ -> Type.t IMap.t -> unit

val set_type_graph : 'phase t_ -> Graph_explorer.graph -> unit

val set_all_unresolved : 'phase t_ -> ISet.t IMap.t -> unit

val set_graph : 'phase t_ -> 'phase Type.Constraint.node IMap.t -> unit

val set_trust_graph : 'phase t_ -> Trust_constraint.node IMap.t -> unit

val set_property_maps : 'phase t_ -> Type.Properties.map -> unit

val set_call_props : 'phase t_ -> Type.t IMap.t -> unit

val set_export_maps : 'phase t_ -> Type.Exports.map -> unit

val set_exists_checks : 'phase t_ -> ExistsCheck.t ALocMap.t -> unit

val set_exists_excuses : 'phase t_ -> ExistsCheck.t ALocMap.t -> unit

val set_use_def : 'phase t_ -> Scope_api.With_ALoc.info * Ssa_api.With_ALoc.values -> unit

val set_module_map : 'phase t_ -> Type.t NameUtils.Map.t -> unit

val set_local_env : 'phase t_ -> ALocSet.t SMap.t option -> unit

val clear_master_shared : 'phase t_ -> 'phase sig_t_ -> unit

(* Flow allows you test test if a property exists inside a conditional. However, we only wan to
 * allow this test if there's a chance that the property might exist. So `if (foo.bar)` should be
 *
 * - Allowed for the types `{ bar: string }`, `any`, `mixed`, `{ bar: string } | number`, etc
 *
 * - Disallowed for the types ` { baz: string }`, `number`, ` { baz: string} | number`
 *
 * It's really difficult to say that something never happens in Flow. Our best way of approximating
 * this is waiting until typechecking is done and then seeing if something happened. In this case,
 * we record if testing a property ever succeeds. If if never succeeds after typechecking is done,
 * we emit an error.
 *)
val test_prop_hit : 'phase t_ -> Type.ident -> unit

val test_prop_miss :
  'phase t_ -> Type.ident -> Reason.name option -> Reason.t * Reason.t -> Type.use_op -> unit

val test_prop_get_never_hit :
  'phase t_ -> (Reason.name option * (Reason.t * Reason.t) * Type.use_op) list

val computed_property_state_for_id : 'phase t_ -> Type.ident -> computed_property_state option

val computed_property_add_lower_bound : 'phase t_ -> Type.ident -> Reason.t -> unit

val computed_property_add_multiple_lower_bounds : 'phase t_ -> Type.ident -> unit

val spread_widened_types_get_widest : 'phase t_ -> Type.ident -> Type.Object.slice option

val spread_widened_types_add_widest : 'phase t_ -> Type.ident -> Type.Object.slice -> unit

val mark_optional_chain : 'phase t_ -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_optional_chains : 'phase t_ -> (ALoc.t * Reason.t) list

val mark_invariant : 'phase t_ -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_invariants : 'phase t_ -> (ALoc.t * Reason.t) list

(* utils *)
val iter_props : 'phase t_ -> Type.Properties.id -> (Reason.name -> Type.Property.t -> unit) -> unit

val iter_real_props :
  'phase t_ -> Type.Properties.id -> (Reason.name -> Type.Property.t -> unit) -> unit

val fold_real_props :
  'phase t_ -> Type.Properties.id -> (Reason.name -> Type.Property.t -> 'a -> 'a) -> 'a -> 'a

val has_prop : 'phase t_ -> Type.Properties.id -> Reason.name -> bool

val get_prop : 'phase t_ -> Type.Properties.id -> Reason.name -> Type.Property.t option

val set_prop : 'phase t_ -> Type.Properties.id -> Reason.name -> Type.Property.t -> unit

val has_export : 'phase t_ -> Type.Exports.id -> Reason.name -> bool

val set_export : 'phase t_ -> Type.Exports.id -> Reason.name -> ALoc.t option * Type.t -> unit

(* constructors *)
val make_aloc_id : 'phase t_ -> ALoc.t -> ALoc.id

val make_generic_id : 'phase t_ -> string -> ALoc.t -> Generic.id

val generate_property_map : 'phase t_ -> Type.Properties.t -> Type.Properties.id

val make_source_property_map : 'phase t_ -> Type.Properties.t -> ALoc.t -> Type.Properties.id

val make_call_prop : 'phase t_ -> Type.t -> int

val make_export_map : 'phase t_ -> Type.Exports.t -> Type.Exports.id

val generate_poly_id : 'phase t_ -> Type.Poly.id

val make_source_poly_id : 'phase t_ -> ALoc.t -> Type.Poly.id

val find_constraints : 'phase t_ -> Type.ident -> Type.ident * 'phase Type.Constraint.constraints

val find_graph : 'phase t_ -> Type.ident -> 'phase Type.Constraint.constraints

val find_root : 'phase t_ -> Type.ident -> Type.ident * 'phase Type.Constraint.root

val find_resolved : 'phase t_ -> Type.t -> Type.t option

val find_trust_constraints :
  'phase t_ -> Trust_constraint.ident -> Trust_constraint.ident * Trust_constraint.constraints

val find_trust_graph : 'phase t_ -> Trust_constraint.ident -> Trust_constraint.constraints

val find_trust_root :
  'phase t_ -> Trust_constraint.ident -> Trust_constraint.ident * Trust_constraint.root

val constraint_cache : 'phase t_ -> Type.FlowSet.t ref

val subst_cache : 'phase t_ -> (Type.Poly.id * Type.t list, subst_cache_err list * Type.t) Hashtbl.t

val instantiation_cache : 'phase t_ -> (Reason.t * Reason.t * Reason.t Nel.t, Type.t) Hashtbl.t

val repos_cache : 'phase t_ -> Repos_cache.t ref

val eval_id_cache :
  'phase t_ ->
  (Type.Eval.id, Type.t) Hashtbl.t * (Type.t * Type.defer_use_t, Type.Eval.id) Hashtbl.t

val eval_repos_cache : 'phase t_ -> (Type.t * Type.defer_use_t * Type.Eval.id, Type.t) Hashtbl.t

val fix_cache : 'phase t_ -> (Reason.t * Type.t, Type.t) Hashtbl.t

val spread_cache : 'phase t_ -> Spread_cache.t

val speculation_state : 'phase t_ -> Speculation_state.t

val speculation_id : 'phase t_ -> (int * int) option
