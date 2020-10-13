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

exception Tvar_not_found of Constraint.ident

type env = Scope.t list

(* The Context module defines types for data which is passed around during type
 * checking, providing access to commonly needed state. The data is layered
 * according to their lifetimes and falls into three categories: *)

(* 1. Per-file information is needed during the AST traversal, to answer
 * questions like "what options are enabled" where options can be set on a
 * perf-file bases, like the ability to munge underscores. *)
type t

(* 2. Per-component information is needed during constraint solving, which
 * happens outside the context of any specific file -- specifically when dealing
 * with constraints between files in a cycle. *)
type component_t

(* 3. Inter-component information, i.e., stuff that we might want to know about
 * dependencies, like what modules they export and what types correspond to what
 * resolved tvars. *)
type sig_t

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
  enable_this_annot: bool;
  enforce_strict_call_arity: bool;
  exact_by_default: bool;
  generate_tests: bool;
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

type phase =
  | Checking
  | Merging
  | Normalizing

type type_assert_kind =
  | Is
  | Throws
  | Wraps

type voidable_check = {
  public_property_map: Type.Properties.id;
  private_property_map: Type.Properties.id;
  errors: ALoc.t Property_assignment.errors;
}

type computed_property_state =
  | ResolvedOnce of Reason.t
  | ResolvedMultipleTimes

type subst_cache_err =
  | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
  | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

val make_sig : unit -> sig_t

val make_ccx : sig_t -> ALoc.table Lazy.t Utils_js.FilenameMap.t -> component_t

val make :
  component_t -> metadata -> File_key.t -> ALoc.reverse_table Lazy.t -> string -> phase -> t

val metadata_of_options : Options.t -> metadata

val docblock_overrides : Docblock.t -> metadata -> metadata

val trust_constructor : t -> unit -> Trust.trust_rep

val cx_with_trust : t -> (unit -> Trust.trust_rep) -> t

val sig_cx : t -> sig_t

val graph_sig : sig_t -> Constraint.node IMap.t

val find_module_sig : sig_t -> string -> Type.t

(* accessors *)
val current_phase : t -> phase

val all_unresolved : t -> ISet.t IMap.t

val metadata : t -> metadata

val max_literal_length : t -> int

val babel_loose_array_spread : t -> bool

val enable_const_params : t -> bool

val enable_enums : t -> bool

val enable_enums_with_unknown_members : t -> bool

val enable_this_annot : t -> bool

val enforce_strict_call_arity : t -> bool

val envs : t -> env IMap.t

val errors : t -> Flow_error.ErrorSet.t

val error_suppressions : t -> Error_suppressions.t

val evaluated : t -> Type.t Type.Eval.Map.t

val goals : t -> Type.t IMap.t

val exact_by_default : t -> bool

val generate_tests : t -> bool

val file : t -> File_key.t

val aloc_tables : t -> ALoc.table Lazy.t Utils_js.FilenameMap.t

val find_props : t -> Type.Properties.id -> Type.Properties.t

val find_real_props : t -> Type.Properties.id -> Type.Properties.t

val find_call : t -> int -> Type.t

val find_exports : t -> Type.Exports.id -> Type.Exports.t

val find_require : t -> ALoc.t -> Type.t

val find_module : t -> string -> Type.t

val find_tvar : t -> Constraint.ident -> Constraint.node

val mem_nominal_prop_id : t -> Constraint.ident -> bool

val mem_nominal_poly_id : t -> Type.Poly.id -> bool

val graph : t -> Constraint.node IMap.t

val trust_graph : t -> Trust_constraint.node IMap.t

val is_checked : t -> bool

val is_verbose : t -> bool

val is_weak : t -> bool

val is_strict : t -> bool

val is_strict_local : t -> bool

val include_suppressions : t -> bool

val severity_cover : t -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t

val max_trace_depth : t -> int

val module_kind : t -> Module_info.kind

val require_map : t -> Type.t ALocMap.t

val module_map : t -> Type.t SMap.t

val exported_locals : t -> ALocSet.t SMap.t option

val module_ref : t -> string

val property_maps : t -> Type.Properties.map

val call_props : t -> Type.t IMap.t

val export_maps : t -> Type.Exports.map

val react_runtime : t -> Options.react_runtime

val recursion_limit : t -> int

val root : t -> Path.t

val facebook_fbs : t -> string option

val facebook_fbt : t -> string option

val facebook_module_interop : t -> bool

val haste_module_ref_prefix : t -> string option

val should_ignore_non_literal_requires : t -> bool

val should_munge_underscores : t -> bool

val should_strip_root : t -> bool

val suppress_types : t -> SSet.t

val default_lib_dir : t -> Path.t option

val trust_mode : t -> Options.trust_mode

val trust_tracking : t -> bool

val trust_errors : t -> bool

val type_asserts : t -> bool

val type_graph : t -> Graph_explorer.graph

val type_asserts_map : t -> (type_assert_kind * ALoc.t) ALocMap.t

val matching_props : t -> (Reason.reason * string * Type.t * Type.t) list

val literal_subtypes : t -> (Type.t * Type.use_t) list

val verbose : t -> Verbose.t option

val max_workers : t -> int

val jsx : t -> Options.jsx_mode

val exists_checks : t -> ExistsCheck.t ALocMap.t

val exists_excuses : t -> ExistsCheck.t ALocMap.t

val voidable_checks : t -> voidable_check list

val use_def : t -> Scope_api.With_ALoc.info * Ssa_api.With_ALoc.values

val pid_prefix : t -> string

val copy_of_context : t -> t

val merge_into : sig_t -> sig_t -> unit

val automatic_require_default : t -> bool

(* modules *)
val push_declare_module : t -> Module_info.t -> unit

val pop_declare_module : t -> unit

val module_info : t -> Module_info.t

(* mutators *)
val add_env : t -> int -> env -> unit

val add_error : t -> ALoc.t Flow_error.t -> unit

val add_error_suppression : t -> Loc.t -> Suppression_comments.applicable_codes -> unit

val add_severity_cover : t -> File_key.t -> ExactCover.lint_severity_cover -> unit

val add_lint_suppressions : t -> LocSet.t -> unit

val add_require : t -> ALoc.t -> Type.t -> unit

val add_module : t -> string -> Type.t -> unit

val add_property_map : t -> Type.Properties.id -> Type.Properties.t -> unit

val add_call_prop : t -> int -> Type.t -> unit

val add_export_map : t -> Type.Exports.id -> Type.Exports.t -> unit

val add_tvar : t -> Constraint.ident -> Constraint.node -> unit

val add_trust_var : t -> Trust_constraint.ident -> Trust_constraint.node -> unit

val add_type_assert : t -> ALoc.t -> type_assert_kind * ALoc.t -> unit

val add_matching_props : t -> Reason.reason * string * Type.t * Type.t -> unit

val add_literal_subtypes : t -> Type.t * Type.use_t -> unit

val add_voidable_check : t -> voidable_check -> unit

val remove_tvar : t -> Constraint.ident -> unit

val set_envs : t -> env IMap.t -> unit

val set_evaluated : t -> Type.t Type.Eval.Map.t -> unit

val set_goals : t -> Type.t IMap.t -> unit

val set_type_graph : t -> Graph_explorer.graph -> unit

val set_all_unresolved : t -> ISet.t IMap.t -> unit

val set_graph : t -> Constraint.node IMap.t -> unit

val set_trust_graph : t -> Trust_constraint.node IMap.t -> unit

val set_property_maps : t -> Type.Properties.map -> unit

val set_call_props : t -> Type.t IMap.t -> unit

val set_export_maps : t -> Type.Exports.map -> unit

val set_exists_checks : t -> ExistsCheck.t ALocMap.t -> unit

val set_exists_excuses : t -> ExistsCheck.t ALocMap.t -> unit

val set_use_def : t -> Scope_api.With_ALoc.info * Ssa_api.With_ALoc.values -> unit

val set_module_map : t -> Type.t SMap.t -> unit

val set_local_env : t -> ALocSet.t SMap.t option -> unit

val clear_master_shared : t -> sig_t -> unit

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
val test_prop_hit : t -> Constraint.ident -> unit

val test_prop_miss :
  t -> Constraint.ident -> string option -> Reason.t * Reason.t -> Type.use_op -> unit

val test_prop_get_never_hit : t -> (string option * (Reason.t * Reason.t) * Type.use_op) list

val computed_property_state_for_id : t -> Constraint.ident -> computed_property_state option

val computed_property_add_lower_bound : t -> Constraint.ident -> Reason.t -> unit

val computed_property_add_multiple_lower_bounds : t -> Constraint.ident -> unit

val spread_widened_types_get_widest : t -> Constraint.ident -> Type.Object.slice option

val spread_widened_types_add_widest : t -> Constraint.ident -> Type.Object.slice -> unit

val mark_optional_chain : t -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_optional_chains : t -> (ALoc.t * Reason.t) list

val mark_invariant : t -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_invariants : t -> (ALoc.t * Reason.t) list

(* utils *)
val iter_props : t -> Type.Properties.id -> (string -> Type.Property.t -> unit) -> unit

val iter_real_props : t -> Type.Properties.id -> (string -> Type.Property.t -> unit) -> unit

val fold_real_props : t -> Type.Properties.id -> (string -> Type.Property.t -> 'a -> 'a) -> 'a -> 'a

val has_prop : t -> Type.Properties.id -> string -> bool

val get_prop : t -> Type.Properties.id -> string -> Type.Property.t option

val set_prop : t -> Type.Properties.id -> string -> Type.Property.t -> unit

val has_export : t -> Type.Exports.id -> string -> bool

val set_export : t -> Type.Exports.id -> string -> ALoc.t option * Type.t -> unit

(* constructors *)
val make_aloc_id : t -> ALoc.t -> ALoc.id

val make_generic_id : t -> string -> ALoc.t -> Generic.id

val generate_property_map : t -> Type.Properties.t -> Type.Properties.id

val make_source_property_map : t -> Type.Properties.t -> ALoc.t -> Type.Properties.id

val make_call_prop : t -> Type.t -> int

val make_export_map : t -> Type.Exports.t -> Type.Exports.id

val generate_poly_id : t -> Type.Poly.id

val make_source_poly_id : t -> ALoc.t -> Type.Poly.id

val find_constraints : t -> Constraint.ident -> Constraint.ident * Constraint.constraints

val find_graph : t -> Constraint.ident -> Constraint.constraints

val find_root : t -> Constraint.ident -> Constraint.ident * Constraint.root

val find_resolved : t -> Type.t -> Type.t option

val find_trust_constraints :
  t -> Trust_constraint.ident -> Trust_constraint.ident * Trust_constraint.constraints

val find_trust_graph : t -> Trust_constraint.ident -> Trust_constraint.constraints

val find_trust_root : t -> Trust_constraint.ident -> Trust_constraint.ident * Trust_constraint.root

val with_normalizer_mode : t -> (t -> 'a) -> 'a

val in_normalizer_mode : t -> bool

val constraint_cache : t -> Type.FlowSet.t ref

val subst_cache : t -> (Type.Poly.id * Type.t list, subst_cache_err list * Type.t) Hashtbl.t

val instantiation_cache : t -> (Reason.t * Reason.t * Reason.t Nel.t, Type.t) Hashtbl.t

val repos_cache : t -> Repos_cache.t ref

val eval_id_cache :
  t -> (Type.Eval.id, Type.t) Hashtbl.t * (Type.t * Type.defer_use_t, Type.Eval.id) Hashtbl.t

val eval_repos_cache : t -> (Type.t * Type.defer_use_t * Type.Eval.id, Type.t) Hashtbl.t

val fix_cache : t -> (Reason.t * Type.t, Type.t) Hashtbl.t

val spread_cache : t -> Spread_cache.t

val speculation_state : t -> Speculation_state.t
