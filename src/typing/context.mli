(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

exception Props_not_found of Type.Properties.id

exception Exports_not_found of Type.Exports.id

exception Require_not_found of string

exception Module_not_found of string

module TypeAppExpansion : sig
  (* Array types function like type applications but are not implemented as such. Unless
     we decide to unify their implementation with regular typeapps, they need special
     handling here *)
  type root =
    | Type of Type.t
    | Array of Reason.t
    | ROArray of Reason.t
    | Tuple of Reason.t * int (* arity *)

  module RootSet : Flow_set.S with type elt = root

  type entry = Type.t * RootSet.t list
end

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
type sig_t = Type.TypeContext.t

type master_context =
  | EmptyMasterContext
  | NonEmptyMasterContext of {
      builtin_leader_file_key: File_key.t;
      builtin_locs: Loc.t Type_sig_collections.Locs.t;
      builtins: Type_sig_collections.Locs.index Packed_type_sig.Builtins.t;
    }

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

type typing_mode =
  | CheckingMode
  | SynthesisMode
  | HintEvaluationMode

type resolved_require = (Type.t, Reason.name) result

type resolve_require = string -> resolved_require

type voidable_check = {
  public_property_map: Type.Properties.id;
  private_property_map: Type.Properties.id;
  errors: ALoc.t Property_assignment.errors;
}

type subst_cache_err =
  | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
  | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

val make_ccx : unit -> component_t

val make :
  component_t ->
  metadata ->
  File_key.t ->
  ALoc.table Lazy.t ->
  resolve_require ->
  (t -> Builtins.t) ->
  t

val metadata_of_options : Options.t -> metadata

val docblock_overrides : Docblock.t -> File_key.t -> metadata -> metadata

val sig_cx : t -> sig_t

(* accessors *)
val all_unresolved : t -> ISet.t IMap.t

val metadata : t -> metadata

val max_literal_length : t -> int

val babel_loose_array_spread : t -> bool

val builtins : t -> Builtins.t

val casting_syntax : t -> Options.CastingSyntax.t

val component_syntax : t -> bool

val hooklike_functions : t -> bool

val react_rule_enabled : t -> Options.react_rules -> bool

val react_rules_always : t -> bool

val enable_as_const : t -> bool

val enable_const_params : t -> bool

val enable_enums : t -> bool

val enable_relay_integration : t -> bool

val relay_integration_esmodules : t -> bool

val relay_integration_module_prefix : t -> string option

val errors : t -> Flow_error.ErrorSet.t

val error_suppressions : t -> Error_suppressions.t

val evaluated : t -> Type.t Type.Eval.Map.t

val goals : t -> Type.t IMap.t

val exact_by_default : t -> bool

val file : t -> File_key.t

val aloc_tables : t -> ALoc.table Lazy.t Utils_js.FilenameMap.t

val find_props : t -> Type.Properties.id -> Type.Properties.t

val find_real_props : t -> Type.Properties.id -> Type.Properties.t

val find_call : t -> int -> Type.t

val find_exports : t -> Type.Exports.id -> Type.Exports.t

val find_require : t -> string -> resolved_require

val find_tvar : t -> Type.ident -> Type.Constraint.node

val graph : t -> Type.Constraint.graph

val in_implicit_instantiation : t -> bool

val is_checked : t -> bool

val is_verbose : t -> bool

val is_strict : t -> bool

val is_strict_local : t -> bool

val available_platforms : t -> Platform_set.t option

val has_explicit_supports_platform : t -> bool

val include_suppressions : t -> bool

val severity_cover : t -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t

val max_trace_depth : t -> int

val property_maps : t -> Type.Properties.map

val call_props : t -> Type.t IMap.t

val export_maps : t -> Type.Exports.map

val react_runtime : t -> Options.react_runtime

val recursion_limit : t -> int

val root : t -> File_path.t

val facebook_fbs : t -> string option

val facebook_fbt : t -> string option

val facebook_module_interop : t -> bool

val should_ignore_non_literal_requires : t -> bool

val should_munge_underscores : t -> bool

val should_strip_root : t -> bool

val suppress_types : t -> SSet.t

val ts_syntax : t -> bool

val type_graph : t -> Graph_explorer.graph

val matching_props : t -> (string * ALoc.t * ALoc.t) list

val literal_subtypes : t -> (ALoc.t * Env_api.literal_check) list

val post_inference_polarity_checks :
  t -> (Type.typeparam Subst_name.Map.t * Polarity.t * Type.t) list

val post_inference_validation_flows : t -> (Type.t * Type.use_t) list

val renders_type_argument_validations :
  t -> (ALoc.t * Flow_ast.Type.Renders.variant * bool * Type.t) list

val missing_local_annot_lower_bounds : t -> Type.t Nel.t ALocFuzzyMap.t

val namespaces : t -> bool

val verbose : t -> Verbose.t option

val slow_to_check_logging : t -> Slow_to_check_logging.t

val max_workers : t -> int

val missing_module_generators : t -> (Str.regexp * string) list

val jsx : t -> Options.jsx_mode

val exists_checks : t -> Type.TypeSet.t ALocMap.t

val exists_excuses : t -> ExistsCheck.t ALocMap.t

val voidable_checks : t -> voidable_check list

val reachable_deps : t -> Utils_js.FilenameSet.t

val environment : t -> Loc_env.t

val typing_mode : t -> typing_mode

val node_cache : t -> Node_cache.t

val pid_prefix : t -> string

val copy_of_context : t -> t

val any_propagation : t -> bool

val automatic_require_default : t -> bool

(* modules *)
val in_declare_module : t -> bool

val in_declare_namespace : t -> bool

(* mutators *)
val add_exhaustive_check : t -> ALoc.t -> ALoc.t list * bool -> unit

val add_error : t -> ALoc.t Flow_error.t -> unit

val reset_errors : t -> Flow_error.ErrorSet.t -> unit

val add_error_suppressions : t -> Error_suppressions.t -> unit

val add_severity_covers : t -> ExactCover.lint_severity_cover Utils_js.FilenameMap.t -> unit

val add_property_map : t -> Type.Properties.id -> Type.Properties.t -> unit

val add_call_prop : t -> int -> Type.t -> unit

val add_export_map : t -> Type.Exports.id -> Type.Exports.t -> unit

val add_tvar : t -> Type.ident -> Type.Constraint.node -> unit

val mk_placeholder : t -> Reason.t -> Type.t

val add_matching_props : t -> string * ALoc.t * ALoc.t -> unit

val add_literal_subtypes : t -> ALoc.t * Env_api.literal_check -> unit

val add_post_inference_polarity_check :
  t -> Type.typeparam Subst_name.Map.t -> Polarity.t -> Type.t -> unit

val add_post_inference_validation_flow : t -> Type.t -> Type.use_t -> unit

val add_post_inference_subtyping_check : t -> Type.t -> Type.use_op -> Type.t -> unit

val add_renders_type_argument_validation :
  t -> allow_generic_t:bool -> ALoc.t -> Flow_ast.Type.Renders.variant -> Type.t -> unit

val add_missing_local_annot_lower_bound : t -> ALoc.t -> Type.t -> unit

val add_voidable_check : t -> voidable_check -> unit

val add_monomorphized_component : t -> Type.Properties.id -> Type.t -> unit

val add_reachable_dep : t -> File_key.t -> unit

val set_evaluated : t -> Type.t Type.Eval.Map.t -> unit

val set_goals : t -> Type.t IMap.t -> unit

val set_type_graph : t -> Graph_explorer.graph -> unit

val set_all_unresolved : t -> ISet.t IMap.t -> unit

val set_graph : t -> Type.Constraint.graph -> unit

val run_in_implicit_instantiation_mode : t -> (unit -> 'a) -> 'a

val set_property_maps : t -> Type.Properties.map -> unit

val set_call_props : t -> Type.t IMap.t -> unit

val set_export_maps : t -> Type.Exports.map -> unit

val set_exists_checks : t -> Type.TypeSet.t ALocMap.t -> unit

val add_exists_check : t -> ALoc.t -> Type.t -> unit

val set_exists_excuses : t -> ExistsCheck.t ALocMap.t -> unit

val set_environment : t -> Loc_env.t -> unit

val set_signature_help_callee : t -> ALoc.t -> Type.t -> unit

val set_union_opt : t -> ALoc.t -> Type.t -> unit

val run_and_rolled_back_cache : t -> (unit -> 'a) -> 'a

val run_in_synthesis_mode : t -> (unit -> 'a) -> bool * 'a

val run_in_hint_eval_mode : t -> (unit -> 'a) -> 'a

val add_env_cache_entry :
  t -> for_value:bool -> int -> (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result -> unit

val add_hint_eval_cache_entry : t -> int -> Type.t option -> unit

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
val test_prop_hit : t -> Type.ident -> unit

val test_prop_miss :
  t ->
  Type.ident ->
  Reason.name option ->
  Reason.t * Reason.t ->
  Type.use_op ->
  string option ->
  unit

val test_prop_get_never_hit :
  t -> (Reason.name option * (Reason.t * Reason.t) * Type.use_op * string option) list

val with_allowed_method_unbinding : t -> ALoc.t -> (unit -> 'a) -> 'a

val allowed_method_unbinding : t -> ALoc.t -> bool

val mark_optional_chain : t -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_optional_chains : t -> (ALoc.t * Reason.t) list

val mark_invariant : t -> ALoc.t -> Reason.t -> useful:bool -> unit

val unnecessary_invariants : t -> (ALoc.t * Reason.t) list

val mark_maybe_unused_promise : t -> ALoc.t -> Type.t -> async:bool -> unit

val maybe_unused_promises : t -> (ALoc.t * Type.t * bool) list

(* utils *)
val iter_props : t -> Type.Properties.id -> (Reason.name -> Type.Property.t -> unit) -> unit

val iter_real_props : t -> Type.Properties.id -> (Reason.name -> Type.Property.t -> unit) -> unit

val fold_real_props :
  t -> Type.Properties.id -> (Reason.name -> Type.Property.t -> 'a -> 'a) -> 'a -> 'a

val has_prop : t -> Type.Properties.id -> Reason.name -> bool

val get_prop : t -> Type.Properties.id -> Reason.name -> Type.Property.t option

(* constructors *)
val make_aloc_id : t -> ALoc.t -> ALoc.id

val make_generic_id : t -> Subst_name.t -> ALoc.t -> Generic.id

val generate_property_map : t -> Type.Properties.t -> Type.Properties.id

val make_source_property_map :
  t -> Type.Properties.t -> type_sig:bool -> ALoc.t -> Type.Properties.id

val make_call_prop : t -> Type.t -> int

val make_export_map : t -> Type.Exports.t -> Type.Exports.id

val make_source_poly_id : t -> type_sig:bool -> ALoc.t -> Type.Poly.id

val find_constraints : t -> Type.ident -> Type.ident * Type.Constraint.constraints

val find_graph : t -> Type.ident -> Type.Constraint.constraints

val find_root : t -> Type.ident -> Type.ident * Type.Constraint.node * Type.Constraint.root

val find_root_id : t -> Type.ident -> Type.ident

val find_resolved : t -> Type.t -> Type.t option

val env_cache_find_opt :
  t -> for_value:bool -> int -> (Type.t, Type.t * Env_api.cacheable_env_error Nel.t) result option

val hint_eval_cache_find_opt : t -> int -> Type.t option option

val hint_map_arglist_cache : t -> (ALoc.t * Type.call_arg) list ALocMap.t ref

val hint_map_jsx_cache :
  t ->
  ( Reason.t * string * ALoc.t list * ALoc.t,
    Type.t Lazy.t * (Type.t list * Type.t option) Lazy.t
  )
  Hashtbl.t

val constraint_cache : t -> Type.FlowSet.t ref

val subst_cache : t -> (subst_cache_err list * Type.t) Type.SubstCacheMap.t ref

val repos_cache : t -> Repos_cache.t ref

val eval_id_cache : t -> Type.t Type.EvalIdCacheMap.t ref * Type.Eval.id Type.IdCacheMap.t ref

val eval_repos_cache : t -> Type.t Type.EvalReposCacheMap.t ref

val fix_cache : t -> Type.t Type.FixCacheMap.t ref

val spread_cache : t -> Spread_cache.t ref

val instantiation_stack : t -> TypeAppExpansion.entry list ref

val const_fold_cache : t -> int Type.ConstFoldMap.t IMap.t ref

val exhaustive_check : t -> ALoc.t -> ALoc.t list * bool

val speculation_state : t -> Speculation_state.t

val speculation_id : t -> (int * int) option

val add_avar : t -> int -> Type.AConstraint.t -> unit

val find_avar : t -> int -> Type.AConstraint.t

val find_avar_opt : t -> int -> Type.AConstraint.t option

val find_monomorphized_component : t -> Type.Properties.id -> Type.t option

val get_signature_help_callee : t -> ALoc.t -> Type.t option

val iter_union_opt : t -> f:(ALocMap.key -> Type.t -> unit) -> unit

val remove_avar : t -> int -> unit

val iter_annot_dependent_set : t -> (int -> Type.AConstraint.op -> unit) -> ISet.t -> unit

type cache_snapshot

val take_cache_snapshot : t -> cache_snapshot

val restore_cache_snapshot : t -> cache_snapshot -> unit

val use_mixed_in_catch_variables : t -> bool

val new_specialized_callee : t -> Type.specialized_callee
