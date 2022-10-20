(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* lookup modes:

   - ForValue is a lookup from a syntactic value location, i.e. standard
     JS code

   - ForType is a lookup from a syntactic type location, e.g. annotations,
     interface declarations etc.

   - ForTypeof is a lookup from a typeof expression (necessarily in a type
     location)

   Rules:

   1. ForValue lookups give errors if they retrieve type aliases (note: we
      have a single namespace, so any name resolves uniquely to either a
      value or type)

   2. ForValue lookups give errors if they forward reference non-hoisted
      things (lets or consts)

   3. ForType lookups may return values or type aliases, since some values
      also denote types - e.g. a generator function F also denotes the type
      of the objects it creates. Of course many values don't also have a type
      denotation and thus errors in type position. But we don't know the type
      of a symbol during local inference as a rule, so errors of this kind are
      not raised here.

   4. ForTypeof lookups are in fact ForValue lookups, but due to the order in
      which AST traversal takes place, these lookups may legitimately violate
      rule #2, hence the need for a special mode.
*)
module LookupMode : sig
  type t =
    | ForValue
    | ForType
    | ForTypeof
end

val in_toplevel_scope : Context.t -> bool

val in_global_scope : Context.t -> bool

val var_scope_kind : Context.t -> Name_def.scope_kind

val in_async_scope : Context.t -> bool

val in_predicate_scope : Context.t -> bool

(** Set the current scope kind and return the previous scope kind. *)
val set_scope_kind : Context.t -> Name_def.scope_kind -> Name_def.scope_kind

val in_class_scope : Context.t -> ALoc.t -> (unit -> 'a) -> 'a

val init_env : Context.t -> Name_def.scope_kind -> unit

(***)

val bind_function_param : Context.t -> Type.t -> ALoc.t -> unit

val bind_function_this : Context.t -> Type.t -> ALoc.t -> unit

val bind_class_instance_this : Context.t -> Type.t -> ALoc.t -> unit

val bind_class_static_this : Context.t -> Type.t -> ALoc.t -> unit

val bind_class_instance_super : Context.t -> Type.t -> ALoc.t -> unit

val bind_class_static_super : Context.t -> Type.t -> ALoc.t -> unit

val bind_fun : Context.t -> Reason.name -> Type.t -> ALoc.t -> unit

val bind_this_tparam : Type.t -> ALoc.t -> unit

val bind_class_self_type : Context.t -> ALoc.t -> Type.t -> unit

val init_var : Context.t -> use_op:Type.use_op -> Type.t -> ALoc.t -> unit

val init_let : Context.t -> use_op:Type.use_op -> Type.t -> ALoc.t -> unit

val init_implicit_let : Context.t -> use_op:Type.use_op -> Type.t -> ALoc.t -> unit

val init_const : Context.t -> use_op:Type.use_op -> Type.t -> ALoc.t -> unit

val init_implicit_const : Context.t -> use_op:Type.use_op -> Type.t -> ALoc.t -> unit

val is_provider : Context.t -> ALoc.t -> bool

val local_scope_entry_exists : Context.t -> ALoc.t -> bool

val is_global_var : Context.t -> ALoc.t -> bool

val get_class_entries : Context.t -> Type.class_binding list

val has_hint : Context.t -> ALoc.t -> bool

val get_hint : Context.t -> ALoc.t -> Type.lazy_hint_t

val get_var : ?lookup_mode:LookupMode.t -> Context.t -> string -> ALoc.t -> Type.t

val get_module_exports : Context.t -> ALoc.t -> Type.t

val get_var_declared_type :
  ?lookup_mode:LookupMode.t ->
  ?is_declared_function:bool ->
  Context.t ->
  Reason.name ->
  ALoc.t ->
  Type.t

val constraining_type : default:Type.t -> Context.t -> ALoc.t -> Type.t

val read_declared_type : Context.t -> Reason.t -> ALoc.t -> Type.t

val var_ref :
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  ?desc:Reason.reason_desc ->
  Reason.name ->
  ALoc.t ->
  Type.t

val read_class_self_type : Context.t -> ALoc.t -> Type.t

val find_write : Context.t -> Env_api.With_ALoc.def_loc_type -> Reason.reason -> Type.t

val query_var :
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  Reason.name ->
  ?desc:Reason.reason_desc ->
  ALoc.t ->
  Type.t

val predicate_refinement_maps :
  Context.t -> ALoc.t -> Type.predicate Key_map.t * Type.predicate Key_map.t

val set_var : Context.t -> use_op:Type.use_op -> string -> Type.t -> ALoc.t -> unit

val set_module_exports : Context.t -> Type.t -> unit

val get_refinement : Context.t -> Key.t -> ALoc.t -> Type.t option

val discriminant_after_negated_cases :
  Context.t -> ALoc.t -> (Reason.name * Key.proj list) option -> Type.t option

val get_next : Context.t -> ALoc.t -> Type.t

val init_declare_module_synthetic_module_exports :
  Context.t ->
  export_type:(Context.t -> Reason.name -> ALoc.t option -> Type.t -> unit) ->
  ALoc.t ->
  Reason.reason ->
  unit

val init_builtins_from_libdef : Context.t -> Reason.name list

val check_readable : Context.t -> Env_api.def_loc_type -> ALoc.t -> unit

val resolve_env_entry :
  use_op:Type.use_op ->
  update_reason:bool ->
  Context.t ->
  Type.t ->
  Env_api.def_loc_type ->
  ALoc.t ->
  unit

val unify_write_entry :
  Context.t -> use_op:Type.use_op -> Type.t -> Env_api.def_loc_type -> ALoc.t -> unit

val provider_type_for_def_loc :
  ?intersect:bool -> Context.t -> Loc_env.t -> Env_api.Provider_api.L.t -> Type.t

val ref_entry_exn :
  lookup_mode:LookupMode.t -> Context.t -> Env_api.With_ALoc.L.t -> Reason.reason -> Type.t

val t_option_value_exn : Context.t -> ALoc.t -> Type.t option -> Type.t
