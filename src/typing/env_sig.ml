(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Scope

module LookupMode = struct
  type t =
    | ForValue
    | ForType
    | ForTypeof
end

module type S = sig
  type scope

  type t = scope list

  val in_toplevel_scope : Context.t -> bool

  val in_global_scope : Context.t -> bool

  val var_scope_kind : Context.t -> Scope.var_scope_kind

  val in_async_scope : Context.t -> bool

  val in_predicate_scope : Context.t -> bool

  val get_global_value_type : Context.t -> Reason.name -> Reason.t -> Type.t

  val find_entry :
    Context.t -> Reason.name -> ?desc:Reason.reason_desc -> ALoc.t -> Scope.t * Entry.t

  val push_var_scope : Context.t -> Scope.t -> Scope.var_scope_kind

  val pop_var_scope : Context.t -> Scope.var_scope_kind -> unit

  val in_lex_scope : (unit -> 'a) -> 'a

  val in_class_scope : Context.t -> ALoc.t -> (unit -> 'a) -> 'a

  val env_depth : unit -> int

  val trunc_env : int -> unit

  val init_env : ?exclude_syms:NameUtils.Set.t -> Context.t -> ALoc.t -> Scope.t -> unit

  (***)

  val bind_var :
    ?state:State.t -> Context.t -> string -> Type.annotated_or_inferred -> ALoc.t -> unit

  val bind_let :
    ?state:State.t -> Context.t -> string -> Type.annotated_or_inferred -> ALoc.t -> unit

  val bind_function_this : Context.t -> Type.t -> ALoc.t -> unit

  val bind_class_instance_this : Context.t -> Type.t -> ALoc.t -> unit

  val bind_class_static_this : Context.t -> Type.t -> ALoc.t -> unit

  val bind_class_instance_super : Context.t -> Type.t -> ALoc.t -> unit

  val bind_class_static_super : Context.t -> Type.t -> ALoc.t -> unit

  val bind_implicit_let :
    ?state:State.t ->
    Entry.let_binding_kind ->
    Context.t ->
    Reason.name ->
    Type.annotated_or_inferred ->
    ALoc.t ->
    unit

  val bind_fun : ?state:State.t -> Context.t -> Reason.name -> Type.t -> ALoc.t -> unit

  val bind_implicit_const :
    ?state:State.t ->
    Entry.const_binding_kind ->
    Context.t ->
    string ->
    Type.annotated_or_inferred ->
    ALoc.t ->
    unit

  val bind_const :
    ?state:State.t -> Context.t -> string -> Type.annotated_or_inferred -> ALoc.t -> unit

  val bind_this_tparam : state:State.t -> Context.t -> Type.t -> ALoc.t -> unit

  val bind_class_self_type : Context.t -> ALoc.t -> Type.t -> Type.t -> unit

  val bind_declare_fun : Context.t -> predicate:bool -> Reason.name -> Type.t -> ALoc.t -> unit

  val init_var :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_let :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_implicit_let :
    Entry.let_binding_kind ->
    Context.t ->
    use_op:Type.use_op ->
    Reason.name ->
    has_anno:bool ->
    Type.t ->
    ALoc.t ->
    unit

  val init_fun : Context.t -> use_op:Type.use_op -> Reason.name -> Type.t -> ALoc.t -> unit

  val init_const :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_implicit_const :
    Entry.const_binding_kind ->
    Context.t ->
    use_op:Type.use_op ->
    Reason.name ->
    has_anno:bool ->
    Type.t ->
    ALoc.t ->
    unit

  val init_import : lookup_mode:LookupMode.t -> Context.t -> Reason.name -> ALoc.t -> Type.t -> unit

  val is_provider : Context.t -> ALoc.t -> bool

  val local_scope_entry_exists : Context.t -> ALoc.t -> string -> bool

  val is_global_var : Context.t -> string -> ALoc.t -> bool

  val get_class_entries : Context.t -> Type.class_binding list

  val get_var : ?lookup_mode:LookupMode.t -> Context.t -> string -> ALoc.t -> Type.t

  val get_module_exports : Context.t -> ALoc.t -> Type.t

  val get_var_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_declared_function:bool ->
    Context.t ->
    Reason.name ->
    ALoc.t ->
    Type.t

  val constraining_type : default:Type.t -> Context.t -> Reason.name -> ALoc.t -> Type.t

  val unify_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_func:bool ->
    Context.t ->
    Reason.name ->
    ALoc.t ->
    Type.t ->
    unit

  val read_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_func:bool ->
    Context.t ->
    Reason.name ->
    Reason.t ->
    ALoc.t ->
    Type.t

  val unify_declared_fun_type : Context.t -> Reason.name -> ALoc.t -> Type.t -> unit

  val var_ref :
    ?lookup_mode:LookupMode.t ->
    Context.t ->
    ?desc:Reason.reason_desc ->
    Reason.name ->
    ALoc.t ->
    Type.t

  val query_var :
    ?lookup_mode:LookupMode.t ->
    Context.t ->
    Reason.name ->
    ?desc:Reason.reason_desc ->
    ALoc.t ->
    Type.t

  val set_var : Context.t -> use_op:Type.use_op -> string -> Type.t -> ALoc.t -> unit

  val set_module_exports : Context.t -> ALoc.t -> Type.t -> unit

  val set_expr : Context.t -> Key.t -> ALoc.t -> refined:Type.t -> original:Type.t -> unit

  val get_refinement : Context.t -> Key.t -> ALoc.t -> Type.t option

  val record_expression_type_if_needed :
    Context.t -> Env_api.def_loc_type -> ALoc.t -> Type.t -> unit

  val discriminant_after_negated_cases :
    Context.t ->
    ALoc.t ->
    (Reason.name * Key.proj list) option ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    Type.t option

  val valid_declaration_check : Context.t -> Reason.name -> ALoc.t -> unit

  val get_next : Context.t -> ALoc.t -> Type.t

  val init_class_self_type : Context.t -> ALoc.t -> Reason.reason -> Type.t

  val init_declare_module_synthetic_module_exports :
    Context.t ->
    export_type:(Context.t -> Reason.name -> ALoc.t option -> Type.t -> unit) ->
    ALoc.t ->
    Reason.reason ->
    Scope.t ->
    unit

  val init_builtins_from_libdef : Context.t -> Scope.t -> Reason.name list
end
