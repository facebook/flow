(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Scope

type t = Scope.t list

val peek_scope: unit -> Scope.t

val peek_env: unit -> t

val clone_env: t -> t

val string_of_env: Context.t -> t -> string

val var_scope_kind: unit -> Scope.var_scope_kind

val in_async_scope: unit -> bool
val in_generator_scope: unit -> bool
val in_predicate_scope: unit -> bool

val all_entries: unit -> Entry.t SMap.t

val peek_frame: unit -> int

val push_var_scope: Context.t -> Scope.t -> unit
val pop_var_scope: unit -> unit

val retrieve_closure_changeset: unit -> Changeset.t

val in_lex_scope: Context.t -> (unit -> 'a) -> 'a

val env_depth: unit -> int
val trunc_env: int -> unit

val init_env:
  ?exclude_syms:SSet.t ->
  Context.t ->
  Scope.t ->
  unit

val update_env: Context.t -> Loc.t -> t -> unit

(***)

val promote_to_const_like: Context.t -> Loc.t -> bool

val bind_class: Context.t -> int -> Type.Properties.id -> Type.Properties.id -> unit

val bind_var: ?state:State.t -> Context.t -> string -> Type.t ->
  Loc.t -> unit

val bind_let: ?state:State.t -> Context.t -> string -> Type.t ->
  Loc.t -> unit

val bind_implicit_let: ?state:State.t -> Entry.let_binding_kind ->
  Context.t -> string -> Type.t -> Loc.t -> unit

val bind_fun: ?state:State.t -> Context.t -> string -> Type.t ->
  Loc.t -> unit

val bind_implicit_const: ?state:State.t -> Entry.const_binding_kind ->
  Context.t -> string -> Type.t -> Loc.t -> unit

val bind_const: ?state:State.t -> Context.t -> string -> Type.t ->
  Loc.t -> unit

val bind_import: Context.t -> string -> Type.t -> Loc.t -> unit

val bind_type: ?state:State.t -> Context.t -> string -> Type.t ->
  Loc.t -> unit

val bind_import_type: Context.t -> string -> Type.t -> Loc.t -> unit

val bind_declare_var: Context.t -> string -> Type.t -> Loc.t -> unit
val bind_declare_fun: Context.t -> string -> Type.t -> Loc.t -> unit

val declare_const: Context.t -> string -> Loc.t -> unit
val declare_let: Context.t -> string -> Loc.t -> unit

val declare_implicit_let: Entry.let_binding_kind -> Context.t -> string ->
  Loc.t -> unit

val init_var: Context.t -> use_op:Type.use_op -> string -> has_anno:bool -> Type.t -> Loc.t -> unit
val init_let: Context.t -> use_op:Type.use_op -> string -> has_anno:bool -> Type.t -> Loc.t -> unit
val init_implicit_let:
  Entry.let_binding_kind
    -> Context.t
    -> use_op:Type.use_op
    -> string
    -> has_anno:bool
    -> Type.t
    -> Loc.t
    -> unit
val init_fun: Context.t -> use_op:Type.use_op -> string -> Type.t -> Loc.t -> unit
val init_const: Context.t -> use_op:Type.use_op -> string -> has_anno:bool -> Type.t -> Loc.t -> unit
val init_type: Context.t -> string -> Type.t -> Loc.t -> unit

val pseudo_init_declared_type: Context.t -> string -> Loc.t -> unit

module LookupMode: sig
  type t = ForValue | ForType | ForTypeof
end

val local_scope_entry_exists: string -> bool

val get_env_entry: string -> t -> Scope.Entry.t option
val get_current_env_entry: string -> Scope.Entry.t option
val get_env_refi: Key.t -> t -> Scope.refi_binding option
val get_current_env_refi: Key.t -> Scope.refi_binding option
val get_class_entries: unit -> Type.class_binding list

val get_var:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  Loc.t ->
  Type.t

val get_internal_var:
  Context.t ->
  string ->
  Loc.t ->
  Type.t

val get_var_declared_type:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  Loc.t ->
  Type.t

val unify_declared_type:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  Type.t ->
  unit

val var_ref:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  ?desc:Reason.reason_desc ->
  Loc.t ->
  Type.t

val set_var: Context.t -> use_op:Type.use_op -> string -> Type.t -> Loc.t ->
  Changeset.EntryRef.t option

val set_internal_var: Context.t -> string -> Type.t -> Loc.t ->
  Changeset.EntryRef.t option

val set_expr: Key.t -> Loc.t -> Type.t -> Type.t ->
  Changeset.RefiRef.t

val refine_with_preds:
  Context.t ->
  Loc.t ->
  Type.predicate Key_map.t ->
  Type.t Key_map.t ->
  Changeset.t

val in_refined_env:
  Context.t ->
  Loc.t ->
  Type.predicate Key_map.t ->
  Type.t Key_map.t ->
  (unit -> 'a) ->
  'a

val merge_env:
  Context.t ->
  Loc.t ->
  t * t * t ->
  Changeset.t ->
  unit

val widen_env: Context.t -> Loc.t -> unit

val copy_env:
  Context.t ->
  Loc.t ->
  t * t ->
  Changeset.t ->
  unit

val havoc_all: unit -> unit

val reset_current_activation: Loc.t -> unit

val havoc_vars: Changeset.t -> unit

val havoc_heap_refinements: unit -> unit
val havoc_heap_refinements_with_propname: private_:bool -> string -> unit

val get_refinement: Context.t -> Key.t -> Loc.t -> Type.t option

val is_global_var: Context.t -> string -> bool
