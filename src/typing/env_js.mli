(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Reason_js
open Scope

val peek_scope: unit -> Scope.t

val peek_env: unit -> Scope.t list

val clone_env: Scope.t list -> Scope.t list

val string_of_env: Context.t -> Scope.t list -> string

val in_async_scope: unit -> bool
val in_generator_scope: unit -> bool

val all_entries: unit -> Entry.t SMap.t

val peek_frame: unit -> int

val push_var_scope: Context.t -> Scope.t -> unit
val pop_var_scope: unit -> unit

val retrieve_closure_changeset: unit -> Changeset.t

val in_lex_scope: Context.t -> (unit -> 'a) -> 'a

val env_depth: unit -> int
val trunc_env: int -> unit

val init_env:
  ?exclude_syms:Utils.SSet.t ->
  Context.t ->
  Scope.t ->
  unit

val update_env: Context.t -> reason -> Scope.t list -> unit

(***)

val bind_var: ?state:State.t -> Context.t -> string -> Type.t ->
  reason -> unit

val bind_let: ?state:State.t -> Context.t -> string -> Type.t ->
  reason -> unit

val bind_implicit_let: ?state:State.t -> Entry.implicit_let_kinds ->
  Context.t -> string -> Type.t -> reason -> unit

val bind_fun: ?state:State.t -> Context.t -> string -> Type.t ->
  reason -> unit

val bind_const: ?state:State.t -> Context.t -> string -> Type.t ->
  reason -> unit

val bind_type: ?state:State.t -> Context.t -> string -> Type.t ->
  reason -> unit

val bind_declare_var: Context.t -> string -> Type.t -> reason -> unit
val bind_declare_fun: Context.t -> string -> Type.t -> reason -> unit

val declare_const: Context.t -> string -> reason -> unit
val declare_let: Context.t -> string -> reason -> unit

val declare_implicit_let: Entry.implicit_let_kinds -> Context.t -> string ->
  reason -> unit

val init_var: Context.t -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_let: Context.t -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_implicit_let:
  Entry.implicit_let_kinds
    -> Context.t
    -> string
    -> has_anno:bool
    -> Type.t
    -> reason
    -> unit
val init_fun: Context.t -> string -> Type.t -> reason -> unit
val init_const: Context.t -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_type: Context.t -> string -> Type.t -> reason -> unit

val pseudo_init_declared_type: Context.t -> string -> reason -> unit

module LookupMode: sig
  type t = ForValue | ForType | ForTypeof
end

val local_scope_entry_exists: Context.t -> string -> bool

val get_var:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  reason ->
  Type.t

val get_var_declared_type:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  reason ->
  Type.t

val var_ref:
  ?lookup_mode:LookupMode.t ->
  Context.t ->
  string ->
  reason ->
  Type.t

val set_var: Context.t -> string -> Type.t -> reason -> unit

val set_expr: Context.t -> Key.t -> reason -> Type.t -> Type.t -> unit

val refine_with_preds:
  Context.t ->
  reason ->
  Type.predicate KeyMap.t ->
  Type.t KeyMap.t ->
  unit

val in_refined_env:
  Context.t ->
  reason ->
  Type.predicate KeyMap.t ->
  Type.t KeyMap.t ->
  (unit -> 'a) ->
  'a

val merge_env:
  Context.t ->
  reason ->
  Scope.t list * Scope.t list * Scope.t list ->
  Changeset.t ->
  unit

val widen_env: Context.t -> reason -> unit

val copy_env:
  Context.t ->
  reason ->
  Scope.t list * Scope.t list ->
  Changeset.t ->
  unit

val havoc_all: unit -> unit

val havoc_current_activation: reason -> unit

val havoc_vars: Changeset.t -> unit

val havoc_heap_refinements: unit -> unit
val havoc_heap_refinements_with_propname: string -> unit

val get_refinement: Context.t -> Key.t -> reason -> Type.t option
