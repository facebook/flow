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
open Constraint_js
open Scope (* from Constraint_js *)

val peek_scope: unit -> Scope.t

val get_scopes: unit -> Scope.t list

val clone_scopes: Scope.t list -> Scope.t list

val in_async_scope: unit -> bool
val in_generator_scope: unit -> bool

val all_entries: unit -> Entry.t SMap.t

type changeset = SSet.t * KeySet.t

val peek_changeset: unit -> changeset

val clear_changeset: unit -> changeset

val merge_changeset: changeset -> changeset

val peek_frame: unit -> ident

val clear_env: Reason_js.reason -> unit

val push_env: context -> Scope.t -> unit

val pop_env: unit -> unit

val init_env: context -> Scope.t -> unit

val update_env: context -> Scope.t list -> unit

(***)

val bind_var: ?state:Entry.state -> context -> string -> Type.t -> Loc.t -> unit
val bind_let: ?state:Entry.state -> context -> string -> Type.t -> Loc.t -> unit
val bind_implicit_let:
  ?state:Entry.state
    -> Entry.implicit_let_kinds
    -> context
    -> string
    -> Type.t
    -> Loc.t
    -> unit
val bind_const: ?state:Entry.state -> context -> string -> Type.t -> Loc.t -> unit
val bind_type: context -> string -> Type.t -> Loc.t -> unit

val bind_declare_var: context -> string -> Type.t -> Loc.t -> unit
val bind_declare_fun: context -> string -> Type.t -> reason -> unit

val declare_const: context -> string -> reason -> unit
val declare_let: context -> string -> reason -> unit
val declare_implicit_let:
  Entry.implicit_let_kinds
    -> context
    -> string
    -> reason
    -> unit

val init_var: context -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_let: context -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_implicit_let:
  Entry.implicit_let_kinds
    -> context
    -> string
    -> has_anno:bool
    -> Type.t
    -> reason
    -> unit
val init_const: context -> string -> has_anno:bool -> Type.t -> reason -> unit
val init_type: context -> string -> Type.t -> reason -> unit

val pseudo_init_declared_type: context -> string -> reason -> unit

module LookupMode: sig
  type t = ForValue | ForType | ForTypeof
end

val get_var: ?lookup_mode:LookupMode.t -> context -> string ->
  reason -> Type.t

val get_var_declared_type: ?lookup_mode:LookupMode.t  -> context ->
  string -> reason -> Type.t

val var_ref: ?lookup_mode:LookupMode.t  -> context -> string ->
  reason -> Type.t

val set_var: context -> string -> Type.t -> reason -> unit

val add_heap_refinement: context -> Key.t -> reason ->
  Type.t -> Type.t -> unit

val refine_with_preds: context -> reason ->
  Type.predicate KeyMap.t ->
  Type.t KeyMap.t ->
  unit

val refine_env: context -> reason ->
  Type.predicate KeyMap.t ->
  Type.t KeyMap.t ->
  (unit -> 'a) ->
  'a

val merge_env: context -> reason ->
  Scope.t list * Scope.t list * Scope.t list ->
  changeset -> unit

val widen_env: context -> reason -> unit

val copy_env: context -> reason ->
  Scope.t list * Scope.t list ->
  changeset -> unit

val let_env: string -> Entry.t -> (unit -> 'a) -> unit

val havoc_all: unit -> unit

val havoc_vars: changeset -> unit

val havoc_heap_refinements: unit -> unit
val havoc_heap_refinements_with_propname: string -> unit

val string_of_env: context -> Scope.t list -> string

val get_refinement: context -> Key.t -> reason -> Type.t option
