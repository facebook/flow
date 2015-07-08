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

val peek_scope: unit -> Scope.t

val get_scopes: unit -> Scope.t list

val clone_scopes: Scope.t list -> Scope.t list

val in_async_scope: unit -> bool

val all_entries: unit -> Scope.entry SMap.t

val peek_changeset: unit -> SSet.t

val clear_changeset: unit -> SSet.t

val merge_changeset: SSet.t -> SSet.t

val peek_frame: unit -> ident

val clear_env: Reason_js.reason -> unit

val push_env: context -> Scope.t -> unit

val pop_env: unit -> unit

val init_env: context -> Scope.t -> unit

val update_env: context -> Scope.t list -> unit

(***)

val get_var: ?for_type:bool -> context -> string ->
  reason -> Type.t

val get_var_declared_type: ?for_type:bool -> context ->
  string -> reason -> Type.t

val var_ref: ?for_type:bool -> context -> string ->
  reason -> Type.t

val set_var: ?for_type:bool -> context -> string -> Type.t ->
  reason -> unit

val init_var: context -> string -> Scope.entry -> unit
val init_declare_fun: context -> string -> Scope.entry -> unit

val add_direct_refinement: context -> string -> reason -> Type.t -> unit

val refine_with_preds: context -> reason ->
  Type.predicate SMap.t ->
  Type.t SMap.t ->
  unit

val refine_env: context -> reason ->
  Type.predicate SMap.t ->
  Type.t SMap.t ->
  (unit -> 'a) ->
  'a

val merge_env: context -> reason ->
  Scope.t list * Scope.t list * Scope.t list ->
  SSet.t -> unit

val widen_env: context -> reason -> unit

val copy_env: context -> reason ->
  Scope.t list * Scope.t list ->
  SSet.t -> unit

val let_env: string -> Scope.entry -> (unit -> 'a) -> unit

val havoc_all: unit -> unit

val havoc_vars: SSet.t -> unit

val havoc_heap_refinements: unit -> unit

val string_of_env: context -> Scope.t list -> string

val refinement_key: string list -> string

val get_refinement: context -> string -> reason -> Type.t option
