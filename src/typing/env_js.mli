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

type env = scope list ref

val env : env

val global_scope : scope

val push_env : scope -> unit

val pop_env : unit -> unit

val flat_env :
  unit ->
  scope_entry Utils.SMap.t

val get_var : ?for_type:bool -> context -> string -> reason -> Type.t

val get_var_in_scope : ?for_type:bool -> context -> string -> reason -> Type.t

val var_ref : ?for_type:bool -> context -> string -> reason -> Type.t

val set_var : ?for_type:bool -> context -> string -> Type.t -> reason -> unit

val init_env : context -> string -> scope_entry -> scope_kind -> unit

val clone_env : scope list -> scope list

val update_frame : context -> scope list -> unit

val refine_with_pred : context -> reason ->
  Type.predicate SMap.t ->
  Type.t SMap.t ->
  unit

val refine_env : context -> reason ->
  Type.predicate SMap.t ->
  Type.t SMap.t ->
  (unit -> 'a) ->
  'a

val merge_env : context -> reason ->
  scope list * scope list * scope list ->
  SSet.t -> unit

val widen_env : context -> reason -> unit

val copy_env : context -> reason ->
  scope list * scope list ->
  SSet.t -> unit

val let_env : string -> scope_entry -> (unit -> 'a) -> unit

val havoc_env : unit -> unit

val havoc_env2 : SSet.t -> unit

val havoc_heap_refinements : unit -> unit

val clear_env : Reason_js.reason -> unit

val string_of_env : context -> scope list -> string

val changeset: SSet.t ref
val swap_changeset: (SSet.t -> SSet.t) -> SSet.t

val refinement_key : string list -> string
val get_refinement: context -> string -> reason -> Type.t option
