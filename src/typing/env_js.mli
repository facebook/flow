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

type env = block list ref

val env : env

val global_block : block

val push_env : block -> unit

val pop_env : unit -> unit

val flat_env :
  unit ->
  block_entry Utils.SMap.t

val get_var : context -> string -> reason -> Type.t

val get_var_in_scope : context -> string -> reason -> Type.t

val var_ref : context -> string -> reason -> Type.t

val set_var : context -> string -> Type.t -> reason -> unit

val init_env : context -> string -> block_entry -> unit

val clone_env : block list -> block list

val update_frame : context -> block list -> unit

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
  block list * block list * block list ->
  SSet.t -> unit

val widen_env : context -> reason -> unit

val copy_env : context -> reason ->
  block list * block list ->
  SSet.t -> unit

val let_env : string -> block_entry -> (unit -> 'a) -> unit

val havoc_env : unit -> unit

val havoc_env2 : SSet.t -> unit

val havoc_heap_refinements : unit -> unit

val clear_env : Reason_js.reason -> unit

val changeset: SSet.t ref
val swap_changeset: (SSet.t -> SSet.t) -> SSet.t

val refinement_key : string list -> string
val get_refinement: context -> string -> reason -> Type.t option

