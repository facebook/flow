(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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

val set_var : context -> string -> Type.t -> reason -> unit

val init_env : context -> string -> block_entry -> unit

val clone_env : block list -> block list

val update_frame : context -> block list -> unit

val refine_with_pred : context -> reason ->
  Type.predicate SMap.t -> unit

val refine_env : context -> reason ->
  Type.predicate SMap.t -> (unit -> 'a) -> 'a

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

val clear_env : Reason_js.reason -> unit

val changeset: SSet.t ref
val swap_changeset: (SSet.t -> SSet.t) -> SSet.t
