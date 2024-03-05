(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val free_var_finder : Context.t -> ?bound:Subst_name.Set.t -> Type.t -> Subst_name.Set.t

val new_name : Subst_name.t -> Subst_name.Set.t -> Subst_name.t

val subst :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  Type.t Subst_name.Map.t ->
  Type.t ->
  Type.t

val subst_destructor :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  Type.t Subst_name.Map.t ->
  Type.destructor ->
  Type.destructor

val subst_instance_type :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  Type.t Subst_name.Map.t ->
  Type.instance_t ->
  Type.instance_t
