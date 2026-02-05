(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val call_prop : 'a #Type_mapper.t -> Context.t -> 'a -> int -> int

val props : 'a #Type_mapper.t -> Context.t -> 'a -> Type.Properties.id -> Type.Properties.id

val exports : 'a #Type_mapper.t -> Context.t -> 'a -> Type.Exports.id -> Type.Exports.id

val free_var_finder : Context.t -> ?bound:Subst_name.Set.t -> Type.t -> Subst_name.Set.t

val free_var_finder_in_destructor :
  Context.t -> ?bound:Subst_name.Set.t -> Type.destructor -> Subst_name.Set.t

val new_name : Subst_name.t -> Subst_name.Set.t -> Subst_name.t

module Purpose : sig
  type t =
    | Normal
    | ConditionalTypeAnySubst
end

val subst :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  ?purpose:Purpose.t ->
  Type.t Subst_name.Map.t ->
  Type.t ->
  Type.t

val subst_destructor :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  ?purpose:Purpose.t ->
  Type.t Subst_name.Map.t ->
  Type.destructor ->
  Type.destructor

val subst_instance_type :
  Context.t ->
  ?use_op:Type.use_op ->
  ?force:bool ->
  ?placeholder_no_infer:bool ->
  ?purpose:Purpose.t ->
  Type.t Subst_name.Map.t ->
  Type.instance_t ->
  Type.instance_t
