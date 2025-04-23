(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class virtual ['a] type_subst_base :
  object
    method arr_type : Context.t -> 'a -> Type.arrtype -> Type.arrtype

    method call_prop : Context.t -> 'a -> int -> int

    method def_type : Context.t -> 'a -> Type.def_t -> Type.def_t

    method defer_use_type : Context.t -> 'a -> Type.defer_use_t -> Type.defer_use_t

    method destructor : Context.t -> 'a -> Type.destructor -> Type.destructor

    method dict_type : Context.t -> 'a -> Type.dicttype -> Type.dicttype

    method enum_info : Context.t -> 'a -> Type.enum_info -> Type.enum_info

    method virtual eval_id : Context.t -> 'a -> Type.Eval.id -> Type.Eval.id

    method export_types : Context.t -> 'a -> Type.exporttypes -> Type.exporttypes

    method exports : Context.t -> 'a -> Type.Exports.id -> Type.Exports.id

    method fun_type : Context.t -> 'a -> Type.funtype -> Type.funtype

    method inst_type : Context.t -> 'a -> Type.insttype -> Type.insttype

    method instance_type : Context.t -> 'a -> Type.instance_t -> Type.instance_t

    method object_kit_spread_operand :
      Context.t -> 'a -> Type.Object.Spread.operand -> Type.Object.Spread.operand

    method object_kit_spread_operand_slice :
      Context.t -> 'a -> Type.Object.Spread.operand_slice -> Type.Object.Spread.operand_slice

    method obj_flags : Context.t -> 'a -> Type.flags -> Type.flags

    method obj_type : Context.t -> 'a -> Type.objtype -> Type.objtype

    method namespace_type : Context.t -> 'a -> Type.namespace_type -> Type.namespace_type

    method predicate : Context.t -> 'a -> Type.predicate -> Type.predicate

    method prop : Context.t -> 'a -> Type.Property.t -> Type.Property.t

    method props : Context.t -> 'a -> Type.Properties.id -> Type.Properties.id

    method selector : Context.t -> 'a -> Type.selector -> Type.selector

    method targ : Context.t -> 'a -> Type.targ -> Type.targ

    method tvar : Context.t -> 'a -> Reason.t -> Type.ident -> Type.ident

    method type_ : Context.t -> 'a -> Type.t -> Type.t

    method type_param : Context.t -> 'a -> Type.typeparam -> Type.typeparam
  end

val free_var_finder : Context.t -> ?bound:Subst_name.Set.t -> Type.t -> Subst_name.Set.t

val free_var_finder_in_destructor :
  Context.t -> ?bound:Subst_name.Set.t -> Type.destructor -> Subst_name.Set.t

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
