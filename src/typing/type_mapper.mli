(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class virtual ['a] t :
  object
    method arr_type : Context.t -> 'a -> Type.arrtype -> Type.arrtype

    method virtual call_prop : Context.t -> 'a -> int -> int

    method def_type : Context.t -> 'a -> Type.def_t -> Type.def_t

    method defer_use_type : Context.t -> 'a -> Type.defer_use_t -> Type.defer_use_t

    method destructor : Context.t -> 'a -> Type.destructor -> Type.destructor

    method dict_type : Context.t -> 'a -> Type.dicttype -> Type.dicttype

    method enum : Context.t -> 'a -> Type.enum_t -> Type.enum_t

    method virtual eval_id : Context.t -> 'a -> Type.Eval.id -> Type.Eval.id

    method export_types : Context.t -> 'a -> Type.exporttypes -> Type.exporttypes

    method virtual exports : Context.t -> 'a -> Type.Exports.id -> Type.Exports.id

    method fun_type : Context.t -> 'a -> Type.funtype -> Type.funtype

    method inst_type : Context.t -> 'a -> Type.insttype -> Type.insttype

    method object_kit_spread_operand :
      Context.t -> 'a -> Type.Object.Spread.operand -> Type.Object.Spread.operand

    method object_kit_spread_operand_slice :
      Context.t -> 'a -> Type.Object.Spread.operand_slice -> Type.Object.Spread.operand_slice

    method obj_flags : Context.t -> 'a -> Type.flags -> Type.flags

    method obj_type : Context.t -> 'a -> Type.objtype -> Type.objtype

    method predicate : Context.t -> 'a -> Type.predicate -> Type.predicate

    method prop : Context.t -> 'a -> Type.Property.t -> Type.Property.t

    method virtual props : Context.t -> 'a -> Type.Properties.id -> Type.Properties.id

    method selector : Context.t -> 'a -> Type.selector -> Type.selector

    method targ : Context.t -> 'a -> Type.targ -> Type.targ

    method virtual tvar : Context.t -> 'a -> Reason.t -> Type.ident -> Type.ident

    method type_ : Context.t -> 'a -> Type.t -> Type.t

    method type_param : Context.t -> 'a -> Type.typeparam -> Type.typeparam

    method type_map : Context.t -> 'a -> Type.type_map -> Type.type_map

    method class_binding : Context.t -> 'a -> Type.class_binding -> Type.class_binding

    method call_arg : Context.t -> 'a -> Type.call_arg -> Type.call_arg

    method fun_call_type : Context.t -> 'a -> Type.funcalltype -> Type.funcalltype
  end

val union_flatten : Context.t -> Type.t list -> Type.t list
