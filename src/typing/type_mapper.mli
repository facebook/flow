(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class virtual ['a, 'phase] t :
  object
    method arr_type : 'phase Context.t_ -> 'a -> Type.arrtype -> Type.arrtype

    method bounds : 'phase Context.t_ -> 'a -> Type.Constraint.bounds -> Type.Constraint.bounds

    method virtual call_prop : 'phase Context.t_ -> 'a -> int -> int

    method def_type : 'phase Context.t_ -> 'a -> Type.def_t -> Type.def_t

    method defer_use_type : 'phase Context.t_ -> 'a -> Type.defer_use_t -> Type.defer_use_t

    method destructor : 'phase Context.t_ -> 'a -> Type.destructor -> Type.destructor

    method dict_type : 'phase Context.t_ -> 'a -> Type.dicttype -> Type.dicttype

    method enum : 'phase Context.t_ -> 'a -> Type.enum_t -> Type.enum_t

    method virtual eval_id : 'phase Context.t_ -> 'a -> Type.Eval.id -> Type.Eval.id

    method export_types : 'phase Context.t_ -> 'a -> Type.exporttypes -> Type.exporttypes

    method virtual exports : 'phase Context.t_ -> 'a -> Type.Exports.id -> Type.Exports.id

    method fun_type : 'phase Context.t_ -> 'a -> Type.funtype -> Type.funtype

    method inst_type : 'phase Context.t_ -> 'a -> Type.insttype -> Type.insttype

    method object_kit_spread_operand :
      'phase Context.t_ -> 'a -> Type.Object.Spread.operand -> Type.Object.Spread.operand

    method object_kit_spread_operand_slice :
      'phase Context.t_ ->
      'a ->
      Type.Object.Spread.operand_slice ->
      Type.Object.Spread.operand_slice

    method obj_flags : 'phase Context.t_ -> 'a -> Type.flags -> Type.flags

    method obj_type : 'phase Context.t_ -> 'a -> Type.objtype -> Type.objtype

    method predicate : 'phase Context.t_ -> 'a -> Type.predicate -> Type.predicate

    method prop : 'phase Context.t_ -> 'a -> Type.Property.t -> Type.Property.t

    method virtual props : 'phase Context.t_ -> 'a -> Type.Properties.id -> Type.Properties.id

    method selector : 'phase Context.t_ -> 'a -> Type.selector -> Type.selector

    method targ : 'phase Context.t_ -> 'a -> Type.targ -> Type.targ

    method virtual tvar : 'phase Context.t_ -> 'a -> Reason.t -> Type.ident -> Type.ident

    method type_ : 'phase Context.t_ -> 'a -> Type.t -> Type.t

    method type_param : 'phase Context.t_ -> 'a -> Type.typeparam -> Type.typeparam

    method type_map : 'phase Context.t_ -> 'a -> Type.type_map -> Type.type_map

    method virtual use_type : 'phase Context.t_ -> 'a -> Type.use_t -> Type.use_t
  end

class virtual ['a, 'phase] t_with_uses :
  object
    inherit ['a, 'phase] t

    method call_arg : 'phase Context.t_ -> 'a -> Type.call_arg -> Type.call_arg

    method choice_use_tool : 'phase Context.t_ -> 'a -> Type.choice_use_tool -> Type.choice_use_tool

    method class_binding : 'phase Context.t_ -> 'a -> Type.class_binding -> Type.class_binding

    method cont : 'phase Context.t_ -> 'a -> Type.cont -> Type.cont

    method create_class_knot :
      'phase Context.t_ -> 'a -> Type.React.CreateClass.knot -> Type.React.CreateClass.knot

    method create_class_spec :
      'phase Context.t_ -> 'a -> Type.React.CreateClass.spec -> Type.React.CreateClass.spec

    method create_class_tool :
      'phase Context.t_ -> 'a -> Type.React.CreateClass.tool -> Type.React.CreateClass.tool

    method default_props :
      'phase Context.t_ ->
      'a ->
      Type.React.CreateClass.default_props ->
      Type.React.CreateClass.default_props

    method elem_action : 'phase Context.t_ -> 'a -> Type.elem_action -> Type.elem_action

    method fun_call_type : 'phase Context.t_ -> 'a -> Type.funcalltype -> Type.funcalltype

    method method_call_type : 'phase Context.t_ -> 'a -> Type.methodcalltype -> Type.methodcalltype

    method method_action : 'phase Context.t_ -> 'a -> Type.method_action -> Type.method_action

    method initial_state :
      'phase Context.t_ ->
      'a ->
      Type.React.CreateClass.initial_state ->
      Type.React.CreateClass.initial_state

    method intersection_preprocess_tool :
      'phase Context.t_ ->
      'a ->
      Type.intersection_preprocess_tool ->
      Type.intersection_preprocess_tool

    method lookup_action : 'phase Context.t_ -> 'a -> Type.lookup_action -> Type.lookup_action

    method lookup_kind : 'phase Context.t_ -> 'a -> Type.lookup_kind -> Type.lookup_kind

    method object_kit_acc_element :
      'phase Context.t_ -> 'a -> Type.Object.Spread.acc_element -> Type.Object.Spread.acc_element

    method object_kit_resolve_tool :
      'phase Context.t_ -> 'a -> Type.Object.resolve_tool -> Type.Object.resolve_tool

    method object_kit_slice : 'phase Context.t_ -> 'a -> Type.Object.slice -> Type.Object.slice

    method object_kit_tool : 'phase Context.t_ -> 'a -> Type.Object.tool -> Type.Object.tool

    method prop_ref : 'phase Context.t_ -> 'a -> Type.propref -> Type.propref

    method react_tool : 'phase Context.t_ -> 'a -> Type.React.tool -> Type.React.tool

    method resolve : 'phase Context.t_ -> 'a -> Type.Object.resolve -> Type.Object.resolve

    method resolved_prop : 'phase Context.t_ -> 'a -> Type.Object.prop -> Type.Object.prop

    method resolve_array :
      'phase Context.t_ -> 'a -> Type.React.resolve_array -> Type.React.resolve_array

    method resolve_object :
      'phase Context.t_ -> 'a -> Type.React.resolve_object -> Type.React.resolve_object

    method resolve_spread :
      'phase Context.t_ -> 'a -> Type.resolve_spread_type -> Type.resolve_spread_type

    method resolved : 'phase Context.t_ -> 'a -> Type.Object.resolved -> Type.Object.resolved

    method resolved_object :
      'phase Context.t_ -> 'a -> Type.React.resolved_object -> Type.React.resolved_object

    method resolved_param : 'phase Context.t_ -> 'a -> Type.resolved_param -> Type.resolved_param

    method simplify_prop_type_tool :
      'phase Context.t_ ->
      'a ->
      Type.React.SimplifyPropType.tool ->
      Type.React.SimplifyPropType.tool

    method spec : 'phase Context.t_ -> 'a -> Type.spec -> Type.spec

    method spread_resolve : 'phase Context.t_ -> 'a -> Type.spread_resolve -> Type.spread_resolve

    method stack_head :
      'phase Context.t_ ->
      'a ->
      Type.React.CreateClass.stack_head ->
      Type.React.CreateClass.stack_head

    method stack_tail :
      'phase Context.t_ ->
      'a ->
      Type.React.CreateClass.stack_tail ->
      Type.React.CreateClass.stack_tail

    method stack_tail_elem :
      'phase Context.t_ ->
      'a ->
      Type.React.CreateClass.stack_head
      * Type.t list
      * Type.React.CreateClass.spec Type.React.CreateClass.maybe_known list ->
      Type.React.CreateClass.stack_head
      * Type.t list
      * Type.React.CreateClass.spec Type.React.CreateClass.maybe_known list

    method unresolved_param :
      'phase Context.t_ -> 'a -> Type.unresolved_param -> Type.unresolved_param

    method use_type : 'phase Context.t_ -> 'a -> Type.use_t -> Type.use_t
  end

val union_flatten : 'phase Context.t_ -> Type.t list -> Type.t list

val unwrap_type : 'phase Context.t_ -> Type.t -> Type.t
