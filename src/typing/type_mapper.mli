(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class virtual ['a] t :
  object
    method arr_type : Context.t -> 'a -> Type.arrtype -> Type.arrtype

    method bounds : Context.t -> 'a -> Constraint.bounds -> Constraint.bounds

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

    method obj_type : Context.t -> 'a -> Type.objtype -> Type.objtype

    method predicate : Context.t -> 'a -> Type.predicate -> Type.predicate

    method prop : Context.t -> 'a -> Type.Property.t -> Type.Property.t

    method virtual props : Context.t -> 'a -> Type.Properties.id -> Type.Properties.id

    method selector : Context.t -> 'a -> Type.selector -> Type.selector

    method targ : Context.t -> 'a -> Type.targ -> Type.targ

    method virtual tvar : Context.t -> 'a -> Reason.t -> Constraint.ident -> Constraint.ident

    method type_ : Context.t -> 'a -> Type.t -> Type.t

    method type_param : Context.t -> 'a -> Type.typeparam -> Type.typeparam

    method type_map : Context.t -> 'a -> Type.type_map -> Type.type_map

    method virtual use_type : Context.t -> 'a -> Type.use_t -> Type.use_t
  end

class virtual ['a] t_with_uses :
  object
    inherit ['a] t

    method call_arg : Context.t -> 'a -> Type.call_arg -> Type.call_arg

    method choice_use_tool : Context.t -> 'a -> Type.choice_use_tool -> Type.choice_use_tool

    method class_binding : Context.t -> 'a -> Type.class_binding -> Type.class_binding

    method cont : Context.t -> 'a -> Type.cont -> Type.cont

    method create_class_knot :
      Context.t -> 'a -> Type.React.CreateClass.knot -> Type.React.CreateClass.knot

    method create_class_spec :
      Context.t -> 'a -> Type.React.CreateClass.spec -> Type.React.CreateClass.spec

    method create_class_tool :
      Context.t -> 'a -> Type.React.CreateClass.tool -> Type.React.CreateClass.tool

    method default_props :
      Context.t ->
      'a ->
      Type.React.CreateClass.default_props ->
      Type.React.CreateClass.default_props

    method elem_action : Context.t -> 'a -> Type.elem_action -> Type.elem_action

    method fun_call_type : Context.t -> 'a -> Type.funcalltype -> Type.funcalltype

    method method_action : Context.t -> 'a -> Type.method_action -> Type.method_action

    method initial_state :
      Context.t ->
      'a ->
      Type.React.CreateClass.initial_state ->
      Type.React.CreateClass.initial_state

    method intersection_preprocess_tool :
      Context.t -> 'a -> Type.intersection_preprocess_tool -> Type.intersection_preprocess_tool

    method lookup_action : Context.t -> 'a -> Type.lookup_action -> Type.lookup_action

    method lookup_kind : Context.t -> 'a -> Type.lookup_kind -> Type.lookup_kind

    method object_kit_acc_element :
      Context.t -> 'a -> Type.Object.Spread.acc_element -> Type.Object.Spread.acc_element

    method object_kit_resolve_tool :
      Context.t -> 'a -> Type.Object.resolve_tool -> Type.Object.resolve_tool

    method object_kit_slice : Context.t -> 'a -> Type.Object.slice -> Type.Object.slice

    method object_kit_tool : Context.t -> 'a -> Type.Object.tool -> Type.Object.tool

    method prop_ref : Context.t -> 'a -> Type.propref -> Type.propref

    method react_tool : Context.t -> 'a -> Type.React.tool -> Type.React.tool

    method resolve : Context.t -> 'a -> Type.Object.resolve -> Type.Object.resolve

    method resolved_prop : Context.t -> 'a -> Type.Object.prop -> Type.Object.prop

    method resolve_array : Context.t -> 'a -> Type.React.resolve_array -> Type.React.resolve_array

    method resolve_object :
      Context.t -> 'a -> Type.React.resolve_object -> Type.React.resolve_object

    method resolve_spread : Context.t -> 'a -> Type.resolve_spread_type -> Type.resolve_spread_type

    method resolved : Context.t -> 'a -> Type.Object.resolved -> Type.Object.resolved

    method resolved_object :
      Context.t -> 'a -> Type.React.resolved_object -> Type.React.resolved_object

    method resolved_param : Context.t -> 'a -> Type.resolved_param -> Type.resolved_param

    method simplify_prop_type_tool :
      Context.t -> 'a -> Type.React.SimplifyPropType.tool -> Type.React.SimplifyPropType.tool

    method spec : Context.t -> 'a -> Type.spec -> Type.spec

    method spread_resolve : Context.t -> 'a -> Type.spread_resolve -> Type.spread_resolve

    method stack_head :
      Context.t -> 'a -> Type.React.CreateClass.stack_head -> Type.React.CreateClass.stack_head

    method stack_tail :
      Context.t -> 'a -> Type.React.CreateClass.stack_tail -> Type.React.CreateClass.stack_tail

    method stack_tail_elem :
      Context.t ->
      'a ->
      Type.React.CreateClass.stack_head
      * Type.t list
      * Type.React.CreateClass.spec Type.React.CreateClass.maybe_known list ->
      Type.React.CreateClass.stack_head
      * Type.t list
      * Type.React.CreateClass.spec Type.React.CreateClass.maybe_known list

    method unresolved_param : Context.t -> 'a -> Type.unresolved_param -> Type.unresolved_param

    method use_type : Context.t -> 'a -> Type.use_t -> Type.use_t
  end

val union_flatten : Context.t -> Type.t list -> Type.t list

val unwrap_type : Context.t -> Type.t -> Type.t
