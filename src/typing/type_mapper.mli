(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
class t: object
   method arr_type: Context.t -> Type.arrtype -> Type.arrtype
   method bounds: Context.t -> Constraint.bounds -> Constraint.bounds
   method call_arg: Context.t -> Type.call_arg -> Type.call_arg
   method choice_use_tool: Context.t -> Type.choice_use_tool -> Type.choice_use_tool
   method cont: Context.t -> Type.cont -> Type.cont
   method create_class_knot: Context.t -> Type.React.CreateClass.knot -> Type.React.CreateClass.knot
   method create_class_spec: Context.t -> Type.React.CreateClass.spec -> Type.React.CreateClass.spec
   method create_class_tool: Context.t -> Type.React.CreateClass.tool -> Type.React.CreateClass.tool
   method def_type: Context.t -> Type.def_t -> Type.def_t
   method default_props:
     Context.t -> Type.React.CreateClass.default_props -> Type.React.CreateClass.default_props
   method defer_use_type: Context.t -> Type.defer_use_t -> Type.defer_use_t
   method destructor: Context.t -> Type.destructor -> Type.destructor
   method dict_type: Context.t -> Type.dicttype -> Type.dicttype
   method elem_action: Context.t -> Type.elem_action -> Type.elem_action
   method export_types: Context.t -> Type.exporttypes -> Type.exporttypes
   method exports: Context.t -> Type.Exports.id -> Type.Exports.id
   method fun_call_type: Context.t -> Type.funcalltype -> Type.funcalltype
   method fun_type: Context.t -> Type.funtype -> Type.funtype
   method initial_state:
     Context.t -> Type.React.CreateClass.initial_state -> Type.React.CreateClass.initial_state
   method inst_type: Context.t -> Type.insttype -> Type.insttype
   method intersection_preprocess_tool:
     Context.t -> Type.intersection_preprocess_tool -> Type.intersection_preprocess_tool
   method lookup_action: Context.t -> Type.lookup_action -> Type.lookup_action
   method lookup_kind: Context.t -> Type.lookup_kind -> Type.lookup_kind
   method obj_type: Context.t -> Type.objtype -> Type.objtype
   method object_spread_state: Context.t -> Type.ObjectSpread.state -> Type.ObjectSpread.state
   method object_spread_tool: Context.t -> Type.ObjectSpread.tool -> Type.ObjectSpread.tool
   method predicate: Context.t -> Type.predicate -> Type.predicate
   method prop_ref: Context.t -> Type.propref -> Type.propref
   method react_tool: Context.t -> Type.React.tool -> Type.React.tool
   method resolve: Context.t -> Type.ObjectSpread.resolve -> Type.ObjectSpread.resolve
   method resolve_array: Context.t -> Type.React.resolve_array -> Type.React.resolve_array
   method resolve_object: Context.t -> Type.React.resolve_object -> Type.React.resolve_object
   method resolve_spread: Context.t -> Type.resolve_spread_type -> Type.resolve_spread_type
   method resolved: Context.t -> Type.ObjectSpread.resolved -> Type.ObjectSpread.resolved
   method resolved_object: Context.t -> Type.React.resolved_object -> Type.React.resolved_object
   method resolved_param: Context.t -> Type.resolved_param -> Type.resolved_param
   method selector: Context.t -> Type.selector -> Type.selector
   method simplify_prop_type_tool:
     Context.t -> Type.React.SimplifyPropType.tool -> Type.React.SimplifyPropType.tool
   method spec: Context.t -> Type.spec -> Type.spec
   method spread_resolve: Context.t -> Type.spread_resolve -> Type.spread_resolve
   method stack_head:
     Context.t -> Type.React.CreateClass.stack_head -> Type.React.CreateClass.stack_head
   method stack_tail:
     Context.t -> Type.React.CreateClass.stack_tail -> Type.React.CreateClass.stack_tail
   method stack_tail_elem:
     Context.t -> Type.React.CreateClass.stack_head * Type.t list *
     Type.React.CreateClass.spec Type.React.CreateClass.maybe_known list ->
     Type.React.CreateClass.stack_head * Type.t list *
     Type.React.CreateClass.spec Type.React.CreateClass.maybe_known
     list
   method type_: Context.t -> Type.t -> Type.t
   method type_param: Context.t -> Type.typeparam -> Type.typeparam
   method unresolved_param: Context.t -> Type.unresolved_param -> Type.unresolved_param
   method use_type: Context.t -> Type.UseTypeMap.key -> Type.UseTypeMap.key
end
