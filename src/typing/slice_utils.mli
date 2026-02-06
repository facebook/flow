(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_object_type :
  Context.t ->
  reason:Reason.reason ->
  ?wrap_on_exact_obj:bool ->
  invalidate_aliases:bool ->
  interface:(Type.t * Type.insttype) option ->
  reachable_targs:(Type.t * Polarity.t) list ->
  kind:Subst_name.op_kind ->
  Type.flags ->
  int option ->
  Type.Properties.id ->
  Type.t ->
  Generic.spread_id ->
  Type.t

val type_optionality_and_missing_property : Type.Object.prop -> Type.t * bool * bool

val possibly_missing_prop : Reason.name -> Reason.reason -> Type.t -> Type.t

val make_optional_with_possible_missing_props :
  Reason.name -> bool -> bool -> Reason.reason -> Type.t -> Type.t

val merge_dro : ALoc.t option -> ALoc.t option -> ALoc.t option

val read_prop : Reason.reason -> Type.flags -> Reason.name -> Type.property -> Type.Object.prop

val read_dict : Reason.reason -> Type.dicttype -> Type.t

val object_slice :
  Context.t ->
  interface:(Type.t * Type.insttype) option ->
  Reason.reason ->
  Type.Properties.id ->
  Type.flags ->
  bool ->
  (Type.t * Polarity.t) list ->
  Generic.spread_id ->
  Type.Object.slice

val get_prop :
  Reason.reason -> Type.Object.prop option -> Type.dicttype option -> Type.Object.prop option

val merge :
  (Type.Object.slice -> Type.Object.slice -> Type.Object.slice) ->
  Type.Object.slice Nel.t ->
  Type.Object.slice Nel.t * Type.Object.slice Nel.t list ->
  Type.Object.slice Nel.t

val merge_result :
  ('a -> 'a -> ('a, Error_message.t) result) ->
  ('b -> 'a Nel.t) ->
  'b ->
  'b * 'b list ->
  ('a Nel.t, Error_message.t) result

exception CannotSpreadError of Error_message.t

val spread :
  dict_check:(Context.t -> Type.use_op -> Type.dicttype -> Type.dicttype -> unit) ->
  Context.t ->
  use_op:Type.use_op ->
  Reason.reason ->
  Type.Object.Spread.acc_element * Type.Object.Spread.acc_element list ->
  ((bool * Reason.reason option * Type.Object.slice) Nel.t, Error_message.t) result

val spread_mk_object :
  Context.t -> Reason.reason -> Type.Object.Spread.target -> Type.Object.slice -> Type.t

val object_spread :
  dict_check:(Context.t -> Type.use_op -> Type.dicttype -> Type.dicttype -> unit) ->
  add_output:(Context.t -> Error_message.t -> unit) ->
  return:(Context.t -> Type.use_op -> Type.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  Type.Object.Spread.target ->
  Type.Object.Spread.state ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.slice Nel.t ->
  'a

val check_component_config :
  add_output:(Context.t -> Error_message.t -> unit) ->
  return:(Context.t -> Type.use_op -> Type.t -> 'a) ->
  allow_ref_in_spread:bool ->
  Type.property NameUtils.Map.t ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.slice Nel.t ->
  'a

val object_rest :
  add_output:(Context.t -> Error_message.t -> unit) ->
  return:(Context.t -> (Polarity.t -> Type.use_op) -> Type.Object.Rest.merge_mode -> Type.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  subt_check:(use_op:Type.use_op -> Context.t -> Type.t * Type.t -> unit) ->
  Type.Object.Rest.merge_mode ->
  Type.Object.Rest.state ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.slice Nel.t ->
  'a

val object_make_exact : Context.t -> Reason.reason -> Type.Object.slice Nel.t -> Type.t

val object_read_only : Context.t -> Reason.reason -> Type.Object.slice Nel.t -> Type.t

val object_update_optionality :
  [ `Partial | `Required ] -> Context.t -> Reason.reason -> Type.Object.slice Nel.t -> Type.t

val intersect2 :
  Context.t ->
  Reason.reason ->
  Type.Object.slice ->
  Type.Object.slice ->
  Type.Object.prop NameUtils.Map.t
  * Type.flags
  * bool
  * Generic.spread_id
  * (Type.t * Polarity.t) list

val intersect2_with_reason :
  Context.t ->
  Reason.reason ->
  ALoc.t ->
  Type.Object.slice ->
  Type.Object.slice ->
  Type.Object.slice

val resolved :
  next:
    (Context.t -> Type.use_op -> Type.Object.tool -> Reason.reason -> Type.Object.slice Nel.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.resolve ->
  Type.Object.tool ->
  Type.Object.slice Nel.t ->
  'a

val interface_slice :
  Context.t ->
  Reason.reason ->
  static:Type.t ->
  inst:Type.insttype ->
  Type.Properties.id ->
  Generic.spread_id ->
  Type.Object.slice

val resolve :
  add_output:(Context.t -> Error_message.t -> unit) ->
  return:(Context.t -> Type.use_op -> Type.t -> 'a) ->
  next:
    (Context.t -> Type.use_op -> Type.Object.tool -> Reason.reason -> Type.Object.slice Nel.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  statics:(Context.t -> Reason.reason -> Type.t -> Type.t) ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.resolve ->
  Type.Object.tool ->
  Type.t ->
  'a

val super :
  return:(Context.t -> Type.use_op -> Type.t -> 'a) ->
  next:
    (Context.t -> Type.use_op -> Type.Object.tool -> Reason.reason -> Type.Object.slice Nel.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.resolve ->
  Type.Object.tool ->
  Type.Object.slice ->
  Type.t ->
  'a

val mk_mapped_prop_type :
  use_op:Type.use_op ->
  mapped_type_optionality:Type.mapped_type_optionality ->
  poly_prop:Type.t ->
  Type.t ->
  bool ->
  Type.t

val is_prop_optional : Type.t -> bool

val map_object :
  Type.t ->
  Type.mapped_type_flags ->
  Context.t ->
  Reason.reason ->
  Type.use_op ->
  ((Reason.name * Reason.reason) list * Type.t list) option ->
  Type.Object.slice ->
  Type.t

val run :
  add_output:(Context.t -> Error_message.t -> unit) ->
  return:(Context.t -> Type.use_op -> Type.t -> 'a) ->
  next:
    (Context.t -> Type.use_op -> Type.Object.tool -> Reason.reason -> Type.Object.slice Nel.t -> 'a) ->
  recurse:
    (Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    'a
    ) ->
  statics:(Context.t -> Reason.reason -> Type.t -> Type.t) ->
  Context.t ->
  Type.use_op ->
  Reason.reason ->
  Type.Object.resolve_tool ->
  Type.Object.tool ->
  Type.t ->
  'a
