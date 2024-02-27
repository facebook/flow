(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

(* propagates sources to sinks following a subtype relation *)
val flow : Context.t -> Type.t * Type.use_t -> unit

val flow_t : Context.t -> Type.t * Type.t -> unit

val unify : Context.t -> ?use_op:Type.use_op -> Type.t -> Type.t -> unit

val flow_p :
  Context.t ->
  use_op:Type.use_op ->
  reason ->
  (* lreason *)
  reason ->
  (* ureason *)
  Type.propref ->
  Type.property_type * Type.property_type ->
  unit

val flow_use_op : Context.t -> Type.use_op -> Type.use_t -> Type.use_t

val reposition : Context.t -> ALoc.t -> Type.t -> Type.t

val reposition_reason : Context.t -> Reason.reason -> ?use_desc:bool -> Type.t -> Type.t

(* constraint utils *)
val filter_optional : Context.t -> reason -> Type.t -> Type.ident

module Cache : sig
  val summarize_flow_constraint : Context.t -> (string * int) list
end

val mk_typeapp_instance_annot :
  Context.t ->
  use_op:Type.use_op ->
  reason_op:Reason.reason ->
  reason_tapp:Reason.reason ->
  from_value:bool ->
  ?cache:bool ->
  Type.t ->
  Type.t list ->
  Type.t

val resolve_spread_list :
  Context.t ->
  use_op:Type.use_op ->
  reason_op:Reason.t ->
  Type.unresolved_param list ->
  Type.spread_resolve ->
  unit

(* polymorphism *)

val subst :
  Context.t -> ?use_op:Type.use_op -> ?force:bool -> Type.t Subst_name.Map.t -> Type.t -> Type.t

val check_polarity : Context.t -> Type.typeparam Subst_name.Map.t -> Polarity.t -> Type.t -> unit

(* destructors *)

val mk_type_destructor :
  Context.t -> Type.use_op -> Reason.reason -> Type.t -> Type.destructor -> Type.Eval.id -> Type.t

val mk_possibly_evaluated_destructor :
  Context.t -> Type.use_op -> Reason.reason -> Type.t -> Type.destructor -> Type.Eval.id -> Type.t

(* ... *)

val mk_default : Context.t -> reason -> Type.t Default.t -> Type.t

(* contexts *)

val add_output : Context.t -> Error_message.t -> unit

(* builtins *)

val get_builtin_type : Context.t -> reason -> ?use_desc:bool -> string -> Type.t

val get_builtin_typeapp : Context.t -> reason -> ?use_desc:bool -> string -> Type.t list -> Type.t

val mk_instance :
  Context.t -> ?type_t_kind:Type.type_t_kind -> reason -> ?use_desc:bool -> Type.t -> Type.t

val possible_concrete_types_for_inspection : Context.t -> Reason.reason -> Type.t -> Type.t list

val possible_concrete_types_for_imports_exports :
  Context.t -> Reason.reason -> Type.t -> Type.t list

val singleton_concrete_type_for_inspection : Context.t -> Reason.reason -> Type.t -> Type.t

val possible_concrete_types_for_computed_props : Context.t -> Reason.reason -> Type.t -> Type.t list

val resolve_id : Context.t -> int -> Type.t -> unit

module FlowJs : Flow_common.S
