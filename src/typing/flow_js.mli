(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

(* exceptions *)

exception Not_expect_bound of string

exception Attempted_operation_on_bound of string

(* propagates sources to sinks following a subtype relation *)
val flow : Context.t -> Type.t * Type.use_t -> unit

val flow_t : Context.t -> Type.t * Type.t -> unit

(* given a use type, return a tvar constrained by the use type *)
val tvar_with_constraint : Context.t -> ?trace:Trace.t -> ?derivable:bool -> Type.use_t -> Type.t

val unify : Context.t -> Type.t -> Type.t -> unit

val flow_p :
  Context.t ->
  use_op:Type.use_op ->
  reason ->
  (* lreason *)
  reason ->
  (* ureason *)
  Type.propref ->
  Type.property * Type.property ->
  unit

val reposition :
  Context.t ->
  ?trace:Trace.t ->
  ALoc.t ->
  ?desc:reason_desc ->
  ?annot_loc:ALoc.t ->
  Type.t ->
  Type.t

(* constraint utils *)
val filter_optional : Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.ident

module Cache : sig
  val stats_poly_instantiation : Context.t -> Hashtbl.statistics

  val summarize_flow_constraint : Context.t -> (string * int) list
end

val get_builtin_typeapp : Context.t -> ?trace:Trace.t -> reason -> string -> Type.t list -> Type.t

val mk_typeapp_instance :
  Context.t ->
  ?trace:Trace.t ->
  use_op:Type.use_op ->
  reason_op:Reason.reason ->
  reason_tapp:Reason.reason ->
  ?cache:Reason.reason list ->
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

val subst : Context.t -> ?use_op:Type.use_op -> ?force:bool -> Type.t SMap.t -> Type.t -> Type.t

val check_with_generics : Context.t -> Type.typeparam list -> (Type.t SMap.t -> 'a) -> 'a

val match_this_binding : Type.t SMap.t -> (Type.t -> bool) -> bool

val check_polarity :
  Context.t -> ?trace:Trace.t -> Type.typeparam SMap.t -> Polarity.t -> Type.t -> unit

(* selectors *)

val eval_selector :
  Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.selector -> Type.tvar -> int -> unit

val visit_eval_id : Context.t -> Type.Eval.id -> (Type.t -> unit) -> unit

(* destructors *)

val eval_evalt : Context.t -> ?trace:Trace.t -> Type.t -> Type.defer_use_t -> Type.Eval.id -> Type.t

val mk_type_destructor :
  Context.t ->
  trace:Trace.t ->
  Type.use_op ->
  Reason.reason ->
  Type.t ->
  Type.destructor ->
  Type.Eval.id ->
  bool * Type.t

(* ... *)

val mk_default : Context.t -> reason -> Type.t Default.t -> Type.t

val is_munged_prop_name : Context.t -> string -> bool

(* val graph: bounds IMap.t ref *)
val lookup_module : Context.t -> string -> Type.t

(* contexts *)
val mk_builtins : Context.t -> unit

val add_output : Context.t -> ?trace:Trace.t -> Error_message.t -> unit

(* builtins *)

val builtins : Context.t -> Type.t

val get_builtin : Context.t -> ?trace:Trace.t -> string -> reason -> Type.t

val lookup_builtin :
  Context.t -> ?trace:Trace.t -> string -> reason -> Type.lookup_kind -> Type.tvar -> unit

val get_builtin_type : Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> string -> Type.t

val set_builtin : Context.t -> ?trace:Trace.t -> string -> Type.t -> unit

val mk_instance : Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> Type.t -> Type.t

val mk_typeof_annotation :
  Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> ?internal:bool -> Type.t -> Type.t

(* strict *)
val types_of : Constraint.constraints -> Type.t list

val enforce_strict : Context.t -> Type.t -> should_munge_underscores:bool -> unit

val possible_types : Context.t -> Constraint.ident -> Type.t list

val possible_types_of_type : Context.t -> Type.t -> Type.t list

val possible_uses : Context.t -> Constraint.ident -> Type.use_t list

(* trust *)
val mk_trust_var : Context.t -> ?initial:Trust.trust_qualifier -> unit -> Type.ident

val strengthen_trust : Context.t -> Type.ident -> Trust.trust_qualifier -> Error_message.t -> unit

val widen_obj_type :
  Context.t -> ?trace:Trace.t -> use_op:Type.use_op -> Reason.reason -> Type.t -> Type.t
