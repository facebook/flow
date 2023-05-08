(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val is_flow_successful :
  Context.t -> Reason.reason -> upper_unresolved:bool -> Type.t -> Type.use_t -> bool

val get_method_type_opt : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t option

(* NOTE The unsafe functions below may throw SpeculationSingletonError exception *)

val flow_t_unsafe : Context.t -> Reason.reason -> upper_unresolved:bool -> Type.t * Type.t -> unit

val resolved_lower_flow_unsafe : Context.t -> Reason.t -> Type.t * Type.use_t -> unit

val resolved_lower_flow_t_unsafe : Context.t -> Reason.t -> Type.t * Type.t -> unit

val resolved_upper_flow_t_unsafe : Context.t -> Reason.t -> Type.t * Type.t -> unit

val get_method_type_unsafe : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t
