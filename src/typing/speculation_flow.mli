(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val flow_t : Context.t -> Reason.reason -> upper_unresolved:bool -> Type.t * Type.t -> unit

val possible_concrete_types : Context.t -> Reason.t -> Type.t -> Type.t list

val try_singleton_no_throws :
  Context.t -> Reason.reason -> upper_unresolved:bool -> Type.t -> Type.use_t -> bool

val resolved_lower_flow : Context.t -> Reason.t -> Type.t * Type.use_t -> unit

val resolved_lower_flow_t : Context.t -> Reason.t -> Type.t * Type.t -> unit

val resolved_upper_flow_t : Context.t -> Reason.t -> Type.t * Type.t -> unit

val get_method_type : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t

val get_method_type_no_throw : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t
