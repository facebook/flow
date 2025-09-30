(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val try_custom :
  Context.t ->
  ?use_op:Type.use_op ->
  ?use_t:Type.use_t ->
  ?default_resolve:(unit -> unit) ->
  no_match_error_loc:ALoc.t ->
  (unit -> unit) list ->
  unit

val is_flow_successful : Context.t -> Type.t -> Type.use_t -> bool

val is_subtyping_successful : Context.t -> Type.t -> Type.t -> bool

val get_method_type_opt : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t option

(* NOTE The unsafe functions below may throw SpeculationSingletonError exception *)

val flow_t_unsafe : Context.t -> Type.t * Type.t -> unit

val resolved_lower_flow_unsafe : Context.t -> Reason.t -> Type.t * Type.use_t -> unit

val resolved_lower_flow_t_unsafe : Context.t -> Reason.t -> Type.t * Type.t -> unit

val resolved_upper_flow_t_unsafe : Context.t -> Reason.t -> Type.t * Type.t -> unit

val get_method_type_unsafe : Context.t -> Type.t -> Reason.reason -> Type.propref -> Type.t
