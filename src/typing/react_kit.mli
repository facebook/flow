(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

module type REACT = sig
  val run :
    Context.t -> Type.DepthTrace.t -> use_op:use_op -> reason -> Type.t -> Type.React.tool -> unit

  val component_class : Context.t -> Reason.reason -> Type.t -> Type.t

  val subtype_class_component_render :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    Type.t ->
    reason_op:reason ->
    Type.t ->
    unit

  val get_config :
    Context.t ->
    Type.DepthTrace.t ->
    Type.t ->
    use_op:use_op ->
    reason_op:reason ->
    Type.React.tool ->
    Polarity.t ->
    Type.t ->
    unit

  val err_incompatible : Context.t -> use_op:use_op -> reason -> Type.React.tool -> unit
end

module Kit (_ : Flow_common.S) : REACT
