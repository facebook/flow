(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type OBJECT = sig
  val run :
    Type.DepthTrace.t ->
    Context.t ->
    Type.use_op ->
    Reason.t ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    tout:Type.t ->
    unit

  val mapped_type_of_keys :
    Context.t ->
    Type.DepthTrace.t ->
    Type.use_op ->
    Reason.t ->
    keys:Type.t ->
    property_type:Type.t ->
    Type.mapped_type_flags ->
    Type.t
end

module Kit (_ : Flow_common.S) : OBJECT
