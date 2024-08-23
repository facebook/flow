(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Keys : sig
  val key : allow_optional:bool -> ('loc, 't) Flow_ast.Expression.t -> ('t option * Key.t) option
end

include module type of Keys

val get :
  allow_optional:bool ->
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  ALoc.t ->
  Type.t option
