(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

type default = (ALoc.t, ALoc.t) Flow_ast.Expression.t Default.t
type binding = string * ALoc.t * Type.t * default option

(* build up a params value *)
val empty: t

val add_simple: Context.t ->
  optional: bool ->
  ?default: (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  ALoc.t -> (ALoc.t * string) option -> Type.t ->
  t -> t

val add_complex: Context.t ->
  expr:(
    Context.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t
  ) ->
  (ALoc.t, ALoc.t) Flow_ast.Function.Param.t -> Type.t ->
  t ->
  t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Function.Param.t

val add_rest: Context.t ->
  ALoc.t -> (ALoc.t * string) option -> Type.t ->
  t -> t

(* (name, type) of each param, in order *)
(* destructured params will be unnamed *)
val value: t -> (string option * Type.t) list

(* The rest param *)
val rest: t -> (string option * ALoc.t * Type.t) option

(* iterates over all bindings, traversing through any destructued
   bindings as well, in source order of declaration *)
val iter: (binding -> unit) -> t -> unit

val subst: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t -> t
