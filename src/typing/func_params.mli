(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

type default = (Loc.t, Loc.t) Flow_ast.Expression.t Default.t
type binding = string * Loc.t * Type.t * default option

(* build up a params value *)
val empty: t

val add_simple: Context.t ->
  optional: bool ->
  ?default: (Loc.t, Loc.t) Flow_ast.Expression.t ->
  Loc.t -> (Loc.t * string) option -> Type.t ->
  t -> t

val add_complex: Context.t ->
  expr:(
    Context.t -> (Loc.t, Loc.t) Flow_ast.Expression.t ->
    (Loc.t, Loc.t * Type.t) Flow_ast.Expression.t
  ) ->
  ?default: (Loc.t, Loc.t) Flow_ast.Expression.t ->
  (Loc.t, Loc.t) Flow_ast.Pattern.t -> Type.t ->
  t ->
  t * (Loc.t, Loc.t * Type.t) Flow_ast.Pattern.t

val add_rest: Context.t ->
  Loc.t -> (Loc.t * string) option -> Type.t ->
  t -> t

(* (name, type) of each param, in order *)
(* destructured params will be unnamed *)
val value: t -> (string option * Type.t) list

(* The rest param *)
val rest: t -> (string option * Loc.t * Type.t) option

(* iterates over all bindings, traversing through any destructued
   bindings as well, in source order of declaration *)
val iter: (binding -> unit) -> t -> unit

val subst: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t -> t
