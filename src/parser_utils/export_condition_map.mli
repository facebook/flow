(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

type 'a condition_value

val empty : t

val create : conditions:(string * t condition_value) list -> t

val create_from_shorthand : path:string -> t

val resolve_package_target : t -> string option -> string list -> string option

val parse : ('a, 'b) Flow_ast.Expression.t' -> t option
