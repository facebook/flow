(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t

val create : subpaths:Export_condition_map.t SMap.t -> t

val resolve_package : t -> string -> string list -> string option

val parse : ('a, 'b) Flow_ast.Expression.t' -> t option
