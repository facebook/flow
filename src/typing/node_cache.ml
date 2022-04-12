(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections

type t = {
  mutable annotations: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation ALocMap.t;
  mutable expressions: (ALoc.t, ALoc.t * Type.t) Ast.Expression.t ALocMap.t;
  mutable functions: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t) ALocMap.t;
}

let empty = { annotations = ALocMap.empty; expressions = ALocMap.empty; functions = ALocMap.empty }

let set_annotation cache ((loc, _) as anno) =
  cache.annotations <- ALocMap.add loc anno cache.annotations

let set_expression cache (((loc, _), _) as exp) =
  cache.expressions <- ALocMap.add loc exp cache.expressions

let set_function cache loc fn = cache.functions <- ALocMap.add loc fn cache.functions

let get_annotation cache loc = ALocMap.find_opt loc cache.annotations

let get_expression cache loc = ALocMap.find_opt loc cache.expressions

let get_function cache loc = ALocMap.find_opt loc cache.functions
