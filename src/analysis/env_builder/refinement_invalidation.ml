(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

type reason =
  | FunctionCall
  | ConstructorCall
  | PropertyAssignment
  | Await
  | Yield

type t = reason ALocMap.t

let singleton ~loc ~reason = ALocMap.singleton loc reason

let merge = function
  | (None, None) -> None
  | (Some m, None)
  | (None, Some m) ->
    Some m
  | (Some m1, Some m2) -> Some (ALocMap.union m1 m2)
