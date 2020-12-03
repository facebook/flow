(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Constraint

(* These possible_* functions would ideally be in constraint.ml, but since they use
 * Context and Context depends on Constraint we need to extract these functions
 * to a separate module in order to avoid a circular dependency *)

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of (Context.find_graph cx id) |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let possible_uses cx id = uses_of (Context.find_graph cx id) |> List.filter is_proper_use
