(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Nast

(* this should never be exposed / thrown outside of this module; translate
 * it into a result type first *)
exception Not_static_exn of Pos.t

let rec static_string_exn = function
  | _, Binop (Ast.Dot, s1, s2) ->
    let s1 = static_string_exn s1 in
    let s2 = static_string_exn s2 in
    s1 ^ s2
  | _, String (_p, s) -> s
  | p, _ -> raise (Not_static_exn p)

let static_string expr =
  try Result.Ok (static_string_exn expr)
  with Not_static_exn p -> Result.Error p
