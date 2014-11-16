(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This converts collections that have value semantics, which are tuples and
  shapes to arrays (since that is how they behave).
    Example:
      old: $x = tuple(1, "hello", true);
      new: $x = array(1, "hello", true);
    Example:
      old: $x = shape('foo' => 10, 'bar' => "ten");
      new: $x = array('foo' => 10, 'bar' => "ten");
*)


module M = Map_ast
open Ast

let make_shape_field = function
  | (SFlit ((p, _) as pstring) , expr) -> AFkvalue ((p, String pstring), expr)
  | (SFclass_const ((p, _) as id, pstring), expr) ->
      AFkvalue ((p, Class_const (id, pstring)), expr)

let replace_tuples = function
  | Call ((_, Id (_, "tuple")), args, []) ->
          Array (List.map (fun a -> AFvalue a) args)
  | Shape args ->
          Array (List.map make_shape_field args)
  | e -> e

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_expr_ = fun (k, _) expr_ -> k (replace_tuples expr_);
  }
