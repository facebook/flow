(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module M = Map_ast
open Ast
open Ast_ext

(* miscellenaous simple transforms :
    1. wraps a call to a class constructor within a function,
       if its nested within some expressions
       Example:
        old: (new Foo())->bar()
        new: \\hacklib_id(new Foo())->bar().
    2. automatically converts nullsafe calls to explicitly call
       a nullsafe function.
       Example:
        old: $foo?->bar()
        new: \\hacklib_nullsafe($foo)->bar()
*)

let process_expr (k, _) = function
  | Obj_get ((p, _) as v1, v2, OG_nullsafe) ->
      let new_obj = call_func p "\\hacklib_nullsafe" [v1] in
      k (Obj_get (new_obj, v2, OG_nullthrows))
  | Obj_get ((p, New (klass, pargs, uargs)), v2, OG_nullthrows) ->
      let new_call = (p, New (klass, pargs, uargs)) in
      k (Obj_get (call_func p "\\hacklib_id" [new_call], v2, OG_nullthrows))
  | e -> k e

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_expr_ = process_expr;
  }
