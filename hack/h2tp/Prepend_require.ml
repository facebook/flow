(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast

(*
  Create a require_once statement that can be inserted into the ast.
*)
let prepare_require src =
  let p = Pos.make_from src in
  Stmt (Expr (p,
    Import (RequireOnce, (p,
      Array_get (
      (p, Lvar (p, "$GLOBALS")),
      Some (p, String (p, "HACKLIB_ROOT"))
    )))
  ))

(*
  The purpose of this function is to insert a require_statement to the hacklib.

  In php, the first statement in a file should be a namespace declaration if there
  is one.
  We prepare a require_once to be inserted into the file, and then search for the
  first namespace declaration we can insert it into. If there are none,
  we just prepend it to the top of the file.
*)
let prepend ast src =
  let require_elem = prepare_require src in
  let try_insert (defs, did_insert) def =
    match (did_insert, def) with
    | (false, Namespace (id, prog)) ->
        ((Namespace (id, require_elem::prog))::defs, true)
    | _ -> (def::defs, did_insert) in
  let (ast, did_insert) = List.fold_left try_insert ([], false) ast in
  let ast = List.rev ast in
  if did_insert then ast else require_elem::ast
