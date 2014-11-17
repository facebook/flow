(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
  This is to deal with the fact that all boolean checks could potentially contain
  collections. Empty collections in hack are falsey and there is no analogue for
  regular php objects being falsey. Hence this transformation looks for places
  where we might be dealing with a collection and call into our library function
  hacklib_cast_as_boolean.
  Example:
    old: (bool) (ImmVector {});
    new: (bool) (!\hacklib_id(new ImmVector(array()))->isEmpty());
    old: (bool) $v;
    new: (bool) \hacklib_cast_as_boolean($v);
*)

module M = Map_ast
module CE = Common_exns
open Ast
open Ast_ext
open Utils


let cast_known_collection p expr_ =
  call_inst_func p (p, expr_) "isEmpty" [] |> negate

(**
 * This is used when the expression or statement containing this expression
 * forces a cast to boolean. Here depending upon the type of expression we
 * either leave the expression alone, or explicitly cast it to bool.
 *)
let rec cast_as_boolean (p, expr_) = match expr_ with
  | Expr_list es -> (p, Expr_list (handle_expr_list es))
  | _ -> match is_collection_expr_ expr_ with
          | Some true -> cast_known_collection p expr_
          | None -> call_func p "\\hacklib_cast_as_boolean" [(p, expr_)]
          | Some false -> (p, expr_)
and handle_expr_list = function
  | [] -> []
  | [e] -> [cast_as_boolean e]
  | e1::es -> e1::(handle_expr_list es)

let modify_bool_expr_ = function
   | Cast ((p1, Happly ((p2, hint), [])), expr)
      when List.mem hint ["bool"; "boolean"] ->
      Cast ((p1, Happly ((p2, hint), [])), (cast_as_boolean expr))
   | Binop (bop, e1, e2)
      when List.mem bop [AMpamp; BArbar;] ->
      Binop (bop, (cast_as_boolean e1), (cast_as_boolean e2))
   | Unop (Unot, e) -> Unop (Unot, cast_as_boolean e)
   | Eif (e1, e2, e3) -> Eif (cast_as_boolean e1, e2, e3)
   (* in zend, empty can only be called on variables, array access or property access *)
   | Call ((p1, Id (p2, "empty")), [(p3, e3)], [])
        when is_collection_expr_ e3 <> Some false ->
          Binop (
            BArbar,
            negate (call_func p1 "isset" [(p3, e3)]),
            (p2, Binop (Eqeq, (p3, e3), (p3, False)))
          )
   | e -> e

let modify_bool_stmt = function
  | For (e1, e2, e3, block) ->
      For (e1, (cast_as_boolean e2), e3, block)
  | If (e, thenBlock, elseBlock) ->
      If (cast_as_boolean e, thenBlock, elseBlock)
  | Do (block, e) -> Do (block, cast_as_boolean e)
  | While (e, block) -> While (cast_as_boolean e, block)
  | s -> s

(*
 * Unlike other mappers, this one works by first calling the transformation on
 * sub expressions and then choosing what to do. This is done so as to prevent
 * calling hacklib_cast_as_boolean twice in a row.
 *)
let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_expr_ = (fun (k, _) expr_ -> modify_bool_expr_ (k expr_));
    M.k_stmt = (fun (k, _) stmt -> modify_bool_stmt (k stmt));
  }
