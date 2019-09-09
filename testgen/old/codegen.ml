(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Main module for generating code *)
module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils
module Config = Flowtestgen_config
module FTypes = Flowtestgen_types
module FRandom = Utils.FRandom
open Code

(* Check the expression is of the given type *)
let mk_type_assertion (etype : T.t') (expr : t') : t =
  (* Make a variable declaration first *)
  let final_id = E.Identifier (Loc.none, "t") in
  let final_decl = Mutator.mk_vardecl_code "t" (Some etype) (Some expr) in
  let callee = E.Identifier (Loc.none, "assert_type") in
  let expected = FTypes.mk_literal_expr etype in
  let arguments = [E.Expression (Loc.none, final_id); E.Expression (Loc.none, expected.expr)] in
  let call = E.Call.(E.Call { callee = (Loc.none, callee); arguments }) in
  { expr = call; expr_deps = [final_decl] } |> Mutator.mk_expr_code

(* We move function definitions to the end at random *)
let shuffle_fun_defs (code : t) : t =
  let rec move_fun_defs stmt_acc fun_acc slist =
    match slist with
    | [] -> List.rev stmt_acc @ List.rev fun_acc
    | hd :: tl ->
      (match hd with
      | (_, S.FunctionDeclaration _) when FRandom.rbool () ->
        (* We move function defs to the end *)
        move_fun_defs stmt_acc (hd :: fun_acc) tl
      | _ -> move_fun_defs (hd :: stmt_acc) fun_acc tl)
  in
  (* Move some functions to the end *)
  let result = Utils.list_of_code code |> move_fun_defs [] [] |> Utils.code_of_stmt_list in
  match result with
  | None -> failwith "This cannot be None."
  | Some s -> s

(* Widen the type and mutate the value *)
let rec mk_widen_and_mutation
    (obj_name : string) (prop_name : string) (etype : T.t') (prev_stmt : t) : T.t' * t =
  (* Widen the type *)
  let new_t = Widener.widen_type etype in
  if new_t = etype then
    (etype, prev_stmt)
  else
    (* mutate the value *)
    let f =
      match new_t with
      | T.Array _ -> Mutator.mk_array_mutation
      | _ -> FRandom.choice [|Mutator.mk_assignment_mutation; Mutator.mk_func_mutation|]
    in
    let new_stmt = f new_t obj_name prop_name prev_stmt in
    if FRandom.rbool () then
      (new_t, new_stmt)
    else
      (* We continue to widen & mutate at random *)
      mk_widen_and_mutation obj_name prop_name new_t new_stmt

(* Make a code. It only makes literals and property reads *)
let mk_random_code () : t =
  FRandom.init_hist ();
  let etype = FTypes.random_type () in
  (* wrap the initial values into a new object *)
  let obj_name = Utils.mk_var () in
  let prop_name = Utils.mk_prop () in
  let prop_read = Mutator.mk_prop_read obj_name prop_name in
  let obj_decl = Mutator.mk_objdecl_code prop_read etype in
  (* Widen the type and mutate the property *)
  let (_, widening) = mk_widen_and_mutation obj_name prop_name etype obj_decl in
  (* Check the type *)
  let read_expr = { expr = E.Member prop_read; expr_deps = [widening] } in
  mk_type_assertion etype read_expr |> shuffle_fun_defs
