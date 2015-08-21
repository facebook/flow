(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Nast

type eval_error =
  | Not_static of Pos.t
  | Type_error (* type errors are already handled by naming / typing code,
                * so the caller can usually ignore them *)

(* these should never be exposed / thrown outside of this module; translate
 * them into eval_error types first *)
exception Not_static_exn of Pos.t
exception Type_error_exn

let rec static_string_exn ~allow_consts cclass = function
  | _, Binop (Ast.Dot, s1, s2) ->
      let p1, s1 = static_string_exn ~allow_consts cclass s1 in
      let _p2, s2 = static_string_exn ~allow_consts cclass s2 in
      p1, s1 ^ s2
  | _, String (p, s) -> p, s
  | p, Class_const (CIself, (_, s)) when allow_consts -> begin
      match cclass with
      | None -> raise Type_error_exn
      | Some class_ ->
          (* TODO: return a list of positions so we can pinpoint lint errors
           * to the exact constant in which they occurred *)
          p, snd (get_const s cclass class_)
    end
  | p, Class_const (CI (_, id), (_, s)) when allow_consts -> begin
      match Naming_heap.ClassHeap.get id with
      | None -> raise Type_error_exn
      | Some class_ -> p, snd (get_const s cclass class_)
    end
  | p, _ -> raise (Not_static_exn p)

and get_const s cclass class_ =
  let (_, (p, _), cst_expr_opt) =
    try
      List.find_exn class_.c_consts ~f:(fun (_, (_, id), _) -> id = s)
    with Not_found ->
      raise Type_error_exn in
  match cst_expr_opt with
  | Some cst_expr -> static_string_exn ~allow_consts:true cclass cst_expr
  | None -> raise (Not_static_exn p)

let static_string ~allow_consts cclass expr =
  try Result.Ok (static_string_exn ~allow_consts cclass expr) with
  | Type_error_exn -> Result.Error Type_error
  | Not_static_exn p -> Result.Error (Not_static p)

(* e.g. user attributes do not support consts *)
let static_string_no_consts = static_string ~allow_consts:false None

let static_string = static_string ~allow_consts:true
