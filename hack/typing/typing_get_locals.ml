(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Core
open Utils

module FuncTerm = Typing_func_terminality

(* Module calculating the locals for a statement
 * This is useful when someone uses $x on both sides
 * of an If statement, for example:
 * if(true) {
 *   $x = 0;
 * } else {
 *   $x = 1;
 * }
 *)

(* TODO It really sucks that this and Nast_terminality.Terminal are very
 * slightly different (notably, this version is somewhat buggier). Fixing that
 * exposes a lot of errors in www unfortunately -- we should bite the bullet on
 * fixing switch all the way when we do that, most likely though -- see tasks
 * #3140431 and #2813555. *)
let rec terminal tcopt nsenv ~in_try stl =
  List.iter stl (terminal_ tcopt nsenv ~in_try)

and terminal_ tcopt nsenv ~in_try = function
  | Throw _ when not in_try -> raise Exit
  | Throw _ -> ()
  | Continue _
  | Expr (_, (Call ((_, Id (_, "assert")), [_, False], [])
                 | Call ((_, Id (_, "invariant")), (_, False) :: _ :: _, [])))
  | Return _ -> raise Exit
  | Expr (_, Call ((_, Id fun_id), _, _)) ->
    let _, fun_name = Namespaces.elaborate_id nsenv NSFun fun_id in
    FuncTerm.(raise_exit_if_terminal (get_fun tcopt fun_name))
  | Expr (_, Call ((_, Class_const (cls_id, (_, meth_name))), _, _))
    when (snd cls_id).[0] <> '$' ->
    let _, cls_name = Namespaces.elaborate_id nsenv NSClass cls_id in
    FuncTerm.(raise_exit_if_terminal
      (get_static_meth tcopt cls_name meth_name))
  | If (_, b1, b2) ->
    (try terminal tcopt nsenv ~in_try b1; () with Exit ->
      terminal tcopt nsenv ~in_try b2)
  | Switch (_, cl) ->
    terminal_cl tcopt nsenv ~in_try cl
  | Block b -> terminal tcopt nsenv ~in_try b
  | Try (b, catch_l, _fb) ->
    (* return is not allowed in finally, so we can ignore fb *)
    (terminal tcopt nsenv ~in_try:true b;
     List.iter catch_l (terminal_catch tcopt nsenv ~in_try))
  | Do _
  | While _
  | For _
  | Foreach _
  | Noop
  | Expr _
  | Unsafe
  | Fallthrough
  | Break _ (* TODO this is terminal sometimes too, except switch, see above. *)
  | Static_var _ -> ()

and terminal_catch tcopt nsenv ~in_try (_, _, b) =
  terminal tcopt nsenv ~in_try b

and terminal_cl tcopt nsenv ~in_try = function
  | [] -> raise Exit
  | Case (_, b) :: rl ->
    (try
      terminal tcopt nsenv ~in_try b;
      if List.exists b (function Break _ -> true | _ -> false)
      then ()
      else raise Exit
    with Exit -> terminal_cl tcopt nsenv ~in_try rl)
  | Default b :: rl ->
    begin try terminal tcopt nsenv ~in_try b with
      | Exit ->
        terminal_cl tcopt nsenv ~in_try rl
    end

let is_terminal tcopt nsenv stl =
  try terminal tcopt nsenv ~in_try:false stl; false
  with Exit -> true

let smap_union ((nsenv:Namespace_env.env), (m1:Pos.t SMap.t))
    (m2:Pos.t SMap.t) =
  let m_combined = SMap.fold SMap.add m1 m2 in
  nsenv, m_combined

let rec lvalue tcopt (acc:(Namespace_env.env * Pos.t SMap.t)) = function
  | (p, Lvar (_, x)) ->
    let nsenv, m = acc in
    nsenv, SMap.add x p m
  | _, List lv -> List.fold_left lv ~init:acc ~f:(lvalue tcopt)
  (* Ref forms a local inside a foreach *)
  | (_, Unop (Uref, (p, Lvar (_, x)))) ->
    let nsenv, m = acc in
    nsenv, SMap.add x p m
  | _ -> acc

let rec stmt tcopt (acc:(Namespace_env.env * Pos.t SMap.t)) st =
  let nsenv = fst acc in
  match st with
  | Expr (_, Binop (Eq None, lv, rv))
  | Expr (_, Eif ((_, Binop (Eq None, lv, rv)), _, _)) ->
    let acc = stmt tcopt acc (Expr rv) in
    lvalue tcopt acc lv
  | Unsafe
  | Fallthrough
  | Expr _ | Break _ | Continue _ | Throw _
  | Do _ | While _ | For _ | Foreach _
  | Return _ | Static_var _ | Noop -> acc
  | Block b -> block tcopt acc b
  | If (_, b1, b2) ->
    let term1 = is_terminal tcopt nsenv b1 in
    let term2 = is_terminal tcopt nsenv b2 in
    if term1 && term2
    then acc
    else if term1
    then
      let _, m2 = block tcopt (nsenv, SMap.empty) b2 in
      smap_union acc m2
    else if term2
    then
      let _, m1 = block tcopt (nsenv, SMap.empty) b1 in
      smap_union acc m1
    else begin
      let _, m1 = block tcopt (nsenv, SMap.empty) b1 in
      let _, m2 = block tcopt (nsenv, SMap.empty) b2 in
      let (m:Pos.t SMap.t) = (smap_inter m1 m2) in
      smap_union acc m
    end
  | Switch (_e, cl) ->
    let cl = List.filter cl begin function
      | Case (_, b)
      | Default b -> not (is_terminal tcopt nsenv b)
    end in
    let cl = casel tcopt nsenv cl in
    let c = smap_inter_list cl in
    smap_union acc c
  | Try (b, cl, _fb) ->
    let _, c = block tcopt (nsenv, SMap.empty) b in
    let cl = List.filter cl begin fun (_, _, b) ->
      not (is_terminal tcopt nsenv b)
    end in
    let lcl = List.map cl (catch tcopt nsenv) in
    let c = smap_inter_list (c :: lcl) in
    smap_union acc c

and block tcopt acc l = List.fold_left l ~init:acc ~f:(stmt tcopt)

and casel tcopt nsenv = function
  | [] -> []
  | Case (_, []) :: rl -> casel tcopt nsenv rl
  | Default b :: rl
  | Case (_, b) :: rl ->
      let _, b = block tcopt (nsenv, SMap.empty) b in
      b :: casel tcopt nsenv rl

and catch tcopt nsenv (_, _, b) =
  snd (block tcopt (nsenv, SMap.empty) b)
