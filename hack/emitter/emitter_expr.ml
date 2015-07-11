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
open Utils

open Emitter_core


let is_lval expr =
  match expr with
  (* XXX: other lvals! *)
  | Lvar _  | Obj_get _ | Array_get _ -> true
  | _ -> false

let is_empty_block = function
  | [] | [Noop] -> true
  | _ -> false

(* Emit a conditional branch based on an expression. jump_if indicates
 * whether to jump when the condition is true or false.
 * This is its own thing so that we can efficiently handle !/&&/||
 * by directly incorporating their behavior into the branching rather than
 * computing their results as intermediate values. *)
let rec emit_cond env (_, expr_ as expr) jump_if target =
  match expr_ with
  | Unop (Ast.Unot, e) -> emit_cond env e (not jump_if) target
  | Binop (Ast.AMpamp, e1, e2) ->
    emit_shortcircuit env e1 e2 jump_if target false
  | Binop (Ast.BArbar, e1, e2) ->
    emit_shortcircuit env e1 e2 jump_if target true
  | _ ->
    let env = emit_expr env expr in
    emit_cjmp env jump_if target

(* Helper for emit_cond that emits a shortcircuiting conditional
 * (&&/||) since it turns out that && and || are duals or some such.
 * Arguments are the same as emit_cond except for shortcircuit_on which
 * indicates which truth value triggers shortcircuiting *)
and emit_shortcircuit env e1 e2 jump_if target shortcircuit_on =
  if jump_if = shortcircuit_on then
  (* The value that we shortcircuit on matches the value we jump on,
   * so just emit both sides, jumping to the target if it matches *)
  let env = emit_cond env e1 jump_if target in
  emit_cond env e2 jump_if target
  else
  (* We shortcircuit on the *opposite* value from the one we jump on,
   * so emit a label after the second expr and have the first conditional
   * jump to that when short circuiting *)
  let env, shortcircuit_label = fresh_label env in
  let env = emit_cond env e1 shortcircuit_on shortcircuit_label in
  let env = emit_cond env e2 jump_if target in
  emit_label env shortcircuit_label

and emit_member env (_, expr_ as expr) =
  match expr_ with
  | Lvar id -> env, Mlocal id
  | Int (_, n) -> env, Mint n
  | String (_, s) -> env, Mstring s
  | _ ->
    let env = emit_expr env expr in
    env, Mexpr

and emit_base env (_, expr_ as expr) =
  match expr_ with
  | This -> env, Bthis
  | _ when is_lval expr_ ->
    let env, lval = emit_lval env expr in
    env, Blval lval
  | _ ->
    let env = emit_expr env expr in
    env, Bexpr

(* emit code for an lval that we may be either reading or writing from
 * and return a descriptor for the lval.
 * May push values on the stack that will be consumed when the lval
 * is accessed. *)
and emit_lval env (_, expr_) =
  match expr_ with
  | Lvar id -> env, Llocal id
  | Array_get (e1, maybe_e2) ->
    let env, base = emit_base env e1 in
    let env, member = (match maybe_e2 with
      | Some e2 -> emit_member env e2
      | None -> env, Mappend) in
    env, lmember (base, (MTelem, member))
  | Obj_get (e1, (_, Id (_, id)), _null_flavor_TODO) ->
    let env, base = emit_base env e1 in
    env, lmember (base, (MTprop, Mstring id))

  | Obj_get _ -> unimpl "Obj_get with non-Id rhs???"; assert false

  | _ -> bug "emit_lval: not an lvalue"; assert false

and emit_call_lhs env (_, expr_ as expr) nargs =
  match expr_ with
  | Id (_, name)
  | Fun_id (_, name) -> emit_FPushFuncD env nargs (strip_ns name)
  | Obj_get (obj, (_, Id (_, name)), null_flavor) ->
    let env = emit_method_base env obj in
    emit_FPushObjMethodD env nargs name (fmt_null_flavor null_flavor)
  (* what all is even allowed here? *)
  | _ ->
    let env = emit_expr env expr in
    emit_FPushFunc env nargs

and emit_method_base env (_, expr_ as expr) =
  match expr_ with
  | This -> emit_This env
  | _ -> emit_expr env expr

and emit_ctor_lhs env class_id nargs =
  match class_id with
  | CI (_, name) -> emit_FPushCtorD env nargs (strip_ns name)
  | _ -> unimpl "unsupported constructor lhs"; assert false

(* emit code to push args and call a function after the thing being
 * called has already been pushed; doesn't do any stack adjustment
 * after the call *)
and emit_call env args uargs =
  let all_args = args @ uargs in
  let nargs = List.length all_args in
  let env, _ = List.fold_left begin fun (env, i) arg ->
      let env = if is_lval (snd arg) then
        let env, lval = emit_lval env arg in
        emit_FPassLval env i lval
      else
        let env, flavor = emit_flavored_expr env arg in
        emit_FPass env flavor i
      in
      env, i+1
    end (env, 0) all_args in
  if uargs = []
  then emit_FCall env nargs
  else emit_FCallUnpack env nargs

(* emit code to evaluate an expression and discard the result *)
and emit_ignored_expr env e =
  let env, flavor = emit_flavored_expr env e in
  emit_Pop env flavor

(* emit code to evaluate an expression that might have a non-Cell flavor;
 * certain operations want to handle this; most will just call
 * emit_expr or emit_ignored_expr with will automatically unbox/pop
 * R flavored things. *)
and emit_flavored_expr env (_, expr_ as expr) =
  match expr_ with
  | Call (Cnormal, ef, args, uargs) ->
    let nargs = List.length args + List.length uargs in
    let env = emit_call_lhs env ef nargs in
    let env = emit_call env args uargs in
    env, FR
  | Call (Cuser_func, _, _, _) -> unimpl "call_user_func"; assert false

  (* For most things, just fall back to emit_expr *)
  | _ -> emit_expr env expr, FC

(* emit code to evaluate an expression;
 * doesn't rely on the incoming stack state, leaves the value of the
 * expression on the stack as a cell. *)
and emit_expr env (_, expr_ as expr) =
  match expr_ with
  (* Calls produce flavor R, so emit it and then unbox *)
  | Call (_, _, _, _) ->
    let env, flavor = emit_flavored_expr env expr in
    assert (flavor = FR);
    emit_UnboxR env

  (* N.B: duplicate with is_lval but we want to exhaustiveness check  *)
  | Lvar _
  | Obj_get _
  | Array_get _ ->
    let env, lval = emit_lval env expr in
    emit_CGet env lval

  (* Assignment is technically a binop, although it is weird. *)
  | Binop (Ast.Eq _, (_, List _), _) ->
    unimpl "destructuring assignment"; assert false

  | Binop (Ast.Eq obop, e1, e2) ->
    let env, lval = emit_lval env e1 in
    let env = emit_expr env e2 in
    (match obop with
    | None -> emit_Set env lval
    | Some bop -> emit_SetOp env lval (fmt_eq_binop bop))

  | Binop ((Ast.AMpamp | Ast.BArbar) as bop, _, _) ->
    (* emit_cond generates better code when the jump is being done on
     * the value that makes the operator short circuit; so we arrange to
     * conditionally jump on false for && and true for || *)
    let jump_on = bop = Ast.BArbar in

    let env, jump_label, end_label = fresh_labels_2 env in
    let env = emit_cond env expr jump_on jump_label in
    let env = emit_bool env (not jump_on) in
    let env = emit_Jmp env end_label in
    let env = emit_label env jump_label in
    let env = emit_bool env jump_on in
    emit_label env end_label

  | Eif (etest, Some etrue, efalse) ->
    let env, false_label, end_label = fresh_labels_2 env in
    (* emit test, jumping to false_label on false *)
    let env = emit_cond env etest false false_label in

    (* emit true value and jump past false *)
    let env = emit_expr env etrue in
    let env = emit_Jmp env end_label in
    (* emit false value *)
    let env = emit_label env false_label in
    let env = emit_expr env efalse in
    emit_label env end_label

  | Eif (etest, None, efalse) ->
    let env, end_label = fresh_label env in
    (* emit the test expression;
     * we don't use emit_cond because we have to Dup it *)
    let env = emit_expr env etest in
    let env = emit_Dup env in
    (* jump to the end if it is true and we want to use the dup'd val *)
    let env = emit_cjmp env true end_label in

    (* pop the dup'd value and emit the false case *)
    let env = emit_PopC env in
    let env = emit_expr env efalse in

    emit_label env end_label

  (* Normal binops *)
  | Binop (bop, e1, e2) ->
    let env = emit_expr env e1 in
    let env = emit_expr env e2 in
    emit_binop env bop

  (* ~ and ! are the only unops that are handled with a unop opcode *)
  | Unop (Ast.Utild, e) ->
    let env = emit_expr env e in
    emit_BitNot env
  | Unop (Ast.Unot, e) ->
    let env = emit_expr env e in
    emit_BitNot env
  (* unary + and - use math with zero *)
  | Unop (Ast.Uplus, e) ->
    let env = emit_Int env "0" in
    let env = emit_expr env e in
    emit_binop env (Ast.Plus)
  | Unop (Ast.Uminus, e) ->
    let env = emit_Int env "0" in
    let env = emit_expr env e in
    emit_binop env (Ast.Minus)
  (* all the rest are pre/post inc/dec *)
  | Unop (uop, e) ->
    let env, lval = emit_lval env e in
    emit_IncDec env lval (fmt_inc_dec_unop uop)

  | New (cls, args, uargs) ->
    let nargs = List.length args + List.length uargs in
    let env = emit_ctor_lhs env cls nargs in
    let env = emit_call env args uargs in
    emit_PopR env

  (* comma operator: evaluate all the expressions, ignoring all but the last *)
  | Expr_list es ->
    (match List.rev es with
    | [] -> env
    | last :: rest ->
      let env =
        List.fold_right (fun e env -> emit_ignored_expr env e) rest env in
      emit_expr env last)

  | Int (_, n) -> emit_Int env (fmt_int n)
  | Float (_, x) -> emit_Float env (fmt_float x)
  | String (_, s)
  | String2 ([], s) -> emit_String env s
  | Null -> emit_Null env
  | True -> emit_bool env true
  | False -> emit_bool env false

  (* TODO: lots of ways to be better;
   * use NewStructArray/NewArray/NewPackedArray/array lits *)
  (* strict actually disallows mixing field types *)
  | Array afields ->
    let env = emit_NewMixedArray env (List.length afields) in
    let emit_afield env = function
      | AFvalue e ->
        let env = emit_expr env e in
        emit_AddNewElemC env
      | AFkvalue (ek, ev) ->
        let env = emit_expr env ek in
        let env = emit_expr env ev in
        emit_AddElemC env
    in
    List.fold_left emit_afield env afields

  | Clone e ->
    let env = emit_expr env e in
    emit_Clone env

  | Any | Lplaceholder _ -> bug "what even is this"; assert false
  | List _ -> bug "list on RHS"; assert false

  | String2 _ -> unimpl "String2 going to take ast changes"; assert false

  (* XXX: is this right?? *)
  | This -> emit_BareThis env "Notice"

  | Cast (h, e) ->
    let env = emit_expr env e in
    emit_cast env h

  | Shape _
  | ValCollection _
  | KeyValCollection _
  | Pair _

  | Id _
  | Fun_id _
  | Method_id _
  | Method_caller _
  | Smethod_id _
  | Class_get _
  | Class_const _
  | Special_func _
  | Yield_break
  | Yield _
  | Await _
  | InstanceOf _
  | Efun _
  | Xml _
  | Assert _
    -> unimpl "expression"; assert false
