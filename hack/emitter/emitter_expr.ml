(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Code for emitting expressions and various related forms (like lvalues) *)

open Core
open Utils
open Nast

open Emitter_core
module SN = Naming_special_names

let is_xhp_prop = function | _, Id (_, s) -> s.[0] = ':' | _ -> false

let is_lval expr =
  match expr with
  | Lvar _  | Array_get _ | Class_get _ | Lplaceholder _ -> true
  | Obj_get (_, prop, _) -> not (is_xhp_prop prop)
  | _ -> false

let resolve_class_id env = function
  | CIparent ->
      Option.value_map env.parent_name
        ~default:(RCdynamic `parent) ~f:(fun x -> RCstatic x)
  | CIself ->
      Option.value_map env.self_name
        ~default:(RCdynamic `self) ~f:(fun x -> RCstatic x)
  | CI (_, s) -> RCstatic (fmt_name s)
  | CIstatic -> RCdynamic `static
  | CIexpr e -> RCdynamic (`var e)

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
  (* handling true/false makes things like while (true) nicer *)
  (* PERF: maybe we should check for other manifestly true/false things?
   * is while (1) an idiom? *)
  | True | False ->
    let cond_true = expr_ = True in
    if cond_true = jump_if then
    emit_Jmp env target else
    env
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

and emit_expr_to_var env (_, expr_ as expr) =
  match expr_ with
  | Lvar id -> env, None, get_lid_name id
  | _ ->
    let env = emit_expr env expr in
    let env, id = fresh_tempvar env in
    let env = emit_SetL env id in
    let env = emit_PopC env in
    let env, label = fresh_faultlet env in
    env, Some label, id

and emit_member env (_, expr_ as expr) =
  match expr_ with
  | Lvar id -> env, Mlocal (get_lid_name id)
  | Int (_, n) -> env, Mint n
  | String (_, s) -> env, Mstring s
  | _ ->
    let env = emit_expr env expr in
    env, Mexpr

and emit_base env (_, expr_ as expr) =
  match expr_ with
  | This -> env, Bthis
  | _ when is_lval expr_ ->
    let env, lval = emit_lval_inner env expr in
    env, Blval lval
  | _ ->
    let env = emit_expr env expr in
    env, Bexpr

(* This is the main guts of lval emitting. It does all the work of
 * eval emitting except for emitting the class id when operating
 * on a static property. This is because for member instructions on
 * static props, the class id needs to come *last* on the stack,
 * after everything else. (Argh.) So we save it and emit it
 * at the "top level" of lval emitting.  *)
and emit_lval_inner env (_, expr_) =
  match expr_ with
  (* superglobals *)
  | Lvar id when SN.Superglobals.is_superglobal (get_lid_name id) ->
    let env = emit_String env (lstrip (get_lid_name id) "$") in
    env, Lglobal
  (* indexes into $GLOBALS: ignore the $GLOBALS and emit the index
   * as the global  *)
  | Array_get ((_, Lvar id), Some e)
      when (get_lid_name id = SN.Superglobals.globals) ->
    let env = emit_expr env e in
    env, Lglobal

  | Lvar id -> env, llocal id
  | Lplaceholder _ -> env, Llocal SN.SpecialIdents.placeholder
  | Array_get (e1, maybe_e2) ->
    let env, base = emit_base env e1 in
    let env, member = (match maybe_e2 with
      | Some e2 -> emit_member env e2
      | None -> env, Mappend) in
    env, lmember (base, (MTelem, member))
  | Obj_get (e1, (_, Id (_, id)), null_flavor) ->
    let env, base = emit_base env e1 in
    env, lmember (base, (MTprop null_flavor, Mstring id))
  | Obj_get _ -> unimpl "Obj_get with non-Id rhs???"

  | Class_get (cid, (_, field)) ->
    let field_name = lstrip field "$" in
    let env = emit_String env field_name in
    env, Lsprop cid

  | _ -> bug "emit_lval: not an lvalue"

(* emit code for an lval that we may be either reading or writing from
 * and return a descriptor for the lval.
 * May push values on the stack that will be consumed when the lval
 * is accessed. *)
and emit_lval env expr =
  let env, lval = emit_lval_inner env expr in
  (* If we are operating on a static prop, emit the class id now,
   * which needs to come last... *)
  let env = match lval with
    | Lsprop cid
    | Lmember (Blval (Lsprop cid), _) ->
      emit_class_id env (resolve_class_id env cid)
    | _ -> env
  in
  env, lval

and emit_dynamic_class_id env = function
  | `parent -> emit_Parent env
  | `self -> emit_Self env
  | `static -> emit_LateBoundCls env
  | `var (_, Lvar id) -> emit_AGetL env (get_lid_name id)
  | `var e ->
    let env = emit_expr env e in
    emit_AGetC env

and emit_class_id env = function
  | RCstatic s ->
    let env = emit_String env s in
    emit_AGetC env
  | RCdynamic dyid -> emit_dynamic_class_id env dyid

and requires_empty_stack = function
  | _, Await _ | _, Yield _ -> true
  | _ -> false

and emit_assignment env obop e1 e2 =
  let env, opt_outer_faultlet, rhs_tag =
    if requires_empty_stack e2 then
      let env, faultlet = fresh_faultlet env in
      let env, temp = fresh_tempvar env in
      let env = emit_expr env e2 in
      let env = emit_SetL env temp in
      let env = emit_PopC env in
      let env = emit_fault_enter env faultlet in
      env, Some faultlet, Some temp
    else
      env, None, None
  in
  let env = match e1 with
  (* Destructuring assignment needs to be handled very differently. *)
  | _, List _ ->
    assert (obop = None);

    (* build up a list of pairings that tell us what assignments to do;
     * we build pairs of (lval, path in array) *)
    let rec collect_assignments path = function
      | _, List es ->
        let collect i e =
          collect_assignments ((MTelem, Mint (string_of_int i)) :: path) e in
        List.concat_mapi ~f:collect es
      | e -> [e, path]
    in
    let assignments = collect_assignments [] e1 in

    (* First we emit all the lvals. We do this first so any effects will
     * happen left to right. *)
    let emit_lhs env (lhs, path) =
      let env, lval = emit_lval env lhs in
      env, (lval, path)
    in
    let env, assignments = List.map_env env assignments emit_lhs in

    (* Store off the rhs to a (maybe temporary) local *)
    let env, opt_faultlet, id = match rhs_tag with
      | None -> emit_expr_to_var env e2
      | Some id -> env, None, id in
    let env = opt_fold emit_fault_enter env opt_faultlet in

    (* Assign to all the components *)
    let base = Blval (Llocal id) in
    let emit_assign (lval, path) env =
      let env = emit_CGet env (Lmember (base, path)) in
      let env = emit_Set env lval in
      emit_PopC env
    in
    (* Assignment is now done right to left, popping whatever we set up
     * off the stack. *)
    let env = List.fold_right ~f:emit_assign ~init:env assignments in

    (* And push the variable back to give the expr its value *)
    let env = if opt_outer_faultlet = None && opt_faultlet = None then
        emit_CGetL env id else
        emit_PushL env id
    in

    (* we need to unset the variable when we leave *)
    let env = opt_fold emit_fault_exit env opt_faultlet in
    let cleanup env = emit_UnsetL env id in
    let env = opt_fold (emit_fault_cleanup ~faultlet_extras:cleanup)
                env opt_faultlet in

    env

  (* Regular assignment is more straightforward. *)
  | _ ->
    let env, lval = emit_lval env e1 in
    let env = match rhs_tag with
      | None -> emit_expr env e2
      | Some tmp -> emit_PushL env tmp in
    (match obop with
    | None -> emit_Set env lval
    | Some bop -> emit_SetOp env lval (fmt_eq_binop bop))

  in

  let env = opt_fold emit_fault_exit env opt_outer_faultlet in
  let cleanup env = emit_UnsetL env (unsafe_opt rhs_tag) in
  let env = opt_fold (emit_fault_cleanup ~faultlet_extras:cleanup)
                env opt_outer_faultlet in

  env


and emit_call_lhs env (_, expr_ as expr) nargs =
  match expr_ with
  | Id (_, name)
  | Fun_id (_, name) -> emit_FPushFuncD env nargs (fmt_name name)
  | Obj_get (obj, (_, Id (_, name)), null_flavor) ->
    let env = emit_method_base env obj in
    emit_FPushObjMethodD env nargs name (fmt_null_flavor null_flavor)

  | Class_const (cid, (_, field)) ->
    let emit_dynamic_call env emit_op =
      let env = emit_String env field in
      let env = emit_class_id env (resolve_class_id env cid) in
      emit_op env nargs
    in

    (match cid with
    | CI (_, s) -> emit_FPushClsMethodD env nargs field (fmt_name s)
    | CIself | CIparent ->
      (* Calls through self:: or parent:: need to be "forwarding"
       * calls that preserve the late static binding "called class".
       * FPushClsMethodF is forwarding, the others aren't *)
      emit_dynamic_call env emit_FPushClsMethodF
    | CIexpr _ | CIstatic ->
      emit_dynamic_call env emit_FPushClsMethod)

  (* what all is even allowed here? *)
  | _ ->
    let env = emit_expr env expr in
    emit_FPushFunc env nargs

and emit_method_base env (_, expr_ as expr) =
  match expr_ with
  | This -> emit_This env
  | _ -> emit_expr env expr

and emit_ctor_lhs env cid nargs =
  match resolve_class_id env cid with
  | RCstatic class_name -> emit_FPushCtorD env nargs class_name
  | RCdynamic dyid ->
    let env = emit_dynamic_class_id env dyid in
    emit_FPushCtor env nargs

(* emit code to push args and call a function after the thing being
 * called has already been pushed; doesn't do any stack adjustment
 * after the call *)
and emit_args_and_call env args uargs =
  let all_args = args @ uargs in
  let nargs = List.length all_args in
  let env = List.foldi ~f:begin fun i env arg ->
      if is_lval (snd arg) then
        let env, lval = emit_lval env arg in
        emit_FPassLval env i lval
      else
        let env, flavor = emit_flavored_expr env arg in
        emit_FPass env flavor i
    end ~init:env all_args in
  if uargs = []
  then emit_FCall env nargs
  else emit_FCallUnpack env nargs

(* emit code to evaluate an expression and discard the result *)
and emit_ignored_expr env e =
  let env, flavor = emit_flavored_expr env e in
  emit_Pop env flavor

(* Just emit a normal call *)
and emit_normal_call env ef args uargs =
  let nargs = List.length args + List.length uargs in
  let env = emit_call_lhs env ef nargs in
  let env = emit_args_and_call env args uargs in
  env, FR

and emit_call env (pos, expr_ as expr) args uargs =
  match expr_, args with
  | Id (_, echo), _ when echo = SN.SpecialFunctions.echo ->
    let nargs = List.length args in
    let env = List.foldi ~f:begin fun i env arg ->
       let env = emit_expr env arg in
       let env = emit_Print env in
       if i = nargs-1 then env else emit_PopC env
    end ~init:env args in
    env, FC
  | Id (_, idx), [_; _] when idx = "\\array_key_exists" ->
    let env = List.fold_left ~f:emit_expr ~init:env args in
    let env = emit_AKExists env in
    env, FC
  | Id (_, idx), ([_; _]|[_; _; _]) when idx = SN.FB.idx ->
    let env = List.fold_left ~f:emit_expr ~init:env args in
    (* If there are two arguments, add Null as a third *)
    let env = if List.length args = 2 then emit_Null env else env in
    let env = emit_Idx env in
    env, FC

  | Id (_, isset), [e] when isset = SN.PseudoFunctions.isset ->
    let env, lval = emit_lval env e in
    emit_Isset env lval, FC
  (* isset on a list is true if they are all true, and shortcircuits.
   * desugar to && *)
  | Id (_, isset), (e::es) when isset = SN.PseudoFunctions.isset ->
    let make_isset arg = pos, Call (Cnormal, expr, arg, []) in
    let desugared = pos, Binop (Ast.AMpamp, make_isset [e], make_isset es) in
    emit_expr env desugared, FC

  | Id (_, empty), [e] when empty = SN.PseudoFunctions.empty ->
    let env, lval = emit_lval env e in
    emit_Empty env lval, FC
  | Id (_, unset), _ when unset = SN.PseudoFunctions.unset ->
    let unset env e =
      let env, lval = emit_lval env e in
      emit_Unset env lval
    in
    let env = List.fold_left ~f:unset ~init:env args in
    (* emit a dummy null so the expression has a return value *)
    emit_Null env, FC

  (* Functions that call set_frame_metadata need an "86metadata" local
   * allocated. *)
  | Id (_, "\\HH\\set_frame_metadata"), _  -> unimpl "set_frame_metadata"
  (* Different variants of call_user_func;
   * see emitCallUserFunc in emitter.cpp *)
  | Id (_, ("\\call_user_func_array" |
            "\\forward_static_call" |
            "\\forward_static_call_array" |
            "\\fb_call_user_func_safe" |
            "\\fb_call_user_func_array_safe" |
            "\\fb_call_user_func_safe_return")), _  ->
    unimpl "call_user_func"


  (* TODO: a billion other builtins *)

  | _ -> emit_normal_call env expr args uargs

(* emit code to evaluate an expression that might have a non-Cell flavor;
 * certain operations want to handle this; most will just call
 * emit_expr or emit_ignored_expr which will automatically unbox/pop
 * R flavored things. *)
and emit_flavored_expr env (pos, expr_ as expr) =
  match expr_ with
  | Call (Cnormal, ef, args, uargs) -> emit_call env ef args uargs
  | Fun_id s ->
      emit_call env (pos, Id (pos, "\\fun")) [fst s, String s] []
  | Method_id (e, s) ->
      emit_call env (pos, Id (pos, "\\inst_meth"))
        [e; fst s, String s] []
  | Smethod_id (s1, s2) ->
      emit_call env (pos, Id (pos, "\\class_meth"))
        [fst s1, String s1; fst s2, String s2] []
  | Method_caller (s1, s2) ->
      emit_call env (pos, Id (pos, "\\meth_caller"))
        [fst s1, String s1; fst s2, String s2] []
  | Call (Cuser_func, _, _, _) -> unimpl "call_user_func"

  | Special_func f ->
    let f, args = match f with
      | Gena e -> SN.FB.fgena, [e]
      | Genva es -> SN.FB.fgenva, es
      | Gen_array_rec e -> SN.FB.fgen_array_rec, [e]
    in
    emit_call env (pos, Id (pos, f)) args []

  (* this is just XHP Obj_get. it is in emit_flavored_expr because it
   * desugars to function calls *)
  | Obj_get (e, prop, nf) when is_xhp_prop prop ->
    let desugared = Emitter_xhp.convert_obj_get pos (e, prop, nf) in
    emit_flavored_expr env desugared

  (* For most things, just fall back to emit_expr *)
  | _ -> emit_expr env expr, FC

(* emit code to evaluate an expression;
 * doesn't rely on the incoming stack state, leaves the value of the
 * expression on the stack as a cell. *)
and emit_expr env (pos, expr_ as expr) =
  match expr_ with
  (* Calls produce flavor R, so emit it and then unbox *)
  | Call (_, _, _, _)
  | Fun_id _ | Method_id _ | Method_caller _ | Smethod_id _
  | Special_func _ ->
    let env, flavor = emit_flavored_expr env expr in
    (* Some builtin functions actually produce C, so we only unbox
     * if it is really an R. *)
    if flavor = FR then emit_UnboxR env else env
  (* XHP props are *not* lvalues *)
  | Obj_get (_, prop, _) when is_xhp_prop prop ->
    let env, flavor = emit_flavored_expr env expr in
    if flavor = FR then emit_UnboxR env else env

  (* N.B: duplicate with is_lval but we want to exhaustiveness check  *)
  | Lplaceholder _
  | Lvar _
  | Obj_get _
  | Array_get _
  | Class_get _ ->
    let env, lval = emit_lval env expr in
    emit_CGet env lval

  (* Assignment is technically a binop, although it is weird. *)
  | Binop (Ast.Eq obop, e1, e2) -> emit_assignment env obop e1 e2

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

  | NullCoalesce (etrue, efalse) ->
      (* Desugar `$a ?? $b` into `isset($a) ? $a : $b` *)
      let c = pos, Call (Cnormal, (pos, (Id (pos, SN.PseudoFunctions.isset))),
                         [etrue], []) in
      let eif = (pos, Eif (c, Some etrue, efalse)) in
      emit_expr env eif

  (* Normal binops *)
  (* HHVM can sometimes evaluate binops (and some other things) right to left
   * if the LHS is a variable. We don't do this (because it seems silly),
   * but it wouldn't be that hard to handle. *Most* situations where it
   * it actually observable are ruled out by the typechecker, but it can
   * be observed with effectful __toString() methods or HH_FIXME. *)
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
    emit_Not env
  (* unary + and - use math with zero *)
  | Unop (Ast.Uplus, e) ->
    let env = emit_Int env "0" in
    let env = emit_expr env e in
    emit_binop env (Ast.Plus)
  | Unop (Ast.Uminus, e) ->
    let env = emit_Int env "0" in
    let env = emit_expr env e in
    emit_binop env (Ast.Minus)
  | Unop (Ast.Uref, _) -> unimpl "references"
  (* all the rest are pre/post inc/dec *)
  | Unop ((Ast.Uincr | Ast.Udecr | Ast.Upincr | Ast.Updecr) as uop, e) ->
    let env, lval = emit_lval env e in
    emit_IncDec env lval (fmt_inc_dec_unop uop)

  | New (cls, args, uargs) ->
    let nargs = List.length args + List.length uargs in
    let env = emit_ctor_lhs env cls nargs in
    let env = emit_args_and_call env args uargs in
    emit_PopR env

  (* comma operator: evaluate all the expressions, ignoring all but the last *)
  | Expr_list es ->
    (match List.rev es with
    | [] -> emit_Null env (* output a dummy value *)
    | last :: rest ->
      let env = List.fold_right
        ~f:(fun e env -> emit_ignored_expr env e) ~init:env rest in
      emit_expr env last)

  | Int (_, n) -> emit_Int env (fmt_int n)
  | Float (_, x) -> emit_Double env (fmt_float x)
  | Null -> emit_Null env
  | True -> emit_bool env true
  | False -> emit_bool env false
  | String (_, s) -> emit_String env s

  | String2 [] -> bug "empty String2"
  (* If there are multiple parts of the String2, they will get
   * stringified when they get concatenated. If there is just one
   * we need to cast it manually. *)
  | String2 [e] ->
    let env = emit_expr env e in
    emit_cast env (Hprim Tstring)
  | String2 (e1::el) ->
    let env = emit_expr env e1 in
    List.fold_left el ~init:env ~f:begin fun env e ->
      let env = emit_expr env e in
      emit_binop env Ast.Dot
    end

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
    List.fold_left ~f:emit_afield ~init:env afields

  | Clone e ->
    let env = emit_expr env e in
    emit_Clone env

  (* XXX: is this right?? *)
  | This -> emit_BareThis env "Notice"

  | Cast ((_, h), e) ->
    let env = emit_expr env e in
    emit_cast env h

  (* Transform back into a Class_const *)
  | Typename sid ->
    emit_expr env (pos, Class_const (CI sid, (pos, "class")))

  (* handle ::class; just emit the name if we have it,
   * otherwise use NameA to get it *)
  | Class_const (cid, (_, "class")) ->
    (match resolve_class_id env cid with
    | RCstatic class_name -> emit_String env class_name
    | RCdynamic dyid ->
      let env = emit_dynamic_class_id env dyid in
      emit_NameA env)

  | Class_const (cid, (_, field)) ->
    (match resolve_class_id env cid with
    | RCstatic class_name -> emit_ClsCnsD env field class_name
    | RCdynamic dyid ->
      let env = emit_dynamic_class_id env dyid in
      emit_ClsCns env field)

  | Await e ->
    (* XXX: await only works in certain contexts but Hack doesn't actually
     * check for this! *)
    let env, skip_label = fresh_label env in
    let env = emit_expr env e in

    (* Await opcode can't handle nulls so we have to check *)
    let env = emit_Dup env in
    let env = emit_IsTypeC env "Null" in
    let env = emit_cjmp env true skip_label in
    let env = emit_Await env in
    emit_label env skip_label

  | Yield af ->
    (match af with
    | AFvalue e ->
      let env = emit_expr env e in
      emit_Yield env
    | AFkvalue (ek, ev) ->
      (* mark that we are a pair generator *)
      let function_props =
        { env.function_props with is_pair_generator = true } in
      let env = { env with function_props } in

      let env = emit_expr env ek in
      let env = emit_expr env ev in
      emit_YieldK env)

  (* Just return null *)
  | Yield_break ->
    env.nonlocal.return_action ~is_initial:true ~has_value:false env

  | List es ->
    (* List here represents tuple(...). Compile to an array. *)
    let array = pos, make_varray es in
    emit_expr env array

  | Shape smap ->
    (* Compile to an array. *)
    let shape_field_to_expr = function
      | SFlit (pos, _ as s) -> pos, String s
      | SFclass_const ((pos, _ as id), s) -> pos, Class_const (CI id, s)
    in
    let shape_fields =
      List.map ~f:(fun (k, v) -> (shape_field_to_expr k, v))
        (extract_shape_fields smap) in
    let array = pos, make_kvarray shape_fields in
    emit_expr env array


  (* TODO: use ColFromArray when possible *)
  | ValCollection (col, es) ->
    let col_id = get_collection_id col in
    let env = emit_NewCol env col_id in
    let emit_entry env e =
        let env = emit_expr env e in
        emit_ColAddNewElemC env
    in
    List.fold_left ~f:emit_entry ~init:env es
  | KeyValCollection (col, fields) ->
    let col_id = get_collection_id col in
    let env = emit_NewCol env col_id in
    let emit_field env (ek, ev) =
        let env = emit_expr env ek in
        let env = emit_expr env ev in
        emit_MapAddElemC env
    in
    List.fold_left ~f:emit_field ~init:env fields
  | Pair (e1, e2) ->
    emit_expr env (pos, ValCollection ("\\Pair", [e1; e2]))

  | InstanceOf (e, cid) ->
    let env = emit_expr env e in
    (match resolve_class_id env cid with
      | RCstatic class_name -> emit_InstanceOfD env class_name
      | RCdynamic dyid ->
        (* Annoyingly, the InstanceOf instruction doesn't want a classref
         * but instead wants a string or an object. So if we have a CIexpr,
         * emit it directly, to avoid doing AGet* immediately followed by
         * NameA.... *)
        let env = match dyid with
          | `var e -> emit_expr env e
          | (`parent | `self | `static) as dyid ->
            let env = emit_dynamic_class_id env dyid in
            emit_NameA env in
        emit_InstanceOf env)

  | Xml (id, attrs, children) ->
    let desugared = Emitter_xhp.convert_xml pos (id, attrs, children) in
    emit_expr env desugared

  | Efun (fun_, vars) ->
    let cstate = env.closure_state in
    incr cstate.closure_counter;
    let count = !(cstate.closure_counter) in
    let name = "Closure$" ^ fmt_name cstate.full_name ^
      (if count > 1 then "#"^string_of_int count else "") in

    (* capture list might have duplicates... *)
    let vars =
      List.dedup ~compare:(fun (_, i1) (_, i2) -> compare i1 i2) vars in

    (* The CreateCL opcode constructs a closure object, taking the
     * closed over variables from the stack. So first we need to
     * emit all the closed over variables. *)
    (* We use CUGetL instead of CGetL because it is tolerant of the
     * variables being unset; unset variables should be propagated to
     * closures (a variable may be conditionally unset and referenced
     * conditionally in the closure only in the cases where it wasn't
     * unset, for example) *)
    let env = List.fold_left vars ~init:env
      ~f:(fun env id -> emit_CUGetL env (get_lid_name id)) in

    let env = emit_CreateCl env (List.length vars) name in

    { env with
      pending_closures = (name, cstate, (fun_, vars)) :: env.pending_closures }


  | Id (_, id) when SN.PseudoConsts.is_pseudo_const id -> unimpl "pseudo consts"
  (* In this context, Id is a global constant *)
  | Id (_, id) -> emit_Cns env id

  | Assert _ -> unimpl "Assert"
  | Any -> unimpl "UNSAFE_EXPR/import/??"
