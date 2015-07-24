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
  | Lvar _  | Obj_get _ | Array_get _ | Class_get _ | Lplaceholder _ -> true
  | _ -> false

(* Try to convert a class id into a static string; if we can't, return None *)
let fmt_class_id env cid =
  match cid with
  | CIparent -> env.parent_name
  | CIself -> env.self_name
  | CI (_, s) -> Some (strip_ns s)
  | CIstatic | CIvar _ -> None

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
  | String2 ([], s) -> env, Mstring (unescape_str s)
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
  | Lvar id -> env, llocal id
  | Lplaceholder (_, s) -> env, Llocal s
  | Array_get (e1, maybe_e2) ->
    let env, base = emit_base env e1 in
    let env, member = (match maybe_e2 with
      | Some e2 -> emit_member env e2
      | None -> env, Mappend) in
    env, lmember (base, (MTelem, member))
  | Obj_get (e1, (_, Id (_, id)), _null_flavor_TODO) ->
    let env, base = emit_base env e1 in
    env, lmember (base, (MTprop, Mstring id))
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
      emit_class_id env cid
    | _ -> env
  in
  env, lval


and emit_class_id env cid =
  (* if we have a static name for the class, use that *)
  match fmt_class_id env cid with
  | Some s ->
    let env = emit_String env s in
    emit_AGetC env
  (* otherwise we need to dynamically find the class *)
  | None ->
    match cid with
    | CI _ -> assert false
    | CIparent -> emit_Parent env
    | CIself -> emit_Self env
    | CIstatic -> emit_LateBoundCls env
    | CIvar (_, Lvar id) -> emit_AGetL env (get_lid_name id)
    | CIvar e ->
      let env = emit_expr env e in
      emit_AGetC env

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
        Core_list.concat_mapi ~f:collect es
      | e -> [e, path]
    in
    let assignments = collect_assignments [] e1 in

    (* First we emit all the lvals. We do this first so any effects will
     * happen left to right. *)
    let emit_lhs env (lhs, path) =
      let env, lval = emit_lval env lhs in
      env, (lval, path)
    in
    let env, assignments = lmap emit_lhs env assignments in

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
    let env = List.fold_right emit_assign assignments env in

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
  | Fun_id (_, name) -> emit_FPushFuncD env nargs (strip_ns name)
  | Obj_get (obj, (_, Id (_, name)), null_flavor) ->
    let env = emit_method_base env obj in
    emit_FPushObjMethodD env nargs name (fmt_null_flavor null_flavor)
  | Class_const (cid, (_, field)) ->
    (match fmt_class_id env cid with
    | Some class_name -> emit_FPushClsMethodD env nargs field class_name
    | None -> let env = emit_String env field in
              let env = emit_class_id env cid in
              emit_FPushClsMethod env nargs)

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
  | _ -> unimpl "unsupported constructor lhs"

(* emit code to push args and call a function after the thing being
 * called has already been pushed; doesn't do any stack adjustment
 * after the call *)
and emit_args_and_call env args uargs =
  let all_args = args @ uargs in
  let nargs = List.length all_args in
  let env = Core_list.foldi ~f:begin fun i env arg ->
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

and emit_call env ef args uargs =
  match snd ef with
  | Id (_, echo) when echo = SN.SpecialFunctions.echo ->
    let nargs = List.length args in
    let env = Core_list.foldi ~f:begin fun i env arg ->
       let env = emit_expr env arg in
       let env = emit_Print env in
       if i = nargs-1 then env else emit_PopC env
    end ~init:env args in
    env, FC
  (* TODO: a billion other builtins *)

  | _ -> emit_normal_call env ef args uargs

(* emit code to evaluate an expression that might have a non-Cell flavor;
 * certain operations want to handle this; most will just call
 * emit_expr or emit_ignored_expr which will automatically unbox/pop
 * R flavored things. *)
and emit_flavored_expr env (_, expr_ as expr) =
  match expr_ with
  | Call (Cnormal, ef, args, uargs) -> emit_call env ef args uargs
  | Call (Cuser_func, _, _, _) -> unimpl "call_user_func"

  (* For most things, just fall back to emit_expr *)
  | _ -> emit_expr env expr, FC

(* emit code to evaluate an expression;
 * doesn't rely on the incoming stack state, leaves the value of the
 * expression on the stack as a cell. *)
and emit_expr env (pos, expr_ as expr) =
  match expr_ with
  (* Calls produce flavor R, so emit it and then unbox *)
  | Call (_, _, _, _) ->
    let env, flavor = emit_flavored_expr env expr in
    assert (flavor = FR);
    emit_UnboxR env

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
  (* all the rest are pre/post inc/dec *)
  | Unop (uop, e) ->
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
    | [] -> env
    | last :: rest ->
      let env =
        List.fold_right (fun e env -> emit_ignored_expr env e) rest env in
      emit_expr env last)

  | Int (_, n) -> emit_Int env (fmt_int n)
  | Float (_, x) -> emit_Float env (fmt_float x)
  | String (_, s) -> emit_String env s
  | String2 ([], s) -> emit_String env (unescape_str s)
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

  | Any -> bug "what even is this"

  (* probably going to take AST changes *)
  | String2 _ -> unimpl "double-quoted string interpolation"

  (* XXX: is this right?? *)
  | This -> emit_BareThis env "Notice"

  | Cast (h, e) ->
    let env = emit_expr env e in
    emit_cast env h

  | Class_const (cid, (_, field)) ->
    (match fmt_class_id env cid with
    | Some class_name -> emit_ClsCnsD env field class_name
    | None -> let env = emit_class_id env cid in
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
    let env = emit_Await env env.next_iterator in
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
    let array = pos, Array (List.map (fun e -> AFvalue e) es) in
    emit_expr env array

  | Shape smap ->
    (* Compile to an array. *)
    let shape_field_to_expr = function
      | SFlit (pos, _ as s) -> pos, String s
      | SFclass_const ((pos, _ as id), s) -> pos, Class_const (CI id, s)
    in
    (* The doc comment says only use in testing code but this is
     * /actually/ the right thing here *)
    let shape_fields =
      List.map (fun (k, v) -> (shape_field_to_expr k, v))
        (ShapeMap.elements smap) in
    (* Sort the fields by their position in order to have the same insertion
     * order as HHVM. Sigh. *)
    let shape_fields = List.sort
      (fun ((p1, _), _) ((p2, _), _) -> Pos.compare p1 p2)
      shape_fields in
    let afields = List.map (fun (k, v) -> AFkvalue (k, v)) shape_fields in
    let array = pos, Array afields in
    emit_expr env array


  (* TODO: use ColFromArray when possible *)
  | ValCollection (col, es) ->
    let col_id = Emitter_consts.get_collection_id (strip_ns col) in
    let env = emit_NewCol env col_id in
    let emit_entry env e =
        let env = emit_expr env e in
        emit_ColAddNewElemC env
    in
    List.fold_left emit_entry env es
  | KeyValCollection (col, fields) ->
    let col_id = Emitter_consts.get_collection_id (strip_ns col) in
    let env = emit_NewCol env col_id in
    let emit_field env (ek, ev) =
        let env = emit_expr env ek in
        let env = emit_expr env ev in
        emit_MapAddElemC env
    in
    List.fold_left emit_field env fields
  | Pair (e1, e2) ->
    emit_expr env (pos, ValCollection ("\\Pair", [e1; e2]))


  | Id _ -> unimpl "Id"
  | Fun_id _ -> unimpl "Fun_id"
  | Method_id _ -> unimpl "Method_id"
  | Method_caller _ -> unimpl "Method_caller"
  | Smethod_id _ -> unimpl "Smethod_id"
  | Special_func _ -> unimpl "Special_func"
  | InstanceOf _ -> unimpl "InstanceOf"
  | Efun _ -> unimpl "Efun"
  | Xml _ -> unimpl "Xml"
  | Assert _ -> unimpl "Assert"
