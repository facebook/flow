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

let unimpl s =
  Printf.eprintf "UNIMPLEMENTED: %s\n" s
(* anything that trips this should have passed the typechecker, I think. *)
let bug s =
  Printf.eprintf "BUG: %s\n" s

(*** Types associated with translation ***)

type flavor =
  | FC
  | FR

(* To deal with lvalues, we construct descriptors describing how they
 * are accessed; these descriptors are then used to pick which opcode to
 * use when operating on it as well as the arguments to the
 * instruction. *)

type member =
  | Mexpr
  | Mlocal of id
  | Mint of string
  | Mstring of string
  | Mappend

type member_type =
  | MTelem
  | MTprop

type lval =
  | Llocal of id
  (* Indexing and projection operations *)
  (* This list is stored reversed because ~functional programming~ *)
  | Lmember of base * (member_type * member) list
  (* XXX: static props (hack strict doesn't support globals, I think) *)
and base =
  | Blval of lval
  | Bexpr
  | Bthis

(* Smart constructor for lmember that flattens things out *)
let lmember (base, mem) =
  match base with
  | Blval (Lmember (base', idxes)) -> Lmember (base', mem::idxes)
  | _ -> Lmember (base, [mem])

(* *)
let get_lid_name (_, id) = Ident.get_name id
(* ok, I am pretty sure the strings are preescaped, so we just quote them *)
let quote_str s = "\"" ^ s ^ "\""
(* XXX: actually convert the int to decimal *)
let fmt_int s = s
(* XXX: what format conversions do we need to do? *)
let fmt_float s = s

(* *)

let fmt_member mem =
  match mem with
  | MTelem, Mexpr -> "EC"
  | MTelem, Mint s -> "EI:"^fmt_int s
  | MTelem, Mlocal id -> "EL:"^get_lid_name id
  | MTelem, Mappend -> "W"
  | MTelem, Mstring s -> "ET:"^quote_str s
  | MTprop, Mstring s -> "PT:"^quote_str s
  | MTprop, _ -> unimpl "unsupported member??"; assert false

let fmt_base base =
  match base with
  | Bexpr -> "C"
  | Bthis -> "H"
  | Blval (Llocal id) -> "L:"^get_lid_name id
  | Blval (Lmember _) -> bug "invalid base"; assert false

(* returns the suffix to use on opcodes operating on the lval,
 * the argument describing the lval,
 * and whether to reverse the arguments to any 2 operand opcode
 * using this (argh!) *)
let fmt_lval lval =
  match lval with
  | Llocal id -> "L", get_lid_name id, false
  | Lmember (base, mems) ->
    "M",
    "<" ^ fmt_base base ^ " " ^
    String.concat " " (List.rev_map fmt_member mems) ^
    ">",
    true


(*** Environment manipulation ***)
type label = string

type env = {
  reversed_output: string list;
  indent: int;
  next_label: int;
  continue_target: label;
  break_target: label;
  cleanups: (env -> env) list;
}

let new_env () = {
  reversed_output = [];
  indent = 0;
  next_label = 0;
  continue_target = "BOGUS";
  break_target = "BOGUS";
  cleanups = [];
}

let start_new_function env = { env with next_label = 0 }

let fresh_id pre env =
  { env with next_label = env.next_label+1 },
  pre^string_of_int env.next_label
let fresh_label = fresh_id "L"
let fresh_catch = fresh_id "C"
let fresh_faultlet = fresh_id "F"
let fresh_defvar = fresh_id "DV"

let fresh_labels_2 env =
  let env, a_label = fresh_label env in
  let env, b_label = fresh_label env in
  env, a_label, b_label
let fresh_labels_3 env =
  let env, a_label, b_label = fresh_labels_2 env in
  let env, c_label = fresh_label env in
  env, a_label, b_label, c_label


let enter_construct env = { env with indent = env.indent+1 }
let exit_construct env = { env with indent = env.indent-1 }

(* we basically use env as the state in a State monad, but really we
 * also want Reader and Writer, so we do some /mildly/ hokey simulation
 * of them.
 * Sigh. Maybe I would be happier actually just using state.
 *)

(* Reader like things *)
let with_indent env indent f arg =
  let old = env.indent in
  let env = f { env with indent = indent } arg in
  { env with indent = old }
let with_targets env continue_target break_target f arg =
  let old_continue = env.continue_target in
  let old_break = env.break_target in
  let env, x = f { env with continue_target = continue_target;
                            break_target = break_target } arg in
  { env with continue_target = old_continue;
             break_target = old_break }, x

(* Writer like things *)
let emit_str env s =
  let s = String.make (2*env.indent) ' ' ^ s in
  { env with reversed_output = s :: env.reversed_output }
let emit_strs env ss = emit_str env (String.concat " " ss)
let emit_op_strs env ss = emit_strs env ss
let get_output env = String.concat "\n" (List.rev env.reversed_output)

let collect_output env f arg =
  let old = env.reversed_output in
  let env, res = f { env with reversed_output = [] } arg in
  { env with reversed_output = old }, get_output env, res

(* Cleanup handling; unfortunate? *)
(* XXX: doc rationale?? *)
let add_cleanup env f = { env with cleanups = f :: env.cleanups }
let run_cleanups env =
  let env = List.fold_right (fun f env -> f env) env.cleanups env in
  { env with cleanups = [] }

(*** opcode emitting functions ***)

(* Right now we just emit assembly strings and never have any real
 * datastructure containing this stuff. The actual codegen mostly
 * just deals with the emitting functions so it wouldn't be too hard
 * to change that if necessary. *)

(* uses hokey abbreviations for opcode types:
 * s = string, e = string needing quoting, i = int, l = lval;
 * lvals get special handling and can emit different opcodes *)
let emit_op0 s env = emit_op_strs env [s]
let emit_op1s s env arg1 = emit_op_strs env [s; arg1]
let emit_op1e s env arg1 = emit_op_strs env [s; quote_str arg1]
let emit_op1i s env arg1 = emit_op_strs env [s; string_of_int arg1]
let emit_op2ie s env arg1 arg2 =
  emit_op_strs env [s; string_of_int arg1; quote_str arg2]
let emit_op2ies s env arg1 arg2 arg3 =
  emit_op_strs env [s; string_of_int arg1; quote_str arg2; arg3]
let emit_op1l s env arg1 =
  let t, a, _ = fmt_lval arg1 in
  emit_op_strs env [s^t; a]
(* SetOp{L,M} take their arguments in different orders!
 * Argh. Same with IncDec *)
let emit_op2ls_screwy s env arg1 arg2 =
  let t, a, reverse = fmt_lval arg1 in
  if not reverse then
  emit_op_strs env [s^t; a; arg2] else
  emit_op_strs env [s^t; arg2; a]
(* But it's not even *consistently* inconsistent. FPass doesn't do this *)
let emit_op2il s env arg1 arg2 =
  let t, a, _ = fmt_lval arg2 in
  emit_op_strs env [s^t; string_of_int arg1; a]


let emit_label env l = with_indent env (env.indent-1) emit_str (l^":")

(* specific opcodes *)
let emit_SetOp =          emit_op2ls_screwy "SetOp"
let emit_IncDec =         emit_op2ls_screwy "IncDec"
let emit_CGet =           emit_op1l   "CGet"
let emit_Set =            emit_op1l   "Set"
let emit_RetC =           emit_op0    "RetC"
let emit_PopC =           emit_op0    "PopC"
let emit_PopR =           emit_op0    "PopR"
let emit_UnboxR =         emit_op0    "UnboxR"
let emit_String =         emit_op1e   "String"
let emit_Int =            emit_op1s   "Int"
let emit_Float =          emit_op1s   "Float"
let emit_Null =           emit_op0    "Null"
let emit_FPushFunc =      emit_op1i   "FPushFunc"
let emit_FPushFuncD =     emit_op2ie  "FPushFuncD"
let emit_FPushCtorD =     emit_op2ie  "FPushCtorD"
let emit_FPushObjMethodD =emit_op2ies "FPushObjMethodD"
let emit_FPassLval =      emit_op2il  "FPass"
let emit_FCall =          emit_op1i   "FCall"
let emit_FCallUnpack =    emit_op1i   "FCallUnpack"
let emit_DefCls =         emit_op1i   "DefCls"
let emit_NewArray =       emit_op1i   "NewArray"
let emit_NewMixedArray =  emit_op1i   "NewMixedArray"
let emit_Jmp =            emit_op1s   "Jmp"
let emit_Not =            emit_op0    "Not"
let emit_BitNot =         emit_op0    "BitNot"
let emit_Clone =          emit_op0    "Clone"
let emit_Dup =            emit_op0    "Dup"
let emit_This =           emit_op0    "This"
let emit_BareThis =       emit_op1s   "BareThis"
let emit_AddNewElemC =    emit_op0    "AddNewElemC"
let emit_AddElemC =       emit_op0    "AddElemC"
let emit_Throw =          emit_op0    "Throw"
let emit_Catch =          emit_op0    "Catch"
let emit_Unwind =         emit_op0    "Unwind"

let emit_bool env = function | true -> emit_op0 "True" env
                             | false -> emit_op0 "False" env
let emit_cjmp env = function | true -> emit_op1s "JmpNZ" env
                             | false -> emit_op1s "JmpZ" env

let emit_FPass env = function | FC -> emit_op1i "FPassCE" env
                              | FR -> emit_op1i "FPassR" env
let emit_Pop env = function | FC -> emit_PopC env
                            | FR -> emit_PopR env


let fmt_binop bop =
  match bop with
  | Ast.Plus -> "AddO"
  | Ast.Minus -> "SubO"
  | Ast.Star -> "MulO"
  | Ast.Slash -> "Div"
  | Ast.Eqeq -> "Eq"
  | Ast.EQeqeq -> "Same"
  | Ast.Starstar -> "Pow"
  | Ast.Diff -> "Neq"
  | Ast.Diff2 -> "NSame"
  | Ast.Lt -> "Lt"
  | Ast.Lte -> "Lte"
  | Ast.Gt -> "Gt"
  | Ast.Gte -> "Gte"
  | Ast.Dot -> "Concat"
  | Ast.Amp -> "BitAnd"
  | Ast.Bar -> "BitOr"
  | Ast.Ltlt -> "Shl"
  | Ast.Gtgt -> "Shr"
  | Ast.Percent -> "Mod"
  | Ast.Xor -> "Xor"
  | Ast.Eq _ | Ast.AMpamp | Ast.BArbar -> bug "nonstandard binop"; assert false

let emit_binop env bop = emit_op0 (fmt_binop bop) env

let fmt_inc_dec_unop bop =
  match bop with
  | Ast.Uincr -> "PreInc"
  | Ast.Udecr -> "PreDec"
  | Ast.Upincr -> "PostInc"
  | Ast.Updecr -> "PostDec"
  | _ -> bug "non inc/dec unop"; assert false

(* These have different names and I don't understand why. *)
let fmt_eq_binop bop =
  match bop with
  | Ast.Plus -> "PlusEqualO"
  | Ast.Minus -> "MinusEqualO"
  | Ast.Star -> "MulEqualO"
  | Ast.Slash -> "DivEqual"
  | Ast.Starstar -> "PowEqual"
  | Ast.Dot -> "ConcatEqual"
  | Ast.Amp -> "AndEqual"
  | Ast.Bar -> "OrEqual"
  | Ast.Ltlt -> "SlEqual"
  | Ast.Gtgt -> "SrEqual"
  | Ast.Percent -> "ModEqual"
  | Ast.Xor -> "XorEqual"
  | Ast.Eq _ | Ast.AMpamp | Ast.BArbar | Ast.Eqeq
  | Ast.EQeqeq | Ast.Diff | Ast.Diff2 | Ast.Lt
  | Ast.Lte | Ast.Gt | Ast.Gte -> bug "not a eq binop"; assert false

(* XXX: what all casts do we allow? *)
let fmt_cast (_, h) =
  match h with
  | Hprim Tint -> "Int"
  | Hprim Tbool -> "Bool"
  | Hprim Tfloat -> "Double"
  | Hprim Tstring -> "String"
  | Harray _ -> "Array"
  | Happly _ -> "Object" (* XXX *)
  | _ -> bug "cast we don't understand"; assert false

let emit_cast env h = emit_op0 ("Cast" ^ fmt_cast h) env

let fmt_null_flavor vis =
  match vis with
  | OG_nullthrows -> "NullThrows"
  | OG_nullsafe  -> "NullSafe"

(* *)
let fmt_options = function [] -> ""
                         | xs -> "[" ^ String.concat " " xs ^ "] "
let bool_option s b =
  if b then [s] else []

let emit_enter env command options name extra =
  let env = emit_str env (command ^ " " ^ fmt_options options ^
                          name ^ extra ^ " {") in
  enter_construct env
let emit_exit env =
  let env = exit_construct env in
  let env = emit_str env "}" in
  env

(*** actual codegen! ***)

let is_lval expr =
  match expr with
  (* XXX: other lvals! *)
  | Lvar _  | Obj_get _ | Array_get _ -> true
  | _ -> false

let is_empty_block = function
  | [] | [Noop] -> true
  | _ -> false

(* Emit bytecode for a block. Stack should start and end empty.
 * Returns whether the block is terminal. *)
let rec emit_block env stmts =
  List.fold_left (fun (env, _) stmt -> emit_stmt env stmt)
                 (env, false) stmts

(* Emit a conditional branch based on an expression. jump_if indicates
 * whether to jump when the condition is true or false.
 * This is its own thing so that we can efficiently handle !/&&/||
 * by directly incorporating their behavior into the branching rather than
 * computing their results as intermediate values. *)
and emit_cond env (_, expr_ as expr) jump_if target =
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

(* Emit bytecode for a statement. Stack should start and end empty.
 * Returns whether the statement is terminal. *)
and emit_stmt env stmt =
  match stmt with
  | Noop -> env, false
  | Return (_, ret) ->
    let env = match ret with Some e -> emit_expr env e
                           | None -> emit_Null env in
    emit_RetC env, true
  | Expr e ->
    emit_ignored_expr env e, false

  | If (e, btrue, bfalse) ->
    let env, false_label, end_label = fresh_labels_2 env in
    (* emit cond and jump to false case *)
    let env = emit_cond env e false false_label in

    (* emit true case and unconditional jump after false case *)
    let env, true_terminal = emit_block env btrue in
    (* we skip the unconditional jump if the false block is empty
     * or the true block is terminal *)
    let skip_jump = true_terminal || is_empty_block bfalse in
    let env = if skip_jump then env else
              emit_Jmp env end_label in

    (* emit false cases and labels *)
    let env = emit_label env false_label in
    let env, false_terminal = emit_block env bfalse in
    let env = if skip_jump then env else
              emit_label env end_label in

    env, true_terminal && false_terminal

  | While (etest, body) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* we duplicate the condition for while loops *)
    (* XXX: should we always do this? *)

    (* emit cond and jmp past loop *)
    let env = emit_cond env etest false break_label in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the test again *)
    let env = emit_label env cont_label in
    let env = emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false

  | Do (body, etest) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the test *)
    let env = emit_label env cont_label in
    let env = emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false

  | For (einit, etest, estep, body) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* we duplicate the condition for for loops *)
    (* XXX: should we always do this? *)

    (* emit init *)
    let env = emit_ignored_expr env einit in

    (* emit test and conditional jmp past loop *)
    let env = emit_cond env etest false break_label in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the step *)
    let env = emit_label env cont_label in
    let env = emit_ignored_expr env estep in

    (* emit the test again *)
    let env = emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false


  | Break _ -> emit_Jmp env env.break_target, true
  | Continue _ -> emit_Jmp env env.continue_target, true

  | Throw (is_terminal, e) ->
    let env = emit_expr env e in
    emit_Throw env, is_terminal

  | Try (try_body, catches, finally_body) ->
    let env, target = fresh_label env in
    let env, catch_labels = lmap (fun env _ -> fresh_catch env) env catches in
    let catches = List.combine catch_labels catches in

    let fmt_catch_hdr (label, ((_, cls), _, _)) =
      "(" ^ strip_ns cls ^ " " ^ label ^ ")" in
    let catch_hdrs = String.concat " " (List.rev_map fmt_catch_hdr catches) in

    (* If we have a finally, generate a finally label
     * and output a try_fault handler *)
    let env, opt_faultlet =
      if is_empty_block finally_body then env, None else
      let env, label = fresh_faultlet env in
      let env = emit_enter env ".try_fault" [] label "" in
      env, Some label in

    (* emit the try block *)
    let env = emit_enter env ".try_catch" [] catch_hdrs "" in
    let env, _ = emit_block env try_body in
    let env = emit_Jmp env target in
    let env = emit_exit env in

    let emit_catch env (label, (_, var, body)) =
      let env = emit_label env label in
      let env = emit_Catch env in
      let env = emit_Set env (Llocal var) in
      let env = emit_PopC env in
      let env, _ = emit_block env body in
      (* PERF: want to be able to skip the jump on the last one, I suppose *)
      emit_Jmp env target
    in

    let env = List.fold_left emit_catch env catches in
    let env = if opt_faultlet = None then env else emit_exit env in

    let env = emit_label env target in

    (* If we had a finally, output the finally code and register a cleanup
     * that will emit the faultlet *)
    let env = match opt_faultlet with
    | None -> env
    | Some finally_label ->
      let emit_faultlet env =
        let env = emit_label env finally_label in
        let env, _ = emit_block env finally_body in
        emit_Unwind env
      in
      let env = add_cleanup env emit_faultlet in
      let env, _ = emit_block env finally_body in
      env
    in

    env, false

  | Static_var _
  | Switch _
  | Foreach _
  | Fallthrough ->
    unimpl "statement"; assert false

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

let emit_func_body env body =
  let env, terminal = emit_block env body.fnb_nast in
  if terminal then env else
  let env = emit_Null env in
  emit_RetC env

let emit_param p =
  assert (not p.param_is_reference); (* actually right *)
  assert (not p.param_is_variadic && p.param_expr = None);
  get_lid_name p.param_id

let fmt_params params =
  let param_names = List.map emit_param params in
  "(" ^ String.concat ", " param_names ^ ")"

let fmt_visibility vis =
  match vis with
  | Public -> "public"
  | Private -> "private"
  | Protected  -> "protected"


let emit_fun nenv x =
  let f = Naming_heap.FunHeap.find_unsafe x in
  let nb = Naming.func_body nenv f in

  assert (f.f_user_attributes = [] &&
          f.f_variadic = FVnonVariadic && f.f_fun_kind = Ast.FSync);

  let env = new_env () in

  let options = ["mayusevv"] in
  let env = emit_enter env ".function" options (strip_ns x)
                       (fmt_params f.f_params) in
  let env = emit_func_body env nb in
  let env = run_cleanups env in
  let env = emit_exit env in

  get_output env

(* XXX: *lots* of duplication from emit_fun *)
let emit_method_ env m name =
  let env = start_new_function env in
  let nb = assert_named_body m.m_body in

  assert (m.m_user_attributes = [] &&
          m.m_variadic = FVnonVariadic && m.m_fun_kind = Ast.FSync);

  let options = bool_option "abstract" m.m_abstract @
                bool_option "final" m.m_final @
                [fmt_visibility m.m_visibility; "mayusevv"] in
  let env = emit_enter env ".method" options name
                       (fmt_params m.m_params) in
  let env = emit_func_body env nb in
  let env = run_cleanups env in
  let env = emit_exit env in
  env
let emit_method env m = emit_method_ env m (snd m.m_name)
let emit_default_ctor env name abstract =
  let options = bool_option "abstract" abstract @ ["public"; "mayusevv"] in
  let env = emit_enter env ".method" options name (fmt_params []) in
  let env = emit_Null env in
  let env = emit_RetC env in
  let env = emit_exit env in
  env

(* extends lists and things are hints,
 * but I *think* it needs to be an apply? *)
let fmt_class_hint = function
  | _, Happly ((_, cls), _) -> strip_ns cls
  | _ -> bug "class hint not apply??"; assert false
let fmt_class_list op classes =
  match classes with
  | [] -> ""
  | [x] -> " " ^ op ^ " " ^ fmt_class_hint x
  | xs ->  " " ^ op ^ " (" ^ String.concat " " (List.map fmt_class_hint xs) ^")"

let emit_var env var =
  (* Only handle simple cases. In particular, only uninitialized vars! *)
  assert (var.cv_final = false && var.cv_is_xhp = false && var.cv_expr = None);
  let options = fmt_options [fmt_visibility var.cv_visibility] in
  emit_strs env [".property"; options; snd var.cv_id; "="; "\"\"\"N;\"\"\";"]

let emit_use env use =
  emit_strs env [".use"; fmt_class_hint use ^ ";"]

let class_kind_options  = function
  | Ast.Cabstract -> ["abstract"]
  | Ast.Cnormal -> []
  | Ast.Cinterface -> ["interface"]
  | Ast.Ctrait -> ["final"; "trait"]
  | Ast.Cenum -> unimpl "enum not supported"; assert false

let emit_class nenv x =
  let cls = Naming_heap.ClassHeap.find_unsafe x in
  let cls = Naming.class_meth_bodies nenv cls in
  let env = new_env () in

  (* we only handle a very limited range of things right now *)
  assert (cls.c_is_xhp = false &&
          cls.c_xhp_attr_uses = [] &&
          cls.c_consts = [] &&
          cls.c_typeconsts = [] &&
          cls.c_static_vars = [] &&
          cls.c_static_methods = [] &&
          cls.c_user_attributes = [] &&
          cls.c_enum = None);

  let options = bool_option "final" cls.c_final @
                class_kind_options cls.c_kind in

  let extend_tag = if cls.c_kind = Ast.Cinterface
                   then "implements" else "extends" in
  let extends_list = fmt_class_list extend_tag cls.c_extends in
  let implements_list = fmt_class_list "implements" cls.c_implements in

  let env = emit_enter env ".class" options (strip_ns x)
                       (extends_list^implements_list) in


  let env = List.fold_left emit_use env cls.c_uses in
  let env = List.fold_left emit_var env cls.c_vars in
  let env = List.fold_left emit_method env cls.c_methods in

  let env = match cls.c_constructor with
            | None -> emit_default_ctor env "86ctor"
                                        (cls.c_kind = Ast.Cinterface)
            | Some m -> emit_method_ env m "86ctor" in

  let env = emit_exit env in

  get_output env


(* Bogusly emit a call to a hardcoded test function *)
let emit_test_call env =
  let env = emit_FPushFuncD env 0 "test" in
  let env = emit_FCall env 0 in
  emit_PopR env

let emit_main classes =
  let env = new_env () in
  let env = emit_enter env ".main" [] "" "" in

  (* emit def classes *)
  let env, _ = List.fold_left begin fun (env, i) _ ->
      let env = emit_DefCls env i in
      env, i+1
    end (env, 0) classes in

  (* emit debugging test *)
  let env = emit_test_call env in

  (* emit the expected return *)
  let env = emit_Int env "1" in
  let env = emit_RetC env in

  let env = emit_exit env in
  Printf.printf "%s\n\n" (get_output env)

let emit_file nenv {FileInfo.file_mode; funs; classes; _} () =
  if file_mode <> Some FileInfo.Mstrict then () else
  let fun_code = List.map (fun (_, x) -> emit_fun nenv x) funs in
  let class_code = List.map (fun (_, x) -> emit_class nenv x) classes in

  emit_main classes;
  List.iter (Printf.printf "%s\n\n") fun_code;
  List.iter (Printf.printf "%s\n\n") class_code
