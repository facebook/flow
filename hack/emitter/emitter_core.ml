(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This contains the underlying data structures and hhas emitting routines. *)

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
  | Mlocal of Nast.id
  | Mint of string
  | Mstring of string
  | Mappend

type member_type =
  | MTelem
  | MTprop

type lval =
  | Llocal of Nast.id
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
  | Nast.Hprim Nast.Tint -> "Int"
  | Nast.Hprim Nast.Tbool -> "Bool"
  | Nast.Hprim Nast.Tfloat -> "Double"
  | Nast.Hprim Nast.Tstring -> "String"
  | Nast.Harray _ -> "Array"
  | Nast.Happly _ -> "Object" (* XXX *)
  | _ -> bug "cast we don't understand"; assert false

let emit_cast env h = emit_op0 ("Cast" ^ fmt_cast h) env

let fmt_null_flavor vis =
  match vis with
  | Nast.OG_nullthrows -> "NullThrows"
  | Nast.OG_nullsafe  -> "NullSafe"

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
