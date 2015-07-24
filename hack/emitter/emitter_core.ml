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
  Printf.eprintf "UNIMPLEMENTED: %s\n" s; assert false
(* anything that trips this should have passed the typechecker, I think. *)
let bug s =
  Printf.eprintf "BUG: %s\n" s; assert false

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
  | Mlocal of string
  | Mint of string
  | Mstring of string
  | Mappend

type member_type =
  | MTelem
  | MTprop

type lval =
  | Llocal of string
  (* Indexing and projection operations *)
  (* This list is stored reversed because ~functional programming~ *)
  | Lmember of base * (member_type * member) list
  | Lsprop of Nast.class_id
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
(* XXX: ocaml and php probably don't have exactly the same escaping rules *)
let escape_str = String.escaped
let unescape_str = Scanf.unescaped
(* Whenever we need to emit a quoted string, we escape it.
 * This means that places that for String2, which has preescaped strings,
 * we need to *unescape* before passing the string to core emitting functions.
 * This is a little silly, but it keeps the interface consistent. *)
let quote_str s = "\"" ^ escape_str s ^ "\""
(* XXX: actually convert the int to decimal *)
let fmt_int s = s
(* XXX: what format conversions do we need to do? *)
let fmt_float s = s
let fmt_str_vec v = "<" ^ String.concat " " v ^ ">"
let fmt_vec f v = "<" ^ String.concat " " (List.map f v) ^ ">"

let llocal id = Llocal (get_lid_name id)

(* *)

let fmt_member mem =
  match mem with
  | MTelem, Mexpr -> "EC"
  | MTelem, Mint s -> "EI:"^fmt_int s
  | MTelem, Mlocal id -> "EL:"^id
  | MTelem, Mappend -> "W"
  | MTelem, Mstring s -> "ET:"^quote_str s
  | MTprop, Mstring s -> "PT:"^quote_str s
  | MTprop, _ -> unimpl "unsupported member??"

let fmt_base base =
  match base with
  | Bexpr -> "C"
  | Bthis -> "H"
  | Blval (Llocal id) -> "L:"^id
  | Blval (Lsprop _) -> "SC"
  | Blval (Lmember _) -> bug "invalid base"

(* returns the suffix to use on opcodes operating on the lval,
 * the argument describing the lval,
 * and whether to reverse the arguments to any 2 operand opcode
 * using this (argh!) *)
let fmt_lval lval =
  match lval with
  | Llocal id -> "L", id, false
  | Lsprop _ -> "S", "", false
  | Lmember (base, mems) ->
    "M",
    "<" ^ fmt_base base ^ " " ^
    String.concat " " (List.rev_map fmt_member mems) ^
    ">",
    true


(*** Environment manipulation ***)
type label = string

(* information we need to gather about a function while emitting it *)
type function_props = {
  is_pair_generator: bool;
  (* I think this struct is likely to grow, and if it only has one element
   * ocaml will warn when doing updates on it with "with"... *)
  dummy_warning_suppression: unit;
}
type nonlocal_actions = {
  continue_action: is_initial:bool -> env -> env;
  break_action: is_initial:bool -> env -> env;
  return_action: has_value:bool -> is_initial:bool -> env -> env;
}
and env = {
  reversed_output: string list;
  indent: int;
  next_label: int;
  num_iterators: int;
  next_iterator: int; (* iterators allocated in a stack discipline *)
  function_props: function_props;
  nonlocal: nonlocal_actions;
  cleanups: (env -> env) list;
  self_name: string option;
  parent_name: string option;
}

let default_function_props = {
  is_pair_generator = false;
  dummy_warning_suppression = ();
}
let empty_nonlocal_actions = {
  continue_action = (fun ~is_initial:_ _ -> assert false);
  break_action = (fun ~is_initial:_ _ -> assert false);
  return_action = (fun ~has_value:_ ~is_initial:_ _ -> assert false);
}
let new_env () = {
  reversed_output = [];
  indent = 0;
  next_label = 0;
  num_iterators = 0;
  next_iterator = 0;
  function_props = default_function_props;
  nonlocal = empty_nonlocal_actions;
  cleanups = [];
  self_name = None;
  parent_name = None;
}

let start_new_function env =
  let nenv = new_env () in
  { nenv with
    reversed_output = env.reversed_output;
    indent = env.indent;
    self_name = env.self_name;
    parent_name = env.parent_name;
  }

let fresh_id pre env =
  { env with next_label = env.next_label+1 },
  pre^string_of_int env.next_label
let fresh_label = fresh_id "L"
let fresh_catch = fresh_id "C"
let fresh_faultlet = fresh_id "F"
let fresh_defvar = fresh_id "DV"

(* XXX: these are all wrong; we actually want unnamed variables,
 * but this works well enough for now *)
let fresh_tempvar = fresh_id "$___"
let get_ret_var env = env, "$___ret"
let get_nonlocal_var env = env, "$___nonlocal"

let fresh_labels_2 env =
  let env, a_label = fresh_label env in
  let env, b_label = fresh_label env in
  env, a_label, b_label
let fresh_labels_3 env =
  let env, a_label, b_label = fresh_labels_2 env in
  let env, c_label = fresh_label env in
  env, a_label, b_label, c_label

let fresh_iterator env =
  { env with
    next_iterator = env.next_iterator+1;
    num_iterators = if env.next_iterator < env.num_iterators then
                    env.num_iterators else
                    env.num_iterators+1
  }, env.next_iterator
let free_iterator env i =
  assert (env.next_iterator = i+1);
  { env with next_iterator = env.next_iterator-1 }

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
let with_actions env nonlocal f arg =
  let old_nonlocal = env.nonlocal in
  let env, x = f { env with nonlocal = nonlocal } arg in
  { env with nonlocal = old_nonlocal }, x

(* Writer like things *)
let emit_str_raw env s =
  { env with reversed_output = s :: env.reversed_output }
let emit_str env s =
  let s = String.make (2*env.indent) ' ' ^ s in
  emit_str_raw env s
let emit_strs env ss = emit_str env (String.concat " " ss)
let emit_op_strs env ss = emit_strs env ss
let get_output env = String.concat "\n" (List.rev env.reversed_output)

let collect_output env f arg =
  let old = env.reversed_output in
  let env = f { env with reversed_output = [] } arg in
  { env with reversed_output = old }, get_output env

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
 * s = string, e = string needing quoting+escaping, i = int, l = lval;
 * lvals get special handling and can emit different opcodes *)
let emit_op0 s env = emit_op_strs env [s]
let emit_op1s s env arg1 = emit_op_strs env [s; arg1]
let emit_op1e s env arg1 = emit_op_strs env [s; quote_str arg1]
let emit_op1i s env arg1 = emit_op_strs env [s; string_of_int arg1]
let emit_op2ie s env arg1 arg2 =
  emit_op_strs env [s; string_of_int arg1; quote_str arg2]
let emit_op2es s env arg1 arg2 =
  emit_op_strs env [s; quote_str arg1; arg2]
let emit_op2ee s env arg1 arg2 =
  emit_op_strs env [s; quote_str arg1; quote_str arg2]
let emit_op3ies s env arg1 arg2 arg3 =
  emit_op_strs env [s; string_of_int arg1; quote_str arg2; arg3]
let emit_op3iee s env arg1 arg2 arg3 =
  emit_op_strs env [s; string_of_int arg1; quote_str arg2; quote_str arg3]
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

let emit_op3iter s env arg1 arg2 = function
  | [id] ->
    emit_op_strs env [s; string_of_int arg1; arg2; id]
  | [id1; id2] ->
    emit_op_strs env [s ^ "K"; string_of_int arg1; arg2; id1; id2]
  | _ -> assert false



let emit_label env l = with_indent env (env.indent-1) emit_str (l^":")

(* specific opcodes *)
let emit_SetOp =          emit_op2ls_screwy "SetOp"
let emit_IncDec =         emit_op2ls_screwy "IncDec"
let emit_CGet =           emit_op1l   "CGet"
let emit_CGetL =          emit_op1s   "CGetL"
let emit_PushL =          emit_op1s   "PushL"
let emit_IssetL =         emit_op1s   "IssetL"
let emit_UnsetL =         emit_op1s   "UnsetL"
let emit_Set =            emit_op1l   "Set"
let emit_SetL =           emit_op1s   "SetL"
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
let emit_FPushObjMethodD =emit_op3ies "FPushObjMethodD"
let emit_FPushClsMethod = emit_op1i   "FPushClsMethod"
let emit_FPushClsMethodD =emit_op3iee "FPushClsMethodD"
let emit_ClsCns =         emit_op1e   "ClsCns"
let emit_ClsCnsD =        emit_op2ee  "ClsCnsD"
let emit_FPassLval =      emit_op2il  "FPass"
let emit_FCall =          emit_op1i   "FCall"
let emit_FCallUnpack =    emit_op1i   "FCallUnpack"
let emit_DefCls =         emit_op1i   "DefCls"
let emit_NewArray =       emit_op1i   "NewArray"
let emit_NewMixedArray =  emit_op1i   "NewMixedArray"
let emit_NewCol =         emit_op1i   "NewCol"
let emit_Jmp =            emit_op1s   "Jmp"
let emit_Not =            emit_op0    "Not"
let emit_BitNot =         emit_op0    "BitNot"
let emit_Clone =          emit_op0    "Clone"
let emit_Dup =            emit_op0    "Dup"
let emit_This =           emit_op0    "This"
let emit_BareThis =       emit_op1s   "BareThis"
let emit_AddNewElemC =    emit_op0    "AddNewElemC"
let emit_ColAddNewElemC = emit_op0    "ColAddNewElemC"
let emit_AddElemC =       emit_op0    "AddElemC"
let emit_MapAddElemC =    emit_op0    "MapAddElemC"
let emit_Throw =          emit_op0    "Throw"
let emit_Catch =          emit_op0    "Catch"
let emit_Unwind =         emit_op0    "Unwind"
let emit_Print =          emit_op0    "Print"
let emit_IterInit =       emit_op3iter "IterInit"
let emit_IterNext =       emit_op3iter "IterNext"
let emit_IterFree =       emit_op1i   "IterFree"
let emit_CheckProp =      emit_op1e   "CheckProp"
let emit_InitProp =       emit_op2es  "InitProp"
let emit_LateBoundCls =   emit_op0    "LateBoundCls"
let emit_Self =           emit_op0    "Self"
let emit_Parent =         emit_op0    "Parent"
let emit_AGetL =          emit_op1s   "AGetL"
let emit_AGetC =          emit_op0    "AGetC"
let emit_Await =          emit_op1i   "Await"
let emit_IsTypeC =        emit_op1s   "IsTypeC"
let emit_CreateCont =     emit_op0    "CreateCont"
let emit_Yield =          emit_op0    "Yield"
let emit_YieldK =         emit_op0    "YieldK"

let emit_Switch env labels base bound =
  emit_op_strs env ["Switch"; fmt_str_vec labels; string_of_int base; bound]
let emit_IterBreak env iters label =
  let fmt_iter i = "(Iter) " ^ string_of_int i in
  emit_op_strs env ["IterBreak"; fmt_vec fmt_iter iters; label]

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
  | Ast.Eq _ | Ast.AMpamp | Ast.BArbar -> bug "nonstandard binop"

let emit_binop env bop = emit_op0 (fmt_binop bop) env

let fmt_inc_dec_unop bop =
  match bop with
  | Ast.Uincr -> "PreInc"
  | Ast.Udecr -> "PreDec"
  | Ast.Upincr -> "PostInc"
  | Ast.Updecr -> "PostDec"
  | _ -> bug "non inc/dec unop"

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
  | Ast.Lte | Ast.Gt | Ast.Gte -> bug "not a eq binop"

(* XXX: what all casts do we allow? *)
let fmt_cast (_, h) =
  match h with
  | Nast.Hprim Nast.Tint -> "Int"
  | Nast.Hprim Nast.Tbool -> "Bool"
  | Nast.Hprim Nast.Tfloat -> "Double"
  | Nast.Hprim Nast.Tstring -> "String"
  | Nast.Harray _ -> "Array"
  | Nast.Happly _ -> "Object" (* XXX *)
  | _ -> bug "cast we don't understand"

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

(* Some stuff for managing faultlets/fault regions *)
let make_opt_faultlet env need_faultlet =
  if need_faultlet then
  let env, label = fresh_faultlet env in env, Some label else
  env, None

let emit_fault_enter env label = emit_enter env ".try_fault" [] label ""

let emit_fault_exit env _ = emit_exit env

let emit_fault_cleanup ?faultlet_extras:(extra=(fun env -> env))
                       ?cleanup:(f=(fun env->env))
                       env label =
  let env = f env in
  let emit_faultlet env =
    let env = emit_label env label in
    let env = extra env in
    let env = f env in
    emit_Unwind env
  in
  add_cleanup env emit_faultlet
