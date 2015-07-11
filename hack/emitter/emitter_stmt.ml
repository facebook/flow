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
open Emitter_expr

(* Emit bytecode for a block. Stack should start and end empty.
 * Returns whether the block is terminal. *)
let rec emit_block env stmts =
  List.fold_left (fun (env, _) stmt -> emit_stmt env stmt)
                 (env, false) stmts

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
