(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Code for emitting stmts and blocks *)

open Core
open Utils
module N = Nast

open Emitter_core

(*** Stuff for handling break/continue/finally ***)
let assign_label env v =
  let env, l = fresh_label env in env, (l, v)

(* special cleanup that needs to be done for nonlocal exits in order
 * to handle finally *)
let loop_exit_cleanup ~is_initial env =
  if is_initial then env else
  let env, id = get_nonlocal_var env in
  emit_UnsetL env id

let default_jmp_action ~is_initial target env =
  let env = loop_exit_cleanup ~is_initial env in
  emit_Jmp env target

let default_return_action ~has_value ~is_initial env =
  let env =
    if not has_value then emit_Null env else
      if is_initial then env else
        (* came in from a finally; get the val from the variable *)
        let env, id = get_ret_var env in
        emit_CGetL env id
  in
  emit_RetC env

let with_targets env continue_target break_target f arg =
  let nonlocal = { env.nonlocal with
                   continue_action = default_jmp_action continue_target;
                   break_action = default_jmp_action break_target } in
  with_actions env nonlocal f arg


let is_empty_block = function
  | [] | [N.Noop] -> true
  | _ -> false

(*** Actual statement emission *)

(* Emit bytecode for a block. Stack should start and end empty.
 * Returns whether the block is terminal. *)
let rec emit_block env stmts =
  List.fold_left ~f:(fun (env, _) stmt -> emit_stmt env stmt)
                 ~init:(env, false) stmts

(* Emit bytecode for a statement. Stack should start and end empty.
 * Returns whether the statement is terminal. *)
and emit_stmt env stmt =
  match stmt with
  | N.Noop | N.Fallthrough -> env, false

  | N.Return (_, ret) ->
    let env = opt_fold Emitter_expr.emit_expr env ret in
    let env = env.nonlocal.return_action ~is_initial:true
                                         ~has_value:(ret <> None) env in
    env, true
  | N.Break _ -> env.nonlocal.break_action ~is_initial:true env, true
  | N.Continue _ -> env.nonlocal.continue_action ~is_initial:true env, true

  | N.Expr e ->
    Emitter_expr.emit_ignored_expr env e, false

  | N.If (e, btrue, bfalse) ->
    let env, false_label, end_label = fresh_labels_2 env in
    (* emit cond and jump to false case *)
    let env = Emitter_expr.emit_cond env e false false_label in

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

  | N.While (etest, body) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* we duplicate the condition for while loops *)
    (* XXX: should we always do this? *)

    (* emit cond and jmp past loop *)
    let env = Emitter_expr.emit_cond env etest false break_label in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the test again *)
    let env = emit_label env cont_label in
    let env = Emitter_expr.emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false

  | N.Do (body, etest) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the test *)
    let env = emit_label env cont_label in
    let env = Emitter_expr.emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false

  | N.For (einit, etest, estep, body) ->
    let env, top_label, break_label, cont_label = fresh_labels_3 env in

    (* we duplicate the condition for for loops *)
    (* XXX: should we always do this? *)

    (* emit init *)
    let env = Emitter_expr.emit_ignored_expr env einit in

    (* emit test and conditional jmp past loop *)
    let env = Emitter_expr.emit_cond env etest false break_label in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _terminal =
      with_targets env cont_label break_label emit_block body in

    (* emit the step *)
    let env = emit_label env cont_label in
    let env = Emitter_expr.emit_ignored_expr env estep in

    (* emit the test again *)
    let env = Emitter_expr.emit_cond env etest true top_label in
    let env = emit_label env break_label in

    env, false

  | N.Foreach (e, binding, body) ->
    let env = Emitter_expr.emit_expr env e in

    let env, top_label, break_label, cont_label = fresh_labels_3 env in
    let env, faultlet = fresh_faultlet env in
    let env, iter = fresh_iterator env in

    let env, id = match binding with
      (* Hack only accepts variable names here, which makes life easier. *)
      | N.As_v (_, N.Lvar id) -> env, [get_lid_name id]
      | N.As_kv ((_, N.Lvar id1), (_, N.Lvar id2)) ->
        env, [get_lid_name id2; get_lid_name id1]
      | _ -> unimpl "await bindings"
    in

    let env = emit_IterInit env iter break_label id in
    let env = emit_fault_enter env faultlet in

    (* Nonlocal exit handling in foreach bodies is somewhat subtle:
     * continue: nothing weird
     * break: needs to use IterBreak to free the iterator while breaking
     * return: needs to IterFree, then execute the return action *)
    let old_return_action = env.nonlocal.return_action in
    let return_action ~has_value ~is_initial env =
      let env = emit_IterFree env iter in
      old_return_action ~has_value ~is_initial env
    in
    let break_action ~is_initial env =
      let env = loop_exit_cleanup ~is_initial env in
      emit_IterBreak env [iter] break_label
    in
    let actions = {
      break_action = break_action;
      continue_action = default_jmp_action cont_label;
      return_action = return_action;
    } in

    (* emit loop body *)
    let env = emit_label env top_label in
    let env, _ = with_actions env actions emit_block body in

    (* emit the step *)
    let env = emit_label env cont_label in
    let env = emit_IterNext env iter top_label id in

    (* close out the loop, finish up the faultlet *)
    let env = emit_fault_exit env faultlet in
    let env =
      emit_fault_cleanup env
          ~faultlet_extras:(fun env -> emit_IterFree env iter)
          ~cleanup:(fun env -> env)
          faultlet
    in
    let env = emit_label env break_label in

    env, false

  | N.Throw (is_terminal, e) ->
    let env = Emitter_expr.emit_expr env e in
    emit_Throw env, is_terminal

  | N.Try (try_body, catches, finally_body) ->
    let env, target = fresh_label env in
    let env, catch_labels = List.map_env env catches begin fun env _ ->
      fresh_catch env
    end in
    let catches = List.zip_exn catch_labels catches in

    let fmt_catch_hdr (label, ((_, cls), _, _)) =
      "(" ^ fmt_name cls ^ " " ^ label ^ ")" in
    let catch_hdrs = String.concat " " (List.rev_map catches fmt_catch_hdr) in

    (* If we have a finally, generate a finally label
     * and output a try_fault handler *)
    let has_finally = not (is_empty_block finally_body) in
    let env, opt_faultlet = make_opt_faultlet env has_finally in
    let env = opt_fold emit_fault_enter env opt_faultlet in

    (* emit the try block *)
    let emit_try_and_catches env () =
      let env = if catches = [] then env else
                emit_enter env ".try_catch" [] catch_hdrs "" in
      let env, _ = emit_block env try_body in
      let env = emit_Jmp env target in
      let env = if catches = [] then env else emit_exit env in

      let ncatches = List.length catches in
      let emit_catch i env (label, (_, var, body)) =
        let env = emit_label env label in
        let env = emit_Catch env in
        let env = emit_Set env (llocal var) in
        let env = emit_PopC env in
        let env, _ = emit_block env body in
        (* want to be able to skip the jump on the last one *)
        if i = ncatches-1 then env else
        emit_Jmp env target
      in

      let env = List.foldi ~f:emit_catch ~init:env catches in
      env, ()
    in

    let env = match opt_faultlet with
    (* If no finally, just emit the try/catch and the output label *)
    | None ->
      let env, () = emit_try_and_catches env () in
      emit_label env target
    (* If we had a finally, also output the finally code and register
     * a cleanup that will emit the faultlet. Somewhat more subtly,
     * we also need to arrange for nonlocal exits (break/continue/return)
     * from inside the try to excute the finally block; this gets pretty
     * hairy. *)
    | Some finally_label ->
      let had_break, had_continue, had_ret = ref false, ref false, ref false in
      (* We handle nonlocal exits from the try block by setting a flag
       * variable that indicates which nonlocal exit we are taking. We
       * only need to set this flag when the action is initiated,
       * not when it gets dispatched to from another finally block. *)
      let loop_action num r ~is_initial env =
        let env = if not is_initial then env else
          let env, id = get_nonlocal_var env in
          let env = emit_Int env num in
          let env = emit_SetL env id in
          emit_PopC env
        in
        r := true;
        emit_Jmp env target
      in
      let had_value = ref false in
      let return_action num ~has_value ~is_initial env =
        had_value := has_value;
        let env = if not (is_initial && has_value) then env else
          let env, id = get_ret_var env in
          let env = emit_SetL env id in
          emit_PopC env
        in
        loop_action num had_ret ~is_initial env
      in

      let actions = {
        (* XXX: these numbers are hardcoded and kind of janky;
         * hhvm generates them per function based on which are
         * actually used, I think? *)
        break_action = loop_action "0" had_break;
        continue_action = loop_action "1" had_continue;
        return_action = return_action "2"
      } in

      let tmpvar_cleanup env =
        let env, id = get_nonlocal_var env in
        let env = emit_UnsetL env id in
        let env, id = get_ret_var env in
        emit_UnsetL env id
      in

      let env, () = with_actions env actions emit_try_and_catches () in
      (* Emit exit and finally *)
      let env = emit_fault_exit env finally_label in
      let env = emit_label env target in
      let env =
        emit_fault_cleanup env
          ~faultlet_extras:tmpvar_cleanup
          ~cleanup:(fun env -> fst (emit_block env finally_body))
          finally_label in

      (* Now that we've run the finally block, we need to return control
       * to wherever it is supposed to be. *)
      (* XXX: the ordering of targets needs to match the numbers above *)
      let targets = [
        !had_break, env.nonlocal.break_action;
        !had_continue, env.nonlocal.continue_action;
        !had_ret, env.nonlocal.return_action ~has_value:(!had_value)
      ] in
      let env, targets = List.map_env env targets assign_label in
      let active_targets = List.filter ~f:(fun (_, (had, _)) -> had) targets in

      (* Which of break/return/continue are actually used? *)
      let env = match active_targets with
      (* None used: don't need to handle nonlocal flow *)
      | [] -> env
      | _ ->
        (* There is nonlocal flow; first, check if the nonlocal
         * flag variable is set; if not, just run the regular code *)
        let env, id = get_nonlocal_var env in
        let env, out_label = fresh_label env in
        let env = emit_IssetL env id in
        let env = emit_cjmp env false out_label in

        (* If there is only one that came up, just do it; otherwise
         * we need to switch *)
        let env = match active_targets with
        | [_, (_, f)] -> f ~is_initial:false env
        | _ ->
          let emit_target env (label, (had, f)) =
            let env = emit_label env label in
            if not had then env else
            f ~is_initial:false env
          in
          let labels = List.map ~f:fst targets in
          let env = emit_CGetL env id in
          let env = emit_Switch env labels 0 "Unbounded" in
          List.fold_left ~f:emit_target ~init:env targets
        in
        emit_label env out_label
      in

      env
    in

    env, false

  (* TODO: support Switch/SSwitch *)
  | N.Switch (e, cases) ->
    let cases, defaults = List.partition_map ~f:begin function
      | N.Case (e, b) -> `Fst (e, b)
      | N.Default b -> `Snd b
    end cases in

    (* If the expr isn't a local, make a temp local and assign to it *)
    let env, opt_faultlet, id = Emitter_expr.emit_expr_to_var env e in
    let env = opt_fold emit_fault_enter env opt_faultlet in

    let env, end_label = fresh_label env in
    let env, cases = List.map_env env cases assign_label in
    let env, defaults = List.map_env env defaults assign_label in

    (* emit the compare/jmp sequence *)
    let emit_compare env (label, (expr, _)) =
      let env = Emitter_expr.emit_expr env expr in
      let env = emit_CGetL env id in
      let env = emit_binop env (Ast.Eqeq) in
      emit_cjmp env true label
    in
    let env = List.fold_left ~f:emit_compare ~init:env cases in
    (* emit either a jump to the default or a jump to the end *)
    let env = match defaults with | [def_label, _] -> emit_Jmp env def_label
                                  | _ -> emit_Jmp env end_label in

    (* emit the actual blocks *)
    let case_blocks = List.map ~f:(fun (l, (_, b)) -> l, b) cases in
    let all_blocks = case_blocks @ defaults in
    let emit_block env (label, body) =
      let env = emit_label env label in
      (* set the break and continue targets to be the end label *)
      let env, _ = with_targets env end_label end_label emit_block body in
      env
    in
    let env = List.fold_left ~f:emit_block ~init:env all_blocks in

    let env = opt_fold emit_fault_exit env opt_faultlet in
    let env = emit_label env end_label in
    (* we need to unset the variable when we leave *)
    let cleanup env = emit_UnsetL env id in
    let env = opt_fold (emit_fault_cleanup ~cleanup:cleanup) env opt_faultlet in

    env, false

  | N.Static_var _ -> unimpl "static variables"
