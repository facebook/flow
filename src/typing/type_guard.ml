(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
module Ast = Flow_ast
module Flow = Flow_js

(* This check to be performed after the function has been checked to ensure all
 * entries have been prepared for type checking. *)
let check_type_guard_consistency cx reason one_sided param_loc tg_param tg_reason type_guard =
  let env = Context.environment cx in
  let { Loc_env.var_info; _ } = env in
  let { Env_api.type_guard_consistency_maps; _ } = var_info in
  let (name_loc, name) = tg_param in
  let param_reason = mk_reason (RParameter (Some name)) param_loc in
  match Loc_collections.ALocMap.find_opt name_loc type_guard_consistency_maps with
  | None ->
    (* Entry missing when function does not return. Error raised in Func_sig. *)
    ()
  | Some (Some havoced_loc_set, _) ->
    Flow_js_utils.add_output
      cx
      Error_message.(
        ETypeGuardFunctionParamHavoced
          {
            param_reason;
            type_guard_reason = tg_reason;
            call_locs = Loc_collections.ALocSet.elements havoced_loc_set;
          }
      )
  | Some (None, reads) ->
    (* Each read corresponds to a return expression. *)
    Base.List.iter
      reads
      ~f:(fun (ret_expr, return_reason, { Env_api.write_locs = pos_write_locs; _ }, neg_refi) ->
        let is_return_false_statement =
          match ret_expr with
          | Some (_, Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = false; _ }) -> true
          | _ -> false
        in
        let return_loc = Reason.loc_of_reason return_reason in
        match
          Type_env.type_guard_at_return
            cx
            param_reason
            ~param_loc
            ~return_loc
            ~pos_write_locs
            ~neg_refi:(neg_refi, param_loc, Pattern_helper.Root)
        with
        | Ok (t, neg_pred) ->
          (* Positive *)
          let guard_type_reason = reason_of_t type_guard in
          let use_op =
            Op
              (PositiveTypeGuardConsistency
                 {
                   reason;
                   param_reason;
                   guard_type_reason;
                   return_reason;
                   is_return_false_statement;
                 }
              )
          in
          Flow.flow cx (t, UseT (use_op, type_guard));
          (* Negative *)
          if not one_sided then
            let type_guard_with_neg_pred =
              match neg_pred with
              | None -> type_guard
              | Some neg_pred ->
                Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where
                  cx
                  tg_reason
                  (Predicate_kit.run_predicate_for_filtering cx type_guard neg_pred)
            in
            if
              not
                (Flow_js.FlowJs.speculative_subtyping_succeeds
                   cx
                   type_guard_with_neg_pred
                   (EmptyT.at ALoc.none)
                )
            then
              Flow_js_utils.add_output
                cx
                Error_message.(
                  ENegativeTypeGuardConsistency
                    { reason; return_reason; type_reason = reason_of_t type_guard }
                )
        | Error write_locs ->
          Flow_js_utils.add_output
            cx
            Error_message.(
              ETypeGuardFunctionInvalidWrites
                { reason = return_reason; type_guard_reason = tg_reason; write_locs }
            )
    )

let check_type_guard cx params (TypeGuard { reason; one_sided; param_name; type_guard }) =
  let err_with_desc desc type_guard_reason binding_loc =
    let binding_reason = mk_reason desc binding_loc in
    Flow_js.add_output
      cx
      Error_message.(ETypeGuardInvalidParameter { type_guard_reason; binding_reason })
  in
  let error_on_non_root_binding name expr_reason binding =
    let open Pattern_helper in
    match binding with
    | (_, Root) -> ()
    | (loc, Rest) -> err_with_desc (RRestParameter (Some name)) expr_reason loc
    | (loc, Select _) -> err_with_desc (RPatternParameter name) expr_reason loc
  in
  let (name_loc, name) = param_name in
  let tg_reason = mk_reason (RTypeGuardParam name) name_loc in
  match SMap.find_opt name (Pattern_helper.bindings_of_params params) with
  | None -> Flow_js_utils.add_output cx Error_message.(ETypeGuardParamUnbound tg_reason)
  | Some (param_loc, Pattern_helper.Root) ->
    check_type_guard_consistency cx reason one_sided param_loc param_name tg_reason type_guard
  | Some binding -> error_on_non_root_binding name tg_reason binding
