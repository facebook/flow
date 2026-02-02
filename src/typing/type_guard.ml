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
          | (_, Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = false; _ }) -> true
          | _ -> false
        in
        let is_return_true_statement =
          match ret_expr with
          | (_, Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = true; _ }) -> true
          | _ -> false
        in
        let return_loc = Reason.loc_of_reason return_reason in
        match
          Type_env.checked_type_guard_at_return
            cx
            param_reason
            ~param_loc
            ~return_loc
            ~pos_write_locs
            ~neg_refi
        with
        | Ok (t, neg_pred) ->
          (* Positive *)
          ( if not is_return_false_statement then
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
            Flow.flow cx (t, UseT (use_op, type_guard))
          );
          (* Negative *)
          if (not one_sided) && not is_return_true_statement then
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

let check_type_guard cx params (TypeGuard { reason; inferred; one_sided; param_name; type_guard }) =
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
  if not inferred then
    let (name_loc, name) = param_name in
    let tg_reason = mk_reason (RTypeGuardParam name) name_loc in
    match SMap.find_opt name (Pattern_helper.bindings_of_params params) with
    | None ->
      if name = "this" then
        Flow_js_utils.add_output cx Error_message.(ETypeGuardThisParam (mk_reason RThis name_loc))
      else
        Flow_js_utils.add_output cx Error_message.(ETypeGuardParamUnbound tg_reason)
    | Some (param_loc, Pattern_helper.Root) ->
      check_type_guard_consistency cx reason one_sided param_loc param_name tg_reason type_guard
    | Some binding -> error_on_non_root_binding name tg_reason binding

(* Given an function of the form `(x: T) => e`, we will infer a type guard iff
 * 1. the body `e` encodes some refinement,
 * 2. the refinement P encoded in `e` does not include the truthy predicate,
 * 3. P narrows the type of `x`, ie. if T' the type of `x` after P has been
 *    applied to T, then it has to be the case that T </: T'. Due to T' being a
 *    refined version of T it trivially holds that T' <: T.
 *)

(* All predicates are allowed to contribute to the inferred type guard except
 * for the trivial truthy predicate to avoid things like: `(x: mixed) => x`. *)
let is_inferable_type_guard_predicate =
  let exception Invalid in
  let visitor =
    object
      inherit [unit] Type_visitor.t as super

      method! predicate cx acc p =
        match p with
        | TruthyP -> raise Invalid
        | _ -> super#predicate cx acc p
    end
  in
  fun cx p ->
    match visitor#predicate cx () p with
    | exception Invalid -> false
    | _ -> true

let is_inferable_type_guard_read cx read =
  match Type_env.read_to_predicate cx read with
  | Some p -> is_inferable_type_guard_predicate cx p
  | None -> false

let infer_type_guard_from_read ~infer_expr cx name return_expr return_reason read =
  let { Env_api.write_locs; _ } = read in
  let (param_loc, { Ast.Identifier.name = pname; _ }) = name in
  let param_reason = mk_reason (RParameter (Some pname)) param_loc in
  let mk_guard type_guard =
    TypeGuard
      {
        inferred = true;
        reason = param_reason;
        param_name = (param_loc, pname);
        type_guard;
        one_sided = true;
      }
  in
  let returns_bool () =
    let ((_, body_t), _) = infer_expr cx return_expr in
    Flow_js.FlowJs.speculative_subtyping_succeeds cx body_t (DefT (reason_of_t body_t, BoolGeneralT))
  in
  if Context.typing_mode cx <> Context.CheckingMode then
    None
  else if is_inferable_type_guard_read cx read then
    let return_loc = Reason.loc_of_reason return_reason in
    let param_t = Type_env.find_write cx Env_api.OrdinaryNameLoc param_reason in
    let guard_t = Type_env.inferred_type_guard_at_return cx param_reason ~return_loc ~write_locs in
    (* Only keep the type guard if the function is actually refining the input
     * and is returning a boolean expression. *)
    if Flow_js.FlowJs.speculative_subtyping_succeeds cx param_t guard_t || not (returns_bool ())
    then
      None
    else
      Some (mk_guard guard_t)
  else
    None

let infer_type_guard cx ~infer_expr params =
  let env = Context.environment cx in
  let { Loc_env.var_info; _ } = env in
  let { Env_api.type_guard_consistency_maps; _ } = var_info in
  match params with
  | ( _,
      {
        Ast.Function.Params.params =
          [
            ( _,
              Ast.Function.Param.RegularParam
                { argument = (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }); _ }
            );
          ];
        rest = None;
        _;
      }
    ) ->
    let (param_loc, _) = name in
    begin
      match Loc_collections.ALocMap.find_opt param_loc type_guard_consistency_maps with
      | None -> None
      | Some (Some _havoced_loc_set, _) -> None
      | Some (None, [(ret_expr, return_reason, read, _)]) ->
        infer_type_guard_from_read ~infer_expr cx name ret_expr return_reason read
      | Some (None, _) -> None
    end
  | _ -> None
