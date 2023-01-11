(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module ALocFuzzyMap = Loc_collections.ALocFuzzyMap
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Hardcoded_Ty_Fixes = Codemod_hardcoded_ty_fixes.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)
open Insert_type_utils

let mapper
    ~ignore_suppressed
    ~file_options
    ~preserve_literals
    ~generalize_maybe
    ~annotate_special_fun_return
    ~max_type_size
    ~default_any
    ~provided_error_set
    (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  let reader = cctx.Codemod_context.Typed.reader in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let cx = Codemod_context.Typed.context cctx in
  let errors = Flow_error.ErrorSet.union (Context.errors cx) provided_error_set in
  let errors =
    if ignore_suppressed then
      Error_suppressions.filter_suppressed_error_set
        ~root:(Context.root cx)
        ~file_options:(Some file_options)
        ~loc_of_aloc
        (Context.error_suppressions cx)
        errors
    else
      errors
  in
  let implicit_instantiation_aloc_results =
    Codemod_context.Typed.context cctx |> Context.implicit_instantiation_ty_results
  in
  let implicit_instantiation_results =
    ALocFuzzyMap.fold
      (fun aloc result acc ->
        let loc = loc_of_aloc aloc in
        LMap.add loc result acc)
      implicit_instantiation_aloc_results
      LMap.empty
  in

  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true
        ~generalize_react_mixed_element:true
        () as super

    val mutable loc_error_map = LMap.empty

    method private get_annot ploc ty annot =
      let f loc _annot ty' =
        this#annotate_node loc ty' (fun (_, a) -> Ast.Expression.CallTypeArg.Explicit a)
      in
      let error _ = Ast.Expression.CallTypeArg.Explicit flowfixme_ast in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method private fix_and_validate loc ty =
      let (acc', ty) =
        Hardcoded_Ty_Fixes.run
          ~cctx
          ~preserve_literals
          ~generalize_maybe:false
          ~merge_arrays:false
          ~generalize_react_mixed_element:true
          acc
          loc
          ty
      in
      this#set_acc acc';
      Codemod_annotator.validate_ty cctx ~max_type_size ty

    method private annotate_special_call_args loc expr =
      let { Ast.Expression.Call.callee; arguments; targs; _ } = expr in
      match callee with
      | ( _,
          Ast.Expression.Member
            {
              Ast.Expression.Member.property =
                Ast.Expression.Member.PropertyIdentifier
                  (_, { Ast.Identifier.name = "map"; comments = _ });
              _;
            }
        ) ->
        (match this#get_implicit_instantiation_results loc targs with
        | Some targs ->
          let open Ast.Expression.ArgList in
          let (loc, { arguments; comments }) = arguments in
          let annotate_not_fully_annotated_function func =
            match func.Ast.Function.return with
            | Ast.Type.Available _ -> None
            | Ast.Type.Missing ret_loc ->
              (match targs with
              | ( _,
                  {
                    Ast.Expression.CallTypeArgs.arguments =
                      Ast.Expression.CallTypeArg.Explicit t :: _;
                    _;
                  }
                ) ->
                Some { func with Ast.Function.return = Ast.Type.Available (ret_loc, t) }
              | _ -> None)
          in
          let annot_expr (loc, expr) =
            let open Ast.Expression in
            match expr with
            | ArrowFunction func ->
              annotate_not_fully_annotated_function func
              |> Base.Option.map ~f:(fun f -> (loc, ArrowFunction f))
            | Function func ->
              annotate_not_fully_annotated_function func
              |> Base.Option.map ~f:(fun f -> (loc, Function f))
            | _ -> None
          in
          let (annotated, args_rev) =
            Base.List.fold arguments ~init:(false, []) ~f:(fun (acc_annotated, acc_args_rev) -> function
              | Ast.Expression.Expression expr ->
                let annotated_expr = annot_expr expr in
                ( acc_annotated || Base.Option.is_some annotated_expr,
                  Ast.Expression.Expression (Base.Option.value ~default:expr annotated_expr)
                  :: acc_args_rev
                )
              | Ast.Expression.Spread (loc, spread) ->
                let annotated_expr = annot_expr spread.Ast.Expression.SpreadElement.argument in
                ( acc_annotated || Base.Option.is_some annotated_expr,
                  Ast.Expression.Spread
                    ( loc,
                      {
                        spread with
                        Ast.Expression.SpreadElement.argument =
                          Base.Option.value
                            ~default:spread.Ast.Expression.SpreadElement.argument
                            annotated_expr;
                      }
                    )
                  :: acc_args_rev
                )
            )
          in
          if annotated then
            Some (loc, { arguments = List.rev args_rev; comments })
          else
            None
        | _ -> None)
      | _ -> None

    method! call loc expr =
      let expr = super#call loc expr in
      if annotate_special_fun_return then
        match this#annotate_special_call_args loc expr with
        | Some arguments -> { expr with Ast.Expression.Call.arguments }
        | _ -> expr
      else
        let open Ast.Expression.Call in
        let targs = this#get_implicit_instantiation_results loc expr.targs in
        match targs with
        | Some targs -> { expr with targs = Some targs }
        | None -> expr

    method! new_ loc expr =
      let expr = super#new_ loc expr in
      if annotate_special_fun_return then
        expr
      else
        let open Ast.Expression.New in
        let targs = this#get_implicit_instantiation_results loc expr.targs in
        match targs with
        | Some targs -> { expr with targs = Some targs }
        | None -> expr

    method private get_implicit_instantiation_results loc current_targs =
      let is_ith_targ_annotated i =
        match current_targs with
        | None -> false
        | Some (_, { Ast.Expression.CallTypeArgs.arguments = targs; _ }) ->
          (match List.nth_opt targs i with
          | Some (Ast.Expression.CallTypeArg.Explicit _) -> true
          | _ -> false)
      in
      match (LMap.find_opt loc implicit_instantiation_results, LMap.find_opt loc loc_error_map) with
      | (Some targ_tys_with_names, Some tparam_names) ->
        let arguments =
          List.mapi
            (fun i (ty, name) ->
              let open Ast.Expression.CallTypeArg in
              match ty with
              | _
                when (not (SSet.mem (Subst_name.string_of_subst_name name) tparam_names))
                     && not (is_ith_targ_annotated i) ->
                Implicit (loc, { Implicit.comments = None })
              | None -> Explicit flowfixme_ast
              | Some ty ->
                let default = Implicit (loc, { Implicit.comments = None }) in
                let ty_result = this#fix_and_validate loc ty in
                this#get_annot loc ty_result default)
            targ_tys_with_names
        in
        let targs = { Ast.Expression.CallTypeArgs.comments = None; arguments } in
        let has_explicit_targ =
          List.exists
            (fun targ ->
              match targ with
              | Ast.Expression.CallTypeArg.Explicit _ -> true
              | _ -> false)
            arguments
        in
        if has_explicit_targ then
          Some (loc, targs)
        else
          None
      | _ -> None

    method private init_loc_error_map =
      loc_error_map <-
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.EImplicitInstantiationUnderconstrainedError { bound; reason_call; _ }
            | Error_message.EImplicitInstantiationWidenedError { bound; reason_call; _ } ->
              let loc = Reason.aloc_of_reason reason_call in
              LMap.update
                (loc_of_aloc loc)
                (function
                  | None -> Some (SSet.singleton bound)
                  | Some names -> Some (SSet.add bound names))
                acc
            | _ -> acc)
          errors
          loc_error_map

    method private post_run () = ()

    method! program prog =
      this#init_loc_error_map;
      if LMap.is_empty loc_error_map then
        prog
      else
        super#program prog
  end
