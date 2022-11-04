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
    ~preserve_literals ~generalize_maybe ~max_type_size ~default_any (cctx : Codemod_context.Typed.t)
    =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  let reader = cctx.Codemod_context.Typed.reader in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let errors = Codemod_context.Typed.context cctx |> Context.errors in
  let implicit_instantiation_aloc_results =
    Codemod_context.Typed.context cctx |> Context.implicit_instantiation_results
  in
  let ty_normalizer_options = Ty_normalizer_env.default_options in
  let typed_ast = Codemod_context.Typed.typed_ast cctx in
  let file_sig = Codemod_context.Typed.file_sig cctx in
  let full_cx = Codemod_context.Typed.context cctx in
  let file = Codemod_context.Typed.file cctx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~typed_ast in
  let implicit_instantiation_results =
    ALocFuzzyMap.fold
      (fun aloc result acc ->
        let loc = loc_of_aloc aloc in
        let call_args =
          List.map
            (fun (t, name) ->
              ( (match Ty_normalizer.from_type ~options:ty_normalizer_options ~genv t with
                | Ok (Ty.Type ty) -> Some (Ok ty)
                | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> Some (Ok (Ty.TypeOf (Ty.TSymbol s)))
                | _ -> None),
                name
              ))
            result
        in
        LMap.add loc call_args acc)
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

    method! call loc expr =
      let expr = super#call loc expr in
      let targs = this#get_implicit_instantiation_results loc in
      let open Ast.Expression.Call in
      match targs with
      | Some targs -> { expr with targs = Some targs }
      | None -> expr

    method! new_ loc expr =
      let expr = super#new_ loc expr in
      let targs = this#get_implicit_instantiation_results loc in
      let open Ast.Expression.New in
      match targs with
      | Some targs -> { expr with targs = Some targs }
      | None -> expr

    method private get_implicit_instantiation_results loc =
      match (LMap.find_opt loc implicit_instantiation_results, LMap.find_opt loc loc_error_map) with
      | (Some targ_tys_with_names, Some tparam_names) ->
        let targs =
          {
            Ast.Expression.CallTypeArgs.comments = None;
            arguments =
              List.map
                (fun (ty, name) ->
                  let open Ast.Expression.CallTypeArg in
                  match ty with
                  | _ when not (SSet.mem (Subst_name.string_of_subst_name name) tparam_names) ->
                    Implicit (loc, { Implicit.comments = None })
                  | None -> Explicit flowfixme_ast
                  | Some ty ->
                    let default = Implicit (loc, { Implicit.comments = None }) in
                    let ty_result = ty >>= this#fix_and_validate loc in
                    this#get_annot loc ty_result default)
                targ_tys_with_names;
          }
        in
        Some (loc, targs)
      | _ -> None

    method private init_loc_error_map =
      loc_error_map <-
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.(EImplicitInstantiationUnderconstrainedError { bound; _ }) ->
              (match Flow_error.loc_of_error error with
              | Some loc ->
                LMap.update
                  (loc_of_aloc loc)
                  (function
                    | None -> Some (SSet.singleton bound)
                    | Some names -> Some (SSet.add bound names))
                  acc
              | None -> acc)
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
