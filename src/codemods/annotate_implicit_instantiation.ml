(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
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
            (fun t ->
              match Ty_normalizer.from_type ~options:ty_normalizer_options ~genv t with
              | Ok (Ty.Type ty) -> Some (Ok ty)
              | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> Some (Ok (Ty.TypeOf (Ty.TSymbol s)))
              | _ -> None)
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

    val mutable loc_error_set = LSet.empty

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
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
      match LMap.find_opt loc implicit_instantiation_results with
      | None -> None
      | Some targ_tys ->
        let targs =
          {
            Ast.Expression.CallTypeArgs.comments = None;
            arguments =
              List.map
                (fun ty ->
                  match ty with
                  | None -> Ast.Expression.CallTypeArg.Explicit flowfixme_ast
                  | Some ty ->
                    (match
                       this#get_annot loc (ty >>= this#fix_and_validate loc) (Ast.Type.Missing loc)
                     with
                    | Ast.Type.Available (_, t) -> Ast.Expression.CallTypeArg.Explicit t
                    | Ast.Type.Missing _ -> Ast.Expression.CallTypeArg.Explicit flowfixme_ast))
                targ_tys;
          }
        in
        Some (loc, targs)

    method private init_loc_error_set =
      loc_error_set <-
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.EImplicitInstantiationUnderconstrainedError _ ->
              (match Flow_error.loc_of_error error with
              | Some loc -> LSet.add (loc_of_aloc loc) acc
              | None -> acc)
            | _ -> acc)
          errors
          loc_error_set

    method private post_run () = ()

    method! program prog =
      this#init_loc_error_set;
      if LSet.is_empty loc_error_set then
        prog
      else
        super#program prog
  end
