(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module LSet = Loc_collections.LocSet
open Insert_type_utils
open Reason
open Loc_collections
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)

let max_intermediate_type_size = 1000

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe:false
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:false
        () as super

    method private post_run () = ()

    method private unsealed_annot loc ty =
      let { Codemod_context.Typed.full_cx = cx; file; file_sig; typed_ast; _ } = cctx in
      let aloc = ALoc.of_loc loc in
      let indexers = Context.inferred_indexers cx in
      let validate = function
        | Ok (Ty.Type ty) ->
          Codemod_annotator.validate_ty cctx ~max_type_size:max_intermediate_type_size ty
        | _ -> Error [Error.Missing_annotation_or_normalizer_error]
      in
      let obj_kind =
        match ALocMap.find_opt aloc indexers with
        | Some (_ :: _ as indexers) ->
          let keys =
            List.map (fun { Type.key; _ } -> key) indexers
            |> TypeUtil.union_of_ts (mk_reason RUnion aloc)
          in
          let values =
            List.map (fun { Type.value; _ } -> value) indexers
            |> TypeUtil.union_of_ts (mk_reason RUnion aloc)
          in
          let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~file_sig ~typed_ast in
          let options = Ty_normalizer_env.default_options in
          (match
             ( Ty_normalizer.from_type ~options ~genv keys |> validate,
               Ty_normalizer.from_type ~options ~genv values |> validate
             )
           with
          | (Ok dict_key, Ok dict_value) ->
            Ty.IndexedObj { Ty.dict_polarity = Ty.Neutral; dict_name = None; dict_key; dict_value }
          | (Error errs, Ok _)
          | (Ok _, Error errs) ->
            this#report_errors loc errs;
            ty.Ty.obj_kind
          | (Error errs_key, Error errs_val) ->
            this#report_errors loc (errs_key @ errs_val);
            ty.Ty.obj_kind)
        | None
        | Some [] ->
          ty.Ty.obj_kind
      in
      { ty with Ty.obj_kind }

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method private report_errors loc errors =
      Base.List.iter errors ~f:(fun e -> this#update_acc (fun acc -> Acc.error acc loc e));
      codemod_error_locs <- LSet.add loc codemod_error_locs

    method! call cloc expr =
      let open Ast.Expression in
      let open Call in
      let { callee; targs; arguments; comments } = expr in
      let open Member in
      match (callee, arguments, targs) with
      | ( (_, Member { property = PropertyIdentifier (_, { Ast.Identifier.name = "reduce"; _ }); _ }),
          (_, { ArgList.arguments = [_; Expression (loc, Object { Object.properties = []; _ })]; _ }),
          None
        ) ->
        let ty_result =
          Codemod_annotator.get_validated_ty
            cctx
            ~preserve_literals
            ~max_type_size:max_intermediate_type_size
            loc
        in
        (match ty_result with
        | Ok (Ty.Obj ty) ->
          let ty_obj = this#unsealed_annot loc ty in
          (match ty_obj.Ty.obj_kind with
          | Ty.IndexedObj _ ->
            let ty_obj = { ty_obj with Ty.obj_props = [] } in
            (match Codemod_annotator.validate_ty cctx ~max_type_size (Ty.Obj ty_obj) with
            | Ok ty ->
              (match this#get_annot cloc (Ok ty) (Ast.Type.Missing cloc) with
              | Ast.Type.Available (_, t) ->
                let targlist = [CallTypeArg.Explicit t] in
                let targs = Some (cloc, { CallTypeArgs.arguments = targlist; comments = None }) in
                { callee; arguments; comments; targs }
              | _ -> super#call cloc expr)
            | Error errs ->
              this#report_errors loc errs;
              super#call cloc expr)
          | _ -> super#call cloc expr)
        | Ok _ -> super#call cloc expr
        | Error errs ->
          this#report_errors loc errs;
          super#call cloc expr)
      | _ -> super#call cloc expr

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | (loc, Object { Object.properties = []; _ }) ->
        let ty_result =
          Codemod_annotator.get_validated_ty
            cctx
            ~preserve_literals
            ~max_type_size:max_intermediate_type_size
            loc
        in
        (match ty_result with
        | Ok (Ty.Obj ty) ->
          let ty_obj = this#unsealed_annot loc ty in
          (match ty_obj.Ty.obj_kind with
          | Ty.IndexedObj _ ->
            let ty_obj = { ty_obj with Ty.obj_props = [] } in
            (match Codemod_annotator.validate_ty cctx ~max_type_size (Ty.Obj ty_obj) with
            | Ok ty ->
              (match this#get_annot loc (Ok ty) (Ast.Type.Missing loc) with
              | Ast.Type.Available annot' ->
                ( loc,
                  Ast.Expression.TypeCast
                    { annot = annot'; expression = expr; Ast.Expression.TypeCast.comments = None }
                )
              | _ -> super#expression expr)
            | Error errs ->
              this#report_errors loc errs;
              super#expression expr)
          | _ -> super#expression expr)
        | Ok _ -> super#expression expr
        | Error errs ->
          this#report_errors loc errs;
          super#expression expr)
      | _ -> super#expression expr

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id; init }) = decl in
      match init with
      | Some (oloc, Ast.Expression.Object Ast.Expression.Object.{ properties = []; comments = _ })
        ->
        let id =
          match id with
          | ( ploc,
              Ast.Pattern.Identifier
                Ast.Pattern.Identifier.{ annot = Ast.Type.Missing _ as annot; name; optional }
            ) ->
            let ty_result =
              Codemod_annotator.get_validated_ty
                cctx
                ~preserve_literals
                ~max_type_size:max_intermediate_type_size
                ploc
            in
            (match ty_result with
            | Ok (Ty.Obj ty) ->
              let ty_obj = this#unsealed_annot oloc ty in
              (match ty_obj.Ty.obj_kind with
              | Ty.IndexedObj _ ->
                let ty_obj = { ty_obj with Ty.obj_props = [] } in
                (match Codemod_annotator.validate_ty cctx ~max_type_size (Ty.Obj ty_obj) with
                | Ok ty ->
                  let annot' = this#get_annot ploc (Ok ty) annot in
                  ( ploc,
                    Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional }
                  )
                | Error errs ->
                  this#report_errors loc errs;
                  id)
              | _ -> id)
            | Ok _ -> id
            | Error errs ->
              this#report_errors loc errs;
              id)
          | _ -> id
        in
        (loc, { id; init })
      | _ -> super#variable_declarator ~kind decl
  end
