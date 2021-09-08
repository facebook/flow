(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

module ErrorStats = struct
  type t = { num_total_errors: int }

  let empty = { num_total_errors = 0 }

  let combine c1 c2 = { num_total_errors = c1.num_total_errors + c2.num_total_errors }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_total_errors]

  let report s = [string_of_row ~indent:2 "Number of empty object errors" s.num_total_errors]
end

module Codemod_empty_annotator = Codemod_annotator.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let { Codemod_context.Typed.file_sig; docblock; metadata; options; _ } = cctx in
  let imports_react = Insert_type_imports.ImportsHelper.imports_react file_sig in
  let metadata = Context.docblock_overrides docblock metadata in
  let { Context.strict; strict_local; _ } = metadata in
  let lint_severities =
    if strict || strict_local then
      StrictModeSettings.fold
        (fun lint_kind lint_severities ->
          LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
        (Options.strict_mode options)
        (Options.lint_severities options)
    else
      Options.lint_severities options
  in
  let suppress_types = Options.suppress_types options in
  let exact_by_default = Options.exact_by_default options in
  let flowfixme_ast = Builtins.flowfixme_ast ~lint_severities ~suppress_types ~exact_by_default in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        ~max_type_size
        ~exact_by_default
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~default_any
        cctx as super

    method private post_run () = ErrorStats.{ num_total_errors = 0 }

    method private unsealed_annot loc ty =
      let { Codemod_context.Typed.full_cx = cx; file; file_sig; typed_ast; _ } = cctx in
      let loc = ALoc.of_loc loc in
      let indexers = Context.inferred_indexers cx in
      let validate = function
        | Ok (Ty.Type ty) -> Codemod_annotator.validate_ty cctx ~max_type_size ty
        | _ ->
          let errors = [Error.Missing_annotation_or_normalizer_error] in
          Error (errors, Ty.explicit_any)
      in
      let obj_kind =
        match ALocMap.find_opt loc indexers with
        | Some (_ :: _ as indexers) ->
          let keys =
            List.map (fun { Type.key; _ } -> key) indexers
            |> TypeUtil.union_of_ts (mk_reason RUnion loc)
          in
          let values =
            List.map (fun { Type.value; _ } -> value) indexers
            |> TypeUtil.union_of_ts (mk_reason RUnion loc)
          in
          let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~file_sig ~typed_ast in
          let options = Ty_normalizer_env.default_options in
          begin
            match
              ( Ty_normalizer.from_type ~options ~genv keys |> validate,
                Ty_normalizer.from_type ~options ~genv values |> validate )
            with
            | (Ok dict_key, Ok dict_value) ->
              Ty.IndexedObj
                { Ty.dict_polarity = Ty.Neutral; dict_name = None; dict_key; dict_value }
            | _ -> ty.Ty.obj_kind
          end
        | None
        | Some [] ->
          ty.Ty.obj_kind
      in
      { ty with Ty.obj_kind }

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method! call cloc expr =
      let open Ast.Expression in
      let open Call in
      let { callee; targs; arguments; comments } = expr in
      let open Member in
      match (callee, arguments, targs) with
      | ( (_, Member { property = PropertyIdentifier (_, { Ast.Identifier.name = "reduce"; _ }); _ }),
          (_, { ArgList.arguments = [_; Expression (loc, Object { Object.properties = []; _ })]; _ }),
          None ) ->
        let ty_result = Codemod_annotator.get_ty cctx ~preserve_literals ~max_type_size loc in
        (match ty_result with
        | Ok (Ty.Obj ty) ->
          let ty_obj = this#unsealed_annot loc ty in
          begin
            match ty_obj.Ty.obj_kind with
            | Ty.IndexedObj _ ->
              let ty_obj = { ty_obj with Ty.obj_props = [] } in
              (match this#get_annot cloc (Ok (Ty.Obj ty_obj)) (Ast.Type.Missing cloc) with
              | Ast.Type.Available (_, t) ->
                let targlist = [CallTypeArg.Explicit t] in
                let targs = Some (cloc, { CallTypeArgs.arguments = targlist; comments = None }) in
                { callee; arguments; comments; targs }
              | _ -> super#call cloc expr)
            | _ -> super#call cloc expr
          end
        | _ -> super#call cloc expr)
      | _ -> super#call cloc expr

    method! expression expr =
      let open Ast.Expression in
      match expr with
      | (loc, Object { Object.properties = []; _ }) ->
        let ty_result = Codemod_annotator.get_ty cctx ~preserve_literals ~max_type_size loc in
        (match ty_result with
        | Ok (Ty.Obj ty) ->
          let ty_obj = this#unsealed_annot loc ty in
          begin
            match ty_obj.Ty.obj_kind with
            | Ty.IndexedObj _ ->
              let ty_obj = { ty_obj with Ty.obj_props = [] } in
              (match this#get_annot loc (Ok (Ty.Obj ty_obj)) (Ast.Type.Missing loc) with
              | Ast.Type.Available annot' ->
                ( loc,
                  Ast.Expression.TypeCast
                    { annot = annot'; expression = expr; Ast.Expression.TypeCast.comments = None }
                )
              | _ -> super#expression expr)
            | _ -> super#expression expr
          end
        | _ -> super#expression expr)
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
                Ast.Pattern.Identifier.{ annot = Ast.Type.Missing _ as annot; name; optional } ) ->
            let ty_result = Codemod_annotator.get_ty cctx ~preserve_literals ~max_type_size ploc in
            (match ty_result with
            | Ok (Ty.Obj ty) ->
              let ty_obj = this#unsealed_annot oloc ty in
              begin
                match ty_obj.Ty.obj_kind with
                | Ty.IndexedObj _ ->
                  let ty_obj = { ty_obj with Ty.obj_props = [] } in
                  let annot' = this#get_annot ploc (Ok (Ty.Obj ty_obj)) annot in
                  ( ploc,
                    Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional }
                  )
                | _ -> id
              end
            | _ -> id)
          | _ -> id
        in
        (loc, { id; init })
      | _ -> super#variable_declarator ~kind decl
  end
