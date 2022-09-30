(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)

let mapper
    ~preserve_literals
    ~generalize_maybe
    ~generalize_react_mixed_element
    ~max_type_size
    ~default_any
    (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~generalize_react_mixed_element
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true
        () as super

    method private post_run () = ()

    method private get_annot ploc annot =
      match annot with
      | Ast.Type.Available _ -> annot
      | Ast.Type.Missing _ ->
        let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
        let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
        let ty_result =
          Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size ploc
          |> Base.Result.bind ~f:(function
                 | Ty.Bot _ ->
                   Error [Insert_type_utils.Error.Missing_annotation_or_normalizer_error]
                 | t -> Ok t
                 )
        in
        this#opt_annotate ~f ~error ~expr:None ploc ty_result annot

    method private annotate_fn_param ploc patt =
      match patt with
      | Ast.Pattern.Object Ast.Pattern.Object.{ annot; properties; comments } ->
        let annot' = this#get_annot ploc annot in
        (ploc, Ast.Pattern.Object Ast.Pattern.Object.{ annot = annot'; properties; comments })
      | Ast.Pattern.Array Ast.Pattern.Array.{ annot; elements; comments } ->
        let annot' = this#get_annot ploc annot in
        (ploc, Ast.Pattern.Array Ast.Pattern.Array.{ annot = annot'; elements; comments })
      | Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot; name; optional } ->
        let annot' = this#get_annot ploc annot in
        (ploc, Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional })
      | Ast.Pattern.Expression _ ->
        (* No such thing as a pattern expression *)
        (ploc, patt)

    method private annotate_use_callback_fn fn =
      let {
        Ast.Function.params = (params_loc, { Ast.Function.Params.this_; params; rest; comments });
        _;
      } =
        fn
      in
      let params =
        Base.List.map
          params
          ~f:(fun (loc, { Ast.Function.Param.argument = (ploc, patt); default }) ->
            (loc, { Ast.Function.Param.argument = this#annotate_fn_param ploc patt; default })
        )
      in
      {
        fn with
        Ast.Function.params = (params_loc, { Ast.Function.Params.this_; params; rest; comments });
      }

    method! call loc expr =
      let open Ast.Expression in
      match expr with
      | {
       (* Call to `useCallback` or `React.useCallback` *)
       Call.callee =
         ( (_, Identifier (_, { Ast.Identifier.name = "useCallback"; _ }))
         | ( _,
             Member
               {
                 Member._object = (_, Identifier (_, { Ast.Identifier.name = "React"; _ }));
                 property = Member.PropertyIdentifier (_, { Ast.Identifier.name = "useCallback"; _ });
                 _;
               }
           ) ) as callee;
       targs = None;
       arguments = (arg_loc, { ArgList.arguments = Expression e :: rest; comments = arg_comments });
       comments;
      } ->
        (* useCallback((a, b) => {...}, ...) or useCallback(function(a, b) {...}, ...) *)
        (match e with
        | (loc, ArrowFunction fn) ->
          let fn_arg = Expression (loc, ArrowFunction (this#annotate_use_callback_fn fn)) in
          {
            Call.callee;
            targs = None;
            arguments = (arg_loc, { ArgList.arguments = fn_arg :: rest; comments = arg_comments });
            comments;
          }
        | (loc, Function fn) ->
          let fn_arg = Expression (loc, Function (this#annotate_use_callback_fn fn)) in
          {
            Call.callee;
            targs = None;
            arguments = (arg_loc, { ArgList.arguments = fn_arg :: rest; comments = arg_comments });
            comments;
          }
        | _ -> super#call loc expr)
      | _ -> super#call loc expr
  end
