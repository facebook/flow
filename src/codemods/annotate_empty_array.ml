(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet

let mapper
    ~ignore_suppressed
    ~file_options
    ~preserve_literals
    ~max_type_size
    ~default_any
    ~generalize_maybe
    ~generalize_react_mixed_element
    ~provided_error_set
    (cctx : Codemod_context.Typed.t) =
  let reader = cctx.Codemod_context.Typed.reader in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
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
  let error ((loc, _) as e) =
    ( loc,
      Ast.Expression.TypeCast
        {
          Ast.Expression.TypeCast.expression = e;
          annot = (Loc.none, flowfixme_ast);
          comments = None;
        }
    )
  in

  object (this)
    inherit
      Annotate_declarations.annotate_declarations_mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~generalize_react_mixed_element
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true as super

    val! arrays_only = true

    method! private init_loc_error_set =
      super#init_loc_error_set;
      loc_error_set <-
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.EEmptyArrayNoProvider _ ->
              (match Flow_error.loc_of_error error with
              | Some loc -> LSet.add (loc_of_aloc loc) acc
              | None -> acc)
            | _ -> acc)
          errors
          loc_error_set

    method! expression (expr : (Loc.t, Loc.t) Ast.Expression.t) =
      let open Ast.Expression in
      match expr with
      | (loc, Array { Array.elements = []; comments = _ }) ->
        if LSet.mem loc loc_error_set then
          let t = Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size loc in
          this#opt_annotate ~f:this#annotate_expr ~error ~expr:(Some expr) loc t expr
        else
          super#expression expr
      | _ -> super#expression expr
  end
