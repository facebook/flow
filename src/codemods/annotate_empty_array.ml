(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe:true
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true
        () as super

    method private post_run () = ()

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method! variable_declarator ~kind decl =
      let open Ast.Expression in
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id; init }) = decl in
      (* We are matching: `const x /* no annotation */ = [];` *)
      match (id, init) with
      | ( ( id_loc,
            Ast.Pattern.Identifier
              { Ast.Pattern.Identifier.name; annot = Ast.Type.Missing _; optional }
          ),
          Some (empty_array_loc, Array { Array.elements = []; comments = _ })
        ) ->
        let ty_result =
          Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size id_loc
        in
        (match ty_result with
        | Ok (Ty.Arr { Ty.arr_elt_t = Ty.Bot _; _ }) -> super#variable_declarator ~kind decl
        | Ok _ ->
          (match this#get_annot loc ty_result (Ast.Type.Missing empty_array_loc) with
          | Ast.Type.Available _ as annot ->
            let id =
              (id_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot; optional })
            in
            (loc, { id; init })
          | _ -> super#variable_declarator ~kind decl)
        | Error errors ->
          this#update_acc (fun acc ->
              Base.List.fold ~f:(fun acc e -> Acc.error acc loc e) ~init:acc errors
          );
          codemod_error_locs <- Loc_collections.LocSet.add loc codemod_error_locs;
          super#variable_declarator ~kind decl)
      | _ -> super#variable_declarator ~kind decl
  end
