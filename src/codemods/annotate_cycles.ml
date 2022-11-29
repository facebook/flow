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

module ErrorStats = struct
  type t = {
    num_missing_annots: int;
    num_annots_skipped: int;
    num_cycles_unannotatable: int;
  }

  let empty = { num_missing_annots = 0; num_annots_skipped = 0; num_cycles_unannotatable = 0 }

  let combine c1 c2 =
    {
      num_missing_annots = c1.num_missing_annots + c2.num_missing_annots;
      num_annots_skipped = c1.num_annots_skipped + c2.num_annots_skipped;
      num_cycles_unannotatable = c1.num_cycles_unannotatable + c2.num_cycles_unannotatable;
    }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_missing_annots]

  let report s =
    [
      string_of_row ~indent:2 "Number of cycles" s.num_missing_annots;
      string_of_row ~indent:2 "Number of annotations skipped" s.num_annots_skipped;
      string_of_row ~indent:2 "Number of unannotatable cycles" s.num_cycles_unannotatable;
    ]
end

module Codemod_declaration_annotator = Codemod_annotator.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

class annotate_cycles_mapper
  cctx
  ~default_any
  ~generalize_maybe
  ~generalize_react_mixed_element
  ~max_type_size
  ~preserve_literals
  ~merge_arrays =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  let cx = Codemod_context.Typed.context cctx in
  let errors = Context.errors cx in

  object (this)
    inherit
      Codemod_declaration_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~generalize_react_mixed_element
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays
        () as super

    val mutable loc_error_set = LSet.empty

    val mutable unannotatable_cycles = 0

    method private init_loc_error_set =
      let open Error_message in
      let tables = Context.aloc_tables cx in
      let (errors, unannotatable) =
        Flow_error.ErrorSet.fold
          (fun err (acc, ct) ->
            match Flow_error.msg_of_error err with
            | ERecursiveDefinition { annot_locs = []; _ } -> (acc, ct + 1)
            | ERecursiveDefinition { annot_locs; _ } ->
              let acc =
                Base.List.map
                  ~f:(function
                    | Env_api.Loc loc
                    | Env_api.Object { loc; _ } ->
                      ALoc.to_loc_with_tables tables loc)
                  annot_locs
                |> LSet.of_list
                |> LSet.union acc
              in
              (acc, ct)
            | EDefinitionCycle cycle ->
              let annotations =
                Nel.to_list cycle
                |> Base.List.map ~f:(fun (_, _, annot_locs) ->
                       Base.List.map
                         ~f:(function
                           | Env_api.Loc loc
                           | Env_api.Object { loc; _ } ->
                             ALoc.to_loc_with_tables tables loc)
                         annot_locs
                   )
                |> List.flatten
              in
              if Base.List.length annotations = 0 then
                (acc, ct + 1)
              else
                (Base.List.fold ~f:(fun acc loc -> LSet.add loc acc) ~init:acc annotations, ct)
            | _ -> (acc, ct))
          errors
          (loc_error_set, unannotatable_cycles)
      in
      loc_error_set <- errors;
      unannotatable_cycles <- unannotatable

    method private post_run () =
      this#add_unannotated_loc_warnings
        (LSet.fold (fun loc -> LMap.add loc ()) loc_error_set LMap.empty);
      {
        ErrorStats.num_missing_annots = LSet.cardinal loc_error_set;
        num_annots_skipped = LSet.cardinal wont_annotate_locs;
        num_cycles_unannotatable = unannotatable_cycles;
      }

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method! function_ loc ({ Ast.Function.return; _ } as func) =
      let func =
        match return with
        | Ast.Type.Missing loc when LSet.mem loc loc_error_set ->
          let ty_result =
            Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size loc
          in
          let return = this#get_annot loc ty_result return in
          { func with Ast.Function.return }
        | _ -> func
      in
      super#function_ loc func

    method! variable_declarator_pattern ~kind ((ploc, patt) : ('loc, 'loc) Ast.Pattern.t) =
      if LSet.mem ploc loc_error_set then (
        let ty_result =
          Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size ploc
        in
        match patt with
        | Ast.Pattern.Object Ast.Pattern.Object.{ annot; properties; comments } ->
          let annot' = this#get_annot ploc ty_result annot in
          (ploc, Ast.Pattern.Object Ast.Pattern.Object.{ annot = annot'; properties; comments })
        | Ast.Pattern.Array Ast.Pattern.Array.{ annot; elements; comments } ->
          let annot' = this#get_annot ploc ty_result annot in
          (ploc, Ast.Pattern.Array Ast.Pattern.Array.{ annot = annot'; elements; comments })
        | Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot; name; optional } ->
          let annot' = this#get_annot ploc ty_result annot in
          (ploc, Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional })
        | Ast.Pattern.Expression _ ->
          (* No such thing as a pattern expression *)
          this#update_acc (fun acc -> Acc.error acc ploc Error.Unsupported_error_kind);
          codemod_error_locs <- LSet.add ploc codemod_error_locs;
          (ploc, patt)
      ) else
        super#variable_declarator_pattern ~kind (ploc, patt)

    method! program prog =
      this#init_loc_error_set;
      if LSet.is_empty loc_error_set then
        prog
      else
        super#program prog
  end
