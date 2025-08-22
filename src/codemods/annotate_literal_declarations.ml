(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module ALSet = Loc_collections.ALocSet
module LMap = Loc_collections.LocMap
open Insert_type_utils

(* Adding annotation to the object declaration can be potentially helpful, if there is an error
   * at the reference location of the object literal declaration. *)
let compute_annotation_sites_for_potential_fixes ~require_suppression_with_error_code ~reader cx ast
    =
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let (errors, _warnings, suppressions) =
    Error_suppressions.filter_lints
      ~include_suppressions:(Context.include_suppressions cx)
      (Context.error_suppressions cx)
      (Context.errors cx)
      (Context.aloc_tables cx)
      (Context.severity_cover cx)
  in
  let (unsuppressed_errors, _, _) =
    Error_suppressions.filter_suppressed_errors
      ~root:(Context.root cx)
      ~file_options:(Some (Context.file_options cx))
      ~require_suppression_with_error_code
      ~unsuppressable_error_codes:SSet.empty
      ~loc_of_aloc
      suppressions
      errors
      ~unused:suppressions
  in
  let all_possible_annotation_sites =
    Context.array_or_object_literal_declaration_upper_bounds cx
    |> Base.List.filter_map ~f:(fun (loc, list) ->
           match Nel.of_list list with
           | None -> None
           | Some nel -> Some (loc_of_aloc loc, nel)
       )
    |> Loc_collections.LocMap.of_list
    |> Loc_collections.LocMap.map (fun (t, ts) ->
           match ts with
           | [] -> t
           | t1 :: ts -> Type.IntersectionT (TypeUtil.reason_of_t t, Type.InterRep.make t t1 ts)
       )
  in
  let unsuppressed_errors_locs =
    let open Flow_errors_utils in
    ConcreteLocPrintableErrorSet.elements unsuppressed_errors
    |> Base.List.filter_map ~f:(fun printable_error ->
           match kind_of_printable_error printable_error with
           | InferError -> Some (loc_of_printable_error printable_error)
           | _ -> None
       )
    |> Loc_collections.LocSet.of_list
  in
  let scope_info =
    Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:true ast
  in
  let loc_filter loc _ =
    let uses =
      match Scope_api.With_Loc.def_of_use_opt scope_info loc with
      | None -> Loc_collections.LocSet.empty
      | Some def -> Scope_api.With_Loc.uses_of_def scope_info ~exclude_def:true def
    in
    Loc_collections.LocSet.exists
      (fun use -> Loc_collections.LocSet.mem use unsuppressed_errors_locs)
      uses
  in
  ( Loc_collections.LocMap.filter loc_filter all_possible_annotation_sites,
    Loc_collections.LocSet.cardinal unsuppressed_errors_locs
  )

module Stats = struct
  type t = {
    number_of_type_errors: int;
    number_of_added_annotations: int;
  }

  let empty = { number_of_type_errors = 0; number_of_added_annotations = 0 }

  let combine c1 c2 =
    {
      number_of_type_errors = c1.number_of_type_errors + c2.number_of_type_errors;
      number_of_added_annotations = c1.number_of_added_annotations + c2.number_of_added_annotations;
    }

  let serialize s =
    let open Utils_js in
    [
      spf "number_of_type_errors: %d" s.number_of_type_errors;
      spf "number_of_added_annotations: %d" s.number_of_added_annotations;
    ]

  let report s =
    [
      string_of_row ~indent:2 "Number of type errors" s.number_of_type_errors;
      string_of_row ~indent:2 "Number of annotations added" s.number_of_added_annotations;
    ]
end

module Codemod_exports_annotator = Codemod_annotator.Make (Stats)
module Acc = Acc (Stats)

let mapper ~max_type_size (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in

  object (this)
    inherit
      Codemod_exports_annotator.mapper
        cctx
        ~default_any:false
        ~generalize_maybe:true
        ~generalize_react_mixed_element:true
        ~lint_severities
        ~max_type_size
        ~merge_arrays:false
        () as super

    (* initialized in this#program *)
    val mutable annotation_sites = LMap.empty

    val mutable type_error_count = 0

    val mutable total_success = 0

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      match decl with
      | ( dloc,
          {
            id =
              ( id_loc,
                Ast.Pattern.Identifier
                  {
                    Ast.Pattern.Identifier.name;
                    annot = Ast.Type.Missing _ as annot;
                    optional = false as optional;
                  }
              );
            init = Some (eloc, _) as init;
          }
        )
        when LMap.mem id_loc annotation_sites ->
        let ty =
          let t = LMap.find id_loc annotation_sites in
          let genv =
            Ty_normalizer_flow.mk_genv
              ~options:Ty_normalizer_env.default_codemod_options
              ~cx:cctx.Codemod_context.Typed.cx
              ~file_sig:cctx.Codemod_context.Typed.file_sig
              ~typed_ast_opt:(Some cctx.Codemod_context.Typed.typed_ast)
          in
          match Ty_normalizer_flow.from_type genv t with
          | Ok elt ->
            Ty_utils.typify_elt elt
            |> Base.Result.of_option ~error:[Error.Missing_annotation_or_normalizer_error]
            |> Base.Result.map ~f:(fun ty ->
                   (* We know that we are annotating an object, so we don't have to include maybe types.
                    * This will allow us to insert Foo instead of ?Foo, which will exceed size limit. *)
                   Ty_utils.unmaybe_ty (Ty_utils.simplify_type ~merge_kinds:true ty)
               )
            |> Base.Result.bind ~f:(fun ty -> Codemod_annotator.validate_ty cctx ~max_type_size ty)
          | Error _ -> Error [Insert_type_utils.Error.Missing_annotation_or_normalizer_error]
        in
        let f loc _annot ty =
          this#annotate_node loc ty (fun a ->
              total_success <- total_success + 1;
              Ast.Type.Available a
          )
        in
        let error _ = annot in
        let annot' = this#opt_annotate ~f ~error ~expr:init eloc ty annot in
        if annot == annot' then
          super#variable_declarator ~kind decl
        else
          let decl' =
            {
              id =
                ( id_loc,
                  Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot = annot'; optional }
                );
              init;
            }
          in
          super#variable_declarator ~kind (dloc, decl')
      | _ -> super#variable_declarator ~kind decl

    method private post_run () =
      let stats =
        {
          Stats.number_of_type_errors = type_error_count;
          number_of_added_annotations = total_success;
        }
      in
      stats

    method! program prog =
      let (annotation_sites_, type_error_count_) =
        compute_annotation_sites_for_potential_fixes
          ~require_suppression_with_error_code:
            (Options.require_suppression_with_error_code cctx.Codemod_context.Typed.options)
          ~reader:cctx.Codemod_context.Typed.reader
          cctx.Codemod_context.Typed.cx
          prog
      in
      annotation_sites <- annotation_sites_;
      type_error_count <- type_error_count_;
      if LMap.is_empty annotation_sites then
        (* short when no signature *)
        prog
      else
        super#program prog
  end
