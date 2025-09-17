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

module Stats = struct
  type t = {
    number_of_potential_fix_sites: int;
    number_of_added_annotations: int;
  }

  let empty = { number_of_potential_fix_sites = 0; number_of_added_annotations = 0 }

  let combine c1 c2 =
    {
      number_of_potential_fix_sites =
        c1.number_of_potential_fix_sites + c2.number_of_potential_fix_sites;
      number_of_added_annotations = c1.number_of_added_annotations + c2.number_of_added_annotations;
    }

  let serialize s =
    let open Utils_js in
    [
      spf "number_of_potential_fix_sites: %d" s.number_of_potential_fix_sites;
      spf "number_of_added_annotations: %d" s.number_of_added_annotations;
    ]

  let report s =
    [
      string_of_row ~indent:2 "Number of potential fix sites" s.number_of_potential_fix_sites;
      string_of_row ~indent:2 "Number of annotations added" s.number_of_added_annotations;
    ]
end

module Codemod_exports_annotator = Codemod_annotator.Make (Stats)
module Acc = Acc (Stats)

let mapper ~max_type_size (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in

  object (_this)
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
    val mutable stats = None

    method private post_run () = Base.Option.value_exn stats

    method! program prog =
      stats <- None;
      let cx = cctx.Codemod_context.Typed.cx in
      let reader = cctx.Codemod_context.Typed.reader in
      let file_sig = cctx.Codemod_context.Typed.file_sig in
      let typed_ast = cctx.Codemod_context.Typed.typed_ast in
      let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
      let get_ast_from_shared_mem = Parsing_heaps.Reader_dispatcher.get_ast ~reader in
      let get_type_sig = Parsing_heaps.Reader_dispatcher.get_type_sig ~reader in
      let get_haste_module_info f =
        let addr = Parsing_heaps.get_file_addr_unsafe f in
        Parsing_heaps.Reader_dispatcher.get_haste_module_info ~reader addr
      in
      let annotation_sites =
        let (errors, _warnings, _suppressions) =
          Error_suppressions.filter_lints
            ~include_suppressions:(Context.include_suppressions cx)
            (Context.error_suppressions cx)
            (Context.errors cx)
            (Context.aloc_tables cx)
            (Context.severity_cover cx)
        in
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.EInvariantSubtypingWithUseOp
                {
                  explanation =
                    Some
                      Flow_intermediate_error_types.(
                        ( LazyExplanationInvariantSubtypingDueToMutableArray
                            {
                              lower_array_loc = lower_loc;
                              lower_array_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                              upper_array_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                              _;
                            }
                        | LazyExplanationInvariantSubtypingDueToMutableProperty
                            {
                              lower_obj_loc = lower_loc;
                              lower_obj_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                              upper_obj_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                              _;
                            }
                        | LazyExplanationInvariantSubtypingDueToMutableProperties
                            {
                              lower_obj_loc = lower_loc;
                              lower_obj_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                              upper_obj_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                              _;
                            } ));
                  _;
                }
              when match lower_desc with
                   | Reason.RObjectLit
                   | Reason.RObjectLit_UNSOUND
                   | Reason.RArrayLit
                   | Reason.RArrayLit_UNSOUND ->
                     true
                   | _ -> false ->
              (loc_of_aloc lower_loc, upper_ty) :: acc
            | _ -> acc)
          errors
          []
        |> Base.List.sort_and_group ~compare:(fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2)
        |> Base.List.map ~f:(fun list ->
               let loc = fst (Base.List.hd_exn list) in
               let tys = Nel.of_list_exn (Base.List.map ~f:snd list) in
               let ty =
                 match tys with
                 | (ty, []) -> ty
                 | (ty0, ty1 :: tys) -> Ty.Inter (ty0, ty1, tys)
               in
               (loc, Ty_utils.simplify_type ~merge_kinds:true ty)
           )
      in
      let (prog', fixed_count) =
        Base.List.fold
          annotation_sites
          ~init:(prog, 0)
          ~f:(fun (ast, fixed_count) (lower_loc, upper_ty) ->
            if Base.Option.is_none (Ty_utils.size_of_type ~max:max_type_size upper_ty) then
              (ast, fixed_count)
            else
              let ast' =
                Insert_type.insert_type_ty
                  ~cx
                  ~loc_of_aloc
                  ~get_ast_from_shared_mem
                  ~get_haste_module_info
                  ~get_type_sig
                  ~file_sig
                  ~typed_ast
                  ~strict:false
                  ast
                  lower_loc
                  upper_ty
              in
              let fixed_count =
                if ast == ast' then
                  fixed_count
                else
                  fixed_count + 1
              in
              (ast', fixed_count)
        )
      in
      stats <-
        Some
          {
            Stats.number_of_potential_fix_sites = Base.List.length annotation_sites;
            number_of_added_annotations = fixed_count;
          };
      super#program prog'
  end
