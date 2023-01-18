(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module LSet = Loc_collections.LocSet
module T = Type
open Insert_type_utils

module Let_syntax = struct
  let return = return

  let bind x ~f = x >>= f

  let map x ~f = x >>| f
end

module Normalize_union = struct
  (* Wrap Ty.mk_union to filter out union branches that have an `any` propogated
   * from React *)
  let mk_union ~from_bounds ?(flattened = false) (t0, ts) =
    let filtered_ts =
      List.filter
        (function
          | Ty.Any (Ty.Annotated aloc) when is_react_loc aloc -> false
          | _ -> true)
        (t0 :: ts)
    in
    match filtered_ts with
    | [] -> t0
    | t1 :: ts -> Ty.mk_union ~from_bounds ~flattened (t1, ts)

  let mapper =
    object
      inherit [_] Ty.map_ty as super

      method! on_t env t =
        match t with
        | Ty.Union (from_bounds, t0, t1, ts) -> super#on_t env (mk_union ~from_bounds (t0, t1 :: ts))
        | _ -> super#on_t env t
    end

  let normalize t = mapper#on_t () t
end

let loc_of_lti_error cctx error =
  match Flow_error.msg_of_error error with
  | Error_message.EMissingLocalAnnotation { reason; _ } ->
    let aloc = Reason.aloc_of_reason reason in
    let context = Codemod_context.Typed.context cctx in
    Some (ALoc.to_loc_with_tables (Context.aloc_tables context) aloc)
  | _ -> None

module Loc_this_finder = This_finder.Make (LSet)

let find_this_locs (fn : ('a, 'a) Ast.Function.t) =
  let Ast.Function.{ body; params; _ } = fn in
  let finder = new Loc_this_finder.finder in
  LSet.union (finder#eval finder#function_body_any body) (finder#eval finder#function_params params)

(*
 * This is logic to specifically handle normalizing a common case for `this`
 * annotations. `this` annotations are often needed in functions that are only exported
 * (not called in the file) leaving the inference with no information about the lower
 * bounds. Additionally, the most common operation is just to access a prop on `this`.
 * If these two cases, no lower bounds and only prop accesses, are met, the type
 * will be an interface with prop names typed as `any`.
 *)
module Normalize_this_getPropT = struct
  let get_props bounds =
    let lowers = T.TypeMap.keys bounds.T.Constraint.lower in
    if List.length lowers == 0 then
      let uppers = List.map fst (T.Constraint.UseTypeMap.keys bounds.T.Constraint.upper) in
      List.fold_left
        (fun prop_names upper ->
          match (prop_names, upper) with
          | (Ok prop_names, T.MethodT (_, _, _, T.Named (_, Reason.OrdinaryName name), _, _))
          | (Ok prop_names, T.GetPropT (_, _, _, T.Named (_, Reason.OrdinaryName name), _)) ->
            Ok (name :: prop_names)
          | (_, _) -> Error "Not all uses are property accesses")
        (Ok [])
        uppers
    else
      Error "Has lower bounds"

  let build_combined_type ~cctx types =
    let full_cx = Codemod_context.Typed.context cctx in
    let prop_names =
      List.fold_left
        (fun props type_ ->
          match (props, type_) with
          | (Ok props, T.OpenT (_, id)) ->
            let (_, constraints) = Context.find_constraints full_cx id in
            (match constraints with
            | T.Constraint.Unresolved bounds -> get_props bounds >>| fun props' -> props' @ props
            | _ -> Error "Bounds are already resolved")
          | (Error msg, _) -> Error msg
          | _ -> Error "Not an open type variable")
        (Ok [])
        types
    in

    prop_names >>| fun prop_names ->
    let unique_names =
      List.fold_left
        (fun acc name ->
          if List.mem name acc then
            acc
          else
            name :: acc)
        []
        prop_names
    in
    let props =
      List.map
        (fun name ->
          Ty.NamedProp
            {
              name = Reason.OrdinaryName name;
              prop = Ty.Field { t = Ty.explicit_any; polarity = Ty.Neutral; optional = false };
              inherited = false;
              source = Ty.Other;
              def_loc = None;
            })
        unique_names
    in
    Ty.InlineInterface Ty.{ if_extends = []; if_props = props; if_dict = None }

  let ty_from_locs cctx locs =
    let typed_ast = Codemod_context.Typed.typed_ast cctx in
    let cx = Codemod_context.Typed.context cctx in

    locs
    |> LSet.elements
    |> List.filter_map (fun loc ->
           ALoc.of_loc loc |> Typed_ast_utils.find_exact_match_annotation cx typed_ast
       )
    |> List.map (fun { T.TypeScheme.type_; _ } -> type_)
    |> build_combined_type ~cctx

  let get_ty ~max_type_size ~cctx this_locs =
    let validate_ty = Codemod_annotator.validate_ty cctx ~max_type_size in
    match ty_from_locs cctx this_locs with
    | Ok ty -> validate_ty ty
    | Error _ -> Error [Error.Missing_annotation_or_normalizer_error]
end

module ErrorStats = struct
  type t = { num_total_errors: int }

  let empty = { num_total_errors = 0 }

  let combine c1 c2 = { num_total_errors = c1.num_total_errors + c2.num_total_errors }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_total_errors]

  let report s = [string_of_row ~indent:2 "Number of LTI errors" s.num_total_errors]
end

module Codemod_lti_annotator = Codemod_annotator.Make (ErrorStats)
module Hardcoded_Ty_Fixes = Codemod_hardcoded_ty_fixes.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

let mapper
    ~ignore_suppressed
    ~file_options
    ~loc_of_aloc
    ~preserve_literals
    ~max_type_size
    ~default_any
    ~skip_normal_params
    ~skip_this_params
    ~skip_class_properties
    ~(provided_error_locs : Loc.t list)
    (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  let provided_error_locs = LSet.of_list provided_error_locs in
  object (this)
    inherit
      Codemod_lti_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe:false
        ~generalize_react_mixed_element:false
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:false
        () as super

    val mutable loc_error_map = LMap.empty

    method private post_run () =
      this#add_unannotated_loc_warnings loc_error_map;
      {
        ErrorStats.num_total_errors =
          LMap.cardinal loc_error_map + LSet.cardinal provided_error_locs;
      }

    method private get_annot loc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None loc ty annot

    method private fix_and_validate loc ty =
      let (acc', ty) =
        Hardcoded_Ty_Fixes.run
          ~cctx
          ~preserve_literals
          ~generalize_maybe:false
          ~generalize_react_mixed_element:false
          ~merge_arrays:false
          acc
          loc
          ty
      in
      this#set_acc acc';
      Codemod_annotator.validate_ty cctx ~max_type_size ty

    method private class_property_annot annot init =
      match (annot, init) with
      | (Ast.Type.Missing ploc, (Ast.Class.Property.Declared | Ast.Class.Property.Uninitialized))
        when not skip_class_properties ->
        let ty_result =
          Codemod_annotator.get_ty cctx ~preserve_literals ploc
          >>| Normalize_union.normalize
          >>= this#fix_and_validate ploc
        in
        this#get_annot ploc ty_result annot
      | (Ast.Type.Missing ploc, Ast.Class.Property.Initialized _)
        when (not skip_class_properties) && LMap.mem ploc loc_error_map ->
        let ty_result =
          Codemod_annotator.get_ty cctx ~preserve_literals ploc
          >>| Normalize_union.normalize
          >>= this#fix_and_validate ploc
        in
        this#get_annot ploc ty_result annot
      | _ -> annot

    method! class_property loc prop =
      let open Ast.Class.Property in
      let prop = super#class_property loc prop in
      let { annot; value; _ } = prop in
      let annot = this#class_property_annot annot value in
      { prop with annot }

    method! class_private_field loc prop =
      let open Ast.Class.PrivateField in
      let prop = super#class_private_field loc prop in
      let { annot; value; _ } = prop in
      let annot = this#class_property_annot annot value in
      { prop with annot }

    method! function_param_pattern ((ploc, patt) : ('loc, 'loc) Ast.Pattern.t) =
      if
        (not skip_normal_params)
        && (LMap.mem ploc loc_error_map || LSet.mem ploc provided_error_locs)
      then (
        let ty_result =
          Codemod_annotator.get_ty cctx ~preserve_literals ploc
          >>| Normalize_union.normalize
          >>= this#fix_and_validate ploc
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
        super#function_param_pattern (ploc, patt)

    method private needs_this_annot loc =
      let open Base.Option.Let_syntax in
      let needs_annot =
        let%bind err = LMap.find_opt loc loc_error_map in
        let%map reason =
          match Flow_error.msg_of_error err with
          | Error_message.EMissingLocalAnnotation { reason; _ } -> Some reason
          | _ -> None
        in
        match Reason.desc_of_reason reason with
        | Reason.(RImplicitThis (RFunction RNormal)) -> true
        | _ -> false
      in
      Base.Option.value ~default:false needs_annot

    method! function_ loc (expr : ('loc, 'loc) Ast.Function.t) =
      let open Ast.Function in
      let { params; return; _ } = expr in
      let annotated_params = this#function_params params in
      let return =
        match return with
        | Ast.Type.Missing rloc when LMap.mem rloc loc_error_map ->
          let ty_result =
            Codemod_annotator.get_ty cctx ~preserve_literals rloc
            >>| Normalize_union.normalize
            >>= this#fix_and_validate rloc
          in
          this#get_annot rloc ty_result return
        | _ -> return
      in

      let (ploc, _) = params in
      let with_this_param =
        if (not skip_this_params) && this#needs_this_annot ploc then
          let this_locs = find_this_locs expr in
          if not (LSet.is_empty this_locs) then
            let ty_result =
              Codemod_annotator.get_validated_ty
                cctx
                ~preserve_literals
                ~max_type_size
                (LSet.choose this_locs)
            in
            let annot =
              match this#get_annot Loc.none ty_result (Ast.Type.Missing Loc.none) with
              | Ast.Type.Available
                  ( l,
                    Ast.Type.
                      ( _,
                        Generic
                          {
                            Generic.id =
                              Generic.Identifier.Unqualified (_, { Ast.Identifier.name = "this"; _ });
                            _;
                          }
                      )
                  ) ->
                (l, flowfixme_ast)
              | Ast.Type.Available annot' -> annot'
              | Ast.Type.Missing _ ->
                (* Not able to find a type with standard normalizer. Try the custom one *)
                let ty_result = Normalize_this_getPropT.get_ty ~max_type_size ~cctx this_locs in
                (match this#get_annot Loc.none ty_result (Ast.Type.Missing Loc.none) with
                | Ast.Type.Available annot' -> annot'
                | Ast.Type.Missing _ -> (Loc.none, (Loc.none, Ast.Type.Any None)))
            in
            let (_, params') = annotated_params in
            ( ploc,
              {
                params' with
                Ast.Function.Params.this_ =
                  Some (Loc.none, Ast.Function.{ annot; ThisParam.comments = None });
              }
            )
          else
            annotated_params
        else
          annotated_params
      in

      if with_this_param == params then
        super#function_ loc { expr with return }
      else
        super#function_ loc { expr with return; params = with_this_param }

    method! program prog =
      let cx = Codemod_context.Typed.context cctx in
      let errors = Context.errors cx in
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
      if LSet.is_empty provided_error_locs then
        loc_error_map <-
          Flow_error.ErrorSet.fold
            (fun error acc ->
              match loc_of_lti_error cctx error with
              | Some loc -> LMap.add loc error acc
              | None -> acc)
            errors
            LMap.empty;
      if LMap.is_empty loc_error_map && LSet.is_empty provided_error_locs then
        prog
      else
        super#program prog
  end
