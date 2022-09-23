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
open Loc_collections

module ErrorStats = struct
  type t = {
    num_error_vars: int;
    num_renamable_vars: int;
    num_annots_skipped: int;
  }

  let empty = { num_error_vars = 0; num_renamable_vars = 0; num_annots_skipped = 0 }

  let combine c1 c2 =
    {
      num_error_vars = c1.num_error_vars + c2.num_error_vars;
      num_renamable_vars = c1.num_renamable_vars + c2.num_renamable_vars;
      num_annots_skipped = c1.num_annots_skipped + c2.num_annots_skipped;
    }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_error_vars; spf "renamable_vars: %d" s.num_renamable_vars]

  let report s =
    [
      string_of_row ~indent:2 "Number of vars with write errors" s.num_error_vars;
      string_of_row ~indent:2 "Number of possibly renamable vars" s.num_renamable_vars;
      string_of_row ~indent:2 "Number of annotations skipped" s.num_annots_skipped;
    ]
end

module Codemod_declaration_annotator = Codemod_annotator.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

module Simplify = struct
  let is_resolved_empty t =
    match t with
    | Ty.Bot (Ty.NoLowerWithUpper _) -> true
    | _ -> false

  (* If all types in the union are arrays, filter out those that contain the
     empty type when it's due to resolving a tvar with no lowers *)
  let mk_arr_union ts =
    Base.List.fold_result
      ~init:[]
      ~f:(fun acc t ->
        match t with
        | Ty.Arr arr -> Ok (arr :: acc)
        | _ -> Error ())
      ts
    |> Base.Result.map
         ~f:
           (Base.List.filter_map ~f:(fun ({ Ty.arr_elt_t; _ } as arr) ->
                if is_resolved_empty arr_elt_t then
                  None
                else
                  Some (Ty.Arr arr)
            )
           )

  (* If all types in the union are functions with the same arity, filter out those that contain the
     empty type in parameters *)
  let mk_fun_union ts =
    Base.List.fold_result
      ~init:(None, [])
      ~f:(fun (param, acc) t ->
        (* Ensure that this is a union of "the same" function, in the sense of having the same arity.
           If there are different arities, it's maybe less likely that the elements of the union
           are the same type up to empty tvars *)
        match (param, t) with
        | (None, Ty.Fun ({ Ty.fun_params; fun_rest_param; _ } as fn)) ->
          Ok (Some (List.length fun_params, Base.Option.is_some fun_rest_param), fn :: acc)
        | (Some (ct, rest), Ty.Fun ({ Ty.fun_params; fun_rest_param; _ } as fn))
          when List.length fun_params = ct && Base.Option.is_some fun_rest_param = rest ->
          Ok (param, fn :: acc)
        | _ -> Error ())
      ts
    |> Base.Result.map ~f:snd
    |> Base.Result.map
         ~f:
           (Base.List.filter_map ~f:(fun ({ Ty.fun_params; fun_rest_param; _ } as fn) ->
                if
                  Base.List.exists ~f:(fun (_, t, _) -> is_resolved_empty t) fun_params
                  || Base.Option.value_map
                       ~f:(fun (_, t) -> is_resolved_empty t)
                       ~default:false
                       fun_rest_param
                then
                  None
                else
                  Some (Ty.Fun fn)
            )
           )

  (* If all types in the union are generics, and are the same generics, filter out those that contain the
     empty type in type arguments *)
  let mk_gen_union ts =
    Base.List.fold_result
      ~init:(None, [])
      ~f:(fun (sym, acc) t ->
        match (sym, t) with
        | (None, Ty.Generic (sym, gen, Some ts)) -> Ok (Some sym, (sym, gen, ts) :: acc)
        | (Some sym', Ty.Generic (sym, gen, Some ts)) when sym = sym' ->
          Ok (Some sym', (sym, gen, ts) :: acc)
        | _ -> Error ())
      ts
    |> Base.Result.map ~f:snd
    |> Base.Result.map
         ~f:
           (Base.List.filter_map ~f:(fun (sym, gen, ts) ->
                if Base.List.exists ~f:is_resolved_empty ts then
                  None
                else
                  Some (Ty.Generic (sym, gen, Some ts))
            )
           )

  let mk_union ?(flattened = false) (t0, ts) =
    let filtered_ts =
      match mk_arr_union (List.rev (t0 :: ts)) with
      | Ok filter_ts -> filter_ts
      | Error () ->
        (match mk_fun_union (List.rev (t0 :: ts)) with
        | Ok filter_ts -> filter_ts
        | Error () ->
          (match mk_gen_union (List.rev (t0 :: ts)) with
          | Ok filter_ts -> filter_ts
          | Error () -> t0 :: ts))
    in
    match filtered_ts with
    | [] -> Ty.mk_union ~flattened (t0, ts)
    | t0 :: ts -> Ty.mk_union ~flattened (t0, ts)

  let mapper =
    object
      inherit [_] Ty.map_ty as super

      method! on_t env t =
        match t with
        | Ty.Union (from_bounds, t0, t1, ts) -> super#on_t env (mk_union ~from_bounds (t0, t1 :: ts))
        | _ -> super#on_t env t
    end

  let normalize t =
    match t with
    | Codemod_annotator.Ty_ t -> Codemod_annotator.Ty_ (mapper#on_t () t)
    | _ -> t
end

let extractable_assignments cx loc_error_set loc =
  (* See if we should run the rename-redefinitions codemod by seeing if any write to this variable is extractable *)
  let is_extractable = Codemod_constrained_write_utils.is_extractable_assignment cx loc_error_set in
  let { Loc_env.var_info = { Env_api.scopes; ssa_values; _ }; _ } = Context.environment cx in
  let uses = Scope_api.With_ALoc.uses_of_use scopes loc in
  ALocSet.fold
    (fun use acc ->
      match ALocMap.find_opt use ssa_values with
      | None (* is a write *) when is_extractable use -> ALocSet.add use acc
      | _ -> acc)
    uses
    ALocSet.empty

class annotate_declarations_mapper
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

    (* This is a config option for codemods that extend this class *)
    val arrays_only = false

    val mutable renamable = ALocSet.empty

    val mutable loc_error_set = LSet.empty

    method private init_loc_error_set =
      loc_error_set <-
        Flow_error.ErrorSet.fold
          (fun error ->
            LSet.fold
              (fun loc -> LSet.add loc)
              (Codemod_constrained_write_utils.declaration_locs_of_constrained_write_error
                 ~arrays_only
                 cx
                 error
              ))
          errors
          loc_error_set

    method private post_run () =
      this#add_unannotated_loc_warnings
        (LSet.fold (fun loc -> LMap.add loc ()) loc_error_set LMap.empty);
      {
        ErrorStats.num_error_vars = LSet.cardinal loc_error_set;
        num_renamable_vars = ALocSet.cardinal renamable;
        num_annots_skipped = LSet.cardinal wont_annotate_locs;
      }

    method private get_annot ploc ty annot =
      let f loc _annot ty' =
        let simplified = Simplify.normalize ty' in
        this#annotate_node loc simplified (fun a -> Ast.Type.Available a)
      in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

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
          renamable <-
            ALocSet.union renamable (extractable_assignments cx loc_error_set (ALoc.of_loc ploc));

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
