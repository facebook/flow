(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* WARNING! Hard-coded fixes ahead!
 *
 * The level of inferred types (Ty.t). The advantage here is that we have
 * access to more information over types than just their structure (e.g.
 * provenance)
 *)

open Insert_type_utils

module PreserveLiterals = struct
  type mode =
    | Always
    | Never
    | Auto

  (* A string literal that contains alphabetic characters of underscores is likely
   * to be a tag *)
  let tag_like_regex = Str.regexp "^[a-zA-Z0-9-_]+$"

  let enforce ~mode t =
    let enforce_string s =
      match mode with
      | Always -> t
      | Never -> Ty.Str None
      | Auto ->
        if Str.string_match tag_like_regex s 0 then
          t
        else
          Ty.Str None
    in
    let enforce_number =
      match mode with
      | Always -> t
      | Never
      | Auto ->
        Ty.Num None
    in
    let enforce_bool =
      match mode with
      | Always -> t
      | Never
      | Auto ->
        Ty.Bool None
    in
    match t with
    | Ty.Str (Some s) ->
      (* TODO consider handling internal names explicitly *)
      enforce_string (Reason.display_string_of_name s)
    | Ty.Num (Some _) -> enforce_number
    | Ty.Bool (Some _) -> enforce_bool
    | _ -> t
end

module Make (Extra : BASE_STATS) = struct
  module Acc = Acc (Extra)

  let mapper_type_normalization_hardcoded_fixes
      ~cctx ~lint_severities ~suppress_types ~imports_react ~preserve_literals ~generalize_maybe acc
      =
    object (this)
      inherit Insert_type_utils.patch_up_react_mapper ~imports_react () as super

      val mutable acc = acc

      method acc () = acc

      val sanitized_any = Builtins.flowfixme_ty lint_severities suppress_types

      method! on_Union env _ from_bounds ty1 ty2 tys =
        (* The following moves the `any` component of a union to the beginning of the
         * union. This is a heuristic that helps union resolution later on. *)
        let (ty1, ty2, tys) =
          match (ty1, ty2) with
          | (Ty.Any _, _) -> (sanitized_any, ty2, tys)
          | (_, Ty.Any _) -> (sanitized_any, ty1, tys)
          | _ ->
            if List.exists Ty.is_dynamic tys then
              let tys' = List.filter Utils_js.(Ty.is_dynamic %> not) tys in
              if tys == tys' then
                (ty1, ty2, tys)
              else
                (sanitized_any, ty1, ty2 :: tys')
            else
              (ty1, ty2, tys)
        in

        (*
         * If there are named type aliases in a union generated by a codemod (i.e. from_bounds
         * is true), they are likely the best types to use. The other union branches
         * with literal types are usually noise.
         *)
        let ts = ty1 :: ty2 :: tys in
        let has_type_aliases =
          List.exists
            (function
              | Ty.Generic (_, Ty.TypeAliasKind, _) -> true
              | _ -> false)
            ts
        in
        let ts =
          if has_type_aliases && from_bounds then
            List.filter
              (function
                | Ty.Obj { Ty.obj_literal = Some true; _ }
                | Ty.Num (Some _)
                | Ty.Str (Some _)
                | Ty.Bool (Some _) ->
                  false
                | _ -> true)
              ts
          else
            ts
        in

        (* At this point we can recurse on the overall union, and then destruct it again
           for further analysis. We need to do this because of other transformations that
           won't fire correctly--e.g. we need to have not simplified literals for the
           `has_type_aliases` step above, but we must have simplified in order to see a
           unified representation for Fbt as below. *)
        match ts with
        | [] -> super#on_t env (Ty.mk_union ~from_bounds (ty1, ty2 :: tys))
        | [t] -> this#on_t env t
        | ts ->
          let ts = Base.List.map ~f:(this#on_t env) ts in

          (* The Fbt type contains string *)
          let has_fbt =
            List.exists
              (function
                | Ty.Generic
                    ( {
                        Ty.sym_name = Reason.OrdinaryName "Fbt";
                        sym_provenance = Ty_symbol.Library _;
                        _;
                      },
                      _,
                      None
                    ) ->
                  true
                | _ -> false)
              ts
          in
          let ts =
            if has_fbt && from_bounds then
              List.filter
                (function
                  | Ty.Str _ -> false
                  | _ -> true)
                ts
            else
              ts
          in

          let ts =
            if generalize_maybe && from_bounds && List.length ts > 1 then
              if List.mem Ty.Null ts && not (List.mem Ty.Void ts) then
                Ty.Void :: ts
              else if List.mem Ty.Void ts && not (List.mem Ty.Null ts) then
                Ty.Null :: ts
              else
                ts
            else
              ts
          in

          (* React.Node | React.Element<'div'> becomes just React.Node *)
          let has_react_node =
            List.exists
              (function
                | Ty.Generic
                    ( {
                        Ty.sym_name = Reason.OrdinaryName ("Node" | "React.Node");
                        sym_provenance = Ty_symbol.Library _;
                        sym_def_loc;
                        _;
                      },
                      _,
                      _
                    ) ->
                  is_react_loc sym_def_loc
                | _ -> false)
              ts
          in
          let ts =
            if has_react_node && from_bounds then
              List.filter
                (function
                  | Ty.Generic
                      ( {
                          Ty.sym_name = Reason.OrdinaryName ("Element" | "React.Element");
                          sym_provenance = Ty_symbol.Library _;
                          sym_def_loc;
                          _;
                        },
                        _,
                        Some _
                      ) ->
                    not (is_react_loc sym_def_loc)
                  | Ty.Generic
                      ( {
                          Ty.sym_name = Reason.OrdinaryName "Fbt";
                          sym_provenance = Ty_symbol.Library _;
                          _;
                        },
                        _,
                        None
                      )
                    when (Codemod_context.Typed.metadata cctx).Context.facebook_fbt <> None ->
                    false
                  | _ -> true)
                ts
            else
              ts
          in

          (match ts with
          | [] -> Ty.mk_union ~from_bounds (ty1, ty2 :: tys)
          | [t] -> t
          | t :: ts -> Ty.mk_union ~from_bounds (t, ts))

      method! on_t env t =
        let loc = env in
        match t with
        (* `empty` with no upper bounds becomes `any`. All other allowed occurrence
         * `empty` remain as is. *)
        | Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper) ->
          acc <- Acc.warn acc loc Warning.Empty_NoUpper;
          Builtins.flowfixme_empty_ty lint_severities suppress_types
        | Ty.Bot Ty.EmptyType ->
          (* `empty` annotations remain *)
          t
        | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeKnownUpper ub)) ->
          (* empty with `UseT ub` upper bounds becomes `ub` *)
          acc <- Acc.warn acc loc Warning.Empty_SomeKnownUpper;
          this#on_t env ub
        (* Heuristic: These are rarely useful as full precision literal types *)
        | Ty.Num _
        | Ty.Bool _
        | Ty.Str _ ->
          PreserveLiterals.enforce ~mode:preserve_literals t
        (* E.g. React$Element<'div'> will become React.Element<'div'> *)
        | Ty.Generic
            ( ( {
                  Ty.sym_name = Reason.OrdinaryName "React$Element";
                  sym_provenance = Ty_symbol.Library _;
                  sym_def_loc;
                  _;
                } as symbol
              ),
              kind,
              (Some [(Ty.Str _ | Ty.StrLit _)] as args_opt)
            )
          when is_react_loc sym_def_loc ->
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Element" } in
          this#on_t env Ty.(Generic (symbol, kind, args_opt))
        (* E.g. React$Element<typeof A> will become React.Node *)
        | Ty.Generic
            ( ( {
                  Ty.sym_name = Reason.OrdinaryName "React$Element";
                  sym_provenance = Ty_symbol.Library _;
                  sym_def_loc;
                  _;
                } as symbol
              ),
              kind,
              Some _
            )
          when is_react_loc sym_def_loc ->
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Node" } in
          this#on_t env Ty.(Generic (symbol, kind, None))
        | Ty.Generic
            ( ( {
                  Ty.sym_name =
                    Reason.OrdinaryName
                      ("FbtElement" | "FbtResultBase" | "$FbtResultBase" | "FbtString");
                  sym_provenance = Ty_symbol.Library _;
                  _;
                } as symbol
              ),
              _,
              None
            )
          when (Codemod_context.Typed.metadata cctx).Context.facebook_fbt = Some "FbtElement" ->
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Fbt" } in
          Ty.(Generic (symbol, Ty.TypeAliasKind, None))
        (*
         * Minimal form of evaluating utility types:
         *
         * $Call<<K>(_0: K) => K, T, ...> --> T
         *)
        | Ty.(
            Utility
              (Call
                ( Fun
                    {
                      fun_type_params =
                        Some
                          [
                            {
                              tp_name = p;
                              tp_bound = None;
                              tp_polarity = Neutral;
                              tp_default = None;
                            };
                          ];
                      fun_params = [(_, Bound (_, b), { prm_optional = false })];
                      fun_rest_param = None;
                      fun_return = Bound (_, r);
                      fun_static = _;
                    },
                  t :: _
                )
                ))
          when p = b && b = r ->
          this#on_t env t
        (* All any's that have reached this point will be serialized. By making them
         * all of the same kind we enable type simplification (which depends on
         * structural equality). *)
        | Ty.Any _ -> sanitized_any
        | _ -> super#on_t env t
    end

  (* Converts types like 'Array<T> | Array<S> | R' to '$ReadOnlyArray<T | S> | R'
   * In certain kinds of codemods this has shown to cause fewer [ambiguous-speculation]
   * errros. *)
  let array_simplification t =
    let open Ty in
    let arr_elts ts =
      List.fold_left
        (fun (arr_acc, other_acc) t ->
          match t with
          | Arr { arr_elt_t; _ } -> (arr_elt_t :: arr_acc, other_acc)
          | _ -> (arr_acc, t :: other_acc))
        ([], [])
        ts
    in
    let members = bk_union t |> Nel.to_list in
    let (arr_members, other_members) = arr_elts members in
    match arr_members with
    | []
    | [_] ->
      t
    | t :: ts ->
      let arr =
        Arr
          {
            arr_elt_t = mk_union ~from_bounds:true (t, ts);
            arr_literal = None;
            arr_readonly = true;
          }
      in
      mk_union ~from_bounds:true (arr, other_members)

  let run
      ~cctx
      ~preserve_literals
      ~generalize_maybe
      ~merge_arrays
      ?(lint_severities = Codemod_context.Typed.lint_severities cctx)
      ?(suppress_types = Options.suppress_types cctx.Codemod_context.Typed.options)
      ?(imports_react =
        Insert_type_imports.ImportsHelper.imports_react cctx.Codemod_context.Typed.file_sig)
      acc
      loc
      t =
    let mapper =
      mapper_type_normalization_hardcoded_fixes
        ~cctx
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~generalize_maybe
        acc
    in
    let t' = mapper#on_t loc t in
    let t' =
      if merge_arrays then
        array_simplification t'
      else
        t'
    in
    let t'' =
      if t == t' then
        t
      else
        Ty_utils.simplify_type ~merge_kinds:false t'
    in
    (mapper#acc (), t'')
end
