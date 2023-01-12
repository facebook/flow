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
open Base.Option.Let_syntax

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

(* Given a GraphQL file foo.graphql.js exporting a type
 *
 *   export type Foo = {|
 *     +f?: {|
 *       +g: {|
 *         +h: ?$ReadOnlyArray<{|
 *           +i: {||}
 *         |}>
 *       |};
 *     |}
 *   |};
 *
 * [extract_graphql_fragment cx file_sig typed_ast tgt_aloc] returns the type of
 * the AST node at location [tgt_aloc] expressed in terms of an exported type alias:
 *
 *   $NonMaybeType<Foo?.["f"]?.["g"]?.["h"]?.[0]?.["i"]>
 *
 * Note that the solution falls back to optional index chaining as soon as the first
 * optional type or property is encountered. In that case, we prefix the resulting
 * type with `$NonMaybeType`.
 *)
module GraphQL : sig
  val extract_graphql_fragment :
    Context.t ->
    File_sig.With_ALoc.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Program.t ->
    ALoc.t ->
    Ty.t option
end = struct
  let reader = State_reader.create ()

  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader

  let abstract_reader = Abstract_state_reader.State_reader reader

  let rec visit_object_property_type defs tgt ~opt_chain ty p =
    let open Ast.Type.Object.Property in
    let (_, { key; value; optional; static = _; proto = _; _method = _; variance = _; comments = _ })
        =
      p
    in
    let open Ast.Expression.Object.Property in
    match key with
    | Identifier (_, { Ast.Identifier.name; _ })
    | Literal (_, { Ast.Literal.value = Ast.Literal.String name; _ }) -> begin
      match value with
      | Init t ->
        let optional = opt_chain || optional in
        let ty' =
          Ty.IndexedAccess { _object = ty; index = Ty.StrLit (Reason.OrdinaryName name); optional }
        in
        visit_type defs tgt ~opt_chain:optional ty' t
      | Get _
      | Set _ ->
        None
    end
    | Literal _
    | PrivateName _
    | Computed _ ->
      None

  and visit_object_type defs tgt ~opt_chain ty ot =
    let open Ast.Type.Object in
    let { properties; exact = _; inexact = _; comments = _ } = ot in
    Base.List.find_map properties ~f:(fun p ->
        match p with
        | Property p -> visit_object_property_type defs tgt ~opt_chain ty p
        | SpreadProperty _
        | Indexer _
        | InternalSlot _
        | CallProperty _ ->
          None
    )

  and visit_readonlyarray defs tgt ~opt_chain ty t' =
    let ty' = Ty.IndexedAccess { _object = ty; index = Ty.NumLit "0"; optional = opt_chain } in
    visit_type defs tgt ~opt_chain ty' t'

  and visit_type defs tgt ~opt_chain ty t =
    let open Ast.Type in
    match t with
    | (oloc, Object ot) ->
      if tgt = oloc then
        Some (ty, opt_chain)
      else
        visit_object_type defs tgt ~opt_chain ty ot
    | (_, Nullable { Nullable.argument = (oloc, Object ot); _ }) ->
      if tgt = oloc then
        Some (ty, true)
      else
        visit_object_type defs tgt ~opt_chain:true ty ot
    | ( _,
        Generic
          {
            Generic.id =
              Generic.Identifier.Unqualified (_, { Ast.Identifier.name = "$ReadOnlyArray"; _ });
            targs = Some (_, { Ast.Type.TypeArgs.arguments = [t']; _ });
            _;
          }
      ) ->
      visit_readonlyarray defs tgt ~opt_chain ty t'
    | ( _,
        Nullable
          {
            Nullable.argument =
              ( _,
                Generic
                  {
                    Generic.id =
                      Generic.Identifier.Unqualified
                        (_, { Ast.Identifier.name = "$ReadOnlyArray"; _ });
                    targs = Some (_, { Ast.Type.TypeArgs.arguments = [t']; _ });
                    _;
                  }
              );
            _;
          }
      ) ->
      visit_readonlyarray defs tgt ~opt_chain:true ty t'
    | _ -> None

  let visit_type_alias defs tgt type_alias =
    let open Ast.Statement.TypeAlias in
    match type_alias with
    | { id; tparams = None; right; comments = _ } ->
      let (id_loc, { Ast.Identifier.name; _ }) = id in
      let (name, sym_provenance) =
        match Loc_collections.LocMap.find_opt id_loc defs with
        | Some (_, local_name, _) -> (local_name, Ty_symbol.Local)
        | None -> (name, Ty_symbol.Remote { Ty_symbol.imported_as = None })
      in
      let symbol =
        {
          Ty_symbol.sym_provenance;
          sym_def_loc = ALoc.of_loc id_loc;
          sym_name = Reason.OrdinaryName name;
          sym_anonymous = false;
        }
      in
      let ty = Ty.Generic (symbol, Ty.TypeAliasKind, None) in
      let%map (ty, opt_chain) = visit_type defs tgt ~opt_chain:false ty right in
      if opt_chain then
        Ty.Utility (Ty.NonMaybeType ty)
      else
        ty
    | _ -> None

  let visit_declaration defs tgt decl =
    let open Ast.Statement in
    match decl with
    | (_loc, TypeAlias type_alias) -> visit_type_alias defs tgt type_alias
    | _ -> None

  let visit_statement defs tgt stmt =
    let open Ast.Statement in
    match stmt with
    | ( _loc,
        ExportNamedDeclaration
          {
            ExportNamedDeclaration.export_kind = ExportType;
            source = _;
            specifiers = _;
            declaration = Some decl;
            _;
          }
      ) ->
      visit_declaration defs tgt decl
    | _ -> None

  let visit_program defs tgt prog =
    let open Ast.Program in
    let (_, { statements; _ }) = prog in
    Base.List.find_map ~f:(visit_statement defs tgt) statements

  let get_imported_ident cx (local_name, loc, import_mode, { Type.TypeScheme.type_; _ }) =
    let t =
      match Ty_normalizer.Lookahead.peek cx type_ with
      | Ty_normalizer.Lookahead.LowerBounds [t] -> t
      | Ty_normalizer.Lookahead.LowerBounds _
      | Ty_normalizer.Lookahead.Recursive ->
        type_
    in
    (loc_of_aloc (TypeUtil.def_loc_of_t t), (loc, local_name, import_mode))

  let extract_graphql_fragment cx file_sig typed_ast tgt_aloc =
    let graphql_file = Base.Option.value_exn (ALoc.source tgt_aloc) in
    let graphql_ast =
      Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader:abstract_reader graphql_file
    in
    (* Collect information about imports in currect file to accurately compute
     * wether the base GraphQL type is imported in the current file. *)
    let defs =
      Ty_normalizer_imports.extract_imported_idents file_sig
      |> Ty_normalizer_imports.extract_schemes cx typed_ast
      |> List.map (get_imported_ident cx)
      |> Loc_collections.LocMap.of_list
    in
    let tgt_loc = loc_of_aloc tgt_aloc in
    let r = visit_program defs tgt_loc graphql_ast in
    if Base.Option.is_none r then
      Hh_logger.info "Failed to extract GraphQL type fragment %s" (Reason.string_of_loc tgt_loc);
    r
end

module Make (Extra : BASE_STATS) = struct
  module Acc = Acc (Extra)

  let mapper_type_normalization_hardcoded_fixes
      ~cctx
      ~lint_severities
      ~suppress_types
      ~imports_react
      ~preserve_literals
      ~generalize_maybe
      ~generalize_react_mixed_element
      acc =
    let { Codemod_context.Typed.full_cx = cx; file_sig; typed_ast; _ } = cctx in
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

          (* Array<empty> or Array<any>, commonly from [], is never a useful type.
             We should remove it from the union. *)
          let ts =
            let ts_without_array_empty =
              List.filter
                (function
                  | Ty.Arr { Ty.arr_elt_t = Ty.Bot _; _ } -> false
                  | Ty.Arr { Ty.arr_elt_t = Ty.Any _; _ } -> false
                  | _ -> true)
                ts
            in
            if Base.List.is_empty ts_without_array_empty then
              ts
            else
              ts_without_array_empty
          in

          (* React.MixedElement | React.Element<'div'> becomes just React.MixedElement *)
          let ts =
            let react_element_def_loc =
              List.find_map
                (function
                  | Ty.Generic
                      ( {
                          Ty.sym_name =
                            Reason.OrdinaryName
                              ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
                          sym_provenance = Ty_symbol.Library _;
                          sym_def_loc;
                          _;
                        },
                        _,
                        _
                      )
                    when is_react_loc sym_def_loc ->
                    Some sym_def_loc
                  | _ -> None)
                ts
            in
            match react_element_def_loc with
            | Some react_element_def_loc when from_bounds && generalize_react_mixed_element ->
              let ts =
                List.filter
                  (function
                    | Ty.Generic
                        ( {
                            Ty.sym_name =
                              Reason.OrdinaryName
                                ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
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
              in
              Ty.Generic
                ( {
                    Ty.sym_name = Reason.OrdinaryName "React.MixedElement";
                    sym_provenance = Ty_symbol.Library { Ty_symbol.imported_as = None };
                    sym_def_loc = react_element_def_loc;
                    sym_anonymous = false;
                  },
                  Ty.TypeAliasKind,
                  None
                )
              :: ts
            | _ -> ts
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
                          Ty.sym_name =
                            Reason.OrdinaryName
                              ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
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
        (* E.g. React$Element<typeof A> will become React.MixedElement or React.Node.
           The reason for this conversion is that it's a common idiom to keep SomeComponentClass to
           a local scope (e.g. that of a function) which would make the annotation ill-formed. *)
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
          let name =
            if generalize_react_mixed_element then
              "MixedElement"
            else
              "Node"
          in
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName name } in
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
        | Ty.Obj { Ty.obj_def_loc = Some aloc; _ } ->
          let remote_file = Base.Option.value_exn (ALoc.source aloc) in
          if String.ends_with (File_key.to_string remote_file) ~suffix:"graphql.js" then
            match GraphQL.extract_graphql_fragment cx file_sig typed_ast aloc with
            | Some t -> t
            | None -> super#on_t env t
          else
            super#on_t env t
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
      ~generalize_react_mixed_element
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
        ~generalize_react_mixed_element
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
