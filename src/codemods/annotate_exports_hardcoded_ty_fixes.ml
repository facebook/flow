(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

open Annotate_exports_utils

(* A string literal that contains alphabetic characters of underscores is likely
 * to be a tag *)
let maybe_string_literal_tag =
  let re = Str.regexp "^[a-zA-Z0-9-_]+$" in
  (fun s -> Str.string_match re s 0)

let mapper_type_normalization_hardcoded_fixes ~cctx ~strict ~imports_react ~preserve_literals =
  object (this)
    inherit Insert_type_utils.patch_up_react_mapper ~imports_react () as super

    val inferred_any =
      if strict then
        Builtins.flowfixme
      else
        Ty.explicit_any

    method! on_t env t =
      let loc = env in
      match t with
      (* `empty` with no upper bounds becomes `any`. All other allowed occurrence
       * `empty` remain as is. *)
      | Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper) ->
        Logger.warn loc Warning.Empty_NoUpper;
        Builtins.flowfixme_empty
      | Ty.Bot Ty.EmptyType ->
        (* `empty` annotations remain *)
        t
      | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeKnownUpper ub)) ->
        (* empty with `UseT ub` upper bounds becomes `ub` - equivalent to `MergedT (UseT ub)` *)
        Logger.warn loc Warning.Empty_SomeKnownUpper;
        this#on_t env ub
      (* Heuristic: These are rarely useful as full precision literal types *)
      | Ty.Num (Some _) when not preserve_literals -> Ty.Num None
      | Ty.Bool (Some _) when not preserve_literals -> Ty.Bool None
      | Ty.Str (Some s) when (not preserve_literals) && not (maybe_string_literal_tag s) ->
        Ty.Str None
      | Ty.Arr { Ty.arr_literal = true; arr_elt_t; _ } ->
        let arr_elt_t = this#on_t env arr_elt_t in
        Ty.Generic (Insert_type_utils.temporary_arraylit_symbol, Ty.TypeAliasKind, Some [arr_elt_t])
      | Ty.Obj ({ Ty.obj_literal = true; _ } as obj) ->
        let obj = this#on_obj_t env obj in
        Ty.Generic
          ( Insert_type_utils.temporary_objectlit_symbol,
            Ty.TypeAliasKind,
            Some [Ty.Obj { obj with Ty.obj_literal = false }] )
      (* E.g. React$Element<'div'> will become React.Element<'div'> *)
      | Ty.Generic
          ( ( { Ty.sym_name = "React$Element"; sym_provenance = Ty_symbol.Library _; sym_def_loc; _ }
            as symbol ),
            kind,
            (Some [(Ty.Str _ | Ty.StrLit _)] as args_opt) )
        when is_react_loc sym_def_loc ->
        let symbol = { symbol with Ty.sym_name = "Element" } in
        this#on_t env Ty.(Generic (symbol, kind, args_opt))
      (* E.g. React$Element<typeof A> will become React.Node *)
      | Ty.Generic
          ( ( { Ty.sym_name = "React$Element"; sym_provenance = Ty_symbol.Library _; sym_def_loc; _ }
            as symbol ),
            kind,
            Some _ )
        when is_react_loc sym_def_loc ->
        let symbol = { symbol with Ty.sym_name = "Node" } in
        this#on_t env Ty.(Generic (symbol, kind, None))
      | Ty.Generic
          ( ( {
                Ty.sym_name = "FbtElement" | "FbtResultBase";
                sym_provenance = Ty_symbol.Library _;
                _;
              } as symbol ),
            kind,
            None )
        when (Codemod_context.Typed.metadata cctx).Context.facebook_fbt = Some "FbtElement" ->
        let symbol = { symbol with Ty.sym_name = "Fbt" } in
        Ty.(Generic (symbol, kind, None))
      (* The following moves the `any` component of a union to the beginning of the
       * union. This is a heuristic that helps union resolution later on. *)
      | Ty.Union (Ty.Any _, _, _) -> super#on_t env t
      | Ty.Union (first, (Ty.Any _ as any), rest) ->
        let t = Ty.Union (any, first, rest) in
        super#on_t env t
      | Ty.Union (first, second, rest) when List.exists Ty.is_dynamic rest ->
        let rest' = List.filter Utils_js.(Ty.is_dynamic %> not) rest in
        let t =
          if rest == rest' then
            t
          else
            Ty.mk_union (Ty.explicit_any, first :: second :: rest')
        in
        super#on_t env t
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
                        [{ tp_name = p; tp_bound = None; tp_polarity = Neutral; tp_default = None }];
                    fun_params = [(_, Bound (_, b), { prm_optional = false })];
                    fun_rest_param = None;
                    fun_return = Bound (_, r);
                    fun_static = _;
                  },
                t :: _ )))
        when p = b && b = r ->
        this#on_t env t
      (* All any's that have reached this point will be serialized. By making them
       * all of the same kind we enable type simplification (which depends on
       * structural equality). *)
      | Ty.Any _ -> inferred_any
      | _ -> super#on_t env t
  end

let run ~cctx ~strict ~imports_react ~preserve_literals loc t =
  let mapper =
    mapper_type_normalization_hardcoded_fixes ~cctx ~strict ~imports_react ~preserve_literals
  in
  let t' = mapper#on_t loc t in
  let t'' =
    if t == t' then
      t
    else
      Ty_utils.simplify_type ~merge_kinds:false t'
  in
  Ok t''
