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
      ~cctx ~lint_severities ~suppress_types ~imports_react ~preserve_literals acc =
    object (this)
      inherit Insert_type_utils.patch_up_react_mapper ~imports_react () as super

      val mutable acc = acc

      method acc () = acc

      val sanitized_any = Builtins.flowfixme_ty lint_severities suppress_types

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
                } as symbol ),
              kind,
              (Some [(Ty.Str _ | Ty.StrLit _)] as args_opt) )
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
                } as symbol ),
              kind,
              Some _ )
          when is_react_loc sym_def_loc ->
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Node" } in
          this#on_t env Ty.(Generic (symbol, kind, None))
        | Ty.Generic
            ( ( {
                  Ty.sym_name = Reason.OrdinaryName ("FbtElement" | "FbtResultBase");
                  sym_provenance = Ty_symbol.Library _;
                  _;
                } as symbol ),
              kind,
              None )
          when (Codemod_context.Typed.metadata cctx).Context.facebook_fbt = Some "FbtElement" ->
          let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Fbt" } in
          Ty.(Generic (symbol, kind, None))
        (* The following moves the `any` component of a union to the beginning of the
         * union. This is a heuristic that helps union resolution later on. *)
        | Ty.Union (Ty.Any _, _, _) -> super#on_t env t
        | Ty.Union (first, Ty.Any _, rest) ->
          let t = Ty.Union (sanitized_any, first, rest) in
          super#on_t env t
        | Ty.Union (first, second, rest) when List.exists Ty.is_dynamic rest ->
          let rest' = List.filter Utils_js.(Ty.is_dynamic %> not) rest in
          let t =
            if rest == rest' then
              t
            else
              Ty.mk_union (sanitized_any, first :: second :: rest')
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
                  t :: _ )))
          when p = b && b = r ->
          this#on_t env t
        (* All any's that have reached this point will be serialized. By making them
         * all of the same kind we enable type simplification (which depends on
         * structural equality). *)
        | Ty.Any _ -> sanitized_any
        | _ -> super#on_t env t
    end

  let run ~cctx ~lint_severities ~suppress_types ~imports_react ~preserve_literals acc loc t =
    let mapper =
      mapper_type_normalization_hardcoded_fixes
        ~cctx
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        acc
    in
    let t' = mapper#on_t loc t in
    let t'' =
      if t == t' then
        t
      else
        Ty_utils.simplify_type ~merge_kinds:false t'
    in
    (mapper#acc (), t'')
end
