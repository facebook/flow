(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameMap = Utils_js.FilenameMap

let check_type_visitor wrap =
  let open Ty in
  object (self)
    inherit [_] iter_ty as super

    method! private on_prop env =
      function
      | NamedProp { prop; _ } -> self#on_named_prop env prop
      | CallProp _ -> wrap (Reason.RCustom "object Call Property")
      | SpreadProp _ -> wrap (Reason.RCustom "object Spread Property")

    method! private on_named_prop env =
      function
      | Field { t; _ } -> self#on_t env t
      | Method _ -> wrap (Reason.RMethod None)
      | Get _
      | Set _ ->
        wrap Reason.RGetterSetterProperty

    method! on_t env =
      function
      | TVar _ -> wrap (Reason.RCustom "recursive type")
      | Fun _ -> wrap Reason.RFunctionType
      | Generic (_, _, Some _) -> wrap (Reason.RCustom "class with generics")
      | Mu _ -> wrap (Reason.RCustom "recursive type")
      | Any (Annotated _) -> Reason.RAnyExplicit |> wrap
      | Any _ -> Reason.RAnyImplicit |> wrap
      | Bound (_, name) -> wrap (Reason.RCustom ("bound type var " ^ name))
      | Top -> wrap Reason.RMixed
      | Bot _ -> wrap Reason.REmpty
      | (Obj _ | Arr _ | Tup _ | Union _ | Inter _) as t -> super#on_t env t
      | Void
      | Null
      | Symbol
      | Num _
      | Str _
      | Bool _
      | NumLit _
      | StrLit _
      | BoolLit _
      | TypeOf _
      | Generic _
      | Utility _
      | IndexedAccess _
      | CharSet _
      | InlineInterface _ ->
        ()
  end

let detect_invalid_calls ~full_cx typed_ast file_sig =
  let options = Ty_normalizer_env.default_options in
  let check_valid_call ~genv (call_loc : ALoc.t) (_, targ_loc) =
    let typed_ast = genv.Ty_normalizer_env.typed_ast in
    let ty_opt = Typed_ast_utils.find_exact_match_annotation typed_ast targ_loc in
    Base.Option.iter ty_opt ~f:(fun scheme ->
        let desc = Reason.RCustom "TypeAssert library function" in
        let reason_main = Reason.mk_reason desc call_loc in
        let wrap reason =
          Flow_js.add_output
            full_cx
            (Error_message.EInvalidTypeArgs (reason_main, Reason.mk_reason reason call_loc))
        in
        match Ty_normalizer.from_scheme ~options ~genv scheme with
        | Ok (Ty.Type ty) -> (check_type_visitor wrap)#on_t () ty
        | Ok _
        | Error _ ->
          let { Type.TypeScheme.type_ = t; _ } = scheme in
          wrap (TypeUtil.desc_of_t t))
  in
  let file = Context.file full_cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~typed_ast ~file_sig in
  Loc_collections.ALocMap.iter (check_valid_call ~genv) (Context.type_asserts_map full_cx)
