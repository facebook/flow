(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Type
open Hint_api

module Make (Destructuring : Destructuring_sig.S) (Statement : Statement_sig.S) :
  Func_stmt_config_sig.S with module Types = Func_stmt_config_types.Types = struct
  module Types = Func_stmt_config_types.Types
  open Types

  let param_type (Param { t; pattern; default; _ }) =
    match pattern with
    | Id id ->
      let { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); optional; _ } = id in
      let t =
        if optional || default <> None then
          TypeUtil.optional t
        else
          t
      in
      (Some name, t)
    | _ ->
      let t =
        if default <> None then
          TypeUtil.optional t
        else
          t
      in
      (None, t)

  let rest_type (Rest { t; loc; id; _ }) =
    let { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ } = id in
    (Some name, loc, t)

  let this_type (This { t; _ }) = t

  let subst_param cx map param =
    let (Param { t; loc; ploc; pattern; default; has_anno }) = param in
    let t = Flow.subst cx map t in
    Param { t; loc; ploc; pattern; default; has_anno }

  let subst_rest cx map rest =
    let (Rest { t; loc; ploc; id; has_anno }) = rest in
    let t = Flow.subst cx map t in
    Rest { t; loc; ploc; id; has_anno }

  let subst_this cx map (This { t; loc; annot }) =
    let t = Flow.subst cx map t in
    This { t; loc; annot }

  let bind cx t loc =
    if Context.enable_const_params cx then
      Env.bind_implicit_const cx t loc
    else
      Env.bind_implicit_let cx t loc

  let destruct cx ~use_op:_ ~name_loc ~has_anno _name t =
    let t_bound =
      if has_anno then
        Annotated t
      else
        Inferred t
    in
    bind cx t_bound name_loc;
    t

  let eval_default cx ~annot_t =
    let hint =
      match annot_t with
      | Some t -> Hint_t t
      | None -> Hint_None
    in
    Base.Option.map ~f:(Statement.expression cx ~hint)

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno }) =
    match pattern with
    | Id ({ Ast.Pattern.Identifier.name = ((name_loc, _), _); optional; _ } as id) ->
      let default = eval_default cx ~annot_t:None default in
      let () =
        let t =
          if optional && default = None then
            TypeUtil.optional t
          else
            t
        in
        let t =
          if has_anno then
            Annotated t
          else
            Inferred t
        in
        bind cx t name_loc
      in
      (loc, { Ast.Function.Param.argument = ((ploc, t), Ast.Pattern.Identifier id); default })
    | Object { annot; properties; comments } ->
      let annot_t =
        match annot with
        | Ast.Type.Missing _ -> None
        | Ast.Type.Available (_, ((_, annot_t), _)) -> Some annot_t
      in
      let default = eval_default cx ~annot_t default in
      let properties =
        let default = Base.Option.map default ~f:(fun ((_, t), _) -> Default.expr t) in
        let init = Destructuring.empty ?default t ~annot:has_anno in
        let f = destruct cx ~has_anno in
        Destructuring.object_properties cx ~f init properties
      in
      ( loc,
        {
          Ast.Function.Param.argument =
            ((ploc, t), Ast.Pattern.Object { Ast.Pattern.Object.properties; annot; comments });
          default;
        }
      )
    | Array { annot; elements; comments } ->
      let default = eval_default cx ~annot_t:None default in
      let elements =
        let default = Base.Option.map default ~f:(fun ((_, t), _) -> Default.expr t) in
        let init = Destructuring.empty ?default t ~annot:has_anno in
        let f = destruct cx ~has_anno in
        Destructuring.array_elements cx ~f init elements
      in
      ( loc,
        {
          Ast.Function.Param.argument =
            ((ploc, t), Ast.Pattern.Array { Ast.Pattern.Array.elements; annot; comments });
          default;
        }
      )

  let eval_rest cx (Rest { t; loc; ploc; id; has_anno }) =
    let () =
      let { Ast.Pattern.Identifier.name = ((loc, _), _); _ } = id in
      let t =
        if has_anno then
          Annotated t
        else
          Inferred t
      in
      bind cx t loc
    in
    ( loc,
      { Ast.Function.RestParam.argument = ((ploc, t), Ast.Pattern.Identifier id); comments = None }
    )

  let eval_this _ (This { t = _; annot; loc }) =
    (* this does not bind any parameters *)
    (loc, { Ast.Function.ThisParam.annot; comments = None })
end
