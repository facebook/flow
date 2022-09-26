(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason

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

  let is_param_type_annotated (Param { has_anno; _ }) = has_anno

  let is_rest_type_annotated (Rest { has_anno; _ }) = has_anno

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

  let destruct _cx ~use_op:_ ~name_loc:_ _name t = t

  let eval_default cx = Base.Option.map ~f:(Statement.expression cx)

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno }) =
    match pattern with
    | Id ({ Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } as id) ->
      let default = eval_default cx default in
      let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
      let t =
        if Context.in_synthesis_mode cx then
          (* Do not attempt to read unpopulated env entry in synthesis mode *)
          t
        else
          Env.find_write cx Env_api.OrdinaryNameLoc reason
      in
      (loc, { Ast.Function.Param.argument = ((ploc, t), Ast.Pattern.Identifier id); default })
    | Object { annot; properties; comments } ->
      let default = eval_default cx default in
      let properties =
        let default = Base.Option.map default ~f:(fun ((_, t), _) -> Default.expr t) in
        let init = Destructuring.empty ?default t ~annot:has_anno in
        let f = destruct cx in
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
      let default = eval_default cx default in
      let elements =
        let default = Base.Option.map default ~f:(fun ((_, t), _) -> Default.expr t) in
        let init = Destructuring.empty ?default t ~annot:has_anno in
        let f = destruct cx in
        Destructuring.array_elements cx ~f init elements
      in
      ( loc,
        {
          Ast.Function.Param.argument =
            ((ploc, t), Ast.Pattern.Array { Ast.Pattern.Array.elements; annot; comments });
          default;
        }
      )

  let eval_rest cx (Rest { t = _; loc; ploc; id; has_anno = _ }) =
    let { Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } = id in
    let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
    let t = Env.find_write cx Env_api.OrdinaryNameLoc reason in
    ( loc,
      { Ast.Function.RestParam.argument = ((ploc, t), Ast.Pattern.Identifier id); comments = None }
    )

  let eval_this _cx (This { t = _; annot; loc }) =
    (loc, { Ast.Function.ThisParam.annot; comments = None })
end
