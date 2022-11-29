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

  let destruct cx ~use_op ~name_loc name default t =
    Base.Option.iter
      ~f:(fun d ->
        let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
        let default_t = Flow.mk_default cx reason d in
        Flow.flow cx (default_t, Type.UseT (use_op, t)))
      default;
    t

  let flow_default cx annot_t default_t =
    let open Type in
    let use_op =
      Op
        (AssignVar
           { var = Some (TypeUtil.reason_of_t annot_t); init = TypeUtil.reason_of_t default_t }
        )
    in
    Flow.flow cx (default_t, UseT (use_op, annot_t))

  let eval_default cx ?(always_flow_default = false) annot_t has_anno =
    Base.Option.map ~f:(fun e ->
        let (((loc, default_t), expr) as e) = Statement.expression cx e in
        if has_anno then (
          flow_default cx annot_t default_t;
          ((loc, annot_t), expr)
        ) else (
          if always_flow_default then flow_default cx annot_t default_t;
          e
        )
    )

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno }) =
    match pattern with
    | Id ({ Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } as id) ->
      let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
      let t = Env.find_write cx Env_api.OrdinaryNameLoc reason in
      let default = eval_default cx ~always_flow_default:true t has_anno default in
      (loc, { Ast.Function.Param.argument = ((ploc, t), Ast.Pattern.Identifier id); default })
    | Object { annot; properties; comments } ->
      let default = eval_default cx t has_anno default in
      let properties =
        let init = Destructuring.empty t ~annot:has_anno in
        let f = destruct cx in
        Destructuring.object_properties cx ~f ~parent_loc:ploc init properties
      in
      ( loc,
        {
          Ast.Function.Param.argument =
            ((ploc, t), Ast.Pattern.Object { Ast.Pattern.Object.properties; annot; comments });
          default;
        }
      )
    | Array { annot; elements; comments } ->
      let default = eval_default cx t has_anno default in
      let elements =
        let init = Destructuring.empty t ~annot:has_anno in
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
