(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason

(* This is an implementation for the component declaration param handling. Given params
 * from the component_sig.DeclarationParamConfig representation, we turn those params into
 * types and define a function for extracting the config type of the component *)
module Make (Destructuring : Destructuring_sig.S) (Statement : Statement_sig.S) :
  Component_params_intf.Config with module Types = Component_sig_types.DeclarationParamConfig =
struct
  module Types = Component_sig_types.DeclarationParamConfig
  open Types

  let param_type_with_name (Param { t; name; default; _ }) =
    let open Ast.Statement.ComponentDeclaration.Param in
    let mk_type t =
      match default with
      | None -> t
      | Some _ -> TypeUtil.optional t
    in
    match name with
    | None -> None
    | Some (Identifier (loc, { Ast.Identifier.name; _ })) -> Some (loc, name, mk_type t)
    | Some (StringLiteral (loc, { Ast.StringLiteral.value; _ })) -> Some (loc, value, mk_type t)

  let rest_type (Rest { t; _ }) = t

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

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno; name = prop_name; shorthand })
      =
    let reconstruct_prop_name t =
      let open Ast.Statement.ComponentDeclaration.Param in
      match prop_name with
      | Some (Identifier (loc, name)) -> Some (Identifier ((loc, t), name))
      | Some (StringLiteral lit) -> Some (StringLiteral lit)
      | None -> None
    in

    match pattern with
    | Id ({ Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } as id) ->
      let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
      let t = Env.find_write cx Env_api.OrdinaryNameLoc reason in
      let default = eval_default cx ~always_flow_default:true t has_anno default in
      ( loc,
        {
          Ast.Statement.ComponentDeclaration.Param.local = ((ploc, t), Ast.Pattern.Identifier id);
          default;
          shorthand;
          name = reconstruct_prop_name t;
        }
      )
    | Object { annot; properties; comments } ->
      let default = eval_default cx t has_anno default in
      let properties =
        let init = Destructuring.empty t ~annot:has_anno in
        let f = destruct cx in
        Destructuring.object_properties cx ~f ~parent_loc:ploc init properties
      in
      ( loc,
        {
          Ast.Statement.ComponentDeclaration.Param.local =
            ((ploc, t), Ast.Pattern.Object { Ast.Pattern.Object.properties; annot; comments });
          default;
          shorthand;
          name = reconstruct_prop_name t;
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
          Ast.Statement.ComponentDeclaration.Param.local =
            ((ploc, t), Ast.Pattern.Array { Ast.Pattern.Array.elements; annot; comments });
          default;
          shorthand;
          name = reconstruct_prop_name t;
        }
      )

  let eval_rest cx (Rest { t = _; loc; ploc; id; has_anno = _ }) =
    let { Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } = id in
    let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
    let t = Env.find_write cx Env_api.OrdinaryNameLoc reason in
    ( loc,
      {
        Ast.Statement.ComponentDeclaration.RestParam.argument =
          ((ploc, t), Ast.Pattern.Identifier id);
        comments = None;
      }
    )
end
