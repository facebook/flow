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

  let read_react cx loc =
    match Context.react_runtime cx with
    | Options.ReactRuntimeClassic ->
      let (_ : Type.t) =
        Type_env.var_ref ~lookup_mode:Type_env.LookupMode.ForValue cx (OrdinaryName "React") loc
      in
      ()
    | Options.ReactRuntimeAutomatic -> ()

  let param_type_with_name (Param { t; name; default; pattern; _ }) =
    let open Ast.Statement.ComponentDeclaration.Param in
    let t =
      match pattern with
      | Id id ->
        let { Ast.Pattern.Identifier.optional; _ } = id in
        let t =
          if optional || default <> None then
            TypeUtil.optional t
          else
            t
        in
        t
      | _ ->
        if default <> None then
          TypeUtil.optional t
        else
          t
    in
    match name with
    | Identifier (loc, { Ast.Identifier.name; _ }) -> (loc, name, t)
    | StringLiteral (loc, { Ast.StringLiteral.value; _ }) -> (loc, value, t)

  let rest_type (Rest { t; pattern; _ }) =
    let optional =
      match pattern with
      | Id { Ast.Pattern.Identifier.optional; _ } -> optional
      | _ -> false
    in
    if optional then
      TypeUtil.optional t
    else
      t

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
      | Identifier (loc, name) -> Identifier ((loc, t), name)
      | StringLiteral lit -> StringLiteral lit
    in

    match pattern with
    | Id ({ Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); _ } as id) ->
      let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
      let t = Type_env.find_write cx Env_api.OrdinaryNameLoc reason in
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

  let eval_rest cx (Rest { t; loc; ploc; pattern; has_anno }) =
    match pattern with
    | Id id ->
      ( loc,
        {
          Ast.Statement.ComponentDeclaration.RestParam.argument =
            ((ploc, t), Ast.Pattern.Identifier id);
          comments = None;
        }
      )
    | Object { annot; properties; comments } ->
      let properties =
        let init = Destructuring.empty t ~annot:has_anno in
        let f = destruct cx in
        Destructuring.object_properties cx ~f ~parent_loc:ploc init properties
      in
      ( loc,
        {
          Ast.Statement.ComponentDeclaration.RestParam.argument =
            ((ploc, t), Ast.Pattern.Object { Ast.Pattern.Object.properties; annot; comments });
          comments = None;
        }
      )
    | Array { annot; elements; comments } ->
      let elements =
        let init = Destructuring.empty t ~annot:has_anno in
        let f = destruct cx in
        Destructuring.array_elements cx ~f init elements
      in
      ( loc,
        {
          Ast.Statement.ComponentDeclaration.RestParam.argument =
            ((ploc, t), Ast.Pattern.Array { Ast.Pattern.Array.elements; annot; comments });
          comments = None;
        }
      )
end
