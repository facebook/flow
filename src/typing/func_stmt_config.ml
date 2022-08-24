(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason
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

  let bind cx name t loc =
    if Context.enable_const_params cx then
      Env.bind_implicit_const cx t loc
    else
      Env.bind_implicit_let cx (OrdinaryName name) t loc

  let destruct cx ~use_op ~name_loc ~has_anno name default t =
    Base.Option.iter
      ~f:(fun d ->
        let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
        let default_t = Flow.mk_default cx reason d in
        Flow.flow cx (default_t, UseT (use_op, t)))
      default;
    let t_bound =
      if has_anno then
        Annotated t
      else
        Inferred t
    in
    bind cx name t_bound name_loc;
    t

  let eval_default cx ~annot_t = function
    | None -> None
    | Some e ->
      (match annot_t with
      | Some annot_t ->
        let ((default_loc, default_t), default_ast) =
          Statement.expression cx ~hint:(Hint_t annot_t) e
        in
        let use_op =
          Op
            (AssignVar
               { var = Some (TypeUtil.reason_of_t annot_t); init = TypeUtil.reason_of_t default_t }
            )
        in
        Flow.flow cx (default_t, UseT (use_op, annot_t));
        Some ((default_loc, annot_t), default_ast)
      | None -> Some (Statement.expression cx ~hint:Hint_None e))

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno }) =
    match pattern with
    | Id
        ( { Ast.Pattern.Identifier.name = ((name_loc, _), { Ast.Identifier.name; _ }); optional; _ }
        as id
        ) ->
      let default = eval_default cx ~annot_t:None default in
      let () =
        match default with
        | None -> ()
        | Some ((_, default_t), _) ->
          let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
          let use_op =
            Op (AssignVar { var = Some reason; init = TypeUtil.reason_of_t default_t })
          in
          Flow.flow cx (default_t, UseT (use_op, t))
      in
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
        bind cx name t name_loc
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
        let init = Destructuring.empty ?default t ~annot:(Base.Option.is_some annot_t) in
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
        let init =
          Destructuring.empty
            ?default
            t
            ~annot:
              (match annot with
              | Ast.Type.Missing _ -> false
              | Ast.Type.Available _ -> true)
        in
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
      let { Ast.Pattern.Identifier.name = ((loc, _), { Ast.Identifier.name; _ }); _ } = id in
      let t =
        if has_anno then
          Annotated t
        else
          Inferred t
      in
      bind cx name t loc
    in
    ( loc,
      { Ast.Function.RestParam.argument = ((ploc, t), Ast.Pattern.Identifier id); comments = None }
    )

  let eval_this _ (This { t = _; annot; loc }) =
    (* this does not bind any parameters *)
    (loc, { Ast.Function.ThisParam.annot; comments = None })
end
