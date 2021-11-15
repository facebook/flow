(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason
open Type

module Make
    (Env : Env_sig.S)
    (Destructuring : Destructuring_sig.S)
    (Statement : Statement_sig.S with module Env := Env) : Func_stmt_config_sig.S = struct
  type 'T ast = (ALoc.t, 'T) Ast.Function.Params.t

  type 'T param_ast = (ALoc.t, 'T) Ast.Function.Param.t

  type 'T rest_ast = (ALoc.t, 'T) Ast.Function.RestParam.t

  type 'T this_ast = (ALoc.t, 'T) Ast.Function.ThisParam.t

  type pattern =
    | Id of (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t
    | Object of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        properties: (ALoc.t, ALoc.t) Ast.Pattern.Object.property list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }
    | Array of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        elements: (ALoc.t, ALoc.t) Ast.Pattern.Array.element list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }

  type param =
    | Param of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        pattern: pattern;
        default: (ALoc.t, ALoc.t) Ast.Expression.t option;
        has_anno: bool;
      }

  type rest =
    | Rest of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        id: (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t;
        has_anno: bool;
      }

  type this_param =
    | This of {
        t: Type.t;
        loc: ALoc.t;
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation;
      }

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
    Scope.(
      if Context.enable_const_params cx then
        let kind = Entry.ConstParamBinding in
        Env.bind_implicit_const ~state:State.Initialized kind cx name t loc
      else
        Env.bind_implicit_let
          ~state:State.Initialized
          Entry.ParamBinding
          cx
          (OrdinaryName name)
          t
          loc
    )

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

  let eval_default cx = function
    | None -> None
    | Some e -> Some (Statement.expression cx ~annot:None e)

  let eval_param cx (Param { t; loc; ploc; pattern; default; has_anno }) =
    match pattern with
    | Id id ->
      let default = eval_default cx default in
      let () =
        match default with
        | None -> ()
        | Some ((_, default_t), _) -> Flow.flow_t cx (default_t, t)
      in
      let () =
        let { Ast.Pattern.Identifier.name = ((loc, _), { Ast.Identifier.name; _ }); optional; _ } =
          id
        in
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
        bind cx name t loc
      in
      (loc, { Ast.Function.Param.argument = ((ploc, t), Ast.Pattern.Identifier id); default })
    | Object { annot; properties; comments } ->
      let default = eval_default cx default in
      let properties =
        let default = Base.Option.map default (fun ((_, t), _) -> Default.expr t) in
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
        let default = Base.Option.map default (fun ((_, t), _) -> Default.expr t) in
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
