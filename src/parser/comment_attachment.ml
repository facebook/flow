(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast
open Parser_env

let id = Flow_ast_mapper.id

let map_loc = Flow_ast_mapper.map_loc

let id_list_last (map : 'a -> 'a) (lst : 'a list) : 'a list =
  match List.rev lst with
  | [] -> lst
  | hd :: tl ->
    let hd' = map hd in
    if hd == hd' then
      lst
    else
      List.rev (hd' :: tl)

(* Mapper that removes all trailing comments that appear after a given position in an AST node *)
class ['loc] trailing_comments_remover ~after_pos =
  object (this)
    inherit ['loc] Flow_ast_mapper.mapper

    method! syntax comments =
      let open Syntax in
      let { trailing; _ } = comments in
      let trailing' =
        List.filter (fun (loc, _) -> Loc.(pos_cmp loc.start after_pos < 0)) trailing
      in
      if List.length trailing = List.length trailing' then
        comments
      else
        { comments with trailing = trailing' }

    method! array _loc expr =
      let open Ast.Expression.Array in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! array_type t =
      let open Ast.Type.Array in
      let { comments; _ } = t in
      id this#syntax_opt comments t (fun comments' -> { t with comments = comments' })

    method! assignment _loc expr =
      let open Ast.Expression.Assignment in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! binary _loc expr =
      let open Ast.Expression.Binary in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! block _loc stmt =
      let open Ast.Statement.Block in
      let { comments; _ } = stmt in
      id this#syntax_opt comments stmt (fun comments' -> { stmt with comments = comments' })

    method! call _annot expr =
      let open Ast.Expression.Call in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! class_ _loc cls =
      let open Ast.Class in
      let { body; comments; _ } = cls in
      let body' = this#class_body body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        cls
      else
        { cls with body = body'; comments = comments' }

    method! class_body body = body

    method! conditional _loc expr =
      let open Ast.Expression.Conditional in
      let { alternate; comments; _ } = expr in
      let alternate' = this#expression alternate in
      let comments' = this#syntax_opt comments in
      if alternate == alternate' && comments = comments' then
        expr
      else
        { expr with alternate = alternate'; comments = comments' }

    method! function_ _loc func =
      let open Ast.Function in
      let { body; comments; _ } = func in
      let body' = this#function_body_any body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        func
      else
        { func with body = body'; comments = comments' }

    method! function_type _loc func =
      let open Ast.Type.Function in
      let { return; comments; _ } = func in
      let return' = this#type_ return in
      let comments' = this#syntax_opt comments in
      if return == return' && comments == comments' then
        func
      else
        { func with return = return'; comments = comments' }

    method! generic_type _loc t =
      let open Ast.Type.Generic in
      let { id; comments; _ } = t in
      let id' = this#generic_identifier_type id in
      let comments' = this#syntax_opt comments in
      if id == id' && comments == comments' then
        t
      else
        { t with id = id'; comments = comments' }

    method! generic_identifier_type git =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> id this#identifier i git (fun i -> Unqualified i)
      | Qualified (loc, ({ id; _ } as qualified)) ->
        let id' = this#identifier id in
        if id == id' then
          git
        else
          Qualified (loc, { qualified with id = id' })

    method! import _loc expr =
      let open Ast.Expression.Import in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! interface_type _loc t =
      let open Ast.Type.Interface in
      let { body; comments; _ } = t in
      let body' = map_loc this#object_type body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        t
      else
        { t with body = body'; comments = comments' }

    method! intersection_type _loc t =
      let { Ast.Type.Intersection.types = (t0, t1, ts); comments } = t in
      let (t1', ts') =
        match ts with
        | [] -> (this#type_ t1, [])
        | _ -> (t1, id_list_last this#type_ ts)
      in
      let comments' = this#syntax_opt comments in
      if t1 == t1' && ts == ts' && comments == comments' then
        t
      else
        { Ast.Type.Intersection.types = (t0, t1', ts'); comments = comments' }

    method! jsx_element _loc elem =
      let open Ast.JSX in
      let { comments; _ } = elem in
      id this#syntax_opt comments elem (fun comments' -> { elem with comments = comments' })

    method! jsx_fragment _loc frag =
      let open Ast.JSX in
      let { frag_comments = comments; _ } = frag in
      id this#syntax_opt comments frag (fun comments' -> { frag with frag_comments = comments' })

    method! logical _loc expr =
      let open Ast.Expression.Logical in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! new_ _loc expr =
      let open Ast.Expression.New in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! member _loc expr =
      let open Ast.Expression.Member in
      let { property; comments; _ } = expr in
      let property' = this#member_property property in
      let comments' = this#syntax_opt comments in
      if property == property' && comments == comments' then
        expr
      else
        { expr with property = property'; comments = comments' }

    method! object_ _loc expr =
      let open Ast.Expression.Object in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! object_type _loc obj =
      let open Ast.Type.Object in
      let { comments; _ } = obj in
      id this#syntax_opt comments obj (fun comments' -> { obj with comments = comments' })

    method! predicate pred =
      let open Ast.Type.Predicate in
      let (loc, { kind; comments }) = pred in
      id this#syntax_opt comments pred (fun comments' -> (loc, { kind; comments = comments' }))

    method! sequence _loc expr =
      let open Ast.Expression.Sequence in
      let { expressions; comments } = expr in
      let expressions' = id_list_last this#expression expressions in
      let comments' = this#syntax_opt comments in
      if expressions == expressions' && comments == comments' then
        expr
      else
        { expressions = expressions'; comments = comments' }

    method! template_literal _loc expr =
      let open Ast.Expression.TemplateLiteral in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! tuple_type t =
      let open Ast.Type.Tuple in
      let { comments; _ } = t in
      id this#syntax_opt comments t (fun comments' -> { t with comments = comments' })

    method! type_cast _loc expr =
      let open Ast.Expression.TypeCast in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! type_params tparams =
      let open Ast.Type.TypeParams in
      let (loc, { params; comments }) = tparams in
      id this#syntax_opt comments tparams (fun comments' -> (loc, { params; comments = comments' }))

    method! union_type _loc t =
      let { Ast.Type.Union.types = (t0, t1, ts); comments } = t in
      let (t1', ts') =
        match ts with
        | [] -> (this#type_ t1, [])
        | _ -> (t1, id_list_last this#type_ ts)
      in
      let comments' = this#syntax_opt comments in
      if t1 == t1' && ts == ts' && comments == comments' then
        t
      else
        { Ast.Type.Union.types = (t0, t1', ts'); comments = comments' }

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id = ident; init }) = decl in
      match init with
      | None ->
        id (this#variable_declarator_pattern ~kind) ident decl (fun ident' ->
            (loc, { id = ident'; init }))
      | Some init ->
        id this#expression init decl (fun init' -> (loc, { id = ident; init = Some init' }))
  end

let mk_remover_after_last_loc env =
  let open Loc in
  match Parser_env.last_loc env with
  | None -> None
  | Some { _end; _ } -> Some (new trailing_comments_remover ~after_pos:_end)

let mk_remover_after_last_line env =
  let open Loc in
  match Parser_env.last_loc env with
  | None -> None
  | Some { _end = { line; _ }; _ } ->
    let next_line_start = { line = line + 1; column = 0 } in
    Some (new trailing_comments_remover ~after_pos:next_line_start)

type trailing_and_remover_result = {
  trailing: Loc.t Comment.t list;
  remove_trailing: 'a. 'a -> (Loc.t trailing_comments_remover -> 'a -> 'a) -> 'a;
}

(* Returns a remover function which removes comments beginning after the previous token.
   No trailing comments are returned, since all comments since the last loc should be removed. *)
let trailing_and_remover_after_last_loc : Parser_env.env -> trailing_and_remover_result =
 fun env ->
  let remover =
    if Peek.comments env <> [] then
      mk_remover_after_last_loc env
    else
      None
  in
  {
    trailing = [];
    remove_trailing =
      (fun node f ->
        match remover with
        | None -> node
        | Some remover -> f remover node);
  }

(* Consumes and returns comments on the same line as the previous token. Also returns a remover
   function which can be used to remove comments beginning after the previous token's line. *)
let trailing_and_remover_after_last_line : Parser_env.env -> trailing_and_remover_result =
 fun env ->
  let trailing = Eat.comments_until_next_line env in
  let remover =
    if trailing <> Peek.comments env then
      mk_remover_after_last_line env
    else
      None
  in
  {
    trailing;
    remove_trailing =
      (fun node f ->
        match remover with
        | None -> node
        | Some remover -> f remover node);
  }
