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

    method! block _loc stmt =
      let open Ast.Statement.Block in
      let { comments; _ } = stmt in
      id this#syntax_opt comments stmt (fun comments' -> { stmt with comments = comments' })

    method! object_type _loc obj =
      let open Ast.Type.Object in
      let { comments; _ } = obj in
      id this#syntax_opt comments obj (fun comments' -> { obj with comments = comments' })
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

let apply_remover remover node f =
  match remover with
  | None -> node
  | Some remover -> f remover node

(* Returns a remover function which removes comments beginning after the previous token. *)
let remover_after_last_loc env =
  let remover =
    if Peek.comments env <> [] then
      mk_remover_after_last_loc env
    else
      None
  in
  apply_remover remover

(* Consumes and returns comments on the same line as the previous token. Also returns a remover
   function which can be used to remove comments beginning after the previous token's line. *)
let trailing_and_remover_after_last_line env =
  let trailing = Eat.comments_until_next_line env in
  let remover =
    if trailing <> Peek.comments env then
      mk_remover_after_last_line env
    else
      None
  in
  (trailing, apply_remover remover)
