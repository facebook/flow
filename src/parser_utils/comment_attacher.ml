(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

open Flow_ast_visitor

type attachment_pos =
  | LeadingLine
  (* NOT IMPLEMENTED *)
  (* TODO: Add support for these attachment types *)
  | InlineLeft (* On the same line immediately to the left *)
  | InlineRight (* On the same line immediately to the right *)
  | InlineAtEnd (* Last item on line, takes precedence over `InlineRight` *)
  | TrailingLine (* Any non statement level comments, not attachable as `LeadingLine` *)
  | TrailingBlock (* Any comments after the last statement within a block *)

type info = {
  (* Resulting map of node loc to list of comments and attachment type *)
  attached_comments: ((attachment_pos * Loc.t Ast.Comment.t) list) Utils_js.LocMap.t;
  unattached_comments: Loc.t Ast.Comment.t list;
}
module Acc = struct
  type t = info
  let init (comments: Loc.t Ast.Comment.t list) = {
    attached_comments = Utils_js.LocMap.empty;
    (* Sort comments into  *)
    unattached_comments = List.sort
      (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2)
      comments;
  }
end

class comment_attacher ~comments = object(this)
  inherit [Acc.t, Loc.t] visitor ~init:(Acc.init comments) as super

  method private check_loc node_loc =
    match acc.unattached_comments with
    | [] -> ()
    | (comment_loc, _) as comment::rest ->
      let open Loc in
      (* Check if comment is on line before node *)
      if comment_loc.start.line < node_loc.start.line then begin
        this#update_acc (fun acc ->
          let existing = match Utils_js.LocMap.get node_loc acc.attached_comments with
            | Some e -> e | None -> []
          in
          {
          attached_comments = Utils_js.LocMap.add node_loc
            (existing @ [(LeadingLine, comment)])
            acc.attached_comments;
          unattached_comments = rest
        });
        this#check_loc node_loc
      end

  method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
    let (loc, _) = stmt in
    this#check_loc loc;
    super#statement stmt

  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let (loc, _) = expr in
    this#check_loc loc;
    super#expression expr

  method! identifier (expr: Loc.t Ast.Identifier.t) =
    let (loc, _) = expr in
    this#check_loc loc;
    super#identifier expr

  method! object_property (prop: (Loc.t, Loc.t) Ast.Expression.Object.Property.t) =
    let (loc, _) = prop in
    this#check_loc loc;
    super#object_property prop

  method! class_element (elem: (Loc.t, Loc.t) Ast.Class.Body.element) =
    let open Ast.Class.Body in
    begin match elem with
    | Method (loc, _)
    | Property (loc, _)
    | PrivateField (loc, _) -> this#check_loc loc
    end;
    super#class_element elem

end

let program ~ast =
  let (_, _, comments) = ast in
  let walk = new comment_attacher ~comments in
  walk#eval walk#program ast
