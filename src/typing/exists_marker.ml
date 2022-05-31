(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_polymorphic_ast_mapper
module Ast = Flow_ast

class marker cx =
  object (this)
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] mapper as super

    val mutable cond = false

    val mutable predicate = false

    method on_type_annot x = x

    method on_loc_annot x = x

    method push : 'a 'r. set:('a -> unit) -> get:(unit -> 'a) -> 'a -> 'r Lazy.t -> 'r =
      fun ~set ~get v r ->
        let init = get () in
        set v;
        let res = Lazy.force r in
        set init;
        res

    method push_pred = this#push ~set:(fun x -> predicate <- x) ~get:(fun () -> predicate)

    method! expression expr = this#base_expression false expr

    method! predicate_expression expr = this#base_expression true expr

    method base_expression c expr =
      let open Ast.Expression in
      cond <- c;
      begin
        if c then
          match expr with
          | ( (loc, ty),
              OptionalMember
                { OptionalMember.member = { Member.property = Member.PropertyExpression _; _ }; _ }
            )
          | (_, OptionalMember { OptionalMember.filtered_out = (loc, ty); _ })
          | ((loc, ty), Member _)
          | ((_, ty), Assignment { Assignment.left = ((loc, _), Ast.Pattern.Identifier _); _ }) ->
            Context.add_exists_check cx loc ty
          | ((loc, ty), _) when Base.Option.is_some (Refinement.Keys.key ~allow_optional:false expr)
            ->
            Context.add_exists_check cx loc ty
          | _ -> ()
      end;
      super#expression expr

    method! type_predicate (pred : (_, _) Ast.Type.Predicate.t) =
      let open Ast.Type.Predicate in
      let (annot, { kind; comments = _ }) = pred in
      let _annot' = this#on_loc_annot annot in
      let _kind' =
        match kind with
        | Inferred -> kind
        | Declared expr -> Declared (this#base_expression cond expr)
      in
      pred

    method! return (stmt : (_, _) Ast.Statement.Return.t) =
      let open Ast.Statement.Return in
      let { argument; comments = _; return_out = _ } = stmt in
      let _argument' = Base.Option.map ~f:(this#base_expression predicate) argument in
      stmt

    method! function_ ({ Ast.Function.predicate; _ } as fn) =
      this#push_pred (Base.Option.is_some predicate) (lazy (super#function_ fn))

    method! call annot (expr : (_, _) Ast.Expression.Call.t) =
      let open Ast.Expression.Call in
      let { callee; targs; arguments; comments = _ } = expr in
      match (callee, targs, arguments) with
      | ( (_, Flow_ast.Expression.Identifier (_, { Flow_ast.Identifier.name = "invariant"; _ })),
          None,
          ( _,
            {
              Ast.Expression.ArgList.arguments = Ast.Expression.Expression conditional :: arguments;
              comments = _;
            }
          )
        ) ->
        let _ = this#expression callee in
        let _ = this#base_expression true conditional in
        let _ = Base.List.map ~f:this#expression_or_spread arguments in
        expr
      | _ -> super#call annot expr

    method! logical (expr : (_, _) Ast.Expression.Logical.t) =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments = _ } = expr in
      let c =
        match operator with
        | Ast.Expression.Logical.(And | Or) -> true
        | Ast.Expression.Logical.NullishCoalesce -> false
      in
      let cur_c = cond in
      let _left' = this#base_expression c left in
      let _right' = this#base_expression cur_c right in
      expr

    method! assignment (expr : (_, _) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator; left; right; comments = _ } = expr in
      let left_expr =
        match left with
        | (lhs_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
          Some (lhs_loc, Ast.Expression.Identifier name)
        | (lhs_loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
          Some (lhs_loc, Ast.Expression.Member mem)
        | _ -> None
      in
      match (operator, left_expr) with
      | (Some Ast.Expression.Assignment.(AndAssign | OrAssign), Some left_expr) ->
        let _ = this#base_expression true left_expr in
        let _ = this#expression right in
        expr
      | _ -> super#assignment expr

    method! unary_expression (expr : (_, _) Ast.Expression.Unary.t) =
      let open Ast.Expression.Unary in
      let { argument; operator; comments = _ } = expr in
      match operator with
      | Not ->
        let _ = this#base_expression cond argument in
        expr
      | _ -> super#unary_expression expr
  end

let mark cx ast =
  let marker = new marker cx in
  let _ = marker#program ast in
  ()
