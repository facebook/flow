(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Token
open Parser_env
open Ast
module Error = Parse_error

module Pattern
  (Parse: Parser_common.PARSER)
  (Type: Type_parser.TYPE)
 = struct
  (* Reinterpret various expressions as patterns.
   * This is not the correct thing to do and is only used for assignment
   * expressions. This should be removed and replaced ASAP.
   *)
  let object_from_expr =
    let property env prop =
      Ast.Expression.Object.(match prop with
      | Property (loc, { Property.key; value; shorthand; _ }) ->
        let key = Property.(match key with
        | Literal lit -> Pattern.Object.Property.Literal lit
        | Identifier id -> Pattern.Object.Property.Identifier id
        | Computed expr -> Pattern.Object.Property.Computed expr) in
        let pattern = Property.(match value with
        | Init t -> Parse.pattern_from_expr env t
        | Get (loc, f)
        | Set (loc, f) ->
          (* these should never happen *)
          error_at env (loc, Error.UnexpectedIdentifier);
          loc, Pattern.Expression (loc, Ast.Expression.Function f)
        ) in
        Pattern.(Object.Property (loc, Object.Property.({
          key;
          pattern;
          shorthand;
        })))
      | SpreadProperty (loc, { SpreadProperty.argument; }) ->
          let argument = Parse.pattern_from_expr env argument in
          Pattern.Object.(RestProperty (loc, RestProperty.({
            argument;
          }))))

    in fun env (loc, obj) ->
      let properties =
        List.map (property env) obj.Ast.Expression.Object.properties in
      loc, Pattern.(Object Object.({
        properties;
        typeAnnotation = None;
      }))

  let array_from_expr =
    let element env = Ast.Expression.(function
      | None -> None
      | Some (Spread (loc, spread)) ->
          let argument = Parse.pattern_from_expr env (spread.SpreadElement.argument) in
          Some Pattern.Array.(RestElement (loc, { RestElement.argument; }))
      | Some (Expression (loc, expr)) ->
          Some Pattern.Array.(Element (Parse.pattern_from_expr env (loc, expr)))
    )

    in fun env (loc, arr) ->
      let elements =
        List.map (element env) arr.Ast.Expression.Array.elements in
      loc, Pattern.(Array Array.({
        elements;
        typeAnnotation = None;
      }))

  let from_expr env (loc, expr) =
    Ast.Expression.(match expr with
    | Object obj -> object_from_expr env (loc, obj)
    | Array arr ->  array_from_expr env (loc, arr)
    | Identifier name -> loc, Pattern.Identifier {
        Pattern.Identifier.name;
                           typeAnnotation=None;
                           optional=false;
    }
    | Assignment { Assignment.operator = Assignment.Assign; left; right } ->
        loc, Pattern.Assignment { Pattern.Assignment.left; right }
    | expr -> loc, Pattern.Expression (loc, expr))

  (* Parse object destructuring pattern *)
  let rec _object restricted_error =
    let rec property env =
      let start_loc = Peek.loc env in
      if Expect.maybe env T_ELLIPSIS
      then begin
        let argument = pattern env restricted_error in
        let loc = Loc.btwn start_loc (fst argument) in
        Some Pattern.Object.(RestProperty (loc, RestProperty.({
          argument
        })))
      end else begin
        let key = Ast.Expression.Object.Property.(
          match Parse.object_key env with
          | _, Literal lit -> Pattern.Object.Property.Literal lit
          | _, Identifier id -> Pattern.Object.Property.Identifier id
          | _, Computed expr -> Pattern.Object.Property.Computed expr
        ) in
        let prop = match Peek.token env with
          | T_COLON ->
            Expect.token env T_COLON;
            Some (pattern env restricted_error, false)
          | _ ->
            (match key with
            | Pattern.Object.Property.Identifier name ->
              let pattern = (fst name, Pattern.Identifier {
                Pattern.Identifier.name;
                                   typeAnnotation=None;
                                   optional=false;
              }) in
              Some (pattern, true)
            | _ ->
              error_unexpected env; (* invalid shorthand destructuring *)
              None)
        in
        match prop with
        | Some (pattern, shorthand) ->
          let pattern = match Peek.token env with
            | T_ASSIGN ->
              Expect.token env T_ASSIGN;
              let default = Parse.assignment env in
              let loc = Loc.btwn (fst pattern) (fst default) in
              loc, Pattern.(Assignment Assignment.({
                left = pattern;
                right = default;
              }));
            | _ -> pattern
          in
          let loc = Loc.btwn start_loc (fst pattern) in
          Some Pattern.Object.(Property (loc, Property.({
            key;
            pattern;
            shorthand;
          })))
        | None -> None
      end

    and properties env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | _ ->
        (match property env with
        | Some prop ->
          if Peek.token env <> T_RCURLY
          then Expect.token env T_COMMA;
          properties env (prop::acc)
        | None -> properties env acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let properties = properties env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      let end_loc, typeAnnotation =
        if Peek.token env = T_COLON then
          let typeAnnotation = Type.annotation env in
          fst typeAnnotation, Some typeAnnotation
        else end_loc, None
      in
      Loc.btwn start_loc end_loc, Pattern.(Object Object.({
        properties;
        typeAnnotation;
      }))

  (* Parse array destructuring pattern *)
  and _array restricted_error =
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET -> List.rev acc
      | T_COMMA ->
        Expect.token env T_COMMA;
        elements env (None::acc)
      | T_ELLIPSIS ->
        let start_loc = Peek.loc env in
        Expect.token env T_ELLIPSIS;
        let argument = pattern env restricted_error in
        let loc = Loc.btwn start_loc (fst argument) in
        let element = Pattern.Array.(RestElement (loc, RestElement.({
          argument;
        }))) in
        elements env ((Some element)::acc)
      | _ ->
        let pattern = pattern env restricted_error in
        let pattern = match Peek.token env with
          | T_ASSIGN ->
            Expect.token env T_ASSIGN;
            let default = Parse.assignment env in
            let loc = Loc.btwn (fst pattern) (fst default) in
            loc, Pattern.(Assignment Assignment.({
              left = pattern;
              right = default;
            }))
          | _ -> pattern
        in
        let element = Pattern.Array.(Element pattern) in
        if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
        elements env ((Some element)::acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LBRACKET;
      let elements = elements env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RBRACKET;
      let end_loc, typeAnnotation =
        if Peek.token env = T_COLON then
          let typeAnnotation = Type.annotation env in
          fst typeAnnotation, Some typeAnnotation
        else end_loc, None
      in
      Loc.btwn start_loc end_loc, Pattern.(Array Array.({
        elements;
        typeAnnotation;
      }))

  and pattern env restricted_error =
    match Peek.token env with
    | T_LCURLY ->
        _object restricted_error env
    | T_LBRACKET ->
        _array restricted_error env
    | _ ->
        let loc, id = Parse.identifier_with_type env restricted_error in
        loc, Pattern.Identifier id
end
