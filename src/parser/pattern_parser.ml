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

module Pattern
  (Parse: Parser_common.PARSER)
  (Type: Type_parser.TYPE)
 = struct
  (* Reinterpret various expressions as patterns.
   * This is not the correct thing to do and is only used for assignment
   * expressions. This should be removed and replaced ASAP.
   *)
  let rec object_from_expr =
    let rec properties env acc = Ast.Expression.Object.(function
      | [] -> List.rev acc
      | Property (loc, { Property.key; value; shorthand; _ })::remaining ->
          let key = Property.(match key with
          | Literal lit -> Pattern.Object.Property.Literal lit
          | Identifier id -> Pattern.Object.Property.Identifier id
          | PrivateName _ -> failwith "Internal Error: Found object private prop"
          | Computed expr -> Pattern.Object.Property.Computed expr) in
          let pattern = Property.(match value with
          | Init t -> from_expr env t
          | Get (loc, f)
          | Set (loc, f) ->
            (* these should never happen *)
            error_at env (loc, Parse_error.UnexpectedIdentifier);
            loc, Pattern.Expression (loc, Ast.Expression.Function f)
          ) in
          let acc = Pattern.(Object.Property (loc, { Object.Property.
            key;
            pattern;
            shorthand;
          })) :: acc in
          properties env acc remaining
      | SpreadProperty (loc, { SpreadProperty.argument; })::[] ->
          let acc = Pattern.Object.(RestProperty (loc, { RestProperty.
            argument = from_expr env argument;
          })) :: acc in
          properties env acc []
      | SpreadProperty (loc, _)::remaining ->
          error_at env (loc, Parse_error.PropertyAfterRestProperty);
          properties env acc remaining
    ) in

    fun env (loc, obj) ->
      loc, Pattern.(Object { Object.
        properties = properties env [] obj.Ast.Expression.Object.properties;
        typeAnnotation = None;
      })

  and array_from_expr =
    let rec elements env acc = Ast.Expression.(function
      | [] -> List.rev acc
      | Some (Spread (loc, { SpreadElement.argument }))::[] ->
          (* spreads are ok as the last element *)
          let argument = from_expr env argument in
          let acc = (Some Pattern.Array.(RestElement (loc, { RestElement.argument; })))::acc in
          elements env acc []
      | Some (Spread (loc, _))::remaining ->
          error_at env (loc, Parse_error.ElementAfterRestElement);
          elements env acc remaining
      | Some (Expression (loc, expr))::remaining ->
          let acc = Some Pattern.Array.(Element (from_expr env (loc, expr))) :: acc in
          elements env acc remaining
      | None::remaining ->
          elements env (None::acc) remaining
    )
    in

    fun env (loc, arr) ->
      loc, Pattern.(Array { Array.
        elements = elements env [] arr.Ast.Expression.Array.elements;
        typeAnnotation = None;
      })

  and from_expr env (loc, expr) =
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
  let rec object_ restricted_error =
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
          | _, PrivateName _ -> failwith "Internal Error: Found object private prop"
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
  and array_ restricted_error =
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
        object_ restricted_error env
    | T_LBRACKET ->
        array_ restricted_error env
    | _ ->
        let loc, id = Parse.identifier_with_type env restricted_error in
        loc, Pattern.Identifier id
end
