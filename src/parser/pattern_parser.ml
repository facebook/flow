(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_common
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
      | Property (loc, prop)::remaining ->
          let key, pattern, shorthand = match prop with
          | Property.Init { key; value; shorthand } ->
            key, from_expr env value, shorthand
          | Property.Method { key; value = (loc, f) } ->
            error_at env (loc, Parse_error.MethodInDestructuring);
            key, (loc, Pattern.Expression (loc, Ast.Expression.Function f)), false
          | Property.Get { key; value = (loc, f) }
          | Property.Set { key; value = (loc, f) } ->
            (* these should never happen *)
            error_at env (loc, Parse_error.UnexpectedIdentifier);
            key, (loc, Pattern.Expression (loc, Ast.Expression.Function f)), false
          in
          let key = match key with
          | Property.Literal lit -> Pattern.Object.Property.Literal lit
          | Property.Identifier id -> Pattern.Object.Property.Identifier id
          | Property.PrivateName _ -> failwith "Internal Error: Found object private prop"
          | Property.Computed expr -> Pattern.Object.Property.Computed expr
          in
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

    fun env (loc, { Ast.Expression.Object.properties = props }) ->
      loc, Pattern.(Object { Object.
        properties = properties env [] props;
        typeAnnotation = None;
      })

  and array_from_expr =
    (* Convert an Expression to a Pattern if it is a valid
       DestructuringAssignmentTarget, which must be an Object, Array or
       IsValidSimpleAssignmentTarget.
       #sec-destructuring-assignment-static-semantics-early-errors *)
    let assignment_target env ((loc, _) as expr) =
      if Parse.is_assignable_lhs expr then
        Some (from_expr env expr)
      else begin
        error_at env (loc, Parse_error.InvalidLHSInAssignment);
        None
      end
    in

    let rec elements env acc = Ast.Expression.(function
      | [] -> List.rev acc
      | Some (Spread (loc, { SpreadElement.argument }))::[] ->
          (* AssignmentRestElement is a DestructuringAssignmentTarget, see
             #prod-AssignmentRestElement *)
          let acc = match assignment_target env argument with
          | Some argument ->
            (Some Pattern.Array.(RestElement (loc, { RestElement.argument; }))) :: acc
          | None ->
            acc
          in
          elements env acc []
      | Some (Spread (loc, _))::remaining ->
          error_at env (loc, Parse_error.ElementAfterRestElement);
          elements env acc remaining
      | Some (Expression (_, Assignment { Assignment.
          operator = Assignment.Assign; _
        } as expr))::remaining ->
          (* AssignmentElement is a `DestructuringAssignmentTarget Initializer`, see
             #prod-AssignmentElement *)
          let acc = Some (Pattern.Array.Element (from_expr env expr)) :: acc in
          elements env acc remaining
      | Some (Expression expr)::remaining ->
          (* AssignmentElement is a DestructuringAssignmentTarget, see
             #prod-AssignmentElement *)
          let acc = match assignment_target env expr with
          | Some expr -> (Some (Pattern.Array.Element expr)) :: acc
          | None -> acc
          in
          elements env acc remaining
      | None::remaining ->
          elements env (None::acc) remaining
    )
    in

    fun env (loc, { Ast.Expression.Array.elements = elems }) ->
      loc, Pattern.Array { Pattern.Array.
        elements = elements env [] elems;
        typeAnnotation = None;
      }

  and from_expr env (loc, expr) =
    Ast.Expression.(match expr with
    | Object obj -> object_from_expr env (loc, obj)
    | Array arr ->  array_from_expr env (loc, arr)
    | Identifier ((id_loc, string_val) as name) ->
        (* per #sec-destructuring-assignment-static-semantics-early-errors,
           it is a syntax error if IsValidSimpleAssignmentTarget of this
           IdentifierReference is false. That happens when `string_val` is
           "eval" or "arguments" in strict mode. *)
        if in_strict_mode env && is_restricted string_val then
          error_at env (id_loc, Parse_error.StrictLHSAssignment)
        (* per #prod-IdentifierReference, yield is only a valid
           IdentifierReference when [~Yield], and await is only valid
           when [~Await]. but per #sec-identifiers-static-semantics-early-errors,
           they are already invalid in strict mode, which we should have
           already errored about when parsing the expression that we're now
           converting into a pattern. *)
        else if not (in_strict_mode env) then begin
          if allow_yield env && string_val = "yield" then
            error_at env (id_loc, Parse_error.YieldAsIdentifierReference)
          else if allow_await env && string_val = "await" then
            error_at env (id_loc, Parse_error.AwaitAsIdentifierReference)
        end;
        loc, Pattern.Identifier { Pattern.Identifier.
          name;
          typeAnnotation = None;
          optional = false;
        }
    | Assignment { Assignment.operator = Assignment.Assign; left; right } ->
        loc, Pattern.Assignment { Pattern.Assignment.left; right }
    | expr -> loc, Pattern.Expression (loc, expr))

  (* Parse object destructuring pattern *)
  let rec object_ restricted_error =
    let rec property env =
      if Peek.token env = T_ELLIPSIS then begin
        let loc, argument = with_loc (fun env ->
          Expect.token env T_ELLIPSIS;
          pattern env restricted_error
        ) env in
        Some Pattern.Object.(RestProperty (loc, { RestProperty.
          argument
        }))
      end else begin
        let start_loc = Peek.loc env in
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
            | Pattern.Object.Property.Identifier ((id_loc, string_val) as name) ->
              (* #sec-identifiers-static-semantics-early-errors *)
              begin
                if is_reserved string_val && string_val <> "yield" && string_val <> "await" then
                  (* it is a syntax error if `name` is a reserved word other than await or yield *)
                  error_at env (id_loc, Parse_error.UnexpectedReserved)
                else if is_strict_reserved string_val then
                  (* it is a syntax error if `name` is a strict reserved word, in strict mode *)
                  strict_error_at env (id_loc, Parse_error.StrictReservedWord)
              end;
              let pattern = (id_loc, Pattern.Identifier { Pattern.Identifier.
                name;
                typeAnnotation = None;
                optional = false;
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

    (* seen_rest is true when we've seen a rest element. rest_trailing_comma is the location of
     * the rest element's trailing command
     * Trailing comma: `let { ...rest, } = obj`
     * Still invalid, but not a trailing comma: `let { ...rest, x } = obj` *)
    and properties env ~seen_rest ~rest_trailing_comma acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        begin match rest_trailing_comma with
        | Some loc -> error_at env (loc, Parse_error.TrailingCommaAfterRestElement)
        | None -> ()
        end;
        List.rev acc
      | _ ->
        (match property env with
        | Some (Pattern.Object.Property (loc, _) | Pattern.Object.RestProperty (loc, _) as prop) ->
          let rest_trailing_comma =
            if seen_rest
            then begin
              error_at env (loc, Parse_error.PropertyAfterRestProperty);
              None
            end
            else rest_trailing_comma
          in
          let seen_rest, rest_trailing_comma = begin match prop with
          | Pattern.Object.RestProperty _ ->
            true, if Peek.token env = T_COMMA then Some (Peek.loc env) else None
          | _ -> seen_rest, rest_trailing_comma end in
          if Peek.token env <> T_RCURLY
          then Expect.token env T_COMMA;
          properties env ~seen_rest ~rest_trailing_comma (prop::acc)
        | None -> properties env ~seen_rest ~rest_trailing_comma acc)
    in
    with_loc (fun env ->
      Expect.token env T_LCURLY;
      let properties = properties env ~seen_rest:false ~rest_trailing_comma:None [] in
      Expect.token env T_RCURLY;
      let typeAnnotation =
        if Peek.token env = T_COLON then Some (Type.annotation env)
        else None
      in
      Pattern.Object { Pattern.Object.properties; typeAnnotation; }
    )

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
        let loc, argument = with_loc (fun env ->
          Expect.token env T_ELLIPSIS;
          pattern env restricted_error
        ) env in
        let element = Pattern.Array.(RestElement (loc, { RestElement.
          argument;
        })) in
        (* rest elements are always last, the closing ] should be next. but if not,
           error and keep going so we recover gracefully by parsing the rest of the
           elements. *)
        if Peek.token env <> T_RBRACKET then begin
          error_at env (loc, Parse_error.ElementAfterRestElement);
          if Peek.token env = T_COMMA then Eat.token env
        end;
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
    in
    with_loc (fun env ->
      Expect.token env T_LBRACKET;
      let elements = elements env [] in
      Expect.token env T_RBRACKET;
      let typeAnnotation =
        if Peek.token env = T_COLON then Some (Type.annotation env)
        else None
      in
      Pattern.Array { Pattern.Array.elements; typeAnnotation; }
    )

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
