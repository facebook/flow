(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_common
open Parser_env
open Flow_ast

let missing_annot env = Ast.Type.Missing (Peek.loc_skip_lookahead env)

module Pattern (Parse : Parser_common.PARSER) (Type : Type_parser.TYPE) = struct
  (* Reinterpret various expressions as patterns.
   * This is not the correct thing to do and is only used for assignment
   * expressions. This should be removed and replaced ASAP.
   *)
  let rec object_from_expr =
    let rec properties env acc =
      let open Ast.Expression.Object in
      function
      | [] -> List.rev acc
      | Property (loc, prop) :: remaining ->
        let acc =
          match prop with
          | Property.Init { key; value; shorthand } ->
            let open Ast.Expression in
            let key =
              match key with
              | Property.Literal lit -> Pattern.Object.Property.Literal lit
              | Property.Identifier id -> Pattern.Object.Property.Identifier id
              | Property.PrivateName _ -> failwith "Internal Error: Found object private prop"
              | Property.Computed key -> Pattern.Object.Property.Computed key
            in
            let (pattern, default) =
              match value with
              | (_loc, Assignment { Assignment.operator = None; left; right; comments = _ }) ->
                (left, Some right)
              | _ -> (from_expr env value, None)
            in
            Pattern.Object.Property
              (loc, { Pattern.Object.Property.key; pattern; default; shorthand })
            :: acc
          | Property.Method { key = _; value = (loc, _) } ->
            error_at env (loc, Parse_error.MethodInDestructuring);
            acc
          | Property.Get { key = _; value = (loc, _) }
          | Property.Set { key = _; value = (loc, _) } ->
            (* these should never happen *)
            error_at env (loc, Parse_error.Unexpected "identifier");
            acc
        in
        properties env acc remaining
      | [SpreadProperty (loc, { SpreadProperty.argument; comments })] ->
        let acc =
          Pattern.Object.RestElement
            (loc, { Pattern.RestElement.argument = from_expr env argument; comments })
          :: acc
        in
        properties env acc []
      | SpreadProperty (loc, _) :: remaining ->
        error_at env (loc, Parse_error.PropertyAfterRestElement);
        properties env acc remaining
    in
    fun env (loc, { Ast.Expression.Object.properties = props; comments = _ (* TODO *) }) ->
      ( loc,
        Pattern.(Object { Object.properties = properties env [] props; annot = missing_annot env })
      )

  and array_from_expr =
    (* Convert an Expression to a Pattern if it is a valid
       DestructuringAssignmentTarget, which must be an Object, Array or
       IsValidSimpleAssignmentTarget.
       #sec-destructuring-assignment-static-semantics-early-errors *)
    let assignment_target env ((loc, _) as expr) =
      if Parse.is_assignable_lhs expr then
        Some (from_expr env expr)
      else (
        error_at env (loc, Parse_error.InvalidLHSInAssignment);
        None
      )
    in
    let rec elements env acc =
      let open Ast.Expression in
      function
      | [] -> List.rev acc
      | [Some (Spread (loc, { SpreadElement.argument; comments }))] ->
        (* AssignmentRestElement is a DestructuringAssignmentTarget, see
             #prod-AssignmentRestElement *)
        let acc =
          match assignment_target env argument with
          | Some argument ->
            Some (Pattern.Array.RestElement (loc, { Pattern.RestElement.argument; comments }))
            :: acc
          | None -> acc
        in
        elements env acc []
      | Some (Spread (loc, _)) :: remaining ->
        error_at env (loc, Parse_error.ElementAfterRestElement);
        elements env acc remaining
      | Some
          (Expression (loc, Assignment { Assignment.operator = None; left; right; comments = _ }))
        :: remaining ->
        (* AssignmentElement is a `DestructuringAssignmentTarget Initializer`, see
             #prod-AssignmentElement *)
        let acc =
          Some
            (Pattern.Array.Element
               (loc, { Pattern.Array.Element.argument = left; default = Some right }))
          :: acc
        in
        elements env acc remaining
      | Some (Expression expr) :: remaining ->
        (* AssignmentElement is a DestructuringAssignmentTarget, see
             #prod-AssignmentElement *)
        let acc =
          match assignment_target env expr with
          | Some ((loc, _) as expr) ->
            let element =
              Pattern.Array.Element (loc, { Pattern.Array.Element.argument = expr; default = None })
            in
            Some element :: acc
          | None -> acc
        in
        elements env acc remaining
      | None :: remaining -> elements env (None :: acc) remaining
    in
    fun env (loc, { Ast.Expression.Array.elements = elems; comments }) ->
      ( loc,
        Pattern.Array
          { Pattern.Array.elements = elements env [] elems; annot = missing_annot env; comments } )

  and from_expr env (loc, expr) =
    let open Ast.Expression in
    match expr with
    | Object obj -> object_from_expr env (loc, obj)
    | Array arr -> array_from_expr env (loc, arr)
    | Identifier ((id_loc, { Identifier.name = string_val; comments = _ }) as name) ->
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
      else if not (in_strict_mode env) then
        if allow_yield env && string_val = "yield" then
          error_at env (id_loc, Parse_error.YieldAsIdentifierReference)
        else if allow_await env && string_val = "await" then
          error_at env (id_loc, Parse_error.AwaitAsIdentifierReference);
      ( loc,
        Pattern.Identifier { Pattern.Identifier.name; annot = missing_annot env; optional = false }
      )
    | expr -> (loc, Pattern.Expression (loc, expr))

  (* Parse object destructuring pattern *)
  let rec object_ restricted_error =
    let rest_property env =
      let leading = Peek.comments env in
      let (loc, argument) =
        with_loc
          (fun env ->
            Expect.token env T_ELLIPSIS;
            pattern env restricted_error)
          env
      in
      Pattern.Object.RestElement
        ( loc,
          { Pattern.RestElement.argument; comments = Flow_ast_utils.mk_comments_opt ~leading () } )
    in
    let property_default env =
      match Peek.token env with
      | T_ASSIGN ->
        Expect.token env T_ASSIGN;
        Some (Parse.assignment env)
      | _ -> None
    in
    let rec property env =
      if Peek.token env = T_ELLIPSIS then
        Some (rest_property env)
      else
        let start_loc = Peek.loc env in
        let raw_key = Parse.object_key env in
        match Peek.token env with
        | T_COLON ->
          Expect.token env T_COLON;
          let (loc, (pattern, default)) =
            with_loc
              ~start_loc
              (fun env ->
                let pattern = pattern env restricted_error in
                let default = property_default env in
                (pattern, default))
              env
          in
          let key =
            let open Ast.Expression.Object.Property in
            match raw_key with
            | (_, Literal lit) -> Pattern.Object.Property.Literal lit
            | (_, Identifier id) -> Pattern.Object.Property.Identifier id
            | (_, PrivateName _) -> failwith "Internal Error: Found object private prop"
            | (_, Computed key) -> Pattern.Object.Property.Computed key
          in
          Some
            Pattern.Object.(Property (loc, Property.{ key; pattern; default; shorthand = false }))
        | _ ->
          (match raw_key with
          | ( _,
              Ast.Expression.Object.Property.Identifier
                ((id_loc, { Identifier.name = string_val; comments = _ }) as name) ) ->
            (* #sec-identifiers-static-semantics-early-errors *)
            if is_reserved string_val && string_val <> "yield" && string_val <> "await" then
              (* it is a syntax error if `name` is a reserved word other than await or yield *)
              error_at env (id_loc, Parse_error.UnexpectedReserved)
            else if is_strict_reserved string_val then
              (* it is a syntax error if `name` is a strict reserved word, in strict mode *)
              strict_error_at env (id_loc, Parse_error.StrictReservedWord);
            let (loc, (pattern, default)) =
              with_loc
                ~start_loc
                (fun env ->
                  let pattern =
                    ( id_loc,
                      Pattern.Identifier
                        { Pattern.Identifier.name; annot = missing_annot env; optional = false } )
                  in
                  let default = property_default env in
                  (pattern, default))
                env
            in
            Some
              Pattern.Object.(
                Property
                  ( loc,
                    { Property.key = Property.Identifier name; pattern; default; shorthand = true }
                  ))
          | _ ->
            error_unexpected ~expected:"an identifier" env;

            (* invalid shorthand destructuring *)
            None)
    (* seen_rest is true when we've seen a rest element. rest_trailing_comma is the location of
     * the rest element's trailing command
     * Trailing comma: `let { ...rest, } = obj`
     * Still invalid, but not a trailing comma: `let { ...rest, x } = obj` *)
    and properties env ~seen_rest ~rest_trailing_comma acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        begin
          match rest_trailing_comma with
          | Some loc -> error_at env (loc, Parse_error.TrailingCommaAfterRestElement)
          | None -> ()
        end;
        List.rev acc
      | _ ->
        (match property env with
        | Some ((Pattern.Object.Property (loc, _) | Pattern.Object.RestElement (loc, _)) as prop) ->
          let rest_trailing_comma =
            if seen_rest then (
              error_at env (loc, Parse_error.PropertyAfterRestElement);
              None
            ) else
              rest_trailing_comma
          in
          let (seen_rest, rest_trailing_comma) =
            match prop with
            | Pattern.Object.RestElement _ ->
              ( true,
                if Peek.token env = T_COMMA then
                  Some (Peek.loc env)
                else
                  None )
            | _ -> (seen_rest, rest_trailing_comma)
          in
          if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
          properties env ~seen_rest ~rest_trailing_comma (prop :: acc)
        | None -> properties env ~seen_rest ~rest_trailing_comma acc)
    in
    with_loc (fun env ->
        Expect.token env T_LCURLY;
        let properties = properties env ~seen_rest:false ~rest_trailing_comma:None [] in
        Expect.token env T_RCURLY;
        let annot =
          if Peek.token env = T_COLON then
            Ast.Type.Available (Type.annotation env)
          else
            missing_annot env
        in
        Pattern.Object { Pattern.Object.properties; annot })

  (* Parse array destructuring pattern *)
  and array_ restricted_error =
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET ->
        List.rev acc
      | T_COMMA ->
        Expect.token env T_COMMA;
        elements env (None :: acc)
      | T_ELLIPSIS ->
        let leading = Peek.comments env in
        let (loc, argument) =
          with_loc
            (fun env ->
              Expect.token env T_ELLIPSIS;
              pattern env restricted_error)
            env
        in
        let element =
          Pattern.Array.RestElement
            ( loc,
              {
                Pattern.RestElement.argument;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              } )
        in
        (* rest elements are always last, the closing ] should be next. but if not,
           error and keep going so we recover gracefully by parsing the rest of the
           elements. *)
        if Peek.token env <> T_RBRACKET then (
          error_at env (loc, Parse_error.ElementAfterRestElement);
          if Peek.token env = T_COMMA then Eat.token env
        );
        elements env (Some element :: acc)
      | _ ->
        let (loc, (pattern, default)) =
          with_loc
            (fun env ->
              let pattern = pattern env restricted_error in
              let default =
                match Peek.token env with
                | T_ASSIGN ->
                  Expect.token env T_ASSIGN;
                  Some (Parse.assignment env)
                | _ -> None
              in
              (pattern, default))
            env
        in
        let element = Pattern.Array.(Element (loc, { Element.argument = pattern; default })) in
        if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
        elements env (Some element :: acc)
    in
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LBRACKET;
        let elements = elements env [] in
        Expect.token env T_RBRACKET;
        let annot =
          if Peek.token env = T_COLON then
            Ast.Type.Available (Type.annotation env)
          else
            missing_annot env
        in
        let trailing = Peek.comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        Pattern.Array { Pattern.Array.elements; annot; comments })

  and pattern env restricted_error =
    match Peek.token env with
    | T_LCURLY -> object_ restricted_error env
    | T_LBRACKET -> array_ restricted_error env
    | _ ->
      let (loc, id) = Parse.identifier_with_type env restricted_error in
      (loc, Pattern.Identifier id)
end
