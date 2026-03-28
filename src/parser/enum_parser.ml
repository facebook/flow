(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
open Parser_common
open Parser_env
open Token

module Enum (Parse : Parser_common.PARSER) : sig
  val declaration :
    ?leading:Loc.t Comment.t list ->
    const_:bool ->
    env ->
    (Loc.t, Loc.t) Statement.EnumDeclaration.t
end = struct
  open Flow_ast.Statement.EnumDeclaration

  type acc = {
    members: Loc.t member list;
    has_unknown_members: Loc.t option;
    internal_comments: Loc.t Comment.t list;
  }

  type init =
    | NoInit
    | InvalidInit of Loc.t
    | BooleanInit of Loc.t * Loc.t BooleanLiteral.t
    | NumberInit of Loc.t * Loc.t NumberLiteral.t
    | StringInit of Loc.t * Loc.t StringLiteral.t
    | BigIntInit of Loc.t * Loc.t BigIntLiteral.t

  let empty_acc = { members = []; has_unknown_members = None; internal_comments = [] }

  let end_of_member_init env =
    match Peek.token env with
    | T_SEMICOLON
    | T_COMMA
    | T_RCURLY ->
      true
    | _ -> false

  let number_init env loc ~neg ~leading ~kind ~raw =
    let value = Parse.number env kind raw in
    let (value, raw) =
      if neg then
        (-.value, "-" ^ raw)
      else
        (value, raw)
    in
    let trailing = Eat.trailing_comments env in
    if end_of_member_init env then
      NumberInit
        ( loc,
          {
            NumberLiteral.value;
            raw;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
        )
    else
      InvalidInit loc

  let member_init env =
    let loc = Peek.loc env in
    let leading = Peek.comments env in
    match Peek.token env with
    | T_MINUS ->
      Eat.token env;
      (match Peek.token env with
      | T_NUMBER { kind; raw } -> number_init env loc ~neg:true ~leading ~kind ~raw
      | _ -> InvalidInit loc)
    | T_NUMBER { kind; raw } -> number_init env loc ~neg:false ~leading ~kind ~raw
    | T_STRING (loc, value, raw, octal) ->
      if octal then strict_error env Parse_error.StrictOctalLiteral;
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      if end_of_member_init env then
        StringInit
          ( loc,
            {
              StringLiteral.value;
              raw;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            }
          )
      else
        InvalidInit loc
    | (T_TRUE | T_FALSE) as token ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      if end_of_member_init env then
        BooleanInit
          ( loc,
            {
              BooleanLiteral.value = token = T_TRUE;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            }
          )
      else
        InvalidInit loc
    | T_BIGINT { kind; raw } ->
      let value = Parse.bigint env kind raw in
      let trailing = Eat.trailing_comments env in
      if end_of_member_init env then
        BigIntInit
          ( loc,
            {
              BigIntLiteral.value;
              raw;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            }
          )
      else
        InvalidInit loc
    | _ ->
      Eat.token env;
      InvalidInit loc

  let member_raw =
    with_loc (fun env ->
        let id = identifier_name env in
        let init =
          match Peek.token env with
          | T_ASSIGN ->
            Expect.token env T_ASSIGN;
            member_init env
          | T_COLON ->
            let (_, { Identifier.name = member_name; _ }) = id in
            error env (Parse_error.EnumInvalidInitializerSeparator { member_name });
            Expect.token env T_COLON;
            member_init env
          | _ -> NoInit
        in
        (id, init)
    )

  let enum_member ~enum_name ~explicit_type acc env =
    let (member_loc, (id, init)) = member_raw env in
    let (_, { Identifier.name = member_name; _ }) = id in
    (* if we parsed an empty name, something has gone wrong and we should abort analysis *)
    if member_name = "" then
      acc
    else
      match init with
      | BooleanInit (loc, value) ->
        let member = BooleanMember (member_loc, { InitializedMember.id; init = (loc, value) }) in
        { acc with members = member :: acc.members }
      | NumberInit (loc, value) ->
        let member = NumberMember (member_loc, { InitializedMember.id; init = (loc, value) }) in
        { acc with members = member :: acc.members }
      | StringInit (loc, value) ->
        let member = StringMember (member_loc, { InitializedMember.id; init = (loc, value) }) in
        { acc with members = member :: acc.members }
      | BigIntInit (loc, value) ->
        let member = BigIntMember (member_loc, { InitializedMember.id; init = (loc, value) }) in
        { acc with members = member :: acc.members }
      | InvalidInit loc ->
        error_at
          env
          (loc, Parse_error.EnumInvalidMemberInitializer { enum_name; explicit_type; member_name });
        acc
      | NoInit ->
        let member = DefaultedMember (member_loc, { DefaultedMember.id }) in
        { acc with members = member :: acc.members }

  let rec enum_members ~enum_name ~explicit_type acc env =
    match Peek.token env with
    | T_RCURLY
    | T_EOF ->
      (List.rev acc.members, acc.has_unknown_members, acc.internal_comments)
    | T_ELLIPSIS ->
      let loc = Peek.loc env in
      (* Internal comments may appear before the ellipsis *)
      let internal_comments = Peek.comments env in
      Eat.token env;
      (match Peek.token env with
      | T_RCURLY
      | T_EOF ->
        ()
      | T_COMMA ->
        Expect.token env T_COMMA;
        let trailing_comma =
          match Peek.token env with
          | T_RCURLY
          | T_EOF ->
            true
          | _ -> false
        in
        error_at env (loc, Parse_error.EnumInvalidEllipsis { trailing_comma })
      | _ -> error_at env (loc, Parse_error.EnumInvalidEllipsis { trailing_comma = false }));
      enum_members
        ~enum_name
        ~explicit_type
        { acc with has_unknown_members = Some loc; internal_comments }
        env
    | _ ->
      let acc = enum_member ~enum_name ~explicit_type acc env in
      (match Peek.token env with
      | T_RCURLY
      | T_EOF ->
        ()
      | T_SEMICOLON ->
        error env Parse_error.EnumInvalidMemberSeparator;
        Expect.token env T_SEMICOLON
      | _ -> Expect.token env T_COMMA);
      enum_members ~enum_name ~explicit_type acc env

  let parse_explicit_type ~enum_name env =
    if Eat.maybe env T_OF then (
      Eat.push_lex_mode env Lex_mode.TYPE;
      let type_loc = Peek.loc env in
      let result =
        match Peek.token env with
        | T_BOOLEAN_TYPE BOOLEAN -> Some (type_loc, Boolean)
        | T_NUMBER_TYPE -> Some (type_loc, Number)
        | T_STRING_TYPE -> Some (type_loc, String)
        | T_SYMBOL_TYPE -> Some (type_loc, Symbol)
        | T_BIGINT_TYPE -> Some (type_loc, BigInt)
        | T_IDENTIFIER { value; _ } ->
          let supplied_type = Some value in
          error env (Parse_error.EnumInvalidExplicitType { enum_name; supplied_type });
          None
        | _ ->
          error env (Parse_error.EnumInvalidExplicitType { enum_name; supplied_type = None });
          None
      in
      Eat.token env;
      Eat.pop_lex_mode env;
      result
    ) else
      None

  let enum_body ~enum_name =
    with_loc (fun env ->
        let explicit_type = parse_explicit_type ~enum_name env in
        let leading =
          if explicit_type <> None then
            Peek.comments env
          else
            []
        in
        Expect.token env T_LCURLY;
        let explicit_type_t = Option.map snd explicit_type in
        let (members, has_unknown_members, internal) =
          enum_members ~enum_name ~explicit_type:explicit_type_t empty_acc env
        in
        let internal = internal @ Peek.comments env in
        Expect.token env T_RCURLY;
        let trailing =
          match Peek.token env with
          | T_EOF
          | T_RCURLY ->
            Eat.trailing_comments env
          | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
          | _ -> []
        in
        let comments =
          Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ()
        in
        { Body.members; explicit_type; has_unknown_members; comments }
    )

  let declaration ?(leading = []) ~const_ env =
    let leading = leading @ Peek.comments env in
    Expect.token env T_ENUM;
    let id = Parse.identifier env in
    let (_, { Identifier.name = enum_name; _ }) = id in
    let body = enum_body ~enum_name env in
    let comments = Flow_ast_utils.mk_comments_opt ~leading () in
    { Statement.EnumDeclaration.id; body; const_; comments }
end
