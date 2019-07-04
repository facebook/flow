(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
open Parser_common
open Parser_env
open Token

module SSet = Set.Make(String)

module Enum
  (Parse: Parser_common.PARSER)
: sig
  val declaration: env -> (Loc.t, Loc.t) Statement.t
end = struct
  open Flow_ast.Statement.EnumDeclaration

  type members = {
    boolean_members: (bool, Loc.t) InitializedMember.t list;
    number_members: (NumberLiteral.t, Loc.t) InitializedMember.t list;
    string_members: (StringLiteral.t, Loc.t) InitializedMember.t list;
    defaulted_members: Loc.t DefaultedMember.t list;
  }
  type acc = {
    members: members;
    seen_names: SSet.t;
  }
  type init =
    | NoInit
    | InvalidInit of Loc.t
    | BooleanInit of Loc.t * bool
    | NumberInit of Loc.t * NumberLiteral.t
    | StringInit of Loc.t * StringLiteral.t

  let empty_members = {
    boolean_members = [];
    number_members = [];
    string_members = [];
    defaulted_members = [];
  }
  let empty_acc = {
    members = empty_members;
    seen_names = SSet.empty;
  }

  let member_init env =
    match Peek.token env, Peek.ith_token ~i:1 env with
    | T_NUMBER {kind; raw}, (T_COMMA | T_RCURLY) ->
      let loc = Peek.loc env in
      let value = Parse.number env kind raw in
      NumberInit (loc, {NumberLiteral.value; raw})
    | T_STRING (loc, value, raw, octal), (T_COMMA | T_RCURLY) ->
      if octal then strict_error env Error.StrictOctalLiteral;
      Eat.token env;
      StringInit (loc, {StringLiteral.value; raw})
    | (T_TRUE | T_FALSE) as token, (T_COMMA | T_RCURLY) ->
      let loc = Peek.loc env in
      Eat.token env;
      BooleanInit (loc, token = T_TRUE)
    | _ ->
      let loc = Peek.loc env in
      Eat.token env;
      InvalidInit loc

  let member_raw = with_loc (fun env ->
    let id = identifier_name env in
    let init = if Expect.maybe env T_ASSIGN then member_init env else NoInit in
    id, init)

  let check_explicit_type_mismatch env ~enum_name ~explicit_type ~member_name literal_type loc =
    match explicit_type with
    | Some enum_type when enum_type <> literal_type ->
        error_at env (loc,
          Parse_error.EnumInvalidMemberInitializer {enum_name; explicit_type; member_name});
    | _ -> ()

  let enum_member ~enum_name ~explicit_type acc env =
    let {members; seen_names} = acc in
    let member_loc, (id, init) = member_raw env in
    let id_loc, {Identifier.name = member_name; _} = id in
    if SSet.mem member_name seen_names then
      error_at env (id_loc, Parse_error.EnumDuplicateMemberName {enum_name; member_name});
    (* if we parsed an empty name, something has gone wrong and we should abort analysis *)
    if member_name = "" then acc else
    let acc = {acc with seen_names = SSet.add member_name seen_names} in
    let check_explicit_type_mismatch =
      check_explicit_type_mismatch env ~enum_name ~explicit_type ~member_name
    in
    match init with
    | BooleanInit (loc, value) ->
      check_explicit_type_mismatch Enum_common.Boolean loc;
      let member = member_loc, {InitializedMember.id; init = loc, value} in
      {acc with members = {members with boolean_members = member :: members.boolean_members}}
    | NumberInit (loc, value) ->
      check_explicit_type_mismatch Enum_common.Number loc;
      let member = member_loc, {InitializedMember.id; init = loc, value} in
      {acc with members = {members with number_members = member :: members.number_members}}
    | StringInit (loc, value) ->
      check_explicit_type_mismatch Enum_common.String loc;
      let member = member_loc, {InitializedMember.id; init = loc, value} in
      {acc with members = {members with string_members = member :: members.string_members}}
    | InvalidInit loc ->
      error_at env (loc,
        Parse_error.EnumInvalidMemberInitializer {enum_name; explicit_type; member_name});
      acc
    | NoInit ->
      begin match explicit_type with
      | Some Enum_common.Boolean ->
        error_at env (member_loc,
          Parse_error.EnumBooleanMemberNotInitialized {enum_name; member_name});
        acc
      | Some Enum_common.Number ->
        error_at env (member_loc,
          Parse_error.EnumNumberMemberNotInitialized {enum_name; member_name});
        acc
      | Some Enum_common.String
      | Some Enum_common.Symbol
      | None ->
        let member = member_loc, {DefaultedMember.id} in
        {acc with
          members = {members with defaulted_members = member :: members.defaulted_members}}
      end

  let rec enum_members ~enum_name ~explicit_type acc env =
    match Peek.token env with
    | T_RCURLY -> {
        boolean_members = List.rev acc.members.boolean_members;
        number_members = List.rev acc.members.number_members;
        string_members = List.rev acc.members.string_members;
        defaulted_members = List.rev acc.members.defaulted_members;
      }
    | _ ->
      let acc = enum_member ~enum_name ~explicit_type acc env in
      if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
      enum_members ~enum_name ~explicit_type acc env

  let string_body ~env ~enum_name ~is_explicit string_members defaulted_members =
    let initialized_len = List.length string_members in
    let defaulted_len = List.length defaulted_members in
    let defaulted_body () = StringBody {StringBody.
      members = StringBody.Defaulted defaulted_members;
      explicitType = is_explicit;
    } in
    let initialized_body () = StringBody {StringBody.
      members = StringBody.Initialized string_members;
      explicitType = is_explicit;
    } in
    match initialized_len, defaulted_len with
    | 0, 0
    | 0, _ ->
      defaulted_body ()
    | _, 0 ->
      initialized_body ()
    | _ when defaulted_len > initialized_len ->
      List.iter (fun (loc, _) ->
        error_at env (loc, Parse_error.EnumStringMemberInconsistentlyInitailized {enum_name});
      ) string_members;
      defaulted_body ()
    | _ ->
      List.iter (fun (loc, _) ->
        error_at env (loc, Parse_error.EnumStringMemberInconsistentlyInitailized {enum_name});
      ) defaulted_members;
      initialized_body ()

  let parse_explicit_type ~enum_name env =
    if Expect.maybe env T_OF then begin
      Eat.push_lex_mode env Lex_mode.TYPE;
      let result = match Peek.token env with
        | T_BOOLEAN_TYPE BOOLEAN -> Some Enum_common.Boolean
        | T_NUMBER_TYPE -> Some Enum_common.Number
        | T_STRING_TYPE -> Some Enum_common.String
        | T_IDENTIFIER {value = "symbol"; _} -> Some Enum_common.Symbol
        | T_IDENTIFIER {value; _} ->
          let supplied_type = Some value in
          error env (Parse_error.EnumInvalidExplicitType {enum_name; supplied_type});
          None
        | _ ->
          error env (Parse_error.EnumInvalidExplicitType {enum_name; supplied_type = None});
          None
      in
      Eat.token env;
      Eat.pop_lex_mode env;
      result
    end else
      None

  let declaration = with_loc (fun env ->
    Expect.token env T_ENUM;
    let id = Parse.identifier env in
    let id_loc, {Identifier.name = enum_name; _} = id in
    let explicit_type = parse_explicit_type ~enum_name env in
    Expect.token env T_LCURLY;
    let members = enum_members ~enum_name ~explicit_type empty_acc env in
    let body = match explicit_type with
      | Some Enum_common.Boolean ->
        BooleanBody {BooleanBody.members = members.boolean_members; explicitType = true}
      | Some Enum_common.Number ->
        NumberBody {NumberBody.members = members.number_members; explicitType = true}
      | Some Enum_common.String ->
        string_body ~env ~enum_name ~is_explicit:true
          members.string_members
          members.defaulted_members
      | Some Enum_common.Symbol ->
        SymbolBody {SymbolBody.members = members.defaulted_members}
      | None ->
        let bools_len = List.length members.boolean_members in
        let nums_len = List.length members.number_members in
        let strs_len = List.length members.string_members in
        let defaulted_len = List.length members.defaulted_members in
        let empty () = StringBody {
          StringBody.members = StringBody.Defaulted [];
          explicitType = false;
        } in
        begin match bools_len, nums_len, strs_len, defaulted_len with
        | 0, 0, 0, 0 ->
          empty ()
        | 0, 0, _, _ ->
          string_body ~env ~enum_name ~is_explicit:false
            members.string_members
            members.defaulted_members
        | _, 0, 0, _ when bools_len >= defaulted_len ->
          List.iter
            (fun (loc, {DefaultedMember.id = _, {Identifier.name = member_name; _}}) ->
              error_at env (loc,
                Parse_error.EnumBooleanMemberNotInitialized {enum_name; member_name}))
            members.defaulted_members;
          BooleanBody {BooleanBody.members = members.boolean_members; explicitType = false}
        | 0, _, 0, _ when nums_len >= defaulted_len ->
          List.iter
            (fun (loc, {DefaultedMember.id = _, {Identifier.name = member_name; _}}) ->
              error_at env (loc,
                Parse_error.EnumNumberMemberNotInitialized {enum_name; member_name}))
            members.defaulted_members;
          NumberBody {NumberBody.members = members.number_members; explicitType = false}
        | _ ->
          error_at env (id_loc, Parse_error.EnumInconsistentMemberValues {enum_name});
          empty ()
        end
    in
    Expect.token env T_RCURLY;
    Statement.EnumDeclaration {
      id;
      body;
    })
end
