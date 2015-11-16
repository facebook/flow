(*
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Lexer_flow
open Parser_env
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
module SMap = Map.Make(String)

let is_future_reserved = function
  | "enum" -> true
  | _ -> false

let is_strict_reserved = function
  | "interface"
  | "implements"
  | "package"
  | "private"
  | "protected"
  | "public"
  | "static"
  | "yield" -> true
  | _ -> false

let is_restricted = function
  | "eval"
  | "arguments" -> true
  | _ -> false

(* Answer questions about what comes next *)
module Peek = struct
  open Loc

  (* If you're looping waiting for a token, then use token_loop instead. *)
  let token ?(i=0) env = (lookahead ~i env).lex_token
  let value ?(i=0) env = (lookahead ~i env).lex_value
  let loc ?(i=0) env = (lookahead ~i env).lex_loc

  (* True if there is a line terminator before the next token *)
  let line_terminator env =
    match last_loc env with
      | None -> false
      | Some loc' ->
          (loc env).start.line > loc'.start.line

  let is_implicit_semicolon env =
    match token env with
    | T_EOF | T_RCURLY -> true
    | T_SEMICOLON -> false
    | _ -> line_terminator env

  let semicolon_loc ?(i=0) env =
    if token ~i env = T_SEMICOLON
    then Some (loc ~i env)
    else None

  (* This returns true if the next token is identifier-ish (even if it is an
   * error) *)
  let identifier ?(i=0) env =
    let name = value ~i env in
    match token ~i env with
    | _ when
      is_strict_reserved name ||
      is_restricted name ||
      is_future_reserved name-> true
    | T_LET
    | T_TYPE
    | T_OF
    | T_DECLARE
    | T_ASYNC
    | T_AWAIT
    | T_IDENTIFIER -> true
    | _ -> false

  let _function ?(i=0) env =
    token ~i env = T_FUNCTION ||
    (token ~i env = T_ASYNC && token ~i:(i+1) env = T_FUNCTION)
end

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)

(* Complains about an error at the location of the lookahead *)
let error env e =
  let loc = Peek.loc env in
  error_at env (loc, e)

let strict_error env e = if strict env then error env e

let strict_error_at env (loc, e) = if strict env then error_at env (loc, e)

let rec filter_duplicate_errors acc = function
| [_] | [] as l -> l
| (loc1, _) :: ((loc2, _) :: _ as rl) when Loc.compare loc1 loc2 = 0 ->
    filter_duplicate_errors acc rl
| x :: rl -> filter_duplicate_errors (x :: acc) rl

let get_unexpected_error = function
  | T_EOF, _ -> Error.UnexpectedEOS
  | T_NUMBER _, _ -> Error.UnexpectedNumber
  | T_JSX_TEXT _, _
  | T_STRING _, _ -> Error.UnexpectedString
  | T_IDENTIFIER, _ -> Error.UnexpectedIdentifier
  | _, word when is_future_reserved word -> Error.UnexpectedReserved
  | _, word when is_strict_reserved word -> Error.StrictReservedWord
  | _, value -> Error.UnexpectedToken value

let error_unexpected env =
  let lookahead = lookahead env in
  (* So normally we consume the lookahead lex result when Eat.advance is
   * called, which will add any lexing errors to our list of errors. However,
   * raising an unexpected error for a lookahead is kind of like consuming that
   * token, so we should process any lexing errors before complaining about the
   * unexpected token *)
  error_list env (lookahead.lex_errors);
  clear_lookahead_errors env;
  error env (get_unexpected_error (lookahead.lex_token, lookahead.lex_value))

(* Consume zero or more tokens *)
module Eat : sig
  val advance : env -> lex_env * lex_result -> lex_mode -> unit
  val token : env -> unit
  val push_lex_mode : env -> lex_mode -> unit
  val pop_lex_mode : env -> unit
  val double_pop_lex_mode : env -> unit
  val semicolon : env -> unit
end = struct
  let advance = Parser_env.advance

  (* TODO should this be in Parser_env? *)
  (* Consume a single token *)
  let token env =
    let last = lex_env env, lookahead env in
    advance env last (lex_mode env)

  let push_lex_mode = Parser_env.push_lex_mode
  let pop_lex_mode = Parser_env.pop_lex_mode
  let double_pop_lex_mode = Parser_env.double_pop_lex_mode

  (* Semicolon insertion is handled here :(. There seem to be 2 cases where
  * semicolons are inserted. First, if we reach the EOF. Second, if the next
  * token is } or is separated by a LineTerminator.
  *)
  let semicolon env =
    if not (Peek.is_implicit_semicolon env)
    then
      if Peek.token env = T_SEMICOLON
      then token env
      else error_unexpected env
end

module Expect = struct
  let token env t =
    if Peek.token env <> t then error_unexpected env;
    Eat.token env

  let eof env =
    if Peek.token env <> T_EOF then error_unexpected env;
    let eof_lex_result = lookahead env in
    Eat.advance env (lex_env env, eof_lex_result) (lex_mode env)

  (* If the next token is t, then eat it and return true
   * else return false *)
  let maybe env t =
    if Peek.token env = t
    then begin
      Eat.token env;
      true
    end else false

  let contextual env str =
    if Peek.value env <> str
    then error_unexpected env;
    Eat.token env
end

module rec Parse : sig
  val program : env -> Ast.program
  val statement : env -> Ast.Statement.t
  val statement_list_item : env -> Ast.Statement.t
  val statement_list : term_fn:(token->bool) -> env -> Ast.Statement.t list
  val statement_list_with_directives : term_fn:(token->bool) -> env -> Ast.Statement.t list * bool
  val module_body : term_fn:(token->bool) -> env -> Ast.Statement.t list
  val expression : env -> Ast.Expression.t
  val assignment : env -> Ast.Expression.t
  val object_initializer : env -> Loc.t * Ast.Expression.Object.t
  val array_initializer : env -> Loc.t * Ast.Expression.Array.t
  val identifier : ?restricted_error:Error.t -> env -> Ast.Identifier.t
  val identifier_or_reserved_keyword : env -> (Ast.Identifier.t * (Loc.t * Error.t) option)
  val identifier_with_type : env -> Error.t -> Ast.Identifier.t
  val block_body : env -> Loc.t * Ast.Statement.Block.t
  val function_block_body : env -> Loc.t * Ast.Statement.Block.t * bool
  val jsx_element : env -> Loc.t * Ast.JSX.element
  val pattern : env -> Ast.Expression.t -> Ast.Pattern.t
  val object_pattern_with_type : env -> (Loc.t * Ast.Expression.Object.t) -> Ast.Pattern.t
  val array_pattern_with_type : env -> (Loc.t * Ast.Expression.Array.t) -> Ast.Pattern.t
  val object_key : env -> Loc.t * Ast.Expression.Object.Property.key
  val class_declaration : env -> Ast.Statement.t
  val class_expression : env -> Ast.Expression.t
  val is_assignable_lhs : Ast.Expression.t -> bool
end = struct
  module Type : sig
    val _type : env -> Ast.Type.t
    val type_parameter_declaration : env -> Ast.Type.ParameterDeclaration.t option
    val type_parameter_instantiation : env -> Ast.Type.ParameterInstantiation.t option
    val generic : env -> Loc.t * Ast.Type.Generic.t
    val _object : ?allow_static:bool -> env -> Loc.t * Type.Object.t
    val function_param_list : env -> Type.Function.Param.t option * Type.Function.Param.t list
    val annotation : env -> Ast.Type.annotation
    val annotation_opt : env -> Ast.Type.annotation option
  end = struct
    type param_list_or_type =
      | ParamList of (Type.Function.Param.t option * Type.Function.Param.t list)
      | Type of Type.t

    let rec _type env = union env

    and annotation env =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let start_loc = Peek.loc env in
      Expect.token env T_COLON;
      let typeAnnotation = _type env in
      let end_loc = match last_loc env with
      | Some loc -> loc
      | None -> assert false in
      Loc.btwn start_loc end_loc, typeAnnotation

    and rev_nonempty_acc acc =
      let end_loc = match acc with
      | (loc, _)::_ -> loc
      | _ -> assert false in
      let acc = List.rev acc in
      let start_loc = match acc with
      | (loc, _)::_ -> loc
      | _ -> assert false in
      Loc.btwn start_loc end_loc, acc

    and union env =
      let left = intersection env in
      union_with env left

    and union_with =
      let rec unions env acc =
        match Peek.token env with
        | T_BIT_OR ->
            Expect.token env T_BIT_OR;
            unions env (intersection env::acc)
        | _ ->
            let loc, acc = rev_nonempty_acc acc in
            loc, Type.Union acc
      in fun env left ->
        if Peek.token env = T_BIT_OR
        then unions env [left]
        else left

    and intersection env =
      let left = prefix env in
      intersection_with env left

    and intersection_with =
      let rec intersections env acc =
        match Peek.token env with
        | T_BIT_AND ->
            Expect.token env T_BIT_AND;
            intersections env (prefix env::acc)
        | _ ->
            let loc, acc = rev_nonempty_acc acc in
            loc, Type.Intersection acc
      in fun env left ->
        if Peek.token env = T_BIT_AND
        then intersections env [left]
        else left

    and prefix env =
      match Peek.token env with
      | T_PLING ->
          let loc = Peek.loc env in
          Expect.token env T_PLING;
          let t = prefix env in
          Loc.btwn loc (fst t), Type.Nullable t
      | _ ->
          postfix env

    and postfix env =
      let t = primary env in
      postfix_with env t

    and postfix_with env t =
      if Expect.maybe env T_LBRACKET
      then begin
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        let loc = Loc.btwn (fst t) end_loc in
        let t = loc, Type.Array t in
        postfix_with env t
      end else t

    and primary env =
      let loc = Peek.loc env in
      match Peek.token env with
      | T_MULT ->
          Expect.token env T_MULT;
          loc, Type.Exists
      | T_LESS_THAN -> _function env
      | T_LPAREN -> function_or_group env
      | T_LCURLY ->
          let loc, o = _object env in
          loc, Type.Object o
      | T_TYPEOF ->
          let start_loc = Peek.loc env in
          Expect.token env T_TYPEOF;
          let t = primary env in
          Loc.btwn start_loc (fst t), Type.Typeof t
      | T_LBRACKET -> tuple env
      | T_IDENTIFIER ->
          let loc, g = generic env in
          loc, Type.Generic g
      | T_STRING (loc, value, raw, octal)  ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          loc, Type.(StringLiteral StringLiteral.({
            value;
            raw;
          }))
      | T_NUMBER_SINGLETON_TYPE (number_type, value) ->
          let raw = Peek.value env in
          Expect.token env (T_NUMBER_SINGLETON_TYPE (number_type, value));
          if number_type = LEGACY_OCTAL
          then strict_error env Error.StrictOctalLiteral;
          loc, Type.(NumberLiteral NumberLiteral.({
            value;
            raw;
          }))
      | (T_TRUE | T_FALSE) as token ->
          let raw = Peek.value env in
          Expect.token env token;
          let value = token = T_TRUE in
          loc, Type.(BooleanLiteral BooleanLiteral.({
            value;
            raw;
          }))
      | token ->
          match primitive token with
          | Some t ->
              Expect.token env token;
              loc, t
          | None ->
              error_unexpected env;
              loc, Type.Any

    and primitive = function
      | T_ANY_TYPE     -> Some Type.Any
      | T_BOOLEAN_TYPE -> Some Type.Boolean
      | T_NUMBER_TYPE  -> Some Type.Number
      | T_STRING_TYPE  -> Some Type.String
      | T_VOID_TYPE    -> Some Type.Void
      | T_NULL         -> Some Type.Null
      | _ -> None

    and tuple =
      let rec types env acc =
        match Peek.token env with
        | T_EOF
        | T_RBRACKET -> List.rev acc
        | _ ->
            let acc = (_type env)::acc in
            if Peek.token env = T_RBRACKET
            (* Trailing comma support (like [number, string,]) *)
            then begin
              if Peek.token env = T_COMMA then Expect.token env T_COMMA
            end else Expect.token env T_COMMA;
            types env acc

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let tl = types env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Type.Tuple tl

    and function_param_with_id env name =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let optional = Expect.maybe env T_PLING in
      Expect.token env T_COLON;
      let typeAnnotation = _type env in
      Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
        name;
        typeAnnotation;
        optional;
      })

    and function_param_list_without_parens =
      let param env =
        let name, _ = Parse.identifier_or_reserved_keyword env in
        function_param_with_id env name

      in let rec param_list env acc =
        match Peek.token env with
        | T_EOF
        | T_ELLIPSIS
        | T_RPAREN as t ->
            let rest = if t = T_ELLIPSIS
            then begin
              Expect.token env T_ELLIPSIS;
              Some (param env)
            end else None in
            rest, List.rev acc
        | _ ->
          let acc = (param env)::acc in
          if Peek.token env <> T_RPAREN
          then Expect.token env T_COMMA;
          param_list env acc

      in fun env -> param_list env

    and function_param_list env =
        Expect.token env T_LPAREN;
        let ret = function_param_list_without_parens env [] in
        Expect.token env T_RPAREN;
        ret

    and param_list_or_type env =
      Expect.token env T_LPAREN;
      let ret = match Peek.token env with
      | T_EOF
      | T_ELLIPSIS ->
          (* (... is definitely the beginning of a param list *)
          ParamList (function_param_list_without_parens env [])
      | T_RPAREN ->
          (* () or is definitely a param list *)
          ParamList (None, [])
      | T_IDENTIFIER ->
          (* This could be a function parameter or a generic type *)
          function_param_or_generic_type env
      | token ->
          (match primitive token with
          | None ->
              (* All params start with an identifier or ... *)
              Type (_type env)
          | Some t ->
              (* Don't know if this is (number) or (number: number) (the first is
              * a type the second is a param) *)
            let name, _ = Parse.identifier_or_reserved_keyword env in
            match Peek.token env with
            | T_PLING
            | T_COLON ->
                (* Ok this is definitely a parameter *)
                if not (should_parse_types env)
                then error env Error.UnexpectedTypeAnnotation;
                let optional = Expect.maybe env T_PLING in
                Expect.token env T_COLON;
                let typeAnnotation = _type env in
                if Peek.token env <> T_RPAREN
                then Expect.token env T_COMMA;
                let param = Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
                  name;
                  typeAnnotation;
                  optional;
                }) in
                ParamList (function_param_list_without_parens env [param])
            | _ ->
                (* Ok this is definitely a type *)
                (* Note; what we really want here (absent 2-token LA :) is
                    Eat.vomit env;
                    Type (_type env)
                  ...but currently there's bad interaction between Eat.vomit,
                  Expect.tok, and possibly mode switching. See e.g. Type
                  Grouping test failures when the above is used.
                *)
                Type
                  (union_with env
                    (intersection_with env
                      (postfix_with env (fst name, t)))
                ))
      in
      Expect.token env T_RPAREN;
      ret

    and function_param_or_generic_type env =
      let id = Parse.identifier env in
      match Peek.token env with
      | T_PLING (* optional param *)
      | T_COLON ->
          let param = function_param_with_id env id in
          ignore (Expect.maybe env T_COMMA);
          ParamList (function_param_list_without_parens env [param])
      | _ ->
          Type (union_with env
                  (intersection_with env
                    (postfix_with env (generic_type_with_identifier env id)))
                )

    and function_or_group env =
      let start_loc = Peek.loc env in
      match param_list_or_type env with
      | ParamList (rest, params) ->
        Expect.token env T_ARROW;
        let returnType = _type env in
        let end_loc = fst returnType in
        Loc.btwn start_loc end_loc, Type.(Function Function.({
          params;
          returnType;
          rest;
          typeParameters = None;
        }))
      | Type _type -> _type

    and _function env =
      let start_loc = Peek.loc env in
      let typeParameters = type_parameter_declaration env in
      let rest, params = function_param_list env in
      Expect.token env T_ARROW;
      let returnType = _type env in
      let end_loc = fst returnType in
      Loc.btwn start_loc end_loc, Type.(Function Function.({
        params;
        returnType;
        rest;
        typeParameters;
      }))

    and _object =
      let methodish env start_loc =
        let typeParameters = type_parameter_declaration env in
        let rest, params = function_param_list env in
        Expect.token env T_COLON;
        let returnType = _type env in
        let loc = Loc.btwn start_loc (fst returnType) in
        loc, Type.Function.({
          params;
          returnType;
          rest;
          typeParameters;
        })

      in let method_property env start_loc static key =
        let value = methodish env start_loc in
        let value = fst value, Type.Function (snd value) in
        fst value, Type.Object.Property.({
          key;
          value;
          optional = false;
          static;
          _method = true;
        })

      in let call_property env start_loc static =
        let value = methodish env (Peek.loc env) in
        Loc.btwn start_loc (fst value), Type.Object.CallProperty.({
          value;
          static;
        })

      in let property env start_loc static key =
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        let optional = Expect.maybe env T_PLING in
        Expect.token env T_COLON;
        let value = _type env in
        Loc.btwn start_loc (fst value), Type.Object.Property.({
          key;
          value;
          optional;
          static;
          _method = false;
        })

      in let indexer_property env start_loc static =
        Expect.token env T_LBRACKET;
        let id, _ = Parse.identifier_or_reserved_keyword env in
        Expect.token env T_COLON;
        let key = _type env in
        Expect.token env T_RBRACKET;
        Expect.token env T_COLON;
        let value = _type env in
        Loc.btwn start_loc (fst value), Type.Object.Indexer.({
          id;
          key;
          value;
          static;
        })

      in let semicolon env =
        match Peek.token env with
        | T_COMMA | T_SEMICOLON -> Eat.token env
        | T_RCURLY -> ()
        | _ -> error_unexpected env

      in let rec properties ~allow_static env (acc, indexers, callProperties) =
        let start_loc = Peek.loc env in
        let static = allow_static && Expect.maybe env T_STATIC in
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc, List.rev indexers, List.rev callProperties
        | T_LBRACKET ->
          let indexer = indexer_property env start_loc static in
          semicolon env;
          properties allow_static env (acc, indexer::indexers, callProperties)
        | T_LESS_THAN
        | T_LPAREN ->
          let call_prop = call_property env start_loc static in
          semicolon env;
          properties allow_static env (acc, indexers, call_prop::callProperties)
        | _ ->
          let static, (_, key) = match static, Peek.token env with
          | true, T_COLON ->
              strict_error_at env (start_loc, Error.StrictReservedWord);
              let static_key =
                start_loc, Expression.Object.Property.Identifier ( start_loc, {
                  Identifier.name = "static";
                  optional = false;
                  typeAnnotation = None;
                }) in
              false, static_key
          | _ ->
              Eat.push_lex_mode env NORMAL_LEX;
              let key = Parse.object_key env in
              Eat.pop_lex_mode env;
              static, key
          in
          let property = match Peek.token env with
          | T_LESS_THAN
          | T_LPAREN -> method_property env start_loc static key
          | _ -> property env start_loc static key in
          semicolon env;
          properties allow_static env (property::acc, indexers, callProperties)

      in fun ?(allow_static=false) env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let properties, indexers, callProperties =
          properties ~allow_static env ([], [], []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Type.Object.({
          properties;
          indexers;
          callProperties
        })

    and type_parameter_declaration =
      let rec params env acc =
        let acc = (Parse.identifier_with_type env Error.StrictParamName)::acc in
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          Expect.token env T_COMMA;
          params env acc

      in fun env ->
          let start_loc = Peek.loc env in
          if Peek.token env = T_LESS_THAN
          then begin
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeAnnotation;
            Expect.token env T_LESS_THAN;
            let params = params env [] in
            let loc = Loc.btwn start_loc (Peek.loc env) in
            Expect.token env T_GREATER_THAN;
            Some (loc, Type.ParameterDeclaration.({
              params;
            }))
          end else None

    and type_parameter_instantiation =
      let rec params env acc =
        let acc = (_type env)::acc in
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          Expect.token env T_COMMA;
          params env acc

      in fun env ->
          let start_loc = Peek.loc env in
          if Peek.token env = T_LESS_THAN
          then begin
            Expect.token env T_LESS_THAN;
            let params = params env [] in
            let loc = Loc.btwn start_loc (Peek.loc env) in
            Expect.token env T_GREATER_THAN;
            Some (loc, Type.ParameterInstantiation.({
              params;
            }))
          end else None

    and generic env = raw_generic_with_identifier env (Parse.identifier env)

    and raw_generic_with_identifier =
      let rec identifier env (q_loc, qualification) =
        if Peek.token env = T_PERIOD
        then begin
          Expect.token env T_PERIOD;
          let id = Parse.identifier env in
          let loc = Loc.btwn q_loc (fst id) in
          let qualification = Type.Generic.Identifier.(Qualified (loc, {
            qualification;
            id;
          })) in
          identifier env (loc, qualification)
        end else (q_loc, qualification)

      in fun env id ->
        let id = fst id, Type.Generic.Identifier.Unqualified id in
        let id_loc, id = identifier env id in
        let typeParameters = type_parameter_instantiation env in
        let loc = match typeParameters with
        | None -> id_loc
        | Some (loc, _) -> Loc.btwn id_loc loc in
        loc, Type.Generic.({
          id;
          typeParameters;
        })

    and generic_type_with_identifier env id =
      let loc, generic = raw_generic_with_identifier env id in
      loc, Type.Generic generic

    and annotation_opt env =
      match Peek.token env with
      | T_COLON -> Some (annotation env)
      | _ -> None

    let wrap f env =
      let env = env |> with_strict true in
      Eat.push_lex_mode env TYPE_LEX;
      let ret = f env in
      Eat.pop_lex_mode env;
      ret

    let _type = wrap _type
    let type_parameter_declaration = wrap type_parameter_declaration
    let type_parameter_instantiation = wrap type_parameter_instantiation
    let _object ?(allow_static=false) env = wrap (_object ~allow_static) env
    let function_param_list = wrap function_param_list
    let annotation = wrap annotation
    let annotation_opt = wrap annotation_opt
    let generic = wrap generic
  end

  module Declaration = struct
    let pattern env restricted_error =
      match Peek.token env with
      | T_LCURLY ->
          let obj = Parse.object_initializer env in
          Parse.object_pattern_with_type env obj
      | T_LBRACKET ->
          let arr = Parse.array_initializer env in
          Parse.array_pattern_with_type env arr
      | _ ->
          let id = Parse.identifier_with_type env restricted_error in
          fst id, Pattern.Identifier id

    let check_param =
      let rec pattern ((env, _) as check_env) (loc, p) = Pattern.(match p with
        | Object o -> _object check_env o
        | Array arr -> _array check_env arr
        | Identifier id -> identifier check_env id
        | Expression _ -> (
            error_at env (loc, Error.InvalidLHSInFormalsList);
            check_env
          )
      )

      and _object check_env o =
        List.fold_left
          object_property
          check_env
          o.Pattern.Object.properties

      and object_property check_env = Pattern.Object.(function
        | Property (_, property) -> Property.(
            let check_env = match property.key with
            | Identifier id -> identifier_no_dupe_check check_env id
            | _ -> check_env in
            pattern check_env property.pattern)
        | SpreadProperty (_, { SpreadProperty.argument; }) ->
            pattern check_env argument)

      and _array check_env arr =
        List.fold_left
        array_element
        check_env
        arr.Pattern.Array.elements

      and array_element check_env = Pattern.Array.(function
        | None -> check_env
        | Some (Element p) -> pattern check_env p
        | Some (Spread (_, { SpreadElement.argument; })) ->
            pattern check_env argument)

      and identifier (env, param_names) (loc, { Identifier.name; _ } as id) =
        if SSet.mem name param_names
        then error_at env (loc, Error.StrictParamDupe);
        let env, param_names =
          identifier_no_dupe_check (env, param_names) id in
        env, SSet.add name param_names

      and identifier_no_dupe_check (env, param_names) (loc, { Identifier.name; _ }) =
        if is_restricted name
        then strict_error_at env (loc, Error.StrictParamName);
        if is_future_reserved name || is_strict_reserved name
        then strict_error_at env (loc, Error.StrictReservedWord);
        env, param_names

      in pattern

    (* Strict is true if we were already in strict mode or if we are newly in
     * strict mode due to a directive in the function.
     * Simple is the IsSimpleParameterList thing from the ES6 spec *)
    let strict_post_check env ~strict ~simple id params =
      if strict || not simple
      then
        (* If we are doing this check due to strict mode than there are two
         * cases to consider. The first is when we were already in strict mode
         * and therefore already threw strict errors. In this case we want to
         * do these checks outside of strict mode. The other is if we
         * originally parsed in non-strict mode but now are strict. Then we
         * want to do these checks in strict mode *)
        let env =
          if strict
          then env |> with_strict (not (Parser_env.strict env))
          else env in
        (match id with
        | Some (loc, { Identifier.name; _ }) ->
            if is_restricted name
            then strict_error_at env (loc, Error.StrictFunctionName);
            if is_future_reserved name || is_strict_reserved name
            then strict_error_at env (loc, Error.StrictReservedWord)
        | None -> ());
        ignore (List.fold_left check_param (env, SSet.empty) params)

    let function_params =
      let rec param env =
        let id = pattern env Error.StrictParamName in
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          let default = Parse.assignment env in
          id, Some default
        end else
          id, None
      and param_list env (params, defaults, has_default) =
        match Peek.token env with
        | T_EOF
        | T_RPAREN
        | T_ELLIPSIS as t ->
            let rest = if t = T_ELLIPSIS
            then begin
              Expect.token env T_ELLIPSIS;
              Some (Parse.identifier_with_type env Error.StrictParamName)
            end else None in
            if Peek.token env <> T_RPAREN
            then error env Error.ParameterAfterRestParameter;
            List.rev params, (if has_default then List.rev defaults else []), rest
        | _ ->
            let param, default = param env in
            let has_default = has_default || default <> None in
            if Peek.token env <> T_RPAREN
            then Expect.token env T_COMMA;
            param_list env (param::params, default::defaults, has_default)

      in fun env ->
        Expect.token env T_LPAREN;
        let params, defaults, rest = param_list env ([], [], false) in
        Expect.token env T_RPAREN;
        params, defaults, rest

    let function_body env ~async ~generator =
      let env = enter_function env ~async ~generator in
      let loc, block, strict = Parse.function_block_body env in
      loc, Statement.FunctionDeclaration.BodyBlock (loc, block), strict

    let concise_function_body env ~async ~generator =
      let env = env |> with_in_function true in
      match Peek.token env with
      | T_LCURLY ->
          let _, body, strict = function_body env ~async ~generator in
          body, strict
      | _ ->
          let env = enter_function env ~async ~generator in
          let expr = Parse.assignment env in
          Statement.FunctionDeclaration.BodyExpression expr, strict env

    let generator env is_async =
      match is_async, Expect.maybe env T_MULT with
      | true, true ->
          error env Error.AsyncGenerator;
          true
      | _, result -> result

    let async env = Expect.maybe env T_ASYNC

    let is_simple_function_params =
      let is_simple_param = function
      | _, Pattern.Identifier _ ->  true
      | _ -> false

      in fun params defaults rest ->
        defaults = [] && rest = None && List.for_all is_simple_param params

    let _function env =
      let start_loc = Peek.loc env in
      let async = async env in
      Expect.token env T_FUNCTION;
      let generator = generator env async in
      let id = (
        match in_export env, Peek.token env with
        | true, T_LPAREN -> None
        | _ -> Some(
            Parse.identifier ~restricted_error:Error.StrictFunctionName env
          )
      ) in
      let typeParameters = Type.type_parameter_declaration env in
      let params, defaults, rest = function_params env in
      let returnType = Type.annotation_opt env in
      let _, body, strict = function_body env ~async ~generator in
      let simple = is_simple_function_params params defaults rest in
      strict_post_check env ~strict ~simple id params;
      let end_loc, expression = Ast.Statement.FunctionDeclaration.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      Loc.btwn start_loc end_loc, Statement.(FunctionDeclaration FunctionDeclaration.({
        id;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        expression;
        returnType;
        typeParameters;
      }))

    let variable_declaration_list =
      let variable_declaration env =
        let id = pattern env Error.StrictVarName in
        let init, errs = if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          Some (Parse.assignment env), []
        end else Ast.Pattern.(
          match id with
          | _, Identifier _ -> None, []
          | loc, _ -> None, [(loc, Error.NoUninitializedDestructuring)]
        ) in
        let end_loc = match init with
        | Some expr -> fst expr
        | _ -> fst id in
        (Loc.btwn (fst id) end_loc, Ast.Statement.VariableDeclaration.Declarator.({
          id;
          init;
        })), errs

      in let rec helper env decls errs =
        let decl, errs_ = variable_declaration env in
        let decls = decl::decls in
        let errs = errs_ @ errs in
        if Peek.token env = T_COMMA
        then begin
          Expect.token env T_COMMA;
          helper env decls errs
        end else
          let end_loc = match decls with
          | (loc, _)::_ -> loc
          | _ -> Loc.none in
          let declarations = List.rev decls in
          let start_loc = match decls with
          | (loc, _)::_ -> loc
          | _ -> Loc.none in
          Loc.btwn start_loc end_loc, declarations, List.rev errs

      in fun env -> helper env [] []

    let declarations token kind env =
      let start_loc = Peek.loc env in
      Expect.token env token;
      let loc, declarations, errs = variable_declaration_list env in
      (Loc.btwn start_loc loc, Statement.VariableDeclaration.({
        kind;
        declarations;
      })), errs

    let var = declarations T_VAR Statement.VariableDeclaration.Var

    let const env =
      let env = env |> with_no_let true in
      let (loc, variable), errs =
        declarations T_CONST Statement.VariableDeclaration.Const env in
      (* Make sure all consts defined are initialized *)
      let errs = Statement.VariableDeclaration.(
        List.fold_left (fun errs decl ->
          match decl with
          | loc, { Declarator.init = None; _ } ->
              (loc, Error.NoUninitializedConst)::errs
          | _ -> errs
        ) errs variable.declarations
      ) in
      (loc, variable), List.rev errs

    let _let env =
      let env = env |> with_no_let true in
      declarations T_LET Statement.VariableDeclaration.Let env

    let variable env =
      let start_loc = Peek.loc env in
      let (end_loc, variable), errs = match Peek.token env with
      | T_CONST -> const env
      | T_LET   -> _let env
      | T_VAR   -> var env
      | _ ->
          error_unexpected env;
          (* We need to return something. This is as good as anything else *)
          var env in
      (Loc.btwn start_loc end_loc, Statement.VariableDeclaration variable), errs
  end

  module Expression = struct
    (* AssignmentExpression :
     *   ConditionalExpression
     *   LeftHandSideExpression = AssignmentExpression
     *   LeftHandSideExpression AssignmentOperator AssignmentExpression
     *   ArrowFunctionFunction
     *
     *   Originally we were parsing this without backtracking, but
     *   ArrowFunctionExpression got too tricky. Oh well.
     *)
    let rec assignment =
      let assignment_but_not_arrow_function env =
        let expr = conditional env in
        (match assignment_op env with
        | Some operator ->
          if not (is_assignable_lhs expr)
          then error_at env (fst expr, Error.InvalidLHSInAssignment);

          (match expr with
          | loc, Expression.Identifier (_, { Identifier.name = name; _ })
            when is_restricted name ->
              strict_error_at env (loc, Error.StrictLHSAssignment)
          | _ -> ());

          let left = Parse.pattern env expr in
          let right = assignment env in
          let loc = Loc.btwn (fst left) (fst right) in

          loc, Expression.(Assignment Assignment.({
            operator;
            left;
            right;
          }))
        | _ -> expr)

      in let error_callback _ _ = raise Try.Rollback

      (* So we may or may not be parsing the first part of an arrow function
       * (the part before the =>). We might end up parsing that whole thing or
       * we might end up parsing only part of it and thinking we're done. We
       * need to look at the next token to figure out if we really parsed an
       * assignment expression or if this is just the beginning of an arrow
       * function *)
      in let try_assignment_but_not_arrow_function env =
        let env = env |> with_error_callback error_callback in
        let ret = assignment_but_not_arrow_function env in
        match Peek.token env with
        | T_ARROW (* x => 123 *)
        | T_COLON -> (* (x): number => 123 *)
          raise Try.Rollback
        (* async x => 123 -- and we've already parsed async as an identifier
         * expression *)
        | _ when Peek.identifier env -> begin match snd ret with
          | Expression.Identifier (_, {Identifier.name = "async"; _ })
              when not (Peek.line_terminator env) ->
            raise Try.Rollback
          | _ -> ret
          end
        | _ -> ret

      in fun env ->
        match Peek.token env, Peek.identifier env with
        | T_YIELD, _ when (allow_yield env) -> yield env
        | T_LPAREN, _
        | T_LESS_THAN, _
        | _, true ->

          (* Ok, we don't know if this is going to be an arrow function of a
          * regular assignment expression. Let's first try to parse it as an
          * assignment expression. If that fails we'll try an arrow function.
          *)
          (match Try.to_parse env try_assignment_but_not_arrow_function with
          | Try.ParsedSuccessfully expr -> expr
          | Try.FailedToParse ->
            (match Try.to_parse env try_arrow_function with
              | Try.ParsedSuccessfully expr -> expr
              | Try.FailedToParse ->

                  (* Well shoot. It doesn't parse cleanly as a normal
                   * expression or as an arrow_function. Let's treat it as a
                   * normal assignment expression gone wrong *)
                  assignment_but_not_arrow_function env
            )
          )
        | _ -> assignment_but_not_arrow_function env

    and yield env =
      let start_loc = Peek.loc env in
      Expect.token env T_YIELD;
      if not (allow_yield env)
      then error env Error.IllegalYield;
      let delegate = Expect.maybe env T_MULT in
      let has_argument = not (
        Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
      ) in
      let argument =
        if delegate || has_argument
        then Some (assignment env)
        else None in
      let end_loc = match argument with
      | Some expr -> fst expr
      | None ->
          let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> start_loc in
          Eat.semicolon env;
          end_loc in
      Loc.btwn start_loc end_loc, Expression.(Yield Yield.({
        argument;
        delegate;
      }))

    and is_lhs = Expression.(function
      | _, Member _
      | _, Identifier _ -> true
      | _, Array _
      | _, Object _
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Class _
      | _, Function _
      | _, New _
      | _, Call _
      | _, Comprehension _
      | _, Generator _
      | _, Assignment _
      | _, Binary _
      | _, Conditional _
      | _, Logical _
      | _, Sequence _
      | _, Unary _
      | _, Update _
      | _, ArrowFunction _
      | _, Yield _
      | _, JSXElement _
      | _, Let _
      | _, TypeCast _ -> false)

    and is_assignable_lhs = Expression.(function
      | _, Array _
      | _, Object _
      | _, Member _
      | _, Identifier _ -> true
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Class _
      | _, Function _
      | _, New _
      | _, Call _
      | _, Comprehension _
      | _, Generator _
      | _, Assignment _
      | _, Binary _
      | _, Conditional _
      | _, Logical _
      | _, Sequence _
      | _, Unary _
      | _, Update _
      | _, ArrowFunction _
      | _, Yield _
      | _, JSXElement _
      | _, Let _
      | _, TypeCast _ -> false)


    and assignment_op env =
      let op = Expression.Assignment.(match Peek.token env with
      | T_RSHIFT3_ASSIGN -> Some RShift3Assign
      | T_RSHIFT_ASSIGN -> Some RShiftAssign
      | T_LSHIFT_ASSIGN -> Some LShiftAssign
      | T_BIT_XOR_ASSIGN -> Some BitXorAssign
      | T_BIT_OR_ASSIGN -> Some BitOrAssign
      | T_BIT_AND_ASSIGN -> Some BitAndAssign
      | T_MOD_ASSIGN -> Some ModAssign
      | T_DIV_ASSIGN -> Some DivAssign
      | T_MULT_ASSIGN -> Some MultAssign
      | T_MINUS_ASSIGN -> Some MinusAssign
      | T_PLUS_ASSIGN -> Some PlusAssign
      | T_ASSIGN -> Some Assign
      | _ -> None) in
      if op <> None then Eat.token env;
      op

    and conditional env =
      let expr = logical env in
      if Peek.token env = T_PLING
      then begin
        Expect.token env T_PLING;
        (* no_in is ignored for the consequent *)
        let env' = env |> with_no_in false in
        let consequent = assignment env' in
        Expect.token env T_COLON;
        let alternate = assignment env in
        let test = expr in
        let loc = Loc.btwn (fst test) (fst alternate) in
        loc, Expression.(Conditional Conditional.({
          test;
          consequent;
          alternate;
        }))
      end else expr

    and logical =
      let rec logical_and env left =
        match Peek.token env with
        | T_AND ->
            Expect.token env T_AND;
            let right = binary env in
            let loc = Loc.btwn (fst left) (fst right) in
            logical_and env (loc, Expression.(Logical Logical.({
              operator = And;
              left;
              right;
            })))
        | _  -> left
      and logical_or env left =
        match Peek.token env with
        | T_OR ->
            Expect.token env T_OR;
            let right = logical_and env (binary env) in
            let loc = Loc.btwn (fst left) (fst right) in
            logical_or env (loc, Expression.(Logical Logical.({
              operator = Or;
              left;
              right;
            })))
        | _ -> left
      in fun env ->
        let left = binary env in
        logical_or env (logical_and env left)

    and binary =
      (* All BinaryExpression operators are left associative *)
      let binary_op env =
        let ret = Expression.Binary.(match Peek.token env with
        (* Lowest pri *)
        | T_BIT_OR -> Some (BitOr, 2)
        | T_BIT_XOR -> Some (Xor, 3)
        | T_BIT_AND -> Some (BitAnd, 4)
        | T_EQUAL -> Some (Equal, 5)
        | T_STRICT_EQUAL -> Some (StrictEqual, 5)
        | T_NOT_EQUAL -> Some (NotEqual, 5)
        | T_STRICT_NOT_EQUAL -> Some (StrictNotEqual, 5)
        | T_LESS_THAN -> Some (LessThan, 6)
        | T_LESS_THAN_EQUAL -> Some (LessThanEqual, 6)
        | T_GREATER_THAN -> Some (GreaterThan, 6)
        | T_GREATER_THAN_EQUAL -> Some (GreaterThanEqual, 6)
        | T_IN ->
            if (no_in env) then None else Some (In, 6)
        | T_INSTANCEOF -> Some (Instanceof, 6)
        | T_LSHIFT -> Some (LShift, 7)
        | T_RSHIFT -> Some (RShift, 7)
        | T_RSHIFT3 -> Some (RShift3, 7)
        | T_PLUS -> Some (Plus, 8)
        | T_MINUS -> Some (Minus, 8)
        | T_MULT -> Some (Mult, 9)
        | T_DIV -> Some (Div, 9)
        | T_MOD -> Some (Mod, 9)
        (* Highest priority *)
        | _ -> None)
        in if ret <> None then Eat.token env;
        ret

      in let make_binary left right operator =
        Loc.btwn (fst left) (fst right), Expression.(Binary Binary.({
          operator;
          left;
          right;
        }))

      in let rec add_to_stack right (rop, rpri) = function
        | (left, (lop, lpri))::rest when lpri >= rpri->
            add_to_stack (make_binary left right lop) (rop, rpri) rest
        | stack -> (right, (rop, rpri))::stack

      in let rec collapse_stack right = function
        | [] -> right
        | (left, (lop, _))::rest ->
            collapse_stack (make_binary left right lop) rest

      in let rec helper env stack =
        let right = unary (env |> with_no_in false) in
        if Peek.token env = T_LESS_THAN
        then begin
          match right with
          | _, Expression.JSXElement _ ->
              error env Error.AdjacentJSXElements
          | _ -> ()
        end;
        match binary_op env with
        | None -> (match stack with
          | [] -> right
          | _ -> collapse_stack right stack)
        | Some (rop, rpri) ->
            helper env (add_to_stack right (rop, rpri) stack)

      in fun env -> helper env []

    and unary env =
      let begin_loc = Peek.loc env in
      let op = Expression.Unary.(match Peek.token env with
      | T_NOT -> Some Not
      | T_BIT_NOT -> Some BitNot
      | T_PLUS -> Some Plus
      | T_MINUS -> Some Minus
      | T_TYPEOF -> Some Typeof
      | T_VOID -> Some Void
      | T_DELETE -> Some Delete
      (* If we are in a unary expression context, and within an async function,
       * assume that a use of "await" is intended as a keyword, not an ordinary
       * identifier. This is a little bit inconsistent, since it can be used as
       * an identifier in other contexts (such as a variable name), but it's how
       * Babel does it. *)
      | T_AWAIT when allow_await env -> Some Await
      | _ -> None) in
      match op with
      | None -> begin
          let op = Expression.Update.(match Peek.token env with
          | T_INCR -> Some Increment
          | T_DECR -> Some Decrement
          | _ -> None) in
          match op with
          | None -> postfix env
          | Some operator ->
              Eat.token env;
              let argument = unary env in
              if not (is_lhs argument)
              then error_at env (fst argument, Error.InvalidLHSInAssignment);
              (match argument with
              | _, Expression.Identifier (_, { Identifier.name; _ })
                when is_restricted name ->
                  strict_error env Error.StrictLHSPrefix
              | _ -> ());
              Loc.btwn begin_loc (fst argument), Expression.(Update Update.({
                operator;
                prefix = true;
                argument;
              }))
        end
      | Some operator ->
        Eat.token env;
        let argument = unary env in
        let loc = Loc.btwn begin_loc (fst argument) in
        Expression.(match operator, argument with
        | Unary.Delete, (_, Identifier _) ->
            strict_error_at env (loc, Error.StrictDelete)
        | _ -> ());
        loc, Expression.(Unary Unary.({
          operator;
          prefix = true;
          argument;
        }))

    and postfix env =
      let argument = left_hand_side env in
      (* No line terminator allowed before operator *)
      if Peek.line_terminator env
      then argument
      else let op = Expression.Update.(match Peek.token env with
      | T_INCR -> Some Increment
      | T_DECR -> Some Decrement
      | _ -> None) in
      match op with
      | None -> argument
      | Some operator ->
          if not (is_lhs argument)
          then error_at env (fst argument, Error.InvalidLHSInAssignment);
          (match argument with
          | _, Expression.Identifier (_, { Identifier.name; _ })
            when is_restricted name ->
              strict_error env Error.StrictLHSPostfix
          | _ -> ());
          let end_loc = Peek.loc env in
          Eat.token env;
          Loc.btwn (fst argument) end_loc, Expression.(Update Update.({
            operator;
            prefix = false;
            argument;
          }))

    and left_hand_side env =
      let expr = match Peek.token env with
      | T_NEW -> _new env (fun new_expr _args -> new_expr)
      | _ when Peek._function env -> _function env
      | _ -> primary env in
      let expr = member env expr in
      match Peek.token env with
      | T_LPAREN -> call env expr
      | T_TEMPLATE_PART part ->
          member env (tagged_template env expr part)
      | _ -> expr

    and call env left =
      match Peek.token env with
      | T_LPAREN when not (no_call env) ->
          let args_loc, arguments = arguments env in
          call env (Loc.btwn (fst left) args_loc, Expression.(Call Call.({
            callee = left;
            arguments;
          })))
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression env in
          let last_loc = Peek.loc env in
          let loc = Loc.btwn (fst left) last_loc in
          Expect.token env T_RBRACKET;
          call env (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          call env (Loc.btwn (fst left) (fst id), Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | T_TEMPLATE_PART part -> tagged_template env left part
      | _ -> left

    and _new env finish_fn =
      match Peek.token env with
      | T_NEW ->
          let start_loc = Peek.loc env in
          Expect.token env T_NEW;
          let finish_fn' callee args =
            let end_loc, arguments = match args with
            | Some (loc, args) -> loc, args
            | _ -> fst callee, [] in
            let callee' = Loc.btwn start_loc end_loc, Expression.(New New.({
              callee;
              arguments;
            })) in
            finish_fn callee' None in
          _new env finish_fn'
      | _ ->
          let expr = match Peek.token env with
          | _ when Peek._function env -> _function env
          | _ -> primary env in
          let callee = member (env |> with_no_call true) expr in
          (* You can do something like
           *   new raw`42`
           *)
          let callee = match Peek.token env with
          | T_TEMPLATE_PART part -> tagged_template env callee part
          | _ -> callee in
          let args = match Peek.token env with
          | T_LPAREN -> Some (arguments env)
          | _ -> None in
          finish_fn callee args

    and arguments =
      let argument env =
        match Peek.token env with
        | T_ELLIPSIS ->
            let start_loc = Peek.loc env in
            Expect.token env T_ELLIPSIS;
            let argument = assignment env in
            let loc = Loc.btwn start_loc (fst argument) in
            Expression.(Spread (loc, SpreadElement.({
              argument;
            })))
        | _ -> Expression.Expression (assignment env)

      in let rec arguments' env acc =
        match Peek.token env with
        | T_EOF
        | T_RPAREN -> List.rev acc
        | _ ->
            let acc = (argument env)::acc in
            if Peek.token env <> T_RPAREN
            then Expect.token env T_COMMA;
            arguments' env acc

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LPAREN;

        let args = arguments' env []

        in let end_loc = Peek.loc env in
        Expect.token env T_RPAREN;
        Loc.btwn start_loc end_loc, args

    and member env left =
      match Peek.token env with
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression (env |> with_no_call false) in
          let last_loc = Peek.loc env in
          Expect.token env T_RBRACKET;
          call env (Loc.btwn (fst left) last_loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          call env (Loc.btwn (fst left) (fst id), Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | _ -> left

    and _function env =
      let start_loc = Peek.loc env in
      let async = Declaration.async env in
      Expect.token env T_FUNCTION;
      let generator = Declaration.generator env async in
      let id, typeParameters =
        if Peek.token env = T_LPAREN
        then None, None
        else begin
          let id = match Peek.token env with
            | T_LESS_THAN -> None
            | _ -> Some (Parse.identifier ~restricted_error:Error.StrictFunctionName env) in
          id, Type.type_parameter_declaration env
        end in
      let params, defaults, rest = Declaration.function_params env in
      let returnType = Type.annotation_opt env in
      let end_loc, body, strict =
        Declaration.function_body env ~async ~generator in
      let simple = Declaration.is_simple_function_params params defaults rest in
      Declaration.strict_post_check env ~strict ~simple id params;
      let expression = Ast.Statement.FunctionDeclaration.(
        match body with
        | BodyBlock _ -> false
        | BodyExpression _ -> true) in
      Loc.btwn start_loc end_loc, Expression.(Function Function.({
        id;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        expression;
        returnType;
        typeParameters;
      }))

    and number env number_type =
      let value = Peek.value env in
      let value = match number_type with
      | LEGACY_OCTAL ->
        strict_error env Error.StrictOctalLiteral;
        float (int_of_string ("0o"^value))
      | BINARY
      | OCTAL ->
        float (int_of_string value)
      | NORMAL ->
        Sys_utils.float_of_string value
      in
      Expect.token env (T_NUMBER number_type);
      value

    and primary env =
      let loc = Peek.loc env in
      match Peek.token env with
      | T_THIS ->
          Expect.token env T_THIS;
          loc, Expression.This
      | T_NUMBER number_type ->
          let raw = Peek.value env in
          let value = Literal.Number (number env number_type) in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, Expression.(Literal { Literal.value; raw; })
      | (T_TRUE | T_FALSE) as token ->
          let raw = Peek.value env in
          Expect.token env token;
          let value = (Literal.Boolean (token = T_TRUE)) in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_NULL ->
          let raw = Peek.value env in
          Expect.token env T_NULL;
          let value = Literal.Null in
          loc, Expression.(Literal { Literal.value; raw; })
      | T_LPAREN -> group env
      | T_LCURLY -> object_initializer env
      | T_LBRACKET ->
          let loc, arr = array_initializer env in
          loc, Expression.Array arr
      | T_DIV -> regexp env ""
      | T_DIV_ASSIGN -> regexp env "="
      | T_LESS_THAN ->
          let loc, element = Parse.jsx_element env in
          loc, Expression.JSXElement element
      | T_TEMPLATE_PART part ->
          let loc, template = template_literal env part in
          loc, Expression.(TemplateLiteral template)
      | T_CLASS -> Parse.class_expression env
      | T_SUPER ->
          let loc = Peek.loc env in
          Expect.token env T_SUPER;
          let id = loc, {
            Identifier.name = "super";
            typeAnnotation = None;
            optional = false; } in
          loc, Expression.Identifier id
      | _ when Peek.identifier env ->
          let id = Parse.identifier env in
          fst id, Expression.Identifier id
      | t ->
          error_unexpected env;
          (* Let's get rid of the bad token *)
          if t = T_ERROR
          then Eat.token env;
          (* Really no idea how to recover from this. I suppose a null
           * expression is as good as anything *)
          let value = Literal.Null in
          let raw = "null" in
          loc, Expression.(Literal { Literal.value; raw; })

    and object_initializer env =
      let loc, obj = Parse.object_initializer env in
      loc, Expression.Object obj

    and template_literal =
      let rec template_parts env quasis expressions =
        let expr = Parse.expression env in
        let expressions = expr::expressions in
        match Peek.token env with
        | T_RCURLY ->
            let lex_env, lex_result = lex_template_part (lex_env env) in
            set_lex_env env lex_env;
            Eat.advance env (lex_env, lex_result) NORMAL_LEX;
            let loc, part = match lex_result.lex_token with
            | T_TEMPLATE_PART ((loc, part), _) -> loc, part
            | _ -> assert false in
            let quasis = (loc, part)::quasis in
            if part.Expression.TemplateLiteral.Element.tail
            then loc, List.rev quasis, List.rev expressions
            else template_parts env quasis expressions
        | _ ->
            (* Malformed template *)
            error_unexpected env;
            let imaginary_quasi = fst expr, Expression.TemplateLiteral.Element.({
              value = {
                raw = "";
                cooked = "";
              };
              tail = true;
            }) in
            fst expr, List.rev (imaginary_quasi::quasis), List.rev expressions

      in fun env ((head, _) as part) ->
        Expect.token env (T_TEMPLATE_PART part);
        let start_loc = fst head in
        let end_loc, quasis, expressions =
          if (snd head).Expression.TemplateLiteral.Element.tail
          then start_loc, [head], []
          else template_parts env [head] [] in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.TemplateLiteral.({
          quasis;
          expressions;
        })

    and tagged_template env tag part =
      let quasi = template_literal env part in
      Loc.btwn (fst tag) (fst quasi), Expression.(TaggedTemplate TaggedTemplate.({
        tag;
        quasi;
      }))

    and group env =
      Expect.token env T_LPAREN;
      let expression = assignment env in
      let ret = (match Peek.token env with
      | T_COMMA -> sequence env [expression]
      | T_COLON ->
          let typeAnnotation = Type.annotation env in
          Expression.(Loc.btwn (fst expression) (fst typeAnnotation),
            TypeCast TypeCast.({
              expression;
              typeAnnotation;
            }))
      | _ -> expression) in
      Expect.token env T_RPAREN;
      ret

    and array_initializer =
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
            let argument = assignment env in
            let loc = Loc.btwn start_loc (fst argument) in
            let elem = Expression.(Spread (loc, SpreadElement.({
              argument;
            }))) in
            elements env ((Some elem)::acc)
        | _ ->
            let elem = Expression.Expression (assignment env) in
            if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
            elements env ((Some elem)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let elements = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Expression.Array.({
          elements;
        })

    and regexp env prefix =
      let lex_env, lex_result = lex_regexp (lex_env env) prefix in
      set_lex_env env lex_env;
      Eat.advance env (lex_env, lex_result) NORMAL_LEX;
      let pattern, raw_flags = match lex_result.lex_token with
        | T_REGEXP (_, pattern, flags) -> pattern, flags
        | _ -> assert false in
      let filtered_flags = Buffer.create (String.length raw_flags) in
      String.iter (function
        | 'g' | 'i' | 'm' | 'y' as c -> Buffer.add_char filtered_flags c
        | _ -> ()) raw_flags;
      let flags = Buffer.contents filtered_flags in
      if flags <> raw_flags
      then error env (Error.InvalidRegExpFlags raw_flags);
      let value = Literal.(RegExp { RegExp.pattern; flags; }) in
      let raw = lex_result.lex_value in
      lex_result.lex_loc, Expression.(Literal { Literal.value; raw; })

    and try_arrow_function =
      (* Certain errors (almost all errors) cause a rollback *)
      let error_callback _ = Error.(function
        (* Don't rollback on these errors. *)
        | StrictParamName
        | ParameterAfterRestParameter
        | NewlineBeforeArrow -> ()
        (* Everything else causes a rollback *)
        | _ -> raise Try.Rollback) in

      fun env ->
        let env = env |> with_error_callback error_callback in

        let start_loc = Peek.loc env in
        (* a T_ASYNC could either be a parameter name or it could be indicating
         * that it's an async function *)
        let async = Peek.token ~i:1 env <> T_ARROW && Declaration.async env in
        let typeParameters = Type.type_parameter_declaration env in
        let params, defaults, rest, returnType =
          (* Disallow all fancy features for identifier => body *)
          if Peek.identifier env && typeParameters = None
          then
            let id =
              Parse.identifier ~restricted_error:Error.StrictParamName env in
            let param = fst id, Pattern.Identifier id in
            [param], [], None, None
          else
            let params, defaults, rest = Declaration.function_params env in
            params, defaults, rest, Type.annotation_opt env in

        (* It's hard to tell if an invalid expression was intended to be an
         * arrow function before we see the =>. If there are no params, that
         * implies "()" which is only ever found in arrow params. Similarly,
         * rest params indicate arrow functions. Therefore, if we see a rest
         * param or an empty param list then we can disable the rollback and
         * instead generate errors as if we were parsing an arrow function *)
        let env =
          if params = [] || rest <> None
          then without_error_callback env
          else env in

        if Peek.line_terminator env && Peek.token env = T_ARROW
        then error env Error.NewlineBeforeArrow;
        Expect.token env T_ARROW;

        (* Now we know for sure this is an arrow function *)
        let env = without_error_callback env in

        let body, strict =
          Declaration.concise_function_body env ~async ~generator:false in
        let simple =
          Declaration.is_simple_function_params params defaults rest in
        Declaration.strict_post_check env ~strict ~simple None params;
        let end_loc, expression = Ast.Statement.FunctionDeclaration.(
          match body with
          | BodyBlock (loc, _) -> loc, false
          | BodyExpression (loc, _) -> loc, true) in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.(ArrowFunction ArrowFunction.({
          id = None;
          params;
          defaults;
          rest;
          body;
          async;
          generator = false; (* arrow functions cannot be generators *)
          expression;
          returnType;
          typeParameters;
        }))

    and sequence env acc =
      match Peek.token env with
      | T_COMMA ->
          Expect.token env T_COMMA;
          let expr = assignment env in
          sequence env (expr::acc)
      | _ ->
        let last_loc = (match acc with
          | (loc, _)::_ -> loc
          | _ -> Loc.none) in
        let expressions = List.rev acc in
        let first_loc = (match expressions with
          | (loc, _)::_ -> loc
          | _ -> Loc.none) in
        Loc.btwn first_loc last_loc, Expression.(Sequence Sequence.({
          expressions;
        }))

    (* You can do things like
     * var x = { if : 4 }
     * x.if
     *)
    and identifier_or_reserved_keyword env =
      let { lex_token; lex_value; lex_loc; _ } = lookahead env in
      match lex_token with
      | T_IDENTIFIER -> Parse.identifier env, None
      | _ ->
        let err = match lex_token with
        | T_FUNCTION
        | T_IF
        | T_IN
        | T_INSTANCEOF
        | T_RETURN
        | T_SWITCH
        | T_THIS
        | T_THROW
        | T_TRY
        | T_VAR
        | T_WHILE
        | T_WITH
        | T_CONST
        | T_LET
        | T_NULL
        | T_FALSE
        | T_TRUE
        | T_BREAK
        | T_CASE
        | T_CATCH
        | T_CONTINUE
        | T_DECLARE
        | T_DEFAULT
        | T_DO
        | T_FINALLY
        | T_FOR
        | T_CLASS
        | T_EXTENDS
        | T_STATIC
        | T_ELSE
        | T_NEW
        | T_DELETE
        | T_TYPEOF
        | T_VOID
        | T_ENUM
        | T_EXPORT
        | T_IMPORT
        | T_SUPER
        | T_IMPLEMENTS
        | T_INTERFACE
        | T_PACKAGE
        | T_PRIVATE
        | T_PROTECTED
        | T_PUBLIC
        | T_YIELD
        | T_TYPE
        | T_OF
        | T_ANY_TYPE
        | T_BOOLEAN_TYPE
        | T_NUMBER_TYPE
        | T_STRING_TYPE
        | T_VOID_TYPE
        | T_ASYNC
        | T_AWAIT
        | T_DEBUGGER ->
            Some (lex_loc, get_unexpected_error (lex_token, lex_value))
        | _ ->
            error_unexpected env;
            None
        in
        Eat.token env;
        (lex_loc, Identifier.({
          name = lex_value;
          typeAnnotation = None;
          optional = false;
        })), err
  end

  (* A module for parsing various object related things, like object literals
   * and classes *)
  module Object : sig
    val key : env -> Loc.t * Ast.Expression.Object.Property.key
    val _initializer : env -> Loc.t * Ast.Expression.Object.t
    val class_declaration : env -> Ast.Statement.t
    val class_expression : env -> Ast.Expression.t
  end = struct
    let rec decorator_list env decorators =
      match Peek.token env with
      | T_AT ->
          Eat.token env;
          let decorators = decorators @ [Expression.left_hand_side env] in
          decorator_list env decorators
      | _ ->
          decorators

    let key ?allow_computed_key:(allow_computed_key=true) env =
      Ast.Expression.Object.Property.(match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, Literal (loc, { Literal.value; raw; })
      | T_NUMBER number_type ->
          let raw = Peek.value env in
          let loc = Peek.loc env in
          let value = Expression.number env number_type in
          let value = Literal.Number value in
          loc,  Literal (loc, { Literal.value; raw; })
      | T_LBRACKET when allow_computed_key ->
          let start_loc = Peek.loc env in
          Expect.token env T_LBRACKET;
          let expr = Parse.assignment (env |> with_no_in false) in
          let end_loc = Peek.loc env in
          Expect.token env T_RBRACKET;
          Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.Computed expr
      | _ ->
          let id, _ = Expression.identifier_or_reserved_keyword env in
          fst id, Identifier id)

    let _method env kind =
      (* this is a getter or setter, it cannot be async *)
      let async = false in
      let generator = Declaration.generator env async in
      let _, key = key env in
      (* It's not clear how type params on getters & setters would make sense
       * in Flow's type system. Since this is a Flow syntax extension, we might
       * as well disallow it until we need it *)
      let typeParameters = Ast.Expression.Object.Property.(match kind with
      | Get | Set -> None
      | _ -> Type.type_parameter_declaration env) in
      Expect.token env T_LPAREN;
      let params = Ast.Expression.Object.Property.(match kind with
      | Get -> []
      | Set ->
        (* TODO: support more param pattern types here *)
        let param = Parse.identifier_with_type env Error.StrictParamName in
        [ (fst param, Pattern.Identifier param) ]
      | Init -> assert false) in
      Expect.token env T_RPAREN;
      let returnType = Type.annotation_opt env in
      let _, body, strict = Declaration.function_body env ~async ~generator in
      let defaults = [] in
      let rest = None in
      let simple = Declaration.is_simple_function_params params defaults rest in
      Declaration.strict_post_check env ~strict ~simple None params;
      let end_loc, expression = Ast.Statement.FunctionDeclaration.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      let value = end_loc, Ast.Expression.Function.({
        id = None;
        params;
        defaults;
        rest;
        body;
        generator;
        async;
        expression;
        returnType;
        typeParameters;
      }) in
      key, value

    let _initializer =
      let rec property env = Ast.Expression.Object.(
        let start_loc = Peek.loc env in
        if Peek.token env = T_ELLIPSIS
        then begin
          (* Spread property *)
          Expect.token env T_ELLIPSIS;
          let argument = Parse.assignment env in
          SpreadProperty (Loc.btwn start_loc (fst argument), SpreadProperty.({
            argument;
          }))
        end else begin
          (* look for a following identifier to tell whether to parse a function
           * or not *)
          let async = Peek.identifier ~i:1 env && Declaration.async env in
          Property (match async , Declaration.generator env async, key env with
          | false, false, (_, (Property.Identifier (_, { Ast.Identifier.name =
              "get"; _}) as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN -> init env start_loc key false false
              | _ -> get env start_loc)
          | false, false, (_, (Property.Identifier (_, { Ast.Identifier.name =
              "set"; _}) as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN -> init env start_loc key false false
              | _ -> set env start_loc)
          | async, generator, (_, key) ->
              init env start_loc key async generator
          )
        end
      )

      and get env start_loc =
        let key, (end_loc, fn) =
          _method env Ast.Expression.Object.Property.Get in
        let value = end_loc, Ast.Expression.Function fn in
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
          key;
          value;
          kind = Get;
          _method = false;
          shorthand = false;
        })

      and set env start_loc =
        let key, (end_loc, fn) =
          _method env Ast.Expression.Object.Property.Set in
        let value = end_loc, Ast.Expression.Function fn in
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
          key;
          value;
          kind = Set;
          _method = false;
          shorthand = false;
        })

      and init env start_loc key async generator =
        Ast.Expression.Object.Property.(
          let value, shorthand, _method =
            match Peek.token env with
            | T_RCURLY
            | T_COMMA ->
                (match key with
                | Literal lit -> fst lit, Ast.Expression.Literal (snd lit)
                | Identifier id -> fst id, Ast.Expression.Identifier id
                | Computed expr -> expr), true, false
            | T_LESS_THAN
            | T_LPAREN ->
                let typeParameters = Type.type_parameter_declaration env in
                let params, defaults, rest = Declaration.function_params env in
                let returnType = Type.annotation_opt env in
                let _, body, strict =
                  Declaration.function_body env ~async ~generator in
                let simple = Declaration.is_simple_function_params params defaults rest in
                Declaration.strict_post_check env ~strict ~simple None params;
                let end_loc, expression = Ast.Statement.FunctionDeclaration.(
                  match body with
                  | BodyBlock (loc, _) -> loc, false
                  | BodyExpression (loc, _) -> loc, true) in
                let value = end_loc, Ast.Expression.(Function Function.({
                  id = None;
                  params;
                  defaults;
                  rest;
                  body;
                  generator;
                  async;
                  expression;
                  returnType;
                  typeParameters;
                })) in
                value, false, true
            | _ ->
              Expect.token env T_COLON;
              Parse.assignment env, false, false in
          Loc.btwn start_loc (fst value), {
            key;
            value;
            kind = Init;
            _method;
            shorthand;
          }
        )

      and check_property env prop_map prop = Ast.Expression.Object.(
        match prop with
        | Property (prop_loc, ({ Property.key = Property.Literal _ | Property.Identifier _; _ } as prop)) ->
            Property.(
              let key = match prop.key with
              | Literal (_, { Literal.value = Literal.String s; _; } ) -> s
              | Literal (_, { Literal.value = Literal.Boolean b; _; } ) -> string_of_bool b
              | Literal (_, { Literal.value = Literal.Null; _; } ) -> "null"
              | Literal (_, { Literal.value = Literal.Number f; _; } ) -> string_of_float f
              | Literal (_, { Literal.value = Literal.RegExp _; _; } ) ->
                  failwith "RegExp cannot be property key"
              | Identifier (_, { Identifier.name; _ }) -> name
              | Computed _ -> assert false in
              let prev_kinds =
                try SMap.find key prop_map
                with Not_found -> SSet.empty in
              let kind_string = match prop.kind with
              | Init -> "Init"
              | Get -> "Get"
              | Set -> "Set" in
              (match kind_string with
              | "Init" when SSet.mem "Init" prev_kinds ->
                  strict_error_at env (prop_loc, Error.StrictDuplicateProperty)
              | "Init" when SSet.mem "Set" prev_kinds || SSet.mem "Get" prev_kinds ->
                  error_at env (prop_loc, Error.AccessorDataProperty)
              | "Get"
              | "Set" when SSet.mem "Init" prev_kinds ->
                  error_at env (prop_loc, Error.AccessorDataProperty)
              | ("Get" | "Set") as kind when SSet.mem kind prev_kinds ->
                  error_at env (prop_loc, Error.AccessorGetSet)
              | _ -> ());
              let kinds = SSet.add kind_string prev_kinds in
              SMap.add key kinds prop_map)
        | _ -> prop_map
      )

      and properties env (prop_map, acc) =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let prop = property env in
            let prop_map = check_property env prop_map prop in
            if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
            properties env (prop_map, prop::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let props = properties env (SMap.empty, []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Ast.Expression.Object.({
          properties = props;
        })

    let rec _class env =
      let superClass, superTypeParameters =
        if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          let superClass =
            Expression.left_hand_side (env |> with_allow_yield false) in
          let superTypeParameters = Type.type_parameter_instantiation env in
          Some superClass, superTypeParameters
        end else None, None in
      let implements =
        if Peek.token env = T_IMPLEMENTS
        then begin
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeInterface;
          Expect.token env T_IMPLEMENTS;
          class_implements env []
        end else [] in
      let body = class_body env in
      body, superClass, superTypeParameters, implements

    and class_implements env acc =
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_instantiation env in
      let loc = match typeParameters with
      | None -> fst id
      | Some (loc, _) -> Loc.btwn (fst id) loc in
      let implement = loc, Ast.Class.Implements.({
        id;
        typeParameters;
      }) in
      let acc = implement::acc in
      match Peek.token env with
      | T_COMMA ->
          Expect.token env T_COMMA;
          class_implements env acc
      | _ -> List.rev acc

    and class_body =
      let rec elements env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | T_SEMICOLON ->
            (* Skip empty elements *)
            Expect.token env T_SEMICOLON;
            elements env acc
        | _ -> elements env ((class_element env)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let body = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Ast.Class.Body.({
          body;
        })

    (* In the ES6 draft, all elements are methods. No properties (though there
     * are getter and setters allowed *)
    and class_element =
      let get env start_loc decorators static =
        let key, (end_loc, _ as value) =
          _method env Ast.Expression.Object.Property.Get in
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind = Get;
          static;
          decorators;
        })))

      in let set env start_loc decorators static =
        let key, (end_loc, _ as value) =
          _method env Ast.Expression.Object.Property.Set in
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind = Set;
          static;
          decorators;
        })))

      in let init env start_loc decorators key async generator static =
        match Peek.token env with
        | T_COLON
        | T_ASSIGN
        | T_SEMICOLON when not async && not generator ->
          (* Class property with annotation *)
          let typeAnnotation = Type.annotation_opt env in
          let options = parse_options env in
          let value =
            if Expect.maybe env T_ASSIGN then (
              if static && options.esproposal_class_static_fields
                 || (not static) && options.esproposal_class_instance_fields
              then Some (Parse.expression env)
              else None
            ) else None in
          let end_loc = Peek.loc env in
          Expect.token env T_SEMICOLON;
          let loc = Loc.btwn start_loc end_loc in
          Ast.Class.(Body.Property (loc, Property.({
            key;
            value;
            typeAnnotation;
            static;
          })))
        | _ ->
          let typeParameters = Type.type_parameter_declaration env in
          let params, defaults, rest = Declaration.function_params env in
          let returnType = Type.annotation_opt env in
          let _, body, strict =
            Declaration.function_body env ~async ~generator in
          let simple =
            Declaration.is_simple_function_params params defaults rest in
          Declaration.strict_post_check env ~strict ~simple None params;
          let end_loc, expression = Ast.Statement.FunctionDeclaration.(
            match body with
            | BodyBlock (loc, _) -> loc, false
            | BodyExpression (loc, _) -> loc, true) in
          let value = end_loc, Ast.Expression.Function.({
            id = None;
            params;
            defaults;
            rest;
            body;
            generator;
            async;
            expression;
            returnType;
            typeParameters;
          }) in
          let kind = Ast.(match key with
            | Expression.Object.Property.Identifier (_, {
                Identifier.name = "constructor";
                _;
              })
            | Expression.Object.Property.Literal (_, {
                Literal.value = Literal.String "constructor";
                _;
              }) ->
              Class.Method.Constructor
            | _ ->
              Class.Method.Method) in
          Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
            key;
            value;
            kind;
            static;
            decorators
          })))

      in fun env -> Ast.Expression.Object.Property.(
        let start_loc = Peek.loc env in
        let decorators =
          if (parse_options env).esproposal_decorators then
            decorator_list env []
          else
            []
        in
        let static = Expect.maybe env T_STATIC in
        let async = Peek.token ~i:1 env <> T_LPAREN && Declaration.async env in
        let generator = Declaration.generator env async in
        match (async, generator, key env) with
        | false, false,
          (_, (Identifier (_, { Identifier.name = "get"; _ }) as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN -> init env start_loc decorators key async generator static
            | _ -> get env start_loc decorators static )
        | false, false,
          (_, (Identifier (_, { Identifier.name = "set"; _ }) as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN -> init env start_loc decorators key async generator static
            | _ -> set env start_loc decorators static)
        | _, _, (_, key) ->
            init env start_loc decorators key async generator static
      )

    let class_declaration env =
      (* 10.2.1 says all parts of a class definition are strict *)
      let env = env |> with_strict true in
      let start_loc = Peek.loc env in
      Expect.token env T_CLASS;
      let tmp_env = env |> with_no_let true in
      let id = (
        match in_export env, Peek.identifier tmp_env with
        | true, false -> None
        | _ -> Some(Parse.identifier tmp_env)
      ) in
      let typeParameters = Type.type_parameter_declaration env in
      let body, superClass, superTypeParameters, implements = _class env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Ast.Statement.(ClassDeclaration Class.({
        id;
        body;
        superClass;
        typeParameters;
        superTypeParameters;
        implements;
      }))

    let class_expression env =
      let start_loc = Peek.loc env in
      Expect.token env T_CLASS;
      let id, typeParameters = match Peek.token env with
        | T_EXTENDS
        | T_LESS_THAN
        | T_LCURLY -> None, None
        | _ ->
            let id = Some (Parse.identifier env) in
            let typeParameters = Type.type_parameter_declaration env in
            id, typeParameters in
      let body, superClass, superTypeParameters, implements = _class env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Ast.Expression.(Class Class.({
        id;
        body;
        superClass;
        typeParameters;
        superTypeParameters;
        implements;
      }))

    let key = key ~allow_computed_key:false
  end

  module Statement: sig
    val _for: env -> Ast.Statement.t
    val _if: env -> Ast.Statement.t
    val _let: env -> Ast.Statement.t
    val _try: env -> Ast.Statement.t
    val _while: env -> Ast.Statement.t
    val _with: env -> Ast.Statement.t
    val block: env -> Ast.Statement.t
    val break: env -> Ast.Statement.t
    val continue: env -> Ast.Statement.t
    val debugger: env -> Ast.Statement.t
    val declare: ?in_module:bool -> env -> Ast.Statement.t
    val declare_export_declaration: env -> Ast.Statement.t
    val do_while: env -> Ast.Statement.t
    val empty: env -> Ast.Statement.t
    val export_declaration: env -> Ast.Statement.t
    val expression: env -> Ast.Statement.t
    val import_declaration: env -> Ast.Statement.t
    val interface: env -> Ast.Statement.t
    val maybe_labeled: env -> Ast.Statement.t
    val return: env -> Ast.Statement.t
    val switch: env -> Ast.Statement.t
    val throw: env -> Ast.Statement.t
    val type_alias: env -> Ast.Statement.t
    val var_or_const: env -> Ast.Statement.t
  end = struct
    let rec empty env =
      let loc = Peek.loc env in
      Expect.token env T_SEMICOLON;
      loc, Statement.Empty

    and break env =
      let start_loc = Peek.loc env in
      Expect.token env T_BREAK;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let label = Parse.identifier env in
          let name = (snd label).Identifier.name in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end
      in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match label with
        | Some id -> fst id
        | None -> start_loc) in
      let loc = Loc.btwn start_loc end_loc in
      if label = None && not (in_loop env || in_switch env)
      then error_at env (loc, Error.IllegalBreak);
      Eat.semicolon env;
      loc, Statement.Break {
        Statement.Break.label = label;
      }

    and continue env =
      let start_loc = Peek.loc env in
      Expect.token env T_CONTINUE;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let (_, { Identifier.name; _ }) as label = Parse.identifier env in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match label with
        | Some id -> fst id
        | None -> start_loc) in
      let loc = Loc.btwn start_loc end_loc in
      if not (in_loop env)
      then error_at env (loc, Error.IllegalContinue);
      Eat.semicolon env;
      loc, Statement.Continue {
        Statement.Continue.label = label;
      }

    and debugger env =
      let start_loc = Peek.loc env in
      Expect.token env T_DEBUGGER;
      let end_loc = match Peek.semicolon_loc env with
      | None -> start_loc
      | Some loc -> loc in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.Debugger;

    and do_while env =
      let start_loc = Peek.loc env in
      Expect.token env T_DO;
      let body = Parse.statement (env |> with_in_loop true) in
      Expect.token env T_WHILE;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      let end_loc = Peek.loc env in
      Expect.token env T_RPAREN;
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
      | Some loc -> loc in
      (* The rules of automatic semicolon insertion in ES5 don't mention this,
       * but the semicolon after a do-while loop is optional. This is properly
       * specified in ES6 *)
      if Peek.token env = T_SEMICOLON
      then Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(DoWhile DoWhile.({
        body;
        test;
      }))

    and _for =
      let assert_can_be_forin_or_forof env err = Statement.VariableDeclaration.(function
        | Some (Statement.For.InitDeclaration (loc, {
          declarations;
          _;
        })) ->
            (* Only a single declarator is allowed, without an init. So
             * something like
             *
             * for (var x in y) {}
             *
             * is allowed, but we disallow
             *
             * for (var x, y in z) {}
             * for (var x = 42 in y) {}
             *)
            (match declarations with
            | [ (_, { Declarator.init = None; _; }) ] -> ()
            | _ -> error_at env (loc, err))
        | Some (Statement.For.InitExpression (loc, expr)) ->
            (* Only certain expressions can be the lhs of a for in or for of *)
            if not (Parse.is_assignable_lhs (loc, expr))
            then error_at env (loc, err)
        | _ -> error env err
      ) in

      fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_FOR;
        Expect.token env T_LPAREN;

        let init, errs =
          match Peek.token env with
          | T_SEMICOLON -> None, []
          | T_LET ->
              let decl, errs = Declaration._let (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | T_CONST ->
              let decl, errs = Declaration.const (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | T_VAR ->
              let decl, errs = Declaration.var (env |> with_no_in true) in
              Some (Statement.For.InitDeclaration decl), errs
          | _ ->
              let expr = Parse.expression (env |> with_no_in true |> with_no_let true) in
              Some (Statement.For.InitExpression expr), []
        in

        match Peek.token env with
        | T_IN ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForIn init;
            let left = Statement.(match init with
            | Some (For.InitDeclaration decl) -> ForIn.LeftDeclaration decl
            | Some (For.InitExpression expr) -> ForIn.LeftExpression expr
            | None -> assert false) in
            (* This is a for in loop *)
            Expect.token env T_IN;
            let right = Parse.expression env in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(ForIn ForIn.({
              left;
              right;
              body;
              each = false;
            }))
        | T_OF ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForOf init;
            let left = Statement.(match init with
            | Some (For.InitDeclaration decl) -> ForOf.LeftDeclaration decl
            | Some (For.InitExpression expr) -> ForOf.LeftExpression expr
            | None -> assert false) in
            (* This is a for of loop *)
            Expect.token env T_OF;
            let right = Parse.assignment env in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(ForOf ForOf.({
              left;
              right;
              body;
            }))
        | _ ->
            (* This is a for loop *)
            errs |> List.iter (error_at env);
            Expect.token env T_SEMICOLON;
            let test = match Peek.token env with
            | T_SEMICOLON -> None
            | _ -> Some (Parse.expression env) in
            Expect.token env T_SEMICOLON;
            let update = match Peek.token env with
            | T_RPAREN -> None
            | _ -> Some (Parse.expression env) in
            Expect.token env T_RPAREN;
            let body = Parse.statement (env |> with_in_loop true) in
            Loc.btwn start_loc (fst body), Statement.(For For.({
              init;
              test;
              update;
              body;
            }))

    and _if env =
      let start_loc = Peek.loc env in
      Expect.token env T_IF;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      Expect.token env T_RPAREN;
      let consequent = match Peek.token env with
      | _ when Peek._function env ->
          strict_error env Error.StrictFunctionStatement;
          Declaration._function env
      | _ -> Parse.statement env in
      let alternate = if Peek.token env = T_ELSE
      then begin
        Expect.token env T_ELSE;
        Some (Parse.statement env)
      end else None in
      let end_loc = match alternate with
      | Some stmt -> fst stmt
      | None -> fst consequent in
      Loc.btwn start_loc end_loc, Statement.(If If.({
        test;
        consequent;
        alternate;
      }))

    and return env =
      if not (in_function env)
      then error env Error.IllegalReturn;
      let start_loc = Peek.loc env in
      Expect.token env T_RETURN;
      let argument =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else Some (Parse.expression env) in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> (match argument with
        | Some argument -> fst argument
        | None -> start_loc) in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(Return Return.({
        argument;
      }))

    and switch =
      let rec case_list env (seen_default, acc) =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
          let start_loc = Peek.loc env in
          let test = match Peek.token env with
          | T_DEFAULT ->
              if seen_default
              then error env Error.MultipleDefaultsInSwitch;
              Expect.token env T_DEFAULT; None
          | _ ->
              Expect.token env T_CASE;
              Some (Parse.expression env) in
          let seen_default = seen_default || test = None in
          let end_loc = Peek.loc env in
          Expect.token env T_COLON;
          let term_fn = function
          | T_RCURLY | T_DEFAULT | T_CASE -> true
          | _ -> false in
          let consequent =
            Parse.statement_list ~term_fn (env |> with_in_switch true) in
          let end_loc = match List.rev consequent with
          | last_stmt::_ -> fst last_stmt
          | _ -> end_loc in
          let acc = (Loc.btwn start_loc end_loc, Statement.Switch.Case.({
            test;
            consequent;
          }))::acc in
          case_list env (seen_default, acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_SWITCH;
        Expect.token env T_LPAREN;
        let discriminant = Parse.expression env in
        Expect.token env T_RPAREN;
        Expect.token env T_LCURLY;
        let cases = case_list env (false, []) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        Loc.btwn start_loc end_loc, Statement.(Switch Switch.({
          discriminant;
          cases;
          lexical = false; (* TODO *)
        }))

    and throw env =
      let start_loc = Peek.loc env in
      Expect.token env T_THROW;
      if Peek.line_terminator env
      then error_at env (start_loc, Error.NewlineAfterThrow);
      let argument = Parse.expression env in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> fst argument in
      Eat.semicolon env;
      Loc.btwn start_loc end_loc, Statement.(Throw Throw.({
        argument;
      }))

    and _try env =
      let start_loc = Peek.loc env in
      Expect.token env T_TRY;
      let block = Parse.block_body env in
      let handler = match Peek.token env with
      | T_CATCH ->
          let start_loc = Peek.loc env in
          Expect.token env T_CATCH;
          Expect.token env T_LPAREN;
          let id = Parse.identifier ~restricted_error:Error.StrictCatchVariable env in
          let param = fst id, Pattern.Identifier id in
          Expect.token env T_RPAREN;
          let body = Parse.block_body env in
          let loc = Loc.btwn start_loc (fst body) in
          Some (loc, Ast.Statement.Try.CatchClause.({
            param;
            (* This SpiderMonkey specific feature is not on track to be in a
            * standard so I'm not supporting it *)
            guard = None;
            body;
          }))
      | _ -> None in
      (* SpiderMonkey feature, not supported in ES6 *)
      let guardedHandlers = [] in
      let finalizer = match Peek.token env with
      | T_FINALLY ->
          Expect.token env T_FINALLY;
          Some (Parse.block_body env)
      | _ -> None in
      let end_loc = match finalizer with
      | Some finalizer -> fst finalizer
      | None ->
          (match handler with
          | Some handler -> fst handler
          | None ->
              (* No catch or finally? That's an error! *)
              error_at env (fst block, Error.NoCatchOrFinally);
              fst block) in
      Loc.btwn start_loc end_loc, Statement.(Try Try.({
        block;
        handler;
        guardedHandlers;
        finalizer;
      }));

    and var_or_const env =
      let (start_loc, declaration), errs = Declaration.variable env in
      let end_loc = match Peek.semicolon_loc env with
      | None -> start_loc
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      errs |> List.iter (error_at env);
      Loc.btwn start_loc end_loc, declaration

    and _let env =
      let start_loc = Peek.loc env in
      Expect.token env T_LET;
      if Peek.token env = T_LPAREN
      then begin
        (* Let statement *)
        Expect.token env T_LPAREN;
        let end_loc, declarations, errs =
          Declaration.variable_declaration_list (env |> with_no_let true) in
        let head = List.map
          (fun (_, {Ast.Statement.VariableDeclaration.Declarator.id; init;}) ->
            Statement.Let.({ id; init; }))
          declarations in
        Expect.token env T_RPAREN;
        let body = Parse.statement env in
        let end_loc = match Peek.semicolon_loc env with
        | None -> end_loc
        | Some end_loc -> end_loc in
        Eat.semicolon env;
        errs |> List.iter (error_at env);
        Loc.btwn start_loc end_loc, Statement.(Let Let.({
          head;
          body;
        }))
      end else begin
        (* Let declaration *)
        let end_loc, declarations, errs =
          Declaration.variable_declaration_list (env |> with_no_let true) in
        let declaration =
          Ast.(Statement.VariableDeclaration Statement.VariableDeclaration.({
            declarations;
            kind = Let;
          })) in
        let end_loc = match Peek.semicolon_loc env with
        | None -> end_loc
        | Some end_loc -> end_loc in
        Eat.semicolon env;
        errs |> List.iter (error_at env);
        Loc.btwn start_loc end_loc, declaration
      end

    and _while env =
      let start_loc = Peek.loc env in
      Expect.token env T_WHILE;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      Expect.token env T_RPAREN;
      let body = Parse.statement (env |> with_in_loop true) in
      Loc.btwn start_loc (fst body), Statement.(While While.({
        test;
        body;
      }));

    and _with env =
      let start_loc = Peek.loc env in
      Expect.token env T_WITH;
      Expect.token env T_LPAREN;
      let _object = Parse.expression env in
      Expect.token env T_RPAREN;
      let body = Parse.statement env in
      let loc = Loc.btwn start_loc (fst body) in
      strict_error_at env (loc, Error.StrictModeWith);
      loc, Statement.(With With.({
        _object;
        body;
      }))

    and block env =
      let loc, block = Parse.block_body env in
      loc, Statement.Block block

    and maybe_labeled env =
      let expr = Parse.expression env in
      match (expr, Peek.token env) with
      | ((loc, Ast.Expression.Identifier label), T_COLON) ->
          let { Identifier.name; _ } = snd label in
          Expect.token env T_COLON;
          if SSet.mem name (labels env)
          then error_at env (loc, Error.Redeclaration ("Label", name));
          let env = add_label env name in
          let labeled_stmt = Parse.statement env in
          Loc.btwn loc (fst labeled_stmt), Statement.Labeled {
            Statement.Labeled.label = label;
            Statement.Labeled.body = labeled_stmt;
          }
      | expression, _ ->
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> (fst expression) in
          Eat.semicolon env;
          Loc.btwn (fst expression) end_loc, Statement.(Expression Expression.({
            expression;
          }))

    and expression env =
      let expression = Parse.expression env in
      let end_loc = match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> fst expression in
      Eat.semicolon env;
      Loc.btwn (fst expression) end_loc, Statement.(Expression Expression.({
        expression;
      }))

    and type_alias_helper env =
      let start_loc = Peek.loc env in
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAlias;
      Expect.token env T_TYPE;
      Eat.push_lex_mode env TYPE_LEX;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration env in
      Expect.token env T_ASSIGN;
      let right = Type._type env in
      let end_loc = match Peek.semicolon_loc env with
      | None -> fst right
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, Statement.TypeAlias.({
        id;
        typeParameters;
        right;
      })

    and type_alias env =
      if Peek.identifier ~i:1 env
      then
        let loc, type_alias = type_alias_helper env in
        loc, Statement.TypeAlias type_alias
      else
        Parse.statement env

    and interface =
      let rec supers env acc =
        let super = Type.generic env in
        let acc = super::acc in
        match Peek.token env with
        | T_COMMA ->
            Expect.token env T_COMMA;
            supers env acc
        | _ -> List.rev acc

      in fun env ->
        let start_loc = Peek.loc env in
        if Peek.identifier ~i:1 env
        then begin
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeInterface;
          Expect.token env T_INTERFACE;
          let id = Parse.identifier env in
          let typeParameters = Type.type_parameter_declaration env in
          let extends = if Peek.token env = T_EXTENDS
          then begin
            Expect.token env T_EXTENDS;
            supers env []
          end else [] in
          let body = Type._object ~allow_static:true env in
          let loc = Loc.btwn start_loc (fst body) in
          loc, Statement.(InterfaceDeclaration Interface.({
            id;
            typeParameters;
            body;
            extends;
            mixins = [];
          }))
        end else begin
          expression env
        end

    and declare_class =
      let rec supers env acc =
        let super = Type.generic env in
        let acc = super::acc in
        match Peek.token env with
        | T_COMMA ->
          Expect.token env T_COMMA;
          supers env acc
        | _ -> List.rev acc

      (* This is identical to `interface`, except that mixins are allowed *)
      in fun env start_loc ->
        let env = env |> with_strict true in
        Expect.token env T_CLASS;
        let id = Parse.identifier env in
        let typeParameters = Type.type_parameter_declaration env in
        let extends = if Peek.token env = T_EXTENDS
          then begin
            Expect.token env T_EXTENDS;
            supers env []
          end else [] in
        let mixins = if Peek.value env = "mixins"
          then begin
            Expect.contextual env "mixins";
            supers env []
          end else [] in
        let body = Type._object ~allow_static:true env in
        let loc = Loc.btwn start_loc (fst body) in
        loc, Statement.Interface.({
          id;
          typeParameters;
          body;
          extends;
          mixins;
        })

    and declare_class_statement env start_loc =
      let loc, fn = declare_class env start_loc in
      loc, Statement.DeclareClass fn

    and declare_function env start_loc =
      Expect.token env T_FUNCTION;
      let id = Parse.identifier env in
      let start_sig_loc = Peek.loc env in
      let typeParameters = Type.type_parameter_declaration env in
      let rest, params = Type.function_param_list env in
      Expect.token env T_COLON;
      let returnType = Type._type env in
      let end_loc = fst returnType in
      let loc = Loc.btwn start_sig_loc end_loc in
      let value = loc, Ast.Type.(Function {Function.
        params;
        returnType;
        rest;
        typeParameters;
      }) in
      let typeAnnotation = Some ((fst value), value) in
      let id =
        Loc.btwn (fst id) end_loc,
        Ast.Identifier.({(snd id) with typeAnnotation; })
      in
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      let loc = Loc.btwn start_loc end_loc in
      loc, Statement.DeclareFunction.({ id; })

    and declare_function_statement env start_loc =
      let loc, fn = declare_function env start_loc in
      loc, Statement.DeclareFunction fn

    and declare_var env start_loc =
      Expect.token env T_VAR;
      let id = Parse.identifier_with_type env Error.StrictVarName in
      let end_loc = match Peek.semicolon_loc env with
      | None -> fst id
      | Some loc -> loc in
      let loc = Loc.btwn start_loc end_loc in
      Eat.semicolon env;
      loc, Statement.DeclareVariable.({ id; })

    and declare_var_statement env start_loc =
      let loc, var = declare_var env start_loc in
      loc, Statement.DeclareVariable var

    and declare_module =
      let rec module_items env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ -> module_items env (declare ~in_module:true env::acc)

      in fun env start_loc ->
        Expect.contextual env "module";
        let id = match Peek.token env with
        | T_STRING (loc, value, raw, octal) ->
            if octal then strict_error env Error.StrictOctalLiteral;
            Expect.token env (T_STRING (loc, value, raw, octal));
            let value = Literal.String value in
            Statement.DeclareModule.Literal (loc, { Literal.value; raw; })
        | _ ->
            Statement.DeclareModule.Identifier (Parse.identifier env) in
        let body_start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let body = module_items env [] in
        Expect.token env T_RCURLY;
        let body_end_loc = Peek.loc env in
        let body_loc = Loc.btwn body_start_loc body_end_loc in
        let body = body_loc, { Statement.Block.body; } in
        Loc.btwn start_loc (fst body),
        Statement.(DeclareModule DeclareModule.({ id; body; }))


    and declare ?(in_module=false) env =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeDeclaration;
      let start_loc = Peek.loc env in
      (* eventually, just emit a wrapper AST node *)
      (match Peek.token ~i:1 env with
      | T_CLASS ->
          Expect.token env T_DECLARE;
          declare_class_statement env start_loc
      | T_INTERFACE ->
          Expect.token env T_DECLARE;
          interface env
      | T_TYPE ->
          Expect.token env T_DECLARE;
          type_alias env;
      | T_FUNCTION ->
          Expect.token env T_DECLARE;
          declare_function_statement env start_loc
      | T_VAR ->
          Expect.token env T_DECLARE;
          declare_var_statement env start_loc
      | T_ASYNC ->
          Expect.token env T_DECLARE;
          error env Error.DeclareAsync;
          Expect.token env T_ASYNC;
          declare_function_statement env start_loc
      | T_IDENTIFIER when not in_module && Peek.value ~i:1 env = "module" ->
          Expect.token env T_DECLARE;
          declare_module env start_loc
      | _ when in_module ->
          (* Oh boy, found some bad stuff in a declare module. Let's just
            * pretend it's a declare var (arbitrary choice) *)
          Expect.token env T_DECLARE;
          declare_var_statement env start_loc
      | _ ->
          Parse.statement env)

      let export_source env =
        Expect.contextual env "from";
        match Peek.token env with
        | T_STRING (loc, value, raw, octal) ->
            if octal then strict_error env Error.StrictOctalLiteral;
            Expect.token env (T_STRING (loc, value, raw, octal));
            let value = Literal.String value in
            loc, { Literal.value; raw; }
        | _ ->
            (* Just make up a string for the error case *)
            let raw = Peek.value env in
            let value = Literal.String raw in
            let ret = Peek.loc env, Literal.({ value; raw; }) in
            error_unexpected env;
            ret

      let rec export_specifiers_and_errs env specifiers errs =
        match Peek.token env with
        | T_EOF
        | T_RCURLY ->
            List.rev specifiers, List.rev errs
        | _ ->
            let id, err = Parse.identifier_or_reserved_keyword env in
            let name, err, end_loc = if Peek.value env = "as"
            then begin
              Expect.contextual env "as";
              let name, _ = Parse.identifier_or_reserved_keyword env in
              Some name, None, fst name
            end else None, err, fst id in
            let loc = Loc.btwn (fst id) end_loc in
            let specifier = loc, {
              Statement.ExportDeclaration.Specifier.id;
              name;
            } in
            if Peek.token env = T_COMMA
            then Expect.token env T_COMMA;
            let errs = match err with
            | Some err -> err::errs
            | None -> errs in
            export_specifiers_and_errs env (specifier::specifiers) errs

      let export_declaration env =
        let env = env |> with_strict true |> with_in_export true in
        let start_loc = Peek.loc env in
        Expect.token env T_EXPORT;
        Statement.ExportDeclaration.(match Peek.token env with
        | T_DEFAULT ->
            (* export default ... *)
            Expect.token env T_DEFAULT;
            let end_loc, declaration = match Peek.token env with
            | T_FUNCTION ->
                (* export default function foo (...) { ... } *)
                let fn = Declaration._function env in
                fst fn, Some (Declaration fn)
            | T_CLASS ->
                (* export default class foo { ... } *)
                let _class = Object.class_declaration env in
                fst _class, Some (Declaration _class)
            | _ ->
                (* export default [assignment expression]; *)
                let expr = Parse.assignment env in
                let end_loc = match Peek.semicolon_loc env with
                | Some loc -> loc
                | None -> fst expr in
                Eat.semicolon env;
                end_loc, Some (Expression expr)
              in
            Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
              default = true;
              declaration;
              specifiers = None;
              source = None;
              exportKind = ExportValue;
            }
        | T_TYPE when (Peek.token env ~i:1) <> T_LCURLY ->
            (* export type ... *)
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeExport;
            let type_alias = type_alias env in
            let end_loc = fst type_alias in
            Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
              default = false;
              declaration = Some (Declaration type_alias);
              specifiers = None;
              source = None;
              exportKind = ExportType;
            }
        | T_LET
        | T_CONST
        | T_VAR
        | T_CLASS
        (* not using Peek._function here because it would guard all of the
          * cases *)
        | T_ASYNC
        | T_FUNCTION ->
            let stmt = Parse.statement_list_item env in
            let declaration = Some (Declaration stmt) in
            Loc.btwn start_loc (fst stmt), Statement.ExportDeclaration {
              default = false;
              declaration;
              specifiers = None;
              source = None;
              exportKind = ExportValue;
            }
        | T_MULT ->
            let loc = Peek.loc env in
            let specifiers = Some (ExportBatchSpecifier loc) in
            Expect.token env T_MULT;
            let source = export_source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = Some source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
              default = false;
              declaration = None;
              specifiers;
              source;
              exportKind = ExportValue;
            }
        | _ ->
            let exportKind = (
              match Peek.token env with
              | T_TYPE -> Eat.token env; ExportType
              | _ -> ExportValue
            ) in
            Expect.token env T_LCURLY;
            let specifiers, errs = export_specifiers_and_errs env [] [] in
            let specifiers = Some (ExportSpecifiers specifiers) in
            let end_loc = Peek.loc env in
            Expect.token env T_RCURLY;
            let source = if Peek.value env = "from"
            then Some (export_source env)
            else begin
              errs |> List.iter (error_at env);
              None
            end in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None ->
                (match source with
                | Some source -> fst source
                | None -> end_loc) in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ExportDeclaration {
              default = false;
              declaration = None;
              specifiers;
              source;
              exportKind;
            }
        )

      and declare_export_declaration env =
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeDeclaration;
        let start_loc = Peek.loc env in
        Expect.token env T_DECLARE;

        let env = env |> with_strict true |> with_in_export true in
        Expect.token env T_EXPORT;
        Statement.DeclareExportDeclaration.(match Peek.token env with
        | T_DEFAULT ->
            (* declare export default ... *)
            Expect.token env T_DEFAULT;
            let end_loc, declaration = match Peek.token env with
            | T_FUNCTION ->
                (* declare export default function foo (...): ...  *)
                let fn = declare_function env start_loc in
                fst fn, Some (Function fn)
            | T_CLASS ->
                (* declare export default class foo { ... } *)
                let _class = declare_class env start_loc in
                fst _class, Some (Class _class)
            | _ ->
                (* declare export default [type]; *)
                let _type = Type._type env in
                let end_loc = match Peek.semicolon_loc env with
                | Some loc -> loc
                | None -> fst _type in
                Eat.semicolon env;
                end_loc, Some (DefaultType _type)
              in
            Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
              default = true;
              declaration;
              specifiers = None;
              source = None;
            }
        | T_LET
        | T_CONST
        | T_VAR
        | T_CLASS
        | T_FUNCTION ->
            let end_loc, declaration = match Peek.token env with
            | T_FUNCTION ->
                (* declare export function foo (...): ...  *)
                let fn = declare_function env start_loc in
                fst fn, Some (Function fn)
            | T_CLASS ->
                (* declare export class foo { ... } *)
                let _class = declare_class env start_loc in
                fst _class, Some (Class _class)
            | T_LET
            | T_CONST
            | T_VAR as token ->
                (match token with
                | T_LET -> error env Error.DeclareExportLet
                | T_CONST -> error env Error.DeclareExportConst
                | _ -> ());
                (* declare export var foo: ... *)
                let var = declare_var env start_loc in
                fst var, Some (Variable var)
            | _ -> assert false in
            Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
              default = false;
              declaration;
              specifiers = None;
              source = None;
            }
        | T_MULT ->
            (* declare export * from 'foo' *)
            let loc = Peek.loc env in
            let specifiers = Some (Statement.ExportDeclaration.ExportBatchSpecifier loc) in
            Expect.token env T_MULT;
            let source = export_source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = Some source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
              default = false;
              declaration = None;
              specifiers;
              source;
            }
        | _ ->
            if Peek.token env = T_TYPE
            then error env Error.DeclareExportType;
            Expect.token env T_LCURLY;
            let specifiers, errs = export_specifiers_and_errs env [] [] in
            let specifiers = Some (Statement.ExportDeclaration.ExportSpecifiers specifiers) in
            let end_loc = Peek.loc env in
            Expect.token env T_RCURLY;
            let source = if Peek.value env = "from"
            then Some (export_source env)
            else begin
              errs |> List.iter (error_at env);
              None
            end in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None ->
                (match source with
                | Some source -> fst source
                | None -> end_loc) in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
              default = false;
              declaration = None;
              specifiers;
              source;
            }
        )

      and import_declaration =
        let source env =
          Expect.contextual env "from";
          match Peek.token env with
          | T_STRING (loc, value, raw, octal) ->
              if octal then strict_error env Error.StrictOctalLiteral;
              Expect.token env (T_STRING (loc, value, raw, octal));
              let value = Literal.String value in
              loc, { Literal.value; raw; }
          | _ ->
              (* Just make up a string for the error case *)
              let raw = Peek.value env in
              let value = Literal.String raw in
              let ret = Peek.loc env, Literal.({ value; raw; }) in
              error_unexpected env;
              ret

        in let rec specifier_list env acc =
          match Peek.token env with
          | T_EOF
          | T_RCURLY -> List.rev acc
          | _ ->
              let id, err = Parse.identifier_or_reserved_keyword env in
              let end_loc, name = if Peek.value env = "as"
              then begin
                Expect.contextual env "as";
                let name = Parse.identifier env in
                fst name, Some name
              end else begin
                (match err with
                | Some err -> error_at env err
                | None -> ());
                fst id, None
              end in
              let loc = Loc.btwn (fst id) end_loc in
              let specifier =
                loc, Statement.ImportDeclaration.NamedSpecifier.({
                  id;
                  name;
                }) in
              if Peek.token env = T_COMMA
              then Expect.token env T_COMMA;
              specifier_list env (specifier::acc)

        in let specifier env =
          let start_loc = Peek.loc env in
          match Peek.token env with
          | T_MULT ->
              Expect.token env T_MULT;
              Expect.contextual env "as";
              let id = Parse.identifier env in
              Statement.ImportDeclaration.NameSpace (Loc.btwn start_loc (fst id), id)
          | _ ->
              Expect.token env T_LCURLY;
              let specifiers = specifier_list env [] in
              let end_loc = Peek.loc env in
              Expect.token env T_RCURLY;
              Statement.ImportDeclaration.Named (Loc.btwn start_loc end_loc, specifiers)

        in fun env ->
          let env = env |> with_strict true in
          let start_loc = Peek.loc env in
          Expect.token env T_IMPORT;
          (* It might turn out that we need to treat this "type" token as an
           * identifier, like import type from "module" *)
          let importKind, type_ident = Statement.ImportDeclaration.(
            match Peek.token env with
            | T_TYPE ->
              if not (should_parse_types env)
              then error env Error.UnexpectedTypeImport;
              ImportType, Some(Parse.identifier env)
            | T_TYPEOF ->
              if not (should_parse_types env)
              then error env Error.UnexpectedTypeImport;
              Expect.token env T_TYPEOF;
              ImportTypeof, None
            | _ -> ImportValue, None
          ) in
          Statement.ImportDeclaration.(
            match Peek.token env, Peek.identifier env with
            | T_STRING (str_loc, value, raw, octal), _
              when importKind = ImportValue ->
                (* import "ModuleSpecifier"; *)
                if octal then strict_error env Error.StrictOctalLiteral;
                Expect.token env (T_STRING (str_loc, value, raw, octal));
                let value = Literal.String value in
                let source = (str_loc, { Literal.value; raw; }) in
                let end_loc = match Peek.semicolon_loc env with
                | Some loc -> loc
                | None -> str_loc in
                Eat.semicolon env;
                Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
                  default = None;
                  specifier = None;
                  source;
                  importKind;
                }
            | T_COMMA, _
            | _, true ->
                (* import defaultspecifier ... *)
                let importKind, default = (
                  match type_ident, Peek.token env, Peek.value env with
                  | Some _, T_COMMA, _
                  | Some _, T_IDENTIFIER, "from" ->
                      (* import type, ... *)
                      (* import type from ... *)
                      Statement.ImportDeclaration.ImportValue, type_ident
                  | _ ->
                      (* import type foo ...
                      * import foo ... *)
                      importKind, Some (Parse.identifier env)
                ) in
                let specifier = (match Peek.token env with
                  | T_COMMA ->
                      Expect.token env T_COMMA;
                      Some (specifier env)
                  | _ -> None) in
                  let source = source env in
                  let end_loc = match Peek.semicolon_loc env with
                  | Some loc -> loc
                  | None -> fst source in
                  let source = source in
                  Eat.semicolon env;
                  Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
                    default;
                    specifier;
                    source;
                    importKind;
                  }
            | _ ->
                let specifier = Some (specifier env) in
                let source = source env in
                let end_loc = match Peek.semicolon_loc env with
                | Some loc -> loc
                | None -> fst source in
                let source = source in
                Eat.semicolon env;
                Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
                  default = None;
                  specifier;
                  source;
                  importKind;
                }
          )
  end

  module Pattern = struct
    let _object =
      let property env prop =
        Ast.Expression.Object.(match prop with
        | Property (loc, { Property.key; value; _ }) ->
          let key = Property.(match key with
          | Literal lit -> Pattern.Object.Property.Literal lit
          | Identifier id -> Pattern.Object.Property.Identifier id
          | Computed expr -> Pattern.Object.Property.Computed expr) in
          let pattern = Parse.pattern env value in
          Pattern.(Object.Property (loc, Object.Property.({
            key;
            pattern;
          })))
        | SpreadProperty (loc, { SpreadProperty.argument; }) ->
            let argument = Parse.pattern env argument in
            Pattern.(Object.SpreadProperty (loc, Object.SpreadProperty.({
              argument;
            }))))

      in fun ?(with_type=false) env (loc, obj) ->
        let properties =
          List.map (property env) obj.Ast.Expression.Object.properties in
        let loc, typeAnnotation =
          if with_type && Peek.token env = T_COLON
          then begin
            let typeAnnotation = Type.annotation env in
            Loc.btwn loc (fst typeAnnotation), Some typeAnnotation
          end else loc, None in
        loc, Pattern.(Object Object.({
          properties;
          typeAnnotation;
        }))

    let _array =
      let element env = Ast.Expression.(function
        | None -> None
        | Some (Spread (loc, spread)) ->
            let argument = Parse.pattern env (spread.SpreadElement.argument) in
            Some Pattern.(Array.Spread (loc, { Array.SpreadElement.argument; }))
        | Some (Expression (loc, expr)) ->
            Some Pattern.Array.(Element (Parse.pattern env (loc, expr)))
      )

      in fun ?(with_type=false) env (loc, arr) ->
        let elements =
          List.map (element env) arr.Ast.Expression.Array.elements in
        let loc, typeAnnotation =
          if with_type && Peek.token env = T_COLON
          then begin
            let typeAnnotation = Type.annotation env in
            Loc.btwn loc (fst typeAnnotation), Some typeAnnotation
          end else loc, None in
        loc, Pattern.(Array Array.({
          elements;
          typeAnnotation;
        }))

    (* Reinterpret various expressions as patterns *)
    let pattern env (loc, expr) =
      Ast.Expression.(match expr with
      | Object obj -> _object env (loc, obj)
      | Array arr ->  _array env (loc, arr)
      | Identifier id -> loc, Pattern.Identifier id
      | expr -> loc, Pattern.Expression (loc, expr))
  end

  module JSX = struct
    let spread_attribute env =
      Eat.push_lex_mode env NORMAL_LEX;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      Expect.token env T_ELLIPSIS;
      let argument = Expression.assignment env in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.SpreadAttribute.({
        argument;
      })

    let expression_container env =
      Eat.push_lex_mode env NORMAL_LEX;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let expression = if Peek.token env = T_RCURLY
        then None
        else Some (Parse.expression env) in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.ExpressionContainer.({
        expression;
      })

    let identifier env =
      let loc = Peek.loc env in
      let name = Peek.value env in
      Expect.token env T_JSX_IDENTIFIER;
      loc, JSX.Identifier.({ name; })

    let name =
      let rec member_expression env member =
        match Peek.token env with
        | T_PERIOD ->
            let _object = JSX.MemberExpression.MemberExpression member in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst member) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            member_expression env member
        | _ -> member

      in fun env ->
        let name = identifier env in
        match Peek.token env with
        | T_COLON ->
            let namespace = name in
            Expect.token env T_COLON;
            let name = identifier env in
            let loc = Loc.btwn (fst namespace) (fst name) in
            JSX.NamespacedName (loc, JSX.NamespacedName.({
              namespace;
              name;
            }))
        | T_PERIOD ->
            let _object = JSX.MemberExpression.Identifier name in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst name) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            JSX.MemberExpression (member_expression env member)
        | _ -> JSX.Identifier name


    let attribute env =
      let start_loc = Peek.loc env in
      let name = identifier env in
      let end_loc, name =
        if Peek.token env = T_COLON
        then begin
          Expect.token env T_COLON;
          let namespace = name in
          let name = identifier env in
          let loc = Loc.btwn (fst namespace) (fst name) in
          loc, JSX.Attribute.NamespacedName (loc, JSX.NamespacedName.({
            namespace;
            name;
          }))
        end else fst name, JSX.Attribute.Identifier name in
      let end_loc, value =
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          match Peek.token env with
          | T_LCURLY ->
              let loc, expression_container = expression_container env in
              if expression_container.JSX.ExpressionContainer.expression = None
              then error env Error.JSXAttributeValueEmptyExpression;
              loc, Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
          | T_JSX_TEXT (loc, value, raw) as token ->
              Expect.token env token;
              let value = Ast.Literal.String value in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
          | _ ->
              error env Error.InvalidJSXAttributeValue;
              let loc = Peek.loc env in
              let raw = "" in
              let value = Ast.Literal.String "" in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
        end else end_loc, None in
      Loc.btwn start_loc end_loc, JSX.Attribute.({
        name;
        value;
      })

      let opening_element_without_lt =
        let rec attributes env acc =
          match Peek.token env with
          | T_EOF
          | T_DIV
          | T_GREATER_THAN -> List.rev acc
          | T_LCURLY ->
              let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
              attributes env (attribute::acc)
          | _ ->
              let attribute = JSX.Opening.Attribute (attribute env) in
              attributes env (attribute::acc)

        in fun env start_loc ->
          let name = name env in
          let attributes = attributes env [] in
          let selfClosing = Peek.token env = T_DIV in
          if selfClosing then Expect.token env T_DIV;
          let end_loc = Peek.loc env in
          Expect.token env T_GREATER_THAN;
          Eat.pop_lex_mode env;
          Loc.btwn start_loc end_loc, JSX.Opening.({
            name;
            selfClosing;
            attributes;
          })

      let closing_element_without_lt env start_loc =
        Expect.token env T_DIV;
        let name = name env in
        let end_loc = Peek.loc env in
        Expect.token env T_GREATER_THAN;
        (* We double pop to avoid going back to childmode and re-lexing the
         * lookahead *)
        Eat.double_pop_lex_mode env;
        Loc.btwn start_loc end_loc, JSX.Closing.({
          name;
        })

      type element_or_closing =
        | Closing of JSX.Closing.t
        | ChildElement of (Loc.t * JSX.element)


      let rec child env =
        match Peek.token env with
        | T_LCURLY ->
            let expression_container = expression_container env in
            fst expression_container, JSX.ExpressionContainer (snd expression_container)
        | T_JSX_TEXT (loc, value, raw) as token ->
            Expect.token env token;
            loc, JSX.Text { JSX.Text.value; raw; }
        | _ ->
            let element = element env in
            fst element, JSX.Element (snd element)

      and element_without_lt =
        let element_or_closing env =
          Eat.push_lex_mode env JSX_TAG;
          let start_loc = Peek.loc env in
          Expect.token env T_LESS_THAN;
          match Peek.token env with
          | T_EOF
          | T_DIV -> Closing (closing_element_without_lt env start_loc)
          | _ -> ChildElement (element_without_lt env start_loc)

        in let rec children_and_closing env acc =
          match Peek.token env with
          | T_LESS_THAN -> (
              match element_or_closing env with
              | Closing closingElement ->
                  List.rev acc, Some closingElement
              | ChildElement element ->
                  let element = fst element, JSX.Element (snd element) in
                  children_and_closing env (element::acc))
          | T_EOF ->
              error_unexpected env;
              List.rev acc, None
          | _ ->
              children_and_closing env ((child env)::acc)

        in let rec normalize name = JSX.(match name with
          | Identifier (_, { Identifier.name }) -> name
          | NamespacedName (_, { NamespacedName.namespace; name; }) ->
              (snd namespace).Identifier.name ^ ":" ^ (snd name).Identifier.name
          | MemberExpression (_, { MemberExpression._object; property; }) ->
              let _object = match _object with
              | MemberExpression.Identifier id -> (snd id).Identifier.name
              | MemberExpression.MemberExpression e -> normalize (JSX.MemberExpression e) in
              _object ^ "." ^ (snd property).Identifier.name
        )

        in fun env start_loc ->
          let openingElement = opening_element_without_lt env start_loc in
          let children, closingElement =
            if (snd openingElement).JSX.Opening.selfClosing
            then [], None
            else begin
              Eat.push_lex_mode env JSX_CHILD;
              let ret = children_and_closing env [] in
              ret
            end in
          let end_loc = match closingElement with
          | Some (loc, { JSX.Closing.name }) ->
              let opening_name = normalize (snd openingElement).JSX.Opening.name in
              if normalize name <> opening_name
              then error env (Error.ExpectedJSXClosingTag opening_name);
              loc
          | _ -> fst openingElement in
          Loc.btwn (fst openingElement) end_loc, JSX.({
            openingElement;
            closingElement;
            children;
          })

      and element env =
        let start_loc = Peek.loc env in
        Eat.push_lex_mode env JSX_TAG;
        Expect.token env T_LESS_THAN;
        element_without_lt env start_loc
  end

  let rec program env =
    let stmts = module_body_with_directives env (fun _ -> false) in
    let loc = match stmts with
    | [] -> Loc.from_lb (source env) (lb env)
    | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts))) in
    Expect.eof env;
    let comments = List.rev (comments env) in
    loc, stmts, comments

  and directives =
      let check env (loc, token) =
        match token with
        | T_STRING (_, _, _, octal) ->
            if octal then strict_error_at env (loc, Error.StrictOctalLiteral)
        | _ -> failwith ("Nooo: "^(token_to_string token)^"\n")

      in let rec statement_list env term_fn item_fn (string_tokens, stmts) =
        match Peek.token env with
        | T_EOF -> env, string_tokens, stmts
        | t when term_fn t -> env, string_tokens, stmts
        | _ ->
            let string_token = Peek.loc env, Peek.token env in
            let possible_directive = item_fn env in
            let stmts = possible_directive::stmts in
            (match possible_directive with
            | _, Ast.Statement.Expression {
                Ast.Statement.Expression.expression = loc, Ast.Expression.Literal {
                  Ast.Literal.value = Ast.Literal.String str;
                  _;
                }
              } ->
                (* 14.1.1 says that it has to be "use strict" without any
                  * escapes, so "use\x20strict" is disallowed. We could in theory
                  * keep the raw string around, but that's a pain. This is a hack
                  * that actually seems to work pretty well (make sure the string
                  * has the right length)
                  *)
                let len = Loc.(loc._end.column - loc.start.column) in
                let strict = (strict env) || (str = "use strict" && len = 12) in
                let string_tokens = string_token::string_tokens in
                statement_list
                  (env |> with_strict strict)
                  term_fn
                  item_fn
                  (string_tokens, stmts)
            | _ ->
                env, string_tokens, stmts)

      in fun env term_fn item_fn ->
        let env, string_tokens, stmts = statement_list env term_fn item_fn ([], []) in
        List.iter (check env) (List.rev string_tokens);
        env, stmts

  (* 15.2 *)
  and module_item env =
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration env
    | T_IMPORT -> Statement.import_declaration env
    | T_DECLARE when Peek.token ~i:1 env = T_EXPORT ->
        Statement.declare_export_declaration env
    | _ -> statement_list_item env

  and module_body_with_directives env term_fn =
    let env, directives = directives env term_fn module_item in
    let stmts = module_body ~term_fn env in
    (* Prepend the directives *)
    List.fold_left (fun acc stmt -> stmt::acc) stmts directives

  and module_body =
    let rec module_item_list env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> module_item_list env term_fn (module_item env::acc)

    in fun ~term_fn env ->
      module_item_list env term_fn []

  and statement_list_with_directives ~term_fn env =
    let env, directives = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt::acc) stmts directives in
    stmts, (strict env)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn ((statement_list_item env)::acc)

    in fun ~term_fn env -> statements env term_fn []


  and statement_list_item env =
    Statement.(match Peek.token env with
    (* Remember kids, these look like statements but they're not
      * statements... (see section 13) *)
    | T_LET -> _let env
    | T_CONST -> var_or_const env
    | _ when Peek._function env -> Declaration._function env
    | T_CLASS -> class_declaration env
    | T_INTERFACE -> interface env
    | T_DECLARE -> declare env
    | T_TYPE -> type_alias env
    | _ -> statement env)

  and statement env =
    Statement.(match Peek.token env with
    | T_EOF ->
        error_unexpected env;
        Peek.loc env, Ast.Statement.Empty
    | T_SEMICOLON -> empty env
    | T_LCURLY -> block env
    | T_VAR -> var_or_const env
    | T_BREAK -> break env
    | T_CONTINUE -> continue env
    | T_DEBUGGER -> debugger env
    | T_DO -> do_while env
    | T_FOR -> _for env
    | T_IF -> _if env
    | T_RETURN -> return env
    | T_SWITCH -> switch env
    | T_THROW -> throw env
    | T_TRY -> _try env
    | T_WHILE -> _while env
    | T_WITH -> _with env
    | _ when Peek.identifier env -> maybe_labeled env
    (* If we see an else then it's definitely an error, but we can probably
     * assume that this is a malformed if statement that is missing the if *)
    | T_ELSE -> _if env
    (* There are a bunch of tokens that aren't the start of any valid
     * statement. We list them here in order to skip over them, rather than
     * getting stuck *)
    | T_COLON
    | T_RPAREN
    | T_RCURLY
    | T_RBRACKET
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_IN
    | T_INSTANCEOF
    | T_CATCH
    | T_FINALLY
    | T_CASE
    | T_DEFAULT
    | T_EXTENDS
    | T_STATIC
    | T_IMPORT (* TODO *)
    | T_EXPORT (* TODO *)
    | T_AT
    | T_ELLIPSIS ->
        error_unexpected env;
        Eat.token env;
        statement env
    | _ -> expression env)

  and expression env =
    let expr = Expression.assignment env in
    match Peek.token env with
    | T_COMMA -> Expression.sequence env [expr]
    | _ ->
        expr

  and assignment = Expression.assignment
  and object_initializer = Object._initializer
  and object_key = Object.key
  and class_declaration = Object.class_declaration
  and class_expression = Object.class_expression
  and array_initializer = Expression.array_initializer

  and is_assignable_lhs = Expression.is_assignable_lhs

  and identifier ?restricted_error env =
    let loc = Peek.loc env in
    let name = Peek.value env in
    (match Peek.token env with
    | T_LET ->
    (* So "let" is disallowed as an identifier in a few situations. 11.6.2.1
     * lists them out. It is always disallowed in strict mode *)
      if strict env
      then strict_error env Error.StrictReservedWord
      else
        if no_let env
        then error env (Error.UnexpectedToken name);
      Eat.token env
    | _ when is_strict_reserved name ->
      strict_error env Error.StrictReservedWord;
      Eat.token env
    | T_DECLARE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    | T_TYPE as t ->
        (* These aren't real identifiers *)
        Expect.token env t
    | _ -> Expect.token env T_IDENTIFIER);
    (match restricted_error with
    | Some err when is_restricted name -> strict_error_at env (loc, err)
    | _ -> ());
    loc, Identifier.({
      name;
      typeAnnotation = None;
      optional = false;
    })

  and identifier_or_reserved_keyword = Expression.identifier_or_reserved_keyword

  and identifier_with_type env restricted_error =
    let loc, id = identifier ~restricted_error env in
    let loc, id =
      if Peek.token env = T_PLING
      then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        let loc = Loc.btwn loc (Peek.loc env) in
        Expect.token env T_PLING;
        loc, { id with Identifier.optional = true; }
      end else (loc, id) in
    if Peek.token env = T_COLON
    then begin
      let typeAnnotation = Type.annotation env in
      let loc = Loc.btwn loc (fst typeAnnotation) in
      let typeAnnotation = Some typeAnnotation in
      Identifier.(loc, { id with typeAnnotation; })
    end else loc, id

  and block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body = statement_list ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }

  and function_block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body, strict = statement_list_with_directives ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }, strict

  and jsx_element = JSX.element

  and pattern = Pattern.pattern
  and object_pattern_with_type = Pattern._object ~with_type:true
  and array_pattern_with_type = Pattern._array ~with_type:true
end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse_program fail ?(token_sink=None) ?(parse_options=None) filename content =
  let lb = Lexing.from_string content in
  (match filename with
    | None | Some Loc.Builtins -> ()
    | Some Loc.LibFile fn | Some Loc.SourceFile fn ->
      lb.Lexing.lex_curr_p <- {
        lb.Lexing.lex_curr_p with Lexing.pos_fname = fn
      });
  let env = init_env ~token_sink ~parse_options filename lb in
  let ast = Parse.program env in
  if fail && (errors env) <> []
  then raise (Error.Error (filter_duplicate_errors [] (errors env)));
  ast, List.rev (errors env)

let program ?(fail=true) ?(token_sink=None) ?(parse_options=None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  parse_program fail ~token_sink ~parse_options filename content
