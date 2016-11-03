(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Token = Lexer_flow.Token
open Token
open Parser_env
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
module SMap = Map.Make(String)

(* Sometimes we add the same error for multiple different reasons. This is hard
   to avoid, so instead we just filter the duplicates out. This function takes
   a reversed list of errors and returns the list in forward order with dupes
   removed. This differs from a set because the original order is preserved. *)
let filter_duplicate_errors =
  let module ErrorSet = Set.Make(struct
    type t = Loc.t * Error.t
    let compare (a_loc, a_error) (b_loc, b_error) =
      let loc = Loc.compare a_loc b_loc in
      if loc = 0
      then Pervasives.compare a_error b_error
      else loc
  end) in
  fun errs ->
    let errs = List.rev errs in
    let _, deduped = List.fold_left (fun (set, deduped) err ->
      if ErrorSet.mem err set then (set, deduped)
      else (ErrorSet.add err set, err::deduped)
    ) (ErrorSet.empty, []) errs in
    List.rev deduped

let with_loc fn env =
  let start_loc = Peek.loc env in
  let result = fn env in
  let end_loc = match last_loc env with
  | Some loc -> loc
  | None ->
      error env (Error.Assertion "did not consume any tokens");
      Peek.loc env
  in
  Loc.btwn start_loc end_loc, result

let string_starts_with long short =
  try
    let long = String.sub long 0 (String.length short) in
    long = short
  with Invalid_argument _ ->
    false

module rec Parse : sig
  val program : env -> Ast.program
  val statement : env -> Ast.Statement.t
  val statement_list_item : ?decorators:Ast.Expression.t list -> env -> Ast.Statement.t
  val statement_list : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list
  val statement_list_with_directives : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list * bool
  val module_body : term_fn:(Token.t -> bool) -> env -> Ast.Statement.t list
  val expression : env -> Ast.Expression.t
  val conditional : env -> Ast.Expression.t
  val assignment : env -> Ast.Expression.t
  val left_hand_side : env -> Ast.Expression.t
  val object_initializer : env -> Loc.t * Ast.Expression.Object.t
  val array_initializer : env -> Loc.t * Ast.Expression.Array.t
  val identifier : ?restricted_error:Error.t -> env -> Ast.Identifier.t
  val identifier_or_reserved_keyword : env -> (Ast.Identifier.t * (Loc.t * Error.t) option)
  val identifier_with_type : env -> ?no_optional:bool -> Error.t -> Loc.t * Ast.Pattern.Identifier.t
  val block_body : env -> Loc.t * Ast.Statement.Block.t
  val function_block_body : env -> Loc.t * Ast.Statement.Block.t * bool
  val jsx_element : env -> Loc.t * Ast.JSX.element
  val pattern : env -> Error.t -> Ast.Pattern.t
  val pattern_from_expr : env -> Ast.Expression.t -> Ast.Pattern.t
  val object_key : env -> Loc.t * Ast.Expression.Object.Property.key
  val class_declaration : env -> Ast.Expression.t list -> Ast.Statement.t
  val class_expression : env -> Ast.Expression.t
  val is_assignable_lhs : Ast.Expression.t -> bool
end = struct
  module Type : sig
    val _type : env -> Ast.Type.t
    val type_parameter_declaration : env -> Ast.Type.ParameterDeclaration.t option
    val type_parameter_declaration_with_defaults : env -> Ast.Type.ParameterDeclaration.t option
    val type_parameter_instantiation : env -> Ast.Type.ParameterInstantiation.t option
    val generic : env -> Loc.t * Ast.Type.Generic.t
    val _object : ?allow_static:bool -> env -> Loc.t * Type.Object.t
    val function_param_list : env -> Type.Function.Param.t list * Type.Function.RestParam.t option
    val annotation : env -> Ast.Type.annotation
    val annotation_opt : env -> Ast.Type.annotation option
    val predicate_opt : env -> Ast.Type.Predicate.t option
    val annotation_and_predicate_opt : env -> Ast.Type.annotation option * Ast.Type.Predicate.t option
  end = struct
    type param_list_or_type =
      | ParamList of (Type.Function.Param.t list * Type.Function.RestParam.t option)
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

    and variance env =
      let loc = Peek.loc env in
      match Peek.token env with
       | T_PLUS ->
           Eat.token env;
           Some (loc, Variance.Plus)
       | T_MINUS ->
           Eat.token env;
           Some (loc, Variance.Minus)
       | _ ->
           None

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
      let _ = Expect.maybe env T_BIT_OR in
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
            match acc with
            | t0::t1::ts -> loc, Type.Union (t0, t1, ts)
            | _ -> assert false
      in fun env left ->
        if Peek.token env = T_BIT_OR
        then unions env [left]
        else left

    and intersection env =
      let _ = Expect.maybe env T_BIT_AND in
      let left = anon_function_without_parens env in
      intersection_with env left

    and intersection_with =
      let rec intersections env acc =
        match Peek.token env with
        | T_BIT_AND ->
            Expect.token env T_BIT_AND;
            intersections env (anon_function_without_parens env::acc)
        | _ ->
            let loc, acc = rev_nonempty_acc acc in
            match acc with
            | t0::t1::ts -> loc, Type.Intersection (t0, t1, ts)
            | _ -> assert false
      in fun env left ->
        if Peek.token env = T_BIT_AND
        then intersections env [left]
        else left


    and anon_function_without_parens env =
      let param = prefix env in
      anon_function_without_parens_with env param

    and anon_function_without_parens_with env param =
      match Peek.token env with
      | T_ARROW when not (no_anon_function_type env)->
        let start_loc, typeParameters, params =
          let param = anonymous_function_param env param in
          fst param, None, ([param], None)
        in
        function_with_params env start_loc typeParameters params
      | _ -> param

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
      if not (Peek.is_line_terminator env) && Expect.maybe env T_LBRACKET
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
      | T_LCURLY
      | T_LCURLYBAR ->
        let loc, o = _object env ~allow_exact:true in
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
          loc, Type.StringLiteral {
            Type.StringLiteral.value;
            raw;
          }
      | T_NUMBER_SINGLETON_TYPE (number_type, value) ->
          let raw = Peek.value env in
          Expect.token env (T_NUMBER_SINGLETON_TYPE (number_type, value));
          if number_type = LEGACY_OCTAL
          then strict_error env Error.StrictOctalLiteral;
          loc, Type.NumberLiteral {
            Type.NumberLiteral.value;
            raw;
          }
      | (T_TRUE | T_FALSE) as token ->
          let raw = Peek.value env in
          Expect.token env token;
          let value = token = T_TRUE in
          loc, Type.BooleanLiteral {
            Type.BooleanLiteral.value;
            raw;
          }
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
      | T_MIXED_TYPE   -> Some Type.Mixed
      | T_EMPTY_TYPE   -> Some Type.Empty
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
            (* Trailing comma support (like [number, string,]) *)
            if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
            types env acc

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let tl = types env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Type.Tuple tl

    and anonymous_function_param _env typeAnnotation =
      fst typeAnnotation, Type.Function.Param.({
        name = None;
        typeAnnotation;
        optional = false;
      })


    and function_param_with_id env name =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let optional = Expect.maybe env T_PLING in
      Expect.token env T_COLON;
      let typeAnnotation = _type env in
      Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
        name = Some name;
        typeAnnotation;
        optional;
      })

    and function_param_list_without_parens =
      let param env =
        match Peek.token ~i:1 env with
        | T_COLON | T_PLING ->
            let name, _ = Parse.identifier_or_reserved_keyword env in
            function_param_with_id env name
        | _ ->
            let typeAnnotation = _type env in
            anonymous_function_param env typeAnnotation

      in let rec param_list env acc =
        match Peek.token env with
        | T_EOF
        | T_ELLIPSIS
        | T_RPAREN as t ->
          let rest =
            if t = T_ELLIPSIS then begin
              let start_loc = Peek.loc env in
              Expect.token env T_ELLIPSIS;
              let argument = param env in
              let loc = Loc.btwn start_loc (fst argument) in
              Some (loc, Type.Function.RestParam.({
                argument;
              }))
            end else
              None
          in
          List.rev acc, rest
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
      let ret =
        let env = with_no_anon_function_type false env in
        match Peek.token env with
        | T_EOF
        | T_ELLIPSIS ->
            (* (... is definitely the beginning of a param list *)
            ParamList (function_param_list_without_parens env [])
        | T_RPAREN ->
            (* () or is definitely a param list *)
            ParamList ([], None)
        | T_IDENTIFIER ->
            (* This could be a function parameter or a generic type *)
            function_param_or_generic_type env
        | token ->
            (match primitive token with
            | None ->
                (* All params start with an identifier or `...` *)
                Type (_type env)
            | Some _ ->
                (* Don't know if this is (number) or (number: number). The first
                 * is a type, the second is a param. *)
                match Peek.token ~i:1 env with
                | T_PLING | T_COLON ->
                  (* Ok this is definitely a parameter *)
                  ParamList (function_param_list_without_parens env [])
                | _ ->
                  Type (_type env)
            )
      in
      (* Now that we allow anonymous parameters in function types, we need to
       * disambiguate a little bit more *)
      let ret = match ret with
      | ParamList _ -> ret
      | Type _ when no_anon_function_type env -> ret
      | Type t ->
          (match Peek.token env with
          | T_RPAREN ->
              (* Reinterpret `(type) =>` as a ParamList *)
              if Peek.token ~i:1 env = T_ARROW
              then
                let param = anonymous_function_param env t in
                ParamList (function_param_list_without_parens env [param])
              else Type t
          | T_COMMA ->
              (* Reinterpret `(type,` as a ParamList *)
              Expect.token env T_COMMA;
              let param = anonymous_function_param env t in
              ParamList (function_param_list_without_parens env [param])
          | _ -> ret) in
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
          Type (
            generic_type_with_identifier env id
            |> postfix_with env
            |> anon_function_without_parens_with env
            |> intersection_with env
            |> union_with env
          )

    and function_or_group env =
      let start_loc = Peek.loc env in
      match param_list_or_type env with
      | ParamList params -> function_with_params env start_loc None params
      | Type _type -> _type

    and _function env =
      let start_loc = Peek.loc env in
      let typeParameters = type_parameter_declaration ~allow_default:false env in
      let params = function_param_list env in
      function_with_params env start_loc typeParameters params

    and function_with_params env start_loc typeParameters params =
      Expect.token env T_ARROW;
      let returnType = _type env in
      let end_loc = fst returnType in
      Loc.btwn start_loc end_loc, Type.(Function Function.({
        params;
        returnType;
        typeParameters;
      }))

    and _object =
      let methodish env start_loc =
        let typeParameters = type_parameter_declaration ~allow_default:false env in
        let params = function_param_list env in
        Expect.token env T_COLON;
        let returnType = _type env in
        let loc = Loc.btwn start_loc (fst returnType) in
        loc, Type.Function.({
          params;
          returnType;
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
          variance = None;
        })

      in let call_property env start_loc static =
        let value = methodish env (Peek.loc env) in
        Loc.btwn start_loc (fst value), Type.Object.CallProperty.({
          value;
          static;
        })

      in let property env start_loc static variance key =
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
          variance;
        })

      in let indexer_property env start_loc static variance =
        Expect.token env T_LBRACKET;
        let id =
          if Peek.token ~i:1 env = T_COLON
          then begin
            let id, _ = Parse.identifier_or_reserved_keyword env in
            Expect.token env T_COLON;
            Some id
          end else None in
        let key = _type env in
        Expect.token env T_RBRACKET;
        Expect.token env T_COLON;
        let value = _type env in
        Loc.btwn start_loc (fst value), Type.Object.Indexer.({
          id;
          key;
          value;
          static;
          variance;
        })

      in let semicolon exact env =
        match Peek.token env with
        | T_COMMA | T_SEMICOLON -> Eat.token env
        | T_RCURLYBAR when exact -> ()
        | T_RCURLY when not exact -> ()
        | _ -> error_unexpected env

      in let error_unsupported_variance env = function
      | Some (loc, _) -> error_at env (loc, Error.UnexpectedVariance)
      | None -> ()

      in let rec properties ~allow_static ~exact env
        (acc, indexers, callProperties) =
        let start_loc = Peek.loc env in
        let static = allow_static && Expect.maybe env T_STATIC in
        let variance = variance env in
        match Peek.token env with
        | T_EOF ->
          List.rev acc, List.rev indexers, List.rev callProperties
        | T_RCURLYBAR when exact ->
          List.rev acc, List.rev indexers, List.rev callProperties
        | T_RCURLY when not exact ->
          List.rev acc, List.rev indexers, List.rev callProperties
        | T_LBRACKET ->
          let indexer = indexer_property env start_loc static variance in
          semicolon exact env;
          properties allow_static exact env
            (acc, indexer::indexers, callProperties)
        | T_LESS_THAN
        | T_LPAREN ->
          error_unsupported_variance env variance;
          let call_prop = call_property env start_loc static in
          semicolon exact env;
          properties allow_static exact env
            (acc, indexers, call_prop::callProperties)
        | token ->
          let static, (_, key) = match static, variance, token with
          | true, None, T_COLON ->
              strict_error_at env (start_loc, Error.StrictReservedWord);
              let static_key =
                start_loc, Expression.Object.Property.Identifier (
                  start_loc,
                  "static"
                ) in
              false, static_key
          | _ ->
              Eat.push_lex_mode env Lex_mode.NORMAL;
              let key = Parse.object_key env in
              Eat.pop_lex_mode env;
              static, key
          in
          let property = match Peek.token env with
          | T_LESS_THAN
          | T_LPAREN ->
            error_unsupported_variance env variance;
            method_property env start_loc static key
          | _ ->
            property env start_loc static variance key
          in
          semicolon exact env;
          properties allow_static exact env
            (property::acc, indexers, callProperties)

      in fun ?(allow_static=false) ?(allow_exact=false) env ->
        let exact = allow_exact && Peek.token env = T_LCURLYBAR in
        let start_loc = Peek.loc env in
        Expect.token env (if exact then T_LCURLYBAR else T_LCURLY);
        let properties, indexers, callProperties =
          properties ~allow_static ~exact env ([], [], []) in
        let end_loc = Peek.loc env in
        Expect.token env (if exact then T_RCURLYBAR else T_RCURLY);
        Loc.btwn start_loc end_loc, Type.Object.({
          exact;
          properties;
          indexers;
          callProperties
        })

    and type_parameter_declaration =
      let rec params env ~allow_default ~require_default acc = Type.ParameterDeclaration.TypeParam.(
        let variance = variance env in
        let loc, {
          Pattern.Identifier.name = (_, name);
          typeAnnotation = bound;
          _;
        } = Parse.identifier_with_type env Error.StrictParamName in
        let default, require_default = match allow_default, Peek.token env with
        | false, _ -> None, false
        | true, T_ASSIGN ->
            Eat.token env;
            Some (_type env), true
        | true, _ ->
            if require_default
            then error_at env (loc, Error.MissingTypeParamDefault);
            None, require_default in
        let param = loc, {
          name;
          bound;
          variance;
          default;
        } in
        let acc = param::acc in
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          Expect.token env T_COMMA;
          if Peek.token env = T_GREATER_THAN
          then List.rev acc
          else params env ~allow_default ~require_default acc
      )
      in fun ~allow_default env ->
          let start_loc = Peek.loc env in
          if Peek.token env = T_LESS_THAN
          then begin
            if not (should_parse_types env)
            then error env Error.UnexpectedTypeAnnotation;
            Expect.token env T_LESS_THAN;
            let params = params env ~allow_default ~require_default:false [] in
            let loc = Loc.btwn start_loc (Peek.loc env) in
            Expect.token env T_GREATER_THAN;
            Some (loc, Type.ParameterDeclaration.({
              params;
            }))
          end else None

    and type_parameter_instantiation =
      let rec params env acc =
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN -> List.rev acc
        | _ ->
          let acc = (_type env)::acc in
          if Peek.token env <> T_GREATER_THAN
          then Expect.token env T_COMMA;
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

    let predicate env =
      let checks_loc = Peek.loc env in
      Expect.token env T_CHECKS;
      if Peek.token env = T_LPAREN then begin
        Expect.token env T_LPAREN;
        Eat.push_lex_mode env Lex_mode.NORMAL;
        let exp = Parse.conditional env in
        Eat.pop_lex_mode env;
        let rparen_loc = Peek.loc env in
        Expect.token env T_RPAREN;
        let loc = Loc.btwn checks_loc rparen_loc in
        (loc, Ast.Type.Predicate.Declared exp)
      end else
        (checks_loc, Ast.Type.Predicate.Inferred)

    let predicate_opt env =
      let env = with_no_anon_function_type false env in
      match Peek.token env with
      | T_CHECKS -> Some (predicate env)
      | _ -> None

    let annotation_and_predicate_opt env =
      match Peek.token env, Peek.token ~i:1 env with
      | T_COLON, T_CHECKS ->
        Expect.token env T_COLON;
        (None, predicate_opt env)
      | T_COLON, _ ->
         let annotation = annotation_opt env in
         let predicate = predicate_opt env in
         (annotation, predicate)
      | _ -> None, None

    let wrap f env =
      let env = env |> with_strict true in
      Eat.push_lex_mode env Lex_mode.TYPE;
      let ret = f env in
      Eat.pop_lex_mode env;
      ret

    let _type = wrap _type
    let type_parameter_declaration_with_defaults =
      wrap (type_parameter_declaration ~allow_default:true)
    let type_parameter_declaration =
      wrap (type_parameter_declaration ~allow_default:false)
    let type_parameter_instantiation = wrap type_parameter_instantiation
    let _object ?(allow_static=false) env =
      wrap (_object ~allow_static ~allow_exact:false) env
    let function_param_list = wrap function_param_list
    let annotation = wrap annotation
    let annotation_opt = wrap annotation_opt
    let predicate_opt = wrap predicate_opt
    let annotation_and_predicate_opt = wrap annotation_and_predicate_opt
    let generic = wrap generic
  end

  module Declaration = struct
    let check_param =
      let rec pattern ((env, _) as check_env) (loc, p) = Pattern.(match p with
        | Object o -> _object check_env o
        | Array arr -> _array check_env arr
        | Assignment { Assignment.left; _ } -> pattern check_env left
        | Identifier id -> identifier_pattern check_env id
        | Expression _ -> (
            error_at env (loc, Error.ExpectedPatternFoundExpression);
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
        | RestProperty (_, { RestProperty.argument; }) ->
            pattern check_env argument)

      and _array check_env arr =
        List.fold_left
        array_element
        check_env
        arr.Pattern.Array.elements

      and array_element check_env = Pattern.Array.(function
        | None -> check_env
        | Some (Element p) -> pattern check_env p
        | Some (RestElement (_, { RestElement.argument; })) ->
            pattern check_env argument)

      and identifier_pattern check_env {Pattern.Identifier.name=id; _;} =
        identifier check_env id

      and identifier (env, param_names) (loc, name as id) =
        if SSet.mem name param_names
        then error_at env (loc, Error.StrictParamDupe);
        let env, param_names =
          identifier_no_dupe_check (env, param_names) id in
        env, SSet.add name param_names

      and identifier_no_dupe_check (env, param_names) (loc, name) =
        if is_restricted name
        then strict_error_at env (loc, Error.StrictParamName);
        if is_future_reserved name || is_strict_reserved name
        then strict_error_at env (loc, Error.StrictReservedWord);
        env, param_names

      in pattern

    (* Strict is true if we were already in strict mode or if we are newly in
     * strict mode due to a directive in the function.
     * Simple is the IsSimpleParameterList thing from the ES6 spec *)
    let strict_post_check env ~strict ~simple id (params, rest) =
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
          then env |> with_strict (not (Parser_env.in_strict_mode env))
          else env in
        (match id with
        | Some (loc, name) ->
            if is_restricted name
            then strict_error_at env (loc, Error.StrictFunctionName);
            if is_future_reserved name || is_strict_reserved name
            then strict_error_at env (loc, Error.StrictReservedWord)
        | None -> ());
        let acc = List.fold_left check_param (env, SSet.empty) params in
        match rest with
        | Some (_, { Function.RestElement.argument }) ->
          ignore (check_param acc argument)
        | None ->
          ()

    let function_params =
      let rec param env =
        let left = Parse.pattern env Error.StrictParamName in
        (* TODO: shouldn't Parse.pattern recognize Assignment patterns? *)
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          let right = Parse.assignment env in
          let loc = Loc.btwn (fst left) (fst right) in
          (loc, Pattern.Assignment { Pattern.Assignment.left; right })
        end else
          left
      and param_list env acc =
        match Peek.token env with
        | T_EOF
        | T_RPAREN
        | T_ELLIPSIS as t ->
            let rest =
              if t = T_ELLIPSIS then begin
                let start_loc = Peek.loc env in
                Expect.token env T_ELLIPSIS;
                let id = Parse.pattern env Error.StrictParamName in
                let loc = Loc.btwn start_loc (fst id) in
                Some (loc, { Function.RestElement.argument = id; })
              end else
                None
            in
            if Peek.token env <> T_RPAREN
            then error env Error.ParameterAfterRestParameter;
            List.rev acc, rest
        | _ ->
            let the_param = param env in
            if Peek.token env <> T_RPAREN
            then Expect.token env T_COMMA;
            param_list env (the_param::acc)

      in fun env ->
        Expect.token env T_LPAREN;
        let params = param_list env [] in
        Expect.token env T_RPAREN;
        params

    let function_body env ~async ~generator =
      let env = enter_function env ~async ~generator in
      let loc, block, strict = Parse.function_block_body env in
      loc, Function.BodyBlock (loc, block), strict

    let concise_function_body env ~async ~generator =
      let env = env |> with_in_function true in
      match Peek.token env with
      | T_LCURLY ->
          let _, body, strict = function_body env ~async ~generator in
          body, strict
      | _ ->
          let env = enter_function env ~async ~generator in
          let expr = Parse.assignment env in
          Function.BodyExpression expr, in_strict_mode env

    let variance env is_async is_generator =
      let loc = Peek.loc env in
      let variance = match Peek.token env with
      | T_PLUS ->
          Eat.token env;
          Some (loc, Variance.Plus)
      | T_MINUS ->
          Eat.token env;
          Some (loc, Variance.Minus)
      | _ ->
          None
      in
      match variance with
      | Some (loc, _) when is_async || is_generator ->
          error_at env (loc, Error.UnexpectedVariance);
          None
      | _ ->
          variance

    let generator env = Expect.maybe env T_MULT

    let async env = Expect.maybe env T_ASYNC

    let is_simple_function_params =
      let is_simple_param = function
      | _, Pattern.Identifier _ ->  true
      | _ -> false

      in fun (params, rest) ->
        rest = None && List.for_all is_simple_param params

    let _function env =
      let start_loc = Peek.loc env in
      let async = async env in
      Expect.token env T_FUNCTION;
      let generator = generator env in
      let (typeParameters, id) = (
        match in_export env, Peek.token env with
        | true, T_LPAREN -> (None, None)
        | true, T_LESS_THAN ->
          let typeParams = Type.type_parameter_declaration env in
          let id = if Peek.token env = T_LPAREN then None else Some (
            Parse.identifier ~restricted_error:Error.StrictFunctionName env
          ) in
          (typeParams, id)
        | _ ->
          let id =
            Parse.identifier ~restricted_error:Error.StrictFunctionName env
          in
          (Type.type_parameter_declaration env, Some id)
      ) in
      let params = function_params env in
      let (returnType, predicate) = Type.annotation_and_predicate_opt env in
      let _, body, strict = function_body env ~async ~generator in
      let simple = is_simple_function_params params in
      strict_post_check env ~strict ~simple id params;
      let end_loc, expression = Ast.Function.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      Loc.btwn start_loc end_loc, Statement.(FunctionDeclaration Function.({
        id;
        params;
        body;
        generator;
        async;
        predicate;
        expression;
        returnType;
        typeParameters;
      }))

    let variable_declaration_list =
      let variable_declaration env =
        let id = Parse.pattern env Error.StrictVarName in
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
    type op_precedence = Left_assoc of int | Right_assoc of int
    let is_tighter a b =
      let a_prec = match a with Left_assoc x -> x | Right_assoc x -> x - 1 in
      let b_prec = match b with Left_assoc x -> x | Right_assoc x -> x in
      a_prec >= b_prec

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
          | loc, Expression.Identifier (_, name)
            when is_restricted name ->
              strict_error_at env (loc, Error.StrictLHSAssignment)
          | _ -> ());

          let left = Parse.pattern_from_expr env expr in
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
        | _ when Peek.is_identifier env -> begin match snd ret with
          | Expression.Identifier (_, "async")
              when not (Peek.is_line_terminator env) ->
            raise Try.Rollback
          | _ -> ret
          end
        | _ -> ret
      in fun env ->
        match Peek.token env, Peek.is_identifier env with
        | T_YIELD, _ when (allow_yield env) -> yield env
        | T_LPAREN, _
        | T_LESS_THAN, _
        | _, true ->

          (* Ok, we don't know if this is going to be an arrow function or a
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
      | _, MetaProperty _
      | _, Identifier _ -> true
      | _, Array _
      | _, Object _
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Super
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
      | _, TypeCast _ -> false)

    and is_assignable_lhs = Expression.(function
      | _, Array _
      | _, Object _
      | _, Member _
      | _, MetaProperty _
      | _, Identifier _ -> true
      | _, Literal _
      | _, TemplateLiteral _
      | _, TaggedTemplate _
      | _, This
      | _, Super
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
      | T_EXP_ASSIGN -> Some ExpAssign
      | T_MINUS_ASSIGN -> Some MinusAssign
      | T_PLUS_ASSIGN -> Some PlusAssign
      | T_ASSIGN -> Some Assign
      | _ -> None) in
      if op <> None then Eat.token env;
      op

    and conditional env =
      let start_loc = Peek.loc env in
      let expr = logical env in
      if Peek.token env = T_PLING
      then begin
        Expect.token env T_PLING;
        (* no_in is ignored for the consequent *)
        let env' = env |> with_no_in false in
        let consequent = assignment env' in
        Expect.token env T_COLON;
        let end_loc, alternate = with_loc assignment env in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.(Conditional Conditional.({
          test = expr;
          consequent;
          alternate;
        }))
      end else expr

    and logical =
      let open Expression in
      let make_logical left right operator loc =
        loc, Logical Logical.({operator; left; right;})
      in let rec logical_and env left lloc =
        match Peek.token env with
        | T_AND ->
            Expect.token env T_AND;
            let rloc, right = with_loc binary env in
            let loc = Loc.btwn lloc rloc in
            logical_and env (make_logical left right Logical.And loc) loc
        | _  -> lloc, left
      and logical_or env left lloc =
        match Peek.token env with
        | T_OR ->
            Expect.token env T_OR;
            let rloc, right = with_loc binary env in
            let rloc, right = logical_and env right rloc in
            let loc = Loc.btwn lloc rloc in
            logical_or env (make_logical left right Logical.Or loc) loc
        | _ -> lloc, left
      in fun env ->
        let loc, left = with_loc binary env in
        let loc, left = logical_and env left loc in
        let _, type_ = logical_or env left loc in
        type_

    and binary =
      let binary_op env =
        let ret = Expression.Binary.(match Peek.token env with
        (* Most BinaryExpression operators are left associative *)
        (* Lowest pri *)
        | T_BIT_OR -> Some (BitOr, Left_assoc 2)
        | T_BIT_XOR -> Some (Xor, Left_assoc 3)
        | T_BIT_AND -> Some (BitAnd, Left_assoc 4)
        | T_EQUAL -> Some (Equal, Left_assoc 5)
        | T_STRICT_EQUAL -> Some (StrictEqual, Left_assoc 5)
        | T_NOT_EQUAL -> Some (NotEqual, Left_assoc 5)
        | T_STRICT_NOT_EQUAL -> Some (StrictNotEqual, Left_assoc 5)
        | T_LESS_THAN -> Some (LessThan, Left_assoc 6)
        | T_LESS_THAN_EQUAL -> Some (LessThanEqual, Left_assoc 6)
        | T_GREATER_THAN -> Some (GreaterThan, Left_assoc 6)
        | T_GREATER_THAN_EQUAL -> Some (GreaterThanEqual, Left_assoc 6)
        | T_IN ->
            if (no_in env) then None else Some (In, Left_assoc 6)
        | T_INSTANCEOF -> Some (Instanceof, Left_assoc 6)
        | T_LSHIFT -> Some (LShift, Left_assoc 7)
        | T_RSHIFT -> Some (RShift, Left_assoc 7)
        | T_RSHIFT3 -> Some (RShift3, Left_assoc 7)
        | T_PLUS -> Some (Plus, Left_assoc 8)
        | T_MINUS -> Some (Minus, Left_assoc 8)
        | T_MULT -> Some (Mult, Left_assoc 9)
        | T_DIV -> Some (Div, Left_assoc 9)
        | T_MOD -> Some (Mod, Left_assoc 9)
        | T_EXP -> Some (Exp, Right_assoc 10)
        (* Highest priority *)
        | _ -> None)
        in if ret <> None then Eat.token env;
        ret

      in let make_binary left right operator loc =
        loc, Expression.(Binary Binary.({
          operator;
          left;
          right;
        }))

      in let rec add_to_stack right (rop, rpri) rloc = function
        | (left, (lop, lpri), lloc)::rest when is_tighter lpri rpri ->
            let loc = Loc.btwn lloc rloc in
            let right = make_binary left right lop loc in
            add_to_stack right (rop, rpri) loc rest
        | stack -> (right, (rop, rpri), rloc)::stack

      in let rec collapse_stack right rloc = function
        | [] -> right
        | (left, (lop, _), lloc)::rest ->
            let loc = Loc.btwn lloc rloc in
            collapse_stack (make_binary left right lop loc) loc rest

      in let rec helper env stack =
        let start_loc = Peek.loc env in
        let is_unary = peek_unary_op env <> None in
        let right = unary (env |> with_no_in false) in
        let end_loc = match last_loc env with
        | Some loc -> loc
        | None -> fst right
        in
        let right_loc = Loc.btwn start_loc end_loc in
        if Peek.token env = T_LESS_THAN
        then begin
          match right with
          | _, Expression.JSXElement _ ->
              error env Error.AdjacentJSXElements
          | _ -> ()
        end;
        match binary_op env with
        | None ->
          collapse_stack right right_loc stack
        | Some (rop, rpri) ->
          if is_unary && rop = Expression.Binary.Exp then
            error_at env (right_loc, Error.InvalidLHSInExponentiation);
          helper env (add_to_stack right (rop, rpri) right_loc stack)

      in fun env -> helper env []

    and peek_unary_op env =
      let open Expression.Unary in
      match Peek.token env with
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
      | _ -> None

    and unary env =
      let begin_loc = Peek.loc env in
      let op = peek_unary_op env in
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
              | _, Expression.Identifier (_, name)
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
      if Peek.is_line_terminator env
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
          | _, Expression.Identifier (_, name)
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
      let start_loc = Peek.loc env in
      let allow_new = not (no_new env) in
      let env = with_no_new false env in
      let expr = match Peek.token env with
      | T_NEW when allow_new -> new_expression env
      | _ when Peek.is_function env -> _function env
      | _ -> primary env in
      let expr = member env start_loc expr in
      match Peek.token env with
      | T_LPAREN -> call env start_loc expr
      | T_TEMPLATE_PART part ->
          member env start_loc (tagged_template env start_loc expr part)
      | _ -> expr

    and call env start_loc left =
      match Peek.token env with
      | T_LPAREN when not (no_call env) ->
          let args_loc, arguments = arguments env in
          let loc = Loc.btwn start_loc args_loc in
          call env start_loc (loc, Expression.(Call Call.({
            callee = left;
            arguments;
          })))
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression env in
          let last_loc = Peek.loc env in
          let loc = Loc.btwn start_loc last_loc in
          Expect.token env T_RBRACKET;
          call env start_loc (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          let loc = Loc.btwn start_loc (fst id) in
          call env start_loc (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | T_TEMPLATE_PART part -> tagged_template env start_loc left part
      | _ -> left

    and new_expression env =
      let start_loc = Peek.loc env in
      Expect.token env T_NEW;

      if in_function env && Peek.token env = T_PERIOD then begin
        Expect.token env T_PERIOD;
        let meta = start_loc, "new" in
        if Peek.value env = "target" then
          let property = Parse.identifier env in
          let end_loc = fst property in
          Loc.btwn start_loc end_loc, Expression.(MetaProperty MetaProperty.({
            meta;
            property;
          }))
        else begin
          error_unexpected env;
          Eat.token env; (* skip unknown identifier *)
          start_loc, Expression.Identifier meta (* return `new` identifier *)
        end
      end else
        let callee_loc = Peek.loc env in
        let expr = match Peek.token env with
        | T_NEW -> new_expression env
        | _ when Peek.is_function env -> _function env
        | _ -> primary env in
        let callee = member (env |> with_no_call true) callee_loc expr in
        (* You can do something like
         *   new raw`42`
         *)
        let callee = match Peek.token env with
        | T_TEMPLATE_PART part -> tagged_template env callee_loc callee part
        | _ -> callee in
        let end_loc, arguments = match Peek.token env with
        | T_LPAREN -> arguments env
        | _ -> fst callee, [] in

        Loc.btwn start_loc end_loc, Expression.(New New.({
          callee;
          arguments;
        }))

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

    and member env start_loc left =
      match Peek.token env with
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          let expr = Parse.expression (env |> with_no_call false) in
          let last_loc = Peek.loc env in
          Expect.token env T_RBRACKET;
          let loc = Loc.btwn start_loc last_loc in
          call env start_loc (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyExpression expr;
            computed = true;
          })))
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          let id, _ = identifier_or_reserved_keyword env in
          let loc = Loc.btwn start_loc (fst id) in
          call env start_loc (loc, Expression.(Member Member.({
            _object  = left;
            property = PropertyIdentifier id;
            computed = false;
          })))
      | _ -> left

    and _function env =
      let start_loc = Peek.loc env in
      let async = Declaration.async env in
      Expect.token env T_FUNCTION;
      let generator = Declaration.generator env in
      let id, typeParameters =
        if Peek.token env = T_LPAREN
        then None, None
        else begin
          let id = match Peek.token env with
            | T_LESS_THAN -> None
            | _ -> Some (Parse.identifier ~restricted_error:Error.StrictFunctionName env) in
          id, Type.type_parameter_declaration env
        end in
      let params = Declaration.function_params env in
      let returnType, predicate = Type.annotation_and_predicate_opt env in
      let end_loc, body, strict =
        Declaration.function_body env ~async ~generator in
      let simple = Declaration.is_simple_function_params params in
      Declaration.strict_post_check env ~strict ~simple id params;
      let expression = Function.(
        match body with
        | BodyBlock _ -> false
        | BodyExpression _ -> true) in
      Loc.btwn start_loc end_loc, Expression.(Function Function.({
        id;
        params;
        body;
        generator;
        async;
        predicate;
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
        try Lexer_flow.FloatOfString.float_of_string value
        with _ when Sys.win32 ->
          error env Parse_error.WindowsFloatOfString;
          789.0
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
      | T_DIV
      | T_DIV_ASSIGN -> regexp env
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
          loc, Expression.Super
      | _ when Peek.is_identifier env ->
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
            Eat.push_lex_mode env Lex_mode.TEMPLATE;
            let loc, part, is_tail = match Peek.token env with
            | T_TEMPLATE_PART (loc, {cooked; raw; _}, tail) ->
                let open Ast.Expression.TemplateLiteral in
                Eat.token env;
                loc, { Element.value = { Element.cooked; raw; }; tail; }, tail
            | _ -> assert false in
            Eat.pop_lex_mode env;
            let quasis = (loc, part)::quasis in
            if is_tail
            then loc, List.rev quasis, List.rev expressions
            else template_parts env quasis expressions
        | _ ->
            (* Malformed template *)
            error_unexpected env;
            let imaginary_quasi = fst expr, { Expression.TemplateLiteral.Element.
              value = { Expression.TemplateLiteral.Element.
                raw = "";
                cooked = "";
              };
              tail = true;
            } in
            fst expr, List.rev (imaginary_quasi::quasis), List.rev expressions

      in fun env ((start_loc, {cooked; raw; _}, is_tail) as part) ->
        Expect.token env (T_TEMPLATE_PART part);
        let end_loc, quasis, expressions =
          let head = Ast.Expression.TemplateLiteral.(start_loc, {
            Element.value = { Element.cooked; raw; };
            tail = is_tail;
          }) in
          if is_tail
          then start_loc, [head], []
          else template_parts env [head] [] in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.TemplateLiteral.({
          quasis;
          expressions;
        })

    and tagged_template env start_loc tag part =
      let quasi = template_literal env part in
      Loc.btwn start_loc (fst quasi), Expression.(TaggedTemplate TaggedTemplate.({
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
            if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
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

    and regexp env =
      Eat.push_lex_mode env Lex_mode.REGEXP;
      let loc = Peek.loc env in
      let raw, pattern, raw_flags = match Peek.token env with
        | T_REGEXP (_, pattern, flags) ->
            let raw = Peek.value env in
            Eat.token env;
            raw, pattern, flags
        | _ -> assert false in
      Eat.pop_lex_mode env;
      let filtered_flags = Buffer.create (String.length raw_flags) in
      String.iter (function
        | 'g' | 'i' | 'm' | 'u' | 'y' as c -> Buffer.add_char filtered_flags c
        | _ -> ()) raw_flags;
      let flags = Buffer.contents filtered_flags in
      if flags <> raw_flags
      then error env (Error.InvalidRegExpFlags raw_flags);
      let value = Literal.(RegExp { RegExp.pattern; flags; }) in
      loc, Expression.(Literal { Literal.value; raw; })

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
        let params, returnType, predicate =
          (* Disallow all fancy features for identifier => body *)
          if Peek.is_identifier env && typeParameters = None
          then
            let loc, name =
              Parse.identifier ~restricted_error:Error.StrictParamName env in
            let param = loc, Pattern.Identifier {
              Pattern.Identifier.name = loc, name;
                                 typeAnnotation=None;
                                 optional=false;
            } in
            ([param], None), None, None
          else
            let params = Declaration.function_params env in
            (* There's an ambiguity if you use a function type as the return
             * type for an arrow function. So we disallow anonymous function
             * types in arrow function return types unless the function type is
             * enclosed in parens *)
            let returnType, predicate = env
              |> with_no_anon_function_type true
              |> Type.annotation_and_predicate_opt in
            params, returnType, predicate in

        (* It's hard to tell if an invalid expression was intended to be an
         * arrow function before we see the =>. If there are no params, that
         * implies "()" which is only ever found in arrow params. Similarly,
         * rest params indicate arrow functions. Therefore, if we see a rest
         * param or an empty param list then we can disable the rollback and
         * instead generate errors as if we were parsing an arrow function *)
        let env = match params with
          | _, Some _
          | [], _ -> without_error_callback env
          | _ -> env
        in

        if Peek.is_line_terminator env && Peek.token env = T_ARROW
        then error env Error.NewlineBeforeArrow;
        Expect.token env T_ARROW;

        (* Now we know for sure this is an arrow function *)
        let env = without_error_callback env in

        let end_loc, (body, strict) = with_loc
          (Declaration.concise_function_body ~async ~generator:false)
          env
        in
        let simple = Declaration.is_simple_function_params params in
        Declaration.strict_post_check env ~strict ~simple None params;
        let expression = Function.(
          match body with
          | BodyBlock _ -> false
          | BodyExpression _ -> true) in
        let loc = Loc.btwn start_loc end_loc in
        loc, Expression.(ArrowFunction Function.({
          id = None;
          params;
          body;
          async;
          generator = false; (* arrow functions cannot be generators *)
          predicate;
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
      let lex_token = Peek.token env in
      let lex_value = Peek.value env in
      let lex_loc = Peek.loc env in
      match lex_token with
      (* Anything that is a special token in Flow but not in the ES6 spec
         should be here. *)
      | T_ASYNC
      | T_DECLARE
      | T_IDENTIFIER
      | T_OF
      | T_TYPE
        -> Parse.identifier env, None
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
        | T_ANY_TYPE
        | T_BOOLEAN_TYPE
        | T_NUMBER_TYPE
        | T_STRING_TYPE
        | T_VOID_TYPE
        | T_AWAIT
        | T_DEBUGGER ->
            Some (lex_loc, get_unexpected_error (lex_token, lex_value))
        | _ ->
            error_unexpected env;
            None
        in
        Eat.token env;
        (lex_loc, lex_value), err
  end

  (* A module for parsing various object related things, like object literals
   * and classes *)
  module Object : sig
    val key : env -> Loc.t * Ast.Expression.Object.Property.key
    val _initializer : env -> Loc.t * Ast.Expression.Object.t
    val class_declaration : env -> Ast.Expression.t list -> Ast.Statement.t
    val class_expression : env -> Ast.Expression.t
    val decorator_list : env -> Ast.Expression.t list
  end = struct
    let decorator_list =
      let rec decorator_list_helper env decorators =
        match Peek.token env with
        | T_AT ->
            Eat.token env;
            decorator_list_helper env ((Expression.left_hand_side env)::decorators)
        | _ ->
            decorators

      in fun env ->
        if (parse_options env).esproposal_decorators
        then List.rev (decorator_list_helper env [])
        else []

    let key env =
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
      | T_LBRACKET ->
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
      let generator = Declaration.generator env in
      let key_loc, key = key env in
      let start_loc = Peek.loc env in
      (* It's not clear how type params on getters & setters would make sense
       * in Flow's type system. Since this is a Flow syntax extension, we might
       * as well disallow it until we need it *)
      let typeParameters = Ast.Expression.Object.Property.(match kind with
      | Get | Set -> None
      | _ -> Type.type_parameter_declaration env) in
      let params = Declaration.function_params env in
      Ast.Expression.Object.Property.(match kind, params with
      | Get, ([], None) -> ()
      | Set, ([(_, Pattern.Assignment _)], None) ->
          (* defaults don't make sense on a setter *)
          error_at env (key_loc, Error.SetterArity)
      | Set, (_, Some _rest) ->
          (* rest params don't make sense on a setter *)
          error_at env (key_loc, Error.SetterArity)
      | Set, ([_], _) -> ()
      | Get, _ -> error_at env (key_loc, Error.GetterArity)
      | Set, _ -> error_at env (key_loc, Error.SetterArity)
      | Init, _ -> ());
      let returnType = Type.annotation_opt env in
      let _, body, strict = Declaration.function_body env ~async ~generator in
      let simple = Declaration.is_simple_function_params params in
      Declaration.strict_post_check env ~strict ~simple None params;
      let end_loc, expression = Function.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      let loc = Loc.btwn start_loc end_loc in
      let value = loc, Function.({
        id = None;
        params;
        body;
        generator;
        async;
        predicate = None; (* setters/getter are not predicates *)
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
          let async =
            Peek.is_literal_property_name ~i:1 env && Declaration.async env in
          Property (match async , Declaration.generator env, key env with
          | false, false, (_, (Property.Identifier (_, "get") as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN
              | T_COMMA
              | T_RCURLY -> init env start_loc key false false
              | _ -> get env start_loc)
          | false, false, (_, (Property.Identifier (_, "set") as key)) ->
              (match Peek.token env with
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN
              | T_COMMA
              | T_RCURLY -> init env start_loc key false false
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
                let start_loc = Peek.loc env in
                let typeParameters = Type.type_parameter_declaration env in
                let params = Declaration.function_params env in
                let returnType = Type.annotation_opt env in
                let _, body, strict =
                  Declaration.function_body env ~async ~generator in
                let simple = Declaration.is_simple_function_params params in
                Declaration.strict_post_check env ~strict ~simple None params;
                let end_loc, expression = match body with
                  | Function.BodyBlock (loc, _) -> loc, false
                  | Function.BodyExpression (loc, _) -> loc, true
                in
                let loc = Loc.btwn start_loc end_loc in
                let value = loc, Ast.Expression.(Function Function.({
                  id = None;
                  params;
                  body;
                  generator;
                  async;
                  (* TODO: add support for object method predicates *)
                  predicate = None;
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

      and properties env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let prop = property env in
            if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
            properties env (prop::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let props = properties env [] in
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

      in let error_unsupported_variance env = function
      | Some (loc, _) -> error_at env (loc, Error.UnexpectedVariance)
      | None -> ()

      in let rec init env start_loc decorators key async generator static variance =
        match Peek.token env with
        | T_COLON
        | T_ASSIGN
        | T_SEMICOLON when not async && not generator ->
          (* Class property with annotation *)
          let typeAnnotation = Type.annotation_opt env in
          let options = parse_options env in
          let value =
            if Peek.token env = T_ASSIGN then (
              if static && options.esproposal_class_static_fields
                 || (not static) && options.esproposal_class_instance_fields
              then begin
                Expect.token env T_ASSIGN;
                Some (Parse.expression env)
              end else None
            ) else None in
          let end_loc = Peek.loc env in
          if Expect.maybe env T_SEMICOLON then () else begin
            if Peek.token env == T_LBRACKET || Peek.token env == T_LPAREN then error_unexpected env
          end;
          let loc = Loc.btwn start_loc end_loc in
          Ast.Class.(Body.Property (loc, Property.({
            key;
            value;
            typeAnnotation;
            static;
            variance;
          })))
        | T_PLING ->
          (* TODO: add support for optional class properties *)
          error_unexpected env;
          Eat.token env;
          init env start_loc decorators key async generator static variance
        | _ ->
          error_unsupported_variance env variance;
          let func_loc = Peek.loc env in
          let typeParameters = Type.type_parameter_declaration env in
          let params = Declaration.function_params env in
          let returnType = Type.annotation_opt env in
          let _, body, strict =
            Declaration.function_body env ~async ~generator in
          let simple = Declaration.is_simple_function_params params in
          Declaration.strict_post_check env ~strict ~simple None params;
          let end_loc, expression = Function.(
            match body with
            | BodyBlock (loc, _) -> loc, false
            | BodyExpression (loc, _) -> loc, true) in
          let loc = Loc.btwn func_loc end_loc in
          let value = loc, Function.({
            id = None;
            params;
            body;
            generator;
            async;
            (* TODO: add support for method predicates *)
            predicate = None;
            expression;
            returnType;
            typeParameters;
          }) in
          let kind = Ast.(match key with
            | Expression.Object.Property.Identifier (_, "constructor")
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
            decorators;
          })))

      in fun env -> Ast.Expression.Object.Property.(
        let start_loc = Peek.loc env in
        let decorators = decorator_list env in
        let static =
          Peek.token ~i:1 env <> T_LPAREN &&
          Peek.token ~i:1 env <> T_LESS_THAN &&
          Expect.maybe env T_STATIC in
        let async =
          Peek.token ~i:1 env <> T_LPAREN &&
          Peek.token ~i:1 env <> T_COLON &&
          Declaration.async env in
        let generator = Declaration.generator env in
        let variance = Declaration.variance env async generator in
        let generator = match generator, variance with
        | false, Some _ -> Declaration.generator env
        | _ -> generator
        in
        match (async, generator, key env) with
        | false, false,
          (_, (Identifier (_, "get") as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN ->
              init env start_loc decorators key async generator static variance
            | _ ->
              error_unsupported_variance env variance;
              get env start_loc decorators static)
        | false, false,
          (_, (Identifier (_, "set") as key)) ->
            (match Peek.token env with
            | T_LESS_THAN
            | T_COLON
            | T_ASSIGN
            | T_SEMICOLON
            | T_LPAREN ->
              init env start_loc decorators key async generator static variance
            | _ ->
              error_unsupported_variance env variance;
              set env start_loc decorators static)
        | _, _, (_, key) ->
            init env start_loc decorators key async generator static variance
      )

    let class_declaration env decorators =
      (* 10.2.1 says all parts of a class definition are strict *)
      let env = env |> with_strict true in
      let start_loc = Peek.loc env in
      let decorators = decorators @ (decorator_list env) in
      Expect.token env T_CLASS;
      let tmp_env = env |> with_no_let true in
      let id = (
        match in_export env, Peek.is_identifier tmp_env with
        | true, false -> None
        | _ -> Some(Parse.identifier tmp_env)
      ) in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      let body, superClass, superTypeParameters, implements = _class env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Ast.Statement.(ClassDeclaration Class.({
        id;
        body;
        superClass;
        typeParameters;
        superTypeParameters;
        implements;
        classDecorators=decorators;
      }))

    let class_expression env =
      let start_loc = Peek.loc env in
      let decorators = decorator_list env in
      Expect.token env T_CLASS;
      let id, typeParameters = match Peek.token env with
        | T_EXTENDS
        | T_LESS_THAN
        | T_LCURLY -> None, None
        | _ ->
            let id = Some (Parse.identifier env) in
            let typeParameters = Type.type_parameter_declaration_with_defaults env in
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
        classDecorators=decorators;
      }))
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
    val declare_export_declaration: ?allow_export_type:bool -> env -> Ast.Statement.t
    val do_while: env -> Ast.Statement.t
    val empty: env -> Ast.Statement.t
    val export_declaration: env -> Ast.Expression.t list -> Ast.Statement.t
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
          let (_, name) as label =
            Parse.identifier env in
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
          let (_, name) as label =
            Parse.identifier env in
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
        let async = allow_await env && Expect.maybe env T_AWAIT in
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
        (* If `async` is true, this must be a for-await-of loop. *)
        | t when t = T_OF || async ->
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
              async;
            }))
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
      | _ when Peek.is_function env ->
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
        }))

    and throw env =
      let start_loc = Peek.loc env in
      Expect.token env T_THROW;
      if Peek.is_line_terminator env
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
          let param = fst id, Pattern.Identifier {
            Pattern.Identifier.name=id;
                               typeAnnotation=None;
                               optional=false;
          } in
          Expect.token env T_RPAREN;
          let body = Parse.block_body env in
          let loc = Loc.btwn start_loc (fst body) in
          Some (loc, Ast.Statement.Try.CatchClause.({
            param;
            body;
          }))
      | _ -> None in
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
          let _, name = label in
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
      let loc, expression = with_loc Parse.expression env in
      let loc = match Peek.semicolon_loc env with
      | Some semicolon_loc -> Loc.btwn loc semicolon_loc
      | None -> loc in
      Eat.semicolon env;
      loc, Statement.(Expression Expression.({
        expression;
      }))

    and type_alias_helper env =
      let start_loc = Peek.loc env in
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAlias;
      Expect.token env T_TYPE;
      Eat.push_lex_mode env Lex_mode.TYPE;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
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
      if Peek.is_identifier ~i:1 env
      then
        let loc, type_alias = type_alias_helper env in
        loc, Statement.TypeAlias type_alias
      else
        Parse.statement env

    and interface_helper =
      let rec supers env acc =
        let super = Type.generic env in
        let acc = super::acc in
        match Peek.token env with
        | T_COMMA ->
            Expect.token env T_COMMA;
            supers env acc
        | _ -> List.rev acc
      in
      fun env ->
        let start_loc = Peek.loc env in
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeInterface;
        Expect.token env T_INTERFACE;
        let id = Parse.identifier env in
        let typeParameters = Type.type_parameter_declaration_with_defaults env in
        let extends = if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          supers env []
        end else [] in
        let body = Type._object ~allow_static:true env in
        let loc = Loc.btwn start_loc (fst body) in
        loc, Statement.Interface.({
          id;
          typeParameters;
          body;
          extends;
          mixins = [];
        })


    and interface env =
      if Peek.is_identifier ~i:1 env
      then
        let loc, iface = interface_helper env in
        loc, Statement.InterfaceDeclaration iface
      else expression env

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
        let typeParameters = Type.type_parameter_declaration_with_defaults env in
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
      let params = Type.function_param_list env in
      Expect.token env T_COLON;
      let returnType = Type._type env in
      let end_loc = fst returnType in
      let predicate = Type.predicate_opt env in
      let loc = Loc.btwn start_sig_loc end_loc in
      let typeAnnotation = loc, Ast.Type.(Function {Function.
        params;
        returnType;
        typeParameters;
      }) in
      let typeAnnotation = fst typeAnnotation, typeAnnotation in
      let id = Loc.btwn (fst id) end_loc, snd id in
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
      | Some end_loc -> end_loc in
      Eat.semicolon env;
      let loc = Loc.btwn start_loc end_loc in
      loc, Statement.DeclareFunction.({
        id;
        typeAnnotation;
        predicate;
      })

    and declare_function_statement env start_loc =
      let loc, fn = declare_function env start_loc in
      loc, Statement.DeclareFunction fn

    and declare_var env start_loc =
      Expect.token env T_VAR;
      let loc, { Pattern.Identifier.name; typeAnnotation; _; } =
        Parse.identifier_with_type env ~no_optional:true Error.StrictVarName in
      let end_loc = match Peek.semicolon_loc env with
      | None -> loc
      | Some loc -> loc in
      let loc = Loc.btwn start_loc end_loc in
      Eat.semicolon env;
      loc, Statement.DeclareVariable.({ id=name; typeAnnotation; })

    and declare_var_statement env start_loc =
      let loc, var = declare_var env start_loc in
      loc, Statement.DeclareVariable var

    and declare_module =
      let rec module_items env ~module_kind acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> (module_kind, List.rev acc)
        | _ ->
          let stmt = declare ~in_module:true env in
          let module_kind = Statement.(
            let (loc, stmt) = stmt in
            match (module_kind, stmt) with
            (**
             * The first time we see either a `declare export` or a
             * `declare module.exports`, we lock in the kind of the module.
             *
             * `declare export type` and `declare export interface` are the two
             * exceptions to this rule because they are valid in both CommonJS
             * and ES modules (and thus do not indicate an intent for either).
             *)
            | None, DeclareModuleExports _ -> Some (DeclareModule.CommonJS loc)
            | None, DeclareExportDeclaration {
                DeclareExportDeclaration.declaration;
                _;
              } ->
              (match declaration with
                | Some (DeclareExportDeclaration.NamedType _)
                | Some (DeclareExportDeclaration.Interface _)
                  -> module_kind
                | _ -> Some (DeclareModule.ES loc)
              )

            (**
             * There should never be more than one `declare module.exports`
             * statement *)
            | Some (DeclareModule.CommonJS _), DeclareModuleExports _ ->
              error env Parse_error.DuplicateDeclareModuleExports;
              module_kind

            (**
             * It's never ok to mix and match `declare export` and
             * `declare module.exports` in the same module because it leaves the
             * kind of the module (CommonJS vs ES) ambiguous.
             *
             * The 1 exception to this rule is that `export type/interface` are
             * both ok in CommonJS modules.
             *)
            | Some (DeclareModule.ES _), DeclareModuleExports _ ->
              error env Parse_error.AmbiguousDeclareModuleKind;
              module_kind
            | Some (DeclareModule.CommonJS _), DeclareExportDeclaration {
                DeclareExportDeclaration.declaration;
                _;
              } ->
                (match declaration with
                  | Some (DeclareExportDeclaration.NamedType _)
                  | Some (DeclareExportDeclaration.Interface _)
                    -> ()
                  | _ -> error env Parse_error.AmbiguousDeclareModuleKind
                );
                module_kind

            | _ -> module_kind
          ) in
          module_items env ~module_kind (stmt::acc)

      in fun env start_loc ->
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
        let (module_kind, body) = module_items env ~module_kind:None [] in
        Expect.token env T_RCURLY;
        let body_end_loc = Peek.loc env in
        let body_loc = Loc.btwn body_start_loc body_end_loc in
        let body = body_loc, { Statement.Block.body; } in
        let loc = Loc.btwn start_loc (fst body) in
        let kind =
          match module_kind with
          | Some k -> k
          | None -> Statement.DeclareModule.CommonJS loc
        in
        loc,
        Statement.(DeclareModule DeclareModule.({ id; body; kind; }))

    and declare_module_exports env start_loc =
      Expect.token env T_PERIOD;
      Expect.contextual env "exports";
      let type_annot = Type.annotation env in
      let end_loc =
        match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> fst type_annot
      in
      Eat.semicolon env;
      let loc = Loc.btwn start_loc end_loc in
      (loc, Statement.DeclareModuleExports type_annot)

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
        | T_EXPORT when in_module ->
            declare_export_declaration ~allow_export_type:in_module env
        | T_IDENTIFIER when Peek.value ~i:1 env = "module" ->
            Expect.token env T_DECLARE;
            Expect.contextual env "module";
            if in_module || Peek.token env = T_PERIOD
            then declare_module_exports env start_loc
            else declare_module env start_loc
        | _ when in_module ->
            (* Oh boy, found some bad stuff in a declare module. Let's just
              * pretend it's a declare var (arbitrary choice) *)
            Expect.token env T_DECLARE;
            declare_var_statement env start_loc
        | _ ->
            Parse.statement env
      )

    and export_source env =
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
          let ret = Peek.loc env, { Literal.value; raw; } in
          error_unexpected env;
          ret

    and extract_pattern_binding_names =
      let rec fold acc = Pattern.(function
        | (_, Object {Object.properties; _;}) ->
          List.fold_left (fun acc prop ->
            match prop with
            | Object.Property (_, {Object.Property.pattern; _;})
            | Object.RestProperty (_, {Object.RestProperty.argument = pattern;})
              -> fold acc pattern
          ) acc properties
        | (_, Array {Array.elements; _;}) ->
          List.fold_left Array.(fun acc elem ->
            match elem with
            | Some (Element pattern)
            | Some (RestElement (_, {RestElement.argument = pattern;}))
              -> fold acc pattern
            | None -> acc
          ) acc elements
        | (_, Assignment {Assignment.left;_;}) -> fold acc left
        | (_, Identifier {Pattern.Identifier.name; _; }) ->
          name::acc
        | (_, Expression _) ->
          failwith "Parser error: No such thing as an expression pattern!"
      ) in
      List.fold_left fold

    and extract_ident_name (_, name) = name

    and export_specifiers_and_errs env specifiers errs =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
          List.rev specifiers, List.rev errs
      | _ ->
          let local, err = Parse.identifier_or_reserved_keyword env in
          let exported, err, end_loc = if Peek.value env = "as"
          then begin
            Expect.contextual env "as";
            let name, _ = Parse.identifier_or_reserved_keyword env in
            (record_export env (fst name, extract_ident_name name));
            Some name, None, fst name
          end else begin
            let loc = fst local in
            record_export env (loc, extract_ident_name local);
            None, err, loc
          end in
          let loc = Loc.btwn (fst local) end_loc in
          let specifier = loc, {
            Statement.ExportNamedDeclaration.ExportSpecifier.local;
            exported;
          } in
          if Peek.token env = T_COMMA
          then Expect.token env T_COMMA;
          let errs = match err with
          | Some err -> err::errs
          | None -> errs in
          export_specifiers_and_errs env (specifier::specifiers) errs

    and export_declaration env decorators =
      let env = env |> with_strict true |> with_in_export true in
      let start_loc = Peek.loc env in
      Expect.token env T_EXPORT;
      match Peek.token env with
      | T_DEFAULT ->
          (* export default ... *)
          let open Statement.ExportDefaultDeclaration in
          Expect.token env T_DEFAULT;
          record_export env (Loc.btwn start_loc (Peek.loc env), "default");
          let end_loc, declaration = match Peek.token env with
          | T_FUNCTION ->
              (* export default function foo (...) { ... } *)
              let fn = Declaration._function env in
              fst fn, Declaration fn
          | _ when Peek.is_class env ->
              (* export default class foo { ... } *)
              let _class = Object.class_declaration env decorators in
              fst _class, Declaration _class
          | _ ->
              (* export default [assignment expression]; *)
              let expr = Parse.assignment env in
              let end_loc = match Peek.semicolon_loc env with
              | Some loc -> loc
              | None -> fst expr in
              Eat.semicolon env;
              end_loc, Expression expr
            in
          Loc.btwn start_loc end_loc, Statement.ExportDefaultDeclaration {
            declaration;
            exportKind = Statement.ExportValue;
          }
      | T_TYPE when (Peek.token env ~i:1) <> T_LCURLY ->
          (* export type ... *)
          let open Statement.ExportNamedDeclaration in
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeExport;
          let type_alias = type_alias env in
          (match type_alias with
            | (loc, Statement.TypeAlias {Statement.TypeAlias.id; _;}) ->
              record_export env (loc, extract_ident_name id)
            | _ -> failwith (
                "Internal Flow Error! Parsed `export type` into something " ^
                "other than a type alias!"
              )
          );
          let end_loc = fst type_alias in
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = Some type_alias;
            specifiers = None;
            source = None;
            exportKind = Statement.ExportType;
          }
      | T_INTERFACE ->
          (* export interface I { ... } *)
          let open Statement.ExportNamedDeclaration in
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeExport;
          let interface = interface env in
          (match interface with
            | (loc, Statement.InterfaceDeclaration {Statement.Interface.id; _;}) ->
              record_export env (loc, extract_ident_name id)
            | _ -> failwith (
                "Internal Flow Error! Parsed `export interface` into something " ^
                "other than an interface declaration!"
              )
          );
          let end_loc = fst interface in
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = Some interface;
            specifiers = None;
            source = None;
            exportKind = Statement.ExportType;
          }
      | T_LET
      | T_CONST
      | T_VAR
      (* not using Peek.is_class here because it would guard all of the
        * cases *)
      | T_AT
      | T_CLASS
      (* not using Peek.is_function here because it would guard all of the
        * cases *)
      | T_ASYNC
      | T_FUNCTION ->
          let open Statement.ExportNamedDeclaration in
          let stmt = Parse.statement_list_item env ~decorators:decorators in
          let names = Statement.(
            match stmt with
            | (_, VariableDeclaration { VariableDeclaration.declarations; _; }) ->
              List.fold_left (fun names (_, declaration) ->
                let id = declaration.VariableDeclaration.Declarator.id in
                extract_pattern_binding_names names [id]
              ) [] declarations
            | (loc, ClassDeclaration { Class.id = Some id; _; })
            | (loc, FunctionDeclaration { Function.id = Some id; _; })
              -> [(loc, extract_ident_name id)]
            | (loc, ClassDeclaration { Class.id = None; _; }) ->
              error_at env (loc, Error.ExportNamelessClass);
              []
            | (loc, FunctionDeclaration { Function.id = None; _; }) ->
              error_at env (loc, Error.ExportNamelessFunction);
              []
            | _ -> failwith "Internal Flow Error! Unexpected export statement declaration!"
          ) in
          List.iter (record_export env) names;
          Loc.btwn start_loc (fst stmt), Statement.ExportNamedDeclaration {
            declaration = Some stmt;
            specifiers = None;
            source = None;
            exportKind = Statement.ExportValue;
          }
      | T_MULT ->
          let open Statement.ExportNamedDeclaration in
          let loc = Peek.loc env in
          Expect.token env T_MULT;
          let local_name =
            let parse_export_star_as =
              (parse_options env).esproposal_export_star_as
            in
            if Peek.value env = "as"
            then (
              Expect.contextual env "as";
              if parse_export_star_as
              then Some (Parse.identifier env)
              else (error env Error.UnexpectedTypeDeclaration; None)
            ) else None
          in
          let specifiers =
            Some (ExportBatchSpecifier (loc, local_name))
          in
          let source = export_source env in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          let source = Some source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = None;
            specifiers;
            source;
            exportKind = Statement.ExportValue;
          }
      | _ ->
          let open Statement.ExportNamedDeclaration in
          let exportKind = (
            match Peek.token env with
            | T_TYPE -> Eat.token env; Statement.ExportType
            | _ -> Statement.ExportValue
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
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = None;
            specifiers;
            source;
            exportKind;
          }

    and declare_export_declaration ?(allow_export_type=false) env =
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
          Expect.token env T_MULT;
          let parse_export_star_as =
            (parse_options env).esproposal_export_star_as
          in
          let local_name =
            if Peek.value env = "as"
            then (
              Expect.contextual env "as";
              if parse_export_star_as
              then Some (Parse.identifier env)
              else (error env Error.UnexpectedTypeDeclaration; None)
            ) else None
          in
          let specifiers = Statement.ExportNamedDeclaration.(
            Some (ExportBatchSpecifier (loc, local_name))
          ) in
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
      | T_TYPE when allow_export_type ->
          (* declare export type = ... *)
          let (alias_loc, alias) = type_alias_helper env in
          let loc = Loc.btwn start_loc alias_loc in
          (loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = Some (NamedType (alias_loc, alias));
            specifiers = None;
            source = None;
          })
      | T_INTERFACE when allow_export_type ->
          (* declare export interface ... *)
          let (iface_loc, iface) = interface_helper env in
          let loc = Loc.btwn start_loc iface_loc in
          (loc, Statement.DeclareExportDeclaration {
            default = false;
            declaration = Some (Interface (iface_loc, iface));
            specifiers = None;
            source = None;
          })
      | _ ->
          (match Peek.token env with
            | T_TYPE -> error env Error.DeclareExportType
            | T_INTERFACE -> error env Error.DeclareExportInterface
            | _ -> ()
          );
          Expect.token env T_LCURLY;
          let specifiers, errs = export_specifiers_and_errs env [] [] in
          let specifiers = Some (Statement.ExportNamedDeclaration.ExportSpecifiers specifiers) in
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
      let open Statement.ImportDeclaration in

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
            let ret = Peek.loc env, { Literal.value; raw; } in
            error_unexpected env;
            ret

      in let rec specifier_list env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let remote, err = Parse.identifier_or_reserved_keyword env in
            let specifier =
              if Peek.value env = "as" then begin
                Expect.contextual env "as";
                let local = Some (Parse.identifier env) in
                ImportNamedSpecifier { local; remote; }
              end else begin
                (match err with Some err -> error_at env err | None -> ());
                ImportNamedSpecifier { local = None; remote; }
              end
            in
            if Peek.token env = T_COMMA
            then Expect.token env T_COMMA;
            specifier_list env (specifier::acc)

      in let named_or_namespace_specifier env =
        let start_loc = Peek.loc env in
        match Peek.token env with
        | T_MULT ->
            Expect.token env T_MULT;
            Expect.contextual env "as";
            let id = Parse.identifier env in
            [ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)]
        | _ ->
            Expect.token env T_LCURLY;
            let specifiers = specifier_list env [] in
            Expect.token env T_RCURLY;
            specifiers

      in fun env ->
        let env = env |> with_strict true in
        let start_loc = Peek.loc env in
        Expect.token env T_IMPORT;
        (* It might turn out that we need to treat this "type" token as an
         * identifier, like import type from "module" *)
        let importKind, type_ident =
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
        in
        match Peek.token env, Peek.is_identifier env with
        (* import "ModuleName"; *)
        | T_STRING (str_loc, value, raw, octal), _
            when importKind = ImportValue ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (str_loc, value, raw, octal));
          let value = Literal.String value in
          let source = (str_loc, { Literal.value; raw; }) in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> str_loc in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
            importKind;
            source;
            specifiers = [];
          }

        (* import [type] SomeDefault ... *)
        | T_COMMA, _ (* `import type, ...` *)
        | _, true -> (* `import type Foo` or `import type from` *)
            let importKind, default_specifier = (
              match type_ident, Peek.token env, Peek.value env with
              | Some type_ident, T_COMMA, _ (* `import type,` *)
              | Some type_ident, T_IDENTIFIER, "from" -> (* `import type from` *)
                ImportValue, ImportDefaultSpecifier type_ident
              | _ -> (* Either `import type Foo` or `import Foo` *)
                importKind, ImportDefaultSpecifier (Parse.identifier env)
            ) in

            let additional_specifiers = (
              match Peek.token env with
              | T_COMMA -> (* `import Foo, ...` *)
                  Expect.token env T_COMMA;
                  named_or_namespace_specifier env
              | _ -> []
            ) in

            let source = source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
              importKind;
              source;
              specifiers = default_specifier::additional_specifiers;
            }

        (* `import [type] { ... } ...` or `import [typeof] * as ...` *)
        | _ ->
            let specifiers = named_or_namespace_specifier env in
            let source = source env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst source in
            let source = source in
            Eat.semicolon env;
            Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
              importKind;
              source;
              specifiers;
            }
  end

  module Pattern = struct
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
          let pattern = Parse.pattern_from_expr env value in
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
              let default = Parse.expression env in
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

  module JSX = struct
    let spread_attribute env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
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
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let expression = if Peek.token env = T_RCURLY
        then
          let empty_loc = Loc.btwn_exclusive start_loc (Peek.loc env) in
          JSX.ExpressionContainer.EmptyExpression empty_loc
        else JSX.ExpressionContainer.Expression (Parse.expression env) in
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
              begin
                let open JSX.ExpressionContainer in
                match expression_container.expression with
                | EmptyExpression _ ->
                    error_at env (loc, Error.JSXAttributeValueEmptyExpression);
                | _ -> ()
              end;
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
          Eat.push_lex_mode env Lex_mode.JSX_TAG;
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
              | MemberExpression.Identifier (_, {Identifier.name=id; _;}) -> id
              | MemberExpression.MemberExpression e ->
                  normalize (JSX.MemberExpression e) in
                  _object ^ "." ^ (snd property).Identifier.name
        )

        in fun env start_loc ->
          let openingElement = opening_element_without_lt env start_loc in
          let children, closingElement =
            if (snd openingElement).JSX.Opening.selfClosing
            then [], None
            else begin
              Eat.push_lex_mode env Lex_mode.JSX_CHILD;
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
        Eat.push_lex_mode env Lex_mode.JSX_TAG;
        Expect.token env T_LESS_THAN;
        element_without_lt env start_loc
  end

  let rec program env =
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    let loc = match stmts with
    | [] -> end_loc
    | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts))) in
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
                let strict =
                  (in_strict_mode env) ||
                  (str = "use strict" && len = 12)
                in
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
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration env decorators
    | T_IMPORT ->
        error_on_decorators env decorators;
        Statement.import_declaration env
    | T_DECLARE when Peek.token ~i:1 env = T_EXPORT ->
        error_on_decorators env decorators;
        Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

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
    stmts, (in_strict_mode env)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn ((statement_list_item env)::acc)

    in fun ~term_fn env -> statements env term_fn []


  and statement_list_item ?(decorators=[]) env =
    if not (Peek.is_class env)
    then error_on_decorators env decorators;
    Statement.(match Peek.token env with
    (* Remember kids, these look like statements but they're not
      * statements... (see section 13) *)
    | T_LET -> _let env
    | T_CONST -> var_or_const env
    | _ when Peek.is_function env -> Declaration._function env
    | _ when Peek.is_class env -> class_declaration env decorators
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
    | _ when Peek.is_identifier env -> maybe_labeled env
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

  and conditional = Expression.conditional
  and assignment = Expression.assignment
  and left_hand_side = Expression.left_hand_side
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
      if in_strict_mode env
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
    loc, name

  and identifier_or_reserved_keyword = Expression.identifier_or_reserved_keyword

  and identifier_with_type =
    let with_loc_helper no_optional restricted_error env =
      let name = identifier ~restricted_error env in
      let optional = not no_optional && Peek.token env = T_PLING in
      if optional then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        Expect.token env T_PLING
      end;
      let typeAnnotation =
        if Peek.token env = T_COLON
        then Some (Type.annotation env)
        else None in
      Ast.Pattern.Identifier.({
        name;
        optional;
        typeAnnotation;
      })

    in fun env ?(no_optional=false) restricted_error ->
      with_loc (with_loc_helper no_optional restricted_error) env

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
  and pattern_from_expr = Pattern.from_expr

end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  if fail && error_list <> []
  then raise (Error.Error error_list);
  ast, error_list

let parse_program fail ?(token_sink=None) ?(parse_options=None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail=true) ?(token_sink=None) ?(parse_options=None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  let env = init_env ~token_sink ~parse_options filename content in
  match Peek.token env with
  | T_LBRACKET
  | T_LCURLY
  | T_STRING _
  | T_NUMBER _
  | T_TRUE
  | T_FALSE
  | T_NULL ->
    do_parse env Parse.expression fail
  | T_MINUS ->
    (match Peek.token ~i:1 env with
    | T_NUMBER _ ->
      do_parse env Parse.expression fail
    | _ ->
      error_unexpected env;
      raise (Error.Error (errors env)))
  | _ ->
    error_unexpected env;
    raise (Error.Error (errors env))

let jsx_pragma_expression =
  let left_hand_side env =
    let ast = Parse.left_hand_side (with_no_new true env) in
    Expect.token env T_EOF;
    ast

  in fun content filename ->
    let env = init_env ~token_sink:None ~parse_options:None filename content in
    do_parse env left_hand_side true
