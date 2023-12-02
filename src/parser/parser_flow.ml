(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing
module Ast = Flow_ast
open Token
open Parser_env
open Parser_common

(* Sometimes we add the same error for multiple different reasons. This is hard
   to avoid, so instead we just filter the duplicates out. This function takes
   a reversed list of errors and returns the list in forward order with dupes
   removed. This differs from a set because the original order is preserved. *)
let filter_duplicate_errors =
  let module PrintableErrorSet = Flow_set.Make (struct
    type t = Loc.t * Parse_error.t

    let compare (a_loc, a_error) (b_loc, b_error) =
      let loc = Loc.compare a_loc b_loc in
      if loc = 0 then
        Parse_error.compare a_error b_error
      else
        loc
  end) in
  fun errs ->
    let errs = List.rev errs in
    let (_, deduped) =
      List.fold_left
        (fun (set, deduped) err ->
          if PrintableErrorSet.mem err set then
            (set, deduped)
          else
            (PrintableErrorSet.add err set, err :: deduped))
        (PrintableErrorSet.empty, [])
        errs
    in
    List.rev deduped

let check_for_duplicate_exports =
  let open Ast in
  let record_export env seen (loc, { Identifier.name = export_name; comments = _ }) =
    if export_name = "" then
      (* empty identifiers signify an error, don't export it *)
      seen
    else if SSet.mem export_name seen then (
      error_at env (loc, Parse_error.DuplicateExport export_name);
      seen
    ) else
      SSet.add export_name seen
  in
  let extract_pattern_binding_names =
    let rec fold acc =
      let open Pattern in
      function
      | (_, Object { Object.properties; _ }) ->
        List.fold_left
          (fun acc prop ->
            match prop with
            | Object.Property (_, { Object.Property.pattern; _ })
            | Object.RestElement (_, { RestElement.argument = pattern; comments = _ }) ->
              fold acc pattern)
          acc
          properties
      | (_, Array { Array.elements; _ }) ->
        List.fold_left
          (fun acc elem ->
            match elem with
            | Array.Element (_, { Array.Element.argument = pattern; default = _ })
            | Array.RestElement (_, { RestElement.argument = pattern; comments = _ }) ->
              fold acc pattern
            | Array.Hole _ -> acc)
          acc
          elements
      | (_, Identifier { Pattern.Identifier.name; _ }) -> name :: acc
      | (_, Expression _) -> failwith "Parser error: No such thing as an expression pattern!"
    in
    List.fold_left fold
  in
  let record_export_of_statement env seen decl =
    match decl with
    | (_, Statement.ExportDefaultDeclaration { Statement.ExportDefaultDeclaration.default; _ }) ->
      record_export env seen (Flow_ast_utils.ident_of_source (default, "default"))
    | ( _,
        Statement.ExportNamedDeclaration
          { Statement.ExportNamedDeclaration.specifiers = Some specifiers; declaration = None; _ }
      ) ->
      let open Statement.ExportNamedDeclaration in
      (match specifiers with
      | ExportSpecifiers specifiers ->
        List.fold_left
          (fun seen (_, { Statement.ExportNamedDeclaration.ExportSpecifier.local; exported }) ->
            match exported with
            | Some exported -> record_export env seen exported
            | None -> record_export env seen local)
          seen
          specifiers
      | ExportBatchSpecifier _ ->
        (* doesn't export specific names *)
        seen)
    | ( _,
        Statement.ExportNamedDeclaration
          { Statement.ExportNamedDeclaration.specifiers = None; declaration = Some declaration; _ }
      ) ->
      (match declaration with
      | ( loc,
          ( Statement.TypeAlias { Statement.TypeAlias.id; _ }
          | Statement.OpaqueType { Statement.OpaqueType.id; _ }
          | Statement.InterfaceDeclaration { Statement.Interface.id; _ }
          | Statement.ClassDeclaration { Class.id = Some id; _ }
          | Statement.FunctionDeclaration { Function.id = Some id; _ }
          | Statement.EnumDeclaration { Statement.EnumDeclaration.id; _ }
          | Statement.ComponentDeclaration { Statement.ComponentDeclaration.id; _ } )
        ) ->
        record_export
          env
          seen
          (Flow_ast_utils.ident_of_source (loc, Flow_ast_utils.name_of_ident id))
      | (_, Statement.VariableDeclaration { Statement.VariableDeclaration.declarations; _ }) ->
        declarations
        |> List.fold_left
             (fun names (_, { Statement.VariableDeclaration.Declarator.id; _ }) ->
               extract_pattern_binding_names names [id])
             []
        |> List.fold_left (record_export env) seen
      | ( _,
          Statement.(
            ( Block _ | Break _
            | ClassDeclaration { Class.id = None; _ }
            | Continue _ | Debugger _ | DeclareClass _ | DeclareComponent _ | DeclareEnum _
            | DeclareExportDeclaration _ | DeclareFunction _ | DeclareInterface _ | DeclareModule _
            | DeclareModuleExports _ | DeclareTypeAlias _ | DeclareOpaqueType _ | DeclareVariable _
            | DoWhile _ | Empty _ | ExportDefaultDeclaration _ | ExportNamedDeclaration _
            | Expression _ | For _ | ForIn _ | ForOf _
            | FunctionDeclaration { Function.id = None; _ }
            | If _ | ImportDeclaration _ | Labeled _ | Return _ | Switch _ | Throw _ | Try _
            | While _ | With _ ))
        ) ->
        (* these don't export names -- some are invalid, but the AST allows them *)
        seen)
    | ( _,
        Statement.ExportNamedDeclaration
          { Statement.ExportNamedDeclaration.declaration = None; specifiers = None; _ }
      )
    | ( _,
        Statement.ExportNamedDeclaration
          { Statement.ExportNamedDeclaration.declaration = Some _; specifiers = Some _; _ }
      ) ->
      (* impossible *)
      seen
    | ( _,
        Statement.(
          ( Block _ | Break _ | ClassDeclaration _ | Continue _ | Debugger _ | DeclareClass _
          | DeclareComponent _ | DeclareEnum _ | DeclareExportDeclaration _ | DeclareFunction _
          | DeclareInterface _ | DeclareModule _ | DeclareModuleExports _ | DeclareTypeAlias _
          | DeclareOpaqueType _ | DeclareVariable _ | DoWhile _ | Empty _ | EnumDeclaration _
          | Expression _ | For _ | ForIn _ | ForOf _ | FunctionDeclaration _
          | ComponentDeclaration _ | If _ | ImportDeclaration _ | InterfaceDeclaration _ | Labeled _
          | Return _ | Switch _ | Throw _ | Try _ | TypeAlias _ | OpaqueType _
          | VariableDeclaration _ | While _ | With _ ))
      ) ->
      seen
  in
  (fun env stmts -> ignore (List.fold_left (record_export_of_statement env) SSet.empty stmts))

module rec Parse : PARSER = struct
  module Type = Type_parser.Type (Parse)
  module Declaration = Declaration_parser.Declaration (Parse) (Type)
  module Pattern_cover = Pattern_cover.Cover (Parse)
  module Expression = Expression_parser.Expression (Parse) (Type) (Declaration) (Pattern_cover)
  module Object = Object_parser.Object (Parse) (Type) (Declaration) (Expression) (Pattern_cover)
  module Statement =
    Statement_parser.Statement (Parse) (Type) (Declaration) (Object) (Pattern_cover)
  module Pattern = Pattern_parser.Pattern (Parse) (Type)
  module JSX = Jsx_parser.JSX (Parse) (Expression)

  let annot = Type.annotation

  let identifier ?restricted_error env =
    let id = identifier_name env in
    assert_identifier_name_is_identifier ?restricted_error env id;
    id

  let rec program env =
    let interpreter =
      match Peek.token env with
      | T_INTERPRETER (loc, value) ->
        Eat.token env;
        Some (loc, value)
      | _ -> None
    in
    let leading = Eat.program_comments env in
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    check_for_duplicate_exports env stmts;
    let loc =
      match stmts with
      | [] -> end_loc
      | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts)))
    in
    let all_comments = List.rev (comments env) in
    ( loc,
      {
        Ast.Program.statements = stmts;
        interpreter;
        comments = Flow_ast_utils.mk_comments_opt ~leading ();
        all_comments;
      }
    )

  and directives =
    let check env token =
      match token with
      | T_STRING (loc, _, _, octal) ->
        if octal then strict_error_at env (loc, Parse_error.StrictOctalLiteral)
      | _ -> failwith ("Nooo: " ^ token_to_string token ^ "\n")
    in
    let rec statement_list env term_fn item_fn (string_tokens, stmts, contains_use_strict) =
      match Peek.token env with
      | T_EOF -> (env, string_tokens, stmts, contains_use_strict)
      | t when term_fn t -> (env, string_tokens, stmts, contains_use_strict)
      | T_STRING _ as string_token ->
        let possible_directive = item_fn env in
        let stmts = possible_directive :: stmts in
        (match possible_directive with
        | (loc, Ast.Statement.Expression { Ast.Statement.Expression.directive = Some raw; _ }) ->
          (* 14.1.1 says that it has to be "use strict" without any
             escapes, so "use\x20strict" is disallowed. *)
          let strict = raw = "use strict" in
          if strict && not (has_simple_parameters env) then
            error_at env (loc, Parse_error.StrictParamNotSimple);
          let env =
            if strict then
              with_strict true env
            else
              env
          in
          let string_tokens = string_token :: string_tokens in
          statement_list env term_fn item_fn (string_tokens, stmts, contains_use_strict || strict)
        | _ -> (env, string_tokens, stmts, contains_use_strict))
      | _ -> (env, string_tokens, stmts, contains_use_strict)
    in
    fun env term_fn item_fn ->
      let env = with_allow_directive true env in
      let (env, string_tokens, stmts, contains_use_strict) =
        statement_list env term_fn item_fn ([], [], false)
      in
      let env = with_allow_directive false env in
      List.iter (check env) (List.rev string_tokens);
      (env, stmts, contains_use_strict)

  (* 15.2 *)
  and module_item env =
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration ~decorators env
    | T_IMPORT ->
      error_on_decorators env decorators;
      let statement =
        match Peek.ith_token ~i:1 env with
        | T_LPAREN (* import(...) *)
        | T_PERIOD (* import.meta *) ->
          Statement.expression env
        | _ -> Statement.import_declaration env
      in
      statement
    | T_DECLARE when Peek.ith_token ~i:1 env = T_EXPORT ->
      error_on_decorators env decorators;
      Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

  and module_body_with_directives env term_fn =
    let (env, directives, _contains_use_strict) = directives env term_fn module_item in
    let stmts = module_body ~term_fn env in
    (* Prepend the directives *)
    List.fold_left (fun acc stmt -> stmt :: acc) stmts directives

  and module_body =
    let rec module_item_list env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> module_item_list env term_fn (module_item env :: acc)
    in
    (fun ~term_fn env -> module_item_list env term_fn [])

  and statement_list_with_directives ~term_fn env =
    let (env, directives, contains_use_strict) = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt :: acc) stmts directives in
    (stmts, contains_use_strict)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn (statement_list_item env :: acc)
    in
    (fun ~term_fn env -> statements env term_fn [])

  and statement_list_item ?(decorators = []) env =
    if not (Peek.is_class env) then error_on_decorators env decorators;
    let open Statement in
    match Peek.token env with
    (* Remember kids, these look like statements but they're not
       * statements... (see section 13) *)
    | T_LET -> let_ env
    | T_CONST -> const env
    | _ when Peek.is_function env -> Declaration._function env
    | _ when Peek.is_class env -> class_declaration env decorators
    | T_INTERFACE -> interface env
    | T_DECLARE -> declare env
    | T_TYPE -> type_alias env
    | T_OPAQUE -> opaque_type env
    | T_ENUM when (parse_options env).enums -> Declaration.enum_declaration env
    | _ when Peek.is_component env -> Declaration.component env
    | _ -> statement env

  and statement env =
    let open Statement in
    match Peek.token env with
    | T_EOF ->
      error_unexpected ~expected:"the start of a statement" env;
      (Peek.loc env, Ast.Statement.Empty { Ast.Statement.Empty.comments = None })
    | T_SEMICOLON -> empty env
    | T_LCURLY -> block env
    | T_VAR -> var env
    | T_BREAK -> break env
    | T_CONTINUE -> continue env
    | T_DEBUGGER -> debugger env
    | T_DO -> do_while env
    | T_FOR -> for_ env
    | T_IF -> if_ env
    | T_RETURN -> return env
    | T_SWITCH -> switch env
    | T_THROW -> throw env
    | T_TRY -> try_ env
    | T_WHILE -> while_ env
    | T_WITH -> with_ env
    (* If we see an else then it's definitely an error, but we can probably
     * assume that this is a malformed if statement that is missing the if *)
    | T_ELSE -> if_ env
    (* There are a bunch of tokens that aren't the start of any valid
     * statement. We list them here in order to skip over them, rather than
     * getting stuck *)
    | T_COLON
    | T_RPAREN
    | T_RCURLY
    | T_RBRACKET
    | T_COMMA
    | T_PERIOD
    | T_PLING_PERIOD
    | T_ARROW
    | T_IN
    | T_INSTANCEOF
    | T_CATCH
    | T_FINALLY
    | T_CASE
    | T_DEFAULT
    | T_EXTENDS
    | T_STATIC
    | T_EXPORT
    (* TODO *)
    | T_ELLIPSIS ->
      error_unexpected ~expected:"the start of a statement" env;
      Eat.token env;
      statement env
    (* The rest of these patterns handle ExpressionStatement and its negative
       lookaheads, which prevent ambiguities.
       See https://tc39.github.io/ecma262/#sec-expression-statement *)
    | _ when Peek.is_function env ->
      let func = Declaration._function env in
      function_as_statement_error_at env (fst func);
      func
    | T_LET when Peek.ith_token ~i:1 env = T_LBRACKET ->
      (* `let [foo]` is ambiguous: either a let binding pattern, or a
         member expression, so it is banned. *)
      let loc = Loc.btwn (Peek.loc env) (Peek.ith_loc ~i:1 env) in
      error_at env (loc, Parse_error.AmbiguousLetBracket);
      Statement.expression env
    (* recover as a member expression *)
    | _ when Peek.is_identifier env -> maybe_labeled env
    | _ when Peek.is_class env ->
      error_unexpected env;
      Eat.token env;
      Statement.expression env
    | _ -> Statement.expression env

  and expression env =
    let start_loc = Peek.loc env in
    let expr = Expression.assignment env in
    match Peek.token env with
    | T_COMMA -> Expression.sequence env ~start_loc [expr]
    | _ -> expr

  and expression_or_pattern env =
    let start_loc = Peek.loc env in
    let expr_or_pattern = Expression.assignment_cover env in
    match Peek.token env with
    | T_COMMA ->
      let expr = Pattern_cover.as_expression env expr_or_pattern in
      let seq = Expression.sequence env ~start_loc [expr] in
      Cover_expr seq
    | _ -> expr_or_pattern

  and conditional = Expression.conditional

  and assignment = Expression.assignment

  and left_hand_side = Expression.left_hand_side

  and object_initializer = Object._initializer

  and object_key = Object.key

  and class_declaration = Object.class_declaration

  and class_expression = Object.class_expression

  and is_assignable_lhs = Expression.is_assignable_lhs

  and number = Expression.number

  and bigint = Expression.bigint

  and identifier_with_type =
    let with_loc_helper no_optional restricted_error env =
      let name = identifier ~restricted_error env in
      let optional = (not no_optional) && Peek.token env = T_PLING in
      if optional then (
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
        Expect.token env T_PLING
      );
      let annot = Type.annotation_opt env in
      Ast.Pattern.Identifier.{ name; optional; annot }
    in
    fun env ?(no_optional = false) restricted_error ->
      with_loc (with_loc_helper no_optional restricted_error) env

  and block_body env =
    let start_loc = Peek.loc env in
    let leading = Peek.comments env in
    Expect.token env T_LCURLY;
    let term_fn t = t = T_RCURLY in
    let body = statement_list ~term_fn env in
    let end_loc = Peek.loc env in
    let internal =
      if body = [] then
        Peek.comments env
      else
        []
    in
    Expect.token env T_RCURLY;
    let trailing = Eat.trailing_comments env in
    ( Loc.btwn start_loc end_loc,
      {
        Ast.Statement.Block.body;
        comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
      }
    )

  and function_block_body ~expression =
    with_loc_extra (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LCURLY;
        let term_fn t = t = T_RCURLY in
        let (body, contains_use_strict) = statement_list_with_directives ~term_fn env in
        let internal =
          if body = [] then
            Peek.comments env
          else
            []
        in
        Expect.token env T_RCURLY;
        let trailing =
          match (expression, Peek.token env) with
          | (true, _)
          | (_, (T_RCURLY | T_EOF)) ->
            Eat.trailing_comments env
          | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
          | _ -> []
        in
        let comments =
          Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ()
        in
        ({ Ast.Statement.Block.body; comments }, contains_use_strict)
    )

  and jsx_element_or_fragment = JSX.element_or_fragment ~parent_opening_name:None

  and pattern = Pattern.pattern

  and pattern_from_expr = Pattern.from_expr
end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  match error_list with
  | e :: es when fail -> raise (Parse_error.Error (e, es))
  | _ -> (ast, error_list)

let parse_program fail ?(token_sink = None) ?(parse_options = None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail = true) ?(token_sink = None) ?(parse_options = None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail = true) ?(token_sink = None) ?(parse_options = None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

let parse_annot ?(parse_options = None) filename content =
  let env = init_env ~token_sink:None ~parse_options filename content in
  do_parse env Parse.annot false

let package_json_file =
  let parser env =
    let (loc, obj, { if_expr; _ }) = Parse.object_initializer env in
    List.iter (error_at env) if_expr;
    (loc, obj)
  in
  fun ?(fail = true) ?(token_sink = None) ?(parse_options = None) content filename ->
    let env = init_env ~token_sink ~parse_options filename content in
    do_parse env parser fail

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file =
  let null_fallback _env = Ast.Expression.NullLiteral None in
  let parser env =
    match Peek.token env with
    | T_LBRACKET
    | T_LCURLY
    | T_STRING _
    | T_NUMBER _
    | T_TRUE
    | T_FALSE
    | T_NULL ->
      Parse.expression env
    | T_MINUS ->
      (match Peek.ith_token ~i:1 env with
      | T_NUMBER _ -> Parse.expression env
      | _ ->
        error_unexpected ~expected:"a number" env;
        with_loc null_fallback env)
    | _ ->
      error_unexpected ~expected:"a valid JSON value" env;
      with_loc null_fallback env
  in
  fun ?(fail = true) ?(token_sink = None) ?(parse_options = None) content filename ->
    let env = init_env ~token_sink ~parse_options filename content in
    do_parse env parser fail

let jsx_pragma_expression =
  let left_hand_side env =
    let ast = Parse.left_hand_side (with_no_new true env) in
    Expect.token env T_EOF;
    ast
  in
  fun content filename ->
    let env = init_env ~token_sink:None ~parse_options:None filename content in
    do_parse env left_hand_side true

let string_is_valid_identifier_name str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  Flow_lexer.is_valid_identifier_name lexbuf
