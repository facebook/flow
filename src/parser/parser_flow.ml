(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

module rec Parse : PARSER = struct
  module Type = Type_parser.Type (Parse)
  module Declaration = Declaration_parser.Declaration (Parse) (Type)
  module Pattern_cover = Pattern_cover.Cover (Parse)
  module Expression = Expression_parser.Expression (Parse) (Type) (Declaration) (Pattern_cover)
  module Object = Object_parser.Object (Parse) (Type) (Declaration) (Expression) (Pattern_cover)
  module Statement =
    Statement_parser.Statement (Parse) (Type) (Declaration) (Object) (Pattern_cover)
  module Pattern = Pattern_parser.Pattern (Parse) (Type)
  module JSX = Jsx_parser.JSX (Parse)

  let identifier ?restricted_error env =
    (match Peek.token env with
    (* "let" is disallowed as an identifier in a few situations. 11.6.2.1
       lists them out. It is always disallowed in strict mode *)
    | T_LET when in_strict_mode env -> error env Parse_error.StrictReservedWord
    | T_LET when no_let env -> error_unexpected env
    | T_LET -> ()
    (* `allow_await` means that `await` is allowed to be a keyword,
       which makes it illegal to use as an identifier.
       https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
    | T_AWAIT when allow_await env -> error env Parse_error.UnexpectedReserved
    | T_AWAIT -> ()
    (* `allow_yield` means that `yield` is allowed to be a keyword,
       which makes it illegal to use as an identifier.
       https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
    | T_YIELD when allow_yield env -> error env Parse_error.UnexpectedReserved
    | T_YIELD when in_strict_mode env -> error env Parse_error.StrictReservedWord
    | T_YIELD -> ()
    | t when token_is_strict_reserved t -> strict_error env Parse_error.StrictReservedWord
    | t when token_is_reserved t -> error_unexpected env
    | t ->
      (match restricted_error with
      | Some err when token_is_restricted t -> strict_error env err
      | _ -> ()));
    identifier_name env

  let rec program env =
    let leading = Eat.program_comments env in
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    let loc =
      match stmts with
      | [] -> end_loc
      | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts)))
    in
    let all_comments = List.rev (comments env) in
    ( loc,
      {
        Ast.Program.statements = stmts;
        comments = Flow_ast_utils.mk_comments_opt ~leading ();
        all_comments;
      } )

  and directives =
    let check env token =
      match token with
      | T_STRING (loc, _, _, octal) ->
        if octal then strict_error_at env (loc, Parse_error.StrictOctalLiteral)
      | _ -> failwith ("Nooo: " ^ token_to_string token ^ "\n")
    in
    let rec statement_list env term_fn item_fn (string_tokens, stmts) =
      match Peek.token env with
      | T_EOF -> (env, string_tokens, stmts)
      | t when term_fn t -> (env, string_tokens, stmts)
      | T_STRING _ as string_token ->
        let possible_directive = item_fn env in
        let stmts = possible_directive :: stmts in
        (match possible_directive with
        | (_, Ast.Statement.Expression { Ast.Statement.Expression.directive = Some raw; _ }) ->
          (* 14.1.1 says that it has to be "use strict" without any
             escapes, so "use\x20strict" is disallowed. *)
          let strict = in_strict_mode env || raw = "use strict" in
          let string_tokens = string_token :: string_tokens in
          statement_list (env |> with_strict strict) term_fn item_fn (string_tokens, stmts)
        | _ -> (env, string_tokens, stmts))
      | _ -> (env, string_tokens, stmts)
    in
    fun env term_fn item_fn ->
      let env = with_allow_directive true env in
      let (env, string_tokens, stmts) = statement_list env term_fn item_fn ([], []) in
      let env = with_allow_directive false env in
      List.iter (check env) (List.rev string_tokens);
      (env, stmts)

  (* 15.2 *)
  and module_item env =
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration ~decorators env
    | T_IMPORT ->
      error_on_decorators env decorators;
      let statement =
        match Peek.ith_token ~i:1 env with
        | T_LPAREN -> Statement.expression env
        | _ -> Statement.import_declaration env
      in
      statement
    | T_DECLARE when Peek.ith_token ~i:1 env = T_EXPORT ->
      error_on_decorators env decorators;
      Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

  and module_body_with_directives env term_fn =
    let (env, directives) = directives env term_fn module_item in
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
    let (env, directives) = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt :: acc) stmts directives in
    (stmts, in_strict_mode env)

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
        comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal;
      } )

  and function_block_body ~expression env =
    let start_loc = Peek.loc env in
    let leading = Peek.comments env in
    Expect.token env T_LCURLY;
    let term_fn t = t = T_RCURLY in
    let (body, strict) = statement_list_with_directives ~term_fn env in
    let end_loc = Peek.loc env in
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
    ( Loc.btwn start_loc end_loc,
      {
        Ast.Statement.Block.body;
        comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal;
      },
      strict )

  and jsx_element_or_fragment = JSX.element_or_fragment

  and pattern = Pattern.pattern

  and pattern_from_expr = Pattern.from_expr
end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  if fail && error_list <> [] then raise (Parse_error.Error error_list);
  (ast, error_list)

(* Makes the input parser expect EOF at the end. Use this to error on trailing
 * junk when parsing non-Program nodes. *)
let with_eof parser env =
  let ast = parser env in
  Expect.token env T_EOF;
  ast

let parse_statement env fail = do_parse env (with_eof Parse.statement_list_item) fail

let parse_expression env fail = do_parse env (with_eof Parse.expression) fail

let parse_program fail ?(token_sink = None) ?(parse_options = None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail = true) ?(token_sink = None) ?(parse_options = None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail = true) ?(token_sink = None) ?(parse_options = None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file ?(fail = true) ?(token_sink = None) ?(parse_options = None) content filename =
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
    (match Peek.ith_token ~i:1 env with
    | T_NUMBER _ -> do_parse env Parse.expression fail
    | _ ->
      error_unexpected ~expected:"a number" env;
      raise (Parse_error.Error (errors env)))
  | _ ->
    let errs =
      match errors env with
      | [] ->
        error_unexpected ~expected:"a valid JSON value" env;
        errors env
      | errs -> errs
    in
    raise (Parse_error.Error errs)

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
