(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
module SSet = Set.Make (String)

module Lex_mode = struct
  type t =
    | NORMAL
    | TYPE
    | JSX_TAG
    | JSX_CHILD
    | TEMPLATE
    | REGEXP

  let debug_string_of_lex_mode (mode : t) =
    match mode with
    | NORMAL -> "NORMAL"
    | TYPE -> "TYPE"
    | JSX_TAG -> "JSX_TAG"
    | JSX_CHILD -> "JSX_CHILD"
    | TEMPLATE -> "TEMPLATE"
    | REGEXP -> "REGEXP"
end

(* READ THIS BEFORE YOU MODIFY:
 *
 * The current implementation for lookahead beyond a single token is
 * inefficient. If you believe you need to increase this constant, do one of the
 * following:
 * - Find another way
 * - Benchmark your change and provide convincing evidence that it doesn't
 *   actually have a significant perf impact.
 * - Refactor this to memoize all requested lookahead, so we aren't lexing the
 *   same token multiple times.
 *)
let maximum_lookahead = 2

module Lookahead : sig
  type t

  val create : Lex_env.t -> Lex_mode.t -> t

  val peek : t -> int -> Lex_result.t

  val lex_env : t -> int -> Lex_env.t

  val junk : t -> unit
end = struct
  type t = {
    mutable la_results: (Lex_env.t * Lex_result.t) option array;
    mutable la_num_lexed: int;
    la_lex_mode: Lex_mode.t;
    mutable la_lex_env: Lex_env.t;
  }

  let create lex_env mode =
    let lex_env = Lex_env.clone lex_env in
    { la_results = [||]; la_num_lexed = 0; la_lex_mode = mode; la_lex_env = lex_env }

  let next_power_of_two n =
    let rec f i =
      if i >= n then
        i
      else
        f (i * 2)
    in
    f 1

  (* resize the tokens array to have at least n elements *)
  let grow t n =
    if Array.length t.la_results < n then
      let new_size = next_power_of_two n in
      let filler i =
        if i < Array.length t.la_results then
          t.la_results.(i)
        else
          None
      in
      let new_arr = Array.init new_size filler in
      t.la_results <- new_arr

  (* precondition: there is enough room in t.la_results for the result *)
  let lex t =
    let lex_env = t.la_lex_env in
    let (lex_env, lex_result) =
      match t.la_lex_mode with
      | Lex_mode.NORMAL -> Flow_lexer.token lex_env
      | Lex_mode.TYPE -> Flow_lexer.type_token lex_env
      | Lex_mode.JSX_TAG -> Flow_lexer.jsx_tag lex_env
      | Lex_mode.JSX_CHILD -> Flow_lexer.jsx_child lex_env
      | Lex_mode.TEMPLATE -> Flow_lexer.template_tail lex_env
      | Lex_mode.REGEXP -> Flow_lexer.regexp lex_env
    in
    let cloned_env = Lex_env.clone lex_env in
    t.la_lex_env <- lex_env;
    t.la_results.(t.la_num_lexed) <- Some (cloned_env, lex_result);
    t.la_num_lexed <- t.la_num_lexed + 1

  let lex_until t i =
    grow t (i + 1);
    while t.la_num_lexed <= i do
      lex t
    done

  let peek t i =
    lex_until t i;
    match t.la_results.(i) with
    | Some (_, result) -> result
    (* only happens if there is a defect in the lookahead module *)
    | None -> failwith "Lookahead.peek failed"

  let lex_env t i =
    lex_until t i;
    match t.la_results.(i) with
    | Some (lex_env, _) -> lex_env
    (* only happens if there is a defect in the lookahead module *)
    | None -> failwith "Lookahead.peek failed"

  (* Throws away the first peeked-at token, shifting any subsequent tokens up *)
  let junk t =
    lex_until t 0;
    if t.la_num_lexed > 1 then Array.blit t.la_results 1 t.la_results 0 (t.la_num_lexed - 1);
    t.la_results.(t.la_num_lexed - 1) <- None;
    t.la_num_lexed <- t.la_num_lexed - 1
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Token.t;
  token_context: Lex_mode.t;
}

type parse_options = {
  enums: bool;  (** enable parsing of Flow enums *)
  esproposal_class_instance_fields: bool;  (** enable parsing of class instance fields *)
  esproposal_class_static_fields: bool;  (** enable parsing of class static fields *)
  esproposal_decorators: bool;  (** enable parsing of decorators *)
  esproposal_export_star_as: bool;  (** enable parsing of `export * as` syntax *)
  esproposal_nullish_coalescing: bool;  (** enable parsing of nullish coalescing (`??`) *)
  esproposal_optional_chaining: bool;  (** enable parsing of optional chaining (`?.`) *)
  types: bool;  (** enable parsing of Flow types *)
  use_strict: bool;  (** treat the file as strict, without needing a "use strict" directive *)
}

let default_parse_options =
  {
    enums = false;
    esproposal_class_instance_fields = false;
    esproposal_class_static_fields = false;
    esproposal_decorators = false;
    esproposal_export_star_as = false;
    esproposal_optional_chaining = false;
    esproposal_nullish_coalescing = false;
    types = true;
    use_strict = false;
  }

type allowed_super =
  | No_super
  | Super_prop
  | Super_prop_or_call

type env = {
  errors: (Loc.t * Parse_error.t) list ref;
  comments: Loc.t Comment.t list ref;
  labels: SSet.t;
  exports: SSet.t ref;
  last_lex_result: Lex_result.t option ref;
  in_strict_mode: bool;
  in_export: bool;
  in_loop: bool;
  in_switch: bool;
  in_formal_parameters: bool;
  in_function: bool;
  no_in: bool;
  no_call: bool;
  no_let: bool;
  no_anon_function_type: bool;
  no_new: bool;
  allow_yield: bool;
  allow_await: bool;
  allow_directive: bool;
  allow_super: allowed_super;
  error_callback: (env -> Parse_error.t -> unit) option;
  lex_mode_stack: Lex_mode.t list ref;
  (* lex_env is the lex_env after the single lookahead has been lexed *)
  lex_env: Lex_env.t ref;
  (* This needs to be cleared whenever we advance. *)
  lookahead: Lookahead.t ref;
  token_sink: (token_sink_result -> unit) option ref;
  parse_options: parse_options;
  source: File_key.t option;
  (* It is a syntax error to reference private fields not in scope. In order to enforce this,
   * we keep track of the privates we've seen declared and used. *)
  privates: (SSet.t * (string * Loc.t) list) list ref;
  (* The position up to which comments have been consumed, exclusive. *)
  consumed_comments_pos: Loc.position ref;
}

(* constructor *)
let init_env ?(token_sink = None) ?(parse_options = None) source content =
  (* let lb = Sedlexing.Utf16.from_string
     content (Some Sedlexing.Utf16.Little_endian) in *)
  let (lb, errors) =
    try (Sedlexing.Utf8.from_string content, [])
    with Sedlexing.MalFormed ->
      (Sedlexing.Utf8.from_string "", [({ Loc.none with Loc.source }, Parse_error.MalformedUnicode)])
  in
  let parse_options =
    match parse_options with
    | Some opts -> opts
    | None -> default_parse_options
  in
  let enable_types_in_comments = parse_options.types in
  let lex_env = Lex_env.new_lex_env source lb ~enable_types_in_comments in
  {
    errors = ref errors;
    comments = ref [];
    labels = SSet.empty;
    exports = ref SSet.empty;
    last_lex_result = ref None;
    in_strict_mode = parse_options.use_strict;
    in_export = false;
    in_loop = false;
    in_switch = false;
    in_formal_parameters = false;
    in_function = false;
    no_in = false;
    no_call = false;
    no_let = false;
    no_anon_function_type = false;
    no_new = false;
    allow_yield = false;
    allow_await = false;
    allow_directive = false;
    allow_super = No_super;
    error_callback = None;
    lex_mode_stack = ref [Lex_mode.NORMAL];
    lex_env = ref lex_env;
    lookahead = ref (Lookahead.create lex_env Lex_mode.NORMAL);
    token_sink = ref token_sink;
    parse_options;
    source;
    privates = ref [];
    consumed_comments_pos = ref { Loc.line = 0; column = 0 };
  }

(* getters: *)
let in_strict_mode env = env.in_strict_mode

let lex_mode env = List.hd !(env.lex_mode_stack)

let in_export env = env.in_export

let comments env = !(env.comments)

let labels env = env.labels

let in_loop env = env.in_loop

let in_switch env = env.in_switch

let in_formal_parameters env = env.in_formal_parameters

let in_function env = env.in_function

let allow_yield env = env.allow_yield

let allow_await env = env.allow_await

let allow_directive env = env.allow_directive

let allow_super env = env.allow_super

let no_in env = env.no_in

let no_call env = env.no_call

let no_let env = env.no_let

let no_anon_function_type env = env.no_anon_function_type

let no_new env = env.no_new

let errors env = !(env.errors)

let parse_options env = env.parse_options

let source env = env.source

let should_parse_types env = env.parse_options.types

(* mutators: *)
let error_at env (loc, e) =
  env.errors := (loc, e) :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env e

let record_export env (loc, { Identifier.name = export_name; comments = _ }) =
  if export_name = "" then
    ()
  else
    (* empty identifiers signify an error, don't export it *)
    let exports = !(env.exports) in
    if SSet.mem export_name exports then
      error_at env (loc, Parse_error.DuplicateExport export_name)
    else
      env.exports := SSet.add export_name !(env.exports)

(* Since private fields out of scope are a parse error, we keep track of the declared and used
 * private fields.
 *
 * Whenever we enter a class, we push new empty lists of declared and used privates.
 * When we encounter a new declared private, we add it to the top of the declared_privates list
 * via add_declared_private. We do the same with used_privates via add_used_private.
 *
 * When we exit a class, we look for all the unbound private variables. Since class fields
 * are hoisted to the scope of the class, we may need to look further before we conclude that
 * a field is out of scope. To do that, we add all of the unbound private fields to the
 * next used_private list. Once we run out of declared private lists, any leftover used_privates
 * are unbound private variables. *)
let enter_class env = env.privates := (SSet.empty, []) :: !(env.privates)

let exit_class env =
  let get_unbound_privates declared_privates used_privates =
    List.filter (fun x -> not (SSet.mem (fst x) declared_privates)) used_privates
  in
  match !(env.privates) with
  | [(declared_privates, used_privates)] ->
    let unbound_privates = get_unbound_privates declared_privates used_privates in
    List.iter
      (fun (name, loc) -> error_at env (loc, Parse_error.UnboundPrivate name))
      unbound_privates;
    env.privates := []
  | (loc_declared_privates, loc_used_privates) :: privates ->
    let unbound_privates = get_unbound_privates loc_declared_privates loc_used_privates in
    let (decl_head, used_head) = List.hd privates in
    env.privates := (decl_head, used_head @ unbound_privates) :: List.tl privates
  | _ -> failwith "Internal Error: `exit_class` called before a matching `enter_class`"

let add_declared_private env name =
  match !(env.privates) with
  | [] -> failwith "Internal Error: Tried to add_declared_private with outside of class scope."
  | (declared, used) :: xs -> env.privates := (SSet.add name declared, used) :: xs

let add_used_private env name loc =
  match !(env.privates) with
  | [] -> error_at env (loc, Parse_error.PrivateNotInClass)
  | (declared, used) :: xs -> env.privates := (declared, (name, loc) :: used) :: xs

let consume_comments_until env pos = env.consumed_comments_pos := pos

(* lookahead: *)
let lookahead ~i env =
  assert (i < maximum_lookahead);
  Lookahead.peek !(env.lookahead) i

(* functional operations: *)
let with_strict in_strict_mode env = { env with in_strict_mode }

let with_in_formal_parameters in_formal_parameters env = { env with in_formal_parameters }

let with_in_function in_function env = { env with in_function }

let with_allow_yield allow_yield env = { env with allow_yield }

let with_allow_await allow_await env = { env with allow_await }

let with_allow_directive allow_directive env = { env with allow_directive }

let with_allow_super allow_super env = { env with allow_super }

let with_no_let no_let env = { env with no_let }

let with_in_loop in_loop env = { env with in_loop }

let with_no_in no_in env = { env with no_in }

let with_no_anon_function_type no_anon_function_type env = { env with no_anon_function_type }

let with_no_new no_new env = { env with no_new }

let with_in_switch in_switch env = { env with in_switch }

let with_in_export in_export env = { env with in_export }

let with_no_call no_call env = { env with no_call }

let with_error_callback error_callback env = { env with error_callback = Some error_callback }

(* other helper functions: *)
let error_list env = List.iter (error_at env)

let last_loc env =
  match !(env.last_lex_result) with
  | Some lex_result -> Some (Lex_result.loc lex_result)
  | None -> None

let last_token env =
  match !(env.last_lex_result) with
  | Some lex_result -> Some (Lex_result.token lex_result)
  | None -> None

let without_error_callback env = { env with error_callback = None }

let add_label env label = { env with labels = SSet.add label env.labels }

let enter_function env ~async ~generator =
  {
    env with
    in_formal_parameters = false;
    in_function = true;
    in_loop = false;
    in_switch = false;
    labels = SSet.empty;
    allow_await = async;
    allow_yield = generator;
  }

(* #sec-keywords *)
let is_keyword = function
  | "await"
  | "break"
  | "case"
  | "catch"
  | "class"
  | "const"
  | "continue"
  | "debugger"
  | "default"
  | "delete"
  | "do"
  | "else"
  | "export"
  | "extends"
  | "finally"
  | "for"
  | "function"
  | "if"
  | "import"
  | "in"
  | "instanceof"
  | "new"
  | "return"
  | "super"
  | "switch"
  | "this"
  | "throw"
  | "try"
  | "typeof"
  | "var"
  | "void"
  | "while"
  | "with"
  | "yield" ->
    true
  | _ -> false

let token_is_keyword =
  Token.(
    function
    | T_IDENTIFIER { raw; _ } when is_keyword raw -> true
    | T_AWAIT
    | T_BREAK
    | T_CASE
    | T_CATCH
    | T_CLASS
    | T_CONST
    | T_CONTINUE
    | T_DEBUGGER
    | T_DEFAULT
    | T_DELETE
    | T_DO
    | T_ELSE
    | T_EXPORT
    | T_EXTENDS
    | T_FINALLY
    | T_FOR
    | T_FUNCTION
    | T_IF
    | T_IMPORT
    | T_IN
    | T_INSTANCEOF
    | T_NEW
    | T_RETURN
    | T_SUPER
    | T_SWITCH
    | T_THIS
    | T_THROW
    | T_TRY
    | T_TYPEOF
    | T_VAR
    | T_VOID
    | T_WHILE
    | T_WITH
    | T_YIELD ->
      true
    | _ -> false)

(* #sec-future-reserved-words *)
let is_future_reserved = function
  | "enum" -> true
  | _ -> false

let token_is_future_reserved =
  Token.(
    function
    | T_IDENTIFIER { raw; _ } when is_future_reserved raw -> true
    | T_ENUM -> true
    | _ -> false)

(* #sec-strict-mode-of-ecmascript *)
let is_strict_reserved = function
  | "interface"
  | "implements"
  | "package"
  | "private"
  | "protected"
  | "public"
  | "static"
  | "yield" ->
    true
  | _ -> false

let token_is_strict_reserved =
  Token.(
    function
    | T_IDENTIFIER { raw; _ } when is_strict_reserved raw -> true
    | T_INTERFACE
    | T_IMPLEMENTS
    | T_PACKAGE
    | T_PRIVATE
    | T_PROTECTED
    | T_PUBLIC
    | T_STATIC
    | T_YIELD ->
      true
    | _ -> false)

(* #sec-strict-mode-of-ecmascript *)
let is_restricted = function
  | "eval"
  | "arguments" ->
    true
  | _ -> false

let token_is_restricted =
  Token.(
    function
    | T_IDENTIFIER { raw; _ } when is_restricted raw -> true
    | _ -> false)

(* #sec-reserved-words *)
let is_reserved str_val =
  is_keyword str_val
  || is_future_reserved str_val
  ||
  match str_val with
  | "null"
  | "true"
  | "false" ->
    true
  | _ -> false

let token_is_reserved t =
  token_is_keyword t
  || token_is_future_reserved t
  ||
  match t with
  | Token.T_IDENTIFIER { raw = "null" | "true" | "false"; _ }
  | Token.T_NULL
  | Token.T_TRUE
  | Token.T_FALSE ->
    true
  | _ -> false

let is_reserved_type str_val =
  match str_val with
  | "any"
  | "bool"
  | "boolean"
  | "empty"
  | "false"
  | "mixed"
  | "null"
  | "number"
  | "bigint"
  | "static"
  | "string"
  | "true"
  | "typeof"
  | "void"
  | "interface"
  | "extends"
  | "_" ->
    true
  | _ -> false

(* Answer questions about what comes next *)
module Peek = struct
  open Loc
  open Token

  let ith_token ~i env = Lex_result.token (lookahead ~i env)

  let ith_loc ~i env = Lex_result.loc (lookahead ~i env)

  let ith_errors ~i env = Lex_result.errors (lookahead ~i env)

  let ith_comments ~i env =
    let comments = Lex_result.comments (lookahead ~i env) in
    List.filter
      (fun ({ Loc.start; _ }, _) -> Loc.pos_cmp !(env.consumed_comments_pos) start <= 0)
      comments

  let ith_lex_env ~i env = Lookahead.lex_env !(env.lookahead) i

  let token env = ith_token ~i:0 env

  let loc env = ith_loc ~i:0 env

  (* loc_skip_lookahead is used to give a loc hint to optional tokens such as type annotations *)
  let loc_skip_lookahead env =
    let loc =
      match last_loc env with
      | Some loc -> loc
      | None -> failwith "Peeking current location when not available"
    in
    Loc.{ loc with start = loc._end }

  let errors env = ith_errors ~i:0 env

  let comments env = ith_comments ~i:0 env

  let has_eaten_comments env =
    let comments = Lex_result.comments (lookahead ~i:0 env) in
    List.exists
      (fun ({ Loc.start; _ }, _) -> Loc.pos_cmp start !(env.consumed_comments_pos) < 0)
      comments

  let lex_env env = ith_lex_env ~i:0 env

  (* True if there is a line terminator before the next token *)
  let ith_is_line_terminator ~i env =
    let loc =
      if i > 0 then
        Some (ith_loc ~i:(i - 1) env)
      else
        last_loc env
    in
    match loc with
    | None -> false
    | Some loc' -> (ith_loc ~i env).start.line > loc'.start.line

  let is_line_terminator env = ith_is_line_terminator ~i:0 env

  let ith_is_implicit_semicolon ~i env =
    match ith_token ~i env with
    | T_EOF
    | T_RCURLY ->
      true
    | T_SEMICOLON -> false
    | _ -> ith_is_line_terminator ~i env

  let is_implicit_semicolon env = ith_is_implicit_semicolon ~i:0 env

  let ith_is_identifier ~i env =
    match ith_token ~i env with
    | t when token_is_strict_reserved t -> true
    | t when token_is_future_reserved t -> true
    | t when token_is_restricted t -> true
    | T_LET
    | T_TYPE
    | T_OPAQUE
    | T_OF
    | T_DECLARE
    | T_ASYNC
    | T_AWAIT
    | T_POUND
    | T_IDENTIFIER _ ->
      true
    | _ -> false

  let ith_is_type_identifier ~i env =
    match lex_mode env with
    | Lex_mode.TYPE ->
      begin
        match ith_token ~i env with
        | T_IDENTIFIER _ -> true
        | _ -> false
      end
    | Lex_mode.NORMAL ->
      (* Sometimes we peek at type identifiers while in normal lex mode. For
         example, when deciding whether a `type` token is an identifier or the
         start of a type declaration, based on whether the following token
         `is_type_identifier`. *)
      begin
        match ith_token ~i env with
        | T_IDENTIFIER { raw; _ } when is_reserved_type raw -> false
        (* reserved type identifiers, but these don't appear in NORMAL mode *)
        | T_ANY_TYPE
        | T_MIXED_TYPE
        | T_EMPTY_TYPE
        | T_NUMBER_TYPE
        | T_BIGINT_TYPE
        | T_STRING_TYPE
        | T_VOID_TYPE
        | T_SYMBOL_TYPE
        | T_BOOLEAN_TYPE _
        | T_NUMBER_SINGLETON_TYPE _
        | T_BIGINT_SINGLETON_TYPE _
        (* identifier-ish *)
        | T_ASYNC
        | T_AWAIT
        | T_BREAK
        | T_CASE
        | T_CATCH
        | T_CLASS
        | T_CONST
        | T_CONTINUE
        | T_DEBUGGER
        | T_DECLARE
        | T_DEFAULT
        | T_DELETE
        | T_DO
        | T_ELSE
        | T_ENUM
        | T_EXPORT
        | T_EXTENDS
        | T_FALSE
        | T_FINALLY
        | T_FOR
        | T_FUNCTION
        | T_IDENTIFIER _
        | T_IF
        | T_IMPLEMENTS
        | T_IMPORT
        | T_IN
        | T_INSTANCEOF
        | T_INTERFACE
        | T_LET
        | T_NEW
        | T_NULL
        | T_OF
        | T_OPAQUE
        | T_PACKAGE
        | T_PRIVATE
        | T_PROTECTED
        | T_PUBLIC
        | T_RETURN
        | T_SUPER
        | T_SWITCH
        | T_THIS
        | T_THROW
        | T_TRUE
        | T_TRY
        | T_TYPE
        | T_VAR
        | T_WHILE
        | T_WITH
        | T_YIELD ->
          true
        (* identifier-ish, but not valid types *)
        | T_STATIC
        | T_TYPEOF
        | T_VOID ->
          false
        (* syntax *)
        | T_LCURLY
        | T_RCURLY
        | T_LCURLYBAR
        | T_RCURLYBAR
        | T_LPAREN
        | T_RPAREN
        | T_LBRACKET
        | T_RBRACKET
        | T_SEMICOLON
        | T_COMMA
        | T_PERIOD
        | T_ARROW
        | T_ELLIPSIS
        | T_AT
        | T_POUND
        | T_CHECKS
        | T_RSHIFT3_ASSIGN
        | T_RSHIFT_ASSIGN
        | T_LSHIFT_ASSIGN
        | T_BIT_XOR_ASSIGN
        | T_BIT_OR_ASSIGN
        | T_BIT_AND_ASSIGN
        | T_MOD_ASSIGN
        | T_DIV_ASSIGN
        | T_MULT_ASSIGN
        | T_EXP_ASSIGN
        | T_MINUS_ASSIGN
        | T_PLUS_ASSIGN
        | T_ASSIGN
        | T_PLING_PERIOD
        | T_PLING_PLING
        | T_PLING
        | T_COLON
        | T_OR
        | T_AND
        | T_BIT_OR
        | T_BIT_XOR
        | T_BIT_AND
        | T_EQUAL
        | T_NOT_EQUAL
        | T_STRICT_EQUAL
        | T_STRICT_NOT_EQUAL
        | T_LESS_THAN_EQUAL
        | T_GREATER_THAN_EQUAL
        | T_LESS_THAN
        | T_GREATER_THAN
        | T_LSHIFT
        | T_RSHIFT
        | T_RSHIFT3
        | T_PLUS
        | T_MINUS
        | T_DIV
        | T_MULT
        | T_EXP
        | T_MOD
        | T_NOT
        | T_BIT_NOT
        | T_INCR
        | T_DECR
        | T_EOF ->
          false
        (* literals *)
        | T_NUMBER _
        | T_BIGINT _
        | T_STRING _
        | T_TEMPLATE_PART _
        | T_REGEXP _
        (* misc that shouldn't appear in NORMAL mode *)
        | T_JSX_IDENTIFIER _
        | T_JSX_TEXT _
        | T_ERROR _ ->
          false
      end
    | Lex_mode.JSX_TAG
    | Lex_mode.JSX_CHILD
    | Lex_mode.TEMPLATE
    | Lex_mode.REGEXP ->
      false

  let ith_is_identifier_name ~i env = ith_is_identifier ~i env || ith_is_type_identifier ~i env

  (* This returns true if the next token is identifier-ish (even if it is an
     error) *)
  let is_identifier env = ith_is_identifier ~i:0 env

  let is_identifier_name env = ith_is_identifier_name ~i:0 env

  let is_type_identifier env = ith_is_type_identifier ~i:0 env

  let is_function env =
    token env = T_FUNCTION
    || token env = T_ASYNC
       && ith_token ~i:1 env = T_FUNCTION
       && (loc env)._end.line = (ith_loc ~i:1 env).start.line

  let is_class env =
    match token env with
    | T_CLASS
    | T_AT ->
      true
    | _ -> false
end

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)

(* Complains about an error at the location of the lookahead *)
let error env e =
  let loc = Peek.loc env in
  error_at env (loc, e)

let get_unexpected_error ?expected token =
  if token_is_future_reserved token then
    Parse_error.UnexpectedReserved
  else if token_is_strict_reserved token then
    Parse_error.StrictReservedWord
  else
    let unexpected = Token.explanation_of_token token in
    match expected with
    | Some expected_msg -> Parse_error.UnexpectedWithExpected (unexpected, expected_msg)
    | None -> Parse_error.Unexpected unexpected

let error_unexpected ?expected env =
  (* So normally we consume the lookahead lex result when Eat.token calls
   * Parser_env.advance, which will add any lexing errors to our list of errors.
   * However, raising an unexpected error for a lookahead is kind of like
   * consuming that token, so we should process any lexing errors before
   * complaining about the unexpected token *)
  error_list env (Peek.errors env);
  error env (get_unexpected_error ?expected (Peek.token env))

let error_on_decorators env =
  List.iter (fun decorator -> error_at env (fst decorator, Parse_error.UnsupportedDecorator))

let strict_error env e = if in_strict_mode env then error env e

let strict_error_at env (loc, e) = if in_strict_mode env then error_at env (loc, e)

let function_as_statement_error_at env loc =
  error_at env (loc, Parse_error.FunctionAsStatement { in_strict_mode = in_strict_mode env })

(* Consume zero or more tokens *)
module Eat = struct
  (* Consume a single token *)
  let token env =
    (* If there's a token_sink, emit the lexed token before moving forward *)
    (match !(env.token_sink) with
    | None -> ()
    | Some token_sink ->
      token_sink
        {
          token_loc = Peek.loc env;
          token = Peek.token env;
          (*
           * The lex mode is useful because it gives context to some
           * context-sensitive tokens.
           *
           * Some examples of such tokens include:
           *
           * `=>` - Part of an arrow function? or part of a type annotation?
           * `<`  - A less-than? Or an opening to a JSX element?
           * ...etc...
           *)
          token_context = lex_mode env;
        });

    env.lex_env := Peek.lex_env env;

    error_list env (Peek.errors env);
    env.comments := List.rev_append (Lex_result.comments (lookahead ~i:0 env)) !(env.comments);
    env.last_lex_result := Some (lookahead ~i:0 env);

    Lookahead.junk !(env.lookahead)

  (** [maybe env t] eats the next token and returns [true] if it is [t], else return [false] *)
  let maybe env t =
    if Peek.token env = t then (
      token env;
      true
    ) else
      false

  let push_lex_mode env mode =
    env.lex_mode_stack := mode :: !(env.lex_mode_stack);
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let pop_lex_mode env =
    let new_stack =
      match !(env.lex_mode_stack) with
      | _mode :: stack -> stack
      | _ -> failwith "Popping lex mode from empty stack"
    in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let double_pop_lex_mode env =
    let new_stack =
      match !(env.lex_mode_stack) with
      | _ :: _ :: stack -> stack
      | _ -> failwith "Popping lex mode from empty stack"
    in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let trailing_comments env =
    let open Loc in
    let loc = Peek.loc env in
    if Peek.token env = Token.T_COMMA && Peek.ith_is_line_terminator ~i:1 env then (
      let trailing_before_comma = Peek.comments env in
      let trailing_after_comma =
        List.filter
          (fun (comment_loc, _) -> comment_loc.start.line <= loc._end.line)
          (Lex_result.comments (lookahead ~i:1 env))
      in
      let trailing = trailing_before_comma @ trailing_after_comma in
      consume_comments_until env { Loc.line = loc._end.line + 1; column = 0 };
      trailing
    ) else
      let trailing = Peek.comments env in
      consume_comments_until env loc._end;
      trailing

  let comments_until_next_line env =
    let open Loc in
    match !(env.last_lex_result) with
    | None -> []
    | Some { Lex_result.lex_loc = last_loc; _ } ->
      let comments = Peek.comments env in
      let comments = List.filter (fun (loc, _) -> loc.start.line <= last_loc._end.line) comments in
      consume_comments_until env { line = last_loc._end.line + 1; column = 0 };
      comments

  let program_comments env =
    let open Flow_ast.Comment in
    let comments = Peek.comments env in
    let flow_directive = "@flow" in
    let flow_directive_length = String.length flow_directive in
    let contains_flow_directive { text; _ } =
      let text_length = String.length text in
      let rec contains_flow_directive_after_offset off =
        if off + flow_directive_length > text_length then
          false
        else
          String.sub text off flow_directive_length = flow_directive
          || contains_flow_directive_after_offset (off + 1)
      in
      contains_flow_directive_after_offset 0
    in
    (* Comments up through the last comment with an @flow directive are considered program comments *)
    let rec flow_directive_comments comments =
      match comments with
      | [] -> []
      | (loc, comment) :: rest ->
        if contains_flow_directive comment then (
          (env.consumed_comments_pos := Loc.(loc._end));
          List.rev ((loc, comment) :: rest)
        ) else
          flow_directive_comments rest
    in
    let program_comments = flow_directive_comments (List.rev comments) in
    let program_comments =
      if program_comments <> [] then
        program_comments
      else
        (* If there is no @flow directive, consider the first block comment a program comment if
           it starts with "/**" *)
        match comments with
        | ((loc, { kind = Block; text; _ }) as first_comment) :: _
          when String.length text >= 1 && text.[0] = '*' ->
          (env.consumed_comments_pos := Loc.(loc._end));
          [first_comment]
        | _ -> []
    in
    program_comments
end

module Expect = struct
  let error env t =
    let expected = Token.explanation_of_token ~use_article:true t in
    error_unexpected ~expected env

  let token env t =
    if Peek.token env <> t then error env t;
    Eat.token env

  (** [token_opt env T_FOO] eats a token if it is [T_FOO], and errors without consuming if not.
      This differs from [token], which always consumes. Only use [token_opt] when it's ok for
      the parser to not advance, like if you are guaranteed that something else has eaten a
      token. *)
  let token_opt env t =
    if Peek.token env <> t then
      error env t
    else
      Eat.token env

  let identifier env name =
    let t = Peek.token env in
    begin
      match t with
      | Token.T_IDENTIFIER { raw; _ } when raw = name -> ()
      | _ ->
        let expected = Printf.sprintf "the identifier `%s`" name in
        error_unexpected ~expected env
    end;
    Eat.token env
end

(* This module allows you to try parsing and rollback if you need. This is not
 * cheap and its usage is strongly discouraged *)
module Try = struct
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  type saved_state = {
    saved_errors: (Loc.t * Parse_error.t) list;
    saved_comments: Loc.t Flow_ast.Comment.t list;
    saved_last_lex_result: Lex_result.t option;
    saved_lex_mode_stack: Lex_mode.t list;
    saved_lex_env: Lex_env.t;
    saved_consumed_comments_pos: Loc.position;
    token_buffer: ((token_sink_result -> unit) * token_sink_result Queue.t) option;
  }

  let save_state env =
    let token_buffer =
      match !(env.token_sink) with
      | None -> None
      | Some orig_token_sink ->
        let buffer = Queue.create () in
        env.token_sink := Some (fun token_data -> Queue.add token_data buffer);
        Some (orig_token_sink, buffer)
    in
    {
      saved_errors = !(env.errors);
      saved_comments = !(env.comments);
      saved_last_lex_result = !(env.last_lex_result);
      saved_lex_mode_stack = !(env.lex_mode_stack);
      saved_lex_env = !(env.lex_env);
      saved_consumed_comments_pos = !(env.consumed_comments_pos);
      token_buffer;
    }

  let reset_token_sink ~flush env token_buffer_info =
    match token_buffer_info with
    | None -> ()
    | Some (orig_token_sink, token_buffer) ->
      env.token_sink := Some orig_token_sink;
      if flush then Queue.iter orig_token_sink token_buffer

  let rollback_state env saved_state =
    reset_token_sink ~flush:false env saved_state.token_buffer;
    env.errors := saved_state.saved_errors;
    env.comments := saved_state.saved_comments;
    env.last_lex_result := saved_state.saved_last_lex_result;
    env.lex_mode_stack := saved_state.saved_lex_mode_stack;
    env.lex_env := saved_state.saved_lex_env;
    env.consumed_comments_pos := saved_state.saved_consumed_comments_pos;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env);

    FailedToParse

  let success env saved_state result =
    reset_token_sink ~flush:true env saved_state.token_buffer;
    ParsedSuccessfully result

  let to_parse env parse =
    let saved_state = save_state env in
    (try success env saved_state (parse env) with Rollback -> rollback_state env saved_state)

  let or_else env ~fallback parse =
    match to_parse env parse with
    | ParsedSuccessfully result -> result
    | FailedToParse -> fallback
end
