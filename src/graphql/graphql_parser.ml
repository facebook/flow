module Ast = Graphql_ast
module Tok = Graphql_lexer.Token
module Lex_result = Graphql_lexer.Lex_result
module TemplateLiteral = Spider_monkey_ast.Expression.TemplateLiteral

exception Unexpected_token of (Loc.t * string)

type result =
  | ResTok of Lex_result.t
  | ResExpr of Spider_monkey_ast.Expression.t

type state = {
  tokens: result array;
  mutable pos: int;
}

let advance s = s.pos <- (s.pos + 1)

let unexpected s =
  let (loc, str) = match Array.get s.tokens s.pos with
    | ResTok tok ->
      Lex_result.loc tok, Tok.string_of_token (Lex_result.token tok)
    | ResExpr (loc, _) -> (loc, "expression") in
  Unexpected_token (loc, str)

module Peek = struct
  let result s = Array.get s.tokens s.pos

  let _res_tok s = match Array.get s.tokens s.pos with
    | ResTok x -> x
    | ResExpr (loc, _) ->
      failwith ("Unexpected expression at " ^ (Loc.to_string loc))

  let token s = Lex_result.token (_res_tok s)
  let value s = Lex_result.value (_res_tok s)
  let loc s = Lex_result.loc (_res_tok s)

  let is_eof s = Array.length s.tokens = s.pos
end

module Expect = struct
  let tok_and_val s tok value =
    if tok = Peek.token s && value = Peek.value s
    then advance s
    else raise (unexpected s)

  let tok s tok =
    if tok = Peek.token s
    then advance s
    else raise (unexpected s)
end

let rec parse_document s: Ast.Document.t =
  let rec parse_def s acc =
    if Peek.is_eof s then acc
    else parse_def s (parse_definition s :: acc)
  in
  Ast.Document.{
    definitions = List.rev (parse_def s [])
  }

and parse_definition s =
  match Peek.token s, Peek.value s with
  | Tok.T_NAME, "fragment" ->
    Ast.Document.FragmentDefinition (parse_fragment_definition s)
  | Tok.T_NAME, _ ->
    Ast.Document.OperationDefinition (parse_operation_definition s)
  | _ -> raise (unexpected s)

and parse_operation_definition s: Ast.OperationDefinition.t =
  let name =
    match Peek.token s with
    | Tok.T_NAME ->
      let name = Peek.value s in
      advance s;
      name
    | _ -> raise (unexpected s)
  in
  let selectionSet = parse_selection_set s in
  Ast.OperationDefinition.{
    operation = Query;
    name;
    selectionSet;
  }

and parse_fragment_definition s: Ast.FragmentDefinition.t =
  Expect.tok_and_val s Tok.T_NAME "fragment";

  let name = match Peek.token s, Peek.value s with
    | Tok.T_NAME, name when name <> "on" -> parse_name s
    | _ -> Loc.none, "" in
  Expect.tok_and_val s Tok.T_NAME "on";
  let typeName = parse_name s in

  let selectionSet = parse_selection_set s in

  Ast.FragmentDefinition.{
    name;
    typeName;
    selectionSet;
  }

and parse_name s: Ast.Name.t =
  match Peek.token s, Peek.value s, Peek.loc s with
  | Tok.T_NAME, name, loc ->
    advance s;
    loc, name
  | _ -> raise (unexpected s)

and parse_selection_set s: Ast.SelectionSet.t =
  let open Ast.SelectionSet in
  let rec helper fields =
    match Peek.result s with
    | ResTok t ->
      begin match Lex_result.token t with
      | Tok.T_NAME -> helper (Field (parse_field s) :: fields)
      | Tok.T_RCURLY ->
        advance s;
        List.rev fields
      | _ -> raise (unexpected s)
      end
    | ResExpr expr ->
      advance s;
      helper (Expression expr :: fields)
  in
  Expect.tok s Tok.T_LCURLY;
  {
    selections = helper []
  }

and parse_field s: Ast.Field.t =
  let (loc, name) =
    match Peek.token s with
    | Tok.T_NAME ->
      let name = Peek.value s in
      let loc = Peek.loc s in
      advance s;
      (loc, name)
    | _ -> raise (unexpected s)
  in
  let selectionSet =
    match Peek.result s with
    | ResTok _ ->
      begin match Peek.token s with
      | Tok.T_LCURLY -> Some (parse_selection_set s)
      | _ -> None
      end
    | _ -> None
  in

  Ast.Field.{
    name;
    selectionSet;
    loc;
  }

let string_to_tokens str loc =
  let lexbuf = Lexing.from_string str in
  let rec next acc =
    match Graphql_lexer.read lexbuf loc with
    | {Lex_result.lex_token = Tok.T_EOF; _} -> acc
    | {Lex_result.lex_token = Tok.T_ILLEGAL; lex_loc; _} ->
      raise (Unexpected_token (lex_loc, "illegal char"))
    | res -> next (res :: acc)
  in
  next []

let parse_template (literal: TemplateLiteral.t) =
  let open TemplateLiteral in
  let rec eat_string quasis expressions acc =
    match quasis with
    | (loc, { Element.value = {Element.cooked; _}; _ }) :: quasis ->
      let tokens = string_to_tokens cooked loc in
      let tokens = List.map (fun x -> ResTok x) tokens in
      let acc = List.append tokens acc in
      eat_expr quasis expressions acc
    | _ -> failwith "Quasis cannot be empty."
  and eat_expr quasis expressions acc =
    match expressions with
    | [] -> acc
    | expr :: expressions ->
      eat_string quasis expressions (ResExpr expr :: acc)
  in

  let tokens = eat_string literal.quasis literal.expressions [] in
  let tokens = List.rev tokens in

  parse_document {tokens = Array.of_list tokens; pos = 0}
