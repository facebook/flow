open Utils

module Ast = Spider_monkey_ast

val parse_object:
  string ->
  string ->
  Ast.Expression.t SMap.t * (Ast.Loc.t * Parser_flow.Error.t) list
