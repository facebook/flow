open Spider_monkey_ast
open Utils

val parse_object:
  string ->
  string ->
  Expression.t SMap.t * (Loc.t * Parser_flow.Error.t) list
