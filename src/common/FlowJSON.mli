open Spider_monkey_ast
open Utils
open Utils_js

val parse_object:
  string ->
  filename ->
  Expression.t SMap.t * (Loc.t * Parser_flow.Error.t) list
