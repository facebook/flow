val make_span:
  Spider_monkey_ast.Loc.position ->
  Spider_monkey_ast.Loc.position ->
  Spider_monkey_ast.Loc.t

val string_of_span: Spider_monkey_ast.Loc.t -> string

module SpanMap : Utils.MapSig with type key = Spider_monkey_ast.Loc.t
