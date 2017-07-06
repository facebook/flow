val make_span:
  Loc.position ->
  Loc.position ->
  Loc.t

val string_of_span: Loc.t -> string

module SpanMap : MyMap.S with type key = Loc.t
