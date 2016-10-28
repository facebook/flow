val with_source: Loc.filename option -> (unit -> 'a) -> 'a

val curr_source: unit -> Loc.filename option

val set_loc: Lexing.lexbuf -> Loc.t -> unit

val set_js_exprs: (Loc.t * Type.t) list -> unit
val js_expr: Lexing.lexbuf -> Type.t
