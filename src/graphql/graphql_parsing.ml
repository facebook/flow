type state = {
  mutable source: Loc.filename option;
  mutable exprs: (Loc.t * Type.t) list;
}

let state = {
  source = None;
  exprs = [];
}

let clear_state () =
  state.source <- None;
  state.exprs <- []

let with_source source fn =
  try
    state.source <- source;
    let ret = fn () in
    clear_state ();
    ret
  with e ->
    clear_state ();
    raise e

let curr_source () = state.source

let set_loc lexbuf loc =
  Lexing.(
    let pos = loc.Loc.start in
    let lcp = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { lcp with
      pos_lnum = pos.Loc.line;
      pos_bol = -1 - pos.Loc.column;
    }
  )

let set_js_exprs exprs =
  state.exprs <- exprs

let js_expr lexbuf =
  let (loc, expr) = List.hd state.exprs in
  state.exprs <- List.tl state.exprs;
  set_loc lexbuf loc;
  expr
