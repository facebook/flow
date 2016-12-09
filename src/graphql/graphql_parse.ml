exception SyntaxError of Loc.t

let parse_file src path =
  let lexbuf = Lexing.from_string src in
  try
    Graphql_parser.doc Graphql_lexer.token lexbuf
  with
    | Parsing.Parse_error ->
      raise (SyntaxError (Loc.from_lb (Some (Loc.ResourceFile path)) lexbuf))

let parse_tpl quasis exprs source =
  let exprs =
    List.map2 (fun (loc, _) expr -> (loc, expr)) (List.tl quasis) exprs in
  let src =
    quasis |> List.map (fun (_, str) -> str) |> String.concat "<__JS_EXP>" in
  let lexbuf = Lexing.from_string src in
  let (loc, _) = List.hd quasis in
  try
    Graphql_parsing.with_source source (fun () ->
      Graphql_parsing.set_js_exprs exprs;
      Graphql_parsing.set_loc lexbuf loc;
      Graphql_parser.doc Graphql_lexer.token lexbuf
    )
  with
    | Parsing.Parse_error ->
      raise (SyntaxError (Loc.from_lb loc.Loc.source lexbuf))
