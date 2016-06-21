module Token = struct
  type t =
    | T_NAME
    | T_LCURLY
    | T_RCURLY
end
open Token

module Lex_result = struct
  type t = {
    lex_token: Token.t;
    lex_loc: Loc.t;
    lex_value: string;
  }

  let token result = result.lex_token
  let loc result = result.lex_loc
  let value result = result.lex_value
end

let token_to_string result =
  match Lex_result.token result with
  | T_NAME -> "T_NAME(" ^ (Lex_result.value result) ^ ")"
  | T_LCURLY -> "T_LCURLY"
  | T_RCURLY -> "T_RCURLY"
;;

type state = {
  str: string;
  mutable pos: int;

  source: Loc.filename option;
  mutable line: int;
  mutable column: int;
  mutable offset: int;
}

let pos_from_state s =
  {
    Loc.line = s.line;
    column = s.column;
    offset = s.offset;
  }

let read_token s tok len =
  let start = pos_from_state s in
  let lex_value = String.sub s.str s.pos len in

  s.pos <- s.pos + len;
  s.column <- s.column + len;
  s.offset <- s.offset + len;

  Lex_result.{
    lex_token = tok;
    lex_loc = {
      Loc.source = s.source;
      start;
      _end = pos_from_state s;
    };
    lex_value;
  }
;;

let rec read_tokens str loc =
  let start = loc.Loc.start in
  let s = {
    str;
    pos = 0;

    source = loc.Loc.source;
    line = start.Loc.line;
    column = start.Loc.column;
    offset = start.Loc.offset;
  } in
  let rec helper acc =
    if String.length s.str > s.pos then
      match String.get s.str s.pos with
      | 'a'..'z'
      | 'A'..'Z'
      | '_' -> helper (read_name s :: acc)
      | '{' -> helper (read_token s T_LCURLY 1 :: acc)
      | '}' -> helper (read_token s T_RCURLY 1 :: acc)
      | ' ' ->
        s.pos <- s.pos + 1;
        s.column <- s.column + 1;
        s.offset <- s.offset + 1;
        helper acc
      | '\n' ->
        s.pos <- s.pos + 1;
        s.line <- s.line + 1;
        s.column <- 0;
        s.offset <- s.offset + 1;
        helper acc
      | x -> failwith ("Unknown char: `" ^ (Char.escaped x) ^ "`")
    else acc
  in
  helper []

and read_name s =
  let rec find_end pos =
    match String.get s.str pos with
    | 'a'..'z'
    | 'A'..'Z'
    | '-'
    | '0'..'9' -> find_end (pos + 1)
    | _ -> pos
  in
  let end_pos = find_end s.pos in
  read_token s T_NAME (end_pos - s.pos)
;;

let lex_str src loc =
  List.rev (read_tokens src loc)
