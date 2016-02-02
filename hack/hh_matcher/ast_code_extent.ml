(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils
open Sys_utils

let format_file_pos (pos : File_pos.t) : string =
  let line, bol, offset = File_pos.line_beg_offset pos in
  let file = "" in
  Printf.sprintf "%s: line num:%d beginning of line:%d char offset in file:%d"
    file line bol offset

let lexing_slice_to_string
      ((start, end_) : File_pos.t * File_pos.t)
      (code : string) : string =
  if start = end_ ||
     File_pos.is_dummy start ||
     File_pos.is_dummy end_
  then "Unable to print range."
  else
    (String.sub
       code (File_pos.offset start)
       (File_pos.offset end_ - File_pos.offset start))

let parse_file file =
  let abs_fn = Relative_path.to_absolute file in
  let content = cat abs_fn in
  let parser_out = (Parser_hack.program file content) in
  parser_out

(* required keywords we want to stop before *)
let hack_keywords =
  ["function"; "case"; "catch"; "const"; "class"; "do";
   "else"; "elseif"; "foreach"; "for"; "if"; "goto";
   "include"; "interface"; "namespace"; "new"; "newtype"; "switch";
   "throw"; "type"; "while"; "try"; "var"; "yield"]

(* optional keywords that come in an arbitrary order before
   required keywords and modify them *)
let hack_keyword_mods =
  ["abstract"; "final"; "global"; "private";
   "protected"; "public"; "static"]

(* words that cannot be class names *)
let hack_not_cname =
  ["abstract"; "and"; "as"; "bool"; "break"; "callable"; "case"; "catch";
   "class"; "clone"; "const"; "continue"; "declare"; "default"; "else";
   "elseif"; "enddeclare"; "endfor"; "endforeach"; "endif"; "endswitch";
   "endwhile"; "extends"; "final"; "finally"; "float"; "for"; "foreach";
   "function"; "global"; "goto"; "if"; "implements"; "include"; "instanceof";
   "insteadof"; "int"; "interface"; "mixed"; "namespace"; "new"; "newtype";
   "num"; "or"; "private"; "protected"; "public"; "return"; "static";
   "string"; "switch"; "throw"; "trait"; "try"; "type"; "unset"; "use";
   "var"; "while"; "xor"; "yield"]

(*======================= Lexing helpers =========================*)

type lex_env = {
    (* its a stack, because if we have an expression within a string
       we want to lex it as an expression, but once we're done with that
       we want to go back to lexing the string*)
    next_tok_fn : (Relative_path.t -> Lexing.lexbuf -> Lexer_hack.token) list;
    next_tok_upd_fn : (Lexer_hack.token -> lex_env ->
                       Relative_path.t -> Lexing.lexbuf -> lex_env) list;
    last_token : Lexer_hack.token;
    last_lexeme : string;
  }

let look_ahead (lb : Lexing.lexbuf) (f : Lexing.lexbuf -> 'a) : 'a =
  let saved = Parser_hack.save_lexbuf_state lb in
  let ret = f lb in
  Parser_hack.restore_lexbuf_state lb saved;
  ret

let is_xhp file lexbuf lex_env =
  let is_keyword lexeme =
    List.exists (fun keyword -> lexeme = keyword) hack_not_cname in
  let looks_like_xhp = look_ahead lexbuf (fun lb ->
    let tok = Lexer_hack.xhpname file lb in
    tok = Lexer_hack.Txhpname &&
    let tok2 = Lexer_hack.xhpattr file lb in
    tok2 = Lexer_hack.Tgt || tok2 = Lexer_hack.Tword ||
    (tok2 = Lexer_hack.Tslash &&
       Lexer_hack.xhpattr file lb = Lexer_hack.Tgt)) in
  looks_like_xhp &&
    (* isnt something like Vector<string> *)
    (lex_env.last_token <> Lexer_hack.Tword ||
       is_keyword lex_env.last_lexeme)

(* determines if a { in a string is an expression or a { literal *)
let is_encapsed_expr file lexbuf lex_env =
  lex_env.last_token = Lexer_hack.Tdollar ||
  look_ahead lexbuf
    (fun lb ->
     let tok = Lexer_hack.string2 file lb in
     tok = Lexer_hack.Tdollar || tok = Lexer_hack.Tlvar)

let pop_lex_env env =
  { env with next_tok_fn = List.tl env.next_tok_fn;
    next_tok_upd_fn = List.tl env.next_tok_upd_fn }

let push_lex_env env tf tuf =
  { env with next_tok_fn = tf :: env.next_tok_fn;
    next_tok_upd_fn = tuf :: env.next_tok_upd_fn }

let rec get_token_string file lexbuf = Lexer_hack.string file lexbuf
and upd_fn_string tok lex_env _ _ =
  match tok with
  | Lexer_hack.Tquote -> pop_lex_env lex_env
  | _ -> lex_env

and get_token_string2 file lexbuf = Lexer_hack.string2 file lexbuf
and upd_fn_string2 tok lex_env file lexbuf =
  match tok with
  | Lexer_hack.Tdquote -> pop_lex_env lex_env
  | Lexer_hack.Tlcb ->
     (* we got {, is an expr iff last was $ or next is $ or lvar *)
     if is_encapsed_expr file lexbuf lex_env
     then push_lex_env lex_env plain_get_token upd_fn_rcb
     else lex_env
  | _ -> lex_env

(* xhp strings can't contain exprs*)
and upd_fn_xhp_string tok lex_env _ _ =
  match tok with
  | Lexer_hack.Tdquote -> pop_lex_env lex_env
  | _ -> lex_env

and upd_fn_rcb tok lex_env _ _ =
  match tok with
  | Lexer_hack.Trcb -> pop_lex_env lex_env
  | _ -> lex_env

and get_token_xhp file lexbuf = Lexer_hack.xhpname file lexbuf
and upd_fn_xhp tok lex_env _ _ =
  match tok with
  | Lexer_hack.Txhpname ->
     let le = pop_lex_env lex_env in
     push_lex_env le get_token_xhp_attr upd_fn_xhp_attr
  | _ -> failwith "Wat"

and get_token_xhp_attr file lexbuf = begin
  let tok = Lexer_hack.xhpattr file lexbuf in
  (match tok with
  | Lexer_hack.Tslash ->
     (* if slash, also go to gt but return slash; for dealing with
        the /> at the end of xhp things *)
     ignore(Lexer_hack.xhpattr file lexbuf)
  | _ -> ());
  tok end
and upd_fn_xhp_attr tok lex_env _ _ =
  match tok with
  | Lexer_hack.Tslash -> pop_lex_env lex_env
  | Lexer_hack.Tgt ->
     let le = pop_lex_env lex_env in
     push_lex_env le get_token_xhp_body upd_fn_xhp_body
  | Lexer_hack.Tword ->
     push_lex_env lex_env get_token_xhp_attr_value upd_fn_xhp_attr_value
  | _ -> lex_env

and get_token_xhp_attr_value file lexbuf =
  Lexer_hack.xhpattr file lexbuf
and upd_fn_xhp_attr_value tok lex_env _ _ =
  match tok with
  | Lexer_hack.Trcb -> pop_lex_env lex_env
  | Lexer_hack.Tdquote ->
     let le = pop_lex_env lex_env in
     push_lex_env le get_token_string2 upd_fn_xhp_string
  | _ -> lex_env

and get_token_xhp_body file lexbuf =
  Lexer_hack.xhptoken file lexbuf

and upd_fn_gt tok lex_env _ _ =
  match tok with
  | Lexer_hack.Tgt -> pop_lex_env lex_env
  | _ -> lex_env

and upd_fn_xhp_body tok lex_env file lexbuf =
  match tok with
  | Lexer_hack.Tlcb -> push_lex_env lex_env plain_get_token upd_fn_rcb
  | Lexer_hack.Tlt ->
     if is_xhp file lexbuf lex_env
     then push_lex_env lex_env get_token_xhp upd_fn_xhp
     else (* must be </xhpname> *)
       let le = pop_lex_env lex_env in
       push_lex_env le get_token_xhp_body upd_fn_gt
  | Lexer_hack.Topen_xhp_comment ->
     push_lex_env lex_env get_tok_xhp_comment upd_fn_xhp_comment
  | _ -> lex_env

(* skip these *)
and get_tok_xhp_comment file lexbuf = begin
  Lexer_hack.xhp_comment file lexbuf;
  Lexer_hack.Topen_xhp_comment end
and upd_fn_xhp_comment _ lex_env _ _ = pop_lex_env lex_env

and get_tok_heredoc _ lexbuf = Lexer_hack.heredoc_token lexbuf

(* This is so that we can know what token we end the heredoc on *)
and upd_fn_heredoc_hd _ lex_env _ lexbuf =
  let env = pop_lex_env lex_env in
  let lexeme = Lexing.lexeme lexbuf in
  push_lex_env env get_tok_heredoc (upd_fn_heredoc_body lexeme)

(* pop when we find the end of the heredoc *)
and upd_fn_heredoc_body to_stop tok lex_env _ lexbuf =
  (* need newline, to_stop, semicolon to stop *)
  if lex_env.last_token = Lexer_hack.Tnewline &&
     tok = Lexer_hack.Tword &&
     Lexing.lexeme lexbuf = to_stop
  then
    let lookaheadres =
      look_ahead
        lexbuf
        (fun lexbuf ->
         match Lexer_hack.heredoc_token lexbuf with
         | Lexer_hack.Tsc -> true
         | _ -> false) in
    if lookaheadres
    then pop_lex_env lex_env
    else lex_env
  else lex_env

and plain_get_token file lexbuf = Lexer_hack.token file lexbuf
and plain_upd_fn tok lex_env file lexbuf =
  match tok with
  | Lexer_hack.Tquote -> push_lex_env lex_env get_token_string upd_fn_string
  | Lexer_hack.Tdquote -> push_lex_env lex_env get_token_string2 upd_fn_string2
  | Lexer_hack.Tlt when is_xhp file lexbuf lex_env ->
     push_lex_env lex_env get_token_xhp upd_fn_xhp
  | Lexer_hack.Theredoc ->
     push_lex_env lex_env plain_get_token upd_fn_heredoc_hd
  | _ -> lex_env

let default_lex_env =
  { next_tok_fn = [plain_get_token];
    next_tok_upd_fn = [plain_upd_fn];
    (* random token as dummy *)
    last_token = Lexer_hack.Tword;
    last_lexeme = "" }

(* gets a token from the lexbuf, handling the cases where
   a string is received from the lexbuf by returning the
   ending quote
   Assumes that the code being lexed is syntactically correct
   WARNING side effect of advancing lexbuf *)
let token_from_lb
      (file : Relative_path.t)
      (lexbuf : Lexing.lexbuf)
      (lex_env : lex_env) :
      (Lexer_hack.token * Pos.t) * lex_env =
  let tok_fn = List.hd lex_env.next_tok_fn in
  let token = tok_fn file lexbuf in
  let lex_env = (List.hd lex_env.next_tok_upd_fn) token lex_env file lexbuf in
  let position = Pos.make file lexbuf in
  let lex_env = { lex_env with
                  last_token = token;
                  last_lexeme = Lexing.lexeme lexbuf } in
  (token, position), lex_env

(* ================== End Lexing Helpers ============================ *)

(* comparison that accommodate File_pos.dummy *)
let position_min
      (pos1 : File_pos.t)
      (pos2 : File_pos.t) : File_pos.t =
  if File_pos.is_dummy pos2 ||
       (File_pos.compare pos1 pos2 < 0 && not (File_pos.is_dummy pos1))
  then pos1
  else pos2
let position_max
      (pos1 : File_pos.t)
      (pos2 : File_pos.t) : File_pos.t =
  (* dummy is smaller than valid positions *)
  if File_pos.compare pos1 pos2 < 0 then pos2 else pos1

type range_accum = {
    (* source positions of leftmost, rightmost leaf *)
    left : File_pos.t;
    right : File_pos.t;
    (* So we can do matching of open curly braces and parentheses
       starting from the first identifier.
       Using the list of positions is necessary so that we can match
       delimiters in the case that there are two opening delimiters
       before the first identifier:
       try {
         do {
            ...
         } while (...);
       }
       in this case if we get code extent of the try block, we must
       match both opening delimiters which happen before the first identifier *)
    seen_first_id : bool;
    unmatched_lcb : File_pos.t list;
    unmatched_lp : File_pos.t list;
    (* this is so that we can correctly ignore keywords in situations where we
       don't want to be stopping on the last keyword before our target *)
    ignore_keywords : bool;
    (* Store the lexing env *)
    lex_env : lex_env;
  }
(* currently matches curly braces except when the first identifier is inside of
    a block that we want to match curly braces for (e.g. closures, try block) *)

let default_range_accum =
  { left = File_pos.dummy;
    right = File_pos.dummy;
    seen_first_id = false;
    unmatched_lcb = [];
    unmatched_lp = [];
    ignore_keywords = false;
    lex_env = default_lex_env;
  }

(* advances lexbuf to the specified position,
   returning a quad
   fst = the position corresponding to the start
   of the construct at the position specified
   (sensitive to keywords)
   snd = pair of: list (stack) of unmatched left curly braces,
           list (stack) of unmatched left parentheses
   (for matching delimiters)
   3rd = whether or not we need the leading opening parenthesis if it
   exists (to fix an off-by-one)
   4th = modified lex_env for correct lexing *)
let advance_lexbuf_to_pos
      ~(file : Relative_path.t)
      ~(lexbuf : Lexing.lexbuf)
      ~(goal_test : Pos.t -> bool)
      ~(unmatched_lcb : File_pos.t list)
      ~(unmatched_lp : File_pos.t list)
      ~(ignore_keywords : bool)
      ~(lex_env : lex_env) :
      (File_pos.t *
         (File_pos.t list * File_pos.t list) *
           lex_env) =
  (* make sure not to consume tokens if already at/past goal *)
  let curpos = Pos.make file lexbuf in
  if goal_test curpos
  then (File_pos.dummy, (unmatched_lcb, unmatched_lp), lex_env)
  else
  let rec advance_until
            (leftmost_modifier : File_pos.t)
            (seen_keyword : bool)
            (unmatched_lcb : File_pos.t list)
            (unmatched_lp : File_pos.t list)
            (lex_env : lex_env) :
            (File_pos.t *
               (File_pos.t list * File_pos.t list) *
                 lex_env) =
    let (token,pos), lex_env = token_from_lb file lexbuf lex_env in
    (* update the delimiters we've seen *)
    (* dealing with empty list case is necessary because of weird off-by-one
       thing with positions + skipping opening parens
       (e.g. matching increment of for loop) *)
    let pop_hd = function
      | [] -> []
      | _ :: tl -> tl in
    let unmatched_lcb =
      match token with
      | Lexer_hack.Tlcb -> Pos.pos_start pos:: unmatched_lcb
      | Lexer_hack.Trcb -> pop_hd unmatched_lcb
      | _ -> unmatched_lcb in
    let unmatched_lp =
      match token with
      | Lexer_hack.Tlp -> Pos.pos_start pos :: unmatched_lp
      | Lexer_hack.Trp -> pop_hd unmatched_lp
      | _ -> unmatched_lp in
    let cur_lexeme = Lexing.lexeme lexbuf in
    if token = Lexer_hack.Teof
    then Pos.pos_end pos, (unmatched_lcb, unmatched_lp), lex_env
    else
    if goal_test pos
    then
      if not (File_pos.is_dummy leftmost_modifier)
      then (leftmost_modifier, (unmatched_lcb, unmatched_lp), lex_env)
      else (Pos.pos_start pos, (unmatched_lcb, unmatched_lp), lex_env)
    else
      (* these are to make sure we include modifiers and keywords in the
         code extent we return for the AST node because the first identifier
         we see is often after these keywords and modifiers. *)
      let cur_is_modifier =
        List.exists
          (fun modifier -> cur_lexeme = modifier)
          hack_keyword_mods &&
          not ignore_keywords in
      let cur_is_keyword =
        List.exists
          (fun keyword -> cur_lexeme = keyword)
          hack_keywords &&
          not ignore_keywords in
      if seen_keyword
      (* a modifier or keyword will cause a reset *)
      then
        if cur_is_modifier || cur_is_keyword
        then
          advance_until
            (Pos.pos_start pos)
            cur_is_keyword unmatched_lcb unmatched_lp lex_env
        else
          advance_until
            leftmost_modifier true unmatched_lcb unmatched_lp lex_env
      else
        if cur_is_keyword
        then
          advance_until
            (Pos.pos_start pos) true unmatched_lcb unmatched_lp lex_env
        else
          if cur_is_modifier
          then
            advance_until
              (if File_pos.is_dummy leftmost_modifier
               then (Pos.pos_start pos)
               else leftmost_modifier)
              false
              unmatched_lcb
              unmatched_lp
              lex_env
          else
            advance_until
              File_pos.dummy false unmatched_lcb unmatched_lp lex_env in
  advance_until File_pos.dummy false unmatched_lcb unmatched_lp lex_env

(* skips to necessary number of right curly braces
   and any semicolons after that *)
let skip_trailing_delims
      (lexbuf : Lexing.lexbuf)
      (lcb_stack : File_pos.t list)
      (lp_stack : File_pos.t list)
      (file : Relative_path.t)
      (lex_env : lex_env) :
      File_pos.t =
  begin
  let rec go_until
            (lex_env : lex_env)
            (lcb_stack : File_pos.t list)
            (lp_stack : File_pos.t list) =
    match lcb_stack, lp_stack with
    | [], [] -> File_pos.of_lexing_pos (Lexing.lexeme_end_p lexbuf)
    | _, _ ->
      let (token,pos), lex_env = token_from_lb file lexbuf lex_env in
      if token = Lexer_hack.Teof then Pos.pos_end pos else
      match token with
      | Lexer_hack.Trcb->
         go_until
           lex_env
           (if List.length lcb_stack > 0
            then List.tl lcb_stack
            else [])
           lp_stack
      | Lexer_hack.Trp ->
         go_until
           lex_env
           lcb_stack
           (if List.length lp_stack > 0
            then List.tl lp_stack
            else [])
      | _ -> go_until lex_env lcb_stack lp_stack in
  go_until lex_env lcb_stack lp_stack
  end

(* for making sure we consume the template type arg in typehints
   e.g. for Typename<string> *)
let hint_skip_trailing
      (lexbuf : Lexing.lexbuf)
      (file : Relative_path.t)
      (lex_env : lex_env) : File_pos.t =
  let cur_end = File_pos.of_lexing_pos (Lexing.lexeme_end_p lexbuf) in
  (* cur_end computed here because of side effects in token_from_lb *)
  let (next_tok,_), lex_env = token_from_lb file lexbuf lex_env in
  if next_tok <> Lexer_hack.Tlt
  then cur_end
  else
    (* tparam count is how many type parameters we're inside of
       in case of Type<Type<type>> *)
    let rec go_until lex_env tparam_count =
      let (next_tok, next_pos), lex_env = token_from_lb file lexbuf lex_env in
      match next_tok with
      | Lexer_hack.Tlt -> go_until lex_env (tparam_count + 1)
      | Lexer_hack.Tgt ->
         if tparam_count = 1
         then Pos.pos_end next_pos
         else go_until lex_env (tparam_count - 1)
      | Lexer_hack.Tgtgt ->
         if tparam_count <= 2
         then Pos.pos_end next_pos
         else go_until lex_env (tparam_count - 2)
      | Lexer_hack.Teof -> failwith "unclosed '<', expected '>'"
      | _ -> go_until lex_env tparam_count in
    go_until lex_env 1

let update_to_enclosing
      (accum : range_accum)
      (pos : File_pos.t) : range_accum =
  { accum with left = (position_min pos accum.left);
               right = (position_max pos accum.right); }

class range_find_visitor rel_file content =
object (this)
  inherit [range_accum] Ast_visitor.ast_visitor as super

  val file = rel_file;
  val lexbuf = Lexing.from_string (content);

  method private advance_lexbuf_and_update_left acc pos =
    begin
      let goal_test_left cur_pos =
        (Pos.pos_start cur_pos) >= (Pos.pos_start pos) in
      let (start, (unmatched_lcb, unmatched_lp), new_env) =
        advance_lexbuf_to_pos
          ~file:file
          ~lexbuf:lexbuf
          ~goal_test:goal_test_left
          ~unmatched_lcb:acc.unmatched_lcb
          ~unmatched_lp:acc.unmatched_lp
          ~ignore_keywords:acc.ignore_keywords
          ~lex_env:acc.lex_env in
      let acc = { acc with lex_env = new_env } in
      let old_left = acc.left in
      (* this is to address a particular off-by-one error with parentheses when
         matching cases like the first expression in a for loop:
         $a from for ($a = 1; ...).
         NOTE: for ( $a = 1; ...) works correctly without this, we cannot change
         the advance_lexbuf method to fix the case or other things break. *)
      let minpos =
        position_min (position_min start (Pos.pos_start pos)) acc.left in
      let acc =
        if acc.unmatched_lp = [] && acc.unmatched_lcb = [] &&
             Pos.start_cnum pos - File_pos.offset minpos = 1 &&
               File_pos.is_dummy old_left
        then { acc with left = Pos.pos_start pos }
        else update_to_enclosing acc (minpos) in
      let new_left = acc.left in
      if new_left <> old_left
      then
        (* if we change the leftmost end of our code extent,
           forget any opening delimiters outside of that range *)
        let rec find_first_relevant_delim
                  (delim_stack : File_pos.t list) =
          match delim_stack with
          | [] -> []
          | hd :: tl ->
             if File_pos.compare new_left hd < 0
             then List.rev (hd :: tl)
             else find_first_relevant_delim tl in
        let unmatched_lcb =
          find_first_relevant_delim (List.rev unmatched_lcb) in
        let unmatched_lp =
          find_first_relevant_delim (List.rev unmatched_lp) in
        { acc with
          seen_first_id = true;
          unmatched_lcb = unmatched_lcb;
          unmatched_lp = unmatched_lp }
      else
        { acc with
          unmatched_lcb = unmatched_lcb;
          unmatched_lp = unmatched_lp }
    end


  method private advance_lexbuf_and_update_right acc pos =
    begin
      let goal_test_right cur_pos =
        Pos.pos_end cur_pos >= Pos.pos_end pos in
      let (_start, (new_unmatched_lcb, new_unmatched_lp), new_env) =
        advance_lexbuf_to_pos
          ~file:file
          ~lexbuf:lexbuf
          ~goal_test:goal_test_right
          ~unmatched_lcb:acc.unmatched_lcb
          ~unmatched_lp:acc.unmatched_lp
          ~ignore_keywords:acc.ignore_keywords
          ~lex_env:acc.lex_env in
      let acc =
        { acc with
          unmatched_lcb = new_unmatched_lcb;
          unmatched_lp = new_unmatched_lp;
          lex_env = new_env } in
      let acc = update_to_enclosing acc (Pos.pos_end pos) in
      acc
    end

  method get_lexbuf () = lexbuf

  (* override only methods for AST pieces that have a position *)
  method! on_id acc id =
    begin
      let acc = this#advance_lexbuf_and_update_left acc (fst id) in
      let acc = super#on_id acc id in
      let acc = this#advance_lexbuf_and_update_right acc (fst id) in
      acc
    end

  method! on_pstring acc pstr =
    begin
      let acc = this#advance_lexbuf_and_update_left acc (fst pstr) in
      let acc = super#on_pstring acc pstr in
      let acc = this#advance_lexbuf_and_update_right acc (fst pstr) in
      acc
    end

  method! on_hint acc h =
    begin
      let acc = this#advance_lexbuf_and_update_left acc (fst h) in
      let acc = super#on_hint acc h in
      let acc = this#advance_lexbuf_and_update_right acc (fst h) in
      acc
    end

  method! on_expr acc e =
    begin
      let acc = this#advance_lexbuf_and_update_left acc (fst e) in
      let acc = super#on_expr acc e in
      let acc = this#advance_lexbuf_and_update_right acc (fst e) in
      acc
    end

  method! on_break acc p =
    begin
      let acc = this#advance_lexbuf_and_update_left acc p in
      let acc = super#on_break acc p in
      let acc = this#advance_lexbuf_and_update_right acc p in
      acc
    end

  method! on_continue acc p =
    begin
      let acc = this#advance_lexbuf_and_update_left acc p in
      let acc = super#on_continue acc p in
      let acc = this#advance_lexbuf_and_update_right acc p in
      acc
    end

  method! on_return acc p eopt =
    begin
      let acc = this#advance_lexbuf_and_update_left acc p in
      let acc = super#on_return acc p eopt in
      let acc = this#advance_lexbuf_and_update_right acc p in
      acc
    end
end

(* methods exposed by module for finding source extent *)

let source_extent_catch file source c =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_catch default_range_accum c in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file
                accum.lex_env in
  (accum.left, right)

let source_extent_do file source b e =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_do default_range_accum b e in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_efun file source f ibl =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_efun
                { default_range_accum with ignore_keywords = false } f ibl in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_eif file source e1 eopt e2 =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_eif default_range_accum e1 eopt e2 in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_expr file source e =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_expr
                { default_range_accum with ignore_keywords = true } e in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_for file source e1 e2 e3 b =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_for default_range_accum e1 e2 e3 b in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_foreach file source e popt ae b =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_foreach default_range_accum e popt ae b in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_hint file source h =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_hint
                { default_range_accum with ignore_keywords = true } h in
  let right = hint_skip_trailing
                (visitor#get_lexbuf ())
                file accum.lex_env in
  (accum.left, right)

let source_extent_if file source e b1 b2 =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_if default_range_accum e b1 b2 in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_lfun file source f =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_lfun
                { default_range_accum with ignore_keywords = true } f in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

(* some stmts have keywords that we want to include, others don't *)
let rec find_should_ignore_stmt (stmt : Ast.stmt) : bool =
  match stmt with
  | Ast.Unsafe | Ast.Fallthrough | Ast.Expr _ | Ast.Break _ | Ast.Continue _
  | Ast.Return (_, _) | Ast.Static_var _ | Ast.Noop -> true
  | Ast.Block sl -> begin
     match sl with
     | [] -> true
     | hd :: _ -> find_should_ignore_stmt hd end
  | _ -> false

let source_extent_stmt file source s =
  let visitor = new range_find_visitor file source in
  let accum =
    visitor#on_stmt
      { default_range_accum with
        ignore_keywords = find_should_ignore_stmt s } s in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_switch file source e cl =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_switch default_range_accum e cl in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_throw file source e =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_throw default_range_accum e in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_try file source b1 cl b2 =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_try default_range_accum b1 cl b2 in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_while file source e b =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_while default_range_accum e b in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_class_ file source c =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_class_ default_range_accum c in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)


let source_extent_def
      (file:Relative_path.t)
      (source:string)
      (def:Ast.def) :
      (File_pos.t * File_pos.t) =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_def default_range_accum def in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_fun_ file source f =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_fun_ default_range_accum f in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)

let source_extent_method_ file source m = begin
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_method_ default_range_accum m in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right) end

let source_extent_program file source p =
  let visitor = new range_find_visitor file source in
  let accum = visitor#on_program default_range_accum p in
  let right = skip_trailing_delims
                (visitor#get_lexbuf ())
                accum.unmatched_lcb
                accum.unmatched_lp
                file accum.lex_env in
  (accum.left, right)
