{
(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(*****************************************************************************)
(* Comments accumulator. *)
(*****************************************************************************)

let (comment_list: (Pos.t * string) list ref) = ref []

(*****************************************************************************)
(* Fixmes accumulators *)
(*****************************************************************************)
let (fixmes: Pos.t IMap.t IMap.t ref) = ref IMap.empty

let add_fixme err_nbr pos =
  let line, _, _ = Pos.info_pos pos in
  let line_value =
    match IMap.get line !fixmes with
    | None -> IMap.empty
    | Some x -> x
  in
  fixmes := IMap.add line (IMap.add err_nbr pos line_value) !fixmes;
  ()

(*****************************************************************************)
(* The type for tokens. Some of them don't represent "real" tokens coming
 * from the buffer. For example Terror can be used to tag an error, or Tyield
 * doesn't really correspond to a string, it's just there to encode the
 * priority.
 *)
(*****************************************************************************)

type token =
  | Tlvar
  | Tint
  | Tfloat
  | Tat
  | Tclose_php
  | Tword
  | Tbacktick
  | Tphp
  | Thh
  | Tlp
  | Trp
  | Tsc
  | Tcolon
  | Tcolcol
  | Tcomma
  | Teq
  | Tbareq
  | Tpluseq
  | Tstareq
  | Tslasheq
  | Tdoteq
  | Tminuseq
  | Tpercenteq
  | Txoreq
  | Tampeq
  | Tlshifteq
  | Trshifteq
  | Teqeq
  | Teqeqeq
  | Tdiff
  | Tdiff2
  | Tbar
  | Tbarbar
  | Tampamp
  | Tplus
  | Tminus
  | Tstar
  | Tstarstar
  | Tslash
  | Tbslash
  | Txor
  | Tlcb
  | Trcb
  | Tlb
  | Trb
  | Tdot
  | Tlte
  | Tlt
  | Tgt
  | Tgte
  | Tltlt
  | Tgtgt
  | Tsarrow
  | Tnsarrow
  | Tarrow
  | Tlambda
  | Tem
  | Tqm
  | Tamp
  | Ttild
  | Tincr
  | Tdecr
  | Tunderscore
  | Trequired
  | Tellipsis
  | Tdollar
  | Tpercent
  | Teof
  | Tquote
  | Tdquote
  | Tunsafe
  | Tunsafeexpr
  | Tfallthrough
  | Theredoc
  | Txhpname
  | Tref
  | Tspace
  | Topen_comment
  | Tclose_comment
  | Tline_comment
  | Topen_xhp_comment
  | Tclose_xhp_comment

(* Fake tokens *)
  | Tyield
  | Tawait
  | Timport
  | Teval
  | Tprint
  | Tinstanceof
  | Tnew
  | Tclone
  | Telseif
  | Telse
  | Tendif
  | Tcast
  | Terror
  | Tnewline
  | Tany

(*****************************************************************************)
(* Backtracking. *)
(*****************************************************************************)

let yyback n lexbuf =
  Lexing.(
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
  let currp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { currp with pos_cnum = currp.pos_cnum - n }
 )

let back lb =
  let n = Lexing.lexeme_end lb - Lexing.lexeme_start lb in
  yyback n lb

(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)

let token_to_string = function
  | Tat           -> "@"
  | Tbacktick     -> "`"
  | Tlp           -> "("
  | Trp           -> ")"
  | Tsc           -> ";"
  | Tcolon        -> ":"
  | Tcolcol       -> "::"
  | Tcomma        -> ","
  | Teq           -> "="
  | Tbareq        -> "|="
  | Tpluseq       -> "+="
  | Tstareq       -> "*="
  | Tslasheq      -> "/="
  | Tdoteq        -> ".="
  | Tminuseq      -> "-="
  | Tpercenteq    -> "%="
  | Txoreq        -> "^="
  | Tampeq        -> "&="
  | Tlshifteq     -> "<<="
  | Trshifteq     -> ">>="
  | Teqeq         -> "=="
  | Teqeqeq       -> "==="
  | Tdiff         -> "!="
  | Tdiff2        -> "!=="
  | Tbar          -> "|"
  | Tbarbar       -> "||"
  | Tampamp       -> "&&"
  | Tplus         -> "+"
  | Tminus        -> "-"
  | Tstar         -> "*"
  | Tstarstar     -> "**"
  | Tslash        -> "/"
  | Tbslash       -> "\\"
  | Txor          -> "^"
  | Tlcb          -> "{"
  | Trcb          -> "}"
  | Tlb           -> "["
  | Trb           -> "]"
  | Tdot          -> "."
  | Tlte          -> "<="
  | Tlt           -> "<"
  | Tgt           -> ">"
  | Tgte          -> ">="
  | Tltlt         -> "<<"
  | Tgtgt         -> ">>"
  | Tsarrow       -> "=>"
  | Tnsarrow      -> "?->"
  | Tarrow        -> "->"
  | Tlambda       -> "==>"
  | Tem           -> "!"
  | Tqm           -> "?"
  | Tamp          -> "&"
  | Ttild         -> "~"
  | Tincr         -> "++"
  | Tdecr         -> "--"
  | Tunderscore   -> "_"
  | Tellipsis     -> "..."
  | Tdollar       -> "$"
  | Tpercent      -> "%"
  | Tquote        -> "'"
  | Tdquote       -> "\""
  | Tclose_php    -> "?>"
  | Tlvar         -> "lvar"
  | Tint          -> "int"
  | Tfloat        -> "float"
  | Tword         -> "word"
  | Tphp          -> "php"
  | Thh           -> "hh"
  | Trequired     -> "required"
  | Teof          -> "eof"
  | Tyield        -> "yield"
  | Tawait        -> "await"
  | Timport       -> "import"
  | Teval         -> "eval"
  | Tprint        -> "print"
  | Tinstanceof   -> "instanceof"
  | Tnew          -> "new"
  | Tclone        -> "clone"
  | Telseif       -> "elseif"
  | Telse         -> "else"
  | Tendif        -> "endif"
  | Tcast         -> "cast"
  | Tref          -> "ref"
  | Theredoc      -> "heredoc"
  | Txhpname      -> "xhpname"
  | Terror        -> "error"
  | Tunsafe       -> "unsafe"
  | Tunsafeexpr   -> "unsafeexpr"
  | Tfallthrough  -> "fallthrough"
  | Tnewline      -> "newline"
  | Tany          -> "any"
  | Tspace        -> "space"
  | Topen_comment -> "open_comment"
  | Tclose_comment -> "close_comment"
  | Tline_comment  -> "line_comment"
  | Topen_xhp_comment -> "open_xhp_comment"
  | Tclose_xhp_comment -> "close_xhp_comment"

}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z''_']
let alphanumeric = digit | letter
let varname = letter alphanumeric*
let word_part = (letter alphanumeric*) | (['a'-'z'] (alphanumeric | '-')* alphanumeric)
let word = ('\\' | word_part)+ (* Namespaces *)
let xhpname = ('%')? varname ([':' '-'] varname)*
let otag = '<' ['a'-'z''A'-'Z'] (alphanumeric | ':' | '-')*
let ctag = '<' '/' (alphanumeric | ':' | '-')+ '>'
let lvar = '$' varname
let ws = [' ' '\t' '\r' '\x0c']
let hex = digit | ['a'-'f''A'-'F']
let hex_number = '0' 'x' hex+
let bin_number = '0' 'b' ['0'-'1']+
let decimal_number = '0' | ['1'-'9'] digit*
let octal_number = '0' ['0'-'7']+
let int = decimal_number | hex_number | bin_number | octal_number
let float =
  (digit* ('.' digit+) ((('e'|'E') ('+'?|'-') digit+))?) |
  (digit+ ('.' digit*) ((('e'|'E') ('+'?|'-') digit+))?) |
  (digit+ ('e'|'E') ('+'?|'-') digit+)
let unsafe = "//" ws* "UNSAFE" [^'\n']*
let unsafeexpr_start = "/*" ws* "UNSAFE_EXPR"
let fixme_start = "/*" ws* ("HH_FIXME" | "HH_IGNORE_ERROR")
let fallthrough = "//" ws* "FALLTHROUGH" [^'\n']*

rule token file = parse
  (* ignored *)
  | ws+                { token file lexbuf }
  | '\n'               { Lexing.new_line lexbuf; token file lexbuf }
  | unsafeexpr_start   { let buf = Buffer.create 256 in
                         let start = lexbuf.Lexing.lex_start_p in
                         ignore (comment buf file lexbuf);
                         (* unsafeexpr is technically made up of multiple
                          * tokens, but we want to treat it as a single token
                          * as far as start / end positions are concerned *)
                         lexbuf.Lexing.lex_start_p <- start;
                         Tunsafeexpr
                       }
  | fixme_start        { let fixme = fixme_state0 file lexbuf in
                         let tok = token file lexbuf in
                         (match fixme with
                           | Some err_nbr -> add_fixme err_nbr (Pos.make file lexbuf)
                           | None -> ());
                         tok
                       }
  | "/*"               { let buf = Buffer.create 256 in
                         comment_list := comment buf file lexbuf :: !comment_list;
                         token file lexbuf
                       }
  | "//"               { line_comment lexbuf; token file lexbuf }
  | "#"                { line_comment lexbuf; token file lexbuf }
  | '\"'               { Tdquote      }
  | '''                { Tquote       }
  | "<<<"              { Theredoc     }
  | int                { Tint         }
  | float              { Tfloat       }
  | '@'                { Tat          }
  | "?>"               { Tclose_php   }
  | word               { Tword        }
  | lvar               { Tlvar        }
  | '$'                { Tdollar      }
  | '`'                { Tbacktick    }
  | "<?php"            { Tphp         }
  | "<?hh"             { Thh          }
  | '('                { Tlp          }
  | ')'                { Trp          }
  | ';'                { Tsc          }
  | ':'                { Tcolon       }
  | "::"               { Tcolcol      }
  | ','                { Tcomma       }
  | '='                { Teq          }
  | "|="               { Tbareq       }
  | "+="               { Tpluseq      }
  | "*="               { Tstareq      }
  | "/="               { Tslasheq     }
  | ".="               { Tdoteq       }
  | "-="               { Tminuseq     }
  | "%="               { Tpercenteq   }
  | "^="               { Txoreq       }
  | "&="               { Tampeq       }
  | "<<="              { Tlshifteq    }
  | ">>="              { Trshifteq    }
  | "=="               { Teqeq        }
  | "==="              { Teqeqeq      }
  | "!="               { Tdiff        }
  | "!=="              { Tdiff2       }
  | '|'                { Tbar         }
  | "||"               { Tbarbar      }
  | "&&"               { Tampamp      }
  | '+'                { Tplus        }
  | '-'                { Tminus       }
  | '*'                { Tstar        }
  | "**"               { Tstarstar    }
  | '/'                { Tslash       }
  | '^'                { Txor         }
  | '%'                { Tpercent     }
  | '{'                { Tlcb         }
  | '}'                { Trcb         }
  | '['                { Tlb          }
  | ']'                { Trb          }
  | '.'                { Tdot         }
  | "<="               { Tlte         }
  | '<'                { Tlt          }
  | '>'                { Tgt          }
  | ">="               { Tgte         }
  | "<<"               { Tltlt        }
  | ">>"               { Tgtgt        }
  | "=>"               { Tsarrow      }
  | "?->"              { Tnsarrow     }
  | "->"               { Tarrow       }
  | "==>"              { Tlambda      }
  | '!'                { Tem          }
  | '?'                { Tqm          }
  | '&'                { Tamp         }
  | '~'                { Ttild        }
  | "++"               { Tincr        }
  | "--"               { Tdecr        }
  | "_"                { Tunderscore  }
  | "@required"        { Trequired    }
  | "..."              { Tellipsis    }
  | unsafe             { Tunsafe      }
  | fallthrough        { Tfallthrough }
  | eof                { Teof         }
  | _                  { Terror       }

and xhpname file = parse
  | eof                { Terror      }
  | '\n'               { Lexing.new_line lexbuf; xhpname file lexbuf }
  | ws+                { xhpname file lexbuf }
  | "/*"               { ignore (comment (Buffer.create 256) file lexbuf);
                         xhpname file lexbuf
                       }
  | "//"               { line_comment lexbuf; xhpname file lexbuf }
  | "#"                { line_comment lexbuf; xhpname file lexbuf }
  | word               { Txhpname    }
  | xhpname            { Txhpname    }
  | _                  { Terror      }

and xhptoken file = parse
  | eof                { Teof        }
  | '\n'               { Lexing.new_line lexbuf; xhptoken file lexbuf }
  | '<'                { Tlt         }
  | '>'                { Tgt         }
  | '{'                { Tlcb        }
  | '}'                { Trcb        }
  | '/'                { Tslash      }
  | '\"'               { Tdquote     }
  | word               { Tword       }
  | "<!--"             { xhp_comment file lexbuf; xhptoken file lexbuf }
  | _                  { xhptoken file lexbuf }

and xhpattr file = parse
  | eof                { Teof        }
  | ws+                { xhpattr file lexbuf }
  | '\n'               { Lexing.new_line lexbuf; xhpattr file lexbuf }
  | fixme_start        { let fixme = fixme_state0 file lexbuf in
                         let tok = xhpattr file lexbuf in
                         (match fixme with
                           | Some err_nbr -> add_fixme err_nbr (Pos.make file lexbuf)
                           | None -> ());
                         tok
                       }
  | "/*"               { ignore (comment (Buffer.create 256) file lexbuf);
                         xhpattr file lexbuf
                       }
  | "//"               { line_comment lexbuf; xhpattr file lexbuf }
  | '\n'               { Lexing.new_line lexbuf; xhpattr file lexbuf }
  | '<'                { Tlt         }
  | '>'                { Tgt         }
  | '{'                { Tlcb        }
  | '}'                { Trcb        }
  | '/'                { Tslash      }
  | '\"'               { Tdquote     }
  | word               { Tword       }
  | _                  { Terror      }

and heredoc_token = parse
  | eof                { Teof        }
  | '\n'               { Lexing.new_line lexbuf; Tnewline }
  | word               { Tword       }
  | ';'                { Tsc         }
  | _                  { Tany        }

and comment buf file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unterminated_comment pos;
                         pos, Buffer.contents buf
                       }
  | '\n'               { Lexing.new_line lexbuf;
                         Buffer.add_char buf '\n';
                         comment buf file lexbuf
                       }
  | "*/"               { Pos.make file lexbuf, Buffer.contents buf }
  | _                  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                         comment buf file lexbuf
                       }

(* HH_FIXME... *)
and fixme_state0 file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unterminated_comment pos;
                         None
                       }
  | ws+                { fixme_state0 file lexbuf
                       }
  | '\n'               { Lexing.new_line lexbuf;                        
                         fixme_state0 file lexbuf
                       }
  | '['                { fixme_state1 file lexbuf }
  | _                  { Errors.fixme_format (Pos.make file lexbuf);
                         ignore (comment (Buffer.create 256) file lexbuf);
                         None
                       }

(* HH_FIXME[... *)
and fixme_state1 file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unterminated_comment pos;
                         None
                       }
  | ws+                { fixme_state1 file lexbuf }
  | '\n'               { Lexing.new_line lexbuf;
                         fixme_state1 file lexbuf
                       }
  | int                { let err_nbr = Lexing.lexeme lexbuf in
                         let err_nbr = int_of_string err_nbr in
                         fixme_state2 err_nbr file lexbuf }
  | _                  { Errors.fixme_format (Pos.make file lexbuf);
                         ignore (comment (Buffer.create 256) file lexbuf);
                         None
                       }

(* HH_FIXME[NUMBER... *)
and fixme_state2 err_nbr file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unterminated_comment pos;
                         None
                       }
  | "*/" ws* '\n'      { Lexing.new_line lexbuf;
                         Some err_nbr
                       }
  | "*/"               { Some err_nbr }
  | '\n'               { Lexing.new_line lexbuf;
                         fixme_state2 err_nbr file lexbuf
                       }
  | _                  { fixme_state2 err_nbr file lexbuf }

and line_comment = parse
  | eof                { () }
  | '\n'               { Lexing.new_line lexbuf }
  | _                  { line_comment lexbuf }

and xhp_comment file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unterminated_xhp_comment pos;
                         ()
                       }
  | '\n'               { Lexing.new_line lexbuf; xhp_comment file lexbuf }
  | "-->"              { () }
  | _                  { xhp_comment file lexbuf }

and gt_or_comma file = parse
  | eof                { Terror }
  | ws+                { gt_or_comma file lexbuf }
  | '\n'               { Lexing.new_line lexbuf; gt_or_comma file lexbuf }
  | "/*"               { ignore (comment (Buffer.create 256) file lexbuf);
                         gt_or_comma file lexbuf
                       }
  | "//"               { line_comment lexbuf; gt_or_comma file lexbuf }
  | '\n'               { Lexing.new_line lexbuf; gt_or_comma file lexbuf }
  | '>'                { Tgt  }
  | ','                { Tcomma  }
  | _                  { Terror }

and no_space_id = parse
  | eof                { Terror }
  | word               { Tword  }
  | _                  { Terror }

and string file = parse
  | eof                { Teof }
  | '\n'               { Lexing.new_line lexbuf; string file lexbuf }
  | '\\'               { string_backslash file lexbuf; string file lexbuf }
  | '''                { Tquote }
  | _                  { string file lexbuf }

and string_backslash file = parse
  | eof                { let pos = Pos.make file lexbuf in
                         Errors.unexpected_eof pos;
                         ()
                       }
  | '\n'               { Lexing.new_line lexbuf }
  | _                  { () }

and string2 file = parse
  | eof                { Teof }
  | '\n'               { Lexing.new_line lexbuf; string2 file lexbuf }
  | '\\'               { string_backslash file lexbuf; string2 file lexbuf }
  | '\"'               { Tdquote }
  | '{'                { Tlcb }
  | '}'                { Trcb }
  | '['                { Tlb }
  | ']'                { Trb }
  | "->"               { Tarrow }
  | '$'                { Tdollar }
  | '''                { Tquote }
  | int                { Tint }
  | word_part          { Tword  }
  | lvar               { Tlvar }
  | _                  { Tany }

and header file = parse
  | eof                         { `error }
  | ws+                         { header file lexbuf }
  | '\n'                        { Lexing.new_line lexbuf; header file lexbuf }
  | "//"                        { line_comment lexbuf; header file lexbuf }
  | "/*"                        { ignore (comment (Buffer.create 256) file lexbuf);
                                  header file lexbuf
                                }
  | "#"                         { line_comment lexbuf; header file lexbuf }
  | "<?hh"                      { `default_mode }
  | "<?hh" ws* "//"             { `explicit_mode }
  | "<?php" ws* "//" ws* "decl" { `php_decl_mode }
  | "<?php"                     { `php_mode }
  | _                           { `error }

and next_newline_or_close_cb = parse
  | eof                { () }
  | '\n'               { Lexing.new_line lexbuf }
  | '}'                { back lexbuf }
  | _                  { next_newline_or_close_cb lexbuf }

and look_for_open_cb = parse
  | eof                { () }
  | '\n'               { Lexing.new_line lexbuf; look_for_open_cb lexbuf }
  | '{'                { () }
  | _                  { look_for_open_cb lexbuf }

(* Normally you can just use "token" and get back Tlvar, but specifically for
 * member variable accesses, the part to the right of the "->" isn't a word
 * (cannot contain '-' for example) but doesn't start with '$' so isn't an lvar
 * either. *)
and varname = parse
  | varname            { Tword  }
  | _                  { Terror }

(****************************************************************************)
(* hh_format tokenizers. *)
(****************************************************************************)

and format_comment = parse
  | [' '  '\t']        { Tspace         }
  | ws* '\n'           { Tnewline       } (* eat up trailing spaces *)
  | eof                { Teof           }
  | "*/"               { Tclose_comment }
  | _                  { Tany           }

and format_token = parse
  | [' '  '\t']        { Tspace        }
  | ws* '\n'           { Tnewline      } (* eat up trailing spaces *)
  | "/*"               { Topen_comment }
  | "//"               { Tline_comment }
  | "#"                { Tline_comment }
  | '\"'               { Tdquote       }
  | '''                { Tquote        }
  | "<<<"              { Theredoc      }
  | int                { Tint          }
  | float              { Tfloat        }
  | '@'                { Tat           }
  | "?>"               { Tclose_php    }
  | word_part          { Tword         }
  | lvar               { Tlvar         }
  | '$'                { Tdollar       }
  | '`'                { Tbacktick     }
  | "<?php"            { Tphp          }
  | "<?hh"             { Thh           }
  | '('                { Tlp           }
  | ')'                { Trp           }
  | ';'                { Tsc           }
  | ':'                { Tcolon        }
  | "::"               { Tcolcol       }
  | ','                { Tcomma        }
  | '='                { Teq           }
  | "|="               { Tbareq        }
  | "+="               { Tpluseq       }
  | "*="               { Tstareq       }
  | "/="               { Tslasheq      }
  | ".="               { Tdoteq        }
  | "-="               { Tminuseq      }
  | "%="               { Tpercenteq    }
  | "^="               { Txoreq        }
  | "&="               { Tampeq        }
  | "<<="              { Tlshifteq     }
  | ">>="              { Trshifteq     }
  | "=="               { Teqeq         }
  | "==="              { Teqeqeq       }
  | "!="               { Tdiff         }
  | "!=="              { Tdiff2        }
  | '|'                { Tbar          }
  | "||"               { Tbarbar       }
  | "&&"               { Tampamp       }
  | '+'                { Tplus         }
  | '-'                { Tminus        }
  | '*'                { Tstar         }
  | "**"               { Tstarstar     }
  | '/'                { Tslash        }
  | '\\'               { Tbslash       }
  | '^'                { Txor          }
  | '%'                { Tpercent      }
  | '{'                { Tlcb          }
  | '}'                { Trcb          }
  | '['                { Tlb           }
  | ']'                { Trb           }
  | '.'                { Tdot          }
  | "<="               { Tlte          }
  | '<'                { Tlt           }
  | '>'                { Tgt           }
  | ">="               { Tgte          }
  | "<<"               { Tltlt         }
  | "=>"               { Tsarrow       }
  | "?->"              { Tnsarrow      }
  | "->"               { Tarrow        }
  | "==>"              { Tlambda       }
  | '!'                { Tem           }
  | '?'                { Tqm           }
  | '&'                { Tamp          }
  | '~'                { Ttild         }
  | "++"               { Tincr         }
  | "--"               { Tdecr         }
  | "_"                { Tunderscore   }
  | "..."              { Tellipsis     }
  | eof                { Teof          }
  | _                  { Terror        }

and format_xhptoken = parse
  | eof                { Teof        }
  | ws* '\n'           { Tnewline    } (* eat up trailing spaces *)
  | ' '                { Tspace      }
  | '<'                { Tlt         }
  | '>'                { Tgt         }
  | '{'                { Tlcb        }
  | '}'                { Trcb        }
  | '/'                { Tslash      }
  | '\"'               { Tdquote     }
  | "/*"               { Topen_comment      }
  | "*/"               { Tclose_comment     }
  | "//"               { Tline_comment      }
  | word               { Tword              }
  | "<!--"             { Topen_xhp_comment  }
  | "-->"              { Tclose_xhp_comment }
  | _                  { Terror             }
