
exception InvalidCodepoint of int
exception MalFormed
type apos = int
type lexbuf 
val lexbuf_clone : lexbuf -> lexbuf

val from_int_array : int array -> lexbuf
val from_int_code_point : int -> lexbuf
val new_line : lexbuf -> unit
val next : lexbuf -> Uchar.t option
val mark : lexbuf -> int -> unit
val start : lexbuf -> unit
val backtrack : lexbuf -> int
val rollback : lexbuf -> unit
val lexeme_start : lexbuf -> int
val lexeme_end : lexbuf -> int
val loc : lexbuf -> int * int
val lexeme_length : lexbuf -> int
val sub_lexeme : lexbuf -> int -> int -> int array
val lexeme : lexbuf -> int array
module Utf8 :
  sig
    val from_string : string -> lexbuf
    val sub_lexeme : lexbuf -> int -> int -> string
    val lexeme : lexbuf -> string
  end