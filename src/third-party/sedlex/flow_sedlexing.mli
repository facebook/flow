
(** This is a module provides the minimal Sedlexing support
  It is mostly a subset of Sedlexing with two functions for performance reasons:
  - Utf8.lexeme_to_buffer
  - Utf8.lexeme_to_buffer2
*)
exception InvalidCodepoint of int
exception MalFormed
type apos = int
type lexbuf
val lexbuf_clone : lexbuf -> lexbuf

val from_int_array : int array -> lexbuf
val new_line : lexbuf -> unit
val next : lexbuf -> Uchar.t option

(**/**)
val __private__next_int : lexbuf -> int
(**/**)

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
module Utf8 : sig
  val from_string : string -> lexbuf
  val sub_lexeme : lexbuf -> int -> int -> string
  val lexeme : lexbuf -> string
  (** This API avoids another allocation *)
  val lexeme_to_buffer : lexbuf -> Buffer.t -> unit
  val lexeme_to_buffer2 : lexbuf -> Buffer.t -> Buffer.t -> unit
end

val string_of_utf8 : int array -> string

(** Two APIs used when we want to do customize lexing
    instead of using the regex based engine
*)
val current_code_point : lexbuf -> int
val backoff : lexbuf -> int -> unit
val set_lexeme_start : lexbuf -> int -> unit
