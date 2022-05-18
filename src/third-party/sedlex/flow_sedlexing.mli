
(** This is a module provides the minimal Sedlexing suppport
  It is mostly a subset of Sedlexing with two functions for performance reasons:
  - Utf8.lexeme_to_buffer
  - Utf8.lexeme_to_buffer2
*)
exception InvalidCodepoint of int
exception MalFormed
type apos = int
type lexbuf = {
  buf: int array;
  (* Number of meaningful char in buffer *)
  len: int;
  (* pos is the index in the buffer *)
  mutable pos: int;
  (* bol is the index in the input stream but not buffer *)
  mutable curr_bol: int;
  (* start from 1, if it is 0, we would not track postion info for you *)
  mutable curr_line: int;
  (* First char we need to keep visible *)
  mutable start_pos: int;
  mutable start_bol: int;
  mutable start_line: int;
  mutable marked_pos: int;
  mutable marked_bol: int;
  mutable marked_line: int;
  mutable marked_val: int;
}
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
val backoff : lexbuf -> int -> unit
val rawbuffer : lexbuf -> int array