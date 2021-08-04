(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)
external ( .!()<- ) : int array -> int -> int -> unit = "%array_unsafe_set"
external ( .!() ) : int array -> int -> int = "%array_unsafe_get"
external ( .![] ) : string -> int -> char = "%string_unsafe_get"
external ( .![]<- ) : bytes -> int -> char -> unit = "%bytes_unsafe_set"

exception InvalidCodepoint of int

exception MalFormed

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  mutable buf: int array;
  (* Number of meaningful char in buffer *)
  mutable len: int;
  (* Position of the first char in buffer in the input stream *)
  mutable offset: apos;
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

let lexbuf_clone (x : lexbuf) : lexbuf =
  {
    buf = x.buf;
    len = x.len;
    offset = x.offset;
    pos = x.pos;
    curr_bol = x.curr_bol;
    curr_line = x.curr_line;
    start_pos = x.start_pos;
    start_bol = x.start_bol;
    start_line = x.start_line;
    marked_pos = x.marked_pos;
    marked_bol = x.marked_bol;
    marked_line = x.marked_line;
    marked_val = x.marked_val;
  }

let empty_lexbuf =
  {
    buf = [||];
    len = 0;
    offset = 0;
    pos = 0;
    curr_bol = 0;
    curr_line = 0;
    start_pos = 0;
    start_bol = 0;
    start_line = 0;
    marked_pos = 0;
    marked_bol = 0;
    marked_line = 0;
    marked_val = 0;
  }

let from_int_array a =
  let len = Array.length a in
  { empty_lexbuf with buf = a; len }

let from_int_sub_array a len =
  { empty_lexbuf with buf = a; len }

let new_line lexbuf =
  if lexbuf.curr_line != 0 then lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset

let next lexbuf : Stdlib.Uchar.t option =
  if lexbuf.pos = lexbuf.len then
    None
  else
    let ret = lexbuf.buf.!(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    if ret = 10 then new_line lexbuf;
    Some (Stdlib.Uchar.unsafe_of_int ret)

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  lexbuf.start_bol <- lexbuf.curr_bol;
  lexbuf.start_line <- lexbuf.curr_line;
  mark lexbuf (-1)

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.curr_bol <- lexbuf.marked_bol;
  lexbuf.curr_line <- lexbuf.marked_line;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start_pos;
  lexbuf.curr_bol <- lexbuf.start_bol;
  lexbuf.curr_line <- lexbuf.start_line

let lexeme_start lexbuf = lexbuf.start_pos + lexbuf.offset

let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset

let loc lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos

let sub_lexeme lexbuf pos len = Array.sub lexbuf.buf (lexbuf.start_pos + pos) len

let lexeme lexbuf = Array.sub lexbuf.buf lexbuf.start_pos (lexbuf.pos - lexbuf.start_pos)


(* Decode UTF-8 encoded [s] into codepoints in [a], returning the length of the
 * decoded string.
 *
 * To call this function safely:
 * - ensure that [slen] is not greater than the length of [s]
 * - ensure that [a] has enough capacity to hold the decoded value
 *)
let unsafe_utf8_of_string (s : string) slen (a : int array) : int =
  let spos = ref 0 in
  let apos = ref 0 in
  while !spos < slen do
    let spos_code = s.![!spos] in
    (match spos_code with
    | '\000' .. '\127' as c ->
      a.!(!apos) <- Char.code c;
      incr spos
    | '\192' .. '\223' as c ->
      let n1 = Char.code c in
      let n2 = Char.code s.![!spos + 1] in
      if n2 lsr 6 != 0b10 then raise MalFormed;
      a.!(!apos) <- ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f);
      spos := !spos + 2
    | '\224' .. '\239' as c ->
      let n1 = Char.code c in
      let n2 = Char.code s.![!spos + 1] in
      let n3 = Char.code s.![!spos + 2] in
      let p = ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f) in
      if (n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10) || (p >= 0xd800 && p <= 0xdf00) then raise MalFormed;
      a.!(!apos) <- p;
      spos := !spos + 3
    | '\240' .. '\247' as c ->
      let n1 = Char.code c in
      let n2 = Char.code s.![!spos + 1] in
      let n3 = Char.code s.![!spos + 2] in
      let n4 = Char.code s.![!spos + 3] in
      if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then raise MalFormed;
      a.!(!apos) <-
        ((n1 land 0x07) lsl 18)
        lor ((n2 land 0x3f) lsl 12)
        lor ((n3 land 0x3f) lsl 6)
        lor (n4 land 0x3f);
      spos := !spos + 4
    | _ -> raise MalFormed);
    incr apos
  done;
  !apos

(* Encode the decoded codepoints in [a] as UTF-8 into [b], returning the length
 * of the encoded string.
 *
 * To call this function safely:
 * - ensure that [offset + len] is not greater than the length of [a]
 * - ensure that [b] has sufficient capacity to hold the encoded value
 *)
let unsafe_string_of_utf8 (a : int array) ~(offset : int) ~(len : int) (b : bytes) : int =
  let apos = ref offset in
  let len = ref len in
  let i = ref 0 in
  while !len > 0 do
    let u = a.!(!apos) in
    if u < 0 then
      raise MalFormed
    else if u <= 0x007F then begin
      b.![!i] <- Char.unsafe_chr u;
      incr i
    end else if u <= 0x07FF then (
      b.![!i] <- Char.unsafe_chr (0xC0 lor (u lsr 6));
      b.![!i + 1] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
      i := !i + 2
    ) else if u <= 0xFFFF then (
      b.![!i] <- Char.unsafe_chr (0xE0 lor (u lsr 12));
      b.![!i + 1] <- Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F));
      b.![!i + 2] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
      i := !i + 3
    ) else if u <= 0x10FFFF then (
      b.![!i] <- Char.unsafe_chr (0xF0 lor (u lsr 18));
      b.![!i + 1] <- Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F));
      b.![!i + 2] <- Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F));
      b.![!i + 3] <- Char.unsafe_chr (0x80 lor (u land 0x3F));
      i := !i + 4
    ) else
      raise MalFormed;
    incr apos;
    decr len
  done;
  !i

module Utf8 = struct
  let from_string s =
    let slen = String.length s in
    let a = Array.make slen 0 in
    let len = unsafe_utf8_of_string s slen a in
    from_int_sub_array a len

  let sub_lexeme lexbuf pos len : string =
    let offset = lexbuf.start_pos + pos in
    let b = Bytes.create (len * 4) in
    let buf = lexbuf.buf in
    (* Assertion needed, since we make use of unsafe API below *)
    assert (offset + len <= Array.length buf);
    let i = unsafe_string_of_utf8 buf ~offset ~len b in
    Bytes.sub_string b 0 i

  let lexeme lexbuf : string =
    let offset = lexbuf.start_pos in
    let len = lexbuf.pos - offset in
    let b = Bytes.create (len * 4) in
    let buf = lexbuf.buf in
    let i = unsafe_string_of_utf8 buf ~offset ~len b in
    Bytes.sub_string b 0 i

  let lexeme_to_buffer lexbuf buffer : unit =
    let offset = lexbuf.start_pos in
    let len = lexbuf.pos - offset in
    let b = Bytes.create (len * 4) in
    let buf = lexbuf.buf in
    let i = unsafe_string_of_utf8 buf ~offset ~len b in
    Buffer.add_subbytes buffer b 0 i

  let lexeme_to_buffer2 lexbuf buf1 buf2 : unit =
    let offset = lexbuf.start_pos in
    let len = lexbuf.pos - offset in
    let b = Bytes.create (len * 4) in
    let buf = lexbuf.buf in
    let i = unsafe_string_of_utf8 buf ~offset ~len b in
    Buffer.add_subbytes buf1 b 0 i;
    Buffer.add_subbytes buf2 b 0 i
end
