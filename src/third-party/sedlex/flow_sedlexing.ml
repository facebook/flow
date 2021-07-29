(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed


let gen_of_channel chan =
  let f () =
    try Some (input_char chan)
    with End_of_file -> None
  in
  f

let (>>=) o f = match o with
  | Some x -> f x
  | None -> None


(* For legacy purposes. *)
let gen_of_stream stream =
  let f () =
    try Some (Stream.next stream)
    with Stream.Failure -> None
  in f

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill: (Uchar.t array -> int -> int -> int);
  mutable buf: Uchar.t array;
  mutable len: int;    (* Number of meaningful char in buffer *)
  mutable offset: apos; (* Position of the first char in buffer
                            in the input stream *)
  mutable pos: int; (* pos is the index in the buffer *)
  mutable curr_bol: int; (* bol is the index in the input stream but not buffer *)
  mutable curr_line: int; (* start from 1, if it is 0, we would not track postion info for you *)
  mutable start_pos: int; (* First char we need to keep visible *)
  mutable start_bol: int;
  mutable start_line: int;

  mutable marked_pos: int;
  mutable marked_bol: int;
  mutable marked_line: int;
  mutable marked_val: int;

  mutable filename: string;

  mutable finished: bool;
}

let chunk_size = 512

let empty_lexbuf = {
  refill = (fun _ _ _ -> assert false);
  buf = [| |];
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
  filename = "";
  finished = false;
}

let create f = {
  empty_lexbuf with
    refill = f;
    buf = Array.make chunk_size (Uchar.of_int 0);
    curr_line = 1;
}

let set_position lexbuf position =
  lexbuf.offset <- position.Lexing.pos_cnum - lexbuf.pos;
  lexbuf.curr_bol <- position.Lexing.pos_bol;
  lexbuf.curr_line <- position.Lexing.pos_lnum

let set_filename lexbuf fname =
  lexbuf.filename <- fname

let fill_buf_from_gen f gen buf pos len =
  let rec aux i =
    if i >= len then len
    else match gen () with
      | Some c -> buf.(pos + i) <- f c ; aux (i+1)
      | None -> i
  in
  aux 0

let from_gen s =
  create (fill_buf_from_gen (fun id -> id) s)

let from_stream s = from_gen @@ gen_of_stream s

let from_int_array a =
  let len = Array.length a in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> Uchar.of_int a.(i));
      len = len;
      finished = true;
  }

let from_uchar_array a =
  let len = Array.length a in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> a.(i));
      len = len;
      finished = true;
  }


let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf
  then begin
    let s = lexbuf.start_pos in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen (Uchar.of_int 0) in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.start_pos <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if n = 0
  then lexbuf.finished <- true
  else lexbuf.len <- lexbuf.len + n

let new_line lexbuf =
  if lexbuf.curr_line != 0 then
  lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset

let next lexbuf =
  if (not lexbuf.finished) && (lexbuf.pos = lexbuf.len) then refill lexbuf;
  if lexbuf.finished && (lexbuf.pos = lexbuf.len) then None
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    if ret = (Uchar.of_int 10) then new_line lexbuf;
    Some ret
  end

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

let sub_lexeme lexbuf pos len =
  Array.sub lexbuf.buf (lexbuf.start_pos + pos) len

let lexeme lexbuf =
  Array.sub lexbuf.buf (lexbuf.start_pos) (lexbuf.pos - lexbuf.start_pos)

let lexeme_char lexbuf pos =
  lexbuf.buf.(lexbuf.start_pos + pos)

let lexing_positions lexbuf =
  let start_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_pos + lexbuf.offset;
    pos_bol = lexbuf.start_bol;
  } and curr_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.pos + lexbuf.offset;
    pos_bol = lexbuf.curr_bol;
  } in
  (start_p, curr_p)

let with_tokenizer lexer' lexbuf =
  let lexer () =
    let token = lexer' lexbuf in
    let (start_p, curr_p) = lexing_positions lexbuf in
    (token, start_p, curr_p)
  in lexer

module Latin1 = struct
  let from_gen s =
    create (fill_buf_from_gen Uchar.of_char s)

  let from_stream s = from_gen @@ gen_of_stream s

  let from_string s =
    let len = String.length s in
    {
     empty_lexbuf with
     buf = Array.init len (fun i -> Uchar.of_char s.[i]);
     len = len;
     finished = true;
    }

  let from_channel ic =
    from_gen (gen_of_channel ic)

  let to_latin1 c =
    if Uchar.is_char c
    then Uchar.to_char c
    else raise (InvalidCodepoint (Uchar.to_int c))

  let lexeme_char lexbuf pos =
    to_latin1 (lexeme_char lexbuf pos)

  let sub_lexeme lexbuf pos len =
    let s = Bytes.create len in
    for i = 0 to len - 1 do Bytes.set s i (to_latin1 lexbuf.buf.(lexbuf.start_pos + pos + i)) done;
    Bytes.to_string s

  let lexeme lexbuf =
    sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end


module Utf8 = struct
  module Helper = struct
    (* http://www.faqs.org/rfcs/rfc3629.html *)

    let width = Array.make 256 (-1)
    let () =
      for i = 0 to 127 do width.(i) <- 1 done;
      for i = 192 to 223 do width.(i) <- 2 done;
      for i = 224 to 239 do width.(i) <- 3 done;
      for i = 240 to 247 do width.(i) <- 4 done

    let next s i =
      match s.[i] with
      | '\000'..'\127' as c ->
          Char.code c
      | '\192'..'\223' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
          if (n2 lsr 6 != 0b10) then raise MalFormed;
          ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
      | '\224'..'\239' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise MalFormed;
	  let p =
            ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
	  in
	  if (p >= 0xd800) && (p <= 0xdf00) then raise MalFormed;
	  p
      | '\240'..'\247' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
	  let n4 = Char.code s.[i+3] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
	  then raise MalFormed;
          ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
          ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)
      | _ -> raise MalFormed


    let from_gen s =
      Gen.next s >>= function
      | '\000'..'\127' as c ->
          Some (Uchar.of_char c)
      | '\192'..'\223' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
          if (n2 lsr 6 != 0b10) then raise MalFormed;
          Some (Uchar.of_int (((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)))
      | '\224'..'\239' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
	  Gen.next s >>= fun c3 ->
	  let n3 = Char.code c3 in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise MalFormed;
          Some (Uchar.of_int (((n1 land 0x0f) lsl 12)
                              lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)))
      | '\240'..'\247' as c ->
	  let n1 = Char.code c in
	  Gen.next s >>= fun c2 ->
	  let n2 = Char.code c2 in
	  Gen.next s >>= fun c3 ->
	  let n3 = Char.code c3 in
	  Gen.next s >>= fun c4 ->
	  let n4 = Char.code c4 in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
	  then raise MalFormed;
          Some (Uchar.of_int (((n1 land 0x07) lsl 18)
                              lor ((n2 land 0x3f) lsl 12)
                              lor ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)))
      | _ -> raise MalFormed



    let compute_len s pos bytes =
      let rec aux n i =
        if i >= pos + bytes then if i = pos + bytes then n else raise MalFormed
        else
          let w = width.(Char.code s.[i]) in
          if w > 0 then aux (succ n) (i + w)
          else raise MalFormed
      in
      aux 0 pos

    let rec blit_to_int s spos a apos n =
      if n > 0 then begin
        a.(apos) <- next s spos;
        blit_to_int s (spos + width.(Char.code s.[spos])) a (succ apos) (pred n)
      end

    let to_int_array s pos bytes =
      let n = compute_len s pos bytes in
      let a = Array.make n 0 in
      blit_to_int s pos a 0 n;
      a

(**************************)

    let store b p =
      if p <= 0x7f then
        Buffer.add_char b (Char.chr p)
      else if p <= 0x7ff then (
        Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0xffff then (
        if (p >= 0xd800 && p < 0xe000) then raise MalFormed;
        Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0x10ffff then (
        Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6)  land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else raise MalFormed

    let from_uchar_array a apos len =
      let b = Buffer.create (len * 4) in
      let rec aux apos len =
        if len > 0
        then (store b (Uchar.to_int a.(apos)); aux (succ apos) (pred len))
        else Buffer.contents b in
      aux apos len

    let gen_from_char_gen s = (fun () -> from_gen s)
  end

  let from_channel ic =
    from_gen (Helper.gen_from_char_gen (gen_of_channel ic))

  let from_gen s =
    create (fill_buf_from_gen (fun id -> id)
        (Helper.gen_from_char_gen s))

  let from_stream s = from_gen @@ gen_of_stream s

  let from_string s =
    from_int_array (Helper.to_int_array s 0 (String.length s))

  let sub_lexeme lexbuf pos len =
    Helper.from_uchar_array lexbuf.buf (lexbuf.start_pos + pos) len

  let lexeme lexbuf =
    sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end


module Utf16 = struct
  type byte_order = Little_endian | Big_endian
  module Helper = struct
    (* http://www.ietf.org/rfc/rfc2781.txt *)

    let number_of_char_pair bo c1 c2 = match bo with
    | Little_endian -> ((Char.code c2) lsl 8) + (Char.code c1)
    | Big_endian -> ((Char.code c1) lsl 8) + (Char.code c2)

    let char_pair_of_number bo num = match bo with
    | Little_endian ->
        (Char.chr (num land 0xFF), Char.chr ((num lsr 8) land 0xFF ))
    | Big_endian ->
        (Char.chr ((num lsr 8) land 0xFF), Char.chr (num land 0xFF))

    let next_in_gen bo s =
      Gen.next s >>= fun c1 ->
      Gen.next s >>= fun c2 ->
      Some (number_of_char_pair bo c1 c2)

    let from_gen bo s w1 =
      if w1 = 0xfffe then raise (InvalidCodepoint w1);
      if w1 < 0xd800 || 0xdfff < w1 then Some (Uchar.of_int w1)
      else if w1 <= 0xdbff
      then
        next_in_gen bo s >>= fun w2 ->
        if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
        let upper10 = (w1 land 0x3ff) lsl 10
        and lower10 = w2 land 0x3ff in
        Some (Uchar.of_int (0x10000 + upper10 + lower10))
      else raise MalFormed

    let gen_from_char_gen opt_bo s =
      let bo = ref opt_bo in
      fun () ->
        Gen.next s >>= fun c1 ->
        Gen.next s >>= fun c2 ->
        let o = match !bo with
          | Some o -> o
          | None ->
              let o = match (Char.code c1, Char.code c2) with
                | (0xff,0xfe) -> Little_endian
                | _ -> Big_endian in
              bo := Some o;
              o in
        from_gen o s (number_of_char_pair o c1 c2)


    let compute_len opt_bo str pos bytes =
      let s = gen_from_char_gen opt_bo
          (Gen.init ~limit:(bytes - pos) (fun i -> (str.[i + pos])))
      in
      let l = ref 0 in
      Gen.iter (fun _ -> incr l) s ;
      !l

    let blit_to_int opt_bo s spos a apos bytes =
      let s = gen_from_char_gen opt_bo
          (Gen.init ~limit:(bytes - spos) (fun i -> (s.[i + spos]))) in
      let p = ref apos in
      Gen.iter (fun x -> a.(!p) <- x ; incr p) s


    let to_uchar_array opt_bo s pos bytes =
      let len = compute_len opt_bo s pos bytes in
      let a = Array.make len (Uchar.of_int 0) in
      blit_to_int opt_bo s pos a 0 bytes ;
       a

    let store bo buf code =
      if code < 0x10000
      then (
        let (c1,c2) = char_pair_of_number bo code in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2
       ) else (
        let u' = code - 0x10000  in
        let w1 = 0xd800 + (u' lsr 10)
        and w2 = 0xdc00 + (u' land 0x3ff) in
        let (c1,c2) = char_pair_of_number bo w1
        and (c3,c4) = char_pair_of_number bo w2 in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2;
        Buffer.add_char buf c3;
        Buffer.add_char buf c4
       )

    let from_uchar_array bo a apos len bom =
      let b = Buffer.create (len * 4 + 2) in (* +2 for the BOM *)
      if bom then store bo b 0xfeff ; (* first, store the BOM *)
      let rec aux apos len =
        if len > 0
        then (store bo b (Uchar.to_int a.(apos)); aux (succ apos) (pred len))
        else Buffer.contents b  in
      aux apos len
end


  let from_gen s opt_bo =
    from_gen (Helper.gen_from_char_gen opt_bo s)

  let from_stream s = from_gen @@ gen_of_stream s

  let from_channel ic opt_bo =
    from_gen (gen_of_channel ic) opt_bo

  let from_string s opt_bo =
    let a = Helper.to_uchar_array opt_bo s 0 (String.length s) in
    from_uchar_array a

  let sub_lexeme lb pos len bo bom  =
    Helper.from_uchar_array bo lb.buf (lb.start_pos + pos) len bom

  let lexeme lb bo bom =
    sub_lexeme lb 0 (lb.pos - lb.start_pos) bo bom
end
