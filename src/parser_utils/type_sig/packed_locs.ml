(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines a compact encoding for sorted locations with the same
 * source file.
 *
 * As a first optimization, we don't store the source file at all. Instead, we
 * assume that the source information is stored elsewhere and can be provided
 * when unpacking.
 *
 * Line and column numbers use a variable-length code, similar to unsigned
 * LEB128. Numbers less than 0xFF can be stored in a single byte, e.g.
 *
 * Positions are stored relative to the previous location's start position. This
 * helps keep the line/column numbers small, making them more amenable to the
 * variable-length code, hopefully each fitting into a single byte.
 *
 * Finally, we distinguish between some common cases to avoid storing all four
 * of start line, start column, end line, and end column. The first byte of each
 * location is a tag:
 *
 * 0x00 - 0x3F + 1 int: single line, start_rline = 0, start_rcolumn < 0x40
 *  { .tag = start_rcolumn;
 *    .0 = length }
 *
 * 0x40 - 0x7E + 2 ints: single line, start_rline < 0x3F
 *  { .tag = start_rline + 0x40;
 *    .0 = if start_rline = 0 then start_rcolumn - 0x40 else start_column;
 *    .1 = length }
 *
 * 0x7F + 3 ints: single line, rline >= 0x3F
 *  { .tag = 0x7F;
 *    .0 = start_rline - 0x3F;
 *    .1 = start_column;
 *    .2 = length }
 *
 * 0x80 - 0xBF + 2 ints: multi line, rline = 0, rcolumn < 0x40
 *  { .tag = start_rcolumn + 0x80;
 *    .0 = end_rline;
 *    .1 = end_column }
 *
 * 0xC0 - 0xFE + 3 ints: multi line, rline < 0x3F
 *  { .tag = start_rline + 0xC0;
 *    .0 = if start_rline = 0 then start_rcolumn - 0x40 else start_column;
 *    .1 = end_rline;
 *    .2 = end_column }
 *
 * 0xFF + 4 ints; multi line, rline >= 0x3F
 *  { .tag = 0xFF;
 *    .0 = start_rline - 0x3F;
 *    .1 = start_column;
 *    .2 = end_rline;
 *    .3 = end_column }
 *)

(* Buffer.add_uint8 was added in OCaml 4.08, but we're still on 4.07.
 * TODO: Delete this and just use Buffer once we update. *)
module Buf : sig
  type t

  val create : unit -> t

  val add_i8 : t -> int -> unit

  val contents : t -> string
end = struct
  type t = {
    mutable bytes: bytes;
    mutable length: int;
  }

  let create () = { bytes = Bytes.create 16; length = 0 }

  external unsafe_set_int8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"

  let add_i8 buf i =
    if buf.length = Bytes.length buf.bytes then begin
      let new_bytes = Bytes.create (buf.length * 2) in
      Bytes.blit buf.bytes 0 new_bytes 0 buf.length;
      buf.bytes <- new_bytes
    end;
    unsafe_set_int8 buf.bytes buf.length i;
    buf.length <- buf.length + 1

  let contents buf = Bytes.sub_string buf.bytes 0 buf.length
end

(* unsigned leb128-ish encoding *)
let add_int =
  let rec loop buf i =
    let byte = i land 0x7F in
    let i = i lsr 7 in
    if i != 0 then begin
      Buf.add_i8 buf (byte land 0x80);
      loop buf i
    end else
      Buf.add_i8 buf byte
  in
  fun buf i ->
    assert (i >= 0);
    loop buf i

let write_loc_start buf tag_adjust prev_column rline column =
  if rline = 0 then
    let rcolumn = column - prev_column in
    if rcolumn < 0x40 then
      Buf.add_i8 buf (tag_adjust + rcolumn)
    else begin
      Buf.add_i8 buf (tag_adjust + 0x40);
      add_int buf (rcolumn - 0x40)
    end
  else if rline < 0x3F then begin
    Buf.add_i8 buf (tag_adjust + 0x40 + rline);
    add_int buf column
  end else begin
    Buf.add_i8 buf (tag_adjust + 0x7F);
    add_int buf (rline - 0x3F);
    add_int buf column
  end

let pack len iter =
  let buf = Buf.create () in
  let prev_line = ref 0 in
  let prev_column = ref 0 in
  add_int buf len;
  iter (fun loc ->
      let open Loc in
      let start_rline = loc.start.line - !prev_line in
      let end_rline = loc._end.line - loc.start.line in
      if end_rline = 0 then begin
        (* single line *)
        write_loc_start buf 0 !prev_column start_rline loc.start.column;
        add_int buf (loc._end.column - loc.start.column)
      end else begin
        (* multiline *)
        write_loc_start buf 0x80 !prev_column start_rline loc.start.column;
        add_int buf end_rline;
        add_int buf loc._end.column
      end;
      prev_line := loc.start.line;
      prev_column := loc.start.column);
  Buf.contents buf

let mk_loc source start_line start_column end_line end_column =
  {
    Loc.source;
    start = { Loc.line = start_line; column = start_column };
    _end = { Loc.line = end_line; column = end_column };
  }

let unpack source init packed =
  let prev_line = ref 0 in
  let prev_column = ref 0 in
  let pos = ref 0 in

  let read_i8 () =
    let c = packed.[!pos] in
    incr pos;
    Char.code c
  in

  let read_int =
    let rec loop result shift =
      let byte = read_i8 () in
      let result = result lor ((byte land 0x7F) lsl shift) in
      if byte land 0x80 = 0 then
        result
      else
        loop result (shift + 7)
    in
    (fun () -> loop 0 0)
  in

  let read_loc () =
    let tag = read_i8 () in
    if tag < 0x40 then
      let length = read_int () in
      let start_line = !prev_line in
      let start_column = !prev_column + tag in
      mk_loc source start_line start_column start_line (start_column + length)
    else if tag < 0x7F then
      let start_rcolumn = read_int () in
      let length = read_int () in
      let start_rline = tag - 0x40 in
      let start_line = !prev_line + start_rline in
      let start_column =
        if start_rline = 0 then
          !prev_column + start_rcolumn + 0x40
        else
          start_rcolumn
      in
      mk_loc source start_line start_column start_line (start_column + length)
    else if tag = 0x7F then
      let start_rline = read_int () + 0x3F in
      let start_column = read_int () in
      let length = read_int () in
      let start_line = !prev_line + start_rline in
      mk_loc source start_line start_column start_line (start_column + length)
    else if tag < 0xC0 then
      let end_rline = read_int () in
      let end_column = read_int () in
      let start_line = !prev_line in
      let start_column = !prev_column + tag - 0x80 in
      mk_loc source start_line start_column (start_line + end_rline) end_column
    else if tag < 0xFF then
      let start_rcolumn = read_int () in
      let end_rline = read_int () in
      let end_column = read_int () in
      let start_rline = tag - 0xC0 in
      let start_line = !prev_line + start_rline in
      let start_column =
        if start_rline = 0 then
          !prev_column + start_rcolumn + 0x40
        else
          start_rcolumn
      in
      mk_loc source start_line start_column (start_line + end_rline) end_column
    else
      let start_rline = read_int () + 0x3F in
      let start_column = read_int () in
      let end_rline = read_int () in
      let end_column = read_int () in
      let start_line = !prev_line + start_rline in
      mk_loc source start_line start_column (start_line + end_rline) end_column
  in

  let len = read_int () in
  init len (fun _ ->
      let loc = read_loc () in
      let open Loc in
      prev_line := loc.start.line;
      prev_column := loc.start.column;
      loc)
