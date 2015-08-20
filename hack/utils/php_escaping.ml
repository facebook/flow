(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Implementation of string escaping stuff. Ugggggggh.
 * See http://php.net/manual/en/language.types.string.php *)

exception Invalid_string of string;;

let is_printable c = c >= ' ' && c <= '~'
let is_lit_printable c = is_printable c && c <> '\\' && c <> '\"'

let is_hex c = (c >= '0' && c <= '9') ||
               (c >= 'a' && c <= 'f') ||
               (c >= 'A' && c <= 'F')
let is_oct c = c >= '0' && c <= '7'

(* This escapes a string using the format understood by the assembler
 * and php serialization. The assembler and php serialization probably
 * don't actually have the same rules but this should safely fit in both.
 * It will escape $ in octal so that it can also be used as a PHP double
 * string. *)
let escape s =
  let buf = Buffer.create (String.length s) in

  let process_char = function
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '$' -> Buffer.add_string buf "\\044"
    | c when is_lit_printable c -> Buffer.add_char buf c
    | c -> Buffer.add_string buf (Printf.sprintf "\\%03o" (Char.code c))
  in
  String.iter process_char s;

  Buffer.contents buf

(* Convert a codepoint to utf-8, appending the the bytes to a buffer *)
let codepoint_to_utf8 n buf =
  let add i = Buffer.add_char buf (Char.chr i) in
  if n <= 0x7F then begin
    add n
  end else if n <= 0x7FF then begin
    add (0xC0 lor ((n lsr 6)          ));
    add (0x80 lor ((n      ) land 0x3F));
  end else if n <= 0x00FFFF then begin
    add (0xE0 lor ((n lsr 12)          ));
    add (0x80 lor ((n lsr  6) land 0x3F));
    add (0x80 lor ((n       ) land 0x3F));
  end else if n <= 0x10FFFF then begin
    add (0xF0 lor ((n lsr 18)          ));
    add (0x80 lor ((n lsr 12) land 0x3F));
    add (0x80 lor ((n lsr  6) land 0x3F));
    add (0x80 lor ((n       ) land 0x3F));
  end else
    raise (Invalid_string "UTF-8 codepoint too large")

let parse_int s =
  try
    int_of_string s
  with _ -> raise (Invalid_string "invalid numeric escape")
let parse_numeric_escape s =
  try
    Char.chr (parse_int s)
  with _ -> raise (Invalid_string "escaped character too large")

let unescape_double s =
  let len = String.length s in
  let buf = Buffer.create len in
  let idx = ref 0 in
  let next () =
    if !idx >= len then raise (Invalid_string "string ended early") else
      let c = s.[!idx] in (incr idx; c)
  in

  (* Count how many characters, starting at the current string index.
   * Will always stop at i=max. *)
  let rec count_f f ~max i =
    if i >= max || !idx + i >= len || not (f s.[!idx+i]) then i
    else count_f f max (i+1)
  in

  while !idx < len do
    let c = next () in
    if c <> '\\' then Buffer.add_char buf c else begin
      let c = next () in
      match c with
      | '\'' -> Buffer.add_char buf '\''
      | 'n'  -> Buffer.add_char buf '\n'
      | 'r'  -> Buffer.add_char buf '\r'
      | 't'  -> Buffer.add_char buf '\t'
      | 'v'  -> Buffer.add_char buf '\x0b'
      | 'e'  -> Buffer.add_char buf '\x1b'
      | 'f'  -> Buffer.add_char buf '\x0c'
      | '\\' -> Buffer.add_char buf '\\'
      | '$'  -> Buffer.add_char buf '$'
      | '\"' -> Buffer.add_char buf '\"'
      | 'u' when !idx < len && s.[!idx] = '{' ->
        let _ = next () in
        let unicode_count = count_f (fun c -> c <> '}') ~max:6 0 in
        let n = parse_int ("0x" ^ String.sub s (!idx) unicode_count) in
        codepoint_to_utf8 n buf;
        idx := !idx + unicode_count;
        if next () <> '}' then
          raise (Invalid_string "Invalid UTF-8 escape sequence")
      | 'x' ->
        let hex_count = count_f is_hex ~max:2 0 in
        if hex_count = 0 then
          Buffer.add_string buf "\\x"
        else
          let c = parse_numeric_escape ("0x" ^ String.sub s (!idx) hex_count) in
          Buffer.add_char buf c;
          idx := !idx + hex_count
      | c when is_oct c ->
        idx := !idx - 1;
        let oct_count = count_f is_oct ~max:3 0 in
        let c = parse_numeric_escape ("0o" ^ String.sub s (!idx) oct_count) in
        Buffer.add_char buf c;
        idx := !idx + oct_count
      (* unrecognized escapes are just copied over *)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    end;

  done;

  Buffer.contents buf

let unescape_single s =
  let len = String.length s in
  let buf = Buffer.create len in
  let idx = ref 0 in
  let next () =
    if !idx >= len then raise (Invalid_string "string ended early") else
      let c = s.[!idx] in (incr idx; c)
  in

  while !idx < len do
    let c = next () in
    if c <> '\\' then Buffer.add_char buf c else begin
      let c = next () in
      match c with
      | '\'' ->Buffer.add_char buf '\''
      | '\\' ->Buffer.add_char buf '\\'
      (* unrecognized escapes are just copied over *)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    end;

  done;

  Buffer.contents buf
