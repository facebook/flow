(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* Implementation of string escaping stuff. Ugggggggh.
 * See http://php.net/manual/en/language.types.string.php *)

open Core_kernel

exception Invalid_string of string

let is_printable c = c >= ' ' && c <= '~'

let is_lit_printable c = is_printable c && c <> '\\' && c <> '\"'

let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_oct c = c >= '0' && c <= '7'

(* This escapes a string using the format understood by the assembler
 * and php serialization. The assembler and php serialization probably
 * don't actually have the same rules but this should safely fit in both.
 * It will escape $ in octal so that it can also be used as a PHP double
 * string. *)

let escape_char = function
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\\' -> "\\\\"
  | '"' -> "\\\""
  | '$' -> "$"
  | '?' -> "\\?"
  | c when is_lit_printable c -> String.make 1 c
  | c -> Printf.sprintf "\\%03o" (Char.to_int c)

let escape ?(f = escape_char) s =
  let buf = Buffer.create (String.length s) in
  Caml.String.iter (fun c -> Buffer.add_string buf @@ f c) s;
  Buffer.contents buf

(* Convert a codepoint to utf-8, appending the the bytes to a buffer *)
let codepoint_to_utf8 n buf =
  let add i = Buffer.add_char buf (Char.of_int_exn i) in
  if n <= 0x7F then
    add n
  else if n <= 0x7FF then (
    add (0xC0 lor (n lsr 6));
    add (0x80 lor (n land 0x3F))
  ) else if n <= 0x00FFFF then (
    add (0xE0 lor (n lsr 12));
    add (0x80 lor ((n lsr 6) land 0x3F));
    add (0x80 lor (n land 0x3F))
  ) else if n <= 0x10FFFF then (
    add (0xF0 lor (n lsr 18));
    add (0x80 lor ((n lsr 12) land 0x3F));
    add (0x80 lor ((n lsr 6) land 0x3F));
    add (0x80 lor (n land 0x3F))
  ) else
    raise (Invalid_string "UTF-8 codepoint too large")

let parse_int s =
  try int_of_string s
  with _ -> raise (Invalid_string "invalid numeric escape")

let parse_numeric_escape ?(trim_to_byte = false) s =
  try
    let v = parse_int s in
    let v =
      if trim_to_byte then
        v land 0xFF
      else
        v
    in
    Char.of_int_exn v
  with _ -> raise (Invalid_string "escaped character too large")

type literal_kind =
  | Literal_heredoc
  | Literal_double_quote
  | Literal_backtick
  (* string containing C-style char escapes (mimics escapeChar in as.cpp) *)
  | Literal_long_string

let unescape_literal literal_kind s =
  let len = String.length s in
  let buf = Buffer.create len in
  let idx = ref 0 in
  let next () =
    if !idx >= len then
      raise (Invalid_string "string ended early")
    else
      let c = s.[!idx] in
      incr idx;
      c
  in
  (* Count how many characters, starting at the current string index.
   * Will always stop at i=max. *)
  let rec count_f f ~max i =
    if i >= max || !idx + i >= len || not (f s.[!idx + i]) then
      i
    else
      count_f f max (i + 1)
  in
  while !idx < len do
    let c = next () in
    (* If it's the last character we're done *)
    if c <> '\\' || !idx = len then
      Buffer.add_char buf c
    else
      let c = next () in
      match c with
      | 'a' when literal_kind = Literal_long_string ->
        Buffer.add_char buf '\x07'
      | 'b' when literal_kind = Literal_long_string ->
        Buffer.add_char buf '\x08'
      | '\'' -> Buffer.add_string buf "\\\'"
      | 'n' ->
        if literal_kind <> Literal_long_string then Buffer.add_char buf '\n'
      | 'r' ->
        if literal_kind <> Literal_long_string then Buffer.add_char buf '\r'
      | 't' -> Buffer.add_char buf '\t'
      | 'v' -> Buffer.add_char buf '\x0b'
      | 'e' -> Buffer.add_char buf '\x1b'
      | 'f' -> Buffer.add_char buf '\x0c'
      | '\\' -> Buffer.add_char buf '\\'
      | '?' when literal_kind = Literal_long_string ->
        Buffer.add_char buf '\x3f'
      | '$' when literal_kind <> Literal_long_string -> Buffer.add_char buf '$'
      | '`' when literal_kind <> Literal_long_string ->
        if literal_kind = Literal_backtick then
          Buffer.add_char buf '`'
        else
          Buffer.add_string buf "\\`"
      | '\"' ->
        if
          literal_kind = Literal_double_quote
          || literal_kind = Literal_long_string
        then
          Buffer.add_char buf '\"'
        else
          Buffer.add_string buf "\\\""
      | 'u'
        when literal_kind <> Literal_long_string && !idx < len && s.[!idx] = '{'
        ->
        let _ = next () in
        let unicode_count = count_f (fun c -> c <> '}') ~max:6 0 in
        let n = parse_int ("0x" ^ Caml.String.sub s !idx unicode_count) in
        codepoint_to_utf8 n buf;
        idx := !idx + unicode_count;
        if next () <> '}' then
          raise (Invalid_string "Invalid UTF-8 escape sequence")
      | ('x' | 'X') as c ->
        let hex_count = count_f is_hex ~max:2 0 in
        if hex_count = 0 then (
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
        ) else
          let c =
            parse_numeric_escape ("0x" ^ Caml.String.sub s !idx hex_count)
          in
          Buffer.add_char buf c;
          idx := !idx + hex_count
      | c when is_oct c ->
        idx := !idx - 1;
        let oct_count = count_f is_oct ~max:3 0 in
        let c =
          parse_numeric_escape
            ~trim_to_byte:true
            ("0o" ^ Caml.String.sub s !idx oct_count)
        in
        Buffer.add_char buf c;
        idx := !idx + oct_count
      (* unrecognized escapes are just copied over *)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
  done;

  Buffer.contents buf

let unescape_double s = unescape_literal Literal_double_quote s

let unescape_backtick s = unescape_literal Literal_backtick s

let unescape_heredoc s = unescape_literal Literal_heredoc s

let unescape_single_or_nowdoc ~is_nowdoc s =
  let len = String.length s in
  let buf = Buffer.create len in
  let idx = ref 0 in
  let next () =
    if !idx >= len then
      raise (Invalid_string "string ended early")
    else
      let c = s.[!idx] in
      incr idx;
      c
  in
  while !idx < len do
    let c = next () in
    if is_nowdoc || c <> '\\' then
      Buffer.add_char buf c
    else
      let c = next () in
      match c with
      | '\'' -> Buffer.add_char buf '\''
      | '\\' -> Buffer.add_char buf '\\'
      (* unrecognized escapes are just copied over *)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
  done;

  Buffer.contents buf

let unescape_single s = unescape_single_or_nowdoc ~is_nowdoc:false s

let unescape_nowdoc s = unescape_single_or_nowdoc ~is_nowdoc:true s

let unescape_long_string s = unescape_literal Literal_long_string s

let extract_unquoted_string ~start ~len content =
  (* Using String.sub; Invalid_argument when str too short *)
  try
    if
      len >= 3 && Caml.String.sub content start 3 = "<<<" (* The heredoc case *)
    then
      (* These types of strings begin with an opening line containing <<<
       * followed by a string to use as a terminator (which is optionally
       * quoted) and end with a line containing only the terminator and a
       * semicolon followed by a blank line. We need to drop the opening line
       * as well as the blank line and preceding terminator line.
       *)
      let start_ = Core_kernel.String.index_from_exn content start '\n' + 1 in
      let end_ =
        Core_kernel.String.rindex_from_exn content (start + len - 2) '\n'
      in
      (* An empty heredoc, this way, will have start >= end *)
      if start_ >= end_ then
        ""
      else
        Caml.String.sub content start_ (end_ - start_)
    else
      match (content.[start], content.[start + len - 1]) with
      | ('"', '"')
      | ('\'', '\'')
      | ('`', '`') ->
        Caml.String.sub content (start + 1) (len - 2)
      | _ ->
        if start = 0 && len = String.length content then
          content
        else
          Caml.String.sub content start len
  with
  | Invalid_argument _
  | Core_kernel.Not_found_s _
  | Caml.Not_found ->
    content
