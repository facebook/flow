(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Note on character encodings:
 *
 * Throughout Flow, we assume that program text uses a UTF-8 encoding. OCaml strings are just a
 * sequence of bytes, so any handling of multi-byte characters needs to be done explicitly.
 *
 * Column numbers in `Loc.position`s are based on the number of characters into a line the position
 * appears, not the number of bytes. Single-byte and multi-byte characters are treated the same for
 * the purposes of counting columns.
 *
 * However, offsets are most useful (at least when working with OCaml's string representation) when
 * they represent the number of bytes into the text a given position is. Therefore, this utility
 * returns such offsets.
 *
 * In contrast, JavaScript strings must behave as if they have a UTF-16 encoding, and each element
 * is a single 16-bit entry. So, each character occupies either one or two elements of a JavaScript
 * string. Esprima, for example, returns ranges based on index into a JS string. For example, this
 * utility would consider the smiley emoji (code point 0x1f603) to have width 4 (because its UTF-8
 * encoding is 4 8-bit elements), but Esprima would consider it to have width 2 (because its UTF-16
 * encodinng is 2 16-bit elements).
 *
 * If necessary to improve compatibility with Esprima, this utility could be extended to
 * additionally track what the offsets into JS-style strings would be.
 *)

(* A structure that allows for quick computation of offsets when given a Loc.position *)
type t

(* Create a table for offsets in the given file. Takes O(n) time and returns an object that takes
 * O(n) space, where `n` is the size of the given program text. *)
val make : string (* program text *) -> t

exception Offset_lookup_failed of Loc.position * string

(* Returns the offset for the given location. This is the offset in bytes (not characters!) into the
 * file where the given position can be found. Constant time operation. Raises
 * `Offset_lookup_failed` if the given position does not exist in the file contents which were used
 * to construct the table. *)
val offset : t -> Loc.position -> int

val debug_string : t -> string
