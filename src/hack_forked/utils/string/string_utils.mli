(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
   definitions that should not actually be exposed to the caller. If you notice
   that this interface file is a poor interface, please take a few minutes to
   clean it up manually, and then delete this comment once the interface is in
   shape. *)

exception Incorrect_format

val soi : int -> string

val string_of_char : char -> string

val string_before : string -> int -> string

val string_after : string -> int -> string

val string_starts_with : string -> string -> bool

val string_ends_with : string -> string -> bool

(** [substring_index needle haystack] returns the index of the first occurrence of
    string [needle] in string [haystack]. If not found, returns [-1].

    An implementation of the Knuth-Morris-Pratt (KMP) algorithm. *)
val substring_index : string -> string -> int

val is_substring : string -> string -> bool

(** [lstrip s prefix] returns a copy of [s] with [prefix] removed from
    the beginning if [s] begins with [prefix], or [s] itself if not.
    Physical equality is maintained in the latter case. *)
val lstrip : string -> string -> string

(** [rstrip s suffix] returns a copy of [s] with [suffix] removed from
    the end if [s] ends with [suffix], or [s] itself if not. Physical
    equality is maintained in the latter case. *)
val rstrip : string -> string -> string

val rpartition : string -> char -> string * string

val truncate : int -> string -> string

(** [index_not_from_opt str i chars] is like [index_from_opt], but returns the index of the first
    char in [str] after position [i] that is not in [chars] if it exists, or [None] otherwise. *)
val index_not_from_opt : string -> int -> string -> int option

(** [index_not_opt str chars] is like [index_opt], but returns the index of the first char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)
val index_not_opt : string -> string -> int option

(** [rindex_not_from_opt str i chars] is like [rindex_from_opt], but returns the index of the last
    char in [str] before position [i+1] that is not in [chars] if it exists, or [None] otherwise. *)
val rindex_not_from_opt : string -> int -> string -> int option

(** [rindex_not_opt str chars] is like [rindex_opt], but returns the index of the last char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)
val rindex_not_opt : string -> string -> int option

val zero_code : int

val nine_code : int

val is_decimal_digit : char -> bool

val is_lowercase_char : char -> bool

val is_not_lowercase : string -> int -> int -> bool

val fold_left : f:('a -> char -> 'a) -> acc:'a -> string -> 'a

val split : char -> string -> string list

val split2 : char -> string -> (string * string) option

val split2_exn : char -> string -> string * string

(** [replace_char needle replacement str] replaces all instances of the [needle]
    character in [str] with the [replacement] character *)
val replace_char : char -> char -> string -> string

(** Splits a string into a list of strings using "\n", "\r" or "\r\n" as
    delimiters. If the string starts or ends with a delimiter, there WILL be an
    empty string at the beginning or end of the list, like [Str.split_delim] does. *)
val split_into_lines : string -> string list

(** Splits a string into lines, indents each non-empty line, and concats with newlines *)
val indent : int -> string -> string

(** Splits a string into a list of strings using only "\n" as a delimiter.
    If the string ends with a delimiter, an empty string representing the
    contents after the final delimiter is NOT included (unlike [Str.split_delim]). *)
val split_on_newlines : string -> string list

module Internal : sig
  val to_list : string -> char list

  val of_list : char list -> string
end

val to_list : string -> char list

val of_list : char list -> string

module CharSet : sig
  include Flow_set.S with type elt = Char.t

  val of_string : string -> t

  val to_string : t -> string
end

(** Levenshtein distance algorithm.

   Based on the public domain implementation at
   https://bitbucket.org/camlspotter/ocaml_levenshtein/src/default/
*)
val levenshtein_distance : string -> string -> int
