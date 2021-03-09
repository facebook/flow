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

val substring_index : string -> string -> int
(** [substring_index needle haystack] returns the index of the first occurrence of
    string [needle] in string [haystack]. If not found, returns [-1].

    An implementation of the Knuth-Morris-Pratt (KMP) algorithm. *)

val is_substring : string -> string -> bool

val lstrip : string -> string -> string
(** [lstrip s prefix] returns a copy of [s] with [prefix] removed from
    the beginning if [s] begins with [prefix], or [s] itself if not.
    Physical equality is maintained in the latter case. *)

val rstrip : string -> string -> string
(** [rstrip s suffix] returns a copy of [s] with [suffix] removed from
    the end if [s] ends with [suffix], or [s] itself if not. Physical
    equality is maintained in the latter case. *)

val rpartition : string -> char -> string * string

val truncate : int -> string -> string

val index_not_from_opt : string -> int -> string -> int option
(** [index_not_from_opt str i chars] is like [index_from_opt], but returns the index of the first
    char in [str] after position [i] that is not in [chars] if it exists, or [None] otherwise. *)

val index_not_opt : string -> string -> int option
(** [index_not_opt str chars] is like [index_opt], but returns the index of the first char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)

val rindex_not_from_opt : string -> int -> string -> int option
(** [rindex_not_from_opt str i chars] is like [rindex_from_opt], but returns the index of the last
    char in [str] before position [i+1] that is not in [chars] if it exists, or [None] otherwise. *)

val rindex_not_opt : string -> string -> int option
(** [rindex_not_opt str chars] is like [rindex_opt], but returns the index of the last char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)

val zero_code : int

val nine_code : int

val is_decimal_digit : char -> bool

val is_lowercase_char : char -> bool

val is_not_lowercase : string -> int -> int -> bool

val fold_left : f:('a -> char -> 'a) -> acc:'a -> string -> 'a

val split : char -> string -> string list

val split2 : char -> string -> (string * string) option

val split2_exn : char -> string -> string * string

val replace_char : char -> char -> string -> string
(** [replace_char needle replacement str] replaces all instances of the [needle]
    character in [str] with the [replacement] character *)

val split_into_lines : string -> string list
(** Splits a string into a list of strings using "\n", "\r" or "\r\n" as
    delimiters. If the string starts or ends with a delimiter, there WILL be an
    empty string at the beginning or end of the list, like [Str.split_delim] does. *)

val indent : int -> string -> string
(** Splits a string into lines, indents each non-empty line, and concats with newlines *)

val split_on_newlines : string -> string list
(** Splits a string into a list of strings using only "\n" as a delimiter.
    If the string ends with a delimiter, an empty string representing the
    contents after the final delimiter is NOT included (unlike [Str.split_delim]). *)

module Internal : sig
  val to_list : string -> char list

  val of_list : char list -> string
end

val to_list : string -> char list

val of_list : char list -> string

module CharSet : sig
  type elt = Char.t

  type t = Set.Make(Char).t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt

  val max_elt_opt : t -> elt option

  val choose : t -> elt

  val choose_opt : t -> elt option

  val split : elt -> t -> t * bool * t

  val find : elt -> t -> elt

  val find_opt : elt -> t -> elt option

  val find_first : (elt -> bool) -> t -> elt

  val find_first_opt : (elt -> bool) -> t -> elt option

  val find_last : (elt -> bool) -> t -> elt

  val find_last_opt : (elt -> bool) -> t -> elt option

  val of_list : elt list -> t

  val of_string : string -> t

  val to_string : t -> string
end

val levenshtein_distance : string -> string -> int
(** Levenshtein distance algorithm.

   Based on the public domain implementation at
   https://bitbucket.org/camlspotter/ocaml_levenshtein/src/default/
*)
