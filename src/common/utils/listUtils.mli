(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val fold_left_opt : ('a -> 'b -> 'a option) -> 'a -> 'b list -> 'a option

val fold_left_until : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> 'a

val first_upto_n : int -> (int -> 'a option) -> 'a list -> 'a list

val copy_n : int -> 'a -> 'a list

val ident_map : ('a -> 'a) -> 'a list -> 'a list

val ident_mapi : (int -> 'a -> 'a) -> 'a list -> 'a list

val ident_map_multiple : ('a -> 'a list) -> 'a list -> 'a list

val ident_filter : ('a -> bool) -> 'a list -> 'a list

val combine3 : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

val zipi : 'a list -> 'b list -> (int * 'a * 'b) list

val range_with : (int -> 'a) -> int -> int -> 'a list

val range : int -> int -> int list

val repeat : int -> 'a -> 'a list

val concat_fold : ('a -> 'b -> 'a * 'c list) -> 'a -> 'b list -> 'a * 'c list

val last_opt : 'a list -> 'a option

val dedup : 'a list -> 'a list

val to_string : string -> ('a -> string) -> 'a list -> string

val assoc_to_string :
  string -> ('a -> string) -> string -> ('b -> string) -> ('a * 'b) list -> string
