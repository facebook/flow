(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module M_ = Monad

val fold_left_opt : ('a -> 'b -> 'a option) -> 'a -> 'b list -> 'a option
val fold_left_until : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> 'a
val fold_left_while : ('a -> 'b -> bool) ->
                      ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_left_for : int -> ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val first_some_map : ('a -> 'b option) -> 'a list -> 'b option
val first_upto_n : int -> (int -> 'a option) -> 'a list -> 'a list
val first_n : int -> 'a list -> 'a list
val last_n : int -> 'a list -> 'a list
val copy_n : int -> 'a -> 'a list
val uniq : 'a list -> 'a list
val phys_uniq : 'a list -> 'a list
val ident_map : ('a -> 'a) -> 'a list -> 'a list
val combine3 : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val zipi : 'a list -> 'b list -> (int * 'a * 'b) list
val range_with : (int -> 'a) -> int -> int -> 'a list
val range : int -> int -> int list
val repeat : int -> 'a -> 'a list
val cat_maybes : 'a option list -> 'a list
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val concat_fold : ('a -> 'b -> 'a * 'c list) -> 'a -> 'b list -> 'a * 'c list

module Monad (M : M_.S) : sig
  include M_.S with type 'a t := 'a M.t
  val fold_map_m : ('a -> 'b -> ('a * 'c) M.t) -> 'a -> 'b list -> ('a * 'c list) M.t
  val concat_fold_m : ('a -> 'b -> ('a * 'c list) M.t)
                   -> 'a -> 'b list
                   -> ('a * 'c list) M.t
end

module Monad2 (M : M_.S2) : sig
  include M_.S2 with type ('a,'b) t := ('a,'b) M.t
  val fold_map_m : ('a -> 'b -> ('a * 'c, 'd) M.t) -> 'a -> 'b list -> ('a * 'c list, 'd) M.t
  val concat_fold_m : ('a -> 'b -> ('a * 'c list, 'd) M.t)
                   -> 'a -> 'b list
                   -> ('a * 'c list, 'd) M.t
end

val to_string: string -> ('a -> string) -> ('a list -> string)
val assoc_to_string: string -> ('a -> string) -> string -> ('b -> string) ->
  (('a * 'b) list -> string)
