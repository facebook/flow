(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t

val is_empty : t -> bool

val of_focused_list : File_key.t list -> t

val cardinal : t -> int

val focused_cardinal : t -> int

val dependents_cardinal : t -> int

val dependencies_cardinal : t -> int

val mem : File_key.t -> t -> bool

val add :
  ?focused:Utils_js.FilenameSet.t ->
  ?dependents:Utils_js.FilenameSet.t ->
  ?dependencies:Utils_js.FilenameSet.t ->
  t ->
  t

val remove : Utils_js.FilenameSet.t -> t -> t

val fold : ('a -> File_key.t -> 'a) -> 'a -> t -> 'a

val union : t -> t -> t

(** [diff a b] removes from [a] every key which exists in [b] and which has an equal or higher
    kind in [b] than it does in [a], where Focused > Dependent > Dependency. So

    {[
      diff
        { A: Focused, B: Focused,   C: Dependency, D: Dependent }
        { A: Focused, B: Dependent, C: Dependent}
      = { B: Focused, D: Dependent }
    ]}
 *)
val diff : t -> t -> t

val filter : f:(File_key.t -> bool) -> t -> t

val all : t -> Utils_js.FilenameSet.t

val focused : t -> Utils_js.FilenameSet.t

val dependents : t -> Utils_js.FilenameSet.t

val dependencies : t -> Utils_js.FilenameSet.t

(* This is O(n) in the size of the checked set. Because checked sets are typically very large, this
 * operation should be avoided in production code. *)
val debug_equal : t -> t -> bool

val debug_to_string : ?limit:int -> t -> string

val debug_counts_to_string : t -> string
