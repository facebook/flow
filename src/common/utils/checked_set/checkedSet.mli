(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
type kind

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

val filter : f:(File_key.t -> kind -> bool) -> t -> t
val filter_into_set : f:(kind -> bool) -> t -> Utils_js.FilenameSet.t
val partition : f:(File_key.t -> kind -> bool) -> t -> t * t
val all : t -> Utils_js.FilenameSet.t
val is_focused : kind -> bool
val is_dependent : kind -> bool
val is_dependency : kind -> bool
val focused : t -> Utils_js.FilenameSet.t
val dependents : t -> Utils_js.FilenameSet.t
val dependencies : t -> Utils_js.FilenameSet.t
val mem_focused : File_key.t -> t -> bool
val mem_dependent : File_key.t -> t -> bool
val mem_dependency : File_key.t -> t -> bool

(* This is O(n) in the size of the checked set. Because checked sets are typically very large, this
 * operation should be avoided in production code. *)
val debug_equal : t -> t -> bool
val debug_to_string : ?limit:int -> t -> string
val debug_counts_to_string : t -> string
