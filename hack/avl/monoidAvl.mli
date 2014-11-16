(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* This code has been copy/pasted from the OCaml standard library.
 * There is only a slight modification.
 * Every element of the Avl tree is augmented with an additional element
 * of a monoid.
 * During all the operations on the tree, the element of the monoid
 * is maintained, the composition is always from left to right.
 * So if you have the tree ((0, Empty, Empty), 34, (45, Empty, Empty)
 * The "augmented tree" will look like this:
 * (make 0, 0, Empty, Empty), 
 * (compose (make 0) (compose ((make 34) (make 45)))), 34, 
 * (make 34, 45, Empty, Empty)
 *
 * We need this data-structure to quickly scan a tree.
 * We want 2 operations to be fast:
 * 1) add a file with a timestamp to the tree
 * 2) fetch all the files younger than time t
 *
 * The element of the monoid is helping us to make 2) fast.
 * Our composition function is "max", this way for any given
 * node we know how old the youngest element in a node is.
 * If the youngest element is too old, we can cut the branch.
 * (cf function walk in monoidAvl.ml)
*)

module type MonoidOrderedType =
  sig
    type elt
    val compare : elt -> elt -> int

    type monoelt
    val neutral: monoelt
    val compose: monoelt -> monoelt -> monoelt
    val make: elt -> monoelt
  end

module type S =
  sig
    type elt
    type monoelt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val split: elt -> t -> t * bool * t
    val walk: (monoelt -> bool) -> (elt -> unit) -> t -> unit
 end

module Make (Ord : MonoidOrderedType) : S 
with type elt = Ord.elt 
with type monoelt = Ord.monoelt
