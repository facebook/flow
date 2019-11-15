(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * When we store data to the shared heap, we first marshal it using OCaml's marshaller, then we
 * compress it. OCaml's marshaling algorithm uses a more compact representation for smaller
 * integers, so it is advantageous to use small integers rather than large ones when serializing to
 * the shared heap.
 *
 * To that end, this utility converts locations so that the end position is stored relative to the
 * start position, rather than storing it in absolute terms. The intuition is that the end location
 * will always be closer to (or as close as) the start position than to the start of the file, so
 * the numbers stored will be smaller and therefore have a more compact representation, on average.
 *
 * This does not change the in-memory size of the location. It does, however make it smaller to
 * serialize.
 * *)

type t

val of_loc : Loc.t -> t

val to_loc : t -> File_key.t option -> Loc.t
