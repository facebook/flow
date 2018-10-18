(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

(* TODO at some point we will need a state type for the lookup table. probably going to pass it
 * around genv. *)

(* TODO at some point we will also need to provide a key of some sort here *)
val of_loc: Loc.t -> t
val to_loc: t -> Loc.t

val none: t

val source: t -> File_key.t option

val compare: t -> t -> int
