(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* given a map from keys to dependencies, returns whether the dependencies are
   cyclic, as well as a topologically sorted list of key lists where any keys in
   a list only depend on keys in a subsequent list
*)
val topsort : roots:FilenameSet.t -> FilenameSet.t FilenameMap.t -> File_key.t Nel.t list

val log : File_key.t Nel.t list -> unit
