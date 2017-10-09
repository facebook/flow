(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* given a map from keys to dependencies, returns whether the dependencies are
   cyclic, as well as a topologically sorted list of key lists where any keys in
   a list only depend on keys in a subsequent list
*)
val topsort: FilenameSet.t FilenameMap.t -> File_key.t list list IMap.t
val reverse: FilenameSet.t FilenameMap.t -> FilenameSet.t FilenameMap.t
val log: File_key.t list list IMap.t -> unit
val component_map: File_key.t list list IMap.t -> (File_key.t list) FilenameMap.t
