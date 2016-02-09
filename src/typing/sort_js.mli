(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(* given a map from keys to dependencies, returns whether the dependencies are
   cyclic, as well as a topologically sorted list of key lists where any keys in
   a list only depend on keys in a subsequent list
*)
val topsort: FilenameSet.t FilenameMap.t -> filename list list IMap.t
val reverse: FilenameSet.t FilenameMap.t -> FilenameSet.t FilenameMap.t
val log: filename list list IMap.t -> unit
