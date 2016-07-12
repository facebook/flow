(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type ident = int

(***************************************)

type node =
| Goto of ident
| Root of root

and root = {
  rank: int;
  constraints: constraints;
}

and constraints =
| Resolved of Type.t
| Unresolved of bounds

and bounds = {
  mutable lower: Trace.t Type.TypeMap.t;
  mutable upper: Trace.t Type.UseTypeMap.t;
  mutable lowertvars: Trace.t IMap.t;
  mutable uppertvars: Trace.t IMap.t;
}

val new_unresolved_root: unit -> node
val bounds_of_unresolved_root: node -> bounds

val copy_node: node -> node
