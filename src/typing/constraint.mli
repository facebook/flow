(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  mutable lower: (Trace.t * Type.use_op) Type.TypeMap.t;
  mutable upper: Trace.t Type.UseTypeMap.t;
  mutable lowertvars: Trace.t IMap.t;
  mutable uppertvars: Trace.t IMap.t;
}

val new_unresolved_root: unit -> node

val copy_node: node -> node
