(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ident = int

module UseTypeKey : sig
  type speculation_id = int

  type case_id = int

  type key = Type.use_t * (speculation_id * case_id) option

  type t = key

  val compare : key -> key -> int
end

module UseTypeMap : sig
  include module type of WrappedMap.Make (UseTypeKey)
end

(***************************************)

type node =
  | Goto of ident
  | Root of root

and root = {
  rank: int;
  constraints: constraints;
}

and constraints =
  | Resolved of Type.use_op * Type.t
  | FullyResolved of Type.use_op * Type.t
  | Unresolved of bounds

and bounds = {
  mutable lower: (Trace.t * Type.use_op) Type.TypeMap.t;
  mutable upper: Trace.t UseTypeMap.t;
  mutable lowertvars: (Trace.t * Type.use_op) IMap.t;
  mutable uppertvars: (Trace.t * Type.use_op) IMap.t;
}

val new_unresolved_root : unit -> node

val new_resolved_root : Type.t -> Type.use_op -> node
