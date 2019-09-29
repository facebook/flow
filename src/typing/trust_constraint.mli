(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

type ident = Type.ident

type bounds

type root

type node =
  | TrustGoto of ident
  | TrustRoot of root

type constraints =
  | TrustResolved of trust_qualifier
  | TrustUnresolved of bounds

val get_constraints : root -> constraints

val get_bounds : bounds -> ISet.t * ISet.t

val get_trust : bounds -> trust_qualifier

val resolved_trust_constraint : Reason.t -> trust_qualifier -> constraints

val new_unresolved_root : trust_qualifier -> node

val new_resolved_root : trust_qualifier -> node

val new_goto : ident -> node

val set_trust : bounds -> trust_qualifier -> unit

val extend_uppervars : bounds -> ISet.t -> unit

val extend_lowervars : bounds -> ISet.t -> unit
