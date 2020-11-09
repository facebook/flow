(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type trust_qualifier

type trust_rep

type expanded_trust =
  | QualifiedTrust of trust_qualifier
  | InferredTrust of int

val is_ident : trust_rep -> bool

val is_qualifier : trust_rep -> bool

val expand : trust_rep -> expanded_trust

val compress : expanded_trust -> trust_rep

val as_qualifier : trust_rep -> trust_qualifier

val as_ident : trust_rep -> int

val from_ident : int -> trust_rep

val from_qualifier : trust_qualifier -> trust_rep

val trust_value : default:trust_qualifier -> trust_rep -> trust_qualifier

val trust_value_map : default:'t -> f:(trust_qualifier -> 't) -> trust_rep -> 't

val string_of_trust : trust_qualifier -> string

val string_of_trust_rep : (int -> trust_qualifier) -> trust_rep -> string

val bogus_trust : unit -> trust_rep

val literal_trust : unit -> trust_rep

val annot_trust : unit -> trust_rep

val unknown_qualifier : unit -> trust_qualifier

val dynamic_qualifier : unit -> trust_qualifier

val make_trusted : trust_qualifier -> trust_qualifier

val make_private : trust_qualifier -> trust_qualifier

val make_enforced : trust_qualifier -> trust_qualifier

val is_public : trust_qualifier -> bool

val is_tainted : trust_qualifier -> bool

val subtype_trust : trust_qualifier -> trust_qualifier -> bool

val publicize_with : trust_qualifier -> trust_qualifier -> trust_qualifier

val taint_with : trust_qualifier -> trust_qualifier -> trust_qualifier

val join_trust : trust_qualifier -> trust_qualifier -> trust_qualifier

val fix : trust_qualifier -> trust_qualifier
