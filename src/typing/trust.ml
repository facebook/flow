(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Trust information describes a type's relationship with the any type.
   These may be applied to any DefT (except AnyT), and determine whether
   the type is a subtype, supertype, both, or neither of "any".
   * Initial = T < any, any </ T
       Describes types where the "history" of the values that inhabit them
       is fully known and excludes the any type, or when the values have no
       history--e.g. literals, which we know are trusted but which may flow
       elsewhere in the future
   * Dynamic = any < T < any
       Describes types that allow free interaction with the any type. You can't
       trust something Dynamic (because something incorrect could have flowed
       into it through any) and it can flow into any.
   * Terminal = T </ any, any < T
       This is kind of a weird one; you're not likely to actually run into it
       in the wild--why bother being private if you're not also trusted, right?
       But it's important in that both Dynamic and Static things can flow into
       it, which makes it the top of this lattice, and DefT(_, Terminal, MixedT)
       is now going to be the absolute top of the DefT type hierarchy.
   * Static = T </ any, any </ T
       Static things are entirely unrelated to any--as far as they're concerned,
       they exist in a wholly statically typed language, without any.
   To determine whether something is trusted we just need to care about whether
   any is a subtype of it or not: if any is a subtype, then we can't trust it,
   otherwise we can. However, we care about the dual notion of "privacy" because
   of contravariance: if we were allowed to pass (ex.) an array of trusted ints
   to any, we would lose our guarantees of soundness, because the any-typed code
   could arbitrarily mutate the array to contain strings. To prevent this, such
   an array must be private.

   For a formal treatment of these ideas, see Avik Chaudhuri's manuscript
   "Safe Types in Untyped Contexts" (to appear)
 *)
type trust = Initial | Dynamic | Terminal | Static

let string_of_trust = function
  | Initial -> "<Trusted>"
  | Dynamic -> "<Untrusted>"
  | Terminal -> "<Untrusted/Private>"
  | Static -> "<Trusted/Private>"

(* Trust creation functions and modules:
   These functions will be used to generate trust information when DefTs are
   instantiated (unless the trust information ought to be propagated from some
   other type. *)
(* bogus_trust is a development placeholder, used to invent trust when we
   haven't yet figured out what the right trust information to use is. The
   number of calls to bogus_trust in the codebase is likely to be a decent
   measure of how complete the trusted types project is. *)
let bogus_trust () = Dynamic

(* literal_trust is the trust of literals: they're known to be trusted, since
   we can see their entire (trivial) history, but they're by default free to
   flow anywhere. *)
let literal_trust () = Initial

(* annot_trust is the trust of standard type annotations; currently, unless they're made to be
   trusted with the below operators, they accept untrusted values. *)
let annot_trust () = Dynamic

let dynamic_trust () = Dynamic

(* Given a trust datum, add (if not already present) the requirement that it
   be trusted or private. *)
let make_trusted = function
  | Dynamic -> Initial
  | Terminal -> Static
  | t -> t
let make_private = function
  | Dynamic -> Terminal
  | Initial -> Static
  | t -> t


let is_tainted = function
  | Dynamic | Terminal -> true
  | _ -> false
let is_public = function
  | Dynamic | Initial -> true
  | _ -> false

let subtype_trust l u =
  match l, u with
  | Initial, _
  | _, Terminal -> true
  | _ -> l = u
