(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* trust_qualifier information describes a type's relationship with the any type.
   These may be applied to any DefT (except AnyT), and determine whether
   the type is a subtype, supertype, both, or neither of "any".
   To determine whether something is trusted we just need to care about whether
   any is a subtype of it or not: if any is a subtype, then we can't trust_qualifier it,
   otherwise we can. However, we care about the dual notion of "privacy" because
   of contravariance: if we were allowed to pass (ex.) an array of trusted ints
   to any, we would lose our guarantees of soundness, because the any-typed code
   could arbitrarily mutate the array to contain strings. To prevent this, such
   an array must be private.

   In addition, the trustedness of some types are not always known. For example,
   we may wish to infer whether some type annotation is trusted or not in order
   to report the trust_qualifier coverage of a module.
   To do this inference, we need to be able to track interactions between types.
   For example, if any flows into number, then that number needs to be marked as
   untrusted. We'll accomplish this by reusing some of the ideas behind Flow's
   tvar inference, but in order to do so, the type whose trust_qualifier is being inferred
   (in this case number) needs to have an index into a graph of trust_qualifier inference
   variables, just like tvars contain an index into the context graph.

   This module defines trust_qualifier information for Flow types. Since trust_qualifier information
   is stored on ALL DefTs in the system (see type.ml), we want trust_qualifier to have a
   very compact representation, and we need to represent both
   trustedness/privacy and pointers into the trust_qualifier graph.

   The primary types that this module exports are the `trust` type and the
   `trust_rep` type. A value of type `trust` represents trustedness and privacy:
   these are the values that are directly compared to raise errors if an any
   type flows into a type that is annotated as being trusted, for example.
   A value of type 'trust_rep' is what is stored in DefTs, and represents EITHER
   a `trust` value, OR a pointer into the trust_qualifier variable graph.

   ** trust_rep **
   In principle, we'd love to define the `trust_rep` type as such:
     type trust_rep = Qualifier of trust_qualifier | Inferred of ident
   but such a representation in every DefT would cause a pretty huge memory
   overhead. Instead, we use a bitwise representation: a trust_rep value is a
   63bit int with the layout
     (lower bits) [ tag | data ...... ] (upper bits)
                    ^     ^
                  1 bit   62 bits

   If tag is 0, then data should be interpreted as trust_qualifier information, and can be
   converted to type `trust`. (See below for the representation of `trust`.)

   If tag is 1, then data should be interpreted as an identifier, which will be
   a pointer into a graph for trust_qualifier inference, and be converted to an ident.
   We're assuming we'll never have more than 2^62 vars :)

   This is all internal implementation details within this module, of course.
   Externally, the core API for using trust_rep is:
     is_qualifier: Does a trust_rep represent trust_qualifier data?
     is_ident: Does a trust_rep represent and inference ident?
     as_qualifier,
     as_ident: convert to trust_qualifier or ident and assert_false if the value does not
                correspond
     from_qualifier,
     from_ident: convert from trust_qualifier or ident to trust_rep
     expand: convert from a trust_rep into the expanded trust_qualifier representation as
              above (qualifier of trust_qualifier | Inferred of ident)
     compress: convert from the expanded representation into the compact one.

   ** trust_qualifier **
   A value of type `trust` represents whether a type is trusted or tainted,
   and public or private. Each trust_qualifier value has two components, represented by
   the `trust_level` type, which represent trust_qualifier and privacy. Each trust_qualifier element
   can be "top" (representing trusted or private) or "bot" (representing
   tainted or public). The `trust_level` type is also inhabited by "unk" for when
   the trustedness or privacy of a trust_qualifier value is unknown, as in the initial
   state of a trust_qualifier variable, and "enf", which represents when runtime
   checks are ensuring that a value is trusted.

   Internally a trust_qualifier value  is represented as an int, with the `trust_level`s each
   consisting of two bits:
     (lower bits) [ privacy | trust_qualifier | unused... ] (upper bits)
                     ^         ^
                   trust_level  trust_level
                   2 bits    2 bits

   trust_levels do not need to be used externally: instead, the API for `trust`
   values allows clients to check if a trust_qualifier value is trusted or private,
   whether two trust_qualifier values are related by subtyping, etc.

   ** trust_qualifier constructors **

   This module exports several constructors for trusts and trust_reps which are
   described below. The basic trust_qualifier values they refer to are

    * Initial = (Top, Bot) = T < any, any </ T
        Describes types where the "history" of the values that inhabit them
        is fully known and excludes the any type, or when the values have no
        history--e.g. literals, which we know are trusted but which may flow
        elsewhere in the future
    * Dynamic = (Bot, Bot) = any < T < any
        Describes types that allow free interaction with the any type. You can't
        trust_qualifier something Dynamic (because something incorrect could have flowed
        into it through any) and it can flow into any.
    * Terminal = (Bot, Top) = T </ any, any < T
        This is kind of a weird one; you're not likely to actually run into it
        in the wild--why bother being private if you're not also trusted, right?
        But it's important in that both Dynamic and Static things can flow into
        it, which makes it the top of this lattice, and DefT(_, Terminal, MixedT)
        is now going to be the absolute top of the DefT type hierarchy.
    * Static = (Top, Top) = T </ any, any </ T
        Static things are entirely unrelated to any--as far as they're concerned,
        they exist in a wholly statically typed language, without any.
    * Inferred = (Unk, Unk)
        For when trust_qualifier is unknown and we're trying to infer, or if we just don't care.
   For a formal treatment of these ideas, see Avik Chaudhuri's manuscript
   "Safe Types in Untyped Contexts" (to appear)
 *)
let mask b = (1 lsl b) - 1

type ident = int

module TrustRepresentation : sig
  type trust_qualifier

  type trust_rep

  type trust_level

  val fail_trust : (int -> string, unit, string) format -> trust_qualifier -> 'a

  val fail_trust_rep : (int -> string, unit, string) format -> trust_rep -> 'a

  val untag_ident : trust_rep -> ident

  val untag_qualifier : trust_rep -> trust_qualifier

  val tag_ident : ident -> trust_rep

  val tag_qualifier : trust_qualifier -> trust_rep

  val is_ident : trust_rep -> bool

  val is_qualifier : trust_rep -> bool

  val bot : trust_level

  val top : trust_level

  val unk : trust_level

  val enf : trust_level

  val get_taint : trust_qualifier -> trust_level

  val get_pub : trust_qualifier -> trust_level

  val make_trust : trust_level -> trust_level -> trust_qualifier
end = struct
  type bitrep = int

  type trust_qualifier = bitrep

  type trust_rep = bitrep

  let fail s (n : int) = Utils_js.assert_false (Utils_js.spf s n)

  let fail_trust s (n : trust_qualifier) = fail s n

  let fail_trust_rep s (n : trust_rep) = fail s n

  module Tag : sig
    type tag

    val qualifier : tag

    val ident : tag

    val get_tag : trust_rep -> tag

    val untag : trust_rep -> bitrep

    val tag : tag -> bitrep -> trust_rep
  end = struct
    type tag = int

    let tag_size = 1

    let tag_mask = mask tag_size

    let qualifier = 0

    let ident = 1

    let get_tag n = n land tag_mask

    let untag n = n lsr tag_size

    let tag t n = (n lsl tag_size) lor t
  end

  let is_ident (n : trust_rep) : bool = Tag.get_tag n = Tag.ident

  let is_qualifier (n : trust_rep) : bool = Tag.get_tag n = Tag.qualifier

  let untag_ident : trust_rep -> ident = Tag.untag

  let untag_qualifier : trust_rep -> trust_qualifier = Tag.untag

  let tag_qualifier : trust_qualifier -> trust_rep = Tag.tag Tag.qualifier

  let tag_ident : ident -> trust_rep = Tag.tag Tag.ident

  module Elt : sig
    type trust_level

    val bot : trust_level

    val top : trust_level

    val unk : trust_level

    val enf : trust_level

    val get_taint : trust_qualifier -> trust_level

    val get_pub : trust_qualifier -> trust_level

    val make_trust : trust_level -> trust_level -> trust_qualifier
  end = struct
    type trust_level = int

    let elt_size = 2

    let elt_mask = mask elt_size

    let bot : trust_level = 0

    let top : trust_level = 1

    let unk : trust_level = 2

    let enf : trust_level = 3

    let get_taint (n : trust_qualifier) : trust_level = (n lsr elt_size) land elt_mask

    let get_pub (n : trust_qualifier) : trust_level = n land elt_mask

    let make_trust (taint : trust_level) (pub : trust_level) : trust_qualifier =
      (taint lsl elt_size) lor pub
  end

  include Elt
end

include TrustRepresentation

type expanded_trust =
  | Qualifier of trust_qualifier
  | Inferred of ident

let dynamic = make_trust bot bot

let initial = make_trust top bot

let terminal = make_trust bot top

let static = make_trust top top

let infertrust = make_trust unk unk

let dynamic_info = tag_qualifier dynamic

let _initial_info = tag_qualifier initial

let _terminal_info = tag_qualifier terminal

let _static_info = tag_qualifier static

let infer_info = tag_qualifier infertrust

let bad_trust_rep n = fail_trust "invalid trust_qualifier representation: %d" n

let expand n =
  if is_ident n then
    Inferred (untag_ident n)
  else
    Qualifier (untag_qualifier n)

let compress x =
  match x with
  | Inferred n -> tag_ident n
  | Qualifier trust_qualifier -> tag_qualifier trust_qualifier

let as_qualifier n =
  if is_qualifier n then
    untag_qualifier n
  else
    fail_trust_rep "trust_rep value does not represent trust: %d" n

let as_ident n =
  if is_ident n then
    untag_ident n
  else
    fail_trust_rep "trust_rep value does not represent inference ident: %d" n

let from_ident ident = tag_ident ident

let from_qualifier n = tag_qualifier n

let trust_value ~default n =
  if is_qualifier n then
    untag_qualifier n
  else
    default

let trust_value_map ~default ~f n =
  if is_qualifier n then
    untag_qualifier n |> f
  else
    default

let string_of_taint t =
  let n = get_taint t in
  if n = bot then
    "Tainted"
  else if n = top then
    "Trusted"
  else if n = unk then
    "?"
  else if n = enf then
    "Enforced"
  else
    bad_trust_rep t

let string_of_pub t =
  let n = get_pub t in
  if n = bot then
    "Public"
  else if n = top then
    "Private"
  else if n = unk then
    "?"
  else if n = enf then
    "Enforced"
  else
    bad_trust_rep t

let string_of_trust n = Printf.sprintf "<%s/%s>" (string_of_taint n) (string_of_pub n)

let string_of_trust_rep get_trust n =
  if is_qualifier n then
    untag_qualifier n |> string_of_trust
  else
    Printf.sprintf "%d -> [%s]" (untag_ident n) (untag_ident n |> get_trust |> string_of_trust)

(* trust_qualifier creation functions and modules:
   These functions will be used to generate trust_qualifier information when DefTs are
   instantiated (unless the trust_qualifier information ought to be propagated from some
   other type. *)
(* bogus_trust is a development placeholder, used to invent trust_qualifier when we
   haven't yet figured out what the right trust_qualifier information to use is. The
   number of calls to bogus_trust in the codebase is likely to be a decent
   measure of how complete the trusted types project is. *)
let bogus_trust () = dynamic_info

(* literal_trust is the trust_qualifier of literals: they're known to be trusted, since
   we can see their entire (trivial) history, but they're by default free to
   flow anywhere. *)
let literal_trust () = infer_info

(* annot_trust is the trust_qualifier of standard type annotations; currently, unless they're made to be
   trusted with the below operators, they accept untrusted values. *)
let annot_trust () = dynamic_info

let unknown_qualifier () = infertrust

let dynamic_qualifier () = dynamic

(* Given a trust_qualifier datum, add (if not already present) the requirement that it
   be trusted or private. *)
let make_trusted n = make_trust top (get_pub n)

let make_private n = make_trust (get_taint n) top

let make_enforced n = make_trust enf (get_pub n)

let is_tainted n = get_taint n = bot

let is_public n = get_pub n = bot

let subtype_bit l u = l = u || (l = bot && u = top) || l = unk || u = unk || l = enf || u = enf

let subtype_trust l u =
  subtype_bit (get_taint u) (get_taint l) && subtype_bit (get_pub l) (get_pub u)

let taint_with tainted other =
  if get_taint other <> enf && is_tainted tainted then
    make_trust bot (get_pub other)
  else
    other

let publicize_with public other =
  if is_public public then
    make_trust (get_taint other) bot
  else
    other

let join_bit l r =
  if l = enf || r = enf then
    enf
  else if l = top || r = top then
    top
  else if l = bot || r = bot then
    bot
  else
    unk

let join_trust l r =
  make_trust (join_bit (get_taint l) (get_taint r)) (join_bit (get_pub l) (get_pub r))

let fix_bit n =
  if n = unk then
    bot
  else
    n

let fix t = make_trust (fix_bit (get_taint t)) (fix_bit (get_pub t))
