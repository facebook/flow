(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel

type 'a t
(** [Lazy_string_table.t] provides a memoization cache for any
    [(string * 'a) Sequence.t] where:

    + It is desirable to look up elements in the sequence by their [string] key
    + Conflicts between multiple elements with the same key can be resolved by
      a [merge] function

    The cache will advance the provided sequence only as needed, and the
    user-provided [is_canonical] function helps to enable this (see {!make} for
    more details). The cache is guaranteed to traverse the input sequence only
    once (so it will behave correctly if the input sequence depends on mutable
    state).

    Originally written for caches of class members, where we want to lazily
    parse ancestor classes only as necessary, and our implementation of [merge]
    provides the logic for member overriding. *)

val make :
  is_canonical:('a -> bool) ->
  merge:(earlier:'a -> later:'a -> 'a) ->
  (string * 'a) Sequence.t ->
  'a t
(** Create a new [Lazy_string_table.t] memoizing the given sequence.

    A good implementation of [merge] is necessary for correctness, since [merge]
    is used when a key-value pair emitted later in the sequence has the same key
    as a pair emitted earlier in the sequence. [merge] is provided with the
    value emitted earlier in the sequence (or, potentially, the output of a
    prior invocation of [merge]) and the value emitted later in the sequence. It
    may return one or the other, or create a new value to be stored instead of
    either.

    A good implementation of [is_canonical] is not necessary for correctness,
    but it is necessary for performance. The contract of [is_canonical] is as
    follows: [is_canonical v] must return [true] only when [merge v u] would
    return [v] for all possible values of [u] that could be emitted later in the
    sequence. Canonicity is what allows the cache to return results from {!get}
    and {!mem} without traversing the entire sequence (i.e., the trivial
    implementation [fun _ -> false] for [is_canonical] is always correct, but
    will always force the cache to traverse the entire sequence on the first
    lookup). *)

val get : 'a t -> string -> 'a option
(** Return the value associated with the given key. If the value is canonical
    and was already emitted by the input sequence, or if the input sequence has
    been exhausted, this function is guaranteed to complete in constant time.
    Otherwise, its worst-case runtime is proportional to the length of the input
    sequence (provided that [is_canonical] and [merge] complete in constant
    time, and ignoring any time required to compute elements of the sequence).

    Guaranteed not to advance the input sequence if the sequence has previously
    emitted a canonical value for the given key. *)

val mem : 'a t -> string -> bool
(** Return [true] if a value associated with the given key exists. If a value
    associated with this key was already emitted by the input sequence, or if
    the input sequence has been exhausted, this function is guaranteed to
    complete in constant time. Otherwise, its worst-case runtime is proportional
    to the length of the input sequence (provided that [is_canonical] and
    [merge] complete in constant time, and ignoring any time required to compute
    elements of the sequence).

    Guaranteed not to advance the input sequence if the sequence has previously
    emitted any value for the given key. *)

val to_seq : 'a t -> (string * 'a) Sequence.t
(** Eagerly exhaust the input sequence, then return a sequence iterating over
    all values stored in the cache, in undefined order. *)
