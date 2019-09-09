(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Negative
  | Neutral
  | Positive

(* Subtype relation for polarities, interpreting neutral as positive &
   negative: whenever compat(p1,p2) holds, things that have polarity p1 can
   appear in positions that have polarity p2. *)
let compat = function
  | (Positive, Positive)
  | (Negative, Negative)
  | (Neutral, _) ->
    true
  | _ -> false

let inv = function
  | Positive -> Negative
  | Negative -> Positive
  | Neutral -> Neutral

let mult = function
  | (Positive, Positive) -> Positive
  | (Negative, Negative) -> Positive
  | (Neutral, _)
  | (_, Neutral) ->
    Neutral
  | _ -> Negative

(* printer *)
let string = function
  | Positive -> "covariant"
  | Negative -> "contravariant"
  | Neutral -> "invariant"

let sigil = function
  | Positive -> "+"
  | Negative -> "-"
  | Neutral -> ""
