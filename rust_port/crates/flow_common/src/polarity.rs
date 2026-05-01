/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum Polarity {
    Negative,
    Neutral,
    Positive,
}

impl Polarity {
    // Subtype relation for polarities, interpreting neutral as positive &
    // negative: whenever compat(p1,p2) holds, things that have polarity p1 can
    // appear in positions that have polarity p2. *)
    pub fn compat(p1: Polarity, p2: Polarity) -> bool {
        match (p1, p2) {
            (Polarity::Positive, Polarity::Positive)
            | (Polarity::Negative, Polarity::Negative)
            | (Polarity::Neutral, _) => true,
            _ => false,
        }
    }

    pub fn inv(self) -> Polarity {
        match self {
            Polarity::Positive => Polarity::Negative,
            Polarity::Negative => Polarity::Positive,
            Polarity::Neutral => Polarity::Neutral,
        }
    }

    pub fn mult(p1: Polarity, p2: Polarity) -> Polarity {
        match (p1, p2) {
            (Polarity::Positive, Polarity::Positive) => Polarity::Positive,
            (Polarity::Negative, Polarity::Negative) => Polarity::Positive,
            (Polarity::Neutral, _) | (_, Polarity::Neutral) => Polarity::Neutral,
            _ => Polarity::Negative,
        }
    }

    pub fn equal(p1: Polarity, p2: Polarity) -> bool {
        match (p1, p2) {
            (Polarity::Positive, Polarity::Positive)
            | (Polarity::Negative, Polarity::Negative)
            | (Polarity::Neutral, Polarity::Neutral) => true,
            _ => false,
        }
    }

    pub fn string(self) -> &'static str {
        match self {
            Polarity::Positive => "covariant",
            Polarity::Negative => "contravariant",
            Polarity::Neutral => "invariant",
        }
    }

    pub fn sigil(self) -> &'static str {
        match self {
            Polarity::Positive => "+",
            Polarity::Negative => "-",
            Polarity::Neutral => "",
        }
    }

    pub fn object_literal_polarity(const_: bool) -> Polarity {
        if const_ {
            Polarity::Positive
        } else {
            Polarity::Neutral
        }
    }

    pub fn apply_const(const_: bool, p: Polarity) -> Polarity {
        if const_ { Polarity::Positive } else { p }
    }
}
