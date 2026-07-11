/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Suit {
  Hearts,
  Diamonds,
  Clubs,
  Spades,
}

export enum Rank of number {
  Ace = 1,
  Two = 2,
  Three = 3,
  Four = 4,
  Five = 5,
  Six = 6,
  Seven = 7,
  Eight = 8,
  Nine = 9,
  Ten = 10,
  Jack = 11,
  Queen = 12,
  King = 13,
}

type Card = {suit: Suit, rank: Rank};

export function cardValue(card: Card): number {
  return match (card.rank) {
    Rank.Ace => 11,
    Rank.Jack | Rank.Queen | Rank.King => 10,
    Rank.Two => 2,
    Rank.Three => 3,
    Rank.Four => 4,
    Rank.Five => 5,
    Rank.Six => 6,
    Rank.Seven => 7,
    Rank.Eight => 8,
    Rank.Nine => 9,
    Rank.Ten => 10,
  };
}

export function suitSymbol(suit: Suit): string {
  return match (suit) {
    Suit.Hearts => "\u2665",
    Suit.Diamonds => "\u2666",
    Suit.Clubs => "\u2663",
    Suit.Spades => "\u2660",
  };
}

