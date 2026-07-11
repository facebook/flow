Write Flow code combining enums with match expressions.

Create a `Suit` enum (Hearts, Diamonds, Clubs, Spades) and a `Rank` enum of number (Ace=1 through King=13). Define `type Card = {suit: Suit, rank: Rank}`.

Write:
- `cardValue(card: Card): number` — Ace=11, face cards=10, others=numeric value
- `suitSymbol(suit: Suit): string` — return the Unicode symbol for each suit
