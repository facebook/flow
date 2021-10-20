// @flow

const FrozenSuite = Object.freeze({
  DIAMONDS: 'Diamonds',
  CLUBS: 'Clubs',
  HEARTS: 'Hearts',
  SPADES: 'Spades',
});

// Indirection through get property
const IndirectFrozenSuite = Object.freeze({
  DIAMONDS: FrozenSuite.DIAMONDS,
  CLUBS: FrozenSuite.CLUBS,
  HEARTS: FrozenSuite.HEARTS,
  SPADES: FrozenSuite.SPADES,
});

export type IndirectFrozenSuiteValues = $Values<typeof IndirectFrozenSuite>;
