// TSX type literals retain computed keys and optionality while preserving index signatures.
type T = {
  [TREND_SERIES_ALL]: number; // OK
  ['data-x']?: string; // OK
  [key: string]: number; // OK
};
