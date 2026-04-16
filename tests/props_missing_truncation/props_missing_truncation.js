// @flow

type Large = {
  a: number,
  b: number,
  c: number,
  d: number,
  e: number,
  f: number,
  g: number,
  h: number,
  i: number,
  j: number,
  k: number,
  l: number,
  m: number,
};

// 13 properties missing — should show first 10 + "... (3 more)"
({} as Large); // Error

// 3 properties missing — should show all 3 (no truncation)
({a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10} as Large); // Error
