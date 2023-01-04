// @flow

[1, 2, 3].reduce((acc, n) => { // ANNOT
  acc.push(n);
  return acc;
}, []);
