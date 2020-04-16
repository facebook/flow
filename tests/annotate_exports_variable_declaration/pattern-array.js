// @flow

declare opaque type O;
declare var o: O;
declare var os: Array<O>;

var [
  a,
  b = 0,
  [c, ...ds]
] = [
  o,
  o,
  os,
];

module.exports = [a, b, c, ds];
