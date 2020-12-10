// @flow

declare var x: ?{
  a: $ReadOnlyArray<number>,
};

(x ? x.a : []).map(n => {
  if (n !== '') { // number incompatible with string
  }
});
