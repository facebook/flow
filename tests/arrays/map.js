declare var x: ?{
  a: ReadonlyArray<number>,
};

(x ? x.a : []).map(n => {
  if (n !== '') { // number incompatible with string
  }
});
