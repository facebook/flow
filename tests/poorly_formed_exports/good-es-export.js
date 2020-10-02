// @flow

function addsAProp(x) {
  // `module.exports` and `x` are aliased, but this is an ES module, so this is okay
  x.foo = '42';
}

addsAProp(module.exports);

export default 42;
