// @flow

class A {
  // $FlowFixMe[signature-verification-failure]
  // $FlowFixMe[missing-annot]
  reduce(state, payload): any {}
}

module.exports = (new A(): A);
