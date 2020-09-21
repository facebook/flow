// @flow

class A {
  // $FlowFixMe
  reduce(state, payload): any {}
}

module.exports = (new A(): A);
