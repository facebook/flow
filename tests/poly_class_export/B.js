// @flow

const A = require('./A');

class B extends A<string> {
  constructor() {
    super();
  }
}

module.exports = (new B(): B);
