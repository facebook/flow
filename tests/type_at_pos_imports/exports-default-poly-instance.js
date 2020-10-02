// @flow

class P<X> {}

module.exports = (new P<number>(): P<number>);
