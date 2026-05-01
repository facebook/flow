// @flow

class P<X> {}

module.exports = new P<number>() as P<number>;
