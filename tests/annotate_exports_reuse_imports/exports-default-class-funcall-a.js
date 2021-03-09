// @flow

declare class Base {}
class B extends Base {}

function foo(): typeof B { return B };

module.exports = (foo(): typeof B);
