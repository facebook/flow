// @flow

export function foo() {}

exports.foo; // ERROR
module.exports.foo; // ERROR
