/* @flow */

const a = require('./a');
const b: { a(): void, bar(): void } = Object.assign({ bar() {}, ...{} }, a);
b.a(); // works here
module.exports = b;
