// @flow

declare class C {}

declare function foo1a(): C;
declare function foo1b(): typeof C;

module.exports = { C, foo1a, foo1b };
