/* @flow */

type A = { foo: ?string };

function foo(a: A) {
}

const a: A = { foo: 'foo' };
const b = { foo: 'foo' };

foo({ ...a, foo: 'foo' }); // OK
foo({ foo: 'foo', ...a }); // OK

foo({ ...b, foo: 'foo' }); // OK
foo({ foo: 'foo', ...b }); // OK
