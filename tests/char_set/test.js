/* @flow */

type C$flags = $CharSet<'ab'>;

declare class C {
  constructor(flags?: C$flags): C;
}

function f() {
  'a' as $CharSet<'ab'>; // ok
  'b' as $CharSet<'ab'>; // ok
  'ab' as $CharSet<'ab'>; // ok
  'ba' as $CharSet<'ab'>; // ok
  'aaaa' as $CharSet<'ab'>; // error
  'c' as $CharSet<'ab'>; // error
  'ac' as $CharSet<'ab'>; // error
}

function g(x: C$flags) {
  new C(x);
}
g('abcd');

function h(x: $CharSet<'ab'>) {
  x as 'foo';
}
