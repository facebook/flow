// @flow

const F = require('./library');

enum E {Foo, Bar}
//   ^

const a = E.Foo;
//        ^

const b = F.Foo;
//        ^

const c = E.Foo;
//          ^

const c = E.Bar;
//          ^

const d = F.Foo;
//          ^

const e = E.cast("Foo");
//          ^

const f = F.isValid("Foo");
//          ^
