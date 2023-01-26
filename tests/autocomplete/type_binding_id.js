// @flow

class Foo {}

type F = string;
//    ^

opaque type F = string;
//           ^

function foo<F>() {}
//            ^
