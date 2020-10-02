//@flow

class Foo {
  foo : "foo";
}

class Bar extends Foo {
  bar : "bar";
}

class Baz extends Bar {
  baz : "baz";
  foo : "oof";
}

(new Baz()).
//          ^
