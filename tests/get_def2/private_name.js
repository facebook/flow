// @flow

class Foo {
  #foo = 3;
//  ^

  foo() {
    class Bar {
      #bar(f: Foo, b: Bar) {
//      ^
        f.#foo;
//          ^
        b.#bar(f, b);
//          ^
      }
    }
  }
}

class Shadowed {
  #foo = 1;
  foo() {
    class Shadowing {
      #foo = 2;
      foo() {
        this.#foo;
//            ^
      }
    }
    this.#foo;
//         ^
  }
}
