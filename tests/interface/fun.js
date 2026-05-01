interface F {
  (): void;
}

var f0: F = () => 0; // error: number ~> void
var f1: F = () => {}; // ok

// function statics
function f() {}
f as interface {}; // OK
f as interface {xxx: boolean}; // ERROR

class S {
  static build(): S {
    return new S();
  }
}

const build = S.build;
build as interface {}; // OK
build as interface {foo: boolean}; // ERROR
